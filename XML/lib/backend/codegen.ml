(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Format
open Target
open Machine
open Emission
open State
open Base

(* Storage for all the live-variables, their locations *)
module Env = struct
  type t = (string, loc) Hashtbl.Poly.t

  let empty () = Hashtbl.Poly.create ~size:16 ()
  let bind t x loc = Hashtbl.set t ~key:x ~data:loc
  let find t x = Hashtbl.find t x
end

let reg_is_used env r =
  Hashtbl.fold env ~init:false ~f:(fun ~key:_ ~data:loc acc ->
    acc
    ||
    match loc with
    | Reg r' -> equal_reg r r'
    | Stack _ -> false)
;;

let rec split_n lst n =
  if n <= 0
  then [], lst
  else (
    match lst with
    | [] -> [], []
    | x :: xs ->
      let l1, l2 = split_n xs (n - 1) in
      x :: l1, l2)
;;

let rec gen_exp env dst expr ppf : Env.t State.t =
  let open State in
  match expr with
  | Expression.Exp_constant (Constant.Const_integer n) ->
    Emission.emit li dst n;
    return env
  | Expression.Exp_ident x ->
    (match Env.find env x with
     | Some (Reg r) ->
       if not (equal_reg r dst) then Emission.emit mv dst r;
       return env
     | Some (Stack offset) ->
       Emission.emit ld dst offset;
       return env
     | None -> failwith ("Unbound identifier as value: " ^ x))
  | Expression.Exp_tuple (_, _, _) -> failwith "Tuples as values not supported"
  | Expression.Exp_apply (f, arg) ->
    (match f with
     | Expression.Exp_ident op
       when Base.List.mem
              [ "+"; "-"; "*"; "="; "<"; ">"; "<="; ">=" ]
              op
              ~equal:String.equal ->
       (match arg with
        | Expression.Exp_tuple (a1, a2, []) ->
          let* env = gen_exp env (T 0) a1 ppf in
          let* lhs_loc = Emission.spill_with_frame (T 0) ~comm:"binop: spill LHS" in
          let lhs_ofs =
            match lhs_loc with
            | Stack (_, ofs) -> ofs
            | _ -> failwith "spill_with_frame must return Stack"
          in
          let* env = gen_exp env (T 1) a2 ppf in
          Emission.emit ld (T 2) (S 0, lhs_ofs);
          Emission.emit_bin_op op dst (T 2) (T 1);
          return env
        | _ -> failwith "binary operator expects 2-tuple")
     | Expression.Exp_ident fname ->
       let open State in
       let args =
         match arg with
         | Expression.Exp_tuple (a1, a2, rest) -> a1 :: a2 :: rest
         | _ -> [ arg ]
       in
       let* env =
         List.foldi args ~init:(return env) ~f:(fun i acc arg ->
           let* env = acc in
           if i < Target.arg_regs_count
           then gen_exp env (A i) arg ppf
           else failwith "too many args")
       in
       let* env = Emission.emit_save_caller_regs env in
       Emission.emit call fname;
       if not (equal_reg dst (A 0)) then Emission.emit mv dst (A 0);
       return env
     | _ -> failwith "unsupported application")
  | Expression.Exp_if (cond, then_e, Some else_e) ->
    let* env = gen_exp env (T 0) cond ppf in
    let lbl_else = State.fresh_label "else_" in
    let lbl_end = State.fresh_label "end_" in
    Emission.emit beq (T 0) Zero lbl_else;
    let* env = gen_exp env dst then_e ppf in
    Emission.emit j lbl_end;
    Emission.emit label lbl_else;
    let* env = gen_exp env dst else_e ppf in
    Emission.emit label lbl_end;
    return env
  | Expression.Exp_fun _ -> failwith "nested function values not supported"
  | Expression.Exp_let (Expression.Nonrecursive, (vb1, vb_list), body) ->
    let bindingsl = vb1 :: vb_list in
    let open State in
    let rec bind_all env_acc = function
      | [] -> return env_acc
      | vb :: tl ->
        (match vb.Expression.pat with
         | Pattern.Pat_var id ->
           let* env_acc = gen_exp env_acc (A 0) vb.Expression.expr ppf in
           Env.bind env_acc id (Reg (A 0));
           bind_all env_acc tl
         | Pattern.Pat_construct (_, None) ->
           let* env_acc = gen_exp env_acc (A 0) vb.Expression.expr ppf in
           bind_all env_acc tl
         | _ -> failwith "let-pattern not supported in this simplified backend")
    in
    let* env = bind_all env bindingsl in
    gen_exp env dst body ppf
  | _ -> failwith "Not implemented"
;;

let gen_func func_name argsl expr ppf =
  let arg, argl = argsl in
  let argsl = arg :: argl in
  let arity = List.length argsl in
  let reg_count = Array.length Target.arg_regs in
  let reg_params, stack_params = split_n argsl (min arity reg_count) in
  let env = Env.empty () in
  List.iteri reg_params ~f:(fun i pat ->
    match pat with
    | Pattern.Pat_var name -> Env.bind env name (Reg (A i))
    | _ -> failwith "Pattern not supported for arg");
  List.iteri stack_params ~f:(fun i pat ->
    match pat with
    | Pattern.Pat_var name ->
      let off = (2 * Target.word_size) + (i * Target.word_size) in
      (* s0 == fp *)
      Env.bind env name (Stack (S 0, off))
    | _ -> failwith "Pattern not supported for arg");
  let local_count = 4 in
  let stack_size = (2 + local_count) * Target.word_size in
  Emission.emit_prologue func_name stack_size;
  let _env, _st = gen_exp env (A 0) expr ppf { frame_offset = 0 } in
  Emission.emit_epilogue stack_size;
  Emission.flush_queue ppf
;;

let gen_start ppf =
  fprintf ppf ".section .text\n";
  fprintf ppf ".global main\n";
  fprintf ppf ".type main, @function\n"
;;

let gen_program ppf program =
  (* reset fresh label counter for determinism per program *)
  State.reset_labels ();
  let has_main =
    List.exists program ~f:(function
      | Structure.Str_value (_, (vb1, vbl)) ->
        let vbs = vb1 :: vbl in
        List.exists vbs ~f:(fun vb ->
          match vb.Expression.pat with
          | Pattern.Pat_var "main" -> true
          | _ -> false)
      | _ -> false)
  in
  if has_main then gen_start ppf;
  List.iter program ~f:(function
    | Structure.Str_value (_rec_flag, (vb1, vbl)) ->
      let vbs = vb1 :: vbl in
      List.iter vbs ~f:(fun vb ->
        match vb.Expression.pat, vb.Expression.expr with
        | Pattern.Pat_var name, Expression.Exp_fun (args, body) ->
          gen_func name args body ppf
        | Pattern.Pat_var "main", expr ->
          Emission.emit_prologue "main" (4 * Target.word_size);
          let _env, _st = gen_exp (Env.empty ()) (A 0) expr ppf { frame_offset = 0 } in
          Emission.emit_epilogue 64;
          Emission.flush_queue ppf
        | Pattern.Pat_var _name, _ -> ()
        | _ -> failwith "unsupported pattern")
    | _ -> ())
;;
