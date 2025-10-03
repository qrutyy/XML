(** Copyright 2024, Mikhail Gavrilenko, Daniil Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Format
open Target
open Machine
open Emission.Emission

let label_counter = ref 0

let fresh_label prefix =
  let n = !label_counter in
  incr label_counter;
  prefix ^ string_of_int n
;;

type loc =
  | Reg of reg
  | Stack of reg

(* Storage for all the live-variables, their locations *)
module Env = struct
  type t = (string, loc) Hashtbl.t

  let empty () = Hashtbl.create 16
  let bind t x loc = Hashtbl.replace t x loc
  let find t x = Hashtbl.find_opt t x
end

let reg_is_used env r =
  Hashtbl.fold
    (fun _ loc acc ->
       acc
       ||
       match loc with
       | Reg r' -> r = r'
       | Stack _ -> false)
    env
    false
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

let rec gen_exp env dst expr ppf =
  match expr with
  | Expression.Exp_constant (Constant.Const_integer n) ->
    emit li dst n;
    env
  | Expression.Exp_ident x ->
    (match Env.find env x with
     | Some (Reg r) ->
       if equal_reg r dst then emit mv dst r;
       env
     | Some (Stack offset) ->
       emit ld dst offset;
       env
     | None ->
       (* external ident: assume function name or global; move name into a register? *)
       (* In our simple convention: trying to use an identifier as value is error *)
       failwith ("Unbound identifier as value: " ^ x))
  | Expression.Exp_tuple (_, _, _) -> failwith "Tuples as values not supported"
  | Expression.Exp_apply (f, arg) ->
    (match f with
     | Expression.Exp_ident op
       when List.mem op [ "+"; "-"; "*"; "="; "<"; ">"; "<="; ">=" ] ->

       (match arg with
| Expression.Exp_tuple (a1, a2, []) ->
          let env = gen_exp env (T 0) a1 ppf in

          emit mv (T 2) (T 0);

          let env = gen_exp env (T 1) a2 ppf in

          emit_bin_op op dst (T 2) (T 1);
          env
        | _ -> failwith "binary operator expects 2-tuple")
     | Expression.Exp_ident fname ->
       (match arg with
        | Expression.Exp_constant _ | Expression.Exp_ident _ | Expression.Exp_apply _ ->
          let env = gen_exp env (T 0) arg ppf in
          emit mv (A 0) (T 0);
          emit call fname;
          if dst <> A 0 then emit mv dst (A 0);
          env
        | _ ->
          let env = gen_exp env (T 0) arg ppf in
          emit mv (A 0) (T 0);
          emit call fname;
          if dst <> A 0 then emit mv dst (A 0);
          env)
     | _ -> failwith "unsupported application")
  | Expression.Exp_if (cond, then_e, Some else_e) ->
    let env = gen_exp env (T 0) cond ppf in
    let lbl_else = fresh_label "else_" in
    let lbl_end = fresh_label "end_" in
    emit beq (T 0) Zero lbl_else;
    let env = gen_exp env dst then_e ppf in
    emit j lbl_end;
    emit label lbl_else;
    let env = gen_exp env dst else_e ppf in
    emit label lbl_end;
    env
  | Expression.Exp_fun _ -> failwith "nested function values not supported"
  | Expression.Exp_let (Expression.Nonrecursive, (vb1, vb_list), body) ->
    let bindingsl = vb1 :: vb_list in
    let env =
      List.fold_left
        (fun env_acc vb ->
           match vb.Expression.pat with
           | Pattern.Pat_var id ->
             (* rhs -> a0 *)
             let env_acc = gen_exp env_acc (A 0) vb.Expression.expr ppf in
             let loc = Reg (A 0) in
             Env.bind env_acc id loc;
             env_acc
           | Pattern.Pat_construct (name, _) when name = "()" ->
             let _ = gen_exp env_acc (A 0) vb.Expression.expr ppf in
             env_acc
           | _ -> failwith "let-pattern not supported in this simplified backend")
        env
        bindingsl
    in
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
  List.iteri
    (fun i pat ->
       match pat with
       | Pattern.Pat_var name -> Env.bind env name (Reg (A i))
       | _ -> failwith "Pattern not supported for arg")
    reg_params;
  List.iteri
    (fun i pat ->
       match pat with
       | Pattern.Pat_var name ->
         Env.bind env name (Stack (Offset (A i, (i + 1) * Target.word_size)))
       | _ -> failwith "Pattern not supported for arg")
    stack_params;
  let local_count = 4 in
  let stack_size = (2 + local_count) * Target.word_size in
  (* Emit function prologue, then body into the queue, flush, and epilogue *)
  emit_prologue func_name stack_size ppf;
  let _env = gen_exp env (A 0) expr ppf in
  flush_queue ppf;
  emit_epilogue ppf
;;

let gen_start ppf =
  fprintf ppf ".global _start\n";
  fprintf ppf "_start:\n";
  fprintf ppf "  call main\n";
  (* result of main is already in a0 *)
  fprintf ppf "  li a7, 93\n";
  (* 93 = SYS_exit *)
  fprintf ppf "  ecall\n\n"
;;

let gen_program ppf program =
  (* reset fresh label counter for determinism per program *)
  label_counter := 0;
  let has_main =
    List.exists
      (function
        | Structure.Str_value (_, (vb1, vbl)) ->
          let vbs = vb1 :: vbl in
          List.exists
            (fun vb ->
               match vb.Expression.pat with
               | Pattern.Pat_var "main" -> true
               | _ -> false)
            vbs
        | _ -> false)
      program
  in
  if has_main then gen_start ppf;
  List.iter
    (function
      | Structure.Str_value (_rec_flag, (vb1, vbl)) ->
        let vbs = vb1 :: vbl in
        List.iter
          (fun vb ->
             match vb.Expression.pat, vb.Expression.expr with
             | Pattern.Pat_var name, Expression.Exp_fun (args, body) ->
               gen_func name args body ppf
             | Pattern.Pat_var "main", expr ->
               emit_prologue "main" (4 * Target.word_size) ppf;
               let _env = gen_exp (Env.empty ()) (A 0) expr ppf in
               flush_queue ppf;
               emit_epilogue ppf
             | Pattern.Pat_var _name, _ -> ()
             | _ -> failwith "unsupported pattern")
          vbs
      | _ -> ())
    program
;;
