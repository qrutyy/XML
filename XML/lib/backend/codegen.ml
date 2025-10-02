(** Copyright 2024, Mikhail Gavrilenko, Daniil Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Format
    open Target
    open Emission

type loc =
  | Reg of string
  | Stack of int

(* Storage for all the live-variables, their locations *)
module Env = struct
  type t = (string, loc) Hashtbl.t

  let empty () = Hashtbl.create 16
  let bind t x loc = Hashtbl.replace t x loc
  let find t x = Hashtbl.find_opt t x
end

let rec split_n lst n =
  if n <= 0 then ([], lst)
  else
    match lst with
    | [] -> ([], [])
    | x :: xs ->
        let (l1, l2) = split_n xs (n-1) in
        (x :: l1, l2)
;;

let rec gen_exp env dst expr ppf =
  match expr with
  | Expression.Exp_constant (Constant.Const_integer n) ->
        emit li dst n;
    env
  | Expression.Exp_ident x ->
    (match Env.find env x with
     | Some (Reg r) ->
        emit mv dst r;
       env
     | Some (Stack offset) ->
        emit ld dst offset; 
       env
     | None -> failwith ("Unbound variable: " ^ x))
  | Expression.Exp_apply (f, arg) ->
    (match f with
     | Expression.Exp_ident op when List.mem op [ "+"; "-"; "*"; "=" ]  ->
       let t0 = Target.temp_regs.(0) in
       let t1 = Target.temp_regs.(1) in
       (match arg with
        | Expression.Exp_tuple (a1, a2, []) ->
          let env = gen_exp env t0 a1 ppf in
          let env = gen_exp env t1 a2 ppf in
          if instr <> "" then emit_bin_op f dst t0 t1;
          env
        | _ -> failwith "unsupported argument for binary operator")
     | Expression.Exp_ident fname ->
       let t0 = Target.temp_regs.(0) in
       let env = gen_exp env t0 arg ppf in
                emit mv a0 t0;
                emit call fname;
       if dst <> "a0" then emit mv dst a0;
       env
     | _ -> failwith "unsupported application")
  | Expression.Exp_if (cond, then_e, Some else_e) ->
    let t0 = Target.temp_regs.(0) in
    let env = gen_exp env t0 cond ppf in
    let lbl_else = "L_else_" ^ string_of_int (Hashtbl.hash expr) in
    let lbl_end = "L_end_" ^ string_of_int (Hashtbl.hash expr) in
    fprintf ppf "  beq %s, x0, %s\n" t0 lbl_else;
    let env = gen_exp env dst then_e ppf in
    fprintf ppf "  j %s\n" lbl_end;
    fprintf ppf "%s:\n" lbl_else;
    let env = gen_exp env dst else_e ppf in
    fprintf ppf "%s:\n" lbl_end;
    env
  | _ -> failwith "Expression not implemented yet"
;;

let gen_func func_name argsl expr ppf =
  let arg, argl = argsl in
  let argsl = arg :: argl in
  let arity = List.length argsl in
  let reg_count = Array.length Target.arg_regs in
  let reg_params, stack_params = split_n argsl (min arity reg_count) in
  let env = Env.empty () in

List.iteri (fun i pat ->
  match pat with
  | Pattern.Pat_var name -> Env.bind env name (Reg Target.arg_regs.(i))
  | _ -> failwith "Pattern not supported for arg"
) reg_params;

List.iteri (fun i pat ->
  match pat with
  | Pattern.Pat_var name -> Env.bind env name (Stack ((i + 1) * Target.word_size))
  | _ -> failwith "Pattern not supported for arg"
) stack_params;
  let local_count = 4 in
  let stack_size = (2 + local_count) * Target.word_size in
  let _env = gen_exp env Target.arg_regs.(0) expr ppf in
    Emission.emit_prologue func_name stack_size ppf;
    Emission.emit_epilogue ppf
;; 



let gen_program ppf program =
  fprintf ppf ".global _start\n";
  fprintf ppf "_start:\n";
  fprintf ppf "  li a0, 6\n";
  fprintf ppf "  call fact\n";
  fprintf ppf "  li a5, 256\n";
  fprintf ppf "  rem a0, a0, a5\n";
  fprintf ppf "  li a7, 93\n";
  fprintf ppf "  ecall\n";
List.iter (function
    | Structure.Str_value (Expression.Recursive, (vb1, _)) ->
      let pat = vb1.pat in
      let exp = vb1.expr in
      (match pat with
                | Pattern.Pat_var name -> gen_func name (pat, []) exp ppf
       | _ -> failwith "Unsupported pattern")
    | _ -> ())
  program
;;
