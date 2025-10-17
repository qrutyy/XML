(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Format

(* Immediate, atomic expressions that do not require the reduction *)
    type im_expr = 
        | Imm_num of int
        | Imm_ident of ident 
    ;;

(* Complex/Computable expression *)
    type comp_expr = 
    | Comp_imm of im_expr
    | Comp_binop of binop * im_expr * im_expr (* x + y *)
    | Comp_app of im_expr * im_expr list       (* f(x, y) *)
  | Comp_branch of im_expr * anf_expr * anf_expr      (* if c then ... else ... *)
  | Comp_func of ident * anf_expr                (* fun x -> ... *)

type anf_expr = 
| Anf_comp_expr of comp_expr  (* Atomic Computable Expression *)
  | Anf_let of rec_flag * ident * comp_expr * anf_expr (* let x = cexpr in aexpr *)



let normalise_const = fun const_expr cps_k =
    match const_expr with 
    | Const_integer e -> Imm_num e
    | Const_char e -> Imm_ident e 
    | Const_string e -> Imm_ident e (* mb ident list*)

(* basic list cps *)
let normalise_list = fun expr_list cps_k = 
    match expr_list with 
        | hd :: tl -> normalise_expr hd (fun result_hd -> 
    normalise_list tl (fun result_tl -> 
    cps_k (result_hd :: result_tl)))
    | [] -> k []


(* main ANF normalisation function *)
let normalise_expr = fun expr cps_k =
    match expr with
        | Exp_ident e -> cps_k @@ Imp_ident e
        | Exp_constant e -> cps_k @@ normalise_const e 
        | Exp_tuple expr_l2 ->
        match expr_l2 with 
        | (expr1, expr2, []) ->

        | Exp_apply (expr1, Exp_tuple(expr_l2)) -> 
            when binop expr1 then
            let op = get_binop binop
            normalise_expr expr_l2 (fun res ->
                let temp_reg = get_new_t_reg in 
                let rest = k @@ Imm_ident temp_reg in 
            match rest with 
                | ACE (Comp_imm (Imm_ident id)) when String.equal t id ->
          return @@ ACE (Comp_binop (op, exp1_res, exp2_res))
        | _ ->
          return
          @@ Anf_let (Nonrecursive, t, Comp_binop (, exp1_res, exp2_res), rest)))
