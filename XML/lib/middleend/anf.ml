(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast.Expression
open Common.Ast.Constant
open Common.Ast.Structure
open Common.Ast

(* Immediate, atomic expressions that do not require the reduction *)
type im_expr =
  | Imm_num of int
  | Imm_ident of ident

(* Complex/Computable expression *)
type comp_expr =
  | Comp_imm of im_expr
  | Comp_binop of ident * im_expr * im_expr (* x + y *)
  | Comp_app of im_expr * im_expr list (* f(x, y) *)
  | Comp_branch of im_expr * anf_expr * anf_expr (* if c then ... else ... *)
  | Comp_func of ident list * anf_expr (* fun x y ... -> ... *)
  | Comp_tuple of im_expr list

and anf_expr =
  | Anf_comp_expr of comp_expr
  | Anf_let of rec_flag * ident * comp_expr * anf_expr

type astructure_item =
  | Anf_str_eval of anf_expr
  | Anf_str_value of rec_flag * ident * anf_expr

type aprogram = astructure_item list

(* ---------- helpers ---------- *)

let normalise_const = function
  | Const_integer e -> Imm_num e
  | Const_char e -> Imm_ident (String.make 1 e)
  | Const_string e -> Imm_ident e
;;

let flatten_arg_expr e =
  match e with
  | Exp_tuple (e1, e2, rest) -> e1 :: e2 :: rest
  | _ -> [ e ]
;;

let pat_vars = function
  | Pattern.Pat_var p -> [ p ]
  | _ -> failwith "Only simple variable patterns are allowed in function parameters"
;;

let rec collect_params_and_body expr acc =
  match expr with
  | Exp_fun ((first_pat, rest_pats), body) ->
    let vars = pat_vars first_pat @ List.concat_map pat_vars rest_pats in
    collect_params_and_body body (acc @ vars)
  | _ -> acc, expr
;;

type nstate = { next : int }

let initial_state = { next = 0 }

let fresh (st : nstate) : ident * nstate =
  let name = "t_" ^ string_of_int st.next in
  name, { next = st.next + 1 }
;;

let rec norm_comp expr (k : comp_expr -> nstate -> anf_expr * nstate) (st : nstate)
  : anf_expr * nstate
  =
  match expr with
  | Exp_ident e -> k (Comp_imm (Imm_ident e)) st
  | Exp_constant c -> k (Comp_imm (normalise_const c)) st
  | Exp_tuple (expr1, expr2, rest_list) ->
    let all_exprs = expr1 :: expr2 :: rest_list in
    norm_list_to_imm all_exprs (fun imm_list st -> k (Comp_tuple imm_list) st) st
  | Exp_apply (Exp_ident op, Exp_tuple (expr1, expr2, []))
    when List.mem op [ "+"; "-"; "*"; "="; "<"; ">"; "<="; ">="; "<>" ] ->
    norm_to_imm
      expr1
      (fun v1 ->
         norm_to_imm expr2 (fun v2 ->
           let ce =
             match v1, v2 with
             | Imm_num n1, Imm_num n2 ->
               (match op with
                | "+" -> Comp_imm (Imm_num (n1 + n2))
                | "-" -> Comp_imm (Imm_num (n1 - n2))
                | "*" -> Comp_imm (Imm_num (n1 * n2))
                | _ -> Comp_binop (op, v1, v2))
             | _ -> Comp_binop (op, v1, v2)
           in
           k ce))
      st
  | Exp_apply (_, _) ->
    let rec collect_args_and_func expr acc =
      match expr with
      | Exp_apply (f, arg) -> collect_args_and_func f (arg :: acc)
      | f -> f, acc
    in
    let func_expr, args_exprs = collect_args_and_func expr [] in
    let flat_args = List.concat_map flatten_arg_expr args_exprs in
    norm_to_imm
      func_expr
      (fun func_imm ->
         norm_list_to_imm flat_args (fun args_imms st ->
           k (Comp_app (func_imm, args_imms)) st))
      st
  | Exp_if (cond, then_, Some else_) ->
    norm_to_imm
      cond
      (fun cond_imm st ->
         let then_anf, st1 = norm_body then_ st in
         let else_anf, st2 = norm_body else_ st1 in
         k (Comp_branch (cond_imm, then_anf, else_anf)) st2)
      st
  | Exp_fun _ ->
    let params, body = collect_params_and_body expr [] in
    (match params with
     | [] -> failwith "Function with no parameters found"
     | _ ->
       let body_anf, st' = norm_body body st in
       k (Comp_func (params, body_anf)) st')
  | Exp_let (rec_flag, (first_binding, other_bindings), body) ->
    (match other_bindings with
     | [] -> ()
     | _ -> failwith "`let ... and ...` is not supported in ANF yet");
    let { pat; expr = vb_expr } = first_binding in
    (match pat with
     | Pattern.Pat_var x ->
       norm_comp
         vb_expr
         (function
           | (Comp_func _ | Comp_tuple _) as ce ->
             fun st ->
               let tmp, st1 = fresh st in
               let body_anf, st2 = norm_comp body k st1 in
               ( Anf_let
                   ( Nonrecursive
                   , tmp
                   , ce
                   , Anf_let (rec_flag, x, Comp_imm (Imm_ident tmp), body_anf) )
               , st2 )
           | (Comp_imm _ | Comp_binop _ | Comp_app _ | Comp_branch _) as ce ->
             fun st ->
               let body_anf, st' = norm_comp body k st in
               Anf_let (rec_flag, x, ce, body_anf), st')
         st
     | Pattern.Pat_any | Pattern.Pat_construct ("()", None) ->
       norm_comp
         vb_expr
         (fun ce st ->
            let tmp, st1 = fresh st in
            let body_anf, st2 = norm_comp body k st1 in
            Anf_let (Nonrecursive, tmp, ce, body_anf), st2)
         st
     | _ -> failwith ("Unsupported pattern in `let`-binding: " ^ Pattern.show pat))
  | _ -> failwith "unsupported expression in ANF normaliser"

and norm_to_imm expr (k : im_expr -> nstate -> anf_expr * nstate) (st : nstate)
  : anf_expr * nstate
  =
  norm_comp
    expr
    (fun ce st ->
       match ce with
       | Comp_imm imm -> k imm st
       | _ ->
         let tmp, st1 = fresh st in
         let body, st2 = k (Imm_ident tmp) st1 in
         Anf_let (Nonrecursive, tmp, ce, body), st2)
    st

and norm_list_to_imm
      expr_list
      (k : im_expr list -> nstate -> anf_expr * nstate)
      (st : nstate)
  : anf_expr * nstate
  =
  match expr_list with
  | [] -> k [] st
  | hd :: tl ->
    norm_to_imm
      hd
      (fun imm st1 -> norm_list_to_imm tl (fun imms st2 -> k (imm :: imms) st2) st1)
      st

and norm_body expr (st : nstate) : anf_expr * nstate =
  norm_to_imm expr (fun imm st -> Anf_comp_expr (Comp_imm imm), st) st
;;

let norm_item (item : structure_item) (st : nstate) : astructure_item * nstate =
  match item with
  | Str_eval expr ->
    let body_anf, st' = norm_body expr st in
    Anf_str_eval body_anf, st'
  | Str_value (rec_flag, (first_binding, other_bindings)) ->
    (match other_bindings with
     | [] -> ()
     | _ ->
       failwith "Mutually recursive `let ... and ...` bindings are not supported yet.");
    let { pat; expr } = first_binding in
    (match pat with
     | Pattern.Pat_var name ->
       let body_anf, st' = norm_body expr st in
       Anf_str_value (rec_flag, name, body_anf), st'
     | _ ->
       failwith
         "Unsupported pattern in a top-level let-binding. Only simple variables are \
          allowed.")
  | _ -> failwith "Unsupported top-level structure item."
;;

let anf_program (program : structure_item list) : aprogram =
  let _, rev_items =
    List.fold_left
      (fun (st, acc) item ->
         let it, st' = norm_item item st in
         st', it :: acc)
      (initial_state, [])
      program
  in
  List.rev rev_items
;;
