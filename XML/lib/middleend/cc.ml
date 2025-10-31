(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Anf
module S = Set.Make (String)

let word_size = 8

type state =
  { next_fn_id : int
  ; new_defs : astructure_item list
  }

let initial_state = { next_fn_id = 0; new_defs = [] }

let fresh_lambda_name st =
  let id = st.next_fn_id in
  let name = "lambda_" ^ string_of_int id in
  name, { st with next_fn_id = id + 1 }
;;

(**
  * =========================================================================
  *                            Free variable analysis 
  * =========================================================================
*)

let rec fv_anf_expr bound_vars expr =
  match expr with
  | Anf_comp_expr cexpr -> fv_comp_expr bound_vars cexpr
  | Anf_let (rec_flag, name, cexpr, body) ->
    let fv_cexpr = fv_comp_expr bound_vars cexpr in
    let bound_vars' =
      match rec_flag with
      | Expression.Recursive -> S.add name bound_vars
      | Expression.Nonrecursive -> bound_vars
    in
    let fv_body = fv_anf_expr (S.add name bound_vars') body in
    let bound_vars_for_cexpr =
      match rec_flag with
      | Expression.Recursive -> S.add name bound_vars
      | Expression.Nonrecursive -> bound_vars
    in
    let fv_cexpr_final = fv_comp_expr bound_vars_for_cexpr cexpr in
    S.union fv_cexpr_final fv_body

and fv_comp_expr bound_vars cexpr =
  let fv_imm imm =
    match imm with
    | Imm_num _ -> S.empty
    | Imm_ident id -> if S.mem id bound_vars then S.empty else S.singleton id
  in
  let fv_imms imms =
    List.fold_left (fun acc imm -> S.union acc (fv_imm imm)) S.empty imms
  in
  match cexpr with
  | Comp_imm imm -> fv_imm imm
  | Comp_binop (_, v1, v2) -> S.union (fv_imm v1) (fv_imm v2)
  | Comp_app (func, args) -> S.union (fv_imm func) (fv_imms args)
  | Comp_branch (cond, then_, else_) ->
    S.union
      (fv_imm cond)
      (S.union (fv_anf_expr bound_vars then_) (fv_anf_expr bound_vars else_))
  | Comp_func (params, body) ->
    let bound_vars' = S.add_seq (List.to_seq params) bound_vars in
    fv_anf_expr bound_vars' body
  | Comp_tuple imms -> fv_imms imms
  | Comp_alloc imms -> fv_imms imms
  | Comp_load (addr, _) -> fv_imm addr
;;

(**
  * =========================================================================
  *                      Closure transformation (Top-Down)
  * =========================================================================
*)

(**
 * Ex: stitch_let ("z", E_body) (let x = E1 in let y = E2 in E_res)
 * Result: let x = E1 in let y = E2 in let z = E_res in E_body
 *)
let rec stitch_let (rec_flag, name, body_to_insert) anf_with_hole =
  match anf_with_hole with
  | Anf_comp_expr ce -> Anf_let (rec_flag, name, ce, body_to_insert)
  | Anf_let (rf, n, ce, b) ->
    Anf_let (rf, n, ce, stitch_let (rec_flag, name, body_to_insert) b)
;;

let rec cc_anf_expr bound_vars st expr : anf_expr * state =
  match expr with
  | Anf_comp_expr cexpr ->
    let expr', st' = cc_comp_expr bound_vars st cexpr in
    expr', st'
  | Anf_let (rec_flag, name, cexpr, body) ->
    let cexpr_anf, st' = cc_comp_expr bound_vars st cexpr in
    let body', st'' = cc_anf_expr (S.add name bound_vars) st' body in
    let final_anf = stitch_let (rec_flag, name, body') cexpr_anf in
    final_anf, st''

and cc_comp_expr bound_vars st cexpr : anf_expr * state =
  match cexpr with
  | Comp_func (params, body) ->
    let free_vars = S.elements (fv_anf_expr (S.of_list params) body) in
    let code_name, st' = fresh_lambda_name st in
    let env_param = "env" in
    let new_params = env_param :: params in
    let new_body =
      let lets_to_add =
        List.mapi
          (fun i var ->
             Anf_let
               ( Expression.Nonrecursive
               , var
               , Comp_load (Imm_ident env_param, (i + 1) * word_size)
               , Anf_comp_expr (Comp_imm (Imm_num 0)) ))
          free_vars
      in
      let body_with_lets =
        List.fold_right
          (fun let_binding inner_body ->
             match let_binding with
             | Anf_let (rf, name, ce, _) -> Anf_let (rf, name, ce, inner_body)
             | _ -> failwith "impossible: not a let binding")
          lets_to_add
          body
      in
      let bound_for_body = S.add_seq (List.to_seq (params @ free_vars)) bound_vars in
      let body', st_for_body = cc_anf_expr bound_for_body st' body_with_lets in
      body', st_for_body
    in
    let final_body, st_after_body = new_body in
    let new_func_code = Anf_comp_expr (Comp_func (new_params, final_body)) in
    let new_func_def =
      Anf_str_value (Expression.Nonrecursive, code_name, new_func_code)
    in
    let st_final =
      { st_after_body with new_defs = new_func_def :: st_after_body.new_defs }
    in
    (* Create an expression that allocates memory for the closure and initializes it. )
    ( [code_address, value_fv1, value_fv2, ...] *)
    let closure_alloc =
      Comp_alloc (Imm_ident code_name :: List.map (fun v -> Imm_ident v) free_vars)
    in
    Anf_comp_expr closure_alloc, st_final
  | Comp_app (func, args) ->
    (* Closure call. func is a variable containing a pointer to the closure block.
    Transformation: f(x) =>
    let clo = f in
    let code_ptr = clo[0] in
    code_ptr(clo, x)
    *)
    let temp_closure = "t_clo" in
    let temp_code = "t_code" in
    let call_expr =
      Anf_let
        ( Expression.Nonrecursive
        , temp_closure
        , Comp_imm func
        , Anf_let
            ( Expression.Nonrecursive
            , temp_code
            , Comp_load (Imm_ident temp_closure, 0)
            , Anf_comp_expr
                (Comp_app (Imm_ident temp_code, Imm_ident temp_closure :: args)) ) )
    in
    call_expr, st
  | Comp_branch (cond, then_, else_) ->
    let then', st' = cc_anf_expr bound_vars st then_ in
    let else', st'' = cc_anf_expr bound_vars st' else_ in
    Anf_comp_expr (Comp_branch (cond, then', else')), st''
  | Comp_imm _ | Comp_binop _ | Comp_alloc _ | Comp_load _ | Comp_tuple _ ->
    Anf_comp_expr cexpr, st
;;

let cc_structure_item st item =
  let bound_vars = S.empty in
  match item with
  | Anf_str_eval expr ->
    let expr', st' = cc_anf_expr bound_vars st expr in
    Anf_str_eval expr', st'
  | Anf_str_value (rec_flag, name, expr) ->
    (* Global let-bindings are added to bound_vars. *)
    let bound_vars' = S.add name bound_vars in
    let expr', st' = cc_anf_expr bound_vars' st expr in
    Anf_str_value (rec_flag, name, expr'), st'
;;

(**
 * @param program ANF program.
 * @return cc. 
*)
let cc_program (program : aprogram) : aprogram =
  let st, transformed_items =
    List.fold_left
      (fun (st_acc, items_acc) item ->
         let item', st' = cc_structure_item st_acc item in
         st', item' :: items_acc)
      (initial_state, [])
      program
  in
  let final_items = List.rev transformed_items in
  st.new_defs @ final_items
;;
