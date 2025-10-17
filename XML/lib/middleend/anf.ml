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
  [@@deriving eq, show { with_path = false }, qcheck]

(* Complex/Computable expression *)
type comp_expr =
  | Comp_imm of im_expr
  | Comp_binop of ident * im_expr * im_expr (* x + y *)
  | Comp_app of im_expr * im_expr list (* f(x, y) *)
  | Comp_branch of im_expr * anf_expr * anf_expr (* if c then ... else ... *)
  | Comp_func of ident * anf_expr (* fun x -> ... *)
  | Comp_tuple of im_expr list
  [@@deriving eq, show { with_path = false }, qcheck]

and anf_expr =
  | Anf_comp_expr of comp_expr (* Atomic Computable Expression *)
  | Anf_let of rec_flag * ident * comp_expr * anf_expr (* let x = cexpr in anf_expr *)
  [@@deriving eq, show { with_path = false }, qcheck]
;;

type astructure_item =
  | Anf_str_eval of anf_expr
  | Anf_str_value of rec_flag * ident * anf_expr
  [@@deriving eq, show { with_path = false }, qcheck]

type aprogram = astructure_item list
  [@@deriving eq, show { with_path = false }, qcheck]

let normalise_const = fun const_expr ->
  match const_expr with
  | Const_integer e -> Imm_num e
  | Const_char e -> Imm_ident (String.make 1 e) (* i guess a standalone immediate type is redundant *)
  | Const_string e -> Imm_ident e 
;;

let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t

let get_new_temp_reg =
  let counter = ref 0 in
  fun () ->
    let name = "t_" ^ string_of_int !counter in
    counter := !counter + 1;
    name
;;


let rec normalise_expr expr cps_k =

(* basic list cps *)
    let rec normalise_list =
  fun expr_list cps_k ->
  match expr_list with
  | hd :: tl ->
    normalise_expr hd (fun result_hd ->
      normalise_list tl (fun result_tl -> cps_k (result_hd :: result_tl)))
  | [] -> cps_k []
    in

  match expr with
  | Exp_ident e -> cps_k (Imm_ident e)
  | Exp_constant c -> cps_k (normalise_const c)
  | Exp_tuple (expr1, expr2, rest_list) ->
    let all_exprs = expr1 :: expr2 :: rest_list in
    normalise_list all_exprs (fun imm_list ->
      let temp_name = get_new_temp_reg () in
      let rest_of_program = cps_k (Imm_ident temp_name) in
      Anf_let (Nonrecursive, temp_name, Comp_tuple imm_list, rest_of_program))
  | Exp_apply (Exp_ident op, Exp_tuple (expr1, expr2, []))
    when List.mem op [ "+"; "-"; "*"; "="; "<"; ">"; "<="; ">=" ] ->
    normalise_expr expr1 (fun v1 ->
      normalise_expr expr2 (fun v2 ->
        let ce =
          match v1, v2 with
          | Imm_num n1, Imm_num n2 ->
            (match op with
             | "+" -> Comp_imm (Imm_num (n1 + n2))
             | "-" -> Comp_imm (Imm_num (n1 - n2))
             | "*" -> Comp_imm (Imm_num (n1 * n2))
             (* add bool_to_int for comparing *)
             | _ -> Comp_binop (op, v1, v2))
          | _, _ -> Comp_binop (op, v1, v2)
        in
        let temp_name = get_new_temp_reg () in
        let rest_of_program = cps_k (Imm_ident temp_name) in
        Anf_let (Nonrecursive, temp_name, ce, rest_of_program)))
  | Exp_apply (_, _) ->
    let rec collect_args_and_func expr acc =
      match expr with
      | Exp_apply (f, arg) -> collect_args_and_func f (arg :: acc)
      | f -> f, acc
    in
    let func_expr, args_exprs = collect_args_and_func expr [] in
    normalise_expr func_expr (fun func_imm ->
      normalise_list args_exprs (fun args_imms ->
        let temp_name = get_new_temp_reg () in
        let rest_of_program = cps_k (Imm_ident temp_name) in
        Anf_let (Nonrecursive, temp_name, Comp_app (func_imm, args_imms), rest_of_program)))
  | Exp_if (cond, then_, Some else_) ->
    normalise_expr cond (fun cond_imm ->
      let then_anf = normalise_expr then_ (fun res -> Anf_comp_expr (Comp_imm res)) in
      let else_anf = normalise_expr else_ (fun res -> Anf_comp_expr (Comp_imm res)) in
      let temp_name = get_new_temp_reg () in
      let rest_of_program = cps_k (Imm_ident temp_name) in
      Anf_let
        ( Nonrecursive
        , temp_name
        , Comp_branch (cond_imm, then_anf, else_anf)
        , rest_of_program ))
  | Exp_fun ((Pattern.Pat_var x, []), body) ->
    let body_anf = normalise_expr body (fun res -> Anf_comp_expr (Comp_imm res)) in
    let temp_name = get_new_temp_reg () in
    let rest_of_program = cps_k (Imm_ident temp_name) in
    Anf_let (Nonrecursive, temp_name, Comp_func (x, body_anf), rest_of_program)
  | Exp_let (rec_flag, ({ pat = Pattern.Pat_var x; expr = vb_expr }, []), body) ->
    normalise_expr vb_expr (fun vb_imm ->
      (* vb_imm - result of vb_expr reduction. linking it with x. *)
      let rest_of_program = normalise_expr body cps_k in
      Anf_let (rec_flag, x, Comp_imm vb_imm, rest_of_program))
  | _ -> failwith "unsupported expression in ANF normaliser"
;;

let normalize_toplevel_body expr =
  normalise_expr expr (fun final_imm_value ->
    Anf_comp_expr (Comp_imm final_imm_value)
  )
let normalise_str_item (item : structure_item) : astructure_item =
  match item with
  | Str_eval expr ->
      let body_anf = normalize_toplevel_body expr in
      Anf_str_eval body_anf

  | Str_value (rec_flag, (first_binding, other_bindings)) ->
      if other_bindings <> [] then
        failwith "Mutually recursive `let ... and ...` bindings are not supported yet.";

      let { pat; expr } = first_binding in

      (match pat with
      | Pattern.Pat_var name ->
          let body_anf = normalize_toplevel_body expr in
          Anf_str_value (rec_flag, name, body_anf)

      | _ ->
          failwith "Unsupported pattern in a top-level let-binding. Only simple variables are allowed (e.g., `let x = ...`).")

  | _ -> failwith "Unsupported top-level structure item."

let anf_program program = map normalise_str_item program
