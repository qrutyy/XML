(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Ast.Expression
open Common.Ast.Structure
open Common.Ast.Pattern
open Common.Ast.TypeExpr
open Common.Pprinter

let rec occurs_check tv = function
  | Type_var tv' when tv == tv' -> failwith "occurs check"
  | Type_var { contents = Link t } -> occurs_check tv t
  | Type_arrow (t1, t2) ->
    occurs_check tv t1;
    occurs_check tv t2
  | Type_tuple (t1, t2, tl) -> List.map (occurs_check tv) (t1 :: t2 :: tl) |> ignore
  | Type_construct (_, lst) -> List.map (occurs_check tv) lst |> ignore
  | _ -> ()
;;

let rec unify t1 t2 =
  match t1, t2 with
  | t1, t2 when t1 == t2 -> ()
  | Type_var { contents = Link t1 }, t2 | t1, Type_var { contents = Link t2 } ->
    unify t1 t2
  | Type_var ({ contents = Unbound _ } as tv), t'
  | t', Type_var ({ contents = Unbound _ } as tv) ->
    occurs_check tv t';
    tv := Link t'
  | Type_arrow (l1, l2), Type_arrow (r1, r2) ->
    unify l1 r1;
    unify l2 r2
  | Type_tuple (l1, l2, ltl), Type_tuple (r1, r2, rtl) ->
    if List.length ltl <> List.length rtl
    then failwith "cannot unify tuple types of different size";
    List.map2 unify (l1 :: l2 :: ltl) (r1 :: r2 :: rtl) |> ignore
  | Type_construct (lc, llst), Type_construct (rc, rlst) ->
    if lc <> rc
    then failwith ("can't unify different constructors: " ^ lc ^ " and " ^ rc)
    else List.map2 unify llst rlst |> ignore
  | Quant_type_var _, _ | _, Quant_type_var _ ->
    failwith "cannot unify with a quantified type"
  | _ ->
    failwith
      (Format.asprintf "cannot unify types: %a and %a" pprint_type t1 pprint_type t2)
;;

let rec generalize = function
  | Type_var { contents = Unbound name } -> Quant_type_var name
  | Type_var { contents = Link ty } -> generalize ty
  | Type_arrow (ty1, ty2) -> Type_arrow (generalize ty1, generalize ty2)
  | Type_tuple (t1, t2, tl) ->
    Type_tuple (generalize t1, generalize t2, List.map generalize tl)
  | Type_construct (c, lst) -> Type_construct (c, List.map generalize lst)
  | ty -> ty
;;

type env = (ident * TypeExpr.t) list

let gensym_counter = ref 0
let reset_gensym : unit -> unit = fun () -> gensym_counter := 0

let gensym : unit -> string =
  fun () ->
  let n = !gensym_counter in
  let () = incr gensym_counter in
  if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n)) else "t" ^ string_of_int n
;;

let newvar () = Type_var (ref (Unbound (gensym ())))

let inst =
  let rec loop subst = function
    | Quant_type_var name ->
      (match List.assoc_opt name subst with
       | Some typ -> typ, subst
       | None ->
         let tv = newvar () in
         tv, (name, tv) :: subst)
    | Type_var { contents = Link ty } -> loop subst ty
    | Type_arrow (t1, t2) ->
      let t1', subst = loop subst t1 in
      let t2', subst = loop subst t2 in
      Type_arrow (t1', t2'), subst
    | Type_tuple (t1, t2, tl) ->
      let t1', subst = loop subst t1 in
      let t2', subst = loop subst t2 in
      let tl'_rev, subst =
        List.fold_left
          (fun (acc, subst) t ->
             let t', subst = loop subst t in
             t' :: acc, subst)
          ([], subst)
          tl
      in
      let tl' = List.rev tl'_rev in
      Type_tuple (t1', t2', tl'), subst
    | Type_construct (constr, lst) ->
      let lst'_rev, subst =
        List.fold_left
          (fun (acc, subst) t ->
             let t', subst = loop subst t in
             t' :: acc, subst)
          ([], subst)
          lst
      in
      let lst' = List.rev lst'_rev in
      Type_construct (constr, lst'), subst
    | ty -> ty, subst
  in
  fun ty -> fst (loop [] ty)
;;

let rec infer_pat env = function
  | Pat_any ->
    let fresh = newvar () in
    env, fresh
  | Pat_var id ->
    let fresh = newvar () in
    let new_env = (id, fresh) :: env in
    new_env, fresh
  | Pat_constant const ->
    (match const with
     | Const_char _ -> env, Type_construct ("char", [])
     | Const_integer _ -> env, Type_construct ("int", [])
     | Const_string _ -> env, Type_construct ("string", []))
  | Pat_tuple (p1, p2, ptl) ->
    let new_env, ty1 = infer_pat env p1 in
    let new_env1, ty2 = infer_pat new_env p2 in
    let new_env2, tytl =
      List.fold_left
        (fun (eacc, tacc) exp ->
           let curr_env, ty = infer_pat eacc exp in
           curr_env, ty :: tacc)
        (new_env1, [])
        ptl
    in
    new_env2, Type_tuple (ty1, ty2, List.rev tytl)
  | Pat_construct (name, pat) ->
    let ty = List.assoc name env in
    let inst_ty = inst ty in
    (match inst_ty, pat with
     | Type_arrow (arg, body), Some p ->
       let new_env, new_ty = infer_pat env p in
       unify arg new_ty;
       new_env, body
     | _ -> env, inst_ty)
  | Pat_constraint (p, ty) ->
    let new_env, new_ty = infer_pat env p in
    unify ty new_ty;
    new_env, new_ty
;;

let add_rec_names env vb_list =
  List.fold_left
    (fun cenv { pat; _ } ->
       match pat with
       | Pat_var id | Pat_constraint (Pat_var id, _) ->
         let ncenv, typ_p = infer_pat cenv pat in
         (id, typ_p) :: ncenv
       | _ ->
         failwith
           "only variables are allowed as left-hand side of 'let rec' (during adding rec \
            names)")
    env
    vb_list
;;

let rec get_pat_names acc pat =
  match pat with
  | Pat_var id -> id :: acc
  | Pat_tuple (pat1, pat2, rest) ->
    Base.List.fold_left ~f:get_pat_names ~init:acc (pat1 :: pat2 :: rest)
  | Pat_construct ("Some", Some pat) -> get_pat_names acc pat
  | Pat_constraint (pat, _) -> get_pat_names acc pat
  | _ -> acc
;;

let rec infer_vb env { pat; expr } =
  (* we don't need local names *)
  let _, typ_e = infer_exp env expr in
  let new_env, typ_p = infer_pat env pat in
  unify typ_p typ_e;
  let pat_names = get_pat_names [] pat in
  let new_env1 =
    List.fold_left
      (fun env name ->
         let typ = List.assoc name env in
         let env = List.remove_assoc name env in
         (name, generalize typ) :: env)
      new_env
      pat_names
  in
  new_env1

and infer_vb_rec env { pat; expr } =
  match pat with
  | Pat_var id | Pat_constraint (Pat_var id, _) ->
    let new_env, typ_p = infer_pat env pat in
    let new_env = (id, typ_p) :: new_env in
    let new_env1, typ_e =
      match expr with
      | Exp_ident eid when id = eid ->
        failwith "this kind of expression is not allowed as right-hand side of `let rec'"
      | _ -> infer_exp new_env expr
    in
    unify typ_p typ_e;
    let pat_names = get_pat_names [] pat in
    let new_env2 =
      List.fold_left
        (fun env name ->
           let typ = List.assoc name env in
           let env = List.remove_assoc name env in
           (name, generalize typ) :: env)
        new_env1
        pat_names
    in
    new_env2
  | _ -> failwith "only variables are allowed as left-hand side of 'let rec'"

and infer_exp env = function
  | Exp_ident id ->
    (match List.assoc_opt id env with
     | Some ty -> env, inst ty
     | None -> failwith ("unbound variable: " ^ id))
  | Exp_constant const ->
    (match const with
     | Const_char _ -> env, Type_construct ("char", [])
     | Const_integer _ -> env, Type_construct ("int", [])
     | Const_string _ -> env, Type_construct ("string", []))
  | Exp_fun ((pat, pats), exp) ->
    let new_env, typ_p = infer_pat env pat in
    let newest_env, typ_exp =
      match pats with
      | hd :: tl -> infer_exp new_env (Exp_fun ((hd, tl), exp))
      | [] -> infer_exp new_env exp
    in
    newest_env, Type_arrow (typ_p, typ_exp)
  | Exp_apply (Exp_ident op, Exp_tuple (exp1, exp2, [])) ->
    (match op with
     | "*" | "/" | "+" | "-" | "<" | ">" | "=" | "<>" | "<=" | ">=" | "&&" | "||" ->
       let new_env, typ1 = infer_exp env exp1 in
       let new_env1, typ2 = infer_exp new_env exp2 in
       let arg_typ, res_typ =
         match List.assoc_opt op env with
         | Some (Type_arrow (arg, Type_arrow (_, res))) -> inst arg, inst res
         | _ -> failwith ("operator was not found in env: " ^ op)
       in
       unify typ1 arg_typ;
       unify typ2 arg_typ;
       new_env1, res_typ
     | _ ->
       let new_env, typ_op = infer_exp env (Exp_ident op) in
       let new_env1, typ_args = infer_exp new_env (Exp_tuple (exp1, exp2, [])) in
       let typ_res = newvar () in
       unify typ_op (Type_arrow (typ_args, typ_res));
       new_env1, typ_res)
  | Exp_apply (Exp_ident "-", arg) ->
    let new_env1, typ_arg = infer_exp env arg in
    unify typ_arg (Type_construct ("int", []));
    new_env1, Type_construct ("int", [])
  | Exp_apply (f, arg) ->
    let new_env, typ_f = infer_exp env f in
    let new_env1, typ_arg = infer_exp new_env arg in
    let typ_res = newvar () in
    unify typ_f (Type_arrow (typ_arg, typ_res));
    new_env1, typ_res
  | Exp_construct (name, Some exp) -> infer_exp env (Exp_apply (Exp_ident name, exp))
  | Exp_construct (name, None) -> infer_exp env (Exp_ident name)
  | Exp_tuple (e1, e2, etl) ->
    let new_env, ty1 = infer_exp env e1 in
    let new_env1, ty2 = infer_exp new_env e2 in
    let new_env2, tytl =
      List.fold_left
        (fun (eacc, tacc) exp ->
           let curr_env, ty = infer_exp eacc exp in
           curr_env, ty :: tacc)
        (new_env1, [])
        etl
    in
    new_env2, Type_tuple (ty1, ty2, List.rev tytl)
  | Exp_if (cond, the, els) ->
    let new_env, ty1 = infer_exp env cond in
    unify ty1 (Type_construct ("bool", []));
    let new_env1, ty2 = infer_exp new_env the in
    (match els with
     | None ->
       unify ty2 (Type_construct ("unit", []));
       new_env1, ty2
     | Some els ->
       let new_env, ty3 = infer_exp new_env1 els in
       unify ty2 ty3;
       new_env, ty3)
  | Exp_let (Nonrecursive, (vb, vbs), exprb) ->
    let new_env = List.fold_left (fun env bind -> infer_vb env bind) env (vb :: vbs) in
    infer_exp new_env exprb
  | Exp_let (Recursive, (vb, vbs), exprb) ->
    let new_env = add_rec_names env (vb :: vbs) in
    let new_env1 =
      List.fold_left (fun env bind -> infer_vb_rec env bind) new_env (vb :: vbs)
    in
    infer_exp new_env1 exprb
  | Exp_match (expr, (case, rest)) ->
    let new_env, typ_main = infer_exp env expr in
    let fresh = newvar () in
    let typ_res =
      List.fold_left
        (fun acc_typ curr_case ->
           let pat_names = get_pat_names [] curr_case.first in
           let pat_env, typ_pat = infer_pat new_env curr_case.first in
           unify typ_pat typ_main;
           let pat_env =
             List.fold_left
               (fun env name ->
                  let typ = List.assoc name env in
                  let env = List.remove_assoc name env in
                  (name, generalize typ) :: env)
               pat_env
               pat_names
           in
           let _, typ_exp = infer_exp pat_env curr_case.second in
           unify acc_typ typ_exp;
           acc_typ)
        fresh
        (case :: rest)
    in
    new_env, typ_res
  | Exp_function (case, rest) ->
    let fresh_p = newvar () in
    let fresh_e = newvar () in
    let typ_res =
      List.fold_left
        (fun acc_typ curr_case ->
           let env_pat, typ_pat = infer_pat env curr_case.first in
           unify typ_pat fresh_p;
           let _, typ_exp = infer_exp env_pat curr_case.second in
           unify acc_typ typ_exp;
           acc_typ)
        fresh_e
        (case :: rest)
    in
    env, Type_arrow (fresh_p, typ_res)
  | Exp_constraint (e, ty) ->
    let new_env, new_ty = infer_exp env e in
    unify ty new_ty;
    new_env, new_ty
;;

let infer_structure_item env = function
  | Str_eval exp ->
    let _, typ = infer_exp env exp in
    ("-", typ) :: env, []
  | Str_value (Nonrecursive, (vb, vbs)) ->
    let new_names =
      List.fold_left (fun names { pat; _ } -> get_pat_names names pat) [] (vb :: vbs)
    in
    let new_env = List.fold_left (fun env bind -> infer_vb env bind) env (vb :: vbs) in
    new_env, new_names
  | Str_value (Recursive, (vb, vbs)) ->
    let new_names =
      List.fold_left (fun names { pat; _ } -> get_pat_names names pat) [] (vb :: vbs)
    in
    let new_env = add_rec_names env (vb :: vbs) in
    let new_env1 =
      List.fold_left (fun env bind -> infer_vb_rec env bind) new_env (vb :: vbs)
    in
    new_env1, new_names
  | Str_adt _ -> failwith "str_adts are not supported"
;;

let infer_program env prog =
  let new_env, new_names =
    List.fold_left
      (fun (env, names) str_item ->
         let new_env, new_names = infer_structure_item env str_item in
         new_env, new_names @ names)
      (env, [])
      prog
  in
  new_env, new_names
;;

let env_with_things =
  let type_bool = Type_construct ("bool", []) in
  let type_unit = Type_construct ("unit", []) in
  let type_int = Type_construct ("int", []) in
  let things_list =
    [ "||", Type_arrow (type_bool, Type_arrow (type_bool, type_bool))
    ; "&&", Type_arrow (type_bool, Type_arrow (type_bool, type_bool))
    ; "print_int", Type_arrow (type_int, type_unit)
    ; "print_gc_status", Type_arrow (type_unit, type_unit)
    ; "collect", Type_arrow (type_unit, type_unit)
    ; "alloc_block", Type_arrow (type_int, type_unit)
    ; "+", Type_arrow (type_int, Type_arrow (type_int, type_int))
    ; "-", Type_arrow (type_int, Type_arrow (type_int, type_int))
    ; "*", Type_arrow (type_int, Type_arrow (type_int, type_int))
    ; "/", Type_arrow (type_int, Type_arrow (type_int, type_int))
    ; "=", Type_arrow (Quant_type_var "a", Type_arrow (Quant_type_var "a", type_bool))
    ; "<>", Type_arrow (Quant_type_var "a", Type_arrow (Quant_type_var "a", type_bool))
    ; "<", Type_arrow (Quant_type_var "a", Type_arrow (Quant_type_var "a", type_bool))
    ; "<=", Type_arrow (Quant_type_var "a", Type_arrow (Quant_type_var "a", type_bool))
    ; ">", Type_arrow (Quant_type_var "a", Type_arrow (Quant_type_var "a", type_bool))
    ; ">=", Type_arrow (Quant_type_var "a", Type_arrow (Quant_type_var "a", type_bool))
    ; "None", Type_construct ("option", [ Quant_type_var "a" ])
    ; ( "Some"
      , Type_arrow (Quant_type_var "a", Type_construct ("option", [ Quant_type_var "a" ]))
      )
    ; "true", type_bool
    ; "false", type_bool
    ; "()", type_unit
    ; "[]", Type_construct ("list", [ Quant_type_var "a" ])
    ; ( "::"
      , Type_arrow
          ( Type_tuple
              (Quant_type_var "a", Type_construct ("list", [ Quant_type_var "a" ]), [])
          , Type_construct ("list", [ Quant_type_var "a" ]) ) )
    ]
  in
  things_list
;;
