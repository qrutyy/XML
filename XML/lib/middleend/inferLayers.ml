(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Ast.Expression
open Common.Ast.Pattern

type typ =
  | Type_arrow of typ * typ
  | Type_tuple of typ List2.t
  | Type_var of tv ref
  | Quant_type_var of ident
  | Type_construct of ident * typ list [@deriving]
[@@deriving eq, show { with_path = false }]

and tv =
  | Unbound of ident
  | Link of typ
[@@deriving eq, show { with_path = false }]

let rec pprint_type_tuple ?(poly_names_map = Base.Map.empty (module Base.String)) fmt =
  let open Format in
  function
  | [] -> ()
  | [ h ] ->
    (match h with
     | Type_arrow (_, _) -> fprintf fmt "(%a)" (pprint_typ ~poly_names_map) h
     | _ -> fprintf fmt "%a" (pprint_typ ~poly_names_map) h)
  | h :: tl ->
    (match h with
     | Type_arrow (_, _) ->
       fprintf
         fmt
         "(%a) * %a"
         (pprint_typ ~poly_names_map)
         h
         (pprint_type_tuple ~poly_names_map)
         tl
     | _ ->
       fprintf
         fmt
         "%a * %a"
         (pprint_typ ~poly_names_map)
         h
         (pprint_type_tuple ~poly_names_map)
         tl)

and pprint_type_list_with_parens
      ?(poly_names_map = Base.Map.empty (module Base.String))
      fmt
      ty_list
  =
  let open Format in
  let rec print_types fmt = function
    | [] -> ()
    | [ ty ] -> (pprint_type_with_parens_if_tuple ~poly_names_map) fmt ty
    | ty :: rest ->
      fprintf
        fmt
        "%a %a"
        (pprint_type_with_parens_if_tuple ~poly_names_map)
        ty
        print_types
        rest
  in
  print_types fmt ty_list

and pprint_typ fmt ?(poly_names_map = Base.Map.empty (module Base.String)) =
  let open Format in
  function
  | Type_arrow (t1, t2) ->
    fprintf
      fmt
      "(%a -> %a)"
      (pprint_typ ~poly_names_map)
      t1
      (pprint_typ ~poly_names_map)
      t2
  | Type_tuple (t1, t2, tl) ->
    fprintf
      fmt
      "(%s)"
      (String.concat
         " * "
         (List.map
            (fun t -> asprintf "%a" (pprint_typ ~poly_names_map) t)
            (t1 :: t2 :: tl)))
  | Type_var { contents = Unbound id } ->
    (match Base.Map.find poly_names_map id with
     | Some k -> fprintf fmt "'%s" k
     | None -> fprintf fmt "'%s" id)
  | Type_var { contents = Link t } -> pprint_typ fmt t
  | Quant_type_var id -> fprintf fmt "'%s" id
  | Type_construct (name, []) -> fprintf fmt "%s" name
  | Type_construct (name, ty_list) ->
    fprintf fmt "%a %s" (pprint_type_list_with_parens ~poly_names_map) ty_list name

and pprint_type_with_parens_if_tuple
      ?(poly_names_map = Base.Map.empty (module Base.String))
      fmt
      ty
  =
  let open Format in
  match ty with
  | Type_tuple _ -> fprintf fmt "(%a)" (pprint_typ ~poly_names_map) ty
  | _ -> (pprint_typ ~poly_names_map) fmt ty
;;

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
  | _ -> failwith "cannot unify types"
;;

(* | _ -> failwith "error" *)

let rec generalize : typ -> typ = function
  | Type_var { contents = Unbound name } -> Quant_type_var name
  | Type_var { contents = Link ty } -> generalize ty
  | Type_arrow (ty1, ty2) -> Type_arrow (generalize ty1, generalize ty2)
  | Type_tuple (t1, t2, tl) ->
    Type_tuple (generalize t1, generalize t2, List.map generalize tl)
  | Type_construct (c, lst) -> Type_construct (c, List.map generalize lst)
  | ty -> ty
;;

type env = (ident * typ) list

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
    (match ty, pat with
     | Type_arrow (arg, body), Some p ->
       let new_env, new_ty = infer_pat env p in
       unify arg new_ty;
       new_env, body
     | _ -> env, inst_ty)
  (* | Pat_constraint (p, ty) ->
    let new_env, new_ty = infer_pat env p in
    unify ty new_ty;
    new_env, new_ty *)
  | _ -> failwith "infer pat not implemented"
;;

let rec infer_vb env { pat; expr } =
  let new_env, typ_p = infer_pat env pat in
  let new_env1, typ_e = infer_exp new_env expr in
  unify typ_p (generalize typ_e);
  new_env1

and infer_exp env = function
  | Exp_ident id -> env, inst (List.assoc id env)
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
    let new_env = List.fold_left (fun env bind -> infer_vb env bind) env (vb :: vbs) in
    infer_exp new_env exprb
  (* | Exp_constraint (exp, ty) ->
    let new_env, new_ty = infer_exp env exp in
    unify ty new_ty;
    new_env new_ty *)
  (* |Exp_match _ |Exp_function _ -> *)
  | _ -> failwith "infer exp not implemented"
;;

(* нужно реализовать матчи, фанкшены, леты. затем протестить,
что все работает, как надо, затем добавлять левела *)
