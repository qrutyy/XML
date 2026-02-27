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
  | Type_construct of ident * typ list

and tv =
  | Unbound of ident
  | Link of typ

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
    tv := Link t1
  | Type_arrow (l1, l2), Type_arrow (r1, r2) ->
    unify l1 r1;
    unify l2 r2
  | Type_tuple (l1, l2, ltl), Type_tuple (r1, r2, rtl) ->
    List.map2 unify (l1 :: l2 :: ltl) (r1 :: r2 :: rtl) |> ignore
  | Type_construct (lc, llst), Type_construct (rc, rlst) ->
    if lc <> rc
    then failwith "can't unify different constructors"
    else List.map2 unify llst rlst |> ignore
  | _ -> failwith "error"
;;

let rec gen : typ -> typ = function
  | Type_var { contents = Unbound name } -> Quant_type_var name
  | Type_var { contents = Link ty } -> gen ty
  | Type_arrow (ty1, ty2) -> Type_arrow (gen ty1, gen ty2)
  | Type_tuple (t1, t2, tl) -> Type_tuple (gen t1, gen t2, List.map gen tl)
  | Type_construct (c, lst) -> Type_construct (c, List.map gen lst)
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

let rec infer_exp env = function
  | Exp_ident id -> inst (List.assoc id env)
  | Exp_fun ((Pat_var id, []), exp) ->
    let typ_id = newvar () in
    let typ_exp = infer_exp ((id, typ_id) :: env) exp in
    Type_arrow (typ_id, typ_exp)
  | Exp_apply (f, arg) ->
    let typ_f = infer_exp env f in
    let typ_arg = infer_exp env arg in
    let typ_res = newvar () in
    unify typ_f (Type_arrow (typ_arg, typ_res));
    typ_res
  | Exp_let (Nonrecursive, ({ pat = Pat_var id; expr }, []), exprb) ->
    let typ_e = infer_exp env expr in
    infer_exp ((id, gen typ_e) :: env) exprb
  | _ -> failwith "infer exp not implemented"
;;
