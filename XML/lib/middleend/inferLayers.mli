(** Copyright 2026, Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast

(* Need to place into Common.Ast *)
type typ =
  | Type_arrow of typ * typ
  | Type_tuple of typ List2.t
  | Type_var of tv ref
  | Quant_type_var of ident
  | Type_construct of ident * typ list

and tv =
  | Unbound of ident
  | Link of typ

val show_typ : typ -> string
val show_tv : tv -> string

val pprint_typ
  :  Format.formatter
  -> ?poly_names_map:(ident, ident, Base.String.comparator_witness) Base.Map.t
  -> typ
  -> unit

(** resets counter for type variables *)
val reset_gensym : unit -> unit

(** [infer_exp env exp] infers type of the expression [exp] in the environment [env] and returns
  updated environment and type of [exp] *)
val infer_exp : (ident * typ) list -> Expression.t -> (ident * typ) list * typ

(** [infer_pat env pat] infers type of the pattern [pat] in the environment [env] and returns
  updated environment and type of [pat] *)
val infer_pat : (ident * typ) list -> Pattern.t -> (ident * typ) list * typ

(** [infer_structure_item env item] infers type of the item [item] in the environment [env] and returns
  updated environment and type of [item] *)
val infer_structure_item
  :  (ident * typ) list
  -> Structure.structure_item
  -> (ident * typ) list

(** [infer_program env prog] infers all types in program [prog] with initial environment [env] and returns
  updated environment

  for basic environment, use [env_with_things] *)
val infer_program
  :  (ident * typ) list
  -> Structure.structure_item list
  -> (ident * typ) list

(** [env_with_things] is the basic environment that contains built-in functions and constructors *)
val env_with_things : (ident * typ) list
