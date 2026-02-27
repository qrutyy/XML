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

val infer_exp : (ident * typ) list -> Expression.t -> (ident * typ) list * typ
