(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val pp_print_list_with_sep
  :  sep:string
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a list
  -> unit

val pp_rec_flag : Format.formatter -> Common.Ast.Expression.rec_flag -> unit
val print_immediate : Format.formatter -> Anf.im_expr -> unit
val print_anf_expr_impl : needs_parens:bool -> Format.formatter -> Anf.anf_expr -> unit
val print_anf_expr : Format.formatter -> Anf.anf_expr -> unit
val print_anf_structure_item : Format.formatter -> Anf.astructure_item -> unit
val print_anf_program : Format.formatter -> Anf.aprogram -> unit
