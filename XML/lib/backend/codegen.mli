(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Common.Ast

(* gens program on riscv asm from the ast *)
val gen_program : formatter -> Structure.structure_item list -> unit
