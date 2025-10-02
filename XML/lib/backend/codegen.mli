(** Copyright 2024, Mikhail Gavrilenko, Daniil Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Common.Ast

val gen_program: formatter -> Structure.structure_item list -> unit

