(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

(* gens program on LLMV IR from the ast *)
val gen_program_ir : Middleend.Anf.aprogram -> string -> unit
