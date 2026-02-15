(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

(** [gen_program_ir prog triple opt] gens program in LLMV IR from the program [prog]
for the target architecture specified by [triple] with optimization level [opt] if not None, O0 otherwise*)
val gen_program_ir : Middleend.Anf.aprogram -> string -> string option -> string
