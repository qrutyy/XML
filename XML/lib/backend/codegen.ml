(** Copyright 2024, Mikhail Gavrilenko, Daniil Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Format

let gen_program ppf (_p : program) =
  (* Minimal stub to make backend compile; expand with real codegen later *)
  fprintf ppf ".global _start\n";
  fprintf ppf "_start:\n";
  ()
;;

(*TODO: structure gen -> func_gen -> expressions_gen *)
