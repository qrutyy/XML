(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Llvm

let context = Llvm.global_context ()
let i64_type = Llvm.i64_type context
let void_type = Llvm.void_type context
let ptr_type = Llvm.pointer_type context
let builder = Llvm.builder context
let the_module = Llvm.create_module context "main"

let gen_im_expr_ir = function
  | Imm_num _ -> failwith "not implemented"
  | Imm_ident _ -> failwith "not_implemented"
;;

let gen_comp_expr_ir = function
  | _ -> failwith "not implemented"
;;

let gen_anf_expr = function
  | Anf_comp_expr _ -> failwith "not implemented"
  | Anf_let _ -> failwith "not implemented"
;;

let gen_astructure_item = function
  | Anf_str_eval _ -> failwith "not implemented"
  | _ -> failwith "not implemented"
;;

let gen_program_ir (program : aprogram) (triple : string) =
  let () = Llvm.set_target_triple triple the_module in
  let () = assert (Llvm_executionengine.initialize ()) in
  let _the_execution_engine = Llvm_executionengine.create the_module in
  let module LL = (val LL.make context builder the_module) in
  (* Fold on program here *)
  Llvm.print_module "out.ll" the_module
;;
