(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf

let context = Llvm.global_context ()
let i64_type = Llvm.i64_type context
let void_type = Llvm.void_type context
let ptr_type = Llvm.pointer_type context
let builder = Llvm.builder context
let the_module = Llvm.create_module context "main"
let env : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16

let gen_im_expr_ir = function
  | Imm_num n -> Llvm.const_int i64_type n
  | Imm_ident id ->
    (match Hashtbl.find_opt env id with
     | Some v -> Llvm.build_load (Llvm.type_of v) v id builder
     | None -> invalid_arg ("Name not bound: " ^ id))
;;

let gen_comp_expr_ir = function
  | Comp_imm imm -> gen_im_expr_ir imm
  | Comp_binop (op, lhs, rhs) ->
    let lhs_val = gen_im_expr_ir lhs in
    let rhs_val = gen_im_expr_ir rhs in
    let build_oper, name =
      match op with
      | "+" -> Llvm.build_add, "addtmp"
      | "-" -> Llvm.build_sub, "subtmp"
      | "*" -> Llvm.build_mul, "multmp"
      | "/" -> Llvm.build_sdiv, "divtmp"
      | "<" -> Llvm.build_icmp Llvm.Icmp.Slt, "cmptmp"
      | "<=" -> Llvm.build_icmp Llvm.Icmp.Sle, "cmptmp"
      | ">" -> Llvm.build_icmp Llvm.Icmp.Sgt, "cmptmp"
      | ">=" -> Llvm.build_icmp Llvm.Icmp.Sge, "cmptmp"
      | "=" -> Llvm.build_icmp Llvm.Icmp.Eq, "cmptmp"
      | "<>" -> Llvm.build_icmp Llvm.Icmp.Ne, "cmptmp"
      | _ -> invalid_arg ("Unsupported binary operator: " ^ op)
    in
    build_oper lhs_val rhs_val name builder
  | _ -> failwith "not implemented"
;;

let gen_anf_expr = function
  | Anf_comp_expr comp -> gen_comp_expr_ir comp
  | Anf_let _ -> failwith "not implemented"
;;

let gen_astructure_item = function
  | Anf_str_eval expr -> gen_anf_expr expr
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
