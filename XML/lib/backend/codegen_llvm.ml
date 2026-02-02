(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Common.Ast

(* Don't forget about tagging ints *)

let context = Llvm.global_context ()
let i64_type = Llvm.i64_type context
let void_type = Llvm.void_type context
let default_type = i64_type (* *)
let ptr_type = Llvm.pointer_type context
let builder = Llvm.builder context
let the_module = Llvm.create_module context "main"
let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 32

module FuncMap = struct
  module K = struct
    type t = ident

    let compare = Stdlib.compare
  end

  module M = Map.Make (K)

  type t = (Llvm.llvalue * Llvm.lltype) M.t

  let empty () : t = M.empty
  let bind (t : t) (x : ident) (ll : Llvm.llvalue * Llvm.lltype) : t = M.add x ll t
  let find (t : t) (x : ident) : (Llvm.llvalue * Llvm.lltype) option = M.find_opt x t
  let keys = M.bindings
end

let decl_and_bind fmap id retty argc =
  let argtyps = Array.make argc i64_type in
  let ftyp = Llvm.function_type retty argtyps in
  let fval = Llvm.declare_function id ftyp the_module in
  FuncMap.bind fmap id (fval, ftyp)
;;

(* Return types from runtime.c *)
let initial_fmap =
  let fmap = FuncMap.empty () in
  let fmap = decl_and_bind fmap "print_int" void_type 1 in
  let fmap = decl_and_bind fmap "alloc_block" i64_type 1 in
  let fmap = decl_and_bind fmap "alloc_closure" i64_type 1 in
  let fmap = decl_and_bind fmap "apply1" i64_type 2 in
  let fmap = decl_and_bind fmap "print_gc_status" void_type 0 in
  let fmap = decl_and_bind fmap "collect" void_type 0 in
  let fmap = decl_and_bind fmap "create_tuple" i64_type 1 in
  let fmap = decl_and_bind fmap "field" i64_type 2 in
  fmap
;;

let prefill_fmap (fmap0 : FuncMap.t) (program : aprogram) : FuncMap.t =
  List.fold_left
    (fun fm -> function
       | Anf_str_value (_rf, name, anf_expr) ->
         (match anf_expr with
          | Anf_let (_, _, Comp_func (ps, _), _)
          (* FuncMap.bind am name (typ (List.length ps)) *)
          (* decl_and_bind fm name i64_type (List.length ps) *)
          | Anf_comp_expr (Comp_func (ps, _)) ->
            decl_and_bind fm name i64_type (List.length ps)
            (* FuncMap.bind am name (typ (List.length ps)) *)
          | _ ->
            (* FuncMap.bind am name (typ 0) *)
            decl_and_bind fm name i64_type 0)
       | _ -> fm)
    fmap0
    program
;;

let gen_im_expr_ir fmap = function
  | Imm_num n -> Llvm.const_int i64_type n
  | Imm_ident id ->
    (match Hashtbl.find_opt named_values id with
     | Some v -> Llvm.build_load default_type v id builder
     | None ->
       (match FuncMap.find fmap id with
        | Some _ ->
          (* create a pointer to the code of id and take its number of args
            then create a closure and return created call to "alloc_closure"
          *)
          let _ =
            match Llvm.lookup_function id the_module with
            | Some _ -> failwith "not impl some _"
            | None -> failwith "not impl none"
          in
          failwith "aaa"
          (* in *)
          (* let cptr = Llvm.build_pointercast "codeptrtmp" builder *)
        | None -> invalid_arg ("Name not bound: " ^ id)))
;;

let create_entry_alloca the_fun var_name =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_fun)) in
  Llvm.build_alloca i64_type var_name builder
;;

let rec gen_comp_expr_ir fmap = function
  | Comp_imm imm -> gen_im_expr_ir fmap imm
  | Comp_binop (op, lhs, rhs) ->
    let lhs_val = gen_im_expr_ir fmap lhs in
    let rhs_val = gen_im_expr_ir fmap rhs in
    let build_oper, name =
      match op with
      | "+" -> Llvm.build_add, "addtmp"
      | "-" -> Llvm.build_sub, "subtmp"
      | "*" -> Llvm.build_mul, "multmp"
      | "/" -> Llvm.build_sdiv, "divtmp"
      | "<" -> Llvm.build_icmp Llvm.Icmp.Slt, "slttmp"
      | "<=" -> Llvm.build_icmp Llvm.Icmp.Sle, "sletmp"
      | ">" -> Llvm.build_icmp Llvm.Icmp.Sgt, "sgttmp"
      | ">=" -> Llvm.build_icmp Llvm.Icmp.Sge, "sgetmp"
      | "=" -> Llvm.build_icmp Llvm.Icmp.Eq, "eqtmp"
      | "<>" -> Llvm.build_icmp Llvm.Icmp.Ne, "neqtmp"
      | _ -> invalid_arg ("Unsupported binary operator: " ^ op)
    in
    build_oper lhs_val rhs_val name builder
  | Comp_app (Imm_ident f, args) ->
    let f_val =
      match Llvm.lookup_function f the_module with
      | Some name -> name
      | None -> invalid_arg ("Undefined function: " ^ f)
    in
    let _ =
      if Int.equal (List.length args) (Array.length (Llvm.params f_val))
      then ()
      else invalid_arg ("Invalid parameter num for function: " ^ f)
    in
    let arg_vals = Array.map (gen_im_expr_ir fmap) (Array.of_list args) in
    let arg_types = Array.map Llvm.type_of arg_vals in
    let f_type = Llvm.function_type default_type arg_types in
    Llvm.build_call f_type f_val arg_vals "calltmp" builder
  | Comp_branch (cond, br_then, br_else) ->
    let cv = gen_im_expr_ir fmap cond in
    let zero = Llvm.const_int i64_type 0 in
    let cond_val = Llvm.build_icmp Llvm.Icmp.Ne cv zero "cond" builder in
    let start_bb = Llvm.insertion_block builder in
    let the_fun = Llvm.block_parent start_bb in
    let then_bb = Llvm.append_block context "then" the_fun in
    Llvm.position_at_end then_bb builder;
    let then_val = gen_anf_expr fmap br_then in
    let new_then_bb = Llvm.insertion_block builder in
    let else_bb = Llvm.append_block context "else" the_fun in
    Llvm.position_at_end else_bb builder;
    let else_val = gen_anf_expr fmap br_else in
    let new_else_bb = Llvm.insertion_block builder in
    let merge_bb = Llvm.append_block context "ifcont" the_fun in
    Llvm.position_at_end merge_bb builder;
    let incoming = [ then_val, new_then_bb; else_val, new_else_bb ] in
    let phi = Llvm.build_phi incoming "iftmp" builder in
    Llvm.position_at_end start_bb builder;
    let _ = Llvm.build_cond_br cond_val then_bb else_bb builder in
    Llvm.position_at_end new_then_bb builder;
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end new_else_bb builder;
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end merge_bb builder;
    phi
  (* | Comp_alloc imms | Comp_tuple imms ->
    let imm_vals = List.map (fun i -> gen_im_expr_ir fmap i) imms in
    Llvm.build_call ty fn imm_vals "tupletmp" builder in
    failwith "a" *)
  | _ -> failwith "not implemented"

and gen_anf_expr fmap = function
  | Anf_comp_expr comp -> gen_comp_expr_ir fmap comp
  | Anf_let (_, name, comp_expr, body) ->
    let init_val = gen_comp_expr_ir fmap comp_expr in
    let the_fun = Llvm.block_parent (Llvm.insertion_block builder) in
    let alloca = create_entry_alloca the_fun name in
    let _ = Llvm.build_store init_val alloca builder in
    Hashtbl.add named_values name alloca;
    gen_anf_expr fmap body
;;

let gen_function fmap name params body =
  Hashtbl.clear named_values;
  let param_types = Array.map (fun _ -> default_type) (Array.of_list params) in
  let f_type = Llvm.function_type default_type param_types in
  let the_fun =
    match Llvm.lookup_function name the_module with
    | None -> Llvm.declare_function name f_type the_module
    | Some f ->
      if Array.length (Llvm.basic_blocks f) = 0
      then ()
      else invalid_arg ("Redefinition of function: " ^ name);
      if Array.length (Llvm.params f) = List.length params
      then ()
      else invalid_arg ("Redefinition of function with different number of args: " ^ name);
      f
  in
  (* build allocas and add names for parameters *)
  Array.iteri
    (fun i pval ->
       let name = List.nth params i in
       Llvm.set_value_name name pval;
       Hashtbl.add named_values name pval)
    (Llvm.params the_fun);
  let bb = Llvm.append_block context "entry" the_fun in
  Llvm.position_at_end bb builder;
  Array.iteri
    (fun i ai ->
       let name = List.nth params i in
       let alloca = create_entry_alloca the_fun name in
       let _ = Llvm.build_store ai alloca builder in
       Hashtbl.replace named_values name alloca)
    (Llvm.params the_fun);
  (* Need to check for error here *)
  let ret_val = gen_anf_expr fmap body in
  let _ = Llvm.build_ret ret_val builder in
  (* (match Llvm_analysis.verify_function the_fun with
   | true -> ()
   | false ->
     Stdlib.Format.printf
       "invalid function generated\n%s\n"
       (Llvm.string_of_llvalue the_fun);
     Llvm_analysis.assert_valid_function the_fun); *)
  the_fun
;;

(*
   let gen_variable name value =
  let the_var =
    match Llvm.lookup_global name the_module with
    | None -> Llvm.define_global name value the_module
    | Some _ -> invalid_arg ("Redefinition of a global variable: " ^ name)
  in
  Hashtbl.add named_values name the_var;
  the_var
;; *)

let gen_astructure_item fmap = function
  | Anf_str_eval expr -> gen_anf_expr fmap expr
  | Anf_str_value (_, name, Anf_comp_expr (Comp_func (params, body))) ->
    gen_function fmap name params body
  | Anf_str_value (_, name, expr) ->
    (* let value = gen_anf_expr fmap expr in
    gen_variable name value *)
    gen_function fmap name [] expr
;;

let gen_program_ir (program : aprogram) (triple : string) =
  Llvm.set_target_triple triple the_module;
  assert (Llvm_executionengine.initialize ());
  let fmap = prefill_fmap initial_fmap program in
  let _ = List.map (fun item -> gen_astructure_item fmap item) program in
  Llvm.string_of_llmodule the_module
;;
