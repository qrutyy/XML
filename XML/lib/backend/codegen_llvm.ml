(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Common.Ast
open Target

let context = Llvm.global_context ()
let i64_type = Llvm.i64_type context
let gl_align = Target.word_size
let void_type = Llvm.void_type context
let gcheader_type = Llvm.struct_type context [| i64_type |]
let block_elms_type = Llvm.array_type i64_type 0
let block_type = Llvm.struct_type context [| gcheader_type; i64_type; block_elms_type |]
let ptr_type = Llvm.pointer_type context
let builder = Llvm.builder context
let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 32

module FuncMap = struct
  module K = struct
    type t = ident

    let compare = Stdlib.compare
  end

  module M = Map.Make (K)

  type status =
    | User
    | Lib

  type t = (Llvm.llvalue * Llvm.lltype * status) M.t

  let empty () : t = M.empty

  let bind (t : t) (x : ident) (ll : Llvm.llvalue * Llvm.lltype * status) : t =
    M.add x ll t
  ;;

  let find (t : t) (x : ident) : (Llvm.llvalue * Llvm.lltype * status) option =
    M.find_opt x t
  ;;

  let find_exn (t : t) (x : ident) : Llvm.llvalue * Llvm.lltype * status = M.find x t
  let keys = M.bindings

  let print_fmap (t : t) =
    let _ =
      List.map
        (fun (id, (fval, _, s)) ->
           let stat =
             match s with
             | User -> "user"
             | Lib -> "lib"
           in
           Format.printf
             "Id: %s Arity: %d Status: %s\n"
             id
             (Array.length (Llvm.params fval))
             stat)
        (keys t)
    in
    ()
  ;;
end

(* Return types from runtime.c *)
let initial_fmap the_mod =
  let decl fmap id retty argtyps =
    let ftyp = Llvm.function_type retty argtyps in
    let fval = Llvm.declare_function id ftyp the_mod in
    FuncMap.bind fmap id (fval, ftyp, FuncMap.Lib)
  in
  let fmap = FuncMap.empty () in
  let fmap = decl fmap "print_int" void_type [| i64_type |] in
  let fmap = decl fmap "alloc_block" i64_type (*ptr*) [| i64_type |] in
  let fmap =
    decl fmap "alloc_closure" i64_type (*ptr*) [| i64_type (*ptr*); i64_type |]
  in
  let fmap =
    decl fmap "apply1" i64_type (*ptr or int*) [| i64_type (*ptr*); i64_type |]
  in
  let fmap = decl fmap "print_gc_status" void_type [||] in
  let fmap = decl fmap "collect" void_type [||] in
  let fmap = decl fmap "create_tuple" i64_type (*ptr*) [| i64_type |] in
  let fmap =
    decl fmap "create_tuple_init" i64_type (*ptr*) [| i64_type; i64_type (*ptr*) |]
  in
  let fmap = decl fmap "field" i64_type (*ptr or int*) [| i64_type (*ptr*); i64_type |] in
  let fmap = decl fmap "rt_init" void_type [| i64_type |] in
  fmap
;;

let build_call_mb_void ftype fval argvs name =
  match Llvm.return_type ftype with
  | ty when ty = void_type ->
    let _ = Llvm.build_call ftype fval argvs "" builder in
    Llvm.const_int i64_type 0
  | _ -> Llvm.build_call ftype fval argvs name builder
;;

let decl_and_bind fmap the_mod id retty argc =
  match FuncMap.find fmap id with
  | Some (_, _, FuncMap.Lib) -> fmap
  | _ when argc = 0 -> fmap
  | _ ->
    let argtyps = Array.make argc i64_type in
    let ftyp = Llvm.function_type retty argtyps in
    let fval = Llvm.declare_function id ftyp the_mod in
    FuncMap.bind fmap id (fval, ftyp, FuncMap.User)
;;

let prefill_fmap (fmap0 : FuncMap.t) the_mod (program : aprogram) : FuncMap.t =
  List.fold_left
    (fun fm -> function
       | Anf_str_value (_rf, name, anf_expr) ->
         (match anf_expr with
          | Anf_let (_, _, Comp_func (ps, _), _) | Anf_comp_expr (Comp_func (ps, _)) ->
            decl_and_bind fm the_mod name i64_type (List.length ps)
          | _ -> decl_and_bind fm the_mod name i64_type 0)
       | _ -> fm)
    fmap0
    program
;;

(* for debug *)
let _print_untag fmap n =
  let pival, pityp, _ = FuncMap.find_exn fmap "print_int" in
  let _ = build_call_mb_void pityp pival [| n |] "_" in
  ()
;;

let build_alloc_closure fmap func =
  let acval, actyp, _ = FuncMap.find_exn fmap "alloc_closure" in
  let argc = Array.length (Llvm.params func) in
  let argc = Llvm.const_int i64_type argc in
  let func_as_i64 = Llvm.build_pointercast func i64_type "func_as_i64" builder in
  (* _print_untag fmap func_as_i64;
  _print_untag fmap argc; *)
  Llvm.build_call actyp acval [| func_as_i64; argc |] "closure_tmp" builder
;;

let gen_im_expr_ir fmap = function
  | Imm_num n -> Llvm.const_int i64_type ((n lsl 1) lor 1)
  | Imm_ident id ->
    (match Hashtbl.find_opt named_values id with
     | Some v ->
       let temp = Llvm.build_load i64_type v id builder in
       Llvm.set_alignment gl_align temp;
       temp
     | None ->
       (match FuncMap.find fmap id with
        | Some (fval, ftyp, _) ->
          if Array.length (Llvm.params fval) = 0
          then build_call_mb_void ftyp fval [||] "calltmp"
          else
            (* return a pointer to a closure *)
            build_alloc_closure fmap fval
        | None -> invalid_arg ("Name not bound: " ^ id)))
;;

let create_entry_alloca the_fun var_name =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_fun)) in
  Llvm.build_alloca i64_type var_name builder
;;

(* working with tagged integers *)
let gen_tagged_binop fmap op lhs rhs =
  let left = gen_im_expr_ir fmap lhs in
  let right = gen_im_expr_ir fmap rhs in
  let one = Llvm.const_int i64_type 1 in
  (* let build_oper, name = *)
  match op with
  | "+" ->
    (* Llvm.build_add, "addtmp" *)
    let temp = Llvm.build_add left right "addtmp1" builder in
    Llvm.build_sub temp one "addtmp2" builder
  | "-" ->
    let temp = Llvm.build_sub left right "subtmp1" builder in
    Llvm.build_add temp one "subtmp2" builder
  | "*" ->
    let left' = Llvm.build_lshr left one "multmp1" builder in
    let right' = Llvm.build_sub right one "multmp2" builder in
    let temp = Llvm.build_mul left' right' "multmp3" builder in
    Llvm.build_add temp one "multmp4" builder
  | "/" ->
    let left' = Llvm.build_lshr left one "divtmp1" builder in
    let right' = Llvm.build_lshr right one "divtmp2" builder in
    let temp = Llvm.build_sdiv left' right' "divtmp3" builder in
    let temp1 = Llvm.build_add temp temp "divtmp4" builder in
    Llvm.build_add temp1 one "divtmp5" builder
  | "<" ->
    (* if we don't extend, Llvm will generate store i1 instead of store i64
      and this will lead to strange behaviour *)
    let temp = Llvm.build_icmp Llvm.Icmp.Slt left right "slttmp" builder in
    Llvm.build_zext temp i64_type "slttmp_as_i64" builder
  | "<=" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Sle left right "sletmp" builder in
    Llvm.build_zext temp i64_type "sletmp_as_i64" builder
  | ">" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Sgt left right "sgttmp" builder in
    Llvm.build_zext temp i64_type "sgttmp_as_i64" builder
  | ">=" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Sge left right "sgetmp" builder in
    Llvm.build_zext temp i64_type "sgetmp_as_i64" builder
  | "=" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Eq left right "eqtmp" builder in
    Llvm.build_zext temp i64_type "eqtmp_as_i64" builder
  | "<>" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Ne left right "neqtmp" builder in
    Llvm.build_zext temp i64_type "neqtmp_as_i64" builder
  | _ -> invalid_arg ("Unsupported binary operator: " ^ op)
;;

let build_apply_part fmap fclos args =
  let apval, aptyp, _ = FuncMap.find_exn fmap "apply1" in
  List.fold_left
    (fun clos arg ->
       let clos_as_i64 = Llvm.build_pointercast clos i64_type "clos_as_i64" builder in
       (* _print_untag fmap clos_as_i64;
       _print_untag fmap arg; *)
       build_call_mb_void aptyp apval [| clos_as_i64; arg |] "apptmp")
    fclos
    args
;;

let rec gen_comp_expr_ir fmap = function
  | Comp_imm imm -> gen_im_expr_ir fmap imm
  | Comp_binop (op, lhs, rhs) -> gen_tagged_binop fmap op lhs rhs
  | Comp_app (Imm_ident f, args) ->
    (* Format.printf "Id: %s got called with %d args\n" f (List.length args); *)
    (match FuncMap.find fmap f with
     | Some (fval, ftype, _) ->
       let pvs = Llvm.params fval in
       let argvs = List.map (fun arg -> gen_im_expr_ir fmap arg) args in
       if List.length args = Array.length pvs
       then
         build_call_mb_void ftype fval (Array.of_list argvs) "calltmp"
         (* build_apply fmap fval argvs *)
       else (
         let fclos = build_alloc_closure fmap fval in
         build_apply_part fmap fclos argvs)
     | _ ->
       (* maybe it's a closure in this scope *)
       (match Hashtbl.find_opt named_values f with
        | Some clos_ptr ->
          let clos_val = Llvm.build_load i64_type clos_ptr (f ^ "_val") builder in
          Llvm.set_alignment gl_align clos_val;
          let argvs = List.map (fun arg -> gen_im_expr_ir fmap arg) args in
          build_apply_part fmap clos_val argvs
        | _ -> invalid_arg ("Id: " ^ f ^ " not found")))
  | Comp_app (Imm_num _, _) -> invalid_arg "cannot apply number as a function"
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
  | Comp_alloc imms | Comp_tuple imms ->
    let ctval, cttyp, _ = FuncMap.find_exn fmap "create_tuple_init" in
    let argc = Llvm.const_int i64_type (List.length imms) in
    (* let ret = Llvm.build_call cttyp ctval [| argc |] "tuple_ret" builder in *)
    (* let ptr = Llvm.build_inttoptr ret ptr_type "tuple_ptr" builder in *)
    let argv = List.map (fun im -> gen_im_expr_ir fmap im) imms in
    let alloca = Llvm.build_array_alloca i64_type argc "tuple_vals_alloca" builder in
    List.iteri
      (fun i elem ->
         let ptr_to_elem =
           Llvm.build_gep
             i64_type
             alloca
             [| Llvm.const_int i64_type i |]
             "ptr_to_elem"
             builder
         in
         let store = Llvm.build_store elem ptr_to_elem builder in
         Llvm.set_alignment gl_align store;
         ())
      argv;
    let alloca_as_i64 = Llvm.build_pointercast alloca i64_type "alloca_as_i64" builder in
    Llvm.build_call cttyp ctval [| argc; alloca_as_i64 |] "tuple_tmp" builder
  | Comp_load (imexpr, offset) ->
    (*addr of the tuple *)
    let vbase = gen_im_expr_ir fmap imexpr in
    let voffst = Llvm.const_int i64_type (offset / Target.word_size) in
    let fifn, fity, _ = FuncMap.find_exn fmap "field" in
    Llvm.build_call fity fifn [| vbase; voffst |] "load_tmp" builder
  | Comp_func (_, _) -> invalid_arg "anonymous functions should be lambda-lifted"

and gen_anf_expr fmap = function
  | Anf_comp_expr comp -> gen_comp_expr_ir fmap comp
  | Anf_let (_, name, comp_expr, body) ->
    let init_val = gen_comp_expr_ir fmap comp_expr in
    let the_fun = Llvm.block_parent (Llvm.insertion_block builder) in
    let alloca = create_entry_alloca the_fun name in
    let store = Llvm.build_store init_val alloca builder in
    Llvm.set_alignment gl_align store;
    Hashtbl.add named_values name alloca;
    gen_anf_expr fmap body
;;

let gen_function fmap the_mod name params body =
  Hashtbl.clear named_values;
  let param_types = Array.map (fun _ -> i64_type) (Array.of_list params) in
  let f_type = Llvm.function_type i64_type param_types in
  let the_fun =
    match Llvm.lookup_function name the_mod with
    | None -> Llvm.declare_function name f_type the_mod
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
       let store = Llvm.build_store ai alloca builder in
       Llvm.set_alignment gl_align store;
       Hashtbl.replace named_values name alloca)
    (Llvm.params the_fun);
  (* Need to check for error here *)
  let ret_val = gen_anf_expr fmap body in
  let _ = Llvm.build_ret ret_val builder in
  if Llvm_analysis.verify_function the_fun
  then ()
  else (
    Stdlib.Format.printf
      "invalid function generated\n%s\n"
      (Llvm.string_of_llvalue the_fun);
    Llvm_analysis.assert_valid_function the_fun);
  the_fun
;;

let gen_astructure_item fmap the_mod = function
  | Anf_str_eval expr -> gen_anf_expr fmap expr
  | Anf_str_value (_, name, Anf_comp_expr (Comp_func (params, body))) ->
    gen_function fmap the_mod name params body
  | Anf_str_value (_, name, expr) ->
    let main_fn =
      match Llvm.lookup_function "main" the_mod with
      | Some fn -> fn
      | _ -> invalid_arg ("cannot generate value: " ^ name ^ ", main function not found")
    in
    Llvm.position_at_end (Llvm.entry_block main_fn) builder;
    let value = gen_anf_expr fmap expr in
    let alloca = create_entry_alloca main_fn name in
    Hashtbl.add named_values name alloca;
    let store = Llvm.build_store value alloca builder in
    Llvm.set_alignment gl_align store;
    store
;;

let optimize_ir the_mod (triple : string) (opt : string option) =
  let target = Llvm_target.Target.by_triple triple in
  let machine = Llvm_target.TargetMachine.create ~triple target in
  let opts = Llvm_passbuilder.create_passbuilder_options () in
  let optflag =
    match opt with
    | Some opt -> opt
    | _ -> "O0"
  in
  let optflag = "default<" ^ optflag ^ ">" in
  (match Llvm_passbuilder.run_passes the_mod optflag machine opts with
   | Error e -> invalid_arg e
   | Ok () -> ());
  Llvm_passbuilder.dispose_passbuilder_options opts
;;

let gen_program_ir (program : aprogram) (triple : string) (opt : string option) =
  let the_module = Llvm.create_module context "main" in
  Llvm_all_backends.initialize ();
  Llvm.set_target_triple triple the_module;
  assert (Llvm_executionengine.initialize ());
  let fmap = prefill_fmap (initial_fmap the_module) the_module program in
  (* FuncMap.print_fmap fmap; *)
  let main_ty = Llvm.function_type i64_type [||] in
  let main_fn = Llvm.define_function "main" main_ty the_module in
  Llvm.position_at_end (Llvm.entry_block main_fn) builder;
  let initfn, initty, _ = FuncMap.find_exn fmap "rt_init" in
  let _ =
    build_call_mb_void initty initfn [| Llvm.const_int i64_type (5 * 1024) |] "inittmp"
  in
  let _ = List.map (gen_astructure_item fmap the_module) program in
  let bbs = Llvm.basic_blocks main_fn in
  Llvm.position_at_end bbs.(Array.length bbs - 1) builder;
  let col_fn, col_ty, _ = FuncMap.find_exn fmap "collect" in
  let _ = build_call_mb_void col_ty col_fn [||] "_" in
  let _ = Llvm.build_ret (Llvm.const_int i64_type 0) builder in
  optimize_ir the_module triple opt;
  match Llvm_analysis.verify_module the_module with
  | Some r -> invalid_arg r
  | None -> Llvm.string_of_llmodule the_module
;;
