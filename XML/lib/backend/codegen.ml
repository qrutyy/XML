(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Middleend.Anf
open Format
open Target
open Machine
open Emission.Emission

type loc =
  | Reg of reg
  | Stack_offset of int

module Env = struct
  module M = Map.Make (String)

  type t = loc M.t

  let empty () : t = M.empty
  let bind (t : t) (x : string) (loc : loc) : t = M.add x loc t
  let find (t : t) (x : string) : loc option = M.find_opt x t
  let fold (f : string -> loc -> 'a -> 'a) (t : t) (acc : 'a) : 'a = M.fold f t acc
end

module ArityMap = struct
  module K = struct
    type t = ident

    let compare = Stdlib.compare
  end

  module M = Map.Make (K)

  type t = int M.t

  let empty () : t = M.empty
  let bind (t : t) (x : ident) (arity : int) : t = M.add x arity t
  let find (t : t) (x : ident) : int option = M.find_opt x t
end

type cg_state =
  { env : Env.t
  ; stack_offset : int
  ; arity : ArityMap.t
  ; next_label : int
  ; deferred : (string * ident list * anf_expr) list
  }

type cg_error =
  [ `Unbound_identifier of string
  | `Stack_args_not_impl_direct
  | `Stack_args_not_impl_external
  | `Too_many_args of string * int * int
  | `Call_non_function
  | `Tuple_not_impl
  | `Too_many_reg_params
  ]

type 'a r = ('a, cg_error) result

let ok x = Ok x
let err e = Error e
let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

let fresh_label (prefix : string) (st : cg_state) : string * cg_state =
  let n = st.next_label in
  prefix ^ "_" ^ string_of_int n, { st with next_label = n + 1 }
;;

let gen_im_expr (state : cg_state) (dst : reg) (imm : im_expr) : unit r =
  match imm with
  | Imm_num n ->
    emit li dst n;
    ok ()
  | Imm_ident x ->
    (match Env.find state.env x with
     | Some (Reg r) ->
       if not (equal_reg r dst) then emit mv dst r;
       ok ()
     | Some (Stack_offset offset) ->
       emit ld dst (S 0, offset);
       ok ()
     | None ->
       (match ArityMap.find state.arity x with
        | Some 0 ->
          emit call x;
          if not (equal_reg (A 0) dst) then emit mv dst (A 0);
          ok ()
        | Some arity ->
          emit la (A 0) x;
          emit li (A 1) arity;
          emit call "alloc_closure";
          if not (equal_reg (A 0) dst) then emit mv dst (A 0);
          ok ()
        | None -> err (`Unbound_identifier x)))
;;

let rec gen_anf_expr (state : cg_state) (dst : reg) (aexpr : anf_expr) : cg_state r =
  match aexpr with
  | Anf_let (_rec_flag, name, comp_expr, body) ->
    let* state_after_cexpr = gen_comp_expr state (T 0) comp_expr in
    let new_offset = state_after_cexpr.stack_offset - Target.word_size in
    emit sd (T 0) (S 0, new_offset);
    let env' = Env.bind state_after_cexpr.env name (Stack_offset new_offset) in
    let new_state = { state_after_cexpr with stack_offset = new_offset; env = env' } in
    gen_anf_expr new_state dst body
  | Anf_comp_expr comp_expr -> gen_comp_expr state dst comp_expr

and gen_comp_expr (state : cg_state) (dst : reg) (cexpr : comp_expr) : cg_state r =
  match cexpr with
  | Comp_imm imm ->
    let* () = gen_im_expr state dst imm in
    ok state
  | Comp_binop (op, v1, v2) ->
    let* () = gen_im_expr state (T 0) v1 in
    let* () = gen_im_expr state (T 1) v2 in
    emit_bin_op op dst (T 0) (T 1);
    ok state
  | Comp_app (func_imm, args_imms) ->
    (* сохранить живые регистры *)
    let live_regs_to_save =
      Env.fold
        (fun _ loc acc ->
           match loc with
           | Reg r when not (equal_reg r (S 0)) -> r :: acc
           | _ -> acc)
        state.env
        []
    in
    List.iter
      (fun reg ->
         emit addi SP SP (-Target.word_size);
         emit sd reg (SP, 0))
      live_regs_to_save;
    let argc = List.length args_imms in
    let apply_chain () : unit r =
      let rec loop = function
        | [] -> ok ()
        | arg_imm :: tl ->
          let* () = gen_im_expr state (T 1) arg_imm in
          emit mv (A 0) (T 0);
          emit mv (A 1) (T 1);
          emit call "apply1";
          emit mv (T 0) (A 0);
          loop tl
      in
      loop args_imms
    in
    let* state =
      match func_imm with
      | Imm_ident fname ->
        (match Env.find state.env fname with
         | Some _ ->
           (* замыкание из окружения *)
           let* () = gen_im_expr state (T 0) func_imm in
           let* () = apply_chain () in
           ok state
         | None ->
           (match ArityMap.find state.arity fname with
            | Some n when n = 0 ->
              emit call fname;
              emit mv (T 0) (A 0);
              let* () = apply_chain () in
              ok state
            | Some n when argc = n ->
              (* аргументов хватает - делаем вызов сразу*)
              List.iter
                (fun arg_imm ->
                   let () =
                     let _ = () in
                     ()
                   in
                   emit addi SP SP (-Target.word_size);
                   let (_ : unit r) = gen_im_expr state (T 0) arg_imm in
                   emit sd (T 0) (SP, 0))
                (List.rev args_imms);
              let rec load_args i = function
                | [] -> ok ()
                | _ :: tl when i < Array.length Target.arg_regs ->
                  emit ld (A i) (SP, 0);
                  emit addi SP SP Target.word_size;
                  load_args (i + 1) tl
                | _ -> err `Stack_args_not_impl_direct
              in
              let* () = load_args 0 args_imms in
              emit call fname;
              emit mv (T 0) (A 0);
              ok state
            | Some n when argc < n ->
              (* частичное применение если аргументов меньше чем надо, в будущем чуть придется поравить чтобы создавать одно замыканние а не много*)
              let m = argc in
              if m > 0 then emit addi SP SP (-m * Target.word_size);
              let rec save_args i = function
                | [] -> ok ()
                | arg_imm :: tl ->
                  let* () = gen_im_expr state (T 1) arg_imm in
                  emit sd (T 1) (SP, i * Target.word_size);
                  save_args (i + 1) tl
              in
              let* () = save_args 0 args_imms in
              emit la (A 0) fname;
              emit li (A 1) n;
              emit call "alloc_closure";
              emit mv (T 0) (A 0);
              let rec apply_saved i = function
                | [] -> ok ()
                | _ :: tl ->
                  emit ld (T 1) (SP, i * Target.word_size);
                  emit mv (A 0) (T 0);
                  emit mv (A 1) (T 1);
                  emit call "apply1";
                  emit mv (T 0) (A 0);
                  apply_saved (i + 1) tl
              in
              let* () = apply_saved 0 args_imms in
              if m > 0 then emit addi SP SP (m * Target.word_size);
              ok state
            | Some n -> err (`Too_many_args (fname, n, argc))
            | None ->
              (* внешняя функция для того чтобы не было ошибки с вызовом рантайма *)
              List.iter
                (fun arg_imm ->
                   let (_ : unit r) = gen_im_expr state (T 0) arg_imm in
                   emit addi SP SP (-Target.word_size);
                   emit sd (T 0) (SP, 0))
                (List.rev args_imms);
              let rec load_args i = function
                | [] -> ok ()
                | _ :: tl when i < Array.length Target.arg_regs ->
                  emit ld (A i) (SP, 0);
                  emit addi SP SP Target.word_size;
                  load_args (i + 1) tl
                | _ -> err `Stack_args_not_impl_external
              in
              let* () = load_args 0 args_imms in
              emit call fname;
              emit mv (T 0) (A 0);
              ok state))
      | Imm_num _ -> err `Call_non_function
    in
    List.iter
      (fun reg ->
         emit ld reg (SP, 0);
         emit addi SP SP Target.word_size)
      (List.rev live_regs_to_save);
    if not (equal_reg dst (T 0)) then emit mv dst (T 0);
    ok state
  | Comp_branch (cond_imm, then_anf, else_anf) ->
    let* () = gen_im_expr state (T 0) cond_imm in
    let lbl_else, state = fresh_label "else" state in
    let lbl_end, state = fresh_label "endif" state in
    emit beq (T 0) Zero lbl_else;
    let* state_then = gen_anf_expr state dst then_anf in
    emit j lbl_end;
    emit label lbl_else;
    let* state_else = gen_anf_expr state_then dst else_anf in
    emit label lbl_end;
    ok state_else
  | Comp_func (params, body) ->
    (* создаём уникальный ярлык функции, добавляем в deferred *)
    let func_label, state = fresh_label "lambda" state in
    let arity' = ArityMap.bind state.arity func_label (List.length params) in
    let state =
      { state with
        arity = arity'
      ; deferred = (func_label, params, body) :: state.deferred
      }
    in
    emit la (A 0) func_label;
    emit li (A 1) (List.length params);
    emit call "alloc_closure";
    if not (equal_reg dst (A 0)) then emit mv dst (A 0);
    ok state
  | Comp_tuple _ -> err `Tuple_not_impl
;;

(* counts the number of let bindings to allocate space on stack *)
let rec count_locals_in_anf (aexpr : anf_expr) : int =
  match aexpr with
  | Anf_let (_, _, comp_expr, body) ->
    let locals_in_comp = count_locals_in_comp comp_expr in
    let locals_in_body = count_locals_in_anf body in
    max locals_in_comp (1 + locals_in_body)
  | Anf_comp_expr comp_expr -> count_locals_in_comp comp_expr

and count_locals_in_comp (cexpr : comp_expr) : int =
  match cexpr with
  | Comp_imm _ | Comp_binop _ | Comp_app _ | Comp_func _ | Comp_tuple _ -> 0
  | Comp_branch (_, then_anf, else_anf) ->
    let locals_in_then = count_locals_in_anf then_anf in
    let locals_in_else = count_locals_in_anf else_anf in
    max locals_in_then locals_in_else
;;

let gen_func
      ~arity_map
      (func_name : string)
      (params : ident list)
      (body_anf : anf_expr)
      ppf
      (st : cg_state)
  : cg_state r
  =
  let env_params_res =
    let rec go i env = function
      | [] -> ok env
      | p :: ps when i < Array.length Target.arg_regs ->
        go (i + 1) (Env.bind env p (Reg (A i))) ps
      | _ -> err `Too_many_reg_params
    in
    go 0 (Env.empty ()) params
  in
  let* env_params = env_params_res in
  let local_count = count_locals_in_anf body_anf in
  let stack_size = (2 + local_count) * Target.word_size in
  emit_prologue func_name stack_size;
  let local_state = { st with env = env_params; stack_offset = 0; arity = arity_map } in
  let* state_after = gen_anf_expr local_state (A 0) body_anf in
  flush_queue ppf;
  emit_epilogue stack_size;
  ok { st with next_label = state_after.next_label; deferred = state_after.deferred }
;;

let gen_start ppf =
  fprintf ppf ".section .text\n";
  fprintf ppf ".global main\n";
  fprintf ppf ".type main, @function\n"
;;

let prefill_arities (arity_map0 : ArityMap.t) (program : aprogram) : ArityMap.t =
  List.fold_left
    (fun am -> function
       | Anf_str_value (_rf, name, anf_expr) ->
         (match anf_expr with
          | Anf_let (_, _, Comp_func (ps, _), _) -> ArityMap.bind am name (List.length ps)
          | Anf_comp_expr (Comp_func (ps, _)) -> ArityMap.bind am name (List.length ps)
          | _ -> ArityMap.bind am name 0)
       | _ -> am)
    arity_map0
    program
;;

let gen_program_res ppf (program : aprogram) : unit r =
  let has_main =
    List.exists
      (function
        | Anf_str_value (_, "main", _) | Anf_str_eval _ -> true
        | _ -> false)
      program
  in
  if has_main then gen_start ppf;
  let arity_map = prefill_arities (ArityMap.empty ()) program in
  let st0 =
    { env = Env.empty ()
    ; stack_offset = 0
    ; arity = arity_map
    ; next_label = 0
    ; deferred = []
    }
  in
  let* st1 =
    List.fold_left
      (fun acc_res item ->
         let* st = acc_res in
         match item with
         | Anf_str_eval anf_expr -> gen_func ~arity_map "main" [] anf_expr ppf st
         | Anf_str_value (_rec_flag, name, anf_expr) ->
           let params, body =
             match anf_expr with
             | Anf_let (_, _, Comp_func (ps, b), _) -> ps, b
             | Anf_comp_expr (Comp_func (ps, b)) -> ps, b
             | _ -> [], anf_expr
           in
           gen_func ~arity_map name params body ppf st)
      (ok st0)
      program
  in
  let rec drain st =
    match st.deferred with
    | [] -> ok st
    | defs ->
      let st' = { st with deferred = [] } in
      let* st'' =
        List.fold_left
          (fun acc_res (name, ps, body) ->
             let* st_acc = acc_res in
             gen_func ~arity_map name ps body ppf st_acc)
          (ok st')
          (List.rev defs)
      in
      drain st''
  in
  let* _st_final = drain st1 in
  flush_queue ppf;
  ok ()
;;

let gen_program ppf (program : aprogram) =
  match gen_program_res ppf program with
  | Ok () -> ()
  | Error (`Unbound_identifier x) ->
    invalid_arg ("Unbound identifier during codegen: " ^ x)
  | Error `Stack_args_not_impl_direct ->
    invalid_arg "Stack arguments for direct call not implemented"
  | Error `Stack_args_not_impl_external ->
    invalid_arg "Stack arguments for external calls not implemented"
  | Error (`Too_many_args (fname, expected, got)) ->
    invalid_arg
      (Printf.sprintf
         "Too many arguments for function %s: expected %d, got %d"
         fname
         expected
         got)
  | Error `Call_non_function -> invalid_arg "Runtime error: attempted to call a number."
  | Error `Tuple_not_impl -> invalid_arg "Tuple values are not yet implemented"
  | Error `Too_many_reg_params -> invalid_arg "Too many arguments for register passing"
;;
