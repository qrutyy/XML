(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Format
open Machine
open Target
open State

type loc =
  | Reg of reg
  | Stack of offset

module Emission = struct
  let code : (instr * string) Queue.t = Queue.create ()
  let emit ?(comm = "") push_instr = push_instr (fun i -> Queue.enqueue code (i, comm))

  let flush_queue ppf =
    while not (Queue.is_empty code) do
      let i, comm = Queue.dequeue_exn code in
      (match i with
       | Label _ -> fprintf ppf "%a" pp_instr i
       | _ -> fprintf ppf "  %a" pp_instr i);
      if String.(comm <> "") then fprintf ppf " # %s" comm;
      fprintf ppf "\n"
    done
  ;;

  let emit_bin_op op rd r1 r2 =
    match op with
    | "+" -> emit add rd r1 r2
    | "-" -> emit sub rd r1 r2
    | "*" -> emit mul rd r1 r2
    | "=" ->
      emit xor rd r1 r2;
      emit seqz rd rd
    | "<" -> emit slt rd r1 r2
    | ">" -> emit slt rd r2 r1
    | "<=" ->
      emit slt rd r2 r1;
      emit xori rd rd 1
    | ">=" ->
      emit slt rd r1 r2;
      emit xori rd rd 1
    | _ -> failwith ("Unknown binary operator: " ^ op)
  ;;

  let spill_with_frame ?(comm = "") reg =
    let open State in
    let* () = move_frame_offset (fun fr_ofs -> fr_ofs + Target.word_size) in
    let* state = get in
    let ofs = -state.frame_offset in
    emit sd reg (S 0, ofs) ~comm;
    return (Stack (S 0, ofs))
  ;;

  let emit_save_caller_regs (env : (string, loc) Hashtbl.t) =
    let open State in
    (* собрать пары (name, reg) только для caller-saved *)
    let regs =
      Hashtbl.fold env ~init:[] ~f:(fun ~key:name ~data:loc acc ->
        match loc with
        | Reg r ->
          (match r with
           | A _ | T _ -> (name, r) :: acc
           | _ -> acc)
        | _ -> acc)
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    let spill_count = List.length regs in
    let frame_size = spill_count * Target.word_size in
    if frame_size > 0 then emit addi SP SP (-frame_size) ~comm:"Saving 'live' regs";
    (* пролить каждый регистр и обновить env *)
    let rec loop = function
      | [] -> return env
      | (name, r) :: tl ->
        let* new_loc = spill_with_frame r in
        Hashtbl.set env ~key:name ~data:new_loc;
        loop tl
    in
    loop regs
  ;;

  (*миша я переписал через емит чтобы у нас вся оработка шла черз один модуль*)
  let emit_prologue name stack_size =
    (* name: *)
    emit label name;
    (* addi sp, sp, -stack_size *)
    emit addi SP SP (-stack_size);
    (* sd ra, (sp + stack_size - word) *)
    emit sd RA (SP, stack_size - Target.word_size);
    (* sd fp(S0), (sp + stack_size - 2*word) *)
    emit sd (S 0) (SP, stack_size - (2 * Target.word_size));
    (* fp := sp + stack_size - 2*word *)
    emit addi (S 0) SP (stack_size - (2 * Target.word_size))
  ;;

  let emit_epilogue =
    fun () ->
    emit addi SP (S 0) (2 * Target.word_size);
    (* sp = fp + 2*word *)
    emit ld RA (S 0, Target.word_size);
    (* ra = [fp+word] *)
    emit ld (S 0) (S 0, 0);
    (* fp = [fp+0] *)
    emit ret
  ;;
end
