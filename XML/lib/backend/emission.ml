(** Copyright 2024, Mikhail Gavrilenko, Daniil Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Format
open Machine
open Target

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
      let t = T 0 in
      emit slt rd r1 r2;
      emit slt t r2 r1;
      emit xor rd rd t
    | "<" -> emit slt rd r1 r2
    | ">" -> emit slt rd r2 r1
    | "<=" ->
      emit slt rd r2 r1;
      emit xori rd rd 1
    | ">=" ->
      emit slt rd r1 r2;
      emit xori rd rd 1
    | _ -> failwith "Not implemented"
  ;;

  let emit_prologue name stack_size ppf =
    fprintf ppf "%s:\n" name;
    fprintf ppf "  addi sp, sp, -%d\n" stack_size;
    fprintf ppf "  sd ra, %d(sp)\n" (stack_size - Target.word_size);
    fprintf ppf "  sd fp, %d(sp)\n" (stack_size - (2 * Target.word_size));
    fprintf ppf "  addi fp, sp, %d\n" (stack_size - (2 * Target.word_size))
  ;;

  let emit_epilogue ppf =
    fprintf ppf "  addi sp, fp, 0\n";
    fprintf ppf "  ld ra, %d(fp)\n" Target.word_size;
    fprintf ppf "  ld fp, 0(fp)\n";
    fprintf ppf "  ret\n"
  ;;
end
