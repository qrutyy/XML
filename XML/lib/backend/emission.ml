open Base
open Format
open MachineIR

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
      emit xor dst dst t
    | "<" -> emit slt rd r1 r2
    | ">" -> emit slt rd r2 r1
    | "<=" -> emit slt rd r2 r1 xori dst dst (T 1)
    | ">=" -> emit slt rd r1 r2 xori dst dst (T 1)
    | _ -> failwith "Not implemented"
  ;;
end
