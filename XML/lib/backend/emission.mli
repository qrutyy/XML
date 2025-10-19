(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type loc =
  | Reg of Machine.reg
  | Stack of Machine.offset

module Emission : sig
  val code : (Machine.instr * string) Base.Queue.t
  val emit : ?comm:string -> ((Machine.instr -> unit) -> 'a) -> 'a
  val flush_queue : Format.formatter -> unit
  val emit_bin_op : string -> Machine.reg -> Machine.reg -> Machine.reg -> unit
  val spill_with_frame : ?comm:string -> Machine.reg -> loc State.State.t

  val emit_save_caller_regs
    :  (string, loc) Base.Hashtbl.t
    -> (string, loc) Base.Hashtbl.t State.State.t

  val emit_prologue : string -> int -> unit
  val emit_epilogue : int -> unit
end
