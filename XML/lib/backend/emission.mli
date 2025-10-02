module Emission :
  sig
    val code : (MachineIR.instr * string) Base.Queue.t
    val emit : ?comm:string -> ((MachineIR.instr -> unit) -> 'a) -> 'a
    val flush_queue : Format.formatter -> unit
    val emit_bin_op :
      string -> MachineIR.reg -> MachineIR.reg -> MachineIR.reg -> unit
    val emit_prologue : string -> int -> Format.formatter -> unit
    val emit_epilogue : Format.formatter -> unit
  end
