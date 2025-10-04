(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | A of int
  | T of int
  | S of int
  | Zero
  | Offset of reg * int

val equal_reg : reg -> reg -> bool
val pp_reg : Format.formatter -> reg -> unit

type instr =
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Slt of reg * reg * reg
  | Xor of reg * reg * reg
  | Xori of reg * reg * int
  | Li of reg * int
  | Ecall
  | Call of string
  | Ret
  | Lla of reg * string
  | Ld of reg * reg
  | Sd of reg * reg
  | Mv of reg * reg
  | Beq of reg * reg * string
  | Blt of reg * reg * string
  | Ble of reg * reg * string
  | J of string
  | Label of string
  | Comment of string

val pp_instr : Format.formatter -> instr -> unit
val addi : (instr -> 'a) -> reg -> reg -> int -> 'a
val add : (instr -> 'a) -> reg -> reg -> reg -> 'a
val sub : (instr -> 'a) -> reg -> reg -> reg -> 'a
val mul : (instr -> 'a) -> reg -> reg -> reg -> 'a
val slt : (instr -> 'a) -> reg -> reg -> reg -> 'a
val xor : (instr -> 'a) -> reg -> reg -> reg -> 'a
val xori : (instr -> 'a) -> reg -> reg -> int -> 'a
val li : (instr -> 'a) -> reg -> int -> 'a
val ecall : (instr -> 'a) -> 'a
val call : (instr -> 'a) -> string -> 'a
val ret : (instr -> 'a) -> 'a
val lla : (instr -> 'a) -> reg -> string -> 'a
val ld : (instr -> 'a) -> reg -> reg -> 'a
val sd : (instr -> 'a) -> reg -> reg -> 'a
val mv : (instr -> 'a) -> reg -> reg -> 'a
val beq : (instr -> 'a) -> reg -> reg -> string -> 'a
val blt : (instr -> 'a) -> reg -> reg -> string -> 'a
val ble : (instr -> 'a) -> reg -> reg -> string -> 'a
val j : (instr -> 'a) -> string -> 'a
val comment : (instr -> 'a) -> string -> 'a
val label : (instr -> 'a) -> string -> 'a
