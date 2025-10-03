(** Copyright 2024, Mikhail Gavrilenko, Daniil Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | A of int (*function args*)
  | T of int (*temporary*)
  | S of int (*saved*)
  | Zero
  | Offset of reg * int [@deriving eq]
[@@deriving eq]

let rec pp_reg ppf =
  let open Format in
  function
  | A n -> fprintf ppf "a%d" n
  | T n -> fprintf ppf "t%d" n
  | S n -> fprintf ppf "s%d" n
  | Zero -> fprintf ppf "x0"
  | Offset (r, n) -> fprintf ppf "%d(%a)" n pp_reg r (* ex: -8(sp) *)
;;

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
  | Ld of reg * reg (** [ld ra, (sp)] *)
  | Sd of reg * reg
  | Mv of reg * reg
  | Beq of reg * reg * string
  | Blt of reg * reg * string
  | Ble of reg * reg * string
  | J of string
  | Label of string
  | Comment of string 
[@@deriving eq]

let rec pp_instr ppf =
  let open Format in
  function
  | Addi (r1, r2, im) -> fprintf ppf "addi %a, %a, %d" pp_reg r1 pp_reg r2 im
  | Add (rd, r1, r2) -> fprintf ppf "add %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Sub (rd, r1, r2) -> fprintf ppf "sub %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Mul (rd, r1, r2) -> fprintf ppf "mul %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Slt (rd, r1, r2) -> fprintf ppf "slt %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Xor (rd, r1, r2) -> fprintf ppf "xor %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Xori (rd, r1, im) -> fprintf ppf "xori %a, %a, %d" pp_reg rd pp_reg r1 im
  | Li (r1, n) -> fprintf ppf "li %a, %d" pp_reg r1 n
  | Ecall -> fprintf ppf "ecall"
  | Call s -> fprintf ppf "call %s" s
  | Ret -> fprintf ppf "ret"
  | Lla (r1, s) -> fprintf ppf "lla %a, %s" pp_reg r1 s
  | Ld (r1, r2) -> fprintf ppf "ld %a, %a" pp_reg r1 pp_reg r2
  | Sd (r1, r2) -> fprintf ppf "sd %a, %a" pp_reg r1 pp_reg r2
  | Mv (r1, r2) -> fprintf ppf "mv %a, %a" pp_reg r1 pp_reg r2
  | Beq (r1, r2, s) -> fprintf ppf "beq %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Blt (r1, r2, s) -> fprintf ppf "blt %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Ble (r1, r2, s) -> fprintf ppf "ble %a, %a, %s" pp_reg r1 pp_reg r2 s
  | J (s) -> fprintf ppf "j %s" s
  | Label s -> fprintf ppf "%s:" s
  | Comment s -> fprintf ppf " # %s" s
;;

let addi k r1 r2 n = k @@ Addi (r1, r2, n)
let add k rd r1 r2 = k @@ Add (rd, r1, r2)
let sub k rd r1 r2 = k @@ Sub (rd, r1, r2)
let mul k rd r1 r2 = k @@ Mul (rd, r1, r2)
let slt k rd r1 r2 = k @@ Slt (rd, r1, r2)
let xor k rd r1 r2 = k @@ Xor (rd, r1, r2)
let xori k rd r1 im = k @@ Xori (rd, r1, im)
let li k r n = k (Li (r, n))
let ecall k = k Ecall
let call k name = k (Call name)
let ret k = k Ret
let lla k a s = k (Lla (a, s))
let ld k a b = k (Ld (a, b))
let sd k a b = k (Sd (a, b))
let mv k a b = k (Mv (a, b))
let beq k r1 r2 r3 = k @@ Beq (r1, r2, r3)
let blt k r1 r2 r3 = k @@ Blt (r1, r2, r3)
let ble k r1 r2 r3 = k @@ Ble (r1, r2, r3)
let j k s = k @@ (J s)
let comment k s = k (Comment s)
let label k s = k (Label s)
