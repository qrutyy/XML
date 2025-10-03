(** Copyright 2024, Mikhail Gavrilenko, Daniil Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Parser
open Format

let compile_to_asm ast =
  let buf = Buffer.create 1024 in
  let ppf = formatter_of_buffer buf in
  Backend.Codegen.gen_program ppf ast;
  pp_print_flush ppf ();
  Buffer.contents buf

(* --- AST для factorial + main --- *)

let%expect_test "factorial_codegen" =
  let ast_factorial = parse_str "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)

let main =
  let () = print_int (fac 0) in
  0
;;" in
  let asm = compile_to_asm ast_factorial in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

    fac:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd fp, 32(sp)
      addi fp, sp, 32
      li t1, 1
      slt t0, t1, t0
      xori t0, t0, t1
      beq t0, x0, else_0
      li a0, 1
      j end_1
    else_0:
      li t1, 1
      sub t0, t0, t1
      mv a0, t0
      call fac
      mv t1, a0
      mul a0, t0, t1
    end_1:
      addi sp, fp, 0
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret
    |}]


let%expect_test "simple let" =
  let ast_factorial = parse_str "let x;;" in
  let asm = compile_to_asm ast_factorial in
  print_endline asm;
  [%expect {| |}]

let%expect_test "factorial_basic_codegen" =
  let ast_factorial = parse_str "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
;;" in
  let asm = compile_to_asm ast_factorial in
  print_endline asm;
  [%expect {|
    fac:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd fp, 32(sp)
      addi fp, sp, 32
      li t1, 1
      slt t0, t1, t0
      xori t0, t0, t1
      beq t0, x0, else_0
      li a0, 1
      j end_1
    else_0:
      li t1, 1
      sub t0, t0, t1
      mv a0, t0
      call fac
      mv t1, a0
      mul a0, t0, t1
    end_1:
      addi sp, fp, 0
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret
    |}]
