(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan *)

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
  let () = print_int (fac 1) in
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
      mv t0, a0
      mv t2, t0
      li t1, 1
      slt t0, t1, t2
      xori t0, t0, 1
      beq t0, x0, else_0
      li a0, 1
      j end_1
    else_0:
      mv t0, a0
      mv t2, t0
      mv t0, a0
      mv t2, t0
      li t1, 1
      sub t0, t2, t1
      mv a0, t0
      call fac
      mv t1, a0
      mul a0, t2, t1
    end_1:
      ld ra, 8(fp)
      ld fp, 0(fp)
      addi sp, sp, 48
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd fp, 16(sp)
      addi fp, sp, 16
      li t0, 3
      mv a0, t0
      call fac
      mv t0, a0
      mv a0, t0
      call print_int
      li a0, 0
      ld ra, 8(fp)
      ld fp, 0(fp)
      addi sp, sp, 64
      ret
    |}]


let%expect_test "simple let" =
  let ast_factorial = parse_str "let x;;" in
  let asm = compile_to_asm ast_factorial in
  print_endline asm;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from XML_manytests__Compiler.(fun) in file "many_tests/compiler.ml", line 83, characters 22-41
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]

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
      mv t0, a0
      mv t2, t0
      li t1, 1
      slt t0, t1, t2
      xori t0, t0, 1
      beq t0, x0, else_0
      li a0, 1
      j end_1
    else_0:
      mv t0, a0
      mv t2, t0
      mv t0, a0
      mv t2, t0
      li t1, 1
      sub t0, t2, t1
      mv a0, t0
      call fac
      mv t1, a0
      mul a0, t2, t1
    end_1:
      ld ra, 8(fp)
      ld fp, 0(fp)
      addi sp, sp, 48
      ret
    |}]
