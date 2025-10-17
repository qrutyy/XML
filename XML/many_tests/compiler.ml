(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Parser
open Format

let to_asm ast =
  let buf = Buffer.create 1024 in
  let ppf = formatter_of_buffer buf in
  Backend.Codegen.gen_program ppf ast;
  pp_print_flush ppf ();
  Buffer.contents buf


(*--- аст для арифметичских операций*)
let%expect_test "add_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (1 + 2) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 1
      mv t2, t0
      li t1, 2
      add t0, t2, t1
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

let%expect_test "sub_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (5 - 3) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 5
      mv t2, t0
      li t1, 3
      sub t0, t2, t1
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

let%expect_test "mul_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (6 * 7) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 6
      mv t2, t0
      li t1, 7
      mul t0, t2, t1
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

(* AST для ветвления и проверки условий*)
let%expect_test "if_lt_true_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 2 < 3 then 11 else 22) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 2
      mv t2, t0
      li t1, 3
      slt t0, t2, t1
      beq t0, zero, else_0
      li t0, 11
      j end_1
    else_0:
      li t0, 22
    end_1:
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

let%expect_test "if_lt_false_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 5 < 4 then 1 else 0) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 5
      mv t2, t0
      li t1, 4
      slt t0, t2, t1
      beq t0, zero, else_0
      li t0, 1
      j end_1
    else_0:
      li t0, 0
    end_1:
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

let%expect_test "if_gt_true_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 4 > 3 then 7 else 9) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 4
      mv t2, t0
      li t1, 3
      slt t0, t1, t2
      beq t0, zero, else_0
      li t0, 7
      j end_1
    else_0:
      li t0, 9
    end_1:
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

let%expect_test "if_le_true_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 3 <= 3 then 10 else 20) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 3
      mv t2, t0
      li t1, 3
      slt t0, t1, t2
      xori t0, t0, 1
      beq t0, zero, else_0
      li t0, 10
      j end_1
    else_0:
      li t0, 20
    end_1:
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

let%expect_test "if_ge_true_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 5 >= 2 then 8 else 9) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 5
      mv t2, t0
      li t1, 2
      slt t0, t2, t1
      xori t0, t0, 1
      beq t0, zero, else_0
      li t0, 8
      j end_1
    else_0:
      li t0, 9
    end_1:
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

let%expect_test "if_eq_false_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 5 = 6 then 1 else 2) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 5
      mv t2, t0
      li t1, 6
      xor t0, t2, t1
      seqz t0, t0
      beq t0, zero, else_0
      li t0, 1
      j end_1
    else_0:
      li t0, 2
    end_1:
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

(*--- Ast для вызова функции*)

let%expect_test "simple_call_codegen" =
  let ast = parse_str {|
let inc x = x + 1

let main =
  let () = print_int (inc 41) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    inc:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t2, t0
      li t1, 1
      add a0, t2, t1
      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 48
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 41
      mv a0, t0
      call inc
      mv t0, a0
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;

let%expect_test "sum_recursion_codegen" =
  let ast = parse_str {|
let rec sum n = if n <= 1 then n else n + sum (n - 1)

let main =
  let () = print_int (sum 5) in
  0
;; |} in
  let asm = compile_to_asm ast in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    sum:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t2, t0
      li t1, 1
      slt t0, t1, t2
      xori t0, t0, 1
      beq t0, zero, else_0
      mv a0, a0
      j end_1
    else_0:
      mv t2, t0
      mv t2, t0
      li t1, 1
      sub t0, t2, t1
      mv a0, t0
      call sum
      mv t1, a0
      add a0, t2, t1
    end_1:
      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 48
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 5
      mv a0, t0
      call sum
      mv t0, a0
      mv a0, t0
      call print_int
      li a0, 0
    |}]
;;


(* --- AST для factorial + main --- *)

let%expect_test "factorial_codegen" =
  let ast_factorial = parse_str "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)

let main =
  let () = print_int (fac 2) in
  0
;;" in
  let asm = to_asm ast_factorial in
  print_endline asm;
  [%expect {|
    .global _start
    _start:
      call main
      li a7, 93
      ecall

      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    fac:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t2, t0
      li t1, 1
      slt t0, t1, t2
      xori t0, t0, 1
      beq t0, zero, else_0
      li a0, 1
      j end_1
    else_0:
      mv t2, t0
      mv t2, t0
      li t1, 1
      sub t0, t2, t1
      mv a0, t0
      call fac
      mv t1, a0
      mul a0, t2, t1
    end_1:
      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 48
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 2
      mv a0, t0
      call fac
      mv t0, a0
      mv a0, t0
      call print_int
      li a0, 0
    |}]


let%expect_test "simple let" =
  let ast_factorial = parse_str "let x;;" in
  let asm = to_asm ast_factorial in
  print_endline asm;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from XML_manytests__Compiler.(fun) in file "many_tests/compiler.ml", line 532, characters 22-41
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]

let%expect_test "factorial_basic_codegen" =
  let ast_factorial = parse_str "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
;;" in
  let asm = to_asm ast_factorial in
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


let%expect_test "fibonacci" =
  let ast_factorial = parse_str "let rec fib n = if n == 0 then 0 else n + fib(n - 1);;" in
  let asm = to_asm ast_factorial in
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

let%expect_test "fib" =
  let ast_factorial = parse_str "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2);;" in
  let asm = compile_to_asm ast_factorial in
  print_endline asm;
  [%expect {|
      ld ra, 8(s0)
      ld s0, 0(s0)
      addi sp, sp, 64
      ret
    fac:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t2, t0
      li t1, 1
      slt t0, t1, t2
      xori t0, t0, 1
      beq t0, zero, else_0
      li a0, 1
      j end_1
    else_0:
      mv t2, t0
      mv t2, t0
      li t1, 1
      sub t0, t2, t1
      mv a0, t0
      call fac
      mv t1, a0
      mul a0, t2, t1
    end_1:
    |}]
