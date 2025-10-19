(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Parser
open Format

let compile_to_asm ast =
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
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 1
      mv s1, t0
      li t1, 2
      add a0, s1, t1
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 5
      mv s1, t0
      li t1, 3
      sub a0, s1, t1
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 6
      mv s1, t0
      li t1, 7
      mul a0, s1, t1
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 2
      mv s1, t0
      li t1, 3
      slt t0, s1, t1
      beq t0, zero, else_0
      li a0, 11
      j end_1
    else_0:
      li a0, 22
    end_1:
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 5
      mv s1, t0
      li t1, 4
      slt t0, s1, t1
      beq t0, zero, else_0
      li a0, 1
      j end_1
    else_0:
      li a0, 0
    end_1:
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 4
      mv s1, t0
      li t1, 3
      slt t0, t1, s1
      beq t0, zero, else_0
      li a0, 7
      j end_1
    else_0:
      li a0, 9
    end_1:
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 3
      mv s1, t0
      li t1, 3
      slt t0, t1, s1
      xori t0, t0, 1
      beq t0, zero, else_0
      li a0, 10
      j end_1
    else_0:
      li a0, 20
    end_1:
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 5
      mv s1, t0
      li t1, 2
      slt t0, s1, t1
      xori t0, t0, 1
      beq t0, zero, else_0
      li a0, 8
      j end_1
    else_0:
      li a0, 9
    end_1:
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 5
      mv s1, t0
      li t1, 6
      xor t0, s1, t1
      seqz t0, t0
      beq t0, zero, else_0
      li a0, 1
      j end_1
    else_0:
      li a0, 2
    end_1:
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    inc:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t0, a0
      mv s1, t0
      li t1, 1
      add a0, s1, t1
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li a0, 41
      call inc
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
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
    .section .text
    .global main
    .type main, @function
    sum:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t0, a0
      mv s1, t0
      li t1, 1
      slt t0, t1, s1
      xori t0, t0, 1
      beq t0, zero, else_0
      j end_1
    else_0:
      mv t0, a0
      mv s1, t0
      mv t0, a0
      mv s1, t0
      li t1, 1
      sub a0, s1, t1
      addi sp, sp, -8 # Saving 'live' regs
      sd a0, -8(s0)
      call sum
      mv t1, a0
      add a0, s1, t1
    end_1:
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li a0, 5
      call sum
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;


(* --- AST для factorial + main --- *)

let%expect_test "factorial_codegen" =
  let ast_factorial = parse_str "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)

let main =
  let () = print_int (fac 3) in
  0
;;" in
  let asm = compile_to_asm ast_factorial in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    fac:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t0, a0
      mv s1, t0
      li t1, 1
      slt t0, t1, s1
      xori t0, t0, 1
      beq t0, zero, else_0
      li a0, 1
      j end_1
    else_0:
      mv t0, a0
      mv s1, t0
      mv t0, a0
      mv s1, t0
      li t1, 1
      sub a0, s1, t1
      addi sp, sp, -8 # Saving 'live' regs
      sd a0, -8(s0)
      call fac
      mv t1, a0
      mul a0, s1, t1
    end_1:
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li a0, 3
      call fac
      call print_int
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
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
  Called from XML_manytests__Compiler.(fun) in file "many_tests/compiler.ml", line 494, characters 22-41
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
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t0, a0
      mv s1, t0
      li t1, 1
      slt t0, t1, s1
      xori t0, t0, 1
      beq t0, zero, else_0
      li a0, 1
      j end_1
    else_0:
      mv t0, a0
      mv s1, t0
      mv t0, a0
      mv s1, t0
      li t1, 1
      sub a0, s1, t1
      addi sp, sp, -8 # Saving 'live' regs
      sd a0, -8(s0)
      call fac
      mv t1, a0
      mul a0, s1, t1
    end_1:
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
