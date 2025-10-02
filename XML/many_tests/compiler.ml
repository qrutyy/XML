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
  let () = print_int (fac 4) in
  0
;;" in
  let asm = compile_to_asm ast_factorial in
  print_endline asm;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "Top-level non-function let is not supported")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from XML_manytests__Compiler.compile_to_asm in file "many_tests/compiler.ml", line 12, characters 2-37
  Called from XML_manytests__Compiler.(fun) in file "many_tests/compiler.ml", line 25, characters 12-40
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]


let%expect_test "simple let" =
  let ast_factorial = parse_str "let x = 1;;" in
  let asm = compile_to_asm ast_factorial in
  print_endline asm;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "Top-level non-function let is not supported")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
  Called from XML_manytests__Compiler.compile_to_asm in file "many_tests/compiler.ml", line 12, characters 2-37
  Called from XML_manytests__Compiler.(fun) in file "many_tests/compiler.ml", line 44, characters 12-40
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]

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