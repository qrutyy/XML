(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.InferTypes
open Middleend.InferLayers
open Common.Ast.Constant
open Common.Ast.Expression

(* TODO: get rid of failwith in infer *)

let pprint_etyp env exp =
   let _, ty = infer_exp env exp in
  pprint_typ Format.std_formatter ty;;

let show_etyp env exp =
  let _, ty = infer_exp env exp in
  Base.print_endline (show_typ ty)

let type_bool = Type_construct ("bool", [])
let type_unit = Type_construct ("unit", [])

(************************** Expressions **************************)

let%expect_test "char" =
 pprint_etyp [] (Exp_constant (Const_char 'a'));
 [%expect {| char |}];;


let%expect_test "int" =
 pprint_etyp[] (Exp_constant (Const_integer 1));
 [%expect {| int |}];;


let%expect_test "str" =
 pprint_etyp [] (Exp_constant (Const_string "Kakadu"));
 [%expect {| string |}];;


 let%expect_test "id in env" =
 let env = ["m", (Type_var {contents = Unbound "a"})] in
 pprint_etyp env (Exp_ident "m");
 [%expect {| 'a |}];;

 let%expect_test "id not in env" =
 pprint_etyp [] (Exp_ident "m");
 [%expect.unreachable]
 [@@expect.uncaught_exn {|
   (* CR expect_test_collector: This test expectation appears to contain a backtrace.
      This is strongly discouraged as backtraces are fragile.
      Please change this test to not include a backtrace. *)

   Not_found
   Raised at Stdlib__List.assoc in file "list.ml", line 191, characters 10-25
   Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 249, characters 30-49
   Called from XML_unittests__Infer.pprint_etyp in file "many_tests/unit/infer.ml", line 13, characters 15-32
   Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 46, characters 1-31
   Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}];;


let%expect_test "tuple 2" =
 pprint_etyp [] (Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), []));
 [%expect {| (int * int) |}];;


let%expect_test "tuple 3" =
 pprint_etyp [] (Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), [Exp_constant (Const_integer 3)]));
 [%expect {| (int * int * int) |}];;


let%expect_test "tuple 4" =
 pprint_etyp [] (Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), [Exp_constant (Const_integer 3); Exp_constant(Const_integer 4)])) ;
 [%expect {| (int * int * int * int) |}];;


let%expect_test "tuples in tuple" =
 pprint_etyp []
 (Exp_tuple(
  Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), []),
  Exp_tuple (Exp_constant (Const_integer 3), Exp_constant (Const_integer 4), []), []));
 [%expect {| ((int * int) * (int * int)) |}];;


 let%expect_test "construct none" =
 let env = ["None", Type_construct ("option", [ Quant_type_var "a" ])]  in
 pprint_etyp env (Exp_construct ("None", None));
 [%expect {| 'a option |}]


 let%expect_test "construct some" =
 let env = ["Some", Type_arrow (Type_var {contents = Unbound "a" }, Type_construct("option", [Quant_type_var "a"]))] in
 pprint_etyp env (Exp_construct ("Some", Some (Exp_constant (Const_integer 1))));
 [%expect {| 'b option |}]


let%expect_test "if (not bool) then a" =
  pprint_etyp [] (Exp_if (Exp_constant (Const_string "trololo"), Exp_constant (Const_integer 1), None))
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: string and bool")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 285, characters 4-43
  Called from XML_unittests__Infer.pprint_etyp in file "many_tests/unit/infer.ml", line 13, characters 15-32
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "if (bool) then (not unit)" =
  let env = ["cond", type_bool] in
  pprint_etyp env (Exp_if ((Exp_ident "cond"), Exp_constant (Const_integer 1), None));
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: int and unit")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 289, characters 7-46
  Called from XML_unittests__Infer.pprint_etyp in file "many_tests/unit/infer.ml", line 13, characters 15-32
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 112, characters 2-85
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "if (bool) then (unit)" =
  let env = ["cond", type_bool; "bodyvar", type_unit] in
  pprint_etyp env (Exp_if ((Exp_ident "cond"), Exp_ident "bodyvar", None));
  [%expect{| unit |}]


let%expect_test "if (bool) then a else a" =
  let env = ["cond", type_bool] in
  pprint_etyp env (Exp_if ((Exp_ident "cond"), Exp_constant (Const_integer 1), (Some (Exp_constant (Const_integer 2)))));
  [%expect {| int |}]


let%expect_test "if (bool) then a else b" =
  let env = ["cond", type_bool] in
  pprint_etyp env (Exp_if ((Exp_ident "cond"), Exp_constant (Const_integer 1), (Some (Exp_constant (Const_char 'a')))));
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: int and char")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 293, characters 7-20
  Called from XML_unittests__Infer.pprint_etyp in file "many_tests/unit/infer.ml", line 13, characters 15-32
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 141, characters 2-119
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]



(************************** Patterns **************************)

(************************** Mixed **************************)

 (************************** Structure items **************************)

  (************************** Programs **************************)
