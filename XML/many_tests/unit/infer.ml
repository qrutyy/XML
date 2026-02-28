(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.InferTypes
open Middleend.InferLayers
open Common.Ast.Constant
open Common.Ast.Expression

(* TODO: get rid of failwith in infer *)

let inf_pprint_etyp_env ?(rst=true) env exp =
  if rst then reset_gensym ();
   let new_env, ty = infer_exp env exp in
  pprint_typ Format.std_formatter ty;
  new_env


let inf_pprint_ptyp_env ?(rst=true) env pat =
  if rst then reset_gensym ();
  let new_env, ty = infer_pat env pat in
  pprint_typ Format.std_formatter ty;
  new_env

let inf_pprint_etyp ?(rst = true) env exp = inf_pprint_etyp_env ~rst env exp |> ignore

let inf_pprint_ptyp ?(rst = true) env pat = inf_pprint_ptyp_env ~rst env pat |> ignore


let show_etyp env exp =
  let _, ty = infer_exp env exp in
  Base.print_endline (show_typ ty)

let type_bool = Type_construct ("bool", [])
let type_unit = Type_construct ("unit", [])
let type_char = Type_construct ("char", [])
let type_int = Type_construct ("int", [])
let type_string = Type_construct ("string", [])

(************************** Expressions **************************)

let%expect_test "char" =
 inf_pprint_etyp [] (Exp_constant (Const_char 'a'));
 [%expect {| char |}];;


let%expect_test "int" =
 inf_pprint_etyp [] (Exp_constant (Const_integer 1));
 [%expect {| int |}];;


let%expect_test "str" =
 inf_pprint_etyp [] (Exp_constant (Const_string "Kakadu"));
 [%expect {| string |}];;


 let%expect_test "id in env" =
 let env = ["m", (Type_var {contents = Unbound "a"})] in
 inf_pprint_etyp env (Exp_ident "m");
 [%expect {| 'a |}];;

 let%expect_test "id not in env" =
 inf_pprint_etyp [] (Exp_ident "m");
 [%expect.unreachable]
 [@@expect.uncaught_exn {|
   (* CR expect_test_collector: This test expectation appears to contain a backtrace.
      This is strongly discouraged as backtraces are fragile.
      Please change this test to not include a backtrace. *)

   Not_found
   Raised at Stdlib__List.assoc in file "list.ml", line 191, characters 10-25
   Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 249, characters 30-49
   Called from XML_unittests__Infer.inf_pprint_etyp_env in file "many_tests/unit/infer.ml", line 14, characters 21-38
   Called from XML_unittests__Infer.inf_pprint_etyp in file "many_tests/unit/infer.ml" (inlined), line 25, characters 44-76
   Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 63, characters 1-35
   Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}];;


let%expect_test "tuple 2" =
 inf_pprint_etyp [] (Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), []));
 [%expect {| (int * int) |}];;


let%expect_test "tuple 3" =
 inf_pprint_etyp [] (Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), [Exp_constant (Const_integer 3)]));
 [%expect {| (int * int * int) |}];;


let%expect_test "tuple 4" =
 inf_pprint_etyp [] (Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), [Exp_constant (Const_integer 3); Exp_constant(Const_integer 4)])) ;
 [%expect {| (int * int * int * int) |}];;


let%expect_test "tuples in tuple" =
 inf_pprint_etyp []
 (Exp_tuple(
  Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), []),
  Exp_tuple (Exp_constant (Const_integer 3), Exp_constant (Const_integer 4), []), []));
 [%expect {| ((int * int) * (int * int)) |}];;


 let%expect_test "construct none" =
 let env = ["None", Type_construct ("option", [ Quant_type_var "a" ])]  in
 inf_pprint_etyp env (Exp_construct ("None", None));
 [%expect {| 'a option |}]


 let%expect_test "construct some" =
 let env = ["Some", Type_arrow (Type_var {contents = Unbound "a" }, Type_construct("option", [Quant_type_var "a"]))] in
 inf_pprint_etyp env (Exp_construct ("Some", Some (Exp_constant (Const_integer 1))));
 [%expect {| 'a option |}]


let%expect_test "if (string) " =
  inf_pprint_etyp [] (Exp_if (Exp_constant (Const_string "trololo"), Exp_constant (Const_integer 1), None))
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: string and bool")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 288, characters 4-43
  Called from XML_unittests__Infer.inf_pprint_etyp_env in file "many_tests/unit/infer.ml", line 14, characters 21-38
  Called from XML_unittests__Infer.inf_pprint_etyp in file "many_tests/unit/infer.ml" (inlined), line 25, characters 44-76
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 115, characters 2-107
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "if (bool) then (not unit)" =
  let env = ["cond", type_bool] in
  inf_pprint_etyp env (Exp_if ((Exp_ident "cond"), Exp_constant (Const_integer 1), None));
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: int and unit")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 292, characters 7-46
  Called from XML_unittests__Infer.inf_pprint_etyp_env in file "many_tests/unit/infer.ml", line 14, characters 21-38
  Called from XML_unittests__Infer.inf_pprint_etyp in file "many_tests/unit/infer.ml" (inlined), line 25, characters 44-76
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 132, characters 2-89
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "if (bool) then (unit)" =
  let env = ["cond", type_bool; "bodyvar", type_unit] in
  inf_pprint_etyp env (Exp_if ((Exp_ident "cond"), Exp_ident "bodyvar", None));
  [%expect{| unit |}]


let%expect_test "if (bool) then 'a else 'a" =
  let env = ["cond", type_bool; "x", Type_var {contents = Unbound "a"}; "y", Type_var {contents = Unbound "a"}] in
  inf_pprint_etyp env (Exp_if ((Exp_ident "cond"), Exp_ident "x", (Some (Exp_ident "y"))));
  [%expect{| 'a |}]


let%expect_test "if (bool) then 'a else 'b" =
  let env = ["cond", type_bool; "x", Type_var {contents = Unbound "a"}; "y", Type_var {contents = Unbound "b"}] in
  inf_pprint_etyp env (Exp_if ((Exp_ident "cond"), Exp_ident "x", (Some (Exp_ident "y"))));
  [%expect{| 'b |}]


let%expect_test "apply int -> int to int" =
  let env = ["f", Type_arrow (type_int, type_int); "x", type_int] in
  inf_pprint_etyp env (Exp_apply (Exp_ident "f", Exp_ident "x") );
  [%expect{| int |}]
  

let%expect_test "apply int -> int to string" =
  let env = ["f", Type_arrow (type_int, type_int); "x", type_string] in
  inf_pprint_etyp env (Exp_apply (Exp_ident "f", Exp_ident "x") );
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure "can't unify different constructors: int and string")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 131, characters 4-15
    Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 263, characters 4-47
    Called from XML_unittests__Infer.inf_pprint_etyp_env in file "many_tests/unit/infer.ml", line 14, characters 21-38
    Called from XML_unittests__Infer.inf_pprint_etyp in file "many_tests/unit/infer.ml" (inlined), line 25, characters 44-76
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 174, characters 2-65
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "apply 'a -> 'a to 'b" =
  let env = ["f", Type_arrow (Type_var {contents = Unbound "s"}, Type_var {contents = Unbound "s"}); "x", Type_var {contents = Unbound "t"}] in
  inf_pprint_etyp env (Exp_apply (Exp_ident "f", Exp_ident "x") ) ~rst:false;
  [%expect{| 'b |}]



let%expect_test "apply 'a to 'a (different vars)" =
  let env = ["f", Type_var {contents = Unbound "t"}; "x", Type_var {contents = Unbound "t"}] in
  inf_pprint_etyp env (Exp_apply (Exp_ident "f", Exp_ident "x") ) ~rst:false;
  [%expect {| 'c |}]

(* 
let%expect_test "apply 'a to 'a (same var)" =
  let env = ["x", Type_var {contents = Unbound "t"}] in
  inf_pprint_etyp env (Exp_apply (Exp_ident "x", Exp_ident "x") ) ~rst:false;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "occurs check")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.occurs_check in file "lib/middleend/inferLayers.ml", line 114, characters 4-22
  Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 128, characters 4-22
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 263, characters 4-47
  Called from XML_unittests__Infer.inf_pprint_etyp_env in file "many_tests/unit/infer.ml", line 14, characters 21-38
  Called from XML_unittests__Infer.inf_pprint_etyp in file "many_tests/unit/infer.ml" (inlined), line 25, characters 42-74
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 178, characters 2-76
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}] *)

(* 
let%expect_test "apply 'a to 'b" =
  let env = ["f", Type_var {contents = Unbound "s"}; "x", Type_var {contents = Unbound "t"}] in
  inf_pprint_etyp env (Exp_apply (Exp_ident "f", Exp_ident "x") ) ~rst:false;
  [%expect{| 'd |}] *)


(************************** Patterns **************************)

 let%expect_test "id in env" =
 let env = ["m", (Type_var {contents = Unbound "c"})] in
 inf_pprint_ptyp env (Pat_var "m");
 [%expect {| 'a |}];;


  let%expect_test "id not in env" =
 inf_pprint_ptyp [] (Pat_var "m");
 [%expect {| 'a |}];;


   let%expect_test "any" =
 inf_pprint_ptyp [] (Pat_any);
 [%expect {| 'a |}];;


let%expect_test "char" =
 inf_pprint_ptyp [] (Pat_constant (Const_char 'a'));
 [%expect {| char |}];;


let%expect_test "int" =
 inf_pprint_ptyp[] (Pat_constant (Const_integer 1));
 [%expect {| int |}];;


let%expect_test "str" =
 inf_pprint_ptyp [] (Pat_constant (Const_string "Kakadu"));
 [%expect {| string |}];;


let%expect_test "tuple 2" =
 inf_pprint_ptyp [] (Pat_tuple (Pat_constant (Const_integer 1), Pat_constant (Const_integer 2), []));
 [%expect {| (int * int) |}];;


let%expect_test "tuple 3" =
 inf_pprint_ptyp [] (Pat_tuple (Pat_constant (Const_integer 1), Pat_constant (Const_integer 2), [Pat_constant (Const_integer 3)]));
 [%expect {| (int * int * int) |}];;


let%expect_test "tuple 4" =
 inf_pprint_ptyp [] (Pat_tuple (Pat_constant (Const_integer 1), Pat_constant (Const_integer 2), [Pat_constant (Const_integer 3); Pat_constant(Const_integer 4)])) ;
 [%expect {| (int * int * int * int) |}];;


let%expect_test "tuples in tuple" =
 inf_pprint_ptyp []
 (Pat_tuple(
  Pat_tuple (Pat_constant (Const_integer 1), Pat_constant (Const_integer 2), []),
  Pat_tuple (Pat_constant (Const_integer 3), Pat_constant (Const_integer 4), []), []));
 [%expect {| ((int * int) * (int * int)) |}];;


 let%expect_test "construct none" =
 let env = ["None", Type_construct ("option", [ Quant_type_var "a" ])]  in
 inf_pprint_ptyp env (Pat_construct ("None", None));
 [%expect {| 'a option |}]


 let%expect_test "construct some" =
 let env = ["Some", Type_arrow (Type_var {contents = Unbound "a" }, Type_construct("option", [Quant_type_var "n"]))] in
 inf_pprint_ptyp env (Pat_construct ("Some", Some (Pat_constant (Const_integer 1))));
 [%expect {| 'n option |}]


(************************** Mixed **************************)

let%expect_test "fun 'a -> 'a (new var)" =
 inf_pprint_etyp [] (Exp_fun ((Pat_var "x", []), Exp_ident "x"));
  [%expect {| ('a -> 'a) |}]


let%expect_test "fun 'a -> 'a (shadow)" =
 inf_pprint_etyp ["x", Type_var {contents = Unbound "type 's"}] (Exp_fun ((Pat_var "x", []), Exp_ident "x"));
  [%expect {| ('a -> 'a) |}]


let%expect_test "fun 'a -> 'b (not in env)" =
 inf_pprint_etyp [] (Exp_fun ((Pat_var "x", []), Exp_ident "y"));
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  Not_found
  Raised at Stdlib__List.assoc in file "list.ml", line 191, characters 10-25
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 249, characters 30-49
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 257, characters 28-49
  Called from XML_unittests__Infer.inf_pprint_etyp_env in file "many_tests/unit/infer.ml", line 14, characters 21-38
  Called from XML_unittests__Infer.inf_pprint_etyp in file "many_tests/unit/infer.ml" (inlined), line 25, characters 44-76
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 311, characters 1-64
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "fun 'a -> 'b (in env)" =
 inf_pprint_etyp ["y", Type_var {contents = Unbound "s"}] (Exp_fun ((Pat_var "x", []), Exp_ident "y"));
  [%expect{| ('a -> 's) |}]


  (* does not halt! *)
(* let%expect_test "fun x -> fun y -> x y;;" =
  inf_pprint_etyp [] (Exp_fun((Pat_var "x", []), Exp_fun((Pat_var "y", []), Exp_apply(Exp_ident "x", Exp_ident "y"))));
  [%expect{| 'a |}] *)


    (* TODO *)
  (* let%expect_test {| (fun f a b -> f a, f b) (fun x -> x) 1 "mystr" |} =
  inf_pprint_etyp [] (Exp_ident "a");
  [%expect.unreachable]
 *)

 (************************** Structure items **************************)

  (************************** Programs **************************)
(* 
  план такой: дописать тесты на все, что реализовано выше, исправить по необходимости.
  затем доделать инфер c failwith и без левелов.
  затем добавить левелы. проверить, что тесты не провалились. добавить тесты на то, что левелы работают (со странички и из презы)
  затем заменить старый инфер на новый.
  затем попытаться убрать failwith, заменяя монадами. *)