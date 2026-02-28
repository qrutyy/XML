(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.InferLayers
open Common.Ast.Constant
open Common.Ast.Expression
open Common.Parser

(* TODO: get rid of failwith in infer *)

let infer_exp_str ?(rst = true) ?(env = []) str =
  let exp = parse_exp_str str in
  if rst then reset_gensym ();
  let _, ty = infer_exp env exp in
  pprint_typ Format.std_formatter ty;;

let infer_pat_str ?(rst = true) ?(env = []) str =
  let pat = parse_pat_str str in
  if rst then reset_gensym ();
  let _, ty = infer_pat env pat in
  pprint_typ Format.std_formatter ty;;

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
  infer_exp_str {| 'a' |};
  [%expect{| char |}]


let%expect_test "int" =
  infer_exp_str {| 1 |};
  [%expect{| int |}]


let%expect_test "str" =
  infer_exp_str {| "Kakadu" |};
  [%expect{| string |}]


let%expect_test "id in env" =
  infer_exp_str {| m |} ~env:[("m", Type_var {contents = Unbound "a"})];
  [%expect{| 'a |}]


let%expect_test "id not in env" =
  infer_exp_str {| m |};
 [%expect.unreachable]
 [@@expect.uncaught_exn {|
   (* CR expect_test_collector: This test expectation appears to contain a backtrace.
      This is strongly discouraged as backtraces are fragile.
      Please change this test to not include a backtrace. *)

   (Failure "unbound variable: m")
   Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
   Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
   Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 57, characters 2-23
   Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}];;


let%expect_test "tuple 2" =
  infer_exp_str {| (1, 2) |};
  [%expect{| (int * int) |}]


let%expect_test "tuple 3" =
  infer_exp_str {| (1, 2, 3) |};
  [%expect{| (int * int * int) |}]


  let%expect_test "tuple 4" =
  infer_exp_str {| (1, 2, 3, 4) |};
  [%expect{| (int * int * int * int) |}]


let%expect_test "tuples in tuple" =
  infer_exp_str {| ((1, 2), (3, 4)) |};
  [%expect{| ((int * int) * (int * int)) |}]


 let%expect_test "construct none" =
 let env = ["None", Type_construct ("option", [ Quant_type_var "a" ])] in
  infer_exp_str {| None |} ~env;
 [%expect {| 'a option |}]


let%expect_test "construct some" =
 let env = ["Some", Type_arrow (Type_var {contents = Unbound "a" }, Type_construct("option", [Quant_type_var "a"]))] in
  infer_exp_str {| Some 1 |} ~env;
 [%expect {| 'a option |}]


let%expect_test "if (string) " =
  infer_exp_str {| if "trololo" then 1 |};
  [@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: string and bool")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 348, characters 4-43
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "if (bool) then (not unit)" =
  let env = ["cond", type_bool] in
  infer_exp_str {| if cond then 1 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: int and unit")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 352, characters 7-46
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 119, characters 2-41
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "if (bool) then (unit)" =
  let env = ["cond", type_bool; "bodyvar", type_unit] in
  infer_exp_str {| if cond then bodyvar |} ~env;
  [%expect{| unit |}]


let%expect_test "if (bool) then 'a else 'a" =
  let env = ["cond", type_bool; "x", Type_var {contents = Unbound "a"}; "y", Type_var {contents = Unbound "a"}] in
  infer_exp_str  {| if cond then x else y |}  ~env;
  [%expect{| 'a |}]


let%expect_test "if (bool) then 'a else 'b" =
  let env = ["cond", type_bool; "x", Type_var {contents = Unbound "a"}; "y", Type_var {contents = Unbound "b"}] in
  infer_exp_str  {| if cond then x else y |}  ~env;
  [%expect{| 'b |}]


let%expect_test "apply int -> int to int" =
  let env = ["f", Type_arrow (type_int, type_int); "x", type_int] in
  infer_exp_str  {| f x |} ~env;
  [%expect{| int |}]
  

let%expect_test "apply int -> int to string" =
  let env = ["f", Type_arrow (type_int, type_int); "x", type_string] in
  infer_exp_str {| f x |} ~env;
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure "can't unify different constructors: int and string")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 131, characters 4-15
    Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 330, characters 4-47
    Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 160, characters 2-30
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "apply 'a -> 'a to 'b" =
  let env = ["f", Type_arrow (Type_var {contents = Unbound "s"}, Type_var {contents = Unbound "s"}); "x", Type_var {contents = Unbound "t"}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect{| 'b |}]


  (* not sure if this is right *)
let%expect_test "apply 'a to 'a (different vars)" =
  let env = ["f", Type_var {contents = Unbound "t"}; "x", Type_var {contents = Unbound "t"}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect {| 'c |}]


let%expect_test "apply 'a to 'a (same var)" =
  let env = ["x", Type_var {contents = Unbound "t"}] in
  infer_exp_str {| x x |} ~env ~rst: false;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "occurs check")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.occurs_check in file "lib/middleend/inferLayers.ml", line 114, characters 4-22
  Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 128, characters 4-22
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 330, characters 4-47
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 191, characters 2-42
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "apply 'a to 'b" =
  let env = ["f", Type_var {contents = Unbound "s"}; "x", Type_var {contents = Unbound "t"}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect{| 'e |}]


let%expect_test "binary op" =
  let env = ["=", Type_arrow (Type_var {contents = Unbound "a"}, Type_arrow(Type_var {contents = Unbound "a"}, type_bool))] in
  infer_exp_str {| 1 = 1 |} ~env;
  [%expect {| bool |}]

(************************** Patterns **************************)

let%expect_test "id in env" =
  let env = ["m", (Type_var {contents = Unbound "c"})] in
  infer_pat_str {| m |} ~env;
  [%expect {| 'a |}];;


let%expect_test "id not in env" =
  infer_pat_str {| m |};
  [%expect {| 'a |}];;


let%expect_test "any" =
  infer_pat_str {| _ |};
  [%expect {| 'a |}];;


let%expect_test "char" =
  infer_pat_str {| 'a' |};
  [%expect {| char |}];;


let%expect_test "int" =
  infer_pat_str {| 1 |};
  [%expect {| int |}];;


let%expect_test "str" =
  infer_pat_str {| "kakadu" |};
  [%expect {| string |}];;


let%expect_test "tuple 2" =
  infer_pat_str {| (1, 2) |};
  [%expect {| (int * int) |}];;


let%expect_test "tuple 3" =
  infer_pat_str {| (1, 2, 3) |};
  [%expect {| (int * int * int) |}];;


let%expect_test "tuple 4" =
  infer_pat_str {| (1, 2, 3, 4) |};
   [%expect {| (int * int * int * int) |}];;


let%expect_test "tuples in tuple" =
  infer_pat_str {| ((1, 2), (3, 4)) |};
   [%expect {| ((int * int) * (int * int)) |}];;


let%expect_test "construct none" =
  let env = ["None", Type_construct ("option", [ Quant_type_var "a" ])]  in
  infer_pat_str {| None |} ~env;
  [%expect {| 'a option |}]


 let%expect_test "construct some" =
 let env = ["Some", Type_arrow (Type_var {contents = Unbound "a" },
            Type_construct("option", [Quant_type_var "n"]))] in
  infer_pat_str {| Some 1 |} ~env;
 [%expect {| 'n option |}]


(************************** Funs **************************)

let%expect_test "fun 'a -> 'a (new var)" =
  infer_exp_str {| fun x -> x |};
  [%expect {| ('a -> 'a) |}]


let%expect_test "fun 'a -> 'a (shadow)" =
  let env = ["x", Type_var {contents = Unbound "type 's"}] in
  infer_exp_str {| fun x -> x |} ~env;
  [%expect {| ('a -> 'a) |}]


let%expect_test "fun 'a -> 'b (not in env)" =
  infer_exp_str {| fun x -> y |};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "unbound variable: y")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 304, characters 14-35
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 299, characters 2-32
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "fun 'a -> 'b (in env)" =
  let env = ["y", Type_var {contents = Unbound "s"}] in
  infer_exp_str {| fun x -> y |} ~env;
  [%expect{| ('a -> 's) |}]


let%expect_test _=
  infer_exp_str {| fun x -> fun y -> x y |};
 [%expect{| (('b -> 'c) -> ('b -> 'c)) |}]


let%expect_test _ =
  infer_exp_str {| fun x y -> x y |};
  [%expect{| (('b -> 'c) -> ('b -> 'c)) |}]


let%expect_test _ =
  infer_exp_str {| (fun f a b -> f a, f b) (fun x -> x) 1 "mystr" |};
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure "can't unify different constructors: int and string")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 131, characters 4-15
    Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 330, characters 4-47
    Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 331, characters 2-68
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


(************************** Let in **************************)

let%expect_test _ =
  infer_exp_str {| let 1 = 1 in 2 |};
  [%expect{| int |}]


let%expect_test _ =
  infer_exp_str {| let a = 1 in 2 |};
  [%expect{| int |}]


let%expect_test _ =
infer_exp_str {| let a = 1 in a |};
  [%expect{| int |}]


let%expect_test _ =
  infer_exp_str {| let a = 1 in a |};
  [%expect{| int |}]


let%expect_test _ =
  infer_exp_str {| let a = 1 in "str" |};
  [%expect{| string |}]


let%expect_test _ =
  infer_exp_str {| let a = fun x -> x in a |};
  [%expect{| ('c -> 'c) |}]


let%expect_test _ =
  infer_exp_str {| let a = fun x -> x in (a 1, a "str") |};
  [%expect{| (int * string) |}]


let%expect_test _ =
  infer_exp_str  {| let a, b = 1, 2 in a |} ;
  [%expect{| int |}]


let%expect_test _ =
  infer_exp_str  {| let a, b, c = 1, 2 in a |} ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "cannot unify tuple types of different size")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 135, characters 9-62
  Called from Middleend__InferLayers.infer_vb in file "lib/middleend/inferLayers.ml", line 274, characters 2-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 359, characters 18-84
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 390, characters 2-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test _ =
  infer_exp_str {| let a, b = 1, 2, 3 in a |};
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure "cannot unify tuple types of different size")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 135, characters 9-62
    Called from Middleend__InferLayers.infer_vb in file "lib/middleend/inferLayers.ml", line 274, characters 2-32
    Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
    Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 359, characters 18-84
    Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 409, characters 2-45
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "and" =
  infer_exp_str {| let a = 1 and b = "punk" in b |};
  [%expect {| string |}]


let%expect_test "FACTORIAL" =
  let env = ["=", Type_arrow (Type_var {contents = Unbound "a"}, Type_arrow(Type_var {contents = Unbound "a"}, type_bool));
             "*", Type_arrow (type_int, Type_arrow(type_int, type_int));
             "-", Type_arrow (type_int, Type_arrow(type_int, type_int))] in
  infer_exp_str {| let rec fac n = if n = 1 then 1 else n * fac (n-1) in fac 4 |} ~env;
  [%expect {| int |}]


let%expect_test "FIBONACCI" =
  let env = ["<=", Type_arrow (Type_var {contents = Unbound "a"}, Type_arrow(Type_var {contents = Unbound "a"}, type_bool));
             "-", Type_arrow (type_int, Type_arrow(type_int, type_int));
             "+", Type_arrow (type_int, Type_arrow(type_int, type_int))] in
  infer_exp_str {| let rec fib n = if n <= 1 then 1 else (fib (n-1)) + (fib (n-2)) in fib 4 |} ~env;
  [%expect {| int |}]


let%expect_test "mutual recursion" =
  let env = ["=", Type_arrow (Type_var {contents = Unbound "a"}, Type_arrow(Type_var {contents = Unbound "a"}, type_bool));
             "-", Type_arrow (type_int, Type_arrow(type_int, type_int));
             "not", Type_arrow (type_bool, type_bool); "true", type_bool] in
  infer_exp_str ~env {|
  let rec is_odd n =
    if n = 0 then true else is_even (n-1)
  and is_even n = not (is_odd n)
    in is_odd 5
  |};
  [%expect {| bool |}]


(************************** Structure items **************************)

(************************** Programs **************************)
(* 
  план такой: дописать тесты на все, что реализовано выше, исправить по необходимости.
  затем доделать инфер c failwith и без левелов.
  затем добавить левелы. проверить, что тесты не провалились. добавить тесты на то, что левелы работают (со странички и из презы)
  затем заменить старый инфер на новый.
  затем попытаться убрать failwith, заменяя монадами. *)