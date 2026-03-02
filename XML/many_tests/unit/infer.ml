(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Infer
open Common.Parser
open Common.Pprinter
open Common.Ast.TypeExpr

(* TODO: get rid of failwith in infer *)

let infer_exp_str ?(rst = true) ?(env = []) str =
  let exp = parse_exp_str str in
  if rst then reset_gensym ();
  let _, ty = infer_exp env exp in
  pprint_type Format.std_formatter ty;;

let infer_pat_str ?(rst = true) ?(env = []) str =
  let pat = parse_pat_str str in
  if rst then reset_gensym ();
  let _, ty = infer_pat env pat in
  pprint_type Format.std_formatter ty;;

let infer_prog_str ?(env = env_with_things) str =
  let prog = parse_str str in
  reset_gensym ();
  let env, names = infer_program env prog in
  pprint_env env names

let show_etyp env exp =
  let _, ty = infer_exp env exp in
  Base.print_endline (Common.Ast.TypeExpr.show ty)

let type_bool = Type_construct ("bool", [])
let type_unit = Type_construct ("unit", [])
let type_char = Type_construct ("char", [])
let type_int = Type_construct ("int", [])
let type_string = Type_construct ("string", [])

let env = env_with_things

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
  infer_exp_str {| m |} ~env:[("m", Type_var {contents = Unbound ("a", 0)})];
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
   Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 65, characters 2-23
   Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}];;


let%expect_test "tuple 2" =
  infer_exp_str {| (1, 2) |};
  [%expect{| int * int |}]


let%expect_test "tuple 3" =
  infer_exp_str {| (1, 2, 3) |};
  [%expect{| int * int * int |}]


  let%expect_test "tuple 4" =
  infer_exp_str {| (1, 2, 3, 4) |};
  [%expect{| int * int * int * int |}]


let%expect_test "tuples in tuple" =
  infer_exp_str {| ((1, 2), (3, 4)) |};
  [%expect{| (int * int) * (int * int) |}]


 let%expect_test "construct none" =
  infer_exp_str {| None |} ~env;
 [%expect {| 'a option |}]


let%expect_test "construct some" =
  infer_exp_str {| Some 1 |} ~env;
 [%expect {| int option |}]


let%expect_test "if (string) " =
  infer_exp_str {| if "trololo" then 1 |};
  [@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: string and bool")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 298, characters 4-43
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
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 302, characters 7-46
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 125, characters 2-41
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "if (bool) then (unit)" =
  let env = ["cond", type_bool; "bodyvar", type_unit] in
  infer_exp_str {| if cond then bodyvar |} ~env;
  [%expect{| unit |}]


let%expect_test "if (bool) then 'a else 'a" =
  let env = ["cond", type_bool; "x", Type_var {contents = Unbound ("a", 0)}; "y", Type_var {contents = Unbound ("a", 0)}] in
  infer_exp_str  {| if cond then x else y |}  ~env;
  [%expect{| 'a |}]


let%expect_test "if (bool) then 'a else 'b" =
  let env = ["cond", type_bool; "x", Type_var {contents = Unbound ("a", 0)}; "y", Type_var {contents = Unbound ("b", 0)}] in
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
    Called from Middleend__Infer.unify in file "lib/middleend/infer.ml", line 44, characters 4-15
    Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 280, characters 4-47
    Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 166, characters 2-30
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "apply 'a -> 'a to 'b" =
  let env = ["f", Type_arrow (Type_var {contents = Unbound ("s", 0)}, Type_var {contents = Unbound ("s", 0)}); "x", Type_var {contents = Unbound ("t", 0)}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect{| 'b |}]


  (* not sure if this is right *)
let%expect_test "apply 'a to 'a (different vars)" =
  let env = ["f", Type_var {contents = Unbound ("t", 0)}; "x", Type_var {contents = Unbound ("t", 0)}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect {| 'c |}]


let%expect_test "apply 'a to 'a (same var)" =
  let env = ["x", Type_var {contents = Unbound ("t", 0)}] in
  infer_exp_str {| x x |} ~env ~rst: false;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "occurs check")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.occurs_check in file "lib/middleend/infer.ml", line 27, characters 4-22
  Called from Middleend__Infer.unify in file "lib/middleend/infer.ml", line 41, characters 4-22
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 280, characters 4-47
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 197, characters 2-42
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "apply 'a to 'b" =
  let env = ["f", Type_var {contents = Unbound ("s", 0)}; "x", Type_var {contents = Unbound ("t", 0)}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect{| 'e |}]


(************************** Patterns **************************)

let%expect_test "id in env" =
  let env = ["m", (Type_var {contents = Unbound ("c", 0)})] in
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
  [%expect {| int * int |}];;


let%expect_test "tuple 3" =
  infer_pat_str {| (1, 2, 3) |};
  [%expect {| int * int * int |}];;


let%expect_test "tuple 4" =
  infer_pat_str {| (1, 2, 3, 4) |};
   [%expect {| int * int * int * int |}];;


let%expect_test "tuples in tuple" =
  infer_pat_str {| ((1, 2), (3, 4)) |};
   [%expect {| (int * int) * (int * int) |}];;


let%expect_test "construct none" =
  infer_pat_str {| None |} ~env;
  [%expect {| 'a option |}]


 let%expect_test "construct some" =
  infer_pat_str {| Some 1 |} ~env;
 [%expect{| int option |}]


(************************** Funs **************************)

let%expect_test "fun 'a -> 'a (new var)" =
  infer_exp_str {| fun x -> x |};
  [%expect {| 'a -> 'a |}]


let%expect_test "fun 'a -> 'a (shadow)" =
  let env = ["x", Type_var {contents = Unbound ("s", 0)}] in
  infer_exp_str {| fun x -> x |} ~env;
  [%expect {| 'a -> 'a |}]


let%expect_test "fun 'a -> 'b (not in env)" =
  infer_exp_str {| fun x -> y |};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "unbound variable: y")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 250, characters 14-35
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 297, characters 2-32
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "fun 'a -> 'b (in env)" =
  let env = ["y", Type_var {contents = Unbound ("s", 0)}] in
  infer_exp_str {| fun x -> y |} ~env;
  [%expect{| 'a -> 's |}]


let%expect_test _=
  infer_exp_str {| fun x -> fun y -> x y |};
 [%expect{| ('b -> 'c) -> 'b -> 'c |}]


let%expect_test _ =
  infer_exp_str {| fun x y -> x y |};
  [%expect{| ('b -> 'c) -> 'b -> 'c |}]


let%expect_test _ =
  infer_exp_str {| (fun f a b -> f a, f b) (fun x -> x) 1 "mystr" |};
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure "can't unify different constructors: int and string")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Middleend__Infer.unify in file "lib/middleend/infer.ml", line 44, characters 4-15
    Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 280, characters 4-47
    Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 329, characters 2-68
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


(************************** Match, function **************************)

let%expect_test "correct match" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound  ("a", 0)}])] @ env in
  infer_exp_str {| match a with | Some x -> 1 | None -> 2 |} ~env;
  [%expect {|
    int |}]


let%expect_test "use match pattern in body" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match a with | Some x -> x | None -> 2 |} ~env;
  [%expect {|
    int |}]


let%expect_test "match different constructors" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match a with | Some x -> 1 | [] -> 2 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: list and option")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_exp.(fun) in file "lib/middleend/infer.ml", line 329, characters 11-33
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 325, characters 6-685
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 363, characters 2-63
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "match option with list constructors" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match a with | x :: tl -> 1 | [] -> 2 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: list and option")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_exp.(fun) in file "lib/middleend/infer.ml", line 329, characters 11-33
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 325, characters 6-685
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 382, characters 2-64
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "match different types of expr 1" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match a with | Some x -> 'a' | None -> 1234 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: char and int")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_exp.(fun) in file "lib/middleend/infer.ml", line 340, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 325, characters 6-685
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 401, characters 2-70
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "match different types of expr 2" =
  let env = [ "b", Type_construct("list", [Type_var {contents = Unbound ("a", 0)}])] @ env in
  infer_exp_str {| match b with | x :: y :: tl -> 'a' | x :: tl -> 'b' | _ -> 1234 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: char and int")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_exp.(fun) in file "lib/middleend/infer.ml", line 340, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 325, characters 6-685
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 420, characters 2-90
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "correct function" =
  infer_exp_str {| function | Some x -> 1 | None -> 2 |} ~env;
  [%expect {|
    'd option -> int |}]


let%expect_test "use function pattern in body" =
  infer_exp_str {| function | Some x -> x | None -> 2 |} ~env;
  [%expect {|
    int option -> int |}]


let%expect_test "function different constructors" =
  infer_exp_str {| function | Some x -> 1 | [] -> 2 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: list and option")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_exp.(fun) in file "lib/middleend/infer.ml", line 353, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 350, characters 6-314
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 450, characters 2-59
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "function different types of expr 1" =
  infer_exp_str {| function | Some x -> 'a' | None -> 1234 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: char and int")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_exp.(fun) in file "lib/middleend/infer.ml", line 355, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 350, characters 6-314
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 468, characters 2-66
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "function different types of expr 2" =
  infer_exp_str {| function | x :: y :: tl -> 'a' | x :: tl -> 'b' | _ -> 1234 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: char and int")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_exp.(fun) in file "lib/middleend/infer.ml", line 355, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 350, characters 6-314
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 486, characters 2-86
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
  infer_exp_str {| let a = 1 in "str" |};
  [%expect{| string |}]


let%expect_test "let poly" =
  infer_exp_str {| let a = fun x -> x in a |};
  [%expect {| 'c -> 'c |}]


let%expect_test "let poly 2" =
  infer_exp_str {| let a = fun x -> x in (a 1, a "str") |};
  [%expect{| int * string |}]


let%expect_test "poly in env" =
  let env = ["=", Type_arrow (Quant_type_var "a", Type_arrow(Quant_type_var "a", type_bool))] in
  infer_exp_str {| let a = 1 in 1 = 1, "str" = "str" |} ~env;
  [%expect{| bool * bool |}]


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
  Called from Middleend__Infer.unify in file "lib/middleend/infer.ml", line 48, characters 9-62
  Called from Middleend__Infer.infer_vb in file "lib/middleend/infer.ml", line 197, characters 2-19
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 310, characters 18-84
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 547, characters 2-46
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
    Called from Middleend__Infer.unify in file "lib/middleend/infer.ml", line 48, characters 9-62
    Called from Middleend__Infer.infer_vb in file "lib/middleend/infer.ml", line 197, characters 2-19
    Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
    Called from Middleend__Infer.infer_exp in file "lib/middleend/infer.ml", line 310, characters 18-84
    Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 566, characters 2-45
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "let and" =
  infer_exp_str {| let a = 1 and b = "punk" in b |};
  [%expect {| string |}]


let%expect_test "factorial" =
  infer_exp_str {| let rec fac n = if n = 1 then 1 else n * fac (n-1) in fac 4 |} ~env;
  [%expect {| int |}]


let%expect_test "fibonacci" =
  infer_exp_str {| let rec fib n = if n <= 1 then 1 else (fib (n-1)) + (fib (n-2)) in fib 4 |} ~env;
  [%expect {| int |}]


let%expect_test "mutual recursion" =
  let env = env @ ["not", Type_arrow (type_bool, type_bool); "true", type_bool] in
  infer_exp_str ~env {|
  let rec is_odd n =
    if n = 0 then true else is_even (n-1)
  and is_even n = not (is_odd n)
    in is_odd 5
  |};
  [%expect {| bool |}]


let%expect_test "shadow with itself" =
  infer_prog_str 
  {|
  let test3 a b c =
    let a = print_int a in 0
  |};
[%expect{| val test3 : int -> 'a -> 'b -> int |}];;


let%expect_test "shadow" =
  infer_prog_str {|
    let x = 5
    
    let x = "string"
  |};
[%expect {|
  val x : string |}]


let%expect_test "shadow with itself 2" =
  infer_prog_str {|
    let x = 5
    
    let x = x
  |};
[%expect {|
  val x : int |}]


let%expect_test "weird let rec" =
  infer_prog_str {| let rec x = x |};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure
    "this kind of expression is not allowed as right-hand side of `let rec'")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__Infer.infer_vb_rec in file "lib/middleend/infer.ml", line 218, characters 8-89
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_structure_item in file "lib/middleend/infer.ml", line 386, characters 6-80
  Called from Middleend__Infer.infer_program.(fun) in file "lib/middleend/infer.ml", line 397, characters 34-67
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__Infer.infer_program in file "lib/middleend/infer.ml", line 395, characters 4-189
  Called from XML_unittests__Infer.infer_prog_str in file "many_tests/unit/infer.ml", line 27, characters 19-41
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 640, characters 2-36
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "too polymorphic1" =
  infer_prog_str {| let map f p = let (a,b) = p in (f a, f b) |};
[%expect {| val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b |}]

let%expect_test "too polymorphic2" =
  infer_prog_str {|
    let rec fix f x = f (fix f) x
    let map f p = let (a,b) = p in (f a, f b)
    let fixpoly l =
      fix (fun self l -> map (fun li x -> li (self l) x) l) l
  |};
[%expect {|
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
  val fixpoly : 'a -> ('b -> 'c) * ('b -> 'c) |}]

