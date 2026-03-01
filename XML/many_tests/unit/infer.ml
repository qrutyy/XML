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

let infer_prog_str ?(env = env_with_things) str =
  let prog = parse_str str in
  reset_gensym ();
  let new_env = infer_program env prog in
  List.iter (fun (id, typ) ->
    Format.printf "%s : " id;
    pprint_typ Format.std_formatter typ;
    Format.printf "\n%!")
    new_env;
  ()

let show_etyp env exp =
  let _, ty = infer_exp env exp in
  Base.print_endline (show_typ ty)

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
   Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 70, characters 2-23
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
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 402, characters 4-43
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
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 406, characters 7-46
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 130, characters 2-41
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
    Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 156, characters 4-15
    Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 384, characters 4-47
    Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 171, characters 2-30
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
  Called from Middleend__InferLayers.occurs_check in file "lib/middleend/inferLayers.ml", line 139, characters 4-22
  Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 153, characters 4-22
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 384, characters 4-47
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 202, characters 2-42
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "apply 'a to 'b" =
  let env = ["f", Type_var {contents = Unbound "s"}; "x", Type_var {contents = Unbound "t"}] in
  infer_exp_str {| f x |} ~env ~rst: false;
  [%expect{| 'e |}]


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
  infer_pat_str {| None |} ~env;
  [%expect {| 'a option |}]


 let%expect_test "construct some" =
  infer_pat_str {| Some 1 |} ~env;
 [%expect.unreachable]
 [@@expect.uncaught_exn {|
   (* CR expect_test_collector: This test expectation appears to contain a backtrace.
      This is strongly discouraged as backtraces are fragile.
      Please change this test to not include a backtrace. *)

   (Failure "cannot unify with a quantified type")
   Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
   Called from Middleend__InferLayers.infer_pat in file "lib/middleend/inferLayers.ml", line 270, characters 7-23
   Called from XML_unittests__Infer.infer_pat_str in file "many_tests/unit/infer.ml", line 21, characters 14-31
   Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 284, characters 2-33
   Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


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
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 358, characters 14-35
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 313, characters 2-32
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
    Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 156, characters 4-15
    Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 384, characters 4-47
    Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 345, characters 2-68
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


(************************** Match, function **************************)

let%expect_test "correct match" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound  "a"}])] @ env in
  infer_exp_str {| match a with | Some x -> 1 | None -> 2 |} ~env;
  [%expect {|
    int |}]


let%expect_test "use match pattern in body" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound  "a"}])] @ env in
  infer_exp_str {| match a with | Some x -> x | None -> 2 |} ~env;
  [%expect {|
    int |}]


let%expect_test "match different constructors" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound  "a"}])] @ env in
  infer_exp_str {| match a with | Some x -> 1 | [] -> 2 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: list and option")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp.(fun) in file "lib/middleend/inferLayers.ml", line 429, characters 11-33
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 425, characters 6-685
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 379, characters 2-63
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "match option with list constructors" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound  "a"}])] @ env in
  infer_exp_str {| match a with | x :: tl -> 1 | [] -> 2 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: list and option")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp.(fun) in file "lib/middleend/inferLayers.ml", line 429, characters 11-33
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 425, characters 6-685
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 398, characters 2-64
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "match different types of expr 1" =
  let env = [ "a", Type_construct("option", [Type_var {contents = Unbound  "a"}])] @ env in
  infer_exp_str {| match a with | Some x -> 'a' | None -> 1234 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: char and int")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp.(fun) in file "lib/middleend/inferLayers.ml", line 440, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 425, characters 6-685
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 417, characters 2-70
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "match different types of expr 2" =
  let env = [ "b", Type_construct("list", [Type_var {contents = Unbound  "a"}])] @ env in
  infer_exp_str {| match b with | x :: y :: tl -> 'a' | x :: tl -> 'b' | _ -> 1234 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: char and int")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp.(fun) in file "lib/middleend/inferLayers.ml", line 440, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 425, characters 6-685
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 436, characters 2-90
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "correct function" =
  infer_exp_str {| function | Some x -> 1 | None -> 2 |} ~env;
  [%expect {|
    ('a option -> int) |}]


let%expect_test "use function pattern in body" =
  infer_exp_str {| function | Some x -> x | None -> 2 |} ~env;
  [%expect {|
    ('a option -> int) |}]


let%expect_test "function different constructors" =
  infer_exp_str {| function | Some x -> 1 | [] -> 2 |} ~env;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "can't unify different constructors: list and option")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp.(fun) in file "lib/middleend/inferLayers.ml", line 453, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 450, characters 6-314
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 466, characters 2-59
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
  Called from Middleend__InferLayers.infer_exp.(fun) in file "lib/middleend/inferLayers.ml", line 455, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 450, characters 6-314
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 484, characters 2-66
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
  Called from Middleend__InferLayers.infer_exp.(fun) in file "lib/middleend/inferLayers.ml", line 455, characters 11-32
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 450, characters 6-314
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 502, characters 2-86
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
  show_etyp [] (parse_exp_str {| let a = fun x -> x in a |});
  [%expect {| (Type_arrow ((Type_var ref ((Unbound "d"))), (Type_var ref ((Unbound "d"))))) |}]


let%expect_test "let poly 2" =
  infer_exp_str {| let a = fun x -> x in (a 1, a "str") |};
  [%expect{| (int * string) |}]


let%expect_test "poly in env" =
  let env = ["=", Type_arrow (Quant_type_var "a", Type_arrow(Quant_type_var "a", type_bool))] in
  infer_exp_str {| let a = 1 in 1 = 1, "str" = "str" |} ~env;
  [%expect{| (bool * bool) |}]


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
  Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 160, characters 9-62
  Called from Middleend__InferLayers.infer_vb in file "lib/middleend/inferLayers.ml", line 309, characters 2-19
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 413, characters 18-84
  Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
  Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 563, characters 2-46
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
    Called from Middleend__InferLayers.unify in file "lib/middleend/inferLayers.ml", line 160, characters 9-62
    Called from Middleend__InferLayers.infer_vb in file "lib/middleend/inferLayers.ml", line 309, characters 2-19
    Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
    Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 413, characters 18-84
    Called from XML_unittests__Infer.infer_exp_str in file "many_tests/unit/infer.ml", line 15, characters 14-31
    Called from XML_unittests__Infer.(fun) in file "many_tests/unit/infer.ml", line 582, characters 2-45
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}]


let%expect_test "let and" =
  infer_exp_str {| let a = 1 and b = "punk" in b |};
  [%expect {| string |}]


let%expect_test "factorial" =
  let env = ["=", Type_arrow (Quant_type_var "a", Type_arrow(Quant_type_var "a", type_bool));
             "*", Type_arrow (type_int, Type_arrow(type_int, type_int));
             "-", Type_arrow (type_int, Type_arrow(type_int, type_int))] in
  infer_exp_str {| let rec fac n = if n = 1 then 1 else n * fac (n-1) in fac 4 |} ~env;
  [%expect {| int |}]


let%expect_test "fibonacci" =
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


(* сделать pprint для текущего инфера
   заменить старый инфер на новый
   проверить, что тесты не упали, если упали, то починить
   затем добавить уровни
   проверить что тесты не упали, если упали, то починить
   добавить монады вместо failwith
   *)