(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Middleend.Pprinter
open Middleend.Cc
open Middleend.Ll
open Common.Parser
open Common.Pprinter

let to_cc str =
    let prog = parse_str str in
    let ccprog = cc_program prog in
    (* let aprog = anf_program ccprog in
    let llprog = lambda_lift_program aprog in *)
    pprint_program Format.std_formatter ccprog


let%expect_test "020closures.pdf" =
to_cc {|
  let fac n =
    let rec fack n k =
      if n <= 1 then k 1
      else fack (n-1) (fun m -> k (m * n))
    in
    fack n (fun x -> x)
|};
[%expect {|
  let fac = (fun n -> (let rec fack = (fun n k -> (if n <= 1
    then (k 1)
    else (fack n - 1) (((fun k n m -> (k m * n)) k) n))) in (fack n) (fun x -> x)));; |}]


let%expect_test "LN_CC_1" =
to_cc {|
  let f x =
    let g y = x
    in
    g
|};
[%expect {|
  let f = (fun x -> (let g = ((fun x y -> x) x) in (g x)));; |}]


let%expect_test "LN_CC_2" =
to_cc {|
  fun x -> x + a + b
|};
[%expect {|
  ((fun a b x -> x + a + b) a) b ;; |}]


let%expect_test "LN_CC_3" =
to_cc {|
  let mkPair = fun x ->
    let add = fun y -> x + y in
    let mul = fun z -> x * z in
    (add, mul)
|};
[%expect {|
  let mkPair = (fun x -> (let add = ((fun x y -> x + y) x) in (let mul = ((fun x z -> x * z) x) in ((add x), (mul x)))));; |}]
