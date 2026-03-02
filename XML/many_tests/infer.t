  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/001fac.ml -typedtree
  val fac : int -> int
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/002fac.ml -typedtree
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/003fib.ml -typedtree
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/004manyargs.ml -typedtree
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/005fix.ml -typedtree
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/006partial.ml -typedtree
  val foo : int -> int
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/006partial2.ml -typedtree
  val foo : int
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/006partial3.ml -typedtree
  val foo : int -> int -> int -> unit
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/007order.ml -typedtree
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/008ascription.ml -typedtree
  Fatal error: exception Failure(" exp constraint is not implemented yet")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 455, characters 14-35
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 454, characters 20-63
  Called from Middleend__InferLayers.infer_exp in file "lib/middleend/inferLayers.ml", line 454, characters 20-63
  Called from Middleend__InferLayers.infer_vb in file "lib/middleend/inferLayers.ml", line 399, characters 20-38
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_structure_item in file "lib/middleend/inferLayers.ml", line 573, characters 18-84
  Called from Middleend__InferLayers.infer_program.(fun) in file "lib/middleend/inferLayers.ml", line 591, characters 34-67
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Middleend__InferLayers.infer_program in file "lib/middleend/inferLayers.ml", line 589, characters 4-189
  Called from Dune__exe__XML_llvm.compile_and_write in file "bin/XML_llvm.ml", line 51, characters 7-40
  [2]

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/009let_poly.ml -typedtree
  val temp : int * bool

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/010fac_anf.ml -typedtree
  val fac : int -> int
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/010faccps_ll.ml -typedtree
  val id : 'a -> 'a
  val fresh_1 : int -> (int -> 'a) -> int -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/010fibcps_ll.ml -typedtree
  val id : 'a -> 'a
  val fresh_2 : int -> (int -> 'a) -> int -> 'a
  val fresh_1 : int -> (int -> 'a) -> (int -> (int -> 'a) -> 'b) -> int -> 'b
  val fib : int -> (int -> 'a) -> 'a
  val main : int

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/011mapcps.ml -typedtree
  val map : ('a -> 'b) -> 'a list -> ('b list -> 'c) -> 'c
  val iter : ('a -> 'b) -> 'a list -> unit
  val main : unit

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/012fibcps.ml -typedtree
  val fib : int -> (int -> 'a) -> 'a
  val main : unit

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/013foldfoldr.ml -typedtree
  val id : 'a -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val main : unit

map is wrong because gen is unsound?
  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/015tuples.ml -typedtree
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('a -> 'b) -> 'c * 'd -> 'b * 'b
  val fixpoly : 'a -> 'b -> 'c * 'b -> 'c
  val feven : 'a * 'b -> int -> int
  val fodd : 'a * 'b -> int -> int
  val tie : 'a -> 'b * 'a -> 'b
  val meven : int -> int
  val modd : int -> int
  val main : int


  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/016lists.ml -typedtree
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val main : int
