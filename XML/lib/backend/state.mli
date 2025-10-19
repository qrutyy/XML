(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type codegen_state = { frame_offset : int }

module State : sig
  type 'a t = codegen_state -> 'a * codegen_state

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val get : codegen_state t
  val put : codegen_state -> unit t
  val move_frame_offset : (int -> int) -> unit t
  val fresh_label : string -> string
  val reset_labels : unit -> unit
end
