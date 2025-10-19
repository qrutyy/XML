(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type codegen_state = { frame_offset : int }

module State = struct
  type 'a t = codegen_state -> 'a * codegen_state

  let return x st = x, st

  let bind m f =
    fun st ->
    let x, st' = m st in
    f x st'
  ;;

  let ( let* ) = bind
  let get st = st, st
  let put st = fun _ -> (), st

  let move_frame_offset f =
    let* st = get in
    put { st with frame_offset = f st.frame_offset }
  ;;

  let label_counter = ref 0

  let fresh_label prefix =
    let n = !label_counter in
    incr label_counter;
    prefix ^ string_of_int n
  ;;

  let reset_labels () = label_counter := 0
end
