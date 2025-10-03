(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Base
open Stdio
open Common.Parser
open Backend.Codegen

let read_all_from_channel ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       let line = In_channel.input_line ic in
       match line with
       | None -> raise End_of_file
       | Some l -> Buffer.add_string buf l; Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  Buffer.contents buf
;;

let usage () =
  eprintf "Usage: XML [input_file] [output_file]\n";
  exit 2
;;

let () =
  let args = Sys.get_argv () |> Array.to_list |> List.tl_exn in
  let source_file, output_file =
    match args with
    | [] -> (None, "out.ll")
    | [ file ] -> (Some file, "out.ll")
    | [ file; out ] -> (Some file, out)
    | _ -> usage ()
  in
  let source =
    match source_file with
    | None -> read_all_from_channel In_channel.stdin
    | Some file -> In_channel.with_file file ~f:read_all_from_channel
  in
  let ast = parse_str source in
  let asm =
    let buf = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer buf in
    gen_program ppf ast;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  Out_channel.with_file output_file ~f:(fun oc ->
      Out_channel.output_string oc asm)
;;
