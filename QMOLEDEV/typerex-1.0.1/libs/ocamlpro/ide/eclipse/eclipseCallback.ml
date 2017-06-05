(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                           Tiphaine Turpin                              *)
(*                                                                        *)
(*  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open OcpLang
open Unix
open Face
include Debug.Tag(struct let tag = "eclipseCallback" end)

let callback_read_error = "CALLBACK_READ_ERROR"
let error_in_callback = "ERROR_IN_CALLBACK"
let quit = "Quit"

exception CallbackReadError of string * string
exception ErrorInCallback of string

module Make(Arg : sig
  val connection : Ocp_rpc.tagged_connection
end) = struct

  let command_k k fmt =
    Printf.ksprintf
      (function comand ->
        match
          Arg.connection#send_request
            ~write_command:(function oc -> output_string oc comand)
        with
          | error_mark :: err when error_mark = callback_read_error ->
              raise (CallbackReadError (String.concat "\n" err, comand))
          | error_mark :: err when error_mark = error_in_callback ->
	      if err = [quit] then
		raise IDE_Callback.Quit
	      else
		let e = String.concat " " err in
		raise (ErrorInCallback e)
          | res -> k (String.concat "\n" res))
      fmt

  let parse_string s =
    let len = String.length s in
    if len >= 2 && s.[0] = '"' && s.[len-1] = '"' then
      String.sub s 1 (len - 2)
    else
      invalid_arg "parse_string"

  let quote s = String.replace_chars (String.escaped s) [' ', "\\32"]

  let command_int fmt = command_k int_of_string fmt
  let command_bool fmt =
    command_k
      (function s ->
        match parse_string s with
          | "true" -> true
          | "false" -> false
          | b -> Printf.ksprintf invalid_arg "bool: %s" b)
      fmt
  let command_string fmt = command_k parse_string fmt
  let command_unit fmt = command_k ignore fmt

  let pos ?(unit=`byte) n =
    match unit with
      | `char -> assert false
      | `byte -> Printf.sprintf "%d" n

  let goto_char ?unit n = command_unit "goto-char %s" (pos ?unit n)
  let goto_pos ?unit = function
    | `lc (l, c) -> command_unit "goto-lc %d %d" l c
    | `cnum cnum -> goto_char ?unit cnum

  let point ?(unit=`byte) () =
    match unit with
      | `char -> assert false
      | `byte -> command_int "point-bytes"
  let line_column_bytes () =
    let lc = command_string "line-column-bytes" in
    match String.split lc ' ' with
      | [l ; c] -> int_of_string l, int_of_string c
      | _ -> invalid_arg "line_column_bytes"
(*
  let line_number : unit -> int
  let column_number : unit -> int
*)
  let do_auto_save () = command_unit "do-auto-save"
  let message fmt =
    Printf.ksprintf (function s -> command_unit "message %s" (quote s)) fmt
  let display_temp buffer_name = message
  let read_from_minibuffer fmt =
    Printf.ksprintf
      (function s ->
        command_string "prompt %s" (quote s)) fmt
  let y_or_n_p fmt =
    Printf.ksprintf
      (function s ->
        command_bool "y-or-n-p %s" (quote s)) fmt
  let buffer_file_name () = command_string "buffer-file-name"
(*
  let buffer_name : unit -> string
  let cd : ('a, unit, string, unit) format4 -> 'a
  let insert : ('a, unit, string, unit) format4 -> 'a
  let set_cleared_buffer : ('a, unit, string, unit) format4 -> 'a
*)
  let find_file fname = command_unit "find-file %s" fname

  let goto ?unit file pos = find_file file ; goto_pos ?unit pos
  let revert_with_history () = ()
  let revert_buffer_visiting _ = ()
  let save_buffer_visiting _ = ()
  let rename_file _ _ = assert false
  let highlight ?unit face b e = () (* Stub *)
  let highlight_regions ?unit ?forever:bool rs = ()
(*
  let highlight_regions :
    ?unit:[ `byte | `char ] -> ?forever:bool -> (Face.face * int * int) list -> unit
  let propertize_regions :
    ?unit:[ `byte | `char ] -> (int * int * property list) list -> unit
  let propertize_region_lists :
    ?unit:[ `byte | `char ] -> (property list * (int * int) list) list -> unit
  let propertize_region_lists_command :
    ?unit:[ `byte | `char ] -> (property list * (int * int) list) list -> string
*)

  (* unused *)
  let eclipse_face = function
    | `typerex `governing -> "governing"
    | `font_lock `keyword -> "keyword"
    | `font_lock `ftype -> "type"
    | `font_lock `variable_name -> "variableName"
    | `desc [`weight `ultra_bold]
    | _ -> "normal"

  let show_completions _ _ = assert false

end
