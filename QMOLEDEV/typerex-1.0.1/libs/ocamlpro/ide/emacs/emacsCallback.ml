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
include Debug.Tag(struct let tag = "emacsCallback" end)

let callback_read_error = "CALLBACK_READ_ERROR"
let error_in_callback = "ERROR_IN_CALLBACK"
let quit = "Quit"

exception CallbackReadError of string
exception ErrorInCallback of string

let property_list props =
  Printf.sprintf "(%s)"
    (String.concat " "
       (List.map
          (function
            | `face face -> Printf.sprintf "face ,%s" (face_emacs_name face)
            | `help_echo (Some s) -> Printf.sprintf "help-echo ,%S" s
            | `help_echo None -> Printf.sprintf "help-echo ,nil"
            | `fontified -> "fontified t"
            | `font_lock_multiline -> "jit-lock-defer-multiline t"
                (* "font-lock-multiline t" *))
          props))

  let region_list rs =
    String.concat " "
      (List.map
         (function x, y ->
           Printf.sprintf "(%d %d)" (x+1) (y+1))
         rs)

(* More efficient *)
  let region_list rs =
    let buf = Buffer.create 1024 in
    List.iter
      (function x, y ->
        Buffer.add_char buf  '(';
        Buffer.add_string buf (string_of_int (x+1));
        Buffer.add_char buf  ' ';
        Buffer.add_string buf (string_of_int (y+1));
        Buffer.add_char buf  ')')
      rs;
    Buffer.contents buf

  let propertize_region_lists_command ?(unit=`byte) rs =
    Printf.sprintf "(propertize-region-lists-%s `(%s))"
      (match unit with
        | `byte -> "byte"
        | `char -> "char")
      (String.concat " "
         (List.map
            (function properties, regions ->
              Printf.sprintf "(%s (%s))"
                (property_list properties)
                (Profile.time_call "region_list" region_list regions))
            rs))

  let pos ?(unit=`byte) n =
    let pos = Printf.sprintf "(check-position %d)" (n+1) in
    match unit with
      | `char -> Printf.sprintf "%s" pos
      | `byte -> Printf.sprintf "(byte-to-position %s)" pos

  let pos' ?(unit=`byte) = function
    | `lc (l, c) -> Printf.sprintf "(line-column-to-pos %d %d)" l c
    | `cnum cnum -> pos ~unit cnum

  let regions ?unit rs =
    String.concat " "
      (List.map
         (function face, x, y ->
           Printf.sprintf "(,%s ,%s ,%s)"
             (face_emacs_name face) (pos' ?unit x) (pos' ?unit y)) rs)

module Make(Arg : sig
  val connection : Ocp_rpc.tagged_connection
end) = struct

  let command_k k fmt =
    Printf.ksprintf
      (function command ->
        match
          Arg.connection#send_request
            ~write_command:(function oc -> output_string oc command)
        with
          | [res] when res = callback_read_error ->
              raise (CallbackReadError command)
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

  (* {4 Generic stubs} *)

  (* The following generic commands are used as [Printf.sprintf] *)

  let command_int fmt = command_k int_of_string fmt
  let command_bool fmt =
    command_k
      (function "t" -> true | "nil" -> false | _ -> invalid_arg "bool")
      fmt
  let command_string fmt = command_k parse_string fmt
  let command_unit fmt = command_k ignore fmt

  let goto_char ?unit n = command_unit
    "(progn (push (point) buffer-undo-list) (goto-char %s))" (pos ?unit n)

  let goto_pos ?unit n = command_unit
    "(progn (push (point) buffer-undo-list) (goto-char %s))" (pos' ?unit n)

  let point ?(unit=`byte) () =
    match unit with
      | `char -> command_int "(point)" - 1
      | `byte -> command_int "(position-bytes (point))" - 1
  let line_column_bytes () =
    let lc = command_string "(line-column-bytes)" in
    match String.split lc ' ' with
      | [l ; c] -> int_of_string l, int_of_string c
      | _ -> invalid_arg "line_column_bytes"
  let line_number () = command_int "(line-number-at-pos)"
  let column_number () = command_int "(current-column)"
  let do_auto_save () = command_unit "(do-auto-save)"
  let message fmt =
    Printf.ksprintf (command_unit "(message %S)") fmt
  let display_temp buffer_name fmt =
    Printf.ksprintf (command_unit "(display-temp %S %S)" buffer_name) fmt
  let read_from_minibuffer fmt =
    Printf.ksprintf (command_string "(read-from-minibuffer %S)") fmt
  let y_or_n_p fmt =
    Printf.ksprintf (command_bool "(y-or-n-check-height %S)") fmt
  let buffer_name () = command_string "(buffer-name)"
  let buffer_file_name () = command_string "(buffer-file-name)"
  let cd fmt = Printf.ksprintf (command_unit "(cd %S)") fmt
  let insert fmt = Printf.ksprintf (command_unit "(insert %S)") fmt
  let revert_with_history () = command_unit "(revert-with-history)"
  let revert_buffer_visiting = command_unit "(revert-buffer-visiting %S)"
  let save_buffer_visiting = command_unit "(save-buffer-visiting %S)"
  let rename_file = command_unit "(renamed-file %S %S)"
  let find_file = command_unit "(find-file %S)"
  let goto ?unit file loc = find_file file ; goto_pos ?unit loc

  let set_cleared_buffer fmt =
    Printf.ksprintf (command_unit "(set-cleared-buffer %S)") fmt

  let highlight ?unit face x y =
    command_unit "(highlight %s %s %s)" (face_emacs_name face) (pos ?unit x) (pos ?unit y)

  let highlight_regions ?unit ?(forever = false) rs =
    command_unit "(highlight-regions %s `(%s))"
      (if forever then "t" else "nil")
      (regions ?unit rs)

  let highlight ?unit face x y =
      highlight_regions ?unit [face, x, y]

  let propertize_regions ?unit rs =
    command_unit "(propertize-regions `(%s))"
      (String.concat " "
         (List.map
            (function x, y, properties ->
              Printf.sprintf "(,%s ,%s %s)"
                (pos ?unit x) (pos ?unit y) (property_list properties))
            rs))

  let propertize_region_lists ?(unit=`byte) rs =
    command_unit "(propertize-region-lists-%s `(%s))"
      (match unit with
        | `byte -> "byte"
        | `char -> "char")
      (String.concat " "
         (List.map
            (function properties, regions ->
              Printf.sprintf "(%s (%s))"
                (property_list properties) (region_list regions))
            rs))

  let remove_properties ?unit b e props =
    command_unit "(remove-list-of-text-properties %s %s `(%s))"
      (pos ?unit b) (pos ?unit e)
      (String.concat " "
         (List.map
            (function
              | `face -> "face")
            props))

  let show_completions word_beginning completions =
    command_unit
      "(ocp-show-completions %s '(%s))"
      (pos word_beginning)
      (String.concat " "
         (List.map (Printf.sprintf "%S") completions))

end
