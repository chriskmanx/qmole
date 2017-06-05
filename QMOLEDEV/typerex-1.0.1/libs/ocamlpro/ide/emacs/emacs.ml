(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                 Thomas Gazagnaire, Fabrice Le Fessant                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open IDE
open OcpLang

let lang = "emacs"
let sp = Printf.sprintf

(* pairs *)
let string_int (s,i) =
  sp "(list %s %d)" s i

let fst x =
  sp "(car %s)" x

let snd x =
  sp "(cadr %s)" x

(* elisp values are stored internally as strings *)
type 'a t = string

let sp = Printf.sprintf

let gen str =
  let count = ref 0 in
  fun () ->
    incr count;
    sp "*%s%d*" str !count

let new_var = gen "var"
let new_buffer = gen "buffer"

(* combinators *)
let unit = "<unit>"

let bind x fn =
  let v = new_var () in
  sp "(let ((%s %s))\n  %s)" v x (fn v)

let (>>=) = bind

let run c =
  sp "(%s)" (name c)

let seq l =
  sp "(progn\n  %s)" (String.concat "\n    " l)

(* dynamic type checks *)
let assert_type pred typ s =
  sp "(if (not %s)\n    (error (concat (prin1-to-string %s) \" should be convertible to %s\")))"
    (pred s) s typ

let assert_int    = assert_type (sp "(integerp %s)") "int"
let assert_nil    = assert_type (sp "(null %s)") "nil"
let assert_list   = assert_type (sp "(listp %s)") "list"
let assert_string = assert_type (sp "(stringp %s)") "string"

let assert_pair x  =
  seq [
    assert_list x;
    assert_type (sp "(>= (length %s) 2)") "pair" x;
  ]

let raw_exec buffer sl =
  sp "(exec-in-buffer %s (list %s))" buffer (String.concat " " sl)

let exec sl =
  assert_nil (raw_exec "nil" sl)


let exec_with_callbacks port =
  Printf.ksprintf
    (function command ->
      sp "(command-with-callbacks %d %S)" port command)

let process_ocp_wizard_with_callbacks fmt =
  Printf.ksprintf
    (function command ->
      sp "(checked-string-command %S)" command)
    fmt

(* filenames *)

type filename = string

let current_filename =
  "(buffer-name)"

let delete_file filename =
  sp "(delete-file %s)" filename

let temp_file =
  "(make-temp-file \"ocptemp\")"


(* buffers *)

type buffer = string

let current_buffer =
  "(current-buffer)"

let create_buffer buffer =
  sp "(create-buffer \"%s\")" buffer

let exec_in_buffer = raw_exec

let string_of_buffer buffer =
  sp "(string-of-buffer %s)" buffer

let mem_buffer buffer =
  sp "(get-buffer \"%s\")" buffer

let find_buffer buffer =
  sp "(set-buffer (get-buffer-create \"%s\"))" buffer

let save_buffers =
  "(do-auto-save)"

let eval_buffer buffer =
  sp "(with-current-buffer %s (eval-buffer))" buffer

let save_buffer buffer filename =
  sp "(with-current-buffer %s (write-region nil nil %s nil 'x nil))" buffer filename

(* ints *)

let int i =
  sp "%d" i

let exec_int l =
  create_buffer (new_buffer ()) >>= (fun buffer ->
    seq [
      raw_exec buffer l;
      sp "(int-of-string (string-of-buffer %s))" buffer >>= (fun int ->
        seq [
          assert_int int;
          int;
        ])])

(* strings *)

let string x =
  sp "%S" x

let strings sl =
  sp "(list %s)" (String.concat " " (List.map string sl))

let timestamp =
  "(format-time-string \"%D\" (current-time))"

let insert_string s =
  sp "(insert %s)" s

let display_string s =
  sp "(message %s)" s

let insert = insert_string
let display = display_string

let exec_string sl =
  create_buffer (new_buffer ()) >>= (fun buffer ->
    seq [
      raw_exec buffer sl;
      sp "(string-of-buffer %s)" buffer >>= (fun string ->
        seq [
          assert_string string;
          string;
        ])])

let exec_strings sl =
  create_buffer (new_buffer ()) >>= (fun buffer ->
    seq [
      raw_exec buffer sl;
      sp "(strings-of-buffer %s)" buffer
    ])
      
(* identifiers *)

type ident = string

let current_ident =
  "(current-ident)"

let delete_current_ident =
  "(delete-current-ident)"


(* positions *)
type position = string

let current_position =
  "(point)"

let current_line =
  "(current-line)"

let goto_position p =
  sp "(goto-position %s)" p

let string_of_int s =
  sp "(int-to-string %s)" s

let int_of_string s =
  sp "(int-of-string %s)" s


(* locations *)
type location = string

let goto_location p =
  seq [
    sp "(find-file (car %s))" p;
    sp "(goto-position (cadr %s))" p;
  ]

let position_of_location p =
  sp "(cadr %s)" p

let filename_of_location p =
  sp "(car %s)" p

let exec_location sl =
  exec_string sl >>= (fun loc ->
    seq [
      assert_pair loc;
      assert_string (fst loc);
      assert_int (snd loc);
      sp "(list %s (int-of-string %s))" (fst loc) (snd loc);
    ])


(* choices *)
let read_string ?(prompt="choice") ?initial ?exact choices =
  let initial = match initial with
    | None   -> ""
    | Some i -> sp " %s" i in
  let exact = match exact with
    | None      -> "nil"
    | Some true -> "t"
    | Some false -> "0" in
  sp "(completing-read \"%s: \" %s nil %s%s)" prompt choices exact initial

(* code generation *)

(* XXX broken *)
let code_of_keymap_aux ~name ?(add_hook=false) (*?(interactive=true)*) k =

  let key1 = function
    | CTRL   -> "control" 
    | TAB    -> "tab"
    | Char c -> sp "%c" c in
  let keys1 k = sp "(%s)" (String.concat " " (List.map key1 k)) in
  let keys_seq1 k = sp "[%s]" (String.concat " " (List.map keys1 k)) in

  let key2 = function
    | CTRL   -> "\\C" 
    | TAB    -> "tab"
    | Char c -> sp "%c" c in
  let keys2 k = String.concat "-" (List.map key2 k) in
  let keys_seq2 k = sp "\"%s\"" (String.concat "" (List.map keys2 k)) in

  let contains c = List.exists (List.exists ((=)c)) in

  let aux (name, keys) =
    let keys_seq = if contains TAB keys then keys_seq2 keys else keys_seq1 keys in
    sp "  (define-key (current-local-map) %s '%s)" keys_seq name
  in
  sp "(defun %s ()%s\n%s)%s"
    name
    (if false (*interactive*) then "\n (interactive)" else "")
    (String.concat "\n" (List.map aux k))
    (if add_hook then
        sp "\n\n(add-hook 'typerex-mode-hook '%s)" name
     else
        "")

let code_of_keymap = code_of_keymap_aux ~name:"typerex"

let code_of_command c =
  sp "(defun %s ()\n  %S\n  (interactive)\n  %s)" (name c) (doc c) (code c)

