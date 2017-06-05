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

open OcpLang
open OcpSystem

type key =
  | CTRL
  | TAB
  | Char of char

type 'a command = {
  name : string;
  doc  : string;
  keys : key list list;
  code : 'a;
}

let name cmd = cmd.name
let doc cmd = cmd.doc
let code cmd = cmd.code

type lang = {
  lang             : string;
  mutable commands : unit command list;
  mutable keymap   : (string * key list list) list;
}

let lang_tbl : (string * lang) list ref = ref []

let add_command lang cmd =
  if List.mem_assoc lang !lang_tbl then
    let l = List.assoc lang !lang_tbl in
    if cmd.keys <> [] then
      l.keymap <- (cmd.name, cmd.keys) :: l.keymap;
    l.commands <- Obj.magic cmd :: l.commands;
  else
    let l = {
      lang = lang;
      commands = [Obj.magic cmd];
      keymap   = if cmd.keys = [] then [] else [cmd.name, cmd.keys]
    }in
    lang_tbl := (lang, l) :: !lang_tbl

let create_command ~lang ~name ~doc ?(keys=[]) code =
  let cmd = { name = name; doc = doc; keys = keys; code = code } in
  add_command lang cmd;
  cmd

let save ~lang ~code_of_command ~code_of_keymap ~filename =
  let l = List.assoc lang !lang_tbl in
  let code_of_command cmd = code_of_command (Obj.magic cmd) in
  let commands = List.rev l.commands in
  let lines =
    List.map code_of_command commands
    @ [code_of_keymap l.keymap] in
  let lines = List.intercalate [ ""; ""] lines in
  File.file_of_lines filename lines

module type Lang = sig

  val lang : string

  (* language native value *)
  type 'a t

  (* combinators *)
  val bind : 'a t -> ('a t -> 'b t) -> 'b t
  val seq  : unit t list -> unit t
  val run  : 'a t command -> 'a t

  (* Execute a shell command *)
  val exec : string t list -> unit t

  (* callbacks *)
  val exec_with_callbacks : int -> ('a, unit, string, string) format4 -> 'a
  val process_ocp_wizard_with_callbacks : ('a, unit, string, string) format4 -> 'a

  (* filenames *)
  type filename = string t
  val current_filename : filename
  val temp_file        : filename
  val delete_file      : filename -> unit t

  (* buffers *)
  type buffer
  val current_buffer   : buffer
  val create_buffer    : string -> buffer
  val mem_buffer       : string -> bool t
  val find_buffer      : string -> buffer
  val exec_in_buffer   : buffer -> string t list -> unit t
  val save_buffer      : buffer -> filename -> unit t
  val string_of_buffer : buffer -> string t
  val save_buffers     : unit t
  val eval_buffer      : buffer -> unit t

  (* ints *)
  val int           : int -> int t
  val exec_int      : string t list -> int t
  val string_of_int : int t -> string t
  val int_of_string : string t -> int t

  (* strings *)
  val string       : string -> string t
  val strings      : string list -> string list t
  val timestamp    : string t
  val insert       : string t -> unit t
  val display      : string t -> unit t
  val exec_string  : string t list -> string t
  val exec_strings : string t list -> string list t

  (* pairs *)
  val fst : ('a * 'b) t -> 'a t
  val snd : ('a * 'b) t -> 'b t

  (* identifiers *)
  type ident = string t (* qualified identifier, ie. Foo.Bar.t *)
  val current_ident        : ident
  val delete_current_ident : unit t

  (* positions *)
  type position = int t
  val current_position : position
  val current_line     : position
  val goto_position    : position -> unit t

  (* locations *)
  type location = (string * int) t
  val goto_location : location -> unit t
  val exec_location : string t list -> location

  (* choices *)
  val read_string :
    ?prompt:string -> ?initial:string t -> ?exact:bool -> string list t -> string t

  (* code generation *)
  val code_of_command : 'a t command -> string
  val code_of_keymap  : ?add_hook:bool -> (string * key list list) list -> string

end
