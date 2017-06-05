(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

open Format

exception Error

(* Optionally preprocess a source file *)

let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      let tmpfile = Filename.temp_file "camlpp" "" in
      let comm = Printf.sprintf "%s %s > %s"
                                pp (Filename.quote sourcefile) tmpfile
      in
      if Ccomp.command comm <> 0 then begin
        Misc.remove_file tmpfile;
        raise Error;
      end;
      tmpfile

let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> Misc.remove_file inputfile

let remove_preprocessed_if_ast inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ ->
      if inputfile <> !Location.input_name then Misc.remove_file inputfile

(* Parse a file or get a dumped syntax tree in it *)

exception Outdated_version = Ast_file.Outdated_version

let file ppf inputfile read_fun parse_fun =
  let ic = open_in_bin inputfile in
  let ast =
    try
      read_fun ic
    with
      Outdated_version ->
        Misc.fatal_error "Ocaml and preprocessor have incompatible versions"
  in
  let ast =
    try
      match ast with
          Some (input_name, ast) ->
            if !Clflags.fast then
              fprintf ppf "@[Warning: %s@]@."
                "option -unsafe used with a preprocessor returning a syntax tree";
            Location.input_name := input_name;
            ast
        | None ->
          seek_in ic 0;
          Location.input_name := inputfile;
          let lexbuf = Lexing.from_channel ic in
          Location.init lexbuf inputfile;
          parse_fun lexbuf
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast
