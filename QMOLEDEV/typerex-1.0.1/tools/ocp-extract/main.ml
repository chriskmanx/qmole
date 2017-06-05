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

open OcpSystem
open OcpLang
open Parsetree

let cmd = Filename.basename Sys.argv.(0)

let arg_usage =
  Printf.sprintf "
Usage:

    %s [file.ml[i] [extractors]*]* : extract fields from source files
" cmd

let nice_exit () =
  Printf.eprintf "%s\n%!" arg_usage;
  exit 2

(* Global options *)
module Global = struct

  type format =
    | Loc
    | Loc_end
    | Str

  type config = {
    mutable format   : format option;
    mutable comments : bool;
    mutable continue : bool;
  }

  let config = {
    format   = None;
    comments = false;
    continue = false;
  }

  let formats = [ "loc"; "end-loc"; "str" ]

  let format str  =
    match config.format with
      | Some _ ->
        Printf.eprintf "Use only one -format option\n%!";
        nice_exit ()
      | None   ->
        match str with
          | "loc"     -> config.format <- Some Loc
          | "str"     -> config.format <- Some Str
          | "end-loc" -> config.format <- Some Loc_end
          | _         -> raise Not_found

  let format_fun = Arg.Symbol (formats, format)

  let comments () =
    if config.comments then begin
      Printf.printf "Use only one -with-comments tag\n%!";
      nice_exit ()
    end;
    config.comments <- true

  let comments_fun = Arg.Unit comments

  let continue () =
    if config.comments then begin
      Printf.printf "Use only one -k tag\n%!";
      nice_exit ()
    end;
    config.comments <- true

  let continue_fun = Arg.Unit continue

  let should_exit () =
    if not config.continue then
      exit 2

end

(* Local options *)
module Local = struct

  type ast =
    | Structure of Parsetree.structure
    | Signature of Parsetree.signature

  type config = {
    mutable filename : string option;
    mutable file     : string option;
    mutable ast      : ast option;
    mutable comments : (string * Location.t) list;
    mutable buffer   : string;
  }

  let config = {
    filename = None;
    file     = None;
    ast      = None;
    comments = [];
    buffer   = "";
  }

  let copy () =
    (* copy the current config *)
    { config with filename = config.filename }

  let process kind name str =
    config.buffer <- str;
    Location.input_name := name;
    let lexbuf = Lexing.from_string str in
    config.filename <- Some name;
    config.file <- Some str;
    (match kind with
      | `impl -> config.ast <- Some (Structure (ParseOCaml.Safe.structure name lexbuf))
      | `intf -> config.ast <- Some (Signature (ParseOCaml.Safe.signature name lexbuf))
      | `unknown ->
        let ast =
          try Structure (ParseOCaml.Safe.structure name lexbuf)
          with _ ->
            try Signature (ParseOCaml.Safe.signature name lexbuf)
            with _ -> failwith "unknown extension" in
        config.ast <- Some ast);
    config.comments <- ParseOCaml.comments ()

  let set_filename name =
    let str = File.string_of_file name in
    let kind =
      if Filename.check_suffix name ".ml" then
       `impl
      else if Filename.check_suffix name ".mli" then
        `intf
      else
        `unknown in
    process kind name str

  let set_implementation name =
    let str = File.string_of_file name in
    process `impl name str

  let set_interface name =
    let str = File.string_of_file name in
    process `intf name str

  let impl_of_string str =
    process `impl "<impl>.ml" str

  let intf_of_string str =
    process `intf "<intf>.mli" str

  let set_string str =
    try impl_of_string str
    with _ -> intf_of_string str

  let filename config =
    match config.filename with
      | Some f -> f
      | None   ->
        Printf.eprintf "no filename given";
        nice_exit ()

end

module Ops = struct

  type t =
    | Type of Local.config * string
    | Value of Local.config * string
    | Module of Local.config * string
    | Position of Local.config * int

  let queue = Queue.create ()

  let get_type_by_name name =
    Queue.push (Type (Local.copy (), name)) queue

  let get_value_by_name name =
    Queue.push (Value (Local.copy (), name)) queue

  let get_module_by_name name =
    Queue.push (Module (Local.copy (), name)) queue

  let get_by_pos p =
    Queue.push (Position (Local.copy (), p)) queue

end

module Process = struct

  open Extract
  module G = Global
  module L = Local

  let pcomments config loc =
    if G.config.G.comments then begin
      let comments = get_comments_from_buffer config.L.buffer config.L.comments loc in
      List.iter (Printf.printf "%s\n%!") (List.map snd comments)
    end

  let loc_of_comments config loc =
    if G.config.G.comments then begin
      let comments = get_comments_from_buffer config.L.buffer config.L.comments loc in
      let init = ParseOCaml.get_c_num loc in
      List.fold_left
        (fun accu (s,e) -> min s (fst accu), max e (snd accu))
        init
        (List.map fst comments)
    end else
      ParseOCaml.get_c_num loc

  let process config loc =
    match G.config.G.format with
      | Some G.Loc     -> pprint_c_loc (loc_of_comments config loc)
      | Some G.Loc_end -> Printf.printf "%d\n%!" (snd (loc_of_comments config loc))
      | Some G.Str
      | None           ->
        let s, e = loc_of_comments config loc in
        Printf.printf "%s\n%!" (String.sub config.L.buffer s (e-s))

  let pstr config si =
    process config ( Structure.get_loc si)

  let psig config si =
    process config (Signature.get_loc si)

  let make str_fn sig_fn config name =
    match config.L.ast with
      | Some (L.Structure s) -> pstr config (str_fn s name)
      | Some (L.Signature s) -> psig config (sig_fn s name)
      | None               ->
        Printf.eprintf "You need to specify at least one file\n%!";
        nice_exit ()

  let get_type_by_name =
    make Structure.get_type_by_name Signature.get_type_by_name

  let get_value_by_name =
    make Structure.get_value_by_name Signature.get_value_by_name

  let get_module_by_name =
    make Structure.get_module_by_name Signature.get_module_by_name

  let get_by_pos =
    make Structure.get_by_pos Signature.get_by_pos

  let process op =
    let aux fn kind config name =
      let path = path_of_string name in
      try fn config path
      with Not_found ->
        Printf.eprintf "Could not find any %s named %s in %s\n%!" kind name (L.filename config);
        G.should_exit () in
    match op with
      | Ops.Type (config, name)    -> aux get_type_by_name "type" config name
      | Ops.Value (config, name)   -> aux get_value_by_name "value" config name
      | Ops.Module (config, name)  -> aux get_module_by_name "module" config name
      | Ops.Position (config, pos) ->
          try get_by_pos config pos
          with Not_found ->
            Printf.eprintf "Could not find any item at position %d in %s\n" pos (L.filename config);
            G.should_exit ()

  let process_all () =
    while not (Queue.is_empty Ops.queue) do
      process (Queue.take Ops.queue)
    done

end

let arg_list = Arg.align [
  (* Global options *)
  "-format"  , Global.format_fun  , " choose output format (default is 'str')";
  "-comments", Global.comments_fun, " extract preceeding comment too";
  "-k"       , Global.continue_fun, " continue in case of error";

  (* Local options *)
  "-pos"   , Arg.Int    Ops.get_by_pos        , "<int> extract a top-level item";
  "-type"  , Arg.String Ops.get_type_by_name  , "<name> extract a type definition";
  "-val"   , Arg.String Ops.get_value_by_name , "<name> extract a value definition";
  "-module", Arg.String Ops.get_module_by_name, "<name> extract a module definition";
  "-str"   , Arg.String Local.set_string      , "<str> use the argument as an implementation or interface";
  "-impl"  , Arg.String Local.set_implementation, "<file> consider the given file as an .ml file";
  "-intf"  , Arg.String Local.set_interface     , "<file> consider the given file as an .mli file";
]

let _ =
  Arg.parse arg_list Local.set_filename arg_usage;
  Process.process_all ()
