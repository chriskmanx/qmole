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

(* Génération et lecture du fichier de configuration *)

open OcpLang

open Genlex
open BuildOCPTypes
open BuildGlobals
open BuildTypes
open BuildMisc



let lexer = Genlex.make_lexer  [ "=" ; "["; "]" ]


(* TODO: ocamlc should have a wrapper to try to use ocamlc.opt when possible *)
let ocamldep_cmd = new_strings_option "ocamldep" [ "ocamldep.opt" ]
let ocamlc_cmd = new_strings_option "ocamlc" [ "ocamlc.opt" ]
let ocamlopt_cmd = new_strings_option "ocamlopt" [ "ocamlopt.opt" ]
let ocamllex_cmd = new_strings_option "ocamllex" [ "ocamllex.opt" ]
let ocamlyacc_cmd = new_strings_option "ocamlyacc" [ "ocamlyacc" ]
let ranlib_cmd = ref "ranlib"
let ar_cmd = ref "ar"
let mklib_cmd = ref MKLIB_Unix
let bindir = ref "/usr/local/bin"
let libdirs = ref []

let parse_error_at filename loc msg =
  BuildMisc.print_loc filename loc;
  Printf.eprintf "Parse error: %s\n%!" msg;
  exit 2

let parse_error filename tokens msg =
  match tokens with
      [] -> assert false
    | (_, loc) :: _ -> parse_error_at filename loc msg

(* TODO: we should BuildOCPParse *)

let rec parse_file filename tokens =
  if !verbosity > 4 then begin
    match tokens with
	[] -> Printf.eprintf "end of stream\n%!"
      | (tok,_) :: _ -> Printf.eprintf "first token %s\n%!" (Genlex.string_of_token tok)
  end;
  match tokens with
      [] -> ()
    | (Ident ident, loc) :: (Kwd "=", _) :: (String s, _) :: next_tokens ->
      begin
	match ident with
	    "ocamlc" -> set_strings_option ocamlc_cmd [s]
	  | "ocamlopt" -> set_strings_option ocamlopt_cmd [ s ]
	  | "ocamldep" -> set_strings_option ocamldep_cmd [ s ]
	  | "ocamllex" -> set_strings_option ocamllex_cmd [ s ]
	  | "ocamlyacc" -> set_strings_option ocamlyacc_cmd [ s ]
	  | "mklib" -> begin
	    match String.lowercase s with
		"unix" -> mklib_cmd := MKLIB_Unix
	      | "msvc" -> mklib_cmd := MKLIB_Msvc
	      | _ ->
		parse_error filename tokens (Printf.sprintf "Unknown mklib value \"%s\"" s)
	  end
	  | "ranlib" -> ranlib_cmd := s
	  | "ar" -> ar_cmd := s
	  | "bindir" -> bindir := s
	  | cmd ->
	    parse_error filename tokens (Printf.sprintf "Unknown string ident \"%s\"" cmd)
      end;
      parse_file filename next_tokens
    | (Ident "mklib", loc) :: (Kwd "=", _) :: (Ident s, _) :: next_tokens ->
      begin
	match String.lowercase s with
	    "unix" -> mklib_cmd := MKLIB_Unix
	  | "msvc" -> mklib_cmd := MKLIB_Msvc
	  | _ ->
	    parse_error filename tokens (Printf.sprintf "Unknown mklib value \"%s\"" s)
      end;
      parse_file filename next_tokens
    | (Ident ident, loc) :: (Kwd "=", _) :: (Kwd "[", _) :: next_tokens ->
      let ref = match ident with
	| "libdirs" -> libdirs
	| cmd ->
	  parse_error filename tokens (Printf.sprintf "Unknown dict ident \"%s\"" cmd)
      in
      let (list, next_tokens) = parse_dict filename next_tokens [] in
      ref := list;
      parse_file filename next_tokens
    | (token, _) :: _ ->
      parse_error filename tokens (Printf.sprintf "Unexpected token %s"
				     (Genlex.string_of_token token))

and parse_dict filename tokens list =
  match tokens with
      (Ident name, _) :: (Kwd "=", _) :: (String s, _) :: next_tokens ->
	parse_dict filename next_tokens ( (name, s) :: list )
    | (Kwd "]", _) :: next_tokens ->
      (List.rev list, next_tokens)
    | _ -> parse_error filename tokens "Bad syntax for dict assoc"

let read_config_file filename =
  Printf.eprintf "Reading configuration file from %s\n%!" filename;
  try
    let tokens = BuildMisc.token_list_of_filename filename lexer in
    parse_file filename tokens;
    ()
  with BuildMisc.ParseError as e -> raise e

(*
let read_home_config () =
  read_config_file global_config_file
*)

let read_local_config local_config_file =
  read_config_file local_config_file


let sep_PATH =
  match Sys.os_type with
      "Win32" -> ';'
    | _ -> ':'

let get_PATH () =
  try
    let path = Sys.getenv "PATH" in
    String.split path sep_PATH
  with Not_found ->
    failwith "Env variable PATH not defined"

let check_command_exists filename =
  let st = Unix.stat filename in
  match st.Unix.st_kind with
      Unix.S_REG ->
	begin
	  try
	    Unix.access filename [Unix.X_OK];
	    filename
	  with e ->
	    Printf.eprintf "Warning: %s in PATH has not executable permission\n%!"
	      filename;
	    raise e
	end
    | _ ->
      Printf.eprintf "Warning: %s in PATH is not a regular command\n%!" filename;
      raise Not_found

let rec find_in_PATH command path =
  match path with
      [] -> raise Not_found
    | dirname :: path ->
      let filename = Filename.concat dirname command in
      try
	check_command_exists filename
      with _ ->
	find_in_PATH command path

let must_find_in_PATH command path =
  try
    find_in_PATH command path
  with Not_found ->
    Printf.eprintf "Error: Command %s not found in PATH. Abording.\n%!" command;
    exit 2

let opt_find_in_PATH command path =
  try
    Some (find_in_PATH command path)
  with Not_found -> None

let opt_command_exists command =
  try
    Some (check_command_exists command)
  with _ -> None

let b = Buffer.create 10000
let get_stdout_lines cmd args =
  let temp_file = Filename.temp_file "ocp-build-" ".out" in
  let pid = BuildMisc.create_process (cmd@args) (Some temp_file) None in
  let status = BuildMisc.wait_command pid in
  let lines = ref [] in
  begin try
	  let ic = open_in temp_file in
	  begin

	    try
	      while true do
		lines := (input_line ic) :: !lines
	      done
	    with _ -> ()
	  end;
	  close_in ic;
	  Sys.remove temp_file;
    with _ -> ()
  end;
  (status, List.rev !lines)

let get_stdout_line cmd args =
  try
  let (status, lines) = get_stdout_lines cmd args in
  if status <> 0 then failwith "Bad status";
  match lines with
      [] -> failwith "no output"
    | line :: _ -> line
  with e ->
    failwith (Printf.sprintf "get_stdout_lines %s: %s\n%!"
		(String.concat " " (cmd @ args)) (Printexc.to_string e))

let guess_config () =

  let path = get_PATH () in

  let ocamlc = must_find_in_PATH "ocamlc" path in
  set_strings_option ocamlc_cmd [ocamlc];
  let bindir = Filename.dirname ocamlc in
  let version = get_stdout_line [ocamlc] [ "-version" ] in
  let libdir = get_stdout_line [ocamlc] [ "-where" ] in

  libdirs := [ ("std", libdir) ];

  let opt_command_exists compiler =
    opt_command_exists (Filename.concat bindir compiler)
  in

  let check_compiler compiler =
    let compiler = opt_command_exists compiler in
    match compiler with
	None -> None
      | Some compiler ->
	let opt_version = get_stdout_line [compiler] [ "-version" ] in
	let opt_libdir = get_stdout_line [compiler] [ "-where" ] in
	if opt_version = version && opt_libdir = libdir then
	  Some compiler
	else begin
	  Printf.eprintf "Warning: %s and %s have different versions or libdir\n%!"
	    ocamlc compiler;
	  None
	end
  in

  begin
    match check_compiler "ocamlc.opt" with
	None -> ()
      | Some ocamlc -> set_strings_option ocamlc_cmd [ocamlc]
  end;

  begin
    match check_compiler "ocamlopt.opt" with
      | Some ocamlc -> set_strings_option ocamlopt_cmd [ocamlc]
      | None ->
	match check_compiler "ocamlopt" with
	  | Some ocamlc -> set_strings_option ocamlopt_cmd [ocamlc]
	  | None -> ()
  end;

  begin
    match opt_command_exists "ocamllex.opt" with
      | Some ocamlc -> set_strings_option ocamllex_cmd [ocamlc]
      | None ->
	match opt_command_exists "ocamllex" with
	  | Some ocamlc -> set_strings_option ocamllex_cmd [ocamlc]
	  | None -> ()
  end;

  begin
    match opt_command_exists "ocamldep.opt" with
      | Some ocamlc -> set_strings_option ocamldep_cmd [ocamlc]
      | None ->
	match opt_command_exists "ocamldep" with
	  | Some ocamlc -> set_strings_option ocamldep_cmd [ocamlc]
	  | None -> ()
  end;

  begin
    match opt_command_exists "ocamlyacc" with
      | Some ocamlc -> set_strings_option ocamlyacc_cmd  [ocamlc]
      | None -> ()
  end;
  ()

let save_config filename =
  safe_mkdir (Filename.dirname filename);
  let oc = open_out filename in

  Printf.fprintf oc "ocamlc = \"%s\"\n" (get_strings_option ocamlc_cmd);
  Printf.fprintf oc "ocamlopt = \"%s\"\n" (get_strings_option ocamlopt_cmd);
  Printf.fprintf oc "ocamldep = \"%s\"\n" (get_strings_option ocamldep_cmd);
  Printf.fprintf oc "ocamllex = \"%s\"\n" (get_strings_option ocamllex_cmd);
  Printf.fprintf oc "ocamlyacc = \"%s\"\n" (get_strings_option ocamlyacc_cmd);
  Printf.fprintf oc "mklib = %s\n" (match !mklib_cmd with
      MKLIB_Unix -> "unix"
    | MKLIB_Msvc -> "msvc");
  Printf.fprintf oc "ranlib = \"%s\"\n" !ranlib_cmd;
  Printf.fprintf oc "ar = \"%s\"\n" !ar_cmd;

  Printf.fprintf oc "bindir = \"%s\"\n" !bindir;
  Printf.fprintf oc "libdirs = [\n";
  List.iter (fun (name, libdir) ->
    Printf.fprintf oc "\t%s = \"%s\"\n" name libdir
  ) !libdirs;
  Printf.fprintf oc "]\n";

  close_out oc;

  Printf.eprintf "Configuration filename %s generated\n%!" filename;
  ()

let generate_config_file filename =
  guess_config ();
  save_config filename

let load_config local_config_file =

(*  let has_global_config_file = Sys.file_exists global_config_file in *)
  let has_local_config_file = Sys.file_exists local_config_file in

(*
  if has_global_config_file && not !no_global_arg then
    read_config_file global_config_file;
*)

  if has_local_config_file then
    read_config_file local_config_file;

(*
  if not has_local_config_file  && not has_global_config_file then begin
    if !verbosity_arg > 0 then begin
      Printf.eprintf "Warning: Could not find any configuration file. You should call:\n%!";
      Printf.eprintf "\t$ %s -conf\n%!" Sys.argv.(0);
    end;
    guess_config ();
  end;
*)
  ()

