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

(* open SafeCaml *)
open Genlex
open BuildOCPTypes
open BuildMisc

open Ocamllexer
open BuildOCPParser

type config = {
  config_options : source_options;
  config_files : (string * set_option list) list;
  config_requires : string list;
  config_dirname : File.t;
  config_filename : string;
}

let read_ocamlconf filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let lexer = Ocamllexer.make_lexer
    [ "begin"; "end"; "true"; "false";
      "library"; "program"; "objects"; "config"; "include"; "type";
      "files"; "requires"; "file"; "use"; "pack";
      "["; "]"; ";"; "("; ")"; "="; "+=";
    ]
  in
  let token_of_token token_opt =
    match token_opt with
	None -> EOF
      | Some token ->
	match token with
	  | String s -> STRING s
	  | Float f -> FLOAT f
	  | Int i -> INT i
	  | Char c -> CHAR c
	  | Kwd ";" -> SEMI
	  | Kwd "[" -> LBRACKET
	  | Kwd "]" -> RBRACKET
	  | Kwd "(" -> LPAREN
	  | Kwd ")" -> RPAREN
	  | Kwd "begin" -> BEGIN
	  | Kwd "end" -> END
	  | Kwd "objects" -> OBJECTS
	  | Kwd "library" -> LIBRARY
	  | Kwd "config" -> CONFIG
	  | Kwd "use" -> USE
	  | Kwd "program" -> PROGRAM
	  | Kwd "type" -> TYPE
	  | Kwd "include" -> INCLUDE
	  | Kwd "=" -> EQUAL
	  | Kwd "+=" -> PLUSEQUAL
	  | Kwd "-=" -> MINUSEQUAL
	  | Kwd "true" -> TRUE
	  | Kwd "false" -> FALSE
	  | Kwd "files" -> FILES
	  | Kwd "file" -> FILE
	  | Kwd "requires" -> REQUIRES
	  | Kwd "pack" -> PACK
	  | Ident s -> IDENT s
	  | Kwd _ -> assert false

  in
  let ast =
    try
      BuildOCPParser.main (fun lexbuf -> token_of_token (lexer lexbuf)) lexbuf
    with Parsing.Parse_error ->
      BuildMisc.print_loc filename (Lexing.lexeme_start lexbuf);
      Printf.eprintf "Parse error\n%!";
      exit 2
  in
  close_in ic;
  ast


let empty_config () =
  { config_options = new_options ();
    config_files = [];
    config_requires = [];
    config_dirname = File.of_string "";
    config_filename = "";
  }

let configs = Hashtbl.create 17


let define_config config_name options = Hashtbl.add configs config_name options
let find_config config_name = Hashtbl.find configs config_name

let option_list_set options name list =
  { options with
    options_vars =
      StringMap.add name (OptionList list) options.options_vars }

let rec option_list_remove prev list =
  match list with
      [] -> prev
    | first_ele :: next_eles ->
      let rec iter prev before =
        match prev with
            [] -> List.rev before
          | x :: tail ->
            if x = first_ele then
              cut_after x tail tail next_eles before
            else
              iter tail (x :: before)

      and cut_after head tail list1 list2 before =
        match list1, list2 with
            _, [] -> iter list1 before
          | x1 :: tail1, x2 :: tail2 when x1 = x2 ->
            cut_after head tail tail1 tail2 before
          | _ -> iter tail (head :: before)
      in
      iter prev []

let option_list_remove options name list =
    try
      match StringMap.find name options.options_vars with
	  OptionList prev ->
            let new_list = option_list_remove prev list in
            { options with
              options_vars =
                StringMap.add name (OptionList new_list) options.options_vars }
	| _ ->
	  failwith  (Printf.sprintf "OptionListRemove %s: incompatible values" name);
    with Not_found -> options



let option_list_append options name list =
  let list =
    try
      match StringMap.find name options.options_vars with
	  OptionList prev -> prev @ list
	| _ ->
	  failwith  (Printf.sprintf "OptionListAppend %s: incompatible values" name);
    with Not_found -> list
  in
  { options with
    options_vars =
      StringMap.add name (OptionList list) options.options_vars }

let options_list_append options names list =
  List.fold_left (fun options var ->
    option_list_append options var list
  ) options names

let options_list_remove options names list =
  List.fold_left (fun options var ->
    option_list_remove options var list
  ) options names

let options_list_set options names list =
  List.fold_left (fun options var ->
    option_list_set options var list
  ) options names

let meta_options = [
  "o",      [ "dep"; "bytecomp"; "bytelink"; "asmcomp"; "asmlink" ];
  "byte",   [        "bytecomp"; "bytelink"; ];
  "asm",   [        "asmcomp"; "asmlink"; ];
  "comp",   [        "bytecomp"; "asmcomp"; ];
  "link",   [        "bytelink"; "asmlink"; ];
]

let meta_options =
  let list = ref StringMap.empty in
  List.iter (fun (name, names) -> list := StringMap.add name names !list) meta_options;
  !list

let option_list_set options name list =
  try
    let names = StringMap.find name meta_options in
    options_list_set options names list
  with Not_found -> option_list_set options name list

let option_list_append options name list =
  try
    let names = StringMap.find name meta_options in
    options_list_append options names list
  with Not_found -> option_list_append options name list


let option_list_remove options name list =
  try
    let names = StringMap.find name meta_options in
    options_list_remove options names list
  with Not_found -> option_list_remove options name list



let rec translate_options options list =
  match list with
      [] -> options
    | option :: list ->
      let options = translate_option options option in
      translate_options options list

and translate_option options op =
  match op with
    | OptionConfigSet config_name ->
	find_config config_name options

    | OptionListSet (name, list) -> option_list_set options name list

    | OptionListAppend (name, list) -> option_list_append options name list
    | OptionListRemove (name, list) -> option_list_remove options name list

    | OptionBoolSet (name, bool) ->
      { options with
	options_vars =
	  StringMap.add name (OptionBool bool) options.options_vars }

(*
module MakeParser(BuildGlobals : sig

  type project_info

  val new_project_id : unit -> int
  val register_project : project_info  BuildOCPTypes.project -> unit
  val new_project :
	  (* project name *) string ->
	  (* project dirname *) string ->
	  (* configuration filename *) string ->
    project_info BuildOCPTypes.project

  val register_installed : string -> unit

end) = struct
*)

open BuildOCPTree


let add_project_dep pk link s =
  try
    let pd = StringMap.find s pk.package_deps in
    pd.dep_link <- pd.dep_link || link
  with Not_found ->
    pk.package_deps <- StringMap.add s {
      dep_for = [];
      dep_project = s;
      dep_link = link;
    } pk.package_deps

let define_project pj name config kind =
  let pk = new_package pj name
    config.config_dirname config.config_filename
  in
  let project_options = config.config_options in
  pk.package_type <- kind;
  pk.package_provides <- name;
  pk.package_sources <-  config.config_files;
  List.iter (fun dep -> add_project_dep pk true dep) config.config_requires;
  pk.package_options <- project_options;
  begin try
	  match StringMap.find dirname_option.option_name project_options.options_vars with
	      OptionList list ->
		pk.package_dirname <- File.add_basenames (File.of_string ".") list
	    | _ -> raise Not_found
    with Not_found -> ()
  end

let define_package pj program_name config package_type =
  define_project pj program_name config package_type

let rec translate_statements pj config list =
  match list with
      [] -> config
    | stmt :: list ->
      let config =
	match stmt with
	  | StmtDefineConfig (config_name, options) ->
	    begin
(*	      let options = translate_options config.config_options options in *)
	      define_config config_name (fun old_options -> translate_options old_options options);
	    end;
	    config
	  | StmtDefinePackage (package_type, library_name, simple_statements) ->
	    begin
	      let config = translate_simple_statements pj config simple_statements in
	      define_package pj library_name config package_type
	    end;
	    config
	  | StmtBlock statements ->
	    ignore (translate_statements pj config statements);
	    config
	  | _ -> translate_simple_statement pj config stmt
      in
      translate_statements pj config list


and translate_simple_statements pj config list =
  match list with
      [] -> config
    | stmt :: list ->
      let config = translate_simple_statement pj config stmt in
      translate_simple_statements pj config list

and translate_simple_statement pj config stmt =
  match stmt with
      StmtInclude filename ->
	translate_statements pj config (read_ocamlconf filename)
    | StmtRequiresSet requirements ->
      { config with config_requires = requirements }
    | StmtRequiresAppend requirements ->
      { config with config_requires = config.config_requires @ requirements }
    | StmtFilesSet files ->
(*      let files = List.map (fun (file, options) ->
	(file, translate_options config.config_options options)) files in *)
      { config with config_files = files }
    | StmtFilesAppend files ->
(*      let files = List.map (fun (file, options) ->
	(file, translate_options config.config_options options)) files in *)
      { config with config_files = config.config_files @ files }
    | StmtOption option ->
      { config with config_options = translate_option config.config_options option }
    | StmtBlock _
    | StmtDefinePackage _
    | StmtDefineConfig _ -> assert false

let read_ocamlconf pj config filename =
  let ast = read_ocamlconf filename in
  translate_statements pj
    { config
      with
	config_files = [];
	config_requires = [];
	config_dirname = File.dirname (File.of_string filename);
	config_filename = filename;
    } ast

(*
end
*)

