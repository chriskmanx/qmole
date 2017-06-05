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
open BuildEngineTypes


type package_type =
    ProjectProgram
  | ProjectToplevel
  | ProjectLibrary
  | ProjectObjects


type statement =
    StmtOption of set_option
  | StmtBlock of statement list
  | StmtInclude of string
  | StmtDefineConfig of string * set_option list
  | StmtDefinePackage of package_type * string * statement list
  | StmtFilesSet of (string * set_option list) list
  | StmtFilesAppend of (string * set_option list) list
  | StmtRequiresSet of string list
  | StmtRequiresAppend of string list

and set_option =
    OptionListSet of string * string list
  | OptionListAppend of string * string list
  | OptionListRemove of string * string list
  | OptionBoolSet of string * bool
  | OptionConfigSet of string
(*  | OptionConfigAppend of string *)

type package = {
  package_project : project;
  package_name : string; (* basename of project *)
  mutable package_dirname : File.t; (* where the project files are *)

  mutable package_provides : string; (* what the project provides,
					default "" => same as name.
					if provides is specified, then
					the name of the object should
					be that one. TODO: it should
					be an option, since it should
					apply to modules too. *)
  mutable package_type : package_type; (* what it generates *)
  mutable package_tag : string; (* if tags are specified, then the
				   following rules apply when choosing
				   among several projects providing
				   the same name,

				   1/ if a tag is used in one of the
				   other projects, then the project
				   with that tag is preferred.

				   2/ If several tags are used in
				   other projects, and these tags are
				   present in different projects, then
				   a conflict is found and an error is
				   raised.

				   For example, if tags "debug" and
				   "threads" are used, then a conflict
				   is found if "stdlib" has two
				   versions, "debug" and "threads",
				   but the conflict is resolved if
				   "stdlib" also has a
				   "threads"+"debug" version.

				   Automatic tags: a "debug" version
				   is always compiled for bytecode and
				   native code, and a "profile"
				   version is always compiled for
				   native code. They are stored in
				   project+"+debug" and
				   project+"+profile", using the
				   interfaces from project. (how to do that ?)
				*)
  mutable package_version : string; (* unused: TODO *)
  mutable package_auto : string option; (* unused: TODO *)

  package_loc : int;
  package_filename : string;
  package_id : int;

  package_node : Toposort.node;
  mutable package_missing_deps : int;

  mutable package_has_byte_debug : bool;   (* unused: TODO *)
  mutable package_has_asm_debug : bool;    (* unused: TODO *)
  mutable package_has_asm_profile : bool;  (* unused: TODO *)

  mutable package_sources : (string * set_option list) list; (* the sources of the project, plus the flags to compile them. *)

  (* list of projects, on which compilation depends *)
  mutable package_deps : string package_dependency StringMap.t;
  (* bool = should the project be linked (true) or just a dependency (false) *)
  mutable package_requires : package package_dependency list;
  mutable package_added : bool;

  mutable package_options : source_options;
}

and 'a package_dependency =
    {
      dep_project : 'a;
      mutable dep_for : string list;
      mutable dep_link : bool;
    }

and source_options =
    {
      options_vars : option_value StringMap.t;
(*      options_inherit : source_options option; *)
      options_inherit : unit;
    }

and option_value =
    OptionBool of bool
  | OptionString of string
  | OptionList of string list

and project = {
  mutable project_dir : File.t;
  mutable project_file : File.t;
  project_config : SimpleConfig.config_file;
  mutable project_packages : package IntMap.t;
  mutable project_npackages : int;
  project_files : File.t list SimpleConfig.config_option;

  project_validated : (string * string, package) Hashtbl.t;
  project_missing : (string * string, package list ref) Hashtbl.t;
  mutable project_disabled : package list;
  mutable project_incomplete : package list;
  mutable project_sorted : package list;
}

(*
let disabled_projects = ref ([] : BuildTypes.package_info list)
let incomplete_projects = ref ([] : BuildTypes.package_info list)
*)
















let ocp_verbosity = ref 0

let new_options () = {
    options_vars = StringMap.empty;
(*    options_inherit = None; *)
  options_inherit = ();
}

let default_options = new_options ()

type 'a source_option = {
  option_name : string;
  mutable option_default : 'a;
}

let new_bool_option name default =
  { option_name = name; option_default = default }

let new_strings_option name (default : string list) =
  { option_name = name; option_default = default }

let options_find option options =
(*  try *)
    StringMap.find option.option_name options.options_vars
(*  with Not_found as e ->
    match options.options_inherit with
	None -> raise e
      | Some options ->
	StringMap.find option.option_name options.options_vars
*)

let bool_option_true options bool_option =
  try
    match options_find bool_option options with
	OptionBool bool -> bool
      | _ ->
	Printf.eprintf "Warning: bad type for bool option %s. Returning default value %b\n%!" bool_option.option_name bool_option.option_default;
	bool_option.option_default
  with Not_found -> bool_option.option_default

let set_strings_option strings_option default =
  strings_option.option_default <- default

let strings_option options strings_option =
  try
    match options_find strings_option options with
	OptionList list -> list
      | _ ->
	Printf.eprintf "Warning: bad type for string list option %s. Returning default value [%s]\n%!" strings_option.option_name
	  (String.concat ";" strings_option.option_default);
	strings_option.option_default
  with Not_found -> strings_option.option_default

let direct_strings_option options strings_option =
  try
    match StringMap.find strings_option.option_name options.options_vars with
	OptionList list -> list
      | _ ->
	Printf.eprintf "Warning: bad type for string list option %s. Returning default value [%s]\n%!" strings_option.option_name
	  (String.concat ";" strings_option.option_default);
	strings_option.option_default
  with Not_found -> strings_option.option_default

let string_option options string_option =  String.concat " " (strings_option options string_option)

let get_strings_option strings_option =
  String.concat " " strings_option.option_default

let package_option = new_strings_option "package" ([] : string list)
let dirname_option = new_strings_option "dirname" ([] : string list)
let subdir_option = new_strings_option "subdir" ([] : string list)
let cclib_option = new_strings_option "cclib" ([] : string list)
let ccopt_option = new_strings_option "ccopt" ([] : string list)
let cflags_option = new_strings_option "cflags" ([] : string list)
let pp_option = new_strings_option "pp" []


let install_interface_option = new_bool_option "install_cmi" true
let generated_option = new_bool_option "generated" false
let enabled_option = new_bool_option "enabled" true
let byte_option = new_bool_option "has_byte" true
let asm_option = new_bool_option "has_asm" true
let pack_option = new_strings_option "pack" ([] : string list)
let packed_option = new_strings_option "packed" ([] : string list)
let ml_file_option = new_bool_option "ml" false
let sort_files_option = new_bool_option "sort" false
let mli_file_option = new_bool_option "mli" false
let pp_requires = new_strings_option "pp_requires" []
let no_mli_option = new_bool_option "no_mli" false


let bytecomp_option = new_strings_option "bytecomp" ([] : string list)
let bytelink_option = new_strings_option "bytelink" ([] : string list)
let asmcomp_option = new_strings_option "asmcomp" ([] : string list)
let asmlink_option = new_strings_option "asmlink" ([] : string list)
let dep_option = new_strings_option "dep" ([] : string list)

let modname_of_fullname fullname =
  let modname = Filename.chop_extension (Filename.basename fullname) in
  modname.[0] <- Char.uppercase modname.[0];
  modname

let rec string_of_set_option option =
  match option with
    | OptionListSet (x, list) ->
      Printf.sprintf "%s = [ %s ]" x (String.concat " " (List.map (fun s ->
        Printf.sprintf "\"%s\"" (String.escaped s)) list))
  | OptionListAppend (x, list) ->
      Printf.sprintf "%s += [ %s ]" x (String.concat " " (List.map (fun s ->
        Printf.sprintf "\"%s\"" (String.escaped s)) list))
  | OptionListRemove (x, list) ->
      Printf.sprintf "%s -= [ %s ]" x (String.concat " " (List.map (fun s ->
        Printf.sprintf "\"%s\"" (String.escaped s)) list))
    | OptionBoolSet (x, y) -> Printf.sprintf "%s = %b" x y
    | OptionConfigSet c -> Printf.sprintf "config \"%s\"" c


let new_package pj name (dirname : File.t) filename =
  let package_id = pj.project_npackages in
  pj.project_npackages <- pj.project_npackages + 1;
  let pk = {
    package_project = pj;
    package_id = package_id;
    package_tag = "";
    package_auto = None;
    package_version = "";
    package_loc = (-1);
    package_filename = filename;
    package_node = Toposort.new_node ();
    package_added = false;
    package_requires = [];
    package_name = name;
    package_missing_deps = 0;
    package_provides = name;
    package_type = ProjectLibrary;
(*    package_native = true; *)
(*    package_enabled = true; *)
    package_sources = [];
    package_dirname = dirname;
    package_deps = StringMap.empty;
    package_options = new_options ();
(*    package_cflags = ""; *)
(*    package_cclib = ""; *)
(*    package_details = None; *)
(*    package_has_byte = true; *)
    package_has_byte_debug = false;
(*    package_has_asm = true; *)
    package_has_asm_debug = false;
    package_has_asm_profile = false;
  } in
  pj.project_packages <- IntMap.add pk.package_id pk pj.project_packages;
  pk
