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
open BuildOCPTypes
open SimpleConfig

let rec validate_project pk =
  let pj = pk.package_project in
  if !ocp_verbosity > 1 then
    Printf.eprintf "validate_project: %s, tag=%s, id=%d\n" pk.package_name pk.package_tag pk.package_id;
  if pk.package_missing_deps = 0 then begin
    let key =  (pk.package_name, pk.package_tag) in
    begin try
            let pk2 = Hashtbl.find pj.project_validated key in
            Printf.eprintf "Error: two projects called %s\n" pk.package_name;
            Printf.eprintf "  One is defined in %s\n" (File.to_string pk.package_dirname);
            Printf.eprintf "  and one is defined in %s\n" (File.to_string pk2.package_dirname);
            Printf.eprintf "%!";
            exit 2
      with Not_found -> ()
    end;
    Hashtbl.add pj.project_validated key pk;
    try
      let list_ref = Hashtbl.find pj.project_missing key in
      Hashtbl.remove pj.project_missing key;
      List.iter (fun pk2 ->
	pk2.package_missing_deps <- pk2.package_missing_deps - 1;
	let pkdep = StringMap.find pk.package_name pk2.package_deps in
	pk2.package_requires <- { pkdep with dep_project = pk } :: pk2.package_requires;
	validate_project pk2
      ) !list_ref;
    with Not_found -> ()
  end

let check_project pk =
  let pj = pk.package_project in
  if bool_option_true pk.package_options enabled_option then begin

    pk.package_missing_deps <- 0;
    StringMap.iter (fun name pkdep ->
      let key = (name, "") in (* TODO: we should use a datastructure that can handle
                                 dependencies by tag and by version *)
      try
	let pk2 = Hashtbl.find pj.project_validated key in
	pk.package_requires <- { pkdep with dep_project = pk2 } :: pk.package_requires;
      with Not_found ->
	let list_ref =
	  try
	    Hashtbl.find pj.project_missing key
	  with Not_found ->
	    let list_ref = ref [] in
	    Hashtbl.add pj.project_missing key list_ref;
	    list_ref
	in
	list_ref := pk :: !list_ref;
	pk.package_missing_deps <- pk.package_missing_deps + 1
    ) pk.package_deps;
    validate_project pk
  end



(*
val find_project : (File.t -> File.t)
*)
let find_project file_t basename =
  let rec find dirname basename =
    let file = File.add_basename dirname basename in
    if File.X.exists file then file else
      let new_dirname = File.dirname dirname in
      if new_dirname == dirname then raise Not_found;
      find new_dirname basename
  in
  let file_t = if File.is_absolute file_t then file_t else
      File.concat (File.X.getcwd ()) file_t
  in
  find file_t basename

(*
val open_project : (File.t -> project)
*)
let open_project file_t =

  let config_file = SimpleConfig.create_config_file file_t in
  let files = create_option config_file [ "files" ]
   [ "List of configuration files for this project"]
    (list_option file_option) []
  in
  let pj =
  {
    project_config = config_file;
    project_file = file_t;
    project_dir = File.dirname file_t;
    project_files = files;
    project_packages = IntMap.empty;
    project_npackages = 0;
    project_missing = Hashtbl.create 111;
    project_validated = Hashtbl.create 111;
    project_disabled = [];
    project_incomplete = [];
    project_sorted = [];
  }
  in
  SimpleConfig.load config_file;
  pj

module PackageSorter = Toposort.Make(struct
  type t = package  package_dependency
  let node pd = pd.dep_project.package_node
  let iter_deps f pd = List.iter f pd.dep_project.package_requires
end)


(* Do a closure of all dependencies for this project *)
let update_deps pj =

  if !ocp_verbosity > 1 then begin
    Printf.eprintf "BEFORE update_deps: Project %s depends on:\n%!" pj.package_name;
    List.iter (fun pd ->
      Printf.eprintf "\t%s\n%!" pd.dep_project.package_name
    ) pj.package_requires
  end;

  let list = ref [] in
  let deps = Hashtbl.create 111 in
  let rec add_dep pd =
    let pj2 = pd.dep_project in
    let dep_link =
      match pj2.package_type with
	  ProjectLibrary
	| ProjectObjects -> pd.dep_link
	| ProjectProgram
	| ProjectToplevel -> false
    in
    let add_more_deps =
      try
	let pd2 = Hashtbl.find deps pj2.package_id in
	if (not pd2.dep_link) && dep_link then begin
	  pd2.dep_link <- true;
	  true
	end else false
      with Not_found ->
	let pd2 = { pd with dep_link = dep_link } in
	list :=  pd2 :: !list;
	Hashtbl.add deps pj2.package_id pd;
	dep_link
    in
    if add_more_deps then
      List.iter add_dep pj2.package_requires
  in
  List.iter add_dep pj.package_requires;
(*  TODO: do better
List.iter (fun pd ->
    List.iter add_dep pj.package_requires
  ) pj.package_requires; *)
  pj.package_requires <- PackageSorter.sort_sorted !list;

  if !ocp_verbosity > 1 then begin
    Printf.eprintf "AFTER update_deps: Project %s depends on:\n%!" pj.package_name;
    List.iter (fun pd ->
      Printf.eprintf "\t%s%s\n%!" pd.dep_project.package_name
	(if pd.dep_link then "" else "(nolink)")
    ) pj.package_requires
  end;

  ()

(*
val load_packages : (project -> int)
*)
let load_packages pj =
  let nerrors = ref 0 in

  let config = BuildOCPParse.empty_config () in

  let rec iter parents files =
    match files with
	[] -> ()
      | file :: next_files ->
	match parents with
	    [] -> assert false
	  | (parent, config) :: next_parents ->
            let file = File.to_string file in
	    if OcpString.starts_with file parent then
	      let dirname = Filename.dirname file in
	      if !ocp_verbosity > 2 then
	        Printf.eprintf "Reading %s with context from %s\n%!" file parent;
	      let config =
		try
		  BuildOCPParse.read_ocamlconf pj config file
		with BuildMisc.ParseError ->
		  incr nerrors;
		  config
	      in
	      iter ( (dirname, config) :: parents ) next_files
	    else
	      iter next_parents files
  in
  let _config = iter [ "", config ] !! (pj.project_files) in

  IntMap.iter (fun _ pk -> check_project pk) pj.project_packages;
  IntMap.iter (fun _ pk ->
    if bool_option_true pk.package_options enabled_option then begin
      if pk.package_missing_deps > 0 then
	pj.project_incomplete <- pk :: pj.project_incomplete
    end else
      pj.project_disabled <- pk :: pj.project_disabled
  ) pj.project_packages;

  let list = ref [] in
  Hashtbl.iter (fun _ pj ->
    list := { dep_project = pj; dep_for = []; dep_link = true } :: !list
  ) pj.project_validated;
  let list = PackageSorter.sort !list in
  pj.project_sorted <- List.map (fun pd -> pd.dep_project) list;
  List.iter update_deps pj.project_sorted;
  !nerrors

(*
val save_project : (File.t -> (project -> unit))
*)
let save_project file_t pj =
  save_with_help pj.project_config

(*
val scan_project : (project -> unit)
*)
let scan_project pj =
  let files = ref [] in
  BuildScanner.scan_directory_for_suffix
    (File.to_string pj.project_dir) ".ocp" (fun filename ->
    files := File.of_string filename :: !files);
  pj.project_files =:= List.rev !files;
  save_project pj.project_file pj;
  ()

(*

  if !list_ocp_files || !verbosity_arg > 1 then begin
    Printf.eprintf "Found %d project files:\n%!" (List.length !files);
    List.iter (fun file ->
      Printf.eprintf "\t%s\n%!" file) !files;
  end;

*)

let find_package pj file =
  let list = ref [] in

  let st = File.X.lstat file in
  let dir_t = pj.project_dir in
  let _dir = File.to_string dir_t in
  let check_file pk filename =
    let filename = File.of_string filename in
    let file = File.concat pk.package_dirname filename in
    try
      let st2 = File.X.lstat file in
      if
        st.Unix.st_ino = st2.Unix.st_ino &&
        st.Unix.st_dev = st2.Unix.st_dev then
        list := pk :: !list
    with _ -> ()
  in
  List.iter (fun pk ->
    List.iter (fun (filename, _) ->
      check_file pk filename;
      let (kernel, extension) = File.cut_last_extension filename in
      match extension with
        | "ml" -> check_file pk (filename ^ ".mli")
        | "mli" -> ()
        | "mll" -> check_file pk (filename ^ ".ml")
        | "mly" ->
          check_file pk (filename ^ ".ml");
          check_file pk (filename ^ ".mli")
        | _ -> ()
    ) pk.package_sources
  ) pj.project_sorted;

  !list
