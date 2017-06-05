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


(* TODO: generates "order" directives to define an order between two unrelated
projects *)


open OcpLang
open OcpSystem

open BuildConfig
open BuildOCPTypes
open BuildTypes
open BuildGlobals

(*
let distrib_file_basename =  "ocp-distrib.ocp"
let local_config_file = Filename.concat build_dir_basename distrib_file_basename
let global_config_file = Filename.concat ocpdir distrib_file_basename
*)

let new_installed_project_id = BuildGlobals.new_id_generator ()


open BuildObjectInspector

let projects_by_name = Hashtbl.create 111
let installed_projects = Hashtbl.create 111

let register_project pj =
  Printf.eprintf "OLD INSTALLED [%s]x[%s]\n" pj.project_name pj.project_dirname;
  let name =
    if pj.project_provides = "" then pj.project_name else
      pj.project_provides
  in
  Hashtbl.add installed_projects (pj.project_name, pj.project_dirname) pj;
  try
    let list = Hashtbl.find projects_by_name name in
    list := pj :: !list
  with Not_found ->
    Hashtbl.add projects_by_name name (ref [pj])

let file_mtime filename =
  try
    (Unix.lstat filename).Unix.st_mtime
  with _ -> max_float

module InstalledOCPParser = BuildOCPParse.MakeParser(struct

  type project_info = BuildTypes.project_info

  let new_options = new_options
  let new_project_id = BuildGlobals.new_project_id
  let new_options = new_options
  let new_project = BuildGlobals.new_project
  let default_options = default_options

  let register_project pj =
    let registerp = match pj.project_auto with
	None -> true
      | Some filename ->
	if Sys.file_exists filename then  begin
	  let registerp = file_mtime filename < file_mtime pj.project_filename in
	  if not registerp then
	    Printf.eprintf "Removing obsolete description of %s from %s\n%!"
	      pj.project_name pj.project_dirname;
	  registerp
	end else false
    in
    if registerp then begin
(*      pj.project_has_byte <- false; *)
      pj.project_has_byte_debug <- false;
(*      pj.project_has_asm <- false; *)
      pj.project_has_asm_debug <- false;
      pj.project_has_asm_profile <- false;
      register_project pj
    end

  let register_installed _ = ()
end)

let output_projects_by_name target_dirname =
  Hashtbl.iter (fun name list ->
    let target_filename =
      (Filename.concat target_dirname (Printf.sprintf "ocp-installed-%s.ocp" name)) in
    Printf.eprintf "Generating file %s\n%!" target_filename;
    let oc = open_out target_filename in
    Printf.fprintf oc "generated = true\n";
    List.iter (fun pj ->
      if pj.project_sources <> [] then
	BuildOCPPrint.output_project oc pj
    ) !list;
    close_out oc
  ) projects_by_name

let gen_from_distrib target_dirname =


(*
  let nerrors = ref 0 in
  BuildScanner.scan_directory_for_suffix target_dirname ".ocp" (fun filename ->
    try
      InstalledOCPParser.read_ocamlconf filename
    with BuildMisc.ParseError -> incr nerrors);
  if !nerrors > 0 then exit 2;

  let new_projects = ref [] in
  let cmi_providers = Hashtbl.create 111 in
  let cmx_providers = Hashtbl.create 111 in

  let provide_cmx_module pj modname digest =
    let set =
      try
	Hashtbl.find cmx_providers (modname, digest)
      with Not_found ->
	let set = ref IntMap.empty in
	Hashtbl.add cmx_providers (modname, digest) set;
	set
    in
    if not (IntMap.mem pj.project_id !set) then
      set := IntMap.add pj.project_id pj !set
  in

  let provide_cmi_module pj modname digest =
    let set =
      try
	Hashtbl.find cmi_providers (modname, digest)
      with Not_found ->
	let set = ref IntMap.empty in
	Hashtbl.add cmi_providers (modname, digest) set;
	set
    in
    if not (IntMap.mem pj.project_id !set) then
      set := IntMap.add pj.project_id pj !set
  in

  let load_providing_library pj filename =
    Printf.eprintf "load_object %s\n%!" filename;
    let obj_desc = BuildObjectInspector.load_object_file filename in
    match obj_desc with
	BuildObjectInspector.CMA lib ->
	  List.iter (fun unit ->
	    List.iter (fun (intfname, digest) ->
	      if intfname = unit.cu_name then
		provide_cmi_module pj intfname digest
	    ) unit.cu_imports;
	  ) lib.cma_units

      | BuildObjectInspector.CMXA lib ->
	List.iter (fun (unit, digest) ->
	  provide_cmx_module pj unit.ui_name digest;
	  List.iter (fun (intfname, digest) ->
	    if intfname = unit.ui_name then
	      provide_cmi_module pj intfname digest
	  ) unit.ui_imports_cmi;
	) lib.cmxa_units


      | _ -> assert false
  in

  let add_project extension filename =
    let is_byte = match extension with
	".cma" -> true
      | ".cmxa" -> false
      | _ -> assert false
    in
    let is_profile = ref false in
    let is_debug = ref false in

    let dirname = Filename.dirname filename in
    let basename = Filename.basename filename in
    let kernel_name = Filename.chop_suffix basename extension in
    let kernel_name =
      if Filename.check_suffix kernel_name ".p" then begin
	is_profile := true;
	Filename.chop_suffix kernel_name ".p"
      end else
	if Filename.check_suffix kernel_name ".d" then begin
	  is_debug := true;
	  Filename.chop_suffix kernel_name ".d"
	end else kernel_name
    in
    Printf.eprintf "FIND INSTALLED [%s]x[%s]\n" dirname kernel_name;
    let pj = try
	       Hashtbl.find installed_projects (kernel_name, dirname)
      with Not_found ->
	let pj = new_project kernel_name dirname "" in
	register_project pj;
	new_projects := pj :: !new_projects;
	pj
    in
    if is_byte then
      if !is_debug then
	pj.project_has_byte_debug <- true
      else
(* 	pj.project_has_byte <- true *) ()
    else
      if !is_debug then
	pj.project_has_asm_debug <- true
      else
	if !is_profile then
	  pj.project_has_asm_profile <- true
	else
(*	  pj.project_has_asm <- true; *) ();
    load_providing_library pj filename
  in

  let extension_map = StringMap.of_list [
    ".cma", add_project ".cma";
    ".cmxa", add_project ".cmxa";
  ] in

  List.iter (fun (symb, dirname) ->
    Printf.eprintf "Scanning %s...\n%!" dirname;
    BuildScanner.scan_directory_for_extensions dirname extension_map)
    !libdirs;

  let load_project_lib pj filename =
    let obj_desc = BuildObjectInspector.load_object_file filename in
    let requires = ref IntMap.empty in
    let add_require set modname score =
      IntMap.iter (fun _ pj ->
	let (r,score_ref, _) =
	  try
	    IntMap.find pj.project_id !requires
	  with Not_found ->
	    let r = ref StringMap.empty in
	    let score_ref = ref 0 in
	    requires := IntMap.add pj.project_id (r, score_ref, pj) !requires;
	    (r, score_ref, pj)
	in
	score_ref := !score_ref + score;
	r := StringMap.add modname modname !r
      ) set
    in
    begin
      match obj_desc with
	  CMA lib ->
	    (*
	      Printf.eprintf "\t%d modules\n" (List.length lib.cma_units);
	      Printf.eprintf "\tCustom %b\n" lib.cma_custom;
	      Printf.eprintf "\t%d cc objs\n" (List.length lib.cma_ccobjs);
	      Printf.eprintf "\t%d cc opts\n" (List.length lib.cma_ccopts);
	      Printf.eprintf "\t%d dll\n" (List.length lib.cma_dllibs);
	    *)

	    pj.project_auto <- Some filename;
	    pj.project_sources <- List.map (fun unit ->
	      List.iter (fun (cmi, digest) ->
		try
		  let set = Hashtbl.find cmi_providers (cmi, digest) in
		  if not (IntMap.mem pj.project_id !set) then
		  if IntMap.cardinal !set = 1 then
		    add_require !set cmi 1
		  else
		    add_require !set cmi 0
		with Not_found -> ()
	      ) unit.cu_imports;

	      (String.uncapitalize unit.cu_name) ^ ".ml", None, default_options
	    ) lib.cma_units;


	| CMXA lib ->
	  (*
	    Printf.eprintf "\t%d modules\n" (List.length lib.cmxa_units);
	  (*      Printf.eprintf "\tCustom %b\n" lib.cmxa_custom; *)
	    Printf.eprintf "\t%d cc objs\n" (List.length lib.cmxa_ccobjs);
	    Printf.eprintf "\t%d cc opts\n" (List.length lib.cmxa_ccopts);
	  (*      Printf.eprintf "\t%d dll\n" (List.length lib.cmxa_dllibs); *)
	  *)
	  pj.project_auto <- Some filename;
	  pj.project_sources <- List.map (fun (unit,_) ->

	      List.iter (fun (cmx, digest) ->
		try
		  let set = Hashtbl.find cmx_providers (cmx, digest) in
		  if not (IntMap.mem pj.project_id !set) then
		  if IntMap.cardinal !set = 1 then
		    add_require !set cmx 1
		  else
		    add_require !set cmx 0
		with Not_found -> ()
	      ) unit.ui_imports_cmx;

	    (String.uncapitalize unit.ui_name) ^ ".ml", None, default_options
	  ) lib.cmxa_units

	| _ -> assert false
    end;

    IntMap.iter (fun _ (modules, score, pj2) ->
      let dep = {
	  dep_for = [];
	  dep_project = pj2;
	  dep_link = (!score > 0);
	} in
	pj.project_requires <-  dep :: pj.project_requires;
      StringMap.iter (fun _ modname ->
	dep.dep_for <- modname :: dep.dep_for
      ) !modules;
    ) !requires;

  in
  let analyse_project pj =
    if pj.project_options.options_has_asm then
      let filename = Filename.concat pj.project_dirname (pj.project_name ^ ".cmxa") in
      load_project_lib pj filename
    else
      if pj.project_options.options_has_byte then
	let filename = Filename.concat pj.project_dirname (pj.project_name ^ ".cma") in
	load_project_lib pj filename

  in
  List.iter analyse_project !new_projects;

  output_projects_by_name target_dirname;
*)
  ()


let find_installed target_dirname =
  let oc = open_out (Filename.concat target_dirname "ocp-installed.ocp") in
  Printf.fprintf oc "installed = [ \n";

  let add_file filename =
    Printf.fprintf oc "   \"%s\"\n" filename
  in
  let extension_map = StringMap.of_list [
    ".cmo", add_file;
    ".cmx", add_file;
    ".cmi", add_file;
  ] in

  List.iter (fun (symb, dirname) ->
    Printf.eprintf "Scanning %s...\n%!" dirname;
    BuildScanner.scan_directory_for_extensions dirname extension_map)
    !libdirs;

  Printf.fprintf oc "]\n";
  close_out oc

type file = {
  file_node : Toposort.node;
  file_filename : string;
  file_modname : string;
  mutable file_internal_deps : file list;
  mutable file_external_deps :  StringSet.t;
}


module FileSorter = Toposort.Make(struct
  type t = file
  let node file = file.file_node
  let iter_deps f file = List.iter f file.file_internal_deps
end)

let get_module_deps options filename =
  let (status, lines) = get_stdout_lines (strings_option options ocamldep_cmd) ["-modules"; filename] in
  match lines with
    | [line] ->
      let (_, after) = String.cut_at line ':' in
      String.split after ' '
    | _ -> []

let get_module_deps options filename =
  let deps = get_module_deps options filename in
  Printf.eprintf "DEPS %s\n" filename;
  List.iter (fun dep ->
    Printf.eprintf "\t%s\n" dep
  ) deps;
  deps

let autogen target_dirname target =

(*
  let nerrors = ref 0 in
  List.iter (fun dirname ->
    BuildScanner.scan_directory_for_suffix dirname ".ocp" (fun filename ->
      try
	InstalledOCPParser.read_ocamlconf filename
      with BuildMisc.ParseError -> incr nerrors);
  ) [
    global_config_dir;
    local_config_dir;
    "."
  ];
  if !nerrors > 0 then exit 2;

  let internal_modules = ref StringMap.empty in
  let files = Sys.readdir target_dirname in
  Array.iter (fun file ->
    if Filename.check_suffix file ".ml" then
      let filename = Filename.concat target_dirname file in
      let modname = String.capitalize (Filename.chop_suffix file ".ml") in
      let deps_ml = get_module_deps filename in
      let deps_mli =
	let filename_mli = filename ^ "i" in
	if Sys.file_exists filename_mli then
	  let deps_mli = get_module_deps filename_mli in
	  deps_mli
	else [] in
      let file = {
	file_node = Toposort.new_node ();
	file_filename = filename;
	file_modname =  modname;
	file_internal_deps = [];
	file_external_deps = StringSet.empty;
      } in
      List.iter (fun modname ->
	file.file_external_deps <- StringSet.add modname file.file_external_deps
      ) (deps_ml @ deps_mli);
      internal_modules := StringMap.add file.file_modname file !internal_modules
  ) files;

  let all_files = ref [] in
  let internal_modules = !internal_modules in
  StringMap.iter (fun _ file ->
    all_files := file :: !all_files;
    StringSet.iter (fun modname ->
      try
	let file2 = StringMap.find modname internal_modules in
	file.file_external_deps <- StringSet.remove modname file.file_external_deps;
	file.file_internal_deps <- file2 :: file.file_internal_deps
      with Not_found -> ()
    ) file.file_external_deps;
  ) internal_modules;

  let files = FileSorter.sort !all_files in

  let pj = BuildGlobals.new_project
    target target_dirname "" in

  pj.project_sources <- List.map (fun file ->
    Filename.basename file.file_filename, None, default_options
  ) files;

  let filename = Filename.concat target_dirname "build.ocp" in
  let oc = open_out filename in
  BuildOCFPrint.output_project oc pj;
  close_out oc
*)
  ()
(* TODO: find external dependencies *)
(* TODO: topological sort of modules *)
(* TODO: dump 'build.ocp' file *)



*)
