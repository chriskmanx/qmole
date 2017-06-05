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

let nopervasives = BuildOCPTypes.new_bool_option "nopervasives" false
let nodeps_option = BuildOCPTypes.new_strings_option "nodeps" []

let b = Buffer.create 1000

let parse_dependencies s =
  let len = String.length s in
  let dependencies = ref [] in

  let rec parse_name pos pos0 =
(*    Printf.eprintf "parse_name %d %d\n%!" pos pos0; *)
    if pos+1 < len && s.[pos] = ':' && s.[pos+1] = ' ' then
      let target = String.sub s pos0 (pos-pos0) in
      skip_spaces target [] (pos+1)
    else
      if pos+1 < len && s.[pos] = ':' && s.[pos+1] = '\n' then begin
	let target = String.sub s pos0 (pos-pos0) in
	dependencies := (target, []) :: !dependencies;
	parse_name (pos+2) (pos+2)
      end else
	if pos = len then
	  !dependencies
	else
	  parse_name (pos+1) pos0

  and skip_spaces target deps pos =
(*    Printf.eprintf "skip_spaces %d\n%!" pos; *)
    if pos = len then
      (target, deps) :: !dependencies
    else
      match s.[pos] with
	  '\\' when pos+1 < len && s.[pos+1] = '\n' ->
	    skip_spaces target deps (pos+2)
	| ' ' ->
	  skip_spaces target deps (pos+1)
	| '\n' ->
	  dependencies := (target, deps) :: !dependencies;
	  parse_name (pos+1) (pos+1)
	| '\\' when pos + 1 < len && s.[pos+1] = ' ' ->
	  Buffer.clear b;
	  Buffer.add_char b ' ';
	  parse_dependency target deps (pos+2)
	| c ->
	  Buffer.clear b;
	  Buffer.add_char b c;
	  parse_dependency target deps (pos+1)

  and parse_dependency target deps pos =
(*    Printf.eprintf "parse_dependency %d\n%!" pos; *)
    if pos = len then
      (target, (Buffer.contents b) :: deps) :: !dependencies
    else
      match s.[pos] with
	| '\\' when pos + 1 < len && s.[pos+1] = ' ' ->
	  Buffer.add_char b ' ';
	  parse_dependency target deps (pos+2)
	| ' ' ->
	  skip_spaces target ((Buffer.contents b) :: deps) (pos+1)
	| '\n' ->
	  dependencies := (target, (Buffer.contents b) :: deps) :: !dependencies;
	  parse_name (pos+1) (pos+1)
	| c ->
	  Buffer.add_char b c;
	  parse_dependency target deps (pos+1)
  in
  parse_name 0 0


	    (* ocamldep generates dependencies towards .cmo files for
	       .cmi files, even in the case where we are only
	       interested in .cmx files !  Problem: if we add two
	       dependencies, one to .cmo and one to .cmx, then
	       rebuilding any of them will trigger regenerating the
	       .cmi, while in fact, only rebuilding both should
	       trigger the rebuilding.

	       In a general case, what should we do if there is a
	       dependency towards a bytecode file (in particular for
	       camlp4) when specifying native building ?

	       In fact, probably, we want to add the first active
	       dependency among a set of dependencies. So, the
	       dependency would not be a filename by a list of
	       filenames.
	    *)

let expanse_dependencies list =
  List.map (fun (target, deps) ->
    if Filename.check_suffix target  ".cmi" then
      (target, List.map (fun dep ->

	if Filename.check_suffix target ".cmo" then
	  let cmx = String.copy dep in
	  cmx.[String.length cmx - 1 ] <- 'x';
	  [ dep; cmx ]
	else
	  [dep]
       ) deps)
    else
      (target, List.map (fun dep -> [dep]) deps)
  ) list

(* load_dependencies: the old way, i.e. path to files in the load_path *)

let load_dependencies filename =
  Buffer.clear b;
  let ic = open_in filename in
  begin
    try
      while true do
	let line = input_line ic in
	Printf.bprintf b "%s\n%!" line
      done
    with End_of_file -> ()
  end;
  close_in ic;
  expanse_dependencies (
    parse_dependencies (Buffer.contents b))


(* Another solution:

Use ocamldep -modules toto.ml

When reading, we must keep track of what project this file belongs to.
Then, we can infer from which projects the dependencies are
*)

open BuildEngineTypes
open BuildTypes
open BuildGlobals
open BuildOCPTypes

let print_dependencies deps =
  List.iter (fun (dep, deps) ->
    Printf.eprintf "%s: " dep;
    List.iter (fun list ->
      match list with
	  [] -> ()
	| [ x ] -> Printf.eprintf "%s " x
	| [ x; y ] -> Printf.eprintf " (%s|%s) " x y
	| _ -> assert false
    ) deps;
    Printf.eprintf "\n%!";
  ) deps


let load_ocamldep_modules filename =
  let ic = open_in filename in
  let s = input_line ic in
  close_in ic;
  let (source, modules) = String.cut_at s ':' in
  let modules = String.split modules ' ' in
  source, modules

(* With packing, tracking dependencies is trickier. From a given module,
we can only access internal modules of the current project, and external
modules of the other projects.
*)

let modname_of_file options filename =
  let filename = Filename.basename filename in
  let is_ml =
    Filename.check_suffix filename ".ml"
    || bool_option_true options ml_file_option
  in
  let basename = Filename.chop_extension filename in
  let modname = String.capitalize basename in
  is_ml, modname, basename

let load_modules_dependencies lib options dst_dir pack_for filename =
  if !verbosity > 2 then
    Printf.eprintf "load_modules_dependencies %s\n" filename;
  let source, modules = load_ocamldep_modules filename in
  let modules =
    if bool_option_true options nopervasives then modules
    else "Pervasives" :: modules
  in
  let nodeps =
    let nodeps = ref StringSet.empty in
    List.iter (fun modname ->
      nodeps := StringSet.add modname !nodeps)
    (strings_option options nodeps_option);
    !nodeps
  in
  let modules = List.filter (fun modname ->
    not (StringSet.mem modname nodeps)) modules in

  let (is_ml, modname, basename) = modname_of_file options source in
(*  let is_ml = Filename.check_suffix source ".ml" in *)
(*  let full_basename = Filename.chop_extension source in *)
(*  let basename = Filename.basename full_basename in *)
(*  let modname = String.capitalize basename in *)

  let deps = lib.lib_requires in
  let deps = List.map (fun dep ->
    let lib = dep.dep_project in
    (lib.lib_dst_dir, lib.lib_modules)
  ) deps in
  let rec add_internal_deps pack_for deps =
    match pack_for with
	[] ->
	    let deps = (lib.lib_dst_dir, lib.lib_modules) :: deps in
	    deps
      | _ :: tail ->
	let deps = add_internal_deps tail deps in
	let (dst_dir, map) = StringsMap.find pack_for lib.lib_internal_modules in
	let deps = (dst_dir, map) :: deps in
	deps
  in
  let deps = add_internal_deps (List.rev pack_for) deps in

  if !verbosity > 5  then begin
    Printf.eprintf "load_modules_dependencies %s\n" filename;
    List.iter (fun (dst_dir, map) ->
      Printf.eprintf "   IN %s :\n\t" dst_dir.dir_fullname;
      StringMap.iter (fun modname _ ->
	Printf.eprintf "%s " modname
      ) !map;
      Printf.eprintf "\n"
    ) deps;

  end;

  let dependencies =
  if is_ml then

    let cmo_target = Filename.concat dst_dir.dir_fullname (basename ^ ".cmo") in

    let cmo_dependencies = ref [] in
    let rec find_module deps depname =
(*      Printf.eprintf "find_module CMO %s\n" depname; *)
      match deps with
	  [] ->
	    if !verbosity > 2 then
	      Printf.eprintf "Warning: could not solve dependency %s for %s\n" depname filename;
	    ()
	| (dst_dir, lib_modules) :: deps ->
	      try
		let (kind, basename) = StringMap.find depname !lib_modules in
		let dst_dir = dst_dir.dir_fullname in
		let full_basename = Filename.concat dst_dir basename in
		let deps =
		match kind with
		  | ML ->
		    [ full_basename ^ ".cmo" ]
		  | MLI ->
		     [ full_basename ^ ".cmi" ]
		  | MLandMLI ->
		     [ full_basename ^ ".cmi" ]
		in
		cmo_dependencies := deps :: !cmo_dependencies
	      with Not_found ->
		find_module deps depname
    in
    List.iter (find_module deps) modules;

    let cmx_target = Filename.concat dst_dir.dir_fullname (basename ^ ".cmx") in


    let cmx_dependencies = ref [] in
    let rec find_module deps depname =
(*      Printf.eprintf "find_module CMX %s\n" depname; *)
      match deps with
	  [] ->
	    if !verbosity > 2 then
	      Printf.eprintf "Warning: could not solve dependency %s for %s\n" depname filename;
	    ()
	| (dst_dir, lib_modules) :: deps ->
	      try
		let (kind, basename) = StringMap.find depname !lib_modules in
		let src_dir = dst_dir.dir_fullname in
		let full_basename = Filename.concat src_dir basename in
		let deps =
		match kind with
		  | ML ->
		    [ full_basename ^ ".cmx" ]
		  | MLI ->
		     [ full_basename ^ ".cmi" ]
		  | MLandMLI ->
		     [ full_basename ^ ".cmx" ]
		in
		cmx_dependencies := deps :: !cmx_dependencies
	      with Not_found ->
		find_module deps depname
    in
    List.iter (find_module deps) modules;
    [
      cmo_target, !cmo_dependencies;
      cmx_target, !cmx_dependencies;
    ]
  else
    let cmi_target = Filename.concat dst_dir.dir_fullname (basename ^ ".cmi") in
    let dependencies = ref [] in
    let rec find_module deps depname =
(*      Printf.eprintf "find_module CMI %s\n" depname; *)
      match deps with
	  [] ->
	    if !verbosity > 2 then
	      Printf.eprintf "Warning: could not solve dependency %s for %s\n" depname filename;
	    ()
	| (dst_dir, lib_modules) :: deps ->
	      try
		let (kind, basename) = StringMap.find depname !lib_modules in
		let dst_dir = dst_dir.dir_fullname in
		let full_basename = Filename.concat dst_dir basename in
		match kind with
		  | ML ->
		      dependencies := [ full_basename ^ ".cmo"; full_basename ^ ".cmx" ] :: !dependencies
		  | MLI ->
		      dependencies := [ full_basename ^ ".cmi" ] :: !dependencies
		  | MLandMLI ->
		      dependencies := [ full_basename ^ ".cmi" ] :: !dependencies

	      with Not_found ->
		find_module deps depname
    in
    List.iter (find_module deps) modules;
    [cmi_target, !dependencies]
  in
  if !verbosity > 2 then
    Printf.eprintf "load_modules_dependencies %s DONE\n" filename;
  if !verbosity > 1 then
    print_dependencies dependencies;
  dependencies


