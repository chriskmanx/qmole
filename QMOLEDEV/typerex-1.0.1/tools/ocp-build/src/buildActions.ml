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

open BuildGlobals
open BuildOCPTypes
open BuildTypes
open BuildEngineTypes
open BuildEngineGlobals

let deleted_files = ref []

let delete_file filename =
  deleted_files := filename :: !deleted_files;
  if not !fake_arg then
    Unix.unlink filename

let rec delete_file_or_directory filename =
  begin try
	  let files = Sys.readdir filename in
	  Array.iter (fun file ->
	    delete_file_or_directory (Filename.concat filename file)) files
    with _ -> ()
  end;
  begin try
	  delete_file filename
    with _ -> try
		Unix.rmdir filename
      with _ -> ()
  end

let init_deleted () = deleted_files := []

let print_deleted () =
  if !verbosity > 0 then begin
    Printf.eprintf "Deleted files%s:"
      (if !fake_arg then "(faked)" else "");
    List.iter (fun filename ->
      Printf.eprintf " %s" filename
    ) !deleted_files;
    Printf.eprintf "\nTotal: %d files\n%!" (List.length !deleted_files);
    deleted_files := [];
  end

(* [do_clean ()] remove all files that appear
  as the result of a generating rule, with an
   OCaml extension.
*)
let do_clean b =
  init_deleted ();
  Hashtbl.iter (fun _ file ->
    match file.file_kind with
	FILE_VIRTUAL -> ()
      | FILE_REAL | FILE_TEMPORARY ->
	    let filename = file_filename file in
	    match file.file_target_of with
	[] -> ()
      |  _ ->

	let _, ext = File.cut_last_extension file.file_basename in
	Printf.eprintf "Testing generated file %s[%s]\n" filename ext;

	match ext with
	    "o" | "a" | "cmi" | "cmo" | "cmx" | "cmxa"
	  | "cma" | "byte" | "opt" ->

	    if Sys.file_exists filename then begin
	      Printf.eprintf "\tscheduling for deletion\n";
	      delete_file filename
	    end else
	      Printf.eprintf "\tfile does not exist\n";
	  | _ -> ()
  ) b.build_files;
  print_deleted ()


let clean_map =
  StringMap.of_list (List.map (fun x ->
  x, delete_file) [
  (* Generated files *)
  "o"; "cmo"; "cmi"; "cmx"; "cma"; "cmxa"; "annot"; "a";
    "byte"; "asm"; "opt";
    "mldep"; "mlidep";
   (* Modified source files *)
  "ml~"; "mli~"; "mll~"; "mly~"; "ocp~";
  ])

let do_distclean () =
  init_deleted ();
  delete_file_or_directory "_obuild";
  let map = ref clean_map in
  List.iter (fun (x,y) ->
    map := StringMap.add x y !map
  ) [
    ".mll", (fun filename ->
      let kernel_name = Filename.chop_suffix filename ".mll" in
      delete_file (kernel_name ^ ".ml"));
    ".mly", (fun filename ->
      let kernel_name = Filename.chop_suffix filename ".mly" in
      delete_file (kernel_name ^ ".ml");
      delete_file (kernel_name ^ ".mli");
    );
  ];
  BuildScanner.scan_directory_for_extensions "." !map;
  print_deleted ();
  ()

let list_of_ocp_files_filename = "ocp-build.root"

let do_scan pj =
  BuildOCP.scan_project pj

let do_load_ocp_files pj =
  let nerrors = BuildOCP.load_packages pj in
  if nerrors > 0 then exit 2;
  ()
