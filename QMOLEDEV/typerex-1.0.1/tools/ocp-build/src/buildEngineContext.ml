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
open BuildEngineGlobals
(*open BuildGlobals *)
(*open BuildConfig *)

(* TODO do this somewhere
let _ =
*)

let find_directory b dirname =
  let st = Unix.lstat dirname in
  let key = (st.Unix.st_dev, st.Unix.st_ino) in
  Hashtbl.find b.build_directories key

let rec add_directory b filename =
  let st = Unix.lstat filename in
  let key = (st.Unix.st_dev, st.Unix.st_ino) in
  try
    Hashtbl.find b.build_directories key
  with Not_found ->
    let dir =
      let dirname = Filename.dirname filename in
      match st.Unix.st_kind with
	  Unix.S_LNK ->
	    let link = Unix.readlink filename in
	    let filename =
	      if Filename.is_relative link then
		Filename.concat dirname link
	      else link
	    in
	    add_directory b filename
	| Unix.S_DIR -> begin
	  let basename = Filename.basename filename in
	  if dirname = filename then
	    let rec dir = {
	      dir_key = key;
	      dir_id = new_dir_id b;
	      dir_basename = basename;
	      dir_parent = dir;
              dir_file = File.of_string "/";
	      dir_files = StringMap.empty;
	      dir_dirs = StringMap.empty;
	      dir_fullname = filename;
	    } in
	    dir
	  else
	    let parent_dir = add_directory b dirname in
	    match basename with
		"." -> parent_dir
	      | ".." -> parent_dir.dir_parent
	      | _ ->
		  (* educated guess *)
		let dirname = parent_dir.dir_fullname in
		let basename =
		  try
		    let st2 = Unix.lstat (Filename.concat dirname basename) in
		    if key = (st2.Unix.st_dev, st.Unix.st_ino) then
		      basename
		    else raise Not_found
		  with _ ->
		    let files = Sys.readdir dirname in
		    let nfiles = Array.length files in
		    let rec iter i =
		      assert (i < nfiles);
		      let file = files.(i) in
		      let st2 = Unix.lstat (Filename.concat dirname file) in
		      if key = (st2.Unix.st_dev, st.Unix.st_ino) then
			file
		      else
			iter (i+1)
		    in
		    iter 0
		in
		let dir =
		  try
		    StringMap.find basename parent_dir.dir_dirs
		  with Not_found ->
		    let dir = {
		      dir_basename = basename;
		      dir_parent = parent_dir;
                      dir_file = File.add_basename parent_dir.dir_file basename;
		      dir_key = key;
		      dir_id = new_dir_id b;
		      dir_files = StringMap.empty;
		      dir_dirs = StringMap.empty;
		      dir_fullname = Filename.concat parent_dir.dir_fullname basename;
		    } in
		    parent_dir.dir_dirs <- StringMap.add basename dir parent_dir.dir_dirs;
		    dir
		in
		dir
	end
	| _ -> assert false
    in
    Hashtbl.add b.build_directories key dir;
    dir

let find_dir dir basename =
  StringMap.find basename dir.dir_dirs

let find_file dir basename =
  StringMap.find basename dir.dir_files


(* let build_dir = add_directory b build_dir_filename *)

let add_any_file b dir basename file_kind =
  try find_file dir basename with Not_found ->
    let file = {
      file_id = new_file_id b;
      file_kind = file_kind;
      file_basename = basename;
      file_dir = dir;
      file_file = File.add_basename dir.dir_file basename;
      file_exists = false; (* shall we do that now ? *)
      file_mtime = BuildEngineMtime.zero;
      file_target_of = [];
      file_source_for = [];
    } in
    dir.dir_files <- StringMap.add basename file dir.dir_files;
    Hashtbl.add b.build_files file.file_id file;
    file

let add_virtual_file b dir basename =
  add_any_file b dir basename FILE_VIRTUAL

let add_temp_file b dir basename =
  add_any_file b dir basename FILE_TEMPORARY

let add_file b dir basename =
  add_any_file b dir basename FILE_REAL

let add_filename b dir filename =
  let dirname = Filename.dirname filename in
  let basename = Filename.basename filename in
  let dir = if dirname = "." || dirname = "" then dir else
      add_directory b (Filename.concat dir.dir_fullname dirname)
  in
  add_file b dir basename


let create current_dir_filename build_dir_filename =
  BuildMisc.safe_mkdir build_dir_filename;

  let (build_rules : (int, build_rule) Hashtbl.t) = Hashtbl.create 1111 in
  let (build_files : (int, build_file) Hashtbl.t) = Hashtbl.create 1111 in
  let (build_directories : (int * int,   build_directory) Hashtbl.t) = Hashtbl.create 1111 in

  let build_cache_content = ref 0 in
  let build_cache = ref DigestMap.empty in
  let build_cache_filename = Filename.concat build_dir_filename "cache.cmd" in
  begin
    match try
	    let ic = open_in build_cache_filename in
	    Some ic
      with e -> None
    with
	None -> ()
      | Some ic ->
	try
	  while true do
	    let line = input_line ic in
	    if String.length line > 0 then
	    match line.[0] with
		'#' -> ()
	      | _ ->
	      let targets, command = String.cut_at line ' ' in
	      let targets = Digest.of_hex targets in
	      let command = Digest.of_hex command in
	      incr build_cache_content;
	      build_cache := DigestMap.add targets command !build_cache;
	  done
	with End_of_file -> close_in ic
  end;
  let build_cache_log = open_out (build_cache_filename ^ ".log") in
  let build_log = open_out
    (Filename.concat build_dir_filename "build.log") in
  Printf.eprintf "Cache: %d digests loaded\n" !build_cache_content;
  let b =
    {
      build_directories;
      build_files;
      build_rules;
      build_next_dir_id = 0;
      build_next_file_id = 0;
      build_next_rule_id = 0;
      build_next_process_id = 0;

      build_dir_filename = build_dir_filename;                   (* "/..../_obuild" *)
      build_dir_basename = Filename.basename build_dir_filename; (* "_obuild" *)
      build_dir = File.of_string build_dir_filename;

      build_log = build_log;

      build_cache_input = !build_cache;
      build_cache_entries = IntMap.empty;
      build_cache_filename = build_cache_filename;
      build_cache_log = build_cache_log;

      verbosity_arg = 0;
      cross_arg = None;
      stop_on_error_arg = true;
    }

  in
  let dir = add_directory b current_dir_filename in
  if b.verbosity_arg > 3 then Printf.eprintf "Current directory: %s\n" dir.dir_fullname;
  dir.dir_fullname <- ".";
  dir.dir_file <- File.of_string ".";
  let dir2 = add_directory b current_dir_filename in
  assert (dir == dir2);
  b
