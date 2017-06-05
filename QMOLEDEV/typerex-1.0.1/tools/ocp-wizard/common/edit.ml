(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                           Tiphaine Turpin                              *)
(*                                                                        *)
(*  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           *)
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
open Util

let buf_len = 1024
let buf = String.create buf_len

(* Check that replaces is an ordered list of non overlapping
   replacements between start and end_p. *)
let rec check start end_p = function
  | [] -> ()
  | (b, e, _) :: replaces ->
    if start <= b && e <= end_p then
      check e end_p replaces
    else
      invalid_arg "replacement lists is not valid"

let rec copy_until_end ic oc =
  let n = input ic buf 0 buf_len in
  output oc buf 0 n;
  if n > 0 then
    copy_until_end ic oc

let rec copy len ic oc =
  if len > 0 then
    let n = input ic buf 0 (min len buf_len) in
    output oc buf 0 n;
    if n = 0 then
      invalid_arg "Edit.copy: end of file reached"
    else
      copy (len - n) ic oc

let rec replace ic oc = function
  | [] -> copy_until_end ic oc
  | (b, e, s) :: l ->
    copy (b - pos_in ic) ic oc;
    output_string oc s;
    seek_in ic e;
    replace ic oc l

let rec check_original ic = function
  | [] -> ()
  | (b, e, check) :: l ->
    seek_in ic b;
    let count = e - b in
    let s = String.create count in
    really_input ic s 0 count;
    check s;
    check_original ic l

let cp ?(overwrite=false) file new_file =
  if not overwrite && Sys.file_exists new_file then
    invalid_arg "Edit.cp : target file exists"
  else
    let ic = open_in_bin file in
    let oc = open_out_bin new_file in
    copy_until_end ic oc;
    close_out oc;
    close_in ic

let mv ?(overwrite=false) file new_file =
  cp ~overwrite file new_file;
  Sys.remove file

let prepare_edit eds file =
  let ic = open_in_bin file in
  let new_file, oc =
    Filename.open_temp_file ~mode:[Open_binary]
      ~temp_dir:(Filename.dirname file) (Filename.basename file) ".edited"
  in
  replace ic oc eds;
  close_out oc;
  close_in ic;
  new_file

let check_original eds file =
  let ic = open_in_bin file in
  check_original ic eds;
  close_in ic

let check_exists file =
  if not (Sys.file_exists file) then
    failwith ("File " ^ file ^ " does not exists");
  if Sys.is_directory file then
    failwith ("File " ^ file ^ " is a directory")

let edit eds file =
  check_exists file;
  check 0 (Unix.stat file).Unix.st_size eds;
  let new_file = prepare_edit eds file in
  Sys.rename new_file file

(* Check that files are renamed or edited at most once (with valid
   ranges), and that targets and sources are disjoint, and checks read
   files exists and that created file do not.

   This only checks the consistency and applicability of the edits ;
   further checks are done below to avoid writing files for which
   auto-saves exist. *)
let check_edits edits renames =
  let origins = ref [] in
  let check_origin f =
    if List.mem f !origins then
      invalid "File %s appears multiple times in edit" f;
    origins := f :: !origins;
    check_exists f
  in
  let targets = ref [] in
  let check_target f =
    if List.mem f !targets then
      invalid "File %s appears multiple times in edit" f;
    targets := f :: !targets;
    if Sys.file_exists f then
      invalid "File %s already exists" f
  in
  List.iter
    (function f, eds ->
      check_origin f;
      check 0 (Unix.stat f).Unix.st_size eds)
    edits;
  List.iter
    (function f, f' ->
      check_origin f;
      check_target f')
    renames;
  List.iter
    (function f ->
      (* We could allow this if the destination file is also renamed,
         but let's keep it simple. *)
      if List.mem f !origins then
        invalid "file %s is both a source and a target of editing" f)
    !targets

(* Checking that renaming origins
   and targets do not have no auto-save, or that we will remove
   these auto-saves.

   This assumes that all files exists (which is always the case)
   because either we just created them, or checked their existence. *)
let check_renames_for_autosave removes all_renames =
  List.iter
    (function file, dest ->
      let ensure_no_auto_save file =
	if ProgramCache.has_auto_save file &&
	  not (List.mem (ProgramCache.check_auto_save file) removes)
	then
	  fail_owz "%s" ("File " ^ file ^ " has an auto-save")
      in
      ensure_no_auto_save file;
      ensure_no_auto_save dest)
    all_renames

(* Performs a list of file renaming and removals (which are assumed to
   not affect the same files), after *)
let perform_renames removes all_renames =
  try
    List.iter
      (function f, f' -> Sys.rename f f')
      all_renames;
    List.iter Sys.remove removes
  with e ->
    failwith "Serious error: an exception occured during file renaming"

(* for each edit or rename we store:
   - the new filename
   - the contents of the file just before the edit
   - the hash of its contents just after the edit
   - either `renamed oldname or `edited *)
type undo_stack = frame Stack.t

and frame = {
  name : string; (* A name for the edit action *)
  changes : (string * string * Digest.t * [`renamed of string | `edited]) list
}

let empty_undo = Stack.create

let check_undo stack =
  let {name = name ; changes = changes} = Stack.top stack in
  name,
  List.filter_map
    (function file, _, hash, origin ->
      (match origin with
	| `renamed from ->
	    if Sys.file_exists from then
	      fail_owz "cannot undo the renaming of %s: file already exists" from
	    else if ProgramCache.has_auto_save from then
	      fail_owz "cannot undo the renaming of %s: file has an auto-save" from
	| _ -> ());
      if Sys.file_exists file &&
	(Digest.file file = hash && not (ProgramCache.has_auto_save file)) then
	None
      else
	Some file)
    changes

let undo_last stack =
  let {name = name; changes = changes} = Stack.top stack in
  let all_renames =
    List.map
      (function file, contents, _, origin ->
	let restored_file, oc =
	  Filename.open_temp_file ~mode:[Open_binary]
	    ~temp_dir:(Filename.dirname file)
	    (Filename.basename file) ".restored"
	in
	output_string oc contents;
	close_out oc;
	let rename_to =
	  match origin with
	    | `renamed old_file -> old_file
	    | `edited -> file
	in
	restored_file, rename_to)
      changes
  and removes =
    List.filter_map
      (function file, _, _, _ -> ProgramCache.auto_save file)
      changes @
    List.filter_map
      (function file, _, _, `renamed _ -> Some file | _ -> None)
      changes
  in
  check_renames_for_autosave removes all_renames;
  perform_renames removes all_renames;
  ignore (Stack.pop stack);
  List.map
    (function file, _, _, origin -> file, origin)
    changes

let edit_files stack name edits renames =
  check_edits edits renames;
  List.iter
    (function _, rename_to ->
      if Sys.file_exists rename_to then
	fail_owz "cannot rename into %s: file already exists" rename_to)
    renames;
  check_renames_for_autosave []
    (List.map
       (function f, _ -> f, f)
       edits @
     renames);
  let edit_renames =
    List.map
      (function file, eds -> prepare_edit eds file, file)
      edits
  in
  let undo_renames =
    List.map
      (function from, rename_to ->
	rename_to, File.string_of_file from, Digest.file from, `renamed from)
      renames
  and undo_edits =
    List.map
      (function from, rename_to ->
	rename_to, File.string_of_file rename_to, Digest.file from, `edited)
      edit_renames
  in
  perform_renames [] (edit_renames @ renames);
  Stack.push {name = name; changes = undo_edits @ undo_renames} stack
