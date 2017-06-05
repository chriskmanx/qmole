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

open Typedtree
open OcpLang
open Util
open OcpWizard
open Location
open Lexing
open Resolve
include Debug.Tag(struct let tag = "owzUI" end)

let fix_case kind name =
  debugln "checking ident %s" name;
  match name.[0] with
      'a'..'z' | 'A'..'Z' ->
        begin    let name =
      if
        kind = Env_untyped.Module &&
        List.exists
          (Filename.check_suffix name)
          (SimpleProgram.ocaml_source_extensions ())
      then
        Filename.chop_extension name
      else
        name
    in
    let name =
      match kind with
        | Env_untyped.Module | Env_untyped.Modtype | Env_untyped.Constructor ->
          String.capitalize name
        | _ -> String.uncapitalize name
    in
    try
      match parse_lid kind name with
        | Longident.Lident _ -> name
        | _ -> raise Parsing.Parse_error
    with
        Parsing.Parse_error ->
          fail_owz "'%s' is not a valid %s identifier"
            name (Env_untyped.kind2string kind)
        end
    | _ ->
      fail_owz "Cannot rename into an operator"

(* Remove duplicates elements in a *sorted* list. *)
let rec remove_duplicates = function
  | x :: (y :: _ as l) ->
    if x = y then
      remove_duplicates l
    else
      x :: remove_duplicates l
  | l -> l

(** Sort a list of locations by file and order. *)
let sort_locations program locs =
  let t = Hashtbl.create 2 in
  List.iter
    (function loc ->
      let fname = loc.loc_start.pos_fname in
      Hashtbl.add t fname loc)
    locs;
  List.map
    (function f ->
      f,
      remove_duplicates
	(List.sort
	   (fun l l' -> compare l.loc_start.pos_cnum l'.loc_start.pos_cnum)
	   (* This comparison is total because def locations are
	      either disjoint or identical. *)
	   (Hashtbl.find_all t f)))
    (hashtbl_keys t)

let file_renames ?(for_grep=false) ?prefix program fnames =
  List.concat
    (List.map
       (function f ->
(*	 let open Program in *)
	     match Program.find_unit program f with
	       | Program.Concrete unit ->
	         let l =
	           match unit.Program.interface with
		     | Some i -> [Program.source ?prefix program i]
		     | None -> [] in
	         (match unit.Program.implementation with
	           | Some i
                   (* We check if implem and interf have the same source (.mly) *)
		       when not (List.mem (Program.source ?prefix program i) l) ->
		     Program.source ?prefix program i :: l
	           | _ -> l)
	       | Program.Abstract unit ->
                 if for_grep then [Program.abstract_signature ?prefix program unit]
                 else fail_owz "cannot rename abstract module %s" f
	       | Program.Pack unit ->
                 match unit.Program.p_interface with
		   | Some i -> [Program.source ?prefix program i]
		   | None -> [])
       fnames)

let sort_replaces program replaces =
  let locs = List.map fst replaces in
  List.map
    (function f, locs ->
      f,
      List.map
	(function loc ->
	  let rep = List.assoc loc replaces in
	  loc.loc_start.pos_cnum, loc.loc_end.pos_cnum, rep)
	locs)
    (sort_locations program locs)

let rename_module new_name file =
  let newfile =
    Filename.concat
      (Filename.dirname file)
      (String.uncapitalize new_name ^ extension file)
  in
  file, newfile

let check_replaced ?errors file b e old txt =
  try_do ?errors (function () ->
    let ok =
      match old with
        | `lid lid ->
          (try parse_string Parser.any_longident txt = lid
           with _ -> false)
        | `id id -> id = txt
        | `op -> true (* not urgent: works with camlp4 too *)
    and expected =
      match old with
        | `lid lid -> lid2string lid
        | `id id -> id
        | `op -> Printf.sprintf "open ..."
    in
    if not ok then
      fail_owz
        "file %s, characters %d-%d: contents is\n  %S\nbut should be\n  %S"
        file b e txt expected
  )

(* In fact, we could even use the check to widen or narrow the
   locations when appropriate. *)
let replace_in_file ?errors program (file, replaces) =
  let absolute_file = Program.prefix_with ~prefix:`absolute program file in
  Edit.check_original
    (List.map
       (function b, e, (old, _) ->
         b, e, check_replaced ?errors file b e old)
       replaces)
    absolute_file;
  absolute_file,
  List.map
    (function b, e, (_, text) -> b, e, text)
    replaces

let undo_stack =
  ProgramCache.make_cache
  (function program -> program.Program.root)
  (fun _ _ -> ())
  (function _ -> Edit.empty_undo ())

let perform_edit program name edits renames =
  match edits, renames with
    | [file, edits], [] -> Edit.edit edits file
    | _ -> Edit.edit_files (undo_stack program) name edits renames

let singleton = function
  | `lc (l, c) -> `lc ((l, c), (l, c + 1))
  | `cnum cnum -> `cnum (cnum, cnum + 1)

let goto_definition program source_id pos =
    let loc = singleton pos in
    let kind, gid, _, _ =
      try locate_ident_from_def_or_use program source_id loc
      with
        | Not_found -> fail_owz "No ident found here"
(*
        | Unbound (kind, lid) when !catch_errors ->
          fail_owz "%s %s not found in %s"
            (Env_untyped.kind2string kind) (lid2string lid)
            (String.concat " " !Config.load_path)
*)
    in
    debugln "found global ident: %s" (Ident.name_with_ctx gid);
    try
      FindName.id2loc program gid
    with Not_found ->
      fail_owz "%s %s not found"
        (Env_untyped.kind2string kind) (Ident.name_with_ctx gid)

let rec find_other ~first loc = function
  | [] -> assert false
  | [loc'] ->
    if loc = loc' then first else invalid_arg "find_other"
  | loc' :: (loc'' :: _ as locs) ->
    if loc = loc' then loc'' else find_other ~first loc locs

let find_other name loc locs =
  let locs = List.sort compare locs in
  match locs with
    | [] | [_] -> fail_owz "no alternate definition found for %s" name
    | first :: _ as locs -> find_other ~first loc locs

let cycle_definitions program source loc =
  let loc = singleton loc in
  let kind, id, loc =
    try visible_ident_definition `innermost source loc program
    with Not_found -> fail_owz "No ident found here"
  in
  let errors = ref [] in
  let defs = all_defs ~errors kind id program in
  fdebugln "found %d located defs:\n%t" (List.length defs)
    (function c -> List.iter (print c) defs);
  find_other (Ident.name id) loc defs

let comment_definition program source_id loc =
    let source_file = Program.find_source program source_id in
    let _env = ProgramCache.source_env program source_file in
    let loc = singleton loc in
    let kind, id, desc, def_ref =
      try locate_ident_from_def_or_use program source_id loc
      with Not_found -> fail_owz "No ident found here"
    in
    debugln "found global ident: %s" (Ident.name id);
    let comments =
      match id2comments program id with
        | None -> ""
        | Some comments -> "\n\n" ^ comments
    in
    let desc =
      match def_ref with
        | `ref path -> Env_untyped.decl2string path desc
        | `def -> Env_untyped.decl2string (Path.Pident id) desc
    in
    desc ^ comments

let matches program file locs =
  let absolute_file = Program.prefix_with ~prefix:`absolute program file in
  debugln "found occurrences in %s" absolute_file;
  let lines =
    Array.of_list
      (File.lines_of_file (ProgramCache.check_auto_save absolute_file)) in
  List.map
    (function loc ->
      let line =
        try lines.(loc.loc_start.pos_lnum-1)
        with _ -> "ERROR: invalid line number" in
      let n, line =
        let line' = String.lstrip line in
        String.length line - String.length line',
        String.rstrip line'
      in
      let start = max 0 (-n + loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
      and endp =
        min (String.length line) (-n + loc.loc_end.pos_cnum - loc.loc_start.pos_bol)
      in
      loc, start, endp, line)
    locs

let all_matches program (idents, fnames, occs) =
  let all_matches occs =
    List.map
      (function file, locs -> file, matches program file locs)
      (sort_locations program occs)
  in
  all_matches idents,
  all_matches occs,
  file_renames ~for_grep:true program fnames

let ignore_project_file = ref false
let default_cwd = ref true

let modify ~buffername ~filename ~first_time ~start ~old_length data =
  Profile.time_push "receive region";
  let contents = String.concat "\n" data in
  Profile.time_switch_to "find buffer";
  let init () =
    let file = Filename.basename filename
    and dirname = Filename.dirname filename in
    let program =
      lazy (
        let source, program =
          try
            SimpleProgram.program
              ~ignore_absent:true ~ignore_extension:true
              ~ignore_project_file:!ignore_project_file
              ~default_cwd:!default_cwd ~cwd:dirname file
          with _ ->
            SimpleProgram.program
              ~ignore_absent:true ~ignore_extension:true
              ~ignore_project_file:true ~cwd:dirname file
        in
        debugln "found program with root %s" program.Program.root;
        program, source
      ) in
    filename,
    program
  in
  let buffer = OcamlBuffer.find_or_create buffername ~init in
  Profile.time_switch_to "modify region";
  OcamlBuffer.update_buffer buffer ~start ~old_length first_time contents;
  Profile.time_pop ()

let pre_cache_buffer buffername =
  ignore (Lazy.force (OcamlBuffer.find_or_create buffername).OcamlBuffer.program)

let colorize buffer_name =
  let buffer = OcamlBuffer.find_or_create buffer_name in
    match buffer.OcamlBuffer.needs_refontifying with
      | Some (start, end_) ->
        let {OcamlTokenize.OCamlTokenBuffer.chars = chars} = buffer.OcamlBuffer.contents in
        let start_p = GapBuffer.mark2pos chars start
        and end_p = GapBuffer.mark2pos chars end_ in
        GapBuffer.delete_mark chars start;
        GapBuffer.delete_mark chars end_;
        buffer.OcamlBuffer.needs_refontifying <- None;
        debugln "colorizing tokens [%d, %d[" start_p end_p;
        let range, faces, helps =
          Profile.time_call "colors"
            (Colorize.colors buffer.OcamlBuffer.contents start_p) end_p
        in
        range, faces, helps, (start_p, end_p)
      | None -> (0, 0), [], [], (0, 0)

let completion buffername pos =
(*  let open OcamlBuffer in *)
  let prefix, candidates =
    try
      let buffer = Hashtbl.find OcamlBuffer.buffers buffername in
      let program, source_id = Lazy.force buffer.OcamlBuffer.program in
      let prefix, candidates =
        Completion.completions
          program source_id buffer.OcamlBuffer.contents buffer.OcamlBuffer.local_envs pos in
      buffer.OcamlBuffer.last_completion <- candidates;
      prefix, candidates
    with _ when !catch_errors -> "", []
  in
  prefix,
  List.map
    (function kind, name, doc -> Env_untyped.kind2char kind, name)
    candidates

let last_completion_doc buffername ~candidate =
  let buffer = OcamlBuffer.find_or_create buffername in
  let _, _, doc =
    List.find (function _, name, _ -> name = candidate)
      buffer.OcamlBuffer.last_completion
  in
  Lazy.force doc

let prune_lids ~errors program source_id =
  let source_file = Program.find_source program source_id in
  let _env = ProgramCache.source_env program source_file in
  let replaces = prune program source_file in
  match sort_replaces program replaces with
    | [] -> fail_owz (*Printf.fprintf oc*) "All references are already minimal"
    | [file, replaces] -> replace_in_file ~errors program (file, replaces)
    | _ -> assert false

let eliminate_open ~errors program source_id loc =
  let source_file = Program.find_source program source_id in
  let fname = source_file.Program.source in
  let _env = ProgramCache.source_env program source_file in
  let r = ProgramCache.last_cnum2old_lc program source_file loc in
  debugln "Locating open at (%d, %d)" (fst r) (snd r);
  let r = r, r in
  let loc, typedtree_and_open =
    let typedtree =
      (ProgramCache.typedtree program source_file :> TypedtreeOps.node) in
    try
      TypedtreeLocate.locate_map_item
        (function loc -> function
          | `structure (s, ({str_desc = Tstr_open (p, p_text)} as item)) ->
            Some (loc, `str_open (s, item, p, p_text))
          | `signature (s, ({sig_desc = Tsig_open (p, p_text)} as item)) ->
            Some (loc, `sig_open (s, item, p, p_text))
          | _ -> None)
        (TypedtreeLocate.included_lc r) TypedtreeLocate.loc_included_lc
        typedtree
    with Not_found -> try
      TypedtreeLocate.locate_map `innermost
        (function loc -> function
          | `expression {exp_desc = Texp_open (p, lid, e)} ->
            if TypedtreeLocate.included_lc r e.exp_loc then
              None
            else
              Some (
                {loc with loc_end = e.exp_loc.loc_start}
                  , `exp_open (p, lid, e))
          | _ -> None)
        (TypedtreeLocate.included_lc r) TypedtreeLocate.loc_included_lc
        typedtree
    with Not_found -> fail_owz "no open statement to eliminate here" in
  debugln "Computing replaces";
  let replaces =
    try eliminate_open ~fname typedtree_and_open
    with RenameLid.Cannot_eliminate -> fail_owz "Cannot eliminate this open"
  in
  debugln "OK";
  match sort_replaces program ((loc, (`op, "")) :: replaces) with
    | [file, replaces] -> replace_in_file ~errors program (file, replaces)
    | _ -> assert false

module Make(IDE : IDE_Callback.Callback)(Specifics : IDE_Specifics.T) = struct

  open IDE
  open Specifics

  let append2file f s =
    let flags = [ Open_append ]
    and perm = 0o640 in
    let c = open_out_gen flags perm f in
    output_string c s;
    close_out c

  let ignore_project_file = !ignore_project_file
  let default_cwd = !default_cwd

  (* Collect the "simple program" containing a file, but if
     [interactive] is set, allow the user to fix the project file. *)
  let simple_program
      ?(ignore_absent=false) ?(ignore_extension=false) ?(interactive=true)
      ?cwd file =
    let source, program =
      (*
        try OcpProgram.program file with Not_found ->
      *)
      try
        SimpleProgram.program
          ~ignore_absent:(ignore_absent && not interactive)
          ~ignore_extension ~ignore_project_file ~default_cwd ?cwd file
      with
        | SimpleProgram.FileNotInProgram (pf, dir) when interactive ->
          let subdir =
            match dir with
              | [] -> "."
              | t :: q -> List.fold_left Filename.concat t q
          in
          if
            y_or_n_p
              ("file %s in subdirectory %s is not in project with root %s\n" ^^
                  "append directory %s to %s? ")
              file subdir (Filename.dirname pf) subdir pf
          then (
            append2file pf (subdir ^ "\n");
            SimpleProgram.program
              ~ignore_absent ~ignore_extension ~ignore_project_file ~default_cwd ?cwd file
          ) else
            fail_owz "Quit"
        | SimpleProgram.FileExcluded (pf, f) when interactive ->
          fail_owz "%s is explicitely excluded from project with root %s"
            f (Filename.dirname pf)
    in
    debugln "read a program with %d compilation units (%d source files)"
      (Hashtbl.length program.Program.units)
      (Program.fold_sources (fun _ _ -> succ) program 0);
    debugln "read program:\n%t"
      (function c -> output_string c (Program.string_of_program program));
    (* make sure that the program is made current *)
    (try
       ignore (ProgramCache.source_env program (Program.find_source program source))
     with _ -> ());
    source, program

let saved_program ?interactive ?cwd file check =
  let source, program = simple_program ?interactive ?cwd file in
  let errors = ref [] in
  check ~errors program (Program.find_source program source);
  let unsaved =
    List.filter_map
      (function ProgramCache.Unsaved f -> Some f | _ -> None)
      !errors
  in
  match unsaved with
    | [] -> source, program
    | _ ->
      if y_or_n_p "save %s and proceed? "
        (String.concat ", " (List.map (function f -> f.Program.source) unsaved)) then (
          List.iter
            (function f ->
              save_buffer_visiting (Program.source ~prefix:`absolute program f))
            unsaved;
          simple_program ?interactive ?cwd file
         ) else
        fail_owz "quit"

let callback_test port =
  try
    ()
(*
    highlight_regions
      [Face.highlight_definition, 1, 10 ;
       Face.highlight_reference, 20, 30];
    message "%s" (command_string ")")
*)
  with
    | EmacsCallback.CallbackReadError c -> message "read error: %S" c
    | EmacsCallback.ErrorInCallback e -> message "callback error: %S" e
(*
    | _ -> message "other error"
*)

let ignoring_auto_save f =
  let ignore = !ProgramCache.ignore_auto_save in
  ProgramCache.ignore_auto_save := true;
  try
    let res = f () in
    ProgramCache.ignore_auto_save := ignore;
    res
  with e ->
    ProgramCache.ignore_auto_save := ignore;
    raise e

let program ?check filename =
  debugln "filename=%s" filename;
  let file = Filename.basename filename
  and dirname = Filename.dirname filename in
  match check with
    | Some check -> saved_program ~cwd:dirname file check
    | None -> simple_program ~cwd:dirname file

let with_current_source ?check f =
  let source, program = program ?check (buffer_file_name ()) in
  match check with
    | Some _ -> ignoring_auto_save (function () -> f program source)
    | None -> f program source

(*
let current_pos () = `cnum (point ())
*)
let current_pos () = `lc (line_column_bytes ())

let with_current_loc ?check f =
  with_current_source ?check
    (fun program source -> f program source (current_pos ()))

let locate_ident toplevel program source =
  if toplevel then
    let modname = Program.source2modname source in
    Env_untyped.Module,
    Ident.create_persistent modname
  else
    let loc = let loc = current_pos () in singleton loc in
    let kind, id, _, _ = locate_ident_from_def_or_use program source loc in
    kind, id

let list_errors errors =
  String.concat "\n"
    (List.map
       (function e ->
         Printf.sprintf "  %s" (Exceptions.print_error e))
       (List.setify errors))

let ignore_any_errors action errors =
  if !errors <> [] then
    if
      y_or_n_p
        ("Some exception(s) occured while %s:\n%s\n"
         ^^ "Refactoring might be wrong or partial. Proceed anyway? ")
        action (list_errors !errors)
    then
      errors := []
    else
      fail_owz "Quit"

(* Renaming entry point: user interface... *)
let rename toplevel =
  do_auto_save ();
  (* Read the program *)
  Profile.time_push "read program";
  with_current_source
    ~check:(fun ~errors program _ ->
      ProgramCache.check_for_refactoring ~errors program)
    (fun program (prefix, _ as source) ->

    let errors = ref [] in
    ProgramCache.check_for_refactoring ~errors program;

    (* Get the "initial" id to rename and its sort and location *)
    Profile.time_switch_to "locate longident";

    let renamed_kind, id =
      try locate_ident toplevel program source
      with Not_found -> fail_owz "Cannot rename anything here"
    in

    if List.mem renamed_kind [Env_untyped.Class ; Env_untyped.Cltype] &&
      not
      (y_or_n_p "Renaming of %s is currently incomplete. Proceed anyway? "
         (match renamed_kind with
           | Env_untyped.Class -> "classes"
           | Env_untyped.Cltype -> "class types"
           | _ -> assert false))
    then
      fail_owz "Quit";

    let old_name = Ident.name id in

    let new_name =
      read_from_minibuffer "Rename %s %s with: "
        (Env_untyped.kind2string renamed_kind) old_name in
    let new_name = fix_case renamed_kind new_name in

    Profile.time_switch_to "rename in files";
    let check_errors = !errors in
    errors := [];
    let idents, fnames, occs =
      try
        let res =
          rename
            ~errors ~restrict_propagation:!RenamePropagation.restrict_propagation
	    renamed_kind id new_name program
        in
        check_any !errors
          (function
            | Masked_by _ | AmbiguousOrder _ | RenamePropagation.EscapingRenaming _
                as e -> raise e
            | _ -> ());
        res
      with
        | Masked_by (renamed, id) ->
	  let loc = FindName.id2loc ~accept_none:true program id in
          let loc =
	    if loc = none then
	      "At implicit location (include):\n"
            else
              loc2string loc
          in
	  if renamed then
	    fail_owz
              "%sThis existing definition of %s would capture an occurrence of %s"
              loc new_name old_name
	  else
            fail_owz
	      "%sThis definition of %s that you are trying to rename would \
                 capture an occurrence of an existing definition of %s"
              loc old_name new_name
        | AmbiguousOrder (names, found) ->
          fail_owz
            ("A toplevel module %s already exists, therefore "^^
                "renaming %s would at least make their ordering ambiguous.")
            new_name old_name
        | RenamePropagation.EscapingRenaming _ ->
	  fail_owz "This renaming is not local and propagation was disabled"
    in
    errors := check_errors @
    List.filter
      (function Qualified (_, e) | e ->
        not (List.mem e check_errors))
      !errors;

    ignore_any_errors "computing renaming" errors;

    Profile.time_switch_to "sort_replace";
    let replaces = sort_replaces program (idents @ occs) in
    let fnames = file_renames ~prefix:`absolute program (List.map fst fnames) in

    let n_files = List.length replaces in
    let local = fnames = [] && n_files <= 1 in
    if local ||
      y_or_n_p "Rename %s in %d files? "
      old_name (List.length fnames + n_files)
    then (
      Profile.time_switch_to "apply renamings";
      (* Replace lids in the source file *)
      let renames' = List.map (rename_module new_name) fnames
      and edits = List.map (replace_in_file ~errors program) replaces in
      ignore_any_errors "performing replaces" errors;
      try
        perform_edit program
          (Printf.sprintf "renaming %s into %s" old_name new_name)
          edits renames';
        if local then
          revert_with_history ()
        else (
          List.iter (function file, file' -> rename_file file file') renames';
          List.iter (function file, _ -> revert_buffer_visiting file) edits
        );
        message "Renamed %d definition(s) and %d reference(s) in %d file(s)%s"
          (List.length idents) (List.length occs) (List.length replaces)
          (match fnames with
            | [] -> ""
            | _ -> Printf.sprintf ", and renamed %d source file(s)"
              (List.length fnames));
        Profile.time_pop ()
      with
          e ->
	    message
              "Renaming failed while editing the files !\n%s%s"
              (Printexc.get_backtrace())
	      (Printexc.to_string e)
    ) else
      message "Quit"
  )

let undo_last () =
  with_current_source
    (fun program _ ->
  try
    do_auto_save ();
    let undo_stack = undo_stack program in
    let name, modifs = Edit.check_undo undo_stack in
    if
      y_or_n_p "Undo %s? " name &&
	(modifs = [] ||
	    y_or_n_p
	    "Changes to the following files will be discarded!:\n  %s\nProceed? "
	    (String.concat "\n  " modifs))
    then (
      let changes = Edit.undo_last undo_stack in
      List.iter
	(function file, origin ->
	  match origin with
	    | `renamed file' -> rename_file file file'
	    | `edited -> revert_buffer_visiting file)
	changes;
      message "Undone %s" name
    )
  with Stack.Empty ->
    message "No multiple-file action to undo")

let pos2lc pos = `lc (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)


(*
let loc2region_lc loc =
  `lc (
    (loc.loc_start.pos_lnum, loc.loc_start.pos_bol),
    (loc.loc_end.pos_lnum, loc.loc_end.pos_bol)
  )
*)
(* Grep entry point: user interface... *)
let grep toplevel =
  do_auto_save ();
  Profile.time_push "read program";
  with_current_source (fun program (prefix, _ as source) ->
    Profile.time_switch_to "locate ident";
    let errors = ref [] in
    ProgramCache.check_for_refactoring ~errors program;
    debugln "found %d errors while checking the program" (List.length !errors);
    let kind, id =
      try locate_ident toplevel program source
      with Not_found -> fail_owz "Cannot grep anything here"
    in
    message "Grep %s%s %s"
      (if toplevel then "toplevel " else "")
      (Env_untyped.kind2string kind) (Ident.name id);
    Profile.time_switch_to "grep in files";
    let check_errors = !errors in
    errors := [];
    let result = grep ~errors kind id program in
    errors := check_errors @
    List.filter
      (function Qualified (_, e) | e ->
        not (List.mem e check_errors))
      !errors;
    debugln "found %d errors after grep" (List.length !errors);
    Profile.time_switch_to "print results and wait";
    let source = Program.source program (Program.find_source program source) in
    let result = all_matches program result in
    Profile.time_pop ();
    return_grep_results
      ~root:program.Program.root kind id result ~current:source ~errors:!errors
  )

let jump_to program loc =
  let infile = loc.loc_start.pos_cnum = -1 in
  try
    goto
      (Program.file_of_loc ~prefix:`absolute program loc)
      (if infile then `lc (1, 0) else pos2lc loc.loc_start);
    if not infile then
      highlight_regions
        [Face.highlight_definition, pos2lc loc.loc_start, pos2lc loc.loc_end]
  with Not_found ->
    fail_owz "file %s is not in the program" loc.loc_start.pos_fname

let goto_definition () =
  do_auto_save ();
  with_current_loc (fun program source_id pos ->
    jump_to program (goto_definition program source_id pos)
  )

let cycle_definitions () =
  do_auto_save ();
  with_current_loc (fun program source loc ->
    jump_to program (cycle_definitions program source loc)
  )

let prune_lids () =
  do_auto_save ();
  with_current_source ~check:ProgramCache.check_source_for_refactoring
    (fun program source_id ->
      let source_file = Program.find_source program source_id in
      let errors = ref [] in
      ProgramCache.check_source_for_refactoring ~errors program source_file;
      List.iter
        (function ProgramCache.NoCmt _ as e -> raise e | _ -> ()) !errors;
      ignore_any_errors "checking source file" errors;
      let file, replaces = prune_lids ~errors program source_id in
      ignore_any_errors "performing replaces" errors;
      Edit.edit replaces file;
      revert_with_history ();
      message "Simplified %d references" (List.length replaces)
    )

let eliminate_open () =
  do_auto_save ();
  with_current_loc ~check:ProgramCache.check_source_for_refactoring
    (fun program source_id loc ->
      let source_file = Program.find_source program source_id in
      let errors = ref [] in
      ProgramCache.check_source_for_refactoring ~errors program source_file;
      List.iter (function ProgramCache.NoCmt _ as e -> raise e | _ -> ()) !errors;
      ignore_any_errors "checking source file" errors;
      let file, replaces = eliminate_open ~errors program source_id loc in
      ignore_any_errors "performing replaces" errors;
      Edit.edit replaces file;
      revert_with_history ();
      message "Qualified %d references" (List.length replaces - 1)
    )

let comment_definition () =
  do_auto_save ();
  with_current_loc (fun program source_id loc ->
    display_temp "*definition*" "%s" (comment_definition program source_id loc)
  )

let colorize buffer_name =
  let (b, e), faces, helps, forced = colorize buffer_name in
  Profile.time_call "compute faces for IDE"
    (return_fontifying_data b e faces helps) forced

let completion buffername pos =
  let prefix, candidates = completion buffername pos in
  return_completion_data pos prefix candidates

end














