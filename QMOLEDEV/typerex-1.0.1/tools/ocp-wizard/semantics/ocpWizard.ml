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

open Location
open Lexing
open OcpLang
open Util
open Location
open Typedtree
open RenameLid
open ProgramCache
include Debug.Tag(struct let tag = "ocpWizard" end)

let fst3 (x, _, _) = x

type replaced_contents = [`id of string | `lid of Longident.t]

(* check lids for renaming *)
let check_lids renamed_kind ids new_name lids =
  List.iter
    (function source, _, lid, env, kind ->
      check_lid renamed_kind ids new_name env kind lid)
    lids

(* classify a list of longidents by source file, sort them by
   increasing cnum, and remove duplicates *)
let group_by_source =
  group_by_first
    ~split:(function (source, loc, lid, env, kind) ->
      source, (loc, lid, env, kind))
    ~group:(function l ->
      List.setify
        (List.sort
	   (fun (l, _, _, _) (l', _, _, _) ->
             compare l.loc_start.Lexing.pos_cnum l'.loc_start.Lexing.pos_cnum)
           l))

(** Adapt an lid renaming to handle the case of coincident names,
    which may come from the abbreviated syntax of label bindings.

    TODO: do something about argument labels too ! *)
let lid_replacement loc2lid loc2id source loc kind lid =
  if kind = Env_untyped.Label then
    try
      let lid', _ = Hashtbl.find loc2lid (source, loc, Env_untyped.Value) in
      lid2string lid ^ " = " ^ lid2string lid'
    with Not_found ->
      try
	let id = Hashtbl.find loc2id (source, loc, Env_untyped.Value) in
	lid2string lid ^ " = " ^ Ident.name id
      with Not_found ->
	lid2string lid
  else if kind = Env_untyped.Value then
    try
      let lid', _ = Hashtbl.find loc2lid (source, loc, Env_untyped.Label) in
      lid2string lid' ^ " = " ^ lid2string lid
    with Not_found ->
      lid2string lid
  else
    lid2string lid

let rename_lids ?errors program renamed_kind id name' lids loc2lid loc2id =
  List.concat
    (List.map
       (function source, lids ->
         let source_file = Program.find_source program source in
         let _env = source_env program source_file in
         debugln "renaming lids in %s" (Program.source2string source);
         List.filter_map
           (function loc, old_lid, env, kind ->
             try_default
               ~prefix:(Printf.sprintf "checking %s in %s: "
                          (lid2string old_lid) source_file.Program.source)
               ?errors None (function () ->
             match rename_in_lid renamed_kind id name' env kind source old_lid
             with
	       | Some lid ->
	         let r = lid_replacement loc2lid loc2id source loc kind lid in
	         Some (loc, (`lid old_lid, r))
	       | None -> None)
           )
           lids)
       (List.rev lids))

let rename_lids ?errors program renamed_kind id name' lids loc2lid =
  Profile.time_call "rename lids"
    (rename_lids ?errors program renamed_kind id name' lids loc2lid)

let prune_lids lids  =
  List.filter_map
    (fun (text, old_lid, (env, kind)) ->
      match prune_lid env kind old_lid with
	| Some lid ->
	  let r = lid2string lid in
	  Some (text.loc, (`lid old_lid, r))
	| None -> None)
    (List.rev lids)

let get_ids ?errors name program =
  debugln "getting all id definitions for %s" name;
  Program.fold_sources
    (fun source source_file acc ->
      let filename = source_file.Program.source in
      try_default ?errors ~prefix:(Printf.sprintf "File %s: " filename)
      acc (function () ->
      TypedtreeOps.find_all_map_ident_definitions
	(fun kind id text _ ->
	  if Ident.name id = name then (
            let text =
              { text with loc = Program.correct_fname filename text.loc }
            in
	    debugln "  found %s %s:%s"
	      (Env_untyped.kind2string kind)
	      (Program.source2string source)
	      (Ident.unique_name id);
	    Some (source, kind, id, text)
	  ) else
	    None)
	(typedtree program source_file :> TypedtreeOps.node)
      @ acc))
    program
    []

let rec filter cond = function
  | Longident.Lident x -> cond x
  | Longident.Ldot (lid, x) -> filter cond lid || cond x
  | Longident.Lapply (lid, lid') -> filter cond lid || filter cond lid'

let filter_relevant_names names lid _ =
  let cond n = List.mem n names in
  filter cond lid &&
    not (List.mem (lid2string lid) ["false" ; "()"])

let rename_defs defs loc2lid kind name =
  List.filter_map
    (function
      | `ident_at (f, loc, id) ->
        let r =
	  if kind = Env_untyped.Value then
	    try
	      let lid, _ = Hashtbl.find loc2lid (f, loc, Env_untyped.Label) in
	      lid2string lid ^ " = " ^ name
	    with Not_found ->
	      name
	  else
	    name
        in
        Some (loc, (`id id, r))
      | `source_name _ -> None)
    defs,
  List.filter_map
    (function `source_name f -> Some (f, name) | `ident_at _ -> None)
    defs

let id2comments program id =
  let comments =
    try
      let loc = FindName.id2loc ~accept_none:false program id in
      let pos_fname = Program.file_of_loc ~prefix:`absolute program loc in
      let loc = { loc with
        loc_start = { loc.loc_start with pos_fname = pos_fname };
        loc_end = { loc.loc_end with pos_fname = pos_fname } } in
      fdebugln "get comment in %a" print loc;
      Extract.get_comments_permissive
        ~odoc:false loc
    with _ -> []
  in
  match comments with
    | [] -> None
    | _ ->
      Some
        (String.concat "\n"
           (List.map
              (function _, c ->
	        Extract.pretify_comment c)
              comments))

let prune program source_file =
  let filter = (fun _ _ -> true) in
  let fname = source_file.Program.source in
  let lids =
    FindName.get_lids ~fname
      (typedtree program source_file :> TypedtreeOps.node)
      filter in
  prune_lids lids

let eliminate_open ?fname typedtree_and_item =
  debugln "extracting range";
  let range, path, path_text =
    match typedtree_and_item with
      | `str_open (s, open_item, p, p_text) ->
	let not_this_open item = item.str_loc != open_item.str_loc in
	let after_open =
	  match List.drop_while not_this_open s.str_items
	  with _ :: l -> l | [] -> assert false in
	let not_same_open = function
	  | { str_desc = Tstr_open (p',_) } -> p' <> p
	  | _ -> true
	in
	let range = List.take_while not_same_open after_open in
	`structure { s with str_items = range }, p, p_text
      | `sig_open (s, open_item, p, p_text) ->
	let not_this_open item = item.sig_loc != open_item.sig_loc in
	let after_open =
	  match List.drop_while not_this_open s.sig_items
	  with _ :: l -> l | [] -> assert false in
	let not_same_open = function
	  | { sig_desc = Tsig_open (p',_) } -> p' <> p
	  | _ -> true
	in
	let range = List.take_while not_same_open after_open in
	`signature { s with sig_items = range }, p, p_text
      | `exp_open (p, p_text, e) ->
        `expression e, p, p_text
  in
  debugln "collecting lids in range";
  let lids = FindName.get_lids ?fname range (fun _ _ -> true) in
  debug "computing replaces ";
  let replaces =
    List.filter_map
      (fun (text, old_lid, (env, kind)) ->
	match eliminate_open path path_text.txt env kind old_lid with
	  | Some lid ->
	    let r = lid2string lid in
	    Some (text.loc, (`lid old_lid, r))
	  | None -> None)
      (List.rev lids)
  in
  debugln "OK";
  replaces

(* Rename an ident in a list of source files. *)
let rename
    ?errors ?(restrict_propagation = false) renamed_kind id new_name program =

  let old_name = Ident.name id in

  Profile.time_push "constraints";
  (* Collect constraints requiring simultaneous renaming and deduce
     the minimal set of ids to rename *)
  let all_constraints, mli_constraints, includes, redefs =
    RenamePropagation.constrain_all_files ?errors renamed_kind old_name program in

  Profile.time_switch_to "propagate";
  let mli_constraints =
    if restrict_propagation then Some mli_constraints else None in
  let ids, implicit_refs =
    RenamePropagation.propagate
      renamed_kind id all_constraints ?mli_constraints includes redefs in

  debugln "found %d idents to rename" (List.length ids);

  Profile.time_switch_to "check other implicit";
  (* Check that our new name will not capture useful signature members *)
  RenamePropagation.check_other_implicit_references
    renamed_kind ids new_name all_constraints includes;

  Profile.time_switch_to "check renamed implicit";
  (* Check that useful renamed signature members are not masked. *)
  RenamePropagation.check_renamed_implicit_references
    renamed_kind ids new_name implicit_refs;

  Profile.time_switch_to "collect lids";
  (* Collect all lids *)
  let lids = FindName.collect_lids_in_program ?errors
    (filter_relevant_names [old_name; new_name]) program in

  (* Collect all ids defs with the renamed name *)
  let all_ids = get_ids ?errors old_name program in

  let loc2lid = Hashtbl.create 10
  and loc2id = Hashtbl.create 10 in
  List.iter
    (function source, loc, lid, env, kind ->
      Hashtbl.add loc2lid (source, loc, kind) (lid, env))
    lids;
  List.iter
    (function source, kind, id, text ->
      Hashtbl.add loc2id (source, text.loc, kind) id)
    all_ids;

  Profile.time_switch_to "check other lids";
  (* Check that our new name will not capture other occurrences *)
  check_lids renamed_kind ids new_name lids;

  Profile.time_switch_to "find id defs";
  (* Compute the replacements for the *definitions* of the rename ids *)
  let defs, outside = FindName.locate_defs program ids in
  if outside <> [] then fail_owz "Cannot rename external ident(s) %s"
    (String.concat ", "
       (List.map
          (function
          `library l -> "in " ^ l | `predef -> "(predef)" | `unit_name -> old_name)
          outside));
  Profile.time_switch_to "rename lids";
  (* Compute renamed lids, checking that they are not captured *)
  let occ_replaces =
    rename_lids ?errors
      program renamed_kind ids new_name (group_by_source lids) loc2lid loc2id in
  Profile.time_pop ();
  let idents, fnames = rename_defs defs loc2lid renamed_kind new_name in
  idents, fnames, occ_replaces

(* Collect an ident in a list of source files. *)
let grep ?errors ?prefix kind id program =
  let old_name = Ident.name id in
  let all_constraints, mli_constraints, includes, redefs =
    Profile.time_call "collect constraints"
      (RenamePropagation.constrain_all_files ?errors kind old_name) program in
  let new_name = "invalid name" in
  let ids, _ =
    Profile.time_call "propagate"
      (RenamePropagation.propagate
         ?errors ~rename:false kind id all_constraints includes) redefs in
  let defs, _ = Profile.time_call "locate defs"
    (FindName.locate_defs program) ids in
  let lids = Profile.time_call " get all lids"
    (FindName.collect_lids_in_program
       ?errors (filter_relevant_names [old_name])) program in
  let occs =
    rename_lids ?errors
      program kind ids new_name (group_by_source lids) (Hashtbl.create 0) (Hashtbl.create 0) in
  let idents =
    List.filter_map
      (function `ident_at (_, loc, _) -> Some loc | `source_name _ -> None)
      defs
  and fnames =
  List.filter_map
    (function `source_name f -> Some f | `ident_at _ -> None)
    defs
  in
  idents, fnames, List.map fst occs

let all_defs ?errors kind id program =
  let old_name = Ident.name id in
  let all_constraints, mli_constraints, includes, redefs =
    RenamePropagation.constrain_all_files ?errors kind old_name program in
  let ids, _ =
    RenamePropagation.propagate
      ?errors ~rename:false kind id all_constraints includes redefs in
  let defs, _ = FindName.locate_defs program ids in
  let defs =
  List.filter_map
    (function loc ->
      match loc with
	| `ident_at (_, loc, _) -> Some loc
	| `source_name _ -> None)
    defs
  in
List.setify defs

(*
let last2old_range program unit (a, b) =
  ProgramCache.last2old `char program unit a,
  ProgramCache.last2old `char program unit b
*)

type region = [`lc of (int * int) * (int * int) | `cnum of int * int]

let region2pos = function
  | `lc (b, e) -> `lc b, `lc e
  | `cnum (b, e) -> `cnum b, `cnum e

let locate_ident_from_def_or_use program (prefix, _ as source) region =
  let b, e = region2pos region in
  let unit = Program.find_source program source in
  let loc = last_cnum2old_lc program unit b, last_cnum2old_lc program unit e in
  let s =
    try typedtree program (Program.find_source program source)
    with Not_found -> fail_owz "source file is not in the program"
  in
  fdebugln "%t"
    (TypedtreeDump.signature (signature program prefix));
  let ref =
    try
      Some
        (FindName.lid_of_loc
           (s :> TypedtreeOps.node) (TypedtreeLocate.included_lc loc)
           TypedtreeLocate.loc_included_lc)
    with Not_found -> None
  in
  let def =
    try
      Some
        (TypedtreeLocate.ident_definition
           (TypedtreeLocate.included_lc loc) TypedtreeLocate.loc_included_lc
           (s :> TypedtreeOps.node))
    with Not_found -> None
  in
  let included_strict l l' =
    TypedtreeLocate.loc_included_lc l l' &&
    not (TypedtreeLocate.loc_included_lc l' l)
  in
  let found =
    match ref, def with
      | Some ref, None -> `ref ref
      | None, Some def -> `def def
      | None, None -> raise Not_found
      | Some ({loc = ref_loc}, lid, _ as ref),
        Some (_, id, {loc = def_loc}, _ as def) ->
        debugln "Found an reference to %s" (lid2string lid);
        debugln "and a definition of %s" (Ident.name id);
        if included_strict ref_loc def_loc then `ref ref
        else if included_strict def_loc ref_loc then `def def
        else fail_owz "found both a definition and a reference"
  in
  match found with
    | `ref (_, lid, (env, kind)) ->
      let _env = source_env program unit in
      (try
         let path, desc = Resolve.checked_untyped_lookup kind lid env in
         let id = Resolve.resolve_path kind path env in
         debugln "Found an reference to %s" (Ident.name id);
         kind, id, desc, `ref path
       with Not_found ->
         fail_owz "failed to resolve %s %s"
           (Env_untyped.kind2string kind) (lid2string lid))
    | `def (kind, id, text, desc) ->
      debugln "Found a definition of %s" (Ident.name id);
      kind, id, desc, `def

let visible_ident_definition p source region program =
  let b, e = region2pos region in
  let source_file = Program.find_source program source in
  let loc =
    last_cnum2old_lc program source_file b, last_cnum2old_lc program source_file e in
  let typedtree = typedtree program source_file in
  let kind, id, text =
    TypedtreeLocate.visible_ident_definition
      p (TypedtreeLocate.included_lc loc) TypedtreeLocate.loc_included_lc
      (typedtree :> TypedtreeOps.node) in
  let loc = Program.correct_fname source_file.Program.source text.loc in
  fdebugln "found visible definition of %s at loc: %a"
    (Ident.name id) print loc;
  kind, id,
  old2last_loc program loc
