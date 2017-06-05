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
open Types
open Typedtree
open TypedtreeOps
open Resolve
open Location

include Debug.Tag(struct let tag = "findName" end)

let get_lids ?fname typedtree cond =
  List.filter_map
    (function text, p, (env, kind) ->
      let loc =
        match fname with
          | Some fname -> Program.correct_fname fname text.loc
          | None -> text.loc
      in
      let lid = text.txt in
      if cond lid loc then
	Some ({text with loc = loc}, lid, (env, kind))
      else
	None)
    (find_all_paths typedtree)

let get_lids ?fname typedtree =
  Profile.time_call "get lids" (get_lids ?fname typedtree)

(* locations filenames are relative to the root. *)
let collect_lids_in_program ?errors filter program =
  Program.fold_sources
    (fun source source_file acc ->
      let filename = source_file.Program.source in
      try_default ?errors ~prefix:(Printf.sprintf "File %s: " filename)
      acc (function () ->
      let typedtree = ProgramCache.typedtree program source_file in
      List.map
	(function text, lid, (env, kind) ->
	  let loc = ProgramCache.old2last_loc program text.loc in
          debugln "get_lids: in %s, found %s" filename (lid2string lid);
	  source, loc, lid, env, kind)
	(get_lids ~fname:filename (typedtree :> node) filter)
      @ acc))
    program
    []

let innermost_lids ?fname typedtree cond included =
  let lids = get_lids ?fname typedtree cond in
  List.filter
    (function {loc = loc}, _, _ ->
      not
        (List.exists
           (function {loc = loc'}, _, _ ->
             included loc' loc && not (included loc loc'))
           lids))
    lids

let lid_of_loc typedtree included_in loc_included_in =
  match
    innermost_lids typedtree (function _ -> included_in)
      loc_included_in
  with
    | [] -> raise Not_found
    | [id] -> id
    | ids ->
      fail_owz "found multiple longidents at this location: %s"
        (String.concat ", "
           (List.map
              (function _, lid, (_, kind) ->
                Env_untyped.kind2string kind ^ " " ^ lid2string lid)
              ids))

let find_ident_definitions id ast =
  find_all_map_ident_definitions
    (fun kind id' text _ ->
      if Ident.same id id' then
	Some (kind, id', text)
      else
	None)
    ast

let find_ident_definition id ast =
  match find_ident_definitions id ast with
    | [] -> raise Not_found
    | [id] -> id
    | _ -> assert false

let id2loc ?(accept_none=false) program id =
  debug "gid2loc: ";
  let loc =
    try
      match ident_context id with
	| Typedtree source -> debugln "source";
	  let source_file = Program.find_source program source in
	  let _, id, text =
	    find_ident_definition id
              (ProgramCache.typedtree program source_file :> node)
          in
          let loc = Program.correct_fname source_file.Program.source text.loc in
	  debugln "source=%s" (Program.source2string source);
	  debugln "pos_fname=%s" loc.loc_start.Lexing.pos_fname;
	  loc
        | Library _ -> raise Not_found
	| Toplevel dir -> debugln "toplevel";
          let load_path = [dir] in
	  let f = Program.modname2source_file program ~load_path (Ident.name id) in
	  in_file f.Program.source
	| Predef | PackImplem _ -> raise Not_found
    with Not_found when accept_none -> none
  in
  ProgramCache.old2last_loc program loc

(* Above and below should be merged ! *)

let locate_defs program ids =
  let inside = ref []
  and outside = ref []
  and add_to x l = l := x :: !l in
  List.iter
    (fun id ->
      match ident_context id with
	| Typedtree (prefix, _ as f) ->
	  debug "Rename %s in %s"(Ident.name id) prefix;
	  let source_file = Program.find_source program f in
	  let typedtree = ProgramCache.typedtree program source_file in
	  List.iter
	    (function _, _, {loc = loc ; txt = id} ->
              let loc = Program.correct_fname source_file.Program.source loc in
	      let loc = ProgramCache.old2last_loc program loc in
	      add_to (`ident_at (f, loc, id)) inside)
	    (find_ident_definitions id (typedtree :> node))
        | Library prefix -> add_to (`library prefix) outside
	| Toplevel dir ->
          (try
             let prefix =
               Program.modname2prefix program ~load_path:[dir] (Ident.name id) in
             add_to (`source_name prefix) inside
           with Not_found -> (* We could try to get the filename *)
              add_to (`unit_name) outside)
        | Predef -> add_to (`predef) outside
        | PackImplem _ -> ())
    ids;
  !inside, !outside
