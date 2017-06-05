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

open Asttypes
open OcpLang
open Types
open Typedtree
open TypedtreeOps
open Env
open Location
open Env_untyped
include Debug.Tag(struct let tag = "typedtreeLocate" end)

let fst3 (fst,_,_) = fst
let fst4 (fst,_,_,_) = fst
let snd3 (_,snd,_) = snd
let snd4 (_,snd,_,_) = snd
let snd5 (_,snd,_,_,_) = snd

let contains loc (b', e') =
  let b, e = ParseOCaml.get_c_num loc in
  b <= b' && e' <= e

let included range loc = contains loc range

let pos_lc pos =
  pos.Lexing.pos_lnum,
  pos.Lexing.pos_cnum - pos.Lexing.pos_bol

let loc2lc loc = pos_lc loc.loc_start, pos_lc loc.loc_end

let contains_lc loc (b, e) =
  (pos_lc loc.loc_start) <= b && e <= (pos_lc loc.loc_end)

let loc_contains_lc loc loc' =
  (pos_lc loc.loc_start) <= (pos_lc loc'.loc_start) &&
  (pos_lc loc'.loc_end) <= (pos_lc loc.loc_end)

let contains_lc' (b, e) (b', e') =
  b <= b' && e' <= e'

(*
let contains_lc loc ((l, c), (l', c') as loc') =
  fdebugln
    "checking whether %a(absolute [%d, %d[)\n\t\tcontains [(%d, %d), (%d, %d)[ "
    Location.print loc loc.loc_start.Lexing.pos_cnum loc.loc_end.Lexing.pos_cnum
    l c l' c';
  let res = contains_lc loc loc' in
  debugln "-> %B" res;
  res
*)

let included_lc range loc = contains_lc loc range
let loc_included_lc loc loc' = loc_contains_lc loc' loc
let included_lc' r r' = contains_lc' r' r

let node2loc = function
  | `module_expr e -> e.mod_loc
  | `module_type t -> t.mty_loc
  | `pattern p -> p.pat_loc
  | `expression e -> e.exp_loc
  | `structure {str_items = s} when s <> [] ->
    let loc =
      { (List.hd s).str_loc with
	loc_end = (List.last s).str_loc.loc_end }
    in
    loc
  | `signature {sig_items = s} when s <> [] ->
    let loc =
      { (List.hd s).sig_loc with
	loc_end = (List.last s).sig_loc.loc_end }
    in
    loc
  | `structure_item i -> i.str_loc
  | `signature_item i -> i.sig_loc
  | `type_declaration d -> d.typ_loc
  | `class_type_declaration { ci_loc = ci_loc }
  | `class_declaration { ci_loc = ci_loc }
  | `class_description { ci_loc = ci_loc } -> ci_loc
  | _ -> raise Not_found

let find_map_fun f included_in node =
  try
    let loc = node2loc node in
    if included_in loc then
      f loc node
    else None
  with Not_found -> None

let locate_all_map f included_in =
  find_all_map (find_map_fun f included_in)

let locate_all_map priority f included_in included ast =
  let candidates =
    locate_all_map
      (fun loc node -> match f loc node with
        | Some res -> Some (loc, res)
        | None -> None)
      included_in ast in
  let strictly_better loc loc' =
    match priority with
      | `innermost -> included loc loc' && not (included loc' loc)
      | `outermost -> included loc' loc && not (included loc loc')
  in
  List.filter_map
    (function loc, node ->
      if
        List.exists
          (function loc', _ ->
            strictly_better loc' loc)
          candidates
      then
        None
      else Some node)
    candidates

let locate_map priority f included_in =
  find_map priority (find_map_fun f included_in)

let locate_map priority f included_in included ast =
  match locate_all_map priority f included_in included ast with
    | [] -> raise Not_found
    | v :: _ -> v

let locate priority = locate_map priority (fun _ x -> Some x)

let locate_field0 included_in ploc fs tfs =
  List.find
    (function _, tf -> included_in (ploc tf).loc)
    (List.combine fs tfs)

let locate_field included_in pid ploc fs tfs =
  let f, tf =
    List.find
      (function _, tf -> included_in (ploc tf).loc)
      (List.combine fs tfs)
  in
  (pid f, ploc tf)

let locate_id0 included_in ploc decls =
  List.find
    (function d -> included_in (ploc d).loc)
    decls

let locate_id included_in pid ploc decls =
  let v =
    (List.find
       (function d -> included_in (ploc d).loc)
       decls)
  in
  pid v, ploc v

let ident_definition included_in included s =
  let kind, id, text, desc =
    locate_map `innermost
      (fun _ n ->
        let found = ref None in
        TypedtreeOps.apply2defs
          (fun kind id text desc ->
            if included_in text.loc then
              found := Some (kind, id, text, desc))
          n;
        !found)
      included_in included
      s
  in
  if included_in text.loc then
    kind, id, text, desc
  else
    raise Not_found

let locate_map_item f included_in included =
  locate_map `innermost
    (function _ -> function
      | `structure s ->
	let item =
	  locate_map `outermost
	    (function _ -> function
            `structure_item item -> Some item | _ -> None)
	    included_in included (`structure s)
	in
	f item.str_loc (`structure (s, item))
      | `signature s ->
	let item =
	  locate_map `outermost
	    (function _ -> function
            `signature_item item -> Some item | _ -> None)
	    included_in included (`signature s)
	in
	f item.sig_loc (`signature (s, item))
      | _ -> None)
    included_in included

let locate_item = locate_map_item (fun loc item -> Some (loc, item))

let locate_declaration loc_of_decl id_of_decl included_in =
  OcpList.find_map
    (function d ->
      if included_in (loc_of_decl d) then
	Some (id_of_decl d)
      else
	None)

let locate_value =
  locate_declaration
    (function p, e -> { p.pat_loc with loc_end = e.exp_loc.loc_end })
    (function p, _ ->
      match Typedtree.pat_bound_idents p with
	| [] -> raise Not_found
	| id :: _ -> id)

let locate_type =
  locate_declaration
    (function id, text, d -> { text.loc with loc_end = d.typ_loc.loc_end })
    (function id, text, _ -> id, text)

let locate_recmodule =
  locate_declaration
    (function id, text, _, m -> { text.loc with loc_end = m.mod_loc.loc_end })
    (function id, text, _, _ -> id,text)

let locate_sig_recmodule =
  locate_declaration
    (function id, text, mt -> { text.loc with loc_end = mt.mty_loc.loc_end })
    (function id, text, _ -> id,text)

let visible_ident_definition priority loc s =
  locate_map priority
    (function _ -> function
      | `structure_item s ->
	(match s.str_desc with
	  | Tstr_primitive (id, text, _) -> Some (Value, (id, text))
	  | Tstr_exception (id, text, _) -> Some (Constructor, (id, text))
	  | Tstr_exn_rebind (id, text, _, _) -> Some (Constructor, (id, text))
	  | Tstr_module (id, text, _) -> Some (Module, (id, text))
	  | Tstr_modtype (id, text, _) -> Some (Modtype, (id, text))

	  | Tstr_value (_, vs) -> Some (Value, locate_value loc vs)
	  | Tstr_type ts -> Some (Type, locate_type loc ts)
	  | Tstr_recmodule ms -> Some (Value, locate_recmodule loc ms)

	  (* TODO *)
	  | Tstr_class cs -> None
	  | Tstr_class_type cts -> None

	  | Tstr_eval _
	  | Tstr_open _
	  | Tstr_include _ -> None)

      | `signature_item s ->
	(match s.sig_desc with
	  | Tsig_value (id, text, _) -> Some (Value, (id, text))
	  | Tsig_exception (id, text, _) -> Some (Constructor, (id,text))
	  | Tsig_module (id, text, _) -> Some (Module, (id,text))
	  | Tsig_modtype (id, text, _) -> Some (Modtype, (id,text))

	  | Tsig_type ts -> Some (Type, locate_type loc ts)
	  | Tsig_recmodule ms -> Some (Value, locate_sig_recmodule loc ms)

	  (* TODO *)
	  | Tsig_class cs -> None
	  | Tsig_class_type cts -> None

	  | Tsig_open _
	  | Tsig_include _ -> None)
      | _ -> None)
    loc
    s

let visible_ident_definition priority included_in included t =
  let (sort, (id, text)) =
    visible_ident_definition priority included_in included t in
  (sort, id, text)
