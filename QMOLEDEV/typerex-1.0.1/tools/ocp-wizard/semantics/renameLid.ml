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

open Longident
open Resolve
open Util

(* Rename the ident id of type renamed_kind in the longident lid of kind sort *)
let rec rename_in_lid renamed_kind ids name env kind modname lid =
  let rename = rename_in_lid renamed_kind ids name env Env_untyped.Module modname in
  match renamed_kind, lid with
    | _, Lident i ->
      if kind = renamed_kind && resolves_to kind env lid ids then (
	check kind ~ids ~new_name:name (env, Env.summary env) ~renamed:true;
	Some (Lident name)
      ) else
	None
    | _, Ldot (pref, n) ->
      let n' =
	if kind = renamed_kind && resolves_to kind env lid ids then (
          let s = module_lid2sig env pref in
	  check_in_sig kind ~ids ~new_name:name s ~renamed:true;
	  Some name
	) else
	  None
      and pref' = rename pref in
      (match pref', n' with
	| None, None -> None
	| None, Some n -> Some (Ldot(pref, n))
	| Some pref, None -> Some (Ldot(pref, n))
	| Some pref, Some n -> Some (Ldot(pref, n)))
    | Env_untyped.Module, Lapply (lid, lid') ->
      (match rename lid, rename lid' with
	| None, None -> None
	| Some lid, None -> Some (Lapply (lid, lid'))
	| None, Some lid' -> Some (Lapply (lid, lid'))
	| Some lid, Some lid' -> Some (Lapply (lid, lid')))
    | _, Lapply _ -> None

let rec check_lid renamed_kind ids name env kind lid =
  let check_lid = check_lid renamed_kind ids name env Env_untyped.Module in
  match lid with
    | Lident (i) ->
      if kind = renamed_kind && i = name then
	check kind ~ids ~new_name:name (env, (Env.summary env)) ~renamed:false
    | Ldot (pref, n) ->
      check_lid pref;
      if kind = renamed_kind && n = name then
        let s = module_lid2sig env pref in
	check_in_sig kind ~ids ~new_name:name s ~renamed:false
    | Lapply (lid, lid') ->
      if renamed_kind = Env_untyped.Module then (
	check_lid lid;
	check_lid lid'
      )

let rec prune_lid env kind lid =
  match lid with
    | Lident _ -> None
    | Ldot (pref, n) ->
      let lid' = Lident (n) in
      if
	(try
	   let p = Env_untyped.lookup kind lid env in
	   let p' = Env_untyped.lookup kind lid' env in
	   p' = p
	 with Not_found -> false)
      then
	Some lid'
      else
	(match prune_lid env Env_untyped.Module pref with
	  | Some pref -> Some (Ldot (pref, n))
	  | None -> None)
    | Lapply (lid, lid') ->
      match prune_lid env Env_untyped.Module lid, prune_lid env Env_untyped.Module lid' with
	| Some lid, Some lid' -> Some (Lapply (lid, lid'))
	| None, Some lid' -> Some (Lapply (lid, lid'))
	| Some lid, None -> Some (Lapply (lid, lid'))
	| None, None -> None

exception Cannot_eliminate

let rec eliminate_open opened_path opened_lid env kind lid =
  match lid with
    | Lident (n) ->
      let path, _ = checked_untyped_lookup kind lid env in
      (match path with
	| Path.Pident _ -> None
	| Path.Pdot (p, _, _) ->
	  if p = opened_path then
	    let lid = Ldot (opened_lid, n) in
	    if
	      (try Env_untyped.lookup kind lid env = path
	       with Not_found -> false)
	    then
	      Some lid
	    else
	      raise Cannot_eliminate
	  else
	    None
	| Path.Papply _ -> assert false)
    | Ldot (pref, n) ->
      (match eliminate_open opened_path opened_lid env Env_untyped.Module pref with
	| Some pref -> Some (Ldot (pref, n))
	| None -> None)
    | Lapply (lid, lid') ->
      match
	eliminate_open opened_path opened_lid env Env_untyped.Module lid,
	eliminate_open opened_path opened_lid env Env_untyped.Module lid'
      with
	| Some lid, Some lid' -> Some (Lapply (lid, lid'))
	| None, Some lid' -> Some (Lapply (lid, lid'))
	| Some lid, None -> Some (Lapply (lid, lid'))
	| None, None -> None
