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

open Env
open Path
open Types

module Lazy = EnvLazy

let ident_tbl_fold f t acc =
  List.fold_right
    (fun key acc -> f key (Ident.find_same key t) acc)
    (Ident.keys t)
    acc

let find_all proj1 proj2 f lid env =
  match lid with
    | None ->
      ident_tbl_fold
	(fun id (p, data) -> f (Ident.name id) p data)
	(proj1 env)
    | Some l ->
      let p, desc = lookup_module_descr l env in
      begin match Lazy.force desc with
          Structure_comps c ->
            Tbl.fold
	      (fun s (data, pos) -> f s (Pdot (p, s, pos)) data)
	      (proj2 c)
	| Functor_comps _ ->
          raise Not_found
      end

let fold_modules f lid env acc =
  match lid with
    | None ->
      let acc =
	ident_tbl_fold
	  (fun id (p, data) -> f (Ident.name id) p data)
	  env.modules
	  acc
      in
      Hashtbl.fold
	(fun name ps ->
	  f name (Pident(Ident.create_persistent name)) (Mty_signature ps.ps_sig))
	persistent_structures
	acc
    | Some l ->
      let p, desc = lookup_module_descr l env in
      begin match Lazy.force desc with
          Structure_comps c ->
            Tbl.fold
	      (fun s (data, pos) -> f s (Pdot (p, s, pos)) (Lazy.force data))
	      c.comp_modules
	      acc
	| Functor_comps _ ->
          raise Not_found
      end

let fold_values f =
  find_all (fun env -> env.values) (fun sc -> sc.comp_values) f
and fold_constructors f =
  find_all (fun env -> env.constrs) (fun sc -> sc.comp_constrs) f
and fold_labels f =
  find_all (fun env -> env.labels) (fun sc -> sc.comp_labels) f
and fold_types f =
  find_all (fun env -> env.types) (fun sc -> sc.comp_types) f
and fold_modtypes f =
  find_all (fun env -> env.modtypes) (fun sc -> sc.comp_modtypes) f
and fold_classs f =
  find_all (fun env -> env.classes) (fun sc -> sc.comp_classes) f
and fold_cltypes f =
  find_all (fun env -> env.cltypes) (fun sc -> sc.comp_cltypes) f
