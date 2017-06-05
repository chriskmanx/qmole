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
open Resolve
include Debug.Tag(struct let tag = "renamePropagation" end)

let restrict_propagation = ref false

module ConstraintSet =
  Set.Make (struct
    type t = Types.signature * Types.signature
    let compare = compare
  end)

module IncludeSet =
  Set.Make (struct
    type t = Types.signature * Ident.t list
    let compare = compare
  end)

module PersistentSet =
  Set.Make (struct
    type t = Ident.t * (string * Program.compilation_unit) option
    let compare = compare
  end)

let rec collect_persistent pers = function
  | Path.Pident id ->
    if Ident.persistent id then
      pers := PersistentSet.add
        (id, try Some (ProgramCache.modname2unit (Ident.name id))
         with Not_found -> None)
        !pers
  | Path.Papply (p, p') -> collect_persistent pers p ; collect_persistent pers p'
  | Path.Pdot (p, _, _) -> collect_persistent pers p

let find_module pers p env =
  collect_persistent pers p;
  Resolve.checked_find Env_untyped.Module Env.find_module p env

let find_modtype pers p env =
  collect_persistent pers p;
  Resolve.checked_find Env_untyped.Modtype Env.find_modtype p env

let rec constraint_modtype ?errors incs env t env' t' =
  fdebugln "constraint_modtype\n%twith\n%t"
    (TypedtreeDump.module_type t)
    (TypedtreeDump.module_type t');
  (* We rely on the fact that
     "module type X
     does not match
     module type X = sig  end" *)
  try
    let t' = modtype env' t' in
    let t =
      try modtype env t
      with Abstract_modtype -> assert false
    in
    match t, t' with
      | `sign sg, `sign sg' -> constraint_signature ?errors incs env sg env' sg'
      | `func (id, arg, res), `func (id', arg', res') ->
	constraint_modtype ?errors incs env' arg' env arg;
	constraint_modtype ?errors incs
	  (Env.add_module id arg env) res
	  (Env.add_module id' arg' env') res'
      | _ -> assert false
  with
      Abstract_modtype -> ()

and constraint_signature ?errors incs env sg env' sg' =
  debugln "constraint_signature";
  fdebugln "\nconstraint:\n%twith\n%t"
    (TypedtreeDump.signature sg)
    (TypedtreeDump.signature sg');
  incs := ConstraintSet.add (sg, sg') !incs;
  let sg = add_environments env sg
  and sg' = add_environments env' sg' in
  List.iter
    (function env', item ->
      try_do ?errors (function () ->
        match item with
      | Sig_module (id, t', _) ->
	(match
	    lookup_in_signature Env_untyped.Module (Ident.name id) sg
	 with
	   | env, Sig_module (_, t, _) ->
	     constraint_modtype ?errors incs env t env' t'
	   | _ -> assert false)
      | Sig_modtype (id, Modtype_manifest t') ->
	(match
	    lookup_in_signature Env_untyped.Modtype (Ident.name id) sg
	 with
	   | env, Sig_modtype (_, Modtype_manifest t) ->
	     constraint_modtype ?errors incs env t env' t'
	   | _ -> assert false)
      | _ -> ()))
    sg'

let constraint_with_interface ?errors incs env (prefix, _ as file) impl intf =
  match impl, intf with
    | `structure {str_type = sg}, `signature {sig_type = sg'} ->
  debugln "\ninternal interface of %s:" (Program.source2string file);
  TypedtreeDump.signature sg debug_formatter;
  debugln "external interface of unit %s:" prefix;
  TypedtreeDump.signature sg' debug_formatter;
  constraint_signature ?errors incs
    env sg env sg'
    | _ -> assert false

type constraints = {
  errors : exn list ref option;
  incs : ConstraintSet.t ref;
  mli_incs : ConstraintSet.t ref;
  includes : IncludeSet.t ref;
  redefs : (Ident.t * Ident.t) list ref;
  pers : PersistentSet.t ref
}

let union c c' = {
  errors =
    (match c.errors, c'.errors with
      | Some c, Some c' -> Some (ref (!c @ !c'))
      | None, None -> None
      | _ -> assert false);
  incs = ref (ConstraintSet.union !(c.incs) !(c'.incs));
  mli_incs = ref (ConstraintSet.union !(c.mli_incs) !(c'.mli_incs));
  includes = ref (IncludeSet.union !(c.includes) !(c'.includes));
  redefs = ref (!(c.redefs) @ !(c.redefs));
  pers = ref (PersistentSet.union !(c.pers) !(c'.pers));
}

let empty ~fail_fast = {
  errors = if fail_fast then None else Some (ref []);
  incs = ref ConstraintSet.empty;
  mli_incs = ref ConstraintSet.empty;
  includes = ref IncludeSet.empty;
  redefs = ref [];
  pers = ref PersistentSet.empty
}

let constraints_constructors redefs =
  List.iter2
    (fun (c, _, _) (c', _, _) ->
      redefs := (c, c') :: !redefs)

let constraints_fields redefs =
  List.iter2
    (fun (c, _, _) (c', _, _) ->
      redefs := (c, c') :: !redefs)

(* Collect the set of signature inclusion constraints implied by a typedtree. *)
let collect_signature_inclusions {errors = errors;
                                  incs = incs;
                                  pers = pers;
                                  includes = includes;
                                  redefs = redefs;
                                 } file s =
  let process loc cons = fdebugln "%a\nprocessing %s" Location.print loc cons in
  let constraint_modtype = constraint_modtype ?errors in
  let enter node =
    let prefix =
      try Some (loc2string (TypedtreeLocate.node2loc node))
      with _ -> None
(*
    and warn fmt = fail fmt in
*)
    and warn fmt = Printf.ifprintf () fmt in
    try_apply ~errors ?prefix () (function
    | `module_expr m ->
      (match m.mod_desc with

	| Tmod_constraint (m, t, cs, co) ->
	  constraint_modtype incs m.mod_env m.mod_type m.mod_env t
	(* what about cs and co ? *)

	| Tmod_apply (f, m', co) ->
          process m.mod_loc "tmod_apply";
	  let (_, param_t, res_t) = modtype_functor f.mod_env f.mod_type in
	  constraint_modtype incs f.mod_env m'.mod_type m'.mod_env param_t;
	  constraint_modtype incs f.mod_env res_t m.mod_env m.mod_type
        (* f.mod_env is probably wrong, since it does not contain the argument *)
	(* what about co ? *)

	| Tmod_unpack _ ->
          warn "propagation for Tmod_unpack not implemented"

	| Tmod_ident (p, lid) ->
          (* mandatory (see [iter_class_expr] in functor body in TypedtreeOps *)
          process m.mod_loc "tmod_ident";
          let m' = find_module pers p m.mod_env in
          constraint_modtype incs m.mod_env m' m.mod_env m.mod_type

	| Tmod_structure _ -> ()
	| Tmod_functor (id, _, param_t, res) ->
          process m.mod_loc "tmod_functor";
	  let (_, param_t', res_t') = modtype_functor m.mod_env m.mod_type in
	  constraint_modtype
            incs param_t.mty_env param_t.mty_type m.mod_env param_t';
	  constraint_modtype incs m.mod_env res_t' res.mod_env res.mod_type
      )

    | `module_type t ->
      (match t.mty_desc with
        | Tmty_functor (id, _, param_t, res) ->
          process t.mty_loc "mty_functor";
	  let (_, param_t', res_t') = modtype_functor t.mty_env t.mty_type in
	  constraint_modtype
            incs param_t.mty_env param_t.mty_type t.mty_env param_t';
	  constraint_modtype incs t.mty_env res_t' res.mty_env res.mty_type
        | Tmty_signature s ->
	  constraint_modtype
            incs t.mty_env t.mty_type t.mty_env (Mty_signature s.sig_type)
        | Tmty_ident (p, lid) ->
          process t.mty_loc "tmty_ident";
          (match find_modtype pers p t.mty_env with
            | Modtype_manifest m' ->
              constraint_modtype incs t.mty_env m' t.mty_env t.mty_type
            | Modtype_abstract -> ())
        | Tmty_with (mt, cs) ->
          warn "propagation for with constraints not implemented"
        | Tmty_typeof {mod_env = mod_env ; mod_type = mod_type} ->
          process t.mty_loc "mty_typeof";
          constraint_modtype incs mod_env mod_type t.mty_env t.mty_type)

    | `structure_item {str_desc = Tstr_include (m, ids) ; str_loc = str_loc } ->
	  (* We may have
  	     module G(X : sig module type T module X : T end) =
             struct include X end *)
          process str_loc "str_include";
	  (try
	     let sign = modtype_signature m.mod_env m.mod_type in
	     includes := IncludeSet.add (sign, ids) !includes
	   with Abstract_modtype -> ())

    | `signature_item {sig_desc = Tsig_include (mt, sg) ;
                       sig_loc = sig_loc; sig_env = sig_env } ->

          process sig_loc "sig_include";
	  (try
	     let sg' = modtype_signature mt.mty_env mt.mty_type in
             constraint_signature ?errors incs sig_env sg mt.mty_env sg'
          (*
	     includes := IncludeSet.add (sg, sg') !includes
          *)
	   with Abstract_modtype -> ())

    | `structure_item {str_desc = Tstr_type ts ; str_loc = loc ; str_env = env}
    | `signature_item {sig_desc = Tsig_type ts ; sig_loc = loc ; sig_env = env} ->
          process loc "type_declarations";
	  List.iter
	    (function _, _, {typ_type = d} ->
	      match d.type_manifest, d.type_kind with
		| Some t, (Type_variant _ | Type_record _) ->
		  let d0 = match t.desc with
		    | Tconstr (p, _, _) ->
		      Resolve.checked_find Env_untyped.Type
                        Env.find_type p env
		    | _ -> assert false
		  in
		  (match d0.type_kind, d.type_kind with
		    | Type_variant cs0, Type_variant cs ->
		      constraints_constructors redefs cs0 cs
		    | Type_record (fs0, _), Type_record (fs, _) ->
		      constraints_fields redefs fs0 fs
		    | _ -> assert false)
		| _ -> ())
	    ts

(*
    | `expression e ->
      (match e.exp_desc with
        | Texp_letmodule (_, _, m) ->
        | _ -> ())
*)

    | _ -> ())
        node
  in
  TypedtreeOps.iterator ~enter ~leave:ignore s

let constrain_one_file
    ~fail_fast program (prefix, source_kind as file) source_file =
  let {errors = errors; incs = incs;
       mli_incs = mli_incs; includes = includes;
       redefs = redefs } as constraints =
    empty ~fail_fast in
  let filename = source_file.Program.source in
  try_do ?errors ~prefix:(Printf.sprintf "File %s: " filename)
    (function () ->
        (* (file, source_file, e)*)
      let env = ProgramCache.source_env program source_file in
      let typedtree =
	try ProgramCache.typedtree program source_file
	with Not_found -> raise (ProgramCache.NoCmt source_file)
      in
      let `structure {str_type = t} | `signature {sig_type = t} = typedtree in
      fdebugln "collecting signature inclusions in %s\n with intf: %t"
        filename (TypedtreeDump.signature t);
      Profile.time_call "collect signature inclusions"
        (collect_signature_inclusions constraints file)
        (typedtree :> TypedtreeOps.node);
      if source_kind = `ml then
        match Program.find_unit program prefix with
          | Program.Concrete
              {Program.interface = Some intf ; implementation = Some impl} ->
            debugln "constraining %s with its external interface" filename;
                Profile.time_call "constraint with interface"
                  (constraint_with_interface mli_incs env file
                     typedtree)
                  (ProgramCache.typedtree program intf)
          | Program.Concrete
              {Program.interface = None ; implementation = Some _} -> ()
          | _ -> assert false);
  constraints

let constrain_pack ~fail_fast program prefix unit =
  let sg, units = ProgramCache.pack program unit in
  let {errors = errors; incs = incs;
       mli_incs = mli_incs;
       includes = includes; redefs = redefs } as constraints =
    empty ~fail_fast in
  try_do ?errors ~prefix:(Printf.sprintf "In pack module %s: " prefix)
    (function () ->
      let env = ProgramCache.pack_env program unit in
      let package =
        List.filter_map
          (function
            | Sig_module (id, Mty_signature sg, Trec_not) ->
              try_default ~errors None (function () ->
                let name = Ident.name id in
                let pref, unit =
                  try ProgramCache.modname2unit name
                  with Not_found ->
                    fail "compilation unit %s not found" name
                in
                let sg' =
                  try ProgramCache.signature program pref
                  with Not_found -> fail "compilation unit %s not found" pref
                in
                Some
                  (Sig_module
                     (Ident.create_persistent name, Mty_signature sg', Trec_not)))
            | _ -> assert false)
          sg
      in
      constraint_signature ?errors constraints.incs env package env sg);
  constraints

let constrain_one_file_permissive =
  ProgramCache.make_program_source_file_cache
    ~value_up_to_date:
    (fun program constraints ->
      List.map
        (function id , unit ->
          try
            let prefix, unit =
              match unit with
                | Some unit -> unit
                | None -> ProgramCache.modname2unit (Ident.name id)
            in
            let unit = Program.find_unit program prefix in
            let t = ProgramCache.modtime (ProgramCache.unit2cmi prefix unit) in
            debugln "modtime(%s) = %f" prefix t;
            t
          with _ -> -. max_float)
        (PersistentSet.elements !(constraints.pers)))
    (fun program source_id ->
      let source =
        try Program.find_source program source_id
        with Not_found -> assert false
      in
      constrain_one_file ~fail_fast:false
        program source_id source)

let constrain_one_file ~fail_fast program source_id =
  if fail_fast then
    constrain_one_file ~fail_fast:true
      program source_id (Program.find_source program source_id)
  else
    constrain_one_file_permissive program source_id

let constrain_all_files ?errors kind name program =
  let fail_fast = errors = None in
  let constraints =
    Program.fold_sources
      ~pack:(fun prefix unit constraints ->
        let constraints =
          match unit.Program.p_interface with
            | Some i ->
              union constraints
                (constrain_one_file ~fail_fast program (prefix, `mli))
            | None -> constraints
        in
        union constraints (constrain_pack ~fail_fast program prefix unit))
      (fun source_id source_file constraints ->
        union constraints (constrain_one_file ~fail_fast program source_id))
      program
      (empty ~fail_fast)
  in
  (match errors, constraints.errors with
    | Some c, Some c' -> c := !c @ !c'
    | None, None -> ()
    | _ -> assert false);
  ConstraintSet.union
    !(constraints.incs) !(constraints.mli_incs), !(constraints.mli_incs),
  !(constraints.includes), !(constraints.redefs)

module Eq : sig

  type 'a t = ('a, 'a list ref) Hashtbl.t

  val add : 'a t -> 'a -> 'a -> unit

  val find : 'a t -> 'a -> 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t

  val union : 'a t -> 'a t -> 'a t

end = struct
(* An equivalence relation is represented by a mapping from elements
   to their (non-trivial) equivalence class. *)
  type 'a t = ('a, 'a list ref) Hashtbl.t

  let add eq x y =
	match x, y, Hashtbl.mem eq x, Hashtbl.mem eq y with
	  | _, _, false, false ->
	    let l = ref [x ; y] in
	    Hashtbl.add eq x l;
	    Hashtbl.add eq y l
	  | _, _, true, true ->
	    let lx = Hashtbl.find eq x and ly = Hashtbl.find eq y in
	    if lx !=  ly then (
	      lx := List.rev_append !ly !lx;
	      List.iter
		(fun y -> Hashtbl.replace eq y lx)
		!ly
	    )
	  | x, y, true, false
	  | y, x, false, true ->
	    let x = Hashtbl.find eq x in
	    x := y :: !x;
	    Hashtbl.add eq y x

  let map f eq =
    let eq' = Hashtbl.create 10 in
    Hashtbl.iter
      (fun k l -> Hashtbl.add eq' (f k) (ref (List.map f !l)))
      eq;
    eq'

  let union eq eq' =
    (* Copy is not enough *)
    let eq = map (function x -> x) eq in
    Hashtbl.iter
      (fun x l -> List.iter (add eq x) !l)
      eq';
    eq

  let find eq x =
    try !(Hashtbl.find eq x)
    with Not_found -> [x]

end

let propagate_constraints bind_id_to_member kind name incs errors =
  ConstraintSet.iter
    (function sg, sg' ->
      try_do ~errors (function () ->
        try
	  let id' = find_in_signature kind name sg' in
	  bind_id_to_member `certain sg id'
        with Not_found -> ()))
    incs

let propagate_includes bind_id_to_member ambiguous kind name includes errors =
  IncludeSet.iter
    (function sg, ids ->
      try_do ~errors (function () ->
      match List.filter (function id -> Ident.name id = name) ids with
	| [] -> ()
	  (* WARNING ! We should check if the name id define
	     with the right kind in sg ! *)
	| [id] -> bind_id_to_member `maybe sg id
	| ids -> (* correct choice would require access to the resulting
		    environment to check ids w.r.t. kind. *)
	  List.iter
	    (function id
	    -> bind_id_to_member `maybe sg id)
	    ids;
	    (* because we still need to check them for capture *)
	  ambiguous := (find_in_signature kind name sg) :: !ambiguous))
    includes

exception EscapingRenaming of Ident.t list * Ident.t list

let propagate
    ?errors ?(rename=true) kind id incs ?mli_constraints includes redefs =
  let name = Ident.name id in
  let eq = Hashtbl.create 10 in
  let implicit_refs = ref []
  and ambiguous = ref [] in
  let bind_id_to_member flag sg id' =
    try
      let id = find_in_signature kind name sg in
      debugln "bind %s with %s"
        (Ident.name_with_ctx id') (Ident.name_with_ctx id);
      implicit_refs := (flag, sg, id') :: !implicit_refs;
      Eq.add eq id id'
    with Not_found ->
      match flag with
        | `maybe -> ()
        | _ ->
          TypedtreeDump.signature sg Format.str_formatter;
          fail "bind_id_to_member: %s %s not found in signature: %s"
            (Env_untyped.kind2string kind) name (Format.flush_str_formatter ())
  in
  debugln "propagating includes...";
  propagate_includes bind_id_to_member ambiguous kind name includes errors;
  let restrict_propagation =
    match mli_constraints with
      | Some cs ->
	debugln "propagating mli constraints only...";
	propagate_constraints bind_id_to_member kind name cs errors;
	let ids = Eq.find eq id in
	(function ids' ->
	  if ids' <> ids then
	    raise (EscapingRenaming (ids, ids'));
	  (* We could emit only a warning *)
	  ids)
      | None -> function x -> x
  in
  debugln "propagating all constraints...";
  propagate_constraints bind_id_to_member kind name incs errors;
  (* Type redefs are NOT implicit references. *)
  List.iter (function gid, gid' -> Eq.add eq gid gid') redefs;
  let ids = Eq.find eq id in
  let ids = restrict_propagation ids in

  (* Checks in case of renaming *)
  if rename then (
    (* Check if propagation reached an unlocalised id *)
    List.iter (* TODO: Fix this! *)
      (function id ->
	match Resolve.ident_context id with
	  | _ -> ())
      ids;
    (* Check if ids intersect ambiguous *)
    List.iter
      (function id ->
	if List.mem id ids then
	  fail_owz
	    "Cannot perform renaming because of an ambiguous include")
      !ambiguous
  );
  ids, !implicit_refs

(* Not used anymore *)
let select_ids ctx =
  List.filter_map
    (function ctx', id -> if ctx' = ctx then Some id else None)

(* Check that the implicit ident references which are concerned by
   renaming will not be masked (i.e., that the bound signature items
   remain the same). *)
let check_renamed_implicit_references renamed_kind ids name' implicit_refs =
  List.iter
    (function flag, sg, id ->
      try
	if List.mem id ids then
	  check_in_sig renamed_kind ~ids ~new_name:name' sg ~renamed:true
      with Not_found ->
	assert (flag = `maybe))
    implicit_refs

(* Check that the implicit ident references which are concerned by
   renaming will not be masked (i.e., that the bound signature items
   remain the same). *)
let check_other_implicit_references renamed_kind ids name' constraints includes =
  ConstraintSet.iter
    (function sg, sg' ->
      try
	let _ = find_in_signature renamed_kind name' sg' in
	check_in_sig renamed_kind ~ids ~new_name:name' sg ~renamed:false
      with
	  Not_found -> ())
    constraints;
  IncludeSet.iter
    (function sg, ids' ->
       match List.filter (function id -> Ident.name id = name') ids' with
	 | [] -> ()
	 | _ ->
	   try
	     check_in_sig renamed_kind ~ids ~new_name:name' sg ~renamed:false
	   with
	       Not_found -> ()
		   (* Because we don't know the sort od these ids *))
    includes
