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

open Path
open Types
open Util
open Env
open OcpLang
open Program
open Env_untyped

include Debug.Tag(struct let tag = "resolve" end)


type ident_context =
  | Typedtree of source_file_id
  | Library of string
  | Toplevel of string
  | Predef
  | PackImplem of string

let persistent2prefix id =
  let modname = Ident.name id in
  fst (ProgramCache.modname2unit modname)

exception Unbound of path_sort * Longident.t

let ident_context id =
  let name = Ident.name id in
  let is = debugln "ident %s is %s" name in
  match ProgramCache.classify_ident id with
    | `persistent -> is "persistent";
      let prefix =
        try
          persistent2prefix id
        with Not_found ->
          raise (Unbound (Module, Longident.Lident (Ident.name_with_ctx id)))
      in
      Toplevel (Filename.dirname prefix)
    | `predef -> is "predef";
      Predef
    | `fresh -> (* stub! *) assert false
    | `dumped -> is "dumped";
      (try
         let prefix, unit = ProgramCache.ctx2prefix id.Ident.ctx
         and source_kind =
           match id.Ident.ctx.Ident.kind with
             | `interface -> `mli
             | `implementation | `pack -> `ml
         in
           match unit with
             | Concrete _ when
                 not (String.starts_with ~prefix:Config.standard_library prefix)
                 -> Typedtree (prefix, source_kind)
             | Pack _ ->
               if id.Ident.ctx.Ident.kind = `interface then
                 Typedtree (prefix, `mli)
               else PackImplem prefix
             | _ -> Library prefix
       with Not_found ->
         raise (Unbound (Module, Longident.Lident id.Ident.ctx.Ident.modname)))
    | `hidden -> is "hidden";
      Printf.ksprintf failwith "don't know what to do with hidden ident %S" name

let context2string = function
  | Typedtree s -> source2string s
  | Library s -> s ^ ".cmi"
  | Toplevel dir -> Printf.sprintf "toplevel(%s)" dir
  | Predef -> "(predef)"
  | PackImplem s -> s ^ "(pack)"

let gid2string (ctx, id) =
  Ident.unique_name id ^ " in " ^ context2string ctx

let parse_string parse s =
  let lexbuf = Lexing.from_string s in
  parse Lexer.token lexbuf

let wrap_lookup unbound lookup x e =
  try lookup x e
  with Not_found -> unbound x

let checked_lookup kind =
  wrap_lookup (function lid -> raise (Unbound (kind, lid)))

let checked_find kind =
  wrap_lookup (function p -> raise (Unbound (kind, Untypeast.lident_of_path p)))

let checked_untyped_lookup kind = checked_lookup kind (untyped_lookup kind)

let checked_lookup_module lid e =
  wrap_lookup (function lid -> raise (Unbound (Module, lid)))
    lookup_module lid e

(* Get the ident of a signature item, if it has one, and matches the kind. *)
let sig_item sort item =
  match sort, item with
    | Value, Sig_value (i, _)
    | Type, Sig_type (i, _, _)
    | Module, Sig_module (i, _, _)
    | Modtype, Sig_modtype (i, _)
    | Class, Sig_class (i, _, _)
    | Cltype, Sig_class_type (i, _, _)
    | Constructor, Sig_exception (i, _)
      -> Some i
    | _ -> None

let sig_item_descr = function
    | Sig_value (_, d) -> DValue d
    | Sig_type (_, d, _) -> DType d
    | Sig_module (_, d, _) -> DModule d
    | Sig_modtype (_, d) -> DModtype d
    | Sig_class (_, d, _) -> DClass d
    | Sig_class_type (_, d, _) -> DCltype d
    | Sig_exception (_, cstr_args) ->
      DConstructor {cstr_args = cstr_args ; cstr_full = None}

(* Get the ident of a summary item, if it has one, and matches the kind. *)
let summary_item kind item =
  match kind, item with
    | Value, Env_value (_, i, _)
    | Type, Env_type (_, i, _)
    | Module, Env_module (_, i, _)
    | Modtype, Env_modtype (_, i, _)
    | Class, Env_class (_, i, _)
    | Cltype, Env_cltype (_, i, _)
    | Constructor, Env_exception (_, i, _)
      -> Some i
    | _ -> None

let summary_item_descr = function
  | Env_value (e, _, d) -> e, DValue d
  | Env_type (e, _, d) -> e, DType d
  | Env_module (e, _, d) -> e, DModule d
  | Env_modtype (e, _, d) -> e, DModtype d
  | Env_class (e, _, d) -> e, DClass d
  | Env_cltype (e, _, d) -> e, DCltype d
  | Env_exception (e, _, cstr_args) ->
    e, DConstructor {cstr_args = cstr_args ; cstr_full = None}
  | Env_open _ | Env_empty -> invalid_arg "summary_item_descr"

(*
let parse_lid kind =
  match kind with
    | Value ->
	(function s ->
	   try parse_string Parser.val_longident s
	   with _ -> Longident.Lident (parse_string Parser.operator s, Location.none))
    | Type -> parse_string Parser.type_longident
    | Module -> parse_string Parser.mod_ext_longident
    | Modtype -> parse_string Parser.mty_longident
    | Constructor -> parse_string Parser.constr_longident
    | Label -> parse_string Parser.label_longident
    | Class -> parse_string Parser.class_longident
    | Cltype -> parse_string Parser.clty_longident
    | Annot -> invalid_arg "parse_lid"
*)
let parse_lid _ s = Longident.parse s

exception Abstract_modtype

let find_modtype p env =
  match find_modtype p env with
    | Modtype_abstract -> raise Abstract_modtype
    | Modtype_manifest mt -> mt

let rec resolve_modtype kind find env p =
  debugln "resolving module type%s %s (%s)"
    (match kind with
      | Module -> " of"
      | Modtype -> ""
      | _ -> assert false)
    (name p)
    (try Ident.name_with_ctx (Path.head p)
     with _ -> "");
  match p with
    | (*Pident*) _ ->
      let mt = checked_find kind find p env in
      modtype env mt
and modtype env mt =
  let mt =
    match mt with
      | Mty_ident p ->
        resolve_modtype Modtype find_modtype env p
      | Mty_signature s -> `sign s
      | Mty_functor (id, t, t') -> `func (id, t, t')
  in
  debugln "modtype resolves to a %ssignature"
    (match mt with
      | `func _ -> "functor "
      | `sign _ -> "");
    mt

(*
let rec resolve_modtype kind find env gp =
  let ctx, mt =
    wrap_lookup
      (function p -> fail_owz "unbound %s %s" (kind2string kind) (name p.p_id))
      find gp env
  in
  match ctx
*)

let modtype_signature env m =
  match modtype env m with
    | `sign s -> s
    | `func _ -> invalid_arg "modtype_signature"

let module_lid2sig env pref =
  let _, t = checked_lookup_module pref env in
  modtype_signature env t


let modtype_functor env m =
  match modtype env m with
    | `func f -> f
    | `sign _ -> invalid_arg "modtype_signature"

let resolve_module env = resolve_modtype Module find_module env

let resolve_module env path =
  match resolve_module env path with
    | `sign s -> s
    | `func _ -> invalid_arg "modtype_signature"

exception Found of int * Ident.t * description
exception AmbiguousOrder of string array * (int * (Path.t * Types.module_type)) list

(* We assume that the kind is correct *)
let first_of_in_id names id d =
  let name = Ident.name id in
  Array.iteri
    (fun i n ->
      if name = n then
	raise (Found (i, id, d)))
    names

(* The type itself is excluded *)
let first_of_in_type_decl kind names tdecl =
  match kind, tdecl.type_kind with
    | Constructor, Type_variant constrs ->
      List.iter
	(function id, cstr_args, _ ->
          first_of_in_id names id (DConstructor
                                     {cstr_args = cstr_args ; cstr_full = None}))
	constrs
    | Label, Type_record (fields, _) ->
      List.iter
	(function id, lbl_mut, lbl_arg ->
          first_of_in_id names id (DLabel {lbl_mut = lbl_mut;
                                           lbl_arg = lbl_arg ;
                                           lbl_full = None}))
	fields
    | _ -> ()

let first_of_in_sig kind names sg =
  List.iter
    (function item ->
      (match sig_item kind item with
	| Some id ->
	  first_of_in_id names id (sig_item_descr item)
	| None -> ());
      (match item with
	| Sig_type (s, tdecl, _) ->
	  first_of_in_type_decl kind names tdecl
	| _ -> ()))
    (List.rev sg)

let find first_of kind name arg =
  try
    first_of kind [|name|] arg;
    raise Not_found
  with
    | Found (0, id, d) -> id, d
    | Found _ -> assert false

let find_gid_in_signature = find first_of_in_sig

let find_in_signature kind name sg =
  fst (find_gid_in_signature kind name sg)

let first_of_in_persistent kind names env =
  if kind = Module then
    let found = ref [] in
    Array.iteri
      (fun i modname ->
	try
          let p = lookup_module (Longident.Lident modname) env in
	  found := (i, p) :: !found
	with Not_found ->
	  ())
      names;
    match !found with
      | [] -> raise Not_found
      | [i, (Pident id, mt)] ->
        raise (Found (i, id, DModule mt))
      | _ -> raise (AmbiguousOrder (names, !found))

let rec first_of kind names env = function
  | Env_empty -> first_of_in_persistent kind names env
  | Env_open (s, p) ->
    let sign =
      try resolve_module env p
      with Not_found ->
        assert false
(* This should be [Unbound] now
        Printf.ksprintf failwith "first_of: module %s not found in path\n  %s"
          (name p)
          (String.concat "\n  "
             !Config.load_path)
*)
    in
    first_of_in_sig kind names sign;
    first_of kind names env s
  | summary ->
    (match summary_item kind summary with
      | Some id ->
        let e, d = summary_item_descr summary in
	  first_of_in_id names id d
      | None -> ());
    (match summary with
      | Env_type (s, _, tdecl) ->
	first_of_in_type_decl kind names tdecl
      | _ -> ());
    match summary with
      | Env_value (s, _, _)
      | Env_type (s, _, _)
      | Env_exception (s, _, _)
      | Env_module (s, _, _)
      | Env_modtype (s, _, _)
      | Env_class (s, _, _)
      | Env_cltype (s, _, _)
	-> first_of kind names env s
      | Env_open _ | Env_empty -> assert false

and find_in_env kind = find first_of kind

let add_environments env sg =
  let _, sg =
    List.fold_left
      (fun (env, sg) item -> add_item item env, (env, item) :: sg)
      (env, [])
      sg
  in
  List.rev sg

let lookup_in_signature kind name sg =
  if kind = Module || kind = Modtype then
    List.find
      (function _, item -> match sig_item kind item with
	| Some id -> Ident.name id = name
	| None -> false)
      sg
  else
    invalid_arg "lookup_in_signature"

let resolve_member kind env path name =
  debugln "resolve_member %s . %s" (Path.name path) name;
  let sg = resolve_module env path in
  let gid, _ = find_gid_in_signature kind name sg in
  (*
    assert (not (Ident.persistent id));
  *)
  debugln "member resolves to %s" (Ident.name gid);
  gid

(* True if p.name means id *)
let member_resolves_to kind env path name ids =
  try
    List.mem (resolve_member kind env path name) ids
  with
    | Not_found -> false

let resolve_path kind path env =
  match path with
    | Pident id -> id
    | Pdot (p, n, _) ->
      debugln "lookup yields a member of %s" (name p);
      resolve_member kind env p n
    | Papply _ -> invalid_arg "resolves_lid"

let resolve_lid kind env lid =
  let path, _ = checked_untyped_lookup kind lid env in
  resolve_path kind path env

(* Test whether a p reffers to id in environment env. This indicates
   that the rightmost name in lid needs renaming. *)
let resolves_to kind env lid ids =
  try
    List.mem (resolve_lid kind env lid) ids
  with
    | Not_found -> false

exception Masked_by of bool * Ident.t

(* Check that the renaming of one of ids in name is not masked in the env. *)

let check_in ~renamed first_of ~ids ~new_name =
  let old_name = Ident.name (List.hd ids) in
  try
    ignore (first_of [|old_name ; new_name|]);
    invalid "check: %s not found" (if renamed then old_name else new_name)
  with
    | Found (i, id, _) -> (
      match renamed, i with
	| (true, 0 | false, 1) -> ()
	| (true, 1 | false, 0) ->
	    (* for a non-renamed existing occurrence of new_name, we check that
	       the found definition of old_name actually points to ids. *)
	  if renamed || List.mem id ids then
	    raise (Masked_by (renamed, id))
	| _ -> assert false
    )

let check kind (env, summary) ~renamed ~ids ~new_name =
  check_in ~ids ~new_name ~renamed
    (function names -> first_of kind names env summary)

and check_in_sig kind sg ~renamed ~ids ~new_name =
  check_in ~ids ~new_name ~renamed
    (function names -> first_of_in_sig kind names sg)

open Longident
(* shortest_path cond p returns (cond p') for the shortest sub-path of
   p (inclusive) such that (cond p') does not raise Not_found. *)
let rec shortest_path cond = function
  | Lident _ as lid -> cond lid
  | Ldot (p, id) -> (
    try shortest_path cond (Lident (id))
    with Not_found ->
      shortest_path
	(fun p -> cond (Ldot (p, id)))
	p
  )
  | Lapply (p, p') ->
    shortest_path
      (function p ->
	shortest_path
	  (function p' -> cond (Lapply (p, p')))
	  p')
      p

let rev_lookup_id kind env lid res =
  shortest_path
    (function lid ->
      let id = resolve_lid kind env lid in
      if id = res then lid else raise Not_found)
    lid

let rev_lookup kind env p =
  let lid = Untypeast.lident_of_path p in
  shortest_path
    (function lid ->
      if lookup kind lid env = p then
	lid
      else
	raise Not_found)
    lid

(*
let rev_lookup_path kind env ctx p res =
  let lid = Untypeast.lident_of_path p
  and id, _ =
  rev_lookup kind env ctx lid res
*)

