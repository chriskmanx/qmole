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
open Types
open Location
open Env_untyped

open Asttypes
open Typedtree
open TypedtreeIter
open Env_untyped

type typedtree = [ `structure of structure | `signature of signature]

type node = [
  typedtree
| `value_description of value_description
| `type_declaration of type_declaration
| `exception_declaration of exception_declaration
| `pattern of pattern
| `expression of expression
| `package_type of package_type
| `signature_item of signature_item
| `modtype_declaration of modtype_declaration
| `module_type of module_type
| `module_expr of module_expr
| `with_constraint of with_constraint
| `class_expr of class_expr
| `class_signature of class_signature
| `class_declaration of class_declaration
| `class_description of class_description
| `class_type_declaration of class_type_declaration
| `class_type of class_type
| `class_type_field of class_type_field
| `core_type of core_type
| `core_field_type of core_field_type
| `class_structure of class_structure
| `class_field of class_field
| `structure_item of structure_item
| `binding of pattern * expression
| `bindings of Asttypes.rec_flag
]

let node_kind = function
  | `structure _ -> "structure"
  | `value_description _ -> "value_description"
  | `type_declaration _ -> "type_declaration"
  | `exception_declaration _ -> "exception_declaration"
  | `pattern _ -> "pattern"
  | `expression _ -> "expression"
  | `package_type _ -> "package_type"
  | `signature _ -> "signature"
  | `signature_item _ -> "signature_item"
  | `modtype_declaration _ -> "modtype_declaration"
  | `module_type _ -> "module_type"
  | `module_expr _ -> "module_expr"
  | `with_constraint _ -> "with_constraint"
  | `class_expr _ -> "class_expr"
  | `class_signature _ -> "class_signature"
  | `class_declaration _ -> "class_declaration"
  | `class_description _ -> "class_description"
  | `class_type_declaration _ -> "class_type_declaration"
  | `class_type _ -> "class_type"
  | `class_type_field _ -> "class_type_field"
  | `core_type _ -> "core_type"
  | `core_field_type _ -> "core_field_type"
  | `class_structure _ -> "class_structure"
  | `class_field _ -> "class_field"
  | `structure_item _ -> "structure_item"
  | `binding _ -> "binding"
  | `bindings _ -> "bindings"

module MakeIterator
  (Arg : IteratorArgument) = struct

    include MakeIterator (Arg)

    let process = function
      | `structure s -> iter_structure s
      | `signature s -> iter_signature s
      | `structure_item s -> iter_structure_item s
      | `signature_item s -> iter_signature_item s
      | `expression s -> iter_expression s
      | `module_type s -> iter_module_type s
      | `pattern s -> iter_pattern s
      | `class_expr s -> iter_class_expr s
      | s -> failwith ("iter not implemented for " ^ node_kind s)

end

let iterator ~enter ~leave =
  let module Iterator = MakeIterator(struct

    let enter_structure x = enter (`structure x)
    let enter_value_description x = enter (`value_description x)
    let enter_type_declaration x = enter (`type_declaration x)
    let enter_exception_declaration x = enter (`exception_declaration x)
    let enter_pattern x = enter (`pattern x)
    let enter_expression x = enter (`expression x)
    let enter_package_type x = enter (`package_type x)
    let enter_signature x = enter (`signature x)
    let enter_signature_item x = enter (`signature_item x)
    let enter_modtype_declaration x = enter (`modtype_declaration x)
    let enter_module_type x = enter (`module_type x)
    let enter_module_expr x = enter (`module_expr x)
    let enter_with_constraint x = enter (`with_constraint x)
    let enter_class_expr x = enter (`class_expr x)
    let enter_class_signature x = enter (`class_signature x)
    let enter_class_declaration x = enter (`class_declaration x)
    let enter_class_description x = enter (`class_description x)
    let enter_class_type_declaration x = enter (`class_type_declaration x)
    let enter_class_type x = enter (`class_type x)
    let enter_class_type_field x = enter (`class_type_field x)
    let enter_core_type x = enter (`core_type x)
    let enter_core_field_type x = enter (`core_field_type x)
    let enter_class_structure x = enter (`class_structure x)
    let enter_class_field x = enter (`class_field x)
    let enter_structure_item x = enter (`structure_item x)
    let enter_binding x y = enter (`binding (x, y))
    let enter_bindings x = enter (`bindings x)

    let leave_structure x = leave (`structure x)
    let leave_value_description x = leave (`value_description x)
    let leave_type_declaration x = leave (`type_declaration x)
    let leave_exception_declaration x = leave (`exception_declaration x)
    let leave_pattern x = leave (`pattern x)
    let leave_expression x = leave (`expression x)
    let leave_package_type x = leave (`package_type x)
    let leave_signature x = leave (`signature x)
    let leave_signature_item x = leave (`signature_item x)
    let leave_modtype_declaration x = leave (`modtype_declaration x)
    let leave_module_type x = leave (`module_type x)
    let leave_module_expr x = leave (`module_expr x)
    let leave_with_constraint x = leave (`with_constraint x)
    let leave_class_expr x = leave (`class_expr x)
    let leave_class_signature x = leave (`class_signature x)
    let leave_class_declaration x = leave (`class_declaration x)
    let leave_class_description x = leave (`class_description x)
    let leave_class_type_declaration x = leave (`class_type_declaration x)
    let leave_class_type x = leave (`class_type x)
    let leave_class_type_field x = leave (`class_type_field x)
    let leave_core_type x = leave (`core_type x)
    let leave_core_field_type x = leave (`core_field_type x)
    let leave_class_structure x = leave (`class_structure x)
    let leave_class_field x = leave (`class_field x)
    let leave_structure_item x = leave (`structure_item x)
    let leave_binding x y = leave (`binding (x, y))
    let leave_bindings x = leave (`bindings x)

  end)
  in
  Iterator.process

let find_all_map cond s =
  let l = ref [] in
  let enter x =
    match cond x with
      | Some x -> l := x :: !l
      | None -> ()
  and leave _ = () in
  iterator ~enter ~leave s;
  List.rev !l

let find_map priority cond s =
  let result = ref None in
  let visit x =
    match cond x with
      | Some x -> result := Some x; raise Exit
      | None -> ()
  in
  let enter, leave = match priority with
    | `innermost -> ignore, visit
    | `outermost -> visit, ignore
  in
  try
    iterator ~leave ~enter s;
    raise Not_found
  with
      Exit ->
        match !result with None -> assert false | Some x -> x

let find_map_pattern priority cond =
  find_map priority (function `pattern p -> cond p | _ -> None)

let find_map_expression priority cond =
  find_map priority (function `expression e -> cond e | _ -> None)

let apply2paths f =
 function
    | `pattern {pat_desc = pat_desc; pat_env = env} ->
      (match pat_desc with
	| Tpat_construct ( p, text, _, _) -> f env Constructor p text
	| Tpat_record (fs, _) ->
	  List.iter (function ( l, text, _, _) -> f env Label l text) fs
	| Tpat_alias (_, TPat_type (p, text))-> f env Type p text
	| _ -> ())
    | `expression {exp_desc = exp_desc ; exp_env = env; exp_loc = exp_loc} ->
      (match exp_desc with
	| Texp_ident (p, text, _) -> f env Value p text
	| Texp_construct (p, text, _, _) -> f env Constructor p text
	| Texp_record (fs, _) ->
	  List.iter (function l, text, _, _ -> f env Label l text) fs
	| Texp_field (_, p, text, _) | Texp_setfield (_, p, text, _, _) ->
	  f env Label p text
	| Texp_new (p, text, _) -> f env Class p text
	| Texp_instvar (self, p, text)
	| Texp_setinstvar (self, p, text, _) ->
          ()
(* Fabrice:  TODO (this one and the next one)
   removed since text is "string loc" while expecting "Longident.t loc"
            found env Value text p]
*)
	| Texp_override ( self, ps) ->
(*
	  let self_env =
	    match Env.find_value self env with
	      | {Types.val_kind = Types.Val_self (_, vars, cl_num, t)} ->
		Types.Vars.fold (* Inexact *)
		  (fun _ (id, mut, _, _) env ->
		    Env.add_value id
		      {Types.val_type = t ; val_kind = Types.Val_ivar (mut, cl_num)}
		      env)
		  !vars
		  env
	      | _ -> assert false
	  in
(* TODO CHECK	  (found env Value text self ) :: *)
	    List.map
	    (function p, text, _ -> found self_env Value text p)
	    ps
*)
          ()
	| Texp_open (p, text, _) -> f env Module p text
	| _ -> ())
    | `class_expr {cl_desc = Tcl_ident (p, text, _) ; cl_env = cl_env} ->
      f cl_env Class p text
    | `module_expr {mod_desc = Tmod_ident (p, text) ; mod_env = mod_env;
                    mod_loc = mod_loc} ->
      f mod_env Module p text
    | `structure_item {str_desc = str_desc ; str_env = env} ->
      (match str_desc with
	| Tstr_exn_rebind (_, _, p, text) -> f env Constructor p text
	| Tstr_open (p, text) -> f env Module p text
	| _ -> ())
    | `module_type {mty_desc = mty_desc; mty_env = env ;
                    mty_type = mty_type; mty_loc = mty_loc } ->
      (match mty_desc with
	| Tmty_ident (p, text) -> f env Modtype p text
	| Tmty_with (_, ps) ->
	  let sg_env = match mty_type with
	    | Types.Mty_signature sg -> Env.add_signature sg env (* Inexact *)
	    | _ -> assert false
	  in
	  List.iter
            (function p, text, c ->
	      match c with
		| Twith_type _ | Twith_typesubst _ -> f sg_env Type p text
		| Twith_module (p', text') | Twith_modsubst (p', text') ->
		  f sg_env Module p text ; f env Module p' text')
	    ps
	| _ -> ())
    | `signature_item {sig_desc = Tsig_open (p, text) ; sig_env = sig_env} ->
      f sig_env Module p text
    | `core_type {ctyp_desc = ctyp_desc; ctyp_env = env} ->
      (match ctyp_desc with
	| Ttyp_constr (p, text, _) -> f env Type p text
	| Ttyp_class (p, text, _, _) -> f env Type p text
	| Ttyp_package pt -> f env Modtype pt.pack_name pt.pack_txt
	| _ -> ())
    | `class_type {cltyp_desc = Tcty_constr (p, text, _) ; cltyp_env = cltyp_env } ->
      f cltyp_env Cltype p text
    | _ -> ()

let iter_paths ~enter ~leave =
  iterator ~enter:(apply2paths enter) ~leave:(apply2paths leave)

let find_all_map_paths cond s =
  let l = ref [] in
  let enter env kind text p =
    match cond env kind text p with
      | Some x -> l := x :: !l
      | None -> ()
  and leave _ _ _ _ = () in
  iter_paths ~enter ~leave s;
  List.rev !l

let find_all_paths ?(keep_ghosts=false) s =
  find_all_map_paths
    (fun env kind p text ->
      if keep_ghosts || not text.loc.Location.loc_ghost then
        Some (text, p, (env, kind))
      else
        None)
    s
(* Missing:
   - cstr_meths,
   - Tmeth_val: self#id
   - Tcf_inher may bind an instance variable (e.g., "super")
   - Tcf_val binds values,
   - Tcf_let (unused) *)
let apply2defs f =
  function
  (* Values *)
  | `pattern {pat_desc =
      Tpat_var (id, text) |
          Tpat_alias (_, TPat_alias (id, text)) ; pat_type = pat_type}
      when Ident.name id <> "*opt*" ->
    let d = DValue { val_type = pat_type ; val_kind = Val_reg } in
    f Value id text d

  | `expression {exp_desc = Texp_for (id, text, _, _, _, _)} ->
    let d = DValue { val_type = Predef.type_int ; val_kind = Val_reg } in
    f Value id text d
  | `structure_item {str_desc = Tstr_primitive (id, text, d)}
  | `signature_item {sig_desc = Tsig_value (id, text, d)} ->
    f Value id text (DValue d.val_val)

  (* Missing in locate_map ! *)
  | `class_expr {cl_desc =
      Tcl_fun (_, _, bs, _, _) | Tcl_let (_, _, bs, _)} ->
    List.iter
      (function id, text, d ->
        f Value id text (DValue { val_type = d.exp_type ; val_kind = Val_reg }))
      bs

  (* Modules *)
  | `structure_item {str_desc = Tstr_module (id, text, {mod_type = t})}
  | `signature_item {sig_desc = Tsig_module (id, text, {mty_type = t})}
  | `module_expr {mod_desc = Tmod_functor (id, text, {mty_type = t}, _)}
  | `module_type {mty_desc = Tmty_functor (id, text, {mty_type = t}, _)}
  | `expression {exp_desc = Texp_letmodule (id, text, {mod_type = t}, _)} ->
    f Module id text (DModule t)

  | `structure_item {str_desc = Tstr_recmodule mods} ->
    List.iter
      (function id, text, t, _ -> f Module id text (DModule t.mty_type))
      mods

  | `signature_item {sig_desc = Tsig_recmodule mods} ->
    List.iter
      (function id, text, t -> f Module id text (DModule t.mty_type))
      mods

  (* Module types *)
  | `structure_item {str_desc = Tstr_modtype (id, text, {mty_type = mty_type})} ->
    f Modtype id text (DModtype (Modtype_manifest mty_type))
  | `signature_item {sig_desc = Tsig_modtype (id, text, d)} ->
    let d = match d with
      | Tmodtype_abstract -> Modtype_abstract
      | Tmodtype_manifest t -> Modtype_manifest t.mty_type
    in
    f Modtype id text (DModtype d)

  (* Types *)
  | `structure_item {str_desc = Tstr_type types}
  | `signature_item {sig_desc = Tsig_type types} ->
    List.iter (function id, text, d -> f Type id text (DType d.typ_type)) types

  (* Constructors, fields, and exceptions *)
  | `type_declaration d ->
    (match d.typ_type.type_kind, d.typ_kind with (* TODO: simplify *)
      | Type_variant cs, Ttype_variant tcs ->
	List.iter2
          (fun (_, args, _) (id, text, _, _) ->
            f Constructor id text
              (DConstructor {cstr_args = args ; cstr_full = None}))
          cs tcs
      | Type_record (fs, _), Ttype_record tfs ->
	List.iter2
          (fun (_, lbl_mut, lbl_arg) (id, text, _, _, _) ->
            f Label id text (DLabel {lbl_arg = lbl_arg; lbl_mut = lbl_mut ;
                                     lbl_full = None}))
          fs tfs
      | Type_abstract, Ttype_abstract -> ()
      | _ -> assert false)

  | `structure_item {str_desc = Tstr_exception (id, text, d)}
  | `signature_item {sig_desc = Tsig_exception (id, text, d)} ->
    let cstr_args = List.map (function t -> t.ctyp_type) d in
    let d = { cstr_args = cstr_args; cstr_full = None} in
    f Constructor id text (DConstructor d)

  | `structure_item
      {str_desc = Tstr_exn_rebind (id, text, p, lid) ; str_env = str_env} ->
    let _, d = Env.lookup_constructor lid.txt str_env in
    let d =
      { cstr_args = d.Types.cstr_args ; cstr_full = Some d} in
    f Constructor id text (DConstructor d)

  (* Class types *)
  | `class_type_declaration c ->
    f Cltype c.ci_id_class_type c.ci_id_name (DCltype c.ci_type_decl)

  (* Classes *)
  | `class_declaration {ci_id_name = ci_id_name; ci_id_class = ci_id_class ;
                        ci_decl = ci_decl}
  | `class_description {ci_id_name = ci_id_name ; ci_id_class = ci_id_class;
                        ci_decl = ci_decl} ->
    f Class ci_id_class ci_id_name (DClass ci_decl)

  | _ -> ()

let iter_ident_definitions ~enter ~leave =
  iterator ~enter:(apply2defs enter) ~leave:(apply2defs leave)

let find_all_map_ident_definitions cond s =
  let l = ref [] in
  let enter k text id desc =
    match cond k text id desc with
      | Some x -> l := x :: !l
      | None -> ()
  and leave _ _ _ _ = () in
  iter_ident_definitions ~enter ~leave s;
  List.rev !l
