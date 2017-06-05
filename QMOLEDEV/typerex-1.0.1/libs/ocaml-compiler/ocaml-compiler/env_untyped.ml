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
open Env_fold

(* Force the linking of Includemod, to fill the corward declaration in Env. *)
let _ = Includemod.modtypes

type path_sort =
  | Value
  | Annot
  | Constructor
  | Label
  | Type
  | Module
  | Modtype
  | Class
  | Cltype

let kind2string = function
  | Value -> "value"
  | Type -> "type"
  | Annot -> "annot"
  | Constructor -> "constructor"
  | Label -> "label"
  | Module -> "module"
  | Modtype -> "module type"
  | Class -> "class"
  | Cltype -> "class type"

let kind2char = function
  | Value -> 'v'
  | Type -> 't'
  | Annot -> 'a'
  | Constructor -> 'c'
  | Label -> 'f'
  | Module -> 'm'
  | Modtype -> 'M'
  | Class -> 'c'
  | Cltype -> 'C'

let string2kind = function
  | "value" | "val" -> Value
  | "type" -> Type
  | "annot" -> Annot
  | "constructor" | "variant" -> Constructor
  | "label" | "field" -> Label
  | "module" -> Module
  | "module type" | "modtype" -> Modtype
  | "class" -> Class
  | "class type" | "cltype" -> Cltype
  | _ -> invalid_arg "string2kind"

type constructor_description = {
  cstr_args: Types.type_expr list;
    (* Type of the arguments *)
  cstr_full: Types.constructor_description option
    (* full description if available *)
}

type label_description = {
  lbl_arg: Types.type_expr;
    (* Type of the argument *)
  lbl_mut: Asttypes.mutable_flag;
    (* Is this a mutable field? *)
  lbl_full: Types.label_description option
    (* full description if available *)
}

(** The type of descriptions associated with the various kinds of idents *)
type description =
  | DValue of Types.value_description
  | DConstructor of constructor_description (** exceptions too *)
  | DLabel of label_description
  | DType of Types.type_declaration
  | DModule of Types.module_type
  | DModtype of Types.modtype_declaration
  | DClass of Types.class_declaration
  | DCltype of Types.class_type_declaration

let untyped_lookup =
  let wrap lookup ret lid env =
    let path, desc = lookup lid env in path, ret desc in
  function
    | Value -> wrap lookup_value (function v -> DValue v)
    | Type -> wrap lookup_type (function v -> DType v)
    | Module -> wrap lookup_module (function v -> DModule v)
    | Constructor ->
      wrap lookup_constructor
        (function v ->
          DConstructor {cstr_args = v.Types.cstr_args ; cstr_full = Some v})
    | Label ->
      wrap lookup_label
        (function v ->
          DLabel {
            lbl_arg = v.Types.lbl_arg ; lbl_mut = v.Types.lbl_mut;
            lbl_full = Some v})
    | Modtype -> wrap lookup_modtype (function v -> DModtype v)
    | Class -> wrap lookup_class (function v -> DClass v)
    | Cltype -> wrap lookup_cltype (function v -> DCltype v)
    | Annot -> assert false

let lookup kind lid env = fst (untyped_lookup kind lid env)

let wrap fold wrap_desc f env acc =
  fold
    (fun name path desc acc -> f name path (wrap_desc desc) acc)
    env
    acc

let fold f = function
  | Value -> wrap fold_values (function d -> DValue d) f
  | Annot -> assert false
  | Constructor ->
    wrap fold_constructors
      (function {Types.cstr_args = cstr_args} as d ->
        DConstructor {cstr_args = cstr_args; cstr_full = Some d}) f
  | Label ->
    wrap fold_labels
      (function {Types.lbl_arg = lbl_arg; lbl_mut = lbl_mut} as d ->
        DLabel {lbl_arg = lbl_arg; lbl_mut = lbl_mut ;
                lbl_full = Some d}) f
  | Type -> wrap fold_types (function d -> DType d) f
  | Module -> wrap fold_modules (function d -> DModule d) f
  | Modtype -> wrap fold_modtypes (function d -> DModtype d) f
  | Class -> wrap fold_classs (function d -> DClass d) f
  | Cltype -> wrap fold_cltypes (function d -> DCltype d) f

let all_kinds = [
  Value
                (* ; Annot
                *)
  ; Constructor
  ; Label
  ; Class
  ; Cltype
  ; Type
  ; Module
  ; Modtype
]

let fold_all f env lid =
  List.fold_right
    (fun kind -> fold (f kind) kind lid env)
    all_kinds

let decl2string path desc =
  let name = Path.name path in
  let ident =  (Ident.create name) in
  let fmt = Format.str_formatter in
  (match desc with
    | DValue d -> Printtyp.value_description ident fmt d
    | DConstructor d ->
      Format.fprintf fmt "constructor %s" name;
      (match d.cstr_args with
        | [] -> Format.fprintf fmt " (constant)"
        | [arg] ->
          Format.fprintf fmt " of %a" Printtyp.type_expr arg
        | arg :: args ->
          Format.fprintf fmt " of %a" Printtyp.type_expr arg;
          List.iter (Format.fprintf fmt " * %a" Printtyp.type_expr) args);
      (match d.cstr_full with
        | Some d ->
          Format.fprintf fmt "@\nof type %a" Printtyp.type_expr d.Types.cstr_res
        | None -> ())
    | DLabel d ->
      if d.lbl_mut = Asttypes.Mutable then Format.fprintf fmt "mutable ";
      Format.fprintf fmt "field %s : " name;
      Printtyp.type_expr fmt d.lbl_arg;
      (match d.lbl_full with
        | Some d ->
          Format.fprintf fmt "@\nof type %a" Printtyp.type_expr d.Types.lbl_res
        | None -> ())
    | DType d -> Printtyp.type_declaration ident fmt d
    | DModule d ->
      Format.fprintf fmt "module %s : %a" name Printtyp.modtype d
    | DModtype d -> Printtyp.modtype_declaration ident fmt d
    | DClass d -> Printtyp.class_declaration ident fmt d
    | DCltype d -> Printtyp.cltype_declaration ident fmt d);
  Format.flush_str_formatter ()
