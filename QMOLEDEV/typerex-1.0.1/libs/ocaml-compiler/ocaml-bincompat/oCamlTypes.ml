(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

module Tbl = struct
  type ('a, 'b) t = ('a, 'b) Tbl.t =
    Empty
  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int
end


module Lexing = struct

  type position = Lexing.position = {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

end

module Location = struct

  type t = Location.t = { loc_start: Lexing.position; loc_end: Lexing.position; loc_ghost: bool };;

  type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : t;
  }

end

module Asttypes = struct

  type constant = Asttypes.constant =
    Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

  type rec_flag = Asttypes.rec_flag = Nonrecursive | Recursive | Default

  type direction_flag = Asttypes.direction_flag = Upto | Downto

  type private_flag = Asttypes.private_flag = Private | Public

  type mutable_flag = Asttypes.mutable_flag = Immutable | Mutable

  type virtual_flag = Asttypes.virtual_flag = Virtual | Concrete

  type label = string

  type override_flag = Asttypes.override_flag = Override | Fresh
  type closed_flag = Asttypes.closed_flag = Closed | Open

  type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : Location.t;
  }

end

module Longident = struct

  type t = Longident.t =
      Lident of string
           | Ldot of t * string
           | Lapply of t * t

end

(*
module Longident = V3120_types.Longident
module Asttypes = struct
  include V3120_types.Asttypes
  type 'a loc = 'a Location.loc = {
    txt : 'a;
    loc : Location.t;
  }
end

(*
module Parsetree = struct
open Asttypes
type core_type =
  { ptyp_desc: core_type_desc;
    ptyp_loc: Location.t }

and core_type_desc =
    Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t loc * core_type list
  | Ptyp_object of core_field_type list
  | Ptyp_class of Longident.t loc * core_type list * label list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * bool * label list option
  | Ptyp_poly of string list * core_type
  | Ptyp_package of package_type

and package_type = Longident.t loc * (string loc * core_type) list

and core_field_type =
  { pfield_desc: core_field_desc;
    pfield_loc: Location.t }

and core_field_desc =
    Pfield of string * core_type
  | Pfield_var

and row_field =
    Rtag of label * bool * core_type list
  | Rinherit of core_type
type 'a class_infos =
  { pci_virt: virtual_flag;
    pci_params: string loc list * Location.t;
    pci_name: string loc;
    pci_expr: 'a;
    pci_variance: (bool * bool) list;
    pci_loc: Location.t }
type pattern =
  { ppat_desc: pattern_desc;
    ppat_loc: Location.t }

and pattern_desc =
    Ppat_any
  | Ppat_var of string loc
  | Ppat_alias of pattern * string loc
  | Ppat_constant of constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t loc * pattern option * bool
  | Ppat_variant of label * pattern option
  | Ppat_record of (Longident.t loc * pattern) list * closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of Longident.t loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string loc
type expression =
  { pexp_desc: expression_desc;
    pexp_loc: Location.t }

and expression_desc =
    Pexp_ident of Longident.t loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_function of label * expression option * (pattern * expression) list
  | Pexp_apply of expression * (label * expression) list
  | Pexp_match of expression * (pattern * expression) list
  | Pexp_try of expression * (pattern * expression) list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t loc * expression option * bool
  | Pexp_variant of label * expression option
  | Pexp_record of (Longident.t loc * expression) list * expression option
  | Pexp_field of expression * Longident.t loc
  | Pexp_setfield of expression * Longident.t loc * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of string loc *  expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type option * core_type option
  | Pexp_when of expression * expression
  | Pexp_send of expression * string
  | Pexp_new of Longident.t loc
  | Pexp_setinstvar of string loc * expression
  | Pexp_override of (string loc * expression) list
  | Pexp_letmodule of string loc * module_expr * expression
  | Pexp_assert of expression
  | Pexp_assertfalse
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string * expression
  | Pexp_pack of module_expr
  | Pexp_open of Longident.t loc * expression

(* Value descriptions *)

and value_description =
  { pval_type: core_type;
    pval_prim: string list;
    pval_loc : Location.t
    }

(* Type declarations *)

and type_declaration =
  { ptype_params: string loc option list;
    ptype_cstrs: (core_type * core_type * Location.t) list;
    ptype_kind: type_kind;
    ptype_private: private_flag;
    ptype_manifest: core_type option;
    ptype_variance: (bool * bool) list;
    ptype_loc: Location.t }

and type_kind =
    Ptype_abstract
  | Ptype_variant of
      (string loc * core_type list * core_type option * Location.t) list
  | Ptype_record of
      (string loc * mutable_flag * core_type * Location.t) list

and exception_declaration = core_type list

(* Type expressions for the class language *)

and class_type =
  { pcty_desc: class_type_desc;
    pcty_loc: Location.t }

and class_type_desc =
    Pcty_constr of Longident.t loc * core_type list
  | Pcty_signature of class_signature
  | Pcty_fun of label * core_type * class_type

and class_signature = {
    pcsig_self : core_type;
    pcsig_fields : class_type_field list;
    pcsig_loc : Location.t;
  }

and class_type_field = {
    pctf_desc : class_type_field_desc;
    pctf_loc : Location.t;
  }

and class_type_field_desc =
    Pctf_inher of class_type
  | Pctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Pctf_virt  of (string * private_flag * core_type)
  | Pctf_meth  of (string * private_flag * core_type)
  | Pctf_cstr  of (core_type * core_type)

and class_description = class_type class_infos

and class_type_declaration = class_type class_infos

(* Value expressions for the class language *)

and class_expr =
  { pcl_desc: class_expr_desc;
    pcl_loc: Location.t }

and class_expr_desc =
    Pcl_constr of Longident.t loc * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of label * expression option * pattern * class_expr
  | Pcl_apply of class_expr * (label * expression) list
  | Pcl_let of rec_flag * (pattern * expression) list * class_expr
  | Pcl_constraint of class_expr * class_type

and class_structure = {
    pcstr_pat : pattern;
    pcstr_fields :  class_field list;
  }

and class_field = {
    pcf_desc : class_field_desc;
    pcf_loc : Location.t;
  }

and class_field_desc =
    Pcf_inher of override_flag * class_expr * string option
  | Pcf_valvirt of (string loc * mutable_flag * core_type)
  | Pcf_val of (string loc * mutable_flag * override_flag * expression)
  | Pcf_virt  of (string loc * private_flag * core_type)
  | Pcf_meth of (string loc * private_flag *override_flag * expression)
  | Pcf_constr  of (core_type * core_type)
  | Pcf_let   of rec_flag * (pattern * expression) list
  | Pcf_init  of expression

and class_declaration = class_expr class_infos

(* Type expressions for the module language *)

and module_type =
  { pmty_desc: module_type_desc;
    pmty_loc: Location.t }

and module_type_desc =
    Pmty_ident of Longident.t loc
  | Pmty_signature of signature
  | Pmty_functor of string loc * module_type * module_type
  | Pmty_with of module_type * (Longident.t loc * with_constraint) list
  | Pmty_typeof of module_expr

and signature = signature_item list

and signature_item =
  { psig_desc: signature_item_desc;
    psig_loc: Location.t }

and signature_item_desc =
    Psig_value of string loc * value_description
  | Psig_type of (string loc * type_declaration) list
  | Psig_exception of string loc * exception_declaration
  | Psig_module of string loc * module_type
  | Psig_recmodule of (string loc * module_type) list
  | Psig_modtype of string loc * modtype_declaration
  | Psig_open of Longident.t loc
  | Psig_include of module_type
  | Psig_class of class_description list
  | Psig_class_type of class_type_declaration list

and modtype_declaration =
    Pmodtype_abstract
  | Pmodtype_manifest of module_type

and with_constraint =
    Pwith_type of type_declaration
  | Pwith_module of Longident.t loc
  | Pwith_typesubst of type_declaration
  | Pwith_modsubst of Longident.t loc

(* value expressions for the module language *)

and module_expr =
  { pmod_desc: module_expr_desc;
    pmod_loc: Location.t }

and module_expr_desc =
    Pmod_ident of Longident.t loc
  | Pmod_structure of structure
  | Pmod_functor of string loc * module_type * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression

and structure = structure_item list

and structure_item =
  { pstr_desc: structure_item_desc;
    pstr_loc: Location.t }

and structure_item_desc =
    Pstr_eval of expression
  | Pstr_value of rec_flag * (pattern * expression) list
  | Pstr_primitive of string loc * value_description
  | Pstr_type of (string loc * type_declaration) list
  | Pstr_exception of string loc * exception_declaration
  | Pstr_exn_rebind of string loc * Longident.t loc
  | Pstr_module of string loc * module_expr
  | Pstr_recmodule of (string loc * module_type * module_expr) list
  | Pstr_modtype of string loc * module_type
  | Pstr_open of Longident.t loc
  | Pstr_class of class_declaration list
  | Pstr_class_type of class_type_declaration list
  | Pstr_include of module_expr
end
*)
*)

module Primitive = struct

type description = Primitive.description =
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_float: bool }  (* Does the above operate on unboxed floats? *)

end

module Ident = struct

  type ctx = Ident.ctx = {
    modname : string;
    kind : [ `interface | `implementation ];
    source_digest : Digest.t
  }

  type t = Ident.t = { stamp: int; name: string; mutable flags: int ; ctx: ctx }

  type 'a tbl = 'a Ident.tbl =
      Empty
    | Node of 'a tbl * 'a data * 'a tbl * int

  and 'a data = 'a Ident.data =
      { ident: t;
        data: 'a;
        previous: 'a data option }

end

module Path = struct

type t = Path.t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

end

module Types = struct

open Asttypes
type type_expr = Types.type_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable id: int }

and type_desc = Types.type_desc =
    Tvar of string option
  | Tarrow of label * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr         (* for copying *)
  | Tvariant of row_desc
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * string list * type_expr list

and row_desc = Types.row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_bound: unit;
      row_closed: bool;
      row_fixed: bool;
      row_name: (Path.t * type_expr list) option }

and row_field = Types.row_field =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

and abbrev_memo = Types.abbrev_memo =
    Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref

and field_kind = Types.field_kind =
    Fvar of field_kind option ref
  | Fpresent
  | Fabsent

and commutable = Types.commutable =
    Cok
  | Cunknown
  | Clink of commutable ref

module TypeOps = struct
  type t = type_expr
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

module OrderedString = Types.OrderedString
module Meths = Types.Meths
module Vars = Types.Vars

type value_description = Types.value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind }

and value_kind = Types.value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of (Ident.t * type_expr) Meths.t ref *
                (Ident.t * Asttypes.mutable_flag *
                 Asttypes.virtual_flag * type_expr) Vars.t ref *
                string * type_expr
                                        (* Self *)
  | Val_anc of (string * Ident.t) list * string
                                        (* Ancestor *)
  | Val_unbound                         (* Unbound variable *)

type constructor_description = Types.constructor_description =
  { cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_normal: int;                   (* Number of non generalized constrs *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag }        (* Read-only constructor? *)

and constructor_tag = Types.constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_exception of Path.t            (* Exception constructor *)

type label_description = Types.label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag }         (* Read-only field? *)

and record_representation = Types.record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)

type type_declaration = Types.type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_kind;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: (bool * bool * bool) list;
    type_newtype_level: int option }
            (* covariant, contravariant, weakly contravariant *)

and type_kind = Types.type_kind =
    Type_abstract
  | Type_record of
      (Ident.t * mutable_flag * type_expr) list * record_representation
  | Type_variant of (Ident.t * type_expr list * type_expr option) list

type exception_declaration = type_expr list

module Concr = Types.Concr

type class_type = Types.class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_fun of label * type_expr * class_type

and class_signature = Types.class_signature =
  { cty_self: type_expr;
    cty_vars:
      (Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr) Vars.t;
    cty_concr: Concr.t;
    cty_inher: (Path.t * type_expr list) list }

type class_declaration = Types.class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: (bool * bool) list }

type class_type_declaration = Types.class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_variance: (bool * bool) list }

type module_type = Types.module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of Ident.t * module_type * module_type

and signature = signature_item list

and signature_item = Types.signature_item =
    Sig_value of Ident.t * value_description
  | Sig_type of Ident.t * type_declaration * rec_status
  | Sig_exception of Ident.t * exception_declaration
  | Sig_module of Ident.t * module_type * rec_status
  | Sig_modtype of Ident.t * modtype_declaration
  | Sig_class of Ident.t * class_declaration * rec_status
  | Sig_class_type of Ident.t * class_type_declaration * rec_status

and modtype_declaration = Types.modtype_declaration =
    Modtype_abstract
  | Modtype_manifest of module_type

and rec_status = Types.rec_status =
    Trec_not                            (* not recursive *)
  | Trec_first                          (* first in a recursive group *)
  | Trec_next                           (* not first in a recursive group *)
end

(*
module Annot = V3120_types.Annot

module Subst = struct
open Types
type t =
  { types: (Ident.t, Path.t) Tbl.t;
    modules: (Ident.t, Path.t) Tbl.t;
    modtypes: (Ident.t, module_type) Tbl.t;
    for_saving: bool }
end
*)

module Env = struct

  type pers_flags = Env.pers_flags = Rectypes

end

(*

module EnvLazy = struct
  type 'a t
  type ('a,'b) maker
end

open Types
type summary =
    Env_empty
  | Env_value of summary * Ident.t * value_description
  | Env_type of summary * Ident.t * type_declaration
  | Env_exception of summary * Ident.t * exception_declaration
  | Env_module of summary * Ident.t * module_type
  | Env_modtype of summary * Ident.t * modtype_declaration
  | Env_class of summary * Ident.t * class_declaration
  | Env_cltype of summary * Ident.t * class_type_declaration
  | Env_open of summary * Path.t
type t = {
  values: (Path.t * value_description) Ident.tbl;
  annotations: (Path.t * Annot.ident) Ident.tbl;
  constrs: (Path.t * constructor_description) Ident.tbl;
  labels: (Path.t * label_description) Ident.tbl;
  constrs_by_path: (Path.t * (constructor_description list)) Ident.tbl;
  types: (Path.t * type_declaration) Ident.tbl;
  modules: (Path.t * module_type) Ident.tbl;
  modtypes: (Path.t * modtype_declaration) Ident.tbl;
  components: (Path.t * module_components) Ident.tbl;
  classes: (Path.t * class_declaration) Ident.tbl;
  cltypes: (Path.t * class_type_declaration) Ident.tbl;
  summary: summary;
  local_constraints: bool;
  level_map: (int * int) list;
}

and module_components = module_components_repr EnvLazy.t

and module_components_repr =
    Structure_comps of structure_components
  | Functor_comps of functor_components

and structure_components = {
  mutable comp_values: (string, (value_description * int)) Tbl.t;
  mutable comp_annotations: (string, (Annot.ident * int)) Tbl.t;
  mutable comp_constrs: (string, (constructor_description * int)) Tbl.t;
  mutable comp_labels: (string, (label_description * int)) Tbl.t;
  mutable comp_constrs_by_path:
      (string, (constructor_description list * int)) Tbl.t;
  mutable comp_types: (string, (type_declaration * int)) Tbl.t;
  mutable comp_modules: (string, (module_type EnvLazy.t * int)) Tbl.t;
  mutable comp_modtypes: (string, (modtype_declaration * int)) Tbl.t;
  mutable comp_components: (string, (module_components * int)) Tbl.t;
  mutable comp_classes: (string, (class_declaration * int)) Tbl.t;
  mutable comp_cltypes: (string, (class_type_declaration * int)) Tbl.t
}

and functor_components = {
  fcomp_param: Ident.t;                 (* Formal parameter *)
  fcomp_arg: module_type;               (* Argument signature *)
  fcomp_res: module_type;               (* Result signature *)
  fcomp_env: t;     (* Environment in which the result signature makes sense *)
  fcomp_subst: Subst.t;  (* Prefixing substitution for the result signature *)
  fcomp_cache: (Path.t, module_components) Hashtbl.t  (* For memoization *)
}

type pers_flags = V3120_types.Env.pers_flags = Rectypes
end

module Typedtree = struct
open Asttypes
open Types
type partial = Partial | Total
type optional = Required | Optional
type pattern =
  { pat_desc: pattern_desc;
    pat_loc: Location.t;
    pat_type: type_expr;
    mutable pat_env: Env.t }

(* Instead of adding two new constructs (Tpat_constraint and Tpat_type),
we chose to change Tpat_alias, so that we don't have to modify too much
the pattern matching engine, and don't introduce bugs. *)

and alias_kind =
    TPat_alias of Ident.t * string loc
  | TPat_constraint of core_type
  | TPat_type of Path.t * Longident.t loc

and pattern_desc =
    Tpat_any
  | Tpat_var of Ident.t * string loc
  | Tpat_alias of pattern * alias_kind
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of Path.t * Longident.t loc * constructor_description * pattern list
  | Tpat_variant of label * pattern option * row_desc ref
  | Tpat_record of ( Path.t * Longident.t loc * label_description * pattern) list * closed_flag
  | Tpat_array of pattern list
  | Tpat_or of pattern * pattern * row_desc option
  | Tpat_lazy of pattern

and expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_type: type_expr;
    exp_env: Env.t }

and expression_desc =
    Texp_ident of Path.t * Longident.t loc * Types.value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of label * (pattern * expression) list * partial
  | Texp_apply of expression * (label * expression option * optional) list
  | Texp_match of expression * (pattern * expression) list * partial
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of Path.t * Longident.t loc * constructor_description * expression list
  | Texp_variant of label * expression option
  | Texp_record of (Path.t * Longident.t loc * label_description * expression) list * expression option
  | Texp_field of expression * Path.t * Longident.t loc * label_description
  | Texp_setfield of expression * Path.t * Longident.t loc * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * string loc * expression * expression * direction_flag * expression
  | Texp_constraint of expression * core_type option * core_type option
  | Texp_when of expression * expression
  | Texp_send of expression * meth * expression option
  | Texp_new of Path.t * Longident.t loc * Types.class_declaration
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_override of Path.t * (Path.t * string loc * expression) list
  | Texp_letmodule of Ident.t * string loc * module_expr * expression
  | Texp_assert of expression
  | Texp_assertfalse
  | Texp_lazy of expression
  | Texp_poly of expression * core_type option
  | Texp_object of class_structure * string list
  | Texp_newtype of string * expression
  | Texp_pack of module_expr
  | Texp_open of Path.t * Longident.t loc * expression

and meth =
    Tmeth_name of string
  | Tmeth_val of Ident.t

(* Value expressions for the class language *)

and class_expr =
  { cl_desc: class_expr_desc;
    cl_loc: Location.t;
    cl_type: Types.class_type;
    cl_env: Env.t }

and class_expr_desc =
    Tcl_ident of Path.t * Longident.t loc * core_type list
  | Tcl_structure of class_structure
  | Tcl_fun of label * pattern * (Ident.t * string loc * expression) list * class_expr * partial
  | Tcl_apply of class_expr * (label * expression option * optional) list
  | Tcl_let of rec_flag *  (pattern * expression) list *
                  (Ident.t * string loc * expression) list * class_expr
  | Tcl_constraint of class_expr * class_type option * string list * string list * Concr.t
    (* Visible instance variables, methods and concretes methods *)

and class_structure =
  {
    cstr_pat : pattern;
    cstr_fields: class_field list;
    cstr_type : Types.class_signature;
    cstr_meths: Ident.t Meths.t }

and class_field =
   {
    cf_desc : class_field_desc;
    cf_loc : Location.t;
  }

and class_field_kind =
  Tcfk_virtual of core_type
| Tcfk_concrete of expression

and class_field_desc =
  Tcf_inher of override_flag * class_expr * string option * (string * Ident.t) list * (string * Ident.t) list
    (* Inherited instance variables and concrete methods *)
  | Tcf_val of string * string loc * mutable_flag * Ident.t * class_field_kind * bool
        (* None = virtual, true = override *)
  | Tcf_meth of string * string loc * private_flag * class_field_kind * bool
  | Tcf_constr of core_type * core_type
  | Tcf_let of rec_flag * (pattern * expression) list *
              (Ident.t * string loc * expression) list
  | Tcf_init of expression

(* Value expressions for the module language *)

and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: Types.module_type;
    mod_env: Env.t }

and module_type_constraint =
  Tmodtype_implicit
| Tmodtype_explicit of module_type

and module_expr_desc =
    Tmod_ident of Path.t * Longident.t loc
  | Tmod_structure of structure
  | Tmod_functor of Ident.t * string loc * module_type * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of module_expr * Types.module_type * module_type_constraint * module_coercion
  | Tmod_unpack of expression * Types.module_type

and structure = {
  str_items : structure_item list;
  str_type : Types.signature;
}

and structure_item =
  { str_desc : structure_item_desc;
    str_loc : Location.t;
    str_env : Env.t
  }

and structure_item_desc =
    Tstr_eval of expression
  | Tstr_value of rec_flag * (pattern * expression) list
  | Tstr_primitive of Ident.t * string loc * value_description
  | Tstr_type of (Ident.t * string loc * type_declaration) list
  | Tstr_exception of Ident.t * string loc * exception_declaration
  | Tstr_exn_rebind of Ident.t * string loc * Path.t * Longident.t loc
  | Tstr_module of Ident.t * string loc * module_expr
  | Tstr_recmodule of (Ident.t * string loc * module_type * module_expr) list
  | Tstr_modtype of Ident.t * string loc * module_type
  | Tstr_open of Path.t * Longident.t loc
  | Tstr_class of (class_declaration * string list * virtual_flag) list
  | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list
  | Tstr_include of module_expr * Ident.t list

and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of Primitive.description

and module_type =
  { mty_desc: module_type_desc;
    mty_type : Types.module_type;
    mty_env : Env.t;
    mty_loc: Location.t }

and module_type_desc =
    Tmty_ident of Path.t * Longident.t loc
  | Tmty_signature of signature
  | Tmty_functor of Ident.t * string loc * module_type * module_type
  | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
  | Tmty_typeof of module_expr

and signature = {
  sig_items : signature_item list;
  sig_type : Types.signature;
}

and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }

and signature_item_desc =
    Tsig_value of Ident.t * string loc * value_description
  | Tsig_type of (Ident.t * string loc * type_declaration) list
  | Tsig_exception of Ident.t * string loc * exception_declaration
  | Tsig_module of Ident.t * string loc * module_type
  | Tsig_recmodule of (Ident.t * string loc * module_type) list
  | Tsig_modtype of Ident.t * string loc * modtype_declaration
  | Tsig_open of Path.t * Longident.t loc
  | Tsig_include of module_type * Types.signature
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list

and modtype_declaration =
    Tmodtype_abstract
  | Tmodtype_manifest of module_type

and with_constraint =
    Twith_type of type_declaration
  | Twith_module of Path.t * Longident.t loc
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t * Longident.t loc

and core_type =
(* mutable because of [Typeclass.declare_method] *)
  { mutable ctyp_desc : core_type_desc;
    mutable ctyp_type : type_expr;
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t }

and core_type_desc =
    Ttyp_any
  | Ttyp_var of string
  | Ttyp_arrow of label * core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of Path.t * Longident.t loc * core_type list
  | Ttyp_object of core_field_type list
  | Ttyp_class of Path.t * Longident.t loc * core_type list * label list
  | Ttyp_alias of core_type * string
  | Ttyp_variant of row_field list * bool * label list option
  | Ttyp_poly of string list * core_type
  | Ttyp_package of package_type

and package_type = {
  pack_name : Path.t;
  pack_fields : (string loc * core_type) list;
  pack_type : Types.module_type;
  pack_txt : Longident.t loc;
}

and core_field_type =
  { field_desc: core_field_desc;
    field_loc: Location.t }

and core_field_desc =
    Tcfield of string * core_type
  | Tcfield_var

and row_field =
    Ttag of label * bool * core_type list
  | Tinherit of core_type

and value_description =
  { val_desc : core_type;
    val_val : Types.value_description;
    val_prim : string list;
    val_loc : Location.t;
    }

and type_declaration =
  { typ_params: string loc option list;
    typ_type : Types.type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_variance: (bool * bool) list;
    typ_loc: Location.t }

and type_kind =
    Ttype_abstract
  | Ttype_variant of (Ident.t * string loc * core_type list * Location.t) list
  | Ttype_record of
      (Ident.t * string loc * mutable_flag * core_type * Location.t) list

and exception_declaration = core_type list

and class_type =
  { cltyp_desc: class_type_desc;
    cltyp_type : Types.class_type;
    cltyp_env : Env.t; (* BINANNOT ADDED *)
    cltyp_loc: Location.t }

and class_type_desc =
    Tcty_constr of Path.t * Longident.t loc * core_type list
  | Tcty_signature of class_signature
  | Tcty_fun of label * core_type * class_type

and class_signature = {
    csig_self : core_type;
    csig_fields : class_type_field list;
    csig_type : Types.class_signature;
    csig_loc : Location.t;
  }

and class_type_field = {
    ctf_desc : class_type_field_desc;
    ctf_loc : Location.t;
  }

and class_type_field_desc =
    Tctf_inher of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_virt  of (string * private_flag * core_type)
  | Tctf_meth  of (string * private_flag * core_type)
  | Tctf_cstr  of (core_type * core_type)

and class_declaration =
  class_expr class_infos

and class_description =
  class_type class_infos

and class_type_declaration =
  class_type class_infos

and 'a class_infos =
  { ci_virt: virtual_flag;
    ci_params: string loc list * Location.t;
    ci_id_name : string loc;
    ci_id_class: Ident.t;
    ci_id_class_type : Ident.t;
    ci_id_object : Ident.t;
    ci_id_typesharp : Ident.t;
    ci_expr: 'a;
    ci_decl: Types.class_declaration;
    ci_type_decl : Types.class_type_declaration;
    ci_variance: (bool * bool) list;
    ci_loc: Location.t }
end
module Cmi_format = struct
type pers_flags = Rectypes
end
module Lambda = struct
open Asttypes
type primitive =
    Pidentity
  | Pignore
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag
  | Pfield of int
  | Psetfield of int * bool
  | Pfloatfield of int
  | Psetfloatfield of int
  | Pduprecord of Types.record_representation * int
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of comparison
  (* String operations *)
  | Pstringlength | Pstringrefu | Pstringsetu | Pstringrefs | Pstringsets
  (* Array operations *)
  | Pmakearray of array_kind
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Bitvect operations *)
  | Pbittest
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of boxed_integer
  | Pmodbint of boxed_integer
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout

and comparison =
    Ceq | Cneq | Clt | Cgt | Cle | Cge

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout
type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string
type function_kind = Curried | Tupled
type let_kind = Strict | Alias | StrictOpt | Variable
type meth_kind = Self | Public | Cached
type shared_code = (int * int) list
end
module Cmo_format = struct
type reloc_info =
    Reloc_literal of Lambda.structured_constant    (* structured constant *)
  | Reloc_getglobal of Ident.t              (* reference to a global *)
  | Reloc_setglobal of Ident.t              (* definition of a global *)
  | Reloc_primitive of string
type compilation_unit =
  { cu_name: string;                    (* Name of compilation unit *)
    mutable cu_pos: int;                (* Absolute position in file *)
    cu_codesize: int;                   (* Size of code block *)
    cu_reloc: (reloc_info * int) list;  (* Relocation information *)
    cu_imports: (string * Digest.t) list; (* Names and CRC of intfs imported *)
    cu_primitives: string list;         (* Primitives declared inside *)
    mutable cu_force_link: bool;        (* Must be linked even if unref'ed *)
    mutable cu_debug: int;              (* Position of debugging info, or 0 *)
    cu_debugsize: int }
type library =
  { lib_units: compilation_unit list;   (* List of compilation units *)
    lib_custom: bool;                   (* Requires custom mode linking? *)
    lib_ccobjs: string list;            (* C object files needed for -custom *)
    lib_ccopts: string list;            (* Extra opts to C compiler *)
    lib_dllibs: string list }
end
module Debuginfo = struct
type kind = Dinfo_call | Dinfo_raise
type t = {
  dinfo_kind: kind;
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int
}
end
module Clambda = struct
open Asttypes
open Lambda
type function_label = string
type ulambda =
    Uvar of Ident.t
  | Uconst of structured_constant * string option
  | Udirect_apply of function_label * ulambda list * Debuginfo.t
  | Ugeneric_apply of ulambda * ulambda list * Debuginfo.t
  | Uclosure of (function_label * int * Ident.t list * ulambda) list
              * ulambda list
  | Uoffset of ulambda * int
  | Ulet of Ident.t * ulambda * ulambda
  | Uletrec of (Ident.t * ulambda) list * ulambda
  | Uprim of primitive * ulambda list * Debuginfo.t
  | Uswitch of ulambda * ulambda_switch
  | Ustaticfail of int * ulambda list
  | Ucatch of int * Ident.t list * ulambda * ulambda
  | Utrywith of ulambda * Ident.t * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of Ident.t * ulambda * ulambda * direction_flag * ulambda
  | Uassign of Ident.t * ulambda
  | Usend of meth_kind * ulambda * ulambda * ulambda list * Debuginfo.t

and ulambda_switch =
  { us_index_consts: int array;
    us_actions_consts : ulambda array;
    us_index_blocks: int array;
    us_actions_blocks: ulambda array}
type function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool;           (* True if environment not used *)
    mutable fun_inline: (Ident.t list * ulambda) option }
type value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
  | Value_integer of int
  | Value_constptr of int
end
module Cmx_format = struct
open Clambda
type unit_infos =
  { mutable ui_name: string;                    (* Name of unit implemented *)
    mutable ui_symbol: string;            (* Prefix for symbols *)
    mutable ui_defines: string list;      (* Unit and sub-units implemented *)
    mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
    mutable ui_imports_cmx: (string * Digest.t) list; (* Infos imported *)
    mutable ui_approx: Clambda.value_approximation; (* Approx of the structure *)
    mutable ui_curry_fun: int list;             (* Currying functions needed *)
    mutable ui_apply_fun: int list;             (* Apply functions needed *)
    mutable ui_send_fun: int list;              (* Send functions needed *)
    mutable ui_force_link: bool }
type library_infos =
  { lib_units: (unit_infos * Digest.t) list;  (* List of unit infos w/ CRCs *)
    lib_ccobjs: string list;            (* C object files needed *)
    lib_ccopts: string list }
end

*)

