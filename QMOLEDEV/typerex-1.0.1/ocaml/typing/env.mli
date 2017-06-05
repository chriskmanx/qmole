(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Environment handling *)

open Types

module EnvLazy : sig
  type 'a t
  type ('a,'b) maker

  val force : 'a t -> 'a
  val create : ('a,'b) maker -> 'a -> 'b t
  val declare_maker : string -> ('a,'b) maker
  val register_maker : ('a,'b) maker -> ('a -> 'b) -> unit

  exception UnknownLazyMaker of string
  val eager : bool ref
end

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



val empty: t
val initial: t
val diff: t -> t -> Ident.t list

(* Lookup by paths *)

val find_value: Path.t -> t -> value_description
val find_type: Path.t -> t -> type_declaration
val find_constructors: Path.t -> t -> constructor_description list
val find_module: Path.t -> t -> module_type
val find_modtype: Path.t -> t -> modtype_declaration
val find_class: Path.t -> t -> class_declaration
val find_cltype: Path.t -> t -> class_type_declaration
val find_cltype: Path.t -> t -> class_type_declaration
val find_annot: Path.t -> t -> Annot.ident

val find_type_expansion:
    ?use_local:bool -> ?level:int -> Path.t -> t -> type_expr list * type_expr
val find_type_expansion_opt: Path.t -> t -> type_expr list * type_expr
(* Find the manifest type information associated to a type for the sake
   of the compiler's type-based optimisations. *)
val find_modtype_expansion: Path.t -> t -> Types.module_type

val has_local_constraints: t -> bool
val map_newtype_level: t -> int -> int

(* Lookup by long identifiers *)

val lookup_value: Longident.t -> t -> Path.t * value_description
val lookup_annot: Longident.t -> t -> Path.t * Annot.ident
val lookup_constructor: Longident.t -> t -> Path.t * constructor_description
val lookup_label: Longident.t -> t -> Path.t * label_description
val lookup_type: Longident.t -> t -> Path.t * type_declaration
val lookup_module: Longident.t -> t -> Path.t * module_type
val lookup_modtype: Longident.t -> t -> Path.t * modtype_declaration
val lookup_class: Longident.t -> t -> Path.t * class_declaration
val lookup_cltype: Longident.t -> t -> Path.t * class_type_declaration

(* Exported to implement fold *)
val lookup_module_descr : Longident.t -> t -> Path.t * module_components

(* Insertion by identifier *)

val add_value: Ident.t -> value_description -> t -> t
val add_annot: Ident.t -> Annot.ident -> t -> t
val add_type: Ident.t -> type_declaration -> t -> t
val add_exception: Ident.t -> exception_declaration -> t -> t
val add_module: Ident.t -> module_type -> t -> t
val add_modtype: Ident.t -> modtype_declaration -> t -> t
val add_class: Ident.t -> class_declaration -> t -> t
val add_cltype: Ident.t -> class_type_declaration -> t -> t
val add_local_constraint: Ident.t -> type_declaration -> int -> t -> t

(* Insertion of all fields of a signature. *)

val add_item: signature_item -> t -> t
val add_signature: signature -> t -> t

(* Insertion of all fields of a signature, relative to the given path.
   Used to implement open. *)

val open_signature: Path.t -> signature -> t -> t
val open_pers_signature: string -> t -> t

(* Insertion by name *)

val enter_value: string -> value_description -> t -> Ident.t * t
val enter_type: string -> type_declaration -> t -> Ident.t * t
val enter_exception: string -> exception_declaration -> t -> Ident.t * t
val enter_module: string -> module_type -> t -> Ident.t * t
val enter_modtype: string -> modtype_declaration -> t -> Ident.t * t
val enter_class: string -> class_declaration -> t -> Ident.t * t
val enter_cltype: string -> class_type_declaration -> t -> Ident.t * t

(* Initialize the cache of in-core module interfaces. *)
val reset_cache: unit -> unit

(* Remember the name of the current compilation unit. *)
val set_unit_name: string -> unit

(* Read, save a signature to/from a file *)

val read_signature: string -> string -> signature
        (* Arguments: module name, file name. Results: signature. *)
val save_signature: signature -> string -> string -> unit
        (* Arguments: signature, module name, file name. *)
val save_signature_with_imports:
            signature -> string -> string -> (string * Digest.t) list -> unit
        (* Arguments: signature, module name, file name,
           imported units with their CRCs. *)

(* Return the CRC of the interface of the given compilation unit *)

val crc_of_unit: string -> Digest.t

(* Return the set of compilation units imported, with their CRC *)

val imported_units: unit -> (string * Digest.t) list

(* Direct access to the table of imported compilation units with their CRC *)

val crc_units: Consistbl.t

(* Summaries -- compact representation of an environment, to be
   exported in debugging information. *)
val summary: t -> summary

(* Error report *)

type error =
    Not_an_interface of string
  | Corrupted_interface of string
  | Illegal_renaming of string * string
  | Inconsistent_import of string * string * string
  | Need_recursive_types of string * string

exception Error of error

open Format

val report_error: formatter -> error -> unit

(* Forward declaration to break mutual recursion with Includemod. *)
val check_modtype_inclusion:
      (t -> module_type -> Path.t -> module_type -> unit) ref

(* Persistent structures. All the following values are made available
   to be able to intercept the automatic loading of persistent structures
   during lookups, for example to use something else than .cmi files.
*)

type pers_flags = Rectypes

type pers_struct =
    { ps_name: string;
      ps_sig: signature;
      ps_comps: module_components;
      ps_crcs: (string * Digest.t) list;
      ps_filename: string;
      ps_flags: pers_flags list }

val current_unit : string ref
val persistent_structures : (string, pers_struct) Hashtbl.t
val find_cmi : string -> string list -> string
val read_pers_struct_fun : (string -> string -> pers_struct) ref
val find_pers_struct_fun : (string -> pers_struct) ref

type cmi_infos = {
    cmi_name : string;
    cmi_sign : Types.signature_item list;
    mutable cmi_crcs : (string * Digest.t) list;
    cmi_flags : pers_flags list;
}

val output_cmi : string -> out_channel -> cmi_infos -> Digest.t
val input_cmi : in_channel -> cmi_infos
val read_cmi_fun : (string -> cmi_infos) ref
