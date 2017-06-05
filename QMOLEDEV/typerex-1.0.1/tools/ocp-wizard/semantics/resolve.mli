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

(** Different sort of names, and their bindings. *)

(** We consider the following three sorts of idents.
    - First, there are predef idents, which have a timestamp <1000.
    - Second, if a toplevel module [M] belongs to the program (i.e., has
    any corresponding source file m.ml, or m.mli, or both) then for all
    ident [x] bound in each source file of [M] (exported or not) there is a
    non-persistent ident [x] which appears in the corresponding typedtree
    (the field [sig_items] or [str_items] of the .cmti or .cmt) and is
    referred to in this typedtree. This ident is tagged [`source
    ("path/to/m", `ml)] or [`source ("path/to/m", `mli)], respectively,
    where the path is the name of the source file in the program (See
    module {!OcamlFile}). If the source file is a .mli, or is a .ml
    without .mli, then those idents are also referred to from the
    outside.
    - Finally, toplevel-modules (whether in the program or not) and
    predefined types and exceptions have only one ident which is tagged
    [`toplevel]. The former are persistent, and the latter are not, but
    have a time stamp strictly below 1000 (See module {!Predef}). *)

(** The context for interpreting an ident is either:
    - a source file (more precisely, its dumped typedtree)
    - the toplevel (with a specified dirname to allow equally named modules)
    - the set of predefined types, constructors, and exceptions. *)
type ident_context =
  | Typedtree of Program.source_file_id
  | Library of string
  | Toplevel of string (** dirname relative to root *)
  | Predef
  | PackImplem of string

val ident_context : Ident.t -> ident_context

val context2string : ident_context -> string

val parse_string :
  ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a) ->
  string -> 'a
val parse_lid : Env_untyped.path_sort -> string -> Longident.t

(* not used outside Resolve
   val lookup : PathSort.path_sort -> Longident.t -> PathSort.t -> Path.t
   val summary_item : ...
*)

val sig_item : Env_untyped.path_sort -> Types.signature_item -> Ident.t option

(** Raised by [checked_lookup*] instead of [Not_found]. *)
exception Unbound of Env_untyped.path_sort * Longident.t

val checked_lookup:
  Env_untyped.path_sort -> (Longident.t -> 'a -> 'b) -> Longident.t -> 'a -> 'b
val checked_find:
  Env_untyped.path_sort -> (Path.t -> 'a -> 'b) -> Path.t -> 'a -> 'b
val checked_untyped_lookup:
  Env_untyped.path_sort -> Longident.t -> Env.t -> Path.t * Env_untyped.description

(* not used anymore
val checked_lookup_module: Longident.t -> Env.t -> Path.path * Types.module_type
*)

(** Raised by [modtype], [modtype_signature], and [modtype_functor] when
    looking for the signature of an abstract module type. *)
exception Abstract_modtype

(** The following functions keep track of the context in which the
    idents and paths should be interpreted.

    When we get a module type through find_module or find_modtype, the
    ident(s) are renamed if the path comes from a persistent
    signature. We assume that idents can only be in the same dumped
    typedtree, or in a persistent signature. *)

(** Get the signature (or functor signature) corresponding to a module type *)
val modtype :
  Env.t -> Types.module_type ->
    [ `func of Ident.t * Types.module_type * Types.module_type
    | `sign of Types.signature ]

(** Same as [modtype], but the result must be a signature. *)
val modtype_signature :
  Env.t -> Types.module_type ->
  Types.signature

(** Same as [modtype], but the result must be a functor. *)
val modtype_functor :
  Env.t -> Types.module_type ->
  (Ident.t * Types.module_type * Types.module_type)

val module_lid2sig :
  Env.t -> Longident.t -> Types.signature

(* Unused outside of this module
(** See modtype_signature. *)
val resolve_module : Env.t -> Path.t -> Types.signature
val resolve_module_lid : Env.t -> Longident.t -> Types.signature

(** See modtype. *)
val resolve_modtype :
  Env.t ->
  Path.t ->
  [ `func of Ident.t * Types.module_type * Types.module_type
  | `sign of Types.signature ]

*)

val resolve_lid : Env_untyped.path_sort -> Env.t -> Longident.t -> Ident.t
val resolve_path : Env_untyped.path_sort -> Path.t -> Env.t -> Ident.t

(** [resolves_to kind env lid ids] tests whether [lid] reffers to one
    of [ids] in environment [env], i.e., if the object directly
    denoted by [lid] is named with one of [ids]. This indicates that
    the rightmost name in [lid] needs renaming (assuming we are
    renaming [ids]). *)
val resolves_to :
  Env_untyped.path_sort -> Env.t -> Longident.t -> Ident.t list -> bool

(** Retrieve a module or modtype in a signature from its name *)
val lookup_in_signature :
  Env_untyped.path_sort -> string -> (Env.t * Types.signature_item) list ->
  Env.t * Types.signature_item

(** Insert the environment before each signature item in a signature. *)
val add_environments :
  Env.t -> Types.signature -> (Env.t * Types.signature_item) list

(** Retrieve an element in a signature from its name *)
val find_in_signature :
  Env_untyped.path_sort -> string -> Types.signature -> Ident.t

(** Raised by [check] to signal an impossible renaming due to a masking
    of an existing occurrence of the new name, or of a renamed
    occurrence (the boolean specifies if it is a renamed occurrence). *)
exception Masked_by of bool * Ident.t

(** Raised by [check] to signal an impossible toplevel module renaming
    because a module with the new name already exists. *)
exception AmbiguousOrder of string array * (int * (Path.t * Types.module_type)) list

(** Check that the renaming of a list of idents (with the same name)
    into a new name would not change the meaning of a reference in a
    given environment, i.e., this reference being either one of the
    ids, or a different existing id already named with the new name,
    as denoted by the boolean renamed.

    In other words, if renamed is true, this function ensures that the
    new name will indeed reffer to one of the ids, and otherwise, that
    the existing instance of the new name will not.

    @raise [Unbound] if none of the given idents or name is in the
    environment.

    @raise [Masked_by id] if masking would occur. *)
val check :
  Env_untyped.path_sort -> Env.t * Env.summary ->
  renamed:bool -> ids:Ident.t list -> new_name:string -> unit

(** Similar to [check], but for a signature. *)
val check_in_sig :
  Env_untyped.path_sort -> Types.signature_item list ->
  renamed:bool -> ids:Ident.t list -> new_name:string -> unit

(** [rev_lookup kind env p] returns the longident made of the shortest
    suffix of the path [p] of type [kind] which, in environment [env],
    denotes [p]. *)
val rev_lookup : Env_untyped.path_sort -> Env.t -> Path.t -> Longident.t

(** [rev_lookup kind env lid gid] returns the shortest suffix of the
    longiodent [lid] of type [kind] which, in environment [env],
    resolves to [gid].

    Use Untypeast.lident_of_path to get a longident from a path. *)
val rev_lookup_id :
  Env_untyped.path_sort -> Env.t -> Longident.t -> Ident.t ->
  Longident.t
