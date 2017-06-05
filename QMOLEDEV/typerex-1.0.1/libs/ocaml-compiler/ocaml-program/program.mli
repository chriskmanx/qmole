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

(** Managing OCaml files and programs *)

(** The following is a minimalist description of the static semantics
    of OCaml "programs" (or libraries), which are defined as sets of
    modules. In particular, it follows the following design choices:
    - There is no notion of structure, project name or executable name.
    This ensures independency w.r.t. build systems/project descriptions.
    - We do not care about compilers (ocamlc/ocamlopt, .byte/.opt) or
    linking (so there is no distinction between standalone programs and
    libraries).
    - A program is the set of the compilation units that form it; we
    do not distinguish between the "own program's" modules
    (modifiable, described in built, etc.) and libraries (except the
    sources and typedtrees of the latter may not be available).
    - Programs may be complete (closed), with all libraries (including
    the stdlib), or not. Such distinctions should be done at the user
    level.
    - We focus on the two kinds of source file known to OCaml:
    interfaces and implementations. Theier origin (e.g., parser
    generators or other preprocessing) is only secondary.

    A program has an absolute [root] directory, w.r.t. which all
    filenames (and their "prefixes") are interpreted.

    A compilation unit is identified by its prefix w.r.t the root,
    which is the prefix of its source files if they exist, or
    typedtrees or compiled interfaces otherwise.

    We call [Abstract] a compilation unit for which no source file or
    typedtree is available (unless it is known to be a pack), and
    which is only described by its signature (an the load path to
    interpret it). *)

(** {2 Representation of OCaml programs.} *)

(** The type of information associated with the two different kinds of
    source files understood by the OCaml compiler: interfaces and
    implementations (possibly generated from other kind of source
    files). All file and directory names are relative to the root (see
    [program], or absolute (for the load_path in particular). *)
type source_file = {
  load_path : string list;
    (** load path for this source file, including the path to the
	stdlib (unless -nostdlib) ; the paths are in the order they
	are traversed when looking for a toplevel module (i.e., the
	reverse order of -I arguments). Relative to the root !

        For directories belonging to the program, they specify where
        to find the source files for the units (i.e., the .cmi and
        .cmt[i] may not be at the same place. *)
  nopervasives : bool;
  (* options ? *)
  source : string;
    (** file name of the actual source file (e.g., "parser.mly"), which
	may be available or not. Relative to the root ! *)
  preprocessor : [`ocamllex | `ocamlyacc | `camlp4 of string ] option;
  typedtree : string
    (** file name of the dumped typedtree (.cmt or .cmti) file,
        available or not.  Relative to the root ! *)
}

(** A concrete compilation unit is a toplevel module which is not a
    pack and whose sources are known and available. Either of its
    interface and implementation may be absent, but not both. When
    only the implementation exists, we expect its typedtree (".cmt")
    to contain signature information (if it exists). *)
type concrete_unit = {
  (** At most one of [interface] and [implementation] may be None. *)
  interface : source_file option;
  implementation : source_file option
}

(** An abstract compilation unit has no sources, no typedtree, but a
    compiled interface file (this should be a cmi, not a cmti or cmt,
    and we will not use anything more than cmi information), and a
    load path. *)
type abstract_unit = {
  a_load_path : string list;
    (** load path for interpreting the persistent idents appearing in
        the unit's signature. Relative to the root ! *)
  a_signature : string;
    (** file name of the compiled interface (.cmi file). Relative to
	the root ! *)
}

(** A packed unit is generated with the [-pack] option. There may
    exist an interface file (.mli), against which the packed signature
    is matched. its implementation is described by the prefixes of the
    compilation units which are packed in it (order is not
    meaningful), and which should be present in the program. The load
    path (for the implementation) is also specified, as well as the
    .cmt file (which must be of pack kind). *)
type packed_unit = {
  p_interface : source_file option;
  (** explicit interface (.mli), if any *)
  p_units : string list;
    (** prefixes for the packed units *)
  p_typedtree : string;
    (** packed cmt. *)
  p_load_path : string list
    (** load path for interpreting the implementation (since the
        interface has its own load path, if there is one. *)
}

(** In the following we identify a compilation unit by its (usually
    non-capitalized) prefix, such as "name" or "path/to/name" or
    "/absolute/path/to/name". Relative prefixes must be interpreted
    w.r.t. the [root] field of programs). *)


(** A compilation unit is either concrete, packed, or abstract, which
    means that only its load path and signature are known. *)
type compilation_unit =
  | Concrete of concrete_unit
  (** A unit is concrete if we have access to its implementation,
      i.e., know if it has a .ml, .mli, or both (and their contents,
      and typedtrees). *)
  | Abstract of abstract_unit
  (** An abstract unit is only known by its signature. *)
  | Pack of packed_unit
  (** A pack unit is specified by the list of compilation units that
      it contains, which should be in the program. *)

(** A program is a list of compilation units. *)
type program = {
  root : string;
    (** absolute path to the "project's root" which should be used for
	interpreting all the relative names appearing in the
	representation of programs. *)
  (* compiler version ? *)
  units : (string, compilation_unit) Hashtbl.t
    (** compilation units of the program, indexed by their prefix. *)
}

(** The two different kinds of source files understood by the OCaml
    compiler (possibly generated from other kind of source files). *)
type source_kind = [`ml | `mli]

(** A source file is given by its prefix (see the discussion above)
    and its source kind. *)
type source_file_id = string * source_kind

(** {2 Accessing OCaml programs.} *)

(** Rebasing file names and paths *)

(** The type of prefixing options, to make relative paths (w.r.t. the
    root) either absolute or relative to a subdir. *)
type prefix_option = [`absolute | `subdir of string]

(** If the given path is not absolute, assume it is relative to the
    project's root and prefix it appropriately so as to make it either
    absolute, or relative to the specified subdir of the project's
    root. *)
val prefix_with : ?prefix:prefix_option -> program -> string -> string

val source_load_path :
  ?prefix:prefix_option -> program -> source_file -> string list
val source :
  ?prefix:prefix_option -> program -> source_file -> string
val typedtree :
  ?prefix:prefix_option -> program -> source_file -> string
val pack_typedtree :
  ?prefix:prefix_option -> program -> packed_unit -> string
val pack_unit_load_path :
  ?prefix:prefix_option -> program -> packed_unit -> string list
val abstract_load_path :
  ?prefix:prefix_option -> program -> abstract_unit -> string list
val abstract_signature :
  ?prefix:prefix_option -> program -> abstract_unit -> string

(** Converting to module names *)

val prefix2modname : string -> string
val source2modname : source_file_id -> string

(** Warning ! [source2string] always returns the name of a .ml or .mli
    file, for example even if a .mly exists. This is intended for
    debugging only. *)
val source2string : source_file_id -> string

(** Accessors *)

(** Retrieve a compilation unit by its prefix.

    @raise [Not_found] if no unit exists with that prefix. *)
val find_unit : program -> string -> compilation_unit


(*
(** Get a source file of a specific kind.

    @raise [Not_found] if the unit has no source of this kind. *)
val unit2source : concrete_unit -> source_kind -> source_file
*)

(** Retrieve a source file by its source id.

    @raise [Not_found] if no unit exists with that prefix, or if the
    specified source is not available. *)
val find_source : program -> source_file_id -> source_file

(** Retrieve a source file id by its source name. If the interface and
    implementation have the same name (e.g. file.mly) then we return
    the implementation.

    @raise [Not_found] if there is no source file with this name. *)
val find_source_name : program -> string -> source_file_id

(** Get the mli, then the ml file. *)
val mli_or_ml : concrete_unit -> source_kind * source_file

(* unused

(** Return the interface if it exists, or the implementation otherwise

    @raise [Not_found] if no concrete unit exists with that prefix. *)
val prefix2source_id : program -> string -> source_file_id
*)

(** Getting the load path. *)

(* Possibly inaccurate, and unused
val concrete_unit_load_path :
  ?prefix:prefix_option -> program -> concrete_unit -> string list
val unit_load_path :
  ?prefix:prefix_option -> program -> compilation_unit -> string list
*)

(** Return the load path of a unit given its prefix.

    @raise Not_found if there is no unit with this prefix. *)
val load_path : ?prefix:prefix_option -> program -> string -> string list

(** Access by module name (possibly ambiguous!) *)

(** Raised by [modname2unit] if a module name cannot be uniquely
    resolved. *)
exception AmbiguousPersistent of
    string * string list option * (string * compilation_unit) list

(** The following functions take two optional parameters [permissive]
    and [load_path], in addition to [program]. If a load path is
    provided, it must use relative names w.r.t. root when possible,
    and only compilation units whose prefix is in the load path may be
    returned. This allows programs to contain duplicate modules with the
    same name, if they never appear at the same time in the load path
    for a specific source file.

    In case of ambiguity, an error is raised, unless [permissive] is
    set to true, in which case the first occurrence in the ordered
    load_path is chosen (TODO: ensure this behavior). *)

(** Return the prefix and compilation unit for a module name.

    @raise [Not_found] if there is no unit with this modname.
    @raise [AmbiguousPersistent] if there is more than one unit with this
    modname. *)
val modname2unit :
  program -> ?permissive:bool -> ?load_path:string list ->
  string -> string * compilation_unit

(** Same as [modname2unit], but return only the prefix. *)
val modname2prefix :
  program -> ?permissive:bool -> ?load_path:string list -> string -> string

(*
(** Same as [modname2unit], but return the source filename of given kind.

    @raise [Not_found] if the unit is abstract or has no source file
    of this kind. *)
val modname2filename :
  ?prefix:prefix_option -> program -> ?permissive:bool ->
  ?load_path:string list ->
  source_kind -> string -> string
*)

(** Return the source id of the interface if it exists, of the
    implementation otherwise. *)
val modname2source_id :
  program -> ?permissive:bool -> ?load_path:string list -> string -> source_file_id

(** Return the source file for the interface if it exists, of the
    implementation otherwise. *)
val modname2source_file :
  program -> ?permissive:bool -> ?load_path:string list -> string -> source_file

val find_all_modname :
  program -> string -> (string -> compilation_unit -> bool) ->
  (string * compilation_unit) list

(** {5 Iterators.} *)

(** Iterators. Iteration order is unspecified. Provided strings are
    prefixes relative to the project's root. *)

val iter_units : (string -> compilation_unit -> unit) -> program -> unit
val fold_units : (string -> compilation_unit -> 'a -> 'a) -> program -> 'a -> 'a

(** Flat iterators on OCaml source files. *)

(** Note: the following iterators apply the source processing function
    also to pack interfaces, unless a pack function is provided.  *)

val iter_sources :
  ?abstract:(string -> abstract_unit -> unit) ->
  ?pack:(string -> packed_unit -> unit) ->
  (source_file_id -> source_file -> unit) ->
  program -> unit
val fold_sources :
  ?abstract:(string -> abstract_unit -> 'a -> 'a) ->
  ?pack:(string -> packed_unit -> 'a -> 'a) ->
  (source_file_id -> source_file -> 'a -> 'a) ->
  program -> 'a -> 'a

(** {5 Location manipulation.} *)

(** The following two functions should be used with locations whose
    fname fields are relative to the root. When this is not the case,
    we try to find a unique unit matching the basename, and fail
    otherwise. *)

(** Return the filename of a location (with the correct prefix)

    @raise [Not_found] if the location source file's basename does not
    match any compilation unit.

    @raise [Failure] if the location source file's prefix does not
    exactly match a module, and its basename matches more than one. *)
val file_of_loc : ?prefix:prefix_option -> program -> Location.t -> string

(** Same as [file_of_loc], but return the source id instead.

    @raise [Invalid_argument] if the found unit is abstract. *)
val source_id_of_loc : program -> Location.t -> source_file_id

(** Set the two pos_fname fields of a location *)
val correct_fname : string -> Location.t -> Location.t

(** Dump a program data-structure (generated by ocp-codegen). *)
val string_of_program : program -> string
