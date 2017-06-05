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

(** Accessing Programs *)

(** The mutable state in this module is global, because it is meant to
    be shared by all program-manipulating processing. We could make it
    a functor if several independent caches are needed.

    We have a global state consisting of a current program and a
    current source file, which are set each time [source_env] is
    called. [Config.load_path] is also set to the absolute load path,
    but we do not use it since we redefine [find_pers_struct] to
    search in the program. *)

open Program

(** If set, never look at auto-save files. *)
val ignore_auto_save : bool ref

(** Raised if some edited source file needs to be saved. *)
exception Unsaved of source_file

(** Raised if some source_file has no cmti or cmt *)
exception NoCmt of source_file

(** Raised if a pack module has no cmt *)
exception NoCmtPack of packed_unit

(** Raised if some source_file has no cmi (or cmti or cmt) *)
exception NoCmi of string

(** Raised if some edited source file needs to be re-typed. *)
exception OutdatedCmt of source_file

(** Raised if we are about to load a persistent structure for an unit
    which has a cmti but no (or ignored) mli. *)
exception ExistingIgnoredCmti of string * Program.compilation_unit

(** Check that the program is
    - fully saved (no remaining auto-save files)
    - fully compiled (with up-to-date cmt(i) files.

    @raise [Unsaved], [NoCmt], [OutdatedCmt]. *)
val check_for_refactoring : errors : exn list ref -> program -> unit

(** Check a single source file (maybe we should check its dependencies
    as well).

    @raise [Unsaved], [NoCmt], [OutdatedCmt]. *)
val check_source_for_refactoring :
  errors : exn list ref -> program -> source_file -> unit

(** Return the autosaved version of a file if it exists and is more
    recent than the base file, or the file itself otherwise. *)
val check_auto_save : string -> string

(** Return the autosaved version of a file if it exists and is more
    recent than the base file, None otherwise. *)
val auto_save : string -> string option

(** Test whether a file has an autosave which is more recent than the
    file itself. *)
val has_auto_save : string -> bool

(** Cached version of [Cmt_format.read_cmt]. Does not check that the
    file exists! *)
val read_cmt : string -> Cmt_format.cmt_infos * Cmt_format.source_info option

(** Get the typedtree file of a source file. Raise [NoCmt] if there is
    none. *)
val typedtree : program -> source_file -> TypedtreeOps.typedtree
val pack : program -> packed_unit -> Types.signature * string list

(** Return the typing environment for a source file, intializing the
    load path and flushing the persistent structure table.

    @raise [Failure] if pervasives.cmi cannot be found. *)
val source_env : program -> source_file -> Env.t
val pack_env : program -> packed_unit -> Env.t

(* broken !
(** UNSAFE! Return the initial environment for a program, intializing
    the load path (assuming the it is the same for all source files)
    and flushing the persistent structure table.

    @raise [Failure] if pervasives.cmi cannot be found. *)
val program_env : ?nopervasives:bool -> program -> Env.t
*)

(** Return the program argument passed to the last invocation of
    [source_env] or [program_env].

    @raise [Not_found] if there is no current program. *)
val program : unit -> program

(** Return the source_file argument passed to the last invocation of
    [source_env] (this value is cleared when calling [program_env]).

    @raise [Not_found] if there is no current unit *)
val current_source : unit -> source_file

(** Classify an ident according to its creation time. *)
val classify_ident :
  Ident.t -> [`predef | `persistent | `dumped | `hidden | `fresh]

(** Getting the signature of units. *)


(** The following functions raise [NoCmt] or [NoCmi] if the needed
    cmt(i) is absent, or [Failure] if a problem occurs while reading a
    cmt(i) file. *)
(*
val concrete_unit_signature : program -> concrete_unit -> Types.signature
val unit_signature : program -> compilation_unit -> Types.signature
*)
val unit2cmi : string -> Program.compilation_unit -> string
val signature : program -> string -> Types.signature

(** Find a module name in the load path of the current source file and
    return the corresponding prefix and compilation unit.

    @raise [Not_found] if the toplevel module cannot be found at all. *)
val modname2unit : string -> string * Program.compilation_unit

(** Find a the compilation unit defining an ident context (without
    using the current source for disambiguation).

    @raise [Not_found] if the toplevel module cannot be found at all.
    @raise [AmbiguousPersistent] if the source digest does not match
    and there are several units with the context's module name. *)
val ctx2prefix : Ident.ctx -> string * Program.compilation_unit

(** Get the signature of a unit specified by its prefix. *)

(** {5 Location conversions.} *)

(** Converting character counts, line counts, and locations between
    the last-compiled and last-autosaved versions of source files.
    Don't use the `line versions, which are inaccurate. *)

val old2last : [ `char | `line ] -> program -> source_file -> int -> int
val last2old : [ `char | `line ] -> program -> source_file -> int -> int

(** The following conversions use only (pos_lnum, pos_bol), because
    only these values are updated by line number directives (in
    text-format preprocessed sources such as for ocamlyacc and
    ocamllex). *)

val old2last_loc : program -> Location.t -> Location.t
val last2old_loc : program -> Location.t -> Location.t

type pos = [ `cnum of int | `lc of int * int ]

(** Convert a "current" char number to "last-compiled" line (from 1)
    and column (from 0) numbers. *)
val last_cnum2old_lc : program -> source_file -> pos -> int * int

(** Create a cache for a function taking as arguments a program and a
    unit source file in this program. Programs are only distinguished
    according to absoluter root directory, and are never invalidated.
    units are distinguished by their prefix, and are invalidated if
    the modtime (or the presence) of any of their source files
    change. *)
val make_program_source_file_cache :
  value_up_to_date:(Program.program -> 'a -> 'b) ->
  (Program.program -> Program.source_file_id -> 'a) ->
  Program.program -> Program.source_file_id -> 'a

(*
val make_program_unit_cache :
  (Program.program -> string -> 'a) ->
  Program.program -> string -> 'a
*)

(** Should not be here: *)
val make_cache : ('a -> 'b) -> ('a -> 'c -> 'd) -> ('a -> 'c) -> 'a -> 'c
val cached_digest : string -> Digest.t

(** Safe {i w.r.t.} absent files.  *)
val modtime : string -> float
