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

(** Efficiently applying replacements and renames to (sets of) files. *)

(** Note: all file operations in this module behave according to
    [Sys.getcwd()]. All files are opened in binary mode. *)

(** {4 Applying a list of replacement to a single file} *)

(** [edit replaces filename] applies the replacements specified by
    [replaces] to the file [filename]. [replaces must be an ordered
    list of non-overlapping replacements, of the form [(start, end,
    text)] which are applied "simultaneously", in the sense that all
    offset are relative to the initial unmodified file. Each
    replacement means the substitution of \[start, end\[ with
    text. Positions start at 0, and the range of each replacement must
    be between 0 and the file length.

    @raise [Invalid_argument] if the replacements are overlapping,
    un-ordered, or exceed the range of the file.

    @raise [Failure] if the the file does not exist. *)
val edit : (int * int * string) list -> string -> unit

val check_original : (int * int * (string -> unit)) list -> string -> unit


(** {4 Atomically undoable editing and renaming of multiple files} *)

(** The type of undo stacks. Each item represent a multiple-file edit
    performed through [edit_files]. *)
type undo_stack

(** [edit_files stack name edits renames] atomically performs the
    specified files [edits], and renames (moves) the specified
    [files]. Each edit is specified by a filename and an ordered list
    of modifications as for [edit]. Renames are specified by a pair
    (old name, new name). This operation is recorded in [stack] with
    given [name] for undoing.

    @raise [Invalid_argument] if
    - some files are both edited and renamed (from or to), or
    - some file appears multiple times in either list, or
    - some file edit does not meet the requirements of [edit].

    @raise [Failure] if some edited file does not exist.

    @raise [OwzFailure] if some edited or renamed (from or to) file
    has an auto-save. *)
val edit_files :
  undo_stack -> string ->
  (string * (int * int * string) list) list -> (string * string) list -> unit

(** Return the name of the next action to be undone and the list of
    files which have been modified (or renamed) since then.

    @raise [Util.OwzFailure] if some file which was the origin of a
    renaming in this action has been re-created, or has an existing
    auto-save.

    @raise Stack.empty if there is nothing to undo. *)
val check_undo : undo_stack -> string * string list

(** Return a new empy undo stack. *)
val empty_undo : unit -> undo_stack

(** Undo the last (not already undone) multiple-file edit.  Auto-saves
    of edited (and not renamed) files are removed.
    Make sure to call check_undo before.

    @raise Stack.empty if there is nothing to undo. *)
val undo_last : undo_stack -> (string * [`renamed of string | `edited]) list

(** {4 Modifying text files.} *)

val cp : ?overwrite:bool -> string -> string -> unit
val mv : ?overwrite:bool -> string -> string -> unit





