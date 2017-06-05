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

(** Callbacks to an IDE *)

(** Raised when the user aborts the operation during a callback (for
    example, with C-g under Emacs). *)
exception Quit

type property = [
  `face of Face.face | `fontified | `help_echo of string option
| `font_lock_multiline
]

type pos = [ `cnum of int | `lc of int * int ]


(** Editor API *)
module type Callback = sig

  (** In the following, positions are converted to start from 0 instead
      of 1 in Emacs, and the unit is byte by default, instead of char.
      Filenames are always absolute.

      We have commented all functions that are not currently used, to
      minimize the cost of porting TypeRex to another editor. Furthermore,
      only the default [`byte] unit is currently used. *)

  (** {4 Informations about the current state of the editor} *)

  (** Filename under focus (to which the querry applies). *)
  val buffer_file_name : unit -> string

  (** Current cursor position. *)
  val line_column_bytes : unit -> int * int

  (** {4 Displaying information in the editor} *)

  (** Goto the given (filename, position), in the current
      frame (openning the file as needed). The cursor motion should be
      made (buffer-locally) undoable. *)
  val goto : ?unit:[ `byte | `char ] -> string -> pos -> unit

  (** Display a short message. *)
  val message : ('a, unit, string, unit) format4 -> 'a

  (** Display a possibly long message in the most appropriate way,
      until next user event. *)
  val display_temp : string -> ('a, unit, string, unit) format4 -> 'a

  (** Highlight the specified range in the current buffer, until the
      next user action. *)
  val highlight_regions :
    ?unit:[ `byte | `char ] -> ?forever:bool -> (Face.face * pos * pos) list ->
    unit

  (** {4 Prompting for information in the editor} *)

  (** Prompt the user for a string, with provided question. *)
  val read_from_minibuffer : ('a, unit, string, string) format4 -> 'a

  (** Prompt the user for a yes/no answer. *)
  val y_or_n_p : ('a, unit, string, bool) format4 -> 'a

  (** {4 Saving and reverting opened files} *)

  (** Write each modified buffer in #<file>#. *)
  val do_auto_save : unit -> unit

  (** Save the specified file if it is opened. *)
  val save_buffer_visiting : string -> unit

  (** Reread the current file. This should keep the modifications
      history for undoing, and push a new undo item on the local undo
      list. *)
  val revert_with_history : unit -> unit

  (** Reload the specified filename if it is currently opened, and
      flush its undo-list. A new undo item may be pushed which just
      calls ocp-undo, for convenience. *)
  val revert_buffer_visiting : string -> unit

  (** If the filename is opened, record that its name has changed to
      the provided new name, and flush its undo list. *)
  val rename_file : string -> string -> unit

end

module type SocketCallback =
  functor
    (Arg : sig
      val connection : Ocp_rpc.tagged_connection
    end) ->
      Callback
