(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                 Thomas Gazagnaire, Fabrice Le Fessant                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(** The type for keys *)
type key =
  | CTRL
  | TAB
  | Char of char

(** The type for user-defined commands *)
type 'a command
  
(** This module abstract away the code and the language constructs for
    a given target language. It is used to auto-generate code in this
    target language. *)
module type Lang = sig

  (** The target language name *)
  val lang : string

  (** A code fragment, which returns a value of type [`a] once
      evaluated. *)
  type 'a t


  (** {2 combinators} *)

  (** [bind code f] returns the code to :
      - evaluate [code];
      - put the result in an tempory variable; and
      - apply [f] with the tempory variable as argument. 

      This corresponds to the pseudo-code [let x = <eval code> in f
      x].  *)
  val bind : 'a t -> ('a t -> 'b t) -> 'b t

  (** [seq l] takes a list of code fragment as argument, and returns
      the code to evalutate all the fragments in order. *)
  val seq  : unit t list -> unit t

  (** [run c] returns the code which run the user-defined command
      [c] *)
  val run  : 'a t command -> 'a t

  (** [exec l] returns the code which call the shell command [l]. The
      command output should be empty. *)
  val exec : string t list -> unit t

  (* callbacks *)
  val exec_with_callbacks : int -> ('a, unit, string, string) format4 -> 'a
  val process_ocp_wizard_with_callbacks : ('a, unit, string, string) format4 -> 'a


  (** {2 Filenames} *)

  (** A filename is a piece of code which returns a string *)
  type filename = string t

  (** The code to get the name of the current file *)
  val current_filename : filename

  (** The code to create a tempory file *)
  val temp_file        : filename

  (** The code to delete a file *)
  val delete_file      : filename -> unit t


  (** {2 buffers} *)

  (** A buffer is a piece of code which returns an (abstract) editor
      buffer *)
  type buffer

  (** The code to get the current buffer *)
  val current_buffer : buffer

  (** [crete_buffer name] returns the code to create a buffer
      named [name] *)
  val create_buffer : string -> buffer

  (** [mem_buffer name] returns the code to test whether a buffer
      named [name] is opened. *)
  val mem_buffer : string -> bool t

  (** [find_buffer name] returns the code to find a buffer named
      [name]. If no buffer with this name exists, the it creates a new
      one. *)
  val find_buffer : string -> buffer

  (** [exec buffer e] returns the code to execute the shell command
      [e], redirecting is outputs (stderr and stdout) to [buffer]. *)
  val exec_in_buffer : buffer -> string t list -> unit t

  (** [save_buffer buffer] returns the code to save [buffer] contents
      into a the file returned by [filename]. *)
  val save_buffer : buffer -> filename -> unit t

  (** [string_of_buffer buffer] returns the code to take a [buffer]
      and returns its content as a string. *)
  val string_of_buffer : buffer -> string t

  (** The code to save all the opened buffer. *)
  val save_buffers : unit t

  (** [eval_buffer buffer] returns a the code to evaluate the [buffer]
      contents and discard the result (ie. the evaluation should have
      side-effects). *)
  val eval_buffer : buffer -> unit t


  (** {2 Integers} *)

  (** [int i] returns the code which returns the integer [i]. *)
  val int : int -> int t

  (** [exec_int e] returns the code to execute the shell command [e]
      and return its result as an integer. It display an error message
      if it is not the case. *)
  val exec_int : string t list -> int t

  (** [string_of_int i] returns the code to translate the result
      of [i] into a string. *)
  val string_of_int : int t -> string t

  (** [int_of_string s] returns the code to translate the result of
      [s] into an integer. *)
  val int_of_string : string t -> int t

  (** {2 Strings} *)

  (** [string s] returns the code to return the string [s]. *)
  val string : string -> string t

  (** [strings l] returns the code to return the string list [l]. *)
  val strings : string list -> string list t

  (** The code to get a timestamp string *)
  val timestamp    : string t

  (** [insert s] returns the code to insert the result of [s] into
      the current buffer, at the current position *)
  val insert       : string t -> unit t

  (** [display s] returns the code to display the result of [s] to the
      user. *)
  val display      : string t -> unit t

  (** [exec_string e] returns the code to execute the shell command
      [e] and to return its outputs (stderr and stdout) as a string. *)
  val exec_string  : string t list -> string t

  (** [exec_strings e] returns the code to execute the shell command
      [e] and to return its outputs (stderr and stdout) as a list of
      lines. *)
  val exec_strings : string t list -> string list t


  (** {2 Pairs} *)

  (** The code to get the first element of a tuple. *)
  val fst : ('a * 'b) t -> 'a t

  (** The code to get the second element of a tuple. *)
  val snd : ('a * 'b) t -> 'b t


  (** {2 Identifiers} *)

  (** An identifier is a string without spaces. It corresponds to
      OCaml qualified identifiers, keywords and symbols. *)
  type ident = string t

  (** The code to get the current identifier. *)
  val current_ident        : ident

  (** The code to delete the current identifier. *)
  val delete_current_ident : unit t


  (** {2 Positions} *)

  (** A position is a code fragment which returns a integer
      (corresponding to a buffer offset. *)
  type position = int t

  (** The code to get the cursor position. *)
  val current_position : position

  (** The code to get the current line. *)
  val current_line     : position

  (** [goto_position p] returns the code to move the cursor to the
      position returned by [p]. *)
  val goto_position    : position -> unit t

  (** {2 Locations} *)

  (** A location is a code fragment which returns a pair of
      [filename] times [offset]. *)
  type location = (string * int) t

  (** [goto_location l] returns the code to go to the location
      returned by [l]. *)
  val goto_location : location -> unit t

  (** [exec_location l] returns the code to execute the shell command
      [e] and to return its output (stderr and stdout) as a location. *)
  val exec_location : string t list -> location

  (** {2 User inputs} *)

  (** [read_string l] returns the code to ask the user to choose a
      string among [l]. *)
  val read_string :
    ?prompt:string -> ?initial:string t -> ?exact:bool -> string list t -> string t

  (**/*)
  val code_of_command : 'a t command -> string
  val code_of_keymap  : ?add_hook:bool -> (string * key list list) list -> string
  (**/*)
end

(**/*)
val name : 'a command -> string
val doc  : 'a command -> string
val code : 'a command -> 'a

val create_command :
  lang:string ->
  name:string ->
  doc:string ->
  ?keys:(key list list) -> 
  'a ->
  'a command

val save :
  lang:string ->
  code_of_command:('a command -> string) ->
  code_of_keymap:((string * key list list) list -> string) ->
  filename:string ->
  unit
