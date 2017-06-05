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

(** Editor interface for OCP Wizard functions *)

(** Functions in this module may raise exceptions, but only of the
    kind [`fail] as defined by [Exceptions.classify_errors]. *)

val ignore_project_file : bool ref
val default_cwd : bool ref

(** {2 Non-interactive commands} *)

val modify :
  buffername:string -> filename:string -> first_time:bool ->
  start:int -> old_length:int -> string list -> unit
val colorize :
  string ->
  (int * int) *
  (Face.face * (int * int) list) list *
  ((int * int) * string) list *
  (int * int)
val completion : string -> int -> string * (char * string) list
val last_completion_doc : string -> candidate:string -> string
val pre_cache_buffer : string -> unit

(** {2 Fully interactive UI (needs callbacks)} *)

module Make :
  functor (IDE : IDE_Callback.Callback) ->
  functor (Specifics : IDE_Specifics.T) -> sig

  val rename : bool -> unit
  val prune_lids : unit -> unit
  val eliminate_open : unit -> unit
  val grep : bool -> string
  val comment_definition : unit -> unit
  val goto_definition : unit -> unit
  val cycle_definitions : unit -> unit
  val undo_last : unit -> unit

  val colorize : string -> string
  val completion : string -> int -> string
  val callback_test : unit -> unit

end

(** {2 Callback-free API for some commands} *)

(** These functions are for an hypothetical callback-less interface
    with some other editor. *)

val comment_definition :
  Program.program -> Program.source_file_id -> ProgramCache.pos -> string
val goto_definition :
  Program.program -> Program.source_file_id -> ProgramCache.pos -> Location.t
val cycle_definitions :
  Program.program -> Program.source_file_id -> ProgramCache.pos -> Location.t
val prune_lids :
  errors:exn list ref -> Program.program -> Program.source_file_id ->
  string * (int * int * string) list
val eliminate_open :
  errors:exn list ref -> Program.program -> Program.source_file_id -> ProgramCache.pos ->
  string * (int * int * string) list










