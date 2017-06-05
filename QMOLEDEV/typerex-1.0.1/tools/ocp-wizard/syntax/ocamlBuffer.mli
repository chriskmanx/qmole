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

(** Following OCaml source code buffers editing *)

type buffer_state = {
  filename : string; (** absolute *)
  contents : OcamlTokenize.OCamlTokenBuffer.tokenized_buffer;
  program : (Program.program * Program.source_file_id) Lazy.t;
  local_envs : Completion.local_envs;
    (** coarse environment approx for completion (indexed by tokens) *)
  mutable needs_refontifying : (GapBuffer.mark * GapBuffer.mark) option;
    (** portion which has been modified and not yet refontified.
        This field is updated by [update_buffer], and by the
        fontifying command. *)
  mutable last_completion : (Env_untyped.path_sort * string * string Lazy.t) list
}

val buffers : (string, buffer_state) Hashtbl.t

val find_or_create :
  ?init:(unit -> string * (Program.program * Program.source_file_id) Lazy.t) ->
  string ->
  buffer_state

(** [update_buffer buffer ~start ~old_length first_time chars]
    replaces in [buffer] the [old_length] characters starting from
    [start] by [chars]. If [first_time] is true then the length is
    ignored and the buffer contents is cleared before proceeding. *)
val update_buffer :
  buffer_state -> start:int -> old_length:int -> bool -> string -> unit
