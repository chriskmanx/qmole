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

(** OCaml (ident) completion. *)

(** Completion-specific data associated with the buffer *)
type local_envs

val initial_env : (Program.program * Program.source_file_id) Lazy.t -> local_envs
val invalidate_env_after : local_envs -> int -> unit

(** [completion buffername pos] returns, if any, the relevant prefix
    just preceeding position pos (possibly empty), and a list of
    candidates which start with this prefix. Each candidate is also
    tagged with its sort, and documentation (which is kept lazy for
    performance reasons. The candidate list is also stored in the
    buffer associated data. *)
val completions :
  Program.program -> Program.source_file_id ->
  OcamlTokenize.OCamlTokenBuffer.tokenized_buffer -> local_envs ->
  int ->
  string * (Env_untyped.path_sort * string * string Lazy.t) list




















