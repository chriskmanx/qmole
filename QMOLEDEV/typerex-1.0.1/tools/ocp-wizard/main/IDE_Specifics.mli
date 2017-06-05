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

(** IDE-specific message format for some ocp-wizard functions *)

open IDE_Callback

module type T = sig
  (** This function should computes the answer that the ide expects to
      a [modify-buffer] command, currently fontifying information and
      inline help. arguments are the modified range (which should
      first be cleared from existing faces and helps), a list of
      faces, each with a list of ranges to apply to, and a list of
      inline helps to put. *)
  val return_fontifying_data :
    int -> int ->
    (Face.face * (int * int) list) list ->
    ((int * int) * string) list ->
    (int * int) ->
    string

  (** This function should computes the answer that the ide expects to
      a [complete] command. arguments are the position, the prefix
      string to complete (with the relevant definition of "word"), and
      then list of candidates (with a one-char description of their
      kind). *)
  val return_completion_data :
    int -> 
    prefix:string ->
    (char * string) list ->
    string

  (** Maybe it's not a good idea to remove grep results from IDE. We
      need to find a more general interface anyway for Eclipse. *)

  (** This function should computes the answer that the ide expects to
      a [grep] command. Arguments are:
      - abolute path to the project's root
      - grepped kind
      - grepped ident
      - lists of def, refs, (sorted by relative filename), and files
      - relative filename of the current buffer
      - errors, if any. *)
  val return_grep_results :
    root:string ->
    Env_untyped.path_sort ->
    Ident.t ->
    (string * (Location.t * int * int * string) list) list *
    (string * (Location.t * int * int * string) list) list *
    string list ->
    current:string ->
    errors:exn list ->
    string

  val config_info : string

end

module Emacs : T
module Eclipse : T



















