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

(** Exceptions handling in OCP Wizard *)

(** Exceptions are classified in two categories:
    - controled failure, and
    - unexpected error (with a short name) *)
type kind = [ `error of string | `fail ]

(** Return the kind of an exception. *)
val classify_error : exn -> kind

(** Returnand a human-readable description of it of an exception. *)
val print_error : exn -> string

(** [catch_owz ~oc f ()] calls [f ()] and catches any exception other
    than break, by applying the following protocol:
    - failure is prefixed by "Failed\n", and only the description is
    printed (on [oc])
    - errors are prefixed by "Error\n", and printed with their
    backtrace. *)
val catch_owz : oc:out_channel -> (unit -> unit) -> unit
