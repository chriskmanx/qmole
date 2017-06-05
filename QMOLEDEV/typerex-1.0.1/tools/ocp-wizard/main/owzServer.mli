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

(** OCP Wizard generic server *)

open IDE

module OwzSocketServer :
  functor (SocketCallback : IDE_Callback.SocketCallback) ->
  functor (Specifics : IDE_Specifics.T) -> sig
      val start_server : in_channel -> out_channel -> 'a
    end

module MakePlugin : functor (L : Lang) ->
sig
  (* Should not be emacs-specific *)
  val make_emacs_plugin : string -> unit
end

(** Where to write time profiling information. *)
val profile_file : string

(** Command to profile *)
val profile : string option ref






