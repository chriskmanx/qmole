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

(** Callbacks to Eclipse *)

(** The functions provided in this functor are available to any OCaml
    program started from eclipse with [call-process-with-callbacks],
    defined in callback.el, with the same port number. *)

(** Raised if an error occurs during the reading of the command sent
    to Emacs (i.e., a parse error). The string arguments are
    - the description of the error
    - the command whose reading failed. *)
exception CallbackReadError of string * string

(** Raised if an error occurs during the evaluation of the lisp form
    by Emacs. The string argument is the printing of the lisp error
    itself, of the form "(error-condition-name arguments)". *)
exception ErrorInCallback of string

(** Emacs editor interface *)
module Make : IDE_Callback.SocketCallback
