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

open MakeDigest

include MakeDigest.Make(struct
  let length = 16
  let name = "Md4"

external context_size : unit -> int = "md4_context_size_ml" "noalloc"
external context_init : context -> unit = "md4_context_init_ml" "noalloc"
external context_append : context -> string -> int -> int -> unit =
    "md4_context_append_ml" "noalloc"
external context_finish : string -> context -> unit =
    "md4_context_finish_ml" "noalloc"
end)
