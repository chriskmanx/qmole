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

(** Reading modified text files. *)

(** This module calls the diff -f command. *)

(** A chunk of a modified file is either an unmodified portion or
    modified one, which has an old and a new versions. Chunks may
    contain newline characters. *)
type chunk =
  | Same of string
  | Changed of string * string

(** Read a modified file as a list of chunks, given the old and the
    new versions of the file. Absent files are treated as empty by
    default.

    @raise [Failure] if executing 'diff' fails for any reason. *)
val read_modified_file : ?empty_absent:bool -> string -> string -> chunk list

val read_unmodified_file : string -> chunk list

(** Translating character counts from one version of the file to the
    other. The line count option is rather inaccurate ; use cnum2lnum
    instead. *)

val old2last : [`char | `line] -> chunk list -> int -> int
val last2old : [`char | `line] -> chunk list -> int -> int

(** Return the line number (starting from 1), and the number of chars
    since the last newline *)
val cnum2lnum : [`old | `last] -> chunk list -> int -> int * int

(** Return the absolute char number (starting from 0).  *)
val lnum2cnum : [`old | `last] -> chunk list -> int -> int -> int

(** Cut a modified file, only keeping the count first characters of
    the new version. *)
val cut_new : int -> chunk list -> chunk list

(** Print a modified file in a somewhat readable form. *)
val print_modified : out_channel -> chunk list -> unit

(*
(** parse_with_errors parse file diff return the result of the best
    merge of diff for which parse succeeds. The string argument given
    to the parse function is the temporary filename in which
    candidates are written. The resturned function may be used to
    translate positions seen by the parser to positions in the new
    version of the file (only pos_cnum, and this may yield duplicate
    positions). *)
val parse_with_errors :
  (string -> 'a) -> string -> chunk list ->
  'a * (Lexing.position -> Lexing.position)
*)
