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

open Parsetree

(** A comment is start/end position in a file and a content *)
type comments = ((int * int) * string) list

(** [get_comments_from_buffer ?odocl buffer comments loc] returns the
    pair (location x content) of comments associated to the location
    [loc]. If [odoc] is set, it returns the ocamldoc comments only.

    http://caml.inria.fr/pub/docs/manual-ocaml/manual029.html states
    that:

    * A comment before an element is associated to this element if:

    - There is no blank line or another comment between the comment
    and the element. However, for odoc comments, a regular comment can
    occur between the odoc comment and the element.

    - The comment is not already associated to the previous
    element. WARNING: this is not taken into account by this
    function (as we are doing only local search only).

    -The comment is not the first one of a toplevel module. WARNING:
    this is not taken into accound by this function (as we are doing
    local search only).

    * A comment after an element is associated to this element if there
    is no blank line or comment between the comment and the element. *)
val get_comments_from_buffer :
  ?odoc:bool -> string -> (string * Location.t) list -> Location.t -> comments

(** [get_comments ?odocl loc] returns the pair (location x content) of
    comments associated to the location [loc]. If [odoc] is set, it
    returns the ocamldoc comments only. The location filename MUST
    be set correclty *)
val get_comments : ?odoc:bool -> Location.t -> comments

(** TypeRex IDE-friendly version. *)
val get_comments_permissive : ?odoc:bool -> Location.t -> comments

(** [pretify_comment c] removes starting "(*" and trailing "*)" and
    all the unnecessary "*" in the middle of the comment *)
val pretify_comment : string -> string

(** Pretty-print a char location *)
val pprint_c_loc : int * int -> unit

(** Qualified name : [String.iter] is represented as {[ { modules =
    ["String"]; name = "iter" } ]} *)
type path = {
  modules : string list;
  name    : string;
}

val path_of_string : string -> path

module type Sig = sig

  (** [t] is either a structure or a signature *)
  type t

  (** [item] is either a structure item or a signature item *)
  type item

  (** [get_by_pos ast p] returns the structure item at position [p] in [ast] *)
  val get_by_pos : t -> int -> item

  (** [get_by_type_name ast t] returns the type named [t] in [ast] *)
  val get_type_by_name : t -> path -> item

  (** [get_by_value_name ast v] returns the last value named [v] in [ast] *)
  val get_value_by_name : t -> path -> item

  (** [get_by_module_name ast m] returns the module named [m] in [ast] *)
  val get_module_by_name : t -> path -> item

  (** Return the location of an item *)
  val get_loc : item -> Location.t

  (** Pretty-print an item *)
  val pprint_item : item -> unit
end

(** Extraction in .ml files *)
module Structure : Sig
  with type t = structure
  and  type item = structure_item

(** Extraction in .mli files *)
module Signature : Sig
  with type t = signature
  and  type item = signature_item
