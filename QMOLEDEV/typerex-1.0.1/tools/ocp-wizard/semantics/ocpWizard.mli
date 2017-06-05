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

(** OCP Wizard API *)

(** The following functions always manipulate locations which are
    relative to the last, possibly auto-saved version of the
    sources. Querries are specified using ranges, which are the most
    flexible since a position p may be encoded as \[p, p+1\[. *)

type region = [`lc of (int * int) * (int * int) | `cnum of int * int]

(** Find the global ident of which the definition, or an occurrence,
    appears at the specified position. *)
val locate_ident_from_def_or_use :
  Program.program -> Program.source_file_id -> region ->
  Env_untyped.path_sort * Ident.t * Env_untyped.description * [ `ref of Path.t | `def ]

(** Return the "visible" gobal ident whose definition contains the
    specified range, as well as this ident's location. *)
val visible_ident_definition :
  [ `innermost | `outermost ] -> Program.source_file_id -> region ->
  Program.program ->
  Env_untyped.path_sort * Ident.t * Location.t

(** Try to find any comments associated with an identified. This
    function never raises an exception. *)
val id2comments : Program.program -> Ident.t -> string option

type replaced_contents = [`id of string | `lid of Longident.t]

(** Compute a renaming operation and return the necessary replacements. *)
val rename :
  ?errors : exn list ref -> 
  ?restrict_propagation : bool ->
  (** Specifies if we should we prevent renaming propagation arising
      from e.g., (M : S) ?  (default false) *)
  Env_untyped.path_sort ->
  Ident.t ->
  string ->
  Program.program ->
  (Location.t * (replaced_contents * string)) list *
    (string * string) list *
    (Location.t * (replaced_contents * string)) list

(** Prune all longidents appearing in the givn source file w.r.t. open
    statements. *)
val prune :
  Program.program -> Program.source_file ->
  (Location.t * (replaced_contents * string)) list

val eliminate_open :
  ?fname:string ->
  [ `sig_open of
      Typedtree.signature * Typedtree.signature_item * Path.t *
        Longident.t Location.loc
  | `str_open of
      Typedtree.structure * Typedtree.structure_item * Path.t *
        Longident.t Location.loc
  | `exp_open of
      Path.t * Longident.t Location.loc * Typedtree.expression ] ->
  (Location.t * ([>replaced_contents] * string)) list

(** Perform a grep and return the found defs and uses, respectively. *)
val grep :
  ?errors : exn list ref -> 
  ?prefix:Program.prefix_option ->
  Env_untyped.path_sort ->
  Ident.t ->
  Program.program ->
  Location.t list * string list * Location.t list

(** Find the locations of all the "related" definitions of an ident
    (typically ml/mli). This is used to implement cycle-definitions. *)
val all_defs :
  ?errors : exn list ref -> 
  Env_untyped.path_sort -> Ident.t -> Program.program -> Location.t list





