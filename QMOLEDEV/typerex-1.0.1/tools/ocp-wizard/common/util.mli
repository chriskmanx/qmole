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

(** Exceptions *)

(** Set this to false to stop at earlier (and get a better backtrace) *)
val catch_errors : bool ref

(** Indicates that the query cannot be processed, for some legitimate
    reason. The message will be printed on stdout before exiting with
    signal 1). *)
exception OwzFailure of string
exception Qualified of string * exn

val fail_owz : ('a, unit, string, 'b) format4 -> 'a
val fail : ('a, unit, string, 'b) format4 -> 'a
val invalid : ('a, unit, string, 'b) format4 -> 'a

(** The following functions are used to implement permissive behavior,
    by adding exceptions into a list instead of passing them to the
    caller. They take an argument [~errors], with type [exn list ref
    option], and behave as follows:
    - if the value is [None] (or if [!catch_errors] is false, then
    exceptions escape.
    - otherwise, all exceptions other than [Sys.Break] are added to the
    list instead.
    
    An optional argument [?prefix] of type [string] may be provided,
    which will be applied (using exception [Qualified]) to exceptions
    before adding them to the list (to allow for e.g., exception
    localisation). *)

val try_apply :
  ?prefix:string -> errors : exn list ref option -> 'a -> ('b -> 'a) -> 'b -> 'a
val try_default :
  ?prefix:string -> errors : exn list ref option -> 'a -> (unit -> 'a) -> 'a
val try_do :
  ?prefix:string -> errors : exn list ref option -> (unit -> unit) -> unit

val check_any : exn list -> (exn -> unit) -> unit

(** Locations *)

val loc2string : Location.t -> string
val get_c_num : Location.t -> int * int

(** Traverse a source file to get a list of locations. *)
val source_locations :
  string -> (Location.t * 'a) list -> (Location.t * string * 'a) list

(** Longidents and paths *)

val lid2string : Longident.t -> string

(**  Misc *)

val hashtbl_keys : ('a, 'b) Hashtbl.t -> 'a list
val group_by_first :
  split:('a -> 'b * 'c) -> group:('c list -> 'd) -> 'a list -> ('b * 'd) list

val remove_prefix : prefix:string -> string -> string
val extension : string -> string
