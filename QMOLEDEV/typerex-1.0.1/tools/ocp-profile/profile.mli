(**************************************************************************)
(*                                                                        *)
(*                          Profile                                       *)
(*  Tiphaine Turpin                                                       *)
(*  Copyright 2009 INRIA Rennes - Bretagne Atlantique                     *)
(*                                                                        *)
(*  Profile is free software: you can redistribute it and/or modify       *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.                       *)
(*                                                                        *)
(*  Profile is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with Profile (see LICENSE.Profile under the root        *)
(*  directory of TypeRex).  If not, see <http://www.gnu.org/licenses/>.   *)
(*                                                                        *)
(**************************************************************************)

(** Time profiling *)

(** This module implements a flexible approach to time profiling of
    ocaml programs. We require the user to instrument the program
    explicitely, which has two advantages:
    - We avoid too fine-grained instrumentation (e.g., Pervasives.compare)
      which sometimes make the result of "-p" profiling irrelevant.
    - We can differentiate successive steps in a piece of code without
      having to put them into separate functions.

    Times are recorded for nodes (typically, functions) with and without
    child nodes (calees), and for individual edges. *)

(** {3 Usage} *)

(** The program should be instrumented using the five [time_]*
    functions (see their respective documentation) and it should
    eventually call [print_time_stats] just before termination, which
    dumps timing information in the specified file (by default
    profile.out in the current directory).

    Then, calling the executable [profile] (which only consists of the
    present module) on <file>.out (or no argument to use
    profile.out) will read this dump file and write a time-annotated
    call-graph in <file>.dot (these two steps allow tuning the dot
    graph without having to re-run a one-hour long execution each
    time, as well as alternative visualisations).

    The dot file is then processed with dot, for example with [dot
    profile.dot -Tsvg -o profile.svg] for a SVG output which may be
    rendered by web browsers.

    {b Limitation:} recursion is not supported. *)

(** {3 Interface for instrumentation} *)

(** Enter a new node with given name. *)
val time_push : string -> unit

(** Leave the topmost node on the call stack. The dynamic calls to
    [time_push] and [time_pop] should match each other (be careful
    with exceptions). *)
val time_pop : unit -> unit

(** Same as pop then push. *)
val time_switch_to : string -> unit

(** The above three functions are used as follows:

    {[
    ...
    time_push "step 1";
    ... (* code for step 1 *)
    time_switch_to "step 2";
    ... (* code for step 2 *)
    time_pop ();
    ...]} *)

(** Use e.g. [let f = time_call "f" f] to instrument f as a whole (for
    currified functions, write [let f x y = time_call "f" (f x) y]).
    [time_call] takes care of exceptions. *)
val time_call : string -> ('a -> 'b) -> 'a -> 'b

(** Controls the four [time_*] functions. Initial value is true. *)
val enabled : bool ref

(** Write raw data in profile.out. *)
val print_time_stats : ?filename:string -> unit -> unit

(** Empty the call stack and set the initial time to now *)
val time_reinit : unit -> unit

(** Reinit from existing profile data. *)
val time_reinit_from : string -> unit

(**/**)

val default_dump_filename : string

val write_graph :
  ?print_time:[ `percentage | `seconds ] ->
  float * (string * float * float) list * (string * string * float) list ->
  string -> unit

val read_stats :
  string ->
  float * (string * float * float) list * (string * string * float) list







