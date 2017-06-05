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

open Printf

let enabled = ref true

let default_dump_filename = "profile.out"

type state = {
  mutable init_time : float;
  mutable stack : (string * float) list;
    (* The (abstract) call stack *)
  edges : (string * string, float ref) Hashtbl.t
    (* Maps each pair of functions to the time spent in this context *)
}

let initial_state () =
  let init_time = Unix.gettimeofday () in
  {
    init_time = init_time;
    stack = ["main", init_time];
    edges = Hashtbl.create 100
  }

let state = initial_state ()

let reinit_state state =
  state.init_time <- Unix.gettimeofday ();
  state.stack <- ["main", state.init_time];
  Hashtbl.clear state.edges

let time_reinit () = reinit_state state

let lookup {edges = edges} e =
  try
    Hashtbl.find edges e
  with Not_found ->
    let r = ref 0. in
      Hashtbl.add edges e r;
      r

(* Operations for instrumentation *)
let time_op state op =
  if !enabled then
  let time = Unix.gettimeofday () in
    (match op with
       | `push f ->
	 state.stack <- (f, time) :: state.stack
       | `pop ->
	   (match state.stack with
	      | (f, t) :: ((f', _) :: _ as st) ->
		  let r = lookup state (f', f) in
		    r := ! r +. time -. t;
		    state.stack <- st
	      | [] | [_] -> () (*invalid_arg "time_pop"*))
       | `switch_to f'' ->
	   (match state.stack with
	      | (f, t) :: ((f', _) :: _ as st) ->
		  let r = lookup state (f', f) in
		    r := ! r +. time -. t;
		    state.stack <- (f'', time) :: st
	      | [] | [_] -> ()(*invalid_arg "time_switch_to"*))
    )

let time_switch_to op = time_op state (`switch_to op)
let time_push op = time_op state (`push op)
let time_pop () = time_op state `pop

let time_call name f x =
  (* try ... finally *)
  time_push name;
  try
    let r = f x in
      time_pop ();
      r
  with e when !enabled (* avoids corrupting backtraces *) ->
    time_pop ();
    raise e

(*
  let time_call _ = identity
*)

let time_stack c =
  match state.stack with
    | [] -> ()
    | (f, _) :: stack ->
      fprintf c "%s" f;
      List.iter
	(function f, _ ->
	  fprintf c "->%s" f)
	stack

let hashtbl_keys t =
  Hashtbl.fold
    (fun k _ l ->
      match l with
	| k' :: _ as l when k = k' -> l
	| l -> k :: l)
    t
    []

let remove_duplicates l =
  List.fold_right
    (fun x l -> if List.mem x l then l else x :: l)
    l
    []

(* Computing node values from edge values *)
let time_stats state =
  while List.tl state.stack <> [] do time_pop () done;
  let total_time = Unix.gettimeofday () -. state.init_time in
  let nodes =
    "main" :: (List.map (function _, f -> f) (hashtbl_keys state.edges)) in
  let nodes = remove_duplicates nodes in
  let node_times =
    List.map
      (function f ->
        f,
        (if f = "main" then total_time else
	    Hashtbl.fold
	      (fun (_, f') r n -> if f' = f then !r +. n else n)
	      state.edges
	      0.),
	Hashtbl.fold
	  (fun (f', _) r n -> if f' = f then !r +. n else n)
	  state.edges
	  0.)
      nodes
  in
  total_time, node_times

(* Writing data to a file *)
let write_stats {edges = edges} (total_time, node_times) filename =
  let c = open_out filename in
  fprintf c
    "total time: %f, monitored functions: %d\n"
    total_time (List.length node_times);
  List.iter
    (function f, tin, tout ->
      fprintf c "function %S total %f calees %f\n"
	f tin tout)
    node_times;
  Hashtbl.iter
    (fun (f, f') t ->
      fprintf  c "edge %S calling %S total %f\n"
	f f' !t)
    edges;
    close_out c

let rec parse_lines f =
  try
    let i = input_line f in
      (i ^ "\n") :: parse_lines f
  with
      End_of_file -> []

let lines_of f =
  let c = open_in f in
  let d = parse_lines c in
    close_in c;
    d

let rec cut acc = function
  | 0, l -> List.rev acc, l
  | n, t :: q -> cut (t :: acc) (n-1, q)
  | _, [] -> invalid_arg "cut"

let cut n l = cut [] (n, l)

(* Reading saved data *)
let read_stats filename =
  match lines_of filename with
    | first :: lines ->
      let total_time, n =
	Scanf.sscanf
	  first
	  "total time: %f, monitored functions: %d" (fun t n -> t, n)
      in
      let nodes, edges = cut n lines in
      let nodes =
	List.map
	  (function l ->
	    Scanf.sscanf l
	      "function %S total %f calees %f"
	      (fun f tin tout -> f, tin, tout))
	  nodes
      in
      let edges =
	List.map
	  (function s ->
	    Scanf.sscanf s
	      "edge %S calling %S total %f"
	      (fun f f' t -> f, f', t))
	  edges
      in
      total_time, nodes, edges
    | [] -> invalid_arg "read_stats"

let time_reinit_from filename =
  time_reinit ();
  if Sys.file_exists filename then
    let total, _, edges = read_stats filename in
    state.init_time <- state.init_time -. total;
    List.iter
      (function f, g, t -> Hashtbl.add state.edges (f, g) (ref t))
      edges

let print_time_stats ?(filename=default_dump_filename) () =
  write_stats state (time_stats state) filename

(* dot graph output *)
let write_graph
    ?(print_time=`percentage) (total_time, node_times, edge_times) filename =
  let print t c =
    match print_time with
      | `percentage -> fprintf c "%4.1f" (t  *. 100. /. total_time)
      | `seconds ->
        if total_time > 1000. then fprintf c "%.0f" t
        else if total_time > 100. then fprintf c "%.1f" t
        else if total_time > 10. then fprintf c "%.2f" t
        else if total_time > 1. then fprintf c "%.3f" t
        else fprintf c "%.4f" t
  and fill_color tin tout c =
    let h = 0.5
    and s = 0. +. 1. *. ((tin-.tout) /. total_time) ** 0.7
    and v = 1. in
    fprintf c "\"%f,%f,%f\"" h s v
  and color tin tout c =
    let h = 0.5
    and s = 0. +. 1. *. ((tin-.tout) /. total_time) ** 0.7
    and v = 1. -. (tin /. total_time) ** 1. in
    fprintf c "\"%f,%f,%f\"" h s v
  and edge_color t c =
    let h = 0.
    and s = 0.
    and v = 0.95 *. (1. -. (t /. total_time) ** 0.8) in
    fprintf c "\"%f,%f,%f\"" h s v
  and text_color t c =
    let h = 0.
    and s = 0.
    and v = 0.85 *. (1. -. (t /. total_time) ** 0.8) in
    fprintf c "\"%f,%f,%f\"" h s v
  in
  let node_times =
    List.sort (fun (_, t, _) (_, t', _) -> compare t' t) node_times
  in
  let c = open_out filename in
  fprintf c
    "digraph profile {\n\
       rankdir=LR;\n\
       splines=true;\n\
       node [shape=ellipse];\n\
       node [style=\"filled,bold\"];\n\
       node [fontsize=\"10pt\"];\n\
       edge [style=bold];\n";
  List.iter
    (function f, tin, tout ->
      fprintf c
         "\"%s\" [\
           label=\"\\N\\n\\n%t / %t\",\
           fontcolor=%t,\
           color=%t,\
           fillcolor=%t];\n"
	f (print tin) (print (tin-.tout))
	(text_color tin) (color tin tout) (fill_color tin tout))
    node_times;
  List.iter
    (function f, f', t ->
      fprintf c
         "\"%s\" -> \"%s\" [\
           label=\"%t\",\
           color=%t,\
           fontcolor=%t];\n"
	f f' (print t) (edge_color t) (text_color t))
    edge_times;
  fprintf c "}";
(*
           weight=$d:int_of_float t$,\
*)
    close_out c

