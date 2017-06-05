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

open OcpLang
include Debug.Tag(struct let tag = "align" end)

let align cost u v =
  (* m.(i).(j) containt the best alignment of u[0--i[ with v[0--j[,
     together with its score. *)
  let n_u = Array.length u and n_v = Array.length v in
  let m = Array.make_matrix (n_u + 1) (n_v + 1) ([], 0.) in
  for i = 1 to n_u do
    let s, c = m.(i-1).(0) and char = `u u.(i-1) in
    m.(i).(0) <- char :: s, c +. cost char
  done ;
  for j = 1 to n_v do
    let s, c = m.(0).(j-1) and char = `v v.(j-1) in
    m.(0).(j) <- char :: s, c +. cost char
  done ;
  for i = 1 to n_u do
    for j = 1 to n_v do
      let options = [
	m.(i-1).(j-1), `subst (u.(i-1), v.(j-1));
	m.(i).(j-1), `v v.(j-1);
	m.(i-1).(j), `u u.(i-1)
      ] in
      m.(i).(j) <-
	List.fold_left
	(fun (best_s, best_c as best) ((s, c), opt) ->
	  let c' = c +. cost opt in
	  if c' < best_c then
	    opt :: s, c'
	  else
	    best)
	([], infinity)
	options
    done
  done;
  m.(n_u).(n_v)

let rec index_from_cond cond s length start =
  if start = length || cond s.[start] then
    start
  else
    index_from_cond cond s length (start + 1)

let rec split_cond cond s length start =
  if start = length then
    []
  else
    let i = index_from_cond (cond true) s length start in
    let j = index_from_cond (cond false) s length i in
    let l = split_cond cond s length j in
    if i = start || i = j then
      String.sub s start (j - start) :: l
    else
      String.sub s start (i - start) :: String.sub s i (j - i) :: l

let split_cond cond s =
  split_cond (fun b c -> b = cond c) s (String.length s) 0

let split_blanks =
  split_cond (function c -> List.mem c [' '; '\t'; '\n'])

let string2array s = Array.init (String.length s) (String.get s)

let blank = String.for_all (function c -> List.mem c [' '; '\t'; '\n'])

let cost = function
  | `subst (a, b) ->
    if a = b then 0.
    else if blank a && blank b then 0.05
    else if not (blank a || blank b) then 0.5
    else 1.
  | `u _ | `v _ -> 0.3 (* > 0.5 / 2 *)

let align_words s s' =
  let s = Array.of_list (split_blanks s)
  and s' = Array.of_list (split_blanks s') in
  debugln "computing alignment";
  let a, _ = align cost s s' in
  debugln "Done";
  List.rev a

let show =
  List.fold_left
    (fun (s1, s2) c ->
      let len = String.length s1
      and len' = match c with
        | `subst (t1, t2) -> max (String.length t1) (String.length t2)
        | `u t | `v t -> String.length t
      in
      let s1' = String.make (len + len') '_' in
      let s2' = String.copy s1' in
      String.blit s1 0 s1' 0 len;
      String.blit s2 0 s2' 0 len;
      (match c with
        | `subst (t, _) | `u t -> String.blit t 0 s1' len (String.length t)
        | `v _ -> ());
      (match c with
        | `subst (_, t) | `v t -> String.blit t 0 s2' len (String.length t)
        | `u _ -> ());
      s1', s2')
    ("", "")

let present a =
  let u =
    List.fold_right
      (fun c s ->
	let c = match c with | `subst (c, _) | `u c -> c | `v _ -> '_'  in
	s ^ String.make 1 c)
      a
      ""
  and v =
    List.fold_right
      (fun c s ->
	let c = match c with | `subst (_, c) | `v c -> c | `u _ -> '_'  in
	s ^ String.make 1 c)
      a
      ""
  in
  print_endline u ; print_endline v

(*
let align s s' = align cost (string2array s) (string2array s')

let a, _ = align "some text" "some more text"
let _ = present a
*)
