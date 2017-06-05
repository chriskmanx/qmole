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

open Location
open Lexing

open Asttypes
open Longident
open Format
include Debug.Tag(struct let tag = "util" end)

let catch_errors = ref true

let loc2string loc =
  Location.print Format.str_formatter loc;
  Format.flush_str_formatter ()

let get_c_num loc =
  loc.Location.loc_start.Lexing.pos_cnum,
  loc.Location.loc_end.Lexing.pos_cnum

let rec lid2string = function
    Lident s -> s
  | Ldot(lid, s) -> lid2string lid ^ "." ^ s
  | Lapply(l1, l2) -> lid2string l1 ^  "(" ^ lid2string l2 ^ ")"

let source_locations file locs =
  match locs with
    | [] -> []
    | _ ->
      let c = open_in file in
      let acc =
	List.fold_left
	  (fun acc (loc, x) ->
	    let len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
	    let s = String.create len in
	    seek_in c loc.loc_start.pos_cnum;
	    really_input c s 0 len;
	    (loc, s, x) :: acc)
	  []
	  locs
      in
      close_in c;
      List.rev acc

exception OwzFailure of string
exception Qualified of string * exn

let fail_owz s = raise (OwzFailure s)

let fail_owz s = Printf.ksprintf fail_owz s
let fail s = Printf.ksprintf failwith s
let invalid s = Printf.ksprintf invalid_arg s

let try_apply ?prefix ~errors v f x =
  match errors with
    | Some es ->
      (try f x
       with e when !catch_errors && e <> Sys.Break ->
         let backtrace = Printexc.get_backtrace () in
         debugln "\ncatching %s\n%s" (Printexc.to_string e) backtrace;
         let e = match prefix with
           | Some p -> debugln "prefix is: %s" p; Qualified (p, e)
           | None -> e
         in
         es := e :: !es;
         v)
    | None -> f x

let try_default ?prefix ~errors v f = try_apply ?prefix ~errors v f ()

let try_do ?prefix ~errors = try_default ?prefix ~errors ()

let check_any errors cond =
  let rec check = function
    | Qualified (_, e) -> check e
    | e -> cond e
  in
  List.iter check errors

let hashtbl_keys t =
  Hashtbl.fold
    (fun k _ l ->
      match l with
	| k' :: _ as l when k = k' -> l
	| l -> k :: l)
    t
    []

(** Sort a list of locations by file and order. *)
let group_by_first ~split ~group l =
  let t = Hashtbl.create 2 in
  List.iter
    (function x ->
      let key, value = split x in
      Hashtbl.add t key value)
    l;
  List.map
    (function k -> k, group (Hashtbl.find_all t k))
    (hashtbl_keys t)

let remove_prefix ~prefix s =
  let l = String.length prefix
  and l' = String.length s in
  if l' >= l && String.sub s 0 l = prefix then
    String.sub s l (l' - l)
  else
    raise Not_found


let extension f = remove_prefix ~prefix:(Filename.chop_extension f) f
