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

module Map = Map.Make(struct type t = int let compare = compare end)

include Map

let to_list map =
  let list = ref [] in
  iter (fun x y -> list := (x,y) :: !list) map;
  List.rev !list

let to_list1 map =
  let list = ref [] in
  iter (fun x y -> list := x :: !list) map;
  List.rev !list

let to_list2 map =
  let list = ref [] in
  iter (fun x y -> list := y :: !list) map;
  List.rev !list

exception MinElt
let exn_MinElt = MinElt

let min_elt map =
  let x = ref None in
  try
    iter (fun key v -> x := Some (key, v); raise exn_MinElt) map;
    None
  with MinElt -> !x


