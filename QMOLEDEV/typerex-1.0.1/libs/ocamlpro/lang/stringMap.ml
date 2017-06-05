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


module Map = Map.Make(String)

include Map

let of_list list =
  let map = ref empty in
  List.iter (fun (x,y) -> map := add x y !map) list;
  !map

let to_list map =
  let list = ref [] in
  iter (fun x y -> list := (x,y) :: !list) map;
  List.rev !list

let to_list_of_keys map =
  let list = ref [] in
  iter (fun x y -> list := x :: !list) map;
  List.rev !list
