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

(* {{extend Stream}} *)
include Stream 

let to_list stream =
  let list = ref [] in
  Stream.iter (
    fun token ->
      list := token :: !list
  ) stream;
  List.rev !list

(* Build a stream from a list of lines *)
let of_lines lines =
  let lines = Array.of_list lines in
  let fn = OcpString.indexes lines in
  let elt i =
    try let n,j = fn i in Some lines.(n).[j]
    with OcpString.Out_of_bounds -> None in
  from elt

let is_empty s =
  try ignore (Stream.peek s); false
  with Stream.Failure -> true
