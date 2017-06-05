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

open Hashtbl

let to_list h =
  fold (fun k v accu -> (k,v) :: accu) h []

let of_list l =
  let h = create (List.length l) in
  List.iter (fun (k,v) -> add h k v) l;
  h

let incr h key =
  if mem h key then
    let n = find h key in
    replace h key (n+1)
  else
    add h key 1

let for_all h fn =
  fold (fun k v accu -> accu && fn k v) h true

let exists h fn =
  fold (fun k v accu -> accu || fn k v) h false
