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

(* open SafeCaml *)
open OcpSystem

exception LocalNotFound
let not_found = LocalNotFound

let scan_directory f directory =
  let queue = Stack.create () in
  Stack.push directory queue;
  while not (Stack.is_empty queue) do
    try
      let dirname = Stack.pop queue in
      let files = Sys.readdir dirname in
      Array.sort compare files;
      Array.iter (fun file ->
	let filename = Filename.concat dirname file in
	try
	  f file filename
	with LocalNotFound ->
	  Stack.push filename queue
      ) files;
    with _ -> ()
  done;
  ()

let scan_directory_for_suffix directory extension f =
  scan_directory (fun file filename ->
    if Filename.check_suffix filename extension then
	f filename
    else raise LocalNotFound) directory

let scan_directory_for_files directory extensions =
  scan_directory (fun file filename ->
    let f = try StringMap.find file extensions
      with Not_found -> raise not_found in
    f filename) directory

let scan_directory_for_extensions directory extensions =
  scan_directory (fun file filename ->
    let (_, last_ext) = File.cut_last_extension file in
    let last_ext = String.lowercase last_ext in
    let f = try StringMap.find last_ext extensions
      with Not_found -> raise not_found in
    f filename
  ) directory

(*
let scan_directory_for_extensions2 directory extensions =
  let rec iter dirname subdir =
    let files = Sys.readdir dirname in
    Array.iter (fun file ->
      let filename = Filename.concat dirname file in
      let subdir = Filename.concat subdir file in
      let (_, last_ext) = cut_last_extension file in
      let last_ext = String.lowercase last_ext in
      try
	let f = try StringMap.find last_ext extensions
	  with Not_found -> raise not_found in
	f subdir filename
      with LocalNotFound ->
	try
	  iter filename subdir
	with _ -> ()
    ) files
  in
  iter directory ""
*)
