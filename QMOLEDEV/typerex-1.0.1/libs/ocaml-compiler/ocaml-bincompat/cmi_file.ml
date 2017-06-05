(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open Env

let versions = [
  Config.cmi_magic_number, ("Current", input_cmi);
  V3120_types.cmi_magic_number, ("3.12", V3120_input.input_cmi);
  V3112_types.cmi_magic_number, ("3.11", V3112_input.input_cmi);
(*
  Types3_11.cmi_magic_number, ("3.11", Types3_11.input_cmi);
  Types3_10.cmi_magic_number, ("3.10", Types3_10.input_cmi);
*)
]

let magic_length = String.length Config.cmi_magic_number

let input_cmi filename ic magic_number =
  let version, inputer =
    try
      List.assoc magic_number versions
    with Not_found ->
      raise
        (Error(Not_an_interface filename))
  in
  try
    inputer ic
  with e ->
    raise(Error(Corrupted_interface(Printf.sprintf "%s(%s)" filename version)))

let read_cmi filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create magic_length in
    really_input ic buffer 0 magic_length;
    let cmi = input_cmi filename ic buffer in
    close_in ic;
    cmi
  with e ->
    close_in ic;
    raise
      (match e with Error _ -> e | _ -> Error(Corrupted_interface(filename)))

let read_module modname filename =
  let cmi = read_cmi filename in
  if cmi.cmi_name <> modname then
    raise(Error(Illegal_renaming(cmi.cmi_name, filename)));
  cmi
