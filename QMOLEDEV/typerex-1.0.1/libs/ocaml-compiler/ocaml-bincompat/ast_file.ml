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

exception Outdated_version

module INTERFACE = struct

  let input_ast_intf ic =
    let input_name = input_value ic in
    let intf = input_value ic in
    (input_name, intf)

  let versions =
  [
    Config.ast_intf_magic_number, ("Current", input_ast_intf);
    V3120_types.ast_intf_magic_number, ("3.12", V3120_input.input_ast_intf);
    V3120_camlp4.camlp4_ast_intf_magic_number,
    ("3.12", V3120_camlp4.input_camlp4_ast_intf);
  ]

  let ast_magic_number_len = String.length Config.ast_intf_magic_number
  let camlp4_ast_magic_number_len =
    String.length V3120_camlp4.camlp4_ast_intf_magic_number
  let camlp4_more_magic = camlp4_ast_magic_number_len - ast_magic_number_len

  let ast_magic_prefix = "Caml1999N"

  let read_interface ic =
    let magic =
      String.create (max ast_magic_number_len camlp4_ast_magic_number_len) in
    let pp =
    try
      let version, input =
        try
          really_input ic magic 0 ast_magic_number_len; (* the min of the two *)
          List.assoc (String.sub magic 0 ast_magic_number_len) versions
        with _ ->
          really_input ic magic ast_magic_number_len camlp4_more_magic;
          List.assoc (String.sub magic 0 camlp4_ast_magic_number_len) versions
      in
      Some input
    with
      | Not_found when
          String.sub magic 0 (String.length ast_magic_prefix) =
                    ast_magic_prefix ->
        raise Outdated_version
      | _ -> None
    in
    match pp with
      | Some input -> Some (input  ic)
      | None -> None

  let read_implementation ic = None

end


module IMPLEMENTATION = struct

  let input_ast_impl ic =
    let input_name = input_value ic in
    let impl = input_value ic in
 (input_name, impl)

  let versions =
  [
    Config.ast_impl_magic_number, ("Current", input_ast_impl);
    V3120_types.ast_impl_magic_number, ("3.12", V3120_input.input_ast_impl);
    V3120_camlp4.camlp4_ast_impl_magic_number,
    ("3.12", V3120_camlp4.input_camlp4_ast_impl);
  ]

  let ast_magic_number_len = String.length Config.ast_impl_magic_number
  let camlp4_ast_magic_number_len =
    String.length V3120_camlp4.camlp4_ast_impl_magic_number
  let camlp4_more_magic = camlp4_ast_magic_number_len - ast_magic_number_len

  let ast_magic_prefix = "Caml1999M"

  let read_implementation ic =
    let magic =
      String.create (max ast_magic_number_len camlp4_ast_magic_number_len) in
    let pp =
    try
      let version, input =
        try
          really_input ic magic 0 ast_magic_number_len; (* the min of the two *)
          List.assoc (String.sub magic 0 ast_magic_number_len) versions
        with _ ->
          really_input ic magic ast_magic_number_len camlp4_more_magic;
          List.assoc (String.sub magic 0 camlp4_ast_magic_number_len) versions
      in
      Some input
    with
      | Not_found when
          String.sub magic 0 (String.length ast_magic_prefix) =
                    ast_magic_prefix ->
        raise Outdated_version
      | _ -> None
    in
    match pp with
      | Some input -> Some (input  ic)
      | None -> None

end

let read_interface = INTERFACE.read_interface
let read_implementation = IMPLEMENTATION.read_implementation









