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

let source_id, program = OcpProgram.program "tools/ocp-ide/emacsCallback.ml"
let _ = print_endline (Program.string_of_program program)
let _ = Printf.eprintf "collected %d units\n%!" (List.length program.Program.units)
let _ =
  match source_id with
    | Some f -> Printf.eprintf "source_id = %s\n%!" (Program.source2string f)
    | None -> assert false
