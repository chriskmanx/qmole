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

type error =
    TODO of string
  | No_Such_Feature of string * string
  | No_Such_Version of string
  | No_Such_Magic of string * string
  | Corrupted_interface of string

exception Error of error

type version =
  | V3_11_2
  | V3_12_0
  | V3_13_0


let default_version = V3_13_0
let supported_versions =
  [
    "3.11.2" , V3_11_2;
    "3.12.0" , V3_12_0;
    "3.12.1" , V3_12_0;
    "trunk" ,  V3_13_0;
  ]

module VersionMap = Map.Make(struct type t = version let compare = compare end)

let version_of_string =
  let map = ref StringMap.empty in
  List.iter (fun (sv,v) -> map := StringMap.add sv v !map) supported_versions;
  !map

let string_of_version =
  let map = ref VersionMap.empty in
  List.iter (fun (sv,v) -> map := VersionMap.add v sv !map) supported_versions;
  !map

let current_version = ref
  begin
    try
      let expected_version = Sys.getenv "OCAML_TARGET" in
      try
        StringMap.find expected_version version_of_string
      with Not_found ->
        if expected_version <> "" then begin
          Printf.fprintf stderr "ocaml-bincompat: version %s not supported\n%!"
            expected_version;
          exit 2
        end;
        default_version
    with Not_found -> default_version
  end

open Format

let report_error ppf = function
  | TODO funct -> fprintf ppf
      "Function %s@ still has to be implemented" funct
  | No_Such_Feature (version, feature) -> fprintf ppf
      "Feature %s is not available in version %s" feature version
  | No_Such_Version version -> fprintf ppf
      "Version %s@ is not supported by the compatibility layer" version
  | No_Such_Magic (kind, magic) -> fprintf ppf
      "Magic %s@ for %s is not supported by the compatibility layer"
    (String.escaped magic) kind
  | Corrupted_interface filename -> fprintf ppf
      "Corrupted compiled interface@ %s" filename
