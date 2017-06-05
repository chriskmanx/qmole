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

include Debug.Tag(struct let tag = "cmt_env" end)

let read_cmi file =
  debugln "Loading %s" file;
  if Filename.check_suffix file ".cmi" then
    Cmi_file.read_cmi file
  else
    Cmt_format.read_cmi file

let init () =
  Clflags.cmi_exts := [ ".cmti"; ".cmt"; ".cmi" ];
  Clflags.binary_annotations := true;
  Env.read_cmi_fun := read_cmi;
  Env.EnvLazy.eager := true;
  ()
