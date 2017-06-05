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



(* Scan directories for .ocp files, and create ocp-build.root *)
val do_scan : BuildOCPTypes.project -> unit

(* Load ocp-build.root file, and build project list *)
val do_load_ocp_files : BuildOCPTypes.project -> unit

(* clean all generated object files *)
val do_clean : BuildEngineTypes.build_context -> unit

(* clean all generated/modified files *)
val do_distclean : unit -> unit
