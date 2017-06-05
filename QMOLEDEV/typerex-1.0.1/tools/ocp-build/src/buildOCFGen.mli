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

(*
(***********************************************************************)
(*                                                                     *)
(*                TypeRex : OCaml Development Tools                    *)
(*                                                                     *)
(*                       OCamlPro S.A.S.                               *)
(*                                                                     *)
(*  Copyright 2011 OCamlPro SAS                                        *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(***********************************************************************)



(* [find_installed dirname] generates a file 'ocp-installed.ocp' in
directory [dirname] with a list of already installed files, that
should not be regenerated. *)
val find_installed : string -> unit

(* [gen_from_distrib dirname] generates a list of files of the form
 'ocp-installed-*.ocp' in ~/.ocp/ corresponding to the descriptions
  of all the libraries in the current distribution. *)
val gen_from_distrib : string -> unit

(* [autogen dirname target]: generate file 'build.ocp' in [dirname]
  describing a project [target], built from all the source files inside
  the directory [dirname]. *)
val autogen : string -> string -> unit
*)

