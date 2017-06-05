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

type unused

type cmo_desc = {
  cu_name: string;                    (* Name of compilation unit *)
  mutable cu_pos: unused;
  cu_codesize: unused;
  cu_reloc: unused;
  cu_imports: (string * Digest.t) list; (* Names and CRC of intfs imported *)
  cu_primitives: string list;         (* Primitives declared inside *)
  mutable cu_force_link: bool;        (* Must be linked even if unref'ed *)
  mutable cu_debug: int;              (* Position of debugging info, or 0 *)
  cu_debugsize: unused; }

type cma_desc =
    { cma_units: cmo_desc list;   (* List of compilation units *)
      cma_custom: bool;                   (* Requires custom mode linking? *)
      cma_ccobjs: string list;            (* C object files needed for -custom *)
      cma_ccopts: string list;            (* Extra opts to C compiler *)
      cma_dllibs: string list; }          (* 3.04, DLLs needed *)


type cmi_flag = Rectypes

type cmi_desc = {
  cmi_name : string;
  cmi_sign : unused;
  cmi_crcs : (string * Digest.t) list;
  cmi_flags : cmi_flag list;
}

type cmx_desc =
    { mutable ui_name: string;                    (* Name of unit implemented *)
      mutable ui_symbol: string;                  (* Prefix for symbols *)
      mutable ui_defines: string list;            (* Unit and sub-units implemented *)
      mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
      mutable ui_imports_cmx: (string * Digest.t) list; (* Infos imported *)
      mutable ui_approx: unused;                  (* Approx of the structure *)
      mutable ui_curry_fun: unused;               (* Currying functions needed *)
      mutable ui_apply_fun: unused;               (* Apply functions needed *)
      mutable ui_send_fun: unused;                (* Send functions needed *)
      mutable ui_force_link: bool }               (* Always linked *)

type cmxa_desc =
    { cmxa_units: (cmx_desc * Digest.t) list;  (* List of unit infos w/ CRCs *)
      cmxa_ccobjs: string list;            (* C object files needed *)
      cmxa_ccopts: string list }           (* Extra opts to C compiler *)

type obj_desc =
    CMO of cmo_desc
  | CMA of cma_desc
  | CMI of cmi_desc
  | CMX of cmx_desc
  | CMXA of cmxa_desc

(* Load the description of some object files *)
val load_object_file : string -> obj_desc
