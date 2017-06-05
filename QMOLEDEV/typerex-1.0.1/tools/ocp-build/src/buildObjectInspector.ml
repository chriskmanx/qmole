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

  type inspector = {
    insp_name : string;
    insp_reader : (string -> in_channel -> obj_desc);
  }

  let inspectors = Hashtbl.create 111

  let magic_length = String.length "Caml1999Z010"

  let register_inspector magic name inspector =
    if String.length magic <> magic_length then begin
      Printf.eprintf "Bad length for magic \"%s\"\n%!" magic;
      exit 2
    end;
    if Hashtbl.mem inspectors magic then begin
      Printf.eprintf "An inspector already exists for magic \"%s\"\n%!"
	magic;
      exit 2
    end;
    Hashtbl.add inspectors magic {
      insp_reader = inspector;
      insp_name = name;
    }

  module CMI_FORMAT = struct

    let cmi_magic_number_001 = "Caml1999I001" (* csl 1.06 - csl 1.07 *)
    let cmi_magic_number_002 = "Caml1999I002" (* csl 1.10 - csl 1.15 - 1.00 - 1.05 *)
    let cmi_magic_number_003 = "Caml1999I003" (* 1.06 - 1.07 *)
    let cmi_magic_number_004 = "Caml1999I004" (* 2.00 - 2.04 *)
    let cmi_magic_number_005 = "Caml1999I005" (* 2.99 *)
    let cmi_magic_number_006 = "Caml1999I006" (* 3.00 *)
    let cmi_magic_number_007 = "Caml1999I007" (* 3.01 *)
    let cmi_magic_number_008 = "Caml1999I008" (* 3.02 - 3.05 *)
    let cmi_magic_number_009 = "Caml1999I009" (* 3.06 - 3.07 *)
    let cmi_magic_number_010 = "Caml1999I010" (* 3.08 - 3.10.2 *)
    let cmi_magic_number_011 = "Caml1999I011" (* 3.11.0 - 3.11.2 *)
    let cmi_magic_number_012 = "Caml1999I012" (* 3.12.0 - 3.12.1 *)

    let load_cmi filename ic =
      let (name, sign) = input_value ic in
      let crcs = input_value ic in
      let flags = input_value ic in
      let cmi_desc = {
	cmi_name = name;
	cmi_sign = sign;
	cmi_crcs = crcs;
	cmi_flags = flags;
      }
      in
(*      use_cmi filename cmi_file *)
      CMI cmi_desc

    let _ =
	register_inspector cmi_magic_number_012 "cmi v 12" load_cmi

  end

  module CMA_FORMAT = struct (* inspired from cmo_format.mli *)

(* Weird: TODO check again *)
    let cmo_magic_number_001 = "Caml1999O001" (* csl 1.06 - csl 1.07 *)
    let cmo_magic_number_002 = "Caml1999O002" (* csl 1.10 - csl 1.15 *)
    let cmo_magic_number_003 = "Caml1999O003" (* 1.00 - 1.05 *)
    let cmo_magic_number_004 = "Caml1999O004" (* 1.06 - 2.99 *)
    let cmo_magic_number_005 = "Caml1999O005" (* 3.00 - 3.03-alpha *)
    let cmo_magic_number_006 = "Caml1999O006" (* 3.04 - 3.07-beta1, 3.07beta2, 3.07-pl2 *)
    let cmo_magic_number_007 = "Caml1999O007" (* 3.08 - 3.12.1 *)
(*    let cmo_magic_number_008 = "Caml1999O008" (* 3.11.1-rc0 - 3.12 *) *)

    let cma_magic_number_001 = "Caml1999A001" (* csl 1.06 - csl 1.07 *)
    let cma_magic_number_002 = "Caml1999A002" (* csl 1.10 - csl 1.15 *)
    let cma_magic_number_003 = "Caml1999A003" (* 1.00 - 1.05 *)
    let cma_magic_number_004 = "Caml1999A004" (* 1.06 - 2.99 *)
    let cma_magic_number_005 = "Caml1999A005" (* 3.00 - 3.03-alpha *)
    let cma_magic_number_006 = "Caml1999A006" (* 3.04 - 3.07-beta1, 3.07beta2, 3.07-pl2 *)
    let cma_magic_number_007 = "Caml1999A007" (* 3.08 - 3.11.0-rc1 *)
    let cma_magic_number_008 = "Caml1999A008" (* 3.11.1-rc0 - 3.12.1 *)

  (* OK from 3.04 *)

    let load_cmo filename ic =
      let cmo_desc = (input_value ic : cmo_desc) in
      CMO cmo_desc

    let load_cma filename ic =
(*      Printf.eprintf "Project \"%s\"\n" ipj.ipj_name; *)
      Printf.eprintf "load_cma %s\n%!" filename;
      let pos_toc = input_binary_int ic in    (* Go to table of contents *)
      seek_in ic pos_toc;
      let lib = (input_value ic : cma_desc) in
      CMA lib

    let _ =
      register_inspector cmo_magic_number_006 "cmo v  6" load_cmo;
      register_inspector cmo_magic_number_007 "cmo v  7" load_cmo;
(*      register_inspector cmo_magic_number_008 "cmo v  8" load_cmo; *)

      register_inspector cma_magic_number_006 "cma v  6"load_cma;
      register_inspector cma_magic_number_007 "cma v  7" load_cma;
      register_inspector cma_magic_number_008 "cma v  8" load_cma;
      ()


  end

  module CMXA_FORMAT = struct

    let cmxa_magic_number_001 = "Caml1999Z001" (* csl 1.06 - csl 1.07 *)
    let cmxa_magic_number_002 = "Caml1999Z002" (* csl 1.10 - csl 1.15 *)
    let cmxa_magic_number_003 = "Caml1999Z003" (* 1.00 - 1.03 *)
    let cmxa_magic_number_004 = "Caml1999Z004" (* 1.04 - 1.05 *)
    let cmxa_magic_number_005 = "Caml1999Z005" (* 1.06 - 1.07 *)
    let cmxa_magic_number_006 = "Caml1999Z006" (* 2.00 - 2.99 *)
    let cmxa_magic_number_007 = "Caml1999Z007" (* 3.00 - 3.03-alpha *)
    let cmxa_magic_number_008 = "Caml1999Z008" (* 3.04 - 3.05 *)
    let cmxa_magic_number_009 = "Caml1999Z009" (* 3.06 - 3.07 *)
    let cmxa_magic_number_010 = "Caml1999Z010" (* 3.08 - 3.12.1 *)

    let cmx_magic_number_001 = "Caml1999Y001" (* csl 1.06 - csl 1.07 *)
    let cmx_magic_number_002 = "Caml1999Y002" (* csl 1.10 - csl 1.15 *)
    let cmx_magic_number_003 = "Caml1999Y003" (* 1.00 - 1.03 *)
    let cmx_magic_number_004 = "Caml1999Y004" (* 1.04 - 1.05 *)
    let cmx_magic_number_005 = "Caml1999Y005" (* 1.06 - 1.07 *)
    let cmx_magic_number_006 = "Caml1999Y006" (* 2.00 - 3.04 *)
    let cmx_magic_number_007 = "Caml1999Y007" (* 3.05 *)
    let cmx_magic_number_008 = "Caml1999Y008" (* 3.06 - 3.07 *)
    let cmx_magic_number_009 = "Caml1999Y009" (* 3.08.0 - 3.08.4 *)
    let cmx_magic_number_010 = "Caml1999Y010" (* 3.09.0 - 3.09.3 *)
    let cmx_magic_number_011 = "Caml1999Y011" (* 3.10 - 3.12.1 *)

  (* OK from 3.08 *)

    let load_cmx  filename ic =
      let cmx_desc = (input_value ic : cmx_desc) in
      CMX cmx_desc

    let load_cmxa filename ic =
(*      Printf.eprintf "Project \"%s\"\n" ipj.ipj_name; *)
      Printf.eprintf "load_cmxa %s\n%!" filename;
      let cmxa_desc = (input_value ic : cmxa_desc) in
      CMXA cmxa_desc

    let _ =
      register_inspector cmxa_magic_number_010 "cmxa v 10" load_cmxa;

      register_inspector cmx_magic_number_009 "cmx v  9" load_cmx;
      register_inspector cmx_magic_number_010 "cmx v 10" load_cmx;
      register_inspector cmx_magic_number_011 "cmx v 11" load_cmx;
      ()


  end


  let load_object_file filename =
    let magic = String.create magic_length in
    let ic = open_in filename in
    really_input ic magic 0 magic_length;
    let inspector =
      try
	Hashtbl.find inspectors magic
      with Not_found ->
	Printf.eprintf "No inspector for magic \"%s\"\n%!"
	  (String.escaped magic);
	exit 2
    in
    let obj_desc = inspector.insp_reader filename ic in
    close_in ic;
    obj_desc

