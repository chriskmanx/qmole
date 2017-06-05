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

(* $Id$ *)

(***********************************************************************)
(**                                                                   **)
(**               WARNING WARNING WARNING                             **)
(**                                                                   **)
(** When you change this file, you must make the parallel change      **)
(** in config.mlbuild                                                 **)
(**                                                                   **)
(***********************************************************************)

open SimpleConfig
open OcpLang

let fail fmt = Printf.ksprintf (function s -> prerr_string s ; exit 2) fmt

(* Tifn: users should be respondible for exitting in the most
   appropriate way. *)
let fail fmt = Printf.ksprintf failwith fmt

(* The main OCaml version string has moved to ../VERSION *)
let version = Sys.ocaml_version

let homedir =
  try
    Sys.getenv "HOME"
  with Not_found ->
    fail "Fatal error: environment variable HOME not set\n%!"

let homedir = File.of_string homedir
let ocp_dir = File.add_basename homedir ".ocp"
let ocaml_conf = File.add_basename ocp_dir "ocaml.conf"

let config_file = SimpleConfig.create_config_file ocaml_conf

(* TODO : the only case where we should allow the absence of the configuration file
is when we are building ocamlc/ocamlopt themselves, in which case we should actually
NOT USE the information from ~/.ocp/ocaml.conf *)

let _ =
  if File.X.exists ocaml_conf then
    SimpleConfig.load config_file

let ocamllib = create_option config_file
    [ "ocamllib" ]
    ["Where OCaml libraries should be found (`ocamlc -where`)."]
    (option_option string_option) None

let standard_library_default =
  match !!ocamllib with
    | Some path -> `ocaml_conf, path
    | None -> `configure, Typerex_config.ocamllib

let config_origin, standard_library =
  try `env_OCAMLLIB, Sys.getenv "OCAMLLIB" with Not_found ->
  try `env_CAMLLIB, Sys.getenv "CAMLLIB" with Not_found ->
    standard_library_default

let rec remove_trailing_sep name =
  if name <> Filename.dir_sep && String.ends_with name ~suffix:Filename.dir_sep then
    let len = String.length name - String.length Filename.dir_sep in
    remove_trailing_sep (String.sub name 0 len)
  else
    name

let standard_library_default = remove_trailing_sep (snd standard_library_default)

let check_stdlib_path () =
  let pervasives =
    File.add_basename (File.of_string standard_library) "pervasives.cmi" in
  if not (File.X.exists pervasives) then
    fail ("Fatal error: no file '%s',"^^
             " standard library path specified by %s is wrong.\n%!")
      (File.to_string pervasives)
      (match config_origin with
        | `env_OCAMLLIB -> "'OCAMLLIB' environment variable"
        | `env_CAMLLIB -> "'CAMLLIB' environment variable"
        | `ocaml_conf ->
          Printf.sprintf "'ocamllib' option in '%s'" (File.to_string ocaml_conf)
        | `configure -> "'configure' script detection")

let standard_runtime = "%%BYTERUN%%"
let ccomp_type = "%%CCOMPTYPE%%"
let bytecomp_c_compiler = "%%BYTECC%%"
let bytecomp_c_libraries = "%%BYTECCLIBS%%"
let native_c_compiler = "%%NATIVECC%%"
let native_c_libraries = "%%NATIVECCLIBS%%"
let native_pack_linker = "%%PACKLD%%"
let ranlib = "%%RANLIBCMD%%"
let cc_profile = "%%CC_PROFILE%%"
let mkdll = "%%MKDLL%%"
let mkexe = "%%MKEXE%%"
let mkmaindll = "%%MKMAINDLL%%"

let exec_magic_number = "OPro2011X008"
and cmi_magic_number = "OPro2011I013"
and cmo_magic_number = "OPro2011O007"
and cma_magic_number = "OPro2011A008"
and cmx_magic_number = "OPro2011Y011"
and cmxa_magic_number = "OPro2011Z010"
and ast_impl_magic_number = "OPro2011M014"
and ast_intf_magic_number = "OPro2011N013"
and cmxs_magic_number = "OPro2011D001"
and cmt_magic_number = "OPro2011T001"

let load_path = ref ([] : string list)

let interface_suffix = ref ".mli"

let max_tag = 245
(* This is normally the same as in obj.ml, but we have to define it
   separately because it can differ when we're in the middle of a
   bootstrapping phase. *)
let lazy_tag = 246

let max_young_wosize = 256
let stack_threshold = 256 (* see byterun/config.h *)

let architecture = "%%ARCH%%"
let model = "%%MODEL%%"
let system = "%%SYSTEM%%"

let asm = "%%ASM%%"

let ext_obj = "%%EXT_OBJ%%"
let ext_asm = "%%EXT_ASM%%"
let ext_lib = "%%EXT_LIB%%"
let ext_dll = "%%EXT_DLL%%"

let default_executable_name =
  match Sys.os_type with
    "Unix" -> "a.out"
  | "Win32" | "Cygwin" -> "camlprog.exe"
  | _ -> "camlprog"

let systhread_supported = false (* %%SYSTHREAD_SUPPORT%%;; *)

let print_config oc =
  let p name valu = Printf.fprintf oc "%s: %s\n" name valu in
  let p_bool name valu = Printf.fprintf oc "%s: %B\n" name valu in
  p "version" version;
  p "standard_library_default" standard_library_default;
  p "standard_library" standard_library;
  p "standard_runtime" standard_runtime;
  p "ccomp_type" ccomp_type;
  p "bytecomp_c_compiler" bytecomp_c_compiler;
  p "bytecomp_c_libraries" bytecomp_c_libraries;
  p "native_c_compiler" native_c_compiler;
  p "native_c_libraries" native_c_libraries;
  p "native_pack_linker" native_pack_linker;
  p "ranlib" ranlib;
  p "cc_profile" cc_profile;
  p "architecture" architecture;
  p "model" model;
  p "system" system;
  p "asm" asm;
  p "ext_obj" ext_obj;
  p "ext_asm" ext_asm;
  p "ext_lib" ext_lib;
  p "ext_dll" ext_dll;
  p "os_type" Sys.os_type;
  p "default_executable_name" default_executable_name;
  p_bool "systhread_supported" systhread_supported;
  flush oc;
;;
