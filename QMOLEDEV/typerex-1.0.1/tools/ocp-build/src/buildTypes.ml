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

open OcpLang
open BuildEngineTypes
open BuildOCPTypes

module StringsMap = Map.Make(struct
  type t = string list
  let compare = compare
end)

type mklib_kind =
    MKLIB_Unix
  | MKLIB_Msvc

type module_origin =
    ML | MLI | MLandMLI

and package_info = {
(*  lib_package : package; *)
  lib_id : int;
  lib_name : string;
  mutable lib_dirname : File.t;
  mutable lib_provides : string;
  mutable lib_type : package_type;
  mutable lib_tag : string;

  lib_filename : string;

  lib_node : Toposort.node;
  mutable lib_missing_deps : int;

  mutable lib_deps : string package_dependency StringMap.t;
  mutable lib_requires : package_info package_dependency list;
  mutable lib_added : bool;
  mutable lib_options : source_options;



  lib_loc : string * int * string;
  lib_src_dir :  build_directory;
  lib_dst_dir :  build_directory;
  lib_modules : (module_origin * string) StringMap.t ref;
  mutable lib_internal_modules :
    (build_directory *
    ((module_origin * string) StringMap.t ref)) StringsMap.t;
  mutable lib_byte_targets : build_file list;
  mutable lib_asm_targets : build_file list;
  mutable lib_bytecomp_deps : build_file list;
  mutable lib_asmcomp_deps : build_file list;
  mutable lib_bytelink_deps : build_file list;
  mutable lib_asmlink_deps : build_file list;
  mutable lib_clink_deps : build_file list;
  mutable lib_dep_deps : build_file IntMap.t;
  mutable lib_includes : string list option;
  mutable lib_sources : (string * source_options) list;
}
