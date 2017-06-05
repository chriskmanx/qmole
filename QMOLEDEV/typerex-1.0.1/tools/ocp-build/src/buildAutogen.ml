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

open BuildOCPTypes

let create_package name ptype dirname_t =
  let file_t = File.add_basename dirname_t (name ^ ".ocp") in

  let map = ref StringMap.empty in
  let files = File.Dir.list dirname_t in
  List.iter (fun file ->
    try
      let modfile = String.copy file in
      modfile.[0] <- Char.uppercase modfile.[0];
      let basename, ext = File.cut_last_extension modfile in
      let modfile = basename ^ "." ^ ext in
      map := StringMap.add modfile (file, basename, ext) !map
    with Not_found -> ()
  ) files;

  let map = !map in
  let files = ref [] in
  StringMap.iter (fun modfile (file, basename, ext) ->
    match ext with
        "ml" ->
          if not (StringMap.mem (basename ^ ".mll") map) &&
            not (StringMap.mem (basename ^ ".mly") map) then
            files := file :: !files
      | "mli" ->
        if not (StringMap.mem (basename ^ ".mly") map) &&
          not (StringMap.mem (basename ^ ".ml") map) then
          files := file :: !files
      | "mly"
      | "mll"
      | "c" ->
        files := file :: !files
      | _ -> ()
  ) map;
  let source_files = !files in

  let oc = File.X.open_out file_t in
  Printf.fprintf oc "begin %s \"%s\"\n"
    (match ptype with
        ProjectProgram -> "program"
      | ProjectObjects -> "objects"
      | ProjectLibrary -> "library"
      | ProjectToplevel -> "toplevel"
    )
    name;
  Printf.fprintf oc "   sort = true\n";
  Printf.fprintf oc "   files = [ %s ]\n" (match source_files with
      [] -> ""
    | _ -> "\"" ^ String.concat "\" \"" source_files ^ "\"");
  Printf.fprintf oc "   requires = [ ]\n";
  Printf.fprintf oc "end\n";
  close_out oc
