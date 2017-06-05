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

open OcpLang
open Program
open BuildOCPTypes
open Filename
open BuildOCamlRules

let package_dirs p =
  let dirs =
    List.setify
      (List.map (function source, _ -> dirname source) p.package_sources)
  in
  List.map
    (function d ->
      let package_dirname = File.to_string p.package_dirname in
      if d = current_dir_name && package_dirname <> current_dir_name then
        package_dirname
      else
        concat package_dirname d) dirs

let rec package_deps acc p =
  if not (List.mem p !acc) then (
(*
    Printf.eprintf "  adding %s\n%!" p.package_name;
*)
    acc := p :: !acc;
    List.iter
      (function p' -> package_deps acc p'.dep_project)
      p.package_requires
  )

let package_deps p =
(*
  Printf.eprintf "adding the dependencies of %s\n%!" p.package_name;
*)
  let acc = ref [] in
  package_deps acc p;
  !acc

(*
let package_deps p =
  p ::
    List.map
    (function _, p -> p.dep_project)
    (StringMap.to_list p.package_deps)
*)

let remove_prefix dir filename =
  if String.starts_with filename ~prefix:dir then
    let len = String.length filename
    and dlen = String.length dir in
    let start =
      if len > dlen && filename.[dlen] = '/' then
        dlen + 1
      else
        dlen
    in
    String.sub filename start (len - start)
  else
    invalid_arg "remove_prefix"

let files project =
  let root = File.to_string project.project_dir in
  let rev_prefixes = ref []
  and units = Hashtbl.create 100 in
    IntMap.iter
      (fun _ p ->
        List.iter
          (function (source, options) ->
            let options =
              (BuildOCPParse.translate_options p.package_options options)
            and load_path =
              List.map (remove_prefix root)
                (List.concat
                   (List.map package_dirs
                      (package_deps p)))
            in
            let load_path =
              List.map (Misc.expand_directory Config.standard_library) load_path
              @ [Config.standard_library]
            in
            let package_dirname = remove_prefix root (File.to_string p.package_dirname) in
            let add tag =
              let source = Filename.concat package_dirname source in
              let prefix = Filename.chop_extension source in
              rev_prefixes := prefix :: !rev_prefixes;
              Hashtbl.add units prefix (tag, (source, options, load_path))
            in
              if check_suffix source ".ml"
                || bool_option_true options ml_file_option then
                add `ml
              else if check_suffix source ".mli"
                  || bool_option_true options mli_file_option then
                add `mli
	      else if check_suffix source ".mly" then
                add `mly
	      else if check_suffix source ".mly" then
                add `mll)
          p.package_sources)
      project.project_packages;
  List.rev_map
    (function prefix ->
      let files = Hashtbl.find_all units prefix in
      let find tag cmt =
        let source, options, load_path = List.assoc tag files
        in {
          load_path;
          nopervasives = false;
          source;
          preprocessor = None;
          typedtree = prefix ^ cmt
        } in
      let interface =
        try Some (find `mli ".cmti") with Not_found ->
        try
          let ml = find `ml ".cmti" in
          let mli = ml.source ^ "i" in
          if Sys.file_exists (Filename.concat root mli) then
            Some { ml with source = mli }
          else None
        with Not_found ->
        try Some (find `mly ".cmti") with Not_found ->
        None
      and implementation =
        try Some (find `ml ".cmt") with Not_found ->
        try Some (find `mly ".cmt") with Not_found ->
        try Some (find `mll ".cmt") with Not_found ->
        None in
      prefix, Concrete { interface ; implementation })
    (List.setify !rev_prefixes)

let program f =
  let absolute_filename = File.add_basename (File.X.getcwd ()) f in
  let root_dir = File.dirname absolute_filename in
  let root = BuildOCP.find_project root_dir "ocp-build.root" in
  let project = BuildOCP.open_project root in
  let root = File.to_string project.project_dir in
  let _ = BuildOCP.load_packages project in
  let units = files project in
  let program = {root ; units = units} in
  Some
    (Program.find_source_name program
       (remove_prefix root (File.to_string absolute_filename))),
  program
