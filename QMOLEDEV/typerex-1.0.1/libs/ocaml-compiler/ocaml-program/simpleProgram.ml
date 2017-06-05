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
open Cmt_format
include Debug.Tag(struct let tag = "simpleProgram" end)

let project_file_names = [".typerex"]

let plain_impl_extensions = ref [".ml"]
let plain_intf_extensions = ref [".mli"]
let intf_generating_extensions = ref [".mly"]
let impl_generating_extensions = ref [".mll"; ".mly"; ".mlp"]

let ocaml_impl_extensions () = !impl_generating_extensions @ !plain_impl_extensions
let ocaml_intf_extensions () = !intf_generating_extensions @ !plain_intf_extensions

let ocaml_source_extensions () =
  List.union_set (ocaml_impl_extensions ()) (ocaml_intf_extensions ())

(* Try to locate a project file in the directory containing d.  d
   must be an absolute directory name ! *)
let rec find_file_backwards d filenames =
  try
    List.find_map
      (function pf ->
	let pf = Filename.concat d pf in
	if Sys.file_exists pf then
	  Some (d, pf, [], [])
	else
	  None)
      filenames
  with Not_found when d <> "/" ->
    let d', pf, suffix, rel = find_file_backwards (Filename.dirname d) filenames in
    d', pf, suffix @ [Filename.basename d],
    Filename.parent_dir_name :: rel

let find_file_backwards d filenames =
  if List.exists (fun f -> Filename.basename f <> f) filenames
  then
    invalid_arg "find_file_backwards: filenames must not be paths"
  else
    find_file_backwards d filenames

let find_project_file d =
  if Filename.is_relative d then
    invalid_arg "find_project_file: dirname must be absolute"
  else
    find_file_backwards d project_file_names

let words s = String.split_chars s [' '; '\t']

type project_file = {
  source_dirs : string list;
  include_dirs : string list;
  excluded_names : string list;
  impl : string list;
  intf : string list;
  nostdlib : bool;
  cmt : string option
}

exception ProjectFileError of (string * string)

let parse_line
    ~permissive ~dirs ~incs ~exclude ~impl ~intf ~cmt_dir ~nostdlib file l =
  let l = String.strip l in
  if l = "" then
    ()
  else if l.[0] = '-' then
    let l = String.sub l 1 (String.length l - 1) in
    exclude := !exclude @ words l
  else if String.starts_with l ~prefix:"IMPL" then
    let l = String.sub l 4 (String.length l - 4) in
    impl := !impl @ words l
  else if String.starts_with l ~prefix:"INTF" then
    let l = String.sub l 4 (String.length l - 4) in
    intf := !intf @ words l
  else if l.[0] = 'I' then
    let l = String.sub l 1 (String.length l - 1) in
    incs := !incs @ words l
  else if String.starts_with l ~prefix:"CMT" then
    if not permissive && !cmt_dir <> None then
      raise (ProjectFileError (file, "duplicate 'CMT' directive"))
    else
      let l = String.sub l 3 (String.length l - 3) in
      if not permissive && l = "" then
        raise (ProjectFileError (file, "invalid 'CMT' directive"))
      else
        cmt_dir := Some (String.strip l)
  else if l = "NOSTDLIB" then
    nostdlib := true
  else if l.[0] = '#' then
    ()
  else
    dirs := !dirs @ words l

(* Return the dirs, include dirs, and exclude files of a project. *)
let parse_project_file ~permissive pf =
  let dirs = ref []
  and incs = ref []
  and exclude = ref []
  and impl = ref []
  and intf = ref []
  and cmt_dir = ref None
  and nostdlib = ref false in
  List.iter
    (parse_line ~permissive ~dirs ~incs ~exclude ~impl ~intf ~cmt_dir ~nostdlib pf)
    (File.lines_of_file pf);
  List.iter (debugln "IMPL %s") !impl;
  {
    source_dirs = List.rev !dirs;
    include_dirs = List.rev !incs;
    excluded_names = List.rev !exclude;
    impl = List.rev !impl;
    intf = List.rev !intf;
    nostdlib = !nostdlib;
    cmt = !cmt_dir
  }

let make_absolute file =
  if Filename.is_relative file then
    if file = "." then Sys.getcwd () else
      Filename.concat (Sys.getcwd ()) file
  else
    file

let concat_if_relative d name =
  if name = Filename.current_dir_name then
    d
  else if d = Filename.current_dir_name then
    name
  else if Filename.is_relative name then
    Filename.concat d name
  else
    name

let prefix_by l name =
  if Filename.is_relative name (*&& (name = "" || name.[0] <> '~')*) then
    List.fold_right Filename.concat l name
  else
    name

let classify_source f =
  if List.exists (Filename.check_suffix f) (ocaml_impl_extensions ()) then
    `ml
  else if List.exists (Filename.check_suffix f) (ocaml_intf_extensions ()) then
    `mli
  else
    `other

let is_generated f =
  let kind = classify_source f in
  let prefix = Misc.chop_extension_if_any f in
  match kind with
    | `mli ->
        List.exists
          (function ext -> Sys.file_exists (prefix ^ ext))
          !intf_generating_extensions
    | `ml ->
        List.exists
          (function ext -> Sys.file_exists (prefix ^ ext))
          !impl_generating_extensions
    | `other -> false

let directory_of file =
  if Sys.file_exists file && Sys.is_directory file then
    file
  else
    Filename.dirname file

type project_info = {
  absolute_root : string;
    (* absolute path to the root of the project *)
  project_file : string option;
  here2root : string list;
    (* relative path from here to the root, e.g. [".."; ".."] *)
  root2here : string list;
    (* relative path from root to here *)
  dirs : string list;
    (* project directories (relative to the root) *)
  incs : string list;
    (* include directories (relative to the root) *)
  exclude : string list;
    (* excluded source files or units *)
  impls : string list;
  intfs : string list;
  cmt_dir : string option
}

let rec remove_trailing_sep name =
  if name <> Filename.dir_sep && String.ends_with name ~suffix:Filename.dir_sep then
    let len = String.length name - String.length Filename.dir_sep in
    remove_trailing_sep (String.sub name 0 len)
  else
    name

let project_dirs ~permissive ~ignore_project_file ~default_cwd ~stdlib file =
  let absolute_dir = remove_trailing_sep (make_absolute (directory_of file)) in
  let stdlib = remove_trailing_sep stdlib in
  try
    if ignore_project_file then raise Not_found;
    let absolute_root, pf, root2here, here2root =
      find_project_file absolute_dir in
    let project_file = parse_project_file ~permissive pf in
    { absolute_root = remove_trailing_sep absolute_root;
      here2root = here2root;
      project_file = Some pf;
      root2here = root2here;
      dirs = List.map remove_trailing_sep project_file.source_dirs;
      incs =
        List.map
          (function d -> remove_trailing_sep (Misc.expand_directory stdlib d))
          project_file.include_dirs
      @ (if project_file.nostdlib then [] else [stdlib]);
      exclude = project_file.excluded_names;
      impls = project_file.impl;
      intfs = project_file.intf;
      cmt_dir = Option.map remove_trailing_sep project_file.cmt }
  with Not_found ->
    { absolute_root = absolute_dir;
      project_file = None;
      here2root = [];
      root2here = [];
      dirs = if default_cwd then ["."] else [];
      incs = [stdlib];
      exclude = [];
      impls = [];
      intfs = [];
      cmt_dir = None }

let check_project p =
  let fail fmt =
    Printf.ksprintf
    (function e ->
      let file = match p.project_file with
        | Some f -> f
        | None -> Printf.sprintf "'implicit project at %s'" p.absolute_root
      in
      raise (ProjectFileError (file, e)))
      fmt
  in
  let check_duplicate fail dirs =
  ignore
    (List.fold_right
       (fun d dirs ->
         if List.mem d dirs then
           fail d
         else
           d :: dirs)
       (List.map (concat_if_relative p.absolute_root) dirs)
       [])
  in
  check_duplicate (fail "duplicate include directory %s") p.incs;
  check_duplicate (fail "duplicate source directory %s") p.dirs;
  check_duplicate (fail "%s is both a source and include directory")
    (p.dirs @ p.incs);
  let check_absent =
    List.iter
      (function d ->
        let d = concat_if_relative p.absolute_root d in
        if not (Sys.file_exists d) then
          fail "directory %s does not exist" d
       else if not (Sys.is_directory d) then
          fail "%s is not a directory" d)
  in
  check_absent p.incs;
  check_absent p.dirs;
  check_absent (List.map Filename.dirname p.exclude);
  check_absent (List.map Filename.dirname p.impls);
  check_absent (List.map Filename.dirname p.intfs);
  match p.cmt_dir with
    | Some d -> check_absent [d]
    | None -> ()

let cmt2packed_units ~root file =
  let cmt_info, _ = ProgramCache.read_cmt (concat_if_relative root file) in
  match cmt_info.Cmt_format.cmt_annots with
    | Cmt_format.Packed (sg, units) ->
      let units =
        List.map
          (function u ->
            let basename = Misc.chop_extension_if_any (Filename.basename u) in
            concat_if_relative (Filename.dirname file) basename)
          units
      in
      Some units
    | _ -> None

(* Given a project description, return the set of compilation units
   contained in those directories. *)
let project_units ~inc ~root dirs ~assumed =
  let units =
    List.fold_left
      (fun files d ->
        let d_abs = concat_if_relative root d
        and d = if d = "." then "" else d in
        if Sys.file_exists d_abs && Sys.is_directory d_abs then
          let fs = Array.to_list (Sys.readdir d_abs) in
          let mls =
	    List.filter_map
	      (function f ->
	        if not inc &&
	          (List.exists (Filename.check_suffix f)
                     (".mlpack" :: ocaml_source_extensions ()) ||
                   Filename.check_suffix f ".cmt" &&
                   cmt2packed_units ~root (concat_if_relative d f) <> None ||
                   List.mem f assumed)
	          || inc &&
	          List.exists (Filename.check_suffix f) [".cmt" ; ".cmti" ; ".cmi"]
	        then
	          Some (Filename.concat d (Misc.chop_extension_if_any f))
	        else
	          None)
	      fs
          in
          files @ mls
        else
          files)
      []
      dirs
  in List.setify units

let rec collect_cmts abs rel t =
  Array.iter
    (function f ->
      let abs = Filename.concat abs f
      and rel = Filename.concat rel f in
      if Sys.file_exists abs && Sys.is_directory abs then
        collect_cmts abs rel t
      else if Filename.check_suffix f ".cmt" || Filename.check_suffix f ".cmti" then
        Hashtbl.add t f rel)
    (Sys.readdir abs)

let collect_cmts ~root dir =
  let t = Hashtbl.create 100 in
  let abs = concat_if_relative root dir in
  if Sys.file_exists abs && Sys.is_directory abs then
    collect_cmts abs dir t;
  t

(*
let rec find_in_rec ?(root="") acc files f =
  if List.mem f files then
    Filename.concat acc f
  else
    List.find_map
      (function d ->
        let acc = Filename.concat acc d in
        let abs = Filename.concat root acc in
        if Sys.file_exists abs && Sys.is_directory abs then
          try
            Some (find_in_rec ~root acc (Array.to_list (Sys.readdir abs)) f)
          with
              Not_found -> None
        else
          None)
      files

let find_in_rec ?(root="") ~dir f = find_in_rec ~root "" [dir] f
*)

let source_kind2cmt_suffix = function
  | `ml -> ".cmt"
  | `mli -> ".cmti"

let mlpack2units ~root file =
  let paths =
    List.concat
      (List.map
         (function l ->
           let l = String.strip l in
           words l)
         (File.lines_of_file (concat_if_relative root file)))
  in
  List.map
    (function p ->
      let dir = concat_if_relative (Filename.dirname file) (Filename.dirname p)
      and modname = Filename.basename p in
      try
        List.find_map
          (function suffix ->
            let prefix = concat_if_relative dir modname in
            if Sys.file_exists (concat_if_relative root prefix ^ suffix) then
              Some prefix
            else
            let prefix = concat_if_relative dir (String.uncapitalize modname) in
            if Sys.file_exists (concat_if_relative root prefix ^ suffix) then
              Some prefix
            else
              None)
          (ocaml_source_extensions () @ [".mlpack"; ".cmti" ; ".cmt"])
      with Not_found ->
        (* There should be a warning *)
        concat_if_relative dir (String.uncapitalize modname))
    paths

let packed_units ~root file =
  if Filename.check_suffix file ".mlpack" then
    Some (mlpack2units ~root file)
  else if Filename.check_suffix file ".cmt" then
    cmt2packed_units ~root file
  else
    invalid_arg "packed_units"

let read_one_unit ~root ?assume ~impl ~intf ~load_path ~exclude prefix ~inc =
  (match assume with
    | Some f -> debugln "assuming %s" f
    | None -> ());
  let rec first_existing = function
    | [] -> None
    | t :: q ->
      let f = prefix ^ t in
      debugln "trying %s" f;
      let abs_f = concat_if_relative root f in
      let excluded = List.mem f exclude || List.mem prefix exclude in
      if assume = Some f ||
        not excluded && Sys.file_exists abs_f && not (Sys.is_directory abs_f)
      then
	Some (f, t)
      else
	first_existing q
  in
  let first_existing' forced exts =
    try Some (List.find (function f -> String.starts_with f ~prefix) forced, "")
    with Not_found -> first_existing exts
  in
  let file kind sort =
    let source = 
      match kind with
	| `ml -> first_existing' impl (ocaml_impl_extensions ())
	| `mli -> first_existing' intf (ocaml_intf_extensions ())
    in
    (* We should make a concrete unit even when no sources are
       available (to allow getting documentation from embedded
       sources, for example). *)
    match source with
      | Some (source, extension) -> Some (
        debugln "found %s" source;
	let preprocessor =
	  match extension with
	    | ".mll" -> Some `ocamllex
	    | ".mly" -> Some `ocamlyacc
	    | _ -> None (* stub *)
	in
	{
	  source = source;
	  preprocessor = preprocessor;
	  nopervasives = false; (* stub *)
	  load_path = load_path;
	  typedtree = prefix ^ source_kind2cmt_suffix kind;
	}
      )
      | None -> None
  in
  let interface = file `mli (function `signature s -> s | _ -> assert false) in
  let implementation =
    match file `ml (function `structure s -> s | _ -> assert false) with
      | Some impl -> `impl impl
      | None ->
        match first_existing [".mlpack" ; ".cmt"] with
          | Some (file, _) ->
            (match packed_units root file with
              | Some p_units ->
                `pack {
                  p_interface = interface;
                  p_load_path = load_path;
                  p_units = p_units;
                  p_typedtree = prefix ^ ".cmt"
                }
              | None -> `none)
          | None -> `none
  in
  if implementation = `none && interface = None || inc then
    match first_existing [".cmti" ; ".cmt" ; ".cmi"] with
      | Some (a_signature, _) ->
        Some (prefix, Abstract { a_load_path = load_path ; a_signature = a_signature })
      | None -> None
  else
    let unit =
      match implementation with
        | `impl impl -> Concrete { implementation = Some impl ; interface = interface }
        | `none -> Concrete { implementation = None ; interface = interface }
        | `pack p -> Pack p
    in
    Some (prefix, unit)

let exact_matches root cmts sourcefile =
  List.find_all
    (function cmt ->
      try
        (fst (ProgramCache.read_cmt (concat_if_relative root cmt)))
          .cmt_source_digest =
        ProgramCache.cached_digest (concat_if_relative root sourcefile)
      with _ -> false)
    cmts

let assign_cmt first root cmts ~sources assigned_cmts prefix kind typedtree =
  let sourcefile_suffix =
    match kind with
      | `ml -> ".ml"
      | `mli -> ".mli"
  in
  let together = prefix ^ source_kind2cmt_suffix kind in
  let cmts = Hashtbl.find_all cmts (Filename.basename together) in
  match
    cmts,
    Hashtbl.find_all sources (Filename.basename prefix),
    lazy (exact_matches root cmts (prefix ^ sourcefile_suffix))
  with
    | _ when typedtree <> together -> typedtree
    | [typedtree], [_source], _ -> typedtree
    | _, _, lazy [typedtree] ->
      assert first;
      Hashtbl.add assigned_cmts typedtree ();
      typedtree
    | _ -> typedtree

let assign_cmts_to_unit first root cmts ~sources assigned_cmts prefix =
  let assign kind = function
    | Some file ->
      Some {file with typedtree =
          assign_cmt first root cmts ~sources assigned_cmts prefix kind file.typedtree}
    | None -> None
  in
  function
    | Concrete { implementation = implementation; interface = interface } ->
      Concrete {
        implementation = assign `ml implementation;
        interface = assign `mli interface
      }
    | Pack unit ->
      Pack { unit with
        p_interface = assign `mli unit.p_interface;
        p_typedtree =
          assign_cmt first root cmts ~sources assigned_cmts prefix `ml unit.p_typedtree
      }
    | unit -> unit

let assign_cmts root cmts units =
  let sources = Hashtbl.create 100 in
  List.iter
    (function prefix, _ -> Hashtbl.add sources (Filename.basename prefix) prefix)
    units;
  let assigned_cmts = Hashtbl.create 100 in
  (* assignement for sources with only one match or only one exact match *)
  let units =
    List.map
      (function prefix, unit ->
        prefix, assign_cmts_to_unit true root cmts ~sources assigned_cmts prefix unit)
      units
  in
  (* Second assignement for sources with saeveral matches, none exact,
     but all but one already assigned to another source. *)
  let unassigned_cmts = Hashtbl.create 100 in
  Hashtbl.iter
    (fun basename cmt ->
      if not (Hashtbl.mem assigned_cmts cmt) then
        Hashtbl.add unassigned_cmts basename cmt
      else
        debugln "already assigned %s" cmt)
    cmts;
  List.map
    (function prefix, unit ->
      prefix,
      assign_cmts_to_unit false
        root unassigned_cmts ~sources (Hashtbl.create 100) prefix unit)
    units

exception NotASourceFile of string
exception FileNotInProgram of string * string list
exception FileExcluded of string * string

let program
    ?(ignore_absent=false) ?(ignore_extension=false) ?(ignore_project_file=false)
    ?(default_cwd=true) ?(cwd=Sys.getcwd ()) file =

  (* We may lift this restriction later. *)
  if Filename.basename file <> file then
    invalid_arg "file must be in the current directory";

  (* Quick hack: *)
  let old_cwd = Sys.getcwd () in
  Sys.chdir cwd;

  if Sys.file_exists file && Sys.is_directory file then
    invalid_arg "file is a directory";

  let project =
    project_dirs
      ~permissive:ignore_absent ~ignore_project_file ~default_cwd
      ~stdlib:Config.standard_library
      file
  in
  check_project project;
  let units_proj =
    project_units ~inc:false ~root:project.absolute_root project.dirs
      ~assumed:(project.impls @ project.intfs)
  and units_libs =
    project_units ~inc:true ~root:project.absolute_root project.incs ~assumed:[]
  and load_path = project.dirs @ project.incs
  and exclude = project.exclude in
  let file = prefix_by project.root2here file in
  let absolute_file = Filename.concat project.absolute_root file in
  let source_kind =
    match classify_source file with
      | `other ->
          if List.mem file project.impls then `ml
          else if List.mem file project.intfs then `mli
          else if ignore_extension then `ml
        else raise (NotASourceFile file)
      | `ml | `mli as k -> k
  in
  let file =
    if List.mem (Filename.dirname absolute_file) (project.dirs @ project.incs) then
      absolute_file
    else
      file
  in
  let prefix =
    try Misc.chop_extension_if_any file
    with Invalid_argument _ when ignore_extension -> file in
  let source = prefix, source_kind in
  let missing = not (List.mem prefix units_proj) in
  let excluded_by =
    try
      Some (List.find (function x -> x = file || x = prefix) exclude)
    with
        Not_found -> None
  in
  debugln "source = %s, prefix = %s, excluded = %s"
    file prefix (String.concat "," exclude);
  if not ignore_absent then (
    match project.project_file with
      | Some pf ->
        (match excluded_by with
          | Some x -> raise (FileExcluded (pf, x))
          | None -> ());
        if missing && project.dirs <> [] then
          raise (FileNotInProgram (pf, project.root2here))
      | None -> ()
  );
  let units_proj =
    if missing then (
      (*
        assert (units = [] || ignore_absent);
      *)
      prefix :: units_proj
    ) else
      units_proj
  in
  let files inc =
    List.filter_map
      (function pref ->
        let assume =
          if prefix = pref && ignore_absent then
            Some file
          else
            None
        in
        read_one_unit
          ?assume ~impl:project.impls ~intf:project.intfs
          ~root:project.absolute_root ~load_path ~exclude pref ~inc)
  in
  let files = files false units_proj @ files true units_libs in
  let files =
    match project.cmt_dir with
      | Some dir ->
        assign_cmts project.absolute_root
          (collect_cmts ~root:project.absolute_root dir)
          files
      | None -> files
  in
  let project = {
    root = project.absolute_root;
    units = Hashtbl.of_list files
  } in
  Sys.chdir old_cwd;
  source, project






