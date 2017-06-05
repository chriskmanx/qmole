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

open Location
open Lexing
open Typedtree
open Program
include Debug.Tag(struct let tag = "programCache" end)

let modtime f =
  if Sys.file_exists f then
    (Unix.stat f).Unix.st_mtime
  else
    -. max_float


let normalize f =
  if Filename.is_relative f then
    invalid_arg (f ^ " is not an absolute filename")
  else
    f

let make_cache key sign value =
  let cache = Hashtbl.create 100 in
  function f ->
    let sign value = try `value (sign f value) with e -> `exn e
    and key = key f in
    try
      let sign', value = Hashtbl.find cache key in
      if sign value <> sign' then
	raise Not_found
      else
	value
    with Not_found ->
      let value = value f in
      Hashtbl.replace cache key (sign value, value);
      value

(* This is wrong because the unit or source file is always searched
   in the orignial program !
let make_unit_cache program f =
  make_cache
    (function prefix -> prefix)
    (function prefix ->
      let unit = find_unit program prefix in
      match unit with
        | Concrete unit ->
          let modtime = function
            | Some source ->
              Some (modtime_if_exists (typedtree ~prefix:`absolute program source))
            | None -> None
          in
          `concrete (modtime unit.interface, modtime unit.implementation)
        | Abstract unit ->
          `abstract
            (modtime_if_exists (abstract_signature ~prefix:`absolute program unit))
        | Pack _ -> `pack)
    f

let make_source_file_cache program f =
  make_cache
    (function source_id -> source_id)
    (function source_id ->
      let source_file = find_source program source_id in
      let typedtree = typedtree ~prefix:`absolute program source_file in
      modtime_if_exists typedtree)
    f

let make_program_cache f =
  make_cache
    (function program -> program.root)
    (function program -> ()) (* should be cleared when includes change ! *)
    f

let make_program_unit_cache f =
  make_program_cache
    (function program -> make_unit_cache program (function unit -> f program unit))

let make_program_source_file_cache f =
  make_program_cache
    (function program ->
      make_source_file_cache program (function source_file -> f program source_file))
*)

let make_program_source_file_cache ~value_up_to_date f =
  let f =
    make_cache
      (function program, source_id -> program.root, source_id)
      (* should be cleared when includes change ! *)
      (fun (program, source_id) v ->
        let source_file = find_source program source_id in
        let typedtree = typedtree ~prefix:`absolute program source_file in
         modtime typedtree,
        value_up_to_date program v)
      (function program, source_id -> f program source_id)
  in
  fun program source_id -> f (program, source_id)

(* The following tables are indexed by absolute filenames (which must
   not contain "." or ".."), in order to ensure that they are unique. *)

(* Read cmi, cmt, and cmti files. *)
let read =
  make_cache
    normalize
    (fun f _ -> modtime f)
    (function file ->
      debugln "Loading %s" file;
      if Filename.check_suffix file ".cmi" then
        Some (Cmi_file.read_cmi file), None, None
      else
        Profile.time_call "read cmt" Cmt_format.read file)

(* The following two functions are copied from [Cmt_format]. *)

(* Return the external interface info in a cmi, cmt(if no cmti), or cmt file. *)
let read_cmi filename =
  match read filename with
      None, _, _ -> raise (Env.Error (Env.Not_an_interface filename))
    | Some cmi, _, _ -> cmi

exception Unsaved of source_file
exception NoCmt of source_file
exception NoCmtPack of packed_unit
exception OutdatedCmt of source_file
exception NoCmi of string

let read_cmi cm =
  if Sys.file_exists cm then
    read_cmi cm
  else if Filename.check_suffix cm ".cmt" || Filename.check_suffix cm ".cmti" then
    let cmi = Filename.chop_extension cm ^ ".cmi" in
    if Sys.file_exists cmi then
      read_cmi cmi
    else
      raise (NoCmi cm)
  else
    raise (NoCmi cm)

(* Return the typedtree info in a cmt(i) file *)
let read_cmt filename =
  match read filename with
      _, None, _ -> raise (Cmt_format.Error (Cmt_format.Not_a_typedtree filename))
    | _, Some cmt, source -> cmt, source

let read_typedtree file =
  let cmt, _ = read_cmt file in
  match cmt.Cmt_format.cmt_annots with
    | Cmt_format.Implementation str -> `structure str
    | Cmt_format.Interface sg -> `signature sg
    | annots ->
      failwith
        (Printf.sprintf "error reading %s: %s" file
           (match annots with
             | Cmt_format.Partial_implementation _ -> "partial implementation"
             | Cmt_format.Partial_interface _ -> "partial interface"
             | Cmt_format.Packed _ -> "packed"
             | _ -> assert false))

let read_pack file =
  let cmt, _ = read_cmt file in
  match cmt.Cmt_format.cmt_annots with
    | Cmt_format.Packed (sg, mods) -> sg, mods
    | _ -> failwith (file ^ "is not a pack typedtree")

let read_modified_source =
  make_cache
    (function old, source, last -> normalize source)
    (fun (old, source, last) _ ->
      old,
      (match old with
        | `file old -> `file (modtime old)
        | `embedded s -> `embedded s),
      last, modtime last)
    (function old, _, last ->
      let old_file =
        match old with
          | `file s -> s
          | `embedded contents ->
            let name, c = Filename.open_temp_file "ocp" ".embedded" in
            output_string c contents;
            close_out c;
            name
      in
      debugln "reading modified file";
      let chunks = Diff.read_modified_file old_file last in
      debugln "Done";
      (match old with
        | `file _ -> ()
        | `embedded _ -> Sys.remove old_file);
      chunks)

let cached_digest = make_cache normalize (fun f _ -> modtime f) Digest.file

let current_source_file = ref None
let current_relative_load_path = ref None
let current_program = ref None

let current_source () =
  match !current_source_file with
    | Some f -> f
    | None -> raise Not_found

let program () =
  match !current_program with
    | Some p -> p
    | None -> raise Not_found

exception ExistingIgnoredCmti of string * compilation_unit

(* return a prefix, and kind *)
let modname2unit modname =
  let program = program () in
  let load_path =
    try
      let source = current_source () in
      Some (source_load_path program source)
    with Not_found ->
      !current_relative_load_path
  in
  debugln "find %s in load path\n  %s" modname
    (match load_path with Some p -> String.concat "\n  " p | None -> "*none*");
  let prefix, unit = modname2unit program ?load_path modname in
  debugln "found unit %s" prefix;
  prefix, unit

(* In theory ,this should be enough, but this is not the case. *)
let source_digest_matches program unit digest =
  let _, file = Program.mli_or_ml unit in
  let t = typedtree ~prefix:`absolute program file in
  Sys.file_exists t &&
    (fst (read_cmt t)).Cmt_format.cmt_source_digest = digest

(* We have to check both mli and ml (because we may be in the ml itself maybe) *)
let source_digest_matches program unit digest =
  let matches = function
    | Some file ->
        let t = prefix_with ~prefix:`absolute program file.typedtree in
        Sys.file_exists t &&
          (fst (read_cmt t)).Cmt_format.cmt_source_digest = digest
    | None -> false
  in
  matches unit.interface || matches unit.implementation

(* new version, avoiding load path if possible
   should be merged with [find] ! *)
let ctx2prefix ctx =
  debugln "ctx2prefix %s" ctx.Ident.modname;
  let program = program () in
  let units = find_all_modname program ctx.Ident.modname (fun _ _ -> true) in
  let units =
    List.map
      (function prefix, unit ->
        let digest_ok =
          match unit with
            | Concrete unit ->
              debugln "concrete %s" prefix;
              source_digest_matches program unit ctx.Ident.source_digest
            | _ -> false
        in
        debugln "source digest for %s %s" prefix
          (if digest_ok then "matches" else "does not match");
        ((prefix, unit), digest_ok))
      units
  in
  match units, List.filter (function _, digest_ok -> digest_ok) units with
    | [(prefix, unit), _], _
    | _, [(prefix, unit), _] -> prefix, unit
    | [], _ -> raise Not_found
    | _, units ->
      raise (AmbiguousPersistent (ctx.Ident.modname, None, List.map fst units))

let unit2cmi prefix = function
  | Abstract unit -> abstract_signature ~prefix:`absolute (program ()) unit
  | unit ->
    let no_cmt, cmt =
      match unit with
        | Abstract _ -> assert false
        | Concrete {interface = Some source}
        | Concrete {implementation = Some source}
        | Pack {p_interface = Some source} ->
          NoCmt source,
          typedtree ~prefix:`absolute (program ()) source
        | Pack ({p_interface = None ; p_typedtree = p_typedtree} as unit) ->
          NoCmtPack unit,
          Program.prefix_with ~prefix:`absolute (program ()) p_typedtree
        | Concrete _ -> assert false
    in
    if Sys.file_exists (cmt ^ "i") then
      raise (ExistingIgnoredCmti (prefix, unit))
    else if Sys.file_exists cmt then
      cmt
    else
      let cmi = Filename.chop_extension cmt ^ ".cmi" in
      if Sys.file_exists cmi then
        cmi
      else
        raise no_cmt

let read_pers_struct_fun =
  let read_pers_struct_fun =
    make_cache
      (function _, f -> normalize f)
      (fun (_, f) _ -> modtime f)
      (function modname, filename -> !Env.read_pers_struct_fun modname filename)
  in
  fun modname filename -> read_pers_struct_fun (modname, filename)

(* We should relly on the digest too, if possible. *)
let find_pers_struct modname =
  debugln "searching persistent structure %s" modname;
  let prefix, unit = modname2unit modname in
  let filename = unit2cmi prefix unit in
  read_pers_struct_fun modname filename

let starting_time = 500000000

let () =
  Cmt_env.init ();
  (* We use lazyness for efficiency (e.g. Core) but then we have to
     prevent any renaming, because lazy renamings appear inside the
     [disabling_renaming] wrapper in [components_of_module], which
     would not be disabled. *)
  Env.EnvLazy.eager := false;
  Ident.renaming_disabled := true;
  Ident.set_current_time starting_time;
  Env.find_pers_struct_fun :=
    Profile.time_call "find_pers_struct" find_pers_struct;
  Env.read_cmi_fun := Profile.time_call "read_cmi" read_cmi

let classify_ident id =
  let time = Ident.binding_time id in
  if time = -1 then `hidden
  else if time = 0 then `persistent
  else if time < 1000 then `predef
  else if time > starting_time then `fresh
  else `dumped

let initial_env ?source_file ?program ~load_path ?relative_load_path ~nopervasives =
  Config.load_path := load_path;
  current_source_file := source_file;
  current_relative_load_path := relative_load_path;
  current_program := program;
  Env.reset_cache ();
  try
    if nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    Printf.ksprintf failwith "module Pervasives not found in\n  %s"
      (String.concat "\n  " load_path)

let source_env program source_file =
  let load_path = source_load_path ~prefix:`absolute program source_file
  and relative_load_path = source_load_path program source_file in
  initial_env ~program ~nopervasives:source_file.nopervasives
    ~source_file ~load_path ~relative_load_path

let pack_env program unit =
  let load_path = pack_unit_load_path ~prefix:`absolute program unit
  and relative_load_path = pack_unit_load_path program unit in
  initial_env ~program ~nopervasives:false
    ?source_file:None ~load_path ~relative_load_path

let source_env program = Profile.time_call "source env" (source_env program)

(*
exception Found of string
let program_env ?(nopervasives=false) program =
  let load_path =
    try
      Hashtbl.iter
        (fun prefix _ -> raise (Found prefix))
        program.units;
      [Config.standard_library] (* stub *)
    with
        Found prefix -> load_path ~prefix:`absolute program prefix
  in
  initial_env ~program ~nopervasives ~load_path ?source_file:None
*)

let typedtree program source_file =
  let f = typedtree ~prefix:`absolute program source_file in
  if Sys.file_exists f then
    read_typedtree f
  else
    raise (NoCmt source_file)

let pack program file =
  let f = pack_typedtree ~prefix:`absolute program file in
  if Sys.file_exists f then
    read_pack f
  else
    raise (NoCmtPack file)
(*
let concrete_unit_signature program unit =
  let _, source_file = Program.mli_or_ml unit in
  let f = Program.typedtree ~prefix:`absolute program source_file in
  try
    (* We also try the cmi if no cmt *)
    (read_cmi f).Env.cmi_sign
  with NoCmi _ ->
    raise (NoCmt source_file)

let unit_signature program = function
  | Concrete unit -> concrete_unit_signature program unit
  | Abstract unit ->
    let f = abstract_signature ~prefix:`absolute program unit in
    (read_cmi f).Env.cmi_sign
  | Pack units -> assert false

let signature program prefix = unit_signature program (find_unit program prefix)
*)

let signature program prefix =
  let unit = find_unit program prefix in
  (read_cmi (unit2cmi prefix unit)).Env.cmi_sign

(*
let is_signature = function
  | `signature s -> s
  | `structure _ -> invalid_arg "is_signature"

let is_structure = function
  | `structure s -> s
  | `signature _ -> invalid_arg "is_structure"

let implementation project prefix =
  is_structure (typedtree project (find_source project (prefix, `ml)))

let interface project prefix =
  is_signature (typedtree project (find_source project (prefix, `mli)))
*)

let ignore_auto_save = ref false

let auto_save f =
  let b = Filename.basename f and d = Filename.dirname f in
  let auto_save = Filename.concat d ("#" ^ b ^ "#") in
  if not !ignore_auto_save && Sys.file_exists auto_save &&
    modtime auto_save >= modtime f then (
    Some auto_save
  ) else
    None

let check_auto_save f =
  match auto_save f with
  | None -> f
  | Some f -> f

let has_auto_save f = auto_save f <> None

let check_source_for_refactoring ~errors program source_file =
  let raise e = errors := e :: !errors in
  let absolute_source = source ~prefix:`absolute program source_file
  and absolute_typedtree =
    Program.typedtree ~prefix:`absolute program source_file in
  let autosave = auto_save absolute_source in
  if autosave <> None then
    raise (Unsaved source_file);
  if not (Sys.file_exists absolute_typedtree) then
    raise (NoCmt source_file)
  else if modtime absolute_source > modtime absolute_typedtree then
    raise (OutdatedCmt source_file)

let check_for_refactoring ~errors program =
  iter_sources
    (fun _ source_file ->
      check_source_for_refactoring ~errors program source_file)
    program

(* Return the absolute filenames of the last-compiled, normal, and
   last version of a source file (any of which can be equal). *)

let source_versions program source_file =
  let absolute_source = source ~prefix:`absolute program source_file
  and absolute_typedtree = Program.typedtree ~prefix:`absolute program source_file in
  let autosave = auto_save absolute_source in
  if not (Sys.file_exists absolute_typedtree) then
    raise (NoCmt source_file);
  let outdated = modtime absolute_source > modtime absolute_typedtree in
  let snapshot =
    if outdated then
      let embedded =
        if Sys.file_exists absolute_typedtree then
          try snd (read_cmt absolute_typedtree)
          with Not_found -> None
        else
          None
      in match embedded with
        | Some source ->
          let source = match source.Cmt_format.generated with
            | Some (_, file) -> file
            | None -> source.Cmt_format.source_file
          in
          Some (`embedded source.Cmt_format.contents)
        | None ->
          let last = absolute_source ^ ".last_compiled" in
          if Sys.file_exists last then
            Some (`file last)
          else
            None
    else
      None
  in
  let old =
    match snapshot with
      | Some f -> (*debugln "found last source %s" f ;*) f
      | None ->
	if outdated then
	  debugln
	    "WARNING !!! no source backup for outdated %s; \
                       position shifts may prevent the processing of your request."
	    source_file.source;
	`file absolute_source
  and last =
    match autosave with
      | Some f -> (*debugln "found auto-save %s" f ;*) f
      | None -> absolute_source
  in
  if old = `file last then
    None
  else
    Some (old, absolute_source, last)

let source2modified program source_file =
  match source_versions program source_file with
    | Some versions -> read_modified_source versions
    | None ->
      Diff.read_unmodified_file (source ~prefix:`absolute program source_file)

let translate shift program source pos =
  match source_versions program source with
    | Some versions -> shift (read_modified_source versions) pos
    | None -> pos

type pos = [ `cnum of int | `lc of int * int ]

let last_cnum2old_lc program source pos =
  let last_lc chunks =
    let old_cnum = 
      match pos with
        | `lc (l, c) -> Diff.lnum2cnum `last chunks l c
        | `cnum cnum -> cnum
    in
    Diff.cnum2lnum `old chunks (Diff.last2old `char chunks old_cnum)
  in
  try
    let chunks = source2modified program source in
    last_lc chunks
  with e -> try
    let chunks =
      Diff.read_unmodified_file
        (Program.source ~prefix:`absolute program source) in
    last_lc chunks    
  with _ ->
    Printf.ksprintf failwith
      "error translating location: %s" (Printexc.to_string e)

let translate_loc shift target program loc =
  let origin = match target with `last -> `old | `old -> `last in
  try
    let source = source_id_of_loc program loc in
    fdebug "translating location (cnum = %d) %a"
      loc.loc_start.pos_cnum print loc;
    let source = find_source program source in
    debugln "computing chunks";
    let chunks = source2modified program source in
    let translate pos =
      debugln "call lnum2cnum";
      let lnum_cnum =
        Diff.lnum2cnum origin chunks pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
      in
      debugln "lnum_cnum = %d" lnum_cnum;
      let lnum_cnum = translate (shift `char) program source lnum_cnum in
      let pos_lnum, chars = Diff.cnum2lnum target chunks lnum_cnum in
      { pos with pos_cnum = lnum_cnum ; pos_lnum = pos_lnum ; pos_bol = lnum_cnum - chars }
    in
    let loc =
      { loc with
        loc_start = translate loc.loc_start ; loc_end = translate loc.loc_end }
    in
    fdebug "-> (cnum = %d) %a" loc.loc_start.pos_cnum print loc;
    loc
  with e -> debugln "error translating location: %s" (Printexc.to_string e); loc

let old2last length unit = translate (Diff.old2last length) unit
let last2old length unit = translate (Diff.last2old length) unit

let old2last_loc unit = translate_loc Diff.old2last `last unit
let last2old_loc unit = translate_loc Diff.last2old `old unit

