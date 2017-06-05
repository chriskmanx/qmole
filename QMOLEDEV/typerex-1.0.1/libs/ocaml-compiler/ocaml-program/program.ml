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
open OcpLang

type source_file = {
  load_path : string list;
  nopervasives : bool;
  source : string;
  preprocessor : [`ocamllex | `ocamlyacc | `camlp4 of string ] option;
  typedtree : string;
}

type concrete_unit = {
  interface : source_file option;
  implementation : source_file option
}

type abstract_unit = {
  a_load_path : string list;
  a_signature : string;
}

type packed_unit = {
  p_interface : source_file option;
  p_units : string list;
  p_typedtree : string;
  p_load_path : string list
}

type compilation_unit =
  | Concrete of concrete_unit
  | Abstract of abstract_unit
  | Pack of packed_unit

type program = {
  root : string;
  units : (string, compilation_unit) Hashtbl.t
}

type source_kind = [`ml | `mli]

type source_file_id = string * source_kind

type prefix_option = [`absolute | `subdir of string]

let rec remove_common_prefix d f =
  match d with
    | t :: q when String.starts_with f ~prefix:t ->
      let len = String.length t in
      remove_common_prefix q (String.sub f len (String.length f - len))
    | l -> f, l

let prefix_with ?prefix program f =
  if Filename.is_relative f then
    match prefix with
      | Some `absolute -> Filename.concat program.root f
      | Some `subdir d ->
	let d = String.split d '/' in
	let f, d = remove_common_prefix d f in
	List.fold_right
	  (function _ -> Filename.concat Filename.parent_dir_name)
	  d f
      | None -> f
  else
    f
let prefix_load_path ?prefix program load_path =
  List.map (prefix_with ?prefix program) load_path

let source_load_path ?prefix program source =
  prefix_load_path ?prefix program source.load_path
let source ?prefix program source =
  prefix_with ?prefix program source.source
let typedtree ?prefix program source =
  prefix_with ?prefix program source.typedtree
let pack_typedtree ?prefix program pack =
  prefix_with ?prefix program pack.p_typedtree
let abstract_load_path ?prefix program unit =
  prefix_load_path ?prefix program unit.a_load_path
let abstract_signature ?prefix program unit =
  prefix_with ?prefix program unit.a_signature

let prefix2modname p = String.capitalize (Filename.basename p)

let source2modname (p, _) = prefix2modname p

let source2string (prefix, kind) =
  prefix ^
    match kind with
      | `ml -> ".ml"
      | `mli -> ".mli"

let unit2source unit kind =
  match kind, unit with
    | `ml, { implementation = Some i } -> i
    | `mli, { interface = Some i } -> i
    | _ -> raise Not_found

let mli_or_ml unit =
  try `mli, unit2source unit `mli
  with Not_found -> try `ml, unit2source unit `ml
  with Not_found -> assert false

let find_unit {units = files} prefix = Hashtbl.find files prefix

let concrete_unit_load_path ?prefix program unit =
  match unit.implementation, unit.interface with
    | _, Some sg -> source_load_path ?prefix program sg
    | Some s, None -> source_load_path ?prefix program s
    | _ -> assert false

let pack_unit_load_path ?prefix program unit =
  match unit.p_interface with
    | Some sg -> source_load_path ?prefix program sg
    | None -> prefix_load_path ?prefix program unit.p_load_path

let unit_load_path ?prefix program = function
  | Concrete unit -> concrete_unit_load_path ?prefix program unit
  | Abstract p -> abstract_load_path ?prefix program p
  | Pack unit -> pack_unit_load_path ?prefix program unit

let load_path ?prefix program pref =
  unit_load_path ?prefix program (find_unit program pref)

let prefix2source_id program prefix =
  match find_unit program prefix with
    | Concrete unit -> prefix, (fst (mli_or_ml unit))
    | _ -> raise Not_found

exception AmbiguousPersistent of
    string * string list option * (string * compilation_unit) list

let find_all_modname program m cond =
  Hashtbl.fold
    (fun prefix unit l ->
      if prefix2modname prefix = m && cond prefix unit then
        (prefix, unit) :: l
      else
        l)
    program.units
    []

let modname2unit program ?(permissive=false) ?load_path m =
  match load_path with
    | Some load_path ->
      List.find_map
        (function p ->
          match
            find_all_modname program m
              (fun prefix _ -> Filename.dirname prefix = p)
          with
            | [] -> None
            | [unit] -> Some unit
            | _ -> assert false)
        load_path
    | None ->
      match find_all_modname program m (fun _ _ -> true) with
        | [] -> raise Not_found
        | [unit] -> unit
        | unit :: _ as units ->
          if permissive then
            unit
          else
            raise (AmbiguousPersistent (m, load_path, units))

let modname2source_file program ?permissive ?load_path m =
  match modname2unit program ?permissive ?load_path m with
    | _, Concrete unit -> snd (mli_or_ml unit)
    | _ -> raise Not_found

let modname2source_id program ?permissive ?load_path m =
  match modname2unit program ?permissive ?load_path m with
    | prefix, Concrete unit -> prefix, (fst (mli_or_ml unit))
    | _ -> raise Not_found

let modname2prefix program ?permissive ?load_path m =
  fst (modname2unit program ?permissive ?load_path m)

let modname2filename ?prefix program ?permissive ?load_path kind m =
  match modname2unit program ?permissive ?load_path m with
    | _, Concrete unit -> source ?prefix program (unit2source unit kind)
    | _ -> raise Not_found

let correct_fname pos_fname loc =
  let correct pos = { pos with pos_fname = pos_fname } in
  { loc with
    loc_start = correct loc.loc_start;
    loc_end = correct loc.loc_end }

let unit_of_loc program loc =
  let fname = loc.Location.loc_start.Lexing.pos_fname in
  let basename = Filename.basename fname
  and prefix = Filename.chop_extension fname in
  let modname = Filename.chop_extension basename in
  let prefix, unit =
    try prefix, find_unit program prefix
    with Not_found ->
      match
        Hashtbl.fold
          (fun prefix unit l ->
	    if Filename.basename prefix = modname then
              (prefix, unit) :: l
            else l)
          program.units
          []
      with
        | [] -> raise Not_found
        | [u] -> u
        | l ->
          Printf.ksprintf failwith "location file name %s is ambiguous" fname
  in
  let present tag l = function
    | Some source_file when Filename.basename source_file.source = basename ->
      tag :: l
    | _ -> l
  in
  let suffixes =
    match unit with
      | Concrete unit ->
	`concrete
	  (present `ml (present `mli [] unit.interface) unit.implementation)
      | Abstract p -> `abstract p
      | Pack unit -> `packed unit
  in
  prefix, suffixes, Filename.concat (Filename.dirname prefix) basename, unit

let file_of_loc ?prefix program loc =
  let _, _, filename, _ = unit_of_loc program loc in
  prefix_with ?prefix program filename

let source_id_of_loc program loc =
  match unit_of_loc program loc with
    | prefix, `concrete (suffix :: _), _, _ -> prefix, suffix
    | prefix, `pack {p_interface = Some source}, _, _
      when Filename.basename source.source = Filename.basename prefix -> prefix, `mli
    | _ -> invalid_arg
      ("source_id_of_loc: " ^ loc.Location.loc_start.Lexing.pos_fname)

let find_source program (prefix, kind) =
  match find_unit program prefix with
    | Concrete unit -> unit2source unit kind
    | Pack {p_interface = Some source} when kind = `mli -> source
    | _ -> raise Not_found

let fold_units f {units = files} =
  Hashtbl.fold (fun prefix unit -> f prefix unit) files

let iter_units f {units = files} =
  Hashtbl.iter f files

let fold_sources ?(abstract = fun _ _ acc -> acc) ?pack concrete =
  fold_units
    (fun prefix unit acc ->
      match unit with
	| Concrete unit ->
	  let acc =
	    match unit.implementation with
              | Some source -> concrete (prefix, `ml) source acc
	      | None -> acc
	  in
	  let acc =
	    match unit.interface with
              | Some source -> concrete (prefix, `mli) source acc
	      | None -> acc
	  in
	  acc
	| Abstract p -> abstract prefix p acc
	| Pack unit ->
          match pack with
            | Some pack -> pack prefix unit acc
            | None ->
              match unit.p_interface with
                | Some source -> concrete (prefix, `mli) source acc
                | None -> acc)

let iter_sources ?(abstract = fun _ _ -> ()) ?pack concrete program =
  fold_sources
    ~abstract:
    (fun prefix load_path () -> abstract prefix load_path)
    ?pack:(match pack with
      | Some pack -> Some (fun prefix load_path () -> pack prefix load_path)
      | None -> None)
    (fun source_id source () -> concrete source_id source)
    program
    ()

exception Found of source_file_id

let find_source_name program source_name =
  try
    iter_sources
      (fun source_id source_file ->
        if source_file.source = source_name then
          raise (Found source_id))
      program;
    raise Not_found
  with
      Found source_id -> source_id


(* 10/14/11 : generated by ocp-codegen *)
let string_of_source_file = function r ->
  let aux_load_path = function l ->
    Printf.sprintf "[%s]"(String.concat "; " l)
  and aux_nopervasives = string_of_bool
  and aux_source = function str -> str
  and aux_preprocessor = function
    | None   -> "None"
    | Some o ->
        let aux = function
  | `ocamllex -> "ocamllex"
      | `ocamlyacc -> "ocamlyacc"
      | `camlp4 v -> Printf.sprintf "camlp4 %s" v in
        Printf.sprintf "Some (%s)" (aux o)
  and aux_typedtree = function str -> str in
  let load_path = Printf.sprintf "load_path: %s" (aux_load_path r.load_path)
  and nopervasives = Printf.sprintf "nopervasives: %s" (aux_nopervasives r.nopervasives)
  and source = Printf.sprintf "source: %s" (aux_source r.source)
  and preprocessor = Printf.sprintf "preprocessor: %s" (aux_preprocessor r.preprocessor)
  and typedtree = Printf.sprintf "typedtree: %s" (aux_typedtree r.typedtree) in
  let ll = String.concat ";\n  " [load_path; nopervasives; source; preprocessor; typedtree] in
  Printf.sprintf "{\n  %s\n}" ll

(* 10/14/11 : generated by ocp-codegen *)
let string_of_concrete_unit = function r ->
  let aux_interface = function
    | None   -> "None"
    | Some o -> "Some " ^ string_of_source_file o
  and aux_implementation = function
    | None   -> "None"
    | Some o -> "Some " ^ string_of_source_file o in
  let interface = Printf.sprintf "interface: %s" (aux_interface r.interface)
  and implementation = Printf.sprintf "implementation: %s" (aux_implementation r.implementation) in
  let ll = String.concat ";\n  " [interface; implementation] in
  Printf.sprintf "{\n  %s\n}" ll

(* 10/14/11 : generated by ocp-codegen *)
let string_of_abstract_unit = function r ->
  let aux_a_load_path = function l ->
    Printf.sprintf "[%s]"(String.concat "; " l)
  and aux_a_signature = function str -> str in
  let a_load_path = Printf.sprintf "a_load_path: %s" (aux_a_load_path r.a_load_path)
  and a_signature = Printf.sprintf "a_signature: %s" (aux_a_signature r.a_signature) in
  let ll = String.concat ";\n  " [a_load_path; a_signature] in
  Printf.sprintf "{\n  %s\n}" ll

let string_of_packed_unit u =
  Printf.sprintf
    "{\n  p_interface=%s;\n  p_units=[%s];\n  p_typedtree=%s;\n  p_load_path=[%s]\n}"
    (match u.p_interface with
      | Some i -> "Some"
      | None -> "None")
    (String.concat "; " u.p_units)
    u.p_typedtree
    (String.concat "; " u.p_load_path)

(* 10/14/11 : generated by ocp-codegen *)
let rec string_of_compilation_unit = function
  | Concrete v -> Printf.sprintf "Concrete %s" (string_of_concrete_unit v)
  | Abstract v -> Printf.sprintf "Abstract %s" (string_of_abstract_unit v)
  | Pack v -> Printf.sprintf "Pack %s" (string_of_packed_unit v)

(* 10/14/11 : generated by ocp-codegen *)
let string_of_program = function r ->
  let aux_root = function str -> str
  and aux_files = function l ->
    let aux = function (t0, t1) ->
      let ll = [t0; string_of_compilation_unit t1] in
      Printf.sprintf "(%s)" (String.concat ", " ll) in
    let ll = List.map aux l in
    Printf.sprintf "[%s]" (String.concat "; " ll) in
  let root = Printf.sprintf "root: %s" (aux_root r.root)
  and files = Printf.sprintf "files: %s" (aux_files (Hashtbl.to_list r.units)) in
  let ll = String.concat ";\n  " [root; files] in
  Printf.sprintf "{\n  %s\n}" ll










