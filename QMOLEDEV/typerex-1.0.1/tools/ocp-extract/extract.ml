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

open Asttypes
open Parsetree
open OcpSystem
open OcpLang

type comments = ( (int * int) * string) list

let is_connected buffer s e =
  let aux () =
    try
      let n = ref 0 in
      for i = s to e do
        if buffer.[i] = '\n' then
          incr n;
        if !n >= 2 then
          raise Exit
      done;
      true
    with Exit -> false in
  s <= e && aux ()

let extract_string buffer (s, e) =
  String.sub buffer s (e - s)

let old_comment_of_new_comment (comment_string, comment_location) =
  let loc_start = comment_location.Location.loc_start.Lexing.pos_cnum in
  let loc_end = comment_location.Location.loc_end.Lexing.pos_cnum in
  (loc_start, loc_end)

let get_comments_from_buffer ?(odoc=false) buffer comments loc =
  let comments = List.map old_comment_of_new_comment comments in

  let loc_start = loc.Location.loc_start.Lexing.pos_cnum in
  let loc_end = loc.Location.loc_end.Lexing.pos_cnum in
  let item = (loc_start, loc_end) in

  let get_comment c =
    c, extract_string buffer c in

  let is_connected (_,e) (s,_) =
    is_connected buffer e s in

  let is_special (s, _) =
    buffer.[s + 2] = '*' && buffer.[s + 3] <> '*' in

  let rec is_before ((s, e) as comment) nexts =
    (* there is no blank line between the comment and the element *)
    is_connected comment item
    (* For special comments, a regular comment can occur between the
       special comment and the element *)
    || match nexts with
        | []     -> false
        | h :: t ->
          is_connected comment h
          && is_before h t
          && (not odoc || is_special h || is_connected h item) in

  let is_after comment =
    is_connected item comment in

  let good_comments =
    let rec aux accu = function
      | []     -> List.rev accu
      | h :: t ->
        let keep1 = not odoc || is_special h in
        let keep2 = is_before h t || is_after h in
        if keep1 && keep2 then
          aux (h :: accu) t
        else
          aux accu t in
    aux [] comments in

  List.map get_comment good_comments

let get_comments ?(odoc=false) loc =
  if loc.Location.loc_ghost then
    failwith "cannot handle ghost locations";
  let filename = loc.Location.loc_start.Lexing.pos_fname in
  let filename = File.of_string filename in
  let buffer = File.X.read_to_string filename in
  let comments =
    if File.check_suffix filename ".ml" then
      ignore (ParseOCaml.Safe.structure_of_file filename)
    else
      ignore (ParseOCaml.Safe.signature_of_file filename);
    Lexer.comments () in
  get_comments_from_buffer ~odoc buffer comments loc

let get_comments_permissive ?(odoc=false) loc =
  if loc.Location.loc_ghost then
    failwith "cannot handle ghost locations";
  let filename = loc.Location.loc_start.Lexing.pos_fname in
  let filename = File.of_string filename in
  let buffer = File.X.read_to_string filename in
  let comments =
    if File.check_suffix filename ".ml" then
      ignore (ParseOCaml.Raw.structure_of_file filename)
    else
      ignore (ParseOCaml.Raw.signature_of_file filename);
    Lexer.comments () in
  get_comments_from_buffer ~odoc buffer comments loc

let pretify_comment c =
  let remove_star = ref false in

  let remove_suffix c =
    if not !remove_star && String.ends_with ~suffix:"*)" c then
      if String.length c >= 3 then
        String.before c (String.length c - 3)
      else
        ""
    else if !remove_star && String.ends_with ~suffix:")" c then
      if String.length c >= 2 then
        String.before c (String.length c - 2)
      else
        ""
    else
      failwith "pretify_comment:1" in

  let remove_prefix c =
    if !remove_star then
      c
    else if not !remove_star && String.starts_with ~prefix:"*" c then
      String.after c 0
    else
      failwith "pretify_comment:2" in

  let c =
    (* We let in one '*' char *)
    if String.starts_with ~prefix:"(**" c then
      String.after c 1
    else if String.starts_with ~prefix:"(*" c then
      String.after c 0
    else
      failwith "pretify_comment:3" in

  let l = String.split c '\n' in
  let l = List.map String.strip l in
  remove_star := List.for_all (fun s -> String.length s > 0 && s.[0] = '*') l;

  (* Remove starting '*' *)
  let l =
    if !remove_star then
      List.map (fun s -> String.after s 0) l
    else
      l in

  (* Remove start/end of commentary *)
  let l = match l with
    | [h]  -> [remove_prefix (remove_suffix h)]
    | h::t ->
      remove_prefix h :: (
        match List.rev t with
          | h::t -> List.rev t @ [remove_suffix h]
          | []   -> [])
    | []   -> [] in

  (* Remove '*' lines *)
  let l = List.map (fun c -> if String.for_all ((=)'*') c then "" else c) l in

  (* Strip lines *)
  let l = List.map String.strip l in

  (* Remove starting/ending blank lines *)
  let l = List.drop_while (String.for_all String.is_ws) l in
  let l = List.rev (List.drop_while (String.for_all String.is_ws) (List.rev l)) in

  String.concat "\n" l

let pprint_c_loc (s, e) =
  Printf.printf  "(%d %d)\n%!" s e

type path = {
  modules : string list;
  name    : string;
}

module type Sig = sig
  type t
  type item
  val get_by_pos : t -> int -> item
  val get_type_by_name : t -> path -> item
  val get_value_by_name : t -> path -> item
  val get_module_by_name : t -> path -> item
  val get_loc : item -> Location.t
  val pprint_item : item -> unit
end

let path_of_string name =
  match List.rev (String.split name '.') with
  | []   -> failwith "path_of_string"
  | h::t -> { modules = List.rev t; name = h }

module Structure = struct

  type t = structure
  type item = structure_item

  let module_find fn m = match m.pmod_desc with
    | Pmod_structure s -> (try Some (List.find_map fn s) with Not_found -> None)
    | _                -> None

  let get_by_pos ast p =
    let rec aux = function
      | { pstr_desc = Pstr_module (_,m) } as i ->
          if ParseOCaml.contains i.pstr_loc p then
            match module_find aux m with
              | None -> Some i
              | x    -> x
          else
            None
      | { pstr_loc = pstr_loc } as i -> if ParseOCaml.contains pstr_loc p then Some i else None in
    List.find_map aux ast

  let get_module ast name =
    let aux = function
      | { pstr_desc = Pstr_module (n,m) } ->
	if n.txt = name then Some m else None
      | _                                 -> None in
    List.find_map aux ast

  let get_structure ast path =
    let get ast name =
      let m = get_module ast name in
      let s = match m.pmod_desc with
          | Pmod_structure s -> s
          | _                -> raise Not_found in
      s in
    let rec aux ast = function
      | []   -> ast
      | h::t -> aux (get ast h) t in
    aux ast path

  let get_module_by_name ast path =
    let ast = get_structure ast path.modules in
    let aux = function
      | { pstr_desc = Pstr_module (n,m) } -> n.txt = path.name
      | _                                 -> false in
    List.find aux ast

  let get_value_by_name ast path =
    let ast = get_structure ast path.modules in
    let is_name p =
      match p.ppat_desc with
        | Ppat_alias (_, n) | Ppat_var n -> n.txt = path.name
        | _ -> false in
    let aux = function
      | { pstr_desc = Pstr_value (_,vs) } -> List.exists is_name (List.map fst vs)
      | _                                 -> false in
    List.last (List.find_all aux ast)

  let rec get_type_by_name ast path =
    let ast = get_structure ast path.modules in
    let aux = function
      | { pstr_desc = Pstr_type ts } ->
	List.mem path.name (List.map (function id, _ -> id.txt) ts)
      | _                            -> false in
    List.find aux ast

  let get_loc si =
    si.pstr_loc

  let pprint_item si =
    Format.fprintf Format.std_formatter "%a@." Pprintast.print_structure [si]
end

module Signature = struct

  type t = signature
  type item = signature_item

  let module_find fn m = match m.pmty_desc with
    | Pmty_signature s -> (try Some (List.find_map fn s) with Not_found -> None)
    | _                -> None

  let modtype_find fn = function
    | Pmodtype_abstract   -> None
    | Pmodtype_manifest m -> module_find fn m

  let get_by_pos ast p =
    let rec contains fn i =
      if ParseOCaml.contains i.psig_loc p then
        match fn () with
          | None -> Some i
          | x    -> x
      else
        None
    and aux = function
      | { psig_desc = Psig_module (_,m) }  as i -> contains (fun () -> module_find aux m) i
      | { psig_desc = Psig_modtype (_,m) } as i -> contains (fun () -> modtype_find aux m) i
      | { psig_loc = psig_loc } as i -> if ParseOCaml.contains psig_loc p then Some i else None in
    List.find_map aux ast

  let get_module ast name =
    let rec aux = function
      | { psig_desc = Psig_module (n,m) }  ->
	if n.txt = name then Some m else None
      | { psig_desc = Psig_modtype (n,m) } ->
	if n.txt = name then modtype_find aux m else None
      | _                                  -> None in
    List.find_map aux ast

  let get_signature ast path =
    let get ast name =
      let m = get_module ast name in
      let s = match m.pmty_desc with
          | Pmty_signature s -> s
          | _                -> raise Not_found in
      s in
    let rec aux ast = function
      | []   -> ast
      | h::t -> aux (get ast h) t in
    aux ast path

  let get_type_by_name ast path =
    let ast = get_signature ast path.modules in
    let rec aux = function
      | { psig_desc = Psig_type ts } as i ->
	if List.mem path.name (List.map (function id, _ -> id.txt) ts) then
	  Some i
	else
	  None
      | _                                 -> None in
    List.find_map aux ast

  let get_value_by_name ast path =
    let ast = get_signature ast path.modules in
    let rec aux = function
      | { psig_desc = Psig_value (v,_) } as i ->
	if v.txt = path.name then Some i else None
      | _                                     -> None in
    List.find_map aux ast

  let get_module_by_name ast path =
    let ast = get_signature ast path.modules in
    let rec aux = function
      | { psig_desc = Psig_module (m,a) } as i ->
	if m.txt = path.name then Some i else module_find aux a
      | _                                      -> None in
    List.find_map aux ast

  let get_loc si =
    si.psig_loc

  let pprint_item si =
    Format.fprintf Format.std_formatter "%a@." Pprintast.print_signature [si]
end
