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
open IDE_Callback
include Debug.Tag(struct let tag = "IDE_Specifics" end)
open Location
open Lexing

module type T = sig
  val return_fontifying_data :
    int -> int ->
    (Face.face * (int * int) list) list ->
    ((int * int) * string) list ->
    (int * int) ->
    string
  val return_completion_data :
    int ->
    prefix:string ->
    (char * string) list ->
    string
(*
  val show_completions : int -> string list -> unit
*)
  val return_grep_results :
    root:string ->
    Env_untyped.path_sort ->
    Ident.t ->
    (string * (Location.t * int * int * string) list) list *
    (string * (Location.t * int * int * string) list) list *
    string list ->
    current:string ->
    errors:exn list ->
    string
  val config_info : string
end

module Emacs = struct

  open EmacsCallback

  let return_fontifying_data b e faces helps forced =
    let regions =
      List.map (function face, rs -> [`face face], rs) faces @
        List.map (function r, help -> [`help_echo (Some help)], [r]) helps in
    let regions =
      ([`face `none], [forced]) ::
      ([`help_echo None ; `font_lock_multiline], [b, e]) ::
        regions
    in
    let command = propertize_region_lists_command regions in
    command

  let return_completion_data position ~prefix candidates =
    Printf.sprintf "(%s (%s))"
      (pos (position - String.length prefix))
      (String.concat " "
         (List.map
            (function symb, c ->
              Printf.sprintf "(%S \"%c\")" c symb)
            candidates))

let pos2lc pos = `lc (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let bprint_grep_result kind id (defs, refs, fnames) =
  let count = List.fold_left (fun count (_, l) -> count + List.length l) 0 in
  let oc = Buffer.create 16
  and rev_overlays = ref [] in

  let name = Ident.name id in

  let show face =
    List.iter
      (function file, locs ->
	List.iter
	  (function loc, start, endp, line ->
	    Printf.bprintf oc "%s:%d:\n  " file loc.loc_start.pos_lnum;
            let count = Buffer.length oc in
            let start = count + start
            and endp = count + endp in
            rev_overlays := (face, `cnum start, `cnum endp) :: !rev_overlays;
	    Printf.bprintf oc "%s\n" line)
	  locs)
  in
  Printf.bprintf oc "OCP Grep %s %s:\n\n" (Env_untyped.kind2string kind) name;
  Printf.bprintf oc "Definitions:\n\n";
  List.iter (Printf.bprintf oc "%s:0:\n") fnames;
  debugln "defs:";
  show Face.highlight_definition defs;
  Printf.bprintf oc "\nUses:\n\n";
  debugln "occs:";
  show Face.highlight_reference refs;
  Printf.bprintf oc "\nFound ";
  if defs <> [] || fnames = [] then
    Printf.bprintf oc "%d definition(s) and " (count defs);
  Printf.bprintf oc "%d reference(s)" (count refs);
  if fnames <> [] then
    Printf.bprintf oc ", and %d toplevel module(s)\n" (List.length fnames)
  else
    Printf.bprintf oc "\n";
  Buffer.contents oc, (*List.rev*) !rev_overlays

let list_errors errors =
  String.concat "\n"
    (List.map
       (function e ->
         Printf.sprintf "  %s" (Exceptions.print_error e))
       (List.setify errors))

let return_grep_results ~root kind id (defs, occs, _ as result) ~current ~errors =
  let local_defs = try List.assoc current defs with Not_found -> []
  and local_occs = try List.assoc current occs with Not_found -> [] in
  let contents, overlays = bprint_grep_result kind id result in
  let overlay face =
    List.map
      (function loc, _, _, _ -> face, pos2lc loc.loc_start, pos2lc loc.loc_end)
  in
  let local_overlays =
    overlay Face.highlight_definition local_defs @
      overlay Face.highlight_reference local_occs
  in
  let errors =
    match errors with
      | [] -> None
      | _ ->
          Some
            (Printf.sprintf
               ("Warning! Some exception(s) occured while scanning the program "
                ^^ "for propagation:\n%s\n"
                ^^ "Result list might be partial!")
               (list_errors errors))
  in
  let errors =
    match errors with
      | Some errors -> "\n\n" ^ errors
      | None -> ""
  in
  Printf.sprintf "(%S %S `(%s) `(%s))"
    root (contents ^ errors)
    (EmacsCallback.regions overlays) (EmacsCallback.regions local_overlays)

let config_info =
  Printf.sprintf
    "((version . %S) (ocamllib . %S))"
    Typerex_config.typerex_version Config.standard_library

end

module Eclipse = struct
  let return_fontifying_data b e faces helps forced =
    let regions =
      List.fold_left
        (fun acc (face, rs) ->
          List.fold_left
            (fun acc (b, e) -> ((b, e), face) :: acc)
            acc
            rs)
        []
        faces
    in
    let regions =
      List.sort (fun (r, _) (r', _) -> compare r r') regions
    in
    let result =
      Printf.sprintf "%d %d" b e ^
        String.concat ""
        (List.map
           (function (b, e), face ->
             Printf.sprintf "\n%s %d %d" (Face.face_eclipse_name face) b e)
           regions)
    in
    debugln "result:\n%s\n" result;
    result

  let return_completion_data pos ~prefix candidates = assert false

  let return_grep_results ~root kind id (defs, refs, fnames) ~current ~errors =
    let matches occs =
      String.concat ""
        (List.map
           (function fname, locs ->
             String.concat ""
               (List.map
                  (function loc, start, endp, line ->
                    Printf.sprintf "\n%s:%d:%d:%d:%s"
                      fname
                      loc.loc_start.pos_lnum
                      loc.loc_start.pos_cnum loc.loc_end.pos_lnum
                      line)
                  locs))
           occs)
    in
    Printf.sprintf "%s %s %s %s\nDefinitions%s%s\nUses%s"
      root  (Env_untyped.kind2string kind) (Ident.name id) current
      (String.concat "" (List.map (Printf.sprintf "\n%s:") fnames))
      (matches defs)
      (matches refs)

let config_info =
  Printf.sprintf
    "((version . %S) (ocamllib . %S))"
    Typerex_config.typerex_version Config.standard_library
end














