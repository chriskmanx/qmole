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

include Debug.Tag(struct let tag = "diff" end)

(* The type of editing actions output by diff -f. *)
type action =
  | D of int * int option (* Delete lines b -- e *)
  | C of int * int option * string list (* Change lines b-e by r *)
  | A of int * string list (* Add r after line b *)
(* Lines start at 1 when interpreting diff files. *)

let rec read_until_dot f =
  match input_line f with
    | "." -> []
    | s -> (s ^ "\n") :: read_until_dot f

(* Parse a single action in the output of diff -f. *)
let parse_instr f =
  let l = input_line f in
  let c = l.[0]
  and l = String.sub l 1 (String.length l - 1) in
  let b, e =
    try
      let i = String.index l ' ' in
	int_of_string (String.sub l 0 i),
        Some (int_of_string (String.sub l (i + 1) (String.length l - i - 1)))
    with
	Not_found -> int_of_string l, None
  in
    match c with
      | 'd' -> D (b, e)
      | 'c' -> C (b, e, read_until_dot f)
      | 'a' -> A (b, read_until_dot f)
      | _ -> invalid_arg "parse_instr"

let simplify = function
  | D (b, e) -> b, (match e with Some e -> e | None -> b), []
  | C (b, e, r) -> b, (match e with Some e -> e | None -> b), r
  | A (b, r) -> b + 1, b, r

let rec parse_diff f =
  try
    let i = parse_instr f in
    simplify i :: parse_diff f
  with
      End_of_file -> []

(* Parse a file generated with diff -f and return a list of changes of the form
   (b, e, r) with meaning replace [b, e - 1[ by r. *)
let parse_diff_file f =
  let c = open_in f in
  let d = parse_diff c in
    close_in c;
  d

let rec parse_lines f =
  try
    let i = input_line f in
      (i ^ "\n") :: parse_lines f
  with
      End_of_file -> []

(* Get the lines of a text file (with end of lines). *)
let lines_of f =
  let c = open_in f in
  let d = parse_lines c in
    close_in c;
    d

(* Apply a diff, so that apply_diff (1, diff a b, a) = b. For
   testing purpose. *)
let rec apply_diff = function
  | count, (b, e, r) :: d, s when b <= count && e < count ->
    r @ apply_diff (count, d, s)
  | count, ((b, _, _) :: _ as d), _ :: s when b <= count ->
    apply_diff (count + 1, d, s)
  | count, ((b, _, _) :: _ as d), l :: s ->
    l :: apply_diff (count + 1, d, s)
  | _, [], s -> s
  | _ -> invalid_arg "apply_diff"

(* A chunk of a modified file is either an unmodified portion or
   modified one, which has an old and a new versions. Chunks may
   contain newline characters. *)
type chunk =
  | Same of string
  | Changed of string * string (* old, new *)

let rec cut_new count = function
  | (Same s | Changed (_, s)) as c :: chunks ->
    let l = String.length s in
    if count <= l then
      let s = String.sub s 0 count in
      let c =
	match c with
	  | Same _ -> Same s
	  | Changed (old, _) -> Changed (old, s)
      in
      [c]
    else
      c :: cut_new (count - l) chunks
  | [] ->  if count = 0 then [] else invalid_arg "cut_new"

let print_modified c =
  List.iter
    (function
      | Same s -> Printf.fprintf c "%s" s
      | Changed (old, last) ->
	Printf.fprintf c "*** REPLACED ***\n%s*** WITH ***\n%s*** END ***\n"
	  old last)

(* goto b acc (count, l) pops elements from l while incrementing
   counts and pushes them on acc, until count >= b (so, we move b -
   count elements, or 0 if count > b). *)
let rec goto b acc = function
  | count, l when count >= b -> acc, count, l
  | count, t :: q -> goto b (acc ^ t) (count + 1, q)
  | _ -> invalid_arg "goto"

let rec modified_file = function
  | count, (b, e, r) :: d, s ->
    let same, count, s = goto b "" (count, s) in
    let old, count, s = goto (e + 1) "" (count, s) in
    let r = String.concat "" r in
    Same (same) :: Changed (old, r) :: modified_file (count, d, s)
  | _, [], [] -> []
  | _, [], s -> [Same (String.concat "" s)]

(* Compute a modified file, i.e., a list of chunks, given the old
   version of the file and the diff between the old and the new
   versions. *)
let modified_file old diff = modified_file (1, diff, old)

let append_alignment =
  List.fold_right
    (fun c acc ->
      match c with
	| `subst (a, b) when a = b ->
	  let w, l =
	    match acc with
	      | Same w :: l -> w, l
	      | _ -> "", acc
	  in
	  Same (a ^ w) :: l
	| _ ->
	  let a, b =
	    match c with
	      | `subst (a, b) -> a, b
	      | `u a -> a, ""
	      | `v b -> "", b
	  and (a', b'), l =
	    match acc with
	      | Changed (a, b) :: l -> (a, b), l
	      | _ -> ("", ""), acc
	  in
	  Changed (a ^ a', b ^ b') :: l
    )

let modified_file old diff =
  List.fold_right
    (fun chunk l ->
      match chunk with
	| Same _ -> chunk :: l
	| Changed (old, last) ->
          let alignment = Align.align_words old last in
          debugln "alignment:\n  %t"
            (function c ->
              let o, l = Align.show alignment in
              Printf.fprintf c "%s\n  %s" o l);
	  append_alignment alignment l)
    (modified_file old diff)
    []

let read_unmodified_file file = [Same (String.concat "" (lines_of file))]

let read_modified_file ?(empty_absent = true) old_file new_file =
  match Sys.file_exists old_file, Sys.file_exists new_file with
    | true, true ->
      if old_file = new_file then
        read_unmodified_file old_file
      else
      let diff_file =
	Filename.temp_file
          (Filename.basename new_file ^ "-" ^ Filename.basename old_file) ".diff" in
      let command =
        "diff -f " ^ Filename.quote old_file ^ " " ^ Filename.quote new_file
	^ " >" ^ Filename.quote diff_file
      in
      debugln "%f: invoking %s" (Unix.gettimeofday ()) command;
      (match
	  let res = (*Sys.command*) Unix.system command in
          debugln "%f: diff finished" (Unix.gettimeofday ());
          res
       with
	| Unix.WEXITED (0 | 1) -> ()
	| _ -> failwith "error when invoking diff");
      debugln "reading files";
      let old = lines_of old_file
      and diff = parse_diff_file diff_file in
	Sys.remove diff_file;
      debugln "computing modified file";
	modified_file old diff
    | _ when not empty_absent -> invalid_arg "read_modified_file"
    | false, true -> [Changed ("", String.concat "" (lines_of new_file))]
    | true, false -> [Changed (String.concat "" (lines_of old_file), "")]
    | false, false -> [Same ""]

(* Given a list l, best_lexicographic [] l returns cond l' for
   the best sub-list l' of l such that cond l' does not raise Not_found *)
let rec best_lexicographic acc cond = function
  | Same s :: d ->
      best_lexicographic (`same s :: acc) cond d
  | Changed (s, s') :: d ->
      (try
	 best_lexicographic (`last (s, s') :: acc) cond d
       with
	   Not_found ->
	     best_lexicographic (`old (s, s') :: acc) cond d)
  | [] -> cond (List.rev acc)

let rec old2new length backward pos = function
  | [] -> if pos = 0 then 0 else invalid_arg "old2new"
  | Same s :: d ->
    let len = length s in
    if pos < len
    then
      pos
    else
      len + old2new length backward (pos - len) d
  | Changed (s, s') :: d ->
    let s_from, s_to = if backward then s', s else s, s' in
      let len = length s_from in
      if pos < len
      then
        (* Taking the min ensures that we at least stay in the same
           chunks, which yields better relocation after,
           e.g. renaming. *)
	min pos (length s_to)
      else
	length s_to + old2new length backward (pos - len) d

let new_lines s =
  let count = ref 0
  and chars = ref 0 in
  String.iter (function '\n' -> incr count ; chars := 0 | _ -> incr chars) s;
  !count, !chars

let old2new = function
  | `char -> old2new String.length
  | `line -> old2new (function s -> fst (new_lines s))

let old2new backward length chunks pos = old2new length backward pos chunks

let old2last = old2new false
and last2old = old2new true

let rec cnum2lnum version pos = function
  | [] -> if pos = 0 then 0, 0 else invalid_arg "cnum2lnum"
  | chunk :: d ->
    let s =
      match version, chunk with
	| _, Same s
	| `old, Changed (s, _)
	| `last, Changed (_, s) -> s
    in
    let len = String.length s in
    if pos < len
    then
      new_lines (String.sub s 0 pos)
    else
      let count, chars = new_lines s
      and count', chars' = cnum2lnum version (pos - len) d in
      count + count', if count' = 0 then chars + chars' else chars'

let cnum2lnum version chunks pos =
  let count, chars = cnum2lnum version pos chunks in
  count + 1, chars

let rec lnum2cnum version chunks l c =
  match chunks with
    | [] -> if l = 1 && c = 0 then 0 else invalid_arg "lnum2cnum"
    | chunk :: d ->
      let s =
        match version, chunk with
	  | _, Same s
	  | `old, Changed (s, _)
	  | `last, Changed (_, s) -> s
      in
      let l = ref l and index = ref 0 in
      try
        while !l > 1 do
          index := String.index_from s !index '\n' + 1;
          decr l
        done;
        !index + c
      with
          Not_found -> String.length s + lnum2cnum version d !l c

(*
  let rec translate pos = function
  | [] -> if pos = 0 then 0 else invalid_arg "translate"
  | (`same s | `last (_, s)) :: q ->
  let len = String.length s in
  if pos < len
  then
  pos
  else
  len + translate (pos - len) q
  | `old (old, last) :: q ->
  let len = String.length old in
  if pos < len
  then
  pos
  else
  (String.length last) + translate (pos - len) q

  let translate_pos candidate pos =
  {pos with Lexing.pos_cnum = translate pos.Lexing.pos_cnum candidate}

  let try_parse parse file candidate =
  let c = open_out file in
  List.iter
  (function
  | `same s | `last (_, s) | `old (s, _) -> Printf.fprintf c "%s" s)
  candidate;
  close_out c;
  (try parse file with _ -> raise Not_found),
  translate_pos candidate

  let try_parse parse file outfile candidate =
  prerr_endline "0";
  let open Unix in
  Sys.remove file;
  prerr_endline "0.5";
  mkfifo file 0o600;
  prerr_endline "0.6";
  let p = create_process parse [|parse ; file ; "-o" ; outfile |] stdin stdout stderr in
  let c = openfile file [O_WRONLY] 0o600 in
  let c = out_channel_of_descr c in
  prerr_endline "1";
  prerr_endline "2";
  List.iter
  (function
  | `same s | `last (_, s) | `old (s, _) -> Printf.fprintf c "%s" s)
  candidate;
  close_out c;
  prerr_endline "3";
  let _, st = waitpid [] p in
  prerr_endline "4";
  match st with
  | WEXITED n ->
  Printf.eprintf "camlp4 exited with code %d\n%!" n;
  if n = 0 then
  outfile, translate_pos candidate
  else
  raise Not_found
  | _ -> raise Not_found

(* Warning : specific to .ml files *)
let parse_with_errors parse file outfile diff =
  let file = Filename.temp_file
    (Filename.basename file ^ "-corrected") ".ml" in
    try
      let res = best_lexicographic [] (try_parse parse file outfile) diff in
	Sys.remove file;
	res
    with e ->
      Sys.remove file;
      raise e

let implementation0 file =
  let c = open_in file in
  let lexbuf = Lexing.from_channel c in
  let res = Owz_parser.implementation Owz_lexer.token lexbuf in
    close_in c;
    res

module Pparse = struct
  open Format

exception Outdated_version

let file ppf inputfile parse_fun ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = String.create (String.length ast_magic) in
      really_input ic buffer 0 (String.length ast_magic);
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        Misc.fatal_error "Ocaml and preprocessor have incompatible versions"
    | _ -> false
  in
  let ast =
    try
      if is_ast_file then begin
        if !Clflags.fast then
          fprintf ppf "@[Warning: %s@]@."
            "option -unsafe used with a preprocessor returning a syntax tree";
        Location.input_name := input_value ic;
        input_value ic
      end else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        let lexbuf = Lexing.from_channel ic in
        Location.init lexbuf inputfile;
        parse_fun lexbuf
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast

end
*)

(*
let implementation file =
  match Sys.command ("camlp4o.opt " ^ file ^ ">/tmp/errors-ast") with
    | 0 ->
	prerr_endline "OK1";
	(try
	   let res =
	     Pparse.file Format.err_formatter "/tmp/errors-ast"
	       (function _ -> assert false) Config.ast_impl_magic_number
	   in
(*
	  let c = open_in "/tmp/errors-ast" in
	  let res = input_value c in
	    close_in c;
*)
	    prerr_endline "OK2";
	    res
	 with e -> prerr_endline (Printexc.to_string e); raise e)
    | n ->
	Printf.eprintf "camlp4o exited with %d\n%!" n;
	raise Not_found

let implementation_with_errors file =
  let outfile = file ^ ".ast" in
  parse_with_errors "camlp4o" file outfile
*)

(*
let implementation_with_errors = parse_with_errors implementation
*)
(*
let d = read_modified_file "test/errors.ml.last_compiled" "test/errors.ml"
let _ = print_modified stderr d ; flush stderr
let ast_file, translate = implementation_with_errors "test/errors.ml" d
let ast =
  Pparse.file Format.err_formatter "test/errors.ml.ast"
    (function _ -> assert false) Config.ast_impl_magic_number
let () = Printast.implementation Format.err_formatter ast
let _ =
  List.iter
    (fun p ->
       let pos = {Lexing.dummy_pos with Lexing.pos_cnum = p} in
       Printf.eprintf "%d -> %d\n" p (translate pos).Lexing.pos_cnum)
    [0 ; 34 ; 35]
let _ = exit 0
*)
(*
  let o = lines_of "../test/test.ml"
  let d = parse_diff_file "../test/diff.f"
  let f = apply_diff (1, d, o)
  let _ = List.iter print_endline f

  let o = lines_of "../../parsing/parser.mly"
  let d = parse_diff_file "../test/diff"
  let f = apply_diff (1, d, o)
  let _ =
  let c = open_out "../test/patched.mly" in
  List.iter (function l -> output_string c l ; output_char c '\n') f;
  close_out c
*)
