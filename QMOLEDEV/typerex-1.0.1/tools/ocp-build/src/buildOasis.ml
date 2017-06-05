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

let find_indent line =
  let rec find_indent line i len =
    if i < len then
      match line.[i] with
          ' ' | '\t' -> find_indent line (i+1) len
        | '#' -> (i, true)
        | _ -> (i, false)
    else (i, false)
  in
  let len = String.length line in
  let (indent, comment) = find_indent line 0 len in
  (indent, comment, String.sub line indent (len - indent))

(* From oasis-0.2.1~alpha1%  less src/oasis/OASISRecDescParser.ml LGPL *)
let oasis_lexer = Genlex.make_lexer
  [
        (* Statement *)
    "+:"; "$:"; ":"; "if"; "{"; "}"; "else";
        (* Section *)
    "Flag"; "Library"; "Executable";
    "SourceRepository"; "Test";
    "Document";
        (* Expression *)
    "!"; "&&"; "||"; "("; ")"; "true"; "false"
  ]

type oasis_line =
    Line of string * oasis_line list ref

let read_oasis filename =
  let ic = open_in filename in
(*
  let in_line = ref None in
  let in_section = ref None in
  let lines = ref [] in
  let finish_section () =
    match !in_section with
        None -> ()
      | Some (kind, name, section_lines) ->
        in_section := None;
        lines := (Section (kind,name, List.rev !section_lines)) :: !lines
  in
  let finish_line () =
    match !in_line with
        None -> ()
      | Some (_, tokens) ->
        in_line := None;
        match !in_section with
            None ->
              lines := (Line tokens) :: !lines
          | Some (_, _, section_lines) ->
            section_lines := tokens :: !section_lines
  in
*)

  let lines = ref [] in
  try
    let rec read_line stack ic =
      let line = input_line ic in
      let (indent, comment, line) = find_indent line in
      if comment then
        read_line stack ic
      else
        push_line stack ic indent line

    and push_line stack ic indent line =
      match stack with
          [] -> assert false
        | (current_indent, lines) :: previous_stack ->
            if indent = current_indent then begin
              lines := Line (line, ref []) :: !lines;
              read_line stack ic
            end else
              if indent < current_indent then
                push_line previous_stack ic indent line
              else (* indent > current_indent *)
                match !lines with
                    [] -> assert false
                  | Line (previous_line, new_lines) :: _ ->
                    new_lines := Line (line, ref []) :: !new_lines;
                    let stack = (indent, new_lines) :: stack in
                    read_line stack ic
    in
    read_line [(0, lines)] ic

(*
    while true do
      let line = input_line ic in
      let (indent, comment) = find_indent line in
      if indent = 0 then begin
        finish_line ();
        finish_section ();
        if not comment then
          if OcpString.starts_with line "Library " then
            in_section := Some (line, ref [])
          else
          if OcpString.starts_with line "Executable " then
            in_section := Some (line, ref [])
          else
          if OcpString.starts_with line "Test " then
            in_section := Some (line, ref [])
          else
          if OcpString.starts_with line "SourceRepository " then
            in_section := Some (line, ref [])
          else
          if OcpString.starts_with line "Document " then
            in_section := Some (line, ref [])
          else
          if OcpString.starts_with line "Flag " then
            in_section := Some (line, ref [])
          else
          if OcpString.starts_with line "SourceRepository " then
            in_section := Some (line, ref [])
          else
            | [ Kwd "Executable" ; (Ident name | String name) ] ->
                in_section := Some ("Executable", name, ref [])
            | [ Kwd "Test" ; (Ident name | String name) ] ->
                in_section := Some ("Test", name, ref [])
            | [ Kwd "SourceRepository" ; (Ident name | String name) ] ->
                in_section := Some ("SourceRepository", name, ref [])
            | [ Kwd "Document" ; (Ident name | String name) ] ->
                in_section := Some ("Document", name, ref [])
            | _ ->
              in_line := Some (0, tokens)
      end else
        if not comment then
          let tokens = OcpGenlex.tokens_of_string oasis_lexer line in
          match !in_line with
              None ->
                in_line := Some (indent, tokens)
            | Some (previous_indent, previous_tokens) ->
              if indent > previous_indent then
                in_line := Some (previous_indent, previous_tokens @ tokens)
              else begin
                finish_line ();
                in_line := Some (indent, tokens)
              end
    done;
    assert false *)
  with End_of_file ->
(*
    finish_line ();
    finish_section ();
*)
    close_in ic;
    !lines

let print_oasis lines =
  let rec print indent lines =
    List.iter (fun line ->
      let Line (s, lines) = line in
      Printf.fprintf stderr "%s%s\n" indent s;
      print (indent ^ "___") !lines
    ) (List.rev lines)
  in
  print "" lines;
  Printf.fprintf stderr "%!"

let parse_package kind name lines =
  Printf.fprintf stderr "parse_package %s\n%!" name;
  let open Genlex in
  List.iter (fun line ->
    let Line (s, lines) = line in
    try
      let tokens = OcpGenlex.tokens_of_string oasis_lexer s in
      match tokens with
          [ Ident "Name" ; Kwd ":" ; (String name | Ident name) ] ->
            ()
        | _ -> raise Not_found
    with _ ->
      Printf.fprintf stderr "[%s]Discarding line [%s]\n%!" name s
  ) (List.rev lines)


let parse_oasis lines =
  let open Genlex in
  let project_name = ref "" in
  List.iter (fun line ->
    let Line (s, lines) = line in
    try
      let tokens = OcpGenlex.tokens_of_string oasis_lexer s in
      match tokens with
          [ Ident "Name" ; Kwd ":" ; (String name | Ident name) ] ->
            project_name := name
        | [ Kwd "Library"; (String name | Ident name) ] ->
          let name = if name = !project_name then name else Printf.sprintf "%s.%s" !project_name name in
          parse_package "library" name !lines
        | [ Kwd "Executable"; (String name | Ident name) ] ->
          let name = if name = !project_name then name else Printf.sprintf "%s.%s" !project_name name in
          parse_package "program" (name ^ "-command") !lines
        | _ -> ()
    with _ ->
      Printf.fprintf stderr "Discarding line [%s]\n%!" s
  ) (List.rev lines)


let translate filename =
  let lines = read_oasis filename in
  print_oasis lines;
  parse_oasis lines

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    translate Sys.argv.(i)
  done


