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

let verbose = ref false
let rtt = ref false
let nortt = ref false
let save_types = ref false
let ocp_type = ref "ocp-type"
let ocamlc = ref "ocamlc"
let ocamlopt = ref "ocamlopt"
let ocamlc_opt = ref "ocamlc.opt"
let ocamlopt_opt = ref "ocamlopt.opt"
let wrap_camlp4 = ref true

let rec extract_includes = function
  | [] -> []
  | "-I" :: d :: l -> "-I" :: d :: extract_includes l
  | _ :: l -> extract_includes l

let rec extract_sources = function
  | [] -> []
  | "-intf" :: f :: l -> `intf f :: extract_sources l
  | "-impl" :: f :: l -> `impl f :: extract_sources l
  | f :: l when Filename.check_suffix f ".mli" -> `mli f :: extract_sources l
  | f :: l when Filename.check_suffix f ".ml" -> `ml f :: extract_sources l
  | _ :: l -> extract_sources l

let wrap_pp pp =
  let contains c =
    try ignore (String.find c pp) ; true
    with Not_found -> false
  in
  if contains "camlp4" then
    if !wrap_camlp4 then
      pp ^ " -printer Camlp4AstDumper"
    else
      pp
  else if contains "ocp-pp" then
    pp
  else (
    Printf.eprintf
      "ocp-wrapper: Warning: preprocessor command %S\n" pp;
    Printf.eprintf
      "is unknown to ocp-wrapper. Assuming output has correct locations\n%!";
    pp
  )

let map_suffix suffixes f =
  List.find_map
    (function e, e' ->
      if Filename.check_suffix f e then
        Some (Filename.chop_extension f ^ e')
      else
        None)
    suffixes

let rec filter_for_type = function
  | [] -> []
  | "-o" :: f :: l ->
    (try
       "-o" :: map_suffix [".cmi", ".cmti" ; ".cmo", ".cmt" ; ".cmx", ".cmt"] f ::
       filter_for_type l
     with Not_found -> filter_for_type l)
  | "-pp" :: pp :: l ->
    "-pp" :: wrap_pp pp :: filter_for_type l
  | ("-custom" | "-fPIC" | "-fno-PIC" | "-compact" | "-nodynlink" | "-p" |
     "-S" | "-shared" | "-vmthread" |
     "-instr" | "-dcmm" | "-dsel" | "-dcombine" | "-dlive" | "-dspill" |
     "-dinterf" | "-dprefer" | "-dalloc" | "-dreload" | "-dscheduling" |
     "-dlinear" | "-dstartup") :: l
  | ("-dllib" | "-dllpath" | "-inline") :: _ :: l -> filter_for_type l
  | a :: l ->
    (try map_suffix [".cmxa", ".cma" ; ".cmx", ".cmo" ] a
     with Not_found -> a) ::
    filter_for_type l

(*
let show_escaped s =
  let s' = String.escaped s in
    if s' = s && not (String.contains s ' ') then
      s
    else
      "\"" ^ s' ^ "\""

let execvp command args =
  let argv = command :: args in
    if !verbose then
      prerr_endline
	(String.concat
	   " "
	   (List.map show_escaped argv));
    Unix.execvp command (Array.of_list argv)
*)

let exec command argv =
  let command =
    String.concat
      " "
      (command :: List.map Filename.quote argv)
  in
    if !verbose then
      prerr_endline command;
    Sys.command command

let wrap backend compiler args =
  let set f = List.mem f args in
  let sources = extract_sources args in
  let stop = List.exists set [
    "-config"; "-v"; "-version"; "-vnum"; "-warn-help"; "-where";
    "-help" ; "--help" ; "-i"
  ] in
  let compile = not (stop (*|| set "-a"*)) && sources <> [] || set "-pack"
  and link = not (stop || set "-c" || set "-pack" || set "-a")
  and kept =
    List.filter (function a -> List.mem a ["-nostdlib"; "-nopervasives"]) args
  and source_includes =
    List.concat
      (List.map
	 (function `intf f | `impl f | `mli f | `ml f ->
	   match Filename.dirname f with
	     | "." -> []
	     | d -> ["-I"; d])
	 sources)
  and includes = extract_includes args in
  let lib = match backend with `byte -> "rtt.cma" | `opt ->"rtt.cmxa" in
  let inc, compile_args, link_args =
    match !rtt, !nortt with
      | false, false -> [], [], []
      | true, true ->
	  raise (Arg.Bad "-rtt and -nortt cannot be set at the same time")
      | true, false ->
	  let pp = String.concat " " ("rtt" :: "-text" :: kept @ includes) in
	    ["-I"; "+rtt"], ["-pp"; pp], [lib]
      | false, true -> ["-I"; "+nortt"], [], [lib]
  in
  let all_args =
    (if compile then compile_args else []) @
    (if compile || link then inc else []) @
    (if link then link_args else []) @
    source_includes @
    args
  in
  (* This works after bootstrapping !
     prerr_endline (DynOps.to_string command);
  *)
  let code = exec compiler all_args in
  if compile && !save_types && code = 0 then (
    match
      exec
        !ocp_type ("-save-types" :: "-w" :: "-a" ::
	    compile_args @ inc @ source_includes @ filter_for_type args)
    with
      | 0 -> () (* if !verbose then prerr_endline "ocp-type completed" *)
      | n -> Printf.eprintf "ocp-type exited with code %d\n%!" n
  );
  exit code

let options =
(*  let open Arg in *)
  Arg.align [
    "-save-types", Arg.Set save_types,
    " Save typedtrees using ocp-type";

    "-no-wrap-camlp4", Arg.Clear wrap_camlp4,
    " Do not append '-printer Camlp4AstDumper' to camlp4 preprocessor commands ";

    "-with-ocp-type", Arg.String (( := ) ocp_type),
    " specify the ocp-type command";

    "-with-ocamlc", Arg.String (( := ) ocamlc),
    " specify the ocamlc command";

    "-with-ocamlopt", Arg.String (( := ) ocamlopt),
    " specify the ocamlopt command";

    "-with-ocamlc.opt", Arg.String (( := ) ocamlc_opt),
    " specify the ocamlc.opt command";

    "-with-ocamlopt.opt", Arg.String (( := ) ocamlopt_opt),
    " specify the ocamlopt.opt command";
(*
    "-rtt", Set rtt,
    " Enable runtime types using the rtt pre-processor";

    "-nortt", Set nortt,
    " Compile normally with stub implementation of rtt";

  -rtt                 Enable runtime types using the rtt pre-processor
  -nortt               Compile normally with stub implementation of rtt
*)
    "-v", Arg.Set verbose,
    " Print executed commands to error output";

    Typerex_config.version;
  ]

let usage =
  "Usage:\n  \
   ocp-wrapper [options] {ocamlc|ocamlopt}[.opt] <arguments>"

let anonymous s =
  let n = !Arg.current + 1 in
  let args = Array.sub Sys.argv n (Array.length Sys.argv - n) in
  let args = Array.to_list (args) in
  match s, args with
    | "ocamlc", args -> wrap `byte !ocamlc args
    | "ocamlc.opt", args -> wrap `byte !ocamlc_opt args
    | "ocamlfind", "ocamlc" :: args -> wrap `byte (s ^ "ocamlc") args
    | "ocamlopt", args -> wrap `opt !ocamlopt args
    | "ocamlopt.opt", args -> wrap `opt !ocamlopt_opt args
    | _ ->  raise (Arg.Bad ("unknown command " ^ s))

let () =
  if String.starts_with Sys.argv.(0) ~prefix:"ocp-"
    && not (String.starts_with Sys.argv.(0) ~prefix:"ocp-wrapper") then (
    save_types := true;
    anonymous (String.after Sys.argv.(0) (String.length "ocp-" - 1))
  ) else
      Arg.parse options anonymous usage










