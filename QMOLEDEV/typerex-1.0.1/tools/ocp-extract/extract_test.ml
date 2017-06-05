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

open Extract
open Test
open OcpSystem
open OcpLang

let ml = "\
  let x = 3

  (* test1 *)
  (** test2
    * test2 *)
  (* test3 *)
  let y = 42
  (** test4 *)

  type t = int list

  module M = struct end

  (* test5 *)
  let z = 51
"

let mli = "\
  (** test0 *)

  (* test1 *)
  (** test2
    * test2 *)
  (* test3 *)
  (** test4 *)
  (* test5 *)
  val y : int
  (** test6 *)

  type t

  (* test7 *)
  val z : int

  module M : sig end
"
;;

let loc i =
  let loc = { Lexing.
    pos_fname = "_";
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = i;
  } in
  { Location.loc_start = loc; loc_end = loc; loc_ghost = true }

let ml_offset = String.index2 ml 'y' ' '
let mli_offset = String.index2 mli 'y' ' '
let loc_ml = loc ml_offset
let loc_mli = loc mli_offset

let test1 = "(** test0 *)"
let test1 = "(* test1 *)"
let test2 = "(** test2\n    * test2 *)"
let test3 = "(* test3 *)"
let test4 = "(** test4 *)"
let test5 = "(* test5 *)"
let test6 = "(** test6 *)"
let y = { modules = []; name = "y" }
let m = { modules = []; name = "M" }
let t = { modules = []; name = "t" }

let tmpfile = Filename.temp_file "extract" "tmp"

let init str =
  let oc = open_out tmpfile in
  output_string oc str;
  close_out oc

let clean () =
  Unix.safe_unlink tmpfile

let ml_ast, ml_comments =
  init ml;
  let a = ParseOCaml.Safe.structure_of_file (File.of_string tmpfile) in
  let b = ParseOCaml.comments () in
  clean ();
  a, b

let mli_ast, mli_comments =
  init mli;
  let a = ParseOCaml.Safe.signature_of_file (File.of_string tmpfile) in
  let b = ParseOCaml.comments () in
  clean ();
  a, b

let pretty_string print =
  String.strip ~fn:String.is_ws_or_nl (Unix.string_of_stdout print)

let get_comments_from_buffer = group "get_comments_from_buffer" [
  test (fun () ->
    init ml;
    let r =
      List.map snd (get_comments_from_buffer ml ml_comments loc_ml)
      = [ test1; test2; test3; test4 ] in
    clean ();
    r);
  test (fun () ->
    init ml;
    let r =
      List.map snd (get_comments_from_buffer ~odoc:true ml ml_comments loc_ml)
      = [ test2; test4 ] in
    clean ();
    r);
  test (fun () ->
    init ml;
    let r =
      List.map snd (get_comments_from_buffer mli mli_comments loc_mli)
      = [ test1; test2; test3; test4; test5; test6 ] in
    clean ();
    r);
  test (fun () ->
    init ml;
    let r =
      List.map snd (get_comments_from_buffer ~odoc:true mli mli_comments loc_mli)
      = [ test4; test6 ] in
    clean ();
    r);
]

let pretify_comment = group "pretify_comment" [
  test (fun () -> pretify_comment "\
(**
 * Ceci est un test
 ******************
 * Haha !
 * Hihi !
 ******************
 * Hoho !
 *
 *)" = "\
Ceci est un test

Haha !
Hihi !

Hoho !");
  test (fun () -> pretify_comment "(** *)" = "");
]

module Structure = struct

  open Extract.Structure

  let get_by_pos = group "get_by_pos" [
    test (fun () ->
      pretty_string (fun () -> pprint_item (get_by_pos ml_ast ml_offset))
      = "let y = 42");
  ]

  let get_value_by_name = group "get_value_by_name" [
    test (fun () ->
      pretty_string (fun () -> pprint_item (get_value_by_name ml_ast y))
      = "let y = 42");
  ]

  let get_type_by_name = group "get_type_by_name" [
    test (fun () ->
      pretty_string (fun () -> pprint_item (get_type_by_name ml_ast t))
      = "type t = int list");
    ]

  let get_module_by_name = group "get_module_by_name" [
    test (fun () ->
      pretty_string (fun () -> pprint_item (get_module_by_name ml_ast m))
      = "module M = struct end");
  ]

end

module Signature = struct

  open Extract.Signature

  let get_by_pos = group "get_by_pos" [
    test (fun () ->
      pretty_string (fun () -> pprint_item (get_by_pos mli_ast mli_offset))
      = "val y : int");
  ]

  let get_value_by_name = group "get_value_by_name" [
    test (fun () ->
      pretty_string (fun () -> pprint_item (get_value_by_name mli_ast y))
      = "val y : int");
  ]

  let get_type_by_name = group "get_type_by_name" [
    test (fun () ->
      pretty_string (fun () -> pprint_item (get_type_by_name mli_ast t))
      = "type t");
    ]

  let get_module_by_name = group "get_module_by_name" [
    test (fun () ->
      pretty_string (fun () ->
        pprint_item (get_module_by_name mli_ast m))
      = "module M : sig end");
  ]

end

let _ = register "Extract" [
  get_comments_from_buffer;
  pretify_comment;
  group "Structure" [
    Structure.get_by_pos;
    Structure.get_value_by_name;
    Structure.get_type_by_name;
    Structure.get_module_by_name;
  ];
  group "Signature" [
    Signature.get_by_pos;
    Signature.get_value_by_name;
    Signature.get_type_by_name;
    Signature.get_module_by_name;
  ];
]
