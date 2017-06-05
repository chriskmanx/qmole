(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree

(*
let read modname filename =
  if Filename.check_suffix filename ".cmti"
    || Filename.check_suffix filename ".cmt"
  then
      Cmt_format.read_signature modname filename
  else
    Cmi_compat.read_pers_struct modname filename
*)

let () = Cmt_env.init ()

let () = Clflags.dont_write_files := true

(* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory (unless the -nostdlib option is given).
 *)

let init_path () =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
    else !Clflags.include_dirs in
  let exp_dirs =
    List.map (expand_directory Config.standard_library) dirs in
  load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache ()

(* Return the initial environment in which compilation proceeds. *)

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#1775) *)
let initial_env () =
  Ident.reinit();
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    fatal_error ("cannot open Pervasives")

(* Note: this function is duplicated in optcompile.ml *)
let check_unit_name ppf filename name =
  try
    begin match name.[0] with
    | 'A'..'Z' -> ()
    | _ ->
       Location.print_warning (Location.in_file filename) ppf
        (Warnings.Bad_module_name name);
       raise Exit;
    end;
    for i = 1 to String.length name - 1 do
      match name.[i] with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> ()
      | _ ->
         Location.print_warning (Location.in_file filename) ppf
           (Warnings.Bad_module_name name);
         raise Exit;
    done;
  with Exit -> ()
;;

(* Compile a .mli file *)

let interface ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  init_path ();
  let modulename =
    String.capitalize(Filename.basename(chop_extension_if_any sourcefile)) in
  check_unit_name ppf sourcefile modulename;
  Env.set_unit_name modulename;
  Ident.set_context modulename `interface sourcefile;
  let inputfile = Pparse.preprocess sourcefile in
  try
    let parsetree as ast =
      Pparse.file ppf inputfile Ast_file.read_interface Parse.interface in
    if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface parsetree;
    let tsg = Typemod.transl_signature (initial_env()) ast in
    let sg = tsg.sig_type in
    if !Clflags.print_types then
      fprintf std_formatter "%a@." Printtyp.signature
        (Typemod.simplify_signature sg);
    Warnings.check_fatal ();
    if not !Clflags.print_types then begin
(*      Env.save_signature sg modulename
        (outputprefix ^ List.hd !Clflags.cmi_exts); *)
      Typemod.save_signature modulename tsg outputprefix sourcefile
        (sg, Env.imported_units());
    end;
    Pparse.remove_preprocessed inputfile
  with e ->
    Pparse.remove_preprocessed_if_ast inputfile;
    raise e

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let implementation ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  init_path ();
  let modulename =
    String.capitalize(Filename.basename(chop_extension_if_any sourcefile)) in
  check_unit_name ppf sourcefile modulename;
  Env.set_unit_name modulename;
  Ident.set_context modulename `implementation sourcefile;
  let inputfile = Pparse.preprocess sourcefile in
  let env = initial_env() in
  if !Clflags.print_types then begin
    try
      let ast =
	Pparse.file ppf inputfile Ast_file.read_implementation Parse.implementation
      in
      ignore (print_if ppf Clflags.dump_parsetree Printast.implementation ast);
      ignore (Typemod.type_implementation
	sourcefile outputprefix modulename env ast)
    with x ->
      Pparse.remove_preprocessed_if_ast inputfile;
      raise x
  end else begin
    try
      let ast =
	Pparse.file ppf inputfile Ast_file.read_implementation Parse.implementation
      in
      ignore (print_if ppf Clflags.dump_parsetree Printast.implementation ast);
      ignore (Unused_var.warn ppf ast);
      ignore (Typemod.type_implementation
		sourcefile outputprefix modulename env ast);
      Warnings.check_fatal ();
      Pparse.remove_preprocessed inputfile;
      Stypes.dump (outputprefix ^ ".annot");
    with x ->
      Pparse.remove_preprocessed_if_ast inputfile;
      Stypes.dump (outputprefix ^ ".annot");
      raise x
  end

let c_file name =
  Location.input_name := name;
  if Ccomp.compile_file name <> 0 then exit 2

let package_files files targetfile =
  let files =
    List.map
      (fun f ->
        let f =
(*          let open Filename in *)
          if Filename.check_suffix f ".cmi" then Filename.chop_extension f ^ ".cmti"
          else if Filename.check_suffix f ".cmo" then Filename.chop_extension f ^ ".cmt"
          else f
          in
        try find_in_path !Config.load_path f
        with Not_found ->
          eprintf "@[File %s not found@]@." f;
          exit 2)
      files in
  let targetfile = Misc.chop_extension_if_any targetfile ^ ".cmt" in
  let prefix = chop_extensions targetfile in
  let targetcmti = prefix ^  List.hd !Clflags.cmi_exts in
  let targetname = String.capitalize(Filename.basename prefix) in
    Ident.set_pack_context targetname;
  try
    let _coercion = Typemod.package_units files targetcmti targetname in
    ()
  with x ->
    remove_file targetfile; raise x















