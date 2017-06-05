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

open Util
include Debug.Tag(struct let tag = "exceptions" end)

type kind = [ `error of string | `fail ]

let rec classify_error = function
  | ProgramCache.Unsaved _
  | ProgramCache.NoCmt _
  | ProgramCache.NoCmtPack _
  | ProgramCache.OutdatedCmt _
  | ProgramCache.ExistingIgnoredCmti _
  | Resolve.Unbound _
  | Program.AmbiguousPersistent _
  | SimpleProgram.ProjectFileError _
  | SimpleProgram.NotASourceFile _
  | IDE_Callback.Quit
  | OwzFailure _ -> `fail
  | Failure _ -> `error "Failure"
  | Env.Error _ -> `error "Env.Error"
  | Qualified (_, e) -> classify_error e
  | e -> `error "exception"

let rec print_error e =
  let print format = Format.fprintf Format.str_formatter format in
  let () =
    match e with
      (* Handled failure *)
      | ProgramCache.Unsaved source ->
        print "File %s must be saved" source.Program.source
      | ProgramCache.NoCmt source ->
        print "No cmt(i) file for %s" source.Program.source
      | ProgramCache.NoCmtPack unit ->
        print "No pack cmt %s" unit.Program.p_typedtree
      | ProgramCache.OutdatedCmt source ->
        print "%s is older than %s"
          source.Program.typedtree source.Program.source
      | ProgramCache.ExistingIgnoredCmti (prefix, unit) ->
        print "found a .cmti file for unit %s which has no (or ignored) .mli"
          prefix
      | Resolve.Unbound (kind, lid) ->
        print "%s %s not found" (Env_untyped.kind2string kind) (lid2string lid)
      | Program.AmbiguousPersistent (m, load_path, units) ->
        print "ambiguous module name %s (%s)%s" m
          (String.concat ", " (List.map fst units))
          (match load_path with
            | Some p ->
              " in load_path\n  " ^
                (String.concat "\n  " p)
            | None -> "")
      | Resolve.AmbiguousOrder (names, found) ->
        print "The ordering of the following toplevel modules is ambiguous: %s"
          (String.concat ", " (List.map (function i, _ -> names.(i)) found))
      | SimpleProgram.ProjectFileError (file, desc) ->
        print "error in project description, file %s: %s" file desc
      | SimpleProgram.NotASourceFile file ->
        print "file %s is not known to be an OCaml source file" file
      | IDE_Callback.Quit ->
        print "Quit"
      | OwzFailure s ->
        print "%s" s
      (* Unexpected errors *)
      | Failure s ->
        print "%s" s
      | Env.Error e ->
        print "%a" Env.report_error e
      | Qualified (p, e) -> print "%s%s" p (print_error e)
      | e ->
        print "%s" (Printexc.to_string e)
  in
  Format.flush_str_formatter ()

(* What if we fail after some output has been printed ? *)
let catch_owz ~oc f =
  let prefix p = output_string oc p in
  try
    f ();
  with
    | e when e <> Sys.Break ->
      let backtrace = Printexc.get_backtrace () in
      let c = classify_error e
      and e = print_error e in
      match c with
        | `fail ->
          debugln "Catched command failure %s\n%s" e backtrace;
          prefix "Failed\n";
          output_string oc e;
          if not !catch_errors then
            Printf.fprintf oc "\n%s" backtrace
        | `error kind ->
          debugln "Catched error %s: %s\n%s" kind e backtrace;
          prefix "Error\n";
          Printf.fprintf oc "Error: %s: %s\n%s" kind e backtrace
