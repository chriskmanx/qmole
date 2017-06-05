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

(*
(* TODO: generates "order" directives to define an order between two unrelated
projects *)

(* open SafeCaml *)

(* open BuildConfig *)
open BuildOCPTypes
(* open BuildTypes *)
(* open BuildGlobals *)

let output_string_option oc name v =
  Printf.fprintf oc  "\t\t\t%s= \"%s\"\n" name (String.escaped v)

let output_ident_option oc name v =
  Printf.fprintf oc  "\t\t\t%s= %s\n" name (String.escaped v)

let output_option oc name op =
  match op with
      None -> ()
    | Some x -> output_string_option oc name x

let output_bool oc name v =
  output_ident_option oc name (if v then "true" else "false")

let output_flags oc name list =
  Printf.fprintf oc  "\t\t\t%s= [" name;
  List.iter (fun s ->
    Printf.fprintf oc "\"%s\" " (String.escaped s)
  ) list;
  Printf.fprintf oc "]\n"

let output_options oc op =
(*  output_string_option oc "pp" op.options_pp; *)
  output_flags oc "dep" (strings_option op dep_option);
  output_flags oc "bytecomp" (strings_option op bytecomp_option);
  output_flags oc "asmcomp" (strings_option op asmcomp_option);
  output_flags oc "bytelink" (strings_option op bytelink_option);
  output_flags oc "asmlink" (strings_option op asmlink_option);
  output_bool oc "generated" (bool_option_true op generated_option);
  ()

let output_project oc pj =

  Printf.fprintf oc "begin \"%s\"\n" pj.package_name;
  if pj.package_provides <> "" then
    Printf.fprintf oc "\tprovides = \"%s\"\n" pj.package_provides;
  Printf.fprintf oc "\tversion = \"%s\"\n" pj.package_version;
  Printf.fprintf oc "\tdirname = \"%s\"\n" (String.concat pj.package_dirname;
  (match pj.package_auto with None -> () | Some filename ->
    Printf.fprintf oc "\tauto = \"%s\"\n" filename);
  Printf.fprintf oc "\tenabled = %b\n" (bool_option_true pj.package_options enabled_option);
  Printf.fprintf oc "\thas_byte = %b\n"        (bool_option_true pj.package_options byte_option );
  Printf.fprintf oc "\thas_asm = %b\n"         (bool_option_true pj.package_options asm_option );
  Printf.fprintf oc "\thas_byte_debug = %b\n"  pj.package_has_byte_debug;
  Printf.fprintf oc "\thas_asm_debug = %b\n"   pj.package_has_asm_debug;
  Printf.fprintf oc "\thas_asm_profile = %b\n" pj.package_has_asm_profile;
  Printf.fprintf oc "\tfiles = [ \n";
  List.iter (fun (file, options) ->
    Printf.fprintf oc "\t\t\"%s\"" file;
    if options != [] then begin
      Printf.fprintf oc "(\n";
      List.iter (fun o ->
        Printf.fprintf oc "%s " (string_of_set_option o)
      ) options;
      Printf.fprintf oc "\t\t)";

    end;
    Printf.fprintf oc "\n";
  ) pj.package_sources;
  Printf.fprintf oc "\t]\n";
  Printf.fprintf oc "\trequires = [\n";
  StringMap.iter (fun _ dep ->
    Printf.fprintf oc "\t\t\"%s\"\n" dep.dep_project
  ) pj.package_deps;
(*
  List.iter (fun dep ->
    let pj2 = dep.dep_project in
    if not (StringMap.mem pj2.package_provides pj.package_deps) then begin
      if dep.dep_link then begin
	Printf.fprintf oc "\t\t\"%s\"\n" pj2.package_provides
      end else begin
	Printf.fprintf oc "\t\t (* \"%s\" *) \n" pj2.package_provides
      end;
      Printf.fprintf oc "\t\t\t (* ";
      List.iter (fun modname ->
	Printf.fprintf oc "\"%s\" " modname
      ) dep.dep_for;
      Printf.fprintf oc " *)\n";
    end
  ) pj.package_requires;
*)
  Printf.fprintf oc "\t]\n";
  Printf.fprintf oc "end\n\n";
  ()
*)
