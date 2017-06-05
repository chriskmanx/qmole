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

open OcpLang
open SimpleConfig

open BuildEngineTypes
open BuildOCPTypes
open BuildTypes
open BuildGlobals

let version = "2011-11-16 08:47 Fabrice"

let print_version () =
  Printf.fprintf stderr "%s\n%!" version;
  exit 0

let set_verbosity v =
  verbosity := v;
  BuildOCPTypes.ocp_verbosity := v


let t0 = Unix.gettimeofday ()

let time s f x =
  if !time_arg then
    let t0 = Unix.gettimeofday () in
    let y = f x in
    let t1 = Unix.gettimeofday () in
    Printf.printf s (t1 -. t0);
    y
  else
    f x

let time2 s f x1 x2 =
  if !time_arg then
    let t0 = Unix.gettimeofday () in
    let y = f x1 x2 in
    let t1 = Unix.gettimeofday () in
    Printf.printf s (t1 -. t0);
    y
  else
    f x1 x2


let delete_orphans_arg = ref KeepOrphans
let list_projects_arg = ref false
let list_byte_targets_arg = ref false
let list_asm_targets_arg = ref false
let build_dir_basename_arg = ref "_obuild"

let init_arg = ref false

let arg_list = [
  "-init", Arg.Set init_arg, " : create the ocp-build.root file";

  "-byte", Arg.Set byte_arg, " : build only bytecode version";
  "-asm", Arg.Set asm_arg, " : build only native code version";
  "-clean", Arg.Set clean_arg, " : clean all compiled files and exit";
  "-distclean", Arg.Set distclean_arg, " : clean all generated files and exit";

  "-obuild", Arg.String (fun s -> build_dir_basename_arg := s), " <dir> : change _obuild directory";
  "-sanitize", Arg.Unit (fun _ -> delete_orphans_arg := DeleteOrphanFiles), " : remove orphan objects from _obuild";
  "-sanitize-dirs", Arg.Unit (fun _ -> delete_orphans_arg := DeleteOrphanFilesAndDirectories), " : remove orphan directories from _obuild";

  "-v", Arg.Int (fun verb ->
    verbosity_arg := Some verb;
    set_verbosity verb;
  ), " : set verbosity_arg (-1 = none, 0 = result, 1 = default)";
  "-list-ocp-files", Arg.Set list_ocp_files, " : list all .ocp files found";
  "-cores", Arg.Int (fun n ->
    ncores_arg := Some n), " <ncores> : use <ncores> cores to compile";

  "-save-config", Arg.Set save_config_arg, " : save ~/.ocp/ocp-build.conf file";
  "-save-project", Arg.Set save_project_arg, " : save ocp-build.root file";
  (*
    "-conf", Arg.Set conf_arg, " : generate a configuration file and exit";
    "-global", Arg.Set global_arg, " : modify global configuration instead of local configuration";
    "-no-global", Arg.Set no_global_arg, " : don't load global configuration";
  *)

  "-cross", Arg.String (fun arch -> cross_arg := Some arch), " : use a cross-compilation directory";
  "-k", Arg.Clear stop_on_error_arg, " : continue after errors";
  "-fake", Arg.Set fake_arg, " : fake actions, do not execute them";

  "-list-projects", Arg.Set list_projects_arg, " : list projects";
  "-list-targets", Arg.Unit (fun _ ->
    list_byte_targets_arg := true;
    list_asm_targets_arg := true), " : list all targets";
  "-list-byte-targets", Arg.Set list_byte_targets_arg, " : list bytecode targets";
  "-list-asm-targets", Arg.Set list_asm_targets_arg, " : list native targets";

  "-time", Arg.Set time_arg, " : print timing";

  "-digest", Arg.Unit (fun _ -> digest_arg := Some true), " : use content hash change instead of modification to trigger recompilation";
  "-no-digest", Arg.Unit (fun _ -> digest_arg := Some false), " : use modification instead of content hash change to trigger recompilation";

  "-library-ocp", Arg.String (fun name ->
    BuildAutogen.create_package name ProjectLibrary (File.of_string "."); exit 0;
  ), " <name> : auto-generate a .ocp file for a library";

  "-program-ocp", Arg.String (fun name ->
    BuildAutogen.create_package name ProjectProgram (File.of_string "."); exit 0;
  ), " <name> : auto-generate a .ocp file for a library";

  "-scan", Arg.Unit (fun _ ->
    save_project_arg := true;
    autoscan_arg := Some true),
  " : force scan for .ocp files";
  "-no-scan", Arg.Unit (fun _ -> autoscan_arg := Some false),
  " : force scan for .ocp files";
  (*
    "-distrib", Arg.Set distrib_arg," : generate configuration files for installed libraries (beta)";
    "-autogen", Arg.Set autogen_arg, " : generate configuration files for sources (beta)";
  *)
  Typerex_config.version;
]

let arg_usage = "ocp-build [options] targets : a tool to build Objective-Caml projects"


let _ =
  if File.X.exists global_config_file then begin
    try
      SimpleConfig.load config_file
    with e ->
      Printf.fprintf stderr "Error while loading %s\n" (File.to_string global_config_file);
      Printf.fprintf stderr "\tException %s\n%!" (Printexc.to_string e);
      exit 2
  end else begin
    File.Dir.make_all global_config_dir;
    SimpleConfig.save_with_help config_file
  end

let _ =
  set_verbosity !!verbosity_option;
  Arg.parse arg_list (fun s -> targets_arg := s :: !targets_arg) arg_usage;

  if !save_config_arg then
    SimpleConfig.save_with_help config_file;
  ()

let _ =
  if !init_arg then
    let oc = open_out "ocp-build.root" in
    close_out oc

let project_filename =
  try
    BuildOCP.find_project (File.X.getcwd ())  "ocp-build.root"
  with Not_found ->
    Printf.fprintf stderr "Fatal error: no ocp-build.root file found.\n%!";
    Printf.fprintf stderr "\tYou can use the -init option at the root of the project\n";
    Printf.fprintf stderr "\tto create the initial file.\n%!";
    exit 2

let pj = BuildOCP.open_project project_filename

let main () =
  let build_dir_basename = !build_dir_basename_arg in
  let build_dir_filename = absolute_filename build_dir_basename in
  let local_config_file = Filename.concat build_dir_basename BuildGlobals.config_file_basename in
  let _local_config_dir = build_dir_basename in

  let b = BuildEngineContext.create (File.to_string pj.project_dir) build_dir_filename in

  b.stop_on_error_arg <- !stop_on_error_arg;

  targets_arg := List.rev !targets_arg;

  if !verbosity > 1 then Printf.eprintf "Arguments parsed\n%!";

  if !conf_arg then begin
    if !verbosity > 0 then
      Printf.eprintf "Generating configuration file...\n%!";
    BuildConfig.generate_config_file local_config_file;
    if !verbosity > 0 then
      Printf.eprintf "Generating configuration file... DONE\n%!";
  end;


  time "Config time: %.2fs\n%!" BuildConfig.load_config local_config_file;

  (*
    if !distrib_arg then begin
    let dirname =
    if not !global_arg then
    local_config_dir else
    BuildConfig.global_config_dir in

    if !verbosity > 0 then
    Printf.eprintf "Analysing already installed files...\n%!";
    BuildOCFGen.find_installed dirname;

    if !verbosity > 0 then
    Printf.eprintf "Generating configuration for already installed files...\n%!";
    BuildOCFGen.gen_from_distrib dirname;
    if !verbosity > 0 then
    Printf.eprintf "Generating configuration file... DONE\n%!";
    end;

    if !autogen_arg then begin
    List.iter (fun dirname ->
    let basename = Filename.basename dirname in
    let basename = if basename = "." || basename = ".." || basename = "" then
    "???" else basename in
    if !verbosity > 0 then
    Printf.eprintf "Auto-generating .ocp file...\n%!";
    BuildOCFGen.autogen dirname basename;
    if !verbosity > 0 then
    Printf.eprintf "Auto-generating .ocp file...DONE\n%!";
    ) !targets_arg
    end;
  *)

  if !conf_arg || !distrib_arg || !autogen_arg then exit 0;

  if not !byte_arg && not !asm_arg then begin
    byte_arg := true;
    asm_arg := true;
  end;

  let project_ocpbuild_version = create_option pj.project_config
    [ "ocpbuild_version" ]
    ["The version of ocp-build used to save this file"]
    SimpleConfig.string_option version
  in

  let project_autoscan_option = create_option pj.project_config
    [ "autoscan" ]
    ["Always scan for .ocp files in this project sub-directories";
     " - 'None' means use default user settings;";
     " - 'true'/'false' override user setttings"
    ]
    (option_option bool_option) None
  in

  let project_ncores_option = create_option pj.project_config
  [ "ncores" ] ["Number of cores to use on this computer"]
  (option_option int_option) None
  in

  let project_verbosity_option = create_option pj.project_config
    [ "verbosity" ] ["Default verbosity"]
    (option_option int_option) None
  in

  let project_digest_option = create_option pj.project_config
    [ "digest" ]
    ["Use content digest change instead of modification to trigger recompilation"]
    (option_option bool_option) None
  in

  let user_pref arg project_option option =
    match !arg with
        Some x -> x
      | None ->
        match !!project_option with
            None -> !!option
          | Some x -> x
  in

  let force_scan = user_pref
    autoscan_arg project_autoscan_option autoscan_option in

  let use_digests = user_pref
    digest_arg project_digest_option digest_option in

  verbosity := user_pref
    verbosity_arg project_verbosity_option verbosity_option;

  let ncores = user_pref
    ncores_arg project_ncores_option ncores_option in

  if use_digests then BuildEngineMtime.use_digests true;
  if force_scan then  BuildActions.do_scan pj;

  if !init_arg || !save_project_arg then begin

    let set_from_arg arg project_option =
      match !arg with None -> ()
        | Some x -> project_option =:= Some x
    in
    set_from_arg digest_arg project_digest_option;
    set_from_arg verbosity_arg project_verbosity_option;
    set_from_arg autoscan_arg project_autoscan_option;
    set_from_arg ncores_arg project_ncores_option;
    project_ocpbuild_version =:= version;

    SimpleConfig.save_with_help pj.project_config
  end;

  b.verbosity_arg <- !verbosity;
  b.cross_arg <- !cross_arg;


  time "Loading time: %.2fs\n%!" BuildActions.do_load_ocp_files pj;

  let print_project pj = Printf.eprintf "\t%s in %s (%s)\n" pj.package_name (File.to_string pj.package_dirname)
	(match pj.package_type with
	    ProjectProgram -> "program"
	  | ProjectLibrary -> "library"
	  | ProjectToplevel -> "toplevel"
	  | ProjectObjects -> "objects")
      in

    if !verbosity > 2 || !list_projects_arg then begin

      Printf.eprintf "Disabled projects:\n";
      List.iter print_project pj.project_disabled;

      Printf.eprintf "Validated projects:\n";
      Hashtbl.iter (fun _ pj ->
	print_project pj
      ) pj.project_validated;
    end;

    if pj.project_incomplete <> [] then begin
      Printf.eprintf "Warning: %d incomplete projects:\n" (List.length pj.project_incomplete);
      List.iter print_project pj.project_incomplete;
    end;

  Hashtbl.iter (fun (name, tag) list_ref ->
    Printf.eprintf "   \"%s\"%s missed by %d projects\n" name
      (if tag = "" then "" else
	  Printf.sprintf " with tag \"%s\"" tag) (List.length !list_ref);
    List.iter print_project !list_ref;
  ) pj.project_missing;

  time "Context time: %.2fs\n%!" BuildOCamlRules.create pj b;

  if !distclean_arg then begin
    BuildActions.do_distclean ();
    exit 0
  end;

  if !clean_arg then begin
    BuildActions.do_clean b;
    exit 0;
  end;

  if !list_byte_targets_arg then begin
    Printf.eprintf "Bytecode targets:\n";
    StringMap.iter (fun _ lib ->
      if lib.lib_byte_targets <> [] then begin
	List.iter (fun target -> Printf.eprintf "\t%s\t->\t%s\n" lib.lib_name target.file_basename) lib.lib_byte_targets;
      end) !packages_by_name;
	Printf.eprintf "%!"
  end;

  let targets = ref [] in
  let add_project_targets lib =
    if !byte_arg then
      targets := lib.lib_byte_targets @ !targets;
    if !asm_arg then
      targets := lib.lib_asm_targets @ !targets;
  in
  begin
    match !targets_arg with
	[] ->
	  StringMap.iter (fun _ pj -> add_project_targets pj) !packages_by_name
      | list ->
	List.iter (fun name ->
	  try
	    let pj = StringMap.find name !packages_by_name in
	    add_project_targets pj
	  with Not_found ->
	    Printf.eprintf "Error: Could not find target project %s\n%!" name;
	    exit 2
	) list
  end;
  if !targets <> [] then begin
    begin
      try
	time2 "Build init time: %.2f\n%!" BuildEngine.init b !targets
      with BuildEngine.MissingSourceWithNoBuildingRule (r, filename) ->
	let (rule_filename, rule_loc, rule_name) = r.rule_loc in
	BuildMisc.print_loc rule_filename rule_loc;
	Printf.eprintf "Error: in project \"%s\", the source filename\n"
	  rule_name;
	Printf.eprintf "\t\"%s\" does not exist\n" filename;
	BuildRules.print_rule r;
	exit 2
    end;
    let orphans = time2 "Sanitizing time: %.2fs\n%!" BuildEngine.sanitize b !delete_orphans_arg in
    if orphans > 0 then begin
      Printf.fprintf stderr "Error: found %d orphan files in _obuild. You must remove them.\n" orphans;
      Printf.fprintf stderr "\n";
      Printf.fprintf stderr "   You can add the -sanitize argument to automatically remove\n";
      Printf.fprintf stderr "   orphan files\n";
      Printf.fprintf stderr "\n";
      exit 2;
    end else
      if orphans < 0 then
        Printf.fprintf stderr "Warning: deleted %d orphan files in _obuild\n" (-orphans);
    time2  "Building time: %.2fs\n%!" BuildEngine.parallel_loop b ncores;
    let errors = BuildEngine.fatal_errors() @ BuildEngine.errors() in
    if !verbosity >= 0 || errors <> [] then begin
      Printf.eprintf "%s. %d commands executed, %d files generated.\n%!"
	(if errors = [] then "No error" else
	    Printf.sprintf "%d errors" (List.length errors))
	!BuildEngine.stats_command_executed
	!BuildEngine.stats_files_generated;
    end;
    if errors <> [] then begin
      List.iter (fun lines ->
	Printf.eprintf "Error:\n";
	List.iter (fun line ->
	  Printf.eprintf "%s\n" line
	) lines

      ) errors;
      exit 2
    end;
  end;
  Printf.eprintf "%!";
  let t1 = Unix.gettimeofday () in
  if !time_arg then
    Printf.printf "Total time: %.2fs\n%!" (t1 -. t0)


let _ =
  Unix.chdir (File.to_string pj.project_dir);
  Printf.fprintf stdout "ocp-build: Entering directory `%s'\n%!"  (File.to_string pj.project_dir);
  let res =
    try
      main (); 0
    with e ->
      Printf.fprintf stderr "ocp-build: Fatal Exception %s\n%!" (Printexc.to_string e);
      2
  in
  Printf.fprintf stdout "ocp-build: Leaving directory `%s'\n%!" (File.to_string pj.project_dir);

  exit res
