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

(* TODO: we should check the results of rules, so that we can make
   sure that the two rules building the same temporary files are not
   executed concurrently. *)

open OcpLang
open BuildEngineTypes
open BuildEngineGlobals
open BuildRules

(*
(* open BuildGlobals *)

(* refactor: replace "open X" by "let v = X.v, etc." *)
let verbosity_arg = BuildGlobals.verbosity_arg
let stop_on_error_arg = BuildGlobals.stop_on_error_arg
let cross_arg = BuildGlobals.cross_arg

let build_rules = BuildGlobals.build_rules
let build_files = BuildGlobals.build_files

let build_dir_basename = BuildGlobals.build_dir_basename
let build_dir_filename = BuildGlobals.build_dir_filename

let new_id_generator = BuildGlobals.new_id_generator (* should be in misc: new_counter_from_0 *)
*)

(* TODO: what about files that are in the distribution. We actually have
dependencies towards them, so we should take care of them ! *)

(* TODO: we should have a -cmi option to ocamlc/ocamlopt, to generate
   the .cmi file and stop. That could be useful to completely typecheck a
   project, without actually compiling it. *)

exception MissingSourceWithNoBuildingRule of build_rule * string

let stats_command_executed = ref 0
let stats_files_generated = ref 0

let queue_inactive = ref ([] : build_rule list)
let queue_ready = ref (IntMap.empty : build_rule IntMap.t)
(* let queue_executed = ref (IntMap.empty : BuildEngineTypes.project_info build_rule IntMap.t) *)
(* let queue_running = ref (IntMap.empty : BuildEngineTypes.project_info build_rule IntMap.t) *)
let queue_waiting = ref (IntMap.empty : build_rule IntMap.t)
let queue_not_waiting = ref (IntMap.empty : build_rule IntMap.t)


let cmdbuf = Buffer.create 10000

let rule_need_execution b r =
  if r.rule_forced then true else
    let missing_target = ref false in
    Buffer.clear cmdbuf;
(*    let oldest_target = ref max_float in *)
    let targets = ref [] in
    List.iter (fun f ->
      targets := file_filename f :: !targets;
      if f.file_mtime = BuildEngineMtime.zero then
	missing_target := true
(*      else
	oldest_target := min !oldest_target f.file_mtime *)
    ) r.rule_targets;
    let targets = List.sort compare !targets in
    List.iter (fun target ->
      Printf.bprintf cmdbuf "#target:%s\n" target;
    ) targets;

    let missing_sources = ref 0 in
(*    let newest_source = ref (-1.0) in *)
    IntMap.iter (fun _ f ->
      if f.file_exists then begin
	Printf.bprintf cmdbuf "#source:%s %s\n" (file_filename f) (BuildEngineMtime.to_string f.file_mtime);
(*	newest_source := max !newest_source f.file_mtime *)
      end else begin
	incr missing_sources;
	match f.file_target_of with
	    [] -> assert false; (* raise (MissingSourceWithNoBuildingRule (r, file_filename f)) *)
	  | _ -> ()
      end
    ) r.rule_sources;

    let command_need_execution =
      !missing_target || !missing_sources > 0 (* || !oldest_target < !newest_source *)
    in
    (* Generates the commands to be executed, in order to compare with the
       one stored in the cache. If the command needs to be executed, the old
       command is removed from the cache, and the new command is put in a
       waiting slot, and will be copied in the cache if the command succeeds.
    *)
    let rec iter cmdbuf commands =
      match commands with
	  [] -> Buffer.contents cmdbuf
	| cmd :: commands ->
	  let commands =
	    match cmd with
		Execute cmd ->
		  Printf.bprintf cmdbuf "#command: '%s' '%s'\n"
		    (String.concat "' '" cmd.cmd_command)
		    (String.concat "' '" (List.map string_of_argument cmd.cmd_args));
		  commands
	      | Move (f1, f2) ->
		Printf.bprintf cmdbuf "#move: %s %s\n"
		  (string_of_argument f1) (string_of_argument f2);
		commands
	      | Copy (f1, f2) ->
		Printf.bprintf cmdbuf "#copy: %s %s\n"
		  (string_of_argument f1) (string_of_argument f2);
		commands
	      | MoveIfExists (f1, f2) ->
(* MoveIfExists() is only for non-targets ! Thus, it is not taken into account
in the compilation results.

		Printf.bprintf cmdbuf "#move? %s %s\n"
		  (File.to_string f1) (File.to_string f2);

 *)
		commands
	      | LoadDeps ( _, file, _) ->
		Printf.bprintf cmdbuf "#loaddeps %s\n"
		  (file_filename file); commands
	      | DynamicAction (msg, f) ->
		let actions =
		  try
		    Lazy.force f
		  with e ->
		    Printf.eprintf "Error: exception %s with DynamicAction %s\n%!"
		      (Printexc.to_string e) msg ;
		    exit 2
		in
		actions @ commands
	  in
	  iter cmdbuf commands
    in
    let cmd = iter cmdbuf  r.rule_commands in
    let main_target_digest = Digest.string (file_filename r.rule_main_target) in
    let command_digest = Digest.string cmd in
    Printf.fprintf b.build_cache_log "#RULE %d : %s -> %s if OK\n%s"
      r.rule_id
      (Digest.to_hex main_target_digest)
      (Digest.to_hex command_digest) cmd;
    let command_need_execution =
      try
	let old_digest = DigestMap.find main_target_digest b.build_cache_input
	in
	Printf.fprintf b.build_cache_log "#CACHE was %s\n"
	  (Digest.to_hex old_digest);
	if command_need_execution then begin
	  Printf.fprintf b.build_cache_log
	    "#COMMAND ALREADY NEEDS EXECUTION\n";
	  command_need_execution
	end else
	  if old_digest = command_digest then begin
	    Printf.fprintf b.build_cache_log "#NO NEED FOR EXECUTION CONFIRMED\n";
	    command_need_execution
	  end else begin
	    Printf.fprintf b.build_cache_log "#EXECUTION WOULD BE NEEDED\n";
	    true
	  end
      with Not_found -> (* command_need_execution *)
	Printf.fprintf b.build_cache_log "#NO CACHE: must be executed\n";
	true
    in
    if command_need_execution then begin
      (* if command needs execution, we should prepare to save the
	 command digest in case of success *)
      b.build_cache_entries <- IntMap.add r.rule_id
	(main_target_digest, command_digest) b.build_cache_entries;
	  (* if command needs execution, we can get rid
	     of the former command used *)
      b.build_cache_input <- DigestMap.remove main_target_digest
	b.build_cache_input;
    end;
    assert (!missing_sources = 0);
    command_need_execution

let init b targets =
  if b.verbosity_arg > 2 then
    Printf.eprintf "BuildEngine.init, phase 1: init\n";
  (* Phase 1: clean everything *)
  (* reset and initialize *)
  Hashtbl.iter (fun _ r ->
    r.rule_missing_sources <- 0;
    r.rule_state <- RULE_INACTIVE;
  ) b.build_rules;

  (* Phase 2: check existence and modification times *)
  if b.verbosity_arg > 2 then
    Printf.eprintf "BuildEngine.init, phase 2: loading times\n";

  Hashtbl.iter (fun _ f ->

    if b.verbosity_arg > 3 then Printf.eprintf "Filename %s " (file_filename f);
    begin
      begin
	match f.file_kind with
	    FILE_VIRTUAL ->
	      if b.verbosity_arg > 3 then Printf.eprintf " virtual";
	      f.file_exists <- false;
	      f.file_mtime <- BuildEngineMtime.zero;
	  | FILE_TEMPORARY -> ()
	  | FILE_REAL ->
	    try
              let filename = file_filename f in
	      if b.verbosity_arg > 3 then Printf.eprintf " exists";
	      f.file_exists <- true;
	      f.file_mtime <- BuildEngineMtime.compute filename
	    with _ ->
	      if b.verbosity_arg > 3 then Printf.eprintf " does not exist";
	      f.file_exists <- false;
	      f.file_mtime <- BuildEngineMtime.zero
      end
    end;
      if  b.verbosity_arg > 3 then begin
	begin
	  match f.file_target_of with
	      [] -> Printf.eprintf "(source)"
	    | _ -> Printf.eprintf "(to build [%d rules])" (List.length f.file_target_of)
	end;
	Printf.eprintf "\n%!"
      end;
    ) b.build_files;

(* Phase 3: activate only needed rules to build the targets *)
  (* Keep only rules that are useful to build the current targets *)

  let delayed_rules = ref [] in
  let rec activate_rule r =
    match r.rule_state with
	RULE_INACTIVE ->
	  if b.verbosity_arg > 3 then begin
	    Printf.eprintf "ACTIVATING RULE\n%!";
	    print_rule r;
	  end;
	  r.rule_state <- RULE_ACTIVE;
	  if b.verbosity_arg > 4 then
	    Printf.eprintf "rule %d <- STATE ACTIVE\n" r.rule_id;
	  IntMap.iter (fun _ file -> activate_source file) r.rule_sources
      | RULE_ACTIVE -> () (* already active *)
      | _ -> assert false

  and activate_source f =
    if b.verbosity_arg > 4 then
      Printf.eprintf "activate_source [%s]\n" (file_filename f);
    match f.file_target_of with
	[] ->
	  if b.verbosity_arg > 4 then
	    Printf.eprintf "no rule to build target [%s]\n" (file_filename f);

      | [r] -> activate_rule r
      | _ -> delayed_rules := f.file_target_of :: !delayed_rules
  in
  if b.verbosity_arg > 2 then
    Printf.eprintf "BuildEngine.init, phase 3: activating rules\n";

  List.iter activate_source targets;
  if b.verbosity_arg > 1 then
    Printf.eprintf "%d targets waiting for active rule\n" (List.length !delayed_rules);

  let rec iter_delayed_rules () =
    match !delayed_rules with
	[] -> ()
      | rules :: tail ->
	delayed_rules := tail;
	if List.for_all (fun r -> r.rule_state = RULE_INACTIVE) rules then begin
	  match rules with
	      []  -> assert false
	    | r :: _ ->

	      (* TODO: what shall we do ???? We have several rules
		 leading to the same file, and none of them has been
		 activated yet. Usually, it corresponds to rules
		 generating source files that are used in different
		 projects. Since source files are generated in the
		 source directory, there is no easy way to detect
		 early that all these rules are similar. Either we
		 activate all of them, or we only activate one.  *)
	      if b.verbosity_arg > 2 then begin
		Printf.eprintf "Picking rule %d among other ones\n" r.rule_id;
		Printf.eprintf "All rules:";
		List.iter (fun r -> print_rule r) rules;
		Printf.eprintf "\n";
	      end;
	      activate_rule r
	end;
	iter_delayed_rules ()
  in
  iter_delayed_rules ();
  if b.verbosity_arg > 2 then
    Printf.eprintf "BuildEngine.init, phase 3: activating delayed rules\n";

  (* Phase 4: update the readiness of all active rules *)

  if b.verbosity_arg > 3 then Printf.eprintf "Rules activated\n%!";
  (* A rule needs to be replayed if one of the targets is either non existing
     or older than one of the sources. Even if it exists, a file might be set as
     non existing if it has to be rebuilt.

     We should save the Digests of all files, so that we might not call a
     command if we know that all sources have the same digests as before,
     so we only need to update the timestamps.*)

  let rec check_rule r =
    if match r.rule_state with
	 RULE_ACTIVE | RULE_WAITING -> true
      | _ -> false
	then  (* r.rule_active *)
(*    if not r.rule_waiting then *)
      let missing_target = ref false in
(*      let oldest_target = ref max_float in *)
      List.iter (fun f ->
	if f.file_exists then begin
(*	  oldest_target := min !oldest_target f.file_mtime *)
          ()
	end else
	  missing_target := true
      ) r.rule_targets;

      let missing_sources = ref 0 in
(*      let newest_source = ref (-1.0) in *)
      IntMap.iter (fun _ f ->
	if f.file_exists then begin
(*	  newest_source := max !newest_source f.file_mtime *)
	end else begin
	  incr missing_sources;
	  if b.verbosity_arg > 3 then Printf.eprintf "Missing %s\n%!" (file_filename f);
	  match f.file_target_of with
	      [] -> raise (MissingSourceWithNoBuildingRule (r, file_filename f))
	    | _ -> ()
	end
      ) r.rule_sources;

      IntMap.iter (fun _ f ->
	if f.file_exists then
	(* Time dependencies do not introduce rebuilding rules *)
(*	  newest_source := max !newest_source f.file_mtime *)
	  ()
	else
	  let active_rule = ref false in
	  List.iter (fun r ->
	    if (* r.rule_active *) r.rule_state <> RULE_INACTIVE then begin
	      active_rule := true;
	      incr missing_sources;
	      if b.verbosity_arg > 3 then Printf.eprintf "Waiting for %s\n%!" (file_filename f);
	    end) f.file_target_of;
(*	  if not !active_rule then
	    raise (MissingSourceWithNoBuildingRule (r, file_filename f)) *)
      ) r.rule_time_dependencies;

      r.rule_missing_sources <- !missing_sources;
      if b.verbosity_arg > 3 then Printf.eprintf "Initializing rule %d missing sources to %d\n%!" r.rule_id r.rule_missing_sources;
      if r.rule_state <> RULE_WAITING &&
	(!missing_target ||
	  !missing_sources > 0 || true
         (* || !oldest_target < !newest_source *) )
      then begin
	if b.verbosity_arg > 4 then
	  Printf.eprintf "rule %d <- STATE WAITING\n" r.rule_id;
	r.rule_state  <- RULE_WAITING;
	if b.verbosity_arg > 3 then Printf.eprintf "Killing targets from rule %d\n%!" r.rule_id;

	List.iter kill_target r.rule_targets
      end

  (* We have a problem with .cmi files: They are actually modified by
     both ocamlc and ocamlopt from the same file. To solve this
     problem, we should never put a dependency on a .cmi file if there
     is a corresponding .cmo or .cmx file that should carry the
     dependency instead.  *)

  (* [kill_target file] if [file] already exists at the beginning of the
     build process, it should be recomputed. *)
  and kill_target f =
    if f.file_exists then begin
      if b.verbosity_arg > 3 then Printf.eprintf "Killing target %s\n%!" (file_filename f);
      f.file_exists <- false;
      List.iter kill_rule f.file_target_of;
      List.iter (fun r ->
	if r.rule_state <> RULE_INACTIVE then
	  r.rule_missing_sources <- r.rule_missing_sources + 1;
(*	Printf.eprintf "Killed %s =>\n%!" (file_filename f); *)
(*	Printf.eprintf "Setting rule %d missing sources to %d\n%!" r.rule_id r.rule_missing_sources; *)
	kill_rule r
      ) f.file_source_for;
    end

  and kill_rule r =
    if r.rule_state = RULE_ACTIVE then begin
      if b.verbosity_arg > 3 then Printf.eprintf "Killing rule %d\n%!" r.rule_id;
      if b.verbosity_arg > 4 then
	Printf.eprintf "rule %d <- STATE WAITING\n" r.rule_id;
      r.rule_state <- RULE_WAITING;
      List.iter kill_target r.rule_targets
    end
  in
  if b.verbosity_arg > 2 then
    Printf.eprintf "BuildEngine.init, phase 4: checking active rules\n";
  Hashtbl.iter (fun _ r ->
     check_rule r) b.build_rules;

(* Phase 5: update the different queues *)
(* Now, fill the queues ! *)

  if b.verbosity_arg > 2 then
    Printf.eprintf "BuildEngine.init, phase 5: filling queues\n";

  Hashtbl.iter (fun _ r ->
    match r.rule_state with
	RULE_ACTIVE ->
	  queue_not_waiting := IntMap.add r.rule_id r !queue_not_waiting;
      | RULE_WAITING ->
	if r.rule_missing_sources > 0 then
	  queue_waiting := IntMap.add r.rule_id r !queue_waiting
	else
	  queue_ready := IntMap.add r.rule_id r !queue_ready
      | RULE_INACTIVE ->
	queue_inactive := r :: !queue_inactive
      | RULE_EXECUTING
      | RULE_EXECUTED -> assert false
  ) b.build_rules;

  if b.verbosity_arg > 2 then
    Printf.eprintf "BuildEngine.init, phase 5: done\n";
  ()


let print_waiting_queue q =
  Printf.eprintf "WAITING QUEUES:\n";
  begin
    IntMap.iter (fun _ r ->
      Printf.eprintf "Rule %d ready\n%!" r.rule_id;
      List.iter (fun file ->
	Printf.eprintf "\tTARGET %s\n%!" (file_filename file))
	r.rule_targets;
      List.iter print_indented_command r.rule_commands;
      IntMap.iter (fun _ f ->
	if not f.file_exists then
	  Printf.eprintf "\t\t%s missing\n%!" (file_filename f)
	else
	  Printf.eprintf "\t\t%s done\n%!" (file_filename f)
      ) r.rule_sources
    ) !queue_ready
  end;
  begin
    IntMap.iter (fun _ r ->
      Printf.eprintf "Rule %d waiting for %d sources\n%!" r.rule_id r.rule_missing_sources;
      List.iter (fun file ->
	Printf.eprintf "\tTARGET %s\n%!" (file_filename file))
	r.rule_targets;
      List.iter print_indented_command r.rule_commands;
      IntMap.iter (fun _ f ->
	if not f.file_exists then
	  Printf.eprintf "\t\t%s missing\n%!" (file_filename f)
      ) r.rule_sources
    ) !queue_waiting
  end;
  begin
    List.iter (fun r ->
      Printf.eprintf "Rule %d inactive\n%!" r.rule_id;
      List.iter (fun file ->
	Printf.eprintf "\tTARGET %s\n%!" (file_filename file))
	r.rule_targets;
      List.iter print_indented_command r.rule_commands;
      IntMap.iter (fun _ f ->
	if not f.file_exists then
	  Printf.eprintf "\t\t%s missing\n%!" (file_filename f)
	else
	  Printf.eprintf "\t\t%s done\n%!" (file_filename f)
      ) r.rule_sources
    ) !queue_inactive
  end

let temp_files = ref IntMap.empty

let check_temporary b r file =
  try
    let (r',list) = IntMap.find file.file_id !temp_files in
    if r.rule_missing_sources = 0 && b.verbosity_arg > 1 then
      Printf.eprintf "rule %d postponed to avoid conflict with rule %d\n" r.rule_id r'.rule_id;
    list := r :: !list;
    r.rule_missing_sources <- r.rule_missing_sources + 1;
  with Not_found -> ()

let lock_temporary r file =
  temp_files := IntMap.add file.file_id (r, ref []) !temp_files

let check_temporaries b r =
  if (r.rule_missing_sources <> 0) then begin
    Printf.eprintf "check_temporaries: missing_sources = %d\n" r.rule_missing_sources;
    exit 2;
  end;
  let check_temporary = check_temporary b r in
  List.iter check_temporary r.rule_temporaries;
  List.iter check_temporary r.rule_targets;
  if r.rule_missing_sources > 0 then begin
    queue_waiting := IntMap.add r.rule_id r !queue_waiting;
    true
  end else begin
    let lock_temporary = lock_temporary r in
    List.iter lock_temporary r.rule_temporaries;
    List.iter lock_temporary r.rule_targets;
    false
  end

let release_temporaries b r =
  let release_temporary file =
    let _, rules = IntMap.find file.file_id !temp_files in
    temp_files := IntMap.remove file.file_id !temp_files;
    List.iter (fun r ->

      if b.verbosity_arg > 2 then
	Printf.eprintf "\t\t\trule %d: missing %d -> %d (release_temporary)\n" r.rule_id r.rule_missing_sources (r.rule_missing_sources - 1);
      r.rule_missing_sources <- r.rule_missing_sources - 1;
      assert (r.rule_missing_sources >= 0);
      if r.rule_missing_sources = 0 then begin
	queue_waiting := IntMap.remove r.rule_id !queue_waiting;
	queue_ready := IntMap.add r.rule_id r !queue_ready
      end
    ) !rules
  in
  List.iter release_temporary r.rule_temporaries;
  List.iter release_temporary r.rule_targets;
  ()


let rec next_rule b =
  if b.verbosity_arg > 3 then Printf.eprintf "next_rule: %d targets ready\n%!" (IntMap.cardinal !queue_ready);
  match IntMap.min_elt !queue_ready with
      None ->
	None
    | Some (id, r) ->
      if b.verbosity_arg > 3 then Printf.eprintf "next_rule: testing rule %d\n%!" r.rule_id;
      queue_ready := IntMap.remove id !queue_ready;
      if r.rule_missing_sources > 0 || r.rule_state = RULE_EXECUTED || check_temporaries b r then
	next_rule b
      else begin
      if b.verbosity_arg > 3 then
	begin
	  Printf.eprintf "NEXT RULE\n%!";
	  print_rule r;
	  IntMap.iter (fun _ f ->
	    if not f.file_exists then
	      Printf.eprintf "ERROR: missing source %s\n%!" (file_filename f)
	  ) r.rule_sources;
	end;
	if b.verbosity_arg > 4 then
	  Printf.eprintf "rule %d <- STATE EXECUTING\n" r.rule_id;
	r.rule_state <- RULE_EXECUTING;
	Some r
      end

let errors = ref []
let fatal_errors = ref []

type execution_status =
    EXECUTION_SUCCESS
  | EXECUTION_FAILURE
  | EXECUTION_AVOIDED

let rule_executed b r execution_status =
  if b.verbosity_arg > 2 then
    Printf.eprintf "rule_executed %d\n" r.rule_id;
  if b.verbosity_arg > 4 then
    Printf.eprintf "rule %d <- STATE EXECUTED\n" r.rule_id;
  r.rule_state <- RULE_EXECUTED;
  let temp_dir = rule_temp_dir r in
  if File.X.exists temp_dir then File.Dir.remove_all temp_dir;
  begin
    match execution_status with
	EXECUTION_SUCCESS ->
	  begin try
		  let (target_digest, command_digest) =
		    IntMap.find r.rule_id b.build_cache_entries
		  in
		  b.build_cache_input <- DigestMap.add
		    target_digest command_digest
		    b.build_cache_input
	    with Not_found -> ()
	  end
      | EXECUTION_FAILURE ->
	b.build_cache_entries <- IntMap.remove r.rule_id b.build_cache_entries
      | EXECUTION_AVOIDED -> ()
  end;
  release_temporaries b r;
  List.iter (fun f ->
    if not f.file_exists then begin
      begin
	match f.file_kind with
	    FILE_VIRTUAL ->
	      f.file_exists <- true;
	      f.file_mtime <- BuildEngineMtime.zero;
	(* a virtual dependency should not cause re-running the rule commands *)
	  | FILE_TEMPORARY -> assert false
	  | FILE_REAL ->
	  try
            let filename = file_filename f in
	    ignore ( Unix.stat filename : Unix.stats);
	    f.file_exists <- true;
	    f.file_mtime <- BuildEngineMtime.compute filename
	  with e ->
	    Printf.eprintf "Error, exception %s in rule_executed\n%!"
	      (Printexc.to_string e);
	    errors :=
	      [Printf.sprintf "Target %s not built" (file_filename f);] :: !errors
      end;
      if f.file_exists then
	List.iter (fun r2 ->
	  if r2.rule_state <> RULE_INACTIVE then begin
	  if b.verbosity_arg > 2 then
	    Printf.eprintf "\t\t\trule %d: missing %d -> %d (rule %d executed)\n" r2.rule_id r2.rule_missing_sources (r2.rule_missing_sources - 1) r.rule_id;
	  r2.rule_missing_sources <- r2.rule_missing_sources - 1;
	  if (r2.rule_missing_sources < 0) then begin
	    print_rule r;
	    print_rule r2;
	    exit 2
	  end;
(*	  Printf.eprintf "Generated %s =>\n%!" (file_filename f); *)
(*	  Printf.eprintf "Setting rule %d missing sources to %d\n%!" r.rule_id r.rule_missing_sources; *)

	  if r2.rule_state = RULE_WAITING && r2.rule_missing_sources = 0 then begin
	    queue_waiting := IntMap.remove r2.rule_id !queue_waiting;
	    queue_ready := IntMap.add r2.rule_id r2 !queue_ready
	  end
	  end
	) f.file_source_for
    end else begin
(* TODO: find something to do in that case. .cmi files can be generated as
sub-targets of both .cmo and .cmx files. As such, they are not actually targets
of the rules, but side-effects. *)
      (* We should remove it if it was generated again !!! *)
      if b.verbosity_arg > 3 then Printf.eprintf "Target file was already generated\n%!";
(*
      if !cross_arg && Sys.file_exists (file_filename f) then
	Unix.unlink (file_filename f)
*)
    end
  ) r.rule_targets



let cross_dirname b dirname =
  match b.cross_arg with
      None -> dirname
    | Some arch ->
      let cross_dirname = Filename.concat dirname b.build_dir_basename in
      if not (Sys.file_exists cross_dirname) then dirname else cross_dirname

let unmanaged_dependencies = ref []

(* TODO: file_target_of should be a list of rules. These rules should
   have a weight.  When several rules are available, take the active one
   with the highest weight.

   This would allow to move the .cmo/.cmi -> .cmx/.cmi dependency modification
   from here to buildOCamlRules.ml
*)

exception EmptyListOfDependencies

let rec add_dependency b r target_file filenames =
  match filenames with
      [] -> raise EmptyListOfDependencies
    | filename :: other_filenames ->
      if b.verbosity_arg > 3 then Printf.eprintf "\tDEP %s\n%!" filename;
      let dirname = Filename.dirname filename in
      (*      let pj = r.rule_lib.lib_project in *)
      let dirname = cross_dirname b dirname in
      let dir = BuildEngineContext.add_directory b dirname in
      let basename = Filename.basename filename in
      let src_file =
	try Some (BuildEngineContext.find_file dir basename)
	with Not_found ->
	  unmanaged_dependencies := filename :: !unmanaged_dependencies;
	  None
      in
      match src_file with
	  None -> ()
	| Some src_file ->
	  if List.for_all (fun r -> r.rule_state = RULE_INACTIVE) src_file.file_target_of then begin
	    if b.verbosity_arg > 3 then
	      Printf.eprintf "\t\tDisabled. Trying next.\n";
	    add_dependency b r target_file other_filenames
	  end else
	    if not (IntMap.mem src_file.file_id r.rule_sources) then
	      begin
		if b.verbosity_arg > 3 then
		  Printf.eprintf "\t\tAdding dependency\n";
		r.rule_sources <-
		  IntMap.add src_file.file_id src_file r.rule_sources;
		src_file.file_source_for <- r :: src_file.file_source_for;
		if not src_file.file_exists then begin
		    (*			Printf.eprintf "Missing new dep %s =>\n%!" (file_filename src_file); *)
		    (*			Printf.eprintf "Setting rule %d missing sources to %d\n%!" r.rule_id r.rule_missing_sources; *)

		  if b.verbosity_arg > 3 then
		    Printf.eprintf "\t\t\trule %d: missing %d -> %d\n" r.rule_id r.rule_missing_sources (r.rule_missing_sources + 1);
		  r.rule_missing_sources <- r.rule_missing_sources + 1;
		  if r.rule_missing_sources = 1 then
		    queue_waiting := IntMap.add r.rule_id r !queue_waiting
		end else begin
		  if b.verbosity_arg > 3 then Printf.eprintf "Adding useless dependency to %s\n%!"
		    (file_filename src_file);
		end
	      end

let add_dependency b r target_file filenames =
  try
    add_dependency b r target_file filenames
  with EmptyListOfDependencies ->
      let (rule_filename, rule_loc, fule_project) = r.rule_loc in
      BuildMisc.print_loc rule_filename rule_loc;
    Printf.eprintf "Error, unexpected situation:\n";
    Printf.eprintf "  Dependencies needed by %s\n"
      (file_filename target_file);
    Printf.eprintf "  cannot be obtained by any active compilation rule:\n%!";
    List.iter (fun filename ->
      Printf.eprintf "\t%s\n%!" filename
    ) filenames;
    exit 2


(* TODO: replace BuildOcamldep.load_dependencies by a generic function inside LoadDeps *)
let load_dependency_file b loader file r =
  if b.verbosity_arg > 3 then Printf.eprintf "Loading dependencies from %s\n%!" (file_filename file);
  begin try
	  let dependencies = (* BuildOcamldep.load_dependencies *) loader (file_filename file) in

	  List.iter (fun (filename, deps) ->
	    if b.verbosity_arg > 3 then Printf.eprintf "FILE %s\n%!" filename;
	    let dirname = Filename.dirname filename in
	    let (rule_filename, rule_loc, rule_name) = r.rule_loc in
	    let dirname = cross_dirname b dirname in
	    let dir = BuildEngineContext.add_directory b dirname in
	    let target_file =
	      BuildEngineContext.find_file dir (Filename.basename filename) in
	    List.iter (fun r ->
	      if r.rule_state <> RULE_INACTIVE then begin
		if b.verbosity_arg > 3 then begin
		  Printf.eprintf "Adding deps to rule %d \n%!" r.rule_id;
		  print_rule r;
		end;
		assert (match r.rule_state with RULE_INACTIVE | RULE_WAITING -> true | _ -> false);
		begin
		  unmanaged_dependencies := [];
		  List.iter (add_dependency b r target_file) deps;
		  List.iter (fun filename ->
		    BuildMisc.print_loc rule_filename rule_loc;
		    Printf.eprintf "Warning: file \"%s\" of project \"%s\" depends on\n" (file_filename target_file)  rule_name;
		    Printf.eprintf "  file \"%s\", that is not generated by any project\n%!" filename
		  ) !unmanaged_dependencies;
		  if !unmanaged_dependencies <> [] then begin
		    Unix.unlink (file_filename file);
		    errors := [Printf.sprintf "Dependency file %s contains unmanaged dependencies. Removed. You should rebuild." (file_filename file)] :: !errors
		  end;
		  unmanaged_dependencies := []
		end
	      end
	    )
	      target_file.file_target_of
	  ) dependencies
    with e ->
  (* An exception while reading dependencies. Probably there was a
     problem with the generated file. We should generate it again.
     TODO: add an option to not remove the file for debugging purpose !
  *)
      Unix.unlink (file_filename file);
      errors := [Printf.sprintf "Incorrect dependency file %s (Exception %s). Removed. You should rebuild." (file_filename file) (Printexc.to_string e)] :: !errors
  end


let temp_stdout b r =
  Filename.concat b.build_dir_filename (Printf.sprintf "rule_%d.stdout" r.rule_id)

let temp_stderr b r =
  Filename.concat b.build_dir_filename (Printf.sprintf "rule_%d.stderr" r.rule_id)

let execute_command b proc =
  let r = proc.proc_rule in
  let cmd = match proc.proc_last with
      None -> assert false
    | Some cmd -> cmd
  in
  let cmd_args =
    cmd.cmd_command @ List.map (argument_of_argument r) cmd.cmd_args
  in
  if b.verbosity_arg > 0 then begin
    Printf.eprintf "[%d.%d] BEGIN '%s'\n%!" r.rule_id proc.proc_step
      (String.concat "' '" cmd_args);
  end;
  Printf.fprintf b.build_log "'%s' %s\n"
    (String.concat "' '" cmd_args)
    (match cmd.cmd_stdout_pipe with
	None -> ""
      | Some filename -> Printf.sprintf "> %s" filename);
  let pid = BuildMisc.create_process cmd_args
    (Some (temp_stdout b r)) (Some (temp_stderr b r))
  in
  incr stats_command_executed;
  if b.verbosity_arg > 3 then Printf.eprintf "EXEC started\n%!";
  pid

let new_proc r =
  File.Dir.make_all (rule_temp_dir r);

  let proc = {
    proc_step = 0;
    proc_rule = r;
    proc_commands = r.rule_commands;
    proc_last = None;
  } in
  proc

let print_file message filename =
  let ic = open_in filename in
  let message_printed = ref false in
  begin
    try
      while true do
	let line = input_line ic in
	if not !message_printed then begin
	  message_printed := true;
	  Printf.eprintf "%s\n%!" message
	end;
	Printf.eprintf "%s\n%!" line
      done
    with _ -> ()
  end;
  close_in ic

(* Copy line-oriented file *)
let copy_file b src dst =
  if b.verbosity_arg > 3 then Printf.eprintf "copy_file from %s to %s\n%!" src dst;
  try
  let ic = open_in src in
  let oc = open_out dst in
  begin
    try
      while true do
	let line = input_line ic in
	Printf.fprintf oc "%s\n%!" line
      done
    with _ -> ()
  end;
  close_in ic;
  close_out oc;
  0
  with e ->
    fatal_errors := [
      Printf.sprintf "Error while copying %s to %s:" src dst;
      Printf.sprintf "exception %s" (Printexc.to_string e);
    ] :: !fatal_errors;
    2

let command_executed b proc status =
  if b.verbosity_arg > 3 then Printf.eprintf "command_executed...\n%!";
  match proc.proc_last with
    | Some cmd ->
      let r = proc.proc_rule in
      let cmd_args =
        cmd.cmd_command @ List.map (argument_of_argument r) cmd.cmd_args
      in

      let verbose = status <> 0 || b.verbosity_arg > 0 in

      if verbose then Printf.eprintf "[%d.%d]   END %s\n%!" r.rule_id proc.proc_step
	 (String.concat " " cmd_args);
      let copy_status =
	match cmd.cmd_stdout_pipe with
	    None ->
	      if verbose then
		print_file  "Command stdout:" (temp_stdout b r);
	      0
	  | Some file ->
	    let src = temp_stdout b r in
	    copy_file b src file;
      in
      Unix.unlink (temp_stdout b r);
      if verbose then
	print_file  "Command stderr:" (temp_stderr b r);
      if status <> 0 then begin
	errors :=
	  [
	    Printf.sprintf "[%d.%d] %s" r.rule_id proc.proc_step
	       (String.concat " " cmd_args);
	    File.string_of_file (temp_stderr b r)
	  ] :: !errors;
      end;
      Unix.unlink (temp_stderr b r);
      stats_files_generated := (List.length r.rule_targets) + !stats_files_generated;
      if status <> 0 then status else copy_status
    | None -> assert false

(*
let print_project_location pj =
  let pos = pj.project_loc in
  Printf.eprintf "File \"%s\", line 0, characters %d-%d:\n%!"
    pj.project_filename pos pos
*)

let parallel_loop b ncores =
  let slots = ref IntMap.empty in

  (* [iter freeslots] tries to associate a waiting action to a free slot, if one
     is available. *)
  let rec iter nslots =
    if nslots > 0 then begin
      if
	!fatal_errors <> [] ||
	  (b.stop_on_error_arg && !errors <> []) then wait_for_end nslots else
	match next_rule b with
	    None -> wait_for_end nslots
	  | Some r ->
	    if b.verbosity_arg > 1 then Printf.eprintf "[%d.0] Examining rule\n%!" r.rule_id;
	    if rule_need_execution b r then begin
	      let proc = new_proc r in
	      iter (execute_proc proc nslots)
	    end else begin
	      if b.verbosity_arg > 1 then Printf.eprintf "[%d.0] Execution not needed !!!!\n%!" r.rule_id;
	      rule_executed b r EXECUTION_AVOIDED;
	      iter nslots
	    end
    end else
      wait nslots

  and wait_for_end nslots =
    if !slots = IntMap.empty then begin
      if b.verbosity_arg > 1 then
	print_waiting_queue ()
      else begin
	let len = IntMap.cardinal !queue_waiting in
	if len > 0 then
	  Printf.eprintf "Warning: %d rules waiting in queue !\n%!" len
      end
    end else
      wait nslots

  (* [wait freeslots] waits for one process to terminate, and call [iter] with the new number
     of available slots. *)
  and wait nslots =
    let nslots =
      try
	if b.verbosity_arg > 1 then Printf.eprintf "Wait for %d processes\n%!" (IntMap.cardinal !slots);
	let (pid, status) = Unix.wait () in
	match status with
	  | Unix.WEXITED status ->
	    begin
	      try
		let proc = IntMap.find pid !slots in
		slots := IntMap.remove pid !slots;
		let status =
		  match proc.proc_last with
		      None -> status
		    | Some cmd ->
		      let status = command_executed b proc status in
		      if b.verbosity_arg > 1 then
			Printf.eprintf "[%d.%d] Just finished executing\n%!"
			  proc.proc_rule.rule_id proc.proc_step;
		      (* if b.verbosity_arg > 3 then print_indented_command cmd; *)
		      status
		in
		proc.proc_last <- None;
		if status <> 0 then begin
		  let (rule_filename, rule_loc, rule_name) = proc.proc_rule.rule_loc in
		  (* print_project_location pj; *)
		  Printf.eprintf "[%d.%d] ERROR in project %s\n%!"
		    proc.proc_rule.rule_id proc.proc_step rule_name;
		  (*		   then must_stop := true; *)
		  rule_executed b proc.proc_rule EXECUTION_FAILURE;
		  nslots + 1
		end else
		  execute_proc proc (nslots + 1) (* we just freeed a slot *)
	      with Not_found -> nslots
	    end
	  | _ -> nslots (* We don't care about other signals *)
      with e ->
	if b.verbosity_arg > 3 then
	  Printf.eprintf "waiting loop: exception %s\n%!" (Printexc.to_string e);
	nslots
    in
    iter nslots

  and execute_proc proc nslots =
    match proc.proc_commands with
	[] ->
	  if b.verbosity_arg > 1 then
	    Printf.eprintf "[%d.%d] rule finished\n%!" proc.proc_rule.rule_id proc.proc_step;
	  rule_executed b proc.proc_rule EXECUTION_SUCCESS;
	  nslots
      | cmd :: tail ->
	if b.verbosity_arg > 1 then
	  Printf.eprintf "[%d.%d] command executed\n%!" proc.proc_rule.rule_id proc.proc_step;
	proc.proc_step <- proc.proc_step + 1;
	proc.proc_commands <- tail;
        let r = proc.proc_rule in
	match cmd with
	    Execute cmd ->
	      proc.proc_last <- Some cmd;
	      if b.verbosity_arg > 1 then
		Printf.eprintf "[%d.%d] new exec\n%!" proc.proc_rule.rule_id proc.proc_step;
	      let pid = execute_command b proc in
	      slots := IntMap.add pid proc !slots;
	      nslots - 1
	  | LoadDeps (loader, file, r) ->
	    if b.verbosity_arg > 3 then
	      Printf.eprintf "[%d.%d] load deps\n%!" proc.proc_rule.rule_id proc.proc_step;
	    (*	    let loader =
		    try
		    find_dependency_loader loader
		    with Not_found ->
		    Printf.eprintf "Error: Unable to load dependencies of type '%s'\n" loader;
		    exit 2
		    in *)
	    load_dependency_file b loader file r;
	    execute_proc proc nslots

	  | Copy (f1, f2) ->
	    if b.verbosity_arg > 0 then
	      Printf.eprintf "[%d.%d] cp %s %s\n%!" proc.proc_rule.rule_id proc.proc_step
		(argument_of_argument r f1) (argument_of_argument r f2)
	    ;
	    Printf.fprintf b.build_log "cp %s %s\n"
	      (argument_of_argument r f1) (argument_of_argument r f2);
	    begin try
		    File.X.copy_file (file_of_argument r f1) (file_of_argument r f2)
	      with e ->
		Printf.eprintf "Error copying %s to %s: %s\n%!"
                  (argument_of_argument r f1) (argument_of_argument r f2) (Printexc.to_string e);
		if not (File.X.exists (file_of_argument r f1)) then
		  Printf.eprintf "\tSource file %s does not exist\n%!" (argument_of_argument r f1);
		if not (File.X.exists (File.dirname (file_of_argument r f2))) then
		  Printf.eprintf "\tDestination directory %s does not exist\n%!" (argument_of_argument r f2);
		exit 2;
	    end;
	    execute_proc proc nslots

	  | Move (f1, f2) ->
	    if b.verbosity_arg > 0 then
	      Printf.eprintf "[%d.%d] mv %s %s\n%!" proc.proc_rule.rule_id proc.proc_step
		(argument_of_argument r f1) (argument_of_argument r f2)
	    ;
	    Printf.fprintf b.build_log "mv %s %s\n"
	      (argument_of_argument r f1) (argument_of_argument r f2);
	    begin try
		    Sys.rename (argument_of_argument r f1) (argument_of_argument r f2)
	      with e ->
		Printf.eprintf "Error moving %s to %s: %s\n%!"
                  (argument_of_argument r f1) (argument_of_argument r f2) (Printexc.to_string e);
		if not (File.X.exists (file_of_argument r f1)) then
		  Printf.eprintf "\tSource file %s does not exist\n%!" (argument_of_argument r f1);
		if not (File.X.exists (File.dirname (file_of_argument r f2))) then
		  Printf.eprintf "\tDestination directory %s does not exist\n%!" (argument_of_argument r f2);
		exit 2;
	    end;
	    execute_proc proc nslots

	  | MoveIfExists (f1, f2) ->
            if File.X.exists (file_of_argument r f1) then
	      begin try
	              if b.verbosity_arg > 0 then
	                Printf.eprintf "[%d.%d] mv? %s %s\n%!" proc.proc_rule.rule_id proc.proc_step
		          (argument_of_argument r f1) (argument_of_argument r f2)
	              ;
	              Printf.fprintf b.build_log "mv? %s %s\n"
	                (argument_of_argument r f1)  (argument_of_argument r f2);

		      File.X.rename (file_of_argument r f1) (file_of_argument r f2)
	        with e ->
		  Printf.eprintf "Error moving %s to %s: %s\n%!" (argument_of_argument r f1) (argument_of_argument r f2) (Printexc.to_string e);
		  if not (File.X.exists (file_of_argument r f1)) then
		    Printf.eprintf "\tSource file %s does not exist\n%!" (argument_of_argument r f1);
		  if not (File.X.exists (File.dirname (file_of_argument r f2))) then
		    Printf.eprintf "\tDestination directory %s does not exist\n%!" (File.to_string (File.dirname (file_of_argument r f2)));
		  exit 2;
	      end;
	    execute_proc proc nslots
	  | DynamicAction (msg, f) ->
	    let actions =
	      try
		Lazy.force f
	      with e ->
		Printf.eprintf "Error: exception %s with DynamicAction %s\n%!"
		  (Printexc.to_string e) msg ;
		exit 2
	    in
	    proc.proc_commands <- actions @ proc.proc_commands;
	    execute_proc proc nslots
  in
  iter ncores

let save_cache b =
  let oc = open_out b.build_cache_filename in
  DigestMap.iter (fun d1 d2 ->
    Printf.fprintf oc "%s %s\n" (Digest.to_hex d1) (Digest.to_hex d2)
  ) b.build_cache_input;
  close_out oc;
  close_out b.build_cache_log;
  close_out b.build_log

let parallel_loop b ncores =
  try
    parallel_loop b ncores;
    save_cache b
  with e ->
    save_cache b;
    raise e

let errors () = !errors
let fatal_errors () = !fatal_errors

let sanitize b delete_orphans =
  let dir = File.of_string b.build_dir_filename in
  let cdir = BuildEngineContext.find_directory b b.build_dir_filename in
  let orphan_files = ref 0 in
  let rec iter dir cdir =
    File.Dir.iter (fun basename ->
      let filename = File.add_basename dir basename in
      if File.X.is_directory filename then
        try
          let cdir = BuildEngineContext.find_dir cdir basename in
          iter filename cdir
        with Not_found ->
          Printf.fprintf stderr "Warning: orphan directory %s/%s\n%!"  cdir.dir_fullname basename;
          match delete_orphans with
              KeepOrphans
            | DeleteOrphanFiles -> ()
            | DeleteOrphanFilesAndDirectories ->
              File.Dir.remove_all filename
      else
        try
          ignore (BuildEngineContext.find_file cdir basename)
        with Not_found ->
          Printf.fprintf stderr "Warning: orphan file %s/%s\n%!" cdir.dir_fullname basename;
          match delete_orphans with
              KeepOrphans -> incr orphan_files
            | DeleteOrphanFiles
            | DeleteOrphanFilesAndDirectories ->
              decr orphan_files;
              File.X.remove filename
    ) dir

  in
  File.Dir.iter (fun basename ->
    let filename = File.add_basename dir basename in
    if File.X.is_directory filename then
      try
        let cdir = BuildEngineContext.find_dir cdir basename in
        iter filename cdir
      with Not_found ->
        Printf.fprintf stderr "Warning: orphan directory %s/%s\n%!" cdir.dir_fullname basename;
        match delete_orphans with
            KeepOrphans
          | DeleteOrphanFiles -> ()
          | DeleteOrphanFilesAndDirectories ->
            File.Dir.remove_all filename

  ) dir;
  !orphan_files



