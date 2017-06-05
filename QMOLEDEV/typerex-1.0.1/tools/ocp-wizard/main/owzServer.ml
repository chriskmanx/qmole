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
open Util
include Debug.Tag(struct let tag = "owzServer" end)

let profile_file = ".ocp-wizard-profile.out"
let profile = ref None

exception Invalid_command

type command =
  | Rename of bool
  | Grep of bool
  | GotoDefinition
  | CommentDefinition
  | CycleDefinitions
  | PruneLids
  | EliminateOpen
  | Undo
  | Callback
  | ModifyBuffer of string * string * int * int * int * bool
  | FontifyBuffer of string
  | Complete of string * int (* buffer name, byte pos from 0 *)
  | CompletionDoc of string * string
  | PreCacheBuffer of string
  | Config

open IDE

(* Command names must be valid Java idents, up to replacing all
   '-' by '_'. *)
let callback_commands = [

  "rename", [ [CTRL; Char 'o'] ; [Char 'r' ] ],
  "Rename the current identifier",
  Rename false;

  "rename-toplevel", [ [CTRL; Char 'o'] ; [Char 't' ] ; [Char 'r' ] ],
  "Rename the toplevel module defined by the current source file",
  Rename true;

(*
  "grep", [ [CTRL; Char 'o'] ; [Char 'g' ] ],
  "Show all definitions and references for the current ident",
  Grep false;

  "grep-toplevel", [ [CTRL; Char 'o'] ; [Char 't' ] ; [Char 'g' ] ],
  "Grep the toplevel module defined by the current source file",
  Grep true;
*)

  "undo", [ [CTRL; Char 'o'] ; [Char 'u' ] ],
  "Undo the last multiple-file refactoring action",
  Undo;

  "callback-test", [ [CTRL; Char 'o'] ; [CTRL; Char 't' ] ],
  "testing callbacks",
  Callback;

  "goto-definition", [ [CTRL; Char 'o'] ; [Char 'd' ] ],
  "Go to the definition referenced by the current ident",
  GotoDefinition;

  "comment-definition", [ [CTRL; Char 'o'] ; [Char 'c' ] ],
  "Show the comments associated with the current ident",
  CommentDefinition;

  "cycle-definitions", [ [CTRL; Char 'o'] ; [Char 'a' ] ],
  "Cycle between the alternative definitions of the current def.",
  CycleDefinitions;

  "prune-lids", [ [CTRL; Char 'o'] ; [Char 'p' ] ],
  "Simplify all references in the current file",
  PruneLids;

  "eliminate-open",  [ [CTRL; Char 'o'] ; [Char 'q' ] ],
  "Eliminate an open statement by qualifying all references",
  EliminateOpen

]

module MakePlugin (L : Lang) = struct

  let register_commands () =
    List.map
      (function name, keys, doc, _ ->
        create_command  ~lang:L.lang ~name:("ocp-" ^ name) ~doc ~keys
          (
            L.process_ocp_wizard_with_callbacks "%s" name))
      callback_commands

  let make_emacs_plugin filename =
    ignore (register_commands ());
        let code_of_keymap =
          Emacs.code_of_keymap_aux ~name:"ocp-wizard-plugin" ~add_hook:false in
        IDE.save
          ~lang:L.lang ~code_of_command:L.code_of_command ~code_of_keymap ~filename

end

let commands =
  List.map
    (function name, _, _, command ->
      name,
      (function
        | [] -> command
        | _ -> raise Invalid_command))
    callback_commands
  @ [

    "modify-buffer", (function
      | [buffername ; filename ; start ; end_p ; old_length ; first_time] ->
        ModifyBuffer
          (buffername, filename,
           int_of_string start, int_of_string end_p, int_of_string old_length,
           bool_of_string first_time)
      | _ -> raise Invalid_command);

    "fontify-buffer", (function
      | [buffername] -> FontifyBuffer buffername
      | _ -> raise Invalid_command);

    "completion", (function
      | [buffername ; pos] -> Complete (buffername, int_of_string pos)
      | _ -> raise Invalid_command);

    "completion-doc", (function
      | [buffername ; candidate] -> CompletionDoc (buffername, candidate)
      | _ -> raise Invalid_command);

    "pre-cache-buffer", (function
      | [buffername] -> PreCacheBuffer buffername
      | _ -> raise Invalid_command);

    "version", (function
      | [] -> Config
      | _ -> raise Invalid_command);

  "grep", (function
    | [] -> Grep false
    | _ -> raise Invalid_command);

  "grep-toplevel", (function
    | [] -> Grep true
    | _ -> raise Invalid_command);

  ]

module OwzSocketServer
  (SocketCallback : IDE_Callback.SocketCallback)
  (Specifics : IDE_Specifics.T) = struct

    let process_regular_command ~background ?cwd ~data connection command =
      let module IDE =
            SocketCallback(struct
              let connection = connection
            end)
      in
      let module UI = OwzUI.Make(IDE)(Specifics) in
  (*      let open UI in *)
      match command with
        | Rename toplevel -> UI.rename toplevel ; "OK"
        | Grep toplevel -> UI.grep toplevel
        | GotoDefinition -> UI.goto_definition () ; "OK"
        | CommentDefinition -> UI.comment_definition () ; "OK"
        | CycleDefinitions -> UI.cycle_definitions () ; "OK"
        | PruneLids -> UI.prune_lids () ; "OK"
        | EliminateOpen -> UI.eliminate_open () ; "OK"
        | Undo -> UI.undo_last () ; "OK"
        | Callback -> UI.callback_test () ; "OK"
        | ModifyBuffer
            (buffername, filename, start, end_p, old_length, first_time) ->
          OwzUI.modify ~buffername ~filename ~first_time ~start ~old_length data;
          "OK"
        | FontifyBuffer (buffername) -> UI.colorize buffername
        | Complete (buffername, pos) -> UI.completion buffername pos
        | CompletionDoc (buffername, candidate) ->
          OwzUI.last_completion_doc buffername ~candidate
        | PreCacheBuffer buffername ->
          background := Some (function () -> OwzUI.pre_cache_buffer buffername);
          "OK"
        | Config -> Specifics.config_info

class owzConnection ic oc = object (self)
  inherit Ocp_rpc.tagged_connection ic oc
  val background = ref None
  method process_request command oc =
    Exceptions.catch_owz ~oc
      (function () ->
        Profile.time_reinit ();
        let command_name, args, data =
          match command with
            | [] -> assert false
            | first_line :: data ->
              match String.split first_line ' ' with
                | c :: args -> c, args, data
                | [] -> failwith "empty command"
        in
        let command =
          try
            Profile.time_call "build command"
              (List.assoc command_name commands) args
          with Not_found ->
            fail_owz "Unknown command %s" command_name
        in
        let res =
          Profile.time_call command_name
            (process_regular_command ~background
               ~data (self :> Ocp_rpc.tagged_connection)) command
        in
        debugln "SENDING RESULT:\n%s\n(END OF RESULT)" res;
        Profile.time_call "send result"
          (output_string oc) res;
        if !profile = Some command_name then
          Profile.print_time_stats
            ~filename:
            (Filename.concat (Sys.getenv "HOME") profile_file)
            ();
      )
  method between_requests =
    match !background with
      | Some command ->
        (try command () with _ -> ());
        background := None
      | None -> ()
end

open Unix

let start_server ic oc =
(*  let open Unix in *)
  let t = Unix.localtime (Unix.time ()) in
  debugln
    "\n**************************************\n\
         OCP Wizard server started at %d:%d:%d.\n\
         **************************************\n"
    t.tm_hour t.tm_min t.tm_sec;
  at_exit (function () ->
    let t' = Unix.localtime (Unix.time ()) in
    debugln
      "\n*************************************\n\
         OCP Wizard server stoped at %d:%d:%d.\n\
         *************************************"
      t'.tm_hour t'.tm_min t'.tm_sec
  );
  Sys.catch_break true;
  let connection = new owzConnection ic oc in
  try
    connection#accept_requests
  with Sys.Break ->
    debugln "server killed.";
    exit 0

end
