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

(** OCP Wizard command *)

open OcpLang
open Util
open Arg
include Debug.Tag(struct let tag = "owzMain" end)

type main_command =
  | EmacsServer of int
  | EclipseServer of int
  | EmacsPlugin of string

exception Invalid_command

let log_file = ".ocp-wizard-log"
let log_something = ref false

let main_commands = [
  "emacs-server", (function
    | [ port ] ->
      if !log_something then
        Debug.append_log_to
          (Filename.concat (Sys.getenv "HOME") log_file);
      EmacsServer (int_of_string port)
    | _ -> raise Invalid_command);

  "eclipse-server", (function
    | [ port ] ->
      if !log_something then
        Debug.append_log_to
          (Filename.concat (Sys.getenv "HOME") log_file);
      EclipseServer (int_of_string port)
    | _ -> raise Invalid_command);

  "emacs-plugin", (function
    | [filename] -> EmacsPlugin filename
    | _ -> raise Invalid_command);
]

let enable_debug s =
  log_something := true;
  List.iter
    (function k ->
      if k = "all" then
        Debug.set_verbose_all true
      else
        Debug.set_verbose_tag k true)
    (String.split s ',')


let enable_profile s =
  Profile.enabled := true;
  OwzServer.profile := Some s

let append_to_list_ref l l' = l := !l @ l'

let extension_option name list desc =
  (name,
   String (function s -> append_to_list_ref list (String.split s ',')),
   Printf.sprintf " Register additional %s" desc)

let options = align [  

  ("-debug", String enable_debug,
   "{all|m1,m2...} Log specified debug information for modules m1, m2 to ~/"^
     log_file);

  ("-backtrace", Unit (function () -> Printexc.record_backtrace true),
   " Print a backtrace in case of error");

  ("-profile", String enable_profile,
   (" Record time profiling information in ~/" ^ OwzServer.profile_file));

  ("-ignore-project-file", Set OwzUI.ignore_project_file,
   " Do not look for a project file");

  ("-default-to-current-dir", Bool (( := ) OwzUI.default_cwd),
   " If no project file is found, consider . as the only project dir");

  ("-restrict-propagation", Set RenamePropagation.restrict_propagation,
   " Do not propagate renaming in case of module type constraints");

  ("-dont-catch-errors", Clear catch_errors,
   " Disable some permissive behaviors and error prettt-printing");

  ("-coloring-theme", String Colorize.set_theme,
   Printf.sprintf " Use the specified color theme (%s)"
     (String.concat ", " (Colorize.list_themes())));

  extension_option "-add-intf-suffixes" SimpleProgram.plain_intf_extensions
    "interface suffixes";
  extension_option "-add-impl-suffixes" SimpleProgram.plain_impl_extensions
    "implementation suffixes";
  extension_option "-add-intf-generating" SimpleProgram.intf_generating_extensions
    "interface-generating suffixes";
  extension_option "-add-impl-generating" SimpleProgram.impl_generating_extensions
    "implementation-generating suffixes";

  Typerex_config.version
]

let usage =
  "Usage: " ^ Sys.argv.(0) ^ " [common options] <command> <arguments>\n\
   Commands:\n  " ^
  String.concat "\n  " (List.map fst main_commands) ^
  "\nOptions:"

let command = ref None

let anonymous current argv c =
  if List.mem_assoc c main_commands then
    let i = !current + 1 and len = Array.length argv in
    let a = Array.to_list (Array.sub argv i (len - i)) in
    (try
       command := Some (List.assoc c main_commands a);
       debugln "parsed command %s" c;
       current := Array.length argv
     with Invalid_command ->
       fail_owz "invalid command %s %s" c (String.concat " " a))
  else
    fail_owz "unknown command %s" c

let parse_command_line ?(current=current) ?(argv=Sys.argv) () =
  try
    parse_argv ~current argv options (anonymous current argv) usage;
    match !command with
      | Some command -> command
      | None ->
        Printf.eprintf "%s: no command provided\n" Sys.argv.(0);
        Arg.usage options usage;
        exit 1
  with Help _ ->
    Arg.usage options usage;
    exit 0

let process_command = function
  | EmacsServer port ->
    let module OwzEmacsServer =
      OwzServer.OwzSocketServer(EmacsCallback.Make)(IDE_Specifics.Emacs) in
    Ocp_rpc.client (fun _ -> OwzEmacsServer.start_server) port
  | EclipseServer port ->
    let module OwzEclipseServer =
      OwzServer.OwzSocketServer(EclipseCallback.Make)(IDE_Specifics.Eclipse) in
    Ocp_rpc.client (fun _ -> OwzEclipseServer.start_server) port
  | EmacsPlugin filename ->
    let module Plugin = OwzServer.MakePlugin(Emacs) in
    Plugin.make_emacs_plugin filename

let _ =
  Profile.enabled := false;
  try
    let command = parse_command_line () in
    process_command command
  with e ->
    let backtrace = Printexc.get_backtrace () in
    debugln "OCP Wizard server terminated on uncaught exception: %s\n%s"
      (Printexc.to_string e) backtrace;
    exit 2





