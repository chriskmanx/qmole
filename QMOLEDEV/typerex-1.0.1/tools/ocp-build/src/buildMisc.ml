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
open OcpSystem

(* open SafeCaml *)

let print_loc filename pos =
  let line = ref 1 in
  let last_line_pos = ref 0 in
  begin try
	  let s = File.string_of_file filename in
	  for i = 0 to pos - 1 do
	    if s.[i] = '\n' then begin
	      incr line;
	      last_line_pos := i
	    end
	  done
    with _ -> ()
  end;
  let pos = pos - !last_line_pos in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n%!"
    filename !line pos pos

exception ParseError

let token_list_of_filename filename lexer =
  let s = File.string_of_file filename in
  let str1 = Stream.of_string s in
  let str2 = lexer str1 in
  let list = ref [] in
  try
    Stream.iter (fun token ->
      let token_pos = Stream.count str1 in
      list := (token, token_pos) :: !list) str2;
    List.rev !list
  with
      Stream.Error error ->
	print_loc filename (Stream.count str1);
	Printf.eprintf "Error: %s\n%!" error;
	raise ParseError

let openflags =  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]

module Unix2 : sig
  open Unix

  type error_handler = exn -> unit

  val create_process : error_handler ->
    string -> string array -> file_descr -> file_descr -> file_descr -> int

  val create_process_env : error_handler ->
    string -> string array -> string array -> file_descr -> file_descr ->
    file_descr -> int

end = struct
  open Unix
  type error_handler = exn -> unit


let rec safe_dup fd =
  let new_fd = dup fd in
  if Obj.magic new_fd >= 3 then (* TODO: windows incompatibility *)
    new_fd
  else begin
    let res = safe_dup fd in
    close new_fd;
    res
  end

let safe_close fd =
  try close fd with Unix_error(_,_,_) -> ()

let perform_redirections new_stdin new_stdout new_stderr =
  let newnewstdin = safe_dup new_stdin in
  let newnewstdout = safe_dup new_stdout in
  let newnewstderr = safe_dup new_stderr in
  safe_close new_stdin;
  safe_close new_stdout;
  safe_close new_stderr;
  dup2 newnewstdin stdin; close newnewstdin;
  dup2 newnewstdout stdout; close newnewstdout;
  dup2 newnewstderr stderr; close newnewstderr

let create_process error_handler cmd args new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      begin try
        perform_redirections new_stdin new_stdout new_stderr;
        execvp cmd args
      with e ->
        error_handler e;
        exit 127
      end
  | id -> id

let create_process_env error_handler cmd args env new_stdin new_stdout new_stderr =
  match fork() with
    0 ->
      begin try
        perform_redirections new_stdin new_stdout new_stderr;
        execvpe cmd args env
      with e ->
        error_handler e;
        exit 127
      end
  | id -> id

end

let create_process list stdout stderr =
  match list with
      [] -> assert false
    | cmd :: args ->
      let stdout_fd = match stdout with
	  None -> Unix.stdout
	| Some filename -> Unix.openfile filename openflags 0o644
      in
      let stderr_fd = match stderr with
	  None -> Unix.stderr
	| Some filename -> Unix.openfile filename openflags 0o644
      in
      let error_handler e =
        Printf.eprintf "Error while executing subprocess\n";
        Printf.eprintf "  exception %s\n%!" (Printexc.to_string e);
      in

      let pid = Unix2.create_process error_handler cmd (Array.of_list (cmd :: args))
	Unix.stdin stdout_fd stderr_fd in
      (match stdout with None -> () | Some _ -> Unix.close stdout_fd);
      (match stderr with None -> () | Some _ -> Unix.close stderr_fd);
      pid

let rec wait_command pid =
  try
    let rec iter pid =
      let (_, status) = Unix.waitpid [] pid in
      match status with
	  Unix.WEXITED n -> n
	| _ -> iter pid
    in
    iter pid
  with e ->
    Printf.eprintf "Exception %s in waitpid\n%!" (Printexc.to_string e);
    exit 2


let new_counter_int0 () =
  let counter = ref 0 in
  fun () ->
  let id = !counter in
  incr counter;
  id


let _ =
  Printexc.register_printer (fun exn ->
    match exn with
	Unix.Unix_error (error, s1, s2) ->
          Some (Printf.sprintf "Unix_error(%s, %s, %s)"
		  (Unix.error_message error) s1 s2)
      | _ -> None)
;;

let rec safe_mkdir filename =
  try
    let st = Unix.stat filename in
    match st.Unix.st_kind with
	Unix.S_DIR ->
	  ()
      | _ ->
	Printf.fprintf stderr
	  "Error in BuildGlobals.safe_mkdir: %s is not a directory. Exiting.\n"
	  filename;
	exit 2
  with _ ->
    let dirname = Filename.dirname filename in
    safe_mkdir dirname;
    Unix.mkdir filename 0o755
