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

open Unix
include Debug.Tag(struct let tag = "server" end)

(* Indicates that following data (starting on next line) is a request,
   and not the answer to a request. This tag is used even in the most
   generic case of a recursive server. *)
let request_start = "REQUEST_START"

(* Mark the end of a message (request or answer). Must be preceeded and
   followed by newlines that are not part of the data. Using this tag is
   optional, it will help for unbounded data. *)
let end_of_message = "END_OF_MESSAGE"

let server server_fun port =
  let sockaddr = ADDR_INET (inet_addr_loopback, port) in
  let sock = socket (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  (* Avoids bind errors when restarting with the same port after a crash: *)
  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  listen sock 0;
  while true do
    debugln "SERVER: WAITING FOR A CONNECTION";
    let (s, caller) = accept sock in
    debugln "SERVER: ACCEPTED A CONNECTION";
    let ic = in_channel_of_descr s in
    let oc = out_channel_of_descr s in
    server_fun ic oc;
    debugln "SERVER: CONNECTION CLOSED BY CLIENT"
  done;
  assert false

let client client_fun port =
  let sockaddr = ADDR_INET (inet_addr_loopback, port) in
  let sock = socket (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  connect sock sockaddr;
  debugln "CLIENT : CONNECTED TO SERVER";
  let ic = in_channel_of_descr sock in
  let oc = out_channel_of_descr sock in
    client_fun sock ic oc

class virtual connection ~nomark ic oc = object (self)

  method private virtual answer_callback : unit
  method between_requests = ()

  val mutable callback_depth = 0

  method private wait_for_message_accepting_callbacks =
    debugln "WAITING FOR REQUEST (LEVEL %d)%s" callback_depth
      (if callback_depth = 0 then "" else " OR ANSWER");
    let task =
      if nomark && callback_depth = 0 then
        `request
      else
        let line = input_line ic in
        if line = request_start then
          `request
        else
          `answer line
    in
    match task with
      | `request ->
        debugln "RECEIVED REQUEST (LEVEL %d)" callback_depth;
        callback_depth <- callback_depth + 1;
        let () = self#answer_callback in
        callback_depth <- callback_depth - 1;
        debugln "ANSWERED REQUEST (LEVEL %d)" callback_depth;
        self#between_requests;
        self#wait_for_message_accepting_callbacks
      | `answer line ->
        line

  method accept_requests : 'a . 'a =
    let answer = self#wait_for_message_accepting_callbacks in
    failwith ("received answer without request: " ^ answer)

  method private send_command :
    'a .
      write_command:(out_channel -> unit) ->
      read_result:(first_line:string -> in_channel -> 'a) -> 'a
        = fun ~write_command ~read_result ->
          if not (nomark && callback_depth = 0) then
            output_string oc (request_start ^ "\n");
          debugln "SENDING COMMAND (LEVEL %d):" callback_depth;
          debugln "%t" write_command;
          write_command oc;
          flush oc;
          debugln "COMMAND SENT";
          callback_depth <- callback_depth + 1;
          let first_line = self#wait_for_message_accepting_callbacks in
          callback_depth <- callback_depth - 1;
          debugln "RECEIVED ANSWER (LEVEL %d) FIRST LINE = %s"
            callback_depth first_line;
          read_result ~first_line ic

end

class virtual string_connection ~nomark ic oc = object (self)

  inherit connection ~nomark ic oc

  method virtual process_request: string -> string

  method send_request c =
    self#send_command
      ~write_command:
      (function oc ->
        output_string oc (c ^ "\n"))
      ~read_result:(fun ~first_line _ -> first_line)

  method private answer_callback =
    let command = input_line ic in
    debugln "PROCESSING COMMAND (NEXT LEVEL %d) %s" callback_depth command;
    let result = self#process_request command in
    debugln "SENDING_RESULT (NEXT LEVEL %d) %s" callback_depth result;
    output_string oc (result ^ "\n");
    flush oc

end

let read_until_end acc ic =
  let rev_lines = ref acc in
  while
    (match !rev_lines with
      | [] -> true
      | t :: _ -> t <> end_of_message)
  do
    rev_lines := input_line ic :: !rev_lines
  done;
  match !rev_lines with
    | _ :: q -> List.rev q
    | [] -> assert false

class virtual tagged_connection ic oc = object (self)

  inherit connection ~nomark:false ic oc

  method virtual process_request : string list -> out_channel -> unit

  method send_request ~write_command =
    self#send_command
      ~write_command:
      (function oc ->
        let () = write_command oc in
        output_string oc ("\n" ^ end_of_message ^"\n"))
      ~read_result:
      (fun ~first_line ic ->
        let res = read_until_end [first_line] ic in
        res)

  method private answer_callback =
    let command = read_until_end [] ic in
    debugln "PROCESSING REQUEST:\n%t(END OF REQUEST)"
      (function c -> List.iter (Printf.fprintf c "%s\n") command);
    self#process_request command oc;
    output_string oc ("\n" ^ end_of_message ^"\n");
    flush oc

end
