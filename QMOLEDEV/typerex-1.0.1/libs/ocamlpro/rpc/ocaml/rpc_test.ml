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

(** Outdated tests ! *)

include Debug.Tag(struct let tag = "serverTest" end)

let _ = Debug.set_verbose_all true

let server =
  Ocp_rpc.server
    (fun ic oc ->
      let connection =
        object (self)
          inherit Ocp_rpc.string_connection ~nomark:true ic oc
          method process_request command =
            if command = "hello" then
              ignore (self#send_request "callback");
            command ^ " yourself"
        end
      in
      try connection#accept_requests
      with End_of_file ->
        close_out oc)

let client =
  Ocp_rpc.client
    (fun socket ic oc ->
      let connection =
        object (self)
          inherit Ocp_rpc.string_connection ~nomark:true ic oc
          method process_request _ =
            ignore (self#send_request "inner callback");
            "callback result"
        end
      in
      ignore (connection#send_request "hello");
      close_out oc)

(** Un autre test plus rigolo: *)

class decr ic oc = object (self)
  inherit Ocp_rpc.string_connection ~nomark:false ic oc
  method process_request command =
    let n = int_of_string command in
    if n > 0 then
      ignore (self#send_request (string_of_int (n-1)));
    "OK"
end

let server =
  Ocp_rpc.server
    (fun ic oc ->
      let connection = new decr ic oc in
      try connection#accept_requests
      with End_of_file ->
        close_out oc)

let client =
  Ocp_rpc.client
    (fun socket ic oc ->
      let connection = new decr ic oc in
      ignore (connection#send_request "3");
      close_out oc)

let server =
  Ocp_rpc.server
    (fun ic oc ->
      let connection =
        object (self)
          inherit Ocp_rpc.tagged_connection ic oc
          method process_request command oc =
            if command = ["hello"] then (
              ignore
                (self#send_request
                   (function oc ->
                     output_string oc "(message \"hello client\")"));
              ignore
                (self#send_request
                   (function oc ->
                     output_string oc "(string-command \"hi\")"))
            );
            output_string oc (List.hd command ^ " yourself")
        end
      in
      try connection#accept_requests
      with End_of_file ->
        close_out oc)


let _ =
  match Sys.argv.(1) with
    | "client" -> client 1888
    | "server" -> server 1888
    | _ -> raise Exit
