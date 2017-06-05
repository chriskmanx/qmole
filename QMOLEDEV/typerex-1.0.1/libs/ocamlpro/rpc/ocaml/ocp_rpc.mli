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

(** Nested remote procedure calls over a socket *)

(** Note: the term RPC is used in a general sense here, and does not
    mean the ONC RPC standard. *)

(** {4 Connecting sockets} *)

(** The following two functions offer a simple high-level way to
    connect sequential processes though sockets, independently of any
    application protocol. *)

(** Run a single-threaded server which will accept arbitrary many
    consecutive connections on the specified port, by applying the
    provided function to the channels (this function should close the
    connection before return).

    Connection is closed by the client: the server receives an EOF on
    its input and then closes its output, ready for another
    connection. *)
val server : (in_channel -> out_channel -> 'a) -> int -> 'b

(** Connect to a server through the specified port, then apply the
    provided function to the channels and return its result (or error)
    after closing the connection. *)
val client : (Unix.file_descr -> in_channel -> out_channel -> 'a) -> int -> 'a
(* This function does not implement its specification and should be
   checked before other use, as the only user currently (ocp-wizard)
   never returns or raises an exception. *)

(** {4 Implementing remote procedure calls} *)

(** The following classes implement different versions of a recursive
    command/response protocol allowing arbitrarily nested
    requests/callbacks between sequential processes. Each class can be
    used to implement either the server or the client (or both if they
    are both in OCaml). To use one of this class, you should:

    - extend it, providing an implementation of the virtual method
    [process_request]. This implementation is responsible for catching
    application-specific errors and encoding them as appropriate.

    - invoke the method [send_request] as necessary,

    - for the server-side, call the method [accept_requests].

    See [ServerTest] for examples. *)

(** This version of the protocol is designed to allow an easy
    implementation by having all data clearly delimited with an ending
    tag preceeded and followed by newline characters. Thus, commands
    and answer may be arbitrary strings with newlines (as long as they
    don't contain the ending tag). *)
class virtual tagged_connection : in_channel -> out_channel -> object

  (** Accept incomming requests forever. This is the method to call to
      run a server.

      @raise [End_of_file] when disconnected.

      @raise [Failure] in case of protocol error. *)
  method accept_requests : 'a

  (** The function for processing of requests must be provided here,
      and will be invoked with the lines (without newlines) of each
      request. It may invoke [send_request] to send callbacks. Result
      should be written to the channel.

      This funcion is responsible for catching application-specific errors
      and encoding them as appropriate ! *)
  method virtual process_request : string list -> out_channel -> unit

  (** Send a command to the other side. *)
  method send_request : write_command:(out_channel -> unit) -> string list

  (** Undocumented hook *)
  method between_requests : unit
end

(**/**)

(** A very simple version of the protocol: all commands are strings
    which may not contain newlines. Therefore, only one tag is needed
    to distinguish between a callback and an answer. If [nomark] is
    true, then this mark is omitted for the toplevel requests to the
    server. *)
class virtual string_connection : nomark:bool -> in_channel -> out_channel ->
object
  (** Accept incomming requests forever. This is the method to call to
      run a server.
      @raise End_of_file when disconnected. *)
  method accept_requests : 'a

  (** The function for processing of requests must be provided
      here. It may invoke [send_string_command] to send callbacks. *)
  method virtual process_request : string -> string

  (** Send a command to the other side. *)
  method send_request : string -> string

  (** Undocumented hook *)
  method between_requests : unit
end
