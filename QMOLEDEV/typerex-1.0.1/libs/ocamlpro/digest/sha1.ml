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

open Maker

module PreSha1 = Make(struct

  external unsafe_string : string -> string -> int -> unit = "sha1_unsafe_string" "noalloc"
  external unsafe_file : string -> string -> int64 -> unit = "sha1_unsafe_file" "noalloc"
  external digest_subfile : string -> Unix.file_descr -> int64 -> int64 -> unit =
    "sha1_unsafe64_fd" "noalloc"

  module Base = Base32

end)

include PreSha1
(*
open PreSha1
*)
open Checksum.Test

let enabled =
  try
    let sha1 = "ABCDEFGHGHIJKLMNOPQRSTUVWXYZ2ABC" in
    assert (to_string (of_string sha1) = sha1);

    assert (to_string (string s1) =
        "3I42H3S6NNFQ2MSVX7XZKYAYSCX5QBYJ");
    assert (to_string (string s2) =
        "LOUTZHNQZ74T6UVVEHLUEDSD63W2E6CP");
    assert (to_string (string s3) =
        "ORWD6TJINRJR4BS6RL3W4CWAQ2EDDRVU");
    assert (to_string (string s4) =
        "UUHHSQPHQXN5X6EMYK6CD7IJ7BHZTE77");

    true
  with e ->
    Printf.eprintf "Unable to compute correct Sha1 hashes.\n";
    Printf.eprintf "Send a bug report with your configuration\n";
    Printf.eprintf "and how you obtained this executable.\n";
    Printf.eprintf "Running with Sha1 tree corruption detection disabled.\n";
    Printf.eprintf "(used only if you run the BitTorrent plugin)\n";
    false
    *)


open MakeDigest
include MakeDigest.Make(struct
  let length = 20
  let name = "Sha1"

external context_size : unit -> int = "sha1_context_size_ml" "noalloc"
external context_init : context -> unit = "sha1_context_init_ml" "noalloc"
external context_append : context -> string -> int -> int -> unit =
    "sha1_context_append_ml" "noalloc"
external context_finish : string -> context -> unit =
    "sha1_context_finish_ml" "noalloc"
end)

let _ =
  let sha1 = "ABCDEFGHGHIJKLMNOPQRSTUVWXYZ2ABC" in
  assert (to_hex (of_hex sha1) = sha1);
  assert (to_HEX (of_hex sha1) = sha1);
  ()


