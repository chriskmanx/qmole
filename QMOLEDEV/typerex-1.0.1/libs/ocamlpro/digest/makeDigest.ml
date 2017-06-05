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

module DigestBase = struct

  type t = string

  let direct_to_string s = s
  let direct_of_string s = s

  let equal s1 s2 = s1 = s2
  let of_hex s = Base16.of_string s
  let to_hex s = Base16.to_string_case false s
  let to_HEX s = Base16.to_string_case true s

  let xor m1 m2 =
    Checksum.xor (direct_to_string m1) (direct_to_string m2)


end

type context = string
let alloc_context len =
  String.create len


module Make(S : sig
  val length : int
  val name : string

  val context_size : unit -> int
  val context_init : context -> unit
  val context_append : context -> string -> int -> int -> unit
  val context_finish : string -> context -> unit

end) = struct

  include S
  include DigestBase
  let random () = Checksum.random_string length
  let null = String.make length '\000'


  let context_size = context_size ()
  let context_create () =
    let s = alloc_context context_size in
    context_init s;
    s


  let context_copy ctx = String.copy ctx
  let context_finish ctx =
    let ctx = String.copy ctx in
    let digest = String.create length in
    context_finish digest ctx;
    direct_of_string digest

  let string s =
    let ctx = context_create () in
    context_append ctx s 0 (String.length s);
    context_finish ctx

  let file f =
    let ctx = context_create () in
    File.X.iter (context_append ctx) f;
    context_finish ctx

end

module type DigestSig = sig
  type t

  val length : int
  val name : string
  val equal : string -> string -> bool
  val direct_to_string : t -> string
  val direct_of_string : string -> t
  val string : string -> t
  val file : File.t -> t

  val null : t
  val of_hex : string -> t
  val to_hex : t -> string
  val to_HEX : t -> string
  val random : unit -> t
  val xor : t -> t -> t

(*
  val null : string
val one : string
val two : string
val to_bits : string -> string
val xor : string -> string -> string
        (*        val digest_subfile : Unix.file_descr.t -> int64 -> int64 -> string *)
val create : unit -> string
val random : unit -> string
val of_string : string -> string
val of_string_safe : string -> string
val to_string : string -> string
val to_string_case : bool -> string -> string
val of_hexa : string -> string
val to_hexa : string -> string
val to_hexa_case : bool -> string -> string
val of_base32 : string -> string
val to_base32 : string -> string
val to_base32_case : bool -> string -> string
val up : string -> int
val up2 : string -> int
val up3 : string -> int
val enabled : bool
val enabled : bool
type context = string
external context_init : context -> unit = "sha1_context_init_ml"
    "noalloc"
external context_append : context -> string -> int -> int -> unit
  = "sha1_context_append_ml" "noalloc"
val context_size : int
val context_create : unit -> string
val context_copy : string -> string
val context_finish : string -> string
val string : string -> string
val enabled : bool
*)

end

(*
open Checksum

module Make(M: sig
      val hash_length : int
      val hash_name : string

(* [unsafe_string digest string string_len] *)
      val unsafe_string : string -> string -> int -> unit

(* [unsafe_file digest filename filesize] *)
        val unsafe_file : string -> string -> int64 -> unit
(* [unsafe_string digest file_fd offset len]
      val digest_subfile : string -> string -> int64 -> int64 -> unit
*)

      module Base : Base
    end) = struct
    open M

    type t = string

    let length = hash_length

    let null = String.make hash_length '\000'
    let one = String.make hash_length '\001'
    let two =  String.make hash_length '\002'

    let equal h1 h2 = (String.compare h1 h2) = 0

    let string s =
      let len = String.length s in
      let digest = String.create hash_length in
      unsafe_string digest s len;
      digest

    let to_bits s =
      let len = String.length s in
      let digest = String.create (8*len) in
      for i = 0 to len-1 do
        let c = int_of_char s.[i] in
        for j = 7 downto 0 do
          digest.[i*8 + (7-j)] <-
            (if c land (1 lsl j) <> 0 then '1' else '0')

        done
      done;
      digest


    let file s =
      let digest = String.create hash_length in
      let file_size = File.X.size64 s in
      unsafe_file digest (File.to_string s) file_size;
      digest

(*
    let digest_subfile fd pos len =
      let digest = String.create hash_length in
      File.iter fd pos len
        (fun fd pos ->
          digest_subfile digest fd pos len);
      digest
*)

    let create () =  String.create hash_length

    let random () = random_string hash_length

    let of_string s =
      let s = Base.of_string s in
      let len = String.length s in
      if len <> length then failwith "Checksum.of_string: bad hash length";
      s

    let of_string_safe s =
      let s = Base.of_string s in
      let len = String.length s in
      if len < length then
        (String.make (length - len) '\000') ^ s
      else s

    let to_string = Base.to_string
    let to_string_case upper s = Base.to_string_case upper s

    let of_hexa = Base16.of_string
    let to_hexa = Base16.to_string
    let to_hexa_case upper s = Base16.to_string_case upper s

    let of_base32 = Base32.of_string
    let to_base32 = Base32.to_string
    let to_base32_case upper s = Base32.to_string_case upper s

(*
    let value_to_hash v = of_string (value_to_string v)

    let hash_to_value v = string_to_value (to_string v)

    let option =
      define_option_class hash_name value_to_hash hash_to_value
*)

    let up s = int_of_char s.[0]
    let up2 s = (int_of_char s.[0])*256+(int_of_char s.[1])
    let up3 s = (int_of_char s.[0])*65536+(int_of_char s.[1])*256+(int_of_char s.[2])

    let enabled = true
  end
*)

open Checksum

(* RFC 2104 examples *)

module HMAC ( H : DigestSig) = struct

    let ipad = String.make 64 (char_of_int 0x36)
    let opad = String.make 64 (char_of_int 0x5c)

    let compute key text =
      let key =
        let len = String.length key in
        if len < 64 then
            let s = String.make 64 '\000' in
            String.blit key 0 s 0 len;
          s else
        if len = 64 then key else
          String.sub key 0 64
      in

      let x1 = xor ipad key in
      let x = x1 ^ text in
      let x = H.string x in
      let x = H.direct_to_string x in
      let x2 = xor opad key in
      let x = x2 ^ x in
      H.string x

  end
