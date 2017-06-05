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

    let direct_to_string s = s
    let direct_of_string s = s

    let xor m1 m2 =
      xor (direct_to_string m1) (direct_to_string m2)

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

open Checksum

(* RFC 2104 examples *)

module HMAC ( H : Digest) = struct

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
