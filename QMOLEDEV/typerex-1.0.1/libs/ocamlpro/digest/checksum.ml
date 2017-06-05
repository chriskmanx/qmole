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

let i_a = int_of_char 'a'
let i_A = int_of_char 'A'
let i_f = int_of_char 'f'
let i_F = int_of_char 'F'
let i_0 = int_of_char '0'
let i_9 = int_of_char '9'

let digit_hexa c =
  let i = int_of_char c in
  if i >= i_a && i <= i_f then i - i_a + 10 else
    if i >= i_A && i <= i_F then i - i_A + 10 else
      if i >= i_0 && i <= i_9 then i - i_0 else
        failwith (Printf.sprintf "Bad hexa char [\\%d]" i)


module type Base =
sig
  val to_string : string -> string
  val to_string_case : bool -> string -> string
  val of_string : string -> string
end


external xor_c : string -> string -> string -> unit = "md4_xor" "noalloc"

let random_string hash_length =
  let s = String.create hash_length in
  for i = 0 to hash_length - 1 do
    s.[i] <- char_of_int (Random.int 256)
  done;
  s

let xor m1 m2 =
  let len1 = String.length m1 in
  let len2 = String.length m2 in
  let len = min len1 len2 in
  let m3 = String.create len in
  xor_c m1 m2 m3;
  m3

module type Digest =
      sig
        type t
        val null : t
        val one : t
        val two : t
        val equal : t -> t -> bool
        val to_string : t -> string
        val to_string_case : bool -> t -> string
        val of_string : string -> t
        val of_string_safe : string -> t
        val to_bits : t -> string
        val to_hexa : t -> string
        val to_hexa_case : bool -> t -> string
        val of_hexa : string -> t
        val to_base32 : t -> string
        val to_base32_case : bool -> t -> string
        val of_base32 : string -> t
        val string : string -> t
        val create : unit -> t
        val direct_of_string : string -> t
        val direct_to_string : t -> string
        val random : unit -> t
(*        val digest_subfile : Unix.file_descr -> int64 -> int64 -> t *)
        val xor : t -> t -> t
        val up : t -> int
        val up2 : t -> int
        val up3 : t -> int
        val length : int
        val enabled : bool
      end

module Test  = struct
  let s1 = ""
  let s2 = "\000"
  let s3 = String.make 1024 'A'
  let s4 = String.make 1025 'A'
end
