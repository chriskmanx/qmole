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

    let char_of_int5 n =
      char_of_int (if n < 26 then 65+n else
          50+(n-26))

    let int5_of_char n =
      match n with
        'A' .. 'Z' -> int_of_char n - 65
      | 'a' .. 'z' -> int_of_char n - 97
      | '0' .. '9' -> (int_of_char n+26)-50
      | _ -> failwith "Base32.int_of_char bad char"

    let of_string r =
      let len = String.length r in
      let hash_length = (len * 5 - 4) / 8 + 1 in
      let s = String.make hash_length '\000' in
      for i = 0 to len - 1 do
        let pos = i * 5 in
        let byte = pos / 8 in
        let bit = pos mod 8 in
        let c = int5_of_char r.[i] in
        if bit < 3 then
          let x = c lsl (3-bit) in
          s.[byte] <- char_of_int (int_of_char s.[byte] lor x);
        else
        let x = (c lsr (bit - 3)) land 0xff in
        s.[byte] <- char_of_int (int_of_char s.[byte] lor x);
        if byte+1 < hash_length then
          let y = (c lsl (11 - bit)) land 0xff in
          s.[byte+1] <- char_of_int (int_of_char s.[byte+1] lor y);
      done;
      s

    let to_string s =
      let hash_length = String.length s in
      let len = (hash_length * 8 + 4)/5 in
      let r = String.create len in
      for i = 0 to len - 1 do
        let pos = i * 5 in
        let byte = pos / 8 in
        let bit = pos mod 8 in
        if bit < 3 then
          let x = int_of_char s.[byte] in
          let c = (x lsr (3 - bit)) land 0x1f in
          r.[i] <- char_of_int5 c
        else
        let x = int_of_char s.[byte] in
        let y = if byte + 1 = hash_length then 0 else
            int_of_char s.[byte+1] in
        let x = (x lsl 8) + y in
        let c = (x lsr (11 - bit)) land 0x1f in
        r.[i] <- char_of_int5 c
      done;
      r

    let char_of_int5 upper n =
      char_of_int (if n < 26 then (if upper then 65 else 97)+n else
          50+(n-26))

    let to_string_case upper s =
      let hash_length = String.length s in
      let len = (hash_length * 8 + 4)/5 in
      let r = String.create len in
      for i = 0 to len - 1 do
        let pos = i * 5 in
        let byte = pos / 8 in
        let bit = pos mod 8 in
        if bit < 3 then
          let x = int_of_char s.[byte] in
          let c = (x lsr (3 - bit)) land 0x1f in
          r.[i] <- char_of_int5 upper c
        else
        let x = int_of_char s.[byte] in
        let y = if byte + 1 = hash_length then 0 else
            int_of_char s.[byte+1] in
        let x = (x lsl 8) + y in
        let c = (x lsr (11 - bit)) land 0x1f in
        r.[i] <- char_of_int5 upper c
      done;
      r
