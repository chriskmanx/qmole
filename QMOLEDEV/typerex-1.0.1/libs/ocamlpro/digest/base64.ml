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

    let base64tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    let base64tbl_inv = Array.create 128 (-1)
    let _ =
      for i = 0 to String.length base64tbl - 1 do
        base64tbl_inv.(int_of_char base64tbl.[i]) <- i
      done;
      base64tbl_inv.(int_of_char '=') <- 0

(* base64tbl_inv.(int_of_char '=') <- 0 *)

    let int_of_char8 = int_of_char
    let char8_of_int = char_of_int
    let char6_of_int n = base64tbl.[n]
    let int_of_char6 c =
      let n = base64tbl_inv.(int_of_char c) in
      assert (n >= 0);
      n

    let to_string s =
(*      lprintf "Base64.to_string\n"; *)
      let len = String.length s in

      let newlen = (len * 8 + 5) / 6 in
      let dest = String.create newlen in
(*      lprintf "len = %d newlen = %d\n" len newlen; *)

      let rec iter1 pos0 pos1 acc nbits =
(*        lprintf "iter1 %d %d %d %d\n" pos0 pos1 acc nbits; *)
        if pos0 = len then begin
            if nbits > 0 then begin
(*                lprintf "nbits = %d\n" nbits; *)
                let c = char6_of_int (acc lsl (6 - nbits))  in
                dest.[pos1] <- c;
                pos1+1
              end else pos1
          end else
        let c = s.[pos0] in
        let c = int_of_char8 c in
        let acc = (acc lsl 8) lor c in
        let nbits = nbits + 8 in
        iter2 (pos0+1) pos1 acc nbits

      and iter2 pos0 pos1 acc nbits =
(*        lprintf "iter2 %d %d %d %d\n" pos0 pos1 acc nbits; *)
        if nbits >= 6 then
          let nbits = nbits - 6 in
          let c = acc lsr nbits in
(*          lprintf "c = %d\n" c; *)
          let c = char6_of_int c in
          dest.[pos1] <- c;
          let acc = ((1 lsl nbits) - 1) land acc in
          iter2 pos0 (pos1+1) acc nbits
        else
          iter1 pos0 pos1 acc nbits
      in
      let pos = iter1 0 0 0 0 in
      assert (pos = newlen);
      dest

    let of_string s =
(*      lprintf "Base64.of_string\n";       *)
      let len = String.length s in
      let rec iter len =
        if s.[len-1] = '=' then
          iter (len-1) else len
      in
      let len = iter len in

      let newlen = (len * 6) / 8 in
      let dest = String.create newlen in

(*      lprintf "len = %d newlen = %d\n" len newlen; *)

      let rec iter1 pos0 pos1 acc nbits =
        if pos0 = len then begin
            if pos1 < newlen then begin
                let c = char8_of_int acc in
                dest.[pos1] <- c;
                pos1+1
              end else pos1
          end else
        let c = s.[pos0] in
        let c = int_of_char6 c in
        let acc = (acc lsl 6) lor c in
        let nbits = nbits + 6 in
        iter2 (pos0+1) pos1 acc nbits

      and iter2 pos0 pos1 acc nbits =
        if nbits >= 8 then
          let nbits = nbits - 8 in
          let c = acc lsr nbits in
          let c = char8_of_int c in
          dest.[pos1] <- c;
          let acc = ((1 lsl nbits) - 1) land acc in
          iter2 pos0 (pos1+1) acc nbits
        else
          iter1 pos0 pos1 acc nbits
      in
      let pos = iter1 0 0 0 0 in
      assert (pos = newlen);
      dest

    let to_string_case _ x = to_string x
