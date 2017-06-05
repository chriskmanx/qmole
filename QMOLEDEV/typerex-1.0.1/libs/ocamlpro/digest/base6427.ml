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

    let _ = assert (String.length base64tbl = 64)

    let to_string hashbin =
      let hash64 = String.create 30 in
      let hashbin n = int_of_char hashbin.[n] in
      hash64.[0] <- '=';
      let j = ref 1 in
      for i = 0 to 6 do
        let tmp = if i < 6 then
            ((hashbin (3*i)) lsl 16) lor ((hashbin(3*i+1)) lsl 8)
            lor (hashbin (3*i+2))
          else
            ((hashbin(3*i)) lsl 16) lor ((hashbin(3*i+1)) lsl 8)
        in
        for k = 0 to 3 do
          hash64.[!j] <- base64tbl.[(tmp lsr ((3- k)*6)) land 0x3f];
          incr j
        done
      done;
      hash64.[!j-1] <- '=';
      String.sub hash64 0 !j

    let base64tbl_inv = String.create 126
    let _ =
      for i = 0 to 63 do
        base64tbl_inv.[int_of_char base64tbl.[i]] <- char_of_int i
      done

    let of_string hash64 =
      let hashbin = String.make 20 '\000' in
      let hash64 n =
        let c = hash64.[n] in
        int_of_char base64tbl_inv.[int_of_char c]
      in
      let j = ref 0 in
      for i = 0 to 6 do
        if i < 6 then
          let tmp = ref 0 in
          for k = 0 to 3 do
            tmp := (!tmp lsl 6) lor (hash64 (i*4+k+1))
          done;
          hashbin.[!j] <- char_of_int ((!tmp lsr 16) land 0xff);
          hashbin.[!j+1] <- char_of_int ((!tmp lsr  8) land 0xff);
          hashbin.[!j+2] <- char_of_int ((!tmp lsr  0) land 0xff);
          j := !j + 3;
        else
        let tmp = ref 0 in
        for k = 0 to 2 do
          tmp := (!tmp lsl 6) lor (hash64 (i*4+k+1))
        done;
        tmp := (!tmp lsl 6);
        hashbin.[!j] <- char_of_int ((!tmp lsr 16) land 0xff);
        hashbin.[!j+1] <- char_of_int ((!tmp lsr  8) land 0xff);
        j := !j + 2;
      done;
      hashbin

    let to_string_case _ = to_string
