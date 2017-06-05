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


type t =
    Inode of int * int * float
  | Digest of Digest.t

let use_digests = ref false

let zero = Inode (0,0, 0.)

let to_string mtime =
  match mtime with
      Inode (dev, ino, mtime) -> Printf.sprintf "%d.%d.%.0f" dev ino mtime
    | Digest sha1 -> Digest.to_hex sha1

let compute filename =
  if !use_digests then
    Digest (Digest.file filename)
  else
    let st = Unix.lstat filename in
    Inode (st.Unix.st_dev, st.Unix.st_ino, st.Unix.st_mtime)

let use_digests bool = use_digests := bool

