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

open Unix

type iso8601 = string

let iso8601 () =
  let t = time () in
  let t = gmtime t in
  Printf.sprintf 
    "%04d%02d%02dT%02d:%02d:%02dZ"
    (1900 + t.tm_year)
    (1 + t.tm_mon)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec

let string_of_iso8601 x = x

type timestamp = string

let timestamp () =
  let t = time () in
  let t = gmtime t in
  Printf.sprintf 
    "%04d%02d%02d%02d%02d%02d"
    (1900 + t.tm_year)
    (1 + t.tm_mon)
    t.tm_mday
    t.tm_hour
    t.tm_min
    t.tm_sec

let string_of_timestamp x = x
