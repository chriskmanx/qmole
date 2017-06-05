(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//
(* ****** ****** *)

//
// SML Basis Library: String (http://www.standardml.org/Basis/string.html)
//

(* ****** ****** *)
//
// HX: this one is undefined!
//
fun maxSize (): size_t // maximal string size allowed

(* ****** ****** *)

fun size {n:nat} (s: string n): size_t n

fun sub {n,i:nat | i < n} (s: string n, i: size_t i): char

fun extract {n,i:nat} {b:bool;j:nat | i+j <= n}
  (s: string n, i: size_t i, j: option (size_t j, b))
  : [k:nat | (b && k == j) || (~b && k == n-i)] string (k)

fun substring
  {n:int} {i,ln:nat | i+ln <= n}
  (s: string n, ofs: size_t i, len: size_t ln): string (ln)
// end of [substring]

fun ^ {i,j:nat} (s1: string i, s2: string j): string (i+j)

fun concat (ss: list0 string): string
fun concatWith (sep: string, ss: list0 string): string

fun str (c: char): string (1)

fun implode (cs: list0 char): string
fun explode (str: string): list0 char

(* ****** ****** *)

fun map {n:nat}
  (f: char -<cloref1> char, s: string n): string n
// end of [map]

fun translate (f: char -<cloref1> string, s: string): string

(* ****** ****** *)

fun tokens (isdelim: char -<cloref1> bool, s: string): list0 string
fun fields (isdelim: char -<cloref1> bool, s: string): list0 string

(* ****** ****** *)

fun isPrefix (s1: string, s2: string): bool
fun isSubstring (s1: string, s2: string): bool
fun isSuffix (s1: string, s2: string): bool

(* ****** ****** *)

fun lt (s1: string, s2: string): bool
fun lte (s1: string, s2: string): bool
fun gt (s1: string, s2: string): bool
fun gte (s1: string, s2: string): bool

fun eq (s1: string, s2: string): bool 
fun neq (s1: string, s2: string): bool 

fun compare (s1: string, s2: string): int

fun collate (f: (char, char) -<cloref1> int, s1: string, s2: string): int

(* ****** ****** *)

(*

these one are yet to be implemented:

val toString : string -> String.string
val scan       : (char, 'a) StringCvt.reader
                       -> (string, 'a) StringCvt.reader
val fromString : String.string -> string option
val toCString : string -> String.string
val fromCString : String.string -> string option 

*)

(* ****** ****** *)

(* end of [string.sats] *)
