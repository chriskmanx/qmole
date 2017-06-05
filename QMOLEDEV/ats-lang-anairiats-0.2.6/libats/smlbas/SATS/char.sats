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
// SML Basis Library: Char (http://www.standardml.org/Basis/char.html)
//

(* ****** ****** *)

val minChar : char
val maxChar : char
val maxOrd : uint

(* ****** ****** *)

fun ord (c: char): uint
fun chr (i: uint): char
fun succ (c: char): char
fun pred (c: char): char

(* ****** ****** *)

fun compare (c1: char, c2: char): int

fun lt (c1: char, c2: char): bool
overload < with lt

fun lte (c1: char, c2: char): bool
overload <= with lte

fun gt (c1: char, c2: char): bool
overload > with gt

fun gte (c1: char, c2: char): bool
overload >= with gte

(* ****** ****** *)

fun contains (s: string, c: char): bool
fun notContains (s: string, c: char): bool

(* ****** ****** *)

fun isAscii (c: char): bool
fun isAlpha (c: char): bool
fun isAlphaNum (c: char): bool
fun isCntrl (c: char): bool
fun isDigit (c: char): bool
fun isGraph (c: char): bool
fun isHexDigit (c: char): bool
fun isLower (c: char): bool
fun isPrint (c: char): bool
fun isSpace (c: char): bool
fun isPunct (c: char): bool
fun isUpper (c: char): bool

(* ****** ****** *)

fun toLower (c: char): char
fun toUpper (c: char): char

(* ****** ****** *)

(*

fun toString (c: char): string // not implemented
fun fromString (s: string): option0 char // not implemented

*)

(* ****** ****** *)

//
// not yet implemented
// fun scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
//

(* ****** ****** *)

fun toCString (c: char): string
fun fromCString (s: string): option0 char

(* ****** ****** *)

(* end of [char.sats] *)
