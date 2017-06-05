(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [char.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

#define CHAR_MAX  127 // 2^7 - 1
#define CHAR_MIN ~128 // -2^7
#define UCHAR_MAX 255 // 2^8 - 1

(* ****** ****** *)

// some common functions on characters

(* ****** ****** *)

castfn char_of_schar (c: schar):<> char
castfn schar_of_char (c: char):<> schar
castfn char_of_uchar (c: uchar):<> char
castfn uchar_of_char (c: char):<> uchar

(* ****** ****** *)

fun char_of_int
  (i: int):<> char = "atspre_char_of_int"
// end of [char_of_int]

fun char_of_int1
  {i: nat | i <= UCHAR_MAX} (i: int i):<> char
  = "atspre_char_of_int"
// end of [char_of_int1]

fun char_of_uint
  (u: uint):<> char = "atspre_char_of_uint"
// end of [char_of_uint]

fun char_of_uint1
  {i: nat | i <= UCHAR_MAX} (u: uint i):<> char
  = "atspre_char_of_uint"
// end of [char_of_uint1]

(* ****** ****** *)

fun schar_of_int
  (i: int):<> schar = "atspre_schar_of_int"
// end of [schar_of_int]

fun schar_of_int1
  {i: nat | i <= UCHAR_MAX} (i: int i):<> schar
  = "atspre_schar_of_int"
// end of [schar_of_int1]

fun schar_of_uint
  (u: uint):<> schar = "atspre_schar_of_uint"
// end of [schar_of_uint]

fun schar_of_uint1
  {i: nat | i <= UCHAR_MAX} (u: uint i):<> schar
  = "atspre_schar_of_uint"
// end of [schar_of_uint1]

(* ****** ****** *)

fun uchar_of_int
  (i: int):<> uchar = "atspre_uchar_of_int"
// end of [uchar_of_int]

fun uchar_of_int1
  {i: nat | i <= UCHAR_MAX} (i: int i):<> uchar
  = "atspre_uchar_of_int"
// end of [uchar_of_int1]

fun uchar_of_uint
  (u: uint):<> uchar = "atspre_uchar_of_uint"
// end of [uchar_of_uint]

fun uchar_of_uint1
  {i: nat | i <= UCHAR_MAX} (u: uint i):<> uchar
  = "atspre_uchar_of_uint"
// end of [uchar_of_uint1]

(* ****** ****** *)

// HX: also declared in [integer.sats]
fun int_of_char
  (c: char):<> int = "atspre_int_of_char"
// end of [int_of_char]

// HX: also declared in [integer.sats]
fun int1_of_char (c: char)
  :<> [i:int | CHAR_MIN <= i; i <= CHAR_MAX] int i
  = "atspre_int_of_char"
// end of [int1_of_char]

// HX: also declared in [integer.sats]
fun int_of_uchar (c: uchar):<> int = "atspre_int_of_uchar"

// HX: also declared in [integer.sats]
fun int1_of_uchar (c: uchar)
  :<> [i:nat | i <= UCHAR_MAX] int i = "atspre_int_of_uchar"
// end of [int1_of_uchar]

(* ****** ****** *)

// implemented in [integer.cats]
fun uint_of_char (c: char):<> uint = "atspre_uint_of_char"

// implemented in [integer.cats]
fun uint1_of_char (c: char)
  :<> [i:nat | i <= UCHAR_MAX] uint i = "atspre_uint_of_char"
// end of [uint1_of_char]

// implemented in [integer.cats]
fun uint_of_uchar (c: uchar):<> uint
  = "atspre_uint_of_uchar"

// implemented in [integer.cats]
fun uint1_of_uchar (c: uchar)
  :<> [i:nat | i <= UCHAR_MAX] uint i = "atspre_uint_of_uchar"
// end of [uint1_of_uchar]

(* ****** ****** *)

fun sub_char_char
  (c1: char, c2: char):<> int = "atspre_sub_char_char"
overload - with sub_char_char

(* ****** ****** *)

fun lt_char_char
  (c1: char, c2: char):<> bool = "atspre_lt_char_char"
and lte_char_char
  (c1: char, c2: char):<> bool = "atspre_lte_char_char"
overload < with lt_char_char
overload <= with lte_char_char

fun gt_char_char
  (c1: char, c2: char):<> bool = "atspre_gt_char_char"
and gte_char_char
  (c1: char, c2: char):<> bool = "atspre_gte_char_char"
overload > with gt_char_char
overload >= with gte_char_char

fun eq_char_char
  (c1: char, c2: char):<> bool = "atspre_eq_char_char"
and neq_char_char
  (c1: char, c2: char):<> bool = "atspre_neq_char_char"
overload = with eq_char_char
overload <> with neq_char_char
overload != with neq_char_char

fun compare_char_char
  (c1: char, c2: char):<> Sgn = "atspre_compare_char_char"
overload compare with compare_char_char

(* ****** ****** *)
//
// HX: print functions for characters
//
symintr fprint_char

fun fprint0_char (out: FILEref, x: char):<!exnref> void
  = "atspre_fprint_char"

fun fprint1_char {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: char):<!exnref> void
  = "atspre_fprint_char"

overload fprint_char with fprint0_char
overload fprint_char with fprint1_char
overload fprint with fprint_char

(* ****** ****** *)

fun print_char (c: char):<!ref> void = "atspre_print_char"
and prerr_char (c: char):<!ref> void = "atspre_prerr_char"

overload print with print_char
overload prerr with prerr_char

//
// stringization
//

fun tostrptr_char
  (c: char):<> strptr1 = "atspre_tostrptr_char"
overload tostrptr with tostrptr_char
fun tostring_char
  (c: char):<> string (1) = "atspre_tostrptr_char"
overload tostring with tostring_char

(* ****** ****** *)

fun char_isalpha (c: char):<> bool // whether the char is in the alphabet
  = "atspre_char_isalpha"
and char_isalnum (c: char):<> bool // whether the char is in the alphanumeric
  = "atspre_char_isalnum"
and char_isascii (c: char):<> bool = "atspre_char_isascii"
and char_iscntrl (c: char):<> bool = "atspre_char_iscntrl"
and char_isdigit (c: char):<> bool // whether the char is a digit
  = "atspre_char_isdigit"
and char_isgraph (c: char):<> bool = "atspre_char_isgraph"
and char_islower (c: char):<> bool = "atspre_char_islower"
and char_isnull (c: char):<> bool = "atspre_char_isnull"
and char_isprint (c: char):<> bool = "atspre_char_isprint"
and char_ispunct (c: char):<> bool = "atspre_char_ispunct"
and char_isspace (c: char):<> bool = "atspre_char_isspace"
and char_isupper (c: char):<> bool = "atspre_char_isupper"
and char_isxdigit (c: char):<> bool // whether the char is a hex digit
  = "atspre_char_isxdigit"

(* ****** ****** *)

fun char_tolower (c: char):<> char = "atspre_char_tolower"
and char_toupper (c: char):<> char = "atspre_char_toupper"

(* ****** ****** *)
//
// HX: indexed char type
//
(* ****** ****** *)

castfn char1_of_char (c: char):<> [c:char] char c

(* ****** ****** *)

fun char1_of_int
  (i: int):<> [c:char] char c = "atspre_char1_of_int"
// end of [char1_of_int]

fun char1_of_uint
  (u: uint):<> [c:char] char c = "atspre_char1_of_uint"
// end of [char1_of_uint]

(* ****** ****** *)

fun sub_char1_char1 {c1,c2:char}
  (c1: char c1, c2: char c2):<> int (c1-c2) = "atspre_sub_char_char"
overload -  with sub_char1_char1

(* ****** ****** *)

fun lt_char1_char1 {c1,c2:char}
  (c1: char c1, c2: char c2):<> bool (c1 < c2)
  = "atspre_lt_char_char"
and lte_char1_char1 {c1,c2:char}
  (c1: char c1, c2: char c2):<> bool (c1 <= c2)
  = "atspre_lte_char_char"
overload < with lt_char1_char1
overload <= with lte_char1_char1

fun gt_char1_char1 {c1,c2:char}
  (c1: char c1, c2: char c2):<> bool (c1 > c2)
  = "atspre_gt_char_char"
and gte_char1_char1 {c1,c2:char}
  (c1: char c1, c2: char c2):<> bool (c1 >= c2)
  = "atspre_gte_char_char"
overload > with gt_char1_char1
overload >= with gte_char1_char1

fun eq_char1_char1 {c1,c2:char}
  (c1: char c1, c2: char c2):<> bool (c1 == c2)
  = "atspre_eq_char_char"
and neq_char1_char1 {c1,c2:char}
  (c1: char c1, c2: char c2):<> bool (c1 <> c2)
  = "atspre_neq_char_char"
overload = with eq_char1_char1
overload <> with neq_char1_char1

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [char.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

(* end of [char.sats] *)
