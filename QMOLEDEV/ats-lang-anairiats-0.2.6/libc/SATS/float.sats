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
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
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
(* author: Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu) *)

(* ****** ****** *)
//
// HX-2011-08-01: note that [float.h] is rather new (gcc 4.6)
//
(* ****** ****** *)

%{#
#include "libc/CATS/float.cats"
%} // end of [%{#]

(* ****** ****** *)

(*
#define ATS_STALOADFLAG 0 // no need for static loading at run-time
*)

(* ****** ****** *)

fun{a:t@ype} RADIX_tmp (): int
macdef DBL_RADIX = $extval (int, "DBL_RADIX")
macdef FLT_RADIX = $extval (int, "FLT_RADIX")
macdef LDBL_RADIX = $extval (int, "LDBL_RADIX")

//
// HX: there is no DBL_ROUNDS or LDBL_ROUNDS
//
macdef FLT_ROUNDS = $extval (int, "FLT_ROUNDS")

(* ****** ****** *)

fun{a:t@ype} DIG_tmp (): int
macdef DBL_DIG = $extval (int, "DBL_DIG")
macdef FLT_DIG = $extval (int, "FLT_DIG")
macdef LDBL_DIG = $extval (int, "LDBL_DIG")

fun{a:t@ype} MANT_DIG_tmp (): int
macdef DBL_MANT_DIG = $extval (int, "DBL_MANT_DIG")
macdef FLT_MANT_DIG = $extval (int, "FLT_MANT_DIG")
macdef LDBL_MANT_DIG = $extval (int, "LDBL_MANT_DIG")

(* ****** ****** *)

fun{a:t@ype} MAX_EXP_tmp ():<> int
macdef DBL_MAX_EXP = $extval (int, "DBL_MAX_EXP")
macdef FLT_MAX_EXP = $extval (int, "FLT_MAX_EXP")
macdef LDBL_MAX_EXP = $extval (int, "LDBL_MAX_EXP")

fun{a:t@ype} MIN_EXP_tmp ():<> int
macdef DBL_MIN_EXP = $extval (int, "DBL_MIN_EXP")
macdef FLT_MIN_EXP = $extval (int, "FLT_MIN_EXP")
macdef LDBL_MIN_EXP = $extval (int, "LDBL_MIN_EXP")

(* ****** ****** *)

fun{a:t@ype} MAX_10_EXP_tmp ():<> int
macdef DBL_MAX_10_EXP = $extval (int, "DBL_MAX_10_EXP")
macdef FLT_MAX_10_EXP = $extval (int, "FLT_MAX_10_EXP")
macdef LDBL_MAX_10_EXP = $extval (int, "LDBL_MAX_10_EXP")

fun{a:t@ype} MIN_10_EXP_tmp ():<> int
macdef DBL_MIN_10_EXP = $extval (int, "DBL_MIN_10_EXP")
macdef FLT_MIN_10_EXP = $extval (int, "FLT_MIN_10_EXP")
macdef LDBL_MIN_10_EXP = $extval (int, "LDBL_MIN_10_EXP")

(* ****** ****** *)

fun{a:t@ype} MAX_tmp ():<> a
macdef DBL_MAX = $extval (double, "DBL_MAX")
macdef FLT_MAX = $extval (float, "FLT_MAX")
macdef LDBL_MAX = $extval (ldouble, "LDBL_MAX")

fun{a:t@ype} MIN_tmp ():<> a
macdef DBL_MIN = $extval (double, "DBL_MIN")
macdef FLT_MIN = $extval (float, "FLT_MIN")
macdef LDBL_MIN = $extval (ldouble, "LDBL_MIN")

(* ****** ****** *)

fun{a:t@ype} EPSILON_tmp ():<> a
macdef DBL_EPSILON = $extval (double, "DBL_EPSILON")
macdef FLT_EPSILON = $extval (float, "FLT_EPSILON")
macdef LDBL_EPSILON = $extval (ldouble, "LDBL_EPSILON")

(* ****** ****** *)

(* end of [float.sats] *)
