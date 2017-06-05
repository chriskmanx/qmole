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

(* ****** ****** *)

//
// for supporting SIMD on vectors of 2 doubles
//

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no need for static loading at run-time

(* ****** ****** *)

%{#
#include "libc/CATS/SIMD_v2df.cats"
%} // end of [%{#]

(* ****** ****** *)

abst@ype v2df = $extype"ats_v2df_type"

(* ****** ****** *)

val v2df_0_0: v2df = "atslib_v2df_0_0"
val v2df_1_1: v2df = "atslib_v2df_1_1"

(* ****** ****** *)

symintr v2df_make

fun v2df_make_double_double
  (d0: double, d1: double): v2df = "atslib_v2df_make_double_double"
// end of [v2df_make_double_double]
overload v2df_make with v2df_make_double_double

fun v2df_make_int_int
  (i0: int, i1: int): v2df = "atslib_v2df_make_int_int"
// end of [v2df_make_int_int]

//

fun v2df_get_fst (dd: v2df): double = "atslib_v2df_get_fst"
fun v2df_get_snd (dd: v2df): double = "atslib_v2df_get_snd"

//

fun add_v2df_v2df (_: v2df, _: v2df): v2df = "atslib_add_v2df_v2df"
overload + with add_v2df_v2df

fun sub_v2df_v2df (_: v2df, _: v2df): v2df = "atslib_sub_v2df_v2df"
overload - with sub_v2df_v2df

fun mul_v2df_v2df (_: v2df, _: v2df): v2df = "atslib_mul_v2df_v2df"
overload * with mul_v2df_v2df

fun div_v2df_v2df (_: v2df, _: v2df): v2df = "atslib_div_v2df_v2df"
overload / with div_v2df_v2df

(* ****** ****** *)

(* end of [SIMD_v2df.sats] *)
