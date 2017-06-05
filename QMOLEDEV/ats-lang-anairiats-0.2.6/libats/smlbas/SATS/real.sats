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
// SML Basis Library: Real (http://www.standardml.org/Basis/real.html)
//

(* ****** ****** *)

abst@ype real_t0ype = double // implemented as floating point number of double precision
typedef real = real_t0ype

(* ****** ****** *)

castfn real_of_double (x: double): real
castfn double_of_real (x: real): double

(* ****** ****** *)

fun fprint_real (out: FILEref, r: real): void
overload fprint with fprint_real

(* ****** ****** *)

fun add_real_real (r1: real, r2: real): real
overload + with add_real_real

fun sub_real_real (r1: real, r2: real): real
overload - with sub_real_real

fun mul_real_real (r1: real, r2: real): real
overload * with mul_real_real

fun div_real_real (r1: real, r2: real): real
overload / with div_real_real

// this one is name [rem] in [smlbas]
fun mod_real_real (r1: real, r2: real): real
overload mod with mod_real_real

(* ****** ****** *)

fun muladd_real_real (r1: real, r2: real, r3: real): real
fun mulsub_real_real (r1: real, r2: real, r3: real): real

(* ****** ****** *)

fun neg_real (r: real): real
fun abs_real (r: real): real

(* ****** ****** *)

fun lt_real_real (r1: real, r2: real): bool
overload < with lt_real_real

fun lte_real_real (r1: real, r2: real): bool
overload <= with lte_real_real

fun gt_real_real (r1: real, r2: real): bool
overload > with gt_real_real

fun gte_real_real (r1: real, r2: real): bool
overload >= with gte_real_real

fun eq_real_real (r1: real, r2: real): bool
overload = with eq_real_real

fun neq_real_real (r1: real, r2: real): bool
overload <> with neq_real_real

fun compare_real_real (r1: real, r2: real): int

(* ****** ****** *)

fun min_real_real (r1: real, r2: real): real
overload min with min_real_real

fun max_real_real (r1: real, r2: real): real
overload max with max_real_real

(* ****** ****** *)

fun realCeil (r: real): real
fun realFloor (r: real): real
fun realRound (r: real): real
fun realTrunc (r: real): real

(* ****** ****** *)

(* end of [real.sats] *)
