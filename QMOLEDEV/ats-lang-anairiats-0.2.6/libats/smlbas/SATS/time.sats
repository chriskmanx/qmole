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
// SML Basis Library: Time (http://www.standardml.org/Basis/time.html)
//

(* ****** ****** *)

staload REAL = "libats/smlbas/SATS/real.sats" 

(* ****** ****** *)

abst@ype time_t0ype = $REAL.real
typedef time = time_t0ype

(* ****** ****** *)

fun toReal (x: time): $REAL.real
fun fromReal (x: $REAL.real): time 

(* ****** ****** *)

fun fprint_time (out: FILEref, x: time): void
overload fprint with fprint_time

(* ****** ****** *)

fun toSeconds (x: time): lint
fun fromSeconds (x: lint): time

fun toMilliseconds (x: time): llint
fun fromMilliseconds (x: llint): time

fun toMicroseconds (x: time): llint
fun fromMicroseconds (x: llint): time

fun toNanoseconds (x: time): llint
fun fromNanoseconds (x: llint): time

(* ****** ****** *)

fun now (): time

(* ****** ****** *)

(* end of [time.sats] *)
