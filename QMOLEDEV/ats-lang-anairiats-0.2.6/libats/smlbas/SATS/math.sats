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
// SML Basis Library: Math (http://www.standardml.org/Basis/math.html)
//

(* ****** ****** *)

staload REAL = "libats/smlbas/SATS/real.sats"
typedef real = $REAL.real

(* ****** ****** *)

val pi : real // = 3.1415926535897932384626 
val e  : real // = 2.718281828

fun sqrt (x: real): real

fun sin (x: real): real
fun cos (x: real): real
fun tan (x: real): real

fun asin (x: real): real
fun acos (x: real): real
fun atan (x: real): real
fun atan2 (y: real, x: real): real

(* ****** ****** *)

fun exp (x: real): real
fun pow (x: real, y: real): real

(* ****** ****** *)

fun ln (x: real): real
fun log10 (x: real): real

(* ****** ****** *)

fun sinh (x: real): real
fun cosh (x: real): real
fun tanh (x: real): real

(* ****** ****** *)

(* end of [math.sats] *)
