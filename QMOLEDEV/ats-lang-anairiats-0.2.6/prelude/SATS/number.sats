(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
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

(*
**
** An interface for various common funtion on numbers
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

(* ****** ****** *)

// this is originally done for the ATS/CBLAS package

(* ****** ****** *)
//
// legend:
// S: single precision float (float)
// D: double precision float (double)
// C: single precision complex (ccmplx)
// Z: double precision complex (zcmplx)
//
(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} print_typ (): void
fun{a:t@ype} print_elt (x: a): void

(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} of_int (x: int):<> a

(* ****** ****** *)

// S, D
fun{a:t@ype} of_size (x: size_t):<> a

(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} of_double (x: double):<> a

(* ****** ****** *)

// S, D
fun{a:t@ype} to_int (x:a):<> int
fun{a:t@ype} to_float (x: a):<> float
fun{a:t@ype} to_double (x: a):<> double

(* ****** ****** *)

// S, D, C, Z
fun{a1,a2:t@ype} abs (x: a1):<> a2

(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} neg (x: a):<> a

(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} add (x1: a, x2: a):<> a
fun{a:t@ype} sub (x1: a, x2: a):<> a
fun{a:t@ype} mul (x1: a, x2: a):<> a
fun{a:t@ype} div (x1: a, x2: a):<> a

(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} pow (base: a, exp: a):<> a

(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} sqrt (x: a):<> a

(* ****** ****** *)

// S, D
fun{a:t@ype} ceil (x:a) :<> a
fun{a:t@ype} floor (x:a) :<> a

(* ****** ****** *)

// (S, S), (D, D)
// (S, C), (C, C), (D, Z), (Z, Z)
fun{a1,a2:t@ype} scal (x1: a1, x2: a2):<> a2

(* ****** ****** *)

// S, D
fun{a:t@ype} lt (x1: a, x2: a):<> bool
fun{a:t@ype} lte (x1: a, x2: a):<> bool
fun{a:t@ype} gt (x1: a, x2: a):<> bool
fun{a:t@ype} gte (x1: a, x2: a):<> bool

(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} eq (x1: a, x2: a):<> bool
fun{a:t@ype} neq (x1: a, x2: a):<> bool

(* ****** ****** *)

// S, D
fun{a:t@ype} signof (x: a):<> Sgn
fun{a:t@ype} compare (x1: a, x2: a):<> Sgn

(* ****** ****** *)

// S, D
fun{a:t@ype} min (x:a, y:a):<> a
fun{a:t@ype} max (x:a, y:a):<> a

(* ****** ****** *)

// (S, C), (D, Z)
// a1 = |a2|
fun{a1,a2:t@ype} cmplx_make_cart (real: a1, imag: a1):<> a2

(* ****** ****** *)

// (S, C), (D, Z)

fun {a1,a2:t@ype} creal (x: a2):<> a1
fun {a1,a2:t@ype} cimag (x: a2):<> a1

(* ****** ****** *)

// C, Z
fun{a:t@ype} conj (x: a):<> a

(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} sin (x: a):<> a
fun{a:t@ype} cos (x: a):<> a
fun{a:t@ype} tan (x: a):<> a

(* ****** ****** *)

// S, D, C, Z
fun{a:t@ype} asin (x: a):<> a
fun{a:t@ype} acos (x: a):<> a
fun{a:t@ype} atan (x: a):<> a
fun{a:t@ype} atan2 (x1: a, x2: a):<> a

(* ****** ****** *)

(* end of [number.sats] *)
