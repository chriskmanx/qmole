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

%{^
extern double round (double) ;
extern double trunc (double) ;
%}
staload MATH = "libc/SATS/math.sats"

(* ****** ****** *)

staload "libats/smlbas/SATS/real.sats"

(* ****** ****** *)

// please note that this does not really
assume real_t0ype = double // implement the specification given in [smlbas].

(* ****** ****** *)

implement real_of_double (x) = x // cast function
implement double_of_real (x) = x // cast function

(* ****** ****** *)

implement fprint_real (out, x) = fprint_double (out, x)

(* ****** ****** *)

implement add_real_real (r1, r2) = add_double_double (r1, r2)
implement sub_real_real (r1, r2) = sub_double_double (r1, r2)
implement mul_real_real (r1, r2) = mul_double_double (r1, r2)
implement div_real_real (r1, r2) = div_double_double (r1, r2)
implement mod_real_real (r1, r2) = $MATH.fmod (r1, r2)

(* ****** ****** *)

implement muladd_real_real (r1, r2, r3) =
  add_double_double (mul_double_double (r1, r2), r3)

implement mulsub_real_real (r1, r2, r3) =
  sub_double_double (mul_double_double (r1, r2), r3)

(* ****** ****** *)

implement neg_real (r) = neg_double (r)
implement abs_real (r) = abs_double (r)

(* ****** ****** *)

implement lt_real_real (r1, r2) = lt_double_double (r1, r2)
implement lte_real_real (r1, r2) = lte_double_double (r1, r2)
implement gt_real_real (r1, r2) = gt_double_double (r1, r2)
implement gte_real_real (r1, r2) = gte_double_double (r1, r2)

implement eq_real_real (r1, r2) = eq_double_double (r1, r2)
implement neq_real_real (r1, r2) = neq_double_double (r1, r2)

implement compare_real_real (r1, r2) = compare_double_double (r1, r2)

(* ****** ****** *)

implement min_real_real (r1, r2) = min_double_double (r1, r2)
implement max_real_real (r1, r2) = max_double_double (r1, r2)

(* ****** ****** *)

implement realCeil (r) = $MATH.ceil (r)
implement realFloor (r) = $MATH.floor (r)
implement realRound (r) = $MATH.round (r)
implement realTrunc (r) = $MATH.trunc (r)

(* ****** ****** *)

(* end of [real.dats] *)
