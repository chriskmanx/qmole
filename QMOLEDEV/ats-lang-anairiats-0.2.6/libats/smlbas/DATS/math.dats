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

staload LM = "libc/SATS/math.sats"

(* ****** ****** *)

staload "libats/smlbas/SATS/real.sats"
staload "libats/smlbas/SATS/math.sats"

(* ****** ****** *)

#define d2r real_of_double
#define r2d double_of_real

(* ****** ****** *)

implement e = d2r ($LM.M_E) // = 2.7182818284590452354
implement pi = d2r ($LM.M_PI) // = 3.14159265358979323846

(* ****** ****** *)

implement sqrt (x) = d2r ($LM.sqrt (r2d x))

(* ****** ****** *)

implement sin (x) = d2r ($LM.sin (r2d x))
implement cos (x) = d2r ($LM.cos (r2d x))
implement tan (x) = d2r ($LM.tan (r2d x))

(* ****** ****** *)

implement asin (x) = d2r ($LM.asin (r2d x))
implement acos (x) = d2r ($LM.acos (r2d x))
implement atan (x) = d2r ($LM.atan (r2d x))
implement atan2 (y, x) = d2r ($LM.atan2 (r2d y, r2d x))

(* ****** ****** *)

implement exp (x) = d2r ($LM.exp (r2d x))
implement pow (x, y) = d2r ($LM.pow (r2d x, r2d y))

(* ****** ****** *)

implement ln (x) = d2r ($LM.log (r2d x))
implement log10 (x) = d2r ($LM.log10 (r2d x))

(* ****** ****** *)

implement sinh (x) = d2r ($LM.sinh (r2d x))
implement cosh (x) = d2r ($LM.cosh (r2d x))
implement tanh (x) = d2r ($LM.tanh (r2d x))

(* ****** ****** *)

(* end of [math.dats] *)
