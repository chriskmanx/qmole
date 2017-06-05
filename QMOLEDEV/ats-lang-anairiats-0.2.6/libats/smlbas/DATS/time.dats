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
macdef r2d (x) = $REAL.double_of_real ,(x)
macdef d2r (x) = $REAL.real_of_double ,(x)

(* ****** ****** *)

staload "libats/smlbas/SATS/time.sats" 

(* ****** ****** *)

assume time_t0ype = $REAL.real

(* ****** ****** *)

implement toReal (x) = x
implement fromReal (x) = x

(* ****** ****** *)

implement fprint_time (out, x) = $REAL.fprint_real (out, x)

(* ****** ****** *)

implement toSeconds (x) = lint_of_double (r2d (x))
implement fromSeconds (x) = d2r (double_of_lint (x))

implement toMilliseconds (x) = llint_of_double (1E3 * r2d (x))
implement fromMilliseconds (x) = d2r (1E-3 * double_of_llint (x))

implement toMicroseconds (x) = llint_of_double (1E6 * r2d (x))
implement fromMicroseconds (x) = d2r (1E-6 * double_of_llint (x))

implement toNanoseconds (x) = llint_of_double (1E9 * r2d (x))
implement fromNanoseconds (x) = d2r (1E-9 * double_of_llint (x))

(* ****** ****** *)

local

staload TIME = "libc/SATS/time.sats"

in // in of [local]

implement now () = d2r ($TIME.double_of_time ($TIME.time_get ()))

end // end of [local]

(* ****** ****** *)

(* end of [time.dats] *)
