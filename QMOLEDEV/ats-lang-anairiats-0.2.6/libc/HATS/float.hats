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
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
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

staload "libc/SATS/float.sats"

(* ****** ****** *)

implement DIG_tmp<double> () = DBL_DIG
implement DIG_tmp<float> () = FLT_DIG
implement DIG_tmp<ldouble> () = LDBL_DIG

implement MANT_DIG_tmp<double> () = DBL_MANT_DIG
implement MANT_DIG_tmp<float> () = FLT_MANT_DIG
implement MANT_DIG_tmp<ldouble> () = LDBL_MANT_DIG

(* ****** ****** *)

implement RADIX_tmp<double> () = DBL_RADIX
implement RADIX_tmp<float> () = FLT_RADIX
implement RADIX_tmp<ldouble> () = LDBL_RADIX

(* ****** ****** *)

implement MAX_EXP_tmp<double> () = DBL_MAX_EXP
implement MAX_EXP_tmp<float> () = FLT_MAX_EXP
implement MAX_EXP_tmp<ldouble> () = LDBL_MAX_EXP

implement MIN_EXP_tmp<double> () = DBL_MIN_EXP
implement MIN_EXP_tmp<float> () = FLT_MIN_EXP
implement MIN_EXP_tmp<ldouble> () = LDBL_MIN_EXP

(* ****** ****** *)

implement MAX_10_EXP_tmp<double> () = DBL_MAX_10_EXP
implement MAX_10_EXP_tmp<float> () = FLT_MAX_10_EXP
implement MAX_10_EXP_tmp<ldouble> () = LDBL_MAX_10_EXP

implement MIN_10_EXP_tmp<double> () = DBL_MIN_10_EXP
implement MIN_10_EXP_tmp<float> () = FLT_MIN_10_EXP
implement MIN_10_EXP_tmp<ldouble> () = LDBL_MIN_10_EXP

(* ****** ****** *)

implement MAX_tmp<double> () = DBL_MAX
implement MAX_tmp<float> () = FLT_MAX
implement MAX_tmp<ldouble> () = LDBL_MAX

implement MIN_tmp<double> () = DBL_MIN
implement MIN_tmp<float> () = FLT_MIN
implement MIN_tmp<ldouble> () = LDBL_MIN

(* ****** ****** *)

implement EPSILON_tmp<double> () = DBL_EPSILON
implement EPSILON_tmp<float> () = FLT_EPSILON
implement EPSILON_tmp<ldouble> () = LDBL_EPSILON

(* ****** ****** *)

(* end of [float.hats] *)
