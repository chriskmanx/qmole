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

#define ATS_DYNLOADFLAG 0 // there is no need for dynloading at run-time

(* ****** ****** *)

staload "libc/SATS/complex.sats"

(* ****** ****** *)

%{^

// ccmplx = ats_fcomplex_type
// zcmplx = ats_dcomplex_type

ats_fcomplex_type
atslib_ccmplx_imag_unit = _Complex_I ;

ats_dcomplex_type
atslib_zcmplx_imag_unit = _Complex_I ;

//
// print functions
//
ats_void_type
atslib_fprint_ccmplx
  (ats_ptr_type out, ats_fcomplex_type c) {
  int n ;
  float c_i = cimagf(c) ;

  if (c_i >= 0.0) {
    n = fprintf((FILE *)out, "%f+i*%f", crealf(c), c_i) ;
  } else {
    n = fprintf((FILE *)out, "%f-i*%f", crealf(c), -c_i) ;
  }
  return ;
} // end of [ats_fprint_ccmplx]

ats_void_type
atslib_fprint_zcmplx
  (ats_ptr_type out, ats_dcomplex_type z) {
  int n ;
  double z_i = cimag(z) ;

  if (z_i >= 0.0) {
    n = fprintf((FILE *)out, "%f+i*%f", creal(z), z_i) ;
  } else {
    n = fprintf((FILE *)out, "%f-i*%f", creal(z), -z_i) ;
  }
  return ;
} // end of [ats_fprint_zcmplx]

%} // end of [{%{^]

(* ****** ****** *)

implement print_ccmplx (c) = print_mac (fprint_ccmplx, c)
implement prerr_ccmplx (c) = prerr_mac (fprint_ccmplx, c)

implement print_zcmplx (z) = print_mac (fprint_zcmplx, z)
implement prerr_zcmplx (z) = prerr_mac (fprint_zcmplx, z)

(* ****** ****** *)

(* end of [complex.dats] *)
