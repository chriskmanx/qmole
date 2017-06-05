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
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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

staload "libc/SATS/gmp.sats"

(* ****** ****** *)

%{

ats_void_type
atslib_mpz_inp_str_exn (
  ats_mpz_ptr_type x
, ats_ptr_type file
, ats_int_type base
) {
  size_t n = mpz_inp_str(x, (FILE*)file, base) ;
  if (n == 0) {
    ats_exit_errmsg (1, "exit(ATS): [mpz_inp_str] failed.\n") ;
  } // end of [if]
  return ;
} // end of [atslib_mpz_inp_str_exn]

ats_void_type
atslib_mpz_out_str_exn (
  ats_ptr_type file
, ats_int_type base
, const ats_mpz_ptr_type x
) {
  size_t n = mpz_out_str((FILE*)file, base, (mpz_ptr)x) ;
  if (n == 0) {
    ats_exit_errmsg (1, "exit(ATS): [mpz_out_str] failed.\n") ;
  } // end of [if]
  return ;
} // end of [atslib_mpz_out_str_exn]

%} // end of [%{]

implement print_mpz (x) = print_mac (fprint1_mpz, x)
implement prerr_mpz (x) = prerr_mac (fprint1_mpz, x)

implement tostrptr_mpz (x) = mpz_get_str (10, x)
implement tostring_mpz (x) = let
  val str = mpz_get_str (10, x) in string_of_strptr (str)
end // end of [tostring_mpz]

(* ****** ****** *)

implement
mpq_incby (x, p, q) = let
  val (pf_xp, fpf_xp | p_xp) = mpq_numref (x)
  val (pf_xq, fpf_xq | p_xq) = mpq_denref (x)
  val () = mpz_mul (!p_xp, q)
  val () = mpz_addmul (!p_xp, !p_xq, p)
  val () = mpz_mul (!p_xq, q)
  prval () = fpf_xp (pf_xp)
  prval () = fpf_xq (pf_xq)
in
  mpq_canonicalize (x)
end // end of [mpq_incby]

implement
mpq_decby (x, p, q) = let
  val (pf_xp, fpf_xp | p_xp) = mpq_numref (x)
  val (pf_xq, fpf_xq | p_xq) = mpq_denref (x)
  val () = mpz_mul (!p_xp, q)
  val () = mpz_submul (!p_xp, !p_xq, p)
  val () = mpz_mul (!p_xq, q)
  prval () = fpf_xp (pf_xp)
  prval () = fpf_xq (pf_xq)
in
  mpq_canonicalize (x)
end // end of [mpq_decby]

(* ****** ****** *)

implement
mpq_pow3_ui (res, x, n) = let
//
  val (pf_resp, fpf_resp | p_resp) = mpq_numref (res)
  val (pf_xp, fpf_xp | p_xp) = mpq_numref (x)
  val () = mpz_pow_ui (!p_resp, !p_xp, n)
  prval () = fpf_xp (pf_xp)
  prval () = fpf_resp (pf_resp)
//
  val (pf_resq, fpf_resq | p_resq) = mpq_denref (res)
  val (pf_xq, fpf_xq | p_xq) = mpq_denref (x)
  val () = mpz_pow_ui (!p_resq, !p_xq, n)
  prval () = fpf_xq (pf_xq)
  prval () = fpf_resq (pf_resq)
//
in
  // no need for cannonicalizing
end // end of [mpq_pow3_ui]

implement mpq_pow2_ui (x, n) = let
//
  val (pf_xp, fpf_xp | p_xp) = mpq_numref (x)
  val () = mpz_pow_ui (!p_xp, n)
  prval () = fpf_xp (pf_xp)
//
  val (pf_xq, fpf_xq | p_xq) = mpq_denref (x)
  val () = mpz_pow_ui (!p_xq, n)
  prval () = fpf_xq (pf_xq)
//
in
  // no need for cannonicalizing
end // end of [mpq_pow2_ui]

(* ****** ****** *)

%{

ats_void_type
atslib_mpq_inp_str_exn (
  ats_mpq_ptr_type x
, ats_ptr_type file
, ats_int_type base
) {
  size_t n = mpq_inp_str(x, (FILE*)file, base) ;
  if (n == 0) {
    ats_exit_errmsg (1, "exit(ATS): [mpq_inp_str] failed.\n") ;
  } // end of [if]
  return ;
} // end of [atslib_mpq_inp_str_exn]

ats_void_type
atslib_mpq_out_str_exn (
  ats_ptr_type file
, ats_int_type base
, const ats_mpq_ptr_type x
) {
  size_t n = mpq_out_str((FILE*)file, base, (mpq_ptr)x) ;
  if (n == 0) {
    ats_exit_errmsg (1, "exit(ATS): [mpq_out_str] failed.\n") ;
  } // end of [if]
  return ;
} // end of [atslib_mpq_out_str_exn]

%} // end of [%{]

implement print_mpq (x) = print_mac (fprint1_mpq, x)
implement prerr_mpq (x) = prerr_mac (fprint1_mpq, x)

(* ****** ****** *)

%{

ats_void_type
atslib_mpf_inp_str_exn (
  ats_mpf_ptr_type x
, ats_ptr_type file
, ats_int_type base
) {
  size_t n = mpf_inp_str(x, (FILE*)file, base) ;
  if (n == 0) {
    ats_exit_errmsg (1, "exit(ATS): [mpf_inp_str] failed.\n") ;
  } // end of [if]
  return ;
} // end of [atslib_mpf_inp_str_exn]

ats_void_type
atslib_mpf_out_str_exn (
  ats_ptr_type file
, ats_int_type base
, ats_size_type ndigit
, const ats_mpf_ptr_type x
) {
  size_t n = mpf_out_str((FILE*)file, base, ndigit, (mpf_ptr)x) ;
  if (n == 0) {
    ats_exit_errmsg (1, "exit(ATS): [mpf_out_str] failed.\n") ;
  } // end of [if]
  return ;
} // end of [atslib_mpf_out_str_exn]

%} // end of [%{]

(* ****** ****** *)

(* end of [gmp.dats] *)
