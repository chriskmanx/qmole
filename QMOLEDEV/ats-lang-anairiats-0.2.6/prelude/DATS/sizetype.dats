(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no initialization is needed

(* ****** ****** *)

staload "prelude/SATS/sizetype.sats"

(* ****** ****** *)

implement
divmod_size1_size1
  {m,n} (m, n, r) = let
//
  prval () = size_param_lemma (m)
  prval () = size_param_lemma (n)
//
  val (pfdiv_mn | q) = div2_size1_size1 (m, n)
  val (pfmul_qn | qn) = mul2_size1_size1 (q, n)
//
  prval pfdivmod_mn = divmod_istot {m,n} ()
  prval () = divmod_isfun (pfdiv_mn, pfdivmod_mn)
  prval pfmul_qn_alt = divmod_elim (pfdivmod_mn)
//
  prval () = mul_isfun (pfmul_qn, pfmul_qn_alt)
//
  val () = r := m - qn
in
  (pfdivmod_mn | q)
end // end of [quotrem_size1_size1]

(* ****** ****** *)

(* end of [sizetype.dats] *)
