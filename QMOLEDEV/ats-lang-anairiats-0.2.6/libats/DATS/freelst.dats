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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** A simple implementation of free-lists (of memory items)
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: August, 2009
**
*)

(* ****** ****** *)

staload "libats/SATS/freelst.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

%{^

ats_ptr_type atslib_freeitm_nxt_get
  (ats_ptr_type x) { return *((ats_ptr_type*)x) ; }
/* end of [freeitm_nxt_get] */

ats_void_type atslib_freeitm_nxt_set
  (ats_ptr_type x, ats_ptr_type p) { *((ats_ptr_type*)x) = p ; return  ; }
/* end of [freeitm_nxt_set] */

%}

(* ****** ****** *)

implement freelst_cons {a}
  {l_at,l} (pf_at, pf | p_at, p) = let
//
extern prfun freeitm_of (pf: !a? @ l_at >> freeitm_t a @ l_at): void
//
  prval () = freeitm_of (pf_at)
  val () = freeitm_nxt_set {a} (!p_at, p)
  prval () = pf := freelst_v_cons {a} (pf_at, pf)
in
  // nothing
end // end of [freelst_cons]

implement freelst_uncons {a} {l} (pf | p) = let
  prval (pf1, pf2) = freelst_v_uncons {a} (pf)
  val p1 = freeitm_nxt_get {a} (!p); prval () = pf := pf2
//
extern prfun of_freeitm (pf: !freeitm_t a @ l >> a? @ l): void
//
  prval () = of_freeitm (pf1)
in
  (pf1 | p1)
end // end of [freelst_uncons]

(* ****** ****** *)

implement{a} freelst_add_bytes
  (pf, pf_arr | p, p_arr, n) =
  freelst_add_bytes_tsz (pf, pf_arr | p, p_arr, n, sizeof<a>)
// end of [freelst_add_bytes]

implement freelst_add_bytes_tsz {a}
  {l} {n} {l_arr} (pf, pf_arr | p, p_arr, n, tsz) = let
//
extern prfun freeitm_of_bytes
  {l0:addr} (pf: b0ytes (sizeof a) @ l0): freeitm_t a @ l0
//
extern prfun bytes_v_split {n,i:nat | i <= n}
  {l:addr} (pf: b0ytes n @ l):<prf> @(b0ytes i @ l, b0ytes (n-i) @ (l+i))
//
in
  if n >= tsz then let
    prval (pf1, pf2_arr) = bytes_v_split {n,sizeof a} (pf_arr)
    prval pf1 = freeitm_of_bytes (pf1)
    val () = freeitm_nxt_set {a} (!p_arr, p)
    prval () = pf := freelst_v_cons (pf1, pf)
  in
    freelst_add_bytes_tsz (pf, pf2_arr | p_arr, p_arr + tsz, n - tsz, tsz)
  end else let
//
    prval () = __leak (pf_arr) where { extern prfun __leak {v:view} (pf: v): void }
//
  in
    p // loop exits
  end // end of [if]
end (* end of [freelst_add_bytes_tsz] *)

(* ****** ****** *)

(* end of [freelst.dats] *)
