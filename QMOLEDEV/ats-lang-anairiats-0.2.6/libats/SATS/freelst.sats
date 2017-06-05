(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
** A simple implementation of free-lists (of memory items)
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: August, 2009
**
*)

(* ****** ****** *)

(*
** memory for storing a pointer to the next item
*)
sortdef itm = {a:viewt@ype | sizeof a >= sizeof ptr}

(* ****** ****** *)

abst@ype freeitm_t (a:viewt@ype, l:addr)
typedef freeitm_t (a:viewt@ype) = [l:addr] freeitm_t (a, l)

(* ****** ****** *)

fun freeitm_nxt_get {a:itm}
  {l:addr} (x: &freeitm_t (a, l)):<> ptr l // x
  = "atslib_freeitm_nxt_get"

fun freeitm_nxt_set {a:itm} {l1,l2:addr}
  (x: &freeitm_t (a, l1) >> freeitm_t (a, l2), p: ptr l2):<> void // x := p
  = "atslib_freeitm_nxt_set"

(* ****** ****** *)

absview freelst_v (a:viewt@ype, l:addr)

prfun freelst_v_nil {a:viewt@ype} (): freelst_v (a, null)
prfun freelst_v_unnil {a:viewt@ype} (pf: freelst_v (a, null)): void

prfun freelst_v_cons {a:viewt@ype} {l0:addr} {l:addr}
  (pf1: freeitm_t (a, l) @ l0, pf2: freelst_v (a, l)): freelst_v (a, l0)
// end of [extern]

prfun freelst_v_uncons {a:viewt@ype} {l0:addr | l0 <> null}
  (pf: freelst_v (a, l0)): [l:addr] (freeitm_t (a, l) @ l0, freelst_v (a, l))
// end of [extern]

(* ****** ****** *)

fun freelst_cons
  {a:itm} {l_at,l:addr} (
    pf_at: a? @ l_at, pf: !freelst_v (a, l) >> freelst_v (a, l_at)
  | p_at: ptr l_at, p: ptr l
  ) :<> void
// end of [freelst_cons]

fun freelst_uncons 
  {a:itm} {l:addr | l <> null} (
    pf: !freelst_v (a, l) >> freelst_v (a, l_nxt) | p: ptr l)
  :<> #[l_nxt:addr] (a? @ l | ptr l_nxt)
// end of [freelst_uncons]

(* ****** ****** *)

fun{a:itm} freelst_add_bytes
  {l:addr} {n:nat} {l_arr:addr} (
    pf: !freelst_v (a, l) >> freelst_v (a, l), pf_arr: b0ytes n @ l_arr
  | p: ptr l, p_arr: ptr l_arr, n: size_t n
  ) :<> #[l:addr] ptr l
// end of [freelst_add_bytes]

fun freelst_add_bytes_tsz
  {a:itm} {l:addr} {n:nat} {l_arr:addr} (
    pf: !freelst_v (a, l) >> freelst_v (a, l), pf_arr: b0ytes n @ l_arr
  | p: ptr l, p_arr: ptr l_arr, n: size_t n, tsz: sizeof_t a
  ) :<> #[l:addr] ptr l
// end of [freelst_add_bytes_tsz]

(* ****** ****** *)

(* end of [freelst.sats] *)
