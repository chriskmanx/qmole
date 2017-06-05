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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

(*
//
// HX-2010-10-14: this accurately describes [ptrarr]:
//
dataview ptrarr_v (n:int, l:addr) =
  | {l:addr} ptrarr_v_nil (0, l) of ptr(null) @ l
  | {l:addr} ptrarr_v_cons (n+1, l) of (Ptr1 @ l, ptrarr_v (n, l+sizeof(ptr)))
// end of [ptrarr_v]
*)
abst@ype ptrarr (n:int) = array (ptr, n)

praxi ptrarr_takeout {vt:viewtype}
  {n:nat} {l:addr} (pf: ptrarr(n) @ l): (
  array_v (vt, n, l), array_v (vt, n, l) -<lin,prf> ptrarr(n) @ l
) // end of [ptrarr_takeout]

(* ****** ****** *)

fun ptrarr_size {n:nat}
  (x: &ptrarr(n)): size_t(n) = "atspre_ptrarr_size"
// end of [ptrarr_size]

(* ****** ****** *)

(* end of [ptrarr.sats] *)
