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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: December, 2010
//
(* ****** ****** *)
//
// HX: fully indexed generic arrays
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

staload "libats/SATS/ilistp.sats" // for handling integer sequences

(* ****** ****** *)

absviewt@ype
elt_v0type_int (a:viewt@ype, x:int) = a
stadef elt = elt_v0type_int
stadef elt (a:viewt@ype) = [x:int] elt (a, x)

prfun eltencode {a:viewt@ype}
  (x: &a >> elt (a, x)): #[x:int] void
prfun eltdecode
  {a:viewt@ype} {x:int} (x: &elt (a, x) >> a): void

(* ****** ****** *)

dataview gfarray_v
  (a:viewt@ype, ilist, addr) =
  | {x:int} {xs:ilist} {l:addr}
    gfarray_v_cons (a, ilist_cons (x, xs), l) of
      (elt (a, x) @ l, gfarray_v (a, xs, l+sizeof(a)))
  | {l:addr} gfarray_v_nil (a, ilist_nil, l) of ()
// end of [gfarray_v]

(* ****** ****** *)

prfun array_of_gfarray
  {a:viewt@ype} {xs:ilist} {l:addr}
  (pf: !gfarray_v (a, xs, l) >> array_v (a, n, l)): #[n:nat] LENGTH (xs, n)
// end of [array_of_gfarray]

prfun gfarray_of_array
  {a:viewt@ype} {n:int} {l:addr}
  (pf: !array_v (a, n, l) >> gfarray_v (a, xs, l)): #[xs:ilist] LENGTH (xs, n)
// end of [gfarray_of_array]

(* ****** ****** *)

prfun gfarray_v_split {a:viewt@ype} 
  {xs:ilist} {n,i:nat | i <= n} {l:addr} {ofs:int} (
  pflen: LENGTH (xs, n)
, pfmul: MUL (i, sizeof a, ofs)
, pfarr: gfarray_v (a, xs, l)
) :<prf> [xs1,xs2:ilist] (
  LENGTH (xs1, i), APPEND (xs1, xs2, xs), gfarray_v (a, xs1, l), gfarray_v (a, xs2, l+ofs)
) // end of [gfarray_v_split]

prfun gfarray_v_unsplit {a:viewt@ype}
  {xs1,xs2:ilist} {n1:int} {l:addr} {ofs:int} (
  pflen: LENGTH (xs1, n1)
, pfmul: MUL (n1, sizeof a, ofs)
, pfarr1: gfarray_v (a, xs1, l)
, pfarr2: gfarray_v (a, xs2, l+ofs)
) :<prf> [xs:ilist] (APPEND (xs1, xs2, xs), gfarray_v (a, xs, l))
// end of [gfarray_v_unsplit]

(* ****** ****** *)

prfun gfarray_v_extend {a:viewt@ype}
  {xs:ilist} {x:int} {xsx:ilist} {n:nat} {l:addr} {ofs:int} (
  pfsnoc: SNOC (xs, x, xsx)
, pflen: LENGTH (xs, n)
, pfmul: MUL (n, sizeof a, ofs)
, pfarr: gfarray_v (a, xs, l)
, pfat: a @ l+ofs
) : gfarray_v (a, xsx, l)
// end of [gfarray_v_extend]

prfun gfarray_v_unextend {a:viewt@ype}
  {xs:ilist} {n:int | n > 0} {l:addr} {ofs:int} (
  pflen: LENGTH (xs, n)
, pfmul: MUL (n, sizeof a, ofs)
, pfarr: gfarray_v (a, xs, l)
) :<prf> [xsf:ilist;x:int] ( // xsf: the front
  SNOC (xsf, x, xs), gfarray_v (a, xsf, l), elt (a, x) @ l+ofs-sizeof a
) // end of [gfarray_v_unextend]

(* ****** ****** *)

(* end of [gfarray.sats] *)
