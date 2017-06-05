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
// HX: reasoning about integer multisets
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

datasort imset = // abstract

(* ****** ****** *)

sta mnil : imset // for the empty multiset

(* ****** ****** *)

dataprop MSETEQ (imset, imset) = {xs:imset} MSETEQ (xs, xs) of ()

(* ****** ****** *)

absprop MSIZE (imset, int)

prfun msize_istot {xs:imset} (): [n:nat] MSIZE (xs, n)
prfun msize_isfun {xs:imset} {n1,n2:int}
  (pf1: MSIZE (xs, n1), pf2: MSIZE (xs, n2)): [n1==n2] void
// end of [msize_isfun]

prfun msize_isnat
  {xs:imset} {n:int} (pf: MSIZE (xs, n)): [n>=0] void
// end of [MSIZE_isnat]

prfun msize_mnil (): MSIZE (mnil, 0)

(* ****** ****** *)

absprop MCONS (x: int, xs: imset, res: imset)

prfun mcons_istot
  {x:int} {xs:imset} (): [res:imset] MCONS (x, xs, res)
prfun mcons_isfun {x:int} {xs:imset} {res1,res2:imset}
  (pf1: MCONS (x, xs, res1), pf2: MCONS (x, xs, res2)): MSETEQ (res1, res2)
// end of [mcons_isfun]

prfun mcons_msize
  {x:int} {xs:imset} {res:imset} {n:int}
  (pf1: MCONS (x, xs, res), pf1: MSIZE (xs, n)): MSIZE (res, n+1)
// end of [mcons_msize]

prfun mcons_uncons {xs:imset} {n:pos}
  (pf: MSIZE (xs, n)): [x:int;xs1:imset] MCONS (x, xs1, xs)
// end of [mcons_uncons]

(* ****** ****** *)

absprop MUNION (
  xs1: imset, xs2: imset, res: imset
) // end of [MUNION]

prfun munion_istot
  {xs,ys:imset} (): [zs:imset] MUNION (xs, ys, zs)
// end of [munion_istot]

prfun munion_isfun {xs,ys:imset} {zs1,zs2:imset}
  (pf1: MUNION (xs, ys, zs1), pf2: MUNION (xs, ys, zs2)): MSETEQ (zs1, zs2)
// end of [munion_isfun]

prfun munion_unit {xs:imset} (): MUNION (mnil, xs, xs)

prfun munion_commute
  {xs,ys:imset} {zs:imset} (pf: MUNION (xs, ys, zs)): MUNION (ys, xs, zs)
// end of [munion_commute]

prfun munion_associcate
  {m1,m2,m3:imset} {m12,m23:imset} {m12_3,m1_23:imset} (
  pf1: MUNION (m1, m2, m12)
, pf2: MUNION (m2, m3, m23)
, pf3: MUNION (m12, m3, m12_3)
, pf4: MUNION (m1, m23, m1_23)
) : MSETEQ (m12_3, m1_23) // end of [munion_associcate]

(* ****** ****** *)

(* end of [imsetp.sats] *)
