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
// Time: January, 2011
//
(* ****** ****** *)
//
// HX: reasoning about integer trees
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

staload "libats/SATS/imsetp.sats" // for handling integer multiset

(* ****** ****** *)

datasort itree =
  | itree_nil of () | itree_cons of (int, itree, itree)
// end of [itree]

stadef itree_sing (x:int): itree = itree_cons (x, itree_nil, itree_nil)

(* ****** ****** *)

absprop MSETIZE (itree, imset)

(* ****** ****** *)

dataprop
itreeeq (itree, itree) =
  | itreeeq_nil (
      itree_nil, itree_nil
    ) of ()
  | {x:int} {t1l,t2l:itree} {t1r,t2r:itree}
    itreeeq_cons (
      itree_cons (x, t1l, t1r), itree_cons (x, t2l, t2r)
    ) of (
      itreeeq (t1l, t2l), itreeeq (t1r, t2r)
    ) // end of [itreeeq_cons]
// end of [itreeeq]

(* ****** ****** *)

dataprop ITREEEQ
  (itree, itree) = {t:itree} ITREEEQ (t, t) of ()
// end of [ITREEEQ]

prfun itreeeq_elim
  {t1,t2:itree} (pf: itreeeq (t1, t2)): ITREEEQ (t1, t2)
// end of [itreeeq_elim]

(* ****** ****** *)

dataprop TREESZ (itree, int) =
  | TREESZnil (itree_nil, 0)
  | {x:int} {tl,tr:itree} {nl,nr:nat}
    TREESZcons (
      itree_cons (x, tl, tr), 1 + nl + nr
    ) of (
      TREESZ(tl, nl), TREESZ (tr, nr)
    ) // end of [TREESZcons]
// end of [TREESZ]

(* ****** ****** *)

absprop LTB (x:int, t:itree)

prfun ltb_istot {xs:itree} (): [x:int] LTB (x, xs)

prfun ltb_nil {x:int} (): LTB (x, itree_nil)

prfun ltb_cons {x0:int} {x:int | x0 < x} {xsl,xsr:itree}
  (pfl: LTB (x0, xsl), pfr: LTB (x0, xsr)): LTB (x0, itree_cons (x, xsl, xsr))
// end of [ltb_cons]
prfun ltb_cons_elim {x0:int} {x:int} {xsl,xsr:itree}
  (pf: LTB (x0, itree_cons (x, xsl, xsr))): [x0 < x] (LTB (x0, xsl), LTB (x0, xsr))
// end of [ltb_cons_elim]

prfun ltb_dec {x1:int}
  {x2:int | x2 <= x1} {xs:itree} (pf: LTB (x1, xs)): LTB (x2, xs)
// end of [ltb_dec]

(* ****** ****** *)

absprop LTEB (x:int, t:itree)

prfun lteb_istot {xs:itree} (): [x:int] LTEB (x, xs)

prfun lteb_nil {x:int} (): LTEB (x, itree_nil)

prfun lteb_cons {x0:int} {x:int | x0 <= x} {xsl,xsr:itree}
  (pfl: LTEB (x0, xsl), pfr: LTEB (x0, xsr)): LTEB (x0, itree_cons (x, xsl, xsr))
// end of [lteb_cons]
prfun lteb_cons_elim {x0:int} {x:int} {xsl,xsr:itree}
  (pf: LTEB (x0, itree_cons (x, xsl, xsr))): [x0 <= x] (LTEB (x0, xsl), LTEB (x0, xsr))
// end of [lteb_cons_elim]

prfun lteb_dec {x1:int}
  {x2:int | x2 <= x1} {xs:itree} (pf: LTEB (x1, xs)): LTEB (x2, xs)
// end of [lteb_dec]

(* ****** ****** *)

dataprop ISHEAP (itree) =
  | ISHEAPnil (itree_nil) of ()
  | {x:int} {tl,tr:itree}
    ISHEAPcons (
      itree_cons (x, tl, tr)
    ) of (
      ISHEAP tl, ISHEAP tr, LTEB (x, tl), LTEB (x, tr)
    ) // end of [ISHEAPcons]
// end of [ISHEAP]

(* ****** ****** *)

(* end of [itreep.sats] *)
