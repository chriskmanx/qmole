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
// Time: October, 2010
//
(* ****** ****** *)
//
// HX: reasoning about integer sequences
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

staload "libats/SATS/imsetp.sats" // for handling integer multiset

(* ****** ****** *)

datasort ilist =
  | ilist_nil of () | ilist_cons of (int, ilist)
// end of [ilist]

stadef ilist_sing (x:int): ilist = ilist_cons (x, ilist_nil)

(* ****** ****** *)

absprop MSETIZE (ilist, imset)

(* ****** ****** *)

dataprop
ilisteq (ilist, ilist) =
  | ilisteq_nil (
      ilist_nil, ilist_nil
    ) of ()
  | {x:int} {xs1,xs2:ilist}
    ilisteq_cons (
      ilist_cons (x, xs1), ilist_cons (x, xs2)
    ) of (
      ilisteq (xs1, xs2)
    ) // end of [ilisteq_cons]
// end of [ilisteq]

(* ****** ****** *)

dataprop ILISTEQ
  (ilist, ilist) = {xs:ilist} ILISTEQ (xs, xs) of ()
// end of [ILISTEQ]

prfun ilisteq_elim
  {xs1,xs2:ilist} (pf: ilisteq (xs1, xs2)): ILISTEQ (xs1, xs2)
// end of [ilisteq_elim]

(* ****** ****** *)

dataprop
ISEMP (ilist, bool) =
  | ISEMPnil (ilist_nil, true)
  | {x:int} {xs:ilist}
    ISEMPcons (ilist_cons (x, xs), false)
// end of [ISEMP]

(* ****** ****** *)

dataprop
LENGTH (ilist, int) =
  | LENGTHnil (ilist_nil, 0) of ()
  | {x:int} {xs:ilist} {n:nat}
    LENGTHcons (ilist_cons (x, xs), n+1) of LENGTH (xs, n)
// end of [LENGTH]

prfun length_istot {xs:ilist} (): [n:nat] LENGTH (xs, n)
prfun length_isfun {xs:ilist} {n1,n2:int}
  (pf1: LENGTH (xs, n1), pf2: LENGTH (xs, n2)): [n1==n2] void
// end of [length_isfun]

prfun length_isnat
  {xs:ilist} {n:int} (pf: LENGTH (xs, n)): [n>=0] void
// end of [length_isnat]

prfun length_msize {xs:ilist;mxs:imset} {n:int}
  (pf1: MSETIZE (xs, mxs), pf2: LENGTH (xs, n)): MSIZE (mxs, n)
// end of [length_msize]

(* ****** ****** *)

dataprop SNOC (ilist, int, ilist) =
  | {x:int} SNOCnil (ilist_nil, x, ilist_sing (x)) of ()
  | {x0:int} {xs1:ilist} {x:int} {xs2:ilist}
    SNOCcons (ilist_cons (x0, xs1), x, ilist_cons (x0, xs2)) of SNOC (xs1, x, xs2)
// end of [SNOC]

prfun snoc_istot {xs:ilist} {x:int} (): [xsx:ilist] SNOC (xs, x, xsx)
prfun snoc_isfun {xs:ilist} {x:int}
  {xsx1,xsx2:ilist} (pf1: SNOC (xs, x, xsx1), pf2: SNOC (xs, x, xsx2)): ILISTEQ (xsx1, xsx2)
// end of [snoc_isfun]

prfun snoc_length_lemma
  {xs:ilist} {x:int} {xsx:ilist} {n:nat}
  (pf1: SNOC (xs, x, xsx), pf2: LENGTH (xs, n)): LENGTH (xsx, n+1)
// end of [snoc_length_lemma]

(* ****** ****** *)

dataprop APPEND (ilist, ilist, ilist) =
  | {ys:ilist} APPENDnil (ilist_nil, ys, ys) of ()
  | {x:int} {xs:ilist} {ys:ilist} {zs:ilist}
    APPENDcons (ilist_cons (x, xs), ys, ilist_cons (x, zs)) of APPEND (xs, ys, zs)
// end of [APPEND]

prfun append_istot {xs,ys:ilist} (): [zs:ilist] APPEND (xs, ys, zs)
prfun append_isfun {xs,ys:ilist} {zs1,zs2:ilist}
  (pf1: APPEND (xs, ys, zs1), pf2: APPEND (xs, ys, zs2)): ILISTEQ (zs1, zs2)
// end of [append_isfun]

prfun append_unit1 {xs:ilist} (): APPEND (ilist_nil, xs, xs)
prfun append_unit2 {xs:ilist} (): APPEND (xs, ilist_nil, xs)

prfun append_length_lemma
  {xs1,xs2:ilist} {xs:ilist} {n1,n2:int} (
  pf: APPEND (xs1, xs2, xs), pf1len: LENGTH (xs1, n1), pf2len: LENGTH (xs2, n2)
) : LENGTH (xs, n1+n2) // end of [append_length_lemma]

prfun append_snoc_lemma
  {xs1:ilist}
  {x:int}
  {xs2:ilist}
  {xs1x:ilist}
  {xs:ilist} (
  pf1: APPEND (xs1, ilist_cons (x, xs2), xs)
, pf2: SNOC (xs1, x, xs1x)
) : APPEND (xs1x, xs2, xs) // end of [append_snoc_lemma]

(* ****** ****** *)

dataprop REVAPP (ilist, ilist, ilist) =
  | {ys:ilist} REVAPPnil (ilist_nil, ys, ys) of ()
  | {x:int} {xs:ilist} {ys:ilist} {zs:ilist}
    REVAPPcons (ilist_cons (x, xs), ys, zs) of REVAPP (xs, ilist_cons (x, ys), zs)
// end of [REVAPP]

(* ****** ****** *)

dataprop
NTH (x0:int, ilist, int) =
  | {xs:ilist} NTHbas (x0, ilist_cons (x0, xs), 0)
  | {x:int} {xs:ilist} {n:nat}
    NTHind (x0, ilist_cons (x, xs), n+1) of NTH (x0, xs, n)
// end of [NTH]
//
// HX: reverse NTH
//
dataprop
RNTH (x0:int, ilist, int) =
  | {xs:ilist} {n:nat}
    RNTHbas (x0, ilist_cons (x0, xs), n) of LENGTH (xs, n)
  | {x:int} {xs:ilist} {n:nat}
    RNTHind (x0, ilist_cons (x, xs), n) of RNTH (x0, xs, n)
// end of [RNTH]

(* ****** ****** *)

prfun nth_rnth_lemma
  {x:int} {xs:ilist}
  {n:int} {i:nat | i < n}
  (pf1: NTH (x, xs, i), pf2: LENGTH (xs, n)): RNTH (x, xs, n-1-i)
// end of [nth_rnth_lemma]

prfun rnth_nth_lemma
  {x:int} {xs:ilist}
  {n:int} {i:nat | i < n}
  (pf1: RNTH (x, xs, i), pf2: LENGTH (xs, n)): NTH (x, xs, n-1-i)
// end of [rnth_nth_lemma]

(* ****** ****** *)

stadef b2i = int_of_bool

(* ****** ****** *)

dataprop
MSETCNT (x0:int, ilist, int) =
  | MSETCNTnil (x0, ilist_nil, 0) of ()
  | {x:int} {xs:ilist} {n:nat}
    MSETCNTcons (x0, ilist_cons (x, xs), n+b2i(x0==x)) of MSETCNT (x0, xs, n)
// end of [MSETCNT]

prfun msetcnt_istot
  {x0:int} {xs:ilist} (): [n:nat] MSETCNT (x0, xs, n)
prfun msetcnt_isfun
  {x0:int} {xs:ilist} {n1,n2:int} (
    pf1: MSETCNT (x0, xs, n1), pf2: MSETCNT (x0, xs, n2)
  ) : [n1==n2] void
// end of [msetcnt_isfun]
prfun msetcnt_first
  {x:int} {xs:ilist} (): [n:pos] MSETCNT (x, ilist_cons (x, xs), n)
// end of [msetcnt_first]

(* ****** ****** *)

prfun nth_msetcnt_lemma
  {x:int} {xs:ilist} {i:nat} (pf: NTH (x, xs, i)): [n:pos] MSETCNT (x, xs, n)
// end of [nth_msetcnt_lemma]
prfun msetcnt_nth_lemma
  {x:int} {xs:ilist} {n:pos} (pf: MSETCNT (x, xs, n)): [i:nat] NTH (x, xs, i)
// end of [msetcnt_nth_lemma]

(* ****** ****** *)

dataprop
INSERT (
  x0:int, ilist, int, ilist
) = // INSERT (x0, xs, i, ys): insert x0 in xs at i = ys
  | {xs:ilist}
    INSERTbas (
      x0, xs, 0, ilist_cons (x0, xs)
    ) of () // end of [INSERTbas]
  | {x:int} {xs:ilist} {i:nat} {ys:ilist}
    INSERTind (
      x0, ilist_cons (x, xs), i+1, ilist_cons (x, ys)
    ) of INSERT (x0, xs, i, ys) // end of [INSERTind]
// end of [INSERT]

prfun insert_length_lemma
  {x0:int} {xs:ilist} {i:int} {ys:ilist} {n:nat}
  (pf1: INSERT (x0, xs, i, ys), pf2: LENGTH (xs, n)): LENGTH (ys, n+1)
// end of [insert_length_lemma]

prfun nth_insert_lemma
  {x:int} {xs:ilist} {n:nat}
  (pf: NTH (x, xs, n)): [ys:ilist] INSERT (x, ys, n, xs)
// end of [nth_insert_lemma]

(* ****** ****** *)

propdef
PERMUTE (xs1:ilist, xs2:ilist) =
  {x0:int} {n:nat} MSETCNT (x0, xs1, n) -<prf> MSETCNT (x0, xs2, n)
// end of [PERMUTE]

prfun permute_refl {xs:ilist} (): PERMUTE (xs, xs)
prfun permute_symm
  {xs1,xs2:ilist} (pf: PERMUTE (xs1, xs2)): PERMUTE (xs2, xs1)
prfun permute_trans {xs1,xs2,xs3:ilist}
  (pf1: PERMUTE (xs1, xs2), pf2: PERMUTE (xs2, xs3)): PERMUTE (xs1, xs3)

prfun permute_insert_lemma
  {x:int} {xs:ilist} {ys:ilist}
  (pf: PERMUTE (ilist_cons (x, xs), ys)): [ys1:ilist;i:nat] INSERT (x, ys1, i, ys)
// end of [permute_insert_lemma]

prfun permute_length_lemma
  {xs1,xs2:ilist} {n:nat}
  (pf1: PERMUTE (xs1, xs2), pf2: LENGTH (xs1, n)): LENGTH (xs2, n)
// end of [permute_length_lemma]

prfun permute_seteq_intr
  {xs1,xs2:ilist} {mxs:imset}
  (pf1: MSETIZE (xs1, mxs), pf2: MSETIZE (xs2, mxs)): PERMUTE (xs1, xs2)
// end of [permute_seteq]

prfun permute_seteq_elim
  {xs1:ilist;mxs1:imset}
  {xs2:ilist;mxs2:imset} (
  pf: PERMUTE (xs1, xs2), pf1: MSETIZE (xs1, mxs1), pf2: MSETIZE (xs2, mxs2)
) : MSETEQ (mxs1, mxs2) // end of [permute_mseteq]

(* ****** ****** *)

(*
//
// HX-2010-12-27: see [imsetp.dats]
//
propdef
MUNION (xs1:ilist, xs2:ilist, xs3:ilist) =
  {x0:int} {n1,n2:nat}
  (MSETCNT (x0, xs1, n1), MSETCNT (x0, xs2, n2)) -<prf> MSETCNT (x0, xs3, n1+n2)
// end of [MUNION]

prfun append_munion_lemma
  {xs,ys,zs:ilist} (pf: APPEND (xs,ys,zs)): MUNION (xs, ys, zs)
// end of [append_munion_lemma]
*)

(* ****** ****** *)

(*
//
// HX-2010-12-27: see [imsetp.dats]
//
propdef
MSUBSET (xs1:ilist, xs2:ilist) =
  {x0:int} {n1,n2:nat}
  (MSETCNT (x0, xs1, n1), MSETCNT (x0, xs1, n2)) -<prf> [n1 <= n2] void
// end of [MSUBSET]
*)

(* ****** ****** *)

(*
dataprop
MSETALL (P: int -> prop, ilist) =
  | MSETALLnil (P, ilist_nil) of ()
  | {x:int} {xs:ilist}
    MSETALLcons (P, ilist_cons (x, xs)) of (P x, MSETALL (P, xs))
// end of [MSETALL]

prfun msetall_trans
  {P1:int->prop} {P2:int->prop} {xs:ilist}
  (pf: MSETALL (P1, xs), fpf: {x:int} P1 x -<prf> P2 x): MSETALL (P2, xs)
// end of [msetall_trans]
*)

(* ****** ****** *)

absprop LTB
  (x: int, xs: ilist) // [x] is a strict lower bound for [xs]
// end of [LTB]

prfun ltb_istot {xs:ilist} (): [x:int] LTB (x, xs)

prfun ltb_nil {x:int} (): LTB (x, ilist_nil)

prfun ltb_cons {x0:int}
  {x:int | x0 < x} {xs:ilist} (pf: LTB (x0, xs)): LTB (x0, ilist_cons (x, xs))
// end of [ltb_cons]
prfun ltb_cons_elim {x0:int}
  {x:int} {xs:ilist} (pf: LTB (x0, ilist_cons (x, xs))): [x0 < x] LTB (x0, xs)
// end of [ltb_cons_elim]

prfun ltb_dec {x1:int}
  {x2:int | x2 <= x1} {xs:ilist} (pf: LTB (x1, xs)): LTB (x2, xs)
// end of [ltb_dec]

prfun ltb_permute_lemma {x:int}
  {xs1,xs2:ilist} (pf1: LTB (x, xs1), pf2: PERMUTE (xs1, xs2)): LTB (x, xs2)
// end of [ltb_permute_lemma]

(* ****** ****** *)

absprop LTEB
  (x: int, xs: ilist) // [x] is a lower bound for [xs]
// end of [LTEB]

prfun lteb_istot {xs:ilist} (): [x:int] LTEB (x, xs)

prfun lteb_nil {x:int} (): LTEB (x, ilist_nil)

prfun lteb_cons {x0:int}
  {x:int | x0 <= x} {xs:ilist} (pf: LTEB (x0, xs)): LTEB (x0, ilist_cons (x, xs))
// end of [lteb_cons]
prfun lteb_cons_elim {x0:int}
  {x:int} {xs:ilist} (pf: LTEB (x0, ilist_cons (x, xs))): [x0 <= x] LTEB (x0, xs)
// end of [lteb_cons_elim]

prfun lteb_dec {x1:int}
  {x2:int | x2 <= x1} {xs:ilist} (pf: LTEB (x1, xs)): LTEB (x2, xs)
// end of [lteb_dec]

prfun lteb_permute_lemma {x:int}
  {xs1,xs2:ilist} (pf1: LTEB (x, xs1), pf2: PERMUTE (xs1, xs2)): LTEB (x, xs2)
// end of [lteb_permute_lemma]

(* ****** ****** *)

dataprop ISORD (ilist) =
  | ISORDnil (ilist_nil) of ()
  | {x:int} {xs:ilist}
    ISORDcons (ilist_cons (x, xs)) of (ISORD xs, LTEB (x, xs))
// end of [ISORD]

(* ****** ****** *)
//
// SORT (xs, ys):
// [ys] is a sorted version of [xs]
//
absprop SORT (xs: ilist, ys: ilist)

prfun sort_elim {xs,ys:ilist}
  (pf: SORT (xs, ys)): @(PERMUTE (xs, ys), ISORD ys)
prfun sort_make {xs,ys:ilist}
  (pf1: PERMUTE (xs, ys), pf2: ISORD ys): SORT (xs, ys)

(* ****** ****** *)

(* end of [ilistp.sats] *)
