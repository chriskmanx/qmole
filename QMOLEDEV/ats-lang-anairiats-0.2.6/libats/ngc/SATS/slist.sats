(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
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
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)
// Start Time: February, 2011
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

sortdef t0p = t@ype
sortdef vt0p = viewt@ype

(* ****** ****** *)

absview
slnode_v (a:viewt@ype+, la: addr, lb: addr)

prfun
slnode_ptr_is_gtz
  {a:viewt@ype} {la,lb:addr}
  (pf: !slnode_v (a, la, lb)): [la > null] void
// end of [slnode_ptr_is_gtz]

(* ****** ****** *)

typedef
slnode_get_next_type
  (a:viewt@ype) = {la,lb:addr} (
  !slnode_v (a, la, lb) | ptr la
) -<fun> ptr lb // end of [slnode_get_next_type]
fun{a:vt0p} slnode_get_next : slnode_get_next_type (a) // specific

typedef
slnode_set_next_type
  (a:viewt@ype) = {la,lb1:addr} {lb2:addr} (
  !slnode_v (a, la, lb1) >> slnode_v (a, la, lb2) | ptr la, ptr lb2
) -<fun> void // end of [slnode_set_next_type]
fun{a:vt0p} slnode_set_next : slnode_set_next_type (a) // specific

(* ****** ****** *)

prfun
slnode_v_takeout_val
  {a:vt0p} {la,lb:addr}
  (pf: slnode_v (a, la, lb))
  : (a @ la, {a:vt0p} a @ la -<lin,prf> slnode_v (a, la, lb))
// end of [slnode_v_takeout_val]

(* ****** ****** *)

typedef
slnode_alloc_type
  (a:viewt@ype) =
  () -<fun> [la,lb:addr] (
  option_v (slnode_v (a?, la, lb), la > null) | ptr la
) // end of [typedef]
fun{a:vt0p} slnode_alloc : slnode_alloc_type (a) // specific

typedef
slnode_free_type (a:viewt@ype) =
  {la,lb:addr} (slnode_v (a?, la, lb) | ptr la) -<fun> void
fun{a:vt0p} slnode_free : slnode_free_type (a) // specifc

(* ****** ****** *)

dataview
slseg_v (
  a:viewt@ype+, int, addr, addr
) =
  | {n:nat}
    {la,lb,lz:addr | la > null}
    slseg_v_cons (a, n+1, la, lz) of (
      slnode_v (a, la, lb), slseg_v (a, n, lb, lz)
    ) // end of [slseg_v_cons]
  | {la:addr} slseg_v_nil (a, 0, la, la)
// end of [slseg_v]

viewdef slist_v
  (a: viewt@ype, n:int, l:addr) = slseg_v (a, n, l, null)
// end of [slist_v]

(* ****** ****** *)

fun slist_ptr_is_nil
  {a:vt0p} {n:int} {l:addr} (
  pf: !slist_v (a, n, l) | p: ptr l
) :<> bool (n==0) = "atspre_ptr_is_null"

fun slist_ptr_is_cons
  {a:vt0p} {n:int} {l:addr} (
  pf: !slist_v (a, n, l) | p: ptr l
) :<> bool (n > 0) = "atspre_ptr_isnot_null"

(* ****** ****** *)

prfun slseg_v_append
  {a:vt0p} {n1,n2:nat} {la,lm,lz:addr} (
  pf1seg: slseg_v (a, n1, la, lm), pf2seg: slseg_v (a, n2, lm, lz)
) :<prf> slseg_v (a, n1+n2, la, lz) // end of [slseg_v_append]

(* ****** ****** *)

prfun slseg_v_extend
  {a:vt0p} {n:nat} {la,ly,lz:addr} (
  pfseg: slseg_v (a, n, la, ly), pfnod: slnode_v (a, ly, lz)
) :<prf> slseg_v (a, n+1, la, lz)
// end of [slseg_v_extend]

(* ****** ****** *)

fun{a:vt0p}
slist_split_at
  {n:int} {i:nat | i < n} {la:addr}
  (pflst: slist_v (a, n, la) | p: ptr la, i: size_t i)
  : [lm:addr] (slseg_v (a, i, la, lm), slist_v (a, n-i, lm) | ptr lm)
// end of [slist_split_at]

(* ****** ****** *)

absviewtype slist (a:viewt@ype+, n:int)

prfun slist_fold
  {a:vt0p} {n:int} {la:addr}
  (pflst: slist_v (a, n, la) | p: !ptr la >> slist (a, n)): void
// end of [slist_fold]

prfun slist_unfold
  {a:vt0p} {n:int}
  (xs: !slist (a, n) >> ptr la):<> #[la:addr] (slist_v (a, n, la) | void)
// end of [slist_unfold]

castfn slist_encode
  {a:vt0p} {n:int} {la:addr}
  (pflst: slist_v (a, n, la) | p: ptr la):<> slist (a, n)
// end of [slist_encode]

castfn slist_decode
  {a:vt0p} {n:int}
  (xs: slist (a, n)):<> [la:addr] (slist_v (a, n, la) | ptr la)
// end of [slist_decode]

(* ****** ****** *)

fun{a:vt0p}
slist_nil ():<> slist (a, 0)

fun{a:vt0p}
slist_cons {n:nat} {la,lb:addr} (
  pfnod: slnode_v (a, la, lb) | p: ptr la, xs: slist (a, n)
) :<> slist (a, n+1) // end of [slist_cons]

(* ****** ****** *)

fun{a:t0p}
slist_free {n:nat} (xs: slist (a, n)):<> void

(* ****** ****** *)

fun{a:vt0p}
slist_free_funenv
  {v:view} {vt:viewtype} {n:nat} (
  pfv: !v
| xs: slist (a, n), f: (!v | &a >> a?, !vt) -<fun> void, env: !vt
) :<> void // end of [slist_free_funenv]

fun{a:vt0p}
slist_free_fun {n:nat}
  (xs: slist (a, n), f: (&a >> a?) -<fun> void):<> void
// end of [slist_free_fun]

fun{a:vt0p}
slist_free_vclo {v:view} {n:nat}
  (pfv: !v | xs: slist (a, n), f: &(!v | &a >> a?) -<clo> void):<> void
// end of [slist_free_vclo]

(* ****** ****** *)

fun{a:vt0p}
slist_length
  {n:nat} (xs: !slist (a, n)):<> size_t (n)
// end of [slist_length]

(* ****** ****** *)

fun{a:vt0p}
slist_append {m,n:nat}
  (xs: slist (a, m), ys: slist (a, n)):<> slist (a, m+n)
// end of [slist_append]

(* ****** ****** *)

fun{a:vt0p}
slist_reverse {n:nat} (xs: slist (a, n)):<> slist (a, n)

(* ****** ****** *)

fun{a:vt0p}
slist_foreach_funenv
  {v:view} {vt:viewtype} {n:nat} (
  pfv: !v
| xs: !slist (a, n), f: (!v | &a, !vt) -<fun> void, env: !vt
) :<> void // end of [slist_foreach_funenv]

fun{a:vt0p}
slist_foreach_fun
  {n:nat} (
  xs: !slist (a, n), f: (&a) -<fun> void
) :<> void // end of [slist_foreach_fun]

fun{a:vt0p}
slist_foreach_vclo
  {v:view} {n:nat} (
  pfv: !v | xs: !slist (a, n), f: &(!v | &a) -<clo> void
) :<> void // end of [slist_foreach_clo]

(* ****** ****** *)

(* end of [slist.sats] *)
