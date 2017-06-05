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
// Start Time: March, 2011
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

sortdef t0p = t@ype
sortdef vt0p = viewt@ype

(* ****** ****** *)

absview
dlnode_v (
  a:viewt@ype+
, l: addr, lp: addr, ln: addr // lp: previous; ln: next
) // end of [dlnode_v]

prfun
dlnode_ptr_is_gtz
  {a:vt0p} {l,lp,ln:addr}
  (pf: !dlnode_v (a, l, lp, ln)): [l > null] void
// end of [dlnode_ptr_is_gtz]

(* ****** ****** *)

typedef
dlnode_get_prev_type
  (a:viewt@ype) = {l,lp,ln:addr} (
  !dlnode_v (a, l, lp, ln) | ptr l
) -<fun> ptr lp // end of [dlnode_get_prev_type]
fun{a:vt0p} dlnode_get_prev : dlnode_get_prev_type (a) // specific template

typedef
dlnode_set_prev_type
  (a:viewt@ype) = {l,lp1,ln:addr} {lp2:addr} (
  !dlnode_v (a, l, lp1, ln) >> dlnode_v (a, l, lp2, ln) | ptr l, ptr lp2
) -<fun> void // end of [dlnode_set_prev_type]
fun{a:vt0p} dlnode_set_prev : dlnode_set_prev_type (a) // specific template

(* ****** ****** *)

typedef
dlnode_get_next_type
  (a:viewt@ype) = {l,lp,ln:addr} (
  !dlnode_v (a, l, lp, ln) | ptr l
) -<fun> ptr ln // end of [dlnode_get_next_type]
fun{a:vt0p} dlnode_get_next : dlnode_get_next_type (a) // specific template

typedef
dlnode_set_next_type
  (a:viewt@ype) = {l,lp,ln1:addr} {ln2:addr} (
  !dlnode_v (a, l, lp, ln1) >> dlnode_v (a, l, lp, ln2) | ptr l, ptr ln2
) -<fun> void // end of [dlnode_set_next_type]
fun{a:vt0p} dlnode_set_next : dlnode_set_next_type (a) // specific template

(* ****** ****** *)

prfun
dlnode_v_takeout_val
  {a:vt0p} {l,lp,ln:addr}
  (pf: dlnode_v (a, l, lp, ln))
  : (a @ l, {a:vt0p} a @ l -<lin,prf> dlnode_v (a, l, lp, ln))
// end of [dlnode_v_takeout_val]

(* ****** ****** *)

typedef
dlnode_alloc_type
  (a:viewt@ype) =
  () -<fun> [l,lp,ln:addr] (
  option_v (dlnode_v (a?, l, lp, ln), l > null) | ptr l
) // end of [typedef]
fun{a:vt0p} dlnode_alloc : dlnode_alloc_type (a) // specific template

typedef
dlnode_free_type (a:viewt@ype) =
  {l,lp,ln:addr} (dlnode_v (a?, l, lp, ln) | ptr l) -<fun> void
fun{a:vt0p} dlnode_free : dlnode_free_type (a) // specifc template

(* ****** ****** *)

dataview
dlseg_v (
  a:viewt@ype+, int, addr, addr, addr, addr
) =
  | {n:nat}
    {lf,lfp:addr}
    {lr,lrn:addr}
    {lfn:addr}
    dlseg_v_cons (a, n+1, lf, lfp, lr, lrn) of (
      dlnode_v (a, lf, lfp, lfn), dlseg_v (a, n, lfn, lf, lr, lrn)
    ) // end of [dlseg_v_cons]
  | {lf:addr}
    {lr:addr}
    dlseg_v_nil (a, 0, lf, lr, lr, lf) of ()
// end of [dlseg_v]

dataview
rdlseg_v (
  a:viewt@ype+, int, addr, addr, addr, addr
) =
  | {n:nat}
    {lf,lfp:addr}
    {lr,lrn:addr}
    {lrp:addr}
    rdlseg_v_cons (a, n+1, lf, lfp, lr, lrn) of (
      rdlseg_v (a, n, lf, lfp, lrp, lr), dlnode_v (a, lr, lrp, lrn)
    ) // end of [dlseg_v_cons]
  | {lf:addr}
    {lr:addr}
    rdlseg_v_nil (a, 0, lf, lr, lr, lf) of ()
// end of [rdlseg_v]

(* ****** ****** *)

dataview
dlist_v (
  a:viewt@ype, int, int, lm:addr
) =
  | {nf:nat;nr:pos}
    {lf,lmp,lr:addr}
    dlist_v_cons (a, nf, nr, lm) of (
      rdlseg_v (a, nf, lf, null, lmp, lm), dlseg_v (a, nr, lm, lmp, lr, null)
    ) // end of [dlist_v_cons]
  | dlist_v_nil (a, 0, 0, null) of ()
// end of [dlist_v]

(* ****** ****** *)

fun dlist_ptr_is_nil
  {a:vt0p} {nf,nr:int} {lm:addr} (
  pf: !dlist_v (a, nf, nr, lm) | p: ptr lm
) :<> bool (nr==0) = "atspre_ptr_is_null"

fun dlist_ptr_is_cons
  {a:vt0p} {nf,nr:int} {lm:addr} (
  pf: !dlist_v (a, nf, nr, lm) | p: ptr lm
) :<> bool (nr > 0) = "atspre_ptr_isnot_null"

(* ****** ****** *)

absviewtype dlist (a:viewt@ype+, nf:int, nr: int) = ptr

prfun
dlist_length_is_nonnegative
  {a:vt0p} {nf,nr:int}
  (xs: !dlist (a, nf, nr)): [nf>=0;nr>=0] void
// end of [dlist_length_is_nonnegative]

prfun dlist_fold
  {a:vt0p} {nf,nr:int} {lm:addr}
  (pflst: dlist_v (a, nf, nr, lm) | p: !ptr lm >> dlist (a, nf, nr)): void
// end of [dlist_fold]

prfun dlist_unfold
  {a:vt0p} {nf,nr:int}
  (xs: !dlist (a, nf, nr) >> ptr lm):<> #[lm:addr] (dlist_v (a, nf, nr, lm) | void)
// end of [dlist_unfold]

castfn dlist_encode
  {a:vt0p} {nf,nr:int} {lm:addr}
  (pflst: dlist_v (a, nf, nr, lm) | p: ptr lm):<> dlist (a, nf, nr)
// end of [dlist_encode]

castfn dlist_decode
  {a:vt0p} {nf,nr:int}
  (xs: dlist (a, nf, nr)):<> [lm:addr] (dlist_v (a, nf, nr, lm) | ptr lm)
// end of [dlist_decode]

(* ****** ****** *)

fun dlist_is_nil
  {a:vt0p} {nf,nr:int} (
  xs: !dlist (a, nf, nr)
) :<> bool (nr==0) = "atspre_ptr_is_null"

fun dlist_is_cons
  {a:vt0p} {nf,nr:int} (
  xs: !dlist (a, nf, nr)
) :<> bool (nr > 0) = "atspre_ptr_isnot_null"

(* ****** ****** *)

fun{a:vt0p}
dlist_is_at_end
  {nf,nr:int | nr > 0} (
  xs: !dlist (a, nf, nr)
) :<> bool (nr <= 1)
// end of [dlist_is_at_end]

fun{a:vt0p}
dlist_isnot_at_end
  {nf,nr:int | nr > 0} (
  xs: !dlist (a, nf, nr)
) :<> bool (nr >= 2)
// end of [dlist_isnot_at_end]

fun{a:vt0p}
dlist_is_at_beg
  {nf,nr:int | nr > 0} (
  xs: !dlist (a, nf, nr)
) :<> bool (nf <= 0)
// end of [dlist_is_at_beg]

fun{a:vt0p}
dlist_isnot_at_beg
  {nf,nr:int | nr > 0} (
  xs: !dlist (a, nf, nr)
) :<> bool (nf >= 1)
// end of [dlist_isnot_at_beg]

(* ****** ****** *)

fun{a:vt0p}
dlist_nil ():<> dlist (a, 0, 0)

(* ****** ****** *)

fun{a:vt0p}
dlist_sing
  {l,lp,ln:addr} (
  pfnod: dlnode_v (a, l, lp, ln)
| p: ptr l
) :<> dlist (a, 0, 1) // end of [dlist_sing]

castfn
dlist_unsing
  {a:vt0p} (
  xs: dlist (a, 0, 1)
) : [l:addr] (
  dlnode_v (a, l, null, null) | ptr l
) // end of [dlist_unsing]

(* ****** ****** *)

fun{a:vt0p}
dlist_cons
  {nr:int}
  {l1,lp,ln:addr} (
  pfnod: dlnode_v (a, l1, lp, ln)
| p1: ptr l1, xs: dlist (a, 0, nr)
) : dlist (a, 0, nr+1)

fun{a:vt0p}
dlist_uncons
  {nr:pos} (
  xs: &dlist (a, 0, nr) >> dlist (a, 0, nr-1)
) : [l,lp,ln:addr] (
  dlnode_v (a, l, lp, ln) | ptr l
) // end of [dlist_uncons]

(* ******** ******* *)

fun{a:vt0p}
dlist_insert_after
  {nf,nr:int | nr > 0}
  {l1,lp,ln:addr} (
  pfnod: dlnode_v (a, l1, lp, ln)
| p1: ptr l1
, xs: !dlist (a, nf, nr) >> dlist (a, nf, nr+1)
) :<> void // end of [dlist_insert_after]

fun{a:vt0p}
dlist_insert_before
  {nf,nr:int | nr > 0}
  {l1,lp,ln:addr} (
  pfnod: dlnode_v (a, l1, lp, ln)
| p1: ptr l1
, xs: !dlist (a, nf, nr) >> dlist (a, nf+1, nr)
) :<> void // end of [dlist_insert_before]

(* ******** ******* *)

fun{a:vt0p}
dlist_move_forward
  {nf,nr:int | nr >= 2}
  (xs: dlist (a, nf, nr)):<> dlist (a, nf+1, nr-1)
// end of [dlist_move_forward]

fun{a:vt0p}
dlist_move_backward
  {nf,nr:int | nf > 0}
  (xs: dlist (a, nf, nr)):<> dlist (a, nf-1, nr+1)
// end of [dlist_move_backward]

(* ******** ******* *)

fun{a:vt0p}
dlist_remove
 {nf,nr:int | nr >= 2} (
 xs: &dlist (a, nf, nr) >> dlist (a, nf, nr-1)
) :<> [l1,lp,ln:addr] (
  dlnode_v (a, l1, lp, ln) | ptr l1
) // end of [dlist_remove]

fun{a:vt0p}
dlist_remove_after
 {nf,nr:int | nr >= 2} (
 xs: !dlist (a, nf, nr) >> dlist (a, nf, nr-1)
) :<> [l1,lp,ln:addr] (
  dlnode_v (a, l1, lp, ln) | ptr l1
) // end of [dlist_remove_after]

fun{a:vt0p}
dlist_remove_before
 {nf,nr:int | nf > 0} (
 xs: !dlist (a, nf, nr) >> dlist (a, nf-1, nr)
) :<> [l1,lp,ln:addr] (
  dlnode_v (a, l1, lp, ln) | ptr l1
) // end of [dlist_remove_before]

(* ******** ******* *)

fun{a:t0p}
dlist_free {nf,nr:int} (
  xs: dlist (a, nf, nr)
) :<> void // end of [dlist_free]

fun{a:vt0p}
dlist_free_funenv
  {v:view} {vt:viewtype} {nf,nr:int} (
  pfv: !v
| xs: dlist (a, nf, nr), f: (!v | &a >> a?, !vt) -<fun> void
, env: !vt
) :<> void // end of [dlist_free_funenv]

fun{a:vt0p}
dlist_free_fun {nf,nr:int} (
  xs: dlist (a, nf, nr), f: (&a >> a?) -<fun> void
) :<> void // end of [dlist_free_fun]

fun{a:vt0p}
dlist_free_vclo {v:view} {nf,nr:int} (
  pfv: !v | xs: dlist (a, nf, nr), f: &(!v | &a >> a?) -<clo> void
) :<> void // end of [dlist_free_vclo]

(* ******** ******* *)

fun{a:vt0p}
dlist_appfst_funenv
  {v:view}
  {vt:viewtype}
  {nf,nr:int | nr > 0} (
  pfv: !v
| xs: !dlist (a, nf, nr), f: (!v | &a, !vt) -<fun> void
, env: !vt
) :<> void // end of [dlist_appfst_funenv]

fun{a:vt0p}
dlist_appfst_fun
  {nf,nr:int | nr > 0} (
  xs: !dlist (a, nf, nr), f: (&a) -<fun> void
) :<> void // end of [dlist_appfst_fun]

fun{a:vt0p}
dlist_appfst_vclo
  {v:view} {nf,nr:int | nr > 0} (
  pfv: !v | xs: !dlist (a, nf, nr), f: &(!v | &a) -<clo> void
) :<> void // end of [dlist_appfst_vclo]

(* ******** ******* *)

(* end of [dlist.sats] *)
