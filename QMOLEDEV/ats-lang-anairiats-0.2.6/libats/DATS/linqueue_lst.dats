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
** A list-based queue implementation
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: July, 2010 // based on a version done in October, 2008
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload "libats/SATS/linqueue_lst.sats"

(* ****** ****** *)

viewtypedef
wptr(a:viewt@ype) = (a, ptr) // with [ptr]

dataview
slseg_v (
  a:viewt@ype+, int, addr, addr
) =
  | {n:nat} {la,lb,lz:addr}
    slseg_v_cons (a, n+1, la, lz) of (
      free_gc_v (wptr(a)?, la), (a, ptr lb) @ la, slseg_v (a, n, lb, lz)
    ) // end of [slseg_v_cons]
  | {la:addr} slseg_v_nil (a, 0, la, la)
// end of [slseg_v]

viewdef slist_v
  (a: viewt@ype, n:int, l:addr) = slseg_v (a, n, l, null)
// end of [slist_v]

(* ****** ****** *)

extern
prfun slseg_v_extend
  {a:viewt@ype}
  {n:nat}
  {la,ly,lz:addr} (
  pf_sl: slseg_v (a, n, la, ly)
, pf_gc: free_gc_v (wptr(a)?, ly)
, pf_at: (a, ptr lz) @ ly
) :<prf> slseg_v (a, n+1, la, lz)
// end of [slseg_v_extend]

implement
slseg_v_extend
  {a} (pf_sl, pf_gc, pf_at) = let
  prfun extend
    {n:nat} {la,ly,lz:addr} .<n>. (
      pf_sl: slseg_v (a, n, la, ly)
    , pf_gc: free_gc_v (wptr(a)?, ly)
    , pf_at: (a, ptr lz) @ ly
    ) :<prf> slseg_v (a, n+1, la, lz) =
    case+ pf_sl of
    | slseg_v_cons (pf1_gc, pf1_at, pf1_sl) => begin
        slseg_v_cons {a} (pf1_gc, pf1_at, slseg_v_extend (pf1_sl, pf_gc, pf_at))
      end // end of [slseg_v_cons]
    | slseg_v_nil () => slseg_v_cons {a} (pf_gc, pf_at, slseg_v_nil ())
  // end of [extend]
in
  extend (pf_sl, pf_gc, pf_at)
end // end of [slseg_v_extend]

(* ****** ****** *)

extern
fun{a:viewt@ype}
slseg_length
  {n:nat} {la,lz:addr} (
  pf_sl: !slseg_v (a, n, la, lz) | p1: ptr la, p2: ptr lz
) :<> size_t (n) // end of [slseg_length]

implement{a}
slseg_length
  {n} {la,lz} (pf_sl | p1, p2) = let
  fun loop
    {la,lz:addr} {n,k:nat} .<n>. (
    pf_sl: !slseg_v (a, n, la, lz)
  | p1: ptr la, p2: ptr lz, k: size_t k
  ) :<> size_t (n+k) =
  if p1 <> p2 then let
    prval slseg_v_cons (pf_gc, pf_at, pf1_sl) = pf_sl
    val res = loop (pf1_sl | p1->1, p2, k+1)
    prval () = pf_sl := slseg_v_cons {a} (pf_gc, pf_at, pf1_sl)
  in
    res
  end else let
    prval () = __assert () where {
      extern prfun __assert (): [n <= 0] void
    } // end of [prval]
  in
    k
  end // end of [if]
in
  loop (pf_sl | p1, p2, 0)
end // end of [slseg_length]

(* ****** ****** *)

absview slseg1_v (
  a:viewt@ype+, n:int, la:addr, lz:addr
) // end of [slseg1_v]

(* ****** ****** *)

extern
prfun slseg1_v_decode0
  {a:viewt@ype} {n:nat} {l2:addr}
  (pf: slseg1_v (a, n, null, l2)):<> [n == 0] void
// end of [slseg1_v_decode0]

extern
prfun slseg1_v_encode0
  {a:viewt@ype} {l2:addr} (): slseg1_v (a, 0, null, l2)
// end of [slseg1_v_encode0]

(* ****** ****** *)

extern
prfun slseg1_v_decode1
  {a:viewt@ype}
  {n:int} {l1,l2:addr | l1 > null}
  (pf: slseg1_v (a, n, l1, l2))
:<> [n > 0] (
  slseg_v (a, n-1, l1, l2)
, free_gc_v (wptr(a)?, l2)
, (a, ptr?) @ l2
) // end of [slseg1_v_decode1]

extern
prfun slseg1_v_encode1
  {a:viewt@ype}
  {n:int} {l1,l2:addr} (
  pf_sl: slseg_v (a, n, l1, l2)
, pf_gc: free_gc_v (wptr(a)?, l2)
, pf_at: (a, ptr?) @ l2
) :<> slseg1_v (a, n+1, l1, l2) // end of [slseg1_v_encode1]

(* ****** ****** *)

viewtypedef
QUEUE_vt (
  a:viewt@ype
, n:int
, l1: addr
, l2: addr
) = $extype_struct
  "atslib_linqueue_lst_QUEUE" of {
  pf= slseg1_v (a, n, l1, l2), ptr1= ptr (l1), ptr2= ptr (l2)
} // end of [QUEUE_vt]

viewtypedef QUEUE0_vt (a:viewt@ype) = QUEUE_vt (a, 0, null, null)?

(* ****** ****** *)

assume
QUEUE (
  a:viewt@ype, n:int
) =
  [l1,l2:addr] QUEUE_vt (a, n, l1, l2)
// end of [QUEUE]

(* ****** ****** *)

implement{a}
queue_size {n} (q) = let
  val p1 = q.ptr1; prval () = ptr_is_gtez (p1)
in
  if (p1 > null) then let
    prval (pf_sl, pf_gc, pf_at) = slseg1_v_decode1 {a} (q.pf)
    val n1 = slseg_length<a> (pf_sl | p1, q.ptr2) // n1 = n-1
    prval () = q.pf := slseg1_v_encode1 {a} (pf_sl, pf_gc, pf_at)
  in
    n1 + 1
  end else let
    stavar l2:addr; prval p2 = q.ptr2 : ptr l2
    prval () = slseg1_v_decode0 {a} {n} (q.pf)
    prval () = q.pf := slseg1_v_encode0 {a} {l2} ()
  in
    size1_of_int1 (0)
  end // end of [if]
end // end of [queue_size]

(* ****** ****** *)

implement
queue_is_empty
  {a} {n} (q) = let
  val p1 = q.ptr1 in
  if p1 > null then let
    prval () = __assert () where { extern prfun __assert (): [n > 0] void }
  in
    false
  end else let
    prval () = __assert () where { extern prfun __assert (): [n <= 0] void }
  in
    true
  end // end of [if]
end // end of [queue_is_empty]

implement
queue_isnot_empty (q) = ~queue_is_empty (q)

(* ****** ****** *)

implement
queue_initialize
  {a} (q) = () where {
  prval () = __assert (q) where {
    extern prfun __assert (q: &QUEUE0 a >> QUEUE0_vt a):<> void
  } // end of [val]
  prval () = q.pf := slseg1_v_encode0 {a} {null} ()
  val () = q.ptr1 := null
  val () = q.ptr2 := null
} // end of [queue_initialize]

(* ****** ****** *)

local

extern
castfn list_vt_of_slist
  {a:viewt@ype} {n:nat} {la:addr}
  (pf: slist_v (a, n, la) | p: ptr la):<> list_vt (a, n)
// end of [list_vt_of_slist]

in // in of [local]

implement{a}
queue_uninitialize (q) = let
  val p1 = q.ptr1; prval () = ptr_is_gtez (p1)
  extern prfun __assert (q: &QUEUE0_vt a >> QUEUE0 a):<> void
in
  if p1 > null then let
    prval (pf_sl, pf_gc, pf_at) = slseg1_v_decode1 (q.pf)
    val p2 = q.ptr2
    prval () = __assert (q)
    val () = p2->1 := null // HX-2011-08-13: bugfix: prval -> val!!!
    prval pf_sl_new = slseg_v_extend {a} (pf_sl, pf_gc, pf_at)
  in
    list_vt_of_slist (pf_sl_new | p1)
  end else let
    prval () = slseg1_v_decode0 (q.pf)
    prval () = __assert (q)
  in
    list_vt_nil ()
  end (* end of [if] *)
end // end of [queue_uninitialize]

end // end of [local]

(* ****** ****** *)

implement{a}
queue_insert (q, x) = let
  viewtypedef VT = wptr(a) and VT0 = @(a, ptr?)
  val (pf_gc_new, pf_at_new | p_new) = ptr_alloc<VT> ()
  val () = p_new->0 := x
  val p1 = q.ptr1; prval () = ptr_is_gtez (p1)
in
  if p1 > null then let
    prval (pf_sl, pf_gc, pf_at) = slseg1_v_decode1 {a} (q.pf)
    val p2 = q.ptr2
    val () = p2->1 := p_new
    prval pf_sl_new = slseg_v_extend {a} (pf_sl, pf_gc, pf_at)
    prval () = q.pf := slseg1_v_encode1 (pf_sl_new, pf_gc_new, pf_at_new)
    val () = q.ptr2 := p_new
  in
    // nothing
  end else let
    prval () = slseg1_v_decode0 {a} (q.pf)
    prval () = q.pf := slseg1_v_encode1 {a} (slseg_v_nil (), pf_gc_new, pf_at_new)
    val () = q.ptr1 := p_new
    val () = q.ptr2 := p_new
  in
    // nothing
  end (* end of [if] *)
end // end of [queue_insert]

(* ****** ****** *)

implement{a}
queue_remove {n} (q) = let
  viewtypedef VT = wptr(a)
  stavar l1:addr; val p1 = q.ptr1 : ptr l1
  stavar l2:addr; val p2 = q.ptr2 : ptr l2
  prval () = __assert () where {
    extern prfun __assert (): [l1 > null] void // HX: since n>0 holds
  } // end of [prval]
  prval (pf_sl, pf_gc, pf_at) = slseg1_v_decode1 (q.pf)
in
  if (p1 <> p2) then let
    prval slseg_v_cons (pf1_gc, pf1_at, pf1_sl) = pf_sl
    prval () = q.pf := slseg1_v_encode1 {a} (pf1_sl, pf_gc, pf_at)
    val () = q.ptr1 := p1->1
    val x = p1->0; val () = ptr_free {VT} (pf1_gc, pf1_at | p1)
  in
    x
  end else let
    prval () = __assert () where {
      extern prfun __assert (): [n <= 1] void
    } // end of [prval]
    prval slseg_v_nil () = pf_sl
    prval () = q.pf := slseg1_v_encode0 {a} {null} ()
    val () = q.ptr1 := null and () = q.ptr2 := null
    val x = p2->0; val () = ptr_free {VT} (pf_gc, pf_at | p2)
  in
    x
  end // end of [if]
end // end of [queue_remove]

(* ****** ****** *)

implement{a}
queue_foreach_funenv
  {v} {vt} {n} (pf | q, f, env) = let
  val p1 = q.ptr1
in
  if p1 > null then let
//
    val p2 = q.ptr2
    prval (
      pf_sl, pf_gc, pf_at
    ) =
      slseg1_v_decode1 {a} (q.pf)
    // end of [prval]
//
    val () = loop (
      pf, pf_sl | p1, p2, f, env
    ) where {
      fun loop {n:nat} {l1,l2:addr} .<n>. (
        pf: !v, pf_sl: !slseg_v (a, n, l1, l2)
      | p1: ptr l1, p2: ptr l2, f: (!v | &a, !vt) -<fun> void, env: !vt
      ) :<> void =
        if (p1 <> p2) then let
          prval slseg_v_cons (pf1_gc, pf1_at, pf1_sl) = pf_sl
          val () = f (pf | p1->0, env)
          val p1_nxt = p1->1
          val () = loop (pf, pf1_sl | p1_nxt, p2, f, env) // HX: tail-call
          prval () = pf_sl := slseg_v_cons {a} (pf1_gc, pf1_at, pf1_sl)
        in
          // nothing
        end // end of [if]
    } // end of [val]
//
    val () = f (pf | p2->0, env)
    prval () = q.pf := slseg1_v_encode1 {a} (pf_sl, pf_gc, pf_at)
//
  in
    // nothing
  end (* end of [if] *)
end // end of [queue_foreach_funenv]

(* ****** ****** *)

implement{a}
queue_foreach_fun
  {n} (q, f) = let
//
  val f = coerce (f) where {
    extern castfn coerce
      (f: (&a) -<fun> void):<> (!unit_v | &a, !ptr) -<fun> void
  } // end of [val]
//
  prval pfu = unit_v ()
  val () = queue_foreach_funenv<a> {unit_v} {ptr} (pfu | q, f, null)
  prval unit_v () = pfu
//
in
  // empty
end // end of [queue_foreach_fun]

(* ****** ****** *)

implement{a}
queue_foreach_vclo
  {v} {n} (pf | q, f) = let
  stavar l_f: addr
  val p_f: ptr l_f = &f
  typedef clo_t = (!v | &a) -<clo> void
  typedef vt = ptr l_f
  viewdef v1 = (v, clo_t @ l_f)
  fn app (pf1: !v1 | x: &a, p_f: !vt):<> void = let
    prval pf11 = pf1.1
    val () = !p_f (pf1.0 | x)
    prval () = pf1.1 := pf11
  in
    // nothing
  end // end of [app]
  prval pf1 = (pf, view@ f)
  val () = queue_foreach_funenv<a> {v1} {vt} (pf1 | q, app, p_f)
  prval () = pf := pf1.0
  prval () = view@ f := pf1.1
in
  // nothing
end // end of [queue_foreach_vclo]

(* ****** ****** *)

(* end of [linqueue_lst.dats] *)
