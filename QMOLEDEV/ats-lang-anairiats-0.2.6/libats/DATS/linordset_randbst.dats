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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** A ordered set implementation
** based on randomized binary search trees
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: October, 2011
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynloading at run-time

(* ****** ****** *)

staload "libats/SATS/linordset_randbst.sats"

(* ****** ****** *)

%{^

ats_void_type
atslib_linordset_rngobj_free
  (ats_ptr_type obj) {
  ((linordset_rngobj_struct*)obj)->free (obj) ; return ;
} // end of [atslib_linordset_rngobj_free]

ats_double_type
atslib_linordset_rngobj_eval
  (ats_ptr_type obj) {
  return ((linordset_rngobj_struct*)obj)->get_double (obj) ;
} // end of [atslib_linordset_rngobj_eval]

ats_void_type
atslib_linordset_rngobj_setseed
  (ats_ptr_type obj, ats_ulint_type seed) {
  ((linordset_rngobj_struct*)obj)->setseed (obj, seed) ; return ;
} // end of [atslib_linordset_rngobj_setseed]

%} // end of [%{^]

(* ****** ****** *)

extern
fun linordset_rngobj_eval // linordset_rngobj_randbst.cats
  (obj: !rngobj):<> double = "atslib_linordset_rngobj_eval"
// end of [linordset_rngobj_eval]

(* ****** ****** *)

(*
** 0 if r <= m/(m+n); 1 if r > m/(m+n)
*)
fn{} randchoose_m_n
  {m,n:nat} (
  obj: !rngobj, m: int m, n: int n
) :<> natLt (2) = let
  val r = linordset_rngobj_eval (obj)
in
  if (m+n) * r <= (double_of)m then 0 else 1
end // end of [randchoose_m_n]

(* ****** ****** *)
//
// HX: a dataviewtype for binary search trees
//
dataviewtype
bstree (a:t@ype+, int(*size*)) =
  | {nl,nr:nat} BSTcons (a, 1+nl+nr) of
      (int (1+nl+nr), a, bstree (a, nl), bstree (a, nr))
  | BSTnil (a, 0) of ()
// end of [bstree]

(* ****** ****** *)

assume set_t0ype_type (a:t@ype) = [n:nat] bstree (a, n)

(* ****** ****** *)

fn{a:t@ype}
bstree_size {n:nat} (
  t: !bstree (a, n)
) :<> int n = case+ t of
  | BSTcons (n, _, _, _) => (fold@ t; n) | BSTnil _ => (fold@ t; 0) 
// end of [bstree_size]

implement{a}
linordset_size (xs) = size_of_int1 (bstree_size (xs))

(* ****** ****** *)

fun{a:t@ype}
bstree_height
  {n:nat} .<n>. (
  t: !bstree (a, n)
) :<> size_t =
  case+ t of
  | BSTcons (
      _, _, !p_tl, !p_tr
    ) => let
      val nl = bstree_height (!p_tl)
      and nr = bstree_height (!p_tr)
      prval () = fold@ (t)
    in
      (size_of_int1)1 + (if nl >= nr then nl else nr)
    end // end of [BSTcons]
  | BSTnil _ => (fold@ t; (size_of_int1)0) 
// end of [bstree_height]

implement{a} linordset_height (xs) = bstree_height (xs)

(* ****** ****** *)

implement{} linordset_make_nil () = BSTnil ()

implement{a}
linordset_make_sing (x) = BSTcons (1, x, BSTnil, BSTnil)

(* ****** ****** *)

fn{a:t@ype}
bstree_is_nil {n:nat}
  (t: !bstree (a, n)):<> bool (n == 0) =
  case+ t of
  | BSTcons _ => (fold@ t; false) | BSTnil _ => (fold@ t; true)
// end of [bstree_is_nil]

fn{a:t@ype}
bstree_is_cons {n:nat}
  (t: !bstree (a, n)):<> bool (n > 0) =
  case+ t of
  | BSTcons _ => (fold@ t; true) | BSTnil _ => (fold@ t; false)
// end of [bstree_is_cons]

implement{}
linordset_is_empty {a} (xs) = bstree_is_nil<a> (xs)
implement{}
linordset_isnot_empty {a} (xs) = bstree_is_cons<a> (xs)

(* ****** ****** *)

fun{a:t@ype}
bstree_free
  {n:nat} .<n>.
  (t: bstree (a, n)):<> void =
  case+ t of
  | ~BSTcons (_, _, tl, tr) => (bstree_free tl; bstree_free tr)
  | ~BSTnil () => ()
// end of [bstree_free]

implement{a} linordset_free (xs) = bstree_free<a> (xs)

(* ****** ****** *)

fun{a:t@ype}
bstree_search
  {n:nat} .<n>. (
  t: !bstree (a, n), x0: a, cmp: cmp(a)
) :<> bool = begin case+ t of
  | BSTcons (_, x, !p_tl, !p_tr) => let
      val sgn = compare_elt_elt (x0, x, cmp)
    in
      if sgn < 0 then let
        val ans = bstree_search (!p_tl, x0, cmp) in fold@ t; ans
      end else if sgn > 0 then let
        val ans = bstree_search (!p_tr, x0, cmp) in fold@ t; ans
      end else ( // sgn = 0
        fold@ t; true
      ) // end of [if]
    end // end of [BSTcons]
  | BSTnil () => let
      prval () = fold@ (t) in false
    end // end of [BSTnil]
end (* end of [bstree_search] *)

implement{a}
linordset_is_member
  (xs, x0, cmp) = bstree_search<a> (xs, x0, cmp)
// end of [linordset_is_member]

implement{a}
linordset_isnot_member
  (xs, x0, cmp) = ~bstree_search<a> (xs, x0, cmp)
// end of [linordset_isnot_member]

(* ****** ****** *)

fun{a:t@ype}
bstree_ordsrch
  {d:nat} {n:nat} .<n>. (
  d: int d
, t: !bstree (a, n), x0: a, cmp: cmp(a)
) :<> int = begin case+ t of
  | BSTcons (n, x, !p_tl, !p_tr) => let
      val sgn = compare_elt_elt (x0, x, cmp)
    in
      if sgn < 0 then let
        val ans = bstree_ordsrch (d, !p_tl, x0, cmp) in fold@ t; ans
      end else if sgn > 0 then let
        val nr = bstree_size (!p_tr)
        val ans = bstree_ordsrch (d+n-nr, !p_tr, x0, cmp) in fold@ t; ans
      end else let // sgn = 0
        val nl = bstree_size (!p_tl) in fold@ t; d+nl
      end // end of [if]
    end // end of [BSTcons]
  | BSTnil () => let
      prval () = fold@ (t) in ~1 (*notfound*)
    end // end of [BSTnil]
end (* end of [bstree_ordsrch] *)

implement{a}
linordset_get_order
  (xs, x0, cmp) = bstree_ordsrch (0, xs, x0, cmp)
// end of [linordset_get_order]

(*
implement{a}
linordset_is_member
  (xs, x0, cmp) = linordset_get_order<a> (xs, x0, cmp) >= 0
// end of [linordset_is_member]
*)

(* ****** ****** *)

fun{a:t@ype}
bstree_insert_atroot
  {n:nat} .<n>. (
  t: &bstree (a, n) >> bstree (a, n+i)
, x0: a, cmp: cmp a
) :<> #[i:two] int (i) = begin
  case+ t of
  | BSTcons (
      !p_n, x, !p_tl, !p_tr
    ) => let
      val sgn = compare_elt_elt (x0, x, cmp)
    in
      if sgn < 0 then let
        val ans = bstree_insert_atroot<a> (!p_tl, x0, cmp)
      in
        if ans > 0 then let
          val tl_new = !p_tl
          val+ BSTcons (!p_nl, _(*xl*), !p_tll, !p_tlr) = tl_new
          val n = !p_n; val nll = bstree_size !p_tll
          val () = !p_tl := !p_tlr
          val () = !p_n := n - nll
          prval () = fold@ (t)
          val () = !p_tlr := t
          val () = !p_nl := n + 1
          prval () = fold@ (tl_new)
          val () = t := tl_new
        in
          ans
        end else begin
          fold@ t; ans // [x0] is alreay in the tree [t]
        end // end of [if]
      end else if sgn > 0 then let
        val ans = bstree_insert_atroot<a> (!p_tr, x0, cmp)
      in
        if ans > 0 then let
          val tr_new = !p_tr
          val+ BSTcons (!p_nr, _(*xr*), !p_trl, !p_trr) = tr_new
          val n = !p_n; val nrr = bstree_size !p_trr
          val () = !p_tr := !p_trl
          val () = !p_n := n - nrr
          prval () = fold@ t
          val () = !p_trl := t
          val () = !p_nr := n + 1
          prval () = fold@ (tr_new)
          val () = t := tr_new
        in
          ans
        end else begin
          fold@ t; ans // [x0] is alreay in the tree [t]
        end // end of [if]
      end else begin (* sgn = 0 *)
        fold@ t; 0 // [x0] is the root of [t]
      end // end of [if]
    end (* end of [BSTcons] *)
  | ~BSTnil () => let
      val () = t := BSTcons (1, x0, BSTnil (), BSTnil ()) in 1(*inserted*)
    end // end of [BSTnil]
end (* end of [bstree_insert_atroot] *)

(* ****** ****** *)

fun{a:t@ype}
bstree_insert_random
  {n:nat} .<n>. (
  obj: !rngobj
, t: &bstree (a, n) >> bstree (a, n+i)
, x0: a, cmp: cmp a
) :<> #[i:two] int (i) = begin
  case+ t of
  | BSTcons (!p_n, x, !p_tl, !p_tr) =>
    if randchoose_m_n (obj, 1, !p_n) = 0 then let
      prval () = fold@ (t) in bstree_insert_atroot<a> (t, x0, cmp)
    end else let
      val sgn = compare_elt_elt (x0, x, cmp)
    in
      if sgn < 0 then let
        val ans = bstree_insert_random<a> (obj, !p_tl, x0, cmp) in
        if ans > 0 then (!p_n := !p_n + 1; fold@ t; ans) else (fold@ t; ans)
      end else if sgn > 0 then let
        val ans = bstree_insert_random<a> (obj, !p_tr, x0, cmp) in
        if ans > 0 then (!p_n := !p_n + 1; fold@ t; ans) else (fold@ t; ans)
      end else begin (* sgn = 0 *)
        fold@ t; 0(* [x0] is the root of [t] *)
      end // end of [if]
    end (* end of [BSTcons] *)
  | ~BSTnil () => let
      val () = t := BSTcons (1, x0, BSTnil (), BSTnil ()) in 1
    end (* end of [BSTnil] *)
end (* end of [bstree_insert_random] *)

implement{a}
linordset_insert (obj, xs, x0, cmp) = let
  val ans = bstree_insert_random (obj, xs, x0, cmp) in ans > 0
end // end of [linordset_insert]

(* ****** ****** *)

fun{a:t@ype}
bstree_ordget
  {n,d:nat | d < n} .<n>. (
  t: !bstree (a, n), d: int d
) :<> a = let
  val+ BSTcons
    (_, x, !p_tl, !p_tr) = t
  val nl = bstree_size (!p_tl)
in
  if d < nl then let
    val res = bstree_ordget<a> (!p_tl, d) in fold@ {a} (t); res
  end else if d > nl then let
    val res = bstree_ordget<a> (!p_tr, d-nl-1) in fold@ {a} (t); res
  end else (fold@ {a} (t); x)
end (* end of [bstree_ordget] *)

implement{a}
linordset_ordget
  (xs, d, x0) = let
  val n = bstree_size (xs)
in
  if d < n then let
    val () = x0 := bstree_ordget<a> (xs, d)
    prval () = opt_some {a} (x0) in true
  end else let
    prval () = opt_none {a} (x0) in false
  end // end of [if]
end // end of [linordset_ordget]

(* ****** ****** *)

fun{a:t@ype}
bstree_join_random
  {nl,nr:nat} .<nl+nr>. (
  obj: !rngobj
, tl: bstree (a, nl)
, tr: bstree (a, nr)
) :<> bstree (a, nl+nr) = begin case+ tl of
  | BSTcons (
      !p_nl, _(*xl*), !p_tll, !p_tlr
    ) => begin
    case+ tr of
    | BSTcons (
        !p_nr, _(*xr*), !p_trl, !p_trr
      ) => let
        val nl = !p_nl and nr = !p_nr
        val n = nl + nr
      in
        if randchoose_m_n (obj, nl, nr) = 0 then let
          prval () = fold@ tr
          val () = !p_tlr := bstree_join_random (obj, !p_tlr, tr)
          val () = !p_nl := n
          prval () = fold@ (tl)
        in
          tl
        end else let
          prval () = fold@ tl
          val () = !p_trl := bstree_join_random (obj, tl, !p_trl)
          val () = !p_nr := n
          prval () = fold@ (tr)
        in
          tr
        end // end of [if]
      end (* end of [BSTcons] *)
    | ~BSTnil () => (fold@ tl; tl)
    end (* end of [BSTcons] *)
  | ~BSTnil () => tr
end // end of [bstree_join_random]

(* ****** ****** *)

fun{a:t@ype}
bstree_ordrem
  {n,d:nat | d < n} .<n>. (
  obj: !rngobj
, t: &bstree (a, n) >> bstree (a, n-1), d: int d
) :<> a = let
  val+ BSTcons
    (!p_n, x, !p_tl, !p_tr) = t
  val nl = bstree_size (!p_tl)
in
  if d < nl then let
    val res = bstree_ordrem<a> (obj, !p_tl, d)
    val () = !p_n := !p_n - 1
    prval () = fold@ {a} (t)
  in
    res
  end else if d > nl then let
    val res = bstree_ordrem<a> (obj, !p_tr, d-nl-1)
    val () = !p_n := !p_n - 1
    prval () = fold@ {a} (t)
  in
    res
  end else let
    val t_new = bstree_join_random<a> (obj, !p_tl, !p_tr)
    val () = free@ {a} {0,0} (t)
    val () = t := t_new
  in
    x (* removed *)
  end // end of [if]
end (* end of [bstree_ordrem] *)

implement{a}
linordset_ordrem
  (obj, xs, d, x0) = let
  val n = bstree_size (xs)
in
  if d < n then let
    val () = x0 :=
      bstree_ordrem<a> (obj, xs, d)
    prval () = opt_some {a} (x0)
  in
    true
  end else let
    prval () = opt_none {a} (x0) in false
  end (* end of [if] *)
end // end of [linordset_ordrem]

(* ****** ****** *)

fun{a:t@ype}
bstree_remove_random
  {n:nat} .<n>. (
  obj: !rngobj
, t: &bstree (a, n) >> bstree (a, n-i)
, x0: a, cmp: cmp a
) :<> #[i:two | i <= n] int (i) = begin
  case+ t of
  | BSTcons {..} {nl,nr}
      (!p_n, x, !p_tl, !p_tr) => let
      val sgn = compare_elt_elt (x0, x, cmp)
    in
      if sgn < 0 then let
        val ans = bstree_remove_random (obj, !p_tl, x0, cmp)
        val () = !p_n := !p_n - ans
        prval () = fold@ (t)
      in
        ans
      end else if sgn > 0 then let
        val ans = bstree_remove_random (obj, !p_tr, x0, cmp)
        val () = !p_n := !p_n - ans
        prval () = fold@ (t)
      in
        ans
      end else let
        val t_new = bstree_join_random<a> (obj, !p_tl, !p_tr)
        val () = free@ {a} {0,0} (t)
        val () = t := t_new
      in
        1 (* removed *)
      end // end of [0]
    end (* end of [BSTcons] *)
  | BSTnil () => (fold@ t; 0)
end // end of [bstree_remove_random]

implement{a}
linordset_remove (obj, xs, x0, cmp) = let
  val ans = bstree_remove_random (obj, xs, x0, cmp) in ans > 0
end // end of [linordset_remove]

(* ****** ****** *)

(* end of [linordset_randbst.dats] *)
