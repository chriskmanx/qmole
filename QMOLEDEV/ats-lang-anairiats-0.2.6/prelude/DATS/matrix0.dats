(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
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
//
// matrix0 implementation
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "prelude/SATS/matrix0.sats"
staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

#define i2sz size1_of_int1

(* ****** ****** *)

assume matrix0_viewt0ype_type
  (a:viewt@ype) = ref [l:addr] [m,n:nat] [mn:int] (
  MUL (m, n, mn), free_gc_v (a?, mn, l), @[a][mn] @ l | ptr l, size_t m, size_t n
) // end of [matrix0_viewt0ype_type]

(* ****** ****** *)

implement
matrix0_make_arrsz
  {a} {m,n} (m, n, arrsz) = let
  prval pfmul = mul_istot {m,n} ()
  prval () = mul_elim (pfmul)
in
  ref @(pfmul, arrsz.0, arrsz.1 | arrsz.2, m, n)
end // end of [matrix0_make_arrsz]

implement
matrix0_make_arrsz__main
  {a} {m,n} {mn} (pfmul | m, n, arrsz) = let
in
  ref @(pfmul, arrsz.0, arrsz.1 | arrsz.2, m, n)
end (* end of [matrix0_make_arrsz] *)

(* ****** ****** *)

implement{a}
matrix0_make_elt (row, col, x0) = let
  val [m:int] row = size1_of_size (row)
  val [n:int] col = size1_of_size (col)
  val [mn:int] (pfmul | asz) = mul2_size1_size1 (row, col)
  prval () = mul_nat_nat_nat (pfmul)
  val tsz = sizeof<a>
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {a} (asz, tsz)
  var ini: a = x0
  val () = array_ptr_initialize_elt_tsz {a} (!p_arr, asz, ini, tsz)
in
  matrix0_make_arrsz__main {a} (pfmul | row, col, @(pf_gc, pf_arr | p_arr, asz))
end // end of [matrix0_make_elt]

(* ****** ****** *)

implement matrix0_row (M) = let
  val (vbox pf | p) = ref_get_view_ptr (M) in p->4
end // end of [matrix0_row]

implement matrix0_col (M) = let
  val (vbox pf | p) = ref_get_view_ptr (M) in p->5
end // end of [matrix0_col]

(* ****** ****** *)

// this one is proven in [matrix.dats]
extern prfun lemma_for_matrix_subscripting
  {m,n:nat} {i:nat | i < m} {mn,p:int}
  (pf1: MUL (m, n, mn), pf2: MUL (i, n, p)): [p+n <= mn] void
// end of [lemma_for_matrix_subscripting]  

implement{a}
matrix0_get_elt_at (M, i, j) = let
  val (vbox pf | p) = ref_get_view_ptr (M)
  val i = size1_of_size i
  val j = size1_of_size j
  val p_data = p->3; val row = p->4 and col = p->5
in
  if i < row then (
    if j < col then let
      prval pf_data = p->2
      val (pfmul | icol) = mul2_size1_size1 (i, col)
      prval () = lemma_for_matrix_subscripting (p->0, pfmul)
      prval () = mul_nat_nat_nat (pfmul) 
      val x = p_data->[icol + j]
      prval () = p->2 := pf_data
    in
      x // return value
    end else // out-of-col
      $raise MatrixSubscriptException ()
    // end of [if]
  ) else (
    $raise MatrixSubscriptException () // out-of-row
  ) // end of [if]
end (* end of [matrix0_get_elt_at] *)

implement{a}
matrix0_set_elt_at (M, i, j, x) = let
  val (vbox pf | p) = ref_get_view_ptr (M)
  val i = size1_of_size i
  val j = size1_of_size j
  val p_data = p->3; val row = p->4 and col = p->5
in
  if i < row then (
    if j < col then let
      prval pf_data = p->2
      val (pfmul | icol) = mul2_size1_size1 (i, col)
      prval () = lemma_for_matrix_subscripting (p->0, pfmul)
      prval () = mul_nat_nat_nat (pfmul) 
      val () = p_data->[icol + j] := x
      prval () = p->2 := pf_data
    in
      // nothing
    end else // out-of-col
      $raise MatrixSubscriptException ()
    // end of [if]
  ) else (
    $raise MatrixSubscriptException () // out-of-row
  ) // end of [if]
end (* end of [matrix0_set_elt_at] *)

(* ****** ****** *)

implement{a}
matrix0_get_elt_at__intsz (A, i, j) = let
  val i = int1_of_int i and j = int1_of_int j in
  if i >= 0 then (
    if j >= 0 then (
      matrix0_get_elt_at<a> (A, i2sz i, i2sz j)
    ) else (
      $raise MatrixSubscriptException ()
    ) // end of [if]
  ) else (
    $raise MatrixSubscriptException () // out-of-row
  ) // end of [if]
end (* end of [matrix0_get_elt_at__intsz] *)

implement{a}
matrix0_set_elt_at__intsz (A, i, j, x) = let
  val i = int1_of_int i and j = int1_of_int j in
  if i >= 0 then (
    if j >= 0 then (
      matrix0_set_elt_at<a> (A, i2sz i, i2sz j, x)
    ) else (
      $raise MatrixSubscriptException ()
    ) // end of [if]
  ) else (
    $raise MatrixSubscriptException () // out-of-row
  ) // end of [if]
end (* end of [matrix0_set_elt_at__intsz] *)

(* ****** ****** *)

implement{a}
matrix0_foreach (M, f) = let
  fun loop {k:nat} {l:addr} .<k>. (
      pf: !array_v (a, k, l)
    | p: ptr l, k: size_t k, f: (&a) -<cloref> void
    ) :<> void =
    if k > 0 then let
      prval (pf1, pf2) = array_v_uncons {a} (pf)
      val () = f (!p)
      val () = loop (pf2 | p+sizeof<a>, k-1, f)
    in
      pf := array_v_cons {a} (pf1, pf2)
    end // end of [if]
  // end of [loop]
  val (vbox pf | p) = ref_get_view_ptr (M)
  val m = p->4 and n = p->5
  val (pf_mn | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat (pf_mn)
  prval () = mul_isfun (pf_mn, p->0)
in
  loop (p->2 | p->3, mn, f)
end // end of [matrix0_foreach]

(* ****** ****** *)

implement{a}
matrix0_iforeach (M, f) = let
  fun loop {k:nat} {l:addr} .<k>. (
      pf: !array_v (a, k, l)
    | p: ptr l, k: size_t k
    , i: size_t, n: size_t, j: size_t
    , f: (size_t, size_t, &a) -<cloref> void
    ) :<> void =
    if k > 0 then let
      prval (pf1, pf2) = array_v_uncons {a} (pf)
      val () = f (i, j, !p)
      val j1 = j+1
      val () = (if j1 < n
        then loop (pf2 | p+sizeof<a>, k-1, i, n, j1, f)
        else loop (pf2 | p+sizeof<a>, k-1, i+1, n, 0, f)
      ) : void
    in
      pf := array_v_cons {a} (pf1, pf2)
    end // end of [if]
  // end of [loop]
  val (vbox pf | p) = ref_get_view_ptr (M)
  val m = p->4 and n = p->5
  val (pf_mn | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat (pf_mn)
  prval () = mul_isfun (pf_mn, p->0)
in
  loop (p->2 | p->3, mn, 0, n, 0, f)
end // end of [matrix0_iforeach]

(* ****** ****** *)

implement{a}
matrix0_tabulate (row, col, f) = let
  val [m:int] m = size1_of_size row
  val [n:int] n = size1_of_size col
  val [mn:int] (pf_mn | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat (pf_mn)
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {a} (mn, sizeof<a>)
  fun loop {k:nat} {l:addr} .<k>. (
      pf: !array_v (a?, k, l) >> array_v (a, k, l)
    | p: ptr l, k: size_t k
    , i: size_t, n: size_t n, j: size_t, f: (size_t, size_t) -<cloref> a
    ) :<> void =
    if k > 0 then let
      prval (pf1, pf2) = array_v_uncons {a?} (pf)
      val () = !p := f (i, j)
      val j1 = j+1
    in
      if j1 < n then let
        val () = loop (pf2 | p+sizeof<a>, k-1, i, n, j1, f)
      in
        pf := array_v_cons {a} (pf1, pf2)
      end else let
        val () = loop (pf2 | p+sizeof<a>, k-1, i+1, n, 0, f)
      in
        pf := array_v_cons {a} (pf1, pf2)
      end // end of [if]
    end else let
      prval () = array_v_unnil {a?} (pf)
    in
      pf := array_v_nil {a} ()
    end // end of [if]
  // end of [loop]
  val () = loop (pf_arr | p_arr, mn, 0, n, 0, f)
in
  matrix0_make_arrsz__main {a} (pf_mn | m, n, @(pf_gc, pf_arr | p_arr, mn))
end // end of [array0_tabulate]

(* ****** ****** *)

// [matrix0.sats] is already loaded by a call to [pervasive_load]
staload _(*anonymous*) = "prelude/SATS/matrix0.sats" // this forces that the static
// loading function for [matrix0.sats] is to be called at run-time
// this is really needed only if some datatypes are declared in [matrix0.sats]

(* ****** ****** *)

(* end of [matrix0.dats] *)
