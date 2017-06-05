(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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

%{^
#include "prelude/CATS/array.cats"
%} // end of [%{^]

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "prelude/SATS/matrix.sats"

(* ****** ****** *)

#define i2sz size1_of_int1

(* ****** ****** *)

local

(*
assume matrix_v
  (a:viewt@ype, m:int, n:int, l:addr) =
  [mn:int | m >= 0; n >= 0] (MUL (m, n, mn), array_v (a, mn, l))
// end of [assume]
*)
assume mtrxt (a:viewt@ype, m:int, n:int) = @[@[a][n]][m]

in // in of [local]

implement
array_v_of_matrix_v
  {a} {m,n} (pfmat) = let
  prval pfmn = mul_istot {m,n} ()
  prval () = mul_nat_nat_nat (pfmn)
  prval pfarr = array_v_ungroup (pfmn, pfmat)
in
  (pfmn, pfarr)
end // end of [array_v_of_matrix_v]

implement
matrix_v_of_array_v
  (pf_mul, pfarr) = array_v_group (pf_mul, pfarr)
// end of [matrix_v_of_array_v]

end // end of [local]

(* ****** ****** *)

implement{a}
matrix_ptr_takeout_row (pf | p, i, n) =
  matrix_ptr_takeout_row_tsz (pf | p, i, n, sizeof<a>)
// end of [matrix_ptr_takeout_row]

implement{a}
matrix_ptr_takeout_elt (pf | p, i, n, j) =
  matrix_ptr_takeout_elt_tsz (pf | p, i, n, j, sizeof<a>)
// end of [matrix_ptr_takeout_elt]

(* ****** ****** *)

(*
**
** persistent matrices
**
*)

(* ****** ****** *)

assume
matrix_viewt0ype_int_int_type
  (a:viewt@ype, m:int, n:int) = [l:addr] @{
  data= ptr l, view= vbox (matrix_v (a, m, n, l))
} // end of [matrix_viewt0ype_int_int_type]

(* ****** ****** *)

extern
fun vbox_make_view_ptr_matrix
  {a:viewt@ype} {m,n:int} {l:addr} (
  pf: matrix_v (a, m, n, l) | p: ptr l
) :<> (vbox (matrix_v (a, m, n, l)) | void)= "atspre_vbox_make_view_ptr"
// end of [vbox_make_view_ptr_matrix]

(* ****** ****** *)

implement
matrix_make_arrsz
  {a} {m,n} (m, n, arrsz) = let
  prval pfmul = mul_istot {m,n} ()
  prval () = mul_elim (pfmul)
in
  matrix_make_arrsz__main {a} (pfmul | m, n, arrsz)
end // end of [matrix_make_arrsz]

implement
matrix_make_arrsz__main
  {a} (pfmul | m, n, arrsz) = let
  prval () = free_gc_elim {a?} (arrsz.0) // return the certificate
  prval pfmat = matrix_v_of_array_v (pfmul, arrsz.1)
  val (pfmat_box | ()) = vbox_make_view_ptr_matrix (pfmat | arrsz.2)
in @{
  data= arrsz.2, view= pfmat_box
} end // end of [matrix_make_arrsize__main]

(* ****** ****** *)

implement{a}
matrix_make_elt (m, n, x) = let
  val (pf_mul | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat pf_mul
  val (pfgc, pfarr | p_arr) =
    array_ptr_alloc_tsz {a} (mn, sizeof<a>)
  // end of [val]
  prval () = free_gc_elim {a?} (pfgc) // return the certificate
  val () = array_ptr_initialize_elt<a> (!p_arr, mn, x)
  prval pfmat = matrix_v_of_array_v (pf_mul, pfarr)
  val (pfmat_box | ()) = vbox_make_view_ptr_matrix (pfmat | p_arr)
in @{
  data= p_arr, view= pfmat_box
} end // end of [matrix_make_elt]

(* ****** ****** *)

%{^
#define atspre_matrix_szdiv(i, n) (i / n)
%}
extern
fun szdiv
  {m,n:pos; mn,i:nat | i < mn} (
  pf: MUL (m, n, mn) | i: size_t i, n: size_t n
) :<> [d:nat | d < m] size_t d
  = "mac#atspre_matrix_szdiv" // macro!
// end of [szdiv]

(* ****** ****** *)

infixl ( * ) szmul2; infixl ( mod ) szmod1

implement
matrix_make_funenv_tsz
  {a} {v} {vt} {m,n} {f:eff}
  (pf | m, n, f, tsz, env) = let
  val [mn:int] (pf_mul | mn) = m szmul2 n
  prval () = mul_nat_nat_nat pf_mul
  val (
    pfgc, pfarr | p_arr
  ) = array_ptr_alloc_tsz {a} (mn, tsz)
  prval () = free_gc_elim {a?} (pfgc) // return the certificate to GC
  viewtypedef fun_t =
    (!v | &(a?) >> a, sizeLt m, natLt n, !vt) -<> void
  var !p_f1 = @lam
    (pf: !v | i: sizeLt mn, x: &(a?) >> a, env: !vt): void =<clo,f> let
    val d = szdiv (pf_mul | i, n) and r = i szmod1 n
  in
    f (pf | d, r, x, env)
  end // end of [f1]
  val () = begin
    array_ptr_initialize_cloenv_tsz {a} {v} {vt} (pf | !p_arr, mn, !p_f1, tsz, env)
  end // end of [val]
  prval pfmat = matrix_v_of_array_v (pf_mul, pfarr)
  val (pfmat_box | ()) = vbox_make_view_ptr_matrix (pfmat | p_arr)
in @{
  data= p_arr, view= pfmat_box
} end // end of [matrix_make_funenv_tsz]

implement{a}
matrix_make_fun
  {m,n} {f:eff} (m, n, f) = let
  typedef fun0_t =
    (sizeLt m, sizeLt n, &(a?) >> a) -<fun,f> void
  typedef fun1_t =
    (!unit_v | sizeLt m, sizeLt n, &(a?) >> a, !ptr) -<fun,f> void
  val f = coerce (f) where {
    extern castfn coerce (f: fun0_t):<> fun1_t
  } // end of [val]
//
  prval pfu = unit_v ()
  val M = matrix_make_funenv_tsz {a} {unit_v} {ptr} (pfu | m, n, f, sizeof<a>, null)
  prval unit_v () = pfu
//
in
  M // : matrix (a, m, n)
end // end of [matrix_make_fun_tsz]

implement{a}
matrix_make_vclo
  {v} {m,n} {f:eff}
  (pfv | m, n, f) = M where {
//
  stavar l_f: addr
  val p_f: ptr l_f = &f
//
  typedef clo_t =
    (!v | sizeLt m, sizeLt n, &(a?) >> a) -<clo,f> void
  // end of [typedef]
  viewdef V = (v, clo_t @ l_f)
//
  fn app (
    pf: !V
  | i: sizeLt m, j: sizeLt n, x: &(a?) >> a, p_f: !ptr l_f
  ) :<f> void = let
    prval pfclo = pf.1 // HX: taking it out to support [!p_f]
    val () = !p_f (pf.0 | i, j, x)
    prval () = pf.1 := pfclo
  in
    // nothing
  end // end of [app]
//
  prval pfV = (pfv, view@ f)
  val M = matrix_make_funenv_tsz {a} {V} {ptr l_f} (pfV | m, n, app, sizeof<a>, p_f)
  prval () = (pfv := pfV.0; view@ f := pfV.1)
} // end of [matrix_make_vclo]

(* ****** ****** *)

local

(*
//
// HX-2011-02-26: commented out
//
prfun lemma_for_matrix_subscripting
  {m,n:nat} {i:nat | i < m} {mn,p:int} .<m>.
  (pf1: MUL (m, n, mn), pf2: MUL (i, n, p)): [p+n <= mn] void = let
  prval MULind pf11 = pf1
in
  sif i < m-1 then begin
    lemma_for_matrix_subscripting (pf11, pf2)
  end else let // i = m-1
    prval () = mul_isfun (pf11, pf2)
  in
    // empty
  end // end of [sif]
end // end of [lemma_for_matrix_subscripting]
*)

in // in of [local]

implement{a}
matrix_get_elt_at (M, i, n, j) = let
  prval vbox pfmat = M.view
  val (pfat, fpfmat | p) =
    matrix_ptr_takeout_elt<a> (pfmat | M.data, i, n, j)
  val x = !p
  prval () = pfmat := fpfmat (pfat)
in
  x // return value
end // end of [matrix_get_elt_at]

implement{a}
matrix_set_elt_at (M, i, n, j, x) = let
  prval vbox pfmat = M.view
  val (pfat, fpfmat | p) =
    matrix_ptr_takeout_elt<a> (pfmat | M.data, i, n, j)
  val () = !p := x
  prval () = pfmat := fpfmat (pfat)
in
  // empty
end // end of [matrix_set_elt_at]

end // end of [local]

(* ****** ****** *)

implement{a}
matrix_get_elt_at__intsz
  (M, i, n, j) = let
  val i = i2sz i; val n = i2sz n; val j = i2sz j
in
  matrix_get_elt_at<a> (M, i, n, j)
end // end of [matrix_get_elt_at__intsz]

implement{a}
matrix_set_elt_at__intsz
  (M, i, n, j, x) = let
  val i = i2sz i; val n = i2sz n; val j = i2sz j
in
  matrix_set_elt_at<a> (M, i, n, j, x)
end // end of [matrix_set_elt_at__intsz]

(* ****** ****** *)

implement{a}
matrix_foreach_funenv
  {v} {vt} {m,n}
  (pf | M, f, m, n, env) = let
  typedef fun_t = (!v | &a, !vt) -<fun> void
  typedef mat_t = matrix (a, m, n)
//
  fn* loop1 {i:nat | i <= m} .<m-i+1,0>. (
      pf: !v
    | M: mat_t
    , f: fun_t, m: size_t m, n: size_t n, i: size_t i
    , env: !vt
    ) :<!ref> void = begin
    if i < m then loop2 (pf | M, f, m, n, i, 0, env) else ()
  end // end of [loop1]
//
  and loop2 {i,j:nat | i < m; j <= n} .<m-i,n-j+1>. (
      pf: !v
    | M: mat_t
    , f: fun_t, m: size_t m, n: size_t n, i: size_t i, j: size_t j
    , env: !vt
    ) :<!ref> void = begin
    if j < n then let
      val () = () where {
        prval vbox pfmat = M.view
        val (pf1, fpf2 | p_ij) =
          matrix_ptr_takeout_elt_tsz (pfmat | M.data, i, n, j, sizeof<a>)
        val () = f (pf | !p_ij, env)
        val () = pfmat := fpf2 (pf1)
      } // end of [val]
    in
      loop2 (pf | M, f, m, n, i, j+1, env)
    end else begin
      loop1 (pf | M, f, m, n, i+1, env)
    end
  end // end of [loop2]
//
in
  loop1 (pf | M, f, m, n, 0, env)
end // end of [matrix_foreach_funenv]

implement{a}
matrix_foreach_fun
  {m,n} (M, f, m, n) = let
//
  val f = coerce (f) where { extern castfn
    coerce (f: (&a) -<> void):<> (!unit_v | &a, !ptr) -<> void
  } // end of [where]
//
  prval pfu = unit_v ()
  val () = matrix_foreach_funenv<a> {unit_v} {ptr} (pfu | M, f, m, n, null)
  prval unit_v () = pfu
//
in
  // nothing
end // end of [matrix_foreach_fun]

implement{a}
matrix_foreach_vclo
  {v} {m,n} (pfv | M, f, m, n) = let
//
  stavar l_f: addr
  val p_f: ptr l_f = &f
//
  typedef clo_t = (!v | &a) -<clo> void
  viewdef V = @(v, clo_t @ l_f)
  fn app (
    pf: !V | x: &a, p_f: !ptr l_f
  ) :<> void = let
    prval pfclo = pf.1 in !p_f (pf.0 | x); pf.1 := pfclo
  end // end of [app]
//
  prval pf = (pfv, view@ f)
  val () = matrix_foreach_funenv<a> {V} {ptr l_f} (pf | M, app, m, n, p_f)
  prval (pf1, pf2) = pf
  prval () = (pfv := pf1; view@ f := pf2)
//
in
  // empty
end // end of [matrix_foreach_vclo]

implement{a}
matrix_foreach_cloref {m,n} (M, f, m, n) = let
  viewtypedef cloref_t = (&a) -<cloref> void
  fn app (pf: !unit_v | x: &a, f: !cloref_t):<> void = f (x)
  prval pf = unit_v () 
  val () = matrix_foreach_funenv<a> {unit_v} {cloref_t} (pf | M, app, m, n, f)
  prval unit_v () = pf
in
  // empty
end // end of [matrix_foreach_cloref]

(* ****** ****** *)

implement{a}
matrix_iforeach_funenv
  {v} {vt} {m,n}
  (pf | M, f, m, n, env) = let
//
  typedef fun_t = (!v | sizeLt m, sizeLt n, &a, !vt) -<fun> void
  typedef mat_t = matrix (a, m, n)
//
  fn* loop1 {i:nat | i <= m} .<m-i+1,0>.
    (pf: !v | M: mat_t, f: fun_t, m: size_t m, n: size_t n, i: size_t i, env: !vt)
    :<!ref> void = begin
    if i < m then loop2 (pf | M, f, m, n, i, 0, env) else ()
  end // end of [loop1]
//
  and loop2 {i,j:nat | i < m; j <= n} .<m-i,n-j+1>. (
      pf: !v
    | M: mat_t, f: fun_t, m: size_t m, n: size_t n, i: size_t i, j: size_t j, env: !vt
    ) :<!ref> void = begin
    if j < n then let
      val () = () where {
        prval vbox pfmat = M.view
        val (pf1, fpf2 | p_ij) =
          matrix_ptr_takeout_elt_tsz (pfmat | M.data, i, n, j, sizeof<a>)
        val () = f (pf | i, j, !p_ij, env)
        val () = pfmat := fpf2 (pf1)
      } // end of [val]
    in
      loop2 (pf | M, f, m, n, i, j+1, env)
    end else begin
      loop1 (pf | M, f, m, n, i+1, env)
    end (* end of [if] *)
  end // end of [loop2]
//
in
  loop1 (pf | M, f, m, n, 0, env)
end // end of [matrix_iforeach_funenv]

implement{a}
matrix_iforeach_fun
  {m,n} (M, f, m, n) = let
//
  val f = coerce (f) where {
    extern castfn coerce (
      f: (sizeLt m, sizeLt n, &a) -<> void
    ) :<> (!unit_v | sizeLt m, sizeLt n, &a, !ptr) -<> void
  } // end of [where]
//
  prval pfu = unit_v ()
  val () = matrix_iforeach_funenv<a> {unit_v} {ptr} (pfu | M, f, m, n, null)
  prval unit_v () = pfu
//
in
  // nothing
end // end of [matrix_iforeach_fun]

implement{a}
matrix_iforeach_vclo
  {v} {m,n} (pfv | M, f, m, n) = let
//
  stavar l_f: addr
  val p_f: ptr l_f = &f
//
  typedef clo_t =
    (!v | sizeLt m, sizeLt n, &a) -<clo> void
  viewdef V = @(v, clo_t @ l_f)
//
  fn app (
    pf: !V | i: sizeLt m, j: sizeLt n, x: &a, p_f: !ptr l_f
  ) :<> void = let
    prval pfclo = pf.1
    val () = !p_f (pf.0 | i, j, x)
    prval () = pf.1 := pfclo
  in
    // nothing
  end // end of [app]
//
  prval pf = (pfv, view@ f)
  val () = matrix_iforeach_funenv<a> {V} {ptr l_f} (pf | M, app, m, n, p_f)
  prval (pf1, pf2) = pf
  prval () = (pfv := pf1; view@ f := pf2)
//
in
  // empty
end // end of [matrix_iforeach_vclo]

implement{a}
matrix_iforeach_cloref
  {m,n} (M, f, m, n) = let
//
  viewtypedef cloref_t = (sizeLt m, sizeLt n, &a) -<cloref1> void
//
  prval pf = unit_v ()
  fn app (
    pf: !unit_v
  | i: sizeLt m, j: sizeLt n, x: &a, f: !cloref_t
  ) :<> void =
    $effmask_all (f (i, j, x))
  // end of [app]
  val () = matrix_iforeach_funenv<a> {unit_v} {cloref_t} (pf | M, app, m, n, f)
  prval unit_v () = pf
//
in
  // empty
end // end of [matrix_iforeach_cloref]

(* ****** ****** *)

(* end of [matrix.dats] *)
