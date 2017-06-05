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

#define ATS_STALOADFLAG 0 // no dynamic staloading
#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

staload "prelude/SATS/array.sats"

(* ****** ****** *)

#define i2sz size1_of_int1

(* ****** ****** *)

(* array pointers *)

(* ****** ****** *)

extern
praxi lemma_array_param
  {a:viewt@ype} {n:int} {l:addr}
  (pf: !array_v (a, n, l)): [n>=0;l>null] void
// end of [lemma_array_param]

(* ****** ****** *)

implement{a} array_ptr_get_elt_at (A, i) = A.[i]
implement{a} array_ptr_set_elt_at (A, i, x) = (A.[i] := x)

implement{a}
array_ptr_xch_elt_at (A, i, x) = let
  var tmp: a // uninitialized
  val (pf, fpf | p) = array_ptr_takeout_tsz (view@ A | &A, i, sizeof<a>)
  val () = (tmp := !p; !p := x); prval () = view@ A := fpf pf
in
  x := tmp
end // end of [array_ptr_xch_elt_at]

implement{a}
array_ptr_exch
  (A, i, j) = if i != j then let
  var tmp: a // uninitialized
  extern prfun __copy {l:addr} (pf: !a @ l): a @ l  
  val (pf, fpf | pi) = array_ptr_takeout_tsz (view@ A | &A, i, sizeof<a>)
  prval pfi = __copy (pf)
  prval () = view@ A := fpf (pf)
  val (pf, fpf | pj) = array_ptr_takeout_tsz (view@ A | &A, j, sizeof<a>)
  prval pfj = __copy (pf)
  prval () = view@ A := fpf (pf)
  val () = (tmp := !pi; !pi := !pj; !pj := tmp)
  extern prfun __free {l:addr} (pf: a @ l): void
  prval () = __free (pfi)
  prval () = __free (pfj)
in
  // nothing
end // end of [array_ptr_exch]

(* ****** ****** *)

//
// These functions are present solely for notational convenience:
//

implement{a}
array_ptr_get_elt_at__intsz (A, i) = let val i = i2sz i in A.[i] end

implement{a}
array_ptr_set_elt_at__intsz (A, i, x) = let val i = i2sz i in A.[i] := x end

implement{a}
array_ptr_xch_elt_at__intsz (A, i, x) = let
  val i = i2sz i in array_ptr_xch_elt_at<a> (A, i, x)
end // end of [array_ptr_xch_elt_at__intsz]

implement{a}
array_ptr_exch__intsz (A, i, j) = let
  val i = i2sz i and j = i2sz j in array_ptr_exch (A, i, j)
end // end of [array_ptr_exch__intsz]

(* ****** ****** *)

implement{a}
array_ptr_alloc (asz) = array_ptr_alloc_tsz {a} (asz, sizeof<a>)

(* ****** ****** *)

implement{a}
array_ptr_allocfree (asz) = let
  val [l:addr] (pfgc, pfarr | p_arr) = array_ptr_alloc<a> (asz)
in #[l | (
  pfarr
| p_arr, lam (pfarr | p_arr) =<lin> array_ptr_free {a} (pfgc, pfarr | p_arr)
) ] end // end of [array_ptr_allocfree]

(* ****** ****** *)

implement{a}
array_ptr_free_fun
  (pfgc, pfarr | p, asz, f) = let
  val () = array_ptr_clear_fun<a> (!p, asz, f)
in
  array_ptr_free {a} (pfgc, pfarr | p)
end // end of [array_ptr_free_fun]

(* ****** ****** *)

implement{a}
array_ptr_initialize_elt (A0, n0, x0) = let
//
  prval () = lemma_array_param {a?} (view@ (A0))
//
  fun loop {n:nat} {l:addr} .<n>.
    (pf: array_v (a?, n, l) | p: ptr l, n: size_t n, x0: a)
    :<> (array_v (a, n, l) | void) =
    if n > 0 then let
      prval (pf1, pf2) = array_v_uncons {a?} (pf)
      val () = !p := x0
      val (pf2 | ans) = loop (pf2 | p+sizeof<a>, n-1, x0)
    in
      (array_v_cons {a} (pf1, pf2) | ans)
    end else let
      prval () = array_v_unnil {a?} (pf)
    in
      (array_v_nil {a} () | ())
    end // end of [if]
  // end of [loop]
  val (pf | ()) = loop (view@ A0 | &A0, n0, x0)
in
  view@ A0 := pf
end // end of [array_ptr_initialize_elt]

(* ****** ****** *)

implement{a}
array_ptr_initialize_lst (A0, xs0) = let
//
  prval () = lemma_array_param {a?} (view@ (A0))
//
  fun loop {n:nat} {l:addr} .<n>. (
      pf: array_v (a?, n, l) | p: ptr l, xs: list (a, n)
    ) :<> (
      array_v (a, n, l) | void
    ) = case+ xs of
    | list_cons (x, xs) => let
        prval (pf1, pf2) = array_v_uncons {a?} (pf)
        val () = !p := x
        val (pf2 | ans) = loop (pf2 | p+sizeof<a>, xs)
      in
        (array_v_cons {a} (pf1, pf2) | ans)
      end // end of [list_cons]
    | list_nil () => let
        prval () = array_v_unnil {a?} (pf) in (array_v_nil {a} () | ())
      end // end of [list_nil]  
  // end of [loop]
  val (pf | ()) = loop (view@ A0 | &A0, xs0)
in
  view@ A0 := pf
end // end of [array_ptr_initialize_lst]

(* ****** ****** *)

implement{a}
array_ptr_initialize_lst_vt (A0, xs0) = let
//
  prval () = lemma_array_param {a?} (view@ (A0))
//
  fun loop {n:nat} {l:addr} .<n>. (
      pf: array_v (a?, n, l) | p: ptr l, xs: list_vt (a, n)
    ) :<> (
      array_v (a, n, l) | void
    ) = case+ xs of
    | ~list_vt_cons (x, xs) => let
        prval (pf1, pf2) = array_v_uncons {a?} (pf)
        val () = !p := x
        val (pf2 | ans) = loop (pf2 | p+sizeof<a>, xs)
      in
        (array_v_cons {a} (pf1, pf2) | ans)
      end (* end of [lsit_vt_cons] *)
    | ~list_vt_nil () => let
        prval () = array_v_unnil {a?} (pf) in (array_v_nil {a} () | ())
      end (* end of [list_vt_nil] *)
  // end of [loop]
  val (pf | ()) = loop (view@ A0 | &A0, xs0)
in
  view@ A0 := pf
end // end of [array_ptr_initialize_lst_vt]

(* ****** ****** *)

implement
array_ptr_initialize_funenv_tsz
  {a} {v} {vt} {n} {f:eff}
  (pf | base, asz, f, tsz, env) = let
  fun loop {i:nat | i <= n} {l:addr} .<n-i>. (
    pfv: !v, pfarr: !array_v (a?, n-i, l) >> array_v (a, n-i, l)
  | p: ptr l, n: size_t n, i: size_t i
  , f: (!v | sizeLt n, &(a?) >> a, !vt) -<f> void, tsz: sizeof_t a, env: !vt
  ) :<f> void =
    if i < n then let
      prval (pf1_at, pf2_arr) = array_v_uncons {a?} (pfarr)
      val () = f (pfv | i, !p, env)
      val () = loop (pfv, pf2_arr | p + tsz, n, i+1, f, tsz, env)
      prval () = pfarr := array_v_cons {a} (pf1_at, pf2_arr)
    in
      // nothing
    end else let
      prval () = array_v_unnil (pfarr)
      prval () = pfarr := array_v_nil {a} ()
    in
      // nothing
    end // end of [if]
  // end of [loop]
in
  loop (pf, view@ base | &base, asz, 0, f, tsz, env)
end // end of [array_ptr_initialize_funenv_tsz]

implement{a}
array_ptr_initialize_fun
  {n} {f:eff} (base, asz, f) = let
//
  typedef fun_t = (sizeLt n, &(a?) >> a) -<f> void
  typedef fun1_t = (!unit_v | sizeLt n, &(a?) >> a, !ptr) -<f> void
  val f1 = coerce (f) where {
    extern castfn coerce (f: fun_t):<> fun1_t
  } // end of [val]
//
  prval pfu = unit_v ()
  val () = array_ptr_initialize_funenv_tsz
    {a} {unit_v} {ptr} (pfu | base, asz, f1, sizeof<a>, null)
  prval unit_v () = pfu
//
in
  // empty
end // end of [array_ptr_initialize_fun_tsz]

(* ****** ****** *)

implement
array_ptr_initialize_cloenv_tsz
  {a} {v} {vt} {n} {f:eff}
  (pf | base, asz, f, tsz, env) = let
  fun loop {i:nat | i <= n} {l:addr} .<n-i>. (
      pfv: !v, pfarr: !array_v (a?, n-i, l) >> array_v (a, n-i, l)
    | p: ptr l, n: size_t n, i: size_t i
    , f: &(!v | sizeLt n, &(a?) >> a, !vt) -<clo,f> void, tsz: sizeof_t a, env: !vt
    ) :<f> void =
    if i < n then let
      prval (pf1_at, pf2_arr) = array_v_uncons {a?} (pfarr)
      val () = f (pfv | i, !p, env)
      val () = loop (pfv, pf2_arr | p + tsz, n, i+1, f, tsz, env)
      prval () = pfarr := array_v_cons {a} (pf1_at, pf2_arr)
    in
      // nothing
    end else let
      prval () = array_v_unnil (pfarr)
      prval () = pfarr := array_v_nil {a} ()
    in
      // nothing
    end // end of [if]
  // end of [loop]
in
  loop (pf, view@ base | &base, asz, 0, f, tsz, env)
end // end of [array_ptr_initialize_cloenv_tsz]

implement{a}
array_ptr_initialize_vclo
  {v} {n} {f:eff}
  (pf | base, asz, f) = let
  val p_f = &f; prval pf_f = view@ f
  viewtypedef clo_t = (!v | sizeLt n, &(a?) >> a) -<clo,f> void
  viewtypedef clo1_t = (!v | sizeLt n, &(a?) >> a, !ptr) -<clo,f> void
  prval pf1_f = coerce (pf_f) where {
    extern praxi coerce {l:addr} (pf: clo_t @ l): clo1_t @ l
  } // end of [prval]
  val () = array_ptr_initialize_cloenv_tsz
    {a} {v} {ptr} (pf | base, asz, !p_f, sizeof<a>, null)
  prval pf_f = coerce (pf1_f) where {
    extern praxi coerce {l:addr} (pf: clo1_t @ l): clo_t @ l
  } // end of [prval]
  prval () = view@ f := pf_f
in
  // empty
end // end of [array_ptr_initialize_vclo]

(* ****** ****** *)

implement{a}
array_ptr_clear_fun
  {n} {f:eff} (base, asz, f) = let
//
  prval () = lemma_array_param {a} (view@ (base))
//
  fun clear {n:nat} {l:addr} .<n>. (
      pf_arr: !array_v (a, n, l) >> array_v (a?, n, l)
    | p_arr: ptr l, n: size_t n
    , f: (&a >> a?) -<fun,f> void, tsz: sizeof_t a
    ) :<f> void =
    if n > 0 then let
      prval (pf1_at, pf2_arr) = array_v_uncons {a} (pf_arr)
      val () = f (!p_arr)
      val () = clear (pf2_arr | p_arr + tsz, n-1, f, tsz)
    in
      pf_arr := array_v_cons {a?} (pf1_at, pf2_arr)
    end else let
      prval () = array_v_unnil {a} (pf_arr)
    in
      pf_arr := array_v_nil {a?} ()
    end // end of [if]
  // end of [clear]
in
  clear (view@ base | &base, asz, f, sizeof<a>)
end // end of [array_ptr_clear_fun]

(* ****** ****** *)

implement{a}
array_ptr_split (pf | A, i) =
  array_ptr_split_tsz {a} (pf | A, i, sizeof<a>)
// end of [array_ptr_split]

(* ****** ****** *)

implement{a}
array_ptr_takeout (pf | A, i) =
  array_ptr_takeout_tsz {a} (pf | A, i, sizeof<a>)
// end of [array_ptr_takeout]

(* ****** ****** *)

infixl ( * ) szmul2

implement{a}
array_ptr_takeout2 (pf | A, i1, i2) =
  array_ptr_takeout2_tsz {a} (pf | A, i1, i2, sizeof<a> )
// end of [array_ptr_takeout2]

implement
array_ptr_takeout2_tsz
  {a} {n} {i1,i2} {l0} (pf | A, i1, i2, tsz) = let
  val [off1:int] (pf1_mul | off1) = i1 szmul2 tsz
  val [off2:int] (pf2_mul | off2) = i2 szmul2 tsz
  prval (pf1, pf2, fpf) = array_v_takeout2 {a} (pf1_mul, pf2_mul, pf)
in
  #[ l0+off1, l0+off2 | (pf1, pf2, fpf | A+off1, A+off2) ]
end // end of [array_ptr_takeout2_tsz]

(* ****** ****** *)
//
// HX: array of arrays: this may just be a curiosity
//
implement{a}
array2_ptr_takeout (pf | A, i, n) =
  array2_ptr_takeout_tsz {a} (pf | A, i, n, sizeof<a>)
// end of [array2_ptr_takeout]

(* ****** ****** *)
//
// HX: persistent arrays that can only be reclaimed by GC
//
(* ****** ****** *)

assume array_viewt0ype_int_type
  (a:viewt@ype, n:int) = [l:addr] @{
  data= ptr l, view= vbox (array_v (a, n, l))
} // end of [array_viewt0ype_int_type]

(*
viewtypedef
arraysize_viewt0ype_int_viewt0ype
  (a: viewt@ype, n:int) = [l:addr] (free_gc_v l, @[a][n] @ l | ptr l, int n)
// end of [arraysize_viewt0ype_int_viewt0ype]
*)

implement
array_make_arrsz {a} {n} (arrsz) = let
  prval () = free_gc_elim {a?} (arrsz.0) // return the certificate
  val (pfbox | ()) = vbox_make_view_ptr (arrsz.1 | arrsz.2)
in
  @{ data= arrsz.2, view= pfbox }
end // end of [array_make_arrsz]

(* ****** ****** *)

implement{a}
array_make_elt (asz, x) = let
  val (
    pfgc, pfarr | p_arr
  ) = array_ptr_alloc_tsz {a} (asz, sizeof<a>)
  prval () = free_gc_elim {a?} (pfgc) // return the certificate to GC
  val () = array_ptr_initialize_elt<a> (!p_arr, asz, x)
  val (pfbox | ()) = vbox_make_view_ptr (pfarr | p_arr)
in
  @{ data= p_arr, view= pfbox }
end // end of [array_make_elt]

implement{a}
array_make_lst (asz, xs) = let
  val (
    pfgc, pfarr | p_arr
  ) = array_ptr_alloc_tsz {a} (asz, sizeof<a>)
  prval () = free_gc_elim {a?} (pfgc) // return the certificate to GC
  val () = array_ptr_initialize_lst<a> (!p_arr, xs)
  val (pfbox | ()) = vbox_make_view_ptr (pfarr | p_arr)
in
  @{ data= p_arr, view= pfbox }
end // end of [array_make_lst]

implement{a}
array_make_lst_vt (asz, xs) = let
  val (
    pfgc, pfarr | p_arr
  ) = array_ptr_alloc_tsz {a} (asz, sizeof<a>)
  prval () = free_gc_elim {a?} (pfgc) // return the certificate to GC
  val () = array_ptr_initialize_lst_vt<a> (!p_arr, xs)
  val (pfbox | ()) = vbox_make_view_ptr (pfarr | p_arr)
in
  @{ data= p_arr, view= pfbox }
end // end of [array_make_lst_vt]

(* ****** ****** *)

implement{a}
array_make_vclo
  (pf | asz, f) = let
  val (
    pfgc, pfarr | p_arr
  ) = array_ptr_alloc_tsz {a} (asz, sizeof<a>)
  prval () = free_gc_elim {a?}
    (pfgc) // return the certificate to GC
  val () = array_ptr_initialize_vclo<a> (pf | !p_arr, asz, f)
  val (pfbox | ()) = vbox_make_view_ptr (pfarr | p_arr)
in
  @{ data= p_arr, view= pfbox }
end // end of [array_make_vclo]

(* ****** ****** *)

implement{a}
array_make_cloref
  {n} {f:eff} (asz, f) = let
//
  typedef cloref_t = (sizeLt n, &(a?) >> a) -<cloref,f> void
//
  val (pfgc, pfarr | p_arr) = array_ptr_alloc<a> (asz)
  prval () = free_gc_elim {a?} (pfgc) // return the certificate to GC
//
  prval pfu = unit_v ()
  val () = let
    val app = lam (
      pf: !unit_v | i: sizeLt n, x: &(a?) >> a, f: !cloref_t
    ) : void =<fun,f> f (i, x) // end of [val]
  in
    array_ptr_initialize_funenv_tsz
      {a} {unit_v} {cloref_t} (pfu | !p_arr, asz, app, sizeof<a>, f)
    // end of ...
  end // end of [val]  
  prval unit_v () = pfu
//
  val (pfbox | ()) = vbox_make_view_ptr (pfarr | p_arr)
in
  @{ data= p_arr, view= pfbox }
end // end of [array_make_cloref]

(* ****** ****** *)

(*
//
// HX: these are now casting funtions:
//
implement array_make_view_ptr (pf | p) = @(pf | p)
implement array_get_view_ptr (A) = @(A.view | A.data)
*)

(* ****** ****** *)

implement{a}
array_get_elt_at (A, i) = let
  val A_data = A.data; prval vbox pf = A.view in !A_data.[i]
end // end of [array_get_elt]

implement{a}
array_set_elt_at (A, i, x) = let
  val A_data = A.data; prval vbox pf = A.view in !A_data.[i] := x
end // end of [array_set_elt_at]

implement{a}
array_xch_elt_at (A, i, x) = let
  val A_data = A.data; prval vbox pf = A.view
in
  array_ptr_xch_elt_at<a> (!A_data, i, x)
end // end of [array_xch_elt_at]

(* ****** ****** *)

implement{a}
array_get_elt_at__intsz (A, i) = let
  val i = i2sz i; val A_data = A.data; prval vbox pf = A.view
in
  !A_data.[i]
end // end of [array_get_elt]

implement{a}
array_set_elt_at__intsz (A, i, x) = let
  val i = i2sz i; val A_data = A.data; prval vbox pf = A.view
in
  !A_data.[i] := x
end // end of [array_set_elt_at]

implement{a}
array_xch_elt_at__intsz (A, i, x) = let
  val i = i2sz i; val A_data = A.data; prval vbox pf = A.view
in
  array_ptr_xch_elt_at<a> (!A_data, i, x)
end // end of [array_xch_elt_at]

(* ****** ****** *)

implement{a}
array_exch (A, i1, i2) =
  if i1 <> i2 then let
    val A_data = A.data; prval vbox pf = A.view
  in
    array_ptr_exch<a> (!A_data, i1, i2)
  end // end of [if]
// end of [array_exch]

implement{a}
array_exch__intsz (A, i1, i2) = let
  val i1 = i2sz (i1) and i2 = i2sz (i2)
in
  if i1 <> i2 then let
    val A_data = A.data; prval vbox pf = A.view
  in
    array_ptr_exch<a> (!A_data, i1, i2)
  end // end of [if]
end // end of [array_exch__intsz]

(* ****** ****** *)

// various [foreach] functions on linear arrays

(* ****** ****** *)

implement
array_ptr_foreach_funenv_tsz
  {a} {v} {vt} {n} {f:eff}
  (pf | base, f, asz, tsz, env) = let
  fun loop {i:nat | i <= n} {l:addr} .<i>. (
      pfv: !v, pf_arr: !array_v (a, i, l) >> array_v (a, i, l)
    | p: ptr l
    , f: (!v | &a, !vt) -<f> void, i: size_t i, tsz: sizeof_t a
    , env: !vt
    ) :<f> void =
    if i > 0 then let
      prval (pf1_at, pf2_arr) = array_v_uncons {a} (pf_arr)
      val () = f (pfv | !p, env)
      val () = loop (pfv, pf2_arr | p + tsz, f, i-1, tsz, env)
      prval () = pf_arr := array_v_cons {a} (pf1_at, pf2_arr)
    in
      // nothing
    end else let
      prval () = array_v_unnil (pf_arr)
      prval () = pf_arr := array_v_nil {a} ()
    in
      // nothing
    end // end of [if]
in
  loop (pf, view@ base | &base, f, asz, tsz, env)
end // end of [array_ptr_foreach_funenv_tsz]

(* ****** ****** *)

implement{a}
array_ptr_foreach_fun
  {n} {f:eff} (A, f, asz) = let
//
  viewtypedef fun0_t = (&a) -<fun,f> void
  viewtypedef fun1_t = (!unit_v | &a, !ptr) -<fun,f> void
//
  val f = __cast (f) where { extern castfn __cast (f: fun0_t):<> fun1_t }
//
  prval pfu = unit_v ()
  val () = array_ptr_foreach_funenv_tsz
    {a} {unit_v} {ptr} (pfu | A, f, asz, sizeof<a>, null)
  prval unit_v () = pfu
//
in
  // nothing
end // end of [array_ptr_foreach_fun]

(* ****** ****** *)

implement{a}
array_ptr_foreach_vclo
  {v} {n} {f:eff} (pfv | A, f, asz) = let
//
  viewtypedef clo_t = (!v | &a) -<clo,f> void
  stavar l_f: addr
  val p_f: ptr l_f = &f
  viewdef V = @(v, clo_t @ l_f)
//
  fn app (pf: !V | x: &a, p_f: !ptr l_f):<f> void = let
    prval (pf1, pf2) = pf; val () = !p_f (pf1 | x) in pf := (pf1, pf2)
  end // end of [app]
//
  prval pf = (pfv, view@ f)
  val () = array_ptr_foreach_funenv_tsz
    {a} {V} {ptr l_f} (pf | A, app, asz, sizeof<a>, p_f)
  prval (pf1, pf2) = pf
  prval () = (pfv := pf1; view@ f := pf2)
in
  // empty
end // end of [array_ptr_foreach_vclo]

(* ****** ****** *)

implement
array_ptr_iforeach_funenv_tsz
  {a} {v} {vt} {n} {f:eff}
  (pf | base, f, asz, tsz, env) = let
  fun loop {i:nat | i <= n} {l:addr} .<n-i>. (
      pfv: !v, pf_arr: !array_v (a, n-i, l) >> array_v (a, n-i, l)
    | p: ptr l
    , f: (!v | sizeLt n, &a, !vt) -<f> void, n: size_t n, i: size_t i
    , tsz: sizeof_t a, env: !vt
    ) :<f> void =
    if i < n then let
      prval (pf1_at, pf2_arr) = array_v_uncons {a} (pf_arr)
      val () = f (pfv | i, !p, env)
      val () = loop (pfv, pf2_arr | p + tsz, f, n, i+1, tsz, env)
      prval () = pf_arr := array_v_cons {a} (pf1_at, pf2_arr)
    in
      // nothing
    end else let
      prval () = array_v_unnil (pf_arr)
      prval () = pf_arr := array_v_nil {a} ()
    in
      // nothing
    end // end of [if]
in
  loop (pf, view@ base | &base, f, asz, 0, tsz, env)
end // end of [array_ptr_iforeach_funenv_tsz]

(* ****** ****** *)

implement{a}
array_ptr_iforeach_fun
  {n} {f:eff} (A, f, asz) = let
//
  viewtypedef fun0_t = (sizeLt n, &a) -<fun,f> void
  viewtypedef fun1_t = (!unit_v | sizeLt n, &a, !ptr) -<fun,f> void
  val f = __cast (f) where { extern castfn __cast (f: fun0_t):<> fun1_t }
//
  prval pfu = unit_v ()
  val () = array_ptr_iforeach_funenv_tsz
    {a} {unit_v} {ptr} (pfu | A, f, asz, sizeof<a>, null)
  prval unit_v () = pfu
//
in
  // nothing
end // end of [array_ptr_foreach_fun]

(* ****** ****** *)

implement{a}
array_ptr_iforeach_clo
  {n} {f:eff} (A, f, asz) = let
  typedef clo0_t = (sizeLt n, &a) -<clo,f> void
  typedef clo1_t = (!unit_v | sizeLt n, &a) -<clo,f> void
  prval () = __assert(f) where {
    extern prfun __assert (f: !clo0_t >> clo1_t): void
  } // end of [val]
  prval pfu = unit_v ()
  val () = array_ptr_iforeach_vclo<a> {unit_v} (pfu | A, f, asz)
  prval unit_v () = pfu
  prval () = __assert(f) where {
    extern prfun __assert (f: !clo1_t >> clo0_t): void
  } // end of [val]
in
  // nothing
end // end of [array_ptr_iforeach_clo]

implement{a}
array_ptr_iforeach_vclo
  {v} {n} {f:eff} (pfv | A, f, asz) = let
//
  viewtypedef clo_t = (!v | sizeLt n, &a) -<clo,f> void
  stavar l_f: addr
  val p_f: ptr l_f = &f
  viewdef V = @(v, clo_t @ l_f)
//
  fn app (
    pf: !V | i: sizeLt n, x: &a, p_f: !ptr l_f
  ) :<f> void = let
    prval (pf1, pf2) = pf; val () = !p_f (pf1 | i, x) in pf := (pf1, pf2)
  end // end of [app]
//
  prval pf = (pfv, view@ f)
  val () = array_ptr_iforeach_funenv_tsz
    {a} {V} {ptr l_f} (pf | A, app, asz, sizeof<a>, p_f)
  prval (pf1, pf2) = pf
//
  prval () = (pfv := pf1; view@ f := pf2)
in
  // empty
end // end of [array_ptr_iforeach_vclo]

(* ****** ****** *)
//
// HX: various [foreach] functions on persistent arrays
//
(* ****** ****** *)

implement{a}
array_foreach_funenv
  {v} {vt} {n}
  (pfv | A, f, asz, env) = let
  val (vbox pfarr | p) = array_get_view_ptr (A)
in
  array_ptr_foreach_funenv_tsz (pfv | !p, f, asz, sizeof<a>, env)
end // end of [array_foreach_funenv]

implement{a}
array_foreach_fun
  {n} (A, f, asz) = let
  val f = coerce (f) where { extern castfn
    coerce (f: (&a) -<> void):<> (!unit_v | &a, !ptr) -<> void
  } // end of [where]
//
  prval pfu = unit_v ()
  val () = array_foreach_funenv {unit_v} {ptr} (pfu | A, f, asz, null)
  prval unit_v () = pfu
in
  // nothing
end // end of [array_foreach_fun]

implement{a}
array_foreach_vclo
  {v} {n} (pfv | A, f, asz) = let
  stavar l_f: addr
  typedef clo_t = (!v | &a) -<clo> void
  val p_f: ptr l_f = &f
  viewdef V = (v, clo_t @ l_f)
  fn app (pf: !V | x: &a, p_f: !ptr l_f):<> void = let
    prval (pf1, pf2) = pf in !p_f (pf1 | x); pf := (pf1, pf2)
  end // end of [app]
  prval pf = (pfv, view@ f)
  val () = array_foreach_funenv<a> {V} {ptr l_f} (pf | A, app, asz, p_f)
  prval (pf1, pf2) = pf
  prval () = (pfv := pf1; view@ f := pf2)
in
  // empty
end // end of [array_foreach_vclo]

implement{a}
array_foreach_cloref {n} (A, f, asz) = let
  viewtypedef cloref_t = (&a) -<cloref> void
  fn app (pf: !unit_v | x: &a, f: !cloref_t):<> void = $effmask_all (f x)
  prval pf = unit_v ()
  val () = array_foreach_funenv<a> {unit_v} {cloref_t} (pf | A, app, asz, f)
  prval unit_v () = pf
in
  // empty
end // end of [array_foreach_cloref]

(* ****** ****** *)

implement{a}
array_iforeach_funenv
  {v} {vt} {n} (pfv | A, f, asz, env) = let
  val (vbox pfarr | p) = array_get_view_ptr (A)
in
  array_ptr_iforeach_funenv_tsz (pfv | !p, f, asz, sizeof<a>, env)
end // end of [array_iforeach_funenv]

implement{a}
array_iforeach_fun
  {n} (A, f, asz) = let
  val f = coerce (f) where { extern castfn
    coerce (f: (sizeLt n, &a) -<> void):<> (!unit_v | sizeLt n, &a, !ptr) -<> void
  } // end of [val]
//
  prval pfu = unit_v ()
  val () = array_iforeach_funenv {unit_v} {ptr} (pfu | A, f, asz, null)
  prval unit_v () = pfu
//  
in
  // nothing
end // end of [array_foreach_fun]

implement{a}
array_iforeach_vclo
  {v} {n} (pfv | A, f, asz) = let
  stavar l_f: addr
  typedef clo_t = (!v | sizeLt n, &a) -<clo> void
  val p_f: ptr l_f = &f
  viewdef V = (v, clo_t @ l_f)
  fn app
    (pf: !V | i: sizeLt n, x: &a, p_f: !ptr l_f):<> void = let
    prval (pf1, pf2) = pf in !p_f (pf1 | i, x); pf := (pf1, pf2)
  end // end of [app]
//
  prval pf = (pfv, view@ f)
  val () = array_iforeach_funenv<a> {V} {ptr l_f} (pf | A, app, asz, p_f)
  prval (pf1, pf2) = pf
//
  prval () = (pfv := pf1; view@ f := pf2)
in
  // empty
end // end of [array_iforeach_vclo]

implement{a}
array_iforeach_cloref {n} (A, f, asz) = let
  viewtypedef cloref_t = (sizeLt n, &a) -<cloref> void
  fn app (pf: !unit_v | i: sizeLt n, x: &a, f: !cloref_t):<> void = f (i, x)
  prval pf = unit_v ()
  val () = array_iforeach_funenv<a> {unit_v} {cloref_t} (pf | A, app, asz, f)
  prval unit_v () = pf
in
  // empty
end // end of [array_iforeach_cloref]

(* ****** ****** *)

implement{a}
array_ptr_alloc (n) = array_ptr_alloc_tsz {a} (n, sizeof<a>)
// end of [array_ptr_alloc]

(* ****** ****** *)

%{$

typedef unsigned char byte ;

//
// HX-2010-05-24
// In case 'memcpy' is already defined as a macro ...
//
#ifndef memcpy
extern void *memcpy (void *dst, const void* src, size_t n) ;
#endif // end of [memcpy]

ats_void_type
atspre_array_ptr_initialize_elt_tsz (
  ats_ptr_type A
, ats_size_type asz
, ats_ptr_type ini
, ats_size_type tsz
)  {
  int i, itsz ; int left ; ats_ptr_type p ;
  if (asz == 0) return ;
  memcpy (A, ini, tsz) ;
  i = 1 ; itsz = tsz ; left = asz - i ;
  while (left > 0) {
    p = (ats_ptr_type)(((byte*)A) + itsz) ;
    if (left <= i) { memcpy (p, A, left * tsz) ; return ; }
    memcpy (p, A, itsz);
    i = i + i ; itsz = itsz + itsz ; left = asz - i ;
  } /* end of [while] */
  return ;
} /* end of [atspre_array_ptr_initialize_elt_tsz] */

%} // end of [%{$]

(* ****** ****** *)

// [array.sats] is already loaded by a call to [pervasive_load]
staload _(*anonymous*) = "prelude/SATS/array.sats" // this forces that the static
// loading function for [array.sats] is to be called at run-time
// this is really needed only if some datatypes are declared in [array.sats]

(* ****** ****** *)

(* end of [array.dats] *)
