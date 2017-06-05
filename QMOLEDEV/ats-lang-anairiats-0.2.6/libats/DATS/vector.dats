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
** A dynamically resizable vector implementation
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Secptember, 2010
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // there is no need for dynloading at run-time

(* ****** ****** *)

staload "libats/SATS/vector.sats"

(* ****** ****** *)

implement
vector_cap
  {a} (V) = m where {
  prval pf = VECTOR_decode {a} (V)
  val m = V.m
  prval () = VECTOR_encode {a} (pf | V)
} // end of[ vector_cap]

implement
vector_size
  {a} (V) = n where {
  prval pf = VECTOR_decode {a} (V)
  val n = V.n
  prval () = VECTOR_encode {a} (pf | V)
} // end of[ vector_size]

(* ****** ****** *)

extern
prfun vector_v_encode01
  {a:viewt@ype} {n:int} {l:addr} (pf: array_v (a?, n, l)): vector_v (a, n, 0, l)
// end of [vector_v_encode01]

implement{a}
vector_initialize {m} (V, m) = let
  val [l:addr] (pfgc, pfarr | p) = array_ptr_alloc<a> (m)
  prval pfgc = __cast (pfgc) where {
    extern prfun __cast (pf: free_gc_v (a?, m, l)): free_gc_v l
  } // end of [prval]
  prval pfarr = vector_v_encode01 {a} (pfarr)
  val () = V.m := m
  val () = V.n := size1_of_int1 (0)
  val () = V.ptr := p
  val () = V.vfree := pfgc
  prval () = VECTOR_encode {a} (pfarr | V)
in
  // nothing
end // end of [vector_initialize]

(* ****** ****** *)

implement
vector_clear
  {a} {m,n} (V) = let
  prval pf = VECTOR_decode {a} (V)
  prval pfmul = mul_istot {n,sizeof a} ()
  prval (pf1, pf2) = vector_v_decode {a} (pfmul, pf)
  prval pf = array_v_unsplit {a?} (pfmul, pf1, pf2)
  prval pf = vector_v_encode01 {a} (pf)
  val () = V.n := (size1_of_int1)0
  prval () = VECTOR_encode {a} (pf | V)
in
  // nothing  
end // end of [vector_clear]

(* ****** ****** *)

implement{a}
vector_clear_fun
  {m,n} (V, f) = let
  prval () = __assert () where { extern prfun __assert (): [m>=n;n>=0] void }
  prval pf0 = VECTOR_decode {a} (V)
//
  prval pfmul = mul_istot {n,sizeof a} ()
  prval (pf1, pf2) = vector_v_decode {a} (pfmul, pf0)
//
  val n = V.n
  val p = V.ptr
  val () = array_ptr_clear_fun<a> (!p, n, f)
//
  prval pf = array_v_unsplit {a?} (pfmul, pf1, pf2)
//
  prval pf = vector_v_encode01 {a} (pf)
//
  val () = V.n := (size1_of_int1)0
  prval () = VECTOR_encode {a} (pf | V)
in
  // nothing  
end // end of [vector_clear_fun]

(* ****** ****** *)

implement{a}
vector_uninitialize {m,n} (V) = let
  prval pf = VECTOR_decode {a} (V)
  prval pfmul = mul_istot {n,sizeof a} ()
  prval (pf1, pf2) = vector_v_decode {a} (pfmul, pf)
  prval pf = array_v_unsplit {a?} (pfmul, pf1, pf2)
  val () = __free (V.vfree, pf | V.ptr) where {
    extern fun __free {l:addr}
      (pf_gc: free_gc_v l, pf: array_v (a?, m, l) | p: ptr l):<> void = "ats_free_gc"
    // end of [__free]
  } // end of [val]
in
  // nothing
end // end of [vector_unintialize]

implement{a}
vector_uninitialize_vt {m} (V) = let
  prval pf = VECTOR_decode {a} (V)
  prval pfmul = mul_istot {0,sizeof a} ()
  prval () = mul_elim {0,sizeof a} (pfmul)
  prval (pf1, pf2) = vector_v_decode {a} (pfmul, pf)
  prval () = array_v_unnil (pf1)
  val () = __free (V.vfree, pf2 | V.ptr) where {
    extern fun __free {l:addr}
      (pf_gc: free_gc_v l, pf: array_v (a?, m, l) | p: ptr l):<> void = "ats_free_gc"
    // end of [__free]
  } // end of [val]
in
  // nothing
end // end of [vector_unintialize_vt]

(* ****** ****** *)

implement{a}
vector_get_elt_at
  {m,n} (V, i) = let
  prval pf = VECTOR_decode {a} (V)
  val p = V.ptr
  prval pfmul = mul_istot {n,sizeof a} ()
  prval @(pf1, pf2) = vector_v_decode {a} (pfmul, pf)
  val x = array_ptr_get_elt_at<a> (!p, i)
  prval () = pf := vector_v_encode {a} (pfmul, pf1, pf2)
  prval () = VECTOR_encode {a} (pf | V)
in
  x
end // end of [vector_get_elt_at]

implement{a}
vector_set_elt_at
  {m,n} (V, i, x) = let
  prval pf = VECTOR_decode {a} (V)
  val p = V.ptr
  prval pfmul = mul_istot {n,sizeof a} ()
  prval @(pf1, pf2) = vector_v_decode {a} (pfmul, pf)
  val () = array_ptr_set_elt_at<a> (!p, i, x)
  prval () = pf := vector_v_encode {a} (pfmul, pf1, pf2)
  prval () = VECTOR_encode {a} (pf | V)
in
  // nothing
end // end of [vector_get_elt_at]

(* ****** ****** *)

(*
//
// HX: this is a bit less efficient
//
implement{a}
vector_append {m,n} (V, x) = let
  prval () = __assert () where { extern prfun __assert (): [n>=0] void }
  val n = vector_get_size (V)
in
  vector_insert_at<a> (V, n, x)
end // end of [vector_append]
*)
implement{a}
vector_append (V, x) = let
  prval pf = VECTOR_decode {a} (V)
  val n = V.n
  prval () = __assert (n) where {
    extern prfun __assert {n:int} (_: size_t n): [n>=0] void
  } // end of [val]
  val (pfmul | ofs) = mul2_size1_size1 (n, sizeof<a>)
  prval (pf1, pf2) = vector_v_decode {a} (pfmul, pf)
  prval (pf21, pf22) = array_v_uncons {a?} (pf2)
  val p = V.ptr + ofs; val () = !p := x
  prval pf1 = array_v_extend {a} (pfmul, pf1, pf21)
  prval pfmul = MULind (pfmul)
  prval pf = vector_v_encode {a} (pfmul, pf1, pf22)
  val () = V.n := n + 1
  prval () = VECTOR_encode {a} (pf | V)
in
  // nothing
end // end of [vector_append]

(* ****** ****** *)

implement{a}
vector_prepend {m,n} (V, x) = let
  prval () = __assert () where { extern prfun __assert (): [n>=0] void }
  var x: a = x
in
  vector_insert_at<a> (V, 0, x)
end // end of [vector_prepend]

(* ****** ****** *)

implement{a}
vector_insert_at
  {m,n} {i} (V, i, x) = let
  prval () = __assert () where { extern prfun __assert (): [m>=n;n>=0] void }
  prval pf = VECTOR_decode {a} (V)
//
  stavar l0:addr
  val p0 : ptr l0 = V.ptr
  val [ofs1:int] (pfmul1 | ofs1) = mul2_size1_size1 (i, sizeof<a>)
  val n = V.n
  val [ofs:int] (pfmul | ofs) = mul2_size1_size1 (n, sizeof<a>)
//
  prval (pf1, pf2) = vector_v_decode {a} (pfmul, pf)
  prval (pf11, pf12) = array_v_split {a} {n} {i} (pfmul1, pf1)
  prval (pf21, pf22) = array_v_uncons {a?} (pf2)  
  prval pf3 = array_v_nil {a} {l0+ofs+sizeof a} ()
//
  prval pfmul2 = mul_distribute2 (pfmul, mul_negate (pfmul1))
//
  val (pfr1, pfr2 | ()) =
    loop (pfmul2, pf12, pf21, pf3 | p0+ofs, n-i) where {
    fun loop {n1,n2:nat} {l:addr} {ofs:int} .<n1>. (
        pfmul: MUL (n1, sizeof a, ofs)
      , pf1: array_v (a, n1, l), pf2: a? @ (l+ofs), pf3: array_v (a, n2, l+ofs+sizeof a)
      | p1: ptr (l+ofs), n1: size_t n1
      ) :<> (a? @ l, array_v (a, n1+n2, l+sizeof a) | void) =
      if n1 > 0 then let
        prval (pf11, pf12) = array_v_unextend {a} (pfmul, pf1)
        prval pfmul = mul_add_const {~1} (pfmul)
        val p2 = p1 - sizeof<a>; val () = !p1 := !p2
        prval pf3 = array_v_cons {a} (pf2, pf3)
      in
        loop (pfmul, pf11, pf12, pf3 | p2, n1-1)
      end else let
        prval () = mul_elim {0,sizeof a} (pfmul)
        prval () = array_v_unnil (pf1)
      in
        (pf2, pf3 | ())
      end // end of [if]
    // end of [loop]
  } // end of [val]
  val p = p0 + ofs1; val () = !p := x
  prval pf12 = array_v_cons {a} (pfr1, pfr2)
  prval pf1 = array_v_unsplit {a} (pfmul1, pf11, pf12)
  prval pfmul = MULind (pfmul)
  prval pf = vector_v_encode {a} (pfmul, pf1, pf22)
//
  val () = V.n := n + 1
  prval () = VECTOR_encode {a} (pf | V)
//
in
  // nothing
end // end of [vector_insert_at]  

(* ****** ****** *)

implement{a}
vector_remove_at {m,n} {i} (V, i, x) = let
//
  stadef tsz = sizeof (a)
//
  prval () = __assert () where { extern prfun __assert (): [m>=n;n>=0] void }
  prval pf = VECTOR_decode {a} (V)
//
  stavar l0:addr
  val p0 : ptr l0 = V.ptr
  val [ofs1:int] (pfmul1 | ofs1) = mul2_size1_size1 (i, sizeof<a>)
  prval pfmul = mul_istot {n,tsz} ()
  prval pfmul2 = mul_distribute2 (pfmul, mul_negate (pfmul1))
  prval pfmul2 = mul_add_const {~1} (pfmul2)
//
  prval (pf1, pf2) = vector_v_decode {a} (pfmul, pf)
  prval (pf11, pf12) = array_v_split {a} {n} {i} (pfmul1, pf1)
  prval (pf121, pf122) = array_v_uncons {a} (pf12)
  val p = p0 + ofs1; val () = x := !p
//
  val n = V.n
  val (pf11, pf12 | ()) = loop (
    pfmul1, pfmul2, pf11, pf121, pf122 | p, n-i-1
  ) where {
    fun loop {n1,n2:nat} {ofs1,ofs2:int} .<n2>. (
        pfmul1: MUL (n1, tsz, ofs1)
      , pfmul2: MUL (n2, tsz, ofs2)
      , pf1: array_v (a, n1, l0)
      , pf21: a? @ (l0+ofs1)
      , pf22: array_v (a, n2, l0+ofs1+tsz)
      | p: ptr (l0+ofs1), n2: size_t (n2)
    ) :<> (array_v (a, n1+n2, l0), a? @ (l0+ofs1+ofs2) | void) =
      if n2 > 0 then let
        prval (pf221, pf222) = array_v_uncons {a} (pf22)
        val p1 = p+sizeof<a>; val () = !p := !p1
        prval pf1 = array_v_extend {a} (pfmul1, pf1, pf21)
        prval pfmul1 = mul_add_const {1} (pfmul1)
        prval pfmul2 = mul_add_const {~1} (pfmul2)
      in
        loop (pfmul1, pfmul2, pf1, pf221, pf222 | p1, n2-1)
      end else let
        prval () = array_v_unnil (pf22)
        prval () = mul_elim {0,tsz} (pfmul2)
      in
        (pf1, pf21 | ())
      end // end of [if]
    // end of [loop]
  } // end of [val]
//
  prval pf2 = array_v_cons {a?} (pf12, pf2)
  prval MULind (pfmul) = pfmul
  prval pf = vector_v_encode {a} (pfmul, pf11, pf2)
//
  val () = V.n := n - 1
  prval () = VECTOR_encode {a} (pf | V)
//
in
  // nothing
end // end of [vector_remove_at]

(* ****** ****** *)

implement{a}
vector_resize
  {m,n} {m1} (V, m1) = let
  prval pf = VECTOR_decode {a} (V)
//
  val p1 = realloc (
    V.vfree, pf | V.ptr, m1, sizeof<a>
  ) where {
    extern fun realloc {l:addr} {n <= m1} (
      pf_gc: !free_gc_v l >> free_gc_v l
    , pf: !vector_v (a, m, n, l) >> vector_v (a, m1, n, l)
    | p: ptr l, m1: size_t m1, tsz: sizeof_t a
    ) :<> #[l:addr] ptr l = "mac#atslib_vector_realloc"
  } // end of[ val]
//
  val () = V.m := m1
  val () = V.ptr := p1
  prval () = VECTOR_encode {a} (pf | V)
in
  // nothing
end // end of [vector_resize]

(* ****** ****** *)

implement
vector_foreach_funenv_tsz
  {a} {v} {vt} {m,n}
  (pf | V, f, tsz, env) = let
  prval () = __assert () where {
    extern praxi __assert (): [m>=n;n>=0] void
  } // end of [prval]
  prval pf0 = VECTOR_decode {a} (V)
  prval pfmul = mul_istot {n,sizeof a} ()
  prval (pf1, pf2) = vector_v_decode {a} (pfmul, pf0)
  val p = V.ptr
  val () = array_ptr_foreach_funenv_tsz (pf | !p, f, V.n, tsz, env)
  prval pf0 = vector_v_encode {a} (pfmul, pf1, pf2)
  prval () = VECTOR_encode {a} (pf0 | V)
in
  // nothing
end // end of [vector_foreach_funenv_tsz]

(* ****** ****** *)

implement{a}
vector_foreach_vclo
  {v} {m,n} (pf_v | V, f) = let
  viewtypedef clo_t = (!v | &a) -<clo> void
  stavar l_f: addr
  val p_f: ptr l_f = &f
  viewdef V = @(v, clo_t @ l_f)
  fn app (pf: !V | x: &a, p_f: !ptr l_f):<> void = let
    prval (pf1, pf2) = pf; val () = !p_f (pf1 | x) in pf := (pf1, pf2)
  end // end of [app]
  prval pf = (pf_v, view@ f)
  val () = vector_foreach_funenv_tsz
    {a} {V} {ptr l_f} (pf | V, app, sizeof<a>, p_f)
  prval (pf1, pf2) = pf
  prval () = (pf_v := pf1; view@ f := pf2)
in
  // empty
end // end of [vector_foreach_vclo]

(* ****** ****** *)

implement
vector_iforeach_funenv_tsz
  {a} {v} {vt} {m,n}
  (pf | V, f, tsz, env) = let
  prval () = __assert () where {
    extern praxi __assert (): [m>=n;n>=0] void
  } // end of [prval]
  prval pf0 = VECTOR_decode {a} (V)
  prval pfmul = mul_istot {n,sizeof a} ()
  prval (pf1, pf2) = vector_v_decode {a} (pfmul, pf0)
  val p = V.ptr
  val () = array_ptr_iforeach_funenv_tsz (pf | !p, f, V.n, tsz, env)
  prval pf0 = vector_v_encode {a} (pfmul, pf1, pf2)
  prval () = VECTOR_encode {a} (pf0 | V)
in
  // nothing
end // end of [vector_iforeach_funenv_tsz]

(* ****** ****** *)

implement{a}
vector_iforeach_vclo
  {v} {m,n} (pf_v | A, f) = let
  viewtypedef clo_t = (!v | sizeLt n, &a) -<clo> void
  stavar l_f: addr
  val p_f: ptr l_f = &f
  viewdef V = @(v, clo_t @ l_f)
  fn app (pf: !V | i: sizeLt n, x: &a, p_f: !ptr l_f):<> void = let
    prval (pf1, pf2) = pf; val () = !p_f (pf1 | i, x) in pf := (pf1, pf2)
  end // end of [app]
  prval pf = (pf_v, view@ f)
  val () = vector_iforeach_funenv_tsz
    {a} {V} {ptr l_f} (pf | A, app, sizeof<a>, p_f)
  prval (pf1, pf2) = pf
  prval () = (pf_v := pf1; view@ f := pf2)
in
  // empty
end // end of [vector_iforeach_vclo]

(* ****** ****** *)

(* end of [vector.dats] *)
