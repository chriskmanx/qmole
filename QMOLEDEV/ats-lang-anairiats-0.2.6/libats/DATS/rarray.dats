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
** Reversed Arrays (arrays running from right to left)
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: September, 2010
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // there is no need for run-time dynloading

(* ****** ****** *)

staload "libats/SATS/rarray.sats"

(* ****** ****** *)

implement
rarray_v_unnil (pfarr) = let
  prval rarray_v_nil () = pfarr in (* nothing *)
end // end of [rarray_v_unnil]

implement
rarray_v_uncons (pfarr) = let
  prval rarray_v_cons (pf1arr, pf1at) = pfarr in (pf1arr, pf1at)
end // end of [rarray_v_uncons]

(* ****** ****** *)

implement
rarray_v_of_array_v {a}
  (pfmul, pfarr) = aux (pfmul, pfarr) where {
  prfun aux
    {n:nat} {l:addr} {ofs:int} .<n>. (
      pfmul: MUL (n, sizeof a, ofs), pfarr: array_v (a, n, l)
    ) : rarray_v (a, n, l+ofs) =
    sif n > 0 then let
      prval (pf1arr, pf1at) = array_v_unextend {a} (pfmul, pfarr)
      prval pf1mul = mul_add_const {~1} (pfmul)
      prval pf1arr = aux (pf1mul, pf1arr)
    in
      rarray_v_cons {a} (pf1arr, pf1at)
    end else let
      prval () = array_v_unnil (pfarr)
    in
      rarray_v_nil ()
    end // end of [aux]
  // end of [aux]
} // end of [rarray_v_of_array_v]

(* ****** ****** *)

implement
array_v_of_rarray_v {a}
  (pfmul, pfarr) = aux (pfmul, pfarr) where {
  prfun aux {n:nat} {l:addr} {ofs:int} .<n>. (
      pfmul: MUL (n, sizeof a, ofs), pfarr: rarray_v (a, n, l)
    ) : array_v (a, n, l-ofs) =
    sif n > 0 then let
      prval rarray_v_cons (pf1arr, pf1at) = pfarr
      prval pf1mul = mul_add_const {~1} (pfmul)
      prval pf1arr = aux (pf1mul, pf1arr)
    in
      array_v_extend {a} (pf1mul, pf1arr, pf1at)
    end else let
      prval rarray_v_nil () = pfarr
    in
      array_v_nil {a} ()
    end // end of [if]
  // end of [aux]
} // end of [array_v_of_rarray_v]

(* ****** ****** *)

implement{a}
array2rarray_ptr
  (pfarr | p, n) = let
  val (pfmul | ofs) = mul2_size1_size1 (n, sizeof<a>)
  prval pfarr = rarray_v_of_array_v {a} (pfmul, pfarr)
in
  (pfmul, pfarr | p+ofs)
end // end of [array2rarray_ptr]

implement{a}
rarray2array_ptr
  (pfarr | p, n) = let
  val (pfmul | ofs) = mul2_size1_size1 (n, sizeof<a>)
  prval pfarr = array_v_of_rarray_v {a} (pfmul, pfarr)
in
  (pfmul, pfarr | p-ofs)
end // end of [rarray2array_ptr]

(* ****** ****** *)

implement
rarray_ptr_foreach_funenv_tsz
  {a} {v} {vt} {n}
  (pf, pfarr | p, f, asz, tsz, env) = let
  fun loop {i:nat | i <= n} {l:addr} .<i>. (
      pf: !v, pfarr: !rarray_v (a, i, l)
    | p: ptr l
    , f: (!v | &a, !vt) -<> void, i: size_t i, tsz: sizeof_t a
    , env: !vt
    ) :<> void =
    if i > 0 then let
      prval (pf1arr, pf1at) = rarray_v_uncons {a} (pfarr)
      val p1 = p-tsz
      val () = f (pf | !p1, env)
      val () = loop (pf, pf1arr | p1, f, i-1, tsz, env)
      prval () = pfarr := rarray_v_cons {a} (pf1arr, pf1at)
    in
      // nothing
    end else let
      prval () = rarray_v_unnil (pfarr)
      prval () = pfarr := rarray_v_nil {a} ()
    in
      // nothing
    end // end of [if]
in
  loop (pf, pfarr | p, f, asz, tsz, env)
end // end of [rarray_ptr_foreach_funenv_tsz]

(* ****** ****** *)

implement{a}
rarray_ptr_foreach_fun
  {v} {n} (pfarr | p, f, asz) = let
  viewtypedef fun0_t = (&a) -<fun> void
  viewtypedef fun1_t = (!unit_v | &a, !ptr) -<fun> void
  val f = __cast (f) where { extern castfn __cast (f: fun0_t):<> fun1_t }
//
  prval pfu = unit_v ()
  val () = rarray_ptr_foreach_funenv_tsz
    {a} {unit_v} {ptr} (pfu, pfarr | p, f, asz, sizeof<a>, null)
  prval unit_v () = pfu
//
in
  // nothing
end // end of [rarray_ptr_foreach_fun]

(* ****** ****** *)

implement{a}
rarray_ptr_foreach_vclo
  {v} {n} (pfv, pfarr | p, f, asz) = let
  viewtypedef clo_t = (!v | &a) -<clo> void
  stavar l_f: addr
  val p_f: ptr l_f = &f
  viewdef V = @(v, clo_t @ l_f)
  fn app (pf: !V | x: &a, p_f: !ptr l_f):<> void = let
    prval (pf1, pf2) = pf; val () = !p_f (pf1 | x) in pf := (pf1, pf2)
  end // end of [app]
  prval pf = (pfv, view@ f)
  val () = rarray_ptr_foreach_funenv_tsz
    {a} {V} {ptr l_f} (pf, pfarr | p, app, asz, sizeof<a>, p_f)
  prval (pf1, pf2) = pf
  prval () = (pfv := pf1; view@ f := pf2)
in
  // empty
end // end of [rarray_ptr_foreach_vclo]

(* ****** ****** *)

implement{a}
array_ptr_rforeach_vclo
  (pf | A, f, n) = let
  prval pfarr = view@ (A)
  val (pfmul | ofs) = mul2_size1_size1 (n, sizeof<a>)
  prval pfarr = rarray_v_of_array_v {a} (pfmul, pfarr)
  val () = rarray_ptr_foreach_vclo<a> (pf, pfarr | &A+ofs, f, n)
  prval () = view@(A) := array_v_of_rarray_v {a} (pfmul, pfarr)
in
  // nothing
end // end of [array_ptr_rforeach_vclo]

(* ****** ****** *)

(* end of [rarray.dats] *)
