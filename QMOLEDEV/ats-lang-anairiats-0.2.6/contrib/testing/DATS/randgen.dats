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

(*
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: January, 2011
**
*)

(* ****** ****** *)
//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no staloading at run-time
#define ATS_DYNLOADFLAG 0 // no dynloading at run-time

(* ****** ****** *)

staload "contrib/testing/SATS/randgen.sats"

(* ****** ****** *)

implement{a}
array0_randgen (n) = let
  val (pfgc, pfarr | p) = array_ptr_randgen<a> (n)
in
  array0_make_arrsz {a} @(pfgc, pfarr | p, n) // HX: a unary fun
end // end of [array0_randgen]

(* ****** ****** *)

implement{a}
array_randgen (n) = let
  val (pfgc, pfarr | p) = array_ptr_randgen<a> (n)
  prval () = free_gc_elim {a?} (pfgc) // HX: return the certificate
in
  array_make_view_ptr (pfarr | p)
end // end of [array_randgen]

(* ****** ****** *)

implement{a}
array_ptr_randgen (n) =
  (pfgc, pfarr | p) where {
  val (pfgc, pfarr | p) = array_ptr_alloc_tsz {a} (n, sizeof<a>)
  val () = array_ptr_randinit<a> (pfarr | p, n)
} // end of [array_ptr_randgen]

(* ****** ****** *)

implement{a}
array_ptr_randinit
  (pf | p, n) = let
  fun loop {n:nat} {l:addr} .<n>. (
    pf: !array_v (a?, n, l) >> array_v (a, n, l) | p: ptr l, n: size_t n
  ) : void =
  if n > 0 then let
    prval (pf1, pf2) =
      array_v_uncons {a?} (pf)
    val () = !p := randgen<a> ()
    val () = loop (pf2 | p+sizeof<a>, n-1)
  in
    pf := array_v_cons {a} (pf1, pf2)
  end else let
    prval () = array_v_unnil (pf)
    prval () = pf := array_v_nil ()
  in
    // nothing
  end // end of [if]
in
  loop (pf | p, n)
end // end of [array_ptr_randinit]

(* ****** ****** *)

implement{a}
list0_randgen (n) = let
  val xs = list_randgen (n) in list0_of_list1 (xs)
end // end of [list0_randgen]

(* ****** ****** *)

implement{a}
list_randgen (n) = let
  val xs = list_vt_randgen (n) in list_of_list_vt (xs)
end // end of [list_randgen]

(* ****** ****** *)

implement{a}
list_vt_randgen (n) = let
  fun loop {i,j:nat} (
    i: int i, xs: list_vt (a, j)
  ) : list_vt (a, i+j) =
    if i > 0 then let
      val x = randgen<a> () in loop (i-1, list_vt_cons (x, xs))
    end else xs // end of [if]
  // end of [loop]
in
  loop (n, list_vt_nil ())
end // end of [list_vt_randgen]

(* ****** ****** *)

implement{a}
matrix0_randgen (m, n) = let
  val (pfmul | mn) = mul2_size1_size1 (m, n)
  val (pfgc, pfarr | p) = array_ptr_randgen<a> (mn)
in
  matrix0_make_arrsz__main {a} (pfmul | m, n, (pfgc, pfarr | p, mn))
end // end of [matrix0_randgen]

(* ****** ****** *)

implement{a}
matrix_randgen (m, n) = let
  val (pfmul | mn) = mul2_size1_size1 (m, n)
  val (pfgc, pfarr | p) = array_ptr_randgen<a> (mn)
in
  matrix_make_arrsz__main {a} (pfmul | m, n, (pfgc, pfarr | p, mn))
end // end of [matrix_randgen]

(* ****** ****** *)

(*
prfun array_v_of_matrix_v {a:viewt@ype} {m,n:int} {l:addr}
  (pf_mat: matrix_v (a, m, n, l)):<> [mn:nat] (MUL (m, n, mn), array_v (a, mn, l))
*)
implement{a}
matrix_ptr_randinit (pfmat | p, m, n) = let
  prval (pfmul, pfarr) = array_v_of_matrix_v {a?} (pfmat)
  val (pfmul_alt | mn) = mul2_size1_size1 (m, n)
  prval () = mul_isfun (pfmul, pfmul_alt) // HX: there is only one [mn]
  val () = array_ptr_randinit<a> (pfarr | p, mn)
  prval () = pfmat := matrix_v_of_array_v {a} (pfmul, pfarr)
in
  // nothing
end // end of [matrix_ptr_randinit]

(* ****** ****** *)

(* end of [randgen.dats] *)
