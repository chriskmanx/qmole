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

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

#define i2sz size1_of_int1

(* ****** ****** *)
//
// HX: array0 implementation
//
(* ****** ****** *)

staload "prelude/SATS/array0.sats"
staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

assume array0_viewt0ype_type
  (a:viewt@ype) = ref (Arraysize a)
// end of [array0_viewt0ype_type]

(* ****** ****** *)

(*
//
// HX-2011-01-12:
// it is a casting function now!
//
implement array0_get_arrszref (A) = A
*)
implement array0_make_arrsz (arrsz) = ref_make_elt (arrsz)

(* ****** ****** *)

implement{a}
array0_make_elt (asz, x0) = let
  val [n:int] asz = size1_of_size asz
  val tsz = sizeof<a>
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {a} (asz, tsz)
  var ini: a = x0
  val () = array_ptr_initialize_elt_tsz {a} (!p_arr, asz, ini, tsz)
in
  array0_make_arrsz {a} {n} @(pf_gc, pf_arr | p_arr, asz)
end // end of [array0_make_elt]

(* ****** ****** *)

implement{a}
array0_make_lst (xs) = let
  val [n:int] xs = list1_of_list0 (xs)
  val n = list_length (xs)
  val asz = size1_of_int1 (n)
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {a} (asz, sizeof<a>)
  val () = array_ptr_initialize_lst<a> (!p_arr, xs)
in
  array0_make_arrsz {a} {n} @(pf_gc, pf_arr | p_arr, asz)
end // end of [array_make_lst]

(* ****** ****** *)

implement
array0_size (A) = let
  val (vbox pf_arrsz | p_arrsz) = ref_get_view_ptr (A)
in
  p_arrsz->3
end // end of [array0_size]

(* ****** ****** *)

implement{a}
array0_get_elt_at (A, i) = let
  val (vbox pf_arrsz | p_arrsz) = ref_get_view_ptr (A)
  val i = size1_of_size i
  val p_data = p_arrsz->2; val asz = p_arrsz->3
in
  if i < asz then let
    prval pf_data = p_arrsz->1
    val x = p_data->[i]
    prval () = p_arrsz->1 := pf_data
  in
    x // return value
  end else begin
    $raise ArraySubscriptException ()
  end // end of [if]
end (* end of [array0_get_elt_at] *)

implement{a} array0_set_elt_at (A, i, x) = let
  val (vbox pf_arrsz | p_arrsz) = ref_get_view_ptr (A)
  val i = size1_of_size i
  val p_data = p_arrsz->2; val asz = p_arrsz->3
in
  if i < asz then let
    prval pf_data = p_arrsz->1
    val () = p_data->[i] := x
    prval () = p_arrsz->1 := pf_data
  in
    () // return no value
  end else begin
    $raise ArraySubscriptException ()
  end // end of [if]
end (* end of [array0_set_elt_at] *)

(* ****** ****** *)

implement{a}
array0_get_elt_at__intsz (A, i) = let
  val i = int1_of_int i
in
  if i >= 0 then begin
    array0_get_elt_at<a> (A, i2sz i)
  end else begin
    $raise ArraySubscriptException ()
  end // end of [if]
end (* end of [array0_get_elt_at__intsz] *)
  
implement{a}
array0_set_elt_at__intsz (A, i, x) = let
  val i = int1_of_int i
in
  if i >= 0 then begin
    array0_set_elt_at<a> (A, i2sz i, x)
  end else begin
    $raise ArraySubscriptException ()
  end // end of [if]
end (* end of [array0_set_elt_at__intsz] *)
  
(* ****** ****** *)

implement{a}
array0_foreach (A, f) = let
  fun loop {n:nat} {l:addr} .<n>. (
      pf: !array_v (a, n, l)
    | p: ptr l, n: size_t n, f: (&a) -<cloref> void
    ) :<> void =
    if n > 0 then let
      prval (pf1, pf2) = array_v_uncons {a} (pf)
      val () = f (!p)
      val () = loop (pf2 | p+sizeof<a>, n-1, f)
    in
      pf := array_v_cons {a} (pf1, pf2)
    end // end of [if]
  // end of [loop]
  val (vbox pf_arrsz | p_arrsz) = ref_get_view_ptr (A)
in
  loop (p_arrsz->1 | p_arrsz->2, p_arrsz->3, f)
end // end of [array0_foreach]

implement{a}
array0_iforeach (A, f) = let
  val (vbox pf_arrsz | p_arrsz) = ref_get_view_ptr (A)
  stavar n0: int
  val n0: size_t n0 = p_arrsz->3
  fun loop {n,i:nat | n0==n+i} {l:addr} .<n>. (
      pf: !array_v (a, n, l)
    | p: ptr l, n: size_t n, i: size_t i, f: (size_t, &a) -<cloref> void
    ) :<> void =
    if n > 0 then let
      prval (pf1, pf2) = array_v_uncons {a} (pf)
      val () = f (i, !p)
      val () = loop {n-1,i+1} (pf2 | p+sizeof<a>, n-1, i+1, f)
    in
      pf := array_v_cons {a} (pf1, pf2)
    end // end of [if]
  // end of [loop]
in
  loop (p_arrsz->1 | p_arrsz->2, n0, 0, f)
end // end of [array0_iforeach]

(* ****** ****** *)

implement{a}
array0_tabulate (asz, f) = let
  val [n0:int] asz = size1_of_size asz
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {a} (asz, sizeof<a>)
  fun loop {n,i:nat | n0 == n+i} {l:addr} .<n>. (
      pf: !array_v (a?, n, l) >> array_v (a, n, l)
    | p: ptr l, n: size_t n, i: size_t i, f: size_t -<cloref> a
    ) :<> void =
    if n > 0 then let
      prval (pf1, pf2) = array_v_uncons {a?} (pf)
      val () = !p := f (i)
      val () = loop (pf2 | p+sizeof<a>, n-1, i+1, f)
    in
      pf := array_v_cons {a} (pf1, pf2)
    end else let
      prval () = array_v_unnil {a?} (pf)
    in
      pf := array_v_nil {a} ()
    end // end of [if]
  // end of [loop]
  val () = loop (pf_arr | p_arr, asz, 0, f)
in
  array0_make_arrsz {a} {n0} @(pf_gc, pf_arr | p_arr, asz)
end // end of [array0_tabulate]

(* ****** ****** *)

(* end of [array0.dats] *)
