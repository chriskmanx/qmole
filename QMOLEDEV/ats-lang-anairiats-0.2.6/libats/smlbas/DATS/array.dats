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

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

//
// SML Basis Library: Array (http://www.standardml.org/Basis/array.html)
//

(* ****** ****** *)

staload "libats/smlbas/SATS/general.sats"

(* ****** ****** *)

staload "libats/smlbas/SATS/array.sats"

(* ****** ****** *)

implement maxLen () = $raise Undefined "array:maxLen"

(* ****** ****** *)

assume array_t0ype_type
  (a:t@ype) = [n:nat] '(array (a, n), size_t n)
// end of [assume array_t0ype_type]

(* ****** ****** *)

implement{a} array (asz, ini) = let
  val [n:int] asz = size1_of_size (asz)
  val A = array_make_elt (asz, ini) in '(A, asz)
end // end of [array]  

(* ****** ****** *)

implement{a}
fromList (xs) = let
  val [n:int] xs = list1_of_list0 (xs)
  val asz = size1_of_int1 (list_length xs)
  val A = array_make_lst (asz, xs) in '(A, asz)
end // end of [fromList]

implement{a}
tabulate (asz, f) = let
  val [n:int] asz = size1_of_size (asz)
  val (
    pfgc, pfarr | p_arr
  ) = array_ptr_alloc_tsz {a} (asz, sizeof<a>)
//
  var !p_clo = @lam
    (pf: !unit_v | i: sizeLt n, x: &(a?) >> a): void =<clo> $effmask_all (x := f i)
  (* end of [var] *)
//
  prval pf = unit_v ()
  val () = array_ptr_initialize_vclo<a> {unit_v} {n} (pf | !p_arr, asz, !p_clo)
  prval unit_v () = pf
  prval () = free_gc_elim {a?} (pfgc)
  val A = array_make_view_ptr (pfarr | p_arr)
in
  '(A, asz)
end // end of [tabulate]

(* ****** ****** *)

implement length (A) = A.1

(* ****** ****** *)

implement{a} sub (A, i) = let
  val i = size1_of_size (i) in
  if i < A.1 then array_get_elt_at (A.0, i) else $raise Subscript ()
end // end of [sub]
  
implement{a} update (A, i, x) = let
  val i = size1_of_size (i) in
  if i < A.1 then array_set_elt_at (A.0, i, x) else $raise Subscript ()
end // end of [sub]

(* ****** ****** *)

implement{a} copy
  ([n1:int] src, [n2:int] dst, di) = let
  val [di:int] di = size1_of_size (di)
  val asz1 = src.1 and asz2 = dst.1
  val _src = src.0 and _dst = dst.0
in
  if (di + asz1) <= asz2 then let
    var i: size_t = size1_of_int1 0 and j: size_t = di
  in
    while* {i:nat}
      (i: size_t i, j: size_t (di + i)) =>
      (i < asz1) begin
      _dst[j] := _src[i]; i := i + 1; j := j + 1
    end // end of [while]
  end else
    $raise Subscript ()
  // end of [if]
end (* end of [copy] *)
  
(* ****** ****** *)

implement{a} app (f, [n:int] A) = () where {
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  viewdef v = unit_v; viewdef vt = (a) -<cloref1> void 
  prval pf = unit_v ()
  fn _app
    (pf: !v | x: &a, f: !vt):<> void = $effmask_all (f x)
  val () = array_ptr_foreach_funenv_tsz {a} {v} {vt} (pf | !p_arr, _app, A.1, sizeof<a>, f)
  prval unit_v () = pf
} // end of [app]

implement{a} appi (f, [n:int] A) = () where {
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  viewdef v = unit_v; viewdef vt = (size_t, a) -<cloref1> void 
  prval pf = unit_v ()
  fn _app
    (pf: !v | i: sizeLt n, x: &a, f: !vt):<> void = $effmask_all (f (i, x))
  val () = array_ptr_iforeach_funenv_tsz {a} {v} {vt} (pf | !p_arr, _app, A.1, sizeof<a>, f)
  prval unit_v () = pf
} // end of [appi]

(* ****** ****** *)

implement{a} modify (f, [n:int] A) = () where {
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  viewdef v = unit_v; viewdef vt = (a) -<cloref1> a
  prval pf = unit_v ()
  fn _app (pf: !v | x: &a, f: !vt):<> void = x := $effmask_all (f (x))
  val () = array_ptr_foreach_funenv_tsz {a} {v} {vt} (pf | !p_arr, _app, A.1, sizeof<a>, f)
  prval unit_v () = pf
} // end of [modify]

implement{a} modifyi (f, [n:int] A) = () where {
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  viewdef v = unit_v; viewdef vt = (size_t, a) -<cloref1> a
  prval pf = unit_v ()
  fn _app (pf: !v | i: sizeLt n, x: &a, f: !vt):<> void = x := $effmask_all (f (i, x))
  val () = array_ptr_iforeach_funenv_tsz {a} {v} {vt} (pf | !p_arr, _app, A.1, sizeof<a>, f)
  prval unit_v () = pf
} // end of [modifyi]

(* ****** ****** *)

implement{a,b}
  foldl (f, ini, [n:int] A) = res where {
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  val asz = A.1
  var res: b = ini
  var i: sizeLte n // uninitialized
  val _0: size_t = size1_of_int1 (0)
  val () = begin
    for (i := _0; i < asz; i := i + 1) (res := $effmask_all (f (p_arr->[i], res)))
  end // end of [val]  
} // end of [foldl]

implement{a,b}
  foldli (f, ini, [n:int] A) = res where {
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  val asz = A.1
  var res: b = ini
  var i: sizeLte n // uninitialized
  val _0: size_t = size1_of_int1 (0)
  val () = begin
    for (i := _0; i < asz; i := i + 1) (res := $effmask_all (f (i, p_arr->[i], res)))
  end // end of [val]  
} // end of [foldl]

(* ****** ****** *)

implement{a,b}
  foldr (f, snk, [n:int] A) = res where {
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  var res: b = snk
  var i: sizeLte n = A.1
  val () = while (i > 0) let
    val () = i := i - 1 in res := $effmask_all (f (p_arr->[i], res))
  end // end of [val]  
} // end of [foldr]

implement{a,b}
  foldri (f, snk, [n:int] A) = res where {
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  var res: b = snk
  var i: sizeLte n = A.1
  val () = while (i > 0) let
    val () = i := i - 1 in res := $effmask_all (f (i, p_arr->[i], res))
  end // end of [val]  
} // end of [foldri]

(* ****** ****** *)

implement{a} find (f, [n:int] A) = res where {
  val asz = A.1
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  var i: sizeLte n // uninitialzed ()
  var res: option0 a = option0_none ()
  val _0 = size1_of_int1 (0)
  val () = for (i := _0; i < asz; i := i + 1) let
    val x = p_arr->[i]; val found = $effmask_all (f x)
    val () = if found then (res := option0_some x; break) else ()
  in
    // empty
  end // end of [val]
} // end of [find]

implement{a} findi (f, [n:int] A) = res where {
  val asz = A.1
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  var i: sizeLte n // uninitialzed ()
  var res: option0 @(size_t, a) = option0_none ()
  val _0 = size1_of_int1 (0)
  val () = for (i := _0; i < asz; i := i + 1) let
    val x = p_arr->[i]; val found = $effmask_all (f (i, x))
    val () = if found then (res := option0_some @(i, x); break) else ()
  in
    // empty
  end // end of [val]
} // end of [findi]

(* ****** ****** *)

implement{a} all (f, [n:int] A) = res where {
  val asz = A.1
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  var i: sizeLte n // uninitialzed ()
  var res: bool = true
  val _0 = size1_of_int1 (0)
  val () = for (i := _0; i < asz; i := i + 1) let
    val x = p_arr->[i]; val found = $effmask_all (f x)
(*
    // this does not work because of  a bug!
    val () = if found then () else (res := false; break)
*)
    val () = if :(i: sizeLt n) => found then () else (res := false; break)
  in
    // empty
  end // end of [val]
} // end of [all]

implement{a} exists (f, [n:int] A) = res where {
  val asz = A.1
  val (vbox pfarr | p_arr) = array_get_view_ptr (A.0)
  var i: sizeLte n // uninitialzed ()
  var res: bool = false
  val _0 = size1_of_int1 (0)
  val () = for (i := _0; i < asz; i := i + 1) let
    val x = p_arr->[i]; val found = $effmask_all (f x)
    val () = if :(i: sizeLt n) => found then (res := true; break) else ()
  in
    // empty
  end // end of [val]
} // end of [exists]

(* ****** ****** *)

implement{a} collate
  (cmp, [n1:int] A1, [n2:int] A2) = let
  fun loop {i:nat | i <= min(n1,n2) } .<n1-i>. (
      cmp: (a, a) -<cloref1> int 
    , _A1: array (a, n1), n1: size_t n1 
    , _A2: array (a, n2), n2: size_t n2
    , i: size_t i
    ) :<cloref1> int = case+ 0 of
    | _ when i < n1 =>
        if i < n2 then let
          val sgn = cmp (_A1[i], _A2[i]) in
          if sgn = 0 then loop (cmp, _A1, n1, _A2, n2, i+1) else sgn
        end else 1
    | _ (* i = n1 *) => if i < n2 then ~1 else 0
  // end of [loop]  
in
  loop (cmp, A1.0, A1.1, A2.0, A2.1, 0)
end // end of [val]

(* ****** ****** *)

(* end of [array.dats] *)
