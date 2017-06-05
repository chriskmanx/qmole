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
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
**
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

staload UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

staload "contrib/testing/SATS/fprint.sats"

(* ****** ****** *)

implement{a}
array0_fprint_elt
  (out, A, sep) =
  array0_fprint_fun (out, A, sep, fprint_elt<a>)
// end of [array0_fprint_elt]

implement{a}
array0_fprint_fun
  (out, A, sep, pr) = () where {
  val r = array0_get_arrszref {a} (A)
(*
castfn ref_get_view_ptr
  {a:viewt@ype} (r: ref a):<> [l:addr] (vbox (a @ l) | ptr l)
*)
  val (vbox pf | p) = ref_get_view_ptr (r)
  prval pfarr = p->1; val parr = p->2; val n = p->3
  val () = $effmask_ref (array_ptr_fprint_fun (out, !parr, n, sep, pr))
  prval () = p->1 := pfarr
} // end of [array0_fprint_fun]

(* ****** ****** *)

implement{a}
array_fprint_elt
  (out, A, n, sep) =
  array_fprint_fun (out, A, n, sep, fprint_elt<a>)
// end of [array_fprint_elt]

implement{a}
array_fprint_fun
  (out, A, n, sep, pr) = let
(*
castfn array_get_view_ptr
  {a:viewt@ype} {n:nat}
  (A: array (a, n)):<> [l:addr] (vbox (array_v (a, n, l)) | ptr l)
*)
  val (vbox pf | p) = array_get_view_ptr (A)
in
  $effmask_ref (array_ptr_fprint_fun (out, !p, n, sep, pr))
end // end of [array_fprint_fun]

(* ****** ****** *)

implement{a}
array_ptr_fprint_elt
  (out, A, n, sep) =
  array_ptr_fprint_fun (out, A, n, sep, fprint_elt<a>)
// end of [array_ptr_fprint_elt]

implement{a}
array_ptr_fprint_fun
  {n} (out, A, n, sep, pr) = let
  val sep = $UN.castvwtp1{string} (sep)
  var i: sizeLte (n); val _0 = size1_of_int1 (0)
in
  for (i := _0; i < n; i := i+1) (
    if i > 0 then fprint_string (out, sep); pr (out, A.[i])
  ) // end of [for]
end // end of [array_ptr_fprint_fun]

(* ****** ****** *)

implement{a}
list0_fprint_elt (out, xs, sep) =
  list_fprint_elt (out, list1_of_list0 xs, sep)
// end of [list0_fprint_elt]

implement{a}
list0_fprint_fun (out, xs, sep, pr) =
  list_fprint_fun (out, list1_of_list0 xs, sep, pr)
// end of [list0_fprint_fun]

(* ****** ****** *)

implement{a}
list_fprint_elt
  (out, xs, sep) =
  list_fprint_fun<a> (out, xs, sep, fprint_elt<a>)
// end of [list_fprint_elt]

implement{a}
list_fprint_fun
  (out, xs, sep, pr) = let
  val sep = $UN.castvwtp1{string} (sep)
  fun loop (
    xs: List a, i: int
  ) :<cloref1> void =
    case+ xs of
    | list_cons (x, xs) => let
        val () =
          if i > 0 then fprint_string (out, sep)
        // end of [val]
        val () = pr (out, x)
      in
        loop (xs, i+1)
      end // end of [list_cons]
    | list_nil () => ()
  // end of [loop]
in
  loop (xs, 0)
end // end of [list_fprint_fun]

(* ****** ****** *)

implement{a}
list_vt_fprint_elt
  (out, xs, sep) = (
  list_fprint_elt<a> (out, $UN.castvwtp1 {List a} (xs), sep)
) // end of [list_vt_fprint_elt]

implement{a}
list_vt_fprint_fun
  (out, xs, sep, pr) = (
  list_fprint_fun<a> (out, $UN.castvwtp1 {List a} (xs), sep, pr)
) // end of [list_vt_fprint_fun]

(* ****** ****** *)

implement{a}
matrix0_fprint_elt
  (out, M, sep1, sep2) = (
  matrix0_fprint_fun (out, M, sep1, sep2, fprint_elt<a>)
) // end of [matrix0_fprint_elt]

implement{a}
matrix0_fprint_fun
  (out, M, sep1, sep2, pr) = let
  val sep1 = $UN.castvwtp1{string} (sep1)
  val sep2 = $UN.castvwtp1{string} (sep2)
  val m = matrix0_row (M) and n = matrix0_col (M)
  var !p_clo = @lam (
    i: size_t, j: size_t, x: &a
  ) : void =<clo> $effmask_all {
    val () = if j > 0 then fprint_string (out, sep1)
    val () = pr (out, x)
    val () = if j = n-1 then fprint_string (out, sep2)
  } // end of [var]
  val () = matrix0_iforeach (M, $UN.cast (p_clo))
in
  // nothing
end // end of [matrix0_fprint_fun]

(* ****** ****** *)

implement{a}
matrix_fprint_elt {m,n}
  (out, M, m, n, sep1, sep2) = (
  matrix_fprint_fun (out, M, m, n, sep1, sep2, fprint_elt<a>)
) // end of [matrix_fprint_elt]

implement{a}
matrix_fprint_fun {m,n}
  (out, M, m, n, sep1, sep2, pr) = let
  val sep1 = $UN.castvwtp1{string} (sep1)
  val sep2 = $UN.castvwtp1{string} (sep2)
  var !p_clo = @lam (
    pf: !unit_v | i: sizeLt m, j: sizeLt n, x: &a
  ) : void =<clo> $effmask_all {
    val () = if j > 0 then fprint_string (out, sep1)
    val () = pr (out, x)
    val () = if j = n-1 then fprint_string (out, sep2)
  } // end of [var]
  prval pfu = unit_v ()
  val () = matrix_iforeach_vclo (pfu | M, !p_clo, m, n)
  prval unit_v () = pfu
in
  // nothing
end // end of [matrix_ptr_fprint_fun]

(* ****** ****** *)

implement{a}
matrix_ptr_fprint_elt {m,n}
  (pf | out, p, m, n, sep1, sep2) =
  matrix_fprint_elt (out, $UN.cast (p), m, n, sep1, sep2)
// end of [matrix_ptr_fprint_elt]

implement{a}
matrix_ptr_fprint_fun {m,n}
  (pf | out, p, m, n, sep1, sep2, pr) =
  matrix_fprint_fun (out, $UN.cast (p), m, n, sep1, sep2, pr)
// end of [matrix_ptr_fprint_fun]

(* ****** ****** *)

(* end of [fprint.dats] *)
