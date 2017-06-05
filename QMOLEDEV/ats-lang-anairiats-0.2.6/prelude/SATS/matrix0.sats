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
// HX:
//
// Note that [matrix0] is a persistent array with row/column information
// attached.
//
// This package is mostly for a beginner who is unfamiliar with ATS. After
// some exposure to dependent types and linear types, the programmer is
// strongly recommended to use functions declared in [matrix.sats] instead.
//

(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [matrix0.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)
//
// HX: matrix0 (a) = ref (... | ptr, row, col)
//
(* ****** ****** *)

fun matrix0_make_arrsz
  {a:viewt@ype} {m,n:nat} (
  m: size_t m, n: size_t n, arrsz: arraysize (a, m*n)
  ) :<> matrix0 (a)
// end of [matrix0_make_arrsz]

fun matrix0_make_arrsz__main
  {a:viewt@ype} {m,n:nat} {mn:int} (
    pf_mul: MUL (m, n, mn)
  | m: size_t m, n: size_t n, arrsz: arraysize (a, mn)
  ) :<> matrix0 (a)
// end of [matrix0_make_arrsz]

(* ****** ****** *)

fun{a:t@ype}
matrix0_make_elt (
  row: size_t, col: size_t, x: a
) :<> matrix0 (a) // end of [matrix0_make_elt]

(* ****** ****** *)

fun matrix0_row {a:t@ype} (M: matrix0 a):<!ref> size_t
fun matrix0_col {a:t@ype} (M: matrix0 a):<!ref> size_t

(* ****** ****** *)

fun{a:t@ype} matrix0_get_elt_at
  (M: matrix0 a, i: size_t, j: size_t):<!exnref> a
overload [] with matrix0_get_elt_at

fun{a:t@ype} matrix0_set_elt_at
  (M: matrix0 a, i: size_t, j: size_t, x: a):<!exnref> void
overload [] with matrix0_set_elt_at

(* ****** ****** *)

fun{a:t@ype} matrix0_get_elt_at__intsz
  (M: matrix0 a, i: int, j: int):<!exnref> a
overload [] with matrix0_get_elt_at__intsz

fun{a:t@ype} matrix0_set_elt_at__intsz
  (M: matrix0 a, i: int, j: int, x: a):<!exnref> void
overload [] with matrix0_set_elt_at__intsz

(* ****** ****** *)

// HX: it is done row-by-row
fun{a:t@ype} matrix0_foreach
  (M: matrix0 a, f: (&a) -<cloref> void):<!ref> void
// end of [matrix0_foreach]

// HX: it is done row-by-row
fun{a:t@ype} matrix0_iforeach
  (M: matrix0 a, f: (size_t, size_t, &a) -<cloref> void):<!ref> void
// end of [matrix0_iforeach]

(* ****** ****** *)
//
// HX: [row] and [col] are assumed to be nonzero
//
fun{a:t@ype} matrix0_tabulate // M[i,j] := f(i, j)
  (row: size_t, col: size_t, f: (size_t, size_t) -<cloref> a):<> matrix0 a
// end of [matrix0_tabulate]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [matrix0.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [matrix0.sats] *)
