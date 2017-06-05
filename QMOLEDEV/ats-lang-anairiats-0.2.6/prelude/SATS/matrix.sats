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

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [matrix.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)
//
// HX: [mtrxt] is row-major
//
absviewt@ype
mtrxt // flat matrix type
  (a:viewt@ype+, row:int, col:int) = @[@[a][col]][row]
// end of [mtrxt]

viewdef matrix_v
  (a:viewt@ype, m:int, n:int, l:addr) = mtrxt (a, m, n) @ l
// end of [matrix_v]

(* ****** ****** *)

prfun array_v_of_matrix_v
  {a:viewt@ype} {m,n:nat} {l:addr} (
  pf_mat: matrix_v (a, m, n, l)
) :<> [mn:nat] (MUL (m, n, mn), array_v (a, mn, l))

prfun matrix_v_of_array_v
  {a:viewt@ype} {m,n:nat} {mn:int} {l:addr} (
  pf_mul: MUL (m, n, mn), pf_arr: array_v (a, mn, l)
) :<> matrix_v (a, m, n, l)

(* ****** ****** *)

fun{a:viewt@ype}
matrix_ptr_takeout_row
  {m,n:int} {i:int | i < m} {l0:addr} (
  pf_mat: matrix_v (a, m, n, l0)
| base: ptr l0, i: size_t i, n: size_t n
) :<> [l:addr] (
  array_v (a, n, l)
, array_v (a, n, l) -<lin,prf> matrix_v (a, m, n, l0)
| ptr l
) // end of [matrix_ptr_takeout_row]

fun matrix_ptr_takeout_row_tsz
  {a:viewt@ype}
  {m,n:int} {i:int | i < m} {l0:addr} (
  pf_mat: matrix_v (a, m, n, l0)
| base: ptr l0, i: size_t i, n: size_t n, tsz: sizeof_t a
) :<> [l:addr] (
  array_v (a, n, l)
, array_v (a, n, l) -<lin,prf> matrix_v (a, m, n, l0)
| ptr l
) = "atspre_matrix_ptr_takeout_row_tsz"

(*
//
// HX: this is not a completely safe version, but ...
//
fun{a:viewt@ype}
matrix_ptr_vtakeout_row
  {m,n:int}
  {i:nat | i < m}
  {l0:addr} (
  pf_mat: matrix_v (a, m, n, l0)
| base: ptr l0
, i: size_t i
, n: size_t n
) : [l:addr] (
  array_v (a, n, l)
, array_v (a, n, l) -<> void
| ptr l // l = l0 + i*n*sizeof<a>
) // end of [matrix_ptr_vtakeout_row]
*)

(* ****** ****** *)

fun{a:viewt@ype}
matrix_ptr_takeout_elt
  {m,n:int} {i,j:nat | i < m; j < n} {l0:addr} (
  pf_mat: matrix_v (a, m, n, l0)
| base: ptr l0, i: size_t i, n: size_t n, j: size_t j
) :<> [l:addr] (
  a @ l
, a @ l -<lin,prf> matrix_v (a, m, n, l0)
| ptr l
) // end of [matrix_ptr_takeout_elt]

fun matrix_ptr_takeout_elt_tsz
  {a:viewt@ype}
  {m,n:int} {i,j:nat | i < m; j < n} {l0:addr} (
  pf_mat: matrix_v (a, m, n, l0)
| base: ptr l0, i: size_t i, n: size_t n, j: size_t j, tsz: sizeof_t a
) :<> [l:addr] (
  a @ l
, a @ l -<lin,prf> matrix_v (a, m, n, l0)
| ptr l
) = "atspre_matrix_ptr_takeout_elt_tsz"

(* ****** ****** *)

(*
**
** persistent matrices
**
*)

(* ****** ****** *)

exception MatrixSubscriptException of ()

(* ****** ****** *)

fun matrix_make_arrsz
  {a:viewt@ype} {m,n:nat} (
  m: size_t m, n: size_t n, arrsz: arraysize (a, m*n)
) :<> matrix (a, m, n) // end of [matrix_make_arrsz]

macdef matrix (m, n) (asz) =
  matrix_make_arrsz (,(m), ,(n), ,(asz))
// end of [macdef]

//
// HX: implemented in [prelude/DATS/matrix.dats]
//
fun matrix_make_arrsz__main
  {a:viewt@ype} {m,n:nat} {mn:int}
  (pf: MUL (m, n, mn) | m: size_t m, n: size_t n, arrsz: arraysize (a, mn))
  :<> matrix (a, m, n)
  = "atspre_matrix_make_arrsz__main"
// end of [matrix_make_arrsz__main]

(* ****** ****** *)

fun{a:t@ype}
matrix_make_elt {m,n:pos}
  (row: size_t m, col: size_t n, elt: a):<> matrix (a, m, n)
// end of [matrix_make_elt]

(* ****** ****** *)
//
// HX: implemented in [prelude/DATS/matrix.dats]
//
fun matrix_make_funenv_tsz
  {a:viewt@ype}
  {v:view} {vt:viewtype}
  {m,n:pos} {f:eff} (
  pf: !v
| row: size_t m, col: size_t n
, f: (!v | sizeLt m, sizeLt n, &(a?) >> a, !vt) -<fun,f> void
, tsz: sizeof_t a
, env: !vt
) :<f> matrix (a, m, n)
  = "atspre_matrix_make_funenv_tsz"
// end of [fun]

fun{a:viewt@ype}
matrix_make_fun
  {m,n:pos} {f:eff} (
  row: size_t m, col: size_t n
, f: (sizeLt m, sizeLt n, &(a?) >> a) -<fun,f> void
) :<f> matrix (a, m, n) // end of [matrix_make_fun_tsz]

fun{a:viewt@ype}
matrix_make_vclo
  {v:view}
  {m,n:pos} {f:eff} (
  pfv: !v
| row: size_t m, col: size_t n
, f: &(!v | sizeLt m, sizeLt n, &(a?) >> a) -<clo,f> void
) :<f> matrix (a, m, n) // end of [matrix_make_vclo]

(* ****** ****** *)

fun{a:t@ype}
matrix_get_elt_at {m,n:int} {i,j:nat | i < m; j < n}
  (A: matrix (a, m, n), i: size_t i, n: size_t n, j: size_t j):<!ref> a
overload [] with matrix_get_elt_at

fun{a:t@ype}
matrix_set_elt_at {m,n:int} {i,j:nat | i < m; j < n}
  (A: matrix (a, m, n), i: size_t i, n: size_t n, j: size_t j, x: a):<!ref> void
overload [] with matrix_set_elt_at

(* ****** ****** *)

fun{a:t@ype}
matrix_get_elt_at__intsz {m,n:int} {i,j:nat | i < m; j < n}
  (A: matrix (a, m, n), i: int i, n: int n, j: int j):<!ref> a
overload [] with matrix_get_elt_at__intsz

fun{a:t@ype}
matrix_set_elt_at__intsz {m,n:int} {i,j:nat | i < m; j < n}
  (A: matrix (a, m, n), i: int i, n: int n, j: int j, x: a):<!ref> void
overload [] with matrix_set_elt_at__intsz

(* ****** ****** *)
//
// HX:
// these foreach-functions are just as easy to be
// implemented on the spot
//
(* ****** ****** *)
(*
** HX: implemented in ATS (prelude/DATS/matrix.dats)
*)
fun{a:viewt@ype}
matrix_foreach_funenv
  {v:view} {vt:viewtype} {m,n:nat} (
  pf: !v
| M: matrix (a, m, n)
, f: (!v | &a, !vt) -<fun> void, m: size_t m, n: size_t n
, env: !vt
) :<!ref> void
// end of [matrix_foreach_funenv]

fun{a:viewt@ype}
matrix_foreach_fun {m,n:nat} (
  M: matrix (a, m, n)
, f: (&a) -<fun> void, m: size_t m, n: size_t n
) :<!ref> void // end of [matrix_foreach_fun]

fun{a:viewt@ype}
matrix_foreach_vclo {v:view} {m,n:nat} (
  pf: !v
| M: matrix (a, m, n)
, f: &(!v | &a) -<clo> void, m: size_t m, n: size_t n
) :<!ref> void // end of [matrix_foreach_vclo]

fun{a:viewt@ype}
matrix_foreach_cloref {m,n:nat} (
  M: matrix (a, m, n), f: (&a) -<cloref> void, m: size_t m, n: size_t n
) :<!ref> void // end of [matrix_foreach_cloref]

(* ****** ****** *)
//
// HX:
// these iforeach-functions are just as easy to be
// implemented on the spot
//
(* ****** ****** *)
(*
** HX: implemented in ATS (prelude/DATS/matrix.dats)
*)
fun{a:viewt@ype}
matrix_iforeach_funenv
  {v:view} {vt:viewtype} {m,n:nat} (
  pf: !v
| M: matrix (a, m, n)
, f: (!v | sizeLt m, sizeLt n, &a, !vt) -<fun> void, m: size_t m, n: size_t n
, env: !vt
) :<!ref> void // end of [matrix_iforeach_funenv]

fun{a:viewt@ype}
matrix_iforeach_fun {m,n:nat} (
 M: matrix (a, m, n)
, f: (sizeLt m, sizeLt n, &a) -<fun> void, m: size_t m, n: size_t n
) :<!ref> void // end of [matrix_iforeach_fun]

fun{a:viewt@ype}
matrix_iforeach_vclo {v:view} {m,n:nat} (
  pf: !v
| M: matrix (a, m, n)
, f: &(!v | sizeLt m, sizeLt n, &a) -<clo> void, m: size_t m, n: size_t n
) :<!ref> void // end of [matrix_iforeach_vclo]

fun{a:viewt@ype}
matrix_iforeach_cloref {m,n:nat} (
  M: matrix (a, m, n)
, f: (sizeLt m, sizeLt n, &a) -<cloref1> void, m: size_t m, n: size_t n
) :<fun1> void // end of [matrix_iforeach_cloref]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [matrix.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [matrix.sats] *)
