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
** Fortran matrices: column-major representation
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

%{#
#include "libats/CATS/fmatrix.cats"
%} // end of [%{#]

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"

(* ****** ****** *)
//
// HX: [fmatrix] is column-major
//
absviewt@ype
fmatrix_viewt0ype_int_int_viewt0ype
  (a:viewt@ype+,row:int,col:int) = @[@[a][row]][col]
stadef fmatrix =
  fmatrix_viewt0ype_int_int_viewt0ype

viewdef
fmatrix_v (
  a:viewt@ype, row:int, col:int, l:addr
) = fmatrix (a, row, col) @ l

(* ****** ****** *)

prfun array_v_of_fmatrix_v
  {a:viewt@ype} {m,n:int} {l:addr}
  (pf_mat: fmatrix_v (a, m, n, l))
  :<> [mn:int] (MUL (m, n, mn), array_v (a, mn, l))
// end of [array_v_of_fmatrix_v]

prfun fmatrix_v_of_array_v
  {a:viewt@ype} {m,n:int} {mn:int} {l:addr}
  (pf_mul: MUL (m, n, mn), pf_arr: array_v (a, mn, l))
  :<> fmatrix_v (a, m, n, l)
// end of [fmatrix_v_of_array_v]

(* ****** ****** *)

fun{a:viewt@ype}
fmatrix_ptr_alloc
  {m,n:nat} (
  m: size_t m, n: size_t n
) :<> [mn:nat] [l:agz] (
  free_gc_v (a?, mn, l)
, MUL (m, n, mn)
, fmatrix_v (a?, m, n, l)
| ptr l
) // end of [fmatrix_ptr_alloc]

(* ****** ****** *)

fun fmatrix_ptr_free
  {a:viewt@ype}
  {m,n,mn:nat} {l:addr} (
  pf_gc: free_gc_v (a?, mn, l)
, pf_mn: MUL (m, n, mn)
, pf_fmat: fmatrix_v (a?, m, n, l)
| p: ptr l
) :<> void // end of [fmatrix_ptr_free]

(* ****** ****** *)

fun{a:viewt@ype}
fmatrix_ptr_allocfree
  {m,n:nat} (
  m: size_t m, n: size_t n
) :<> [l:agz] (
  fmatrix_v (a?, m, n, l)
| ptr l, (fmatrix_v (a?, m, n, l) | ptr l) -<lin> void
) // end of [fmatrix_ptr_allocfree]

(* ****** ****** *)

fun{a:t@ype}
fmatrix_ptr_initialize_elt
  {m,n:nat} {l:addr} (
  base: &fmatrix (a?, m, n) >> fmatrix (a, m, n)
, m: size_t m, n: size_t n, x: a
) :<> void // end of [fmatrix_initialize_elt]

(* ****** ****** *)

fun{a:viewt@ype}
fmatrix_ptr_initialize_vclo
  {v:view} {m,n:nat} (
  pf: !v 
| base: &fmatrix (a?, m, n) >> fmatrix (a, m, n)
, m: size_t m, n: size_t n
, f: &(!v | &(a?) >> a, sizeLt m, sizeLt n) -<clo> void
) :<> void // end of [fmatrix_ptr_initialize_vclo]

(* ****** ****** *)

fun{a:viewt@ype}
fmatrix_ptr_takeout
  {m,n:int}
  {i,j:nat | i < m; j < n}
  {l0:addr} (
  pf_mat: fmatrix_v (a, m, n, l0)
| base: ptr l0
, m: size_t m
, i: size_t i, j: size_t j
) :<> [l:addr] (
  a @ l
, a @ l -<lin,prf> fmatrix_v (a, m, n, l0)
| ptr l
) // end of [fmatrix_ptr_takeout]

(* ****** ****** *)

fun{a:t@ype}
fmatrix_ptr_get_elt_at
  {m,n:int} {i,j:nat | i < m; j < n} (
  base: &fmatrix (a, m, n), m: size_t m, i: size_t i, j: size_t j
) :<> a // end of [fmatrix_ptr_ptr_get_elt_at]

fun{a:t@ype}
fmatrix_ptr_set_elt_at
  {m,n:int} {i,j:nat | i < m; j < n} (
  base: &fmatrix (a, m, n), m: size_t m, i: size_t i, j: size_t j, x: a
) :<> void // end of [fmatrix_ptr_ptr_set_elt_at]

overload [] with fmatrix_ptr_get_elt_at
overload [] with fmatrix_ptr_set_elt_at

(* ****** ****** *)

fun{a:t@ype}
fmatrix_ptr_copy {m,n:nat} (
  A: &fmatrix(a, m, n)
, B: &fmatrix(a?, m, n) >> fmatrix(a, m, n)
, m: size_t m, n: size_t n
) : void // end of [fmatrix_ptr_copy]

(* ****** ****** *)

prfun GEVEC_v_of_fmatrix_v
  {a1:viewt@ype} {m,n:nat} {mn:int} {l:addr} (
  pf_mul: MUL (m, n, mn), pf_mat: fmatrix_v (a1, m, n, l)
) :<> (
  GEVEC_v (a1, mn, 1, l)
, {a2:viewt@ype | a1 \tszeq a2}
    GEVEC_v (a2, mn, 1, l) -<prf> fmatrix_v (a2, m, n, l)
  // [fpf: for going back]
) // end of [GEVEC_v_of_fmatrix_v]

(*
// this kind of destroys the genericity of GEVEC
prfun fmatrix_v_of_GEVEC_v_of
  {a:viewt@ype} {m,n:nat} {mn:int} {l:addr} (
  pf_mul: MUL (m, n, mn), pf_mat: GEVEC_v (a, mn, 1, l)
) :<> (
  fmatrix_v (a, m, n, l)
, fmatrix_v (a, m, n, l) -<prf> GEVEC_v (a, mn, 1, l)
) // end of [GEVEC_v_of_fmatrix_v]
*)

(* ****** ****** *)

prfun GEMAT_v_of_fmatrix_v
  {a1:viewt@ype} {m,n:nat} {l:addr} (
  pf_mat: fmatrix_v (a1, m, n, l)
) :<> (
  GEMAT_v (a1, m, n, col, m, l)
, {a2:viewt@ype | a1 \tszeq a2}
    GEMAT_v (a2, m, n, col, m, l) -<prf> fmatrix_v (a2, m, n, l)
  // [fpf: for going back]
) // end of [GEMAT_v_of_fmatrix_v]

(*
// this kind of destroys the genericity of GEMAT
prfun fmatrix_v_of_GEMAT_v
  {a:viewt@ype} {m,n:nat} {l:addr} (
  pf_mat: GEMAT_v (a, m, n, col, m, l)
) :<> (
  fmatrix_v (a, m, n, l)
, fmatrix_v (a, m, n, l) -<prf> GEMAT_v (a, m, n, col, m, l)
) // end of [GEMAT_v_of_fmatrix_v]
*)

(* ****** ****** *)

prfun GBMAT_v_of_fmatrix_v
  {a:viewt@ype} {m,n,k,kl,ku:nat| k == kl+ku+1} {l:addr} (
  pf : fmatrix_v (a, k, n, l), m : size_t m, kl : size_t kl, ku : size_t ku
) :<> (
  GBMAT_v (a, m, n, col, kl, ku, k, l)
, GBMAT_v (a, m, n, col, kl, ku, k, l) -<prf> fmatrix_v (a, k, n, l)
) // end of [GEBAT_v_of_fmatrix_v]

(* ****** ****** *)

fun fmatrix_ptr_foreach_funenv_tsz
  {a:viewt@ype}
  {v:view} {vt:viewtype}
  {ord:order} {m,n:nat} (
  pf: !v
| M: &fmatrix (a, m, n)
, f: (!v | &a, !vt) -<fun> void
, ord: ORDER ord, m: size_t m, n: size_t n
, tsz: sizeof_t a
, env: !vt
) :<> void // end of [fmatrix_foreach_funenv_tsz]

fun{a:viewt@ype}
fmatrix_ptr_foreach_fun {ord:order} {m,n:nat} (
  M: &fmatrix (a, m, n)
, f: (&a) -<fun> void
, ord: ORDER ord, m: size_t m, n: size_t n
) :<> void // end of [fmatrix_foreach_fun]

fun{a:viewt@ype}
fmatrix_ptr_foreach_vclo {v:view} {ord:order} {m,n:nat} (
  pf: !v
| M: &fmatrix (a, m, n)
, f: &(!v | &a) -<clo> void
, ord: ORDER ord, m: size_t m, n: size_t n
) :<> void // end of [fmatrix_foreach_vclo]

(* ****** ****** *)

fun fmatrix_ptr_iforeach_funenv_tsz
  {a:viewt@ype}
  {v:view} {vt:viewtype}
  {ord:order} {m,n:nat} (
  pf: !v
| M: &fmatrix (a, m, n)
, f: (!v | sizeLt m, sizeLt n, &a, !vt) -<fun> void
, ord: ORDER ord, m: size_t m, n: size_t n
, tsz: sizeof_t a
, env: !vt
) :<> void // end of [fmatrix_iforeach_funenv_tsz]

fun{a:viewt@ype}
fmatrix_ptr_iforeach_fun
  {ord:order} {m,n:nat} (
  M: &fmatrix (a, m, n), f: (sizeLt m, sizeLt n, &a) -<fun> void
, ord: ORDER ord, m: size_t m, n: size_t n
) :<> void // end of [fmatrix_iforeach_fun]

fun{a:viewt@ype}
fmatrix_ptr_iforeach_vclo {v:view} {ord:order} {m,n:nat} (
  pf: !v
| M: &fmatrix (a, m, n), f: &(!v | sizeLt m, sizeLt n, &a) -<clo> void
, ord: ORDER ord, m: size_t m, n: size_t n
) :<> void // end of [fmatrix_iforeach_vclo]

(* ****** ****** *)

(* end of [fmatrix.sats] *)
