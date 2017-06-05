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
** Various kinds of (generic) arrays
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Start Time: Summer, 2009
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

%{#
#include "libats/CATS/genarrays.cats"
%} // end of [%{#]

(* ****** ****** *)

sortdef inc = {i: int | i > 0} // for leading dimensions

(* ****** ****** *)

// general vector // elt, size, delta
absviewt@ype GEVEC (a:viewt@ype+, n:int, d:int)

viewdef GEVEC_v
  (a:viewt@ype, n:int, d:int, l:addr) = GEVEC (a, n, d) @ l
// end of [GEVEC_v]

(* ****** ****** *)

prfun GEVEC_v_nil
  {a:viewt@ype} {d:inc} {l:addr} ():<prf> GEVEC_v (a, 0, d, l)
// end of [GEVEC_v_nil]

prfun GEVEC_v_unnil
  {a:viewt@ype} {d:inc} {l:addr} (pf: GEVEC_v (a, 0, d, l)):<prf> void
// end of [GEVEC_v_unnil]

prfun GEVEC_v_cons
  {a:viewt@ype}
  {n:pos} {d:inc} {l:addr} {ofs:int} (
  pf_mul: MUL (d, sizeof a, ofs), pf_at: a @ l, pf_vec: GEVEC_v (a, n-1, d, l+ofs)
) :<prf> GEVEC_v (a, n, d, l) // end of [GEVEC_v_cons]

prfun GEVEC_v_uncons
  {a:viewt@ype}
  {n:pos} {d:inc} {l:addr} {ofs:int} (
  pf_mul: MUL (d, sizeof a, ofs), pf_vec: GEVEC_v (a, n, d, l)
) :<prf> (
  a @ l, GEVEC_v (a, n-1, d, l+ofs)
) // end of [GEVEC_v_uncons]

(* ****** ****** *)

prfun array_v_of_GEVEC_v
  {a:viewt@ype} {n:nat} {l:addr}
  (pf: GEVEC_v (a, n, 1, l)):<prf> array_v (a, n, l)
// end [array_v_of_GEVEC_v]

prfun GEVEC_v_of_array_v
  {a:viewt@ype} {n:nat} {l:addr}
  (pf: array_v (a, n, l)):<prf> GEVEC_v (a, n, 1, l)
// end [array_v_of_GEVEC_v]

(* ****** ****** *)

fun{a:viewt@ype}
GEVEC_ptr_takeout
  {n:nat} {d:inc} {l0:addr} (
  pf: GEVEC_v (a, n, d, l0)
| p_vec: ptr l0, d: size_t d, i: sizeLt n
) :<> [l:addr] (
  a @ l
, a @ l -<lin,prf> GEVEC_v (a, n, d, l0)
| ptr l
) // end of [GEVEC_ptr_takeout]

(* ****** ****** *)

stadef tszeq
  (a1:viewt@ype, a2:viewt@ype) = (sizeof a1 == sizeof a2)
// end of [tszeq]

fun{a1:viewt@ype}
GEVEC_ptr_split
 {n,i:nat | i <= n} {d:inc} {l0:addr} (
  pf: GEVEC_v (a1, n, d, l0)
| p_vec: ptr l0, d: size_t d, i: size_t i
) :<> [l:addr] (
  GEVEC_v (a1, i, d, l0)
, GEVEC_v (a1, n-i, d, l)
, {a2:viewt@ype | a1 \tszeq a2} (
  // [fpf: for unsplitting]
  GEVEC_v (a2, i, d, l0), GEVEC_v (a2, n-i, d, l)
) -<prf> GEVEC_v (a2, n, d, l0)
| ptr l
) // end of [GEVEC_ptr_split]

(* ****** ****** *)

fun{a:t@ype} GEVEC_ptr_get_elt_at {n:nat}
  {d:inc} (V: &GEVEC (a, n, d), d: size_t d, i: sizeLt n):<> a
// end of [GEVEC_ptr_get_elt_at]

fun{a:t@ype}
GEVEC_ptr_set_elt_at {n:nat}
  {d:inc} (V: &GEVEC (a, n, d), d: size_t d, i: sizeLt n, x: a):<> void
// end of [GEVEC_ptr_set_elt_at]

(* ****** ****** *)

fun{a:t@ype}
GEVEC_ptr_initialize_elt {m:nat} {d:inc} (
  X: &GEVEC (a?, m, d) >> GEVEC (a, m, d), m: size_t m, inc: size_t d, alpha: a
) :<> void // end of [GEVEC_ptr_initialize_elt]

(* ****** ****** *)

fun{a:t@ype}
GEVEC_ptr_copy {m:nat} {d1,d2:inc} (
  X1: &GEVEC (a, m, d1), X2: &GEVEC (a?, m, d2) >> GEVEC (a, m, d2)
, m: size_t m, d1: size_t d1, d2: size_t d2
) :<> void // end of [GEVEC_ptr_copy]

(* ****** ****** *)

fun GEVEC_ptr_foreach_funenv_tsz
  {a:viewt@ype} {v:view} {vt:viewtype} {n:nat} {d:inc} (
  pf: !v
| base: &GEVEC (a, n, d)
, f: (!v | &a, !vt) -<> void, vsz: size_t n, inc: size_t d, tsz: sizeof_t a
, env: !vt
) :<> void
  = "atslib_GEVEC_ptr_foreach_funenv_tsz"
// end of [GEVEC_ptr_foreach_funenv_tsz]

fun{a:viewt@ype}
GEVEC_ptr_foreach_fun
  {n:nat} {d:inc} (
  base: &GEVEC (a, n, d)
, f: (&a) -<fun> void, vsz: size_t n, inc: size_t d
) :<> void // end of [GEVEC_ptr_foreach_fun]

fun{a:viewt@ype}
GEVEC_ptr_foreach_vclo
  {v:view} {n:nat} {d:inc} (
  pf: !v
| base: &GEVEC (a, n, d)
, f: &(!v | &a) -<clo> void, vsz: size_t n, inc: size_t d
) :<> void // end of [GEVEC_ptr_foreach_vclo]

(* ****** ****** *)

fun GEVEC_ptr_iforeach_funenv_tsz
  {a:viewt@ype} {v:view} {vt:viewtype} {n:nat} {d:inc} (
  pf: !v
| base: &GEVEC (a, n, d)
, f: (!v | sizeLt n, &a, !vt) -<> void
, vsz: size_t n
, inc: size_t d
, tsz: sizeof_t a
, env: !vt
) :<> void
  = "atslib_GEVEC_ptr_iforeach_funenv_tsz"
// end of [GEVEC_ptr_iforeach_funenv_tsz]

fun{a:viewt@ype}
GEVEC_ptr_iforeach_fun
  {n:nat} {d:inc} (
  base: &GEVEC (a, n, d)
, f: (sizeLt n, &a) -<fun> void, vsz: size_t n, inc: size_t d
) :<> void // end of [fun]

fun GEVEC_ptr_iforeach_cloenv_tsz
  {a:viewt@ype} {v:view} {vt:viewtype} {n:nat} {d:inc} (
  pf: !v
| base: &GEVEC (a, n, d)
, f: &(!v | sizeLt n, &a, !vt) -<clo> void
, vsz: size_t n
, inc: size_t d
, tsz: sizeof_t a
, env: !vt
) :<> void
  = "atslib_GEVEC_ptr_iforeach_cloenv_tsz"
// end of [fun]

fun{a:viewt@ype}
GEVEC_ptr_iforeach_vclo
  {v:view} {n:nat} {d:inc} (
  pf: !v
| base: &GEVEC (a, n, d)
, f: &(!v | sizeLt n, &a) -<clo> void, vsz: size_t n, inc: size_t d
) :<> void // end of [GEVEC_ptr_iforeach_vclo]

(* ****** ****** *)

datasort order =
  | row | col // row major / column major
// end of [order]

datatype ORDER (order) =
  | ORDERrow (row) of () | ORDERcol (col) of ()
// end of [ORDER]

(* ****** ****** *)

(*
datasort uplo =
  | upper | lower // upper right / lower left
// end of [uplo]
*)

sortdef uplo = int
stadef upper = 1 and lower = 0

datatype UPLO (uplo) =
  | UPLOupper (upper) of () | UPLOlower (lower) of ()
// end of [UPLO]

(* ****** ****** *)

datasort diag = unit | nonunit // unit / non-unit
datatype DIAG (diag) = DIAGunit (unit()) | DIAGnonunit (nonunit())

(* ****** ****** *)

datasort transpose = TPN | TPT | TPC

(*

dataprop
transpose_NT (transpose) =
  | TRANSPOSE_NT_N (TPN) of ()
  | TRANSPOSE_NT_T (TPT) of ()
// end of [transpose_NT]

dataprop
transpose_NC (transpose) =
  | TRANSPOSE_NC_N (TPN) of ()
  | TRANSPOSE_NC_C (TPC) of ()
// end of [transpose_NC]

*)

datatype
TRANSPOSE (transpose) =
  | TRANSPOSE_N (TPN) of ()
  | TRANSPOSE_T (TPT) of ()
  | TRANSPOSE_C (TPC) of ()
// end of [TRANSPOSE]

(* ****** ****** *)

(*
** capturing the relation between transpose and order
*)
dataprop tranord_p (order, order) =
  | TRANORDrowcol (row, col) | TRANORDcolrow (col, row)
// end of [tranord_p]

(*
//
// capturing the relation between transpose and uplo
//
dataprop tranuplo_p (uplo, uplo) =
  | TRANUPLOupperlower (upper, lower) | TRANUPLOlowerupper (lower, upper)
// end of [transuplo_p]
*)

(* ****** ****** *)

(*
** capturing the relation between transpose and dimensions
*)
dataprop trandim_p (
  transpose
, int // row
, int // col
, int // new row
, int // new col
) =
  | {m,n:nat} TRANDIM_N (TPN, m, n, m, n) of ()
  | {m,n:nat} TRANDIM_T (TPT, m, n, n, m) of ()
  | {m,n:nat} TRANDIM_C (TPC, m, n, n, m) of ()
(*
  | {m,n:nat} TRANDIM_AC (AC, m, n, m, n) of ()
*)
// end of [trandim_p]

(* ****** ****** *)

datasort side = left | right
datatype SIDE (side) = SIDEleft (left) | SIDEright (right)

(* ****** ****** *)

(*
** capturing the relation between side and row/col
*)
dataprop sidedim_p (
  side, int(*row*), int(*col*), int(*row/col*)
) =
  | {m,n:nat} SIDEDIM_L (left, m, n, m)
  | {m,n:nat} SIDEDIM_R (right, m, n, n)
// end of [sidedim_p]

(* ****** ****** *)

//
// GEneral MATrix representation
//

// elt, row, col, ord, ld
absviewt@ype GEMAT
  (a:viewt@ype+, m:int, n:int, ord:order, ld:int)
// end of [GEMAT]

viewdef GEMAT_v
  (a:viewt@ype, m:int, n:int, ord: order, ld:int, l:addr) =
  GEMAT (a, m, n, ord, ld) @ l
// end of [GEMAT_v]

(* ****** ****** *)

prfun GEMAT_v_trans // HX: yes, [ld:int] is fine
  {a:viewt@ype} {ord1:order} {m,n:nat} {ld:int} {l:addr} (
    pf_mat: !GEMAT_v (a, m, n, ord1, ld, l) >> GEMAT_v (a, n, m, ord2, ld, l)
  ) :<> #[ord2:order] tranord_p (ord1, ord2)
// end of [GEMAT_v_trans]

(* ****** ****** *)

dataprop
MATVECINC (order, order, int, int) =
  | {ld:inc} MATVECINCrowrow (row, row, ld, 1)
  | {ld:inc} MATVECINCrowcol (row, col, ld, ld)
  | {ld:inc} MATVECINCcolrow (col, row, ld, ld)
  | {ld:inc} MATVECINCcolcol (col, col, ld, 1)
// end of [MATVECINC]

// implemented in [genarrays.dats]
fun MATVECINC_get
  {ord1,ord2:order} {ld:int} {d:inc} (
    pf: MATVECINC (ord1, ord2, ld, d)
  | x1: ORDER ord1, x2: ORDER ord2, ld: size_t ld
  ) :<> size_t d
// end of [MATVECINC_get]

(* ****** ****** *)

prfun GEMAT_v_nil_row {a:viewt@ype}
  {ord:order} {n:nat} {ld:inc} {l:addr} () :<prf> GEMAT_v (a, 0, n, ord, ld, l)
// end of [GEMAT_v_nil_row]

prfun GEMAT_v_unnil_row {a:viewt@ype}
  {ord:order} {n:nat} {ld:inc} {l:addr}
  (pf: GEMAT_v (a, 0, n, ord, ld, l)):<prf> void
// end of [GEMAT_v_unnil_row]

prfun GEMAT_v_nil_col {a:viewt@ype}
  {ord:order} {m:nat} {ld:inc} {l:addr} () :<prf> GEMAT_v (a, m, 0, ord, ld, l)
// end of [GEMAT_v_nil_col]

prfun GEMAT_v_unnil_col {a:viewt@ype}
  {ord:order} {m:nat} {ld:inc} {l:addr}
  (pf: GEMAT_v (a, m, 0, ord, ld, l)):<prf> void
// end of [GEMAT_v_unnil_col]

(* ****** ****** *)

prfun GEMAT_v_uncons_row {a:viewt@ype}
  {ord:order} {m:pos;n:nat} {ld:inc} {l:addr}
  (pf: GEMAT_v (a, m, n, ord, ld, l))
  :<prf> [d:inc] (
    MATVECINC (row, ord, ld, d)
  , GEVEC_v (a, n, d, l)
  , GEVEC_v (a, n, d, l) -<lin,prf> GEMAT_v (a, m, n, ord, ld, l)
  )
// end of [GEMAT_uncons_col]

prfun GEMAT_v_uncons_col {a:viewt@ype} 
  {ord:order} {m:nat;n:pos} {ld:inc} {l:addr}
  (pf: GEMAT_v (a, m, n, ord, ld, l))
  :<prf> [d:inc] (
    MATVECINC (col, ord, ld, d)
  , GEVEC_v (a, m, d, l)
  , GEVEC_v (a, m, d, l) -<lin,prf> GEMAT_v (a, m, n, ord, ld, l)
  )
// end of [GEMAT_v_uncons_col]

(* ****** ****** *)

prfun GEVEC_v_of_GEMAT_v_row
  {a1:viewt@ype}
  {ord:order} {n:nat} {ld:inc}
  {l:addr} (
  pf: GEMAT_v (a1, 1, n, ord, ld, l)
) :<> [d:inc] (
  MATVECINC (row, ord, ld, d)
, GEVEC_v (a1, n, d, l)
, {a2:viewt@ype | a1 \tszeq a2}
// [fpf: for unsplitting]
  GEVEC_v (a2, n, d, l) -<prf> GEMAT_v (a2, 1, n, ord, ld, l)
) // end of [GEVEC_v_of_GEMAT_v_row]

prfun GEVEC_v_of_GEMAT_v_col
  {a1:viewt@ype}
  {ord:order} {m:nat} {ld:inc}
  {l:addr} (
  pf: GEMAT_v (a1, m, 1, ord, ld, l)
) :<> [d:inc] (
  MATVECINC (col, ord, ld, d)
, GEVEC_v (a1, m, d, l)
, {a2:viewt@ype | a1 \tszeq a2}
// [fpf: for unsplitting]
  GEVEC_v (a2, m, d, l) -<prf> GEMAT_v (a2, m, 1, ord, ld, l)
) // end of [GEVEC_v_of_GEMAT_v_col]

(* ****** ****** *)

fun{a:viewt@ype}
GEMAT_ptr_takeout
  {ord:order}
  {m,n:nat} {ld:inc} {l0:addr} (
  pf_mat: GEMAT_v (a, m, n, ord, ld, l0)
| ord: ORDER ord
, p_mat: ptr l0
, ld: size_t ld, i: sizeLt m, j: sizeLt n
) :<> [l:addr] (
  a @ l
, a @ l -<lin,prf> GEMAT_v (a, m, n, ord, ld, l0)
| ptr l
) // end of [GEMAT_ptr_takeout]

(* ****** ****** *)

fun{a:t@ype}
GEMAT_ptr_get_elt_at
  {ord:order} {m,n:nat} {ld:inc} (
  ord: ORDER ord
, A: &GEMAT (a, m, n, ord, ld)
, ld: size_t ld, i: sizeLt m, j: sizeLt n
) :<> a // end of [GEMAT_ptr_get_elt_at]

fun{a:t@ype}
GEMAT_ptr_set_elt_at
  {ord:order} {m,n:nat} {ld:inc} (
  ord: ORDER ord
, A: &GEMAT (a, m, n, ord, ld)
, ld: size_t ld, i: sizeLt m, j: sizeLt n
, x: a
) :<> void // end of [GEMAT_ptr_set_elt_at]

(* ****** ****** *)

(*
** HX: this is likely to be slightly more efficient
** than GEMAT_ptr_split2x1
*)
fun{a:viewt@ype}
GEMAT_ptr_tail_row
  {ord:order}
  {m:pos;n:nat}
  {ld:inc}
  {l0:addr} (
  pf_mat: GEMAT_v (a, m, n, ord, ld, l0)
| ord: ORDER ord
, p_mat: ptr l0
, ld: size_t ld
) :<> [l:addr] (
  GEMAT_v (a, m-1, n, ord, ld, l)
, GEMAT_v (a, m-1, n, ord, ld, l) -<lin,prf> GEMAT_v (a, m, n, ord, ld, l0)
| ptr l
) // end of [GEMAT_ptr_tail_row]

(*
** HX: this is likely to be slightly more efficient than GEMAT_ptr_split1x2
*)
fun{a:viewt@ype}
GEMAT_ptr_tail_col
  {ord:order} {m:nat;n:pos} {ld:inc} {l0:addr} (
  pf_mat: GEMAT_v (a, m, n, ord, ld, l0)
| ord: ORDER ord
, p_mat: ptr l0
, ld: size_t ld
) :<> [l:addr] (
  GEMAT_v (a, m, n-1, ord, ld, l)
, GEMAT_v (a, m, n-1, ord, ld, l) -<lin,prf> GEMAT_v (a, m, n, ord, ld, l0)
| ptr l
) // end of [GEMAT_ptr_tail_col]

(* ****** ****** *)

viewtypedef
GEMAT_ptr_split1x2_res_t (
  a1:viewt@ype, m:int, n:int, j:int, ord:order, ld:int, l0:addr
) = [l1,l2:addr] @(
  GEMAT_v (a1, m, j, ord, ld, l1)  
, GEMAT_v (a1, m, n-j, ord, ld, l2)
, {a2:viewt@ype | a1 \tszeq a2} (
  // [fpf: for unsplitting]
  GEMAT_v (a2, m, j, ord, ld, l1), GEMAT_v (a2, m, n-j, ord, ld, l2)
) -<prf>
  GEMAT_v (a2, m, n, ord, ld, l0)
| ptr l1 // l1 should equal l0
, ptr l2
) // end of [GEMAT_ptr_split1x2_res_t]

fun{a1:viewt@ype}
GEMAT_ptr_split1x2
  {ord:order}
  {m,n,j:nat | j <= n}
  {ld:inc}
  {l0:addr} (
  pf_mat: GEMAT_v (a1, m, n, ord, ld, l0)
| ord: ORDER ord, p_mat: ptr l0, ld: size_t ld, j: size_t j
) :<> GEMAT_ptr_split1x2_res_t (a1, m, n, j, ord, ld, l0)
// end of [GEMAT_ptr_split1x2]

(* ****** ****** *)

viewtypedef
GEMAT_ptr_split2x1_res_t (
  a1:viewt@ype, m:int, n:int, i:int, ord:order, ld:int, l0:addr
) = [l1,l2:addr] @(
  GEMAT_v (a1, i, n, ord, ld, l1)  
, GEMAT_v (a1, m-i, n, ord, ld, l2)
, {a2:viewt@ype | a1 \tszeq a2} (
  // [fpf: for unsplitting]
  GEMAT_v (a2, i, n, ord, ld, l1)
, GEMAT_v (a2, m-i, n, ord, ld, l2)
) -<prf>
  GEMAT_v (a2, m, n, ord, ld, l0)
| ptr l1 // l1 should equal l0
, ptr l2
) // end of [GEMAT_ptr_split2x1_res_t]

fun{a1:viewt@ype}
GEMAT_ptr_split2x1
  {ord:order}
  {m,n,i:nat | i <= m}
  {ld:inc}
  {l0:addr} (
  pf_mat: GEMAT_v (a1, m, n, ord, ld, l0)
| ord: ORDER ord, p_mat: ptr l0, ld: size_t ld, i: size_t i
) :<> GEMAT_ptr_split2x1_res_t (a1, m, n, i, ord, ld, l0)
// end of [GEMAT_ptr_split2x1]

(* ****** ****** *)

viewtypedef
GEMAT_ptr_split2x2_res_t (
  a1:viewt@ype, m:int, n:int, i:int, j:int, ord:order, ld:int, l0:addr
) = [l11,l12,l21,l22:addr] @(
  GEMAT_v (a1, i, j, ord, ld, l11)  
, GEMAT_v (a1, i, n-j, ord, ld, l12)
, GEMAT_v (a1, m-i, j, ord, ld, l21)
, GEMAT_v (a1, m-i, n-j, ord, ld, l22)
, {a2:viewt@ype | a1 \tszeq a2} (
  // [fpf: for unsplitting]
  GEMAT_v (a2, i, j, ord, ld, l11)
, GEMAT_v (a2, i, n-j, ord, ld, l12)
, GEMAT_v (a2, m-i, j, ord, ld, l21)
, GEMAT_v (a2, m-i, n-j, ord, ld, l22)
) -<prf>
  GEMAT_v (a2, m, n, ord, ld, l0)
| ptr l11 // l11 should equal l0
, ptr l12
, ptr l21
, ptr l22
) // end of [GEMAT_ptr_split2x2_res_t]

fun{a1:viewt@ype}
GEMAT_ptr_split2x2
  {ord:order}
  {m,n,i,j:nat | i <= m; j <= n}
  {ld:inc}
  {l0:addr} (
  pf_mat: GEMAT_v (a1, m, n, ord, ld, l0)
| ord: ORDER ord, p_mat: ptr l0, ld: size_t ld, i: size_t i, j: size_t j
) :<> GEMAT_ptr_split2x2_res_t (a1, m, n, i, j, ord, ld, l0)
// end of [GEMAT_ptr_split2x2]

(* ****** ****** *)

fun{a:t@ype}
  GEMAT_row_ptr_allocfree
  {m,n:nat} (m: size_t m, n: size_t n)
  : [l:addr] (
  GEMAT (a?, m, n, row, n) @ l
| ptr l
, (GEMAT (a?, m, n, row, n) @ l | ptr l) -<lin> void
) // end of [GEMAT_row_ptr_allocfree]

fun{a:t@ype}
  GEMAT_col_ptr_allocfree
  {m,n:nat} (m: size_t m, n: size_t n)
  : [l:addr] (
  GEMAT (a?, m, n, col, m) @ l
| ptr l
, (GEMAT (a?, m, n, col, m) @ l | ptr l) -<lin> void
) // end of [GEMAT_col_ptr_allocfree]

(* ****** ****** *)

fun{a:t@ype}
  GEMAT_ptr_initialize_elt
  {ord:order} {m,n:nat} {ld:inc} (
  ord: ORDER ord
, X: &GEMAT (a?, m, n, ord, ld) >> GEMAT (a, m, n, ord, ld)
, m: size_t m, n: size_t n, ld: size_t ld
, alpha: a
) :<> void // end of [GEMAT_ptr_initialize_elt]

//
// HX: based on [GEMAT_ptr_iforeach_fun]
//
fun{a:t@ype}
GEMAT_ptr_initialize_fun
  {ord:order} {m,n:nat} {ld:inc} (
  ord: ORDER ord
, M: &GEMAT (a?, m, n, ord, ld) >> GEMAT (a, m, n, ord, ld)
, m: size_t m, n: size_t n, ld: size_t ld
, f: (sizeLt m, sizeLt n, &(a?) >> a) -<> void
) :<> void // end of [GEMAT_ptr_initialize_fun]

//
// HX: based on [GEMAT_ptr_iforeach_vclo]
//
fun{a:t@ype}
GEMAT_ptr_initialize_vclo
  {v:view} {ord:order} {m,n:nat} {ld:inc} (
  pf: !v
| ord: ORDER ord
, M: &GEMAT (a?, m, n, ord, ld) >> GEMAT (a, m, n, ord, ld)
, m: size_t m, n: size_t n, ld: size_t ld
, f: &(!v | sizeLt m, sizeLt n, &(a?) >> a) -<clo> void
) :<> void // end of [GEMAT_ptr_initialize_vclo]

(* ****** ****** *)

(*
** M2 <- M1
*)
fun{a:t@ype} GEMAT_ptr_copy
  {ord:order} {m,n:nat} {ld1,ld2:inc} (
  ord: ORDER ord
, M1: &GEMAT (a, m, n, ord, ld1)
, M2: &GEMAT (a?, m, n, ord, ld2) >> GEMAT (a, m, n, ord, ld2)
, m: size_t m, n: size_t n, ld1: size_t ld1, ld2: size_t ld2
) :<> void // end of [GEMAT_ptr_copy]

fun GEMAT_ptr_copy_tsz {a:t@ype}
  {ord:order} {m,n:nat} {ld1,ld2:inc} (
  ord: ORDER ord
, M1: &GEMAT (a, m, n, ord, ld1)
, M2: &GEMAT (a?, m, n, ord, ld2) >> GEMAT (a, m, n, ord, ld2)
, m: size_t m, n: size_t n, ld1: size_t ld1, ld2: size_t ld2, tsz: sizeof_t a
) :<> void // end of [GEMAT_ptr_copy_tsz]

(* ****** ****** *)

fun GEMAT_ptr_foreach_funenv_tsz
  {a:viewt@ype} {v:view} {vt:viewtype}
  {ord1,ord2:order} {m,n:nat} {ld:inc} (
  pf: !v
| ord1: ORDER ord1
, M: &GEMAT (a, m, n, ord1, ld)
, f: (!v | &a, !vt) -<fun> void
, ord2: ORDER ord2
, m: size_t m, n: size_t n, ld: size_t ld
, tsz: sizeof_t a
, env: !vt
) :<> void // end of [GEMAT_ptr_foreach_funenv_tsz]

fun{a:viewt@ype}
GEMAT_ptr_foreach_fun
  {ord1,ord2:order} {m,n:nat} {ld:inc} (
  ord1: ORDER ord1
, M: &GEMAT (a, m, n, ord1, ld)
, f: (&a) -<fun> void
, ord2: ORDER ord2
, m: size_t m, n: size_t n, ld: size_t ld
) :<> void // end of [GEMAT_ptr_foreach_fun]

fun{a:viewt@ype}
GEMAT_ptr_foreach_vclo
  {v:view} {ord1,ord2:order} {m,n:nat} {ld:inc} (
  pf: !v
| ord1: ORDER ord1
, M: &GEMAT (a, m, n, ord1, ld)
, f: &(!v | &a) -<clo> void
, ord2: ORDER ord2
, m: size_t m, n: size_t n, ld: size_t ld
) :<> void // end of [GEMAT_ptr_foreach_vclo]

(* ****** ****** *)

fun GEMAT_ptr_iforeach_funenv_tsz
  {a:viewt@ype} {v:view} {vt:viewtype}
  {ord1,ord2:order} {m,n:nat} {ld:inc} (
  pf: !v
| ord1: ORDER ord1
, M: &GEMAT (a, m, n, ord1, ld)
, f: (!v | sizeLt m, sizeLt n, &a, !vt) -<fun> void
, ord2: ORDER ord2
, m: size_t m, n: size_t n, ld: size_t ld
, tsz: sizeof_t a
, env: !vt
) :<> void // end of [GEMAT_ptr_iforeach_funenv_tsz]

fun{a:viewt@ype}
GEMAT_ptr_iforeach_fun
  {ord1,ord2:order} {m,n:nat} {ld:inc} (
  ord1: ORDER ord1
, M: &GEMAT (a, m, n, ord1, ld)
, f: (sizeLt m, sizeLt n, &a) -<fun> void
, ord2: ORDER ord2
, m: size_t m, n: size_t n, ld: size_t ld
) :<> void // end of [GEMAT_ptr_iforeach_fun]

fun{a:viewt@ype}
GEMAT_ptr_iforeach_vclo
  {v:view} {ord1,ord2:order} {m,n:nat} {ld:inc} (
  pf: !v
| ord1: ORDER ord1
, M: &GEMAT (a, m, n, ord1, ld)
, f: &(!v | sizeLt m, sizeLt n, &a) -<clo> void
, ord2: ORDER ord2
, m: size_t m, n: size_t n, ld: size_t ld
) :<> void // end of [GEMAT_ptr_iforeach_vclo]

(* ****** ****** *)

dataprop realtyp_p (a:t@ype) =
  | REALTYPfloat (float) of () | REALTYPdouble (double) of ()
// end of [TYPE_IS_REAL]

(* ****** ****** *)

//
// TRiangular MATrix representation (part of GEMAT)
//

// elt, row/col, ord, ul
absviewt@ype TRMAT // dimension: n x n
  (a:viewt@ype+, n:int, ord: order, ul: uplo, dg: diag, ld: int)
// end of [TRMAT]

viewdef TRMAT_v
  (a:viewt@ype, n:int, ord: order, ul: uplo, dg: diag, ld: int, l:addr) =
  TRMAT (a, n, ord, ul, dg, ld) @ l
// end of [TRMAT_v]

(* ****** ****** *)

prfun TRMAT_v_nil {a:viewt@ype} {ord:order}
  {ul:uplo} {dg:diag} {ld:inc} {l:addr} ():<prf> TRMAT_v (a, 0, ord, ul, dg, ld, l)
// end of [TRMAT_v_nil]

prfun TRMAT_v_unnil {a:viewt@ype}
  {ord:order} {ul:uplo} {dg:diag} {ld:inc} {l:addr}
  (pf: TRMAT_v (a, 0, ord, ul, dg, ld, l)):<prf> void
// end of [TRMAT_v_unnil]

(* ****** ****** *)

prfun TRMAT1x1_v_takeout_unit
  {a1:viewt@ype}
  {ord:order} {ul:uplo} {ld:inc} {l:addr} (
  pf: TRMAT_v (a1, 1, ord, ul, unit(), ld, l)
) : (
  {a2:viewt@ype | a1 \tszeq a2} () -<prf> TRMAT_v (a2, 1, ord, ul, unit(), ld, l)
) // end of [TRMAT1x1_v_takeout_unit]

prfun TRMAT1x1_v_takeout_nonunit
  {a1:viewt@ype}
  {ord:order} {ul:uplo} {ld:inc} {l:addr} (
  pf: TRMAT_v (a1, 1, ord, ul, nonunit(), ld, l)
) : (
  a1 @ l
, {a2:viewt@ype | a1 \tszeq a2} a2 @ l -<prf> TRMAT_v (a2, 1, ord, ul, nonunit(), ld, l)
) // end of [TRMAT1x1_v_takeout_nonunit]

(* ****** ****** *)

prfun TRMAT_v_trans {a:viewt@ype}
  {ord1:order} {ul:uplo} {dg:diag} {m:nat} {ld:inc} {l:addr} (
    pf_mat: !TRMAT_v (a, m, ord1, ul, dg, ld, l) >> TRMAT_v (a, m, ord2, 1-ul, dg, ld, l)
  ) :<> #[ord2:order] tranord_p (ord1, ord2)
// end of [TRMAT_v_trans]

prfun TRMAT_v_of_GEMAT_v {a:viewt@ype}
  {ord:order} {ul:uplo} {dg:diag} {n:nat} {ld:inc} {l:addr} (
  pf: GEMAT_v (a, n, n, ord, ld, l), ul: UPLO ul, dg: DIAG dg
) :<> (
  TRMAT_v (a, n, ord, ul, dg, ld, l)
, TRMAT_v (a, n, ord, ul, dg, ld, l) -<lin,prf> GEMAT_v (a, n, n, ord, ld, l)
) // end of [TRMAT_v_of_GEMAT_v]

(* ****** ****** *)

// UN: upper non-unit
fun{a:t@ype} TRMAT_UN_ptr_get_elt_at
   {ord:order} {m,i,j:nat | i <= j; j < m} {ld:inc} (
   ord: ORDER ord
 , A: &TRMAT (a, m, ord, upper, nonunit(), ld)
 , ld: size_t ld, i: size_t i, j: size_t j
 ) :<> a // end of [TRMAT_UN_ptr_get_elt_at]

fun{a:t@ype} TRMAT_UN_ptr_set_elt_at
   {ord:order} {m,i,j:nat | i <= j; j < m} {ld:inc} (
   ord: ORDER ord
 , A: &TRMAT (a, m, ord, upper, nonunit(), ld)
 , ld: size_t ld, i: size_t i, j: size_t j
 , x: a
 ) :<> void // end of [TRMAT_UN_ptr_set_elt_at]

(* ****** ****** *)

// UU: upper unit
fun{a:t@ype} TRMAT_UU_ptr_get_elt_at
   {ord:order} {m,i,j:nat | i < j; j < m} {ld:inc} (
   ord: ORDER ord
 , A: &TRMAT (a, m, ord, upper, unit(), ld)
 , ld: size_t ld, i: size_t i, j: size_t j
 ) :<> a // end of [TRMAT_UU_ptr_get_elt_at]

fun{a:t@ype} TRMAT_UU_ptr_set_elt_at
   {ord:order} {m,i,j:nat | i < j; j < m} {ld:inc} (
   ord: ORDER ord
 , A: &TRMAT (a, m, ord, upper, unit(), ld)
 , ld: size_t ld, i: size_t i, j: size_t j
 , x: a
 ) :<> void // end of [TRMAT_UU_ptr_set_elt_at]

(* ****** ****** *)

// LN: lower non-unit
fun{a:t@ype} TRMAT_LN_ptr_get_elt_at
   {ord:order} {m,i,j:nat | j <= i; i < m} {ld:inc} (
   ord: ORDER ord
 , A: &TRMAT (a, m, ord, lower, nonunit(), ld)
 , ld: size_t ld, i: size_t i, j: size_t j
 ) :<> a // end of [TRMAT_LN_ptr_get_elt_at]

fun{a:t@ype} TRMAT_LN_ptr_set_elt_at
   {ord:order} {m,i,j:nat | j <= i; i < m} {ld:inc} (
   ord: ORDER ord
 , A: &TRMAT (a, m, ord, lower, nonunit(), ld)
 , ld: size_t ld, i: size_t i, j: size_t j
 , x: a
 ) :<> void // end of [TRMAT_LN_ptr_set_elt_at]

(* ****** ****** *)

// LU: lower unit
fun{a:t@ype} TRMAT_LU_ptr_get_elt_at
   {ord:order} {m,i,j:nat | j < i; i < m} {ld:inc} (
   ord: ORDER ord
 , A: &TRMAT (a, m, ord, lower, unit(), ld)
 , ld: size_t ld, i: size_t i, j: size_t j
 ) :<> a // end of [TRMAT_LU_ptr_get_elt_at]

fun{a:t@ype} TRMAT_LU_ptr_set_elt_at
   {ord:order} {m,i,j:nat | j < i; i < m} {ld:inc} (
   ord: ORDER ord
 , A: &TRMAT (a, m, ord, lower, unit(), ld)
 , ld: size_t ld, i: size_t i, j: size_t j
 , x: a
 ) :<> void // end of [TRMAT_LU_ptr_set_elt_at]

(* ****** ****** *)

viewtypedef TRMAT_U_ptr_split2x2_res_t (
  a1:viewt@ype, m:int, i:int, ord:order, dg:diag, ld:int, l0:addr
) = [lu,lo,ll:addr] @(
  TRMAT_v (a1, i, ord, upper, dg, ld, lu)
, GEMAT_v (a1, i, m-i, ord, ld, lo)
, TRMAT_v (a1, m-i, ord, upper, dg, ld, ll)
, {a2:viewt@ype | a1 \tszeq a2} (
  // [fpf: for unsplitting]
  TRMAT_v (a2, i, ord, upper, dg, ld, lu)
, GEMAT_v (a2, i, m-i, ord, ld, lo)
, TRMAT_v (a2, m-i, ord, upper, dg, ld, ll)
) -<prf>
  TRMAT_v (a2, m, ord, upper, dg, ld, l0)
| ptr lu // l11 should equal l0
, ptr lo
, ptr ll
) // end of [TRMAT_U_ptr_split2x2_res_t]

fun{a1:viewt@ype}
TRMAT_U_ptr_split2x2
  {ord:order} {dg:diag}
  {m,i:nat | i <= m} {ld:inc} {l0:addr} (
  pf_mat: TRMAT_v (a1, m, ord, upper, dg, ld, l0)
| ord: ORDER ord, A: ptr l0, ld: size_t ld, i: size_t i
) :<> TRMAT_U_ptr_split2x2_res_t (a1, m, i, ord, dg, ld, l0)
// end of [TRMAT_U_ptr_split2x2]

viewtypedef
TRMAT_L_ptr_split2x2_res_t (
  a1:viewt@ype, m:int, i:int, ord:order, dg:diag, ld:int, l0:addr
) = [lu,lo,ll:addr] @(
  TRMAT_v (a1, i, ord, lower, dg, ld, lu)
, GEMAT_v (a1, m-i, i, ord, ld, lo)
, TRMAT_v (a1, m-i, ord, lower, dg, ld, ll)
, {a2:viewt@ype | a1 \tszeq a2} (
  // [fpf: for unsplitting]
  TRMAT_v (a2, i, ord, lower, dg, ld, lu)
, GEMAT_v (a2, m-i, i, ord, ld, lo)
, TRMAT_v (a2, m-i, ord, lower, dg, ld, ll)
) -<prf>
  TRMAT_v (a2, m, ord, lower, dg, ld, l0)
| ptr lu // l11 should equal l0
, ptr lo
, ptr ll
) // end of [TRMAT_L_ptr_split2x2_res_t]

fun{a1:viewt@ype}
TRMAT_L_ptr_split2x2
  {ord:order} {dg:diag}
  {m,i:nat | i <= m} {ld:inc} {l0:addr} (
  pf_mat: TRMAT_v (a1, m, ord, lower, dg, ld, l0)
| ord: ORDER ord, A: ptr l0, ld: size_t ld, i: size_t i
) :<> TRMAT_L_ptr_split2x2_res_t (a1, m, i, ord, dg, ld, l0)
// end of [TRMAT_L_ptr_split2x2]

(* ****** ****** *)

(*
** M2 <- M1
*)
fun{a:t@ype} TRMAT_ptr_copy
  {ord:order} {ul:uplo} {dg:diag} {m:nat} {ld1,ld2:inc} (
  ord: ORDER ord, ul: UPLO ul, dg: DIAG dg
, M1: &TRMAT (a, m, ord, ul, dg, ld1)
, M2: &TRMAT (a?, m, ord, ul, dg, ld2) >> TRMAT (a, m, ord, ul, dg, ld2)
, m: size_t m, ld1: size_t ld1, ld2: size_t ld2
) :<> void // end of [TRMAT_ptr_copy]

(* ****** ****** *)

//
// SYmmetric MATrix representation (part of GEMAT)
//

// elt, row/col, ord, ul
absviewt@ype SYMAT // dimension: n x n
  (a:viewt@ype+, n:int, ord: order, ul: uplo, ld: int)
// end of [SYMAT]

viewdef SYMAT_v
  (a:viewt@ype, n:int, ord: order, ul: uplo, ld: int, l:addr) =
  SYMAT (a, n, ord, ul, ld) @ l
// end of [SYMAT_v]

prfun SYMAT_v_of_GEMAT_v
  {a:viewt@ype} {n:nat}
  {ord:order} {ul:uplo} {ld:inc} {l:addr} (
  pf: GEMAT_v (a, n, n, ord, ld, l), ul: UPLO ul
) :<> (
  SYMAT_v (a, n, ord, ul, ld, l)
, SYMAT_v (a, n, ord, ul, ld, l) -<lin,prf> GEMAT_v (a, n, n, ord, ld, l)
) // end of [SYMAT_v_of_GEMAT_v]

(* ****** ****** *)

//
// HErmitian matrix representation (* part of GEMAT *)
//

// elt, row/col, ord, ul
absviewt@ype HEMAT // dimension: n x n
  (a:viewt@ype+, n:int, ord: order, ul: uplo, ld: int)
// end of [HEMAT]

viewdef HEMAT_v
  (a:viewt@ype, n:int, ord: order, ul: uplo, ld: int, l:addr) =
  HEMAT (a, n, ord, ul, ld) @ l
// end of [HEMAT_v]

prfun HEMAT_v_of_SYMAT_v
  {a:t@ype} {n:nat}
  {ord:order} {ul:uplo} {ld:inc} {l:addr} (
  pf_typ: realtyp_p (a), pf_mat: SYMAT_v (a, n, ord, ul, ld, l)
) :<> HEMAT_v (a, n, ord, ul, ld, l)
// end of [HEMAT_v_of_SYMAT_v]

prfun SYMAT_v_of_HEMAT_v
  {a:t@ype} {n:nat}
  {ord:order} {ul:uplo} {ld:inc} {l:addr} (
  pf_typ: realtyp_p (a), pf_mat: HEMAT_v (a, n, ord, ul, ld, l)
) :<> SYMAT_v (a, n, ord, ul, ld, l)
// end of [SYMAT_v_of_HEMAT_v]

prfun HEMAT_v_of_GEMAT_v
  {a:viewt@ype} {n:nat}
  {ord:order} {ul:uplo} {ld:inc} {l:addr} (
  pf: GEMAT_v (a, n, n, ord, ld, l), ul: UPLO ul
) :<> (
  HEMAT_v (a, n, ord, ul, ld, l)
, HEMAT_v (a, n, ord, ul, ld, l) -<lin,prf> GEMAT_v (a, n, n, ord, ld, l)
) // end of [HEMAT_v_of_GEMAT_v]

(* ****** ****** *)

//
// General Band MATrix representation
//

// elt, row, col, ord, lower-bandwidth, upper-bandwidth
absviewt@ype GBMAT // dimension: m x n, lower-bandwidth: kl, upper-bandwidth: ku
  (a:viewt@ype+, m:int, n:int, ord: order, kl: int, ku: int, ld: int)
// end of [GBMAT]

viewdef GBMAT_v
  (a:viewt@ype, m:int, n:int, ord: order, kl: int, ku: int, ld: int, l: addr) =
  GBMAT (a, m, n, ord, kl, ku, ld) @ l
// end of [GBMAT_v]

(* ****** ****** *)

//
// Triangular Band MATrix representation
//

// elt, row/col, ord, ul, diag, bandwidth
absviewt@ype TBMAT // dimension: n x n, bandwidth: k
  (a:viewt@ype+, n:int, ord: order, ul: uplo, dg: diag, k: int, ld: int)
// end of [TBMAT]

viewdef TBMAT_v
  (a:viewt@ype, n:int, ord: order, ul: uplo, dg: diag, k: int, ld: int, l: addr) =
  TBMAT (a, n, ord, ul, dg, k, ld) @ l

prfun TBMAT_v_of_GEMAT_v
  {a:viewt@ype} {n,k:nat}
  {ul:uplo} {dg:diag} {l:addr} (
  pf_gmat: GEMAT_v (a, 1+k, n, col, 1+k, l)
, ul: UPLO ul
, dg: DIAG dg
, k: size_t k
) :<prf> (
  TBMAT_v (a, n, col, ul, dg, k, 1+k, l)
, TBMAT_v (a, n, col, ul, dg, k, 1+k, l) -<prf> GEMAT_v (a, 1+k, n, col, 1+k, l)
) // end of [TBMAT_v_of_GEMAT_v]

(* ****** ****** *)

//
// Triangular Packed MATrix representation
//

// elt, row/col, ord, ul, diag
absviewt@ype TPMAT // dimension: n x n
  (a:viewt@ype+, n:int, ord: order, ul: uplo, dg: diag)
// end of [TPMAT]

viewdef TPMAT_v
  (a:viewt@ype, n:int, ord: order, ul: uplo, dg: diag, l: addr) =
  TPMAT (a, n, ord, ul, dg) @ l
// end of [TPMAT_v]

prfun TPMAT_v_of_GEVEC_v {a:viewt@ype}
  {ord:order} {ul:uplo} {dg:diag} {m,n:nat} {l:addr} (
  pf_mul: MUL (n, n+1, m+m)
, pf_vec: GEVEC_v (a, m, 1, l)
, ord: ORDER (ord)
, ul: UPLO (ul), dg: DIAG (dg)
) :<> (
  TPMAT_v (a, n, ord, ul, dg, l)
, TPMAT_v (a, n, ord, ul, dg, l) -<prf> GEVEC_v (a, m, 1, l)
) // end of [TPMAT_v_of_GEVEC_v]

(* ****** ****** *)

//
// Symmetric Band MATrix representation
//

// elt, row/col, ord, ul, band-width
absviewt@ype SBMAT // dimension: n x n
  (a:viewt@ype+, n:int, ord: order, ul: uplo, k:int, ld:int)
// end of [SBMAT]

viewdef SBMAT_v
  (a:viewt@ype, n:int, ord: order, ul: uplo, k:int, ld:int, l: addr) =
  SBMAT (a, n, ord, ul, k, ld) @ l
// end of [SBMAT_v]

(* ****** ****** *)

//
// Symmetric Packed MATrix representation
//

// elt, row/col, ord, ul, diag
absviewt@ype SPMAT // dimension: n x n
  (a:viewt@ype+, n:int, ord: order, ul: uplo)
// end of [SPMAT]

viewdef SPMAT_v
  (a:viewt@ype, n:int, ord: order, ul: uplo, l: addr) =
  SPMAT (a, n, ord, ul) @ l
// end of [SPMAT_v]

prfun SPMAT_v_of_GEVEC_v {a:viewt@ype}
  {ord:order} {ul:uplo} {m,n:nat} {l:addr} (
  pf_mul: MUL (n, n+1, m+m)
, pf_arr: GEVEC_v (a, m, 1, l)
, ord: ORDER (ord), ul: UPLO (ul)
) :<> (
  SPMAT_v (a, n, ord, ul, l)
, SPMAT_v (a, n, ord, ul, l) -<prf> GEVEC_v (a, m, 1, l)
) // end of [HPMAT_v_of_GEVEC_v]

(* ****** ****** *)

//
// Hermitian Band MATrix representation
//

// elt, row/col, ord, ul, band-width
absviewt@ype HBMAT // dimension: n x n
  (a:viewt@ype+, n:int, ord: order, ul: uplo, k:int, ld:int)
// end of [HBMAT]

viewdef HBMAT_v
  (a:viewt@ype, n:int, ord: order, ul: uplo, k:int, ld:int, l: addr) =
  HBMAT (a, n, ord, ul, k, ld) @ l
// end of [HBMAT_v]

prfun HBMAT_v_of_SBMAT_v {a:t@ype} 
  {ord:order} {ul:uplo} {n:nat} {k:int} {ld:int} {l:addr} (
  pf_typ: realtyp_p (a), pf_mat: SBMAT_v (a, n, ord, ul, k, ld, l)
) :<> HBMAT_v (a, n, ord, ul, k, ld, l)
// end of [HBMAT_v_of_SBMAT_v]

prfun SBMAT_v_of_HBMAT_v {a:t@ype}
  {ord:order} {ul:uplo} {n:nat} {k:int} {ld:int} {l:addr} (
  pf_typ: realtyp_p (a), pf_mat: HBMAT_v (a, n, ord, ul, k, ld, l)
) :<> SBMAT_v (a, n, ord, ul, k, ld, l)
// end of [SBMAT_v_of_HBMAT_v]

(* ****** ****** *)

//
// Hermitian Packed MATrix representation
//

// elt, row/col, ord, ul, diag
absviewt@ype HPMAT // dimension: n x n
  (a:viewt@ype+, n:int, ord:order, ul:uplo)
// end of [HPMAT]

viewdef HPMAT_v
  (a:viewt@ype, n:int, ord:order, ul:uplo, l:addr) =
  HPMAT (a, n, ord, ul) @ l
// end of [HPMAT_v]

prfun HPMAT_v_of_SPMAT_v {a:t@ype}
  {ord:order} {ul:uplo} {n:nat} {l:addr} (
    pf_typ: realtyp_p (a), pf_mat: SPMAT_v (a, n, ord, ul, l)
  ) :<> HPMAT_v (a, n, ord, ul, l)
// end of [HPMAT_v_of_SPMAT_v]

prfun SPMAT_v_of_HPMAT_v {a:t@ype}
  {ord:order} {ul:uplo} {n:nat} {l:addr} (
  pf_typ: realtyp_p (a), pf_mat: HPMAT_v (a, n, ord, ul, l)
) :<> SPMAT_v (a, n, ord, ul, l)
// end of [SPMAT_v_of_HPMAT_v]

prfun HPMAT_v_of_GEVEC_v {a:viewt@ype}
  {ord:order} {ul:uplo} {m,n:nat} {l:addr} (
  pf_mul: MUL (n, n+1, m+m)
, pf_arr: GEVEC_v (a, m, 1, l)
, ord: ORDER (ord), ul: UPLO (ul)
) :<> (
  HPMAT_v (a, n, ord, ul, l)
, HPMAT_v (a, n, ord, ul, l) -<prf> GEVEC_v (a, m, 1, l)
) // end of [HPMAT_v_of_GEVEC_v]

(* ****** ****** *)

(* end of [genarrays.sats] *)
