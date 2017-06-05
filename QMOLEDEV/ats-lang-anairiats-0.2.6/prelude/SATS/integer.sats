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
#print "Loading [integer.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

// signed integers (of unindexed type)

(* ****** ****** *)

castfn int_of_uint (u: uint):<> int
overload int_of with int_of_uint

(* ****** ****** *)

fun int_of_char (c: char):<> int = "atspre_int_of_char"
overload int_of with int_of_char

fun int_of_schar (c: schar):<> int = "atspre_int_of_schar"
overload int_of with int_of_schar

fun int_of_uchar (c: uchar):<> int = "atspre_int_of_uchar"
overload int_of with int_of_uchar

// This function is based on [atoi] in [stdlib.h]
fun int_of_string (s: string):<> int = "atspre_int_of_string"
overload int_of with int_of_string

(* ****** ****** *)

// arithmetic functions and comparison functions

fun abs_int (i: int):<> int = "atspre_abs_int"
overload abs with abs_int

fun neg_int (i: int):<> int = "atspre_neg_int"
overload ~ with neg_int

fun succ_int (i: int):<> int = "atspre_succ_int"
and pred_int (i: int):<> int = "atspre_pred_int"
overload succ with succ_int
overload pred with pred_int

fun add_int_int
  (i1: int, i2: int):<> int = "atspre_add_int_int"
and sub_int_int
  (i1: int, i2: int):<> int = "atspre_sub_int_int"
overload + with add_int_int
overload - with sub_int_int

fun mul_int_int
  (i1: int, i2: int):<> int = "atspre_mul_int_int"
and div_int_int
  (i1: int, i2: int):<> int = "atspre_div_int_int"
overload * with mul_int_int
overload / with div_int_int

fun mod_int_int
  (i1: int, i2: int):<> int = "atspre_mod_int_int"
overload mod with mod_int_int

fun gcd_int_int
 (i1: int, i2: int):<> int = "atspre_gcd_int_int"
overload gcd with gcd_int_int

fun lt_int_int (i1: int, i2: int):<> bool
  = "atspre_lt_int_int"
and lte_int_int (i1: int, i2: int):<> bool
  = "atspre_lte_int_int"
overload < with lt_int_int
overload <= with lte_int_int

fun gt_int_int (i1: int, i2: int):<> bool
  = "atspre_gt_int_int"
and gte_int_int (i1: int, i2: int):<> bool
  = "atspre_gte_int_int"
overload > with gt_int_int
overload >= with gte_int_int

fun eq_int_int (i1: int, i2: int):<> bool
  = "atspre_eq_int_int"
fun neq_int_int (i1: int, i2: int):<> bool
  = "atspre_neq_int_int"
overload = with eq_int_int
overload <> with neq_int_int
overload != with neq_int_int

fun compare_int_int (i1: int, i2: int):<> Sgn
  = "atspre_compare_int_int"
overload compare with compare_int_int

fun max_int_int (i1: int, i2: int):<> int
  = "atspre_max_int_int"
and min_int_int (i1: int, i2: int):<> int
  = "atspre_min_int_int"

overload max with max_int_int
overload min with min_int_int

fun square_int (i: int):<> int = "atspre_square_int"
overload square with square_int

fun cube_int (i: int):<> int = "atspre_cube_int"
overload cube with cube_int

fun pow_int_int1 (base: int, exponent: Nat):<> int
  = "atspre_pow_int_int1"
overload pow with pow_int_int1

(* ****** ****** *)

//
// bit operations
//

fun asl_int_int1
  (i: int, n: Nat):<> int = "atspre_asl_int_int1"
overload << with asl_int_int1

fun asr_int_int1
  (i: int, n: Nat):<> int = "atspre_asr_int_int1"
overload >> with asr_int_int1

(* ****** ****** *)

symintr fprint_int

fun fprint0_int (out: FILEref, x: int):<!exnref> void
  = "atspre_fprint_int"
overload fprint_int with fprint0_int

fun fprint1_int {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: int):<!exnref> void
  = "atspre_fprint_int"
overload fprint_int with fprint1_int

overload fprint with fprint_int

fun print_int (i: int):<!ref> void = "atspre_print_int"
overload print with print_int
fun prerr_int (i: int):<!ref> void = "atspre_prerr_int"
overload prerr with prerr_int

(* ****** ****** *)

symintr fscan_int_exn

fun fscan0_int_exn (inp: FILEref, x: &int? >> int):<!exnref> void
  = "atspre_fscan_int_exn"
overload fscan_int_exn with fscan0_int_exn

fun fscan1_int_exn {m:file_mode}
  (pf: file_mode_lte (m, r) | inp: &FILE m, x: &int? >> int):<!exnref> void
  = "atspre_fscan_int_exn"
overload fscan_int_exn with fscan1_int_exn

(* ****** ****** *)
//
// stringization
//
fun tostrptr_int
  (i: int):<> strptr1 = "atspre_tostrptr_int"
overload tostrptr with tostrptr_int
fun tostring_int (i: int):<> string = "atspre_tostrptr_int"
overload tostring with tostring_int

(* ****** ****** *)

// unsigned integers (of unindexed type)

(* ****** ****** *)

castfn uint_of_int (i: int):<> uint
overload uint_of with uint_of_int

(* ****** ****** *)

fun uint_of_char (c: char):<> uint
  = "atspre_uint_of_char"
overload uint_of with uint_of_char

fun uint_of_double (d: double):<> uint
  = "atspre_uint_of_double"
overload uint_of with uint_of_double

(* ****** ****** *)

// arithmetic functions and comparison functions

fun succ_uint (u: uint):<> uint = "atspre_succ_uint"
and pred_uint (u: uint):<> uint = "atspre_pred_uint"

overload succ with succ_uint
overload pred with pred_uint

//

fun add_uint_uint (u1: uint, u2: uint):<> uint
  = "atspre_add_uint_uint"
and sub_uint_uint (u1: uint, u2: uint):<> uint
  = "atspre_sub_uint_uint"
overload + with add_uint_uint
overload - with sub_uint_uint

fun mul_uint_uint (u1: uint, u2: uint):<> uint
  = "atspre_mul_uint_uint"
and div_uint_uint (u1: uint, u2: uint):<> uint
  = "atspre_div_uint_uint"
and mod_uint_uint (u1: uint, u2: uint):<> uint
  = "atspre_mod_uint_uint"
overload * with mul_uint_uint
overload / with div_uint_uint
overload mod with mod_uint_uint

fun gcd_uint_uint (u1: uint, u2: uint):<> uint
  = "atspre_gcd_uint_uint"
overload gcd with gcd_uint_uint

(* ****** ****** *)

fun square_uint
  (u: uint):<> uint = "atspre_square_uint"
overload square with square_uint

fun cube_uint
  (u: uint):<> uint = "atspre_cube_uint"
overload cube with cube_uint

(* ****** ****** *)

fun lt_uint_uint
  (u1: uint, u2: uint):<> bool = "atspre_lt_uint_uint"
and lte_uint_uint
  (u1: uint, u2: uint):<> bool = "atspre_lte_uint_uint"
overload < with lt_uint_uint
overload <= with lte_uint_uint

fun gt_uint_uint
  (u1: uint, u2: uint):<> bool = "atspre_gt_uint_uint"
and gte_uint_uint
  (u1: uint, u2: uint):<> bool = "atspre_gte_uint_uint"
overload > with gt_uint_uint
overload >= with gte_uint_uint

fun eq_uint_uint
  (u1: uint, u2: uint):<> bool = "atspre_eq_uint_uint"
and neq_uint_uint
  (u1: uint, u2: uint):<> bool = "atspre_neq_uint_uint"
overload = with eq_uint_uint
overload <> with neq_uint_uint
overload != with neq_uint_uint

(* ****** ****** *)

fun compare_uint_uint (
  u1: uint, u2: uint
) :<> Sgn = "atspre_compare_uint_uint"
overload compare with compare_uint_uint

fun max_uint_uint
  (u1: uint, u2: uint):<> uint = "atspre_max_uint_uint"
and min_uint_uint
  (u1: uint, u2: uint):<> uint = "atspre_min_uint_uint"
overload max with max_uint_uint
overload min with min_uint_uint

(* ****** ****** *)

// bit operations

fun lnot_uint
  (u: uint):<> uint = "atspre_lnot_uint" (* bitwise *)
overload ~ with lnot_uint

fun land_uint_uint
  (u1: uint, u2: uint):<> uint = "atspre_land_uint_uint"
overload land with land_uint_uint

fun lor_uint_uint
  (u1: uint, u2: uint):<> uint = "atspre_lor_uint_uint"
overload lor with lor_uint_uint

fun lxor_uint_uint
  (u1: uint, u2: uint):<> uint = "atspre_lxor_uint_uint"
overload lxor with lxor_uint_uint

(* ****** ****** *)

fun lsl_uint_int1
  (u: uint, n: Nat):<> uint = "atspre_lsl_uint_int1"
overload << with lsl_uint_int1

fun lsl_uint_uint
  (u: uint, n: uint):<> uint = "mac#atspre_lsl_uint_uint"
overload << with lsl_uint_uint

fun lsr_uint_int1
  (u: uint, n: Nat):<> uint = "atspre_lsr_uint_int1"
overload >> with lsr_uint_int1

fun lsr_uint_uint
  (u: uint, n: uint):<> uint = "mac#atspre_lsr_uint_uint"
overload >> with lsr_uint_uint

(* ****** ****** *)

symintr fprint_uint

fun fprint0_uint (out: FILEref, x: uint):<!exnref> void
  = "atspre_fprint_uint"

fun fprint1_uint {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: uint):<!exnref> void
  = "atspre_fprint_uint"

overload fprint_uint with fprint0_uint
overload fprint_uint with fprint1_uint
overload fprint with fprint_uint

(* ****** ****** *)

fun print_uint (u: uint):<!ref> void
  = "atspre_print_uint"

and prerr_uint (u: uint):<!ref> void
  = "atspre_prerr_uint"

overload print with print_uint
overload prerr with prerr_uint

(* ****** ****** *)
//
// stringization
//
fun tostrptr_uint
  (u: uint):<> strptr1 = "atspre_tostrptr_uint"
overload tostrptr with tostrptr_uint
fun tostring_uint (u: uint):<> string = "atspre_tostrptr_uint"
overload tostring with tostring_uint

(* ****** ****** *)

// signed integers (of indexed type)

(* ****** ****** *)

castfn int1_of_int (i: int):<> [i:int] int i
castfn int1_of_uint1 {i:nat} (i: uint i):<> int i
overload int1_of with int1_of_int
overload int1_of with int1_of_uint1

//

fun int1_of_string (s: string):<> [i:int] int i
  = "atspre_int1_of_string"
overload int1_of with int1_of_string

// arithmetic functions and comparison functions

fun iabs {i:int} (i: int i):<> [j:int | abs_r (i, j)] int j
  = "atspre_iabs"
overload abs with iabs

fun ineg {i:int} (i: int i):<> int (~i)
  = "atspre_ineg"
overload ~ with ineg

fun isucc {i:int} (i: int i):<> int (i + 1)
  = "atspre_isucc"
and ipred {i:int} (i: int i):<> int (i - 1)
  = "atspre_ipred"

overload succ with isucc
overload pred with ipred

fun iadd {i,j:int} (i: int i, j: int j):<> int (i+j)
  = "atspre_iadd"
and isub {i,j:int} (i: int i, j: int j):<> int (i-j)
  = "atspre_isub"
overload + with iadd
overload - with isub

fun imul {i,j:int}
  (i: int i, j: int j):<> int (i*j) // [j] must be a constant!
  = "atspre_imul"
and idiv {i,j:int | j <> 0}
  (i: int i, j: int j):<> int (i/j) // [j] must be a constant!
  = "atspre_idiv"
overload * with imul
overload / with idiv

fun igcd {i,j:int}
  (i: int i, j: int j):<> [r:int | r >= 0] int r = "atspre_igcd"
overload gcd with igcd

//

fun imul1
  (i: Int, j: Int):<> Int = "atspre_imul1"
and idiv1 {j:int | j <> 0}
  (i: Int, j: int j):<> Int = "atspre_idiv1"
fun igcd1 {i,j:int}
  (i: int i, j: int j):<> [r:int | r >= 0] int r = "atspre_igcd1"
// end of [igcd1]

//

fun imul2 {i,j:int}
  (i: int i, j: int j)
  :<> [p:int] (MUL (i, j, p) | int p) = "atspre_imul2"
fun igcd2 {i,j:int}
  (i: int i, j: int j)
  :<> [r:int] (GCD (i, j, r) | int r) = "atspre_igcd2"

//

fun nmul (i: Nat, j: Nat):<> Nat = "atspre_nmul"
fun ndiv (i: Nat, j: Pos):<> Nat = "atspre_ndiv"
//
// HX: there is no [ndiv1]
//
fun ndiv2 {i,j:int | i >= 0; j > 0}
  (i: int i, j: int j):<> [q:int] (DIV (i, j, q) | int q)
  = "atspre_ndiv2"
// end of [ndiv2]

//

fun nmod {i,j:int | i >= 0; j > 0} // [j] must be a constant!
  (i: int i, j: int j) :<> [q,r:nat | r < j; i == q*j + r] int r
  = "atspre_nmod"
fun nmod1 {i,j:int | i >= 0; j > 0} (i: int i, j: int j):<> natLt j
  = "atspre_nmod1"
fun nmod2 {i,j:int | i >= 0; j > 0}
  (i: int i, j: int j):<> [r:int] (MOD (i, j, r) | int r)
  = "atspre_nmod1"

(* ****** ****** *)
//
// HX: it is always inlined
// HX: it is not supported by ATS/Geizella
// HX: DIVMOD is declard in prelude/SATS/arith.sats
//
fun{}
divmod_int1_int1
  {m:nat; n:int | n > 0} (
  m: int m, n: int n
, r: &int? >> int r
) :<> #[q,r:nat | r < n] (DIVMOD (m, n, q, r) | int q)

(* ****** ****** *)

fun ilt {i,j:int} (i: int i, j: int j):<> bool (i < j)
  = "atspre_ilt"
and ilte {i,j:int} (i: int i, j: int j):<> bool (i <= j)
  = "atspre_ilte"
overload < with ilt
overload <= with ilte

fun igt {i,j:int} (i: int i, j: int j):<> bool (i > j)
  = "atspre_igt"
and igte {i,j:int} (i: int i, j: int j):<> bool (i >= j)
  = "atspre_igte"
overload > with igt
overload >= with igte

fun ieq {i,j:int} (i: int i, j: int j):<> bool (i == j)
  = "atspre_ieq"
and ineq {i,j:int} (i: int i, j: int j):<> bool (i <> j)
  = "atspre_ineq"
overload = with ieq
overload <> with ineq
overload != with ineq

fun icompare {i,j:int}
  (i: int i, j: int j):<> [k:int | sgn_r (i-j, k)] int k
  = "atspre_icompare"
overload compare with icompare

fun imax {i,j:int}
  (i: int i, j: int j):<> int (max (i,j)) = "atspre_imax"
and imin {i,j:int}
  (i: int i, j: int j):<> int (min (i,j)) = "atspre_imin"
overload max with imax
overload min with imin

fun ipow (base: Int, exponent: Nat):<> Int = "atspre_ipow"
overload pow with ipow

fun npow (base: Int, exponent: Nat):<> Nat = "atspre_npow"

fun ihalf {i:int}
  (i: int i):<> [q:int | div_r (i, 2, q)] int q = "atspre_ihalf"
// end of [ihalf]

fun nhalf {n:nat}
  (n: int n):<> [q:nat | ndiv_r (n, 2, q)] int q = "atspre_nhalf"
// end of [nhalf]

(* ****** ****** *)

// unsigned integers (of indexed type)

(* ****** ****** *)

castfn uint1_of_uint (i: uint):<> [i:nat] uint i

castfn uint1_of_int (i: int):<> [i:nat] uint i
castfn uint1_of_int1 {i:nat} (i: int i):<> uint i

overload uint1_of with uint1_of_int
overload uint1_of with uint1_of_int1

(* ****** ****** *)

// arithmetic functions and comparison functions

fun usucc {i:nat} (i: uint i):<> uint (i + 1)
  = "atspre_usucc"
and upred {i:pos} (i: uint i):<> uint (i - 1)
  = "atspre_upred"

overload succ with usucc
overload pred with upred

fun uadd {i,j:nat} (i: uint i, j: uint j):<> uint (i+j)
  = "atspre_uadd"
and usub {i,j:nat | i >= j} (i: uint i, j: uint j):<> uint (i-j)
  = "atspre_usub"
overload + with uadd
overload - with usub

fun umul {i,j:nat} (i: uint i, j: uint j):<> uint (i*j)
  = "atspre_umul"
and udiv {i,j:nat | j > 0} (i: uint i, j: uint j):<> uint (i/j)
  = "atspre_udiv"
and umod {i,j:nat | j > 0}
  (i: uint i, j: uint j):<> [r:int] (MOD (i, j, r) | uint r)
  = "atspre_umod"
overload * with umul
overload / with udiv
overload mod with umod

fun uimod {j:nat | j > 0}
  (i: uint, j: int j):<> [r:nat | r < j] int r = "atspre_uimod"
overload mod with uimod

fun ult {i,j:nat} (i: uint i, j: uint j):<> bool (i < j)
  = "atspre_ult"

and ulte {i,j:nat} (i: uint i, j: uint j):<> bool (i <= j)
  = "atspre_ulte"

fun ugt {i,j:nat} (i: uint i, j: uint j):<> bool (i > j)
  = "atspre_ugt"

and ugte {i,j:nat} (i: uint i, j: uint j):<> bool (i >= j)
  = "atspre_ugte"

fun ueq {i,j:nat} (i: uint i, j: uint j):<> bool (i == j)
  = "atspre_ueq"

and uneq {i,j:nat} (i: uint i, j: uint j):<> bool (i <> j)
  = "atspre_uneq"

overload < with ult
overload <= with ulte
overload > with ugt
overload >= with ugte
overload = with ueq
overload <> with uneq
overload != with uneq

fun umax {i,j:nat}
  (i: uint i, j: uint j):<> [k:int | max_r (i, j, k)] uint k
  = "atspre_umax"
and umin {i,j:nat}
  (i: uint i, j: uint j):<> [k:int | min_r (i, j, k)] uint k
  = "atspre_umin"

overload max with umax
overload min with umin

fun uhalf {i:nat} (i: uint i):<> uint (i/2)
  = "atspre_uhalf"

(* ****** ****** *)
//
// signed long integers (unindexed)
//
(* ****** ****** *)

stadef lint = lint_int_t0ype // indexed
typedef lint = int_long_t0ype // unindexed

castfn lint1_of_lint (i: lint):<> [i:int] lint i

fun lint_of_int (i: int):<> lint = "atspre_lint_of_int"
overload lint_of with lint_of_int
fun int_of_lint (li: lint):<> int = "atspre_int_of_lint"
overload int_of with int_of_lint

fun lint_of_uint (u: uint):<> lint = "atspre_lint_of_uint"
overload lint_of with lint_of_uint
fun uint_of_lint (li: lint):<> uint = "atspre_uint_of_lint"
overload uint_of with uint_of_lint

//
// HX: this function is based on [atol] in [stdlib.h]
//
fun lint_of_string (s: string):<> lint = "atspre_lint_of_string"
overload lint_of with lint_of_string

(* ****** ****** *)

// arithmetic functions and comparison functions

fun abs_lint (li: lint):<> lint = "atspre_abs_lint"
overload abs with abs_lint
fun neg_lint (li: lint):<> lint = "atspre_neg_lint"
overload ~ with neg_lint

fun succ_lint (li: lint):<> lint = "atspre_succ_lint"
and pred_lint (li: lint):<> lint = "atspre_pred_lint"
overload succ with succ_lint
overload pred with pred_lint

fun add_lint_lint
  (i: lint, j: lint):<> lint = "atspre_add_lint_lint"
and sub_lint_lint
  (i: lint, j: lint):<> lint = "atspre_sub_lint_lint"
overload + with add_lint_lint
overload - with sub_lint_lint

fun mul_lint_lint
  (i: lint, j: lint):<> lint = "atspre_mul_lint_lint"
and div_lint_lint
  (i: lint, j: lint):<> lint = "atspre_div_lint_lint"
overload * with mul_lint_lint
overload / with div_lint_lint

fun mod_lint_lint
  (i: lint, j: lint):<> lint = "atspre_mod_lint_lint"
overload mod with mod_lint_lint

fun lt_lint_lint (i: lint, j: lint):<> bool
  = "atspre_lt_lint_lint"
and lte_lint_lint (i: lint, j: lint):<> bool
  = "atspre_lte_lint_lint"
overload < with lt_lint_lint
overload <= with lte_lint_lint

fun gt_lint_lint (i: lint, j: lint):<> bool
  = "atspre_gt_lint_lint"
and gte_lint_lint (i: lint, j: lint):<> bool
  = "atspre_gte_lint_lint"
overload > with gt_lint_lint
overload >= with gte_lint_lint

fun eq_lint_lint (i: lint, j: lint):<> bool
  = "atspre_eq_lint_lint"
and neq_lint_lint (i: lint, j: lint):<> bool
  = "atspre_neq_lint_lint"
overload = with eq_lint_lint
overload <> with neq_lint_lint
overload != with neq_lint_lint

fun compare_lint_lint (i1: lint, i2: lint):<> Sgn
  = "atspre_compare_lint_lint"
overload compare with compare_lint_lint

fun max_lint_lint (i: lint, j: lint):<> lint
  = "atspre_max_lint_lint"
and min_lint_lint (i: lint, j: lint):<> lint
  = "atspre_min_lint_lint"

overload max with max_lint_lint
overload min with min_lint_lint

(* ****** ****** *)

symintr fprint_lint

fun fprint0_lint (out: FILEref, x: lint):<!exnref> void
  = "atspre_fprint_lint"
overload fprint_lint with fprint0_lint

fun fprint1_lint {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: lint):<!exnref> void
  = "atspre_fprint_lint"
overload fprint_lint with fprint1_lint

overload fprint with fprint_lint

fun print_lint (li: lint):<!ref> void = "atspre_print_lint"
overload print with print_lint

fun prerr_lint (li: lint):<!ref> void = "atspre_prerr_lint"
overload prerr with prerr_lint

(* ****** ****** *)
//
// stringization
//
fun tostrptr_lint
  (i: lint):<> strptr1 = "atspre_tostrptr_lint"
overload tostrptr with tostrptr_lint
fun tostring_lint (i: lint):<> string = "atspre_tostrptr_lint"
overload tostring with tostring_lint

(* ****** ****** *)
//
// signed long integers (indexed)
//
(* ****** ****** *)

fun lt_lint1_lint1 {i,j:int} (i: lint i, j: lint j):<> bool (i < j)
  = "atspre_lt_lint_lint"
and lte_lint1_lint1 {i,j:int} (i: lint i, j: lint j):<> bool (i <= j)
  = "atspre_lte_lint_lint"
overload < with lt_lint1_lint1
overload <= with lte_lint1_lint1

fun gt_lint1_lint1 {i,j:int} (i: lint i, j: lint j):<> bool (i > j)
  = "atspre_gt_lint_lint"
and gte_lint1_lint1 {i,j:int} (i: lint i, j: lint j):<> bool (i >= j)
  = "atspre_gte_lint_lint"
overload > with gt_lint1_lint1
overload >= with gte_lint1_lint1

fun eq_lint1_lint1 {i,j:int} (i: lint i, j: lint j):<> bool (i == j)
  = "atspre_lt_lint_lint"
and neq_lint1_lint1 {i,j:int} (i: lint i, j: lint j):<> bool (i <> j)
  = "atspre_lte_lint_lint"
overload = with eq_lint1_lint1
overload <> with neq_lint1_lint1
overload != with neq_lint1_lint1

fun compare_lint1_lint1
  {i,j:int} (i: lint i, j: lint j):<> [k:int | sgn_r (i-j, k)] int k
  = "atspre_compare_lint_lint"
overload compare with compare_lint1_lint1

(* ****** ****** *)
//
// unsigned long integers (unindexed)
//
(* ****** ****** *)
//
stadef ulint = ulint_int_t0ype // indexed
typedef ulint = uint_long_t0ype // unindexed
//
castfn ulint1_of_ulint (x: ulint):<> [i:nat] ulint i
castfn ulint1_of_lint1 {i:nat} (x: lint i):<> ulint i
//
castfn lint_of_ulint (x: ulint):<> lint
overload lint_of with lint_of_ulint
//
castfn ulint_of_lint (x: lint):<> ulint
overload ulint_of with ulint_of_lint
//
fun ulint_of_int (i: int):<> ulint = "atspre_ulint_of_int"
overload ulint_of with ulint_of_int
//
fun ulint_of_uint (u: uint):<> ulint = "atspre_ulint_of_uint"
overload ulint_of with ulint_of_uint
//
fun uint_of_ulint (ul: ulint):<> uint = "atspre_uint_of_ulint"
overload uint_of with uint_of_ulint
//
(* ****** ****** *)

// arithmetic functions and comparison functions

fun succ_ulint (lu: ulint):<> ulint
  = "atspre_succ_ulint"

and pred_ulint (lu: ulint):<> ulint
  = "atspre_pred_ulint"

overload succ with succ_ulint
overload pred with pred_ulint

fun add_ulint_ulint
  (i: ulint, j: ulint):<> ulint = "atspre_add_ulint_ulint"
and sub_ulint_ulint
  (i: ulint, j: ulint):<> ulint = "atspre_sub_ulint_ulint"
overload + with add_ulint_ulint
overload - with sub_ulint_ulint

fun mul_ulint_ulint
  (i: ulint, j: ulint):<> ulint = "atspre_mul_ulint_ulint"
and div_ulint_ulint
  (i: ulint, j: ulint):<> ulint = "atspre_div_ulint_ulint"
and mod_ulint_ulint
  (i: ulint, j: ulint):<> ulint = "atspre_mod_ulint_ulint"
overload * with mul_ulint_ulint
overload / with div_ulint_ulint
overload mod with mod_ulint_ulint

fun lt_ulint_ulint (i: ulint, j: ulint):<> bool
  = "atspre_lt_ulint_ulint"
and lte_ulint_ulint (i: ulint, j: ulint):<> bool
  = "atspre_lte_ulint_ulint"
overload < with lt_ulint_ulint
overload <= with lte_ulint_ulint

fun gt_ulint_ulint (i: ulint, j: ulint):<> bool
  = "atspre_gt_ulint_ulint"
and gte_ulint_ulint (i: ulint, j: ulint):<> bool
  = "atspre_gte_ulint_ulint"
overload > with gt_ulint_ulint
overload >= with gte_ulint_ulint

fun eq_ulint_ulint (i: ulint, j: ulint):<> bool
  = "atspre_eq_ulint_ulint"
and neq_ulint_ulint (i: ulint, j: ulint):<> bool
  = "atspre_neq_ulint_ulint"
overload = with eq_ulint_ulint
overload <> with neq_ulint_ulint
overload != with neq_ulint_ulint

fun compare_ulint_ulint (i1: ulint, i2: ulint):<> Sgn
  = "atspre_compare_ulint_ulint"
overload compare with compare_ulint_ulint

fun max_ulint_ulint (i: ulint, j: ulint):<> ulint
  = "atspre_max_ulint_ulint"

and min_ulint_ulint (i: ulint, j: ulint):<> ulint
  = "atspre_min_ulint_ulint"

overload max with max_ulint_ulint
overload min with min_ulint_ulint

(* ****** ****** *)

// bit operations

fun lnot_ulint (u: ulint):<> ulint
  = "atspre_lnot_ulint" (* bitwise *)
overload ~ with lnot_ulint

fun land_ulint_ulint (u1: ulint, u2: ulint):<> ulint
  = "atspre_land_ulint_ulint"

fun lor_ulint_ulint (u1: ulint, u2: ulint):<> ulint
  = "atspre_lor_ulint_ulint"

fun lxor_ulint_ulint (u1: ulint, u2: ulint):<> ulint
  = "atspre_lxor_ulint_ulint"

overload land with land_ulint_ulint
overload lor with lor_ulint_ulint
overload lxor with lxor_ulint_ulint

fun lsl_ulint_int1 (u: ulint, n: Nat):<> ulint
  = "atspre_lsl_ulint_int1"

and lsr_ulint_int1 (u: ulint, n: Nat):<> ulint
  = "atspre_lsr_ulint_int1"

overload << with lsl_ulint_int1
overload >> with lsr_ulint_int1

(* ****** ****** *)

symintr fprint_ulint

fun fprint0_ulint (out: FILEref, x: ulint):<!exnref> void
  = "atspre_fprint_ulint"

fun fprint1_ulint {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: ulint):<!exnref> void
  = "atspre_fprint_ulint"

overload fprint_ulint with fprint0_ulint
overload fprint_ulint with fprint1_ulint
overload fprint with fprint_ulint

(* ****** ****** *)

fun print_ulint (lu: ulint):<!ref> void
  = "atspre_print_ulint"
overload print with print_ulint

fun prerr_ulint (lu: ulint):<!ref> void
  = "atspre_prerr_ulint"
overload prerr with prerr_ulint

(* ****** ****** *)
//
// stringization
//
fun tostrptr_ulint
  (i: ulint):<> strptr1 = "atspre_tostrptr_ulint"
overload tostrptr with tostrptr_ulint
fun tostring_ulint (i: ulint):<> string = "atspre_tostrptr_ulint"
overload tostring with tostring_ulint

(* ****** ****** *)

// signed long long integers

(* ****** ****** *)

stadef llint = llint_int_t0ype // indexed
typedef llint = int_long_long_t0ype // unindexed

castfn llint1_of_llint (i: llint):<> [i:int] llint i

fun llint_of_int (i: int):<> llint = "atspre_llint_of_int"
overload llint_of with llint_of_int
fun int_of_llint (lli: llint):<> int = "atspre_int_of_llint"
overload int_of with int_of_llint

fun llint_of_double
  (d: double):<> llint = "atspre_llint_of_double"
overload llint_of with llint_of_double

//
// HX: This function is based on [atoll] in [stdlib.h]
//
fun llint_of_string
  (s: string):<> llint = "atspre_llint_of_string"
overload llint_of with llint_of_string

// arithmetic functions and comparison functions

fun abs_llint
  (lli: llint):<> llint = "atspre_abs_llint"
overload abs with abs_llint

fun neg_llint
  (lli: llint):<> llint = "atspre_neg_llint"
overload ~ with neg_llint

fun succ_llint
  (lli: llint):<> llint = "atspre_succ_llint"
and pred_llint
  (lli: llint):<> llint = "atspre_pred_llint"
overload succ with succ_llint
overload pred with pred_llint

fun add_llint_llint (i: llint, j: llint):<> llint
  = "atspre_add_llint_llint"
and sub_llint_llint (i: llint, j: llint):<> llint
  = "atspre_sub_llint_llint"
overload + with add_llint_llint
overload - with sub_llint_llint

fun mul_llint_llint (i: llint, j: llint):<> llint
  = "atspre_mul_llint_llint"
and div_llint_llint (i: llint, j: llint):<> llint
  = "atspre_div_llint_llint"
and mod_llint_llint (i: llint, j: llint):<> llint
  = "atspre_mod_llint_llint"
overload * with mul_llint_llint
overload / with div_llint_llint
overload mod with mod_llint_llint

fun lt_llint_llint (i: llint, j: llint):<> bool
  = "atspre_lt_llint_llint"
and lte_llint_llint (i: llint, j: llint):<> bool
  = "atspre_lte_llint_llint"
overload < with lt_llint_llint
overload <= with lte_llint_llint

fun gt_llint_llint (i: llint, j: llint):<> bool
  = "atspre_gt_llint_llint"
and gte_llint_llint (i: llint, j: llint):<> bool
  = "atspre_gte_llint_llint"
overload > with gt_llint_llint
overload >= with gte_llint_llint

fun eq_llint_llint (i: llint, j: llint):<> bool
  = "atspre_eq_llint_llint"
and neq_llint_llint (i: llint, j: llint):<> bool
  = "atspre_neq_llint_llint"
overload = with eq_llint_llint
overload <> with neq_llint_llint
overload != with neq_llint_llint

fun compare_llint_llint (i1: llint, i2: llint):<> Sgn
  = "atspre_compare_llint_llint"
overload compare with compare_llint_llint

fun max_llint_llint (i: llint, j: llint):<> llint
  = "atspre_max_llint_llint"

and min_llint_llint (i: llint, j: llint):<> llint
  = "atspre_min_llint_llint"

overload max with max_llint_llint
overload min with min_llint_llint

(* ****** ****** *)

symintr fprint_llint

fun fprint0_llint (out: FILEref, x: llint):<!exnref> void
  = "atspre_fprint_llint"

fun fprint1_llint {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: llint):<!exnref> void
  = "atspre_fprint_llint"

overload fprint_llint with fprint0_llint
overload fprint_llint with fprint1_llint
overload fprint with fprint_llint

(* ****** ****** *)

fun print_llint (lli: llint):<!ref> void
  = "atspre_print_llint"
overload print with print_llint

fun prerr_llint (lli: llint):<!ref> void
  = "atspre_prerr_llint"
overload prerr with prerr_llint

(* ****** ****** *)
//
// stringization
//
fun tostrptr_llint
  (i: llint):<> strptr1 = "atspre_tostrptr_llint"
overload tostrptr with tostrptr_llint
fun tostring_llint (i: llint):<> string = "atspre_tostrptr_llint"
overload tostring with tostring_llint

(* ****** ****** *)

// unsigned long long integers

(* ****** ****** *)

stadef ullint = ullint_int_t0ype // indexed
typedef ullint = uint_long_long_t0ype // unindexed

//

castfn ullint_of_llint (i: llint):<> ullint
castfn llint_of_ullint (u: ullint):<> llint

//

fun ullint_of_int
  (i: int):<> ullint = "atspre_ullint_of_int"
overload ullint_of with ullint_of_int

fun ullint_of_uint
  (u: uint):<> ullint = "atspre_ullint_of_uint"
overload ullint_of with ullint_of_uint

fun ullint_of_double (d: double):<> ullint
  = "atspre_ullint_of_double"
overload ullint_of with ullint_of_double

(* ****** ****** *)

// arithmetic functions and comparison functions

fun succ_ullint (llu: ullint):<> ullint
  = "atspre_succ_ullint"

and pred_ullint (llu: ullint):<> ullint
  = "atspre_pred_ullint"

overload succ with succ_ullint
overload pred with pred_ullint

fun add_ullint_ullint (i: ullint, j: ullint):<> ullint
  = "atspre_add_ullint_ullint"
and sub_ullint_ullint (i: ullint, j: ullint):<> ullint
  = "atspre_sub_ullint_ullint"
overload + with add_ullint_ullint
overload - with sub_ullint_ullint

fun mul_ullint_ullint (i: ullint, j: ullint):<> ullint
  = "atspre_mul_ullint_ullint"
and div_ullint_ullint (i: ullint, j: ullint):<> ullint
  = "atspre_div_ullint_ullint"
and mod_ullint_ullint (i: ullint, j: ullint):<> ullint
  = "atspre_mod_ullint_ullint"
overload * with mul_ullint_ullint
overload / with div_ullint_ullint
overload mod with mod_ullint_ullint

fun lt_ullint_ullint (i: ullint, j: ullint):<> bool
  = "atspre_lt_ullint_ullint"
and lte_ullint_ullint (i: ullint, j: ullint):<> bool
  = "atspre_lte_ullint_ullint"
overload < with lt_ullint_ullint
overload <= with lte_ullint_ullint

fun gt_ullint_ullint (i: ullint, j: ullint):<> bool
  = "atspre_gt_ullint_ullint"
and gte_ullint_ullint (i: ullint, j: ullint):<> bool
  = "atspre_gte_ullint_ullint"
overload > with gt_ullint_ullint
overload >= with gte_ullint_ullint

fun eq_ullint_ullint (i: ullint, j: ullint):<> bool
  = "atspre_eq_ullint_ullint"
and neq_ullint_ullint (i: ullint, j: ullint):<> bool
  = "atspre_neq_ullint_ullint"
overload = with eq_ullint_ullint
overload <> with neq_ullint_ullint
overload != with neq_ullint_ullint

//
// compare, max and min
//

fun compare_ullint_ullint (i1: ullint, i2: ullint):<> Sgn
  = "atspre_compare_ullint_ullint"
overload compare with compare_ullint_ullint

fun max_ullint_ullint (i: ullint, j: ullint):<> ullint
  = "atspre_max_ullint_ullint"

and min_ullint_ullint (i: ullint, j: ullint):<> ullint
  = "atspre_min_ullint_ullint"

overload max with max_ullint_ullint
overload min with min_ullint_ullint

(* ****** ****** *)

symintr fprint_ullint

fun fprint0_ullint (out: FILEref, x: ullint):<!exnref> void
  = "atspre_fprint_ullint"

fun fprint1_ullint {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: ullint):<!exnref> void
  = "atspre_fprint_ullint"

overload fprint_ullint with fprint0_ullint
overload fprint_ullint with fprint1_ullint
overload fprint with fprint_ullint

(* ****** ****** *)

fun print_ullint (llu: ullint):<!ref> void
  = "atspre_print_ullint"

and prerr_ullint (llu: ullint):<!ref> void
  = "atspre_prerr_ullint"

overload print with print_ullint
overload prerr with prerr_ullint

(* ****** ****** *)
//
// stringization
//
fun tostrptr_ullint
  (i: ullint):<> strptr1 = "atspre_tostrptr_ullint"
overload tostrptr with tostrptr_ullint
fun tostring_ullint (i: ullint):<> string = "atspre_tostrptr_ullint"
overload tostring with tostring_ullint

(* ****** ****** *)

typedef sint = int_short_t0ype
typedef usint = uint_short_t0ype

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [integer.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [integer.sats] *)
