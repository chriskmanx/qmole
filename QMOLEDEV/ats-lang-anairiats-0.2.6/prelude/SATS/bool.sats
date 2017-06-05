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
#print "Loading [bool.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

// some basic operations on boolean values

(* ****** ****** *)

castfn bool1_of_bool (x: bool):<> Bool

(* ****** ****** *)

fun bool_of_int (i: int):<> bool
fun int_of_bool (b: bool):<> int

fun bool1_of_int1 {i:int} (i: int i):<> bool (i <> 0)
fun int1_of_bool1 {b:bool} (b: bool b):<> int (int_of_bool b)

(* ****** ****** *)
//
// HX: the following two function are identical
//
fun neg_bool
  (b: bool):<> bool = "atspre_neg_bool"
overload ~ with neg_bool
overload not with neg_bool

fun add_bool_bool
  (b1: bool, b2: bool):<> bool = "atspre_add_bool_bool"
overload || with add_bool_bool

fun mul_bool_bool
  (b1: bool, b2: bool):<> bool = "atspre_mul_bool_bool"
overload && with mul_bool_bool

(* ****** ****** *)

fun lt_bool_bool
  (b1: bool, b2: bool):<> bool = "atspre_lt_bool_bool"
and lte_bool_bool
  (b1: bool, b2: bool):<> bool = "atspre_lte_bool_bool"
overload < with lt_bool_bool
overload <= with lte_bool_bool

fun gt_bool_bool
  (b1: bool, b2: bool):<> bool = "atspre_gt_bool_bool"
and gte_bool_bool
  (b1: bool, b2: bool):<> bool = "atspre_gte_bool_bool"
overload > with gt_bool_bool
overload >= with gte_bool_bool

fun eq_bool_bool
  (b1: bool, b2: bool):<> bool = "atspre_eq_bool_bool"
and neq_bool_bool
  (b1: bool, b2: bool):<> bool = "atspre_neq_bool_bool"
overload = with eq_bool_bool
overload <> with neq_bool_bool

fun compare_bool_bool
  (b1: bool, b2: bool):<> Sgn = "atspre_compare_bool_bool"
overload compare with compare_bool_bool

(* ****** ****** *)
//
// print functions for booleans
//
(* ****** ****** *)

symintr fprint_bool

fun fprint0_bool (out: FILEref, x: bool):<!exnref> void
  = "atspre_fprint_bool"
overload fprint_bool with fprint0_bool

fun fprint1_bool {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: bool):<!exnref> void
  = "atspre_fprint_bool"
overload fprint_bool with fprint1_bool

overload fprint with fprint_bool

fun print_bool (b: bool):<!ref> void = "atspre_print_bool"
overload print with print_bool
fun prerr_bool (b: bool):<!ref> void = "atspre_prerr_bool"
overload prerr with prerr_bool

(* ****** ****** *)
//
// HX: stringization
//
fun tostring_bool
  (b: bool):<> string = "atspre_tostring_bool"
overload tostring with tostring_bool

(* ****** ****** *)

fun neg_bool1 {b:bool}
  (b: bool b):<> bool (~b) = "atspre_neg_bool1"
overload ~ with neg_bool1
overload not with neg_bool1

(* ****** ****** *)

fun add_bool1_bool1 {b1,b2:bool}
  (b1: bool b1, b2: bool b2):<> bool (b1 || b2)
  = "atspre_add_bool1_bool1"
overload || with add_bool1_bool1

fun mul_bool1_bool1 {b1,b2:bool}
  (b1: bool b1, b2: bool b2):<> bool (b1 && b2)
  = "atspre_mul_bool1_bool1"
overload && with mul_bool1_bool1

(* ****** ****** *)

fun lt_bool1_bool1 {b1,b2:bool}
  (b1: bool b1, b2: bool b2):<> bool (~b1 && b2)
  = "atspre_lt_bool1_bool1"
and lte_bool1_bool1 {b1,b2:bool}
  (b1: bool b1, b2: bool b2):<> bool (~b1 || b2)
  = "atspre_lte_bool1_bool1"
overload < with lt_bool1_bool1
overload <= with lte_bool1_bool1

fun gt_bool1_bool1 {b1,b2:bool}
  (b1: bool b1, b2: bool b2):<> bool (b1 && ~b2)
  = "atspre_gt_bool1_bool1"
and gte_bool1_bool1 {b1,b2:bool}
  (b1: bool b1, b2: bool b2):<> bool (b1 || ~b2)
  = "atspre_gte_bool1_bool1"
overload > with gt_bool1_bool1
overload >= with gte_bool1_bool1

fun eq_bool1_bool1 {b1,b2:bool}
  (b1: bool b1, b2: bool b2):<> bool (b1 == b2)
  = "atspre_eq_bool1_bool1"
and neq_bool1_bool1 {b1,b2:bool}
  (b1: bool b1, b2: bool b2):<> bool (b1 <> b2)
  = "atspre_neq_bool1_bool1"
overload = with eq_bool1_bool1
overload <> with neq_bool1_bool1

(* ****** ****** *)

local

stadef b2i = int_of_bool

in // in of [in]

fun compare_bool1_bool1 {b1,b2:bool}
  (b1: bool b1, b2: bool b2):<> int (b2i b1 - b2i b2)
  = "atspre_compare_bool1_bool1"
overload compare with compare_bool1_bool1

end // end of [local]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [bool.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [bool.sats] *)
