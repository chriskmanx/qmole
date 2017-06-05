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

// note that a byte is just an unsigned char (uchar)

(* ****** ****** *)

#define BYTE_MAX 255 // 2^8 - 1

(* ****** ****** *)

castfn byte_of_char (c: char):<> byte
(*
  = "atspre_byte_of_char"
*)

castfn char_of_byte (b: byte):<> char
(*
  = "atspre_char_of_byte"
*)

castfn byte_of_uchar (c: uchar):<> byte
castfn uchar_of_byte (b: byte):<> uchar

(* ****** ****** *)

fun byte_of_int (i: int):<> byte = "atspre_byte_of_int"
and int_of_byte (b: byte):<> int = "atspre_int_of_byte"

fun byte_of_int1 {i:nat | i <= BYTE_MAX} (i: int i):<> byte
  = "atspre_byte_of_int"
and int1_of_byte (b: byte):<> [i:nat | i <= BYTE_MAX] int i
  = "atspre_int_of_byte"

fun byte_of_uint (u: uint):<> byte = "atspre_byte_of_uint"
and uint_of_byte (b: byte):<> uint = "atspre_uint_of_byte"

fun byte_of_uint1 {i:nat | i <= BYTE_MAX} (i: uint i):<> byte
  = "atspre_byte_of_int"
and uint1_of_byte (b: byte):<> [i:nat | i <= BYTE_MAX] uint i
  = "atspre_int_of_byte"

//
// arithmetic functions and comparison functions
//

fun succ_byte (b: byte):<> byte = "atspre_succ_byte"
overload succ with succ_byte
fun pred_byte (b: byte):<> byte = "atspre_pred_byte"
overload pred with pred_byte

fun add_byte_byte
  (b1: byte, b2: byte):<> byte = "atspre_add_byte_byte"
overload + with add_byte_byte

fun sub_byte_byte
  (b1: byte, b2: byte):<> byte = "atspre_suc_byte_byte"
overload - with sub_byte_byte

fun mul_byte_byte
  (b1: byte, b2: byte):<> byte = "atspre_mul_byte_byte"
overload * with mul_byte_byte

fun div_byte_byte
  (b1: byte, b2: byte):<> byte = "atspre_div_byte_byte"
overload / with div_byte_byte

//
// some operations on bytes
//

fun lt_byte_byte
  (b1: byte, b2: byte):<> bool = "atspre_lt_byte_byte"
overload < with lt_byte_byte
fun lte_byte_byte
  (b1: byte, b2: byte):<> bool = "atspre_lte_byte_byte"
overload <= with lte_byte_byte
fun gt_byte_byte
  (b1: byte, b2: byte):<> bool = "atspre_gt_byte_byte"
overload > with gt_byte_byte
fun gte_byte_byte
  (b1: byte, b2: byte):<> bool = "atspre_gte_byte_byte"
overload >= with gte_byte_byte

fun eq_byte_byte
  (b1: byte, b2: byte):<> bool = "atspre_eq_byte_byte"
overload = with eq_byte_byte
fun neq_byte_byte
  (b1: byte, b2: byte):<> bool = "atspre_neq_byte_byte"
overload <> with neq_byte_byte

//
// bit operations
//

fun lnot_byte
  (b: byte):<> byte (*bitwise*) = "atspre_lnot_byte"
overload ~ with lnot_byte

fun land_byte_byte
  (b1: byte, b2: byte):<> byte = "atspre_land_byte_byte"
overload land with land_byte_byte

fun lor_byte_byte
  (b1: byte, b2: byte):<> byte = "atspre_lor_byte_byte"
overload lor with lor_byte_byte

fun lxor_byte_byte
  (b1: byte, b2: byte):<> byte = "atspre_lxor_byte_byte"
overload lxor with lxor_byte_byte

fun lsl_byte_int1
  (b: byte, n: Nat):<> byte = "atspre_lsl_byte_int1"
overload << with lsl_byte_int1
fun lsr_byte_int1
  (b: byte, n: Nat):<> byte = "atspre_lsr_byte_int1"
overload >> with lsr_byte_int1

(* ****** ****** *)
//
// print functions for characters
//
symintr fprint_byte
fun fprint0_byte
  (out: FILEref, x: byte):<!ref> void = "atspre_fprint_byte"
overload fprint_byte with fprint0_byte
fun fprint1_byte {m:file_mode}
  (pf: file_mode_lte (m, w) | out: !FILE m, x: byte):<!ref> void
  = "atspre_fprint_byte"
overload fprint_byte with fprint1_byte
overload fprint with fprint_byte

fun print_byte (b: byte):<!ref> void = "atspre_print_byte"
and prerr_byte (b: byte):<!ref> void = "atspre_prerr_byte"
overload print with print_byte
overload prerr with prerr_byte

(* ****** ****** *)

(* end of [bytes.sats] *)
