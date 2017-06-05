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

(* ****** ****** *)

fun{a:t@ype}
fprint_elt (out: FILEref, x: a): void // HX: cannot implemented generically

(* ****** ****** *)

fun{a:t@ype}
array0_fprint_elt (
  out: FILEref, A: array0 a, sep: !READ(string)
) : void // end of [array0_fprint_elt]

fun{a:t@ype}
array0_fprint_fun (
  out: FILEref, A: array0 a, sep: !READ(string)
, pr: (FILEref, a) -> void
) : void // end of [array0_fprint_fun]

(* ****** ****** *)

fun{a:t@ype}
array_fprint_elt {n:nat} (
  out: FILEref
, A: array (a, n), n:  size_t n, sep: !READ(string)
) : void // end of [array_fprint_elt]

fun{a:t@ype}
array_fprint_fun {n:nat} (
  out: FILEref
, A: array (a, n), n:  size_t n, sep: !READ(string)
, pr: (FILEref, a) -> void
) : void // end of [array_fprint_fun]

(* ****** ****** *)

fun{a:t@ype}
array_ptr_fprint_elt {n:nat} (
  out: FILEref
, A: &(@[a][n]), n: size_t n, sep: !READ(string)
) : void // end of [array_ptr_fprint_elt]

fun{a:t@ype}
array_ptr_fprint_fun {n:nat} (
  out: FILEref
, A: &(@[a][n]), n: size_t n, sep: !READ(string)
, pr: (FILEref, a) -> void
) : void // end of [array_ptr_fprint_fun]

(* ****** ****** *)

fun{a:t@ype}
list0_fprint_elt (
  out: FILEref, xs: list0 (a), sep: !READ(string)
) : void // end of [list0_fprint_elt]

fun{a:t@ype}
list0_fprint_fun (
  out: FILEref, xs: list0 (a), sep: !READ(string)
, pr: (FILEref, a) -> void
) : void // end of [list0_fprint_fun]

(* ****** ****** *)

fun{a:t@ype}
list_fprint_elt (
  out: FILEref, xs: List (a), sep: !READ(string)
) : void // end of [list_fprint_elt]

fun{a:t@ype}
list_fprint_fun (
  out: FILEref, xs: List (a), sep: !READ(string)
, pr: (FILEref, a) -> void
) : void // end of [list_fprint_fun]

(* ****** ****** *)

fun{a:t@ype}
list_vt_fprint_elt {n:nat} (
  out: FILEref, xs: !list_vt (a, n), sep: !READ(string)
) : void // end of [list_vt_fprint_elt]

fun{a:t@ype}
list_vt_fprint_fun {n:nat} (
  out: FILEref, xs: !list_vt (a, n), sep: !READ(string)
, pr: (FILEref, a) -> void
) : void // end of [list_vt_fprint]

(* ****** ****** *)

fun{a:t@ype}
matrix0_fprint_elt (
  out: FILEref
, M: matrix0 (a)
, sep1: !READ(string), sep2: !READ(string)
) : void // end of [matrix0_fprint_elt]

fun{a:t@ype}
matrix0_fprint_fun (
  out: FILEref
, M: matrix0 (a)
, sep1: !READ(string), sep2: !READ(string)
, pr: (FILEref, a) -> void
) : void // end of [matrix0_fprint_fun]

(* ****** ****** *)

fun{a:t@ype}
matrix_fprint_elt
  {m,n:nat} {l:addr} (
  out: FILEref
, M: matrix (a, m, n), m: size_t m, n: size_t n
, sep1: !READ(string), sep2: !READ(string)
) : void // end of [matrix_fprint_elt]

fun{a:t@ype}
matrix_fprint_fun
  {m,n:nat} {l:addr} (
  out: FILEref
, M: matrix (a, m, n), m: size_t m, n: size_t n
, sep1: !READ(string), sep2: !READ(string)
, pr: (FILEref, a) -> void
) : void // end of [matrix_fprint_fun]

(* ****** ****** *)

fun{a:t@ype}
matrix_ptr_fprint_elt
  {m,n:nat} {l:addr} (
  pf: !matrix_v (a, m, n, l) 
| out: FILEref
, p: ptr l, m: size_t m, n: size_t n
, sep1: !READ(string), sep2: !READ(string)
) : void // end of [matrix_ptr_fprint_elt]

fun{a:t@ype}
matrix_ptr_fprint_fun
  {m,n:nat} {l:addr} (
  pf: !matrix_v (a, m, n, l) 
| out: FILEref
, p: ptr l, m: size_t m, n: size_t n
, sep1: !READ(string), sep2: !READ(string)
, pr: (FILEref, a) -> void
) : void // end of [matrix_ptr_fprint_fun]

(* ****** ****** *)

(* end of [fprint.sats] *)
