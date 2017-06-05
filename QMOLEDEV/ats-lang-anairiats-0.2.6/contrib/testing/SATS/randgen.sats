(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
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

fun{a:t@ype} randgen (): a // HX: cannot implemented generically

(* ****** ****** *)

fun{a:t@ype}
array0_randgen {n:nat} (n: size_t n): array0 (a)

fun{a:t@ype}
array_randgen {n:nat} (n: size_t n): array (a, n)

(* ****** ****** *)

fun{a:t@ype}
array_ptr_randgen {n:nat} (n: size_t n)
  : [l:agz] (free_gc_v (a?, n, l), array_v (a, n, l) | ptr l)
// end of [array_ptr_randgen]

fun{a:t@ype}
array_ptr_randinit {n:nat} {l:addr} (
  pf: !array_v (a?, n, l) >> array_v (a, n, l) | p: ptr l, n: size_t n
) : void // end of [array_ptr_randinit]

(* ****** ****** *)

fun{a:t@ype}
list0_randgen {n:nat} (n: int n): list0 (a)

fun{a:t@ype}
list_randgen {n:nat} (n: int n): list (a, n)
fun{a:t@ype}
list_vt_randgen {n:nat} (n: int n): list_vt (a, n)

(* ****** ****** *)

fun{a:t@ype}
matrix0_randgen
  {m,n:nat} (m: size_t m, n: size_t n): matrix0 (a)
// end of [matrix0_randgen]

fun{a:t@ype}
matrix_randgen
  {m,n:nat} (m: size_t m, n: size_t n): matrix (a, m, n)
// end of [matrix_randgen]

(* ****** ****** *)

fun{a:t@ype}
matrix_ptr_randinit {m,n:nat} {l:addr} (
  pf: !matrix_v (a?, m, n, l) >> matrix_v (a, m, n, l)
| p: ptr l, m: size_t m, n: size_t n
) : void // end of [matrix_ptr_randinit]

(* ****** ****** *)

(* end of [randgen.sats] *)
