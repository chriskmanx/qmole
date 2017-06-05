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

//
// HX:
// Note that [array0] is a persistent array with size information attached.
// This package is mostly for a beginner who is unfamiliar with ATS. After
// some exposure to dependent types and linear types, the programmer is
// recommended to use functions declared in [array.sats] instead.
//

(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [array0.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)
//
// HX: array0 (a) = ref (Arraysize a)
//
(* ****** ****** *)

(*
** HX-2011-01-12:
** it is changed from a fun to a castfn
*)
castfn array0_get_arrszref
  {a:viewt@ype} (A: array0 a):<> ref (Arraysize a)
// end of [array0_get_arrszref]

(* ****** ****** *)

fun array0_make_arrsz {a:viewt@ype}
  {n:nat} (arrsz: arraysize (a, n)):<> array0 (a)
// end of [array0_make_arrsz]

macdef array0 (x) = array0_make_arrsz ,(x)

(* ****** ****** *)

fun{a:t@ype}
array0_make_elt (asz: size_t, x: a):<> array0 a
// end of [array0_make_elt]

fun{a:t@ype} array0_make_lst (xs: list0 a):<> array0 a

(* ****** ****** *)
//
// HX: this is a polymorphic function
//
fun array0_size {a:t@ype} (A: array0 a):<!ref> size_t

(* ****** ****** *)

fun{a:t@ype}
array0_get_elt_at (A: array0 a, i: size_t):<!exnref> a
overload [] with array0_get_elt_at

fun{a:t@ype}
array0_set_elt_at (A: array0 a, i: size_t, x: a):<!exnref> void
overload [] with array0_set_elt_at

(* ****** ****** *)

fun{a:t@ype} array0_get_elt_at__intsz
  (A: array0 a, i: int):<!exnref> a
overload [] with array0_get_elt_at__intsz

fun{a:t@ype} array0_set_elt_at__intsz
  (A: array0 a, i: int, x: a):<!exnref> void
overload [] with array0_set_elt_at__intsz

(* ****** ****** *)

fun{a:t@ype} array0_foreach
  (A: array0 a, f: (&a) -<cloref> void):<!ref> void
// end of [array0_foreach]

fun{a:t@ype} array0_iforeach
  (A: array0 a, f: (size_t, &a) -<cloref> void):<!ref> void
// end of [array0_iforeach]

(* ****** ****** *)

fun{a:t@ype} array0_tabulate
  (asz: size_t, f: size_t -<cloref> a):<> array0 a
// end of [array0_tabulate]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [array0.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [array0.sats] *)
