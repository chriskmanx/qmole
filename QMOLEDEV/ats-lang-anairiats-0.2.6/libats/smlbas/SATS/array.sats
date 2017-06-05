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
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
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

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

//
// SML Basis Library: Array (http://www.standardml.org/Basis/array.html)
//

(* ****** ****** *)

abstype array_t0ype_type (a:t@ype)
typedef array (a:t@ype) = array_t0ype_type (a)

(* ****** ****** *)

// this one is undefined!
fun maxLen (): size_t // maximal array size allowed

(* ****** ****** *)

fun{a:t@ype} array (asz: size_t, ini: a): array a

fun{a:t@ype} fromList (xs: list0 a): array a

fun{a:t@ype} tabulate (asz: size_t, f: size_t -<cloref1> a): array a

(* ****** ****** *)

fun length {a:t@ype} (A: array a): size_t

(* ****** ****** *)

fun{a:t@ype} sub (A: array a, i: size_t): a
fun{a:t@ype} update (A: array a, i: size_t, x: a): void

(* ****** ****** *)

fun{a:t@ype} copy (src: array a, dst: array a, di: size_t): void

(* ****** ****** *)

fun{a:t@ype} app (f: a -<cloref1> void, A: array a): void
fun{a:t@ype} appi (f: (size_t, a) -<cloref1> void, A: array a): void

(* ****** ****** *)

fun{a:t@ype} modify (f: a -<cloref1> a, A: array a): void
fun{a:t@ype} modifyi (f: (size_t, a) -<cloref1> a, A: array a): void

(* ****** ****** *)

fun{a,b:t@ype} foldl (f: (a, b) -<cloref1> b, ini: b, A: array a): b 
fun{a,b:t@ype} foldli (f: (size_t, a, b) -<cloref1> b, ini: b, A: array a): b 

fun{a,b:t@ype} foldr (f: (a, b) -<cloref1> b, fin: b, A: array a): b 
fun{a,b:t@ype} foldri (f: (size_t, a, b) -<cloref1> b, fin: b, A: array a): b 

(* ****** ****** *)

fun{a:t@ype} find (f: a -<cloref1> bool, A: array a): option0 a
fun{a:t@ype} findi (f: (size_t, a) -<cloref1> bool, A: array a): option0 @(size_t, a)

(* ****** ****** *)

fun{a:t@ype} all (f: a -<cloref1> bool, A: array a): bool
fun{a:t@ype} exists (f: a -<cloref1> bool, A: array a): bool

(* ****** ****** *)

// collate: lexicographic ordering
// GREATER: > 0; EQUAL: = 0; LESS: < 0
fun{a:t@ype} collate (cmp: (a, a) -<cloref1> int, A1: array a, A2: array a): int

(* ****** ****** *)

(* end of [array.sats] *)
