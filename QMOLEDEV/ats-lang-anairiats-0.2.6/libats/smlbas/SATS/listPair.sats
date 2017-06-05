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
// SML Basis Library: ListPair (http://www.standardml.org/Basis/list-pair.html)
//
(* ****** ****** *)

exception UnequalLengths of ()

(* ****** ****** *)

fun{a,b:t@ype} zip (xs: list0 a, ys: list0 b): list0 @(a, b)
fun{a,b:t@ype} zipEq (xs: list0 a, ys: list0 b): list0 @(a, b)

fun{a,b:t@ype} unzip (xys: list0 @(a, b)): @(list0 a, list0 b)

(* ****** ****** *)

fun{a,b:t@ype} app
  (f: (a, b) -<cloref1> void, xs: list0 a, ys: list0 b): void
// end of [app]

fun{a,b:t@ype} appEq
  (f: (a, b) -<cloref1> void, xs: list0 a, ys: list0 b): void
// end of [appEq]

(* ****** ****** *)

fun{a,b:t@ype}{c:t@ype} map
  (f: (a, b) -<cloref1> c, xs: list0 a, ys: list0 b): list0 c
// end of [map]

fun{a,b:t@ype}{c:t@ype} mapEq
  (f: (a, b) -<cloref1> c, xs: list0 a, ys: list0 b): list0 c
// end of [mapEq]

(* ****** ****** *)

fun{a,b:t@ype}{c:t@ype} foldl
  (f: (a, b, c) -<cloref1> c, ini: c, xs: list0 a, ys: list0 b): c
// end of [foldl]

fun{a,b:t@ype}{c:t@ype} foldlEq
  (f: (a, b, c) -<cloref1> c, ini: c, xs: list0 a, ys: list0 b): c
// end of [foldlEq]

(* ****** ****** *)

fun{a,b:t@ype}{c:t@ype} foldr
  (f: (a, b, c) -<cloref1> c, ini: c, xs: list0 a, ys: list0 b): c
// end of [foldr]

fun{a,b:t@ype}{c:t@ype} foldrEq
  (f: (a, b, c) -<cloref1> c, ini: c, xs: list0 a, ys: list0 b): c
// end of [foldrEq]

(* ****** ****** *)

fun{a,b:t@ype} all
  (f: (a, b) -<cloref1> bool, xs: list0 a, ys: list0 b): bool
// end of [all]

fun{a,b:t@ype} allEq
  (f: (a, b) -<cloref1> bool, xs: list0 a, ys: list0 b): bool
// end of [allEq]

(* ****** ****** *)

fun{a,b:t@ype} exists
  (f: (a, b) -<cloref1> bool, xs: list0 a, ys: list0 b): bool
// end of [exists]

(* ****** ****** *)

(* end of [listPair.sats] *)
