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
// SML Basis Library: List (http://www.standardml.org/Basis/list.html)
//

(* ****** ****** *)

fun null {a:t@ype} (xs: list0 a): bool

fun{a:t@ype} length (xs: list0 a): int

fun{a:t@ype} append (xs: list0 a, ys: list0 a): list0 a

fun{a:t@ype} hd (xs: list0 a): a 
fun{a:t@ype} tl (xs: list0 a): list0 a 

fun{a:t@ype} last (xs: list0 a): a 

fun{a:t@ype}
getItem (xs: list0 a): option0 @(a, list0 a)

fun{a:t@ype} nth (xs: list0 a, n: int): a

fun{a:t@ype} take (xs: list0 a, i: int): list0 a
fun{a:t@ype} drop (xs: list0 a, i: int): list0 a
 
fun{a:t@ype} rev (xs: list0 a): list0 a
fun{a:t@ype} revAppend (xs: list0 a, ys: list0 a): list0 a

fun{a:t@ype} concat (xs: list0 (list0 a)): list0 a 
    
(* ****** ****** *)

fun{a:t@ype} app (f: a -<cloref1> void, xs: list0 a): void

fun{a:t@ype}{b:t@ype}
map (f: a -<cloref1> b, xs: list0 a) : list0 b

fun{a:t@ype}{b:t@ype}
mapPartial (f: a -<cloref1> option0 b, xs: list0 a) : list0 b

(* ****** ****** *)

fun{a:t@ype}
find (f: a -<cloref1> bool, xs: list0 a): option0 a
fun{a:t@ype}
filter (f: a -<cloref1> bool, xs: list0 a): list0 a

(* ****** ****** *)

fun{a:t@ype}
partition (f: a -<cloref1> bool, xs: list0 a): @(list0 a, list0 a)

(* ****** ****** *)

fun{a,b:t@ype}
foldl (f: (a, b) -<cloref1> b, ini: b, xs: list0 a): b

fun{a,b:t@ype}
foldr (f: (a, b) -<cloref1> b, snk: b, xs: list0 a): b

(* ****** ****** *)

fun{a:t@ype} all (f: a -<cloref1> bool, xs: list0 a): bool
fun{a:t@ype} exists (f: a -<cloref1> bool, xs: list0 a): bool

(* ****** ****** *)

fun{a:t@ype}
tabulate (lsz: int, f: int -<cloref1> a): list0 a

fun{a:t@ype} collate
  (cmp: (a, a) -<cloref1> int, xs: list0 a, ys: list0 a): int 
// end of [collate]

(* ****** ****** *)

(* end of [list.sats] *)
