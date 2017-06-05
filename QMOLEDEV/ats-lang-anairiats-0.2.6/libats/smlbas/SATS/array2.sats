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
// SML Basis Library: Array2 (http://www.standardml.org/Basis/array2.html)
//

(* ****** ****** *)

abstype array2_t0ype_type (a:t@ype)
typedef array (a:t@ype) = array2_t0ype_type (a)

(* ****** ****** *)

typedef region (a:t@ype) = '{
  base= array (a)
, row= int, col= int
, nrows= option0 int, ncols= option0 int
} // end of [region]

datatype traversal = RowMajor | ColMajor

(* ****** ****** *)

fun{a:t@ype} array
  (row: size_t, col: size_t, ini: a): array (a)
// end of [array]

(* ****** ****** *)

fun{a:t@ype} fromList (xss: list0 (list0 a)): array (a)

(* ****** ****** *)

// both [row] and [col] need to be positive; otherwize; an exception (Size)
// is raised.
fun{a:t@ype} tabulate (
    trv: traversal, row: size_t, col: size_t, f: (size_t, size_t) -<cloref1> a
  ) : array (a)
// end of [tabulate]

(* ****** ****** *)

fun{a:t@ype} sub (M: array a, i: size_t, j: size_t): a
fun{a:t@ype} update (M: array a, i: size_t, j: size_t, x: a): void

(* ****** ****** *)

fun dimensions {a:t@ype} (M: array a): @(size_t, size_t)

fun nCols {a:t@ype} (M: array a): size_t
and nRows {a:t@ype} (M: array a): size_t

(* ****** ****** *)

fun{a:t@ype} app
  (trv: traversal, f: a -<cloref1> void, M: array a): void
// end of [app]

fun{a,b:t@ype} fold (
    trv: traversal, f: (a, b) -<cloref1> b, ini: b, M: array a
  ) : void
// end of [fold]

fun{a:t@ype} modify
  (trv: traversal, f: a -<cloref1> a, M: array a): void
// end of [modify]

(* ****** ****** *)

(*

// not implemented
val row : 'a array * int -> 'a Vector.vector
val column : 'a array * int -> 'a Vector.vector
val copy ...
val appi ...
val foldi ...
val modifyi ...

*)

(* ****** ****** *)

(* end of [array2.sats] *)
