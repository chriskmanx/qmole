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
//
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
//
(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [sortdef.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

// some sort definitions

sortdef igz = {a: int | a > 0}
    and ilz = {a: int | a < 0}
    and inz = {a: int | a <> 0}

sortdef nat = {a: int | a >= 0}
    and pos = {a: int | a > 0}
    and neg = {a: int | a < 0}

sortdef one = {a: int | 0 <= a; a < 1}
    and two = {a: int | 0 <= a; a < 2}
    and three = {a: int | 0 <= a; a < 3}
    and four = {a: int | 0 <= a; a < 4}
    and five = {a: int | 0 <= a; a < 5}
    and six = {a: int | 0 <= a; a < 6}
    and seven = {a: int | 0 <= a; a < 7}
    and eight = {a: int | 0 <= a; a < 8}
    and nine = {a: int | 0 <= a; a < 9}
    and ten = {a: int | 0 <= a; a < 10}

sortdef sgn = {a: int | ~1 <= a; a <= 1}

sortdef int8 = {i:int | ~power_2_7 <= i; i < power_2_7}
sortdef int16 = {i:int | ~power_2_15 <= i; i < power_2_15}

sortdef agz = {a:addr | a > null}
    and agez = {a:addr | a >= null}

(* ****** ****** *)
//
// HX-2009:
// should parameterized subset sorts be suported?
// sortdef intbtw (l: int, u: int) = {a: int | l <= a; a < u}
//
(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [sortdef.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

(* end of [sortdef.sats] *)
