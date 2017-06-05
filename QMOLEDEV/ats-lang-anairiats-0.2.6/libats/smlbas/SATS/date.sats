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
// SML Basis Library: Date (http://www.standardml.org/Basis/date.html)
//

(* ****** ****** *)

staload TIME = "libats/smlbas/SATS/time.sats"

(* ****** ****** *)

datatype weekday =
  | Mon | Tue | Wed | Thu | Fri | Sat | Sun
// end of [weekday]

datatype month =
  | Jan | Feb | Mar | Apr | May | Jun
  | Jul | Aug | Sep | Oct | Nov | Dec
// end of [month]

abstype date // a boxed abstract type

exception Date of ()
  
(* ****** ****** *)

fun date (
    year: int, month: month, day: int
  , hour: int, minute: int, second: int
  , offset: option0 ($TIME.time)
  ) : date
// end of [date]  

fun year (_: date): int 
fun month (_: date): month
fun day (_: date): int
fun hour (_: date): int
fun minute (_: date): int
fun second (_: date): int
fun weekday (_: date): weekday
fun yearday (_: date): int
fun offset (_: date): option0 ($TIME.time)
fun isDst (_: date): option0 (bool)

// fun localOffset (): $Time.time

fun compare_date_date (d1: date, d2: date): int

(* ****** ****** *)

fun toString (d: date): string

fun fromString (s: string): date

(* ****** ****** *)

(* end of [date.sats] *)
