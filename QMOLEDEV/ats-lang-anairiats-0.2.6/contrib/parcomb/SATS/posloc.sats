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
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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
** For recording location information for concrete syntax
*)

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Start Time: December 2008

(* ****** ****** *)

abstype filename_t // boxed type

fun filename_make_string (name: string): filename_t

fun fprint_filename
  (out: FILEref, fil: filename_t): void
overload fprint with fprint_filename
fun print_filename (loc: filename_t): void
overload print with print_filename
fun prerr_filename (loc: filename_t): void
overload prerr with prerr_filename

(* ****** ****** *)

val filename_none: filename_t
val filename_stdin: filename_t

fun filename_pop (): void
fun filename_push (x: filename_t): void

fun filename_get_current ():<> filename_t

(* ****** ****** *)

abstype position_t // boxed type

typedef lint = int_long_t0ype
fun position_line (p: position_t):<> int
fun position_loff (p: position_t):<> int
fun position_toff (p: position_t):<> lint

(* ****** ****** *)

fun fprint_position
  (fil: FILEref, pos: position_t): void
overload fprint with fprint_position
fun print_position (pos: position_t): void = "lexing_print_position"
overload print with print_position
fun prerr_position (pos: position_t): void = "lexing_prerr_position"
overload prerr with prerr_position

(* ****** ****** *)

val position_origin: position_t
fun position_next (p: position_t, c: char):<> position_t

fun lt_position_position (p1: position_t, p2: position_t):<> bool
overload < with lt_position_position
fun lte_position_position (p1: position_t, p2: position_t):<> bool
overload <= with lte_position_position

fun eq_position_position (p1: position_t, p2: position_t):<> bool
overload = with eq_position_position
fun neq_position_position (p1: position_t, p2: position_t):<> bool
overload <> with neq_position_position

(* ****** ****** *)

abstype location_t // boxed type

val location_none: location_t
fun location_make (_1: position_t, _2: position_t):<> location_t
fun location_combine (_1: location_t, _2: location_t):<> location_t

fun fprint_location
  (out: FILEref, loc: location_t): void
overload fprint with fprint_location
fun print_location (loc: location_t): void
overload print with print_location
fun prerr_location (loc: location_t): void
overload prerr with prerr_location

fun tostrptr_location (loc: location_t): strptr0

(* ****** ****** *)

(* end of [posloc.sats] *)
