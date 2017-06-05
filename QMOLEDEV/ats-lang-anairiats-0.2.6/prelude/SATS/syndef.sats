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

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

typedef atsyndef__List (a:t@ype) = [n:nat] list_t0ype_int_type (a, n)

(* ****** ****** *)

fun{a:t@ype} atsyndef__of_int (i: int): a

(* ****** ****** *)

fun{} atsyndef__list_is_nil {a:t@ype}
  {n:nat} (xs: list (a, n)):<> bool (n == 0)
fun{} atsyndef__list_is_cons {a:t@ype}
  {n:nat} (xs: list (a, n)):<> bool ( n > 0 )

(* ****** ****** *)

fun{a:t@ype}
atsyndef__list_uncons
  {n:pos} (xs: list (a, n), x: &a? >> a):<> list (a, n-1)
// end of [list_uncons]

(* ****** ****** *)

(*
fun{a:t@ype}
atsyndef__forlist_in_do
  (xs: List a, f: !(a) -<cloptr1> void): void
// end of [forlist_in_do]

fun{a:t@ype}
atsyndef__iforlist_in_do {n:nat}
  (xs: list (a, n), f: !(natLt n, a) -<cloptr1> void): void
// end of [iforlist_in_do]
*)

(* ****** ****** *)

(* end of [syndef.sats] *)
