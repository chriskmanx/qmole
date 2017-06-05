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
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
// Time: December, 2010
//
(* ****** ****** *)
//
// HX: generic fully indexed lists (gflists)
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

staload "libats/SATS/ilistp.sats" // for handling integer sequences

(* ****** ****** *)

abst@ype
elt_t0ype_int (a:t@ype, x:int) = a
stadef elt = elt_t0ype_int
typedef elt (a:t@ype) = [x:int] elt (a, x)
(*
// HX-2011-09-11:
abst@ype elt (a:t@ype, x:int) = !a // according to the new syntax proposal
*)

castfn eltencode : {a:t@ype} a -> elt (a)
castfn eltdecode : {a:t@ype} elt (a) -> a

(* ****** ****** *)

datatype
gflist (a:t@ype, ilist) =
  | {x:int} {xs:ilist}
    gflist_cons (a, ilist_cons (x, xs)) of (elt (a, x), gflist (a, xs))
  | gflist_nil (a, ilist_nil) of ()
// end of [gflist]

(* ****** ****** *)

castfn list_of_gflist {a:t@ype} {xs:ilist}
  (xs: gflist (a, xs)):<> [n:nat] (LENGTH (xs, n) | list (a, n))
// end of [list_of_gflist]

castfn gflist_of_list {a:t@ype} {n:int}
  (xs: list (a, n)):<> [xs:ilist] (LENGTH (xs, n) | gflist (a, xs))
// end of [gflist_of_list]

(* ****** ****** *)

fun{a:t@ype}
gflist_length {xs:ilist}
  (xs: gflist (a, xs)):<> [n:nat] (LENGTH (xs, n) | int n)
// end of [gflist_length]

(* ****** ****** *)

fun{a:t@ype}
gflist_append {xs1,xs2:ilist}
  (xs1: gflist (a, xs1), xs2: gflist (a, xs2))
  : [res:ilist] (APPEND (xs1, xs2, res) | gflist (a, res))
// end of [gflist_append]

fun{a:t@ype}
gflist_revapp {xs1,xs2:ilist}
  (xs1: gflist (a, xs1), xs2: gflist (a, xs2))
  : [res:ilist] (REVAPP (xs1, xs2, res) | gflist (a, res))
// end of [gflist_revapp]

(* ****** ****** *)

(* end of [gflist.sats] *)
