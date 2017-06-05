(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
// HX: generic fully indexed lists 
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

staload "libats/SATS/ilistp.sats"

(* ****** ****** *)

staload "libats/SATS/gflist.sats"

(* ****** ****** *)

implement{a}
gflist_length (xs) = let
  val (pf | xs) =
    list_of_gflist {a} (xs) // no-op casting
  // end of [val]
  val n = list_length<a> (xs)
in
  (pf | n)
end // end of [gflist_length]

(* ****** ****** *)

implement{a}
gflist_append
  {xs,ys} (xs, ys) = let
  val (_pf | xs) = list_of_gflist {a} (xs)
  val (_pf | ys) = list_of_gflist {a} (ys)
  val res = list_append (xs, ys)
  val [res:ilist] (_pf | res) = gflist_of_list (res)
  prval pfres = __assert () where {
    extern prfun __assert (): APPEND (xs, ys, res)
  } // end of [prval]
in
  (pfres | res)
end // end of [gflist_append]

(* ****** ****** *)

implement{a}
gflist_revapp
  {xs,ys} (xs, ys) = let
  val (_pf | xs) = list_of_gflist {a} (xs)
  val (_pf | ys) = list_of_gflist {a} (ys)
  val res = list_reverse_append (xs, ys)
  val [res:ilist] (_pf | res) = gflist_of_list (res)
  prval pfres = __assert () where {
    extern prfun __assert (): REVAPP (xs, ys, res)
  } // end of [prval]
in
  (pfres | res)
end // end of [gflist_append]

(* ****** ****** *)

(* end of [gflist.dats] *)
