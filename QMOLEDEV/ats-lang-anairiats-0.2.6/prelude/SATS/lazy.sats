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

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [lazy.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

exception StreamSubscriptException of ()

(* ****** ****** *)
//
// HX-2010-07-31: please note that !laz is defined to be (1,~ref)
//
(* ****** ****** *)

fun{a:t@ype}
stream_filter_fun
  (xs: stream a, p: a -<!laz> bool):<!laz> stream a
// end of [stream_filter_fun]

fun{a:t@ype}
stream_filter_cloref
  (xs: stream a, p: a -<cloref,!laz> bool):<!laz> stream a
// end of [stream_filter_cloref]

(* ****** ****** *)

fun{a:t@ype}{b:t@ype}
stream_map_fun
  (xs: stream a, f: a -<!laz> b):<!laz> stream b
// end of [stream_map_fun]

fun{a:t@ype}{b:t@ype}
stream_map_cloref
  (xs: stream a, f: a -<cloref,!laz> b):<!laz> stream b
// end of [stream_map_cloref]

(* ****** ****** *)

fun{a1,a2:t@ype}{b:t@ype}
stream_map2_fun
  (xs1: stream a1, xs2: stream a2, f: (a1, a2) -<!laz> b)
  :<!laz> stream b
// end of [stream_map2_fun]

fun{a1,a2:t@ype}{b:t@ype}
stream_map2_cloref
  (xs1: stream a1, xs2: stream a2, f: (a1, a2) -<cloref,!laz> b)
  :<!laz> stream b
// end of [stream_map2_cloref]

(* ****** ****** *)

fun{a:t@ype}
stream_ordmerge_fun
  (xs1: stream a, xs2: stream a, lte: (a, a) -<!laz> bool)
  :<!laz> stream a
// end of [stream_ordmerge_fun]

fun{a:t@ype}
stream_ordmerge_cloref
  (xs1: stream a, xs2: stream a, lte: (a, a) -<cloref,!laz> bool)
  :<!laz> stream a
// end of [stream_ordmerge_cloref]

(* ****** ****** *)

fun{a:t@ype} stream_nth (xs: stream a, i: Nat):<!exn> a

(* ****** ****** *)

fun{a:t@ype}
stream_take {n:nat}
  (xs: stream a, n: int n):<fun> [k:nat | k <= n] list_vt (a, k)
// end of [stream_take]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [lazy.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [lazy.sats] *)
