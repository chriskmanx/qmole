(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
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
**
** A ordered set implementation
** based on randomized binary search trees
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: October, 2011
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

%{#
#include "libats/CATS/linordset_randbst.cats"
%} // end of [%{#]

(* ****** ****** *)

sortdef t0p = t@ype

(* ****** ****** *)

absviewtype
set_t0ype_type (elt: t@ype+)
stadef set = set_t0ype_type

(* ****** ****** *)

typedef cmp (a:t@ype) = (a, a) -<cloref> int

fun{a:t0p}
compare_elt_elt (x1: a, x2: a, cmp: cmp (a)):<> int

(* ****** ****** *)

fun{} linordset_make_nil {a:t@ype} ():<> set (a)
fun{a:t0p} linordset_make_sing (x0: a):<> set (a) // singleton set

(* ****** ****** *)

fun{} linordset_is_empty {a:t0p} (xs: !set a):<> bool
fun{} linordset_isnot_empty {a:t0p} (xs: !set a):<> bool

(* ****** ****** *)
//
// HX: the time complexity of this function is O(1)
fun{a:t@ype} linordset_size (xs: !set a):<> size_t
//
// HX: the time complexity
fun{a:t@ype} // of this function is O(n) where n is
linordset_height (xs: !set a):<> size_t // the size of [xs]
//
(* ****** ****** *)

fun{a:t@ype} linordset_free (xs: set (a)):<> void

(* ****** ****** *)

fun{a:t@ype}
linordset_is_member (xs: !set a, x0: a, cmp: cmp a):<> bool
fun{a:t@ype}
linordset_isnot_member (xs: !set a, x0: a, cmp: cmp a):<> bool

(* ****** ****** *)
//
// HX: the return value [-1] means that [x0] is not found
//
fun{a:t0p}
linordset_get_order (xs: !set a, x0: a, cmp: cmp a):<> int

(* ****** ****** *)

absviewtype rngobj_viewtype = ptr
viewtypedef rngobj = rngobj_viewtype

fun linordset_rngobj_make_drand48
  ():<> rngobj = "atslib_linordset_rngobj_make_drand48"

fun linordset_rngobj_free
  (obj: rngobj):<> void = "atslib_linordset_rngobj_free"

(* ****** ****** *)

fun{a:t0p}
linordset_insert ( // O(log(|xs|))
  obj: !rngobj
, xs: &set (a), x0: a, cmp: cmp (a)
) :<> bool(*[x0] alreay exists in [xs]*)

(* ****** ****** *)

fun{a:t@ype}
linordset_remove ( // O(log(|xs|))
  obj: !rngobj
, xs: &set (a), x0: a, cmp: cmp (a)
) :<> bool(*[x0] removed/not: true/false*)

(* ****** ****** *)

fun{a:t0p}
linordset_ordget {d:nat} ( // O(log(|xs|))
  xs: !set (a), d: int d, x0: &a? >> opt (a, b)
) :<> #[b:bool] bool (b) // true/false : found/not

fun{a:t0p}
linordset_ordrem {d:nat} ( // O(log(|xs|))
  obj: !rngobj, xs: &set (a), d: int d, x0: &a? >> opt (a, b)
) :<> #[b:bool] bool (b) // true/false : removed/not

(* ****** ****** *)

(* end of [linordset_randbst.sats] *)
