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
** A set implementation based on AVL trees
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

fun{} linset_make_nil {a:t@ype} ():<> set (a)
fun{a:t0p} linset_make_sing (x0: a):<> set (a) // singleton set

(* ****** ****** *)

fun{} linset_is_empty {a:t0p} (xs: !set a):<> bool
fun{} linset_isnot_empty {a:t0p} (xs: !set a):<> bool

(* ****** ****** *)
//
// HX: the time complexity of this function is O(n), where n is
fun{a:t@ype} linset_size (xs: !set a):<> size_t // the size of the set
//
(* ****** ****** *)

fun{a:t0p}
linset_is_member (xs: !set a, x0: a, cmp: cmp a):<> bool
fun{a:t0p}
linset_isnot_member (xs: !set a, x0: a, cmp: cmp a):<> bool

(* ****** ****** *)

fun{a:t@ype}
linset_free (xs: set (a)):<> void

(* ****** ****** *)

fun{a:t@ype}
linset_insert (
  xs: &set (a), x0: a, cmp: cmp a
) :<> bool(*[x0] alreay exists in [xs]*) // end of [linset_insert]

(* ****** ****** *)

fun{a:t@ype}
linset_remove (
  xs: &set (a), x0: a, cmp: cmp (a)
) :<> bool(*[x0] removed/not: true/false*) // end of [linset_remove]

(* ****** ****** *)
//
// HX: choose an element in an unspecified manner
//
fun{a:t@ype}
linset_choose (
  xs: !set a, x: &a? >> opt (a, b)
) : #[b:bool] bool (b) // end of [linset_choose]
//
// HX: take out an element in an unspecified manner
//
fun{a:t@ype}
linset_takeout (
  xs: &set a >> set a, x: &a? >> opt (a, b)
) : #[b:bool] bool (b) // end of [linset_takeout]
//
(* ****** ****** *)

fun{a:t@ype} linset_union
  (xs1: set a, xs2: set a, cmp: cmp a):<> set (a)
fun{a:t@ype} linset_intersect
  (xs1: set a, xs2: set a, cmp: cmp a):<> set (a)
fun{a:t@ype} linset_diff
  (xs1: set a, xs2: set a, cmp: cmp a):<> set (a)
fun{a:t@ype} linset_symdiff
  (xs1: set a, xs2: set a, cmp: cmp a):<> set (a)

(* ****** ****** *)

fun{a:t@ype}
linset_is_subset // xs2 contains xs1
  (xs1: !set a, xs2: !set a, cmp: cmp a):<> bool
// end of [linset_is_subset]

fun{a:t@ype}
linset_is_supset // xs1 contains xs2
  (xs1: !set a, xs2: !set a, cmp: cmp a):<> bool
// end of [linset_is_supset]

fun{a:t@ype}
linset_is_equal (xs1: !set a, xs2: !set a, cmp: cmp a):<> bool

(* ****** ****** *)

fun{a:t@ype}
linset_foreach_funenv
  {v:view} {vt:viewtype} (
  pf: !v 
| xs: !set (a)
, f: (!v | a, !vt) -<fun> void
, env: !vt
) :<> void // end of [linset_foreach_funenv]

fun{a:t@ype}
linset_foreach_fun (
  xs: !set (a)
, f: (a) -<fun> void
) :<> void // end of [linset_foreach_fun]

fun{a:t@ype}
linset_foreach_vclo {v:view} (
  pf: !v
| xs: !set (a)
, f: &(!v | a) -<clo> void
) :<> void // end of [linset_foreach_vclo]

fun{a:t@ype}
linset_foreach_cloref (
  xs: !set (a)
, f: (a) -<cloref> void
) :<!ref> void // end of [linset_foreach_cloref]

(* ****** ****** *)

fun{a:t@ype}
linset_listize (xs: !set (a)):<> List_vt (a)

fun{a:t@ype}
linset_listize_free (xs: set (a)):<> List_vt (a)

(* ****** ****** *)

(* end of [linset_avltree.sats] *)
