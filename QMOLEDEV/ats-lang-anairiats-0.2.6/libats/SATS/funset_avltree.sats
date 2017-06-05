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

(*
**
** A functional map implementation based on AVL trees
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: May, 2011 // based on a version done in October, 2008
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

abstype
set_t0ype_type (elt: t@ype+)
stadef set = set_t0ype_type

(* ****** ****** *)

typedef cmp (elt:t@ype) = (elt, elt) -<cloref> int

fun{elt:t@ype}
compare_elt_elt (x1: elt, x2: elt, cmp: cmp elt):<> int

(* ****** ****** *)

fun{} funset_make_nil {a:t@ype} ():<> set (a)
fun{a:t@ype} funset_make_sing (x0: a):<> set (a) // singleton set
fun{a:t@ype} funset_make_list (xs: List a, cmp: cmp a):<> set (a)

(* ****** ****** *)

fun{} funset_is_nil {a:t@ype} (xs: set a):<> bool
fun{} funset_isnot_nil {a:t@ype} (xs: set a):<> bool

(* ****** ****** *)
//
// HX: the time complexity of this function is O(n), where n is
fun{a:t@ype} funset_size (xs: set a):<> size_t // the size of the set
//
// HX: the time complexity of this function is O(n); it is for
fun{a:t@ype} funset_height (xs: set a):<> size_t // gathering stats
//
(* ****** ****** *)

fun{a:t@ype}
funset_is_member (xs: set a, x0: a, cmp: cmp a):<> bool
fun{a:t@ype}
funset_isnot_member (xs: set a, x0: a, cmp: cmp a):<> bool

(* ****** ****** *)

fun{a:t@ype}
funset_insert (
  xs: &set (a), x0: a, cmp: cmp a
) :<> bool(*[x0] alreay exists in [xs]*) // end of [funset_insert]

(* ****** ****** *)

fun{a:t@ype}
funset_remove (
  xs: &set (a), x0: a, cmp: cmp (a)
) :<> bool(*[x0] removed/not: true/false*) // end of [funset_remove]

(* ****** ****** *)
//
// HX: choose an element in an unspecified manner
//
fun{a:t@ype}
funset_choose (
  xs: set a, x: &a? >> opt (a, b)
) : #[b:bool] bool (b) // end of [funset_choose]
//
// HX: take out an element in an unspecified manner
//
fun{a:t@ype}
funset_takeout (
  xs: &set a >> set a, x: &a? >> opt (a, b)
) : #[b:bool] bool (b) // end of [funset_takeout]
//
(* ****** ****** *)

fun{a:t@ype} funset_union
  (xs1: set a, xs2: set a, cmp: cmp a):<> set (a)
fun{a:t@ype} funset_intersect
  (xs1: set a, xs2: set a, cmp: cmp a):<> set (a)
fun{a:t@ype} funset_diff
  (xs1: set a, xs2: set a, cmp: cmp a):<> set (a)
fun{a:t@ype} funset_symdiff
  (xs1: set a, xs2: set a, cmp: cmp a):<> set (a)

(* ****** ****** *)

fun{a:t@ype}
funset_is_subset
  (xs1: set a, xs2: set a, cmp: cmp a):<> bool
// end of [funset_is_subset]

fun{a:t@ype}
funset_is_supset
  (xs1: set a, xs2: set a, cmp: cmp a):<> bool
// end of [funset_is_supset]

fun{a:t@ype}
funset_is_equal (xs1: set a, xs2: set a, cmp: cmp a):<> bool

(* ****** ****** *)

fun{a:t@ype}
funset_foreach_funenv
  {v:view} {vt:viewtype} (
  pf: !v 
| xs: set (a)
, f: (!v | a, !vt) -<fun> void
, env: !vt
) :<> void // end of [funset_foreach_funenv]

fun{a:t@ype}
funset_foreach_fun (
  xs: set (a)
, f: (a) -<fun> void
) :<> void // end of [funset_foreach_fun]

fun{a:t@ype}
funset_foreach_vclo {v:view} (
  pf: !v
| xs: set (a)
, f: &(!v | a) -<clo> void
) :<> void // end of [funset_foreach_vclo]

fun{a:t@ype}
funset_foreach_cloref (
  xs: set (a)
, f: (a) -<cloref> void
) :<!ref> void // end of [funset_foreach_cloref]

(* ****** ****** *)

fun{a:t@ype}
funset_listize (xs: set (a)):<> List_vt (a)

(* ****** ****** *)

(* end of [funset_avltree.sats] *)
