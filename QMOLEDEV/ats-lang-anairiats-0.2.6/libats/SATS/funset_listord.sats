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
** A functional map implementation based on ordered lists
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: May 18, 2011
**
*)

(* ****** ****** *)

abstype
set_t0ype_type (elt: t@ype+)
stadef set = set_t0ype_type

(* ****** ****** *)

typedef cmp (a:t@ype) = (a, a) -<cloref> int

fun{a:t@ype}
compare_elt_elt (x1: a, x2: a, cmp: cmp (a)):<> int

(* ****** ****** *)

fun{} funset_make_nil {a:t@ype} ():<> set (a)
fun{a:t@ype} funset_make_sing (x0: a):<> set (a) // singleton set
fun{a:t@ype} funset_make_list (xs: List a, cmp: cmp a):<> set (a)

(* ****** ****** *)

fun{a:t@ype} funset_size (xs: set (a)): size_t

(* ****** ****** *)

fun{a:t@ype}
funset_is_member (xs: set a, x0: a, cmp: cmp a):<> bool
fun{a:t@ype}
funset_isnot_member (xs: set a, x0: a, cmp: cmp a):<> bool

(* ****** ****** *)

fun{a:t@ype}
funset_insert (
  xs: &set (a)
, x0: a
, cmp: cmp (a)
) :<> bool(*[x0] alreay exists in [xs]*) // end of [funset_insert]

fun{a:t@ype}
funset_remove (
  xs: &set (a)
, x0: a
, cmp: cmp (a)
) :<> bool(*removed/not: true/false*)
// end of [funset_remove]

(* ****** ****** *)

fun{a:t@ype} funset_union
  (xs1: set (a), xs2: set (a), cmp: cmp (a)):<> set (a)
fun{a:t@ype} funset_intersect
  (xs1: set (a), xs2: set (a), cmp: cmp (a)):<> set (a)
fun{a:t@ype} funset_diff
  (xs1: set (a), xs2: set (a), cmp: cmp (a)):<> set (a)
fun{a:t@ype} funset_symdiff
  (xs1: set (a), xs2: set (a), cmp: cmp (a)):<> set (a)

(* ****** ****** *)

fun{a:t@ype}
funset_is_subset
  (xs1: set (a), xs2: set (a), cmp: cmp (a)):<> bool
// end of [funset_is_subset]

fun{a:t@ype}
funset_is_equal (xs1: set (a), xs2: set (a), cmp: cmp (a)):<> bool

(* ****** ****** *)

fun funset_listize {a:t@ype} (xs: set (a)):<> List (a) // HX: identity

(* ****** ****** *)

(* end of [funset_listord.sats] *)
