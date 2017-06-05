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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2010
//
(* ****** ****** *)
//
// HX-2010-02-28:
// In any acconts, [glist] is a terrible package for ATS to incorporate.
// It is simply a _big_ mess, and I have tried my best to make some sense
// out of this mess!
//
(* ****** ****** *)

%{#
#include "contrib/glib/CATS/glib/glist.cats"
%} // end of [%{#]

(* ****** ****** *)
//
// HX-2010-02-27: only need for individual testing
// staload "contrib/glib/SATS/glib/gtypes.sats"
//
(* ****** ****** *)

sortdef vwtp = viewtype

absviewtype GList_ptr
  (a:viewtype+, frnt:int, rear:int) = ptr // = GList*
// end of [GList_ptr]

viewtypedef GList_ptr0 (a:vwtp) = [f,r:nat] GList_ptr (a, f, r)
viewtypedef GList_ptr1 (a:vwtp) = [f:nat;r:pos] GList_ptr (a, f, r)

(* ****** ****** *)

fun g_list_is_nil {a:vwtp} {l,r:nat}
  (list: !GList_ptr (a, l, r)): bool (r==0) = "atsctrb_g_list_is_nil"
// end of [g_list_is_nil]

fun g_list_isnot_nil {a:vwtp} {l,r:nat}
  (list: !GList_ptr (a, l, r)): bool (r > 0) = "atsctrb_g_list_isnot_nil"
// end of [g_list_isnot_nil]

(* ****** ****** *)

fun g_list_new_nil
  {a:vwtp} (): GList_ptr (a, 0, 0) = "mac#atsctrb_g_list_new_nil"
// end of [g_list_new_nil]

fun g_list_free_nil
  {a:vwtp} (xs: GList_ptr (a, 0, 0)):<> void = "mac#atsctrb_g_list_free_nil"
// end of [g_list_free_nil]

(* ****** ****** *)

fun g_list_free
  {a:vwtp} (list: GList_ptr0 (a?)): void = "mac#atsctrb_g_list_free"
// end of [g_list]

fun g_list_free1
  {a:vwtp} (list: GList_ptr (a?, 0, 1)): void = "mac#atsctrb_g_list_free1"
// end of [g_list_free11]

(* ****** ****** *)

fun g_list_next {a:vwtp} {f:nat;r:int | r >= 2} 
  (list: GList_ptr (a, f, r)): GList_ptr (a, f+1, r-1) = "mac#atsctrb_g_list_next"
// end of [g_list_next]

fun g_list_previous {a:vwtp} {f:pos;r:nat} 
  (list: GList_ptr (a, f, r)): GList_ptr (a, f-1, r+1) = "mac#atsctrb_g_list_previous"
// end of [g_list_previous]

fun g_list_last {a:vwtp} {f:nat;r:pos}
  (list: GList_ptr (a, f, r)): GList_ptr (a, f+r-1, 1) = "mac#atsctrb_g_list_last"
// end of [g_list_last]

fun g_list_first
  {a:vwtp} {f,r:nat} (
  list: GList_ptr (a, f, r)
) : GList_ptr (a, 0, f+r) = "mac#atsctrb_g_list_first"
// end of [g_list_first]

(* ****** ****** *)
//
// HX: append at the far end
//
fun g_list_append
  {a:vwtp} {f,r:nat} ( // its complexity is O(n)
  list: GList_ptr (a, f, r), x: !a >> a?!
) : GList_ptr (a, f, r+1) = "mac#atsctrb_g_list_append"
// end of [g_list_append]

(* ****** ****** *)
//
// HX: insert at the current position
//
fun g_list_prepend
  {a:vwtp} {f,r:nat} ( // its complexity is O(1)
  list: GList_ptr (a, f, r), x: !a >> a?!
) : GList_ptr (a, f, r+1) = "mac#atsctrb_g_list_prepend"
// end of [g_list_prepend]

(* ****** ****** *)

fun g_list_length {a:vwtp} {f,r:nat}
  (list: !GList_ptr (a, f, r)): gint r = "mac#atsctrb_g_list_length"
// end of [g_list_length]

(* ****** ****** *)

fun g_list_insert_at
  {a:vwtp} {f,r:nat}
  {i:nat | i <= r} ( // its complexity is O(i)
  list: GList_ptr (a, f, r), x: !a >> a?!, i: gint i
) : GList_ptr (a, f, r+1) = "mac#atsctrb_g_list_insert"
// end of [g_list_insert_at]

fun g_list_insert_sorted
  {a:vwtp} {f,r:nat} ( // its complexity is O(n)
  list: GList_ptr (a, f, r), x: !a >> a?!, cmp: (!a, !a) -<fun> gint
) : GList_ptr (a, f, r+1) = "mac#atsctrb_g_list_insert_sorted"
// end of [g_list_insert_sorted]

(* ****** ****** *)

fun g_list_remove_current
  {a:vwtp} {f,r:int | r > 0} (
  list: GList_ptr (a, f, r), node: &ptr? >> GList_ptr (a, 0, 1)
) : GList_ptr (a, f, r-1) = "atsctrb_g_list_remove_current"

(* ****** ****** *)

fun g_list_concat
  {a:vwtp} {l1,r1:nat} {r2:nat} (
  list1: GList_ptr (a, l1, r1), list2: GList_ptr (a, 0, r2)
) : GList_ptr (a, l1, r1+r2) = "mac#atsctrb_g_list_concat"
// end of [g_list_concat]

(* ****** ****** *)

fun g_list_copy {a:type} {f,r:nat}
  (list: !GList_ptr (a, f, r)): GList_ptr (a, 0, r) = "mac#atsctrb_g_list_copy"
// end of [g_list_copy]

(* ****** ****** *)

fun g_list_reverse {a:vwtp} {r:nat}
  (list: GList_ptr (a, 0, r)): GList_ptr (a, 0, r) = "mac#atsctrb_g_list_reverse"
// end of [g_list_reverse]

(* ****** ****** *)

//
// HX-2010-02-28:
// these sorting functions are based on mergesort implementation
//
fun g_list_sort
  {a:vwtp} {r:nat} (
  list: GList_ptr (a, 0, r), cmp: (!a, !a) -<fun> gint
) : GList_ptr (a, 0, r)
  = "mac#atsctrb_g_list_sort"
// end of [g_list_sort]

fun g_list_sort_with_data
  {a:vwtp} {vt:viewtype} {r:nat} (
  list: GList_ptr (a, 0, r), cmp: (!a, !a, !vt) -<fun> gint, env: !vt
) : GList_ptr (a, 0, r) = "mac#atsctrb_g_list_sort_with_data"
// end of [g_list_sort_with_data]

(* ****** ****** *)

fun g_list_foreach
  {a:vwtp} {vt:viewtype} {f,r:nat} (
  list: !GList_ptr (a, f, r), f: (!a, !vt) -<fun> void, env: !vt
) : void = "mac#atsctrb_g_list_foreach" // end of [g_list_foreach]

(* ****** ****** *)

fun g_list_nth
  {a:vwtp} {f,r:nat} {i:nat | i < r} (
  list: GList_ptr (a, f, r), i: int (i)
) : GList_ptr (a, f+i, r-i) = "mac#atsctrb_g_list_nth"
// end of [g_list_nth]

fun g_list_nth_data
  {a:type} {f,r:nat} (
  list: !GList_ptr (a, f, r), i: natLt r
) : a
  = "mac#atsctrb_g_list_nth_data"
// end of [g_list_nth_data]

fun g_list_nth_prev
  {a:vwtp} {f,r:nat} {i:nat | i <= f}
  (list: GList_ptr (a, f, r), i: int i): GList_ptr (a, f-i, r+i)
  = "mac#atsctrb_g_list_nth"
// end of [g_list_nth_prev]

(* ****** ****** *)

fun g_list_index
  {a:type} {f,r:nat} (
  list: !GList_ptr (a, f, r), x: !a
) : [i:int | ~1 <= i; i < r] gint i = "mac#atsctrb_g_list_index"
// end of [g_list_index]
  
(* ****** ****** *)

(* end of [glist.sats] *)
