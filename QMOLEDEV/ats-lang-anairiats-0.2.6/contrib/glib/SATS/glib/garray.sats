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

(* Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

%{#
#include "contrib/glib/CATS/glib/garray.cats"
%}

(* ****** ****** *)

//
// HX-2010-02-27: only need for individual testing
// staload "contrib/glib/SATS/glib/gtypes.sats"
//

(* ****** ****** *)

//
// HX-2010-02-17: GArray_ref is reference-counted
//
absviewtype GArray_ref (a:viewt@ype+, l:addr) // = GArray*
viewtypedef GArray_ref1 (a:viewt@ype) = [l:agz] GArray_ref (a, l)

(* ****** ****** *)

fun g_array_new {a:viewt@ype}
  (zrotrm: gboolean, clear: gboolean, sz: sizeof_t a): GArray_ref1 a
  = "mac#atsctrb_g_array_new"
// end of [g_array_new]

fun g_array_sized_new {a:viewt@ype} (
    zrotrm: gboolean, clear: gboolean, sz: sizeof_t a, reserved: guint
  ) : GArray_ref1 a = "mac#atsctrb_g_array_sized_new"
// end of [g_array_sized_new]

(* ****** ****** *)

fun g_array_ref {a:viewt@ype} {l:agz}
  (array: !GArray_ref (a, l)): GArray_ref (a, l) = "mac#atsctrb_g_array_ref"
// end of [g_array_ref]

fun g_array_unref {a:viewt@ype}
  (array: GArray_ref1 a): void = "mac#atsctrb_g_array_unref"
// end of [g_array_unref]

(* ****** ****** *)

(*
//
// HX-2010-02-27:
// Given the availability of g_array_unref, I have to be convinced that
// this is really needed
//
*)
fun g_array_free_true
  {a:t@ype} (
  array: GArray_ref1 a
) : void
  = "atsctrb_g_array_free_true" // function!
// end of [g_array_free_true]

(* ****** ****** *)

fun g_array_get_element_size
  {a:viewt@ype}
  {l:agz} (
  array: !GArray_ref (a, l)
) : sizeof_t a
  = "mac#atslib_g_array_get_element_size"
// end of [g_array_get_element_size]

(* ****** ****** *)
//
// HX: unsafe functions
//
fun g_array_takeout_tsz
  {a:viewt@ype} {l:agz} {i:nat} (
  array: !GArray_ref (a, l), i: gint i, tsz: sizeof_t a
) : [l_elt:addr] (
  a @ l_elt, minus (GArray_ref (a, l), a @ l_elt) | ptr l_elt
) = "atsctrb_g_array_takeout_tsz"
// end of [g_array_takeout_tsz]

fun{a:t@ype} g_array_get_elt_at
  {l:agz} {i:nat} (array: !GArray_ref (a, l), i: gint i): a
// end of [g_array_get_elt_at]

fun{a:t@ype} g_array_set_elt_at
  {l:agz} {i:nat} (array: !GArray_ref (a, l), i: gint i, x: a): void
// end of [g_array_set_elt_at]

(* ****** ****** *)

fun g_array_append_val
  {a:viewt@ype} {l:agz} (
  array: !GArray_ref (a, l), v: &a >> a?!
) : void = "mac#atsctrb_g_array_append_val"
// end of [g_array_append_val]

fun g_array_prepend_val
  {a:viewt@ype} {l:agz} (
  array: !GArray_ref (a, l), v: &a >> a?!
) : void = "mac#atsctrb_g_array_prepend_val"
// end of [g_array_prepend_val]

(* ****** ****** *)

fun g_array_set_size
  {a:viewt@ype}
  {l:agz} (
  array: !GArray_ref (a, l), size: guint
) : void
  = "mac#atsctrb_g_array_set_size"
// end of [g_array_set_size]

(* ****** ****** *)

fun g_array_sort
  {a:viewt@ype}
  {l:agz} (
  A: !GArray_ref (a, l), cmp: GCompareFuncRef a
) : void
  = "mac#atsctrb_g_array_sort"
// end of [g_array_sort]

(* ****** ****** *)

(* end of [garray.sats] *)
