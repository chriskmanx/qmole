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
// Start Time: May, 2010
//
(* ****** ****** *)

//
// HX-2010-05-03: [hadj] and [vadj] can be NULL
// if NULL is used, an adjustment is to be created by the [new] function
//
fun gtk_scrolled_window_new
  {c1,c2:cls | c1 <= GtkAdjustment; c2 <= GtkAdjustment}
  {l1,l2:addr} (
  hadj: !gobjref (c1, l1)
, vadj: !gobjref (c2, l2)
) : GtkScrolledWindow_ref1
  = "atsctrb_gtk_scrolled_window_new"
// end of [gtk_scrolled_window_new]

fun gtk_scrolled_window_new_null
  (): GtkScrolledWindow_ref1 = "atsctrb_gtk_scrolled_window_new_null"
// end of [gtk_scrolled_window_new_null]

(* ****** ****** *)

fun gtk_scrolled_window_get_policy
  {c:cls | c <= GtkScrolledWindow} {l:agz} (
  win: !gobjref (c, l)
, hp: &GtkPolicyType? >> GtkPolicyType
, vp: &GtkPolicyType? >> GtkPolicyType
) : void
  = "mac#atsctrb_gtk_scrolled_window_get_policy"
// end of [gtk_scrolled_window_get_policy]

fun gtk_scrolled_window_set_policy
  {c:cls | c <= GtkScrolledWindow}
  {l:agz} (
  win: !gobjref (c, l)
, hp: GtkPolicyType
, vp: GtkPolicyType
) : void
  = "mac#atsctrb_gtk_scrolled_window_set_policy"
// end of [gtk_scrolled_window_set_policy]

(* ****** ****** *)

fun gtk_scrolled_window_get_placement
  {c:cls | c <= GtkScrolledWindow}
  {l:agz} (
  win: !gobjref (c, l)
) : GtkCornerType
  = "mac#atsctrb_gtk_scrolled_window_get_placement"
// end of [gtk_scrolled_window_get_placement]

fun gtk_scrolled_window_set_placement
  {c:cls | c <= GtkScrolledWindow}
  {l:agz} (
  win: !gobjref (c, l), placement: GtkCornerType
) : void
  = "mac#atsctrb_gtk_scrolled_window_set_placement"
// end of [gtk_scrolled_window_set_placement]

(* ****** ****** *)

(* end of [gtkscrolledwindow.sats] *)
