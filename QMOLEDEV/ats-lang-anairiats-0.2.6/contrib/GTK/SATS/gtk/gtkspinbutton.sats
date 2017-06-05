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
// Time: April, 2010
//
(* ****** ****** *)

abst@ype
GtkSpinButtonUpdatePolicy = $extype"GtkSpinButtonUpdatePolicy"
macdef GTK_UPDATE_ALWAYS =
  $extval (GtkSpinButtonUpdatePolicy, "GTK_UPDATE_ALWAYS")
macdef GTK_UPDATE_IF_VALID =
  $extval (GtkSpinButtonUpdatePolicy, "GTK_UPDATE_IF_VALID")

(* ****** ****** *)

abst@ype GtkSpinType = $extype"GtkSpinType"
macdef GTK_SPIN_STEP_FORWARD = $extval (GtkSpinType, "GTK_SPIN_STEP_FORWARD")
macdef GTK_SPIN_STEP_BACKWARD = $extval (GtkSpinType, "GTK_SPIN_STEP_BACKWARD")
macdef GTK_SPIN_PAGE_FORWARD = $extval (GtkSpinType, "GTK_SPIN_PAGE_FORWARD")
macdef GTK_SPIN_PAGE_BACKWARD = $extval (GtkSpinType, "GTK_SPIN_PAGE_BACKWARD")
macdef GTK_SPIN_HOME = $extval (GtkSpinType, "GTK_SPIN_HOME")
macdef GTK_SPIN_END = $extval (GtkSpinType, "GTK_SPIN_END")
macdef GTK_SPIN_USER_DEFINED = $extval (GtkSpinType, "GTK_SPIN_USER_DEFINED")

(* ****** ****** *)

fun gtk_spin_button_new
  {c:cls | c <= GtkAdjustment}
  {l:agz} (
  adj: !gobjref (c, l)
, rate: gdouble, digits: guint
) : GtkSpinButton_ref1
  = "mac#atsctrb_gtk_spin_button_new"
// end of [gtk_spin_button_new]

(* ****** ****** *)

fun gtk_spin_button_configure
  {c1,c2:cls | c1 <= GtkSpinButton; c2 <= GtkAdjustment}
  {l1,l2:agz} (
  button: !gobjref (c1, l1)
, adj: !gobjref (c2, l2)
, rate: gdouble, digits: guint
) : void
  = "mac#atsctrb_gtk_spin_button_configure"
// end of [gtk_spin_button_configure]

(* ****** ****** *)

fun gtk_spin_button_get_range
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
, min: &gdouble? >> gdouble
, max: &gdouble? >> gdouble
) : void
  = "mac#atsctrb_gtk_spin_button_get_range"
// end of [gtk_spin_button_get_range]

fun gtk_spin_button_set_range
  {c:cls | c <= GtkSpinButton} {l:agz}
  (button: !gobjref (c, l), min: gdouble, max: gdouble): void
  = "mac#atsctrb_gtk_spin_button_set_range"
// end of [gtk_spin_button_set_range]

(* ****** ****** *)

fun gtk_spin_button_get_value
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
) : gdouble
  = "mac#atsctrb_gtk_spin_button_get_value"
// end of [gtk_spin_button_get_value]

fun gtk_spin_button_get_value_as_int
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
) : gint
  = "mac#atsctrb_gtk_spin_button_get_value_as_int"
// end of [gtk_spin_button_get_value_as_int]

fun gtk_spin_button_set_value
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l), value: gdouble
) : void
  = "mac#atsctrb_gtk_spin_button_set_value"
// end of [gtk_spin_button_set_value]

(* ****** ****** *)

fun gtk_spin_button_get_digits
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
) : guint
  = "mac#atsctrb_gtk_spin_button_get_digits"
// end of [gtk_spin_button_get_digits]

fun gtk_spin_button_set_digits
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l), digits: guint
) : void
  = "mac#atsctrb_gtk_spin_button_set_digits"
// end of [gtk_spin_button_set_digits]

(* ****** ****** *)

fun gtk_spin_button_get_numeric
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
) : gboolean
  = "mac#atsctrb_gtk_spin_button_get_numeric"
// end of [gtk_spin_button_get_numeric]

fun gtk_spin_button_set_numeric
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l), numeric: gboolean
) : void
  = "mac#atsctrb_gtk_spin_button_set_numeric"
// end of [gtk_spin_button_set_numeric]

(* ****** ****** *)

fun gtk_spin_button_get_wrap
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
) : gboolean
  = "mac#atsctrb_gtk_spin_button_get_wrap"
// end of [gtk_spin_button_get_wrap]

fun gtk_spin_button_set_wrap
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l), wrap: gboolean
) : void
  = "mac#atsctrb_gtk_spin_button_set_wrap"
// end of [gtk_spin_button_set_wrap]

(* ****** ****** *)

fun gtk_spin_button_get_snap_to_ticks
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
) : gboolean
  = "mac#atsctrb_gtk_spin_button_get_snap_to_ticks"
// end of [gtk_spin_button_get_snap_to_ticks]

fun gtk_spin_button_set_snap_to_ticks
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l), s2t: gboolean
) : void
  = "mac#atsctrb_gtk_spin_button_set_snap_to_ticks"
// end of [gtk_spin_button_set_snap_to_ticks]

(* ****** ****** *)

fun gtk_spin_button_get_update_policy
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
) : GtkSpinButtonUpdatePolicy
  = "mac#atsctrb_gtk_spin_button_get_update_policy"
// end of [gtk_spin_button_get_update_policy]

fun gtk_spin_button_set_update_policy
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
, policy: GtkSpinButtonUpdatePolicy
) : void
  = "mac#atsctrb_gtk_spin_button_set_update_policy"
// end of [gtk_spin_button_set_update_policy]

(* ****** ****** *)

fun gtk_spin_button_spin
  {c:cls | c <= GtkSpinButton}
  {l:agz} (
  button: !gobjref (c, l)
, dir: GtkSpinType, inc: gdouble
) : void
  = "mac#atsctrb_gtk_spin_button_spin"
// end of [gtk_spin_button_spin]

(* ****** ****** *)

fun gtk_spin_button_update
  {c:cls | c <= GtkSpinButton} {l:agz}
  (button: !gobjref (c, l)) : void = "mac#atsctrb_gtk_spin_button_update"
// end of [gtk_spin_button_update]

(* ****** ****** *)

(* end of [gtk_spin_button.sats] *)
