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
// Time: June, 2010
//
(* ****** ****** *)

abst@ype GtkProgressBarStyle = $extype"GtkProgressBarStyle"
macdef GTK_PROGRESS_CONTINUOUS =
  $extval (GtkProgressBarStyle, "GTK_PROGRESS_CONTINUOUS")
macdef GTK_PROGRESS_DISCRETE = $extval (GtkProgressBarStyle, "GTK_PROGRESS_DISCRETE")

abst@ype GtkProgressBarOrientation = $extype"GtkProgressBarOrientation"
macdef GTK_PROGRESS_LEFT_TO_RIGHT =
  $extval (GtkProgressBarOrientation, "GTK_PROGRESS_LEFT_TO_RIGHT")
macdef GTK_PROGRESS_RIGHT_TO_LEFT =
  $extval (GtkProgressBarOrientation, "GTK_PROGRESS_RIGHT_TO_LEFT")
macdef GTK_PROGRESS_BOTTOM_TO_TOP =
  $extval (GtkProgressBarOrientation, "GTK_PROGRESS_BOTTOM_TO_TOP")
macdef GTK_PROGRESS_TOP_TO_BOTTOM =
  $extval (GtkProgressBarOrientation, "GTK_PROGRESS_TOP_TO_BOTTOM")

(* ****** ****** *)

fun gtk_progress_bar_new
  (): GtkProgressBar_ref1 = "mac#atsctrb_gtk_progress_bar_new"
// end of [gtk_progress_bar_new]

//
// HX-2010-06-03: this one is deprecated!
// HX-2010-06-03: checked: [adj] can be NULL
//
fun gtk_progress_bar_new_with_adjustment
  {c:cls | c <= GtkAdjustment}
  {l:addr} (
  adj: !gobjref (c, l)
) : GtkProgressBar_ref1
  = "mac#atsctrb_gtk_progress_bar_new_with_adjustment"
// end of [gtk_progress_bar_new_with_adjustment]

(* ****** ****** *)

fun gtk_progress_bar_pulse
  {c:cls | c <= GtkProgressBar}
  {l:agz} (
  pbar: !gobjref (c, l)
) : void
  = "mac#atsctrb_gtk_progress_bar_pulse"
// end of [gtk_progress_bar_pulse]

(* ****** ****** *)

fun gtk_progress_bar_get_text
  {c:cls | c <= GtkProgressBar}
  {l:agz} (
  pbar: !gobjref (c, l)
) : [l1:addr] (
  minus (gobjref (c, l), gstring l1) | gstring l1
) = "mac#atsctrb_gtk_progress_bar_get_text"
// end of [gtk_progress_bar_get_text]

fun gtk_progress_bar_set_text
  {c:cls | c <= GtkProgressBar}
  {l,l1:addr | l > null} (pbar: !gobjref (c, l), text: !gstring l1): void
  = "mac#atsctrb_gtk_progress_bar_set_text"
// end of [gtk_progress_bar_set_text]

(* ****** ****** *)

fun gtk_progress_bar_get_fraction
  {c:cls | c <= GtkProgressBar} {l:agz}
  (pbar: !gobjref (c, l)): gdouble = "mac#atsctrb_gtk_progress_bar_get_fraction"
// end of [gtk_progress_bar_get_fraction]

fun gtk_progress_bar_set_fraction
  {c:cls | c <= GtkProgressBar}
  {l:agz} (
  pbar: !gobjref (c, l), frac: gdouble
) : void
  = "mac#atsctrb_gtk_progress_bar_set_fraction"
// end of [gtk_progress_bar_set_fraction]

(* ****** ****** *)

fun gtk_progress_bar_get_pulse_step
  {c:cls | c <= GtkProgressBar}
  {l:agz} (
  pbar: !gobjref (c, l)
) : gdouble
  = "mac#atsctrb_gtk_progress_bar_get_pulse_step"
// end of [gtk_progress_bar_get_pulse_step]

fun gtk_progress_bar_set_pulse_step
  {c:cls | c <= GtkProgressBar} {l:agz}
  (pbar: !gobjref (c, l), frac: gdouble): void
  = "mac#atsctrb_gtk_progress_bar_set_pulse_step"
// end of [gtk_progress_bar_set_pulse_step]

(* ****** ****** *)

fun gtk_progress_bar_get_orientation
  {c:cls | c <= GtkProgressBar} {l:agz}
  (pbar: !gobjref (c, l)): GtkProgressBarOrientation
  = "mac#atsctrb_gtk_progress_bar_get_orientation"
// end of [gtk_progress_bar_get_orientation]

fun gtk_progress_bar_set_orientation
  {c:cls | c <= GtkProgressBar} {l:agz}
  (pbar: !gobjref (c, l), orient: GtkProgressBarOrientation): void
  = "mac#atsctrb_gtk_progress_bar_set_orientation"
// end of [gtk_progress_bar_set_orientation]

(* ****** ****** *)

fun gtk_progress_bar_get_ellipsize
  {c:cls | c <= GtkProgressBar} {l:agz}
  (pbar: !gobjref (c, l)): PangoEllipsizeMode
  = "mac#atsctrb_gtk_progress_bar_get_ellipsize"
// end of [gtk_progress_bar_get_ellipsize]

fun gtk_progress_bar_set_ellipsize
  {c:cls | c <= GtkProgressBar} {l:agz}
  (pbar: !gobjref (c, l), mode: PangoEllipsizeMode): void
  = "mac#atsctrb_gtk_progress_bar_set_ellipsize"
// end of [gtk_progress_bar_set_ellipsize]

(* ****** ****** *)

(* end of [gtkprogressbar.sats] *)
