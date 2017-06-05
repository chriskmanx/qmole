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
// Start Time: April, 2010
//
(* ****** ****** *)

fun gtk_color_selection_new
  (): GtkColorSelection_ref1 = "mac#atsctrb_gtk_color_selection_new"
// end of [gtk_color_selection_new]

(* ****** ****** *)

fun gtk_color_selection_set_has_palette
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), has_palette: gboolean): void
  = "mac#atsctrb_gtk_color_selection_set_has_palette"
// end of [gtk_color_selection_set_has_palette]

(* ****** ****** *)

fun gtk_color_selection_set_has_opacitiy_control
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), has_opacity: gboolean): void
  = "mac#atsctrb_gtk_color_selection_set_has_opacitiy_control"
// end of [gtk_color_selection_set_has_opacitiy_control]

(* ****** ****** *)

fun gtk_color_selection_get_previous_color
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), color: &GdkColor? >> GdkColor): void
  = "mac#atsctrb_gtk_color_selection_get_previous_color"
// end of [gtk_color_selection_get_previous_color]

fun gtk_color_selection_set_previous_color
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), color: &GdkColor): void
  = "mac#atsctrb_gtk_color_selection_set_previous_color"
// end of [gtk_color_selection_set_previous_color]

(* ****** ****** *)

fun gtk_color_selection_get_current_color
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), color: &GdkColor? >> GdkColor): void
  = "mac#atsctrb_gtk_color_selection_get_current_color"
// end of [gtk_color_selection_get_current_color]

fun gtk_color_selection_set_current_color
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), color: &GdkColor): void
  = "mac#atsctrb_gtk_color_selection_set_current_color"
// end of [gtk_color_selection_set_current_color]

(* ****** ****** *)

fun gtk_color_selection_get_previous_alpha
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), alpha: &guint16? >> guint16): void
  = "mac#atsctrb_gtk_color_selection_get_previous_alpha"
// end of [gtk_color_selection_get_previous_alpha]

fun gtk_color_selection_set_previous_alpha
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), alpha: guint16): void
  = "mac#atsctrb_gtk_color_selection_set_previous_alpha"
// end of [gtk_color_selection_set_previous_alpha]

(* ****** ****** *)

fun gtk_color_selection_get_current_alpha
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), alpha: &guint16? >> guint16): void
  = "mac#atsctrb_gtk_color_selection_get_current_alpha"
// end of [gtk_color_selection_get_current_alpha]

fun gtk_color_selection_set_current_alpha
  {c:cls | c <= GtkColorSelection} {l:agz}
  (dialog: !gobjref (c, l), alpha: guint16): void
  = "mac#atsctrb_gtk_color_selection_set_current_alpha"
// end of [gtk_color_selection_set_current_alpha]

(* ****** ****** *)

(* end of [gtkcolorsel.sats] *)
