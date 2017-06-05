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

fun gtk_font_selection_new ()
  : GtkFontSelection_ref1 = "atsctrb_gtk_font_selection_new" // function!
// end of [gtk_font_selection_new]

(* ****** ****** *)
//
// HX-2010-05-25:
// this is a 'get0' functionl; the return value cannot be NULL
//
fun gtk_font_selection_get_preview_text
  {c:cls | c <= GtkFontSelection} {l:agz}
  (fsd: !gobjref (c, l))
  : [l1:agz] (
  minus (gobjref (c, l), gstring l1) | gstring l1
) = "mac#atsctrb_gtk_font_selection_get_preview_text"

fun gtk_font_selection_set_preview_text
  {c:cls | c <= GtkFontSelection}
  {l,l1:agz} (
  fsd: !gobjref (c, l), text: !gstring l1
) : void
  = "mac#atsctrb_gtk_font_selection_set_preview_text"
// end of [fun]

(* ****** ****** *)

//
// HX-2010-05-25: [title] can be NULL
//
fun gtk_font_selection_dialog_new
  {l:addr} (title: !gstring l): GtkFontSelectionDialog_ref1
  = "atsctrb_gtk_font_selection_dialog_new" // function!
// end of [gtk_font_selection_dialog_new]

(* ****** ****** *)

// HX: this is a 'get0' function
fun gtk_font_selection_dialog_get_ok_button
  {c:cls | c <= GtkFontSelectionDialog} {l:agz}
  (fsd: !gobjref (c, l)): [l1:agz] (
  minus (gobjref (c, l), GtkButton_ref l1)
|  GtkButton_ref l1
) = "mac#atsctrb_gtk_font_selection_dialog_get_ok_button"
// end of [gtk_font_selection_dialog_get_ok_button]

// HX: this is a 'get0' function
fun gtk_font_selection_dialog_get_cancel_button
  {c:cls | c <= GtkFontSelectionDialog} {l:agz}
  (fsd: !gobjref (c, l)): [l1:agz] (
  minus (gobjref (c, l), GtkButton_ref l1)
|  GtkButton_ref l1
) = "mac#atsctrb_gtk_font_selection_dialog_get_cancel_button"
// end of [gtk_font_selection_dialog_get_cancel_button]

(* ****** ****** *)

// HX-2010-05-25: the return value can be NULL
fun gtk_font_selection_dialog_get_font_name
  {c:cls | c <= GtkFontSelectionDialog} {l:agz} (fsd: !gobjref (c, l)): gstring0
  = "mac#atsctrb_gtk_font_selection_dialog_get_font_name"
// end of [gtk_font_selection_dialog_get_font_name]

fun gtk_font_selection_dialog_set_font_name
  {c:cls | c <= GtkFontSelectionDialog}
  {l,l1:agz} (
  fsd: !gobjref (c, l), fontname: !gstring l1
) : gboolean
  = "mac#atsctrb_gtk_font_selection_dialog_set_font_name"
// end of [gtk_font_selection_dialog_set_font_name]  

(* ****** ****** *)

// HX-2010-05-25:
// this is a 'get0' functionl; the return value cannot be NULL
fun gtk_font_selection_dialog_get_preview_text
  {c:cls | c <= GtkFontSelectionDialog}
  {l:agz} (
  fsd: !gobjref (c, l)
) : [l1:agz] (
  minus (gobjref (c, l), gstring l1) | gstring l1
) = "mac#atsctrb_gtk_font_selection_dialog_get_preview_text"
// end of [gtk_font_selection_dialog_get_preview_text]

fun gtk_font_selection_dialog_set_preview_text
  {c:cls | c <= GtkFontSelectionDialog}
  {l,l1:agz} (
  fsd: !gobjref (c, l), text: !gstring l1
 ) : void
  = "mac#atsctrb_gtk_font_selection_dialog_set_preview_text"
// end of [gtk_font_selection_dialog_set_preview_text]

(* ****** ****** *)

(* end of [gtkfontsel.sats] *)
