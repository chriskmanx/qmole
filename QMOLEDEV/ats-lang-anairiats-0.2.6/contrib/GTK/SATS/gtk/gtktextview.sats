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

abst@ype GtkTextWindowType = $extype"GtkTextWindowType"
macdef GTK_TEXT_WINDOW_PRIVATE = $extval (GtkTextWindowType, "GTK_TEXT_WINDOW_PRIVATE")
macdef GTK_TEXT_WINDOW_WIDGET = $extval (GtkTextWindowType, "GTK_TEXT_WINDOW_WIDGET")
macdef GTK_TEXT_WINDOW_TEXT = $extval (GtkTextWindowType, "GTK_TEXT_WINDOW_TEXT")
macdef GTK_TEXT_WINDOW_LEFT = $extval (GtkTextWindowType, "GTK_TEXT_WINDOW_LEFT")
macdef GTK_TEXT_WINDOW_RIGHT = $extval (GtkTextWindowType, "GTK_TEXT_WINDOW_RIGHT")
macdef GTK_TEXT_WINDOW_TOP = $extval (GtkTextWindowType, "GTK_TEXT_WINDOW_TOP")
macdef GTK_TEXT_WINDOW_BOTTOM = $extval (GtkTextWindowType, "GTK_TEXT_WINDOW_BOTTOM")

(* ****** ****** *)

fun gtk_text_view_new
  (): GtkTextView_ref1 = "mac#atsctrb_gtk_text_view_new"
// end of [gtk_text_view_new]

//
// HX-2010-05-03: if [NULL] is passed, a new one is created automatically
//
fun gtk_text_view_new_with_buffer
  {c:cls | c <= GtkTextBuffer}
  {l:addr} (
  tb: !gobjref (c, l)
) : GtkTextView_ref1
  = "mac#atsctrb_gtk_text_view_new_with_buffer"
// end of [gtk_text_view_new_with_buffer]
  
(* ****** ****** *)

fun gtk_text_view_set_buffer
  {c1,c2:cls | c1 <= GtkTextView; c2 <= GtkTextBuffer}
  {l1,l2:agz} (
  tv: !gobjref (c1, l1), tb: !gobjref (c2, l2)
) : void = "mac#atsctrb_gtk_text_view_set_buffer"
// end of [gtk_text_view_set_buffer]

//
// HX-2010-05-06: this one is 'get0'
//
fun gtk_text_view_get_buffer
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l)
) :<> [l_buf:agz] (
  minus (gobjref (c, l), gobjref (GtkTextBuffer, l_buf))
| gobjref (GtkTextBuffer, l_buf)
) = "mac#atsctrb_gtk_text_view_get_buffer"
// end of [gtk_text_view_get_buffer]

(* ****** ****** *)

fun gtk_text_view_get_wrap_mode
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjref (c, l)): GtkWrapMode
  = "mac#atsctrb_gtk_text_view_get_wrap_mode"
// end of [gtk_text_view_get_wrap_mode]

fun gtk_text_view_set_wrap_mode
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjref (c, l), mode: GtkWrapMode): void
  = "mac#atsctrb_gtk_text_view_set_wrap_mode"
// end of [gtk_text_view_set_wrap_mode]

(* ****** ****** *)

fun gtk_text_view_get_editable
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjref (c, l)): gboolean
  = "mac#atsctrb_gtk_text_view_get_editable"
// end of [gtk_text_view_get_editable]

fun gtk_text_view_set_editable
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjref (c, l), editable: gboolean): void
  = "mac#atsctrb_gtk_text_view_set_editable"
// end of [gtk_text_view_set_editable]

(* ****** ****** *)

fun gtk_text_view_get_cursor_visible
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjref (c, l)): gboolean
  = "mac#atsctrb_gtk_text_view_get_cursor_visible"
// end of [gtk_text_view_get_cursor_visible]

fun gtk_text_view_set_cursor_visible
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjref (c, l), visible: gboolean): void
  = "mac#atsctrb_gtk_text_view_set_cursor_visible"
// end of [gtk_text_view_set_cursor_visible]

(* ****** ****** *)

fun gtk_text_view_get_overwrite
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l)
) : gboolean
  = "mac#atsctrb_gtk_text_view_get_overwrite"
// end of [gtk_text_view_get_overwrite]

fun gtk_text_view_set_overwrite
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l)
, overwrite: gboolean
) : void
  = "mac#atsctrb_gtk_text_view_set_overwrite"
// end of [gtk_text_view_set_overwrite]

(* ****** ****** *)

fun gtk_text_view_get_justification
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l)
) : GtkJustification
  = "mac#atsctrb_gtk_text_view_get_justification"
// end of [gtk_text_view_get_justification]

fun gtk_text_view_set_justification
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l)
, justification: GtkJustification
) : void
  = "mac#atsctrb_gtk_text_view_set_justification"
// end of [gtk_text_view_set_justification]

(* ****** ****** *)

fun gtk_text_view_get_left_margin
  {c:cls | c <= GtkTextView} {l:agz} (
  tv: !gobjref (c, l)
) : gint
  = "mac#atsctrb_gtk_text_view_get_left_margin"
// end of [gtk_text_view_get_left_margin]

fun gtk_text_view_set_left_margin
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l), margin: gint
) : void
  = "mac#atsctrb_gtk_text_view_set_left_margin"
// end of [gtk_text_view_set_left_margin]

fun gtk_text_view_get_right_margin
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l)
) : gint
  = "mac#atsctrb_gtk_text_view_get_right_margin"
// end of [gtk_text_view_get_right_margin]

fun gtk_text_view_set_right_margin
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l), margin: gint
) : void
  = "mac#atsctrb_gtk_text_view_set_right_margin"
// end of [gtk_text_view_set_right_margin]

(* ****** ****** *)

fun gtk_text_view_get_indent
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l)
) : gint = "mac#atsctrb_gtk_text_view_get_indent"
// end of [gtk_text_view_get_indent]

fun gtk_text_view_set_indent
  {c:cls | c <= GtkTextView}
  {l:agz} (
  tv: !gobjref (c, l), indent: gint
) : void = "mac#atsctrb_gtk_text_view_set_indent"
// end of [gtk_text_view_set_indent]

(* ****** ****** *)

fun gtk_text_view_get_window
  {c:cls | c <= GtkTextView} {l:agz} (
  tv: !gobjref (c, l), type: GtkTextWindowType
) :<> [l1:addr] (
  minus (gobjref (c, l), GdkWindow_ref l1) | GdkWindow_ref l1
) = "mac#atsctrb_gtk_text_view_get_window"
// end of [gtk_text_view_get_window]

(* ****** ****** *)

fun gtk_text_view_get_border_window_size
  {c:cls | c <= GtkTextView} {l:agz} (
  tv: !gobjref (c, l), type: GtkTextWindowType
) : gint = "mac#atsctrb_gtk_text_view_get_border_window_size"
// end of [gtk_text_view_get_border_window_size]

fun gtk_text_view_set_border_window_size
  {c:cls | c <= GtkTextView} {l:agz} (
  tv: !gobjref (c, l), type: GtkTextWindowType, size: gint
) : void = "mac#atsctrb_gtk_text_view_set_border_window_size"
// end of [gtk_text_view_set_border_window_size]

(* ****** ****** *)

fun gtk_text_view_get_line_at_y
  {c:cls | c <= GtkTextView} {l:agz} (
  tv: !gobjref (c, l)
, iter: &GtkTextIter? >> GtkTextIter
, y: gint
, linetop: &gint? >> gint
) : void = "mac#atsctrb_gtk_text_view_get_line_at_y"
// end of [gtk_text_view_get_line_at_y]

fun gtk_text_view_get_line_yrange
  {c:cls | c <= GtkTextView} {l:agz} (
  tv: !gobjref (c, l)
, iter: &GtkTextIter
, y: &gint? >> gint
, height: &gint? >> gint
) : void = "mac#atsctrb_gtk_text_view_get_line_yrange"
// end of [gtk_text_view_get_line_yrange]

(* ****** ****** *)

fun gtk_text_view_buffer_to_window_coords
  {c:cls | c <= GtkTextView} {l:agz} (
  tv: !gobjref (c, l)
, _type: GtkTextWindowType
, buf_x: gint, buf_y: gint
, win_x: &gint? >> gint, win_y: &gint? >> gint
) : void = "mac#atsctrb_gtk_text_view_buffer_to_window_coords"
// end of [gtk_text_view_buffer_to_window_coords]

fun gtk_text_view_window_to_buffer_coords
  {c:cls | c <= GtkTextView} {l:agz} (
  tv: !gobjref (c, l)
, _type: GtkTextWindowType
, win_x: gint, win_y: gint
, buf_x: &gint? >> gint, buf_y: &gint? >> gint
) : void = "mac#atsctrb_gtk_text_view_window_to_buffer_coords"
// end of [gtk_text_view_window_to_buffer_coords]

(* ****** ****** *)

(* end of [gtktextview.sats] *)
