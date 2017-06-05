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

fun gtk_text_iter_is_start (
  iter: &GtkTextIter
) : gboolean
  = "mac#atsctrb_gtk_text_iter_is_start"
// end of [gtk_text_iter_is_start]

fun gtk_text_iter_is_end (
  iter: &GtkTextIter
) : gboolean
  = "mac#atsctrb_gtk_text_iter_is_end"
// end of [gtk_text_iter_is_end]

(* ****** ****** *)

fun gtk_text_iter_get_line (
  iter: &GtkTextIter
) : gint
  = "mac#atsctrb_gtk_text_iter_get_line"
// end of [gtk_text_iter_get_line]

fun gtk_text_iter_get_line_offset (
  iter: &GtkTextIter
) : gint
  = "mac#atsctrb_gtk_text_iter_get_line_offset"
// end of [gtk_text_iter_get_line_offset]

(* ****** ****** *)

fun gtk_text_iter_forward_char (
  iter: &GtkTextIter
) : void
  = "mac#atsctrb_gtk_text_iter_forward_char"
// end of [gtk_text_iter_forward_char]

fun gtk_text_iter_backward_char (
  iter: &GtkTextIter
) : void
  = "mac#atsctrb_gtk_text_iter_backward_char"
// end of [gtk_text_iter_backward_char]

fun gtk_text_iter_forward_chars
  (iter: &GtkTextIter, cnt: gint): void
  = "mac#atsctrb_gtk_text_iter_forward_chars"
// end of [gtk_text_iter_forward_chars]

fun gtk_text_iter_backward_chars
  (iter: &GtkTextIter, cnt: gint): void
  = "mac#atsctrb_gtk_text_iter_backward_chars"
// end of [gtk_text_iter_backward_chars]

(* ****** ****** *)

fun gtk_text_iter_forward_line (
  iter: &GtkTextIter
) : void
  = "mac#atsctrb_gtk_text_iter_forward_line"
// end of [gtk_text_iter_forward_line]

fun gtk_text_iter_backward_line (
  iter: &GtkTextIter
) : void
  = "mac#atsctrb_gtk_text_iter_backward_line"
// end of [gtk_text_iter_backward_line]

fun gtk_text_iter_forward_lines (
  iter: &GtkTextIter, cnt: gint
) : void
  = "mac#atsctrb_gtk_text_iter_forward_lines"
// end of [gtk_text_iter_forward_lines]

fun gtk_text_iter_backward_lines
  (iter: &GtkTextIter, cnt: gint): void
  = "mac#yatsctrb_gtk_text_iter_backward_lines"
// end of [gtk_text_iter_backward_lines]

(* ****** ****** *)

(* end of [gtktextiter.sats] *)
