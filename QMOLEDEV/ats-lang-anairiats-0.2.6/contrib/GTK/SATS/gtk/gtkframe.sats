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

fun gtk_frame_new {l:addr}
  (name: !gstring l): GtkFrame_ref1 = "mac#atsctrb_gtk_frame_new"
// end of [gtk_frame_new]

//
// HX-2010-04-15: this is just gtk_frame_new (NULL)
//
fun gtk_frame_new_null
  (): GtkFrame_ref1 = "mac#atsctrb_gtk_frame_new_null"
// end of [gtk_frame_new_null]

(* ****** ****** *)

//
// HX-2010-05-08: checked: gstring is shared; it is not copied out
//
fun gtk_frame_get_label
  {c:cls | c <= GtkFrame}
  {l1:agz} (
  frame: !gobjref (c, l1)
) : [l2:addr] (
  minus (gobjref (c, l1), gstring l2) | gstring l2
) = "mac#atsctrb_gtk_frame_get_label"
// end of [gtk_frame_get_label]

fun gtk_frame_set_label
  {c:cls | c <= GtkFrame}
  {l1,l2:agz} (
  frame: !gobjref (c, l1), name: !gstring l2
) : void
  = "mac#atsctrb_gtk_frame_set_label"
// end of [gtk_frame_set_label]

(* ****** ****** *)

fun gtk_frame_get_label_align
  {c:cls | c <= GtkFrame} {l:agz} (
    frame: !gobjref (c, l), x: &gfloat? >> gfloat, y: &gfloat >> gfloat
  ) : void = "mac#atsctrb_gtk_frame_get_label_align"
// end of [gtk_frame_get_label_align]

fun gtk_frame_set_label_align
  {c:cls | c <= GtkFrame} {l1,l2:agz}
  (frame: !gobjref (c, l1), name: !gstring l2, x: gfloat, y: gfloat): void
  = "mac#atsctrb_gtk_frame_set_label_align"
// end of [gtk_frame_set_label_align]

(* ****** ****** *)

//
// HX-2010-05-08: checked: reference count is unchanged
//
fun gtk_frame_get_label_widget
  {c:cls | c <= GtkFrame}
  {l1:agz} (
  frame: !gobjref (c, l1)
) : [l2:addr] (
  minus (gobjref (c, l1), gobjref (GtkLabel, l2))
| gobjref (GtkLabel, l2)
) = "mac#atsctrb_gtk_frame_get_label_widget"
// end of [gtk_frame_get_label_widget]

fun gtk_frame_set_label_widget
  {c1,c2:cls | c1 <= GtkFrame; c2 <= GtkWidget}
  {l1,l2:agz} (
  frame: !gobjref (c1, l1), widget: !gobjref (c2, l2)
) : void
  = "mac#atsctrb_gtk_frame_set_label_widget"
// end of [gtk_frame_set_label_widget]

(* ****** ****** *)

fun gtk_frame_get_shadow_type
  {c:cls | c <= GtkFrame}
  {l:agz} (
  frame: !gobjref (c, l)
) : GtkShadowType
  = "mac#atsctrb_gtk_frame_set_shadow_type"
// end of [gtk_frame_get_shadow_type]

fun gtk_frame_set_shadow_type
  {c:cls | c <= GtkFrame}
  {l:agz} (
  frame: !gobjref (c, l), _type: GtkShadowType
) : void
  = "mac#atsctrb_gtk_frame_set_shadow_type"
// end of [gtk_frame_set_shadow_type]

(* ****** ****** *)

(* end of [gtkframe.sats] *)
