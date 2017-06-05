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
// Time: May, 2010
//
(* ****** ****** *)

staload
CAIRO = "contrib/cairo/SATS/cairo.sats"
stadef cairo_ref = $CAIRO.cairo_ref
stadef cairo_ref1 = $CAIRO.cairo_ref1

(* ****** ****** *)

fun gdk_cairo_create
  {c:cls | c <= GdkDrawable}
  {l:agz} (
  widget: !gobjref (c, l)
) : cairo_ref1
  = "mac#atsctrb_gdk_cairo_create"
// end of [gdk_cairo_create]

(* ****** ****** *)

fun gdk_cairo_set_source_color
  {l:agz} (
  cr: !cairo_ref l, color: &GdkColor
) : void
  = "mac#atsctrb_gdk_cairo_set_source_color"
// end of [gdk_cairo_set_source_color]

(* ****** ****** *)

fun gdk_cairo_set_source_pixbuf
  {c:cls | c <= GdkPixbuf}
  {l1,l2:agz} (
  cr: !cairo_ref l1
, pixbuf: !gobjref (c, l2)
, x: double, y: double
) : void
  = "mac#atsctrb_gdk_cairo_set_source_pixbuf"
// end of [gdk_cairo_set_source_pixbuf]

fun gdk_cairo_set_source_pixmap
  {c:cls | c <= GdkPixmap}
  {l1,l2:agz} (
  cr: !cairo_ref l1
, pixmap: !gobjref (c, l2)
, x: double, y: double
) : void
  = "mac#atsctrb_gdk_cairo_set_source_pixmap"
// end of [gdk_cairo_set_source_pixmap]

(* ****** ****** *)

fun gdk_cairo_rectangle
  {l:agz} (
  cr: !cairo_ref l
, rect: &GdkRectangle
) : void
  = "mac#atsctrb_gdk_cairo_rectangle"
// end of [gdk_cairo_rectangle]

(* ****** ****** *)

fun gdk_cairo_region
  {l:agz} (
  cr: !cairo_ref l
, reg: &GdkRegion
) : void
  = "mac#atsctrb_gdk_cairo_region"
// end of [gdk_cairo_region]

(* ****** ****** *)

fun gdk_cairo_reset_clip
  {c:cls | c <= GdkDrawable}
  {l1,l2:agz} (
  cr: !cairo_ref l1
, drw: !gobjref (c, l2)
) : void
  = "mac#atsctrb_gdk_cairo_reset_clip"
// end of [gdk_cairo_reset_clip]

(* ****** ****** *)

(* end of [gdkcairo.sats] *)
