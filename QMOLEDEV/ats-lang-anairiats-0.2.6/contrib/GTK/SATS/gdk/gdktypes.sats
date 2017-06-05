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

typedef GdkColor =
  $extype_struct "GdkColor" of {
  pixel= guint32, red= guint16, green= guint16, blue= guint16
} // end of [GdkColor]

(* ****** ****** *)

typedef GdkPoint =
  $extype_struct "GdkPoint" of { x= gint, y= gint }
// end of [GdkPoint]

(* ****** ****** *)

typedef GdkRectangle =
  $extype_struct "GdkRectangle" of {
  x= gint
, y= gint
, width= gint
, height= gint
} // end of [GdkRectangle]

(* ****** ****** *)

//
// HX-2010-05-16: it is defined in gdkregion-generic.h
//
typedef GdkRegion =
  $extype_struct "GdkRegion" of {
  size= lint // a long integer
, numRects= lint // a long integer
// , GdkRegionBox *rects
// , extents= GdkRegionBox
} // end of [GdkRegion]

(* ****** ****** *)

typedef GdkSegment =
  $extype_struct "GdkSegment" of {
  x1= gint
, y1= gint
, x2= gint
, y2= gint
} // end of [GdkSegment]

(* ****** ****** *)

typedef GdkSpan =
  $extype_struct "GdkSpan" of {
  x= gint, y= gint, width= gint(*number of pixels in a span*)
} // end of [GdkSpan]

(* ****** ****** *)

abst@ype GdkWChar = $extype"GdKWChar"

(* ****** ****** *)

abstype GdkAtom // typedef struct _GdkAtom *GdkAtom

castfn GDK_ATOM_TO_POINTER (x: GdkAtom):<> gpointer
castfn GDK_POINTER_TO_ATOM (x: gpointer):<> GdkAtom

macdef GDK_NONE = $extval (GdkAtom, "GDK_NONE")

(* ****** ****** *)

abst@ype GdkEventType = $extype"GdkEventType"
abst@ype GdkEventMask = $extype"GdkEventMask"

(* ****** ****** *)

abst@ype GdkModifierType = $extype"GdkModifierType"

macdef GDK_SHIFT_MASK = $extval (GdkModifierType, "GDK_SHIFT_MASK")
macdef GDK_LOCK_MASK = $extval (GdkModifierType, "GDK_LOCK_MASK")
macdef GDK_CONTROL_MASK = $extval (GdkModifierType, "GDK_CONTROL_MASK")
macdef GDK_MOD1_MASK = $extval (GdkModifierType, "GDK_MOD1_MASK")
macdef GDK_MOD2_MASK = $extval (GdkModifierType, "GDK_MOD2_MASK")
macdef GDK_MOD3_MASK = $extval (GdkModifierType, "GDK_MOD3_MASK")
macdef GDK_MOD4_MASK = $extval (GdkModifierType, "GDK_MOD4_MASK")
macdef GDK_MOD5_MASK = $extval (GdkModifierType, "GDK_MOD5_MASK")
macdef GDK_BUTTON1_MASK = $extval (GdkModifierType, "GDK_BUTTON1_MASK")
macdef GDK_BUTTON2_MASK = $extval (GdkModifierType, "GDK_BUTTON2_MASK")
macdef GDK_BUTTON3_MASK = $extval (GdkModifierType, "GDK_BUTTON3_MASK")
macdef GDK_BUTTON4_MASK = $extval (GdkModifierType, "GDK_BUTTON4_MASK")
macdef GDK_BUTTON5_MASK = $extval (GdkModifierType, "GDK_BUTTON5_MASK")
macdef GDK_SUPER_MASK = $extval (GdkModifierType, "GDK_SUPER_MASK")
macdef GDK_HYPER_MASK = $extval (GdkModifierType, "GDK_HYPER_MASK")
macdef GDK_META_MASK = $extval (GdkModifierType, "GDK_META_MASK")
macdef GDK_RELEASE_MASK = $extval (GdkModifierType, "GDK_RELEASE_MASK")
macdef GDK_MODIFIER_MASK = $extval (GdkModifierType, "GDK_MODIFIER_MASK")

(* ****** ****** *)

abst@ype
GdkVisibilityState = $extype"GdkVisibilityState"
abst@ype GdkWindowState = $extype"GdkWindowState"

(* ****** ****** *)

(* end of [gdktypes.sats] *)
