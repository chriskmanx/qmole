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

macdef GDK_SELECTION_PRIMARY = $extval (GdkAtom, "GDK_SELECTION_PRIMARY")
macdef GDK_SELECTION_SECONDARY = $extval (GdkAtom, "GDK_SELECTION_SECONDARY")
macdef GDK_SELECTION_CLIPBOARD = $extval (GdkAtom, "GDK_SELECTION_CLIPBOARD")
macdef GDK_TARGET_BITMAP = $extval (GdkAtom, "GDK_TARGET_BITMAP")
macdef GDK_TARGET_COLORMAP = $extval (GdkAtom, "GDK_TARGET_COLORMAP")
macdef GDK_TARGET_DRAWABLE = $extval (GdkAtom, "GDK_TARGET_DRAWABLE")
macdef GDK_TARGET_PIXMAP = $extval (GdkAtom, "GDK_TARGET_PIXMAP")
macdef GDK_TARGET_STRING = $extval (GdkAtom, "GDK_TARGET_STRING")
macdef GDK_SELECTION_TYPE_ATOM = $extval (GdkAtom, "GDK_SELECTION_TYPE_ATOM")
macdef GDK_SELECTION_TYPE_BITMAP = $extval (GdkAtom, "GDK_SELECTION_TYPE_BITMAP")
macdef GDK_SELECTION_TYPE_COLORMAP = $extval (GdkAtom, "GDK_SELECTION_TYPE_COLORMAP")
macdef GDK_SELECTION_TYPE_DRAWABLE = $extval (GdkAtom, "GDK_SELECTION_TYPE_DRAWABLE")
macdef GDK_SELECTION_TYPE_INTEGER = $extval (GdkAtom, "GDK_SELECTION_TYPE_INTEGER")
macdef GDK_SELECTION_TYPE_PIXMAP = $extval (GdkAtom, "GDK_SELECTION_TYPE_PIXMAP")
macdef GDK_SELECTION_TYPE_WINDOW = $extval (GdkAtom, "GDK_SELECTION_TYPE_WINDOW")
macdef GDK_SELECTION_TYPE_STRING = $extval (GdkAtom, "GDK_SELECTION_TYPE_STRING")

(* ****** ****** *)

(* end of [gdkselection.sats] *)
