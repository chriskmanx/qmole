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

abst@ype GdkWindowType = $extype"GdkWindowType"
macdef GDK_WINDOW_ROOT = $extval (GdkWindowType, "GDK_WINDOW_ROOT")
macdef GDK_WINDOW_TOPLEVEL = $extval (GdkWindowType, "GDK_WINDOW_TOPLEVEL")
macdef GDK_WINDOW_CHILD = $extval (GdkWindowType, "GDK_WINDOW_CHILD")
macdef GDK_WINDOW_DIALOG = $extval (GdkWindowType, "GDK_WINDOW_DIALOG")
macdef GDK_WINDOW_TEMP = $extval (GdkWindowType, "GDK_WINDOW_TEMP")
macdef GDK_WINDOW_FOREIGN = $extval (GdkWindowType, "GDK_WINDOW_FOREIGN")

(* ****** ****** *)

abst@ype
GdkWindowAttributesType = $extype"GdkWindowAttributesType"
macdef GDK_WA_TITLE =
  $extval (GdkWindowAttributesType, "GDK_WA_TITLE")
macdef GDK_WA_X =
  $extval (GdkWindowAttributesType, "GDK_WA_X")
macdef GDK_WA_Y =
  $extval (GdkWindowAttributesType, "GDK_WA_Y")
macdef GDK_WA_CURSOR =
  $extval (GdkWindowAttributesType, "GDK_WA_CURSOR")
macdef GDK_WA_COLORMAP =
  $extval (GdkWindowAttributesType, "GDK_WA_COLORMAP")
macdef GDK_WA_VISUAL =
  $extval (GdkWindowAttributesType, "GDK_WA_VISUAL")
macdef GDK_WA_WMCLASS =
  $extval (GdkWindowAttributesType, "GDK_WA_WMCLASS")
macdef GDK_WA_NOREDIR =
  $extval (GdkWindowAttributesType, "GDK_WA_NOREDIR")
macdef GDK_WA_TYPE_HINT =
  $extval (GdkWindowAttributesType, "GDK_WA_TYPE_HINT")

(* ****** ****** *)

abst@ype GdkWindowHints = $extype"GdkWindowHints"
macdef GDK_HINT_POS = $extval (GdkWindowHints, "GDK_HINT_POS")
macdef GDK_HINT_MIN_SIZE = $extval (GdkWindowHints, "GDK_HINT_MIN_SIZE")
macdef GDK_HINT_MAX_SIZE = $extval (GdkWindowHints, "GDK_HINT_MAX_SIZE")
macdef GDK_HINT_BASE_SIZE = $extval (GdkWindowHints, "GDK_HINT_BASE_SIZE")
macdef GDK_HINT_ASPECT = $extval (GdkWindowHints, "GDK_HINT_ASPECT")
macdef GDK_HINT_RESIZE_INC = $extval (GdkWindowHints, "GDK_HINT_RESIZE_INC")
macdef GDK_HINT_WIN_GRAVITY = $extval (GdkWindowHints, "GDK_HINT_WIN_GRAVITY")
macdef GDK_HINT_USER_POS = $extval (GdkWindowHints, "GDK_HINT_USER_POS")
macdef GDK_HINT_USER_SIZE = $extval (GdkWindowHints, "GDK_HINT_USER_SIZE")

(* ****** ****** *)

abst@ype
GdkWindowTypeHint = $extype"GdkWindowTypeHint"
macdef GDK_WINDOW_TYPE_HINT_NORMAL =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_NORMAL")
macdef GDK_WINDOW_TYPE_HINT_DIALOG =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_DIALOG")
macdef GDK_WINDOW_TYPE_HINT_MENU =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_MENU")
macdef GDK_WINDOW_TYPE_HINT_TOOLBAR =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_TOOLBAR")
macdef GDK_WINDOW_TYPE_HINT_SPLASHSCREEN =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_SPLASHSCREEN")
macdef GDK_WINDOW_TYPE_HINT_UTILITY =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_UTILITY")
macdef GDK_WINDOW_TYPE_HINT_DOCK =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_DOCK")
macdef GDK_WINDOW_TYPE_HINT_DESKTOP =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_DESKTOP")
macdef GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_DROPDOWN_MENU")
macdef GDK_WINDOW_TYPE_HINT_POPUP_MENU =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_POPUP_MENU")
macdef GDK_WINDOW_TYPE_HINT_TOOLTIP =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_TOOLTIP")
macdef GDK_WINDOW_TYPE_HINT_NOTIFICATION =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_NOTIFICATION")
macdef GDK_WINDOW_TYPE_HINT_COMBO =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_COMBO")
macdef GDK_WINDOW_TYPE_HINT_DND =
  $extval (GdkWindowTypeHint, "GDK_WINDOW_TYPE_HINT_DND")

(* ****** ****** *)

(*
** The next two enumeration values current match the
** Motif constants. If this is changed, the implementation
** of gdk_window_set_decorations/gdk_window_set_functions
** will need to change as well.
*)
abst@ype
GdkWMDecoration = $extype"GdkWMDecoration"
macdef GDK_DECOR_ALL = $extval (GdkWMDecoration, "GDK_DECOR_ALL")
macdef GDK_DECOR_BORDER = $extval (GdkWMDecoration, "GDK_DECOR_BORDER")
macdef GDK_DECOR_RESIZEH = $extval (GdkWMDecoration, "GDK_DECOR_RESIZEH")
macdef GDK_DECOR_TITLE = $extval (GdkWMDecoration, "GDK_DECOR_TITLE")
macdef GDK_DECOR_MENU = $extval (GdkWMDecoration, "GDK_DECOR_MENU")
macdef GDK_DECOR_MINIMIZE = $extval (GdkWMDecoration, "GDK_DECOR_MINIMIZE")
macdef GDK_DECOR_MAXIMIZE = $extval (GdkWMDecoration, "GDK_DECOR_MAXIMIZE")

abst@ype
GdkWMFunction = $extype"GdkWMFunction"
macdef GDK_FUNC_ALL = $extval (GdkWMFunction, "GDK_FUNC_ALL")
macdef GDK_FUNC_RESIZE = $extval (GdkWMFunction, "GDK_FUNC_RESIZE")
macdef GDK_FUNC_MOVE = $extval (GdkWMFunction, "GDK_FUNC_MOVE")
macdef GDK_FUNC_MINIMIZE = $extval (GdkWMFunction, "GDK_FUNC_MINIMIZE")
macdef GDK_FUNC_MAXIMIZE = $extval (GdkWMFunction, "GDK_FUNC_MAXIMIZE")
macdef GDK_FUNC_CLOSE = $extval (GdkWMFunction, "GDK_FUNC_CLOSE")

(* ****** ****** *)

abst@ype
GdkGravity = $extype"GdkGravity"
macdef GDK_GRAVITY_NORTH_WEST =
  $extval (GdkGravity, "GDK_GRAVITY_NORTH_WEST")
macdef GDK_GRAVITY_NORTH =
  $extval (GdkGravity, "GDK_GRAVITY_NORTH")
macdef GDK_GRAVITY_NORTH_EAST =
  $extval (GdkGravity, "GDK_GRAVITY_NORTH_EAST")
macdef GDK_GRAVITY_WEST =
  $extval (GdkGravity, "GDK_GRAVITY_WEST")
macdef GDK_GRAVITY_CENTER =
  $extval (GdkGravity, "GDK_GRAVITY_CENTER")
macdef GDK_GRAVITY_EAST =
  $extval (GdkGravity, "GDK_GRAVITY_EAST")
macdef GDK_GRAVITY_SOUTH_WEST =
  $extval (GdkGravity, "GDK_GRAVITY_SOUTH_WEST")
macdef GDK_GRAVITY_SOUTH =
  $extval (GdkGravity, "GDK_GRAVITY_SOUTH")
macdef GDK_GRAVITY_SOUTH_EAST =
  $extval (GdkGravity, "GDK_GRAVITY_SOUTH_EAST")
macdef GDK_GRAVITY_STATIC =
  $extval (GdkGravity, "GDK_GRAVITY_STATIC")

abst@ype
GdkWindowEdge = $extype"GdkWindowEdge"
macdef GDK_WINDOW_EDGE_NORTH_WEST =
  $extval (GdkWindowEdge, "GDK_WINDOW_EDGE_NORTH_WEST")
macdef GDK_WINDOW_EDGE_NORTH =
  $extval (GdkWindowEdge, "GDK_WINDOW_EDGE_NORTH")
macdef GDK_WINDOW_EDGE_NORTH_EAST =
  $extval (GdkWindowEdge, "GDK_WINDOW_EDGE_NORTH_EAST")
macdef GDK_WINDOW_EDGE_WEST =
  $extval (GdkWindowEdge, "GDK_WINDOW_EDGE_WEST")
macdef GDK_WINDOW_EDGE_EAST =
  $extval (GdkWindowEdge, "GDK_WINDOW_EDGE_EAST")
macdef GDK_WINDOW_EDGE_SOUTH_WEST =
  $extval (GdkWindowEdge, "GDK_WINDOW_EDGE_SOUTH_WEST")
macdef GDK_WINDOW_EDGE_SOUTH =
  $extval (GdkWindowEdge, "GDK_WINDOW_EDGE_SOUTH")
macdef GDK_WINDOW_EDGE_SOUTH_EAST =
  $extval (GdkWindowEdge, "GDK_WINDOW_EDGE_SOUTH_EAST")

(* ****** ****** *)

fun gdk_window_set_background
  {c:cls | c <= GdkWindow}
  {l:addr} (
  window: !gobjref (c, l), color: &GdkColor
) : void = "mac#atsctrb_gdk_window_set_background"
// end of [gdk_window_set_background]

(* ****** ****** *)

(*
void gdk_window_invalidate_rect (
  GdkWindow *window, const GdkRectangle *rect, gboolean invalidate_children
) ; // end of [gdk_window_invalidate_rect]
*)
fun gdk_window_invalidate_rect
  {c:cls | c <= GdkWindow}
  {l:addr} (
  window: !gobjref (c, l)
, rect: &GdkRectangle, invalidate_children: gboolean
) : void = "mac#atsctrb_gdk_window_invalidate_rect"
// end of [gdk_window_invalidate_rect]

(* ****** ****** *)

(*
void gdk_window_process_updates (
  GdkWindow *window, gboolean update_children
) ; // end of [gdk_window_process_updates]
*)
fun gdk_window_process_updates
  {c:cls | c <= GdkWindow}
  {l:addr} (
  window: !gobjref (c, l), update_children: gboolean
) : void = "mac#atsctrb_gdk_window_process_updates"
// end of [gdk_window_process_updates]

(*
void gdk_window_process_all_updates (void);
*)
fun gdk_window_process_all_updates
  (): void = "mac#atsctrb_gdk_window_process_all_updates"
// end of [gdk_window_process_all_updates]

(* ****** ****** *)

(* end of [gdkwindow.sats] *)
