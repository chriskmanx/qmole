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

abst@ype GtkAnchorType = $extype"GtkAnchorType"
macdef GTK_ANCHOR_CENTER = $extval (GtkAnchorType, "GTK_ANCHOR_CENTER")
macdef GTK_ANCHOR_NORTH = $extval (GtkAnchorType, "GTK_ANCHOR_NORTH")
macdef GTK_ANCHOR_NORTH_WEST = $extval (GtkAnchorType, "GTK_ANCHOR_NORTH_WEST")
macdef GTK_ANCHOR_NORTH_EAST = $extval (GtkAnchorType, "GTK_ANCHOR_NORTH_EAST")
macdef GTK_ANCHOR_SOUTH = $extval (GtkAnchorType, "GTK_ANCHOR_SOUTH")
macdef GTK_ANCHOR_SOUTH_WEST = $extval (GtkAnchorType, "GTK_ANCHOR_SOUTH_WEST")
macdef GTK_ANCHOR_SOUTH_EAST = $extval (GtkAnchorType, "GTK_ANCHOR_SOUTH_EAST")
macdef GTK_ANCHOR_WEST = $extval (GtkAnchorType, "GTK_ANCHOR_WEST")
macdef GTK_ANCHOR_EAST = $extval (GtkAnchorType, "GTK_ANCHOR_EAST")
macdef GTK_ANCHOR_N = $extval (GtkAnchorType, "GTK_ANCHOR_N")
macdef GTK_ANCHOR_NW = $extval (GtkAnchorType, "GTK_ANCHOR_NW")
macdef GTK_ANCHOR_NE = $extval (GtkAnchorType, "GTK_ANCHOR_NE")
macdef GTK_ANCHOR_S = $extval (GtkAnchorType, "GTK_ANCHOR_S")
macdef GTK_ANCHOR_SW = $extval (GtkAnchorType, "GTK_ANCHOR_SW")
macdef GTK_ANCHOR_SE = $extval (GtkAnchorType, "GTK_ANCHOR_SE")
macdef GTK_ANCHOR_W = $extval (GtkAnchorType, "GTK_ANCHOR_W")
macdef GTK_ANCHOR_E = $extval (GtkAnchorType, "GTK_ANCHOR_E")

(* ****** ****** *)

abst@ype GtkArrowPlacement = $extype"GtkArrowPlacement"
macdef GTK_ARROWS_BOTH = $extval (GtkArrowPlacement, "GTK_ARROWS_BOTH")
macdef GTK_ARROWS_START = $extval (GtkArrowPlacement, "GTK_ARROWS_START")
macdef GTK_ARROWS_END = $extval (GtkArrowPlacement, "GTK_ARROWS_END")

(* ****** ****** *)

abst@ype
GtkArrowType = $extype"GtkArrowType"
macdef GTK_ARROW_UP = $extval (GtkArrowType, "GTK_ARROW_UP")
macdef GTK_ARROW_DOWN = $extval (GtkArrowType, "GTK_ARROW_DOWN")
macdef GTK_ARROW_LEFT = $extval (GtkArrowType, "GTK_ARROW_LEFT")
macdef GTK_ARROW_RIGHT = $extval (GtkArrowType, "GTK_ARROW_RIGHT")
macdef GTK_ARROW_NONE = $extval (GtkArrowType, "GTK_ARROW_NONE")

(* ****** ****** *)

abst@ype GtkAttachOptions = $extype"GtkAttachOptions"
macdef GTK_EXPAND = $extval (GtkAttachOptions, "GTK_EXPAND")
macdef GTK_SHRINK = $extval (GtkAttachOptions, "GTK_SHRINK")
macdef GTK_FILL = $extval (GtkAttachOptions, "GTK_FILL")

(* ****** ****** *)

abst@ype
GtkButtonBoxStyle = $extype"GtkButtonBoxStyle"
macdef GTK_BUTTONBOX_DEFAULT_STYLE =
  $extval (GtkButtonBoxStyle, "GTK_BUTTONBOX_DEFAULT_STYLE")
macdef GTK_BUTTONBOX_SPREAD = $extval (GtkButtonBoxStyle, "GTK_BUTTONBOX_SPREAD")
macdef GTK_BUTTONBOX_EDGE = $extval (GtkButtonBoxStyle, "GTK_BUTTONBOX_EDGE")
macdef GTK_BUTTONBOX_START = $extval (GtkButtonBoxStyle, "GTK_BUTTONBOX_START")
macdef GTK_BUTTONBOX_END = $extval (GtkButtonBoxStyle, "GTK_BUTTONBOX_END")
macdef GTK_BUTTONBOX_CENTER = $extval (GtkButtonBoxStyle, "GTK_BUTTONBOX_CENTER")

(* ****** ****** *)

abst@ype
GtkJustification = $extype"GtkJustification"
macdef GTK_JUSTIFY_LEFT = $extval (GtkJustification, "GTK_JUSTIFY_LEFT")
macdef GTK_JUSTIFY_RIGHT = $extval (GtkJustification, "GTK_JUSTIFY_RIGHT")
macdef GTK_JUSTIFY_CENTER = $extval (GtkJustification, "GTK_JUSTIFY_CENTER")
macdef GTK_JUSTIFY_FILL = $extval (GtkJustification, "GTK_JUSTIFY_FILL")

(* ****** ****** *)

(*
** Placement type for scrolled window
*)
abst@ype GtkCornerType = $extype"GtkCornerType"
macdef GTK_CORNER_TOP_LEFT = $extval (GtkCornerType, "GTK_CORNER_TOP_LEFT")
macdef GTK_CORNER_BOTTOM_LEFT = $extval (GtkCornerType, "GTK_CORNER_BOTTOM_LEFT")
macdef GTK_CORNER_TOP_RIGHT = $extval (GtkCornerType, "GTK_CORNER_TOP_RIGHT")
macdef GTK_CORNER_BOTTOM_RIGHT = $extval (GtkCornerType, "GTK_CORNER_BOTTOM_RIGHT")

(* ****** ****** *)

abst@ype GtkPathType = $extype"GtkPathType"
macdef GTK_PATH_WIDGET = $extval (GtkPathType, "GTK_PATH_WIDGET")
macdef GTK_PATH_WIDGET_CLASS = $extval (GtkPathType, "GTK_PATH_WIDGET_CLASS")
macdef GTK_PATH_CLASS = $extval (GtkPathType, "GTK_PATH_CLASS")

(* ****** ****** *)

(*
** Policy type for scrolled window
*)
abst@ype GtkPolicyType = $extype"GtkPolicyType"
macdef GTK_POLICY_ALWAYS = $extval (GtkPolicyType, "GTK_POLICY_ALWAYS")
macdef GTK_POLICY_AUTOMATIC = $extval (GtkPolicyType, "GTK_POLICY_AUTOMATIC")
macdef GTK_POLICY_NEVER = $extval (GtkPolicyType, "GTK_POLICY_NEVER")

(* ****** ****** *)

abst@ype
GtkPositionType = $extype"GtkPositionType"
macdef GTK_POS_LEFT = $extval (GtkPositionType, "GTK_POS_LEFT")
macdef GTK_POS_RIGHT = $extval (GtkPositionType, "GTK_POS_RIGHT")
macdef GTK_POS_TOP = $extval (GtkPositionType, "GTK_POS_TOP")
macdef GTK_POS_BOTTOM = $extval (GtkPositionType, "GTK_POS_BOTTOM")

(* ****** ****** *)

abst@ype GtkShadowType = $extype"GtkShadowType"
macdef GTK_SHADOW_IN = $extval (GtkShadowType, "GTK_SHADOW_IN")
macdef GTK_SHADOW_OUT = $extval (GtkShadowType, "GTK_SHADOW_OUT") // default
macdef GTK_SHADOW_ETCHED_IN = $extval (GtkShadowType, "GTK_SHADOW_ETCHED_IN")
macdef GTK_SHADOW_ETCHED_OUT = $extval (GtkShadowType, "GTK_SHADOW_ETCHED_OUT")
macdef GTK_SHADOW_NONE = $extval (GtkShadowType, "GTK_SHADOW_NONE")

(* ****** ****** *)

abst@ype GtkStateType = $extype"GtkStateType"
macdef GTK_STATE_NORMAL = $extval (GtkStateType, "GTK_STATE_NORMAL")
macdef GTK_STATE_ACTIVE = $extval (GtkStateType, "GTK_STATE_ACTIVE")
macdef GTK_STATE_PRELIGHT = $extval (GtkStateType, "GTK_STATE_PRELIGHT")
macdef GTK_STATE_SELECTED = $extval (GtkStateType, "GTK_STATE_SELECTED")
macdef GTK_STATE_INSENSITIVE = $extval (GtkStateType, "GTK_STATE_INSENSITIVE")

(* ****** ****** *)

abst@ype GtkMetricType = $extype"GtkMetricType"
macdef GTK_PIXELS = $extval (GtkMetricType, "GTK_PIXELS")
macdef GTK_INCHES = $extval (GtkMetricType, "GTK_INCHES")
macdef GTK_CENTIMETERS = $extval (GtkMetricType, "GTK_CENTIMETERS")

(* ****** ****** *)

abst@ype GtkToolbarStyle = $extype"GtkToolbarStyle"
macdef GTK_TOOLBAR_ICONS = $extval (GtkToolbarStyle, "GTK_TOOLBAR_ICONS")
macdef GTK_TOOLBAR_TEXT = $extval (GtkToolbarStyle, "GTK_TOOLBAR_TEXT")
macdef GTK_TOOLBAR_BOTH = $extval (GtkToolbarStyle, "GTK_TOOLBAR_BOTH")
macdef GTK_TOOLBAR_BOTH_HORIZ = $extval (GtkToolbarStyle, "GTK_TOOLBAR_BOTH_HORIZ")

(* ****** ****** *)

abst@ype
GtkUpdateType = $extype"GtkUpdateType"
macdef GTK_UPDATE_CONTINUOUS =
  $extval (GtkUpdateType, "GTK_UPDATE_CONTINUOUS")
macdef GTK_UPDATE_DISCONTINUOUS =
  $extval (GtkUpdateType, "GTK_UPDATE_DISCONTINUOUS")
macdef GTK_UPDATE_DELAYED = $extval (GtkUpdateType, "GTK_UPDATE_DELAYED")

(* ****** ****** *)

abst@ype
GtkWindowPosition = $extype"GtkWindowPosition"
macdef GTK_WIN_POS_NONE =
  $extval (GtkWindowPosition, "GTK_WIN_POS_NONE")
macdef GTK_WIN_POS_CENTER =
  $extval (GtkWindowPosition, "GTK_WIN_POS_CENTER")
macdef GTK_WIN_POS_MOUSE =
  $extval (GtkWindowPosition, "GTK_WIN_POS_MOUSE")
macdef GTK_WIN_POS_CENTER_ALWAYS =
  $extval (GtkWindowPosition, "GTK_WIN_POS_CENTER_ALWAYS")
macdef GTK_WIN_POS_CENTER_ON_PARENT =
  $extval (GtkWindowPosition, "GTK_WIN_POS_CENTER_ON_PARENT")

(* ****** ****** *)

abst@ype
GtkWindowType = $extype"GtkWindowType"
macdef GTK_WINDOW_TOPLEVEL =
  $extval (GtkWindowType, "GTK_WINDOW_TOPLEVEL")
macdef GTK_WINDOW_POPUP =
  $extval (GtkWindowType, "GTK_WINDOW_POPUP")

(* ****** ****** *)

abst@ype
GtkWrapMode = $extype"GtkWrapMode"
macdef GTK_WRAP_NONE = $extval (GtkWrapMode, "GTK_WRAP_NONE")
macdef GTK_WRAP_CHAR = $extval (GtkWrapMode, "GTK_WRAP_CHAR")
macdef GTK_WRAP_WORD = $extval (GtkWrapMode, "GTK_WRAP_WORD")
macdef GTK_WRAP_WORD_CHAR = $extval (GtkWrapMode, "GTK_WRAP_WORD_CHAR")

(* ****** ****** *)

(* end of [gtkenums.sats] *)

