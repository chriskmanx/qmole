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

macdef GDK_NOTHING = $extval (GdkEventType, "GDK_NOTHING")
macdef GDK_DELETE = $extval (GdkEventType, "GDK_DELETE")
macdef GDK_DESTROY = $extval (GdkEventType, "GDK_DESTROY")
macdef GDK_EXPOSE = $extval (GdkEventType, "GDK_EXPOSE")
macdef GDK_MOTION_NOTIFY = $extval (GdkEventType, "GDK_MOTION_NOTIFY")
macdef GDK_BUTTON_PRESS = $extval (GdkEventType, "GDK_BUTTON_PRESS")
macdef GDK_2BUTTON_PRESS = $extval (GdkEventType, "GDK_2BUTTON_PRESS")
macdef GDK_3BUTTON_PRESS = $extval (GdkEventType, "GDK_3BUTTON_PRESS")
macdef GDK_BUTTON_RELEASE = $extval (GdkEventType, "GDK_BUTTON_RELEASE")
macdef GDK_KEY_PRESS = $extval (GdkEventType, "GDK_KEY_PRESS")
macdef GDK_KEY_RELEASE = $extval (GdkEventType, "GDK_KEY_RELEASE")
macdef GDK_ENTER_NOTIFY = $extval (GdkEventType, "GDK_ENTER_NOTIFY")
macdef GDK_LEAVE_NOTIFY = $extval (GdkEventType, "GDK_LEAVE_NOTIFY")
macdef GDK_FOCUS_CHANGE = $extval (GdkEventType, "GDK_FOCUS_CHANGE")
macdef GDK_CONFIGURE = $extval (GdkEventType, "GDK_CONFIGURE")
macdef GDK_MAP = $extval (GdkEventType, "GDK_MAP")
macdef GDK_UNMAP = $extval (GdkEventType, "GDK_UNMAP")
macdef GDK_PROPERTY_NOTIFY = $extval (GdkEventType, "GDK_PROPERTY_NOTIFY")
macdef GDK_SELECTION_CLEAR = $extval (GdkEventType, "GDK_SELECTION_CLEAR")
macdef GDK_SELECTION_REQUEST = $extval (GdkEventType, "GDK_SELECTION_REQUEST")
macdef GDK_SELECTION_NOTIFY = $extval (GdkEventType, "GDK_SELECTION_NOTIFY")
macdef GDK_PROXIMITY_IN = $extval (GdkEventType, "GDK_PROXIMITY_IN")
macdef GDK_PROXIMITY_OUT = $extval (GdkEventType, "GDK_PROXIMITY_OUT")
macdef GDK_DRAG_ENTER = $extval (GdkEventType, "GDK_DRAG_ENTER")
macdef GDK_DRAG_LEAVE = $extval (GdkEventType, "GDK_DRAG_LEAVE")
macdef GDK_DRAG_MOTION = $extval (GdkEventType, "GDK_DRAG_MOTION")
macdef GDK_DRAG_STATUS = $extval (GdkEventType, "GDK_DRAG_STATUS")
macdef GDK_DROP_START = $extval (GdkEventType, "GDK_DROP_START")
macdef GDK_DROP_FINISHED = $extval (GdkEventType, "GDK_DROP_FINISHED")
macdef GDK_CLIENT_EVENT = $extval (GdkEventType, "GDK_CLIENT_EVENT")
macdef GDK_VISIBILITY_NOTIFY = $extval (GdkEventType, "GDK_VISIBILITY_NOTIFY")
macdef GDK_NO_EXPOSE = $extval (GdkEventType, "GDK_NO_EXPOSE")
macdef GDK_SCROLL = $extval (GdkEventType, "GDK_SCROLL")
macdef GDK_WINDOW_STATE = $extval (GdkEventType, "GDK_WINDOW_STATE")
macdef GDK_SETTING = $extval (GdkEventType, "GDK_SETTING")
macdef GDK_OWNER_CHANGE = $extval (GdkEventType, "GDK_OWNER_CHANGE")
macdef GDK_GRAB_BROKEN = $extval (GdkEventType, "GDK_GRAB_BROKEN")
macdef GDK_DAMAGE = $extval (GdkEventType, "GDK_DAMAGE")

fun eq_GdkEventType_GdkEventType (
  x1: GdkEventType, x2: GdkEventType
) : bool
  = "mac#atsctrb_eq_GdkEventType_GdkEventType"
overload = with eq_GdkEventType_GdkEventType

(* ****** ****** *)

macdef GDK_EXPOSURE_MASK =
  $extval (GdkEventMask, "GDK_EXPOSURE_MASK")
macdef GDK_POINTER_MOTION_MASK =
  $extval (GdkEventMask, "GDK_POINTER_MOTION_MASK")
macdef GDK_POINTER_MOTION_HINT_MASK =
  $extval (GdkEventMask, "GDK_POINTER_MOTION_HINT_MASK")
macdef GDK_BUTTON_MOTION_MASK =
  $extval (GdkEventMask, "GDK_BUTTON_MOTION_MASK")
macdef GDK_BUTTON1_MOTION_MASK =
  $extval (GdkEventMask, "GDK_BUTTON1_MOTION_MASK")
macdef GDK_BUTTON2_MOTION_MASK =
  $extval (GdkEventMask, "GDK_BUTTON2_MOTION_MASK")
macdef GDK_BUTTON3_MOTION_MASK =
  $extval (GdkEventMask, "GDK_BUTTON3_MOTION_MASK")
macdef GDK_BUTTON_PRESS_MASK =
  $extval (GdkEventMask, "GDK_BUTTON_PRESS_MASK")
macdef GDK_BUTTON_RELEASE_MASK =
  $extval (GdkEventMask, "GDK_BUTTON_RELEASE_MASK")
macdef GDK_KEY_PRESS_MASK =
  $extval (GdkEventMask, "GDK_KEY_PRESS_MASK")
macdef GDK_KEY_RELEASE_MASK =
  $extval (GdkEventMask, "GDK_KEY_RELEASE_MASK")
macdef GDK_ENTER_NOTIFY_MASK =
  $extval (GdkEventMask, "GDK_ENTER_NOTIFY_MASK")
macdef GDK_LEAVE_NOTIFY_MASK =
  $extval (GdkEventMask, "GDK_LEAVE_NOTIFY_MASK")
macdef GDK_FOCUS_CHANGE_MASK =
  $extval (GdkEventMask, "GDK_FOCUS_CHANGE_MASK")
macdef GDK_STRUCTURE_MASK =
  $extval (GdkEventMask, "GDK_STRUCTURE_MASK")
macdef GDK_PROPERTY_CHANGE_MASK =
  $extval (GdkEventMask, "GDK_PROPERTY_CHANGE_MASK")
macdef GDK_VISIBILITY_NOTIFY_MASK =
  $extval (GdkEventMask, "GDK_VISIBILITY_NOTIFY_MASK")
macdef GDK_PROXIMITY_IN_MASK =
  $extval (GdkEventMask, "GDK_PROXIMITY_IN_MASK")
macdef GDK_PROXIMITY_OUT_MASK =
  $extval (GdkEventMask, "GDK_PROXIMITY_OUT_MASK")
macdef GDK_SUBSTRUCTURE_MASK =
  $extval (GdkEventMask, "GDK_SUBSTRUCTURE_MASK")
macdef GDK_SCROLL_MASK =
  $extval (GdkEventMask, "GDK_SCROLL_MASK")
macdef GDK_ALL_EVENTS_MASK =
  $extval (GdkEventMask, "GDK_ALL_EVENTS_MASK")

castfn gint_of_GdkEventMask (x: GdkEventMask):<> gint

fun lor_GdkEventMask_GdkEventMask
  (x1: GdkEventMask, x2: GdkEventMask): GdkEventMask
  = "atsctrb_lor_GdkEventMask_GdkEventMask"
overload lor with lor_GdkEventMask_GdkEventMask

(* ****** ****** *)

macdef GDK_VISIBILITY_UNOBSCURED =
  $extval (GdkVisibilityState, "GDK_VISIBILITY_UNOBSCURED")
macdef GDK_VISIBILITY_PARTIAL
  = $extval (GdkVisibilityState, "GDK_VISIBILITY_PARTIAL")
macdef GDK_VISIBILITY_FULLY_OBSCURED =
  $extval (GdkVisibilityState, "GDK_VISIBILITY_FULLY_OBSCURED")

fun eq_GdkVisibilityState_GdkVisibilityState
  (x1: GdkVisibilityState, x2: GdkVisibilityState): bool
  = "atsctrb_eq_GdkVisibilityState_GdkVisibilityState"
overload = with eq_GdkVisibilityState_GdkVisibilityState

(* ****** ****** *)

macdef GDK_WINDOW_STATE_WITHDRAWN =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_WITHDRAWN")
macdef GDK_WINDOW_STATE_ICONIFIED =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_ICONIFIED")
macdef GDK_WINDOW_STATE_MAXIMIZED =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_MAXIMIZED")
macdef GDK_WINDOW_STATE_STICKY =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_STICKY")
macdef GDK_WINDOW_STATE_FULLSCREEN =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_FULLSCREEN")
macdef GDK_WINDOW_STATE_ABOVE =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_ABOVE")
macdef GDK_WINDOW_STATE_BELOW =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_BELOW")

(* ****** ****** *)

typedef GdkEvent = $extype_struct "GdkEvent" of {
  type= GdkEventType
, window= ptr // GdkWindow *window;
, send_event= gint8
, _rest= undefined_t // this field cannot be accessed
} // end of [GdkEvent]

propdef GdkEvent_castdn_t (a:t@ype) = {l:addr}
  (GdkEvent @ l) -<prf> (a @ l, a @ l -<lin,prf> GdkEvent @ l)
// end of [GdkEvent_castdn_t]

(* ****** ****** *)

typedef GdkEventAny =
  $extype_struct "GdkEventAny" of {
  type= GdkEventType
, window= ptr // GdkWindow *window;
, send_event= gint8
} // end of [GdkEventAny]
praxi GdkEventAny_castdn : GdkEvent_castdn_t (GdkEventAny)

(* ****** ****** *)

typedef GdkEventButton =
  $extype_struct "GdkEventButton" of {
  type= GdkEventType
, window= ptr // GdkWindow *window;
, send_event= gint8
, time= guint32
, x= gdouble, y= gdouble
//  gdouble *axes;
, state= guint
, button= guint
, device= ptr // GdkDevice *device;
, x_root= gdouble, y_root= gdouble
, _res= undefined_t // this field cannot be accessed
} // end of [GdkEventButton]
praxi GdkEventButton_castdn : GdkEvent_castdn_t (GdkEventButton)

(* ****** ****** *)

typedef GdkEventExpose =
  $extype_struct "GdkEventExpose" of {
  type= GdkEventType
, window= ptr // GdkWindow *window;
, send_event= gint8
, area= GdkRectangle
, region= ptr // GdkRegion *region;
, count = gint // the number of events that follow
} // end of [GdkEventExpose]
praxi GdkEventExpose_castdn : GdkEvent_castdn_t (GdkEventExpose)

(* ****** ****** *)

typedef GdkEventNoExpose =
  $extype_struct "GdkNoEventExpose" of {
  type= GdkEventType
, window= ptr // GdkWindow *window;
, send_event= gint8
} // end of [GdkEventNoExpose]

(* ****** ****** *)

typedef GdkEventVisibility =
  $extype_struct "GdkEventVisibility" of {
  type= GdkEventType
, window= ptr // GdkWindow *window;
, send_event= gint8
, state= GdkVisibilityState
} // end of [GdkEventVisibility]

(* ****** ****** *)

typedef GdkEventMotion =
  $extype_struct "GdkEventMotion" of {
  type= GdkEventType
, window= ptr // GdkWindow *window;
, send_event= gint8
, time= guint32
, x= gdouble
, y= gdouble
, axes= ptr // gdouble *axes;
, state= guint
, is_hint= gint16
, device= ptr // GdkDevice *device;
, x_root= gdouble
, y_root= gdouble
} // end of [GdkEventMotion]

(* ****** ****** *)

typedef GdkEventKey =
  $extype_struct "GdkEventKey" of {
  type= GdkEventType
, window= ptr // GdkWindow *window;
, send_event= gint8
, time= guint32
, state= guint
, keyval= guint
, length= gint
, string= ptr // gchar*
, hardware_keycode= guint16
, group= guint8
(*
, is_modifier= guint : 1
*)
} // end of [GdkEventKey]

(* ****** ****** *)

(* end of [gdkevents.sats] *)
