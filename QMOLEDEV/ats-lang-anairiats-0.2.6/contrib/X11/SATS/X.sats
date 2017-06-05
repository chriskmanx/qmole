(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
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

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: January, 2010

(* ****** ****** *)

%{#
#include "contrib/X11/CATS/X.cats"
%} // end of [{%#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

//
// HX-2010-02-17: This is ugly:
// Flags in X should have been declared of the type ulint!
//
fun lor_lint_lint
  (x: lint, y: lint):<> lint = "mac#atspre_lor_ulint_ulint"
// end of [lor_lint_lint]
overload lor with lor_lint_lint

(* ****** ****** *)

// Bool is already used in ATS
typedef XBool = bool // unindexed
//
abst@ype Atom = $extype"Atom" // unsigned long int
abst@ype Mask = $extype"Mask" // unsigned long int
abst@ype VisualID = $extype"VisualID" // unsigned long int
abst@ype Time = $extype"Time" // unsigned long int

(* ****** ****** *)

// [XID] is unsigned long int  

abst@ype XID = $extype"XID"
abst@ype Window = $extype"Window" // = XID
abst@ype Font = $extype"Font" // = XID
abst@ype Pixmap = $extype"Pixmap" // = XID
abst@ype Cursor = $extype"Cursor" // = XID
abst@ype Colormap = $extype"Colormap" // = XID
abst@ype GContext = $extype"GContext" // = XID
abst@ype KeySym = $extype"KeySym" // = XID

//

abst@ype Drawable = $extype"Drawable" // = XID

symintr Drawable

castfn Drawable_of_Window (x: Window): Drawable
overload Drawable with Drawable_of_Window

castfn Drawable_of_Pixmap (x: Pixmap): Drawable
overload Drawable with Drawable_of_Pixmap

(* ****** ****** *)

// EVENT DEFINITIONS 

(*
** Input Event Masks.
** Used as event-mask window attribute and as arguments to Grab requests.
*)

abst@ype InputEventMask_t = lint
fun lor_InputEventMask_InputEventMask
  (x: InputEventMask_t, y: InputEventMask_t): InputEventMask_t
  = "atsctrb_lor_InputEventMask_InputEventMask"
// end of [lor_InputEventMask_InputEventMask]
overload lor with lor_InputEventMask_InputEventMask

macdef NoEventMask = $extval (InputEventMask_t, "NoEventMask")
macdef KeyPressMask = $extval (InputEventMask_t, "KeyPressMask")
macdef KeyReleaseMask = $extval (InputEventMask_t, "KeyReleaseMask")
macdef ButtonPressMask = $extval (InputEventMask_t, "ButtonPressMask")
macdef ButtonReleaseMask = $extval (InputEventMask_t, "ButtonReleaseMask")
macdef EnterWindowMask = $extval (InputEventMask_t, "EnterWindowMask")
macdef LeaveWindowMask = $extval (InputEventMask_t, "LeaveWindowMask")
macdef PointerMotionMask = $extval (InputEventMask_t, "PointerMotionMask")
macdef PointerMotionHintMask = $extval (InputEventMask_t, "PointerMotionHintMask")
macdef Button1MotionMask = $extval (InputEventMask_t, "Button1MotionMask")
macdef Button2MotionMask = $extval (InputEventMask_t, "Button2MotionMask")
macdef Button3MotionMask = $extval (InputEventMask_t, "Button3MotionMask")
macdef Button4MotionMask = $extval (InputEventMask_t, "Button4MotionMask")
macdef Button5MotionMask = $extval (InputEventMask_t, "Button5MotionMask")
macdef ButtonMotionMask = $extval (InputEventMask_t, "ButtonMotionMask")
macdef KeymapStateMask = $extval (InputEventMask_t, "KeymapStateMask")
macdef ExposureMask = $extval (InputEventMask_t, "ExposureMask")
macdef VisibilityChangeMask = $extval (InputEventMask_t, "VisibilityChangeMask")
macdef StructureNotifyMask = $extval (InputEventMask_t, "StructureNotifyMask")
macdef ResizeRedirectMask = $extval (InputEventMask_t, "ResizeRedirectMask")
macdef SubstructureNotifyMask = $extval (InputEventMask_t, "SubstructureNotifyMask")
macdef SubstructureRedirectMask = $extval (InputEventMask_t, "SubstructureRedirectMask")
macdef FocusChangeMask = $extval (InputEventMask_t, "FocusChangeMask")
macdef PropertyChangeMask = $extval (InputEventMask_t, "PropertyChangeMask")
macdef ColormapChangeMask = $extval (InputEventMask_t, "ColormapChangeMask")
macdef OwnerGrabButtonMask = $extval (InputEventMask_t, "OwnerGrabButtonMask")

(* ****** ****** *)

(*
** Event names.
** Used in "type" field in XEvent structures
*)

abst@ype EventType_t = int
fun eq_EventType_EventType (x: EventType_t, y: EventType_t): bool
  = "atsctrb_eq_EventType_EventType"
overload = with eq_EventType_EventType

macdef KeyPress = $extval (EventType_t, "KeyPress")
macdef KeyRelease = $extval (EventType_t, "KeyRelease")
macdef ButtonPress = $extval (EventType_t, "ButtonPress")
macdef ButtonRelease = $extval (EventType_t, "ButtonRelease")
macdef MotionNotify = $extval (EventType_t, "MotionNotify")
macdef EnterNotify = $extval (EventType_t, "EnterNotify")
macdef LeaveNotify = $extval (EventType_t, "LeaveNotify")
macdef FocusIn = $extval (EventType_t, "FocusIn")
macdef FocusOut = $extval (EventType_t, "FocusOut")
macdef KeymapNotify = $extval (EventType_t, "KeymapNotify")
macdef Expose = $extval (EventType_t, "Expose")
macdef GraphicsExpose = $extval (EventType_t, "GraphicsExpose")
macdef NoExpose = $extval (EventType_t, "NoExpose")
macdef VisibilityNotify = $extval (EventType_t, "VisibilityNotify")
macdef CreateNotify = $extval (EventType_t, "CreateNotify")
macdef DestroyNotify = $extval (EventType_t, "DestroyNotify")
macdef UnmapNotify = $extval (EventType_t, "UnmapNotify")
macdef MapNotify = $extval (EventType_t, "MapNotify")
macdef MapRequest = $extval (EventType_t, "MapRequest")
macdef ReparentNotify = $extval (EventType_t, "ReparentNotify")
macdef ConfigureNotify = $extval (EventType_t, "ConfigureNotify")
macdef ConfigureRequest = $extval (EventType_t, "ConfigureRequest")
macdef GravityNotify = $extval (EventType_t, "GravityNotify")
macdef ResizeRequest = $extval (EventType_t, "ResizeRequest")
macdef CirculateNotify = $extval (EventType_t, "CirculateNotify")
macdef CirculateRequest = $extval (EventType_t, "CirculateRequest")
macdef PropertyNotify = $extval (EventType_t, "PropertyNotify")
macdef SelectionClear = $extval (EventType_t, "SelectionClear")
macdef SelectionRequest = $extval (EventType_t, "SelectionRequest")
macdef SelectionNotify = $extval (EventType_t, "SelectionNotify")
macdef ColormapNotify = $extval (EventType_t, "ColormapNotify")
macdef ClientMessage = $extval (EventType_t, "ClientMessage")
macdef MappingNotify = $extval (EventType_t, "MappingNotify")
macdef LASTEvent = $extval (EventType_t, "LASTEvent")

(* ****** ****** *)

(*
** GRAPHICS DEFINITIONS
*)

(* graphics functions, as in GC.alu *)

macdef GXclear = $extval (int, "GXclear")
macdef GXand = $extval (int, "GXand")
macdef GXandReverse = $extval (int, "GXandReverse")
macdef GXcopy = $extval (int, "GXcopy")
macdef GXandInverted = $extval (int, "GXandInverted")
macdef GXnoop = $extval (int, "GXnoop")
macdef GXxor = $extval (int, "GXxor")
macdef GXor = $extval (int, "GXor")
macdef GXnor = $extval (int, "GXnor")
macdef GXequiv = $extval (int, "GXequiv")
macdef GXinvert = $extval (int, "GXinvert")
macdef GXorReverse = $extval (int, "GXorReverse")
macdef GXcopyInverted = $extval (int, "GXcopyInverted")
macdef GXorInverted = $extval (int, "GXorInverted")
macdef GXnand = $extval (int, "GXnand")
macdef GXset = $extval (int, "GXset")

(* LineStyle *)

macdef LineSolid = $extval (int, "LineSolid")
macdef LineOnOffDash = $extval (int, "LineOnOffDash")
macdef LineDoubleDash = $extval (int, "LineDoubleDash")

(* capStyle *)

macdef CapNotLast = $extval (int, "CapNotLast")
macdef CapButt = $extval (int, "CapButt")
macdef CapRound = $extval (int, "CapRound")
macdef CapProjecting = $extval (int, "CapProjecting")

(* joinStyle *)

macdef JoinMiter = $extval (int, "JoinMiter")
macdef JoinRound = $extval (int, "JoinRound")
macdef JoinBevel = $extval (int, "JoinBevel")

(* fillStyle *)

macdef FillSolid = $extval (int, "FillSolid")
macdef FillTiled = $extval (int, "FillTiled")
macdef FillStippled = $extval (int, "FillStippled")
macdef FillOpaqueStippled = $extval (int, "FillOpaqueStippled")

(* fillRule *)

macdef EvenOddRule = $extval (int, "EvenOddRule")
macdef WindingRule = $extval (int, "WindingRule")

(* subwindow mode *)

macdef ClipByChildren = $extval (int, "ClipByChildren")
macdef IncludeInferiors = $extval (int, "IncludeInferiors")

(** SetClipRectangles ordering *)

macdef Unsorted = $extval (int, "Unsorted")
macdef YSorted = $extval (int, "YSorted")
macdef YXSorted = $extval (int, "YXSorted")
macdef YXBanded = $extval (int, "YXBanded")

(* CoordinateMode for drawing routines *)

macdef CoordModeOrigin = $extval (int, "CoordModeOrigin")
macdef CoordModePrevious = $extval (int, "CoordModePrevious")

(* Polygon shapes *)

macdef Complex = $extval (int, "Complex")
macdef Nonconvex = $extval (int, "Nonconvex")
macdef Convex = $extval (int, "Convex")

(* Arc modes for PolyFillArc *)

macdef ArcChord = $extval (int, "ArcChord")
macdef ArcPieSlice = $extval (int, "ArcPieSlice")

(*
**
** GC components:
** masks used in CreateGC, CopyGC, ChangeGC, OR'ed into GC.stateChanges
**
*)

macdef GCFunction = $extval (int, "GCFunction")
macdef GCPlaneMask = $extval (int, "GCPlaneMask")
macdef GCForeground = $extval (int, "GCForeground")
macdef GCBackground = $extval (int, "GCBackground")
macdef GCLineWidth = $extval (int, "GCLineWidth")
macdef GCLineStyle = $extval (int, "GCLineStyle")
macdef GCCapStyle = $extval (int, "GCCapStyle")
macdef GCJoinStyle = $extval (int, "GCJoinStyle")
macdef GCFillStyle = $extval (int, "GCFillStyle")
macdef GCFillRule = $extval (int, "GCFillRule")
macdef GCTile = $extval (int, "GCTile")
macdef GCStipple = $extval (int, "GCStipple")
macdef GCTileStipXOrigin = $extval (int, "GCTileStipXOrigin")
macdef GCTileStipYOrigin = $extval (int, "GCTileStipYOrigin")
macdef GCFont = $extval (int, "GCFont")
macdef GCSubwindowMode = $extval (int, "GCSubwindowMode")
macdef GCGraphicsExposures = $extval (int, "GCGraphicsExposures")
macdef GCClipXOrigin = $extval (int, "GCClipXOrigin")
macdef GCClipYOrigin = $extval (int, "GCClipYOrigin")
macdef GCClipMask = $extval (int, "GCClipMask")
macdef GCDashOffset = $extval (int, "GCDashOffset")
macdef GCDashList = $extval (int, "GCDashList")
macdef GCArcMode = $extval (int, "GCArcMode")

macdef GCLastBit = $extval (int, "GCLastBit")

(* ****** ****** *)

(* end of [X.sats] *)
