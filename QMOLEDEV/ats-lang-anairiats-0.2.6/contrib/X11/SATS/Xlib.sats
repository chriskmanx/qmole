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
//
// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Start Time: January, 2010
//
(* ****** ****** *)

%{#
#include "contrib/X11/CATS/Xlib.cats"
%} // end of [{%#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload "contrib/X11/SATS/X.sats"

(* ****** ****** *)

// these are resource allocated by
absviewtype XPtr (a:viewt@ype, l:addr)
viewtypedef XPtr0 (a:viewt@ype) = [l:addr] XPtr (a, l)
viewtypedef XPtr1 (a:viewt@ype) = [l:addr | l > null] XPtr (a, l)
//
prfun XPtr_viewget {a:viewt@ype} {l:agz}
  (x: !XPtr (a, l)): (a @ l, minus (XPtr (a, l), a @ l))
//
castfn ptr_of_XPtr {a:viewt@ype} {l:addr} (x: !XPtr (a, l)): ptr l
overload ptr_of with ptr_of_XPtr

//

absviewtype XArray (a:viewt@ype, n:int, l:addr)
viewtypedef XArray1 (a:viewt@ype, n:int) = [l:agz] XArray (a, n, l)
//
prfun XArray_viewget
  {a:viewt@ype} {n:nat} {l:agz} (x: !XArray (a, n, l))
  : (array_v (a, n, l), minus (XArray (a, n, l), array_v (a, n, l)))
//
castfn ptr_of_XArray
  {a:viewt@ype} {n:nat} {l:addr} (x: !XArray (a, n, l)): ptr l
overload ptr_of with ptr_of_XArray

//

absviewtype XString (l:addr)
viewtypedef XString0 = [l:agez] XString (l)
viewtypedef XString1 = [l:addr | l > null] XString (l)
//
castfn ptr_of_XString {l:addr} (x: !XString (l)): ptr l
overload ptr_of with ptr_of_XString

//
// HX: for a array of linear strings:
//
absviewtype XStrarr (n:int, l:addr) // array + strings are allocated by XAlloc
viewtypedef XStrarr1 (n:int) = [l:addr | l > null] XStrarr (n, l)

(* ****** ****** *)

fun XPtrFree
  {a:viewt@ype}
  {l:addr} (
  x: XPtr (a, l)
) : void
  = "mac#atsctrb_XFree"
// end of [XPtrFree]

fun XArrayFree
  {a:viewt@ype}
  {n:nat}
  {l:addr} (
  x: XArray (a, n, l)
) : void
  = "mac#atsctrb_XFree"
// end of [XArrayFree]

fun XStringFree
  {a:viewt@ype}
  {l:addr} (
  x: XString l
) : void
  = "mac#atsctrb_XFree"
// end of [XStringFree]

(* ****** ****** *)
//
// HX-2010-01-22:
// it is just a pointer; it is not reference counted
//
absviewtype Display_ptr (l:addr) // Display*
viewtypedef Display_ptr0 = [l:agez] Display_ptr l
viewtypedef Display_ptr1 = [l:addr | l > null] Display_ptr l
//
// HX-2010-01-22:
// it is just a pointer; it is not reference counted
//
absviewtype Screen_ptr (l:addr) // Screen*
viewtypedef Screen_ptr0 = [l:agez] Screen_ptr l
viewtypedef Screen_ptr1 = [l:addr | l > null] Screen_ptr l
//
// HX: it is just a pointer; it is not reference counted
//
absviewtype Visual_ptr (l:addr) // Visual*
viewtypedef Visual_ptr0 = [l:agez] Visual_ptr l
viewtypedef Visual_ptr1 = [l:addr | l > null] Visual_ptr l

(* ****** ****** *)
//
// HX: it is just a pointer; it is not reference counted
//
absviewtype GCptr (l:addr) = ptr // $extype"GC" // *GC = _XGC
viewtypedef GCptr0 = [l:agez] GCptr l
viewtypedef GCptr1 = [l:addr | l > null] GCptr l
abstype GCref = $extype"GC" // this one should never be freed!

(* ****** ****** *)
//
//
// Chapter 2: Display Functions
//
//
(* ****** ****** *)
//
// 2.1: opening the display
//
(* ****** ****** *)

fun XOpenDisplay (name: Stropt): Display_ptr0 = "mac#atsctrb_XOpenDisplay"

fun Display_ptr_is_null {l:addr} (p_dpy: !Display_ptr l): bool (l == null)
  = "atspre_ptr_is_null" // defined in $ATSHOME/prelude/CATS/pointer.cats
fun Display_ptr_isnot_null {l:addr} (p_dpy: !Display_ptr l): bool (l > null)
  = "atspre_ptr_isnot_null" // defined in $ATSHOME/prelude/CATS/pointer.cats

(* ****** ****** *)
//
// 2.2: obtaining information about display, image formats or screens
//
(* ****** ****** *)
//
// 2.2.1: display macros
//
(* ****** ****** *)

fun XAllPlanes (): ulint = "mac#atsctrb_XAllPlanes"

fun XBlackPixel {l:agz}
  (dpy: !Display_ptr l, nscr: int):<> ulint
  = "mac#atsctrb_XBlackPixel"
fun XWhitePixel {l:agz}
  (dpy: !Display_ptr l, nscr: int):<> ulint
  = "mac#atsctrb_XWhitePixel"

fun XConnectionNumber {l:agz} (dpy: !Display_ptr l):<> int
  = "mac#atsctrb_XConnectionNumber"

fun XDefaultColormap {l:agz}
  (dpy: !Display_ptr l, nscr: int):<> Colormap
  = "mac#atsctrb_XDefaultColormap"

(* ****** ****** *)

fun XDefaultDepth {l:agz}
  (dpy: !Display_ptr l, nscr: int):<> int
  = "mac#atsctrb_XDefaultDepth"

fun XListDepths {l:agz} (
  dpy: !Display_ptr l
, nscr: int, cnt: &int? >> opt (int n, la > null)
) : #[n:nat;la:addr] XArray (int, n, la)
  = "mac#atsctrb_XListDepths"
// end of [XListDepths]

(* ****** ****** *)

fun XDefaultGC
  {l:agz} (
  dpy: !Display_ptr l, nscr: int
) : GCref
  = "mac#atsctrb_XDefaultGC"
// end of [XDefaultGC]

(* ****** ****** *)

fun XDefaultRootWindow
  {l:agz} (
  dpy: !Display_ptr l
) : Window
  = "mac#atsctrb_XDefaultRootWindow"
// end of [XDefaultRootWindow]

fun XDefaultScreenOfDisplay
  {l1:agz} (
  dpy: !Display_ptr l1
  ) : [l2:agz] (
  minus (Display_ptr l1, Screen_ptr l2) | Screen_ptr l2
) = "mac#atsctrb_XDefaultScreenOfDisplay"
// end of [XDefaultScreenOfDisplay]

fun XScreenOfDisplay
  {l1:agz} (
  dpy: !Display_ptr l1, nsrc: int
) : [l2:agz] (
  minus (Display_ptr l1, Screen_ptr l2) | Screen_ptr l2
) = "mac#atsctrb_XDefaultScreenOfDisplay"
// end of [XDefaultScreenOfDisplay]

fun XDefaultScreen {l:agz} (dpy: !Display_ptr l): int(*nscr*)
  = "mac#atsctrb_XDefaultScreen"

fun XDefaultVisual
  {l1:agz} (
  dpy: !Display_ptr l1, nsrc: int
) : [l2:agz] (
  minus (Display_ptr l1, Visual_ptr l2) | Visual_ptr l2
) = "mac#atsctrb_XDefaultVisual"
// end of [XDefaultVisual]

// number of entries in the default colormap
fun XDisplayCells {l:agz}
  (dpy: !Display_ptr l, nscr: int): int(*ncell*)
  = "mac#atsctrb_XDisplayCells"

// the depth of the root window
fun XDisplayPlanes {l:agz}
  (dpy: !Display_ptr l, nscr: int): int(*depth*)
  = "mac#atsctrb_XDisplayPlanes"

// the name passed to XOpenDisplay
fun XDisplayString {l:agz} (dpy: !Display_ptr l): string
  = "mac#atsctrb_XDisplayString"

(* ****** ****** *)

fun XMaxRequestSize {l:agz}
  (dpy: !Display_ptr l): lint // in 4-byte units
  = "mac#atsctrb_XMaxRequestSize"

// the full serial number for the last processed request
fun XLastKnownRequestProcessed {l:agz} (dpy: !Display_ptr l): ulint
  = "mac#atsctrb_XLastKnownRequestProcessed"

// the full serial number to be used for the next request
fun XNextRequest {l:agz} (dpy: !Display_ptr l): ulint
  = "mac#atsctrb_XNextRequest"

(* ****** ****** *)

fun XProtocolVersion {l:agz} (dpy: !Display_ptr l): int
  = "mac#atsctrb_XProtocolVersion"

fun XProtocolRevision {l:agz} (dpy: !Display_ptr l): int
  = "mac#atsctrb_XProtocolRevision"

(* ****** ****** *)

// the length of the event queue for [dpy]
fun XQLength {l:agz} (dpy: !Display_ptr l): int
  = "mac#atsctrb_XQLength"

(* ****** ****** *)

fun XRootWindow
  {l:agz} (
  dpy: !Display_ptr l, nscr: int
) : Window
  = "mac#atsctrb_XRootWindow"
// end of [fun]

fun XScreenCount {l:agz} (dpy: !Display_ptr l): int
  = "mac#atsctrb_XScreenCount"

fun XServerVendor {l:agz} (dpy: !Display_ptr l): string
  = "mac#atsctrb_XServerVendor"

fun XVendorRelease {l:agz} (dpy: !Display_ptr l): int
  = "mac#atsctrb_XVendorRelease"

(* ****** ****** *)
//
// 2.2.2: image format functions and macros
//
(* ****** ****** *)

typedef XPixmapFormatValues =
  $extype_struct "XPixmapFormatValues" of {
  depth= int, bits_per_pixel= int, scanline_pad= int
} // end of [XPixmapFormatValues]

fun XListPixmapFormats
  {l:agz} (
  dpy: !Display_ptr l, n: &int? >> opt (int n, la > null)
) : #[n:nat;la:addr] XArray (XPixmapFormatValues, n, la)
  = "mac#atsctrb_XListPixmapFormats"

macdef LSBFirst = $extval (int, "LSBFirst")
macdef MSBFirst = $extval (int, "MSBFirst")

fun XImageByteOrder {l:agz} (dpy: !Display_ptr l): int
  = "mac#atsctrb_XImageByteOrder"

fun XBitmapUnit {l:agz} (dpy: !Display_ptr l): int
  = "mac#atsctrb_XBitmapUnit"

fun XBitmapOrder {l:agz} (dpy: !Display_ptr l): int
  = "mac#atsctrb_XBitmapOrder"

fun XBitmapPad {l:agz} (dpy: !Display_ptr l): int
  = "mac#atsctrb_XBitmapPad"

fun XDisplayHeight {l:agz} (dpy: !Display_ptr l, nscr: int): int
  = "mac#atsctrb_XDisplayHeight"

fun XDisplayHeightMM {l:agz} (dpy: !Display_ptr l, nscr: int): int
  = "mac#atsctrb_XDisplayHeightMM"

fun XDisplayWidth {l:agz} (dpy: !Display_ptr l, nscr: int): int
  = "mac#atsctrb_XDisplayWidth"

fun XDisplayWidthMM {l:agz} (dpy: !Display_ptr l, nscr: int): int
  = "mac#atsctrb_XDisplayWidthMM"

(* ****** ****** *)
//
// 2.2.3: screen information macros
//
(* ****** ****** *)

fun XBlackPixelOfScreen {l:agz} (scr: !Screen_ptr l): ulint
  = "mac#atsctrb_XBlackPixelOfScreen"

fun XWhitePixelOfScreen {l:agz} (scr: !Screen_ptr l): ulint
  = "mac#atsctrb_XWhitePixelOfScreen"

fun XCellsOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XCellsOfScreen"

fun XDefaultColormapOfScreen {l:agz} (scr: !Screen_ptr l): Colormap
  = "mac#atsctrb_XDefaultColormapOfScreen"

fun XDefaultDepthOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XDefaultDepthOfScreen"

fun XDefaultGCOfScreen {l:agz} (scr: !Screen_ptr l): GCref
  = "mac#atsctrb_XDefaultGCOfScreen"

//
// HX: the function returns WhenMapped, NotUseful or Always
//
fun XDoesBackingStore {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XDoesBackingStore"

fun XDoesSaveUnders {l:agz} (scr: !Screen_ptr l): bool
  = "mac#atsctrb_XDoesSaveUnders"

fun XScreenNumberOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XScreenNumberofScreen"

fun XEventMaskOfScreen {l:agz} (scr: !Screen_ptr l): lint
  = "mac#atsctrb_XEventMaskOfScreen"

fun XWidthOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XWidthOfScreen"

fun XWidthMMOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XWidthMMOfScreen"

fun XHeightOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XHeightOfScreen"

fun XHeightMMOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XHeightMMOfScreen"

fun XMaxCmapsOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XMaxCmapsOfScreen"

fun XMinCmapsOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XMinCmapsOfScreen"

fun XPlanesOfScreen {l:agz} (scr: !Screen_ptr l): int
  = "mac#atsctrb_XPlanesOfScreen"

fun XRootWindowOfScreen {l:agz} (scr: !Screen_ptr l): Window
  = "mac#atsctrb_XRootWindowOfScreen"

(* ****** ****** *)
//
// 2.3: generating a NoOperation protocol request
//
(* ****** ****** *)

fun XNoOp {l:agz} (dpy: !Display_ptr l): void
  = "mac#atsctrb_XNoOp"

(* ****** ****** *)
//
// 2.4: freeing client-created data
//
(* ****** ****** *)

absview XFree_v (l:addr)

fun XFree0 {l:addr}
  (pf: XFree_v l | p: ptr l): void = "mac#atsctrb_XFree"
// end of [XFree0]

symintr XFree
overload XFree with XFree0
overload XFree with XPtrFree
overload XFree with XArrayFree
overload XFree with XStringFree

(* ****** ****** *)
//
// 2.5: closing the display
//
(* ****** ****** *)

fun XCloseDisplay (dpy: Display_ptr1): void = "mac#atsctrb_XCloseDisplay"

abst@ype close_mode_t = int
macdef DestroyAll = $extval (close_mode_t, "DestroyAll")
macdef RetainPermanent = $extval (close_mode_t, "RetainPermanent")
macdef RetainTemporary = $extval (close_mode_t, "RetainTemporary")
//
// [XSetCloseDownMode] may generate a BadValue error
//
fun XSetCloseDownMode
  {l:agz} (dpy: Display_ptr l, mode: close_mode_t): void
// end of [XSetCloseDownMode]

(* ****** ****** *)
//
//
// Chapter 3: Window Functions
//
//
(* ****** ****** *)
//
// 3.1: visual types
//
(* ****** ****** *)

fun XVisualIDFromVisual {l:agz} (visual: !Visual_ptr l): VisualID
  = "mac#atsctrb_XVisualIDFromVisual"
  
(* ****** ****** *)
//
// 3.2: window attributes
//
(* ****** ****** *)

typedef XSetWindowAttributes =
  $extype_struct "XSetWindowAttributes" of {
  background_pixmap= Pixmap
, background_pixel= ulint
, border_pixmap= Pixmap
, border_pixel= ulint
, bit_gravity= int
, win_gravity= int
, backing_store= int
, backing_planes= ulint
, backing_pixel= ulint
, save_under= bool
, event_mask= lint
, do_not_propagate_mask= lint
, override_redirect= bool
, colormap= Colormap
, cursor= Cursor
} // end of [XSetWindowAttributes]

(* ****** ****** *)
//
// 3.3: creating windows
//
(* ****** ****** *)

fun XCreateWindow {ld,lv:agz} (
  dpy: !Display_ptr ld
, parent: Window
, x: int, y: int
, width: uint, height: uint
, border_width: uint
, depth: uint // can [depth] be negative?
, _class: uint
, visual: !Visual_ptr lv
, valuemask: ulint
, attr: &XSetWindowAttributes
) : Window
  = "mac#atsctrb_XCreateWindow"
// end of [XCreateWindow]

fun XCreateSimpleWindow {ld:agz} (
  dpy: !Display_ptr ld
, parent: Window
, x: int, y: int
, width: uint, height: uint
, border_width: uint // in pixels
, border: ulint // border pixel value
, background: ulint // background pixel value
) : Window
  = "mac#atsctrb_XCreateSimpleWindow"
// end of [XCreateSimpleWindow]

(* ****** ****** *)
//
// 3.4: destroying windows
//
(* ****** ****** *)

fun XDestroyWindow {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XDestroyWindow"

fun XDestroySubwindows {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XDestroyWindow"

(* ****** ****** *)
//
// 3.5: mapping windows
//
(* ****** ****** *)

fun XMapWindow {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XMapWindow"

fun XMapRaised {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XMapRaised"

fun XMapSubwindows {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XMapSubwindows"

(* ****** ****** *)
//
// 3.6: unmapping windows
//
(* ****** ****** *)

fun XUnmapWindow {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XUnmapWindow"

fun XUnmapSubwindows {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XUnmapSubwindows"

(* ****** ****** *)
//
// 3.7: configuring windows
//
(* ****** ****** *)

typedef XWindowChanges =
  $extype_struct "XWindowChanges" of {
  x= int, y= int
, width= int, height= int
, border_width= int
, sibling= Window
, stack_mode= int
} // end of [XWindowChanges]

fun XConfigureWindow {l:agz} (
  dpy: !Display_ptr l
, win: Window
, valmask: uint
, values: &XWindowChanges
) : void
  = "mac#atsctrb_XConfigureWindow"

fun XMoveWindow {l:agz}
  (dpy: !Display_ptr l, win: Window, x: int, y: int): void
  = "mac#atsctrb_XMoveWindow"

fun XResizeWindow {l:agz}
  (dpy: !Display_ptr l, win: Window, width: uint, height: uint): void
  = "mac#atsctrb_XResizeWindow"

fun XMoveResizeWindow {l:agz} (
  dpy: !Display_ptr l, win: Window, x: int, y: int, width: uint, height: uint
) : void
  = "mac#atsctrb_XMoveResizeWindow"

fun XSetWindowBorderWidth {l:agz}
  (dpy: !Display_ptr l, win: Window, border_width: uint): void
  = "mac#atsctrb_XSetWindowBorderWidth"

(* ****** ****** *)
//
// 3.8: changing windows stacking order
//
(* ****** ****** *)

fun XRaiseWindow {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XRaiseWindow"

fun XLowerWindow {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XLowerWindow"

fun XCirculateSubwindows {l:agz}
  (dpy: !Display_ptr l, win: Window, dir: int): void
  = "mac#atsctrb_XCirculateSubwindows"

fun XCirculateSubwindowsUp {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XCirculateSubwindowsUp"

fun XCirculateSubwindowsDown {l:agz}
  (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XCirculateSubwindowsDown"

fun XRestackWindows {l:agz} {n:nat}
  (dpy: !Display_ptr l, wins: &(@[Window][n]), nwin: int n): void
  = "mac#atsctrb_XRestackWindows"

(* ****** ****** *)
//
// 3.9: changing windows attributes
//
(* ****** ****** *)

fun XChangeWindowAttributes {l:agz} (
  dpy: !Display_ptr l
, win: Window
, valmask: ulint
, attr: &XSetWindowAttributes
) : void
  = "mac#atsctrb_XChangeWindowAttributes"

fun XSetWindowBackground {l:agz}
  (dpy: !Display_ptr l, win: Window, bg_pixel: ulint): void
  = "mac#atsctrb_XSetWindowBackground"

fun XSetWindowBackgroundPixmap {l:agz}
  (dpy: !Display_ptr l, win: Window, bg_pixmap: Pixmap): void
  = "mac#atsctrb_XSetWindowBackgroundPixmap"

fun XSetWindowBorder {l:agz}
  (dpy: !Display_ptr l, win: Window, bd_pixel: ulint): void
  = "mac#atsctrb_XSetWindowBorder"

fun XSetWindowBorderPixmap {l:agz}
  (dpy: !Display_ptr l, win: Window, bd_pixmap: Pixmap): void
  = "mac#atsctrb_XSetWindowBorderPixmap"

fun XSetWindowColormap {l:agz}
  (dpy: !Display_ptr l, win: Window, colormap: Colormap): void
  = "mac#atsctrb_XSetWindowColormap"

fun XDefineCursor {l:agz}
  (dpy: !Display_ptr l, win: Window, cursor: Cursor): void
  = "mac#atsctrb_XDefineCursor"

fun XUndefineCursor {l:agz} (dpy: !Display_ptr l, win: Window): void
  = "mac#atsctrb_XUndefineCursor"

(* ****** ****** *)
//
//
// Chapter 4: Window Information Functions
//
//
(* ****** ****** *)

abst@ype Status (i:int) = $extype"Status" // = int
typedef Status = [i:int] Status i
castfn int_of_Status {i:int} (x: Status i):<> int i
overload int_of with int_of_Status

fun XQueryTree {l:agz} (
  dpy: !Display_ptr l
, win: Window
, root: &Window? >> Window
, parent: &Window? >> Window
, children: &ptr? >> opt (XArray1 (Window, n), i <> 0)
, nchilren: &int? >> opt (int n, i <> 0)
) : #[i:int;n:nat] Status i
  = "mac#atsctrb_XQueryTree"
// end of [XQueryTree]

typedef XWindowAttributes =
  $extype_struct "XWindowAttributes" of {
  x= int, y= int
, width= uint, height= uint
, border_width= uint
, depth= int
// , visual= Visual_ptr1
, root= Window
, _class= int
, bit_gravity= int
, win_gravity= int
, backing_store= int
, backing_planes= ulint
, backing_pixel= ulint
, save_under= bool
, colormap= Colormap
, map_installed= bool
, map_state= int
, all_event_mask= lint
, your_event_mask= lint
, do_not_propagate_mask= lint
, override_redirect= bool
// , screen= Screen_ptr1
} // end of [XWindowAttributes]

fun XGetWindowAttributes {l:agz} (
  dpy: !Display_ptr l, win: Window
, attr: &XWindowAttributes? >> opt (XWindowAttributes, i <> 0)
) : #[i:int] Status i
  = "mac#atsctrb_XGetWindowAttributes"

fun XGetGeometry {l:agz} (
  dpy: !Display_ptr l, drw: Drawable
, root: &Window? >> Window
, x: &int? >> int, y: &int? >> int
, width: &uint? >> uint, height: &uint? >> uint
, border_width: &uint? >> uint
, depth: &uint? >> uint
) : Status // ...
  = "mac#atsctrb_XGetWindowAttributes"
// end of [fun]

(* ****** ****** *)
//
// 4.2: translating screen coordinates
//
(* ****** ****** *)

fun XTranslateCoordinates {l:agz} (
  dpy: !Display_ptr l
, win_src: Window, win_dst: Window
, x_src: int, y_src: int
, x_dst: &int? >> int, y_dst: &int? >> int
, child: &Window? >> Window
) : bool
  = "mac#atsctrb_XTranslateCoordinates"
// end of [fun]

fun XQueryPointer {l:agz} (
  dpy: !Display_ptr l
, win: Window
, root: &Window? >> Window, child: &Window? >> Window
, x_root: &int? >> int, y_root: &int? >> int
, x_win: &int? >> int, y_win: &int? >> int
, mask: &uint? >> uint
) : bool
  = "mac#atsctrb_XQueryPointer"
// end of [fun]

(* ****** ****** *)
//
// 4.3: properties and atoms
//
(* ****** ****** *)

fun XInternAtom {l:agz} (
  dpy: !Display_ptr l, atom_name: string, only_if_exists: bool
) : Atom = "mac#atsctrb_XInternAtom"
// end of [XInternAtom]

fun XGetAtomName {l:agz} (
  dpy: !Display_ptr l, atom: Atom
) : XString0
  = "mac#atsctrb_XGetAtomName"
// end of [XGetAtomName]

(* ****** ****** *)
//
// 4.4: obtaining and changing window properties
//
(* ****** ****** *)
//
//
// Chapter 5: Pixmap and Cursor Functions
//
//
(* ****** ****** *)
//
// 5.1: creating and freeing pixmaps
//
(* ****** ****** *)

fun XCreatePixmap {l:agz} (
  dpy: !Display_ptr l
, drw: Drawable, width: uint, height: uint, depth: uint
) : Pixmap
  = "mac#atsctrb_XCreatePixmap"
// end of [fun]

fun XFreePixmap
  {l:agz} (dpy: !Display_ptr l, pixmap: Pixmap): void
  = "mac#atsctrb_XFreePixmap"
// end of [XFreePixmap]

(* ****** ****** *)
//
//
// 5.2: creating, recoloring and freeing cursors
//
//
(* ****** ****** *)

fun XCreateFontCursor
  {l:agz} (
  dpy: !Display_ptr l, shape: uint
) : Cursor
  = "mac#atsctrb_XCreateFontCursor"
// end of [fun]

fun XFreeCursor
  {l:agz} (
  dpy: !Display_ptr l, cursor: Cursor
) : void
  = "mac#atsctrb_XFreeCursor"
// end of [fun]

(* ****** ****** *)
//
//
// Chapter 6: Color Management Functions
//
//
(* ****** ****** *)
//
// 6.1: color structures
//
(* ****** ****** *)

typedef XColor =
  $extype_struct "XColor" of {
  pixel= ulint
, red= usint, green= usint, blue= usint
, flags= char, pad= char
} // end of [XColor]

(* ****** ****** *)
//
// 6.4: creating, copying and destroying
//
(* ****** ****** *)

fun XCreateColormap {l1,l2:agz} (
  dsp: !Display_ptr l1
, win: Window, visual: !Visual_ptr l2, alloc: int
) : Colormap
  = "mac#atsctrb_XCreateColormap"
// end of [fun]

fun XCopyColormapAndFree
  {l:agz} (
  dpy: !Display_ptr l, colormap: Colormap
) : void
  = "mac#atsctrb_XCopyColormapAndFree"
// end of [fun]

fun XFreeColormap
  {l:agz} (
  dpy: !Display_ptr l, colormap: Colormap
) : void
  = "mac#atsctrb_XFreeColormap"
// end of [fun]

(* ****** ****** *)
//
// 6.5: Mapping Color Names to Values
//
(* ****** ****** *)

fun XLookupColor {l:agz} (
  dpy: !Display_ptr l
, colormap: Colormap, color_name: string
, exact_def: &XColor? >> opt (XColor, i <> 0)
, screen_def: &XColor? >> opt (XColor, i <> 0)
) : #[i:int] Status i // nonzero if the name is resolved
  = "mac#atsctrb_XLookupColor"
// end of [XLookupColor]

fun XParseColor {l:agz} (
  dpy: !Display_ptr l
, colormap: Colormap
, spec: string
, exact_def: &XColor? >> opt (XColor, i <> 0)
) : #[i:int] Status i // nonzero if the name is resolved
  = "mac#atsctrb_XParseColor"
// end of [XParseColor]

(*
fun XcmsLookupColor (...)
*)

(* ****** ****** *)
//
// 6.6: Allocating and Freeing Color Cells
//
(* ****** ****** *)

fun XAllocColor {l:agz} (
  dpy: !Display_ptr l
, colormap: Colormap, screen_in_out: &XColor >> opt (XColor, i <> 0)
) : #[i:int] Status i
  = "mac#atsctrb_XAllocColor"
// end of [XAllocColor]

(*
fun XcmsAllocColor (...)
*)

fun XAllocNamedColor {l:agz} (
  dpy: !Display_ptr l
, colormap: Colormap
, color_name: string
, screen_def: &XColor? >> opt (XColor, i <> 0)
, exact_def: &XColor? >> opt (XColor, i <> 0)
) : #[i:int] Status i
  = "mac#atsctrb_XAllocNamedColor"
// end of [XAllocNamedColor]

(*
fun XcmsAllocNamedColor (...)
*)

(*
fun XAllocColorCells (...)
fun XAllocColorPlanes (...)
*)

(*
fun XFreeColors (...)
*)

(* ****** ****** *)
//
// 6.7: Modifying and Querying Colormap Cells
//
(* ****** ****** *)

fun XStoreColor {l:agz} (
  dpy: !Display_ptr l, colormap: Colormap, color: &XColor
) : void = "mac#atsctrb_XStoreColor"
// end of [XStoreColor]

fun XStoreColors {l:agz} {n:nat} (
  dpy: !Display_ptr l
, colormap: Colormap, colors: &(@[XColor][n]), ncolor: int n
) : void = "mac#atsctrb_XStoreColors"
// end of [XStoreColors]

(*
XcmsStoreColor (...)
XcmsStoreColors (...)
*)

fun XStoreNamedColor {l:agz} (
  dpy: !Display_ptr l
, colormap: Colormap, color_name: string, pixel: ulint, flags: int
) : void = "mac#atsctrb_XStoreNamedColor"
// end of [XStoreNamedColor]

fun XQueryColor {l:agz} (
  dpy: !Display_ptr l
, colormap: Colormap, def_in_out: &XColor >> XColor
) : void = "mac#atsctrb_XQueryColor"
// end of [XQueryColor]

fun XQueryColors {l:agz} {n:nat} (
  dpy: !Display_ptr l
, colormap: Colormap, defs_in_out: &(@[XColor][n]), ncolor: int n
) : void = "mac#atsctrb_XQueryColors"
// end of [XQueryColors]

(*
fun XcmsQueryColor (...)
fun XcmsQueryColors (...)
*)

(* ****** ****** *)
//
// 6.8: Color Conversion Context Functions
//
(* ****** ****** *)
//
// 6.9: Converting Between Color Spaces
//
(* ****** ****** *)
//
// 6.10: Callback functions
//
(* ****** ****** *)
//
// 6.11: Gamut querying functions
//
(* ****** ****** *)
//
// 6.12: Color management extensions
//
(* ****** ****** *)
//
//
// Chapter 7: Graphics Context Functions
//
//
(* ****** ****** *)
//
// 7.1: Manipulating Graphics Context/State
//
(* ****** ****** *)

macdef GCFunction = $extval (lint, "GCFunction")
macdef GCPlaneMask = $extval (lint, "GCPlaneMask")
macdef GCForeground = $extval (lint, "GCForeground")
macdef GCBackground = $extval (lint, "GCBackground")
macdef GCLineWidth = $extval (lint, "GCLineWidth")
macdef GCLineStyle = $extval (lint, "GCLineStyle")
macdef GCCapStyle = $extval (lint, "GCCapStyle")
macdef GCJoinStyle = $extval (lint, "GCJoinStyle")
macdef GCFillStyle = $extval (lint, "GCFillStyle")
macdef GCFillRule = $extval (lint, "GCFillRule")
macdef GCTile = $extval (lint, "GCTile")
macdef GCStipple = $extval (lint, "GCStipple")
macdef GCTileStipXOrigin = $extval (lint, "GCTileStipXOrigin")
macdef GCTileStipYOrigin = $extval (lint, "GCTileStipYOrigin")
macdef GCFont = $extval (lint, "GCFont")
macdef GCSubWindowMode = $extval (lint, "GCSubWindowMode")
macdef GCGraphicsExposures = $extval (lint, "GCGraphicsExposures")
macdef GCClipXOrigin = $extval (lint, "GCClipXOrigin")
macdef GCClipYOrigin = $extval (lint, "GCClipYOrigin")
macdef GCClipMask = $extval (lint, "GCClipMask")
macdef GCDashOffset = $extval (lint, "GCDashOffset")
macdef GCDashList = $extval (lint, "GCDashList")
macdef GCArcMode = $extval (lint, "GCArcMode")

typedef XGCValues =
  $extype_struct "XGCValues" of {
  function= int
, plane_mask= ulint
, foreground= ulint
, background= ulint
, line_width= int
, line_style= int
, cap_style= int
, join_style= int
, fill_style= int
, arc_mode= int
, tile= Pixmap
, stipple= Pixmap
, ts_x_origin = int
, ts_y_origin = int
, font= Font
, subwindow_mode= int
, graphics_exposures= XBool
, clip_x_origin= int
, clip_y_origin= int
, clip_mask= Pixmap
, dash_offset= int
, dashes= char
} // end of [XGCValues]

fun GCptr_is_null {l:addr} (p_dpy: !GCptr l): bool (l == null)
  = "atspre_ptr_is_null" // defined in $ATSHOME/prelude/CATS/pointer.cats
fun GCptr_isnot_null {l:addr} (p_dpy: !GCptr l): bool (l > null)
  = "atspre_ptr_isnot_null" // defined in $ATSHOME/prelude/CATS/pointer.cats

fun XCreateGC {l:agz} ( // [values] can be uninitialized ...
  dpy: !Display_ptr l, drw: Drawable, mask: ulint, values: &XGCValues?
) : GCptr1 // HX-2010-04-18: should it be GCptr0?
  = "mac#atsctrb_XCreateGC"
// end of [XCreateGC]

fun XCopyGC {l1,l2,l3:agz}
  (dpy: !Display_ptr l1, src: !GCptr l2, dst: !GCptr l3, mask: ulint): void
  = "mac#atsctrb_XCopyGC"
// end of [XCopyGC]

fun XChangeGC {l1,l2:agz}
  (dpy: !Display_ptr l1, gc: !GCptr l2, mask: ulint, values: &XGCValues): void
  = "mac#atsctrb_XChangeGC"
// end of [XChangeGC]

fun XGetGCValues {l1,l2:agz} (
  dpy: !Display_ptr l1, gc: !GCptr l2, mask: ulint, values: &XGCValues? >> XGCValues
) : void = "mac#atsctrb_XGetGCValues"
// end of [XGetGCValues]

fun XFreeGC {l1,l2:agz}
  (dpy: !Display_ptr l1, gc: GCptr l2): void = "mac#atsctrb_XFreeGC"
// end of [XFreeGC]

fun XFlushGC {l1,l2:agz}
  (dpy: !Display_ptr l1, gc: !GCptr l2): void = "mac#atsctrb_XFlushGC"
// end of [XFlushGC]

(* ****** ****** *)
//
// 7.2: Using GC convenience routines
//
(* ****** ****** *)

fun XSetForeground {l1,l2:agz}
  (dpy: !Display_ptr l1, gc: !GCptr l2, foreground: ulint): void
  = "mac#atsctrb_XSetForeground"
// end of [XSetForeground]

fun XSetBackground {l1,l2:agz}
  (dpy: !Display_ptr l1, gc: !GCptr l2, background: ulint): void
  = "mac#atsctrb_XSetBackground"
// end of [XSetBackground]                                    

fun XSetFunction {l1,l2:agz}
  (dpy: !Display_ptr l1, gc: !GCptr l2, _function: int): void
  = "mac#atsctrb_XSetFunction"
// end of [XSetFunction]                                    

fun XSetPlaneMask {l1,l2:agz}
  (dpy: !Display_ptr l1, gc: !GCptr l2, plane_mask: ulint): void
  = "mac#atsctrb_XSetPlaneMask"
// end of [XSetPlaneMask]                                    

fun XSetFont {l1,l2:agz}
  (dpy: !Display_ptr l1, gc: !GCptr l2, font: Font): void
  = "mac#atsctrb_XSetFont"
// end of [XSetFont]

fun XSetLineAttributes {l1,l2:agz} (
  dpy: !Display_ptr l1, gc: !GCptr l2
, line_width: uint, line_style: int, cap_style: int, join_style: int
) : void = "mac#atsctrb_XSetLineAttributes"
// end of [XSetLineAttributes]

fun XSetDashes {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1, gc: !GCptr l2
, dash_offset: int, dash_list: &(@[char][n]), n: int n
) : void
  = "mac#atsctrb_XSetDashes"
// end of [XSetDashes]

(* ****** ****** *)
//
//
// Chapter 8: Graphics Functions
//
//
(* ****** ****** *)
//
// 8.1: Clearing Areas
//
(* ****** ****** *)

fun XClearArea {l:agz} (
  dsp: !Display_ptr l
, win: Window
, x: int, y: int
, width: uint, height: uint
, exposures: XBool
) : void
  = "mac#atsctrb_XClearArea"
// end of [fun]

fun XClearWindow {l:agz} (dsp: !Display_ptr l, win: Window) : void
  = "mac#atsctrb_XClearWindow"
// end of [XClearWindow]

(* ****** ****** *)
//
// 8.2: Copying Areas
//
(* ****** ****** *)

fun XCopyArea
  {l1,l2:agz} (
  dpy: !Display_ptr l1
, src: Drawable, dst: Drawable
, gc: !GCptr l2
, x_src: int, y_src: int
, width: uint, height: uint
, x_dst: int, y_dst: int
) : void
  = "mac#atsctrb_XCopyArea"
// end of [fun]

fun XCopyPlane
  {l1,l2:agz} (
  dpy: !Display_ptr l1
, src: Drawable, dst: Drawable
, gc: !GCptr l2
, x_src: int, y_src: int
, width: uint, height: uint
, x_dst: int, y_dst: int
, plane: ulint // set exactly one-bit to 1
) : void
  = "mac#atsctrb_XCopyPlane"
// end of [fun]

(* ****** ****** *)
//
// 8.3: Drawing Points, Lines, Rectangles and Arcs
//
(* ****** ****** *)

typedef XSegment =
  $extype_struct "XSegment" of {
  x1= sint, y1= sint, x2= sint, y2= sint
} // end of [XSegment]

typedef XPoint =
  $extype_struct "XPoint" of { x= sint, y= sint }
// end of [XPoint]

typedef XRectangle =
  $extype_struct "XRectangle" of {
  x= sint, y= sint, width= usint, height= usint
} // end of [XRectangle]

typedef XArc =
  $extype_struct "XArc" of {
  x= sint, y= sint
, width= usint, height= usint
, angle1= sint, angle2= sint // uint: 1/64 degree
} // end of [XArc]

(* ****** ****** *)
//
// 8.3.1: Drawing Single and Multiple Points
//
(* ****** ****** *)

fun XDrawPoint {l1,l2:agz} (
  dpy: !Display_ptr l1, drw: Drawable, gc: !GCptr l2, x: int, y: int
) : void = "mac#atsctrb_XDrawPoint"
// end of [XDrawPoint]

fun XDrawPoints {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2, points: &(@[XPoint][n]), n: int n, mode: int
) : void = "mac#atsctrb_XDrawPoints"
// end of [XDrawPoints]

(* ****** ****** *)
//
// 8.3.2: Drawing Single and Multiple Lines
//
(* ****** ****** *)

fun XDrawLine {l1,l2:agz} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2, x1: int, y1: int, x2: int, y2: int
) : void = "mac#atsctrb_XDrawLine"
// end of [XDrawLine]

fun XDrawLines {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2, points: &(@[XPoint][n]), n: int n, mode: int
) : void = "mac#atsctrb_XDrawLines"
// end of [XDrawLines]

fun XDrawSegments {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2, segments: &(@[XSegment][n]), n: int n
) : void = "mac#atsctrb_XDrawSegments"
// end of [XDrawSegments]

(* ****** ****** *)
//
// 8.3.3: Drawing Single and Multiple Rectangles
//
(* ****** ****** *)

fun XDrawRectangle {l1,l2:agz} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2, x: int, y: int, width: uint, height: uint
) : void = "mac#atsctrb_XDrawRectangle"
// end of [XDrawRectangle]

fun XDrawRectangles {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2, rectangles: &(@[XRectangle][n]), n: int n
) : void = "mac#atsctrb_XDrawRectangles"
// end of [XDrawRectangles]

(* ****** ****** *)
//
// 8.3.4: Drawing Single and Multiple Arcs
//
(* ****** ****** *)

fun XDrawArc {l1,l2:agz} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2
, x: int, y: int, width: uint, height: uint, angle1: int, angle2: int
) : void
  = "mac#atsctrb_XDrawArc"
// end of [XDrawArc]

fun XDrawArcs {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2, arcs: &(@[XArc][n]), n: int n
) : void
  = "mac#atsctrb_XDrawArcs"
// end of [XDrawArcs]

(* ****** ****** *)
//
// 8.4: Filling Areas
//
(* ****** ****** *)
//
// 8.4.1: Filling Single and Multiple Rectangles
//
(* ****** ****** *)

fun XFillRectangle {l1,l2:agz} (
    dpy: !Display_ptr l1
  , drw: Drawable, gc: !GCptr l2, x: int, y: int, width: uint, height: uint
  ) : void = "mac#atsctrb_XFillRectangle"
// end of [XFillRectangle]

fun XFillRectangles {l1,l2:agz} {n:nat} (
    dpy: !Display_ptr l1
  , drw: Drawable, gc: !GCptr l2, rectangles: &(@[XRectangle][n]), n: int n
  ) : void = "mac#atsctrb_XFillRectangles"
// end of [XFillRectangles]

(* ****** ****** *)
//
// 8.4.2: Filling a Single Polygon
//
(* ****** ****** *)

fun XFillPolygon {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2, points: &(@[XPoint][n]), n: int n
, shape: int, mode: int
) : void = "mac#atsctrb_XFillPolygon"
// end of [XFillPolygon]

(* ****** ****** *)
//
// 8.4.3: Filling Single and Multiple Arcs
//
(* ****** ****** *)

fun XFillArc {l1,l2:agz} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2
, x: int, y: int, width: uint, height: uint, angle1: int, angle2: int
) : void = "mac#atsctrb_XFillArc"
// end of [XFillArc]

fun XFillArcs {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable, gc: !GCptr l2, arcs: &(@[XArc][n]), n: int n
) : void = "mac#atsctrb_XFillArcs"
// end of [XFillArcs]

(* ****** ****** *)
//
// 8.5: Font Metrics
//
(* ****** ****** *)

typedef XCharStruct =
  $extype_struct "XCharStruct" of {
  lbearing= sint, rbearing= sint
, width= sint, ascent= sint, descent= sint
, attributes= usint
} // end of [XCharStruct]

typedef XFontProp =
  $extype_struct "XFontProp" of { name= Atom, card32= ulint }
// end of [XFontProp]

typedef XChar2b =
  $extype_struct "XChar2b" of { byte1= uchar, byte2= uchar }
// end of [XChar2b]

typedef XFontStruct = $extype_struct "XFontStruct" of {
  XExtData= ptr
, fid= Font
, direction= uint
, min_char_or_byte2= uint
, max_char_or_byte2= uint
, min_byte1= uint
, max_byte1= uint
, all_chars_exist= XBool
, default_char= uint
// , n_properties= int n
// , properties= ptr l_properties // @[XFontProp][n] @ l_properties
, min_bounds= XCharStruct
, max_bounds= XCharStruct
// , per_char= ptr // XCharStruct*
, ascent= int
, descent= int
} // end of [XFontStruct]

(* ****** ****** *)
//
// 8.5.1: Loading and Freeing Fonts
//
(* ****** ****** *)

absview XFreeFont_v (l:addr)
prfun XFreeFont_v_unnull (pf: XFreeFont_v null): void

fun XLoadFont {l:agz}
  (dpy: !Display_ptr l, name: string): Font = "mac#atsctrb_XLoadFont"
// end of [XLoadFont]

fun XQueryFont {l:agz}
  (dpy: !Display_ptr l, font_id: XID)
  : [l:agez] (
  XFreeFont_v l, optvar_v (XFontStruct, l) | ptr l
) = "mac#atsctrb_XQueryFont"
// end of [XQueryFont]

fun XLoadQueryFont {l:agz}
  (dpy: !Display_ptr l, name: string)
  : [l:agez] (
  XFreeFont_v l, optvar_v (XFontStruct, l) | ptr l
) = "mac#atsctrb_XLoadQueryFont"
// end of [XLoadQueryFont]

fun XFreeFont {l1,l2:addr} (
  pf1: XFreeFont_v l2, pf2: XFontStruct @ l2 | dpy: !Display_ptr l1, p_font_struct: ptr l2
) : void  = "mac#atsctrb_XFreeFont"
// end of [XFreeFont]
            
(* ****** ****** *)
//
// 8.5.3: Computing Character String Sizes
//
(* ****** ****** *)

fun XTextWidth {n:nat}
  (ftinfo: &XFontStruct, str: string n, nstr: int n): int
  = "mac#atsctrb_XTextWidth"
// end of [XTextWidth]

fun XTextWidth16 {n:nat}
  (ftinfo: &XFontStruct, str: array (XChar2b, n), nstr: int n): int
  = "mac#atsctrb_XTextWidth16"
// end of [XTextWidth]

(* ****** ****** *)
//
// 8.6: Drawing Text
//
(* ****** ****** *)
//
// 8.6.2: Drawing Text Characters
//
(* ****** ****** *)

fun XDrawString
  {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable
, gc: !GCptr l2
, x: int, y: int
, str: string n
, len: int n
) : void = "mac#atsctrb_XDrawString"
// end of [XDrawString]

fun XDrawString16
  {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable
, gc: !GCptr l2
, x: int, y: int
, str: array (XChar2b, n)
, len: int n
) : void = "mac#atsctrb_XDrawString16"
// end of [XDrawString16]

(* ****** ****** *)
//
// 8.6.3: Drawing Image Characters
//
(* ****** ****** *)

fun XDrawImageString
  {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable
, gc: !GCptr l2
, x: int, y: int
, str: string n
, len: int n
) : void = "mac#atsctrb_XDrawImageString"
// end of [XDrawImageString]

fun XDrawImageString16
  {l1,l2:agz} {n:nat} (
  dpy: !Display_ptr l1
, drw: Drawable
, gc: !GCptr l2
, x: int, y: int
, str: array (XChar2b, n)
, len: int n
) : void = "mac#atsctrb_XDrawImageString16"
// end of [XDrawImageString16]

(* ****** ****** *)
//
//
// Chapter 9: Window and Session Manager Functions
//
//
(* ****** ****** *)
//
// 9.1: Changing the parent of a window
//
(* ****** ****** *)

fun XReparentWindow
  {l:agz} (
  dpy: !Display_ptr l
, win: Window
, parent: Window
, x: int
, y: int
) : void
  = "mac#atsctrb_XReparentWindow"
// end of [XReparentWindow]

(* ****** ****** *)
//
// 9.2: Controlling the Lifetime of a Window
//
(* ****** ****** *)

fun XChangeSaveSet {l:agz}
  (dpy: !Display_ptr l, win: Window, mode: int): void
  = "mac#atsctrb_XChangeSaveSet"
// end of [atsctrb_XChangeSaveSet]

fun XAddSaveSet {l:agz}
  (dpy: !Display_ptr l, win: Window): void = "mac#atsctrb_XAddSaveSet"
// end of [XAddSaveSet]

fun XRemoveFromSaveSet {l:agz}
  (dpy: !Display_ptr l, win: Window): void = "mac#atsctrb_XRemoveFromSaveSet"
// end of [XRemoveFromSaveSet]

(* ****** ****** *)
//
// 9.3: Managing installed colormaps
//
(* ****** ****** *)

fun XInstallColormap {l:agz}
  (dpy: !Display_ptr l, colormap: Colormap): void = "mac#atsctrb_XInstallColormap"
// end of [XInstallColormap]

fun XUninstallColormap {l:agz}
  (dpy: !Display_ptr l, colormap: Colormap): void = "mac#atsctrb_XUninstallColormap"
// end of [XUninstallColormap]

fun XListInstalledColormaps {l:agz} (
  dpy: !Display_ptr l, win: Window, nmap: &int? >> opt (int n, la > null)
) : #[n:nat;la:addr] XArray (Colormap, n, la)
  = "mac#atsctrb_XListInstalledColormaps"
// end of [XListInstalledColormaps]

(* ****** ****** *)
//
// 9.4: Setting and Retrieving the Fond Search Path
//
(* ****** ****** *)
//
// 9.5: Server Grabbing
//
(* ****** ****** *)

fun XGrabServer {l:agz}
  (dpy: !Display_ptr l): void = "mac#atsctrb_XGrabServer"
// end of [XGrabServer]

fun XUngrabServer {l:agz}
  (dpy: !Display_ptr l): void = "mac#atsctrb_XUngrabServer"
// end of [XUngrabServer]

(* ****** ****** *)
//
// 9.6: Killing Clients
//
(* ****** ****** *)

fun XKillClient {l:agz}
  (dpy: !Display_ptr l, resource: XID): void = "mac#atsctrb_XKillClient"
// end of [XKillClient]

(* ****** ****** *)
//
// 9.7: Screen Saver Control
//
(* ****** ****** *)

fun XSetScreenSaver {l:agz} (
  dpy: !Display_ptr l
, timeout: int, interval: int, prefer_blanking: int, allow_exposures: int
) : void = "mac#atsctrb_XSetScreenSaver"
// end of [XSetScreenSaver]

fun XForceScreenSaver
  {l:agz} (
  dpy: !Display_ptr l, mode: int
) : void = "mac#atsctrb_XForceScreenSaver"
// end of [XForceScreenSaver]

fun XActivateScreenSaver
  {l:agz} (dpy: !Display_ptr l): void = "mac#atsctrb_XActivateScreenSaver"
// end of [XActivateScreenSaver]

fun XResetScreenSaver
  {l:agz} (dpy: !Display_ptr l): void = "mac#atsctrb_XResetScreenSaver"
// end of [XResetScreenSaver]

fun XGetScreenSaver {l:agz} (
  dpy: !Display_ptr l
, timeout: &int? >> int
, interval: &int? >> int
, prefer_blanking: &int? >> int
, allow_exposures: &int? >> int
) : void
  = "mac#atsctrb_XGetScreenSaver"
// end of [XGetScreenSaver]

(* ****** ****** *)
//
// 9.8: Controlling Host Access
//
(* ****** ****** *)
//
// 9.8.1: Adding, Getting or Removing Hosts
//
(* ****** ****** *)

typedef XHostAddress = $extype_struct "XHostAddress" of {
  family= int
, length= int
, address= string
} // end of [XHostAddress]

fun XAddHost {l:agz}
  (dpy: !Display_ptr l, host: &XHostAddress): void = "mac#atsctrb_XAddHost"
// end of [XAddHost]

fun XAddHosts
  {l:agz} {n:nat} (
  dpy: !Display_ptr l, hosts: &(@[XHostAddress][n]), n: int n
) : void
  = "mac#atsctrb_XAddHosts"
// end of [XAddHosts]

fun XListHosts {l:agz} (
  dpy: !Display_ptr l
, nhost: &int? >> opt (int n, la > null)
, state: &XBool? >> opt (XBool, la > null)
) : #[n:nat;la:addr] XArray (XHostAddress, n, la)
  = "mac#atsctrb_XListHosts"
// end of [XListHosts]

fun XRemoveHost {l:agz}
  (dpy: !Display_ptr l, host: &XHostAddress): void
  = "mac#atsctrb_XRemoveHost"
// end of [XRemoveHost]

fun XRemoveHosts {l:agz} {n:nat}
  (dpy: !Display_ptr l, hosts: &(@[XHostAddress][n]), n: int n): void
  = "mac#atsctrb_XRemoveHosts"
// end of [XRemoveHosts]

(* ****** ****** *)
//
// 9.8.2: Changing, Enabling or Disabling Access Control
//
(* ****** ****** *)

fun XSetAccessControl {l:agz}
  (dpy: !Display_ptr l, mode: int): void = "mac#atsctrb_XSetAccessControl"
// end of [XSetAccessControl]

fun XEnableAccessControl {l:agz}
  (dpy: !Display_ptr l): void = "mac#atsctrb_XEnableAccessControl"
// end of [XEnableAccessControl]

fun XDisableAccessControl {l:agz}
  (dpy: !Display_ptr l): void = "mac#atsctrb_XDisableAccessControl"
// end of [XDisableAccessControl]

(* ****** ****** *)
//
//
// Chapter 10: Events
//
//
(* ****** ****** *)

typedef XEvent =
  $extype_struct "XEvent" of {
  type= EventType_t // the type of the event
, _rest= undefined_t // this abstract field cannot be accessed
} // end of [XEvent]

propdef XEvent_castdn_t (a:t@ype) = {l:addr}
  (XEvent @ l) -<prf> (a @ l, a @ l -<lin,prf> XEvent @ l)
// end of [XEvent_castdn_t]

(* ****** ****** *)
//
// 10.2: Event Structures
//
(* ****** ****** *)

typedef XAnyEvent = $extype_struct "XAnyEvent" of {
  type= EventType_t
, serial = ulint // # of last request processed by server
, send_event= XBool // true if this comes from a SendEvent request
/*
, display= Display_ptr0 // Display the event was read freom
*/
, window= Window
, _rest= undefined_t
} // end of [XAnyEvent]

praxi XEvent_xany_castdn : XEvent_castdn_t (XAnyEvent)

(* ****** ****** *)
//
// 10.5: Keyboard and Pointer Events
//
(* ****** ****** *)

typedef XKeyEvent = $extype_struct "XKeyEvent" of {
  type= EventType_t
, serial= ulint
, send_event= XBool
/*
, display= Display_ptr0 // display from which the event was read
*/
, window= Window
// individual section
, root= Window
, subwindow= Window
/*
, time= Time
*/
, x= int, y= int
, x_root= int, y_root= int
, state= uint
, keycode= uint
, same_screen= XBool  
, _rest= undefined_t
} // end of [XKeyEvent]

praxi XEvent_xkey_castdn : XEvent_castdn_t (XKeyEvent)

//

typedef XKeyPressedEvent = XKeyEvent
typedef XKeyReleasedEvent = XKeyEvent

//

typedef XButtonEvent = $extype_struct "XButtonEvent" of {
  type= EventType_t
, serial= ulint
, send_event= XBool
/*
, display= Display_ptr0 // Display the event was read freom
*/
, window= Window
// individual section
, root= Window
, subwindow= Window
/*
, time= Time
*/
, x= int, y= int
, x_root= int, y_root= int
, state= uint
, button= uint
, same_screen= XBool
, _rest= undefined_t
} // end of [XButtonEvent]

praxi XEvent_xbutton_castdn : XEvent_castdn_t (XButtonEvent)

//

typedef XMotionEvent = $extype_struct "XMotionEvent" of {
  type= EventType_t
, serial= ulint
, send_event= XBool
/*
, display= Display_ptr0 // Display the event was read freom
*/
, window= Window
// individual section
, root= Window
, subwindow= Window
/*
, time= Time
*/
, x= int, y= int
, x_root= int, y_root= int
, state= uint
, in_hint= char
, same_screen= XBool  
, _rest= undefined_t
} // end of [XMotionEvent]

praxi XEvent_xmotion_castdn : XEvent_castdn_t (XMotionEvent)

(* ****** ****** *)
//
// 10.6: Window Entry/Exit Events
//
(* ****** ****** *)
//
// 10.7: Input Focus Events
//
(* ****** ****** *)
//
// 10.8: Key Map State Notification Events
//
(* ****** ****** *)
//
// 10.9: Exposure Events
//
(* ****** ****** *)

typedef XExposeEvent = $extype_struct "XExposeEvent" of {
  type= EventType_t
, serial= ulint
, send_event= XBool
/*
, display= Display_ptr0 // Display the event was read freom
*/
, window= Window
// individual section
, x= int, y= int
, width= int, height= int
, count= int  
} // end of [XExposeEvent]

praxi XEvent_xexpose_castdn : XEvent_castdn_t (XExposeEvent)

(* ****** ****** *)
//
// 10.10: Window State Change Events
//
(* ****** ****** *)
//
// 10.10.1: CirculateNotify Events
//
(* ****** ****** *)

typedef XCirculateEvent =
  $extype_struct "XCirculateEvent" of {
  type= EventType_t
, serial= ulint
, send_event= XBool
/*
, display= Display_ptr0 // Display the event was read freom
*/
// individual section
, event= Window
, window= Window
, place= int  
, _rest= undefined_t
} // end of [XCirculateEvent]

praxi XEvent_xcirculate_castdn : XEvent_castdn_t (XCirculateEvent)

(* ****** ****** *)
//
// 10.10.2: ConfigureNotify Events
//
(* ****** ****** *)

typedef XConfigureEvent =
  $extype_struct "XConfigureEvent" of {
  type= EventType_t
, serial= ulint
, send_event= XBool
/*
, display= Display_ptr0 // Display the event was read freom
*/
// individual section
, event= Window
, window= Window
, x= int, y= int
, width= int, height= int
, border_width= int
, above= Window
, override_redirect= XBool
, _rest= undefined_t
} // end of [XConfigureEvent]

praxi XEvent_xconfigure_castdn : XEvent_castdn_t (XConfigureEvent)

(* ****** ****** *)
//
// 10.10.2: CreateNotify Events
//
(* ****** ****** *)

typedef XCreateWindowEvent =
  $extype_struct "XCreateWindowEvent" of {
  type= EventType_t
, serial= ulint
, send_event= XBool
/*
, display= Display_ptr0 // Display the event was read freom
*/
// individual section
, parent= Window
, window= Window
, x= int, y= int
, width= int, height= int
, border_width= int
, override_redirect= XBool
, _rest= undefined_t
} // end of [XCreateWindowEvent]

praxi XEvent_xcreatewindow_castdn : XEvent_castdn_t (XCreateWindowEvent)

(* ****** ****** *)
//
// 10.10.2: DestroyNotify Events
//
(* ****** ****** *)

typedef XDestroyWindowEvent =
  $extype_struct "XDestroyWindowEvent" of {
  type= EventType_t
, serial= ulint
, send_event= XBool
/*
, display= Display_ptr0 // Display the event was read freom
*/
// individual section
, event= Window
, window= Window
, x= int, y= int
, _rest= undefined_t
} // end of [XDestroyWindowEvent]

praxi XEvent_xdestroywindow_castdn : XEvent_castdn_t (XDestroyWindowEvent)

(* ****** ****** *)
//
//
// Chapter 11: Event Handling Functions
//
//
(* ****** ****** *)
//
// 11.1: selecting events
//
(* ****** ****** *)

fun XSelectInput {l:agz}
  (dpy: !Display_ptr l, win: Window, eventmask: InputEventMask_t): void
  = "mac#atsctrb_XSelectInput"
// end of [XSelectInput]

(* ****** ****** *)
//
// 11.2: handling the output buffer
//
(* ****** ****** *)

fun XFlush {l:agz} (dpy: !Display_ptr l): void
  = "mac#atsctrb_XFlush"
// end of [XFlush]

fun XSync {l:agz} (dpy: !Display_ptr l, discard: bool): void
  = "mac#atsctrb_XSync"
// end of [XSync]

(* ****** ****** *)
//
// 11.3: Event Queue Management
//
macdef QueuedAlready = $extval (int, "QueuedAlready")
macdef QueuedAfterReading = $extval (int, "QueuedAfterReading")
macdef QueuedAfterFlush = $extval (int, "QueuedAfterFlush")

fun XEventsQueued {l:agz} (dpy: Display_ptr l, mode: int): int
  = "mac#atsctrb_XEventsQueued"
// end of [XEventsQueued]

// this one is equivalent to XEventsQueued (QueuedAfterFlush)
fun XPending {l:agz} (dpy: Display_ptr l): int = "mac#atsctrb_XPending"

(* ****** ****** *)
//
// 11.4: manipulating the event queue
//
(* ****** ****** *)
//
// 11.4.1: returning the next event
//
(* ****** ****** *)

fun XNextEvent {l:agz}
  (dpy: !Display_ptr l, event: &XEvent? >> XEvent): void = "mac#atsctrb_XNextEvent"
// end of [XNextEvent]

fun XPeekEvent {l:agz}
  (dpy: !Display_ptr l, event: &XEvent? >> XEvent): void = "mac#atsctrb_XPeekEvent"
// end of [XPeekEvent]

(* ****** ****** *)
//
//
// Chapter 14: Inter-client communication functions
//
// source: "X11/Xutil.h"
//
(* ****** ****** *)
//
// 14.1: Client to Window Manage Communication
//
(* ****** ****** *)

typedef XTextProperty (l:addr) =
  $extype_struct "XTextProperty" of {
  value= ptr l // property data
, encoding= Atom // type of property
, format= int // 8, 16, or 32
, nitems= ulint // number of items in value
} // end of [XTextProperty]

typedef XTextProperty = [l:addr] XTextProperty l

(* ****** ****** *)
//
// HX: this is an enum type
//
abst@ype XICCEncodingStyle = $extype"XICCEncodingStyle"
macdef XStringStyle = $extval (XICCEncodingStyle, "XStringStyle")
macdef XCompoundTextStyle = $extval (XICCEncodingStyle, "XCompoundTextStyle")
macdef XTextStyle = $extval (XICCEncodingStyle, "XTextStyle")
macdef XStdICCTextStyle = $extval (XICCEncodingStyle, "XStdICCTextStyle")

(* ****** ****** *)
//
// 14.1.1: Manipulating Top-Level Windows
//
(* ****** ****** *)

fun XIconifyWindow
  {l:agz} (
  dpy: !Display_ptr l, win: Window, nscr: int
) : Status
  = "mac#atsctrb_XIconifyWindow"
// end of [XIconifyWindow]

fun XWithdrawWindow
  {l:agz} (
  dpy: !Display_ptr l, win: Window, nscr: int
) : Status
  = "mac#atsctrb_XWithdrawWindow"
// end of [XWithdrawWindow]

fun XReconfigureWMWindow {l:agz} (
  dpy: !Display_ptr l
, win: Window, nscr: int, mask: uint, values: &XWindowChanges?
) : Status = "mac#atsctrb_XReconfigureWMWindow"
// end of [XReconfigureWMWindow]

(* ****** ****** *)
//
// 14.1.2: Converting String Lists
//
(* ****** ****** *)

fun XDefaultString (): string = "mac#atsctrb_XDefaultString"

//

fun XStringToTextProperty (
  str: string
, text: &XTextProperty? >> opt (XTextProperty l, i <> 0)
) : #[i:int;l:addr] (XFree_v l | Status i)
  = "atsctrb_XStringToTextProperty"
// end of [XStringToTextProperty]

fun XStringListToTextProperty {n:nat} (
  list: &(@[string][n]), n: int n
, text: &XTextProperty? >> opt (XTextProperty l, i <> 0)
) : #[i:int;l:addr] (XFree_v l | Status i)
  = "mac#atsctrb_XStringListToTextProperty"
// end of [XStringListToTextProperty]

//

fun XTextPropertyToStringList {l:addr} (
  text: &XTextProperty l
, list: &XStrarr? >> opt (XStrarr1 n, i <> 0)
, n: &int? >> opt (int n, i <> 0)
) : #[i:int;n:nat] Status i = "mac#atsctrb_XTextPropertyToStringList"
// end of [XTextPropertyToStringList]

fun XFreeStringList
  {n:nat} {l:addr} (list: XStrarr (n, l)): void = "mac#atsctrb_XFreeStringList"
// end of [XFreeStringList]

(* ****** ****** *)
//
// 14.1.3: Setting and Reading Text Properties
//
(* ****** ****** *)

fun XSetTextProperty {l:agz} (
  dpy: !Display_ptr l
, win: Window
, text: &XTextProperty
, property: Atom
) : void = "mac#atsctrb_XSetTextProperty"
// end of [XSetTextProperty]

fun XGetTextProperty {l:agz} (
  dpy: !Display_ptr l
, win: Window
, text: &XTextProperty? >> opt (XTextProperty, i <> 0)
, property: Atom
) : #[i:int] Status i = "mac#atsctrb_XGetTextProperty"
// end of [XGetTextProperty]

(* ****** ****** *)
//
// 14.1.4: Setting and Reading the WM_NAME Property
//
(* ****** ****** *)

fun XSetWMName {l:agz} (
  dpy: !Display_ptr l
, win: Window
, text: &XTextProperty
) : void = "mac#atsctrb_XSetWMName"
// end of [XSetWMName]

fun XGetWMName {l:agz} (
  dpy: !Display_ptr l
, win: Window
, text: &XTextProperty >> opt (XTextProperty, i <> 0)
) : #[i:int] Status i = "mac#atsctrb_XGetWMName"
// end of [XGetWMName]

fun XStoreName {l:agz} (
  dpy: !Display_ptr l
, win: Window
, window_name: string // the name is copied into ...
) : void = "mac#atsctrb_XStoreName"
// end of [XStoreName]

absviewtype XString
fun XFetchName {l:agz} (
  dpy: !Display_ptr l
, win: Window
, window_name: &XString? >> opt (XString0, i <> 0)
) : #[i:int] Status i
  = "mac#atsctrb_XFetchName"
// end of [XFetchName]

(* ****** ****** *)
//
// 14.1.5: Setting and Reading the WM_ICON_NAME Property
//
(* ****** ****** *)

fun XSetWMIconName {l:agz} (
  dpy: !Display_ptr l
, win: Window
, text: &XTextProperty
) : void = "mac#atsctrb_XSetWMIconName"
// end of [XSetWMIconName]

fun XGetWMIconName {l:agz} (
  dpy: !Display_ptr l
, win: Window
, text: &XTextProperty >> opt (XTextProperty, i <> 0)
) : #[i:int] Status i = "mac#atsctrb_XGetWMIconName"
// end of [XGetWMIconName]

fun XSetIconName {l:agz} (
  dpy: !Display_ptr l
, win: Window
, icon_name: string // copied into ...
) : void = "mac#atsctrb_XSetIconName"
// end of [XSetIconName]

fun XGetIconName {l:agz} (
  dpy: !Display_ptr l
, win: Window
, icon_name: &XString? >> opt (XString0, i <> 0)
) : #[i:int] Status i = "mac#atsctrb_XGetIconName"
// end of [XGetIconName]

(* ****** ****** *)
//
// 14.1.6: Setting and Reading the WM_HINTS Property
//
macdef InputHint = $extval (lint, "InputHint")
macdef StateHint = $extval (lint, "StateHint")
macdef IconPixmapHint = $extval (lint, "IconPixmapHint")
macdef IconWindowHint = $extval (lint, "IconWindowHint")
macdef IconPositionHint = $extval (lint, "IconPositionHint")
macdef IconMaskHint = $extval (lint, "IconMaskHint")
macdef WindowGroupHint = $extval (lint, "WindowGroupHint")
macdef AllHints = $extval (lint, "AllHints")
macdef XUrgencyHint = $extval (lint, "XUrgencyHint")

//

macdef WithdrawnState = $extval (int, "WithdrawnState")
macdef NormalState = $extval (int, "NormalState")
macdef IconicState = $extval (int, "IconicState")

//

typedef XWMHints =
  $extype_struct "XWMHints" of {
  flags= lint // marks which fields in this structure are defined
, input= XBool // does this application rely on the window manager to get keyword input?
, initial_state= int // see below
, icon_pixmap= Pixmap // pixmap to be used as icon
, icon_window= Window // window to be used as icon
, icon_x= int, icon_y= int // initial position of icon
, icon_mask= Pixmap // pixmap to be used as mask for icon_pixmap
, window_group= XID // id of related window group // may be extended in the future
} // end of [XWMHints]

fun XAllocWMHints
  (): XPtr0 (XWMHints) = "mac#atsctrb_XAllocWMHints"
// end of [XAllocWMHints]

fun XSetWNHints {l:agz}
  (dpy: !Display_ptr l, win: Window, wmhints: &XWMHints): void
  = "mac#atsctrb_XSetWNHints"
// end of [XSetWNHints]

fun XGetWNHints {l:agz}
  (dpy: !Display_ptr l, win: Window): XPtr0 (XWMHints)
  = "mac#atsctrb_XGetWNHints"
// end of [XGetWNHints]

(* ****** ****** *)
//
// 14.1.7: Setting and Reading the WM_NORMAL Property
//
(* ****** ****** *)

typedef XSizeHints_aspect =
  $extype_struct "XSizeHints_aspect" of { x= int, y= int }
// end of [XSizeHints_aspect]

macdef USPosition = $extval (lint, "USPosition")
macdef USSize = $extval (lint, "USSize")
macdef PPosition = $extval (lint, "PPosition")
macdef PSize = $extval (lint, "PSize")
macdef PMinSize = $extval (lint, "PMinSize")
macdef PMaxSize = $extval (lint, "PMaxSize")
macdef PResizeInc = $extval (lint, "PResizeInc")
macdef PAspect = $extval (lint, "PAspect")
macdef PBaseSize = $extval (lint, "PBaseSize")
macdef PWinGravity = $extval (lint, "PWinGravity")

typedef XSizeHints =
  $extype_struct "XSizeHints" of {
  flags= lint
, x= int, y= int
, width= int, height= int
, min_width= int, min_height= int
, max_width= int, max_height= int
, width_inc= int, height_inc= int
, min_aspect= XSizeHints_aspect, max_aspect= XSizeHints_aspect
, base_width= int, base_height= int
, win_gravity= int
} // end of [XSizeHints]

fun XAllocSizeHints ()
  : XPtr0 (XSizeHints) = "mac#atsctrb_XAllocSizeHints"
// end of [XAllocSizeHints]

//

fun XSetWMNormalHints {l:agz}
  (dpy: !Display_ptr l, win: Window, hints: &XSizeHints): void
  = "mac#atsctrb_XSetWMNormalHints"
// end of [XSetWMNormalHints]

fun XGetWMNormalHints {l:agz} (
  dpy: !Display_ptr l, win: Window
, hints: &XSizeHints? >> opt (XSizeHints, i <> 0)
, supplied: &lint? >> lint
) : #[i:int] Status i
  = "mac#atsctrb_XGetWMNormalHints"
// end of [XGetWMNormalHints]

//

fun XSetWMSizeHints
  {l:agz} (
  dpy: !Display_ptr l
, win: Window
, hints: &XSizeHints
, property: Atom
) : void
  = "mac#atsctrb_XSetWMSizeHints"
// end of [XSetWMSizeHints]

fun XGetWMSizeHints {l:agz} (
  dpy: !Display_ptr l, win: Window
, hints: &XSizeHints? >> opt (XSizeHints, i <> 0)
, supplied: &lint? >> lint, property: Atom
) : #[i:int] Status i
  = "mac#atsctrb_XGetWMSizeHints"
// end of [XGetWMSizeHints]

(* ****** ****** *)
//
// 14.1.8: Setting and Reading the WM_CLASS Property
//
(* ****** ****** *)

typedef XClassHint = $extype_struct "XClassHint" of {
  res_name= string
, res_class= string
} // end of [XClassHint]

fun XAllocClassHint ()
  : XPtr0 (XClassHint) = "mac#atsctrb_XAllocClassHint"
// end of [XAllocClassHint]

fun XSetClassHint {l:agz}
  (dpy: !Display_ptr l, win: Window, class_hint: XClassHint): void
  = "mac#atsctrb_XSetClassHint"

fun XGetClassHint {l:agz} (
  dpy: !Display_ptr l
, win: Window
, class_hint: &XClassHint? >> opt (XClassHint, i <> 0)
) : #[i:int] Status i
  = "mac#atsctrb_XGetClassHint"

(* ****** ****** *)
//
// 14.1.9: Setting and Reading the WM_TRANSIENT_FOR Property
//
(* ****** ****** *)

fun XSetTransientForHint {l:agz} (
  dpy: !Display_ptr l, win: Window, prop_window: Window
) : void = "mac#atsctrb_XSetTransientForHint"
// end of [XSetTransientForHint]

fun XGetTransientForHint {l:agz} (
  dpy: !Display_ptr l
, win: Window, prop_window: &Window? >> opt (Window, i <> 0)
) : #[i:int] Status i = "mac#atsctrb_XGetTransientForHint"
// end of [XGetTransientForHint]

(* ****** ****** *)
//
// 14.1.13: Using Window Manager Convenience Functions
//
(* ****** ****** *)

fun XSetWMProperties
  {l:agz} {l1,l2:addr} {n:nat} (
  dpy: !Display_ptr l
, win: Window
, win_name: &XTextProperty l1, icon_name: &XTextProperty l2
, argv: &(@[string][n])
, argc: int n
, normal_hints: &XSizeHints // partially init
, wm_hints: &XWMHints // partially init
, class_hint: &XClassHint // partially init
) : void = "mac#atsctrb_XSetWMProperties"
// end of [XSetWMProperties]

(* ****** ****** *)
//
//
// Chapter 16: Application Unitility Functions
//
//
(* ****** ****** *)
//
// 16.9: Manipulating Bitmaps
//
(* ****** ****** *)

fun XCreatePixmapFromBitmapData {l:agz} (
  dpy: !Display_ptr l
, drw: Drawable, data: ptr(*chars*)
, width: uint, height: uint, fg: ulint, bg: ulint, depth: uint
) : Pixmap
  = "mac#atsctrb_XCreatePixmapFromBitmapData"
// end of [XCreatePixmapFromBitmapData]

fun XCreateBitmapFromData {l:agz} (
  dpy: !Display_ptr l
, drw: Drawable, data: ptr(*chars*), width: uint, height: uint
) : Pixmap
  = "mac#atsctrb_XCreateBitmapFromData"
// end of [XCreateBitmapFromData]

(* ****** ****** *)
//
// 16.10: Using the Context Manager
//
(* ****** ****** *)

(* end of [Xlib.sats] *)
