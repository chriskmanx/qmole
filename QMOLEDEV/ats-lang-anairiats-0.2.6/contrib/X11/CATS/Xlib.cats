/***********************************************************************/
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/***********************************************************************/

/*
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
*/

/* ****** ****** */

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: January, 2010

/* ****** ****** */

#ifndef ATSCTRB_XLIB_XLIB_CATS
#define ATSCTRB_XLIB_XLIB_CATS

/* ****** ****** */

#include "X11/Xlib.h"
#include "X11/Xutil.h"

/* ****** ****** */

//
// Chapter 2: Display Functions
//

/* ****** ****** */

#define atsctrb_XOpenDisplay XOpenDisplay

/* ****** ****** */

#define atsctrb_XAllPlanes XAllPlanes
#define atsctrb_XBlackPixel XBlackPixel
#define atsctrb_XWhitePixel XWhitePixel
#define atsctrb_XConnectionNumber XConnectionNumber
#define atsctrb_XDefaultColormap XDefaultColormap
#define atsctrb_XDefaultDepth XDefaultDepth

#define atsctrb_XListDepths XListDepths // this is a function

#define atsctrb_XDefaultGC XDefaultGC
#define atsctrb_XDefaultRootWindow XDefaultRootWindow
#define atsctrb_XDefaultScreenOfDisplay XDefaultScreenOfDisplay
#define atsctrb_XScreenOfDisplay XScreenOfDisplay
#define atsctrb_XDefaultScreen XDefaultScreen
#define atsctrb_XDefaultVisual XDefaultVisual

#define atsctrb_XDisplayCells XDisplayCells
#define atsctrb_XDisplayPlanes XDisplayPlanes
#define atsctrb_XDisplayString XDisplayString

#define atsctrb_XMaxRequestSize XMaxRequestSize
#define atsctrb_XLastKnownRequestProcessed XLastKnownRequestProcessed
#define atsctrb_XNextRequest XNextRequest

#define atsctrb_XProtocolVersion XProtocolVersion
#define atsctrb_XProtocolRevision XProtocolRevision

#define atsctrb_XQLength XQLength

#define atsctrb_XRootWindow XRootWindow
#define atsctrb_XScreenCount XScreenCount
#define atsctrb_XServerVendor XServerVendor
#define atsctrb_XVendorRelease XVendorRelease

/* ****** ****** */

#define atsctrb_XListPixmapFormats XListPixmapFormats
#define atsctrb_XImageByteOrder XImageByteOrder
#define atsctrb_XBitmapUnit XBitmapUnit
#define atsctrb_XBitmapOrder XBitmapOrder
#define atsctrb_XBitmapPad XBitmapPad

#define atsctrb_XDisplayHeight DisplayHeight
#define atsctrb_XDisplayHeightMM DisplayHeightMM
#define atsctrb_XDisplayWidth DisplayWidth
#define atsctrb_XDisplayWidthMM DisplayWidthMM

/* ****** ****** */

#define atsctrb_XNoOp XNoOp

/* ****** ****** */

#define atsctrb_XFree XFree

/* ****** ****** */

#define atsctrb_XCloseDisplay XCloseDisplay

/* ****** ****** */

//
// Chapter 3: Window Functions
//

/* ****** ****** */

#define atsctrb_XVisualIDFromVisual XVisualIDFromVisual
#define atsctrb_XCreateWindow XCreateWindow
#define atsctrb_XCreateSimpleWindow XCreateSimpleWindow
#define atsctrb_XDestroyWindow XDestroyWindow
#define atsctrb_XDestroyWindow XDestroyWindow

#define atsctrb_XMapWindow XMapWindow
#define atsctrb_XMapRaised XMapRaised
#define atsctrb_XMapSubwindows XMapSubwindows

#define atsctrb_XUnmapWindow XUnmapWindow
#define atsctrb_XUnmapSubwindows XUnmapSubwindows

#define atsctrb_XConfigureWindow XConfigureWindow
#define atsctrb_XMoveWindow XMoveWindow
#define atsctrb_XResizeWindow XResizeWindow
#define atsctrb_XMoveResizeWindow XMoveResizeWindow
#define atsctrb_XSetWindowBorderWidth XSetWindowBorderWidth

#define atsctrb_XRaiseWindow XRaiseWindow
#define atsctrb_XLowerWindow XLowerWindow
#define atsctrb_XCirculateSubwindows XCirculateSubwindows
#define atsctrb_XCirculateSubwindowsUp XCirculateSubwindowsUp
#define atsctrb_XCirculateSubwindowsDown XCirculateSubwindowsDown
#define atsctrb_XRestackWindows XRestackWindows

#define atsctrb_XChangeWindowAttributes XChangeWindowAttributes
#define atsctrb_XSetWindowBackground XSetWindowBackground
#define atsctrb_XSetWindowBackgroundPixmap XSetWindowBackgroundPixmap
#define atsctrb_XSetWindowBorder XSetWindowBorder
#define atsctrb_XSetWindowBorderPixmap XSetWindowBorderPixmap
#define atsctrb_XSetWindowColormap XSetWindowColormap

#define atsctrb_XDefineCursor XDefineCursor
#define atsctrb_XUndefineCursor XUndefineCursor

/* ****** ****** */

//
// Chapter 4: Window Information Functions
//

/* ****** ****** */

#define atsctrb_XQueryTree XQueryTree
#define atsctrb_XGetWindowAttributes XGetWindowAttributes
#define atsctrb_XGetWindowAttributes XGetWindowAttributes

/* ****** ****** */

//
// Chapter 5: Creating and Freeing Pixmaps
//

/* ****** ****** */

#define atsctrb_XCreatePixmap XCreatePixmap
#define atsctrb_XFreePixmap XFreePixmap

#define atsctrb_XCreateFontCursor XCreateFontCursor
#define atsctrb_XFreeCursor XFreeCursor

/* ****** ****** */

//
// Chapter 6: Color Management Functions
//

/* ****** ****** */

#define atsctrb_XCreateColormap XCreateColormap
#define atsctrb_XCopyColormapAndFree XCopyColormapAndFree
#define atsctrb_XFreeColormap XFreeColormap
#define atsctrb_XLookupColor XLookupColor
#define atsctrb_XParseColor XParseColor
#define atsctrb_XAllocColor XAllocColor
#define atsctrb_XAllocNamedColor XAllocNamedColor
#define atsctrb_XStoreColor XStoreColor
#define atsctrb_XStoreColors XStoreColors
#define atsctrb_XStoreNamedColor XStoreNamedColor
#define atsctrb_XQueryColor XQueryColor
#define atsctrb_XQueryColors XQueryColors

/* ****** ****** */

//
// Chapter 7: Graphics Context Functions
//

/* ****** ****** */

#define atsctrb_XCreateGC XCreateGC
#define atsctrb_XCopyGC XCopyGC
#define atsctrb_XChangeGC XChangeGC
#define atsctrb_XGetGCValues XGetGCValues
#define atsctrb_XFreeGC XFreeGC
#define atsctrb_XFlushGC XFlushGC

#define atsctrb_XSetForeground XSetForeground
#define atsctrb_XSetBackground XSetBackground
#define atsctrb_XSetFunction XSetFunction
#define atsctrb_XSetPlaneMask XSetPlaneMask
#define atsctrb_XSetFont XSetFont


#define atsctrb_XSetLineAttributes XSetLineAttributes
#define atsctrb_XSetDashes XSetDashes

/* ****** ****** */

//
// Chapter 8: Graphics Functions
//

/* ****** ****** */

#define atsctrb_XClearArea XClearArea
#define atsctrb_XClearWindow XClearWindow
#define atsctrb_XCopyArea XCopyArea
#define atsctrb_XCopyPlane XCopyPlane

#define atsctrb_XDrawPoint XDrawPoint
#define atsctrb_XDrawPoints XDrawPoints
#define atsctrb_XDrawLine XDrawLine
#define atsctrb_XDrawLines XDrawLines
#define atsctrb_XDrawSegments XDrawSegments
#define atsctrb_XDrawRectangle XDrawRectangle
#define atsctrb_XDrawRectangles XDrawRectangles
#define atsctrb_XDrawArc XDrawArc
#define atsctrb_XDrawArcs XDrawArcs

#define atsctrb_XFillRectangle XFillRectangle
#define atsctrb_XFillRectangles XFillRectangles
#define atsctrb_XFillPolygon XFillPolygon
#define atsctrb_XFillArc XFillArc
#define atsctrb_XFillArcs XFillArcs

#define atsctrb_XLoadFont XLoadFont
#define atsctrb_XQueryFont XQueryFont
#define atsctrb_XLoadQueryFont XLoadQueryFont
#define atsctrb_XFreeFont XFreeFont

#define atsctrb_XTextWidth XTextWidth
#define atsctrb_XTextWidth16 XTextWidth16

#define atsctrb_XDrawString XDrawString
#define atsctrb_XDrawString16 XDrawString16
#define atsctrb_XDrawImageString XDrawImageString
#define atsctrb_XDrawImageString16 XDrawImageString16

/* ****** ****** */

//
// Chapter 9: Window and Session Manager Functions
//

/* ****** ****** */

#define atsctrb_XReparentWindow XReparentWindow
#define atsctrb_XChangeSaveSet XChangeSaveSet
#define atsctrb_XAddSaveSet XAddSaveSet
#define atsctrb_XRemoveFromSaveSet XRemoveFromSaveSet
#define atsctrb_XInstallColormap XInstallColormap
#define atsctrb_XUninstallColormap XUninstallColormap
#define atsctrb_XListInstalledColormaps XListInstalledColormaps
#define atsctrb_XGrabServer XGrabServer
#define atsctrb_XUngrabServer XUngrabServer
#define atsctrb_XKillClient XKillClient
#define atsctrb_XSetScreenSaver XSetScreenSaver
#define atsctrb_XForceScreenSaver XForceScreenSaver
#define atsctrb_XActivateScreenSaver XActivateScreenSaver
#define atsctrb_XResetScreenSaver XResetScreenSaver
#define atsctrb_XGetScreenSaver XGetScreenSaver
#define atsctrb_XAddHost XAddHost
#define atsctrb_XAddHosts XAddHosts
#define atsctrb_XListHosts XListHosts
#define atsctrb_XRemoveHost XRemoveHost
#define atsctrb_XRemoveHosts XRemoveHosts
#define atsctrb_XSetAccessControl XSetAccessControl
#define atsctrb_XEnableAccessControl XEnableAccessControl
#define atsctrb_XDisableAccessControl XDisableAccessControl

/* ****** ****** */

//
// Chapter 11: Event Handling Functions
//

/* ****** ****** */

#define atsctrb_XSelectInput XSelectInput
#define atsctrb_XFlush XFlush
#define atsctrb_XSync XSync

/* ****** ****** */

//
// Chapter 14: Inter-client communication functions
//

/* ****** ****** */

#define atsctrb_XIconifyWindow XIconifyWindow
#define atsctrb_XDestroyWindow XDestroyWindow
#define atsctrb_XReconfigureWMWindow XReconfigureWMWindow

#define atsctrb_XDefaultString XDefaultString

//

static inline
Status
atsctrb_XStringToTextProperty (
  ats_ptr_type str, ats_ref_type text
) { return
  XStringListToTextProperty((char**)&str, 1, (XTextProperty*)text) ;
} // end of [XStringToTextProperty]

#define atsctrb_XStringListToTextProperty XStringListToTextProperty

//

#define atsctrb_XTextPropertyToStringList XTextPropertyToStringList
#define atsctrb_XFreeStringList XFreeStringList

//

#define atsctrb_XSetTextProperty XSetTextProperty
#define atsctrb_XGetTextProperty XGetTextProperty

#define atsctrb_XSetWMName XSetWMName
#define atsctrb_XGetWMName XGetWMName
#define atsctrb_XStoreName XStoreName
#define atsctrb_XFetchName XFetchName

#define atsctrb_XSetWMIconName XSetWMIconName
#define atsctrb_XGetWMIconName XGetWMIconName
#define atsctrb_XSetIconName XSetIconName
#define atsctrb_XGetIconName XGetIconName

#define atsctrb_XSetTransientForHint XSetTransientForHint
#define atsctrb_XGetTransientForHint XGetTransientForHint

//

#define atsctrb_XAllocWMHints XAllocWMHints
#define atsctrb_XAllocSizeHints XAllocSizeHints
#define atsctrb_XAllocClassHint XAllocClassHint

#define atsctrb_XEventsQueued XEventsQueued
#define atsctrb_XPending XPending

#define atsctrb_XNextEvent XNextEvent
#define atsctrb_XPeekEvent XPeekEvent

//

#define atsctrb_XSetWMProperties XSetWMProperties

/* ****** ****** */

//
// Chapter 16: Application Unitility Functions
//

/* ****** ****** */

#define atsctrb_XCreatePixmapFromBitmapData XCreatePixmapFromBitmapData
#define atsctrb_XCreateBitmapFromData XCreateBitmapFromData

/* ****** ****** */

#endif // end of [ATSCTRB_XLIB_XLIB_CATS]

/* end of [Xlib.cats] */
