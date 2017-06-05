/**
 *
 * $Id: defaults.c,v 1.1 2004/08/28 19:25:45 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#include <LTconfig.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

#include <XmI/XmI.h>

#include "mwm.h"


/*
 * This stuff will get used as I write stuff
 *
 Icons
 "Pack Icons" _P  Shift Alt<Key>F7 f.pack_icons
 XBMLANGPATH
 * function names came from running strings against mwm on Solaris
 */
void *working_base = NULL;
ScreenInfo *rscr = NULL;

void
_WmMultiClickTimeDefault(Widget widget, int offset, XrmValue *val)
{
    static int time;

    time = XtGetMultiClickTime(dpy);
    val->addr = (XPointer)&time;
}

void
_WmDefaultBorderWidth(Widget widget, int offset, XrmValue *val)
{
    static Dimension border_width;

    border_width = 5;
    val->addr = (XPointer)&border_width;
}

void
_WmDefaultResizeBorderWidth(Widget widget, int offset, XrmValue *val)
{
    static Dimension border_width;

    border_width = rscr->frame_border_width + 3;
    val->addr = (XPointer)&border_width;
}

void
_WmFocusAutoRaiseDefault(Widget widget, int offset, XrmValue *val)
{
    static Boolean raise;

    if (Mwm.keyboard_focus_policy == XmEXPLICIT)
	raise = True;
    else if (Mwm.keyboard_focus_policy == XmPOINTER)
	raise = False;
    else
    {
	_XmWarning(toplevel, "Keyboard Focus Policy is unknown.\n");
	raise = True;
    }
    val->addr = (XPointer)&raise;
}

void
_WmIconImageBDefault(Widget widget, int offset, XrmValue *val)
{
    val->addr = (XPointer)&rscr->components[MWM_ICON].background;
}

void
_WmIconImageBSCDefault(Widget widget, int offset, XrmValue *val)
{
    val->addr = (XPointer)&rscr->components[MWM_ICON].bottom_shadow_color;
}

void
_WmIconImageBSPDefault(Widget widget, int offset, XrmValue *val)
{
    val->addr = (XPointer)&rscr->components[MWM_ICON].bottom_shadow_pixmap;
}

void
_WmIconImageFDefault(Widget widget, int offset, XrmValue *val)
{
    val->addr = (XPointer)&rscr->components[MWM_ICON].foreground;
}

void
_WmIconImageTSCDefault(Widget widget, int offset, XrmValue *val)
{
    val->addr = (XPointer)&rscr->components[MWM_ICON].top_shadow_color;
}

void
_WmIconImageTSPDefault(Widget widget, int offset, XrmValue *val)
{
    val->addr = (XPointer)&rscr->components[MWM_ICON].top_shadow_pixmap;
}

void
_WmMatteBDefault(Widget widget, int offset, XrmValue *val)
{
    _XmAddBackgroundToColorCache(ScreenOfDisplay( dpy, rscr->screen),
				 DefaultColormap(dpy, rscr->screen),
				 "LightGrey",
				 0xa8a8a8,
				 val);
}

void
_WmMatteBSCDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixel bottom_shadow;

    val->addr = (XPointer)&bottom_shadow;
    val->size = sizeof bottom_shadow;
    bottom_shadow = _XmAccessColorData(
	_XmGetColors(ScreenOfDisplay( dpy, rscr->screen),
		     DefaultColormap(dpy, rscr->screen),
		     ((MwmWindow *)working_base)->matte_background),
	XmBOTTOM_SHADOW);
}

void
_WmMatteBSPDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XPointer)&pix;
}

void
_WmMatteFDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixel foreground;

    val->addr = (XPointer)&foreground;
    val->size = sizeof foreground;
    foreground = _XmAccessColorData(
	_XmGetColors(ScreenOfDisplay( dpy, rscr->screen),
		     DefaultColormap(dpy, rscr->screen),
		     ((MwmWindow *)working_base)->matte_background),
	XmFOREGROUND);
}

void
_WmMatteTSCDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixel top_shadow;

    val->addr = (XPointer)&top_shadow;
    val->size = sizeof top_shadow;
    top_shadow = _XmAccessColorData(
	_XmGetColors(ScreenOfDisplay( dpy, rscr->screen),
		     DefaultColormap(dpy, rscr->screen),
		     ((MwmWindow *)working_base)->matte_background),
	XmTOP_SHADOW);
}

void
_WmMatteTSPDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XPointer)&pix;
}

void
_WmBackgroundDefault(Widget widget, int offset, XrmValue *val)
{
    Screen *temp_screen;
    Colormap temp_cmap;

    switch (((ComponentInfo *)working_base)->type)
    {
    case MWM_MENU:
	/* This isn't a real widget, but it needs to act like one
	 * for this function call.
	 */

	temp_screen = CoreScreen(widget);
	temp_cmap = CoreColormap(widget);
	CoreScreen(widget) = ScreenOfDisplay(dpy, rscr->screen);
	CoreColormap(widget) = DefaultColormap(dpy, rscr->screen);
	_XmBackgroundColorDefault(widget, offset, val);
	CoreScreen(widget) = temp_screen;
	CoreColormap(widget) = temp_cmap;
	break;
    case MWM_FEEDBACK:
	_XmAddBackgroundToColorCache(ScreenOfDisplay( dpy, rscr->screen),
				     DefaultColormap(dpy, rscr->screen),
				     "CadetBlue",
				     0x5f929e,
				     val);
	break;
    default:
	_XmAddBackgroundToColorCache(ScreenOfDisplay( dpy, rscr->screen),
				     DefaultColormap(dpy, rscr->screen),
				     "LightGrey",
				     0xa8a8a8,
				     val);
    }
}

void
_WmBackgroundPixmapDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XPointer)&pix;
    val->size = sizeof pix;

    if (DefaultDepthOfScreen(ScreenOfDisplay( dpy, rscr->screen)) == 1)
    {
	pix = XmGetPixmapByDepth(ScreenOfDisplay( dpy, rscr->screen),
				 "25_foreground",
				 ((ComponentInfo *)working_base)->foreground,
				 ((ComponentInfo *)working_base)->background,
				 1);
    }
}

void
_WmBottomShadowColorDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixel bottom_shadow;

    val->addr = (XPointer)&bottom_shadow;
    val->size = sizeof bottom_shadow;
    bottom_shadow = _XmAccessColorData(
	_XmGetColors(ScreenOfDisplay( dpy, rscr->screen),
		     DefaultColormap(dpy, rscr->screen),
		     ((ComponentInfo *)working_base)->background),
	XmBOTTOM_SHADOW);
}

void
_WmBottomShadowPixmapDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XPointer)&pix;
}

void
_WmForegroundDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixel foreground;

    val->addr = (XPointer)&foreground;
    val->size = sizeof foreground;
    foreground = _XmAccessColorData(
	_XmGetColors(ScreenOfDisplay( dpy, rscr->screen),
		     DefaultColormap(dpy, rscr->screen),
		     ((ComponentInfo *)working_base)->background),
	XmFOREGROUND);
}

void
_WmTopShadowColorDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixel top_shadow;

    val->addr = (XPointer)&top_shadow;
    val->size = sizeof top_shadow;
    top_shadow = _XmAccessColorData(
	_XmGetColors(ScreenOfDisplay( dpy, rscr->screen),
		     DefaultColormap(dpy, rscr->screen),
		     ((ComponentInfo *)working_base)->background),
	XmTOP_SHADOW);
}

void
_WmTopShadowPixmapDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XPointer)&pix;
}

void
_WmABackgroundDefault(Widget widget, int offset, XrmValue *val)
{
    _XmAddBackgroundToColorCache(ScreenOfDisplay( dpy, rscr->screen),
				 DefaultColormap(dpy, rscr->screen),
				 "CadetBlue",
				 0x5f929e,
				 val);
}

void
_WmAForegroundDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixel foreground;

    val->addr = (XPointer)&foreground;
    val->size = sizeof foreground;
    foreground = _XmAccessColorData(
	_XmGetColors(ScreenOfDisplay( dpy, rscr->screen),
		     DefaultColormap(dpy, rscr->screen),
		     ((ComponentInfo *)working_base)->active_background),
	XmFOREGROUND);
}

void
_WmABottomShadowColorDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixel bottom_shadow;

    val->addr = (XPointer)&bottom_shadow;
    val->size = sizeof bottom_shadow;
    bottom_shadow = _XmAccessColorData(
	_XmGetColors(ScreenOfDisplay( dpy, rscr->screen),
		     DefaultColormap(dpy, rscr->screen),
		     ((ComponentInfo *)working_base)->active_background),
	XmBOTTOM_SHADOW);
}

void
_WmATopShadowColorDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixel top_shadow;

    val->addr = (XPointer)&top_shadow;
    val->size = sizeof top_shadow;
    top_shadow = _XmAccessColorData(
	_XmGetColors(ScreenOfDisplay( dpy, rscr->screen),
		     DefaultColormap(dpy, rscr->screen),
		     ((ComponentInfo *)working_base)->active_background),
	XmTOP_SHADOW);
}

void
_WmABackgroundPixmapDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XPointer)&pix;
    val->size = sizeof pix;

    if (DefaultDepthOfScreen(ScreenOfDisplay( dpy, rscr->screen)) == 1)
    {
	pix = XmGetPixmapByDepth(ScreenOfDisplay( dpy, rscr->screen),
				 "50_foreground",
				 ((ComponentInfo *)working_base)->foreground,
				 ((ComponentInfo *)working_base)->background,
				 1);
    }
}

void
_WmATopShadowPixmapDefault(Widget widget, int offset, XrmValue *val)
{
    static Pixmap pix = XmUNSPECIFIED_PIXMAP;

    val->addr = (XPointer)&pix;
}
