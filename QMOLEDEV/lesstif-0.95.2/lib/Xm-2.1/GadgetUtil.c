/**
 *
 * $Id: GadgetUtil.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
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

static const char rcsid[] = "$Id: GadgetUtil.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <XmI/MacrosI.h>
#include <Xm/XmP.h>
#include <Xm/DropSMgr.h>

#include <XmI/DebugUtil.h>


XmGadget
_XmInputInGadget(Widget cw,
		 int x,
		 int y)
{
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, cw, "_XmInputInGadget(%d,%d)\n", x, y));

    for (i = 0; i < MGR_NumChildren(cw); i++)
    {
	Widget g = MGR_Children(cw)[i];

	if (!XmIsGadget(g) || !XtIsManaged(g))
	{
	    continue;
	}

	if ((x >= XtX(g) && x < XtX(g) + XtWidth(g)) &&
	    (y >= XtY(g) && y < XtY(g) + XtHeight(g)))
	{
	    return (XmGadget)g;
	}
    }

    return NULL;
}

/*
#if XmVERSION > 1
XmGadget
XmObjectAtPoint(Widget cw, int x, int y)
{
	return _XmInputInGadget(cw, x, y);
}
#endif
*/

/*
 * this function seems to have the same purpose as the one above...
 * maybe they really _do_ do the same thing...
 *
 * MLM: Oho, those CSF boys must think they're extremely clever.  A little
 * birdy that sometimes sends me email said that one of LessTif's blind spots
 * was the XtNsensitive resource, and suggested that I might find calling
 * InputFor and InputIn on an insensitive gadget "enlightening", when button
 * events occur.  Now all I gotta do is find out if all events are treated
 * this way -- see testXm/pushbg/test7.c for details.  Gotta check this
 * with KeyPresses too.
 */
XmGadget
_XmInputForGadget(Widget cw,
		  int x,
		  int y)
{
    XmGadget g = _XmInputInGadget(cw, x, y);

    if (!g || !XtIsSensitive((Widget)g))
    {
	return NULL;
    }
    else
    {
	return g;
    }
}

void
_XmConfigureObject(Widget g,
		   Position x,
		   Position y,
		   Dimension width,
		   Dimension height,
		   Dimension border_width)
{
    DEBUGOUT(_LtDebug(__FILE__, g,
		      "_XmConfigureObject X %d Y %d W %d H %d BW %d\n",
		      x, y, width, height, border_width));

    XmDropSiteStartUpdate(g);

    if (XmIsGadget(g))
    {
	/* only call the resize if the resize method is there
	 * and something relevant has changed */
	if (XtWidth(g) != width || XtHeight(g) != height ||
	    XtX(g) != x || XtY(g) != y)
	{

	    /* stupid, stupid, stupid.  THIS was why BulletinBoard had the
	     * double expose.  Gadgets aren't smart enough not to piddle
	     * on the floor:
	     *  o GeoUtils would flash-expose the BB subclass (when doing an
	     *    _XmConfigureObject on a Core subclass), which caused
	     *  o Gadgets to expose (via _XmRedisplayGadgets); later,
	     *  o the GeoUtils would _XmConfigure the Gadgets, exposing
	     *    in there new locations.
	     * Without doing the ClearArea below, the Gadgets expose would
	     * splatter the BB.  Lovely stuff.  -- MLM
	     * Could this use ClearRectObjAreas? (GeoUtils.c)
	     */
	    if (XtIsRealized(g) && XtIsManaged(g))
	    {
		XClearArea(XtDisplay(g), XtWindow(g),
			   XtX(g), XtY(g), XtWidth(g), XtHeight(g), True);
	    }

	    XtX(g) = x;
	    XtY(g) = y;
	    XtWidth(g) = width;
	    XtHeight(g) = height;

	    if (XtClass(g)->core_class.resize)
	    {
		(XtClass(g)->core_class.resize) (g);
	    }

	    /* see above: this forces the Gadget redraw by exposing their
	     * region in the BB */
	    if (XtIsRealized(g) && XtIsManaged(g))
	    {
		XClearArea(XtDisplay(g), XtWindow(g),
			   XtX(g), XtY(g), XtWidth(g), XtHeight(g), True);
	    }
	}
    }
    else
    {
	if (XtIsRealized(g))
	{
	    if (width == 0)
	    {
		width = 1;
	    }
	    if (height == 0)
	    {
		height = 1;
	    }
	}

	XtConfigureWidget(g, x, y, width, height, border_width);
    }

    XmDropSiteEndUpdate(g);
}

void
_XmResizeObject(Widget g,
		Dimension width,
		Dimension height,
		Dimension border_width)
{
    DEBUGOUT(_LtDebug(__FILE__, g,
		      "_XmResizeObject W %d H %d\n", width, height));

    XmDropSiteStartUpdate(g);

    if (XmIsGadget(g))
    {
	_XmConfigureObject(g, XtX(g), XtY(g), XtWidth(g), XtHeight(g), 0);
    }
    else
    {
	XtResizeWidget(g, width, height, border_width);
    }

    XmDropSiteEndUpdate(g);
}

void
_XmMoveObject(Widget g,
	      Position x,
	      Position y)
{
    DEBUGOUT(_LtDebug(__FILE__, g,
		      "_XmMoveObject X %d Y %d\n", x, y));

    XmDropSiteStartUpdate(g);

    if (XmIsGadget(g))
    {
	_XmConfigureObject((Widget)g, x, y, XtWidth(g), XtHeight(g), 0);
    }
    else
    {
	XtMoveWidget(g, x, y);
    }

    XmDropSiteEndUpdate(g);
}

void
_XmRedisplayGadgets(Widget w,
		    XEvent *event,
		    Region region)
{
    XExposeEvent *ev = (XExposeEvent *)event;
    Cardinal i;

    for (i = 0; i < MGR_NumChildren(w); i++)
    {
	Widget child = MGR_Children(w)[i];

	if (XtIsManaged(child) && XmIsGadget(child))
	{
	    if (region && XRectInRegion(region, XtX(child), XtY(child),
					XtWidth(child), XtHeight(child)))
	    {
		(*XtClass(child)->core_class.expose) (child, event, region);
	    }
	    else if (ev &&
		     XtX(child) < ev->x + ev->width &&
		     XtX(child) + XtWidth(child) > ev->x &&
		     XtY(child) < ev->y + ev->height &&
		     XtY(child) + XtHeight(child) > ev->y)
	    {
		(*XtClass(child)->core_class.expose) (child, event, region);
	    }
	    else
	    {
		(*XtClass(child)->core_class.expose) (child, event, region);
	    }
	}
    }
}

extern void
_XmDispatchGadgetInput(Widget w,
		       XEvent *event,
		       Mask mask)
{
    XmGadget g = (XmGadget)w;
    XmGadgetClass gc = (XmGadgetClass)w->core.widget_class;

    if (w->core.being_destroyed) return;
    DEBUGOUT(_LtDebug(__FILE__, w,
	  "_XmDispatchGadgetInput: mask 0x%X, FocusIn %s, Parent %s Manager\n",
		      mask,
		      (XmFOCUS_IN_EVENT & mask) ? "True" : "False",
		      XmIsManager(XtParent(w)) ? "is" : "is no"));

    if (XmIsGadget(w) && (G_EventMask(g) & mask))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmDispatchGadgetInput() dispatching..\n"));

	(*gc->gadget_class.input_dispatch) (w, event, mask);
    }

    if ((mask & XmFOCUS_IN_EVENT) && XmIsManager(XtParent(w)))
    {
	MGR_HighlightedWidget(XtParent(w)) = w;
    }

    if ((mask & XmFOCUS_OUT_EVENT) && XmIsManager(XtParent(w)))
    {
	MGR_HighlightedWidget(XtParent(w)) = NULL;
    }
}

extern Time
__XmGetDefaultTime(Widget w,
		   XEvent *event)
{
    return 0;
}
