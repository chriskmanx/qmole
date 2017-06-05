/**
 *
 * $Id: UniqueEvent.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
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

static const char rcsid[] = "$Id: UniqueEvent.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <XmI/XmI.h>
#include <XmI/MacrosI.h>

#include <Xm/XmP.h>
#include <Xm/DisplayP.h>

#include <XmI/DebugUtil.h>


void
_XmRecordEvent(XEvent *event)
{
    /* FIX ME: do something compatible here */
    Widget disp;
    XtPointer *p;

    if (!event || !event->xany.display)
    {
	return;
    }

    disp = XmGetXmDisplay(event->xany.display);

    if (!disp)
    {
	return;
    }

    p = &(((XmDisplayInfo *) Display_DisplayInfo(disp))->UniqueStamp);

    switch (event->xany.type)
    {
    case ButtonPress:
    case ButtonRelease:

	*p = XtRealloc(*p, sizeof(XButtonEvent));
	memcpy(*p, event, sizeof(XButtonEvent));

	break;
    case KeyPress:
    case KeyRelease:

	*p = XtRealloc(*p, sizeof(XKeyEvent));
	memcpy(*p, event, sizeof(XKeyEvent));

	break;
    default:
	/* FIX ME: which eventtypes does Xt redispatch ? */
	break;
    }
}

Boolean
_XmIsEventUnique(XEvent *event)
{
    /* FIX ME: compatible? */
    Widget disp;
    XtPointer rec_event;
    Boolean res;

    disp = XmGetXmDisplay(event->xany.display);
    rec_event = ((XmDisplayInfo *) Display_DisplayInfo(disp))->UniqueStamp;

    if (rec_event == NULL)
    {
	return True;
    }

    switch (event->xany.type)
    {
    case ButtonPress:
    case ButtonRelease:
	{
	    XButtonEvent *rev = (XButtonEvent *)rec_event;
	    XButtonEvent *ev = (XButtonEvent *)event;

	    res = rev->time != ev->time ||
		rev->window != ev->window ||
		rev->type != ev->type ||
		rev->display != ev->display;
	}
	break;
    case KeyPress:
    case KeyRelease:
	{
	    XKeyEvent *rev = (XKeyEvent *)rec_event;
	    XKeyEvent *ev = (XKeyEvent *)event;

	    res = rev->time != ev->time ||
		rev->window != ev->window ||
		rev->type != ev->type ||
		rev->display != ev->display;
	}
	break;
    default:

	_XmError(NULL, "_XmIsEventUnique got event of wrong type.");
	res = True;
	break;
    }

    return res;
}
