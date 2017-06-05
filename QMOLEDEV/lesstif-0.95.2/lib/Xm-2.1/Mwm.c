/**
 *
 * $Id: Mwm.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
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

static const char rcsid[] = "$Id: Mwm.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/MwmUtil.h>

#include <XmI/DebugUtil.h>


/*
 * XmIsMotifWMRunning checks for a running M*tif Window Manager running on
 * the screen where the shell given resides. This function DOES NOT cache
 * its result (M*tif compliant).
 * BTW - the shell parameter need not to be a real shell, any widget or
 * gadget is sufficient.
 */
Boolean
XmIsMotifWMRunning(Widget shell)
{
    Atom MotifWMInfo;
    Atom ActualType;
    int ActualFormat;
    unsigned long nItems;
    unsigned long BytesAfter;
    Window *PropData, MWMWindow, Root, Parent, *Kids;
    unsigned int num_Kids, i;
    Boolean Found = False;

    MotifWMInfo = XmInternAtom(XtDisplayOfObject(shell),
			       _XA_MOTIF_WM_INFO, False);
    if (XGetWindowProperty(XtDisplayOfObject(shell),
			   RootWindowOfScreen(XtScreen(shell)),
			   MotifWMInfo,
			   0, PROP_MOTIF_WM_INFO_ELEMENTS,
			   False,
			   MotifWMInfo,
			   &ActualType, &ActualFormat,
			   &nItems, &BytesAfter,
			   (unsigned char **)&PropData) != Success)
    {
	return False;
    }
    /*
     * First check that the data returned is actually the format we
     * requested.
     */
    if ((ActualType == MotifWMInfo) &&
	(ActualFormat == 32) &&
	(nItems >= 2))
    {
	/*
	 * Okay, the property seems ok. Then check for the presence of
	 * the specified window (id stored in the property) as a child
	 * of the root window.
	 */
	MWMWindow = PropData[1];
	if (XQueryTree(XtDisplayOfObject(shell),
		       RootWindowOfScreen(XtScreen(shell)),
		       &Root, &Parent, &Kids, &num_Kids) != None)
	{
	    if (MWMWindow == Root)
	    {
		Found = True;
	    }
	    else
	    {
		for (i = 0; i < num_Kids; i++)
		{
		    if (Kids[i] == MWMWindow)
		    {
			Found = True;
			break;
		    }
		}
	    }
	    if (Kids != NULL)
	    {
		XFree(Kids);
	    }
	}
    }
    if (PropData != NULL)
    {
	XFree(PropData);
    }
    return Found;
}
