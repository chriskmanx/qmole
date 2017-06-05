/**
 *
 * $Id: Dest.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
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

static const char rcsid[] = "$Id: Dest.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <XmI/MacrosI.h>

#include <Xm/XmP.h>
#include <Xm/DisplayP.h>

#include <XmI/DebugUtil.h>


void
_XmSetDestination(Display *dpy, Widget w)
{
    Widget disp = XmGetXmDisplay(dpy);

    ((XmDisplayInfo *)Display_DisplayInfo(disp))->destinationWidget = w;
}


Widget
XmGetDestination(Display *display)
{
    Widget disp = XmGetXmDisplay(display);

    return ((XmDisplayInfo *)Display_DisplayInfo(disp))->destinationWidget;
}
