/**
 *
 * $Header: /cvsroot/lesstif/lesstif/test/common/XdbPrintTree.c,v 1.5 2002/05/01 16:01:26 amai Exp $
 * 
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1996-2002 LessTif Development Team
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


#include "LTTconfig.h"
 
#include <stdio.h>
#include <stdlib.h>

#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/Xresource.h>

#include <Xm/XmP.h>
#include <Xm/GadgetP.h>
#include <Xm/VendorSEP.h>

#define LIB_LTTEST
#include "Test.h"


static void _XdbPrintTree(Widget w, int level)
{
	int	i;
	CompositeWidget	cw = (CompositeWidget)w;

	if (w == NULL)
		return;

	for (i=0; i<level; i++)
		fprintf(stderr, "\t");
#ifdef  PRINT_ADDRESS
        fprintf(stderr, "%s : %p/%ld", XtName(w), w, XtWindow(w));
#endif
#ifndef PRINT_APPSHELL_NAME
        if(!XtIsApplicationShell(w))
#endif
        fprintf(stderr, "%s", XtName(w));
	fprintf(stderr, "(%s) geo %d %d %d %d",
		w->core.widget_class->core_class.class_name,
		XtX(w), XtY(w), XtWidth(w), XtHeight(w));
#ifdef	PRINT_STATE
	fprintf(stderr, " state: %s %s",
		_XdbState(w), w->core.mapped_when_managed ? "mwm": "");
#endif
	fprintf(stderr, "\n");
	if (XtIsSubclass(w, compositeWidgetClass))
		for (i=0; i<(int)cw->composite.num_children; i++)
			_XdbPrintTree(cw->composite.children[i], level+1);

	for (i=0; i<(int)cw->core.num_popups; i++)
		_XdbPrintTree(cw->core.popup_list[i], level+1);
}


void XdbPrintTree(Widget w)
{
	_XdbPrintTree(w, 0);
}


void XdbPrintCompleteTree(Widget w)
{
	Widget	ww = w;

	while (ww) {
		w = ww;
		ww = XtParent(w);
	}

	_XdbPrintTree(w, 0);
}


void XdbPrintTreeCB(Widget w, XtPointer client, XtPointer call)
{
	XdbPrintTree((Widget)client);
}
