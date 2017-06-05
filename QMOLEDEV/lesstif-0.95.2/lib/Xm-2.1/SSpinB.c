/**
 *
 * $Id: SSpinB.c,v 1.7 2004/08/18 15:11:39 dannybackx Exp $
 * 
 * Copyright (C) 2000, 2001, 2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/SSpinB.c,v 1.7 2004/08/18 15:11:39 dannybackx Exp $";

#include <LTconfig.h>

#include <Xm/XmP.h>
#include <Xm/SSpinB.h>

/* FIX ME this #include shouldn't be necessary */
#include <Xm/SpinB.h>

#include <XmI/DebugUtil.h>

/*
 * This should be a subclass of XmManager, a completely other widget than XmSpinBox,
 *	but easier to handle. Go figure !
 *
 * Some of the XmSpinBox constraint resources become real resources here :
 *	XmNdecimalPoints, XmNincrementValue, XmNmaximumValue, XmNminimumValue,
 *	XmNposition, XmNpositionType, XmNspinBoxChildType, XmNvalues.
 */
/*
 * This is completely wrong but it'll probably do at least something sensible for now.
 */
extern Widget
XmCreateSimpleSpinBox(Widget parent, String name, ArgList arglist, Cardinal argcount)
{
	Widget	w;

	w = XmCreateSpinBox(parent, name, arglist, argcount);

	_XmWarning(w, "XmCreateSimpleSpinBox(): not yet implemented!");

	return w;
}


/* 
 * The XmString specifies a new item that's added to the list of possible values
 *	at the specified position.
 */
extern void
XmSimpleSpinBoxAddItem(Widget w, XmString item, int pos)
{
	_XmWarning(NULL, "XmSimpleSpinBoxAddItem(): not yet implemented!");
}


extern void
XmSimpleSpinBoxDeletePos(Widget w, int pos)
{
	_XmWarning(NULL, "XmSimpleSpinBoxDeletePos(): not yet implemented!");
}


/*
 * Causes this item to be the first visible item in the list
 */
extern void
XmSimpleSpinBoxSetItem(Widget w, XmString item)
{
	_XmWarning(NULL, "XmSimpleSpinBoxSetItem(): not yet implemented!");
}

WidgetClass xmSimpleSpinBoxWidgetClass;
