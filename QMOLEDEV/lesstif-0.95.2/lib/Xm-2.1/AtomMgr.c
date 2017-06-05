/**
 *
 * $Id: AtomMgr.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
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

static const char rcsid[] = "$Id: AtomMgr.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/AtomMgr.h>
#include <XmI/AtomMgrI.h>
#include <Xm/XmP.h>

#include <XmI/DebugUtil.h>


static XContext nameToAtom = (XContext)0;
static XContext atomToName = (XContext)0;

/*
 * this function is now dead
 */
void
_XmFlushAtomsForDisplay(Display *display)
{
}

/*
 * save the atom in a quark derived from the name
 * Note that we don't bother to go to the server here...
 */
void
_XmInternAtomAndName(Display *display, Atom atom, String name)
{
    Atom a;
    XrmQuark q;

    if (nameToAtom == (XContext)0)
    {
	nameToAtom = XUniqueContext();
    }

    if (atomToName == (XContext)0)
    {
	atomToName = XUniqueContext();
    }

    q = XrmStringToQuark(name);

    if (XFindContext(display, q, nameToAtom, (XPointer *)&a) != XCSUCCESS)
    {
	XSaveContext(display, q, nameToAtom, (XPointer)atom);

	XSaveContext(display, q, atomToName, (XPointer)atom);
    }
}

/*
 * save the atom in a quark derived from the name
 * Unlike InternAtomAndName, we do go to the server if it's not already
 * defined.
 */
Atom
XmInternAtom(Display *display, String name, Boolean only_if_exists)
{
    static Boolean initted = False;
    XrmQuark q;
    Atom ret;

    if (name == NULL)
    {
	return None;
    }

    if (!initted)
    {
	initted = True;
	_XmInitAtomPairs(display);
    }

    if (nameToAtom == (XContext)0)
    {
	nameToAtom = XUniqueContext();
    }

    if (atomToName == (XContext)0)
    {
	atomToName = XUniqueContext();
    }

    q = XrmStringToQuark(name);

    if (XFindContext(display, q, nameToAtom, (XPointer *)&ret) != XCSUCCESS)
    {

	ret = XInternAtom(display, name, only_if_exists);

	if (only_if_exists && ret == None)
	{
	    return None;
	}

	XSaveContext(display, q, nameToAtom, (XPointer)ret);

	XSaveContext(display, q, atomToName, (XPointer)ret);
    }

    return ret;
}

/*
 * get the name from a quark associated with the atom.
 * Will go to the server if not already internal.
 */
String
XmGetAtomName(Display *display, Atom atom)
{
    XrmQuark q;
    String ret, tmp;

    if (nameToAtom == (XContext)0)
    {
	nameToAtom = XUniqueContext();
    }

    if (XFindContext(display, atom, atomToName, (XPointer *)&q) != XCSUCCESS)
    {
	ret = XGetAtomName(display, atom);

	q = XrmStringToQuark(ret);

	XSaveContext(display, q, nameToAtom, (XPointer)atom);

	XSaveContext(display, q, atomToName, (XPointer)atom);

	if (ret != NULL)
	{
	    tmp = ret;

	    ret = XtNewString(ret);

	    XFree(tmp);
	}

	return ret;
    }

    ret = XrmQuarkToString(q);

    if (ret != NULL)
    {
	tmp = ret;

	ret = XtNewString(ret);

	XFree(tmp);
    }

    return ret;
}
