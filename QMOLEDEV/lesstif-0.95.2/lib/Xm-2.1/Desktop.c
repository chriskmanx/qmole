/**
 *
 * $Id: Desktop.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
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

static const char rcsid[] = "$Id: Desktop.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/DesktopP.h>

#include <XmI/DebugUtil.h>

#if 0
static void class_initialize();
#endif

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

#if 0
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
#endif

static void insert_child(Widget w);

static void delete_child(Widget w);

/*
 * resources 
 */
#define Offset(field) XtOffsetOf(XmDesktopRec, desktop.field)
static XtResource resources[] =
{
    {
	XmNdesktopParent, XmCDesktopParent, XmRWidget,
	sizeof(Widget), Offset(parent),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNextensionType, XmCExtensionType, XmRExtensionType,
	sizeof(unsigned char), XtOffsetOf(XmDesktopRec, ext.extensionType),
	XmRImmediate, (XtPointer)XmDESKTOP_EXTENSION
    },
};
/* *INDENT-OFF* */
XmDesktopClassRec xmDesktopClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmExtClassRec,
        /* class_name            */ "Desktop",
	/* widget_size           */ sizeof(XmDesktopRec),
	/* class_initialize      */ NULL /*class_initialize*/,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ 0,
	/* compress_exposure     */ 0,
	/* compress_enterleave   */ 0,
	/* visible_interest      */ 0,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ NULL /*set_values*/,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ NULL /*_XmExtGetValuesHook*/,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
        /* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* XmExtObject part */
    {
        /* syn_resources      */ NULL,
        /* num_syn_resources  */ 0,
        /* extension          */ NULL
    },
    /* Desktop Class part */
    {
        /* child_class           */ NULL,
        /* insert_child          */ insert_child,
        /* delete_child          */ delete_child,
        /* extension             */ NULL
    }
};
/* *INDENT-ON* */



WidgetClass xmDesktopClass = (WidgetClass)&xmDesktopClassRec;

#if 0
static void
class_initialize()
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "Desktop class initialize\n"));
}
#endif

/*
 * Make the inheritance of the child insert/delete method work.
 */
#define C_DESKTOP(wc) ((XmDesktopClassRec *) wc)
static void
class_part_initialize(WidgetClass wc)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "Desktop class part initialize\n"));

    if (C_DESKTOP(wc)->desktop_class.insert_child == XmInheritWidgetProc)
    {
	C_DESKTOP(wc)->desktop_class.insert_child =
	    C_DESKTOP(wc->core_class.superclass)->desktop_class.insert_child;
    }
    if (C_DESKTOP(wc)->desktop_class.delete_child == XmInheritWidgetProc)
    {
	C_DESKTOP(wc)->desktop_class.delete_child =
	    C_DESKTOP(wc->core_class.superclass)->desktop_class.delete_child;
    }
}				/* class_part_initialize */
#undef C_DESKTOP

/*
 * Whenever working with extension objects in the context of vendor shells
 * and the like the next drawing comes in handy.
 *
 * The extension objects hierarchy looks like this:
 *
 * object---ext---desktopext-+-vendorshellext---dialogshellext
 *   ^                       |
 *   |                       `-worldext
 *   |
 *   v
 *  core---desktopext---screen
 *
 * XmDesktopRec:
 *   [ObjectPart, XmExtPart, XmDesktopPart]
 * XmShellExtRec:
 *   [ObjectPart, XmExtPart, XmDesktopPart, XmShellExtPart]
 * XmVendorShellExtRec:
 *   [ObjectPart, XmExtPart, XmDesktopPart, XmShellExtPart,
 *     XmVendorShellExtPart]
 * XmDialogShellExtRec:
 *   [ObjectPart, XmExtPart, XmDesktopPart, XmShellExtPart,
 *      XmVendorShellExtPart, XmDialogShellExtPart]
 * XmWorldRec:
 *   [ObjectPart, XmExtPart, XmDesktopPart, XmWorldPart]
 * XmScreenRec:
 *   [CorePart, XmDesktopPart, XmScreenPart]
 */

/*
 * Initialize a desktop extension object or one of its real descendants.
 * The desktop part introduces the concept of children into the extension
 * objects. There is one (maybe BIG) assumption on the classes our children
 * must belong to: only desktop extension objects (or descendents) are
 * allowed or we'll face the big bang here. And just to mention: there is
 * a second **REALLY BIG** assumption: the one who created us **MUST**
 * set the XmNdesktopParent resource, otherwise we're dead.
 */
static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Widget desktopParent;
    XtWidgetProc insertChild;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "Desktop initialize\n"));

    Desktop_Children(new_w) = NULL;
    Desktop_NumChildren(new_w) = 0;
    Desktop_NumSlots(new_w) = 0;

    /*
     * Now call our desktop parent so it get knowledge of us (necessary for
     * paying the alimony...)
     */
    desktopParent = Desktop_Parent(new_w);
    if (desktopParent)
    {
	/*
	 * The problem here is that screen objects are derived from
	 * the core widget class whereas the other extension objects
	 * are derived from the generic extension object class and
	 * therefore from object. These different "family trees" thus
	 * make the desktop class part laying around at different
	 * places within the widget class structure. Sigh.
	 */
	if (_XmIsFastSubclass(XtClass(desktopParent), XmSCREEN_BIT))
	{
	    insertChild = ((XmScreenClassRec *)XtClass(desktopParent))->
		desktop_class.insert_child;
	}
	else
	{
	    insertChild = ((XmDesktopClassRec *)XtClass(desktopParent))->
		desktop_class.insert_child;
	}

	if (insertChild == NULL)
	{
	    _XmError(new_w, "insert_child method of my desktop parent is NULL");
	}

	insertChild(new_w);
    }
    else
    {
	/* rws 19 Jul 2000
	   So, is this a problem????
	_XmError(new_w, "Desktop initialize: I have no desktopParent");
	*/
    }
}

static void
destroy(Widget w)
{
    Widget desktopParent;
    XtWidgetProc deleteChild;

    DEBUGOUT(_LtDebug(__FILE__, w, "Desktop destroy\n"));

    desktopParent = Desktop_Parent(w);

    if (desktopParent)
    {
	if (_XmIsFastSubclass(XtClass(desktopParent), XmSCREEN_BIT))
	{
	    deleteChild = ((XmScreenClassRec *)XtClass(desktopParent))->
		desktop_class.delete_child;
	}
	else
	{
	    deleteChild = ((XmDesktopClassRec *)XtClass(desktopParent))->
		desktop_class.delete_child;
	}

	if (deleteChild == NULL)
	{
	    _XmError(w, "delete_child method of my desktop parent is NULL");
	}

	deleteChild(w);
    }
    else
    {
	/* rws 19 Jul 2000
	   So, is this a problem????
	_XmError(w, "Desktop destroy: I have no desktopParent");
	*/
    }

    XtFree((char *)Desktop_Children(w));
}

/*
 * A new child is about to be added to our list of children. At this point
 * we know that the child is of the (or a subclass of) the desktop extension
 * class.
 */
static void
insert_child(Widget w)
{
    Widget MeTheParent;

    MeTheParent = Desktop_Parent(w);

    /*
     * Make free room for the new child, if necessary.
     */
    if (Desktop_NumChildren(MeTheParent) == Desktop_NumSlots(MeTheParent))
    {
	Desktop_NumSlots(MeTheParent) += Desktop_NumSlots(MeTheParent) / 2 + 2;

	Desktop_Children(MeTheParent) = (WidgetList)
	    XtRealloc((char *)Desktop_Children(MeTheParent),
		      sizeof(Widget) * Desktop_NumSlots(MeTheParent));
    }

    Desktop_Children(MeTheParent)[Desktop_NumChildren(MeTheParent)] = w;
    Desktop_NumChildren(MeTheParent)++;
}

/*
 * Remove a child from our childlist. As I mentioned above (in DesktopInsert-
 * Child) we can assume here that our child is of (or a subclass of) the
 * desktop extension object class.
 */
static void
delete_child(Widget w)
{
    Widget MeTheParent;
    WidgetList Children;
    int NumChildren, i;

    MeTheParent = Desktop_Parent(w);
    Children = Desktop_Children(MeTheParent);
    NumChildren = Desktop_NumChildren(MeTheParent);

    for (i = 0; i < NumChildren; i++)
    {
	if (*Children == w)
	{
	    for (i++; i < NumChildren; i++)
	    {
		Children[0] = Children[1];
		Children++;
	    }

	    Desktop_NumChildren(MeTheParent)--;

	    break;
	}
	Children++;
    }
}

#if 0
static Boolean
set_values(Widget current, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "Desktop set_values\n"));

    return True;		/* FIX ME */
}
#endif

WidgetClass
_XmGetActualClass(Display *display, WidgetClass w_class)
{
    return NULL;
}

void
_XmSetActualClass(Display *display,
		  WidgetClass w_class,
		  WidgetClass actualClass)
{
}
