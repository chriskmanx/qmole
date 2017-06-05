/**
 *
 * $Id: World.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
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

static const char rcsid[] = "$Id: World.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/WorldP.h>

#include <XmI/DebugUtil.h>


XmWorldClassRec xmWorldClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmDesktopClassRec,
        /* class_name            */ "XmWorldClass",
	/* widget_size           */ sizeof(XmWorldRec),
	/* class_initialize      */ NULL,
	/* class_part_initialize */ NULL,
	/* class_inited          */ False,
	/* initialize            */ NULL,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ NULL,
	/* num_resources         */ 0,
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ 0,
	/* compress_exposure     */ 0,
	/* compress_enterleave   */ 0,
	/* visible_interest      */ 0,
	/* destroy               */ NULL,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ NULL,
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
        /* child_class           */ NULL, /* FIX ME */
        /* insert_child          */ NULL, /* FIX ME */
        /* delete_child          */ NULL, /* FIX ME */
        /* extension             */ NULL
    },
    /* World Class part */
    {
        /* extension             */ NULL
    }
};



WidgetClass xmWorldClass = (WidgetClass)&xmWorldClassRec;

XmWorldObject
_XmGetWorldObject(Widget shell,
		  ArgList args,
		  Cardinal *num_args)
{
    return (XmWorldObject)XtCreateManagedWidget("world", xmWorldClass,
						shell, args, *num_args);
}
