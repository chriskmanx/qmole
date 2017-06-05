/**
 *
 * $Id: ExtObject.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
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

static const char rcsid[] = "$Id: ExtObject.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/CacheP.h>

#include <XmI/DebugUtil.h>

static XmExtCache *cache = NULL;

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void class_part_init_prehook(WidgetClass w_class);

static void class_part_init_posthook(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void initialize_prehook(Widget request, Widget new_w,
			       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static Boolean set_values_prehook(Widget current, Widget request, Widget new_w,
				  ArgList args, Cardinal *num_args);

static void get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args);


static void logical_parent(Widget w, int offset, XrmValue *val);

#define Offset(field) XtOffsetOf(XmExtRec, ext.field)
static XtResource resources[] =
{
    {
	XmNlogicalParent, XmCLogicalParent, XmRWidget,
	sizeof(Widget), Offset(logicalParent),
	XmRCallProc, (XtPointer)logical_parent
    },
    {
	XmNextensionType, XmCExtensionType, XmRExtensionType,
	sizeof(unsigned char), Offset(extensionType),
	XmRImmediate, (XtPointer)XmDEFAULT_EXTENSION
    }
};


static XmBaseClassExtRec _XmObjectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ NULL,
    /* set_values_posthook       */ NULL,
    /* secondary_object_class    */ NULL,
    /* secondary_object_create   */ NULL,
    /* get_secondary_resources   */ NULL,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ get_values_prehook,
    /* get_values_posthook       */ NULL,
    /* class_part_init_prehook   */ class_part_init_prehook,
    /* class_part_init_posthook  */ class_part_init_posthook,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ False,
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

XmExtClassRec xmExtClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &objectClassRec,
        /* class_name            */ "XmExtObject",
	/* widget_size           */ sizeof(XmExtRec),
	/* class_initialize      */ class_initialize,
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
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ _XmExtGetValuesHook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
        /* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmObjectClassExtRec
    },
    /* XmExtObject part */
    {
        /* syn_resources      */ NULL,
        /* num_syn_resources  */ 0,
        /* extension          */ NULL
    }

};


WidgetClass xmExtObjectClass = (WidgetClass)&xmExtClassRec;

static void
logical_parent(Widget w,
	       int offset,
	       XrmValue *val)
{
    val->addr = (XPointer)XtParent(w);
}

static void
class_initialize(void)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "ExtObject class initialize\n"));

    if (cache == NULL)
    {
	cache = (XmExtCache *)XtCalloc(XmNUM_ELEMENTS, sizeof(XmExtCache));
    }

    _XmObjectClassExtRec.record_type = XmQmotif;
}

static void
class_part_init_prehook(WidgetClass widget_class)
{
}

static void
class_part_init_posthook(WidgetClass widget_class)
{
}

static void
class_part_initialize(WidgetClass widget_class)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "ExtObject class part initialize\n"));

    if (widget_class == xmExtObjectClass)
    {
	return;
    }

    _XmBaseClassPartInitialize(widget_class);

    /* compile the resources */
    _XmBuildExtResources(widget_class);
}

static void
initialize_prehook(Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "ExtObject initialize: Widget class %s\n",
		      ((ObjectClassPart *)XtClass(new_w))->class_name));

    _XmExtImportArgs(new_w, args, num_args);
}

static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ExtObject destroy\n"));
}

static Boolean
set_values_prehook(Widget current, Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    return False;
}

static Boolean
set_values(Widget current, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "ExtObject set_values\n"));

    _XmExtImportArgs(new_w, args, num_args);

    return True;		/* FIX ME */
}

static void
get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args)
{
}

char *
_XmExtObjAlloc(Cardinal size)
{
    int i;

    if (cache == NULL)
    {
	cache = (XmExtCache *)XtCalloc(XmNUM_ELEMENTS, sizeof(XmExtCache));
    }

    if (size < XmNUM_BYTES)
    {
	for (i = 0; i < XmNUM_ELEMENTS; i++)
	{
	    if (cache[i].inuse == False)
	    {
		cache[i].inuse = True;
		return cache[i].data;
	    }
	}
    }

    return XtCalloc(1, size);
}

void
_XmExtObjFree(XtPointer element)
{
    int i;

    if (cache == NULL)
	cache = (XmExtCache *)XtCalloc(XmNUM_ELEMENTS, sizeof(XmExtCache));

    for (i = 0; i < XmNUM_ELEMENTS; i++)
    {
	if (element == cache[i].data)
	{
	    cache[i].inuse = False;
	    return;
	}
    }

    XtFree((char *)element);
}

void
_XmBuildExtResources(WidgetClass c)
{
    XmExtObjectClass ec = (XmExtObjectClass)c;

    _XmInitializeSyntheticResources(ec->ext_class.syn_resources,
				    ec->ext_class.num_syn_resources);

    if (c != xmExtObjectClass)
    {
	XmExtObjectClass super = (XmExtObjectClass)ec->object_class.superclass;

	_XmBuildResources(&ec->ext_class.syn_resources,
			  &ec->ext_class.num_syn_resources,
			  super->ext_class.syn_resources,
			  super->ext_class.num_syn_resources);
    }
}
