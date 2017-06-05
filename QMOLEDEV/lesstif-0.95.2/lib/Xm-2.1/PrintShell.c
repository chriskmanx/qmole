/**
 * 
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PrintShell.c,v 1.24 2002/04/13 11:08:48 amai Exp $
 *
 * Copyright © 2000,2001,2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PrintShell.c,v 1.24 2002/04/13 11:08:48 amai Exp $";

#include <LTconfig.h>

/*
 * Lots of stuff in this file is only compiled if we have the Xp library.
 *
	#ifdef	HAVE_LIB_XP
 */

#include <stdio.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>

#include <Xm/PrintSP.h>  /* required in any case */
#ifdef HAVE_LIB_XP
#include <Xm/Print.h>
#endif

#include <XmI/DebugUtil.h>


/* Forward Declarations */
static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static void resize(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void _XmPrintNotify(Widget w, XtPointer client, XEvent *evp, Boolean *cont);

#define Offset(field) XtOffsetOf(XmPrintShellRec, print.field)


/* Resources for the PrintShell class */
static XtResource resources[] =
{
    {
	XmNstartJobCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(start_job_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNendJobCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(end_job_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNpageSetupCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(page_setup_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNpdmNotificationCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(pdm_notification_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNminX, XmCMinX, XmRDimension,
	sizeof(Dimension), Offset(min_x),
	XmRImmediate, (XtPointer)NULL		/* dynamic */
    },
    {
	XmNminY, XmCMinY, XmRDimension,
	sizeof(Dimension), Offset(min_y),
	XmRImmediate, (XtPointer)NULL		/* dynamic */
    },
    {
	XmNmaxX, XmCMaxX, XmRDimension,
	sizeof(Dimension), Offset(max_x),
	XmRImmediate, (XtPointer)NULL		/* dynamic */
    },
    {
	XmNmaxY, XmCMaxY, XmRDimension,
	sizeof(Dimension), Offset(max_y),
	XmRImmediate, (XtPointer)NULL		/* dynamic */
    },
    {
	XmNdefaultPixmapResolution, XmCDefaultPixmapResolution, XmRShort,
	sizeof(unsigned short), Offset(default_pixmap_resolution),
	XmRImmediate, (XtPointer)100
    },
};

static XtActionsRec actions[] =
{
#if 0
    {"MenuShellPopdownOne", MenuShellPopdownOne},
#endif
    {NULL, NULL}
};


static XmBaseClassExtRec _XmPrintShellCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL,
    /* set_values_prehook        */ NULL,
    /* initialize_posthook       */ NULL,
    /* set_values_posthook       */ NULL,
    /* secondary_object_class    */ NULL,
    /* secondary_object_create   */ NULL,
    /* get_secondary_resources   */ NULL,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ NULL,
    /* get_values_posthook       */ NULL,
    /* class_part_init_prehook   */ NULL,
    /* class_part_init_posthook  */ NULL,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ False,
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

XmPrintShellClassRec xmPrintShellClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &applicationShellClassRec,
        /* class_name            */ "XmPrintShell",
	/* widget_size           */ sizeof(XmPrintShellRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ XtInheritExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmPrintShellCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */	XtInheritGeometryManager,
        /* change_managed   */	XtInheritChangeManaged,
        /* insert_child     */	XtInheritInsertChild,
        /* delete_child     */	XtInheritDeleteChild,
        /* extension        */	NULL,	
    },
    /* Shell class part */
    {
	/* extension        */ NULL,
    },
    /* WM Shell class part */
    {
	/* extension	*/		NULL,
    },
    /* VendorShell class part */
    {
	/* extension	*/		NULL,
    },
    /* TopLevelShell class part */
    {
	/* extension	*/		NULL,
    },
    /* ApplicationShell class part */
    {
	/* extension	*/		NULL,
    },
    {
    	/* ?? */		NULL,
    },
};

WidgetClass xmPrintShellWidgetClass = (WidgetClass)&xmPrintShellClassRec;


static void
class_initialize(void)
{
    _XmInitializeExtensions();

    _XmPrintShellCoreClassExtRec.record_type = XmQmotif;
}


static void
class_part_initialize(WidgetClass widget_class)
{
#if 0
    _XmFastSubclassInit(widget_class, XmMENU_SHELL_BIT);
#endif
}

#ifdef HAVE_LIB_XP
/*
 * This is a static table to keep the link between widgets and XPContexts.
 * Yeah - this is probably not a very bright idea. Maybe it should also
 * contain the Display.
 */
static int nfields = 0;
typedef struct {
	Widget	w;
	XPContext	c;
} WidgetContext;
static WidgetContext *wctxt = NULL;


static void
_XmStoreWidgetContext(Widget w, XPContext c)
{
	nfields++;
	wctxt = (WidgetContext *)XtRealloc((XtPointer)wctxt, sizeof(WidgetContext) * nfields);
	wctxt[nfields-1].w = w;
	wctxt[nfields-1].c = c;
}

static Widget
_XmPrintContextToWidget(XPContext c)
{
	int	i;

	for (i=0; i<nfields; i++)
		if (wctxt[i].c == c) {
			return wctxt[i].w;
		}
	return NULL;
}

static XPContext
_XmPrintWidgetToContext(Widget w)
{
	int	i;

	for (i=0; i<nfields; i++)
		if (wctxt[i].w == w) {
			return wctxt[i].c;
		}
	return (XPContext)0;
}

static void
SelectNotify(Widget w, int *e, XtPointer *s, int n, XtPointer client)
{
	XPContext	c = XpGetContext(XtDisplay(w));

	if (! c) {
		_XmWarning(w, "XmPrintShell SelectNotify: no print context\n");
		return;
	}

	XpSelectInput(XtDisplay(w), c, XPPrintMask | XPAttributeMask);
	
	_XmStoreWidgetContext(w, c);	/* Here ? */
}


static Boolean
DispatchEvent(XEvent *evp)
{
	XPPrintEvent	*e = (XPPrintEvent*)evp;

	Widget		w = _XmPrintContextToWidget(e->context);

#if 1	/* Only for debugging */
{
	int		error_base, event_base;

	if (!XpQueryExtension(XtDisplay(w), &event_base, &error_base)) {
		return False;
	}

	if (e->type == event_base + XPPrintNotify) {
		switch (e->detail) {
		case XPStartJobNotify:
			DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell-DispatchEvent XPStartJobNotify\n"));
			break;
		case XPEndJobNotify:
			DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell-DispatchEvent XPEndJobNotify\n"));
			break;
		case XPStartDocNotify:
			DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell-DispatchEvent XPStartDocNotify\n"));
			break;
		case XPStartPageNotify:
			DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell-DispatchEvent XPStartPageNotify\n"));
			break;
		case XPEndPageNotify:
			DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell-DispatchEvent XPEndPageNotify\n"));
			break;
		case XPEndDocNotify:
			DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell-DispatchEvent XPEndDocNotify\n"));
			break;
		default:
			DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell DispatchEvent\n"));
		}
	}
}
#endif

	return XtDispatchEventToWidget(w, evp);
}
#endif	/* HAVE_LIB_XP */


static void
initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
#ifdef HAVE_LIB_XP
	int	error_base, event_base;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmPrintShell Initialize\n"));

	if (new_w->core.width == 0 || new_w->core.height == 0) {
		new_w->core.width = 1000;
		new_w->core.height = 1000;	/* FIX ME */
	}

	if (!XpQueryExtension(XtDisplay(new_w), &event_base, &error_base)) {
		DEBUGOUT(_LtDebug(__FILE__, new_w, "XmPrintShell initialize: fail !!\n"));
		return;
	}

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmPrintShell Initialize event_base %d error_base %d\n",
		event_base, error_base));

	XtInsertEventTypeHandler(new_w,
		event_base + XPPrintNotify,
		(XtPointer)XPPrintMask,
		_XmPrintNotify, NULL,
		XtListTail);

	XtRegisterExtensionSelector(XtDisplay(new_w),
		event_base + XPPrintNotify,
                event_base + XPAttributeNotify,
                SelectNotify,
                NULL);

	(void) XtSetEventDispatcher(XtDisplay(new_w),
		event_base + XPPrintNotify,
		DispatchEvent);

	PS_LastPage(new_w) = False;
#endif
}


static void
destroy(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell Destroy\n"));
}


static void
resize(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell Resize\n"));
}


static void
realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmPrintShell Realize\n"));
#if 1

#define superclass (&applicationShellClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass

#endif
}


static Boolean
set_values(Widget current, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmPrintShell SetValues\n"));
	return True;
}

#if 0
/*
 * This isn't a Motif API.
 */
extern Widget
XmCreatePrintShell(Widget parent, char *name, ArgList arglist, Cardinal argcount)
{
    while (parent && !XtIsComposite(parent))
	parent = XtParent(parent);

    return XtCreatePopupShell(name, xmPrintShellWidgetClass, parent,
			      arglist, argcount);
}
#endif

#ifdef HAVE_LIB_XP
static void
_XmPrintNotify(Widget w, XtPointer client, XEvent *evp, Boolean *cont)
{
	XPPrintEvent	*e = (XPPrintEvent *)evp;
	XmPrintShellCallbackStruct	cbs;

	switch (e->detail) {
	case XPStartPageNotify:
		DEBUGOUT(_LtDebug(__FILE__, w, "XPStartPageNotify\n"));

		DEBUGOUT(_LtDebug(__FILE__, w, "XpEndPage\n"));
		XpEndPage(XtDisplay(w));

		if (PS_LastPage(w)) {
			DEBUGOUT(_LtDebug(__FILE__, w, "XpEndJob\n"));
			XpEndJob(XtDisplay(w));
		}
		break;
	case XPEndPageNotify:
		DEBUGOUT(_LtDebug(__FILE__, w, "XPEndPageNotify\n"));

		if (! PS_LastPage(w)) {
			DEBUGOUT(_LtDebug(__FILE__, w, "XpStartPage\n"));
			XpStartPage(XtDisplay(w), XtWindow(w));

			cbs.reason = XmCR_PAGE_SETUP;
			cbs.event = evp;
			cbs.detail = NULL;
			cbs.context = 0;
			cbs.last_page = False;
			if (PS_PageSetupCallback(w))
				XtCallCallbackList(w, PS_PageSetupCallback(w), &cbs);
			PS_LastPage(w) = cbs.last_page;
		}
		break;
	case XPStartDocNotify:
		DEBUGOUT(_LtDebug(__FILE__, w, "XPStartDocNotify\n"));
		break;
	case XPEndDocNotify:
		DEBUGOUT(_LtDebug(__FILE__, w, "XPEndDocNotify\n"));
		break;
	case XPStartJobNotify:
		DEBUGOUT(_LtDebug(__FILE__, w, "XPStartJobNotify\n"));
		PS_LastPage(w) = False;

		cbs.reason = XmCR_START_JOB;
		cbs.event = evp;
		cbs.detail = NULL;
		cbs.context = _XmPrintWidgetToContext(w);
		cbs.last_page = False;

		if (PS_StartJobCallback(w))
			XtCallCallbackList(w, PS_StartJobCallback(w), &cbs);

#if 0
		DEBUGOUT(_LtDebug(__FILE__, w, "XpStartDoc\n"));
		XpStartDoc(XtDisplay(w), XPDocNormal);
#endif

		/* Copied from EndPage section */
		if (! cbs.last_page) {
			DEBUGOUT(_LtDebug(__FILE__, w, "XpStartPage\n"));
			XpStartPage(XtDisplay(w), XtWindow(w));

			cbs.reason = XmCR_PAGE_SETUP;
			cbs.event = evp;
			cbs.detail = NULL;
			cbs.context = _XmPrintWidgetToContext(w);
			cbs.last_page = False;
			if (PS_PageSetupCallback(w))
				XtCallCallbackList(w, PS_PageSetupCallback(w), &cbs);

			PS_LastPage(w) = cbs.last_page;
#if 0
			if (cbs.last_page) {
				DEBUGOUT(_LtDebug(__FILE__, w, "XpEndPage\n"));
				XpEndPage(XtDisplay(w));
			}
#endif
		}
		break;
	case XPEndJobNotify:
		DEBUGOUT(_LtDebug(__FILE__, w, "XPEndJobNotify\n"));
		cbs.reason = XmCR_END_JOB;
		cbs.event = evp;
		cbs.detail = NULL;
		cbs.context = 0;
		cbs.last_page = False;

		if (PS_EndJobCallback(w))
			XtCallCallbackList(w, PS_EndJobCallback(w), &cbs);
		break;
	default:
		DEBUGOUT(_LtDebug(__FILE__, w, "_XmPrintNotify(default)\n"));
		break;
	}
}
#endif	/* HAVE_LIB_XP */
