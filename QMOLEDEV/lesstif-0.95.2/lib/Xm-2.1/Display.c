/**
 *
 * $Id: Display.c,v 1.2 2005/01/04 17:07:05 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Id: Display.c,v 1.2 2005/01/04 17:07:05 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <XmI/MacrosI.h>
#include <XmI/AtomMgrI.h>

#include <Xm/XmP.h>
#include <Xm/DisplayP.h>
#include <Xm/DropSMgrP.h>
#include <Xm/DropTransP.h>
#include <Xm/DragCP.h>
#include <X11/cursorfont.h>

#include <XmI/DebugUtil.h>


#define Offset(field) XtOffsetOf(XmDisplayRec, display.field)

static XtResource resources[] =
{
    {
	XmNdropSiteManagerClass, XmCDropSiteManagerClass, XmRWidgetClass,
	sizeof(WidgetClass), Offset(dropSiteManagerClass),
	XmRImmediate, (XtPointer)&xmDropSiteManagerClassRec
    },
    {
	XmNdropTransferClass, XmCDropTransferClass, XmRWidgetClass,
	sizeof(WidgetClass), Offset(dropTransferClass),
	XmRImmediate, (XtPointer)&xmDropTransferClassRec
    },
    {
	XmNdragContextClass, XmCDragContextClass, XmRWidgetClass,
	sizeof(WidgetClass), Offset(dragContextClass),
	XmRImmediate, (XtPointer)&xmDragContextClassRec
    },
    {
	XmNdragInitiatorProtocolStyle, XmCDragInitiatorProtocolStyle,
	XmRDragInitiatorProtocolStyle,
	sizeof(unsigned char), Offset(dragInitiatorProtocolStyle),
	XmRImmediate, (XtPointer)XmDRAG_PREFER_RECEIVER
    },
    {
	XmNdragReceiverProtocolStyle, XmCDragReceiverProtocolStyle,
	XmRDragReceiverProtocolStyle,
	sizeof(unsigned char), Offset(dragReceiverProtocolStyle),
	XmRImmediate, (XtPointer)XmDRAG_PREFER_PREREGISTER
    },
    {
	XmNdefaultVirtualBindings, "DefaultVirtualBindings" /*XmCString*/, XmRString,
	sizeof(String), Offset(bindingsString),
	XmRImmediate, (XtPointer)NULL	/* adhere to compatibility! */
    },
    {
	XmNnoFontCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(no_font_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNnoRenditionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(no_rendition_callback),
	XmRImmediate, (XtPointer)NULL
    },
};

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

#if 1
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
#endif

static Widget GetXmDisplay(Display *Dsp);

#if 0
/*
 * This one is needed for the automagically refreshing of the modifier
 * mappings.
 */
char _XmDisplay_Translations[] =
    "<Mapping>: RefreshMapping()";


static void RefreshMapping(Widget w, XEvent *event,
			   String *Params, Cardinal *NumParams);


static XtActionsRec actions[] =
{
    {"RefreshMapping", RefreshMapping},
};
#endif

/* *INDENT-OFF* */
static XmBaseClassExtRec _XmDisplayCoreClassExtRec = {
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

XmDisplayClassRec xmDisplayClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &applicationShellClassRec,
        /* class_name            */ "XmDisplay",
	/* widget_size           */ sizeof(XmDisplayRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL, /* Motif has one */
	/* realize               */ XtInheritRealize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ False,
	/* compress_exposure     */ XtExposeNoCompress,
	/* compress_enterleave   */ False,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values, /* Motif doesn't have this */
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmDisplayCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ NULL,
        /* change_managed   */ NULL,
        /* insert_child     */ XtInheritInsertChild, /* Motif has one */
        /* delete_child     */ XtInheritDeleteChild, /* Motif has one */
        /* extension        */ NULL,	
    },
    /* Shell class part */
    {
	/* extension        */ NULL,
    },
    /* WMShell class part */
    {
	/* extension        */ NULL,
    },
    /* vendor shell class part */
    {
	/* extension        */ NULL,
    },
    /* toplevel shell class part */
    {
	/* extension        */ NULL,
    },
    /* application shell class part */    
    {
	/* extension        */ NULL,
    },
    /* display class part */    
    {
        /* GetDisplay       */ GetXmDisplay,
	/* extension        */ NULL,
    }
};
/* *INDENT-ON* */


WidgetClass xmDisplayClass = (WidgetClass)&xmDisplayClassRec;

/*
 * Following is all that stuff (variables) that is needed in order to put
 * the management of XmDisplay widgets per display to live.
 */
#define PDWC_None               ((XContext) 0)
#define PDWC_RID_DisplayWidget  ((XID) 0)

static XContext PerDisplayWidgetContext = PDWC_None;

/*
 * This one is needed due to the internal functions _XmGetXmDisplayClass()
 * and _XmSetXmDisplayClass(). This is one of the reasons why I'm regarding
 * ISO 9000 for software as a bad joke. --aldi
 */
static WidgetClass __XmDisplayClass = (WidgetClass)&xmDisplayClassRec;

String _Xm_MOTIF_DRAG_AND_DROP_MESSAGE = "_MOTIF_DRAG_AND_DROP_MESSAGE";

/*
 * This is a hook for clean-up whenever our display connection gets closed.
 * In reaction to a close we clean-up ourself and all our children.
 * WARNING: The widget ID of the XmDisplay widget is contained in the
 * ClientData parameter and not in the parameter "w"!
 */
static void
DisplaySuicide(Widget w, XtPointer ClientData, XtPointer CallData)
{
    Display *Dsp;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)ClientData, "XmDisplay Connection Close-Down\n"));

    Dsp = XtDisplay((Widget)ClientData);

    XtDestroyWidget((Widget)ClientData);

    _XmFlushAtomsForDisplay(Dsp);	/* do this as last operation */
}

/*
 * _XmAddCloseConnectionCallback registers a callback on the widget w
 * that is triggered by a close-down on the connection the widget is
 * belonging to.
 */
static void
_XmAddCloseConnectionCallback(Widget w, XtCallbackProc callback)
{
#if XtSpecificationRelease >= 6
    XtAddCallback(XtHooksOfDisplay(XtDisplay(w)), XmNdestroyCallback,
		  callback, (XtPointer)w);
#endif
}


/*
 * The widget class methods...
 */
static void
class_initialize(void)
{
    _XmDisplayCoreClassExtRec.record_type = XmQmotif;
}

/*
 * Whenever the XmDisplay class or one of it's subclasses gets initialized,
 * we setup the default proc for getting a display's XmDisplay widget. This
 * way, subclasses can set another default proc within their class init
 * method (or create a chain).
 */
static void
class_part_initialize(WidgetClass widget_class)
{
    if (((XmDisplayClassRec *)widget_class)->display_class.GetDisplay == NULL)
    {
	((XmDisplayClassRec *)widget_class)->display_class.GetDisplay =
	    GetXmDisplay;
    }

    _XmFastSubclassInit(widget_class, XmDISPLAY_BIT);
}

/*
 * Warn the user about double XmDisplay instances for the same display. This
 * can otherwise cause trouble and many tears... Yes, programmers do have
 * feelings.
 */
static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    XPointer FirstDisplayWidget;
    XmDisplay dpy = (XmDisplay)new_w;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:initialize(%d) - %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    dpy->display.shellCount = 0;
    dpy->display.numModals = 0;
    dpy->display.maxModals = 0;
    dpy->display.modals = NULL;

    /*
     * If haven't yet allocated the context with all kind of information
     * about LessTif goodies we'll do it right now. This context contains
     * LessTif goodies on a per display basis.
     */
    if (PerDisplayWidgetContext == PDWC_None)
    {
	PerDisplayWidgetContext = XUniqueContext();
    }

    /*
     * Make sure, that there hasn't already allocated another XmDisplay
     * widget. Then register this widget as the XmDisplay widget for the
     * appropiate display.
     */
    if (XFindContext(XtDisplay(new_w), PDWC_RID_DisplayWidget,
		     PerDisplayWidgetContext,
		     &FirstDisplayWidget) == XCSUCCESS)
    {
	_XmWarning(new_w, "Don't create multiple XmDisplays for one Display");
    }
    else
    {
	XSaveContext(XtDisplay(new_w), PDWC_RID_DisplayWidget,
		     PerDisplayWidgetContext, (XPointer)new_w);
    }

    /*
     * ...
     */
    _XmMessageBoxInstallImages(new_w);

    _XmInstallStippleImages(new_w);

    Display_DisplayInfo(new_w) = (XtPointer)XtMalloc(sizeof(XmDisplayInfo));

    ((XmDisplayInfo *) Display_DisplayInfo(new_w))->SashCursor =
	XCreateFontCursor(XtDisplay(new_w), XC_crosshair);
    ((XmDisplayInfo *) Display_DisplayInfo(new_w))->TearOffCursor =
	XCreateFontCursor(XtDisplay(new_w), XC_fleur);
    ((XmDisplayInfo *) Display_DisplayInfo(new_w))->destinationWidget =
	NULL;
    ((XmDisplayInfo *) Display_DisplayInfo(new_w))->UniqueStamp =
	NULL;

    /*
     * Allocate space for the virtual key bindings and then fetch the
     * translations for the convertion from keys to csf keys. This is all
     * done in the following proc call.
     */
    ((XmDisplayRec *)new_w)->display.bindings = NULL;

    _XmVirtKeysInitialize(new_w);

    /*
     * Register ourself with the current connection so we can kill ourself
     * when the connection is closed.
     */
    _XmAddCloseConnectionCallback(new_w, DisplaySuicide);

    Display_DSM(new_w) = NULL;

    Display_ProxyWindow(new_w) = _XmGetDragProxyWindow(XtDisplay(new_w));
}

/*
 * Don't waste your environment -- clean up everthing and recycle it.
 */
static void
destroy(Widget w)
{
	if (CoreBeingDestroyed(w))
		DEBUGOUT(_LtDebug(__FILE__, w, "Destroy(being_destroyed)\n"));
	else {
		DEBUGOUT(_LtDebug(__FILE__, w, "Destroy\n"));

		/* Hack */
		return;
	}

	if (((XmDisplayRec *)w)->display.bindings) {
		XtFree((char *)((XmDisplayRec *)w)->display.bindings);
		((XmDisplayRec *)w)->display.bindings = NULL;
	}

	/* ... and get rid of old atom id's! */
	_XmFlushAtomsForDisplay(XtDisplay(w));

	DEBUGOUT(_LtDebug(__FILE__, w, "Destroy(end)\n"));
}

#if 1
/*
 * There is almost nothing to do -- yet.
 */
static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:set_values(%d) - %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    return False;
}
#endif

#if 0
/*
 * This action is triggered whenever a MappingNotify event happens on our
 * display's wire. Then we will invalidate the cached modifier mappings
 * and reload the virtual keysym stuff. This way LessTif applications --
 * contrary to some software foundations' stuff -- can be reconfigured
 * during run-time.
 */
static void 
RefreshMapping(Widget w, XEvent *event,
	       String *Params, Cardinal *NumParams)
{
    /*
     * NOTE: w is always NULL with mapping notifications! So we just
     * look up the offending display widget and refresh the virtual
     * key bindings. Happy refreshing...
     */
    DEBUGOUT(_LtDebug(__FILE__, w, "Refreshing virtual key bindings.\n"));

    _XmRefreshVirtKeys(XmGetXmDisplay(event->xmapping.display));
}
#endif

/*
 * This is the real place, where we try to find the XmDisplay widget of
 * a display. If we can't get our hands on the widget, we create one.
 *
 * NOTICE:
 * We now support for user-defined XmDisplay subclasses. Such user-defined
 * replacements can be activated by calling _XmSetXmDisplayClass with a
 * pointer to an appropiate class record.
 */
static Widget 
GetXmDisplay(Display *Dsp)
{
    Widget DisplayWidget = NULL;
    Arg Args[5];
    Cardinal ArgCount;
    String AppName, AppClass;

    /*
     * If we haven't yet a XmDisplay widget for that particular display,
     * we're creating it on the fly.
     */
    if ((PerDisplayWidgetContext == PDWC_None) ||
	(XFindContext(Dsp, PDWC_RID_DisplayWidget, PerDisplayWidgetContext,
		      (XPointer *)&DisplayWidget) != XCSUCCESS))
    {
	/*
	 * Because we need to realize that display widget in the next step,
	 * we have to set the widget's initial size -- otherwise we will
	 * crash with that nasty error message saying, that the shell has
	 * zero with/height...
	 */
	ArgCount = 0;

	XtSetArg(Args[ArgCount], XmNwidth, 1); ArgCount++;
	XtSetArg(Args[ArgCount], XmNheight, 1); ArgCount++;
	XtSetArg(Args[ArgCount], XmNmappedWhenManaged, False); ArgCount++;

	XtGetApplicationNameAndClass(Dsp, &AppName, &AppClass);

	DisplayWidget = XtAppCreateShell(AppName, AppClass, __XmDisplayClass,
					 Dsp, Args, ArgCount);
	/*
	 * The XmDisplay widget is now already registered as the display
	 * object (this is done in DisplayInitialize()).
	 */
    }

    /*
     * Make sure we have a window.
     */
    if (!XtIsRealized(DisplayWidget))
    {
	XtRealizeWidget(DisplayWidget);
    }

    return DisplayWidget;
}

XmDropSiteManagerObject
_XmGetDropSiteManagerObject(XmDisplay xmDisplay)
{
    XmDropSiteManagerObject dsm = NULL;

#if 0
    DEBUGOUT(_LtDebug(__FILE__, (Widget)xmDisplay, "_XmGetDropSiteManagerObject() - %s 0x%p\n",
    	Display_DSM(xmDisplay) ? "Old" : "New", Display_DSM(xmDisplay)));
#endif
    if (!XmIsDisplay(xmDisplay))
    {
    	_XmWarning((Widget)xmDisplay, "%s:_XmGetDropSiteManagerObject(%d) called without an XmDisplay",
    		__FILE__, __LINE__);
    }
    else
    {
	if (Display_DSM(xmDisplay) == NULL)
	{
	    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmGetDropSiteManagerObject(%d) - creating\n",
		__FILE__, __LINE__
		));
	    dsm = (XmDropSiteManagerObject)XtCreateWidget("DropSiteManager",
					   Display_DropSiteManagerClass(xmDisplay),
						       (Widget)xmDisplay, NULL, 0);
	    DEBUGOUT(_LtDebug2(__FILE__, (Widget)xmDisplay, (Widget)dsm, "%s:_XmGetDropSiteManagerObject(%d) - created 0x%p\n",
		__FILE__, __LINE__, dsm
		));

	    Display_DSM(xmDisplay) = dsm;
	}
	else
	{
	    dsm = Display_DSM(xmDisplay);
	}
    }

    return dsm;
}

unsigned char
_XmGetDragProtocolStyle(Widget w)
{
    Widget disp = XmGetXmDisplay(XtDisplay(w));

#if 0
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetDragProtocolStyle()\n"));
#endif
    switch (Display_DragReceiverProtocolStyle(disp))
    {
    case XmDRAG_NONE:
    case XmDRAG_DROP_ONLY:
	return XmDRAG_NONE;

    case XmDRAG_PREFER_PREREGISTER:
    case XmDRAG_PREREGISTER:
    case XmDRAG_PREFER_DYNAMIC:
	return XmDRAG_PREREGISTER;

    case XmDRAG_DYNAMIC:
	return XmDRAG_DYNAMIC;

    case XmDRAG_PREFER_RECEIVER:
    default:
	return XmDRAG_NONE;
    }
}

/*
 * my version of M*tif doesn't define this symbol, but it's in the headers
 * -- MLM
 */
unsigned char
_XmGetDragTrackingMode(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmGetDragTrackingMode(%d)\n",
    	__FILE__, __LINE__));
    return 0;
}

Widget
_XmGetDragContextFromHandle(Widget w, Atom iccHandle)
{
    Widget disp = XmGetXmDisplay(XtDisplay(w)), dc;
    Cardinal c;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmGetDragContextFromHandle(%d)\n",
    	__FILE__, __LINE__));
    for (c = 0; c < MGR_NumChildren(disp); c++)
    {

	dc = MGR_Children(disp)[c];

	if (XmIsDragContext(dc))
	{
	    if (DC_ICCHandle(dc) == iccHandle && !CoreBeingDestroyed(dc))
	    {
		return dc;
	    }
	}
    }

    return NULL;
}

/*
 * This is just there, so one can install her/his own new XmDisplay widget 
 * subclass. It does not make the design clean, but we've to obey the
 * compatibility.
 */
WidgetClass
_XmGetXmDisplayClass(void)
{
    if (__XmDisplayClass == NULL)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmGetXmDisplayClass(%d)\n",
	    __FILE__, __LINE__));
	__XmDisplayClass = (WidgetClass)&xmDisplayClassRec;
    }

    return __XmDisplayClass;
}

/*
 * Install a new XmDisplay subclass, so application developers can replace
 * the default XmDisplay widget class with their own one. The function
 * checks, that the specified widget class is indeed a subclass of the
 * XmDisplay widget class.
 */
WidgetClass
_XmSetXmDisplayClass(WidgetClass wc)
{
    WidgetClass OldDisplayClass, SuperClass;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmSetXmDisplayClass(%d)\n",
    	__FILE__, __LINE__));
    OldDisplayClass = __XmDisplayClass;
    SuperClass = wc;

    while ((SuperClass != NULL) &&
	   (SuperClass != (WidgetClass)&xmDisplayClassRec))
    {
	SuperClass = SuperClass->core_class.superclass;
    }

    if (SuperClass == NULL)
    {
	_XmWarning(NULL,
		   "Can't set XmDisplay class to a non-subclass of XmDisplay.");
    }
    else
    {
	__XmDisplayClass = wc;
    }

    return OldDisplayClass;
}

Widget
XmGetDragContext(Widget w, Time time)
{
    Widget disp = XmGetXmDisplay(XtDisplay(w)), dc = NULL;
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:XmGetDragContext(%d)\n",
    	__FILE__, __LINE__));
    for (i = 0; i < MGR_NumChildren(disp); i++)
    {
	if (XmIsDragContext(MGR_Children(disp)[i]))
	{
	    /* too late */
	    if (DC_DragStartTime(MGR_Children(disp)[i]) > time)
	    {
		continue;
	    }

	    /* too early */
	    if (DC_DragFinishTime(MGR_Children(disp)[i]) != CurrentTime &&
		DC_DragFinishTime(MGR_Children(disp)[i]) < time)
	    {
		continue;
	    }

	    /* either nothing yet, or later than what we have already */
	    if (dc == NULL ||
		(DC_DragStartTime(MGR_Children(disp)[i]) >=
		 DC_DragStartTime(dc)))
	    {

		/* somebody's not doing a Cancel and it's not being removed */
		if (!CoreBeingDestroyed(MGR_Children(disp)[i]))
		{
		    dc = MGR_Children(disp)[i];
		}
	    }
	}
    }

    return dc;
}

/*
 * This thing should create the XmDisplay widget...
 *
 * It is called from lots of places, but certainly from the initialize
 * method of VendorShell. Therefore, we always have a XmDisplay widget
 * created for every display.
 *
 */
Widget 
XmGetXmDisplay(Display *Dsp)
{
    return ((XmDisplayClassRec *)_XmGetXmDisplayClass())->
	display_class.GetDisplay(Dsp);
}
