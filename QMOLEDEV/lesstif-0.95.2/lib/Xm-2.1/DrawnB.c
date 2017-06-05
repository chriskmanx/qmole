/**
 *
 * $Id: DrawnB.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team 
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

static const char rcsid[] = "$Id: DrawnB.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/DrawnBP.h>
#include <Xm/RepType.h>
#include <Xm/TransltnsP.h>

#if XmVERSION > 1
#include <Xm/TraitP.h>
#include <Xm/ActivatableT.h>
#endif

#include <XmI/DebugUtil.h>

/*
 * Experiment
 *
 * Try to add trait stuff by #ifdeffing it.
 */
#if XmVERSION > 1
void _XmDrawnB_TraitAddCallback(Widget, XtCallbackProc, XtPointer, Boolean);

static XmActivatableTraitRec _XmDrawnBTraitRec = {
	/* version      */      0,
	/* cb           */      _XmDrawnB_TraitAddCallback
};
#endif

/* Forward Declarations */
#if XmVERSION > 1
static void class_initialize(void);
#endif
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static void expose(Widget w, XEvent *event, Region region);
static void resize(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);



/*
 * Resources for the pushButton class
 */
#define Offset(field) XtOffsetOf(XmDrawnButtonRec, drawnbutton.field)
static XtResource resources[] =
{
    {
	XmNmultiClick, XmCMultiClick, XmRMultiClick,
	sizeof(unsigned char), Offset(multiClick),
	XmRImmediate, (XtPointer)XmMULTICLICK_KEEP
    },
    {
	XmNpushButtonEnabled, XmCPushButtonEnabled, XmRBoolean,
	sizeof(Boolean), Offset(pushbutton_enabled),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNshadowType, XmCShadowType, XmRShadowType,
	sizeof(unsigned char), Offset(shadow_type),
	XmRImmediate, (XtPointer)XmSHADOW_ETCHED_IN
    },
    {
	XmNactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(activate_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNarmCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(arm_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNdisarmCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(disarm_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNexposeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(expose_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNresizeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(resize_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmDrawnButtonRec, primitive.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNlabelString, XmCXmString, XmRXmString,
	sizeof(XmString), XtOffsetOf(XmDrawnButtonRec, label._label),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmDrawnButtonRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmDrawnButtonRec, primitive.highlight_thickness),
	XmRImmediate, (XtPointer)2
    }
};

static void Arm(Widget w, XEvent *event,
		String *params, Cardinal *num_params);

static void Activate(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void Disarm(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);

static void ArmAndActivate(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);

static void EnterWindow(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void LeaveWindow(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void MultiArm(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void MultiActivate(Widget w, XEvent *event,
			  String *params, Cardinal *num_params);


static XtActionsRec actions[] =
{
    {"Arm", Arm},
    {"Activate", Activate},
    {"MultiActivate", MultiActivate},
    {"MultiArm", MultiArm},
    {"ArmAndActivate", ArmAndActivate},
    {"Disarm", Disarm},
    {"Enter", EnterWindow},
    {"Leave", LeaveWindow},
};

XmPrimitiveClassExtRec _XmDrawnBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmInheritBaselineProc,
    /* widget_display_rect */ XmInheritDisplayRectProc,
    /* widget_margins      */ NULL
};

XmDrawnButtonClassRec xmDrawnButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelClassRec,
	/* class_name            */ "XmDrawnButton",
	/* widget_size           */ sizeof(XmDrawnButtonRec),
#if XmVERSION > 1
	/* class_initialize      */ class_initialize,
#else
	/* class_initialize      */ NULL,
#endif
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
	/* compress_exposure     */ XtExposeNoCompress,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmDrawnB_defaultTranslations,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
	/* border_unhighlight    */ XmInheritBorderUnhighlight,
	/* translations          */ XtInheritTranslations,
	/* arm_and_activate_proc */ ArmAndActivate,
	/* Synthetic Resources   */ NULL,
	/* num syn res           */ 0,
	/* extension             */ (XtPointer)&_XmDrawnBPrimClassExtRec
    },
    /* Label Class part */
    {
	/* setOverrideCallback */ XmInheritSetOverrideCallback,
	/* menuProcs           */ NULL,
	/* translations        */ NULL,
	/* extension           */ NULL
    },
    /* DrawnButton Class part */
    {
	/* extension */ NULL
    }
};
/* *INDENT-ON* */

WidgetClass xmDrawnButtonWidgetClass = (WidgetClass)&xmDrawnButtonClassRec;

#if XmVERSION > 1
static void
class_initialize(void)
{
    if (! XmeTraitSet((XtPointer)xmDrawnButtonWidgetClass, XmQTactivatable,
		(XtPointer)&_XmDrawnBTraitRec)) {
	_XmWarning(NULL,
	    "XmDrawnButton ClassInitialize: XmeTraitSet failed\n");
    }
}
#endif

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmDRAWN_BUTTON_BIT);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "initialize: %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    /* check the RepType resources */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick),
			     DB_MultiClick(new_w), new_w))
	DB_MultiClick(new_w) = XmMULTICLICK_KEEP;
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRShadowType),
			     DB_ShadowType(new_w), new_w))
	DB_ShadowType(new_w) = XmSHADOW_ETCHED_IN;

    DB_Armed(new_w) = False;
    DB_Timer(new_w) = 0;
}

static void
destroy(Widget w)
{
    if (DB_Timer(w) != 0)
    {
	XtRemoveTimeOut(DB_Timer(w));
	DB_Timer(w) = 0;
    }
}

static void
realize(Widget w,
	XtValueMask *value_mask,
	XSetWindowAttributes *attributes)
{
    /* this is about the only reason I can think of why DrawnB should want
     * a realize */
    *value_mask |= CWBackingStore | CWBitGravity;
    attributes->backing_store = NotUseful;
    attributes->bit_gravity = ForgetGravity;

#define superclass (&xmPrimitiveClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "set_values: %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    /* check the RepType resources */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick),
			     DB_MultiClick(new_w), new_w))
	DB_MultiClick(new_w) = DB_MultiClick(old);
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRShadowType),
			     DB_ShadowType(new_w), new_w))
	DB_ShadowType(new_w) = DB_ShadowType(old);

    if (DB_ShadowType(old) != DB_ShadowType(new_w))
    {
	refresh_needed = True;
    }

    /* rws 23 Jul 1997
       Label will have polluted the height and width since it will not
       be able to determine the correct size, since it does not know what
       is in the window!  Therefore if the size has changed use the
       requested size.
       xdir top right hand logo button
       JHG: If there's something identifiable in the button, trust Label.
       rws 11 Sep 1999
       Don't trust Label. Take a look at drawnbutton/test9
     */
    if (/* !Lab_TextRect_height(new_w) && */
	(XtWidth(new_w) != XtWidth(request) ||
	 XtHeight(new_w) != XtHeight(request)))
    {
	XtWidth(new_w) = XtWidth(request);
	XtHeight(new_w) = XtHeight(request);
	refresh_needed = True;
    }
    return refresh_needed;
}

static void
expose(Widget w, XEvent *event, Region region)
{
    XmDrawnButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "Expose (%dx%d%+d%+d)\n",
		      XtWidth(w), XtHeight(w),XtX(w),XtY(w)));

#define superclass (&xmLabelClassRec)
    (*superclass->core_class.expose) (w, event, region);
#undef superclass
#if 0
    /* rws 24 Oct 1998
       mfm clipboard pixmap
     */
    if (XtSensitive(w) &&
	     Lab_Pixmap(w) != XmUNSPECIFIED_PIXMAP)
    {
	XCopyArea(XtDisplay(w),
		  Lab_Pixmap(w),
		  XtWindow(w),
		  Lab_NormalGC(w),
		  0,
		  0,
		  Lab_TextRect_width(w),
		  Lab_TextRect_height(w),
		  (XtWidth(w) - Lab_TextRect_width(w)) / 2,
		  (XtHeight(w) - Lab_TextRect_height(w)) / 2);
    }
    else if (!XtSensitive(w) &&
	     Lab_PixmapInsensitive(w) != XmUNSPECIFIED_PIXMAP)
    {
	XCopyArea(XtDisplay(w),
		  Lab_PixmapInsensitive(w),
		  XtWindow(w),
		  Lab_InsensitiveGC(w),
		  0,
		  0,
		  Lab_TextRect_width(w),
		  Lab_TextRect_height(w),
		  Lab_TextRect_x(w), Lab_TextRect_y(w));
    }
#endif

    _XmDrawShadows(XtDisplay(w), XtWindow(w),
		   Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
		   Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		   XtWidth(w) - (Prim_HighlightThickness(w) << 1),
		   XtHeight(w) - (Prim_HighlightThickness(w) << 1),
		   Prim_ShadowThickness(w),
		   DB_PushButtonEnabled(w)
		   ? XtSensitive(w) && DB_Armed(w)
		     ? XmSHADOW_IN
		     : XmSHADOW_OUT
		   : DB_ShadowType(w));

    if (DB_ExposeCallback(w))
    {
	cbs.reason = XmCR_EXPOSE;
	cbs.event = event;
	cbs.window = XtWindow(w);
	cbs.click_count = DB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   DB_ExposeCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
resize(Widget w)
{
    XmDrawnButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "Resize (%dx%d%+d%+d)\n",
		      XtWidth(w), XtHeight(w),XtX(w),XtY(w)));

#define superclass (&xmLabelClassRec)
    (*superclass->core_class.resize) (w);
#undef superclass

    if (XtIsRealized(w) && DB_ResizeCallback(w))
    {
	cbs.reason = XmCR_RESIZE;
	cbs.event = NULL;
	cbs.window = XtWindow(w);
	cbs.click_count = DB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   DB_ResizeCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
Arm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmDrawnButtonCallbackStruct cbs;

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);

    if (!DB_Armed(w))
    {
	DB_Armed(w) = True;
	if (event)
	    DB_ArmTimeStamp(w) = event->xbutton.time;

	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, NULL);

	if (DB_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.window = XtWindow(w);
	    cbs.click_count = DB_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       DB_ArmCallback(w),
			       (XtPointer)&cbs);
	}
    }
}

static void
Activate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmDrawnButtonCallbackStruct cbs;

    DB_ClickCount(w) = 1;

    DB_Armed(w) = False;

    if (XtIsRealized(w))
	XtClass(w)->core_class.expose(w, event, NULL);

    if (event->xany.type == KeyPress || event->xany.type == KeyRelease ||
	(event->xbutton.x >= 0 && event->xbutton.x < XtWidth(w) &&
	 event->xbutton.y >= 0 && event->xbutton.y < XtHeight(w)))
    {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.window = XtWindow(w);
	cbs.click_count = DB_ClickCount(w);
	if (XmIsRowColumn(XtParent(w)))
	{
	    RC_MenuMenuCallback(w, &cbs);
	}
	if (!Lab_SkipCallback(w) && DB_ActivateCallback(w))
	{
	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       DB_ActivateCallback(w),
			       (XtPointer)&cbs);
	}
    }
}

static void
Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmDrawnButtonCallbackStruct cbs;

    if (DB_Armed(w))
    {
	DB_Armed(w) = False;
	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, (Region)NULL);
    }

    if (DB_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.window = XtWindow(w);
	cbs.click_count = DB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   DB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
ArmTimeout(XtPointer data, XtIntervalId *id)
{
    Widget w = (Widget)data;

    DEBUGOUT(_LtDebug(__FILE__, w, "ArmTimeout\n"));

    DB_Timer(w) = 0;

    if (XtIsRealized(w))
    {
	XtClass(w)->core_class.expose(w, NULL, NULL);
	XFlush(XtDisplay(w));
    }
}

static void
ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmDrawnButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "ArmAndActivate\n"));

    /* Arm, Activate, and Disarm now, but draw the disarmed state later */

    Arm(w, event, params, num_params);
    if (!Lab_SkipCallback(w) && DB_ActivateCallback(w))
    {
	XFlush(XtDisplay(w));
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.window = XtWindow(w);
	cbs.click_count = 1;
	XtCallCallbackList(w,
			   DB_ActivateCallback(w),
			   (XtPointer)&cbs);
    }
    DB_Armed(w) = False;
    if (DB_DisarmCallback(w))
    {
	XFlush(XtDisplay(w));
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.window = XtWindow(w);
	cbs.click_count = 1;
	XtCallCallbackList(w,
			   DB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    if (DB_Timer(w) != 0)
    {
	XtRemoveTimeOut(DB_Timer(w));
	DB_Timer(w) = 0;
    }

    DB_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				  ACTIVATE_DELAY, ArmTimeout, (XtPointer)w);
}

static void
EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    _XmPrimitiveEnter(w, event, NULL, NULL);

    if (DB_PushButtonEnabled(w) && XtSensitive(w) && DB_Armed(w) &&
	XtIsRealized(w))
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		       XtWidth(w) - 2 * Prim_HighlightThickness(w),
		       XtHeight(w) - 2 * Prim_HighlightThickness(w),
		       Prim_ShadowThickness(w),
		       XmSHADOW_IN);
    }
}

static void
LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    _XmPrimitiveLeave(w, event, NULL, NULL);

    if (DB_PushButtonEnabled(w) && XtSensitive(w) && DB_Armed(w) &&
	XtIsRealized(w))
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       Prim_HighlightThickness(w), Prim_HighlightThickness(w),
		       XtWidth(w) - 2 * Prim_HighlightThickness(w),
		       XtHeight(w) - 2 * Prim_HighlightThickness(w),
		       Prim_ShadowThickness(w),
		       XmSHADOW_OUT);
    }
}

static void
MultiArm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MultiArm\n"));

    if (DB_MultiClick(w) == XmMULTICLICK_KEEP)
    {
	Arm(w, event, NULL, NULL);
    }
}

static void
MultiActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    XmDrawnButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "DrawnB: MultiClick\n"));

    if (DB_MultiClick(w) == XmMULTICLICK_KEEP)
    {
	Time mctime = XtGetMultiClickTime(XtDisplay(w));

	if ((event->xbutton.time - DB_ArmTimeStamp(w)) < mctime)
	{
	    DB_ClickCount(w)++;
	}
	else
	{
	    DB_ClickCount(w) = 1;
	}

	DB_Armed(w) = False;

	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, NULL);

	if (ev->type == KeyPress || ev->type == KeyRelease ||
	    ((ev->x >= 0 && ev->x < XtWidth(w)) &&
	     (ev->y >= 0 && ev->y < XtHeight(w))))
	{
	    if (!Lab_SkipCallback(w) && DB_ActivateCallback(w))
	    {
		cbs.reason = XmCR_ACTIVATE;
		cbs.event = event;
		cbs.click_count = DB_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   DB_ActivateCallback(w),
				   (XtPointer)&cbs);
	    }
	}

	Disarm(w, event, params, num_params);
    }
}

Widget
XmCreateDrawnButton(Widget parent, char *name,
		    Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name, xmDrawnButtonWidgetClass, parent,
			  arglist, argcount);
}

#if XmVERSION > 1
void _XmDrawnB_TraitAddCallback(Widget w,
				XtCallbackProc cb,
				XtPointer cbp,
				Boolean set)
{
    if (set)
	XtAddCallback(w, XmNactivateCallback, cb, cbp);
    else
	XtRemoveCallback(w, XmNactivateCallback, cb, cbp);
}
#endif
