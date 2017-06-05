/**
 *
 * $Id: ArrowB.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
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

static const char rcsid[] = "$Id: ArrowB.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ArrowBP.h>
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
void _XmArrowB_TraitAddCallback(Widget, XtCallbackProc, XtPointer, Boolean);

static XmActivatableTraitRec _XmArrowBTraitRec = {
	/* version      */      0,
	/* cb           */      _XmArrowB_TraitAddCallback
};
#endif

#if XmVERSION > 1
#define PUSH_GC(w)			(XtSensitive(w) \
					 ? AB_ArrowGC(w) : AB_InsensitiveGC(w))
#define DETAIL_SHADOW_THICKNESS(w)	AB_DetailShadowThickness(w)
#else
#define PUSH_GC(w)			NULL
#define DETAIL_SHADOW_THICKNESS(w)	Xm3D_ENHANCE_PIXEL
#endif

/* Forward Declarations */
#if XmVERSION > 1
static void class_initialize(void);
#endif
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void expose(Widget w, XEvent *event, Region region);
#if 0
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);
#endif
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

/*
 * Resources for the ArrowButton class
 */
#define Offset(field) XtOffsetOf(XmArrowButtonRec, arrowbutton.field)
static XtResource resources[] =
{
    {
	XmNmultiClick, XmCMultiClick, XmRMultiClick,
	sizeof(unsigned char), Offset(multiClick),
	XmRImmediate, (XtPointer)XmMULTICLICK_KEEP
    },
    {
	XmNarrowDirection, XmCArrowDirection, XmRArrowDirection,
	sizeof(unsigned char), Offset(direction),
	XmRImmediate, (XtPointer)XmARROW_UP
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
#if XmVERSION > 1
    {
	XmNdetailShadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(detail_shadow_thickness),
	XmRImmediate, (XtPointer)2 /* FIX ME */
    }
#endif
};

#if XmVERSION > 1
static XmSyntheticResource syn_resources[] =
{
    {
	XmNdetailShadowThickness,
	sizeof(Dimension), Offset(detail_shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};
#endif

static void Activate(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void Arm(Widget w, XEvent *event,
		String *params, Cardinal *num_params);

static void ArmAndActivate(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);

static void Disarm(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);

static void EnterWindow(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void LeaveWindow(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void MultiActivate(Widget w, XEvent *event,
			  String *params, Cardinal *num_params);

static void MultiArm(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);


static XtActionsRec actions[] = {
    {"Activate",                Activate},
    {"MultiActivate",           MultiActivate},
    {"Arm",                     Arm},
    {"MultiArm",                MultiArm},
    {"Disarm",                  Disarm},
    {"ArmAndActivate",          ArmAndActivate},
    {"Enter",                   EnterWindow},
    {"Leave",                   LeaveWindow},
};

XmArrowButtonClassRec xmArrowButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
	/* class_name            */ "XmArrowButton",
	/* widget_size           */ sizeof(XmArrowButtonRec),
#if XmVERSION > 1
	/* class_initialize      */ class_initialize,
#else
	/* class_initialize      */ NULL,
#endif
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
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
	/* resize                */ NULL,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersionDontCheck,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmArrowB_defaultTranslations,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)NULL
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
	/* border_unhighlight    */ XmInheritBorderUnhighlight,
	/* translations          */ XtInheritTranslations,
	/* arm_and_activate_proc */ ArmAndActivate,
#if XmVERSION > 1
	/* synthetic resources   */ syn_resources,
#else
	/* synthetic resources   */ NULL,
#endif
	/* num syn res           */ 0,
	/* extension             */ (XtPointer)NULL
    },
    /* ArrowButton Class part */
    {
	/* extension */ NULL
    }
};


WidgetClass xmArrowButtonWidgetClass = (WidgetClass)&xmArrowButtonClassRec;

#if XmVERSION > 1
static void
class_initialize(void)
{
    if (! XmeTraitSet((XtPointer)xmArrowButtonWidgetClass, XmQTactivatable,
		(XtPointer)&_XmArrowBTraitRec)) {
	_XmWarning(NULL,
	    "XmArrowButtonWidget ClassInitialize: XmeTraitSet failed\n");
    }
}
#endif

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmARROW_BUTTON_BIT);
}

static void
CreateArrowGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;

    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillSolid;

    AB_ArrowGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateInsensitiveGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction | GCStipple |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask |
	GCTileStipXOrigin | GCTileStipYOrigin;

    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;
    values.stipple =
	XmGetPixmapByDepth(XtScreen(w), "50_foreground", 1, 0, 1);

    AB_InsensitiveGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRArrowDirection),
			     AB_Direction(new_w), new_w))
	AB_Direction(new_w) = XmARROW_UP;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick),
			     AB_MultiClick(new_w), new_w))
	AB_MultiClick(new_w) = XmMULTICLICK_KEEP;

    if (XtWidth(request) == (Dimension)0)
    {
	XtWidth(new_w) += 15;
    }
    if (XtHeight(request) == (Dimension)0)
    {
	XtHeight(new_w) += 15;
    }

    AB_Armed(new_w) = False;

    CreateArrowGC(new_w);
    CreateInsensitiveGC(new_w);

    AB_Timer(new_w) = 0;
}

static void
destroy(Widget w)
{
    if (AB_Timer(w) != 0)
    {
	XtRemoveTimeOut(AB_Timer(w));
	AB_Timer(w) = 0;
    }

    XtReleaseGC(w, AB_ArrowGC(w));
    XtReleaseGC(w, AB_InsensitiveGC(w));
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRArrowDirection),
			     AB_Direction(new_w), new_w))
	AB_Direction(new_w) = AB_Direction(old);

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick),
			     AB_MultiClick(new_w), new_w))
	AB_MultiClick(new_w) = AB_MultiClick(old);

    DEBUGOUT(_LtDebug(__FILE__, new_w, "ArrowB set_values\n"));

    if (AB_Direction(old) != AB_Direction(new_w) ||
#if XmVERSION > 1
	AB_DetailShadowThickness(old) != AB_DetailShadowThickness(new_w) ||
#endif
	XtSensitive(old) != XtSensitive(new_w))
    {
	refresh_needed = True;
    }
    if (Prim_Foreground(old) != Prim_Foreground(new_w) ||
	XtBackground(old) != XtBackground(new_w))
    {
	XtReleaseGC(old, AB_ArrowGC(old));
	XtReleaseGC(old, AB_InsensitiveGC(old));
	CreateArrowGC(new_w);
	CreateInsensitiveGC(new_w);
	refresh_needed = True;
    }

    return refresh_needed;
}

static void
expose(Widget w, XEvent *event, Region region)
{
    int margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w);
    GC myGC;

    if (XtSensitive(w))
    {
	myGC = AB_ArrowGC(w);
    }
    else
    {
	myGC = AB_InsensitiveGC(w);
    }

    if (Prim_Highlighted(w))
    {
	(*PrimC_BorderHighlight(XtClass(w))) (w);
    }
    else
    {
	(*PrimC_BorderUnhighlight(XtClass(w))) (w);
    }

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   Prim_HighlightThickness(w),
		   Prim_HighlightThickness(w),
		   XtWidth(w) - 2 * Prim_HighlightThickness(w),
		   XtHeight(w) - 2 * Prim_HighlightThickness(w),
		   Prim_ShadowThickness(w),
		   XmSHADOW_OUT);

    if (AB_Armed(w))
    {
	_XmDrawArrow(XtDisplay(w),
		     XtWindow(w),
		     Prim_BottomShadowGC(w),
		     Prim_TopShadowGC(w),
		     myGC,
		     margin, margin,
		     XtWidth(w) - (margin << 1),
		     XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     AB_Direction(w));
    }
    else
    {
	_XmDrawArrow(XtDisplay(w),
		     XtWindow(w),
		     Prim_TopShadowGC(w),
		     Prim_BottomShadowGC(w),
		     myGC,
		     margin, margin,
		     XtWidth(w) - (margin << 1),
		     XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     AB_Direction(w));
    }
}

#if 0
static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    answer->request_mode = CWWidth | CWHeight;

    answer->width = XtWidth(w);

    answer->height = XtHeight(w);

    if (((proposed->request_mode & (CWWidth | CWHeight))
	 == (CWWidth | CWHeight)) &&
	proposed->width == answer->width && proposed->height == answer->height)
    {
	return XtGeometryYes;
    }
    else if (answer->width == XtWidth(w) && answer->height == XtHeight(w))
    {
	return XtGeometryNo;
    }
    else
    {
	return XtGeometryAlmost;
    }
}
#endif


static void
Arm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int margin;
    XmArrowButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "Arm\n"));

    if (!AB_Armed(w))
    {
	XmProcessTraversal(w, XmTRAVERSE_CURRENT);

	AB_Armed(w) = True;
	if (event)
	{
	    AB_ArmTimeStamp(w) = event->xbutton.time;
	}

	if (XtIsRealized(w))
	{
	    margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w);
	    _XmDrawArrow(XtDisplay(w),
			 XtWindow(w),
			 Prim_BottomShadowGC(w),
			 Prim_TopShadowGC(w),
			 PUSH_GC(w),
			 margin, margin,
			 XtWidth(w) - (margin << 1),
			 XtHeight(w) - (margin << 1),
			 DETAIL_SHADOW_THICKNESS(w),
			 AB_Direction(w));
	}

	if (AB_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.click_count = AB_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       AB_ArmCallback(w),
			       (XtPointer)&cbs);
	}
    }
}

static void
Activate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int margin;
    XmArrowButtonCallbackStruct cbs;
    XButtonEvent *ev = (XButtonEvent *)event;

    DEBUGOUT(_LtDebug(__FILE__, w, "Activate 000\n"));

    if (XtIsRealized(w))
    {
	margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w);
	_XmDrawArrow(XtDisplay(w),
		     XtWindow(w),
		     Prim_TopShadowGC(w),
		     Prim_BottomShadowGC(w),
		     PUSH_GC(w),
		     margin, margin,
		     XtWidth(w) - (margin << 1),
		     XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     AB_Direction(w));
    }

    AB_Armed(w) = False;
    AB_ClickCount(w) = 1;

    if ((ev->x >= 0 && ev->x < XtWidth(w)) &&
	(ev->y >= 0 && ev->y < XtHeight(w)))
    {

	if (AB_ActivateCallback(w))
	{
	    cbs.reason = XmCR_ACTIVATE;
	    cbs.event = event;
	    cbs.click_count = AB_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       AB_ActivateCallback(w),
			       (XtPointer)&cbs);
	}
    }
}

static void
Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int margin;
    XmArrowButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "Disarm\n"));

    if (AB_Armed(w))
    {
	AB_Armed(w) = False;
	if (XtIsRealized(w))
	{
	    margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w);
	    _XmDrawArrow(XtDisplay(w),
			 XtWindow(w),
			 Prim_TopShadowGC(w),
			 Prim_BottomShadowGC(w),
			 PUSH_GC(w),
			 margin, margin,
			 XtWidth(w) - (margin << 1),
			 XtHeight(w) - (margin << 1),
			 DETAIL_SHADOW_THICKNESS(w),
			 AB_Direction(w));
	}
    }

    if (AB_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = AB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   AB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
ArmTimeout(XtPointer data, XtIntervalId *id)
{
    int margin;
    Widget w = (Widget)data;

    DEBUGOUT(_LtDebug(__FILE__, w, "ArmTimeout\n"));

    AB_Timer(w) = 0;

    if (XtIsRealized(w))
    {
	margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w);
	if (AB_Armed(w))
	{
	    _XmDrawArrow(XtDisplay(w),
			 XtWindow(w),
			 Prim_BottomShadowGC(w),
			 Prim_TopShadowGC(w),
			 PUSH_GC(w),
			 margin, margin,
			 XtWidth(w) - (margin << 1),
			 XtHeight(w) - (margin << 1),
			 DETAIL_SHADOW_THICKNESS(w),
			 AB_Direction(w));
	}
	else
	{
	    _XmDrawArrow(XtDisplay(w),
			 XtWindow(w),
			 Prim_TopShadowGC(w),
			 Prim_BottomShadowGC(w),
			 PUSH_GC(w),
			 margin, margin,
			 XtWidth(w) - (margin << 1),
			 XtHeight(w) - (margin << 1),
			 DETAIL_SHADOW_THICKNESS(w),
			 AB_Direction(w));
	}
	XFlush(XtDisplay(w));
    }
}

static void
ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmArrowButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "ArmAndActivate\n"));

    /* Arm, Activate, and Disarm now, but draw the disarmed state later */

    Arm(w, event, params, num_params);
    if (AB_ActivateCallback(w))
    {
	XFlush(XtDisplay(w));
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.click_count = 1;
	XtCallCallbackList(w,
			   AB_ActivateCallback(w),
			   (XtPointer)&cbs);
    }
    AB_Armed(w) = False;
    if (AB_DisarmCallback(w))
    {
	XFlush(XtDisplay(w));
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = 1;
	XtCallCallbackList(w,
			   AB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    if (AB_Timer(w) != 0)
    {
	XtRemoveTimeOut(AB_Timer(w));
	AB_Timer(w) = 0;
    }

    AB_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				  ACTIVATE_DELAY, ArmTimeout, (XtPointer)w);
}

static void
EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "EnterWindow\n"));

    if (AB_Armed(w))
    {
	_XmPrimitiveLeave(w, event, NULL, NULL);

	_XmDrawArrow(XtDisplay(w),
		     XtWindow(w),
		     Prim_BottomShadowGC(w),
		     Prim_TopShadowGC(w),
		     PUSH_GC(w),
		     margin, margin,
		     XtWidth(w) - (margin << 1),
		     XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     AB_Direction(w));
    }
}

static void
LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "LeaveWindow\n"));

    if (AB_Armed(w))
    {
	_XmPrimitiveLeave(w, event, params, num_params);

	_XmDrawArrow(XtDisplay(w),
		     XtWindow(w),
		     Prim_TopShadowGC(w),
		     Prim_BottomShadowGC(w),
		     PUSH_GC(w),
		     margin, margin,
		     XtWidth(w) - (margin << 1),
		     XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     AB_Direction(w));
    }
}

static void
MultiArm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MultiArm\n"));

    if (AB_MultiClick(w) == XmMULTICLICK_KEEP)
    {
	Arm(w, event, NULL, NULL);
    }
}

static void
MultiActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int margin;
    XButtonEvent *ev = (XButtonEvent *)event;
    XmPushButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "PushB: MultiClick\n"));

    if (AB_MultiClick(w) == XmMULTICLICK_KEEP)
    {
	Time mctime = XtGetMultiClickTime(XtDisplay(w));

	if ((event->xbutton.time - AB_ArmTimeStamp(w)) < mctime)
	{
	    AB_ClickCount(w)++;
	}
	else
	{
	    AB_ClickCount(w) = 1;
	}

	AB_Armed(w) = False;

	if (XtIsRealized(w))
	{
	    margin = Prim_ShadowThickness(w) + Prim_HighlightThickness(w);
	    _XmDrawArrow(XtDisplay(w),
			 XtWindow(w),
			 Prim_TopShadowGC(w),
			 Prim_BottomShadowGC(w),
			 PUSH_GC(w),
			 margin, margin,
			 XtWidth(w) - (margin << 1),
			 XtHeight(w) - (margin << 1),
			 DETAIL_SHADOW_THICKNESS(w),
			 AB_Direction(w));
	}

	if (ev->type == KeyPress || ev->type == KeyRelease ||
	    ((ev->x >= 0 && ev->x < XtWidth(w)) &&
	     (ev->y >= 0 && ev->y < XtHeight(w))))
	{
	    if (AB_ActivateCallback(w))
	    {
		cbs.reason = XmCR_ACTIVATE;
		cbs.event = event;
		cbs.click_count = AB_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   AB_ActivateCallback(w),
				   (XtPointer)&cbs);
	    }
	}

	Disarm(w, event, params, num_params);
    }
}

Widget
XmCreateArrowButton(Widget parent, char *name, Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name, xmArrowButtonWidgetClass,
			  parent, arglist, argcount);
}

#if XmVERSION > 1
void _XmArrowB_TraitAddCallback(Widget w,
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
