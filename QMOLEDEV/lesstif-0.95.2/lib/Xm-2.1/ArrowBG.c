/**
 *
 * $Id: ArrowBG.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
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

static const char rcsid[] = "$Id: ArrowBG.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ArrowBGP.h>
#include <Xm/RepType.h>

#include <XmI/DebugUtil.h>

/*
 * Experiment
 *
 * Try to add trait stuff by #ifdeffing it.
 */
#if XmVERSION > 1
#include <Xm/TraitP.h>
#include <Xm/ActivatableT.h>

void _XmArrowBG_TraitAddCallback(Widget, XtCallbackProc, XtPointer, Boolean);

static XmActivatableTraitRec _XmArrowBGTraitRec = {
	/* version      */      0,
	/* cb           */      _XmArrowBG_TraitAddCallback
};
#endif

#if XmVERSION > 1
#define PUSH_GC(w)			(XtSensitive(w)		\
					 ? ABG_ArrowGC(w)	\
					 : ABG_InsensitiveGC(w))
#define DETAIL_SHADOW_THICKNESS(w)	ABG_DetailShadowThickness(w)
#else
#define PUSH_GC(w)			NULL
#define DETAIL_SHADOW_THICKNESS(w)	Xm3D_ENHANCE_PIXEL
#endif

/* Forward Declarations */


#if XmVERSION > 1
static void class_initialize(void);
#endif

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget w_new,
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

static void input_dispatch(Widget gadget, XEvent *event, Mask event_mask);

/*
 * Resources for the arrowbuttongadget class
 */
#define Offset(field) XtOffsetOf(XmArrowButtonGadgetRec, arrowbutton.field)
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

static void Arm(Widget w, XEvent *event,
		String *params, Cardinal *num_params);

static void Activate(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void Disarm(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);

static void ArmAndActivate(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);

static void Help(Widget w, XEvent *event,
		 String *params, Cardinal *num_params);

static void EnterWindow(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void LeaveWindow(Widget w, XEvent *event,
			String *params, Cardinal *num_params);


#if 0
static XmBaseClassExtRec _XmArrowBGRectClassExtRec = {
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
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

static XmGadgetClassExtRec _XmArrowBGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ NULL,
    /* display_rect_proc         */ NULL,
};
#endif

XmArrowButtonGadgetClassRec xmArrowButtonGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmGadgetClassRec,
	/* class_name            */ "XmArrowButtonGadget",
	/* widget_size           */ sizeof(XmArrowButtonGadgetRec),
#if XmVERSION > 1
	/* class_initialize      */ class_initialize,
#else
	/* class_initialize      */ NULL,
#endif
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
	/* compress_motion       */ True /*False*/,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ True /*False*/,
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
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL /* query_geometry */,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)NULL /*&_XmArrowBGRectClassExtRec*/
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight,
	/* border_unhighlight */ XmInheritBorderUnhighlight,
	/* arm_and_activate   */ ArmAndActivate,
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ NULL, /* FIX ME */
#if XmVERSION > 1
	/* syn_resources      */ syn_resources,
#else
	/* syn_resources      */ NULL,
#endif
	/* num_syn_resources  */ 0,
	/* cache_part         */ NULL,
	/* extension          */ (XtPointer)NULL /*&_XmArrowBGadgetClassExtRec*/
    },
    /* XmArrowButtonGadget part */
    {
	/* extension */ NULL
    },
};


WidgetClass xmArrowButtonGadgetClass =
			(WidgetClass)&xmArrowButtonGadgetClassRec;

#if XmVERSION > 1
static void
class_initialize(void)
{
#if 0
    _XmArrowBGRectClassExtRec.record_type = XmQmotif;
#endif

    if (! XmeTraitSet((XtPointer)xmArrowButtonGadgetClass, XmQTactivatable,
		(XtPointer)&_XmArrowBGTraitRec)) {
	_XmWarning(NULL,
	    "XmArrowButtonGadget ClassInitialize: XmeTraitSet failed\n");
    }
}
#endif

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmARROW_BUTTON_GADGET_BIT);
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
    values.foreground = XmParentForeground(w);
    values.background = XmParentBackground(w);
    values.fill_style = FillSolid;

    ABG_ArrowGC(w) = XtGetGC(w, mask, &values);
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
    values.foreground = XmParentForeground(w);
    values.background = XmParentBackground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;

    values.stipple =
	XmGetPixmapByDepth(XtScreen(w), "50_foreground", 1, 0, 1);

    ABG_InsensitiveGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    if (!XtIsSubclass(XtParent(new_w), xmManagerWidgetClass))
    {
	_XmError(new_w, "parent should be manager.");
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRArrowDirection),
			     ABG_Direction(new_w), new_w))
	ABG_Direction(new_w) = XmARROW_UP;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick),
			     ABG_MultiClick(new_w), new_w))
	ABG_MultiClick(new_w) = XmMULTICLICK_KEEP;

    if (XtWidth(request) == 0)
    {
	XtWidth(new_w) += 15;
    }

    if (XtHeight(request) == 0)
    {
	XtHeight(new_w) += 15;
    }

    /* Gadget override */
    G_HighlightOnEnter(new_w) = True;

    ABG_Armed(new_w) = False;

    CreateArrowGC(new_w);
    CreateInsensitiveGC(new_w);

    G_EventMask(new_w) = XmARM_EVENT | XmACTIVATE_EVENT | XmENTER_EVENT |
	XmLEAVE_EVENT | XmFOCUS_IN_EVENT |
	XmFOCUS_OUT_EVENT | XmMULTI_ARM_EVENT |
	XmMULTI_ACTIVATE_EVENT | XmHELP_EVENT;

    ABG_Timer(new_w) = 0;
}

static void
destroy(Widget w)
{
    if (ABG_Timer(w) != 0)
    {
	XtRemoveTimeOut(ABG_Timer(w));
	ABG_Timer(w) = 0;
    }

    XtReleaseGC(w, ABG_ArrowGC(w));
    XtReleaseGC(w, ABG_InsensitiveGC(w));
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
	XGCValues	gcv;
	Boolean		refresh_needed = False;

	if (!XmRepTypeValidValue(XmRepTypeGetId(XmRArrowDirection),
			     ABG_Direction(new_w), new_w))
		ABG_Direction(new_w) = ABG_Direction(old);

	if (!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick),
			     ABG_MultiClick(new_w), new_w))
		ABG_MultiClick(new_w) = ABG_MultiClick(old);

	/* See bug #772755 : need to query the old widget's values */
	XGetGCValues(XtDisplayOfObject(old), ABG_ArrowGC(old), GCForeground, &gcv);
	if (XmParentForeground(new_w) != gcv.foreground) {
		XtReleaseGC(new_w, ABG_ArrowGC(new_w));
		XtReleaseGC(new_w, ABG_InsensitiveGC(new_w));

		CreateArrowGC(new_w);
		CreateInsensitiveGC(new_w);

		refresh_needed = True;
	}

	if (ABG_Direction(new_w) != ABG_Direction(old) ||
#if XmVERSION > 1
		ABG_DetailShadowThickness(old) != ABG_DetailShadowThickness(new_w) ||
#endif
		XtSensitive(new_w) != XtSensitive(old))
	{
		refresh_needed = True;
	}

	return refresh_needed;
}

static void
expose(Widget w, XEvent *event, Region region)
{
    Dimension margin = G_ShadowThickness(w) + G_HighlightThickness(w);
    GC myGC;

    /* use the right GC */
    if (XtIsSensitive(w))
    {
	myGC = ABG_ArrowGC(w);
    }
    else
    {
	myGC = ABG_InsensitiveGC(w);
    }

    if (G_Highlighted(w))
    {
	(*GC_BorderHighlight(XtClass(w))) (w);
    }
    else
    {
	(*GC_BorderUnhighlight(XtClass(w))) (w);
    }


    _XmDrawShadows(XtDisplayOfObject(w),
		   XtWindowOfObject(w),
		   XmParentTopShadowGC(w),
		   XmParentBottomShadowGC(w),
		   XtX(w) + G_HighlightThickness(w),
		   XtY(w) + G_HighlightThickness(w),
		   XtWidth(w) - 2 * G_HighlightThickness(w),
		   XtHeight(w) - 2 * G_HighlightThickness(w),
		   G_ShadowThickness(w),
		   XmSHADOW_OUT);

    if (ABG_Armed(w))
    {
	_XmDrawArrow(XtDisplayOfObject(w),
		     XtWindowOfObject(w),
		     XmParentBottomShadowGC(w),
		     XmParentTopShadowGC(w),
		     myGC,
		     XtX(w) + margin, XtY(w) + margin,
		     XtWidth(w) - (margin << 1), XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     ABG_Direction(w));
    }
    else
    {
	_XmDrawArrow(XtDisplayOfObject(w),
		     XtWindowOfObject(w),
		     XmParentTopShadowGC(w),
		     XmParentBottomShadowGC(w),
		     myGC,
		     XtX(w) + margin, XtY(w) + margin,
		     XtWidth(w) - (margin << 1), XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     ABG_Direction(w));
    }
}

#if 0
static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    /* Motif does not have this method */
    answer->request_mode = CWWidth | CWHeight;

    answer->width = XtWidth(w);

    answer->height = XtHeight(w);

    if (((proposed->request_mode & (CWWidth | CWHeight))
	 == (CWWidth | CWHeight)) &&
	proposed->width == answer->width &&
	proposed->height == answer->height)
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

    if (!ABG_Armed(w))
    {
	ABG_Armed(w) = True;

	if (XtIsRealized(w))
	{
	    margin = G_ShadowThickness(w) + G_HighlightThickness(w);
	    _XmDrawArrow(XtDisplayOfObject(w),
			 XtWindowOfObject(w),
			 XmParentBottomShadowGC(w),
			 XmParentTopShadowGC(w),
			 PUSH_GC(w),
			 XtX(w) + margin, XtY(w) + margin,
			 XtWidth(w) - (margin << 1),
			 XtHeight(w) - (margin << 1),
			 DETAIL_SHADOW_THICKNESS(w),
			 ABG_Direction(w));
	}

	if (ABG_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.click_count = ABG_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       ABG_ArmCallback(w),
			       (XtPointer)&cbs);
	}
    }
}

static void
Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int margin;
    XmArrowButtonCallbackStruct cbs;

    if (XtIsRealized(w))
    {
	margin = G_ShadowThickness(w) + G_HighlightThickness(w);
	_XmDrawArrow(XtDisplayOfObject(w),
		     XtWindowOfObject(w),
		     XmParentTopShadowGC(w),
		     XmParentBottomShadowGC(w),
		     PUSH_GC(w),
		     XtX(w) + margin, XtY(w) + margin,
		     XtWidth(w) - (margin << 1),
		     XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     ABG_Direction(w));
    }

    if (ABG_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = ABG_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   ABG_DisarmCallback(w),
			   (XtPointer)&cbs);
    }
    ABG_Armed(w) = False;
}

static void
Activate(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    int margin;
    XButtonEvent *ev = (XButtonEvent *)event;
    XmArrowButtonCallbackStruct cbs;

    if ((ev->x >= XtX(w) && ev->x < XtX(w) + XtWidth(w)) &&
	(ev->y >= XtY(w) && ev->y < XtY(w) + XtHeight(w)))
    {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.click_count = ABG_ClickCount(w);

	if (ABG_ActivateCallback(w))
	{
	    if (XtIsRealized(w))
	    {
		margin = G_ShadowThickness(w) + G_HighlightThickness(w);
		_XmDrawArrow(XtDisplayOfObject(w),
			     XtWindowOfObject(w),
			     XmParentBottomShadowGC(w),
			     XmParentTopShadowGC(w),
			     PUSH_GC(w),
			     XtX(w) + margin, XtY(w) + margin,
			     XtWidth(w) - (margin << 1),
			     XtHeight(w) - (margin << 1),
			     DETAIL_SHADOW_THICKNESS(w),
			     ABG_Direction(w));
		XFlush(XtDisplay(w));
	    }

	    XtCallCallbackList(w,
			       ABG_ActivateCallback(w),
			       (XtPointer)&cbs);
	}
    }

    Disarm(w, event, params, num_params);
}

static void
ArmTimeout(XtPointer data, XtIntervalId *id)
{
    int margin;
    Widget w = (Widget)data;

    DEBUGOUT(_LtDebug(__FILE__, w, "ArmTimeout\n"));

    ABG_Timer(w) = 0;

    if (XtIsRealized(w))
    {
	margin = G_ShadowThickness(w) + G_HighlightThickness(w);
	if (ABG_Armed(w))
	{
	    _XmDrawArrow(XtDisplayOfObject(w),
			 XtWindowOfObject(w),
			 XmParentBottomShadowGC(w),
			 XmParentTopShadowGC(w),
			 PUSH_GC(w),
			 XtX(w) + margin, XtY(w) + margin,
			 XtWidth(w) - (margin << 1),
			 XtHeight(w) - (margin << 1),
			 DETAIL_SHADOW_THICKNESS(w),
			 ABG_Direction(w));
	}
	else
	{
	    _XmDrawArrow(XtDisplayOfObject(w),
			 XtWindowOfObject(w),
			 XmParentTopShadowGC(w),
			 XmParentBottomShadowGC(w),
			 PUSH_GC(w),
			 XtX(w) + margin, XtY(w) + margin,
			 XtWidth(w) - (margin << 1),
			 XtHeight(w) - (margin << 1),
			 DETAIL_SHADOW_THICKNESS(w),
			 ABG_Direction(w));
	}
	XFlush(XtDisplayOfObject(w));
    }
}

static void
ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmArrowButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "ArmAndActivate\n"));

    /* Arm, Activate, and Disarm now, but draw the disarmed state later */

    Arm(w, event, params, num_params);
    ABG_Armed(w) = False;
    if (ABG_ActivateCallback(w))
    {
	XFlush(XtDisplayOfObject(w));
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.click_count = 1;
	XtCallCallbackList(w,
			   ABG_ActivateCallback(w),
			   (XtPointer)&cbs);
    }
    if (ABG_DisarmCallback(w))
    {
	XFlush(XtDisplayOfObject(w));
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = 1;
	XtCallCallbackList(w,
			   ABG_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    if (ABG_Timer(w) != 0)
    {
	XtRemoveTimeOut(ABG_Timer(w));
	ABG_Timer(w) = 0;
    }

    ABG_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				   ACTIVATE_DELAY, ArmTimeout, (XtPointer)w);
}

static void
EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int margin = G_ShadowThickness(w) + G_HighlightThickness(w);

    if (ABG_Armed(w))
    {
	_XmDrawArrow(XtDisplayOfObject(w),
		     XtWindowOfObject(w),
		     XmParentBottomShadowGC(w),
		     XmParentTopShadowGC(w),
		     PUSH_GC(w),
		     XtX(w) + margin, XtY(w) + margin,
		     XtWidth(w) - (margin << 1), XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     ABG_Direction(w));
    }
}

static void
LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int margin = G_ShadowThickness(w) + G_HighlightThickness(w);

    if (ABG_Armed(w))
    {
	_XmDrawArrow(XtDisplayOfObject(w),
		     XtWindowOfObject(w),
		     XmParentTopShadowGC(w),
		     XmParentBottomShadowGC(w),
		     PUSH_GC(w),
		     XtX(w) + margin, XtY(w) + margin,
		     XtWidth(w) - (margin << 1), XtHeight(w) - (margin << 1),
		     DETAIL_SHADOW_THICKNESS(w),
		     ABG_Direction(w));
    }
}

static void
Help(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Widget cur = w;
    XmAnyCallbackStruct cbs;

    cbs.reason = XmCR_HELP;
    cbs.event = event;

    while (cur != NULL)
    {
	if (XtHasCallbacks(w, XmNhelpCallback) == XtCallbackHasSome)
	{
	    XtCallCallbacks(w, XmNhelpCallback, (XtPointer)&cbs);

	    return;
	}

	cur = XtParent(cur);
    }
}

static void
input_dispatch(Widget gadget, XEvent *event, Mask event_mask)
{
    Cardinal num_params = 0;

    switch (event_mask)
    {
    case XmARM_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget,
		 "ArrowButtonGadget got arm event\n"));
	Arm(gadget, event, NULL, &num_params);
	break;

    case XmACTIVATE_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget,
		 "ArrowButtonGadget got activate event\n"));
	ABG_ClickCount(gadget) = 1;
	Activate(gadget, event, NULL, &num_params);
	break;

    case XmENTER_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget,
		 "ArrowButtonGadget got enter event\n"));
	EnterWindow(gadget, event, NULL, &num_params);
	break;

    case XmLEAVE_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget,
		 "ArrowButtonGadget got leave event\n"));
	LeaveWindow(gadget, event, NULL, &num_params);
	break;

    case XmFOCUS_IN_EVENT:
	_XmFocusInGadget(gadget, event, NULL, &num_params);
	break;

    case XmFOCUS_OUT_EVENT:
	_XmFocusOutGadget(gadget, event, NULL, &num_params);
	break;

    case XmHELP_EVENT:
	Help(gadget, event, NULL, &num_params);
	break;

    case XmMULTI_ARM_EVENT:
	if (ABG_MultiClick(gadget) == XmMULTICLICK_KEEP)
	{
	    Arm(gadget, event, NULL, &num_params);
	}
	break;

    case XmMULTI_ACTIVATE_EVENT:
	if (ABG_MultiClick(gadget) == XmMULTICLICK_KEEP)
	{
	    ABG_ClickCount(gadget)++;
	    Activate(gadget, event, NULL, &num_params);
	}
	break;

    default:
	_XmError(gadget, "Unexpected event in ArrowButton gadget\n");
	break;
    }
}

Widget
XmCreateArrowButtonGadget(Widget parent, char *name,
			  Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmArrowButtonGadgetClass,
			  parent,
			  arglist,
			  argcount);
}

#if XmVERSION > 1
void _XmArrowBG_TraitAddCallback(Widget w,
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
