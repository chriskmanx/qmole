/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ScrollBar.c,v 1.3 2004/10/20 19:56:03 dannybackx Exp $
 *
 * Copyright © 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2001, 2004 LessTif Development Team
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

static const char rcsid[] = "$Id: ScrollBar.c,v 1.3 2004/10/20 19:56:03 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>
#include <limits.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/ScaleP.h>
#include <Xm/ScreenP.h>
#include <Xm/TransltnsP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/RepType.h>
#include <Xm/VirtKeys.h>

#include <XmI/DebugUtil.h>

#define DEF_MIN			0
#define DEF_MAX			100
#define DEF_INC			1
#define DEF_PAGE_INC		10
#define DEF_INIT_DELAY		250
#define DEF_REP_DELAY		50
#define DEF_SB_DIM		11
#define DEF_SB_LEN		100
#define ARROW_SPACING		1

#define	MAX_ON_BOTTOM_RIGHT(w)	(SCB_ProcessingDirection(w) & 1)
#define	EXPORTED_VALUE(w)	(MAX_ON_BOTTOM_RIGHT(w) ? SCB_Value(w)	\
				 : SCB_Maximum(w) - (SCB_Value(w) -	\
				   SCB_Minimum(w)) - SCB_SliderSize(w))

/*
 * rules
 *
 * SliderAreaWidth is the width of the trough; this is the width of the trough
 *   area
 * SliderAreaHeight is the height of the trough;  this is the height of the
 *   trough area
 * SliderAreaX is the rightmost starting location of the trough after taking
 *   into account the orientation and the arrows (if any)
 * SliderAreaY is the topmost starting location of the trough after taking
 *   into account the orientation and the arrows (if any)
 * SliderX is the leftmost position of the slider.  Note that some SliderX
 *   values make for impossible Values (rounding errors when orientation is
 *   XmHORIZONTAL).  This is due to the physical size being insufficient to
 *   represent all possible Values.
 * SliderY is the topmost position of the slider.  Note that some SliderY
 *   values make for impossible Values (rounding errors when orientation is
 *   XmVERTICAL).  This is due to the physical size being insufficient to
 *   represent all possible Values.
 * if (orientation is horizontal) then
 *    SliderHeight is the same as the SliderAreaHeight
 *    SliderWidth is the relative to the area width
 * else if (orientation is vertical) then
 *    SliderWidth is the same as the SliderAreaWidth
 *    SliderHeight is the relative to the area height
 * else
 *    error
 * SliderSize (in pixels) is defined as a proportion:
 *   if (orientation is XmHORIZONTAL)
 *     SliderSize is (Maximum - Minimum) / SliderAreaWidth * .1
 *   else if (orientation is XmVERTICAL)
 *     SliderSize is (Maximum - Minimum) / SliderAreaHeight * .1
 * end
 * Value is defined by pos, and vice-versa:
 *   if (orientation is XmHORIZONTAL)
 *     SliderX = (Value - Minimum) * ((SliderAreaWidth - SliderSize) /
 *                                    (Maximum - Minimum)) + SliderAreaX
 *     Value = ((pos - SliderAreaX) / (SliderAreaWidth - SliderSize) *
 *              (Maximum - Minimum)) + Minimum
 *   else if (orientation is XmVERTICAL)
 *     SliderY = (Value - Minimum) * ((SliderAreaHeight - SliderSize) /
 *                                    (Maximum - Minimum)) + SliderAreaY
 *     Value = ((pos - SliderAreaY) / (SliderAreaHeight - SliderSize) *
 *              (Maximum - Minimum)) + Minimum
 *
 * 101295 -- Sigh.  HighlightThickness goes back in.  M*tif behavior
 * observation says it's there, it just doesn't get drawn -- unless the
 * parent is a Scale.
 * 102395 -- Sigh again.  There is always a one pixel separation between the
 * arrow and the slider, regardless of size.
 *
 * 20 Nov 1998: Those M*tif guys are doing something with color defaults.
 * They have static function to default for foreground (sets it to same as
 * the Core background), background (does nothing that I can tell that Core
 * didn't already do), and troughColor (same as select).  So I'll just point
 * troughColor to _XmSelectColorDefault, and leave foreground and background
 * alone.  They cover an insensitive widget with a black stipple with no way
 * to change the color; 2.0 does this in the foreground color instead.  Thus
 * leaving foreground as foreground, and not making it (uselessly) duplicate
 * the background.  - Jamie
 */

/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

#if 0
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *request,
				       XtWidgetGeometry *reply);
#endif

static void expose(Widget w, XEvent *event, Region region);

static void resize(Widget w);

static void realize(Widget w, XtValueMask *values,
		    XSetWindowAttributes *attributes);

static void export_value(Widget sw, int offset, XtArgVal *value);
static XmImportOperator import_value(Widget sw, int offset, XtArgVal *value);

static void defBackgroundFromParent(Widget w, int offset, XrmValue *val);

#if 0
static void scrollbar_highlight(Widget w);

static void scrollbar_unhighlight(Widget w);
#endif

/* timer callback for holding the button in to scroll */

static void buttonTimer(XtPointer clientData, XtIntervalId *id);

/* actually move the slider, and call callbacks */

static void change_value(Widget sw, XEvent *event, int change_type);

/* misc scrollbar routines */

static short _XmScrollBarValueToPos(Widget sw, int value);

static short _XmScrollBarSliderPixSize(Widget sw);

static void draw_slider(Widget sw);

static void move_slider(Widget w, short old_x, short old_y,
			short old_width, short old_height);

static void redraw_arrows(Widget sw, int arrow1, int arrow2);

static void check_constraints(Widget sw, Widget current);

static void _XmScrollBarProcessingDirectionDefault(Widget w,
						   int offset,
						   XrmValue *val);

static void _XmScrollBarTraversalOnDefault(Widget w,
					   int offset,
					   XrmValue *val);

/*
 * Resources for the scrollbar class
 */
#define Offset(field) XtOffsetOf(XmScrollBarRec, scrollBar.field)
#define Prim_Offset(field) XtOffsetOf(XmScrollBarRec, primitive.field)
static XtResource resources[] =
{
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), Prim_Offset(navigation_type),
	XmRImmediate, (XtPointer)XmSTICKY_TAB_GROUP
    },
/* till 27.5.1999: it seems that their ScrollBar
 *				   copies its default background 
 *				   from the parent if this is
 *				   a ScrolledWindow or a MainWindow
 */
	{
	XmNbackground, XmCBackground, XmRPixel,
	sizeof(Pixel), XtOffsetOf(XmScrollBarRec, core.background_pixel),
	XmRCallProc, (XtPointer) defBackgroundFromParent,
	},
    {
	XmNtroughColor, XmCTroughColor, XmRPixel,
	sizeof(Pixel), Offset(trough_color),
	XmRCallProc, (XtPointer)_XmSelectColorDefault
    },
    {
	XmNvalue, XmCValue, XmRInt,
	sizeof(int), Offset(value),
	XmRImmediate, (XtPointer)INT_MAX
    },
    {
	XmNminimum, XmCMinimum, XmRInt,
	sizeof(int), Offset(minimum),
	XtRImmediate, (XtPointer)DEF_MIN
    },
    {
	XmNmaximum, XmCMaximum, XmRInt,
	sizeof(int), Offset(maximum),
	XtRImmediate, (XtPointer)DEF_MAX
    },
    {
	XmNsliderSize, XmCSliderSize, XmRInt,
	sizeof(int), Offset(slider_size),
	XmRImmediate, (XtPointer)INT_MAX
    },
    {
	XmNshowArrows, XmCShowArrows, XmRBoolean,
	sizeof(Boolean), Offset(show_arrows),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmVERTICAL
    },
    {
	XmNprocessingDirection, XmCProcessingDirection, XmRProcessingDirection,
	sizeof(unsigned char), Offset(processing_direction),
	XmRCallProc, (XtPointer)_XmScrollBarProcessingDirectionDefault
    },
    {
	XmNincrement, XmCIncrement, XmRInt,
	sizeof(int), Offset(increment),
	XmRImmediate, (XtPointer)DEF_INC
    },
    {
	XmNpageIncrement, XmCPageIncrement, XmRInt,
	sizeof(int), Offset(page_increment),
	XmRImmediate, (XtPointer)DEF_PAGE_INC
    },
    {
	XmNinitialDelay, XmCInitialDelay, XmRInt,
	sizeof(int), Offset(initial_delay),
	XmRImmediate, (XtPointer)DEF_INIT_DELAY
    },
    {
	XmNrepeatDelay, XmCRepeatDelay, XmRInt,
	sizeof(int), Offset(repeat_delay),
	XmRImmediate, (XtPointer)DEF_REP_DELAY
    },
    {
	XmNvalueChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNincrementCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(increment_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNdecrementCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(decrement_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNpageIncrementCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(page_increment_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNpageDecrementCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(page_decrement_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtoTopCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(to_top_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtoBottomCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(to_bottom_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNdragCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(drag_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), Prim_Offset(traversal_on),
	XmRCallProc, (XtPointer)_XmScrollBarTraversalOnDefault,
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), Prim_Offset(highlight_thickness),
	/* My M*tif manual says that this should be 0 by default */
	XmRImmediate, (XtPointer)0 /* 2 */
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNvalue,
	sizeof(int), Offset(value),
	export_value, import_value
    }
};

static void Select(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);

static void Release(Widget w, XEvent *event,
		    String *params, Cardinal *num_params);

static void Moved(Widget w, XEvent *event,
		  String *params, Cardinal *num_params);

static void TopOrBottom(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void CancelDrag(Widget w, XEvent *event,
		       String *params, Cardinal *num_params);

static void PageDownOrRight(Widget w, XEvent *event,
			    String *params, Cardinal *num_params);

static void PageUpOrLeft(Widget w, XEvent *event,
			 String *params, Cardinal *num_params);

static void IncrementDownOrRight(Widget w, XEvent *event,
				 String *params, Cardinal *num_params);

static void IncrementUpOrLeft(Widget w, XEvent *event,
			      String *params, Cardinal *num_params);

static XtActionsRec actions[] =
{
    {"Select", Select},
    {"Release", Release},
    {"Moved", Moved},
    {"TopOrBottom", TopOrBottom},
    {"IncrementUpOrLeft", IncrementUpOrLeft},
    {"IncrementDownOrRight", IncrementDownOrRight},
    {"PageUpOrLeft", PageUpOrLeft},
    {"PageDownOrRight", PageDownOrRight},
    {"CancelDrag", CancelDrag},
};

/* *INDENT-OFF* */
static XmBaseClassExtRec _XmScrollBarCoreClassExtRec = {
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

#if 0
XmPrimitiveClassExtRec _XmScrollBarPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};
#endif

XmScrollBarClassRec xmScrollBarClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
	/* class_name            */ "XmScrollBar",
	/* widget_size           */ sizeof(XmScrollBarRec),
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
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersionDontCheck,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmScrollBar_defaultTranslations /*NULL*/,
	/* query_geometry        */ NULL /* query_geometry */,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmScrollBarCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight /*scrollbar_highlight*/,
       	/* border_unhighlight    */ XmInheritBorderUnhighlight /*scrollbar_unhighlight*/,
       	/* translations          */ NULL /*_XmScrollBar_defaultTranslations*/,
       	/* arm_and_activate_proc */ NULL,
       	/* synthetic resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)NULL /*&_XmScrollBarPrimClassExtRec*/
    },
    /* ScrollBar Class part */
    {
	/* extension */ NULL
    },
};
/* *INDENT-ON* */

WidgetClass xmScrollBarWidgetClass = (WidgetClass)&xmScrollBarClassRec;

#ifdef SCROLLBAR_VERBOSE
static char *warnings[] =
{
    "Maximum value is less than or equal to minimum value.",
    "Specified slider size is less than 1.",
    "Specified slider size is greater than maximum value minus minimum value.",
    "Specified value is less than minimum value.",
    "Specified value is greater than maximum value minus slider size.",
    "Specified increment is less than 1.",
    "Specified page increment is less than 1.",
    "Specified initial delay is less than 1.",
    "Specified repeat delay is less than 1.",
    "Incorrect processing direction."
};

#define MSG_MAX_LTE_MIN		warnings[0]
#define MSG_SLSZ_LT_1		warnings[1]
#define MSG_SLSZ_GT_MAXMIN	warnings[2]
#define MSG_VAL_LT_MIN		warnings[3]
#define MSG_VAL_GT_MAXSLSZ	warnings[4]
#define MSG_INC_LT_1		warnings[5]
#define MSG_PGINC_LT_1		warnings[6]
#define MSG_IDLAY_LT_1		warnings[7]
#define MSG_RDLAY_LT_1		warnings[8]
#define MSG_PROCDIR		warnings[9]
#endif

/* Flag field bits (difference from before):
 *
 * 0x01	Set when first clicking in trough/arrows, but cleared when repeat
 *	kicks in (otherwise not cleared until repeat is performed).
 * 0x02 Set by import_value, so set_values can know if it was explicitly set.
 * 0x04	Multi-click: set when a second click comes in the first timeout
 *	(as defined by bit 0x01).
 * 0x08	\ These two bits are strange; they appear to be mutually exclusive,
 * 0x10	/ with one always on and the other off, set by initialize() and never
 *	  changed after that.  0x08 indicates "regular" scroll bars, or a
 *	  horizontal Scale with processingDirection max_on_right; 0x10 is all
 *	  other combinations.  This means that orientation, processing-
 *	  Direction, and scale status are all part of the flags, but not in a
 *	  meaningful way, and to a use I can't guess.  I'm leaving them both
 *	  off, and someone else is welcome to find out what function they have.
 * 0x20	Always on.
 * 0x40 Set when a mouse button is down, dragging or on arrows/trough.
 * 0x80 Always off.
 */
#define FLG_FIRST_PAGE_MOVE	0x01
#define	FLG_IMPORT_VALUE	0x02
#define	FLG_MULTI_CLICK		0x04
#define	FLG_INITIALIZE		0x20
#define FLG_DRAG		0x40

static void
class_initialize(void)
{
    _XmScrollBarCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmSCROLL_BAR_BIT);
}

static unsigned char
default_processing_direction(Widget w)
{
    return SCB_Orientation(w) == XmVERTICAL
	? XmMAX_ON_BOTTOM
	/* The Reference Guide for 1.2 says it depends on the parent's
	 * stringDirection, but this seems not to be the case.
	 */
	: XmIsManager(XtParent(w)) &&
	  MGR_StringDirection(XtParent(w)) == XmSTRING_DIRECTION_R_TO_L
	? XmMAX_ON_LEFT
	: XmMAX_ON_RIGHT;
}

static void
CreateForegroundGC(Widget w)
{
    XtGCMask mask;
    XGCValues values;

    mask = GCFunction | GCPlaneMask | GCSubwindowMode | GCGraphicsExposures |
	GCForeground | GCBackground | GCFillStyle;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = XtBackground(w);
    values.background = Prim_Foreground(w);
    values.fill_style = FillSolid;

    SCB_ForegroundGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateUnavailableGC(Widget w)
{
    XtGCMask mask;
    XGCValues values;

    mask = GCFunction | GCPlaneMask | GCSubwindowMode | GCGraphicsExposures |
	GCForeground | GCBackground | GCFillStyle | GCStipple |
	GCTileStipXOrigin | GCTileStipYOrigin;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;
    values.stipple = XmGetPixmapByDepth(XtScreen(w),
					"50_foreground",
					1, 0, 1);

    SCB_UnavailableGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s(%i) - initialize: %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    /*
     * First check if the reptypes are valid
     */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmROrientation),
			 SCB_Orientation(new_w),
			 new_w))
    {
	SCB_Orientation(new_w) = XmVERTICAL;
    }
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRProcessingDirection),
			     SCB_ProcessingDirection(new_w),
			     new_w))
    {
	SCB_ProcessingDirection(new_w) = default_processing_direction(new_w);
    }

    /*
     * we override the Primitive width/height if the user didn't specify one
     */

    if (SCB_Orientation(new_w) == XmHORIZONTAL)
    {
	if (XtWidth(request) == 0)
	{
	    XtWidth(new_w) += DEF_SB_LEN;
	}

	if (XtHeight(request) == 0)
	{
	    XtHeight(new_w) += DEF_SB_DIM;
	}

	SCB_Arrow1Orientation(new_w) = XmARROW_LEFT;
	SCB_Arrow2Orientation(new_w) = XmARROW_RIGHT;
    }
    else
    {
	if (XtHeight(request) == 0)
	{
	    XtHeight(new_w) += DEF_SB_LEN;
	}

	if (XtWidth(request) == 0)
	{
	    XtWidth(new_w) += DEF_SB_DIM;
	}

	SCB_Arrow1Orientation(new_w) = XmARROW_UP;
	SCB_Arrow2Orientation(new_w) = XmARROW_DOWN;
    }

    SCB_Arrow1Selected(new_w) = False;
    SCB_Arrow2Selected(new_w) = False;
    SCB_Flags(new_w) = FLG_INITIALIZE;
    SCB_SlidingOn(new_w) = False;
    SCB_EtchedSlider(new_w) = False;

    SCB_Pixmap(new_w) = None;
    SCB_Timer(new_w) = 0;

    CreateForegroundGC(new_w);
    CreateUnavailableGC(new_w);

    check_constraints(new_w, NULL);
    SCB_Value(new_w) = EXPORTED_VALUE(new_w);

    SCB_SavedValue(new_w) = SCB_Value(new_w);
    (*XtClass(new_w)->core_class.resize)(new_w);
}

static void
check_constraints(Widget sw, Widget current)
{
    DEBUGOUT(_LtDebug(__FILE__, sw,
	     "Specified scrollbar values: min=%d, max=%d, val=%d, sl_size=%d\n"
		      " inc=%d, p_inc=%d, p_dir=%d\n",
		      SCB_Minimum(sw), SCB_Maximum(sw), SCB_Value(sw),
		      SCB_SliderSize(sw), SCB_Increment(sw),
		      SCB_PageIncrement(sw),
		      (int)SCB_ProcessingDirection(sw)));

    if (SCB_ProcessingDirection(sw) >> 1 != SCB_Orientation(sw) - 1)
    {
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_PROCDIR);
#endif
	SCB_ProcessingDirection(sw) = current
	    ? (SCB_ProcessingDirection(current) & 1) |
	      ((SCB_Orientation(sw) - 1) << 1)
	    : default_processing_direction(sw);
    }

    if (SCB_Maximum(sw) <= SCB_Minimum(sw))
    {
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_MAX_LTE_MIN);
#endif
	SCB_Minimum(sw) = current ? SCB_Minimum(current) : DEF_MIN;
	SCB_Maximum(sw) = current ? SCB_Maximum(current) : DEF_MAX;
    }

    if (!current && SCB_SliderSize(sw) == INT_MAX)
    {
	SCB_SliderSize(sw) = (SCB_Maximum(sw) - SCB_Minimum(sw)) / 10;
	if (SCB_SliderSize(sw) < 1)
	    SCB_SliderSize(sw) = 1;
    }

    else if (SCB_SliderSize(sw) < 1)
    {
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_SLSZ_LT_1);
#endif
	if (current)
	{
	    SCB_SliderSize(sw) = SCB_SliderSize(current);
	    if (SCB_SliderSize(sw) > SCB_Maximum(sw) - SCB_Minimum(sw))
		SCB_SliderSize(sw) = SCB_Maximum(sw) - SCB_Minimum(sw);
	}
	else
	    SCB_SliderSize(sw) = 1;
    }

    else if (SCB_SliderSize(sw) > SCB_Maximum(sw) - SCB_Minimum(sw))
    {
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_SLSZ_GT_MAXMIN);
#endif
	if (current)
	{
	    SCB_SliderSize(sw) = SCB_SliderSize(current);
	    if (SCB_SliderSize(sw) > SCB_Maximum(sw) - SCB_Minimum(sw))
		SCB_SliderSize(sw) = SCB_Maximum(sw) - SCB_Minimum(sw);
	}
	else
	    SCB_SliderSize(sw) = SCB_Maximum(sw) - SCB_Minimum(sw);
    }

    if (!current && SCB_Value(sw) == INT_MAX)
    {
	SCB_Value(sw) = SCB_Minimum(sw) > 0
	    ? SCB_Minimum(sw)
	    : SCB_Maximum(sw) - SCB_SliderSize(sw) < 0
	    ? SCB_Maximum(sw) - SCB_SliderSize(sw) : 0;
    }

    else if (SCB_Value(sw) < SCB_Minimum(sw))
    {
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_VAL_LT_MIN);
#endif
	SCB_Value(sw) = SCB_Minimum(sw);
    }

    else if (SCB_Value(sw) > SCB_Maximum(sw) - SCB_SliderSize(sw))
    {
	/* For some reason new widgets go minimum
	 * while old ones go maximum.
	 */
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_VAL_GT_MAXSLSZ);
#endif
	SCB_Value(sw) = current
	    ? SCB_Maximum(sw) - SCB_SliderSize(sw) : SCB_Minimum(sw);
    }

    if (SCB_Increment(sw) < 1)
    {
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_INC_LT_1);
#endif
	SCB_Increment(sw) = current ? SCB_Increment(current) : DEF_INC;
    }

    if (SCB_PageIncrement(sw) < 1)
    {
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_PGINC_LT_1);
#endif
	SCB_PageIncrement(sw) =
	    current ? SCB_PageIncrement(current) : DEF_PAGE_INC;
    }

    if (SCB_InitialDelay(sw) < 1)
    {
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_IDLAY_LT_1);
#endif
	SCB_InitialDelay(sw) =
	    current ? SCB_InitialDelay(current) : DEF_INIT_DELAY;
    }

    if (SCB_RepeatDelay(sw) < 1)
    {
#ifdef	SCROLLBAR_VERBOSE
	_XmWarning(sw, MSG_RDLAY_LT_1);
#endif
	SCB_RepeatDelay(sw) = current ? SCB_RepeatDelay(current) : 75;
	/* Yes, 75 is not equal to the default. */
    }

    DEBUGOUT(_LtDebug(__FILE__, sw,
		   "Got scrollbar values: min=%d, max=%d, val=%d, sl_size=%d\n"
		      " inc=%d, p_inc=%d, p_dir=%d\n",
		      SCB_Minimum(sw), SCB_Maximum(sw), SCB_Value(sw),
		      SCB_SliderSize(sw), SCB_Increment(sw),
		      SCB_PageIncrement(sw),
		      (int)SCB_ProcessingDirection(sw)));
}

static void
destroy(Widget w)
{
    if (SCB_Pixmap(w) != None)
    {
	_XmFreeScratchPixmap((XmScreen)XmGetXmScreen(XtScreen(w)),
			     SCB_Pixmap(w));
    }

    if (SCB_Timer(w)) {
	XtRemoveTimeOut(SCB_Timer(w));
	SCB_Timer(w) = 0;
    }

    XtReleaseGC(w, SCB_UnavailableGC(w));
    XtReleaseGC(w, SCB_ForegroundGC(w));
}

#define	CNE(x)	(x(new_w) != x(current))

static Boolean
set_values(Widget current, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean redraw = False;

	/* till 27.5.1999: slider pixmap must be redrawn in many cases;
	 *				   introduced a flag.
	 *				   Start with a couple of color diffs...
	 */
	Boolean redrawSlider = CNE(Prim_Foreground) || CNE(Prim_TopShadowColor) ||
		CNE(Prim_BottomShadowColor);

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s(%d) - set_values: %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(current), XtY(current),
		      XtWidth(current), XtHeight(current),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmROrientation),
			 SCB_Orientation(new_w),
			 new_w))
    {
	SCB_Orientation(new_w) = SCB_Orientation(current);
	if (!CNE(SCB_ProcessingDirection))
	{
	    SCB_ProcessingDirection(new_w) =
		default_processing_direction(new_w);
	}
    }
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRProcessingDirection),
			     SCB_ProcessingDirection(new_w),
			     new_w))
    {
	SCB_ProcessingDirection(new_w) = CNE(SCB_Orientation)
	    ? default_processing_direction(new_w)
	    : SCB_ProcessingDirection(current);
    }

    /* If the orientation changed, an implicit processingDirection
     * change may be in the works.  Ditto for ProcessingDirection and value.
     */
    if (CNE(SCB_Orientation) && !CNE(SCB_ProcessingDirection))
    {
	SCB_ProcessingDirection(new_w) ^= 2;
    }

    check_constraints(new_w, current);

    if ((SCB_Flags(new_w) & FLG_IMPORT_VALUE)
	? !MAX_ON_BOTTOM_RIGHT(new_w)
	: ((SCB_ProcessingDirection(new_w) & 1) ^
	   (SCB_ProcessingDirection(current) & 1)))
    {
	SCB_Value(new_w) = SCB_Maximum(new_w) -
	    (SCB_Value(new_w) - SCB_Minimum(new_w)) - SCB_SliderSize(new_w);
    }
    SCB_Flags(new_w) &= ~FLG_IMPORT_VALUE;

    if (CNE(XtIsSensitive) || CNE(SCB_TroughColor))
    {
	redraw = True;
    }

    /* Reset the window background if:
     * 1. The background or backgroundPixmap changed, to undo what Core did.
     * 2. The troughColor changed, to do what Core didn't.
     */
    if ((CNE(XtBackground) || CNE(CoreBackgroundPixmap) ||
	 CNE(SCB_TroughColor))
	&& XtIsRealized(new_w))
    {
	if (SCB_TroughColor(new_w) != XmUNSPECIFIED_PIXMAP)
		XSetWindowBackground(XtDisplay(new_w), XtWindow(new_w),
			     SCB_TroughColor(new_w));
	if (CoreBackgroundPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
		XSetWindowBackgroundPixmap(XtDisplay(new_w), XtWindow(new_w),
			     CoreBackgroundPixmap(new_w));
    }

    if (CNE(XtBackground))
    {
	XtReleaseGC(new_w, SCB_ForegroundGC(new_w));
	CreateForegroundGC(new_w);
	redrawSlider = redraw = True;
    }
    if (CNE(Prim_Foreground))
    {
	XtReleaseGC(new_w, SCB_UnavailableGC(new_w));
	CreateUnavailableGC(new_w);
	redraw |= !XtIsSensitive(new_w);
    }

    if (CNE(SCB_Orientation))
    {
	if (SCB_Orientation(new_w) == XmHORIZONTAL)
	{
	    SCB_Arrow1Orientation(new_w) = XmARROW_LEFT;
	    SCB_Arrow2Orientation(new_w) = XmARROW_RIGHT;
	}
	else /* vertical */
	{
	    SCB_Arrow1Orientation(new_w) = XmARROW_UP;
	    SCB_Arrow2Orientation(new_w) = XmARROW_DOWN;
	}
    }

    /* Refigure the internal geometry if something internal changed */
    if (Prim_HighlightThickness(current) + Prim_ShadowThickness(current) !=
	Prim_HighlightThickness(new_w) + Prim_ShadowThickness(new_w) ||
	CNE(SCB_ShowArrows) || CNE(SCB_Orientation))
    {
	Dimension new_width, new_height;

	new_width = XtWidth(new_w);
	new_height = XtHeight(new_w);
	XtWidth(new_w) = XtWidth(current);
	XtHeight(new_w) = XtHeight(current);
	(*XtClass(new_w)->core_class.resize)(new_w);
	XtWidth(new_w) = new_width;
	XtHeight(new_w) = new_height;
	redraw = True;
    }

    /* Move or change the slider, a subset of the work resize does.
     * If that's the only visible change, draw it now.
     */
    else if (CNE(SCB_Minimum) || CNE(SCB_Maximum) || CNE(SCB_Value) ||
	     CNE(SCB_ProcessingDirection) || CNE(SCB_SliderSize))
    {
	short old_slider_dim, old_slider_pos;

	if (SCB_Orientation(new_w) == XmHORIZONTAL)
	{
	    old_slider_dim = SCB_SliderWidth(new_w);
	    old_slider_pos = SCB_SliderX(new_w);
	    SCB_SliderWidth(new_w) = _XmScrollBarSliderPixSize(new_w);
	    SCB_SliderX(new_w) =
		_XmScrollBarValueToPos(new_w, SCB_Value(new_w));
	    if (old_slider_dim != SCB_SliderWidth(new_w))
		{
			redrawSlider = True;
	    }
	    if (!redraw)
	    {
		if ( redrawSlider )
		{
			draw_slider(new_w);
			redrawSlider = False;
		}
		move_slider(new_w, old_slider_pos, SCB_SliderY(new_w),
			    old_slider_dim, SCB_SliderHeight(new_w));
	    }
	}
	else
	{
	    old_slider_dim = SCB_SliderHeight(new_w);
	    old_slider_pos = SCB_SliderY(new_w);
	    SCB_SliderHeight(new_w) = _XmScrollBarSliderPixSize(new_w);
	    SCB_SliderY(new_w) =
		_XmScrollBarValueToPos(new_w, SCB_Value(new_w));
	    if (old_slider_dim != SCB_SliderHeight(new_w))
	    {
			redrawSlider = True;
	    }
	    if (!redraw)
	    {
		if ( redrawSlider )
		{
			draw_slider(new_w);
			redrawSlider = False;
		}
		move_slider(new_w, SCB_SliderX(new_w), old_slider_pos,
			    SCB_SliderWidth(new_w), old_slider_dim);
	    }
	}
    }

	if (redrawSlider)
	{
		draw_slider(new_w);
	}
    return redraw;
}

#undef CNE

static void
redraw_arrows(Widget sw, int arrow1, int arrow2)
{
    GC fggc;

    if (!SCB_ShowArrows(sw))
    {
	return;
    }

    /* On updates (not expose) we don't usually need the foreground */
    fggc = (arrow1 ^ arrow2) && Prim_ShadowThickness(sw) != 1
	? NULL
	: SCB_ForegroundGC(sw);

    if (arrow1)
    {
	if (SCB_Arrow1Selected(sw))
	{
	    _XmDrawArrow(XtDisplay(sw), XtWindow(sw),
			 Prim_BottomShadowGC(sw), Prim_TopShadowGC(sw), fggc,
			 SCB_Arrow1X(sw) - 1, SCB_Arrow1Y(sw) - 1,
			 SCB_ArrowWidth(sw) + 2, SCB_ArrowHeight(sw) + 2,
			 Prim_ShadowThickness(sw), SCB_Arrow1Orientation(sw));
	}
	else
	{
	    _XmDrawArrow(XtDisplay(sw), XtWindow(sw),
			 Prim_TopShadowGC(sw), Prim_BottomShadowGC(sw), fggc,
			 SCB_Arrow1X(sw) - 1, SCB_Arrow1Y(sw) - 1,
			 SCB_ArrowWidth(sw) + 2, SCB_ArrowHeight(sw) + 2,
			 Prim_ShadowThickness(sw), SCB_Arrow1Orientation(sw));
	}
    }

    if (arrow2)
    {
	if (SCB_Arrow2Selected(sw))
	{
	    _XmDrawArrow(XtDisplay(sw), XtWindow(sw),
			 Prim_BottomShadowGC(sw), Prim_TopShadowGC(sw), fggc,
			 SCB_Arrow2X(sw) - 1, SCB_Arrow2Y(sw) - 1,
			 SCB_ArrowWidth(sw) + 2, SCB_ArrowHeight(sw) + 2,
			 Prim_ShadowThickness(sw), SCB_Arrow2Orientation(sw));
	}
	else
	{
	    _XmDrawArrow(XtDisplay(sw), XtWindow(sw),
			 Prim_TopShadowGC(sw), Prim_BottomShadowGC(sw), fggc,
			 SCB_Arrow2X(sw) - 1, SCB_Arrow2Y(sw) - 1,
			 SCB_ArrowWidth(sw) + 2, SCB_ArrowHeight(sw) + 2,
			 Prim_ShadowThickness(sw), SCB_Arrow2Orientation(sw));
	}
    }
}

static void
expose(Widget w, XEvent *event, Region region)
{
    if (XtIsRealized(w))
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       Prim_HighlightThickness(w),
		       Prim_HighlightThickness(w),
		       XtWidth(w) - (Prim_HighlightThickness(w) << 1),
		       XtHeight(w) - (Prim_HighlightThickness(w) << 1),
		       Prim_ShadowThickness(w),
		       XmSHADOW_IN);

	if (Prim_Highlighted(w))
	    (*PrimC_BorderHighlight(XtClass(w))) (w);
	else
	    (*PrimC_BorderUnhighlight(XtClass(w))) (w);

	redraw_arrows(w, True, True);

	XCopyArea(XtDisplay(w), SCB_Pixmap(w), XtWindow(w),
	      SCB_ForegroundGC(w),
	      0, 0,
	      SCB_SliderWidth(w),
	      SCB_SliderHeight(w),
	      SCB_SliderX(w),
	      SCB_SliderY(w));

	if (!XtSensitive(w))
	    XFillRectangle(XtDisplay(w),
			   XtWindow(w),
			   SCB_UnavailableGC(w),
			   Prim_HighlightThickness(w),
			   Prim_HighlightThickness(w),
			   XtWidth(w) - (Prim_HighlightThickness(w) << 1),
			   XtHeight(w) - (Prim_HighlightThickness(w) << 1));
    }
}

static void
resize(Widget sw)
{
    int margin = Prim_ShadowThickness(sw) + Prim_HighlightThickness(sw);
    int inner_width = XtWidth(sw) - (margin << 1);
    int inner_height = XtHeight(sw) - (margin << 1);
    short arrow_dim;

    if (SCB_Orientation(sw) == XmHORIZONTAL)
    {
	if (SCB_ShowArrows(sw))
	{
	    /* Don't change these two lines lightly. Remember that a + b/2
	     * need not equal (2*a + b)/2, depending on the rounding in case
	     * of negative dividends. -- PvH
	     */
	    arrow_dim = (short)(XtWidth(sw) - (short)(MIN_SLIDER_LENGTH +
		((ARROW_SPACING + margin) << 1))) / 2;
	    if (arrow_dim > inner_height)
		arrow_dim = inner_height;

	    SCB_ArrowWidth(sw) = arrow_dim > 1 ? arrow_dim : 1;
	    SCB_ArrowHeight(sw) = inner_height > 1 ? inner_height : 1;

	    SCB_SliderAreaX(sw) = SCB_ArrowWidth(sw) + margin + ARROW_SPACING;
	    SCB_SliderAreaY(sw) = margin;

	    SCB_SliderAreaWidth(sw) = XtWidth(sw) - (SCB_SliderAreaX(sw) << 1);
	    SCB_SliderAreaHeight(sw) = SCB_ArrowHeight(sw);

	    SCB_Arrow1X(sw) = SCB_Arrow1Y(sw) = SCB_Arrow2Y(sw) = margin;
	    SCB_Arrow2X(sw) = XtWidth(sw) - (margin + arrow_dim);
	}
	else
	    /* ! showArrows */
	{
	    SCB_ArrowWidth(sw) = 0;
	    SCB_ArrowHeight(sw) = 0;

	    SCB_SliderAreaWidth(sw) = inner_width;
	    if (SCB_SliderAreaWidth(sw) < 1)
		SCB_SliderAreaWidth(sw) = 1;
	    SCB_SliderAreaHeight(sw) = inner_height;
	    if (SCB_SliderAreaHeight(sw) < 1)
		SCB_SliderAreaHeight(sw) = 1;

	    SCB_SliderAreaX(sw) = SCB_SliderAreaY(sw) = margin;

	    SCB_Arrow1X(sw) = SCB_Arrow1Y(sw) =
		SCB_Arrow2X(sw) = SCB_Arrow2Y(sw) = 0;
	}

	SCB_SliderWidth(sw) = _XmScrollBarSliderPixSize(sw);
	SCB_SliderHeight(sw) = SCB_SliderAreaHeight(sw);
	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	SCB_SliderY(sw) = SCB_SliderAreaY(sw);
    }
    else
	/* vertical */
    {
	if (SCB_ShowArrows(sw))
	{
	    /* Don't change these two lines lightly. Remember that a + b/2
	     * need not equal (2*a + b)/2, depending on the rounding in case
	     * of negative dividends. -- PvH
	     */
	    arrow_dim = (short)(XtHeight(sw) - (short)(MIN_SLIDER_LENGTH +
		((ARROW_SPACING + margin) << 1))) / 2;
	    if (arrow_dim > inner_width)
		arrow_dim = inner_width;

	    SCB_ArrowWidth(sw) = inner_width > 1 ? inner_width : 1;
	    SCB_ArrowHeight(sw) = arrow_dim > 1 ? arrow_dim : 1;

	    SCB_SliderAreaX(sw) = margin;
	    SCB_SliderAreaY(sw) = SCB_ArrowHeight(sw) + margin + ARROW_SPACING;

	    SCB_SliderAreaWidth(sw) = SCB_ArrowWidth(sw);
	    SCB_SliderAreaHeight(sw) =
		XtHeight(sw) - (SCB_SliderAreaY(sw) << 1);

	    SCB_Arrow1X(sw) = SCB_Arrow1Y(sw) = SCB_Arrow2X(sw) = margin;
	    SCB_Arrow2Y(sw) = XtHeight(sw) - (margin + arrow_dim);
	}
	else
	    /* ! showArrows */
	{
	    SCB_ArrowWidth(sw) = 0;
	    SCB_ArrowHeight(sw) = 0;

	    SCB_SliderAreaWidth(sw) = inner_width;
	    if (SCB_SliderAreaWidth(sw) < 1)
		SCB_SliderAreaWidth(sw) = 1;
	    SCB_SliderAreaHeight(sw) = inner_height;
	    if (SCB_SliderAreaHeight(sw) < 1)
		SCB_SliderAreaHeight(sw) = 1;

	    SCB_SliderAreaX(sw) = SCB_SliderAreaY(sw) = margin;

	    SCB_Arrow1X(sw) = SCB_Arrow1Y(sw) =
		SCB_Arrow2X(sw) = SCB_Arrow2Y(sw) = 0;
	}

	SCB_SliderWidth(sw) = SCB_SliderAreaWidth(sw);
	SCB_SliderHeight(sw) = _XmScrollBarSliderPixSize(sw);
	SCB_SliderX(sw) = SCB_SliderAreaX(sw);
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
    }
    draw_slider(sw);
}

static void
realize(Widget w, XtValueMask *values, XSetWindowAttributes *attributes)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:realize(%d) %dx%d\n",
    	__FILE__, __LINE__,
    	XtWidth(w), XtHeight(w)));

    if (!XtWidth(w))
	XtWidth(w) = 1;
    if (!XtHeight(w))
	XtHeight(w) = 1;

    *values |= CWBackPixel | CWBitGravity;
    attributes->background_pixel = SCB_TroughColor(w);
    attributes->bit_gravity = ForgetGravity;

    XtCreateWindow(w, (unsigned int)InputOutput,
		   (Visual *)CopyFromParent, *values, attributes);
}

#if 0
static XtGeometryResult
query_geometry(Widget w,
	       XtWidgetGeometry *request,
	       XtWidgetGeometry *reply)
{
    /* Motif does not have this method */
    XtGeometryResult result = XtGeometryYes;
    Dimension width = XtWidth(w);
    Dimension height = XtHeight(w);

    request->request_mode &= CWWidth | CWHeight;

    if (request->request_mode == 0)
    {
	return result;
    }

    if (request->request_mode & CWWidth)
    {
	if (request->width < width)
	{
	    result = XtGeometryAlmost;
	    reply->width = width;
	    reply->request_mode |= CWWidth;
	}
    }
    if (request->request_mode & CWHeight)
    {
	if (request->height < height)
	{
	    result = XtGeometryAlmost;
	    reply->height = height;
	    reply->request_mode |= CWHeight;
	}
    }

    return result;
}
#endif

#if 0
/*
 * Highlighting should be enabled/disabled elsewhere based on widget
 * type and parent widget type.
 * The return statements below make it impossible for scrollbars in an
 * XmAUTOMATIC scrolled window to highlight.
 */
static void
scrollbar_unhighlight(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ScrollBarUnHighlight\n"));

#define superclass (&xmPrimitiveClassRec)
    (*superclass->primitive_class.border_unhighlight) (w);
#undef superclass
}

static void
scrollbar_highlight(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ScrollBarHighlight\n"));

#define superclass (&xmPrimitiveClassRec)
    (*superclass->primitive_class.border_highlight) (w);
#undef superclass
}
#endif

static void
export_value(Widget sw,
	     int offset,
	     XtArgVal *value)
{
    *value = (XtArgVal)EXPORTED_VALUE(sw);
}

static XmImportOperator
import_value(Widget sw,
	     int offset,
	     XtArgVal *value)
{
    SCB_Flags(sw) |= FLG_IMPORT_VALUE;
    return XmSYNTHETIC_NONE;
}

Widget
XmCreateScrollBar(Widget parent,
		  char *name,
		  Arg *arglist,
		  Cardinal argCount)
{
    return XtCreateWidget(name,
			  xmScrollBarWidgetClass,
			  parent,
			  arglist,
			  argCount);
}

void
XmScrollBarGetValues(Widget w,
		     int *value_return,
		     int *slider_size_return,
		     int *increment_return,
		     int *page_increment_return)
{
    if (value_return)
	*value_return = EXPORTED_VALUE(w);
    if (slider_size_return)
	*slider_size_return = SCB_SliderSize(w);
    if (increment_return)
	*increment_return = SCB_Increment(w);
    if (page_increment_return)
	*page_increment_return = SCB_PageIncrement(w);
}

void
XmScrollBarSetValues(Widget sw,
		     int value,
		     int slider_size,
		     int increment,
		     int page_increment,
		     Boolean notify)
{
    XmScrollBarRec current;

    current.scrollBar = ((XmScrollBarWidget)sw)->scrollBar;

    if (slider_size)
    {
	SCB_SliderSize(sw) = slider_size;
    }

    SCB_Value(sw) = value;
    SCB_Value(sw) = EXPORTED_VALUE(sw);

    if (increment)
    {
	SCB_Increment(sw) = increment;
    }

    if (page_increment)
    {
	SCB_PageIncrement(sw) = page_increment;
    }

    check_constraints(sw, (Widget)&current);

    if (SCB_Orientation(sw) == XmVERTICAL)
    {
	if (SCB_SliderSize(sw) != SCB_SliderSize(&current))
	    SCB_SliderHeight(sw) = _XmScrollBarSliderPixSize(sw);
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	if (SCB_SliderHeight(sw) != SCB_SliderHeight(&current))
	    draw_slider(sw);
    }
    else
    {
	if (SCB_SliderSize(sw) != SCB_SliderSize(&current))
	    SCB_SliderWidth(sw) = _XmScrollBarSliderPixSize(sw);
	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	if (SCB_SliderWidth(sw) != SCB_SliderWidth(&current))
	    draw_slider(sw);
    }
    move_slider(sw, SCB_SliderX(&current), SCB_SliderY(&current),
		SCB_SliderWidth(&current), SCB_SliderHeight(&current));

    if (notify && SCB_Value(sw) != SCB_Value(&current))
    {
	XmScrollBarCallbackStruct cbs;

	cbs.event = NULL;
	cbs.value = EXPORTED_VALUE(sw);
	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.pixel = 0;

	XFlush(XtDisplay(sw));
	XtCallCallbackList(sw, SCB_ValueChangedCallback(sw), &cbs);
    }
}

static void
move_slider(Widget sw,
	    short old_x, short old_y,
	    short old_width, short old_height)
{
    /* Only move the slider if it really moved */

    if (XtIsRealized(sw) &&
	(old_x != SCB_SliderX(sw) || old_width != SCB_SliderWidth(sw) ||
	 old_y != SCB_SliderY(sw) || old_height != SCB_SliderHeight(sw)))
    {
	/* Clear what was there but no longer is */

	if (old_x < SCB_SliderX(sw))
	{
	    XClearArea(XtDisplay(sw), XtWindow(sw),
		       old_x, old_y,
		       old_width <= SCB_SliderX(sw) - old_x
			? old_width : SCB_SliderX(sw) - old_x,
		       old_height,
		       False);
	}

	if (old_y < SCB_SliderY(sw))
	{
	    XClearArea(XtDisplay(sw), XtWindow(sw),
		       old_x, old_y,
		       old_width,
		       old_height <= SCB_SliderY(sw) - old_y
			? old_height : SCB_SliderY(sw) - old_y,
		       False);
	}

	if (old_x >= SCB_SliderX(sw) + SCB_SliderWidth(sw))
	{
	    XClearArea(XtDisplay(sw), XtWindow(sw),
		       old_x, old_y,
		       old_width, old_height,
		       False);
	}
	else if (old_x + old_width > SCB_SliderX(sw) + SCB_SliderWidth(sw))
	{
	    XClearArea(XtDisplay(sw), XtWindow(sw),
		       SCB_SliderX(sw) + SCB_SliderWidth(sw),
		       old_y,
		       (old_x + old_width) -
			(SCB_SliderX(sw) + SCB_SliderWidth(sw)),
		       old_height,
		       False);
	}

	if (old_y >= SCB_SliderY(sw) + SCB_SliderHeight(sw))
	{
	    XClearArea(XtDisplay(sw), XtWindow(sw),
		       old_x, old_y,
		       old_width, old_height,
		       False);
	}
	else if (old_y + old_height > SCB_SliderY(sw) + SCB_SliderHeight(sw))
	{
	    XClearArea(XtDisplay(sw), XtWindow(sw),
		       old_x,
		       SCB_SliderY(sw) + SCB_SliderHeight(sw),
		       old_width,
		       (old_y + old_height) -
			(SCB_SliderY(sw) + SCB_SliderHeight(sw)),
		       False);
	}

	/* Copy the slider pixmap */

	XCopyArea(XtDisplay(sw), SCB_Pixmap(sw), XtWindow(sw),
		  SCB_ForegroundGC(sw),
		  0, 0,
		  SCB_SliderWidth(sw), SCB_SliderHeight(sw),
		  SCB_SliderX(sw), SCB_SliderY(sw));

	if (!XtSensitive(sw))
	{
	    /* Re-cover an insensitive slider */

	    if (old_x > SCB_SliderX(sw))
	    {
		old_width += old_x - SCB_SliderX(sw);
		old_x = SCB_SliderX(sw);
	    }
	    if (old_x + old_width < SCB_SliderX(sw) + SCB_SliderWidth(sw))
		old_width = SCB_SliderX(sw) + SCB_SliderWidth(sw) - old_x;
	    if (old_y > SCB_SliderY(sw))
	    {
		old_height += old_y - SCB_SliderY(sw);
		old_y = SCB_SliderY(sw);
	    }
	    if (old_y + old_height < SCB_SliderY(sw) + SCB_SliderHeight(sw))
		old_height = SCB_SliderY(sw) + SCB_SliderHeight(sw) - old_y;
	    XFillRectangle(XtDisplay(sw), XtWindow(sw), SCB_UnavailableGC(sw),
			   old_x, old_y, old_width, old_height);
	}
    }
}

static void
draw_slider(Widget sw)
{
    XmScreen screen = (XmScreen)XmGetXmScreen(XtScreen(sw));

    if (SCB_Pixmap(sw) != None)
    {
	_XmFreeScratchPixmap(screen, SCB_Pixmap(sw));
    }
    SCB_Pixmap(sw) = _XmAllocScratchPixmap(screen,
					   sw->core.depth,
					   SCB_SliderWidth(sw),
					   SCB_SliderHeight(sw));

    XFillRectangle(XtDisplay(sw),
		   SCB_Pixmap(sw),
		   SCB_ForegroundGC(sw),
		   0, 0,
		   SCB_SliderWidth(sw), SCB_SliderHeight(sw));

    _XmDrawShadows(XtDisplay(sw),
		   (Window)SCB_Pixmap(sw),
		   Prim_TopShadowGC(sw),
		   Prim_BottomShadowGC(sw),
		   0, 0,
		   SCB_SliderWidth(sw), SCB_SliderHeight(sw),
		   Prim_ShadowThickness(sw),
		   XmSHADOW_OUT);

    if (SCB_EtchedSlider(sw))
	_XmDrawSeparator(XtDisplay(sw), (Window)SCB_Pixmap(sw),
			 Prim_TopShadowGC(sw), Prim_BottomShadowGC(sw),
			 SCB_ForegroundGC(sw),
			 0, 0,
			 SCB_SliderWidth(sw), SCB_SliderHeight(sw),
			 2, 1,
			 SCB_Orientation(sw) ^ (XmHORIZONTAL ^ XmVERTICAL),
			 XmSHADOW_ETCHED_IN);
}

static void
Select(Widget sw,
       XEvent *event,
       String *params,
       Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    int eventPos;
    int change_type;

    DEBUGOUT(_LtDebug(__FILE__, sw, "Select\n"));

    if (ev->button == Button1 && !XmIsScrolledWindow(XtParent(sw)))
    {
	XmProcessTraversal(sw, XmTRAVERSE_CURRENT);
    }

    if (ev->x < Prim_ShadowThickness(sw) + Prim_HighlightThickness(sw) ||
	ev->y < Prim_ShadowThickness(sw) + Prim_HighlightThickness(sw) ||
	ev->x >= XtWidth(sw) - Prim_ShadowThickness(sw) -
		Prim_HighlightThickness(sw) ||
	ev->y >= XtHeight(sw) - Prim_ShadowThickness(sw) -
		Prim_HighlightThickness(sw))
    {
	return;
    }

    eventPos = (SCB_Orientation(sw) == XmHORIZONTAL) ? ev->x : ev->y;
    change_type = 0;

    /* Yes... CancelDrag cancels a lot more then just dragging... */
    SCB_Flags(sw) |= FLG_DRAG;
    SCB_SavedValue(sw) = SCB_Value(sw);

    if (eventPos < (SCB_Orientation(sw) == XmHORIZONTAL
		    ? SCB_SliderAreaX(sw)
		    : SCB_SliderAreaY(sw)))
    {
	/* left/top button */

	if (SCB_Value(sw) != SCB_Minimum(sw))
	{
	    SCB_Arrow1Selected(sw) = True;
	    redraw_arrows(sw, True, False);
	    change_type = XmCR_DECREMENT;
	}
    }
    else if (eventPos > (SCB_Orientation(sw) == XmHORIZONTAL
			 ? SCB_SliderAreaX(sw) + SCB_SliderAreaWidth(sw)
			 : SCB_SliderAreaY(sw) + SCB_SliderAreaHeight(sw)))
    {
	/* right/bottom button */

	if (SCB_Value(sw) != SCB_Maximum(sw) - SCB_SliderSize(sw))
	{
	    SCB_Arrow2Selected(sw) = True;
	    redraw_arrows(sw, False, True);
	    change_type = XmCR_INCREMENT;
	}
    }

    /* Testing for the button defeats the translation scheme, yet
       this is how Motif seems to do it.  -- PvH
     */

    else if (ev->button != Button1)
    {
	SCB_SlidingOn(sw) = True;
	SCB_InitialX(sw) = SCB_SliderX(sw);
	SCB_InitialY(sw) = SCB_SliderY(sw);
	if (SCB_Orientation(sw) == XmHORIZONTAL)
	{
	    SCB_SeparationX(sw) = SCB_SliderWidth(sw) >> 1;
	    SCB_SeparationY(sw) = ev->y - SCB_SliderY(sw);
	    /* pretty useless, but per Motif 1.2.2 */
	}
	else
	{
	    SCB_SeparationX(sw) = ev->x - SCB_SliderX(sw);
	    SCB_SeparationY(sw) = SCB_SliderHeight(sw) >> 1;
	}
	Moved(sw, event, NULL, 0);
    }
    else if (SCB_Orientation(sw) == XmHORIZONTAL
	     ? eventPos >= SCB_SliderX(sw) &&
	       eventPos < SCB_SliderX(sw) + SCB_SliderWidth(sw)
	     : eventPos >= SCB_SliderY(sw) &&
	       eventPos < SCB_SliderY(sw) + SCB_SliderHeight(sw))
    {
	/* the slider */
	SCB_SlidingOn(sw) = True;
	SCB_InitialX(sw) = SCB_SliderX(sw);
	SCB_InitialY(sw) = SCB_SliderY(sw);
	SCB_SeparationX(sw) = ev->x - SCB_SliderX(sw);
	SCB_SeparationY(sw) = ev->y - SCB_SliderY(sw);
    }
    else
    {
	/* the trough: check to see what side of the slider we're on */
	change_type = eventPos < (SCB_Orientation(sw) == XmHORIZONTAL
				  ? SCB_SliderX(sw)
				  : SCB_SliderY(sw))
	    ? XmCR_PAGE_DECREMENT
	    : XmCR_PAGE_INCREMENT;
    }

    /* handle non-slider movement */
    if (change_type)
    {
	if (SCB_Timer(sw)) {
	    XtRemoveTimeOut(SCB_Timer(sw));
	    SCB_Timer(sw) = 0;
	}
	XSync(XtDisplay(sw), False);
	SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext(sw),
					SCB_InitialDelay(sw),
					buttonTimer,
					(XtPointer)sw);

	SCB_Flags(sw) |= SCB_Flags(sw) & FLG_FIRST_PAGE_MOVE
	    ? FLG_MULTI_CLICK : FLG_FIRST_PAGE_MOVE;
	SCB_ChangeType(sw) = change_type;
	change_value( sw, event, change_type);
    }
}

static void
Release(Widget sw,
	XEvent *event,
	String *params,
	Cardinal *num_params)
{
    int arrow1, arrow2;
    XmScrollBarCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, sw, "Release\n"));

    SCB_Flags(sw) &= ~FLG_DRAG;

    SCB_SavedValue(sw) = SCB_Value(sw);

    arrow1 = SCB_Arrow1Selected(sw);
    arrow2 = SCB_Arrow2Selected(sw);

    if (arrow1 || arrow2)
    {
	SCB_Arrow1Selected(sw) = SCB_Arrow2Selected(sw) = False;

	redraw_arrows(sw, arrow1, arrow2);
    }

    if (SCB_SlidingOn(sw))
    {
	XButtonEvent *ev = (XButtonEvent *)event;

	SCB_SlidingOn(sw) = False;

	/* send a value changed callback */
	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.event = event;
	cbs.value = EXPORTED_VALUE(sw);
	cbs.pixel = (SCB_Orientation(sw) == XmHORIZONTAL ? ev->x : ev->y);

	DEBUGOUT(_LtDebug(__FILE__, sw, "Release: ValueChangedCallback\n"));

	XFlush(XtDisplay(sw));
	XtCallCallbackList(sw, SCB_ValueChangedCallback(sw), &cbs);
    }
}

static void
Moved(Widget sw,
      XEvent *event,
      String *params,
      Cardinal *num_params)
{
    int old_value;
    short old_pos, new_pos;
    XMotionEvent *ev = (XMotionEvent *)event;
    XmScrollBarCallbackStruct cbs;

    if (!ev || !SCB_SlidingOn(sw) || !(SCB_Flags(sw) & FLG_DRAG))
    {
	return;
    }

    old_value = SCB_Value(sw);

    DEBUGOUT(_LtDebug(__FILE__, sw, "Moved\n"));

    if (SCB_Orientation(sw) == XmHORIZONTAL)
    {
	new_pos = ev->x - SCB_SeparationX(sw);
	if (new_pos != SCB_InitialX(sw))
	{
	    if (new_pos < SCB_SliderAreaX(sw))
	    {
		new_pos = SCB_SliderAreaX(sw);
		SCB_Value(sw) = SCB_Minimum(sw);
	    }
	    else if (new_pos > (SCB_SliderAreaX(sw) +
			   SCB_SliderAreaWidth(sw) - SCB_SliderWidth(sw)))
	    {
		new_pos = SCB_SliderAreaX(sw) +
		    SCB_SliderAreaWidth(sw) - SCB_SliderWidth(sw);
		SCB_Value(sw) = SCB_Maximum(sw) - SCB_SliderSize(sw);
	    }
	    else
	    {
		SCB_Value(sw) = SCB_SliderAreaWidth(sw) == SCB_SliderWidth(sw)
		    ? SCB_Maximum(sw) - SCB_SliderSize(sw)
		    : (int)((float)(new_pos - SCB_SliderAreaX(sw))
			    * ((float)(SCB_Maximum(sw) - SCB_Minimum(sw)
				       - SCB_SliderSize(sw))
			       / (float)(SCB_SliderAreaWidth(sw) -
					 SCB_SliderWidth(sw)))
			    + 0.5)
			+ SCB_Minimum(sw);
	    }

	    cbs.pixel = ev->x;
	    if (new_pos != SCB_InitialX(sw))
	    {
		old_pos = SCB_SliderX(sw);
		SCB_SliderX(sw) = new_pos;
		SCB_InitialX(sw) = new_pos;
		move_slider(sw, old_pos, SCB_SliderY(sw),
			    SCB_SliderWidth(sw), SCB_SliderHeight(sw));
	    }
	}
    }
    else
    {
	new_pos = ev->y - SCB_SeparationY(sw);
	if (new_pos != SCB_InitialY(sw))
	{
	    if (new_pos < SCB_SliderAreaY(sw))
	    {
		new_pos = SCB_SliderAreaY(sw);
		SCB_Value(sw) = SCB_Minimum(sw);
	    }
	    else if (new_pos > (SCB_SliderAreaY(sw) +
			   SCB_SliderAreaHeight(sw) - SCB_SliderHeight(sw)))
	    {
		new_pos = SCB_SliderAreaY(sw) +
		    SCB_SliderAreaHeight(sw) - SCB_SliderHeight(sw);
		SCB_Value(sw) = SCB_Maximum(sw) - SCB_SliderSize(sw);
	    }
	    else
	    {
		SCB_Value(sw) = SCB_SliderAreaHeight(sw) == SCB_SliderHeight(sw)
		    ? SCB_Maximum(sw) - SCB_SliderSize(sw)
		    : (int)((float)(new_pos - SCB_SliderAreaY(sw))
			    * ((float)(SCB_Maximum(sw) - SCB_Minimum(sw)
				       - SCB_SliderSize(sw))
			       / (float)(SCB_SliderAreaHeight(sw)
					 - SCB_SliderHeight(sw)))
			    + 0.5)
			+ SCB_Minimum(sw);
	    }

	    cbs.pixel = ev->y;
	    if (new_pos != SCB_InitialY(sw))
	    {
		old_pos = SCB_SliderY(sw);
		SCB_SliderY(sw) = new_pos;
		SCB_InitialY(sw) = new_pos;
		move_slider(sw, SCB_SliderX(sw), old_pos,
			    SCB_SliderWidth(sw), SCB_SliderHeight(sw));
	    }
	}
    }

    /* only call dragcallback if value really changed */
    if (SCB_Value(sw) != old_value)
    {
	DEBUGOUT(_LtDebug(__FILE__, sw, "Moved: DragCallback\n"));

	cbs.event = event;
	cbs.value = EXPORTED_VALUE(sw);
	cbs.reason = XmCR_DRAG;

	XFlush(XtDisplay(sw));
	XtCallCallbackList(sw, SCB_DragCallback(sw), &cbs);
    }
}

static void
TopOrBottom(Widget sw,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    int change_type;

    DEBUGOUT(_LtDebug(__FILE__, sw, "TopOrBottom\n"));

    if (event->type == KeyPress)
    {
	/* This is per Motif: osfBeginLine results in a toTopCallback
	 * whereas all oher keys bound to TopOrBottom() result in a
	 * toBottomCallback -- PvH
	 */

	change_type = XtGetActionKeysym(event, NULL) == osfXK_BeginLine
	    ? XmCR_TO_TOP : XmCR_TO_BOTTOM;
    }
    else
    {
	XButtonEvent *ev = (XButtonEvent *)event;

	if (ev->x < Prim_ShadowThickness(sw) + Prim_HighlightThickness(sw) ||
	    ev->y < Prim_ShadowThickness(sw) + Prim_HighlightThickness(sw) ||
	    ev->x >= XtWidth(sw) - Prim_ShadowThickness(sw) -
	    Prim_HighlightThickness(sw) ||
	    ev->y >= XtHeight(sw) - Prim_ShadowThickness(sw) -
	    Prim_HighlightThickness(sw) ||
	    (ev->x >= SCB_SliderX(sw) && ev->y >= SCB_SliderY(sw) &&
	     ev->x < SCB_SliderX(sw) + SCB_SliderWidth(sw) &&
	     ev->y < SCB_SliderY(sw) + SCB_SliderHeight(sw)))
	{
	    return;
	}

	/* Note: don't exclude the arrow regions, even have them selected... */

	if (SCB_Orientation(sw) == XmHORIZONTAL)
	{
	    if (ev->x < SCB_SliderX(sw))
	    {
		/* left of the slider */

		change_type = XmCR_TO_TOP;
		if (ev->x < SCB_SliderAreaX(sw))
		{
		    SCB_Arrow1Selected(sw) = True;
		}
	    }
	    else
	    {
		/* right of slider */

		change_type = XmCR_TO_BOTTOM;
		if (ev->x > SCB_SliderAreaX(sw) + SCB_SliderAreaWidth(sw))
		{
		    SCB_Arrow2Selected(sw) = True;
		}
	    }
	}
	else
	    /* SCB_Orientation(sw) == XmVERTICAL */
	{
	    if (ev->y < SCB_SliderY(sw))
	    {
		/* up the slider */

		change_type = XmCR_TO_TOP;
		if (ev->y < SCB_SliderAreaY(sw))
		{
		    SCB_Arrow1Selected(sw) = True;
		}
	    }
	    else
	    {
		/* down the slider */

		change_type = XmCR_TO_BOTTOM;
		if (ev->y > SCB_SliderAreaY(sw) + SCB_SliderAreaHeight(sw))
		{
		    SCB_Arrow2Selected(sw) = True;
		}
	    }
	}
    }

    redraw_arrows(sw, SCB_Arrow1Selected(sw), SCB_Arrow2Selected(sw));
    change_value(sw, event, change_type);
}

static void
CancelDrag(Widget sw,
	   XEvent *event,
	   String *params,
	   Cardinal *num_params)
{
    XmScrollBarCallbackStruct cbs;
    short old_slider_x, old_slider_y;

    if (!(SCB_Flags(sw) & FLG_DRAG))
    {
	_XmPrimitiveParentActivate(sw, event, params, num_params);
	/* Per O'Reilly 6B, I guess all the params are passed on, so FIX ME */

	return;
    }

    SCB_SlidingOn(sw) = False;
    SCB_Flags(sw) &= ~FLG_DRAG;

    SCB_Value(sw) = SCB_SavedValue(sw);
    if (SCB_Value(sw) < SCB_Minimum(sw))
	SCB_Value(sw) = SCB_Minimum(sw);
    if (SCB_Value(sw) > SCB_Maximum(sw) - SCB_SliderSize(sw))
	SCB_Value(sw) = SCB_Maximum(sw) - SCB_SliderSize(sw);

    cbs.event = event;
    cbs.value = EXPORTED_VALUE(sw);
    cbs.reason = XmCR_VALUE_CHANGED;

    old_slider_x = SCB_SliderX(sw);
    old_slider_y = SCB_SliderY(sw);

    if (SCB_Orientation(sw) == XmHORIZONTAL)
    {
	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	cbs.pixel = SCB_SliderX(sw);	/* per Motif */
    }
    else
    {
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	cbs.pixel = SCB_SliderY(sw);	/* per Motif */
    }
    move_slider(sw, old_slider_x, old_slider_y,
		SCB_SliderWidth(sw), SCB_SliderHeight(sw));

    XFlush(XtDisplay(sw));
    XtCallCallbackList(sw, SCB_ValueChangedCallback(sw), &cbs);
}

/*
 * The argument can be 0 for Down, or 1 for Right.
 * Or it can be "down" or "right".
 */
static void
PageDownOrRight(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int which;

    DEBUGOUT(_LtDebugAction(__FILE__, w, "PageDownOrRight", params, num_params));

    if (*num_params != 1) {
	_XmWarning(w, "PageDownOrRight: num_params wrong for widget");
	which = 0;
    } else if (strcasecmp(params[0], "down") == 0) {
	    which = 0;
    } else if (strcasecmp(params[0], "right") == 0) {
	    which = 1;
    } else {
	which = atoi(params[0]);
    }

    /* Spooky check which relies on XmVERTICAL + 1 == XmHORIZONTAL */
    if (XmVERTICAL + which == SCB_Orientation(w))
	change_value(w, event, XmCR_PAGE_INCREMENT);
}

/*
 * Up == 0, Left == 1
 */
static void
PageUpOrLeft(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int which;

    DEBUGOUT(_LtDebugAction(__FILE__, w, "PageUpOrLeft", params, num_params));

    if (*num_params != 1) {
	_XmWarning(w, "PageUpOrLeft: num_params wrong for widget");
	which = 0;
    } else if (strcasecmp(params[0], "up") == 0) {
	    which = 0;
    } else if (strcasecmp(params[0], "left") == 0) {
	    which = 1;
    } else {
	which = atoi(params[0]);
    }

    /* Spooky check which relies on XmVERTICAL + 1 == XmHORIZONTAL */
    if (XmVERTICAL + which == SCB_Orientation(w))
	change_value(w, event, XmCR_PAGE_DECREMENT);
}

static void
IncrementDownOrRight(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int which;

    DEBUGOUT(_LtDebugAction(__FILE__, w, "IncrementDownOrRight", params, num_params));

    if (*num_params != 1) {
	_XmWarning(w, "IncrementDownOrRight: num_params wrong for widget");
	which = 0;
    } else if (strcasecmp(params[0], "down") == 0) {
	    which = 0;
    } else if (strcasecmp(params[0], "right") == 0) {
	    which = 1;
    } else {
	which = atoi(params[0]);
    }

    /* Spooky check which relies on XmVERTICAL + 1 == XmHORIZONTAL */
    if (XmVERTICAL + which == SCB_Orientation(w))
	change_value(w, event, XmCR_INCREMENT);
}

static void
IncrementUpOrLeft(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int which;

    DEBUGOUT(_LtDebugAction(__FILE__, w, "IncrementUpOrLeft", params, num_params));

    if (*num_params != 1) {
	_XmWarning(w, "IncrementUpOrLeft: num_params wrong for widget");
	which = 0;
    } else if (strcasecmp(params[0], "up") == 0) {
	    which = 0;
    } else if (strcasecmp(params[0], "left") == 0) {
	    which = 1;
    } else {
	which = atoi(params[0]);
    }

    /* Spooky check which relies on XmVERTICAL + 1 == XmHORIZONTAL */
    if (XmVERTICAL + which == SCB_Orientation(w))
	change_value(w, event, XmCR_DECREMENT);
}

static void
change_value(Widget sw, XEvent *event, int change_type)
{
    int old_value;
    short old_slider_x, old_slider_y;
    XtCallbackList cb;
    XmScrollBarCallbackStruct cbs;
    static int cboffsets[] =
    {
	XtOffsetOf(XmScrollBarRec, scrollBar.increment_callback),
	XtOffsetOf(XmScrollBarRec, scrollBar.decrement_callback),
	XtOffsetOf(XmScrollBarRec, scrollBar.page_increment_callback),
	XtOffsetOf(XmScrollBarRec, scrollBar.page_decrement_callback),
	XtOffsetOf(XmScrollBarRec, scrollBar.to_top_callback),
	XtOffsetOf(XmScrollBarRec, scrollBar.to_bottom_callback)
    };

    old_value = SCB_Value(sw);

    /* Change the value by the appropriate amount */
    switch (change_type)
    {
    case XmCR_INCREMENT:
	SCB_Value(sw) += SCB_Increment(sw);
	if (SCB_Value(sw) > SCB_Maximum(sw) - SCB_SliderSize(sw))
	    SCB_Value(sw) = SCB_Maximum(sw) - SCB_SliderSize(sw);
	break;

    case XmCR_DECREMENT:
	SCB_Value(sw) -= SCB_Increment(sw);
	if (SCB_Value(sw) < SCB_Minimum(sw))
	    SCB_Value(sw) = SCB_Minimum(sw);
	break;

    case XmCR_PAGE_INCREMENT:
	SCB_Value(sw) += SCB_PageIncrement(sw);
	if (SCB_Value(sw) > SCB_Maximum(sw) - SCB_SliderSize(sw))
	    SCB_Value(sw) = SCB_Maximum(sw) - SCB_SliderSize(sw);
	break;

    case XmCR_PAGE_DECREMENT:
	SCB_Value(sw) -= SCB_PageIncrement(sw);
	if (SCB_Value(sw) < SCB_Minimum(sw))
	    SCB_Value(sw) = SCB_Minimum(sw);
	break;

    case XmCR_TO_TOP:
	SCB_Value(sw) = SCB_Minimum(sw);
	break;

    case XmCR_TO_BOTTOM:
	SCB_Value(sw) = SCB_Maximum(sw) - SCB_SliderSize(sw);
	break;

    default:
	_XmWarning(sw, "change_value: unexpected change_type");
	return;
    }

    /* If nothing changed, no action */
    if (SCB_Value(sw) == old_value)
    {
	return;
    }

    /* Move the slider if necessary */
    old_slider_x = SCB_SliderX(sw);
    old_slider_y = SCB_SliderY(sw);

    if (SCB_Orientation(sw) == XmHORIZONTAL)
    {
	SCB_SliderX(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	cbs.pixel = event ? event->xbutton.x : 0;
    }
    else
    {
	SCB_SliderY(sw) = _XmScrollBarValueToPos(sw, SCB_Value(sw));
	cbs.pixel = event ? event->xbutton.y : 0;
    }
    move_slider(sw, old_slider_x, old_slider_y,
		SCB_SliderWidth(sw), SCB_SliderHeight(sw));

    /* Adjust the change_type for processingDirection,
     * and choose its associated callback.
     */
    change_type = ((change_type - 1) ^ (1 - MAX_ON_BOTTOM_RIGHT(sw))) + 1;
    cb = *(XtCallbackList *)
	((char *)sw + cboffsets[change_type - XmCR_INCREMENT]);

    /* Call the callback, or punt to valueChangedCallback */
    cbs.reason = change_type;
    cbs.event = event;
    cbs.value = EXPORTED_VALUE(sw);

    if (!cb)
    {
	cbs.reason = XmCR_VALUE_CHANGED;
	cb = SCB_ValueChangedCallback(sw);
    }

    XFlush(XtDisplay(sw));
    XtCallCallbackList(sw, cb, &cbs);
}

/* timer callback for moving by holding down the mouse button */

static void
buttonTimer(XtPointer clientData,
	    XtIntervalId *id)
{
    Widget sw = (Widget)clientData;

    if (!(SCB_Flags(sw) & FLG_DRAG))
    {
	SCB_Timer(sw) = 0;
	return;
    }

    if (!(SCB_Flags(sw) & FLG_FIRST_PAGE_MOVE))
    {
	XSync(XtDisplay(sw), False);
	SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext(sw),
					SCB_RepeatDelay(sw),
					buttonTimer,
					(XtPointer)sw);
	change_value(sw, NULL, SCB_ChangeType(sw));
    }
    else
    {
	SCB_Flags(sw) &= ~FLG_FIRST_PAGE_MOVE & ~FLG_MULTI_CLICK;
	SCB_Timer(sw) = XtAppAddTimeOut(XtWidgetToApplicationContext(sw),
					SCB_RepeatDelay(sw),
					buttonTimer,
					(XtPointer)sw);
    }
}

void
_XmSetEtchedSlider(XmScrollBarWidget sw)
{
    SCB_EtchedSlider(sw) = True;
}

static short
_XmScrollBarValueToPos(Widget sw, int value)
{
    int delta = SCB_Maximum(sw) - SCB_Minimum(sw) - SCB_SliderSize(sw);

    return SCB_Orientation(sw) == XmHORIZONTAL
	? !delta
	  ? SCB_SliderAreaX(sw)
	  : (short)((float)(value - SCB_Minimum(sw))
		    * ((float)(SCB_SliderAreaWidth(sw) - SCB_SliderWidth(sw))
		       / (float)(delta))
		    + 0.5)
		+ SCB_SliderAreaX(sw)
	: !delta
	  ? SCB_SliderAreaY(sw)
	  : (short)((float)(value - SCB_Minimum(sw))
		    * ((float)(SCB_SliderAreaHeight(sw) - SCB_SliderHeight(sw))
		       / (float)(delta))
		    + 0.5)
		+ SCB_SliderAreaY(sw);
}

static short
_XmScrollBarSliderPixSize(Widget sw)
{
    short ret;

    ret = (short)((float)SCB_SliderSize(sw)
		  * ((float)(SCB_Orientation(sw) == XmHORIZONTAL
			     ? SCB_SliderAreaWidth(sw)
			     : SCB_SliderAreaHeight(sw))
		     / (float)(SCB_Maximum(sw) - SCB_Minimum(sw)))
		  + 0.5);

    return ret >= MIN_SLIDER_LENGTH
	? ret
	: MIN_SLIDER_LENGTH;
}

static void
_XmScrollBarProcessingDirectionDefault(Widget w,
				       int offset,
				       XrmValue *val)
{
    static unsigned char direction;

    direction = default_processing_direction(w);
    val->addr = (XPointer)&direction;
}

static void
_XmScrollBarTraversalOnDefault(Widget w,
			       int offset,
			       XrmValue *val)
{
    static Boolean traversalOn;
/*
 * I believe this is where OSF/Motif versions differ.
 * FIX ME
 */
#if 0
    traversalOn = !XmIsScrolledWindow(XtParent(w));
#else
    /* This is from the OSF/Motif 2.0 man pages */
    traversalOn = False;
    if (XmIsScrolledWindow(XtParent(w)) &&
	SW_ScrollPolicy(XtParent(w)) == XmAUTOMATIC)
    {
	traversalOn = True;
    }
#endif

    val->addr = (XPointer)&traversalOn;
}

/* till 27.5.1999: it seems that their ScrollBar
 *				   copies its default background 
 *				   from the parent if the latter is
 *				   a ScrolledWindow or a MainWindow.
 *				   
 *				   This is different from our (now fixed)
 *				   policy, where ScrolledW enforced its
 *				   background on the scrollbars at creation
 *				   time.
 *
 *				   Test e.g. one of the mainw tests with
 *
 *				   mainw*XmMainWindow.background: green
 *				   mainw*XmScrollBar.background: red
 *
 *				   you will note that M*tif allows the
 *				   second line to take effect.
 *
 *				   Also, if the scrollbars are added after
 *				   creation of the ScrolledW, the old policy
 *				   produces wrong results.
 */

static void
defBackgroundFromParent(Widget w, int offset, XrmValue *val)
{

	if ( XtIsSubclass(XtParent(w),xmScrolledWindowWidgetClass) )
	{
		val->size = sizeof(XmParentBackground(w));
		val->addr = (XPointer) & XmParentBackground(w);
	}
	else
	{
		_XmBackgroundColorDefault(w,offset,val);
	}
}
