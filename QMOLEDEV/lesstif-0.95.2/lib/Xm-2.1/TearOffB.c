/**
 *
 * $Id: TearOffB.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright (C) 1996-2001 LessTif Development Team 
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

static const char rcsid[] = "$Id: TearOffB.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/MenuShell.h>
#include <Xm/MenuUtilP.h>
#include <Xm/TearOffBP.h>
#include <Xm/TearOffP.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/DisplayP.h>
#include <Xm/TransltnsP.h>
#include <Xm/RepType.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize(void);

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
 * Resources for the tearoffbutton class
 */
#define Offset(field) XtOffsetOf(XmTearOffButtonRec, tear_off_button.field)
static XtResource resources[] =
{
    {
	XmNseparatorType, XmCSeparatorType, XmRSeparatorType,
	sizeof(unsigned char), Offset(separator_type),
	XmRImmediate, (XtPointer)XmSHADOW_ETCHED_OUT_DASH
    },
    {
	XmNmargin, XmCMargin, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCDimension, XmRVerticalDimension,
	sizeof(Dimension), XtOffsetOf(XmTearOffButtonRec, core.height),
	XmRImmediate, (XtPointer)1
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNmargin,
	sizeof(Dimension), Offset(margin),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};


static void BDrag(Widget w, XEvent *event,
		  String *params, Cardinal *num_params);

static void BActivate(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);

static void KActivate(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);


static XtActionsRec actions[] =
{
    {"BDrag", BDrag},
    {"BActivate", BActivate},
    {"KActivate", KActivate}
};

XmPrimitiveClassExtRec _XmTearOffBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmInheritBaselineProc,
    /* widget_display_rect */ XmInheritDisplayRectProc,
    /* widget_margins      */ NULL
};

XmTearOffButtonClassRec xmTearOffButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPushButtonClassRec,
	/* class_name            */ "XmTearOffButton",
	/* widget_size           */ sizeof(XmTearOffButtonRec),
	/* class_initialize      */ class_initialize,
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
	/* compress_exposure     */ XtExposeCompressMaximal /*XtExposeCompressMultiple*/,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ XtInheritResize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersionDontCheck,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ XtInheritQueryGeometry /* query_geometry */,
	/* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
	/* border_unhighlight    */ XmInheritBorderUnhighlight,
	/* translations          */ XtInheritTranslations,
	/* arm_and_activate_proc */ XmInheritArmAndActivate,
	/* Synthetic Resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmTearOffBPrimClassExtRec
    },
    /* Label Class part */
    {
	/* setOverrideCallback */ XmInheritSetOverrideCallback,
	/* menuProcs           */ XmInheritMenuProc,
	/* translations        */ XtInheritTranslations,
	/* extension           */ NULL
    },
    /* Push Button part */
    {
	/* extension           */ NULL
    },
    /* Tear Off Button part */
    {
       /* translations         */ _XmTearOffB_overrideTranslations,
    },
};


WidgetClass xmTearOffButtonWidgetClass = (WidgetClass)&xmTearOffButtonClassRec;

static void
class_initialize(void)
{
    /* FIX ME */
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmTearOffButtonWidgetClass tobc;
    XmTearOffButtonWidgetClass swc;

    tobc = (XmTearOffButtonWidgetClass)widget_class;
    swc = (XmTearOffButtonWidgetClass)widget_class->core_class.superclass;

    /* Handle tear off button class part inheritance */
    if (tobc->tearoffbutton_class.translations == XtInheritTranslations)
    {
	tobc->tearoffbutton_class.translations =
	    swc->tearoffbutton_class.translations;
    }
    else if (tobc->tearoffbutton_class.translations != NULL)
    {
	tobc->tearoffbutton_class.translations =
	    (String)XtParseTranslationTable(tobc->tearoffbutton_class.
						translations);
    }

    _XmFastSubclassInit(widget_class, XmTEAROFF_BUTTON_BIT);
}

static void
CreateSeparatorGC(Widget w)
{
    XGCValues values;
    XtGCMask  valueMask;

    valueMask =  GCBackground | GCLineStyle | GCForeground | GCJoinStyle |
			GCCapStyle;

    values.join_style = JoinMiter;
    values.cap_style = CapButt;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.line_style = (TOB_SeparatorType(w) == XmSINGLE_DASHED_LINE ||
			 TOB_SeparatorType(w) == XmDOUBLE_DASHED_LINE)
	? LineDoubleDash
	: LineSolid;

    TOB_SeparatorGC(w) = XtGetGC(w, valueMask, &values);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    int trh;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "TOB_initialize\n"));

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRSeparatorType),
			     TOB_SeparatorType(new_w), new_w))
	TOB_SeparatorType(new_w) = XmSHADOW_ETCHED_OUT_DASH;

    TOB_Orientation(new_w) = XmHORIZONTAL;

    XtOverrideTranslations(new_w, (XtTranslations)
	((XmTearOffButtonWidgetClass)new_w->core.widget_class)
	->tearoffbutton_class.translations);

    CreateSeparatorGC(new_w);

    /* Tweak the "text" rect so the button is slim (but not too slim) */
    Lab_TextRect_width(new_w) = 0;
    if (!XtWidth(request))
	XtWidth(new_w) = 1;
    trh = (Lab_TextRect_height(new_w) >> 1) > (Lab_Shadow(new_w) << 1)
	? (Lab_TextRect_height(new_w) >> 1) - (Lab_Shadow(new_w) << 1)
	: 0;
    if (!XtHeight(request))
	XtHeight(request) += trh - Lab_TextRect_height(new_w);
    Lab_TextRect_height(new_w) = trh;
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, TOB_SeparatorGC(w));
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "TOB_SetValues()\n"));

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRSeparatorType),
			     TOB_SeparatorType(new_w), new_w))
	TOB_SeparatorType(new_w) = TOB_SeparatorType(old);

    if (Prim_Foreground(new_w) != Prim_Foreground(old) ||
	XtBackground(new_w) != XtBackground(old) ||
	((TOB_SeparatorType(new_w) == XmSINGLE_DASHED_LINE ||
	  TOB_SeparatorType(new_w) == XmDOUBLE_DASHED_LINE) ^
	 (TOB_SeparatorType(old) == XmSINGLE_DASHED_LINE ||
	  TOB_SeparatorType(old) == XmDOUBLE_DASHED_LINE)))
    {
	XtReleaseGC(new_w, TOB_SeparatorGC(new_w));
	CreateSeparatorGC(new_w);
	refresh_needed = True;
    }

    if (Lab_TextRect_height(new_w) != Lab_TextRect_height(old)
	|| Lab_Shadow(new_w) != Lab_Shadow(old))
    {
	int trh;

	Lab_TextRect_width(new_w) = 0;
	if (!XtWidth(request) ||
	    (Lab_RecomputeSize(new_w) && XtWidth(request) == XtWidth(old)))
	    XtWidth(new_w) = 1;
	trh = (Lab_TextRect_height(new_w) >> 1) > (Lab_Shadow(new_w) << 1)
	    ? (Lab_TextRect_height(new_w) >> 1) - (Lab_Shadow(new_w) << 1)
	    : 0;
	if (!XtHeight(request) ||
	    (Lab_RecomputeSize(new_w) && XtHeight(request) == XtHeight(old)))
	    XtHeight(request) += trh - Lab_TextRect_height(new_w);
	Lab_TextRect_height(new_w) = trh;
    }

    if (TOB_Margin(new_w) != TOB_Margin(old))
    {
	refresh_needed = True;
    }

    return refresh_needed;
}

static void
expose(Widget w, XEvent *event, Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "TearOffB expose\n"));

    _XmDrawSeparator(XtDisplay(w), XtWindow(w),
		     Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
		     TOB_SeparatorGC(w),
		     0, 0,
		     XtWidth(w), XtHeight(w),
		     Lab_Shadow(w),
		     Lab_Shadow(w) + TOB_Margin(w),
		     TOB_Orientation(w),
		     TOB_SeparatorType(w));

    /* Draw the shadow like PushB does, but don't draw the label */

    if (PB_Armed(w))
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
		       0, 0,
		       XtWidth(w), XtHeight(w),
		       Lab_Shadow(w), XmSHADOW_OUT);
    }
    else
    {
	_XmClearBorder(XtDisplay(w), XtWindow(w),
		       0, 0,
		       XtWidth(w), XtHeight(w),
		       Lab_Shadow(w));
    }
}

#if 0
static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    /* Motif does not have this method */
    XtWidgetGeometry a;		/* Standin for answer if NULL parameter */

    DEBUGOUT(_LtDebug(__FILE__, w, "TOB_query_geometry\n"));

#define	Wants(x)	(proposed->request_mode & x)

    /* NULL case should not yet end here ! */
    if (proposed->request_mode != 0)
    {
	/* If they don't ask width/height, let them have whatever they like */
	if ((!(Wants(CWWidth))) && (!Wants(CWHeight)))
	{
	    if (answer)
	    {
		*answer = *proposed;
	    }

	    return XtGeometryYes;
	}
    }

    if (TOB_Orientation(w) == XmVERTICAL)
    {
	a.width = TOB_Margin(w) + 2 * (Lab_Shadow(w) + Lab_Highlight(w));
	a.height = 2 * (Lab_Shadow(w) + Lab_Highlight(w));
    }
    else
    {
	a.width = 2 * (Lab_Shadow(w) + Lab_Highlight(w));
	a.height = TOB_Margin(w) + 2 * (Lab_Shadow(w) + Lab_Highlight(w));
    }

    a.request_mode = CWWidth | CWHeight;

    if (answer)
    {
	*answer = a;
    }

    /* NULL proposed -> return Width+Height */
    if (proposed->request_mode == 0)
    {
	return XtGeometryAlmost;
    }

    if (proposed->width >= answer->width && proposed->height >= answer->height)
    {
	return XtGeometryYes;
    }
    else if (answer->width == XtWidth(w) && answer->height == XtHeight(w))
    {
	if (answer)
	{
	    answer->request_mode = 0;
	}

	return XtGeometryNo;
    }
    else
    {
	return XtGeometryAlmost;
    }
}
#endif

/*
 * move/draw a window outline
 */
static void
DrawOutline(Widget w, GC gc, int x, int y, int width, int height)
{
    static int lastx = 0;
    static int lasty = 0;
    static int lastWidth = 0;
    static int lastHeight = 0;
    XRectangle rects[2];

    if (x == lastx && y == lasty && width == lastWidth && height == lastHeight)
    {
	return;
    }

    /* undraw the old one, if any */
    if (lastWidth || lastHeight)
    {
	rects[0].x = lastx;
	rects[0].y = lasty;
	rects[0].width = lastWidth;
	rects[0].height = lastHeight;
	rects[1].x = lastx + 1;
	rects[1].y = lasty + 1;
	rects[1].width = lastWidth - 2;
	rects[1].height = lastHeight - 2;

	XDrawRectangles(XtDisplay(w),
			RootWindowOfScreen(XtScreen(w)),
			gc, rects, 2);
    }

    lastx = x;
    lasty = y;
    lastWidth = width;
    lastHeight = height;

    /* draw the new one, if any */
    if (lastWidth || lastHeight)
    {
	rects[0].x = lastx;
	rects[0].y = lasty;
	rects[0].width = lastWidth;
	rects[0].height = lastHeight;
	rects[1].x = lastx + 1;
	rects[1].y = lasty + 1;
	rects[1].width = lastWidth - 2;
	rects[1].height = lastHeight - 2;

	XDrawRectangles(XtDisplay(w),
			RootWindowOfScreen(XtScreen(w)),
			gc, rects, 2);
    }
}

/*
 * Move the rubberband around, return with the new window location
 */
static void
EventLoop(Widget w, GC gc,
	  int XOffset, int YOffset, int Width, int Height,
	  Boolean opaque_move, XEvent *event)
{
    Bool finished = False;
    Bool done;
    int xl, yt;
    XEvent oevent;
    Window root, child;
    int foox, fooy;
    unsigned int mask;

    XQueryPointer(XtDisplay(w), RootWindowOfScreen(XtScreen(w)),
		  &root, &child, &xl, &yt,
		  &foox, &fooy, &mask);

    xl += XOffset;
    yt += YOffset;

    if (!opaque_move)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "Not opaque move\n"));
	DrawOutline(w, gc, xl, yt, Width, Height);
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "Opaque move\n"));
    }

    while (!finished)
    {
	/* block until there is an interesting event */
	XMaskEvent(XtDisplay(w),
		   ButtonPressMask | ButtonReleaseMask | KeyPressMask |
		   PointerMotionMask | ButtonMotionMask, &oevent);

	_XmRecordEvent(&oevent);

	/* discard any extra motion events before a logical release */
	if (oevent.type == MotionNotify)
	{
	    while (XCheckMaskEvent(XtDisplay(w),
				   PointerMotionMask | ButtonMotionMask |
				   ButtonPressMask | ButtonRelease |
				   ExposureMask,
				   &oevent))
	    {
		_XmRecordEvent(&oevent);

		DEBUGOUT(_LtDebug(__FILE__, w, "Discard event\n"));

		if (oevent.type == ButtonRelease)
		{
		    break;
		}
	    }
	}

	done = False;

	switch (oevent.type)
	{
	case KeyPress:
	    done = True;
	    break;

	case ButtonPress:
	    XAllowEvents(XtDisplay(w), ReplayPointer, CurrentTime);
	    done = 1;
	    break;

	case ButtonRelease:
	    if (!opaque_move)
	    {
		DrawOutline(w, gc, 0, 0, 0, 0);
	    }

	    xl = oevent.xmotion.x_root + XOffset;
	    yt = oevent.xmotion.y_root + YOffset;

	    /* Resist moving windows over the edge of the screen! */
	    if ((xl + Width) > WidthOfScreen(XtScreen(w)))
	    {
		xl = WidthOfScreen(XtScreen(w)) - Width;
	    }

	    if (xl < 0)
	    {
		xl = 0;
	    }

	    if ((yt + Height) > HeightOfScreen(XtScreen(w)))
	    {
		yt = HeightOfScreen(XtScreen(w)) - Height;
	    }

	    if (yt < 0)
	    {
		yt = 0;
	    }

	    event->xbutton.x_root = xl;
	    event->xbutton.y_root = yt;

	    done = True;
	    finished = True;
	    break;

	case MotionNotify:

	    DEBUGOUT(_LtDebug(__FILE__, w, "MOTION NOTIFY\n"));

	    xl = oevent.xmotion.x_root;
	    yt = oevent.xmotion.y_root;

	    /* redraw the rubberband */
	    xl += XOffset;
	    yt += YOffset;

	    /* Resist moving windows over the edge of the screen! */
	    if ((xl + Width) > WidthOfScreen(XtScreen(w)))
	    {
		xl = WidthOfScreen(XtScreen(w)) - Width;
	    }

	    if (xl < 0)
	    {
		xl = 0;
	    }

	    if ((yt + Height) > HeightOfScreen(XtScreen(w)))
	    {
		yt = HeightOfScreen(XtScreen(w)) - Height;
	    }

	    if (yt < 0)
	    {
		yt = 0;
	    }

	    if (!opaque_move)
	    {
		DrawOutline(w, gc, xl, yt, Width, Height);
	    }
	    else
	    {
		XtMoveWidget(w, xl, yt);
	    }

	    done = True;
	    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

	    break;

	case Expose:
	    XtDispatchEvent(&oevent);
	    break;

	default:
	    break;
	}

	if (!done && !opaque_move)
	{
	    DrawOutline(w, gc, 0, 0, 0, 0);
	}
    }
}

static void
BDrag(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    /* create a glyph cursor, do an asynchronouse grab of the pointer and
     * keyboard, and select button events so that we can be notified when
     * the user releases the drag. */
    Widget disp = XmGetXmDisplay(XtDisplay(w));
    Boolean opaque_move = _XmGetMoveOpaqueByScreen(XtScreen(w));
    Widget shell = XtParent(XtParent(w));
    Window root;
    int xoff, yoff;
    int dx, dy;
    unsigned int dw, dh, bw, depth;
    XGCValues gcv;
    unsigned long gcm;
    GC gc;

    DEBUGOUT(_LtDebug(__FILE__, w, "BDrag\n"));

    XGetGeometry(XtDisplay(shell), XtWindow(shell),
		 &root, &dx, &dy, &dw, &dh, &bw, &depth);

    gcm = GCFunction | GCLineWidth | GCForeground | GCSubwindowMode;
    gcv.function = GXxor;
    gcv.line_width = 0;
    gcv.foreground = (1UL << depth) - 1;
    gcv.subwindow_mode = IncludeInferiors;

    gc = XCreateGC(XtDisplay(shell), RootWindowOfScreen(XtScreen(shell)),
		   gcm, &gcv);

    dw += bw;
    dh += bw;
    xoff = dx - event->xbutton.x_root;
    yoff = dy - event->xbutton.y_root;

    _XmGrabPointer(shell, True, (ButtonPressMask | ButtonReleaseMask |
				 EnterWindowMask | LeaveWindowMask |
				 PointerMotionMask),
		   GrabModeSync, GrabModeAsync, None,
		   ((XmDisplayInfo *)Display_DisplayInfo(disp))->TearOffCursor,
		   CurrentTime);

    XAllowEvents(XtDisplay(shell), SyncPointer, CurrentTime);

    EventLoop(shell, gc, xoff, yoff, (int)dw, (int)dh, opaque_move, event);

    _XmUngrabPointer(shell, CurrentTime);

    PB_Armed(w) = False;

    _XmClearBorder(XtDisplay(w), XtWindow(w),
		   0, 0, XtWidth(w), XtHeight(w),
		   Lab_Shadow(w));

    XFreeGC(XtDisplay(shell), gc);

    _XmSetInDragMode(w, False);

    _XmTearOffInitiate(w, event);
}

static void
BActivate(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    Widget parent, shell;
    Boolean validButton;

    DEBUGOUT(_LtDebug(__FILE__, w, "BActivate\n"));

    parent = XtParent(w);

    shell = parent;
    while (!XtIsShell(shell))
    {
	shell = XtParent(shell);
    }

    if (event && (event->type == ButtonRelease))
    {
	RC_MenuButton(w, event, &validButton);
    }
    else
    {
	validButton = False;
    }

    if (!validButton)
    {
	return;
    }

    _XmRecordEvent(event);

    PB_Armed(w) = False;

    _XmClearBorder(XtDisplay(w), XtWindow(w),
		   0, 0, XtWidth(w), XtHeight(w),
		   Lab_Shadow(w));

    _XmSetInDragMode(w, False);

    _XmTearOffInitiate(w, event);
}

static void
KActivate(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KActivate\n"));

    _XmTearOffInitiate(w, event);
}
