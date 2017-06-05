#define	FIX_993209
/*
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ScrolledW.c,v 1.9 2005/05/06 13:25:58 dannybackx Exp $
 *
 * Copyright (C) 1996 Free Software Foundation, Inc.
 * Copyright © 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ScrolledW.c,v 1.9 2005/05/06 13:25:58 dannybackx Exp $";

/*
 * Current status of changes in layout (Danny 18/4/1997) :
 *
 * the bottom_right (default) and bottom_left cases are believed to be finished.
 * the top_left and top_right cases are badly broken
 */

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/DrawingAP.h>
#include <Xm/RowColumnP.h>
#include <Xm/TransltnsP.h>
#include <XmI/TransltnsI.h>
#include <XmI/DebugUtil.h>

/* rws 25 Mar 1998
   With this undefined the page list of mgv and mfm work correctly. The mfm
   problem can be seen if you start it and then select a directory from the
   middle list.  At this point the list on the right gets real tiny.
 */
/* rws 2 Jul 1998
   With this undefined ddd 3.x gets into an infinite loop.
 */
#define MFM_BUGS

/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void resize(Widget w);

static void realize(Widget w, Mask *value_mask,
		    XSetWindowAttributes *attributes);

static void expose(Widget w, XEvent *event, Region region);

static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void change_managed(Widget w);

static void insert_child(Widget w);

static void delete_child(Widget w);

static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);

static XtGeometryResult _XmScrolledWGeomRequest(Widget w, XmSWValues *vals);

void _XmConfigureScrollBars(Widget w, Widget child,
				XtWidgetGeometry *childgeom, XmSWValues *vals);

void _XmRepositionScrolledWindow(Widget w,
				     XtPointer client,
				     XtPointer call);

void _XmFixupScrollBars(Widget w, Dimension ww, Dimension wh);

/*
 * Resources for the ScrolledWindow class
 */
#define Offset(field) XtOffsetOf(XmScrolledWindowRec, swindow.field)
#define MGR_Offset(field) XtOffsetOf(XmScrolledWindowRec, manager.field)
static XtResource resources[] =
{
    {
	XmNhorizontalScrollBar, XmCHorizontalScrollBar, XmRWidget,
	sizeof(Widget), Offset(hScrollBar),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNverticalScrollBar, XmCVerticalScrollBar, XmRWidget,
	sizeof(Widget), Offset(vScrollBar),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNworkWindow, XmCWorkWindow, XmRWidget,
	sizeof(Widget), Offset(WorkWindow),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNclipWindow, XmCClipWindow, XmRWidget,
	sizeof(Widget), Offset(ClipWindow),
	XtRImmediate, (XtPointer)NULL
    },
    {
	XmNscrollingPolicy, XmCScrollingPolicy, XmRScrollingPolicy,
	sizeof(unsigned char), Offset(ScrollPolicy),
	XmRImmediate, (XtPointer)XmAPPLICATION_DEFINED
    },
    {
	XmNvisualPolicy, XmCVisualPolicy, XmRVisualPolicy,
	sizeof(unsigned char), Offset(VisualPolicy),
	XmRImmediate, (XtPointer)XmVARIABLE
    },
    {
	XmNscrollBarDisplayPolicy, XmCScrollBarDisplayPolicy,
	XmRScrollBarDisplayPolicy,
	sizeof(unsigned char), Offset(ScrollBarPolicy),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNscrollBarPlacement, XmCScrollBarPlacement, XmRScrollBarPlacement,
	sizeof(unsigned char), Offset(Placement),
	XtRImmediate, (XtPointer)XmBOTTOM_RIGHT
    },
    {
	XmNscrolledWindowMarginWidth, XmCScrolledWindowMarginWidth,
	XmRHorizontalDimension,
	sizeof(Dimension), Offset(WidthPad),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNscrolledWindowMarginHeight, XmCScrolledWindowMarginHeight,
	XmRVerticalDimension,
	sizeof(Dimension), Offset(HeightPad),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNspacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(pad),
	XtRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), MGR_Offset(shadow_thickness),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNtraverseObscuredCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(traverseObscuredCallback),
	XmRImmediate, NULL
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNscrolledWindowMarginWidth,
	sizeof(Dimension), Offset(WidthPad),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNscrolledWindowMarginHeight,
	sizeof(Dimension), Offset(HeightPad),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNspacing,
	sizeof(Dimension), Offset(pad),
	_XmFromVerticalPixels, NULL
    }
};

static void PageUp(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);

static void PageDown(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void PageLeft(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void PageRight(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);

static void BeginLine(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);

static void EndLine(Widget w, XEvent *event,
		    String *params, Cardinal *num_params);

static void BeginData(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);

static void EndData(Widget w, XEvent *event,
		    String *params, Cardinal *num_params);

static void PageUpGrab(Widget w, XEvent *event,
		       String *params, Cardinal *num_params);

static void PageDownGrab(Widget w, XEvent *event,
		         String *params, Cardinal *num_params);

static void PageLeftGrab(Widget w, XEvent *event,
		         String *params, Cardinal *num_params);

static void PageRightGrab(Widget w, XEvent *event,
		          String *params, Cardinal *num_params);

static void BeginLineGrab(Widget w, XEvent *event,
		          String *params, Cardinal *num_params);

static void EndLineGrab(Widget w, XEvent *event,
		        String *params, Cardinal *num_params);

static void BeginDataGrab(Widget w, XEvent *event,
		          String *params, Cardinal *num_params);

static void EndDataGrab(Widget w, XEvent *event,
		        String *params, Cardinal *num_params);

static void SWNoop(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);

/* MLM FIX ME -- the clip and work window translations (Transltns.c) are
 * currently unused. */
static XtActionsRec actions[] =
{
    {"SWBeginLine", BeginLine},
    {"SWEndLine", EndLine},
    {"SWTopLine", BeginData},
    {"SWBottomLine", EndData},
    {"SWLeftPage", PageLeft},
    {"SWRightPage", PageRight},
    {"SWUpPage", PageUp},
    {"SWDownPage", PageDown},
    {"SWBeginLineGrab", BeginLineGrab},
    {"SWEndLineGrab", EndLineGrab},
    {"SWTopLineGrab", BeginDataGrab},
    {"SWBottomLineGrab", EndDataGrab},
    {"SWLeftPageGrab", PageLeftGrab},
    {"SWRightPageGrab", PageRightGrab},
    {"SWUpPageGrab", PageUpGrab},
    {"SWDownPageGrab", PageDownGrab},

    {"SWNoop", SWNoop}
};

static XtTranslations comp_clip_trans;

/* *INDENT-OFF* */
XmScrolledWindowClassRec xmScrolledWindowClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmScrolledWindow",
	/* widget_size           */ sizeof(XmScrolledWindowRec),
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
	/* destroy               */ NULL,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager, 
        /* change_managed   */ change_managed, 
        /* insert_child     */ insert_child,
        /* delete_child     */ delete_child,
        /* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,
        /* subresource_count */ 0,
        /* constraint_size   */ 0,
        /* initialize        */ NULL,
        /* destroy           */ NULL,
        /* set_values        */ NULL,
        /* extension         */ NULL,
    },
    /* XmManager class part */
    {
        /* translations                 */ _XmScrolledW_ScrolledWindowXlations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
        /* extension                    */ NULL /*(XtPointer)&_XmScrolledWMClassExtRec*/
    },
    /* XmScrolledWindow part */
    {
	/* extension */ NULL,
    },
};
/* *INDENT-ON* */

WidgetClass xmScrolledWindowWidgetClass = (WidgetClass)&xmScrolledWindowClassRec;

/* T. Straumann: they don't have this method
 *				 we use it however to compile the translation table
 */
static void
class_initialize(void)
{
	comp_clip_trans = XtParseTranslationTable(_XmScrolledW_ClipWindowTranslationTable);
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmScrolledWindowWidgetClass swclass =
	(XmScrolledWindowWidgetClass)widget_class;
    CompositeClassExtension ext, *extptr;

    extptr = (CompositeClassExtension *)
	_XmGetClassExtensionPtr((XmGenericClassExt *)
				&(swclass->composite_class.extension),
				NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = swclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    swclass->composite_class.extension = (XtPointer)ext;
	}
    }
    _XmFastSubclassInit(widget_class, XmSCROLLED_WINDOW_BIT);

}

/*
 * Note
 * The OSF/Motif manual page for XmScrolledWindow describes a "clip" widget
 * and a "work" widget. The Clip widget is really the area that the user sees,
 * whereas the Work widget is the one that is put in the ScrolledWindow.
 * I.e. if you display a large label widget, then that label is the work widget,
 * and the clip widget is something magic that is private to scrolledwindow.
 */
static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:initialize: %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    if (SW_ScrollBarPolicy(new_w) == (unsigned char)XmUNSPECIFIED)
    {
	if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
	{
	    SW_ScrollBarPolicy(new_w) = XmAS_NEEDED;
	}
	else
	{
	    SW_ScrollBarPolicy(new_w) = XmSTATIC;
	}
    }

    if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
    {
	SW_VisualPolicy(new_w) = XmCONSTANT;
    }
    else
    {
	SW_VisualPolicy(new_w) = XmVARIABLE;
    }

    if (SW_Spacing(new_w) == XmINVALID_DIMENSION)
    {
	SW_Spacing(new_w) = 4;
    }

    if (MGR_ShadowThickness(new_w) == XmINVALID_DIMENSION)
    {
	if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
	{
	    MGR_ShadowThickness(new_w) = 2;
	}
	else
	{
	    MGR_ShadowThickness(new_w) = 0;
	}
    }

    if (SW_VisualPolicy(new_w) == XmCONSTANT)
    {

	/* T. Straumann: M*TIF enforces 0 borderWidth */
	SW_ClipWindow(new_w) = (XmDrawingAreaWidget)XtVaCreateManagedWidget(
						"ScrolledWindowClipWindow",
						xmDrawingAreaWidgetClass, new_w, 
						XmNborderWidth,0,
						NULL);

	{
	/* rws 7 Nov 1998
	   This is for xdir Prefs.
	 */
	DA_ResizePolicy(SW_ClipWindow(new_w)) = XmRESIZE_SWINDOW;
	}

	/* T. Straumann: The compiled translation table is dynamically allocated by Xt
	 *				 Unless you intend to free it in destroy, don't compile it here.
	 *				 Since it's constant anyway we do that in class_initialize().
	 *
	 * comp_clip_trans = XtParseTranslationTable(_XmScrolledW_ClipWindowTranslationTable);
	 */
	XtOverrideTranslations((Widget)SW_ClipWindow(new_w), comp_clip_trans);
    }
    else if (SW_VisualPolicy(new_w) != XmVARIABLE)
    {
	SW_VisualPolicy(new_w) = XmVARIABLE;
    }

    if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
    {
	/* till 27.5.1999: Note that we don't copy our colors to the
	 *				   scrollbars anymore. ScrollBar has got a new
	 *				   ResourceDefaultProc to copy our value as a
	 *				   default.
	 *				   This yields correct results also if the
	 *				   scrollbars are added later.
	 */
	SW_VSB(new_w) =
	    (XmScrollBarWidget)XtVaCreateManagedWidget("VertScrollBar",
						xmScrollBarWidgetClass, new_w,
						XmNorientation, XmVERTICAL,
						XmNsliderSize, 100,
	/* T. Straumann: M*TIF enforces 0 borderWidth */
						XmNborderWidth,0,
						NULL);
	SW_HasVSB(new_w) = True;

	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNincrementCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNdecrementCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNpageIncrementCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNpageDecrementCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNdragCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_VSB(new_w),
		      XmNvalueChangedCallback,
		      _XmRepositionScrolledWindow,
		      NULL);

	SW_HSB(new_w) =
	    (XmScrollBarWidget)XtVaCreateManagedWidget("HorScrollBar",
						xmScrollBarWidgetClass, new_w,
						XmNorientation, XmHORIZONTAL,
						XmNsliderSize, 100,
	/* T. Straumann: M*TIF enforces 0 borderWidth */
						XmNborderWidth,0,
						NULL);
	SW_HasHSB(new_w) = True;

	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNincrementCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNdecrementCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNpageIncrementCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNpageDecrementCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNdragCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
	XtAddCallback((Widget)SW_HSB(new_w),
		      XmNvalueChangedCallback,
		      _XmRepositionScrolledWindow,
		      NULL);
    }
    else
    {
	SW_HasHSB(new_w) = False;
	SW_HasVSB(new_w) = False;
	SW_WorkWindow(new_w) = NULL;
	SW_ClipWindow(new_w) = NULL;
	SW_HSB(new_w) = NULL;
	SW_VSB(new_w) = NULL;

    }

    SW_VSBMinimum(new_w) = 0;
    SW_VSBMaximum(new_w) = 0;
    SW_VSBValue(new_w) = 0;
    SW_VSBSliderSize(new_w) = 0;

    SW_HSBMinimum(new_w) = 0;
    SW_HSBMaximum(new_w) = 0;
    SW_HSBValue(new_w) = 0;
    SW_HSBSliderSize(new_w) = 0;

    SW_HSBX(new_w) = 0;
    SW_HSBY(new_w) = 0;
    SW_HSBWidth(new_w) = 0;
    SW_HSBHeight(new_w) = 0;

    SW_VSBX(new_w) = 0;
    SW_VSBY(new_w) = 0;
    SW_VSBWidth(new_w) = 0;
    SW_VSBHeight(new_w) = 0;

    /*
     * MLM -- 60 and 20 I got from comparing with testXm/scrolledw/test1.motif
     * these do appear to be default Motif values.
     *
     * Danny - Trust me, they're supposed to be 100x100.
     *
     * rws 20 Jul 1997
     * Does this really need to be here?? Taking this out lets me avoid the
     * XmIsScrolledWindow junk in Form:constraint_initialize.  I do not think
     * this hurts anything in the tests or any apps.
     *
     * MLM: I have my suspicions about this.  I think what may happen is that
     * query_geometry returns 100x100 iff w/h is 0x0.
     *
     * Once again, Danny's right.  However, adding the ScrollPolicy check
     * makes all the SW tests appear correctly.
     *
     * HOWEVER, if it isn't defined, then xmgr works right (initial display)
     */
    SW_GivenWidth(new_w) = XtWidth(new_w);
    SW_GivenHeight(new_w) = XtHeight(new_w);

    if (SW_GivenWidth(new_w) != 0)
    {
	SW_CWWidth(new_w) = SW_GivenWidth(new_w) - SW_Spacing(new_w);
    }
    else if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
    {
	SW_GivenWidth(new_w) = 100;
    }
    else
    {
	SW_CWWidth(new_w) = 100;
    }
    if (SW_GivenHeight(new_w) != 0)
    {
	SW_CWHeight(new_w) = SW_GivenHeight(new_w) - SW_Spacing(new_w);
    }
    else if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
    {
	SW_GivenHeight(new_w) = 100;
    }
    else
    {
	SW_CWHeight(new_w) = 100;
    }
    SW_CWX(new_w) = 0;
    SW_CWY(new_w) = 0;

    if (SW_VisualPolicy(new_w) == XmVARIABLE &&
	SW_ScrollBarPolicy(new_w) != XmSTATIC)
    {
	SW_ScrollBarPolicy(new_w) = XmSTATIC;
    }

    SW_FromResize(new_w) = False;
    SW_InInit(new_w) = True;

    SW_InInit(new_w) = False;

    /* There's something spooky about highlighting here :
     * Scrollbars are not highlighted by default except when ...
     * they're in an XmAUTOMATIC scrolledwindow.
     * Surprise :-)
     */
    if (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
    {
	if (Prim_HighlightThickness(SW_VSB(new_w)) == 0)
	{
	    Prim_HighlightThickness(SW_VSB(new_w)) = 2;
	    XtWidth(SW_VSB(new_w)) += 4;
	}
	if (Prim_HighlightThickness(SW_HSB(new_w)) == 0)
	{
	    Prim_HighlightThickness(SW_HSB(new_w)) = 2;
	    XtHeight(SW_HSB(new_w)) += 4;
	}
    }
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean r = False;		/* Must I be redisplayed ? */

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

#define	NE(x)	(x(new_w) != x(old))

    if (NE(SW_ClipWindow))
    {
	SW_ClipWindow(new_w) = SW_ClipWindow(old);
	_XmWarning(new_w,
		   "Attempted to change the clipWindow in scrolled window %s",
		   XtName(new_w));
    }

    if (NE(SW_VisualPolicy))
    {
	SW_VisualPolicy(new_w) = SW_VisualPolicy(old);
	_XmWarning(new_w,
		   "Attempted to change the visualPolicy in scrolled window %s",
		   XtName(new_w));
    }

    if (NE(SW_ScrollPolicy))
    {
	SW_ScrollPolicy(new_w) = SW_ScrollPolicy(old);
	_XmWarning(new_w,
	       "Attempted to change the scrollingPolicy in scrolled window %s",
		   XtName(new_w));
    }

    if (NE(SW_HSB))
    {
	if (SW_HSB(new_w) && XtIsManaged(SW_HSB(new_w)))
	{
	    SW_HasHSB(new_w) = True;
	}
	else
	{
	    SW_HasHSB(new_w) = False;
	}
	r = True;
    }

    if (NE(SW_VSB))
    {
	    DEBUGOUT(_LtDebug2(__FILE__, new_w, (Widget)SW_VSB(new_w), "Changed VSB\n"));
	if (SW_VSB(new_w) && XtIsManaged(SW_VSB(new_w)))
	{
	    SW_HasVSB(new_w) = True;
	}
	else
	{
	    SW_HasVSB(new_w) = False;
	}
	r = True;
    }

    if (NE(SW_ScrollBarPolicy)
	|| NE(SW_Placement)
	|| NE(SW_MarginHeight)
	|| NE(SW_MarginWidth)
	|| NE(SW_Spacing))
    {
	r = True;
    }

    if (NE(SW_WorkWindow))
    {
	DEBUGOUT(_LtDebug0(__FILE__, new_w,
			   "Work Window changed: from %s to %s.\n",
			   SW_WorkWindow(old) != NULL
				? XtName(SW_WorkWindow(old))
				: "(null)",
			   SW_WorkWindow(new_w) != NULL
				? XtName(SW_WorkWindow(new_w))
				: "(null)"));
	DEBUGOUT(_LtDebug0(__FILE__, new_w,
			   "Work Window was: %dx%d now %dx%d.\n",
			   SW_WorkWindow(old) != NULL
				? XtWidth(SW_WorkWindow(old))
				: 0,
			   SW_WorkWindow(old) != NULL
				? XtHeight(SW_WorkWindow(old))
				: 0,
			   SW_WorkWindow(new_w) != NULL
				? XtWidth(SW_WorkWindow(new_w))
				: 0,
			   SW_WorkWindow(new_w) != NULL
				? XtHeight(SW_WorkWindow(new_w))
				: 0));
	DEBUGOUT(_LtDebug0(__FILE__, new_w, "... SW_VisualPolicy %s",
			   (SW_VisualPolicy(new_w) == XmCONSTANT)
				? "XmCONSTANT"
				: (SW_VisualPolicy(new_w) == XmVARIABLE)
					? "XmVARIABLE"
					: (SW_VisualPolicy(new_w) ==
					   XmRESIZE_IF_POSSIBLE)
						? "XmRESIZE_IF_POSSIBLE"
						: "???"));
	DEBUGOUT(_LtDebug0(__FILE__, new_w, " SW_ScrollBarPolicy %s",
			   (SW_ScrollBarPolicy(new_w) == XmSTATIC)
			   ? "XmSTATIC"
			   : (SW_ScrollBarPolicy(new_w) == XmAS_NEEDED)
			   ? "XmAS_NEEDED"
			   : "???"));
	DEBUGOUT(_LtDebug0(__FILE__, new_w, " SW_ScrollPolicy %s\n",
			   (SW_ScrollPolicy(new_w) == XmAPPLICATION_DEFINED)
			   ? "XmAPPLICATION_DEFINED"
			   : (SW_ScrollPolicy(new_w) == XmAUTOMATIC)
			   ? "XmAUTOMATIC"
			   : "???"));
	/* rws 6 Jun 1998
	   If we change the work area we definitly need to refresh the
	   display.  This showed as 1x1 <textarea>'s in Mozilla
	 */
	r = True;
    }

    if (XtWidth(new_w) != XtWidth(old))
    {
	SW_GivenWidth(new_w) = XtWidth(new_w);
    }
    if (XtHeight(new_w) != XtHeight(old))
    {
	SW_GivenHeight(new_w) = XtHeight(new_w);
    }
    
    /* init chained from super to sub order */
    /* rws 6 Jun 1998
       If we are not realized do not do all of this geo stuff.  It will be
       done when we get realized.  This ties in with the Mozilla and ml stuff
       above.
     */
    if (r && XtIsRealized(new_w))
    {
	XmSWValues vals;

	_XmScrolledWPreferredSize(new_w, NULL, NULL, &vals);

	/* rws 11 Aug 1997
	   What's this for ??  The preferred size is calculated and then
	   put back to the original before the geom request. This shows up
	   in ml when trying to read a message.
	 */
	/* rws 11 Sep 1998
	   Ok, let's get rid of this!  It is causing an X proto error
	   in xmgr File->Describe.
	vals.SwW = XtWidth(new_w);
	vals.SwH = XtHeight(new_w);
	*/

	_XmScrolledWGeomRequest(new_w, &vals);

	_XmScrolledWLayout(new_w, NULL, NULL, &vals);

	_XmScrolledWConfigureChildren(new_w, NULL, NULL, &vals);
    }

    return r;
}

static void
resize(Widget w)
{
    XmSWValues vals;

    DEBUGOUT(_LtDebug(__FILE__, w, "Resize: x %d y %d w %d h %d\n",
		      XtX(w), XtY(w), XtWidth(w), XtHeight(w)));

    SW_FromResize(w) = True;

    /* resize not chained */
    _XmScrolledWPreferredSize(w, NULL, NULL, &vals);

    /*
    vals.SwW = XtWidth(w);
    vals.SwH = XtHeight(w);
    */

    _XmScrolledWLayout(w, NULL, NULL, &vals);

    _XmScrolledWConfigureChildren(w, NULL, NULL, &vals);

    SW_FromResize(w) = False;
}
static void _XClearArea(Display *di, Window wi, Position x, Position y, Dimension w, Dimension h, Boolean b)
{
	XClearArea(di,wi,x,y,w,h,b);
}

static void
expose(Widget w, XEvent *event, Region region)
{
	/* T. Straumann:  need to clear the margin areas */
	/* clear area on top of the clip */
	_XClearArea(XtDisplay(w),XtWindow(w),
		(Position)0,
		(Position)0,
		(Dimension)(XtWidth(w)),
		(Dimension)(SW_CWY(w) - MGR_ShadowThickness(w)),
		False);
	/* clear area below clip */
	_XClearArea(XtDisplay(w),XtWindow(w),
		(Position)0,
		(Position)(SW_CWY(w) + SW_CWHeight(w) + MGR_ShadowThickness(w)),
		(Dimension)XtWidth(w),
		(Dimension) 0, /* clear all below y */
		False);
	/* clear to the left of clip */
	_XClearArea(XtDisplay(w),XtWindow(w),
		(Position)0,
		(Position)0,
		(Dimension)(SW_CWX(w) - MGR_ShadowThickness(w)),
		(Dimension)(XtHeight(w)),
		False);
	/* clear to the right of clip */
	_XClearArea(XtDisplay(w),XtWindow(w),
		(Position)(SW_CWX(w) + SW_CWWidth(w) + MGR_ShadowThickness(w)),
		(Position)0,
		(Dimension)0, /* all from x to the window width */
		(Dimension)XtHeight(w),
		False);

    _XmRedisplayGadgets(w, event, region);

    _XmDrawShadows(XtDisplay(w), XtWindow(w),
		   MGR_TopShadowGC(w), MGR_BottomShadowGC(w),
		   SW_CWX(w) - MGR_ShadowThickness(w),
		   SW_CWY(w) - MGR_ShadowThickness(w),
		   SW_CWWidth(w) + 2 * MGR_ShadowThickness(w),
		   SW_CWHeight(w) + 2 * MGR_ShadowThickness(w),
		   MGR_ShadowThickness(w), XmSHADOW_IN);
}

static void
realize(Widget w, Mask *value_mask, XSetWindowAttributes *attributes)
{
    /*
    XmSWValues vals;
    */

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:realize(%d) - %dx%d\n",
    	__FILE__, __LINE__,
    	XtWidth(w), XtHeight(w)));

    if (XtWidth(w) == 0 || XtHeight(w) == 0)
    {
    XtWidgetGeometry request;

    	request.request_mode = 0;
    	request.request_mode |= XtWidth(w) == 0 ? CWWidth : 0;
    	request.request_mode |= XtHeight(w) == 0 ? CWHeight : 0;
    	request.width = 100;
    	request.height = 100;
    	_XmMakeGeometryRequest(w, &request);
    }

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass
}

/*
 * Allow other widgets to ask what the preferred geometry of this widget is.
 */
static XtGeometryResult
query_geometry(Widget w,
	       XtWidgetGeometry *intended,
	       XtWidgetGeometry *preferred)
{
    XmSWValues vals;
    XtWidgetGeometry wants;

    DEBUGOUT(_LtDebug(__FILE__, w, "query_geometry(%s)\n",
		      _LtDebugWidgetGeometry2String(intended)));

    wants = *intended;

    /* not chained */
    _XmScrolledWPreferredSize(w, NULL, NULL, &vals);

    /*
    if (vals.SwW < 100) vals.SwW = 100;
    if (vals.SwH < 100) vals.SwH = 100;
    */

    preferred->width = vals.SwW;
    preferred->height = vals.SwH;

    preferred->width = wants.request_mode & CWWidth ? wants.width : vals.SwW;
    preferred->height = wants.request_mode & CWHeight ? wants.height : vals.SwH;

    return _XmGMReplyToQueryGeometry(w, &wants, preferred);
}

static XtGeometryResult
geometry_manager(Widget w, XtWidgetGeometry *desired, XtWidgetGeometry *allowed)
{
    XtWidgetGeometry wants;
    Widget sw = XtParent(w);
    XmSWValues vals;
	XtGeometryResult rval;

    DEBUGOUT(_LtDebug2(__FILE__, sw, w, "geometry_manager (request %s)\n",
		       _LtDebugWidgetGeometry2String(desired)));

    /* Whoa, nelly.  Not if we control what you do. */
    if (w == (Widget)SW_HSB(sw) || w == (Widget)SW_VSB(sw))
    {
	return XtGeometryNo;
    }

#define	Wants(flag)	(desired->request_mode & flag)

    wants = *desired;

    /* We control the XY of all children. Width/Height is all we care about */
	/* T. Straumann: we also care about borderWidth */
    wants.request_mode &= CWWidth | CWHeight | CWBorderWidth;

    /*
     * Special case : Work Window trying to resize itself in XmAUTOMATIC
     */
    if (SW_ScrollPolicy(sw) == XmAUTOMATIC && w == (Widget)SW_ClipWindow(sw))
    {
	DEBUGOUT(_LtDebug2(__FILE__, sw, w,
			   "BEGIN AUTOMATIC FAKE\n\n"));

	DEBUGOUT(_LtDebug2(__FILE__, sw, w,
			   "geometry_manager: resize WorkWindow: %d %d\n",
			   wants.width, wants.height));

	/*
	 * This works as follows.
	 * Remember the SW in automatic mode has a ClipWindow (SW's child)
	 * and a WorkWindow. When created, the workwindow is reparented to
	 * have ClipWindow as parent, meaning it becomes SW's grandchild.
	 *
	 * When WorkWindow tries to resize, it'll ask its parent. ClipWindow
	 * is a XmDrawingArea which has code to look up whether its parent
	 * is a SW in XmAUTOMATIC mode. If so, it'll trigger SW's Geometry-
	 * Manager by XtMakeResizeRequest (which is how we get here), whose
	 * result is not taken into account. Therefore we can get away with
	 * XtGeometryNo as reply.
	 *
	 * We now call _XmScrolledW* here to make it fulfill the
	 * request by resizing WorkWindow (even though it's not a child),
	 * and letting ClipWindow remain the same size.
	 */
	_XmScrolledWPreferredSize(sw, SW_WorkWindow(sw), &wants, &vals);

	_XmScrolledWLayout(sw, SW_WorkWindow(sw), &wants, &vals);

	_XmConfigureScrollBars(sw, NULL, NULL, &vals);

	_XmFixupScrollBars(sw, vals.WorkW, vals.WorkH);

	_XmScrolledWConfigureChildren(sw, SW_WorkWindow(sw), &wants, &vals);

	/* rws 23 Aug 1997
	 * Taking this out gets the initial display of mfm correct and
	 * does not seem to bother anything else
	 */
#ifndef MFM_BUGS
	_XmRepositionScrolledWindow(w, NULL, NULL);
#endif

	DEBUGOUT(_LtDebug2(__FILE__, sw, w, "END AUTOMATIC FAKE\n"));

	return XtGeometryNo;
    }

    /* not chained */
    _XmScrolledWPreferredSize(sw, w, &wants, &vals);

	/* T. Straumann: we only should ask the parent for a size change
	 *               if we are willing to reconfigure our children.
	 *               Otherwise we may end up with more space, then do the
	 *               layout, which possibly modifies `wants' leading to
	 *               rejection of the child request (--> old configuration
	 *               with new ScrolledW window size)
	 */
	_XmScrolledWLayout(sw,w,&wants,&vals);

	rval = (	(Wants(CWWidth)		 &&	(wants.width != desired->width))
			||	(Wants(CWHeight)		 &&	(wants.height != desired->height))
			||	(Wants(CWBorderWidth) && (wants.border_width != desired->border_width)) )
			? XtGeometryAlmost : XtGeometryYes;

	/* T. Straumann: we also are able to manage the border width */
    wants.request_mode &= desired->request_mode & (CWWidth|CWHeight|CWBorderWidth);

    *allowed = wants;

    /* Strip off unwanted bits */
    allowed->request_mode &= wants.request_mode;


    if (Wants(XtCWQueryOnly))
    {
	_XmWarning(sw, "XmScrolledWindow: geometry_manager QueryOnly"
		   "not implemented (child %s, class %s)",
		   XtName(w), XtClass(w)->core_class.class_name);

	DEBUGOUT(_LtDebug2(__FILE__, sw, w,
		   "geometry_manager QueryOnly not implemented ! (=> YES)\n"));

	return XtGeometryYes;
    }

	/* T. Straumann: if (wants(x) && wants(y) && !(wants(w) && wants(h)) )
	 * didn't make sense to me. Should never pass this test
	 * anyway if you look at the Layout code.
	 */
    if ( (Wants(CWX) || Wants(CWY)) && !(Wants(CWWidth) || Wants(CWHeight) || Wants(CWBorderWidth)))
    {
	DEBUGOUT(_LtDebug2(__FILE__, sw, w, "GeometryManager => No 2\n"));
	return XtGeometryNo;
    }

    if (Wants((CWWidth|CWHeight)) == (CWWidth|CWHeight) &&
	wants.width == desired->width && wants.height == desired->height)
    {
	wants.request_mode &= ~CWWidth;
	wants.request_mode &= ~CWHeight;
	DEBUGOUT(_LtDebug2(__FILE__, sw, w, "GeometryManager => Yes-1 %s\n",
		_LtDebugWidgetGeometry2String(allowed)));
    }
    else if (Wants((CWWidth|CWHeight)) == CWWidth &&
	wants.width == desired->width)
    {
	wants.request_mode &= ~CWWidth;
	DEBUGOUT(_LtDebug2(__FILE__, sw, w, "GeometryManager => Yes-2 %s\n",
		_LtDebugWidgetGeometry2String(allowed)));
    }
    else if (Wants((CWWidth|CWHeight)) == CWHeight &&
	wants.height == desired->height)
    {
	wants.request_mode &= ~CWHeight;
	DEBUGOUT(_LtDebug2(__FILE__, sw, w, "GeometryManager => Yes-3 %s\n",
		_LtDebugWidgetGeometry2String(allowed)));
    }
	else if (rval == XtGeometryYes)
	{
	/* T. Straumann: change in border width only */
	DEBUGOUT(_LtDebug2(__FILE__, sw, w, "GeometryManager => Yes-4 %s\n",
		_LtDebugWidgetGeometry2String(allowed)));
	}
	else
	{
    DEBUGOUT(_LtDebug2(__FILE__, sw, w, "GeometryManager -> Almost %s\n",
		_LtDebugWidgetGeometry2String(allowed)));
	}


	if ( rval == XtGeometryYes)
	{
    if ( _XmScrolledWGeomRequest(sw, &vals) == XtGeometryYes)
    {
#if 1
	/* rws 25 Mar 1998
	 * This makes the mgv page list size correctly
	 *
	 * jonf 08 Apr 1998
	 * I think I've fixed that problem, so I put this back in.
	 */
	/* T. Straumann: should do layout before asking parent for new geometry */
	_XmScrolledWConfigureChildren(sw, w, &wants, &vals);
#else
	_XmScrolledWLayout(sw, NULL, NULL, &vals);
#endif
    }
    else
    {
	DEBUGOUT(_LtDebug2(__FILE__, sw, w, "GeometryManager => No 1\n"));
	return XtGeometryNo;
    }
	}

    return rval;
}

/*
 * This method is called in peculiar ways.
 * Apart from the normal cases (I guess), XmDrawingArea "forwards" calls to its own
 * change_managed() method to this one.
 *
 * Important about this is that we're managing our grandchildren in that case.
 */
static void
change_managed(Widget w)
{
	XmSWValues vals;

	DEBUGOUT(_LtDebug(__FILE__, w, "SW change_managed()\n"));
	DEBUGOUT(_LtDebugPrintManagedChildren(__FILE__, w, "Child"));
	/* The clip is always child #0 */
#if 0
	DEBUGOUT(_LtDebugPrintManagedChildren(__FILE__, MGR_Children(w)[0], "GrandChild"));
#endif

	/*
	 * SW_ScrollPolicy
	 */
	if (SW_ScrollPolicy(w) == XmAPPLICATION_DEFINED) {
		Widget	child;
		int	i;

		for (i=0, child=0; i<MGR_NumChildren(w) && child == NULL; i++)
			if (! XmIsScrollBar(MGR_Children(w)[i]))
				child = MGR_Children(w)[i];
		if (child) {
			DEBUGOUT(_LtDebug2(__FILE__, w, child, "CM %d %d\n",
				XtIsManaged(w), XtIsManaged(child)));
		} else {
			DEBUGOUT(_LtDebug2(__FILE__, w, child, "CM %d child NULL\n",
				XtIsManaged(w)));
		}
#if 1
		if (child && XtIsManaged(w) && !XtIsManaged(child)) {
			DEBUGOUT(_LtDebug2(__FILE__, w, child, "Unmanage SW\n"));
			XtUnmanageChild(w);
		} else if (child && XtIsManaged(child) && !XtIsManaged(w)) {
			DEBUGOUT(_LtDebug2(__FILE__, w, child, "Manage SW\n"));
			XtManageChild(w);
		}
#endif
	}
#if 0
	{
	/* HACK */
		Widget	child;
		int	i;
		for (i=0, child=0; i<MGR_NumChildren(w) && child == NULL; i++) {
			DEBUGOUT(_LtDebug2(__FILE__, w, MGR_Children(w)[i], "Try this ..\n"));
			if ((MGR_Children(w)[i] != (Widget)SW_ClipWindow(w))
					&& ! XmIsScrollBar(MGR_Children(w)[i]))
				child = MGR_Children(w)[i];
		}
		DEBUGOUT(_LtDebug2(__FILE__, w, child, "ChangeManaged GOTCHA\n"));
	}
#endif
	if (True || XtIsManaged(w)) {
		_XmScrolledWPreferredSize(w, NULL, NULL, &vals);

		if (_XmScrolledWGeomRequest(w, &vals) == XtGeometryYes) {
			_XmScrolledWLayout(w, NULL, NULL, &vals);
			_XmScrolledWConfigureChildren(w, NULL, NULL, &vals);
		}

		DEBUGOUT(_LtDebug(__FILE__, w, "change_managed: size %d %d\n",
			  XtWidth(w), XtHeight(w)));
	}
#if 0
	/* Looks like this is dead code */
	else {
		if (SW_ScrollBarPolicy(w) != XmSTATIC) {
			DEBUGOUT(_LtDebug(__FILE__, w,
				"change_managed: unmanaging sb's\n"));

			if (SW_HSB(w) && XtIsManaged((Widget)SW_HSB(w))) {
				XtUnmanageChild((Widget)SW_HSB(w));
			}
			if (SW_VSB(w) && XtIsManaged((Widget)SW_VSB(w))) {
				XtUnmanageChild((Widget)SW_VSB(w));
			}
		}
	}
#endif
}

static void AddScrollWheelMagic(Widget child)
{
	static XtTranslations trans = NULL;
	if (trans == NULL) {
		trans = XtParseTranslationTable(
			"<Btn4Down>,<Btn4Up>: SWUpPage(half)\n"
			"<Btn5Down>,<Btn5Up>: SWDownPage(half)\n"
			);
	}
	XtOverrideTranslations(child, trans);

}

/*
 * In here we probably need some trickery to make things work.
 * One sensible thing to do is to make the work widget the
 * child of the clip widget. This is ugly, though. That's probably the
 * reason why the X Consortium never wanted to add XtReparentWidget().
 *
 * The advantage of reparenting the work widget is, all you have to do
 * to scroll is tell the work widget where it is. Through the magic of
 * the X window system having window hierarchies, things should work.
 */
static void
insert_child(Widget w)
{
    XmScrolledWindowWidget sw = (XmScrolledWindowWidget)XtParent(w);

#define superclass (&xmManagerClassRec)

    /* Special first case : these are not the work window
     * Note this is especially here for our subclass XmMainWindow */
    if (XmIsSeparator(w) || XmIsSeparatorGadget(w) ||
	(XmIsRowColumn(w) && RC_Type(w) == XmMENU_BAR))
    {
	(*superclass->composite_class.insert_child) (w);
    }
    else if (XmIsScrolledWindow(w))
    {
	/*
	 * Second special case (for XmMainWindow).
	 * Avoid reparenting stuff which is already a scrolledwindow (child).
	 * It's unlikely to become the workarea anyway.
	 * Example : Danny's (Motif) clone of Ghostview.
	 * 28/8/1997.
	 */
	(*superclass->composite_class.insert_child) (w);
    }
    else if (XmIsScrollBar(w))
    {
	/* T. Straumann: M*TIF enforces 0 borderWidth on SW children
	 *				 (except the work window)
	 */
	if ( 0 != XtBorderWidth(w) )
	{
		XtVaSetValues(w,XmNborderWidth,0,NULL);
	}
	(*superclass->composite_class.insert_child) (w);

	switch (SCB_Orientation(w))
	{
	case XmHORIZONTAL:
	    SW_HSB(sw) = (XmScrollBarWidget)w;
	    if (XtIsManaged(w))
	    {
		SW_HasHSB(w) = True;
	    }
	    else
	    {
		SW_HasHSB(w) = False;
	    }
	    break;

	case XmVERTICAL:
	    SW_VSB(sw) = (XmScrollBarWidget)w;
	    if (XtIsManaged(w))
	    {
		SW_HasVSB(w) = True;
	    }
	    else
	    {
		SW_HasVSB(w) = False;
	    }
	    break;

	default:
	    _XmWarning((Widget)sw,
		       "Can't determine ScrollBar orientation in "
		       "ScrolledWindow %s: Not adding",
		       XtName(sw));
	    break;
	}
    }
    else if (XtIsShell(w))
    {
	/* Shells cannot be the work area - so make sure we catch them here */
	(*superclass->composite_class.insert_child) (w);
    }
    /*
     * only we can add a clip window, and we know it's id
     */
    else if (SW_VisualPolicy(sw) == XmCONSTANT && SW_ClipWindow(sw) == NULL)
    {
	(*superclass->composite_class.insert_child) (w);
	/*
	 * Stefan Birgersson <birger@musculus.mednet.gu.se> suggests to
	 * assign the clip window here to aid in MainWindow.
	 */
	SW_ClipWindow(sw) = (XmDrawingAreaWidget)w;
    }
    /*
     * THIS IS SOO UGLY
     *
     * if we're not XmCONSTANT, don't reparent
     *
     */
    else if (SW_VisualPolicy(sw) == XmCONSTANT)
    {
	DEBUGOUT(_LtDebug2(__FILE__, (Widget)sw, (Widget)w, "Reparented to %s\n",
			   XtName(SW_ClipWindow(sw))));

	XtParent(w) = (Widget)SW_ClipWindow(sw);

	/*
	 * Now we're reparented. Go on inserting the child as if
	 * nothing happened
	 */
	(*superclass->composite_class.insert_child) (w);

	SW_WorkWindow(sw) = w;
	AddScrollWheelMagic(w);
    }
    else if (SW_WorkWindow(sw) == NULL)
    {
	DEBUGOUT(_LtDebug2(__FILE__, (Widget)sw, w, "Child is Work Window\n"));

	(*superclass->composite_class.insert_child) (w);

	SW_WorkWindow(sw) = w;
	AddScrollWheelMagic(w);
    }
    else
    {
	DEBUGOUT(_LtDebug2(__FILE__, (Widget)sw, w, "Child is goofy\n"));

	(*superclass->composite_class.insert_child) (w);
	AddScrollWheelMagic(w);
    }
#undef superclass
}

static void
delete_child(Widget w)
{
    /* Motif inherits this method */
    Widget sw = XtParent(w);

    if (SW_WorkWindow(sw) == w)
    {
	SW_WorkWindow(sw) = NULL;
    }
    else if ((Widget)SW_ClipWindow(sw) == w)
    {
	SW_ClipWindow(sw) = NULL;
    }
    else if ((Widget)SW_VSB(sw) == w)
    {
	SW_VSB(sw) = NULL;
    }
    else if ((Widget)SW_HSB(sw) == w)
    {
	SW_HSB(sw) = NULL;
    }

#define superclass (&xmManagerClassRec)
    (*superclass->composite_class.delete_child) (w);
#undef superclass
}

void
_XmRepositionScrolledWindow(Widget w, XtPointer client, XtPointer call)
{
    int h = 0, v = 0;
    Widget sw = XtParent(w);

    if (SW_VSB(sw))
    {
	if ((call != NULL) && ((Widget)SW_VSB(sw) == w))
	{
	    v = ((XmScrollBarCallbackStruct *)call)->value;
	}
	else {
	    XtVaGetValues((Widget)SW_VSB(sw), XmNvalue, &v, NULL);
	}

	SW_VSBValue(sw) = v;
    }
    else
    {
	SW_VSBValue(sw) = 0;
    }

    if (SW_HSB(sw))
    {
	if ((call != NULL) && ((Widget)SW_HSB(sw) == w))
	{
	    h = ((XmScrollBarCallbackStruct *)call)->value;
	}
	else {
	    XtVaGetValues((Widget)SW_HSB(sw), XmNvalue, &h, NULL);
	}

	SW_HSBValue(sw) = h;
    }
    else
    {
	SW_HSBValue(sw) = 0;
    }

    DEBUGOUT(_LtDebug2(__FILE__, sw, w, "_XmRepositionScrolledWindow Hor %d Vert %d\n", h, v));

    /*
     * Position the work window
     */
    if (SW_WorkWindow(sw))
    {
	XtMoveWidget(SW_WorkWindow(sw), -h, -v);
    }
}

/*
 * to get here, we have to have gone through the layout function.  All our
 * instance variable should have reasonable values.
 */
void
_XmFixupScrollBars(Widget w, Dimension ww, Dimension wh)
{
    int value, max, min, percent, n;
    Arg args[5];

    if (!SW_ClipWindow(w))
    {
	_XmWarning(w,
		   "Requested to do scrolling without a clip window: %s",
		   XtName(w));

	return;
    }

    /*
     * see if we have a work window.  If not, pick some reasonable defaults
     */
    if (!SW_WorkWindow(w))
    {
	if (SW_HasHSB(w))
	{
	    XtVaGetValues((Widget)SW_HSB(w),
			  XmNmaximum, &max,
			  XmNminimum, &min,
			  NULL);
	    XtVaSetValues((Widget)SW_HSB(w),
			  XmNsliderSize, max - min,
			  XmNvalue, min /* rws 21 Jun 1997 0 */ ,
			  NULL);
	}
	if (SW_HasVSB(w))
	{
	    XtVaGetValues((Widget)SW_VSB(w),
			  XmNmaximum, &max,
			  XmNminimum, &min,
			  NULL);
	    XtVaSetValues((Widget)SW_VSB(w),
			  XmNsliderSize, max - min,
			  XmNvalue, min /* rws 21 Jun 1997 0 */ ,
			  NULL);
	}
	return;
    }

    /*
     * otherwise, fixup the scrollbars.
     */
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "FixupScrollBars Widths : Work %d Clip %d\n",
		      ww, SW_CWWidth(w)));
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "FixupScrollBars Heights : Work %d Clip %d\n",
		      wh, SW_CWHeight(w)));

    if (SW_HasHSB(w))
    {
	if (ww <= SW_CWWidth(w) || ww == 0)
	{
	    percent = 100;
	    max = 100;
	    min = 0;
	}
	else
	{
	    max = ww;
	    min = 0;
	    percent = (((SW_CWWidth(w) * 10000) / ww) * ww) / 10000;
	}
	percent = percent > max - min ? max - min : percent;
	percent = percent < 1 ? 1 : percent;

	n = 0;
	XtSetArg(args[n], XmNminimum, min); n++;
	XtSetArg(args[n], XmNmaximum, max); n++;
	XtSetArg(args[n], XmNsliderSize, percent); n++;
	if (SW_CWWidth(w) > 0)
	{
	    if (SW_CWWidth(w) > max)
	    {
		XtSetArg(args[n], XmNpageIncrement, max);
		n++;
	    }
	    else
	    {
		XtSetArg(args[n], XmNpageIncrement, SW_CWWidth(w));
		n++;
	    }
	}

	XtVaGetValues((Widget)SW_HSB(w),
		XmNvalue, &value,
		NULL);
	value = value > max - percent ? max - percent : value;
	value = value < min ? min : value;
	XtSetArg(args[n], XmNvalue, value); n++;
	XtSetValues((Widget)SW_HSB(w), args, n);

	SW_HSBMinimum(w) = min;
	SW_HSBMaximum(w) = max;
	SW_HSBSliderSize(w) = percent;

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "FixupScrollBars HSB min %d max %d percent %d\n",
			  min, max, percent));
    }
    if (SW_HasVSB(w))
    {
	if (wh <= SW_CWHeight(w) || wh == 0)
	{
	    percent = 100;
	    max = 100;
	    min = 0;
	}
	else
	{
	    max = wh;
	    min = 0;
	    percent = (((SW_CWHeight(w) * 10000) / wh) * wh) / 10000;
	}
	percent = percent > max - min ? max - min : percent;
	percent = percent < 1 ? 1 : percent;

	n = 0;
	XtSetArg(args[n], XmNminimum, min); n++;
	XtSetArg(args[n], XmNmaximum, max); n++;
	XtSetArg(args[n], XmNsliderSize, percent); n++;
	if (SW_CWHeight(w) > 0)
	{
	    if (SW_CWHeight(w) > max)
	    {
		XtSetArg(args[n], XmNpageIncrement, max);
		n++;
	    }
	    else
	    {
		XtSetArg(args[n], XmNpageIncrement, SW_CWHeight(w));
		n++;
	    }
	}

	XtVaGetValues((Widget)SW_VSB(w),
		XmNvalue, &value,
		NULL);
	value = value > max - percent ? max - percent : value;
	value = value < min ? min : value;
	XtSetArg(args[n], XmNvalue, value); n++;
	XtSetValues((Widget)SW_VSB(w), args, n);

	SW_VSBMinimum(w) = min;
	SW_VSBMaximum(w) = max;
	SW_VSBSliderSize(w) = percent;

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "FixupScrollBars VSB min %d max %d percent %d value %d\n",
			  min, max, percent, value));
    }
}

static void
SetMinimum(Widget w)
{
    int m;

    XtVaGetValues(w, XmNminimum, &m, NULL);
    XtVaSetValues(w, XmNvalue, m, NULL);
}

/*
 * _XmScrolledWPreferredSize
 *
 * The widget w is a ScrolledWindow widget.
 * When in XmAUTOMATIC mode, a XmDrawingArea widget has been created by
 *      XmScrolledWindow; this is the clip widget. A widget which is created
 *      as child of the XmScrolledWindow is reparented (see insert_child)
 *      so it really becomes child of the XmDrawingArea.
 * In XmAPPLICATION_DEFINED, the child widget has to take care of most
 *      scrolling all by itself. It can be expected that only two widgets
 *      (XmList and XmText) will ever really succeed in implementing this.
 *
 * To avoid confusion : SW_FromResize(w) is True only when
 *      called from the resize() method, meaning that we cannot resize
 *      the XmScrolledWindow itself.
 *      Actually one exception : in query_geometry, we run this in test
 *      mode, so SW_FromResize(w) doesn't really matter.
 *
 * Rules for combinations of resource settings :
 *
 * Possible values :
 *      ParentResize (True, False) : depends on which method calls us
 *              can be anything in any of the cases described below.
 *      SW_VisualPolicy (XmCONSTANT, XmVARIABLE, XmRESIZE_IF_POSSIBLE)
 *      SW_ScrollBarPolicy (XmAS_NEEDED, XmSTATIC)
 *      SW_ScrollPolicy (XmAPPLICATION_DEFINED, XmAUTOMATIC)
 *
 * Some combinations of the above are not possible.
 *
 * From the XmScrolledWindow(3) man page in OSF/Motif 2.0 :
 * - if XmNscrollBarDisplayPolicy == XmAS_NEEDED and
 *      XmNscrollingPolicy == XmAUTOMATIC then scrollbars are displayed only
 *      if the workspace exceeds the clip area
 * - if XmNscrollBarDisplayPolicy == XmSTATIC then scrollbars are always
 *      shown
 * - if XmNscrollingPolicy == XmAPPLICATION_DEFINED
 *      then XmNscrollBarDisplayPolicy == XmSTATIC
 * - if XmNscrollingPolicy == XmAUTOMATIC
 *      then default XmNscrollBarDisplayPolicy == XmAS_NEEDED,
 *      otherwise default XmNscrollBarDisplayPolicy == XmSTATIC
 * - if XmNscrollingPolicy == XmAUTOMATIC then XmNvisualPolicy == XmCONSTANT
 *      and automatically create scrollbars
 * - if XmNscrollingPolicy == XmAPPLICATION_DEFINED then client must
 *      create scrollbars, add callbacks for them, redisplay, etc.
 * - if visualPolicy == XmVARIABLE then XmSTATIC and allow the work area
 *      to grow & shrink as it wants, but clip window stays same size.
 *      Only resize if requested by parent.
 * - default visualPolicy == XmCONSTANT when scrollingPolicy == XmAUTOMATIC
 *      and visualPolicy == XmVARIABLE otherwise
 *
 * XmNscrollBarDisplayPolicy is CSG (create, set, get)
 * XmNscrollingPolicy is CG (cannot be altered after creation)
 * XmNvisualPolicy is CG (cannot be altered after creation)
 *
 * What do they mean ?
 * 1a. XmNvisualPolicy == XmCONSTANT
 *      The work area grows or shrinks as requested, but a clipping window
 *      forces the size of the visible portion to remain constant.
 *      The only time the viewing area can grow is in response to a resize
 *      from the ScrolledWindow's parent.
 * 1b. XmNvisualPolicy == XmVARIABLE
 *      Allows the work area to grow or shrink at any time and adjusts SW's
 *      layout to accommodate the new size.
 * 1c. XmNvisualPolicy == XmRESIZE_IF_POSSIBLE
 *      ???     The symbol exists but nobody currently uses it.
 * 2a. SW_ScrollBarPolicy == XmAS_NEEDED
 *      Scrollbars are displayed only when needed.
 * 2b. SW_ScrollBarPolicy == XmSTATIC
 *      Scrollbars are always shown.
 * 3a. SW_ScrollPolicy == XmAPPLICATION_DEFINED
 *      Child widget or application does scrolling.
 * 3b. SW_ScrollPolicy == XmAUTOMATIC
 *      Scrolling is automatically handled by ScrolledWindow.
 *
 * Bug #993209 - don't trust the contents of SW_WorkWindow() but treat all grandchildren
 * in the same way.
 */
void
_XmScrolledWPreferredSize(Widget w, Widget child, XtWidgetGeometry *childgeom,
			  XmSWValues *vals)
{
	int	i, count;

	/* Print out how we're called */
	DEBUGOUT(_LtDebug2(__FILE__, w, child, "XmScrolledWPreferredSize [size %d %d]\n",
				XtWidth(w), XtHeight(w)));
	DEBUGOUT(_LtDebug0(__FILE__, w, "... WorkWindow %s, %s managed\n",
			SW_WorkWindow(w) ? XtName(SW_WorkWindow(w)) : "NULL",
			(SW_WorkWindow(w) && XtIsManaged(SW_WorkWindow(w))) ? "is" : "not"));
	DEBUGOUT(_LtDebug0(__FILE__, w, "... SW_VisualPolicy %s",
				_LtDebugVisualPolicy2String((SW_VisualPolicy(w)))));
	DEBUGOUT(_LtDebug0(__FILE__, w, " SW_ScrollBarPolicy %s",
				_LtDebugSBDisplayPolicy2String((SW_ScrollBarPolicy(w)))));
	DEBUGOUT(_LtDebug0(__FILE__, w, " SW_ScrollPolicy %s\n",
				_LtDebugScrollPolicy2String((SW_ScrollPolicy(w)))));

	if (childgeom) {
		/* T. Straumann: allow changes of border width of the work window */
		if (child == SW_WorkWindow(w)) {
			childgeom->request_mode &=
				(CWWidth | CWHeight | CWBorderWidth | XtCWQueryOnly);
		} else {
			childgeom->request_mode &= (CWWidth | CWHeight | XtCWQueryOnly);
		}
	}

	/* Initialize all local variables to reflect their widgets */
	memset(vals, 0, sizeof(XmSWValues));

	if (SW_VSB(w)) {
		/*
		* Don't be confused by HasHSB having similar name to SW_HasHSB().
		* HasHSB is true if the widget exists.
		* ShowHSB is used to manage/unmanage the scrollbar.
		*/
		vals->HasVSB = True;
		vals->ShowVSB = False;
		vals->VsbX = XtX(SW_VSB(w));
		vals->VsbY = XtY(SW_VSB(w));
		vals->VsbW = XtWidth(SW_VSB(w));
		vals->VsbH = XtHeight(SW_VSB(w));
	}

	if (SW_HSB(w)) {
		vals->HasHSB = True;
		vals->ShowHSB = False;
		vals->HsbX = XtX(SW_HSB(w));
		vals->HsbY = XtY(SW_HSB(w));
		vals->HsbW = XtWidth(SW_HSB(w));
		vals->HsbH = XtHeight(SW_HSB(w));
	}

	vals->ClipX = vals->ClipY = vals->WorkX = vals->WorkY = 0;
	vals->ClipW = vals->ClipH = vals->WorkW = vals->WorkH = 0;

#ifdef	FIX_993209
	if (SW_WorkWindow(w) && XtIsManaged(SW_WorkWindow(w))) {
		vals->WorkX = 0;
		vals->WorkY = 0;

		if (SW_VisualPolicy(w) == XmVARIABLE && XtIsRealized(SW_WorkWindow(w))) {
			XtWidgetGeometry workgeo;

			XtQueryGeometry(SW_WorkWindow(w),
				child == SW_WorkWindow(w) ? childgeom : NULL,
				&workgeo);
			vals->WorkW = workgeo.width + 2 * workgeo.border_width;
			vals->WorkH = workgeo.height + 2 * workgeo.border_width;
		} else {
			vals->WorkW = XtWidth(SW_WorkWindow(w)) + 2 * XtBorderWidth(SW_WorkWindow(w));
			vals->WorkH = XtHeight(SW_WorkWindow(w)) + 2 * XtBorderWidth(SW_WorkWindow(w));
		}

		DEBUGOUT(_LtDebug2(__FILE__, w, (Widget)SW_WorkWindow(w),
					"WorkWindow queried geo is W %d H %d X %d Y %d\n",
					vals->WorkW, vals->WorkH, vals->WorkX, vals->WorkY));
	} else {
		vals->WorkX = 0;
		vals->WorkY = 0;
		vals->WorkW = 100;
		vals->WorkH = 100;
	}
	count = 0;
	if (SW_ClipWindow(w))
		DEBUGOUT(_LtDebug(__FILE__, w, "YOW99 nc %d\n", MGR_NumChildren(SW_ClipWindow(w))));
	else
		DEBUGOUT(_LtDebug(__FILE__, w, "YOW99 noclip\n"));
	for (i=0; SW_ClipWindow(w) && i<MGR_NumChildren(SW_ClipWindow(w)); i++) {
		Widget	ch = MGR_Children(SW_ClipWindow(w))[i];

		if (ch && XtIsManaged(ch)) {
			count++;
			vals->WorkX = 0;
			vals->WorkY = 0;

			if (SW_VisualPolicy(w) == XmVARIABLE && XtIsRealized(ch)) {
				XtWidgetGeometry workgeo;

				XtQueryGeometry(ch, child == ch ? childgeom : NULL,
					&workgeo);
				vals->WorkW = workgeo.width + 2 * workgeo.border_width;
				vals->WorkH = workgeo.height + 2 * workgeo.border_width;
			} else {
				vals->WorkW = XtWidth(ch) + 2 * XtBorderWidth(ch);
				vals->WorkH = XtHeight(ch) + 2 * XtBorderWidth(ch);
			}

			DEBUGOUT(_LtDebug2(__FILE__, w, ch,
				"WorkWindow queried geo is W %d H %d X %d Y %d\n",
				vals->WorkW, vals->WorkH, vals->WorkX, vals->WorkY));
		}
	}
#else
	if (SW_WorkWindow(w) && XtIsManaged(SW_WorkWindow(w))) {
		vals->WorkX = 0;
		vals->WorkY = 0;

		if (SW_VisualPolicy(w) == XmVARIABLE && XtIsRealized(SW_WorkWindow(w))) {
			XtWidgetGeometry workgeo;

			XtQueryGeometry(SW_WorkWindow(w),
				child == SW_WorkWindow(w) ? childgeom : NULL,
				&workgeo);
			vals->WorkW = workgeo.width + 2 * workgeo.border_width;
			vals->WorkH = workgeo.height + 2 * workgeo.border_width;
		} else {
			vals->WorkW = XtWidth(SW_WorkWindow(w)) + 2 * XtBorderWidth(SW_WorkWindow(w));
			vals->WorkH = XtHeight(SW_WorkWindow(w)) + 2 * XtBorderWidth(SW_WorkWindow(w));
		}

		DEBUGOUT(_LtDebug2(__FILE__, w, (Widget)SW_WorkWindow(w),
					"WorkWindow queried geo is W %d H %d X %d Y %d\n",
					vals->WorkW, vals->WorkH, vals->WorkX, vals->WorkY));
	} else {
		vals->WorkX = 0;
		vals->WorkY = 0;
		vals->WorkW = 100;
		vals->WorkH = 100;
	}
#endif

	/*
	 * Below, SwW/SwH are initialised from XtWidth/XtHeight.
	 */
	if (SW_VisualPolicy(w) == XmCONSTANT) {
		vals->SwW = XtWidth(w) == 0 ? 100 : XtWidth(w);
		vals->SwH = XtHeight(w) == 0 ? 100 : XtHeight(w);
	} else {
		/*
		 * T. Straumann: at creation time, their scrolledw isn't really
		 * variable but it sets the size (scrolledwindow/test19).
		 */
		if (XtIsRealized(w) && !SW_FromResize(w)) {
			vals->SwW = 0;
			vals->SwH = 0;
		} else {
			vals->SwW = XtWidth(w);
			vals->SwH = XtHeight(w);
		}
	}

	if (childgeom)
	{
		/* only happens for APPLICATION_DEFINED */
		/* T. Straumann: NOT TRUE;
		 * if the work window asks for a size change, we get here
		 * anyway.
		 */
		if (child == (Widget)SW_WorkWindow(w)) {
			/* T. Straumann: added support for borderWidth
			 * (border width of scrollbars and clip are 0)
			 */
			Dimension bw, ww, hh;
			bw = 2 * (childgeom->request_mode & CWBorderWidth
				? childgeom->border_width
				: XtBorderWidth(child) );
			ww = childgeom->request_mode & CWWidth
				? childgeom->width : XtWidth(child);
			hh = childgeom->request_mode & CWHeight
				? childgeom->height : XtHeight(child);
			vals->WorkW = ww + bw;
			vals->WorkH = hh + bw;
		}

		/* only happens for AUTOMATIC */
		if (child == (Widget)SW_ClipWindow(w)) {
			if (childgeom->request_mode & CWWidth) {
				vals->ClipW = childgeom->width;
			}
			if (childgeom->request_mode & CWHeight) {
				vals->ClipH = childgeom->height;
			}
		}
		if (child == (Widget)SW_HSB(w)) {
			if (childgeom->request_mode & CWWidth) {
				vals->HsbW = childgeom->width;
			}
			if (childgeom->request_mode & CWHeight) {
				vals->HsbH = childgeom->height;
			}
		}
		if (child == (Widget)SW_VSB(w)) {
			if (childgeom->request_mode & CWWidth) {
				vals->VsbW = childgeom->width;
			}
			if (childgeom->request_mode & CWHeight) {
				vals->VsbH = childgeom->height;
			}
		}
	}

	if (SW_ScrollPolicy(w) == XmAPPLICATION_DEFINED) {
		if (SW_ScrollBarPolicy(w) != XmSTATIC) {	/* Sanity check */
			_XmWarning(w, "_XmScrolledWPreferredSize: XmAPPLICATION_DEFINED"
					" but not XmSTATIC");

			SW_ScrollBarPolicy(w) = XmSTATIC;
		}

		if (SW_WorkWindow(w) && XtIsManaged(SW_WorkWindow(w))) {
			vals->ShowHSB = SW_HSB(w) ? XtIsManaged(SW_HSB(w)) : False;
			vals->ShowVSB = SW_VSB(w) ? XtIsManaged(SW_VSB(w)) : False;
		} else {
			vals->ShowHSB = False;
			vals->ShowVSB = False;
		}

		/* If we're being resized (grown), make sure child follows */
		if (vals->WorkW + 2 * SW_MarginWidth(w) + 2 * MGR_ShadowThickness(w) < vals->SwW) {
			Dimension xx = vals->WorkW;

			vals->WorkW = vals->SwW - 2 * SW_MarginWidth(w)
				- 2 * MGR_ShadowThickness(w)
				- (vals->ShowVSB ? (SW_Spacing(w) + vals->VsbW) : 0);

			DEBUGOUT(_LtDebug2(__FILE__, w, SW_WorkWindow(w), "Grow WorkW %d -> %d\n",
						xx, vals->WorkW));
		}
		if (vals->WorkH + 2 * SW_MarginHeight(w) + 2 * MGR_ShadowThickness(w) < vals->SwH) {
			Dimension xx = vals->WorkH;

			vals->WorkH = vals->SwH - 2 * SW_MarginHeight(w)
				- 2 * MGR_ShadowThickness(w)
				- (vals->ShowHSB ? (SW_Spacing(w) + vals->HsbH) : 0);

			DEBUGOUT(_LtDebug2(__FILE__, w, SW_WorkWindow(w), "Grow WorkH %d -> %d\n",
						xx, vals->WorkH));
		}

		/* If child is bigger than we are, grow (if we can) */
		if (!SW_FromResize(w) && SW_VisualPolicy(w) == XmVARIABLE ) {
			/* rws 25 Mar 1998
			 * Where does this 3 come from??? Just like FileSB:ListPreferredWidth
			 * we need a fudge factor in here to make the mgv page list come out
			 * the correct size.
			 * 
			 * jonf 08 Apr 1998
			 * The 3 is no longer needed, since the mgv page list problem is now
			 * fixed.
			 */

			/* T. Straumann: their scrolledW doesn't allow the child to
			 * grow at creation time (scrolledwindow/test19).
			 */
			if ( (XtIsRealized(w) || vals->SwW == 0 )
				&& /*3 +*/ vals->WorkW + 2 * (SW_MarginWidth(w) +
					MGR_ShadowThickness(w)) + (vals->ShowVSB ? (SW_Spacing(w)
						+ vals->VsbW) : 0) > vals->SwW) {
				Dimension xx = vals->SwW;

				vals->SwW =/* 3 +*/ vals->WorkW +
					2 * (SW_MarginWidth(w) + MGR_ShadowThickness(w)) +
					(vals->ShowVSB ? (SW_Spacing(w) + vals->VsbW) : 0);

				DEBUGOUT(_LtDebug2(__FILE__, w, SW_WorkWindow(w),
					"Grow SwW %d -> %d\n", xx, vals->SwW));
			}

			/* T. Straumann: their scrolledW doesn't allow the child to
			 * grow at creation time (scrolledwindow/test19).
			 */
			if ( (XtIsRealized(w) || vals->SwH == 0 ) &&
				vals->WorkH + 2 * (SW_MarginHeight(w) + MGR_ShadowThickness(w)) +
				(vals->ShowHSB ? (SW_Spacing(w) + vals->HsbH) : 0) > vals->SwH) {
				Dimension xx = vals->SwH;

				vals->SwH = vals->WorkH +
					2 * (SW_MarginHeight(w) + MGR_ShadowThickness(w)) +
					(vals->ShowHSB ? (SW_Spacing(w) + vals->HsbH) : 0);

				DEBUGOUT(_LtDebug2(__FILE__, w, SW_WorkWindow(w),
					"Grow SwH %d -> %d\n", xx, vals->SwH));
			}
		}
	} else /* SW_ScrollPolicy(w) == XmAUTOMATIC */ {
		/* We're in charge ! */

		if (SW_ScrollBarPolicy(w) == XmSTATIC) { /* Always show */
			vals->ShowHSB = vals->ShowVSB = True;
		} else {
			/* do we need to display scrollbars ? */
			/* T. Straumann: the need for one bar might urge for the
			 * second as well.
			 * Note that the state 'need scrollbars'
			 * is bi-stable for certain window sizes. I.e. `scrollbars
			 * are needed if they're there and not needed if they are not
			 * there' - almost philosophical isn't it :-). This obviously is due
			 * to the space requirement of the scrollbars themselves.
			 *
			 * This algorithm makes sure scrollbars are only shown if
			 * they are _really_ needed.
			 */
			int wtot = vals->WorkW + 2 * (SW_MarginWidth(w) + MGR_ShadowThickness(w));
			int htot = vals->WorkH + 2 * (SW_MarginHeight(w) + MGR_ShadowThickness(w));
#ifdef I_WANT_THEIR_SUCKING_EXTRA_SEP
			int bw   = vals->HasHSB ? Prim_HighlightThickness(SW_HSB(w)) : 2;
			int bh   = vals->HasVSB ? Prim_HighlightThickness(SW_VSB(w)) : 2;
#else
#define bw 0
#define bh 0
#endif
			vals->ShowHSB = (wtot > vals->SwW)
				||	((htot > vals->SwH)
					&& (wtot + bw + SW_Spacing(w) + vals->VsbW > vals->SwW));
			vals->ShowVSB = (htot > vals->SwH)
				||	((wtot > vals->SwW)
					&& (htot + bh + SW_Spacing(w) + vals->HsbH > vals->SwH));
#ifdef bw
#undef bw
#endif
#ifdef bh
#undef bh
#endif
		}
	}

	/*
	 * What follows is code independent of XmAUTOMATIC/XmAPPLICATION_DEFINED
	 *
	 * Try to resize ourselves if necessary (and if we're allowed to).
	 */
	if (SW_VisualPolicy(w) != XmCONSTANT && !SW_FromResize(w) &&
			(vals->SwW == 0 || vals->SwH == 0)) {
		if ( vals->SwW==0 ) {
			vals->SwW = SW_GivenWidth(w)
				? SW_GivenWidth(w)
				: vals->WorkW + 2 * SW_MarginWidth(w) + 2 * MGR_ShadowThickness(w);
		}
		if ( vals->SwH==0 ) {
			vals->SwH = SW_GivenHeight(w)
				? SW_GivenHeight(w)
				: vals->WorkH + 2 * SW_MarginHeight(w) + 2 * MGR_ShadowThickness(w);
		}
	}
}

XtGeometryResult
_XmScrolledWGeomRequest(Widget w, XmSWValues *vals)
{
    XtWidgetGeometry request;
    XtGeometryResult res;

    request.request_mode = CWWidth | CWHeight;
    request.width = vals->SwW;
    request.height = vals->SwH;

    DEBUGOUT(_LtDebug(__FILE__, w, "Request geo %s from parent %s %s\n",
		      _LtDebugWidgetGeometry2String(&request),
		      XtName(XtParent(w)),
		      XtClass(XtParent(w))->core_class.class_name));

    res = _XmMakeGeometryRequest(w, &request);

    DEBUGOUT(_LtDebug(__FILE__, w, "==> Got %s %s\n",
		      _LtDebugGeometryResult2String(res),
		      _LtDebugWidgetGeometry2String(&request)));

    if (res == XtGeometryYes || res == XtGeometryDone)
    {
       /* 15-Jun-04 - dwilliss - Geometry request may have returned Yes, but only after
       * rejecting one or the other change - If so, take what we can and return Almost */
        vals->SwW = (request.request_mode & CWWidth) ? request.width : XtWidth(w);
	vals->SwH = (request.request_mode & CWHeight) ? request.height : XtHeight(w);
	if ((request.request_mode & (CWWidth|CWHeight)) != (CWWidth|CWHeight)) res = XtGeometryAlmost;
    }
    else
    {
	vals->SwW = XtWidth(w);
	vals->SwH = XtHeight(w);
    }

    return res;
}

/*
 *
 * If the geometry you got above is different than requested, adapt
 * the clip window's geometry to it.
 * (Is treated in the switch statement below.)
 * 
 * Figure out the layout, based on placement policy.
 * Note all modifications are done to SWValues members; they're only done
 * to the widgets later on in this function if not in test mode.
 *
 * Rules of this game :
 * The clip window is surrounded by a shadow whose size is
 * MGR_ShadowThickness.
 * Scrollbars and the clip window are separated by an area whose size is
 * SW_Spacing + MGR_ShadowThickness (see above).
 * In the presence of a scrollbar, both sides of the clip area adjacent to
 * it always stay an extra 2 pixels from the side.
 * MGR_MarginWidth/MGR_Marginheight form a border within which everything
 * else should be counted.
 * Scrollbars are not separated from the side by the 2 pixels mentioned
 * above.
 *
 * Note 12/4/1997: the 2 pixels noted above are actually the highlight
 * thickness of the scrollbars.
 */
void
_XmScrolledWLayout(Widget w, Widget child, XtWidgetGeometry *childgeom,
		   XmSWValues *vals)
{
    Dimension bw, bh;
	int		  SWY = vals->SwY + SW_MarginHeight(w);

    /*
     * Motif has certain layout details that don't seem to be documented other
     * than by looking at the behavior of their implementation.
     * One aspect is the presence of borders depending on whether you have
     * horizontal/vertical scrollbars.
     * Another is the actual size of those borders. The two statements below
     * took me weeks to figure out, because it turned out that our scrollbars
     * weren't traversable (which means they didn't highlight), so it actually
     * looked like the scrollbars were in the right place. Sigh.
     *
     * jonf  08 Apr 1998
     *	Is there a test in test/Xm that will show the need for 
     *	these adjustments?
	 *
	 * T. Straumann 3/1/99
	 *  Believe the actual size of 'those borders' is just
	 *  just the highlightThickness ???
	 *  In case there is only ONE scrollbar managed,
	 *  their layout sometimes just goes haywire.
	 *  Well, create one of their ScrolledWindows and
	 *  play around with different VertScrollBar.highlightThickness
	 *  / HorScrollBar.highlightThickness settings. Sit back
	 *  and enjoy their layouting magic.
	 *  Take, e.g. scrolledwindow/test10, and set
	 *
	 *    VertScrollBar.highlightThickness: 0
	 *    HorScrollBar.highlightThickness: 10
	 *
	 *  Note that the vertical scrollbar is neither needed nor 
	 *  shown. Then monotonically increase the hT of the VSB 
	 *  the `extra' space between clipWindow and scrollBar
	 *  will first be negative then it will increase and then
	 *  shrink again.
	 *  I don't feel like spending a lot of time trying to imitate
	 *  this sucking behavior. We should just do it better :-)
	 *  Still, if I_WANT_THEIR_SUCKING_EXTRA_SEP is defined, the extra
	 *  space will be there; however, don't expect behavior to be
	 *  the same as theirs for unequal highlightThicknes settings
	 *  of VSB and HSB.
     */

    bw = vals->HasHSB ? Prim_HighlightThickness(SW_HSB(w)) : 2;
    bh = vals->HasVSB ? Prim_HighlightThickness(SW_VSB(w)) : 2;

    switch (SW_Placement(w))
    {
    case XmTOP_RIGHT:
	/* Independent of scrollbars actually being there */
	vals->HsbY = SWY;
	vals->VsbX = vals->SwW - vals->VsbW - SW_MarginWidth(w);
	/* T. Straumann: no need for bh */
	vals->VsbY = vals->HsbY + vals->HsbH + SW_Spacing(w);
	vals->HsbX = SW_MarginWidth(w);

	break;

    case XmBOTTOM_LEFT:
	/* Independent of scrollbars actually being there */
	/* T. Straumann: respect margin height */
	vals->HsbY = vals->SwH - vals->HsbH - SW_MarginHeight(w);
	vals->VsbX = SW_MarginWidth(w);
	vals->VsbY = SWY;
	/* T. Straumann: no need for bw */
	vals->HsbX = vals->VsbX + vals->VsbW + SW_Spacing(w);

	break;

    case XmTOP_LEFT:
	/* Independent of scrollbars actually being there */
	vals->HsbY = SWY;
	vals->VsbX = SW_MarginWidth(w);
	/* T. Straumann: added spacing, no need for bw/bh */
	vals->VsbY = vals->HsbY + vals->HsbH + SW_Spacing(w);
	vals->HsbX = vals->VsbX + vals->VsbW + SW_Spacing(w);

	break;

    case XmBOTTOM_RIGHT:
    default:
	/* Independent of scrollbars actually being there */
	vals->HsbX = SW_MarginWidth(w);
	vals->HsbY = vals->SwH - vals->HsbH - SW_MarginHeight(w);
	vals->VsbY = SWY;
	vals->VsbX = vals->SwW - vals->VsbW - SW_MarginWidth(w);

	break;
    }

    /*
     * this part is independent of the locations
     */
    /* A starting point */
    vals->ClipY = vals->VsbY + MGR_ShadowThickness(w) + bh;
    vals->ClipX = vals->HsbX + MGR_ShadowThickness(w) + bw;

    if (vals->ShowHSB && vals->HasHSB)
    {
	if (vals->ShowVSB && vals->HasVSB)
	{
	    vals->ClipW = vals->SwW - 2 * bw - 2 * MGR_ShadowThickness(w)
		    - SW_Spacing(w) - vals->VsbW - 2 * SW_MarginWidth(w);
	    vals->ClipH = vals->SwH - 2 * bh - 2 * MGR_ShadowThickness(w)
		    - SW_Spacing(w) - vals->HsbH - SW_MarginHeight(w) - SWY;

	    vals->VsbH = vals->ClipH + 2 * bh + 2 * MGR_ShadowThickness(w);
	    vals->HsbW = vals->ClipW + 2 * bw + 2 * MGR_ShadowThickness(w);
	}
	else
	{
	    vals->ClipY -= bh;
		/* T. Straumann: new clipX, HsbX if vertical bar not visible */
		vals->ClipX = (vals->HsbX = SW_MarginWidth(w)) + MGR_ShadowThickness(w) + bw;
		/* T. Straumann: added margin width */
	    vals->ClipW = vals->SwW - 2 * bw 
			- 2 * (MGR_ShadowThickness(w) + SW_MarginWidth(w));
#ifdef I_WANT_THEIR_SUCKING_EXTRA_SEP
		/* T. Straumann: well, the - 1 * bh isn't really necessary, but
		 * it seems we are behaving strictly M*TIF here
		 */
	    vals->ClipH = vals->SwH - 1 * bh - 2 * MGR_ShadowThickness(w)
		    - SW_Spacing(w) - vals->HsbH - SW_MarginHeight(w) - SWY;
#else
	    vals->ClipH = vals->SwH - 2 * MGR_ShadowThickness(w)
		    - SW_Spacing(w) - vals->HsbH - SW_MarginHeight(w) - SWY;
#endif

	    vals->VsbH = vals->ClipH + 2 * MGR_ShadowThickness(w) + 2 * bh;
	    vals->HsbW = vals->ClipW + 2 * MGR_ShadowThickness(w) + 2 * bw;
	}
    }
    else if (vals->ShowVSB && vals->HasVSB)
    {
	/* T. Straumann: horizontal bar not visible -> new clipY, VsbY, ClipX */
	vals->ClipX -= bw;
	vals->ClipY = (vals->VsbY = SWY ) + MGR_ShadowThickness(w) + bh;
#ifdef I_WANT_THEIR_SUCKING_EXTRA_SEP
	/* T. Straumann: see comment above on 1 * bh */
	vals->ClipW = vals->SwW - 1 * bw - 2 * MGR_ShadowThickness(w)
		- SW_Spacing(w) - vals->VsbW - 2 * SW_MarginWidth(w);
#else
	vals->ClipW = vals->SwW - 2 * MGR_ShadowThickness(w)
		- SW_Spacing(w) - vals->VsbW - 2 * SW_MarginWidth(w);
#endif
	vals->ClipH = vals->SwH - 2 * bh - 2 * MGR_ShadowThickness(w)
	    - SW_MarginHeight(w) - SWY;

	vals->VsbH = vals->ClipH + 2 * MGR_ShadowThickness(w) + 2 * bh;
	vals->HsbW = vals->ClipW + 2 * MGR_ShadowThickness(w) + 2 * bw;
    }
    else
    {
	/* No scrollbars -> no borders anywhere */
	vals->ClipX = SW_MarginWidth(w) + MGR_ShadowThickness(w);
	vals->ClipY = SWY + MGR_ShadowThickness(w);
	vals->ClipW = vals->SwW - vals->ClipX - SW_MarginWidth(w)
		    - MGR_ShadowThickness(w);
	vals->ClipH = vals->SwH - vals->ClipY - SW_MarginHeight(w)
		    - MGR_ShadowThickness(w);

	vals->VsbH = vals->ClipH + 2 * MGR_ShadowThickness(w);
	vals->HsbW = vals->ClipW + 2 * MGR_ShadowThickness(w);
    }

    if (vals->HasHSB && !vals->ShowHSB)
    {
	SetMinimum((Widget)SW_HSB(w));
    }

    if (vals->HasVSB && !vals->ShowVSB)
    {
	SetMinimum((Widget)SW_VSB(w));
    }

    DEBUGOUT(_LtDebug0(__FILE__, w, "##############################\n"));
    DEBUGOUT(_LtDebug2(__FILE__, w, (Widget)SW_ClipWindow(w),
		       "Clip @ x %d y %d w %d h %d\n",
		       vals->ClipX, vals->ClipY, vals->ClipW, vals->ClipH));
    DEBUGOUT(_LtDebug0(__FILE__, w,
		       "HSB @ x %d y %d w %d h %d\n",
		       vals->HsbX, vals->HsbY, vals->HsbW, vals->HsbH));
    DEBUGOUT(_LtDebug0(__FILE__, w,
		       "VSB @ x %d y %d w %d h %d\n",
		       vals->VsbX, vals->VsbY, vals->VsbW, vals->VsbH));
    DEBUGOUT(_LtDebug0(__FILE__, w,
		       "HasHSB %s ShowHSB %s HasVSB %s ShowVSB %s\n",
		       vals->HasHSB ? "True" : "False",
		       vals->ShowHSB ? "True" : "False",
		       vals->HasVSB ? "True" : "False",
		       vals->ShowVSB ? "True" : "False"));
    DEBUGOUT(_LtDebug0(__FILE__, w,
	      "MarginWidth %d MarginHeight %d ShadowThickness %d Spacing %d\n",
		       SW_MarginWidth(w), SW_MarginHeight(w),
		       MGR_ShadowThickness(w), SW_Spacing(w)));
    DEBUGOUT(_LtDebug0(__FILE__, w,
		       "SwW %d SwH %d\n", vals->SwW, vals->SwH));
    DEBUGOUT(_LtDebug0(__FILE__, w, "##############################\n"));

    if (SW_ScrollPolicy(w) == XmAPPLICATION_DEFINED)
    {
	vals->WorkW = vals->ClipW;
	vals->WorkH = vals->ClipH;
	vals->WorkX = vals->ClipX;
	vals->WorkY = vals->ClipY;
    }

    /*
     * In the code above (all of the switch statement really), some
     * variables can become negative where we don't want them to.
     * Fix that here by giving them their original value from the
     * widget resource they represent.
     */
    if (vals->ClipH < 0)
    {
	vals->ClipH = SW_ClipWindow(w) ? XtHeight(SW_ClipWindow(w)) : 0;
    }
    if (vals->ClipW < 0)
    {
	vals->ClipW = SW_ClipWindow(w) ? XtWidth(SW_ClipWindow(w)) : 0;
    }
    if (vals->WorkH < 0)
    {
	vals->WorkH = SW_WorkWindow(w) ? XtHeight(SW_WorkWindow(w)) : 0;
    }
    if (vals->WorkW < 0)
    {
	vals->WorkW = SW_WorkWindow(w) ? XtWidth(SW_WorkWindow(w)) : 0;
    }
    if (vals->VsbH < 0)
    {
	vals->VsbH = SW_VSB(w) ? XtHeight(SW_VSB(w)) : 0;
    }
    if (vals->VsbW < 0)
    {
	vals->VsbW = SW_VSB(w) ? XtWidth(SW_VSB(w)) : 0;
    }
    if (vals->HsbH < 0)
    {
	vals->HsbH = SW_HSB(w) ? XtHeight(SW_HSB(w)) : 0;
    }
    if (vals->HsbW < 0)
    {
	vals->HsbW = SW_HSB(w) ? XtWidth(SW_HSB(w)) : 0;
    }

    if (child != NULL)
    {
	if (child == (Widget)SW_WorkWindow(w))
	{
		/* T. Straumann: we only get here if called by geometry_manager and if
		 * the workWindow instigated a change.
		 * Since there is no field in vals for
		 * the workWindow's border width, we should preserve the
		 * border width flag and value passed in childgeom.
		 */
		childgeom->request_mode &= CWBorderWidth;
	    childgeom->request_mode |= CWX | CWY | CWHeight | CWWidth;
	    childgeom->x = vals->WorkX;
	    childgeom->y = vals->WorkY;
	    childgeom->width = vals->WorkW;
	    childgeom->height = vals->WorkH;
	}
	if (child == (Widget)SW_ClipWindow(w))
	{
	    childgeom->request_mode = CWX | CWY | CWHeight | CWWidth;
	    childgeom->x = vals->ClipX;
	    childgeom->y = vals->ClipY;
	    childgeom->width = vals->ClipW;
	    childgeom->height = vals->ClipH;
	}
	if (child == (Widget)SW_HSB(w))
	{
	    childgeom->request_mode = CWX | CWY | CWHeight | CWWidth;
	    childgeom->x = vals->HsbX;
	    childgeom->y = vals->HsbY;
	    childgeom->width = vals->HsbW;
	    childgeom->height = vals->HsbH;
	}
	if (child == (Widget)SW_VSB(w))
	{
	    childgeom->request_mode = CWX | CWY | CWHeight | CWWidth;
	    childgeom->x = vals->VsbX;
	    childgeom->y = vals->VsbY;
	    childgeom->width = vals->VsbW;
	    childgeom->height = vals->VsbH;
	}

	DEBUGOUT(_LtDebug2(__FILE__, w, child, "feedback => %s\n",
			   _LtDebugWidgetGeometry2String(childgeom)));
    }
}

void
_XmConfigureScrollBars(Widget w, Widget child, XtWidgetGeometry *childgeom,
		    XmSWValues *vals)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmConfigureScrollBars(%d)\n",
    	__FILE__, __LINE__));
    /* Don't display scrollbars unless the child is managed */
    if (SW_WorkWindow(w) && ! XtIsManaged(SW_WorkWindow(w)))
    {
	vals->VsbX = vals->SwW;
	vals->VsbH = vals->SwH + SW_Spacing(w);

	vals->HsbY = vals->SwH;
	vals->HsbW = vals->SwW + SW_Spacing(w);
	/* rws 11 Nov 1998
	   budget
	 */
	vals->ShowVSB = False;
	vals->ShowHSB = False;
	vals->ClipW = vals->SwW - SW_Spacing(w);
	vals->ClipH = vals->SwH - SW_Spacing(w);
    }

    /*
     * Not sure what we need to do here in the XmAPPLICATION_DEFINED
     * case.
     * Need to manage/unmanage ?
     * Need to configure scrollbar geometry ?
     */
    /* Actions ordered to be as fast as possible */
    if (vals->HasVSB)
    {
	if (SW_ScrollPolicy(w) == XmAUTOMATIC)
	{
	    if (XtIsManaged(SW_VSB(w)) && !vals->ShowVSB)
	    {
		DEBUGOUT(_LtDebug2(__FILE__, w, (Widget)SW_VSB(w),
				   "Unmanaging VSB\n"));

		vals->VsbX = vals->SwW;
		vals->VsbH = vals->SwH;
		/* rws 12 Oct 1998
		   scrolledwindow/test10 shows that these do not get UnManaged
		   just pushed off to the side
		XtUnmanageChild((Widget)SW_VSB(w));
		SW_HasVSB(w) = False;
		*/
	    }
	}

	if (child == (Widget)SW_VSB(w))
	{
	    XtX(SW_VSB(w)) = vals->VsbX;
	    XtY(SW_VSB(w)) = vals->VsbY;
	    XtWidth(SW_VSB(w)) = vals->VsbW;
	    XtHeight(SW_VSB(w)) = vals->VsbH;
	}
	else
	{
	    _XmConfigureObject((Widget)SW_VSB(w),
			       vals->VsbX, vals->VsbY,
			       vals->VsbW ? vals->VsbW : 1,
			       vals->VsbH ? vals->VsbH : 1,
			       XtBorderWidth(SW_VSB(w)));
	}

	SW_VSBWidth(w) = vals->VsbW;
	SW_VSBHeight(w) = vals->VsbH;
	SW_VSBX(w) = vals->VsbX;
	SW_VSBY(w) = vals->VsbY;

	if (SW_ScrollPolicy(w) == XmAUTOMATIC)
	{
	    if (vals->ShowVSB && !XtIsManaged(SW_VSB(w)))
	    {
		DEBUGOUT(_LtDebug2(__FILE__, w, (Widget)SW_VSB(w),
				   "Managing VSB\n"));

		if (True || XtIsRealized(w))
		{
		    XtManageChild((Widget)SW_VSB(w));
		    SW_HasVSB(w) = True;
		}
		else
		{
		    SW_HasVSB(w) = False;
		}
	    }
	}
    }

    if (vals->HasHSB)
    {			/* Actions ordered to be as fast as possible */
	if (SW_ScrollPolicy(w) == XmAUTOMATIC)
	{
	    if (XtIsManaged(SW_HSB(w)) && !vals->ShowHSB)
	    {
		DEBUGOUT(_LtDebug2(__FILE__, w, (Widget)SW_HSB(w),
				   "Unmanaging HSB\n"));

		vals->HsbY = vals->SwH;
		vals->HsbW = vals->SwW;
		/* rws 12 Oct 1998
		   scrolledwindow/test10 shows that these do not get UnManaged
		   just pushed off to the side
		XtUnmanageChild((Widget)SW_HSB(w));
		SW_HasHSB(w) = False;
		*/
	    }
	}

	if (child == (Widget)SW_HSB(w))
	{
	    XtX(SW_HSB(w)) = vals->HsbX;
	    XtY(SW_HSB(w)) = vals->HsbY;
	    XtWidth(SW_HSB(w)) = vals->HsbW;
	    XtHeight(SW_HSB(w)) = vals->HsbH;
	}
	else
	{
	    _XmConfigureObject((Widget)SW_HSB(w),
			       vals->HsbX, vals->HsbY,
			       vals->HsbW ? vals->HsbW : 1,
			       vals->HsbH ? vals->HsbH : 1,
			       XtBorderWidth(SW_HSB(w)));
	}

	SW_HSBWidth(w) = vals->HsbW;
	SW_HSBHeight(w) = vals->HsbH;
	SW_HSBX(w) = vals->HsbX;
	SW_HSBY(w) = vals->HsbY;

	if (SW_ScrollPolicy(w) == XmAUTOMATIC)
	{
	    if (vals->ShowHSB && !XtIsManaged(SW_HSB(w)))
	    {
		DEBUGOUT(_LtDebug2(__FILE__, w, (Widget)SW_HSB(w),
				   "Managing HSB\n"));

		if (True || XtIsRealized(w))
		{
		    XtManageChild((Widget)SW_HSB(w));
		    SW_HasHSB(w) = True;
		}
		else
		{
		    SW_HasHSB(w) = False;
		}
	    }
	}
    }
}

/*
 * Bug #993209 - make sure we deal with more than just one (grand)child.
 */
void
_XmScrolledWConfigureChildren(Widget w, Widget child, XtWidgetGeometry *childgeom, XmSWValues *vals)
{
	int	i;

	if (_LtDebugInDebug(__FILE__, w)) {
		DEBUGOUT(_LtDebug2(__FILE__, w, child,
				  "_XmScrolledWConfigureChildren: Sw %dx%d\n",
				  vals->SwW, vals->SwH));
		DEBUGOUT(_LtDebug0(__FILE__, w,
				   "\t\tX\tY\tW\tH\n\tWork\t%d\t%d\t%d\t%d\n",
				   vals->WorkX, vals->WorkY, vals->WorkW, vals->WorkH));
		DEBUGOUT(_LtDebug0(__FILE__, w,
				   "\tClip\t%d\t%d\t%d\t%d\n",
				   vals->ClipX, vals->ClipY, vals->ClipW, vals->ClipH));
		DEBUGOUT(_LtDebug0(__FILE__, w,
				   "\tHsb\t%d\t%d\t%d\t%d\t%3s managed\n",
				   vals->HsbX, vals->HsbY, vals->HsbW, vals->HsbH,
				   (vals->ShowHSB && vals->HasHSB) ? "" : "not"));
		DEBUGOUT(_LtDebug0(__FILE__, w,
				   "\tVsb\t%d\t%d\t%d\t%d\t%3s managed\n",
				   vals->VsbX, vals->VsbY, vals->VsbW, vals->VsbH,
				   (vals->ShowVSB && vals->HasVSB) ? "" : "not"));
	}

	/*
	 * Configure our direct children first.
	 */
	if (SW_WorkWindow(w) && XtIsManaged(SW_WorkWindow(w))) {
		if (child == SW_WorkWindow(w)) {
			XtX(SW_WorkWindow(w)) = vals->WorkX;
			XtY(SW_WorkWindow(w)) = vals->WorkY;
			/*
			 * T. Straumann: configure border width (no field in vals, we take the
			 * value passed in childgeom, if valid)
			 */
			if ( childgeom->request_mode & CWBorderWidth ) {
				XtBorderWidth(SW_WorkWindow(w)) = childgeom->border_width;
			}
			XtWidth(SW_WorkWindow(w)) = vals->WorkW
				- 2 * XtBorderWidth(SW_WorkWindow(w));
			XtHeight(SW_WorkWindow(w)) = vals->WorkH
				- 2 * XtBorderWidth(SW_WorkWindow(w));
		} else {
			_XmConfigureObject(SW_WorkWindow(w),
				vals->WorkX, vals->WorkY,
				vals->WorkW - 2 * XtBorderWidth(SW_WorkWindow(w)),
				vals->WorkH - 2 * XtBorderWidth(SW_WorkWindow(w)),
				XtBorderWidth(SW_WorkWindow(w)));
		}

		_XmConfigureScrollBars(w, child, childgeom, vals);

		if (SW_ClipWindow(w)) {
			if (child == (Widget)SW_ClipWindow(w)) {
				XtX(SW_ClipWindow(w)) = vals->ClipX;
				XtY(SW_ClipWindow(w)) = vals->ClipY;
				XtWidth(SW_ClipWindow(w)) = vals->ClipW;
				XtHeight(SW_ClipWindow(w)) = vals->ClipH;
			} else {
				_XmConfigureObject((Widget)SW_ClipWindow(w),
					vals->ClipX, vals->ClipY,
					vals->ClipW, vals->ClipH,
					XtBorderWidth(SW_ClipWindow(w)));
			}

			SW_CWX(w) = vals->ClipX;
			SW_CWY(w) = vals->ClipY;
			SW_CWWidth(w) = vals->ClipW;
			SW_CWHeight(w) = vals->ClipH;
		}

		if (SW_ScrollPolicy(w) != XmAUTOMATIC) {
			SW_CWX(w) = vals->ClipX;
			SW_CWY(w) = vals->ClipY;
			SW_CWWidth(w) = vals->ClipW;
			SW_CWHeight(w) = vals->ClipH;
		}
	} else {
		_XmConfigureScrollBars(w, child, childgeom, vals);
	}
	
	/*
	 * Now look at our grandchildren.
	 */
#ifdef	FIX_993209
	for (i=0; SW_ClipWindow(w) && i<MGR_NumChildren(SW_ClipWindow(w)); i++) {
		Widget	ch = MGR_Children(SW_ClipWindow(w))[i];
		if (ch && XtIsManaged(ch)) {
			DEBUGOUT(_LtDebug2(__FILE__, w, ch,
					"_XmScrolledWConfigureChildren: treat child\n"));
			if (child == ch) {
				XtX(ch) = vals->WorkX;
				XtY(ch) = vals->WorkY;
				/* T. Straumann: configure border width (no field in vals, we take
				 * the value passed in childgeom, if valid)
				 */
				if ( childgeom->request_mode & CWBorderWidth ) {
					XtBorderWidth(ch) = childgeom->border_width;
				}
				XtWidth(ch) = vals->WorkW - 2 * XtBorderWidth(ch);
				XtHeight(ch) = vals->WorkH - 2 * XtBorderWidth(ch);
			} else {
				_XmConfigureObject(ch,
						vals->WorkX, vals->WorkY,
						vals->WorkW - 2 * XtBorderWidth(ch),
						vals->WorkH - 2 * XtBorderWidth(ch),
						XtBorderWidth(ch));
			}

			_XmConfigureScrollBars(w, child, childgeom, vals);

			if (SW_ClipWindow(w)) {
				if (child == (Widget)SW_ClipWindow(w)) {
					XtX(SW_ClipWindow(w)) = vals->ClipX;
					XtY(SW_ClipWindow(w)) = vals->ClipY;
					XtWidth(SW_ClipWindow(w)) = vals->ClipW;
					XtHeight(SW_ClipWindow(w)) = vals->ClipH;
				} else {
					_XmConfigureObject((Widget)SW_ClipWindow(w),
						vals->ClipX, vals->ClipY,
						vals->ClipW, vals->ClipH,
						XtBorderWidth(SW_ClipWindow(w)));
				}

				SW_CWX(w) = vals->ClipX;
				SW_CWY(w) = vals->ClipY;
				SW_CWWidth(w) = vals->ClipW;
				SW_CWHeight(w) = vals->ClipH;
			}

			if (SW_ScrollPolicy(w) != XmAUTOMATIC) {
				SW_CWX(w) = vals->ClipX;
				SW_CWY(w) = vals->ClipY;
				SW_CWWidth(w) = vals->ClipW;
				SW_CWHeight(w) = vals->ClipH;
			}
		}
	}
#endif
	/*
	 * MLM: We may not have either child.  Avoid messy exposes 10/29/97
	 */
	if (!SW_ClipWindow(w) && !SW_WorkWindow(w)) {
		SW_CWX(w) = vals->ClipX;
		SW_CWY(w) = vals->ClipY;
		SW_CWWidth(w) = vals->ClipW;
		SW_CWHeight(w) = vals->ClipH;
	}

	if (SW_ScrollPolicy(w) == XmAUTOMATIC) {
		_XmFixupScrollBars(w, vals->WorkW, vals->WorkH);
		_XmRepositionScrolledWindow((Widget)SW_ClipWindow(w), NULL, NULL);
	}
}

static String moves[2] =
{
    "0",
    "1"
};
#define PAGE_UP		&moves[0]
#define PAGE_DOWN	&moves[0]
#define PAGE_LEFT	&moves[1]
#define PAGE_RIGHT	&moves[1]

static void
PageUp(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	int	count = 5;

	for (; count > 0; w = XtParent(w))
		if (XmIsScrolledWindow(w))
			break;

	if (! XmIsScrolledWindow(w))
		return;

	if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasVSB(w)) {
		XtCallActionProc((Widget)SW_VSB(w), "PageUpOrLeft", event, PAGE_UP, 1);
		DEBUGOUT(_LtDebugAction(__FILE__, w, "PageUp", params, num_params));
	} else {
		DEBUGOUT(_LtDebugAction(__FILE__, w, "PageUp -> no action", params, num_params));
	}
}

static void
PageDown(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	int	count = 5;

	for (; count > 0; w = XtParent(w))
		if (XmIsScrolledWindow(w))
			break;

	if (! XmIsScrolledWindow(w))
		return;

	if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasVSB(w)) {
		DEBUGOUT(_LtDebugAction(__FILE__, w, "PageDown", params, num_params));
		XtCallActionProc((Widget)SW_VSB(w), "PageDownOrRight", event, PAGE_DOWN, 1);
	} else {
		DEBUGOUT(_LtDebugAction(__FILE__, w, "PageDown -> no action", params, num_params));
	}
}

static void
PageLeft(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasHSB(w)) {
		DEBUGOUT(_LtDebugAction(__FILE__, w, "PageLeft", params, num_params));
		XtCallActionProc((Widget)SW_HSB(w), "PageUpOrLeft", event, PAGE_LEFT, 1);
	} else {
		DEBUGOUT(_LtDebugAction(__FILE__, w, "PageLeft -> no action", params, num_params));
	}
}

static void
PageRight(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasHSB(w)) {
		DEBUGOUT(_LtDebugAction(__FILE__, w, "PageRight", params, num_params));
		XtCallActionProc((Widget)SW_HSB(w), "PageDownOrRight", event, PAGE_RIGHT, 1);
	} else {
		DEBUGOUT(_LtDebugAction(__FILE__, w, "PageRight -> no action", params, num_params));
	}
}

static void
BeginLine(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasHSB(w))
    {
	XtCallActionProc((Widget)SW_HSB(w), "TopOrBottom",
			 event, params, *num_params);
    }
}

static void
EndLine(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasHSB(w))
    {
	XtCallActionProc((Widget)SW_HSB(w), "TopOrBottom",
			 event, params, *num_params);
    }
}

static void
BeginData(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasVSB(w))
    {
	XtCallActionProc((Widget)SW_VSB(w), "TopOrBottom",
			 event, params, *num_params);
    }
}

static void
EndData(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (SW_ScrollPolicy(w) == XmAUTOMATIC && SW_HasVSB(w))
    {
	XtCallActionProc((Widget)SW_VSB(w), "TopOrBottom",
			 event, params, *num_params);
    }
}

static void
PageUpGrab(Widget w, XEvent *event,
	   String *params, Cardinal *num_params)
{
    PageUp(XtParent(w), event, params, num_params);
}

static void
PageDownGrab(Widget w, XEvent *event,
	     String *params, Cardinal *num_params)
{
    PageDown(XtParent(w), event, params, num_params);
}

static void
PageLeftGrab(Widget w, XEvent *event,
	     String *params, Cardinal *num_params)
{
    PageLeft(XtParent(w), event, params, num_params);
}

static void
PageRightGrab(Widget w, XEvent *event,
	      String *params, Cardinal *num_params)
{
    PageRight(XtParent(w), event, params, num_params);
}

static void
BeginLineGrab(Widget w, XEvent *event,
	      String *params, Cardinal *num_params)
{
    BeginLine(XtParent(w), event, params, num_params);
}

static void
EndLineGrab(Widget w, XEvent *event,
	    String *params, Cardinal *num_params)
{
    EndLine(XtParent(w), event, params, num_params);
}

static void
BeginDataGrab(Widget w, XEvent *event,
	      String *params, Cardinal *num_params)
{
    BeginData(XtParent(w), event, params, num_params);
}

static void
EndDataGrab(Widget w, XEvent *event,
	    String *params, Cardinal *num_params)
{
    EndData(XtParent(w), event, params, num_params);
}

static void
SWNoop(Widget w, XEvent *event,
       String *params, Cardinal *num_params)
{
}

Widget
XmCreateScrolledWindow(Widget parent, char *name,
		       Arg *argList, Cardinal argcount)
{
    return XtCreateWidget(name, xmScrolledWindowWidgetClass, parent,
			  argList, argcount);
}

void
XmScrollVisible(Widget scrollw_widget, Widget widget,
		Dimension left_right_margin, Dimension top_bottom_margin)
{
    Position cx, cy, wx, wy;
    int cw, ch, ww, wh;
    int dx, dy;
    int val;
    Widget par;

    /* sanity checks */
    if (SW_ScrollPolicy(scrollw_widget) != XmAUTOMATIC)
    {
	return;
    }

    if (!SW_ClipWindow(scrollw_widget) || !SW_WorkWindow(scrollw_widget))
    {
	return;
    }

    for (par = widget; !XtIsShell(par); par = XtParent(par))
    {
	if (par == scrollw_widget)
	{
	    break;
	}
    }

    if (par != scrollw_widget)
    {
	return;
    }

    /* translate the clip and child coords */
    XtTranslateCoords((Widget)SW_ClipWindow(scrollw_widget), 0, 0, &cx, &cy);
    XtTranslateCoords(widget, 0, 0, &wx, &wy);

    cw = XtWidth(SW_ClipWindow(scrollw_widget));
    ch = XtHeight(SW_ClipWindow(scrollw_widget));
    ww = XtWidth(widget);
    wh = XtHeight(widget);

    /* delta */
    dx = dy = 0;

    /* figure dx */
    if (wx <= cx) /* changed to <= to closer mimic M*tif */
    {
	dx = cx - wx + left_right_margin;
    }
    else if (wx + ww > cx + cw)
    {
	dx = -((wx + ww) - (cx + cw) + left_right_margin);
    }
    /* figure dy */
    if (wy <= cy)
    {
	dy = cy - wy + top_bottom_margin;
    }
    else if (wy + wh > cy + ch)
    {
	dy = -((wy + wh) - (cy + ch) + top_bottom_margin);
    }

    _XmMoveObject(SW_WorkWindow(scrollw_widget),
		  XtX(SW_WorkWindow(scrollw_widget)) + dx,
		  XtY(SW_WorkWindow(scrollw_widget)) + dy);

    if (SW_HSB(scrollw_widget))
    {
    int min, max;

	XtVaGetValues((Widget)SW_HSB(scrollw_widget), 
		XmNvalue, &val, 
		XmNmaximum, &max, 
		XmNminimum, &min, 
		NULL);
	val -= dx;
	val = val < min ? min : val;
	val = val > max ? max : val;
	XtVaSetValues((Widget)SW_HSB(scrollw_widget), XmNvalue, val, NULL);
    }
    if (SW_VSB(scrollw_widget))
    {
    int min, max;

	XtVaGetValues((Widget)SW_VSB(scrollw_widget), 
		XmNvalue, &val, 
		XmNmaximum, &max, 
		XmNminimum, &min, 
		NULL);
	val -= dy;
	val = val < min ? min : val;
	val = val > max ? max : val;
	XtVaSetValues((Widget)SW_VSB(scrollw_widget), XmNvalue, val, NULL);
    }
}

void
XmScrolledWindowSetAreas(Widget widget,
			 Widget h_scrollbar,
			 Widget v_scrollbar,
			 Widget work_region)
{
    DEBUGOUT(_LtDebug(__FILE__, widget, "XmScrolledWindowSetAreas() - %s %s %s\n",
		      h_scrollbar ? XtName(h_scrollbar) : "NULL",
		      v_scrollbar ? XtName(v_scrollbar) : "NULL",
		      work_region ? XtName(work_region) : "NULL"));
    if (h_scrollbar)
    {
	XtVaSetValues(widget,
		  XmNhorizontalScrollBar, h_scrollbar,
		  NULL);
    }
    if (v_scrollbar)
    {
	XtVaSetValues(widget,
		  XmNverticalScrollBar, v_scrollbar,
		  NULL);
    }
    if (work_region)
    {
	XtVaSetValues(widget,
		  XmNworkWindow, work_region,
		  NULL);
    }
}


