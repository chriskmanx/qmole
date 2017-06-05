/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/CascadeB.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/CascadeB.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <stdarg.h>

#include <XmI/XmI.h>
#include <XmI/MacrosI.h>
#include <Xm/XmP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/RowColumnP.h>
#include <Xm/TearOffP.h>
#include <Xm/TransltnsP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/ScreenP.h>

#include <XmI/DebugUtil.h>


#define CASCADE_SPACING	4

/* Forward Declarations */

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget request, Widget new_w,
			       ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w,
				ArgList args, Cardinal *num_args);
static void resize(Widget w);
static void destroy(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void CleanupMenuBar(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);
static void StartDrag(Widget, XEvent *, String *, Cardinal *);
static void KeySelect(Widget, XEvent *, String *, Cardinal *);
static void ArmAndActivate(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);
static void _XmCBMenuBarSelect(Widget, XEvent *, String *, Cardinal *);
static void _XmCBMenuBarDoSelect(Widget, XEvent *, String *, Cardinal *);
static void MenuBarEnter(Widget, XEvent *, String *, Cardinal *);
static void MenuBarLeave(Widget, XEvent *, String *, Cardinal *);
static void DelayedArm(Widget, XEvent *, String *, Cardinal *);
static void CheckDisarm(Widget, XEvent *, String *, Cardinal *);

#if 0
static void ArmAndPost(Widget, XEvent *, String *, Cardinal *);
#endif

static void MenuProcEntry(int proc, Widget rc, ...);


/*
 * Resources for the cascade button class
 */
#define Offset(field) XtOffsetOf(XmCascadeButtonRec, cascade_button.field)
#define POffset(field) XtOffsetOf(XmCascadeButtonRec, primitive.field)
static XtResource resources[] =
{
    {
	XmNactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(activate_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNcascadingCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(cascade_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNsubMenuId, XmCMenuWidget, XmRMenuWidget,
	sizeof(Widget), Offset(submenu),
	XmRMenuWidget, (XtPointer)NULL
    },
    {
	XmNcascadePixmap, XmCPixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(cascade_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNmappingDelay, XmCMappingDelay, XmRInt,
	sizeof(int), Offset(map_delay),
	XmRImmediate, (XtPointer)180
    },
    /* resources we override from XmLabel/XmPrimitive */
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), POffset(shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), POffset(traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), POffset(highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmCascadeButtonRec, label.margin_width),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    }
};


static XtActionsRec actions[] =
{
    {"DelayedArm", DelayedArm},
    {"CheckDisarm", CheckDisarm},
    {"StartDrag", StartDrag},
    {"DoSelect", _XmCBMenuBarDoSelect},
    {"KeySelect", KeySelect},
    {"MenuBarSelect", _XmCBMenuBarSelect},
    {"MenuBarEnter", MenuBarEnter},
    {"MenuBarLeave", MenuBarLeave},
    {"CleanupMenuBar", CleanupMenuBar},
    {"Help", _XmCBHelp},
};


static XmBaseClassExtRec _XmCascadeBCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ XmInheritSetValuesPrehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ XmInheritSetValuesPosthook,
    /* secondary_object_class    */ XmInheritClass,
    /* secondary_object_create   */ XmInheritSecObjectCreate,
    /* get_secondary_resources   */ XmInheritGetSecResData,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ XmInheritGetValuesPrehook,
    /* get_values_posthook       */ XmInheritGetValuesPosthook,
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

XmPrimitiveClassExtRec _XmCascadeBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmInheritBaselineProc,
    /* widget_display_rect */ XmInheritDisplayRectProc,
    /* widget_margins      */ NULL
};

XmCascadeButtonClassRec xmCascadeButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelClassRec,
	/* class_name            */ "XmCascadeButton",
	/* widget_size           */ sizeof(XmCascadeButtonRec),
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
	/* tm_table              */ NULL,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmCascadeBCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight, /* FIX ME */
	/* border_unhighlight    */ XmInheritBorderUnhighlight, /* FIX ME */
	/* translations          */ XtInheritTranslations,
	/* arm_and_activate_proc */ ArmAndActivate,
	/* synthetic resources   */ NULL,
	/* num syn res           */ 0,
	/* extension             */ (XtPointer)&_XmCascadeBPrimClassExtRec,
    },
    /* Label Class part */
    {
	/* setOverrideCallback */ XmInheritSetOverrideCallback,
	/* menuProcs           */ XmInheritMenuProc,
	/* translations        */ XtInheritTranslations,
	/* extension           */ NULL
    },
    /* CascadeButton Class part */
    {
	/* extension */ NULL
    }
};


WidgetClass xmCascadeButtonWidgetClass = (WidgetClass)&xmCascadeButtonClassRec;

static XtTranslations menub_trans;
static XtTranslations popup_trans;

static void
class_initialize(void)
{
    _XmCascadeBCoreClassExtRec.record_type = XmQmotif;

    menub_trans = XtParseTranslationTable(_XmCascadeB_menubar_events);

    popup_trans = XtParseTranslationTable(_XmCascadeB_p_events);
}


static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmCASCADE_BUTTON_BIT);
}


static void
initialize_prehook(Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "%s:initialize_prehook()\n", __FILE__));
    DEBUGOUT(_LtDebug("MENUT", new_w, "%s:initialize_prehook(%d)\n", __FILE__, __LINE__));

    _XmSaveCoreClassTranslations(new_w);

    if (XmIsRowColumn(XtParent(new_w)) &&
	RC_Type(XtParent(new_w)) == XmMENU_BAR)
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w, "    MenuBar translations\n"));
	DEBUGOUT(_LtDebug0("MENUT", new_w, "    MenuBar translations\n"));

	CoreClassTranslations(new_w) = (String)menub_trans;
    }
    else if (XmIsRowColumn(XtParent(new_w)) &&
	     (RC_Type(XtParent(new_w)) == XmMENU_POPUP ||
	      RC_Type(XtParent(new_w)) == XmMENU_PULLDOWN))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w, "    Popup translations\n"));
	DEBUGOUT(_LtDebug0("MENUT", new_w, "    Popup translations\n"));

	CoreClassTranslations(new_w) = (String)popup_trans;
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w, "    Default translations\n"));
	DEBUGOUT(_LtDebug0("MENUT", new_w, "    Default translations\n"));
    }

}


static void
initialize_posthook(Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    _XmRestoreCoreClassTranslations(new_w);
}


static void
place_cascade(Widget w)
{
    Position x;

    CB_Cascade_x(w) = Lab_Shadow(w) + Lab_MarginWidth(w);
    if (Lab_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R)
	CB_Cascade_x(w) = XtWidth(w) - CB_Cascade_x(w) - CB_Cascade_width(w);

    CB_Cascade_y(w) = (XtHeight(w) - CB_Cascade_height(w)) / 2;

    /* Make sure the label and cascade don't overlap */

    if (Lab_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R)
    {
	x = CB_Cascade_x(w) - CASCADE_SPACING - Lab_TextRect_width(w);
	if (Lab_TextRect_x(w) > x)
	    Lab_TextRect_x(w) = x;
    }
    else
    {
	x = CB_Cascade_x(w) + CB_Cascade_width(w) + CASCADE_SPACING;
	if (Lab_TextRect_x(w) < x)
	    Lab_TextRect_x(w) = x;
    }
	DEBUGOUT(_LtDebug(__FILE__, w, "PlaceCascade %d %d\n",
		CB_Cascade_x(w), CB_Cascade_y(w)));
}


static void
size_cascade(Widget w)
{
    int		dummy;
    Window	dummyw;
    unsigned	width, height;

    if (CB_CascadePixmap(w) > XmUNSPECIFIED_PIXMAP)
    {
	XGetGeometry(XtDisplay(w), CB_CascadePixmap(w),
		     &dummyw, &dummy, &dummy,
		     &width, &height, (unsigned *)&dummy, (unsigned *)&dummy);
	CB_Cascade_width(w) = width;
	CB_Cascade_height(w) = height;
    }
    else
    {
	CB_Cascade_width(w) = CB_Cascade_height(w) = 0;
    }
}


static void
initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "%s:initialize()\n", __FILE__));
    DEBUGOUT(_LtDebug("MENUT", new_w, "%s:initialize(%d)\n", __FILE__, __LINE__));

    CB_SetArmed(new_w, False);
    CB_ArmedPixmap(new_w) = None;

    if (CB_MapDelay(new_w) < 0)
    {
	_XmWarning(new_w, "MappingDelay must be non-negative.");
	CB_MapDelay(new_w) = 180;
    }
    if (CB_Submenu(new_w) && (!XmIsRowColumn(CB_Submenu(new_w)) ||
			      (RC_Type(CB_Submenu(new_w)) != XmMENU_PULLDOWN)))
    {
	_XmWarning(new_w, "Submenu must a pull-down menu.");
	CB_Submenu(new_w) = NULL;
    }

    CB_Cascade_width(new_w) = 0;
    CB_Cascade_height(new_w) = 0;
    CB_Cascade_x(new_w) = 0;
    CB_Cascade_y(new_w) = 0;

    if (Lab_MenuType(new_w) != XmMENU_BAR &&
	Lab_MenuType(new_w) != XmMENU_POPUP &&
	Lab_MenuType(new_w) != XmMENU_PULLDOWN)
    {
	_XmError(new_w, "Cascade parent is incorrect type.");
    }

    Prim_TraversalOn(new_w) = True;
    Lab_Highlight(new_w) = 0;

    if (Lab_MarginWidth(new_w) == (Dimension)XmINVALID_DIMENSION)
	Lab_MarginWidth(new_w) = Lab_MenuType(new_w) == XmMENU_BAR ? 6 : 2;

    if (Lab_MenuType(new_w) != XmMENU_BAR)
    {
	int margin;

	if (CB_CascadePixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	    CB_Submenu(new_w))
	    _XmCreateArrowPixmaps(new_w);
	size_cascade(new_w);

	/* Make sure there's enough room on the side for the cascade pixmap */
	margin = (CB_Cascade_width(new_w) + CASCADE_SPACING)
	    - (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	       ? Lab_MarginRight(new_w) : Lab_MarginLeft(new_w));
	if (margin > 0)
	{
	    if (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R)
		Lab_MarginRight(new_w) += margin;
	    else
	    {
		Lab_MarginLeft(new_w) += margin;
		Lab_TextRect_x(new_w) += margin;
	    }
	    if (!XtWidth(request))
		XtWidth(new_w) += margin;
	}

	/* Make sure there's enough room vertically */
	margin = CB_Cascade_height(new_w) - (Lab_TextRect_height(new_w)
					     + Lab_MarginTop(new_w)
					     + Lab_MarginBottom(new_w));
	if (margin > 0)
	{
	    Lab_MarginTop(new_w) += margin >> 1;
	    Lab_MarginBottom(new_w) += (margin + 1) >> 1;
	    if (!XtHeight(request))
	    {
		Lab_TextRect_y(new_w) += margin >> 1;
		XtHeight(new_w) += margin;
	    }
	}

	place_cascade(new_w);
    }

    if (CB_Submenu(new_w))
    {
	/* Make sure the RC also knows how to locate us */
	RC_MenuSubmenu(new_w);
    }

    CB_Timer(new_w) = 0;
    LabClass_MenuProcs(XtClass(new_w)) = MenuProcEntry;
}


static void
resize(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:resize(%d) - %dx%d\n",
			__FILE__, __LINE__,
			XtWidth(w), XtHeight(w)));

#define superclass (&xmLabelClassRec)
    (*superclass->core_class.resize) (w);
#undef superclass

    if (Lab_MenuType(w) != XmMENU_BAR)
    {
	place_cascade(w);
    }
}


static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:destroy()\n", __FILE__));

    if (CB_Timer(w) != 0)
    {
	XtRemoveTimeOut(CB_Timer(w));
	CB_Timer(w) = 0;
    }

    if (CB_ArmedPixmap(w))
    {
	_XmArrowPixmapCacheDelete((XtPointer)CB_ArmedPixmap(w));
	_XmArrowPixmapCacheDelete((XtPointer)CB_CascadePixmap(w));
    }
}


static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:set_values() - %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__,
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    if (CB_MapDelay(new_w) < 0)
    {
	_XmWarning(new_w, "MappingDelay must be non-negative.");
	CB_MapDelay(new_w) = CB_MapDelay(old);
    }
    if (CB_Submenu(new_w) && (!XmIsRowColumn(CB_Submenu(new_w)) ||
			      (RC_Type(CB_Submenu(new_w)) != XmMENU_PULLDOWN)))
    {
	_XmWarning(new_w, "Submenu must a pull-down menu.");
	CB_Submenu(new_w) = CB_Submenu(old);
    }

    Prim_TraversalOn(new_w) = True;
    Lab_Highlight(new_w) = 0;

    if (CB_CascadePixmap(old) != CB_CascadePixmap(new_w) ||
	(CB_Submenu(new_w)
	 ? CB_CascadePixmap(new_w) == XmUNSPECIFIED_PIXMAP
	 : CB_ArmedPixmap(new_w)) ||
	(CB_ArmedPixmap(new_w) && Lab_Font(old) != Lab_Font(new_w)))
    {
	if (CB_ArmedPixmap(old))
	{
	    _XmArrowPixmapCacheDelete((XtPointer)CB_CascadePixmap(old));
	    _XmArrowPixmapCacheDelete((XtPointer)CB_ArmedPixmap(old));
	    if (CB_CascadePixmap(old) == CB_CascadePixmap(new_w))
		CB_CascadePixmap(new_w) = XmUNSPECIFIED_PIXMAP;
	    CB_ArmedPixmap(new_w) = None;
	}
	if (Lab_MenuType(new_w) != XmMENU_BAR)
	{
	    if (CB_CascadePixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
		CB_Submenu(new_w))
	    {
		_XmCreateArrowPixmaps(new_w);
	    }
	    size_cascade(new_w);
	    refresh_needed = True;
	}

    }

    if (XtSensitive(new_w) != XtSensitive(old))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "SetValues: sensitive changes to %d\n",
			  XtSensitive(new_w)));

	refresh_needed = True;
    }

    if (CB_Submenu(old) != CB_Submenu(new_w))
    {
	/* Make sure the RC also knows how to locate us */
	RC_MenuSubmenu(new_w);
    }

    /* Adjust margins for the cascade pixmap size if necessary.
     * Margins be increased or decreased; if the margin was explicitly set
     * in this call, don't decrease it past that (though it can get bigger).
     */
    if (Lab_MenuType(new_w) != XmMENU_BAR &&
	(CB_Cascade_width(new_w) != CB_Cascade_width(old)
	 || CB_Cascade_height(new_w) != CB_Cascade_height(old)
	 || Lab_StringDirection(new_w) != Lab_StringDirection(old)
	 || (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	     ? Lab_MarginRight(new_w) != Lab_MarginRight(old)
	     : Lab_MarginLeft(new_w) != Lab_MarginLeft(old))
	 || Lab_MarginTop(new_w) != Lab_MarginTop(old)
	 || Lab_MarginBottom(new_w) != Lab_MarginBottom(old)))
    {
	int margin, tm, bm;

	margin = CB_Cascade_width(new_w) + CASCADE_SPACING
	    - (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	       ? Lab_MarginRight(new_w) : Lab_MarginLeft(new_w));
	if (margin && (margin > 0 ||
		       (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
			? Lab_MarginRight(new_w) == Lab_MarginRight(old)
			: Lab_MarginLeft(new_w) == Lab_MarginLeft(old))))
	{
	    if (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R)
		Lab_MarginRight(new_w) += margin;
	    else
	    {
		Lab_MarginLeft(new_w) += margin;
		Lab_TextRect_x(new_w) += margin;
	    }
	    if (Lab_RecomputeSize(new_w) || !XtWidth(request))
		XtWidth(new_w) += margin;
	}

	margin = CB_Cascade_height(new_w) - (Lab_TextRect_height(new_w)
					     + Lab_MarginTop(new_w)
					     + Lab_MarginBottom(new_w));
	if (margin)
	{
	    tm = margin / 2;
	    if (tm < (Lab_MarginTop(new_w) == Lab_MarginTop(old)
		      ? XmDEFAULT_TOP_MARGIN - (int)Lab_MarginTop(new_w)
		      : 0))
		tm = (Lab_MarginTop(new_w) == Lab_MarginTop(old)
		      ? XmDEFAULT_TOP_MARGIN - (int)Lab_MarginTop(new_w)
		      : 0);
	    Lab_MarginTop(new_w) += tm;
	    if (Lab_RecomputeSize(new_w) || !XtHeight(request))
		XtHeight(new_w) += tm;

	    bm = (margin + 1) / 2;
	    if (bm < (Lab_MarginBottom(new_w) == Lab_MarginBottom(old)
		      ? XmDEFAULT_BOTTOM_MARGIN - (int)Lab_MarginBottom(new_w)
		      : 0))
		bm = (Lab_MarginBottom(new_w) == Lab_MarginBottom(old)
		      ? XmDEFAULT_BOTTOM_MARGIN - (int)Lab_MarginBottom(new_w)
		      : 0);
	    Lab_MarginBottom(new_w) += bm;
	    if (Lab_RecomputeSize(new_w) || !XtHeight(request))
		XtHeight(new_w) += bm;

	    if (tm != bm)
		Lab_TextRect_y(new_w) += (tm - bm) / 2;
	}

	refresh_needed = True;
    }

    if (Lab_MenuType(new_w) != XmMENU_BAR &&
	(CB_Cascade_width(new_w) != CB_Cascade_width(old)
	 || CB_Cascade_height(new_w) != CB_Cascade_height(old)
	 || Lab_Shadow(new_w) != Lab_Shadow(old)
	 || Lab_MarginWidth(new_w) != Lab_MarginWidth(old)
	 || Lab_TextRect_y(new_w) != Lab_TextRect_y(old)
	 || Lab_TextRect_height(new_w) != Lab_TextRect_height(old)
	 || Lab_StringDirection(new_w) != Lab_StringDirection(old)))
    {
	Dimension width, height;

	width = XtWidth(new_w);
	height = XtHeight(new_w);
	XtWidth(new_w) = XtWidth(old);
	XtHeight(new_w) = XtHeight(old);
	place_cascade(new_w);
	XtWidth(new_w) = width;
	XtHeight(new_w) = height;
	refresh_needed = True;
    }

    return refresh_needed;
}


static void
draw_cascade(Widget w)
{
    Pixmap pm;

    if (CB_IsArmed(w))
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
		       0, 0, XtWidth(w), XtHeight(w),
		       Lab_Shadow(w), XmSHADOW_OUT);
    }

    pm = CB_IsArmed(w) && CB_ArmedPixmap(w) > XmUNSPECIFIED_PIXMAP
	? CB_ArmedPixmap(w)
	: CB_CascadePixmap(w);
    if (pm > XmUNSPECIFIED_PIXMAP)
    {
	XCopyArea(XtDisplay(w), pm, XtWindow(w),
		  Lab_NormalGC(w),
		  0, 0,
		  CB_Cascade_width(w), CB_Cascade_height(w),
		  CB_Cascade_x(w), CB_Cascade_y(w));
    }
}


static void
expose(Widget w, XEvent *event, Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:expose() Armed(%d) type %s\n",
		      __FILE__,
		      CB_IsArmed(w), _LtDebugMenuType2String(Lab_MenuType(w))));

    /* The label's expose routine should be called after the cascade button's
     * expose routine. */

    draw_cascade(w);

#define superclass (&xmLabelClassRec)
    (*superclass->core_class.expose) (w, event, region);
#undef superclass
}


/* ACTION PROCS */
/*
 * CleanupMenuBar
 *    w       is a cascadeButton
 */
static void
CleanupMenuBar(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "CleanupMenuBar()\n"));

    if (w)			/* Try not to crash */
    {
	RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_BAR_CLEANUP,
						XtParent(w), NULL);
    }
}


static void
KeySelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KeySelect\n"));

    /* If we're in keyboard traversal mode, and if we have a submenu
       attached, call the cascading callback, post the submenu and
       allow keyboard traversal of it. */

    /* else call the activate callback, and unpost all active menus
       in the cascade */
}


extern void
_XmCascadingPopup(Widget w, XEvent *event, Boolean doCascade)
{
    /* FIX ME: doCascade? */
    XmAnyCallbackStruct cbs;

#if 1
    /* rws 29 Sep 1999
       I really don't like this here, but rowcolumn/test51 shows that it 
       must be before the CascadingCallback. So I can't roll it into 
       MenuCascading.
     */
    {
    Widget shell;
    Widget sub_pane;
    Boolean was_torn;

	if (XmIsGadget(w))
	    sub_pane = CBG_Submenu(w);
	else
	    sub_pane = CB_Submenu(w);
	if (sub_pane)
	{
	    RCClass_MenuProcs(XtClass(sub_pane))(XmMENU_RESTORE_TEAROFF_TO_MENUSHELL,
				     sub_pane, &shell, &was_torn, event);
	}
    }
#endif
    cbs.reason = XmCR_CASCADING;
    cbs.event = event;

    XFlush(XtDisplay(w));

    if(XmIsGadget(w))
          XtCallCallbackList(w, CBG_CascadeCall(w), &cbs);
    else
          XtCallCallbackList(w, CB_CascadeCall(w),  &cbs);

    if (doCascade)
    {
	RC_MenuCascading(w, event);
    }
}


/*
 *    _XmCBMenuBarDoSelect()
 *
 *      Calls the callbacks in XmNcascadingCallback, posts the submenu attached
 *      to the CascadeButton and enables keyboard traversal within the menu.
 *      If the CascadeButton does not have a submenu attached, this action
 *      calls the callbacks in XmNactivateCallback, activates the
 *      CascadeButton, and unposts all posted menus in the cascade.
 */
static void
_XmCBMenuBarDoSelect(Widget w, XEvent *event,
		     String *params, Cardinal *num_params)
{
Boolean validButton;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmCBMenuBarDoSelect(%d)\n\t%s posted mine %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CB_Submenu(w) ? XtName(CB_Submenu(w)) : "none"
	));
    DEBUGOUT(_LtDebug("MENU", w, "%s:_XmCBMenuBarDoSelect(%d)\n\t%s posted mine %s %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CB_Submenu(w) ? XtName(CB_Submenu(w)) : "none",
	event ? _LtDebugEventType2String(event->xany.type) : "no-event"
	));
    if (event && event->xany.type == ButtonRelease)
    {
	RC_MenuButton(w, event, &validButton);
    }
    else
    {
	validButton = True;
    }
    if (validButton)
    {
	XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);
	_XmMenuFocus(w, XmMENU_FOCUS_SET, CurrentTime);
	if (CB_Submenu(w))
	{
	    if (RC_PopupPosted(XtParent(w)) != CB_Submenu(w))
	    {
		_XmCascadingPopup(w, event, True);
	    }

	    RCClass_MenuTraverse(CB_Submenu(w), XmTRAVERSE_HOME);

	    _XmSetInDragMode(w, False);
	}
	else
	{
	XmAnyCallbackStruct cbs;

	    cbs.reason = XmCR_ACTIVATE;
	    cbs.event = event;
	    if (XmIsRowColumn(XtParent(w)))
	    {
		RC_MenuMenuCallback(w, &cbs);
	    }
	    if (!Lab_SkipCallback(w) && CB_ActivateCall(w))
	    {
		XFlush(XtDisplay(w));

		XtCallCallbackList(w, CB_ActivateCall(w), &cbs);
	    }
	    XmCascadeButtonHighlight(w, False);
	    if (RC_PopupPosted(XtParent(w)))
	    {
	    Boolean poppedUp;

		RC_MenuShellPopdown(w, event, &poppedUp);
	    }
	}
    }
}


static void
_XmCBMenuBarSelect(Widget w, XEvent *event,
		   String *params, Cardinal *num_params)
{
    Boolean validButton;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmCBMenuBarSelect(%d)\n\t%s posted mine %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CB_Submenu(w) ? XtName(CB_Submenu(w)) : "none"
	));
    DEBUGOUT(_LtDebug("MENU", w, "%s:_XmCBMenuBarSelect(%d)\n\t%s posted mine %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CB_Submenu(w) ? XtName(CB_Submenu(w)) : "none"
	));
    if (event && event->xany.type == ButtonPress)
    {
	RC_MenuButton(w, event, &validButton);
    }
    else
    {
	validButton = True;
    }
    if (validButton)
    {
	XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);
	XmProcessTraversal(w, XmTRAVERSE_CURRENT);
	if (CB_Submenu(w))
	{
	Cardinal i;
	Widget tmp = RC_PopupPosted(XtParent(w));

	    if (RC_PopupPosted(XtParent(w)))
	    {
		if (RC_PopupPosted(XtParent(w)) && RC_PopupPosted(XtParent(w)) != CB_Submenu(w))
		{
		    _XmCascadingPopup(w, event, False);
		}
		if (RC_PopupPosted(XtParent(w)) != CB_Submenu(w))
		{
		Boolean poppedUp;

		    RC_MenuShellPopdown(w, event, &poppedUp);
		}
		else if (CB_Submenu(w) && RC_PopupPosted(CB_Submenu(w)))
		{
		Boolean poppedUp;

		    for (i = 0; i < MGR_NumChildren(RC_PopupPosted(CB_Submenu(w))); i++)
		    {
		    Widget w1 = MGR_Children(RC_PopupPosted(CB_Submenu(w)))[i];

			_XmMenuDisarmItem(w1);
		    }
		    RC_MenuShellPopdown(RC_CascadeBtn(RC_PopupPosted(CB_Submenu(w))), event, &poppedUp);
		}
		if (RC_PopupPosted(XtParent(w)) != CB_Submenu(w))
		{
		    RC_MenuCascading(w, event);
		}
		if (tmp)
		{
		    for (i = 0; i < MGR_NumChildren(tmp); i++)
		    {
		    Widget w1 = MGR_Children(tmp)[i];

			_XmMenuDisarmItem(w1);
		    }
		}
	    }
	    else
	    {
		if (RC_PopupPosted(XtParent(w)) != CB_Submenu(w))
		{
		    _XmCascadingPopup(w, event, True);
		}
		if (tmp)
		{
		    for (i = 0; i < MGR_NumChildren(tmp); i++)
		    {
		    Widget w1 = MGR_Children(tmp)[i];

			_XmMenuDisarmItem(w1);
		    }
		}
	    }

	    RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_ARM, XtParent(w), NULL);

	}
	else
	{
	Cardinal i;
	Boolean poppedUp;

	    _XmCascadingPopup(w, event, False);
	    if (RC_PopupPosted(XtParent(w)))
	    {
		for (i = 0; i < MGR_NumChildren(RC_PopupPosted(XtParent(w))); i++)
		{
		Widget w1 = MGR_Children(RC_PopupPosted(XtParent(w)))[i];

		    _XmMenuDisarmItem(w1);
		}
	    }
	    RC_MenuShellPopdown(w, event, &poppedUp);
	}
	XmCascadeButtonHighlight(w, True);
	_XmSetInDragMode(w, True);
	RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_ARM, XtParent(w), NULL);
    }
}


#if 0
static void
ArmAndPost(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ArmAndPost()\n"));

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);

    if (!RC_IsArmed(CBG_Submenu(w)))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "MENU_ARM\n"));

	RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_ARM, XtParent(w), NULL);
    }

    _XmCascadingPopup(w, event, True);
}
#endif


static void
MenuBarEnter(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MenuBarEnter\n\t%s %s %s %s posted posting %s\n",
	_XmGetInDragMode(w) ? "Dragging" : "Not Dragging",
	_LtDebugRcType2String(Lab_MenuType(w)),
	RC_IsArmed(XtParent(w)) ? "Armed" : "Not Armed",
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CB_Submenu(w) ? XtName(CB_Submenu(w)) : "nothing"
	));
    DEBUGOUT(_LtDebug("MENU", w, "MenuBarEnter\n\t%s %s %s %s posted posting %s\n",
	_XmGetInDragMode(w) ? "Dragging" : "Not Dragging",
	_LtDebugRcType2String(Lab_MenuType(w)),
	RC_IsArmed(XtParent(w)) ? "Armed" : "Not Armed",
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CB_Submenu(w) ? XtName(CB_Submenu(w)) : "nothing"
	));

    if (_XmGetInDragMode(w))
    {
	if (Lab_MenuType(w) == XmMENU_BAR)
	{
	    if (RC_IsArmed(XtParent(w)))
	    {
		if (CB_Submenu(w))
		{
		    if (RC_PopupPosted(XtParent(w)) && RC_PopupPosted(XtParent(w)) != CB_Submenu(w))
		    {
			_XmCascadingPopup(w, event, False);
		    }
		    if (RC_PopupPosted(XtParent(w)) && RC_PopupPosted(XtParent(w)) != CB_Submenu(w))
		    {
		    Boolean poppedUp;

			RC_MenuShellPopdown(w, event, &poppedUp);
		    }
		    XmCascadeButtonHighlight(w, True);
		    if (!RC_PopupPosted(XtParent(w)))
		    {
			RC_MenuCascading(w, event);
		    }
		}
		else
		{
		    _XmCascadingPopup(w, event, True);
		    if (RC_PopupPosted(XtParent(w)) && RC_PopupPosted(XtParent(w)) != CB_Submenu(w))
		    {
		    Boolean poppedUp;

			RC_MenuShellPopdown(w, event, &poppedUp);
		    }
		    XmCascadeButtonHighlight(w, True);
		}
	    }
	}
	else
	{
	    _XmWarning(w, "%s(%d) - MenuBarEnter not in MenuBar",
		__FILE__, __LINE__);
	}
    }
}


static void
MenuBarLeave(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MenuBarLeave\n\t%s %s %s %s posted mine %s\n",
	_XmGetInDragMode(w) ? "Dragging" : "Not Dragging",
	_LtDebugRcType2String(Lab_MenuType(w)),
	RC_IsArmed(XtParent(w)) ? "Armed" : "Not Armed",
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CB_Submenu(w) ? XtName(CB_Submenu(w)) : "nothing"
	));
    DEBUGOUT(_LtDebug("MENU", w, "MenuBarLeave\n\t%s %s %s %s posted mine %s\n",
	_XmGetInDragMode(w) ? "Dragging" : "Not Dragging",
	_LtDebugRcType2String(Lab_MenuType(w)),
	RC_IsArmed(XtParent(w)) ? "Armed" : "Not Armed",
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CB_Submenu(w) ? XtName(CB_Submenu(w)) : "nothing"
	));
    if (_XmGetInDragMode(w))
    {
	if (Lab_MenuType(w) == XmMENU_BAR)
	{
	    if (RC_IsArmed(XtParent(w)))
	    {
		if (!CB_Submenu(w) || (RC_PopupPosted(XtParent(w)) && RC_PopupPosted(XtParent(w)) != CB_Submenu(w)))
		{
		    XmCascadeButtonHighlight(w, False);
		}
	    }
	}
	else
	{
	    _XmWarning(w, "%s(%d) - MenuBarLeave not in MenuBar",
		__FILE__, __LINE__);
	}
    }
}


static void
StartDrag(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean validButton;
    Boolean poppedUp;

    DEBUGOUT(_LtDebug(__FILE__, w, "StartDrag()\n"));

    /* queue events up until the next button event. */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    _XmRecordEvent(event);

    /* Is it even the right kind of event? */
    if (!event || event->type != ButtonPress)
    {
	return;
    }

    /* Was it the right button? */
    RC_MenuButton(w, event, &validButton);

    if (validButton)
    {
	if (CB_Submenu(w) && RC_PopupPosted(XtParent(w)) == CB_Submenu(w))
	{
	Cardinal i;

	    for (i = 0; i < MGR_NumChildren(CB_Submenu(w)); i++)
	    {
		_XmMenuDisarmItem(MGR_Children(CB_Submenu(w))[i]);
	    }
	}
	else
	{
	    RC_MenuShellPopdown(w, event, &poppedUp);
	    _XmCascadingPopup(w, event, True);
	    {
	    Cardinal i;
	    Widget menu = XtParent(w);

		for (i = 0; i < MGR_NumChildren(menu); i++)
		{
		    _XmMenuDisarmItem(MGR_Children(menu)[i]);
		}
	    }
	    XmCascadeButtonHighlight(w, True);
	}

	_XmSetInDragMode(w, True);
    }
}


static void
CascadePopupHandler(XtPointer clientData, XtIntervalId *id)
{
    Widget w = (Widget)clientData;

    DEBUGOUT(_LtDebug(__FILE__, w, "CascadePopupHandler\n"));

    CB_Timer(w) = 0;

    _XmCascadingPopup(w, NULL, True);	/* FIX ME: NULL? */
}


static void
DelayedArm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "DelayedArm()\n"));

    /* Signal server to release next pointer event */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    if (_XmGetInDragMode(w))
    {
	if (RC_PopupPosted(XtParent(w)) != CB_Submenu(w))
	{
	Boolean poppedUp;

	    RC_MenuShellPopdown(w, NULL, &poppedUp);
	}
	if (!RC_PopupPosted(XtParent(w)))
	{
	    CB_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
					  CB_MapDelay(w),
					  CascadePopupHandler,
					  (XtPointer)w);

	    XmCascadeButtonHighlight(w, True);
	}
    }
}


static void
CheckDisarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int x = ((XLeaveWindowEvent *)event)->x_root;
    int y = ((XLeaveWindowEvent *)event)->y_root;
    Widget subpane;

    DEBUGOUT(_LtDebug(__FILE__, w, "CheckDisarm()\n"));

    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime); /* NEW */

    if (_XmGetInDragMode(w))
    {
	if (CB_Timer(w))
	{
	    XtRemoveTimeOut(CB_Timer(w));
	    CB_Timer(w) = 0;
	}

	if (!RC_PopupPosted(XtParent(w)) || (CB_Submenu(w) != RC_PopupPosted(XtParent(w))))
	{
	    XmCascadeButtonHighlight(w, False);
	}
	else
	{
	    subpane = XtParent(CB_Submenu(w));

	    if (x < XtX(subpane) || x >= XtX(subpane) + XtWidth(subpane) ||
		y < XtY(subpane) || y >= XtY(subpane) + XtHeight(subpane))
	    {
		Boolean poppedUp;

		RC_MenuShellPopdown(w, event, &poppedUp);
		XmCascadeButtonHighlight(w, False);
	    }
	}
    }
}


extern void
_XmCBHelp(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmCBHelp\n"));

    /* clean up things */

    /* invoke help callbacks */
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}


static void
ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:ArmAndActivate()\n",
		__FILE__));
    if (Lab_MenuType(w) == XmMENU_BAR)
    {
	if (!RC_IsArmed(XtParent(w)))
	{
	    RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_ARM, XtParent(w), NULL);
	    _XmMenuFocus(w, XmMENU_FOCUS_SAVE, CurrentTime);
	    _XmSetInDragMode(XtParent(w), False);
	}
	_XmMenuFocus(w, XmMENU_FOCUS_SET, CurrentTime);
	_XmCBMenuBarSelect(w, event, params, num_params);
	_XmCBMenuBarDoSelect(w, event, params, num_params);
    }
    else /* pullright */
    {
	CascadePopupHandler((XtPointer)w, NULL);
    }
}


static void
MenuProcEntry(int proc, Widget w, ...)
{
    va_list arg_list;

    va_start(arg_list, w);

    switch (proc)
    {
    case XmMENU_ARM:
	{
	  /* XtExposeProc exp = XtClass(w)->core_class.expose; */

	    XmCascadeButtonHighlight(w, True);
	    XmProcessTraversal(w, XmTRAVERSE_CURRENT);
	    /*
	    (exp) (w, event, (Region)NULL);
	    */
	}
	break;
    case XmMENU_DISARM:
	{
	  /* XtExposeProc exp = XtClass(w)->core_class.expose; */

	    XmCascadeButtonHighlight(w, False);
	    /*
	    (exp) (w, event, (Region)NULL);
	    */
	}
	break;
    default:
	_XmWarning(w, "%s(%d) - Invalid menuProc function", __FILE__, __LINE__);
	break;
    }

    va_end(arg_list);
}


extern void
XmCascadeButtonHighlight(Widget w, Boolean highlight)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
	     "XmCascadeButtonHighlight(hl %d, armed %d, apm 0x%X, cpm 0x%X)\n",
		      highlight,
		      XmIsPrimitive(w) ? CB_IsArmed(w) : CBG_IsArmed(w),
		  XmIsPrimitive(w) ? CB_ArmedPixmap(w) : CBG_ArmedPixmap(w),
	    XmIsPrimitive(w) ? CB_CascadePixmap(w) : CBG_CascadePixmap(w)));

    if (XmIsGadget(w))
    {
	XmCascadeButtonGadgetHighlight(w, highlight);

	return;
    }
    else if (!XmIsCascadeButton(w))
    {
	_XmError(w,
	    "XmCascadeButtonHighlight called with non-cascade button widget");

	return;
    }

    CB_SetArmed(w, highlight);

    if (XtIsRealized(w))
    {
	if (!highlight)
	{
	    _XmClearBorder(XtDisplay(w), XtWindow(w),
			   0, 0, XtWidth(w), XtHeight(w), Lab_Shadow(w));
	}
	draw_cascade(w);
    }
}


extern Widget
XmCreateCascadeButton(Widget parent, char *name,
		      Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmCascadeButtonWidgetClass,
			  parent,
			  arglist, argcount);
}
