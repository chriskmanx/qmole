/**
 *
 * $Id: Separator.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
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

static const char rcsid[] = "$Id: Separator.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/SeparatorP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumnP.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

#if 0
static void class_initialize();
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
 * Resources for the separator class
 */
#define Offset(field) XtOffsetOf(XmSeparatorRec, separator.field)
#define POffset(field) XtOffsetOf(XmSeparatorRec, primitive.field)
static XtResource resources[] = {
    {
	XmNseparatorType, XmCSeparatorType, XmRSeparatorType,
	sizeof(unsigned char), Offset(separator_type),
	XmRImmediate, (XtPointer)XmSHADOW_ETCHED_IN
    },
    {
	XmNmargin, XmCMargin, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmHORIZONTAL
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), POffset(traversal_on),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), POffset(highlight_thickness),
	XmRImmediate, (XtPointer)0
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNmargin,
	sizeof(Dimension), Offset(margin),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

#if 0
static void Help(Widget w, XEvent *event,
 String *params, Cardinal *num_params);

static char defaultTranslations[] =
    "<Key>osfHelp:  Help() ";

static XtActionsRec actions[] = {
    {"Help", Help},
};
#endif

/* *INDENT-OFF* */
#if 0
static XmBaseClassExtRec _XmSeparatorCoreClassExtRec = {
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

XmPrimitiveClassExtRec _XmSeparatorPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL, /* FIX ME */
    /* widget_display_rect */ NULL, /* FIX ME */
    /* widget_margins      */ NULL  /* FIX ME */
};
#endif

XmSeparatorClassRec xmSeparatorClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
	/* class_name            */ "XmSeparator",
	/* widget_size           */ sizeof(XmSeparatorRec),
	/* class_initialize      */ NULL /*class_initialize*/,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ NULL /*actions*/,
	/* num_actions           */ 0 /*XtNumber(actions)*/,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressSeries,
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
	/* tm_table              */ NULL /*defaultTranslations*/,
	/* query_geometry        */ NULL /* query_geometry */, /* Motif has NULL */
	/* display_accelerator   */ NULL /*XtInheritDisplayAccelerator*/,
	/* extension             */ (XtPointer)NULL /*&_XmSeparatorCoreClassExtRec*/
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ NULL /*XmInheritBorderHighlight*/,
	/* border_unhighlight    */ NULL /*XmInheritBorderUnhighlight*/,
	/* translations          */ NULL /*XtInheritTranslations*/,
	/* arm_and_activate_proc */ NULL /*XmInheritArmAndActivate*/,
	/* Synthetic Resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)NULL /*&_XmSeparatorPrimClassExtRec*/
    },
    /* Separator Class part */
    {
	/* extension */ NULL
    }
};
/* *INDENT-ON* */

WidgetClass xmSeparatorWidgetClass = (WidgetClass)&xmSeparatorClassRec;

#if 0
static void
class_initialize()
{
    _XmSeparatorCoreClassExtRec.record_type = XmQmotif;
}
#endif

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmSEPARATOR_BIT);
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
    values.line_style = (SEP_SeparatorType(w) == XmSINGLE_DASHED_LINE ||
			 SEP_SeparatorType(w) == XmDOUBLE_DASHED_LINE)
	? LineDoubleDash
	: LineSolid;

    SEP_SeparatorGC(w) = XtGetGC(w, valueMask, &values);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    /* Don't rely on defaults for the proper traversalOn and highlightThickness
     * TraversalOn is always false, and highlightThickness is zero for menus.
     */

    Prim_TraversalOn(new_w) = False;
    if (XmIsRowColumn(XtParent(new_w)) &&
	RC_Type(XtParent(new_w)) > XmWORK_AREA)
	Prim_HighlightThickness(new_w) = 0;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmROrientation),
			     SEP_Orientation(new_w), new_w))
	SEP_Orientation(new_w) = XmHORIZONTAL;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRSeparatorType),
			     SEP_SeparatorType(new_w), new_w))
	SEP_SeparatorType(new_w) = XmSHADOW_ETCHED_IN;

    if (SEP_Orientation(new_w) == XmVERTICAL)
    {
	if (!XtHeight(request))
	{
	    XtHeight(new_w) = (Prim_HighlightThickness(new_w) + 1) << 1;
	}
	if (!XtWidth(request))
	{
	    XtWidth(new_w) = Prim_HighlightThickness(new_w) << 1;
	    switch(SEP_SeparatorType(new_w))
	    {
	    case XmNO_LINE:
		break;
	    case XmSINGLE_LINE:
	    case XmSINGLE_DASHED_LINE:
		XtWidth(new_w) += 3;
		break;
	    case XmDOUBLE_LINE:
	    case XmDOUBLE_DASHED_LINE:
		XtWidth(new_w) += 5;
		break;
	    default:
		XtWidth(new_w) += Prim_ShadowThickness(new_w);
	    }
	    if (!XtWidth(new_w))
		XtWidth(new_w) = 1;
	}
    }
    else
    {
	if (!XtWidth(request))
	{
	    XtWidth(new_w) = (Prim_HighlightThickness(new_w)  + 1) << 1;
	}
	if (!XtHeight(request))
	{
	    XtHeight(new_w) = Prim_HighlightThickness(new_w) << 1;
	    switch(SEP_SeparatorType(new_w))
	    {
	    case XmNO_LINE:
		break;
	    case XmSINGLE_LINE:
	    case XmSINGLE_DASHED_LINE:
		XtHeight(new_w) += 3;
		break;
	    case XmDOUBLE_LINE:
	    case XmDOUBLE_DASHED_LINE:
		XtHeight(new_w) += 5;
		break;
	    default:
		XtHeight(new_w) += Prim_ShadowThickness(new_w);
	    }
	    if (!XtHeight(new_w))
		XtHeight(new_w) = 1;
	}
    }

    CreateSeparatorGC(new_w);
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, SEP_SeparatorGC(w));
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;

    Prim_TraversalOn(new_w) = False;
    if (XmIsRowColumn(XtParent(new_w)) &&
	RC_Type(XtParent(new_w)) > XmWORK_AREA)
	Prim_HighlightThickness(new_w) = 0;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmROrientation),
			     SEP_Orientation(new_w), new_w))
	SEP_Orientation(new_w) = SEP_Orientation(old);

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRSeparatorType),
			     SEP_SeparatorType(new_w), new_w))
	SEP_SeparatorType(new_w) = SEP_SeparatorType(old);

    if (SEP_SeparatorType(new_w) != SEP_SeparatorType(old) ||
	Prim_HighlightThickness(new_w) != Prim_HighlightThickness(old))
    {
	if (SEP_Orientation(new_w) == XmVERTICAL)
	{
	    if (XtWidth(new_w) == XtWidth(old))
	    {
		XtWidth(new_w) = Prim_HighlightThickness(new_w) << 1;
		switch(SEP_SeparatorType(new_w))
		{
		case XmNO_LINE:
		    break;
		case XmSINGLE_LINE:
		case XmSINGLE_DASHED_LINE:
		    XtWidth(new_w) += 3;
		    break;
		case XmDOUBLE_LINE:
		case XmDOUBLE_DASHED_LINE:
		    XtWidth(new_w) += 5;
		    break;
		default:
		    XtWidth(new_w) += Prim_ShadowThickness(new_w);
		}
		if (!XtWidth(new_w))
		    XtWidth(new_w) = 1;
	    }
	}
	else
	{
	    if (XtHeight(new_w) == XtHeight(old))
	    {
		XtHeight(new_w) = Prim_HighlightThickness(new_w) << 1;
		switch(SEP_SeparatorType(new_w))
		{
		case XmNO_LINE:
		    break;
		case XmSINGLE_LINE:
		case XmSINGLE_DASHED_LINE:
		    XtHeight(new_w) += 3;
		    break;
		case XmDOUBLE_LINE:
		case XmDOUBLE_DASHED_LINE:
		    XtHeight(new_w) += 5;
		    break;
		default:
		    XtHeight(new_w) += Prim_ShadowThickness(new_w);
		}
		if (!XtHeight(new_w))
		    XtHeight(new_w) = 1;
	    }
	}
	refresh_needed = True;
    }

    if (Prim_Foreground(new_w) != Prim_Foreground(old) ||
	XtBackground(new_w) != XtBackground(old) ||
	((SEP_SeparatorType(new_w) == XmSINGLE_DASHED_LINE ||
	  SEP_SeparatorType(new_w) == XmDOUBLE_DASHED_LINE) ^
	 (SEP_SeparatorType(old) == XmSINGLE_DASHED_LINE ||
	  SEP_SeparatorType(old) == XmDOUBLE_DASHED_LINE)))
    {
	XtReleaseGC(new_w, SEP_SeparatorGC(new_w));
	CreateSeparatorGC(new_w);
	refresh_needed = True;
    }

    if (SEP_Margin(new_w) != SEP_Margin(old) ||
	SEP_Orientation(new_w) != SEP_Orientation(old))
    {
	refresh_needed = True;
    }

    return refresh_needed;
}

static void
expose(Widget w, XEvent *event, Region region)
{
    _XmDrawSeparator(XtDisplay(w), XtWindow(w),
		     Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
		     SEP_SeparatorGC(w),
		     0,0,
		     XtWidth(w), XtHeight(w),
		     Prim_ShadowThickness(w),
		     Prim_HighlightThickness(w) + SEP_Margin(w),
		     SEP_Orientation(w),
		     SEP_SeparatorType(w));
}

#if 0
static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    /* Motif does not have this method */
    answer->request_mode = CWWidth | CWHeight;

    switch (SEP_Orientation(w))
    {
    case XmHORIZONTAL:
	answer->width = XtWidth(w);
	answer->height = Prim_ShadowThickness(w);
	break;

    case XmVERTICAL:
	answer->height = XtHeight(w);
	answer->width = Prim_ShadowThickness(w);
	break;
    }

    if (((proposed->request_mode & (CWWidth | CWHeight))
	 == (CWWidth | CWHeight)) &&
	proposed->width == answer->width &&
	proposed->height == answer->height)
{
	return XtGeometryYes;
}
    else if (answer->width == XtWidth(w) &&
	     answer->height == XtHeight(w))
{
	return XtGeometryNo;
}
    else
{
	return XtGeometryAlmost;
}
}
#endif

#if 0
static void
Help(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}
#endif

Widget
XmCreateSeparator(Widget parent, char *name,
		  Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name, xmSeparatorWidgetClass, parent,
			  arglist, argcount);
}
