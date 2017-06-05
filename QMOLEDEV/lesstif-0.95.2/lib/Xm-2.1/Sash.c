/**
 *
 * $Id: Sash.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
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

static const char rcsid[] = "$Id: Sash.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/SashP.h>
#include <Xm/DisplayP.h>
#include <Xm/TransltnsP.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);

static void expose(Widget w, XEvent *event, Region region);

#if 0
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);
#endif

/*
 * Resources for the sash class
 */
#define Offset(field) XtOffsetOf(XmSashRec, sash.field)
#define POffset(field) XtOffsetOf(XmSashRec, primitive.field)
static XtResource resources[] =
{
    {
	XmNborderWidth, XmCBorderWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPrimitiveRec, core.border_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNcallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(sash_action),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), POffset(navigation_type),
	XmRImmediate, (XtPointer)XmSTICKY_TAB_GROUP
    },
};

static void SashAction(Widget, XEvent *, String *, Cardinal *);

static void SashFocusIn(Widget, XEvent *, String *, Cardinal *);

static void SashFocusOut(Widget, XEvent *, String *, Cardinal *);

static XtActionsRec actions[] =
{
    {"SashAction", SashAction},
    {"SashFocusIn", SashFocusIn},
    {"SashFocusOut", SashFocusOut},
};


static XmBaseClassExtRec _XmSashCoreClassExtRec = {
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
XmPrimitiveClassExtRec _XmSashPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL,
    /* widget_display_rect */ NULL,
    /* widget_margins      */ NULL
};
#endif

XmSashClassRec xmSashClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
        /* class_name            */ "XmSash",
	/* widget_size           */ sizeof(XmSashRec),
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
	/* resize                */ NULL,
	/* expose                */ expose,
	/* set_values            */ NULL,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmSash_defTranslations,
	/* query_geometry        */ NULL /* query_geometry */,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmSashCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
        /* border_unhighlight    */ XmInheritBorderUnhighlight,
        /* translations          */ NULL,
        /* arm_and_activate_proc */ NULL,
        /* synthetic resources   */ NULL,
	/* num syn res           */ 0,
        /* extension             */ (XtPointer)NULL /*&_XmSashPrimClassExtRec*/,
    },
    /* Sash Class part */
    {
	/* extension */ NULL
    }
};


WidgetClass xmSashWidgetClass = (WidgetClass)&xmSashClassRec;

static void
class_initialize(void)
{
    _XmSashCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmSASH_BIT);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "sash initialize\n"));
}

static void
realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
    Widget disp = XmGetXmDisplay(XtDisplay(w));

    *value_mask |= CWCursor;
    attributes->cursor =
	((XmDisplayInfo *) Display_DisplayInfo(disp))->SashCursor;

#define superclass (&xmPrimitiveClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Sash expose\n"));

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   Prim_TopShadowGC(w),
		   Prim_BottomShadowGC(w),
		   0, 0,
		   XtWidth(w),
		   XtHeight(w),
		   Prim_ShadowThickness(w),
		   XmSHADOW_OUT);

    if (!Prim_Highlighted(w))
    {
	XFillRectangle(XtDisplay(w), XtWindow(w),
		       XmParentBackgroundGC(w),
		       Prim_ShadowThickness(w), Prim_ShadowThickness(w),
		       XtWidth(w) - 2 * Prim_ShadowThickness(w),
		       XtHeight(w) - 2 * Prim_ShadowThickness(w));
    }
}

#if 0
static XtGeometryResult
query_geometry(Widget w,
	       XtWidgetGeometry *proposed,
	       XtWidgetGeometry *answer)
{
    /* Motif does not have this method */
    XmSashWidget sw = (XmSashWidget)w;
    answer->request_mode = CWWidth | CWHeight;

    answer->width = XtWidth(w) + 2 * Prim_ShadowThickness(sw);

    answer->height = XtHeight(w) + 2 * Prim_ShadowThickness(sw);

    if (((proposed->request_mode & (CWWidth | CWHeight))
	 == (CWWidth | CWHeight)) &&
	proposed->width == answer->width &&
	proposed->height == answer->height)
    {
	return XtGeometryYes;
    }
    else if (answer->width == XtWidth(sw) &&
	     answer->height == XtHeight(sw))
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
SashAction(Widget w,
	   XEvent *event,
	   String *params,
	   Cardinal *num_params)
{
    XmSashWidget sw = (XmSashWidget)w;
    SashCallDataRec cd;

    cd.event = event;
    cd.params = params;
    cd.num_params = *num_params;

    XtCallCallbackList((Widget)sw, Sash_SashAction(sw), &cd);

    expose(w, NULL, NULL);
}

static void
SashFocusIn(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Sash FocusIn\n"));
}

static void
SashFocusOut(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "SASH FOCUS OUT\n"));
}
