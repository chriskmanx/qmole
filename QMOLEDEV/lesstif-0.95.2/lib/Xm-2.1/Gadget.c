/**
 *
 * $Id: Gadget.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
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

static const char rcsid[] = "$Id: Gadget.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/GadgetP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/RepType.h>

#include <XmI/DebugUtil.h>

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);


static void input_dispatch(Widget w, XEvent *event, Mask event_mask);

#if 0
static Boolean visual_change(Widget w, Widget parent, Widget n);
#endif

static void focus_change(Widget w, XmFocusChange fevent);

static void secondary_object_create(Widget req, Widget new_w,
				    ArgList args, Cardinal *num_args);

static XmNavigability widget_navigable(Widget w);

static void gadget_border_unhighlight(Widget w);

static void gadget_border_highlight(Widget w);

static void _XmGetParentHighlightColor(Widget w,
				       int offset, XtArgVal *value);

static void _XmGetParentTopShadowColor(Widget w,
				       int offset, XtArgVal *value);

static void _XmGetParentBottomShadowColor(Widget w,
					  int offset, XtArgVal *value);

/*
 * Resources for the Gadget class
 */
#define Offset(field) XtOffsetOf(XmGadgetRec, gadget.field)
static XtResource resources[] =
{
    {
	XmNx, XmCPosition, XmRHorizontalPosition,
	sizeof(Dimension), XtOffsetOf(XmGadgetRec, rectangle.x),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNy, XmCPosition, XmRVerticalPosition,
	sizeof(Dimension), XtOffsetOf(XmGadgetRec, rectangle.y),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNwidth, XmCDimension, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmGadgetRec, rectangle.width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCDimension, XmRVerticalDimension,
	sizeof(Dimension), XtOffsetOf(XmGadgetRec, rectangle.height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNborderWidth, XmCBorderWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmGadgetRec, rectangle.border_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), Offset(traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightOnEnter, XmCHighlightOnEnter, XmRBoolean,
	sizeof(Boolean), Offset(highlight_on_enter),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNunitType, XmCUnitType, XmRUnitType,
	sizeof(unsigned char), Offset(unit_type),
	XmRCallProc, (XtPointer)_XmUnitTypeDefault
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(unsigned char), Offset(navigation_type),
	XmRImmediate, (XtPointer)XmNONE
    },
    {
	XmNhelpCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(help_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNuserData, XmCUserData, XmRPointer,
	sizeof(XtPointer), Offset(user_data),
	XmRPointer, NULL
    }
};

#define GOffset(field)	XtOffset(XmGadget, gadget.field)
#define ROffset(field)	XtOffset(XmGadget, rectangle.field)
#define OOffset(field)	XtOffset(XmGadget, object.field)
static XmSyntheticResource syn_resources[] =
{
    /* rectangle part */
    {
	XmNx,
	sizeof(Position), ROffset(x),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNy,
	sizeof(Position), ROffset(y),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNwidth,
	sizeof(Dimension), ROffset(width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNheight,
	sizeof(Dimension), ROffset(height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    /* gadget */
    {
	XmNhighlightThickness,
	sizeof(Dimension), GOffset(highlight_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNshadowThickness,
	sizeof(Dimension), GOffset(shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    /* object */
    {
	XmNhighlightColor,
	sizeof(Pixel), OOffset(parent),
	_XmGetParentHighlightColor, NULL
    },
    {
	XmNtopShadowColor,
	sizeof(Pixel), OOffset(parent),
	_XmGetParentTopShadowColor, NULL
    },
    {
	XmNbottomShadowColor,
	sizeof(Pixel), OOffset(parent),
	_XmGetParentBottomShadowColor, NULL
    }
};

/* *INDENT-OFF* */
static XmBaseClassExtRec _XmGadgetRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL,
    /* set_values_prehook        */ NULL,
    /* initialize_posthook       */ NULL,
    /* set_values_posthook       */ NULL,
    /* secondary_object_class    */ NULL,
    /* secondary_object_create   */ secondary_object_create,
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
    /* widget_navigable          */ widget_navigable,
    /* focus_change              */ focus_change,
    /* wrapper_data              */ NULL
};

static XmGadgetClassExtRec _XmGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ NULL,
    /* display_rect_proc         */ NULL,
#if XmVERSION >= 2
    /* margins_proc              */ NULL,
#endif
};

XmGadgetClassRec xmGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &rectObjClassRec,
        /* class_name            */ "XmGadget",
	/* widget_size           */ sizeof(XmGadgetRec),
	/* class_initialize      */ class_initialize,
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
	/* compress_exposure     */ XtExposeCompressSeries /*XtExposeNoCompress*/,
	/* compress_enterleave   */ True /*False*/,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ _XmGadgetGetValuesHook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
        /* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmGadgetRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ gadget_border_highlight,
        /* border_unhighlight */ gadget_border_unhighlight,
        /* arm_and_activate   */ NULL,
        /* input_dispatch     */ input_dispatch, 
        /* visual_change      */ NULL /*visual_change*/,
        /* syn_resources      */ syn_resources,
        /* num_syn_resources  */ XtNumber(syn_resources),
	/* cache_part         */ NULL,
        /* extension          */ (XtPointer)&_XmGadgetClassExtRec
    }

};
/* *INDENT-ON* */


WidgetClass xmGadgetClass = (WidgetClass)&xmGadgetClassRec;

static void
class_initialize(void)
{
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmGadget class_initialize\n"));

    _XmRegisterConverters();
    _XmRegisterPixmapConverters();
    _XmInitializeExtensions();
    _XmGadgetRectClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmGadgetClass gc, sc;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmGadget class_part_initialize\n"));

    gc = (XmGadgetClass)widget_class;
    sc = (XmGadgetClass)((XmGadgetClass)widget_class)->rect_class.superclass;

    if (gc->gadget_class.border_highlight == XmInheritBorderHighlight)
    {
	gc->gadget_class.border_highlight =
	    sc->gadget_class.border_highlight;
    }
    if (gc->gadget_class.border_unhighlight == XmInheritBorderUnhighlight)
    {
	gc->gadget_class.border_unhighlight =
	    sc->gadget_class.border_unhighlight;
    }
    if (gc->gadget_class.visual_change == XmInheritVisualChange)
    {
	gc->gadget_class.visual_change =
	    sc->gadget_class.visual_change;
    }
    if (gc->gadget_class.input_dispatch == XmInheritInputDispatch)
    {
	gc->gadget_class.input_dispatch =
	    sc->gadget_class.input_dispatch;
    }
    if (gc->gadget_class.arm_and_activate == XmInheritArmAndActivate)
    {
	gc->gadget_class.arm_and_activate =
	    sc->gadget_class.arm_and_activate;
    }

    if (widget_class != xmGadgetClass)
    {
	XmGadgetClassExt *gce = NULL, *sce = NULL;

	gce = _XmGetGadgetClassExtPtr(gc, NULLQUARK);
	sce = _XmGetGadgetClassExtPtr(sc, NULLQUARK);

	if (gce && sce && *gce && *sce)
	{
	    if ((*gce)->widget_baseline == XmInheritBaselineProc)
	    {
		(*gce)->widget_baseline = (*sce)->widget_baseline;
	    }
	    if ((*gce)->widget_display_rect == XmInheritDisplayRectProc)
	    {
		(*gce)->widget_display_rect = (*sce)->widget_display_rect;
	    }
	}
    }

    _XmBaseClassPartInitialize(widget_class);
    _XmFastSubclassInit(widget_class, XmGADGET_BIT);

    /* compile the resources */
    if (widget_class == xmGadgetClass)
    {
	_XmSortResourceList((XrmResource **)gc->rect_class.resources,
			    gc->rect_class.num_resources);
    }

    _XmBuildGadgetResources(widget_class);
}

static void
secondary_object_create(Widget req, Widget new_w,
			ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    Arg argl[1];
    ArgList merged;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmGadget secondary_object_create\n"));

    XtSetArg(argl[0], XmNlogicalParent, new_w);

    if (*num_args)
    {
	merged = XtMergeArgLists(args, *num_args, argl, XtNumber(argl));

	bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);

	XtCreateWidget(XtName(new_w), (*bce)->secondaryObjectClass,
		       XtParent(new_w)
		       ? XtParent(new_w)
		       : new_w,
		       merged, *num_args + 1);

	XtFree((char *)merged);
    }
    else
    {
	bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);

	XtCreateWidget(XtName(new_w), (*bce)->secondaryObjectClass,
		       XtParent(new_w)
		       ? XtParent(new_w)
		       : new_w,
		       argl, 1);
    }
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmBaseClassExt bce;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmGadget initialize\n"));

    /* Force the borderWidth to 0 */
    new_w->core.border_width = 0;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     G_NavigationType(new_w),
			     new_w))
    {
	G_NavigationType(new_w) = XmNONE;
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     G_UnitType(new_w),
			     new_w))
    {
	G_UnitType(new_w) = XmPIXELS;
    }

    if (XtWidth(request) == (Dimension)0)
    {
	XtWidth(new_w) = (G_HighlightThickness(new_w) * 2
			  + G_ShadowThickness(new_w) * 2);
    }
    if (XtHeight(request) == (Dimension)0)
    {
	XtHeight(new_w) = (G_HighlightThickness(new_w) * 2
			   + G_ShadowThickness(new_w) * 2);
    }

    _XmGadgetImportArgs(new_w, args, num_args);

    /* BaseClass stuff provides this */
    _XmGadgetImportSecondaryArgs(new_w, args, num_args);

    bce = *(XmBaseClassExt *)_XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);

    if (bce && bce->secondaryObjectClass)
    {
	if (bce->secondaryObjectCreate)
	{
	    (bce->secondaryObjectCreate) (request, new_w, args, num_args);
	}
    }

    _XmNavigInitialize(request, new_w, args, num_args);

    G_EventMask(new_w) = 0;
    G_HaveTraversal(new_w) = False;
    G_Highlighted(new_w) = False;
    G_HighlightDrawn(new_w) = False;
}

static void
destroy(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmGadget destroy\n"));

	_XmNavigDestroy(w);
}

static Boolean
set_values(Widget current, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    Boolean need_refresh = False;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmGadget set_values\n"));

    /* Force the borderWidth to 0 */
    new_w->core.border_width = 0;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     G_UnitType(new_w),
			     new_w))
    {
	G_UnitType(new_w) = G_UnitType(current);
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     G_NavigationType(new_w),
			     new_w))
    {
	G_NavigationType(new_w) = G_NavigationType(current);
    }

    _XmGadgetImportArgs(new_w, args, num_args);

    need_refresh = _XmNavigSetValues(current, request, new_w, args, num_args);

    return need_refresh;
}

static void
input_dispatch(Widget w,
	       XEvent *event,
	       Mask event_mask)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "Inside base Gadget input_dispatch() routine "
		      "(you really should write one for this gadget... :)\n"));

    if (event_mask & XmENTER_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Enter event sent to gadget %d\n", w));
    }
    else if (event_mask & XmLEAVE_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Leave event sent to gadget %d\n", w));
    }
    else if (event_mask & XmFOCUS_IN_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Focus in event sent to gadget %d\n", w));
    }
    else if (event_mask & XmFOCUS_OUT_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Focus out event sent to gadget %d\n", w));
    }
    else if (event_mask & XmMOTION_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Motion event sent to gadget %d\n", w));
    }
    else if (event_mask & XmARM_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Arm event sent to gadget %d\n", w));
    }
    else if (event_mask & XmACTIVATE_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Activate event sent to gadget %d\n", w));
    }
    else if (event_mask & XmHELP_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Help event sent to gadget %d\n", w));
    }
    else if (event_mask & XmKEY_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Key event sent to gadget %d\n", w));
    }
    else if (event_mask & XmMULTI_ARM_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Multi arm event sent to gadget %d\n", w));
    }
    else if (event_mask & XmMULTI_ACTIVATE_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Multi activate event sent to gadget %d\n", w));
    }
    else if (event_mask & XmBDRAG_EVENT)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "BDrag event sent to gadget %d\n", w));
    }
}

#if 0
static Boolean
visual_change(Widget w,
	      Widget parent,
	      Widget n)
{
    return False;
}
#endif

/*
 * ENTER/LEAVE should only happen when mwm is in pointer-follows-mouse mode.
 * FOCUS_IN/FOCUS_OUT otherwise.
 */
static void
focus_change(Widget w, XmFocusChange change)
{
    XmGadgetClass gc = (XmGadgetClass)XtClass(w);

	DEBUGOUT(_LtDebug(__FILE__, w, "XmGadget focus_change\n"));

    switch (change)
    {
    case XmENTER:
	if (!G_HighlightOnEnter(w))
	{
	    break;
	}

	if (gc->gadget_class.border_highlight)
	{
	    (gc->gadget_class.border_highlight) (w);
	}

	break;

    case XmFOCUS_IN:
	G_HaveTraversal(w) = True;

	if (gc->gadget_class.border_highlight)
	{
	    (gc->gadget_class.border_highlight) (w);
	}

	break;

    case XmLEAVE:
	if (!G_HighlightOnEnter(w))
	{
	    break;
	}

	if (gc->gadget_class.border_unhighlight)
	{
	    (gc->gadget_class.border_unhighlight) (w);
	}

	break;

    case XmFOCUS_OUT:
	G_HaveTraversal(w) = False;

	if (gc->gadget_class.border_unhighlight)
	{
	    (gc->gadget_class.border_unhighlight) (w);
	}

	break;
    }
}

static XmNavigability
widget_navigable(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmGadget widget_navigable\n"));

    if (XtSensitive(w) && G_TraversalOn(w))
    {
	if (G_NavigationType(w) == XmSTICKY_TAB_GROUP ||
	    G_NavigationType(w) == XmEXCLUSIVE_TAB_GROUP ||
	    (G_NavigationType(w) == XmTAB_GROUP && !_XmShellIsExclusive(w)))
	{
	    return XmTAB_NAVIGABLE;
	}

	return XmCONTROL_NAVIGABLE;
    }

    return XmNOT_NAVIGABLE;
}

static void
gadget_border_unhighlight(Widget w)
{

    DEBUGOUT(_LtDebug(__FILE__, w, "gadget_border_unhighlight\n"));

    /* with zero width, we don't need this... */
    if (G_HighlightThickness(w) == 0)
    {
	return;
    }

    if (XmIsManager(XtParent(w)))
    {
	_XmDrawHighlight(XtDisplayOfObject(w), XtWindowOfObject(w),
			 XmParentBackgroundGC(w),
			 XtX(w), XtY(w), XtWidth(w), XtHeight(w),
			 G_HighlightThickness(w), LineSolid);
    }
    else
    {
	_XmClearBorder(XtDisplay(w),
		       XtWindow(w),
		       XtX(w), XtY(w),
		       XtWidth(w), XtHeight(w),
		       G_HighlightThickness(w));
    }

    G_Highlighted(w) = False;
    G_HighlightDrawn(w) = False;
}

static void
gadget_border_highlight(Widget w)
{

    DEBUGOUT(_LtDebug(__FILE__, w, "gadget_border_highlight\n"));

    /* with zero width, we don't need this... */
    if (G_HighlightThickness(w) == 0)
    {
	return;
    }

    _XmDrawHighlight(XtDisplayOfObject(w), XtWindowOfObject(w),
		     XmParentHighlightGC(w),
		     XtX(w), XtY(w), XtWidth(w), XtHeight(w),
		     G_HighlightThickness(w), LineSolid);

    G_Highlighted(w) = True;
    G_HighlightDrawn(w) = True;
}

static void
_XmGetParentHighlightColor(Widget w, int offset, XtArgVal *value)
{
    *value = XmParentHighlightColor(w);
}

static void
_XmGetParentTopShadowColor(Widget w, int offset, XtArgVal *value)
{
    *value = XmParentTopShadowColor(w);
}

static void
_XmGetParentBottomShadowColor(Widget w, int offset, XtArgVal *value)
{
    *value = XmParentBottomShadowColor(w);
}

/*
 * initialize the synthetic resource crap.  The only way I know to get the
 * CachePart stuff for Gadget subclasses is via the baseclass extension
 * (secondary stuff); unfortunately, I don't know how that works yet.
 */
void
_XmBuildGadgetResources(WidgetClass c)
{
    XmGadgetClass super;
    XmGadgetClass gc = (XmGadgetClass)c;
    XmBaseClassExt *bce = NULL;
    XmExtObjectClass subpclass, esuper;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmBuildGadgetResources\n"));


    _XmInitializeSyntheticResources(gc->gadget_class.syn_resources,
				    gc->gadget_class.num_syn_resources);

    /*
     * if we're the Gadget, our super doesn't have synthetic resources
     * nor do we have an extension, so we can bail here.
     */
    if (c == xmGadgetClass)
    {
	return;
    }

    super = ((XmGadgetClass)c->core_class.superclass);
    _XmBuildResources(&gc->gadget_class.syn_resources,
		      &gc->gadget_class.num_syn_resources,
		      super->gadget_class.syn_resources,
		      super->gadget_class.num_syn_resources);

    /*
     * we can get the subpart stuff from the baseclass extension, now that
     * it's there.
     */
    bce = (XmBaseClassExt *)_XmGetBaseClassExtPtr(c, XmQmotif);

    if (!*bce || !(subpclass = (XmExtClassRec *)(*bce)->secondaryObjectClass))
    {
	return;
    }

    _XmInitializeSyntheticResources(subpclass->ext_class.syn_resources,
				    subpclass->ext_class.num_syn_resources);

    /* core dump avoidance overkill */
    esuper = (XmExtObjectClass)subpclass->object_class.superclass;
    if (subpclass == &xmExtClassRec || esuper == &xmExtClassRec)
    {
	return;
    }

    _XmBuildResources(&subpclass->ext_class.syn_resources,
		      &subpclass->ext_class.num_syn_resources,
		      esuper->ext_class.syn_resources,
		      esuper->ext_class.num_syn_resources);
}
