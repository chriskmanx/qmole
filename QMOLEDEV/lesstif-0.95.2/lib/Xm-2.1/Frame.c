/** 
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Frame.c,v 1.4 2004/10/16 16:52:03 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Id: Frame.c,v 1.4 2004/10/16 16:52:03 dannybackx Exp $";


#include <LTconfig.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/FrameP.h>
#include <Xm/TransltnsP.h>
#include <Xm/RepType.h>

#include <XmI/DebugUtil.h>

#define CON_FROM_RESIZE	0

/*
 * Forward Declarations
 */
/* core */
/* static void class_initialize(void); */

static void class_part_initialize(WidgetClass widget_class);

static void initialize(Widget request,
		       Widget new_w,
		       ArgList args,
		       Cardinal *num_args);

static void resize(Widget w);

static Boolean set_values(Widget current,
			  Widget request,
			  Widget new_w,
			  ArgList args,
			  Cardinal *num_args);

static void expose(Widget w, XEvent *event, Region region);

static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *request,
				       XtWidgetGeometry *reply);

/* composite */
static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);

static void change_managed(Widget w);

/* constraint */
static void constraint_initialize(Widget request,
				  Widget new_w,
				  ArgList args,
				  Cardinal *num_args);

static void constraint_destroy(Widget w);

static Boolean constraint_set_values(Widget current,
				     Widget request,
				     Widget new_w,
				     ArgList args,
				     Cardinal *num_args);

/* helper */
static void _XmFrameComputeSize(Widget w, Widget instig,
				XtWidgetGeometry *desired,
				Dimension *wd, Dimension *ht);

static void _XmFrameConfigureChildren(Widget w, Widget instig,
				      XtWidgetGeometry *desired,
				      Dimension wd, Dimension ht,
				      Boolean commit);

/*
 * resources for the frame class
 */
#define Offset(field) XtOffsetOf(XmFrameRec, frame.field)
static XtResource resources[] =
{
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNshadowType, XmCShadowType, XmRShadowType,
	sizeof(unsigned char), Offset(shadow_type),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION /*((unsigned char)XmUNSPECIFIED)*/
    },
    /* manager override */
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmFrameRec, manager.shadow_thickness),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

/*
 * Constraint Resources for frame's children
 */
#undef Offset
#define Offset(field) XtOffsetOf(XmFrameConstraintRec, frame.field)
static XtResource frameConstraintResources[] =
{
#if 0
    /*
     * This is a weird one. XmNchildType is obsoleted, and replaced by
     * XmNframeChildType. Unfortunately the former is also still supposed
     * to work.
     */
    {
	XmNframeChildType, XmCChildType, XmRChildType,
	sizeof(unsigned char), Offset(child_type),
	XtRImmediate, (XtPointer)XmFRAME_WORKAREA_CHILD
    },
#else
    /*
     * This is the Motif 1.2 version
     * We can't have both of these in here (if an application sets
     * only one of the resources, the other's default value would
     * still apply).
     * Therefore we'll have to simulate one in code.
     */
    {
	XmNchildType, XmCChildType, XmRChildType,
	sizeof(unsigned char), Offset(child_type),
	XtRImmediate, (XtPointer)XmFRAME_WORKAREA_CHILD
    },
#endif
    {
	XmNchildHorizontalAlignment, XmCChildHorizontalAlignment,
	 XmRChildHorizontalAlignment,
	sizeof(unsigned char), Offset(child_h_alignment),
	XtRImmediate, (XtPointer)XmALIGNMENT_BEGINNING
    },
    {
	XmNchildVerticalAlignment, XmCChildVerticalAlignment,
	XmRChildVerticalAlignment,
	sizeof(unsigned char), Offset(child_v_alignment),
	XtRImmediate, (XtPointer)XmALIGNMENT_CENTER
    },
    {
	XmNchildHorizontalSpacing, XmCChildHorizontalSpacing,
	XmRHorizontalDimension,
	sizeof(Dimension), Offset(child_h_spacing),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    }
};

static XmSyntheticResource constraint_syn_resources[] =
{
    {
	XmNchildHorizontalSpacing,
	sizeof(Dimension), Offset(child_h_spacing),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

#if 0
static XmBaseClassExtRec _XmFrameCoreClassExtRec = {
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
#endif

XmFrameClassRec xmFrameClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmFrame",
	/* widget_size           */ sizeof(XmFrameRec),
	/* class_initialize      */ NULL /*class_initialize*/,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
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
	/* extension             */ (XtPointer)NULL /*&_XmFrameCoreClassExtRec*/
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager, 
        /* change_managed   */ change_managed, 
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ (XtPointer)NULL
    },
    /* Constraint class part */
    {
	/* subresources      */ frameConstraintResources,
        /* subresource_count */ XtNumber(frameConstraintResources), 
        /* constraint_size   */ sizeof(XmFrameConstraintRec),
        /* initialize        */ constraint_initialize,
        /* destroy           */ constraint_destroy,
        /* set_values        */ constraint_set_values,
        /* extension         */ NULL,  
    },
    /* XmManager class part */
    {
	/* translations                 */ XtInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ constraint_syn_resources,
        /* num_syn_constraint_resources */ XtNumber(constraint_syn_resources),
        /* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)NULL
    },
    /* XmFrame Area part */
    {
	/* extension */ NULL,
    },
};

WidgetClass xmFrameWidgetClass = (WidgetClass)&xmFrameClassRec;

/* core methods */

#if 0
static void
class_initialize()
{
    _XmFrameCoreClassExtRec.record_type = XmQmotif;
}
#endif

static void
class_part_initialize(WidgetClass widget_class)
{
    CompositeClassExtension ext, *extptr;
    XmFrameWidgetClass fclass = (XmFrameWidgetClass)widget_class;

    extptr = (CompositeClassExtension *)_XmGetClassExtensionPtr((XmGenericClassExt *)&(fclass->composite_class.extension),
								NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);

	if (ext != NULL)
	{
	    ext->next_extension = fclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    fclass->composite_class.extension = (XtPointer)ext;
	}
    }

    _XmFastSubclassInit(widget_class, XmFRAME_BIT);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Frame_TitleArea(new_w) = NULL;
    Frame_WorkArea(new_w) = NULL;

    if (Frame_ShadowType(new_w) == (unsigned char)XmINVALID_DIMENSION /*XmUNSPECIFIED*/)
    {
	if (XtIsShell(XtParent(new_w)))
	{
	    Frame_ShadowType(new_w) = XmSHADOW_OUT;
	}
	else
	{
	    Frame_ShadowType(new_w) = XmSHADOW_ETCHED_IN;
	}
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRShadowType),
			     Frame_ShadowType(new_w),
			     new_w))
    {
	Frame_ShadowType(new_w) = XmSHADOW_ETCHED_IN;
    }

    if (MGR_ShadowThickness(new_w) == XmINVALID_DIMENSION)
    {
	if (XtIsShell(XtParent(new_w)))
	{
	    MGR_ShadowThickness(new_w) = 1;
	}
	else
	{
	    MGR_ShadowThickness(new_w) = 2;
	}
    }

    Frame_OldShadowX(new_w) = Frame_OldShadowY(new_w) = 0;
    Frame_OldShadowThickness(new_w) = MGR_ShadowThickness(new_w);
    Frame_OldWidth(new_w) = XtWidth(new_w);
    Frame_OldHeight(new_w) = XtHeight(new_w);
    Frame_ProcessingConstraints(new_w) = True;

    if (XtWidth(new_w) != 0 && XtHeight(new_w) != 0)
    {
	Frame_ProcessingConstraints(new_w) = False;
    }
}

static Boolean
set_values(Widget current, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean redisplay = False;
    Dimension wd, ht;

    if (Frame_MarginWidth(new_w) != Frame_MarginWidth(current) ||
	Frame_MarginHeight(new_w) != Frame_MarginHeight(current) ||
	Frame_ShadowType(new_w) != Frame_ShadowType(current))
    {
	redisplay = True;
    }

    if (redisplay && XtIsRealized(new_w) && XtIsManaged(new_w))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "Frame set_values %p W %i H %i\n",
			  new_w, XtWidth(new_w), XtHeight(new_w)));

	_XmFrameComputeSize(new_w, NULL, NULL, &wd, &ht);

	/* let the intrinsics do the geom negotiation.  If it succeeds,
	 * then resize() will be called. */
	XtWidth(new_w) = wd;
	XtHeight(new_w) = ht;
    }

    Frame_OldShadowX(new_w) = 0;
    if (Frame_TitleArea(new_w) && XtIsManaged(Frame_TitleArea(new_w)))
    {
	Frame_OldShadowY(new_w) = XtHeight(Frame_TitleArea(new_w)) +
	    MGR_ShadowThickness(new_w) +
	    Frame_MarginHeight(new_w);
    }
    else
    {
	Frame_OldShadowY(new_w) = MGR_ShadowThickness(new_w) +
	    Frame_MarginHeight(new_w);
    }

    Frame_OldShadowThickness(new_w) = MGR_ShadowThickness(current);
    Frame_OldWidth(new_w) = XtWidth(new_w);
    Frame_OldHeight(new_w) = XtHeight(new_w);

    return redisplay;
}

static void
resize(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "Frame resize (%d %d)\n", XtWidth(w), XtHeight(w)));

#if CON_FROM_RESIZE
    if (!XtIsRealized(w))
    {
	Frame_ProcessingConstraints(w) = False;
    }
#endif

    _XmClearShadowType(w, 
    	Frame_OldWidth(w), 
    	Frame_OldHeight(w),
    	Frame_OldShadowThickness(w),
    	0);
    _XmFrameConfigureChildren(w, NULL, NULL, XtWidth(w), XtHeight(w), True);
    Frame_OldWidth(w) = XtWidth(w);
    Frame_OldHeight(w) = XtHeight(w);
}

static void
expose(Widget w, XEvent *event, Region region)
{
    Position topline_y = 0;

    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
    {
	switch (FrameC_ChildVAlignment(Frame_TitleArea(w)))
	{
	case XmALIGNMENT_CENTER:
	    topline_y = XtHeight(Frame_TitleArea(w)) / 2;
	    break;

	case XmALIGNMENT_BASELINE_TOP:
	    /* change this to work with XmWidgetBaseline */
	    topline_y = XtHeight(w);
	    break;

	case XmALIGNMENT_WIDGET_TOP:
	    topline_y = XtHeight(Frame_TitleArea(w));
	    break;

	case XmALIGNMENT_WIDGET_BOTTOM:
	    topline_y = 0;
	    break;

	}
    }

    _XmDrawShadows(XtDisplay(w),
		   XtWindow(w),
		   MGR_TopShadowGC(w),
		   MGR_BottomShadowGC(w),
		   0,
		   topline_y,
		   XtWidth(w),
		   XtHeight(w) - topline_y,
		   MGR_ShadowThickness(w),
		   Frame_ShadowType(w));

    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
    {
	XFillRectangle(XtDisplay(w), XtWindow(w), MGR_BackgroundGC(w),
		       XtX(Frame_TitleArea(w)), XtY(Frame_TitleArea(w)),
		       XtWidth(Frame_TitleArea(w)),
		       XtHeight(Frame_TitleArea(w)));
    }

    /* redisplay our gadget children -- if any */
    _XmRedisplayGadgets(w, event, region);
}

static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
#if 0
    Dimension wd, ht;
    XtGeometryResult r;
    XtWidgetGeometry rr = *request;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "Frame query_geometry %p W %i H %i\n",
		       w, XtWidth(w), XtHeight(w)));

    _XmFrameComputeSize(w, NULL, NULL, &wd, &ht);

    reply->width = wd;
    reply->height = ht;
    reply->request_mode = CWWidth | CWHeight;

    if ((request->request_mode & (CWWidth | CWHeight)) ==
	(CWWidth | CWHeight) &&
	reply->width == request->width && reply->height == request->height)
    {
	r = XtGeometryYes;
    }
    else if (((request->request_mode & CWWidth) &&
	      (request->width == XtWidth(w))) &&
	     ((request->request_mode & CWHeight) &&
	      (request->height == XtHeight(w))))
    {
	r = XtGeometryNo;
    }
    else
    {
	r = XtGeometryAlmost;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "QueryGeometry [%s] => %s [%s]\n",
		      _LtDebugWidgetGeometry2String(&rr),
		      _LtDebugGeometryResult2String(r),
		      _LtDebugWidgetGeometry2String(reply)));

    return r;
#else
    Dimension wd, ht;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "Frame query_geometry %p W %i H %i\n",
		       w, XtWidth(w), XtHeight(w)));

    _XmFrameComputeSize(w, NULL, NULL, &wd, &ht);

    reply->width = wd;
    reply->height = ht;

    return _XmGMReplyToQueryGeometry(w, request, reply);
#endif
}

/*
 * This geometry_manager is a weirdo compared to many others in LessTif.
 *
 * Frame can have two children - a title and a work area.
 * Geometry Manager replies differently depending on which one asks a question.
 *
 * Implementation before October 1996 didn't do that :-(
 */
static XtGeometryResult
geometry_manager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    Dimension wd, ht;
    Widget fw = XtParent(w);
    XtWidgetGeometry wants;

    DEBUGOUT(_LtDebug2(__FILE__, fw, w, "Frame geometry_manager [%s]\n",
		       _LtDebugWidgetGeometry2String(request)));

    wants = *request;
    *reply = wants;

    if (FrameC_ChildType(w) != XmFRAME_WORKAREA_CHILD &&
	FrameC_ChildType(w) != XmFRAME_TITLE_CHILD)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "Frame geometry_manager => No\n"));

	return XtGeometryNo;
    }
    if (((request->request_mode & CWHeight) && request->height == 0) ||
        ((request->request_mode & CWWidth) && request->width == 0))
    {
	_XmWarning(w, "%s(%d):geometry_manager() - Widget requesting %s from %s\n\tReturning XtGeometryNo\n",
		__FILE__, __LINE__, 
		_LtDebugWidgetGeometry2String(request),
		XtName(fw));
	return XtGeometryNo;
    }

    /* Calculate desired Frame geometry in wd/ht from requested child
     * geometry in "request" */
    DEBUGOUT(_LtDebug(__FILE__, fw,
		      "Frame geometry_manager %p W %i H %i\n",
		       fw, XtWidth(fw), XtHeight(fw)));

    _XmFrameComputeSize(fw, w, reply, &wd, &ht);

    if (wd != XtWidth(fw) || ht != XtHeight(fw))
    {
	XtWidgetGeometry geo;

	geo.width = wd;
	geo.height = ht;
	geo.request_mode = CWWidth | CWHeight | CWBorderWidth;
	geo.border_width = XtBorderWidth(fw);

	DEBUGOUT(_LtDebug(__FILE__, fw,
			  "Frame requesting geo: %s : is %d %d\n",
			  _LtDebugWidgetGeometry2String(&geo),
			  XtWidth(fw), XtHeight(fw)));

	if (_XmMakeGeometryRequest(fw, &geo) == XtGeometryYes)
	{
	    DEBUGOUT(_LtDebug(__FILE__, fw,
			      "Got geo %d %d : is %d %d\n",
			      geo.width, geo.height,
			      XtWidth(fw), XtHeight(fw)));
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, fw,
			      "No. Geo is %d %d : WH: %d %d\n",
			      geo.width, geo.height,
			      XtWidth(fw), XtHeight(fw)));
	}

	DEBUGOUT(_LtDebug2(__FILE__, fw, w,
			"geometry_manager : _XmFrameConfigureChildren => %s\n",
			   _LtDebugWidgetGeometry2String(reply)));

	_XmClearShadowType(fw, 
	    Frame_OldWidth(fw), 
	    Frame_OldHeight(fw),
	    Frame_OldShadowThickness(fw),
	    0);
	_XmFrameConfigureChildren(fw, w, reply, XtWidth(fw), XtHeight(fw),
				  False);
    }
    else
    {
	_XmClearShadowType(fw, 
	    Frame_OldWidth(fw), 
	    Frame_OldHeight(fw),
	    Frame_OldShadowThickness(fw),
	    0);
	_XmFrameConfigureChildren(fw, w, reply, XtWidth(fw), XtHeight(fw),
				  False);
	DEBUGOUT(_LtDebug2(__FILE__, fw, w,
			 "geometry_manager determined child size at %d [%s]\n",
			   __LINE__, _LtDebugWidgetGeometry2String(reply)));
    }

    Frame_OldShadowX(fw) = 0;
    if (Frame_TitleArea(fw) && XtIsManaged(Frame_TitleArea(fw)))
    {
	Frame_OldShadowY(fw) = XtHeight(Frame_TitleArea(fw)) +
	    MGR_ShadowThickness(fw) +
	    Frame_MarginHeight(fw);
    }
    else
    {
	Frame_OldShadowY(fw) = MGR_ShadowThickness(fw) +
	    Frame_MarginHeight(fw);
    }

    Frame_OldWidth(fw) = XtWidth(fw);
    Frame_OldHeight(fw) = XtHeight(fw);

    DEBUGOUT(_LtDebug2(__FILE__, fw, w,
			   "Frame geometry_manager => (%s) (%s)\n",
			   _LtDebugWidgetGeometry2String(&wants), _LtDebugWidgetGeometry2String(reply)));
    /* Slight change of logic here. If the caller only requested a change in
     * one direction and we could grant it, we should. The previous version
     * would only return GeometryYes if the caller asked to change both sizes.
     * --dwilliss 27-Aug-04 */
    if (((wants.request_mode & CWWidth) == 0 || reply->width == wants.width) &&
		        ((wants.request_mode & CWHeight) == 0 || reply->height == wants.height))
    {
	DEBUGOUT(_LtDebug2(__FILE__, fw, w,
			   "Frame geometry_manager => Yes (%d %d)\n",
			   reply->width, reply->height));

	_XmFrameConfigureChildren(fw, w, reply, XtWidth(fw), XtHeight(fw),
				  True);

	XtX(w) = reply->x;
	XtY(w) = reply->y;
	XtWidth(w) = reply->width;
	XtHeight(w) = reply->height;
	XtBorderWidth(w) = reply->border_width;

	return XtGeometryYes;
    }
    else if (reply->width == XtWidth(w) && reply->height == XtHeight(w))
    {
	DEBUGOUT(_LtDebug2(__FILE__, fw, w,
			   "Frame geometry_manager same as current=> No\n"));

	return XtGeometryNo;
    }
    else
    {
	DEBUGOUT(_LtDebug2(__FILE__, fw, w,
			   "Frame geometry_manager => Almost (%d %d)\n",
			   reply->width, reply->height));

	reply->request_mode &= wants.request_mode;
	if (reply->request_mode & CWX && request->x == wants.x)
	{
	    reply->request_mode &= ~CWX;
	}
	if (reply->request_mode & CWY && request->y == wants.y)
	{
	    reply->request_mode &= ~CWY;
	}
	if (reply->request_mode & CWWidth && request->width == wants.width)
	{
	    reply->request_mode &= ~CWWidth;
	}
	if (reply->request_mode & CWHeight && request->height == wants.height)
	{
	    reply->request_mode &= ~CWHeight;
	}
	return XtGeometryAlmost;
    }
}

static void
change_managed(Widget w)
{
    Cardinal i;
    Widget child;
    Dimension wd, ht;
    XtWidgetGeometry request;

    Frame_TitleArea(w) = NULL;
    Frame_WorkArea(w) = NULL;

    for (i = 0; i < ((XmFrameWidget)w)->composite.num_children; i++)
    {
	child = ((XmFrameWidget)w)->composite.children[i];

	if (!XtIsManaged(child))
	{
	    continue;
	}
	else if (FrameC_ChildType(child) != XmFRAME_GENERIC_CHILD)
	{
	    if (FrameC_ChildType(child) == XmFRAME_TITLE_CHILD)
	    {
		if (!Frame_TitleArea(w))
		    Frame_TitleArea(w) = child;
	    }
	    else if (FrameC_ChildType(child) == XmFRAME_WORKAREA_CHILD)
	    {
		if (!Frame_WorkArea(w))
		    Frame_WorkArea(w) = child;
	    }
	    else
	    {
		DEBUGOUT(_LtDebug(__FILE__, w,
				  "XmFrame illegal child type resource\n"));
	    }
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "Frame change_managed %p W %i H %i\n",
		       w, XtWidth(w), XtHeight(w)));

    _XmClearShadowType(w, 
    	Frame_OldWidth(w), 
    	Frame_OldHeight(w),
    	Frame_OldShadowThickness(w),
    	0);
    _XmFrameComputeSize(w, NULL, NULL, &wd, &ht);

    request.request_mode = (CWWidth | CWHeight | CWBorderWidth);
    request.width = wd;
    request.height = ht;
    request.border_width = XtBorderWidth(w);

    if (_XmMakeGeometryRequest(w, &request) == XtGeometryYes)
    {
	wd = request.width;
	ht = request.height;
    }

    _XmFrameConfigureChildren(w, NULL, NULL, wd, ht, True);

    Frame_OldShadowX(w) = 0;
    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
    {
	Frame_OldShadowY(w) = XtHeight(Frame_TitleArea(w)) +
	    MGR_ShadowThickness(w) +
	    Frame_MarginHeight(w);
    }
    else
    {
	Frame_OldShadowY(w) = MGR_ShadowThickness(w) +
	    Frame_MarginHeight(w);
    }

    Frame_OldWidth(w) = XtWidth(w);
    Frame_OldHeight(w) = XtHeight(w);

    _XmNavigChangeManaged(w);
}

/* Constraint methods */

static void
constraint_initialize(Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    Cardinal i;

    DEBUGOUT(_LtDebug2(__FILE__, XtParent(new_w), new_w, "constraint_initialize(%s)\n",
			    _LtDebugFrameChildType2String(FrameC_ChildType(new_w))));

    if (num_args)
	for (i=0; i<*num_args; i++) {
	    if (strcmp(args[i].name, XmNframeChildType) == 0) {
		FrameC_ChildType(new_w) = args[i].value;
	    }
	}

    if (FrameC_ChildHSpacing(new_w) == XmINVALID_DIMENSION)
	FrameC_ChildHSpacing(new_w) = Frame_MarginWidth(XtParent(new_w)) + 10;
    if (FrameC_ChildType(new_w) == XmFRAME_WORKAREA_CHILD)
    {
	    Frame_WorkArea(XtParent(new_w))  = new_w;
    }
    if (FrameC_ChildType(new_w) == XmFRAME_TITLE_CHILD)
    {
	    Frame_TitleArea(XtParent(new_w))  = new_w;
    }
}

static void constraint_destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "constraint_destroy()\n"));
}

static Boolean
constraint_set_values(Widget current, Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    Boolean redisplay = False;

    Cardinal i;

    if (num_args)
	for (i=0; i<*num_args; i++) {
	    if (strcmp(args[i].name, XmNframeChildType) == 0) {
		FrameC_ChildType(new_w) = args[i].value;
	    }
	}

    if (FrameC_ChildType(current) != FrameC_ChildType(new_w) ||
	FrameC_ChildHAlignment(current) != FrameC_ChildHAlignment(new_w) ||
	FrameC_ChildHSpacing(current) != FrameC_ChildHSpacing(new_w) ||
	FrameC_ChildVAlignment(current) != FrameC_ChildVAlignment(new_w))
    {
	redisplay = True;
    }

    return redisplay;
}

/*
 * Danny 11/1/1997 - adding some risk to the implementation.
 *
 * This function is only called with an instigator and a desired
 * geometry from geometry_manager.
 */
static void
_XmFrameComputeSize(Widget w, Widget instig, XtWidgetGeometry *desired,
		    Dimension *wd, Dimension *ht)
{
    Dimension curw, curh;
    XtWidgetGeometry geo;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmFrameComputeSize"));

    if (instig)
    {
	DEBUGOUT(_LtDebug0(__FILE__, w, " Instig %s (desired geo %s)",
			   XtName(instig), _LtDebugWidgetGeometry2String(desired)));
    }

    if (Frame_TitleArea(w))
    {
	DEBUGOUT(_LtDebug0(__FILE__, w, " Title %s wid %d ht %d",
		       XtName(Frame_TitleArea(w)), XtWidth(Frame_TitleArea(w)),
			   XtHeight(Frame_TitleArea(w))));
    }

    if (Frame_WorkArea(w))
    {
	DEBUGOUT(_LtDebug0(__FILE__, w, " Work %s wid %d ht %d",
			 XtName(Frame_WorkArea(w)), XtWidth(Frame_WorkArea(w)),
			   XtHeight(Frame_WorkArea(w))));
    }

    DEBUGOUT(_LtDebug0(__FILE__, w, "\n"));

    curw = curh = 0;

    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
    {
	if (instig == Frame_TitleArea(w))
	{
	    if (desired->request_mode & CWHeight)
	    {
		curh += desired->height +
		    2 * XtBorderWidth(Frame_TitleArea(w));
	    }
	    else
	    {
		curh += XtHeight(Frame_TitleArea(w)) +
		    2 * XtBorderWidth(Frame_TitleArea(w));
	    }

	    if (desired->request_mode & CWWidth)
	    {
		curw = desired->width +
		    2 * FrameC_ChildHSpacing(Frame_TitleArea(w)) +
		    2 * XtBorderWidth(Frame_TitleArea(w));
	    }
	    else
	    {
		curw = XtWidth(Frame_TitleArea(w)) +
		    2 * FrameC_ChildHSpacing(Frame_TitleArea(w)) +
		    2 * XtBorderWidth(Frame_TitleArea(w));
	    }
	}
	else
	{
	    geo.request_mode = 0;

	    XtQueryGeometry(Frame_TitleArea(w), NULL, &geo);

	    curh += geo.height + 2 * geo.border_width - MGR_ShadowThickness(w);
	    curw = geo.width + 2 * geo.border_width +
		2 * FrameC_ChildHSpacing(Frame_TitleArea(w));
	}
    }

    /* This seems a little wacky but look at frame/test6 */
    if (Frame_WorkArea(w) /*&& XtIsManaged(Frame_WorkArea(w))*/)
    {
	if (instig == Frame_WorkArea(w))
	{
	    if (desired->request_mode & CWHeight)
	    {
		curh += desired->height + 
		    2 *XtBorderWidth(Frame_WorkArea(w));
	    }
	    else
	    {
		curh += XtHeight(Frame_WorkArea(w)) +
		    2 *XtBorderWidth(Frame_WorkArea(w));
	    }

	    if (desired->request_mode & CWWidth)
	    {
		curw = desired->width +
			      2 * XtBorderWidth(Frame_WorkArea(w));
	    }
	    else
	    {
		curw = _XmMax(curw, XtWidth(Frame_WorkArea(w)) +
			      2 * XtBorderWidth(Frame_WorkArea(w)));
	    }
	}
	else
	{
	    geo.request_mode = 0;

	    XtQueryGeometry(Frame_WorkArea(w), NULL, &geo);

	    curh += geo.height + 2 * geo.border_width;
	    curw = _XmMax(curw, geo.width + 2 * geo.border_width);
	}
    }

    curh += 2 * Frame_MarginHeight(w) + 2 * MGR_ShadowThickness(w);
    curw += 2 * Frame_MarginWidth(w) + 2 * MGR_ShadowThickness(w);

    if (!Frame_ProcessingConstraints(w))
    {
	*wd = XtWidth(w);
	*ht = XtHeight(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, instig,
			   "Not processing constraints %d %d\n", *wd, *ht));
    }
    else
    {
	*wd = curw;
	*ht = curh;
    }

    DEBUGOUT(_LtDebug2(__FILE__, w, instig,
		       "_XmFrameComputeSize (%s) => %d %d : %d %d\n",
		       _LtDebugWidgetGeometry2String(desired), curw, curh,
		       *wd, *ht));
}

static void
_XmFrameConfigureChildren(Widget w, Widget instig, XtWidgetGeometry *desired,
			  Dimension curw, Dimension curh, Boolean commit)
{
    int title_x = 0, title_y = 0;
    int workarea_x = 0, workarea_y = 0;

    DEBUGOUT(_LtDebug2(__FILE__, w, instig,
		       "_XmFrameConfigureChildren (Frame geo %d %d)\n",
		       curw, curh));

    workarea_y = MGR_ShadowThickness(w) + Frame_MarginHeight(w);

    if (Frame_TitleArea(w) && XtIsManaged(Frame_TitleArea(w)))
    {
	switch (FrameC_ChildVAlignment(Frame_TitleArea(w)))
	{
	case XmALIGNMENT_CENTER:
	    title_y = 0;
	    workarea_y = XtHeight(Frame_TitleArea(w)) + Frame_MarginHeight(w) /* +
				MGR_ShadowThickness(w) */;
	    break;

	case XmALIGNMENT_BASELINE_BOTTOM:
	    _XmWarning(w, "_XmFrameConfigureChildren doesn't implement"
		       "XmALIGNMENT_BASELINE_BOTTOM yet\n");
	    /* FIX ME - fill this out */
	    break;

	case XmALIGNMENT_BASELINE_TOP:
	    title_y = 0;
	    workarea_y = MGR_ShadowThickness(w) + Frame_MarginHeight(w);
	    break;

	case XmALIGNMENT_WIDGET_TOP:
	    title_y = 0;
	    workarea_y = XtHeight(Frame_TitleArea(w)) + MGR_ShadowThickness(w)
		+ Frame_MarginHeight(w);
	    break;

	case XmALIGNMENT_WIDGET_BOTTOM:
	    title_y = MGR_ShadowThickness(w) + Frame_MarginHeight(w);
	    workarea_y = MGR_ShadowThickness(w) + Frame_MarginHeight(w);
	    break;
	}

	workarea_y += 2 * XtBorderWidth(Frame_TitleArea(w));

	title_x = MGR_ShadowThickness(w) +
	    FrameC_ChildHSpacing(Frame_TitleArea(w));

	switch (FrameC_ChildHAlignment(Frame_TitleArea(w)))
	{
	case XmALIGNMENT_BEGINNING:
	    if (MGR_StringDirection(w) != XmSTRING_DIRECTION_L_TO_R)
	    {
		title_x = XtWidth(w) - XtWidth(Frame_TitleArea(w)) - title_x -
		    2 * XtBorderWidth(Frame_TitleArea(w));
	    }
	    break;

	case XmALIGNMENT_CENTER:
	    title_x = (curw >> 1) - (XtWidth(Frame_TitleArea(w)) >> 1);
	    break;

	case XmALIGNMENT_END:
	default:
	    if (MGR_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R)
	    {
		title_x = XtWidth(w) - XtWidth(Frame_TitleArea(w)) - title_x -
		    2 * XtBorderWidth(Frame_TitleArea(w));
	    }
	    break;
	}

	if (Frame_TitleArea(w) == instig)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Instig is title\n"));

#if 0
	    desired->request_mode = CWBorderWidth | CWWidth | CWHeight | CWX | CWY;
	    desired->x = title_x;
	    desired->y = title_y;
	    desired->width =
		_XmMin(XtWidth(Frame_TitleArea(w)),
		       curw - 2 * MGR_ShadowThickness(w)
		       - 2 * Frame_MarginWidth(w)
		       - 2 * FrameC_ChildHSpacing(Frame_TitleArea(w)));
	    desired->height = XtHeight(Frame_TitleArea(w));	/* HERE */
#else
	    /* rws 3 Jan 2000
	       frame/test9
	     */
	    desired->x = desired->request_mode & CWX ? desired->x : title_x;
	    desired->y = desired->request_mode & CWY ? desired->y : title_y;
	    desired->width =
		_XmMin(desired->request_mode & CWWidth ? desired->width : XtWidth(Frame_TitleArea(w)),
		       curw - 2 * MGR_ShadowThickness(w)
		       - 2 * Frame_MarginWidth(w)
		       - 2 * FrameC_ChildHSpacing(Frame_TitleArea(w)));
	    desired->height = desired->request_mode & CWHeight ? desired->height : XtHeight(Frame_TitleArea(w));	/* HERE */
	    desired->request_mode = CWBorderWidth | CWWidth | CWHeight | CWX | CWY;
#endif
	    desired->border_width = XtBorderWidth(Frame_TitleArea(w));
	}
	else if (commit)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Configure title\n"));

	    _XmConfigureObject(Frame_TitleArea(w),
			       title_x, title_y,
			       _XmMin(XtWidth(Frame_TitleArea(w)),
				      curw - 2 * MGR_ShadowThickness(w) -
				      2 * Frame_MarginWidth(w) -
				 2 * FrameC_ChildHSpacing(Frame_TitleArea(w)) -
				      2 * XtBorderWidth(Frame_TitleArea(w))),
			       XtHeight(Frame_TitleArea(w)),
			       XtBorderWidth(Frame_TitleArea(w)));
	}
    }

    if (Frame_WorkArea(w) && XtIsManaged(Frame_WorkArea(w)))
    {
	workarea_x = MGR_ShadowThickness(w) + Frame_MarginWidth(w);

	if (curw >= 2 * (MGR_ShadowThickness(w) + Frame_MarginWidth(w) +
			 XtBorderWidth(Frame_WorkArea(w))))
	{
	    curw -= 2 * (MGR_ShadowThickness(w) + Frame_MarginWidth(w) +
			 XtBorderWidth(Frame_WorkArea(w)));
	}
	else
	{
	    curw = 0;
	}

	if (curh >= workarea_y + Frame_MarginHeight(w) +
			MGR_ShadowThickness(w) +
			2 * XtBorderWidth(Frame_WorkArea(w)))
	{
	    curh -= workarea_y + Frame_MarginHeight(w) +
			MGR_ShadowThickness(w) +
			2 * XtBorderWidth(Frame_WorkArea(w));
	}
	else
	{
	    curh = 0;
	}

	if (Frame_WorkArea(w) == instig)
	{
	    desired->request_mode = CWBorderWidth|CWWidth|CWHeight|CWX|CWY;
	    desired->x = workarea_x;
	    desired->y = workarea_y;
	    desired->width = curw;
	    desired->height = curh;
	    desired->border_width = XtBorderWidth(Frame_WorkArea(w));
	}
	else if (commit)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Configure work area\n"));

	    _XmConfigureObject(Frame_WorkArea(w),
			       workarea_x, workarea_y,
			       curw, curh,
			       XtBorderWidth(Frame_WorkArea(w)));
	}
    }
}

Widget
XmCreateFrame(Widget parent,
	      char *name,
	      Arg *arglist,
	      Cardinal argCount)
{
    return XtCreateWidget(name,
				 xmFrameWidgetClass,
				 parent,
				 arglist,
				 argCount);
}
