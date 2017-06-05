/**
 *
 * $Id: DrawingArea.c,v 1.3 2005/04/04 07:22:05 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2001, 2004, 2005 LessTif Development Team 
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

static const char rcsid[] = "$Id: DrawingArea.c,v 1.3 2005/04/04 07:22:05 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/DrawingAP.h>
#include <Xm/TransltnsP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/RepType.h>
#include <XmI/MacrosI.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void resize(Widget w);

static void expose(Widget w, XEvent *event, Region region);

static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);

static void change_managed(Widget w);

static void PreferredSize(Widget w, Widget instig, XtWidgetGeometry *ir,
			 Dimension *wid, Dimension *hei);

static XmNavigability widget_navigable(Widget w);
static Boolean ConstraintSetValues(Widget curr, Widget req, Widget new_w,
	ArgList args, Cardinal *num_args);

/*
 * Resources for the Drawing Area class
 */
#define Offset(field) XtOffsetOf(XmDrawingAreaRec, drawing_area.field)
static XtResource resources[] =
{
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNresizeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(resize_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNexposeCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(expose_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNinputCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(input_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNresizePolicy, XmCResizePolicy, XmRResizePolicy,
	sizeof(unsigned char), Offset(resize_policy),
	XmRImmediate, (XtPointer)XmRESIZE_ANY
    },
    {
	XmNconvertCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(convert_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdestinationCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(destination_callback),
	XmRImmediate, (XtPointer)NULL
    }
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

void _XmDrawingAreaInput(Widget w, XEvent *event, String *parems, Cardinal *num_params);

static XtActionsRec actions[] =
{
    {"DrawingAreaInput", _XmDrawingAreaInput},
};

/* *INDENT-OFF* */
static XmBaseClassExtRec _XmDrawingACoreClassExtRec = {
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
    /* widget_navigable          */ widget_navigable,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};

XmDrawingAreaClassRec xmDrawingAreaClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmDrawingArea",
	/* widget_size           */ sizeof(XmDrawingAreaRec),
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
	/* compress_exposure     */ XtExposeNoCompress,
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
	/* tm_table              */ _XmDrawingA_defaultTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmDrawingACoreClassExtRec
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
	/* subresources      */ NULL,  
        /* subresource_count */ 0,     
        /* constraint_size   */ 0,     
        /* initialize        */ NULL,
        /* destroy           */ NULL,  
        /* set_values        */ ConstraintSetValues,  
        /* extension         */ NULL,  
    },
    /* XmManager class part */
    {
	/* translations                 */ _XmDrawingA_traversalTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)NULL
    },
    /* XmDrawing Area part */
    {
	/* extension */ NULL,
    },
};
/* *INDENT-ON* */


WidgetClass xmDrawingAreaWidgetClass = (WidgetClass)&xmDrawingAreaClassRec;

static void
class_initialize(void)
{
    _XmDrawingACoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmDrawingAreaWidgetClass daclass = (XmDrawingAreaWidgetClass)widget_class;
    CompositeClassExtension ext, *extptr;

    extptr = (CompositeClassExtension *)_XmGetClassExtensionPtr(
		    (XmGenericClassExt *)&(daclass->composite_class.extension),
								   NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = daclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    daclass->composite_class.extension = (XtPointer)ext;
	}
    }
    _XmFastSubclassInit(widget_class, XmDRAWING_AREA_BIT);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "XmDrawingArea initialize: %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    if (DA_ResizePolicy(new_w) != XmRESIZE_SWINDOW &&
	!XmRepTypeValidValue(XmRepTypeGetId(XmRResizePolicy),
			     DA_ResizePolicy(new_w),
			     new_w))
    {
	DA_ResizePolicy(new_w) = XmRESIZE_ANY;
    }

#if 0
    if (XtWidth(request) == 0)
    {
	XtWidth(new_w) = /* 2 * */ DA_MarginWidth(new_w);
    }

    if (XtHeight(request) == 0)
    {
	XtHeight(new_w) = /* 2 * */ DA_MarginHeight(new_w);
    }
#endif

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "DrawingArea initialize: w/h %d %d\n",
		      XtWidth(new_w), XtHeight(new_w)));
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean need_refresh = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "XmDrawingArea SetValues: %i args\n"
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

#define NE(x)	(x(old) != x(new_w))

    if (NE(DA_ResizePolicy))
    {
	if (!XmRepTypeValidValue(XmRepTypeGetId(XmRResizePolicy),
			     DA_ResizePolicy(new_w),
			     new_w))
	{
	    DA_ResizePolicy(new_w) = DA_ResizePolicy(old);
	}
	else
	{
	    need_refresh = True;
	}
    }
    
    if (NE(DA_MarginHeight) ||
	NE(DA_MarginWidth))
    {
	need_refresh = True;
    }

    return need_refresh;
}

static void
expose(Widget w, XEvent *event, Region region)
{
    XmDrawingAreaCallbackStruct cb;

    DEBUGOUT(_LtDebug(__FILE__, w, "DrawingArea expose: w/h %d %d\n", XtWidth(w), XtHeight(w)));

    cb.reason = XmCR_EXPOSE;
    cb.event = event;
    cb.window = XtWindow(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "DA_ExposeCallback\n"));

    XtCallCallbackList(w, DA_ExposeCallback(w), (XtPointer)&cb);

    _XmRedisplayGadgets(w, event, region);
}

static void
resize(Widget w)
{
    XmDrawingAreaCallbackStruct cb;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmDrawingArea Resize() %dx%d\n", XtWidth(w), XtHeight(w)));

    cb.reason = XmCR_RESIZE;
    cb.event = NULL;
    cb.window = XtWindow(w);

    XtCallCallbackList(w, DA_ResizeCallback(w), (XtPointer)&cb);
}

static void
PreferredSize(Widget w, Widget instigator, XtWidgetGeometry *ir,
	     Dimension *wid, Dimension *hei)
{
    Cardinal c;
    int cnt;

    *wid = 0;
    *hei = 0;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmDrawingArea PreferredSize: %dx%d inst %s %s\n",
		      XtWidth(w), XtHeight(w),
		      instigator ? XtName(instigator) : "NULL",
		      _LtDebugWidgetGeometry2String(ir)
		      ));

    if (DA_ResizePolicy(w) == XmRESIZE_NONE /* || _XmGeoCount_kids((CompositeRec *)w) == 0 */)
    {
	*wid = XtWidth(w);
	*hei = XtHeight(w);

	return;
    }

    for (c = 0, cnt = 0; c < MGR_NumChildren(w); c++)
    {
	Widget child = MGR_Children(w)[c];
	XtWidgetGeometry child_geometry;
	Dimension tmp;

	if (!XtIsManaged(child))
	{
	    continue;
	}

	cnt++;

	/* we assume that the routines calling this one fill in the
	   width and height of the child responsible for the resize
	   request with their preferred size. */
	if (instigator == child)
	{
	    child_geometry = *ir;
	}
	else
	{
	    child_geometry.x = XtX(child);
	    child_geometry.y = XtY(child);
	    child_geometry.width = XtWidth(child);
	    child_geometry.height = XtHeight(child);
		/* T. Straumann: don't forget the border width */
	    child_geometry.border_width = XtBorderWidth(child);
	}

	DEBUGOUT(_LtDebug2(__FILE__, w, child,
			   "Child wants %d %d\n",
			   child_geometry.width, child_geometry.height));

	/* T. Straumann: added border width support */
	if ( (tmp = child_geometry.x + child_geometry.width + 2 * child_geometry.border_width) > *wid)
	{
	    *wid = tmp;
	}

	if ( (tmp = child_geometry.y + child_geometry.height + 2 * child_geometry.border_width) > *hei)
	{
	    *hei = tmp;
	}
    }


    /*
     * What if we were to return 0 here ? That's not a valid width or height.
     * Let's see what happens if we return our current size instead in those
     * cases.
     */
     /* rws 7 Oct 1998
        drawingarea/test7
      */
    if (cnt == 0)
    {
	if (XtWidth(w) == 0)
	{
	    *wid = DA_MarginWidth(w);
	}
	else
	{
	    *wid = XtWidth(w) < DA_MarginWidth(w) ? DA_MarginWidth(w) : XtWidth(w);
	}
	if (XtHeight(w) == 0)
	{
	    *hei = DA_MarginHeight(w);
	}
	else
	{
	    *hei = XtHeight(w) < DA_MarginHeight(w) ? DA_MarginHeight(w) : XtHeight(w);
	}

	return;
    }

    /*
     * we assume that the X/Y coordinates of all the children have the
     * drawing area's margin width/height added to them.  But now we
     * increment the total width and height of the drawing area to
     * include the margins on the other side.
     */
    if (DA_ResizePolicy(w) != XmRESIZE_SWINDOW)
    {
	*wid += DA_MarginWidth(w);
	*hei += DA_MarginHeight(w);
    }

    if (DA_ResizePolicy(w) == XmRESIZE_ANY ||
	DA_ResizePolicy(w) == XmRESIZE_SWINDOW)
    {
	return;
    }

    if (*wid < XtWidth(w))
    {
	*wid = XtWidth(w);
    }
    if (*hei < XtHeight(w))
    {
	*hei = XtHeight(w);
    }
}

static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    Dimension wid, hei;
    XtWidgetGeometry geo;

#define	Wants(x)	((geo.request_mode & x) == x)

    DEBUGOUT(_LtDebug(__FILE__, w, "XmDrawingArea QueryGeometry\n",
    	_LtDebugWidgetGeometry2String(proposed) ));

    geo = *proposed;

#if 0
    answer->request_mode = 0;

    PreferredSize(w, NULL, NULL, &wid, &hei);

    answer->request_mode = CWWidth|CWHeight;
    answer->width = wid;
    answer->height = hei;

    if (Wants(CWHeight) && (geo.height != answer->height))
    {
	return XtGeometryNo;
    }
    if (Wants(CWWidth) && (geo.width != answer->width))
    {
	return XtGeometryNo;
    }

    return XtGeometryYes;
#else
    PreferredSize(w, NULL, NULL, &wid, &hei);

    answer->width = wid;
    answer->height = hei;

    return _XmGMReplyToQueryGeometry(w, &geo, answer);
#endif
}

static XtGeometryResult
geometry_manager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    Widget dw = XtParent(w), sw = XtParent(dw);
    XtGeometryResult result;
    Dimension wid = 0, hei = 0;
    XtWidgetGeometry req, preq;

    DEBUGOUT(_LtDebug2(__FILE__, dw, w, "XmDrawingArea GeometryManager(%s)\n",
		     (DA_ResizePolicy(dw) == XmRESIZE_NONE) ? "XmRESIZE_NONE" :
		     (DA_ResizePolicy(dw) == XmRESIZE_GROW) ? "XmRESIZE_GROW" :
		     (DA_ResizePolicy(dw) == XmRESIZE_ANY) ? "XmRESIZE_ANY" :
      (DA_ResizePolicy(dw) == XmRESIZE_SWINDOW) ? "XmRESIZE_SWINDOW" : "???"));
    DEBUGOUT(_LtDebug2("ITEMBIN", dw, w, "geometry_manager(%s)\n",
		     (DA_ResizePolicy(dw) == XmRESIZE_NONE) ? "XmRESIZE_NONE" :
		     (DA_ResizePolicy(dw) == XmRESIZE_GROW) ? "XmRESIZE_GROW" :
		     (DA_ResizePolicy(dw) == XmRESIZE_ANY) ? "XmRESIZE_ANY" :
      (DA_ResizePolicy(dw) == XmRESIZE_SWINDOW) ? "XmRESIZE_SWINDOW" : "???"));

    if (XtIsSubclass(sw, xmScrolledWindowWidgetClass) &&
	SW_ScrollPolicy(sw) == XmAUTOMATIC &&
	(Widget)SW_ClipWindow(sw) == dw)
    {
	DEBUGOUT(_LtDebug2(__FILE__, dw, w,
			   "XmDrawingArea GeometryManager: in ScrolledWindow (%s) => YES\n",
			   _LtDebugWidgetGeometry2String(request)));

	/* T. Straumann: the ScrolledWindow's code knows that it has
	 *				 to manage its _grandchild_ (the workWindow).
	 *				 This kind of magic is not exactly what I thought
	 *				 encapsulation was about...
	 */
	XtMakeGeometryRequest(dw, request, reply);
	XtWidth(w) = request->request_mode & CWWidth ? request->width : XtWidth(w);
	XtHeight(w) = request->request_mode & CWHeight ? request->height : XtHeight(w);
	return XtGeometryYes;
    }

    /* Normal case */
    req = *request;
    *reply = req;

    if (!(req.request_mode & CWX))
    {
	req.x = XtX(w);
    }
    if (!(req.request_mode & CWY))
    {
	req.y = XtY(w);
    }
    if (!(req.request_mode & CWWidth))
    {
	req.width = XtWidth(w);
    }
    if (!(req.request_mode & CWHeight))
    {
	req.height = XtHeight(w);
    }
    if (!(req.request_mode & CWBorderWidth))
    {
	req.border_width = XtBorderWidth(w);
    }

    /*
     * Danny 6/3/1999
     * Special case: (See bug 69)
     *	- we get X, Y but no other geometry requests
     *  - X+Width <= DAWidth
     *  - Y+Height <= DAHeight
     * -> then just say yes.
     */
    /* rws 25 May 1999
       drawingarea/test9 shows this is wrong.
    if ((req.request_mode & ~(CWX|CWY)) == 0) {
	if (req.x + req.width <= XtWidth(dw) &&
			req.y + req.height <= XtHeight(dw)) {
		return XtGeometryYes;
	}
    }
    */

    /* Calculate needed size for DrawingArea widget */
    PreferredSize(dw, w, &req, &wid, &hei);
    DEBUGOUT(_LtDebug2("ITEMBIN", dw, w, "request (%s) need %ix%i is %ix%i\n",
    	_LtDebugWidgetGeometry2String(&req), wid, hei,
    	XtWidth(dw), XtHeight(dw)
    	));

    switch (DA_ResizePolicy(dw))
    {
    case XmRESIZE_NONE:
	if ((wid > XtWidth(dw)) || (hei > XtHeight(dw)))
	{
	    DEBUGOUT(_LtDebug(__FILE__, dw, "Return XtGeometryNo 1\n"));

	    return XtGeometryNo;
	}
	else
	{
	    /* the resize request doesn't require us to grow, accept it */
	    DEBUGOUT(_LtDebug(__FILE__, dw, "Return XtGeometryYes 1\n"));

	    XtX(w) = req.x;
	    XtY(w) = req.y;
	    XtWidth(w) = req.width;
	    XtHeight(w) = req.height;
	    XtBorderWidth(w) = req.border_width;

	    return XtGeometryYes;
	}
	break;

    case XmRESIZE_GROW:
	if ((wid > XtWidth(dw)) || (hei > XtHeight(dw)))
	{
	    /* Attempt to resize DrawingArea widget */
	    preq.request_mode = (CWWidth | CWHeight);
	    preq.width = wid;
	    preq.height = hei;
	    result = _XmMakeGeometryRequest(dw, &preq);

	    if (result == XtGeometryYes)
	    {
		DEBUGOUT(_LtDebug(__FILE__, dw, "Return XtGeometryYes 2\n"));

		XtX(w) = req.x;
		XtY(w) = req.y;
		XtWidth(w) = req.width;
		XtHeight(w) = req.height;
		XtBorderWidth(w) = req.border_width;

		return XtGeometryYes;
	    }
	    else
	    {
		DEBUGOUT(_LtDebug(__FILE__, dw, "Return XtGeometryNo 2\n"));

		return XtGeometryNo;
	    }
	}
	else
	{
	    /* the resize request doesn't require us to grow, accept it */
	    DEBUGOUT(_LtDebug(__FILE__, dw, "Return XtGeometryYes 3\n"));

	    XtX(w) = req.x;
	    XtY(w) = req.y;
	    XtWidth(w) = req.width;
	    XtHeight(w) = req.height;
	    XtBorderWidth(w) = req.border_width;

	    return XtGeometryYes;
	}
	break;

    case XmRESIZE_ANY:
    case XmRESIZE_SWINDOW:
	if ((wid != XtWidth(dw)) || (hei != XtHeight(dw)))
	{

	    preq.request_mode = (CWWidth | CWHeight);
	    preq.width = wid;
	    preq.height = hei;
	    result = _XmMakeGeometryRequest(dw, &preq);
	    DEBUGOUT(_LtDebug2("ITEMBIN", dw, w, "Parent says %s (%s)\n",
	    	_LtDebugGeometryResult2String(result),
	    	_LtDebugWidgetGeometry2String(&preq)
	    	));

	    if (result == XtGeometryYes)
	    {
		DEBUGOUT(_LtDebug(__FILE__, dw, "Return XtGeometryYes 4\n"));

		XtX(w) = req.x;
		XtY(w) = req.y;
		XtWidth(w) = req.width;
		XtHeight(w) = req.height;
		XtBorderWidth(w) = req.border_width;

		return XtGeometryYes;
	    }
	    else
	    {
		if ((req.request_mode & CWX ? wid < XtWidth(dw) : True) && 
		    (req.request_mode & CWY ? hei < XtHeight(dw) : True))
		{
		    XtX(w) = req.request_mode & CWX ? req.x : XtX(w);
		    XtY(w) = req.request_mode & CWY ? req.y : XtY(w);
		    return XtGeometryYes;
		}
		DEBUGOUT(_LtDebug(__FILE__, dw, "Return XtGeometryNo 3\n"));

		return XtGeometryYes;
	    }
	}
	else
	{
	    /* the resize request doesn't require us to alter our geometry,
	     * accept it */

	    DEBUGOUT(_LtDebug(__FILE__, dw, "Return XtGeometryYes 5\n"));

	    XtX(w) = req.x;
	    XtY(w) = req.y;
	    XtWidth(w) = req.width;
	    XtHeight(w) = req.height;
	    XtBorderWidth(w) = req.border_width;

	    return XtGeometryYes;
	}
	break;
    }

    DEBUGOUT(_LtDebug(__FILE__, dw, "Return XtGeometryNo 4\n"));

    return XtGeometryNo;
}

static void
change_managed(Widget w)
{
	Dimension wid, hei;
	Dimension reply_wid, reply_hei;
	XtGeometryResult result;
	XtWidgetGeometry req;

	DEBUGOUT(_LtDebug(__FILE__, w, "DA change_managed(%s)\n",
				_LtDebugResizePolicy2String(DA_ResizePolicy(w))));
	DEBUGOUT(_LtDebugPrintManagedChildren(__FILE__, w, "Child"));

	if (DA_ResizePolicy(w) == XmRESIZE_SWINDOW) {
		XtWidgetProc manager;

		/*
		 * Calling XmScrolledWindow's change_managed() method
		 * instead of handling this ourselves.
		 */
		manager = ((CompositeWidgetClass) (XtClass(XtParent(w))))
			->composite_class.change_managed;
		(*manager)(XtParent(w));
		_XmNavigChangeManaged(w) ;
		return;
	}

	if (DA_ResizePolicy(w) != XmRESIZE_NONE) {
		_XmGMEnforceMargin(w, DA_MarginWidth(w), DA_MarginHeight(w), False);

		PreferredSize(w, NULL, NULL, &wid, &hei);
		wid = XtIsRealized(w) ? wid : XtWidth(w) == 0 ? wid : XtWidth(w);
		hei = XtIsRealized(w) ? hei : XtHeight(w) == 0 ? hei : XtHeight(w);
		if (wid > XtWidth(w) || hei > XtHeight(w)) {

			req.request_mode = (CWWidth | CWHeight);
			req.width = wid;
			req.height = hei;
			DEBUGOUT(_LtDebug(__FILE__, w, "change_managed() - request %s\n",
			_LtDebugWidgetGeometry2String(&req)));
			result = _XmMakeGeometryRequest(w, &req);
			DEBUGOUT(_LtDebug(__FILE__, w, "change_managed() - got %s\n",
			_LtDebugGeometryResult2String(result)));
			reply_wid = req.width;
			reply_hei = req.height;

		} else if (DA_ResizePolicy(w) == XmRESIZE_ANY ||
				DA_ResizePolicy(w) == XmRESIZE_SWINDOW) {
			/* else we're not getting any bigger... */
			req.request_mode = (CWWidth | CWHeight);
			req.width = wid;
			req.height = hei;
			result = _XmMakeGeometryRequest(w, &req);
			reply_wid = req.width;
			reply_hei = req.height;
		}
	}
}

void
_XmDrawingAreaInput(Widget w,
		    XEvent *event,
		    String *parems,
		    Cardinal *num_params)
{
    XmDrawingAreaCallbackStruct cb;

    cb.reason = XmCR_INPUT;
    cb.event = event;
    cb.window = XtWindow(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "DA_InputCallback\n"));

    XtCallCallbackList(w, DA_InputCallback(w), (XtPointer)&cb);
}

Widget
XmCreateDrawingArea(Widget parent, char *name,
		    Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name, xmDrawingAreaWidgetClass, parent,
			  arglist, argcount);
}

static XmNavigability
widget_navigable(Widget w)
{
    if (XtSensitive(w) && MGR_TraversalOn(w))
    {
	if (MGR_NavigationType(w) == XmSTICKY_TAB_GROUP ||
	    MGR_NavigationType(w) == XmEXCLUSIVE_TAB_GROUP ||
	    (MGR_NavigationType(w) == XmTAB_GROUP && !_XmShellIsExclusive(w)))
	{
#if 0
	    DEBUGOUT(_LtDebug(__FILE__, w,
			  "WidgetNavigable => XmDESCENDANTS_TAB_NAVIGABLE\n"));

	    /* Need to look at all children, too */
	    if (MGR_NumChildren(w) == 0)
	    {
		return XmTAB_NAVIGABLE;
	    }

	    return XmDESCENDANTS_TAB_NAVIGABLE;
#else
	    {
	    int i;

	    	for (i = 0; i < MGR_NumChildren(w); i++)
	    	{
		    if ((XmIsManager(MGR_Children(w)[i]) && MGR_TraversalOn(MGR_Children(w)[i])) ||
		        (XmIsPrimitive(MGR_Children(w)[i]) && Prim_TraversalOn(MGR_Children(w)[i])))
		    {
			return XmDESCENDANTS_TAB_NAVIGABLE;
		    }
	    	}
		return XmTAB_NAVIGABLE;
	    }
#endif
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "WidgetNavigable => XmNOT_NAVIGABLE\n"));

    return XmNOT_NAVIGABLE;
}

static Boolean
ConstraintSetValues(Widget curr, Widget req, Widget new_w,
		ArgList args, Cardinal *num_args)
{
	/*
	XtWidgetGeometry preq;
	XtGeometryResult result;
	Widget		dw = XtParent(new_w);
	*/

	if (XtX(curr) != XtX(new_w) || XtY(curr) != XtY(new_w)) {
		_XmMoveObject(new_w, XtX(new_w), XtY(new_w));

		return True;
	}
	return False;
}

