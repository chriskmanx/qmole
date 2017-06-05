/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/CascadeBG.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/CascadeBG.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <stdarg.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/CacheP.h>
#include <Xm/CascadeB.h>	/* For XmCascadeButtonHighlight */
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuShellP.h>
#include <Xm/RowColumnP.h>
#include <Xm/ScreenP.h>
#include <Xm/MenuUtilP.h>

#include <XmI/DebugUtil.h>

#define CASCADE_SPACING	4

/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void resize(Widget w);

static void destroy(Widget w);

static void expose(Widget w, XEvent *event, Region region);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void input_dispatch(Widget gadget, XEvent *event, Mask event_mask);

static Boolean visual_change(Widget w, Widget parent, Widget n);

static void secondary_object_create(Widget request, Widget new_w,
				    ArgList args, Cardinal *num_args);

static void initialize_posthook(Widget request, Widget new_w,
				ArgList args, Cardinal *num_args);

static Boolean set_values_prehook(Widget old, Widget request, Widget new_w,
				  ArgList args, Cardinal *num_args);

static Boolean set_values_posthook(Widget old, Widget request, Widget new_w,
				   ArgList args, Cardinal *num_args);

static void get_values_prehook(Widget new_w,
			       ArgList args, Cardinal *num_args);

static void get_values_posthook(Widget new_w,
				ArgList args, Cardinal *num_args);

static Cardinal get_sec_res_data(WidgetClass wc,
				 XmSecondaryResourceData **data);

extern int _XmCascadeBCacheCompare(XtPointer A, XtPointer B);

static void DelayedArm(Widget w, XEvent *event,
		       String *params, Cardinal *num_params);

static void CheckDisarm(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void ArmAndActivate(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);

static void MenuProcEntry(int proc, Widget rc,...);

static void CascadePopupHandler(XtPointer clientData, XtIntervalId *id);

/*
 * cache resources
 */
#define Offset(field) XtOffsetOf(XmCascadeButtonGCacheObjRec, \
				 cascade_button_cache.field)
static XtResource cache_resources[] =
{
    {
	XmNcascadePixmap, XmCPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(cascade_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNmappingDelay, XmCMappingDelay, XmRInt,
	sizeof(int), Offset(map_delay),
	XmRImmediate, (XtPointer)180
    }
};

XmCascadeButtonGCacheObjClassRec xmCascadeButtonGCacheObjClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGCacheObjClassRec,
	/* class_name            */ "XmCascadeButtonGCacheObjClass",
	/* widget_size           */ sizeof(XmCascadeButtonGCacheObjRec),
	/* class_initialize      */ NULL,
	/* class_part_initialize */ NULL,
	/* class_inited          */ False,
	/* initialize            */ NULL,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ cache_resources,
	/* num_resources         */ XtNumber(cache_resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ 0,
	/* compress_exposure     */ 0,
	/* compress_enterleave   */ 0,
	/* visible_interest      */ 0,
	/* destroy               */ NULL,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ NULL,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersionDontCheck,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* XmExtObject part */
    {
	/* syn_resources      */ NULL,
	/* num_syn_resources  */ 0,
	/* extension          */ NULL
    },
    /* LabelGCacheObj part */
    {
	/* foo                */ 0
    },
    /* CascadeButtonGCacheObj part */
    {
	/* foo                */ 0
    }
};

/*
 * Resources for the cascadebutton class
 */
#undef Offset
#define Offset(field) XtOffsetOf(XmCascadeButtonGadgetRec, cascade_button.field)
#define GOffset(field) XtOffsetOf(XmCascadeButtonGadgetRec, gadget.field)
static XtResource resources[] = {
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
    /* resources we override from XmLabelGadget/XmGadget */
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), GOffset(shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), GOffset(traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), GOffset(highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
};

static XmBaseClassExtRec _XmCascadeBGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ XmInheritInitializePrehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmCascadeButtonGCacheObjClassRec,
    /* secondary_object_create   */ secondary_object_create,
    /* get_secondary_resources   */ get_sec_res_data,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ get_values_prehook,
    /* get_values_posthook       */ get_values_posthook,
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

static XmCacheClassPart cache_part = {
    /* cache head part */
    {
	/* next         */ NULL,
	/* prev         */ NULL,
	/* ref_count    */ 0
    },
    _XmCacheCopy,
    _XmCacheDelete,
    _XmCascadeBCacheCompare
};

int _XmArrowPixmapCacheCompare(XtPointer A, XtPointer B);

static XmCacheClassPart arrow_pixmap_cache = {
    /* cache head part */
    {
	/* next         */ NULL,
	/* prev         */ NULL,
	/* ref_count    */ 0
    },
    _XmCacheCopy,
    _XmArrowPixmapCacheDelete,
    _XmArrowPixmapCacheCompare
};

static XmGadgetClassExtRec _XmCascadeBGGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ XmInheritBaselineProc,
    /* display_rect_proc         */ XmInheritDisplayRectProc,
#if XmVERSION >= 2
    /* margins_proc              */ XmInheritMarginsProc,
#endif
};

XmCascadeButtonGadgetClassRec xmCascadeButtonGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGadgetClassRec,
	/* class_name            */ "XmCascadeButtonGadget",
	/* widget_size           */ sizeof(XmCascadeButtonGadgetRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL, /* FIX ME */
	/* actions               */ NULL,
	/* num_actions           */ 0,
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
	/* extension             */ (XtPointer)&_XmCascadeBGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight, /* FIX ME */
	/* border_unhighlight */ XmInheritBorderUnhighlight, /* FIX ME */
	/* arm_and_activate   */ ArmAndActivate,
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ visual_change,
	/* syn_resources      */ NULL,
	/* num_syn_resources  */ 0,
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&_XmCascadeBGGadgetClassExtRec
    },
    /* XmLabelGadget part */
    {
	/* setOverrideCallback */ XmInheritSetOverrideCallback,
	/* menuProcs           */ XmInheritMenuProc,
	/* extension           */ NULL
    },
    /* XmCascadeButtonGadget part */
    {
	/* extension */ NULL
    },
};


WidgetClass xmCascadeButtonGadgetClass =
				(WidgetClass)&xmCascadeButtonGadgetClassRec;

/********************************* CACHE PART *******************************/
static void
secondary_object_create(Widget request, Widget new_w,
			ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XtPointer nsec, rsec;
    XmWidgetExtData ed;
    Cardinal size;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "CascadeButtonGCacheRec %s being initialized.\n",
		      XtName(new_w)));

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);

    size = (*bce)->secondaryObjectClass->core_class.widget_size;
    nsec = _XmExtObjAlloc(size);
    rsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent(new_w);
    ((XmExtRec *)nsec)->object.xrm_name = new_w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    XtGetSubresources(new_w, nsec, NULL, NULL,
		      (*bce)->secondaryObjectClass->core_class.resources,
		      (*bce)->secondaryObjectClass->core_class.num_resources,
		      args, *num_args);

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;
    ed->reqWidget = (Widget)rsec;

    memcpy(rsec, nsec, size);

    ((XmExtRec *)rsec)->object.self = (Widget)rsec;

    _XmPushWidgetExtData(new_w, ed, XmCACHE_EXTENSION);

    LabG_Cache(new_w) = &(((XmLabelGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmLabelGCacheObject)rsec)->label_cache);

    CBG_Cache(new_w) =
	&(((XmCascadeButtonGCacheObject)nsec)->cascade_button_cache);
    CBG_Cache(request) =
	&(((XmCascadeButtonGCacheObject)rsec)->cascade_button_cache);
}

int
_XmCascadeBCacheCompare(XtPointer A, XtPointer B)
{
    return !memcmp(((XmCascadeButtonGCacheObjPart *)A),
		 ((XmCascadeButtonGCacheObjPart *)B),
		 sizeof(XmCascadeButtonGCacheObjPart));
}

int
_XmArrowPixmapCacheCompare(XtPointer A, XtPointer B)
{
    /* This will get confused by arrow direction and shadow thickness,
     * which aren't defined in the XmArrowPixmap structure.
     * But this is how M*tif defines it, so it's too late to do it right.
     * This will seldom cause confusion, as arrow direction (based on string
     * direction) is usually consistent throughout an application, and
     * different shadow thicknesses imply different pixmap sizes (at least
     * for the same size font).
     */
    return !memcmp(A, B, XtOffsetOf(XmArrowPixmap, pixmap));
}

void
_XmArrowPixmapCacheDelete(XtPointer data)
{
    XmGadgetCachePtr list;

    /* The pixmap alone isn't sufficient - the display should be passed.
     * But this is how M*tif defines it...
     */
    if ((list = ClassCacheHead(&arrow_pixmap_cache).next))
	while (list != &ClassCacheHead(&arrow_pixmap_cache))
	{
	    if (((XmArrowPixmap *)CacheDataPtr(list))->pixmap == (Pixmap)data)
	    {
		if (!--list->ref_count)
		{
		    _XmFreeScratchPixmap((XmScreen)XmGetXmScreen(
			((XmArrowPixmap *)CacheDataPtr(list))->screen),
			(Pixmap)data);
		    list->prev->next = list->next;
		    list->next->prev = list->prev;
		    XtFree((char *)list);
		}
		return;
	    }
	    list = list->next;
	}

    _XmWarning(NULL, "_XmArrowPixmapCacheDelete: pixmap not in cache");
}

static Dimension
default_font_height(XmFontList fl)
{
	XFontStruct *font;
	Dimension	fh;

#ifdef	USE_XFT
	if (fl->renditions[0]->type == XmFONT_IS_XFT) {
		fh = fl->renditions[0]->xft_font->height;
		DEBUGOUT(_LtDebug(__FILE__, NULL, "DefaultFontHeight -> %d\n", fh));
		return fh;
	}
#endif
	fh = _XmFontListGetDefaultFont(fl, &font)
		? font->ascent + font->descent
		: 0;
	DEBUGOUT(_LtDebug(__FILE__, NULL, "DefaultFontHeight -> %d\n", fh));
	return fh;
}

/* Assumes that the existing pixmaps are freed */
void
_XmCreateArrowPixmaps(Widget w)
{
    Pixel ts, bs;
    GC tgc, bgc, fgc;
    XmFontList fl;
    XmScreen sw;
    XmArrowPixmap *cp, *acp;
    Dimension st, th;
    unsigned char sd;
    XGCValues values;
    XmArrowPixmap cpart;

    /* Get info from widget or gadget */
    if (XmIsGadget(w))
    {
	ts = XmParentTopShadowColor(w);
	bs = XmParentBottomShadowColor(w);
	values.foreground = XmParentBackground(w);
	tgc = XmParentTopShadowGC(w);
	bgc = XmParentBottomShadowGC(w);
	st = LabG_Shadow(w);
	sd = LabG_StringDirection(w);
	fl = LabG_Font(w);
    }
    else
    {
	ts = Prim_TopShadowColor(w);
	bs = Prim_BottomShadowColor(w);
	values.foreground = CoreBackground(w);
	tgc = Prim_TopShadowGC(w);
	bgc = Prim_BottomShadowGC(w);
	st = Lab_Shadow(w);
	sd = Lab_StringDirection(w);
	fl = Lab_Font(w);
    }

    /* Fill in the cache part template */
    if (!(th = (default_font_height(fl) << 1) / 3))
	th = 1;
    cpart.height = cpart.width = th + (st << 1);
    cpart.foreground_color = values.foreground;
    cpart.display = XtDisplay(w);
    cpart.screen = XtScreen(w);
    cpart.depth = DefaultDepthOfScreen(cpart.screen);
    cpart.pixmap = XmUNSPECIFIED_PIXMAP;

    /* Get cache records for the normal and armed pixmaps */
    cpart.top_shadow_color = ts;
    cpart.bottom_shadow_color = bs;
    cp = (XmArrowPixmap *)
	_XmCachePart(&arrow_pixmap_cache, (XtPointer)&cpart, sizeof cpart);

    cpart.top_shadow_color = bs;
    cpart.bottom_shadow_color = ts;
    acp = (XmArrowPixmap *)
	_XmCachePart(&arrow_pixmap_cache, (XtPointer)&cpart, sizeof cpart);

    /* If the cached pixmaps don't exist, create them */
    if (cp->pixmap == XmUNSPECIFIED_PIXMAP
	|| acp->pixmap == XmUNSPECIFIED_PIXMAP)
    {
	fgc = XtGetGC(w, GCForeground, &values);
	sw = (XmScreen)XmGetXmScreen(cpart.screen);

	if (cp->pixmap == XmUNSPECIFIED_PIXMAP)
	{
	    cp->pixmap = _XmAllocScratchPixmap(sw, cpart.depth,
					       cpart.width, cpart.height);
	    XFillRectangle(cpart.display, cp->pixmap, fgc, 0, 0,
			   cpart.width, cpart.height);

	    _XmDrawArrow(cpart.display, cp->pixmap, tgc, bgc, fgc,
			 st - 1, st - 1, th + 2, th + 2, st,
			 sd == XmSTRING_DIRECTION_L_TO_R
			 ? XmARROW_RIGHT : XmARROW_LEFT);
	}

	if (acp->pixmap == XmUNSPECIFIED_PIXMAP)
	{
	    acp->pixmap = _XmAllocScratchPixmap(sw, cpart.depth,
					       cpart.width, cpart.height);
	    XFillRectangle(cpart.display, acp->pixmap, fgc, 0, 0,
			   cpart.width, cpart.height);

	    _XmDrawArrow(cpart.display, acp->pixmap, bgc, tgc, fgc,
			 st - 1, st - 1, th + 2, th + 2, st,
			 sd == XmSTRING_DIRECTION_L_TO_R
			 ? XmARROW_RIGHT : XmARROW_LEFT);
	}

	XtReleaseGC(w, fgc);
    }

    /* Copy the cached pixmaps into the widget/gadget */
    if (XmIsGadget(w))
    {
	CBG_ArmedPixmap(w) = acp->pixmap;
	CBG_CascadePixmap(w) = cp->pixmap;
    }
    else
    {
	CB_ArmedPixmap(w) = acp->pixmap;
	CB_CascadePixmap(w) = cp->pixmap;
    }
}

/******************************** GADGET PART *******************************/
static void
class_initialize(void)
{
    XtResourceList combined, labels;
    int ncom;
    Cardinal nlabels;

    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(CBG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(CBG_ClassCachePart(NULL));
    ClassCacheHead(CBG_ClassCachePart(NULL)).next =
	&ClassCacheHead(CBG_ClassCachePart(NULL));

    _XmCascadeBGRectClassExtRec.record_type = XmQmotif;

    /*
     * Label subclasses (ToggleBG, PushBG, CascadeBG) have a problem.  Since
     * we do all the subpart manipulation in the pre- and post- hooks, and
     * since those hooks aren't chained, we have to either make multiple
     * calls to XtGetSubresources/Xt[Get|Set]Subvalues, or merge the resource
     * lists.  Since I just wrote _XmTransformSubresources, seems like a
     * waste not to use it.
     */
    ncom = XtNumber(cache_resources) +
	xmLabelGCacheObjClassRec.object_class.num_resources;

    _XmTransformSubResources(xmLabelGCacheObjClassRec.object_class.resources,
			   xmLabelGCacheObjClassRec.object_class.num_resources,
			     &labels, &nlabels);

    combined = (XtResourceList)XtMalloc(sizeof(XtResource) * ncom);

    memcpy(combined, labels, nlabels * sizeof(XtResource));
    memcpy(&combined[nlabels],
           cache_resources,
 	   XtNumber(cache_resources) * sizeof(XtResource));

    XtFree((char *)labels);

    xmCascadeButtonGCacheObjClassRec.object_class.resources = combined;
    xmCascadeButtonGCacheObjClassRec.object_class.num_resources = ncom;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmCASCADE_BUTTON_GADGET_BIT);
}

static void
initialize_posthook(Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    XmWidgetExtData ext;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "CascadeBG InitializePosthook\n"));

    /* don't let the null fool you */
    LabG_Cache(new_w) = (XmLabelGCacheObjPart *)
	_XmCachePart(LabG_ClassCachePart(NULL),
		     (XtPointer)LabG_Cache(new_w),
		     sizeof(XmLabelGCacheObjPart));
    CBG_Cache(new_w) = (XmCascadeButtonGCacheObjPart *)
	_XmCachePart(CBG_ClassCachePart(NULL),
		     (XtPointer)CBG_Cache(new_w),
		     sizeof(XmCascadeButtonGCacheObjPart));

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);

    XtFree((char *)ext);
}

static void
place_cascade(Widget w)
{
    Position x;

    if (CBG_Submenu(w))
    {
	CBG_Cascade_x(w) =
	    LabG_Highlight(w) + LabG_Shadow(w) + LabG_MarginWidth(w);
	if (LabG_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R)
	    CBG_Cascade_x(w) =
		XtWidth(w) - CBG_Cascade_x(w) - CBG_Cascade_width(w);

	CBG_Cascade_y(w) = (XtHeight(w) - CBG_Cascade_height(w)) / 2;

	/* Make sure the label and cascade don't overlap */

	if (LabG_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R)
	{
	    x = CBG_Cascade_x(w) - CASCADE_SPACING - LabG_TextRect_width(w);
	    if (LabG_TextRect_x(w) > x)
		LabG_TextRect_x(w) = x;
	}
	else
	{
	    x = CBG_Cascade_x(w) + CBG_Cascade_width(w) + CASCADE_SPACING;
	    if (LabG_TextRect_x(w) < x)
		LabG_TextRect_x(w) = x;
	}
    }
    else
    {
    }
}

static void
size_cascade(Widget w)
{
    int		dummy;
    Window	dummyw;
    unsigned	width, height;

    if (CBG_Submenu(w))
    {
	if (CBG_CascadePixmap(w) > XmUNSPECIFIED_PIXMAP)
	{
	    XGetGeometry(XtDisplay(w), CBG_CascadePixmap(w),
			 &dummyw, &dummy, &dummy,
			 &width, &height, (unsigned *)&dummy, (unsigned *)&dummy);
	    CBG_Cascade_width(w) = width;
	    CBG_Cascade_height(w) = height;
	}
	else
	{
	    CBG_Cascade_width(w) = CBG_Cascade_height(w) =
		LabG_MenuType(w) == XmMENU_OPTION
		? default_font_height(LabG_Font(w)) + (LabG_Shadow(w) << 1)
		: 0;
	}
    }
    else
    {
	CBG_Cascade_width(w) = 0;
	CBG_Cascade_height(w) = 0;
    }
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    if (!XmIsManager(XtParent(new_w)))
    {
	_XmError(new_w, "parent should be manager.");
    }

    if (CBG_MapDelay(new_w) < 0)
    {
	_XmWarning(new_w, "MappingDelay must be non-negative.");
	CBG_MapDelay(new_w) = 180;
    }
    if (CBG_Submenu(new_w) && (!XmIsRowColumn(CBG_Submenu(new_w)) ||
	(RC_Type(CBG_Submenu(new_w)) != XmMENU_PULLDOWN)))
    {
	_XmWarning(new_w, "Submenu must a pull-down menu.");
	CBG_Submenu(new_w) = NULL;
    }

    CBG_Cascade_x(new_w) = 0;
    CBG_Cascade_y(new_w) = 0;
    CBG_Cascade_width(new_w) = 0;
    CBG_Cascade_height(new_w) = 0;

    CBG_SetArmed(new_w, False);
    CBG_ArmedPixmap(new_w) = None;

    if (LabG_MenuType(new_w) == XmMENU_BAR ||
	LabG_MenuType(new_w) == XmMENU_POPUP ||
	LabG_MenuType(new_w) == XmMENU_PULLDOWN)
    {
	G_TraversalOn(new_w) = True;
	LabG_Highlight(new_w) = 0;
    }
    else if (LabG_MenuType(new_w) != XmMENU_OPTION)
    {
	_XmError(new_w, "Cascade gadget parent is incorrect type.");
    }

    if (LabG_MenuType(new_w) == XmMENU_BAR)
    {
	Dimension margin_width;
	XtResource resource =
	{
	    XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	    sizeof(Dimension), 0,
	    XmRImmediate, (XtPointer)XmINVALID_DIMENSION
	};

	/* Bend over backwards to see if the user didn't specify a marginWidth.
	 * Why doesn't this just use a default value like CascadeB does?
	 */
	XtGetSubresources(XtParent(new_w), &margin_width, XtName(new_w),
			  "XmCascadeButtonGCacheObjClass", &resource, 1,
			  args, *num_args);
	if (margin_width == (Dimension)XmINVALID_DIMENSION)
	    LabG_MarginWidth(new_w) = 6;
    }

    else
    {
	if (LabG_MenuType(new_w) != XmMENU_OPTION &&
	    CBG_CascadePixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	    CBG_Submenu(new_w))
	{
	    _XmCreateArrowPixmaps(new_w);
	}
	size_cascade(new_w);

	if (CBG_Submenu(new_w))
	{
	    int margin;

	    /* Make sure there's enough room on the side
	     * for the cascade pixmap.
	     */
	    margin = (CBG_Cascade_width(new_w) + CASCADE_SPACING)
		- (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
		   ? LabG_MarginRight(new_w) : LabG_MarginLeft(new_w));
	    if (margin > 0)
	    {
		if (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R)
		    LabG_MarginRight(new_w) += margin;
		else
		{
		    LabG_MarginLeft(new_w) += margin;
		    LabG_TextRect_x(new_w) += margin;
		}
		if (!XtWidth(request))
		    XtWidth(new_w) += margin;
	    }

	    /* Make sure there's enough room vertically */
	    margin = CBG_Cascade_height(new_w) - (LabG_TextRect_height(new_w)
						  + LabG_MarginTop(new_w)
						  + LabG_MarginBottom(new_w));
	    if (margin > 0)
	    {
		LabG_MarginTop(new_w) += margin >> 1;
		LabG_MarginBottom(new_w) += (margin + 1) >> 1;
		if (LabG_MenuType(new_w) != XmMENU_OPTION &&
		    !XtHeight(request))
		{
		    LabG_TextRect_y(new_w) += margin >> 1;
		    XtHeight(new_w) += margin;
		}
	    }
	}

	place_cascade(new_w);
    }

    if (CBG_Submenu(new_w))
    {
	/* Make sure the RC also knows how to locate us */
	RC_MenuSubmenu(new_w);
    }

    G_EventMask(new_w) = XmARM_EVENT | XmACTIVATE_EVENT |
	XmENTER_EVENT | XmLEAVE_EVENT |
	XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT |
	XmHELP_EVENT | XmBDRAG_EVENT;

    CBG_Timer(new_w) = 0;
    LabGClass_MenuProcs(XtClass(new_w)) = MenuProcEntry;
}

static void
resize(Widget w)
{
#define superclass (&xmLabelGadgetClassRec)
    (*superclass->rect_class.resize) (w);
#undef superclass

    if (LabG_MenuType(w) != XmMENU_BAR)
    {
	place_cascade(w);
    }
}

static void
destroy(Widget w)
{
    if (CBG_Timer(w) != 0)
    {
	XtRemoveTimeOut(CBG_Timer(w));
	CBG_Timer(w) = 0;
    }

    if (CBG_ArmedPixmap(w))
    {
	_XmArrowPixmapCacheDelete((XtPointer)CBG_ArmedPixmap(w));
	_XmArrowPixmapCacheDelete((XtPointer)CBG_CascadePixmap(w));
    }

   _XmCacheDelete((XtPointer)CBG_Cache(w));
}

static Boolean
set_values_prehook(Widget old, Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec, rsec;

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);
    rsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent(new_w);
    ((XmExtRec *)nsec)->object.xrm_name = new_w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
	  LabG_Cache(new_w),
	  sizeof(XmLabelGCacheObjPart));
    memcpy(&((XmCascadeButtonGCacheObject)nsec)->cascade_button_cache,
           CBG_Cache(new_w),
	   sizeof(XmCascadeButtonGCacheObjPart));

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));

    ed->widget = (Widget)nsec;
    ed->reqWidget = (Widget)rsec;

    _XmPushWidgetExtData(new_w, ed, XmCACHE_EXTENSION);

    _XmGadgetImportSecondaryArgs(new_w, args, num_args);

    XtSetSubvalues((XtPointer)nsec,
		   (*bce)->secondaryObjectClass->core_class.resources,
		   (*bce)->secondaryObjectClass->core_class.num_resources,
		   args, *num_args);

    memcpy(rsec, nsec, size);

    LabG_Cache(new_w) = &(((XmCascadeButtonGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmCascadeButtonGCacheObject)rsec)->label_cache);

    CBG_Cache(new_w) =
	&(((XmCascadeButtonGCacheObject)nsec)->cascade_button_cache);
    CBG_Cache(request) =
	&(((XmCascadeButtonGCacheObject)rsec)->cascade_button_cache);

    _XmExtImportArgs((Widget)nsec, args, num_args);

    return False;
}

static Boolean
set_values_posthook(Widget old, Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    XmWidgetExtData ext;

    if (!_XmLabelCacheCompare((XtPointer)LabG_Cache(new_w),
			      (XtPointer)LabG_Cache(old)))
    {

	_XmCacheDelete((XtPointer)LabG_Cache(old));

	LabG_Cache(new_w) = (XmLabelGCacheObjPart *)
	    _XmCachePart(LabG_ClassCachePart(NULL),
			 (XtPointer)LabG_Cache(new_w),
			 sizeof(XmLabelGCacheObjPart));
    }
    else
    {
	LabG_Cache(new_w) = LabG_Cache(old);
    }

    if (!_XmCascadeBCacheCompare((XtPointer)CBG_Cache(new_w),
				 (XtPointer)CBG_Cache(old)))
    {
	_XmCacheDelete((XtPointer)CBG_Cache(old));

	CBG_Cache(new_w) = (XmCascadeButtonGCacheObjPart *)
	    _XmCachePart(CBG_ClassCachePart(NULL),
			 (XtPointer)CBG_Cache(new_w),
			 sizeof(XmCascadeButtonGCacheObjPart));
    }
    else
    {
	CBG_Cache(new_w) = CBG_Cache(old);
    }

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);

    XtFree((char *)ext);

    return False;
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "set_values()\n"));

    if (CBG_MapDelay(new_w) < 0)
    {
	_XmWarning(new_w, "MappingDelay must be non-negative.");
	CBG_MapDelay(new_w) = CBG_MapDelay(old);
    }
    if (CBG_Submenu(new_w) && (!XmIsRowColumn(CBG_Submenu(new_w)) ||
	(RC_Type(CBG_Submenu(new_w)) != XmMENU_PULLDOWN)))
    {
	_XmWarning(new_w, "Submenu must a pull-down menu.");
	CBG_Submenu(new_w) = CBG_Submenu(old);
    }

    if (LabG_MenuType(new_w) != XmMENU_OPTION)
    {
	G_TraversalOn(new_w) = True;
	LabG_Highlight(new_w) = 0;
    }

    if (CBG_CascadePixmap(old) != CBG_CascadePixmap(new_w) ||
	(CBG_Submenu(new_w)
	 ? CBG_CascadePixmap(new_w) == XmUNSPECIFIED_PIXMAP
	 : CBG_ArmedPixmap(new_w)) ||
	((CBG_ArmedPixmap(new_w) || (LabG_MenuType(new_w) == XmMENU_OPTION &&
	  CBG_CascadePixmap(new_w) == XmUNSPECIFIED_PIXMAP)) &&
	 LabG_Font(old) != LabG_Font(new_w)))
    {
	if (CBG_ArmedPixmap(old))
	{
	    _XmArrowPixmapCacheDelete((XtPointer)CBG_CascadePixmap(old));
	    _XmArrowPixmapCacheDelete((XtPointer)CBG_ArmedPixmap(old));
	    if (CBG_CascadePixmap(old) == CBG_CascadePixmap(new_w))
		CBG_CascadePixmap(new_w) = XmUNSPECIFIED_PIXMAP;
	    CBG_ArmedPixmap(new_w) = None;
	}
	if (LabG_MenuType(new_w) != XmMENU_BAR)
	{
	    if (LabG_MenuType(new_w) != XmMENU_OPTION &&
		CBG_CascadePixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
		CBG_Submenu(new_w))
	    {
		_XmCreateArrowPixmaps(new_w);
	    }
	    size_cascade(new_w);
	    refresh_needed = True;
	}
    }

    if (XtSensitive(new_w) != XtSensitive(old))
    {
	refresh_needed = True;
    }

    if (CBG_Submenu(old) != CBG_Submenu(new_w))
    {
	/* Make sure the RC also knows how to locate us */
	RC_MenuSubmenu(new_w);
    }

    /* Adjust margins for the cascade pixmap size if necessary.
     * Margins be increased or decreased; if the margin was explicitly set
     * in this call, don't decrease it past that (though it can get bigger).
     */
    if (LabG_MenuType(new_w) != XmMENU_BAR &&
	(CBG_Cascade_width(new_w) != CBG_Cascade_width(old)
	 || CBG_Cascade_height(new_w) != CBG_Cascade_height(old)
	 || LabG_StringDirection(new_w) != LabG_StringDirection(old)
	 || (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	     ? LabG_MarginRight(new_w) != LabG_MarginRight(old)
	     : LabG_MarginLeft(new_w) != LabG_MarginLeft(old))
	 || LabG_MarginTop(new_w) != LabG_MarginTop(old)
	 || LabG_MarginBottom(new_w) != LabG_MarginBottom(old)))
    {
	int margin, tm, bm;

	margin = CBG_Cascade_width(new_w) + CASCADE_SPACING
	    - (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	       ? LabG_MarginRight(new_w) : LabG_MarginLeft(new_w));
	if (margin && (margin > 0 ||
	    (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	     ? LabG_MarginRight(new_w) == LabG_MarginRight(old)
	     : LabG_MarginLeft(new_w) == LabG_MarginLeft(old))))
	{
	    if (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R)
		LabG_MarginRight(new_w) += margin;
	    else
	    {
		LabG_MarginLeft(new_w) += margin;
		LabG_TextRect_x(new_w) += margin;
	    }
	    if (LabG_RecomputeSize(new_w) || !XtWidth(request))
		XtWidth(new_w) += margin;
	}

	margin = CBG_Cascade_height(new_w) - (LabG_TextRect_height(new_w)
					      + LabG_MarginTop(new_w)
					      + LabG_MarginBottom(new_w));
	if (margin)
	{
	    tm = margin / 2;
	    if (tm < (LabG_MarginTop(new_w) == LabG_MarginTop(old)
		      ? XmDEFAULT_TOP_MARGIN - (int)LabG_MarginTop(new_w)
		      : 0))
		tm = (LabG_MarginTop(new_w) == LabG_MarginTop(old)
		      ? XmDEFAULT_TOP_MARGIN - (int)LabG_MarginTop(new_w)
		      : 0);
	    LabG_MarginTop(new_w) += tm;
	    if (LabG_MenuType(new_w) != XmMENU_OPTION &&
		(LabG_RecomputeSize(new_w) || !XtHeight(request)))
		XtHeight(new_w) += tm;

	    bm = (margin + 1) / 2;
	    if (bm < (LabG_MarginBottom(new_w) == LabG_MarginBottom(old)
		      ? XmDEFAULT_BOTTOM_MARGIN - (int)LabG_MarginBottom(new_w)
		      : 0))
		bm = (LabG_MarginBottom(new_w) == LabG_MarginBottom(old)
		      ? XmDEFAULT_BOTTOM_MARGIN - (int)LabG_MarginBottom(new_w)
		      : 0);
	    LabG_MarginBottom(new_w) += bm;
	    if (LabG_RecomputeSize(new_w) || !XtHeight(request))
		XtHeight(new_w) += bm;

	    if (tm != bm)
		LabG_TextRect_y(new_w) += (tm - bm) / 2;
	}

	refresh_needed = True;
    }

    if (LabG_MenuType(new_w) != XmMENU_BAR &&
	(CBG_Cascade_width(new_w) != CBG_Cascade_width(old)
	 || CBG_Cascade_height(new_w) != CBG_Cascade_height(old)
	 || LabG_Shadow(new_w) != LabG_Shadow(old)
	 || LabG_MarginWidth(new_w) != LabG_MarginWidth(old)
	 || LabG_TextRect_y(new_w) != LabG_TextRect_y(old)
	 || LabG_TextRect_height(new_w) != LabG_TextRect_height(old)
	 || LabG_StringDirection(new_w) != LabG_StringDirection(old)))
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
get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec;

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache(new_w),
	   sizeof(XmLabelGCacheObjPart));
    memcpy(&((XmCascadeButtonGCacheObject)nsec)->cascade_button_cache,
           CBG_Cache(new_w),
	   sizeof(XmCascadeButtonGCacheObjPart));

    /*
     * don't do this and ResInd will blow up.
     */
    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent(new_w);
    ((XmExtRec *)nsec)->object.xrm_name = new_w->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;

    _XmPushWidgetExtData(new_w, ed, XmCACHE_EXTENSION);

    XtGetSubvalues((XtPointer)nsec,
		   (*bce)->secondaryObjectClass->core_class.resources,
		   (*bce)->secondaryObjectClass->core_class.num_resources,
		   args, *num_args);

    _XmExtGetValuesHook((Widget)nsec, args, num_args);
}

static void
get_values_posthook(Widget new_w, ArgList args, Cardinal *num_args)
{
    XmWidgetExtData ext;

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree((XtPointer)ext->widget);

    XtFree((char *)ext);
}


static void
draw_cascade(Widget w)
{
    Pixmap pm;

    if (CBG_IsArmed(w) || LabG_MenuType(w) == XmMENU_OPTION)
    {
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       XmParentTopShadowGC(w), XmParentBottomShadowGC(w),
		       XtX(w) + LabG_Highlight(w), XtY(w) + LabG_Highlight(w),
		       XtWidth(w) - (LabG_Highlight(w) << 1),
		       XtHeight(w) - (LabG_Highlight(w) << 1),
		       LabG_Shadow(w), XmSHADOW_OUT);
    }

    pm = CBG_IsArmed(w) && CBG_ArmedPixmap(w) > XmUNSPECIFIED_PIXMAP
	? CBG_ArmedPixmap(w)
	: CBG_CascadePixmap(w);
    if (pm > XmUNSPECIFIED_PIXMAP)
    {
	XCopyArea(XtDisplay(w), pm, XtWindow(w),
		  LabG_NormalGC(w),
		  0, 0,
		  CBG_Cascade_width(w), CBG_Cascade_height(w),
		  XtX(w) + CBG_Cascade_x(w), XtY(w) + CBG_Cascade_y(w));
    }
    else if (pm == XmUNSPECIFIED_PIXMAP && LabG_MenuType(w) == XmMENU_OPTION && CBG_Submenu(w))
    {
	Dimension width, height, ch;

	/* Base the rectangle cascade on the text height,
	 * but for bigger text, it gets successively squatter.
	 */
	width = CBG_Cascade_width(w) - 3;
	height = (LabG_Shadow(w) << 1) + 1;
	ch = CBG_Cascade_height(w) - height;
	if (ch >= 13)
	{
	    width -= 3;
	    height += 3;
	}
	else if (ch >= 9)
	{
	    width -= 2;
	    height += 2;
	}
	else if (ch >= 6)
	{
	    width--;
	    height++;
	}
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       XmParentTopShadowGC(w), XmParentBottomShadowGC(w),
		       XtX(w) + CBG_Cascade_x(w),
		       XtY(w) + CBG_Cascade_y(w) +
			((CBG_Cascade_height(w) - height) >> 1),
		       width, height, LabG_Shadow(w), XmSHADOW_OUT);
    }
}

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "expose() Armed(%d) type %s\n",
		      CBG_IsArmed(w), _LtDebugMenuType2String(LabG_MenuType(w))));

    draw_cascade(w);

#define superclass (&xmLabelGadgetClassRec)
    (*superclass->rect_class.expose) (w, event, region);
#undef superclass
}

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIX ME */

    return _XmSecondaryResourceData(&_XmCascadeBGRectClassExtRec,
				    data, NULL, NULL, NULL, NULL);
}

static void
DoSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
Boolean validButton;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:DoSelect(%d)\n\t%s posted mine %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CBG_Submenu(w) ? XtName(CBG_Submenu(w)) : "none"
	));
    DEBUGOUT(_LtDebug("MENU", w, "%s:DoSelect(%d)\n\t%s posted mine %s %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CBG_Submenu(w) ? XtName(CBG_Submenu(w)) : "none",
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
	_XmMenuFocus(XtParent(w), XmMENU_FOCUS_SET, CurrentTime);
	if (CBG_Submenu(w))
	{
	    if (RC_PopupPosted(XtParent(w)) != CBG_Submenu(w))
	    {
		_XmCascadingPopup(w, event, True);
	    }

	    RCClass_MenuTraverse(CBG_Submenu(w), XmTRAVERSE_HOME);

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
	    if (!LabG_SkipCallback(w) && CBG_ActivateCall(w))
	    {
		XFlush(XtDisplay(w));

		XtCallCallbackList(w, CBG_ActivateCall(w), &cbs);
	    }
	    XmCascadeButtonGadgetHighlight(w, False);
	    if (RC_PopupPosted(XtParent(w)))
	    {
	    Boolean poppedUp;

		RC_MenuShellPopdown(w, event, &poppedUp);
	    }
	}
    }
}

static void
MenuBarSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean validButton;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:MenuBarSelect(%d)\n\t%s posted mine %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CBG_Submenu(w) ? XtName(CBG_Submenu(w)) : "none"
	));
    DEBUGOUT(_LtDebug("MENU", w, "%s:MenuBarSelect(%d)\n\t%s posted mine %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CBG_Submenu(w) ? XtName(CBG_Submenu(w)) : "none"
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
	if (RC_PopupPosted(XtParent(w)))
	{
	Cardinal i;

	    for (i = 0; i < MGR_NumChildren(RC_PopupPosted(XtParent(w))); i++)
	    {
	    Widget w1 = MGR_Children(RC_PopupPosted(XtParent(w)))[i];

		_XmMenuDisarmItem(w1);
	    }
	    if (RC_PopupPosted(XtParent(w)) != CBG_Submenu(w))
	    {
	    Boolean poppedUp;

		RC_MenuShellPopdown(w, event, &poppedUp);
	    }
	    else if (CBG_Submenu(w) && RC_PopupPosted(CBG_Submenu(w)))
	    {
	    Boolean poppedUp;

		RC_MenuShellPopdown(RC_CascadeBtn(RC_PopupPosted(CBG_Submenu(w))), event, &poppedUp);
	    }
	}
	XmCascadeButtonGadgetHighlight(w, True);
	MGR_ActiveChild(XtParent(w)) = w;

	RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_ARM, XtParent(w), NULL);

	if (RC_PopupPosted(XtParent(w)) != CBG_Submenu(w))
	{
	    _XmCascadingPopup(w, event, True);
	}
	_XmSetInDragMode(w, True);
    }
}

static void
ArmAndPost(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ArmAndPost(): %p %p\n", w, CBG_Submenu(w)));
    DEBUGOUT(_LtDebug("MENU", w, "%s:ArmAndPost(%d)\n\t%s posted mine %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CBG_Submenu(w) ? XtName(CBG_Submenu(w)) : "none"
	));

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);

#if 0
    if (!RC_IsArmed(CBG_Submenu(w)))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "MENU_ARM\n"));

	RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_ARM, XtParent(w), NULL);
    }

    _XmCascadingPopup(w, event, True);
    _XmSetInDragMode(w, True);
#else
    XmCascadeButtonGadgetHighlight(w, True);
    MGR_ActiveChild(XtParent(w)) = w;
    RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_ARM, XtParent(w), NULL);
    _XmCascadingPopup(w, event, True);
    _XmSetInDragMode(w, True);
#endif
}

static void
MenuBarEnter(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "MenuBarEnter\n\t%s %s %s %s posted posting %s\n",
	_XmGetInDragMode(w) ? "Dragging" : "Not Dragging",
	_LtDebugRcType2String(LabG_MenuType(w)),
	RC_IsArmed(XtParent(w)) ? "Armed" : "Not Armed",
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CBG_Submenu(w) ? XtName(CBG_Submenu(w)) : "nothing"
	));
    DEBUGOUT(_LtDebug("MENU", w, "MenuBarEnter\n\t%s %s %s %s posted posting %s\n",
	_XmGetInDragMode(w) ? "Dragging" : "Not Dragging",
	_LtDebugRcType2String(LabG_MenuType(w)),
	RC_IsArmed(XtParent(w)) ? "Armed" : "Not Armed",
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "nothing",
	CBG_Submenu(w) ? XtName(CBG_Submenu(w)) : "nothing"
	));

    if (_XmGetInDragMode(w))
    {
	if (LabG_MenuType(w) == XmMENU_BAR)
	{
	    if (RC_IsArmed(XtParent(w)))
	    {
		if (CBG_Submenu(w))
		{
		    if (RC_PopupPosted(XtParent(w)) && RC_PopupPosted(XtParent(w)) != CBG_Submenu(w))
		    {
			_XmCascadingPopup(w, event, False);
		    }
		    if (RC_PopupPosted(XtParent(w)) && RC_PopupPosted(XtParent(w)) != CBG_Submenu(w))
		    {
		    Boolean poppedUp;

			RC_MenuShellPopdown(w, event, &poppedUp);
		    }
		    XmCascadeButtonGadgetHighlight(w, True);
		    if (!RC_PopupPosted(XtParent(w)))
		    {
			RC_MenuCascading(w, event);
		    }
		}
		else
		{
		    _XmCascadingPopup(w, event, True);
		    if (RC_PopupPosted(XtParent(w)) && RC_PopupPosted(XtParent(w)) != CBG_Submenu(w))
		    {
		    Boolean poppedUp;

			RC_MenuShellPopdown(w, event, &poppedUp);
		    }
		    XmCascadeButtonGadgetHighlight(w, True);
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
    if (_XmGetInDragMode(w))
    {
	if (LabG_MenuType(w) == XmMENU_BAR)
	{
	    if (RC_IsArmed(XtParent(w)))
	    {
		if (!CBG_Submenu(w) || (RC_PopupPosted(XtParent(w)) && RC_PopupPosted(XtParent(w)) != CBG_Submenu(w)))
		{
		    XmCascadeButtonGadgetHighlight(w, False);
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

    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    _XmRecordEvent(event);

    /* Is it even the right kind of event? */
    if (!event || event->type != ButtonPress)
    {
	return;
    }

    /* If the submenu is already active, disable keyboard traversal
       and set it to mouse traversal */

    /* Was it the right button? */
    RC_MenuButton(w, event, &validButton);

    DEBUGOUT(_LtDebug("MENU", w, "%s:StartDrag(%d) - %s posted mine %s\n",
	__FILE__, __LINE__,
	RC_PopupPosted(XtParent(w)) ? XtName(RC_PopupPosted(XtParent(w))) : "None",
	CBG_Submenu(w) ? XtName(CBG_Submenu(w)) : "None"
	));
    if (validButton)
    {
	if (CBG_Submenu(w) && RC_PopupPosted(XtParent(w)) == CBG_Submenu(w))
	{
	Cardinal i;

	    for (i = 0; i < MGR_NumChildren(CBG_Submenu(w)); i++)
	    {
		_XmMenuDisarmItem(MGR_Children(CBG_Submenu(w))[i]);
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
	    XmCascadeButtonGadgetHighlight(w, True);
	}

	_XmSetInDragMode(w, True);
    }
}

static void
CascadePopupHandler(XtPointer clientData, XtIntervalId *id)
{
    Widget w = (Widget)clientData;

    CBG_Timer(w) = 0;

    DEBUGOUT(_LtDebug(__FILE__, w, "CascadePopupHandler()\n"));

    _XmCascadingPopup(w, NULL, True);	/* FIX ME: NULL? */
}


static void
DelayedArm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "DelayedArm()\n"));

    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    if (_XmGetInDragMode(w))
    {
	if (RC_PopupPosted(XtParent(w)) != CBG_Submenu(w))
	{
	Boolean poppedUp;

	    RC_MenuShellPopdown(w, NULL, &poppedUp);
	}
	if (!RC_PopupPosted(XtParent(w)))
	{
	    CBG_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
					   CBG_MapDelay(w),
					   CascadePopupHandler,
					   (XtPointer)w);

	    XmCascadeButtonGadgetHighlight(w, True);
	}
    }
}

static void
CheckDisarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int x = ((XLeaveWindowEvent *)event)->x_root;
    int y = ((XLeaveWindowEvent *)event)->y_root;
    Widget subpane;

    DEBUGOUT(_LtDebug(__FILE__, w, "CheckDisarm() - %i %i %i\n", x, y, XtHeight(w)));

    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    if (_XmGetInDragMode(w))
    {
	if (CBG_Timer(w))
	{
	    XtRemoveTimeOut(CBG_Timer(w));
	    CBG_Timer(w) = 0;
	}
	if (!RC_PopupPosted(XtParent(w)) || (CBG_Submenu(w) != RC_PopupPosted(XtParent(w))))
	{
	    XmCascadeButtonGadgetHighlight(w, False);
	}
	else
	{
	    subpane = XtParent(CBG_Submenu(w));

	    if (x < XtX(subpane) || x >= XtX(subpane) + XtWidth(subpane) ||
		y < XtY(subpane) || y >= XtY(subpane) + XtHeight(subpane))
	    {
		Boolean poppedUp;

		RC_MenuShellPopdown(w, event, &poppedUp);
		XmCascadeButtonGadgetHighlight(w, False);
	    }
	}
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
ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ArmAndActivate\n"));

    MGR_ActiveChild(XtParent(w)) = w;
    if (LabG_MenuType(w) == XmMENU_BAR)
    {
	if (!RC_IsArmed(XtParent(w)))
	{
	    RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_ARM, XtParent(w), NULL);
	    _XmMenuFocus(XtParent(w), XmMENU_FOCUS_SAVE, CurrentTime);
	    _XmSetInDragMode(XtParent(w), False);
	}
	_XmMenuFocus(XtParent(w), XmMENU_FOCUS_SET, CurrentTime);
	MenuBarSelect(w, event, params, num_params);
	DoSelect(w, event, params, num_params);
    }
    else if (LabG_MenuType(w) == XmMENU_OPTION)
    {
	XmProcessTraversal(w, XmTRAVERSE_CURRENT);

	if (CBG_Submenu(w) && !RC_IsArmed(CBG_Submenu(w)))
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "MENU_ARM\n"));

	    RCClass_MenuProcs(XtClass(XtParent(w)))(XmMENU_ARM,
						    XtParent(w), NULL);
	}

	CascadePopupHandler((XtPointer)w, NULL);
    }
    else /* pullright */
    {
	CascadePopupHandler((XtPointer)w, NULL);
    }
}

static void
input_dispatch(Widget gadget, XEvent *event, Mask event_mask)
{
    Cardinal num_params = 0;

    switch (event_mask)
    {
    case XmARM_EVENT:
	if (LabG_MenuType(gadget) == XmMENU_BAR)
	{
	    MenuBarSelect(gadget, event, NULL, &num_params);
	}
	else if (LabG_MenuType(gadget) == XmMENU_OPTION)
	{
	    ArmAndPost(gadget, event, NULL, &num_params);
	}
	else
	{
	    StartDrag(gadget, event, NULL, &num_params);
	}
	break;

    case XmACTIVATE_EVENT:
	DoSelect(gadget, event, NULL, &num_params);
	break;

    case XmENTER_EVENT:
	if ((LabG_MenuType(gadget) == XmMENU_PULLDOWN) ||
	    (LabG_MenuType(gadget) == XmMENU_POPUP))	/* rws 23 Mar 1997 */
	{
	    DelayedArm(gadget, event, NULL, &num_params);
	}
	else if (LabG_MenuType(gadget) == XmMENU_BAR)
	{
	    MenuBarEnter(gadget, event, NULL, &num_params);
	}
	break;

    case XmLEAVE_EVENT:
	if ((LabG_MenuType(gadget) == XmMENU_PULLDOWN) ||
	    (LabG_MenuType(gadget) == XmMENU_POPUP))	/* rws 23 Mar 1997 */
	{
	    CheckDisarm(gadget, event, NULL, &num_params);
	}
	else if (LabG_MenuType(gadget) == XmMENU_BAR)
	{
	    MenuBarLeave(gadget, event, NULL, &num_params);
	}
	break;

    case XmFOCUS_IN_EVENT:
	_XmFocusInGadget(gadget, event, NULL, &num_params);
	break;

    case XmFOCUS_OUT_EVENT:
	_XmFocusOutGadget(gadget, event, NULL, &num_params);
	break;

    case XmBDRAG_EVENT:	/* FIX ME: MLM - is this right? */
	_XmProcessDrag(gadget, event, NULL, NULL);
	break;

    case XmHELP_EVENT:
	Help(gadget, event, NULL, &num_params);
	break;
    }
}

static Boolean
visual_change(Widget w,
	      Widget parent,
	      Widget n)
{
    _XmWarning(w,"%s(%d) - visual_change not written\n", __FILE__, __LINE__);
    return False;
}

static void
MenuProcEntry(int proc, Widget w,...)
{
    va_list arg_list;
    /*
    XEvent *event = NULL;
    */

    va_start(arg_list, w);

    switch (proc)
    {
    case XmMENU_ARM:
	{
	/* XtExposeProc exp = XtClass(w)->core_class.expose; */

	    XmCascadeButtonGadgetHighlight(w, True);
	    XmProcessTraversal(w, XmTRAVERSE_CURRENT);
	    MGR_ActiveChild(XtParent(w)) = w;
	    /*
	    (exp) (w, event, (Region)NULL);
	    */
	}
	break;
    case XmMENU_DISARM:
	{
	/* XtExposeProc exp = XtClass(w)->core_class.expose; */

	    XmCascadeButtonGadgetHighlight(w, False);
	    MGR_ActiveChild(XtParent(w)) = NULL;
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

void
XmCascadeButtonGadgetHighlight(Widget w, Boolean highlight)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "XmCascadeButtonGadgetHighlight(hl %d, armed %d,"
		      " apm 0x%X, cpm 0x%X), %s, geo %d %d %dx%d\n",
		      highlight,
		      XmIsPrimitive(w) ? CB_IsArmed(w) : CBG_IsArmed(w),
		  XmIsPrimitive(w) ? CB_ArmedPixmap(w) : CBG_ArmedPixmap(w),
	      XmIsPrimitive(w) ? CB_CascadePixmap(w) : CBG_CascadePixmap(w),
		      _LtDebugMenuType2String(LabG_MenuType(w)),
		      XtX(w), XtY(w), XtWidth(w), XtHeight(w)
	     ));

    if (XmIsPrimitive(w))
    {
	XmCascadeButtonHighlight(w, highlight);

	return;
    }
    else if (!XmIsCascadeButtonGadget(w))
    {
	_XmError(w,
	    "XmCascadeButtonGadgetHighlight called with non-cascade button gadget");

	return;
    }

    if (LabG_MenuType(w) != XmMENU_OPTION)
	CBG_SetArmed(w, highlight);

    if (XtIsRealized(w))
    {
	if (!highlight)
	{
	    _XmClearBorder(XtDisplayOfObject(w),
			   XtWindowOfObject(w),
			   XtX(w), XtY(w), XtWidth(w), XtHeight(w),
			   LabG_Shadow(w));
	}
	draw_cascade(w);
    }
}

Widget
XmCreateCascadeButtonGadget(Widget parent, char *name,
			    Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmCascadeButtonGadgetClass,
			  parent,
			  arglist,
			  argcount);
}
