/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ToggleBG.c,v 1.2 2004/10/21 18:53:59 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ToggleBG.c,v 1.2 2004/10/21 18:53:59 dannybackx Exp $";

#include <LTconfig.h>

#include <stdarg.h>
#include <string.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/ToggleBGP.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/RepType.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuShell.h>
#include <Xm/MenuUtilP.h>
#include <Xm/CacheP.h>
#include <X11/ShellP.h>
#include <Xm/ToggleBP.h>

#include <XmI/DebugUtil.h>


/* Some M*tif versions (e.g. 1.2.6 on Solaris 2.6) leave less room above and
 * below the indicator than others (e.g. 1.2.5 on HP-UX 10, my reference).
 */
#undef LESS_VERTICAL_PADDING

#define UNSPECIFIED_TBG_BOOLEAN	((Boolean)85)
#define SQUARE_INDICATOR_DEC	3
#define SQUARE_INDICATOR_ELBOW	10
#define PIXMAP_INDICATOR_ELBOW	13

#define DETAIL_SHADOW_THICKNESS(w)	TBG_DetailShadowThickness(w)
#define INDICATOR_BOX_MASK		0x03
#define NEXT_TOGGLE(w)			(TBG_ToggleMode(w) == XmTOGGLE_BOOLEAN\
					 || TBG_IndType(w) != XmN_OF_MANY     \
					 ? !TBG_Set(w) : (TBG_Set(w) + 1) % 3)

/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static void resize(Widget w);

static void expose(Widget w, XEvent *event, Region region);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void input_dispatch(Widget gadget, XEvent *event, Mask event_mask);

static void secondary_object_create(Widget request, Widget new_w,
				    ArgList args, Cardinal *num_args);

static void initialize_posthook(Widget request, Widget new_w,
				ArgList args, Cardinal *num_args);

static Boolean set_values_prehook(Widget old, Widget request, Widget new_w,
				  ArgList args, Cardinal *num_args);

static Boolean set_values_posthook(Widget old, Widget request, Widget new_w,
				   ArgList args, Cardinal *num_args);

static void get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args);

static void get_values_posthook(Widget new_w, ArgList args, Cardinal *num_args);

static Cardinal get_sec_res_data(WidgetClass wc,
				 XmSecondaryResourceData **data);


static void Arm(Widget w, XEvent *event,
		String *params, Cardinal *num_params);

static void Select(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);

static void Disarm(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);

static void ArmAndActivate(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);

static void Help(Widget w, XEvent *event,
		 String *params, Cardinal *num_params);

static void EnterWindow(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void LeaveWindow(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void ButtonDown(Widget w, XEvent *event,
		       String *params, Cardinal *num_params);

static void ButtonUp(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void MenuProcEntry(int proc, Widget rc,...);

static void draw_toggle(Widget w, XEvent *event, Region region,
			int is_expose, int visual_set);

static int implicit_indicator(Widget w);
static void _XmUnselectColorDefault(Widget, int, XrmValue *);

/*
 * resources
 */
#define Offset(field) XtOffsetOf(XmToggleButtonGCacheObjRec, toggle_cache.field)
static XtResource cache_resources[] =
{
    {
	XmNindicatorSize, XmCIndicatorSize, XmRVerticalDimension,
	sizeof(Dimension), Offset(indicator_dim),
	XmRImmediate, (XtPointer)XmINVALID_DIMENSION
    },
    {
	XmNindicatorType, XmCIndicatorType, XmRIndicatorType,
	sizeof(unsigned char), Offset(ind_type),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNvisibleWhenOff, XmCVisibleWhenOff, XmRBoolean,
	sizeof(Boolean), Offset(visible),
	XmRImmediate, (XtPointer)UNSPECIFIED_TBG_BOOLEAN
    },
    {
	XmNspacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(spacing),
	XmRImmediate, (XtPointer)4
    },
    {
	XmNselectPixmap, XmCSelectPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(on_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNselectInsensitivePixmap, XmCSelectInsensitivePixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(insen_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	/* The type changed to an unsigned char in 2.0..oh just lovely. */
	XmNindicatorOn, XmCIndicatorOn, XmRIndicatorOn,
	sizeof(unsigned char), Offset(ind_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNfillOnSelect, XmCFillOnSelect, XmRBoolean,
	sizeof(Boolean), Offset(fill_on_select),
	XmRImmediate, (XtPointer)UNSPECIFIED_TBG_BOOLEAN
    },
    {
	XmNselectColor, XmCSelectColor, XmRPixel,
	sizeof(Pixel), Offset(select_color),
	XmRCallProc, (XtPointer)_XmSelectColorDefault
    },
    {
	XmNunselectColor, XmCUnselectColor, XmRPixel,
	sizeof(Pixel), Offset(unselect_color),
	XmRCallProc, (XtPointer)_XmUnselectColorDefault
    }
};

static XmSyntheticResource cache_syn_resources[] =
{
    {
	XmNspacing,
	sizeof(Dimension), Offset(spacing),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNindicatorSize,
	sizeof(Dimension), Offset(indicator_dim),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};

XmToggleButtonGCacheObjClassRec xmToggleButtonGCacheObjClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGCacheObjClassRec,
	/* class_name            */ "XmToggleButtonGCacheObjClass",
	/* widget_size           */ sizeof(XmToggleButtonGCacheObjRec),
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
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* XmExtObject part */
    {
	/* syn_resources      */ cache_syn_resources,
	/* num_syn_resources  */ XtNumber(cache_syn_resources),
	/* extension          */ NULL
    },
    /* LabelGCacheObj part */
    {
	/* foo                */ 0
    },
    /* ToggleButtonGCacheObj part */
    {
	/* foo                */ 0
    }
};

#undef Offset
#define Offset(field) XtOffsetOf(XmToggleButtonGadgetRec, toggle.field)

/* Resources for the togglebutton class */
static XtResource resources[] = {
    {
	XmNset, XmCSet, XmRBoolean,
	sizeof(Boolean), Offset(set),
	XmRImmediate,(XtPointer)False
    },
    {
	XmNvalueChangedCallback, XmCValueChangedCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_CB),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNarmCallback, XmCArmCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(arm_CB),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNdisarmCallback, XmCDisarmCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(disarm_CB),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmToggleButtonGadgetRec, gadget.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension),
	XtOffsetOf(XmToggleButtonGadgetRec,gadget.highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
    /* New for 2.0 : */
    {
	XmNdetailShadowThickness, XmCDetailShadowThickness,
	XmRDimension, sizeof(Dimension),
	Offset(detail_shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNindeterminatePixmap, XmCIndeterminatePixmap,
	XmRGadgetPixmap, sizeof(Pixmap),
	Offset(indeterminate_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNtoggleMode, XmCToggleMode,
	XmRToggleMode, sizeof(unsigned char),
	Offset(toggle_mode),
	XmRImmediate, (XtPointer)XmTOGGLE_BOOLEAN
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNdetailShadowThickness,
	sizeof(Dimension), Offset(detail_shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
     }
};

static XmBaseClassExtRec _XmToggleBGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ XmInheritInitializePrehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmToggleButtonGCacheObjClassRec,
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
    _XmToggleBCacheCompare
};

static XmGadgetClassExtRec _XmToggleBGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ XmInheritBaselineProc,
    /* display_rect_proc         */ XmInheritDisplayRectProc,
    /* margins_proc              */ XmInheritMarginsProc,
};

XmToggleButtonGadgetClassRec xmToggleButtonGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGadgetClassRec,
	/* class_name            */ "XmToggleButtonGadget",
	/* widget_size           */ sizeof(XmToggleButtonGadgetRec),
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
	/* compress_motion       */ True /*False*/,
	/* compress_exposure     */ XtExposeCompressMaximal /*XtExposeNoCompress*/,
	/* compress_enterleave   */ True /*False*/,
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
	/* extension             */ (XtPointer)&_XmToggleBGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight,  /* FIX ME */
	/* border_unhighlight */ XmInheritBorderUnhighlight, /* FIX ME */
	/* arm_and_activate   */ ArmAndActivate,
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ NULL, /* FIX ME */
	/* syn_resources      */ syn_resources,
	/* num_syn_resources  */ 0,
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&_XmToggleBGadgetClassExtRec
    },
    /* XmLabelGadget part */
    {
	/* setOverrideCallback */ XmInheritSetOverrideCallback,
	/* menuProcs           */ XmInheritMenuProc,
	/* extension           */ NULL
    },
    /* XmToggleButtonGadget part */
    {
	/* extension */ NULL
    },
};


WidgetClass xmToggleButtonGadgetClass = (WidgetClass)&xmToggleButtonGadgetClassRec;

/*
 *  Some #defines to make the code below more readable
 */

#define IN_MENU(w) (LabG_MenuType(w) == XmMENU_POPUP || \
		    LabG_MenuType(w) == XmMENU_PULLDOWN)

/******************************* CACHE PART *********************************/
static void
secondary_object_create(Widget request,
			Widget new_w,
			ArgList args,
			Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XtPointer nsec, rsec;
    XmWidgetExtData ed;
    Cardinal size;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "ToggleButtonGCacheRec %s being initialized.\n",
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

    TBG_Cache(new_w) = &(((XmToggleButtonGCacheObject)nsec)->toggle_cache);
    TBG_Cache(request) = &(((XmToggleButtonGCacheObject)rsec)->toggle_cache);
}

int
_XmToggleBCacheCompare(XtPointer A, XtPointer B)
{
    return !memcmp(((XmToggleButtonGCacheObjPart *)A),
		 ((XmToggleButtonGCacheObjPart *)B),
		 sizeof(XmToggleButtonGCacheObjPart));
}

/******************************* CACHE PART *********************************/
static void
class_initialize(void)
{
    XtResourceList combined, labels;
    int ncom;
    Cardinal nlabels;

    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(TBG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(TBG_ClassCachePart(NULL));
    ClassCacheHead(TBG_ClassCachePart(NULL)).next =
	&ClassCacheHead(TBG_ClassCachePart(NULL));

    _XmToggleBGRectClassExtRec.record_type = XmQmotif;

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

    xmToggleButtonGCacheObjClassRec.object_class.resources = combined;
    xmToggleButtonGCacheObjClassRec.object_class.num_resources = ncom;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmTOGGLE_BUTTON_GADGET_BIT);
}

static void
CreateSelectGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;

    if (TBG_VisualSet(w) == XmINDETERMINATE)
    {
	mask |= GCStipple | GCTileStipXOrigin | GCTileStipYOrigin;
	values.fill_style = FillOpaqueStippled;
	values.ts_x_origin = values.ts_y_origin = 0;
	values.stipple =
	    XmGetPixmapByDepth(XtScreen(w), "50_foreground", 1, 0, 1);
    } else {
	values.fill_style = FillSolid;
    }
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = TBG_SelectColor(w);
    values.background = XmParentBackground(w);

    TBG_SelectGC(w) = XtGetGC(w, mask, &values);

    values.foreground = TBG_UnselectColor(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "CreateSelectGC: unselect color %p\n",
	TBG_UnselectColor(w)));

    TBG_UnselectGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateBackgroundGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;

    values.fill_style = FillSolid;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = XmParentBackground(w);
    values.background = XmParentForeground(w);

    TBG_BackgroundGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_posthook(Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    XmWidgetExtData ext;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "ToggleBG InitializePosthook\n"));

    /* don't let the null fool you */
    LabG_Cache(new_w) = (XmLabelGCacheObjPart *)
	_XmCachePart(LabG_ClassCachePart(NULL),
		     (XtPointer)LabG_Cache(new_w),
		     sizeof(XmLabelGCacheObjPart));
    TBG_Cache(new_w) = (XmToggleButtonGCacheObjPart *)
	_XmCachePart(TBG_ClassCachePart(NULL),
		     (XtPointer)TBG_Cache(new_w),
		     sizeof(XmToggleButtonGCacheObjPart));

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);
    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);
    XtFree((char *)ext);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    if (!XtIsSubclass(XtParent(new_w), xmManagerWidgetClass))
    {
	_XmError(new_w, "parent should be manager.");
    }

    TBG_Armed(new_w) = False;

    TBG_VisualSet(new_w) = TBG_Set(new_w);

    /* Fix up the pixmaps */
    if (LabG_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	TBG_OnPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
	LabG_Pixmap(new_w) = TBG_OnPixmap(new_w);

    if (LabG_PixmapInsensitive(new_w) == XmUNSPECIFIED_PIXMAP &&
	TBG_InsenPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
	LabG_PixmapInsensitive(new_w) = TBG_InsenPixmap(new_w);

    if (LabG_IsPixmap(new_w) &&
	(TBG_IndeterminatePixmap(new_w) != XmUNSPECIFIED_PIXMAP ||
	 ((XtSensitive(new_w)
	   ? TBG_OnPixmap(new_w)
	   : TBG_InsenPixmap(new_w))
	  != XmUNSPECIFIED_PIXMAP)))
    {
	Dimension width, height;
	Dimension iwidth, iheight;

	_XmLabelGetPixmapSize(new_w,
			      (XtSensitive(new_w)
			       ? TBG_OnPixmap(new_w)
			       : TBG_InsenPixmap(new_w)),
			      &width, &height);

	_XmLabelGetPixmapSize(new_w, TBG_IndeterminatePixmap(new_w),
			      &iwidth, &iheight);
	if (width < iwidth)
	    width = iwidth;
	if (height < iheight)
	    height = iheight;

	if (LabG_TextRect_width(new_w) < width ||
	    LabG_TextRect_height(new_w) < height)
	{
	    if (LabG_TextRect_width(new_w) < width)
		LabG_TextRect_width(new_w) = width;
	    if (LabG_TextRect_height(new_w) < height)
		LabG_TextRect_height(new_w) = height;
	    if (!XtWidth(request) || !XtHeight(request))
	    {
		if (!XtWidth(request))
		    XtWidth(new_w) = 0;
		if (!XtHeight(request))
		    XtHeight(new_w) = 0;
		xmToggleButtonGadgetClassRec.rect_class.resize(new_w);
	    }
	}
    }

    if (TBG_IndicatorDim(new_w) == XmINVALID_DIMENSION)
    {
	TBG_IndicatorSet(new_w) = LabG_IsPixmap(new_w);
	TBG_IndicatorDim(new_w) = implicit_indicator(new_w);
    }
    else
    {
	TBG_IndicatorSet(new_w) = True;
    }

    CreateSelectGC(new_w);
    CreateBackgroundGC(new_w);

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRToggleMode),
			     TBG_ToggleMode(new_w), new_w))
	TBG_ToggleMode(new_w) = XmTOGGLE_BOOLEAN;

    if (TBG_IndType(new_w) == (unsigned char)XmUNSPECIFIED ||
	!XmRepTypeValidValue(XmRepTypeGetId(XmRIndicatorType),
			     TBG_IndType(new_w), new_w))
    {
	TBG_IndType(new_w) =
	    XmIsRowColumn(XtParent(new_w)) && RC_RadioBehavior(XtParent(new_w))
	    ? XmONE_OF_MANY
	    : XmN_OF_MANY;
    }

    if (TBG_Visible(new_w) == UNSPECIFIED_TBG_BOOLEAN)
	TBG_Visible(new_w) = !IN_MENU(new_w);

    if (TBG_FillOnSelect(new_w) == UNSPECIFIED_TBG_BOOLEAN)
	TBG_FillOnSelect(new_w) = (TBG_IndType(new_w) == XmN_OF_MANY
				   ? TBG_IndOn(new_w) & INDICATOR_BOX_MASK
				   : TBG_IndOn(new_w)) != 0;

    if (TBG_IndOn(new_w))
    {
	int margin;

	/* Make sure there's enough room on the side for the indicator */
	margin = (TBG_IndicatorDim(new_w) + TBG_Spacing(new_w))
	    - (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	       ? LabG_MarginLeft(new_w) : LabG_MarginRight(new_w));
	if (margin > 0)
	{
	    if (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R)
	    {
		LabG_MarginLeft(new_w) += margin;
		LabG_TextRect_x(new_w) += margin;
		LabG_AccTextRect(new_w).x += margin;
	    }
	    else
		LabG_MarginRight(new_w) += margin;
	    if (!XtWidth(request))
		XtWidth(new_w) += margin;
	}

	/* Make sure there's enough room vertically.
	 * Non-menu toggles want some padding space.
	 */
	margin = TBG_IndicatorDim(new_w) - (LabG_TextRect_height(new_w)
					    + LabG_MarginTop(new_w)
					    + LabG_MarginBottom(new_w));
#ifdef LESS_VERTICAL_PADDING
	margin -= LabG_MarginHeight(new_w) << 1;
#else
	if (!IN_MENU(new_w))
	    margin += (LabG_Shadow(new_w) + Xm3D_ENHANCE_PIXEL) << 1;
#endif
	if (margin > 0)
	{
	    LabG_MarginTop(new_w) += margin >> 1;
	    LabG_MarginBottom(new_w) += margin >> 1;
	    if (!XtHeight(request))
	    {
		LabG_TextRect_y(new_w) += margin >> 1;
		LabG_AccTextRect(new_w).y += margin >> 1;
		XtHeight(new_w) += margin;
	    }
	}
    }

    if (IN_MENU(new_w))
    {
	LabG_Highlight(new_w) = 0;
	if (LabG_Shadow(new_w) == 0)
	    LabG_Shadow(new_w) = 2;
	LabGClass_MenuProcs(XtClass(new_w)) = MenuProcEntry;
    }

    G_EventMask(new_w) = XmARM_EVENT | XmACTIVATE_EVENT | XmENTER_EVENT |
	XmLEAVE_EVENT | XmMOTION_EVENT | XmFOCUS_IN_EVENT |
	XmFOCUS_OUT_EVENT | XmMULTI_ARM_EVENT | XmMULTI_ACTIVATE_EVENT |
	XmHELP_EVENT | XmBDRAG_EVENT;
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, TBG_SelectGC(w));
    XtReleaseGC(w, TBG_BackgroundGC(w));
    XtReleaseGC(w, TBG_UnselectGC(w));

    _XmCacheDelete((XtPointer)TBG_Cache(w));
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
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = new_w;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache(new_w),
	   sizeof(XmLabelGCacheObjPart));
    memcpy(&((XmToggleButtonGCacheObject)nsec)->toggle_cache,
           TBG_Cache(new_w),
	   sizeof(XmToggleButtonGCacheObjPart));

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

    LabG_Cache(new_w) = &(((XmToggleButtonGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmToggleButtonGCacheObject)rsec)->label_cache);
    TBG_Cache(new_w) = &(((XmToggleButtonGCacheObject)nsec)->toggle_cache);
    TBG_Cache(request) = &(((XmToggleButtonGCacheObject)rsec)->toggle_cache);

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

    if (!_XmToggleBCacheCompare((XtPointer)TBG_Cache(new_w),
				(XtPointer)TBG_Cache(old)))
    {

	_XmCacheDelete((XtPointer)TBG_Cache(old));

	TBG_Cache(new_w) = (XmToggleButtonGCacheObjPart *)
	    _XmCachePart(TBG_ClassCachePart(NULL),
			 (XtPointer)TBG_Cache(new_w),
			 sizeof(XmToggleButtonGCacheObjPart));
    }
    else
    {
	TBG_Cache(new_w) = TBG_Cache(old);
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
	Boolean		refresh_needed = False;
	XGCValues	gcv;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "ToggleBG set_values\n"));

    if (TBG_IndType(old) != TBG_IndType(new_w) &&
	!XmRepTypeValidValue(XmRepTypeGetId(XmRIndicatorType),
			     TBG_IndType(new_w), new_w))
	TBG_IndType(new_w) = TBG_IndType(old);

    if (TBG_ToggleMode(old) != TBG_ToggleMode(new_w) &&
	!XmRepTypeValidValue(XmRepTypeGetId(XmRIndicatorType),
			     TBG_ToggleMode(new_w), new_w))
	TBG_ToggleMode(new_w) = TBG_ToggleMode(old);


    if (TBG_SelectColor(new_w) != TBG_SelectColor(old))
    {
	XtReleaseGC(new_w, TBG_SelectGC(new_w));
	CreateSelectGC(new_w);
	refresh_needed = True;
    }
	/* See bug #772755 : need to query the old widget's values */
	XGetGCValues(XtDisplayOfObject(old), LabG_NormalGC(old), GCBackground, &gcv);
/*
	if (XmParentBackground(new_w) != XmParentBackground(old))
 */
	if (XmParentBackground(new_w) != gcv.background) {
		XtReleaseGC(new_w, TBG_BackgroundGC(new_w));
		CreateBackgroundGC(new_w);
		refresh_needed = True;
	}

    /* Changes to the on or off pixmap */
    if (LabG_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	TBG_OnPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	LabG_Pixmap(new_w) = TBG_OnPixmap(new_w);

	if (LabG_IsPixmap(new_w) && XtSensitive(new_w))
	    refresh_needed = True;
    }

    if (LabG_PixmapInsensitive(new_w) == XmUNSPECIFIED_PIXMAP &&
	TBG_InsenPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	LabG_PixmapInsensitive(new_w) = TBG_InsenPixmap(new_w);

	if (LabG_IsPixmap(new_w) && !XtSensitive(new_w))
	    refresh_needed = True;
    }

    if (LabG_IsPixmap(new_w) && (LabG_RecomputeSize(new_w) ||
	TBG_IndeterminatePixmap(new_w) != XmUNSPECIFIED_PIXMAP ||
	(XtSensitive(new_w)
	 ? LabG_Pixmap(request) != LabG_Pixmap(old) ||
	   TBG_OnPixmap(new_w) != TBG_OnPixmap(old)
	 : LabG_PixmapInsensitive(request) != LabG_PixmapInsensitive(old) ||
	   TBG_InsenPixmap(new_w) != TBG_InsenPixmap(old))))
    {
	Dimension width, height;
	Dimension iwidth, iheight;

	_XmLabelGetPixmapSize(new_w,
			      (XtSensitive(new_w)
			       ? TBG_OnPixmap(new_w)
			       : TBG_InsenPixmap(new_w)),
			      &width, &height);
	_XmLabelGetPixmapSize(new_w, TBG_IndeterminatePixmap(new_w),
			      &iwidth, &iheight);
	if (width < iwidth)
	    width = iwidth;
	if (height < iheight)
	    height = iheight;

	if (LabG_TextRect_width(new_w) < width ||
	    LabG_TextRect_height(new_w) < height)
	{
	    if (LabG_TextRect_width(new_w) < width)
		LabG_TextRect_width(new_w) = width;
	    if (LabG_TextRect_height(new_w) < height)
		LabG_TextRect_height(new_w) = height;

	    if (LabG_RecomputeSize(new_w))
	    {
		if (XtWidth(request) == XtWidth(old))
		    XtWidth(new_w) = 0;
		if (XtHeight(request) == XtHeight(old))
		    XtHeight(new_w) = 0;
	    }
	    if (!XtWidth(request) || !XtWidth(new_w) ||
		!XtHeight(request) || !XtHeight(new_w))
	    {
		if (!XtWidth(request))
		    XtWidth(new_w) = 0;
		if (!XtHeight(request))
		    XtHeight(new_w) = 0;
		xmToggleButtonGadgetClassRec.rect_class.resize(new_w);
	    }

	    width = XtWidth(new_w);
	    height = XtHeight(new_w);
	    XtWidth(new_w) = XtWidth(old);
	    XtHeight(new_w) = XtHeight(old);
	    xmToggleButtonGadgetClassRec.rect_class.resize(new_w);
	    XtWidth(new_w) = width;
	    XtHeight(new_w) = height;
	}
    }

    if (TBG_IndicatorDim(new_w) == XmINVALID_DIMENSION)
	TBG_IndicatorSet(new_w) = False;

    if ((!TBG_IndicatorSet(new_w) &&
	 (TBG_IndicatorDim(new_w) == XmINVALID_DIMENSION ||
	  TBG_IndOn(new_w) != TBG_IndOn(old) ||
	  LabG_Label(new_w) != LabG_Label(old) ||
	  LabG_Font(new_w) != LabG_Font(old))) ||
	(TBG_IndicatorDim(new_w) == TBG_IndicatorDim(old)
	 && LabG_IsPixmap(new_w)
	 && LabG_TextRect_height(new_w) != LabG_TextRect_height(old)))
    {
	TBG_IndicatorDim(new_w) = implicit_indicator(new_w);
    }

    if (LabG_IsPixmap(new_w))
	TBG_IndicatorSet(new_w) = True;

    /* Adjust margins for the indicator size if necessary.
     * Margins be increased or decreased; if the margin was explicitly set
     * in this call, don't decrease it past that (though it can get bigger).
     */
    if (TBG_IndOn(new_w) &&
	(TBG_IndicatorDim(new_w) != TBG_IndicatorDim(old)
	 || TBG_Spacing(new_w) != TBG_Spacing(old)
	 || (!IN_MENU(new_w) && LabG_Shadow(new_w) != LabG_Shadow(old))
	 || LabG_StringDirection(new_w) != LabG_StringDirection(old)
	 || (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	     ? LabG_MarginLeft(new_w) != LabG_MarginLeft(old)
	     : LabG_MarginRight(new_w) != LabG_MarginRight(old))
	 || LabG_MarginTop(new_w) != LabG_MarginTop(old)
	 || LabG_MarginBottom(new_w) != LabG_MarginBottom(old)))
    {
	int margin, tm;

	margin = TBG_IndicatorDim(new_w) + TBG_Spacing(new_w)
	    - (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	       ? LabG_MarginLeft(new_w) : LabG_MarginRight(new_w));
	if (margin && (margin > 0 ||
	    (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	     ? LabG_MarginLeft(new_w) == LabG_MarginLeft(old)
	     : LabG_MarginRight(new_w) == LabG_MarginRight(old))))
	{
	    if (LabG_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R)
	    {
		LabG_MarginLeft(new_w) += margin;
		LabG_TextRect_x(new_w) += margin;
		LabG_AccTextRect(new_w).x += margin;
	    }
	    else
		LabG_MarginRight(new_w) += margin;
	    if (LabG_RecomputeSize(new_w) || !XtWidth(request))
		XtWidth(new_w) += margin;
	}

	margin = TBG_IndicatorDim(new_w) - (LabG_TextRect_height(new_w)
					    + LabG_MarginTop(new_w)
					    + LabG_MarginBottom(new_w));
#ifdef LESS_VERTICAL_PADDING
	margin -= LabG_MarginHeight(new_w) << 1;
#else
	if (!IN_MENU(new_w))
	    margin += (LabG_Shadow(new_w) + Xm3D_ENHANCE_PIXEL) << 1;
#endif
	if (margin)
	{
	    margin /= 2;
	    tm = margin;
	    if (tm < (LabG_MarginTop(new_w) == LabG_MarginTop(old)
		      ? XmDEFAULT_TOP_MARGIN - (int)LabG_MarginTop(new_w)
		      : 0))
		tm = (LabG_MarginTop(new_w) == LabG_MarginTop(old)
		      ? XmDEFAULT_TOP_MARGIN - (int)LabG_MarginTop(new_w)
		      : 0);
	    LabG_MarginTop(new_w) += tm;
	    if (LabG_RecomputeSize(new_w) || !XtHeight(request))
		XtHeight(new_w) += tm;

	    if (margin < (LabG_MarginBottom(new_w) == LabG_MarginBottom(old)
		      ? XmDEFAULT_BOTTOM_MARGIN - (int)LabG_MarginBottom(new_w)
		      : 0))
		margin = (LabG_MarginBottom(new_w) == LabG_MarginBottom(old)
		      ? XmDEFAULT_BOTTOM_MARGIN - (int)LabG_MarginBottom(new_w)
		      : 0);
	    LabG_MarginBottom(new_w) += margin;
	    if (LabG_RecomputeSize(new_w) || !XtHeight(request))
		XtHeight(new_w) += margin;

	    if (tm != margin)
	    {
		LabG_TextRect_y(new_w) += (tm - margin) / 2;
		LabG_AccTextRect(new_w).y += (tm - margin) / 2;
	    }
	}

	refresh_needed = True;
    }

    if (TBG_IndType(new_w) != TBG_IndType(old)
	|| TBG_DetailShadowThickness(new_w) != TBG_DetailShadowThickness(old)
	|| ((TBG_IndOn(new_w) || TBG_IndOn(old))
	    && (TBG_Visible(new_w) != TBG_Visible(old)
		|| TBG_FillOnSelect(new_w) != TBG_FillOnSelect(old))))
    {
	refresh_needed = True;
    }

    if (TBG_Set(old) != TBG_Set(new_w))
    {
	if (!refresh_needed)
	    draw_toggle(new_w, NULL, NULL, False, TBG_Set(new_w));
	else if ((TBG_Set(old) == XmINDETERMINATE ||
		  TBG_Set(new_w) == XmINDETERMINATE)
		 && TBG_SelectColor(new_w) == TBG_SelectColor(old))
	{
	    XtReleaseGC(new_w, TBG_SelectGC(new_w));
	    CreateSelectGC(new_w);
	}
	TBG_VisualSet(new_w) = TBG_Set(new_w);
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
    memcpy(&((XmToggleButtonGCacheObject)nsec)->toggle_cache,
           TBG_Cache(new_w),
	   sizeof(XmToggleButtonGCacheObjPart));

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
resize(Widget w)
{
    Position x;

    xmLabelGadgetClassRec.rect_class.resize(w);

    /* Make sure the label and toggle don't overlap */

    if (TBG_IndOn(w))
    {
	if (LabG_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R)
	{
	    x = LabG_Highlight(w) + LabG_Shadow(w) + LabG_MarginWidth(w)
		+ TBG_IndicatorDim(w) + TBG_Spacing(w);
	    if (LabG_TextRect_x(w) < x)
	    {
		LabG_AccTextRect(w).x += x - LabG_TextRect_x(w);
		LabG_TextRect_x(w) = x;
	    }
	}
	else
	{
	    x = XtWidth(w) - LabG_Highlight(w) - LabG_Shadow(w)
		- LabG_MarginWidth(w) - TBG_IndicatorDim(w) - TBG_Spacing(w)
		- LabG_TextRect_width(w);
	    if (LabG_TextRect_x(w) > x)
	    {
		LabG_AccTextRect(w).x -= LabG_TextRect_x(w) - x;
		LabG_TextRect_x(w) = x;
	    }
	}
    }
}

static void
expose(Widget w, XEvent *event, Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ToggleBG Expose\n"));
    draw_toggle(w, event, region, True, 0);
}

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIX ME */

    return _XmSecondaryResourceData(&_XmToggleBGRectClassExtRec,
				    data, NULL, NULL, NULL, NULL);
}

static void
Arm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;

    if (!TBG_Armed(w))
    {
	TBG_Armed(w) = True;
	MGR_ActiveChild(XtParent(w)) = w;

	if (TBG_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.set = TBG_Set(w);

	    XFlush(XtDisplay(w));
	    XtCallCallbackList(w, TBG_ArmCallback(w), (XtPointer)&cbs);
	}
    }
    draw_toggle(w, NULL, NULL, False, NEXT_TOGGLE(w));
}

static void
Select(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;
    XButtonEvent *ev = (XButtonEvent *)event;

    if (TBG_Armed(w) &&
	(ev->type == KeyPress || ev->type == KeyRelease ||
	 ((ev->x >= XtX(w) && ev->x < XtX(w) + XtWidth(w)) &&
	  (ev->y >= XtY(w) && ev->y < XtY(w) + XtHeight(w)))))
    {
	TBG_Armed(w) = False;

	TBG_Set(w) = TBG_VisualSet(w);

	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, NULL);

	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.event = event;
	cbs.set = TBG_Set(w);
	if (XmIsRowColumn(XtParent(w)))
	{
	    RC_MenuMenuCallback(w, &cbs);
	}
	/* Menu callback might cancel the set (radioAlwaysOne). */
	cbs.set = TBG_Set(w);
	if (!LabG_SkipCallback(w) && TBG_ValueChangedCallback(w))
	{
	    XFlush(XtDisplay(w));
	    XtCallCallbackList(w,
			       TBG_ValueChangedCallback(w),
			       (XtPointer)&cbs);
	}
    }
}


static void
Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;

    if (XtIsRealized(w))
	_XmClearBorder(XtDisplay(w), XtWindow(w),
		       XtX(w), XtY(w), XtWidth(w), XtHeight(w),
		       LabG_Shadow(w));

    if (TBG_Armed(w))
	TBG_Armed(w) = False;

    if (TBG_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.set = TBG_Set(w);

	XFlush(XtDisplay(w));
	XtCallCallbackList(w, TBG_DisarmCallback(w), (XtPointer)&cbs);
    }

    draw_toggle(w, NULL, NULL, False, TBG_Set(w));
}

static void
ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean poppedUp;

    Arm(w, event, params, num_params);

    if (IN_MENU(w))
    {
	RC_MenuButtonPopdown(w, event, &poppedUp);
    }

    Select(w, event, params, num_params);
    Disarm(w, event, params, num_params);
}

static void
ButtonUp(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean validButton, poppedUp;
    XmToggleButtonCallbackStruct cbs;

    XAllowEvents(XtDisplayOfObject(w), SyncPointer, CurrentTime);

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

    if (!TBG_Armed(w))
    {
	return;
    }

    RC_MenuButtonPopdown(w, event, &poppedUp);

    _XmRecordEvent(event);

    TBG_Armed(w) = False;

    if (XtIsRealized(w))
	_XmClearBorder(XtDisplay(w), XtWindow(w),
		       XtX(w), XtY(w), XtWidth(w), XtHeight(w),
		       LabG_Shadow(w));

    TBG_Set(w) = NEXT_TOGGLE(w);
    draw_toggle(w, NULL, NULL, False, TBG_Set(w));

    cbs.reason = XmCR_VALUE_CHANGED;
    cbs.event = event;
    cbs.set = TBG_Set(w);
    if (XmIsRowColumn(XtParent(w)))
    {
	RC_MenuMenuCallback(w, &cbs);
    }
    if (!LabG_SkipCallback(w) && TBG_ValueChangedCallback(w))
    {
	XFlush(XtDisplay(w));
	XtCallCallbackList(w, TBG_ValueChangedCallback(w), (XtPointer)&cbs);
    }
    if (TBG_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.set = TBG_Set(w);

	XFlush(XtDisplay(w));
	XtCallCallbackList(w, TBG_DisarmCallback(w), (XtPointer)&cbs);
    }

    _XmSetInDragMode(w, False);
}


static void
ButtonDown(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int validButton;
    XmToggleButtonCallbackStruct cbs;

    XAllowEvents(XtDisplayOfObject(w), SyncPointer, CurrentTime);

    if (event && (event->type == ButtonPress))
    {
	RC_MenuButton(w, event, &validButton);
	if (!validButton)
	    return;
    }

    _XmSetInDragMode(w, True);

    {
	Boolean poppedUp;

	RC_MenuShellPopdown(w, event, &poppedUp);
    }
    {
	Cardinal i;
	Widget menu = XtParent(w);

	for (i = 0; i < MGR_NumChildren(menu); i++)
	{
	    Widget w1 = MGR_Children(menu)[i];

	    if (w1 && (w1 != w))
	    {
		_XmMenuDisarmItem(w1);
	    }
	}
    }

    if (!TBG_Armed(w))
    {
	TBG_Armed(w) = True;

	if (XtIsRealized(w))
	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   XmParentTopShadowGC(w), XmParentBottomShadowGC(w),
			   XtX(w), XtY(w), XtWidth(w), XtHeight(w),
			   LabG_Shadow(w), XmSHADOW_OUT);

	if (TBG_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.set = TBG_Set(w);

	    XFlush(XtDisplay(w));
	    XtCallCallbackList(w, TBG_ArmCallback(w), (XtPointer)&cbs);
	}
    }

    _XmRecordEvent(event);
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
EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;

    if (!IN_MENU(w))
    {
	_XmEnterGadget(w, event, NULL, NULL);
	if (TBG_Armed(w))
	    draw_toggle(w, NULL, NULL, False, NEXT_TOGGLE(w));
    }
    else
    {
	if (_XmGetInDragMode(w))
	{
	    Boolean poppedUp;

	    RC_MenuShellPopdown(w, event, &poppedUp);

	    TBG_Armed(w) = True;

	    if (XtIsRealized(w))
		_XmDrawShadows(XtDisplay(w), XtWindow(w),
			       XmParentTopShadowGC(w),
			       XmParentBottomShadowGC(w),
			       XtX(w), XtY(w), XtWidth(w), XtHeight(w),
			       LabG_Shadow(w), XmSHADOW_OUT);

	    if (TBG_ArmCallback(w))
	    {
		cbs.reason = XmCR_ARM;
		cbs.event = event;
		cbs.set = TBG_Set(w);

		XFlush(XtDisplay(w));
		XtCallCallbackList(w, TBG_ArmCallback(w), (XtPointer)&cbs);
	    }
	}
    }
}

static void
LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;

    if (!IN_MENU(w))
    {
	_XmLeaveGadget(w, event, NULL, NULL);
	if (TBG_Armed(w))
	    draw_toggle(w, NULL, NULL, False, TBG_Set(w));
    }
    else
    {
	if (_XmGetInDragMode(w))
	{
	    TBG_Armed(w) = False;

	    if (XtIsRealized(w))
		_XmClearBorder(XtDisplay(w), XtWindow(w),
			       XtX(w), XtY(w), XtWidth(w), XtHeight(w),
			       LabG_Shadow(w));

	    if (TBG_DisarmCallback(w))
	    {
		cbs.reason = XmCR_DISARM;
		cbs.event = event;
		cbs.set = TBG_Set(w);

		XFlush(XtDisplay(w));
		XtCallCallbackList(w, TBG_DisarmCallback(w), (XtPointer)&cbs);
	    }
	}
    }
}

static void
input_dispatch(Widget gadget, XEvent *event, Mask event_mask)
{
    Cardinal num_params = 0;

    switch (event_mask)
    {
    case XmARM_EVENT:
    case XmMULTI_ARM_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget,
			  "ToggleButtonGadget got arm event\n"));
	if (IN_MENU(gadget))
	{
	    ButtonDown(gadget, event, NULL, &num_params);
	}
	else
	{
	    Arm(gadget, event, NULL, &num_params);
	}
	break;

    case XmACTIVATE_EVENT:
    case XmMULTI_ACTIVATE_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget,
			  "ToggleButtonGadget got activate event\n"));
	if (IN_MENU(gadget))
	{
	    ButtonUp(gadget, event, NULL, &num_params);
	}
	else
	{
	    Select(gadget, event, NULL, &num_params);
	    Disarm(gadget, event, NULL, &num_params);
	}
	break;

    case XmENTER_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget,
			  "ToggleButtonGadget enter window\n"));
	EnterWindow(gadget, event, NULL, &num_params);
	break;

    case XmLEAVE_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget,
			  "ToggleButtonGadget leave window\n"));
	LeaveWindow(gadget, event, NULL, &num_params);
	break;

    case XmMOTION_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget,
			  "ToggleButtonGadget motion event\n"));
	break;

    case XmFOCUS_IN_EVENT:
	_XmFocusInGadget(gadget, event, NULL, &num_params);
	break;

    case XmFOCUS_OUT_EVENT:
	_XmFocusOutGadget(gadget, event, NULL, &num_params);
	break;

    case XmHELP_EVENT:
	Help(gadget, event, NULL, &num_params);
	break;

    case XmBDRAG_EVENT:
	_XmProcessDrag(gadget, event, NULL, NULL);
	break;

    default:
	_XmWarning(gadget, "ToggleButtonGadget got unknown event\n");
    }
}

static void
MenuProcEntry(int proc, Widget w,...)
{
    va_list arg_list;
    /*
    Cardinal num_params = 0;
    */

    va_start(arg_list, w);

    switch (proc)
    {
    case XmMENU_ARM:
	{
	    Boolean poppedUp;

	    if (!TBG_Armed(w))
	    {
		RC_MenuShellPopdown(w, NULL, &poppedUp);

		TBG_Armed(w) = True;

		if (XtIsRealized(w))
		    _XmDrawShadows(XtDisplay(w), XtWindow(w),
				   XmParentTopShadowGC(w),
				   XmParentBottomShadowGC(w),
				   XtX(w), XtY(w), XtWidth(w), XtHeight(w),
				   LabG_Shadow(w), XmSHADOW_OUT);

		MGR_ActiveChild(XtParent(w)) = w;
		if (TBG_ArmCallback(w))
		{
		XmToggleButtonCallbackStruct cbs;

		    cbs.reason = XmCR_ARM;
		    cbs.event = NULL;
		    cbs.set = TBG_Set(w);

		    XFlush(XtDisplay(w));
		    XtCallCallbackList(w, TBG_ArmCallback(w), (XtPointer)&cbs);
		}
	    }
	}
	break;
    case XmMENU_DISARM:
	{
	    if (TBG_Armed(w))
	    {
		TBG_Armed(w) = False;

		if (XtIsRealized(w))
		    _XmClearBorder(XtDisplay(w), XtWindow(w),
				   XtX(w), XtY(w), XtWidth(w), XtHeight(w),
				   LabG_Shadow(w));

		MGR_ActiveChild(XtParent(w)) = w;
		if (TBG_DisarmCallback(w))
		{
		XmToggleButtonCallbackStruct cbs;

		    cbs.reason = XmCR_DISARM;
		    cbs.event = NULL;
		    cbs.set = TBG_Set(w);

		    XFlush(XtDisplay(w));
		    XtCallCallbackList(w, TBG_DisarmCallback(w), (XtPointer)&cbs);
		}
	    }
	}
	break;
    default:
	_XmWarning(w, "%s(%d) - Invalid menuProc function", __FILE__, __LINE__);
	break;
    }

    va_end(arg_list);
}

/* This may be called from expose, in which case everything is drawn,
 * but there's generally no reason to erase anything;
 * or it could be called by an action or such, in which case the only
 * things drawn are what got changed by toggling, but things may be
 * erased as well.
 */

static void
draw_toggle(Widget w, XEvent *event, Region region, int is_expose,
	    int visual_set)
{
    Dimension dim, delta, fill;
    Position x, y;
    Pixmap tmp_pix = XmUNSPECIFIED_PIXMAP;

    if (is_expose)
    {
	/* Called from expose: get visual_set and draw everything */
	visual_set = TBG_VisualSet(w);
    }
    else
    {
	/* Called from an action: set visual_set and draw some stuff */
	if (TBG_VisualSet(w) == visual_set)
	    return;
	if (TBG_VisualSet(w) == XmINDETERMINATE ||
	    visual_set == XmINDETERMINATE)
	{
	    /* FIX ME: Change the GC for a toggle?  That's ridiculous!
	     * Yet it seems necessary - there's no field available
	     * for an indeterminateGC.  - JHG
	     */
	    XtReleaseGC(w, TBG_SelectGC(w));
	    TBG_VisualSet(w) = visual_set;
	    CreateSelectGC(w);
	} else {
	    TBG_VisualSet(w) = visual_set;
	}
	if (!XtIsRealized(w))
	    return;
    }

    /* Fill in the widget only if toggling the background */
    if (LabG_IsText(w) && !TBG_IndOn(w) && TBG_FillOnSelect(w))
    {
	XFillRectangle(XtDisplay(w), XtWindow(w),
	    visual_set ? TBG_SelectGC(w) : TBG_BackgroundGC(w),
	    XtX(w) + LabG_Highlight(w) + LabG_Shadow(w),
	    XtY(w) + LabG_Highlight(w) + LabG_Shadow(w),
	    XtWidth(w) - ((LabG_Highlight(w) + LabG_Shadow(w)) << 1),
	    XtHeight(w) - ((LabG_Highlight(w) + LabG_Shadow(w)) << 1));
    }

    if (is_expose || (!TBG_IndOn(w) && TBG_FillOnSelect(w)) ||
	(LabG_IsPixmap(w) && (
	 TBG_IndeterminatePixmap(w) != XmUNSPECIFIED_PIXMAP ||
	 (XtSensitive(w) ? TBG_OnPixmap(w) :
	  TBG_InsenPixmap(w)) != XmUNSPECIFIED_PIXMAP)))
    {
	if (LabG_IsPixmap(w))
	{
	    if (visual_set)
	    {
		/* Switch pixmaps before drawing label */
		if (XtSensitive(w))
		{
		    if (visual_set == XmINDETERMINATE &&
			TBG_IndeterminatePixmap(w) != XmUNSPECIFIED_PIXMAP) {
			tmp_pix = LabG_Pixmap(w);
			LabG_Pixmap(w) = TBG_IndeterminatePixmap(w);
		    } else if (TBG_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
			tmp_pix = LabG_Pixmap(w);
			LabG_Pixmap(w) = TBG_OnPixmap(w);
		    }
		} else {
		    if (visual_set == XmINDETERMINATE &&
			TBG_IndeterminatePixmap(w) != XmUNSPECIFIED_PIXMAP) {
			tmp_pix = LabG_Pixmap(w);
			LabG_Pixmap(w) = TBG_IndeterminatePixmap(w);
		    } else if (TBG_InsenPixmap(w) != XmUNSPECIFIED_PIXMAP)
		    {
			tmp_pix = LabG_PixmapInsensitive(w);
			LabG_PixmapInsensitive(w) = TBG_InsenPixmap(w);
		    }
		}
	    }
	    if (!is_expose && (TBG_IndOn(w) || !TBG_FillOnSelect(w))) {
		XRectangle cliprect;

		/* Changing pixmaps: erase the TextRect just in case
		 * they're different sizes.  Take a bit of trouble here
		 * to avoid excess drawing, which causes more work later.
		 * Based on similar work in Label's expose.
		 */
		cliprect.x =
		    LabG_Highlight(w) + LabG_Shadow(w) + LabG_MarginLeft(w);
		cliprect.y =
		    LabG_Highlight(w) + LabG_Shadow(w) + LabG_MarginTop(w);
		cliprect.width  = XtWidth(w)
		    - ((LabG_Shadow(w) + LabG_Highlight(w)) << 1)
		    - LabG_MarginLeft(w) - LabG_MarginRight(w);
		cliprect.height = XtHeight(w)
		    - ((LabG_Shadow(w) + LabG_Highlight(w)) << 1)
		    - LabG_MarginTop(w) - LabG_MarginBottom(w);
		if (cliprect.x + cliprect.width > LabG_TextRect_x(w) &&
		    cliprect.x < LabG_TextRect_x(w) + LabG_TextRect_width(w) &&
		    cliprect.y + cliprect.height > LabG_TextRect_y(w) &&
		    cliprect.y < LabG_TextRect_y(w) + LabG_TextRect_height(w))
		{
		    if (cliprect.x < LabG_TextRect_x(w))
		    {
			cliprect.width -= LabG_TextRect_x(w) - cliprect.x;
			cliprect.x = LabG_TextRect_x(w);
		    }
		    if (cliprect.x + cliprect.width >
			LabG_TextRect_x(w) + LabG_TextRect_width(w))
		    {
			cliprect.width = (LabG_TextRect_x(w)
			    + LabG_TextRect_width(w)) - cliprect.x;
		    }
		    if (cliprect.y < LabG_TextRect_y(w))
		    {
			cliprect.height -= LabG_TextRect_y(w) - cliprect.y;
			cliprect.y = LabG_TextRect_y(w);
		    }
		    if (cliprect.y + cliprect.height >
			LabG_TextRect_y(w) + LabG_TextRect_height(w))
		    {
			cliprect.height = (LabG_TextRect_y(w)
			    + LabG_TextRect_height(w)) - cliprect.y;
		    }
		    XFillRectangle(XtDisplay(w), XtWindow(w),
				   TBG_BackgroundGC(w),
				   XtX(w) + cliprect.x,
				   XtY(w) + cliprect.y,
				   cliprect.width, cliprect.height);
		}
	    }
	}
#define superclass (&xmLabelGadgetClassRec)
	(*superclass->rect_class.expose) (w, event, region);
#undef superclass
	if (LabG_IsPixmap(w) && visual_set)
	{
	    /* Switch pixmaps back after drawing label */
	    if (tmp_pix != XmUNSPECIFIED_PIXMAP)
	    {
		if (XtSensitive(w))
		{
		    LabG_Pixmap(w) = tmp_pix;
		}
		else
		{
		    LabG_PixmapInsensitive(w) = tmp_pix;
		}
	    }
	}
    }

    if (IN_MENU(w))
    {
	if (is_expose)
	{
	    /* Exposing in a menu: draw or erase shadows */
	    if (TBG_Armed(w))
	    {
		_XmDrawShadows(XtDisplay(w), XtWindow(w),
			       XmParentTopShadowGC(w),
			       XmParentBottomShadowGC(w),
			       XtX(w), XtY(w), XtWidth(w), XtHeight(w),
			       LabG_Shadow(w), XmSHADOW_OUT);
	    }
	    else
	    {
		_XmClearBorder(XtDisplay(w), XtWindow(w),
			       XtX(w), XtY(w), XtWidth(w), XtHeight(w),
			       LabG_Shadow(w));
	    }
	}
    }
    else
    {
	if (is_expose || !TBG_IndOn(w))
	{
	    /* Non-menu: draw shadows in or out */
	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   visual_set == XmINDETERMINATE
			    ? LabG_InsensitiveGC(w)
			    : XmParentTopShadowGC(w),
			   visual_set == XmINDETERMINATE
			    ? LabG_InsensitiveGC(w)
			    : XmParentBottomShadowGC(w),
			   XtX(w) + LabG_Highlight(w),
			   XtY(w) + LabG_Highlight(w),
			   XtWidth(w) - (LabG_Highlight(w) << 1),
			   XtHeight(w) - (LabG_Highlight(w) << 1),
			   LabG_Shadow(w), visual_set && !TBG_IndOn(w)
			   ? XmSHADOW_IN : XmSHADOW_OUT);
	}
    }

    if (TBG_IndOn(w) && (visual_set || !is_expose || TBG_Visible(w)))
    {
	/* Draw (or erase) the indicator */

	x = XtX(w) + LabG_Highlight(w) + LabG_Shadow(w) + LabG_MarginWidth(w);
	dim = TBG_IndicatorDim(w);

	if (TBG_IndicatorSet(w) || !LabG_TextRect_height(w))
	{
	    /* Center indicator on label */
	    y = (LabG_MarginTop(w) << 1) + XtHeight(w)
		- LabG_MarginTop(w) - LabG_MarginBottom(w) - dim;
	}
	else
	{
	    /* Make sure implicit indicator isn't too big to fit.
	     * Align it with top line of text.
	     */
	    y = LabG_TextRect_y(w) << 1;
	    if (IN_MENU(w))
	    {
		/* For the smaller menu indicators.
		 * The "true" formula is (fullsize - smaller) / 2,
		 * but that would require the _XmString calls every time.
		 * This formula is much quicker and usually correct,
		 * with only 1 pixel rounded up 1/6 of the time.
		 * You see, I couldn't sleep, and had nothing better to do...
		 */
		y += (dim + 1) >> 1;
	    }
	    if (dim > XtHeight(w) -
		((LabG_Highlight(w) + LabG_Shadow(w) + LabG_MarginHeight(w))
		 >> 1) - LabG_MarginTop(w) - LabG_MarginBottom(w))
		dim = XtHeight(w) - ((LabG_Highlight(w) + LabG_Shadow(w)
		      + LabG_MarginHeight(w)) >> 1)
		    - LabG_MarginTop(w) - LabG_MarginBottom(w);
	}

	/* Monochrome displays (or anything with the select color the same
	 * as a shadow) get inset by one pixel to make things easier to see.
	 */
	fill = XmParentTopShadowColor(w) != TBG_SelectColor(w) &&
	    XmParentBottomShadowColor(w) != TBG_SelectColor(w);

	if (TBG_IndType(w) == XmN_OF_MANY)
	{
	    /* Make square indicators a bit smaller then they really are */
	    delta = dim <= SQUARE_INDICATOR_DEC + Xm3D_ENHANCE_PIXEL
		? dim >> 1
		: SQUARE_INDICATOR_DEC +
		  (dim >= SQUARE_INDICATOR_ELBOW << 1
		   ? (dim - SQUARE_INDICATOR_ELBOW) / SQUARE_INDICATOR_ELBOW
		   : 0);
	    x += delta >> 1;
	    y += delta;
	    dim -= delta;
	}
	y = XtY(w) + (y >> 1);

	if (LabG_StringDirection(w) == XmSTRING_DIRECTION_R_TO_L)
	    x = XtWidth(w) - x - dim;

	switch (TBG_IndType(w))
	{
	case XmN_OF_MANY:
	    if (visual_set || TBG_Visible(w))
	    {
		if (TBG_IndOn(w) & INDICATOR_BOX_MASK)
		{
		    _XmDrawShadows(XtDisplay(w), XtWindow(w),
				   visual_set == XmINDETERMINATE
				   ? LabG_InsensitiveGC(w)
				   : (TBG_IndOn(w) & INDICATOR_BOX_MASK)
				     == XmINDICATOR_FLAT_BOX
				     ? XmParentBottomShadowGC(w)
				     : XmParentTopShadowGC(w),
				   visual_set == XmINDETERMINATE
				   ? LabG_InsensitiveGC(w)
				   : XmParentBottomShadowGC(w),
				   x, y, dim, dim, DETAIL_SHADOW_THICKNESS(w),
				   visual_set ? XmSHADOW_IN : XmSHADOW_OUT);
		    delta = DETAIL_SHADOW_THICKNESS(w) + (1 - fill);
		} else {
		    delta = 0;
		}
		if (dim > delta << 1) {
		    if (is_expose || TBG_FillOnSelect(w)) {
			XFillRectangle(XtDisplay(w), XtWindow(w),
				       visual_set && TBG_FillOnSelect(w)
				       ? TBG_SelectGC(w) : TBG_BackgroundGC(w),
				       x + delta, y + delta,
				       dim - (delta << 1), dim - (delta << 1));
		    }
		    if (visual_set == XmUNSET) {
				XFillRectangle(XtDisplayOfObject(w),
					XtWindowOfObject(w),
					TBG_UnselectGC(w),
					x, y, dim, dim);

				_XmDrawShadows(XtDisplay(w), XtWindow(w),
				   visual_set == XmINDETERMINATE
				   ? LabG_InsensitiveGC(w)
				   : (TBG_IndOn(w) & INDICATOR_BOX_MASK)
				   == XmINDICATOR_FLAT_BOX
				   ? XmParentBottomShadowGC(w)
				   : XmParentTopShadowGC(w),
				   visual_set == XmINDETERMINATE
				   ? LabG_InsensitiveGC(w)
				   : XmParentBottomShadowGC(w),
				   x, y, dim, dim, DETAIL_SHADOW_THICKNESS(w),
				   visual_set ? XmSHADOW_IN : XmSHADOW_OUT);
			}
		}
	    } else {
		XFillRectangle(XtDisplay(w), XtWindow(w),
			       TBG_BackgroundGC(w), x, y, dim, dim);
	    }
	    break;

	case XmONE_OF_MANY_ROUND:
	    XmeDrawCircle(XtDisplay(w), XtWindow(w),
		visual_set ? XmParentBottomShadowGC(w) : TBG_Visible(w)
		  ? XmParentTopShadowGC(w) : TBG_BackgroundGC(w),
		visual_set ? XmParentTopShadowGC(w) : TBG_Visible(w)
		  ? XmParentBottomShadowGC(w) : TBG_BackgroundGC(w),
		(is_expose || TBG_FillOnSelect(w))
		? visual_set && TBG_FillOnSelect(w)
		  ? TBG_SelectGC(w) : TBG_BackgroundGC(w) : NULL,
		x, y, dim, dim, TBG_DetailShadowThickness(w), 1);
		break;

	default: /* XmONE_OF_MANY[_DIAMOND] */
	    _XmDrawDiamond(XtDisplay(w), XtWindow(w),
		visual_set ? XmParentBottomShadowGC(w) :
			TBG_Visible(w) ? XmParentTopShadowGC(w) : TBG_BackgroundGC(w),
		visual_set ? XmParentTopShadowGC(w) :
			TBG_Visible(w) ? XmParentBottomShadowGC(w) : TBG_BackgroundGC(w),
		(is_expose || TBG_FillOnSelect(w))
			? (visual_set && TBG_FillOnSelect(w))
				? TBG_SelectGC(w)
				: TBG_UnselectGC(w)
			: NULL,
		x, y, dim, dim, DETAIL_SHADOW_THICKNESS(w),
			   !fill);
	}
    }
}

static int
implicit_indicator(Widget w)
{
    int dim;

    /* For a text button, the default indicator size is the height of
     * the first line of text.  For a pixmap, it's related to the pixmap
     * height.  For small pixmaps, just make it the same size.
     * Why 13 for size & slope?  You got me. -- JHG
     */

    if (LabG_IsText(w))
    {
	dim = _XmStringHeight(LabG_Font(w), LabG_Label(w))
	    / _XmStringLineCount(LabG_Label(w));
	if (IN_MENU(w))
	{
	    /* Menu indicators are a bit smaller */
	    dim = (dim << 1) / 3;
	}
	if (dim < XmDEFAULT_INDICATOR_DIM)
	    dim = XmDEFAULT_INDICATOR_DIM;
	return dim;
    }
    else
    {
	return LabG_TextRect_height(w) < PIXMAP_INDICATOR_ELBOW
	    ? LabG_TextRect_height(w)
	    : PIXMAP_INDICATOR_ELBOW +
	      LabG_TextRect_height(w) / PIXMAP_INDICATOR_ELBOW;
    }
}

Widget
XmCreateToggleButtonGadget(Widget parent, char *name,
			   Arg *arglist, Cardinal argcount)
{
    Widget w;

    _XmObjectLock(parent);
    w = XtCreateWidget(name, xmToggleButtonGadgetClass, parent,
		       arglist, argcount);

    _XmObjectUnlock(parent);
    return w;
}

Boolean
XmToggleButtonGadgetGetState(Widget w)
{
    Boolean r;

    _XmObjectLock(w);
    r = XmIsToggleButtonGadget(w)
	? TBG_Set(w)
	: XmIsToggleButton(w)
	? XmToggleButtonGetState(w)
	: False;

    _XmObjectUnlock(w);
    return r;
}

void
XmToggleButtonGadgetSetState(Widget w, Boolean state, Boolean notify)
{
    XmToggleButtonCallbackStruct cbs;

    _XmObjectLock(w);
    if (XtIsWidget(w))
    {
	XmToggleButtonSetState(w, state, notify);
	_XmObjectUnlock(w);
	return;
    }
    if (XmIsToggleButtonGadget(w) && TBG_Set(w) != state)
    {
	TBG_Set(w) = state;
	draw_toggle(w, NULL, NULL, False, state);
	if (notify)
	{
	    cbs.reason = XmCR_VALUE_CHANGED;
	    cbs.event = NULL;
	    cbs.set = TBG_Set(w);
	    if (XmIsRowColumn(XtParent(w)))
	    {
		RC_MenuMenuCallback(w, &cbs);
	    }
	    /* Menu callback might cancel the set (radioAlwaysOne). */
	    cbs.set = TBG_Set(w);
	    if (!LabG_SkipCallback(w) && TBG_ValueChangedCallback(w))
	    {
		XFlush(XtDisplay(w));
		XtCallCallbackList(w, TBG_ValueChangedCallback(w),
				   (XtPointer)&cbs);
	    }
	}
    }
    _XmObjectUnlock(w);
}

static void
_XmUnselectColorDefault(Widget w, int offset, XrmValue *val)
{
	/*
	 * XmNunselectColor
	 * This resource's default for a color display is XmNbackground.
	 * For a monochrome display, the default is set to the background color.
	 * To set the background of the button to XmNunselectColor when
	 * XmNindicatorOn is XmINDICATOR_NONE, the value of XmNfillOnSelect must
	 * be explicitly set to True.
	 */
	val->size = sizeof(Pixel);
	val->addr = (XtPointer)&CoreBackground(w);

#define XmParentBackgroundPixel(w) (((XmManagerWidget) \
        (((XmGadget)(w))->object.parent))->core.background_pixel)

	val->addr = (XtPointer)&XmParentBackgroundPixel(w);
#if 0
	if (DefaultDepth(XtDisplay(w), 0) == 1) {	/* Mono */
	} else {					/* Colour */
	}
#endif

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmUnselectColorDefault(%p)\n",
		*(Pixel *)(val->addr)));
}
