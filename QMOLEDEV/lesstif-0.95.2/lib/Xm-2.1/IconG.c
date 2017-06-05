/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/IconG.c,v 1.2 2006/04/19 18:42:22 dannybackx Exp $
 *
 * Copyright © 1997, 1998, 1999, 2000, 2001, 2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/IconG.c,v 1.2 2006/04/19 18:42:22 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <XmI/XmI.h>
#include <XmI/PixConvI.h>

#include <X11/ShellP.h>

#include <Xm/XmP.h>
#include <Xm/CacheP.h>
#include <Xm/IconGP.h>
#include <Xm/XmosP.h>

#include <Xm/ContItemT.h>
#include <Xm/ContainerP.h>
#include <Xm/ContainerT.h>
#include <Xm/CareVisualT.h>

#include <XmI/DebugUtil.h>


#ifndef IG_Font
#define IG_Font(w)	(((XmIconGadget)(w))->icong.cache->render_table)
#endif

/* Forward Declarations */

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);
static void get_values_hook(Widget w, ArgList args, Cardinal *num_args);
static void input_dispatch(Widget gadget, XEvent *event, Mask event_mask);
static Boolean SetValues(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void secondary_object_create(Widget request, Widget new_w,
				    ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget request,
			       Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request,
				Widget new_w, ArgList args, Cardinal *num_args);
static Boolean set_values_prehook(Widget old, Widget request, Widget new_w,
				  ArgList args, Cardinal *num_args);
static Boolean set_values_posthook(Widget old, Widget request, Widget new_w,
				   ArgList args, Cardinal *num_args);
static void get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args);
static void get_values_posthook(Widget new_w, ArgList args, Cardinal *num_args);
static Cardinal get_sec_res_data(WidgetClass wc,
				 XmSecondaryResourceData **data);
static void export_label_string(Widget w, int offset, XtArgVal *value);


static int _XmIconCacheCompare(XtPointer A, XtPointer B);
static void _XmCalcIconGDimensions(Widget w);

/*
 * Container Item trait
 */
static void _ContainerItemGetValues(Widget w, XmContainerItemData d);
static void _ContainerItemSetValues(Widget w, XmContainerItemData d);

static XmContainerItemTraitRec _ContainerItemTrait = {
	/* version */		0,
	/* setvalues */		_ContainerItemSetValues,
	/* getvalues */		_ContainerItemGetValues
};

/*
 * CareVisual Trait
 */
static Boolean _IgTrait_Redraw(Widget, Widget, Widget, Mask);

static XmCareVisualTraitRec _IgCvTraitRec = {
	/* version */		0,
	/* redraw */		_IgTrait_Redraw
};

/*
 * resources
 */
#define Offset(field) XtOffsetOf(XmIconGCacheObjRec, icon_cache.field)
static XtResource cache_resources[] =
{
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(render_table),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNrenderTable, XmCRenderTable, XmRRenderTable,
	sizeof(XmRenderTable), Offset(render_table),
	XmRImmediate, (XtPointer)NULL
    },
    {
        XmNbackground, XmCBackground, XmRPixel,
        sizeof(Pixel), Offset(background),
        XmRCallProc, (XtPointer)_XmBackgroundColorDefault
    },
    {
        XmNforeground, XmCForeground, XmRPixel,
        sizeof(Pixel), Offset(foreground),
        XmRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {   
	XmNtopShadowColor, XmCTopShadowColor, XmRPixel,
	sizeof(Pixel), Offset(top_shadow_color),
	XmRCallProc, (XtPointer)_XmTopShadowColorDefault
    },
    {
	XmNbottomShadowColor, XmCBottomShadowColor, XmRPixel,
	sizeof(Pixel), Offset(bottom_shadow_color), 
	XmRCallProc, (XtPointer)_XmBottomShadowColorDefault
    },
    {
	XmNhighlightColor, XmCHighlightColor, XmRPixel,
	sizeof(Pixel), Offset(highlight_color),
	XmRCallProc, (XtPointer)_XmHighlightColorDefault
    },
    {
        XmNbackgroundPixmap, XmCPixmap, XmRXmBackgroundPixmap,
        sizeof(Pixmap), Offset(background_pixmap),
        XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNtopShadowPixmap, XmCTopShadowPixmap, XmRTopShadowPixmap,
	sizeof(Pixmap), Offset(top_shadow_pixmap),
	XmRCallProc, (XtPointer)_XmTopShadowPixmapDefault
    },
    {
        XmNbottomShadowPixmap, XmCBottomShadowPixmap, XmRBottomShadowPixmap,
        sizeof(Pixmap), Offset(bottom_shadow_pixmap),
        XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
        XmNhighlightPixmap, XmCHighlightPixmap, XmRHighlightPixmap,
        sizeof(Pixmap), Offset(highlight_pixmap),
        XmRCallProc, (XtPointer)_XmHighlightPixmapDefault
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNspacing, XmCSpacing, XmRDimension,
	sizeof(Dimension), Offset(spacing),
	XmRImmediate, (XtPointer)4
    },
    {
	XmNalignment, XmCAlignment, XmRUnsignedChar,
	sizeof(unsigned char), Offset(alignment),
	XmRImmediate, (XtPointer)XmALIGNMENT_CENTER
    },
};


XmIconGCacheObjClassRec xmIconGCacheObjClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmExtClassRec,
        /* class_name            */ "XmIconGCacheObjClass",
	/* widget_size           */ sizeof(XmIconGCacheObjRec),
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
        /* syn_resources      */ NULL,
        /* num_syn_resources  */ 0,
        /* extension          */ NULL
    },
    /* IconGCacheObj part */
    {
	/* foo                */ 0
    }
};

#undef Offset
#define Offset(field) XtOffsetOf(XmIconGadgetRec, icong.field)

/* Resources for the icon class */
static XtResource resources[] = {
    {
	XmNlabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(label_string),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNlargeIconMask, XmCIconMask, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(large_icon_mask),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNlargeIconPixmap, XmCIconPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(large_icon_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNsmallIconMask, XmCIconMask, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(small_icon_mask),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNsmallIconPixmap, XmCIconPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(small_icon_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNviewType, XmCViewType, XmRViewType,
	sizeof(unsigned char), Offset(view_type),
	XmRImmediate, (XtPointer)XmLARGE_ICON
    },
    {
	XmNvisualEmphasis, XmCVisualEmphasis, XmRVisualEmphasis,
	sizeof(unsigned char), Offset(visual_emphasis),
	XmRImmediate, (XtPointer)XmNOT_SELECTED
    },
    {
	XmNdetail, XmCDetail, XmRStringTable,
	sizeof(XmStringTable), Offset(detail),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdetailCount, XmCDetailCount, XmRCardinal,
	sizeof(Cardinal), Offset(detail_count),
	XmRImmediate, (XtPointer)0
    },
    {   /* This is interesting */
	".cache", ".Cache", XmRPointer,
	sizeof(XtPointer), Offset(cache),
	XmRImmediate, (XtPointer)NULL
    },
};


static XmSyntheticResource syn_resources[] = {
    {
	XmNlabelString,
	sizeof(XmString), Offset(label_string),
	export_label_string, NULL
    }
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
    _XmIconCacheCompare
};

static XmBaseClassExtRec _XmIconGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ initialize_prehook,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmIconGCacheObjClassRec,
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

static XmGadgetClassExtRec _XmIconGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ NULL,
    /* display_rect_proc         */ NULL,
    /* margins proc              */ NULL
};

XmIconGadgetClassRec xmIconGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmGadgetClassRec,
	/* class_name            */ "XmIconGadget",
	/* widget_size           */ sizeof(XmIconGadgetRec),
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
	/* compress_motion       */ False,
	/* compress_exposure     */ XtExposeNoCompress,
	/* compress_enterleave   */ False,
 	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ SetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ get_values_hook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmIconGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight, 
	/* border_unhighlight */ XmInheritBorderUnhighlight, /* FIX ME */
	/* arm_and_activate   */ NULL,
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ NULL,
	/* syn_resources      */ syn_resources,
	/* num_syn_resources  */ XtNumber(syn_resources),
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&_XmIconGadgetClassExtRec
    },
    /* XmIconGadget part */
    {
	/* extension          */ NULL,
	                         NULL
    },
};

WidgetClass xmIconGadgetClass = (WidgetClass)&xmIconGadgetClassRec;

/******************************* GADGET PART *********************************/
static void
secondary_object_create(Widget request, Widget new_w,
			ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XtPointer nsec, rsec;
    XmWidgetExtData ed;
    int size;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "IconGCacheRec %s being initialized.\n",
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

#if 0
    LabG_Cache(new_w) = &(((XmLabelGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmLabelGCacheObject)rsec)->label_cache);
#endif

    IG_Cache(new_w) = &(((XmIconGCacheObject)nsec)->icon_cache);
    IG_Cache(request) = &(((XmIconGCacheObject)rsec)->icon_cache);
}


static int
_XmIconCacheCompare(XtPointer A, XtPointer B)
{
    return !memcmp(((XmIconGCacheObjPart *)A),
		 ((XmIconGCacheObjPart *)B),
		 sizeof(XmIconGCacheObjPart));
}

/******************************* GADGET PART *********************************/
static void
class_initialize(void)
{
    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(IG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(IG_ClassCachePart(NULL));
    ClassCacheHead(IG_ClassCachePart(NULL)).next =
	&ClassCacheHead(IG_ClassCachePart(NULL));

    _XmIconGRectClassExtRec.record_type = XmQmotif;

    /*
     * Label subclasses (ToggleBG, PushBG, CascadeBG) have a problem.  Since
     * we do all the subpart manipulation in the pre- and post- hooks, and
     * since those hooks aren't chained, we have to either make multiple
     * calls to XtGetSubresources/Xt[Get|Set]Subvalues, or merge the resource
     * lists.  Since I just wrote _XmTransformSubresources, seems like a
     * waste not to use it.
     *
     * STUFF DELETED.
     */

    if (! XmeTraitSet((XtPointer)xmIconGadgetClass, XmQTcontainerItem,
			(XtPointer)&_ContainerItemTrait)) {
	_XmWarning(NULL, "XmIconGadget ClassInitialize: XmeTraitSet failed\n");
    }
    if (! XmeTraitSet((XtPointer)xmIconGadgetClass, XmQTcareParentVisual,
			(XtPointer)&_IgCvTraitRec)) {
	_XmWarning(NULL, "XmIconGadget ClassInitialize: XmeTraitSet failed\n");
    }
}


static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmICON_GADGET_BIT);
}

static void
CreateNormalGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = XmParentForeground(w);
    values.background = XmParentBackground(w);
    values.fill_style = FillSolid;

    IG_NormalGC(w) = XtGetGC(w, mask, &values);
}

/* FIX ME */
static void
CreateSelectedGC(Widget w, Pixel sc)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask | GCLineWidth;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    if (sc == XmREVERSED_GROUND_COLORS) {
	values.foreground = XmParentBackground(w);
	values.background = XmParentForeground(w);
    } else {
	values.foreground = sc;
	values.background = XmParentBackground(w);
    }
    values.fill_style = FillSolid;
    values.line_width = 2;

    IG_SelectedGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateBackgroundGC(Widget w)
{
	XGCValues values;
	XtGCMask mask;

	mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
		GCSubwindowMode | GCGraphicsExposures | GCPlaneMask | GCLineWidth;
	values.function = GXcopy;
	values.plane_mask = -1;
	values.subwindow_mode = ClipByChildren;
	values.graphics_exposures = False;
	values.foreground = XmParentBackground(w);
	values.background = XmParentForeground(w);
	values.fill_style = FillSolid;
	values.line_width = 2;

	IG_BackgroundGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_prehook(Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "IconG InitializePrehook\n"));
}

static void
initialize_posthook(Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
	XmWidgetExtData ext;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "IconG InitializePosthook\n"));

	IG_Cache(new_w) = (XmIconGCacheObjPart *)_XmCachePart(
		IG_ClassCachePart(NULL),
		(XtPointer)IG_Cache(new_w),
		sizeof(XmIconGCacheObjPart));

	_XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);
	_XmExtObjFree((XtPointer)ext->widget);
	_XmExtObjFree((XtPointer)ext->reqWidget);
	XtFree((char *)ext);
}

static void
_XmCalcIconGDimensions(Widget w)
{
	Dimension TextHeight, TextWidth;
	unsigned int IconHeight, IconWidth;
	unsigned int tmp;
	int tmpx,tmpy;
	Window tmpwin;
	unsigned Depth;


	DEBUGOUT(_LtDebug(__FILE__, w, "_XmCalcIconGDimensions\n"));

	if (_XmStringIsXmString((XmString)IG_LabelString(w))) {
		IG_LabelString(w) =
			(XmString)_XmStringCreate((XmString)IG_LabelString(w));
	}

	_XmStringExtent(IG_Font(w),
		(_XmString) IG_LabelString(w),
		&TextWidth, &TextHeight);

	DEBUGOUT(_LtDebug(__FILE__, w, "\t Text width %d height %d\n", TextWidth, TextHeight));

	/* Danny 28/8/2001 try to make this look at two types of Pixmap,
	 * see E-mails on mailing list about Xplore 1.1 */
	if (IG_ViewType(w) == XmSMALL_ICON && IG_SmallIconPixmap(w) != XmUNSPECIFIED_PIXMAP) {
		XGetGeometry(XtDisplayOfObject(w), IG_SmallIconPixmap(w),
			&tmpwin, &tmpx, &tmpy,
			&IconWidth, &IconHeight,
			&tmp, &Depth);
	} else if (IG_ViewType(w) == XmLARGE_ICON && IG_LargeIconPixmap(w) != XmUNSPECIFIED_PIXMAP) {
		XGetGeometry(XtDisplayOfObject(w), IG_LargeIconPixmap(w),
			&tmpwin, &tmpx, &tmpy,
			&IconWidth, &IconHeight,
			&tmp, &Depth);
	} else {
		IconWidth = IconHeight = 0;
	}

	DEBUGOUT(_LtDebug(__FILE__, w, "\t Icon width %d height %d\n", IconWidth, IconHeight));

	XtWidth(w) = TextWidth + IconWidth
		+ 2 * IG_Spacing(w) + 2 * IG_MarginWidth(w)
		+ 2 * (G_ShadowThickness(w) + G_HighlightThickness(w));
	XtHeight(w) = (TextHeight > IconHeight) ? TextHeight : IconHeight
		+ 2 * IG_Spacing(w) + 2 * IG_MarginHeight(w)
		+ 2 * G_HighlightThickness(w);

	DEBUGOUT(_LtDebug(__FILE__, w, "\t width %d height %d\n", XtWidth(w), XtHeight(w)));
}


static void
initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
	XmContainerTrait	t;
	XmContainerDataRec	r;

	DEBUGOUT(_LtDebug(__FILE__, new_w,
		"initialize: %i args\n"
		"\trequest X %5i Y %5i W %5i H %5i\n"
		"\t  new_w X %5i Y %5i W %5i H %5i\n",
		*num_args,
		XtX(request), XtY(request),
		XtWidth(request), XtHeight(request),
		XtX(new_w), XtY(new_w),
		XtWidth(new_w), XtHeight(new_w)));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

	/* get the default fontlist if the label was created without one. */
	/* this should come from the renderTable (whatever that is??) FIXME */
	if (IG_Font(new_w) == (XmFontList)XmUNSPECIFIED || IG_Font(new_w) == NULL) {
		IG_Font(new_w) = _XmGetDefaultFontList(new_w, XmLABEL_FONTLIST);
	} else {
		/* if the user specified one, COPY it */
		IG_Font(new_w) = XmFontListCopy(IG_Font(new_w));
	}

	/* If the label was not initialized with the resource labelString set,
	 * use its name -- the follow _XmString code comes from MegaButton */
	if (IG_LabelString(new_w) == (XmString)XmUNSPECIFIED
			|| IG_LabelString(new_w) == (XmString)0) {
			/* Shouldn't be necessary but is */
		XmString xmstring;

		xmstring = _XmOSGetLocalizedString((char *)NULL,
			(Widget)new_w,
			XmNlabelString,
			XtName(new_w));

		IG_LabelString(new_w) = (XmString) _XmStringCreate(xmstring);
		XmStringFree(xmstring);
	}

	if (_XmStringIsXmString((XmString)IG_LabelString(new_w))) {
		IG_LabelString(new_w) = (XmString) _XmStringCreate(
			(XmString)IG_LabelString(new_w));
	}

	if (IG_LargeIconPixmap(new_w) != XmUNSPECIFIED_PIXMAP &&
		IG_LargeIconMask(new_w) == XmUNSPECIFIED_PIXMAP) {
	}

	if (IG_SmallIconPixmap(new_w) != XmUNSPECIFIED_PIXMAP &&
		IG_SmallIconMask(new_w) == XmUNSPECIFIED_PIXMAP) {
	}

	if (IG_RenderTable(new_w) != (XmRenderTable)NULL) {
	}

	if (XmIsContainer(XtParent(new_w)) &&
			(ContainerEntryViewType(XtParent(new_w)) == XmLARGE_ICON ||
			 ContainerEntryViewType(XtParent(new_w)) == XmSMALL_ICON)) {
		IG_ViewType(new_w) = ContainerEntryViewType(XtParent(new_w));
	}

	/* Use the trait to query our (container) parent */
	t = (XmContainerTrait)XmeTraitGet((XtPointer)XtClass(XtParent(new_w)),
		XmQTcontainer);
	if (t) {
		r.valueMask = ContSelectColor;
		t->getValues(XtParent(new_w), &r);
	} else {
		_XmWarning(new_w, "Parent doesn't implement XmQTcontainer trait\n");
	}

	/* have to check request since new_w may have been polluted by a
	 * superclass 
	 */
	if (XtWidth(request) == (Dimension)0) {
		XtWidth(new_w) = 0;
	}
	if (XtHeight(request) == (Dimension)0) {
		XtHeight(new_w) = 0;
	}

	_XmCalcIconGDimensions(new_w);
	resize(new_w);

	/*
	 * the user might have wanted something.  Change it back, if so
	 */
	if (XtWidth(request) != 0) {
		XtWidth(new_w) = XtWidth(request);
	}
	if (XtHeight(request) != 0) {
		XtHeight(new_w) = XtHeight(request);
	}

	/* surge protection */
	if (XtWidth(new_w) == 0) {
		XtWidth(new_w) = 1;
	}
	if (XtHeight(new_w) == 0) {
		XtHeight(new_w) = 1;
	}

	CreateNormalGC(new_w);
	CreateBackgroundGC(new_w);

	if (r.valueMask & ContSelectColor)
		CreateSelectedGC(new_w, r.select_color);
	else
		CreateSelectedGC(new_w, XmParentForeground(new_w));

	if (!XtIsSubclass(XtParent(new_w), xmManagerWidgetClass)) {
		_XmError(new_w, "parent should be manager.");
	}
}


static void
destroy(Widget w)
{
    _XmStringFree((_XmString)IG_LabelString(w));
    XmFontListFree(IG_Font(w));
    XtReleaseGC(w, IG_NormalGC(w));
    XtReleaseGC(w, IG_BackgroundGC(w));
    if (IG_SelectedGC(w)!=NULL)
       XtReleaseGC(w, IG_SelectedGC(w));
    
    _XmCacheDelete((XtPointer)IG_Cache(w));
}


static void
resize(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w,"resize\n"));

    if (!XmIsLabelGadget(w)) {
	return;
    }
}


static Boolean
set_values_prehook(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    int size;
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

    memcpy(&((XmIconGCacheObject)nsec)->icon_cache,
           IG_Cache(new_w),
	   sizeof(XmIconGCacheObjPart));

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

    IG_Cache(new_w) = &(((XmIconGCacheObject)nsec)->icon_cache);
    IG_Cache(request) = &(((XmIconGCacheObject)rsec)->icon_cache);

    _XmExtImportArgs((Widget)nsec, args, num_args);

    return False;
}

static Boolean
set_values_posthook(Widget old, Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    XmWidgetExtData ext;

    if (!_XmIconCacheCompare((XtPointer)IG_Cache(new_w),
			      (XtPointer)IG_Cache(old)))
    {

	_XmCacheDelete((XtPointer)IG_Cache(old));

	IG_Cache(new_w) = (XmIconGCacheObjPart *)
	    _XmCachePart(IG_ClassCachePart(NULL),
			 (XtPointer)IG_Cache(new_w),
			 sizeof(XmIconGCacheObjPart));
    }
    else
    {
	IG_Cache(new_w) = IG_Cache(old);
    }

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);

    XtFree((char *)ext);

    return False;
}

/*
 * FIX ME
 */
static Boolean
SetValues(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
	Boolean refresh_needed = False;

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

	if (IG_VisualEmphasis(new_w) != IG_VisualEmphasis(old)) {
		refresh_needed = True;
	}
	if (IG_ViewType(new_w) != IG_ViewType(old)) {
		refresh_needed = True;
	}
	if (IG_DetailCount(new_w) != IG_DetailCount(old)) {
		refresh_needed = True;
	}

	return refresh_needed;
}


static void
get_values_prehook(Widget new_w, ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    int size;
    XtPointer nsec;

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    memcpy(&((XmIconGCacheObject)nsec)->icon_cache,
           IG_Cache(new_w),
	   sizeof(XmIconGCacheObjPart));

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
get_values_hook(Widget w, ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "GetValuesHook\n"));
}

static void
expose(Widget w, XEvent *event, Region region)
{
	Position TextX, TextY, rectx, recty, shx, shy;
	Dimension TextHeight, TextWidth, rectwid, rectht, shw, shh;
	Position IconX, IconY;
	unsigned int IconHeight, IconWidth;
	XRectangle cliprect;
	GC myGC;
	unsigned int tmp;
	int tmpx,tmpy;
	Window tmpwin;
	unsigned Depth;
	unsigned char vt;

	DEBUGOUT(_LtDebug(__FILE__, w, "expose\n"));

	if (!XtIsRealized(w))
		return;

	/* if the internals of the widget haven't been recomputed yet,
	 * recompute them now (happens if resize isn't called after set_values)
	 */
	resize(w);

	if (IG_VisualEmphasis(w) == XmSELECTED)
		myGC = IG_SelectedGC(w);
	else
		myGC = IG_NormalGC(w);

	/* Set a clip rectangle for the GC - ensure we don't overwrite shadows */
	/* the rectangle used to include MarginWidth and MarginHeight.  */
	cliprect.x = XtX(w) 
		+ G_HighlightThickness(w) + G_ShadowThickness(w);
	cliprect.y = XtY(w)
		+ G_HighlightThickness(w) /*+ G_ShadowThickness(w)*/;
	cliprect.width = XtWidth(w)
		+ 2 * IG_MarginWidth(w)
		- 2 * (/* G_ShadowThickness(w) + */ G_HighlightThickness(w));
	cliprect.height = XtHeight(w)
		+ 2 * IG_MarginHeight(w)
		- 2 * (/*G_ShadowThickness(w) +*/ G_HighlightThickness(w));

	XSetClipRectangles(XtDisplay(w), myGC, 0, 0, &cliprect, 1, Unsorted);

	if (IG_ViewType(w) == XmSMALL_ICON && IG_SmallIconPixmap(w) != XmUNSPECIFIED_PIXMAP) {
		XGetGeometry(XtDisplayOfObject(w), IG_SmallIconPixmap(w), &tmpwin,
			&tmpx, &tmpy, &IconWidth, &IconHeight, &tmp, &Depth);
	} else if (IG_ViewType(w) == XmLARGE_ICON && IG_LargeIconPixmap(w) != XmUNSPECIFIED_PIXMAP) {
		XGetGeometry(XtDisplayOfObject(w), IG_LargeIconPixmap(w), &tmpwin,
			&tmpx, &tmpy, &IconWidth, &IconHeight, &tmp, &Depth);
	} else {
		IconWidth = IconHeight = 0;
	}

	_XmStringExtent(IG_Font(w), (_XmString) IG_LabelString(w), &TextWidth, &TextHeight);

	IconX = XtX(w) + IG_MarginWidth(w)
		+ G_HighlightThickness(w) + G_ShadowThickness(w);
	IconY = ((XtHeight(w) - IconHeight) / 2) + XtY(w)
		+ IG_MarginHeight(w);
	TextX = IconX + IconWidth + G_ShadowThickness(w);
	TextY = ((XtHeight(w) - TextHeight) / 2) + XtY(w)
		+ IG_MarginHeight(w);

	/* Calculate the shadow geometry */
	shx = TextX - (G_ShadowThickness(w) + G_HighlightThickness(w));
	shy = TextY - (G_ShadowThickness(w) + G_HighlightThickness(w));
	shw = TextWidth + 2 * (G_ShadowThickness(w) + G_HighlightThickness(w));
	shh = TextHeight + 2 * (G_ShadowThickness(w) + G_HighlightThickness(w));

	/* The ViewType by itself is not enough, we need to make sure that we have an icon */

	if (IG_ViewType(w) == XmLARGE_ICON && IG_LargeIconPixmap(w) == XmUNSPECIFIED_PIXMAP)
		vt = XmANY_ICON;
	else if (IG_ViewType(w) == XmSMALL_ICON && IG_SmallIconPixmap(w) == XmUNSPECIFIED_PIXMAP)
		vt = XmANY_ICON;
	else
		vt = IG_ViewType(w);

	switch (vt) {
	default:
		rectx = shx;
		recty = shy;
		rectwid = shw;
		rectht = shh;
		break;
	case XmLARGE_ICON:
	case XmSMALL_ICON:
		rectx = IconX - 2;
		recty = IconY - 2;
		rectwid = TextWidth + IconWidth
			+ 2 * (G_ShadowThickness(w) + G_HighlightThickness(w))
			+ 4;
		rectht = ((IconHeight > TextHeight) ? IconHeight : TextHeight) + 4;
		break;
	}


	if (IG_ViewType(w) == XmSMALL_ICON && IG_SmallIconPixmap(w) != XmUNSPECIFIED_PIXMAP) {
		XCopyArea(XtDisplay(w), IG_SmallIconPixmap(w), XtWindow(w), myGC,
			0, 0, IconWidth, IconHeight, IconX, IconY);
	} else if (IG_ViewType(w) == XmLARGE_ICON
			&& IG_LargeIconPixmap(w) != XmUNSPECIFIED_PIXMAP) {
		XCopyArea(XtDisplay(w), IG_LargeIconPixmap(w), XtWindow(w), myGC,
			0, 0, IconWidth, IconHeight, IconX, IconY);
	} else {
		/* ?? */
	}

	_XmStringDraw(XtDisplayOfObject(w),
		XtWindowOfObject(w),
		IG_Font(w),
		(_XmString) IG_LabelString(w),
		myGC,
		TextX,
		TextY,
		TextWidth,
		XmALIGNMENT_BEGINNING,
		0,
		NULL);
	_XmDrawShadows(XtDisplayOfObject(w),
		XtWindowOfObject(w),
		XmParentTopShadowGC(w),
		XmParentBottomShadowGC(w),
		shx, shy,
		shw, shh,
		G_ShadowThickness(w),
		XmSHADOW_OUT);

	XSetClipMask(XtDisplay(w), myGC, None);

	/* Do this after restoring the ClipMask */

	if (IG_VisualEmphasis(w) == XmSELECTED) {
		XDrawRectangle(XtDisplayOfObject(w), XtWindowOfObject(w), myGC,
			rectx,
			recty,
			rectwid,
			rectht);
	} else {
		/* Probably need to erase the rectangle */
		/* FIX ME */
		XDrawRectangle(XtDisplayOfObject(w), XtWindowOfObject(w), IG_BackgroundGC(w),
			rectx,
			recty,
			rectwid,
			rectht);
	}
}


static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    XtWidgetGeometry a;		/* Standin for answer if NULL parameter */
    Dimension wd, ht;

	memset(&a, 0, sizeof(XtWidgetGeometry));

#define	Wants(x)	(proposed->request_mode & x)

    DEBUGOUT(_LtDebug(__FILE__, w, "query_geometry %s\n",
		      _LtDebugWidgetGeometry2String(proposed)));

    wd = XtWidth(w);
    ht = XtHeight(w);

    if (proposed->request_mode & CWWidth)
    {
	XtWidth(w) = proposed->width;
    }
    if (proposed->request_mode & CWHeight)
    {
	XtHeight(w) = proposed->height;
    }

    _XmCalcIconGDimensions(w);

    a.width = XtWidth(w);
    a.height = XtHeight(w);
    a.request_mode = CWWidth | CWHeight;
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "IconGadget queried for size: reporting %d %d %08x\n",
		      XtWidth(w), XtHeight(w), answer));

    XtWidth(w) = wd;
    XtHeight(w) = ht;

    if (answer)
    {
	*answer = a;
    }

    if ((proposed->request_mode & (CWWidth | CWHeight)) ==
	(CWWidth | CWHeight) &&
	proposed->width >= answer->width && proposed->height >= answer->height)
    {
	return XtGeometryYes;
    }
    else if (answer->width == XtWidth(w) && answer->height == XtHeight(w))
    {
	return XtGeometryNo;
    }
    else
    {
	return XtGeometryAlmost;
    }
}


static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIX ME */

    return _XmSecondaryResourceData(&_XmIconGRectClassExtRec,
				    data, NULL, NULL, NULL, NULL);
}

static void
input_dispatch(Widget gadget, XEvent *event, Mask event_mask)
{
    DEBUGOUT(_LtDebug(__FILE__, gadget, "input_dispatch\n"));
}


static void
export_label_string(Widget w, int offset, XtArgVal *value)
{
    _XmString str = *(_XmString *)(((char *)w) + offset);

    *value = str
        ? (XtArgVal)_XmStringCreateExternal(IG_Font(w), str)
        : (XtArgVal)NULL;
}


Widget
XmCreateIconGadget(Widget parent, char *name, Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name, xmIconGadgetClass, parent,
			  arglist, argcount);
}

static void _ContainerItemGetValues(Widget w, XmContainerItemData d)
{
	XmIconGadget	ig = (XmIconGadget)w;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmIconGadget _ContainerItemGetValues\n"));

	if (d->valueMask & ContItemViewType) {
		d->view_type = IG_ViewType(w);
	}
	if (d->valueMask & ContItemVisualEmphasis) {
		d->visual_emphasis = IG_VisualEmphasis(w);
	}
	if (d->valueMask & ContItemIconWidth) {
		/* FIX ME not sure which resource to read here */
		d->icon_width = 0;
	}
	if (d->valueMask & ContItemDetailCount) {
		d->detail_count = IG_DetailCount(w);
	}
}

/*
 * Trait function "SetValues" for ContainerItem trait.
 * FIX ME the implementation is currently a quick hack.
 */
static void _ContainerItemSetValues(Widget w, XmContainerItemData d)
{
	Arg	al[4];
	int	ac = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmIconGadget _ContainerItemSetValues\n"));

	if (d->valueMask & ContItemViewType) {
		XtSetArg(al[ac], XmNviewType, d->view_type);
		ac++;
	}
	if (d->valueMask & ContItemVisualEmphasis) {
		XtSetArg(al[ac], XmNvisualEmphasis, d->visual_emphasis);
		ac++;
	}
	if (d->valueMask & ContItemIconWidth) {
		/* FIX ME not sure what to set ?? */
#if 0
		XtSetArg(al[ac], XmNiconWidth, d->icon_width);
		ac++;
#endif
	}
	if (d->valueMask & ContItemDetailCount) {
		XtSetArg(al[ac], XmNdetailCount, d->detail_count);
		ac++;
	}

	XtSetValues(w, al, ac);
}

/*
 * Motif widgets that depend on their parent's appearance are alerted of an appearance
 * change in the parent by the XmQTcareParentVisual trait.
 * All Manager widgets must notify their children (holding this trait) when the visual
 * appearance of the manager parent changes.
 * Subclasses of XmManager need not implement this as they inherit this behaviour.
 *
 * A return value of True means that the child needs to be redrawn.
 *
 * Note that this does NOT work for XmNselectColor.
 */
static Boolean
_IgTrait_Redraw(Widget child, Widget newp, Widget curp, Mask flag)
{
	XtExposeProc	e;
	Boolean		r = True;

	e = ((XmIconGadgetClassRec *)XtClass(child))->rect_class.expose;

	DEBUGOUT(_LtDebug2(__FILE__, newp, child, "IconGadget CareParentVisualRedraw()\n"));

	if (flag & VisualForeground) {
	}
	if (flag & VisualHighlightPixmap) {
	}
	if (flag & VisualHighlightColor) {
	}
	if (flag & VisualBottomShadowPixmap) {
	}
	if (flag & VisualBottomShadowColor) {
	}
	if (flag & VisualSelectColor) {	/* Note XmManager doesn't do this */
	}
	if (flag & VisualTopShadowPixmap) {
	}
	if (flag & VisualTopShadowColor) {
	}
	if (flag & VisualBackgroundPixel) {
		/* FIX ME do we need to get info from MGR_* here ? */
		r = True;
	}
	if (flag & VisualBackgroundPixmap) {
		/* FIX ME do we need to get info from MGR_* here ? */
		r = True;
	}

	if (r && e) {
		(*e)(child, NULL, NULL);
		return False;
	} else {
		return r;
	}
}
