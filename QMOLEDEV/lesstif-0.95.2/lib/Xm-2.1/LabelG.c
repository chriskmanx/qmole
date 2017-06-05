/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/LabelG.c,v 1.8 2008/01/02 19:42:57 dannybackx Exp $
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/LabelG.c,v 1.8 2008/01/02 19:42:57 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/LabelP.h>
#include <Xm/LabelGP.h>
#include <Xm/RowColumnP.h>
#include <Xm/DragDrop.h>
#include <Xm/MenuShellP.h>
#include <Xm/RepType.h>
#include <Xm/CacheP.h>
#include <Xm/XmosP.h>
#include <Xm/ScreenP.h>
#include <Xm/DragIconP.h>
#include <XmI/AtomMgrI.h>

#include <XmI/DebugUtil.h>


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

#if 0
static void get_values_hook(Widget w, ArgList args, Cardinal *num_args);
#endif

static void input_dispatch(Widget gadget, XEvent *event, Mask event_mask);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void secondary_object_create(Widget request, Widget new_w,
				    ArgList args, Cardinal *num_args);

#if 0
static void initialize_prehook(Widget request, Widget new_w,
			       ArgList args, Cardinal *num_args);
#endif

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
static Boolean widget_baseline(Widget w, Dimension **baselines,
			       int *nbaselines);
static Boolean widget_display_rect(Widget w, XRectangle *rect);
static void widget_margins (Widget w, XmBaselineMargins *margins);
static void set_override_callback(Widget w);
static void export_label_string(Widget w, int offset, XtArgVal *value);
static void drag_drop_finish(Widget w,
			     XtPointer client_data,
			     XtPointer call_data);
static Boolean drag_convert_proc(Widget w, Atom *selection,
				 Atom *target, Atom *type_return,
				 XtPointer *value_return,
				 unsigned long *length_return,
				 int *format_return);
static void preferred_size(Widget w, Dimension *width, Dimension *height);
static void _XmLabelGSetRenderTable(Widget, int, XrmValue *);

/*
 * resources
 */
#define Offset(field) XtOffsetOf(XmLabelGCacheObjRec, label_cache.field)
static XtResource cache_resources[] =
{
    {
	XmNlabelType, XmCLabelType, XmRLabelType,
	sizeof(unsigned char), Offset(label_type),
	XmRImmediate, (XtPointer)XmSTRING
    },
    {
	XmNalignment, XmCAlignment, XmRAlignment,
	sizeof(unsigned char), Offset(alignment),
	XmRImmediate, (XtPointer)XmALIGNMENT_CENTER
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNmarginLeft, XmCMarginLeft, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_left),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginRight, XmCMarginRight, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_right),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginTop, XmCMarginTop, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_top),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNmarginBottom, XmCMarginBottom, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_bottom),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNrecomputeSize, XmCRecomputeSize, XmRBoolean,
	sizeof(Boolean), Offset(recompute_size),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNstringDirection, XmCStringDirection, XmRStringDirection,
	sizeof(XmStringDirection), Offset(string_direction),
	XmRImmediate, (XtPointer)((XmStringDirection)XmUNSPECIFIED)
    }
};

static XmSyntheticResource cache_syn_resources[] =
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
    },
    {
	XmNmarginLeft,
	sizeof(Dimension), Offset(margin_left),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginRight,
	sizeof(Dimension), Offset(margin_right),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginTop,
	sizeof(Dimension), Offset(margin_top),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNmarginBottom,
	sizeof(Dimension), Offset(margin_bottom),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
};

XmLabelGCacheObjClassRec xmLabelGCacheObjClassRec =
{
    /* Object class part */
    {
	/* superclass            */ (WidgetClass)&xmExtClassRec,
	/* class_name            */ "XmLabelGCacheObjClass",
	/* widget_size           */ sizeof(XmLabelGCacheObjRec),
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
    }
};

#undef Offset
#define Offset(field) XtOffsetOf(XmLabelGadgetRec, label.field)

/* Resources for the label class */
static XtResource resources[] =
{
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmLabelGadgetRec, gadget.shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNlabelPixmap, XmCLabelPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNlabelInsensitivePixmap, XmCLabelInsensitivePixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(pixmap_insen),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNlabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(_label),
	XmRImmediate, (XtPointer)NULL
    },
    {
	/*
	 * Note !!
	 * Order is important. This resource must be specified before the
	 * XmNfontList and XmNrenderTable, otherwise the check_set_render_table
	 * flag is not initialised before use in _XmLabelGSetRenderTable().
	 */
	"keep.off", "Keep.off", XmRBoolean,
	sizeof(Boolean), Offset(check_set_render_table),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(font),
	XmRCallProc, (XtPointer)_XmLabelGSetRenderTable
    },
    {
	XmNrenderTable, XmCRenderTable, XmRRenderTable,
	sizeof(XmFontList), Offset(font),
	XmRCallProc, (XtPointer)_XmLabelGSetRenderTable
    },
    /* End of fontlist / rendertable */
    {
	XmNmnemonic, XmCMnemonic, XmRKeySym,
	sizeof(KeySym), Offset(mnemonic),
	XmRImmediate, (XtPointer)NoSymbol
    },
    {
	XmNmnemonicCharSet, XmCMnemonicCharSet, XmRString,
	sizeof(String), Offset(mnemonicCharset),
	XmRImmediate, (XtPointer)XmFONTLIST_DEFAULT_TAG
    },
    {
	XmNaccelerator, XmCAccelerator, XmRString,
	sizeof(String), Offset(accelerator),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNacceleratorText, XmCAcceleratorText, XmRXmString,
	sizeof(XmString), Offset(_acc_text),
	XmRImmediate, (XtPointer)NULL
    },
    /* Things we override from Gadget */
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmLabelGadgetRec, gadget.traversal_on),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmLabelGadgetRec, gadget.highlight_thickness),
	XmRImmediate, (XtPointer)0
    },
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNlabelString,
	sizeof(XmString), Offset(_label),
	export_label_string, NULL
    },
    {
	XmNaccelerator,
	sizeof(String), Offset(accelerator),
	_XmExportString, NULL
    },
    {
	XmNacceleratorText,
	sizeof(XmString), Offset(_acc_text),
	export_label_string, NULL
    },
    {
	XmNmnemonicCharSet,
	sizeof(String), Offset(mnemonicCharset),
	_XmExportString, NULL
    }
};

static XmBaseClassExtRec _XmLabelGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ XmInheritInitializePrehook /*initialize_prehook*/,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmLabelGCacheObjClassRec,
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
	/* next		*/ NULL,
	/* prev		*/ NULL,
	/* ref_count	*/ 0
    },
    _XmCacheCopy,
    _XmCacheDelete,
    _XmLabelCacheCompare
};

static XmGadgetClassExtRec _XmLabelGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline           */ widget_baseline,
    /* widget_display_rect       */ widget_display_rect,
#if XmVERSION >= 2
    /* margins_proc              */ widget_margins,
#endif

};

XmLabelGadgetClassRec xmLabelGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmGadgetClassRec,
	/* class_name            */ "XmLabelGadget",
	/* widget_size           */ sizeof(XmLabelGadgetRec),
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
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost, /* FIX ME */
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmLabelGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight,
	/* border_unhighlight */ XmInheritBorderUnhighlight,
	/* arm_and_activate   */ NULL,
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ XmInheritVisualChange, /* FIX ME */
	/* syn_resources      */ syn_resources,
	/* num_syn_resources  */ XtNumber(syn_resources),
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&_XmLabelGadgetClassExtRec
    },
    /* XmLabelGadget part */
    {
        /* setOverrideCallback */ set_override_callback,
        /* menuProcs           */ NULL,
	/* extension           */ NULL
    },
};


WidgetClass xmLabelGadgetClass = (WidgetClass)&xmLabelGadgetClassRec;


/******************************** CACHE PART *********************************/
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
		      "LabelGCacheRec %s being initialized.\n", XtName(new_w)));

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
}

int
_XmLabelCacheCompare(XtPointer A, XtPointer B)
{
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmLabelGadget CacheCompare()\n"));
	return !memcmp(((XmLabelGCacheObjPart *)A), ((XmLabelGCacheObjPart *)B),
		sizeof(XmLabelGCacheObjPart));
}

void
_XmCalcLabelGDimensions(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmCalcLabelGDimensions\n"));

    if (LabG_IsText(w))
    {
	_XmStringExtent(LabG_Font(w), LabG_Label(w), &LabG_TextRect_width(w),
			&LabG_TextRect_height(w));
	if (!LabG_TextRect_width(w))
	{
	    /* This used to be in _XmStringExtent, but fits better here */

	    LabG_TextRect_height(w) = 0;
	}
    }
    else
    {
    Pixmap mypm;

	mypm = XtIsSensitive(w) ? LabG_Pixmap(w) : LabG_PixmapInsensitive(w);
#if XmVERSION > 1
	if (mypm == XmUNSPECIFIED_PIXMAP)
	{
	    mypm = LabG_Pixmap(w);
	}
#endif

	_XmLabelGetPixmapSize(w, mypm,
			      &LabG_TextRect_width(w),
			      &LabG_TextRect_height(w));
    }

    if (LabG_AcceleratorText(w)) {
	_XmStringExtent(LabG_Font(w), LabG_AcceleratorText(w),
			&LabG_AccTextRect(w).width,
			&LabG_AccTextRect(w).height);
	if (!LabG_AccTextRect(w).width) {
	    LabG_AccTextRect(w).height = 0;
	}
    } else {
	    /* No accelerator text -> no dimensions */
	    LabG_AccTextRect(w).height = 0;
	    LabG_AccTextRect(w).width = 0;
	    LabG_AccTextRect(w).x = 0;
	    LabG_AccTextRect(w).y = 0;
    }
}

void
_XmReCacheLabG(Widget w)
{
    XmWidgetExtData ext;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmReCacheLabG\n"));
    _XmPopWidgetExtData(w, &ext, XmCACHE_EXTENSION);

    LabG_Cache(w) = (XmLabelGCacheObjPart *)
	_XmCachePart(LabG_ClassCachePart(NULL),
		     (XtPointer)LabG_Cache(w),
		     sizeof(XmLabelGCacheObjPart));

    _XmExtObjFree((XtPointer)ext->widget);

    XtFree((char *)ext);
}

void
_XmAssignLabG_MarginHeight(XmLabelGadget lw, Dimension value)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)lw, "_XmAssignLabG_MarginHeight\n"));

    if ((ed = _XmGetWidgetExtData((Widget)lw, XmCACHE_EXTENSION)) != NULL)
    {
	LabG_MarginHeight(lw) = value;
    }

    bce = _XmGetBaseClassExtPtr(XtClass((Widget)lw), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent((Widget)lw);
    ((XmExtRec *)nsec)->object.xrm_name = ((Widget)lw)->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = (Widget)lw;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache((Widget)lw),
	   sizeof(XmLabelGCacheObjPart));

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;

    _XmPushWidgetExtData((Widget)lw, ed, XmCACHE_EXTENSION);

    _XmCacheDelete((XtPointer)LabG_Cache(lw));

    LabG_Cache((Widget)lw) = &(((XmLabelGCacheObject)nsec)->label_cache);

    LabG_MarginHeight(lw) = value;
}

void
_XmAssignLabG_MarginWidth(XmLabelGadget lw, Dimension value)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)lw, "_XmAssignLabG_MarginWidth\n"));

    if ((ed = _XmGetWidgetExtData((Widget)lw, XmCACHE_EXTENSION)) != NULL)
    {
	LabG_MarginWidth(lw) = value;
    }

    bce = _XmGetBaseClassExtPtr(XtClass((Widget)lw), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent((Widget)lw);
    ((XmExtRec *)nsec)->object.xrm_name = ((Widget)lw)->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = (Widget)lw;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache((Widget)lw),
	   sizeof(XmLabelGCacheObjPart));

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;

    _XmPushWidgetExtData((Widget)lw, ed, XmCACHE_EXTENSION);

    LabG_Cache((Widget)lw) = &(((XmLabelGCacheObject)nsec)->label_cache);

    LabG_MarginWidth(lw) = value;
}

void
_XmAssignLabG_MarginLeft(XmLabelGadget lw, Dimension value)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)lw, "_XmAssignLabG_MarginLeft\n"));

    if ((ed = _XmGetWidgetExtData((Widget)lw, XmCACHE_EXTENSION)) != NULL)
    {
	LabG_MarginLeft(lw) = value;
    }

    bce = _XmGetBaseClassExtPtr(XtClass((Widget)lw), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent((Widget)lw);
    ((XmExtRec *)nsec)->object.xrm_name = ((Widget)lw)->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = (Widget)lw;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache((Widget)lw),
	   sizeof(XmLabelGCacheObjPart));

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;

    _XmPushWidgetExtData((Widget)lw, ed, XmCACHE_EXTENSION);

    LabG_Cache((Widget)lw) = &(((XmLabelGCacheObject)nsec)->label_cache);

    LabG_MarginLeft(lw) = value;
}

void
_XmAssignLabG_MarginRight(XmLabelGadget lw, Dimension value)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)lw, "_XmAssignLabG_MarginRight\n"));

    if ((ed = _XmGetWidgetExtData((Widget)lw, XmCACHE_EXTENSION)) != NULL)
    {
	LabG_MarginRight(lw) = value;
    }

    bce = _XmGetBaseClassExtPtr(XtClass((Widget)lw), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent((Widget)lw);
    ((XmExtRec *)nsec)->object.xrm_name = ((Widget)lw)->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = (Widget)lw;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache((Widget)lw),
	   sizeof(XmLabelGCacheObjPart));

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;

    _XmPushWidgetExtData((Widget)lw, ed, XmCACHE_EXTENSION);

    LabG_Cache((Widget)lw) = &(((XmLabelGCacheObject)nsec)->label_cache);

    LabG_MarginRight(lw) = value;
}

void
_XmAssignLabG_MarginTop(XmLabelGadget lw, Dimension value)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)lw, "_XmAssignLabG_MarginTop\n"));

    if ((ed = _XmGetWidgetExtData((Widget)lw, XmCACHE_EXTENSION)) != NULL)
    {
	LabG_MarginTop(lw) = value;
    }

    bce = _XmGetBaseClassExtPtr(XtClass((Widget)lw), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent((Widget)lw);
    ((XmExtRec *)nsec)->object.xrm_name = ((Widget)lw)->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = (Widget)lw;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache((Widget)lw),
	   sizeof(XmLabelGCacheObjPart));

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;

    _XmPushWidgetExtData((Widget)lw, ed, XmCACHE_EXTENSION);

    LabG_Cache((Widget)lw) = &(((XmLabelGCacheObject)nsec)->label_cache);

    LabG_MarginTop(lw) = value;
}

void
_XmAssignLabG_MarginBottom(XmLabelGadget lw, Dimension value)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)lw, "_XmAssignLabG_MarginBottom\n"));

    if ((ed = _XmGetWidgetExtData((Widget)lw, XmCACHE_EXTENSION)) != NULL)
    {
	LabG_MarginBottom(lw) = value;
    }

    bce = _XmGetBaseClassExtPtr(XtClass((Widget)lw), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    ((XmExtRec *)nsec)->object.self = (Widget)nsec;
    ((XmExtRec *)nsec)->object.widget_class = (*bce)->secondaryObjectClass;
    ((XmExtRec *)nsec)->object.parent = XtParent((Widget)lw);
    ((XmExtRec *)nsec)->object.xrm_name = ((Widget)lw)->core.xrm_name;
    ((XmExtRec *)nsec)->object.being_destroyed = False;
    ((XmExtRec *)nsec)->object.destroy_callbacks = NULL;
    ((XmExtRec *)nsec)->object.constraints = NULL;

    ExtObj_LogicalParent(nsec) = (Widget)lw;
    ExtObj_ExtensionType(nsec) = XmCACHE_EXTENSION;

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache((Widget)lw),
	   sizeof(XmLabelGCacheObjPart));

    ed = (XmWidgetExtData)XtMalloc(sizeof(XmWidgetExtDataRec));
    ed->widget = (Widget)nsec;

    _XmPushWidgetExtData((Widget)lw, ed, XmCACHE_EXTENSION);

    LabG_Cache((Widget)lw) = &(((XmLabelGCacheObject)nsec)->label_cache);

    LabG_MarginBottom(lw) = value;
}

static Boolean
drag_convert_proc(Widget w, Atom *selection,
		  Atom *target, Atom *type_return,
		  XtPointer *value_return,
		  unsigned long *length_return,
		  int *format_return)
{
    Atom COMPOUND_TEXT;
    Atom MOTIF_DROP;
    Widget source;
    char *ct, *text;
    XmString label;
    Atom TEXT;
    int len;

	DEBUGOUT(_LtDebug(__FILE__, w, "drag_convert_proc\n"));

    _XmObjectLock(w);
    COMPOUND_TEXT = XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
    MOTIF_DROP = XmInternAtom(XtDisplay(w), _XA_MOTIF_DROP, False);
    TEXT = XmInternAtom(XtDisplay(w), _XA_TEXT, False);

    if (*selection != MOTIF_DROP)
    {
	_XmObjectUnlock(w);
	return False;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "We're dealing with a motif drop\n"));

    XtVaGetValues(w,
    	XmNsourceWidget, &source,
    	NULL);
    XtVaGetValues(source,
    	XmNlabelString, &label,
    	NULL);

    ct = XmCvtXmStringToCT(label);
    XmStringFree(label);
    text = XtNewString(ct);
    len = strlen(text);

    if (*target == XA_STRING){
        *length_return = len;
        *value_return = text;
        *type_return = XA_STRING;
    }
    else if (*target == COMPOUND_TEXT || *target == TEXT){

	    XTextProperty prop;
	    char *buf;
	    int ret;

	    buf = XtMalloc(len + 1);
            strncpy(buf, text, len);
	    buf[len] = '\0';

	    ret = XmbTextListToTextProperty(XtDisplay(w), &buf, 1,
	    			XCompoundTextStyle, &prop);
		
	    XtFree(buf);
	    if (ret != 0){
	    	*length_return = 0;
	    	*value_return  = NULL;
	    }
	    else{
	    	buf = XtMalloc(prop.nitems + 1);
		strncpy(buf, (char*)prop.value, prop.nitems);
		buf[prop.nitems] = '\0';
	    	*length_return = prop.nitems;
	    	*value_return  = buf;
	    }
	   *type_return = COMPOUND_TEXT;

    }
    else
	return False;

    *format_return = 8;

    _XmObjectUnlock(w);
    return True;
}

static void
drag_drop_finish(Widget w, XtPointer client_data, XtPointer call_data)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "drag_drop_finish\n"));
}

void
_XmProcessDrag(Widget w, XEvent *event,
	       String *params, Cardinal *num_params)
{
    Atom export_target[3];
    Arg args[10];
    int n = 0;
    Widget dc;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmProcessDrag\n"));

    if (LabG_IsPixmap(w))
    {
	export_target[0] = XmInternAtom(XtDisplay(w),
				     _XA_PIXMAP,
				     False);
        XtSetArg(args[n], XmNexportTargets, export_target); n++;
        XtSetArg(args[n], XmNnumExportTargets, 1); n++;
    }
    else
    {
	export_target[0] = XmInternAtom(XtDisplay(w), _XA_COMPOUND_TEXT, False);
	export_target[1] = XmInternAtom(XtDisplay(w), _XA_TEXT, False);
	export_target[2] = XA_STRING;
        XtSetArg(args[n], XmNexportTargets, export_target); n++;
        XtSetArg(args[n], XmNnumExportTargets, 3); n++;
    }

    XtSetArg(args[n], XmNdragOperations, XmDROP_COPY); n++;
    XtSetArg(args[n], XmNconvertProc, drag_convert_proc); n++;
    XtSetArg(args[n], XmNclientData, w); n++;
    XtSetArg(args[n], XmNsourceCursorIcon,
	     _XmGetTextualDragIcon(XtParent(w))); n++;

    dc = XmDragStart(w, event, args, n);

    if (dc)
    {
	XtAddCallback(dc, XmNdragDropFinishCallback, drag_drop_finish, NULL);
    }
}

/******************************* GADGET PART *********************************/
static void
class_initialize(void)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmLabelGadget class_initialize\n"));

    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(LabG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(LabG_ClassCachePart(NULL));
    ClassCacheHead(LabG_ClassCachePart(NULL)).next =
	&ClassCacheHead(LabG_ClassCachePart(NULL));

    _XmLabelGRectClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmLabelGadgetClass lc = (XmLabelGadgetClass)widget_class;
    XmLabelGadgetClass sc =
    (XmLabelGadgetClass)(widget_class->core_class.superclass);
    XmGadgetClassExt ext, *extptr, *sextptr;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmLabelGadget class_part_initialize\n"));

    if (lc->label_class.menuProcs == XmInheritMenuProc)
    {
	lc->label_class.menuProcs =
	    sc->label_class.menuProcs;
    }
    if (lc->label_class.setOverrideCallback == XmInheritSetOverrideCallback)
    {
	lc->label_class.setOverrideCallback =
	    sc->label_class.setOverrideCallback;
    }

    extptr = (XmGadgetClassExt *)_XmGetClassExtensionPtr(
			    (XmGenericClassExt *)&(lc->gadget_class.extension),
							    NULLQUARK);
    sextptr = (XmGadgetClassExt *)_XmGetClassExtensionPtr(
			    (XmGenericClassExt *)&(sc->gadget_class.extension),
							     NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (XmGadgetClassExt)XtNew(XmGadgetClassExtRec);
	if (ext != NULL)
	{
	    ext->next_extension = lc->gadget_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XmGadgetClassExtVersion;
	    ext->record_size = sizeof(XmGadgetClassExtRec);
	    lc->gadget_class.extension = (XtPointer)ext;
	}
    }
    else
	ext = *extptr;
    if (sextptr && *sextptr)
    {
	if (ext->widget_baseline == XmInheritBaselineProc)
	{
	    ext->widget_baseline = (*sextptr)->widget_baseline;
	}
	if (ext->widget_display_rect == XmInheritDisplayRectProc)
	{
	    ext->widget_display_rect = (*sextptr)->widget_display_rect;
	}
    }

    _XmFastSubclassInit(widget_class, XmLABEL_GADGET_BIT);
}

static void
CreateNormalGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    DEBUGOUT(_LtDebug(__FILE__, w, "CreateNormalGC\n"));

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = XmParentForeground(w);
    values.background = XmParentBackground(w);
    values.fill_style = FillSolid;

    LabG_NormalGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateInsensitiveGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    DEBUGOUT(_LtDebug(__FILE__, w, "CreateInsensitiveGC\n"));

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction | GCStipple |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask |
	GCTileStipXOrigin | GCTileStipYOrigin;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = XmParentForeground(w);
    values.background = XmParentBackground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;
    values.stipple = XmGetPixmapByDepth(XtScreen(w), "50_foreground", 1, 0, 1);

    LabG_InsensitiveGC(w) = XtGetGC(w, mask, &values);
}

#if 0
static void
initialize_prehook(Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "LabelG InitializePrehook\n"));
}
#endif

static void
initialize_posthook(Widget request,
		    Widget new_w,
		    ArgList args,
		    Cardinal *num_args)
{
    XmWidgetExtData ext;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "LabelG InitializePosthook\n"));

    /* don't let the null fool you */
    LabG_Cache(new_w) = (XmLabelGCacheObjPart *)
	_XmCachePart(LabG_ClassCachePart(NULL),
		     (XtPointer)LabG_Cache(new_w),
		     sizeof(XmLabelGCacheObjPart));

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);
    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);
    XtFree((char *)ext);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
	XmString	xmstring;

#if 1
	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmLabelGadget Initialize fontlist %p (%p %p)\n",
		LabG_Font(new_w)));
#else
	DEBUGOUT(_LtDebug(__FILE__, new_w,
		"%s:initialize(%d) - %i args\n"
		"\trequest X %5i Y %5i W %5i H %5i\n"
		"\t  new_w X %5i Y %5i W %5i H %5i\n",
		__FILE__, __LINE__,
		*num_args,
		XtX(request), XtY(request),
		XtWidth(request), XtHeight(request),
		XtX(new_w), XtY(new_w),
		XtWidth(new_w), XtHeight(new_w)));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));
#endif
	if (LabG_Font(new_w) == NULL || LabG_Font(new_w) == (XmFontList)XmUNSPECIFIED) {
#if (XmVERSION > 1)
		/* Use the trait to find a default renderTable */
		/* FIX ME currently same as 1.2 */
		LabG_Font(new_w) = _XmGetDefaultFontList(new_w, XmTEXT_FONTLIST);
#else
		LabG_Font(new_w) = _XmGetDefaultFontList(new_w, XmTEXT_FONTLIST);
#endif
	}

    LabG_SkipCallback(new_w) = False;

    /* if the parent is a row column, set the menu_type to
       it's type.  Otherwise, XmNONE */

    LabG_MenuType(new_w) = XmIsRowColumn(XtParent(new_w))
	? RC_Type(XtParent(new_w))
	: XmNONE;

    /* Silently discard accelerator for non-menu labels and cascades */
    if ((LabG_MenuType(new_w) != XmMENU_PULLDOWN &&
	 LabG_MenuType(new_w) != XmMENU_POPUP) ||
	XmIsCascadeButtonGadget(new_w))
    {
	LabG_Accelerator(new_w) = NULL;
	LabG_AcceleratorText(new_w) = NULL;
    }

    /* Force the traversal and highlight on enter resources if
       in an popup, pulldown, and option menus */

    if (LabG_MenuType(new_w) != XmWORK_AREA)
    {
#if 0
MLM: WHY TRAVERSAL?
	G_TraversalOn(new_w) = False;
#endif
	G_HighlightOnEnter(new_w) = False;
    }

    /* check the RepType resources */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRAlignment),
			     LabG_Alignment(new_w), new_w))
	LabG_Alignment(new_w) = XmALIGNMENT_CENTER;
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRLabelType),
			     LabG_LabelType(new_w), new_w))
	LabG_LabelType(new_w) = XmSTRING;
    if (LabG_StringDirection(new_w) == (XmStringDirection)XmUNSPECIFIED ||
	!XmRepTypeValidValue(XmRepTypeGetId(XmRStringDirection),
			     LabG_StringDirection(new_w), new_w))
    {
	/* An unspecified stringDirection comes from the parent manager */
	LabG_StringDirection(new_w) = XmIsManager(XtParent(new_w))
	    ? MGR_StringDirection(XtParent(new_w))
	    : XmSTRING_DIRECTION_L_TO_R;
    }

    /* Get the default fontlist if the label was created without one.
     * For this purpose, "button" is defined as "subclass of LabelGadget".
     */
    if (LabG_Font(new_w) == (XmFontList)XmUNSPECIFIED ||
	LabG_Font(new_w) == NULL)
    {
	LabG_Font(new_w) = _XmGetDefaultFontList(new_w,
	    XtClass(new_w) == xmLabelGadgetClass
	    ? XmLABEL_FONTLIST
	    : XmBUTTON_FONTLIST);
    }
    /* if the user specified one, COPY it */
    else
    {
	LabG_Font(new_w) = XmFontListCopy(LabG_Font(new_w));
    }


    /* If the label was not initialized with the resource labelString set,
       use its name -- the follow _XmString code comes from MegaButton */
    if (LabG_Label(new_w) == NULL)
    {
	xmstring = _XmOSGetLocalizedString((char *)NULL,
					   (Widget)new_w,
					   XmNlabelString,
					   XtName(new_w));
	LabG_Label(new_w) = _XmStringCreate(xmstring);
	XmStringFree(xmstring);
    }

    else if (_XmStringIsXmString((XmString)LabG_Label(new_w)))
    {
	LabG_Label(new_w) = _XmStringCreate((XmString)LabG_Label(new_w));
    }
    else
    {
	LabG_Label(new_w) = NULL;
    }

    if (LabG_AcceleratorText(new_w) != NULL)
    {
	LabG_AcceleratorText(new_w) =
	    _XmStringCreate((XmString)LabG_AcceleratorText(new_w));
    }

    /* Size the internal parts */
    _XmCalcLabelGDimensions(new_w);

    /* allocate the normal and insensitive GC's */
    CreateNormalGC(new_w);
    CreateInsensitiveGC(new_w);

    if (!XtIsSubclass(XtParent(new_w), xmManagerWidgetClass))
    {
	_XmError(new_w, "parent should be manager.");
    }

    /* Get mnemonic and accelerator stuff going */
    if (LabG_MnemonicCharset(new_w) != NULL)
    {
	LabG_MnemonicCharset(new_w) = XtNewString(LabG_MnemonicCharset(new_w));
    }
    else
    {
	LabG_MnemonicCharset(new_w) = XtNewString(XmFONTLIST_DEFAULT_TAG);
    }

    if (LabG_Accelerator(new_w))
    {
	LabG_Accelerator(new_w) = XtNewString(LabG_Accelerator(new_w));
	_XmManagerInstallAccelerator(XtParent(new_w), new_w,
				     LabG_Accelerator(new_w));
    }
    if (LabG_Mnemonic(new_w))
    {
	_XmManagerInstallMnemonic(XtParent(new_w), new_w,
				  LabG_Mnemonic(new_w));
    }

    /* have to check request since new_w may have been polluted by a
     * superclass
     */
    if (XtWidth(request) == (Dimension)0)
    {
	XtWidth(new_w) = 0;
    }
    if (XtHeight(request) == (Dimension)0)
    {
	XtHeight(new_w) = 0;
    }
    xmLabelGadgetClassRec.rect_class.resize(new_w);

    G_EventMask(new_w) = XmBDRAG_EVENT | XmHELP_EVENT;
}

static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget destroy\n"));

    XmFontListFree(LabG_Font(w));
    _XmStringFree(LabG_Label(w));
    _XmStringFree(LabG_AcceleratorText(w));
    XtReleaseGC(w, LabG_NormalGC(w));
    XtReleaseGC(w, LabG_InsensitiveGC(w));
    XtFree(LabG_MnemonicCharset(w));
    _XmManagerUninstallAccelerator(XtParent(w), w);
    _XmManagerUninstallMnemonic(XtParent(w), w);

    _XmCacheDelete((XtPointer)LabG_Cache(w));
}

static void
resize(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget resize\n"));

    /* NB. For gadgets, the TextRect is relative to the XtX and XtY
     * positions, as the Xt[X|Y] are added onto them before the text is drawn.
     */

    if (!XmIsLabelGadget(w))
    {
	_XmWarning(w,
	    "Label resize called on non-Label gadget - this shouldn't happen");
	return;
    }

    /* Make sure there's room for the accelerator */
    if (LabG_AcceleratorText(w))
    {
	if (LabG_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R)
	{
	    if (LabG_MarginRight(w) <
		LABEL_ACC_PAD + LabG_AccTextRect(w).width)
	    {
		_XmAssignLabG_MarginRight((XmLabelGadget)w,
		    LABEL_ACC_PAD + LabG_AccTextRect(w).width);
		DEBUGOUT(_LtDebug(__FILE__, w,
				  "resize: set right margin to %d\n",
				  LabG_MarginRight(w)));
		_XmReCacheLabG(w);
	    }
	}
	else
	{
	    if (LabG_MarginLeft(w) <
		LABEL_ACC_PAD + LabG_AccTextRect(w).width)
	    {
		_XmAssignLabG_MarginLeft((XmLabelGadget)w,
		    LABEL_ACC_PAD + LabG_AccTextRect(w).width);
		DEBUGOUT(_LtDebug(__FILE__, w,
				  "resize: set left margin to %d\n",
				  LabG_MarginLeft(w)));
		_XmReCacheLabG(w);
	    }
	}
    }

    /* Set our preferred dimensions if they are currently zero */
    if (!XtWidth(w))
    {
	preferred_size(w, &XtWidth(w), NULL);
    }
    if (!XtHeight(w))
    {
	preferred_size(w, NULL, &XtHeight(w));
    }

    /* An RtoL stringDirection reverses the effect of alignment here */
    switch (LabG_Alignment(w) ^ (LabG_StringDirection(w) << 1))
    {
    case XmALIGNMENT_BEGINNING:
	LabG_TextRect_x(w) = (LabG_Highlight(w)
			      + LabG_Shadow(w)
			      + LabG_MarginWidth(w)
			      + LabG_MarginLeft(w));
	break;

    case XmALIGNMENT_CENTER:
    default:
	LabG_TextRect_x(w) = ((XtWidth(w)
			       - LabG_MarginLeft(w)
			       - LabG_MarginRight(w)
			       - LabG_TextRect_width(w)) >> 1)
	    + LabG_MarginLeft(w);
	break;

    case XmALIGNMENT_END:
	LabG_TextRect_x(w) = (XtWidth(w)
			      - LabG_Highlight(w)
			      - LabG_Shadow(w)
			      - LabG_MarginWidth(w)
			      - LabG_MarginRight(w)
			      - LabG_TextRect_width(w));
	break;
    }

    LabG_TextRect_y(w) = ((XtHeight(w)
			   - LabG_MarginTop(w)
			   - LabG_MarginBottom(w)
			   - LabG_TextRect_height(w)) >> 1)
	+ LabG_MarginTop(w);

    if (LabG_AcceleratorText(w))
    {
	LabG_AccTextRect(w).x =
	    LabG_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R
	    ? (XtWidth(w)
	       - LabG_Highlight(w)
	       - LabG_Shadow(w)
	       - LabG_MarginWidth(w)
	       - LabG_MarginRight(w)
	       + LABEL_ACC_PAD)
	    : (LabG_Highlight(w)
	       + LabG_Shadow(w)
	       + LabG_MarginWidth(w)
	       + LabG_MarginLeft(w)
	       - LABEL_ACC_PAD
	       - LabG_AccTextRect(w).width);

	LabG_AccTextRect(w).y = ((XtHeight(w)
				  - LabG_MarginTop(w)
				  - LabG_MarginBottom(w)
				  - LabG_AccTextRect(w).height) >> 1)
	    + LabG_MarginTop(w);
    }
}

static Boolean
set_values_prehook(Widget old, Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec, rsec;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmLabelGadget set_values_prehook\n"));

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
    ((XmExtRec *)rsec)->object.self = (Widget)rsec;

    LabG_Cache(new_w) = &(((XmLabelGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmLabelGCacheObject)rsec)->label_cache);

    _XmExtImportArgs((Widget)nsec, args, num_args);

    return False;
}

static Boolean
set_values_posthook(Widget old, Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    XmWidgetExtData ext;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmLabelGadget set_values_posthook\n"));

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

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);

    XtFree((char *)ext);

    return False;
}

/*
 * Manipulated to have the layout algorithms be called only once per call to
 * set_values, instead of three or four times... Danny 28/8/96
 */
static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
	XGCValues	gcv;
	Boolean		refresh_needed = False, relayout_needed = False,
			recalc_needed = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "LabelGadget SetValues %d %d\n",
		      LabG_MarginRight(new_w), LabG_MarginRight(old)));

    /* Silently discard accelerator for non-menu labels and cascades */
    if ((LabG_MenuType(new_w) != XmMENU_PULLDOWN &&
	 LabG_MenuType(new_w) != XmMENU_POPUP) ||
	XmIsCascadeButtonGadget(new_w))
    {
	LabG_Accelerator(new_w) = NULL;
	LabG_AcceleratorText(new_w) = NULL;
    }

    /* check the RepType resources */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRAlignment),
			     LabG_Alignment(new_w), new_w))
	LabG_Alignment(new_w) = LabG_Alignment(old);
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRLabelType),
			     LabG_LabelType(new_w), new_w))
	LabG_LabelType(new_w) = LabG_LabelType(old);
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRStringDirection),
			     LabG_StringDirection(new_w), new_w))
	LabG_StringDirection(new_w) = LabG_StringDirection(old);

    if (XtSensitive(old) != XtSensitive(new_w))
    {
	refresh_needed = True;
	if (LabG_IsPixmap(new_w))
	{
	    recalc_needed = True;
	}
    }

    /* This is a Primitive resource but we have the GC's for it... */
    /* See bug #772755 : need to query the old widget's values */
	XGetGCValues(XtDisplayOfObject(old), LabG_NormalGC(old), GCForeground, &gcv);
	if (XmParentForeground(new_w) != gcv.foreground)
/*
	if (XmParentForeground(new_w) != XmParentForeground(old))
 */
	{
		XtReleaseGC(new_w, LabG_NormalGC(new_w));
		XtReleaseGC(new_w, LabG_InsensitiveGC(new_w));

		CreateNormalGC(new_w);
		CreateInsensitiveGC(new_w);

		refresh_needed = True;
	}

    if (LabG_Font(new_w) != LabG_Font(old))
    {
	XmFontListFree(LabG_Font(old));
	if (LabG_Font(new_w) == (XmFontList)XmUNSPECIFIED ||
	    LabG_Font(new_w) == NULL)
	{
	    LabG_Font(new_w) = _XmGetDefaultFontList(new_w,
		XtClass(new_w) == xmLabelGadgetClass
		? XmLABEL_FONTLIST
		: XmBUTTON_FONTLIST);
	}
	else
	{
	    LabG_Font(new_w) = XmFontListCopy(LabG_Font(new_w));
	}

	refresh_needed = True;
	if (LabG_IsText(new_w) || LabG_AcceleratorText(new_w))
	{
	    recalc_needed = True;
	}
    }

    if (LabG_AcceleratorText(new_w) != LabG_AcceleratorText(old))
    {
	if (LabG_AcceleratorText(new_w))
	{
	    LabG_AcceleratorText(new_w) =
		_XmStringCreate((XmString)LabG_AcceleratorText(new_w));
	}
	else
	{
	    /* If the accelerator is removed, its margin is too */
	    if (LabG_StringDirection(old) == XmSTRING_DIRECTION_L_TO_R)
		LabG_MarginRight(new_w) = 0;
	    else
		LabG_MarginLeft(new_w) = 0;
	}

	_XmStringFree(LabG_AcceleratorText(old));

	recalc_needed = True;
    }

    if (LabG_AcceleratorText(new_w) &&
	LabG_StringDirection(new_w) != LabG_StringDirection(old))
    {
	LabG_MarginLeft(new_w) = 0;
	LabG_MarginRight(new_w) = 0;
	recalc_needed = True;
    }

    if (LabG_MnemonicCharset(new_w) != LabG_MnemonicCharset(old))
    {
	LabG_MnemonicCharset(new_w) = XtNewString(LabG_MnemonicCharset(new_w));

	XtFree(LabG_MnemonicCharset(old));

	refresh_needed = True;
    }

    /* if labelString is still NULL, it was set that way by the
     * user: NULL labelStrings get the widget name
     */
    if (LabG_Label(new_w) == NULL)
    {
	XmString tmp;

	tmp = XmStringCreateSimple(XtName(new_w));
	LabG_Label(new_w) = _XmStringCreate(tmp);
	XmStringFree(tmp);

	_XmStringFree(LabG_Label(old));

	if (LabG_IsText(new_w))
	{
	    recalc_needed = True;
	}
    }
    /*
     * The "else" was inserted here to make sure we don't do
     * _XmStringCreate twice
     */
    else if (LabG_Label(new_w) != LabG_Label(old))
    {
	if (_XmStringIsXmString((XmString)LabG_Label(new_w)))
	{
	    LabG_Label(new_w) = _XmStringCreate((XmString)LabG_Label(new_w));
	}

	_XmStringFree(LabG_Label(old));

	if (LabG_IsText(new_w))
	{
	    recalc_needed = True;
	}
    }

    if (LabG_Highlight(new_w) != LabG_Highlight(old)
	|| LabG_Shadow(new_w) != LabG_Shadow(old)
	|| LabG_MarginTop(new_w) != LabG_MarginTop(old)
	|| LabG_MarginBottom(new_w) != LabG_MarginBottom(old)
	|| LabG_MarginLeft(new_w) != LabG_MarginLeft(old)
	|| LabG_MarginRight(new_w) != LabG_MarginRight(old)
	|| LabG_MarginWidth(new_w) != LabG_MarginWidth(old)
	|| LabG_MarginHeight(new_w) != LabG_MarginHeight(old)
	|| LabG_Alignment(new_w) != LabG_Alignment(old)
	|| LabG_StringDirection(new_w) != LabG_StringDirection(old))
    {
	relayout_needed = True;
    }

    if (LabG_IsPixmap(new_w))
    {
	/* check for change in insensitive pixmap */
	if ((LabG_PixmapInsensitive(new_w) != LabG_PixmapInsensitive(old))
	    && !XtSensitive(new_w))
	{
	    recalc_needed = True;
	}

	/* check for change in pixmap */
	if (LabG_Pixmap(new_w) != LabG_Pixmap(old))
	{
	    /* if changed pixmap to UNSPECIFIED,
	     * automatically configure to a string.
	     */
	    if (LabG_IsPixmap(new_w)
		&& LabG_Pixmap(new_w) == (Pixmap)XmUNSPECIFIED_PIXMAP)
	    {
		LabG_LabelType(new_w) = XmSTRING;
	    }
	    if (XtSensitive(new_w))
	    {
		recalc_needed = True;
	    }
	}
    }

    /* did the label change types? */
    if (LabG_LabelType(new_w) != LabG_LabelType(old))
    {
	recalc_needed = True;
    }

    if (LabG_Accelerator(new_w) != LabG_Accelerator(old))
    {

	if (LabG_Accelerator(new_w))
	{
	    LabG_Accelerator(new_w) = XtNewString(LabG_Accelerator(new_w));
	}
	_XmManagerUninstallAccelerator(XtParent(new_w), new_w);
	_XmManagerInstallAccelerator(XtParent(new_w), new_w,
				     LabG_Accelerator(new_w));
    }
    if (LabG_Mnemonic(new_w) != LabG_Mnemonic(old))
    {
	_XmManagerUninstallMnemonic(XtParent(new_w), new_w);
	_XmManagerInstallMnemonic(XtParent(new_w), new_w,
				  LabG_Mnemonic(new_w));

	refresh_needed = True;
    }

    if (XtWidth(new_w) == 0 || XtHeight(new_w) == 0)
    {
	relayout_needed = True;
    }

    /* Resize the internal parts if something about them changed */
    if (recalc_needed)
    {
	_XmCalcLabelGDimensions(new_w);

	relayout_needed = True;
    }

    if (relayout_needed)
    {
	Dimension new_width, new_height;

	/* Get a new size if recomputeSize */
	if (LabG_RecomputeSize(new_w))
	{
	    preferred_size(new_w,
			   XtWidth(new_w) == XtWidth(old)
			   ? &XtWidth(new_w)
			   : NULL,
			   XtHeight(new_w) == XtHeight(old)
			   ? &XtHeight(new_w)
			   : NULL);
	}

	/* Put the internal parts in the right places.
	 * Since the size hasn't changed yet, use the old size.
	 * If it changes, resize will be called again by Xt.
	 */

	new_width = XtWidth(new_w);
	new_height = XtHeight(new_w);
	XtWidth(new_w) = XtWidth(old);
	XtHeight(new_w) = XtHeight(old);
	xmLabelGadgetClassRec.rect_class.resize(new_w);
	XtWidth(new_w) = new_width;
	XtHeight(new_w) = new_height;

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

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmLabelGadget get_values_prehook\n"));

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache(new_w),
	   sizeof(XmLabelGCacheObjPart));

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

	DEBUGOUT(_LtDebug(__FILE__, new_w, "XmLabelGadget get_values_posthook\n"));

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree((XtPointer)ext->widget);

    XtFree((char *)ext);
}

#if 0
static void
get_values_hook(Widget w, ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "LabelG: GetValuesHook\n"));
}
#endif

static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    XRectangle cliprect;
    GC myGC;
    Pixmap mypm;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget expose\n"));

    /*
     * I'm getting paranoid - Danny - see testXm/filesb/test3
     */
    if (!XtIsRealized(w))
    {
	_XmWarning(w,
	    "expose called on unrealized widget - this shouldn't happen");
	return;
    }

    if (G_Highlighted(w))
    {
	(*GC_BorderHighlight(XtClass(w))) (w);
    }
    else
    {
	(*GC_BorderUnhighlight(XtClass(w))) (w);
    }

#if 0
    /* if the internals of the widget haven't been recomputed yet,
     * recompute them now (happens if resize isn't called after set_values)
     */
    xmLabelGadgetClassRec.rect_class.resize(w);
#endif

    /* use the right GC */
    if (XtSensitive(w))
    {
	myGC = LabG_NormalGC(w);
    }
    else
    {
	myGC = LabG_InsensitiveGC(w);
    }

    /* Draw the accelerator without clipping */
    if (LabG_AcceleratorText(w))
    {
	_XmStringDraw(XtDisplay(w), XtWindow(w), LabG_Font(w),
		      LabG_AcceleratorText(w), myGC,
		      XtX(w) + LabG_AccTextRect(w).x,
		      XtY(w) + LabG_AccTextRect(w).y,
		      LabG_AccTextRect(w).width,
		      XmALIGNMENT_BEGINNING, LabG_StringDirection(w), NULL);
    }

    /* Set a clip rectangle for the GC - ensure we don't overwrite shadows */
    /* the rectangle used to include MarginWidth and MarginHeight.  Leaving
     * those two out makes Xinvest and Xmcd look better */
    /*
     * See Label.c:expose to learn about the margins.
     */
    cliprect.x = XtX(w)
               + G_HighlightThickness(w)
               + G_ShadowThickness(w)
               + LabG_MarginLeft(w);
    cliprect.y = XtY(w)
               + G_HighlightThickness(w)
               + G_ShadowThickness(w)
               + LabG_MarginTop(w);
    cliprect.width  = XtWidth(w)
	- ((G_ShadowThickness(w) + G_HighlightThickness(w)) << 1)
	- LabG_MarginLeft(w) - LabG_MarginRight(w);
    cliprect.height = XtHeight(w)
	- ((G_ShadowThickness(w) + G_HighlightThickness(w)) << 1)
	- LabG_MarginTop(w) - LabG_MarginBottom(w);

    DEBUGOUT(_LtDebug(__FILE__, w,
             "Expose: cliprect %d %d %dx%d\n",
            cliprect.x, cliprect.y,
             cliprect.width, cliprect.height));
    DEBUGOUT(_LtDebug(__FILE__, w,
             "Expose: acctextrect %d %d %dx%d\n",
            LabG_AccTextRect(w).x, LabG_AccTextRect(w).y,
             LabG_AccTextRect(w).width, LabG_AccTextRect(w).height));

    if (LabG_IsText(w))
    {
	if (LabG_Mnemonic(w))
	{
	    char m[2];

	    m[0] = LabG_Mnemonic(w);
	    m[1] = '\0';

	    _XmStringDrawMnemonic(XtDisplay(w), XtWindow(w),
				  LabG_Font(w), LabG_Label(w), myGC,
				  XtX(w) + LabG_TextRect_x(w),
				  XtY(w) + LabG_TextRect_y(w),
				  LabG_TextRect_width(w),
				  LabG_Alignment(w),
				  LabG_StringDirection(w),
				  &cliprect,
				  m, LabG_MnemonicCharset(w));
	}
	else
	{
	    _XmStringDraw(XtDisplayOfObject(w),
			  XtWindowOfObject(w),
			  LabG_Font(w),
			  LabG_Label(w),
			  myGC,
			  XtX(w) + LabG_TextRect_x(w),
			  XtY(w) + LabG_TextRect_y(w),
			  LabG_TextRect_width(w),
			  LabG_Alignment(w),
			  LabG_StringDirection(w),
			  &cliprect);
	}
    }
    else
    {
	/* Copy only the required part of the pixmap
	 * (faster than sending a clip to the server).
	 */
	mypm = XtSensitive(w)
	    ? LabG_Pixmap(w)
	    : LabG_PixmapInsensitive(w);
#if XmVERSION > 1
	if (mypm == XmUNSPECIFIED_PIXMAP)
	{
	    mypm = LabG_Pixmap(w);
	}
#endif
	cliprect.x -= XtX(w);
	cliprect.y -= XtY(w);
	if (mypm > XmUNSPECIFIED_PIXMAP &&
	    cliprect.x + cliprect.width > LabG_TextRect_x(w) &&
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
		cliprect.width =
		    (LabG_TextRect_x(w) + LabG_TextRect_width(w)) - cliprect.x;
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
#if XmVERSION > 1
	    {
		unsigned int tmp, tmpw, tmph;
		int tmpx, tmpy;
		unsigned Depth;
		Window tmpwin;

		/* rws 4 Mar 2000
		   According to the comments in label/test12, this does not
		   happen. We should be adding the pixmap to our pixmap
		   cache, and getting the info from there instead of a
		   round trip server query.
		 */
		/* rws 10 Mar 2000
		   Apparently a Motif 2 thing.
		 */
		XGetGeometry(XtDisplayOfObject(w),
			     mypm,
			     &tmpwin,
			     &tmpx, &tmpy,
			     &tmpw, &tmph,
			     &tmp, &Depth);

		if (Depth == 1)
		{
		    XCopyPlane(XtDisplay(w),
			  mypm,
			  XtWindow(w),
			  myGC,
			  cliprect.x - LabG_TextRect_x(w),
			  cliprect.y - LabG_TextRect_y(w),
			  cliprect.width, cliprect.height,
			  cliprect.x + XtX(w), cliprect.y + XtY(w),
			  1);
		}
		else if (Depth > 1)
		{
		    XCopyArea(XtDisplay(w),
			  mypm,
			  XtWindow(w),
			  myGC,
			  cliprect.x - LabG_TextRect_x(w),
			  cliprect.y - LabG_TextRect_y(w),
			  cliprect.width, cliprect.height,
			  cliprect.x + XtX(w), cliprect.y + XtY(w));
		}
	    }
	    if (!XtSensitive(w) && LabG_PixmapInsensitive(w) == XmUNSPECIFIED_PIXMAP)
	    {
	    	/* label/test14 */
	    	XFillRectangle(XtDisplay(w), XtWindow(w),
	    		myGC,
		  cliprect.x,
		  cliprect.y,
		  cliprect.width, cliprect.height);
	    }
#else
	    XCopyArea(XtDisplay(w),
		      mypm,
		      XtWindow(w),
		      myGC,
		      cliprect.x - LabG_TextRect_x(w),
		      cliprect.y - LabG_TextRect_y(w),
		      cliprect.width, cliprect.height,
		      cliprect.x + XtX(w), cliprect.y + XtY(w));
#endif
	}
    }
}


static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
#if 0
    XtWidgetGeometry a;		/* Standin for answer if NULL parameter */
    Dimension wd, ht;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget query_geometry\n"));

    wd = XtWidth(w);
    ht = XtHeight(w);
    /* rws 28 Feb 1997
     * Setting height and width to 0 will calculate the minimum
     * size of the label. If recompute is false we do not want to
     * do that. This helps form/test4 (and some of my own apps)
     * This, however requires a change to the initialize
     *
     * rws 22 Mar 1997
     * As Mitch suspects recompute size is only dealt with in set_values
     */

    if (proposed->request_mode & CWWidth)
    {
	XtWidth(w) = proposed->width;
    }
    if (proposed->request_mode & CWHeight)
    {
	XtHeight(w) = proposed->height;
    }

    if (!LabG_RecomputeSize(w))
    {
	a.width = XtWidth(w);
	a.height = XtHeight(w);
	a.request_mode = CWWidth | CWHeight;

	XtWidth(w) = wd;
	XtHeight(w) = ht;
	if ((proposed->request_mode & (CWWidth | CWHeight)) ==
	    (CWWidth | CWHeight) &&
	    proposed->width == answer->width &&
	    proposed->height == answer->height)
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

    if (XmIsCascadeButtonGadget(w))
    {
	extern void _XmCBGCalcDimensions(Widget w);

	_XmCBGCalcDimensions(w);
    }
    else
    {
	preferred_size(w, &XtWidth(w), &XtHeight(w));
    }

    a.width = XtWidth(w);
    a.height = XtHeight(w);
    a.request_mode = CWWidth | CWHeight;
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "LabelGadget queried for size: reporting %d %d %08x\n",
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
#else
    XtWidgetGeometry pc;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget query_geometry\n"));

	pc = *proposed;
	/* rws 27 Sep 1998
	   label/test8 should prove whether this IsRealized should be here
	   or not, and it should not.
	   label/test9 should prove whether LabG_RecomputeSize should be here
	   or not,
	 */
	if (LabG_RecomputeSize(w))
	{
	    preferred_size(w, &answer->width, &answer->height);
	}
	else
	{
	    answer->width = XtWidth(w);
	    answer->height = XtHeight(w);
	}

#if 0
	if (((pc.request_mode & (CWWidth | CWHeight)) == (CWWidth | CWHeight)) &&
		pc.width == answer->width &&
		pc.height == answer->height)
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
#else
	return _XmGMReplyToQueryGeometry(w, &pc, answer);
#endif
#endif
}

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmLabelGadget get_sec_res_data\n"));

	/* FIX ME */

	return _XmSecondaryResourceData(&_XmLabelGRectClassExtRec,
		data, NULL, NULL, NULL, NULL);
}

static Boolean
widget_baseline(Widget w, Dimension **baselines, int *nbaselines)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget widget_baseline\n"));

    if (!LabG_IsText(w))
	return False;

    *nbaselines = _XmStringLineCount(LabG_Label(w));
    *baselines = (Dimension *)XtMalloc(sizeof(Dimension) * *nbaselines);
    _XmStringBaselines(LabG_Font(w), LabG_Label(w), LabG_TextRect_y(w),
		       *baselines);
    return True;
}

static Boolean
widget_display_rect(Widget w, XRectangle *rect)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget widget_display_rect\n"));
	*rect = ((XmLabelGadget)w)->label.TextRect;
	return True;
}


#if XmVERSION >= 2
static void
widget_margins (Widget w, XmBaselineMargins *margins)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget widget_margins\n"));
 /* FIX ME */
}
#endif

/* FIX ME: there has to be more to this, doesn't there? */
static void
set_override_callback(Widget w)
{
    LabG_SkipCallback(w) = True;
}

static void
export_label_string(Widget w, int offset, XtArgVal *value)
{
    _XmString str = *(_XmString *)(((char *)w) + offset);

	DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget export_label_string\n"));

    *value = str
	? (XtArgVal)_XmStringCreateExternal(LabG_Font(w), str)
	: (XtArgVal)NULL;
}

static void
input_dispatch(Widget gadget, XEvent *event, Mask event_mask)
{
    switch (event_mask)
    {
    case XmHELP_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget, "LabelGadget got help event\n"));
	break;

    case XmBDRAG_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget, "LabelGadget got bdrag event\n"));
	_XmProcessDrag(gadget, event, NULL, NULL);
	break;
    default:
	/* Should this happen ? */
	DEBUGOUT(_LtDebug(__FILE__, gadget, "XmLabelGadget input_dispatch\n"));
	break;
    }
}

static void
preferred_size(Widget w, Dimension *width, Dimension *height)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "XmLabelGadget preferred_size\n"));

    if (width)
    {
	*width =
	    ((LabG_Highlight(w) + LabG_Shadow(w) + LabG_MarginWidth(w)) << 1)
	    + LabG_MarginLeft(w) + (LabG_AcceleratorText(w) &&
				    LabG_MarginRight(w) <
				    LABEL_ACC_PAD + LabG_AccTextRect(w).width
				    ? LABEL_ACC_PAD + LabG_AccTextRect(w).width
				    : LabG_MarginRight(w))
	    + LabG_TextRect_width(w);
	if (*width == 0)
	    *width = 1;
    }

    if (height)
    {
	*height =
	    ((LabG_Highlight(w) + LabG_Shadow(w) + LabG_MarginHeight(w)) << 1)
	    + LabG_MarginTop(w) + LabG_MarginBottom(w)
	    + (LabG_AcceleratorText(w) &&
	       LabG_AccTextRect(w).height > LabG_TextRect_height(w)
	       ? LabG_AccTextRect(w).height
	       : LabG_TextRect_height(w));
	if (*height == 0)
	    *height = 1;
    }
}

Widget
XmCreateLabelGadget(Widget parent, char *name,
		    Arg *arglist, Cardinal argcount)
{
	Widget	w;

	DEBUGOUT(_LtDebug(__FILE__, parent, "XmCreateLabelGadget(%s)\n", name));

	_XmObjectLock(parent);
	w =  XtCreateWidget(name, xmLabelGadgetClass, parent,
			  arglist, argcount);

	DEBUGOUT(_LtDebug2(__FILE__, parent, w, "XmCreateLabelGadget => %p\n", w));

	_XmObjectUnlock(parent);
	return w;
}

static void
_XmLabelGSetRenderTable(Widget w, int o, XrmValue *v)
{
	XmLabelGadget		lw = (XmLabelGadget)w;
	static const XmFontList	nullfl = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmLabelGSetRenderTable (%d) Font was %p\n",
		lw->label.check_set_render_table, lw->label.font));

	++lw->label.check_set_render_table;
	switch (lw->label.check_set_render_table)
	{
	case 1:
                /*
                 * Either the font list or render table resource has
                 * not been set, but do not know yet if both have not
                 * been set.  For now, preserve the value in case one
                 * of the resources has been set.
                 */
                v->addr = (char *)&(lw->label.font);
                break;

	case 2:
                /*
                 * Neither the font list nor render table resource has
                 * been set.  To avoid relying on the structure having
                 * been zero filled by the Xt library, ensure the
                 * font element is set to NULL.
                 */
                v->addr = (char*)&nullfl;
                break;

        default:
                /* This should never happen. */
                v->addr = NULL;
                break;
	}
}
