/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Label.c,v 1.13 2008/01/02 19:42:57 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Label.c,v 1.13 2008/01/02 19:42:57 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/XmosP.h>
#include <Xm/AtomMgr.h>
#include <Xm/DragC.h>
#include <Xm/DragIconP.h>
#include <Xm/LabelP.h>
#include <Xm/LabelGP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumnP.h>
#include <Xm/TransltnsP.h>
#include <XmI/AtomMgrI.h>
#include <XmI/DragDropI.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void initialize_prehook(Widget request, Widget new_w,
			       ArgList args, Cardinal *num_args);

static void initialize_posthook(Widget request, Widget new_w,
				ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static void resize(Widget w);

static void XmLabelExpose(Widget w, XEvent *event, Region region);

static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static Boolean widget_baseline(Widget w, Dimension **baselines,
			       int *nbaselines);

static Boolean widget_display_rect(Widget w, XRectangle *rect);

static void set_override_callback(Widget w);

static void export_label_string(Widget w, int offset, XtArgVal *value);

static Boolean drag_convert_proc(Widget w, Atom *selection,
				 Atom *target, Atom *type_return,
				 XtPointer *value_return,
				 unsigned long *length_return,
				 int *format_return);

static void drag_drop_finish(Widget w,
			     XtPointer client_data,
			     XtPointer call_data);

static void preferred_size(Widget w, Dimension *width, Dimension *height);
static void _XmLabelSetRenderTable(Widget, int, XrmValue *);
/*
 * Resources for the label class
 */
#define Offset(field) XtOffsetOf(XmLabelRec, label.field)
static XtResource resources[] =
{
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmLabelRec, primitive.shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNalignment, XmCAlignment, XmRAlignment,
	sizeof(unsigned char), Offset(alignment),
	XmRImmediate, (XtPointer)XmALIGNMENT_CENTER
    },
    {
	XmNlabelType, XmCLabelType, XmRLabelType,
	sizeof(unsigned char), Offset(label_type),
	XmRImmediate, (XtPointer)XmSTRING
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
	/*
	 * Note !!
	 * Order is important. This resource must be specified before the
	 * XmNfontList and XmNrenderTable, otherwise the check_set_render_table
	 * flag is not initialised before use in _XmLabelSetRenderTable().
	 */
	"keep.off", "Keep.off", XmRBoolean,
	sizeof(Boolean), Offset(check_set_render_table),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(font),
	XmRCallProc, (XtPointer)_XmLabelSetRenderTable
    },
    {
	XmNrenderTable, XmCRenderTable, XmRRenderTable,
	sizeof(XmFontList), Offset(font),
	XmRCallProc, (XtPointer)_XmLabelSetRenderTable
    },
    /* End of fontList and renderTable resources */
    {
	XmNlabelPixmap, XmCLabelPixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNlabelInsensitivePixmap, XmCLabelInsensitivePixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(pixmap_insen),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNlabelString, XmCXmString, XmRXmString,
	sizeof(XmString), Offset(_label),
	XmRImmediate, (XtPointer)NULL
    },
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
    {
	XmNrecomputeSize, XmCRecomputeSize, XmRBoolean,
	sizeof(Boolean), Offset(recompute_size),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNstringDirection, XmCStringDirection, XmRStringDirection,
	sizeof(XmStringDirection), Offset(string_direction),
	XmRImmediate, (XtPointer)((XmStringDirection)XmUNSPECIFIED)
    },
    /* resources we override from XmPrimitive */
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmLabelRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmLabelRec, primitive.highlight_thickness),
	XmRImmediate, (XtPointer)0
    },
};

/* Synthetic Resources for the Label Widget */

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
    {
	XmNlabelString,
	sizeof(XmString), Offset(_label),
	export_label_string, NULL
    },
    {
	XmNmnemonicCharSet,
	sizeof(String), Offset(mnemonicCharset),
	_XmExportString, NULL
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
};

static void ProcessDrag(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void Help(Widget w, XEvent *event,
		 String *params, Cardinal *num_params);

static XtActionsRec actions[] =
{
    {"Enter", _XmPrimitiveEnter},
    {"Leave", _XmPrimitiveLeave},
    {"Help", Help},
    {"ProcessDrag", ProcessDrag},
};

static XmBaseClassExtRec _XmLabelCoreClassExtRec = {
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
    /* get_values_posthook       */ XmInheritGetValuesPrehook,
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

XmPrimitiveClassExtRec _XmLabelPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ widget_baseline,
    /* widget_display_rect */ widget_display_rect,
    /* widget_margins      */ NULL
};

XmLabelClassRec xmLabelClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmPrimitiveClassRec,
        /* class_name            */ "XmLabel",
	/* widget_size           */ sizeof(XmLabelRec),
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
	/* expose                */ XmLabelExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost, /* FIX ME */
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersionDontCheck,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmLabelCoreClassExtRec
    },
    /* Primitive Class part */
    {
        /* border_highlight      */ XmInheritBorderHighlight,
        /* border_unhighlight    */ XmInheritBorderUnhighlight,
        /* translations          */ XtInheritTranslations,
        /* arm_and_activate_proc */ NULL,
        /* Synthetic Resources   */ syn_resources,
        /* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmLabelPrimClassExtRec
    },
    /* Label Class part */
    {
        /* setOverrideCallback */ set_override_callback,
        /* menuProcs           */ NULL,
        /* translations        */ NULL,
	/* extension           */ NULL
    }
};

WidgetClass xmLabelWidgetClass = (WidgetClass)&xmLabelClassRec;


static XtTranslations menu_trans = NULL;
static XtTranslations default_trans = NULL;

static void
class_initialize(void)
{
    menu_trans = XtParseTranslationTable(_XmLabel_menuTranslations);
    default_trans = XtParseTranslationTable(_XmLabel_defaultTranslations);

    _XmLabelCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmLabelWidgetClass lwc = (XmLabelWidgetClass)widget_class;
    XmLabelWidgetClass swc =
    (XmLabelWidgetClass)(widget_class->core_class.superclass);
    XmPrimitiveClassExt ext, *extptr, *sextptr;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "XmLabel class_part_initialize - %s -> %s\n",
		swc->core_class.class_name,
		widget_class->core_class.class_name));
    DEBUGOUT(_LtDebug0("MENUT", NULL, "XmLabel class_part_initialize - %s -> %s\n",
		swc->core_class.class_name,
		widget_class->core_class.class_name));
    /* Handle label class part inheritance */

    if (lwc->label_class.menuProcs == XmInheritMenuProc)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL, "    XmInheritMenuProc\n"));
	lwc->label_class.menuProcs =
	    swc->label_class.menuProcs;
    }
    if (lwc->label_class.setOverrideCallback == XmInheritSetOverrideCallback)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL, "    XmInheritSetOverrideCallback\n"));
	lwc->label_class.setOverrideCallback =
	    swc->label_class.setOverrideCallback;
    }
    if (lwc->label_class.translations == XtInheritTranslations)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL, "    XtInheritTranslations\n"));
	DEBUGOUT(_LtDebug0("MENUT", NULL, "    XtInheritTranslations\n"));
	lwc->label_class.translations =
	    swc->label_class.translations;
    }
    else if (lwc->label_class.translations != NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL, "    supplied by subclass\n"));
	DEBUGOUT(_LtDebug0("MENUT", NULL, "    supplied by subclass\n"));
	lwc->label_class.translations =
	    (String)XtParseTranslationTable(lwc->label_class.translations);
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL, "    Use menu traversal translations\n"));
	DEBUGOUT(_LtDebug0("MENUT", NULL, "    Use menu traversal translations\n"));
	lwc->label_class.translations =
	    (String)XtParseTranslationTable(_XmLabel_menu_traversal_events);
    }

    extptr = (XmPrimitiveClassExt *)_XmGetClassExtensionPtr(
			(XmGenericClassExt *)&(lwc->primitive_class.extension),
							       NULLQUARK);
    sextptr = (XmPrimitiveClassExt *)_XmGetClassExtensionPtr(
			(XmGenericClassExt *)&(swc->primitive_class.extension),
								NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (XmPrimitiveClassExt)XtNew(XmPrimitiveClassExtRec);
	if (ext != NULL)
	{
	    ext->next_extension = lwc->primitive_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XmPrimitiveClassExtVersion;
	    ext->record_size = sizeof(XmPrimitiveClassExtRec);
	    lwc->primitive_class.extension = (XtPointer)ext;
	}
    }
    else
    {
	ext = *extptr;
    }

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
	if (ext->widget_margins == XmInheritMarginsProc)
	{
	    ext->widget_margins = (*sextptr)->widget_margins;
	}
    }

    _XmFastSubclassInit(widget_class, XmLABEL_BIT);
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
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillSolid;

    Lab_NormalGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateInsensitiveGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction | GCStipple |
	GCPlaneMask | GCSubwindowMode | GCGraphicsExposures |
	GCTileStipXOrigin | GCTileStipYOrigin;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = Prim_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillStippled;
    values.ts_x_origin = values.ts_y_origin = 0;
    values.stipple = XmGetPixmapByDepth(XtScreen(w), "50_foreground", 1, 0, 1);

    Lab_InsensitiveGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_prehook(Widget request,
		   Widget new_w,
		   ArgList args,
		   Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "Initialize Prehook\n"));

    _XmSaveCoreClassTranslations(new_w);

    if (XmIsRowColumn(XtParent(new_w)) &&
	(RC_Type(XtParent(new_w)) == XmMENU_PULLDOWN ||
	 RC_Type(XtParent(new_w)) == XmMENU_POPUP ||
	 RC_Type(XtParent(new_w)) == XmMENU_BAR))
    {
	CoreClassTranslations(new_w) = (String)menu_trans;
    }
    else if (!XmIsDrawnButton(new_w))
    {
	CoreClassTranslations(new_w) = (String)default_trans;
    }
}

static void
initialize_posthook(Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "Initialize Posthook\n"));

    _XmRestoreCoreClassTranslations(new_w);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
	DEBUGOUT(_LtDebug(__FILE__, new_w,
		"XmLabel initialize - %i args, font %p\n"
		"\trequest X %5i Y %5i W %5i H %5i\n"
		"\t  new_w X %5i Y %5i W %5i H %5i\n",
		*num_args, ((XmLabelWidget)new_w)->label.font,
		XtX(request), XtY(request),
		XtWidth(request), XtHeight(request),
		XtX(new_w), XtY(new_w),
		XtWidth(new_w), XtHeight(new_w)));
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

	if (Lab_Font(new_w) == NULL || Lab_Font(new_w) == (XmFontList)XmUNSPECIFIED) {
		/* Use the trait to find a default renderTable */
		/* FIX ME currently same as 1.2 */
		Lab_Font(new_w) = _XmGetDefaultFontList(new_w, XmTEXT_FONTLIST);
	}

    Lab_SkipCallback(new_w) = False;

    /* if the parent is a row column, set the menu_type to
       it's type.  Otherwise, XmNONE */

    Lab_MenuType(new_w) = XmIsRowColumn(XtParent(new_w))
	? RC_Type(XtParent(new_w))
	: XmNONE;

    /* Silently discard accelerator for non-menu labels and cascades */
    if ((Lab_MenuType(new_w) != XmMENU_PULLDOWN &&
	 Lab_MenuType(new_w) != XmMENU_POPUP) ||
	XmIsCascadeButton(new_w))
    {
	Lab_Accelerator(new_w) = NULL;
	Lab_AcceleratorText(new_w) = NULL;
    }

    /* Force the traversal and highlight on enter resources if in a menu */
    if (Lab_MenuType(new_w) != XmWORK_AREA)
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w, "    merge in Label Class Translations\n"));
#if 0
MLM: WHY TRAVERSAL?
	Prim_TraversalOn(new_w) = False;
#endif
	Prim_HighlightOnEnter(new_w) = False;

	DEBUGOUT(_LtDebug("MENUT", new_w, "%s:initialize(%d) - Override translations\n",
	    __FILE__, __LINE__
	    ));
	XtOverrideTranslations(new_w,
			       (XtTranslations)LabClass_Translations(XtClass(new_w)));
    }

    /* check the RepType resources */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRAlignment),
			     Lab_Alignment(new_w), new_w))
	Lab_Alignment(new_w) = XmALIGNMENT_CENTER;
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRLabelType),
			     Lab_LabelType(new_w), new_w))
	Lab_LabelType(new_w) = XmSTRING;
    if (Lab_StringDirection(new_w) == (XmStringDirection)XmUNSPECIFIED ||
	!XmRepTypeValidValue(XmRepTypeGetId(XmRStringDirection),
			     Lab_StringDirection(new_w), new_w))
    {
	/* An unspecified stringDirection comes from the parent manager */
	Lab_StringDirection(new_w) = XmIsManager(XtParent(new_w))
	    ? MGR_StringDirection(XtParent(new_w))
	    : XmSTRING_DIRECTION_L_TO_R;
    }

    /* Get the default fontlist if the label was created without one.
     * For this purpose, "button" is defined as "subclass of Label".
     */
    DEBUGOUT(_LtDebug(__FILE__, new_w,
    	"XmLabelInitialize: font %p, count %d dpy %p rend %p\n",
	Lab_Font(new_w), Lab_Font(new_w)->count,
	Lab_Font(new_w)->dpy, Lab_Font(new_w)->renditions));

    if (Lab_Font(new_w) == (XmFontList)XmUNSPECIFIED ||
	Lab_Font(new_w) == NULL)
    {
	Lab_Font(new_w) = _XmGetDefaultFontList(new_w,
	    XtClass(new_w) == xmLabelWidgetClass
	    ? XmLABEL_FONTLIST
	    : XmBUTTON_FONTLIST);
	if (Lab_Font(request)) {
			XmFontListFree(Lab_Font(request));
			Lab_Font(request) = NULL;
	}
    }
    /* if the user specified one, COPY it */
    else
    {
	Lab_Font(new_w) = XmFontListCopy(Lab_Font(new_w));
    }

    /* If the label was not initialized with the resource labelString set,
       use its name -- the following _XmString code comes from MegaButton */
    if (Lab_Label(new_w) == (_XmString)XmUNSPECIFIED ||
	Lab_Label(new_w) == NULL)
    {
	XmString xmstring;

	xmstring = Lab_Label(new_w)
	    ? XmStringCreateSimple(XmS)
	    : _XmOSGetLocalizedString((char *)NULL,
				      (Widget)new_w,
				      XmNlabelString,
				      XtName(new_w));
	Lab_Label(new_w) = _XmStringCreate(xmstring);
	XmStringFree(xmstring);
    }

    else if (_XmStringIsXmString((XmString)Lab_Label(new_w)))
    {
	Lab_Label(new_w) = _XmStringCreate((XmString)Lab_Label(new_w));
    }

    if (Lab_AcceleratorText(new_w) != NULL)
    {
	Lab_AcceleratorText(new_w) =
	    _XmStringCreate((XmString)Lab_AcceleratorText(new_w));
    }

    /* Size the internal parts */
    _XmCalcLabelDimensions(new_w);

    /* allocate the normal and insensitive GC's */
    CreateNormalGC(new_w);
    CreateInsensitiveGC(new_w);

    /* Get mnemonic and accelerator stuff going */
    if (Lab_MnemonicCharset(new_w) != NULL)
    {
	Lab_MnemonicCharset(new_w) = XtNewString(Lab_MnemonicCharset(new_w));
    }
    else
    {
	Lab_MnemonicCharset(new_w) = XtNewString(XmFONTLIST_DEFAULT_TAG);
    }

    if (Lab_Accelerator(new_w))
    {
	Lab_Accelerator(new_w) = XtNewString(Lab_Accelerator(new_w));
	_XmManagerInstallAccelerator(XtParent(new_w), new_w,
				     Lab_Accelerator(new_w));
    }

    if (Lab_Mnemonic(new_w))
    {
	_XmManagerInstallMnemonic(XtParent(new_w), new_w, Lab_Mnemonic(new_w));
    }

    /*
     * have to check request since new may have been polluted by a
     * superclass
     * MLM: Who?  *Primitive*?  The only ones it could use to change
     * width/height we override (shadowThickness/highlightThickness)
     * JHG: Only the default is overridden - resources may override these
     *  and Primitive would still change width/height with them.
     */
    if (XtWidth(request) == (Dimension)0)
    {
	XtWidth(new_w) = 0;
    }
    if (XtHeight(request) == (Dimension)0)
    {
	XtHeight(new_w) = 0;
    }
    xmLabelClassRec.core_class.resize(new_w);
}

static void
destroy(Widget w)
{
    _XmManagerUninstallMnemonic(XtParent(w), w);
    _XmManagerUninstallAccelerator(XtParent(w), w);
    XtFree(Lab_Accelerator(w));
    XtFree(Lab_MnemonicCharset(w));
    XtReleaseGC(w, Lab_InsensitiveGC(w));
    XtReleaseGC(w, Lab_NormalGC(w));
    _XmStringFree(Lab_AcceleratorText(w));
    _XmStringFree(Lab_Label(w));
    XmFontListFree(Lab_Font(w));
    Lab_Font(w) = NULL;
}


/* It is bad to call _XmCalcLabelDimensions [now: resize] inside SetValues
 * since it is possible the parent will reject the size
 * changes and _XmLabelRecomputeSize will have changed widget
 * internals based on the size it thinks it will get.
 * If the changes are rejected, we are left with the changed
 * internals and the unchanged Width and Height.
 *
 * We could change the label widget to use set_values_almost, but
 * I don't see the need to do so.  In fact, I can't think of any
 * simple widgets that use set_values_almost anyway.  The internal
 * positions are always calcuated in resize.
 *
 * Lab_RecomputeSize seems to be checked in OSF/Motif when either
 *      o the pixmap changes
 *      o the labelString changes
 * it is possible to change into the other...
 *
 * What happens if we are changing the label (possibly
 * to a pixmap) and the size change is rejected or the new size evaluates
 * to the same value as the old size?  Resize won't be called.  We need to
 * do something so expose doesn't use the old internal values.
 *
 * Just recompute the label internals in expose, by calling resize()
 * directly. :)
 *
 * That solution is gone now - Rick says it caused loop with subclasses,
 * and Motif never did that anyway.  The thing to do is to call resize
 * with the original dimensions, then put the changed ones back up afterwards.
 * That should be the method for all widgets.
 */
static Boolean
set_values(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False, relayout_needed = False,
	recalc_needed = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "XmLabel set_values: %i args\n"
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

    /* Silently discard accelerator for non-menu labels and cascades */
    if ((Lab_MenuType(new_w) != XmMENU_PULLDOWN &&
	 Lab_MenuType(new_w) != XmMENU_POPUP) ||
	XmIsCascadeButton(new_w))
    {
	Lab_Accelerator(new_w) = NULL;
	Lab_AcceleratorText(new_w) = NULL;
    }

    /* check the RepType resources */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRAlignment),
			     Lab_Alignment(new_w), new_w))
	Lab_Alignment(new_w) = Lab_Alignment(old);
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRLabelType),
			     Lab_LabelType(new_w), new_w))
	Lab_LabelType(new_w) = Lab_LabelType(old);
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRStringDirection),
			     Lab_StringDirection(new_w), new_w))
	Lab_StringDirection(new_w) = Lab_StringDirection(old);

    if (XtSensitive(old) != XtSensitive(new_w))
    {
	refresh_needed = True;
	if (Lab_IsPixmap(new_w))
	{
	    recalc_needed = True;
	}
    }
    /* This is a Primitive resource but we have the GC's for it... */
    if (Prim_Foreground(new_w) != Prim_Foreground(old) ||
	XtBackground(new_w) != XtBackground(old))
    {
	XtReleaseGC(new_w, Lab_NormalGC(new_w));
	XtReleaseGC(new_w, Lab_InsensitiveGC(new_w));

	CreateNormalGC(new_w);
	CreateInsensitiveGC(new_w);

	refresh_needed = True;
    }

    if (Lab_Font(new_w) != Lab_Font(old))
    {
	XmFontListFree(Lab_Font(old));
	Lab_Font(old) = NULL;
	if (Lab_Font(new_w) == (XmFontList)XmUNSPECIFIED ||
	    Lab_Font(new_w) == NULL)
	{
	    Lab_Font(new_w) = _XmGetDefaultFontList(new_w,
		XtClass(new_w) == xmLabelWidgetClass
		? XmLABEL_FONTLIST
		: XmBUTTON_FONTLIST);
	}
	else
	{
	    Lab_Font(new_w) = XmFontListCopy(Lab_Font(new_w));
	}

	refresh_needed = True;
	if (Lab_IsText(new_w) || Lab_AcceleratorText(new_w))
	{
	    recalc_needed = True;
	}
    }

    /* HERE */
    if (Lab_AcceleratorText(new_w) != Lab_AcceleratorText(old)) {
	if (Lab_AcceleratorText(new_w)) {
	    Lab_AcceleratorText(new_w) = _XmStringCreate((XmString)Lab_AcceleratorText(new_w));
	} else {
	    /* If the accelerator is removed, its margin is too */
	    if (Lab_StringDirection(old) == XmSTRING_DIRECTION_L_TO_R)
		Lab_MarginRight(new_w) = 0;
	    else
		Lab_MarginLeft(new_w) = 0;
	}   

	_XmStringFree(Lab_AcceleratorText(old));

	recalc_needed = True;
	refresh_needed = True;
    }

    if (Lab_AcceleratorText(new_w) && Lab_StringDirection(new_w) != Lab_StringDirection(old)) {
	Lab_MarginLeft(new_w) = 0;
	Lab_MarginRight(new_w) = 0;
	recalc_needed = True;
    }

    if (Lab_MnemonicCharset(new_w) != Lab_MnemonicCharset(old)) {
	Lab_MnemonicCharset(new_w) = XtNewString(Lab_MnemonicCharset(new_w));

	XtFree(Lab_MnemonicCharset(old));

	refresh_needed = True;
    }

    if (Lab_Label(new_w) == NULL) {
	XmString tmp;

	tmp = XmStringCreateSimple(XtName(new_w));
	Lab_Label(new_w) = _XmStringCreate(tmp);
	XmStringFree(tmp);

	_XmStringFree(Lab_Label(old));

	if (Lab_IsText(new_w)) {
	    recalc_needed = True;
	}
    } else if (Lab_Label(new_w) != Lab_Label(old)) {
	if (_XmStringIsXmString((XmString)Lab_Label(new_w))) {
	    Lab_Label(new_w) = _XmStringCreate((XmString)Lab_Label(new_w));
	}

	_XmStringFree(Lab_Label(old));

	if (Lab_IsText(new_w)) {
	    recalc_needed = True;
	}
    }

    if (Lab_Highlight(new_w) != Lab_Highlight(old)
	|| Lab_Shadow(new_w) != Lab_Shadow(old)
	|| Lab_MarginTop(new_w) != Lab_MarginTop(old)
	|| Lab_MarginBottom(new_w) != Lab_MarginBottom(old)
	|| Lab_MarginLeft(new_w) != Lab_MarginLeft(old)
	|| Lab_MarginRight(new_w) != Lab_MarginRight(old)
	|| Lab_MarginWidth(new_w) != Lab_MarginWidth(old)
	|| Lab_MarginHeight(new_w) != Lab_MarginHeight(old)
	|| Lab_Alignment(new_w) != Lab_Alignment(old)
	|| Lab_StringDirection(new_w) != Lab_StringDirection(old))
    {
	relayout_needed = True;
    }

    if (Lab_IsPixmap(new_w))
    {
	/* check for change in insensitive pixmap */
	if ((Lab_PixmapInsensitive(new_w) != Lab_PixmapInsensitive(old))
	    && !XtSensitive(new_w))
	{
	    recalc_needed = True;
	}

	/* check for change in pixmap */
	if (Lab_Pixmap(new_w) != Lab_Pixmap(old))
	{
	    /* If changed pixmap to UNSPECIFIED,
	     * automatically configure to a string.
	     */
	    if (Lab_IsPixmap(new_w)
		&& Lab_Pixmap(new_w) == (Pixmap)XmUNSPECIFIED_PIXMAP)
	    {
		Lab_LabelType(new_w) = XmSTRING;
	    }
	    if (XtSensitive(new_w))
	    {
		recalc_needed = True;
	    }
	}
    }

    /* did the label change types? */
    if (Lab_LabelType(new_w) != Lab_LabelType(old))
    {
	recalc_needed = True;
    }

    if (Lab_Accelerator(new_w) != Lab_Accelerator(old))
    {

	if (Lab_Accelerator(new_w))
	{
	    Lab_Accelerator(new_w) = XtNewString(Lab_Accelerator(new_w));
	}

	_XmManagerUninstallAccelerator(XtParent(new_w), new_w);
	_XmManagerInstallAccelerator(XtParent(new_w), new_w,
				     Lab_Accelerator(new_w));
    }
    if (Lab_Mnemonic(new_w) != Lab_Mnemonic(old))
    {
	_XmManagerUninstallMnemonic(XtParent(new_w), new_w);
	_XmManagerInstallMnemonic(XtParent(new_w), new_w, Lab_Mnemonic(new_w));
	refresh_needed = True;
    }

    if (XtWidth(new_w) == 0 || XtHeight(new_w) == 0)
    {
	relayout_needed = True;
    }

    /* Resize the internal parts if something about them changed */
    if (recalc_needed)
    {
	_XmCalcLabelDimensions(new_w);

	relayout_needed = True;
    }

    if (relayout_needed)
    {
	Dimension new_width, new_height;

	/* Get a new size if recomputeSize */
	if (Lab_RecomputeSize(new_w))
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
	xmLabelClassRec.core_class.resize(new_w);
	XtWidth(new_w) = new_width;
	XtHeight(new_w) = new_height;

	refresh_needed = True;
    }

    return refresh_needed;
}

static void
XmLabelExpose(Widget w, XEvent *event, Region region)
{
	XRectangle cliprect;
	GC myGC;
	Pixmap mypm;

	DEBUGOUT(_LtDebug(__FILE__, w, "Expose geo %dx%d+%d+%d bw %d hlt %d\n",
				XtX(w), XtY(w), XtWidth(w), XtHeight(w), XtBorderWidth(w),
				Prim_HighlightThickness(w)));

	/*
	* I'm becoming paranoid - see testXm/filesb/test3
	*/
	if (!XtIsRealized(w)) {
		_XmWarning(w, "expose called on unrealized widget - this shouldn't happen");
		return;
	}

	if (Prim_Highlighted(w)) {
		(*PrimC_BorderHighlight(XtClass(w))) (w);
	} else {
		(*PrimC_BorderUnhighlight(XtClass(w))) (w);
	}

	/* use the right GC */
	if (XtSensitive(w)) {
		myGC = Lab_NormalGC(w);
	} else {
		myGC = Lab_InsensitiveGC(w);
	}

	/* Draw the accelerator without clipping */
	if (Lab_AcceleratorText(w)) {
		DEBUGOUT(_LtDebug(__FILE__, w, "Expose: acctextrect %d %d %dx%d\n",
			Lab_AccTextRect(w).x, Lab_AccTextRect(w).y,
			Lab_AccTextRect(w).width, Lab_AccTextRect(w).height));
		_XmStringDraw(XtDisplay(w), XtWindow(w), Lab_Font(w),
				Lab_AcceleratorText(w), myGC,
				Lab_AccTextRect(w).x, Lab_AccTextRect(w).y,
				Lab_AccTextRect(w).width,
				XmALIGNMENT_BEGINNING, Lab_StringDirection(w), NULL);
	}

	/* Set a clip rectangle for the GC - ensure we don't overwrite shadows */
	/* the rectangle used to include MarginWidth and MarginHeight.  Leaving
	 * those two out makes Xinvest and Xmcd look better */
	/*
	 * However, not accounting for the margin screws up the default button
	 * rendering; PushButton will fiddle with the margins to keep the label
	 * from drawing over the extra shadow.  Basically we want to make the
	 * label draw itself as though it were smaller than it is but we want
	 * to keep geometry management doing the "right thing" so we can't
	 * just fiddle with XmNwidth and XmNheight.  The problem will only
	 * show up if a PushButton is forced to be smaller than it wants to
	 * be _and_ there is a defaultButton.  Inspection has revealed that
	 * the (Top|Bottom|Left|Right)Margin's are the ones we want (yes,
	 * I did count the pixels).
	 *
	 * See similar things in LabelG.c.
	 */
	cliprect.x = Prim_HighlightThickness(w) + Prim_ShadowThickness(w) + Lab_MarginLeft(w);
	cliprect.y = Prim_HighlightThickness(w) + Prim_ShadowThickness(w) + Lab_MarginTop(w);
	cliprect.width  = XtWidth(w)
		- ((Prim_ShadowThickness(w) + Prim_HighlightThickness(w)) << 1)
		- Lab_MarginLeft(w) - Lab_MarginRight(w);
	cliprect.height = XtHeight(w)
		- ((Prim_ShadowThickness(w) + Prim_HighlightThickness(w)) << 1)
		- Lab_MarginTop(w) - Lab_MarginBottom(w);

	DEBUGOUT(_LtDebug(__FILE__, w, "Expose: cliprect %d %d %dx%d\n",
				cliprect.x, cliprect.y, cliprect.width, cliprect.height));

	if (Lab_IsText(w)) {		/* LabelString */
		if (Lab_Mnemonic(w)) {
			char m[2];

			m[0] = Lab_Mnemonic(w);
			m[1] = '\0';

			_XmStringDrawMnemonic(XtDisplay(w), XtWindow(w),
				  Lab_Font(w), Lab_Label(w), myGC,
				  Lab_TextRect_x(w),
				  Lab_TextRect_y(w),
				  Lab_TextRect_width(w),
				  Lab_Alignment(w),
				  Lab_StringDirection(w),
				  &cliprect,
				  m, Lab_MnemonicCharset(w));
		} else {
			_XmStringDraw(XtDisplay(w), XtWindow(w),
					Lab_Font(w), Lab_Label(w), myGC,
					Lab_TextRect_x(w), Lab_TextRect_y(w),
					Lab_TextRect_width(w), Lab_Alignment(w),
					Lab_StringDirection(w), &cliprect);
		}
	} else {
		/* Copy only the required part of the pixmap
		* (faster than sending a clip to the server).
		*/
		mypm = XtSensitive(w) ? Lab_Pixmap(w) : Lab_PixmapInsensitive(w);
		if (mypm == XmUNSPECIFIED_PIXMAP) {
			mypm = Lab_Pixmap(w);
		}
		if (mypm > XmUNSPECIFIED_PIXMAP &&
				cliprect.x + cliprect.width > Lab_TextRect_x(w) &&
				cliprect.x < Lab_TextRect_x(w) + Lab_TextRect_width(w) &&
				cliprect.y + cliprect.height > Lab_TextRect_y(w) &&
				cliprect.y < Lab_TextRect_y(w) + Lab_TextRect_height(w))
		{
			unsigned int tmp, tmpw, tmph;
			int tmpx, tmpy;
			int Depth;
			Window tmpwin;
			char *name ;
			Pixel fg, bg ;
			int hotx, hoty ;
			u_int wid, hgt ;

			if (cliprect.x < Lab_TextRect_x(w)) {
				cliprect.width -= Lab_TextRect_x(w) - cliprect.x;
				cliprect.x = Lab_TextRect_x(w);
			}
			if (cliprect.x + cliprect.width > Lab_TextRect_x(w) + Lab_TextRect_width(w))
			{
				cliprect.width = (Lab_TextRect_x(w) + Lab_TextRect_width(w))
					- cliprect.x;
			}
			if (cliprect.y < Lab_TextRect_y(w)) {
				cliprect.height -= Lab_TextRect_y(w) - cliprect.y;
				cliprect.y = Lab_TextRect_y(w);
			}
			if (cliprect.y + cliprect.height >
					Lab_TextRect_y(w) + Lab_TextRect_height(w)) {
				cliprect.height = (Lab_TextRect_y(w) + Lab_TextRect_height(w)) - cliprect.y;
			}

			/* rws 4 Mar 2000
			 * According to the comments in label/test12, this does not
			 * happen. We should be adding the pixmap to our pixmap
			 * cache, and getting the info from there instead of a
			 * round trip server query.
			 */
			/* rws 10 Mar 2000
			 * Apparently a Motif 2 thing.
			 */
			/*
			 * eaf 3 Apr 2000 use the cache for this.  Tests indicate
			 * that Motif does this in set_values, but I like waiting
			 * until we actually need the information.
			 */
			if( !_XmGetPixmapData(XtScreen(w), mypm, &name,
						&Depth, &fg, &bg, &hotx, &hoty, &wid, &hgt)) {
				/*
				 * This pixmap has never been seen before.
				 * Generate a unique name and add it to the cache.
				 * The cache mechanism will obtain the depth.
				 * Then we can try again.
				 */
				char newname[64] ;
				sprintf(newname, "--anon pixmap %p-%x",
					(void *)XtScreen(w), (unsigned)mypm) ;
				(void) _XmInstallPixmap(mypm, XtScreen(w), newname,
							Prim_Foreground(w), XtBackground(w)) ;

				/* Ok, let's try again. */
				if( !_XmGetPixmapData(XtScreen(w), mypm, &name,
						&Depth, &fg, &bg, &hotx, &hoty, &wid, &hgt)) {
					/* Still not found?
					 * Don't panic, ask the server directly.
					 */
					unsigned uDepth;
					XGetGeometry(XtDisplayOfObject(w), mypm, &tmpwin,
						&tmpx, &tmpy, &tmpw, &tmph, &tmp, &uDepth);
					Depth=uDepth;
				}
			}

			if (Depth == 1) {
				XCopyPlane(XtDisplay(w), mypm, XtWindow(w), myGC,
						cliprect.x - Lab_TextRect_x(w),
						cliprect.y - Lab_TextRect_y(w),
						cliprect.width, cliprect.height,
						cliprect.x, cliprect.y, 1);
			} else if (Depth > 1) {
				XCopyArea(XtDisplay(w), mypm, XtWindow(w), myGC,
						cliprect.x - Lab_TextRect_x(w),
						cliprect.y - Lab_TextRect_y(w),
						cliprect.width, cliprect.height,
						cliprect.x, cliprect.y);
			}

			if (!XtSensitive(w) && Lab_PixmapInsensitive(w) == XmUNSPECIFIED_PIXMAP) {
				/* label/test14 */
				XFillRectangle(XtDisplay(w), XtWindow(w), myGC,
					cliprect.x, cliprect.y, cliprect.width, cliprect.height);
			}
		}
	}
}

static void
resize(Widget w)
{
	if (!XmIsLabel(w)) {
		_XmWarning(w, "Label resize called on non-Label widget - this shouldn't happen");
		return;
	}

	/* Make sure there's room for the accelerator */
	if (Lab_AcceleratorText(w)) {
		if (Lab_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R) {
			if (Lab_MarginRight(w) < LABEL_ACC_PAD + Lab_AccTextRect(w).width)
				Lab_MarginRight(w) = LABEL_ACC_PAD + Lab_AccTextRect(w).width;
		} else {
			if (Lab_MarginLeft(w) < LABEL_ACC_PAD + Lab_AccTextRect(w).width)
				Lab_MarginLeft(w) = LABEL_ACC_PAD + Lab_AccTextRect(w).width;
		}
	}

	/* Set our preferred dimensions if they are currently zero */
	if (!XtWidth(w))
		preferred_size(w, &XtWidth(w), NULL);
	if (!XtHeight(w))
		preferred_size(w, NULL, &XtHeight(w));

	/* An RtoL stringDirection reverses the effect of alignment here */
	switch (Lab_Alignment(w) ^ (Lab_StringDirection(w) << 1)) {
	case XmALIGNMENT_BEGINNING:
		Lab_TextRect_x(w) = (Lab_Highlight(w) + Lab_Shadow(w)
				+ Lab_MarginWidth(w) + Lab_MarginLeft(w));
		break;

	case XmALIGNMENT_CENTER:
	default:
		Lab_TextRect_x(w) = ((XtWidth(w) - Lab_MarginLeft(w) - Lab_MarginRight(w)
					- Lab_TextRect_width(w)) >> 1) + Lab_MarginLeft(w);
		break;

	case XmALIGNMENT_END:
		Lab_TextRect_x(w) = (XtWidth(w) - Lab_Highlight(w) - Lab_Shadow(w)
				- Lab_MarginWidth(w) - Lab_MarginRight(w) - Lab_TextRect_width(w));
		break;
	}

	Lab_TextRect_y(w) = ((XtHeight(w) - Lab_MarginTop(w) - Lab_MarginBottom(w)
				- Lab_TextRect_height(w)) >> 1) + Lab_MarginTop(w);

	if (Lab_AcceleratorText(w)) {
		Lab_AccTextRect(w).x = Lab_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R
			? (XtWidth(w) - Lab_Highlight(w) - Lab_Shadow(w)
					- Lab_MarginWidth(w) - Lab_MarginRight(w) + LABEL_ACC_PAD)
			: (Lab_Highlight(w) + Lab_Shadow(w) + Lab_MarginWidth(w)
					+ Lab_MarginLeft(w) - LABEL_ACC_PAD
					- Lab_AccTextRect(w).width);

		Lab_AccTextRect(w).y = ((XtHeight(w) - Lab_MarginTop(w) - Lab_MarginBottom(w)
					- Lab_AccTextRect(w).height) >> 1) + Lab_MarginTop(w);
#if 0
		DEBUGOUT(_LtDebug(__FILE__, w, "Accelerator @ %d %d (x = %d - %d - %d - %d)\n",
			Lab_AccTextRect(w).x, Lab_AccTextRect(w).y,
			XtWidth(w), Lab_Shadow(w), Lab_Highlight(w),
			Lab_AccTextRect(w).width));
#endif
	}
	DEBUGOUT(_LtDebug(__FILE__, w, "XmLabel Resize -> tr %dx%d+%d+%d acc %dx%d+%d+%d\n",
		Lab_TextRect_height(w),
		Lab_TextRect_width(w),
		Lab_TextRect_x(w),
		Lab_TextRect_y(w),
		Lab_AccTextRect(w).height,
		Lab_AccTextRect(w).width,
		Lab_AccTextRect(w).x,
		Lab_AccTextRect(w).y));
}

static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
#if 0
    XtWidgetGeometry a;
    Dimension wd, ht;

    DEBUGOUT(_LtDebug(__FILE__, w, "query_geometry\n"));

    wd = XtWidth(w);
    ht = XtHeight(w);

    /* rws 28 Feb 1997
     * Setting height and width to 0 will calculate the minimum
     * size of the label. If recompute is false we do not want to
     * do that. This helps form/test4 (and some of my own apps)
     * This, however requires a change to the initialize code
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

    if (!Lab_RecomputeSize(w))
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

    if (XmIsCascadeButton(w))
    {
	extern void _XmCBCalcDimensions(Widget w);

	_XmCBCalcDimensions(w);
    }
    else
    {
	preferred_size(w, &XtWidth(w), &XtHeight(w));
    }

    a.width = XtWidth(w);
    a.height = XtHeight(w);
    a.request_mode = CWWidth | CWHeight;

    XtWidth(w) = wd;
    XtHeight(w) = ht;

    if (answer)
    {
	*answer = a;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "query_geometry => %s\n",
		      _LtDebugWidgetGeometry2String(answer)));

    if ((proposed->request_mode & (CWWidth | CWHeight)) ==
	(CWWidth | CWHeight) &&
	proposed->width == answer->width && proposed->height == answer->height)
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

	pc = *proposed;
	/* rws 27 Sep 1998
	   label/test8 should prove whether this IsRealized should be here
	   or not, and it should not.
	   label/test9 should prove whether Lab_RecomputeSize should be here
	   or not,
	 */
	if (Lab_RecomputeSize(w)) {
		preferred_size(w, &answer->width, &answer->height);
		DEBUGOUT(_LtDebug(__FILE__, w, "query_geometry (preferred_size) -> %d %d\n",
					answer->width, answer->height));

	}
	else
	{
	    answer->width = XtWidth(w);
	    answer->height = XtHeight(w);
		DEBUGOUT(_LtDebug(__FILE__, w, "query_geometry -> %d %d\n",
					answer->width, answer->height));
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

static Boolean
widget_baseline(Widget w, Dimension **baselines, int *nbaselines)
{
    if (!Lab_IsText(w))
	return False;

    *nbaselines = _XmStringLineCount(Lab_Label(w));
    *baselines = (Dimension *)XtMalloc(sizeof(Dimension) * *nbaselines);

    /* The thing to do would be to go through the string here,
     * gathering baselines as we go.  But as it relies on functions
     * static to XmString.c, do the work there.
     */

    _XmStringBaselines(Lab_Font(w), Lab_Label(w), Lab_TextRect_y(w),
		       *baselines);
    return True;
}

static Boolean
widget_display_rect(Widget w, XRectangle *rect)
{
    *rect = ((XmLabelWidget)w)->label.TextRect;
    return True;
}

/* FIX ME: there has to be more to this, doesn't there? */
static void
set_override_callback(Widget w)
{
    Lab_SkipCallback(w) = True;
}

static void
export_label_string(Widget w, int offset, XtArgVal *value)
{
    _XmString str = *(_XmString *)(((char *)w) + offset);

    *value = str
	? (XtArgVal)_XmStringCreateExternal(Lab_Font(w), str)
	: (XtArgVal)NULL;
}

static void
preferred_size(Widget w, Dimension *width, Dimension *height)
{
    if (width)
    {
	*width =
	    ((Lab_Highlight(w) + Lab_Shadow(w) + Lab_MarginWidth(w)) << 1)
	    + Lab_MarginLeft(w) + (Lab_AcceleratorText(w) &&
				   Lab_MarginRight(w) <
				   LABEL_ACC_PAD + Lab_AccTextRect(w).width
				   ? LABEL_ACC_PAD + Lab_AccTextRect(w).width
				   : Lab_MarginRight(w))
	    + Lab_TextRect_width(w);
	if (*width == 0)
	    *width = 1;
    }

    if (height)
    {
	*height =
	    ((Lab_Highlight(w) + Lab_Shadow(w) + Lab_MarginHeight(w)) << 1)
	    + Lab_MarginTop(w) + Lab_MarginBottom(w)
	    + (Lab_AcceleratorText(w) &&
	       Lab_AccTextRect(w).height > Lab_TextRect_height(w)
	       ? Lab_AccTextRect(w).height
	       : Lab_TextRect_height(w));
	if (*height == 0)
	    *height = 1;
    }
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
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:drag_drop_finish(%d)\n",
    	__FILE__, __LINE__));
}

static void
ProcessDrag(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Atom export_target[3];
    Arg args[10];
    int n = 0;
    Widget dc;

    _XmObjectLock(w);
    DEBUGOUT(_LtDebug(__FILE__, w, "Processing a drag-drop request\n"));

    if (Lab_IsPixmap(w))
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
    XtSetArg(args[n], XmNsourceCursorIcon, _XmGetTextualDragIcon(w)); n++;

    dc = XmDragStart(w, event, args, n);

    if (dc)
    {
	XtAddCallback(dc, XmNdragDropFinishCallback, drag_drop_finish, NULL);
    }
    _XmObjectUnlock(w);
}

static void
Help(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    _XmObjectLock(w);
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
    _XmObjectUnlock(w);
}

void
_XmLabelGetPixmapSize(Widget w, Pixmap Pix,
		      Dimension *width, Dimension *height)
{
    unsigned int tmp, tmpw, tmph;
    int tmpx, tmpy;
    unsigned Depth;
    Window tmpwin;

    _XmObjectLock(w);

    if (Pix == XmUNSPECIFIED_PIXMAP || Pix == None)
    {
	*width = *height = 0;
	_XmObjectUnlock(w);
	return;
    }

    XGetGeometry(XtDisplayOfObject(w),
		 Pix,
		 &tmpwin,
		 &tmpx, &tmpy,
		 &tmpw, &tmph,
		 &tmp, &Depth);

    *width = (Dimension)tmpw;
    *height = (Dimension)tmph;
    _XmObjectUnlock(w);
}

void
_XmCalcLabelDimensions(Widget w)
{
	_XmObjectLock(w);

	if (Lab_IsText(w)) {
		__XmRenderTableFinalise(w, Lab_Font(w), Lab_Label(w));
		_XmStringExtent(Lab_Font(w), Lab_Label(w), &Lab_TextRect_width(w),
				&Lab_TextRect_height(w));
		if (!Lab_TextRect_width(w)) {
			/* This used to be in _XmStringExtent, but fits better here: */
			/*
			 * I added this because of nedit.  The height of a menu item is
			 * is set to 0 (well, really 1); however, without this code,
			 * XmCalcLabelDimensions returns a different value, even though
			 * the true height of the label is 0.  Do not comment this code
			 * (if part).
			 */
			Lab_TextRect_height(w) = 0;
		}
	} else {
		Pixmap mypm;

		mypm = XtIsSensitive(w) ? Lab_Pixmap(w) : Lab_PixmapInsensitive(w);
		if (mypm == XmUNSPECIFIED_PIXMAP) {
			mypm = Lab_Pixmap(w);
		}

		_XmLabelGetPixmapSize(w, mypm, &Lab_TextRect_width(w), &Lab_TextRect_height(w));
	}

	if (Lab_AcceleratorText(w)) {
		_XmStringExtent(Lab_Font(w), Lab_AcceleratorText(w),
				&Lab_AccTextRect(w).width, &Lab_AccTextRect(w).height);
		if (!Lab_AccTextRect(w).width) {
			Lab_AccTextRect(w).height = 0;
		}
	} else {
		Lab_AccTextRect(w).height = 0;
		Lab_AccTextRect(w).width = 0;
		Lab_AccTextRect(w).x = 0;
		Lab_AccTextRect(w).y = 0;
	}
	_XmObjectUnlock(w);

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmCalcLabelDimensions -> tr %dx%d+%d+%d acc %dx%d+%d+%d\n",
		Lab_TextRect_height(w),
		Lab_TextRect_width(w),
		Lab_TextRect_x(w),
		Lab_TextRect_y(w),
		Lab_AccTextRect(w).height,
		Lab_AccTextRect(w).width,
		Lab_AccTextRect(w).x,
		Lab_AccTextRect(w).y));
}

Widget
XmCreateLabel(Widget parent, char *name,
	      Arg *arglist, Cardinal argcount)
{
	return XtCreateWidget(name, xmLabelWidgetClass, parent,
			  arglist, argcount);
}

static void
_XmLabelSetRenderTable(Widget w, int o, XrmValue *v)
{
	XmLabelWidget		lw = (XmLabelWidget)w;
	static const XmFontList	nullfl = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmLabelSetRenderTable (%d)\n",
		lw->label.check_set_render_table));

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
		v->addr = (char*)&(lw->label.font);
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
