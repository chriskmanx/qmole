/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Scale.c,v 1.2 2004/10/27 20:57:58 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Id: Scale.c,v 1.2 2004/10/27 20:57:58 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ScaleP.h>
#include <Xm/LabelGP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/BulletinBP.h>
#include <Xm/MenuShellP.h>
#include <Xm/VendorSP.h>
#include <Xm/VendorSEP.h>

#include <XmI/DebugUtil.h>


#define USE_LAYOUT	1

#define CHILD_LAB	0
#define CHILD_SB	1

/* FIX ME/BUGS
 *   - highlight
 */

/*
 * these values come from behavior observation of M*tif.  Change at your own
 * risk
 *
 * Hmm, I've observed HSLIDERSIZE to be 30 (M*tif 1.2.2) -- PvH
 */
#define SCB_MAX		1000000000
#define SCB_MIN		0
#define HSLIDERSIZE	/*40*/30
#define VSLIDERSIZE	12
#define MIN_SLIDE	4

#define SB_TRANSVERSAL_SIZE	15
#define SB_LONGITUDAL_SIZE	100

/* T. Straumann: special value to indicate that the value rectangle position
 * is invalid and hence must not be erased.
 */
#define SHOW_VALUE_X_INVALID -65536

#define Scale_ShadowThickness(x) MGR_ShadowThickness(x)

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *request,
				       XtWidgetGeometry *reply);
static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);
static void expose(Widget w, XEvent *event, Region region);
static void resize(Widget w);
static void change_managed(Widget w);

#if 0
static void insert_child(Widget w);
#endif

static XmKidGeometry _XmScalePreferredSize(Widget scale, Widget child,
					   XtWidgetGeometry *childgeom,
					   Dimension *w, Dimension *ht);
static XtGeometryResult _XmScaleGeomRequest(Widget scale,
					    Dimension *wd, Dimension *ht);
static void _XmScaleLayout(Widget scale, XmKidGeometry boxes,
			   Widget child, XtWidgetGeometry *childgeom,
			   Dimension curw, Dimension curh);
static void _XmScaleConfigureChildren(Widget scale,
				      Widget child,
				      XmKidGeometry boxes);
static int _XmScaleConvertWidthToSliderSize(Widget w);
static int _XmScaleConvertSCBValueToScaleValue(Widget w, int value);
static int _XmScaleConvertScaleValueToSCBValue(Widget w);
static void _XmScaleConvertScaleIncrementToSCBIncrements(Widget w,
							 int* inc,
							 int* page_inc);
static void _ScaleValueChanged(Widget sb, XtPointer cd, XtPointer data);
static void _ScaleDrag(Widget sb, XtPointer cd, XtPointer data);
static void computeValueSize(Widget w);
static void showValue(Widget w, int scb_value, int scale_value);

#if 0
static void GetFocus(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);
static void LoseFocus(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);
#endif

static void _XmScaleProcessingDirectionDefault(Widget w,
					       int offset,
					       XrmValue *val);

/*
 * Resources for the scale class 
 */
#define Offset(field) XtOffsetOf(XmScaleRec, scale.field)
static XtResource resources[] =
{
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmScaleRec, manager.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNvalue, XmCValue, XmRInt,
	sizeof(int), Offset(value),
	XmRImmediate, (XtPointer)INT_MAX
    },
    {
	XmNmaximum, XmCMaximum, XmRInt,
	sizeof(int), Offset(maximum),
	XmRImmediate, (XtPointer)100
    },
    {
	XmNminimum, XmCMinimum, XmRInt,
	sizeof(int), Offset(minimum),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmVERTICAL
    },
    {
	XmNprocessingDirection, XmCProcessingDirection, XmRProcessingDirection,
	sizeof(unsigned char), Offset(processing_direction),
	XmRCallProc, (XtPointer)_XmScaleProcessingDirectionDefault
    },
    {
	XmNtitleString, XmCTitleString, XmRXmString,
	sizeof(XmString), Offset(title),
	XtRImmediate, (XtPointer)NULL
    },
    {
	XmNfontList, XmCFontList, XmRFontList,
	sizeof(XmFontList), Offset(font_list),
	XtRImmediate, (XtPointer)NULL
    },
    {
	XmNshowValue, XmCShowValue, XmRBoolean,
	sizeof(Boolean), Offset(show_value),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNdecimalPoints, XmCDecimalPoints, XmRShort,
	sizeof(short), Offset(decimal_points),
	XmRImmediate, (XtPointer)(short)0
    },
    {
	XmNscaleWidth, XmCScaleWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(scale_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNscaleHeight, XmCScaleHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(scale_height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNhighlightOnEnter, XmCHighlightOnEnter, XmRBoolean,
	sizeof(Boolean), Offset(highlight_on_enter),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNvalueChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNdragCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(drag_callback),
	XmRCallback, (XtPointer)NULL
    },
#if XmVERSION > 1
    {
	XmNeditable, XmCEditable, XmRBoolean,
	sizeof(Boolean), Offset(editable),
	XmRImmediate, (XtPointer)True
    },
#endif
    {
	XmNscaleMultiple, XmCScaleMultiple, XmRInt,
	sizeof(int), Offset(scale_multiple),
	XmRImmediate, (XtPointer)0
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNtitleString,
	sizeof(XmString), Offset(title),
	_XmExportXmString, NULL
    },
    {
	XmNscaleWidth,
	sizeof(Dimension), Offset(scale_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNscaleHeight,
	sizeof(Dimension), Offset(scale_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
};

#if 0
static char defaultTranslations[] =
    "<FocusIn>: FocusIn() \n\
     <FocusOut>: FocusOut()";

static XtActionsRec actions[] =
{
    {"FocusIn", GetFocus},
    {"FocusOut", LoseFocus}
};
#endif

static XmBaseClassExtRec _XmScaleCoreClassExtRec = {
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
    /* widget_navigable          */ XmInheritWidgetNavigable, /* Motif has one */
    /* focus_change              */ XmInheritFocusChange, /* Motif has NULL */
    /* wrapper_data              */ NULL
};

#if 0
static XmManagerClassExtRec _XmScaleMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIX ME */
};
#endif

XmScaleClassRec xmScaleClassRec =
{
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) & xmManagerClassRec,
	/* class_name            */ "XmScale",
	/* widget_size           */ sizeof(XmScaleRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ NULL /*actions*/,
	/* num_actions           */ 0 /*XtNumber(actions)*/,
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
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations /*defaultTranslations*/,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL /*XtInheritDisplayAccelerator*/,
	/* extension             */ (XtPointer)&_XmScaleCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager,
	/* change_managed   */ change_managed,
	/* insert_child     */ XtInheritInsertChild /*insert_child*/,
	/* delete_child     */ XtInheritDeleteChild,
	/* extension        */ NULL,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,
	/* subresource_count */ 0,
	/* constraint_size   */ 0,
	/* initialize        */ NULL,
	/* destroy           */ NULL,
	/* set_values        */ NULL,
	/* extension         */ NULL,
    },
    /* XmManager class part */
    {
        /* translations                 */ XtInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
        /* extension                    */ (XtPointer)NULL /*&_XmScaleMClassExtRec*/
    },
    /* XmScale part */
    {
	/* extension */ NULL,
    },
};



WidgetClass xmScaleWidgetClass = (WidgetClass)&xmScaleClassRec;


static void
class_initialize(void)
{
    _XmScaleCoreClassExtRec.record_type = XmQmotif;
}


static void
class_part_initialize(WidgetClass widget_class)
{
    XmScaleWidgetClass scclass = (XmScaleWidgetClass)widget_class;
    CompositeClassExtension ext, *extptr;

    extptr = (CompositeClassExtension *)_XmGetClassExtensionPtr(
		    (XmGenericClassExt *)&(scclass->composite_class.extension),
								   NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = scclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    scclass->composite_class.extension = (XtPointer)ext;
	}
    }

    _XmFastSubclassInit(widget_class, XmSCALE_BIT);
}


static void
CreateForegroundGC(Widget w)
{
    XtGCMask mask;
    XGCValues values;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction | GCFont |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = MGR_Foreground(w);
    values.background = XtBackground(w);
    values.fill_style = FillSolid;
    values.font = Scale_FontStruct(w)->fid;

    Scale_ForegroundGC(w) = XtGetGC(w, mask, &values);
}


static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Widget title;
    Widget sb;
    Arg argl[15];
    int argc;
    /*
    XmKidGeometry boxes;
    Dimension wd, ht;
    */

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

    if (Scale_ScaleWidth(new_w) == 0)
    {
    	/* rws 22 Dec 1999
    	   scale/test18 scale/test16
    	 */
	if (Scale_Orientation(new_w) == XmHORIZONTAL)
	{
	    Scale_ScaleWidth(new_w) = XtWidth(request);
    	}
    	else
	{
	    Scale_ScaleWidth(new_w) = XtHeight(request);
    	}
    }
    if (Scale_Value(new_w) == INT_MAX)
    {
	if (Scale_Minimum(new_w) >= 0)
	{
	    Scale_Value(new_w) = Scale_Minimum(new_w);
	}
	else
	{
	    Scale_Value(new_w) = 0;
	}
    }

    if (Scale_ScaleMultiple(new_w) == 0)
    {
	Scale_ScaleMultiple(new_w) =
	    (Scale_Maximum(new_w) - Scale_Minimum(new_w)) / 10;
    }

    Scale_LastValue(new_w) = Scale_Value(new_w);

	/* T. Straumann: there is no old value to be erased in showValue() */
	Scale_ShowValueX(new_w) = SHOW_VALUE_X_INVALID;

    if (Scale_FontList(new_w) == NULL)
    {
	Scale_FontList(new_w) = _XmGetDefaultFontList(new_w, XmLABEL_FONTLIST);
    }
    /* if user specified fontlist, COPY it */
    else
    {
	Scale_FontList(new_w) = XmFontListCopy(Scale_FontList(new_w));
    }

    /* MLM FIX ME -- Should the titleString get the FontList from the scale? */
    argc = 0;
    if (Scale_Title(new_w) != NULL)
    {
	Scale_Title(new_w) = XmStringCopy(Scale_Title(new_w));
    }
    XtSetArg(argl[argc], XmNlabelString, Scale_Title(new_w)); argc++;
    XtSetArg(argl[argc], XmNalignment, XmALIGNMENT_BEGINNING); argc++;

    title = XmCreateLabelGadget(new_w, "Title", argl, argc);
    if (Scale_Title(new_w) != NULL)
    {
	XtManageChild(title);
    }

    argc = 0;
    XtSetArg(argl[argc], XmNshowArrows, False); argc++;
    XtSetArg(argl[argc], XmNtraversalOn, True); argc++;
    XtSetArg(argl[argc], XmNhighlightOnEnter, True); argc++;
    XtSetArg(argl[argc], XmNmaximum, SCB_MAX); argc++;
    XtSetArg(argl[argc], XmNminimum, SCB_MIN); argc++;
    XtSetArg(argl[argc], XmNorientation, Scale_Orientation(new_w)); argc++;
    XtSetArg(argl[argc], XmNprocessingDirection,
	     Scale_ProcessingDirection(new_w)); argc++;
    XtSetArg(argl[argc], XmNwidth, Scale_ScaleWidth(new_w)); argc++;
    XtSetArg(argl[argc], XmNheight, Scale_ScaleHeight(new_w)); argc++;
    XtSetArg(argl[argc], XmNshadowThickness,
	     Scale_ShadowThickness(new_w)); argc++;
    XtSetArg(argl[argc], XmNhighlightThickness,
	     Scale_HighlightThickness(new_w)); argc++;
    XtSetArg(argl[argc], XmNhighlightOnEnter,
	     Scale_HighlightOnEnter(new_w)); argc++;
	/* T. Straumann:  M*TIF enforces 0 borderWidth for scrollBar */
    XtSetArg(argl[argc], XmNborderWidth, 0); argc++;

    sb = XmCreateScrollBar(new_w, "Scrollbar", argl, argc);

    _XmSetEtchedSlider((XmScrollBarWidget)sb);

    XtAddCallback(sb, XmNdragCallback, _ScaleDrag, NULL);

    if (Scale_Value(new_w) == INT_MAX)
    {
	Scale_Value(new_w) = Scale_Minimum(new_w);
    }

    XtAddCallback(sb, XmNvalueChangedCallback, _ScaleValueChanged, NULL);
    XtManageChild(sb);

    _XmFontListGetDefaultFont(Scale_FontList(new_w), &Scale_FontStruct(new_w));

    if (Scale_FontStruct(new_w) == NULL)
    {
	Scale_FontList(new_w) = _XmFontListCreateDefault(XtDisplay(new_w));

	_XmFontListGetDefaultFont(Scale_FontList(new_w),
				  &Scale_FontStruct(new_w));
    }

    CreateForegroundGC(new_w);

#if 1
    { Dimension ht, wd;
    XmKidGeometry boxes;
    wd = ht = 0;
    boxes = _XmScalePreferredSize(new_w, NULL, NULL, &wd, &ht);
    _XmScaleGeomRequest(new_w, &wd, &ht);
    _XmScaleLayout(new_w, boxes, NULL, NULL, wd, ht);
    _XmScaleConfigureChildren(new_w, NULL, boxes);
    XtFree((char *)boxes);
    }
#endif
}


static void
destroy(Widget w)
{

    if (Scale_Title(w))
     {
         XmStringFree(Scale_Title(w));
     }
    XmFontListFree(Scale_FontList(w));
    XtDestroyGC(Scale_ForegroundGC(w));
}


/*
 * called when the user changes a resource
 */
/*
 * FIX ME: changing sensitivity will move value.
 */
static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh = False;
    Arg argl[8];
    int argc;
    Widget sb, lab;
    int resize = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "XmScale set_values()\n"));

    sb = ((XmScaleWidget)new_w)->composite.children[CHILD_SB];
    lab = ((XmScaleWidget)new_w)->composite.children[CHILD_LAB];

    if (Scale_FontList(new_w) &&
	Scale_FontList(new_w) != (XmFontList)XmUNSPECIFIED &&
	Scale_FontList(new_w) != Scale_FontList(old))
    {
	XmFontListFree(Scale_FontList(old));
	Scale_FontList(new_w) = XmFontListCopy(Scale_FontList(new_w));
    }

    if (MGR_Foreground(new_w) != MGR_Foreground(old) ||
	XtBackground(new_w) != XtBackground(old) ||
	Scale_FontList(new_w) != Scale_FontList(old))
    {
	_XmFontListGetDefaultFont(Scale_FontList(new_w),
				  &Scale_FontStruct(new_w));

	if (Scale_FontStruct(new_w) == NULL)
	{
	    Scale_FontList(new_w) = _XmFontListCreateDefault(XtDisplay(new_w));

	    _XmFontListGetDefaultFont(Scale_FontList(new_w),
				      &Scale_FontStruct(new_w));
	}

	XtReleaseGC(new_w, Scale_ForegroundGC(new_w));
	CreateForegroundGC(new_w);

	refresh = True;
    }

    if (Scale_Title(new_w) != Scale_Title(old))
    {
	if (Scale_Title(old))
	{
	    XmStringFree(Scale_Title(old));
	}

	if (Scale_Title(new_w) != NULL)
	{
	    Scale_Title(new_w) = XmStringCopy(Scale_Title(new_w));

	    argc = 0;

	    XtSetArg(argl[argc], XmNlabelString, Scale_Title(new_w)); argc++;
	    XtSetValues(lab, argl, argc);

	    if (!XtIsManaged(lab))
	    {
	        XtManageChild(lab);
	    }
	}
    }

    if (Scale_Maximum(new_w) != Scale_Maximum(old) ||
	Scale_Minimum(new_w) != Scale_Minimum(old) ||
	Scale_ProcessingDirection(new_w) != Scale_ProcessingDirection(old) ||
	Scale_ScaleMultiple(new_w) != Scale_ScaleMultiple(old) ||
	Scale_HighlightOnEnter(new_w) != Scale_HighlightOnEnter(old) ||
	Scale_HighlightThickness(new_w) != Scale_HighlightThickness(old) ||
	Scale_Value(new_w) != Scale_Value(old))
    {
	int newval;
	int inc, page_inc;


	newval = _XmScaleConvertScaleValueToSCBValue(new_w);
	_XmScaleConvertScaleIncrementToSCBIncrements(new_w, &inc, &page_inc);

	argc = 0;
	XtSetArg(argl[argc], XmNorientation, Scale_Orientation(new_w)); argc++;
	XtSetArg(argl[argc], XmNprocessingDirection,
		 Scale_ProcessingDirection(new_w)); argc++;
	XtSetArg(argl[argc], XmNvalue, newval); argc++;
	XtSetArg(argl[argc], XmNincrement, inc); argc++;
	XtSetArg(argl[argc], XmNpageIncrement, page_inc); argc++;
	XtSetArg(argl[argc], XmNshadowThickness,
		 Scale_ShadowThickness(new_w)); argc++;
	XtSetArg(argl[argc], XmNhighlightThickness,
		 Scale_HighlightThickness(new_w)); argc++;
	XtSetArg(argl[argc], XmNhighlightOnEnter,
		 Scale_HighlightOnEnter(new_w)); argc++;
	XtSetValues(sb, argl, argc);

	refresh = True;
    }

    if (Scale_Orientation(new_w) != Scale_Orientation(old))

    {
	argc = 0;
	XtSetArg(argl[argc], XmNorientation, Scale_Orientation(new_w)); argc++;

	XtSetValues(sb, argl, argc);

	/* Surprise, surprise, M*tif doesn't do a relayout here */
	resize = False;
    }

    if (Scale_ScaleWidth(new_w) != Scale_ScaleWidth(old) ||
	Scale_ScaleHeight(new_w) != Scale_ScaleHeight(old))
    {
	argc = 0;
	if (Scale_ScaleWidth(new_w) != Scale_ScaleWidth(old) ) {
	    XtSetArg(argl[argc], XmNwidth, Scale_ScaleWidth(new_w)); argc++;
	}
	if ( Scale_ScaleHeight(new_w) != Scale_ScaleHeight(old) ) {
	    XtSetArg(argl[argc], XmNheight, Scale_ScaleHeight(new_w)); argc++;
	}

	XtSetValues(sb, argl, argc);
       
#if 0
	resize = False;

	/* FIX ME: adapt for new style _XmScaleLayout */
	_XmScaleLayout(new_w, resize, NULL, False, NULL);
#endif

	refresh = True;
    }

	/* T. Straumann: if Scale_ShowValue changed we have to
	 * do some more things
	 */
	if (Scale_ShowValue(new_w) != Scale_ShowValue(old))
	{
		refresh = True;
		if ( ! Scale_ShowValue(new_w) )
		{
			/* undraw the old value */
			if (XtIsRealized(old) && SHOW_VALUE_X_INVALID != Scale_ShowValueX(old))
			{
				XFillRectangle(XtDisplay(old), XtWindow(old), MGR_BackgroundGC(old),
					Scale_ShowValueX(old), Scale_ShowValueY(old),
					Scale_ShowValueWidth(old), Scale_ShowValueHeight(old));
			}
		}
		/* Mark Scale_ShowValueX as invalid */
		Scale_ShowValueX(new_w) = SHOW_VALUE_X_INVALID;
	}

    if (Scale_Maximum(new_w) != Scale_Maximum(old) ||
        Scale_Minimum(new_w) != Scale_Minimum(old) ||
        Scale_DecimalPoints(new_w) != Scale_DecimalPoints(old) ||
	Scale_FontList(new_w) != Scale_FontList(old))
    {
	refresh = True;

        if (Scale_ShowValue(new_w))
        {
            computeValueSize(new_w);
            if (XtIsRealized(old) && SHOW_VALUE_X_INVALID != Scale_ShowValueX(old))
            {
                XFillRectangle(XtDisplay(old), XtWindow(old), MGR_BackgroundGC(old),
			Scale_ShowValueX(old), Scale_ShowValueY(old),
			Scale_ShowValueWidth(old), Scale_ShowValueHeight(old));    
            } 
            Scale_ShowValueX(new_w) = SHOW_VALUE_X_INVALID;
        }
#if 0
	resize = False;

	/* FIX ME: adapt for new style _XmScaleLayout */
	_XmScaleLayout(new_w, resize, NULL, False, XtWidth(new_w),
		       XtHeight(new_w));
#endif
    }

#if 0
    /* dunno about this */
    XmKidGeometry boxes;
    Dimension wd, ht;

    wd = XtWidth(new_w);
    ht = XtHeight(new_w);
    boxes = _XmScalePreferredSize(new_w, NULL, NULL, &wd, &ht);

    _XmScaleGeomRequest(new_w, &wd, &ht);
    _XmScaleLayout(new_w, boxes, NULL, NULL, wd, ht);
    _XmScaleConfigureChildren(new_w, NULL, boxes);

    XtFree((char *)boxes);
#endif

    return refresh;
}


static void
expose(Widget w,
       XEvent *event,
       Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmScale expose()\n"));

    _XmRedisplayGadgets(w, event, region);

    if (Scale_ShowValue(w))
    {
	showValue(w,
		  _XmScaleConvertScaleValueToSCBValue(w),
		  Scale_Value(w));
    }
}


/*
 * called when our parent wants to find out how we would like to look.  It
 * doesn't have to honor our preference, though.
 */
static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
#if 0
    XtWidgetGeometry wants;
    XmKidGeometry boxes;
    Dimension wd, ht;

#define Wants(x)	((wants.request_mode & x) == x)

    DEBUGOUT(_LtDebug(__FILE__, w, "XmScale %s QueryGeometry\n", XtName(w)));

    wants = *request;
    *reply = *request;
    reply->request_mode = 0;

    if (Wants(CWWidth))
    {
	reply->request_mode |= CWWidth;
	reply->width = XtWidth(w);
    }

    if (Wants(CWHeight))
    {
	reply->request_mode |= CWHeight;
	reply->height = XtHeight(w);
    }

    boxes = _XmScalePreferredSize(w, NULL, NULL, &wd, &ht);

    reply->width = wd;
    reply->height = ht;

    if (Wants(CWHeight) && (wants.height < reply->height))
    {
	return XtGeometryNo;
    }
    else if (Wants(CWHeight) && (wants.height > reply->height))
    {
	return XtGeometryAlmost;
    }

    if (Wants(CWWidth) && (wants.width < reply->width))
    {
	return XtGeometryNo;
    }
    else if (Wants(CWWidth) && (wants.width > reply->width))
    {
	return XtGeometryAlmost;
    }

    return XtGeometryYes;

#undef Wants
#else
    XtWidgetGeometry wants;
    Dimension wd, ht;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmScale %s QueryGeometry\n", XtName(w)));

    wants = *request;

    _XmScalePreferredSize(w, NULL, NULL, &wd, &ht);

    reply->width = wd;
    reply->height = ht;

    return _XmGMReplyToQueryGeometry(w, &wants, reply);
#endif
}


/*
 * called when our parent is resizing us.  We have no choice but to obey.
 */
static void
resize(Widget w)
{
    XmKidGeometry boxes;
    Dimension wd, ht;

    DEBUGOUT(_LtDebug(__FILE__, w, "Scale %s Resize: x %d y %d w %d h %d\n",
		      XtName(w), XtX(w), XtY(w), XtWidth(w), XtHeight(w)));

    Scale_SliderSize(w) = _XmScaleConvertWidthToSliderSize(w);

    wd = XtWidth(w);
    ht = XtHeight(w);
    boxes = _XmScalePreferredSize(w, NULL, NULL, &wd, &ht);

    _XmScaleLayout(w, boxes, NULL, NULL, wd, ht);

    _XmScaleConfigureChildren(w, NULL, boxes);

    XtFree((char *)boxes);
}


/*
 * called when one of our children wants to change.  We control whether
 * the child can, or not
 * Note that the scrolbar can't change it's size.
 */
#define       Wants(x)        (wants.request_mode & x)
static XtGeometryResult
geometry_manager(Widget w, XtWidgetGeometry *request, XtWidgetGeometry *reply)
{
    Dimension wd, ht;
    XmKidGeometry boxes;
    XtWidgetGeometry wants;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmScale geometry_manager()\n"));

    wants = *request;

    if ((Wants(CWX) /* FIX ME: && request->x != XtX(w) */ ) ||
	(Wants(CWY) /* FIX ME: && request->y != XtY(w) */ ))
    {
	return XtGeometryNo;
    }

    /* FIX ME: Should XtCWQueryOnly be handled at all? */
    if (Wants(XtCWQueryOnly))
    {
	_XmWarning(XtParent(w),
		   "FIX ME: XtCWQueryOnly not handled (child %s)\n",
		   XtName(w));
    }

    boxes = _XmScalePreferredSize(XtParent(w), w, request, &wd, &ht);

    _XmScaleGeomRequest(XtParent(w), &wd, &ht);

    *reply = *request;
    reply->request_mode &= CWWidth|CWHeight;

    if (Wants(CWWidth) && Wants(CWHeight) &&
	wants.width == reply->width && wants.height == reply->height)
    {
	_XmScaleConfigureChildren(XtParent(w), w, boxes);

	return XtGeometryYes;
    }

    if (Wants(CWWidth) && wants.width == reply->width)
    {
	_XmScaleConfigureChildren(XtParent(w), w, boxes);

	return XtGeometryYes;
    }

    if (Wants(CWHeight) && wants.height == reply->height)
    {
	_XmScaleConfigureChildren(XtParent(w), w, boxes);

	return XtGeometryYes;
    }

    if (Wants(CWWidth) && wants.width == reply->width)
    {
    	reply->request_mode &= ~CWWidth;
    }
    if (Wants(CWHeight) && wants.height == reply->height)
    {
    	reply->request_mode &= ~CWHeight;
    }
    return XtGeometryAlmost;
#undef Wants
}


/*
 * called when our Window is allocated.  We may want to change our layout,
 * and we can ask our parent to allow this
 */
static void
realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
    /*
    Dimension wd, ht;
    Dimension pwd, pht;
    XmKidGeometry boxes;
    */

    DEBUGOUT(_LtDebug(__FILE__, w, "XmScale realize()\n"));

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass

#if 0
    wd = ht = 0;
    boxes = _XmScalePreferredSize(w, NULL, NULL, &wd, &ht);
    pwd = wd;
    pht = ht;

    _XmScaleGeomRequest(w, &wd, &ht);
    _XmScaleLayout(w, boxes, NULL, NULL, pwd, pht);
    _XmScaleConfigureChildren(w, NULL, boxes);

    XtFree((char *)boxes);
#endif
}


/*
 * one of our children has become managed or unmanaged.  We need to relayout
 */
static void
change_managed(Widget w)
{
    Dimension wd, ht;
    XmKidGeometry boxes;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmScale change_managed()\n"));

    wd = ht = 0;
    boxes = _XmScalePreferredSize(w, NULL, NULL, &wd, &ht);

    _XmScaleGeomRequest(w, &wd, &ht);

    _XmScaleLayout(w, boxes, NULL, NULL, wd, ht);

    _XmScaleConfigureChildren(w, NULL, boxes);

    XtFree((char *)boxes);

    _XmNavigChangeManaged(w);
}


#if 0
/*
 * a child has been added
 */
static void
insert_child(Widget w)
{
#define superclass      (&xmManagerClassRec)
    (*superclass->composite_class.insert_child) (w);
#undef  superclass

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "Scale %s: insert child %s\n",
		      XtName(XtParent(w)), XtName(w)));
}
#endif


extern Widget
XmCreateScale(Widget parent,
	      char *name,
	      Arg *arglist,
	      Cardinal argcount)
{
    return XtCreateWidget(name,
			  xmScaleWidgetClass,
			  parent,
			  arglist,
			  argcount);
}


extern void
XmScaleGetValue(Widget widget,
		int *value_return)
{
    *value_return = Scale_Value(widget);
}


extern void
XmScaleSetValue(Widget widget,
		int value)
{
    Widget sb;
    int newval;

    sb = ((XmScaleWidget)widget)->composite.children[CHILD_SB];

    if (value < Scale_Minimum(widget) || value > Scale_Maximum(widget))
    {
	return;
    }

    Scale_Value(widget) = value;
    newval = _XmScaleConvertScaleValueToSCBValue(widget);

    XtVaSetValues((Widget)sb,
		  XmNvalue, newval,
		  NULL);

    if (Scale_ShowValue(widget))
    {
	showValue(widget, newval, Scale_Value(widget));
    }
}


static void
valueToString(Widget w, char *buf)
{
    int base, i;
    char buf2[17];		/* anybody got larger than 64 bit ints? */

    if (Scale_DecimalPoints(w) <= 0)
    {
	sprintf(buf, "%d", Scale_Value(w));
    }
    else
    {
	/* I'd like to use %d.%*d here, but not everyone has that */
	base = 10;
	for (i = 1; i < Scale_DecimalPoints(w); i++)
	{
	    base *= 10;
	}

#if 0
	sprintf(buf, "%d.", Scale_Value(w) / base);
#else
	sprintf(buf, "%s%d.", Scale_Value(w) >= 0 ? "" : "-",
		abs(Scale_Value(w) / base));
#endif
	for (i = 1; i <= Scale_DecimalPoints(w); i++)
	{
	    strcat(buf, "0");
	}

#if 0
	sprintf(buf2, "%d", Scale_Value(w) % base);
#else
	sprintf(buf2, "%d", abs(Scale_Value(w) % base));
#endif
	strcpy(&buf[strlen(buf) - strlen(buf2)], buf2);
    }
}

/*
 * layout rules
 *
 * scrollbar stays at rightmost or bottommost position, when no label
 * ScaleWidth/ScaleHeight are hints.  If it is set, pay attention to it;
 * otherwise, obey the parent.
 *
 * FIX ME - currently, we always allow a widget to resize.
 *
 * FIX ME: signs are some of the XtIsManaged tests will have to go.
 *
 * FIX ME: if scaleWidth/Height is specified  scrollbar_width/height can
 * get arbitrary small. If width/height is specified slider_width/height
 * is bound to a minimum.
 *
 * FIX ME:
 * Srollbar should not go off the scale by wide tick marks.
 * Children's initial geometry not respected.
 *
 * FIX ME: tick mark x/y seems to have a minimum value of -4
 *
 * layout is normal processing as per the M*tif docs
 * (Hmm.. tick marks and title get laid out on opposite sides of SB -- PvH)
 *  per me, this is as follows (FIX ME -- this is not complete):
 *    if (orientation is HORIZONTAL) then
 *      layout title (if managed), and other children (if present)
 *        along the bottom edge.  This initially defines our preferred
 *        width, and the tallest child defines the slider's bottom edge.
 *        if (TestMode) then
 *          if the child widget argument is the same as the current child
 *             and the geometry request is a match for the calculated
 *             geometry, the return XtGeometryYes.
 *          else
 *             return XtGeometryNo
 *          endif
 *        endif
 *      layout the slider above the tallest managed child other than the
 *        scrollbar (if any).
 *        if (ScaleHeight hint is set) then
 *          we must use that as the slider height
 *        else
 *          use the default slider height
 *        endif
 *        if (ScaleWidth hint is set)
 *          we must use it as the slider width
 *        else
 *          use our width as the slider width
 *        endif
 *        if (TestMode) then
 *          if the child widget argument is the same as the current child
 *             and the geometry request is a match for the calculated
 *             geometry, the return XtGeometryYes.
 *          else
 *             return XtGeometryNo
 *          endif
 *        endif
 *        layout the value (if ShowValue is true) to be from the top of the
 *          slider. The value should not affect the width calculation.
 *    else if (orientation is VERTICAL) then
 *      layout title (if managed), and other children (if present)
 *        along the right edge.  This initially defines our preferred
 *        height, and the tallest child defines the slider's right edge.
 *        if (TestMode) then
 *          if the child widget argument is the same as the current child
 *             and the geometry request is a match for the calculated
 *             geometry, the return XtGeometryYes.
 *          else
 *             return XtGeometryNo
 *          endif
 *        endif
 *      layout the slider left of the tallest managed child other than the
 *        scrollbar (if any).
 *        if (ScaleWidth hint is set) then
 *          we must use that as the slider width
 *        else
 *          use the default slider width
 *        endif
 *        if (ScaleHeight hint is set)
 *          we must use it as the slider height
 *        else
 *          use our height as the slider height
 *        endif
 *        if (TestMode) then
 *          if the child widget argument is the same as the current child
 *             and the geometry request is a match for the calculated
 *             geometry, the return XtGeometryYes.
 *          else
 *             return XtGeometryNo
 *          endif
 *        endif
 *      layout the value (if ShowValue is true) to be from the right of the
 *        slider. The value should not affect the height calculation.
 *    endif
 *
 *    we have now computed our preferred width and height, or returned.
 *    if (ParentResizeMode) then
 *      do
 *        request size change
 *      while (change not allowed, but close)
 *      our width = new width
 *      our height = new height
 *    else
 *      our width = XtWidth
 *      our height = XtHeight
 *    endif
 *
 *    if (our width < preferred width) then
 *      compute layouts that maximize the display of the scrollbar and
 *      the value (if ShowValue is true)
 *    else
 *      compute proportional layouts
 *    endif
 *    if (our height < preferred height) then
 *      compute layouts that maximize the display of the scrollbar and
 *      the value (if ShowValue is true)
 *    else
 *      compute proportional layouts
 *    endif
 *
 *    for each widget
 *      XmConfigureWidget
 *    endfor
 *
 *  end algorithm
 *
 * My additions:
 * never allow a child to request an XY change. (is this correct?)
 * the ScrollBar shall always be children[1]
 * the LabelGadget shall always be children[0]
 * This is per M*tif!
 */
static XmKidGeometry
_XmScalePreferredSize(Widget scale, Widget child, XtWidgetGeometry *childgeom,
		      Dimension *wd, Dimension *ht)
{
    Position curx, cury;
    XmKidGeometry boxes;
    Dimension curw, curh;
    Dimension incw, inch;
    int maxw, maxh;
    Widget *children;
    int num_children;
    int i, cnt;

    curx = cury = 0;
    curw = curh = 0;
    incw = inch = 0;
    maxw = maxh = 0;

    num_children = MGR_NumChildren(scale);
    children = MGR_Children(scale);

    boxes = (XmKidGeometry)XtCalloc(num_children + 1, sizeof(XmKidGeometryRec));

    boxes[CHILD_SB].kid = children[CHILD_SB];
    if (children[CHILD_SB])
    {
	_XmGeoLoadValues(children[CHILD_SB], XmGET_PREFERRED_SIZE,
			 child, childgeom, &boxes[CHILD_SB].box);
    }

    boxes[CHILD_LAB].kid = children[CHILD_LAB];
    if (children[CHILD_LAB])
    {
	_XmGeoLoadValues(children[CHILD_LAB], XmGET_PREFERRED_SIZE,
			 child, childgeom, &boxes[CHILD_LAB].box);
    }

    for (i = 2, cnt = 2; i < num_children; i++)
    {
	if (XtIsManaged(children[i]))
	{
	    boxes[cnt].kid = children[i];

	    _XmGeoLoadValues(children[i], XmGET_PREFERRED_SIZE,
			     child, childgeom, &boxes[cnt].box);

	    if (maxw < boxes[cnt].box.width)
	    {
		maxw = boxes[cnt].box.width;
	    }
	    if (maxh < boxes[cnt].box.height)
	    {
		maxh = boxes[cnt].box.height;
	    }

	    cnt++;
	}
	else
	{
	    boxes[i].kid = NULL;
	}
    }

    boxes[i].kid = NULL;

    num_children = cnt;

    /*
     * compute it.
     */
    if (Scale_Orientation(scale) == XmHORIZONTAL)
    {

	curh = maxh;
	cury = maxh;

	/*
	 * do the value, if there is one
	 */
	if (Scale_ShowValue(scale))
	{

	    computeValueSize(scale);

	    incw = Scale_ShowValueWidth(scale);
	    inch = Scale_ShowValueHeight(scale);

	    if (incw > curw)
	    {
		curw = incw;
	    }
	    curh += inch;

	    cury += inch;
	}

	/*
	 * then do the ScrollBar
	 */
	if (XtIsManaged(boxes[CHILD_SB].kid))
	{

	    boxes[CHILD_SB].box.x = curx;
	    boxes[CHILD_SB].box.y = cury;

	    inch = Scale_ScaleHeight(scale);

	    if (inch == 0)
	    {
		inch = SB_TRANSVERSAL_SIZE +
		    2 * Scale_HighlightThickness(scale);
	    }

	    incw = Scale_ScaleWidth(scale);

	    if (incw == 0)
	    {
		incw = XtWidth(scale);
	    }

#if 0
	    if (incw == 0 && num_children >= 3)
#else
	    if (Scale_ScaleWidth(scale) == 0 && num_children >= 3)
#endif
	    {
		if (num_children == 3)
		{
		    incw = boxes[2].box.width;
		}
		else
		{
		    incw = HSLIDERSIZE +
			2 * (Prim_HighlightThickness(boxes[CHILD_SB].kid) +
			     Prim_ShadowThickness(boxes[CHILD_SB].kid)) +
			maxw * (num_children - 3);
		}
	    }

	    if (incw == 0)
	    {
		incw = SB_LONGITUDAL_SIZE + 2 * Scale_HighlightThickness(scale);
	    }

	    boxes[CHILD_SB].box.width = incw;
	    boxes[CHILD_SB].box.height = inch;

	    if (incw > curw)
	    {
		curw = incw;
	    }

	    curh += inch;
	    cury += inch;
	}

	/* lay out the tick marks */
	if (num_children > 2)
	{
	    /* centres are nicely aligned */

	    if (num_children > 3)
	    {
		int spacing;

		if (curw == 0)
		{
		    spacing = maxw * (num_children - 3);
		}
		else
		{
		    spacing = (curw -
			       (HSLIDERSIZE +
			     2 * (Prim_HighlightThickness(boxes[CHILD_SB].kid) +
				  Prim_ShadowThickness(boxes[CHILD_SB].kid))));
		}

		for (i = 2; i < num_children; i++)
		{
		    boxes[i].box.y = (maxh - boxes[i].box.height) / 2;
		    boxes[i].box.x = (HSLIDERSIZE - boxes[i].box.width) / 2 +
			    Prim_HighlightThickness(boxes[CHILD_SB].kid) +
			    Prim_ShadowThickness(boxes[CHILD_SB].kid) +
			    (i - 2) * spacing / (num_children - 3);
		}
	    }
	    else
	    {
		boxes[2].box.y = 0;
		boxes[2].box.x = (curw - boxes[2].box.width) / 2;
	    }
	}

	/*
	 * then the title, if there is one
	 */
	if (Scale_Title(scale) && XtIsManaged(boxes[CHILD_LAB].kid))
	{
	    boxes[CHILD_LAB].box.x = curx;
	    boxes[CHILD_LAB].box.y = cury;

	    incw = boxes[CHILD_LAB].box.width;
	    inch = boxes[CHILD_LAB].box.height;

	    if (incw > curw)
	    {
		curw = incw;
	    }
	    curh += inch;

	    curx += incw;
	    curh += 2 * MGR_ShadowThickness(scale);
	}
    }
    /* XmNorientation == XmVERTICAL */
    else
    {

	curw = maxw;
	curx = maxw;

	/*
	 * do the value, if there is one
	 */
	if (Scale_ShowValue(scale))
	{

	    computeValueSize(scale);

	    inch = Scale_ShowValueHeight(scale);
	    incw = Scale_ShowValueWidth(scale);

	    if (inch > curh)
	    {
		curh = inch;
	    }

	    curw += incw;

	    curx += incw;
	}

	/*
	 * then do the ScrollBar
	 */
	if (XtIsManaged(boxes[CHILD_SB].kid))
	{
	    boxes[CHILD_SB].box.x = curx;
	    boxes[CHILD_SB].box.y = cury;

	    incw = Scale_ScaleWidth(scale);

	    if (incw == 0)
	    {
		incw = SB_TRANSVERSAL_SIZE +
		    2 * Scale_HighlightThickness(scale);
	    }

	    inch = Scale_ScaleHeight(scale);

	    if (inch == 0)
	    {
		inch = XtHeight(scale);
	    }

#if 0
	    if (inch == 0 && num_children >= 3)
#else
	    if (Scale_ScaleHeight(scale) == 0 && num_children >= 3)
#endif
	    {
		if (num_children == 3)
		{
		    inch = boxes[2].box.height;
		}
		else
		{
		    /* FIX ME: SrollBar's or Scale's? */
		    inch = HSLIDERSIZE +
			2 * (Prim_HighlightThickness(boxes[CHILD_SB].kid) +
			     Prim_ShadowThickness(boxes[CHILD_SB].kid)) +
			maxh * (num_children - 3);
		}
	    }

	    if (inch == 0)
	    {
		inch = SB_LONGITUDAL_SIZE + 2 * Scale_HighlightThickness(scale);
	    }

	    boxes[CHILD_SB].box.width = incw;
	    boxes[CHILD_SB].box.height = inch;

	    if (inch > curh)
	    {
		curh = inch;
	    }

	    curw += incw;
	    curx += incw;
	}

	/* lay out the tick marks */
	if (num_children > 2)
	{
	    /* centres are nicely aligned */

	    if (num_children > 3)
	    {
		int spacing;

		if (curh == 0)
		{
		    spacing = maxh * (num_children - 3);
		}
		else
		{
		    spacing = (curh - (HSLIDERSIZE +
			     2 * (Prim_HighlightThickness(boxes[CHILD_SB].kid) +
				  Prim_ShadowThickness(boxes[CHILD_SB].kid))));
		}
		printf("curh %i %i %i %i %i %i %i\n",
			spacing,
			curh,
			HSLIDERSIZE,
			maxh,
			num_children,
			Prim_HighlightThickness(boxes[CHILD_SB].kid),
			Prim_ShadowThickness(boxes[CHILD_SB].kid));

		for (i = 2; i < num_children; i++)
		{
		    boxes[i].box.x = (maxw - boxes[i].box.width) / 2;
		    boxes[i].box.y = 1 + (HSLIDERSIZE - boxes[i].box.height) / 2 +
			    Prim_HighlightThickness(boxes[CHILD_SB].kid) +
			    Prim_ShadowThickness(boxes[CHILD_SB].kid) +
			    (i - 2) * spacing / (num_children - 3);
		}
#if 1
		/* scale/test13 ???? */
		curh++;
		boxes[CHILD_SB].box.height++;
#endif
	    }
	    else
	    {
		boxes[2].box.x = 0;
		boxes[2].box.y = (curh - boxes[2].box.height) / 2;
	    }
	}

	/*
	 * then the title, if there is one
	 */
	if (Scale_Title(scale) && XtIsManaged(boxes[CHILD_LAB].kid))
	{

	    boxes[CHILD_LAB].box.x = curx;
	    boxes[CHILD_LAB].box.y = cury;


	    incw = boxes[CHILD_LAB].box.width;
	    inch = boxes[CHILD_LAB].box.height;

	    if (inch > curh)
	    {
		curh = inch;
	    }

	    curw += incw;
	    cury += inch;
	    curw += 2 * MGR_ShadowThickness(scale);
/*
printf("COMPUTED %d %d : %d %d\n", curw, cury, incw, inch);
*/
	}
    }

    /*
     * if we're being queried for our geometry, return what we've got
     */
    *wd = curw;
    *ht = curh;

    return boxes;
}


/*
 * if we can, change our size to match
 */
static XtGeometryResult
_XmScaleGeomRequest(Widget scale, Dimension *wd, Dimension *ht)
{
    XtWidgetGeometry request;
    XtGeometryResult result;

    request.request_mode = (CWWidth | CWHeight);
    request.width = *wd;
    request.height = *ht;

    DEBUGOUT(_LtDebug(__FILE__, scale, "_XmScaleGeomRequest(ing) (%d %d)\n",
		      *wd, *ht));

    result = _XmMakeGeometryRequest(scale, &request);

    if (result == XtGeometryYes)
    {
	*wd = request.width;
	*ht = request.height;
    }
    else
    {
	*wd = XtWidth(scale);
	*ht = XtHeight(scale);
    }

    DEBUGOUT(_LtDebug(__FILE__, scale, "_XmScaleLayout (%d %d) => %s\n",
		      *wd, *ht, _LtDebugGeometryResult2String(result)));

    return result;
}


static void
_XmScaleLayout(Widget scale, XmKidGeometry boxes,
	       Widget child, XtWidgetGeometry *childgeom,
	       Dimension curw, Dimension curh)
{
    int i, delta;

	int num_children = MGR_NumChildren(scale);
    /*
     * do the layout
     */
    if (Scale_Orientation(scale) == XmHORIZONTAL)
    {
#if 0
	if (Scale_ScaleWidth(scale) == 0)
	    boxes[CHILD_SB].box.width = XtWidth(scale);

	if (boxes[CHILD_SB].box.width < (HSLIDERSIZE + MIN_SLIDE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale)))
	{
	    boxes[CHILD_SB].box.width = (HSLIDERSIZE + MIN_SLIDE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale));
	    boxes[CHILD_SB].box.x = (XtWidth(scale) -
		boxes[CHILD_SB].box.width) / 2;
	}

	if (num_children == 2)
	{
	    boxes[CHILD_LAB].box.width = boxes[CHILD_SB].box.width;
	    boxes[CHILD_LAB].box.x = boxes[CHILD_SB].box.x;
	}

	if (XtHeight(scale) < (boxes[CHILD_SB].box.y + VSLIDERSIZE +
			       2 * Prim_ShadowThickness(children[CHILD_SB]) +
			       2 * Scale_HighlightThickness(scale)))
	{
	    boxes[CHILD_SB].box.y -= (boxes[CHILD_SB].box.y + VSLIDERSIZE +
				  2 * Prim_ShadowThickness(children[CHILD_SB]) +
				  2 * Scale_HighlightThickness(scale)) -
		XtHeight(scale) + 1;
	}

	if (XtWidth(scale) > curw)
	{
	    for (i = 2; i < num_children; i++)
	    {
		boxes[i].box.width = (Dimension)((double)boxes[i].box.width /
						 (double)curw *
						 (double)XtWidth(scale));
	    }
	}
#endif /* 0 */

	if ((delta = XtHeight(scale) - curh))
	{
	    if (delta < 0 && XtIsManaged(boxes[CHILD_LAB].kid))
	    {
		delta += boxes[CHILD_LAB].box.height +
		    (MGR_ShadowThickness(scale) << 1);
		if (delta > 0)
		    delta = 0;
	    }
	    for (i = 0; i < 2 && boxes[i].kid; i++)
	    {
		boxes[i].box.y += delta;
	    }
	}

	/* T. Straumann: adjust width / tickmark positions if our geometry
	 * is different from the one used in _XmScalePreferredSize().
	 *
	 * They do this only if Scale_ScaleWidth() == 0 or delta < 0
	 * as can be confirmed by test14. Try also to resize 
	 *
	 *   test13 -xrm "*scaleHeight:200"
	 *
	 * using the WM
	 */
	delta = XtWidth(scale) - curw;
	if ( delta < 0 || (delta > 0 && 0 == Scale_ScaleWidth(scale) ) )
	{
		/* different layout whether one or more tick marks are present */
		int denom = (num_children == 3 ? 2 : num_children -3);
		for (i = 0; boxes[i].kid; i++)
		{
			switch (i)
			{
				/* adjust the width of SB and LAB */
				case CHILD_LAB:
				case CHILD_SB:
					boxes[i].box.width = XtWidth(scale);
				break;

				/* adjust the tick marks */

				default:
					boxes[i].box.x += delta * (i - 2) / denom;
				break;
			}
		}
	}

    }
    else if (Scale_Orientation(scale) == XmVERTICAL)
    {
#if 0
	if (Scale_ScaleHeight(scale) == 0)
	    boxes[CHILD_SB].box.height = XtHeight(scale);

	if (boxes[CHILD_SB].box.height < (HSLIDERSIZE + MIN_SLIDE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale)))
	{
	    boxes[CHILD_SB].box.height = (HSLIDERSIZE + MIN_SLIDE +
				 2 * Prim_ShadowThickness(children[CHILD_SB]) +
				 2 * Scale_HighlightThickness(scale));
	    boxes[CHILD_SB].box.y = (XtHeight(scale) -
		boxes[CHILD_SB].box.height) / 2;
	}

	if (num_children == 2)
	{
	    boxes[CHILD_LAB].box.height = boxes[CHILD_SB].box.height;
	    boxes[CHILD_LAB].box.y = boxes[CHILD_SB].box.y;
	}

	if (XtWidth(scale) < (boxes[CHILD_SB].box.width + VSLIDERSIZE +
			      2 * Prim_ShadowThickness(children[CHILD_SB]) +
			      2 * Scale_HighlightThickness(scale)))
	{
	    boxes[CHILD_SB].box.x -= (boxes[CHILD_SB].box.x + VSLIDERSIZE +
				  2 * Prim_ShadowThickness(children[CHILD_SB]) +
				  2 * Scale_HighlightThickness(scale)) -
		XtWidth(scale) + 1;
	}

	if (XtWidth(scale) > curw)
	{
	    for (i = 2; i < num_children; i++)
		boxes[i].box.height = (Dimension)((double)boxes[i].box.height /
						  (double)curw *
						  (double)XtWidth(scale));
	}
#endif /* 0 */

	if ((delta = XtWidth(scale) - curw))
	{
	    if (delta < 0 && XtIsManaged(boxes[CHILD_LAB].kid))
	    {
		delta += boxes[CHILD_LAB].box.width +
		    (MGR_ShadowThickness(scale) << 1);
		if (delta > 0)
		    delta = 0;
	    }
	    for (i = 0; boxes[i].kid; i++)
	    {
		boxes[i].box.x += delta;
	    }
	}

	/* T. Straumann: adjust height / tickmark positions if our geometry
	 * is different from the one used in _XmScalePreferredSize()
	 *
	 * They do this only if Scale_ScaleWidth() == 0 or delta < 0
	 * as can be confirmed by test14. Try also to resize 
	 *
	 *   test13 -xrm "*scaleHeight:200"
	 *
	 * using the WM
	 */
	delta = XtHeight(scale) - curh;
	if ( delta < 0 || (delta > 0 && 0 == Scale_ScaleHeight(scale) ) )
	{
		/* different layout whether one or more tick marks are present */
		int denom = (num_children == 3 ? 2 : num_children -3);
		for (i = 0; boxes[i].kid; i++)
		{
			switch (i)
			{
				/* adjust the height of SB and LAB */
				case CHILD_LAB:
				case CHILD_SB:
					boxes[i].box.height = XtHeight(scale);
				break;

				/* adjust the tick marks */
				default:
					boxes[i].box.y += delta * (i - 2) / denom;
				break;
			}
		}
	}
    }
}


/*
 * if we've gotten here, layout the children
 */
static void
_XmScaleConfigureChildren(Widget scale, Widget child, XmKidGeometry boxes)
{
    int inc;
    int p_inc;

    _XmSetKidGeo(boxes, child);

    Scale_SliderSize(scale) = _XmScaleConvertWidthToSliderSize(scale);
    _XmScaleConvertScaleIncrementToSCBIncrements(scale, &inc, &p_inc);

    XmScrollBarSetValues(boxes[CHILD_SB].kid,
			 _XmScaleConvertScaleValueToSCBValue(scale),
			 Scale_SliderSize(scale),
			 inc, p_inc, False);
}


static void
computeValueSize(Widget w)
{
    int maxlen, maxval;

#if 0
    maxval = ( Scale_Maximum(w) > -Scale_Minimum(w) ) ?
                Scale_Maximum(w) :
                Scale_Minimum(w);
 
    maxlen = ( maxval < 0 ) ? 2 : 1;
#else
    maxval = abs(Scale_Maximum(w)) > abs(Scale_Minimum(w)) ? abs(Scale_Maximum(w)) : abs(Scale_Minimum(w));
    if (Scale_Maximum(w) < 0 || Scale_Minimum(w) < 0)
    {
    	maxlen = 2;
    }
    else
    {
    	maxlen = 1;
    }
#endif
 
    for (maxval = abs(maxval); maxval >= 10; maxlen++)
    {
        maxval /= 10;
    }
 
    if (Scale_DecimalPoints(w))
    {
        maxlen++;
        maxlen = maxlen < 3 ? 3 : maxlen;
    }
 
    Scale_ShowValueWidth(w) = 2 + Scale_FontStruct(w)->max_bounds.width * maxlen;
    Scale_ShowValueHeight(w) = 3 + Scale_FontStruct(w)->max_bounds.ascent +
        Scale_FontStruct(w)->max_bounds.descent;
}


static void
showValue(Widget w, int scb_value, int scale_value)
{
    char buf[256];
    Widget sb;

    sb = ((XmScaleWidget)w)->composite.children[CHILD_SB];
    valueToString(w, buf);

#if 1
/* Fix from John Richardson <jrichard@zko.dec.com> 1/7/1996 */
	/* T. Straumann: only erase if Scale_ShowValueX() is valid */
    if (XtIsRealized(w) && SHOW_VALUE_X_INVALID != Scale_ShowValueX(w))
    {
	XFillRectangle(XtDisplay(w), XtWindow(w), MGR_BackgroundGC(w),
		       Scale_ShowValueX(w), Scale_ShowValueY(w),
		       Scale_ShowValueWidth(w), Scale_ShowValueHeight(w));
    }
#else
    if (XtIsRealized(w))
    {
	XFillRectangle(XtDisplay(w), XtWindow(w), MGR_BackgroundGC(w),
		Scale_ShowValueX(w), Scale_ShowValueY(w) - Scale_FontStruct(w)->descent,
		Scale_ShowValueWidth(w), Scale_ShowValueHeight(w));
    }
#endif

    if (Scale_Orientation(w) == XmHORIZONTAL)
    {
	Scale_ShowValueX(w) = SCB_SliderX(sb) + (SCB_SliderWidth(sb) >> 1) -
	    (Scale_ShowValueWidth(w) / 2) + XtX(sb);

	Scale_ShowValueY(w) = XtY(sb) - Scale_ShowValueHeight(w);
    }
    else if (Scale_Orientation(w) == XmVERTICAL)
    {
	Scale_ShowValueY(w) = SCB_SliderY(sb) + (SCB_SliderHeight(sb) >> 1) -
	    (Scale_ShowValueHeight(w) / 2) + XtY(sb);

	Scale_ShowValueX(w) = XtX(sb) - Scale_ShowValueWidth(w);
    }
    else
    {
	_XmError(w, "Scale Orientation wrong.");
    }

    if (XtIsRealized(w))
    {
	int offset;

	offset = (Scale_ShowValueWidth(w) -
		  XTextWidth(Scale_FontStruct(w), buf, strlen(buf))) ;
	if (Scale_Orientation(w) == XmHORIZONTAL)
	{
	    XDrawString(XtDisplay(w), XtWindow(w),
			Scale_ForegroundGC(w),
			Scale_ShowValueX(w) + offset / 2,
			Scale_ShowValueY(w) + Scale_FontStruct(w)->ascent,
			buf, strlen(buf));
	}
	else if (Scale_Orientation(w) == XmVERTICAL)
	{
	    XDrawString(XtDisplay(w), XtWindow(w),
			Scale_ForegroundGC(w),
			Scale_ShowValueX(w) + offset,
			Scale_ShowValueY(w) + Scale_FontStruct(w)->ascent,
			buf, strlen(buf));
	}
	else
	    _XmError(w, "Scale Orientation wrong");
    }
}


static void
_ScaleValueChanged(Widget sb, XtPointer cd, XtPointer data)
{
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)data;
    XmScaleCallbackStruct scbs;
    int scale_value;

    Scale_LastValue(XtParent(sb)) = Scale_Value(sb);
    scale_value = _XmScaleConvertSCBValueToScaleValue(XtParent(sb), cbs->value);
    Scale_Value(XtParent(sb)) = scale_value;

    if (Scale_ShowValue(XtParent(sb)))
    {
	showValue(XtParent(sb), cbs->value, scale_value);
    }

    scbs.reason = cbs->reason;
    scbs.event = cbs->event;
    scbs.value = scale_value;

    if (Scale_ValueChangedCallback(XtParent(sb)))
    {
	XtCallCallbackList(XtParent(sb),
			   Scale_ValueChangedCallback(XtParent(sb)), &scbs);
    }
}


static void
_ScaleDrag(Widget sb, XtPointer cd, XtPointer data)
{
    XmScrollBarCallbackStruct *cbs = (XmScrollBarCallbackStruct *)data;
    XmScaleCallbackStruct scbs;
    int scale_value;

#if XmVERSION > 1
    if (! Scale_Editable(XtParent(sb)))
	return;
#endif

    Scale_LastValue(XtParent(sb)) = Scale_Value(sb);
    scale_value = _XmScaleConvertSCBValueToScaleValue(XtParent(sb), cbs->value);
    Scale_Value(XtParent(sb)) = scale_value;

    if (Scale_ShowValue(XtParent(sb)))
    {
	showValue(XtParent(sb), cbs->value, scale_value);
    }

    scbs.reason = cbs->reason;
    scbs.event = cbs->event;
    scbs.value = scale_value;
    if (Scale_DragCallback(XtParent(sb)))
    {
	XtCallCallbackList(XtParent(sb),
			   Scale_DragCallback(XtParent(sb)), &scbs);
    }
}


#if 0
static void
GetFocus(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmScrollBarWidget sb;

    DEBUGOUT(_LtDebug(__FILE__, w, "GetFocus\n"));

    sb = (XmScrollBarWidget)(((XmScaleWidget)w)->composite.children[CHILD_SB]);

    XtCallActionProc((Widget)sb, "PrimitiveFocusIn", event,
		     params, *num_params);
}


static void
LoseFocus(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmScrollBarWidget sb;

    DEBUGOUT(_LtDebug(__FILE__, w, "LoseFocus\n"));

    sb = (XmScrollBarWidget)(((XmScaleWidget)w)->composite.children[CHILD_SB]);

    XtCallActionProc((Widget)sb, "PrimitiveFocusOut", event,
		     params, *num_params);
}
#endif /* 0 */


static int
_XmScaleConvertWidthToSliderSize(Widget w)
{
    int ret, last_ret;
    XmScrollBarWidget sb;
    Dimension ht;
    Dimension st;

    /* FIX ME: the next two statements are a kludge */
    sb = (XmScrollBarWidget)(((XmScaleWidget)w)->composite.children[CHILD_SB]);

    /* FIX ME: Can't we use scale's values here? I.o.w. what shouls scale do
     * when these values are set explicitly and thus differ from scale's ?
     */
    XtVaGetValues((Widget)sb,
		  XmNhighlightThickness, &ht,
		  XmNshadowThickness, &st,
		  NULL);

    ret = Scale_SliderSize(w);
    do
    {
	/* FIX ME: rework the if's */
	last_ret = ret;
	if (Scale_Orientation(w) == XmHORIZONTAL)
	{
	    if (XtWidth(sb) == 2 * (ht + st))
	    {
		/* Note: M*tif 1.2.2 SIGFPE's on this -- PvH */
		ret = (int)((double)(SCB_MAX - SCB_MIN) /
			    (double)(Scale_Maximum(w) - Scale_Minimum(w)) *
			    (double)HSLIDERSIZE);
	    }
	    else if (XtWidth(sb) < HSLIDERSIZE)
	    {
		ret = SCB_MAX - SCB_MIN;
	    }
	    else
	    {
		/* Note: M*tif doesn't use float arithmetic here -- PvH */
		ret = (int)((SCB_MAX - SCB_MIN) /
			    (XtWidth(sb) - 2 * (ht + st)) *
			    HSLIDERSIZE);
	    }
	}
	else
	{
	    if (XtHeight(sb) == 2 * (ht + st))
	    {
		ret = (int)((float)(SCB_MAX - SCB_MIN) /
			    (float)(Scale_Maximum(w) - Scale_Minimum(w)) *
			    (float)HSLIDERSIZE);
	    }
	    else if (XtHeight(sb) < HSLIDERSIZE)
	    {
		ret = SCB_MAX - SCB_MIN;
	    }
	    else
	    {
		/* Note: M*tif doesn't use float arithmetic here -- PvH */
		ret = (int)((SCB_MAX - SCB_MIN) /
			    (XtHeight(sb) - 2 * (ht + st)) *
			    HSLIDERSIZE);
	    }
	}
    }
    while (last_ret != ret);

    return ret;
}


static int
_XmScaleConvertSCBValueToScaleValue(Widget w, int value)
{
    double roundarg;
    int result;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmScaleConvertSCBValueToScaleValue(,%i)\n",
                       value));
    if ((SCB_MAX - SCB_MIN - Scale_SliderSize(w)) == 0)
    {
	return Scale_Minimum(w);
    }

    roundarg = (double)(Scale_Maximum(w) - Scale_Minimum(w)) 
	     * (double)value 
	     / (double)(SCB_MAX - SCB_MIN - Scale_SliderSize(w))
	     + (double)Scale_Minimum(w);

    result = (int)(roundarg + 0.5);

    return result;
}


static int
_XmScaleConvertScaleValueToSCBValue(Widget w)
{

    double roundarg;
    int result;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmScaleConvertScaleValueToSCBValue()\n"));
    roundarg = (double)(Scale_Value(w) - Scale_Minimum(w)) 
	     * (double)(SCB_MAX - SCB_MIN - Scale_SliderSize(w))
	     / (double)(Scale_Maximum(w) - Scale_Minimum(w));
    /* amai: is this the correct formula? 
             looks like the old one in _XmScaleConvertSCBValueToScaleValue() */
    result = (int)(roundarg);
    return result;
}


static void
_XmScaleConvertScaleIncrementToSCBIncrements(Widget w,
					 int* inc,
					 int* page_inc)
{
    *inc = (int)((double)(SCB_MAX - SCB_MIN - Scale_SliderSize(w)) /
		 (double)(Scale_Maximum(w) - Scale_Minimum(w)) +
		 0.5);
    *inc = _XmMax(*inc, 1);	/* FIX ME */
    *page_inc = *inc * Scale_ScaleMultiple(w);
}


static void
_XmScaleProcessingDirectionDefault(Widget w, int offset, XrmValue *val)
{
    static unsigned char direction;
    XmScaleWidget sw = (XmScaleWidget)w;

    if (sw->scale.orientation == XmVERTICAL)
    {
	direction = XmMAX_ON_TOP;
    }
    else
    {
	direction = XmMAX_ON_RIGHT;	/* FIX ME */
    }

    val->addr = (XPointer)&direction;
}
