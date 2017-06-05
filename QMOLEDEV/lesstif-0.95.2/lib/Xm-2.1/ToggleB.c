/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/ToggleB.c,v 1.2 2004/10/20 19:32:11 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2004 LessTif Development Team
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

static const char rcsid[] = "$Id: ToggleB.c,v 1.2 2004/10/20 19:32:11 dannybackx Exp $";

#include <LTconfig.h>

#include <stdarg.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/ToggleBP.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/RepType.h>
#include <Xm/TransltnsP.h>
#include <Xm/ToggleBGP.h>
#include <X11/ShellP.h>

#include <XmI/DebugUtil.h>


/* Some M*tif versions (e.g. 1.2.6 on Solaris 2.6) leave less room above and
 * below the indicator than others (e.g. 1.2.5 on HP-UX 10, my reference).
 */
#undef LESS_VERTICAL_PADDING

#define UNSPECIFIED_TB_BOOLEAN	((Boolean)85)
#define SQUARE_INDICATOR_DEC	3
#define SQUARE_INDICATOR_ELBOW	10
#define PIXMAP_INDICATOR_ELBOW	13

#define DETAIL_SHADOW_THICKNESS(w)	TB_DetailShadowThickness(w)
#define INDICATOR_BOX_MASK		0x03
#define NEXT_TOGGLE(w)			(TB_ToggleMode(w) == XmTOGGLE_BOOLEAN \
					 || TB_IndType(w) != XmN_OF_MANY      \
					 ? !TB_Set(w) : (TB_Set(w) + 1) % 3)

/* Forward Declarations */
static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_prehook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void initialize_posthook(Widget request, Widget new_w, ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static void resize(Widget w);
static void expose(Widget w, XEvent *event, Region region);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void MenuProcEntry(int proc, Widget rc,...);

static void draw_toggle(Widget w, XEvent *event, Region region,
			int is_expose, int visual_set);

static int implicit_indicator(Widget w);
static void _XmUnselectColorDefault(Widget, int, XrmValue *);

/*
 * Resources for the toggleButton class
 */
#define Offset(field) XtOffsetOf(XmToggleButtonRec, toggle.field)
static XtResource resources[] =
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
	XmRImmediate, (XtPointer)UNSPECIFIED_TB_BOOLEAN
    },
    {
	XmNspacing, XmCSpacing, XmRHorizontalDimension,
	sizeof(Dimension), Offset(spacing),
	XtRImmediate, (XtPointer)4
    },
    {
	XmNselectPixmap, XmCSelectPixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(on_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNselectInsensitivePixmap, XmCSelectInsensitivePixmap,
	XmRPrimForegroundPixmap, sizeof(Pixmap), Offset(insen_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNset, XmCSet, XmRBoolean,
	sizeof(Boolean), Offset(set),
	XmRImmediate, (XtPointer)False
    },
    {
	/* The type changed to an unsigned char in 2.0..oh just lovely. */
	XmNindicatorOn, XmCIndicatorOn, XmRIndicatorOn,
	sizeof(unsigned char), Offset(ind_on),
	XtRImmediate, (XtPointer)True
    },
    {
	XmNfillOnSelect, XmCFillOnSelect, XmRBoolean,
	sizeof(Boolean), Offset(fill_on_select),
	XtRImmediate, (XtPointer)UNSPECIFIED_TB_BOOLEAN
    },
    {
	XmNselectColor, XmCSelectColor, XmRPixel,
	sizeof(Pixel), Offset(select_color),
	XmRCallProc, (XtPointer)_XmSelectColorDefault
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
	sizeof(Boolean), XtOffsetOf(XmToggleButtonRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension),
	XtOffsetOf(XmToggleButtonRec, primitive.highlight_thickness),
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
	XmRPrimForegroundPixmap, sizeof(Pixmap),
	Offset(indeterminate_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNtoggleMode, XmCToggleMode,
	XmRToggleMode, sizeof(unsigned char),
	Offset(toggle_mode),
	XmRImmediate, (XtPointer)XmTOGGLE_BOOLEAN
    },
    {
	XmNunselectColor, XmCUnselectColor, XmRPixel,
	sizeof(Pixel), Offset(unselect_color),
	XmRCallProc, (XtPointer)_XmUnselectColorDefault
    }
};

static XmSyntheticResource syn_resources[] =
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
    },
    {
	XmNdetailShadowThickness,
	sizeof(Dimension), Offset(detail_shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

static void Arm(Widget w, XEvent *event,
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

static void Select(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);

static void ButtonUp(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void ButtonDown(Widget w, XEvent *event,
		       String *params, Cardinal *num_params);

static void KeySelect(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);


static XtTranslations default_trans = NULL;
static XtTranslations menu_trans = NULL;

static XtActionsRec actions[] =
{
    {"Arm", Arm},
    {"ArmAndActivate", ArmAndActivate},
    {"Disarm", Disarm},
    {"Select", Select},
    {"Enter", EnterWindow},
    {"Leave", LeaveWindow},
    {"BtnDown", ButtonDown},
    {"BtnUp", ButtonUp},
    {"KeySelect", KeySelect},
    {"Help", Help},
};

/* *INDENT-OFF* */
static XmBaseClassExtRec _XmToggleBCoreClassExtRec = {
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
    /* get_values_posthook       */ XmInheritGetValuesPosthook,
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

XmPrimitiveClassExtRec _XmToggleBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmInheritBaselineProc,
    /* widget_display_rect */ XmInheritDisplayRectProc,
    /* widget_margins      */ NULL
};

XmToggleButtonClassRec xmToggleButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelClassRec,
	/* class_name            */ "XmToggleButton",
	/* widget_size           */ sizeof(XmToggleButtonRec),
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
	/* extension             */ (XtPointer)&_XmToggleBCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight, /* FIX ME */
	/* border_unhighlight    */ XmInheritBorderUnhighlight, /* FIX ME */
	/* translations          */ XtInheritTranslations,
	/* arm_and_activate_proc */ ArmAndActivate,
	/* synthetic resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmToggleBPrimClassExtRec
    },
    /* Label Class part */
    {
	/* setOverrideCallback */ XmInheritSetOverrideCallback,
	/* menuProcs           */ XmInheritMenuProc,
	/* translations        */ XtInheritTranslations,
	/* extension           */ NULL
    },
    /* ToggleButton Class part */
    {
	/* extension */ NULL
    }
};
/* *INDENT-ON* */

WidgetClass xmToggleButtonWidgetClass = (WidgetClass)&xmToggleButtonClassRec;


/*
 * Some #defines to make the code below more readable
 */

#define IN_MENU(w) (Lab_MenuType(w) == XmMENU_POPUP || \
		    Lab_MenuType(w) == XmMENU_PULLDOWN)

static void
class_initialize(void)
{
    menu_trans = XtParseTranslationTable(_XmToggleB_menuTranslations);
    default_trans = XtParseTranslationTable(_XmToggleB_defaultTranslations);

    _XmToggleBCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmTOGGLE_BUTTON_BIT);
}

static void
CreateSelectGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;

    if (TB_VisualSet(w) == XmINDETERMINATE) {
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
    values.foreground = TB_SelectColor(w);
    values.background = XtBackground(w);

    TB_SelectGC(w) = XtGetGC(w, mask, &values);

    values.foreground = TB_UnselectColor(w);
    TB_UnselectGC(w) = XtGetGC(w, mask, &values);
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
    values.foreground = XtBackground(w);
    values.background = Prim_Foreground(w);

	values.background = TB_UnselectColor(w);

    TB_BackgroundGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_prehook(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    _XmSaveCoreClassTranslations(new_w);

    if (XmIsRowColumn(XtParent(new_w)) &&
	(RC_Type(XtParent(new_w)) == XmMENU_PULLDOWN ||
	 RC_Type(XtParent(new_w)) == XmMENU_POPUP))
    {
	CoreClassTranslations(new_w) = (String)menu_trans;
    }
    else
    {
	CoreClassTranslations(new_w) = (String)default_trans;
    }
}

static void
initialize_posthook(Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    _XmRestoreCoreClassTranslations(new_w);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
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

    TB_Armed(new_w) = False;

    TB_VisualSet(new_w) = TB_Set(new_w);

    /* Fix up the pixmaps */
    if (Lab_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	TB_OnPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
	Lab_Pixmap(new_w) = TB_OnPixmap(new_w);

    if (Lab_PixmapInsensitive(new_w) == XmUNSPECIFIED_PIXMAP &&
	TB_InsenPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
	Lab_PixmapInsensitive(new_w) = TB_InsenPixmap(new_w);

    if (Lab_IsPixmap(new_w) &&
	(TB_IndeterminatePixmap(new_w) != XmUNSPECIFIED_PIXMAP ||
	 ((XtSensitive(new_w)
	   ? TB_OnPixmap(new_w)
	   : TB_InsenPixmap(new_w))
	  != XmUNSPECIFIED_PIXMAP)))
    {
	Dimension width, height;
	Dimension iwidth, iheight;

	_XmLabelGetPixmapSize(new_w,
			      (XtSensitive(new_w)
			       ? TB_OnPixmap(new_w)
			       : TB_InsenPixmap(new_w)),
			      &width, &height);
	_XmLabelGetPixmapSize(new_w, TB_IndeterminatePixmap(new_w),
			      &iwidth, &iheight);
	if (width < iwidth)
	    width = iwidth;
	if (height < iheight)
	    height = iheight;
	if (Lab_TextRect_width(new_w) < width ||
	    Lab_TextRect_height(new_w) < height)
	{
	    if (Lab_TextRect_width(new_w) < width)
		Lab_TextRect_width(new_w) = width;
	    if (Lab_TextRect_height(new_w) < height)
		Lab_TextRect_height(new_w) = height;
	    if (!XtWidth(request) || !XtHeight(request))
	    {
		if (!XtWidth(request))
		    XtWidth(new_w) = 0;
		if (!XtHeight(request))
		    XtHeight(new_w) = 0;
		xmToggleButtonClassRec.core_class.resize(new_w);
	    }
	}
    }

    if (TB_IndicatorDim(new_w) == XmINVALID_DIMENSION)
    {
	TB_IndicatorSet(new_w) = Lab_IsPixmap(new_w);
	TB_IndicatorDim(new_w) = implicit_indicator(new_w);
    }
    else
    {
	TB_IndicatorSet(new_w) = True;
    }

    CreateSelectGC(new_w);
    CreateBackgroundGC(new_w);

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRToggleMode),
			     TB_ToggleMode(new_w), new_w))
	TB_ToggleMode(new_w) = XmTOGGLE_BOOLEAN;

    if (TB_IndType(new_w) == (unsigned char)XmUNSPECIFIED ||
	!XmRepTypeValidValue(XmRepTypeGetId(XmRIndicatorType),
			     TB_IndType(new_w), new_w))
    {
	TB_IndType(new_w) =
	    XmIsRowColumn(XtParent(new_w)) && RC_RadioBehavior(XtParent(new_w))
	    ? XmONE_OF_MANY
	    : XmN_OF_MANY;
    }

    if (TB_Visible(new_w) == UNSPECIFIED_TB_BOOLEAN)
	TB_Visible(new_w) = !IN_MENU(new_w);

    if (TB_FillOnSelect(new_w) == UNSPECIFIED_TB_BOOLEAN)
	TB_FillOnSelect(new_w) = (TB_IndType(new_w) == XmN_OF_MANY
				  ? TB_IndOn(new_w) & INDICATOR_BOX_MASK
				  : TB_IndOn(new_w)) != 0;

    if (TB_IndOn(new_w))
    {
	int margin;

	/* Make sure there's enough room on the side for the indicator */
	margin = (TB_IndicatorDim(new_w) + TB_Spacing(new_w))
	    - (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	       ? Lab_MarginLeft(new_w) : Lab_MarginRight(new_w));
	if (margin > 0)
	{
	    if (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R)
	    {
		Lab_MarginLeft(new_w) += margin;
		Lab_TextRect_x(new_w) += margin;
		if (Lab_AcceleratorText(new_w) != NULL)
		    Lab_AccTextRect(new_w).x += margin;
	    }
	    else
		Lab_MarginRight(new_w) += margin;
	    if (!XtWidth(request))
		XtWidth(new_w) += margin;
	}

	/* Make sure there's enough room vertically.
	 * Non-menu toggles want some padding space.
	 */
	margin = TB_IndicatorDim(new_w) - (Lab_TextRect_height(new_w)
					   + Lab_MarginTop(new_w)
					   + Lab_MarginBottom(new_w));
#ifdef LESS_VERTICAL_PADDING
	margin -= Lab_MarginHeight(new_w) << 1;
#else
	if (!IN_MENU(new_w))
	    margin += (Lab_Shadow(new_w) + Xm3D_ENHANCE_PIXEL) << 1;
#endif
	if (margin > 0)
	{
	    Lab_MarginTop(new_w) += margin >> 1;
	    Lab_MarginBottom(new_w) += margin >> 1;
	    if (!XtHeight(request))
	    {
		Lab_TextRect_y(new_w) += margin >> 1;
		if (Lab_AcceleratorText(new_w) != NULL)
		    Lab_AccTextRect(new_w).y += margin >> 1;
		XtHeight(new_w) += margin;
	    }
	}
    }

    if (IN_MENU(new_w))
    {
	Lab_Highlight(new_w) = 0;
	if (Lab_Shadow(new_w) == 0)
	    Lab_Shadow(new_w) = 2;
	LabClass_MenuProcs(XtClass(new_w)) = MenuProcEntry;
    }
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, TB_SelectGC(w));
    XtReleaseGC(w, TB_BackgroundGC(w));
    XtReleaseGC(w, TB_UnselectGC(w));
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "ToggleB set_values()\n"));

    if (TB_IndType(old) != TB_IndType(new_w) &&
	!XmRepTypeValidValue(XmRepTypeGetId(XmRIndicatorType),
			     TB_IndType(new_w), new_w))
	TB_IndType(new_w) = TB_IndType(old);
    if (TB_ToggleMode(old) != TB_ToggleMode(new_w) &&
	!XmRepTypeValidValue(XmRepTypeGetId(XmRToggleMode),
			     TB_ToggleMode(new_w), new_w))
	TB_ToggleMode(new_w) = TB_ToggleMode(old);

    if (TB_SelectColor(new_w) != TB_SelectColor(old))
    {
	XtReleaseGC(new_w, TB_SelectGC(new_w));
	CreateSelectGC(new_w);
	refresh_needed = True;
    }
    if (XtBackground(new_w) != XtBackground(old))
    {
	XtReleaseGC(new_w, TB_BackgroundGC(new_w));
	CreateBackgroundGC(new_w);
	refresh_needed = True;
    }

    /* Changes to the on or off pixmap */
    if (Lab_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	TB_OnPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	Lab_Pixmap(new_w) = TB_OnPixmap(new_w);

	if (Lab_IsPixmap(new_w) && XtSensitive(new_w))
	    refresh_needed = True;
    }

    if (Lab_PixmapInsensitive(new_w) == XmUNSPECIFIED_PIXMAP &&
	TB_InsenPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	Lab_PixmapInsensitive(new_w) = TB_InsenPixmap(new_w);

	if (Lab_IsPixmap(new_w) && !XtSensitive(new_w))
	    refresh_needed = True;
    }

    if (Lab_IsPixmap(new_w) && (Lab_RecomputeSize(new_w) ||
	TB_IndeterminatePixmap(new_w) != XmUNSPECIFIED_PIXMAP ||
	(XtSensitive(new_w)
	 ? Lab_Pixmap(request) != Lab_Pixmap(old) ||
	   TB_OnPixmap(new_w) != TB_OnPixmap(old)
	 : Lab_PixmapInsensitive(request) != Lab_PixmapInsensitive(old) ||
	   TB_InsenPixmap(new_w) != TB_InsenPixmap(old))))
    {
	Dimension width, height;
	Dimension iwidth, iheight;

	_XmLabelGetPixmapSize(new_w,
			      (XtSensitive(new_w)
			       ? TB_OnPixmap(new_w)
			       : TB_InsenPixmap(new_w)),
			      &width, &height);
	_XmLabelGetPixmapSize(new_w, TB_IndeterminatePixmap(new_w),
			      &iwidth, &iheight);
	if (width < iwidth)
	    width = iwidth;
	if (height < iheight)
	    height = iheight;

	if (Lab_TextRect_width(new_w) < width ||
	    Lab_TextRect_height(new_w) < height)
	{
	    if (Lab_TextRect_width(new_w) < width)
		Lab_TextRect_width(new_w) = width;
	    if (Lab_TextRect_height(new_w) < height)
		Lab_TextRect_height(new_w) = height;

	    if (Lab_RecomputeSize(new_w))
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
		xmToggleButtonClassRec.core_class.resize(new_w);
	    }

	    width = XtWidth(new_w);
	    height = XtHeight(new_w);
	    XtWidth(new_w) = XtWidth(old);
	    XtHeight(new_w) = XtHeight(old);
	    xmToggleButtonClassRec.core_class.resize(new_w);
	    XtWidth(new_w) = width;
	    XtHeight(new_w) = height;
	}
    }

    if (TB_IndicatorDim(new_w) == XmINVALID_DIMENSION)
	TB_IndicatorSet(new_w) = False;

    if ((!TB_IndicatorSet(new_w) &&
	 (TB_IndicatorDim(new_w) == XmINVALID_DIMENSION ||
	  TB_IndOn(new_w) != TB_IndOn(old) ||
	  Lab_Label(new_w) != Lab_Label(old) ||
	  Lab_Font(new_w) != Lab_Font(old))) ||
	(TB_IndicatorDim(new_w) == TB_IndicatorDim(old) && Lab_IsPixmap(new_w)
	 && Lab_TextRect_height(new_w) != Lab_TextRect_height(old)))
    {
	TB_IndicatorDim(new_w) = implicit_indicator(new_w);
    }

    if (Lab_IsPixmap(new_w))
	TB_IndicatorSet(new_w) = True;

    /* Adjust margins for the indicator size if necessary.
     * Margins be increased or decreased; if the margin was explicitly set
     * in this call, don't decrease it past that (though it can get bigger).
     */
    if (TB_IndOn(new_w) &&
	(TB_IndicatorDim(new_w) != TB_IndicatorDim(old)
	 || TB_Spacing(new_w) != TB_Spacing(old)
	 || (!IN_MENU(new_w) && Lab_Shadow(new_w) != Lab_Shadow(old))
	 || Lab_StringDirection(new_w) != Lab_StringDirection(old)
	 || (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	     ? Lab_MarginLeft(new_w) != Lab_MarginLeft(old)
	     : Lab_MarginRight(new_w) != Lab_MarginRight(old))
	 || Lab_MarginTop(new_w) != Lab_MarginTop(old)
	 || Lab_MarginBottom(new_w) != Lab_MarginBottom(old)))
    {
	int margin, tm;

	margin = TB_IndicatorDim(new_w) + TB_Spacing(new_w)
	    - (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
	       ? Lab_MarginLeft(new_w) : Lab_MarginRight(new_w));
	if (margin && (margin > 0 ||
		       (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R
			? Lab_MarginLeft(new_w) == Lab_MarginLeft(old)
			: Lab_MarginRight(new_w) == Lab_MarginRight(old))))
	{
	    if (Lab_StringDirection(new_w) == XmSTRING_DIRECTION_L_TO_R)
	    {
		Lab_MarginLeft(new_w) += margin;
		Lab_TextRect_x(new_w) += margin;
		Lab_AccTextRect(new_w).x += margin;
	    }
	    else
		Lab_MarginRight(new_w) += margin;
	    if (Lab_RecomputeSize(new_w) || !XtWidth(request))
		XtWidth(new_w) += margin;
	}

	margin = TB_IndicatorDim(new_w) - (Lab_TextRect_height(new_w)
					   + Lab_MarginTop(new_w)
					   + Lab_MarginBottom(new_w));
#ifdef LESS_VERTICAL_PADDING
	margin -= Lab_MarginHeight(new_w) << 1;
#else
	if (!IN_MENU(new_w))
	    margin += (Lab_Shadow(new_w) + Xm3D_ENHANCE_PIXEL) << 1;
#endif
	if (margin)
	{
	    margin /= 2;
	    tm = margin;
	    if (tm < (Lab_MarginTop(new_w) == Lab_MarginTop(old)
		      ? XmDEFAULT_TOP_MARGIN - (int)Lab_MarginTop(new_w)
		      : 0))
		tm = (Lab_MarginTop(new_w) == Lab_MarginTop(old)
		      ? XmDEFAULT_TOP_MARGIN - (int)Lab_MarginTop(new_w)
		      : 0);
	    Lab_MarginTop(new_w) += tm;
	    if (Lab_RecomputeSize(new_w) || !XtHeight(request))
		XtHeight(new_w) += tm;

	    if (margin < (Lab_MarginBottom(new_w) == Lab_MarginBottom(old)
		       ? XmDEFAULT_BOTTOM_MARGIN - (int)Lab_MarginBottom(new_w)
		       : 0))
		margin = (Lab_MarginBottom(new_w) == Lab_MarginBottom(old)
		       ? XmDEFAULT_BOTTOM_MARGIN - (int)Lab_MarginBottom(new_w)
		       : 0);
	    Lab_MarginBottom(new_w) += margin;
	    if (Lab_RecomputeSize(new_w) || !XtHeight(request))
		XtHeight(new_w) += margin;

	    if (tm != margin)
	    {
		Lab_TextRect_y(new_w) += (tm - margin) / 2;
		Lab_AccTextRect(new_w).y += (tm - margin) / 2;
	    }
	}

	refresh_needed = True;
    }

    if (TB_IndType(new_w) != TB_IndType(old)
	|| TB_DetailShadowThickness(new_w) != TB_DetailShadowThickness(old)
	|| ((TB_IndOn(new_w) || TB_IndOn(old))
	    && (TB_Visible(new_w) != TB_Visible(old)
		|| TB_FillOnSelect(new_w) != TB_FillOnSelect(old))))
    {
	refresh_needed = True;
    }

    if (TB_Set(old) != TB_Set(new_w))
    {
	if (!refresh_needed)
	    draw_toggle(new_w, NULL, NULL, False, TB_Set(new_w));
	else if ((TB_Set(old) == XmINDETERMINATE ||
		  TB_Set(new_w) == XmINDETERMINATE)
		 && TB_SelectColor(new_w) == TB_SelectColor(old))
	{
	    XtReleaseGC(new_w, TB_SelectGC(new_w));
	    CreateSelectGC(new_w);
	}
	TB_VisualSet(new_w) = TB_Set(new_w);
    }

    return refresh_needed;
}

static void
resize(Widget w)
{
    Position x;

    xmLabelClassRec.core_class.resize(w);

    /* Make sure the label and toggle don't overlap */

    if (TB_IndOn(w))
    {
	if (Lab_StringDirection(w) == XmSTRING_DIRECTION_L_TO_R)
	{
	    x = Lab_Highlight(w) + Lab_Shadow(w) + Lab_MarginWidth(w)
		+ TB_IndicatorDim(w) + TB_Spacing(w);
	    if (Lab_TextRect_x(w) < x)
	    {
		Lab_AccTextRect(w).x += x - Lab_TextRect_x(w);
		Lab_TextRect_x(w) = x;
	    }
	}
	else
	{
	    x = XtWidth(w) - Lab_Highlight(w) - Lab_Shadow(w) - Lab_MarginWidth(w)
		- TB_IndicatorDim(w) - TB_Spacing(w) - Lab_TextRect_width(w);
	    if (Lab_TextRect_x(w) > x)
	    {
		Lab_AccTextRect(w).x -= Lab_TextRect_x(w) - x;
		Lab_TextRect_x(w) = x;
	    }
	}
    }
}

static void
expose(Widget w, XEvent *event, Region region)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "ToggleB Expose\n"));
    draw_toggle(w, event, region, True, 0);
}

static void
Arm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);

    if (!TB_Armed(w))
    {
	TB_Armed(w) = True;

	if (TB_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.set = TB_Set(w);

	    XFlush(XtDisplay(w));
	    XtCallCallbackList(w, TB_ArmCallback(w), (XtPointer)&cbs);
	}
    }
    draw_toggle(w, NULL, NULL, False, NEXT_TOGGLE(w));
}

static void
Select(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;
    XButtonEvent *ev = (XButtonEvent *)event;

    if (TB_Armed(w) &&
	(ev->type == KeyPress || ev->type == KeyRelease ||
	 ((ev->x >= 0 && ev->x < XtWidth(w)) &&
	  (ev->y >= 0 && ev->y < XtHeight(w)))))
    {
	TB_Armed(w) = False;

	TB_Set(w) = TB_VisualSet(w);

	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, NULL);

	cbs.reason = XmCR_VALUE_CHANGED;
	cbs.event = event;
	cbs.set = TB_Set(w);
	if (XmIsRowColumn(XtParent(w)))
	{
	    RC_MenuMenuCallback(w, &cbs);
	}
	/* Menu callback might cancel the set (radioAlwaysOne). */
	cbs.set = TB_Set(w);
	if (!Lab_SkipCallback(w) && TB_ValueChangedCallback(w))
	{
	    XFlush(XtDisplay(w));
	    XtCallCallbackList(w, TB_ValueChangedCallback(w), (XtPointer)&cbs);
	}
    }
}

static void
Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;

    if (TB_Armed(w))
	TB_Armed(w) = False;

    if (TB_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.set = TB_Set(w);

	XFlush(XtDisplay(w));
	XtCallCallbackList(w, TB_DisarmCallback(w), (XtPointer)&cbs);
    }
    draw_toggle(w, NULL, NULL, False, TB_Set(w));
}

static void
ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean poppedUp;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:ArmAndActivate(%d)\n",
    	__FILE__, __LINE__));
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

    DEBUGOUT(_LtDebug(__FILE__, w, "ToggleB ButtonUp()\n"));

    /* queue events until the next button event */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

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
    if (!TB_Armed(w))
    {
	return;
    }

    RC_MenuButtonPopdown(w, event, &poppedUp);

    _XmRecordEvent(event);

    TB_Armed(w) = False;

    if (XtIsRealized(w))
	_XmClearBorder(XtDisplay(w), XtWindow(w),
		       0, 0, XtWidth(w), XtHeight(w), Lab_Shadow(w));

    TB_Set(w) = NEXT_TOGGLE(w);
    draw_toggle(w, NULL, NULL, False, TB_Set(w));

    DEBUGOUT(_LtDebug(__FILE__, w, "ButtonUp: HERE\n"));

    cbs.reason = XmCR_VALUE_CHANGED;
    cbs.event = event;
    cbs.set = TB_Set(w);
    if (XmIsRowColumn(XtParent(w)))
    {
	RC_MenuMenuCallback(w, &cbs);
    }
    if (!Lab_SkipCallback(w) && TB_ValueChangedCallback(w))
    {
	XFlush(XtDisplay(w));
	XtCallCallbackList(w, TB_ValueChangedCallback(w), (XtPointer)&cbs);
    }
    if (TB_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.set = TB_Set(w);

	XFlush(XtDisplay(w));
	XtCallCallbackList(w, TB_DisarmCallback(w), (XtPointer)&cbs);
    }

    _XmSetInDragMode(w, False);
}

static void
ButtonDown(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    int validButton;
    XmToggleButtonCallbackStruct cbs;
    Boolean poppedUp;

    /* queue events until the next button event. */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    DEBUGOUT(_LtDebug(__FILE__, w, "ToggleB ButtonDown()\n"));

    if (event && (event->type == ButtonPress))
    {
	RC_MenuButton(w, event, &validButton);
	if (!validButton)
	{
	    return;
	}
    }

    _XmSetInDragMode(w, True);

    RC_MenuShellPopdown(w, event, &poppedUp);
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

    if (!TB_Armed(w))
    {
	TB_Armed(w) = True;

	if (XtIsRealized(w))
	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
			   0, 0, XtWidth(w), XtHeight(w),
			   Lab_Shadow(w), XmSHADOW_OUT);

	if (TB_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.set = TB_Set(w);

	    XFlush(XtDisplay(w));
	    XtCallCallbackList(w, TB_ArmCallback(w), (XtPointer)&cbs);
	}
    }

    _XmRecordEvent(event);
}

static void
Help(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}

static void
EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmToggleButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "ToggleB Enter\n"));

    if (!IN_MENU(w))
    {
	_XmPrimitiveEnter(w, event, NULL, NULL);
	if (TB_Armed(w))
	    draw_toggle(w, NULL, NULL, False, NEXT_TOGGLE(w));
    }
    else
	/* In menu */
    {
	if (_XmGetInDragMode(w))
	{
	    Boolean poppedUp;

	    RC_MenuShellPopdown(w, event, &poppedUp);

	    TB_Armed(w) = True;

	    if (XtIsRealized(w))
		_XmDrawShadows(XtDisplay(w), XtWindow(w),
			       Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
			       0, 0, XtWidth(w), XtHeight(w),
			       Lab_Shadow(w), XmSHADOW_OUT);

	    if (TB_ArmCallback(w))
	    {
		cbs.reason = XmCR_ARM;
		cbs.event = event;
		cbs.set = TB_Set(w);

		XFlush(XtDisplay(w));
		XtCallCallbackList(w, TB_ArmCallback(w), (XtPointer)&cbs);
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
	_XmPrimitiveLeave(w, event, NULL, NULL);
	if (TB_Armed(w))
	    draw_toggle(w, NULL, NULL, False, TB_Set(w));
    }
    else
	/* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{
	    TB_Armed(w) = False;

	    if (XtIsRealized(w))
		_XmClearBorder(XtDisplay(w), XtWindow(w),
			       0, 0, XtWidth(w), XtHeight(w), Lab_Shadow(w));

	    if (TB_DisarmCallback(w))
	    {
		cbs.reason = XmCR_DISARM;
		cbs.event = event;
		cbs.set = TB_Set(w);

		XFlush(XtDisplay(w));
		XtCallCallbackList(w, TB_DisarmCallback(w), (XtPointer)&cbs);
	    }
	}
    }
}

static void
KeySelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KeySelect\n"));

    /* FIX ME */
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
	if (!TB_Armed(w))
	{
	    Boolean poppedUp;

	    RC_MenuShellPopdown(w, NULL, &poppedUp);

	    XmProcessTraversal(w, XmTRAVERSE_CURRENT);
	    TB_Armed(w) = True;

	    if (XtIsRealized(w))
		_XmDrawShadows(XtDisplay(w), XtWindow(w),
			       Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
			       0, 0, XtWidth(w), XtHeight(w),
			       Lab_Shadow(w), XmSHADOW_OUT);

	    if (TB_ArmCallback(w))
	    {
	    XmToggleButtonCallbackStruct cbs;

		cbs.reason = XmCR_ARM;
		cbs.event = NULL;
		cbs.set = TB_Set(w);

		XFlush(XtDisplay(w));
		XtCallCallbackList(w, TB_ArmCallback(w), (XtPointer)&cbs);
	    }
	}
	break;
    case XmMENU_DISARM:
	if (TB_Armed(w))
	{
	    TB_Armed(w) = False;

	    if (XtIsRealized(w))
		_XmClearBorder(XtDisplay(w), XtWindow(w),
			       0, 0, XtWidth(w), XtHeight(w), Lab_Shadow(w));

	    if (TB_DisarmCallback(w))
	    {
	    XmToggleButtonCallbackStruct cbs;

		cbs.reason = XmCR_DISARM;
		cbs.event = NULL;
		cbs.set = TB_Set(w);

		XFlush(XtDisplay(w));
		XtCallCallbackList(w, TB_DisarmCallback(w), (XtPointer)&cbs);
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
	visual_set = TB_VisualSet(w);
    }
    else
    {
	/* Called from an action: set visual_set and draw some stuff */
	if (TB_VisualSet(w) == visual_set)
	    return;
	if (TB_VisualSet(w) == XmINDETERMINATE || visual_set == XmINDETERMINATE)
	{
	    /* FIX ME: Change the GC for a toggle?  That's ridiculous!
	     * Yet it seems necessary - there's no field available
	     * for an indeterminateGC.  - JHG
	     */
	    XtReleaseGC(w, TB_SelectGC(w));
	    TB_VisualSet(w) = visual_set;
	    CreateSelectGC(w);
	} else {
	    TB_VisualSet(w) = visual_set;
	}
	if (!XtIsRealized(w))
	    return;
    }

    /* Fill in the widget only if toggling the background */
    if (Lab_IsText(w) && !TB_IndOn(w) && TB_FillOnSelect(w))
    {
	XFillRectangle(XtDisplay(w), XtWindow(w),
	    visual_set ? TB_SelectGC(w) : TB_BackgroundGC(w),
	    Lab_Highlight(w) + Lab_Shadow(w),
	    Lab_Highlight(w) + Lab_Shadow(w),
	    XtWidth(w) - ((Lab_Highlight(w) + Lab_Shadow(w)) << 1),
	    XtHeight(w) - ((Lab_Highlight(w) + Lab_Shadow(w)) << 1));
    }

    if (is_expose || (!TB_IndOn(w) && TB_FillOnSelect(w)) ||
	(Lab_IsPixmap(w) && ( TB_IndeterminatePixmap(w) != XmUNSPECIFIED_PIXMAP ||
	 (XtSensitive(w) ? TB_OnPixmap(w) : TB_InsenPixmap(w)) != XmUNSPECIFIED_PIXMAP)))
    {
	if (Lab_IsPixmap(w))
	{
	    if (visual_set)
	    {
		/* Switch pixmaps before drawing label */
		if (XtSensitive(w))
		{
		    if (visual_set == XmINDETERMINATE &&
			TB_IndeterminatePixmap(w) != XmUNSPECIFIED_PIXMAP) {
			tmp_pix = Lab_Pixmap(w);
			Lab_Pixmap(w) = TB_IndeterminatePixmap(w);
		    } else if (TB_OnPixmap(w) != XmUNSPECIFIED_PIXMAP) {
			tmp_pix = Lab_Pixmap(w);
			Lab_Pixmap(w) = TB_OnPixmap(w);
		    }
		} else {
		    if (visual_set == XmINDETERMINATE &&
			TB_IndeterminatePixmap(w) != XmUNSPECIFIED_PIXMAP) {
			tmp_pix = Lab_PixmapInsensitive(w);
			Lab_PixmapInsensitive(w) = TB_IndeterminatePixmap(w);
		    } else if (TB_InsenPixmap(w) != XmUNSPECIFIED_PIXMAP) {
			tmp_pix = Lab_PixmapInsensitive(w);
			Lab_PixmapInsensitive(w) = TB_InsenPixmap(w);
		    }
		}
	    }
	    if (!is_expose && (TB_IndOn(w) || !TB_FillOnSelect(w))) {
		XRectangle cliprect;

		/* Changing pixmaps: erase the TextRect just in case
		 * they're different sizes.  Take a bit of trouble here
		 * to avoid excess drawing, which causes more work later.
		 * Based on similar work in Label's expose.
		 */
		cliprect.x =
		    Lab_Highlight(w) + Lab_Shadow(w) + Lab_MarginLeft(w);
		cliprect.y =
		    Lab_Highlight(w) + Lab_Shadow(w) + Lab_MarginTop(w);
		cliprect.width  = XtWidth(w)
		    - ((Lab_Shadow(w) + Lab_Highlight(w)) << 1)
		    - Lab_MarginLeft(w) - Lab_MarginRight(w);
		cliprect.height = XtHeight(w)
		    - ((Lab_Shadow(w) + Lab_Highlight(w)) << 1)
		    - Lab_MarginTop(w) - Lab_MarginBottom(w);
		if (cliprect.x + cliprect.width > Lab_TextRect_x(w) &&
		    cliprect.x < Lab_TextRect_x(w) + Lab_TextRect_width(w) &&
		    cliprect.y + cliprect.height > Lab_TextRect_y(w) &&
		    cliprect.y < Lab_TextRect_y(w) + Lab_TextRect_height(w))
		{
		    if (cliprect.x < Lab_TextRect_x(w))
		    {
			cliprect.width -= Lab_TextRect_x(w) - cliprect.x;
			cliprect.x = Lab_TextRect_x(w);
		    }
		    if (cliprect.x + cliprect.width >
			Lab_TextRect_x(w) + Lab_TextRect_width(w))
		    {
			cliprect.width = (Lab_TextRect_x(w)
			    + Lab_TextRect_width(w)) - cliprect.x;
		    }
		    if (cliprect.y < Lab_TextRect_y(w))
		    {
			cliprect.height -= Lab_TextRect_y(w) - cliprect.y;
			cliprect.y = Lab_TextRect_y(w);
		    }
		    if (cliprect.y + cliprect.height >
			Lab_TextRect_y(w) + Lab_TextRect_height(w))
		    {
			cliprect.height = (Lab_TextRect_y(w)
			    + Lab_TextRect_height(w)) - cliprect.y;
		    }
		    XFillRectangles(XtDisplay(w), XtWindow(w),
				   TB_BackgroundGC(w), &cliprect, 1);
		}
	    }
	}
#define superclass (&xmLabelClassRec)
	(*superclass->core_class.expose) (w, event, region);
#undef superclass
	if (Lab_IsPixmap(w) && visual_set)
	{
	    /* Switch pixmaps back after drawing label */
	    if (tmp_pix != XmUNSPECIFIED_PIXMAP)
	    {
		if (XtSensitive(w))
		    Lab_Pixmap(w) = tmp_pix;
		else
		    Lab_PixmapInsensitive(w) = tmp_pix;
	    }
	}
    }

    if (IN_MENU(w))
    {
	if (is_expose)
	{
	    /* Exposing in a menu: draw or erase shadows */
	    if (TB_Armed(w))
	    {
		_XmDrawShadows(XtDisplay(w), XtWindow(w),
			       Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
			       0, 0, XtWidth(w), XtHeight(w),
			       Lab_Shadow(w), XmSHADOW_OUT);
	    }
	    else
	    {
		_XmClearBorder(XtDisplay(w), XtWindow(w),
			       0, 0, XtWidth(w), XtHeight(w), Lab_Shadow(w));
	    }
	}
    }
    else
    {
	if (is_expose || !TB_IndOn(w))
	{
	    /* Non-menu: draw shadows in or out */
	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
		visual_set == XmINDETERMINATE ? Lab_InsensitiveGC(w) : Prim_TopShadowGC(w),
		visual_set == XmINDETERMINATE ? Lab_InsensitiveGC(w) : Prim_BottomShadowGC(w),
		Lab_Highlight(w), Lab_Highlight(w),
		XtWidth(w) - (Lab_Highlight(w) << 1),
		XtHeight(w) - (Lab_Highlight(w) << 1),
		Lab_Shadow(w), visual_set && !TB_IndOn(w) ? XmSHADOW_IN : XmSHADOW_OUT);
	}
    }

    if (TB_IndOn(w) && (visual_set || !is_expose || TB_Visible(w)))
    {
	/* Draw (or erase) the indicator */

	x = Lab_Highlight(w) + Lab_Shadow(w) + Lab_MarginWidth(w);
	dim = TB_IndicatorDim(w);

	if (TB_IndicatorSet(w) || !Lab_TextRect_height(w))
	{
	    /* Center indicator on label */
	    y = (Lab_MarginTop(w) << 1) + XtHeight(w)
		- Lab_MarginTop(w) - Lab_MarginBottom(w) - dim;
	}
	else
	{
	    /* Make sure implicit indicator isn't too big to fit.
	     * Align it with top line of text.
	     */
	    y = Lab_TextRect_y(w) << 1;
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
		((Lab_Highlight(w) + Lab_Shadow(w) + Lab_MarginHeight(w)) >> 1)
		- Lab_MarginTop(w) - Lab_MarginBottom(w))
		dim = XtHeight(w) - ((Lab_Highlight(w) + Lab_Shadow(w)
		      + Lab_MarginHeight(w)) >> 1)
		    - Lab_MarginTop(w) - Lab_MarginBottom(w);
	}

	/* Monochrome displays (or anything with the select color the same
	 * as a shadow) get inset by one pixel to make things easier to see.
	 */
	fill = Prim_TopShadowColor(w) != TB_SelectColor(w) &&
	    Prim_BottomShadowColor(w) != TB_SelectColor(w);

	if (TB_IndType(w) == XmN_OF_MANY)
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
	y >>= 1;

	if (Lab_StringDirection(w) == XmSTRING_DIRECTION_R_TO_L)
	    x = XtWidth(w) - x - dim;

	switch (TB_IndType(w))
	{
	case XmN_OF_MANY:
	    if (visual_set || TB_Visible(w))
	    {
		if (TB_IndOn(w) & INDICATOR_BOX_MASK)
		{
		    _XmDrawShadows(XtDisplay(w), XtWindow(w),
				   visual_set == XmINDETERMINATE
				   ? Lab_InsensitiveGC(w)
				   : (TB_IndOn(w) & INDICATOR_BOX_MASK)
				   == XmINDICATOR_FLAT_BOX
				   ? Prim_BottomShadowGC(w)
				   : Prim_TopShadowGC(w),
				   visual_set == XmINDETERMINATE
				   ? Lab_InsensitiveGC(w)
				   : Prim_BottomShadowGC(w),
				   x, y, dim, dim, DETAIL_SHADOW_THICKNESS(w),
				   visual_set ? XmSHADOW_IN : XmSHADOW_OUT);
		    delta = DETAIL_SHADOW_THICKNESS(w) + (1 - fill);
		}
		else
		{
		    delta = 0;
		}
		if (dim > delta << 1)
		{
		    if (is_expose || TB_FillOnSelect(w))
		    {
			XFillRectangle(XtDisplay(w), XtWindow(w),
				       visual_set && TB_FillOnSelect(w)
				       ? TB_SelectGC(w) : TB_BackgroundGC(w),
				       x + delta, y + delta,
				       dim - (delta << 1), dim - (delta << 1));
		    }
		DEBUGOUT(_LtDebug(__FILE__, w,
			"draw_toggle tbion %d ibm %d vs %d ie %d tbfos %d\n",
			TB_IndOn(w), ~INDICATOR_BOX_MASK,
			visual_set, is_expose, TB_FillOnSelect(w)));
#if 0
		    if ((TB_IndOn(w) & ~INDICATOR_BOX_MASK) &&
			(visual_set || !(is_expose || TB_FillOnSelect(w))))
		    {
			XmeDrawIndicator(XtDisplay(w), XtWindow(w),
				visual_set == XmINDETERMINATE ? Lab_InsensitiveGC(w) :
					visual_set ? Lab_NormalGC(w) : TB_BackgroundGC(w),
				x, y, dim, dim, delta,
				TB_IndOn(w));
		    }
#else	/* HACK Danny */
		{
			GC	gc;
			switch (visual_set) {
			case XmINDETERMINATE :
				gc = Lab_InsensitiveGC(w);
				break;
			case XmSET :
				gc = Lab_NormalGC(w);
				break;
			case XmUNSET :
			default:
				gc = TB_UnselectGC(w);
				break;
			}
			XmeDrawIndicator(XtDisplay(w), XtWindow(w),
				gc,
				x, y, dim, dim, delta,
				TB_IndOn(w));
			if (visual_set == XmUNSET) {
				XFillRectangle(XtDisplay(w), XtWindow(w), TB_UnselectGC(w),
					x, y, dim, dim);
				_XmDrawShadows(XtDisplay(w), XtWindow(w),
				   visual_set == XmINDETERMINATE
				   ? Lab_InsensitiveGC(w)
				   : (TB_IndOn(w) & INDICATOR_BOX_MASK)
				   == XmINDICATOR_FLAT_BOX
				   ? Prim_BottomShadowGC(w)
				   : Prim_TopShadowGC(w),
				   visual_set == XmINDETERMINATE
				   ? Lab_InsensitiveGC(w)
				   : Prim_BottomShadowGC(w),
				   x, y, dim, dim, DETAIL_SHADOW_THICKNESS(w),
				   visual_set ? XmSHADOW_IN : XmSHADOW_OUT);
			}
		  }
#endif
		}
	    }
	    else
	    {
		XFillRectangle(XtDisplay(w), XtWindow(w),
			       TB_BackgroundGC(w), x, y, dim, dim);
	    }
	    break;

	case XmONE_OF_MANY_ROUND:
	    XmeDrawCircle(XtDisplay(w), XtWindow(w),
		visual_set ? Prim_BottomShadowGC(w) :
			TB_Visible(w) ? Prim_TopShadowGC(w) : TB_BackgroundGC(w),
		visual_set ? Prim_TopShadowGC(w) :
			TB_Visible(w) ? Prim_BottomShadowGC(w) : TB_BackgroundGC(w),
		(is_expose || TB_FillOnSelect(w)) ? visual_set && TB_FillOnSelect(w)
			? TB_SelectGC(w) : TB_BackgroundGC(w) : NULL,
		x, y, dim, dim, TB_DetailShadowThickness(w), 1);
	    break;

	default: /* XmONE_OF_MANY[_DIAMOND] */
	    DEBUGOUT(_LtDebug(__FILE__, w, "Toggle %s\n", visual_set ? "on" : "off"));

	    _XmDrawDiamond(XtDisplay(w), XtWindow(w),
		visual_set ? Prim_BottomShadowGC(w) :
			TB_Visible(w) ? Prim_TopShadowGC(w) : TB_BackgroundGC(w),
		visual_set ? Prim_TopShadowGC(w) :
			TB_Visible(w) ? Prim_BottomShadowGC(w) : TB_BackgroundGC(w),
		(is_expose || TB_FillOnSelect(w))
			? (visual_set && TB_FillOnSelect(w))
				? TB_SelectGC(w)
				: TB_UnselectGC(w)
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

    if (Lab_IsText(w))
    {
	dim = _XmStringHeight(Lab_Font(w), Lab_Label(w))
	    / _XmStringLineCount(Lab_Label(w));
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
	return Lab_TextRect_height(w) < PIXMAP_INDICATOR_ELBOW
	    ? Lab_TextRect_height(w)
	    : PIXMAP_INDICATOR_ELBOW +
	      Lab_TextRect_height(w) / PIXMAP_INDICATOR_ELBOW;
    }
}

Widget
XmCreateToggleButton(Widget parent, char *name,
		     Arg *arglist, Cardinal argcount)
{
    Widget w;

    _XmObjectLock(parent);
    w = XtCreateWidget(name, xmToggleButtonWidgetClass, parent,
		       arglist, argcount);
    _XmObjectUnlock(parent);
    return w;
}

Boolean
XmToggleButtonGetState(Widget w)
{
    Boolean r;

    _XmObjectLock(w);
    r = XmIsToggleButtonGadget(w)
	? XmToggleButtonGadgetGetState(w)
	: XmIsToggleButton(w)
	? TB_Set(w)
	: False;

    _XmObjectUnlock(w);
    return r;
}

void
XmToggleButtonSetState(Widget w, Boolean state, Boolean notify)
{
    XmToggleButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "ToggleB SetState: %d %d\n",
		      state, notify));

    _XmObjectLock(w);
    if (XmIsGadget(w))
    {
	XmToggleButtonGadgetSetState(w, state, notify);
	_XmObjectUnlock(w);
	return;
    }

    if (XmIsToggleButton(w) && TB_Set(w) != state)
    {
	TB_Set(w) = state;
	draw_toggle(w, NULL, NULL, False, state);
	if (notify)
	{
	    cbs.reason = XmCR_VALUE_CHANGED;
	    cbs.event = NULL;
	    cbs.set = TB_Set(w);
	    if (XmIsRowColumn(XtParent(w)))
	    {
		RC_MenuMenuCallback(w, &cbs);
	    }
	    /* Menu callback might cancel the set (radioAlwaysOne). */
	    cbs.set = TB_Set(w);
	    if (!Lab_SkipCallback(w) && TB_ValueChangedCallback(w))
	    {
		XFlush(XtDisplay(w));
		XtCallCallbackList(w, TB_ValueChangedCallback(w),
				   (XtPointer)&cbs);
	    }
	}
    }
    _XmObjectUnlock(w);
}

static void
_XmUnselectColorDefault(Widget w, int offset, XrmValue *val)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmUnselectColorDefault\n"));

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
#if 0
	if (DefaultDepth(XtDisplay(w), 0) == 1) {	/* Mono */
	} else {					/* Colour */
	}
#endif
}

