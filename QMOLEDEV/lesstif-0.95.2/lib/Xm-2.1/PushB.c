/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PushB.c,v 1.3 2005/06/25 09:45:49 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PushB.c,v 1.3 2005/06/25 09:45:49 dannybackx Exp $";

#include <LTconfig.h>

#include <stdarg.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/PushBP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumnP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/TransltnsP.h>
#include <Xm/MenuShell.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>

#include <Xm/TraitP.h>
#include <Xm/ActivatableT.h>
#include <Xm/ContItemT.h>


#include <XmI/DebugUtil.h>


void _XmPushB_TraitAddCallback(Widget, XtCallbackProc, XtPointer, Boolean);

static XmActivatableTraitRec _XmPushBTraitRec = {
	/* version      */      0,
	/* cb           */      _XmPushB_TraitAddCallback
};

static void _XmPushB_ContainerItemGetValues(Widget w, XmContainerItemData d);
static void _XmPushB_ContainerItemSetValues(Widget w, XmContainerItemData d);

static XmContainerItemTraitRec _XmPushB_ContainerItemTrait = {
	/* version */		0,
	/* setvalues */		_XmPushB_ContainerItemGetValues,
	/* getvalues */		_XmPushB_ContainerItemSetValues
};

/* Forward Declarations */

/* Note: JAC speaks:
 * It's possible to invoke these routines with a NULL event via
 * XtCallActionProc.  Unfortunately, they expected an event from which to
 * get the time.  I've modified them to check for the existence of an
 * event before using it in all cases, and to use CurrentTime if it needs
 * the time of a NULL event.  I suppose you could use
 * XtLastTimestampProcessed() as well. */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void initialize_prehook(Widget request,
			       Widget new_w, ArgList args, Cardinal *num_args);

static void initialize_posthook(Widget request,
				Widget new_w, ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static void XmPushButtonExpose(Widget w, XEvent *event, Region region);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void export_show_as_default(Widget sw, int offset, XtArgVal *value);

static XmImportOperator import_show_as_default(Widget sw, int offset,
					       XtArgVal *value);

static void MenuProcEntry(int proc, Widget rc,...);

/*
 * Resources for the pushButton class
 */
#define Offset(field) XtOffsetOf(XmPushButtonRec, pushbutton.field)
static XtResource resources[] =
{
    {
	XmNmultiClick, XmCMultiClick, XmRMultiClick,
	sizeof(unsigned char), Offset(multiClick),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNfillOnArm, XmCFillOnArm, XmRBoolean,
	sizeof(unsigned char), Offset(fill_on_arm),
	XtRImmediate, (XtPointer)True
    },
    {
	XmNarmColor, XmCArmColor, XmRPixel,
	sizeof(Pixel), Offset(arm_color),
	XmRCallProc, (XtPointer)_XmSelectColorDefault
    },
    {
	XmNarmPixmap, XmCArmPixmap, XmRPrimForegroundPixmap,
	sizeof(Pixmap), Offset(arm_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNshowAsDefault, XmCShowAsDefault, XmRBooleanDimension,
	sizeof(Dimension), Offset(show_as_default),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(activate_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNarmCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(arm_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNdisarmCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(disarm_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
    sizeof(Dimension), XtOffsetOf(XmPushButtonRec, primitive.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNdefaultButtonShadowThickness, XmCDefaultButtonShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(default_button_shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    /* Resources redefined from Primitive/Label */
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmPushButtonRec, primitive.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
 sizeof(Dimension), XtOffsetOf(XmPushButtonRec, primitive.highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNshowAsDefault,
	sizeof(Dimension), Offset(show_as_default),
	export_show_as_default, import_show_as_default
    },
    {
	XmNdefaultButtonShadowThickness,
	sizeof(Dimension), Offset(default_button_shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNhighlightThickness,
 sizeof(Dimension), XtOffsetOf(XmPushButtonRec, primitive.highlight_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

static void Arm(Widget w, XEvent *event,
		String *params, Cardinal *num_params);

static void Activate(Widget w, XEvent *event,
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

static void ButtonUp(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void ButtonDown(Widget w, XEvent *event,
		       String *params, Cardinal *num_params);

static void MultiArm(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);

static void MultiActivate(Widget w, XEvent *event,
			  String *params, Cardinal *num_params);

static void KeySelect(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);


static XtTranslations default_trans = NULL;
static XtTranslations menu_trans = NULL;

static XtActionsRec actions[] =
{
    {"Arm", Arm},
    {"MultiArm", MultiArm},
    {"Activate", Activate},
    {"MultiActivate", MultiActivate},
    {"ArmAndActivate", ArmAndActivate},
    {"Disarm", Disarm},
    {"BtnDown", ButtonDown},
    {"BtnUp", ButtonUp},
    {"Enter", EnterWindow},
    {"Leave", LeaveWindow},
    {"KeySelect", KeySelect},
    {"Help", Help},
};

/* *INDENT-OFF* */
static XmBaseClassExtRec _XmPushBCoreClassExtRec = {
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

XmPrimitiveClassExtRec _XmPushBPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ XmInheritBaselineProc,
    /* widget_display_rect */ XmInheritDisplayRectProc,
    /* widget_margins      */ NULL
};

XmPushButtonClassRec xmPushButtonClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelClassRec,
	/* class_name            */ "XmPushButton",
	/* widget_size           */ sizeof(XmPushButtonRec),
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
	/* resize                */ XtInheritResize, /* FIX ME */
	/* expose                */ XmPushButtonExpose,
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
	/* extension             */ (XtPointer)&_XmPushBCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ XmInheritBorderHighlight,
	/* border_unhighlight    */ XmInheritBorderUnhighlight,
	/* translations          */ XtInheritTranslations,
	/* arm_and_activate_proc */ ArmAndActivate,
	/* synthetic resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmPushBPrimClassExtRec
    },
    /* Label Class part */
    {
	/* setOverrideCallback */ XmInheritSetOverrideCallback,
	/* menuProcs           */ XmInheritMenuProc,
	/* translations        */ XtInheritTranslations,
	/* extension           */ NULL
    },
    /* PushButton Class part */
    {
	/* extension */ NULL
    }
};
/* *INDENT-ON* */

WidgetClass xmPushButtonWidgetClass = (WidgetClass)&xmPushButtonClassRec;

/*
   Some #defines to make the code below more readable
 */

#define IN_MENU(w) (Lab_MenuType(w) == XmMENU_POPUP || \
		    Lab_MenuType(w) == XmMENU_PULLDOWN)

static void
class_initialize(void)
{
    menu_trans = XtParseTranslationTable(_XmPushB_menuTranslations);
    default_trans = XtParseTranslationTable(_XmPushB_defaultTranslations);

    _XmPushBCoreClassExtRec.record_type = XmQmotif;

	if (! XmeTraitSet((XtPointer)xmPushButtonWidgetClass, XmQTactivatable,
			(XtPointer)&_XmPushBTraitRec)) {
		_XmWarning(NULL, "XmPushButton ClassInitialize: XmeTraitSet failed\n");
	}
	if (! XmeTraitSet((XtPointer)xmPushButtonWidgetClass, XmQTcontainerItem,
			(XtPointer)&_XmPushB_ContainerItemTrait)) {
		_XmWarning(NULL, "XmPushButton ClassInitialize: XmeTraitSet failed\n");
	}
}

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmPUSH_BUTTON_BIT);
}

static void
CreateFillGC(Widget w)
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
    values.foreground = PB_ArmColor(w);
    values.background = XtBackground(w);

    PB_FillGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateBackgroundGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;

    if (CoreBackgroundPixmap(w) != None &&
	CoreBackgroundPixmap(w) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed background */
	mask |= GCTile;

	values.tile = CoreBackgroundPixmap(w);
	values.fill_style = FillTiled;
    }
    else
    {
	values.fill_style = FillSolid;
    }

    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = XtBackground(w);
    values.background = Prim_Foreground(w);

    PB_BackgroundGC(w) = XtGetGC(w, mask, &values);
}

static void
initialize_prehook(Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
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
    Dimension margin, width, height;

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

    /* check the RepType resources */
    if (PB_MultiClick(new_w) == (unsigned char)XmUNSPECIFIED ||
	!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick),
			    PB_MultiClick(new_w), new_w))
    {
	PB_MultiClick(new_w) =
	    IN_MENU(new_w) ? XmMULTICLICK_DISCARD : XmMULTICLICK_KEEP;
    }

    PB_Armed(new_w) = False;

    CreateFillGC(new_w);
    CreateBackgroundGC(new_w);

    if (Lab_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	PB_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	Lab_Pixmap(new_w) = PB_ArmPixmap(new_w);
    }

    PB_UnarmPixmap(new_w) = Lab_Pixmap(new_w);

    if (XtSensitive(new_w) && Lab_IsPixmap(new_w) &&
	PB_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	_XmLabelGetPixmapSize(new_w, PB_ArmPixmap(new_w), &width, &height);
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
		xmLabelClassRec.core_class.resize(new_w);
	    }
	}
    }

    if (IN_MENU(new_w))
    {
	Lab_Highlight(new_w) = 0;
	/* Install the LabClass menuProcs */
	LabClass_MenuProcs(XtClass(new_w)) = MenuProcEntry;
    }
    else
    {
	/* take care of the default button shadow stuff */
	/*
	 * This new code adjusts button size in two cases : when ShowAsDefault
	 * is non-zero, and when DefaultButtonShadow is non-zero.
	 */
	/*
	 * This really new code finally figures out what that damned
	 * compatible flag does.  Back in the days of 1.1 and earlier,
	 * DefaultButtonShadow didn't exist.  Lesstif has come full
	 * circle, in the same way and for the same reason I suspect
	 * Motif did -- to avoid unnecessary geometry negotiation.
	 * What our original code did caused geometry negotiation to happen
	 * in the set_values method when we changed ShowAsDefault -- even
	 * though we weren't actually changing the geometry of the widget...
	 * See the tail end of the set_values method for changing ShowAsDefault
	 * MLM
	 */
	PB_Compatible(new_w) = !PB_DefaultButtonShadow(new_w);

	if (PB_Compatible(new_w))
	{
	    PB_DefaultButtonShadow(new_w) = PB_ShowAsDefault(new_w);
	}

	if (PB_DefaultButtonShadow(new_w))
	{
	    margin = (PB_DefaultButtonShadow(new_w) << 1) + Lab_Shadow(new_w)
		+ Xm3D_ENHANCE_PIXEL;

	    Lab_MarginLeft(new_w) += margin;
	    Lab_MarginRight(new_w) += margin;
	    Lab_MarginTop(new_w) += margin;
	    Lab_MarginBottom(new_w) += margin;

	    XtWidth(new_w) += margin << 1;
	    XtHeight(new_w) += margin << 1;

	    Lab_TextRect_x(new_w) += margin;
	    Lab_TextRect_y(new_w) += margin;
	    if (Lab_AcceleratorText(new_w)) {
		Lab_AccTextRect(new_w).x += margin;
		Lab_AccTextRect(new_w).y += margin;
	    }

	    /* rws 19 Aug 1997
	     * This does not seem to make sense! Without this the width and
	     * height get set back to their original values when we hit the
	     * constraint_init in the parent.
	     *
	     * This appears to be no longer true.  BulletinBoard, which did
	     * this, if ifdef'd out;  Form records the request (and not new_w)
	     * value in FCP_PrefW/H (which I suspect to be somehow wrong),
	     * but doesn't reset anything.  - JHG 6 Feb 1999
	     */
#if 0
	    XtHeight(request) = XtHeight(new_w);
	    XtWidth(request) = XtWidth(new_w);
#endif

	    DEBUGOUT(_LtDebug(__FILE__, new_w,
			      "adjust margins for default button\n"
			      "\trequest X %5i Y %5i W %5i H %5i\n"
			      "\t  new_w X %5i Y %5i W %5i H %5i\n",
			      XtX(request), XtY(request),
			      XtWidth(request), XtHeight(request),
			      XtX(new_w), XtY(new_w),
			      XtWidth(new_w), XtHeight(new_w)));
	}
    }

    PB_Timer(new_w) = 0;
}

static void
destroy(Widget w)
{
    if (PB_Timer(w) != 0)
    {
	XtRemoveTimeOut(PB_Timer(w));
	PB_Timer(w) = 0;
    }

    XtReleaseGC(w, PB_FillGC(w));
    XtReleaseGC(w, PB_BackgroundGC(w));
}

static Boolean
set_values(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;
    Dimension margin, width, height;

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

    /* check the RepType resources */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick), PB_MultiClick(new_w), new_w))
	PB_MultiClick(new_w) = PB_MultiClick(old);

    /* Fix up GCs for new colors */
    if (PB_ArmColor(new_w) != PB_ArmColor(old)) {
	XtReleaseGC(new_w, PB_FillGC(new_w));
	CreateFillGC(new_w);
	refresh_needed = True;
    }
    if (XtBackground(new_w) != XtBackground(old) ||
	CoreBackgroundPixmap(new_w) != CoreBackgroundPixmap(old)) {
	XtReleaseGC(new_w, PB_BackgroundGC(new_w));
	CreateBackgroundGC(new_w);
	refresh_needed = True;
    }

    if (IN_MENU(new_w)) {
	/* Menu: highlight must be zero */
	Lab_Highlight(new_w) = 0;
    } else {
	/* Non-Menu: deal with default shadows */
	if (PB_DefaultButtonShadow(new_w) != PB_DefaultButtonShadow(old)) {
	    PB_Compatible(new_w) = False;
	}

	if (PB_Compatible(new_w))
	{
	    PB_DefaultButtonShadow(new_w) = PB_ShowAsDefault(new_w);
	}

	if (PB_DefaultButtonShadow(new_w) != PB_DefaultButtonShadow(old))
	{
	    /* pushbutton/test16 */
	    margin = (PB_DefaultButtonShadow(new_w) -
		      PB_DefaultButtonShadow(old)) << 1;
	    if (PB_DefaultButtonShadow(new_w) && !PB_DefaultButtonShadow(old))
		margin += Lab_Shadow(new_w) + Xm3D_ENHANCE_PIXEL;
	    else if (!PB_DefaultButtonShadow(new_w) &&
		     PB_DefaultButtonShadow(old))
		margin -= Lab_Shadow(old) + Xm3D_ENHANCE_PIXEL;

	    Lab_MarginLeft(new_w) += margin;
	    Lab_MarginRight(new_w) += margin;
	    Lab_MarginTop(new_w) += margin;
	    Lab_MarginBottom(new_w) += margin;

	    /* Actually, according to testing, we do change the height/width,
	     * even if the widget is realized, and even if recomputeSize is
	     * false.  - JHG 5 Feb 1999
	     */
	    XtWidth(new_w) += margin << 1;
	    XtHeight(new_w) += margin << 1;

	    Lab_TextRect_x(new_w) += margin;
	    Lab_TextRect_y(new_w) += margin;
	    if (Lab_AcceleratorText(new_w) != NULL) {
		Lab_AccTextRect(new_w).x += margin;
		Lab_AccTextRect(new_w).y += margin;
	    }

	    refresh_needed = True;
	}
    }

    /* Changes to the armed or unarmed pixmap */
    if (Lab_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	PB_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	Lab_Pixmap(new_w) = PB_ArmPixmap(new_w);

	if (Lab_IsPixmap(new_w))
	    refresh_needed = True;
    }

    if (Lab_Pixmap(new_w) != Lab_Pixmap(old))
	PB_UnarmPixmap(new_w) = Lab_Pixmap(new_w);

    if (Lab_IsPixmap(new_w))
    {
	Lab_Pixmap(new_w) = PB_Armed(new_w)
	    ? PB_ArmPixmap(new_w)
	    : PB_UnarmPixmap(new_w);

	if (XtSensitive(new_w) &&
	    (Lab_RecomputeSize(new_w) ||
	     Lab_Pixmap(request) != Lab_Pixmap(old) ||
	     PB_ArmPixmap(new_w) != PB_ArmPixmap(old)))
	{
	    _XmLabelGetPixmapSize(new_w, PB_ArmPixmap(new_w), &width, &height);
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
		    xmLabelClassRec.core_class.resize(new_w);
		}

		width = XtWidth(new_w);
		height = XtHeight(new_w);
		XtWidth(new_w) = XtWidth(old);
		XtHeight(new_w) = XtHeight(old);
		xmLabelClassRec.core_class.resize(new_w);
		XtWidth(new_w) = width;
		XtHeight(new_w) = height;
	    }
	}
    }

    /* May need to draw/erase armed state */
    if ((PB_Armed(new_w) && PB_FillOnArm(new_w) != PB_FillOnArm(old)) ||
	XtSensitive(new_w) != XtSensitive(old))
    {
	refresh_needed = True;
    }

    /* If only showAsDefault changed, draw/erase the shadow now */
    if (!refresh_needed && !IN_MENU(new_w) && XtIsRealized(new_w) && XtIsManaged(new_w) &&
	PB_ShowAsDefault(new_w) != PB_ShowAsDefault(old)) {
	Dimension inset = Lab_Highlight(new_w) + Xm3D_ENHANCE_PIXEL;
	GC tgc, bgc;

	if (PB_ShowAsDefault(new_w)) {
	    if (_XmDifferentBackground(new_w, XtParent(new_w))) {
		tgc = XmParentTopShadowGC(new_w);
		bgc = XmParentBottomShadowGC(new_w);
	    } else {
		tgc = Prim_TopShadowGC(new_w);
		bgc = Prim_BottomShadowGC(new_w);
	    }
	    _XmDrawShadows(XtDisplay(new_w), XtWindow(new_w), tgc, bgc,
			   inset, inset,
			   XtWidth(new_w) - (inset << 1),
			   XtHeight(new_w) - (inset << 1),
			   PB_DefaultButtonShadow(new_w),
			   XmSHADOW_IN);
	} else {
	    _XmDrawSimpleHighlight(XtDisplay(new_w), XtWindow(new_w),
				   XmParentBackgroundGC(new_w),
				   inset, inset,
				   XtWidth(new_w) - (inset << 1),
				   XtHeight(new_w) - (inset << 1),
				   PB_DefaultButtonShadow(new_w));
	}
    }

    return refresh_needed;
}

static void
XmPushButtonExpose(Widget w, XEvent *event, Region region)
{
    Dimension inset;

    if (!XtIsRealized(w))
    {
	_XmWarning(w,
	    "expose called on unrealized widget - this shouldn't happen");
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "PB expose\n"));

    if (!IN_MENU(w))
    {
	inset = Lab_Highlight(w);
	if (PB_DefaultButtonShadow(w))
	    inset += Xm3D_ENHANCE_PIXEL + Lab_Shadow(w)
		  + (PB_DefaultButtonShadow(w) << 1);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "x %d y %d w %d h %d b %d\n",
			  inset, inset,
			  (XtWidth(w) - (inset << 1)),
			  (XtHeight(w) - (inset << 1)),
			  XtBorderWidth(w)));

	/*
	 * this was badly wrong, and relied on label to _NOT_ overwrite
	 * PB shadows.  The correct order is: background, label, shadows.
	 *
	 * Chris:  No.  If you look at the output of xscope, you'll see
	 * that the label's expose routine is called after the pushbuttons.
	 * In general this is the way it's done -- you call your expose
	 * routine, and then call your superclass's.
	 */
	if (PB_FillOnArm(w))
	{
	    XFillRectangle(XtDisplay(w), XtWindow(w),
			   PB_Armed(w) ? PB_FillGC(w) : PB_BackgroundGC(w),
			   inset + Lab_Shadow(w), inset + Lab_Shadow(w),
			   XtWidth(w) - ((inset + Lab_Shadow(w)) << 1),
			   XtHeight(w) - ((inset + Lab_Shadow(w)) << 1));
	}

	/* now draw the normal shadow */
	_XmDrawShadows(XtDisplay(w), XtWindow(w),
		       Prim_TopShadowGC(w),
		       Prim_BottomShadowGC(w),
		       inset, inset,
		       XtWidth(w) - (inset << 1),
		       XtHeight(w) - (inset << 1),
		       Lab_Shadow(w),
		       PB_Armed(w) ? XmSHADOW_IN : XmSHADOW_OUT);

	/* take care of the default button stuff */
	if (PB_DefaultButtonShadow(w))
	{
	    GC tgc, bgc;

	    if (_XmDifferentBackground(w, XtParent(w)))
	    {
		tgc = XmParentTopShadowGC(w);
		bgc = XmParentBottomShadowGC(w);
		_XmDrawSimpleHighlight(XtDisplay(w), XtWindow(w),
				       XmParentBackgroundGC(w),
				       0, 0, XtWidth(w), XtHeight(w),
				       inset);
	    }
	    else
	    {
		tgc = Prim_TopShadowGC(w);
		bgc = Prim_BottomShadowGC(w);
	    }

	    if (PB_ShowAsDefault(w))
	    {
		inset = Lab_Highlight(w) + Xm3D_ENHANCE_PIXEL;
		_XmDrawShadows(XtDisplay(w), XtWindow(w), tgc, bgc,
			       inset, inset,
			       XtWidth(w) - (inset << 1),
			       XtHeight(w) - (inset << 1),
			       PB_DefaultButtonShadow(w),
			       XmSHADOW_IN);
	    }
	}
    }
    else
    {
	if (PB_Armed(w))
	{
	    _XmDrawShadows(XtDisplay(w), XtWindow(w),
			   Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
			   0, 0,
			   XtWidth(w), XtHeight(w),
			   Lab_Shadow(w), XmSHADOW_OUT);
	}
	else
	{
	    _XmClearBorder(XtDisplay(w), XtWindow(w),
			   0, 0,
			   XtWidth(w), XtHeight(w),
			   Lab_Shadow(w));
	}
    }

    if (Lab_IsPixmap(w))
    {
	Lab_Pixmap(w) = PB_Armed(w) && PB_ArmPixmap(w) != XmUNSPECIFIED_PIXMAP
	    ? PB_ArmPixmap(w)
	    : PB_UnarmPixmap(w);
    }

#define superclass (&xmLabelClassRec)
    (*superclass->core_class.expose) (w, event, region);
#undef superclass
}

/* Like _XmFromHorizontalPixels,
 * but don't allow a nonzero value to become zero.
 */
static void
export_show_as_default(Widget w, int offset, XtArgVal *value)
{
    int converted_value;

    converted_value = XmConvertUnits(w, XmHORIZONTAL, XmPIXELS, *value,
				     Prim_UnitType(w));

    *value = !converted_value && *value
	? 1
	: converted_value;
}

/* Like _XmToHorizontalPixels,
 * but don't allow a nonzero value to become zero.
 */
static XmImportOperator
import_show_as_default(Widget w, int offset, XtArgVal *value)
{
    int converted_value;

    converted_value = XmConvertUnits(w, XmHORIZONTAL, Prim_UnitType(w),
				     *value, XmPIXELS);

    PB_ShowAsDefault(w) = *value == !converted_value && *value
	? 1
	: converted_value;
    return XmSYNTHETIC_NONE;
}

static void
Arm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;

    XmProcessTraversal(w, XmTRAVERSE_CURRENT);

    if (!PB_Armed(w))
    {
	PB_Armed(w) = True;
	/* JAC added check for existence of event */
	if (event)
	{
	    PB_ArmTimeStamp(w) = event->xbutton.time;
	}
	else
	{
	    PB_ArmTimeStamp(w) = CurrentTime;
	}

	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, NULL);

	if (PB_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.click_count = PB_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PB_ArmCallback(w),
			       (XtPointer)&cbs);
	}
    }
}


static void
Activate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;
    XButtonEvent *ev = (XButtonEvent *)event;

    DEBUGOUT(_LtDebug(__FILE__, w, "Activate()\n"));
    DEBUGOUT(_LtDebug("ENTRY", w, "Activate()\n"));

    /*
     * This test also broke Accelerators. Refined as well.
     * Guys, please watch out !!
     * Danny 23/5/1996
     *
     * JAC added check for existence of ev
     *
     */
    if (ev && (ev->type == ButtonPress || ev->type == ButtonRelease) &&
	PB_Armed(w) == False)
    {
	return;
    }

    PB_ClickCount(w) = 1;
    PB_Armed(w) = False;

    if (XtIsRealized(w))
	XtClass(w)->core_class.expose(w, event, NULL);

    /*
     * This test should not be necessary.
     * It happens to break accelerators that trigger a button.
     * Danny 5/4/96
     * MLM: Not having means PB's break if the button is released outside
     * the widget after it is armed. Check testXm/pushbutton/test1.  Arm
     * (click) on the button, move outside the button, and release.  If this
     * isn't here, the arm callback will be executed (and it shouldn't be).
     *
     * Test refined so it doesn't fail for accelerators. -- Danny
     *
     * JAC added check for existence of ev
     */
    if (ev && (ev->type == KeyPress || ev->type == KeyRelease
	       || ((ev->x >= 0 && ev->x < XtWidth(w))
		   && (ev->y >= 0 && ev->y < XtHeight(w)))))
    {
	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.click_count = PB_ClickCount(w);
	if (XmIsRowColumn(XtParent(w)))
	{
	    RC_MenuMenuCallback(w, &cbs);
	}
	if (!Lab_SkipCallback(w) && PB_ActivateCallback(w))
	{
	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PB_ActivateCallback(w),
			       (XtPointer)&cbs);
	}
    }
}

static void
Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;

    if (PB_Armed(w))
    {
	PB_Armed(w) = False;
	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, NULL);
    }

    if (PB_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = PB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }
}

static void
ArmTimeout(XtPointer data, XtIntervalId *id)
{
    Widget w = (Widget)data;

    DEBUGOUT(_LtDebug(__FILE__, w, "ArmTimeout\n"));

    PB_Timer(w) = 0;

    if (XtIsRealized(w))
    {
	XtClass(w)->core_class.expose(w, NULL, NULL);
	XFlush(XtDisplay(w));
    }
}

static void
ArmAndActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;
    Boolean poppedUp;

    DEBUGOUT(_LtDebug(__FILE__, w, "ArmAndActivate\n"));
    DEBUGOUT(_LtDebug("MENU", w, "%s:ArmAndActivate(%d)\n",
    	__FILE__, __LINE__
    	));

    /* Arm, Activate, and Disarm now, but draw the disarmed state later */
    Arm(w, event, params, num_params);

    if (IN_MENU(w))
    {
	RC_MenuButtonPopdown(w, event, &poppedUp);
    }

    cbs.reason = XmCR_ACTIVATE;
    cbs.event = event;
    cbs.click_count = 1;
    if (XmIsRowColumn(XtParent(w)))
    {
	RC_MenuMenuCallback(w, &cbs);
    }
    if (!Lab_SkipCallback(w) && PB_ActivateCallback(w))
    {
	XFlush(XtDisplay(w));
	XtCallCallbackList(w,
			   PB_ActivateCallback(w),
			   (XtPointer)&cbs);
    }
    PB_Armed(w) = False;
    if (PB_DisarmCallback(w))
    {
	XFlush(XtDisplay(w));
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = 1;
	XtCallCallbackList(w,
			   PB_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    if (IN_MENU(w))
    {
    }
    else
    {
	if (PB_Timer(w) != 0)
	{
	    XtRemoveTimeOut(PB_Timer(w));
	    PB_Timer(w) = 0;
	}

	PB_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				  ACTIVATE_DELAY, ArmTimeout, (XtPointer)w);
    }
}

static void
Help(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    /* unpost menus */

    /* restore focus */

    /* invoke help callbacks */
    XtCallActionProc(w, "PrimitiveHelp", event, params, *num_params);
}

static void
EnterWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "PushB Enter\n"));
    DEBUGOUT(_LtDebug("MENU", w, "%s:EnterWindow(%d)\n",
    	__FILE__, __LINE__
    	));

    if (!IN_MENU(w))
    {
	_XmPrimitiveEnter(w, event, NULL, NULL);

	if (PB_Armed(w) && XtIsRealized(w))
	{
	    XtClass(w)->core_class.expose(w, event, NULL);
	}
    }
    else
	/* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{
	    Boolean poppedUp;

	    RC_MenuShellPopdown(w, event, &poppedUp);

	    PB_Armed(w) = True;

	    if (XtIsRealized(w))
		XtClass(w)->core_class.expose(w, event, NULL);

	    if (PB_ArmCallback(w))
	    {
		cbs.reason = XmCR_ARM;
		cbs.event = event;
		cbs.click_count = PB_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   PB_ArmCallback(w),
				   (XtPointer)&cbs);
	    }
	}
    }
}

static void
LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "LeaveWindow()\n"));
    DEBUGOUT(_LtDebug("MENU", w, "%s:LeaveWindow(%d) - %s %s %s\n",
    	__FILE__, __LINE__,
    	IN_MENU(w) ? "menu" : "no-menu",
    	IN_MENU(w) && _XmGetInDragMode(w) ? "dragging" : "not-dragging",
    	IN_MENU(w) && PB_Armed(w) ? "armed" : "not-armed"
    	));
    if (!IN_MENU(w))
    {
	_XmPrimitiveLeave(w, event, NULL, NULL);
	if (PB_Armed(w) && XtIsRealized(w))
	{
	    PB_Armed(w) = False;
	    XtClass(w)->core_class.expose(w, event, NULL);
	    PB_Armed(w) = True;
	}
    }
    else
	/* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{
	    if (PB_Armed(w))
	    {
		PB_Armed(w) = False;

		if (XtIsRealized(w))
		    XtClass(w)->core_class.expose(w, event, NULL);

		if (PB_DisarmCallback(w))
		{
		    cbs.reason = XmCR_DISARM;
		    cbs.event = event;
		    cbs.click_count = PB_ClickCount(w);

		    XFlush(XtDisplay(w));

		    XtCallCallbackList(w,
				       PB_DisarmCallback(w),
				       (XtPointer)&cbs);
		}
	    }
	}
    }
}

static void
ButtonUp(Widget w,
	 XEvent *event,
	 String *params,
	 Cardinal *num_params)
{
    Boolean validButton, poppedUp;
    XmPushButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s(%d) - ButtonUp()\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("MENU", w, "%s:ButtonUp(%d)\n",
    	__FILE__, __LINE__
    	));

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
    DEBUGOUT(_LtDebug(__FILE__, w, "%s(%d):ButtonUp() - valid %s %ix%i\n",
    	__FILE__, __LINE__,
    	_LtDebugBoolean2String(validButton),
    	event->xbutton.x, event->xbutton.y));

    if (!validButton)
    {
	return;
    }

    if (!PB_Armed(w))
    {
	return;
    }

    if (event && event->xbutton.x < XtWidth(w) && event->xbutton.y < XtHeight(w))
    {
    PB_Armed(w) = False;

    RC_MenuButtonPopdown(w, event, &poppedUp);

    _XmRecordEvent(event);

    _XmClearBorder(XtDisplay(w), XtWindow(w),
		   0, 0,
		   XtWidth(w), XtHeight(w),
		   Lab_Shadow(w));

    cbs.reason = XmCR_ACTIVATE;
    cbs.event = event;
    cbs.click_count = PB_ClickCount(w);
    if (XmIsRowColumn(XtParent(w)))
    {
	RC_MenuMenuCallback(w, &cbs);
    }
    if (!Lab_SkipCallback(w) && PB_ActivateCallback(w))
    {
	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PB_ActivateCallback(w),
			   &cbs);
    }
    if (PB_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = PB_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PB_DisarmCallback(w),
			   &cbs);
    }

    _XmSetInDragMode(w, False);
    }
}

static void
ButtonDown(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    /* modified from the MegaButton widget */
    int validButton;
    XmPushButtonCallbackStruct cbs;
    Boolean poppedUp;

    DEBUGOUT(_LtDebug(__FILE__, w, "ButtonDown()\n"));
    DEBUGOUT(_LtDebug("MENU", w, "%s:ButtonDown(%d)\n",
    	__FILE__, __LINE__
    	));
    /* queue events until the next button event */
    XAllowEvents(XtDisplay(w), SyncPointer, CurrentTime);

    if (event && (event->type == ButtonPress))
    {
	RC_MenuButton(w, event, &validButton);

	if (!validButton)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "ButtonDown() - Invalid button\n"));
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

    _XmDrawShadows(XtDisplay(w), XtWindow(w),
		   Prim_TopShadowGC(w), Prim_BottomShadowGC(w),
		   0, 0,
		   XtWidth(w), XtHeight(w),
		   Lab_Shadow(w), XmSHADOW_OUT);

    if (!PB_Armed(w))
    {
	PB_Armed(w) = True;
	if (PB_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.click_count = PB_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PB_ArmCallback(w),
			       (XtPointer)&cbs);
	}
    }

    _XmRecordEvent(event);
}

static void
MultiActivate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    XmPushButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "PushB: MultiClick\n"));

    if (PB_MultiClick(w) == XmMULTICLICK_KEEP)
    {
	Time mctime = XtGetMultiClickTime(XtDisplay(w));

	/* JAC added check for existence of event */
	if ((event ? event->xbutton.time : CurrentTime) - PB_ArmTimeStamp(w)
	    < mctime)
	{
	    PB_ClickCount(w)++;
	}
	else
	{
	    PB_ClickCount(w) = 1;
	}

	PB_Armed(w) = False;

	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, NULL);

	/* JAC added check for existence of ev */
	if (ev && (ev->type == KeyPress || ev->type == KeyRelease
		   || ((ev->x >= 0 && ev->x < XtWidth(w))
		       && (ev->y >= 0 && ev->y < XtHeight(w)))))
	{
	    cbs.reason = XmCR_ACTIVATE;
	    cbs.event = event;
	    cbs.click_count = PB_ClickCount(w);
	    if (XmIsRowColumn(XtParent(w)))
	    {
		RC_MenuMenuCallback(w, &cbs);
	    }
	    if (!Lab_SkipCallback(w) && PB_ActivateCallback(w))
	    {
		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   PB_ActivateCallback(w),
				   (XtPointer)&cbs);
	    }
	}
	Disarm(w, event, params, num_params);
    }
}

static void
MultiArm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "PushB: MultiArm\n"));

    if (PB_MultiClick(w) == XmMULTICLICK_KEEP)
	Arm(w, event, NULL, NULL);
}

static void
KeySelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "KeySelect\n"));

    /* FIX ME */
}

void
_XmClearBCompatibility(Widget w)
{
    _XmObjectLock(w);
    PB_Compatible(w) = False;
    _XmObjectUnlock(w);
}

static void
MenuProcEntry(int proc, Widget w,...)
{
    va_list arg_list;
    Cardinal num_params = 0;

    va_start(arg_list, w);

    switch (proc)
    {
    case XmMENU_ARM:
	if (!PB_Armed(w))
	{
	    Arm(w, NULL, NULL, &num_params);
	}
	break;
    case XmMENU_DISARM:
	if (PB_Armed(w))
	{
	    Disarm(w, NULL, NULL, &num_params);
	}
	break;
    default:
	_XmWarning(w, "%s(%d) - Invalid menuProc function", __FILE__, __LINE__);
	break;
    }

    va_end(arg_list);
}

Widget
XmCreatePushButton(Widget parent, char *name, Arg *arglist, Cardinal argcount)
{
	Widget	r;
	_XmObjectLock(parent);
	r = XtCreateWidget(name, xmPushButtonWidgetClass, parent, arglist, argcount);
	_XmObjectUnlock(parent);
	return r;
}

void _XmPushB_TraitAddCallback(Widget w, XtCallbackProc cb, XtPointer cbp, Boolean set)
{
	if (set)
		XtAddCallback(w, XmNactivateCallback, cb, cbp);
	else
		XtRemoveCallback(w, XmNactivateCallback, cb, cbp);
}

/*
 * Trait functions for ContainerItem
 */
static void _XmPushB_ContainerItemGetValues(Widget w, XmContainerItemData d)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmPushB_ContainerItemGetValues()\n"));
}

static void _XmPushB_ContainerItemSetValues(Widget w, XmContainerItemData d)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmPushB_ContainerItemSetValues()\n"));
}
