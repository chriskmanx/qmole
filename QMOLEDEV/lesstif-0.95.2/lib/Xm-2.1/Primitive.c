/**
 *
 * $Id: Primitive.c,v 1.4 2005/06/25 09:45:49 dannybackx Exp $
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

static const char rcsid[] = "$Id: Primitive.c,v 1.4 2005/06/25 09:45:49 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/RepType.h>
#include <Xm/ManagerP.h>
#include <Xm/TransltnsP.h>
#include <Xm/VirtKeysP.h>

/* Includes for 2.0 features */
#include <XmI/DirectionI.h>
#include <Xm/CareVisualT.h>


#include <XmI/DebugUtil.h>


/* Forward Declarations */
static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void XmPrimitiveRealize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static void destroy(Widget w);
static Boolean XmPrimitiveSetValues(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void focus_change(Widget w, XmFocusChange fevent);
static XmNavigability widget_navigable(Widget w);
static void primitive_border_unhighlight(Widget w);
static void primitive_border_highlight(Widget w);
static void _XmPrimitiveExportY(Widget widget, int offset, XtArgVal *value);
static void _XmPrimitiveExportX(Widget widget, int offset, XtArgVal *value);

/*
 * Resources for the primitive class
 */
#define Offset(field) XtOffsetOf(XmPrimitiveRec, primitive.field)
static XtResource resources[] =
{
    {
	XmNunitType, XmCUnitType, XmRUnitType,
	sizeof(unsigned char), Offset(unit_type),
	XmRCallProc, (XtPointer)_XmUnitTypeDefault
    },
    {
	XmNx, XmCPosition, XmRHorizontalPosition,
	sizeof(Position), XtOffsetOf(XmPrimitiveRec, core.x),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNy, XmCPosition, XmRVerticalPosition,
	sizeof(Position), XtOffsetOf(XmPrimitiveRec, core.y),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNwidth, XmCDimension, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPrimitiveRec, core.width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCDimension, XmRVerticalDimension,
	sizeof(Dimension), XtOffsetOf(XmPrimitiveRec, core.height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNborderWidth, XmCBorderWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPrimitiveRec, core.border_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNforeground, XmCForeground, XmRPixel,
	sizeof(Pixel), Offset(foreground),
	XmRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {
	XmNbackground, XmCBackground, XmRPixel,
	sizeof(Pixel), XtOffsetOf(XmPrimitiveRec, core.background_pixel),
	XmRCallProc, (XtPointer)_XmBackgroundColorDefault
    },
    {
	XmNbackgroundPixmap, XmCPixmap, XmRXmBackgroundPixmap,
	sizeof(Pixmap), XtOffsetOf(XmPrimitiveRec, core.background_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), Offset(traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightOnEnter, XmCHighlightOnEnter, XmRBoolean,
	sizeof(Boolean), Offset(highlight_on_enter),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), Offset(navigation_type),
	XmRImmediate, (XtPointer)XmNONE
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNhighlightColor, XmCHighlightColor, XmRPixel,
	sizeof(Pixel), Offset(highlight_color),
	XmRCallProc, (XtPointer)_XmHighlightColorDefault
    },
    {
	XmNhighlightPixmap, XmCHighlightPixmap, XmRHighlightPixmap,
	sizeof(Pixmap), Offset(highlight_pixmap),
	XmRCallProc, (XtPointer)_XmPrimitiveHighlightPixmapDefault
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNtopShadowColor, XmCTopShadowColor, XmRPixel,
	sizeof(Pixel), Offset(top_shadow_color),
	XmRCallProc, (XtPointer)_XmTopShadowColorDefault
    },
    {
	XmNtopShadowPixmap, XmCTopShadowPixmap, XmRTopShadowPixmap,
	sizeof(Pixmap), Offset(top_shadow_pixmap),
	XmRCallProc, (XtPointer)_XmPrimitiveTopShadowPixmapDefault
    },
    {
	XmNbottomShadowColor, XmCBottomShadowColor, XmRPixel,
	sizeof(Pixel), Offset(bottom_shadow_color),
	XmRCallProc, (XtPointer)_XmBottomShadowColorDefault
    },
    {
	XmNbottomShadowPixmap, XmCBottomShadowPixmap, XmRBottomShadowPixmap,
	sizeof(Pixmap), Offset(bottom_shadow_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNhelpCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(help_callback),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNuserData, XmCUserData, XmRPointer,
	sizeof(XtPointer), Offset(user_data),
	XmRImmediate, (XtPointer)NULL
    }
    ,{
	XmNconvertCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(convert_callback),
	XtRPointer, (XtPointer)NULL
    },
    {
	XmNpopupHandlerCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(popup_handler_callback),
	XtRPointer, (XtPointer)NULL
    },
    {
	XmNlayoutDirection, XmCLayoutDirection, XmRDirection,
	sizeof(XmDirection), Offset(layout_direction),
	XmRCallProc, (XtPointer)_XmDirectionDefault
    }
};

#define POffset(field)	XtOffset(XmPrimitiveWidget, primitive.field)
#define COffset(field)	XtOffset(XmPrimitiveWidget, core.field)
static XmSyntheticResource syn_resources[] =
{
    /* core part */
    {
	XmNx,
	sizeof(Position), COffset(x),
	_XmPrimitiveExportX, _XmToHorizontalPixels
    },
    {
	XmNy,
	sizeof(Position), COffset(y),
	_XmPrimitiveExportY, _XmToVerticalPixels
    },
    {
	XmNwidth,
	sizeof(Dimension), COffset(width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNheight,
	sizeof(Dimension), COffset(height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNborderWidth,
	sizeof(Dimension), COffset(border_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    /* primitive */
    {
	XmNhighlightThickness,
	sizeof(Dimension), POffset(highlight_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNshadowThickness,
	sizeof(Dimension), POffset(shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
};

static XtActionsRec actions[] =
{
    {"PrimitiveFocusIn", _XmPrimitiveFocusIn},
    {"PrimitiveFocusOut", _XmPrimitiveFocusOut},
    {"PrimitiveUnmap", _XmPrimitiveUnmap},
    {"PrimitiveHelp", _XmPrimitiveHelp},
    {"PrimitiveEnter", _XmPrimitiveEnter},
    {"PrimitiveLeave", _XmPrimitiveLeave},
    {"PrimitiveTraverseLeft", _XmTraverseLeft},
    {"PrimitiveTraverseRight", _XmTraverseRight},
    {"PrimitiveTraverseUp", _XmTraverseUp},
    {"PrimitiveTraverseDown", _XmTraverseDown},

    {"PrimitiveTraverseNext", _XmTraverseNext},
    {"PrimitiveTraversePrev", _XmTraversePrev},

    {"PrimitiveTraverseHome", _XmTraverseHome},
    {"PrimitiveNextTabGroup", _XmTraverseNextTabGroup},
    {"PrimitivePrevTabGroup", _XmTraversePrevTabGroup},
    {"PrimitiveParentActivate", _XmPrimitiveParentActivate},
    {"PrimitiveParentCancel", _XmPrimitiveParentCancel},
    {"unmap", _XmPrimitiveUnmap},
    {"Help", _XmPrimitiveHelp},
    {"enter", _XmPrimitiveEnter},
    {"leave", _XmPrimitiveLeave},
    {"PrevTabGroup", _XmTraverseNextTabGroup},
    {"NextTabGroup", _XmTraversePrevTabGroup},
};


static XmBaseClassExtRec _XmPrimitiveCoreClassExtRec = {
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
    /* focus_change              */ focus_change,
    /* wrapper_data              */ NULL
};

XmPrimitiveClassExtRec _XmPrimClassExtRec = {
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ NULL,
    /* widget_display_rect */ NULL,
    /* widget_margins      */ NULL
};

XmPrimitiveClassRec xmPrimitiveClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &coreClassRec,
	/* class_name            */ "XmPrimitive",
	/* widget_size           */ sizeof(XmPrimitiveRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XmPrimitiveRealize,
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
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ XmPrimitiveSetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ _XmPrimitiveGetValuesHook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersionDontCheck,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL /*XtInheritDisplayAccelerator*/,
	/* extension             */ (XtPointer)&_XmPrimitiveCoreClassExtRec
    },
    /* Primitive Class part */
    {
	/* border_highlight      */ primitive_border_highlight,
	/* border_unhighlight    */ primitive_border_unhighlight,
	/* translations          */ _XmPrimitive_defaultTranslations,
	/* arm_and_activate_proc */ NULL,
	/* Synthetic Resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer)&_XmPrimClassExtRec
    }
};

WidgetClass xmPrimitiveWidgetClass = (WidgetClass)&xmPrimitiveClassRec;

/* Trait record */
static Boolean _PrimitiveTrait_Redraw(Widget, Widget, Widget, Mask);

static XmCareVisualTraitRec _XmPrimitiveTraitRec = {
	/* version */		0,
	/* redraw */		_PrimitiveTrait_Redraw
};

static void
class_initialize(void)
{
	_XmRegisterConverters();
	_XmRegisterPixmapConverters();
	_XmInitializeExtensions();
	_XmPrimitiveCoreClassExtRec.record_type = XmQmotif;

	if (! XmeTraitSet((XtPointer)xmPrimitiveWidgetClass, XmQTcareParentVisual,
			(XtPointer)&_XmPrimitiveTraitRec)) {
		_XmWarning(NULL, "XmPrimitive ClassInitialize: XmeTraitSet failed\n");
	}
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmPrimitiveWidgetClass pwc, swc;

    pwc = (XmPrimitiveWidgetClass)widget_class;
    swc = (XmPrimitiveWidgetClass)widget_class->core_class.superclass;

    /* Handle the Primitive Widget class's inheritance stuff */
    if (pwc->primitive_class.border_highlight == XmInheritBorderHighlight)
    {
	pwc->primitive_class.border_highlight =
	    swc->primitive_class.border_highlight;
    }
    if (pwc->primitive_class.border_unhighlight == XmInheritBorderUnhighlight)
    {
	pwc->primitive_class.border_unhighlight =
	    swc->primitive_class.border_unhighlight;
    }
    if (pwc->primitive_class.translations == XtInheritTranslations)
    {
	pwc->primitive_class.translations =
	    swc->primitive_class.translations;
    }
    else if (pwc->primitive_class.translations != NULL)
    {
	pwc->primitive_class.translations =
	    (String)XtParseTranslationTable(pwc->primitive_class.translations);
    }
    if (pwc->primitive_class.arm_and_activate == XmInheritArmAndActivate)
    {
	pwc->primitive_class.arm_and_activate =
	    swc->primitive_class.arm_and_activate;
    }

    if (widget_class != xmPrimitiveWidgetClass)
    {
	XmPrimitiveClassExt *pce = NULL, *sce = NULL;

	pce = _XmGetPrimitiveClassExtPtr(pwc, NULLQUARK);
	sce = _XmGetPrimitiveClassExtPtr(swc, NULLQUARK);

	if (pce && sce && *pce && *sce)
	{
	    if ((*pce)->widget_baseline == XmInheritBaselineProc)
	    {
		(*pce)->widget_baseline = (*sce)->widget_baseline;
	    }
	    if ((*pce)->widget_display_rect == XmInheritDisplayRectProc)
	    {
		(*pce)->widget_display_rect = (*sce)->widget_display_rect;
	    }
	    if ((*pce)->widget_margins == XmInheritMarginsProc)
	    {
		(*pce)->widget_margins = (*sce)->widget_margins;
	    }
	}
    }

    _XmBaseClassPartInitialize(widget_class);
    _XmFastSubclassInit(widget_class, XmPRIMITIVE_BIT);

    /* compile the resources */
    /* Note. This is called once per class/subclass; note that primitive
     * will only pass in here once. */
    if (widget_class == xmPrimitiveWidgetClass)
    {
	_XmSortResourceList((XrmResource **)pwc->core_class.resources,
			    pwc->core_class.num_resources);
    }

    _XmInitializeSyntheticResources(pwc->primitive_class.syn_resources,
				    pwc->primitive_class.num_syn_resources);

    if (widget_class != xmPrimitiveWidgetClass)
    {
	_XmBuildResources(&pwc->primitive_class.syn_resources,
			  &pwc->primitive_class.num_syn_resources,
			  swc->primitive_class.syn_resources,
			  swc->primitive_class.num_syn_resources);
    }
}

static void
CreateHighlightGC(XmPrimitiveWidget pw)
{
    XGCValues values;
    unsigned long mask;

    mask = GCForeground | GCBackground;
    values.foreground = Prim_HighlightColor(pw);
    values.background = XtBackground(pw);

    if (Prim_HighlightPixmap(pw) != None &&
	Prim_HighlightPixmap(pw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed highlight */
	mask |= GCTile | GCFillStyle;

	values.tile = Prim_HighlightPixmap(pw);
	values.fill_style = FillTiled;
    }

    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    Prim_HighlightGC(pw) = XtGetGC((Widget)pw, mask, &values);
}

static void
CreateBottomShadowGC(XmPrimitiveWidget pw)
{
    XGCValues values;
    unsigned long mask;

    mask = GCForeground | GCBackground;
    values.foreground = Prim_BottomShadowColor(pw);
    values.background = XtBackground(pw);

    if (Prim_BottomShadowPixmap(pw) != None &&
	Prim_BottomShadowPixmap(pw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed bottom shadow */
	mask |= GCTile | GCFillStyle;

	values.tile = Prim_BottomShadowPixmap(pw);
	values.fill_style = FillTiled;
    }

    /* these GC's get used for shadow drawing, so set 'em up */
    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    Prim_BottomShadowGC(pw) = XtGetGC((Widget)pw, mask, &values);
}

static void
CreateTopShadowGC(XmPrimitiveWidget pw)
{
    XGCValues values;
    unsigned long mask;

    mask = GCForeground | GCBackground;
    values.foreground = Prim_TopShadowColor(pw);
    values.background = XtBackground(pw);

    if (Prim_TopShadowPixmap(pw) != None &&
	Prim_TopShadowPixmap(pw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed top shadow */
	mask |= GCTile | GCFillStyle;

	values.tile = Prim_TopShadowPixmap(pw);
	values.fill_style = FillTiled;
    }

    /* these GC's get used for shadow drawing, so set 'em up */
    mask |= GCLineWidth | GCLineStyle | GCCapStyle | GCJoinStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    Prim_TopShadowGC(pw) = XtGetGC((Widget)pw, mask, &values);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    XmPrimitiveWidget pw = (XmPrimitiveWidget)new_w;
    XmPrimitiveWidgetClass pwc = (XmPrimitiveWidgetClass)XtClass(new_w);

    /* merge the traversal translations into the regular widget translations */
    if (pwc->primitive_class.translations)
    {
	XtOverrideTranslations(new_w,
			       (XtTranslations)pwc->
					primitive_class.translations);
    }

    Prim_HaveTraversal(new_w) =
	Prim_Highlighted(new_w) =
	Prim_HighlightDrawn(new_w) = False;

    /* validate the entries in XmNnavigationType and XmNunitType
     * Are these correct?  Or do we need to call a defaultProc?
     * 14 Nov 1998: no CallProc: XmPIXELS is correct - Jamie
     */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     pw->primitive.navigation_type, new_w))
    {
	pw->primitive.navigation_type = XmNONE;
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     pw->primitive.navigation_type, new_w))
    {
	pw->primitive.unit_type = XmPIXELS;
    }

    _XmNavigInitialize(request, new_w, args, num_args);

    if (XtWidth(request) == (Dimension)0)
    {
	XtWidth(new_w) = (Prim_HighlightThickness(new_w) * 2
			  + Prim_ShadowThickness(new_w) * 2);
    }
    if (XtHeight(request) == (Dimension)0)
    {
	XtHeight(new_w) = (Prim_HighlightThickness(new_w) * 2
			   + Prim_ShadowThickness(new_w) * 2);
    }

    _XmPrimitiveImportArgs(new_w, args, num_args);

    /*
     * there's a really nasty reason for this.  CoreBackgroundPixmap() is
     * field that appears in *all* Core derivatives -- including Shell.
     * If we try to convert a string to a pixmap in for Shells, we'll have
     * a real nasty mess with infinite recursion.  This is because
     * we want to use XmGetPixmapByDepth(), but that function uses
     * XmGetXmScreen() -- which is a VendorShell derivative.  Now consider
     * the case where we say "*.backgroundPixmap: slant_left", and we create
     * our first AppShell.  Neither the XmScreen nor the XmDisplay has been
     * created yet.  The converter proc will call XmGetPixmapByDepth(), which
     * will call XmGetXmScreen(), which will try to create the XmScreen,
     * which is a VendorShell so the converter proc will be called, which
     * will call XmGetPixmapByDepth(), which ..... recursion.
     */
    if (_XmGetBGPixmapName() != NULL)
    {
	CoreBackgroundPixmap(new_w) =
		XmGetPixmapByDepth(XtScreen(new_w),
					    _XmGetBGPixmapName(),
					    Prim_Foreground(new_w),
					    XtBackground(new_w),
					    CoreDepth(new_w));
	_XmClearBGPixmap();
    }

    /* create all the initial GC's */
    CreateHighlightGC(pw);
    CreateBottomShadowGC(pw);
    CreateTopShadowGC(pw);

    /* Install a Virtual Keys magic event handler on all widgets */
    XtAddEventHandler(new_w, KeyPressMask, False, _XmVirtKeysHandler, NULL);
}

static void
destroy(Widget w)
{
    XtReleaseGC(w, Prim_TopShadowGC(w));
    XtReleaseGC(w, Prim_BottomShadowGC(w));
    XtReleaseGC(w, Prim_HighlightGC(w));

    _XmNavigDestroy(w);
}

static void
XmPrimitiveRealize(Widget w,
	XtValueMask *value_mask,
	XSetWindowAttributes *attributes)
{
    int i;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "XmPrimitive Realize wid before create %d ht %d bw %d\n",
		      XtWidth(w), XtHeight(w), XtBorderWidth(w)));

    /*
     * MLM 970510 - if you get an XError: widget has zero width or height.
     */
    if (XtWidth(w) == 0)
    {
	XtWidth(w) = 1;
    }
    if (XtHeight(w) == 0)
    {
	XtHeight(w) = 1;
    }

    *value_mask |= CWDontPropagate;
    attributes->do_not_propagate_mask =
	(KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask|
	 PointerMotionMask);

    XtCreateWindow(w, (unsigned int)InputOutput,
		   (Visual *)CopyFromParent, *value_mask, attributes);

    /* make sure our menushell children are realized */

    for (i = 0; i < w->core.num_popups; i++)
    {
	XtRealizeWidget(w->core.popup_list[i]);
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "XmPrimitive Realize wid %d ht %d bw %d\n",
		      XtWidth(w), XtHeight(w), XtBorderWidth(w)));
}

static Boolean
XmPrimitiveSetValues(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean need_refresh;

    if (XtWidth(new_w) != XtWidth(old) && XtWidth(new_w) == 0) {
        _XmWarning(new_w, "XmPrimitive SetValues: won't set width of widget to 0\n");
	XtWidth(new_w) = XtWidth(old);
    }
    if (XtHeight(new_w) != XtHeight(old) && XtHeight(new_w) == 0) {
        _XmWarning(new_w, "XmPrimitive SetValues: won't set height of widget to 0\n");
	XtHeight(new_w) = XtHeight(old);
    }

    if (_XmGetBGPixmapName() != NULL && XtIsRealized(new_w))
    {
	XSetWindowAttributes attr;

	CoreBackgroundPixmap(new_w) =
		XmGetPixmapByDepth(XtScreen(new_w),
					    _XmGetBGPixmapName(),
					    Prim_Foreground(new_w),
					    XtBackground(new_w),
					    CoreDepth(new_w));

	attr.background_pixmap = CoreBackgroundPixmap(new_w);
	XChangeWindowAttributes(XtDisplay(new_w), XtWindow(new_w),
				CWBackPixmap, &attr);

	_XmClearBGPixmap();
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     Prim_NavigationType(new_w), new_w))
    {
	Prim_NavigationType(new_w) = Prim_NavigationType(old);
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     Prim_UnitType(new_w), new_w))
    {
	Prim_UnitType(new_w) = Prim_UnitType(old);
    }

    need_refresh = _XmNavigSetValues(old, request, new_w, args, num_args);

    /* Debug print */
    if (Prim_Foreground(old) != Prim_Foreground(new_w))
    {
	DEBUGOUT(_LtDebug(__FILE__, new_w,
			  "Primitive SetValues: Foreground changed\n"));
	need_refresh = True;
    }

    if (Prim_ShadowThickness(old) != Prim_ShadowThickness(new_w)
	|| Prim_HighlightThickness(old) != Prim_HighlightThickness(new_w)
	|| Prim_Foreground(old) != Prim_Foreground(new_w))
    {
	need_refresh = True;
    }

    if (Prim_HighlightPixmap(old) != Prim_HighlightPixmap(new_w) ||
	((Prim_HighlightPixmap(new_w) == None ||
	  Prim_HighlightPixmap(new_w) == XmUNSPECIFIED_PIXMAP) &&
	 Prim_HighlightColor(old) != Prim_HighlightColor(new_w)))
    {
	XtReleaseGC(new_w, Prim_HighlightGC(new_w));

	CreateHighlightGC((XmPrimitiveWidget)new_w);

	need_refresh |=
	    Prim_HighlightDrawn(new_w) && Prim_HighlightThickness(new_w);
    }

    if (Prim_TopShadowPixmap(old) != Prim_TopShadowPixmap(new_w) ||
	((Prim_TopShadowPixmap(new_w) == None ||
	  Prim_TopShadowPixmap(new_w) == XmUNSPECIFIED_PIXMAP) &&
	 Prim_TopShadowColor(old) != Prim_TopShadowColor(new_w)))
    {
	XtReleaseGC(new_w, Prim_TopShadowGC(new_w));

	CreateTopShadowGC((XmPrimitiveWidget)new_w);

	need_refresh |= Prim_ShadowThickness(new_w);
    }

    if (Prim_BottomShadowPixmap(old) != Prim_BottomShadowPixmap(new_w) ||
	((Prim_BottomShadowPixmap(new_w) == None ||
	  Prim_BottomShadowPixmap(new_w) == XmUNSPECIFIED_PIXMAP) &&
	 Prim_BottomShadowColor(old) != Prim_BottomShadowColor(new_w)))
    {
	XtReleaseGC(new_w, Prim_BottomShadowGC(new_w));

	CreateBottomShadowGC((XmPrimitiveWidget)new_w);

	need_refresh |= Prim_ShadowThickness(new_w);
    }

    if ((Prim_HighlightDrawn(new_w) || !XtSensitive(new_w))
	&& _XmGetFocusPolicy(new_w) == XmPOINTER
	&& Prim_HighlightOnEnter(old) == True
	&& Prim_HighlightOnEnter(new_w) == False)
    {
	XmPrimitiveWidgetClass pwc = (XmPrimitiveWidgetClass)XtClass(new_w);

	(*pwc->primitive_class.border_unhighlight) (new_w);
    }

    _XmPrimitiveImportArgs(new_w, args, num_args);

    return need_refresh;
}

/*
 * ENTER/LEAVE should only happen when mwm is in pointer-follows-mouse mode.
 * FOCUS_IN/FOCUS_OUT otherwise.
 */
static void
focus_change(Widget w, XmFocusChange change)
{
    XmPrimitiveWidgetClass pc = (XmPrimitiveWidgetClass)XtClass(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "Primitive Focus Change (%s)\n",
		      (change == XmENTER) ? "XmENTER" :
		      (change == XmFOCUS_IN) ? "XmFOCUS_IN" :
		      (change == XmLEAVE) ? "XmLEAVE" :
		      (change == XmFOCUS_OUT) ? "XmENTER" : "???"));

    switch (change)
    {
    case XmENTER:
	if (!Prim_HighlightOnEnter(w))
	{
	    break;
	}
	if (pc->primitive_class.border_highlight)
	{
	    (pc->primitive_class.border_highlight) (w);
	}
	break;

    case XmFOCUS_IN:
	Prim_HaveTraversal(w) = True;
	if (pc->primitive_class.border_highlight)
	{
	    (pc->primitive_class.border_highlight) (w);
	}
	break;

    case XmLEAVE:
	if (!Prim_HighlightOnEnter(w))
	{
	    break;
	}
	if (pc->primitive_class.border_unhighlight)
	{
	    (pc->primitive_class.border_unhighlight) (w);
	}
	break;

    case XmFOCUS_OUT:
	Prim_HaveTraversal(w) = False;
	if (pc->primitive_class.border_unhighlight)
	{
	    (pc->primitive_class.border_unhighlight) (w);
	}
	break;
    }
}

static XmNavigability
widget_navigable(Widget w)
{
    if (XtSensitive(w) && Prim_TraversalOn(w))
    {
	if (Prim_NavigationType(w) == XmSTICKY_TAB_GROUP ||
	    Prim_NavigationType(w) == XmEXCLUSIVE_TAB_GROUP ||
	    (Prim_NavigationType(w) == XmTAB_GROUP && !_XmShellIsExclusive(w)))
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "PrimitiveWidgetNavigable => XmTAB_NAVIGABLE\n"));

	    return XmTAB_NAVIGABLE;
	}

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "PrimitiveWidgetNavigable => XmCONTROL_NAVIGABLE\n"));

	return XmCONTROL_NAVIGABLE;
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
	   "PrimitiveWidgetNavigable (sens %s TravOn %s) => XmNOT_NAVIGABLE\n",
		      _LtDebugBoolean2String(XtSensitive(w)),
		      _LtDebugBoolean2String(Prim_TraversalOn(w))));

    return XmNOT_NAVIGABLE;
}

static void
primitive_border_unhighlight(Widget w)
{
	if (!XtIsRealized(w)) {
		DEBUGOUT(_LtDebug(__FILE__, w, "PrimitiveBorderUnhighlight\n"));
		return;
	}

	/* with zero width, we don't need this... */
	if (Prim_HighlightThickness(w) == 0) {
		DEBUGOUT(_LtDebug(__FILE__, w, "PrimitiveBorderUnhighlight\n"));
		return;
	}

	if (XmIsManager(XtParent(w))) {
		DEBUGOUT(_LtDebug(__FILE__, w, "PrimitiveBorderUnhighlight xmmgr\n"));
		_XmDrawSimpleHighlight(XtDisplay(w), XtWindow(w), XmParentBackgroundGC(w),
				0, 0, XtWidth(w), XtHeight(w),
				Prim_HighlightThickness(w));
	} else {
		DEBUGOUT(_LtDebug(__FILE__, w, "PrimitiveBorderUnhighlight no xmmgr\n"));
		_XmClearBorder(XtDisplay(w), XtWindow(w),
				0, 0, XtWidth(w), XtHeight(w),
				Prim_HighlightThickness(w));
	}

	Prim_Highlighted(w) = False;
	Prim_HighlightDrawn(w) = False;
}

static void
primitive_border_highlight(Widget w)
{
	if (!XtIsRealized(w)) {
		DEBUGOUT(_LtDebug(__FILE__, w, "PrimitiveBorderHighlight not realized\n"));
		return;
	}

	/* with zero width, we don't need this... */
	if (Prim_HighlightThickness(w) == 0) {
		DEBUGOUT(_LtDebug(__FILE__, w, "PrimitiveBorderHighlight 0 highlight thickness\n"));
		return;
	}

	DEBUGOUT(_LtDebug(__FILE__, w, "PrimitiveBorderHighlight wid %d ht %d hl th %d\n",
				XtWidth(w), XtHeight(w), Prim_HighlightThickness(w)));
	_XmDrawSimpleHighlight(XtDisplay(w), XtWindow(w), Prim_HighlightGC(w),
			0, 0, XtWidth(w), XtHeight(w),
			Prim_HighlightThickness(w));

	Prim_Highlighted(w) = True;
	Prim_HighlightDrawn(w) = True;
}

/* ACTION PROCS */

void
_XmTraverseLeft(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_LEFT);
}

void
_XmTraverseRight(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmProcessTraversal(TRAVERSE_RIGHT)\n"));
    DEBUGOUT(_LtDebug(__FILE__, w,
		       "event type: %d code %08x\n",
		       event->type, event->xkey.keycode));

    XmProcessTraversal(w, XmTRAVERSE_RIGHT);
}

void
_XmTraverseNext(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmProcessTraversal(TRAVERSE_NEXT)\n"));

    XmProcessTraversal(w, XmTRAVERSE_NEXT);
}

void
_XmTraversePrev(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmProcessTraversal(TRAVERSE_PREV)\n"));

    XmProcessTraversal(w, XmTRAVERSE_PREV);
}

void
_XmTraverseDown(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_DOWN);
}

void
_XmTraverseUp(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_UP);
}

void
_XmTraverseHome(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_HOME);
}

void
_XmTraverseNextTabGroup(Widget w, XEvent *event,
			String *params, Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_NEXT_TAB_GROUP);
}

void
_XmTraversePrevTabGroup(Widget w, XEvent *event,
			String *params, Cardinal *num_params)
{
    XmProcessTraversal(w, XmTRAVERSE_PREV_TAB_GROUP);
}

void
_XmPrimitiveParentActivate(Widget w, XEvent *event,
			   String *params, Cardinal *num_params)
{
    XmParentProcessDataRec data;
    Widget m = XtParent(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)m->core.widget_class;

    data.input_action.process_type = XmINPUT_ACTION;
    data.input_action.event = event;
    data.input_action.action = XmPARENT_ACTIVATE;
    data.input_action.params = params;
    data.input_action.num_params = num_params;

    if (XmIsManager(m) && mwc->manager_class.parent_process)
    {
	(*mwc->manager_class.parent_process) (m, &data);
    }
}

void
_XmPrimitiveParentCancel(Widget w, XEvent *event,
			 String *params, Cardinal *num_params)
{
    XmParentProcessDataRec data;
    Widget m = XtParent(w);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)m->core.widget_class;

    data.input_action.process_type = XmINPUT_ACTION;
    data.input_action.event = event;
    data.input_action.action = XmPARENT_CANCEL;
    data.input_action.params = params;
    data.input_action.num_params = num_params;

    if (XmIsManager(m) && mwc->manager_class.parent_process)
    {
	(*mwc->manager_class.parent_process) (m, &data);
    }
}

void
_XmPrimitiveHelp(Widget w, XEvent *event,
		 String *params, Cardinal *num_params)
{
    Widget cur = w;
    XmAnyCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPrimitiveHelp\n"));

    cbs.reason = XmCR_HELP;
    cbs.event = event;

    while (cur != NULL)
    {
	if (XtHasCallbacks(cur, XmNhelpCallback) == XtCallbackHasSome)
	{
	    DEBUGOUT(_LtDebug(__FILE__, cur,
			      "_XmPrimitiveHelp ... calling !!\n"));

	    XtCallCallbacks(cur, XmNhelpCallback, (XtPointer)&cbs);

	    return;
	}

	DEBUGOUT(_LtDebug(__FILE__, cur, "_XmPrimitiveHelp ... no luck\n"));
	cur = XtParent(cur);
    }
}

Boolean
_XmDifferentBackground(Widget w, Widget parent)
{
    if (!w || !parent)
    {
	return True;
    }
    if (XmIsPrimitive(w) && XmIsManager(parent) &&
	(XtBackground(w) != XtBackground(parent)))
    {
	return True;
    }
    else
    {
	return False;
    }
}

void
_XmPrimitiveHighlightPixmapDefault(Widget w, int offset, XrmValue *value)
{
    static Pixmap pixmap;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPrimitiveHighlightPixmapDefault\n"));

    pixmap = XmUNSPECIFIED_PIXMAP;

    value->addr = (char *) &pixmap;
    value->size = sizeof (Pixmap);

    if (Prim_HighlightColor(w) == XtBackground(w))
    {
	pixmap = XmGetPixmapByDepth(XtScreen(w), "50_foreground",
				    Prim_HighlightColor(w),
				    Prim_Foreground(w),
				    CoreDepth(w));
    }
}

void
_XmPrimitiveTopShadowPixmapDefault(Widget w, int offset, XrmValue *value)
{
    static Pixmap pixmap;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPrimitiveTopShadowPixmapDefault\n"));

    pixmap = XmUNSPECIFIED_PIXMAP;

    value->addr = (char *) &pixmap;
    value->size = sizeof (Pixmap);

    if (Prim_TopShadowColor(w) == XtBackground(w))
    {
	pixmap = XmGetPixmapByDepth(XtScreen(w), "50_foreground",
				    Prim_TopShadowColor(w),
				    Prim_Foreground(w),
				    CoreDepth(w));
    }
    else if (DefaultDepthOfScreen(XtScreen(w)) == 1)
    {
	pixmap = XmGetPixmapByDepth(XtScreen(w), "50_foreground",
				    Prim_TopShadowColor(w),
				    XtBackground(w),
				    CoreDepth(w));
    }
}

/* Public Functions */

/* An implementation for the file `lib/Xm/Primitive.c'
 * Peter Andrew Pilgrim
 * Sun Sep 21 21:30:51 BST 1997
 * `mailto:peter.pilgrim@it.newsint.co.uk'
 * Or perhaps search the USENET archive under `help-gnu-emacs'
 *
 * The functions XmResolvePartOffsets and XmResolveAllPartOffsets were designed
 * to be used by widgets subclassing OSF/Motif widgets. Therefore, LessTif must
 * provide, but not (necessarily) use them.
 *
 * The big idea is that *binary* compatibility can be obtained for these
 * subclasses, even if the Motif (LessTif) revisions add fields to the
 * instance parts of the widgets.
 *
 * The functions below are used together with a macro called XmField which is
 * in LESSTIF/include/Xm/XmP.h .
 *
 * All this is described very thoroughly in an article called "Achieving Binary
 * Compatibility using the XmResolvePartOffset API" by Daniel DarDailler, who
 * has been in the Motif team at OSF for as long as there was a Motif team at
 * OSF :-)
 *
 * Look for the article at http://www.x.org/people/daniel/xmresolv
 */
void
XmResolvePartOffsets( WidgetClass w_class, XmOffsetPtr *offset_return )
{
    /* A convenience function */
    XmResolveAllPartOffsets( w_class, offset_return, NULL );
}

void
XmResolveAllPartOffsets( WidgetClass w_class,
			 XmOffsetPtr *offset_return,
			 XmOffsetPtr *constraint_offset_return )
{
    /* Implementation thankfully came from careful reading of the description at
     * `http://vetrec5.mit.edu/cgi-bin/in...r-main.cgi/XmResolveAllPartOffsets'
     * I managed to find this with AltaVista by just searching for
     * XmResolveAllPartOffsets
     */
    Boolean	 bfIsConstraint;
    int		 array_size, j, k;
    Cardinal	 widget_part_size, constraint_part_size;
    XmOffsetPtr	 offset=0, constraint_offset=0;
    XmPartResource *		resources;
    Cardinal			num_resources;
    ConstraintWidgetClass	superclass, thisclass, tmp;

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "\nXmResolveAllPartOffsets(`%s')\n",
		       w_class->core_class.class_name));

    /* Say all the complication, let's do all the complex stuff in terms
     * of the ConstraintWidgetClass. This will save us from all this
     * casting.
     */
    thisclass = (ConstraintWidgetClass)w_class;

    /* The widget class must have `XtVersionDontCheck' instead of XtVersion
     * in its class record.
     */
    if (thisclass->core_class.version != XtVersionDontCheck)
    {
	return;
    }

    /*
     * Add the super class widget size, which has already been
     * initialized (strike defined) to the widget part size.
     */
    superclass = (ConstraintWidgetClass)thisclass->core_class.superclass;
    widget_part_size = thisclass->core_class.widget_size;
    if (superclass)
    {
	thisclass->core_class.widget_size += superclass->core_class.widget_size;
    }

    /*
     * Check if this widget class is __derived__ from the
     * `ConstraintWidgetClass'.
     */
    tmp = thisclass;
    while ( tmp && (WidgetClass)tmp != constraintWidgetClass )
    {
	tmp = (ConstraintWidgetClass)tmp->core_class.superclass;
    }

    if ( tmp == (ConstraintWidgetClass)constraintWidgetClass )
    {
	bfIsConstraint = True;
    }
    else
    {
	bfIsConstraint = False;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "\twidget class:`%s' is %s a Constraint type.\n",
		       thisclass->core_class.class_name,
		       (bfIsConstraint ? "" : "NOT" )));

    if (bfIsConstraint)
    {
	/*
	 * If the widget is a subclass of constraint class adds the superclass
	 * constraint size field to the constraint size field.
	 */
	constraint_part_size = thisclass->constraint_class.constraint_size;
	if (superclass)
	{
	    thisclass->constraint_class.constraint_size +=
		superclass->constraint_class.constraint_size;
	}
    }

    /*
     * Now count the number of super classes, by traversing up the
     * class hierarchy.
     */
    array_size = 0;
    for ( tmp = thisclass;
	  tmp;
	  tmp = (ConstraintWidgetClass)tmp->core_class.superclass )
    {
	array_size++;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "\tarray size:%d\n", array_size));

    /*
     * Allocate an array based upon the number of superclass
     * for the core offset record.
     */
    offset = (XmOffsetPtr)XtCalloc( array_size, sizeof(XmOffset) );

    if (bfIsConstraint)
    {
	/*
	 * If the widget is a subclass of the Constraint widget class
	 * allocate an array for the constraint offset record.
	 */
	constraint_offset =
	    (XmOffsetPtr)XtCalloc( array_size, sizeof(XmOffset) );
    }

    if (offset_return)
    {
	*offset_return = offset;
    }
    if (constraint_offset_return)
    {
	*constraint_offset_return = constraint_offset;
    }

    /*
     * Fill in the offsets of all the widget parts and constraint parts
     * with the appropriate values, determined by examining the size
     * fields of all the super class records.
     *
     * Get the widget size of the next super class to calculate the offset
     * of this widget class part. i.e.
     *
     * `offset[XmLabelIndex] = xmPrimitiveClass->core_class.widget_size'
     */


    DEBUGOUT(_LtDebug0(__FILE__, NULL, "\n\t--- Calculate Core offsets ---\n"));

    for ( tmp = (ConstraintWidgetClass)thisclass->core_class.superclass,
	      k = array_size - 1;
	  tmp;
	  tmp = (ConstraintWidgetClass)tmp->core_class.superclass, k-- )
    {
	offset[k] = tmp->core_class.widget_size;

	DEBUGOUT(_LtDebug0(__FILE__, NULL, "\t  offset[%2d] = %-6d `%s'\n",
			   k, offset[k], tmp->core_class.class_name));
    }

    if (bfIsConstraint)
    {
	/*
	 * Fill the Constraint offset record in the same way as the
	 * core resources.
	 *
	 * WARNING: This code will fail if you take short cuts in
	 * declaring constraint records. Some widget class writers
	 * will miss out a range of classes. If the widget class
	 * is not a directly subclass of XmManager. This an error
	 * in widget design!!!
	 * Peter Pilgrim Mon Sep 22 01:08:09 BST 1997
	 *
	 * `offset[XmFormIndex] =
	 *    xmBulletinBoardClass->constraint_class.constraint_size'
	 */

	DEBUGOUT(_LtDebug0(__FILE__, NULL,
			   "\n\t--- Calculate Constraint offsets ---\n"));

	for ( tmp = (ConstraintWidgetClass)thisclass->core_class.superclass,
		  k = array_size - 1;
	      tmp && (WidgetClass)tmp != constraintWidgetClass ;
	      tmp = (ConstraintWidgetClass)tmp->core_class.superclass, k-- )
	{
	    constraint_offset[k] = tmp->constraint_class.constraint_size;

	    DEBUGOUT(_LtDebug0(__FILE__, NULL, "\t  offset[%2d] = %-6d `%s'\n",
			       k, constraint_offset[k],
			       tmp->core_class.class_name));
	}
    }

    /*
     * Use the part offsets array to modify the entries in the resource
     * list to be real offsets.
     */

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "\n\t--- Core Final ---\n"));

    resources = (XmPartResource*)thisclass->core_class.resources;
    num_resources = thisclass->core_class.num_resources;
    for (j = 0; j < num_resources; j++)
    {
	int	widget_index = resources[j].resource_offset >> XmOFFSETBITS;
	int	true_offset =  resources[j].resource_offset &  XmOFFSETMASK;
	resources[j].resource_offset = offset[widget_index] + true_offset;

	DEBUGOUT(_LtDebug0(__FILE__, NULL,
			   "\t  [%2d] windex:%-3d true_offset:%-3d"
			   " real_offset: %-3d\n",
			   j, widget_index, true_offset,
			   resources[j].resource_offset));
    }

    if (bfIsConstraint)
    {
	/*
	 * Use the part offsets array to modify the entries in the
	 * constraint resource list to be real offsets.
	 */

	DEBUGOUT(_LtDebug0(__FILE__, NULL, "\n\t--- Constraint Final ---\n"));

	resources = (XmPartResource*)thisclass->constraint_class.resources;
	num_resources = thisclass->constraint_class.num_resources;
	for (j = 0; j < num_resources; j++)
	{
	    int	widget_index =
		resources[j].resource_offset >> XmOFFSETBITS;
	    int	true_offset =
		resources[j].resource_offset &  XmOFFSETMASK;
	    resources[j].resource_offset = constraint_offset[widget_index] +
						true_offset;

	    DEBUGOUT(_LtDebug0(__FILE__, NULL,
			       "\t  [%2d] windex:%-3d true_offset:%-3d "
			       "real_offset: %-3d\n",
			       j, widget_index, true_offset,
			       resources[j].resource_offset));
	}
    }


    if ( !constraint_offset_return && constraint_offset )
    {
	/* Release constraint offset that were allocated,
	 * silly caller supplied a NULL referenced parameter
	 */
	XtFree((char *)constraint_offset);
    }
    if ( !offset_return && offset )
    {
	/* Release offsets that were allocated,
	 * silly caller supplied a NULL referenced parameter
	 */
	XtFree((char *)offset);
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
		       "END OF XmResolveAllPartOffsets(`%s')\n\n",
		       w_class->core_class.class_name));
}
/*
 * the baseline and display rect extension methods
 */
Boolean
XmWidgetGetBaselines(Widget w, Dimension **baselines, int *line_count)
{
    if (XmIsLabel(w))
    {
	XmPrimitiveClassExt *extptr;

	extptr = _XmGetPrimitiveClassExtPtr(XtClass(w), NULLQUARK);

	if (extptr && *extptr && (*extptr)->widget_baseline)
	{
	    return ((*extptr)->widget_baseline) (w, baselines, line_count);
	}
    }
    else if (XmIsLabelGadget(w))
    {
	XmGadgetClassExt *extptr;

	extptr = _XmGetGadgetClassExtPtr(XtClass(w), NULLQUARK);

	if (extptr && *extptr && (*extptr)->widget_baseline)
	{
	    return ((*extptr)->widget_baseline) (w, baselines, line_count);
	}
    }

    return False;
}

Boolean
XmWidgetGetDisplayRect(Widget w, XRectangle *displayrect)
{
    if (XmIsLabel(w))
    {
	XmPrimitiveClassExt *extptr;

	extptr = _XmGetPrimitiveClassExtPtr(XtClass(w), NULLQUARK);

	if (extptr && *extptr && (*extptr)->widget_display_rect)
	{
	    return ((*extptr)->widget_display_rect) (w, displayrect);
	}
    }
    else if (XmIsLabelGadget(w))
    {
	XmGadgetClassExt *extptr;

	extptr = _XmGetGadgetClassExtPtr(XtClass(w), NULLQUARK);

	if (extptr && *extptr && (*extptr)->widget_display_rect)
	{
	    return ((*extptr)->widget_display_rect) (w, displayrect);
	}
    }

    return False;
}

static void
_XmPrimitiveExportX(Widget widget, int offset, XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    if (XtIsManaged(widget) && XtParent(widget) && XmIsVendorShell(XtParent(widget)))
    {
    	*value = XtX(XtParent(widget));
    }
    converted_value = XmConvertUnits(widget,
				     XmHORIZONTAL,
				     XmPIXELS,
				     *value,
				     unitType);

    *value = converted_value;
}

static void
_XmPrimitiveExportY(Widget widget, int offset, XtArgVal *value)
{
	unsigned char unitType = _XmGetUnitType(widget);
	int converted_value;

	if (XtIsManaged(widget) && XtParent(widget) && XmIsVendorShell(XtParent(widget))) {
		*value = XtY(XtParent(widget));
	}
	converted_value = XmConvertUnits(widget, XmVERTICAL, XmPIXELS, *value, unitType);

	*value = converted_value;
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
_PrimitiveTrait_Redraw(Widget child, Widget newp, Widget curp, Mask flag)
{
	XtExposeProc	e;
	Boolean		r = False;

	e = ((XmPrimitiveClassRec *)XtClass(child))->core_class.expose;

	DEBUGOUT(_LtDebug2(__FILE__, newp, child, "Primitive CareParentVisualRedraw()\n"));

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
