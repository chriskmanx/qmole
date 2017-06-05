/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Manager.c,v 1.6 2007/09/12 20:29:35 jwrdegoede Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Manager.c,v 1.6 2007/09/12 20:29:35 jwrdegoede Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <XmI/XmI.h>
#include <XmI/TraversalI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/ManagerP.h>
#include <Xm/GadgetP.h>
#include <Xm/RepType.h>
#include <Xm/TransltnsP.h>
#include <X11/keysym.h>
#include <Xm/VirtKeysP.h>

/* For determining class type */
#include <Xm/CascadeBP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/MenuUtilP.h>

/*
 * Yes I know this is crazy... Danny
 */
#include <Xm/RowColumnP.h>
#include <Xm/DialogS.h>

#include <XmI/ManagerI.h>
/* 
 * Traits  
 */
#include <Xm/TraitP.h>
#include <Xm/LayoutT.h>
#include <XmI/DirectionI.h>
#include <Xm/CareVisualT.h>

#include <XmI/DebugUtil.h>

static XmDirection get_direction(Widget w);

static XmSpecifyLayoutDirectionTraitRec _XmManagerLayoutTraitRec =
{
    0,				/* version */
    get_direction		/* getValue */
};


/* Forward Declarations */
static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void constraint_initialize(Widget request, Widget new_w,
				  ArgList args, Cardinal *num_args);
static XmNavigability widget_navigable(Widget w);
void _XmManagerInstallAccelerator(Widget m, Widget w, String s);
static void _XmManagerExportX(Widget w, int offset, XtArgVal *value);
static void _XmManagerExportY(Widget w, int offset, XtArgVal *value);

/* action routine prototypes */
void _XmGadgetButtonMotion(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);
void _XmGadgetArm(Widget w, XEvent *event,
		  String *params, Cardinal *num_params);
void _XmGadgetActivate(Widget w, XEvent *event,
		       String *params, Cardinal *num_params);
void _XmGadgetMultiArm(Widget w, XEvent *event,
		       String *params, Cardinal *num_params);
void _XmGadgetMultiActivate(Widget w, XEvent *event,
			    String *params, Cardinal *num_params);
void _XmGadgetDrag(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);
void _XmGadgetSelect(Widget w, XEvent *event,
		     String *params, Cardinal *num_params);
void _XmManagerParentActivate(Widget w, XEvent *event,
			      String *params, Cardinal *num_params);
void _XmManagerParentCancel(Widget w, XEvent *event,
			    String *params, Cardinal *num_params);
void _XmGadgetTraversePrevTabGroup(Widget w, XEvent *event,
				   String *params, Cardinal *num_params);
void _XmGadgetTraverseNextTabGroup(Widget w, XEvent *event,
				   String *params, Cardinal *num_params);
void _XmGadgetTraversePrev(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);
void _XmGadgetTraverseNext(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);
void _XmGadgetTraverseUp(Widget w, XEvent *event,
			 String *params, Cardinal *num_params);
void _XmGadgetTraverseDown(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);
void _XmGadgetTraverseLeft(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);
void _XmGadgetTraverseRight(Widget w, XEvent *event,
			    String *params, Cardinal *num_params);
void _XmGadgetTraverseHome(Widget w, XEvent *event,
			   String *params, Cardinal *num_params);
void _XmGadgetHelp(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);
void _XmGadgetKeyInput(Widget w, XEvent *event,
		       String *params, Cardinal *num_params);
void _XmGadgetHelp(Widget w, XEvent *event,
		   String *params, Cardinal *num_params);


/*
 * the event handler to handle pointer position related events for gadget
 * children
 */
static void _XmManagerEventHandler(Widget w, XtPointer data,
				   XEvent *event, Boolean *cont);

static XtActionsRec actions[] =
{
    {"ManagerEnter", _XmManagerEnter},
    {"ManagerLeave", _XmManagerLeave},
    {"ManagerFocusIn", _XmManagerFocusIn},
    {"ManagerFocusOut", _XmManagerFocusOut},
    {"ManagerGadgetPrevTabGroup", _XmGadgetTraversePrevTabGroup},
    {"ManagerGadgetNextTabGroup", _XmGadgetTraverseNextTabGroup},
    {"ManagerGadgetTraversePrev", _XmGadgetTraversePrev},
    {"ManagerGadgetTraverseNext", _XmGadgetTraverseNext},
    {"ManagerGadgetTraverseLeft", _XmGadgetTraverseLeft},
    {"ManagerGadgetTraverseRight", _XmGadgetTraverseRight},
    {"ManagerGadgetTraverseUp", _XmGadgetTraverseUp},
    {"ManagerGadgetTraverseDown", _XmGadgetTraverseDown},
    {"ManagerGadgetTraverseHome", _XmGadgetTraverseHome},
    {"ManagerGadgetSelect", _XmGadgetSelect},
    {"ManagerParentActivate", _XmManagerParentActivate},
    {"ManagerParentCancel", _XmManagerParentCancel},
    {"ManagerGadgetButtonMotion", _XmGadgetButtonMotion},
    {"ManagerGadgetKeyInput", _XmGadgetKeyInput},
    {"ManagerGadgetHelp", _XmGadgetHelp},
    {"ManagerGadgetArm", _XmGadgetArm},
    {"ManagerGadgetDrag", _XmGadgetDrag},
    {"ManagerGadgetActivate", _XmGadgetActivate},
    {"ManagerGadgetMultiArm", _XmGadgetMultiArm},
    {"ManagerGadgetMultiActivate", _XmGadgetMultiActivate},
    {"Enter", _XmManagerEnter},
    {"FocusIn", _XmManagerFocusIn},
    {"Help", _XmGadgetHelp},
    {"Arm", _XmGadgetArm},
    {"Activate", _XmGadgetActivate},

    {"ManagerGadgetMultiArm", _XmGadgetMultiArm},
};

#define Offset(field) XtOffsetOf(XmManagerRec, manager.field)

/* Resources for the manager class */
static XtResource resources[] =
{
    {
	XmNunitType, XmCUnitType, XmRUnitType,
	sizeof(unsigned char), Offset(unit_type),
	XmRCallProc, (XtPointer)_XmUnitTypeDefault
    },
    {
	XmNx, XmCPosition, XmRHorizontalPosition,
	sizeof(Position), XtOffsetOf(XmManagerRec, core.x),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNy, XmCPosition, XmRVerticalPosition,
	sizeof(Position), XtOffsetOf(XmManagerRec, core.y),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNwidth, XmCDimension, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmManagerRec, core.width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNheight, XmCDimension, XmRVerticalDimension,
	sizeof(Dimension), XtOffsetOf(XmManagerRec, core.height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNborderWidth, XmCBorderWidth, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmManagerRec, core.border_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNforeground, XmCForeground, XmRPixel,
	sizeof(Pixel), Offset(foreground),
	XmRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {
	XmNbackground, XmCBackground, XmRPixel,
	sizeof(Pixel), XtOffsetOf(XmManagerRec, core.background_pixel),
	XmRCallProc, (XtPointer)_XmBackgroundColorDefault
    },
    {
	XmNbackgroundPixmap, XmCPixmap, XmRXmBackgroundPixmap,
	sizeof(Pixmap), XtOffsetOf(XmManagerRec, core.background_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNhighlightColor, XmCHighlightColor, XmRPixel,
	sizeof(Pixel), Offset(highlight_color),
	XmRCallProc, (XtPointer)_XmHighlightColorDefault
    },
    {
	XmNhighlightPixmap, XmCHighlightPixmap, XmRManHighlightPixmap,
	sizeof(Pixmap), Offset(highlight_pixmap),
	XmRCallProc, (XtPointer)_XmManagerHighlightPixmapDefault
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), Offset(navigation_type),
	XmRImmediate, (XtPointer)XmTAB_GROUP
    },
    {
	XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNtopShadowColor, XmCTopShadowColor, XmRPixel,
	sizeof(Pixel), Offset(top_shadow_color),
	XmRCallProc, (XtPointer)_XmTopShadowColorDefault
    },
    {
	XmNtopShadowPixmap, XmCTopShadowPixmap, XmRManTopShadowPixmap,
	sizeof(Pixmap), Offset(top_shadow_pixmap),
	XmRCallProc, (XtPointer)_XmManagerTopShadowPixmapDefault
    },
    {
	XmNbottomShadowColor, XmCBottomShadowColor, XmRPixel,
	sizeof(Pixel), Offset(bottom_shadow_color),
	XmRCallProc, (XtPointer)_XmBottomShadowColorDefault
    },
    {
	XmNbottomShadowPixmap, XmCBottomShadowPixmap, XmRManBottomShadowPixmap,
	sizeof(Pixmap), Offset(bottom_shadow_pixmap),
	XtRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNhelpCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(help_callback),
	XtRPointer, (XtPointer)NULL
    },
    {
	XmNuserData, XmCUserData, XmRPointer,
	sizeof(XtPointer), Offset(user_data),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), Offset(traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNstringDirection, XmCStringDirection, XmRStringDirection,
	sizeof(XmStringDirection), Offset(string_direction),
	XmRImmediate, (XtPointer)((XmStringDirection)XmUNSPECIFIED)
    },
    {
	XmNinitialFocus, XmCInitialFocus, XmRWidget,
	sizeof(Widget), Offset(initial_focus),
	XmRImmediate, (XtPointer)NULL
    }
    ,
    {
	XmNpopupHandlerCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(popup_handler_callback),
	XtRPointer, (XtPointer)NULL
    }
    ,
    {
	XmNlayoutDirection, XmCLayoutDirection, XmRDirection,
	sizeof(XmDirection), Offset(string_direction),
	XmRImmediate, (XtPointer)((XmDirection) NULL)	/* dynamic how? */
    }
};

#define MOffset(field)	XtOffset(XmManagerWidget, manager.field)
#define COffset(field)	XtOffset(XmManagerWidget, core.field)
static XmSyntheticResource syn_resources[] =
{
    /* core part */
    {
	XmNx,
	sizeof(Position), COffset(x),
	_XmManagerExportX, _XmToHorizontalPixels
    },
    {
	XmNy,
	sizeof(Position),
	COffset(y),
	_XmManagerExportY, _XmToVerticalPixels
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
    /* manager */
    {
	XmNshadowThickness,
	sizeof(Dimension), MOffset(shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNstringDirection,
	sizeof(XmStringDirection), MOffset(string_direction),
	_XmFromLayoutDirection, _XmToLayoutDirection
    },
};

static XmBaseClassExtRec _XmManagerCoreClassExtRec = {
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

static CompositeClassExtensionRec managerCompositeExt = 
{
    /* next_extension */  NULL,
    /* record_type    */  NULLQUARK,
    /* version        */  XtCompositeExtensionVersion,
    /* record_size    */  sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ False
#endif
};

static XmManagerClassExtRec _XmManagerClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIX ME */
};

XmManagerClassRec xmManagerClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &constraintClassRec,
        /* class_name            */ "XmManager",
	/* widget_size           */ sizeof(XmManagerRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal /*XtExposeCompressMultiple*/,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ _XmManagerGetValuesHook,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmManager_defaultTranslations,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL /*XtInheritDisplayAccelerator*/,
	/* extension             */ (XtPointer)&_XmManagerCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ NULL,
        /* change_managed   */ NULL,
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ (XtPointer)&managerCompositeExt,
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,
        /* subresource_count */ 0,
        /* constraint_size   */ 0,
        /* initialize        */ constraint_initialize,
        /* destroy           */ NULL,
        /* set_values        */ NULL,
        /* extension         */ NULL,
    },
    /* XmManager class part */
    {
	/* translations                 */ _XmManager_managerTraversalTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ _XmParentProcess,
	/* extension                    */ (XtPointer)&_XmManagerClassExtRec
    },
};




WidgetClass xmManagerWidgetClass = (WidgetClass)&xmManagerClassRec;

static void
class_initialize(void)
{
    _XmRegisterConverters();
    _XmRegisterPixmapConverters();
    _XmInitializeExtensions();
    _XmManagerCoreClassExtRec.record_type = XmQmotif;
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)widget_class;
    XmManagerWidgetClass swc = (XmManagerWidgetClass)widget_class->core_class.superclass;
    CompositeClassExtension ext, *extptr;

    if (mwc->manager_class.translations == XtInheritTranslations)
    {
	mwc->manager_class.translations =
	    swc->manager_class.translations;
    }
    else if (mwc->manager_class.translations != NULL)
    {
	mwc->manager_class.translations =
	    (String)XtParseTranslationTable(mwc->manager_class.translations);
    }
    if (mwc->manager_class.parent_process == XmInheritParentProcess)
    {
	mwc->manager_class.parent_process =
	    swc->manager_class.parent_process;
    }
    extptr = (CompositeClassExtension *)_XmGetClassExtensionPtr(
			(XmGenericClassExt *)&(mwc->composite_class.extension),
								   NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = mwc->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = False;
#endif
	    mwc->composite_class.extension = (XtPointer)ext;
	}
    }
    /*
       else
       {
       (*extptr)->accepts_objects = True;
       }
     */


    if (widget_class != xmManagerWidgetClass)
    {
	XmManagerClassExt *mce = NULL, *sce = NULL;

	mce = _XmGetManagerClassExtPtr(mwc, NULLQUARK);
	sce = _XmGetManagerClassExtPtr(swc, NULLQUARK);

	if (mce && !*mce && sce && *sce)
	{
	    *mce =
		(XmManagerClassExtRec *)XtCalloc(1, sizeof(XmManagerClassExtRec));
	    (*mce)->next_extension = NULL;
	    (*mce)->record_type = NULLQUARK;
	    (*mce)->version = XmManagerClassExtVersion;
	    (*mce)->record_size = sizeof(XmManagerClassExtRec);
	    (*mce)->traversal_children = (*sce)->traversal_children;
	}
	else if (mce && sce && *mce && *sce)
	{
	    if ((*mce)->traversal_children == XmInheritTraversalChildrenProc)
	    {
		(*mce)->traversal_children =
		    (*sce)->traversal_children;
	    }
	}
    }

    _XmBaseClassPartInitialize(widget_class);
    _XmFastSubclassInit(widget_class, XmMANAGER_BIT);

    /* compile the resources */
    if (widget_class == xmManagerWidgetClass)
    {
	_XmSortResourceList((XrmResource **)mwc->core_class.resources,
			    mwc->core_class.num_resources);
    }

    _XmInitializeSyntheticResources(mwc->manager_class.syn_resources,
				    mwc->manager_class.num_syn_resources);
    _XmInitializeSyntheticResources(mwc->manager_class.syn_constraint_resources,
			      mwc->manager_class.num_syn_constraint_resources);

    if (widget_class != xmManagerWidgetClass)
    {
	XmManagerWidgetClass msc =
	(XmManagerWidgetClass)mwc->core_class.superclass;

	_XmBuildResources(&mwc->manager_class.syn_resources,
			  &mwc->manager_class.num_syn_resources,
			  msc->manager_class.syn_resources,
			  msc->manager_class.num_syn_resources);

	_XmBuildResources(&mwc->manager_class.syn_constraint_resources,
			  &mwc->manager_class.num_syn_constraint_resources,
			  msc->manager_class.syn_constraint_resources,
			  msc->manager_class.num_syn_constraint_resources);
    }

    /* FIX ME: do I need to worry about subclasses */
    /* initialize traits */
    if (!XmeTraitSet((XtPointer)xmManagerWidgetClass, XmQTspecifyLayoutDirection,
		     (XtPointer)&_XmManagerLayoutTraitRec))
    {
	_XmWarning(NULL, "XmManager ClassInitialize: XmeTraitSet failed\n");
    }
}

static void
CreateHighlightGC(Widget mw)
{
    XGCValues values;
    unsigned long mask,dynamic,unused;

    mask = GCForeground | GCBackground;
    values.foreground = MGR_HighlightColor(mw);
    values.background = XtBackground(mw);

    if (MGR_HighlightPixmap(mw) != None
	&& MGR_HighlightPixmap(mw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed highlight */
	mask |= GCTile | GCFillStyle;

	values.tile = MGR_HighlightPixmap(mw);
	values.fill_style = FillTiled;
    }

    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    /* these may be changed in _XmDrawHighlight() called from
     * gadget_border_highlight() */
    dynamic = GCLineWidth | GCLineStyle | GCJoinStyle | GCDashList;

    /* have no idea ! */
    unused = 0;

    MGR_HighlightGC(mw) = XtGetGC((Widget)mw, mask, &values);

}

static void
CreateBottomShadowGC(Widget mw)
{
    XGCValues values;
    unsigned long mask;

    mask = GCForeground | GCBackground;
    values.foreground = MGR_BottomShadowColor(mw);
    values.background = XtBackground(mw);

    if (MGR_BottomShadowPixmap(mw) != None
	&& MGR_BottomShadowPixmap(mw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed bottom shadow */
	mask |= GCTile | GCFillStyle;

	values.tile = MGR_BottomShadowPixmap(mw);
	values.fill_style = FillTiled;
    }

    mask |= GCLineWidth;
    values.line_width = 1;

    MGR_BottomShadowGC(mw) = XtGetGC((Widget)mw, mask, &values);
}

static void
CreateTopShadowGC(Widget mw)
{
    XGCValues values;
    unsigned long mask;

    mask = GCForeground | GCBackground;
    values.foreground = MGR_TopShadowColor(mw);
    values.background = XtBackground(mw);

    if (MGR_TopShadowPixmap(mw) != None
	&& MGR_TopShadowPixmap(mw) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed top shadow */
	mask |= GCTile | GCFillStyle;

	values.tile = MGR_TopShadowPixmap(mw);
	values.fill_style = FillTiled;
    }

    /* these GC's get used for shadow drawing, so set 'em up */
    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    MGR_TopShadowGC(mw) = XtGetGC((Widget)mw, mask, &values);
}

static void
CreateBackgroundGC(Widget mw)
{
    XGCValues values;
    unsigned long mask;

    mask = GCForeground | GCBackground;

    values.foreground = XtBackground(mw);
    values.background = XtBackground(mw);

    /* these GC's get used for shadow drawing, so set 'em up */
    mask |= GCLineWidth | GCLineStyle | GCJoinStyle | GCCapStyle;
    values.line_width = 1;
    values.line_style = LineSolid;
    values.join_style = JoinMiter;
    values.cap_style = CapButt;

    MGR_BackgroundGC(mw) = XtGetGC((Widget)mw, mask, &values);
}

static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)new_w->core.widget_class;
    /*XmManagerWidget   mw = (XmManagerWidget)new_w; */

    MGR_EventHandlerAdded(new_w) = False;
    MGR_HighlightedWidget(new_w) = NULL;
    MGR_SelectedGadget(new_w) = NULL;
    MGR_ActiveChild(new_w) = NULL;
    MGR_KeyboardList(new_w) = NULL;
    MGR_NumKeyboardEntries(new_w) = 0;
    MGR_SizeKeyboardList(new_w) = 0;

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     MGR_UnitType(new_w),
			     new_w))
    {
	MGR_UnitType(new_w) = XmPIXELS;
    }


    /* initialize these to values that aren't possible */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     MGR_NavigationType(new_w),
			     new_w))
    {
	MGR_UnitType(new_w) = XmTAB_GROUP;
    }

    /* This is way too simple. FIX ME */
    if (MGR_StringDirection(new_w) == (XmStringDirection)XmUNSPECIFIED)
    {
	if (XmIsManager(XtParent(new_w)))
	{
	    MGR_StringDirection(new_w) = MGR_StringDirection(XtParent(new_w));
	}
	else
	{
	    MGR_StringDirection(new_w) = XmSTRING_DIRECTION_L_TO_R;
	}
    }

    _XmNavigInitialize(request, new_w, args, num_args);

    _XmManagerImportArgs(new_w, args, num_args);

    if (mwc->manager_class.translations)
    {
	XtOverrideTranslations(new_w,
			       (XtTranslations)mwc->
			       manager_class.translations);
    }

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
			       MGR_Foreground(new_w),
			       XtBackground(new_w),
			       CoreDepth(new_w));
	_XmClearBGPixmap();
    }

    CreateTopShadowGC(new_w);
    CreateBottomShadowGC(new_w);
    CreateHighlightGC(new_w);
    CreateBackgroundGC(new_w);

    /* Install a Virtual Keys magic event handler on all widgets */
    XtAddEventHandler(new_w, KeyPressMask, False, _XmVirtKeysHandler, NULL);
}

static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug("RWS", w, "%s:destroy(%d)\n",
		      __FILE__, __LINE__
	     ));
    XtReleaseGC(w, MGR_TopShadowGC(w));
    XtReleaseGC(w, MGR_BottomShadowGC(w));
    XtReleaseGC(w, MGR_HighlightGC(w));
    XtReleaseGC(w, MGR_BackgroundGC(w));
    if (MGR_KeyboardList(w))
    {
        XtFree((char *)MGR_KeyboardList(w));
    }

    if (MGR_EventHandlerAdded(w))
    {
	XtRemoveEventHandler(w,
			     PointerMotionMask,
			     False,
			     _XmManagerEventHandler,
			     NULL);
    }
    _XmNavigDestroy(w);
}

static void
realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
    /* rws 13 Nov 1998
       This causes a parse error with gcc on AIX???
       Boolean wz = False, hz = False; */

    DEBUGOUT(_LtDebug(__FILE__, w, "XmManager realize() - %dx%d\n",
		      XtWidth(w), XtHeight(w)));

    /*
     * MLM 960524 - if you get an XError: widget has zero width or height.
     */
    if (XtWidth(w) == 0)
    {
	/* wz = True; */
	XtWidth(w) = 1;
    }
    if (XtHeight(w) == 0)
    {
	/* hz = True; */
	XtHeight(w) = 1;
    }

    *value_mask |= CWDontPropagate | CWBitGravity;
    attributes->do_not_propagate_mask =
	(KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask |
	 PointerMotionMask);
    attributes->bit_gravity = NorthWestGravity;

    XtCreateWindow(w, (unsigned int)InputOutput,
		   (Visual *)CopyFromParent, *value_mask, attributes);

    /* rws 7 Oct 1998
       drawingarea/test7 shows that this does not happen
       if (wz)
       {
       XtWidth(w) = 0;
       }
       if (hz)
       {
       XtHeight(w) = 0;
       }
     */
}

static Boolean
set_values(Widget old, Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
	Boolean need_refresh = False;
	/* A variable to hold the trait flags */
	Mask	msk = 0;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "SetValues()\n"));

    if (_XmGetBGPixmapName() != NULL)
    {
	XSetWindowAttributes attr;

	CoreBackgroundPixmap(new_w) =
	    XmGetPixmapByDepth(XtScreen(new_w),
			       _XmGetBGPixmapName(),
			       MGR_Foreground(new_w),
			       XtBackground(new_w),
			       CoreDepth(new_w));

	attr.background_pixmap = CoreBackgroundPixmap(new_w);
	XChangeWindowAttributes(XtDisplay(new_w), XtWindow(new_w),
				CWBackPixmap, &attr);

	_XmClearBGPixmap();
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRUnitType),
			     MGR_UnitType(new_w),
			     new_w))
    {
	MGR_UnitType(new_w) = MGR_UnitType(old);
    }

    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRNavigationType),
			     MGR_NavigationType(new_w),
			     new_w))
    {
	MGR_NavigationType(new_w) = MGR_NavigationType(old);
    }

    need_refresh = _XmNavigSetValues(old, request, new_w, args, num_args);

    if (MGR_ShadowThickness(old) != MGR_ShadowThickness(new_w)
	|| MGR_Foreground(old) != MGR_Foreground(new_w))
    {
	need_refresh = True;
    }
	if (MGR_Foreground(old) != MGR_Foreground(new_w))
		msk |= VisualForeground;

	if (MGR_HighlightColor(old) != MGR_HighlightColor(new_w)
			|| MGR_HighlightPixmap(old) != MGR_HighlightPixmap(new_w)) {
		XtReleaseGC(new_w, MGR_HighlightGC(new_w));
		CreateHighlightGC(new_w);
		need_refresh = True;
		if (MGR_HighlightColor(old) != MGR_HighlightColor(new_w))
			msk |= VisualHighlightColor;
		if (MGR_HighlightPixmap(old) != MGR_HighlightPixmap(new_w))
			msk |= VisualHighlightPixmap;
	}

	if (MGR_BottomShadowColor(old) != MGR_BottomShadowColor(new_w)
			|| MGR_BottomShadowPixmap(old) != MGR_BottomShadowPixmap(new_w)) {
		XtReleaseGC(new_w, MGR_BottomShadowGC(new_w));
		CreateBottomShadowGC(new_w);
		need_refresh = True;
		if (MGR_BottomShadowColor(old) != MGR_BottomShadowColor(new_w))
			msk |= VisualBottomShadowColor;
		if (MGR_BottomShadowPixmap(old) != MGR_BottomShadowPixmap(new_w))
			msk |= VisualBottomShadowPixmap;
	}

	if (MGR_TopShadowColor(old) != MGR_TopShadowColor(new_w)
			|| MGR_TopShadowPixmap(old) != MGR_TopShadowPixmap(new_w)) {
		XtReleaseGC(new_w, MGR_TopShadowGC(new_w));
		CreateTopShadowGC(new_w);
		need_refresh = True;
		if (MGR_TopShadowColor(old) != MGR_TopShadowColor(new_w))
			msk |= VisualTopShadowColor;
		if (MGR_TopShadowPixmap(old) != MGR_TopShadowPixmap(new_w))
			msk |= VisualTopShadowPixmap;
	}

	if (XtBackground(old) != XtBackground(new_w)) {
		XtReleaseGC(new_w, MGR_BackgroundGC(new_w));
		CreateBackgroundGC(new_w);
		need_refresh = True;
		if (XtBackground(old) != XtBackground(new_w))
			msk |= VisualBackgroundPixel;
	}

	_XmManagerImportArgs(new_w, args, num_args);

	if (msk) {
		XmCareVisualTrait	t;
		int			i;

		for (i=0; i<MGR_NumChildren(new_w); i++) {
			Widget	child = MGR_Children(new_w)[i];

			t = (XmCareVisualTrait) XmeTraitGet((XtPointer)XtClass(child),
				XmQTcareParentVisual);
			if (t && t->redraw(child, old, new_w, msk)) {
				/* FIX ME this doesn't look like the right action, does it ? */
				need_refresh = True;
			}
		}
	}
	return need_refresh;
}

/* constraint class methods */

static void
constraint_initialize(Widget request,
		      Widget new_w,
		      ArgList args,
		      Cardinal *num_args)
{
    Widget manager = XtParent(new_w);
    /* check to see if the gadget's event mask specifies an event
       that requires us to install an event handler. */

    if (XmIsGadget(new_w))
	if (!MGR_EventHandlerAdded(manager))
	{
	    /* we only need to check if we haven't already added one. */

	    if (G_EventMask(new_w) & XmMOTION_EVENT
		|| G_EventMask(new_w) & XmENTER_EVENT
		|| G_EventMask(new_w) & XmLEAVE_EVENT)
	    {
		DEBUGOUT(_LtDebug(__FILE__, manager, "adding event handler\n"));

		XtAddEventHandler(manager,
				  PointerMotionMask | EnterWindowMask |
				  LeaveWindowMask,
				  True,
				  (XtEventHandler)_XmManagerEventHandler, NULL);

		MGR_EventHandlerAdded(manager) = True;
	    }
	}

}

void
_XmGadgetArm(Widget w,
	     XEvent *event,
	     String *params,
	     Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    Widget g = (Widget)_XmInputForGadget(w, ev->x, ev->y);

    DEBUGOUT(_LtDebug(__FILE__, w, "Inside _XmGadgetArm()\n"));

    if (g)
    {
	XmProcessTraversal(g, XmTRAVERSE_CURRENT);
	_XmDispatchGadgetInput(g,
			       event,
			       XmARM_EVENT);

	MGR_SelectedGadget(w) = (XmGadget)g;
    }
    else
    {
	if (_XmIsNavigable(w))
	{
	    XmProcessTraversal(w, XmTRAVERSE_CURRENT);
	}
    }
    MGR_EligibleForMultiButtonEvent(w) = NULL;
}

void
_XmGadgetActivate(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    Widget g = (Widget)MGR_SelectedGadget(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "Inside _XmGadgetActivate()\n"));

    if (g)
    {
	_XmDispatchGadgetInput((Widget)g,
			       event,
			       XmACTIVATE_EVENT);
	MGR_EligibleForMultiButtonEvent(w) = (XmGadget)g;
    }
    MGR_SelectedGadget(w) = NULL;
}

void
_XmGadgetMultiArm(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    Widget g = (Widget)_XmInputForGadget(w, ev->x, ev->y);

    DEBUGOUT(_LtDebug(__FILE__, w, "Inside _XmGadgetMultiArm()\n"));

    if (g)
    {
	/* This only counts as a multiArm if we're arming the gadget we just
	 * activated.  Otherwise it's just a regular arm.
	 */

	if (g == (Widget)MGR_EligibleForMultiButtonEvent(w))
	{
	    _XmDispatchGadgetInput(g,
				   event,
				   XmMULTI_ARM_EVENT);
	}
	else
	{
	    XmProcessTraversal(g, XmTRAVERSE_CURRENT);
	    _XmDispatchGadgetInput(g,
				   event,
				   XmARM_EVENT);
	    MGR_EligibleForMultiButtonEvent(w) = NULL;
	}
	MGR_SelectedGadget(w) = (XmGadget)g;
    }
    else
    {
	if (_XmIsNavigable(w))
	{
	    XmProcessTraversal(w, XmTRAVERSE_CURRENT);
	}
	MGR_EligibleForMultiButtonEvent(w) = NULL;
    }
}

void
_XmGadgetMultiActivate(Widget w,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params)
{
    Widget g = (Widget)MGR_SelectedGadget(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "Inside _XmGadgetMultiActivate()\n"));

    if (g)
    {
	/* This only counts as a multiActivate if we're activating a
	 * multiArmed gadget.  Otherwise it's just a regular activate.
	 */

	if (g == (Widget)MGR_EligibleForMultiButtonEvent(w))
	{
	    _XmDispatchGadgetInput((Widget)g,
				   event,
				   XmMULTI_ACTIVATE_EVENT);
	}
	else
	{
	    _XmDispatchGadgetInput((Widget)g,
				   event,
				   XmACTIVATE_EVENT);
	    MGR_EligibleForMultiButtonEvent(w) = (XmGadget)g;
	}
    }

    MGR_SelectedGadget(w) = NULL;
}

void
_XmGadgetDrag(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    XButtonEvent *ev = (XButtonEvent *)event;
    Widget g = (Widget)_XmInputForGadget(w, ev->x, ev->y);

    DEBUGOUT(_LtDebug(__FILE__, w, "Inside _XmGadgetDrag()\n"));

    if (g)
    {
	_XmDispatchGadgetInput((Widget)g,
			       event,
			       XmBDRAG_EVENT);
    }
}

void
_XmGadgetSelect(Widget w,
		XEvent *event,
		String *params,
		Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    DEBUGOUT(_LtDebug2(__FILE__, w, g, "Inside _XmGadgetSelect()\n"));

    if (g && XmIsGadget(g))
    {
	XmGadgetClass gc = (XmGadgetClass)XtClass(g);

	if (gc->gadget_class.arm_and_activate)
	{
	    (*gc->gadget_class.arm_and_activate) (g, event, params, num_params);
	}
    }
}

void
_XmManagerParentActivate(Widget w,
			 XEvent *event,
			 String *params,
			 Cardinal *num_params)
{
    XmParentProcessDataRec data;
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)w->core.widget_class;

    DEBUGOUT(_LtDebug(__FILE__, w, "Inside ManagerParentActivate()\n"));

    data.input_action.process_type = XmINPUT_ACTION;
    data.input_action.event = event;
    data.input_action.action = XmPARENT_ACTIVATE;
    data.input_action.params = params;
    data.input_action.num_params = num_params;

    if (XmIsManager(w) && mwc->manager_class.parent_process)
    {
	if (!(*mwc->manager_class.parent_process) (w, &data))
	{
	    /* propagate to parent? */
	}
    }
}

void
_XmGadgetTraversePrevTabGroup(Widget w,
			      XEvent *event,
			      String *params,
			      Cardinal *num_params)
{
	Widget g = MGR_ActiveChild(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, g, "_XmGadgetTraversePrevTabGroup()\n"));
	XmProcessTraversal(g, XmTRAVERSE_PREV_TAB_GROUP);
}

void
_XmGadgetTraverseNextTabGroup(Widget w,
			      XEvent *event,
			      String *params,
			      Cardinal *num_params)
{
	Widget g = MGR_ActiveChild(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, g, "_XmGadgetTraverseNextTabGroup()\n"));
	XmProcessTraversal(g, XmTRAVERSE_NEXT_TAB_GROUP);
}

void
_XmGadgetTraverseUp(Widget w,
		    XEvent *event,
		    String *params,
		    Cardinal *num_params)
{
	Widget g = MGR_ActiveChild(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, g, "_XmGadgetTraverseUp()\n"));
	XmProcessTraversal(g, XmTRAVERSE_UP);
}

void
_XmGadgetTraverseDown(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params)
{
	Widget g = MGR_ActiveChild(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, g, "_XmGadgetTraverseDown()\n"));
	XmProcessTraversal(g, XmTRAVERSE_DOWN);
}

void
_XmGadgetTraverseLeft(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params)
{
	Widget g = MGR_ActiveChild(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, g, "_XmGadgetTraverseLeft()\n"));
	XmProcessTraversal(g, XmTRAVERSE_LEFT);
}

void
_XmGadgetTraverseRight(Widget w,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params)
{
	Widget g = MGR_ActiveChild(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, g, "_XmGadgetTraverseRight()\n"));
	XmProcessTraversal(g, XmTRAVERSE_RIGHT);
}

void
_XmGadgetTraverseHome(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params)
{
	Widget g = MGR_ActiveChild(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, g, "_XmGadgetTraverseHome()\n"));
	XmProcessTraversal(g, XmTRAVERSE_HOME);
}

void
_XmGadgetHelp(Widget w,
	      XEvent *event,
	      String *params,
	      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);
    Widget cur;
    XmAnyCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "Inside _XmGadgetHelp()\n"));

    if (g)
    {
	_XmDispatchGadgetInput(g,
			       event,
			       XmHELP_EVENT);
    }
    else
    {
	cur = w;

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
}

void
_XmManagerHelp(Widget w,
	       XEvent *event,
	       String *params,
	       Cardinal *num_params)
{
    _XmGadgetHelp(w, event, params, num_params);
}

void
_XmGadgetKeyInput(Widget w,
		  XEvent *event,
		  String *params,
		  Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    DEBUGOUT(_LtDebug2(__FILE__, w, g, "Inside _XmGadgetKeyInput()\n"));
    DEBUGOUT(_LtDebug2(__FILE__, w, g,
		       "event type: %d code %08x\n",
		       event->type, event->xkey.keycode));

    if (g)
    {
	_XmDispatchGadgetInput(g,
			       event,
			       XmKEY_EVENT);
    }
}

void
_XmGadgetButtonMotion(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params)
{
    Widget g = MGR_ActiveChild(w);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "Inside _XmGadgetButtonMotion(), gadget %s\n",
		      g ? XtName(g) : "(null)"));

    if (g)
    {
	_XmDispatchGadgetInput(g,
			       event,
			       XmMOTION_EVENT);
    }
}

void
_XmManagerParentCancel(Widget w,
		       XEvent *event,
		       String *params,
		       Cardinal *num_params)
{
    XmParentProcessDataRec data;
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)w->core.widget_class;

    DEBUGOUT(_LtDebug(__FILE__, w, "ManagerParentCancel\n"));

    data.input_action.process_type = XmINPUT_ACTION;
    data.input_action.event = event;
    data.input_action.action = XmPARENT_CANCEL;
    data.input_action.params = params;
    data.input_action.num_params = num_params;

    if (XmIsManager(w) && mwc->manager_class.parent_process)
    {
	(*mwc->manager_class.parent_process) (w, &data);
    }
}

void
_XmGadgetTraversePrev(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params)
{
	Widget g = MGR_ActiveChild(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, g, "_XmGadgetTraversePrev()\n"));
	XmProcessTraversal(g, XmTRAVERSE_PREV);
}


void
_XmGadgetTraverseNext(Widget w,
		      XEvent *event,
		      String *params,
		      Cardinal *num_params)
{
	Widget g = MGR_ActiveChild(w);

	DEBUGOUT(_LtDebug2(__FILE__, w, g, "_XmGadgetTraverseNext()\n"));
	XmProcessTraversal(g, XmTRAVERSE_NEXT);
}

/*
 * yeah, right.  Who came up with these names, anyway?
 */
void
_XmSocorro(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmAnyCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "Inside _XmSocorro()\n"));

    cbs.reason = XmCR_HELP;
    cbs.event = event;

    while (w != NULL)
    {
	if (XtHasCallbacks(w, XmNhelpCallback) == XtCallbackHasSome)
	{
	    XtCallCallbacks(w, XmNhelpCallback, (XtPointer)&cbs);
	    return;
	}
	w = XtParent(w);
    }
}

/* we don't handle anything by default.  We leave that up to the other
 * manager widgets, like bulletin board. Just pop up to our parent*/
Boolean
_XmParentProcess(Widget widget,
		 XmParentProcessData data)
{
    Widget p = XtParent(widget);
    XmManagerWidgetClass mwc = (XmManagerWidgetClass)p->core.widget_class;

    if (XmIsManager(p) && mwc->manager_class.parent_process)
    {
	return ((*mwc->manager_class.parent_process) (p, data));
    }

    return False;
}

static void
_XmManagerEventHandler(Widget w,
		       XtPointer data,
		       XEvent *event,
		       Boolean *cont)
{
    XmGadget g;

    if (CoreBeingDestroyed(w))
    {
	return;
    }

    g = _XmInputForGadget(w, event->xmotion.x, event->xmotion.y);
    if (g == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "### _XmManagerEventHandler(gadget NULL)\n"));
    }
    else
    {
	DEBUGOUT(_LtDebug2(__FILE__, w,
			   (Widget)g, "### _XmManagerEventHandler()\n"));
    }

    /* We've got to tie this into the focus policy.  If the focus mode is
     * explicit, we get focus in/out events.  If the focus mode is pointer,
     * we get enter/leave events.  Regardless, we get one or the other,
     * but not both.  You can see this in Motif/mwm, but not in LessTif/lmwm.
     * There's a lot of wacky stuff that Motif does in the BaseClass
     * extension procs.  This (at least for Gadgets) is one of them (the
     * FocusChange proc).
     * Started this, but currently it is a hack.  Focus in/out and Enter/Leave
     * look the same -- and they shouldn't.  The trouble I'm having is how
     * to distinguish the two cases.  It really needs to follow when the
     * Gadget gets the keyboard focus (er, well, really when the parent
     * gets the keyboard focus). MLM
     *
     * 960720 -- fixed this.  Now this event handler only does the work for
     * XmPOINTER.  XmEXPLICIT is handled via the focus mechanism in Traversal.c
     * MLM
     *
     * 961114 -- Sometimes I am slow.  POINTER or InDragMode, stupid.  MLM
     *
     * Not so.  Enter and Leave events are quite independent of focus,
     * and despite its name, MGR_HighlightedWidget is only used for tracking
     * Enter/Leave.  So regardless of focus policy, set MGR_HighlightedWidget
     * to track the gadget under the pointer.  JHG
     */
    if (g == NULL)
    {
	/* 
	 * we're now not in a gadget child, or the child isn't listening
	 * 
	 * if we weren't in a gadget before, do nothing.
	 * else
	 * if we were in a gadget before, send a leave event to the old gadget
	 */

	if (MGR_HighlightedWidget(w) != NULL)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Sending LEAVE event to gadget\n"));
	    _XmDispatchGadgetInput(MGR_HighlightedWidget(w),
				   event,
				   XmLEAVE_EVENT);

	    MGR_HighlightedWidget(w) = NULL;
	}
    }
    else
    {
	/* we are in a gadget child 
	 * if we weren't in a gadget before, send an enter event to the new
	 *    gadget
	 * else
	 * if we were in a gadget before:
	 *    if it's the same gadget, send a motion event.
	 *    else
	 *    it it's not the same gadget, send a leave event to the old
	 *       one and an enter event to the new one.
	 */

	if (MGR_HighlightedWidget(w) == NULL)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Sending ENTER event to gadget\n"));

	    _XmDispatchGadgetInput((Widget)g,
				   event,
				   XmENTER_EVENT);
	    MGR_HighlightedWidget(w) = (Widget)g;
	}
	else
	{
	    /* the motion events are already propagated to highlightedwidget */
	    if (MGR_HighlightedWidget(w) != (Widget)g)
	    {

		DEBUGOUT(_LtDebug(__FILE__, w,
				  "Sending LEAVE event to old gadget\n"));

		_XmDispatchGadgetInput(MGR_HighlightedWidget(w),
				       event,
				       XmLEAVE_EVENT);

		DEBUGOUT(_LtDebug(__FILE__, w,
				  "Sending ENTER event to new gadget\n"));

		_XmDispatchGadgetInput((Widget)g,
				       event,
				       XmENTER_EVENT);

		MGR_HighlightedWidget(w) = (Widget)g;
	    }
	}
    }
}

void
_XmDestroyParentCallback(Widget w, XtPointer client, XtPointer call)
{
    DEBUGOUT(_LtDebug2("NEDIT", XtParent(w), w,
		       "%s:_XmDestroyParentCallback(%d)\n",
		       __FILE__, __LINE__));
    XtDestroyWidget(XtParent(w));
}

void
_XmClearShadowType(Widget w,
		   Dimension old_width, Dimension old_height,
		   Dimension old_shadow_thickness,
		   Dimension old_highlight_thickness)
{
    if (old_shadow_thickness > 0 && XtIsRealized(w))
    {
	if (XtWidth(w) >= old_width)
	{
	    XClearArea(XtDisplay(w), XtWindow(w),
		    old_width - old_shadow_thickness - old_highlight_thickness,
		       0, old_shadow_thickness,
		       old_height - old_highlight_thickness, False);
	}
	if (XtHeight(w) >= old_height)
	{
	    XClearArea(XtDisplay(w), XtWindow(w),
		       0,
		   old_height - old_shadow_thickness - old_highlight_thickness,
		       old_width - old_highlight_thickness,
		       old_shadow_thickness, False);
	}
    }
}

/*
 * Accelerators and Mnemonics -  How they work.
 *
 * Accelerators are an issue to be dealt with on a per "window" basis -
 * if you take the user's perception of a window.
 * We collect the information on accelerators in the Menu Bar (a RowColumn
 * hence a subclass of Manager).
 *
 * Information on mnemonics is stored in the menu pane itself - also a
 * RowColumn.
 *
 * MENU BAR Mnemonics:
 * Two cases :
 * If one of the cascades in the menu bar has been activated, then
 *      the mnemonic's key by itself should trigger the corresponding
 *      cascade button.
 * If the menu bar is not selected, then Alt+the mnemonic should work.
 *
 * Only the latter case is treated here - the other is treated by ???
 * when the menu is activated, either through the mouse or the keyboard.
 *
 * MENU PULLDOWN Mnemonics:
 * If the menu is up and has the focus, then the mnemonic's key
 * by itself should trigger the widget
 *
 * MENU OPTION Mnemonics:
 * FIX ME -- don't know what this is supposed to do
 */
static void
_XmAcceleratorHandler(Widget w, XtPointer client_data,
		      XEvent *event, Boolean *cont)
{
    int i;
    XtActionProc act = NULL;
    Widget comp = (Widget)client_data;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "Got Accelerator key: type %d code %04x "
		      "state %08x data %08x\n",
		      event->type, event->xkey.keycode, event->xkey.state,
		      client_data));
    DEBUGOUT(_LtDebug("NMEM", w,
		      "Got Accelerator key: type %d code %04x "
		      "state %08x data %08x\n",
		      event->type, event->xkey.keycode, event->xkey.state,
		      client_data));

    if (!XmIsManager(w) ||
	(event->type != KeyPress && event->type != KeyRelease))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Bad match for event.  Not manager or bad event.\n"));
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
		    "Got Accelerator key: code: %08x state: %08x cd %08x %s\n",
		      event->xkey.keycode, event->xkey.state, comp,
		      XtName(comp)));
    DEBUGOUT(_LtDebug("NMEM", w,
		    "Got Accelerator key: code: %08x state: %08x cd %08x %s\n",
		      event->xkey.keycode, event->xkey.state, comp,
		      XtName(comp)));

    /* JAC added this test.  Occasionally in nedit the accelerators cause
       segfaults.  The comp value passed has 0 for all fields in the
       Widget.  e.g.:

       (gdb) p *comp

       $10 = {core = {self = 0x0, widget_class = 0x0, parent = 0x0,
       xrm_name = 0, being_destroyed = 0 '\000', destroy_callbacks = 0x0,
       constraints = 0x0, x = 0, y = 0, width = 0, height = 0,
       border_width = 0, managed = 0 '\000', sensitive = 0 '\000',
       ancestor_sensitive = 0 '\000', event_table = 0x0, tm = {
       translations = 0x0, proc_table = 0x0, current_state = 0x0,
       lastEventTime = 0}, accelerators = 0x0, border_pixel = 0,
       border_pixmap = 0, popup_list = 0x0, num_popups = 0, name = 0x0,
       screen = 0x0, colormap = 0, window = 0, depth = 0, background_pixel
       = 0, background_pixmap = 0, visible = 0 '\000', mapped_when_managed
       = 0 '\000'}} 

       I suspect that "comp" may be the address of a widget that's been
       destroyed, and for some reason we're still getting the callback,
       but that's a WAG.
     */
    if (!comp->core.widget_class)
    {
#ifdef	LESSTIF_VERBOSE
	_XmWarning(NULL, "XmManager _XmAcceleratorHandler: bad news!!!\n");
#endif
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "_XmAcceleratorHandler: called for w=%s"
			  " with invalid comp, ignoring\n",
			  XtName(w)));
	return;

    }
    /* Another one of these. Danny 21/10/2001 */
    if (comp->core.being_destroyed) {
#ifdef	LESSTIF_VERBOSE
	_XmWarning(NULL, "XmManager _XmAcceleratorHandler: being destroyed, bad news!!!\n");
#endif
	return;
    }

    /* Yet another one - Danny 30/5/2002 */
    if (comp->core.self != comp) {
#ifdef	LESSTIF_VERBOSE
	_XmWarning(NULL, "XmManager _XmAcceleratorHandler: I am not myself lately\n");
#endif
	return;
    }

    /*
     * Insensitive widgets shouldn't be activated by accelerators or
     * mnemonics.  We could keep inserting and removing things from
     * the MGR_KeyboardList as widgets change sensitivity but that's
     * just sick.  Hence, we just filter out insensitive widgets.
     * This test could go inside the following search loop but there's
     * no point in searching when we know that we can't do anything with
     * what we find.
     * --mu@trends.net, 1998.09.06 */
    if (!XtIsSensitive(comp))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			"_XmAcceleratorHandler: %s is insensitive, ignoring\n",
			  XtName(comp)));
	return;
    }

    for (i = 0; i < MGR_NumKeyboardEntries(w); i++)
    {
          /* Modified 21-Sep-04 - dwilliss - MicroImages, Inc
	 Accelerator shouldn't care if CapsLock or NumLock is down. Ideally, the 
	 accelerator specification should have been able to tell us "only these 
	 modifiers" or "these modifiers plus any others" */
	if (/*MGR_KeyboardList(w)[i].eventType == event->type && */
	    MGR_KeyboardList(w)[i].key == event->xkey.keycode &&
	    MGR_KeyboardList(w)[i].modifiers ==
	        (event->xkey.state & ~(Mod2Mask | LockMask)) &&
	    MGR_KeyboardList(w)[i].component == comp &&
	    XtIsManaged(comp))
	{
	    Cardinal num_params = 0;

	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "MATCH!!!! "
			      "code %08x state %08x name: %s:%s mnem: %d\n",
			      event->xkey.keycode, event->xkey.state,
			      comp
			      ? comp->core.widget_class->core_class.class_name
			      : "(nil)",
			      comp ? XtName(comp) : "(nil)",
			      MGR_KeyboardList(w)[i].isMnemonic));
	    DEBUGOUT(_LtDebug("NMEM", w,
			      "MATCH!!!! "
			      "code %08x state %08x name: %s:%s mnem: %d\n",
			      event->xkey.keycode, event->xkey.state,
			      comp
			      ? comp->core.widget_class->core_class.class_name
			      : "(nil)",
			      comp ? XtName(comp) : "(nil)",
			      MGR_KeyboardList(w)[i].isMnemonic));

	    /* Make sure XmGetPostedFromWidget works: ? */

   /* amai: The following "if (act==NULL)" catches are just dumb
            attempts to prevent some rare crashes, e.g. as seen with
   	    NEdit. They really should be replaced by something better.
   	    Actually the only confirmed crash location is the
   	    first one, however I just decided to add a "full set"
	    of checks here ...
	    The _XmWarning()s should help us to get additional feedback
	    on that issue!
	    FIX ME ! */

	    if (!MGR_KeyboardList(w)[i].isMnemonic || RC_Type(XtParent(comp)) == XmMENU_BAR ||
		(MGR_KeyboardList(w)[i].isMnemonic && XtParent(comp) == RC_PopupPosted(w))
		)
	    {
		DEBUGOUT(_LtDebug2(__FILE__, w, MGR_KeyboardList(w)[i].component,
				   "menu showing\n"));
		DEBUGOUT(_LtDebug2("NMEM", w, MGR_KeyboardList(w)[i].component,
				   "menu showing\n"));
		if (XmIsPrimitive(MGR_KeyboardList(w)[i].component))
		{
		    act = PrimC_ArmAndActivate(XtClass(comp));
		    if (act==NULL) {
#ifdef	LESSTIF_VERBOSE
                       _XmWarning(w, "%s:_XmAcceleratorHandler(%d) - act==NULL for %s", 
		               __FILE__, __LINE__, comp->core.widget_class->core_class.class_name);
#endif
		    } else {
                       (*act) (comp, event, NULL, &num_params);
		    }
		}
		else if (XmIsGadget(MGR_KeyboardList(w)[i].component))
		{
		    act = GC_ArmAndActivate(XtClass(comp));
		    if (act==NULL) {
#ifdef	LESSTIF_VERBOSE
                       _XmWarning(w, "%s:_XmAcceleratorHandler(%d) - act==NULL for gadget", 
		               __FILE__, __LINE__);
#endif
	            } else {
                       (*act) (comp, event, NULL, &num_params);
		    }
		}
		else if (XmIsRowColumn(w))
		{
		    act = RCClass_ArmAndActivate(XtClass(comp));
		    if (act==NULL) {
#ifdef	LESSTIF_VERBOSE
                       _XmWarning(w, "%s:_XmAcceleratorHandler(%d) - act==NULL for RowColumn", 
		               __FILE__, __LINE__);
#endif
		    } else {
                       (*act) (comp, event, NULL, &num_params);
		    }
		}
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, MGR_KeyboardList(w)[i].component,
				      "Not a widget/gadget\n"));

		    XtCallActionProc(comp, "Activate", event, NULL, 0);
		}
		*cont = False;
		break;
	    }
	    else
	    {
		DEBUGOUT(_LtDebug2(__FILE__, w, MGR_KeyboardList(w)[i].component,
				   "menu is not showing\n"));
		DEBUGOUT(_LtDebug2("NMEM", w, MGR_KeyboardList(w)[i].component,
				   "menu is not showing\n"));
	    }

	    /*
	    *cont = False;
	    break;
	    */
	}
    }
}

static void
AddKeyboardEntry(Widget m, Widget w, XmKeyboardData *item)
{
    int i;
    unsigned long mask;

    /* Register with the manager widget */
    if (MGR_SizeKeyboardList(m) == 0 ||
	(MGR_NumKeyboardEntries(m) == MGR_SizeKeyboardList(m)))
    {
	i = MGR_SizeKeyboardList(m);

	if (MGR_SizeKeyboardList(m) == 0)
	{
	    MGR_SizeKeyboardList(m) = 8;
	    MGR_KeyboardList(m) =
		(XmKeyboardData *)XtMalloc(sizeof(XmKeyboardData) *
					   MGR_SizeKeyboardList(m));
	}
	else
	{
	    MGR_SizeKeyboardList(m) *= 2;
	    MGR_KeyboardList(m) =
		(XmKeyboardData *)XtRealloc((char *)MGR_KeyboardList(m),
					    sizeof(XmKeyboardData) *
					    MGR_SizeKeyboardList(m));
	}

	if (MGR_SizeKeyboardList(m) > i)
	{
	    memset(&MGR_KeyboardList(m)[i], 0,
		  (MGR_SizeKeyboardList(m) - i) * sizeof(XmKeyboardData));
	}
    }
    else
    {
	i = MGR_NumKeyboardEntries(m);
    }

    if (MGR_KeyboardList(m)[i].component != NULL)
    {
	_XmError(m, "This should not happen: %d\n", i);
    }

    memcpy(&MGR_KeyboardList(m)[i], item, sizeof(XmKeyboardData));

    MGR_NumKeyboardEntries(m)++;

    DEBUGOUT(_LtDebug(__FILE__, m,
		      "GrabKey: key %04x Sym: %04x modifiers %08x %s %s\n",
		      item->key, item->keysym, item->modifiers,
		      item->needGrab ? "Grabbing" : "Not Grabbing",
		      item->isMnemonic ? "mnemonic" : "not mnemonic"));
    DEBUGOUT(_LtDebug("NMEM", m,
		      "GrabKey: key %04x Sym: %04x modifiers %08x %s %s\n",
		      item->key, item->keysym, item->modifiers,
		      item->needGrab ? "Grabbing" : "Not Grabbing",
		      item->isMnemonic ? "mnemonic" : "not mnemonic"));
#if 1
	switch (item->eventType) {
	case KeyPress:
		mask = KeyPressMask;
		break;
	case KeyRelease:
		mask = KeyReleaseMask;
		break;
	case ButtonPress:
		mask = ButtonPressMask;
		break;
	case ButtonRelease:
		mask = ButtonReleaseMask;
		break;
	default:
#ifdef	LESSTIF_VERBOSE
		_XmWarning(m, "AddKeyboardEntry: can't map event %d\n", item->eventType);
#endif
		mask = 0;
	}
#else
	mask = KeyPressMask;
	mask = item->eventType;	/* This must be wrong but the alternatives crash nedit. FIX ME */
#endif
    XtInsertEventHandler(m, mask,
			 False, _XmAcceleratorHandler,
			 (XtPointer)w, XtListHead);

    if (item->needGrab)
    {
      /* Modified 21-Sep-04 - dwilliss - MicroImages, Inc
	 Grab for accelerator - accelerator shouldn't care if CapsLock or NumLock
	 is down, but we have to XtGrabKey with all combinations of them or we only 
	 get called if they're off. Ideally, the accelerator specification should
	 have been able to tell us "only these modifiers" or "these modifiers plus
	 any others" */
	XtGrabKey(m, item->key, item->modifiers, False,
		  GrabModeAsync, GrabModeAsync);
        XtGrabKey(m, item->key, item->modifiers | LockMask, False,
		  GrabModeAsync, GrabModeAsync);
	XtGrabKey(m, item->key, item->modifiers | Mod2Mask, False,
		  GrabModeAsync, GrabModeAsync);
	XtGrabKey(m, item->key, item->modifiers | Mod2Mask | LockMask, False,
		  GrabModeAsync, GrabModeAsync);
    }
}

static void
DeleteKeyboardEntry(Widget m, Widget w, Boolean isMnemonic)
{
    int i;

    /* Danger, Will Robinson */
    if (CoreBeingDestroyed(m))
    {
	return;
    }

    for (i = 0; i < MGR_NumKeyboardEntries(m); i++)
    {
	if (MGR_KeyboardList(m) && w == MGR_KeyboardList(m)[i].component &&
	    ((MGR_KeyboardList(m)[i].isMnemonic && isMnemonic) ||
	     (!MGR_KeyboardList(m)[i].isMnemonic && !isMnemonic)))
	{
	    XmKeyboardData *data = &MGR_KeyboardList(m)[i];

	    XtRemoveEventHandler(m, data->eventType, False,
				 _XmAcceleratorHandler, w);

	    if (data->needGrab)
	    {
	     /* Modified 21-Sep-04 - dwilliss - MicroImages, Inc
		  To balance change made to AddKeyboardEntry above */
		XtUngrabKey(m, data->key, data->modifiers);
		XtUngrabKey(m, data->key, data->modifiers | LockMask);
		XtUngrabKey(m, data->key, data->modifiers | Mod2Mask);
		XtUngrabKey(m, data->key, data->modifiers | LockMask | Mod2Mask);
	    }

	    if (i < MGR_NumKeyboardEntries(m) - 1)
	    {
		memcpy(&MGR_KeyboardList(m)[i],
                       &MGR_KeyboardList(m)[i + 1],
		       sizeof(XmKeyboardData) *
		        (MGR_NumKeyboardEntries(m) - i - 1));

		memset(&MGR_KeyboardList(m)[MGR_NumKeyboardEntries(m) - 1], 0,
		      sizeof(XmKeyboardData));
	    }
	    /* end of list */
	    else
	    {
		memset(&MGR_KeyboardList(m)[i], 0,
		       sizeof(XmKeyboardData));
	    }

	    MGR_NumKeyboardEntries(m)--;
	    break;
	}
    }
}

void
_XmManagerInstallAccelerator(Widget m, Widget w, String s)
{
    Widget mw, sh;
    int eventType = 0;
    unsigned keysym = 0;
    unsigned int modifiers = 0;
    XmKeyboardData data;

    DEBUGOUT(_LtDebug2(__FILE__, m, w,
		       "_XmManagerInstallAccelerator(%s)\n", s));
    DEBUGOUT(_LtDebug2("NMEM", m, w,
		       "_XmManagerInstallAccelerator(%s)\n", s));

    /* Do we really have an accelerator ? */
    if (s == NULL || strlen(s) == 0)
	return;

    /* Check whether we're in a menu system */
    mw = XtParent(w);
    if (!XmIsRowColumn(mw))
    {
	return;
    }

    if (!(RC_Type(mw) == XmMENU_PULLDOWN || RC_Type(mw) == XmMENU_OPTION))
    {
	return;
    }

    /* Find the manager widget just underneath the shell */
    for (mw = m; XtParent(mw); mw = sh)
    {
	sh = XtParent(mw);

	if (XtIsSubclass(sh, applicationShellWidgetClass) ||
	    XtIsSubclass(sh, xmDialogShellWidgetClass) ||
	    XtIsSubclass(sh, transientShellWidgetClass) ||
	    XtIsSubclass(sh, topLevelShellWidgetClass))
	{
	    break;
	}
    }

    if (!XmIsManager(mw))
    {
	return;
    }


    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmManagerInstallAccelerator found manager %s\n",
		      XtName(mw)));


    _XmMapKeyEvent(s, &eventType, &keysym, &modifiers);

    /* Put the data in the entry */
    data.component = w;
    data.eventType = eventType;
    data.isMnemonic = False;
    data.keysym = keysym;
    data.key = XKeysymToKeycode(XtDisplay(w), data.keysym);
    data.modifiers = modifiers;
    data.needGrab = True;
    if (data.key == 0)
    {
#if 1
    KeySym real_sym;
    Modifiers real_mod;

	_XmVirtualToActualKeysym(XtDisplay(w), data.keysym, &real_sym, &real_mod);
	if (real_sym == NoSymbol && real_mod == (Modifiers)0)
	{
	    _XmWarning(m, "%s:_XmManagerInstallAccelerator(%d) - Could not convert >%s< to a KeyCode\n    %s -> %08x -> %i", 
		__FILE__, __LINE__, s,
		s, data.keysym, data.key);
	}
	else
	{
	    data.key = XKeysymToKeycode(XtDisplay(w), real_sym);
	    data.modifiers = real_mod;
	    AddKeyboardEntry(mw, w, &data);
	}
#endif
    }
    else
    {
	AddKeyboardEntry(mw, w, &data);
    }
}

void
_XmManagerUninstallAccelerator(Widget m, Widget w)
{
    Widget mw, sh;

    /* Find the manager widget just underneath the shell */
    for (mw = m; XtParent(mw); mw = sh)
    {
	sh = XtParent(mw);
	if (XtIsSubclass(sh, applicationShellWidgetClass) ||
	    XtIsSubclass(sh, xmDialogShellWidgetClass) ||
	    XtIsSubclass(sh, transientShellWidgetClass) ||
		XtIsSubclass(sh, transientShellWidgetClass) ||
		XtIsSubclass(sh, transientShellWidgetClass) ||
	    XtIsSubclass(sh, topLevelShellWidgetClass))
	{
	    break;
	}
    }

    if (!XmIsManager(mw))
    {
	return;
    }

    DeleteKeyboardEntry(mw, w, False);
}

void
_XmManagerInstallMnemonic(Widget m, Widget w, KeySym mn)
{
    unsigned int modifiers = 0;
    Boolean need_grab = False;
    XmKeyboardData data;

    if (mn == 0 || mn == NoSymbol)
    {
	return;
    }

    DEBUGOUT(_LtDebug2(__FILE__, m, w, "_XmManagerInstallMnemonic(%c)\n", mn));
    DEBUGOUT(_LtDebug2("NMEM", m, w, "_XmManagerInstallMnemonic(%c)\n", mn));

    /* If we're dealing with the menu bar, store the stuff in the highest
     * manager widget that you can find (with all the accelerators) */
    if (XmIsRowColumn(m) &&
	(RC_Type(m) == XmMENU_PULLDOWN || RC_Type(m) == XmMENU_BAR || RC_Type(m) == XmMENU_OPTION))
    {
	Widget mw, sh;
	XmModifierMaskSetReference mods =
	_XmGetModifierMappingsForDisplay(XtDisplay(w));

	if (RC_Type(m) == XmMENU_PULLDOWN)
	{
	}
	else
	{
	    need_grab = True;
	    modifiers |= mods[ALTModifier];
	}

	for (mw = m; XtParent(mw); mw = sh)
	{
	    sh = XtParent(mw);
	    if (XtIsSubclass(sh, applicationShellWidgetClass) ||
		XtIsSubclass(sh, xmDialogShellWidgetClass) ||
		XtIsSubclass(sh, topLevelShellWidgetClass) ||
		(RC_Type(m) != XmMENU_BAR && XmIsRowColumn(mw) && RC_Type(mw) == XmMENU_BAR))
	    {
		break;
	    }
	}

	DEBUGOUT(_LtDebug2(__FILE__, mw, w, "Store mnemonic info\n"));
	DEBUGOUT(_LtDebug2("NMEM", mw, w, "Store mnemonic info\n"));
	m = mw;
    }

    if (XmIsManager(m))
    {
	/* Put the data in the entry */
	data.component = w;
	data.eventType = KeyPress;
	data.keysym = mn;
	data.isMnemonic = True;
	data.key = XKeysymToKeycode(XtDisplay(w), data.keysym);
	data.modifiers = modifiers;
	data.needGrab = need_grab;

	AddKeyboardEntry(m, w, &data);
    }
}

void
_XmManagerUninstallMnemonic(Widget m, Widget w)
{
    Widget mw, sh;

    if (XmIsRowColumn(m) &&
	(RC_Type(m) == XmMENU_PULLDOWN || RC_Type(m) == XmMENU_BAR || RC_Type(m) == XmMENU_OPTION))
    {
	/* Find the manager widget just underneath the shell */
	for (mw = m; XtParent(mw); mw = sh)
	{
	    sh = XtParent(mw);
	    if (XtIsSubclass(sh, applicationShellWidgetClass) ||
		XtIsSubclass(sh, xmDialogShellWidgetClass) ||
		XtIsSubclass(sh, topLevelShellWidgetClass) ||
		(RC_Type(m) != XmMENU_BAR && XmIsRowColumn(mw) && RC_Type(mw) == XmMENU_BAR))
	    {
		break;
	    }
	}
    }
    else
    {
	mw = m;
    }

    if (!XmIsManager(mw))
    {
	return;
    }

    DeleteKeyboardEntry(mw, w, True);
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
	    DEBUGOUT(_LtDebug(__FILE__, w,
			  "WidgetNavigable => XmDESCENDANTS_TAB_NAVIGABLE\n"));

	    return XmDESCENDANTS_TAB_NAVIGABLE;
	}
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "WidgetNavigable => XmDESCENDANTS_NAVIGABLE\n"));

	return XmDESCENDANTS_NAVIGABLE;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "WidgetNavigable => XmNOT_NAVIGABLE\n"));

    return XmNOT_NAVIGABLE;
}


void
_XmManagerHighlightPixmapDefault(Widget w, int offset, XrmValue *value)
{
    static Pixmap pixmap;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerHighlightPixmapDefault\n"));

    pixmap = XmUNSPECIFIED_PIXMAP;

    value->addr = (char *)&pixmap;
    value->size = sizeof(Pixmap);

    if (MGR_HighlightColor(w) == XtBackground(w))
	pixmap = XmGetPixmapByDepth(XtScreen(w), "50_foreground",
				    MGR_HighlightColor(w),
				    MGR_Foreground(w),
				    CoreDepth(w));
}


void
_XmManagerTopShadowPixmapDefault(Widget w, int offset, XrmValue *value)
{
    static Pixmap pixmap;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmManagerTopShadowPixmapDefault\n"));

    pixmap = XmUNSPECIFIED_PIXMAP;

    value->addr = (char *)&pixmap;
    value->size = sizeof(Pixmap);

    if (MGR_TopShadowColor(w) == XtBackground(w))
	pixmap = XmGetPixmapByDepth(XtScreen(w), "50_foreground",
				    MGR_TopShadowColor(w),
				    MGR_Foreground(w),
				    CoreDepth(w));
    else if (DefaultDepthOfScreen(XtScreen(w)) == 1)
	pixmap = XmGetPixmapByDepth(XtScreen(w), "50_foreground",
				    MGR_TopShadowColor(w),
				    XtBackground(w),
				    CoreDepth(w));
}


static void
_XmManagerExportX(Widget widget, int offset, XtArgVal *value)
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
_XmManagerExportY(Widget widget, int offset, XtArgVal *value)
{
    unsigned char unitType = _XmGetUnitType(widget);
    int converted_value;

    if (XtIsManaged(widget) && XtParent(widget) && XmIsVendorShell(XtParent(widget)))
    {
	*value = XtY(XtParent(widget));
    }
    converted_value = XmConvertUnits(widget,
				     XmVERTICAL,
				     XmPIXELS,
				     *value,
				     unitType);

    *value = converted_value;
}

static XmDirection 
get_direction(Widget w)
{
    /* 
       This macro obtains the layout direction resource even
       though it looks like the string direction resource. 
       In 2.0 layout direction replaced string direction
       but the identifier in the internal data structure stayed
       the same.  Everyone still with me. 8)
     */
    return MGR_StringDirection(w);
}

extern Widget
XmObjectAtPoint(Widget w, Position x, Position y)
{

   /* FIX ME */
   /* Used to be in GadgetUtil.c, but didn't do 
      exactly what it is supposed to ... */

    int i;
    
    DEBUGOUT(_LtDebug(__FILE__, w, "XmObjectAtPoint()\n"));
    for (i = 0; i < (int)MGR_NumChildren(w); i++)
    {
	Widget g = MGR_Children(w)[i];

	if (!XtIsManaged(g))
	{
	    continue;
	}

	if ((x >= XtX(g) && x < XtX(g) + XtWidth(g)) &&
	    (y >= XtY(g) && y < XtY(g) + XtHeight(g)))
	{
	    return g;
	}
    }
    /* Northing found */
    return NULL;
}
