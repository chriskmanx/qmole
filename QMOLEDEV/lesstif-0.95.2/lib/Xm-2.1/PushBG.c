/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PushBG.c,v 1.2 2005/06/25 09:45:49 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PushBG.c,v 1.2 2005/06/25 09:45:49 dannybackx Exp $";

#include <LTconfig.h>

#include <stdarg.h>
#include <string.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/CacheP.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuShell.h>
#include <Xm/MenuUtilP.h>
#include <Xm/PushBGP.h>
#include <Xm/RepType.h>
#include <Xm/RowColumnP.h>
#include <Xm/LabelP.h>
#include <X11/ShellP.h>

#include <Xm/TraitP.h>
#include <Xm/ActivatableT.h>
#include <Xm/ContItemT.h>

#include <XmI/DebugUtil.h>

/*
 * Experiment
 *
 * Try to add trait stuff by #ifdeffing it.
 */
void _XmPushBG_TraitAddCallback(Widget, XtCallbackProc, XtPointer, Boolean);

static XmActivatableTraitRec _XmPushBGTraitRec = {
	/* version	*/	0,
	/* cb		*/	_XmPushBG_TraitAddCallback
};

static void _XmPushBG_ContainerItemGetValues(Widget w, XmContainerItemData d);
static void _XmPushBG_ContainerItemSetValues(Widget w, XmContainerItemData d);

static XmContainerItemTraitRec _XmPushBG_ContainerItemTrait = {
        /* version */           0,
        /* setvalues */         _XmPushBG_ContainerItemGetValues,
        /* getvalues */         _XmPushBG_ContainerItemSetValues
};


/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static void expose(Widget w, XEvent *event, Region region);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

#if 0
static void get_values_hook(Widget w, ArgList args, Cardinal *num_args);
#endif

static void input_dispatch(Widget gadget, XEvent *event, Mask event_mask);

static void secondary_object_create(Widget request, Widget new_w,
				    ArgList args, Cardinal *num_args);

#if 0
static void initialize_prehook(Widget request,
			       Widget new_w, ArgList args, Cardinal *num_args);
#endif

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

static void export_show_as_default(Widget sw, int offset, XtArgVal *value);

static XmImportOperator import_show_as_default(Widget sw, int offset,
					       XtArgVal *value);

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


int _XmPushBCacheCompare(XtPointer A, XtPointer B);

static void MenuProcEntry(int proc, Widget rc,...);

/*
 * resources
 */
#define Offset(field) XtOffsetOf(XmPushButtonGCacheObjRec, pushbutton_cache.field)
static XtResource cache_resources[] =
{
    {
	XmNmultiClick, XmCMultiClick, XmRMultiClick,
	sizeof(unsigned char), Offset(multiClick),
	XmRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNdefaultButtonShadowThickness, XmCDefaultButtonShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(default_button_shadow_thickness),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNfillOnArm, XmCFillOnArm, XmRBoolean,
	sizeof(Boolean), Offset(fill_on_arm),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNarmColor, XmCArmColor, XmRPixel,
	sizeof(Pixel), Offset(arm_color),
	XmRCallProc, (XtPointer)_XmSelectColorDefault
    },
    {
	XmNarmPixmap, XmCArmPixmap, XmRGadgetPixmap,
	sizeof(Pixmap), Offset(arm_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    }
};

static XmSyntheticResource cache_syn_resources[] =
{
    {
	XmNdefaultButtonShadowThickness,
	sizeof(Dimension), Offset(default_button_shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};

/* *INDENT-OFF* */
XmPushButtonGCacheObjClassRec xmPushButtonGCacheObjClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGCacheObjClassRec,
	/* class_name            */ "XmPushButtonGCacheObjClass",
	/* widget_size           */ sizeof(XmPushButtonGCacheObjRec),
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
	/* syn_resources      */ cache_syn_resources,
	/* num_syn_resources  */ XtNumber(cache_syn_resources),
	/* extension          */ NULL
    },
    /* LabelGCacheObj part */
    {
	/* foo                */ 0
    },
    /* PushButtonGCacheObj part */
    {
	/* foo                */ 0
    }
};

#undef Offset
#define Offset(field) XtOffsetOf(XmPushButtonGadgetRec, pushbutton.field)

/* Resources for the pushbutton class */
static XtResource resources[] = {
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
	sizeof(Dimension), XtOffsetOf(XmPushButtonGadgetRec, gadget.shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNtraversalOn, XmCTraversalOn, XmRBoolean,
	sizeof(Boolean), XtOffsetOf(XmPushButtonGadgetRec, gadget.traversal_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNhighlightThickness, XmCHighlightThickness, XmRHorizontalDimension,
	sizeof(Dimension), XtOffsetOf(XmPushButtonGadgetRec,gadget.highlight_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNshowAsDefault, XmCShowAsDefault, XmRBooleanDimension,
	sizeof(Dimension), Offset(show_as_default),
	XmRImmediate, 0
    }
};

static XmSyntheticResource syn_resources[] = {
    {
	XmNshowAsDefault,
	sizeof(Dimension), Offset(show_as_default),
	export_show_as_default, import_show_as_default
    },
    {
	XmNhighlightThickness,
	sizeof(Dimension),
	XtOffsetOf(XmPushButtonGadgetRec, gadget.highlight_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
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
    _XmPushBCacheCompare
};

static XmBaseClassExtRec _XmPushBGRectClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ XmInheritInitializePrehook /*initialize_prehook*/,
    /* set_values_prehook        */ set_values_prehook,
    /* initialize_posthook       */ initialize_posthook,
    /* set_values_posthook       */ set_values_posthook,
    /* secondary_object_class    */ (WidgetClass)&xmPushButtonGCacheObjClassRec,
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

static XmGadgetClassExtRec _XmPushBGadgetClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmGadgetClassExtVersion,
    /* size                      */ sizeof(XmGadgetClassExtRec),
    /* widget_baseline_proc      */ XmInheritBaselineProc,
    /* display_rect_proc         */ XmInheritDisplayRectProc,
    /* margins_proc              */ XmInheritMarginsProc,
};

XmPushButtonGadgetClassRec xmPushButtonGadgetClassRec = {
    /* RectObj class part */
    {
	/* superclass            */ (WidgetClass) &xmLabelGadgetClassRec,
	/* class_name            */ "XmPushButtonGadget",
	/* widget_size           */ sizeof(XmPushButtonGadgetRec),
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
	/* resize                */ XtInheritResize, /* FIX ME */
	/* expose                */ expose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL /*get_values_hook*/,
	/* accept_focus          */ NULL,
	/* version               */ XtVersionDontCheck,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmPushBGRectClassExtRec
    },
    /* XmGadget part */
    {
	/* border_highlight   */ XmInheritBorderHighlight,  /* FIX ME */
	/* border_unhighlight */ XmInheritBorderUnhighlight, /* FIX ME */
	/* arm_and_activate   */ ArmAndActivate,
	/* input_dispatch     */ input_dispatch,
	/* visual_change      */ NULL, /* FIX ME */
	/* syn_resources      */ syn_resources,
	/* num_syn_resources  */ XtNumber(syn_resources),
	/* cache_part         */ &cache_part,
	/* extension          */ (XtPointer)&_XmPushBGadgetClassExtRec
    },
    /* XmLabelGadget part */
    {
	/* setOverrideCallback */ XmInheritSetOverrideCallback,
	/* menuProcs           */ XmInheritMenuProc,
	/* extension           */ NULL
    },
    /* XmPushButtonGadget part */
    {
	/* extension          */ NULL,
    },
};
/* *INDENT-ON* */

WidgetClass xmPushButtonGadgetClass = (WidgetClass)&xmPushButtonGadgetClassRec;

/*
 * Some #defines to make the code below more readable
 */
#define IN_MENU(w) (LabG_MenuType(w) == XmMENU_POPUP || \
		    LabG_MenuType(w) == XmMENU_PULLDOWN)

/******************************* GADGET PART *********************************/
static void
secondary_object_create(Widget request, Widget new_w,
			ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XtPointer nsec, rsec;
    XmWidgetExtData ed;
    Cardinal size;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "PushBGCacheRec %s being initialized.\n",
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

    PBG_Cache(new_w) = &(((XmPushButtonGCacheObject)nsec)->pushbutton_cache);
    PBG_Cache(request) = &(((XmPushButtonGCacheObject)rsec)->pushbutton_cache);
}

int
_XmPushBCacheCompare(XtPointer A, XtPointer B)
{
	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmPushBCacheCompare\n"));

	return !memcmp(((XmPushButtonGCacheObjPart *)A), ((XmPushButtonGCacheObjPart *)B),
		sizeof(XmPushButtonGCacheObjPart));
}

/******************************* GADGET PART *********************************/
static void
class_initialize(void)
{
    XtResourceList combined, labels;
    int ncom;
    Cardinal nlabels;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmPushButtonGadget class_initialize\n"));

    /* don't let the nulls fool you.  look at the header file -- the arg
     * isn't used. */
    ClassCacheHead(PBG_ClassCachePart(NULL)).prev =
	&ClassCacheHead(PBG_ClassCachePart(NULL));
    ClassCacheHead(PBG_ClassCachePart(NULL)).next =
	&ClassCacheHead(PBG_ClassCachePart(NULL));

    _XmPushBGRectClassExtRec.record_type = XmQmotif;

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

    xmPushButtonGCacheObjClassRec.object_class.resources = combined;
    xmPushButtonGCacheObjClassRec.object_class.num_resources = ncom;

	if (! XmeTraitSet((XtPointer)xmPushButtonGadgetClass, XmQTactivatable,
			(XtPointer)&_XmPushBGTraitRec)) {
		_XmWarning(NULL, "XmPushButtonGadget ClassInitialize: XmeTraitSet failed\n");
	}
	if (! XmeTraitSet((XtPointer)xmPushButtonGadgetClass, XmQTcontainerItem,
			(XtPointer)&_XmPushBG_ContainerItemTrait)) {
		_XmWarning(NULL, "XmPushButtonGadget ClassInitialize: XmeTraitSet failed\n");
	}
}

static void
class_part_initialize(WidgetClass widget_class)
{
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmPushButtonGadget class_part_initialize\n"));

	_XmFastSubclassInit(widget_class, XmPUSH_BUTTON_GADGET_BIT);
}

static void
CreateFillGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmPushButtonGadget CreateFillGC\n"));

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;

    values.fill_style = FillSolid;
    values.function = GXcopy;
    values.plane_mask = -1;
    values.subwindow_mode = ClipByChildren;
    values.graphics_exposures = False;
    values.foreground = PBG_ArmColor(w);
    values.background = XmParentBackground(w);

    PBG_FillGC(w) = XtGetGC(w, mask, &values);
}

static void
CreateBackgroundGC(Widget w)
{
    XGCValues values;
    XtGCMask mask;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmPushButtonGadget CreateBackgroundGC\n"));

    mask = GCForeground | GCBackground | GCFillStyle | GCFunction |
	GCSubwindowMode | GCGraphicsExposures | GCPlaneMask;

    if (CoreBackgroundPixmap(XtParent(w)) != None &&
	CoreBackgroundPixmap(XtParent(w)) != XmUNSPECIFIED_PIXMAP)
    {
	/* we're dealing with a pixmap'ed background */
	mask |= GCTile;

	values.tile = CoreBackgroundPixmap(XtParent(w));
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
    values.foreground = XmParentBackground(w);
    values.background = XmParentForeground(w);

    PBG_BackgroundGC(w) = XtGetGC(w, mask, &values);
}

#if 0
static void
initialize_prehook(Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "PushBG InitializePrehook\n"));
}
#endif

static void
initialize_posthook(Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    XmWidgetExtData ext;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "PushBG InitializePosthook\n"));

    /* don't let the null fool you */
    LabG_Cache(new_w) = (XmLabelGCacheObjPart *)
	_XmCachePart(LabG_ClassCachePart(NULL),
		     (XtPointer)LabG_Cache(new_w),
		     sizeof(XmLabelGCacheObjPart));
    PBG_Cache(new_w) = (XmPushButtonGCacheObjPart *)
	_XmCachePart(PBG_ClassCachePart(NULL),
		     (XtPointer)PBG_Cache(new_w),
		     sizeof(XmPushButtonGCacheObjPart));

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);
    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree((XtPointer)ext->reqWidget);
    XtFree((char *)ext);
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Dimension margin, width, height;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "PushBG Initialize\n"));

    if (!XtIsSubclass(XtParent(new_w), xmManagerWidgetClass))
    {
	_XmError(new_w, "parent should be manager.");
    }

    /* check the RepType resources */
    if (PBG_MultiClick(new_w) == (unsigned char)XmUNSPECIFIED ||
	!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick),
			     PBG_MultiClick(new_w), new_w))
    {
	PBG_MultiClick(new_w) =
	    IN_MENU(new_w) ? XmMULTICLICK_DISCARD : XmMULTICLICK_KEEP;
    }

    PBG_Armed(new_w) = False;

    CreateFillGC(new_w);
    CreateBackgroundGC(new_w);

    if (LabG_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	PBG_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	LabG_Pixmap(new_w) = PBG_ArmPixmap(new_w);
    }

    PBG_UnarmPixmap(new_w) = LabG_Pixmap(new_w);

    if (XtSensitive(new_w) && LabG_IsPixmap(new_w) &&
	PBG_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	_XmLabelGetPixmapSize(new_w, PBG_ArmPixmap(new_w), &width, &height);
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
		xmLabelGadgetClassRec.rect_class.resize(new_w);
	    }
	}
    }

    if (IN_MENU(new_w))
    {
	LabG_Highlight(new_w) = 0;
	/* FIX ME - I just added the line below because I think it belongs here;
	 * not sure if it's right though. Danny 18/5/1996 */
	G_TraversalOn(new_w) = True;
	LabGClass_MenuProcs(XtClass(new_w)) = MenuProcEntry;
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
	PBG_Compatible(new_w) = !PBG_DefaultButtonShadow(new_w);

	if (PBG_Compatible(new_w))
	{
	    PBG_DefaultButtonShadow(new_w) = PBG_ShowAsDefault(new_w);
	}

	if (PBG_DefaultButtonShadow(new_w))
	{
	    margin = (PBG_DefaultButtonShadow(new_w) << 1) + LabG_Shadow(new_w)
		+ Xm3D_ENHANCE_PIXEL;

	    LabG_MarginLeft(new_w) += margin;
	    LabG_MarginRight(new_w) += margin;
	    LabG_MarginTop(new_w) += margin;
	    LabG_MarginBottom(new_w) += margin;

	    XtWidth(new_w) += margin << 1;
	    XtHeight(new_w) += margin << 1;

	    LabG_TextRect_x(new_w) += margin;
	    LabG_TextRect_y(new_w) += margin;
	    if (LabG_AcceleratorText(new_w) != NULL) {
		LabG_AccTextRect(new_w).x += margin;
		LabG_AccTextRect(new_w).y += margin;
	    }
	}
    }

    G_EventMask(new_w) = XmARM_EVENT | XmACTIVATE_EVENT | XmENTER_EVENT |
	XmLEAVE_EVENT | XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT |
	XmMULTI_ARM_EVENT | XmMULTI_ACTIVATE_EVENT |
	XmHELP_EVENT | XmBDRAG_EVENT;

    PBG_Timer(new_w) = 0;
}

static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "PushBG Destroy\n"));

    if (PBG_Timer(w) != 0)
    {
	XtRemoveTimeOut(PBG_Timer(w));
	PBG_Timer(w) = 0;
    }

    XtReleaseGC(w, PBG_FillGC(w));
    XtReleaseGC(w, PBG_BackgroundGC(w));
    _XmCacheDelete((XtPointer)PBG_Cache(w));
}

static Boolean
set_values_prehook(Widget old, Widget request, Widget new_w,
		   ArgList args, Cardinal *num_args)
{
    XmBaseClassExt *bce;
    XmWidgetExtData ed;
    Cardinal size;
    XtPointer nsec, rsec;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "PushBG set_values_prehook\n"));

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
    memcpy(&((XmPushButtonGCacheObject)nsec)->pushbutton_cache,
           PBG_Cache(new_w),
	   sizeof(XmPushButtonGCacheObjPart));

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

    LabG_Cache(new_w) = &(((XmPushButtonGCacheObject)nsec)->label_cache);
    LabG_Cache(request) = &(((XmPushButtonGCacheObject)rsec)->label_cache);
    PBG_Cache(new_w) = &(((XmPushButtonGCacheObject)nsec)->pushbutton_cache);
    PBG_Cache(request) = &(((XmPushButtonGCacheObject)rsec)->pushbutton_cache);

    _XmExtImportArgs((Widget)nsec, args, num_args);

    return False;
}

static Boolean
set_values_posthook(Widget old, Widget request, Widget new_w,
		    ArgList args, Cardinal *num_args)
{
    XmWidgetExtData ext;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "PushBG set_values_posthook\n"));

    /* Gaaaa.  Forgot the CachePart copy!!! */
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

    if (!_XmPushBCacheCompare((XtPointer)PBG_Cache(new_w),
			      (XtPointer)PBG_Cache(old)))
    {

	_XmCacheDelete((XtPointer)PBG_Cache(old));

	PBG_Cache(new_w) = (XmPushButtonGCacheObjPart *)
	    _XmCachePart(PBG_ClassCachePart(NULL),
			 (XtPointer)PBG_Cache(new_w),
			 sizeof(XmPushButtonGCacheObjPart));
    }
    else
    {
	PBG_Cache(new_w) = PBG_Cache(old);
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
	Dimension	margin, width, height;
	XGCValues	gcv;

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

    if (new_w->core.being_destroyed)
    {
	return(False);
    }
    /* check the RepType resources */
    if (!XmRepTypeValidValue(XmRepTypeGetId(XmRMultiClick),
			     PBG_MultiClick(new_w), new_w))
	PBG_MultiClick(new_w) = PBG_MultiClick(old);

    /* Fix up GCs for new colors */
    if (PBG_ArmColor(new_w) != PBG_ArmColor(old))
    {
	XtReleaseGC(new_w, PBG_FillGC(new_w));
	CreateFillGC(new_w);
	refresh_needed = True;
    }
	/* See bug #772755 : need to query the old widget's values */
	XGetGCValues(XtDisplayOfObject(old), LabG_NormalGC(old), GCBackground, &gcv);
/*
    if (XmParentBackground(new_w) != XmParentBackground(old) ||
 */
	if (XmParentBackground(new_w) != gcv.background ||
	    CoreBackgroundPixmap(XtParent(new_w)) != CoreBackgroundPixmap(XtParent(old)))
	{
		XtReleaseGC(new_w, PBG_BackgroundGC(new_w));
		CreateBackgroundGC(new_w);
		refresh_needed = True;
	}

    if (IN_MENU(new_w))
    {
	/* Menu: highlight must be zero */
	LabG_Highlight(new_w) = 0;
    }
    else
    {
	/* Non-Menu: deal with default shadows */
	if (PBG_DefaultButtonShadow(new_w) != PBG_DefaultButtonShadow(old))
	{
	    PBG_Compatible(new_w) = False;
	}

	if (PBG_Compatible(new_w))
	{
	    PBG_DefaultButtonShadow(new_w) = PBG_ShowAsDefault(new_w);
	}

	if (PBG_DefaultButtonShadow(new_w) != PBG_DefaultButtonShadow(old))
	{
	    /*
	     * If PBG_DefaultButtonShadow is zero then we're not the default
	     * button (right?) so we want to undo the margin hack to get the
	     * label's clipping area correct again.  Blah! default buttons
	     * are a pain.  See pushbg/test8 (note:  this test shows two bugs,
	     * setting margin_extra to zero will fix one of them, the resize
	     * problem may or may not be fixed when you read this).
	     *
	     *	--mu@trends.net, 1998.08.06
	     */
	    margin = (PBG_DefaultButtonShadow(new_w) -
		      PBG_DefaultButtonShadow(old)) << 1;
	    if (PBG_DefaultButtonShadow(new_w) &&
		!PBG_DefaultButtonShadow(old))
		margin += LabG_Shadow(new_w) + Xm3D_ENHANCE_PIXEL;
	    else if (!PBG_DefaultButtonShadow(new_w) &&
		     PBG_DefaultButtonShadow(old))
		margin -= LabG_Shadow(old) + Xm3D_ENHANCE_PIXEL;

	    LabG_MarginLeft(new_w) += margin;
	    LabG_MarginRight(new_w) += margin;
	    LabG_MarginTop(new_w) += margin;
	    LabG_MarginBottom(new_w) += margin;

	    XtWidth(new_w) += margin << 1;
	    XtHeight(new_w) += margin << 1;

	    LabG_TextRect_x(new_w) += margin;
	    LabG_TextRect_y(new_w) += margin;
	    if (LabG_AcceleratorText(new_w)) {
		LabG_AccTextRect(new_w).x += margin;
		LabG_AccTextRect(new_w).y += margin;
	    }

	    refresh_needed = True;
	}
    }

    /* Changes to the armed or unarmed pixmap */
    if (LabG_Pixmap(new_w) == XmUNSPECIFIED_PIXMAP &&
	PBG_ArmPixmap(new_w) != XmUNSPECIFIED_PIXMAP)
    {
	LabG_Pixmap(new_w) = PBG_ArmPixmap(new_w);

	if (LabG_IsPixmap(new_w))
	    refresh_needed = True;
    }

    if (LabG_Pixmap(new_w) != LabG_Pixmap(old))
	PBG_UnarmPixmap(new_w) = LabG_Pixmap(new_w);

    if (LabG_IsPixmap(new_w))
    {
	LabG_Pixmap(new_w) = PBG_Armed(new_w)
	    ? PBG_ArmPixmap(new_w)
	    : PBG_UnarmPixmap(new_w);

	if (XtSensitive(new_w) &&
	    (LabG_RecomputeSize(new_w) ||
	     LabG_Pixmap(request) != LabG_Pixmap(old) ||
	     PBG_ArmPixmap(new_w) != PBG_ArmPixmap(old)))
	{
	    _XmLabelGetPixmapSize(new_w, PBG_ArmPixmap(new_w),
				  &width, &height);
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
		    xmLabelGadgetClassRec.rect_class.resize(new_w);
		}

		width = XtWidth(new_w);
		height = XtHeight(new_w);
		XtWidth(new_w) = XtWidth(old);
		XtHeight(new_w) = XtHeight(old);
		xmLabelGadgetClassRec.rect_class.resize(new_w);
		XtWidth(new_w) = width;
		XtHeight(new_w) = height;
	    }
	}
    }

    /* May need to draw/erase armed state */
    if ((PBG_Armed(new_w) && PBG_FillOnArm(new_w) != PBG_FillOnArm(old))
	|| XtSensitive(new_w) != XtSensitive(old))
    {
	refresh_needed = True;
    }

    /* If only showAsDefault changed, draw/erase the shadow now */
    if (!refresh_needed && !IN_MENU(new_w) && XtIsRealized(new_w) &&
	XtIsManaged(new_w) &&
	PBG_ShowAsDefault(new_w) != PBG_ShowAsDefault(old))
    {
	Dimension inset = LabG_Highlight(new_w) + Xm3D_ENHANCE_PIXEL;

	if (PBG_ShowAsDefault(new_w))
	{
	    _XmDrawShadows(XtDisplay(new_w), XtWindow(new_w),
			   XmParentTopShadowGC(new_w),
			   XmParentBottomShadowGC(new_w),
			   XtX(new_w) + inset, XtY(new_w) + inset,
			   XtWidth(new_w) - (inset << 1),
			   XtHeight(new_w) - (inset << 1),
			   PBG_DefaultButtonShadow(new_w),
			   XmSHADOW_IN);
	}
	else
	{
	    _XmDrawSimpleHighlight(XtDisplay(new_w), XtWindow(new_w),
				   XmParentBackgroundGC(new_w),
				   XtX(new_w) + inset, XtY(new_w) + inset,
				   XtWidth(new_w) - (inset << 1),
				   XtHeight(new_w) - (inset << 1),
				   PBG_DefaultButtonShadow(new_w));
	}
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

    DEBUGOUT(_LtDebug(__FILE__, new_w, "PushBG get_values_prehook\n"));

    bce = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
    size = (*bce)->secondaryObjectClass->core_class.widget_size;

    nsec = _XmExtObjAlloc(size);

    memcpy(&((XmLabelGCacheObject)nsec)->label_cache,
           LabG_Cache(new_w),
	   sizeof(XmLabelGCacheObjPart));
    memcpy(&((XmPushButtonGCacheObject)nsec)->pushbutton_cache,
           PBG_Cache(new_w),
	   sizeof(XmPushButtonGCacheObjPart));

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

    DEBUGOUT(_LtDebug(__FILE__, new_w, "PushBG get_values_posthook\n"));

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree((XtPointer)ext->widget);

    XtFree((char *)ext);
}

#if 0
static void
get_values_hook(Widget w, ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "GetValuesHook\n"));
}
#endif

static void
expose(Widget w, XEvent *event, Region region)
{
    Dimension inset;

    DEBUGOUT(_LtDebug(__FILE__, w, "PBG expose: XY %d %d\n", XtX(w), XtY(w)));

    if (!IN_MENU(w))
    {
	inset = LabG_Highlight(w);
	if (PBG_DefaultButtonShadow(w))
	    inset += Xm3D_ENHANCE_PIXEL + LabG_Shadow(w)
		+ (PBG_DefaultButtonShadow(w) << 1);

	DEBUGOUT(_LtDebug(__FILE__, w,
			  "x %d y %d w %d h %d b %d\n",
			  inset, inset,
			  (XtWidth(w) - (inset << 1)),
			  (XtHeight(w) - (inset << 1)),
			  XtBorderWidth(w)));

	if (PBG_FillOnArm(w))
	{
	    XFillRectangle(XtDisplay(w), XtWindowOfObject(w),
			   PBG_Armed(w) ? PBG_FillGC(w) : PBG_BackgroundGC(w),
			   XtX(w) + inset + LabG_Shadow(w),
			   XtY(w) + inset + LabG_Shadow(w),
			   XtWidth(w) - ((inset + LabG_Shadow(w)) << 1),
			   XtHeight(w) - ((inset + LabG_Shadow(w)) << 1));
	}

	/* now draw the normal shadow */
	_XmDrawShadows(XtDisplay(w), XtWindowOfObject(w),
		       XmParentTopShadowGC(w), XmParentBottomShadowGC(w),
		       XtX(w) + inset, XtY(w) + inset,
		       XtWidth(w) - (inset << 1),
		       XtHeight(w) - (inset << 1),
		       LabG_Shadow(w),
		       PBG_Armed(w) ? XmSHADOW_IN : XmSHADOW_OUT);

	/* take care of the default button stuff */
	if (PBG_DefaultButtonShadow(w) && PBG_ShowAsDefault(w))
	{
	    inset = LabG_Highlight(w) + Xm3D_ENHANCE_PIXEL;
	    _XmDrawShadows(XtDisplay(w), XtWindowOfObject(w),
			   XmParentTopShadowGC(w),
			   XmParentBottomShadowGC(w),
			   XtX(w) + inset, XtY(w) + inset,
			   XtWidth(w) - (inset << 1),
			   XtHeight(w) - (inset << 1),
			   PBG_DefaultButtonShadow(w),
			   XmSHADOW_IN);
	}
    }
    else
    {
	if (PBG_Armed(w))
	{
	    _XmDrawShadows(XtDisplay(w), XtWindowOfObject(w),
			   XmParentTopShadowGC(w), XmParentBottomShadowGC(w),
			   XtX(w), XtY(w),
			   XtWidth(w), XtHeight(w),
			   LabG_Shadow(w), XmSHADOW_OUT);
	}
	else
	{
	    _XmClearBorder(XtDisplay(w), XtWindowOfObject(w),
			   XtX(w), XtY(w),
			   XtWidth(w), XtHeight(w),
			   LabG_Shadow(w));
	}
    }

    if (LabG_IsPixmap(w))
    {
	LabG_Pixmap(w) =
	    PBG_Armed(w) && PBG_ArmPixmap(w) != XmUNSPECIFIED_PIXMAP
	    ? PBG_ArmPixmap(w)
	    : PBG_UnarmPixmap(w);
    }

#define superclass (&xmLabelGadgetClassRec)
    (*superclass->rect_class.expose) (w, event, region);
#undef superclass
}

static Cardinal
get_sec_res_data(WidgetClass wc, XmSecondaryResourceData **data)
{
    /* FIX ME */

    return _XmSecondaryResourceData(&_XmPushBGRectClassExtRec,
				    data, NULL, NULL, NULL, NULL);
}

/* Like _XmFromHorizontalPixels,
 * but don't allow a nonzero value to become zero.
 */
static void
export_show_as_default(Widget w, int offset, XtArgVal *value)
{
    int converted_value;

    converted_value = XmConvertUnits(w, XmHORIZONTAL, XmPIXELS, *value,
				     G_UnitType(w));

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

    converted_value = XmConvertUnits(w, XmHORIZONTAL, G_UnitType(w),
				     *value, XmPIXELS);

    PBG_ShowAsDefault(w) = *value == !converted_value && *value
	? 1
	: converted_value;
    return XmSYNTHETIC_NONE;
}

static void
Arm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;

    /* rws 17 Mar 1998
       When the ArmAndActivate action is called focus does not go to the
       button that was armed.  This means that either ArmAndActivate does
       not call Arm or this should not be here.  Same goes with the
       ActiveChild bit at the end of Arm.
    XmProcessTraversal(w, XmTRAVERSE_CURRENT);
    */
    /* rws 28 Mar 1999
       Actually, the ActiveChild stuff is necessary, but only for the Gadget.
       It is needed for sucessfull keyboard traversal of menus.
     */

    if (!PBG_Armed(w))
    {
	PBG_Armed(w) = True;

	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, NULL);

	if (PBG_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.click_count = PBG_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PBG_ArmCallback(w),
			       (XtPointer)&cbs);
	}
	MGR_ActiveChild(XtParent(w)) = w;
    }
}

static void
Disarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;

    if (PBG_Armed(w))
    {
	PBG_Armed(w) = False;
	if (XtIsRealized(w))
	    XtClass(w)->core_class.expose(w, event, NULL);
    }

    if (PBG_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = PBG_ClickCount(w);

	XFlush(XtDisplay(w));
	XtCallCallbackList(w,
			   PBG_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    MGR_ActiveChild(XtParent(w)) = NULL;
}

static void
Activate(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;
    XButtonEvent *ev = (XButtonEvent *)event;

    DEBUGOUT(_LtDebug(__FILE__, w, "Activate\n"));
    DEBUGOUT(_LtDebug("MENU", w, "%s:Activate(%d)\n",
    	__FILE__, __LINE__
    	));

    if (PBG_Armed(w) == False)
    {
	return;
    }

    PBG_ClickCount(w) = 1;
    PBG_Armed(w) = False;

    if (XtIsRealized(w))
	XtClass(w)->core_class.expose(w, event, NULL);

    /*
     * Having this test breaks accelerators. Danny
     * MLM: Not having means PB's break if the button is released outside
     * the widget after it is armed.
     *
     * Test refined so it doesn't fail for accelerators. -- Danny
     */
    if (ev->type == KeyPress || ev->type == KeyRelease
	|| ((ev->x > XtX(w) && ev->x < XtX(w) + XtWidth(w))
	    && (ev->y > XtY(w) && ev->y < XtY(w) + XtHeight(w))))
    {

	cbs.reason = XmCR_ACTIVATE;
	cbs.event = event;
	cbs.click_count = PBG_ClickCount(w);
	if (XmIsRowColumn(XtParent(w)))
	{
	    RC_MenuMenuCallback(w, &cbs);
	}
	if (!LabG_SkipCallback(w) && PBG_ActivateCallback(w))
	{
	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PBG_ActivateCallback(w),
			       (XtPointer)&cbs);
	}
    }

#if 1
#else
    Disarm(w, event, params, num_params);
#endif
}

static void
ArmTimeout(XtPointer data, XtIntervalId *id)
{
    Widget w = (Widget)data;

    DEBUGOUT(_LtDebug(__FILE__, w, "ArmTimeout\n"));

    PBG_Timer(w) = 0;

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
    if (!LabG_SkipCallback(w) && PBG_ActivateCallback(w))
    {
	XFlush(XtDisplay(w));
	XtCallCallbackList(w,
			   PBG_ActivateCallback(w),
			   (XtPointer)&cbs);
    }
    PBG_Armed(w) = False;
    if (PBG_DisarmCallback(w))
    {
	XFlush(XtDisplay(w));
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = 1;
	XtCallCallbackList(w,
			   PBG_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    if (IN_MENU(w))
    {
    }
    else
    {
	if (PBG_Timer(w) != 0)
	{
	    XtRemoveTimeOut(PBG_Timer(w));
	    PBG_Timer(w) = 0;
	}

	PBG_Timer(w) = XtAppAddTimeOut(XtWidgetToApplicationContext(w),
				       ACTIVATE_DELAY, ArmTimeout, (XtPointer)w);
    }
}

static void
EnterWindow(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "EnterWindow()\n"));
    DEBUGOUT(_LtDebug("MENU", w, "%s:EnterWindow(%d)\n",
    	__FILE__, __LINE__
    	));

    if (!IN_MENU(w))
    {
	_XmEnterGadget(w, event, NULL, NULL);
	if (PBG_Armed(w) && XtIsRealized(w))
	{
	    XtClass(w)->core_class.expose(w, event, NULL);
	}
    }
    else
	/* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{

	    PBG_Armed(w) = True;

	    if (XtIsRealized(w))
		XtClass(w)->core_class.expose(w, event, NULL);

	    if (PBG_ArmCallback(w))
	    {
		cbs.reason = XmCR_ARM;
		cbs.event = event;
		cbs.click_count = PBG_ClickCount(w);

		XFlush(XtDisplay(w));

		XtCallCallbackList(w,
				   PBG_ArmCallback(w),
				   (XtPointer)&cbs);
	    }
	}
    }
}

static void
LeaveWindow(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    XmPushButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug("MENU", w, "%s:LeaveWindow(%d) - %s %s %s\n",
    	__FILE__, __LINE__,
    	IN_MENU(w) ? "menu" : "no-menu",
    	IN_MENU(w) && _XmGetInDragMode(w) ? "dragging" : "not-dragging",
    	IN_MENU(w) && PBG_Armed(w) ? "armed" : "not-armed"
    	));
    if (!IN_MENU(w))
    {
	_XmLeaveGadget(w, event, NULL, NULL);
	if (PBG_Armed(w) && XtIsRealized(w))
	{
	    PBG_Armed(w) = False;
	    XtClass(w)->core_class.expose(w, event, NULL);
	    PBG_Armed(w) = True;
	}
    }
    else
	/* we're in a menu */
    {
	if (_XmGetInDragMode(w))
	{
	    if (PBG_Armed(w))
	    {
		PBG_Armed(w) = False;

		if (XtIsRealized(w))
		    XtClass(w)->core_class.expose(w, event, NULL);

		if (PBG_DisarmCallback(w))
		{
		    cbs.reason = XmCR_DISARM;
		    cbs.event = event;
		    cbs.click_count = PBG_ClickCount(w);

		    XFlush(XtDisplay(w));

		    XtCallCallbackList(w,
				       PBG_DisarmCallback(w),
				       (XtPointer)&cbs);
		}
	    }
	}
    }
}

static void
ButtonUp(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    Boolean validButton, poppedUp;
    XmPushButtonCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "ButtonUp()\n"));

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

    if (!validButton)
    {
	return;
    }

    if (!PBG_Armed(w))
    {
	return;
    }

    PBG_Armed(w) = False;

    RC_MenuButtonPopdown(w, event, &poppedUp);

    _XmRecordEvent(event);

    _XmClearBorder(XtDisplay(w), XtWindowOfObject(w),
		   XtX(w), XtY(w),
		   XtWidth(w), XtHeight(w),
		   LabG_Shadow(w));

    cbs.reason = XmCR_ACTIVATE;
    cbs.event = event;
    cbs.click_count = PBG_ClickCount(w);
    if (XmIsRowColumn(XtParent(w)))
    {
	RC_MenuMenuCallback(w, &cbs);
    }
    if (!LabG_SkipCallback(w) && PBG_ActivateCallback(w))
    {
	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PBG_ActivateCallback(w),
			   (XtPointer)&cbs);
    }
    if (PBG_DisarmCallback(w))
    {
	cbs.reason = XmCR_DISARM;
	cbs.event = event;
	cbs.click_count = PBG_ClickCount(w);

	XFlush(XtDisplay(w));

	XtCallCallbackList(w,
			   PBG_DisarmCallback(w),
			   (XtPointer)&cbs);
    }

    _XmSetInDragMode(w, False);
}


static void
ButtonDown(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    /* modified from the MegaButton widget */
    int validButton;
    XmPushButtonCallbackStruct cbs;
    Boolean poppedUp;

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


    _XmDrawShadows(XtDisplay(w), XtWindowOfObject(w),
		   XmParentTopShadowGC(w), XmParentBottomShadowGC(w),
		   XtX(w), XtY(w),
		   XtWidth(w), XtHeight(w),
		   LabG_Shadow(w), XmSHADOW_OUT);

    if (!PBG_Armed(w))
    {
	PBG_Armed(w) = True;
	if (PBG_ArmCallback(w))
	{
	    cbs.reason = XmCR_ARM;
	    cbs.event = event;
	    cbs.click_count = PBG_ClickCount(w);

	    XFlush(XtDisplay(w));

	    XtCallCallbackList(w,
			       PBG_ArmCallback(w),
			       (XtPointer)&cbs);
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
input_dispatch(Widget gadget, XEvent *event, Mask event_mask)
{
    Cardinal num_params = 0;

    switch (event_mask)
    {
    case XmARM_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget, "got arm event\n"));
	if (IN_MENU(gadget))
	{
	    ButtonDown(gadget, event, NULL, &num_params);
	}
	else
	{
	    Arm(gadget, event, NULL, &num_params);
	}
	break;

    case XmMULTI_ARM_EVENT:
	if (PBG_MultiClick(gadget) == XmMULTICLICK_KEEP)
	{
	    if (IN_MENU(gadget))
	    {
		ButtonDown(gadget, event, NULL, &num_params);
	    }
	    else
	    {
		Arm(gadget, event, NULL, &num_params);
	    }
	}
	break;

    case XmACTIVATE_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget, "got activate event\n"));
	/*
	 * The test below is expanded so it also checks for the Shell.
	 * In tearoff menus, the shell will be a TransientShell, thus we'll
	 * trigger the Activate event.
	 */
	PBG_ClickCount(gadget) = 1;
	if ((LabG_MenuType(gadget) == XmMENU_PULLDOWN &&
	    XtIsSubclass(XtParent(XtParent(gadget)), xmMenuShellWidgetClass)) ||
	    LabG_MenuType(gadget) == XmMENU_POPUP)
	{
	    ButtonUp(gadget, event, NULL, &num_params);
	}
	else
	{
	    Activate(gadget, event, NULL, &num_params);
	}
	break;

    case XmMULTI_ACTIVATE_EVENT:
	if (PBG_MultiClick(gadget) == XmMULTICLICK_KEEP)
	{
	    PBG_ClickCount(gadget)++;
	    if (LabG_MenuType(gadget) == XmMENU_PULLDOWN &&
	      XtIsSubclass(XtParent(XtParent(gadget)), xmMenuShellWidgetClass))
	    {
		ButtonUp(gadget, event, NULL, &num_params);
	    }
	    else
	    {
		Activate(gadget, event, NULL, &num_params);
	    }
	}
	break;

    case XmENTER_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget, "got an enter window\n"));
	EnterWindow(gadget, event, NULL, &num_params);
	break;

    case XmLEAVE_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget, "got a leave window\n"));
	LeaveWindow(gadget, event, NULL, &num_params);
	break;

    case XmMOTION_EVENT:
	DEBUGOUT(_LtDebug(__FILE__, gadget, "got a motion event\n"));
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
	_XmWarning(gadget, "PushButtonGadget got unknown event\n");
    }
}

void
_XmClearBGCompatibility(Widget w)
{
    _XmObjectLock(w);
    PBG_Compatible(w) = False;
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
	Arm(w, NULL, NULL, &num_params);
	break;
    case XmMENU_DISARM:
	if (PBG_Armed(w))
	    Disarm(w, NULL, NULL, &num_params);
	break;
    default:
	_XmWarning(w, "%s(%d) - Invalid menuProc function", __FILE__, __LINE__);
	break;
    }

    va_end(arg_list);
}

Widget
XmCreatePushButtonGadget(Widget parent, char *name,
			 Arg *arglist, Cardinal argcount)
{
	Widget	r;
	_XmObjectLock(parent);
	DEBUGOUT(_LtDebug(__FILE__, parent, "XmCreatePushButtonGadget(%s)\n", name));
	r = XtCreateWidget(name, xmPushButtonGadgetClass, parent,
			  arglist, argcount);
	DEBUGOUT(_LtDebug2(__FILE__, parent, r, "XmCreatePushButtonGadget => %p\n", r));
	_XmObjectUnlock(parent);
	return r;
}

void _XmPushBG_TraitAddCallback(Widget w,
				XtCallbackProc cb,
				XtPointer cbp,
				Boolean set)
{
    if (set)
	XtAddCallback(w, XmNactivateCallback, cb, cbp);
    else
	XtRemoveCallback(w, XmNactivateCallback, cb, cbp);
}

/*
 * Trait functions for ContainerItem
 */
static void _XmPushBG_ContainerItemGetValues(Widget w, XmContainerItemData d)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmPushBG_ContainerItemGetValues()\n"));
}

static void _XmPushBG_ContainerItemSetValues(Widget w, XmContainerItemData d)
{       
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmPushBG_ContainerItemSetValues()\n"));
}
