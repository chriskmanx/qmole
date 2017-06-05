/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PanedW.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1995-1998 Free Software Foundation, Inc.
 * Copyright © 1997, 1998, 1999, 2000, 2001, 2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/PanedW.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/BaseClassP.h>
#include <Xm/PanedWP.h>
#include <Xm/SashP.h>
#include <Xm/SeparatorP.h>

#include <XmI/DebugUtil.h>

/*
 * INSTANCE VARIABLES
 *-------------------
 * Documented resources (public instance variables):
 *  XmNrefigureMode           (PW_RefigureMode)
 *    determines whether the panes' positions are recomputed and repositioned
 *    when programmatic changes are made to the PanedWindow.  Default (True)
 *    sets the pane children to their appropriate positions.
 *  XmNseparatorOn            (PW_SeparatorOn)
 *    determines whether a Separator is created and managed between multiple
 *    panes.  Default (True) is to create the separator.
 *  XmNmarginHeight           (PW_MarginHeight)
 *    distance between top and bottom edges of the PanedWindow and its
 *    children.  Default is 3.
 *  XmNmarginWidth            (PW_MarginWidth)
 *    distance between left and right edges of the PanedWindow and its
 *    children.  Default is 3.
 *  XmNspacing                (PW_Spacing)
 *    specifies the distance between panes.  Default is 8.
 *  XmNsashWidth              (PW_SashWidth)
 *    specifies the width of Sash children.  Default is 10.
 *  XmNsashHeight             (PW_SashHeight)
 *    specifies the height of Sash children.  Default is 10.
 *  XmNsashShadowThickness    (PW_SashShadowThickness)
 *    defined as (variable).  On color displays, the default is 2(?).
 *  XmNsashIndent             (PW_SashIndent)
 *    specifies the placement of Sash children from the left or right margins.
 *    Positive values are offset values from the left margin; negative values
 *    indicate that the sash should be offset from the right margin by the
 *    absolute value of the sash indent.  Default value is -10.
 *
 * Private Instance variables and what they do:
 *  PW_StartY
 *    mouse starting y position when moving a sash.
 *  PW_IncrementCount
 *    sash increment count (?)
 *  PW_PaneCount
 *    number of *managed* panes
 *  PW_NumSlots
 *    total number of slots in managed_children array
 *  PW_NumManagedChildren
 *    number of slots in use in managed_children array
 *  PW_ManagedChildren
 *    misleading.  This array is regenerated whenever change_managed() is
 *    invoked with a pane changing managed state; this leads to peculiar
 *    contents, sometimes.
 *  PW_RecursivelyCalled
 *    protection in change_managed() against children that aren't panes
 *    (sashes and separators)
 *  PW_ResizeAtRealize
 *    to be discovered.
 *  PW_TopPane
 *  PW_BottomPane
 *  PW_FlipGC
 *
 ***************************************************************************
 *
 * CONSTRAINT INSTANCE VARIABLES
 *------------------------------
 * Documented resources (public constraint instance variables)
 *  XmNpaneMinimum            (PWC_Min)
 *    minimum size of pane.  default is 1; can't be less than that.
 *  XmNpaneMaximum            (PWC_Max)
 *    maximum size of pane.  default is 1000; can't be less than paneMinimum.
 *  XmNallowResize            (PWC_AllowResize)
 *    allows an application to control whether a pane can request to be
 *    resized.  It is not effective until the PanedWindow and all pane
 *    children have been realized.  If false; pane geometry requests are
 *    denied (default); else pane geometry requests are honored.
 *  XmNskipAdjust             (PWC_SkipAdjust)
 *    PanedWindow should not (default:False) or should (True) automatically
 *    resize the pane.
 *  XmNpositionIndex          (PWC_PositionIndex)
 *    computed position in MGR_Children list.
 *
 * 2.0 :
 *
 *  XmNorientation
 *
 *
 * NOTE: the above constraint resources are only relevant to panes, not to Sash
 *    or Separators.
 *
 * Private constraint instance varibles:
 *  PWC_Position
 *    pane position in paned window.  Recalculated when panes are managed
 *    or unmanaged.
 *  PWC_DHeight
 *    desired height of pane
 *  PWC_DY
 *    desired Y location of pane
 *  PWC_OldDY
 *    last computed value of Y location
 *  PWC_IsPane
 *    if the child that has these constraints is in fact a pane, this variable
 *    is true.  else false.
 *  PWC_Sash
 *    sash widget associated with a pane, if child is a pane.  If child is
 *    a sash, this is a back-pointer to the associated pane child.
 *  PWC_Separator
 *    separator widget associated with a pane, if the child is a pane.  If the
 *    child is a separator, this is a back pointer to the associated pane.
 *
 * Rules:
 *
 * Testing with Motif indicates the following:
 *  o every pane has a sash and a separator.  They aren't necessarily managed,
 *    but they do get created. (FIXED)
 *  o PW_NumManagedChildren:
 *     if no panes have been unmanaged, then
 *       PW_NumManagedChildren = PW_PaneCount + ((PW_PaneCount - 1) * 2);
 *     (i.e., PW_PaneCount = 4, PW_NumManagedChildren = 10;
 *            PW_PaneCount = 3, PW_NumManagedChildren = 7;
 *            PW_PaneCount = 2, PW_NumManagedChildren = 4;
 *            PW_PaneCount = 1, PW_NumManagedChildren = 1;
 *     if panes have been unmanaged, then
 *       PW_NumManagedChildren = PW_PaneCount + ((PW_PaneCount) * 2);
 *     (i.e., PW_PaneCount = 4, PW_NumManagedChildren = 12, PW_PaneCount was 5;
 *            PW_PaneCount = 3, PW_NumManagedChildren = 9, PW_PaneCount was 4;
 *            PW_PaneCount = 2, PW_NumManagedChildren = 6, PW_PaneCount was 3;
 *            PW_PaneCount = 1, PW_NumManagedChildren = 3, PW_PaneCount was 2;
 *     so obviously the sashes and separators aren't paid attention to if the
 *     PaneCount doesn't indicate they should be.
 *     NOTE: need to check whether or not they are *really* managed.
 *     UPDATE: We just bypass this, and never have anything but true panes
 *     in our managed_children list.
 *  o Contrary to the current implementation, the sashes and separators aren't
 *    passed around, but always stay associated with the pane they're created
 *    with. (FIXED).
 *  o The insertion_position routine returns PW_PaneCount if the child created
 *    isn't a pane (meaning that the oldest panes have the highest numbered
 *    sashes and seps -- they meet in the middle at PW_PaneCount). (FIXED)
 *  o The PWC_Sash() component of a sash child is a back pointer to the pane.
 *    (FIXED).
 *  o The PWC_Separator() component of a separator child is a back pointer to
 *    the pane. (FIXED: This helps in the SashAction routine with performance)
 *  o PWC_Position and PWC_PositionIndex: the PWC_PositionIndex is what the
 *    pane wants.  PWC_Position represents the reality of pane ordering in
 *    the list of PW_PaneCount managed panes.
 *
 * Sash Movement:
 *  o Given a middle pane with PaneMinimum = 10 and PaneMaximum = 20
 *    -- if you move the sash above this pane up, movement will cease when
 *       the pane reaches PaneMaximum in height, or until the pane above
 *       reaches PaneMinimum, whichever is first.
 *    -- if you move the sash above this pane down, and the pane below is
 *       not pane minimum size, *both* separators will animate, and both
 *       panes will be resized (at least until PaneMinimum is reached).
 *    -- the opposite is also True.  That is, if you move the sash below
 *       this pane down, movement will cease when the pane reaches PaneMaximum
 *       in height.  If you move the sash below this pane up, and the Pane
 *       below is not maximum size, *both separators will animate, and both
 *       panes will be resized approaching PaneMinimum.  Movement will stop
 *       if the pane below reaches PaneMaximum 
 */
/*
 * Forward Declarations
 */
/* core */
static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request,
		       Widget new_w,
		       ArgList args,
		       Cardinal *num_args);

static void destroy(Widget w);

static Boolean set_values(Widget current,
			  Widget request,
			  Widget new_w,
			  ArgList args,
			  Cardinal *num_args);

static void resize(Widget w);

static void expose(Widget w,
		   XEvent *event,
		   Region region);

static void realize(Widget w,
		    XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);

static XtGeometryResult query_geometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *desired);

/* composite */
static void insert_child(Widget w);

static void delete_child(Widget w);

static XtGeometryResult geometry_manager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);

static void change_managed(Widget w);

/* constraint */
static void constraint_initialize(Widget request,
				  Widget new_w,
				  ArgList args,
				  Cardinal *num_args);

static Boolean constraint_set_values(Widget current,
				     Widget request,
				     Widget new_w,
				     ArgList args,
				     Cardinal *num_args);

static Cardinal panedw_insert_position(Widget w);

/* helpers */
static void _XmPanedWPreferredSize(Widget w, Widget child,
				   XtWidgetGeometry *cg,
				   XtWidgetGeometry *geo);

static void _XmPanedWLayout(Widget w, Widget instig, XtWidgetGeometry *cg,
			    XtWidgetGeometry *pref, XmKidGeometry *boxes,
			    XmKidGeometry *sashes, XmKidGeometry *seps);

/* manager */
static Boolean traversal_children(Widget mw, Widget **children, Cardinal *num_children);

/*
 * Resources for the PanedWindow class
 */
#define Offset(field) XtOffsetOf(XmPanedWindowRec, paned_window.field)

static XtResource resources[] =
{
    /* 2.0 novelty */
    {
	XmNorientation, XmCOrientation, XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmVERTICAL
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)3
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)3
    },
    {
	XmNspacing, XmCSpacing, XmRVerticalDimension,
	sizeof(Dimension), Offset(spacing),
	XmRImmediate, (XtPointer)8
    },
    {
	XmNrefigureMode, XmCBoolean, XmRBoolean,
	sizeof(Boolean), Offset(refiguremode),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNseparatorOn, XmCSeparatorOn, XmRBoolean,
	sizeof(Boolean), Offset(separator_on),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNsashIndent, XmCSashIndent, XmRHorizontalPosition,
	sizeof(Position), Offset(sash_indent),
	XmRImmediate, (XtPointer)-10
    },
    {
	XmNsashWidth, XmCSashWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(sash_width),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNsashHeight, XmCSashHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(sash_height),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNsashShadowThickness, XmCShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(sash_shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNinsertPosition, XmCInsertPosition, XmRFunction,
  sizeof(XtOrderProc), XtOffsetOf(XmPanedWindowRec, composite.insert_position),
	XmRImmediate, (XtPointer)panedw_insert_position
    }
};

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
	XmNspacing,
	sizeof(Dimension), Offset(spacing),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNsashIndent,
	sizeof(Position), Offset(sash_indent),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNsashWidth,
	sizeof(Dimension), Offset(sash_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNsashHeight,
	sizeof(Dimension), Offset(sash_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNsashShadowThickness,
	sizeof(Dimension), Offset(sash_shadow_thickness),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    }
};
#undef Offset

#define Offset(field) XtOffsetOf(XmPanedWindowConstraintRec, panedw.field)
static XtResource panedWindowConstraintResources[] =
{
    {
	XmNallowResize, XmCBoolean, XmRBoolean,
	sizeof(Boolean), Offset(allow_resize),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNpaneMinimum, XmCPaneMinimum, XmRVerticalDimension,
	sizeof(Dimension), Offset(min),
	XmRImmediate, (XtPointer)1
    },
    {
	XmNpaneMaximum, XmCPaneMaximum, XmRVerticalDimension,
	sizeof(Dimension), Offset(max),
	XmRImmediate, (XtPointer)1000
    },
    {
	XmNskipAdjust, XmCBoolean, XmRBoolean,
	sizeof(Boolean), Offset(skip_adjust),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNpositionIndex, XmCPositionIndex, XmRShort,
	sizeof(short), Offset(position_index),
	XmRImmediate, (XtPointer)XmLAST_POSITION
    }
};

static XmSyntheticResource constraint_syn_resources[] =
{
    {
	XmNpaneMinimum,
	sizeof(Dimension), Offset(min),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
    {
	XmNpaneMaximum,
	sizeof(Dimension), Offset(max),
	_XmFromVerticalPixels, _XmToVerticalPixels
    },
};

static void SashAction(Widget w, XtPointer client_data, XtPointer call_data);

static CompositeClassExtensionRec panedWCompositeExt = {
    /* next_extension */  NULL,
    /* record_type    */  NULLQUARK,
    /* version        */  XtCompositeExtensionVersion,
    /* record_size    */  sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};

static XmBaseClassExtRec _XmPanedWCoreClassExtRec = {
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
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

static XmManagerClassExtRec _XmPanedWMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ traversal_children
};

XmPanedWindowClassRec xmPanedWindowClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
        /* class_name            */ "XmPanedWindow",
	/* widget_size           */ sizeof(XmPanedWindowRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ realize,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMultiple,
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
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmPanedWCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ geometry_manager, 
        /* change_managed   */ change_managed, 
        /* insert_child     */ insert_child,
        /* delete_child     */ delete_child,
        /* extension        */ (XtPointer)&panedWCompositeExt,
    },
    /* Constraint class part */
    {
	/* subresources      */ panedWindowConstraintResources,  
        /* subresource_count */ XtNumber(panedWindowConstraintResources),
        /* constraint_size   */ sizeof(XmPanedWindowConstraintRec),
        /* initialize        */ constraint_initialize,
        /* destroy           */ NULL,
        /* set_values        */ constraint_set_values,
        /* extension         */ NULL,
    },
    /* XmManager class part */
    {
	/* translations                 */ XtInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ constraint_syn_resources,
        /* num_syn_constraint_resources */ XtNumber(constraint_syn_resources),
        /* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)&_XmPanedWMClassExtRec
    },
    /* XmPanedWindow part */
    {
	/* extension */ NULL,
    },
};


WidgetClass xmPanedWindowWidgetClass = (WidgetClass)&xmPanedWindowClassRec;

/*
 * default number of slots.  8 seems to be a reasonable number.
 */
#define DEFAULT_NUM_SLOTS	8

static void
ValidatePaneMin(Widget w)
{
    if (PWC_PaneMinimum(w) < 1)
    {
	XtWarning("PanedWindow: XmNpaneMinimum must be greater than 0.");
	PWC_PaneMinimum(w) = 1;
    }

    if (PWC_PaneMaximum(w) < PWC_PaneMinimum(w))
    {
	_XmWarning(XtParent(w),
		   "XmNpaneMinimum must be less than XmNpaneMaximum.");
	PWC_PaneMaximum(w) = PWC_PaneMinimum(w) + 1;
    }

    if (XtIsRealized(w))
    {
	if (PW_Orientation(w) == XmVERTICAL) {
	    if (XtHeight(w) < PWC_PaneMinimum(w))
            {
		XtHeight(w) = PWC_PaneMinimum(w);
            }
	} else {
	    if (XtWidth(w) < PWC_PaneMinimum(w))
            {
		XtWidth(w) = PWC_PaneMinimum(w);
            }
	}
    }
}


static void
ValidatePaneMax(Widget w)
{
    if (PWC_PaneMaximum(w) < PWC_PaneMinimum(w))
    {
	_XmWarning(XtParent(w),
		   "XmNpaneMaximum must be greater than XmNpaneMinimum.");
	PWC_PaneMinimum(w) = PWC_PaneMaximum(w) - 1;
    }

    if (XtIsRealized(w))
    {
	if (PW_Orientation(w) == XmVERTICAL) {
	    if (XtHeight(w) > PWC_PaneMaximum(w))
	    {
		    XtHeight(w) = PWC_PaneMaximum(w);
	    }
	} else {
	    if (XtWidth(w) > PWC_PaneMaximum(w))
	    {
		    XtWidth(w) = PWC_PaneMaximum(w);
	    }
	}
    }
}


static Boolean
AllPanesRealized(Widget w)
{
    int i;

    if (!XtIsRealized(w))
    {
	return False;
    }

    for (i = 0; i < PW_PaneCount(w); i++)
    {
	if (!XtIsRealized(PW_ManagedChildren(w)[i]))
	{
	    return False;
	}
    }

    return True;
}


static void
class_initialize(void)
{
    _XmPanedWCoreClassExtRec.record_type = XmQmotif;
}


static void
class_part_initialize(WidgetClass widget_class)
{
    CompositeClassExtension ext, *extptr;
    XmPanedWindowWidgetClass pclass = (XmPanedWindowWidgetClass)widget_class;

    extptr = (CompositeClassExtension *)_XmGetClassExtensionPtr((XmGenericClassExt *)&(pclass->composite_class.extension),
								NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = pclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    pclass->composite_class.extension = (XtPointer)ext;
	}
    }
    _XmFastSubclassInit(widget_class, XmPANED_WINDOW_BIT);
}


static void
CreateFlipGC(Widget w)
{
    XtGCMask careabout;
    XGCValues gcv;

    careabout = GCForeground | GCLineWidth | GCSubwindowMode | GCFunction;
    gcv.function = GXxor;
    gcv.line_width = 0;
    gcv.foreground = (1UL << DefaultDepthOfScreen(XtScreen(w))) - 1;
    gcv.subwindow_mode = IncludeInferiors;

    PW_FlipGC(w) = XtGetGC(w, careabout, &gcv);
}


static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    PW_StartY(new_w) = 0;
    PW_PaneCount(new_w) = 0;
    PW_NumSlots(new_w) = DEFAULT_NUM_SLOTS;
    PW_NumManagedChildren(new_w) = 0;
    PW_RecursivelyCalled(new_w) = False;
    PW_ResizeAtRealize(new_w) = True;
    PW_TopPane(new_w) = NULL;
    PW_BottomPane(new_w) = NULL;

    CreateFlipGC(new_w);

    PW_ManagedChildren(new_w) = (WidgetList)XtMalloc(sizeof(Widget) *
						     PW_NumSlots(new_w));
}


static void
constraint_initialize(Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "PanedWindow constraint initialize\n"));

    ValidatePaneMin(new_w);
    ValidatePaneMax(new_w);
    PWC_Position(new_w) = 0;
    PWC_DWidth(new_w) = XtWidth(new_w) < PWC_PaneMinimum(new_w)
	? PWC_PaneMinimum(new_w)
	: XtWidth(new_w);
    PWC_DHeight(new_w) = XtHeight(new_w) < PWC_PaneMinimum(new_w)
	? PWC_PaneMinimum(new_w)
	: XtHeight(new_w);
    PWC_DX(new_w) = 0;
    PWC_DY(new_w) = 0;
    PWC_OldDX(new_w) = 0;
    PWC_OldDY(new_w) = 0;
    PWC_IsPane(new_w) = False;
    PWC_Sash(new_w) = NULL;
    PWC_Separator(new_w) = NULL;
}


static void
destroy(Widget w)
{
    if (PW_ManagedChildren(w))
    {
	XtFree((String)PW_ManagedChildren(w));
    }
    XtReleaseGC(w, PW_FlipGC(w));
}


/*
 * FIX ME We don't call a relayout function anywhere yet from set_values.
 */
static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    int i, argc;
    Boolean redisplay = False;
    Arg argl[3];

    DEBUGOUT(_LtDebug(__FILE__, new_w, "PanedWindow set_valuesn"));

    if (PW_SeparatorOn(new_w) != PW_SeparatorOn(old))
    {
	for (i = 1; i < PW_PaneCount(new_w); i++)
	{
	    if (PW_SeparatorOn(new_w) &&
		PWC_PaneMinimum(PW_ManagedChildren(new_w)[i])
		!= PWC_PaneMaximum(PW_ManagedChildren(new_w)[i]))
	    {
		XtManageChild(PWC_Separator(PW_ManagedChildren(new_w)[i]));
	    }
	    else
	    {
		XtUnmanageChild(PWC_Separator(PW_ManagedChildren(new_w)[i]));
	    }
	}
    }

    if (PW_Orientation(new_w) != PW_Orientation(old)) {
	XtWidgetGeometry	geo;
	Arg			arg;

	DEBUGOUT(_LtDebug(__FILE__, new_w, "SetValues: change orientation\n"));

	/* Tell separators to change orientation too */
	XtSetArg(arg, XmNorientation,
	    (PW_Orientation(new_w) == XmVERTICAL) ? XmHORIZONTAL : XmVERTICAL);
	for (i=0; i<PW_NumManagedChildren(new_w); i++) {
	    if (PWC_Separator(PW_ManagedChildren(new_w)[i]))
		XtSetValues(PWC_Separator(PW_ManagedChildren(new_w)[i]),
			&arg, 1);
	}

	/* Relayout */
	geo.request_mode = CWWidth | CWHeight;
	geo.width = XtWidth(new_w);
	geo.height = XtHeight(new_w);
	_XmPanedWLayout(new_w, NULL, NULL, &geo, NULL, NULL, NULL);

	redisplay = True;
    }

    if (PW_MarginHeight(new_w) != PW_MarginHeight(old)
	|| PW_MarginWidth(new_w) != PW_MarginWidth(old)
	|| PW_Spacing(new_w) != PW_Spacing(old)
	|| PW_SashIndent(new_w) != PW_SashIndent(old))
    {
	redisplay = True;
    }

    if (PW_SashWidth(new_w) != PW_SashWidth(old) ||
	PW_SashHeight(new_w) != PW_SashHeight(old) ||
	PW_SashShadowThickness(new_w) != PW_SashShadowThickness(old))
    {

	argc = 0;
	if (PW_SashWidth(new_w) != PW_SashWidth(old))
	{
	    XtSetArg(argl[argc], XmNwidth, PW_SashWidth(new_w));
	    argc++;
	}
	if (PW_SashHeight(new_w) != PW_SashHeight(old))
	{
	    XtSetArg(argl[argc], XmNheight, PW_SashHeight(new_w));
	    argc++;
	}
	if (PW_SashShadowThickness(new_w) != PW_SashShadowThickness(old))
	{
	    XtSetArg(argl[argc],
		     XmNshadowThickness,
		     PW_SashShadowThickness(new_w));
	    argc++;
	}

	for (i = 0; i < MGR_NumChildren(new_w); i++)
	{

	    if (PWC_IsPane(MGR_Children(new_w)[i]))
	    {
		XtSetValues(PWC_Sash(MGR_Children(new_w)[i]), argl, argc);
	    }
	    else
	    {
		break;
	    }
	}

	redisplay = True;
    }

    if (redisplay == True && PW_RefigureMode(new_w))
    {
	XtWidgetGeometry geo;

	_XmPanedWPreferredSize(new_w, NULL, NULL, &geo);

	XtWidth(new_w) = geo.width;
	XtHeight(new_w) = geo.height;
    }

    return redisplay;
}


static void
expose(Widget w, XEvent *event, Region region)
{
    int i;

    /*
     * make sure panes are lower than sashes and separators 
     * a bit kludgy.  MLM -- it should be ok to do this only on exposes.
     */
    if (XtIsRealized(w))
    {
	for (i = 0; i < PW_PaneCount(w); i++)
	{
	    XLowerWindow(XtDisplay(w), XtWindow(PW_ManagedChildren(w)[i]));
	}
	for (; i < MGR_NumChildren(w); i++)
	{
	    if (XtIsManaged(MGR_Children(w)[i]) && XmIsSash(MGR_Children(w)[i]))
		XRaiseWindow(XtDisplay(w), XtWindow(MGR_Children(w)[i]));
	}
    }

    _XmRedisplayGadgets(w, event, region);
}


static Boolean
constraint_set_values(Widget current, Widget request, Widget new_w,
		      ArgList args, Cardinal *num_args)
{
    Widget pw = XtParent(current);
    Boolean redisplay = False;
    int i;

    DEBUGOUT(_LtDebug(__FILE__, new_w, "PanedWindow constraint_set_values\n"));

    if (!PWC_IsPane(current))
    {
	return False;
    }

    if (PWC_PaneMinimum(current) != PWC_PaneMinimum(new_w))
    {
	redisplay = True;
	ValidatePaneMin(new_w);
    }

    if (PWC_PaneMaximum(current) != PWC_PaneMaximum(new_w))
    {
	redisplay = True;
	ValidatePaneMax(new_w);
    }

    if (PWC_PaneMinimum(current) == PWC_PaneMaximum(current) &&
	PWC_Sash(current) != NULL)
    {
	XtUnmanageChild(PWC_Sash(current));
    }

    /*
     * NOTE: FIX ME
     * This recomputation needs to think about whether the position index
     * is above the range of panes; I need to check what Motif does here if
     * somebody sets the position index to something wacky.
     * MLM - Dec10'96
     */
    if (PWC_PositionIndex(current) > PWC_PositionIndex(new_w))
    {

	for (i = PWC_PositionIndex(current); i > PWC_PositionIndex(new_w); i--)
	{
	    MGR_Children(pw)[i] = MGR_Children(pw)[i - 1];
	    PWC_PositionIndex(MGR_Children(pw)[i]) = i;
	}

	MGR_Children(pw)[i] = new_w;

	redisplay = True;
    }
    else if (PWC_PositionIndex(current) < PWC_PositionIndex(new_w))
    {

	for (i = PWC_PositionIndex(current); i < PWC_PositionIndex(new_w); i++)
	{
	    MGR_Children(pw)[i] = MGR_Children(pw)[i + 1];
	    PWC_PositionIndex(MGR_Children(pw)[i]) = i;
	}

	MGR_Children(pw)[i] = new_w;

	redisplay = True;
    }

    if (PW_Orientation(pw) == XmVERTICAL) {
	if (XtHeight(new_w) != PWC_DHeight(new_w))
	{
	    PWC_DHeight(new_w) = XtHeight(new_w);
	    redisplay = True;
	}
    } else {
	if (XtWidth(new_w) != PWC_DWidth(new_w))
	{
	    PWC_DWidth(new_w) = XtWidth(new_w);
	    redisplay = True;
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, pw,
		      "constraint_set_values: after PreferredSize\n"));
    return redisplay;
}


static void
resize(Widget w)
{
    XtWidgetGeometry geo;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "PanedWindow Resize (%d %d)\n",
		      XtWidth(w), XtHeight(w)));

    geo.request_mode = CWWidth | CWHeight;
    geo.width = XtWidth(w);
    geo.height = XtHeight(w);

    _XmPanedWLayout(w, NULL, NULL, &geo, NULL, NULL, NULL);
}


static void
realize(Widget w,
	XtValueMask *value_mask,
	XSetWindowAttributes *attributes)
{
    XtWidgetGeometry geo;

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass

    DEBUGOUT(_LtDebug(__FILE__, w, "PanedWindow Realize\n"));

    _XmPanedWPreferredSize(w, NULL, NULL, &geo);

    if (_XmMakeGeometryRequest(w, &geo) == XtGeometryYes)
    {
	_XmPanedWLayout(w, NULL, NULL, &geo, NULL, NULL, NULL);
    }
    else
    {
	geo.width = XtWidth(w);
	geo.height = XtHeight(w);
	_XmPanedWLayout(w, NULL, NULL, &geo, NULL, NULL, NULL);
    }
}


/*
 * Insert panes before sashes and separators.
 */
static Cardinal
panedw_insert_position(Widget w)
{
    XmPanedWindowWidget pw = (XmPanedWindowWidget)XtParent(w);
    int cnt;

    for (cnt = 0; cnt < MGR_NumChildren(pw); cnt++)
    {
	if (!PWC_IsPane(MGR_Children(pw)[cnt]))
	    break;
    }

    if (PWC_PositionIndex(w) == XmLAST_POSITION || PWC_PositionIndex(w) < 0 ||
	PWC_PositionIndex(w) > cnt)
    {
	return cnt;
    }

    return PWC_PositionIndex(w);
}


static void
insert_child(Widget w)
{
    XmPanedWindowWidget pw = (XmPanedWindowWidget)XtParent(w);
    int i;

    if (PW_RecursivelyCalled(pw))
    {

	PWC_IsPane(w) = False;
	PWC_Sash(w) = NULL;
	PWC_Separator(w) = NULL;

#define superclass (&xmManagerClassRec)
	(*superclass->composite_class.insert_child) (w);
#undef superclass

	for (i = 0; i < MGR_NumChildren(pw); i++)
	{
	    PWC_PositionIndex(MGR_Children(pw)[i]) = i;
	}

	return;
    }

    PWC_IsPane(w) = True;

#define superclass (&xmManagerClassRec)
    (*superclass->composite_class.insert_child) (w);
#undef superclass

    PW_RecursivelyCalled(pw) = True;

    /*
     * Create separator and sash for the pane.  Don't necessarily manage it,
     * though.
     */
    PWC_Separator(w) = XtVaCreateWidget("separator", xmSeparatorWidgetClass,
					(Widget)pw,
		XmNshadowThickness,	2,
		XmNseparatorType,	XmSHADOW_ETCHED_IN,
		XmNorientation,		(PW_Orientation(pw) == XmVERTICAL)
						? XmHORIZONTAL : XmVERTICAL,
		XmNnavigationType,	XmNONE,
					NULL);

    PWC_Separator(PWC_Separator(w)) = w;
    PWC_Sash(PWC_Separator(w)) = NULL;

    PWC_Sash(w) = XtVaCreateWidget("sash", xmSashWidgetClass,
				   (Widget)pw,
				   XmNshadowThickness,
				   PW_SashShadowThickness(pw),
				   XmNheight, PW_SashHeight(pw),
				   XmNwidth, PW_SashWidth(pw),
				   NULL);
    PWC_Sash(PWC_Sash(w)) = w;
    PWC_Separator(PWC_Sash(w)) = NULL;

    XtAddCallback(PWC_Sash(w),
		  XmNcallback,
		  SashAction,
		  pw);

    PW_RecursivelyCalled(pw) = False;

    XtVaSetValues(w, XmNnavigationType, XmTAB_GROUP, NULL);
}


static void
delete_child(Widget w)
{
    /* have to make sure we delete sash and sep children if child is deleted */
    if (PWC_IsPane(w))
    {
	XtDestroyWidget(PWC_Sash(w));
	XtDestroyWidget(PWC_Separator(w));
    }

#define superclass (&xmManagerClassRec)
    (*superclass->composite_class.delete_child) (w);
#undef superclass
}


static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *desired)
{
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "QueryGeometry (request %s)\n",
		      _LtDebugWidgetGeometry2String(proposed)));

    _XmPanedWPreferredSize(w, NULL, NULL, desired);

    /* Doesn't seem to be set otherwise */
    desired->request_mode |= CWWidth | CWHeight;

    if (proposed->request_mode == 0)
    {
	return XtGeometryYes;
    }

    if ((proposed->request_mode & (CWWidth | CWHeight)) == (CWWidth | CWHeight) &&
	desired->width == XtWidth(w) && desired->height == XtHeight(w))
    {
	return XtGeometryNo;
    }

    if ((proposed->request_mode & (CWWidth | CWHeight)) == (CWWidth | CWHeight) &&
	desired->width == proposed->width &&
	desired->height == proposed->height)
    {
	return XtGeometryYes;
    }

    return XtGeometryAlmost;
}


static XtGeometryResult
geometry_manager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
    int ask = 0, good = 0;
    XtWidgetGeometry want, geo;
    Widget pw = XtParent(w);
    XmKidGeometry boxes = NULL, sashes = NULL, seps = NULL;

    DEBUGOUT(_LtDebug(__FILE__, w, "PanedW: geometry manager\n"));

    if (AllPanesRealized(pw))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "All panes realized in geom_manager\n"));
    }

    if (AllPanesRealized(pw) && !PWC_AllowResize(w) && XtIsManaged(pw))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "Not allowing resize from geom_manager\n"));

	return XtGeometryNo;
    }

#define Wants(x)	(request->request_mode & x)
    if ((Wants(CWX) && request->x != XtX(w)) ||
	(Wants(CWY) && request->y != XtY(w)))
    {
	return XtGeometryNo;
    }

    want = *request;

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "geom_mgr: child %s wants %d %d\n", XtName(w),
		      request->width, request->height));

    _XmPanedWPreferredSize(pw, w, &want, &geo);

    if (_XmMakeGeometryRequest(pw, &geo) == XtGeometryYes)
    {
	DEBUGOUT(_LtDebug(__FILE__, pw,
			  "We wanted %d %d and we got it\n",
			  geo.width, geo.height));
    }
    else
    {
	geo.width = XtWidth(pw);
	geo.height = XtHeight(pw);
	DEBUGOUT(_LtDebug(__FILE__, pw,
			  "We didn't get our request, but %d %d\n",
			  geo.width, geo.height));
    }

    _XmPanedWLayout(pw, w, &want, &geo, &boxes, &sashes, &seps);

    if (Wants(CWWidth))
    {
	ask++;
	if ((want.request_mode & CWWidth) && want.width == request->width)
	{
	    good++;
	    want.request_mode &= ~CWWidth;
	}
    }
    if (Wants(CWHeight))
    {
	ask++;
	if ((want.request_mode & CWHeight) && want.height == request->height)
	{
	    good++;
	    want.request_mode &= ~CWHeight;
	}
    }

    if (reply)
    {
	*reply = want;
    }

    if (ask == good)
    {
	if (PW_Orientation(w) == XmVERTICAL) {
	    if (request->request_mode & CWHeight)
	    {
		PWC_DHeight(w) = want.height;
	    }
	} else {
	    if (request->request_mode & CWWidth)
	    {
		PWC_DWidth(w) = want.width;
	    }
	}

	DEBUGOUT(_LtDebug(__FILE__, w,
		"Child wanted %d and that's ok\n",
		(PW_Orientation(w) == XmVERTICAL) ? want.height : want.width));

	_XmSetKidGeo(boxes, w);
	_XmSetKidGeo(seps, w);
	_XmSetKidGeo(sashes, w);

	XtFree((char *)boxes);
	XtFree((char *)seps);
	XtFree((char *)sashes);

	return XtGeometryYes;
    }

    XtFree((char *)boxes);
    XtFree((char *)seps);
    XtFree((char *)sashes);

    if (good == 0)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
		"Child wanted %d (NO)\n",
		(PW_Orientation(w) == XmVERTICAL) ? want.height : want.width));
	return XtGeometryNo;
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
		"Child wanted %d (ALMOST)\n",
		(PW_Orientation(w) == XmVERTICAL) ? want.height : want.width));
	return XtGeometryAlmost;
    }
#undef Wants
}


static int
pi_sort(const void *a, const void *b)
{
    Widget wa = *(WidgetList)a;
    Widget wb = *(WidgetList)b;

    return PWC_PositionIndex(wa) - PWC_PositionIndex(wb);
}


static void
change_managed(Widget w)
{
    int i, j;
    XtWidgetGeometry geo;

    DEBUGOUT(_LtDebug(__FILE__, w, "PanedWindow change_managed\n"));

    if (PW_RecursivelyCalled(w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "ChangeManaged was recursively called\n"));
	return;
    }

    /*
     * go through our managed list, checking to see if one of our managed
     * children has been unmanaged.
     */
    for (i = 0; i < PW_PaneCount(w); i++)
    {

	if (!XtIsManaged(PW_ManagedChildren(w)[i]))
	{

	    PW_RecursivelyCalled(w) = True;

	    XtUnmanageChild(PWC_Sash(PW_ManagedChildren(w)[i]));
	    XtUnmanageChild(PWC_Separator(PW_ManagedChildren(w)[i]));

	    PW_RecursivelyCalled(w) = False;

	    PW_NumManagedChildren(w)--;
	    PW_PaneCount(w)--;

	    if ((PW_NumManagedChildren(w) - i) > 0)
	    {
		memcpy(&PW_ManagedChildren(w)[i], &PW_ManagedChildren(w)[i + 1], 
		      sizeof(Widget) * (PW_NumManagedChildren(w) - i));
	    }
	}
    }

    /*
     * now go through the pane list and see if a previously unmanaged pane
     * has been managed.  Ugly, O(n**2) linear search, but I expect this
     * is exactly what the M*tif guys did.
     */
    for (i = 0; i < MGR_NumChildren(w); i++)
    {

	if (!PWC_IsPane(MGR_Children(w)[i]))
	{
	    break;
	}

	if (XtIsManaged(MGR_Children(w)[i]))
	{

	    for (j = 0; j < PW_PaneCount(w); j++)
	    {
		if (MGR_Children(w)[i] == PW_ManagedChildren(w)[j])
		    break;
	    }

	    /* not in list if bound met */
	    if (j == PW_PaneCount(w))
	    {

		PW_PaneCount(w)++;

		if (PW_Orientation(w) == XmVERTICAL) {
		    DEBUGOUT(_LtDebug(__FILE__, MGR_Children(w)[i],
			"Setting DHeight in change_managed to %d:"
			" was %d\n",
			XtHeight(MGR_Children(w)[i]),
			PWC_DHeight(MGR_Children(w)[i])));

		    PWC_DHeight(MGR_Children(w)[i]) =
			XtHeight(MGR_Children(w)[i]);
		} else {
		    DEBUGOUT(_LtDebug(__FILE__, MGR_Children(w)[i],
			"Setting DWidth in change_managed to %d:"
			" was %d\n",
			XtWidth(MGR_Children(w)[i]),
			PWC_DWidth(MGR_Children(w)[i])));

		    PWC_DWidth(MGR_Children(w)[i]) =
			XtWidth(MGR_Children(w)[i]);
		}

		if (PW_NumManagedChildren(w) >= PW_NumSlots(w))
		{
		    PW_NumSlots(w) *= 2;
		    PW_ManagedChildren(w) =
			(WidgetList)XtRealloc((char *)PW_ManagedChildren(w),
					      sizeof(Widget) * PW_NumSlots(w));
		}

		PW_ManagedChildren(w)[PW_NumManagedChildren(w)] =
		    MGR_Children(w)[i];
		PW_NumManagedChildren(w)++;
	    }
	}
    }

    /*
     * sort the managed children by position index
     */
    qsort((void *)PW_ManagedChildren(w), PW_NumManagedChildren(w),
	  sizeof(Widget), pi_sort);

    for (i = 0; i < PW_PaneCount(w); i++)
    {
	Widget pane;

	pane = PW_ManagedChildren(w)[i];

	PWC_Position(pane) = i;

	if (i != (PW_PaneCount(w) - 1) && !XtIsManaged(PWC_Sash(pane)))
	{

	    PW_RecursivelyCalled(w) = True;

	    if (PWC_PaneMinimum(pane) != PWC_PaneMaximum(pane))
	    {
		XtManageChild(PWC_Sash(pane));
	    }

	    if (PW_SeparatorOn(w))
	    {
		XtManageChild(PWC_Separator(pane));
	    }

	    PW_RecursivelyCalled(w) = False;
	}
	else if (i == (PW_PaneCount(w) - 1) && XtIsManaged(PWC_Sash(pane)))
	{

	    PW_RecursivelyCalled(w) = True;

	    XtUnmanageChild(PWC_Sash(PW_ManagedChildren(w)[i]));
	    XtUnmanageChild(PWC_Separator(PW_ManagedChildren(w)[i]));

	    PW_RecursivelyCalled(w) = False;
	}
	if (i == (PW_PaneCount(w) - 1) &&
	    (XtIsManaged(PWC_Sash(PW_ManagedChildren(w)[i])) ||
	    (XtIsManaged(PWC_Separator(PW_ManagedChildren(w)[i])))))
	{

	    PW_RecursivelyCalled(w) = True;

	    XtUnmanageChild(PWC_Sash(PW_ManagedChildren(w)[i]));
	    XtUnmanageChild(PWC_Separator(PW_ManagedChildren(w)[i]));

	    PW_RecursivelyCalled(w) = False;
	}
    }

    if (PW_NumManagedChildren(w))
    {
	PW_TopPane(w) = (XmPanedWindowConstraintPtr)
	    CoreConstraints(PW_ManagedChildren(w)[0]);
	PW_BottomPane(w) = (XmPanedWindowConstraintPtr)
	    CoreConstraints(PW_ManagedChildren(w)[PW_NumManagedChildren(w) - 1]);
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "Compute size in change_managed\n"));

    _XmPanedWPreferredSize(w, NULL, NULL, &geo);

    if (_XmMakeGeometryRequest(w, &geo) == XtGeometryYes)
    {
	_XmPanedWLayout(w, NULL, NULL, &geo, NULL, NULL, NULL);
    }
    else
    {
	geo.width = XtWidth(w);
	geo.height = XtHeight(w);
	_XmPanedWLayout(w, NULL, NULL, &geo, NULL, NULL, NULL);
    }

    _XmNavigChangeManaged(w);
}


static Boolean
traversal_children(Widget mw, Widget **children, Cardinal *num_children)
{
    int i, cnt;

    *num_children = PW_PaneCount(mw);
    if (PW_PaneCount(mw) > 1)
    {
	*num_children += (PW_PaneCount(mw) - 1);
    }

    if (*num_children == 0)
    {
	*children = NULL;
	return True;
    }

    /* note the + 1.  Laziness to avoid two loops */
    *children = (Widget *)XtMalloc((*num_children + 1) * sizeof(Widget));

    cnt = 0;
    for (i = 0; i < PW_PaneCount(mw); i++)
    {
	(*children)[cnt] = PW_ManagedChildren(mw)[i];
	cnt++;

	/* NOTE: we put in the last sash, but the count indicates that it
	 * should be skipped */
	(*children)[cnt] = PWC_Sash(PW_ManagedChildren(mw)[i]);
	cnt++;
    }

    return True;
}


/*
 * Preferred Size calculation in the XmVERTICAL case.
 *
 * The PanedWindow is XmVERTICAL, so children are positioned one above
 * the other, and separators (if present) are horizontal lines.
 */
static void
_XmPanedWVPreferredSize(Widget w, Widget rchild,
		       XtWidgetGeometry *childgeom, XtWidgetGeometry *pwg)
{
    Widget child;
    int i;
    Dimension curw, curh;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPanedWVPreferredSize\n"));

    /* Find widest child and compute total height of PanedWindow */
    curw = curh = 0;
    for (i = 0; i < PW_PaneCount(w); i++)
    {

	child = PW_ManagedChildren(w)[i];

	DEBUGOUT(_LtDebug2(__FILE__, w, child,
			   "_XmPanedWVPreferredSize: child wid %d ht %d "
			   "pref.ht. %d panemin %d panemax %d\n",
			   XtWidth(child), XtHeight(child),
			   PWC_DHeight(child),
			   PWC_PaneMinimum(child), PWC_PaneMaximum(child)));

	if (rchild != NULL && child == rchild && childgeom != NULL &&
	    childgeom->request_mode & CWWidth)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Using childgeom width: %d\n",
			      childgeom->width));
	    curw = curw >= childgeom->width ? curw : childgeom->width;
	}
	else
	{
	    curw = curw >= XtWidth(child) ? curw : XtWidth(child);
	}

	if (curh > 0)		/* Add spacing between panes */
	{
	    curh += PW_Spacing(w);
	}

	if (rchild != NULL && child == rchild && childgeom != NULL &&
	    childgeom->request_mode & CWHeight)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Using childgeom height: %d\n",
			      childgeom->height));
	    curh += _XmMax(childgeom->height, PWC_PaneMinimum(child));
	}
	else
	{
	    curh += _XmMax(PWC_DHeight(child), PWC_PaneMinimum(child));
	}
    }

    curh += 2 * PW_MarginHeight(w);
    curw += 2 * PW_MarginWidth(w);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmPanedWVPreferredSize: computed %d %d\n",
		      curw, curh));

    pwg->width = curw;
    pwg->height = curh;
    pwg->request_mode = CWWidth | CWHeight;
}


/*
 * Preferred Size calculation in the XmHORIZONTAL case.
 *
 * The PanedWindow is XmHORIZONTAL, so children are positioned next to
 * one another, and separators (if present) are vertical lines.
 */
static void
_XmPanedWHPreferredSize(Widget w, Widget rchild,
		       XtWidgetGeometry *childgeom, XtWidgetGeometry *pwg)
{
    Widget child;
    int i;
    Dimension curw, curh;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmPanedWPreferredSize\n"));

    /* Find widest child and compute total height of PanedWindow */
    curw = curh = 0;
    for (i = 0; i < PW_PaneCount(w); i++)
    {

	child = PW_ManagedChildren(w)[i];

	DEBUGOUT(_LtDebug2(__FILE__, w, child,
			   "_XmPanedWHPreferredSize: child wid %d ht %d "
			   "pref.ht. %d panemin %d panemax %d\n",
			   XtWidth(child), XtHeight(child),
			   PWC_DHeight(child),
			   PWC_PaneMinimum(child), PWC_PaneMaximum(child)));

	if (rchild != NULL && child == rchild && childgeom != NULL &&
	    childgeom->request_mode & CWHeight)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Using childgeom height: %d\n",
			      childgeom->height));
	    curh = curh >= childgeom->height ? curh : childgeom->height;
	}
	else
	{
	    curh = curh >= XtHeight(child) ? curh : XtHeight(child);
	}

	if (curw > 0)		/* Add spacing between panes */
	{
	    curw += PW_Spacing(w);
	}

	if (rchild != NULL && child == rchild && childgeom != NULL &&
	    childgeom->request_mode & CWWidth)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Using childgeom width: %d\n",
			      childgeom->width));
	    curw += _XmMax(childgeom->width, PWC_PaneMinimum(child));
	}
	else
	{
	    curw += _XmMax(PWC_DWidth(child), PWC_PaneMinimum(child));
	}
    }

    curh += 2 * PW_MarginHeight(w);
    curw += 2 * PW_MarginWidth(w);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmPanedWHPreferredSize: computed %d %d\n",
		      curw, curh));

    pwg->width = curw;
    pwg->height = curh;
    pwg->request_mode = CWWidth | CWHeight;
}


static void
_XmPanedWPreferredSize(Widget w, Widget rchild,
		       XtWidgetGeometry *childgeom, XtWidgetGeometry *pwg)
{
    if (PW_Orientation(w) == XmVERTICAL)
	_XmPanedWVPreferredSize(w, rchild, childgeom, pwg);
    else
	_XmPanedWHPreferredSize(w, rchild, childgeom, pwg);
}


/*
 * Layout in the XmHORIZONTAL case.
 *
 * The PanedWindow is XmHORIZONTAL, so children are positioned next to
 * one another, and separators (if present) are vertical lines.
 */
static void
_XmPanedWHLayout(Widget w, Widget rchild,
		XtWidgetGeometry *childgeom, XtWidgetGeometry *pwg,
		XmKidGeometry *boxptr, XmKidGeometry *sashptr,
		XmKidGeometry *sepptr)
{
#define SEXT(x) \
       ((int)((short)(x)))
    int i, save_mode;
    Dimension width, nw, curw, curh;
    Widget child;
    XmKidGeometry boxes, sashes, seps;
    int total_delta ;

    DEBUGOUT(_LtDebug(__FILE__, w, "Pane Horizontal Layout\n"));

    curw = pwg->width;
    curh = pwg->height;

    /* temp storage */
    boxes = (XmKidGeometry)XtCalloc(PW_PaneCount(w) + 1,
				    sizeof(XmKidGeometryRec));
    sashes = (XmKidGeometry)XtCalloc(PW_PaneCount(w) + 1,
				     sizeof(XmKidGeometryRec));
    seps = (XmKidGeometry)XtCalloc(PW_PaneCount(w) + 1,
				   sizeof(XmKidGeometryRec));

    /* ------- New ------------- */
    /* Pass 1 - each pane can be its desired width but within its PaneMaximum and PaneMinimum */
    width = 2 * PW_MarginWidth(w);     /* margins on either side */
    for (i = 0; i < PW_PaneCount(w); i++)
    {
	child = PW_ManagedChildren(w)[i];

       boxes[i].box.width = PWC_DWidth(child);
       if (SEXT(boxes[i].box.width) > SEXT(PWC_PaneMaximum(child)))
           boxes[i].box.width = PWC_PaneMaximum(child) ;
       else if (SEXT(boxes[i].box.width) < SEXT(PWC_PaneMinimum(child)))
           boxes[i].box.width = PWC_PaneMinimum(child) ;
       width += boxes[i].box.width + PW_Spacing(w) ;
    }
    width -= PW_Spacing(w) ;  /* no need for last spacing */
    total_delta = curw - width ;       /* adjustment needed to fill out to current width */

    /* pass 2 - expand or contract each pane to make fit within curw */
    for (i = PW_PaneCount(w) - 1; i >= 0; i--)
    {
       child = PW_ManagedChildren(w)[i];

       if (PWC_SkipAdjust(child) )     /* we can't change the size of this one */
           continue;
       if (total_delta > 0)    /* need to make some pane(s) bigger */
	{
           if (SEXT(boxes[i].box.width) < SEXT(PWC_PaneMaximum(child)))        /* can grow */
           {
               if (SEXT(boxes[i].box.width) + total_delta <= SEXT(PWC_PaneMaximum(child)))
               {
                   boxes[i].box.width += total_delta;  /* can absorb full delta, so done */
                   break;
               }
               else
               {
                   /* make pane maximum, but this leaves some delta left so continue */
                   total_delta -= SEXT(PWC_PaneMaximum(child)) - SEXT(boxes[i].box.width);
                   boxes[i].box.width = PWC_PaneMaximum(child) ;
               }
           }
	}
       else if (total_delta < 0)       /* need to make some pane(s) smaller */
	{
           if (SEXT(boxes[i].box.width) > SEXT(PWC_PaneMinimum(child)))        /* allowed to shrink */
           {
               if (SEXT(boxes[i].box.width) + total_delta >= SEXT(PWC_PaneMinimum(child)))
               {
                   boxes[i].box.width += total_delta;  /* can contract by full delta, so done */
                   break;
               }
               else
               {
                   /* make pane minimum, but this leaves some delta left so continue */
                   total_delta -= SEXT(PWC_PaneMinimum(child)) - SEXT(boxes[i].box.width);
                   boxes[i].box.width = PWC_PaneMinimum(child) ;
               }
           }
	}
    }
    /* ------- End New ------------- */

    /* now we have the sizes of the panes determined and we can position
       them along with their sashes and Separators */
    width = PW_MarginWidth(w);
    for (i = 0; i < PW_PaneCount(w); i++)
    {
       child = PW_ManagedChildren(w)[i];
       nw = boxes[i].box.width ;
	boxes[i].kid = child;
	boxes[i].box.y = PW_MarginHeight(w);
	boxes[i].box.height = curh - 2 * PW_MarginHeight(w);
	boxes[i].box.x = width;
	boxes[i].box.width = nw;
	boxes[i].box.border_width = XtBorderWidth(child);

	PWC_DX(child) = width;
       width += nw;

	if (i != (PW_PaneCount(w) - 1))
	{
	    width += PW_Spacing(w) / 2;

	    if (PWC_Separator(child))
	    {
		seps[i].kid = PWC_Separator(child);
		seps[i].box.y = 0;
		seps[i].box.x = width - XtWidth(seps[i].kid) / 2;
		seps[i].box.height = curh;
		seps[i].box.width = XtWidth(seps[i].kid);
		seps[i].box.width = 2;	/* HERE */
		seps[i].box.border_width = XtBorderWidth(seps[i].kid);
	    }

	    if (PWC_Sash(child))
	    {
		sashes[i].kid = PWC_Sash(child);

		if (PW_SashIndent(w) >= 0)	/* offset from top */
			sashes[i].box.y = PW_SashIndent(w) ;
		else	/* offset from bottom */
			sashes[i].box.y = curh + PW_SashIndent(w) - PW_SashHeight(w);
		if (sashes[i].box.y < 0)	/* off the top */
			sashes[i].box.y = 0;
		if (sashes[i].box.y > curh - PW_SashHeight(w) ) /* off the bottom */
			sashes[i].box.y = curh - PW_SashHeight(w) ;

		sashes[i].box.x = width - PW_SashWidth(w) / 2;
		sashes[i].box.height = PW_SashHeight(w);
		sashes[i].box.width = PW_SashWidth(w);
		sashes[i].box.border_width = XtBorderWidth(sashes[i].kid);
	    }

	    width += PW_Spacing(w) / 2;
	}
    }

    /* rippling code no longer needed */

    /* now set childgeom */
    if (rchild && childgeom)
    {
	for (i = 0; i < PW_PaneCount(w); i++)
	{
	    child = PW_ManagedChildren(w)[i];

	    if (child == rchild)
	    {
		save_mode = childgeom->request_mode;
		*childgeom = boxes[i].box;
		childgeom->request_mode = save_mode;
	    }
	    if (i != (PW_PaneCount(w) - 1))
	    {
		if (PWC_Sash(child) == rchild)
		{
		    save_mode = childgeom->request_mode;
		    *childgeom = sashes[i].box;
		    childgeom->request_mode = save_mode;
		}
		else if (PWC_Separator(child) == rchild)
		{
		    save_mode = childgeom->request_mode;
		    *childgeom = seps[i].box;
		    childgeom->request_mode = save_mode;
		}
	    }
	}
    }

    /*
     * MLM: 7/22/97 This is dangerous, but xdir needs it.  mxgdb and ddd
     * don't seem to break with it (yet).
     */
    for (i = 0; i < PW_PaneCount(w); i++)
    {
	child = PW_ManagedChildren(w)[i];

	PWC_DWidth(child) = boxes[i].box.width;
    }

    /* update dy */
    for (i = 0; i < PW_PaneCount(w); i++)
    {
	PWC_DX(boxes[i].kid) = boxes[i].box.x;
	if (i < PW_PaneCount(w) - 1)
	{
	    PWC_DX(sashes[i].kid) = sashes[i].box.x;
	    PWC_DX(seps[i].kid) = seps[i].box.x;
	}
    }

    /* if not from geometry manager, do layout */
    if (!rchild)
    {
	_XmSetKidGeo(boxes, rchild);
	_XmSetKidGeo(seps, rchild);
	_XmSetKidGeo(sashes, rchild);

	XtFree((char *)boxes);
	XtFree((char *)seps);
	XtFree((char *)sashes);
    }
    else
    {
	*boxptr = boxes;
	*sashptr = sashes;
	*sepptr = seps;
    }
}


static void
_XmPanedWVLayout(Widget w, Widget rchild,
		XtWidgetGeometry *childgeom, XtWidgetGeometry *pwg,
		XmKidGeometry *boxptr, XmKidGeometry *sashptr,
		XmKidGeometry *sepptr)
{
#define SEXT(x) \
       ((int)((short)(x)))
    int i, save_mode;
    Dimension height, nh, curw, curh;
    Widget child;
    XmKidGeometry boxes, sashes, seps;
    int total_delta ;

    DEBUGOUT(_LtDebug(__FILE__, w, "Pane Vertical Layout\n"));

    curw = pwg->width;
    curh = pwg->height;

    /* temp storage */
    boxes = (XmKidGeometry)XtCalloc(PW_PaneCount(w) + 1,
				    sizeof(XmKidGeometryRec));
    sashes = (XmKidGeometry)XtCalloc(PW_PaneCount(w) + 1,
				     sizeof(XmKidGeometryRec));
    seps = (XmKidGeometry)XtCalloc(PW_PaneCount(w) + 1,
				   sizeof(XmKidGeometryRec));

    /* ------- New ------------- */
    /* Pass 1 - each pane can be its desired height but within its PaneMaximum and PaneMinimum */
    height = 2 * PW_MarginHeight(w);   /* margins on either side */
    for (i = 0; i < PW_PaneCount(w); i++)
    {
	child = PW_ManagedChildren(w)[i];

       boxes[i].box.height = PWC_DHeight(child);
       if (SEXT(boxes[i].box.height) > SEXT(PWC_PaneMaximum(child)))
           boxes[i].box.height = PWC_PaneMaximum(child) ;
       else if (SEXT(boxes[i].box.height) < SEXT(PWC_PaneMinimum(child)))
           boxes[i].box.height = PWC_PaneMinimum(child) ;
       height += boxes[i].box.height + PW_Spacing(w) ;
    }
    height -= PW_Spacing(w) ;  /* no need for last spacing */
    total_delta = curh - height ;      /* adjustment needed to fill out to current height */

    /* pass 2 - expand or contract each pane to make fit within curh */
    for (i = PW_PaneCount(w) - 1; i >= 0; i--)
    {
       child = PW_ManagedChildren(w)[i];

       if (PWC_SkipAdjust(child) )     /* we can't change the size of this one */
           continue;
       if (total_delta > 0)    /* need to make some pane(s) bigger */
	{
           if (SEXT(boxes[i].box.height) < SEXT(PWC_PaneMaximum(child)))       /* can grow */
           {
               if (SEXT(boxes[i].box.height) + total_delta <= SEXT(PWC_PaneMaximum(child)))
               {
                   boxes[i].box.height += total_delta; /* can absorb full delta, so done */
                   break;
               }
               else
               {
                   /* make pane maximum, but this leaves some delta left so continue */
                   total_delta -= SEXT(PWC_PaneMaximum(child)) - SEXT(boxes[i].box.height);
                   boxes[i].box.height = PWC_PaneMaximum(child) ;
               }
           }
	}
       else if (total_delta < 0)       /* need to make some pane(s) smaller */
	{
           if (SEXT(boxes[i].box.height) > SEXT(PWC_PaneMinimum(child)))       /* allowed to shrink */
           {
               if (SEXT(boxes[i].box.height) + total_delta >= SEXT(PWC_PaneMinimum(child)))
               {
                   boxes[i].box.height += total_delta; /* can contract by full delta, so done */
                   break;
               }
               else
               {
                   /* make pane minimum, but this leaves some delta left so continue */
                   total_delta -= SEXT(PWC_PaneMinimum(child)) - SEXT(boxes[i].box.height);
                   boxes[i].box.height = PWC_PaneMinimum(child) ;
               }
           }
	}
    }
    /* ------- End New ------------- */

    /* now we have the sizes of the panes determined and we can position
       them along with their sashes and Separators */
    height = PW_MarginHeight(w);

    for (i = 0; i < PW_PaneCount(w); i++)
    {
       child = PW_ManagedChildren(w)[i];

       nh = boxes[i].box.height ;

	boxes[i].kid = child;
	boxes[i].box.x = PW_MarginWidth(w);
	boxes[i].box.width = curw - 2 * PW_MarginWidth(w);
	boxes[i].box.y = height;
	boxes[i].box.height = nh;
	boxes[i].box.border_width = XtBorderWidth(child);

	PWC_DY(child) = height;
       height += nh;

	if (i != (PW_PaneCount(w) - 1))
	{
	    height += PW_Spacing(w) / 2;

	    if (PWC_Separator(child))
	    {
		seps[i].kid = PWC_Separator(child);
		seps[i].box.x = 0;
		seps[i].box.y = height - XtHeight(seps[i].kid) / 2;
		seps[i].box.width = curw;
		seps[i].box.height = XtHeight(seps[i].kid);
		seps[i].box.height = 2; /* HERE */
		seps[i].box.border_width = XtBorderWidth(seps[i].kid);
	    }

	    if (PWC_Sash(child))
	    {
		sashes[i].kid = PWC_Sash(child);

		/* technically sash_indent should be negated if XmNstringDirection is XmSTRING_DIRECTION_R_L */
		if (PW_SashIndent(w) >= 0)	/* offset from left */
			sashes[i].box.x = PW_SashIndent(w) ;
		else	/* offset from right */
			sashes[i].box.x = curw + PW_SashIndent(w) - PW_SashWidth(w);
		if (sashes[i].box.x < 0)	/* off the left side */
			sashes[i].box.x = 0;
		if (sashes[i].box.x > curw - PW_SashWidth(w) ) /* too far right */
			sashes[i].box.x = curw - PW_SashWidth(w) ;

		sashes[i].box.y = height - PW_SashHeight(w) / 2;
		sashes[i].box.width = PW_SashWidth(w);
		sashes[i].box.height = PW_SashHeight(w);
		sashes[i].box.border_width = XtBorderWidth(sashes[i].kid);
	    }

	    height += PW_Spacing(w) / 2;
	}
    }

    /* rippling code no longer needed */

    /* now set childgeom */
    if (rchild && childgeom)
    {
	for (i = 0; i < PW_PaneCount(w); i++)
	{
	    child = PW_ManagedChildren(w)[i];

	    if (child == rchild)
	    {
		save_mode = childgeom->request_mode;
		*childgeom = boxes[i].box;
		childgeom->request_mode = save_mode;
	    }
	    if (i != (PW_PaneCount(w) - 1))
	    {
		if (PWC_Sash(child) == rchild)
		{
		    save_mode = childgeom->request_mode;
		    *childgeom = sashes[i].box;
		    childgeom->request_mode = save_mode;
		}
		else if (PWC_Separator(child) == rchild)
		{
		    save_mode = childgeom->request_mode;
		    *childgeom = seps[i].box;
		    childgeom->request_mode = save_mode;
		}
	    }
	}
    }

    /*
     * MLM: 7/22/97 This is dangerous, but xdir needs it.  mxgdb and ddd
     * don't seem to break with it (yet).
     */
    for (i = 0; i < PW_PaneCount(w); i++)
    {
	child = PW_ManagedChildren(w)[i];

	PWC_DHeight(child) = boxes[i].box.height;
    }

    /* update dy */
    for (i = 0; i < PW_PaneCount(w); i++)
    {
	PWC_DY(boxes[i].kid) = boxes[i].box.y;
	if (i < PW_PaneCount(w) - 1)
	{
	    PWC_DY(sashes[i].kid) = sashes[i].box.y;
	    PWC_DY(seps[i].kid) = seps[i].box.y;
	}
    }

    /* if not from geometry manager, do layout */
    if (!rchild)
    {
	_XmSetKidGeo(boxes, rchild);
	_XmSetKidGeo(seps, rchild);
	_XmSetKidGeo(sashes, rchild);

	XtFree((char *)boxes);
	XtFree((char *)seps);
	XtFree((char *)sashes);
    }
    else
    {
	*boxptr = boxes;
	*sashptr = sashes;
	*sepptr = seps;
    }
}


static void
_XmPanedWLayout(Widget w, Widget rchild,
		XtWidgetGeometry *childgeom, XtWidgetGeometry *pwg,
		XmKidGeometry *boxptr, XmKidGeometry *sashptr,
		XmKidGeometry *sepptr)
{
    if (PW_Orientation(w) == XmVERTICAL)
	_XmPanedWVLayout(w, rchild, childgeom, pwg, boxptr, sashptr, sepptr);
    else
	_XmPanedWHLayout(w, rchild, childgeom, pwg, boxptr, sashptr, sepptr);
}


static void
MoveBorderV(Widget pw, Widget sash, int dy)
{
    Widget panea, paneb;
    XmPanedWindowConstraintPtr cons;

    /*
     * because it's a sash, we know that we have a pane above (which is the
     * back link in the sash constraints [panea]), and a pane below (which
     * appears at the next consecutive spot in the managed_children list --
     * which is panea's position + 1.  The bottom most pane never has a
     * managed sash (so we can't get here).  The top pane's sash is always
     * below the top pane.  Therefore, the pointers must always be valid!
     * If they aren't, something is broken -- but somewhere else, not here.
     */
    panea = PWC_Sash(sash);
    paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];

    for (;;)
    {

	/* Move up? */
	if (dy < 0)
	{
	    DEBUGOUT(_LtDebug(__FILE__, pw,
			      "Move up: %d %d %d %d %d\n",
			      PWC_DHeight(panea), dy, PWC_PaneMaximum(panea),
			      PWC_DHeight(paneb), PWC_PaneMinimum(paneb)));

	    if ((XtHeight(paneb) - dy) < PWC_PaneMaximum(paneb))
	    {
		DEBUGOUT(_LtDebug(__FILE__, paneb, "Widget can grow\n"));

		/*
		 * can panea absorb the change?  If so, then adjust paneb's
		 * height up, it's dy up, and decrease panea's height
		 */
		if ((XtHeight(panea) + dy) > PWC_PaneMinimum(panea))
		{
		    DEBUGOUT(_LtDebug(__FILE__, panea,
				      "Widget can shrink to compensate\n"));

		    cons = PW_BottomPane(pw);
		    XtHeight(PW_ManagedChildren(pw)[cons->panedw.position]) -=
			dy;
		    cons->panedw.dy += dy;
		    cons = (XmPanedWindowConstraintPtr)
			CoreConstraints(PW_ManagedChildren(pw)
					[cons->panedw.position - 1]);
		    while (cons != (XmPanedWindowConstraintPtr)PW_TopPane(pw))
		    {
			cons->panedw.dy += dy;
			cons = (XmPanedWindowConstraintPtr)
			    CoreConstraints(PW_ManagedChildren(pw)
					    [cons->panedw.position - 1]);
		    }
		    XtHeight(panea) += dy;
		    return;
		}
		/*
		 * can panea absorb part of the change?  If so, then adjust
		 * paneb's height as much as possible, adjust his dy as much
		 * as possible, and minimize panea's height.  If possible,
		 * pass the rest of the delta on to the parent.
		 */
		else if (XtHeight(panea) > PWC_PaneMinimum(panea))
		{
		    DEBUGOUT(_LtDebug(__FILE__, panea,
				      "Widget can partially compensate\n"));

		    dy += XtHeight(panea) - PWC_PaneMinimum(panea);

		    XtHeight(paneb) += XtHeight(panea) - PWC_PaneMinimum(panea);
		    PWC_DY(paneb) -= XtHeight(panea) - PWC_PaneMinimum(panea);

		    XtHeight(panea) = PWC_PaneMinimum(panea);

		    if (PWC_Position(panea) > 0)
		    {
			sash = PWC_Sash(PW_ManagedChildren(pw)[PWC_Position(panea) - 1]);
			panea = PWC_Sash(sash);
			paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];
			PW_TopPane(pw) = (XmPanedWindowConstraintPtr)
			    CoreConstraints(panea);
		    }
		}
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * higher pane
		 */
		else if (PWC_Position(panea) > 0)
		{
		    sash = PWC_Sash(PW_ManagedChildren(pw)[PWC_Position(panea) - 1]);
		    panea = PWC_Sash(sash);

		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				      "And we'll try to pass it off\n"));

		    PW_TopPane(pw) = (XmPanedWindowConstraintPtr)
			CoreConstraints(panea);
		}
		/*
		 * no more higher panes, bail out.
		 */
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, pw,
				      "No more panes above\n"));
		    return;
		}
	    }
	    /* paneb can't grow, try to move down */
	    else
	    {
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * lower pane
		 */
		if (PWC_Position(paneb) < (PW_PaneCount(pw) - 1))
		{
		    sash = PWC_Sash(paneb);
		    paneb = PW_ManagedChildren(pw)[PWC_Position(PWC_Sash(sash)) + 1];

		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				      "And we'll try to pass it off\n"));

		    PW_BottomPane(pw) = (XmPanedWindowConstraintPtr)
			CoreConstraints(paneb);
		}
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, paneb, "Can't grow\n"));
		    return;
		}
	    }
	}
	/* move down? */
	else if (dy > 0)
	{
	    DEBUGOUT(_LtDebug(__FILE__, pw,
			      "Move down: %d %d %d %d %d\n",
			      PWC_DHeight(panea), dy, PWC_PaneMaximum(panea),
			      PWC_DHeight(paneb), PWC_PaneMinimum(paneb)));

	    /* can panea grow? */
	    if ((XtHeight(panea) + dy) < PWC_PaneMaximum(panea))
	    {
		DEBUGOUT(_LtDebug(__FILE__, panea, "Pane can grow\n"));

		/*
		 * can paneb absorb the change?  If so, increate panea's
		 * height, increase paneb's dy, and decrease paneb's height.
		 */
		if ((XtHeight(paneb) - dy) > PWC_PaneMinimum(paneb))
		{
		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				  "And this pane can shrink to compensate\n"));

		    XtHeight(paneb) -= dy;
		    PWC_DY(paneb) += dy;

		    cons = PW_TopPane(pw);
		    XtHeight(PW_ManagedChildren(pw)[cons->panedw.position]) += dy;
		    cons = (XmPanedWindowConstraintPtr)
			CoreConstraints(PW_ManagedChildren(pw)
					[cons->panedw.position + 1]);
		    while (cons != PW_BottomPane(pw))
		    {
			cons->panedw.dy += dy;
			cons = (XmPanedWindowConstraintPtr)
			    CoreConstraints(PW_ManagedChildren(pw)
					    [cons->panedw.position + 1]);
		    }
		    return;
		}
		/*
		 * can paneb absorb part of the change? If so, increase panea's
		 * height as much as possible, increate paneb's dy as much
		 * as possible, and decrease paneb's height to the minimum.
		 * Pass the rest on to a lower pane.
		 */
		else if (XtHeight(paneb) > PWC_PaneMinimum(paneb))
		{
		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				  "And this pane can partially compensate\n"));

		    dy -= XtHeight(paneb) - PWC_PaneMinimum(paneb);

		    XtHeight(panea) += XtHeight(paneb) - PWC_PaneMinimum(paneb);
		    PWC_DY(paneb) += XtHeight(paneb) - PWC_PaneMinimum(paneb);

		    XtHeight(paneb) = PWC_PaneMinimum(paneb);

		    if (PWC_Position(paneb) < (PW_PaneCount(pw) - 1))
		    {
			sash = PWC_Sash(paneb);
			panea = PWC_Sash(sash);
			paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];
			PW_BottomPane(pw) = (XmPanedWindowConstraintPtr)
			    CoreConstraints(paneb);
		    }
		}
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * lower pane
		 */
		else if (PWC_Position(paneb) < (PW_PaneCount(pw) - 1))
		{

		    sash = PWC_Sash(paneb);
		    paneb = PW_ManagedChildren(pw)[PWC_Position(PWC_Sash(sash)) + 1];

		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				      "And we'll try to pass it off\n"));

		    PW_BottomPane(pw) = (XmPanedWindowConstraintPtr)
			CoreConstraints(paneb);
		}
		/*
		 * no more lower panes, bail out.
		 */
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, pw, "No more panes below\n"));
		    return;
		}
	    }
	    /* panea can't grow, bail out */
	    else
	    {
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * higher pane
		 */
		if (PWC_Position(panea) > 0)
		{
		    sash = PWC_Sash(PW_ManagedChildren(pw)[PWC_Position(panea) - 1]);
		    panea = PWC_Sash(sash);

		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				      "And we'll try to pass it off\n"));

		    PW_TopPane(pw) = (XmPanedWindowConstraintPtr)
			CoreConstraints(panea);
		}
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, panea, "Pane can't grow\n"));

		    return;
		}
	    }
	}
	/* no motion, bail out */
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, pw, "No motion\n"));

	    return;
	}
    }
}


static void
MoveBorderH(Widget pw, Widget sash, int dx)
{
    Widget panea, paneb;
    XmPanedWindowConstraintPtr cons;

    /*
     * because it's a sash, we know that we have a pane above (which is the
     * back link in the sash constraints [panea]), and a pane below (which
     * appears at the next consecutive spot in the managed_children list --
     * which is panea's position + 1.  The bottom most pane never has a
     * managed sash (so we can't get here).  The top pane's sash is always
     * below the top pane.  Therefore, the pointers must always be valid!
     * If they aren't, something is broken -- but somewhere else, not here.
     */
    panea = PWC_Sash(sash);
    paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];

    for (;;)
    {

	/* Move up? */
	if (dx < 0)
	{
	    DEBUGOUT(_LtDebug(__FILE__, pw,
			      "Move up: %d %d %d %d %d\n",
			      PWC_DWidth(panea), dx, PWC_PaneMaximum(panea),
			      PWC_DWidth(paneb), PWC_PaneMinimum(paneb)));

	    if ((XtWidth(paneb) - dx) < PWC_PaneMaximum(paneb))
	    {
		DEBUGOUT(_LtDebug(__FILE__, paneb, "Widget can grow\n"));

		/*
		 * can panea absorb the change?  If so, then adjust paneb's
		 * height up, it's dy up, and decrease panea's height
		 */
		if ((XtWidth(panea) + dx) > PWC_PaneMinimum(panea))
		{
		    DEBUGOUT(_LtDebug(__FILE__, panea,
				      "Widget can shrink to compensate\n"));

		    cons = PW_BottomPane(pw);
		    XtWidth(PW_ManagedChildren(pw)[cons->panedw.position]) -=
			dx;
		    cons->panedw.dx += dx;
		    cons = (XmPanedWindowConstraintPtr)
			CoreConstraints(PW_ManagedChildren(pw)
					[cons->panedw.position - 1]);
		    while (cons != (XmPanedWindowConstraintPtr)PW_TopPane(pw))
		    {
			cons->panedw.dx += dx;
			cons = (XmPanedWindowConstraintPtr)
			    CoreConstraints(PW_ManagedChildren(pw)
					    [cons->panedw.position - 1]);
		    }
		    XtWidth(panea) += dx;
		    return;
		}
		/*
		 * can panea absorb part of the change?  If so, then adjust
		 * paneb's height as much as possible, adjust his dy as much
		 * as possible, and minimize panea's height.  If possible,
		 * pass the rest of the delta on to the parent.
		 */
		else if (XtWidth(panea) > PWC_PaneMinimum(panea))
		{
		    DEBUGOUT(_LtDebug(__FILE__, panea,
				      "Widget can partially compensate\n"));

		    dx += XtWidth(panea) - PWC_PaneMinimum(panea);

		    XtWidth(paneb) += XtWidth(panea) - PWC_PaneMinimum(panea);
		    PWC_DX(paneb) -= XtWidth(panea) - PWC_PaneMinimum(panea);

		    XtWidth(panea) = PWC_PaneMinimum(panea);

		    if (PWC_Position(panea) > 0)
		    {
			sash = PWC_Sash(PW_ManagedChildren(pw)[PWC_Position(panea) - 1]);
			panea = PWC_Sash(sash);
			paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];
			PW_TopPane(pw) = (XmPanedWindowConstraintPtr)
			    CoreConstraints(panea);
		    }
		}
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * higher pane
		 */
		else if (PWC_Position(panea) > 0)
		{
		    sash = PWC_Sash(PW_ManagedChildren(pw)[PWC_Position(panea) - 1]);
		    panea = PWC_Sash(sash);

		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				      "And we'll try to pass it off\n"));

		    PW_TopPane(pw) = (XmPanedWindowConstraintPtr)
			CoreConstraints(panea);
		}
		/*
		 * no more higher panes, bail out.
		 */
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, pw,
				      "No more panes above\n"));
		    return;
		}
	    }
	    /* paneb can't grow, try to move down */
	    else
	    {
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * lower pane
		 */
		if (PWC_Position(paneb) < (PW_PaneCount(pw) - 1))
		{
		    sash = PWC_Sash(paneb);
		    paneb = PW_ManagedChildren(pw)[PWC_Position(PWC_Sash(sash)) + 1];

		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				      "And we'll try to pass it off\n"));

		    PW_BottomPane(pw) = (XmPanedWindowConstraintPtr)
			CoreConstraints(paneb);
		}
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, paneb, "Can't grow\n"));
		    return;
		}
	    }
	}
	/* move down? */
	else if (dx > 0)
	{
	    DEBUGOUT(_LtDebug(__FILE__, pw,
			      "Move down: %d %d %d %d %d\n",
			      PWC_DWidth(panea), dx, PWC_PaneMaximum(panea),
			      PWC_DWidth(paneb), PWC_PaneMinimum(paneb)));

	    /* can panea grow? */
	    if ((XtWidth(panea) + dx) < PWC_PaneMaximum(panea))
	    {
		DEBUGOUT(_LtDebug(__FILE__, panea, "Pane can grow\n"));

		/*
		 * can paneb absorb the change?  If so, increate panea's
		 * height, increase paneb's dy, and decrease paneb's height.
		 */
		if ((XtWidth(paneb) - dx) > PWC_PaneMinimum(paneb))
		{
		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				  "And this pane can shrink to compensate\n"));

		    XtWidth(paneb) -= dx;
		    PWC_DX(paneb) += dx;

		    cons = PW_TopPane(pw);
		    XtWidth(PW_ManagedChildren(pw)[cons->panedw.position]) += dx;
		    cons = (XmPanedWindowConstraintPtr)
			CoreConstraints(PW_ManagedChildren(pw)
					[cons->panedw.position + 1]);
		    while (cons != PW_BottomPane(pw))
		    {
			cons->panedw.dx += dx;
			cons = (XmPanedWindowConstraintPtr)
			    CoreConstraints(PW_ManagedChildren(pw)
					    [cons->panedw.position + 1]);
		    }
		    return;
		}
		/*
		 * can paneb absorb part of the change? If so, increase panea's
		 * height as much as possible, increate paneb's dy as much
		 * as possible, and decrease paneb's height to the minimum.
		 * Pass the rest on to a lower pane.
		 */
		else if (XtWidth(paneb) > PWC_PaneMinimum(paneb))
		{
		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				  "And this pane can partially compensate\n"));

		    dx -= XtWidth(paneb) - PWC_PaneMinimum(paneb);

		    XtWidth(panea) += XtWidth(paneb) - PWC_PaneMinimum(paneb);
		    PWC_DX(paneb) += XtWidth(paneb) - PWC_PaneMinimum(paneb);

		    XtWidth(paneb) = PWC_PaneMinimum(paneb);

		    if (PWC_Position(paneb) < (PW_PaneCount(pw) - 1))
		    {
			sash = PWC_Sash(paneb);
			panea = PWC_Sash(sash);
			paneb = PW_ManagedChildren(pw)[PWC_Position(panea) + 1];
			PW_BottomPane(pw) = (XmPanedWindowConstraintPtr)
			    CoreConstraints(paneb);
		    }
		}
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * lower pane
		 */
		else if (PWC_Position(paneb) < (PW_PaneCount(pw) - 1))
		{

		    sash = PWC_Sash(paneb);
		    paneb = PW_ManagedChildren(pw)[PWC_Position(PWC_Sash(sash)) + 1];

		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				      "And we'll try to pass it off\n"));

		    PW_BottomPane(pw) = (XmPanedWindowConstraintPtr)
			CoreConstraints(paneb);
		}
		/*
		 * no more lower panes, bail out.
		 */
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, pw, "No more panes below\n"));
		    return;
		}
	    }
	    /* panea can't grow, bail out */
	    else
	    {
		/*
		 * we can't absorb the change here.  Try to pass it off to a
		 * higher pane
		 */
		if (PWC_Position(panea) > 0)
		{
		    sash = PWC_Sash(PW_ManagedChildren(pw)[PWC_Position(panea) - 1]);
		    panea = PWC_Sash(sash);

		    DEBUGOUT(_LtDebug(__FILE__, paneb,
				      "And we'll try to pass it off\n"));

		    PW_TopPane(pw) = (XmPanedWindowConstraintPtr)
			CoreConstraints(panea);
		}
		else
		{
		    DEBUGOUT(_LtDebug(__FILE__, panea, "Pane can't grow\n"));

		    return;
		}
	    }
	}
	/* no motion, bail out */
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, pw, "No motion\n"));

	    return;
	}
    }
}


static void
SashAction(Widget w, XtPointer client_data, XtPointer call_data)
{
    int x, y, i;
    Widget pw = (Widget)client_data;
    XEvent *event = ((SashCallData)call_data)->event;
    XButtonEvent *ev = (XButtonEvent *)event;
    Widget child, separator, sash;

    /* 
     * toed: PW_TopPane and PW_BottomPane need to be set at 
     * every call in case MotionBorder changed them during the 
     * last call 
     */
    PW_TopPane(pw) = (XmPanedWindowConstraintPtr)
	CoreConstraints(PWC_Sash(w));
    PW_BottomPane(pw) = (XmPanedWindowConstraintPtr)
	CoreConstraints(PW_ManagedChildren(pw)
	    [PWC_Position(PWC_Sash(w)) + 1]);

    switch (event->type)
    {
    case ButtonPress:
	PW_StartX(pw) = ev->x;
	PW_StartY(pw) = ev->y;
	break;

    case ButtonRelease:
	for (i = 0; i < PW_PaneCount(pw); i++)
	{
	    Position x, y;
	    Dimension width, height;

	    child = PW_ManagedChildren(pw)[i];
	    if (PW_Orientation(pw) == XmVERTICAL) {
		if (PWC_DY(child) != PWC_OldDY(child))
	        {
		    XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
			0, PWC_DY(child) - PW_Spacing(pw) / 2,
			XtWidth(pw), PWC_DY(child) - PW_Spacing(pw) / 2);
		}
	    } else {
		if (PWC_DX(child) != PWC_OldDX(child))
	        {
		    XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
			PWC_DX(child) - PW_Spacing(pw) / 2, 0,
			PWC_DX(child) - PW_Spacing(pw) / 2, XtHeight(pw));
		}
	    }

	    DEBUGOUT(_LtDebug(__FILE__, child,
			      "SashAction places child %dx%d, size %d/%d\n",
			      XtX(child), PWC_DY(child),
			      XtWidth(child), XtHeight(child)));

	    /*
	     * MLM: You wouldn't think this would be necessary, would you?
	     * Guess what.  Xt "optimizes" perceived "no-change" out.  If
	     * you don't do this, expose events will not be generated, and
	     * the widgets will not redisplay.
	     */
	    if (PW_Orientation(pw) == XmVERTICAL) {
		x = XtX(child);
		width = XtWidth(child);
		height = XtHeight(child);
		y = PWC_DY(child);
		PWC_DHeight(child) = height;
	    } else {
		width = XtWidth(child);
		height = XtHeight(child);
		x = PWC_DX(child);
		y = XtY(child);
		PWC_DWidth(child) = width;
	    }

	    XtX(child) = 0;
	    XtY(child) = 0;
	    XtWidth(child) = 0;
	    XtHeight(child) = 0;

	    _XmConfigureObject(child, x, y, width, height,
			XtBorderWidth(child));

	    if (i != (PW_PaneCount(w) - 1))
	    {

		separator = PWC_Separator(child);
		if (separator)
		{
		    if (PW_Orientation(pw) == XmVERTICAL) {
			_XmConfigureObject(separator,
				0,
				PWC_DY(child) + XtHeight(child)
				+ PW_Spacing(pw) / 2 - XtHeight(separator) / 2,
				XtWidth(pw), XtHeight(separator),
				XtBorderWidth(separator));
		    } else {
			_XmConfigureObject(separator,
				PWC_DX(child) + XtWidth(child)
					+ PW_Spacing(pw) / 2
					- XtWidth(separator) / 2,
				0,
				XtWidth(separator), XtHeight(pw),
				XtBorderWidth(separator));
		    }
		}

		sash = PWC_Sash(child);
		if (sash)
		{
		    DEBUGOUT(_LtDebug(__FILE__, pw, "moving sash\n"));

		    if (PW_Orientation(pw) == XmVERTICAL) {
			Position x;

			if (PW_SashIndent(pw) >= 0)	/* offset from left */
				x = PW_SashIndent(pw) ;
			else	/* offset from right */
				x = XtWidth(pw) + PW_SashIndent(pw) - PW_SashWidth(pw);
			if (x < 0)	/* off the left */
				x = 0;
			if (x > XtWidth(pw) - PW_SashWidth(pw) ) /* off the right */
				x = XtWidth(pw) - PW_SashWidth(pw) ;
			_XmConfigureObject(sash,
				x,
				PWC_DY(child) + XtHeight(child)
				    + (PW_Spacing(pw) - PW_SashHeight(pw)) / 2,
				PW_SashWidth(pw), PW_SashHeight(pw),
				XtBorderWidth(sash));
		    } else {
			Position y;

			if (PW_SashIndent(pw) >= 0)	/* offset from top */
				y = PW_SashIndent(pw) ;
			else	/* offset from bottom */
				y = XtHeight(pw) + PW_SashIndent(pw) - PW_SashHeight(pw);
			if (y < 0)	/* off the top */
				y = 0;
			if (y > XtHeight(pw) - PW_SashHeight(pw) ) /* off the bottom */
				y = XtHeight(pw) - PW_SashHeight(pw) ;

			_XmConfigureObject(sash,
				PWC_DX(child) + XtWidth(child)
				    + (PW_Spacing(pw) - PW_SashWidth(pw)) / 2,
				y,
				PW_SashWidth(pw), PW_SashHeight(pw),
				XtBorderWidth(sash));
		    }
		}
	    }
	}

	break;

    case MotionNotify:

	if (PW_Orientation(pw) == XmVERTICAL) {
	  if ((((XMotionEvent *)event)->y + XtY(w)) < XtY(pw) ||
	      (((XMotionEvent *)event)->y + XtY(w)) > XtY(pw) + XtHeight(pw))
	  {
	      break;
	  }
	} else {
	  if ((((XMotionEvent *)event)->x + XtX(w)) < XtX(pw) ||
	      (((XMotionEvent *)event)->x + XtX(w)) > XtX(pw) + XtWidth(pw))
	  {
	      break;
	  }
	}

	for (i = 0; i < PW_PaneCount(pw); i++)
	{
	    PWC_OldDX(PW_ManagedChildren(pw)[i]) =
		PWC_DX(PW_ManagedChildren(pw)[i]);
	    PWC_OldDY(PW_ManagedChildren(pw)[i]) =
		PWC_DY(PW_ManagedChildren(pw)[i]);
	}

	x = ((XMotionEvent *)event)->x - PW_StartX(pw);
	y = ((XMotionEvent *)event)->y - PW_StartY(pw);

	PW_StartX(pw) = ((XMotionEvent *)event)->x;
	PW_StartY(pw) = ((XMotionEvent *)event)->y;

	if (PW_Orientation(pw) == XmVERTICAL)
	    MoveBorderV((Widget)pw, w, y);
	else
	    MoveBorderH((Widget)pw, w, x);

	for (i = 0; i < PW_PaneCount(pw); i++)
	{
	    child = PW_ManagedChildren(pw)[i];

	    if (PW_Orientation(pw) == XmVERTICAL) {
	      if (PWC_DY(child) != PWC_OldDY(child))
	      {
		XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
			  0, PWC_OldDY(child) - PW_Spacing(pw) / 2,
			  XtWidth(pw), PWC_OldDY(child) - PW_Spacing(pw) / 2);

		XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
			  0, PWC_DY(child) - PW_Spacing(pw) / 2,
			  XtWidth(pw), PWC_DY(child) - PW_Spacing(pw) / 2);
	      }
	    } else {
	      if (PWC_DX(child) != PWC_OldDX(child))
	      {
		XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
			  PWC_OldDX(child) - PW_Spacing(pw) / 2, 0,
			  PWC_OldDX(child) - PW_Spacing(pw) / 2, XtHeight(pw));

		XDrawLine(XtDisplay(pw), XtWindow(pw), PW_FlipGC(pw),
			  PWC_DX(child) - PW_Spacing(pw) / 2, 0,
			  PWC_DX(child) - PW_Spacing(pw) / 2, XtHeight(pw));
	      }
	    }
	}

	break;
    }
}


Widget
XmCreatePanedWindow(Widget parent, char *name,
		    Arg *argList, Cardinal argcount)
{
    return XtCreateWidget(name, xmPanedWindowWidgetClass, parent,
			  argList, argcount);
}
