/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Notebook.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1997-1998 Free Software Foundation, Inc.
 * Copyright (C) 1998-2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Notebook.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

/*
 * This is a widget whose main purpose is to do geometry management for its
 *	children. Fortunately it is rather simple geometry management, unlike
 *	some of the other Motif widgets.
 *
 * Peculiarities that you should know about :
 * - Children are not unmanaged to render them invisible. They are moved out
 *   of sight by moving them to position -10000,-10000.
 * - Layout of children is performed in several places: in Layout() and in
 *   _XmNotebookShowChild().
 * - Size of the Notebook and its children is determined by PreferredSize()
 *   which does an algorithm similar to the one in Layout(). Changes in one
 *   of these functions probably need to be reflected in the other as well.
 */
/*
 * What's TRAIT_WORKAROUND ?
 *
 * It serves to also recognise 1.2 widgets.
 * This is needed to make Notebook work right until all 1.2 widgets
 * have a 2.0 equivalent which holds all the right traits.
 *
 * XmPushButton, XmPushButtonGadget, XmArrowButton, XmArrowButtonGadget,
 * XmText, XmTextField, XmScrollBar, XmDrawnButton; and all their subclasses.
 */

/* definitions used below */
#define	LARGE_PAGE
#define	TRAIT_WORKAROUND

#include <LTconfig.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>

#include <Xm/NotebookP.h>
#include <Xm/XpmP.h>

/*
 * Traits used by Notebook
 */
#include <Xm/TraitP.h>
#include <Xm/NavigatorT.h>
#include <Xm/AccTextT.h>
#include <Xm/ActivatableT.h>

/*
 * Notebook holds (i.e. installs) this trait
 */
#include <Xm/ScrollFrameT.h>

#include <Xm/SpinB.h>
#include <Xm/TextF.h>

#ifdef	TRAIT_WORKAROUND
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/Text.h>
#include <Xm/ScrollBar.h>
#endif

#include <XmI/DebugUtil.h>


/* Forward Declarations */

static void ClassInitialize(void);

static void ClassPartInitialize(WidgetClass w_class);

static void Initialize(Widget request, Widget nw,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static void resize(Widget w);

static void expose(Widget w, XEvent *event, Region region);
static void Realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);

static XtGeometryResult QueryGeometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);

static Boolean SetValues(Widget current, Widget request, Widget nw,
			  ArgList args, Cardinal *num_args);

static XtGeometryResult GeometryManager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);

static void ChangeManaged(Widget w);
static void InsertChild(Widget w);
static void DeleteChild(Widget w);

static void ConstraintInitialize(Widget request, Widget nw,
				  ArgList args, Cardinal *num_args);
static Boolean ConstraintSetValues(Widget current, Widget request, Widget nw,
		      ArgList args, Cardinal *num_args);
static void _XmNBDefaultLastPageNumber(Widget w, int offset, XrmValue *val);
static void _XmNBChangeGC(Widget w);
static void Layout(Widget w);
static void PreferredSize(Widget nb, Widget instig, XtWidgetGeometry *ig,
		Dimension *widp, Dimension *htp);
static void _XmNBFrameBackgroundDefault(Widget w, int offset, XrmValue *val);
static void _XmNBBackPageBackgroundDefault(Widget w, int offset, XrmValue *val);

/* Actions */
static void TraverseTab(Widget w, XEvent *evp, String *par, Cardinal *npar);

/* Trait record */
static void _XmNBTraitInit(Widget, XtCallbackProc, Widget);
static Boolean _XmNBTraitGetInfo(Widget, Cardinal *, Widget **, Cardinal *);
static void _XmNBTraitAddNavigator(Widget, Widget, Mask);
static void _XmNBTraitRemoveNavigator(Widget, Widget);

static XmScrollFrameTraitRec _XmNBScrollFrameTraitRec = {
	/* version */		0,
	/* init */		_XmNBTraitInit,
	/* getInfo */		_XmNBTraitGetInfo,
	/* addNavigator */	_XmNBTraitAddNavigator,
	/* removeNavigator */	_XmNBTraitRemoveNavigator
};

/*
 * Resources for the Notebook class
 */

#define spiral_width 36
#define spiral_height 17
static char spiral_bits[] = {
 0x00,0x10,0x00,0x00,0xf0,0x00,0x10,0x00,0x00,0xf0,0x00,0x10,0x00,0x00,0xf0,
 0x00,0xff,0x01,0x00,0xf0,0xe0,0xff,0x0f,0x00,0xf0,0xf8,0xff,0x3f,0x00,0xf0,
 0xfc,0xff,0x7f,0x00,0xf0,0xfe,0x11,0xf8,0x03,0xf0,0x1f,0x10,0xc0,0x05,0xf0,
 0x0f,0x10,0xa0,0x09,0xf0,0x0f,0x10,0x20,0x09,0xf0,0x0e,0x10,0xa0,0x09,0xf0,
 0x1c,0x10,0xc0,0x04,0xf0,0x38,0x10,0x80,0x03,0xf0,0xe0,0x11,0x00,0x00,0xf0,
 0x00,0x1f,0x00,0x00,0xf0,0x00,0x10,0x00,0x00,0xf0};



/* XPM */
static char *spiral[] = {
/* width height num_colors chars_per_pixel */
"    36    17        5            1",
/* colors */
". c #000000",
"# c #989898",
"a c #c0c4c0",
"b c #c8ccc8",
"c c #f8fcf8",
/* pixels */
"cccccccccccc.aaaaaaaaaaaaaaaaaaaaaaa",
"cccccccccccc.aaaaaaaaaaaaaaaaaaaaaaa",
"cccccccccccc.aaaaaaaaaaaaaaaaaaaaaaa",
"ccccccccbbbbbbbbbaaaaaaaaaaaaaaaaaaa",
"cccccbbb.........bbbaaaaaaaaaaaaaaaa",
"cccbb...............bbaaaaaaaaaaaaaa",
"ccb......##########...baaaaaaaaaaaaa",
"cb....###ccc.aaaaaa###.b##aaaaaaaaaa",
"b...c#cccccc.aaaaaaaaa#.bcbaaaaaaaaa",
"b..c#ccccccc.aaaaaaaa#c#bccbaaaaaaaa",
"b..#cccccccc.aaaaaaaa#cc#ccbaaaaaaaa",
"cb.#cccccccc.aaaaaaaa#cb#ccbaaaaaaaa",
"ccb.#ccccccc.aaaaaaaaab#ccbaaaaaaaaa",
"cccbb#cccccc.aaaaaaaaaabbbaaaaaaaaaa",
"cccccb###ccc.aaaaaaaaaaaaaaaaaaaaaaa",
"ccccccccb###.aaaaaaaaaaaaaaaaaaaaaaa",
"cccccccccccc.aaaaaaaaaaaaaaaaaaaaaaa"
};



#define Offset(field) XtOffsetOf(XmNotebookRec, notebook.field)
static XtResource resources[] =
{
    {
	XmNbackPageForeground, XmCBackPageForeground,
	XmRPixel,
	sizeof(Pixel), Offset(back_page_foreground),
	XmRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {
	XmNbackPageNumber, XmCBackPageNumber,
	XmRInt,
	sizeof(int), Offset(back_page_number),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNbackPagePlacement, XmCBackPagePlacement,
	XmRScrollBarPlacement,	/* FIX ME */
	sizeof(unsigned char), Offset(back_page_pos),
	XmRImmediate, (XtPointer)XmBOTTOM_RIGHT	/* FIX ME should be dynamic */
    },
    {
	XmNbackPageSize, XmCBackPageSize,
	XmRDimension,
	sizeof(Dimension), Offset(back_page_size),
	XmRImmediate, (XtPointer)8
    },
    {
	XmNbindingPixmap, XmCBindingPixmap,
	XmRPixmap,
	sizeof(Pixmap), Offset(binding_pixmap),
	XmRImmediate, (XtPointer)XmUNSPECIFIED_PIXMAP
    },
    {
	XmNbindingType, XmCBindingType,
	XmRBindingType,
	sizeof(unsigned char), Offset(binding_type),
	XmRImmediate, (XtPointer)XmSPIRAL
    },
    {
	XmNbindingWidth, XmCBindingWidth,
	XmRDimension,
	sizeof(Dimension), Offset(binding_width),
       XmRImmediate, (XtPointer) spiral_width
    },
#if 1
    {
	XmNcurrentPageNumber, XmCCurrentPageNumber,
	XmRInt,
	sizeof(int), Offset(current_page_number),
	XmRImmediate, (XtPointer)1	/* FIX ME should be dynamic */
    },
#endif
    {
	XmNfirstPageNumber, XmCFirstPageNumber,
	XmRInt,
	sizeof(int), Offset(first_page_number),
	XmRImmediate, (XtPointer)1
    },
    {
	XmNlastPageNumber, XmCLastPageNumber,
	XmRInt,
	sizeof(int), Offset(last_page_number),
	XmRCallProc, (XtPointer)_XmNBDefaultLastPageNumber
    },
    {
	XmNframeBackground, XmCFrameBackground,
	XmRPixel,
	sizeof(Pixel), Offset(frame_background),
	XmRCallProc, (XtPointer)_XmNBFrameBackgroundDefault
    },
    {
	/* Keep this one AFTER XmNframeBackground */

	XmNbackPageBackground, XmCBackPageBackground,
	XmRPixel,
	sizeof(Pixel), Offset(back_page_background),
	XmRCallProc, (XtPointer)_XmNBBackPageBackgroundDefault
    },
    {
	XmNframeShadowThickness, XmCFrameShadowThickness,
	XmRDimension,
	sizeof(Dimension), Offset(shadow_thickness),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNinnerMarginHeight, XmCInnerMarginHeight,
	XmRDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNinnerMarginWidth, XmCInnerMarginWidth,
	XmRDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNminorTabSpacing, XmCMinorTabSpacing,
	XmRDimension,
	sizeof(Dimension), Offset(minor_spacing),
	XmRImmediate, (XtPointer)3
    },
    {
	XmNmajorTabSpacing, XmCMajorTabSpacing,
	XmRDimension,
	sizeof(Dimension), Offset(major_spacing),
	XmRImmediate, (XtPointer)3
    },
    {
	XmNorientation, XmCOrientation,
	XmROrientation,
	sizeof(unsigned char), Offset(orientation),
	XmRImmediate, (XtPointer)XmHORIZONTAL
    },
    {
	XmNpageChangedCallback, XmCPageChangedCallback,
	XmRCallback,
	sizeof(XtCallbackList), Offset(page_change_callback),
	XmRImmediate, (XtPointer)NULL
    }
};

static XmSyntheticResource syn_resources[] =
{
    /* FIX ME */
    {
	XmNmarginWidth,
	sizeof(Dimension), Offset(margin_width),
	_XmFromHorizontalPixels, _XmToHorizontalPixels
    },
    {
	XmNmarginHeight,
	sizeof(Dimension), Offset(margin_height),
	_XmFromVerticalPixels, _XmToVerticalPixels
    }
};
#undef	Offset

#define Offset(field) XtOffsetOf(XmNotebookConstraintRec, notebook.field)
static XtResource ConstraintResources[] = {
    {
	/*
	 * The spec says that the default should be dynamic here.
	 * We implement that in ConstraintInitialize().
	 */
	XmNnotebookChildType, XmCNotebookChildType,
	XmRNotebookChildType,
	sizeof(unsigned char), Offset(child_type),
	XmRImmediate, (XtPointer)XmNONE
    },
    {
	/*
	 * If no page number if given, the smallest unallocated page number
	 * should be generated.
	 * FIX ME didn't implement that yet.
	 */
	XmNpageNumber, XmCPageNumber,
	XmRInt,
	sizeof(int), Offset(page_number),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNresizable, XmCResizable,
	XmRBoolean,
	sizeof(Boolean), Offset(resizable),
	XmRImmediate, (XtPointer)True
#if 0
    },
    {
	/*
	 * This thing exists but is - according to the documentation -
	 * not accessible via resources.
	 */
	XmNactive, XmCActive,
	XmRBoolean,
	sizeof(Boolean), Offset(active),
	XmRImmediate, (XtPointer)False
#endif
    }
};
static XtActionsRec actions[] =
{
    {"TraverseTab", TraverseTab},
};

/* FIX ME - these should go in other files */
char _XmNotebook_defaultTranslations[] = "";
char _XmNotebook_traversalTranslations[] = "";


static XmBaseClassExtRec _XmNotebookCoreClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ NULL, /* FIXME */
    /* set_values_prehook        */ NULL, /* FIXME */
    /* initialize_posthook       */ NULL, /* FIXME */
    /* set_values_posthook       */ NULL, /* FIXME */
    /* secondary_object_class    */ NULL, /* FIXME */
    /* secondary_object_create   */ NULL, /* FIXME */
    /* get_secondary_resources   */ NULL, /* FIXME */
    /* fast_subclass             */ { 0 }, /* FIXME */
    /* get_values_prehook        */ NULL, /* FIXME */
    /* get_values_posthook       */ NULL, /* FIXME */
    /* class_part_init_prehook   */ NULL, /* FIXME */
    /* class_part_init_posthook  */ NULL, /* FIXME */
    /* ext_resources             */ NULL, /* FIXME */
    /* compiled_ext_resources    */ NULL, /* FIXME */
    /* num_ext_resources         */ 0, /* FIXME */
    /* use_sub_resources         */ False, /* FIXME */
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

static CompositeClassExtensionRec nbCompositeExt = 
{
    /* next_extension */  NULL,
    /* record_type    */  NULLQUARK,
    /* version        */  XtCompositeExtensionVersion,
    /* record_size    */  sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ True,
#if XtSpecificationRelease >= 6
    /* allows_change_managed_set */ True
#endif
};

static XmManagerClassExtRec _XmNotebookMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmNotebookClassRec xmNotebookClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
	/* class_name            */ "XmNotebook",
	/* widget_size           */ sizeof(XmNotebookRec),
	/* class_initialize      */ ClassInitialize,
	/* class_part_initialize */ ClassPartInitialize,
	/* class_inited          */ False,
	/* initialize            */ Initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ Realize,
	/* actions               */ actions,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressSeries,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ resize,
	/* expose                */ expose,
	/* set_values            */ SetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmNotebook_defaultTranslations,
	/* query_geometry        */ QueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmNotebookCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */	GeometryManager, 
        /* change_managed   */	ChangeManaged, 
        /* insert_child     */	InsertChild,
        /* delete_child     */	DeleteChild,
        /* extension        */	(XtPointer)&nbCompositeExt
    },
    /* Constraint class part */
    {
	/* subresources      */ ConstraintResources,  
        /* subresource_count */ XtNumber(ConstraintResources),     
        /* constraint_size   */ sizeof(XmNotebookConstraintRec),
        /* initialize        */ ConstraintInitialize,
        /* destroy           */ NULL,  
        /* set_values        */ ConstraintSetValues,  
        /* extension         */ NULL,  
    },
    /* XmManager class part */
    {
	/* translations                 */ _XmNotebook_traversalTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)&_XmNotebookMClassExtRec
    },
    /* XmNotebook part */
    {
	/* extension */ NULL,
    },
};



WidgetClass xmNotebookWidgetClass = (WidgetClass)&xmNotebookClassRec;

static void
ClassInitialize(void)
{
    _XmNotebookCoreClassExtRec.record_type = XmQmotif;

    if (! XmeTraitSet((XtPointer)xmNotebookWidgetClass, XmQTscrollFrame,
		(XtPointer)&_XmNBScrollFrameTraitRec)) {
	_XmWarning(NULL,
		"XmNotebook ClassInitialize: XmeTraitSet failed\n");
    } 

}


static void
ClassPartInitialize(WidgetClass widget_class)
{
    XmNotebookWidgetClass nbclass = (XmNotebookWidgetClass)widget_class;
    CompositeClassExtension ext, *extptr;

    extptr = (CompositeClassExtension *)_XmGetClassExtensionPtr(
		    (XmGenericClassExt *)&(nbclass->composite_class.extension),
								   NULLQUARK);

    if (extptr == NULL || *extptr == NULL)
    {
	ext = (CompositeClassExtension)XtNew(CompositeClassExtensionRec);
	if (ext != NULL)
	{
	    ext->next_extension = nbclass->composite_class.extension;
	    ext->record_type = NULLQUARK;
	    ext->version = XtCompositeExtensionVersion;
	    ext->record_size = sizeof(CompositeClassExtensionRec);
	    ext->accepts_objects = True;
#if XtSpecificationRelease >= 6
	    ext->allows_change_managed_set = True;
#endif
	    nbclass->composite_class.extension = (XtPointer)ext;
	}
    }
    _XmFastSubclassInit(widget_class, XmNOTEBOOK_BIT);
}


/*
 * Show the child indicated, don't show any other XmPAGE child.
 */
static void
_XmNotebookShowChild(Widget nb, Widget child, Widget status)
{
	Cardinal i;

	if (NBC_ChildType(child) != XmPAGE) {
		_XmWarning(child, "XmNotebook : this child is not an XmPAGE\n");
		return;
	}

	for (i=0; i<MGR_NumChildren(nb); i++) {
		Widget c = MGR_Children(nb)[i];
		if (NBC_ChildType(c) == XmPAGE || NBC_ChildType(c) == XmSTATUS_AREA) {
			if (NBC_Active(c)) {
				NBC_Active(c) = False;

				/* Move out of sight */
				_XmMoveObject(c, -10000, -10000);	/* FIX ME */

				DEBUGOUT(_LtDebug2(__FILE__, nb, c,
					"Move child out of sight\n"));
			}
		}
	}

	if (child) {
		NBC_Active(child) = True;
		_XmMoveObject(child, NB_BindingWidth(nb) + NB_MarginWidth(nb), 0);

		_XmConfigureObject(child,
			/* x */	NB_BindingWidth(nb) + NB_MarginWidth(nb),
			/* y */	0,
			/* w */	NB_PageWidth(nb),
			/* h */	NB_PageHeight(nb),
			/* bw */	XtBorderWidth(child));

		DEBUGOUT(_LtDebug2(__FILE__, nb, child, "Move child to %d %d sz %d %d\n",
			/* x */	NB_BindingWidth(nb) + NB_MarginWidth(nb),
			/* y */	0,
			/* w */	NB_PageWidth(nb),
			/* h */	NB_PageHeight(nb)));
	}

	if (status) {
		NBC_Active(status) = True;
		_XmMoveObject(status, NB_BindingWidth(nb) + NB_MarginWidth(nb), 0);

		_XmConfigureObject(status,
			/* x */	NB_BindingWidth(nb) + NB_MarginWidth(nb),
			/* y */	NB_PageHeight(nb),
			/* w */	NB_StatusWidth(nb),
			/* h */	NB_StatusHeight(nb),
			/* bw */	XtBorderWidth(child));

		DEBUGOUT(_LtDebug2(__FILE__, nb, status, "Move status to %d %d sz %d %d\n",
			/* x */	NB_BindingWidth(nb) + NB_MarginWidth(nb),
			/* y */	NB_PageHeight(nb),
			/* w */	NB_StatusWidth(nb),
			/* h */	NB_StatusHeight(nb)));
	}

	NB_ConstraintChild(nb) = child;
}


static void
_XmNotebookShowPage(Widget w, int page, int reason, XEvent *evp)
{
	Cardinal i;
	XmNotebookCallbackStruct	cbs;
	Widget				cl = NULL, status = 0;
	XmNavigatorTrait		t;
	XmNavigatorDataRec		nd;
	XmTwoDIntRec			ndmin, ndmax, ndval;

	NB_CurrentPageNumber(w) = page;

	/*
	 * Be sure to pick up the last page with this page number.
	 * You can read that in the M*tif specs, if you keep in mind
	 * that children are listed in creation order.
	 */
	for (i=0; i<MGR_NumChildren(w); i++) {
	    Widget c = MGR_Children(w)[i];

	    if (NBC_ChildType(c) == XmPAGE && NBC_PageNumber(c) == page) {
		cl = c;
#if 0
		DEBUGOUT(_LtDebug2(__FILE__, w, cl, "page child found\n"));
#endif
	    }

	    if (NBC_ChildType(c) == XmSTATUS_AREA && NBC_PageNumber(c) == page) {
		status = c;
#if 0
		DEBUGOUT(_LtDebug2(__FILE__, w, status, "Status child found\n"));
#endif
	    }
	}

	if (cl == NULL) {
		/* What to do here ?? FIX ME */
		DEBUGOUT(_LtDebug(__FILE__, w,
			"_XmNotebookShowPage(%d) : not found\n", page));
		return;
	}

	/*
	 * Call a callback to give the application a chance to override
	 */
	cbs.reason = reason;
	cbs.event = evp;
	cbs.page_number = page;
	cbs.page_widget = cl;
	cbs.prev_page_number = -1;			/* FIX ME */
	cbs.prev_page_widget = NULL;			/* FIX ME */

	if (reason)	/* FIX ME */
		XtCallCallbackList(w, NB_PageChangeCallback(w), (XtPointer)&cbs);

	cl = cbs.page_widget;

	/*
	 * Show the XmPAGE widget and the STATUS_AREA.
	 */
	DEBUGOUT(_LtDebug2(__FILE__, w, cl,
		"_XmNotebookShowPage nr %d\n", page));
	_XmNotebookShowChild(w, cl, status);

	/*
	 * Use trait method to update XmPAGE_SCROLLER
	 */
	if (NB_Scroller(w)) {
	    t = (XmNavigatorTrait)XmeTraitGet((XtPointer)XtClass(NB_Scroller(w)), XmQTnavigator);
	    if (t) {
		nd.valueMask = NavValue | NavMinimum | NavMaximum;
		nd.value = &ndval;
		nd.minimum = &ndmin;
		nd.maximum = &ndmax;

		ndval.x = page;
		ndmin.x = NB_FirstPageNumber(w);
		ndmax.x = NB_LastPageNumber(w);

		t->setValue(NB_Scroller(w), &nd, True);
	    }
	}
}


static void
Initialize(Widget request, Widget nw,
	   ArgList args, Cardinal *num_args)
{
    Arg	*l, al[1];

    DEBUGOUT(_LtDebug(__FILE__, nw,
		      "Notebook initialize: w/h %d %d\n",
		      XtWidth(nw), XtHeight(nw)));

    NB_ScrollerChild(nw) = NULL;

    XtSetArg(al[0], XmNnotebookChildType, XmMINOR_TAB);
    l = XtMergeArgLists(al, 1, args, *num_args);

    /* Always create these; don't manage them (yet). */
    NB_NextMajor(nw) = XtCreateWidget("MajorTabScrollerNext",
		xmArrowButtonGadgetClass, nw, args, *num_args);
    NB_PrevMajor(nw) = XtCreateWidget("MajorTabScrollerPrevious",
		xmArrowButtonGadgetClass, nw, args, *num_args);
    NB_NextMinor(nw) = XtCreateWidget("MinorTabScrollerNext",
		xmArrowButtonGadgetClass, nw, l, 1 + *num_args);
    NB_PrevMinor(nw) = XtCreateWidget("MinorTabScrollerPrevious",
		xmArrowButtonGadgetClass, nw, l, 1 + *num_args);

    NB_Scroller(nw) = NULL;

    XtFree((char *)l);

    NB_BackPageGc(nw) = NULL;
    NB_FrameGc(nw) = NULL;
    NB_BindingGc(nw) = NULL;
    NB_ForegroundGc(nw) = NULL;
    NB_BackgroundGc(nw) = NULL;
    NB_SpiralPixmap(nw) = XmUNSPECIFIED_PIXMAP;

    NB_PageWidth(nw) = 0;
    NB_PageHeight(nw) = 0;

    _XmNBChangeGC(nw);

#if 1
    /* FIX ME this must be removed when the widget works somewhat better */
    XtWidth(nw) = 100;
    XtHeight(nw) = 100;
#endif

    /*
     * The spec says this should happen in Realize() but that's a really odd
     *	and inconvenient place.
     */
    if (NB_Scroller(nw) == NULL) {
	Arg	al[2];

	NB_Scroller(nw) = XtCreateWidget("PageScroller",
		xmSpinBoxWidgetClass, nw, NULL, 0);

	/* FIX ME should we add a child to it ? */
	XtSetArg(al[0], XmNspinBoxChildType, XmNUMERIC);
	XtSetArg(al[1], XmNcolumns, 5);

	(void) XtCreateManagedWidget("TextField",
		xmTextFieldWidgetClass, NB_Scroller(nw), al, 2);

	XtManageChild(NB_Scroller(nw));

    } else {
	XmNavigatorTrait		t;
	XmNavigatorDataRec		nd;
	XmTwoDIntRec			ndmin, ndmax, ndval;
	/*
	 * Update the scroller for first/last page
	 * This also happens in SetValues().
	 */
	t = (XmNavigatorTrait)XmeTraitGet((XtPointer)XtClass(NB_Scroller(nw)), XmQTnavigator);
	if (t) {
		nd.valueMask = NavValue | NavMinimum | NavMaximum;
		nd.value = &ndval;
		nd.minimum = &ndmin;
		nd.maximum = &ndmax;

		ndval.x = 0;	/* FIX ME */
		ndmin.x = NB_FirstPageNumber(nw);
		ndmax.x = NB_LastPageNumber(nw);

		t->setValue(NB_Scroller(nw), &nd, True);
	}
    }
}


static void
destroy(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "Destroy\n"));

	XFreeColors(XtDisplay(w),
		DefaultColormapOfScreen(XtScreenOfObject(w)),
		&NB_BackPageBackground(w), 1,
		0);	/* ??? FIX ME */
}


static Boolean
SetValues(Widget old, Widget request, Widget nw,
	   ArgList args, Cardinal *num_args)
{
    Boolean	need_refresh = False,
		need_layout = False;

    DEBUGOUT(_LtDebug(__FILE__, nw, "SetValues\n"));

#define	NE(x)	(x(nw) != x(old))

    /*
     * FIX ME need to check superclass's resources that influence GC
     * also. Check _XmNBChangeGC() for complete list.
     */
    if (NE(NB_BackPageForeground) || NE(NB_BackPageBackground)
			|| NE(NB_FrameBackground)) {
		_XmNBChangeGC(nw);
		need_refresh = True;
	}

    if (NE(NB_CurrentPageNumber)) {
	_XmNotebookShowPage(nw, NB_CurrentPageNumber(nw), 0, NULL);
	need_refresh = True;
    }

    if (need_layout) {
	Layout(nw);
	need_refresh = True;
    }

    if (NE(NB_LastPageNumber)) {
	NB_DynamicLastPageNum(nw) = False;
    }

    if (NE(NB_FirstPageNumber) || NE(NB_LastPageNumber)) {
	XmNavigatorTrait		t;
	XmNavigatorDataRec		nd;
	XmTwoDIntRec			ndmin, ndmax;
	/*
	 * Use trait method to update XmPAGE_SCROLLER
	 */
	if (NB_Scroller(nw)) {
	    t = (XmNavigatorTrait)XmeTraitGet((XtPointer)XtClass(NB_Scroller(nw)), XmQTnavigator);
	    if (t) {
		nd.valueMask = NavMinimum | NavMaximum;
		nd.minimum = &ndmin;
		nd.maximum = &ndmax;

		ndmin.x = NB_FirstPageNumber(nw);
		ndmax.x = NB_LastPageNumber(nw);

		t->setValue(NB_Scroller(nw), &nd, True);
	    }
	}
    }

    need_refresh = True;	/* FIX ME */

    return need_refresh;

#undef	NE
}


static void
draw_back_pages(Widget w)
{
#ifdef LARGE_PAGE
	XDrawLine(XtDisplay(w), XtWindow(w), NB_BindingGc(w),
		XtWidth(w) - 2 * NB_MarginWidth(w) - NB_MajorWidth(w) - NB_BackPageSize(w) / 2,
		0,
		XtWidth(w) - 2 * NB_MarginWidth(w) - NB_MajorWidth(w) - NB_BackPageSize(w) / 2,
		NB_PageHeight(w) + NB_MarginHeight(w) + NB_StatusHeight(w) + NB_BackPageSize(w) / 2);
	XDrawLine(XtDisplay(w), XtWindow(w), NB_BindingGc(w),
		NB_BindingWidth(w) + NB_MarginWidth(w),
		NB_PageHeight(w) + NB_MarginHeight(w) + NB_StatusHeight(w) + NB_BackPageSize(w) / 2,
		XtWidth(w) - 2 * NB_MarginWidth(w) - NB_MajorWidth(w) - NB_BackPageSize(w) / 2,
		NB_PageHeight(w) + NB_MarginHeight(w) + NB_StatusHeight(w) + NB_BackPageSize(w) / 2);
#else
	XDrawLine(XtDisplay(w), XtWindow(w), NB_BindingGc(w),
		NB_BindingWidth(w) + 2 * NB_MarginWidth(w) + NB_PageWidth(w) + NB_BackPageSize(w) / 2,
		0,
		NB_BindingWidth(w) + 2 * NB_MarginWidth(w) + NB_PageWidth(w) + NB_BackPageSize(w) / 2,
		NB_PageHeight(w) + NB_MarginHeight(w) + NB_StatusHeight(w) + NB_BackPageSize(w) / 2);
	XDrawLine(XtDisplay(w), XtWindow(w), NB_BindingGc(w),
		NB_BindingWidth(w) + NB_MarginWidth(w),
		NB_PageHeight(w) + NB_MarginHeight(w) + NB_StatusHeight(w) + NB_BackPageSize(w) / 2,
		NB_BindingWidth(w) + NB_MarginWidth(w) + NB_PageWidth(w) + NB_BackPageSize(w) / 2,
		NB_PageHeight(w) + NB_MarginHeight(w) + NB_StatusHeight(w) + NB_BackPageSize(w) / 2);
#endif

}


static void
draw_binding_pixmap(Widget w, Pixmap p)
{
    /* FIX ME */

    Dimension y;
    /* draw SPIRAL binding */
    for (y=0; y < XtHeight(w); y += spiral_height)
	XCopyArea(XtDisplay(w), p, XtWindow(w),
		NB_BindingGc(w),
		0, 0,					/* source x,y */
               NB_BindingWidth(w), spiral_height,      /* width,height */
		0, y);					/* dest x,y */

    return;
}


static void
draw_binding(Widget w)
{
    Pixmap p;

    switch(NB_BindingType(w))
    {
    case XmNONE: 
        /* just return */
        return;
    case XmPIXMAP: 
        p = NB_BindingPixmap(w);
        break;
    case XmSOLID: 
        /* FIX ME: fill with solid color and return */
        return;
    case XmSPIRAL:
        p = NB_SpiralPixmap(w);
        break;
    case XmPIXMAP_OVERLAP_ONLY: 
        p = NB_BindingPixmap(w);
        break;
    default:
        /* Silently fix things. */
        NB_BindingType(w) = XmSPIRAL;
        p = NB_SpiralPixmap(w);
        break;
    }

    draw_binding_pixmap(w, p);
}


static void
draw_frame(Widget w)
{
   /* FIX ME: draw frame */
}


    /* Now display other stuff */
static void
expose(Widget w, XEvent *event, Region region)
{
    if (! XtWindow(w))
	return;

    DEBUGOUT(_LtDebug(__FILE__, w, "Expose\n"));

    /* Display back page first */
    draw_back_pages(w);

    /* Display Binding */
    draw_binding(w);

    /* Display Frame */
    draw_frame(w);

    _XmRedisplayGadgets(w, event, region);
}


static void
resize(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "Resize\n"));
	if (XtIsRealized(w))
		XClearArea(XtDisplay(w), XtWindow(w), 0, 0, 0, 0, False);
	Layout(w);
	_XmNotebookShowPage(w, NB_CurrentPageNumber(w), 0, NULL);
}


static void
_XmNBTabCallback(Widget w, XtPointer client, XtPointer call)
{
	int			r = (int)(long)client;
	XmAnyCallbackStruct	*cbp = (XmAnyCallbackStruct *)call;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmNBTabCallback\n"));

	_XmNotebookShowPage(XtParent(w), NBC_PageNumber(w),
		/* reason */	r,
		/* event */	cbp->event);
}


static void
_XmNBPageScrollerCallback(Widget w, XtPointer client, XtPointer call)
{
	int			r, page, oldpage;
	XmSpinBoxCallbackStruct	*cbp = (XmSpinBoxCallbackStruct *)call;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmNBPageScrollerCallback\n"));

	/*
	 * Figure out whether we increment or decrement,
	 * fix the reason code to indicate that,
	 * then grab the next page.
	 */

	oldpage = page = 0;
	if (NB_ConstraintChild(XtParent(w))) {
		/* current page */
		oldpage = NBC_PageNumber(NB_ConstraintChild(XtParent(w)));
	}

	/* FIX ME */
	switch (cbp->reason) {
	case XmCR_SPIN_NEXT:
	case XmCR_SPIN_PRIOR:
	case XmCR_SPIN_FIRST:
	case XmCR_SPIN_LAST:
		page = cbp->position;
		if (oldpage < page)
			r = XmCR_PAGE_SCROLLER_INCREMENT;
		else
			r = XmCR_PAGE_SCROLLER_DECREMENT;
		break;
	default:
		return;
	}

	_XmNotebookShowPage(XtParent(w),
		/* new page */	page,
		/* reason */	r,
		/* event */	cbp->event);
}


/*
 * Major tabs should only show if they represent a page,
 * which is not yet represented by a minor tab.
 *
 * The return value reflects whether relayout is needed.
 */
static Boolean
MajorTabsActive(Widget n)
{
	XmNotebookWidget nb = (XmNotebookWidget)n;
	int	i, j, nc = MGR_NumChildren(nb);
	Boolean	change = False, na;

	for (i=0; i<nc; i++) {
	    Widget w = MGR_Children(nb)[i];
	    Boolean	fm = False, fp = False;
	    if (NBC_ChildType(w) == XmMAJOR_TAB) {
		for (j=0; j<nc && !fm; j++) {
		    Widget ww = MGR_Children(nb)[j];

		    if (NBC_PageNumber(ww) != NBC_PageNumber(w))
			continue;

		    if (NBC_ChildType(ww) == XmPAGE) {
			fp = True;
		    } else if (NBC_ChildType(ww) == XmMINOR_TAB) {
			fm = True;
		    }
		}

		if (fm)
		    na = False;
		else if (fp)
		    na = True;
		else
		    na = False;

		if (NBC_Active(w) != na) {
		    NBC_Active(w) = na;
		    change = True;
		}
	    }
	}

	return change;
}


static void
ConstraintInitialize(Widget request, Widget nw,
		      ArgList args, Cardinal *num_args)
{
    Widget nb = XtParent(nw);
    XtPointer	t;

    DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "ConstraintInitialize\n"));

    if (NBC_ChildType(nw) == XmNONE) {
	/* Assign a default child type. */
	if (XmeTraitGet((XtPointer)XtClass(nw), XmQTaccessTextual) != NULL) {
		NBC_ChildType(nw) = XmSTATUS_AREA;
		NBC_Active(nw) = False;
		DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "is a XmSTATUS_AREA\n"));
	} else if (XmeTraitGet((XtPointer)XtClass(nw), XmQTactivatable) != NULL) {
		NBC_ChildType(nw) = XmMAJOR_TAB;
		NBC_Active(nw) = False;
		DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "is a XmMAJOR_TAB\n"));
	} else if (XmeTraitGet((XtPointer)XtClass(nw), XmQTnavigator) != NULL) {
		NBC_ChildType(nw) = XmPAGE_SCROLLER;
		NBC_Active(nw) = False;
		DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "is a XmPAGE_SCROLLER\n"));
		NB_Scroller(nb) = nw;
#ifdef	TRAIT_WORKAROUND
	} else if (XtIsSubclass(nw, xmTextWidgetClass)
			|| XtIsSubclass(nw, xmTextFieldWidgetClass)) {
		NBC_ChildType(nw) = XmSTATUS_AREA;
		NBC_Active(nw) = False;
		DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "is a XmSTATUS_AREA\n"));
	} else if (XtIsSubclass(nw, xmPushButtonWidgetClass)
			|| XtIsSubclass(nw, xmPushButtonGadgetClass)
			|| XtIsSubclass(nw, xmArrowButtonWidgetClass)
			|| XtIsSubclass(nw, xmArrowButtonGadgetClass)) {
		NBC_ChildType(nw) = XmMAJOR_TAB;
		NBC_Active(nw) = False;
		DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "is a XmMAJOR_TAB\n"));
	} else if (XtIsSubclass(nw, xmScrollBarWidgetClass)) {
		NBC_ChildType(nw) = XmPAGE_SCROLLER;
		NBC_Active(nw) = False;
		DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "is a XmPAGE_SCROLLER\n"));
		NB_Scroller(nb) = nw;
#endif
	} else {
		NBC_ChildType(nw) = XmPAGE;
		DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "is a XmPAGE\n"));
	}
    }

    if (NBC_ChildType(nw) == XmMAJOR_TAB || NBC_ChildType(nw) == XmMINOR_TAB) {
	int	rc;

	if (NBC_ChildType(nw) == XmMAJOR_TAB)
		rc = XmCR_MAJOR_TAB;
	else
		rc = XmCR_MINOR_TAB;

	t = XmeTraitGet((XtPointer)XtClass(nw), XmQTactivatable);
	if (t)
		((XmActivatableTrait)t)->changeCB(nw,
			_XmNBTabCallback, (XtPointer)(long)rc,
			True);	/* Add it */
	else
		_XmWarning(nw, "has no XmQTactivatable trait\n");
    } else if (NBC_ChildType(nw) == XmPAGE_SCROLLER) {
	t = XmeTraitGet((XtPointer)XtClass(nw), XmQTnavigator);
	if (t) {
		((XmNavigatorTrait)t)->changeMoveCB(nw,
			_XmNBPageScrollerCallback, NULL,
			True);	/* Add it */
	} else {
		_XmWarning(nw, "has no XmQTnavigator trait\n");
	}
    } else if (NBC_ChildType(nw) == XmPAGE) {
	/*
	 * When a page without a page number is managed, Notebook assigns
	 * it the smallest unallocated page number that is not less than
	 * the first page number and greater than the last allocated page
	 * number.
	 */
	if (NBC_PageNumber(nw) == 0) {
	    Cardinal i, pn;

	    pn = 0;
	    for (i=0; i<MGR_NumChildren(nb); i++) {
		Widget	c = MGR_Children(nb)[i];

		if (NBC_ChildType(c) == XmPAGE && pn < NBC_PageNumber(c))
			pn = NBC_PageNumber(c);
	    }

	    if (pn && pn >= NB_FirstPageNumber(nb))
		NBC_PageNumber(nw) = pn + 1;
	    else
		NBC_PageNumber(nw) = NB_FirstPageNumber(nb);
	} else {
	    /* FIX ME */

	}
	if (NB_FirstPageNumber(nb) > NBC_PageNumber(nw)) {
		NB_FirstPageNumber(nb) = NBC_PageNumber(nw);
	}
	if (NB_DynamicLastPageNum(nb) && (NB_LastPageNumber(nb) < NBC_PageNumber(nw))) {
		NB_LastPageNumber(nb) = NBC_PageNumber(nw);
	}

	NB_LastAllocNum(nb) = NBC_PageNumber(nw);

	DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "Child has page no %d\n",
		NBC_PageNumber(nw)));

	if (XtWidth(nw) > NB_PageWidth(nb))
		XtWidth(nw) = NB_PageWidth(nb);
	if (XtHeight(nw) > NB_PageHeight(nb))
		XtHeight(nw) = NB_PageHeight(nb);
	
    }

    if (NBC_ChildType(nw) == XmMAJOR_TAB || NBC_ChildType(nw) == XmMINOR_TAB ||
	NBC_ChildType(nw) == XmSTATUS_AREA) {
	/*
	 * When a tab or a status area without a page number is managed,
	 * the newly managed widget is assigned the page number of the
	 * most recently managed page, unless the page already has the
	 * same type of child. If the page does have the same type of child,
	 * Notebook assigns the newly managed widget a page number one
	 * greater than the most recently managed page; this new page number
	 * is now occupied.
	 */

	if (NBC_PageNumber(nw) == 0)
	    NBC_PageNumber(nw) = NB_LastAllocNum(nb);
    }

    /* Move any child out of sight by default */
    _XmMoveObject(nw, -10000, -10000);	/* FIX ME */

    NBC_Active(nw) = False;

    if (NBC_ChildType(nw) == XmPAGE && NBC_PageNumber(nw) == 1)
	_XmNotebookShowChild(nb, nw, NULL);

    (void)MajorTabsActive(nb);
    Layout(nb);
}


static Boolean
ConstraintSetValues(Widget current, Widget request, Widget nw,
		      ArgList args, Cardinal *num_args)
{
	/* FIX ME */
	Boolean	need_refresh = True,
		need_layout = True;
	Widget	nb = XtParent(nw);

	DEBUGOUT(_LtDebug2(__FILE__, nb, nw, "ConstraintSetValues\n"));

	/* This resource cannot be changed after creation */
	if (NBC_ChildType(nw) != NBC_ChildType(current))
		NBC_ChildType(nw) = NBC_ChildType(current);

	if (need_layout)
		Layout(nb);

	if (MajorTabsActive(nb))
		need_refresh = True;

	return need_refresh;
}


/*
 * Calculate the preferred size for this widget.
 *
 * There's a lot of speed to be gained here and in Layout() by
 *	having structures which keep information across widget
 *	method invocations.
 *
 * The drawback is we need to do more in InsertChild/DeleteChild.
 */
static void
PreferredSize(Widget nb, Widget instig, XtWidgetGeometry *ig,
		Dimension *widp, Dimension *htp)
{
	Cardinal    i;
	Dimension	wid = 0, ht = 0;
	Dimension	vw, vh, hw, hh;
	Dimension	verttabwid = 0, verttabht = 0,	/* vertical tab */
			hortabwid = 0, hortabht = 0;	/* horizontal tab */
	Dimension	sawid = 0, saht = 0,		/* status area */
			pswid = 0, psht = 0;		/* page scroller */
	int		nverttab = 0, nhortab = 0;
	Widget		child;

	/*
	 * First loop over all children to figure out the maximum
	 * geometry for all pages, all major/minor tabs, and all
	 * status areas.
	 */
	for (i=0; i<MGR_NumChildren(nb); i++) {
		child = MGR_Children(nb)[i];

		if (! XtIsManaged(child))
			continue;

		if (NBC_ChildType(child) == XmPAGE) {
		    DEBUGOUT(_LtDebug2(__FILE__, nb, child,
			"PreferredSize PAGE size %d %d\n",
			XtWidth(child), XtHeight(child)));

			if (child == instig) {
			    if (ig->request_mode & CWWidth)
				if (wid < ig->width)
				    wid = ig->width;
			    if (ig->request_mode & CWHeight)
				if (ht < ig->height)
				    ht = ig->height;
			} else {
				if (wid < XtWidth(child))
				    wid = XtWidth(child);
				if (ht < XtHeight(child))
				    ht = XtHeight(child);
			}
		} else if (NBC_ChildType(child) == XmMAJOR_TAB) {
		    DEBUGOUT(_LtDebug2(__FILE__, nb, child,
			"PreferredSize MAJOR TAB size %d %d\n",
			XtWidth(child), XtHeight(child)));

		    if (NB_Orientation(nb) == XmHORIZONTAL) {
			nverttab++;

			if (verttabht < XtHeight(child))
			    verttabht = XtHeight(child);
			if (verttabwid < XtWidth(child))
			    verttabwid = XtWidth(child);
		    } else {
			nhortab++;

			if (hortabht < XtHeight(child))
			    hortabht = XtHeight(child);
			if (hortabwid < XtWidth(child))
			    hortabwid = XtWidth(child);
		    }
		} else if (NBC_ChildType(child) == XmMINOR_TAB) {
		    DEBUGOUT(_LtDebug2(__FILE__, nb, child,
			"PreferredSize MINOR TAB size %d %d\n",
			XtWidth(child), XtHeight(child)));

		    if (NB_Orientation(nb) == XmHORIZONTAL) {
			nhortab++;

			if (hortabht < XtHeight(child))
			    hortabht = XtHeight(child);
			if (hortabwid < XtWidth(child))
			    hortabwid = XtWidth(child);
		    } else {
			nverttab++;

			if (verttabht < XtHeight(child))
			    verttabht = XtHeight(child);
			if (verttabwid < XtWidth(child))
			    verttabwid = XtWidth(child);
		    }
		} else if (NBC_ChildType(child) == XmSTATUS_AREA) {
		    DEBUGOUT(_LtDebug2(__FILE__, nb, child,
			"PreferredSize STATUS AREA size %d %d\n",
			XtWidth(child), XtHeight(child)));

			if (sawid < XtWidth(child))
				sawid = XtWidth(child);
			if (saht < XtHeight(child))
				saht = XtHeight(child);
		} else if (NBC_ChildType(child) == XmPAGE_SCROLLER) {
		    DEBUGOUT(_LtDebug2(__FILE__, nb, child,
			"PreferredSize PAGE SCROLLER size %d %d\n",
			XtWidth(child), XtHeight(child)));

			pswid = XtWidth(child);
			psht = XtHeight(child);
		} else {
			/* This shouldn't happen */
			_XmWarning(nb, "PreferredSize: child of unknown type 0x%X\n",
				NBC_ChildType(child));
		}
	}

	if (saht < psht)
		saht = psht;

	if (wid < sawid + NB_MarginWidth(nb) + pswid)
		wid = sawid + NB_MarginWidth(nb) + pswid;

	NB_PageWidth(nb) = wid;
	NB_PageHeight(nb) = ht;
	NB_StatusWidth(nb) = sawid;
	NB_StatusHeight(nb) = saht;

	DEBUGOUT(_LtDebug(__FILE__, nb,
		"PreferredSize: %d major tabs (%d x %d), %d minor tabs (%d x %d)\n",
		nverttab, verttabwid, verttabht,
		nhortab, hortabwid, hortabht));

	/* FIX ME add borders and such */
	vw = vh = 0;
	if (nverttab) {
		vh = (nverttab - 1) * NB_MajorSpacing(nb) + verttabht
			+ 2 * NB_MarginHeight(nb);
		vw = verttabwid + NB_MarginWidth(nb);

		DEBUGOUT(_LtDebug(__FILE__, nb,
			"PreferredSize: add %d %d for column of tabs\n",
			vw, vh));

		wid += vw;
		ht += vh;
	}
	hw = hh = 0;
	if (nhortab) {
		hh = (nhortab - 1) * NB_MajorSpacing(nb) + hortabht
			+ 2 * NB_MarginHeight(nb);
		hw = hortabwid + NB_MarginWidth(nb);

		DEBUGOUT(_LtDebug(__FILE__, nb,
			"PreferredSize: add %d %d for row of tabs\n",
			hw, hh));

		wid += hw;
		ht += hh;
	}

	NB_MajorWidth(nb) = vw;
	NB_MajorHeight(nb) = vh;
	NB_MinorWidth(nb) = hw;
	NB_MinorHeight(nb) = hh;

	/* Page Scroller */
	NB_ScrollerWidth(nb) = NB_ScrollerHeight(nb) = 0;
	if (NB_Scroller(nb) && XtIsManaged(NB_Scroller(nb))) {
		if (XtHeight(NB_Scroller(nb)) > psht)
			psht = XtHeight(NB_Scroller(nb));
		if (XtWidth(NB_Scroller(nb)) > pswid)
			pswid = XtWidth(NB_Scroller(nb));

		NB_ScrollerWidth(nb) = XtWidth(NB_Scroller(nb));
		NB_ScrollerHeight(nb) = XtHeight(NB_Scroller(nb));

		DEBUGOUT(_LtDebug2(__FILE__, nb, NB_Scroller(nb),
			"Page Scroller adds %d + 2 * %d to height\n",
			XtHeight(NB_Scroller(nb)), NB_MarginHeight(nb)));
	}

	if (saht > psht)
		ht += saht + 2 * NB_MarginHeight(nb);
	else if (psht)
		ht += psht + 2 * NB_MarginHeight(nb);

	/* Page Scroller + Status Area : width */
	if (wid < sawid + pswid + NB_MarginWidth(nb))
		wid = sawid + pswid + NB_MarginWidth(nb);

	/* Binding, backpage size - this should be the last to influence width */
	wid += NB_BindingWidth(nb) + NB_MarginWidth(nb) + NB_BackPageSize(nb);
	ht += NB_BackPageSize(nb);

	DEBUGOUT(_LtDebug(__FILE__, nb, "PreferredSize => %d %d\n", wid, ht));

	if (widp) *widp = wid;
	if (htp) *htp = ht;
}


static void
Layout(Widget w )
{
	Cardinal	i;
	Position	majx, majy;
	Position	minx, miny;

	DEBUGOUT(_LtDebug(__FILE__, w, "Layout (nb wid %d ht %d)\n",
		XtWidth(w), XtHeight(w)));

#ifdef LARGE_PAGE
	if (XtIsRealized(w)) {
	    NB_PageWidth(w) = XtWidth(w)
		- 3 * NB_MarginWidth(w) - NB_BackPageSize(w)
		- NB_MajorWidth(w);
	    NB_PageHeight(w) = XtHeight(w)
		- 3 * NB_MarginHeight(w) - NB_BackPageSize(w)
		- NB_StatusHeight(w) - NB_MinorHeight(w);

	    if (NB_Orientation(w) == XmHORIZONTAL)
		NB_PageWidth(w) -= NB_BindingWidth(w);
	    else
		NB_PageHeight(w) -= NB_BindingWidth(w);

	    DEBUGOUT(_LtDebug(__FILE__, w, "Layout: set page size to %d %d\n",
		NB_PageWidth(w), NB_PageHeight(w)));
	}

	majx = XtWidth(w) - NB_MarginWidth(w) - NB_MajorWidth(w);
#else
	majx = NB_BindingWidth(w) + NB_MarginWidth(w) + NB_PageWidth(w)
		+ NB_MarginWidth(w) + NB_BackPageSize(w);
#endif
	majy = NB_MarginHeight(w);

	minx = NB_BindingWidth(w) + NB_MarginWidth(w);
	miny = NB_PageHeight(w) + NB_MarginHeight(w) * 2 + NB_StatusHeight(w) + NB_BackPageSize(w);

	for (i=0; i<MGR_NumChildren(w); i++) {
	    Widget	child = MGR_Children(w)[i];
	    if (! XtIsManaged(child))
		continue;

	    if (NBC_ChildType(child) == XmMAJOR_TAB && NBC_Active(child)) {
		_XmMoveObject(child, majx, majy);
		majy += XtHeight(child) + NB_MajorSpacing(w);
	    } else if (NBC_ChildType(child) == XmMINOR_TAB) {
		DEBUGOUT(_LtDebug2(__FILE__, w, child, "Layout: XmMINOR_TAB\n"));
		_XmMoveObject(child, minx, miny);
		minx += XtWidth(child) + NB_MinorSpacing(w);
/* HERE */
	    } else if (NBC_ChildType(child) == XmPAGE_SCROLLER) {
		_XmMoveObject(child,
			NB_BindingWidth(w) + NB_MarginWidth(w) * 2 + NB_StatusWidth(w),
			NB_PageHeight(w) + NB_MarginHeight(w));
	    } else if (NBC_ChildType(child) == XmSTATUS_AREA) {
		DEBUGOUT(_LtDebug2(__FILE__, w, child, "Layout: XmSTATUS_AREA\n"));
	    } else {
		/* XmPAGE treated elsewhere */
	    }
	}
}


static void
InsertChild(Widget w)
{
	/* Widget	nb = XtParent(w); */

	DEBUGOUT(_LtDebug2(__FILE__, XtParent(w), w, "InsertChild\n"));

#define	superclass	(&xmManagerClassRec)
	(*superclass->composite_class.insert_child)(w);
#undef	superclass
}


static void
DeleteChild(Widget w)
{
	/* Widget	nb = XtParent(w); */

	DEBUGOUT(_LtDebug2(__FILE__, XtParent(w), w, "DeleteChild\n"));

#define	superclass	(&xmManagerClassRec)
	(*superclass->composite_class.delete_child)(w);
#undef	superclass
}


static XtGeometryResult
QueryGeometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
    Dimension wid, hei;
    XtWidgetGeometry geo;

#define	Wants(x)	((geo.request_mode & x) == x)

    DEBUGOUT(_LtDebug(__FILE__, w, "QueryGeometry\n"));

    geo = *proposed;

    answer->request_mode = 0;

    PreferredSize(w, NULL, NULL, &wid, &hei);

    answer->request_mode = CWWidth|CWHeight;
    answer->width = wid;
    answer->height = hei;

    if (Wants(CWHeight) && (geo.height != answer->height))
    {
	return XtGeometryNo;
    }
    if (Wants(CWWidth) && (geo.width != answer->width))
    {
	return XtGeometryNo;
    }

    return XtGeometryYes;
}


static XtGeometryResult
GeometryManager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
#if 0
	Widget			p = XtParent(w);
	XtGeometryResult	result;
	Dimension		wid = 0, hei = 0;
	XtWidgetGeometry	req, preq;
#endif

	DEBUGOUT(_LtDebug2(__FILE__, XtParent(w), w, "GeometryManager\n"));

	return XtGeometryNo;
}


static void
ChangeManaged(Widget w)
{
	/* This one usually determines initial size of the widget */
	PreferredSize(w, NULL, NULL, &XtWidth(w), &XtHeight(w));

	(void)MajorTabsActive(w);
	Layout(w);

	DEBUGOUT(_LtDebug(__FILE__, w, "ChangeManaged\n"));
}


static void
TraverseTab(Widget w, XEvent *evp, String *par, Cardinal *npar)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "TraverseTab\n"));
}


static void
_XmNBDefaultLastPageNumber(Widget w, int offset, XrmValue *val)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmNBDefaultLastPageNumber\n"));

	NB_DynamicLastPageNum(w) = True;
	NB_LastPageNumber(w) = 1;
}


static void
_XmNBCreateSpiralPixmap(Widget w)
{
	XpmAttributes   attrib; 
        Pixmap          mask;

	attrib.valuemask = 0;

	XpmCreatePixmapFromData(XtDisplay(w),
                XRootWindowOfScreen(XtScreen(w)),
                spiral,
                &NB_SpiralPixmap(w),
                &mask,
                &attrib);

	/* Fallback - e.g. if not getting any colors */
	if (NB_SpiralPixmap(w) == None)
	{
	    NB_SpiralPixmap(w) = XCreatePixmapFromBitmapData(XtDisplay(w),
		XtWindow(w), spiral_bits, spiral_width, spiral_height,
		NB_BackPageForeground(w), NB_BackPageBackground(w),
		DefaultDepthOfScreen(XtScreen(w)));
	}
}


/*
 * Free the old Graphics Contexts (if any), and allocate new ones.
 *
 * To be called from SetValues and Initialize.
 */
static void
_XmNBChangeGC(Widget w)
{
	XGCValues	values;
	XtGCMask	mask, dynamic, dontcare;

	if (NB_BackPageGc(w)) XFreeGC(XtDisplay(w), NB_BackPageGc(w));
	if (NB_FrameGc(w)) XFreeGC(XtDisplay(w), NB_FrameGc(w));
	if (NB_BindingGc(w)) XFreeGC(XtDisplay(w), NB_BindingGc(w));
	if (NB_ForegroundGc(w)) XFreeGC(XtDisplay(w), NB_ForegroundGc(w));
	if (NB_BackgroundGc(w)) XFreeGC(XtDisplay(w), NB_BackgroundGc(w));

	if (NB_SpiralPixmap(w) != XmUNSPECIFIED_PIXMAP)
		XFreePixmap(XtDisplay(w), NB_SpiralPixmap(w));

	NB_BackPageGc(w) = NULL;
	NB_FrameGc(w) = NULL;
	NB_BindingGc(w)	 = NULL;
	NB_ForegroundGc(w) = NULL;
	NB_BackgroundGc(w) = NULL;
	NB_SpiralPixmap(w) = XmUNSPECIFIED_PIXMAP;

	values.background = XmParentBackground(w);
	mask = GCBackground;
	dynamic = 0;			/* FIX ME */
	dontcare = ~(mask | dynamic);
	NB_BackgroundGc(w) = XtAllocateGC(w, 0, mask, &values,
		dynamic, dontcare);

    /* Binding */
	values.foreground = NB_BackPageForeground(w);
	values.background = NB_BackPageBackground(w);
	mask = GCForeground | GCBackground;
	dynamic = 0;			/* FIX ME */
	dontcare = ~(mask | dynamic);
	NB_BindingGc(w) = XtAllocateGC(w, 0, mask, &values,
		dynamic, dontcare);

       /* FIX ME */
    /* Spiral - note this code is also in Realize(). */
       if (XtWindow(w))
	   _XmNBCreateSpiralPixmap(w);
}


static void
Realize(Widget w, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Realize\n"));

#define superclass (&xmManagerClassRec)
    (*superclass->core_class.realize) (w, value_mask, attributes);
#undef superclass

    if (NB_SpiralPixmap(w) == XmUNSPECIFIED_PIXMAP)
	   _XmNBCreateSpiralPixmap(w);

    Layout(w);
    _XmNotebookShowPage(w, 1, 0, NULL);

    DEBUGOUT(_LtDebug(__FILE__, w, "Realize (end)\n"));
}


static void
_XmNBFrameBackgroundDefault(Widget w, int offset, XrmValue *val)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmNBFrameBackgroundDefault\n"));

    /*
     * The current solution is to copy the overall background colour
     * here; this may need to change.
     */
    _XmBackgroundColorDefault(w, offset, val);
}


/*
 * This should be a lower intensity version of XmNframeBackground
 */
static void
_XmNBBackPageBackgroundDefault(Widget w, int offset, XrmValue *val)
{
    XColor color, back;

    color.pixel = NB_FrameBackground(w);

    XQueryColor(XtDisplayOfObject(w),
		DefaultColormapOfScreen(XtScreenOfObject(w)),
		&color);

    back.red = color.red * 9 / 10;
    back.green = color.green * 9 / 10;
    back.blue = color.blue * 9 / 10;

    /* If the color allocation fails,
     * just use the same as the frame background
     */

    if (!XAllocColor(XtDisplayOfObject(w),
		     DefaultColormapOfScreen(XtScreenOfObject(w)),
		     &back))
    {
	back.pixel = color.pixel;
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
	"_XmNBBackPageBackgroundDefault %d (%d,%d,%d) -> %d (%d,%d,%d)\n",
	color.pixel, color.red, color.green, color.blue,
	back.pixel, back.red, back.green, back.blue));


    val->addr = (XPointer)&back.pixel;
}


static void
_XmNBTraitInit(Widget sfw, XtCallbackProc moveCB, Widget sw)
{
	DEBUGOUT(_LtDebug(__FILE__, sfw, "_XmNBTraitInit\n"));
}


static Boolean
_XmNBTraitGetInfo(Widget sfw,
		Cardinal *dimension,
		Widget **nav_list,
		Cardinal *num_nav_list)
{
	DEBUGOUT(_LtDebug(__FILE__, sfw, "_XmNBTraitGetInfo\n"));

	/* FIX ME unimplemented */
	return False;
}


static void
_XmNBTraitAddNavigator(Widget sfw, Widget nw, Mask dimMask)
{
	DEBUGOUT(_LtDebug(__FILE__, sfw, "_XmNBTraitAddNavigator\n"));
}


static void
_XmNBTraitRemoveNavigator(Widget sfw, Widget nw)
{
	DEBUGOUT(_LtDebug(__FILE__, sfw, "_XmNBTraitRemoveNavigator\n"));
}


/* public interfaces */

extern Widget
XmCreateNotebook(Widget parent, char *name,
		    Arg *arglist, Cardinal argcount)
{
	return XtCreateWidget(name, xmNotebookWidgetClass, parent,
			  arglist, argcount);
}


extern XmNotebookPageStatus
XmNotebookGetPageInfo(Widget w,
                      int page_number,
                      XmNotebookPageInfo *page_info)
{
	Cardinal i;
	Widget cl=NULL;

	_XmWarning(w, "XmNotebookGetPageInfo() is not yet implemented\n");

	/* amai: no, this is not the correct code, but makes the compiler happy */
	for (i=0; i<MGR_NumChildren(w); i++) {
	    Widget c = MGR_Children(w)[i];

	    if (NBC_ChildType(c) == XmPAGE && NBC_PageNumber(c) == page_number)
		cl = c;
       }
	if (cl)
		return XmPAGE_FOUND;
	else
		return XmPAGE_INVALID;
}
