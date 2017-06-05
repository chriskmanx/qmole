/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/SpinBox.c,v 1.3 2005/08/15 19:12:32 dannybackx Exp $
 *
 * Copyright (C) 1997, 1998 Free Software Foundation, Inc.
 * Copyright © 1998, 1999, 2000, 2001, 2003 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/SpinBox.c,v 1.3 2005/08/15 19:12:32 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/SpinBP.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/TraitP.h>
#include <Xm/AccTextT.h>
#include <Xm/NavigatorT.h>
#include <Xm/ScrollFrameT.h>

#include <XmI/DebugUtil.h>

/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget nw,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static void resize(Widget w);

static void expose(Widget w, XEvent *event, Region region);

static XtGeometryResult QueryGeometry(Widget w,
				       XtWidgetGeometry *proposed,
				       XtWidgetGeometry *answer);

static Boolean SetValues(Widget current, Widget request, Widget nw,
			  ArgList args, Cardinal *num_args);

static XtGeometryResult GeometryManager(Widget w,
					 XtWidgetGeometry *request,
					 XtWidgetGeometry *reply);

static void ChangeManaged(Widget w);

static void ConstraintInitialize(Widget request, Widget nw,
				  ArgList args, Cardinal *num_args);
static Boolean ConstraintSetValues(Widget curr, Widget req, Widget nw,
				     ArgList args, Cardinal *num_args);

static void InsertChild(Widget w);
static void _XmSpinBoxShowValue(Widget, Widget);
static char * _XmSpinBoxNumericString(Widget tw);
static void Layout(Widget w, Widget instig);

/* Trait functions */
static void _XmSPBTraitChangeMoveCB(Widget, XtCallbackProc, XtPointer, Boolean);
static void _XmSPBTraitSetValue(Widget, XmNavigatorData, Boolean);
static int _XmSPBTraitGetValue(Widget, XmNavigatorData);

static void _XmSPBTraitMoveCB(Widget nav, XtPointer client, XtPointer call);

/* Actions */
static void SpinBArm(Widget w, XEvent *evp, String *par, Cardinal *npar);
static void SpinBDisarm(Widget w, XEvent *evp, String *par, Cardinal *npar);
static void SpinBFirst(Widget w, XEvent *evp, String *par, Cardinal *npar);
static void SpinBLast(Widget w, XEvent *evp, String *par, Cardinal *npar);
static void SpinBLeft(Widget w, XEvent *evp, String *par, Cardinal *npar);
static void SpinBRight(Widget w, XEvent *evp, String *par, Cardinal *npar);
static void SpinBNext(Widget w, XEvent *evp, String *par, Cardinal *npar);
static void SpinBPrior(Widget w, XEvent *evp, String *par, Cardinal *npar);

static void SpinBoxEnter(Widget w, XEvent *evp, String *par, Cardinal *npar);
static void SpinBoxFocusIn(Widget w, XEvent *evp, String *par, Cardinal *npar);
static void SpinBoxFocusOut(Widget w, XEvent *evp, String *par, Cardinal *npar);

#define	XmRSpacing	"Spacing"

/*
 * Resources for the SpinBox class
 */
#define Offset(field) XtOffsetOf(XmSpinBoxRec, spinBox.field)
static XtResource resources[] =
{
    {
	XmNarrowLayout, XmCArrowLayout, XmRArrowLayout,
	sizeof(unsigned char), Offset(arrow_layout),
	XmRImmediate, (XtPointer)XmARROWS_END
    },
    {
	XmNarrowOrientation, XmCArrowOrientation, XmRArrowOrientation,
	sizeof(unsigned char), Offset(arrow_orientation),
	XmRImmediate, (XtPointer)XmARROWS_VERTICAL
    },
    {
	XmNarrowSize, XmCArrowSize, XmRHorizontalDimension,
	sizeof(Dimension), Offset(arrow_size),
	XmRImmediate, (XtPointer)16
    },
    {
	XmNdefaultArrowSensitivity, XmCDefaultArrowSensitivity, XmRDefaultArrowSensitivity,
	sizeof(unsigned char), Offset(default_arrow_sensitivity),
	XmRImmediate, (XtPointer)XmARROWS_SENSITIVE
    },
    {
	XmNdetailShadowThickness, XmCDetailShadowThickness, XmRHorizontalDimension,
	sizeof(Dimension), Offset(detail_shadow_thickness),
	XmRImmediate, (XtPointer)2
    },
    {
	XmNinitialDelay, XmCInitialDelay, XmRInt,
	sizeof(unsigned int), Offset(initial_delay),
	XmRImmediate, (XtPointer)250
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)8
	/*
	 * FIX ME : default value should be DYNAMIC,
	 * but I don't know what the default is ...
	 */
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)8
	/*
	 * FIX ME : default value should be DYNAMIC,
	 * but I don't know what the default is ...
	 */
    },
    {
	XmNmodifyVerifyCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(modify_verify_cb),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNrepeatDelay, XmCRepeatDelay, XmRInt,
	sizeof(unsigned int), Offset(repeat_delay),
	XmRImmediate, (XtPointer)200
    },
    {
	XmNspacing, XmCSpacing, XmRSpacing,
	sizeof(Dimension), Offset(spacing),
	XmRImmediate, (XtPointer)2
	/*
	 * FIX ME : default value should be DYNAMIC,
	 * but I don't know what the default is ...
	 */
    },
    {
	XmNvalueChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_cb),
	XmRImmediate, (XtPointer)NULL
    },
/* Override superclass's values */
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
	sizeof(XmNavigationType), XtOffsetOf(XmSpinBoxRec, manager.navigation_type),
	XmRImmediate, (XtPointer)XmSTICKY_TAB_GROUP
    }
};
#undef	Offset

static XtActionsRec actions[] =
{
	{"SpinBArm",		SpinBArm},
	{"SpinBDisarm",		SpinBDisarm},
	{"SpinBLeft",		SpinBLeft},
	{"SpinBRight",		SpinBRight},
	{"SpinBFirst",		SpinBFirst},
	{"SpinBLast",		SpinBLast},
	{"SpinBPrior",		SpinBPrior},
	{"SpinBNext",		SpinBNext},
	{"SpinBoxEnter",	SpinBoxEnter},
	{"SpinBoxFocusIn",	SpinBoxFocusIn},
	{"SpinBoxFocusOut",	SpinBoxFocusOut}
};

/*
 * Constraint resources, i.e. resources magically added to all XmSpinBox children.
 */
#define Offset(field) XtOffsetOf(XmSpinBoxConstraintRec, spinBox.field)
static XtResource ConstraintResources[] = {
    {
	XmNarrowSensitivity, XmCArrowSensitivity, XmRArrowSensitivity,
	sizeof(unsigned char), Offset(arrow_sensitivity),
	XmRImmediate, (XtPointer)XmARROWS_DEFAULT_SENSITIVITY
    },
    {
	XmNdecimalPoints, XmCDecimalPoints, XmRShort,
	sizeof(short), Offset(decimal_points),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNincrementValue, XmCIncrementValue, XmRInt,
	sizeof(int), Offset(increment_value),
	XmRImmediate, (XtPointer)1
    },
    {
	XmNmaximumValue, XmCMaximumValue, XmRInt,
	sizeof(int), Offset(maximum_value),
	XmRImmediate, (XtPointer)10
    },
    {
	XmNminimumValue, XmCMinimumValue, XmRInt,
	sizeof(int), Offset(minimum_value),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNnumValues, XmCNumValues, XmRInt,
	sizeof(int), Offset(num_values),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNposition, XmCPosition, XmRInt,
	sizeof(int), Offset(position),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNpositionType, XmCPositionType, XmRChar,
	sizeof(unsigned char), Offset(position_type),
	XmRImmediate, (XtPointer)XmPOSITION_VALUE
    },
    {
	XmNspinBoxChildType, XmCSpinBoxChildType, XmRSpinBoxChildType,
	sizeof(unsigned char), Offset(sb_child_type),
	XmRImmediate, (XtPointer)XmSTRING
    },
    {
	XmNvalues, XmCValues, XmRXmStringTable,
	sizeof(XmStringTable), Offset(values),
	XmRImmediate, (XtPointer)NULL
    },
};

/* FIX ME - this should go in other files */
/* FIX ME - this is currently a mix of Manager and SpinBox stuff */
/* Use ANSI C concatenation so we can add comments */
char _XmSpinBox_defaultTranslations[] =
    "<FocusIn>:			SpinBoxFocusIn() ManagerFocusIn()\n"
    "<FocusOut>:		SpinBoxFocusOut() ManagerFocusOut()\n"
    "<Btn1Down>:		SpinBArm() ManagerGadgetArm()\n"
    "<Btn1Down>,<Btn1Up>:	SpinBDisarm() ManagerGadgetActivate()\n"
    "<Btn1Up>:			SpinBDisarm() ManagerGadgetActivate()\n"
    ":<Key>osfUp:		SpinBPrior()\n"
    ":<Key>osfDown:		SpinBNext()\n"
    ":<Key>osfLeft:		SpinBLeft()\n"
    ":<Key>osfRight:		SpinBRight()\n"
    ":<Key>osfBeginData:	SpinBFirst()\n"
    ":<Key>osfEndData:		SpinBLast()\n"
#if 1
    /* Scroll wheel */
    "<Btn5Down>:	SpinBPrior()\n"
    "<Btn5Up>:		SpinBDisarm()\n"
    "<Btn4Down>:	SpinBNext()\n"
    "<Btn4Up>:		SpinBDisarm()\n"
#endif
;

/* FIX ME - this should go in other files */
char _XmSpinBox_traversalTranslations[] = "";

/* These are added to all traversable text children */
char _XmSpinBox_ChildAccelerators[] =
	"<Key>osfUp:			SpinBPrior()\n"
	"<Key>osfDown:			SpinBNext()\n"
	"<KeyUp>osfUp:			SpinBDisarm()\n"
	"<KeyUp>osfDown:		SpinBDisarm()\n"
	"<Key>osfLeft:			SpinBLeft()\n"
	"<Key>osfRight:			SpinBRight()\n"
	"<KeyUp>osfLeft:		SpinBDisarm()\n"
	"<KeyUp>osfRight:		SpinBDisarm()\n"
	"<Key>osfBeginData:		SpinBFirst()\n"
	"<Key>osfEndData:		SpinBLast()\n";

static XmBaseClassExtRec _XmSpinBoxCoreClassExtRec = {
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

static XmManagerClassExtRec _XmSpinBoxMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL /* FIXME */
};

XmSpinBoxClassRec xmSpinBoxClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmManagerClassRec,
	/* class_name            */ "XmSpinBox",
	/* widget_size           */ sizeof(XmSpinBoxRec),
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
	/* tm_table              */ _XmSpinBox_defaultTranslations,
	/* query_geometry        */ QueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)&_XmSpinBoxCoreClassExtRec
    },
    /* Composite class part */
    {
	/* geometry manager */ GeometryManager, 
        /* change_managed   */ ChangeManaged, 
        /* insert_child     */ InsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ (XtPointer)&nbCompositeExt
    },
    /* Constraint class part */
    {
	/* subresources      */ ConstraintResources,
        /* subresource_count */ XtNumber(ConstraintResources),
        /* constraint_size   */ sizeof(XmSpinBoxConstraintRec),
        /* initialize        */ ConstraintInitialize,
        /* destroy           */ NULL,
        /* set_values        */ ConstraintSetValues,
        /* extension         */ NULL,
    },
    /* XmManager class part */
    {
	/* translations                 */ _XmSpinBox_traversalTranslations,
	/* syn_resources                */ NULL,
	/* num_syn_resources            */ 0,
	/* syn_constraint_resources     */ NULL,
	/* num_syn_constraint_resources */ 0,
	/* parent_process               */ XmInheritParentProcess,
	/* extension                    */ (XtPointer)&_XmSpinBoxMClassExtRec
    },
	/* XmSpinBox part */
    {
	/* extension */ NULL,
    },
};


WidgetClass xmSpinBoxWidgetClass = (WidgetClass)&xmSpinBoxClassRec;

XmNavigatorTraitRec _XmSPBNavigatorTraitRec = {
	/* version */		0,
	/* changeMoveCB */	_XmSPBTraitChangeMoveCB,
	/* setValue */		_XmSPBTraitSetValue,
	/* getValue */		_XmSPBTraitGetValue
};

static void
class_initialize(void)
{
    _XmSpinBoxCoreClassExtRec.record_type = XmQmotif;

    XmeTraitSet((XtPointer)xmSpinBoxWidgetClass, XmQTnavigator,
                (XtPointer)&_XmSPBNavigatorTraitRec);
    XmeTraitSet((XtPointer)xmSpinBoxWidgetClass, XmQTnavigator,
                (XtPointer)&_XmSPBNavigatorTraitRec);
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmSpinBoxWidgetClass nbclass = (XmSpinBoxWidgetClass)widget_class;
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

    _XmFastSubclassInit(widget_class, XmSPINBOX_BIT);
}

static void
initialize(Widget request, Widget nw,
	   ArgList args, Cardinal *num_args)
{
	XmScrollFrameTrait	t;

	DEBUGOUT(_LtDebug(__FILE__, nw,
		      "SpinBox initialize: w/h %d %d\n",
		      XtWidth(nw), XtHeight(nw)));
#if 0
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, nw, args, *num_args, False));
#endif

	SPB_TextW(nw) = NULL;
	SPB_TimerId(nw) = (XtIntervalId)0;

	if (XtWidth(request) == 0 && XtHeight(request) == 0) {
		XtWidth(nw) = 200;
		XtHeight(nw) = 50;

		Layout(nw, NULL);
	}

	/* Find out if our parent is a ScrollFrame */
	t = (XmScrollFrameTrait) XmeTraitGet((XtPointer)XtClass(XtParent(nw)),
					XmQTscrollFrame);
	if (t) {
		t->init(XtParent(nw), _XmSPBTraitMoveCB, nw);	/* ??? FIX ME */
	}
}

static void
destroy(Widget w)
{
}

static Boolean
SetValues(Widget old, Widget request, Widget nw,
	   ArgList args, Cardinal *num_args)
{
    Boolean	need_refresh = False,
		need_layout = False;

    DEBUGOUT(_LtDebug(__FILE__, nw, "SetValues()\n"));

#define	NE(x)	(x(old) != x(nw))

    if (NE(SPB_ArrowSize)
	|| NE(SPB_MarginWidth)
	|| NE(SPB_MarginHeight)
	|| NE(SPB_Spacing)
	|| NE(SPB_DetailShadowThickness)
	|| NE(SPB_ArrowLayout))
		need_layout = True;

    /* Am I being paranoid - not sure this can be set through setvalues */
    if (NE(SPB_UpX) || NE(SPB_UpY)
     || NE(SPB_UpWidth) || NE(SPB_UpHeight)
     || NE(SPB_DownX) || NE(SPB_DownY)
     || NE(SPB_DownWidth) || NE(SPB_DownHeight)) {
	/* FIX ME What to do ? This'll recalculate (i.e. overrule) them again. */
	need_layout = True;
    }

    if (need_layout) {
	Layout(nw, NULL);
	need_refresh = True;
    }

    return need_refresh;
#undef	NE
}

static void
expose(Widget w, XEvent *event, Region region)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "Expose up %d %d down %d %d\n",
		SPB_UpX(w), SPB_UpY(w), SPB_DownX(w), SPB_DownY(w)));

	_XmRedisplayGadgets(w, event, region);

	XmeDrawArrow(XtDisplay(w), XtWindow(w),
			MGR_BottomShadowGC(w),
			MGR_TopShadowGC(w),
			MGR_HighlightGC(w),
			SPB_UpX(w),
			SPB_UpY(w),
			SPB_UpWidth(w),
			SPB_UpHeight(w),
			SPB_DetailShadowThickness(w),
			(SPB_ArrowOrientation(w) == XmARROWS_HORIZONTAL) ? XmARROW_RIGHT : XmARROW_UP);
	XmeDrawArrow(XtDisplay(w), XtWindow(w),
			MGR_BottomShadowGC(w),
			MGR_TopShadowGC(w),
			MGR_HighlightGC(w),
			SPB_DownX(w),
			SPB_DownY(w),
			SPB_DownWidth(w),
			SPB_DownHeight(w),
			SPB_DetailShadowThickness(w),
			(SPB_ArrowOrientation(w) == XmARROWS_HORIZONTAL) ? XmARROW_LEFT : XmARROW_DOWN);
}

static void
resize(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "Resize\n"));
}

/*
 * Because the XmNspinBoxChildType resource is "CG" - which means its value
 * can only be set at creation time - we can at this time set a value to
 * the text widget to make it indicate something sensible initially.
 */
static void
ConstraintInitialize(Widget request, Widget nw,
		      ArgList args, Cardinal *num_args)
{
    Widget sb = XtParent(nw);

	DEBUGOUT(_LtDebug2(__FILE__, sb, nw, "ConstraintInitialize\n"));
#if 0
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, nw, args, *num_args, False));
#endif

		if (SPBC_Numeric(nw)) {
			DEBUGOUT(_LtDebug(__FILE__, nw,
				"ConstraintInitialize(XmNUMERIC) min %d max %d inc %d\n",
				SPBC_Minimum(nw),
				SPBC_Maximum(nw),
				SPBC_Increment(nw) ));
		}
		else {
			DEBUGOUT(_LtDebug(__FILE__, nw,
				"ConstraintInitialize(XmSTRING) num_values %d\n",
				SPBC_NumValues(nw)));
		}

	if (SPBC_ArrowSensitivity(nw) == XmARROWS_DEFAULT_SENSITIVITY)
		SPBC_ArrowSensitivity(nw) = SPB_DefaultSensitivity(sb);

	if (XmeTraitGet((XtPointer)XtClass(nw), XmQTaccessTextual))
		_XmSpinBoxShowValue(sb, nw);
}

static Boolean
ConstraintSetValues(Widget curr, Widget req, Widget nw,
				     ArgList args, Cardinal *num_args)
{
	Widget	sb = XtParent(nw);
	Boolean	r = False;

	DEBUGOUT(_LtDebug2(__FILE__, sb, nw, "ConstraintSetValues\n"));
#if 0
	DEBUGOUT(_LtDebugPrintArgList(__FILE__, nw, args, *num_args, False));
#endif

#define	NE(x)	(x(curr) != x(nw))

	/* In which cases do we need to redraw ? */
	if (NE(SPBC_Minimum) || NE(SPBC_Maximum) || NE(SPBC_Increment)
	 || NE(SPBC_DecimalPoints) || NE(SPBC_Position)
	 || NE(SPBC_ChildType))
		r = True;

	/* In which cases do we need to act on our children ? */
	if (NE(SPBC_Minimum) || NE(SPBC_Maximum)
	 || NE(SPBC_DecimalPoints) || NE(SPBC_Position)
	 || NE(SPBC_ChildType))
		_XmSpinBoxShowValue(sb, nw);

#undef	NE

	return r;
}

/*
 * The layout functions.
 *	PreferredSize()
 *	Layout()
 */

static void
PreferredSize(	Widget w, Widget instig, XtWidgetGeometry *ig,
				Dimension *wid, Dimension *ht)
{
	Cardinal i;
	Position	xx, yy;
	Dimension	hh = 0;

	xx = SPB_MarginWidth(w);
	yy = SPB_MarginHeight(w);

	for (i=0; i<MGR_NumChildren(w); i++)
		if (XtIsManaged(MGR_Children(w)[i])) {
			Widget	child = MGR_Children(w)[i];

			if (XtHeight(child) > hh)
				hh = XtHeight(child);

			xx += XtWidth(child) + SPB_Spacing(w);
			DEBUGOUT(_LtDebug2(__FILE__, w, child,
				"PreferredSize: add width %d\n",
				XtWidth(child)));
	}

	xx += SPB_DownWidth(w) + SPB_Spacing(w);
	xx += SPB_UpWidth(w) + SPB_MarginWidth(w);

	*wid = xx;
	*ht = hh + 2 * SPB_MarginHeight(w);;

	DEBUGOUT(_LtDebug(__FILE__, w,
	"PreferredSize => %d %d (ArrowSize %d MarginWidth %d"
	" MarginHeight %d Spacing %d)\n",
		*wid, *ht,
		SPB_ArrowSize(w),
		SPB_MarginWidth(w), SPB_MarginHeight(w),
		SPB_Spacing(w)));
}

/*
 * Layout is primarily influenced by
 *	SPB_ArrowLayout (XmARROWS_BEGINNING, XmARROWS_END, XmARROWS_SPLIT).
 *
 * FIX ME
 *	Should also take VendorShell's XmNlayoutDirection into account.
 *	ArrowLayout XmARROWS_FLAT_BEGINNING, XmARROWS_FLAT_END missing.
 */
static void
Layout(Widget w, Widget instig)
{
	Cardinal i;
	Position	xx, yy;
	Dimension	hh = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmSpinBox Layout %s\n",
		(SPB_ArrowLayout(w) == XmARROWS_BEGINNING) ? "XmARROWS_BEGINNING" :
		(SPB_ArrowLayout(w) == XmARROWS_END) ? "XmARROWS_END" :
		(SPB_ArrowLayout(w) == XmARROWS_SPLIT) ? "XmARROWS_SPLIT" :
		(SPB_ArrowLayout(w) == XmARROWS_FLAT_BEGINNING) ? "XmARROWS_FLAT_BEGINNING" :
		(SPB_ArrowLayout(w) == XmARROWS_FLAT_END) ? "XmARROWS_FLAT_END" :
		"??"
	));

	for (i=0; i<MGR_NumChildren(w); i++)
	    if (XtHeight(MGR_Children(w)[i]) > hh)
		hh = XtHeight(MGR_Children(w)[i]);

	yy = SPB_MarginHeight(w) + hh / 2
		- SPB_ArrowSize(w) / 2;

	switch (SPB_ArrowLayout(w)) {
	case XmARROWS_BEGINNING:
	    SPB_DownX(w) = SPB_MarginWidth(w);
	    SPB_DownY(w) = yy;
	    SPB_DownWidth(w) = SPB_ArrowSize(w);
	    SPB_DownHeight(w) = SPB_ArrowSize(w);

	    xx = SPB_MarginWidth(w) + SPB_Spacing(w) + SPB_DownWidth(w);

	    SPB_UpX(w) = xx;
	    SPB_UpY(w) = yy;
	    SPB_UpWidth(w) = SPB_ArrowSize(w);
	    SPB_UpHeight(w) = SPB_ArrowSize(w);

	    xx += SPB_UpWidth(w) + SPB_Spacing(w);

	    for (i=0; i<MGR_NumChildren(w); i++)
		if (XtIsManaged(MGR_Children(w)[i])) {
			Widget	child = MGR_Children(w)[i];

			_XmConfigureObject(child, xx, SPB_MarginHeight(w),
				XtWidth(child), XtHeight(child),
				XtBorderWidth(child));

			xx += XtWidth(child) + SPB_Spacing(w);
		}

		break;
	case XmARROWS_END:
	default:
	    xx = SPB_MarginWidth(w);

	    for (i=0; i<MGR_NumChildren(w); i++)
		if (XtIsManaged(MGR_Children(w)[i])) {
			Widget	child = MGR_Children(w)[i];

			_XmConfigureObject(child, xx, SPB_MarginHeight(w),
				XtWidth(child), XtHeight(child),
				XtBorderWidth(child));

			xx += XtWidth(child) + SPB_Spacing(w);
		}

	    SPB_DownX(w) = xx;
	    SPB_DownY(w) = yy;
	    SPB_DownWidth(w) = SPB_ArrowSize(w);
	    SPB_DownHeight(w) = SPB_ArrowSize(w);

	    xx += SPB_DownWidth(w) + SPB_Spacing(w);

	    SPB_UpX(w) = xx;
	    SPB_UpY(w) = yy;
	    SPB_UpWidth(w) = SPB_ArrowSize(w);
	    SPB_UpHeight(w) = SPB_ArrowSize(w);
		break;
	case XmARROWS_SPLIT:
	    SPB_DownX(w) = SPB_MarginWidth(w);
	    SPB_DownY(w) = yy;
	    SPB_DownWidth(w) = SPB_ArrowSize(w);
	    SPB_DownHeight(w) = SPB_ArrowSize(w);

	    xx = SPB_MarginWidth(w) + SPB_Spacing(w) + SPB_DownWidth(w);

	    for (i=0; i<MGR_NumChildren(w); i++)
		if (XtIsManaged(MGR_Children(w)[i])) {
			Widget	child = MGR_Children(w)[i];

			_XmConfigureObject(child, xx, SPB_MarginHeight(w),
				XtWidth(child), XtHeight(child),
				XtBorderWidth(child));

			xx += XtWidth(child) + SPB_Spacing(w);
		}

	    SPB_UpX(w) = xx;
	    SPB_UpY(w) = yy;
	    SPB_UpWidth(w) = SPB_ArrowSize(w);
	    SPB_UpHeight(w) = SPB_ArrowSize(w);
	    break;
	case XmARROWS_FLAT_BEGINNING:				/* FIX ME */
	    SPB_DownX(w) = SPB_MarginWidth(w);
	    SPB_DownY(w) = yy;
	    SPB_DownWidth(w) = SPB_ArrowSize(w);
	    SPB_DownHeight(w) = SPB_ArrowSize(w);

	    xx = SPB_MarginWidth(w) + SPB_Spacing(w) + SPB_DownWidth(w);

	    SPB_UpX(w) = xx;
	    SPB_UpY(w) = yy;
	    SPB_UpWidth(w) = SPB_ArrowSize(w);
	    SPB_UpHeight(w) = SPB_ArrowSize(w);

	    xx += SPB_MarginWidth(w) + SPB_Spacing(w) + SPB_UpWidth(w) + SPB_ArrowSize(w);

	    for (i=0; i<MGR_NumChildren(w); i++)
		if (XtIsManaged(MGR_Children(w)[i])) {
			Widget	child = MGR_Children(w)[i];

			_XmConfigureObject(child, xx, SPB_MarginHeight(w),
				XtWidth(child), XtHeight(child),
				XtBorderWidth(child));

			xx += XtWidth(child) + SPB_Spacing(w);
		}

	    break;
	case XmARROWS_FLAT_END:				/* FIX ME */
	    xx = SPB_MarginWidth(w);

	    for (i=0; i<MGR_NumChildren(w); i++)
		if (XtIsManaged(MGR_Children(w)[i])) {
			Widget	child = MGR_Children(w)[i];

			_XmConfigureObject(child, xx, SPB_MarginHeight(w),
				XtWidth(child), XtHeight(child),
				XtBorderWidth(child));

			xx += XtWidth(child) + SPB_Spacing(w);
		}

	    SPB_DownX(w) = xx;
	    SPB_DownY(w) = yy;
	    SPB_DownWidth(w) = SPB_ArrowSize(w);
	    SPB_DownHeight(w) = SPB_ArrowSize(w);

	    xx += SPB_MarginWidth(w) + SPB_ArrowSize(w) + SPB_Spacing(w);

	    SPB_UpX(w) = xx;
	    SPB_UpY(w) = yy;
	    SPB_UpWidth(w) = SPB_ArrowSize(w);
	    SPB_UpHeight(w) = SPB_ArrowSize(w);
	    break;
	}

	DEBUGOUT(_LtDebug(__FILE__, w,
	    "SpinBoxLayout: down at %d %d up at %d %d, spb size %d %d\n",
	    SPB_DownX(w), SPB_DownY(w), SPB_UpX(w), SPB_UpY(w),
	    XtWidth(w), XtHeight(w)));
	 for (i=0; i<MGR_NumChildren(w); i++) {
		if (XtIsManaged(MGR_Children(w)[i])) {
			Widget	child = MGR_Children(w)[i];
			DEBUGOUT(_LtDebug2(__FILE__, w, child, "x %d y %d sz %d %d\n",
				XtX(child), XtY(child), XtWidth(child), XtHeight(child)));
		}
	}
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
	DEBUGOUT(_LtDebug(__FILE__, w, "QueryGeometry => No.\n"));
	return XtGeometryNo;
    }
    if (Wants(CWWidth) && (geo.width != answer->width))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "QueryGeometry => No.\n"));
	return XtGeometryNo;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "QueryGeometry => Yes.\n"));
    return XtGeometryYes;
}

static XtGeometryResult
GeometryManager(Widget w,
		 XtWidgetGeometry *request,
		 XtWidgetGeometry *reply)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "GeometryManager\n"));

	/* FIX ME */

	*reply = *request;
	return XtGeometryYes;
}

static void
ChangeManaged(Widget w)
{
	XtWidgetGeometry	geo;
	XtGeometryResult	r;

	DEBUGOUT(_LtDebug(__FILE__, w, "ChangeManaged\n"));

	/* Make sure that our size is right */
	PreferredSize(w, NULL, NULL, &geo.width, &geo.height);
	geo.request_mode = CWWidth | CWHeight;

	r = _XmMakeGeometryRequest(w, &geo);
	DEBUGOUT(_LtDebug(__FILE__, w,
		"ChangeManaged Geometry Request %s -> %s\n",
		_LtDebugWidgetGeometry2String(&geo),
		_LtDebugGeometryResult2String(r)));

	if (r == XtGeometryYes) {
		/* FIX ME */
		XtWidth(w) = geo.width;
		XtHeight(w) = geo.height;
	} else {
		/* FIX ME */
	}

	/* Layout */
	Layout(w, NULL);

	_XmNavigChangeManaged(w);
}

Widget
XmCreateSpinBox(Widget parent, char *name,
		    Arg *arglist, Cardinal argcount)
{
    return XtCreateWidget(name, xmSpinBoxWidgetClass, parent,
			  arglist, argcount);
}

/*
 * Globalize the stuff behind the actions.
 *	Calculate the new position based on reason and num.
 *	Num is similar to click-count in other widgets.
 *	Call callback.
 *	Observe callback result.
 *	Update widget if permitted.
 */
static void
SpinBModify(Widget w, XEvent *evp, int reason)
{
	XmSpinBoxCallbackStruct		spb;
	Widget				tw;
	int				oldpos;
	XmString			xms = NULL;
	char				*s = NULL;

	tw = SPB_TextW(w);
	spb.crossed_boundary = False;	/* Detected below */

	if (tw == NULL) {
		DEBUGOUT(_LtDebug(__FILE__, w, "SpinBModify: no text widget\n"));
		return;
	}

	/* Check whether event occurred in one of the arrows */
	if (0)
		return;

#if 0
	/* FIX ME - this only needs to happen in some cases */
	s = XmTextFieldGetString(tw);
	xms = XmStringCreateSimple(s);
#endif

	if (SPBC_Numeric(tw)) {
		oldpos = SPBC_Position(tw);

		switch (reason) {
		case XmCR_SPIN_NEXT:
			SPBC_Position(tw)++;
			if (SPBC_Position(tw) >
				((SPBC_Maximum(tw) - SPBC_Minimum(tw)) / SPBC_Increment(tw))) {
					SPBC_Position(tw) = 0;
					spb.crossed_boundary = True;
			}
			break;
		case XmCR_SPIN_PRIOR:
			SPBC_Position(tw)--;
			if (SPBC_Position(tw) < 0) {
				SPBC_Position(tw) = ((SPBC_Maximum(tw) - SPBC_Minimum(tw))
						/ SPBC_Increment(tw));
				spb.crossed_boundary = True;
			}
			break;
		case XmCR_SPIN_FIRST:
			SPBC_Position(tw) = SPBC_Minimum(tw);
			break;
		case XmCR_SPIN_LAST:
			SPBC_Position(tw) = SPBC_Maximum(tw);
		default:
			return;
		}

		s = _XmSpinBoxNumericString(tw);
		xms = XmStringCreateSimple(s);

		DEBUGOUT(_LtDebug(__FILE__, w,
			"SpinBModify(NUMERIC) %d [%d,%d]\n",
			SPBC_Position(tw), SPBC_Minimum(tw), SPBC_Maximum(tw)));
	} else {
		oldpos = SPBC_Position(tw);

		switch (reason) {
		case XmCR_SPIN_NEXT:
			SPBC_Position(tw)++;
			if (SPBC_Position(tw) >= SPBC_NumValues(tw)) {
				SPBC_Position(tw) = 0;
				spb.crossed_boundary = True;
			}
			break;
		case XmCR_SPIN_PRIOR:
			SPBC_Position(tw)--;
			if (SPBC_Position(tw) <= 0) {
				SPBC_Position(tw) = SPBC_NumValues(tw);
				spb.crossed_boundary = True;
			}
			break;
		case XmCR_SPIN_FIRST:
			SPBC_Position(tw) = 1;
			break;
		case XmCR_SPIN_LAST:
			SPBC_Position(tw) = SPBC_NumValues(tw);
		default:
			return;
		}

		xms = XmStringCopy(SPBC_Values(tw)[SPBC_Position(tw)]);

		DEBUGOUT(_LtDebug(__FILE__, w,
			"SpinBModify(STRING) %d [%d,%d]\n",
			SPBC_Position(tw), 0, SPBC_NumValues(tw)));
	}

	XtFree(s);

	spb.reason = reason;
	spb.event = evp;
	spb.widget = tw;
	spb.doit = True;
	spb.position = SPBC_Position(tw);
	spb.value = xms;

	XtCallCallbackList(w, SPB_ModifyVerifyCB(w), (XtPointer)&spb);

	if (spb.doit) {
		/* Yes, make the modification */

		/* Update text widget */
		_XmSpinBoxShowValue(w, SPB_TextW(w));

		/* Call callback list */
		XtCallCallbackList(w, SPB_ValueChangedCB(w), (XtPointer)&spb);
	} else {
		/* Undo it */
		SPBC_Position(tw) = oldpos;
	}

	XmStringFree(xms);
}

static void
_XmSpinBoxTimer(XtPointer client, XtIntervalId *timerid)
{
	Widget	spb = (Widget)client;

	DEBUGOUT(_LtDebug(__FILE__, spb, "_XmSpinBoxTimer\n"));

	if (SPB_UpArrowPressed(spb))
			SpinBModify(spb, NULL, XmCR_SPIN_NEXT);
	if (SPB_DownArrowPressed(spb))
			SpinBModify(spb, NULL, XmCR_SPIN_PRIOR);

	SPB_TimerId(spb) = XtAppAddTimeOut(XtWidgetToApplicationContext(spb),
			SPB_RepeatDelay(spb), _XmSpinBoxTimer, (XtPointer)spb);
}

static void
SpinBArm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	Widget	tw;

	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBArm at %d %d\n",
		event->xbutton.x, event->xbutton.y));

	if ((tw = SPB_TextW(w)) == NULL)
		return;

	/* In up arrow ? */
	if (SPB_UpX(w) <= event->xbutton.x
			&& event->xbutton.x <= SPB_UpX(w) + SPB_UpWidth(w)
			&& SPB_UpY(w) <= event->xbutton.y
			&& event->xbutton.y <= SPB_UpY(w) + SPB_UpHeight(w)) {
		if (SPBC_ArrowSensitivity(tw) == XmARROWS_SENSITIVE
			|| SPBC_ArrowSensitivity(tw) == XmARROWS_INCREMENT_SENSITIVE) {

			/* Up arrow */
			SPB_UpArrowPressed(w) = True;
			SPB_DownArrowPressed(w) = False;

			SpinBModify(w, event, XmCR_SPIN_NEXT);

			/* Need to initialize timer */
			/* but only if we're supposed to repeat. */
			if (SPB_RepeatDelay(w))
			    SPB_TimerId(w) = XtAppAddTimeOut(
				XtWidgetToApplicationContext(w),
				SPB_InitialDelay(w),
				_XmSpinBoxTimer,
				(XtPointer)w);

		}
	}

	/* In down arrow ? */
	if (SPB_DownX(w) <= event->xbutton.x
			&& event->xbutton.x <= SPB_DownX(w) + SPB_DownWidth(w)
			&& SPB_DownY(w) <= event->xbutton.y
			&& event->xbutton.y <= SPB_DownY(w) + SPB_DownHeight(w)) {
		if (SPBC_ArrowSensitivity(tw) == XmARROWS_SENSITIVE
			|| SPBC_ArrowSensitivity(tw) == XmARROWS_DECREMENT_SENSITIVE) {

			/* Up arrow */
			SPB_UpArrowPressed(w) = False;
			SPB_DownArrowPressed(w) = True;

			SpinBModify(w, event, XmCR_SPIN_PRIOR);

			/* Need to initialize timer */
			/* but only if we're supposed to repeat. */
			if (SPB_RepeatDelay(w))
			    SPB_TimerId(w) = XtAppAddTimeOut(
				XtWidgetToApplicationContext(w),
				SPB_InitialDelay(w),
				_XmSpinBoxTimer,
				(XtPointer)w);
		}
	}
}

static void
SpinBDisarm(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBDisarm\n"));

	if (SPB_TimerId(w))
		XtRemoveTimeOut(SPB_TimerId(w));
	SPB_TimerId(w) = (XtIntervalId)0;

	SPB_UpArrowPressed(w) = False;
	SPB_DownArrowPressed(w) = False;
}

static void
SpinBFirst(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBFirst\n"));
}

static void
SpinBLast(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBLast\n"));
}

static void
SpinBLeft(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBLeft\n"));
}

static void
SpinBRight(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBRight\n"));
}

static void
SpinBNext(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBNext\n"));
}

static void
SpinBPrior(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBPrior\n"));
}

static void
_XmSpinBoxChildFocus(Widget w, XtPointer client, XtPointer call)
{
	Widget	spb = (Widget)client;

	DEBUGOUT(_LtDebug2(__FILE__, spb, w, "_XmSpinBoxChildFocus\n"));

	SPB_TextW(spb) = w;
}

static void
InsertChild(Widget w)
{
    Widget	spb = XtParent(w);

    DEBUGOUT(_LtDebug2(__FILE__, (Widget)spb, w, "SpinBox InsertChild\n"));

#define superclass (&xmManagerClassRec)
    (*superclass->composite_class.insert_child) (w);
#undef superclass

	if (XmeTraitGet((XtPointer)XtClass(w), XmQTaccessTextual)) {
#if 0
		Arg		a;

		DEBUGOUT(_LtDebug2(__FILE__, spb, w,
			"Adding accelerators to child\n"));

		XtSetArg(a, XmNtranslations,
			XtParseTranslationTable(_XmSpinBox_ChildAccelerators));
		XtSetValues(w, &a, 1);
#endif

		SPB_TextW(spb) = w;	/* FIX ME - last text, first, ??? */

		/*
		 * Attach a callback to the text widgets so we know
		 * when they get the focus.
		 */
		XtAddCallback(w, XmNfocusCallback, _XmSpinBoxChildFocus, spb);
	}
}

static char *
_XmSpinBoxNumericString(Widget tw)
{
	char	*s = XtMalloc(20), format[10];
	double	v;
	int		i;

	if (! tw)
		return NULL;

	if (SPBC_DecimalPoints(tw)) {
		sprintf(format, "%%.%df", SPBC_DecimalPoints(tw));

		switch (SPBC_PositionType(tw)) {
		case XmPOSITION_INDEX:
			v = SPBC_Minimum(tw) + SPBC_Position(tw) * SPBC_Increment(tw);
		case XmPOSITION_VALUE:
			v = SPBC_Position(tw);
		}

		for (i=0; i<SPBC_DecimalPoints(tw); i++)
			v /= 10.0;
		sprintf(s, format, v);
	} else {
		switch (SPBC_PositionType(tw)) {
		case XmPOSITION_INDEX:
			i = SPBC_Minimum(tw) + SPBC_Position(tw) * SPBC_Increment(tw);
		case XmPOSITION_VALUE:
			i = SPBC_Position(tw);
		}
		sprintf(s, "%d", i);
	}

	return s;
}


static void
_XmSpinBoxShowValue(Widget spb, Widget t)
{
	char		*s;
	XmString	xms;
	XmAccessTextualTrait	trait;

	DEBUGOUT(_LtDebug(__FILE__, spb, "_XmSpinBoxShowValue\n"));

	if (spb == NULL || t == NULL)
		return;

	trait = (XmAccessTextualTrait)XmeTraitGet((XtPointer)XtClass(t),
	                                           XmQTaccessTextual);

	if (trait == NULL) {
	    _XmError(t, "_XmSpinBoxShowValue: no trait XmQTaccessTextual\n");
	    return;
	} else if (trait->version != 0) {
	    _XmWarning(t, "Version of trait record is %d not %d\n",
						trait->version, 0);
	}

	if (SPBC_ChildType(t) == XmNUMERIC) {
		s = _XmSpinBoxNumericString(t);

		if (trait == NULL)
			XmTextSetString(t, s);
		else
			trait->setValue(t, s, XmFORMAT_WCS);
		XtFree(s);
	} else {
		if (SPBC_Values(t) == NULL)
		    return;	/* FIX ME ??? */

		xms = SPBC_Values(t)[SPBC_Position(t)];

		if (trait == NULL) {
			XmStringGetLtoR(xms, XmFONTLIST_DEFAULT_TAG, &s);
			XmTextSetString(t, s);
			XtFree(s);
		} else
			trait->setValue(t, (XtPointer)xms, XmFORMAT_XmSTRING);
		/* Do *not* free xms */
	}

}

static void
SpinBoxEnter(Widget w, XEvent *evp, String *par, Cardinal *npar)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBoxEnter\n"));
}

static void
SpinBoxFocusIn(Widget w, XEvent *evp, String *par, Cardinal *npar)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBoxFocusIn\n"));
}

static void
SpinBoxFocusOut(Widget w, XEvent *evp, String *par, Cardinal *npar)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "SpinBoxFocusOut\n"));
}

/*
 * Stuff for the Navigator trait
 */
static void
_XmSPBTraitChangeMoveCB(Widget nav,
			XtCallbackProc moveCB,
			XtPointer closure,
			Boolean setUnset)
{
	DEBUGOUT(_LtDebug(__FILE__, nav,
		"_XmSPBTraitChangeMoveCB (%s)\n",
		setUnset ? "set" : "unset"));

	if (setUnset)
		XtAddCallback(nav, XmNvalueChangedCallback, moveCB, closure);
	else
		XtRemoveCallback(nav, XmNvalueChangedCallback, moveCB, closure);
}

static void
_XmSPBTraitSetValue(Widget nav, XmNavigatorData navigatorData, Boolean notify)
{
	Widget	tw;

	DEBUGOUT(_LtDebug(__FILE__, nav, "_XmSPBTraitSetValue\n"));

	tw = SPB_TextW(nav);
	if (tw == NULL)
		return;

	if (navigatorData == NULL)
		return;

	if (navigatorData->valueMask & NavMinimum) {
		SPBC_Minimum(tw) = navigatorData->minimum->x;
	}

	if (navigatorData->valueMask & NavMaximum) {
		SPBC_Maximum(tw) = navigatorData->maximum->x;
	}

	if (navigatorData->valueMask & NavValue) {
		/* FIX ME */
	}

	SPBC_Position(tw) = navigatorData->value->x;

	_XmSpinBoxShowValue(nav, tw);
}

static int
_XmSPBTraitGetValue(Widget nav, XmNavigatorData navigatorData)
{
	DEBUGOUT(_LtDebug(__FILE__, nav, "_XmSPBTraitGetValue\n"));
	return 0;
}

/*
 * Stuff for (calling) the ScrollFrame trait
 */
static void
_XmSPBTraitMoveCB(Widget nav, XtPointer client, XtPointer call)
{
	DEBUGOUT(_LtDebug(__FILE__, nav, "_XmSPBTraitMoveCB\n"));

	/*
	 * This gets called when a navigator (e.g. Notebook) that we're
	 * a registered for (e.g. we're its child) changes what it displays.
	 *
	 * By getting this call we can change our appearance (e.g. page number).
	 */
}


/*
 * An excerpt from http://www.motifdeveloper.com/tips/tip13.html :
 *
 * int XmSpinBoxValidatePosition(Widget text, int *position_value)
 *
 * XmSpinBoxValidatePosition() checks that the text child of an XmSpinBox has a valid position and
 * value. If the current position is valid, it is returned into the address specified by
 * position_value. The function also returns an integer representing the status of the check,
 * as follows: 
 *	XmCURRENT_VALUE 
 *		This is returned if the XmNspinBoxChildType of text is not XmNUMERIC. 
 *	XmMINIMUM_VALUE 
 *		The current XmNposition resource of text is less than the XmNminimumValue
 *		resource of the widget. 
 *	XmMAXIMUM_VALUE 
 *		The current XmNposition resource of text is greater than the XmNmaximumValue
 *		resource of the widget. 
 *	XmINCREMENT_VALUE 
 *		The current XmNposition resource of text falls between the minimum and maximum
 *		bounds set for the widget. In addition, the position should not fall on an
 *		increment boundary. That is, it is not defined by: 
 *			minimum + (n * increment) 
 *		The nearest value (not less than the current value) which does fall on an
 *		increment is calculated, and returned in the position_value parameter of the
 *		function. 
 *	XmNVALID_VALUE 
 *		The current XmNposition resource of text falls between the minimum and maximum
 *		bounds set for the widget. In addition, unlike the case for XmINCREMENT_VALUE,
 *		the position does fall on an increment boundary. 
 * Note that this routine does not modify the current position associated with text. 
 */
extern int
XmSpinBoxValidatePosition(Widget t, int *position)
{
	int			intval, i;
	XmAccessTextualTrait	trait;
	char			*s;
	double			d;

	if (! SPBC_Numeric(t)) {
		return XmCURRENT_VALUE;
	}

	trait = (XmAccessTextualTrait)XmeTraitGet((XtPointer)XtClass(t), XmQTaccessTextual);
	if (trait == NULL) {
		_XmError(t, "_XmSpinBoxShowValue: no trait XmQTaccessTextual\n");
		return XmCURRENT_VALUE;
	} else if (trait->version != 0) {
		_XmWarning(t, "Version of trait record is %d not %d\n", trait->version, 0);
	}

	s = trait->getValue(t, XmFORMAT_WCS);
	if (s == 0) {
		_XmWarning(t, "XmSpinBoxValidatePosition TraitGetValue -> NULL : this shouldn't happen\n");
		return XmCURRENT_VALUE;	/* Maybe the app doesn't crash if we do this */
	}
	d = atof(s);
	for (i=0; i<SPBC_DecimalPoints(t); i++)
		d /= 10;
	intval = d;

	if (intval < SPBC_Minimum(t))
		return XmMINIMUM_VALUE;
	if (intval > SPBC_Maximum(t))
		return XmMAXIMUM_VALUE;

	/* Calculate the nearest greater value that does fall on an increment */
	d = intval;
	d = (d - SPBC_Minimum(t)) / SPBC_Increment(t);
	i = d;
	if (i != d)
		i++;

	*position = SPBC_Minimum(t) + i * SPBC_Increment(t);

	if (i == d)
		return XmVALID_VALUE;
	return XmINCREMENT_VALUE;
}
