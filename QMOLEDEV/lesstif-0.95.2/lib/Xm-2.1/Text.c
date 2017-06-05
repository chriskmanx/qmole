/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Text.c,v 1.3 2005/03/29 15:28:00 dannybackx Exp $
 *
 * Copyright (C) 1997 Free Software Foundation, Inc.
 * Copyright © 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Text.c,v 1.3 2005/03/29 15:28:00 dannybackx Exp $";

#define VERBOSE

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>    /* for  wchar_t */
#include <limits.h>    /* for INT_MAX */
#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif

#include <Xm/XmP.h>
#include <Xm/TextP.h>
#include <Xm/TextOutP.h>
#include <Xm/TextF.h>
#include <Xm/VendorSEP.h>   /* for _XmGetAudibleWarning() only */
#include <Xm/CutPaste.h>
#include <Xm/TransltnsP.h>
#include <Xm/ScrolledW.h>
#include <Xm/XmIm.h>
#include <Xm/TraitP.h>
#include <Xm/AccTextT.h>

#include <XmI/XmI.h>

#include <Xm/AtomMgr.h>
#include <Xm/DragC.h>
#include <Xm/DropSMgr.h>
#include <Xm/DropTrans.h>
#include <XmI/AtomMgrI.h>
#include <XmI/DragDropI.h>
#include <X11/Xatom.h>

#include <XmI/DebugUtil.h>


/* Forward Declarations */

static void ClassInitialize(void);
static void ClassPartInitialize(WidgetClass widget_class);
static void Initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void Destroy(Widget w);
static void Resize(Widget w);
static void Realize(Widget w, XtValueMask *value_mask,
		    XSetWindowAttributes *attributes);
static void DoExpose(Widget w, XEvent *event, Region region);
static XtGeometryResult query_geometry(Widget w,
				      XtWidgetGeometry *proposed,
				      XtWidgetGeometry *answer);
static void _XmTextSetEditable(Widget w, Boolean e);
static Boolean SetValues(Widget current, Widget request, Widget new_w,
			 ArgList args, Cardinal *num_args);
static void RefigureLines(XmTextWidget w);
static void InitializeLineTable(XmTextWidget w);
static Cardinal GetSecResData(WidgetClass wc,
			      XmSecondaryResourceData **resdata);
static OutputCreateProc output_create = _XmTextOutputCreate;
static InputCreateProc input_create = _XmTextInputCreate;
void _XmTextInvalidate(XmTextWidget w, XmTextPosition position,
		       XmTextPosition topos, long delta);
void _XmTextExportValue(Widget w, int offset, XtArgVal *value);
static void TextGetValues(Widget w, ArgList args, Cardinal *num_args);


static XtPointer _XmText_TraitGetValue(Widget w, int format);
static void _XmText_TraitSetValue(Widget w, XtPointer value, int format);
static int _XmText_TraitPreferredFormat(Widget w);

/* Trait record */
XmAccessTextualTraitRec _XmTextTraitRec =
	{       /* version */		0,
		/* getValue */		_XmText_TraitGetValue,
		/* setValue */		_XmText_TraitSetValue,
		/* preferredFormat */	_XmText_TraitPreferredFormat,
	};

#define Offset(_name) XtOffsetOf(XmTextRec, text._name)

/* Resources for the Text  class */
static XtResource resources[] =
{
    {
	XmNsource, XmCSource, XmRPointer,
	sizeof(XmTextSource), Offset(source),
	XmRPointer, (XtPointer)NULL
    },
    {
	XmNactivateCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(activate_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNfocusCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(focus_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNlosingFocusCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(losing_focus_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNvalueChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNmodifyVerifyCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(modify_verify_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNmodifyVerifyCallbackWcs, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(wcs_modify_verify_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNmotionVerifyCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(motion_verify_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNgainPrimaryCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(gain_primary_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNlosePrimaryCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(lose_primary_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNvalue, XmCValue, XmRString,
	sizeof(String), Offset(value),
	XmRString, (XtPointer)NULL
    },
    {
	XmNvalueWcs, XmCValueWcs, XmRValueWcs,
	sizeof(wchar_t *), Offset(wc_value),
	XmRString, (XtPointer)NULL
    },
    {
	XmNmaxLength, XmCMaxLength, XmRInt,
	sizeof(int), Offset(max_length),
	XmRImmediate, (XtPointer)INT_MAX
    },
    {
	XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension,
	sizeof(Dimension), Offset(margin_height),
	XmRImmediate, (XtPointer)5
    },
    {
	XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension,
	sizeof(Dimension), Offset(margin_width),
	XmRImmediate, (XtPointer)5
    },
    {
	XmNoutputCreate, XmCOutputCreate, XmRFunction,
	sizeof(OutputCreateProc), Offset(output_create),
	XmRFunction, (XtPointer)&output_create
    },
    {
	XmNinputCreate, XmCInputCreate, XmRFunction,
	sizeof(InputCreateProc), Offset(input_create),
	XmRFunction, (XtPointer)&input_create
    },
    {
	XmNtopCharacter, XmCTopCharacter, XmRTextPosition,
	sizeof(XmTextPosition), Offset(top_character),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNcursorPosition, XmCCursorPosition, XmRTextPosition,
	sizeof(XmTextPosition), Offset(cursor_position),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNeditMode, XmCEditMode, XmREditMode,
	sizeof(int), Offset(edit_mode),
	XmRImmediate, (XtPointer)XmSINGLE_LINE_EDIT
    },
    {
	XmNautoShowCursorPosition, XmCAutoShowCursorPosition, XmRBoolean,
	sizeof(Boolean), Offset(auto_show_cursor_position),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNeditable, XmCEditable, XmRBoolean,
	sizeof(Boolean), Offset(editable),
	XmRImmediate, (XtPointer)True
    },
    {
	XmNverifyBell, XmCVerifyBell, XmRBoolean,
	sizeof(Boolean), Offset(verify_bell),
	XtRImmediate, (XtPointer)((unsigned char)XmUNSPECIFIED)
    },
    {
	XmNnavigationType, XmCNavigationType, XmRNavigationType,
    sizeof(XmNavigationType), XtOffsetOf(XmTextRec, primitive.navigation_type),
	XmRImmediate, (XtPointer)XmTAB_GROUP
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
	XmNvalue,
	sizeof(String), Offset(value),
	_XmTextExportValue, NULL
    }
};
#undef Offset

static XmBaseClassExtRec textBaseClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,                             
    /* version                   */ XmBaseClassExtVersion,
    /* size                      */ sizeof(XmBaseClassExtRec),
    /* initialize_prehook        */ XmInheritInitializePrehook,
    /* set_values_prehook        */ XmInheritSetValuesPrehook, 
    /* initialize_posthook       */ XmInheritInitializePosthook,
    /* set_values_posthook       */ XmInheritSetValuesPosthook,
    /* secondary_object_class    */ NULL, /* FIX ME */
    /* secondary_object_create   */ XmInheritSecObjectCreate,
    /* get_secondary_resources   */ GetSecResData,
    /* fast_subclass             */ { 0 },
    /* get_values_prehook        */ XmInheritGetValuesPrehook,
    /* get_values_posthook       */ XmInheritGetValuesPosthook,
    /* class_part_init_prehook   */ XmInheritClassPartInitPrehook,
    /* class_part_init_posthook  */ XmInheritClassPartInitPosthook,
    /* ext_resources             */ NULL,
    /* compiled_ext_resources    */ NULL,
    /* num_ext_resources         */ 0,
    /* use_sub_resources         */ False,
    /* widget_navigable          */ XmInheritWidgetNavigable,
    /* focus_change              */ XmInheritFocusChange,
    /* wrapper_data              */ NULL
};

XmPrimitiveClassExtRec _XmTextPrimClassExtRec =
{
    /* next_extension      */ NULL,
    /* record_type         */ NULLQUARK,
    /* version             */ XmPrimitiveClassExtVersion,
    /* record_size         */ sizeof(XmPrimitiveClassExtRec),
    /* widget_baseline     */ _XmTextGetBaselines,
    /* widget_display_rect */ _XmTextGetDisplayRect,
    /* widget_margins      */ _XmTextMarginsProc
};

XmTextClassRec xmTextClassRec =
{
    /* Core class part */
  {
	/* superclass            */ (WidgetClass) & xmPrimitiveClassRec,
	/* class_name            */ "XmText",
	/* widget_size           */ sizeof(XmTextRec),
	/* class_initialize      */ ClassInitialize,
	/* class_part_initialize */ ClassPartInitialize,
	/* class_inited          */ False,
	/* initialize            */ Initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ Realize,
	/* actions               */ NULL, /* set in ClassInitialize */
	/* num_actions           */ 0, /* set in ClassInitialize */
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressMaximal,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ Destroy,
	/* resize                */ Resize,
	/* expose                */ DoExpose,
	/* set_values            */ SetValues,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ TextGetValues,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ query_geometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer) & textBaseClassExtRec
  },
    /* Primitive Class part */
  {
	/* border_highlight      */ XmInheritBorderHighlight,
	/* border_unhighlight    */ XmInheritBorderUnhighlight,
	/* translations          */ NULL,
	/* arm_and_activate_proc */ NULL,
	/* synthetic resources   */ syn_resources,
	/* num syn res           */ XtNumber(syn_resources),
	/* extension             */ (XtPointer) & _XmTextPrimClassExtRec,
  },
    /* Text Class part */
  {
	/* extension */ NULL
  }
};

WidgetClass xmTextWidgetClass = (WidgetClass)&xmTextClassRec;



/* Inner Widget--------------------------------------------------------------
 * the base for the Output and Input objects
 */
/*
 * Is this function ever used ?
 */
static void
InnerInitialize(Widget request, Widget new_w, ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "InnerInitialize: %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

}

/*
 * Is this function ever used ?
 */
static void
InnerDestroy(Widget w)
{
    /* Destroy Output and Input */
    /* Nah - already done in XmText's destroy method (Destroy). */
    DEBUGOUT(_LtDebug(__FILE__, w, "InnerDestroy\n"));
}

/*
 * Is this function ever used ?
 */
static Boolean
InnerSetValues(Widget old, Widget request, Widget new_w,
	       ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "InnerSetValues: %i args\n"
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

    return refresh_needed;
}


XmTextInnerClassRec xmTextInnerClassRec =
{
    /* core_class fields */
    {
    /* superclass               */ (WidgetClass)(&objectClassRec),
    /* class_name               */ "XmTextInner",
    /* widget_size              */ sizeof(XmTextInnerRec),
    /* class_initialize         */ NULL,
    /* class_part_initialize    */ NULL,
    /* class_inited             */ False,
    /* initialize               */ InnerInitialize,
    /* initialize_hook          */ NULL,
    /* obj1                     */ NULL,
    /* obj2                     */ NULL,
    /* obj3                     */ 0,
    /* resources                */ NULL,
    /* num_resources            */ 0,
    /* xrm_class                */ NULLQUARK,
    /* obj4                     */ False,
    /* obj5                     */ False,
    /* obj6                     */ False,
    /* obj7                     */ False,
    /* destroy                  */ InnerDestroy,
    /* obj8                     */ NULL,
    /* obj9                     */ NULL,
    /* set_values               */ InnerSetValues,
    /* set_values_hook          */ NULL,
    /* obj10                    */ NULL,
    /* get_values_hook          */ NULL,
    /* obj11                    */ NULL,
    /* version                  */ XtVersion,
    /* callback_private         */ NULL,
    /* obj12                    */ NULL,
    /* obj13                    */ NULL,
    /* obj14                    */ NULL,
    /* extension                */ NULL
    },
    /* TextInner Class part */
    {
	/* extension */ NULL
    }
};

WidgetClass xmTextInnerObjectClass = (WidgetClass)&xmTextInnerClassRec;


/* Text-----------------------------------------------------------------------

 * Widget methods
 */


static void
ClassInitialize(void)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "ClassInitialize\n"));

    textBaseClassExtRec.record_type = XmQmotif;
    xmTextClassRec.core_class.actions =
	(XtActionsRec *)_XmdefaultTextActionsTable;

    xmTextClassRec.core_class.num_actions = _XmdefaultTextActionsTableSize;

    if (!XmeTraitSet((XtPointer)xmTextWidgetClass, XmQTaccessTextual,
                     (XtPointer) &_XmTextTraitRec)) {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
		"ClassInitialize: XmeTraitSet failed\n"));
    }
}


static void
ClassPartInitialize(WidgetClass widget_class)
{
    int len1 = strlen(_XmTextIn_XmTextEventBindings1);
    int len2 = strlen(_XmTextIn_XmTextEventBindings2);
    int len3 = strlen(_XmTextIn_XmTextEventBindings3);
    char *buf = XtMalloc((unsigned)(len1 + len2 + len3 + 1));
    char *cp = buf;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "ClassPartInitialize\n"));

    /* Moved from ClassInitialize because the Xt routine
     * CoreClassPartInitialize (Xt/Core.c) will overwrite
     * the original core_class.tm_table, thus leaking any
     * malloc'd buffer assigned to tm_table.
     */
    (void)strcpy(cp, _XmTextIn_XmTextEventBindings1);
    cp += len1;
    (void)strcpy(cp, _XmTextIn_XmTextEventBindings2);
    cp += len2;
    (void)strcpy(cp, _XmTextIn_XmTextEventBindings3);

    xmTextClassRec.core_class.tm_table = (String)XtParseTranslationTable(buf);

    XtFree(buf);

    _XmFastSubclassInit(widget_class, XmTEXT_BIT);
}


static void
Initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    XmTextWidget w = (XmTextWidget)new_w;
    Boolean passedSourceAsArg;
    Atom import_target[3];
    Arg ds_args[6];
    int n = 0;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "initialize: %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new   X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    if (XtWidth(request) == 0)
    {
	XtWidth(new_w) = 0;
    }
    if (XtHeight(request) == 0)
    {
	XtHeight(new_w) = 0;
    }

#if 0
    XtAugmentTranslations((Widget)w,
			  XtParseTranslationTable(_XmText_EventBindings2));

    XtAugmentTranslations((Widget)w,
			  XtParseTranslationTable(_XmText_EventBindings3));
#endif

    w->text.inner_widget = XtCreateWidget("inner", xmTextInnerObjectClass,
					  new_w, args, *num_args);

    /* initialize the inner to sane values before fetching resources */
    memset((char *)(w->text.inner_widget) + XtOffsetOf(XmTextInnerRec, inner),
	   0, sizeof(XmTextInnerPart));

    Text_TableSize(w) = Text_TotalLines(w) = 0;
    Text_LineTable(w) = NULL;
    Text_Line(w) = NULL;
    Text_LineMax(w) = Text_LineCount(w) = 0;
    Text_DisableDepth(w) = 0;
    Text_FirstPos(w) = Text_LastPos(w) = Text_TopPos(w) = 0;
    Text_DestPosition(w) = Text_CursorPos(w);

    (*Text_OutputCreate(w)) ((Widget)w, args, *num_args);

    Text_CursorPositionX(w) = Text_OutputData(w)->leftmargin;
    passedSourceAsArg = ( Text_Source(w) != 0 );

    (*Text_InputCreate(w)) ((Widget)w, args, *num_args);

    InitializeLineTable(w);
    if ( Text_CursorPos(w) > GetSrc(w)->data->length )
	Text_CursorPos(w) = GetSrc(w)->data->length;

    Text_InRedisplay(w) = False;
    Text_InRefigureLines(w) = False;
    Text_InExpose(w) = False;
    Text_InResize(w) = False;
    Text_InSetValues(w) = False;
    Text_NeedsRedisplay(w) = True;
    Text_NeedsRefigureLines(w) = False;

	/* The following seem to be the same as M*tif initializes */
    Text_AddMode(w) = False; 
    Text_PendingOff(w) = True; 
    Text_ForgetPast(w) = PASTENDPOS;
    Text_TotalLines(w) = 1;
    Text_ForceDisplay(w) = -1;
    Text_Traversed(w) = False;
    Text_OnOrOff(w) = False;
    Text_NewTop(w) = 0;
    Text_LastTopChar(w) = 0;
    Text_TopLine(w) = 0;
    Text_VSBarScrolling(w) = 0;
    Text_TableIndex(w) = 0;

    /* Highlight array */
    Text_Highlight(w).list = (_XmHighlightRec *)XtCalloc(32,
				      sizeof(_XmHighlightRec));
    Text_Highlight(w).maximum = 32;
    Text_Highlight(w).number = 0;


    Text_Highlight(w).list[ 0 ].position = 0;
    Text_Highlight(w).list[ 0 ].mode = XmHIGHLIGHT_NORMAL;

    Text_OldHighlight(w).maximum = 1;
    Text_OldHighlight(w).number = 0;
    Text_OldHighlight(w).list = XtNew(_XmHighlightRec);

    Text_Repaint(w).number = 1;
    Text_Repaint(w).maximum = 1;
    Text_Repaint(w).range = XtNew(RangeRec);
    Text_Repaint(w).range[0].from=PASTENDPOS;
    Text_Repaint(w).range[0].to = 0;
    Text_PendingScroll(w) = 0;
    Text_HighlightChanged(w) = False;
	/* FIXME : when/how is char_size more than 1 */
    Text_CharSize(w) = 1;

    if ( Text_VerifyBell(w) == (Boolean)XmUNSPECIFIED )
	{
	Widget vendor = XtParent(w);
	while ( vendor && !XmIsVendorShell( vendor ) )
	    vendor = XtParent( vendor );
	if ( vendor )
	    Text_VerifyBell(w) = _XmGetAudibleWarning(vendor);
	}

    if ( (Text_Value(w) && strlen(Text_Value(w))> 0) || passedSourceAsArg )
	{
	_XmTextUpdateLineTable( (Widget)w, 0, strlen( Text_Value(w) ),
		       NULL, True /* it is unused */ );
	Text_BottomPos(w) = Text_LineTable(w)[Text_LineCount(w)].start_pos;
	Text_Repaint(w).range[0].to = Text_BottomPos(w);
	}
     Text_Value(w) = NULL; /* amai: FIX ME! We should check for value! */
     Text_WcsValue(w) = NULL; /* amai: FIX ME! We should check for value! */

    /* initialize default DropSite */
    import_target[0] = XmInternAtom(XtDisplay(new_w), _XA_COMPOUND_TEXT, False);
    import_target[1] = XmInternAtom(XtDisplay(new_w), _XA_TEXT, False);
    import_target[2] = XA_STRING;
    XtSetArg(ds_args[n], XmNimportTargets, import_target); n++;
    XtSetArg(ds_args[n], XmNnumImportTargets, 3); n++;
    XtSetArg(ds_args[n], XmNdropSiteOperations, XmDROP_COPY|XmDROP_MOVE); n++;
    XtSetArg(ds_args[n], XmNdropProc, _Lttext_process_drop); n++;

    XmDropSiteRegister(new_w, ds_args, n);
}


static void
Destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Destroy\n"));

    XmImCloseXIM(w);

    /* rws 2 Oct 1998
     * This is odd, I thought Xt should be taking care of this
     *
     * Danny 30 Nov 1998 : only if the parent widget (XmText) is a composite.
     *   Otherwise (like here) Xt doesn't have the information.
     */
    (*Text_Input(w)->destroy) (w);
    (*Text_Output(w)->destroy) (w);
    XtDestroyWidget(((XmTextWidget)w)->text.inner_widget);

    /* rws 1 Apr 1999
     */
    XtFree((char *)Text_LineTable(w));
    if(Text_Line(w)) XtFree((char *)Text_Line(w));
    XtFree((char *)Text_Highlight(w).list);
    XtFree((char *)Text_OldHighlight(w).list);
    XtFree((char *)Text_Repaint(w).range);

    /* amai: not sure this is necessary ...
             Check out the register calls for more info.
    XmDropSiteUnregister(w); */
}


static Boolean
SetValues(Widget old, Widget request, Widget new_w,
	  ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;
    XmTextWidget nw = (XmTextWidget)new_w;
    XmTextWidget ow = (XmTextWidget)old;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "set_values: %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new   X %5i Y %5i W %5i H %5i\n",
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    Text_InSetValues(nw) = True;

	if (nw->core.sensitive != ow->core.sensitive) {
		OutputData o = Text_OutputData(new_w);
		XGCValues	v;
		int		m;

		if (! nw->core.sensitive) {
			m = GCFillStyle | GCStipple;
			v.fill_style = FillStippled;
			v.stipple = XmGetPixmapByDepth(XtScreen(new_w), "50_foreground", 1, 0, 1);
		} else {
			m = GCFillStyle;
			v.fill_style = FillSolid;
		}

		if (XtIsRealized(new_w))
			XChangeGC(XtDisplay(new_w), Out_DrawGC(o), m, &v);
		refresh_needed = True;
	}

    if (Text_WcsValue(nw))
    {
	XmTextSetStringWcs(new_w, Text_WcsValue(new_w));
	Text_WcsValue(nw) = NULL;
	refresh_needed = True;
    } else if (Text_Value(nw))
    {
	XmTextSetString(new_w, Text_Value(new_w));
	Text_Value(nw) = NULL;
	refresh_needed = True;
    }


    if (Text_Editable(nw) != Text_Editable(ow))
    {
	_XmTextSetEditable(new_w, Text_Editable(nw));
	refresh_needed = True;
    }

    /* Stuff that (hopefully) requires no further treatment,
     * but does require redisplay */
    if (Text_EditMode(nw) != Text_EditMode(ow) ||
	Text_AutoShowCursorPosition(nw) != Text_AutoShowCursorPosition(ow))
    {
	refresh_needed = True;
    }

    /* Call the SetValues in the input/output objects */
    (*Text_Input(new_w)->SetValues) (old, request, new_w, args, num_args);

    if ((*Text_Output(new_w)->SetValues) (old, request, new_w, args, num_args))
    {
	refresh_needed = True;
    }

    /* FIX ME - lots more cases */
    /*
     * From include/Xm/TextP.h :
     * XmTextSource source
     * XtCallbackList activate_callback, focus_callback, losing_focus_callback,
     *    value_changed_callback, modify_verify_callback,
     *    wcs_modify_verify_callback, motion_verify_callback,
     *    gain_primary_callback, lose_primary_callback
     * Dimension margin_height, margin_width, cursor_position_x
     * OutputCreateProc output_create
     * InputCreateProc input_create;
     *
     * XmTextPosition top_character;
     * XmTextPosition bottom_position;
     * XmTextPosition cursor_position;
     * int max_length;
     * Boolean add_mode;
     * Boolean traversed;
     * Boolean highlight_changed;
     * Boolean pendingoff;
     * char char_size;
     * OnOrOff on_or_off;
     * Output output;
     * Input input;
     * XmTextPosition first_position;
     * XmTextPosition last_position;
     * XmTextPosition forget_past;
     * XmTextPosition force_display;
     * XmTextPosition new_top;
     * XmTextPosition last_top_char;
     * XmTextPosition dest_position;
     * int disable_depth;
     * int pending_scroll;
     * int total_lines;
     * int top_line;
     * int vsbar_scrolling;
     * Cardinal number_lines;
     * Cardinal maximum_lines;
     * Line line;
     * Widget inner_widget;
     * XmTextLineTable line_table;
     * unsigned int table_size;
     * unsigned int table_index;
     */

    Text_InSetValues(nw) = False;
    if ( refresh_needed ) /*CP: then we can "force" a complete RefigureLines call by: */
	Text_ForgetPast(nw) = 0;

    return refresh_needed;
}


static void
Realize(Widget aw, XtValueMask *value_mask, XSetWindowAttributes *attributes)
{
    XmTextWidget w = (XmTextWidget)aw;

    DEBUGOUT(_LtDebug(__FILE__, aw, "%s:Realize(%d) - valuemask 0x%X - %dx%d\n",
    		__FILE__, __LINE__, *value_mask,
    		XtWidth(aw), XtHeight(aw)));

#define superclass (&xmPrimitiveClassRec)
    (*superclass->core_class.realize) (aw, value_mask, attributes);
#undef superclass

    RefigureLines(w);

    (*Text_Output(w)->realize) (aw, value_mask, attributes);

    _XmTextSetEditable(aw, Text_Editable(w));

    XmTextShowPosition(aw, Text_CursorPos(w));
}


static void
DoExpose(Widget aw, XEvent *event, Region region)
{
    XmTextWidget w = (XmTextWidget)aw;

    if (XtWindow(aw) == 0)
	return;

    if (event == NULL)
    {
        XClearArea(XtDisplay(aw),XtWindow(aw),0,0,0,0,True);
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, aw, "DoExpose\n"));
    if (Text_DisableDepth(w) != 0)
	return;		/* Should we be doing this ? FIX ME */

    if (!XtIsRealized(w))
    {
	return;
    }

    if (Text_NeedsRefigureLines(w))
    {
	RefigureLines(w);
    }

    if ( Text_InExpose(w) )
	return;

    Text_InExpose(w)= True;

    (*Text_Output(w)->expose) (aw, event, region);
    Text_InExpose(w)= False;
}


static void
Resize(Widget aw)
{
    XmTextWidget w = (XmTextWidget)aw;

    DEBUGOUT(_LtDebug(__FILE__, aw, "%s:resize(%d) - %dx%d\n",
    		__FILE__, __LINE__,
    		XtWidth(aw), XtHeight(aw)));

    /* Resize can get called from XmScrolledWindowSetAreas before we actually
     * made it through our initialize method.  Make sure not to die because
     * of this
     */
    if (Text_Input(w) == NULL || Text_Output(w) == NULL)
    {
	return;
    }

    if ( Text_InResize( w ) )
    	return;

    Text_ForgetPast(w) = 0; /*CP: so that we recalculate all lines again */
    Text_InResize( w ) = True;

    (*Text_Output(w)->resize) (aw, True);

    Text_InResize( w ) = False;

    RefigureLines(w);
}


static XtGeometryResult
query_geometry(Widget w, XtWidgetGeometry *proposed, XtWidgetGeometry *answer)
{
#if 0
    XmTextWidget tw = (XmTextWidget)w;
    XtWidgetGeometry a;

#define Wants(x)   (proposed->request_mode & x)
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "query_geometry proposed width=%d height=%d mode=%X\n",
		   proposed->width, proposed->height, proposed->request_mode));

    if (proposed->request_mode != 0)
    {				/* NULL case should not yet end here ! */
	if ((!(Wants(CWWidth))) && (!Wants(CWHeight)))
	{
	    /* If they don't ask width/height, let them have what they like */
	    if (answer)
	    {
		*answer = *proposed;
	    }

	    return XtGeometryYes;
	}
    }

    a.request_mode = CWWidth | CWHeight;
    a.width = XtWidth(tw);
    a.height = XtHeight(tw);

    if (answer)
    {
	*answer = a;
    }

    /* NULL proposed -> return Width+Height */
    if (proposed->request_mode == 0)
    {
	return XtGeometryAlmost;
    }

    if (proposed->width >= answer->width && proposed->height >= answer->height)
    {
	return XtGeometryYes;
    }
    else if (answer->width == XtWidth(tw) && answer->height == XtHeight(tw))
    {
	if (answer)
	{
	    answer->request_mode = 0;
	}

	return XtGeometryNo;
    }
    else
    {
	return XtGeometryAlmost;
    }
#else
    DEBUGOUT(_LtDebug(__FILE__, w,
		      "query_geometry proposed width=%d height=%d mode=%X\n",
		   proposed->width, proposed->height, proposed->request_mode));

    answer->width = XtWidth(w);
    answer->height = XtHeight(w);

    return _XmGMReplyToQueryGeometry(w, proposed, answer);
#endif
}

/* Drawing Routines ------------------------------------------------------- */
static void
Redisplay(XmTextWidget w)
{
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "Redisplay\n"));

    if (Text_DisableDepth(w) != 0)
	return;

    if (!XtIsRealized((Widget)w))
    {
	return;
    }

    if ( Text_InRedisplay(w) )
	return;

    Text_InRedisplay(w) = True;

    Text_NeedsRedisplay(w) = False;

    if (Text_NeedsRefigureLines(w))
    {
	RefigureLines(w);
    }

    _XmChangeVSB(w, Text_TopPos(w));
    {
    OutputData o = Text_OutputData(w);

    	_XmRedisplayHBar(w, Out_XOffset(o));
    }

    (*Text_Output(w)->expose) ((Widget)w, NULL, NULL);
    Text_InRedisplay(w) = False;
}

static Cardinal
GetSecResData(WidgetClass wc, XmSecondaryResourceData **resdata)
{
    return 0;
}


/* Line Table Routines ---------------------------------------------------- */

static void
LineIncrease(XmTextWidget w, LineNum num)
{
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "LineIncrease %d\n",num));
	if (num > Text_LineMax(w)) {
		LineNum start = Text_LineMax(w);
		Cardinal i;

		Text_Line(w) = (Line)XtRealloc((char *)Text_Line(w),
				sizeof(LineRec) * num);
		Text_LineMax(w) = num;

		for (i = start; i < num; i++) {
			Line line = &Text_Line(w)[i];
			line->extra = NULL;
		}
	}
}

/*
 * Set up the entire line table, starting with the first visible position.
 *
 * Note that there is 1 extra line in the table.  This is so we can find
 * out what the last position in any line is by looking at the first
 * position of the next line.  Oh, the limitations of Motif compatibility!
 */
/*
 * This new implementation of RefigureLines is necessary to implement
 * wrapping.
 * Inspection of the output of test/Xm/text/test6 reveals that Lines can
 * be built from the LineTable. As the LineTable has recently been reprogrammed
 * to contain wrapped lines, is seems logical to use that idea.
 * Some people will accuse me of NIH now :-)
 */
static void
RefigureLines(XmTextWidget w)
{
    XmTextPosition pos, next;
    int nlines;
    Cardinal i;
    Line line = NULL;
    XmTextLineTable lte;
    LineTableExtra extra;
    Boolean more;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w, "RefigureLines\n"));
/*
 * DisableDepth is meant to optimize frequent updates to XmText.
 * This probably means we shouldn't do anything here ...
 *
 * Not too sure about this. FIX ME.
 */
    if (Text_DisableDepth(w) != 0) {
	Text_NeedsRefigureLines(w) = True;
	return;
    }
/* End unsure stuff */

    Text_NeedsRefigureLines(w) = False;

    /* Too verbose to stay on ... */
#ifdef VERBOSE
    /* Show our input */
    for (i = 0;
	 i < Text_TableSize(w) && (i == 0 ||
				   Text_LineTable(w)[i].start_pos != 0);
	 i++)
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "RefigureLines: [%d] start_pos %d virt %d\n",
			  i, Text_LineTable(w)[i].start_pos,
			  Text_LineTable(w)[i].virt_line));
    }
#endif

    pos = Text_TopPos(w);
    nlines = 0;

    /* Find line to start with */
    i = 0;
    lte = Text_LineTable(w);

    if (pos)
    {
	for (i = 1, lte = Text_LineTable(w) + i;
	     i < Text_TableSize(w) && lte->start_pos;
	     i++, lte = Text_LineTable(w) + i)
	{
	    if (lte->start_pos >= pos)
	    {
		break;
	    }
	}
    }

    for (;
	 i < Text_TableSize(w) && (lte->start_pos || i == 0);
	 i++, lte = Text_LineTable(w) + i)
    {
	if (nlines >= Text_LineMax(w))
	{
	    LineIncrease(w, nlines + 16);
	}

	line = &Text_Line(w)[nlines];

	if (line->extra)
	{
	    XtFree((char *)line->extra);
	}

	extra = NULL;
	more = (*Text_Output(w)->MeasureLine) ((XmTextWidget)w, nlines,
					       pos, &next, &extra);

	line->start = lte->start_pos;
	line->changed = False;
	line->changed_position = 0;
	line->extra = extra;

	pos = next;
	nlines++;
    }

    Text_LineCount(w) = nlines;
    Text_TopLine(w) = _XmTextGetTableIndex(w, Text_TopPos(w));

    /* Not sure about this ...
     * Do we need to provide a line beyond the last real line in the text ?
     */
    if (nlines >= Text_LineMax(w))
    {
	LineIncrease(w, nlines + 16);
    }

    line = &Text_Line(w)[nlines];
    if (line->extra)
    {
	XtFree((char *)line->extra);
    }

    line->start = PASTENDPOS;	/* next wasn't good enough */
    line->changed = False;
    line->changed_position = 0;
    line->extra = NULL;
    /* End unsure part */

    if (Text_BottomPos(w) == 0)
    {
	Text_BottomPos(w) = INT_MAX;
    }

    else
    {
	Text_BottomPos(w) = line->start;
    }

    /* This is too verbose to stay on */
#ifdef VERBOSE
    if (_LtDebugInDebug(__FILE__, (Widget)w))
    {
	int i;

	for (i = 0; i <= Text_LineCount(w); i++)
	{
	    Line line = &Text_Line(w)[i];

	    if (line->extra)
	    {
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
				"RefigureLines: line[%d]: start=%d width=%d\n",
				  i, line->start, line->extra->width));
	    }
	    else
	    {
		DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
				"RefigureLines: line[%d]: start=%d width=NA\n",
				  i, line->start));
	    }
	}
    }

#ifdef VERBOSE
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "RefigureLines: top on line %d, bottompos %d\n",
		      i, Text_BottomPos(w)));
#endif
#endif
}

/* Line-table functions (for the whole textstring)------------------------ */

/* Running test/Xm/test6 (be sure to give it focus so something interesting
 * happens!) against Motif reveals that the line table is structured like this :
 *      - the size of its array (initially 64) is in table_size
 *      - table_index is NOT the highest used item; not sure what it's for
 *      - the array starts at the beginning of the text widget, not at the
 *        visible top. Therefore the first entry always contains 0,0.
 *      - the start_pos field obviously points to the first position of the line
 *      - the virt_line field is 1 if this is a line that's not really there in
 *        the text; it is however a line created by wrapping.
 *      - the final entry in the list has 0 start_pos.
 *
 * It also turns out that there's at least *three* places in the XmText
 *      record where stuff is stored for every line :
 *      - the text itself (in the source) as a normal string
 *      - an array of "Line"s
 *      - a LineTable
 */
#define LINETABLE_INCREMENT	64


/*
 * The block and update parameters are currently unused because I don't
 * know what they should be used for.
 */
extern void
_XmTextUpdateLineTable(Widget w,
		       XmTextPosition start,
		       XmTextPosition end,
		       XmTextBlock block,
		       Boolean update)
{
	Cardinal	i;
	XmTextWidget	tw = (XmTextWidget)w;
	XmTextPosition	next, ostart;
	XmTextLineTable line;
	unsigned int	index;
	Boolean		wrap = _XmTextShouldWordWrap(tw), next_is_virtual;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTextUpdateLineTable start %d end %d\n",
    		start, end));
    Text_NeedsRefigureLines(w) = True;
    Text_NeedsRedisplay(w) = True;

    /* This is a hack. Xinvest becomes happier */
    if (start == 0 && end == 0) {
	/* We must initialize the whole line table; let's make sure it ends
	 * with zeroes.
	 */
	for (index=0; index < Text_TableSize(tw); index++) {
		Text_LineTable(tw)[index].start_pos = 0;
		Text_LineTable(tw)[index].virt_line = False;
	}
    }

    /* Initialize ourselves */
    if (start == 0)
    {
	index = 0;
    }
    else
    {
	index = _XmTextGetTableIndex(tw, start);
	start = Text_LineTable(tw)[index].start_pos;
    }

    end = Text_LastPos(w);

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "_XmTextUpdateLineTable(start %d end %d update %s) %s\n",
		      start, end, _LtDebugBoolean2String(update),
		      wrap ? "wrap" : ""));

/* rws 10 Feb 1999
   text/test14, grok phone database, note field.
   I suspect that this should be done in TextOut.c not here, and somehow
   this thing should be able to shrink to some sort of minimum size as the
   text gets smaller.
 */
/*CP:19 May 1999: The difference with GROK and other scrollable
   XmText widgets is that the scrollable window is created first and then
   the XmText widget is created (i.e. it does not use the convenience 
   function XmCreateScrolledText ). When this happens then XmText
   does not have the XmScrolledWindow as it's parent and therefor
   all lines in the XmText must be drawn hence the need to increase
   the lines and column count of the output window.
   This way of creating scrollable XmText windows is not efficient!
   As Rick above states, I am also not sure that this should not be in
   TextOut.c.
*/
    if(!XmIsScrolledWindow(XtParent(w)))
        {
        int max_lines = 1;
        int max_cols = 0;
        int col = 0;
        int index;
        String value = XmTextGetString(w);
        OutputData od = Text_OutputData(w);
        Arg args[2];
        int n,len;

        len=Text_LastPos(w);
    	for (index = 0; index < len; index++)
    	{
    		switch (value[index])
    		{
    		case '\n':
    			max_lines++;
    			col = 0;
    			break;
    		default:
    			col++;
    			break;
    		}
		if (col > max_cols)
		{
		    max_cols = col;
		}
    	}
	max_cols = max_cols < Out_ColumnsSet(od) ? Out_ColumnsSet(od) : max_cols;
	max_lines = max_lines < Out_RowsSet(od) ? Out_RowsSet(od) : max_lines;

	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "_XmTextUpdateLineTable - %d %d %d %d\n",
		      max_lines, max_cols, Out_Rows(od), Out_Columns(od)));
    	XtFree(value);
    	n = 0;
    	if (!wrap && !Out_ScrollHorizontal(od) && max_cols != Out_Columns(od))
    	{
	    XtSetArg(args[n], XmNcolumns, max_cols); n++;
    	}
    	if (!Out_ScrollVertical(od) && max_lines != Out_Rows(od))
    	{
	    XtSetArg(args[n], XmNrows, max_lines); n++;
    	}
    	if (n > 0)
    	{
	    XtSetValues(w, args, n);
    	}
    }

    if (wrap)
    {
	/*
	 * Word-wrapping is on.
	 */
	while (end >= start)
	{
	    if (index >= Text_TableSize(w))
	    {
		i = Text_TableSize(w);

#if 0
		Text_TableSize(w) += LINETABLE_INCREMENT;
#else
		if (Text_TableSize(w) <= LINETABLE_INCREMENT)
			Text_TableSize(w) = 2 * LINETABLE_INCREMENT;
		else
			Text_TableSize(w) *= 2;
#endif
		Text_LineTable(w) = (XmTextLineTable)
		    XtRealloc((char *)Text_LineTable(w),
			      sizeof(XmTextLineTableRec) * Text_TableSize(w));

		for (; i < Text_TableSize(w); i++)
		{
		    Text_LineTable(w)[i].start_pos = 0;
		    Text_LineTable(w)[i].virt_line = False;
		}
	    }

	    /*
	     * So we have ourselves a line of text. We may have to split it up
	     * visually, wrapping around at word boundaries.
	     *
	     * This first line is never a virtual one. If we pass through this
	     * inner loop a second time (or more), then those lines are
	     * virtual ones.
	     */
	    next_is_virtual = False;
	    if (start < 0)
	    {
		start = 0;
		break;
	    }
	    else
	    {
		do
		{
		    ostart = start;

		    /* FIX ME : need to fill up extra ? */
		    next = _XmTextFindLineEnd(tw, start, NULL);

		    line = Text_LineTable(w) + index;
		    line->start_pos = start;
		    line->virt_line = next_is_virtual;

		    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		       "_XmTextUpdateLineTable: line %d start %d virtual %d\n",
				      index, line->start_pos, line->virt_line));

		    next_is_virtual = True;	/* See comment above */

		    start = next + 1;
		    index++;
		}
		while (next != PASTENDPOS && next > ostart &&
		       index < Text_TableSize(w));
	    }
	}
    }
    else
    {
	/*
	 * No word-wrapping.
	 */
	while (end >= start)
	{
	    if (index >= Text_TableSize(w))
	    {
		Cardinal i = Text_TableSize(w);

#if 0
		Text_TableSize(w) += LINETABLE_INCREMENT;
#else
		if (Text_TableSize(w) <= LINETABLE_INCREMENT)
			Text_TableSize(w) = 2 * LINETABLE_INCREMENT;
		else
			Text_TableSize(w) *= 2;
#endif
		Text_LineTable(w) = (XmTextLineTable)
		    XtRealloc((char *)Text_LineTable(w),
			      sizeof(XmTextLineTableRec) * Text_TableSize(w));

		for (; i < Text_TableSize(w); i++)
		{
		    Text_LineTable(w)[i].start_pos = 0;
		    Text_LineTable(w)[i].virt_line = False;
		}
	    }

	    line = Text_LineTable(w) + index;
	    line->start_pos = start;
	    line->virt_line = False;

	    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		       "_XmTextUpdateLineTable nowrap : line %d start %d virtual %d\n",
				      index, line->start_pos, line->virt_line));

	    /* Be careful not to do PASTENDPOS + 1 */
	    start = (*Text_Source(w)->Scan) (Text_Source(w), start,
					  XmSELECT_LINE, XmsdRight, 1, False);

	    if (start == PASTENDPOS)
	    {
		break;		/* terminate loop */
	    }
	    else
	    {
		start++;
	    }

	    index++;
	}
    }

    /*
     * All done. Store some stuff in the widget.
     */
    Text_TotalLines(w) = index;
    for (i=index; i<Text_TableSize(w); i++) {
	Text_LineTable(w)[i].start_pos = 0;
	Text_LineTable(w)[i].virt_line = False;
    }
}

/*
 * This function is the *initialize* variant of the one above.
 */
static void
InitializeLineTable(XmTextWidget w)
{
    int i;

    /* Allocate initial amount */
    Text_LineTable(w) = (XmTextLineTable)XtMalloc(sizeof(XmTextLineTableRec)
						  * LINETABLE_INCREMENT);

    Text_TotalLines(w) = 0;
    Text_TableSize(w) = LINETABLE_INCREMENT;

    for (i = 0; i < LINETABLE_INCREMENT; i++)
    {
	Text_LineTable(w)[i].start_pos = 0;
	Text_LineTable(w)[i].virt_line = False;
    }

    _XmTextUpdateLineTable((Widget)w, 0, 0, NULL, False);
}

/* Not needed beyond this */
#undef LINETABLE_INCREMENT

extern unsigned int
_XmTextGetTableIndex(XmTextWidget w, XmTextPosition pos)
{
    XmTextLineTable current = Text_LineTable(w) + Text_TotalLines(w) - 1;
    unsigned int i = Text_TotalLines(w) - 1;

    if (pos < 0)
    {
	return 0;
    }

    if (Text_LineTable(w) == NULL)
    {
	return 0;
    }

    for (; current->start_pos > pos; current--, i--);

#ifdef VERBOSE
    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "XmTextGetTableIndex Pos %d Index %d\n", pos, i));
#endif

    return i;
}

/* Quasi-Public Functions ------------------------------------------------ */
extern void
_XmTextLineInfo(XmTextWidget w, LineNum line,
		XmTextPosition *startpos, LineTableExtra *extra)
{
    *startpos = Text_Line(w)[line].start;
    *extra = Text_Line(w)[line].extra;
}

extern void
_XmTextMarkRedraw(XmTextWidget w, XmTextPosition left,
		  XmTextPosition right)
{
}

extern LineNum
_XmTextPosToLine(XmTextWidget w, XmTextPosition position)
{
    if (position < 1)
	return 0;
    else if (position > Text_LastPos(w))
	return NOLINE;
    else
	return _XmTextGetTableIndex(w, position) + 1;
}

/*
 * As far as I can see, this function needs to :
 *	- insert text
 *	- update the cursor position (done inside TextStrSo->Replace)
 *	- call XmNvalueChangedCallback (done inside TextStrSo->Replace)
 *	- call XmNmodifyVerifyCallback (and Wcs)	( ?? )
 */
static void
_XmTextInsert(Widget w, XmTextPosition position, char *string, XEvent *evp)
{
    XmTextStatus st;
    XmTextWidget tw = (XmTextWidget)w;
    XmTextBlockRec block;
    XmTextPosition startret, endret;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTextInsert\n"));

    if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldInsert(w, position, string);
	return;
    }

    if (string == NULL)
    {
	return;			/* Can I do this ? FIX ME */
    }

    if (!Text_Source(w))
    {
	_XmWarning(w, "_XmTextInsert: no source\n");

	return;
    }

    block.ptr = string;
    block.length = strlen(string);
    block.format = XmFMT_8_BIT;
    if ( position > Text_LastPos(w) )
	position = Text_LastPos(w) ;                                                                                                                                                                                                                                      
    startret = position;
    endret = position;

    st = (*Text_Source(w)->Replace) (tw, evp, &startret, &endret,
				     &block, True);
    /* FIX ME ?? */

    RefigureLines(tw);

    if (XtIsRealized(w))
    {
	Redisplay(tw);
    }
}


extern void
_XmTextSetCursorPosition(Widget w, XmTextPosition position)
{
    XmTextWidget tw = (XmTextWidget)w;
    XmTextVerifyCallbackStruct cbs;
    cbs.doit = True;
    cbs.newInsert = position;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
		      "_XmTextSetCursorPosition Pos %d \n",
		      position));

    if (position > Text_LastPos(w))
    {
	position = Text_LastPos(w);
    }

    if (Text_CursorPos(w) != position)
    {
	if (Text_MotionVerifyCallback(w))
	{
	    cbs.reason = XmCR_MOVING_INSERT_CURSOR;
	    cbs.event = NULL;
	    cbs.currInsert = Text_CursorPos(w);
	    cbs.startPos = cbs.endPos = 0;
	    cbs.text = NULL;

	    XtCallCallbacks(w, XmNmotionVerifyCallback, &cbs);
	}
    }

    if (cbs.doit)
    {
#ifdef VERBOSE
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "Text_CursorPos before draw: %08x\n",
			  Text_CursorPos(w)));
#endif

	(*Text_Output(w)->DrawInsertionPoint) (tw, Text_CursorPos(w), off);

#if 0
	/* rws 10 Mar 2000
	   This sucks, but text/test16 sub-test 5 proves this
	 */
	Text_CursorPos(w) = cbs.newInsert;
#else
	Text_CursorPos(w) = position;
#endif

#ifdef VERBOSE
	DEBUGOUT(_LtDebug(__FILE__, (Widget)w,
			  "Text_CursorPos after draw: %08x\n",
			  Text_CursorPos(w)));
#endif

	if (Text_AutoShowCursorPosition(w))
	{
	    /* Don't do this before being realized, it'll misplace the text
	     *	(based on premature widget size).
	     *	Fixes xmcd. Danny 25/8/1997 */
	    if (XtIsRealized(w))
		XmTextShowPosition(w, Text_CursorPos(w));
	}
	else
	{
	    _XmTextMovingCursorPosition(tw, Text_CursorPos(w));
	}

	(*Text_Output(w)->DrawInsertionPoint) (tw, Text_CursorPos(w), on);
    }
    (void)_XmImSendSpot(w);
}


static void
_XmTextSetEditable(Widget w, Boolean e)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmTextSetEditable()\n"));

    if (!XtIsRealized(w))
    {
	return;
    }

    if (e)
    {				/* becomes editable */
	Arg args[10];
	int nargs;

	XmImRegister(w, 0);

	nargs = 0;
	XtSetArg(args[nargs], XmNbackground, XtBackground(w)); nargs++;
	XtSetArg(args[nargs], XmNforeground, Prim_Foreground(w)); nargs++;
	XmImSetValues(w, args, nargs);
    }
    else
    {				/* Becomes un-editable */
	XmImUnregister(w);
    }

    _XmStringSourceSetEditable(Text_Source(w), e);
}


extern void
_XmTextDisableRedisplay(XmTextWidget w, Boolean losesbackingstore)
{
    Text_DisableDepth(w)++;
}


extern void
_XmTextEnableRedisplay(XmTextWidget w)
{
    if (Text_DisableDepth(w) > 0)
    {
	Text_DisableDepth(w)--;
	if (Text_DisableDepth(w) == 0)
	{
	    if (Text_NeedsRedisplay(w))
	    {
		Redisplay(w);
	    }
	}
    }
}


/*
 * What on earth does this do ???
 */
extern void
_XmTextInvalidate(XmTextWidget w, XmTextPosition position,
		  XmTextPosition topos, long delta)
{
    Cardinal i;

    for (i = 0;
	 i < Text_LineCount(w) && Text_Line(w)[i].start <= position;
	 i++);

    if (i >= Text_LineCount(w))
    {
	return;
    }

    if (i==0)
    {
       /* This fixes SF [Bug #124809] */
       Text_Line(w)[0].changed = True;
       Text_Line(w)[0].changed_position = position;
    }
    else
    {
       Text_Line(w)[i - 1].changed = True;
       Text_Line(w)[i - 1].changed_position = position;
    }

    (*Text_Output(w)->Invalidate) (w, position, topos, delta);
    (*Text_Input(w)->Invalidate) (w, position, topos, delta);
}


/* Public Functions ------------------------------------------------------ */

extern Widget
XmCreateText(Widget parent, char *name, Arg *arglist, Cardinal argCount)
{
    return XtCreateWidget(name, xmTextWidgetClass, parent, arglist, argCount);
}


extern Widget
XmCreateScrolledText(Widget parent, char *name, Arg *arglist, Cardinal argcount)
{
    Widget sw, w;
    char *sname;
    Cardinal i;
    Arg *al;

    if (name == NULL)
    {
	name = "";
    }

    sname = XtMalloc(strlen(name) + 3);
    strcpy(sname, name);
    strcat(sname, "SW");

    al = (Arg *)XtCalloc(argcount + 4, sizeof(Arg));
    for (i = 0; i < argcount; i++)
    {
	al[i].name = arglist[i].name;
	al[i].value = arglist[i].value;
    }

    XtSetArg(al[i], XmNscrollingPolicy, XmAPPLICATION_DEFINED); i++;
    XtSetArg(al[i], XmNvisualPolicy, XmVARIABLE); i++;
    XtSetArg(al[i], XmNscrollBarDisplayPolicy, XmSTATIC); i++;
    XtSetArg(al[i], XmNshadowThickness, 0); i++;

    sw = XtCreateManagedWidget(sname, xmScrolledWindowWidgetClass, parent,
			       al, i);
    XtFree((char *)sname);

    i = argcount;
    XtSetArg(al[i], XmNeditMode, XmMULTI_LINE_EDIT); i++;

    w = XtCreateManagedWidget(name, xmTextWidgetClass, sw, al, i);
    XtAddCallback(w, XmNdestroyCallback,
		  _XmDestroyParentCallback,
		  (XtPointer)w);

    XtFree((char *)al);

    return w;
}


extern void
XmTextClearSelection(Widget w, Time time)
{
    Boolean sel;
    XmTextPosition left, right;

    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);
	if (sel) 
	{
          /* SG 22/08/1998 this is wrong, it should deselect the text
             for this widget only, not effecting the primary or selection
             properties (according to O'Reilly), it certainly shouldn't
             delete any text.
		_XmTextDelete((XmTextWidget)w, NULL, left, right);
             give this a go instead,based on the same thing in XmTextField,
             however there is a complication, this clears the selection for
             all XmText widgets sharing this source - is this right ??? */
	    (*Text_Source(w)->SetSelection) (Text_Source(w), -1, -1, time);
	}
        return;
    }   
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {   
	XmTextFieldClearSelection(w, time);
	return;
    }       

    _XmWarning(w, "XmTextClearSelection: widget has invalid class");

}


extern Boolean
XmTextCopy(Widget w, Time time)
{
    Display		*dpy;
    Window		win;
    int			r;
    long		item_id;
    XmString		lb = NULL;
    Boolean		sel;
    XmTextPosition	left, right;
    XmTextBlockRec	block;

    if (XtIsSubclass(w, xmTextFieldWidgetClass))
        return XmTextFieldCopy(w,time);
    else if (!XmIsText(w))
    {
	_XmWarning(w, "XmTextCopy: widget has invalid class");
	return False;
    }

    if (w == NULL || XtWindow(w) == None)
	return False;

    dpy = XtDisplay(w);
    win = XtWindow(w);

    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);
    if (!sel)
	return False;

    (*Text_Source(w)->ReadSource) (Text_Source(w), left, right, &block);

    lb = XmStringCreateLocalized("XmText");

    while ((r = XmClipboardStartCopy(dpy, win, lb, time,
		NULL, NULL, &item_id)) == XmClipboardLocked)
	;

    XmStringFree(lb);
    if (r != XmClipboardSuccess) {
	XtFree(block.ptr);
	return False;
    }

    while ((r = XmClipboardCopy(dpy, win, item_id, "STRING",
		block.ptr, right - left + 1,
		0, NULL)) == XmClipboardLocked)
	;

    XtFree(block.ptr);

    if (r != XmClipboardSuccess) {
	(void) XmClipboardEndCopy(dpy, win, item_id);
	return False;
    }

    while ((r = XmClipboardEndCopy(dpy, win, item_id)) == XmClipboardLocked)
	;

    if (r == XmClipboardSuccess)
	return True;
    else
	return False;
}


extern Boolean
XmTextCut(Widget w, Time time)
{
    Boolean		sel, r = False;
    XmTextPosition	left, right;

    if (XtIsSubclass(w, xmTextFieldWidgetClass))
        return XmTextFieldCut(w,time);
    else if (!XmIsText(w))
    {
	_XmWarning(w, "XmTextCut: widget has invalid class");
	return False;
    }

    if ((r = XmTextCopy(w, time)) == False)
        return False;

    sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

    if (!sel || !Text_Editable(w))
	return False;

    /* Now also remove the string from the buffer. */
    _XmTextDelete((XmTextWidget)w, NULL, left, right);

    return r;
}


extern Boolean
XmTextFindString(Widget w, XmTextPosition start, char *string,
		 XmTextDirection direction, XmTextPosition *position)
{
    char *buf, *str;
    int len, i;

    len = XmTextGetLastPosition(w);
    if ((start < 0) || (start > len))
    {
	return False;
    }

    buf = XmTextGetString(w);

    if (direction == XmTEXT_FORWARD) {
	str = strstr(&buf[start], string);
    } else if (direction == XmTEXT_BACKWARD) {
	i = start;
	str = NULL;
	len = strlen(string);
	while (i) {
		if (! strncmp(&buf[i], string, len)) {
			str = &buf[i];
			break;
		}
		i--;
	}
    } else {
	str = NULL;
    }

    if (str == (char *)NULL)
    {
	XtFree(buf);
	return False;
    }

    *str = 0;
    *position = (XmTextPosition)(str-buf);
    XtFree(buf);
    return True;
}


/*
 * Returns an integer value that indicates the y position of the first baseline
 * in the Text widget. The calculation takes into account the margin height,
 * shadow thickness, highlight thickness, and font ascent of the first font in the
 * fontlist. In this calculation the x position of the top of the widget is 0. 
 */
extern int
XmTextGetBaseline(Widget w)
{
	/*
	 * TextField does this :
	 *	Prim_ShadowThickness(w) + Prim_HighlightThickness(w) +
	 *		TextF_MarginHeight(w) + TextF_FontAscent(w)
	 */

	if (XtIsSubclass(w, xmTextWidgetClass)) {
		return Prim_ShadowThickness(w) + Prim_HighlightThickness(w) +
			Text_MarginHeight(w) + Out_Font_Ascent(Text_OutputData(w));
	} else if (XtIsSubclass(w, xmTextFieldWidgetClass)) {
		return XmTextFieldGetBaseline(w);
	}

	_XmWarning(w, "XmTextGetBaseline: widget has invalid class");
	return 0;
}


extern XmTextPosition
XmTextGetCursorPosition(Widget w)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	return Text_CursorPos(w);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldGetCursorPosition(w);
    }

    _XmWarning(w, "XmTextGetCursorPosition: widget has invalid class");

    return 0;
}


extern Boolean
XmTextGetEditable(Widget w)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	return Text_Editable(w);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldGetEditable(w);
    }

    _XmWarning(w, "XmTextGetEditable: widget has invalid class");

    return 0;
}


extern XmTextPosition
XmTextGetInsertionPosition(Widget w)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	return Text_CursorPos(w);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldGetInsertionPosition(w);
    }

    _XmWarning(w, "XmTextGetInsertionPosition: widget has invalid class");

    return 0;
}


extern XmTextPosition
XmTextGetLastPosition(Widget w)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	return Text_LastPos(w);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldGetLastPosition(w);
    }

    _XmWarning(w, "XmTextGetLastPosition: widget has invalid class");

    return 0;
}


extern int
XmTextGetMaxLength(Widget w)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	return _XmStringSourceGetMaxLength(Text_Source(w));
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldGetMaxLength(w);
    }

    _XmWarning(w, "XmTextGetMaxLength: widget has invalid class");

    return 0;
}


extern char *
XmTextGetSelection(Widget w)
{
    Boolean sel;
    XmTextPosition left, right;
    XmTextBlockRec block;

    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);
	if (!sel)
	{
	    return NULL;
	}

	(*Text_Source(w)->ReadSource) (Text_Source(w), left, right, &block);

	return block.ptr;	/* Caller must free */
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldGetSelection(w);
    }

    _XmWarning(w, "XmTextGetSelection: widget has invalid class");

    return NULL;
}


extern Boolean
XmTextGetSelectionPosition(Widget w, XmTextPosition *left, XmTextPosition *right)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	return (*Text_Source(w)->GetSelection) (Text_Source(w), left, right);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldGetSelectionPosition(w, left, right);
    }

    _XmWarning(w, "XmTextGetSelectionPosition: widget has invalid class");

    return False;
}


extern wchar_t *
XmTextGetSelectionWcs(Widget w)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmWarning(w, "XmTextGetSelectionWcs: not implemented");
	return NULL;		/* FIX ME */
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldGetSelectionWcs(w);
    }

    _XmWarning(w, "XmTextGetSelectionWcs: widget has invalid class");

    return NULL;
}


extern char *
XmTextGetString(Widget w)
{
    if (XmIsText(w))
    {
	return _XmStringSourceGetValue(Text_Source(w), False);
    }
    else if (XmIsTextField(w))
    {
	return XmTextFieldGetString(w);
    }

    _XmWarning(w, "XmTextGetString: widget has invalid class");

    return NULL;
}


extern wchar_t *
XmTextGetStringWcs(Widget w)
{
    if (XmIsText(w))
    {
	return (wchar_t *)_XmStringSourceGetValue(Text_Source(w), True);
    }
    else if (XmIsTextField(w))
    {
	return XmTextFieldGetStringWcs(w);
    }

    _XmWarning(w, "XmTextGetStringWcs: widget has invalid class");

    return NULL;
}


extern Boolean
XmTextFindStringWcs(Widget w,
		    XmTextPosition start,
		    wchar_t *wc_string,
		    XmTextDirection direction,
		    XmTextPosition *position)
{
    _XmWarning(w, "XmTextFindStringWcs: not implemented");
    return False;
}


/*
 * XmTextGetSubstring()
 * 
 * Gets a substring of the text 
 */
extern int
XmTextGetSubstring(Widget w, XmTextPosition start, int num_chars,
		   int buffer_size, char *buffer)
{
    int			len;
    int			retval = XmCOPY_SUCCEEDED;
    XmTextBlockRec	block;

    if (XmIsTextField(w))
    {
	return XmTextFieldGetSubstring(w, start, num_chars,
				       buffer_size, buffer);
    }
    if (!XmIsText(w))
    {
	_XmWarning(w, "XmTextGetSubstring: widget has invalid class");
	return XmCOPY_FAILED;
    }

    len = (*Text_Source(w)->ReadSource)
	    (Text_Source(w), start, start+num_chars, &block) - start;

    if(len < num_chars)
    {
	retval = XmCOPY_TRUNCATED;
    } /* no else: retval may be updated to XmCOPY_FAILED in next test */

    if(len > buffer_size)
    {
	retval = XmCOPY_FAILED;
    }
    else
    {
    	memcpy(buffer, block.ptr, len);
    }

    XtFree(block.ptr);

    return retval;
}


extern int
XmTextGetSubstringWcs(Widget w, XmTextPosition start, int num_chars,
		      int buffer_size, wchar_t *buffer)
{
    if (XmIsTextField(w))
    {
	return XmTextFieldGetSubstringWcs(w, start, num_chars,
					  buffer_size, buffer);
    }

    if (!XmIsText(w))
    {
	_XmWarning(w, "XmTextGetSubstringWcs: widget has invalid class");

	return XmCOPY_FAILED;
    }

    _XmWarning(w, "XmTextGetSubstringWcs: not implemented");
    return XmCOPY_FAILED;
}


extern XmTextPosition
XmTextGetTopCharacter(Widget w)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	return Text_TopPos(w);
    }
    else
    {
	_XmWarning(w, "XmTextGetTopCharacter: widget has invalid class");
	return 0;
     }
}


extern void
XmTextInsert(Widget w, XmTextPosition position, char *string)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmTextInsert(w, position, string, NULL);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldInsert(w, position, string);
    }
    else
    {
	_XmWarning(w, "XmTextInsert: widget has invalid class");
    }
}


extern void
XmTextInsertWcs(Widget w, XmTextPosition position, wchar_t *wcstring)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmWarning(w, "XmTextInsertWcs: not implemented");
	;			/* FIX ME */
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldInsertWcs(w, position, wcstring);
    }
    else
    {
	_XmWarning(w, "XmTextInsertWcs: widget has invalid class");
    }
}


extern Boolean
XmTextPaste(Widget w)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	Display		*dpy = XtDisplay(w);
	Window		win = XtWindow(w);
	int		r;
	char		*buf;
	unsigned long	len, nbytes;
	long		private_id;
	XmTextPosition	pos;

	if (win == None || !Text_Editable(w))
	    return False;

	while ((r = XmClipboardStartRetrieve(XtDisplay(w), XtWindow(w),
                XtLastTimestampProcessed(XtDisplay(w)))) == XmClipboardLocked)
	    ;
	if (r != XmClipboardSuccess)
	    return False;

	/* Figure out how much we need to paste */
	while ((r = XmClipboardInquireLength(dpy, win, "STRING", &len))
		== XmClipboardLocked)
	    ;
	if (r != XmClipboardSuccess)
	    return False;

	buf = XtMalloc(len + 1);

	while ((r = XmClipboardRetrieve(dpy, win, "STRING",
		buf, len, &nbytes, &private_id)) == XmClipboardLocked)
	    ;
	buf[len] = '\0';

	if (r != XmClipboardSuccess) {
	    XtFree(buf);
	    return False;
	}
	DEBUGOUT(_LtDebug(__FILE__, w,
	    "XmTextPaste: should be getting %ld bytes\n", nbytes));

	while ((r = XmClipboardEndRetrieve(dpy, win)) == XmClipboardLocked)
	    ;
	if (r != XmClipboardSuccess) {
	    XtFree(buf);
	    return False;
	}

	/* Now insert the text ... */
	pos = Text_CursorPos(w);

	_XmTextInsert(w, pos, buf, NULL);

	XtFree(buf);
	return True;
    } else if (XtIsSubclass(w, xmTextFieldWidgetClass)) {
	return XmTextFieldPaste(w);
    } else {
	_XmWarning(w, "XmTextPaste: widget has invalid class");
    }

    return False;
}


extern Boolean
XmTextPosToXY(Widget w, XmTextPosition position, Position *x, Position *y)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	return (*Text_Output(w)->PosToXY) ((XmTextWidget)w, position, x, y);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldPosToXY(w, position, x, y);
    }

    _XmWarning(w, "XmTextPosToXY: widget has invalid class");

    return False;

}


extern Boolean
XmTextRemove(Widget w)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	Boolean sel;
	XmTextPosition left, right;

	/* Copied part of the code of TextIn.c:DeleteForwardChar */
	sel = (*Text_Source(w)->GetSelection) (Text_Source(w), &left, &right);

	if (!sel || !Text_Editable(w))
	{
	    return False;
        }

	_XmTextDelete((XmTextWidget)w, NULL, left, right);

	return True;
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldRemove(w);
    }
    else
    {
	_XmWarning(w, "XmTextRemove: widget has invalid class");
	return False;
    }
}


extern void
XmTextReplace(Widget w, XmTextPosition from_pos, XmTextPosition to_pos, char *value)
{
    XmTextStatus st;
    XmTextWidget tw = (XmTextWidget)w;
    XmTextBlockRec block;
    XmTextPosition startret, endret;

    if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldReplace(w, from_pos, to_pos, value);

	return;
    }
    else if (!XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmWarning(w, "XmTextReplace: widget has invalid class");

	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "XmTextReplace(from %d to %d '%s'\n",
		      from_pos, to_pos, value));

    block.ptr = value;
    block.length = value ? strlen(value) : 0;
    block.format = XmFMT_8_BIT;

    startret = from_pos;
    endret = to_pos;

    st = (*Text_Source(w)->Replace) (tw, NULL, &startret, &endret,
				     &block, True);
    /* FIX ME ?? */

    RefigureLines(tw);

#if 1
    XmTextShowPosition(w, from_pos + block.length);
#else
    if (XtIsRealized(w))
    {
	Redisplay(tw);
    }
#endif
}


extern void
XmTextReplaceWcs(Widget w, XmTextPosition from_pos, XmTextPosition to_pos,
		 wchar_t *wcstring)
{
    if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldReplaceWcs(w, from_pos, to_pos, wcstring);

	return;
    }
    else if (!XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmWarning(w, "XmTextReplaceWcs: widget has invalid class");

	return;
    }

    _XmWarning(w, "XmTextReplaceWcs is not implemented");
}


extern void
XmTextSetAddMode(Widget w, Boolean state)
{
	XmTextWidget tw = (XmTextWidget)w;

	if (XtIsSubclass(w, xmTextWidgetClass)) {
		Text_AddMode(w) = state;
	} else if (XtIsSubclass(w, xmTextFieldWidgetClass)) {
		XmTextFieldSetAddMode(w, state);
	} else {
		_XmWarning(w, "XmTextSetAddMode: widget has invalid class");
	}
}


extern void
XmTextSetCursorPosition(Widget w, XmTextPosition position)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmTextSetCursorPosition(w, position);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldSetCursorPosition(w, position);
    }
    else
    {
	_XmWarning(w, "XmTextSetCursorPosition: widget has invalid class");
    }
}


extern void
XmTextSetEditable(Widget w, Boolean editable)
{
    if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldSetEditable(w, editable);
    }
    else if (!XmIsText(w))
    {
	_XmWarning(w, "XmTextSetEditable: widget has invalid class");

	return;
    }

    if (Text_Editable(w) != editable)
    {
	_XmTextSetEditable(w, editable);
    }

    Text_Editable(w) = editable;
}


extern void
XmTextSetHighlight(Widget w, XmTextPosition left, XmTextPosition right, XmHighlightMode mode)
{
    if (left >= right || left < 0 )
    {
	/* FIX ME */
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XmTextSetHighlight %d >= %d, not sure what to do.\n",
			  left, right));

	return;
    }

    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	Cardinal i, j, k, l;
	XmHighlightMode lm, rm;

#if 0
	fprintf(stderr,
			  "XmTextSetHighlight(%d,%d,%s)\n",
			  left, right, _LtDebugHighlightMode2String(mode));
#endif
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XmTextSetHighlight(%d,%d,%s)\n",
			  left, right, _LtDebugHighlightMode2String(mode)));

	/* Provide for initial entries if there's none */
	if (Text_Highlight(w).number == 0) 
	{
	    Text_Highlight(w).list[0].position = 0;
	    Text_Highlight(w).list[0].mode = XmHIGHLIGHT_NORMAL;
	    Text_Highlight(w).list[1].position = INT_MAX;
	    Text_Highlight(w).list[1].mode = XmHIGHLIGHT_NORMAL;
	    Text_Highlight(w).number = 2;
	}

	/* Find the place from where to change the highlight */
	for (i=0; i < Text_Highlight(w).number &&
			Text_Highlight(w).list[i].position < left; i++)
		;
	if (i == 0)
	{
	    /* assert(left==0); */
	    lm = XmHIGHLIGHT_NORMAL;
	}
	else
	{
	    lm = Text_Highlight(w).list[i-1].mode;
	}
	for (j=i; j < Text_Highlight(w).number &&
			Text_Highlight(w).list[j].position < right; j++)
		;
	if (j == 0)
	{
	    /* assert(right==0); */
	    rm = XmHIGHLIGHT_NORMAL;
	}
	else
	{
	    rm = Text_Highlight(w).list[j-1].mode;
	}
	if (Text_Highlight(w).list[i].position == left) 
	{
	    Text_Highlight(w).list[i].mode = mode;
	} 
	else if (Text_Highlight(w).list[i].position < right) 
	{
	    Text_Highlight(w).list[i].mode = mode;
	    Text_Highlight(w).list[i].position = left;
	} 
	else 
	{
	    /* Insert an entry for the left */
	    if (lm != mode) 
	    {
		/* Need one more entry */
		/* Make sure there's an empty space */
		if (Text_Highlight(w).number == Text_Highlight(w).maximum) 
		{
		    Text_Highlight(w).maximum += 32;
		    Text_Highlight(w).list = (_XmHighlightRec *)XtRealloc(
				(char *)Text_Highlight(w).list,
				Text_Highlight(w).maximum * sizeof(_XmHighlightRec));
		}

		/* Go ahead. Do we need to insert ? */
		for (j=Text_Highlight(w).number; j>i; j--) 
		{
		    Text_Highlight(w).list[j] = Text_Highlight(w).list[j-1];
		}
		Text_Highlight(w).number++;
		Text_Highlight(w).list[i].mode = mode;
		Text_Highlight(w).list[i].position = left;
	    }
	}

	/* Remove entries */
	/* FIX ME this can be more efficient */
	for (i=0; i<Text_Highlight(w).number && Text_Highlight(w).list[i].position < left; i++) ;
		/* i points to the entry just beyond "left" */
	for (j=i; j<Text_Highlight(w).number && Text_Highlight(w).list[j].position < right; j++) ;
	if (i < j-1) 
	{
	    for (k=i+1, l=j; l<Text_Highlight(w).number; k++,l++) 
	    {
		Text_Highlight(w).list[k] = Text_Highlight(w).list[l];
	    }
	    Text_Highlight(w).number -= j-i-1;
	}
	/* Now this is hilarious - insert one again */
	i++;
	if (rm != mode) 
	{
	    if (Text_Highlight(w).number == Text_Highlight(w).maximum) 
	    {
		Text_Highlight(w).maximum += 32;
		Text_Highlight(w).list = (_XmHighlightRec *)XtRealloc(
			    (char *)Text_Highlight(w).list,
			    Text_Highlight(w).maximum * sizeof(_XmHighlightRec));
	    }
	    for (k=Text_Highlight(w).number; k>i; k--) 
	    {
		Text_Highlight(w).list[k] = Text_Highlight(w).list[k-1];
	    }
	    Text_Highlight(w).number++;
	    Text_Highlight(w).list[i].mode = rm;
	    Text_Highlight(w).list[i].position = right;
	}
	/* End Highlighting */
#if 0
	for (i=0; i<Text_Highlight(w).number; i++)
		fprintf(stderr,
			  "\tXmTextSetHighlight -> [%d] %d %s\n",
			  i, Text_Highlight(w).list[i].position,
			  _LtDebugHighlightMode2String(Text_Highlight(w).list[i].mode));
#endif

	if (XtIsRealized(w))
	{
	    DoExpose(w, NULL, NULL);
	}
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldSetHighlight(w, left, right, mode);
    }
    else
    {
	_XmWarning(w, "XmTextSetHighlight: widget has invalid class");
    }
}


extern void
XmTextSetInsertionPosition(Widget w, XmTextPosition position)
{
    if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldSetCursorPosition(w, position);
    }
    else if (XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmTextSetCursorPosition(w, position);
	Text_DestPosition(w)= Text_CursorPos(w);
    }
    else
    {
	_XmWarning(w, "XmTextSetInsertionPosition: widget has invalid class");
    }
}


extern void
XmTextSetMaxLength(Widget w, int max_length)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmStringSourceSetMaxLength(Text_Source(w), max_length);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldSetMaxLength(w, max_length);
    }
    else
    {
	_XmWarning(w, "XmTextSetMaxLength: widget has invalid class");
    }
}


extern void
XmTextSetSelection(Widget w, XmTextPosition first, XmTextPosition last,
		   Time time)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmTextSetSelection(%d,%d)\n", first, last));

    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	(*Text_Source(w)->SetSelection) (Text_Source(w), first, last, time);
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldSetSelection(w, first, last, time);
    }
    else
    {
	_XmWarning(w, "XmTextSetSelection: widget has invalid class");
    }
}


extern void
XmTextSetString(Widget w, char *value)
{
    XmTextWidget tw = (XmTextWidget)w;

    if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldSetString(w, value);
    }
    else if (XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmStringSourceSetValue(tw, value);

	Text_TopPos(w) = 0;
#if 0
	/* rws 10 Mar 2000
	   This cannot happen here. If the modifyVerify callback came back
	   with doit False, nothing changed!!!
	 */
	Text_LastPos(w) = (value ? strlen(value) : 0);
#endif

#if 0
        // SG 01/11/1998 Reset the insertion point to the beginning
        // however this will only effect this widget using the source,
        // should the source take care of doing this for all its attached
        // widgets.
#endif
	Text_CursorPos(w) = 0;

	DEBUGOUT(_LtDebug(__FILE__, w, "XmTextSetString(%s), lastpos %d\n",
		value, Text_LastPos(w)));

	_XmTextUpdateLineTable(w, 0, 0, NULL, False);

	RefigureLines(tw);

	(*Text_Output(tw)->Invalidate) (tw, 0, 0, 0);

	if (XtIsRealized(w))
	{
	    Redisplay(tw);
	}
    }
    else
    {
	_XmWarning(w, "XmTextSetString: widget has invalid class");
    }
}


extern void
XmTextSetStringWcs(Widget w, wchar_t *wcstring)
{
    if (XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmWarning(w, "XmTextSetStringWcs: not implemented");
	;			/* FIX ME */
    }
    else if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	XmTextFieldSetStringWcs(w, wcstring);
    }
    else
    {
	_XmWarning(w, "XmTextSetStringWcs: widget has invalid class");
    }
}


extern void
XmTextDisableRedisplay(Widget w)
{
    Text_DisableDepth(w)++;
}


extern void
XmTextEnableRedisplay(Widget w)
{
    _XmTextEnableRedisplay((XmTextWidget)w);
}


extern XmTextSource
XmTextGetSource(Widget w)
{
    return Text_Source(w);
}


extern void
XmTextSetSource(Widget w, XmTextSource s,
		XmTextPosition top, XmTextPosition curs)
{
    XmTextWidget tw = (XmTextWidget)w;

    DEBUGOUT(_LtDebug(__FILE__, w, "XmTextSetSource\n"));

    if (s == NULL)
    {
	_XmWarning(w, "Invalid source, source ignored.");
	return;
    }

    (*Text_Source(w)->RemoveWidget) (Text_Source(w), tw);

    Text_Source(w) = s;

    (*Text_Source(w)->AddWidget) (Text_Source(w), tw);

    _XmTextUpdateLineTable(w, 0, 0, NULL, False);

    RefigureLines(tw);

    if (XtIsRealized(w))
    {
	Redisplay(tw);
    }
}


extern void
XmTextScroll(Widget aw, int n)
{
    XmTextWidget w = (XmTextWidget)aw;
    int top_index, index;

    if (!XtIsSubclass((Widget)w, xmTextWidgetClass))
    {
	_XmWarning((Widget)w, "XmTextScroll: widget has invalid class");

	return;
    }

    top_index = _XmTextGetTableIndex(w, Text_TopPos(w));
    if (n < 0)
    {
	index = _XmMax(n + top_index, 0);
    }
    else
    {
	/* bug #672292 */
	index = _XmMin(n + top_index, Text_TotalLines(w)-1);
#if 0
	index = _XmMin(n + top_index,
/* SG (15/08/1998)  Text_TotalLines(w) - 1) - Text_LineCount(w) + 1;
   we would rather like to be able to scroll down to the last line */
		       Text_TotalLines(w) ) - Text_LineCount(w) + 1;
#endif
    }
    DEBUGOUT(_LtDebug(__FILE__, aw, "XmTextScroll index=%d n=%d\n", index, n));

    Text_TableIndex(w) = index;
    XmTextSetTopCharacter(aw, Text_LineTable(w)[index].start_pos);
}


extern void
XmTextSetTopCharacter(Widget w, XmTextPosition top_character)
{
    if (!XtIsSubclass(w, xmTextWidgetClass))
    {
	_XmWarning(w, "XmTextSetTopCharacter: widget has invalid class");

	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "XmTextSetTopCharacter top_character=%d\n",
		      top_character));

    if (Text_EditMode(w) == XmSINGLE_LINE_EDIT)
    {
	/* Scroll horizontally until top_character is leftmost character */
    }
    else
    {
	/*
	 * If w is in MULTI_LINE_EDIT then set Text_TopPos(w)
	 * to the position of the first character of the line that
	 * top_character points at.
	 */

	XmTextLineTable	lte;

	lte = Text_LineTable(w) + _XmTextGetTableIndex((XmTextWidget)w,top_character);
	Text_TopPos(w) = lte->start_pos;

	RefigureLines((XmTextWidget)w);

	if (XtIsRealized(w))
	{
	    Redisplay((XmTextWidget)w);
	}
    }
}


extern void
XmTextShowPosition(Widget aw, XmTextPosition position)
{
    XmTextWidget w = (XmTextWidget)aw;
    unsigned int top_index, index, bottom_index;

    DEBUGOUT(_LtDebug(__FILE__, aw, "XmTextShowPosition pos=%d\n", position));

    if (XtIsSubclass(aw, xmTextFieldWidgetClass)) {
	XmTextFieldShowPosition(aw, position);
    } else if (XtIsSubclass(aw, xmTextWidgetClass)) {
	bottom_index = _XmTextGetTableIndex(w, Text_BottomPos(w));
	top_index = _XmTextGetTableIndex(w, Text_TopPos(w));
	index = _XmTextGetTableIndex(w, position);

	/* We need only be concerned with the Text_TopPos here. */

	if (index < top_index) {
	    ;
	} else if (bottom_index < index) {
	    /* This used to be here, but it's wrong :
		index += top_index - bottom_index;
	     */
	    index = bottom_index;
	} else {
	    index = top_index;
	}
	Text_TopPos(w) = Text_LineTable(w)[index].start_pos;

	/*
	 * ... because the Output object will do the hard work for us anyway.
	 *
	 * MakePositionVisible will (I hope) also do horizontal positioning.
	 */

	(*Text_Output(w)->MakePositionVisible) (w, position);

	if (Text_NeedsRedisplay(w)) {
	    Redisplay(w);
	}
    }
}


extern XmTextPosition
XmTextXYToPos(Widget w, Position x, Position y)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "XmTextXYToPos x=%d y=%d\n", x, y));

    if (XtIsSubclass(w, xmTextFieldWidgetClass))
    {
	return XmTextFieldXYToPos(w, x, y);
    }
    else if (XtIsSubclass(w, xmTextWidgetClass))
    {
	return (*Text_Output(w)->XYToPos) ((XmTextWidget)w, x, y);
    }
    else
    {
	return 0;
    }
}


extern void
_XmTextExportValue(Widget w, int offset, XtArgVal *value)
{
    *value = (XtArgVal)XmTextGetString(w);

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "_XmTextExportValue: value '%s'\n",
		      (char *)*value));
}


/*
 * GetValues works through a whole bunch of stuff here :
 * - the "normal" mechanism : Xt will copy stuff specified in resources
 * - synthetic resources : LessTif does this through GetValuesHook on
 *      the Primitive which is our superclass. _XmTextExportValue above
 *      is an example of this.
 * - explicit call of GetValues functions in our subparts, see below.
 *      The implementations of these are where they belong (in TextIn.c
 *      and TextOut.c), and both use XtGetSubvalues.
 *
 * For the infinitely curious, the last subpart (if you want to call it that),
 *      the XmTextSource, has no resources that you can access this way. It's
 *      not an Xt Object.
 */
static void
TextGetValues(Widget w, ArgList args, Cardinal *num_args)
{
    /* Call the GetValues method in our subparts. */
    if (Text_Output(w))
    {
	(*Text_Output(w)->GetValues) (w, args, *num_args);
    }

    if (Text_Input(w))
    {
	(*Text_Input(w)->GetValues) (w, args, *num_args);
    }

    /* Be verbose about what they're trying to find out */
    DEBUGOUT(_LtDebug(__FILE__, w, "TextGetValues :\n"));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, w, args, *num_args, True));
}


extern int
_XmTextGetTotalLines(Widget w)
{
	return Text_TotalLines(w);
}


extern void
_XmTextSetTopCharacter (  Widget w,
                      XmTextPosition top_character)
{
    if (Text_EditMode(w) == XmSINGLE_LINE_EDIT)
    {
	/* Scroll horizontally until top_character is leftmost character */
    }
    else
    {
	int index = _XmTextGetTableIndex( (XmTextWidget)w, top_character);
	if ( Text_TopPos(w) != top_character )
	    {
	    Text_NeedsRedisplay(w) = True;
	    Text_NeedsRefigureLines(w) = True;
	    }
	Text_NewTop(w) = Text_LineTable(w)[index].start_pos;
	Text_TableIndex(w)=index;
	if ( Text_NeedsRefigureLines(w) ) 
	    RefigureLines((XmTextWidget)w);

	if (XtIsRealized(w))
	{
	    Redisplay((XmTextWidget)w);
	}
    }
}


extern LineNum
_XmTextNumLines(XmTextWidget widget)
{
	return Text_LineCount(widget);
}

/*
 * Not sure whether I need to do all this copying.
 * FIX ME
 */ 
static XtPointer
_XmText_TraitGetValue(Widget w, int format)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmText_TraitGetValue(%s)\n",
		(format == XmFORMAT_XmSTRING) ? "XmFORMAT_XmSTRING" :
		(format == XmFORMAT_MBYTE) ? "XmFORMAT_MBYTE" :
		(format == XmFORMAT_WCS) ? "XmFORMAT_WCS" : "??"));

	switch (format) {
	case XmFORMAT_XmSTRING:
		return (XtPointer)XmStringCreateSimple(Text_Value(w));
	case XmFORMAT_MBYTE:
		return XtNewString(XmTextGetString(w)); /* FIX ME */
	case XmFORMAT_WCS:
		return XtNewString(XmTextGetString(w)); /* FIX ME */
	default:
		return NULL;
	}  
}

static void
_XmText_TraitSetValue(Widget w, XtPointer value, int format)
{
	char    *s;

	DEBUGOUT(_LtDebug(__FILE__, w, "_XmText_TraitSetValue(%s)\n",
		(format == XmFORMAT_XmSTRING) ? "XmFORMAT_XmSTRING" :
		(format == XmFORMAT_MBYTE) ? "XmFORMAT_MBYTE" :
		(format == XmFORMAT_WCS) ? "XmFORMAT_WCS" : "??"));

	switch (format) {
	case XmFORMAT_XmSTRING:
	    if (XmStringGetLtoR((XmString)value, XmFONTLIST_DEFAULT_TAG, &s)) {
		XmTextSetString(w, s);
	    }
	    return;
	case XmFORMAT_MBYTE:
	    /* FIX ME */
	    XmTextSetString(w, (char *)value);
	    return;
	case XmFORMAT_WCS:
	    /* FIX ME */
	    XmTextSetString(w, (char *)value);
	    return;
	default:
	    return;
	}
}

static int
_XmText_TraitPreferredFormat(Widget w)
{
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmText_TraitPreferredFormat\n"));

	return XmFORMAT_WCS;
}

extern Boolean
XmTextCopyLink(Widget widget,
               Time time)
{
   _XmWarning(NULL, "XmTextCopyLink(): not yet implemented!");
   return False;
}
