/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/SelectionBox.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/SelectionBox.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>

#include <XmI/XmI.h>
#include <XmI/MacrosI.h>
#include <Xm/XmP.h>
#include <Xm/SelectioBP.h>
#include <Xm/CommandP.h>
#include <Xm/List.h>
#include <Xm/PushBP.h>
#include <Xm/PushBGP.h>
#include <Xm/RowColumnP.h>
#include <Xm/TransltnsP.h>
#ifdef USE_WIDGETS
#include <Xm/Separator.h>
#else
#include <Xm/SeparatoGP.h>
#endif
#include <Xm/DialogS.h>
#include <Xm/TextF.h>

#include <XmI/DebugUtil.h>


/* Forward Declarations */

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void insert_child(Widget w);
static void DeleteChild(Widget w);
static void _XmDialogTypeDefault(Widget w, int offset, XrmValue *value);
XmGeoMatrix _XmSelectionBoxGeoMatrixCreate(Widget _w, Widget _from,
					   XtWidgetGeometry *_pref);
Boolean _XmSelectionBoxNoGeoRequest(XmGeoMatrix _geoSpec);
void _XmSelectionBoxGetListItems(Widget wid, int resource_offset,
				 XtArgVal *value);
void _XmSelectionBoxGetListItemCount(Widget wid, int resource_offset,
				     XtArgVal *value);
void _XmSelectionBoxGetTextColumns(Widget wid, int resource_offset,
				   XtArgVal *value);
void _XmSelectionBoxGetTextString(Widget wid, int resource_offset,
				  XtArgVal *value);


static XtAccelerators text_accelerators_parsed = NULL;

/*
 * Resources for the Selection Box class
 */
#define Offset(field) XtOffsetOf(XmSelectionBoxRec, selection_box.field)
static XtResource resources[] =
{
    {
	XmNtextAccelerators, XmCAccelerators, XmRAcceleratorTable,
	sizeof(XtAccelerators), Offset(text_accelerators),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNselectionLabelString, XmCSelectionLabelString, XmRXmString,
	sizeof(XmString), Offset(selection_label_string),
	XmRString, (XtPointer)NULL
    },
    {
	XmNlistLabelString, XmCListLabelString, XmRXmString,
	sizeof(XmString), Offset(list_label_string),
	XmRString, (XtPointer)NULL
    },
    {
	XmNtextColumns, XmCColumns, XmRShort,
	sizeof(short), Offset(text_columns),
	XmRImmediate, (XtPointer)20
    },
    {
	XmNtextString, XmCTextString, XmRXmString,
	sizeof(XmString), Offset(text_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNlistItems, XmCItems, XmRXmStringTable,
	sizeof(XmStringTable), Offset(list_items),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNlistItemCount, XmCItemCount, XmRInt,
	sizeof(int), Offset(list_item_count),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNlistVisibleItemCount, XmCVisibleItemCount, XmRInt,
	sizeof(int), Offset(list_visible_item_count),
	XmRImmediate, (XtPointer)8
    },
    {
	XmNokLabelString, XmCOkLabelString, XmRXmString,
	sizeof(XmString), Offset(ok_label_string),
	XmRString, (XtPointer)NULL
    },
    {
	XmNapplyLabelString, XmCApplyLabelString, XmRXmString,
	sizeof(XmString), Offset(apply_label_string),
	XmRString, (XtPointer)NULL
    },
    {
	XmNcancelLabelString, XmCCancelLabelString, XmRXmString,
	sizeof(XmString), Offset(cancel_label_string),
	XmRString, (XtPointer)NULL
    },
    {
	XmNhelpLabelString, XmCHelpLabelString, XmRXmString,
	sizeof(XmString), Offset(help_label_string),
	XmRString, (XtPointer)NULL
    },
    {
	XmNnoMatchCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(no_match_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNmustMatch, XmCMustMatch, XmRBoolean,
	sizeof(Boolean), Offset(must_match),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNminimizeButtons, XmCMinimizeButtons, XmRBoolean,
	sizeof(Boolean), Offset(minimize_buttons),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNokCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(ok_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNapplyCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(apply_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNcancelCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(cancel_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdialogType, XmCDialogType, XmRSelectionType,
	sizeof(unsigned char), Offset(dialog_type),
	XmRCallProc, (XtPointer)_XmDialogTypeDefault
    },
    {
	XmNchildPlacement, XmCChildPlacement, XmRChildPlacement,
	sizeof(unsigned char), Offset(child_placement),
	XmRImmediate, (XtPointer)XmPLACE_ABOVE_SELECTION
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNselectionLabelString,
	sizeof(XmString), Offset(selection_label_string),
	_XmExportXmString, NULL	/* FIX ME */
    },
    {
	XmNlistLabelString,
	sizeof(XmString), Offset(list_label_string),
	_XmExportXmString, NULL	/* FIX ME */
    },
    {
	XmNtextColumns,
	sizeof(short), Offset(text_columns),
	_XmSelectionBoxGetTextColumns, NULL
    },
    {
	XmNtextString,
	sizeof(XmString), Offset(text_string),
	_XmSelectionBoxGetTextString, NULL	/* FIX ME */
    },
    {
	XmNlistItems,
	sizeof(XmStringTable), Offset(list_items),
	_XmSelectionBoxGetListItems, NULL	/* FIX ME */
    },
    {
	XmNlistItemCount,
	sizeof(int), Offset(list_item_count),
	_XmSelectionBoxGetListItemCount, NULL	/* FIX ME */
    },
    {
	XmNlistVisibleItemCount,
	sizeof(int), Offset(list_visible_item_count),
	NULL /* FIX ME */ , NULL	/* FIX ME */
    },
    {
	XmNokLabelString,
	sizeof(XmString), Offset(ok_label_string),
	_XmExportXmString, NULL
    },
    {
	XmNapplyLabelString,
	sizeof(XmString), Offset(apply_label_string),
	_XmExportXmString, NULL
    },
    {
	XmNcancelLabelString,
	sizeof(XmString), Offset(cancel_label_string),
	_XmExportXmString, NULL
    },
    {
	XmNhelpLabelString,
	sizeof(XmString), Offset(help_label_string),
	_XmExportXmString, NULL
    }
};


static XtActionsRec actions[] =
{
	{"UpOrDown", _XmSelectionBoxUpOrDown},
	{"SelectionBoxUpOrDown", _XmSelectionBoxUpOrDown},
	{"SelectionBoxRestore", _XmSelectionBoxRestore},
};

#if 0
static XmBaseClassExtRec _XmSelectionBCoreClassExtRec = {
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

static XmManagerClassExtRec _XmSelectionBMClassExtRec = {
    /* next_extension            */ NULL,
    /* record_type               */ NULLQUARK,
    /* version                   */ XmManagerClassExtVersion,
    /* record_size               */ sizeof(XmManagerClassExtRec),
    /* traversal_children        */ NULL
};
#endif /* 0 */

XmSelectionBoxClassRec xmSelectionBoxClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmBulletinBoardClassRec,
        /* class_name            */ "XmSelectionBox",
	/* widget_size           */ sizeof(XmSelectionBoxRec),
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
	/* resize                */ XtInheritResize,
	/* expose                */ XtInheritExpose,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ XtInheritSetValuesAlmost,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ XtInheritTranslations,
	/* query_geometry        */ XtInheritQueryGeometry,
	/* display_accelerator   */ NULL,
	/* extension             */ (XtPointer)NULL /*&_XmSelectionBCoreClassExtRec*/
    },
    /* Composite class part */
    {
	/* geometry manager */ XtInheritGeometryManager, 
        /* change_managed   */ XtInheritChangeManaged, 
        /* insert_child     */ insert_child,
        /* delete_child     */ DeleteChild,
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
        /* translations                 */ XmInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ XmInheritParentProcess,
        /* extension                    */ (XtPointer)NULL /*&_XmSelectionBMClassExtRec*/
    },
    /* XmBulletinBoard class part */
    {
	/* always_install_accelerators  */ True /*False*/,
	/* geo_matrix_create            */ _XmSelectionBoxGeoMatrixCreate,
	/* focus_moved_proc             */ XmInheritFocusMovedProc,
	/* extension                    */ NULL,

    },
    /* XmSelectionBox part */
    {
	/* list_callback                */ NULL,
	/* extension                    */ NULL,
    }
};



WidgetClass xmSelectionBoxWidgetClass = (WidgetClass)&xmSelectionBoxClassRec;

static void
class_initialize(void)
{
    if (text_accelerators_parsed == NULL)
    {
	text_accelerators_parsed = 
	    XtParseAcceleratorTable(_XmSelectioB_defaultTextAccelerators);
    }
#if 0
    _XmSelectionBCoreClassExtRec.record_type = XmQmotif;
#endif
}

static void
class_part_initialize(WidgetClass widget_class)
{
    XmSelectionBoxWidgetClass sbwc = (XmSelectionBoxWidgetClass)widget_class;
    XmSelectionBoxWidgetClass swc = (XmSelectionBoxWidgetClass)widget_class->core_class.superclass;

    if (sbwc->selection_box_class.list_callback == XmInheritCallbackProc)
    {
    	sbwc->selection_box_class.list_callback = swc->selection_box_class.list_callback;
    }
    _XmFastSubclassInit(widget_class, XmSELECTION_BOX_BIT);
}


/*
 * Determine if the selection is a match with one of the
 * items in the list
 */
Boolean
_XmSelectionBoxMatch(XmSelectionBoxWidget w)
{
    char *s;
    XmString xms;
    Boolean r;

    if (SB_Text(w) == NULL || SB_List(w) == NULL)
    {
	return False;
    }
    s = XmTextFieldGetString(SB_Text(w));
    if (s == NULL || s[0] == '\0')
    {
	return False;
    }
    xms = XmStringCreateLtoR(s, XmSTRING_DEFAULT_CHARSET);
    r = XmListItemExists(SB_List(w), xms);

    XtFree(s);
    XmStringFree(xms);

    return r;
}


/*
 * When the user clicks on an item in the list, copy its value to the
 * text field.
 * Unfortunately, XmTextField is not XmString-based, so we have to
 * convert an XmString into a String.
 *
 * This is not always possible !
 *
 */
static void
ListSingleSelect(Widget w, XtPointer client, XtPointer call)
{
    XmSelectionBoxWidget sb = (XmSelectionBoxWidget)client;
    char *s;
    XmListCallbackStruct *lp = (XmListCallbackStruct *) call;

    if (SB_Text(sb) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "ListSingleSelect (No TextField)\n"));
	return;
    }

    if (lp == NULL || lp->item == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "ListSingleSelect (NULL)\n"));
	return;
    }

    if (!XmStringGetLtoR(lp->item, XmFONTLIST_DEFAULT_TAG, &s))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "ListSingleSelect (Couldn't convert to string)\n"));
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, w,
		      "ListSingleSelect '%s'\n", s));

    XmTextFieldSetString(SB_Text(sb), s);
    XmTextFieldSetInsertionPosition(SB_Text(sb),
				    XmTextFieldGetLastPosition(SB_Text(sb)));
    XtFree(s);
}


/*
 * This routine is called for any button child of selectionbox for the
 * XmNactivateCallback. Make mapping to XmNokCallback etc.
 * Also called for double-click in list.
 */
static void
_XmSbButton(Widget w, XtPointer client, XtPointer call)
{
    XmSelectionBoxWidget sb;
    XmAnyCallbackStruct *a = (XmAnyCallbackStruct *)call;
    XmSelectionBoxCallbackStruct cbs;
    int au = 0;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmSbButton\n"));

    if (XmIsList(w))
    {
	sb = (XmSelectionBoxWidget)XtParent(XtParent(w));
    }
    else
    {
	sb = (XmSelectionBoxWidget)XtParent(w);
    }

    cbs.event = a->event;
    cbs.value = (XmString)0;
    cbs.length = 0;

    if (w == SB_OkButton(sb) || w == SB_List(sb) || XmIsTextField(w))
    {
	if (w == SB_OkButton(sb) || XmIsTextField(w))
	{
	    char *s = NULL;

	    if (SB_Text(sb))
		s = XmTextFieldGetString(SB_Text(sb));

	    cbs.value = XmStringCreateSimple(s);
	    XtFree(s);

	    au++;
	}
	else
	{
	    XmListCallbackStruct *ls = (XmListCallbackStruct *) call;
	    cbs.value = ls->item;
	}
	cbs.length = XmStringLength(cbs.value);

	if (SB_MustMatch(sb))
	{
	    if (_XmSelectionBoxMatch(sb))
	    {
		cbs.reason = XmCR_OK;
		XtCallCallbackList((Widget)sb, SB_OkCallback(sb), &cbs);

		au++;
	    }
	    else
	    {
		cbs.reason = XmCR_NO_MATCH;
		XtCallCallbackList((Widget)sb, SB_NoMatchCallback(sb), &cbs);
	    }
	}
	else
	{
	    cbs.reason = XmCR_OK;
	    XtCallCallbackList((Widget)sb, SB_OkCallback(sb), &cbs);

	    au++;
	}
    }
    else if (w == BB_CancelButton(sb))
    {
	cbs.reason = XmCR_CANCEL;
	XtCallCallbackList((Widget)sb, SB_CancelCallback(sb), &cbs);
    }
    else if (w == SB_ApplyButton(sb))
    {
	/* rws 25 Apr 1997 value and length added 
	   should a check for match be done??
	 */
	char *s = NULL;
	if (SB_Text(sb))
		s = XmTextFieldGetString(SB_Text(sb));

	cbs.value = XmStringCreateSimple(s);

	XtFree(s);

	cbs.length = XmStringLength(cbs.value);
	cbs.reason = XmCR_APPLY;
	/* FIX ME - Must also get value+length in cbs ??? */

	XtCallCallbackList((Widget)sb, SB_ApplyCallback(sb), &cbs);
    }
    else if (w == SB_HelpButton(sb))
    {
	cbs.reason = XmCR_HELP;

	XtCallCallbackList((Widget)sb, sb->manager.help_callback, &cbs);
    }
#if 0
    if (BB_AutoUnmanage(sb) && au)
    {
	Widget s = XtParent((Widget)sb);

	if (XtIsSubclass(s, xmDialogShellWidgetClass))
	{
	    XtUnmanageChild((Widget)sb);

	    DEBUGOUT(_LtDebug2(__FILE__, (Widget)sb, w, "AutoUnmanage\n"));

	    /* XtNpopdownCallback */
	    XtCallCallbackList(s, Shell_PopdownCallback(s), NULL);
	}
    }
#endif /* 0 */
}

/*
 * SelectionBox can work in several ways :
 *      - PROMPT (as in XmCreatePromptDialog)
 *      - generic selectionBox
 *
 * In the PROMPT case, the list and corresponding label don't appear.
 *
 * A subclass, XmCommand, has only a list, a textfield, and the corresponding
 * labels.
 */
static void
initialize(Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmSelectionBoxWidget sb = (XmSelectionBoxWidget)new_w;

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

    SB_List(sb) = NULL;
    SB_ListLabel(sb) = NULL;
    SB_WorkArea(sb) = NULL;

    if (SB_DialogType(sb) != XmDIALOG_PROMPT)
    {
	_XmSelectionBoxCreateListLabel(sb);
	_XmSelectionBoxCreateList(sb);
    }

    _XmSelectionBoxCreateSelectionLabel(sb);
    _XmSelectionBoxCreateText(sb);

#if 0
    MGR_InitialFocus(sb) = SB_Text(sb);
#else
    _XmSetInitialOfTabGroup((Widget)sb, (Widget)SB_Text(sb));
#endif

    SB_Separator(sb) = NULL;
    if (SB_DialogType(sb) != XmDIALOG_COMMAND)
    {

	_XmSelectionBoxCreateSeparator(sb);
	_XmSelectionBoxCreateOkButton(sb);
	_XmSelectionBoxCreateApplyButton(sb);
	_XmSelectionBoxCreateCancelButton(sb);
	_XmSelectionBoxCreateHelpButton(sb);
    }
    else
    {
	/* core dump avoidance in GeoUtils */
	SB_Separator(sb) = NULL;
	BB_CancelButton(sb) = NULL;
	SB_OkButton(sb) = NULL;
	SB_ApplyButton(sb) = NULL;
	SB_HelpButton(sb) = NULL;
	SB_CancelLabelString(sb) = NULL;
	SB_OkLabelString(sb) = NULL;
	SB_ApplyLabelString(sb) = NULL;
	SB_HelpLabelString(sb) = NULL;
    }

#ifdef	DO_FLUSH
    XFlush(XtDisplay(SB_Text(sb)));
#endif

    if (SB_DialogType(sb) != XmDIALOG_COMMAND)
    {
#if 0
	/* rws 14 Apr 1998
	   Since SelectionBox is a sub-class of BulletinBoard, set this
	   directly, otherwise we get into nasty situations with sub-classes
	   of SelectionBox, namely FileSelectionBox.
	 */
	XtVaSetValues(new_w, XmNdefaultButton, SB_OkButton(sb), NULL);
#else
	BB_DefaultButton(new_w) = SB_OkButton(sb);
 	_XmBulletinBoardSetDynDefaultButton(new_w, SB_OkButton(sb));
#endif
    }
    if (SB_TextString(sb) == (XmString)XmUNSPECIFIED)
    {
    	SB_TextString(sb) = XmStringCreateSimple("");
    }
    else
    {
    	SB_TextString(sb) = XmStringCopy(SB_TextString(sb));
    }
    if (SB_TextString(sb))
    {
    String text;

	if (XmStringGetLtoR(SB_TextString(sb), XmSTRING_DEFAULT_CHARSET, &text))
	{
	    XmTextFieldSetString(SB_Text(sb), text);
	    XtFree(text);
	}

    }

    if (SB_SelectionLabelString(sb) == (XmString)XmUNSPECIFIED) {
       SB_SelectionLabelString(sb) = XmStringCreateSimple("");
    } else {
       SB_SelectionLabelString(sb) = XmStringCopy(SB_SelectionLabelString(sb));
    }
    if (SB_OkLabelString(sb) == (XmString)XmUNSPECIFIED) {
       SB_OkLabelString(sb) = XmStringCreateSimple("");
    } else {
       SB_OkLabelString(sb) = XmStringCopy(SB_OkLabelString(sb));
    }
    if (SB_ApplyLabelString(sb) == (XmString)XmUNSPECIFIED) {
#if 0
       SB_ApplyLabelString(sb) = XmStringCreateSimple("");
#endif
    } else {
       SB_ApplyLabelString(sb) = XmStringCopy(SB_ApplyLabelString(sb));
    }
    if (SB_CancelLabelString(sb) == (XmString)XmUNSPECIFIED) {
       SB_CancelLabelString(sb) = XmStringCreateSimple("");
    } else {
       SB_CancelLabelString(sb) = XmStringCopy(SB_CancelLabelString(sb));
    }
    if (SB_HelpLabelString(sb) == (XmString)XmUNSPECIFIED) {
       SB_HelpLabelString(sb) = XmStringCreateSimple("");
    } else {
       SB_HelpLabelString(sb) = XmStringCopy(SB_HelpLabelString(sb));
    }
    
    if (SB_List(sb) && SB_ListItems(sb) && SB_ListItemCount(sb) > 0)
    {
    	XtVaSetValues(SB_List(sb),
    		XmNitems, SB_ListItems(sb),
    		XmNitemCount, SB_ListItemCount(sb),
    		NULL);
		/* T. Straumann: List made a copy of the item list;
		 *				 we have to adjust SB_ListItems
		 */
		XtVaGetValues(SB_List(sb), XmNitems, &SB_ListItems(sb), NULL);
    }
}

static void
destroy(Widget w)
{
    XmSelectionBoxWidget sb = (XmSelectionBoxWidget)w;

    DEBUGOUT(_LtDebug("RWS", w,"%s:destroy(%d)\n",
    	__FILE__, __LINE__
    	));
    /* rws 14 Apr 1998
       Motif does not have this method.  Thats because the strings are
       shared with the child buttons and the buttons manage the storage.
     */
    if (SB_SelectionLabelString(sb)) XmStringFree(SB_SelectionLabelString(sb));
    if (SB_TextString(sb))           XmStringFree(SB_TextString(sb));
    if (SB_OkLabelString(sb))        XmStringFree(SB_OkLabelString(sb));
    if (SB_ApplyLabelString(sb))     XmStringFree(SB_ApplyLabelString(sb));
    if (SB_CancelLabelString(sb))    XmStringFree(SB_CancelLabelString(sb));
    if (SB_HelpLabelString(sb))      XmStringFree(SB_HelpLabelString(sb));
}

/*
 * handle element updates
 */
#define	NE(x)	(x(o) != x(sb))
static Boolean
set_values(Widget old,
	   Widget request,
	   Widget new_w,
	   ArgList args,
	   Cardinal *num_args)
{
    XmSelectionBoxWidget sb = (XmSelectionBoxWidget)new_w;
    XmSelectionBoxWidget o = (XmSelectionBoxWidget)old;
    Boolean r = False;		/* whether to redisplay */
    Arg al[10];
    int ac;

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

    /* this is required for every BB subclass */
    BB_InSetValues(new_w) = True;

    /* XmList */
    ac = 0;
    if (NE(SB_ListItems) || NE(SB_ListItemCount))
    {
	XtSetArg(al[ac], XmNitems, SB_ListItems(sb)); ac++;
	XtSetArg(al[ac], XmNitemCount, SB_ListItemCount(sb)); ac++;
	r = True;
    }
    if (ac && SB_List(sb))
    {
	XtSetValues(SB_List(sb), al, ac);
	/* T. Straumann: List made a copy of the item list;
	 *				 we have to adjust SB_ListItems
	 */
	al[0].value = (XtArgVal)&SB_ListItems(sb);
	XtGetValues(SB_List(sb), al, 1);
    }

    /* Text : Text string */
    ac = 0;
    if (NE(SB_TextString))
    {
	String text;

	SB_TextString(sb) = XmStringCopy(SB_TextString(sb));
	XmStringFree(SB_TextString(o));
	if (XmStringGetLtoR(SB_TextString(sb), XmSTRING_DEFAULT_CHARSET, &text))
	{
	    XmTextFieldSetString(SB_Text(sb), text);
	    XtFree(text);
	    r = True;
	}
    }

    /* Labels : Selection Label */
    ac = 0;
    if (NE(SB_SelectionLabelString))
    {
	SB_SelectionLabelString(sb) = XmStringCopy(SB_SelectionLabelString(sb));
	XmStringFree(SB_SelectionLabelString(o));
	XtSetArg(al[ac], XmNlabelString, SB_SelectionLabelString(sb)); ac++;
	r = True;
    }
    if (ac && SB_SelectionLabel(sb))
    {
	XtSetValues(SB_SelectionLabel(sb), al, ac);
    }

    /* Labels : List Label */
    ac = 0;
    if (NE(SB_ListLabelString))
    {
	SB_ListLabelString(sb) = XmStringCopy(SB_ListLabelString(sb));
	XmStringFree(SB_ListLabelString(o));
	XtSetArg(al[ac], XmNlabelString, SB_ListLabelString(sb)); ac++;
	r = True;
    }
    if (ac && SB_ListLabel(sb))
    {
	XtSetValues(SB_ListLabel(sb), al, ac);
    }

    /* Labels : OK Button Label */
    ac = 0;
    if (NE(SB_OkLabelString))
    {
	SB_OkLabelString(sb) = XmStringCopy(SB_OkLabelString(sb));
	XmStringFree(SB_OkLabelString(o));
	XtSetArg(al[ac], XmNlabelString, SB_OkLabelString(sb)); ac++;
	r = True;
    }
    if (ac && SB_OkButton(sb))
    {
	XtSetValues(SB_OkButton(sb), al, ac);
    }

    /* Labels : Cancel Button Label */
    ac = 0;
    if (NE(SB_CancelLabelString))
    {
	SB_CancelLabelString(sb) = XmStringCopy(SB_CancelLabelString(sb));
	XmStringFree(SB_CancelLabelString(o));
	XtSetArg(al[ac], XmNlabelString, SB_CancelLabelString(sb)); ac++;
	r = True;
    }
    if (ac && SB_CancelButton(sb))
    {
	XtSetValues(BB_CancelButton(sb), al, ac);
    }

    /* Labels : Apply Button Label */
    ac = 0;
    if (NE(SB_ApplyLabelString))
    {
	SB_ApplyLabelString(sb) = XmStringCopy(SB_ApplyLabelString(sb));
	XmStringFree(SB_ApplyLabelString(o));
	XtSetArg(al[ac], XmNlabelString, SB_ApplyLabelString(sb)); ac++;
	r = True;
    }
    if (ac && SB_ApplyButton(sb))
    {
	XtSetValues(SB_ApplyButton(sb), al, ac);
    }

    /* Labels : Help Button Label */
    ac = 0;
    if (NE(SB_HelpLabelString))
    {
	SB_HelpLabelString(sb) = XmStringCopy(SB_HelpLabelString(sb));
	XmStringFree(SB_HelpLabelString(o));
	XtSetArg(al[ac], XmNlabelString, SB_HelpLabelString(sb)); ac++;
	r = True;
    }
    if (ac && SB_HelpButton(sb))
    {
	XtSetValues(SB_HelpButton(sb), al, ac);
    }

    BB_InSetValues(new_w) = False;

    if (r && (XtClass(new_w) == xmSelectionBoxWidgetClass))
    {
	_XmBulletinBoardSizeUpdate(new_w);

	return False;
    }

    return r;
}

/*
 * MLM - I rather suspect that the following functions are an attempt to
 * foil the user that changes the dialog type of an existing selbox
 * widget; not all component widgets are created during initialization,
 * so if the dialog_type changes, the set_values method can invoke these
 * functions for the NULL components.
 */
void
_XmSelectionBoxCreateListLabel(XmSelectionBoxWidget sb)
{
    int ac;
    Arg al[2];

    if (SB_DialogType(sb) != XmDIALOG_COMMAND)
    {
	SB_ListLabel(sb) = _XmBB_CreateLabelG((Widget)sb,
					      SB_ListLabelString(sb),
					      "Items");
	ac = 0;
	XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING); ac++;
	XtSetValues(SB_ListLabel(sb), al, ac);
	XtManageChild(SB_ListLabel(sb));
    }
    else
    {
	SB_ListLabel(sb) = NULL;
    }
}

void
_XmSelectionBoxCreateSelectionLabel(XmSelectionBoxWidget sb)
{
    int ac;
    Arg al[2];

    SB_SelectionLabel(sb) = _XmBB_CreateLabelG((Widget)sb,
					       SB_SelectionLabelString(sb),
					       "Selection");
    ac = 0;
    XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING); ac++;
    XtSetValues(SB_SelectionLabel(sb), al, ac);
    XtManageChild(SB_SelectionLabel(sb));
}

void
_XmSelectionBoxCreateList(XmSelectionBoxWidget sb)
{
    int ac;
    Arg al[4];

    ac = 0;
    XtSetArg(al[ac], XmNvisibleItemCount, SB_ListVisibleItemCount(sb)); ac++;
    XtSetArg(al[ac], XmNselectionPolicy, XmBROWSE_SELECT); ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT); ac++;
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmAS_NEEDED); ac++;
    SB_List(sb) = XmCreateScrolledList((Widget)sb, "ItemsList", al, ac);
    XtManageChild(SB_List(sb));

    /*
    XtAddCallback(SB_List(sb),
		  XmNdefaultActionCallback, _XmSbButton, NULL);
		  */
    XtAddCallback(SB_List(sb),
		  XmNsingleSelectionCallback, ListSingleSelect, sb);
    XtAddCallback(SB_List(sb),
		  XmNbrowseSelectionCallback, ListSingleSelect, sb);
}

void
_XmSelectionBoxCreateText(XmSelectionBoxWidget sb)
{
    int ac;
    Arg al[2];
    XtTranslations save_acc;

    ac = 0;
    XtSetArg(al[0], XmNnavigationType, XmSTICKY_TAB_GROUP); ac++;
    SB_Text(sb) = XmCreateTextField((Widget)sb, "Text", al, ac);
    save_acc = CoreAccelerators(sb);
    if (SB_TextAccelerators(sb) == NULL)
    {
	CoreAccelerators(sb) = text_accelerators_parsed;
    }
    else
    {
	CoreAccelerators(sb) = SB_TextAccelerators(sb);
    }
    XtInstallAccelerators(SB_Text(sb), (Widget) sb);
    CoreAccelerators(sb) = save_acc;
    XtManageChild(SB_Text(sb));
}

void
_XmSelectionBoxCreateSeparator(XmSelectionBoxWidget sb)
{
    int ac;
    Arg al[2];

    ac = 0;
    XtSetArg(al[ac], XmNseparatorType, XmSHADOW_ETCHED_IN); ac++;
    XtSetArg(al[ac], XmNhighlightThickness, 0); ac++;
#ifdef USE_WIDGETS
    SB_Separator(sb) = XmCreateSeparator((Widget)sb, "Separator", al, ac);
#else
    SB_Separator(sb) = XmCreateSeparatorGadget((Widget)sb, "Separator", al, ac);
#endif
    XtManageChild(SB_Separator(sb));
}

void
_XmSelectionBoxCreateOkButton(XmSelectionBoxWidget sb)
{
    SB_OkButton(sb) = _XmBB_CreateButtonG((Widget)sb,
					  SB_OkLabelString(sb),
					  "OK");
    XtManageChild(SB_OkButton(sb));
    XtAddCallback(SB_OkButton(sb),
		  XmNactivateCallback, _XmSbButton, NULL);
}

void
_XmSelectionBoxCreateApplyButton(XmSelectionBoxWidget sb)
{
    SB_ApplyButton(sb) = _XmBB_CreateButtonG((Widget)sb,
					     SB_ApplyLabelString(sb),
					     "Apply");
    /* get rid of auto_unmanage */
    XtRemoveAllCallbacks(SB_ApplyButton(sb), XmNactivateCallback);
    XtAddCallback(SB_ApplyButton(sb),
		  XmNactivateCallback, _XmSbButton, NULL);
    if (XmIsDialogShell(XtParent(sb)) && (SB_DialogType(sb) != XmDIALOG_PROMPT))
    {
	XtManageChild(SB_ApplyButton(sb));
    }
}

void
_XmSelectionBoxCreateCancelButton(XmSelectionBoxWidget sb)
{
    BB_CancelButton(sb) = _XmBB_CreateButtonG((Widget)sb,
					      SB_CancelLabelString(sb),
					      "Cancel");
    XtManageChild(BB_CancelButton(sb));
    XtAddCallback(BB_CancelButton(sb),
		  XmNactivateCallback, _XmSbButton, NULL);
}

void
_XmSelectionBoxCreateHelpButton(XmSelectionBoxWidget sb)
{
    SB_HelpButton(sb) = _XmBB_CreateButtonG((Widget)sb,
					    SB_HelpLabelString(sb),
					    "Help");
    XtManageChild(SB_HelpButton(sb));
    XtRemoveAllCallbacks(SB_HelpButton(sb), XmNactivateCallback);
    /* get rid of auto_unmanage */

    XtAddCallback(SB_HelpButton(sb),
		  XmNactivateCallback, _XmSbButton, NULL);
}

/*
 * MLM: 040296: I believe that most of the following are for syn_resource
 * export procs
 */
void
_XmSelectionBoxGetSelectionLabelString(Widget wid, int resource_offset,
				       XtArgVal *value)
{
}

void
_XmSelectionBoxGetListLabelString(Widget wid, int resource_offset,
				  XtArgVal *value)
{
}

void
_XmSelectionBoxGetTextColumns(Widget wid, int resource_offset,
			      XtArgVal *value)
{
    Arg a;
    int n;

    DEBUGOUT(_LtDebug(__FILE__, wid, "XmSelectionBoxGettTextColumns\n"));

    XtSetArg(a, XmNtextColumns, &n);
    n = -1;
    XtGetValues(SB_Text(wid), &a, 1);

    /* Should this be a copy or the real thing ?? */
    *value = (XtArgVal)n;
}

void
_XmSelectionBoxGetTextString(Widget wid, int resource_offset,
			     XtArgVal *value)
{
    char *str;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmSelectionBoxGetTextString\n"));

    str = XmTextFieldGetString(SB_Text(wid));
    *value = (XtArgVal)XmStringCreateSimple(str);
    XtFree(str);
}

void
_XmSelectionBoxGetListItems(Widget wid, int resource_offset,
			    XtArgVal *value)
{
    XmStringTable l;
    Arg a;

    DEBUGOUT(_LtDebug(__FILE__, wid,
		      "XmSelectionBoxGetListItems(_, %d, _)\n",
		      resource_offset));

    XtSetArg(a, XmNitems, &l);
    l = NULL;
    XtGetValues(SB_List(wid), &a, 1);

    /* Should this be a copy or the real thing ?? */
    /* MLM: This (I think) should be a copy, as the user might free it. Unless
     * the List doesn't make a copy when you do the GetValues call.  You
     * probably could use the access macros from ListP.h and make a copy of
     * that, for better code efficiency.  Also, remember that if the List has
     * a Synthetic resource for ListItems, and it makes a copy, then you've
     * got a copy already if you've used XtGetValues()...
     */

    *value = (XtArgVal)l;
}

void
_XmSelectionBoxGetListItemCount(Widget wid, int resource_offset,
				XtArgVal *value)
{
    Arg a;
    int n;

    DEBUGOUT(_LtDebug(__FILE__, wid, "XmSelectionBoxGetListItemCount\n"));

    XtSetArg(a, XmNitemCount, &n);
    n = -1;
    XtGetValues(SB_List(wid), &a, 1);
    /* Should this be a copy or the real thing ?? */
    /* MLM: I don't think this matters here.  Except that you may want to use
     * the access macros from ListP.h, and avoid the GetValues overhead.
     */
    *value = (XtArgVal)n;
}

void
_XmSelectionBoxGetListVisibleItemCount(Widget wid, int resource_offset,
				       XtArgVal *value)
{
}

void
_XmSelectionBoxGetOkLabelString(Widget wid, int resource_offset,
				XtArgVal *value)
{
}

void
_XmSelectionBoxGetApplyLabelString(Widget wid, int resource_offset,
				   XtArgVal *value)
{
}

void
_XmSelectionBoxGetCancelLabelString(Widget wid, int resource_offset,
				    XtArgVal *value)
{
}

void
_XmSelectionBoxGetHelpLabelString(Widget wid, int resource_offset,
				  XtArgVal *value)
{
}

void
_XmSelectionBoxUpOrDown(Widget wid, XEvent *event,
			String *argv, Cardinal *argc)
{
    Arg args[4];
    Widget text = SB_Text(wid), list = SB_List(wid);
    int type, top_item_position, visible_item_count, item_count;
    XmStringTable items;
    int *position_list, position_count, new_position, old_position;
    /* char *selected_text; */

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmSelectionBoxUpOrDown %s\n",
		      argv ? *argv: "NULL"));
    if (list != NULL && text != NULL)
    {
	if (*argc != 1)
	{
	    return;
	}
	type = atoi(*argv);
	XtSetArg(args[0], XmNtopItemPosition, &top_item_position);
	XtSetArg(args[1], XmNvisibleItemCount, &visible_item_count);
	XtSetArg(args[2], XmNitemCount, &item_count);
	XtSetArg(args[3], XmNitems, &items);
	XtGetValues(list, args, 4);
	if (item_count == 0)
	{
	    return;
	}
	if (XmListGetSelectedPos(list, &position_list, &position_count))
	{
	    old_position = *position_list;
	    XtFree((char *) position_list);
	}
	else
	{
	    old_position = 0;
	}
	
	switch (type)
	{
	case 0:
	    /* move up */
	    if (old_position > 1)
	    {
		new_position = old_position - 1;
	    }
	    else
	    {
		new_position = 1;
	    }
	    break;
	case 1:
	    /* move down */
	    if (old_position < item_count)
	    {
		new_position = old_position + 1;
	    }
	    else
	    {
		new_position = item_count;
	    }
	    break;
	case 2:
	    /* move to first item */
	    new_position = 1;
	    break;
	case 3:
	    /* move to last item */
	    new_position = item_count;
	    break;
	default:
	    new_position = old_position;
	    break;
	}
	if (new_position != old_position)
	{
	    if (new_position < top_item_position)
	    {
		XmListSetPos(list, new_position);
	    }
	    else if (new_position >= top_item_position + visible_item_count)
	    {
		XmListSetBottomPos(list, new_position);
	    }
	    XmListSelectPos(list, new_position, True);
	}
    }
}

void
_XmSelectionBoxRestore(Widget wid, XEvent *event,
		       String *argv, Cardinal *argc)
{
    Arg args[2];
    Widget text = SB_Text(wid), list = SB_List(wid);
    XmStringTable items;
    int item_count, *position_list, position_count;
    char *selected_text;

    DEBUGOUT(_LtDebug(__FILE__, wid, "_XmSelectionBoxRestore\n"));

    if (text != NULL && list != NULL)
    {
	XtSetArg(args[0], XmNitemCount, &item_count);
	XtSetArg(args[1], XmNitems, &items);
	XtGetValues(list, args, 2);
	if (item_count == 0 || 
	    !XmListGetSelectedPos(list, &position_list, &position_count))
	{
	  selected_text = XtMalloc(sizeof(char));
	  *selected_text = '\0';
	}
	else
	{
	    if (!XmStringGetLtoR(items[*position_list - 1],
				 XmFONTLIST_DEFAULT_TAG,
				 &selected_text))
	    {
		selected_text = XtMalloc(sizeof(char));
		*selected_text = '\0';
	    }
	    XtFree((char *) position_list);
	}
	XmTextFieldSetString(text, selected_text);
	XtFree(selected_text);
	XmTextFieldSetInsertionPosition(text,
					XmTextFieldGetLastPosition(text));
    }
}

XmGeoMatrix
_XmSelectionBoxGeoMatrixCreate(Widget _w, Widget _from, XtWidgetGeometry *_pref)
{
    XmGeoMatrix geoSpec;
    register XmGeoRowLayout layoutPtr;
    register XmKidGeometry boxPtr;
    Cardinal numKids;
    Boolean newRow;
    Cardinal nrows, i, nextras;
    Widget *extras;

    numKids = MGR_NumChildren(_w);

    nextras = 0;
    extras = NULL;
    for (i = 0; i < numKids; i++)
    {
	/*
	 * There seems to be no way to add a button to the button area
	 * without first managing a child in the work area. Motif allows
	 * me to create an "unmanaged" child in the work area so that extra
	 * buttons will be in the proper place. -- rws
	 */
	if (MGR_Children(_w)[i] != SB_ListLabel(_w) &&
	    (SB_List(_w)
	     ? MGR_Children(_w)[i] != XtParent(SB_List(_w))
	     : True) &&
	    MGR_Children(_w)[i] != SB_SelectionLabel(_w) &&
	    MGR_Children(_w)[i] != SB_Text(_w) &&
	    MGR_Children(_w)[i] != SB_Separator(_w) &&
	    MGR_Children(_w)[i] != SB_OkButton(_w) &&
	    MGR_Children(_w)[i] != SB_ApplyButton(_w) &&
	    MGR_Children(_w)[i] != SB_HelpButton(_w) &&
	    MGR_Children(_w)[i] != BB_CancelButton(_w))
	{
	    nextras++;
	}
    }

    if (nextras)
    {
	extras = (Widget *)XtMalloc(sizeof(Widget) * nextras);
    }

    nextras = 0;
    for (i = 0; i < numKids; i++)
    {
	if (MGR_Children(_w)[i] != SB_ListLabel(_w) &&
	    (SB_List(_w)
	     ? MGR_Children(_w)[i] != XtParent(SB_List(_w))
	     : True) &&
	    MGR_Children(_w)[i] != SB_SelectionLabel(_w) &&
	    MGR_Children(_w)[i] != SB_Text(_w) &&
	    MGR_Children(_w)[i] != SB_Separator(_w) &&
	    MGR_Children(_w)[i] != SB_OkButton(_w) &&
	    MGR_Children(_w)[i] != SB_ApplyButton(_w) &&
	    MGR_Children(_w)[i] != SB_HelpButton(_w) &&
	    MGR_Children(_w)[i] != BB_CancelButton(_w))
	{
	    extras[nextras] = MGR_Children(_w)[i];
	    nextras++;
	}
    }

    nrows = 0;

    /* note the starting from one.  The zero'th child is the "work area" */
    if (nextras > 0)
    {
	for (i = 1; i < nextras; i++)
	{
	    if (XmIsMenuBar(extras[i]) && XtIsManaged(extras[i]))
		nrows++;
	}
	if (extras[0] && XtIsManaged(extras[0]))
	    nrows++;
    }

    if (SB_ListLabel(_w) && XtIsManaged(SB_ListLabel(_w)))
    {
	nrows++;
    }

    if (SB_List(_w) && XtIsManaged(SB_List(_w)))
    {
	nrows++;
    }

    if (SB_SelectionLabel(_w) && XtIsManaged(SB_SelectionLabel(_w)))
    {
	nrows++;
    }

    if (SB_Text(_w) && XtIsManaged(SB_Text(_w)))
    {
	nrows++;
    }

    if (SB_Separator(_w) && XtIsManaged(SB_Separator(_w)))
    {
	nrows++;
    }

    if ((BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w))) ||
	(SB_OkButton(_w) && XtIsManaged(SB_OkButton(_w))) ||
	(SB_ApplyButton(_w) && XtIsManaged(SB_ApplyButton(_w))) ||
	(SB_HelpButton(_w) && XtIsManaged(SB_HelpButton(_w))))
    {
	nrows++;
    }
    else
    {
	for (i = 1; i < nextras; i++)
	{
	    if (extras[i] && XtIsManaged(extras[i]) &&
		(XmIsPushButton(extras[i]) || XmIsPushButtonGadget(extras[i])))
	    {
		nrows++;
		break;
	    }
	}
    }

    geoSpec = _XmGeoMatrixAlloc(nrows, numKids, 0);
    geoSpec->composite = (Widget)_w;
    geoSpec->instigator = (Widget)_from;
    if (_pref)
    {
	geoSpec->instig_request = *_pref;
    }
    geoSpec->margin_w = BB_MarginWidth(_w) + MGR_ShadowThickness(_w);
    geoSpec->margin_h = BB_MarginHeight(_w) + MGR_ShadowThickness(_w);
    geoSpec->no_geo_request = _XmSelectionBoxNoGeoRequest;

    layoutPtr = &(geoSpec->layouts->row);
    boxPtr = geoSpec->boxes;

    for (i = 1; i < nextras; i++)
    {
	if (XmIsMenuBar(extras[i]) && XtIsManaged(extras[i]) &&
	    _XmGeoSetupKid(boxPtr, extras[i]))
	{
	    layoutPtr->fix_up = _XmMenuBarFix;
	    layoutPtr->space_above = 0;
	    boxPtr += 2;
	    layoutPtr++;
	    break;
	}
    }

    if (SB_ChildPlacement(_w) == XmPLACE_TOP && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
	nrows++;
    }

    if (SB_DialogType(_w) == XmDIALOG_PROMPT &&
	SB_ChildPlacement(_w) == XmPLACE_ABOVE_SELECTION && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
	nrows++;
    }

    newRow = False;
    if (SB_ListLabel(_w) && XtIsManaged(SB_ListLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_ListLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = True;
	boxPtr++;
    }

    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    if (SB_DialogType(_w) == XmDIALOG_COMMAND &&
	SB_ChildPlacement(_w) == XmPLACE_ABOVE_SELECTION && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
	nrows++;
    }

    newRow = False;
    if (SB_List(_w) && XtIsManaged(SB_List(_w)) &&
	_XmGeoSetupKid(boxPtr, XtParent(SB_List(_w))))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->min_height = 40;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->fit_mode = XmGEO_PROPORTIONAL;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = 0;	/* BB_MarginHeight(_w); */
	layoutPtr->space_between = BB_MarginWidth(_w);
	newRow = True;
	boxPtr++;
    }

    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    if (SB_DialogType(_w) != XmDIALOG_COMMAND &&
	SB_DialogType(_w) != XmDIALOG_PROMPT &&
	SB_ChildPlacement(_w) == XmPLACE_ABOVE_SELECTION && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 4;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
	nrows++;
    }

    if (SB_SelectionLabel(_w) && XtIsManaged(SB_SelectionLabel(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_SelectionLabel(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 0;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
    }

    if (SB_Text(_w) && XtIsManaged(SB_Text(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_Text(_w)))
    {
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_height = 1;
	layoutPtr->even_width = 0;
	layoutPtr->space_above = 0;	/* BB_MarginHeight(_w); */
	boxPtr += 2;
	layoutPtr++;
    }

    if (SB_ChildPlacement(_w) == XmPLACE_BELOW_SELECTION && nextras &&
	extras[0] && XtIsManaged(extras[0]) &&
	_XmGeoSetupKid(boxPtr, extras[0]))
    {
	layoutPtr->stretch_height = 1;
	layoutPtr->fill_mode = XmGEO_EXPAND;
	layoutPtr->even_width = 1;
	layoutPtr->even_height = 1;
	layoutPtr->space_above = BB_MarginHeight(_w);
	layoutPtr++;
	boxPtr += 2;
	nrows++;
    }

    if (SB_Separator(_w) && XtIsManaged(SB_Separator(_w)) &&
	_XmGeoSetupKid(boxPtr, SB_Separator(_w)))
    {
	layoutPtr->fix_up = _XmSeparatorFix;
	layoutPtr->space_above = BB_MarginHeight(_w);
	boxPtr += 2;
	layoutPtr++;
    }

    newRow = False;
    if (SB_OkButton(_w) && XtIsManaged(SB_OkButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, SB_OkButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    for (i = 1; i < nextras; i++)
    {
	if (extras[i] && XtIsManaged(extras[i]) &&
	    (XmIsPushButton(extras[i]) || XmIsPushButtonGadget(extras[i])) &&
	    _XmGeoSetupKid(boxPtr++, extras[i]))
	{
	    _XmBulletinBoardSetDefaultShadow(extras[i]);
	    layoutPtr->fill_mode = XmGEO_CENTER;
	    layoutPtr->fit_mode = XmGEO_WRAP;
	    layoutPtr->even_width = 6;
	    layoutPtr->even_height = 6;
	    layoutPtr->space_above = BB_MarginHeight(_w);
	    newRow = True;
	}
    }

    if (SB_ApplyButton(_w) && XtIsManaged(SB_ApplyButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, SB_ApplyButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    if (BB_CancelButton(_w) && XtIsManaged(BB_CancelButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, BB_CancelButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }
    if (SB_HelpButton(_w) && XtIsManaged(SB_HelpButton(_w)) &&
	_XmGeoSetupKid(boxPtr++, SB_HelpButton(_w)))
    {
	layoutPtr->fill_mode = XmGEO_CENTER;
	layoutPtr->fit_mode = XmGEO_WRAP;
	layoutPtr->even_width = 6;
	layoutPtr->even_height = 6;
	layoutPtr->space_above = BB_MarginHeight(_w);
	newRow = True;
    }

    if (newRow)
    {
	layoutPtr++;
	boxPtr++;
    }

    layoutPtr->space_above = 0;	/* BB_MarginHeight(_w); */
    layoutPtr->end = True;
    if (nextras)
    {
	XtFree((char *)extras);
    }
    return (geoSpec);
}

Boolean
_XmSelectionBoxNoGeoRequest(XmGeoMatrix _geoSpec)
{
    if (BB_InSetValues(_geoSpec->composite) &&
	(XtClass(_geoSpec->composite) == xmSelectionBoxWidgetClass ||
	 XtClass(_geoSpec->composite) == xmCommandWidgetClass))
    {
	return True;
    }

    return False;
}

Widget
XmCreateSelectionBox(Widget parent, char *name,
		     Arg *argList, Cardinal argcount)
{
    return XtCreateWidget(name, xmSelectionBoxWidgetClass, parent,
			  argList, argcount);
}

Widget
XmCreateSelectionDialog(Widget parent, char *name,
			Arg *argList, Cardinal argcount)
{
    Widget d, r;
    char *s;
    Arg *al = (Arg *)XtCalloc(argcount + 1, sizeof(Arg));
    Cardinal ac, i;

    s = _XmMakeDialogName(name);

    ac = 0;
    XtSetArg(al[ac], XmNallowShellResize, True); ac++;
    for (i = 0; i < argcount; i++)
    {
	XtSetArg(al[ac], argList[i].name, argList[i].value); ac++;
    }

#if 0
    d = XtCreateWidget(s, xmDialogShellWidgetClass, parent, al, ac);
#else
    d = XmCreateDialogShell(parent, s, al, ac);
#endif
    XtFree(s);

    r = XtCreateWidget(name, xmSelectionBoxWidgetClass, d, al, ac);
    XtFree((XPointer)al);
    return r;
}

Widget
XmCreatePromptDialog(Widget parent, char *name,
		     Arg *argList, Cardinal argcount)
{
    Widget d, ret;
    char *s;
    Arg *al;
    Cardinal i;

    s = _XmMakeDialogName(name);

    al = (Arg *)XtCalloc(argcount + 1, sizeof(Arg));
    for (i = 0; i < argcount; i++)
    {
	al[i] = argList[i];
    }

    XtSetArg(al[argcount], XmNallowShellResize, True); argcount++;
    d = XtCreateWidget(s, xmDialogShellWidgetClass, parent, al, argcount);
    XtFree(s);

    /* This resource goes to the selectionBox, the previous one goes to Shell.
     * Use the same spot in the array for both. */
    XtSetArg(al[argcount - 1], XmNdialogType, XmDIALOG_PROMPT);
    ret = XtCreateWidget(name, xmSelectionBoxWidgetClass, d, al, argcount);

    XtFree((XPointer)al);

    return ret;
}


/* This routine is marked as obsolete as of Motif 2.1 */
Widget
XmSelectionBoxGetChild(Widget w, unsigned char child)
{
    XmSelectionBoxWidget sb = (XmSelectionBoxWidget)w;

    switch (child)
    {

    case XmDIALOG_APPLY_BUTTON:
	return SB_ApplyButton(sb);

    case XmDIALOG_CANCEL_BUTTON:
	return BB_CancelButton(sb);

    case XmDIALOG_DEFAULT_BUTTON:
	return BB_DefaultButton(sb);

    case XmDIALOG_HELP_BUTTON:
	return SB_HelpButton(sb);

    case XmDIALOG_LIST:
	return SB_List(sb);

    case XmDIALOG_LIST_LABEL:
	return SB_ListLabel(sb);
	
    case XmDIALOG_OK_BUTTON:
	return SB_OkButton(sb);

    case XmDIALOG_SELECTION_LABEL:
	return SB_SelectionLabel(sb);

    case XmDIALOG_SEPARATOR:
	return SB_Separator(sb);

    case XmDIALOG_TEXT:
	return SB_Text(sb);

    case XmDIALOG_WORK_AREA:
	return SB_WorkArea(sb);

    /* These are illegal */
    default:
	return NULL;
    }
}


/*      
 * Keep track of button children
 */
static void
insert_child(Widget w)
{
#define superclass      (&xmBulletinBoardClassRec)
    (*superclass->composite_class.insert_child) (w);
#undef  superclass

#if 0
    /* rws 12 Mar 1998
       Now that the ParentProcess we do not need to add this.  Adding
       this makes the callback happen twice.
     */
    if (_XmIsFastSubclass(XtClass(w), XmTEXT_FIELD_BIT))
    {
	XtAddCallback(w, XmNactivateCallback, _XmSbButton, NULL);
    }
#endif
}


static void
DeleteChild(Widget w)
{
	Widget	sb = XtParent(w);

#define superclass      (&xmBulletinBoardClassRec)
    (*superclass->composite_class.delete_child) (w);
#undef  superclass

	if (w == SB_HelpButton(sb))
		SB_HelpButton(sb) = NULL;
	if (w == SB_OkButton(sb))
		SB_OkButton(sb) = NULL;
	if (w == SB_ApplyButton(sb))
		SB_ApplyButton(sb) = NULL;
	if (w == SB_Separator(sb))
		SB_Separator(sb) = NULL;
	if (w == SB_Text(sb))
		SB_Text(sb) = NULL;
	if (w == SB_SelectionLabel(sb))
		SB_SelectionLabel(sb) = NULL;
	if (w == SB_List(sb))
		SB_List(sb) = NULL;
	if (w == SB_WorkArea(sb))
		SB_WorkArea(sb) = NULL;
	if (w == SB_ListLabel(sb))
		SB_ListLabel(sb) = NULL;
}


static void
_XmDialogTypeDefault(Widget w, int offset, XrmValue *value)
{
    static unsigned char dialog_type;
    
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmDialogTypeDefault\n"));
    
    dialog_type = XmDIALOG_WORK_AREA;
    
    value->addr = (char *) &dialog_type;
    value->size = sizeof (unsigned char);
    
    if (XmIsDialogShell(XtParent(w)))
    {
	dialog_type = XmDIALOG_SELECTION;
    }
}
