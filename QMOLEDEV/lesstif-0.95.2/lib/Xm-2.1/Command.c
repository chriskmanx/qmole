/**
 *
 * $Id: Command.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

static const char rcsid[] = "$Id: Command.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>

#include <Xm/XmP.h>
#include <Xm/CommandP.h>
#include <Xm/DialogS.h>
#include <Xm/ListP.h>
#include <Xm/TextF.h>

#include <XmI/DebugUtil.h>


/* Forward Declarations */

/*static void class_initialize();*/

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

/*
static void destroy(Widget w);
*/

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static Boolean _XmCommandParentProcess(Widget widget, XmParentProcessData data);

static void _XmCommandGetHistoryItems(Widget w, int offset, XtArgVal *value);

static void _XmCommandGetHistoryItemCount(Widget w, int offset, XtArgVal *value);

static void _XmCommandGetVisibleItemCount(Widget w, int offset, XtArgVal *value);


/*
 * Resources for the Selection Box class
 */
#define Offset(field) XtOffsetOf(XmCommandRec, command.field)
#define SBOffset(field) XtOffsetOf(XmCommandRec, selection_box.field)
#define BBOffset(field) XtOffsetOf(XmCommandRec, bulletin_board.field)
static XtResource resources[] =
{
    {
	XmNcommandEnteredCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNcommandChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(value_changed_callback),
	XmRCallback, (XtPointer)NULL
    },
    {
	XmNpromptString, XmCPromptString, XmRXmString,
	sizeof(XmString), SBOffset(selection_label_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNcommand, XmCTextString, XmRXmString,
	sizeof(XmString), SBOffset(text_string),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNhistoryItems, XmCItems, XmRXmStringTable,
	sizeof(XmStringTable), SBOffset(list_items),
	XmRImmediate, NULL
    },
    {
	XmNhistoryItemCount, XmCItemCount, XmRInt,
	sizeof(int), SBOffset(list_item_count),
	XmRImmediate, (XtPointer)XmUNSPECIFIED
    },
    {
	XmNhistoryMaxItems, XmCMaxItems, XmRInt,
	sizeof(int), Offset(history_max_items),
	XmRImmediate, (XtPointer)100
    },
    {
	XmNhistoryVisibleItemCount, XmCVisibleItemCount, XmRInt,
	sizeof(int), SBOffset(list_visible_item_count),
	XmRImmediate, (XtPointer)8
    },
    {
	XmNdialogType, XmCDialogType, XmRSelectionType,
	sizeof(unsigned char), SBOffset(dialog_type),
	XmRImmediate, (XtPointer)XmDIALOG_COMMAND
    },
    {
	XmNdefaultPosition, XmCDefaultPosition, XmRBoolean,
	sizeof(Boolean), BBOffset(default_position),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNautoUnmanage, XmCAutoUnmanage, XmRBoolean,
	sizeof(Boolean), BBOffset(auto_unmanage),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNresizePolicy, XmCResizePolicy, XmRResizePolicy,
	sizeof(unsigned char), BBOffset(resize_policy),
	XmRImmediate, (XtPointer)XmRESIZE_NONE
    }
};

static XmSyntheticResource syn_resources[] =
{
    {
	XmNpromptString,
	sizeof(XmString), SBOffset(selection_label_string),
	_XmExportXmString, NULL
    },
    {
	XmNcommand,
	sizeof(XmString), SBOffset(text_string),
	_XmSelectionBoxGetTextString, NULL
    },
    {
	XmNhistoryItems,
	sizeof(XmStringTable), SBOffset(list_items),
	_XmCommandGetHistoryItems , NULL
    },
    {
	XmNhistoryItemCount,
	sizeof(int), SBOffset(list_item_count),
	_XmCommandGetHistoryItemCount , NULL
    },
    {
	XmNhistoryVisibleItemCount,
	sizeof(int), SBOffset(list_visible_item_count),
	_XmCommandGetVisibleItemCount , NULL
    }
};

static XtActionsRec actions[] =
{
	{"Return", _XmCommandReturn},
	{"UpOrDown", _XmCommandUpOrDown},
	{"BulletinBoardReturn", _XmBulletinBoardReturn},
	{"SelectionBoxUpOrDown", _XmSelectionBoxUpOrDown},
};

/* *INDENT-OFF* */
#if 0
static XmBaseClassExtRec _XmCommandCoreClassExtRec = {
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
#endif

XmCommandClassRec xmCommandClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &xmSelectionBoxClassRec,
        /* class_name            */ "XmCommand",
	/* widget_size           */ sizeof(XmCommandRec),
	/* class_initialize      */ NULL /*class_initialize*/,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ XtInheritRealize,
	/* actions               */ actions /*NULL*/,
	/* num_actions           */ XtNumber(actions),
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ True,
	/* compress_exposure     */ XtExposeCompressSeries /*XtExposeCompressMultiple*/,
	/* compress_enterleave   */ True,
	/* visible_interest      */ False,
	/* destroy               */ NULL /*destroy*/,
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
	/* extension             */ (XtPointer)NULL /*&_XmCommandCoreClassExtRec*/
    },
    /* Composite class part */
    {
	/* geometry manager */ XtInheritGeometryManager, 
        /* change_managed   */ XtInheritChangeManaged, 
        /* insert_child     */ XtInheritInsertChild,
        /* delete_child     */ XtInheritDeleteChild,
        /* extension        */ NULL
    },
    /* Constraint class part */
    {
	/* subresources      */ NULL,
        /* subresource_count */ 0,
        /* constraint_size   */ 0,
        /* initialize        */ NULL,
        /* destroy           */ NULL,
        /* set_values        */ NULL,
        /* extension         */ NULL
    },
    /* XmManager class part */
    {
        /* translations                 */ XtInheritTranslations,
        /* syn_resources                */ syn_resources,
        /* num_syn_resources            */ XtNumber(syn_resources),
        /* syn_constraint_resources     */ NULL,
        /* num_syn_constraint_resources */ 0,
        /* parent_process               */ _XmCommandParentProcess,
        /* extension                    */ (XtPointer)NULL
    },
    /* XmBulletinBoard Area part */
    {
	/* always_install_accelerators  */ True /*False*/,
	/* geo_matrix_create            */ XmInheritGeoMatrixCreate,
	/* focus_moved_proc             */ XmInheritFocusMovedProc,
	/* extension                    */ NULL

    },
    /* XmSelectionBox part */
    {
	/* list_callback                */ NULL,
	/* extension                    */ NULL,
    },
    /* XmCommand Class Part */
    {
	/* extension                    */ NULL,
    }
};
/* *INDENT-ON* */


WidgetClass xmCommandWidgetClass = (WidgetClass)&xmCommandClassRec;

#if 0
static void
class_initialize()
{
    _XmCommandCoreClassExtRec.record_type = XmQmotif;
}
#endif

static void
class_part_initialize(WidgetClass widget_class)
{
    _XmFastSubclassInit(widget_class, XmCOMMAND_BOX_BIT);
}

#define	C_ACT		((XtPointer)1)	/* ActivateCallback on textfield */
#define	C_LIST_SELECT	((XtPointer)2)	/* Selection on list */
#define	C_LIST_DOUBLE	((XtPointer)3)	/* DoubleClick on list */

static void
_XmCommandCallback(Widget w, XtPointer client, XtPointer call)
{
    XmCommandCallbackStruct cbs;
    XmAnyCallbackStruct *p = (XmAnyCallbackStruct *)call;
    XmListCallbackStruct *l = (XmListCallbackStruct *) call;
    XmCommandWidget cw = (XmCommandWidget)XtParent(w);
    char *t;

    if (client != C_ACT)
	cw = (XmCommandWidget)XtParent(XtParent(w));

    DEBUGOUT(_LtDebug(__FILE__, (Widget)cw, "_XmCommandCallback [%s]\n",
	(client == C_ACT) ? "TextField Activate" : "List Selection"));

    /*
     * In the cases of TextField Activate and List DefaultAction, we need to
     * call a callback ourselves.
     * In the List SingleSelect case, just make the list item show up in
     * TextField.
     */
    if (client == C_ACT)
    {
	cbs.reason = XmCR_COMMAND_ENTERED;
	cbs.event = p->event;

	t = XmTextFieldGetString(w);

	cbs.value = XmStringCreateSimple(t);
	cbs.length = (t == NULL) ? 0 : strlen(t);

	XtFree(t);

	XtCallCallbackList((Widget)cw, cw->command.callback, &cbs);
    }

    if (client == C_LIST_SELECT)
    {
	if (!XmStringGetLtoR(l->item, XmFONTLIST_DEFAULT_TAG, &t))
	{
	    return;
	}

	XmTextFieldSetString(SB_Text(cw), t);

	cbs.value = XmStringCreateSimple(t);

	XtFree(t);

	return;
    }

    if (client == C_LIST_DOUBLE)
    {
	cbs.reason = XmCR_COMMAND_ENTERED;
	cbs.event = l->event;
	cbs.value = l->item;
	cbs.length = XmStringLength(l->item);

	XtCallCallbackList((Widget)cw, cw->command.callback, &cbs);
    }

    /* If we have an error condition, remove it */
    if (Com_Error(cw))
    {
	Com_Error(cw) = False;
	XmListDeletePos(SB_List(cw), 0);
    }

    /* We need to blindly insert now.  */
    XmListAddItemUnselected(SB_List(cw), cbs.value, 0);  /* 0 is at the end */

    /*
     * If #items in list is larger than HistoryMaxItems, remove the first
     * This code will actually delete more than just the first item, in case
     * some sick programmer decides to manually cram more items in the list
     * than XmNhistoryMaxItems allows
     */
    if (List_ItemCount(SB_List(cw)) > Com_HistoryMaxItems(cw))
    {
	DEBUGOUT(_LtDebug(__FILE__, (Widget)cw,
			  "_XmCommandCallback: List too long; removing %d items"
			  " from list\n",
			  List_ItemCount(SB_List(cw)) -
				Com_HistoryMaxItems(cw)));

	XmListDeleteItemsPos(SB_List(cw),
			     List_ItemCount(SB_List(cw)) -
				Com_HistoryMaxItems(cw), 1);
    }
    if (client != C_LIST_DOUBLE)	/* FIX ME - I think this is right ! */
    {
	XmStringFree(cbs.value);
    }

    /* Clear the text field */
    XmTextFieldSetString(SB_Text(cw), "");
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "Initialize\n"));

    if (Com_PromptString(new_w) == (XmString)XmUNSPECIFIED)
    {
	XmString xms;

	xms = XmStringCreateLocalized(">");
	XtVaSetValues( SB_SelectionLabel(new_w), XmNlabelString, xms, NULL);
	XmStringFree(xms);
	Com_PromptString(new_w) = NULL;
    }
#if 0
    /* rws 2 Oct 1998
       These are just re-named SelectionBox resources so SelectionBox
       will take care of them.
     */
    if (Com_PromptString(new_w) == (XmString)XmUNSPECIFIED)
    {
	Com_PromptString(new_w) = XmStringCreateLocalized(">");
    }
    else if (Com_PromptString(new_w) != NULL)
    {
	Com_PromptString(new_w) = XmStringCopy(Com_PromptString(new_w));
    }

    if (Com_Command(new_w) == (XmString)XmUNSPECIFIED)
    {
	Com_Command(new_w) = XmStringCreateLocalized(">");
    }
    else if (Com_Command(new_w) != NULL)
    {
	Com_Command(new_w) = XmStringCopy(Com_Command(new_w));
    }
#endif

    XtAddCallback(SB_Text(new_w), XmNactivateCallback,
		  _XmCommandCallback, C_ACT);
    XtAddCallback(SB_List(new_w), XmNsingleSelectionCallback,
		  _XmCommandCallback, C_LIST_SELECT);
    XtAddCallback(SB_List(new_w), XmNdefaultActionCallback,
		  _XmCommandCallback, C_LIST_DOUBLE);

    Com_Error(new_w) = False;
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Boolean refresh_needed = False;

    BB_InSetValues(new_w) = True;

#if 0
    /* rws 2 Oct 1998
       These are just re-named SelectionBox resources so SelectionBox
       will take care of them.
     */
    if (Com_PromptString(new_w) != Com_PromptString(old))
    {
	Com_PromptString(new_w) = XmStringCopy(Com_PromptString(new_w));

	XmStringFree(Com_PromptString(old));

	refresh_needed = True;
    }

    if (Com_Command(new_w) != Com_Command(old))
    {
	Com_Command(new_w) = XmStringCopy(Com_Command(new_w));

	XmStringFree(Com_Command(old));

	refresh_needed = True;
    }
#endif

    /* the next 8 lines are required for every BB subclass that uses
     * the GeoMatrix */
    BB_InSetValues(new_w) = False;

    if (refresh_needed && (XtClass(new_w) == xmCommandWidgetClass))
    {
	_XmBulletinBoardSizeUpdate(new_w);

	return False;
    }

    return True;
}

static Boolean
_XmCommandParentProcess(Widget widget, XmParentProcessData data)
{
    return False;
}

void
_XmCommandReturn(Widget wid, XEvent *event,
		 String *params, Cardinal *numParams)
{
}

void
_XmCommandUpOrDown(Widget wid, XEvent *event,
		   String *argv, Cardinal *argc)
{
}

Widget
XmCreateCommand(Widget parent,
		char *name,
		Arg *argList,
		Cardinal argcount)
{
    return XtCreateWidget(name, xmCommandWidgetClass, parent,
			  argList, argcount);
}

Widget
XmCreateCommandDialog(Widget parent, char *name,
		      Arg *arglist, Cardinal argcount)
{
    char *s;
    Widget d;

    s = _XmMakeDialogName(name);

#if 0
    d = XtCreateWidget(s, xmDialogShellWidgetClass, parent, arglist, argcount);
#else
    d = XmCreateDialogShell(parent, s, arglist, argcount);
#endif

    XtFree(s);

    return XtCreateWidget(name, xmCommandWidgetClass, d, arglist, argcount);
}

void
XmCommandSetValue(Widget w, XmString command)
{
    char *t;

    if (SB_Text(w) == NULL)
    {
	return;
    }

    if (!XmStringGetLtoR(command, XmFONTLIST_DEFAULT_TAG, &t))
    {
	return;
    }

    XmTextFieldSetString(SB_Text(w), t);

    XtFree(t);
}

void
XmCommandAppendValue(Widget w, XmString command)
{
}

void
XmCommandError(Widget w, XmString error)
{
    if (Com_Error(w))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "Error Condition detected"));
	XmListDeletePos(SB_List(w), 0);
    }

    XmListAddItemUnselected(SB_List(w), error, 0);

    Com_Error(w) = True;
}

Widget
XmCommandGetChild(Widget w, unsigned char child)
{
    switch (child)
    {
    case XmDIALOG_COMMAND_TEXT:
	return SB_Text(w);

    case XmDIALOG_HISTORY_LIST:
	return SB_List(w);

    case XmDIALOG_PROMPT_LABEL:
	return SB_SelectionLabel(w);
    }

    return NULL;
}

static void
_XmCommandGetHistoryItems(Widget w, int offset, XtArgVal *value)
{
XmStringTable table = NULL;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmCommandGetHistoryItems\n"));
    XtVaGetValues(SB_List(w),
    	XmNitems, &table,
    	NULL);
    *value = (XtArgVal)table;
}

static void
_XmCommandGetHistoryItemCount(Widget w, int offset, XtArgVal *value)
{
int count = 0;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmCommandGetHistoryItemCount\n"));
    XtVaGetValues(SB_List(w),
    	XmNitemCount, &count,
    	NULL);
    *value = (XtArgVal)count;
}

static void
_XmCommandGetVisibleItemCount(Widget w, int offset, XtArgVal *value)
{
int count = 0;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmCommandGetVisibleItemCount\n"));
    XtVaGetValues(SB_List(w),
    	XmNvisibleItemCount, &count,
    	NULL);
    *value = (XtArgVal)count;
}

#if 0
static void 
destroy(Widget w)
{
  XmCommandWidget cw = (XmCommandWidget)w;
    /* rws 2 Oct 1998
       These are just re-named SelectionBox resources so SelectionBox
       will take care of them.
     */
  if (Com_PromptString(cw)) {
    XmStringFree(Com_PromptString(cw));
  }
  if (Com_Command(cw)) {
    XmStringFree(Com_Command(cw));
  }
}
#endif
