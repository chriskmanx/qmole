/**
 *
 * $Id: DropTrans.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002 LessTif Development Team
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

static const char rcsid[] = "$Id: DropTrans.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/DropTransP.h>
#include <Xm/DragCP.h>
#include <XmI/DragDropI.h>
#include <XmI/AtomMgrI.h>
#include <Xm/DisplayP.h>

#include <XmI/DebugUtil.h>

#if 0
static void class_initialize();
#endif

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void select_callback(Widget w, XtPointer cd, Atom *select,
			    Atom *type, XtPointer value,
			    unsigned long *len, int *fmt);

static Widget start_drop_transfer(Widget refWidget,
				  ArgList args, Cardinal argCount);

static void add_drop_transfer(Widget widget,
			      XmDropTransferEntry transfers,
			      Cardinal num_transfers);

#define Offset(field) XtOffsetOf(XmDropTransferRec, dropTransfer.field)
static XtResource resources[] =
{
    {
	XmNdropTransfers, XmCDropTransfers, XmRDropTransfers,
	sizeof(XmDropTransferEntry), Offset(drop_transfers),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNnumDropTransfers, XmCNumDropTransfers, XmRCardinal,
	sizeof(Cardinal), Offset(num_drop_transfers),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNincremental, XmCIncremental, XmRBoolean,
	sizeof(Boolean), Offset(incremental),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNtransferProc, XmCTransferProc, XmRCallbackProc,
	sizeof(XtSelectionCallbackProc), Offset(transfer_callback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtransferStatus, XmCTransferStatus, XmRTransferStatus,
	sizeof(unsigned char), Offset(transfer_status),
	XmRImmediate, (XtPointer)XmTRANSFER_SUCCESS
    }
};

#if 0
static XmBaseClassExtRec _XmDropTransferObjectClassExtRec = {
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
    /* widget_navigable          */ NULL,
    /* focus_change              */ NULL,
    /* wrapper_data              */ NULL
};
#endif

XmDropTransferClassRec xmDropTransferClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &objectClassRec,
        /* class_name            */ "XmDropTransferObject",
	/* widget_size           */ sizeof(XmDropTransferRec),
	/* class_initialize      */ NULL /*class_initialize*/,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* obj1                  */ NULL,
	/* obj2                  */ NULL,
	/* obj3                  */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* obj4                  */ True,
	/* obj5                  */ XtExposeCompressSeries,
	/* obj6                  */ True,
	/* obj7                  */ 0,
	/* destroy               */ destroy,
	/* obj8                  */ NULL,
	/* obj9                  */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* obj10                 */ NULL,
	/* get_values_hook       */ NULL,
	/* obj11                 */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* obj12                 */ NULL,
	/* obj13                 */ NULL,
        /* obj14                 */ NULL,
	/* extension             */ NULL
    },
    /* XmDropTranferObject part */
    {
        /* start_drop_transfer*/ start_drop_transfer,
        /* add_drop_transfer  */ add_drop_transfer,
        /* extension          */ NULL
    }
};


WidgetClass xmDropTransferObjectClass = (WidgetClass)&xmDropTransferClassRec;

#if 0
static void
class_initialize()
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "DropTransfer class initialize\n"));

    _XmDropTransferObjectClassExtRec.record_type = XmQmotif;
}
#endif

static void
class_part_initialize(WidgetClass widget_class)
{
    WidgetClass sc;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "DropTransfer class part initialize\n"));

    _XmFastSubclassInit(widget_class, XmDROP_TRANSFER_BIT);

    sc = widget_class->core_class.superclass;

    if (DTC_StartTransferProc(widget_class) == XmInheritStartTransferProc)
    {
	DTC_StartTransferProc(widget_class) = DTC_StartTransferProc(sc);
    }

    if (DTC_AddTransferProc(widget_class) == XmInheritAddTransferProc)
    {
	DTC_AddTransferProc(widget_class) = DTC_AddTransferProc(sc);
    }
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    XmDropTransferEntry entries;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:initialize(%d) - %i args\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    if (DT_NumDropTransfers(new_w) != 0)
    {

	/* Copy the user specified list */
	DT_NumDropTransferLists(new_w) = 1;

	DT_DropTransferLists(new_w) =
	    (XmDropTransferList)XtMalloc(sizeof(XmDropTransferListRec));

	entries = (XmDropTransferEntry)XtMalloc(DT_NumDropTransfers(new_w) *
						sizeof(XmDropTransferEntryRec));

	memcpy(entries, DT_DropTransfers(new_w),
	      DT_NumDropTransfers(new_w) * sizeof(XmDropTransferEntryRec));

	DT_DropTransferLists(new_w)[0].transfer_list = entries;
	DT_DropTransferLists(new_w)[0].num_transfers =
	    DT_NumDropTransfers(new_w);

	DT_DropTransfers(new_w) = entries;
    }
    else
    {
	DT_DropTransferLists(new_w) = NULL;
	DT_NumDropTransferLists(new_w) = 0;
    }

    DT_MotifDropAtom(new_w) = XmInternAtom(XtDisplay(new_w),
					   _XA_MOTIF_DROP,
					   False);

    DT_CurDropTransferList(new_w) = XmUNSPECIFIED;
    DT_CurXfer(new_w) = XmUNSPECIFIED;

    DT_CurTargets(new_w) = NULL;
    DT_CurClientData(new_w) = NULL;
}

static void
destroy(Widget w)
{
    Widget dc;
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, w, "DropTransfer destroy\n"));

    dc = XmGetDragContext(w, DT_Timestamp(w));

    if (dc && DC_SourceIsExternal(dc))
    {
	XtDestroyWidget(dc);
    }

    for (i = 0; i < DT_NumDropTransferLists(w); i++)
    {
	XtFree((char *)DT_DropTransferLists(w)[i].transfer_list);
    }

    XtFree((char *)DT_DropTransferLists(w));
}

static Boolean
set_values(Widget current, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:set_values(%d) - %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(current), XtY(current),
		      XtWidth(current), XtHeight(current),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    return True;
}

static void
entry_transfer(Widget dt, int which)
{
    XmDropTransferList lst = &DT_DropTransferLists(dt)[which];
    Widget dc = DT_DragContext(dt);
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, dt, "%s:entry_transfer(%d)",
    	__FILE__, __LINE__));

    DT_CurDropTransferList(dt) = which;
    DT_CurTargets(dt) = (Atom *)XtMalloc(lst->num_transfers * sizeof(Atom));
    DT_CurClientData(dt) = (XtPointer *)XtMalloc(lst->num_transfers *
						 sizeof(XtPointer));

    for (i = 0; i < lst->num_transfers; i++)
    {
	DT_CurTargets(dt)[i] = lst->transfer_list[i].target;
	DT_CurClientData(dt)[i] = (XtPointer)dt;
    }

    DT_CurXfer(dt) = 0;

    if (DT_Incremental(dt))
    {
	XtGetSelectionValuesIncremental(DC_CurrReceiverInfo(dc)->shell,
					DC_ICCHandle(dc),
					DT_CurTargets(dt),
					lst->num_transfers, select_callback,
					(XtPointer)DT_CurClientData(dt),
					DT_Timestamp(dt));
    }
    else
    {
	XtGetSelectionValues(DC_CurrReceiverInfo(dc)->shell,
			     DC_ICCHandle(dc),
			     DT_CurTargets(dt),
			     lst->num_transfers, select_callback,
			     (XtPointer)DT_CurClientData(dt),
			     DT_Timestamp(dt));
    }
}

static void
notified_callback(Widget w, XtPointer cd, Atom *select, Atom *type,
		  XtPointer value, unsigned long *len, int *fmt)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:notified_callback(%d)\n",
    	__FILE__, __LINE__));

    if (value != NULL)
    {
	XtFree((char *)value);
    }

    XtDestroyWidget((Widget)cd);
}

static void
terminate_transfer(Widget dt, Atom *select)
{
    Atom how;

    DEBUGOUT(_LtDebug(__FILE__, dt, "%s:terminate_transfer(%d) - drag context %s receiver info %s %s\n",
    	__FILE__, __LINE__,
    	DT_DragContext(dt) ? "Yes" : "No",
    	DC_CurrReceiverInfo(DT_DragContext(dt)) ?  "Yes" : "No",
    	XGetAtomName(XtDisplay(dt), *select)
    	));

    if (DT_TransferStatus(dt) == XmTRANSFER_SUCCESS)
    {
	how = XmInternAtom(XtDisplay(dt), _XA_XmTRANSFER_SUCCESS, False);
    }
    else
    {
	how = XmInternAtom(XtDisplay(dt), _XA_XmTRANSFER_FAILURE, False);
    }

    if (DT_DragContext(dt) && DC_CurrReceiverInfo(DT_DragContext(dt)) && DC_CurrReceiverInfo(DT_DragContext(dt))->shell)
    {
	XtGetSelectionValue(DC_CurrReceiverInfo(DT_DragContext(dt))->shell,
			*select, how, notified_callback, (XtPointer)dt,
			DT_Timestamp(dt));
    }
}

static void
select_callback(Widget w, XtPointer cd, Atom *select, Atom *type,
		XtPointer value, unsigned long *len, int *fmt)
{
    Widget dt = (Widget)cd;
    XmDropTransferList lst;

    DEBUGOUT(_LtDebug(__FILE__, dt, "%s:select_callback(%d)",
    	__FILE__, __LINE__));

    lst = &DT_DropTransferLists(dt)[DT_CurDropTransferList(dt)];

    if (DT_TransferCallback(dt))
    {
	(*DT_TransferCallback(dt)) (dt,
				lst->transfer_list[DT_CurXfer(dt)].client_data,
				    select, type, value, len, fmt);
    }

    lst = &DT_DropTransferLists(dt)[DT_CurDropTransferList(dt)];

    if (!DT_Incremental(dt) ||
	(DT_Incremental(dt) && value != NULL && *len == 0))
    {

	DT_CurXfer(dt)++;

	if (DT_CurXfer(dt) == lst->num_transfers)
	{

	    XtFree((char *)DT_CurTargets(dt));
	    XtFree((char *)DT_CurClientData(dt));

	    DT_CurDropTransferList(dt)++;

	    if (DT_CurDropTransferList(dt) < DT_NumDropTransferLists(dt))
	    {
		entry_transfer(dt, DT_CurDropTransferList(dt));
	    }
	    else
	    {
		terminate_transfer(dt, select);
	    }
	}
    }
}

static void
drop_timer(XtPointer cd, XtIntervalId *id)
{
    Widget dt = (Widget)cd;
    Atom select;

    DEBUGOUT(_LtDebug(__FILE__, dt, "%s:drop_timer(%d)\n",
    	__FILE__, __LINE__));

    if (DT_NumDropTransferLists(dt))
    {
	entry_transfer(dt, 0);
    }
    else
    {
	select = DC_ICCHandle(DT_DragContext(dt));

	terminate_transfer(dt, &select);
    }
}

static Widget
start_drop_transfer(Widget refWidget,
		    ArgList args, Cardinal argCount)
{
    Widget disp = XmGetXmDisplay(XtDisplay(refWidget)), dt;

    DEBUGOUT(_LtDebug(__FILE__, refWidget, "%s:start_drop_transfer(%d)\n",
    	__FILE__, __LINE__));

    dt = XtCreateWidget("drop_transfer",
			Display_DropTransferClass(disp),
			disp, args, argCount);

    /* refWidget had BETTER be a DC */
    DT_DragContext(dt) = refWidget;
    DT_Timestamp(dt) = DC_DragFinishTime(refWidget);

    XtAppAddTimeOut(XtWidgetToApplicationContext(dt), 0,
		    drop_timer, (XtPointer)dt);
    return dt;
}

static void
add_drop_transfer(Widget widget,
		  XmDropTransferEntry transfers,
		  Cardinal num_transfers)
{
    XmDropTransferEntry entries;
    int idx;

    DEBUGOUT(_LtDebug(__FILE__, widget, "%s:add_drop_transfer(%d)",
    	__FILE__, __LINE__));

    idx = DT_NumDropTransferLists(widget);
    DT_NumDropTransferLists(widget)++;

    DT_DropTransferLists(widget) =
	(XmDropTransferList)XtRealloc((char *)DT_DropTransferLists(widget),
				      DT_NumDropTransferLists(widget) *
				      sizeof(XmDropTransferListRec));

    entries = (XmDropTransferEntry)XtMalloc(num_transfers *
					    sizeof(XmDropTransferEntryRec));
    memcpy(entries, transfers, num_transfers * sizeof(XmDropTransferEntryRec));

    DT_DropTransferLists(widget)[idx].transfer_list = entries;
    DT_DropTransferLists(widget)[idx].num_transfers = num_transfers;
}

void
XmDropTransferAdd(Widget drop_transfer,
		  XmDropTransferEntryRec *transfers,
		  Cardinal num_transfers)
{
    DEBUGOUT(_LtDebug(__FILE__, drop_transfer, "%s:XmDropTransferAdd(%d)",
    	__FILE__, __LINE__));

    DTC_AddTransferProc(XtClass(drop_transfer)) (drop_transfer,
						 transfers,
						 num_transfers);
}

Widget
XmDropTransferStart(Widget widget,
		    ArgList arglist,
		    Cardinal argcount)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget));

    DEBUGOUT(_LtDebug(__FILE__, widget, "%s:XmDropTransferStart(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, widget, arglist, argcount, False));


    return DTC_StartTransferProc(Display_DropTransferClass(disp)) (widget,
								   arglist,
								   argcount);
}
