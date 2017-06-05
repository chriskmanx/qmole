/**
 *
 * $Id: DragC.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $
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

/* This code owes a lot to Daniel Dardailler, who wrote up an
 * initial implementation of the dynamic drag protocol for an X/Open
 * contract while still with the X consortium.
 *
 * Here's his copyright notice:
 *
 * Copyright 1996 Daniel Dardailler.  
 * Permission to use, copy, modify, distribute, and sell this software
 * for any purpose is hereby granted without fee, provided that the above
 * copyright notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting documentation,
 * and that the name of Daniel Dardailler not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Daniel Dardailler makes no representations
 * about the suitability of this software for any purpose.  It is
 * provided "as is" without express or implied warranty.
 */

static const char rcsid[] = "$Id: DragC.c,v 1.1 2004/08/28 19:22:43 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/DragCP.h>
#include <Xm/DropSMgrP.h>
#include <XmI/DragDropI.h>
#include <XmI/AtomMgrI.h>
#include <Xm/DisplayP.h>
#include <Xm/AtomMgr.h>
#include <Xm/MenuUtilP.h>	/* for the _XmGrab stuff */

#include <XmI/DebugUtil.h>


/* Forward Declarations */

static void class_initialize(void);

static void class_part_initialize(WidgetClass w_class);

static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);

static void destroy(Widget w);

static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);

static void _XmDCStartDrag(XmDragContext dc,
			   Widget srcW,
			   XEvent *event);

static void _XmDCCancelDrag(XmDragContext dc);

static void internal_notify(Widget w, XtPointer cd, XtPointer cbs);

static void external_notify(Widget w, XtPointer cd, XtPointer cbs);

/*
 * Resources for the dragContext class
 */
#define Offset(field) XtOffsetOf(XmDragContextRec, drag.field)
static XtResource resources[] =
{
    {
	XmNsourceWidget, XmCSourceWidget, XmRWidget,
	sizeof(Widget), Offset(sourceWidget),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNexportTargets, XmCExportTargets, XmRAtomList,
	sizeof(Atom *), Offset(exportTargets),
	XmRImmediate, NULL
    },
    {
	XmNnumExportTargets, XmCNumExportTargets, XmRInt,
	sizeof(Cardinal), Offset(numExportTargets),
	XmRImmediate, (XtPointer)0
    },
    {
	XmNconvertProc, XmCConvertProc, XmRFunction,
	sizeof(XmConvertSelectionRec), Offset(convertProc),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNclientData, XmCClientData, XtRPointer,
	sizeof(XtPointer), Offset(clientData),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNincremental, XmCIncremental, XmRBoolean,
	sizeof(Boolean), Offset(incremental),
	XmRImmediate, (XtPointer)False
    },
    {
	XmNdragOperations, XmCDragOperations, XmRUnsignedChar,
	sizeof(unsigned char), Offset(dragOperations),
	XmRImmediate, (XtPointer)(XmDROP_COPY | XmDROP_MOVE)
    },
    {
	XmNsourceCursorIcon, XmCSourceCursorIcon, XmRWidget,
	sizeof(Widget), Offset(sourceCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNsourcePixmapIcon, XmCSourcePixmapIcon, XmRWidget,
	sizeof(Widget), Offset(sourcePixmapIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNstateCursorIcon, XmCStateCursorIcon, XmRWidget,
	sizeof(Widget), Offset(stateCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNoperationCursorIcon, XmCOperationCursorIcon, XmRWidget,
	sizeof(Widget), Offset(operationCursorIcon),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNcursorBackground, XmCCursorBackground, XmRPixel,
	sizeof(Pixel), Offset(cursorBackground),
	XmRCallProc, (XtPointer)_XmBackgroundColorDefault
    },
    {
	XmNcursorForeground, XmCCursorForeground, XmRPixel,
	sizeof(Pixel), Offset(cursorForeground),
	XmRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {
	XmNvalidCursorForeground, XmCValidCursorForeground, XmRPixel,
	sizeof(Pixel), Offset(validCursorForeground),
	XtRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {
	XmNinvalidCursorForeground, XmCInvalidCursorForeground, XmRPixel,
	sizeof(Pixel), Offset(invalidCursorForeground),
	XmRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {
	XmNnoneCursorForeground, XmCNoneCursorForeground, XmRPixel,
	sizeof(Pixel), Offset(noneCursorForeground),
	XmRCallProc, (XtPointer)_XmForegroundColorDefault
    },
    {
	XmNdropSiteEnterCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(siteEnterCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdropSiteLeaveCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(siteLeaveCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtopLevelEnterCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(topLevelEnterCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdragMotionCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(dragMotionCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtopLevelLeaveCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(topLevelLeaveCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdropStartCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(dropStartCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdragDropFinishCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(dragDropFinishCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNdropFinishCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(dropFinishCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNoperationChangedCallback, XmCCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(operationChangedCallback),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNblendModel, XmCBlendModel, XmRBlendModel,
	sizeof(unsigned char), Offset(blendModel),
	XtRImmediate, (XtPointer)XmBLEND_ALL
    },
    {
	XmNsourceIsExternal, XmCSourceIsExternal, XmRBoolean,
	sizeof(Boolean), Offset(sourceIsExternal),
	XtRImmediate, (XtPointer)False
    },
    {
	XmNsourceWindow, XmCSourceWindow, XmRWindow,
	sizeof(Window), Offset(srcWindow),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNstartTime, XmCStartTime, XmRInt,
	sizeof(Time), Offset(dragStartTime),
	XtRImmediate, (XtPointer)0
    },
    {
	XmNiccHandle, XmCICCHandle, XmRAtom,
	sizeof(Atom), Offset(iccHandle),
	XtRImmediate, (XtPointer)0
    }
};

static void _XmDCCancel(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void _XmDCMotion(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void _XmDCFinish(Widget w, XEvent *event,
			String *params, Cardinal *num_params);

static void _XmDCHelp(Widget w, XEvent *event,
		      String *params, Cardinal *num_params);

static void _XmDCIgnore(Widget w, XEvent *event,
			String *params, Cardinal *num_params);


static char _XmDCTranslations[] =
    "<Btn2Up>:             FinishDrag()\n\
    <BtnUp>:              IgnoreButtons()\n\
    <BtnDown>:            IgnoreButtons()\n\
    <Key>osfCancel:       CancelDrag()\n\
    <Key>osfHelp:         HelpDrag()\n\
    Button2<Enter>:       DragMotion()\n\
    Button2<Leave>:       DragMotion()\n\
    Button2<Motion>:      DragMotion()";

static XtActionsRec actions[] =
{
    {"FinishDrag", _XmDCFinish},
    {"CancelDrag", _XmDCCancel},
    {"HelpDrag", _XmDCHelp},
    {"DragMotion", _XmDCMotion},
    {"IgnoreButtons", _XmDCIgnore}
};


#if 0
static XmBaseClassExtRec _XmDragCCoreClassExtRec = {
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

XmDragContextClassRec xmDragContextClassRec = {
    /* Core class part */
    {
	/* superclass            */ (WidgetClass) &coreClassRec,
        /* class_name            */ "XmDragContext",
	/* widget_size           */ sizeof(XmDragContextRec),
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
	/* compress_motion       */ False /*True*/,
	/* compress_exposure     */ XtExposeNoCompress /*XtExposeCompressMaximal*/,
	/* compress_enterleave   */ False /*True*/,
	/* visible_interest      */ False,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ set_values,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL /*XtInheritSetValuesAlmost*/,
	/* get_values_hook       */ NULL,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ _XmDCTranslations,
	/* query_geometry        */ NULL,
	/* display_accelerator   */ NULL /*XtInheritDisplayAccelerator*/,
	/* extension             */ (XtPointer)NULL /*&_XmDragCCoreClassExtRec*/
    },
    /* DragContext Class part */
    {
        /* start     */ _XmDCStartDrag,
        /* cancel    */ _XmDCCancelDrag,
        /* extension */ NULL,
    }
};


WidgetClass xmDragContextClass = (WidgetClass)&xmDragContextClassRec;

static void
class_initialize(void)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:class_initialize(%d)\n",
    	__FILE__, __LINE__));
#if 0
    _XmDragCCoreClassExtRec.record_type = XmQmotif;
#endif
}

static void
class_part_initialize(WidgetClass widget_class)
{
    WidgetClass sc;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:class_part_initialize(%d)\n",
    	__FILE__, __LINE__));
    _XmFastSubclassInit(widget_class, XmDRAG_CONTEXT_BIT);

    sc = widget_class->core_class.superclass;

    if (DCC_DragStartProc(widget_class) == XmInheritDragStartProc)
    {
	DCC_DragStartProc(widget_class) = DCC_DragStartProc(sc);
    }

    if (DCC_DragCancelProc(widget_class) == XmInheritDragCancelProc)
    {
	DCC_DragCancelProc(widget_class) = DCC_DragCancelProc(sc);
    }
}

static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Atom *exports;
    XmDropSiteManagerObject dsm;
    Arg argl[2];

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

    DC_InDropSite(new_w) = False;
    DC_DragTimerId(new_w) = None;

    DC_RoundOffTime(new_w) = 32;
    DC_DragFinishTime(new_w) = CurrentTime;	/* read 0 */
    DC_DropFinishTime(new_w) = CurrentTime;

    /* copy from resource */
    DC_ActiveBlendModel(new_w) = DC_BlendModel(new_w);

    DC_StartX(new_w) = DC_StartY(new_w) = 0;

    /* no cursor yet */
    DC_CurDragOver(new_w) = NULL;
    DC_OrigDragOver(new_w) = NULL;

    /* setup the notify cb */
#if 0
    dsm = _XmGetDropSiteManagerObject(_XmDCtoDD(new_w));
#else
    dsm = _XmGetDropSiteManagerObject((XmDisplay)XmGetXmDisplay(XtDisplay(new_w)));
#endif
    XtSetArg(argl[0], XmNclientData, (XtPointer)new_w);
    if (DC_SourceIsExternal(new_w))
    {
	XtSetArg(argl[1], XmNnotifyProc, external_notify);
    }
    else
    {
	XtSetArg(argl[1], XmNnotifyProc, internal_notify);
    }
    XtSetValues((Widget)dsm, argl, 2);

    /* copy the export targets */
    if (DC_ExportTargets(new_w))
    {
	exports = (Atom *)XtMalloc(DC_NumExportTargets(new_w) * sizeof(Atom));

	memcpy(exports, DC_ExportTargets(new_w),
	      DC_NumExportTargets(new_w) * sizeof(Atom));

	DC_ExportTargets(new_w) = exports;
    }

    XtWidth(new_w) = XtHeight(new_w) = 32;
    XtX(new_w) = XtY(new_w) = 0;

    /* Setup the receiver info to known state */
    DC_CurrReceiverInfo(new_w) = NULL;
    DC_ReceiverInfos(new_w) = NULL;
    DC_NumReceiverInfos(new_w) = 0;
    DC_MaxReceiverInfos(new_w) = 0;

    if (XtIsRealized(XtParent(new_w)))
    {
	XtRealizeWidget(new_w);
    }

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "Initialize: Parent %s window 0x%X us 0x%X\n",
		      XtName(XtParent(new_w)), XtWindow(XtParent(new_w)),
		      XtWindow(new_w)));
}

static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:destroy(%d)\n",
    	__FILE__, __LINE__));

    if (DC_ExportTargets(w))
    {
	XtFree((char *)DC_ExportTargets(w));
    }

    if (DC_ReceiverInfos(w) != NULL)
    {
	XtFree((char *)DC_ReceiverInfos(w));
    }

    /* if you want to find really weird SIGSEGV's, leave this out */
    if (DC_DragTimerId(w))
    {
	XtRemoveTimeOut(DC_DragTimerId(w));
	DC_DragTimerId(w) = None;
    }
}

static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    Atom *exports;

    DEBUGOUT(_LtDebug(__FILE__, new_w,
		      "%s:set_values(%d) - %i args\n"
		      "\t    old X %5i Y %5i W %5i H %5i\n"
		      "\trequest X %5i Y %5i W %5i H %5i\n"
		      "\t  new_w X %5i Y %5i W %5i H %5i\n",
		      __FILE__, __LINE__,
		      *num_args,
		      XtX(old), XtY(old),
		      XtWidth(old), XtHeight(old),
		      XtX(request), XtY(request),
		      XtWidth(request), XtHeight(request),
		      XtX(new_w), XtY(new_w),
		      XtWidth(new_w), XtHeight(new_w)));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, new_w, args, *num_args, False));

    if (DC_ExportTargets(old) != DC_ExportTargets(new_w))
    {
	if (DC_ExportTargets(old))
	{
	    XtFree((char *)DC_ExportTargets(old));
	}

	exports = (Atom *)XtMalloc(DC_NumExportTargets(new_w) * sizeof(Atom));

	memcpy(exports, DC_ExportTargets(new_w),
	      DC_NumExportTargets(new_w) * sizeof(Atom));

	DC_ExportTargets(new_w) = exports;
    }

    return False;
}

static void
send_drag_message(Widget dc, Window win, XmDragDropCallbackStruct * cbs,
		  int type)
{
    XClientMessageEvent cmsg;
    XmDndMessage *dnd_message = (XmDndMessage *) & cmsg.data.b[0];
    Atom msg;

    DEBUGOUT(_LtDebug(__FILE__, dc, "%s:send_drag_message(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSOURCE", dc, "%s:send_drag_message(%d) - %s to %p\n",
    	__FILE__, __LINE__,
    	_LtDebugDragAndDropMessageType2String(cbs->any.reason), win));
    DEBUGOUT(_LtDebug("DRAGSINK", dc, "%s:send_drag_message(%d) - %s to %p\n",
    	__FILE__, __LINE__,
    	_LtDebugDragAndDropMessageType2String(cbs->any.reason), win));

    msg = XmInternAtom(XtDisplay(dc), _XA_MOTIF_DRAG_AND_DROP_MESSAGE, False);

    cmsg.type = ClientMessage;
    cmsg.serial = LastKnownRequestProcessed(XtDisplay(dc));
    cmsg.send_event = True;
    cmsg.display = XtDisplay(dc);
    cmsg.window = win;
    cmsg.format = 8;
    cmsg.message_type = msg;

    dnd_message->reason = cbs->any.reason;

    dnd_message->byte_order = _XmByteOrder();

    /* we're filling in flags with more stuff that necessary,
       depending on the reason, but it doesn't matter */
    /* MLM: what he's not telling you is that normally the data that gets
     * sent is identical in content to the callback structs... */
    dnd_message->flags = 0;
    dnd_message->flags |= DND_SET_STATUS(0);
    dnd_message->flags |= DND_SET_OPERATION(DC_Operation(dc));
    dnd_message->flags |= DND_SET_OPERATIONS(DC_Operations(dc));
    dnd_message->flags |= DND_SET_COMPLETION(DC_DragCompletionStatus(dc));
    dnd_message->flags |= DND_SET_EVENT_TYPE(type);

    dnd_message->time = DC_LastChangeTime(dc);

    switch (cbs->any.reason)
    {
    case XmDROP_SITE_LEAVE:
	break;

    case XmTOP_LEVEL_ENTER:
    case XmTOP_LEVEL_LEAVE:
	dnd_message->data.top.src_window = DC_SrcWindow(dc);
	dnd_message->data.top.property = DC_ICCHandle(dc);
	break;	/* cannot fall thru since the byte layout is different in
		 * both set of messages, see top and pot union stuff */

    case XmDRAG_MOTION:
    case XmOPERATION_CHANGED:
    case XmDROP_SITE_ENTER:
    case XmDROP_START:
	dnd_message->data.pot.x = XtX(dc);	/* mouse position */
	dnd_message->data.pot.y = XtY(dc);
	dnd_message->data.pot.src_window = DC_SrcWindow(dc);
	dnd_message->data.pot.property = DC_ICCHandle(dc);
	break;

    default:
	break;
    }

    /* need to send the event */
    XSendEvent(XtDisplay(dc), win, False, NoEventMask, (XEvent *)&cmsg);
}

static Boolean
receive_drag_message(Widget dc, XEvent *event, XmDragDropCallbackStruct * cbs)
{
    XClientMessageEvent *cm = (XClientMessageEvent *)event;
    XmDndMessage *dnd_message = (XmDndMessage *) & cm->data.b[0];
    Atom msg;

    DEBUGOUT(_LtDebug(__FILE__, dc, "%s:receive_drag_message(%d)\n",
    	__FILE__, __LINE__));

    msg = XmInternAtom(XtDisplay(dc), _XA_MOTIF_DRAG_AND_DROP_MESSAGE, False);

    DEBUGOUT(_LtDebug("DRAGSOURCE", dc, "%s:receive_drag_message(%d) - %s %i %s\n",
    	__FILE__, __LINE__,
    	_LtDebugDragAndDropMessageType2String(dnd_message->reason & DND_CLEAR_EVENT_TYPE), dnd_message->reason & DND_CLEAR_EVENT_TYPE,
    	cm->message_type == msg ? "Valid DnD message" : "Invalid DnD message"));

    if (cm->message_type != msg)
    {
	_XmWarning(dc, "Invalid Drag/Drop message.\n");

	return False;
    }

    if (dnd_message->byte_order != _XmByteOrder())
    {
	SWAP2BYTES(dnd_message->flags);
	SWAP4BYTES(dnd_message->time);
    }

    cbs->any.reason = dnd_message->reason;

    cbs->any.reason &= DND_CLEAR_EVENT_TYPE;

    cbs->any.timeStamp = dnd_message->time;

    /* FIX ME: we should check the INITIATOR/RECEIVER flag in here */
    switch (cbs->any.reason)
    {
    case XmTOP_LEVEL_ENTER:
	if (dnd_message->byte_order != _XmByteOrder())
	{
	    SWAP4BYTES(dnd_message->data.top.src_window);
	    SWAP4BYTES(dnd_message->data.top.property);
	}
	cbs->tle.window = dnd_message->data.top.src_window;
	cbs->tle.iccHandle = dnd_message->data.top.property;
	break;

    case XmTOP_LEVEL_LEAVE:
	if (dnd_message->byte_order != _XmByteOrder())
	{
	    SWAP4BYTES(dnd_message->data.top.src_window);
	}
	cbs->tle.window = dnd_message->data.top.src_window;
	break;

    case XmDRAG_MOTION:
	cbs->dm.operation = DND_GET_OPERATION(dnd_message->flags);
	cbs->dm.operations = DND_GET_OPERATIONS(dnd_message->flags);
	cbs->dm.dropSiteStatus = DND_GET_STATUS(dnd_message->flags);
	if (dnd_message->byte_order != _XmByteOrder())
	{
	    SWAP2BYTES(dnd_message->data.pot.x);
	    SWAP2BYTES(dnd_message->data.pot.y);
	}
	cbs->dm.x = dnd_message->data.pot.x;
	cbs->dm.y = dnd_message->data.pot.y;
	break;

    case XmOPERATION_CHANGED:
	cbs->oc.operation = DND_GET_OPERATION(dnd_message->flags);
	cbs->oc.operations = DND_GET_OPERATIONS(dnd_message->flags);
	cbs->oc.dropSiteStatus = DND_GET_STATUS(dnd_message->flags);
	break;

    case XmDROP_SITE_ENTER:
	cbs->dse.operation = DND_GET_OPERATION(dnd_message->flags);
	cbs->dse.operations = DND_GET_OPERATIONS(dnd_message->flags);
	cbs->dse.dropSiteStatus = DND_GET_STATUS(dnd_message->flags);
	if (dnd_message->byte_order != _XmByteOrder())
	{
	    SWAP2BYTES(dnd_message->data.pot.x);
	    SWAP2BYTES(dnd_message->data.pot.y);
	}
	cbs->dse.x = dnd_message->data.pot.x;
	cbs->dse.y = dnd_message->data.pot.y;
	break;

    case XmDROP_SITE_LEAVE:
	break;

    case XmDROP_START:
	cbs->ds.operation = DND_GET_OPERATION(dnd_message->flags);
	cbs->ds.operations = DND_GET_OPERATIONS(dnd_message->flags);
	cbs->ds.dropSiteStatus = DND_GET_STATUS(dnd_message->flags);
	cbs->ds.dropAction = DND_GET_COMPLETION(dnd_message->flags);

	if (dnd_message->byte_order != _XmByteOrder())
	{
	    SWAP2BYTES(dnd_message->data.pot.x);
	    SWAP2BYTES(dnd_message->data.pot.y);
	    SWAP4BYTES(dnd_message->data.pot.property);
	    SWAP4BYTES(dnd_message->data.pot.src_window);
	}
	cbs->ds.x = dnd_message->data.pot.x;
	cbs->ds.y = dnd_message->data.pot.y;
	cbs->ds.iccHandle = dnd_message->data.pot.property;
	cbs->ds.window = dnd_message->data.pot.src_window;
	break;

    default:
	_XmWarning(dc,
		   "We got something, but it wasn't a good drag message.\n");
	break;
    }

    return True;
}

static void
maybe_send_drag_message(Widget dc, Window win, int reason)
{
    XmDragDropCallbackStruct cbs;
    Widget disp = XmGetXmDisplay(XtDisplay(dc));
    XmDragTopLevelClientDataStruct tld;
    XmDragMotionClientDataStruct md;

    DEBUGOUT(_LtDebug(__FILE__, dc, "%s:maybe_send_drag_message(%d)\n",
    	__FILE__, __LINE__));

    if (DC_ActiveProtocolStyle(dc) == XmDRAG_DROP_ONLY)
    {
	if (reason == XmTOP_LEVEL_ENTER)
	{
	    _XmDragOverChange((Widget)DC_CurDragOver(dc), XmVALID_DROP_SITE);
	    return;
	}
	else if (reason == XmTOP_LEVEL_LEAVE)
	{
	    _XmDragOverChange((Widget)DC_CurDragOver(dc), XmINVALID_DROP_SITE);
	    return;
	}
    }

    if (DC_ActiveProtocolStyle(dc) == XmDRAG_NONE)
    {
	return;
    }

    if (DC_ActiveProtocolStyle(dc) == XmDRAG_DROP_ONLY &&
	reason != XmDROP_START)
    {
	return;
    }

    cbs.any.reason = reason;
    cbs.any.event = NULL;
    switch (reason)
    {
    case XmTOP_LEVEL_ENTER:
	cbs.tle.timeStamp = DC_LastChangeTime(dc);
	cbs.tle.screen = DC_CurrScreen(dc);
	cbs.tle.window = DC_SrcWindow(dc);
	cbs.tle.x = DC_CurrReceiverInfo(dc)->xOrigin;
	cbs.tle.x = DC_CurrReceiverInfo(dc)->yOrigin;
	cbs.tle.dragProtocolStyle = DC_ActiveProtocolStyle(dc);
	cbs.tle.iccHandle = DC_ICCHandle(dc);
	break;

    case XmTOP_LEVEL_LEAVE:
	cbs.tll.timeStamp = DC_LastChangeTime(dc);
	cbs.tll.screen = DC_CurrScreen(dc);
	cbs.tll.window = DC_SrcWindow(dc);
	break;

    case XmDRAG_MOTION:
	cbs.dm.timeStamp = DC_LastChangeTime(dc);
	cbs.dm.operation = DC_Operation(dc);
	cbs.dm.operations = DC_Operations(dc);
	cbs.dm.dropSiteStatus = XmNO_DROP_SITE;
	cbs.dm.x = XtX(dc);
	cbs.dm.y = XtY(dc);
	break;

    case XmDROP_START:
	cbs.ds.timeStamp = DC_DragFinishTime(dc);
	cbs.ds.operation = DC_Operation(dc);
	cbs.ds.operations = DC_Operations(dc);
	cbs.ds.dropAction = DC_DragCompletionStatus(dc);
	cbs.ds.x = XtX(dc);
	cbs.ds.y = XtY(dc);
	cbs.ds.window = XtWindow(DC_SrcShell(dc));
	cbs.ds.iccHandle = DC_ICCHandle(dc);
	break;

    case XmOPERATION_CHANGED:
	break;

    case XmDROP_SITE_ENTER:
    case XmDROP_SITE_LEAVE:
    case XmDROP_FINISH:
    case XmDRAG_DROP_FINISH:
	break;

    default:
	break;
    }

    /* if we're not in the same client */
    if (DC_CurrReceiverInfo(dc)->shell == NULL &&
	!DC_SourceIsExternal(dc) &&
	(DC_ActiveProtocolStyle(dc) == XmDRAG_DYNAMIC ||
	 reason == XmDROP_START))
    {

	send_drag_message(dc, win, &cbs, XmINITIATOR);
	return;
    }
    else if (reason == XmTOP_LEVEL_ENTER ||
	     reason == XmTOP_LEVEL_LEAVE ||
	     reason == XmDROP_START)
    {

	tld.sourceIsExternal = DC_SourceIsExternal(dc);
	tld.dos = DC_CurDragOver(dc);
	tld.window = DC_CurrReceiverInfo(dc)->window;
	tld.shell = DC_CurrReceiverInfo(dc)->shell;
	tld.xOrigin = DC_CurrReceiverInfo(dc)->xOrigin;
	tld.yOrigin = DC_CurrReceiverInfo(dc)->yOrigin;
	tld.width = DC_CurrReceiverInfo(dc)->width;
	tld.height = DC_CurrReceiverInfo(dc)->height;
	tld.iccInfo = DC_CurrReceiverInfo(dc)->iccInfo;

	_XmDSMUpdate(_XmGetDropSiteManagerObject((XmDisplay)disp),
		     (XtPointer)&tld,
		     (XtPointer)&cbs);
    }
    else if (reason == XmDRAG_MOTION ||
	     reason == XmOPERATION_CHANGED)
    {

	md.dos = DC_CurDragOver(dc);
	md.window = DC_CurrReceiverInfo(dc)->window;

	_XmDSMUpdate(_XmGetDropSiteManagerObject((XmDisplay)disp),
		     (XtPointer)&md,
		     (XtPointer)&cbs);
    }
    else
    {
	_XmDSMUpdate(_XmGetDropSiteManagerObject((XmDisplay)disp),
		     (XtPointer)NULL,
		     (XtPointer)&cbs);
    }
}

static void
invoke_initiator_callback(Widget dc, int cr_reason)
{
    XmDragDropCallbackStruct cbs;
    XtCallbackList cb = NULL;

    DEBUGOUT(_LtDebug(__FILE__, dc, "%s:invoke_initiator_callback(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSOURCE", dc, "%s:invoke_initiator_callback(%d) %s\n",
    	__FILE__, __LINE__,
    	_LtDebugDragAndDropMessageType2String(cr_reason)));

    switch (cr_reason)
    {
    case XmCR_TOP_LEVEL_ENTER:
	if (DC_TopLevelEnterCallback(dc) == NULL)
	{
	    return;
	}
	cb = DC_TopLevelEnterCallback(dc);

	cbs.tle.reason = XmCR_TOP_LEVEL_ENTER;
	cbs.tle.event = NULL;
	cbs.tle.timeStamp = DC_LastChangeTime(dc);
	cbs.tle.screen = DC_CurrScreen(dc);
	/*
	 * From the Motif includes:
	 * the window field is different if this is an outbound or inbound
	 * callback. Outbound == reciever, Inbound == initiator.
	 * This stuff has gotta come from the receiverInfo
	 */
	cbs.tle.dragProtocolStyle = DC_ActiveProtocolStyle(dc);
	cbs.tle.window = DC_CurrReceiverInfo(dc)->window;
	cbs.tle.x = DC_CurrReceiverInfo(dc)->xOrigin;
	cbs.tle.y = DC_CurrReceiverInfo(dc)->yOrigin;
	cbs.tle.iccHandle = DC_ICCHandle(dc);
	break;

    case XmCR_TOP_LEVEL_LEAVE:
	if (DC_TopLevelLeaveCallback(dc) == NULL)
	{
	    return;
	}
	cb = DC_TopLevelLeaveCallback(dc);

	cbs.tll.reason = XmCR_TOP_LEVEL_LEAVE;
	cbs.tll.event = NULL;
	cbs.tll.timeStamp = DC_LastChangeTime(dc);
	cbs.tll.screen = DC_CurrScreen(dc);
	cbs.tll.window = DC_CurrReceiverInfo(dc)->window;
	break;

    case XmCR_DRAG_MOTION:
	if (DC_DragMotionCallback(dc) == NULL)
	{
	    return;
	}
	cb = DC_DragMotionCallback(dc);

	cbs.dm.reason = XmCR_DRAG_MOTION;
	cbs.dm.event = NULL;
	cbs.dm.timeStamp = DC_LastChangeTime(dc);
	cbs.dm.operation = DC_Operation(dc);
	cbs.dm.operations = DC_Operations(dc);
	if (DC_ActiveProtocolStyle(dc) == XmDRAG_DROP_ONLY)
	    cbs.dm.dropSiteStatus = XmVALID_DROP_SITE;
	else
	    cbs.dm.dropSiteStatus = XmNO_DROP_SITE;
	cbs.dm.x = XtX(dc);
	cbs.dm.y = XtY(dc);
	break;

    case XmCR_DROP_SITE_LEAVE:
	if (DC_SiteLeaveCallback(dc) == NULL)
	{
	    return;
	}
	cb = DC_SiteLeaveCallback(dc);

	cbs.dsl.reason = XmCR_DROP_SITE_LEAVE;
	cbs.dsl.event = NULL;
	cbs.dsl.timeStamp = DC_LastChangeTime(dc);
	break;

    case XmCR_DROP_START:
	if (DC_DropStartCallback(dc) == NULL)
	{
	    return;
	}
	cb = DC_DropStartCallback(dc);

	cbs.ds.reason = XmCR_DROP_START;
	cbs.ds.event = NULL;
	cbs.ds.timeStamp = DC_DragFinishTime(dc);
	cbs.ds.operation = DC_Operation(dc);
	cbs.ds.operations = DC_Operations(dc);
	cbs.ds.x = XtX(dc);
	cbs.ds.y = XtY(dc);
	cbs.ds.iccHandle = DC_ICCHandle(dc);
	cbs.ds.window = XtWindow(DC_SrcShell(dc));
	break;

    case XmCR_DROP_FINISH:
	if (DC_DropFinishCallback(dc) == NULL)
	{
	    return;
	}
	cb = DC_DropFinishCallback(dc);

	cbs.df.reason = XmCR_DROP_FINISH;
	cbs.df.event = NULL;
	cbs.df.timeStamp = DC_DropFinishTime(dc);
	cbs.df.completionStatus = DC_DragDropCompletionStatus(dc);
	cbs.df.operation = DC_Operation(dc);
	break;

    case XmCR_DRAG_DROP_FINISH:
	if (DC_DragDropFinishCallback(dc) == NULL)
	{
	    return;
	}
	cb = DC_DragDropFinishCallback(dc);

	cbs.ddf.reason = XmCR_DRAG_DROP_FINISH;
	cbs.ddf.event = NULL;
	cbs.ddf.timeStamp = DC_DropFinishTime(dc);
	break;

    case XmCR_OPERATION_CHANGED:
	if (DC_OperationChangedCallback(dc) == NULL)
	{
	    return;
	}
	cb = DC_OperationChangedCallback(dc);

	cbs.oc.reason = XmCR_OPERATION_CHANGED;
	cbs.oc.event = NULL;
	cbs.oc.timeStamp = DC_LastChangeTime(dc);
	cbs.oc.operation = DC_Operation(dc);
	cbs.oc.operations = DC_Operations(dc);
	break;

    default:
	break;
    }

    if (!cb)
    {
	return;
    }

    XtCallCallbackList(dc, cb, (XtPointer)&cbs);

    switch (cr_reason)
    {
    case XmCR_DROP_FINISH:
	DC_DragCompletionStatus(dc) = cbs.df.completionStatus;
	break;

    default:
	break;
    }
}

static void
invoke_receiver_callback(Widget dc, XmDragDropCallbackStruct * cbs)
{
    XtCallbackList cb = NULL;
    int state;

    DEBUGOUT(_LtDebug(__FILE__, dc, "%s:invoke_receiver_callback(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSOURCE", dc, "%s:invoke_receiver_callback(%d) - %s\n",
    	__FILE__, __LINE__,
    	_LtDebugDragAndDropMessageType2String(cbs->any.reason)));

    switch (cbs->any.reason)
    {
    case XmCR_DROP_SITE_ENTER:
	cb = DC_SiteEnterCallback(dc);
	break;

    case XmCR_DROP_SITE_LEAVE:
	DC_InDropSite(dc) = False;
	cb = DC_SiteLeaveCallback(dc);
	break;

    case XmCR_DRAG_MOTION:
	cb = DC_DragMotionCallback(dc);
	break;

    case XmCR_OPERATION_CHANGED:
	cb = DC_OperationChangedCallback(dc);
	break;

    case XmCR_DROP_START:
	if (DC_DragTimerId(dc))
	{
	    XtRemoveTimeOut(DC_DragTimerId(dc));
	    DC_DragTimerId(dc) = None;
	}
	cb = DC_DropStartCallback(dc);
	break;

    default:
	break;
    }

    if (cb)
    {
	XtCallCallbackList(dc, cb, cbs);
    }

    switch (cbs->any.reason)
    {
    case XmCR_DROP_SITE_ENTER:
	DC_Operation(dc) = cbs->dse.operation;
	DC_Operations(dc) = cbs->dse.operations;
	DC_InDropSite(dc) = True;
	_XmDragOverChange((Widget)DC_CurDragOver(dc), cbs->dse.dropSiteStatus);
	break;

    case XmCR_DROP_SITE_LEAVE:

	/* reset drag ops */
	DC_Operations(dc) = DC_DragOperations(dc);

	state = DC_LastEventState(dc);

	if (state & ShiftMask && state & ControlMask)
	{
	    DC_Operation(dc) = DC_DragOperations(dc) & XmDROP_LINK;
	    DC_Operations(dc) = DC_DragOperations(dc) & XmDROP_LINK;
	}
	else if (state & ShiftMask)
	{
	    DC_Operation(dc) = DC_DragOperations(dc) & XmDROP_MOVE;
	    DC_Operations(dc) = DC_DragOperations(dc) & XmDROP_MOVE;
	}
	else if (state & ControlMask)
	{
	    DC_Operation(dc) = DC_DragOperations(dc) & XmDROP_COPY;
	    DC_Operations(dc) = DC_DragOperations(dc) & XmDROP_COPY;
	}
	else if (DC_DragOperations(dc) & XmDROP_LINK)
	{
	    DC_Operation(dc) = XmDROP_LINK;
	}
	else if (DC_DragOperations(dc) && XmDROP_COPY)
	{
	    DC_Operation(dc) = XmDROP_COPY;
	}
	else if (DC_DragOperations(dc) & XmDROP_MOVE)
	{
	    DC_Operation(dc) = XmDROP_MOVE;
	}
	else
	{
	    DC_Operation(dc) = XmDROP_NOOP;
	    DC_Operations(dc) = XmDROP_NOOP;
	}

	if (DC_DragFinishTime(dc) == CurrentTime)
	{
	    _XmDragOverChange((Widget)DC_CurDragOver(dc), XmNO_DROP_SITE);
	}

	break;

    case XmCR_DRAG_MOTION:
	DC_Operation(dc) = cbs->dm.operation;
	DC_Operations(dc) = cbs->dm.operations;
	_XmDragOverChange((Widget)DC_CurDragOver(dc), cbs->dm.dropSiteStatus);
	break;

    case XmCR_OPERATION_CHANGED:
	DC_Operation(dc) = cbs->oc.operation;
	DC_Operations(dc) = cbs->oc.operations;
	_XmDragOverChange((Widget)DC_CurDragOver(dc), cbs->oc.dropSiteStatus);
	break;

    case XmCR_DROP_START:
	DC_DragCompletionStatus(dc) = cbs->ds.dropAction;
	break;

    default:
	break;
    }
}

static void
internal_notify(Widget w, XtPointer cd, XtPointer cbs)
{
    XmDragDropCallbackStruct *cb = (XmDragDropCallbackStruct *) cbs;
    Widget dc = (Widget)cd;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:internal_notify(%d)\n",
    	__FILE__, __LINE__));

    invoke_receiver_callback(dc, cb);
}

static void
external_notify(Widget w, XtPointer cd, XtPointer cbs)
{
    XmDragDropCallbackStruct *cb = (XmDragDropCallbackStruct *) cbs;
    Widget dc = (Widget)cd;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:external_notify(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSINK", w, "%s:external_notify(%d)\n",
    	__FILE__, __LINE__));

    switch (cb->any.reason)
    {
    case XmCR_DROP_SITE_ENTER:
    case XmCR_DROP_SITE_LEAVE:
    case XmCR_DRAG_MOTION:
    case XmCR_OPERATION_CHANGED:
    case XmCR_DROP_START:
	send_drag_message(dc, DC_SrcWindow(dc), cb, XmRECEIVER);
	break;

    default:
	break;
    }
}

/*
 */
static void
external_msg_handler(Widget widget, XtPointer client_data, XEvent *event,
		     Boolean *dontSwallow)
{
    Widget dc = (Widget)client_data;
    XmDragDropCallbackStruct cbs;

    DEBUGOUT(_LtDebug(__FILE__, widget, "%s:external_msg_handler(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAG_SOURCE", widget, "%s:external_msg_handler(%d)\n",
    	__FILE__, __LINE__));

    if (dc != NULL && event->type != ClientMessage)
    {
	return;
    }

    if (!receive_drag_message(dc, event, &cbs))
    {
	return;
    }

    if (DC_DragStartTime(dc) > cbs.any.timeStamp)
    {
	return;
    }

    if (DC_CrossingTime(dc) > cbs.any.timeStamp)
    {
	return;
    }

    invoke_receiver_callback(dc, &cbs);
}

static Boolean
drop_convert_incr_callback(Widget w, Atom *selection, Atom *target,
			   Atom *type, XtPointer *val,
			   unsigned long *length, int *format,
			   unsigned long *maxlen, XtPointer cd,
			   XtRequestId *receiver)
{
    XSelectionRequestEvent *sev;
    Atom ok, fail, drop;
    Widget dc;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:drop_convert_incr_callback(%d)\n",
    	__FILE__, __LINE__));

    sev = XtGetSelectionRequest(w, *selection, NULL);

    if ((dc = _XmGetDragContextFromHandle(w, *selection)) == NULL)
    {
	_XmWarning(w, "Invalid selection.\n");
	return False;
    }

    ok = XmInternAtom(XtDisplay(dc), _XA_XmTRANSFER_SUCCESS, False);
    fail = XmInternAtom(XtDisplay(dc), _XA_XmTRANSFER_FAILURE, False);

    if (*target == ok || *target == fail)
    {
	if (*target == ok)
	{
	    DC_DragDropCompletionStatus(dc) = XmDROP_SUCCESS;
	}
	else
	{
	    DC_DragDropCompletionStatus(dc) = XmDROP_FAILURE;
	}

	*type = *target;
	*val = NULL;
	*length = 0;
	*format = 32;
	*maxlen = 0;

	DC_DropFinishTime(dc) = sev->time;

	invoke_initiator_callback(dc, XmCR_DROP_FINISH);

	if (DC_BlendModel(dc) != XmBLEND_NONE)
	{
	    _XmDragOverFinish((Widget)DC_CurDragOver(dc),
			      DC_DragDropCompletionStatus(dc));
	}

	invoke_initiator_callback(dc, XmCR_DRAG_DROP_FINISH);

	XtDisownSelection(DC_SrcShell(dc), DC_ICCHandle(dc),
			  DC_DragFinishTime(dc));

	_XmFreeMotifAtom(dc, DC_ICCHandle(dc));

	XtRemoveEventHandler(DC_SrcShell(dc), FocusChangeMask, True,
			     external_msg_handler, (XtPointer)dc);

	XtDestroyWidget(dc);

	return True;
    }
    else
    {
	drop = XmInternAtom(XtDisplay(dc), _XA_MOTIF_DROP, False);

	return (Boolean)(DC_ConvertProc(dc).sel_incr) (dc,
						       &drop,
						       target, type,
						       val, length,
						       format, maxlen,
						       cd, receiver);
    }
}

static Boolean
drop_convert_callback(Widget w, Atom *selection, Atom *target,
		      Atom *type, XtPointer *val,
		      unsigned long *length, int *format)
{
    XSelectionRequestEvent *sev;
    Atom ok, fail, drop;
    Widget dc;

    DEBUGOUT(_LtDebug(__FILE__, w, "DropConvertCallback\n"));

    sev = XtGetSelectionRequest(w, *selection, NULL);

    if ((dc = _XmGetDragContextFromHandle(w, *selection)) == NULL)
    {
	_XmWarning(w, "Invalid selection.\n");

	return False;
    }

    ok = XmInternAtom(XtDisplay(dc), _XA_XmTRANSFER_SUCCESS, False);
    fail = XmInternAtom(XtDisplay(dc), _XA_XmTRANSFER_FAILURE, False);

    if (*target == ok || *target == fail)
    {
	if (*target == ok)
	{
	    DC_DragDropCompletionStatus(dc) = XmDROP_SUCCESS;
	}
	else
	{
	    DC_DragDropCompletionStatus(dc) = XmDROP_FAILURE;
	}

	*type = *target;
	*val = NULL;
	*length = 0;
	*format = 32;

	DC_DropFinishTime(dc) = sev->time;

	invoke_initiator_callback(dc, XmCR_DROP_FINISH);

	if (DC_BlendModel(dc) != XmBLEND_NONE)
	{
	    _XmDragOverFinish((Widget)DC_CurDragOver(dc),
			      DC_DragDropCompletionStatus(dc));
	}

	invoke_initiator_callback(dc, XmCR_DRAG_DROP_FINISH);

	XtDisownSelection(DC_SrcShell(dc), DC_ICCHandle(dc),
			  DC_DragFinishTime(dc));

	_XmFreeMotifAtom(dc, DC_ICCHandle(dc));

	XtRemoveEventHandler(DC_SrcShell(dc), FocusChangeMask, True,
			     external_msg_handler, (XtPointer)dc);

	XtDestroyWidget(dc);

	return True;
    }
    else
    {

	drop = XmInternAtom(XtDisplay(dc), _XA_MOTIF_DROP, False);

	return (Boolean)(DC_ConvertProc(dc).sel) (dc,
						  &drop,
						  target, type,
						  val, length,
						  format);
    }
}

static void
drop_lose_incr_callback(Widget w, Atom *selection, XtPointer cd)
{
    Widget dc;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:drop_lose_incr_callback(%d)\n",
    	__FILE__, __LINE__));

    if ((dc = _XmGetDragContextFromHandle(w, *selection)) == NULL)
    {
	_XmWarning(w, "bad selection\n");
    }

    if (dc && DC_DropFinishTime(dc) == CurrentTime)
    {
	_XmWarning(w, "lost the drop selection.\n");
    }
}

static void
drop_lose_callback(Widget w, Atom *selection)
{
    Widget dc;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:drop_lose_callback(%d)\n",
    	__FILE__, __LINE__));

    if ((dc = _XmGetDragContextFromHandle(w, *selection)) == NULL)
    {
	_XmWarning(w, "bad selection\n");
    }

    if (dc && DC_DropFinishTime(dc) == CurrentTime)
    {
	_XmWarning(w, "lost the drop selection.\n");
    }
}

static void
drop_timeout(XtPointer data, XtIntervalId *id)
{
    Widget dc = (Widget)data;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:drop_timeout(%d)\n",
    	__FILE__, __LINE__));

    DC_DragCompletionStatus(dc) = XmDROP_CANCEL;

    invoke_initiator_callback(dc, XmCR_DROP_START);

    DC_DragDropCompletionStatus(dc) = XmDROP_FAILURE;
    DC_DropFinishTime(dc) = DC_DragFinishTime(dc);

    invoke_initiator_callback(dc, XmCR_DROP_FINISH);

    if (DC_BlendModel(dc) != XmBLEND_NONE)
    {
	_XmDragOverFinish((Widget)DC_CurDragOver(dc),
			  DC_DragDropCompletionStatus(dc));
    }

    invoke_initiator_callback(dc, XmCR_DRAG_DROP_FINISH);

    XtDisownSelection(DC_SrcShell(dc), DC_ICCHandle(dc),
		      DC_DragFinishTime(dc));

    _XmFreeMotifAtom(dc, DC_ICCHandle(dc));

    XtRemoveEventHandler(DC_SrcShell(dc), FocusChangeMask, True,
			 external_msg_handler, (XtPointer)dc);

    XtDestroyWidget(dc);
}

/*
 * create receiver info for a root window
 */
static void
create_root_info(Widget w)
{
    Screen *scr = DC_CurrScreen(w);
    XmDragReceiverInfo ri;
    Display *disp = XtDisplay(w);

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:create_root_info(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSOURCE", w, "%s:create_root_info(%d)\n",
    	__FILE__, __LINE__));

    if (DC_NumReceiverInfos(w) == 0)
    {
	ri = _XmAllocReceiverInfo((XmDragContext)w);
    }
    else
    {
	ri = &DC_ReceiverInfos(w)[0];
    }

    DC_RootReceiverInfo(w) = ri;

    ri->frame = None;
    ri->window = RootWindowOfScreen(scr);
    ri->shell = XtWindowToWidget(disp, XtWindow(DC_CurDragOver(w)));
    ri->xOrigin = 0;
    ri->yOrigin = 0;
    ri->width = WidthOfScreen(scr);
    ri->height = HeightOfScreen(scr);
    ri->depth = DefaultDepthOfScreen(scr);

    if (!_XmGetDragReceiverInfo(disp, ri->window, ri))
    {
	DEBUGOUT(_LtDebug("DRAGSOURCE", w, "%s:create_root_info(%d) - couldn't get receiver info\n",
	    __FILE__, __LINE__));
	return;
    }

    DEBUGOUT(_LtDebug("DRAGSOURCE", w, "%s:create_root_info(%d) - protocol %s\n",
    	__FILE__, __LINE__,
    	_LtDebugDragType2String(ri->dragProtocolStyle)));

    if (ri->dragProtocolStyle == XmDRAG_NONE ||
	ri->dragProtocolStyle == XmDRAG_DROP_ONLY ||
	ri->dragProtocolStyle == XmDRAG_DYNAMIC)
    {
	_XmFreeDragReceiverInfo((XmShellDropSiteInfo) ri->iccInfo);
    }
}

/*
 * create a receiverInfo and a DragOverShell for a new
 * root window
 */
static void
new_root(Widget dc, Window root)
{
    int i, mask, argc;
    XmDragOverShellWidget dos;
    Cursor cursor;
    Arg args[10];

    DEBUGOUT(_LtDebug(__FILE__, dc, "%s:new_root(%d)\n",
    	__FILE__, __LINE__));

    dos = DC_CurDragOver(dc);

    for (i = 0; i < ScreenCount(XtDisplay(dc)); i++)
    {
	if (RootWindowOfScreen(ScreenOfDisplay(XtDisplay(dc), i)) == root)
	{
	    break;
	}
    }

    DC_CurrScreen(dc) = ScreenOfDisplay(XtDisplay(dc), i);
    DC_CurrWmRoot(dc) = root;

    argc = 0;
    if (DC_ActiveProtocolStyle(dc) == XmDRAG_DYNAMIC)
    {
	XtSetArg(args[argc], XmNdragOverMode, XmCURSOR);
	argc++;
    }
    else
    {
	XtSetArg(args[argc], XmNdragOverMode, XmPIXMAP);
	argc++;
    }
    XtSetArg(args[argc], XmNhotX, 1); argc++;
    XtSetArg(args[argc], XmNhotY, 1); argc++;
    XtSetArg(args[argc], XmNbackgroundPixmap, None); argc++;
    XtSetArg(args[argc], XmNscreen, DC_CurrScreen(dc)); argc++;
    XtSetArg(args[argc], XmNdepth,
	     DefaultDepthOfScreen(DC_CurrScreen(dc))); argc++;
    XtSetArg(args[argc], XmNcolormap,
	     DefaultColormapOfScreen(DC_CurrScreen(dc))); argc++;
    XtSetArg(args[argc], XmNvisual,
	     DefaultVisualOfScreen(DC_CurrScreen(dc))); argc++;
    DC_CurDragOver(dc) =
	(XmDragOverShellWidget)XtCreatePopupShell("dragOver",
						  xmDragOverShellWidgetClass,
						  dc, args, argc);

    if (XtScreen(DC_CurDragOver(dc)) == XtScreen(DC_SrcShell(dc)))
    {
	_XmDragOverSetInitialPosition((Widget)DC_CurDragOver(dc),
				      DC_StartX(dc), DC_StartY(dc));
    }

    if (dos)
    {
	if (DC_OrigDragOver(dc) != dos)
	{
	    XtDestroyWidget((Widget)DC_OrigDragOver(dc));
	}
	else
	{
	    _XmDragOverHide((Widget)DC_OrigDragOver(dc), 0, 0, NULL);
	}
    }

    create_root_info(dc);

    if (DC_OrigDragOver(dc) == NULL)
    {
	DC_OrigDragOver(dc) = DC_CurDragOver(dc);
    }

    mask = ButtonMotionMask | ButtonPressMask | ButtonReleaseMask;

    cursor = _XmDragOverGetActiveCursor((Widget)DC_CurDragOver(dc));

    XGrabPointer(XtDisplay(dc), RootWindowOfScreen(XtScreen(dc)), False,
		 mask, GrabModeSync, GrabModeAsync,
		 None, cursor, DC_LastChangeTime(dc));

    XAllowEvents(XtDisplay(dc), SyncPointer, DC_LastChangeTime(dc));
}

/*
 * write the initiator info on the source window
 */
static void
write_initiator(Widget dc)
{
    XmDndSourceProp src_prop;
    Atom init;

    DEBUGOUT(_LtDebug(__FILE__, dc, "%s:write_initiator(%d)\n",
    	__FILE__, __LINE__
    	));

    src_prop.byte_order = _XmByteOrder();
    src_prop.protocol_version = DND_PROTOCOL_VERSION;
    src_prop.target_index = _XmTargetsToIndex(dc,
					      DC_ExportTargets(dc),
					      DC_NumExportTargets(dc));
    src_prop.selection = DC_ICCHandle(dc);

    init = XmInternAtom(XtDisplay(dc), _XA_MOTIF_DRAG_INITIATOR_INFO, False);

    /* write the buffer to the property */
    XChangeProperty(XtDisplay(dc), DC_SrcWindow(dc), DC_ICCHandle(dc),
		    init, 8, PropModeReplace, (unsigned char *)&src_prop,
		    sizeof(XmDndSourceProp));
}

/*
 * find the shell child of a frame.  This will be the
 * child that has the WM_STATE property set -- we need to skip over the
 * frame and the matte.
 */
static Window
find_shell_child(Display *display, Window win)
{
    Atom type = 0;
    int format;
    unsigned int nchildren;
    unsigned long bafter, length;
    unsigned char *prop;
    Window root, parent, *children;
    Atom state;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:find_shell_child(%d)\n",
    	__FILE__, __LINE__));

    state = XmInternAtom(display, _XA_WM_STATE, True);

    XGetWindowProperty(display, win, state, 0L, 0L, False,
		       0, &type, &format, &length, &bafter,
		       &prop);

    if (type != 0)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL, "find_shell_child: %08x\n", win));
	return win;
    }

    if (XQueryTree(display, win, &root, &parent, &children, &nchildren) == 0)
    {
	return None;
    }

    if (nchildren == 0)
    {
	return None;
    }

    while (nchildren > 0)
    {
	win = find_shell_child(display, children[--nchildren]);

	if (win != None)
	{
	    XFree((char *)children);

	    DEBUGOUT(_LtDebug(__FILE__, NULL,
			      "find_shell_child 2: %08x\n", win));

	    return win;
	}
    }

    if (children)
    {
	XFree((char *)children);
    }

    return None;
}

/*
 * from a top-level window, find the receiver info
 */
static XmDragReceiverInfo
find_receiver_info(Widget dc, Window win)
{
    Cardinal i;
    XmDragReceiverInfo ri;

    DEBUGOUT(_LtDebug(__FILE__, dc, "%s:find_receiver_info(%d)\n",
    	__FILE__, __LINE__));

    /* maybe there's a match */
    for (i = 0; i < DC_NumReceiverInfos(dc); i++)
    {
	if (DC_ReceiverInfos(dc)[i].frame == win ||
	    DC_ReceiverInfos(dc)[i].window == win)
	{
	    break;
	}
    }

    if (i == DC_NumReceiverInfos(dc))
    {
	ri = NULL;
    }
    else
    {
	ri = &DC_ReceiverInfos(dc)[i];
    }

    return ri;
}

/*
 * setup the current receiver information.
 */
static void
get_current_receiver(Widget dc, Window root, Window win)
{
    XmDragReceiverInfo ri;
    Window tmp;
    Display *display = XtDisplay(dc);
    unsigned char last_style;
    Position deltax, deltay;
    int rootx, rooty;

    DEBUGOUT(_LtDebug(__FILE__, dc, "%s:get_current_receiver(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSOURCE", dc, "%s:get_current_receiver(%d)\n",
    	__FILE__, __LINE__));

    DC_CrossingTime(dc) = DC_LastChangeTime(dc);

    last_style = DC_ActiveProtocolStyle(dc);

    ri = find_receiver_info(dc, win);

    DC_CurrReceiverInfo(dc) = ri;

    if (XtWindow(DC_SrcShell(dc)) == win)
    {
	if (ri == NULL || ri->frame == ri->window)
	{
	    deltax = DC_StartX(dc) - XtX(DC_SrcShell(dc));
	    deltay = DC_StartY(dc) - XtY(DC_SrcShell(dc));

	    if (deltax < 0)
	    {
		deltax = 0;
	    }
	    if (deltay < 0)
	    {
		deltay = 0;
	    }

	    XTranslateCoordinates(XtDisplay(dc), win, DC_CurrWmRoot(dc),
				  deltax, deltay, &rootx, &rooty,
				  &win);

	    if (ri != NULL)
	    {
		ri->frame = win;
	    }
	}
    }

    if (ri == NULL)
    {

	tmp = find_shell_child(display, win);
	if (tmp == None)
	{
	    tmp = win;
	}

	ri = _XmAllocReceiverInfo((XmDragContext)dc);

	DC_CurrReceiverInfo(dc) = ri;

	ri->frame = win;
	ri->window = tmp;
	ri->shell = XtWindowToWidget(display, tmp);
    }

    /* if it's not the root */
    if (DC_CurrReceiverInfo(dc) != DC_RootReceiverInfo(dc))
    {
	/* if it's not local */
	if (ri->shell == NULL)
	{
	    if (_XmGetDragReceiverInfo(display, ri->window, ri))
	    {
		if (ri->dragProtocolStyle == XmDRAG_NONE ||
		    ri->dragProtocolStyle == XmDRAG_DROP_ONLY ||
		    ri->dragProtocolStyle == XmDRAG_DYNAMIC)
		{
		    _XmFreeDragReceiverInfo((XmShellDropSiteInfo) ri->iccInfo);
		}
	    }
	}
	else
	{
	    if (!_XmDropSiteShell(ri->shell))
	    {
		ri->dragProtocolStyle = XmDRAG_NONE;
	    }
	    else
	    {
#if 1 /* mark/heiko */
		ri->dragProtocolStyle = 
			Display_DragReceiverProtocolStyle(XmGetXmDisplay(XtDisplay(ri->shell)));
#else
		ri->dragProtocolStyle = DC_ActiveProtocolStyle(dc);
#endif
	    }

	    ri->xOrigin = XtX(ri->shell);
	    ri->yOrigin = XtY(ri->shell);
	    ri->width = XtWidth(ri->shell);
	    ri->height = XtHeight(ri->shell);
	    ri->depth = CoreDepth(ri->shell);
	    ri->iccInfo = NULL;
	}
    }

    DC_ActiveProtocolStyle(dc) = _XmGetActiveProtocolStyle(dc);
}

/************************* DRAG START *************************/
/*
 * set the initiator, get the first receiver, and handle motion messages
 */
static void
drag_main_loop(XtPointer data, XtIntervalId *id)
{
    Widget dc = *(Widget *)data, fw;
    XtAppContext app;
    XEvent event;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:drag_main_loop(%d)\n",
    	__FILE__, __LINE__));

    if (dc == NULL)
	return;

    app = XtWidgetToApplicationContext(dc);

    fw = XmGetFocusWidget(DC_SrcShell(dc));

    /* T. Straumann: avoid dereferencing NULL fw */
    if (fw)
    {
	if (_XmGetFocusPolicy(DC_SrcShell(dc)) != XmEXPLICIT)
	{
	    WidgetClass wc = XtClass(fw);

	    if (XmIsPrimitive(fw) && PrimC_BorderUnhighlight(wc) != NULL)
	    {
		PrimC_BorderUnhighlight(wc) (fw);
	    }
	    else if (XmIsGadget(fw) && GC_BorderUnhighlight(wc) != NULL)
	    {
		GC_BorderUnhighlight(wc) (fw);
	    }
	}
	else
	{
	    XtSetKeyboardFocus(DC_SrcShell(dc), NULL);
	}
    }

    write_initiator(dc);

    get_current_receiver(dc,
			 RootWindowOfScreen(XtScreen(dc)),
			 XtWindow(DC_SrcShell(dc)));

    /* need to invoke dragStart callbacks */
    invoke_initiator_callback(dc, XmCR_TOP_LEVEL_ENTER);

    maybe_send_drag_message(dc, DC_CurrReceiverInfo(dc)->window,
			    XmTOP_LEVEL_ENTER);

    maybe_send_drag_message(dc, DC_CurrReceiverInfo(dc)->window,
			    XmDRAG_MOTION);

    if (dc)
    {
	while (*(Widget *)data)
	{
	    XtAppNextEvent(app, &event);

	    switch (event.xany.type)
	    {
	    case KeyPress:
	    case KeyRelease:
	    case ButtonPress:
	    case ButtonRelease:
	    case MotionNotify:
	    case EnterNotify:
	    case LeaveNotify:
		DEBUGOUT(_LtDebug(__FILE__, dc,
				  "Type: %s Window: %08x swindow: %08x "
				  "root: %08x\n",
				  _LtDebugEventType2String(event.type), event.xbutton.subwindow,
				  event.xbutton.window, event.xbutton.root));

		/* so that the event gets delivered to the right widget (us) */
		event.xany.window = XtWindow(dc);
		break;

	    case FocusIn:
	    case FocusOut:
	    default:
		break;
	    }

	    DEBUGOUT(_LtDebug(__FILE__, dc,
			      "Dispatch event: state: %d last %d send: %d\n",
			      event.xmotion.state, DC_LastEventState(dc),
			      event.xany.send_event));

	    XtDispatchEvent(&event);
	}
    }

    if (_XmGetFocusPolicy(DC_SrcShell(dc)) == XmEXPLICIT)
    {
	XtSetKeyboardFocus(DC_SrcShell(dc), fw);
    }
}

/************************* DRAG MOTION *************************/
static void
add_motion(Widget w, XmMotionBuffer * mb, XEvent *event)
{
    XmDragReceiverInfo ri;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:add_motion(%d)\n",
    	__FILE__, __LINE__));

    ri = DC_CurrReceiverInfo(w);

    if (!ri)
    {
	return;
    }

    DC_LastChangeTime(w) = event->xmotion.time;

    switch (event->type)
    {
    case MotionNotify:
	if (mb->num_motions == mb->max_motions)
	{
	    if (mb->max_motions == 0)
	    {
		mb->max_motions = 100;

		mb->motions = (XmMotionRec *) XtMalloc(mb->max_motions *
						       sizeof(XmMotionRec));
	    }
	    else
	    {
		mb->max_motions += 100;

		mb->motions = (XmMotionRec *) XtRealloc((char *)mb->motions,
							mb->max_motions *
							sizeof(XmMotionRec));
	    }
	}

	mb->motions[mb->num_motions].time = event->xmotion.time;
	mb->motions[mb->num_motions].window = event->xmotion.root;
	mb->motions[mb->num_motions].subwindow = event->xmotion.subwindow;
	mb->motions[mb->num_motions].xroot = event->xmotion.x_root;
	mb->motions[mb->num_motions].yroot = event->xmotion.y_root;
	mb->motions[mb->num_motions].state = event->xmotion.state;

	mb->num_motions++;

	break;

    default:
	return;
    }
}

static void
motion_message(Widget w, Window root, Window win)
{
    Boolean left_drop = False;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:motion_message(%d)\n",
    	__FILE__, __LINE__));

    if (root != DC_CurrWmRoot(w) ||
	(DC_CurrReceiverInfo(w)->frame != win))
    {
	if (DC_CurrReceiverInfo(w)->window != None)
	{
	    if (DC_ActiveProtocolStyle(w) != XmDRAG_NONE &&
		DC_ActiveProtocolStyle(w) != XmDRAG_DROP_ONLY)
	    {
		if (DC_ActiveProtocolStyle(w) == XmDRAG_DYNAMIC &&
		    DC_CurrReceiverInfo(w)->shell == NULL &&
		    DC_InDropSite(w))
		{
		    invoke_initiator_callback(w, XmCR_DROP_SITE_LEAVE);

		    DC_InDropSite(w) = False;

		    left_drop = True;
		}

		maybe_send_drag_message(w, DC_CurrReceiverInfo(w)->window,
					XmDRAG_MOTION);

		maybe_send_drag_message(w, DC_CurrReceiverInfo(w)->window,
					XmTOP_LEVEL_LEAVE);
	    }

	    invoke_initiator_callback(w, XmCR_TOP_LEVEL_LEAVE);
	}

	/* did we move over to a new screen? */
	if (DC_CurrWmRoot(w) != root)
	{
	    new_root(w, root);
	}

	get_current_receiver(w, root, win);

	if (DC_CurrReceiverInfo(w)->window != None)
	{
	    if (DC_ActiveProtocolStyle(w) != XmDRAG_NONE)
	    {
		maybe_send_drag_message(w, DC_CurrReceiverInfo(w)->window,
					XmTOP_LEVEL_ENTER);
	    }

	    invoke_initiator_callback(w, XmCR_TOP_LEVEL_ENTER);

	    DC_CurrReceiverInfo(w)->iccInfo = NULL;
	}
    }

    if (DC_CurrReceiverInfo(w)->window != None &&
	DC_ActiveProtocolStyle(w) != XmDRAG_NONE &&
	DC_ActiveProtocolStyle(w) != XmDRAG_DROP_ONLY)
    {
	maybe_send_drag_message(w, DC_CurrReceiverInfo(w)->window,
				XmDRAG_MOTION);
    }

    invoke_initiator_callback(w, XmCR_DRAG_MOTION);

    if (left_drop)
    {
	DC_LastChangeTime(w)++;
    }
}

static void
maybe_operation_changed(Widget w, unsigned int state)
{
    unsigned char ops, op;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:maybe_operation_changed(%d)\n",
    	__FILE__, __LINE__));

    DC_LastEventState(w) = state;

    ops = DC_Operations(w);
    op = DC_Operation(w);

    DC_Operations(w) = DC_DragOperations(w);

    if (state & ShiftMask && state & ControlMask)
    {
	DC_Operation(w) = DC_DragOperations(w) & XmDROP_LINK;
	DC_Operations(w) = DC_DragOperations(w) & XmDROP_LINK;
    }
    else if (state & ShiftMask)
    {
	DC_Operation(w) = DC_DragOperations(w) & XmDROP_MOVE;
	DC_Operations(w) = DC_DragOperations(w) & XmDROP_MOVE;
    }
    else if (state & ControlMask)
    {
	DC_Operation(w) = DC_DragOperations(w) & XmDROP_COPY;
	DC_Operations(w) = DC_DragOperations(w) & XmDROP_COPY;
    }
    else if (DC_DragOperations(w) & XmDROP_LINK)
    {
	DC_Operation(w) = XmDROP_LINK;
    }
    else if (DC_DragOperations(w) && XmDROP_COPY)
    {
	DC_Operation(w) = XmDROP_COPY;
    }
    else if (DC_DragOperations(w) & XmDROP_MOVE)
    {
	DC_Operation(w) = XmDROP_MOVE;
    }
    else
    {
	DC_Operation(w) = XmDROP_NOOP;
	DC_Operations(w) = XmDROP_NOOP;
    }

    if (DC_Operations(w) == ops && DC_Operation(w) == op)
    {
	return;
    }

    if (DC_CurrReceiverInfo(w)->window != None &&
	DC_ActiveProtocolStyle(w) != XmDRAG_NONE &&
	DC_ActiveProtocolStyle(w) != XmDRAG_DROP_ONLY)
    {
	maybe_send_drag_message(w, DC_CurrReceiverInfo(w)->window,
				XmOPERATION_CHANGED);
    }
    else
    {
	invoke_initiator_callback(w, XmCR_OPERATION_CHANGED);
    }
}

/*
 * if events pile up on us, don't look at all events.  This
 * could cause the protocol to thrash.  Instead, look at
 * a progressively smaller subset, based on the number of
 * events queued.
 */
#define OPTIMAL_GRANULARITY	16
static void
motion_hysteresis(Widget w, XmMotionBuffer * mb)
{
    int i, grain_size, box_count, box_offset;
    Window sw = None;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:motion_hysteresis(%d)\n",
    	__FILE__, __LINE__));

    grain_size = mb->num_motions / OPTIMAL_GRANULARITY;

    if (grain_size == 0)
    {
	grain_size = 1;
    }

    box_count = mb->num_motions / grain_size;

    box_offset = (mb->num_motions + grain_size - 1) % grain_size;

    for (i = 0; i < box_count; i++)
    {

	XtX(w) = mb->motions[i * grain_size + box_offset].xroot;
	XtY(w) = mb->motions[i * grain_size + box_offset].yroot;

	if (DC_LastEventState(w) !=
	    mb->motions[i * grain_size + box_offset].state)
	{
	    maybe_operation_changed(w,
			     mb->motions[i * grain_size + box_offset].state);
	}

	if (mb->motions[i * grain_size + box_offset].window != DC_CurrWmRoot(w))
	{
	    motion_message(w,
			   mb->motions[i * grain_size + box_offset].window,
			   None);
	    sw = None;
	}
	else
	{
	    sw = mb->motions[i * grain_size + box_offset].subwindow;
	}
    }

    _XmDragOverMove((Widget)DC_CurDragOver(w), XtX(w), XtY(w));

    if (sw != None)
    {
	motion_message(w, DC_CurrWmRoot(w), sw);
    }

    if (mb->num_motions != 0)
    {
	XtFree((char *)mb->motions);
    }
}

/************************* DRAG FINISH *************************/
static void
drag_finish(Widget w, XEvent *event)
{
    int state = 0;
    XmDisplay disp = _XmDCtoDD(w);
    int ac;
    Arg al[5];
    XtAppContext app;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:drag_finish(%d)\n",
    	__FILE__, __LINE__));

    /* NOTE: This is how we stop the main loop */
    Display_ActiveDC(disp) = NULL;

    if (event != NULL)
    {
	/* Term on button release */
	if (event->type == ButtonRelease)
	{
	    state = event->xbutton.state;

	    XtX(w) = event->xbutton.x_root;
	    XtY(w) = event->xbutton.y_root;

	    DC_LastChangeTime(w) = event->xbutton.time;
	}
	/* Term on KeyCancel */
	else if (event->type == KeyPress)
	{
	    state = event->xkey.state;

	    XtX(w) = event->xkey.x_root;
	    XtY(w) = event->xkey.y_root;

	    DC_LastChangeTime(w) = event->xkey.time;
	}

	DC_Operations(w) = DC_DragOperations(w);
	DC_LastEventState(w) = state;

	if (state & ShiftMask && state & ControlMask)
	{
	    DC_Operation(w) = DC_DragOperations(w) & XmDROP_LINK;
	    DC_Operations(w) = DC_DragOperations(w) & XmDROP_LINK;
	}
	else if (state & ShiftMask)
	{
	    DC_Operation(w) = DC_DragOperations(w) & XmDROP_MOVE;
	    DC_Operations(w) = DC_DragOperations(w) & XmDROP_MOVE;
	}
	else if (state & ControlMask)
	{
	    DC_Operation(w) = DC_DragOperations(w) & XmDROP_COPY;
	    DC_Operations(w) = DC_DragOperations(w) & XmDROP_COPY;
	}
	else if (DC_DragOperations(w) & XmDROP_LINK)
	{
	    DC_Operation(w) = XmDROP_LINK;
	}
	else if (DC_DragOperations(w) && XmDROP_COPY)
	{
	    DC_Operation(w) = XmDROP_COPY;
	}
	else if (DC_DragOperations(w) & XmDROP_MOVE)
	{
	    DC_Operation(w) = XmDROP_MOVE;
	}
	else
	{
	    DC_Operation(w) = XmDROP_NOOP;
	    DC_Operations(w) = XmDROP_NOOP;
	}
    }

    ac = 0;
    XtSetArg(al[ac], XmNhotX, XtX(w)); ac++;
    XtSetArg(al[ac], XmNhotY, XtY(w)); ac++;
    XtSetArg(al[ac], XmNdragOverMode, XmWINDOW); ac++;
    XtSetValues((Widget)DC_CurDragOver(w), al, ac);

    XUngrabPointer(XtDisplay(w), DC_LastChangeTime(w));

    XtUngrabPointer(w, DC_DragFinishTime(w));

    XUngrabKeyboard(XtDisplay(w), DC_LastChangeTime(w));

    _XmRemoveGrab(w);

    DC_DragFinishTime(w) = DC_LastChangeTime(w);

    if (DC_InDropSite(w))
    {
	invoke_initiator_callback(w, XmCR_DROP_SITE_LEAVE);
	DC_InDropSite(w) = False;
    }

    if (DC_CurrReceiverInfo(w)->window != None)
    {
	maybe_send_drag_message(w,
				DC_CurrReceiverInfo(w)->window,
				XmTOP_LEVEL_LEAVE);

	invoke_initiator_callback(w, XmCR_TOP_LEVEL_LEAVE);

	if (DC_ActiveProtocolStyle(w) != XmDRAG_NONE &&
	    (DC_DragCompletionStatus(w) == XmDROP ||
	     DC_DragCompletionStatus(w) == XmDROP_HELP))
	{
	    app = XtWidgetToApplicationContext(w);

	    DC_DragTimerId(w) = XtAppAddTimeOut(app,
						XtAppGetSelectionTimeout(app),
						drop_timeout, (XtPointer)w);

	    maybe_send_drag_message(w,
				    DC_CurrReceiverInfo(w)->window,
				    XmDROP_START);
	}
	else
	{
	    DC_DragDropCompletionStatus(w) = XmDROP_FAILURE;
	    DC_DropFinishTime(w) = DC_DragFinishTime(w);

	    drop_timeout((XtPointer)w, NULL);
	}
    }

    DC_CurrReceiverInfo(w)->frame = None;
}

/******************** ACTION PROCS ********************/
static void
_XmDCStartDrag(XmDragContext dc,
	       Widget srcW,
	       XEvent *event)
{
    unsigned int state;
    Time time;
    XmDisplay disp;
    Cursor cursor;
    int mask;
    XWindowAttributes attr;
    Window win;

    DEBUGOUT(_LtDebug(__FILE__, srcW, "_XmDCStartDrag\n"));
    DEBUGOUT(_LtDebug("DRAGSOURCE", srcW, "_XmDCStartDrag\n"));

    disp = _XmDCtoDD((Widget)dc);

    state = event->xbutton.state;
    time = event->xbutton.time;

    Display_ActiveDC(disp) = dc;

    DC_DragStartTime(dc) = time;
    DC_LastChangeTime(dc) = time;
    DC_CrossingTime(dc) = time;

    XtX(dc) = DC_StartX(dc) = event->xbutton.x_root;
    XtY(dc) = DC_StartY(dc) = event->xbutton.y_root;

    DC_CurDragOver(dc) = NULL;
    DC_OrigDragOver(dc) = NULL;

    /* find the shell this guy lives in */
    DC_SrcShell(dc) = _XmFindTopMostShell(srcW);
    DC_SrcWindow(dc) = XtWindow(DC_SrcShell(dc));

    DC_ICCHandle(dc) = _XmAllocMotifAtom(DC_SrcShell(dc), DC_DragStartTime(dc));

    if (DC_Incremental(dc))
    {
	XtOwnSelectionIncremental(DC_SrcShell(dc), DC_ICCHandle(dc),
				  DC_DragStartTime(dc),
				  drop_convert_incr_callback,
				  drop_lose_incr_callback,
				  NULL, NULL, DC_ClientData(dc));
    }
    else
    {
	XtOwnSelection(DC_SrcShell(dc), DC_ICCHandle(dc),
		       DC_DragStartTime(dc),
		       drop_convert_callback,
		       drop_lose_callback,
		       NULL);
    }

    DC_SourceIsExternal(dc) = False;

    DC_ActiveProtocolStyle(dc) = _XmGetActiveProtocolStyle((Widget)dc);

    DEBUGOUT(_LtDebug("DRAGSOURCE", srcW, "_XmDCStartDrag - Protocol %s (%d)\n",
    	_LtDebugDragType2String(DC_ActiveProtocolStyle(dc)), DC_ActiveProtocolStyle(dc)));

    /* the protocol gotten from _XmGetActiveProtocolStyle() needs to be
     * filtered for reasonable values for the initiator. */
    switch (DC_ActiveProtocolStyle(dc))
    {
    case XmDRAG_DROP_ONLY:
	DC_ActiveProtocolStyle(dc) = XmDRAG_NONE;
	break;

    case XmDRAG_PREREGISTER:
	DC_ActiveProtocolStyle(dc) = XmDRAG_DYNAMIC;
	break;

    default:
	break;
    }

    DC_Operations(dc) = DC_DragOperations(dc);
    DC_LastEventState(dc) = state;

    if (state & ShiftMask && state & ControlMask)
    {
	DC_Operation(dc) = DC_DragOperations(dc) & XmDROP_LINK;
	DC_Operations(dc) = DC_DragOperations(dc) & XmDROP_LINK;
    }
    else if (state & ShiftMask)
    {
	DC_Operation(dc) = DC_DragOperations(dc) & XmDROP_MOVE;
	DC_Operations(dc) = DC_DragOperations(dc) & XmDROP_MOVE;
    }
    else if (state & ControlMask)
    {
	DC_Operation(dc) = DC_DragOperations(dc) & XmDROP_COPY;
	DC_Operations(dc) = DC_DragOperations(dc) & XmDROP_COPY;
    }
    else if (DC_DragOperations(dc) & XmDROP_LINK)
    {
	DC_Operation(dc) = XmDROP_LINK;
    }
    else if (DC_DragOperations(dc) && XmDROP_COPY)
    {
	DC_Operation(dc) = XmDROP_COPY;
    }
    else if (DC_DragOperations(dc) & XmDROP_MOVE)
    {
	DC_Operation(dc) = XmDROP_MOVE;
    }
    else
    {
	DC_Operation(dc) = XmDROP_NOOP;
	DC_Operations(dc) = XmDROP_NOOP;
    }

    new_root((Widget)dc, RootWindowOfScreen(XtScreen(dc)));

    XtInsertEventHandler(DC_SrcShell(dc), FocusChangeMask, True,
			 external_msg_handler, (XtPointer)dc, XtListHead);

    mask = ButtonMotionMask | ButtonPressMask | ButtonReleaseMask;

    cursor = _XmDragOverGetActiveCursor((Widget)DC_CurDragOver(dc));

    win = XtWindow(dc);

    CoreWindow(dc) = RootWindowOfScreen(XtScreen(dc));

    if (XtGrabPointer((Widget)dc, False, mask, GrabModeSync, GrabModeAsync,
		      None, cursor, DC_DragStartTime(dc)) != GrabSuccess ||
	XGrabPointer(XtDisplay(dc), RootWindowOfScreen(XtScreen(dc)),
		     False, mask, GrabModeSync, GrabModeAsync,
		     None, cursor, DC_DragStartTime(dc)) != GrabSuccess ||
	XGrabKeyboard(XtDisplay(dc), RootWindowOfScreen(XtScreen(dc)),
		      False, GrabModeSync, GrabModeAsync,
		      DC_DragStartTime(dc)) != GrabSuccess)
    {
	_XmWarning((Widget)dc, "No grab on _XmDCStartDrag!\n");
    }

    _XmAddGrab((Widget)dc, True, False);

    CoreWindow(dc) = win;

    XGetWindowAttributes(XtDisplay(dc), DC_CurrWmRoot(dc), &attr);

    attr.your_event_mask |= ButtonMotionMask;

    XSelectInput(XtDisplay(dc), DC_CurrWmRoot(dc), attr.your_event_mask);

    if (DC_DragFinishTime(dc) == CurrentTime)
    {
	XAllowEvents(XtDisplay(dc), SyncPointer, DC_LastChangeTime(dc));
    }

    XSync(XtDisplay(dc), False);

    XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)dc), 0, drag_main_loop,
		    (XtPointer)&Display_ActiveDC(disp));
}

/*
 * this gets called directly by XmDragCancel()
 */
static void
_XmDCCancelDrag(XmDragContext dc)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmDCCancelDrag\n"));

    DC_DragCompletionStatus(dc) = XmDROP_CANCEL;

    drag_finish((Widget)dc, NULL);
}

/*
 * but this gets called by translation
 */
static void
_XmDCCancel(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmDCCancel\n"));

    /* make sure we don't cancel if already finished dragging */
    if (DC_DragFinishTime(w) == CurrentTime)
    {
	DC_DragCompletionStatus(w) = XmDROP_CANCEL;
	drag_finish(w, event);
    }
}

/*
 * handle motion events
 */
static void
_XmDCMotion(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    Window child, root;
    unsigned mask, dummy;
    int child_y, child_x;
    int root_y, root_x;
    Boolean done = False;
    XmMotionBuffer mb;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:_XmDCMotion(%d)\n",
    	__FILE__, __LINE__));

    if (event->type == MotionNotify)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmDCMotion: Motion\n"));
    }
    else if (event->type == EnterNotify)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmDCMotion: Enter\n"));
    }
    else if (event->type == LeaveNotify)
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "_XmDCMotion: Leave\n"));
    }

    DEBUGOUT(_LtDebug(__FILE__, w, "DCMotion: w: %08x sw: %08x\n",
		      event->xbutton.window, event->xbutton.subwindow));

    root = DC_CurrWmRoot(w);

    mb.curReceiver = DC_CurrReceiverInfo(w);
    mb.num_motions = 0;
    mb.max_motions = 0;

    add_motion(w, &mb, event);

    mask = ButtonMotionMask | ButtonPressMask | ButtonReleaseMask;

    while (!done && XCheckMaskEvent(XtDisplay(w), mask, event))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "DCMotionLoop 1\n"));

	switch (event->type)
	{
	case ButtonPress:
	case ButtonRelease:
	case KeyPress:
	case KeyRelease:
	    done = True;
	    break;	/* EH? */

	default:
	    done = False;
	}

	if (!done)
	{
	    add_motion(w, &mb, event);
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "Put back button or key event: 1.\n"));

	    XPutBackEvent(XtDisplay(w), event);
	}
    }

    XQueryPointer(XtDisplay(w), root, &root,
		  &child, &root_x, &root_y, &child_x, &child_y,
		  &dummy);

    while (!done && XCheckMaskEvent(XtDisplay(w), mask, event))
    {

	DEBUGOUT(_LtDebug(__FILE__, w, "DCMotionLoop 2\n"));

	switch (event->type)
	{
	case ButtonPress:
	case ButtonRelease:
	case KeyPress:
	case KeyRelease:
	    done = True;
	    break;	/* EH? */

	default:
	    done = False;
	}

	if (!done)
	{
	    add_motion(w, &mb, event);
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, w,
			      "Put back button or key event: 1.\n"));
	    XPutBackEvent(XtDisplay(w), event);
	}

    }

    motion_hysteresis(w, &mb);

    XFlush(XtDisplay(w));
}

/*
 * terminate with drop
 */
static void
_XmDCFinish(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmDCFinish\n"));

    DC_DragCompletionStatus(w) = XmDROP;

    drag_finish(w, event);
}

/*
 * terminate with help
 */
static void
_XmDCHelp(Widget w,
	  XEvent *event,
	  String *params,
	  Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmDCHelp\n"));

    DC_DragCompletionStatus(w) = XmDROP_HELP;

    drag_finish(w, event);
}

/* 
 * We have to allowevents here or else if someone is dragging with
 * button two down and hits button one, we freeze.
 */
static void
_XmDCIgnore(Widget w,
	    XEvent *event,
	    String *params,
	    Cardinal *num_params)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "_XmDCIgnore\n"));

    XAllowEvents(XtDisplay(w), SyncPointer, event->xbutton.time);
}

/*
 * array is dragInitiator (7) by dragReceiver (6).  Matrix comes from
 * 1-371 in OSF/Motif Programmer's Reference
 * Order is:
 *  XmDRAG_NONE,
 *  XmDRAG_DROP_ONLY,
 *  XmDRAG_PREFER_PREREGISTER,
 *  XmDRAG_PREREGISTER,
 *  XmDRAG_PREFER_DYNAMIC,
 *  XmDRAG_DYNAMIC,
 *  XmDRAG_PREFER_RECEIVER
 */
static unsigned char
  protocolStyle[XmDRAG_PREFER_RECEIVER + 1][XmDRAG_DYNAMIC + 1] =
{
    /* dragInitiator is XmDRAG_NONE */
    {XmDRAG_NONE, XmDRAG_NONE, XmDRAG_NONE,
     XmDRAG_NONE, XmDRAG_NONE, XmDRAG_NONE},

    /* dragInitiator is XmDRAG_DROP_ONLY */
    {XmDRAG_NONE, XmDRAG_DROP_ONLY, XmDRAG_DROP_ONLY,
     XmDRAG_DROP_ONLY, XmDRAG_DROP_ONLY, XmDRAG_DROP_ONLY},

    /* dragInitiator is XmDRAG_PREFER_PREREGISTER */
    {XmDRAG_NONE, XmDRAG_DROP_ONLY, XmDRAG_PREREGISTER,
     XmDRAG_PREREGISTER, XmDRAG_PREREGISTER, XmDRAG_DYNAMIC},

    /* dragInitiator is XmDRAG_PREREGISTER */
    {XmDRAG_NONE, XmDRAG_DROP_ONLY, XmDRAG_PREREGISTER,
     XmDRAG_PREREGISTER, XmDRAG_PREREGISTER, XmDRAG_DROP_ONLY},

    /* dragInitiator is XmDRAG_PREFER_DYNAMIC */
    {XmDRAG_NONE, XmDRAG_DROP_ONLY, XmDRAG_DYNAMIC,
     XmDRAG_PREREGISTER, XmDRAG_DYNAMIC, XmDRAG_DYNAMIC},

    /* dragInitiator is XmDRAG_DYNAMIC */
    {XmDRAG_NONE, XmDRAG_DROP_ONLY, XmDRAG_DYNAMIC,
     XmDRAG_DROP_ONLY, XmDRAG_DYNAMIC, XmDRAG_DYNAMIC},

    /* dragInitiator is XmDRAG_PREFER_RECEIVER */
    {XmDRAG_NONE, XmDRAG_DROP_ONLY, XmDRAG_PREREGISTER,
     XmDRAG_PREREGISTER, XmDRAG_DYNAMIC, XmDRAG_DYNAMIC},
};

unsigned char
_XmGetActiveProtocolStyle(Widget w)
{
    unsigned char dis, drs;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmGetActiveProtocolStyle\n"));

    dis = Display_DragInitiatorProtocolStyle(_XmDCtoDD(w));
    drs = Display_DragReceiverProtocolStyle(_XmDCtoDD(w));

    if (!DC_SourceIsExternal(w))
    {
	if (DC_CurrReceiverInfo(w))
	{
	    drs = DC_CurrReceiverInfo(w)->dragProtocolStyle;
	}

	return protocolStyle[dis][drs];
    }
    else
    {
	/* unless and until I can figure out preregister, this is going
	 * to stay this way. */
	switch (drs)
	{
	case XmDRAG_DROP_ONLY:
	case XmDRAG_PREFER_PREREGISTER:
	case XmDRAG_PREREGISTER:
	case XmDRAG_PREFER_DYNAMIC:
	case XmDRAG_DYNAMIC:
	    return XmDRAG_DYNAMIC;

	case XmDRAG_NONE:
	default:
	    return XmDRAG_NONE;
	}
    }
}

XmDragReceiverInfo
_XmAllocReceiverInfo(XmDragContext dc)
{
    XmDragReceiverInfo ri;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmAllocReceiverInfo\n"));

	
#if 1 /* heiko/mark */
    if (DC_ReceiverInfos(dc) == NULL)
#else
    if (DC_CurrReceiverInfo(dc) == NULL)
#endif
    {
	DC_ReceiverInfos(dc) =
	    (XmDragReceiverInfo)XtMalloc(sizeof(XmDragReceiverInfoStruct)
					 * 8);
	DC_MaxReceiverInfos(dc) = 8;
    }

    if (DC_NumReceiverInfos(dc) == DC_MaxReceiverInfos(dc))
    {
	DC_MaxReceiverInfos(dc) <<= 1;
	DC_ReceiverInfos(dc) =
	    (XmDragReceiverInfo)XtRealloc((char *)DC_ReceiverInfos(dc),
					  sizeof(XmDragReceiverInfoStruct)
					  * DC_MaxReceiverInfos(dc));
    }

    ri = &DC_ReceiverInfos(dc)[DC_NumReceiverInfos(dc)];
    DC_NumReceiverInfos(dc)++;

    return ri;
}

Widget
XmDragStart(Widget widget,
	    XEvent *event,
	    ArgList arglist,
	    Cardinal argcount)
{
    Widget dc = NULL;
    Widget disp = XmGetXmDisplay(XtDisplay(widget));
    Arg arg[1];
    ArgList merged;

    DEBUGOUT(_LtDebug(__FILE__, widget, "%s:XmDragStart(%d) - %s\n",
    	__FILE__, __LINE__,
    	_LtDebugDragType2String(Display_DragInitiatorProtocolStyle(disp))));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, widget, arglist, argcount, False));
    DEBUGOUT(_LtDebug("DRAGSOURCE", widget, "%s:XmDragStart(%d) - %s\n",
    	__FILE__, __LINE__,
    	_LtDebugDragType2String(Display_DragInitiatorProtocolStyle(disp))));
    DEBUGOUT(_LtDebugPrintArgList("DRAGSOURCE", widget, arglist, argcount, False));

    if (Display_DragInitiatorProtocolStyle(disp) != XmDRAG_NONE)
    {
	/* appears in a dump of DragC's resources, but isn't documented. */
	XtSetArg(arg[0], XmNsourceWidget, widget);

	if (arglist)
	{
	    merged = XtMergeArgLists(arglist, argcount, arg, 1);
	}
	else
	{
	    merged = arg;
	}

	dc = XtCreateWidget("drag_context",
			    Display_DragContextClass(disp), disp,
			    merged, argcount + 1);

	if (arglist)
	{
	    XtFree((char *)merged);
	}

	_XmDragStart((XmDragContext)dc, widget, event);
    }

    return dc;
}

void
XmDragCancel(Widget dragcontext)
{
    DEBUGOUT(_LtDebug(__FILE__, dragcontext, "XmDragCancel()\n"));

    _XmDragCancel((XmDragContext)dragcontext);
}

Boolean
XmTargetsAreCompatible(Display *dpy,
		       Atom *exportTargets, Cardinal numExportTargets,
		       Atom *importTargets, Cardinal numImportTargets)
{
    Cardinal i, j;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "XmTargetsAreCompatible()\n"));

    /* if ANY import targets matches any export target, they're compat */
    for (i = 0; i < numExportTargets; i++)
    {
	for (j = 0; j < numImportTargets; j++)
	{
	    if (exportTargets[i] == importTargets[j])
	    {
		return True;
	    }
	}
    }

    return False;
}
