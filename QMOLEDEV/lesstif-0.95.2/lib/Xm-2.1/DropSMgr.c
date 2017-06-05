/*
 *
 * $Id: DropSMgr.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
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

static const char rcsid[] = "$Id: DropSMgr.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#include <LTconfig.h>

#include <XmI/XmI.h>

#include <Xm/DropSMgrP.h>
#include <Xm/DisplayP.h>
#include <XmI/DragDropI.h>
#include <Xm/DragCP.h>
#include <Xm/DropTrans.h>
#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>

#include <XmI/DebugUtil.h>


#define HASH_BUCKETS	127


/* Forward Declarations */

static void __XmDSMCreateInfo(XmDropSiteManagerObject dsm,
			      Widget widget,
			      ArgList args,
			      Cardinal num_args);
static void __XmDSMDestroyInfo(XmDropSiteManagerObject dsm,
			       Widget widget);
static void __XmDSMStartUpdate(XmDropSiteManagerObject dsm,
			       Widget widget);
static void __XmDSMRetrieveInfo(XmDropSiteManagerObject dsm,
				Widget widget,
				ArgList args,
				Cardinal num_args);
static void __XmDSMUpdateInfo(XmDropSiteManagerObject dsm,
			      Widget widget,
			      ArgList args,
			      Cardinal num_args);
static void __XmDSMEndUpdate(XmDropSiteManagerObject dsm,
			     Widget widget);
static void __XmDSMUpdateDSM(XmDropSiteManagerObject dsm,
			     XtPointer clientData,
			     XtPointer callData);
static void __XmDSMProcessMotion(XmDropSiteManagerObject dsm,
				 XtPointer clientData,
				 XtPointer callData);
static void __XmDSMProcessDrop(XmDropSiteManagerObject dsm,
			       XtPointer clientData,
			       XtPointer callData);
static void __XmDSMOperationChanged(XmDropSiteManagerObject dsm,
				    XtPointer clientData,
 			    XtPointer callData);
static void __XmDSMChangeRoot(XmDropSiteManagerObject dsm,
			      XtPointer clientData,
			      XtPointer callData);
static void __XmDSMInsertInfo(XmDropSiteManagerObject dsm,
			      XtPointer clientData,
			      XtPointer callData);
static void __XmDSMRemoveInfo(XmDropSiteManagerObject dsm,
			      XtPointer info);
static void __XmDSMSyncTree(XmDropSiteManagerObject dsm,
			    Widget shell);
static int __XmDSMGetTree(XmDropSiteManagerObject dsm,
			  Widget shell,
			  XtPointer dataPtr);
static void __XmDSMCreateDSInfoTable(XmDropSiteManagerObject dsm);
static void __XmDSMDestroyDSInfoTable(XmDropSiteManagerObject dsm);
static void __XmDSMRegisterInfo(XmDropSiteManagerObject dsm,
				Widget widget,
				XtPointer info);
static XtPointer __XmDSMWidgetToInfo(XmDropSiteManagerObject dsm,
				     Widget widget);
static void __XmDSMUnregisterInfo(XmDropSiteManagerObject dsm,
				  XtPointer info);
static void class_initialize(void);
static void class_part_initialize(WidgetClass widget_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
static void updateTree(Widget w, XtPointer cd, XtPointer cbs);

#define Offset(f)	XtOffsetOf(XmDropSiteManagerRec, dropManager.f)
static XtResource resources[] =
{
    {
	XmNnotifyProc, XmCNotifyProc, XmRCallbackProc,
	sizeof(XtCallbackList), Offset(notifyProc),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNtreeUpdateProc, XmCTreeUpdateProc, XmRCallbackProc,
	sizeof(XtCallbackList), Offset(treeUpdateProc),
	XmRImmediate, (XtPointer)NULL
    },
    {
	XmNclientData, XmCClientData, XmRPointer,
	sizeof(XtPointer), Offset(client_data),
	XmRImmediate, (XtPointer)NULL
    }
};
#undef Offset


#if 0
static XmBaseClassExtRec _XmDropSMgrObjectClassExtRec = {
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

XmDropSiteManagerClassRec xmDropSiteManagerClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &objectClassRec,
        /* class_name            */ "XmDropSiteManager",
	/* widget_size           */ sizeof(XmDropSiteManagerRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* obj1                  */ NULL, /* Motif has one of these */
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
	/* extension             */ (XtPointer)NULL /*&_XmDropSMgrObjectClassExtRec*/
    },
    /* Drop site manager class part */
    {
	/* createInfo       */  __XmDSMCreateInfo,
	/* destroyInfo      */  __XmDSMDestroyInfo,
	/* startUpdate      */  __XmDSMStartUpdate,
	/* retrieveInfo     */  __XmDSMRetrieveInfo,
	/* updateInfo       */  __XmDSMUpdateInfo,
	/* endUpdate        */  __XmDSMEndUpdate,
	/* updateDSM        */  __XmDSMUpdateDSM,
	/* processMotion    */  __XmDSMProcessMotion,
	/* processDrop      */  __XmDSMProcessDrop,
	/* operationChanged */  __XmDSMOperationChanged,
	/* changeRoot       */  __XmDSMChangeRoot,
	/* insertInfo       */  __XmDSMInsertInfo,
	/* removeInfo       */  __XmDSMRemoveInfo,
	/* syncTree         */  __XmDSMSyncTree,
	/* getTreeFromDSM   */  __XmDSMGetTree,
	/* createTable      */  __XmDSMCreateDSInfoTable,
	/* destroyTable     */  __XmDSMDestroyDSInfoTable,
	/* registerInfo     */  __XmDSMRegisterInfo,
	/* widgetToInfo     */  __XmDSMWidgetToInfo,
	/* unregisterInfo   */  __XmDSMUnregisterInfo,
	/* extension        */  NULL
    }
};


WidgetClass xmDropSiteManagerObjectClass = (WidgetClass)(&xmDropSiteManagerClassRec);

/*
 * Creates a drop site for the associated widget (perhaps)
 */

static void
class_initialize(void)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:class_initialize(%d)\n",
    	__FILE__, __LINE__));
#if 0
    _XmDropSMgrObjectClassExtRec.record_type = XmQmotif;
#endif
}


static void
class_part_initialize(WidgetClass widget_class)
{
    WidgetClass sc;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:class_part_initialize(%d)\n",
    	__FILE__, __LINE__));
    _XmFastSubclassInit(widget_class, XmDROP_SITE_MANAGER_BIT);

    sc = widget_class->core_class.superclass;

    if (DSC_CreateInfoProc(widget_class) == XmInheritCreateInfoProc)
    {
	DSC_CreateInfoProc(widget_class) = DSC_CreateInfoProc(sc);
    }

    if (DSC_DestroyInfoProc(widget_class) == XmInheritDestroyInfoProc)
    {
	DSC_DestroyInfoProc(widget_class) = DSC_DestroyInfoProc(sc);
    }

    if (DSC_StartUpdateProc(widget_class) == XmInheritStartUpdateProc)
    {
	DSC_StartUpdateProc(widget_class) = DSC_StartUpdateProc(sc);
    }

    if (DSC_RetrieveInfoProc(widget_class) == XmInheritRetrieveInfoProc)
    {
	DSC_RetrieveInfoProc(widget_class) = DSC_RetrieveInfoProc(sc);
    }

    if (DSC_UpdateInfoProc(widget_class) == XmInheritUpdateInfoProc)
    {
	DSC_UpdateInfoProc(widget_class) = DSC_UpdateInfoProc(sc);
    }

    if (DSC_EndUpdateProc(widget_class) == XmInheritEndUpdateProc)
    {
	DSC_EndUpdateProc(widget_class) = DSC_EndUpdateProc(sc);
    }

    if (DSC_UpdateProc(widget_class) == XmInheritUpdateProc)
    {
	DSC_UpdateProc(widget_class) = DSC_UpdateProc(sc);
    }

    if (DSC_ProcessMotionProc(widget_class) == XmInheritProcessMotionProc)
    {
	DSC_ProcessMotionProc(widget_class) = DSC_ProcessMotionProc(sc);
    }

    if (DSC_ProcessDropProc(widget_class) == XmInheritProcessDropProc)
    {
	DSC_ProcessDropProc(widget_class) = DSC_ProcessDropProc(sc);
    }

    if (DSC_OperationChangedProc(widget_class) == XmInheritOperationChangedProc)
    {
	DSC_OperationChangedProc(widget_class) = DSC_OperationChangedProc(sc);
    }

    if (DSC_ChangeRootProc(widget_class) == XmInheritChangeRootProc)
    {
	DSC_ChangeRootProc(widget_class) = DSC_ChangeRootProc(sc);
    }

    if (DSC_InsertInfoProc(widget_class) == XmInheritInsertInfoProc)
    {
	DSC_InsertInfoProc(widget_class) = DSC_InsertInfoProc(sc);
    }

    if (DSC_RemoveInfoProc(widget_class) == XmInheritRemoveInfoProc)
    {
	DSC_RemoveInfoProc(widget_class) = DSC_RemoveInfoProc(sc);
    }

    if (DSC_SyncTreeProc(widget_class) == XmInheritSyncTreeProc)
    {
	DSC_SyncTreeProc(widget_class) = DSC_SyncTreeProc(sc);
    }

    if (DSC_GetTreeFromDSMProc(widget_class) == XmInheritGetTreeFromDSMProc)
    {
	DSC_GetTreeFromDSMProc(widget_class) = DSC_GetTreeFromDSMProc(sc);
    }

    if (DSC_CreateDSInfoTable(widget_class) == XmInheritCreateDSInfoTable)
    {
	DSC_CreateDSInfoTable(widget_class) = DSC_CreateDSInfoTable(sc);
    }

    if (DSC_DestroyDSInfoTable(widget_class) == XmInheritDestroyDSInfoTable)
    {
	DSC_DestroyDSInfoTable(widget_class) = DSC_DestroyDSInfoTable(sc);
    }

    if (DSC_RegisterInfoProc(widget_class) == XmInheritRegisterInfoProc)
    {
	DSC_RegisterInfoProc(widget_class) = DSC_RegisterInfoProc(sc);
    }

    if (DSC_WidgetToInfoProc(widget_class) == XmInheritWidgetToInfoProc)
    {
	DSC_WidgetToInfoProc(widget_class) = DSC_WidgetToInfoProc(sc);
    }

    if (DSC_UnregisterInfoProc(widget_class) == XmInheritUnregisterInfoProc)
    {
	DSC_UnregisterInfoProc(widget_class) = DSC_UnregisterInfoProc(sc);
    }
}


static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    XmDropSiteInfoRec hr;

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

    DS_CurAnimate(new_w) = True;

    DS_CurDropSiteStatus(new_w) = XmINVALID_DROP_SITE;
    DS_CurOperations(new_w) = 0;
    DS_CurOperation(new_w) = 0;

    DS_CurX(new_w) = 0;
    DS_CurY(new_w) = 0;
    DS_OldX(new_w) = 0;
    DS_OldY(new_w) = 0;

    DS_CurDragContext(new_w) = NULL;
    DS_CurTime(new_w) = CurrentTime;
    DS_CurInfo(new_w) = NULL;
    DS_DragUnderData(new_w) = NULL;

    DS_CurAncestorClipRegion(new_w) = _XmRegionCreate();
    DS_NewAncestorClipRegion(new_w) = _XmRegionCreate();

    DSMCreateTable((XmDropSiteManagerObject)new_w);

    DS_RootX(new_w) = 0;
    DS_RootY(new_w) = 0;

    DS_RootWidth(new_w) = XmUNSPECIFIED;
    DS_RootHeight(new_w) = XmUNSPECIFIED;

    DS_DSRoot(new_w) = NULL;
    DS_ClipperList(new_w) = NULL;
    DS_UpdateInfo(new_w) = NULL;

    /* compile the resources */
    XtGetSubresources(new_w, &hr, NULL, NULL,
		      _XmDSResources, _XmNumDSResources,
		      NULL, 0);

    if (DS_TreeUpdateProc(new_w) == NULL)
	DS_TreeUpdateProc(new_w) = updateTree;

}


static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:destroy(%d)\n",
    	__FILE__, __LINE__));
    DSMDestroyTable((XmDropSiteManagerObject)w);

    _XmRegionDestroy(DS_CurAncestorClipRegion(w));
    _XmRegionDestroy(DS_NewAncestorClipRegion(w));
}


static Boolean
set_values(Widget old, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
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

    return True;
}


static void
updateReceiverInfo(Widget w, XtPointer cd, XEvent *ev, Boolean *ctd)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "%s:updateReceiverInfo(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSINK", w, "%s:updateReceiverInfo(%d) - %s\n",
    	__FILE__, __LINE__,
    	_LtDebugEventType2String(ev->xany.type)));
#if 1
    /* rws 8 Feb 2000
       I have no idea if this belongs here or not, but it has to be
       created at some point.....
     */
    /* rws 12 Feb 2000
       This should probably be DragBS.c:_XmSetDragReceiverInfo()
     */
    {
    XmDndReceiverProp ri;
    Atom pa;
    Widget shell = w;

	DEBUGOUT(_LtDebug("DND", shell, "%s:createShellInfoRec(%d)\n",
	    __FILE__, __LINE__));
    	ri.byte_order = _XmByteOrder();
    	ri.protocol_version = DND_PROTOCOL_VERSION;
    	ri.protocol_style = XmDRAG_DYNAMIC;
    	ri.proxy_window = (CARD32)None;
    	ri.num_drop_sites = (CARD32)0;
    	ri.total_size = (CARD32)sizeof(XmDndReceiverProp);
	pa = XmInternAtom(XtDisplay(shell), _XA_MOTIF_DRAG_RECEIVER_INFO, False);
	XChangeProperty(XtDisplay(shell), XtWindow(shell), 
		pa, pa, 8, PropModeReplace,
		(unsigned char *)&ri, sizeof(ri));
    }
#endif
}


static void
SwapMessageData(XmDndMessage *msg)
{
    if (msg->byte_order != _XmByteOrder())
    {
	SWAP2BYTES(msg->flags);
	SWAP4BYTES(msg->time);
	switch (msg->reason & DND_CLEAR_EVENT_TYPE)
	{
	case XmTOP_LEVEL_ENTER:
	case XmTOP_LEVEL_LEAVE:
	    SWAP4BYTES(msg->data.top.property);
	    SWAP4BYTES(msg->data.top.src_window);
	    break;
	case XmDROP_SITE_ENTER:
	case XmDRAG_MOTION:
	case XmDROP_START:
	    SWAP2BYTES(msg->data.pot.x);
	    SWAP2BYTES(msg->data.pot.y);
	    SWAP4BYTES(msg->data.pot.property);
	    SWAP4BYTES(msg->data.pot.src_window);
	    break;
	case XmDROP_SITE_LEAVE:
	case XmDROP_FINISH:
	case XmDRAG_DROP_FINISH:
	case XmOPERATION_CHANGED:
	    break;
	default:
	    _XmWarning(NULL, "Unexpected DnD message type >%i<\n    %s:SwapMessageData(%d)", 
		msg->reason, __FILE__, __LINE__);
	    break;
	}
    }
}


/* for Motif BC ?! */
extern int
_XmMessageTypeToReason(BYTE reason)
{
    switch (reason & DND_CLEAR_EVENT_TYPE)
    {
    case XmTOP_LEVEL_ENTER:
	return(XmCR_TOP_LEVEL_ENTER);
	break;
    case XmTOP_LEVEL_LEAVE:
	return(XmCR_TOP_LEVEL_LEAVE);
	break;
    case XmDROP_SITE_ENTER:
	return(XmCR_DROP_SITE_ENTER);
	break;
    case XmDRAG_MOTION:
	return(XmCR_DRAG_MOTION);
	break;
    case XmDROP_START:
	return(XmCR_DROP_START);
	break;
    case XmDROP_SITE_LEAVE:
	return(XmCR_DROP_SITE_LEAVE);
	break;
    case XmDROP_FINISH:
	return(XmCR_DROP_FINISH);
	break;
    case XmDRAG_DROP_FINISH:
	return(XmCR_DRAG_DROP_FINISH);
	break;
    case XmOPERATION_CHANGED:
	return(XmCR_OPERATION_CHANGED);
	break;
    default:
	_XmWarning(NULL, "Unexpected DnD message type >%i<\n    %s:_XmMessageTypeToReason(%d)", 
	    reason, __FILE__, __LINE__);
	return(reason);
	break;
    }
}


/* for Motif BC ? */
extern void
_XmICCEventToICCCallback(XmDndMessage *msg, XmDragDropCallbackStruct *cbs)
{
    switch (msg->reason & DND_CLEAR_EVENT_TYPE)
    {
    case XmTOP_LEVEL_ENTER:
	/*
	cbs->tle.screen = 
	cbs->tle.x =
	cbs->tle.y =
	cbs->tle.dragProtocolStyle =
	*/
	cbs->tle.window = msg->data.top.src_window;
	cbs->tle.iccHandle = msg->data.top.property;
	break;
    case XmTOP_LEVEL_LEAVE:
	/*
	cbs->tll.screen = 
	*/
	cbs->tle.window = msg->data.top.src_window;
	break;
    case XmDROP_SITE_ENTER:
	cbs->dse.operation = DND_GET_OPERATION(msg->flags);
	cbs->dse.operations = DND_GET_OPERATIONS(msg->flags);
	cbs->dse.dropSiteStatus = DND_GET_STATUS(msg->flags);
	cbs->dse.x = msg->data.pot.x;
	cbs->dse.y = msg->data.pot.y;
	break;
    case XmDRAG_MOTION:
	cbs->dm.operation = DND_GET_OPERATION(msg->flags);
	cbs->dm.operations = DND_GET_OPERATIONS(msg->flags);
	cbs->dm.dropSiteStatus = DND_GET_STATUS(msg->flags);
	cbs->dm.x = msg->data.pot.x;
	cbs->dm.y = msg->data.pot.y;
	break;
    case XmDROP_START:
	cbs->ds.operation = DND_GET_OPERATION(msg->flags);
	cbs->ds.operations = DND_GET_OPERATIONS(msg->flags);
	cbs->ds.dropSiteStatus = DND_GET_STATUS(msg->flags);
	cbs->ds.dropAction = DND_GET_COMPLETION(msg->flags);
	cbs->ds.x = msg->data.pot.x;
	cbs->ds.y = msg->data.pot.y;
	cbs->ds.iccHandle = msg->data.pot.property;
	cbs->ds.window = msg->data.pot.src_window;
	break;
    case XmDROP_SITE_LEAVE:
	/* nothing to do */
	break;
    case XmOPERATION_CHANGED:
	cbs->oc.operation = DND_GET_OPERATION(msg->flags);
	cbs->oc.operations = DND_GET_OPERATIONS(msg->flags);
	cbs->oc.dropSiteStatus = DND_GET_STATUS(msg->flags);
	break;
    default:
	_XmWarning(NULL, "Unexpected DnD message type >%i<\n    %s:_XmICCEventToICCCallback(%d)", 
	    msg->reason, __FILE__, __LINE__);
	break;
    }
}


static void
externalSourceHandler(Widget w, XtPointer cd, XEvent *ev, Boolean *ctd)
{
    DEBUGOUT(_LtDebug2(__FILE__, w, (Widget)cd, "%s:externalSourceHandler(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug2("DRAGSINK", w, (Widget)cd, "%s:externalSourceHandler(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug0("DRAGSINK", w, "\t%s\n",
    	_LtDebugEventType2String(ev->xany.type)));

    switch (ev->xany.type)
    {
    case ClientMessage:
	{
	Atom DndMessage;

	    DndMessage = XmInternAtom(ev->xclient.display, _XA_MOTIF_DRAG_AND_DROP_MESSAGE, False);
	    if (DndMessage == ev->xclient.message_type)
	    {
	    XmDndMessage *msg = (XmDndMessage *)&(ev->xclient.data.b[0]);
	    XmDropSiteManagerObject dsm = (XmDropSiteManagerObject) cd;
	    XmDisplay disp = (XmDisplay)XmGetXmDisplay(ev->xclient.display);
	    XmDragDropCallbackStruct DnDcbs;
	    XmDragTopLevelClientDataStruct TLCcbs;

	    	SwapMessageData(msg);
		DEBUGOUT(_LtDebug0("DRAGSINK", w, "\t%s from %s\n",
		    _LtDebugDragAndDropMessageType2String(msg->reason & DND_CLEAR_EVENT_TYPE),
		    msg->reason & ~DND_CLEAR_EVENT_TYPE ? "Receiver" : "Initiator"));
		DEBUGOUT(_LtDebug0("DRAGSINK", w, "\tDS_CurDragContext %s %p\n",
		    DS_CurDragContext(dsm) ? XtName(DS_CurDragContext(dsm)) : "NULL", DS_CurDragContext(dsm)));
		DEBUGOUT(_LtDebug0("DRAGSINK", w, "\tDisplay_ActiveDC %s %p\n",
		    Display_ActiveDC(disp) ? XtName(Display_ActiveDC(disp)) : "NULL", Display_ActiveDC(disp)));
		DEBUGOUT(_LtDebug0("DRAGSINK", w, "\tXmGetDragContext %s %p\n",
		    XmGetDragContext((Widget)dsm, msg->time) ? XtName(XmGetDragContext((Widget)dsm, msg->time)) : "NULL", XmGetDragContext((Widget)dsm, msg->time)));

		DnDcbs.any.reason = _XmMessageTypeToReason(msg->reason);
		DnDcbs.any.event = ev;
		DnDcbs.any.timeStamp = msg->time;
		_XmICCEventToICCCallback(msg, &DnDcbs);
		TLCcbs.shell = w;
		TLCcbs.sourceIsExternal = True;
		TLCcbs.window = ev->xclient.window;
		TLCcbs.xOrigin = XtX(w);
		TLCcbs.yOrigin = XtY(w);
		TLCcbs.width = XtWidth(w);
		TLCcbs.height = XtHeight(w);
		DS_CurDragContext(dsm) = XmGetDragContext((Widget)dsm,
						  DnDcbs.any.timeStamp);
		DSMUpdate(dsm, (XtPointer)&TLCcbs, (XtPointer)&DnDcbs);
	    }
	    else
	    {
	    /* rws 28 Mar 2001
	       Normal situation
	    char *AtomName;

		AtomName = XGetAtomName(ev->xclient.display, ev->xclient.message_type);
		_XmWarning(w, "Unexpected ClientMessaget, type >%s<\n    %s:externalSourceHandler(%d)", 
		    AtomName, __FILE__, __LINE__);
		XtFree(AtomName);
	    */
	    }
	}
	break;
    /* rws 28 Mar 2001
       Normal situation
    case SelectionClear:
	{
	char *AtomName;

	    AtomName = XGetAtomName(ev->xselectionclear.display, ev->xselectionclear.selection);
	    _XmWarning(w, "Unexpected SelectionClear event, losing %s selection\n    %s:externalSourceHandler(%d)", 
	    	AtomName, __FILE__, __LINE__);
	    XtFree(AtomName);
	}
	break;
    */
    default:
    	/*
    	_XmWarning(w, "Unexpected event >%s<\n    %s:externalSourceHandler(%d)", _LtDebugEventType2String(ev->xany.type), __FILE__, __LINE__);
    	*/
    	break;
    }
#if 0
    Widget dc, disp;
    XmDropSiteManagerObject dsm;

    DEBUGOUT(_LtDebug(__FILE__, w, "External drag event seen\n"));

    disp = XmGetXmDisplay(XtDisplay(w));

    dsm = _XmGetDropSiteManagerObject((XmDisplay)disp);

    if (Display_ActiveDC(disp) != NULL)
    {
	return;
    }

    w = XtVaCreateWidget("ExternalDragC", xmDragContextClass, disp,
			 XmNsourceIsExternal, True, NULL);
#endif
}


/*
 * this should be called with a shell widget.
 */
static void
updateTree(Widget w, XtPointer cd, XtPointer cbs)
{
    Widget disp = XmGetXmDisplay(XtDisplay(w));
    XmTreeUpdateCallbackStruct *tucbs = (XmTreeUpdateCallbackStruct *) cbs;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:updateTree(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSINK", w, "%s:updateTree(%d)\n",
    	__FILE__, __LINE__));

    if (Display_DragReceiverProtocolStyle(disp) == XmDRAG_NONE)
    {
	return;
    }

    if (tucbs->reason == XmCR_ADD_DROP_SITE)
    {
	if (XtIsRealized(tucbs->widget))
	{
	    _XmSetDragReceiverInfo(w, tucbs->widget);
	}
	else
	{
	    XtAddEventHandler(tucbs->widget,
			      StructureNotifyMask,
			      False,
			      updateReceiverInfo,
			      (XtPointer)tucbs->widget);
	}

	XtAddEventHandler(tucbs->widget,
			  NoEventMask,
			  True,
			  externalSourceHandler,
			  (XtPointer)w);

    }
    else if (tucbs->reason == XmCR_REMOVE_DROP_SITE)
    {
	XtRemoveEventHandler(tucbs->widget,
			     NoEventMask,
			     True,
			     externalSourceHandler,
			     (XtPointer)w);

	if (XtIsRealized(tucbs->widget))
	{
	    _XmClearDragReceiverInfo(tucbs->widget);
	}
    }
}


static void
destroyInfo(Widget w, XtPointer cd, XtPointer cbs)
{
    Widget dsm = (Widget)cd;

    DEBUGOUT(_LtDebug(__FILE__, w, "%s:destroyInfo(%d)\n",
    	__FILE__, __LINE__));
    DSMDestroyInfo((XmDropSiteManagerObject)dsm, w);
}


static XmDropSiteInfo
createShellInfoRec(XmDropSiteManagerObject dsm, Widget shell)
{
    XmRegion r = _XmRegionCreate();
    XmDropSiteInfo dsi;
    XRectangle rect;

    DEBUGOUT(_LtDebug(__FILE__, shell, "%s:createShellInfoRec(%d)\n",
    	__FILE__, __LINE__));
    dsi = (XmDropSiteInfo) XtCalloc(1, sizeof(XmDropSiteInfoRec));

    dsi->leaf = True;
    dsi->isShell = 1;

    dsi->animationStyle = XmDRAG_UNDER_NONE;
    dsi->dropSiteType = XmDROP_SITE_COMPOSITE;
    dsi->implicit = True;
    dsi->dropSiteActivity = XmDROP_SITE_INACTIVE;

    dsi->dropSite = shell;

    rect.x = 0;
    rect.y = 0;
    rect.width = XtWidth(shell);
    rect.height = XtHeight(shell);

    _XmRegionUnionRectWithRegion(&rect, r, r);

    dsi->region = r;

    XtAddCallback(shell, XmNdestroyCallback, destroyInfo, (XtPointer)dsm);

    return dsi;
}


static void
addChildToComposite(XmDropSiteInfo pi, XmDropSiteInfo ci, short pos)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:addChildToComposite(%d)\n",
    	__FILE__, __LINE__));

    if (pi == NULL || ci == NULL)
	return;

    if (pi->dropSiteType != XmDROP_SITE_COMPOSITE)
    {
	_XmWarning(pi->dropSite,
		   "Can't add child to simple dropSite.");
	return;
    }

    if (pos > pi->numChildren)
    {
	_XmWarning(pi->dropSite,
		   "Position error adding child to composite dropSite.\n");

	pos = pi->numChildren;
    }

    if (pi->numChildren == pi->maxChildren)
    {

	if (pi->maxChildren == 0)
	{

	    pi->maxChildren = 8;

	    pi->children =
		(XmDropSiteInfo *) XtCalloc(pi->maxChildren,
					    sizeof(XmDropSiteInfo));
	}
	else
	{

	    pi->maxChildren <<= 1;

	    pi->children =
		(XmDropSiteInfo *) XtRealloc((char *)pi->children,
				     pi->maxChildren * sizeof(XmDropSiteInfo));
	}
    }

    if (pi->numChildren)
    {

	/* T. Straumann: use mmove() !! (overlap) 
	 * NOTE: src/dest swapped
	 */
	memmove(&pi->children[pos + 1],&pi->children[pos],
	        (pi->numChildren - pos) * sizeof(XmDropSiteInfo));
    }

    pi->children[pos] = ci;
    pi->numChildren++;

    ci->parent = pi;

    pi->leaf = False;
}


static void
removeChildFromComposite(XmDropSiteInfo pi, XmDropSiteInfo dsi)
{
    int i;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:removeChildFromComposite(%d)\n",
    	__FILE__, __LINE__));

    if (pi->dropSiteType != XmDROP_SITE_COMPOSITE)
    {
	return;
    }

    for (i = 0; i < pi->numChildren; i++)
    {
	if (pi->children[i] == dsi)
	    break;
    }

    if (i == pi->numChildren)
    {
	return;
    }

    if (i != pi->numChildren - 1)
    {
	/* T. Straumann: use mmove() !! (overlap) 
	 * NOTE: src/dest swapped
	 */
	memmove(&pi->children[i], &pi->children[i + 1],
	      (pi->numChildren - i - 1)*sizeof(XmDropSiteInfo));
    }

    pi->numChildren--;
}


static void
destroyInfoRec(XmDropSiteInfo dsi, Boolean thorough)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:destroyInfoRec(%d)\n",
    	__FILE__, __LINE__));

    if (dsi->dropSiteType == XmDROP_SITE_COMPOSITE && thorough &&
	dsi->children != 0)
    {
	XtFree((char *)dsi->children);
    }

    if (!dsi->userRegion)
    {
	_XmRegionDestroy(dsi->region);
    }

    if (thorough)
    {
	XtFree((char *)dsi);
    }
}


/*
 * create the per-widget information
 */
static void
__XmDSMCreateInfo(XmDropSiteManagerObject object,
		  Widget widget,
		  ArgList args,
		  Cardinal num_args)
{
    XmRegion r = _XmRegionCreate();
    XmDropSiteInfo hr;
    int x, y;
    Cardinal i;
    Window root;
    unsigned int width, height, border;
    XRectangle rect;
    Widget shell;
    XmDropSiteInfo info;

    DEBUGOUT(_LtDebug2(__FILE__, (Widget)object, widget,
		      "%s:__XmDSMCreateInfo(%d) - %i args\n", 
		      __FILE__, __LINE__,
		      num_args
		      ));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, (Widget)object, args, num_args, False));
    DEBUGOUT(_LtDebug2("DRAGSINK", (Widget)object, widget,
		      "%s:__XmDSMCreateInfo(%d) - %i args\n", 
		      __FILE__, __LINE__,
		      num_args
		      ));
    DEBUGOUT(_LtDebugPrintArgList("DRAGSINK", (Widget)object, args, num_args, False));

    DSMStartUpdate(object, widget);

    hr = (XmDropSiteInfo) XtCalloc(1, sizeof(XmDropSiteInfoRec));

    hr->dropSite = widget;
    hr->leaf = True;
    XtGetSubresources(widget, (XtPointer)hr, NULL, NULL,
		      _XmDSResources, _XmNumDSResources,
		      args, num_args);

    if (hr->dropSiteActivity == XmDROP_SITE_ACTIVE &&
	hr->dropProc == NULL)
    {
	_XmWarning(widget, "Active dropSite missing dropProc\n");
    }

    if (hr->animationStyle == XmDRAG_UNDER_PIXMAP &&
	hr->animationPixmap != None &&
        hr->animationPixmap != XmUNSPECIFIED_PIXMAP &&
	hr->animationPixmapDepth == 0)
    {
	XGetGeometry(XtDisplay(widget), hr->animationPixmap, &root,
		     &x, &y, &width, &height, &border,
		     &hr->animationPixmapDepth);
    }

    if (hr->dropSiteType == XmDROP_SITE_COMPOSITE &&
	(hr->dropRectangles != NULL || hr->numDropRectangles != 1))
    {
	_XmWarning(widget, "Can't set rectangles/numRectangles on composite.");
	hr->dropRectangles = NULL;
	hr->numDropRectangles = 1;
    }

    if (hr->dropRectangles == NULL)
    {
	rect.x = -XtBorderWidth(widget);
	rect.y = -XtBorderWidth(widget);
	rect.width = XtBorderWidth(widget) * 2 + XtWidth(widget);
	rect.height = XtBorderWidth(widget) * 2 + XtHeight(widget);

	_XmRegionUnionRectWithRegion(&rect, r, r);
    }
    else
    {
	for (i = 0; i < hr->numDropRectangles; i++)
	{
	    _XmRegionUnionRectWithRegion(&hr->dropRectangles[i], r, r);
	}

	hr->userRegion = True;
    }
    hr->region = r;

    XtAddCallback(widget, XmNdestroyCallback, destroyInfo, (XtPointer)object);

    shell = widget;
    while (shell && !XtIsShell(shell))
    {
	shell = XtParent(shell);
    }

    hr->target_index = _XmTargetsToIndex(shell,
					 hr->importTargets,
					 hr->numImportTargets);

    info = (XmDropSiteInfo) DSMWidgetToInfo(object, widget);

    if (info == NULL)
    {
	DSMRegisterInfo(object, widget, (XtPointer)hr);

	DSMInsertInfo(object, (XtPointer)hr, NULL);

	DSMEndUpdate(object, widget);
    }
    else
    {
	if (info->implicit)
	{
	    _XmWarning(widget,
		       "Registering a widget as a dropSite out of sequence.\n");
	}
	else
	{
	    _XmWarning(widget,
		       "Can't register widget as a dropSite more than once.");
	}

	destroyInfoRec(hr, True);
    }
}


static void
__XmDSMDestroyInfo(XmDropSiteManagerObject dsm,
		   Widget widget)
{
    XmDropSiteInfo dsi;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM DestroyInfo\n"));

    dsi = (XmDropSiteInfo) DSMWidgetToInfo(dsm, widget);

    if (!dsi)
    {
	_XmWarning(widget,
		   "Attempt to destroy dropSite info for widget that\n"
		   "hasn't been added as a dropSite.\n");
	return;
    }

    DSMStartUpdate(dsm, widget);

    if (dsi == (XmDropSiteInfo) DS_CurInfo(dsm))
    {
	DS_CurInfo(dsm) = NULL;
    }

    DSMRemoveInfo(dsm, (XtPointer)dsi);

    destroyInfoRec(dsi, True);

    DSMEndUpdate(dsm, widget);
}


static void
__XmDSMStartUpdate(XmDropSiteManagerObject dsm,
		   Widget widget)
{
    Widget shell = widget;
    XmDropSiteInfo info;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM StartUpdate\n"));

    while (!XtIsShell(shell))
    {
	shell = XtParent(shell);
    }

    info = (XmDropSiteInfo) DSMWidgetToInfo(dsm, shell);

    if (info == NULL)
    {
	return;
    }

    if (!info->isShell)
    {
	return;
    }

    info->inUpdate++;
}


static void
__XmDSMRetrieveInfo(XmDropSiteManagerObject dsm,
		    Widget widget,
		    ArgList args,
		    Cardinal num_args)
{
    XmDropSiteInfo info;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM RetrieveInfo\n"));

    if (XmIsDragContext(widget))
    {

	if (widget == DS_CurDragContext(dsm))
	{
	    info = (XmDropSiteInfo) DS_CurInfo(dsm);
	}
	else
	{
	    return;
	}
    }
    else
    {
	info = (XmDropSiteInfo) DSMWidgetToInfo(dsm, widget);
    }

    if (!info)
    {
	return;
    }

    XtGetSubvalues((XtPointer)info,
		   _XmDSResources, _XmNumDSResources,
		   args, num_args);
}


static void
__XmDSMUpdateInfo(XmDropSiteManagerObject dsm,
		  Widget widget,
		  ArgList args,
		  Cardinal num_args)
{
    XmDropSiteInfo dsi, ndsi;
    XmRegion r;
    int x, y;
    Cardinal i;
    Window root;
    unsigned int width, height, border;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM UpdateInfo\n"));

    dsi = (XmDropSiteInfo) DSMWidgetToInfo(dsm, widget);

    if (!dsi || dsi->implicit)
    {
	return;
    }

    DSMStartUpdate(dsm, widget);

    ndsi = (XmDropSiteInfo) XtMalloc(sizeof(XmDropSiteInfoRec));

    memcpy(ndsi, dsi, sizeof(XmDropSiteInfoRec));

    XtSetSubvalues((XtPointer)ndsi,
		   _XmDSResources, _XmNumDSResources,
		   args, num_args);

    /* from here, treat it like a normal set_values() call. */
    if (ndsi->dropSiteType != dsi->dropSiteType)
    {
	_XmWarning(widget, "Can't change DropSiteType after creation\n");
	ndsi->dropSiteType = dsi->dropSiteType;
    }

    if (ndsi->dropRectangles != dsi->dropRectangles ||
	ndsi->numDropRectangles != dsi->numDropRectangles)
    {

	if (ndsi->dropSiteType == XmDROP_SITE_SIMPLE)
	{
	    r = _XmRegionCreate();

	    for (i = 0; i < ndsi->numDropRectangles; i++)
	    {
		_XmRegionUnionRectWithRegion(&ndsi->dropRectangles[i], r, r);
	    }

	    ndsi->region = r;

	    _XmRegionDestroy(dsi->region);
	}
	else
	{
	    _XmWarning(widget, "Can't change rectangles for composite.\n");
	}
    }

    if (ndsi->animationStyle == XmDRAG_UNDER_PIXMAP &&
	ndsi->animationPixmap != None &&
        ndsi->animationPixmap != XmUNSPECIFIED_PIXMAP &&
        ndsi->animationPixmapDepth == 0)
    {
	XGetGeometry(XtDisplay(widget), ndsi->animationPixmap, &root,
		     &x, &y, &width, &height, &border,
		     &ndsi->animationPixmapDepth);
    }

    memcpy(dsi, ndsi, sizeof(XmDropSiteInfoRec));

    XtFree((char *)ndsi);

    DSMEndUpdate(dsm, widget);
}


static void
__XmDSMEndUpdate(XmDropSiteManagerObject dsm,
		 Widget widget)
{
    XmDropSiteInfo dsi;
    Widget shell;
    XmTreeUpdateCallbackStruct tucbs;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM EndUpdate\n"));

    shell = widget;

    while (!XtIsShell(shell))
    {
	shell = XtParent(shell);
    }

    dsi = (XmDropSiteInfo) DSMWidgetToInfo(dsm, shell);

    if (dsi == NULL)
    {
	return;
    }

    if (dsi->inUpdate > 0)
    {
	dsi->inUpdate--;
    }

    if (dsi->inUpdate == 0 && XtIsRealized(shell))
    {

	if (_XmGetDragProtocolStyle(shell) != XmDRAG_DYNAMIC)
	{

	    tucbs.reason = XmCR_ADD_DROP_SITE;
	    tucbs.event = NULL;
	    tucbs.widget = shell;

	    DS_TreeUpdateProc(dsm) ((Widget)dsm, NULL, &tucbs);
	}
	else
	{
	    DSMSyncTree(dsm, shell);
	}

    }
}


static void
__XmDSMUpdateDSM(XmDropSiteManagerObject dsm,
		 XtPointer clientData,
		 XtPointer callData)
{
    XmDragDropCallbackStruct *cbs = (XmDragDropCallbackStruct *) callData;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM UpdateDSM\n"));
    DEBUGOUT(_LtDebug("DRAGSINK", (Widget)dsm, "__XmDSMUpdateDSM\n"));

    switch (cbs->any.reason)
    {
    case XmCR_TOP_LEVEL_ENTER:
    case XmCR_TOP_LEVEL_LEAVE:
	DSMChangeRoot(dsm, clientData, callData);
	break;

    case XmCR_DRAG_MOTION:
	DSMProcessMotion(dsm, clientData, callData);
	break;

    case XmCR_DROP_START:
	DSMProcessDrop(dsm, clientData, callData);
	break;

    case XmCR_OPERATION_CHANGED:
	DSMOperationChanged(dsm, clientData, callData);
	break;

    default:
	DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm,
			  "DSMUpdate default: %d\n", cbs->any.reason));
	break;
    }
}


static Boolean
loc_in_info(XmDropSiteManagerObject dsm, XmDropSiteInfo dsi,
	    Position x, Position y)
{
    Position tx, ty;

    DEBUGOUT(_LtDebug2(__FILE__, (Widget)dsm, dsi->dropSite, "%s:loc_in_info(%d) - %+i%+i %+i%+i\n",
    	__FILE__, __LINE__,
    	x, y,
    	DS_RootX(dsm), DS_RootY(dsm)));

    XtTranslateCoords(dsi->dropSite, 0, 0, &tx, &ty);

    tx -= DS_RootX(dsm);
    ty -= DS_RootY(dsm);

    x -= tx;
    y -= ty;
    DEBUGOUT(_LtDebug0(__FILE__, (Widget)dsm, "\t%+i%+i\n",
    	x, y));

    if (_XmRegionPointInRegion(dsi->region, x, y))
    {
	return True;
    }
    else
    {
	return False;
    }
}


static XmDropSiteInfo
loc_to_info(XmDropSiteManagerObject dsm, XmDropSiteInfo dsi,
	    Position x, Position y)
{
    int i;
    XmDropSiteInfo child;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "%s:loc_to_info(%d) - %+i%+i %s\n",
    	__FILE__, __LINE__,
    	x, y,
    	dsi->leaf ? "Leaf" : "Not leaf"));

    if (!dsi->leaf)
    {
	DEBUGOUT(_LtDebug0(__FILE__, (Widget)dsm, "\t%i children\n", dsi->numChildren));
	for (i = 0; i < dsi->numChildren; i++)
	{
	    child = dsi->children[i];

	    if (loc_in_info(dsm, child, x, y))
	    {
		if (child->dropSiteActivity == XmDROP_SITE_INACTIVE)
		{
		    return NULL;
		}

		if (!child->leaf)
		{
		    XmDropSiteInfo grandchild;

		    grandchild = loc_to_info(dsm, child, x, y);

		    if (grandchild)
		    {
			return grandchild;
		    }
		}

		if (!child->implicit)
		{
		    return child;
		}
	    }
	}
    }

    return NULL;
}


/**
 * Waider 14/06/98 I've just noticed that this function /has/ no
 * function other than, at the moment, to generate core dumps.
 */
static void
animate(XmDropSiteManagerObject dsm,
	XmDragMotionClientDataStruct * cd,
	XmDragProcCallbackStruct *dpc)
{
    XmDropSiteInfo dsi = (XmDropSiteInfo) DS_CurInfo(dsm);
    Widget dc;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "%s:animate(%d)\n",
    	__FILE__, __LINE__));

    if (dsi && dsi->animationStyle == XmDRAG_UNDER_NONE)
    {
	return;
    }

    dc = DS_CurDragContext(dsm);
}


static void
drop_site_enter(XmDropSiteManagerObject dsm, XmDragMotionClientDataStruct * cd,
		XmDragMotionCallbackStruct *cbs, XmDropSiteInfo dsi,
		unsigned char ps)
{
    XmDragProcCallbackStruct dpc;
    XmDropSiteEnterCallbackStruct dse;
    XRectangle extents;
    Position x, y;
    Atom *itargs;
    int nimps;
    Widget par;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "%s:drop_site_enter(%d)\n",
    	__FILE__, __LINE__));

    dpc.reason = XmCR_DROP_SITE_ENTER_MESSAGE;
    dpc.event = NULL;
    dpc.timeStamp = cbs->timeStamp;
    dpc.dragContext = DS_CurDragContext(dsm);
    dpc.x = DS_CurX(dsm);
    dpc.y = DS_CurY(dsm);
    dpc.animate = True;

    dpc.operations = cbs->operations & dsi->dropSiteOperations;
    if (dpc.operations & XmDROP_MOVE)
    {
	dpc.operation = XmDROP_MOVE;
    }
    else if (dpc.operations & XmDROP_COPY)
    {
	dpc.operation = XmDROP_COPY;
    }
    else if (dpc.operations & XmDROP_LINK)
    {
	dpc.operation = XmDROP_LINK;
    }
    else
    {
	dpc.operation = XmDROP_NOOP;
	dpc.operations = XmDROP_NOOP;
    }

    if (dsi->external)
    {
	par = XtParent(dsm);
    }
    else
    {
	par = dsi->dropSite;
    }

    while (!XtIsShell(par))
    {
	par = XtParent(par);
    }

    /* a little self preservation here.  The user defined import targets table
     * might not be around. */
    nimps = _XmIndexToTargets(par, dsi->target_index, &itargs);

    if (dpc.operation != XmDROP_NOOP &&
	XmTargetsAreCompatible(XtDisplay(dsm),
			       DC_ExportTargets(dpc.dragContext),
			       DC_NumExportTargets(dpc.dragContext),
			       itargs, nimps))
    {
	dpc.dropSiteStatus = XmVALID_DROP_SITE;
    }
    else
    {
	dpc.dropSiteStatus = XmINVALID_DROP_SITE;
    }

    dpc.animate = True;

    if (ps == XmDRAG_DYNAMIC && !dsi->external && dsi->dragProc)
    {
	XtTranslateCoords(dsi->dropSite, 0, 0, &x, &y);

	dpc.x -= x;
	dpc.y -= y;

	(*dsi->dragProc) (dsi->dropSite, NULL, &dpc);
    }

    if (dpc.animate && dpc.dropSiteStatus == XmVALID_DROP_SITE)
    {
	animate(dsm, cd, &dpc);
    }

    DS_CurDropSiteStatus(dsm) = dpc.dropSiteStatus;
    DS_CurAnimate(dsm) = dpc.animate;
    DS_CurOperation(dsm) = dpc.operation;
    DS_CurOperations(dsm) = dpc.operations;

    if (DS_NotifyProc(dsm) != NULL)
    {

	_XmRegionGetExtents(dsi->region, &extents);

	dse.reason = XmCR_DROP_SITE_ENTER;
	dse.event = NULL;
	dse.timeStamp = dpc.timeStamp;
	dse.operation = dpc.operation;
	dse.operations = dpc.operations;
	dse.dropSiteStatus = dpc.dropSiteStatus;

	if (dsi->external)
	{
	    dse.x = DS_RootX(dsm) + extents.x;
	    dse.y = DS_RootY(dsm) + extents.y;
	}
	else
	{
	    XtTranslateCoords(dsi->dropSite, 0, 0, &x, &y);

	    dse.x = x + extents.x;
	    dse.y = y + extents.y;
	}

	(*DS_NotifyProc(dsm)) ((Widget)dsm, DS_ClientData(dsm), (XtPointer)&dse);
    }
}


static void
drop_site_leave(XmDropSiteManagerObject dsm, XmDragMotionClientDataStruct * cd,
		XmDragMotionCallbackStruct *cbs, XmDropSiteInfo dsi,
		unsigned char ps)
{
    XmDragProcCallbackStruct dpc;
    XmDropSiteLeaveCallbackStruct dsl;
    Position x, y;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "%s:drop_site_leave(%d)\n",
    	__FILE__, __LINE__));

    dpc.reason = XmCR_DROP_SITE_LEAVE_MESSAGE;
    dpc.event = NULL;
    dpc.timeStamp = cbs->timeStamp;
    dpc.dragContext = DS_CurDragContext(dsm);
    dpc.x = DS_OldX(dsm);
    dpc.y = DS_OldY(dsm);
    dpc.operation = cbs->operation;
    dpc.operations = cbs->operations;
    dpc.animate = DS_CurAnimate(dsm);
    dpc.dropSiteStatus = DS_CurDropSiteStatus(dsm);

    if (ps == XmDRAG_DYNAMIC && !dsi->external && dsi->dragProc)
    {

	XtTranslateCoords(dsi->dropSite, 0, 0, &x, &y);

	dpc.x -= x;
	dpc.y -= y;

	(*dsi->dragProc) (dsi->dropSite, NULL, &dpc);
    }

    if (dpc.animate && dpc.dropSiteStatus == XmVALID_DROP_SITE)
    {
	animate(dsm, cd, &dpc);
    }

    if (DS_NotifyProc(dsm) != NULL)
    {

	dsl.reason = XmCR_DROP_SITE_LEAVE;
	dsl.event = NULL;
	dsl.timeStamp = dpc.timeStamp;

	(*DS_NotifyProc(dsm)) ((Widget)dsm, DS_ClientData(dsm), (XtPointer)&dsl);
    }
}


static void
drag_motion(XmDropSiteManagerObject dsm, XmDragMotionClientDataStruct * cd,
	    XmDragMotionCallbackStruct *cbs, XmDropSiteInfo dsi,
	    unsigned char ps)
{
    XmDragProcCallbackStruct dpc;
    XmDragMotionCallbackStruct dm;
    Position x, y;

    DEBUGOUT(_LtDebug("DRAGSINK", (Widget)dsm, "%s:drag_motion(%d)\n",
    	__FILE__, __LINE__));

    dpc.reason = XmCR_DROP_SITE_MOTION_MESSAGE;
    dpc.event = NULL;
    dpc.timeStamp = cbs->timeStamp;
    dpc.dragContext = DS_CurDragContext(dsm);
    dpc.x = DS_CurX(dsm);
    dpc.y = DS_CurY(dsm);
    dpc.operation = cbs->operation;
    dpc.operations = cbs->operations;
    dpc.animate = DS_CurAnimate(dsm);
    dpc.dropSiteStatus = DS_CurDropSiteStatus(dsm);

    if (dsi != NULL)
    {

	dpc.operation = DS_CurOperation(dsm);
	dpc.operations = DS_CurOperations(dsm);

	if (ps == XmDRAG_DYNAMIC && !dsi->external && dsi->dragProc)
	{
	    XtTranslateCoords(dsi->dropSite, 0, 0, &x, &y);

	    dpc.x -= x;
	    dpc.y -= y;

	    (*dsi->dragProc) (dsi->dropSite, NULL, &dpc);
	}

	if (dpc.animate && dpc.dropSiteStatus != DS_CurDropSiteStatus(dsm))
	{
	    if (dpc.dropSiteStatus == XmVALID_DROP_SITE)
	    {
		dpc.reason = XmCR_DROP_SITE_ENTER;
	    }
	    else
	    {
		dpc.reason = XmCR_DROP_SITE_LEAVE;
	    }

	    animate(dsm, cd, &dpc);

	    dpc.reason = XmCR_DROP_SITE_MOTION_MESSAGE;
	}

	DS_CurDropSiteStatus(dsm) = dpc.dropSiteStatus;
	DS_CurAnimate(dsm) = dpc.animate;
	DS_CurOperation(dsm) = dpc.operation;
	DS_CurOperations(dsm) = dpc.operations;

    }
    else
    {
	dpc.operation = cbs->operation;
	dpc.operations = cbs->operations;
	dpc.dropSiteStatus = XmNO_DROP_SITE;
    }

    if (DS_NotifyProc(dsm) != NULL)
    {
	dm.reason = XmCR_DRAG_MOTION;
	dm.event = NULL;
	dm.timeStamp = dpc.timeStamp;
	dm.x = DS_CurX(dsm);
	dm.y = DS_CurY(dsm);
	dm.operation = dpc.operation;
	dm.operations = dpc.operations;
	dm.dropSiteStatus = dpc.dropSiteStatus;

	(*DS_NotifyProc(dsm)) ((Widget)dsm, DS_ClientData(dsm), (XtPointer)&dm);
    }
}


/*
 * When a drag motion event is sent to this client, we handle 
 * the notification of the drop site (if it has drag under effects)
 */
static void
__XmDSMProcessMotion(XmDropSiteManagerObject dsm,
		     XtPointer clientData,
		     XtPointer callData)
{
    XmDragMotionCallbackStruct *cbs = (XmDragMotionCallbackStruct *)callData;
    XmDragMotionClientDataStruct *cd = (XmDragMotionClientDataStruct *) clientData;
    XmDropSiteInfo dsi = (XmDropSiteInfo) DS_CurInfo(dsm);
    XmDropSiteInfo ndsi;
    unsigned char ps;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "__XmDSMProcessMotion - %s %+i%+i %+i%+i\n",
    	_LtDebugDragAndDropMessageType2String(cbs->reason),
    	cbs->x, cbs->y, DS_RootX(dsm), DS_RootY(dsm)));
    DEBUGOUT(_LtDebug("DRAGSINK", (Widget)dsm, "__XmDSMProcessMotion - %s %+i%+i %+i%+i\n",
    	_LtDebugDragAndDropMessageType2String(cbs->reason),
    	cbs->x, cbs->y, DS_RootX(dsm), DS_RootY(dsm)));

    if (DS_CurDragContext(dsm) == NULL)
    {
	_XmWarning((Widget)dsm, "Eeek!  Where'd that come from? %s(%d)\n",
		__FILE__, __LINE__);
	return;
    }

    DS_CurTime(dsm) = cbs->timeStamp;
    DS_OldX(dsm) = DS_CurX(dsm);
    DS_OldY(dsm) = DS_CurY(dsm);
    DS_CurX(dsm) = cbs->x;
    DS_CurY(dsm) = cbs->y;

    ps = _XmGetActiveProtocolStyle(DS_CurDragContext(dsm));

    if (DS_DSRoot(dsm) != NULL)
    {
	ndsi = loc_to_info(dsm, (XmDropSiteInfo) DS_DSRoot(dsm),
			   cbs->x - DS_RootX(dsm),
			   cbs->y - DS_RootY(dsm));

	if (ndsi != dsi)
	{
	    if (dsi)
	    {
		drop_site_leave(dsm, cd, cbs, dsi, ps);
	    }

            DEBUGOUT(_LtDebug(__FILE__, 
                              (Widget)dsm, "Setting dsi to %p\n", ndsi ));
            DEBUGOUT(_LtDebug("DRAGSINK", 
                              (Widget)dsm, "Setting dsi to %s\n", 
                              ndsi ? XtName(ndsi->dropSite) : "NULL"));
	    DS_CurInfo(dsm) = (XtPointer)ndsi;

	    if (ndsi)
	    {
		drop_site_enter(dsm, cd, cbs, ndsi, ps);
	    }
	}
    }

    DEBUGOUT(_LtDebug("DRAGSINK", 
                      (Widget)dsm, "Dragging on %s\n", 
                      dsi ? XtName(dsi->dropSite) : "NULL"));
    drag_motion(dsm, cd, cbs, dsi, ps);
}


/*
 * When the drop is started (BDrag is released), we let the drop site know that
 * it's being dropped in
 */
static void
__XmDSMProcessDrop(XmDropSiteManagerObject dsm,
		   XtPointer clientData,
		   XtPointer callData)
{
    XmDragTopLevelClientDataStruct *cd = (XmDragTopLevelClientDataStruct *) clientData;
    XmDropStartCallbackStruct *cbs = (XmDropStartCallbackStruct *)callData;
    XmDropProcCallbackStruct dpc;
    Widget dc;
    XmDropSiteInfo dsi = NULL, rsi = NULL;
    Arg al[2];
    int ac;
    Position x, y;
    Widget par;
    Cardinal nimps;
    Atom *itargs;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "__XmDSMProcessDrop\n"));
    DEBUGOUT(_LtDebug("DRAGSINK", (Widget)dsm, "__XmDSMProcessDrop %s %s\n",
    	cbs->dropAction == XmDROP ? "XmDROP" : "not XmDROP",
    	cbs->dropSiteStatus == XmDROP_SITE_VALID ? "XmDROP_SITE_VALID" : "not XmDROP_SITE_VALID"));

    dc = XmGetDragContext((Widget)dsm, cbs->timeStamp);

    if (dc == NULL)
    {
	_XmWarning((Widget)dsm, "Eeek!  Where'd that come from? %s(%d)\n",
		__FILE__, __LINE__);
	return;
    }

    DS_CurTime(dsm) = cbs->timeStamp;
    DS_DSRoot(dsm) = DSMWidgetToInfo(dsm, cd->shell);
    DS_RootX(dsm) = cd->xOrigin;
    DS_RootY(dsm) = cd->yOrigin;
    DS_RootWidth(dsm) = cd->width;
    DS_RootHeight(dsm) = cd->height;

    rsi = (XmDropSiteInfo) DSMWidgetToInfo(dsm, cd->shell);

    if (rsi != NULL)
    {
	dsi = loc_to_info(dsm, (XmDropSiteInfo) DS_DSRoot(dsm),
			  cbs->x - cd->xOrigin, cbs->y - cd->yOrigin);
    }

    if (dsi == NULL)
    {
	ac = 0;
	XtSetArg(al[ac], XmNnumDropTransfers, 0);
	ac++;
	XtSetArg(al[ac], XmNtransferStatus, XmTRANSFER_FAILURE);
	ac++;

	XmDropTransferStart(dc, al, ac);
    }
    else
    {

	DS_CurInfo(dsm) = (XtPointer)dsi;

	dpc.reason = XmCR_DROP_MESSAGE;
	dpc.event = cbs->event;
	dpc.timeStamp = cbs->timeStamp;
	dpc.dragContext = dc;

	XtTranslateCoords(dsi->dropSite, 0, 0, &x, &y);

	dpc.x = cbs->x - x;
	dpc.y = cbs->y - y;

	dpc.operations = cbs->operations & dsi->dropSiteOperations;
	if (dpc.operations & XmDROP_MOVE)
	{
	    dpc.operation = XmDROP_MOVE;
	}
	else if (dpc.operations & XmDROP_COPY)
	{
	    dpc.operation = XmDROP_COPY;
	}
	else if (dpc.operations & XmDROP_LINK)
	{
	    dpc.operation = XmDROP_LINK;
	}
	else
	{
	    dpc.operation = XmDROP_NOOP;
	    dpc.operations = XmDROP_NOOP;
	}

	if (dsi->external)
	{
	    par = XtParent(dsm);
	}
	else
	{
	    par = dsi->dropSite;
	}

	while (!XtIsShell(par))
	{
	    par = XtParent(par);
	}

	/* a little self preservation here.  The user defined import targets
	 * table might not be around. */
	nimps = _XmIndexToTargets(par, dsi->target_index, &itargs);

	if (dpc.operation != XmDROP_NOOP &&
	    XmTargetsAreCompatible(XtDisplay(dsm),
				   DC_ExportTargets(dpc.dragContext),
				   DC_NumExportTargets(dpc.dragContext),
				   itargs, nimps))
	{
	    dpc.dropSiteStatus = XmVALID_DROP_SITE;
	}
	else
	{
	    dpc.dropSiteStatus = XmINVALID_DROP_SITE;
	}

	dpc.dropAction = cbs->dropAction;

	/* T. Straumann: safe guard against NULL added */
	if (dsi->dropProc)
	{
		(*dsi->dropProc) (dsi->dropSite, NULL, (XtPointer)&dpc);
	}
	else
	{
		_XmWarning((Widget)dsm, "__XmDSMProcessDrop():  no dsi->dropProc!\n");
	}

	cbs->operation = dpc.operation;
	cbs->operations = dpc.operations;
	cbs->dropSiteStatus = dpc.dropSiteStatus;
	cbs->dropAction = dpc.dropAction;
    }

    if (DS_NotifyProc(dsm) != NULL)
    {
	(*DS_NotifyProc(dsm)) ((Widget)dsm, DS_ClientData(dsm), (XtPointer)cbs);
    }
}


/*
 * We notify the current drop site (if any) that the operation of the drag
 * has changed
 */
static void
__XmDSMOperationChanged(XmDropSiteManagerObject dsm,
			XtPointer clientData,
			XtPointer callData)
{
    XmOperationChangedCallbackStruct *cbs = (XmOperationChangedCallbackStruct *)callData;
    XmDragMotionClientDataStruct *cd = (XmDragMotionClientDataStruct *) clientData;
    XmDragProcCallbackStruct dpc;
    unsigned char ps;
    XmDropSiteInfo dsi;
    Position x, y;
    Atom *itargs;
    int nimps;
    Widget par;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM OperationChanged\n"));

    if (DS_CurDragContext(dsm) == NULL)
    {
	_XmWarning((Widget)dsm, "Eeek!  Where'd that come from? %s(%d)\n",
		__FILE__, __LINE__);
	return;
    }

    ps = _XmGetActiveProtocolStyle(DS_CurDragContext(dsm));

    dpc.dragContext = DS_CurDragContext(dsm);
    dpc.reason = cbs->reason;
    dpc.timeStamp = cbs->timeStamp;
    dpc.operation = cbs->operation;
    dpc.operations = cbs->operations;
    dpc.x = DS_CurX(dsm);
    dpc.y = DS_CurY(dsm);
    dpc.dropSiteStatus = DS_CurDropSiteStatus(dsm);
    dpc.animate = DS_CurAnimate(dsm);

    if (DS_CurInfo(dsm) != NULL)
    {

	dsi = (XmDropSiteInfo) DS_CurInfo(dsm);

	dpc.operations = cbs->operations & dsi->dropSiteOperations;
	if (dpc.operations & XmDROP_MOVE)
	{
	    dpc.operation = XmDROP_MOVE;
	}
	else if (dpc.operations & XmDROP_COPY)
	{
	    dpc.operation = XmDROP_COPY;
	}
	else if (dpc.operations & XmDROP_LINK)
	{
	    dpc.operation = XmDROP_LINK;
	}
	else
	{
	    dpc.operation = XmDROP_NOOP;
	    dpc.operations = XmDROP_NOOP;
	}

	if (dsi->external)
	{
	    par = XtParent(dsm);
	}
	else
	{
	    par = dsi->dropSite;
	}

	while (!XtIsShell(par))
	    par = XtParent(par);

	/* a little self preservation here.  The user defined import targets
	 * table might not be around. */
	nimps = _XmIndexToTargets(par, dsi->target_index, &itargs);

	if (dpc.operation != XmDROP_NOOP &&
	    XmTargetsAreCompatible(XtDisplay(dsm),
				   DC_ExportTargets(dpc.dragContext),
				   DC_NumExportTargets(dpc.dragContext),
				   itargs, nimps))
	{
	    dpc.dropSiteStatus = XmVALID_DROP_SITE;
	}
	else
	{
	    dpc.dropSiteStatus = XmINVALID_DROP_SITE;
	}

	dpc.animate = True;

	if (ps == XmDRAG_DYNAMIC && !dsi->external && dsi->dragProc)
	{

	    XtTranslateCoords(dsi->dropSite, 0, 0, &x, &y);

	    dpc.x -= x;
	    dpc.y -= y;

	    (*dsi->dragProc) (dsi->dropSite, NULL, &dpc);
	}

	if (dpc.animate && dpc.dropSiteStatus != DS_CurDropSiteStatus(dsm))
	{

	    if (dpc.dropSiteStatus == XmVALID_DROP_SITE)
	    {
		dpc.reason = XmCR_DROP_SITE_ENTER;
	    }
	    else
	    {
		dpc.reason = XmCR_DROP_SITE_LEAVE;
	    }

	    animate(dsm, cd, &dpc);

	    dpc.reason = XmCR_DROP_SITE_MOTION_MESSAGE;
	}

	cbs->operation = dpc.operation;
	cbs->operations = dpc.operations;
	cbs->dropSiteStatus = dpc.dropSiteStatus;

	DS_CurDropSiteStatus(dsm) = dpc.dropSiteStatus;
	DS_CurAnimate(dsm) = dpc.animate;
	DS_CurOperation(dsm) = dpc.operation;
	DS_CurOperations(dsm) = dpc.operations;
    }
    else
    {
	cbs->dropSiteStatus = XmNO_DROP_SITE;
    }

    if (DS_NotifyProc(dsm) != NULL)
    {
	(*DS_NotifyProc(dsm)) ((Widget)dsm, DS_ClientData(dsm), (XtPointer)cbs);
    }
}


static void
__XmDSMChangeRoot(XmDropSiteManagerObject dsm,
		  XtPointer clientData,
		  XtPointer callData)
{
    XmDragTopLevelClientDataStruct *cd =
    (XmDragTopLevelClientDataStruct *) clientData;
    XmDragDropCallbackStruct *cbs = (XmDragDropCallbackStruct *) callData;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "__XmDSMChangeRoot: %s\n",
		      _LtDebugDragAndDropMessageType2String(cbs->any.reason)));
    DEBUGOUT(_LtDebug("DRAGSINK", (Widget)dsm, "__XmDSMChangeRoot: %s\n",
		      _LtDebugDragAndDropMessageType2String(cbs->any.reason)));

    DS_CurTime(dsm) = cbs->any.timeStamp;

    if (cbs->any.reason == XmCR_TOP_LEVEL_ENTER)
    {
	if (cd->sourceIsExternal)
	{
	    DS_CurDragContext(dsm) = XtVaCreateWidget("ExternalDragC", 
	    	xmDragContextClass, XmGetXmDisplay(XtDisplay(dsm)),
		XmNsourceIsExternal, True, 
		XmNsourceWindow, cbs->tle.window,
		XmNiccHandle, cbs->tle.iccHandle,
		NULL);
	    DC_CurrReceiverInfo(DS_CurDragContext(dsm)) = _XmAllocReceiverInfo((XmDragContext)DS_CurDragContext(dsm));
	    (DC_CurrReceiverInfo(DS_CurDragContext(dsm)))->shell = cd->shell;
	}
	else
	{
	    DS_CurDragContext(dsm) = XmGetDragContext((Widget)dsm,
						  cbs->any.timeStamp);
	}

	if (cd->shell)
	{
	    DS_DSRoot(dsm) = DSMWidgetToInfo(dsm, cd->shell);
	}
	else
	{
	    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "EEEEEK!!!\n")); /* EEK! */
	    DS_DSRoot(dsm) = NULL;
	}

	DS_RootX(dsm) = cd->xOrigin;
	DS_RootY(dsm) = cd->yOrigin;
	DS_RootWidth(dsm) = cd->width;
	DS_RootHeight(dsm) = cd->height;
    }
    else
    {	/* LEAVE */
	if (DS_CurInfo(dsm) != NULL)
	{
	    XmDragMotionCallbackStruct dm;
	    XmDragMotionClientDataStruct dmc;

	    dm.reason = XmCR_DROP_SITE_LEAVE;
	    dm.event = cbs->any.event;
	    dm.timeStamp = cbs->any.timeStamp;
	    dm.x = DS_CurX(dsm);
	    dm.y = DS_CurY(dsm);
	    dm.operation = dm.operations = dm.dropSiteStatus = 0;

	    dmc.window = cd->window;
	    dmc.dos = cd->dos;

	    drop_site_leave(dsm, &dmc, &dm, (XmDropSiteInfo) DS_CurInfo(dsm),
			    _XmGetActiveProtocolStyle(DS_CurDragContext(dsm)));

	    DS_CurInfo(dsm) = NULL;
	}

	DS_CurDragContext(dsm) = NULL;
	DS_DSRoot(dsm) = NULL;
	DS_RootX(dsm) = -1;
	DS_RootY(dsm) = -1;
	DS_RootWidth(dsm) = 0;
	DS_RootHeight(dsm) = 0;
    }
}


static void
__XmDSMInsertInfo(XmDropSiteManagerObject dsm,
		  XtPointer clientData,
		  XtPointer callData)
{
    XmDropSiteInfo dsi = (XmDropSiteInfo) clientData, pi;
    Widget w, parent;
    XmTreeUpdateCallbackStruct tucbs;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "%s:__XmDSMInsertInfo(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSINK", (Widget)dsm, "%s:__XmDSMInsertInfo(%d)\n",
    	__FILE__, __LINE__));

    w = dsi->dropSite;

    parent = w;

    do
    {
	parent = XtParent(parent);

	if ((pi = (XmDropSiteInfo) DSMWidgetToInfo(dsm, parent)) != NULL)
	{
	    break;
	}

    }
    while (!XtIsShell(parent));

    if (pi == NULL)
    {

	pi = createShellInfoRec(dsm, parent);

	DSMRegisterInfo(dsm, parent, (XtPointer)pi);

	addChildToComposite(pi, dsi, pi->numChildren);

	if (DS_TreeUpdateProc(dsm) == NULL)
	{
	    return;
	}

	if (XtIsRealized(parent) &&
	    _XmGetDragProtocolStyle(parent) != XmDYNAMIC)
	{
	    return;
	}

	tucbs.reason = XmCR_ADD_DROP_SITE;
	tucbs.event = NULL;
	tucbs.widget = parent;

	DS_TreeUpdateProc(dsm) ((Widget)dsm, NULL, &tucbs);
    }
    else
    {

	if (pi->dropSiteType == XmDROP_SITE_COMPOSITE)
	{
	    addChildToComposite(pi, dsi, pi->numChildren);
	}
	else
	{
	    _XmWarning(parent,
		   "Attempt to add dropSite to parent that isn't composite\n");
	}
    }
}


static void
__XmDSMRemoveInfo(XmDropSiteManagerObject dsm,
		  XtPointer info)
{
    XmDropSiteInfo dsi = (XmDropSiteInfo) info, pi;
    Widget w;
    XmTreeUpdateCallbackStruct tucbs;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM RemoveInfo\n"));

    w = dsi->dropSite;

    pi = dsi->parent;

    if (pi)
    {
	removeChildFromComposite(pi, dsi);
    }

    DSMUnregisterInfo(dsm, (XtPointer)dsi);

    XtRemoveCallback(w, XmNdestroyCallback, destroyInfo, (XtPointer)dsm);

    if (dsi->parent == NULL)
    {
	return;
    }

    if (pi->numChildren != 0)
    {
	return;
    }

    if (!pi->implicit)
    {
	return;
    }

    tucbs.reason = XmCR_REMOVE_DROP_SITE;
    tucbs.event = NULL;
    tucbs.widget = pi->dropSite;

    if (XtIsShell(pi->dropSite))
    {
	if (DS_TreeUpdateProc(dsm) != NULL)
	{
	    DS_TreeUpdateProc(dsm) ((Widget)dsm, NULL, &tucbs);
	}
    }

    DSMDestroyInfo(dsm, pi->dropSite);
}


static void
__XmDSMSyncTree(XmDropSiteManagerObject dsm,
		Widget shell)
{
    DEBUGOUT(_LtDebug2(__FILE__, (Widget)dsm, shell, "%s:__XmDSMSyncTree(%d)\n",
    	__FILE__, __LINE__));
}


/*
 * This function creates (or replaces) the property on the shell widget
 * to contain the entire drop site database.
 */
static int
__XmDSMGetTree(XmDropSiteManagerObject dsm,
	       Widget shell,
	       XtPointer dataPtr)
{
    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM GetTree\n"));

    return 0;
}


/*
 * Creates a hash table that goes from widgets to drop site information
 */
static void
__XmDSMCreateDSInfoTable(XmDropSiteManagerObject dsm)
{
    DSInfoTable *it;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM CreateDSInfoTable\n"));

    it = (DSInfoTable *) XtCalloc(1, sizeof(DSInfoTable));

    DS_DSTable(dsm) = (XtPointer)it;
    it->num_buckets = HASH_BUCKETS;

    it->buckets = (XmDropSiteInfo *) XtCalloc(it->num_buckets,
					      sizeof(XmDropSiteInfo));
}


/*
 * destroys the hash table
 */
static void
__XmDSMDestroyDSInfoTable(XmDropSiteManagerObject dsm)
{
    DSInfoTable *it;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM DestroyDSInfoTable\n"));

    it = (DSInfoTable *) DS_DSTable(dsm);

    XtFree((char *)it->buckets);

    XtFree((char *)it);
}


/*
 * adds a widget and its associated info to the hash table
 */
static void
__XmDSMRegisterInfo(XmDropSiteManagerObject dsm,
		    Widget widget,
		    XtPointer info)
{
    XmDropSiteInfo dsi = (XmDropSiteInfo) info;
    DSInfoTable *dst;
    long hash;

    DEBUGOUT(_LtDebug2(__FILE__, (Widget)dsm, widget, "%s:__XmDSMRegisterInfo(%d)\n",
    	__FILE__, __LINE__));
    DEBUGOUT(_LtDebug2("DRAGSINK", (Widget)dsm, widget, "%s:__XmDSMRegisterInfo(%d)\n",
    	__FILE__, __LINE__));

    if (dsi->registered)
    {
	return;
    }

    dst = (DSInfoTable *) DS_DSTable(dsm);

    hash = (long)widget & dst->num_buckets;

    dsi->next = dst->buckets[hash];
    dst->buckets[hash] = dsi;

    dsi->registered = 1;
}


/*
 * retrieves the info for a given widget from the hash table
 */
static XtPointer
__XmDSMWidgetToInfo(XmDropSiteManagerObject dsm,
		    Widget widget)
{
    DSInfoTable *info;
    long hash;
    XmDropSiteInfo dsi;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM WidgetToInfo\n"));

    info = (DSInfoTable *) DS_DSTable(dsm);

    hash = (long)widget & info->num_buckets;

    if (info->buckets[hash] == NULL)
    {
	return NULL;
    }

    dsi = info->buckets[hash];

    while (dsi != NULL)
    {

	if (dsi->dropSite == widget)
	{
	    return (XtPointer)dsi;
	}

	dsi = dsi->next;
    }

    return NULL;
}


/*
 * removes the info from the hash table, along with the widget that
 * hashes to it.
 */
static void
__XmDSMUnregisterInfo(XmDropSiteManagerObject dsm,
		      XtPointer info)
{
    XmDropSiteInfo dsi = (XmDropSiteInfo) info, *ptr;
    DSInfoTable *dst;
    long hash;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "DSM UnregisterInfo\n"));

    if (!dsi->registered)
    {
	return;
    }

    dst = (DSInfoTable *) DS_DSTable(dsm);

    hash = (long)dsi->dropSite & dst->num_buckets;

    for (ptr = &dst->buckets[hash]; *ptr; ptr = &((*ptr)->next))
    {
	if (*ptr == dsi)
	{
	    *ptr = dsi->next;
	    break;
	}
    }

    dsi->registered = 0;
}


extern void
_XmDSMUpdate(XmDropSiteManagerObject dsm,
	     XtPointer clientData,
	     XtPointer callData)
{
    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "_XmDSMUpdate\n"));

    DSMUpdate(dsm, clientData, callData);
}


extern int
_XmDSMGetTreeFromDSM(XmDropSiteManagerObject dsm,
		     Widget shell,
		     XtPointer dataPtr)
{
    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "_XmDSMGetTreeFromDSM\n"));

    return DSMGetTreeFromDSM(dsm, shell, dataPtr);
}


extern Boolean
_XmDropSiteShell(Widget widget)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget));
    XmDropSiteManagerObject dsm;

    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmDropSiteShell\n"));

    dsm = _XmGetDropSiteManagerObject((XmDisplay)disp);

    if (!XtIsShell(widget))
    {
	return False;
    }

    if (DSMWidgetToInfo(dsm, widget))
    {
	return True;
    }

    return False;
}


static Boolean
has_ds_offspring(XmDropSiteManagerObject dsm, Widget w)
{
    Cardinal i;

    DEBUGOUT(_LtDebug(__FILE__, (Widget)dsm, "%s:has_ds_offspring(%d)\n",
    	__FILE__, __LINE__));

    if (!XtIsComposite(w))
    {
	return False;
    }

    for (i = 0; i < MGR_NumChildren(w); i++)
    {

	if (DSMWidgetToInfo(dsm, w))
	{
	    return True;
	}

	if (has_ds_offspring(dsm, MGR_Children(w)[i]))
	{
	    return True;
	}
    }

    return False;
}


extern Boolean
_XmDropSiteWrapperCandidate(Widget widget)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget)), par;
    XmDropSiteManagerObject dsm;

    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmDropSiteWrapperCandidate\n"));

    dsm = _XmGetDropSiteManagerObject((XmDisplay)disp);

    if (!dsm)
    {
	return False;
    }

    if (DSMWidgetToInfo(dsm, widget))
    {
	return True;
    }

    if (!XtIsComposite(widget))
    {
	return False;
    }

    par = widget;
    while (!XtIsShell(par))
    {
	par = XtParent(par);
    }

    if (!_XmDropSiteShell(par))
    {
	return False;
    }

    return has_ds_offspring(dsm, widget);
}


extern Widget
_XmGetActiveDropSite(Widget widget)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget));
    XmDropSiteManagerObject dsm;

    DEBUGOUT(_LtDebug(__FILE__, widget, "_XmGetActiveDropSite\n"));

    dsm = _XmGetDropSiteManagerObject((XmDisplay)disp);

    if (DS_CurInfo(dsm) != NULL)
    {
	return ((XmDropSiteInfo) DS_CurInfo(dsm))->dropSite;
    }
    else
    {
	return NULL;
    }
}


extern void
_XmSyncDropSiteTree(Widget shell)
{
    Widget disp = XmGetXmDisplay(XtDisplay(shell));
    XmDropSiteManagerObject dsm;

    DEBUGOUT(_LtDebug(__FILE__, shell, "_XmSyncDropSiteTree\n"));

    dsm = _XmGetDropSiteManagerObject((XmDisplay)disp);

    DSMSyncTree(dsm, shell);
}

/*
 * dead function
 */

extern void
_XmIEndUpdate(XtPointer client_data, XtIntervalId *interval_id)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmIEndUpdate(%d)\n",
    	__FILE__, __LINE__));

}


extern void
XmDropSiteConfigureStackingOrder(Widget widget,
				 Widget Sibling,
				 Cardinal stack_mode)
{
    DEBUGOUT(_LtDebug(__FILE__, widget, "XmDropSiteConfigureStackingOrder()\n"));

    /* FIX ME */
}


extern void
XmDropSiteEndUpdate(Widget widget)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget));

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmDropSiteEndUpdate()\n"));

    DSMEndUpdate(_XmGetDropSiteManagerObject((XmDisplay)disp), widget);
}


extern Status
XmDropSiteQueryStackingOrder(Widget widget,
			     Widget *parent_return,
			     Widget **child_returns,
			     Cardinal *num_child_returns)
{
    DEBUGOUT(_LtDebug(__FILE__, widget, "XmDropSiteQueryStackingOrder()\n"));

    /* FIX ME */

    return 0;
}


extern void
XmDropSiteRegister(Widget widget,
		   ArgList arglist,
		   Cardinal argcount)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget));

    DEBUGOUT(_LtDebug(__FILE__, widget,
		      "%s:XmDropSiteRegister(%d) - %i args\n",
		      __FILE__, __LINE__,
		      argcount));
    DEBUGOUT(_LtDebugPrintArgList(__FILE__, widget, arglist, argcount, False));
    DEBUGOUT(_LtDebug("DRAGSINK", widget,
		      "%s:XmDropSiteRegister(%d) - %i args\n",
		      __FILE__, __LINE__,
		      argcount));
    DEBUGOUT(_LtDebugPrintArgList("DRAGSINK", widget, arglist, argcount, False));

    DSMCreateInfo(_XmGetDropSiteManagerObject((XmDisplay)disp),
		  widget, arglist, argcount);
}


extern void
XmDropSiteRetrieve(Widget widget,
		   ArgList arglist,
		   Cardinal argcount)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget));

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmDropSiteRetrieve()\n"));

    DSMRetrieveInfo(_XmGetDropSiteManagerObject((XmDisplay)disp),
		    widget, arglist, argcount);
}


extern void
XmDropSiteStartUpdate(Widget widget)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget));

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmDropSiteStartUpdate()\n"));

    DSMStartUpdate(_XmGetDropSiteManagerObject((XmDisplay)disp), widget);
}


extern void
XmDropSiteUnregister(Widget widget)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget));

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmDropSiteUnregister()\n"));

    DSMDestroyInfo(_XmGetDropSiteManagerObject((XmDisplay)disp), widget);
}


extern void
XmDropSiteUpdate(Widget widget,
		 ArgList arglist,
		 Cardinal argcount)
{
    Widget disp = XmGetXmDisplay(XtDisplay(widget));

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmDropSiteUpdate()\n"));

    DSMUpdateInfo(_XmGetDropSiteManagerObject((XmDisplay)disp),
		  widget, arglist, argcount);
}

/* amai: this call is not within the 2.1 docs for OM ... */
extern XmDropSiteVisuals
XmDropSiteGetActiveVisuals(Widget widget)
{
    /* FIX ME */

    DEBUGOUT(_LtDebug(__FILE__, widget, "XmDropSiteGetActiveVisuals()\n"));

    return NULL;
}


#if XmVERSION >= 2
extern Boolean
XmDropSiteRegistered(Widget w)
{
	/* FIX ME */

	DEBUGOUT(_LtDebug(__FILE__, w, "XmDropSiteRegistered()\n"));
	return False;
}
#endif /* #if XmVERSION >= 2 */
