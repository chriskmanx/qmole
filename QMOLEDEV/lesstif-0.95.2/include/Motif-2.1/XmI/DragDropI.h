/**
 *
 * $Id: DragDropI.h,v 1.1 2004/08/28 19:23:29 dannybackx Exp $
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

/***********************************************************/
/* Copyright 1996 Daniel Dardailler.  
Permission to use, copy, modify, distribute, and sell this software
for any purpose is hereby granted without fee, provided that the above
copyright notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting documentation,
and that the name of Daniel Dardailler not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  Daniel Dardailler makes no representations
about the suitability of this software for any purpose.  It is
provided "as is" without express or implied warranty.
************************************************************/

#ifndef _XMI_DRAGDROPI_H
#define _XMI_DRAGDROPI_H

#include <X11/Xmd.h>

#include <Xm/DragCP.h>
#include <Xm/DragOverSP.h>

/*
 * for drop transfer
 */
#define XmInheritStartTransferProc \
	(XmDropTransferStartTransferProc)(_XtInherit)
#define XmInheritAddTransferProc \
	(XmDropTransferAddTransferProc)(_XtInherit)

/*
 * for drop site manager
 */
#define XmInheritCreateInfoProc		(XmDSMCreateInfoProc)(_XtInherit)
#define XmInheritDestroyInfoProc	(XmDSMDestroyInfoProc)(_XtInherit)
#define XmInheritStartUpdateProc	(XmDSMStartUpdateProc)(_XtInherit)
#define XmInheritRetrieveInfoProc	(XmDSMRetrieveInfoProc)(_XtInherit)
#define XmInheritUpdateInfoProc		(XmDSMUpdateInfoProc)(_XtInherit)
#define XmInheritEndUpdateProc		(XmDSMEndUpdateProc)(_XtInherit)
#define XmInheritUpdateProc		(XmDSMUpdateProc)(_XtInherit)
#define XmInheritProcessMotionProc	(XmDSMProcessMotionProc)(_XtInherit)
#define XmInheritProcessDropProc	(XmDSMProcessDropProc)(_XtInherit)
#define XmInheritOperationChangedProc	(XmDSMOperationChangedProc)(_XtInherit)
#define XmInheritChangeRootProc		(XmDSMChangeRootProc)(_XtInherit)
#define XmInheritInsertInfoProc		(XmDSMInsertInfoProc)(_XtInherit)
#define XmInheritRemoveInfoProc		(XmDSMRemoveInfoProc)(_XtInherit)
#define XmInheritSyncTreeProc		(XmDSMSyncTreeProc)(_XtInherit)
#define XmInheritGetTreeFromDSMProc	(XmDSMGetTreeFromDSMProc)(_XtInherit)
#define XmInheritCreateDSInfoTable	(XmDSMCreateDSInfoTable)(_XtInherit)
#define XmInheritDestroyDSInfoTable	(XmDSMDestroyDSInfoTable)(_XtInherit)
#define XmInheritRegisterInfoProc	(XmDSMRegisterInfoProc)(_XtInherit)
#define XmInheritWidgetToInfoProc	(XmDSMWidgetToInfoProc)(_XtInherit)
#define XmInheritUnregisterInfoProc	(XmDSMUnregisterInfoProc)(_XtInherit)

/*
 * for DragContext
 */
#define XmInheritDragStartProc	(XmDragStartProc)(_XtInherit)
#define XmInheritDragCancelProc	(XmDragCancelProc)(_XtInherit)

/*
 * properties
 */
typedef struct _XmDndSourceProp {
    BYTE	byte_order;
    BYTE	protocol_version;
    CARD16	target_index;
    CARD32	selection;
} XmDndSourceProp;

typedef struct _XmDndReceiverProp {
    BYTE	byte_order;
    BYTE	protocol_version;
    BYTE	protocol_style;
    BYTE	pad1;
    CARD32	proxy_window;
    CARD16	num_drop_sites;
    CARD16	pad2;
    CARD32	total_size;
} XmDndReceiverProp;

/*
 * need to use some union hack since window and property are in
 * different order depending on the message ...
 */
typedef struct _XmDndTop {
    CARD32	src_window;
    CARD32	property;
} XmDndTop;

typedef struct _XmDndPot {
    INT16	x;
    INT16	y;
    CARD32	property;
    CARD32	src_window;
} XmDndPot;

typedef struct _XmDndMessage {
    BYTE	reason;
    BYTE	byte_order;
    CARD16	flags;
    CARD32	time;
    union {
	XmDndTop top;
	XmDndPot pot;
    } data;
} XmDndMessage;

/*
 * this structure is reused for DRAG_ATOM_PAIRS and DRAG_ATOMS...
 */
typedef struct _XmDndTargets {
    BYTE		byte_order;
    BYTE		protocol_version;
    CARD16		num_target_lists;
    CARD32		data_size;
    /* then come series of CARD16,CARD32,CARD32,CARD32... */
} XmDndTargets;

typedef struct _XmDndAtomPairs {
    BYTE		byte_order;
    BYTE		protocol_version;
    CARD16		num_pairs;
    CARD32		data_size;
} XmDndAtomPairs;

typedef struct _XmDndAtoms {
    BYTE		byte_order;
    BYTE		protocol_version;
    CARD16		num_atoms;
    CARD32		data_size;
} XmDndAtoms;

/* protocol version */
#define DND_PROTOCOL_VERSION 0

#define DND_EVENT_TYPE_MASK  ((BYTE)0x80)
#define DND_EVENT_TYPE_SHIFT 7
#define DND_CLEAR_EVENT_TYPE  ((BYTE)0x7F)

/*
 * message_type is data[0] of the client_message
 * this return 1 (receiver bit up) or 0 (initiator)
 */
#define DND_GET_EVENT_TYPE(message_type) \
  ((char) (((message_type) & DND_EVENT_TYPE_MASK) >> DND_EVENT_TYPE_SHIFT))

/*
 * event_type can be 0 (initiator) or 1 (receiver)
 */
#define DND_SET_EVENT_TYPE(event_type) \
  (((BYTE)(event_type) << DND_EVENT_TYPE_SHIFT) & DND_EVENT_TYPE_MASK)

#define XmINITIATOR	0
#define XmRECEIVER	1

#define DND_OPERATION_MASK ((CARD16) 0x000F)
#define DND_OPERATION_SHIFT 0
#define DND_STATUS_MASK ((CARD16) 0x00F0)
#define DND_STATUS_SHIFT 4
#define DND_OPERATIONS_MASK ((CARD16) 0x0F00)
#define DND_OPERATIONS_SHIFT 8
#define DND_COMPLETION_MASK ((CARD16) 0xF000)
#define DND_COMPLETION_SHIFT 12

#define DND_GET_OPERATION(flags) \
  ((unsigned char) \
   (((flags) & DND_OPERATION_MASK) >> DND_OPERATION_SHIFT))

#define DND_SET_OPERATION(operation) \
  (((CARD16)(operation) << DND_OPERATION_SHIFT)\
   & DND_OPERATION_MASK)

#define DND_GET_STATUS(flags) \
  ((unsigned char) \
   (((flags) & DND_STATUS_MASK) >> DND_STATUS_SHIFT))

#define DND_SET_STATUS(status) \
  (((CARD16)(status) << DND_STATUS_SHIFT)\
   & DND_STATUS_MASK)

#define DND_GET_OPERATIONS(flags) \
  ((unsigned char) \
   (((flags) & DND_OPERATIONS_MASK) >> DND_OPERATIONS_SHIFT))

#define DND_SET_OPERATIONS(operation) \
  (((CARD16)(operation) << DND_OPERATIONS_SHIFT)\
   & DND_OPERATIONS_MASK)

#define DND_GET_COMPLETION(flags) \
  ((unsigned char) \
   (((flags) & DND_COMPLETION_MASK) >> DND_COMPLETION_SHIFT))

#define DND_SET_COMPLETION(completion) \
  (((CARD16)(completion) << DND_COMPLETION_SHIFT)\
   & DND_COMPLETION_MASK)


#define SWAP4BYTES(l) {\
	struct { unsigned t :32;} bit32;\
        char n,	*tp = (char *) &bit32;\
	bit32.t = l;\
	n = tp[0]; tp[0] = tp[3]; tp[3] = n;\
	n = tp[1]; tp[1] = tp[2]; tp[2] = n;\
        l = bit32.t;\
}

#define SWAP2BYTES(s) {\
	struct { unsigned t :16; } bit16;\
        char n, *tp = (char *) &bit16;\
	bit16.t = s;\
	n = tp[0]; tp[0] = tp[1]; tp[1] = n;\
        s = bit16.t;\
}

/***** Targets/Index stuff */

typedef struct {
    CARD32			atom;
    CARD16			namelen;
    CARD16			pad;		/* Align to 64 bits */
} XmDndAtomPair;

typedef struct {
    Atom			atom;
    Time			time;
} XmDndAtomsTableEntryRec, *XmDndAtomsTableEntry;

typedef struct {
    int				num_entries;
    XmDndAtomsTableEntry	entries;
} XmDndAtomsTableRec, *XmDndAtomsTable;

typedef struct {
    int				num_targets;
    Atom			*targets;
} XmDndTargetsTableEntryRec, *XmDndTargetsTableEntry;

typedef struct {
    int				num_entries;
    XmDndTargetsTableEntry	entries;
} XmDndTargetsTableRec, *XmDndTargetsTable;

/*
 * the next structure is used for reading and writing drag info
 */
typedef struct {
    char		*atoms;
    char		*atom_ptr;
    char		*atom_start;
    int			atom_size;
    int			atom_avail;
    char		*names;
    char		*name_ptr;
    char		*name_start;
    int			name_size;
    int			name_avail;
} XmDndBufMgrRec, *XmDndBufMgr;

/*
 * iccInfo in XmDragReceiverInfoStruct points
 * to this.
 */
typedef struct _XmShellDropSiteInfo {
    XmDndBufMgrRec info;
    unsigned char byte_order;
    int num_drop_sites;
} XmShellDropSiteInfoRec, *XmShellDropSiteInfo;

#define DSI_Info(x) \
    (((XmShellDropSiteInfo)(x))->info)

#define DSI_ByteOrder(x) \
    (((XmShellDropSiteInfo)(x))->byte_order)

#define DSI_NumDropSites(x) \
    (((XmShellDropSiteInfo)(x))->num_drop_sites)

/*
 * this is the per widget drop site info
 */
typedef struct _XmDropSiteInfo {

    struct _XmDropSiteInfo	*next;

    Boolean		external;
    Boolean		leaf;
    Boolean             implicit;		/* indicates this DS implicitly created */
    Boolean		isShell;
    Boolean		userRegion;
    Boolean		registered;

    struct _XmDropSiteInfo 	*parent;	/* SIMPLE's pointer to parent */
    struct _XmDropSiteInfo	**children;	/* list of children in parent */

    int 	 	inUpdate;
    short		target_index;
    short		numChildren;
    short		maxChildren;
    XmRegion		region;

    unsigned char	dropSiteOperations;
    XtCallbackProc	dragProc;
    XtCallbackProc	dropProc;
    Widget		dropSite;
    unsigned char	dropSiteType;
    unsigned char	animationStyle;
    unsigned char	dropSiteActivity;
    Atom		*importTargets;		/* volatile */
    Cardinal		numImportTargets;
    XRectangle		*dropRectangles;	/* volatile */
    Cardinal		numDropRectangles;
    Pixmap		animationPixmap;
    Cardinal		animationPixmapDepth;
    Pixmap		animationMask;

} XmDropSiteInfoRec, *XmDropSiteInfo;

/*
 * this is the table that the drop site manager uses to
 * store per-widget info
 */
typedef struct {
    long num_buckets;
    XmDropSiteInfo *buckets;
} DSInfoTable;

/*
 * this is used to pass info to the tree update proc
 * the numbering picks up where DragC.h leaves off.
 */
#define XmCR_ADD_DROP_SITE	9
#define XmCR_REMOVE_DROP_SITE	10

typedef struct {
    int	reason;
    XEvent *event;
    Widget widget;
} XmTreeUpdateCallbackStruct;

/*
 * for processing drag motion
 */
typedef struct {
    Time time;
    Window window;
    Window subwindow;
    Position xroot, yroot;
    unsigned int state;
} XmMotionRec;

typedef struct {
    XmDragReceiverInfo	curReceiver;
    int			num_motions;
    int			max_motions;
    XmMotionRec		*motions;
} XmMotionBuffer;

/*
 * this union is used as a short cut in DragC.c
 */
typedef union {
    XmAnyICCCallbackStruct			any;
    XmTopLevelEnterCallbackStruct		tle;
    XmTopLevelLeaveCallbackStruct		tll;
    XmDropSiteEnterCallbackStruct		dse;
    XmDropSiteLeaveCallbackStruct		dsl;
    XmDragMotionCallbackStruct			dm;
    XmOperationChangedCallbackStruct		oc;
    XmDropStartCallbackStruct			ds;
    XmDropFinishCallbackStruct			df;
    XmDragDropFinishCallbackStruct		ddf;
} XmDragDropCallbackStruct;

/*
 * these next two are used for notifying the DropSMgr from the DragC.
 */
typedef struct {
    XmDragOverShellWidget	dos;
    Boolean			sourceIsExternal;
    Window			window;
    Widget			shell;
    Position			xOrigin, yOrigin;
    Dimension			width, height;
    XtPointer			iccInfo;
} XmDragTopLevelClientDataStruct;

typedef struct {
    XmDragOverShellWidget	dos;
    Window			window;
} XmDragMotionClientDataStruct;

/*
 * some atoms
 */
#define _XA_MOTIF_DRAG_WINDOW		"_MOTIF_DRAG_WINDOW"
#define _XA_MOTIF_DRAG_PROXY_WINDOW	"_MOTIF_DRAG_PROXY_WINDOW"
#define _XA_MOTIF_DRAG_ATOM_PAIRS	"_MOTIF_DRAG_ATOM_PAIRS"
#define _XA_MOTIF_DRAG_ATOMS		"_MOTIF_DRAG_ATOMS"
#define _XA_MOTIF_ATOM_0		"_MOTIF_ATOM_0"
#define _XA_MOTIF_ATOM_FORMAT		"_MOTIF_ATOM_%d"
#define _XA_MOTIF_DRAG_TARGETS		"_MOTIF_DRAG_TARGETS"
#define _XA_MOTIF_DRAG_INITIATOR_INFO	"_MOTIF_DRAG_INITIATOR_INFO"
#define _XA_MOTIF_DRAG_RECEIVER_INFO	"_MOTIF_DRAG_RECEIVER_INFO"
#define _XA_MOTIF_MESSAGE		"_MOTIF_DRAG_MESSAGE"
#define _XA_MOTIF_WM_QUERY_N		"_MOTIF_WM_QUERY_%d"
#define _XA_MOTIF_DRAG_AND_DROP_MESSAGE	"_MOTIF_DRAG_AND_DROP_MESSAGE"

/* functions */

Boolean _XmGetDragReceiverInfo(Display *display, Window win,
                               XmDragReceiverInfo ri);
void _XmSetDragReceiverInfo(Widget w, Widget widget);
void _XmClearDragReceiverInfo(Widget shell);
void _XmFreeDragReceiverInfo(XmShellDropSiteInfo di);
unsigned char _XmByteOrder(void);
int _XmMessageTypeToReason(BYTE reason);
void _XmICCEventToICCCallback(XmDndMessage *msg, XmDragDropCallbackStruct *cbs);

/* DragBS.c */
int _XmReadDragBuffer(XmDndBufMgr bmgr, Boolean read_string,
                      char *retbuf, int bufsize);
int _XmWriteDragBuffer(XmDndBufMgr bmgr, Boolean write_string,
                       char *data, int size);


/*
 * resources from DropSMgrI.c
 */
extern XtResource _XmDSResources[];
extern int _XmNumDSResources;
/* functions for MBC */
void _XmDSIAddChild(void* a, void* b, int c);
void _XmDSIRemoveChild(void* a, void* b);
void _XmDSIDestroy(void* a, int b);
int _XmDSIGetBorderWidth(void* a);
int _XmDSIReplaceChild(void* a);
int _XmDSIGetChildPosition(void* a, void* b);
void _XmDSISwapChildren(void* a, void* b, void* c);

/* from TextIn.c */
void _Lttext_process_drop(Widget w, XtPointer client_data, XtPointer call_data);


#endif /* _XMI_DRAGDROPI_H */
