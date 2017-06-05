/* $Header: /cvsroot/lesstif/lesstif/test/extra/daniel/Dnd.c,v 1.2 2002/05/15 10:55:06 amai Exp $ */

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

/***********************************************************/
/* Motif Drag&Drop Dynamic Protocol messaging API code */
/* Only requires Xlib layer - not MT safe */
/* Author: Daniel Dardailler, daniel@x.org */
/***********************************************************/

#include <stdio.h>

#include "DndP.h"

static Atom atom_message_type, atom_receiver_info, atom_src_property_type;

unsigned char _DndByteOrder (void)
{
    static unsigned char byte_order = 0;

    if (!byte_order) {
	unsigned int endian = 1;
	byte_order = (*((char *)&endian))?'l':'B';
    }
    return byte_order ;
}


static void
InitAtoms(Display * dpy) 
{
    if (atom_message_type) return ; /* already Initialized */

    /* Init atoms used in the com */
    atom_message_type =  XInternAtom(dpy, 
				    "_MOTIF_DRAG_AND_DROP_MESSAGE", False);
    atom_src_property_type = XInternAtom(dpy, 
					"_MOTIF_DRAG_INITIATOR_INFO", False);
    atom_receiver_info = XInternAtom(dpy, "_MOTIF_DRAG_RECEIVER_INFO", False);
}



/* Position the _MOTIF_DRAG_INITIATOR_INFO property on the source window.
   Called by the source of the drag to indicate the
   supported target list (thru the index scheme) */
extern void 
DndWriteSourceProperty(Display * dpy,
		       Window window, Atom dnd_selection,
		       Atom * targets, unsigned short num_targets)
{
    DndSrcProp src_prop ;

    InitAtoms(dpy);

    src_prop.byte_order = _DndByteOrder() ;
    src_prop.protocol_version = DND_PROTOCOL_VERSION;
    src_prop.target_index = _DndTargetsToIndex(dpy, targets, num_targets);
    if (src_prop.target_index == -1) return ;
    src_prop.selection = dnd_selection ; 

    /* write the buffer to the property */
    XChangeProperty (dpy, window, dnd_selection, atom_src_property_type,
		     8, PropModeReplace, (unsigned char *)&src_prop,
		     sizeof(DndSrcProp));
}

extern void
DndReadSourceProperty(Display * dpy,
		      Window window, Atom dnd_selection,
		      Atom ** targets, unsigned short * num_targets)
{
    DndSrcProp * src_prop = NULL;
    Atom type ;
    int format ;
    unsigned long bytesafter, lengthRtn;

    InitAtoms(dpy);

    if ((XGetWindowProperty (dpy, window, dnd_selection, 0L, 100000L,
			    False, atom_src_property_type, &type,
			    &format, &lengthRtn, &bytesafter,
			    (unsigned char **) &src_prop) != Success)
	|| (type == None)) {
	printf("No property - DndReadSourceProperty return 0 \n");
	*num_targets = 0;
	return ;
    }

    if (src_prop->byte_order != _DndByteOrder()) {
	SWAP2BYTES(src_prop->target_index);
	SWAP4BYTES(src_prop->selection);
    }

    *num_targets = _DndIndexToTargets(dpy, src_prop->target_index, targets);

    XFree((char*)src_prop);
}


/* Position the _MOTIF_DRAG_RECEIVER_INFO property on the dropsite window.
   Called by the receiver of the drop to indicate the
   supported protocol style : dynamic, drop_only or none */
extern void 
DndWriteReceiverProperty(Display * dpy, Window window, 
			 unsigned char protocol_style)
{
    DndReceiverProp receiver_prop ;

    InitAtoms(dpy);

    receiver_prop.byte_order = _DndByteOrder() ;
    receiver_prop.protocol_version = DND_PROTOCOL_VERSION;
    receiver_prop.protocol_style = protocol_style ;
    receiver_prop.proxy_window =  None ;
    receiver_prop.num_drop_sites = 0 ;
    receiver_prop.total_size = sizeof(DndReceiverProp); 

    /* write the buffer to the property */
    XChangeProperty (dpy, window, atom_receiver_info, atom_receiver_info,
		     8, PropModeReplace, 
		     (unsigned char *)&receiver_prop,
		     sizeof(DndReceiverProp));
}


/* protocol style equiv (preregister stuff really) */
#define DND_DRAG_DROP_ONLY_EQUIV 3
#define DND_DRAG_DYNAMIC_EQUIV1  2
#define DND_DRAG_DYNAMIC_EQUIV2  4

extern void
DndReadReceiverProperty(Display * dpy, Window window,
			unsigned char * protocol_style)
{
    DndReceiverProp *receiver_prop = NULL ;
    Atom type ;
    int format ;
    unsigned long bytesafter, lengthRtn;

    InitAtoms(dpy);

    if ((XGetWindowProperty (dpy, window,
			    atom_receiver_info, /* _MOTIF_DRAG_RECEIVER_INFO */
			    0L, 100000L, False, atom_receiver_info,
			    &type, &format, &lengthRtn, &bytesafter,
			    (unsigned char **) &receiver_prop) != Success)
	|| (type == None)) {
	/* no property => no d&d */
	*protocol_style = DND_DRAG_NONE ;
	return ;
    }

    /* in dynamic we don't really care about byte swapping since
       the pertinent info is all expressed in one byte quantities */
    *protocol_style = receiver_prop->protocol_style;

    /* do the equiv stuff for Motif pre-register */
    if (*protocol_style == DND_DRAG_DROP_ONLY_EQUIV)
	*protocol_style = DND_DRAG_DROP_ONLY ;
    else
    if (*protocol_style == DND_DRAG_DYNAMIC_EQUIV1 ||
	*protocol_style == DND_DRAG_DYNAMIC_EQUIV2)
	*protocol_style = DND_DRAG_DYNAMIC ;
    
    XFree((void *)receiver_prop) ;

}

/* Produce a client message to be sent by the caller */
extern void
DndFillClientMessage(Display * dpy, Window window,
		     XClientMessageEvent *cm, 
		     DndData * dnd_data,
		     char receiver)
{
    DndMessage * dnd_message = (DndMessage*)&cm->data.b[0] ;

    InitAtoms(dpy);

    cm->display = dpy;
    cm->type = ClientMessage;
    cm->serial = LastKnownRequestProcessed(dpy);
    cm->send_event = True;
    cm->window = window;
    cm->format = 8;
    cm->message_type = atom_message_type ;/* _MOTIF_DRAG_AND_DROP_MESSAGE */

    dnd_message->reason = dnd_data->reason | DND_SET_EVENT_TYPE(receiver);

    dnd_message->byte_order = _DndByteOrder();

    /* we're filling in flags with more stuff that necessary,
       depending on the reason, but it doesn't matter */
    dnd_message->flags = 0 ;    
    dnd_message->flags |= DND_SET_STATUS(dnd_data->status) ;
    dnd_message->flags |= DND_SET_OPERATION(dnd_data->operation) ;
    dnd_message->flags |= DND_SET_OPERATIONS(dnd_data->operations) ;
    dnd_message->flags |= DND_SET_COMPLETION(dnd_data->completion) ;

    dnd_message->time = dnd_data->time ;

    switch(dnd_data->reason) {
    case DND_DROP_SITE_LEAVE: break ;
    case DND_TOP_LEVEL_ENTER:
    case DND_TOP_LEVEL_LEAVE:
	dnd_message->data.top.src_window = dnd_data->src_window ;
	dnd_message->data.top.property = dnd_data->property ;
	break ; /* cannot fall thru since the byte layout is different in
		   both set of messages, see top and pot union stuff */

    case DND_DRAG_MOTION:
    case DND_OPERATION_CHANGED:
    case DND_DROP_SITE_ENTER:
    case DND_DROP_START:
	dnd_message->data.pot.x = dnd_data->x ; /* mouse position */
	dnd_message->data.pot.y = dnd_data->y ;
	dnd_message->data.pot.src_window = dnd_data->src_window ;
	dnd_message->data.pot.property = dnd_data->property ;
	break ;
    default:
	printf("DndWriteClientMessage default: warning\n");
	break ;
    }

}

extern Bool
DndParseClientMessage(XClientMessageEvent *cm, DndData * dnd_data,
		      char * receiver)
{
    DndMessage * dnd_message = (DndMessage*)&cm->data.b[0] ;

    InitAtoms(cm->display);

    if (cm->message_type != atom_message_type) {
	printf("Invalid _MOTIF_DRAG_AND_DROP_MESSAGE - DndReadClientMessage fails\n");
	return False ;
    }

    if (dnd_message->byte_order != _DndByteOrder()) {
	SWAP2BYTES(dnd_message->flags);
	SWAP4BYTES(dnd_message->time);
    } /* do the rest in the switch */

    dnd_data->reason = dnd_message->reason  ;
    if (DND_GET_EVENT_TYPE(dnd_data->reason)) 
	*receiver = 1 ;
    else
	*receiver = 0 ;
    dnd_data->reason &= DND_CLEAR_EVENT_TYPE ;

    dnd_data->time = dnd_message->time ;

    /* we're reading in more stuff that necessary. but who cares */
    dnd_data->status = DND_GET_STATUS(dnd_message->flags) ;
    dnd_data->operation = DND_GET_OPERATION(dnd_message->flags) ;
    dnd_data->operations = DND_GET_OPERATIONS(dnd_message->flags) ;
    dnd_data->completion = DND_GET_COMPLETION(dnd_message->flags) ;
    
    switch(dnd_data->reason) {
    case DND_TOP_LEVEL_ENTER:
    case DND_TOP_LEVEL_LEAVE:
	if (dnd_message->byte_order != _DndByteOrder()) {
	    SWAP4BYTES(dnd_message->data.top.src_window);
	    SWAP4BYTES(dnd_message->data.top.property);
	}
	dnd_data->src_window = dnd_message->data.top.src_window ;
	dnd_data->property = dnd_message->data.top.property ;
	break ; /* cannot fall thru, see above comment in write msg */

    case DND_DRAG_MOTION:
    case DND_OPERATION_CHANGED:
    case DND_DROP_SITE_ENTER:
    case DND_DROP_START:
	if (dnd_message->byte_order != _DndByteOrder()) {
	    SWAP2BYTES(dnd_message->data.pot.x);
	    SWAP2BYTES(dnd_message->data.pot.y);
	    SWAP4BYTES(dnd_message->data.pot.property);
	    SWAP4BYTES(dnd_message->data.pot.src_window);
	}
	dnd_data->x = dnd_message->data.pot.x ;
	dnd_data->y = dnd_message->data.pot.y ;
	dnd_data->property = dnd_message->data.pot.property ;
	dnd_data->src_window = dnd_message->data.pot.src_window ;
	break ;

    case DND_DROP_SITE_LEAVE:
        break;
    default:
	printf("DndReadClientMessage default: warning\n");
	break ;
    }

    return True ;
}
