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

#ifndef _DnD_h
#define _DnD_h

/* This is a Dnd Dynamic Motif-compliant Protocol messaging API */
/* Only requires Xlib layer */

#include <X11/Xlib.h>
#include <X11/Xatom.h>

#define DndVersion 1
#define DndRevision 0
#define DndIncludeVersion (DndVersion * 10 + DndRevision)

/* The following values are used in the DndData structure */

/* protocol style */
#define DND_DRAG_NONE            0
#define DND_DRAG_DROP_ONLY       1
#define DND_DRAG_DYNAMIC         5

/* message type */
#define DND_TOP_LEVEL_ENTER   0
#define DND_TOP_LEVEL_LEAVE   1
#define DND_DRAG_MOTION       2
#define DND_DROP_SITE_ENTER   3
#define DND_DROP_SITE_LEAVE   4
#define DND_DROP_START        5
#define DND_OPERATION_CHANGED 8

/* operation(s) */
#define DND_NOOP	0L
#define DND_MOVE 	(1L << 0)
#define DND_COPY	(1L << 1)
#define DND_LINK	(1L << 2)

/* status */
#define DND_NO_DROP_SITE        1
#define DND_INVALID_DROP_SITE   2
#define DND_VALID_DROP_SITE	3

/* completion */
#define DND_DROP        0
#define DND_DROP_HELP   1
#define DND_DROP_CANCEL 2


/* Client side structure used in the API */
typedef struct {
    unsigned char       reason;  /* message type: DND_TOP_LEVEL_ENTER, etc */
    Time                time ;
    unsigned char       operation;
    unsigned char       operations;
    unsigned char       status;
    unsigned char       completion;
    short               x ;
    short               y ;
    Window              src_window ;
    Atom                property ;
} DndData ;

/* extern functions */
/* These provides for basic formatting of ICCCM message going
   back and forth during the D&D gesture */

extern void 
DndWriteSourceProperty(Display * dpy,
		       Window window, Atom dnd_selection,
		       Atom * targets, unsigned short num_targets);

extern void
DndReadSourceProperty(Display * dpy,
		      Window window, Atom dnd_selection,
		      Atom ** targets, unsigned short * num_targets);

extern void 
DndWriteReceiverProperty(Display * dpy, Window window, 
			 unsigned char protocol_style);

extern void
DndReadReceiverProperty(Display * dpy, Window window,
			unsigned char * protocol_style);

extern void
DndFillClientMessage(Display * dpy, Window window,
		     XClientMessageEvent *cm,
		     DndData * dnd_data,
		     char receiver);

extern Bool
DndParseClientMessage(XClientMessageEvent *cm,
		      DndData * dnd_data,
		      char * receiver);

#endif /* _Dnd_h */
