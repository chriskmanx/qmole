/**
 *
 * $Id: AtomMgrI.h,v 1.1 2004/08/28 19:23:29 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
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

#ifndef _XMI_ATOMMGRI_H
#define _XMI_ATOMMGRI_H

#include <Xm/XmP.h>

/*
 * known format atoms
 */
#define _XA_TARGETS			"TARGETS"
#define _XA_MULTIPLE			"MULTIPLE"
#define _XA_TIMESTAMP			"TIMESTAMP"
#define _XA_STRING			"STRING"
#define _XA_COMPOUND_TEXT		"COMPOUND_TEXT"
#define _XA_LIST_LENGTH			"LIST_LENGTH"
#define _XA_PIXMAP			"PIXMAP"
#define _XA_DRAWABLE			"DRAWABLE"
#define _XA_BITMAP			"BITMAP"
#define _XA_FOREGROUND			"FOREGROUND"
#define _XA_BACKGROUND			"BACKGROUND"
#define _XA_COLORMAP			"COLORMAP"
#define _XA_ODIF			"ODIF"
#define _XA_OWNER_OS			"OWNER_OS"
#define _XA_FILE_NAME			"FILE_NAME"
#define _XA_HOST_NAME			"HOST_NAME"
#define _XA_CHARACTER_POSITION		"CHARACTER_POSITION"
#define _XA_LINE_NUMBER			"LINE_NUMBER"
#define _XA_COLUMN_NUMBER		"COLUMN_NUMBER"
#define _XA_LENGTH			"LENGTH"
#define _XA_USER			"USER"
#define _XA_PROCEDURE			"PROCEDURE"
#define _XA_MODULE			"MODULE"
#define _XA_PROCESS			"PROCESS"
#define _XA_TASK			"TASK"
#define _XA_CLASS			"CLASS"
#define _XA_NAME			"NAME"
#define _XA_CLIENT_WINDOW		"CLIENT_WINDOW"

/*
 * other atoms
 */
#define _XA_ATOM_PAIR			"ATOM_PAIR"
#define _XA_AVERAGE_WIDTH		"AVERAGE_WIDTH"
#define _XA_DELETE			"DELETE"
#define _XA_INSERT_SELECTION		"INSERT_SELECTION"
#define _XA_MOTIF_DESTINATION		"MOTIF_DESTINATION"
#define _XA_PIXEL_SIZE			"PIXEL_SIZE"
#define _XA_RESOLUTION_Y		"RESOLUTION_Y"
#define _XA_TEXT			"TEXT"
#define _XA_WM_PROTOCOLS		"WM_PROTOCOLS"
#define _XA_WM_STATE			"WM_STATE"
#define _XA_WM_DELETE_WINDOW		"WM_DELETE_WINDOW"
#define _XA_XmTRANSFER_SUCCESS		"XmTRANSFER_SUCCESS"
#define _XA_XmTRANSFER_FAILURE		"XmTRANSFER_FAILURE"
#define _XA_XM_TEXT_I_S_PROP		"_XM_TEXT_I_S_PROP"

/*
 * No longer relevant, but keep around
 */

#if XtSpecificationRelease < 5

/*
 * Hey folks! This is internal stuff - so stay away from this! Even many
 * Motif distributions do not have this include file (depending on the
 * release level, I think...)
 */
typedef struct _XmAtomCacheEntryRec {
    String   AtomName;         /* primary key for NamesHashTable */
    Display *Dsp;              /* secondary key */
    Atom     AtomID;           /* primary key for AtomsHashTable */
} XmAtomCacheEntryRec, *XmAtomCacheEntry;

typedef struct _XmAtomCacheRec {
    unsigned int     InUse;    /* number of used entries in the hash table */
    unsigned int     HashMask; /* what bits do we count on?                */
    unsigned int     RehashValue;
    XmAtomCacheEntry Entries;
} XmAtomCacheRec, *XmAtomCache;

#endif /* XtSpecificationRelease < 5 */

#ifdef XM_ATOM_CACHE
extern void _XmInternAtomAndName(Display *display, Atom atom, String name);
#endif

extern void _XmFlushAtomsForDisplay(Display *Dsp);

#endif /* _XMI_ATOMMGRI_H */
