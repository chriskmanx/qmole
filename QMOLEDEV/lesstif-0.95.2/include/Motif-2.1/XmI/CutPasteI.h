/**
 *
 * $Id: CutPasteI.h,v 1.1 2004/08/28 19:23:29 dannybackx Exp $
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

#ifndef _XMI_CUTPASTEI_H
#define _XMI_CUTPASTEI_H

#include <Xm/CutPasteP.h>

/*
 * various atoms used by the clipboard.  Some of these appear on the root
 * window; others are apparently involved in the IPC mechanism (especially
 * for incremental copies involving callbacks).
 */
#define _XA_MOTIF_CLIP_HEADER			"_MOTIF_CLIP_HEADER"
#define _XA_MOTIF_CLIP_NEXT_ID			"_MOTIF_CLIP_NEXT_ID"
#define _XA_MOTIF_CLIP_ITEM_N			"_MOTIF_CLIP_ITEM_%d" 
#define _XA_MOTIF_CLIP_TIME			"_MOTIF_CLIP_TIME"
#define _XA_MOTIF_CLIP_DATA_DELETE		"_MOTIF_CLIP_DATA_DELETE"
#define _XA_MOTIF_CLIP_DATA_REQUEST		"_MOTIF_CLIP_DATA_REQUEST"
#define _XA_MOTIF_CLIP_FORMAT_S			"_MOTIF_CLIP_FORMAT_%s"
#define _XA_MOTIF_CLIP_LOCK			"_MOTIF_CLIP_LOCK"
#define _XA_MOTIF_CLIP_LOCK_ACCESS_VALID	"_MOTIF_CLIP_LOCK_ACCESS_VALID"
#define _XA_MOTIF_CLIP_MESSAGE			"_MOTIF_CLIP_MESSAGE"

#define _XA_CLIPBOARD				"CLIPBOARD"
#define _XA_CLIP_TEMPORARY			"CLIP_TEMPORARY"
#define _XA_INCR				"INCR"

#define XmCLIP_HEADER		0
#define XmCLIP_NEXT_ID		1
#define XmCLIP_LOCK		2

#define XmCLIP_PROP_DONT_CARE	0
#define XmCLIP_PROP_FORMAT	1
#define XmCLIP_PROP_ITEM	2
#define XmCLIP_PROP_HEADER	3

/*
 * management structure for the clipboard itself.  This actually appears as a
 * property on the root window of your display, if you've been fooling around
 * with the clipboard in M*tif.  Use xprop to view the contents of this
 * property; halting clients at various points when manipulating the clipboard
 * is "interesting".
 * NOTE: This needs to be made machine independent. (INT32 instead of int, etc).
 */
typedef struct _XmClipboard {
    int			prop_id;
    int			cpad1;
    int			static_item_count;
    int			clip_size;
    int			current_item;
    int			last_item;
    int			next_delete;
    int			current_item2;
    int			by_name_id;
    int			item_count;
    Time		copy_lock_time;
    Time		retrieve_lock_time;
    int			offset;
    Window		clipboard_selection_owner;
    int			retrieve_locked;
    int			copy_locked;
} XmClipboard;

/*
 * management structure for the items on the clipboard.  These actually
 * appear as properties on the root window of your display, if you've been
 * fooling around with the clipboard in M*tif.  Use xprop to view the
 * contents of these properties.
 * BTW: the XmString argument to XmClipboardStartCopy() appears as a property,
 * too.
 * NOTE: This needs to be made machine independent. (INT32 instead of int, etc).
 */
typedef struct _XmClipboardItem {
    int			prop_id;
    int			ipad1;
    Display		*display;
    Window		window;
    int			item_id;
    int			label_id;
    int			item_size;
    int			format_count;
    int			del_format_count;
    int			by_name;
    int			marked_for_delete;
    int			ipad2;
    XmCutPasteProc	callback;
    Widget		widget;
    Window		widget_window;
} XmClipboardItem;

/*
 * management structure for the formats for items on the clipboard.  These
 * actually appear as properties on the root window of your display, if
 * you've been fooling around with the clipboard in M*tif.  Use xprop to view
 * the contents of these properties.
 * BTW: the data argument to XmClipboardCopy() appears as a property, too,
 * after EndCopy has been called (straight copy, not by name).
 * NOTE: This needs to be made machine independent. (INT32 instead of int, etc).
 */
typedef struct _XmClipboardFormat {
    int			prop_id;
    long		item_id;
    Display		*display;
    Window		window;
    Widget		widget;
    Window		widget_window;
    XmCutPasteProc	callback;
    int			length;
    int			data_id;
    Atom		atom;
    int			name_len;
    int			marked_for_delete;
    int			by_name;
    int			format_id;
    long		private_id;
    int			offset;
} XmClipboardFormat;

/*
 * structure for the clipboard lock
 */
typedef struct {
    Window lockowner;
    int lockdepth;
} XmClipboardLockRec;

/*
 * match structure for selection messages
 */
typedef struct _XmMatchFormat {
    Window window;
    int id;
} XmMatchFormat;

typedef struct _XmMatchSelection {
    Display *display;
    Window window;
    Window selection_owner;
    Time retrieve_time;
    char *format_name;
    int len;
    unsigned *data;
    int type;
    Boolean incr;
    Boolean selection_done;
} XmMatchSelection;

#endif /* _XMI_CUTPASTEI_H */
