/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef DND_H
#define DND_H

#define TARGET_APP_COLLECTION_MEMBER_STRING "application/x-" GQ_APPNAME_LC "-collection-member"
#define TARGET_APP_EXIF_ENTRY_STRING "application/x-" GQ_APPNAME_LC "-exif-entry"
#define TARGET_APP_KEYWORD_PATH_STRING "application/x-" GQ_APPNAME_LC "-keyword-path"

enum {
	TARGET_APP_COLLECTION_MEMBER,
	TARGET_APP_EXIF_ENTRY,
	TARGET_APP_KEYWORD_PATH,
	TARGET_URI_LIST,
	TARGET_TEXT_PLAIN
};


extern GtkTargetEntry dnd_file_drag_types[];
extern gint dnd_file_drag_types_count;

extern GtkTargetEntry dnd_file_drop_types[];
extern gint dnd_file_drop_types_count;


/* sets a drag icon to pixbuf, if items is > 1, text is drawn onto icon to indicate value */
void dnd_set_drag_icon(GtkWidget *widget, GdkDragContext *context, GdkPixbuf *pixbuf, gint items);

void dnd_set_drag_label(GtkWidget *widget, GdkDragContext *context, const gchar *text);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
