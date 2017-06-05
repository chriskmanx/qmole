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


#ifndef COLLECT_DLG_H
#define COLLECT_DLG_H


void collection_dialog_save_as(gchar *path, CollectionData *cd);
void collection_dialog_save_close(gchar *path, CollectionData *cd);

void collection_dialog_load(gchar *path);
void collection_dialog_append(gchar *path, CollectionData *cd);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
