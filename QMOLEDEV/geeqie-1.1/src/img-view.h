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


#ifndef IMG_VIEW_H
#define IMG_VIEW_H


void view_window_new(FileData *fd);
void view_window_new_from_list(GList *list);
void view_window_new_from_collection(CollectionData *cd, CollectInfo *info);

void view_window_colors_update(void);

gboolean view_window_find_image(ImageWindow *imd, gint *index, gint *total);



#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
