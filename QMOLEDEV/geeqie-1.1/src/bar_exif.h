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


#ifndef BAR_EXIF_H
#define BAR_EXIF_H

GtkWidget *bar_pane_exif_new_from_config(const gchar **attribute_names, const gchar **attribute_values);
void bar_pane_exif_update_from_config(GtkWidget *pane, const gchar **attribute_names, const gchar **attribute_values);

void bar_pane_exif_entry_add_from_config(GtkWidget *pane, const gchar **attribute_names, const gchar **attribute_values);

/* these are exposed for when duplication of the exif bar's text is needed */

const gchar **bar_exif_key_list;
const gint bar_exif_key_count;


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
