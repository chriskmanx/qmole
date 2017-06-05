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


#ifndef BAR_SORT_H
#define BAR_SORT_H


GtkWidget *bar_sort_new_default(LayoutWindow *lw);
GtkWidget *bar_sort_new_from_config(LayoutWindow *lw, const gchar **attribute_names, const gchar **attribute_values);
void bar_sort_close(GtkWidget *bar);

void bar_sort_write_config(GtkWidget *bar, GString *outstr, gint indent);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
