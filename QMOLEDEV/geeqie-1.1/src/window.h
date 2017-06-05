/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Authors: Vladimir Nadvornik / Laurent Monin
 *
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef WINDOW_H
#define WINDOW_H

GtkWidget *window_new(GtkWindowType type, const gchar *role, const gchar *icon,
		      const gchar *icon_file, const gchar *subtitle);
void window_set_icon(GtkWidget *window, const gchar *icon, const gchar *file);
gboolean window_maximized(GtkWidget *window);

void help_window_show(const gchar *key);


#endif /* WINDOW_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
