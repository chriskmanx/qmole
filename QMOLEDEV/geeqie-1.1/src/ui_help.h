/*
 * (SLIK) SimpLIstic sKin functions
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef UI_HELP_H
#define UI_HELP_H


GtkWidget *help_window_new(const gchar *title,
			   const gchar *subclass,
			   const gchar *path, const gchar *key);
void help_window_set_file(GtkWidget *window, const gchar *path, const gchar *key);
void help_window_set_key(GtkWidget *window, const gchar *key);

GtkWidget *help_window_get_box(GtkWidget *window);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
