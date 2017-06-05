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


#ifndef PREFERENCES_H
#define PREFERENCES_H


void show_config_window(void);
void show_about_window(void);

/* reusable helper functions */
void config_entry_to_option(GtkWidget *entry, gchar **option, gchar *(*func)(const gchar *));


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
