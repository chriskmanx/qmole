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


#ifndef UI_SPINNER_H
#define UI_SPINNER_H


#define SPINNER_SPEED 100


extern const guint8 icon_spinner[];
extern const guint8 icon_tabcomp[];

/* if path is NULL, the built in spinner is used,
 * otherwise path must be the location of the first image of the
 * spinner without the 00.png portion of the pathname, example:
 *
 *     /path/to/spinnerimg_
 *
 * the files required are then:
 *
 *     /path/to/spinnerimg_00.png   non-animated state
 *     /path/to/spinnerimg_01.png   animation frame 1
 *     /path/to/spinnerimg_02.png   animation frame 2
 *     [continues to last frame...]
 */
GtkWidget *spinner_new(const gchar *path, gint interval);

void spinner_set_interval(GtkWidget *spinner, gint interval);
void spinner_step(GtkWidget *spinner, gboolean reset);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
