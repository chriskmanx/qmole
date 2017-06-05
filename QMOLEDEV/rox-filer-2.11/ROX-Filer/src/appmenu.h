/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 *
 * This file by Diego Zamboni <zamboni@cerias.purdue.edu>
 */

#ifndef _APPMENU_H
#define _APPMENU_H

#include <gtk/gtk.h>

/* External interface */
int appmenu_add(const gchar *app_dir, DirItem *item, GtkWidget *menu);
void appmenu_remove(void);

#endif   /* _APPMENU_H */
