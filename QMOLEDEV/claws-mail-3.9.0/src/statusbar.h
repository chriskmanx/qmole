/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __STATUSBAR_H__
#define __STATUSBAR_H__

#include <glib.h>
#include <gtk/gtk.h>

GtkWidget *statusbar_create	(void);
void statusbar_puts		(GtkStatusbar	*statusbar,
				 const gchar	*str);
void statusbar_puts_all		(const gchar	*str);
void statusbar_print		(GtkStatusbar	*statusbar,
				 const gchar	*format, ...)
				 G_GNUC_PRINTF(2, 3);
void statusbar_print_all	(const gchar	*format, ...)
				 G_GNUC_PRINTF(1, 2);
void statusbar_pop_all		(void);

#ifdef MAEMO
void statuswindow_print_all	(const gchar	*format, ...)
				 G_GNUC_PRINTF(1, 2);
void statuswindow_pop_all	(void);
#else
#define statuswindow_print_all statusbar_print_all
#define statuswindow_pop_all   statusbar_pop_all
#endif

void statusbar_verbosity_set	(gboolean	 verbose);

void statusbar_progress_all	(gint done, gint total, gint step);
#define STATUSBAR_PUSH(mainwin, str) \
{ \
	if (mainwin->statusbar) \
		gtk_statusbar_push(GTK_STATUSBAR(mainwin->statusbar), \
			   mainwin->folderview_cid, str); \
	if (mainwin->hbox_stat) \
		gtkut_widget_draw_now(mainwin->hbox_stat); \
}

#define STATUSBAR_POP(mainwin) \
{ \
	if (mainwin->statusbar) \
		gtk_statusbar_pop(GTK_STATUSBAR(mainwin->statusbar), \
			  mainwin->folderview_cid); \
}

#endif /* __STATUSBAR_H__ */
