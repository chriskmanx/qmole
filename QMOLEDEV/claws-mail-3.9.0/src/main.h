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

#ifndef __MAIN_H__
#define __MAIN_H__

#include <glib.h>
#include <gtk/gtk.h>
#ifdef MAEMO
#ifdef CHINOOK
#include <hildon/hildon-program.h>
#else
#include <hildon-widgets/hildon-program.h>
#include <hildon-widgets/hildon-window.h>
#endif
#endif
#include <sys/time.h>

extern gchar *prog_version;
extern gboolean debug_mode;

#ifdef MAEMO
extern HildonProgram *hildon_program;
#endif

typedef struct _SessionStats SessionStats;

struct _SessionStats
{
	gint received;
	gint sent;
	gint replied;
	gint forwarded;
	time_t time_started;
};

void app_will_exit	(GtkWidget *widget, gpointer data);
gboolean clean_quit	(gpointer data);
gboolean claws_is_exiting(void);
gboolean claws_is_starting(void);
gchar *claws_get_socket_name(void);
void main_set_show_at_startup(gboolean show);
gboolean claws_crashed(void);

#ifdef HAVE_NETWORKMANAGER_SUPPORT
gboolean networkmanager_is_online(GError **error);
#endif

#endif /* __MAIN_H__ */
