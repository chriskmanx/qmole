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

#ifndef __SOURCEWINDOW_H__
#define __SOURCEWINDOW_H__

#include <glib.h>
#include <gtk/gtk.h>

#include "procmsg.h"

typedef struct _SourceWindow	SourceWindow;

struct _SourceWindow
{
	GtkWidget *window;
	GtkWidget *scrolledwin;
	GtkWidget *text;
	
	gboolean updating;
	gboolean deferred_destroy;
};

SourceWindow *source_window_create	(void);
void source_window_show			(SourceWindow	*sourcewin);
void source_window_show_msg		(SourceWindow	*sourcewin,
					 MsgInfo	*msginfo);

#endif /* __SOURCEWINDOW_H__ */
