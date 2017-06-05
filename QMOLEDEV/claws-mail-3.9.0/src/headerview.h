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

#ifndef __HEADERVIEW_H__
#define __HEADERVIEW_H__

#include <glib.h>
#include <gtk/gtk.h>

#include "procmsg.h"
#include "viewtypes.h"

struct _HeaderView
{
	GtkWidget *hbox;

	GtkWidget *from_header_label;
	GtkWidget *from_body_label;
	GtkWidget *to_header_label;
	GtkWidget *to_body_label;
	GtkWidget *ng_header_label;
	GtkWidget *ng_body_label;
	GtkWidget *subject_header_label;
	GtkWidget *subject_body_label;
	GtkWidget *tags_header_label;
	GtkWidget *tags_body_label;

	GtkWidget *image;
};

HeaderView *headerview_create	(void);
void headerview_init		(HeaderView	*headerview);
void headerview_show		(HeaderView	*headerview,
				 MsgInfo	*msginfo);
void headerview_clear		(HeaderView	*headerview);
void headerview_set_visibility	(HeaderView	*headerview,
				 gboolean	 visibility);
void headerview_destroy		(HeaderView	*headerview);
void headerview_set_font	(HeaderView 	*headerview);
#endif /* __HEADERVIEW_H__ */
