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

#ifndef __INPUTDIALOG_H__
#define __INPUTDIALOG_H__

#include <glib.h>

gchar *input_dialog			(const gchar	*title,
					 const gchar	*message,
					 const gchar	*default_string);
gchar *input_dialog_with_checkbtn(const gchar	*title,
				   const gchar	*message,
				   const gchar	*default_string,
				   const gchar  *checkbtn_label,
				   gboolean *checkbtn_state);
gchar *input_dialog_with_invisible	(const gchar	*title,
					 const gchar	*message,
					 const gchar	*default_string);
gchar *input_dialog_with_invisible_checkbtn(const gchar	*title,
					   const gchar	*message,
					   const gchar	*default_string,
					   const gchar  *checkbtn_label,
					   gboolean *checkbtn_state);
gchar *input_dialog_combo		(const gchar	*title,
					 const gchar	*message,
					 const gchar	*default_string,
					 GList		*list);
gchar *input_dialog_combo_remember	(const gchar	*title,
					 const gchar	*message,
					 const gchar	*default_string,
					 GList		*list,
					 gboolean	*remember);
gchar *input_dialog_query_password	(const gchar	*server,
					 const gchar	*user);
gchar *input_dialog_query_password_keep	(const gchar	*server,
					 const gchar	*user,
					 gchar		**keep);
#endif /* __INPUTDIALOG_H__ */
