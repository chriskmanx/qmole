/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto & The Claws Mail Team
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

#ifndef __PREFS_ACTIONS_H__
#define __PREFS_ACTIONS_H__

#include "mainwindow.h"

void prefs_actions_read_config		(void);
void prefs_actions_write_config		(void);
void prefs_actions_open			(MainWindow	*mainwin);
void prefs_actions_rename_path		(const gchar *old_path,
					 const gchar *new_path);
gint prefs_actions_find_by_name		(const gchar *name);

#endif /* __PREFS_ACTIONS_H__ */
