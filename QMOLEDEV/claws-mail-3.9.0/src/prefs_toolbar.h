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

#ifndef __PREFS_CUSTOM_TOOLBAR_H__
#define __PREFS_CUSTOM_TOOLBAR_H__

#include "toolbar.h"

void prefs_toolbar_init		(void);
void prefs_toolbar_done		(void);

/* parent is a MainWindow, Compose, or MessageView, depending on the ToolbarType during registration */
typedef void (*ToolbarPluginCallback)(gpointer parent, const gchar *item_name, gpointer data);

void prefs_toolbar_register_plugin_item(ToolbarType toolbar_type, const gchar *plugin_name, const gchar *item_name, ToolbarPluginCallback cb, gpointer cb_data);
void prefs_toolbar_unregister_plugin_item(ToolbarType toolbar_type, const gchar *plugin_name, const gchar *item_name);
void prefs_toolbar_execute_plugin_item(gpointer parent, ToolbarType toolbar_type, const gchar *id);
void prefs_toolbar_update_action_btns(void);

#endif /* __PREFS_CUSTOM_TOOLBAR_H__ */ 
