/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail Team
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

#ifndef PLUGIN_H
#define PLUGIN_H 1

#include <glib.h>

typedef struct _Plugin Plugin;

typedef enum {
	PLUGIN_NOTHING,
	PLUGIN_MIMEVIEWER,
	PLUGIN_MIMEPARSER,
	PLUGIN_FOLDERCLASS,
	PLUGIN_FILTERING,
	PLUGIN_PRIVACY,
	PLUGIN_NOTIFIER,
	PLUGIN_UTILITY,
	PLUGIN_OTHER
} PluginFeatureType;

struct PluginFeature {
	PluginFeatureType type;
	const gchar *subtype;
};

/* Functions to implement by the plugin */
gint plugin_init		(gchar		**error);
gboolean plugin_done		(void);
const gchar *plugin_name	(void);
const gchar *plugin_desc	(void);
const gchar *plugin_version	(void);
struct PluginFeature *plugin_provides (void);

/* Functions by the Claws Mail plugin system */
Plugin *plugin_load		(const gchar	 *filename,
				 gchar		**error);
void plugin_unload		(Plugin		 *plugin);
void plugin_load_all		(const gchar	 *type);
void plugin_unload_all		(const gchar	 *type);
void plugin_save_list		(void);
void plugin_load_standard_plugins (void);

GSList *plugin_get_list		(void);
GSList *plugin_get_unloaded_list(void);
const gchar *plugin_get_name	(Plugin		 *plugin);
const gchar *plugin_get_desc	(Plugin		 *plugin);
const gchar *plugin_get_version	(Plugin		 *plugin);
const gchar *plugin_get_error	(Plugin		 *plugin);
Plugin      *plugin_get_loaded_by_name(const gchar *name);
gint check_plugin_version	(guint32 minimum_claws_version,
				 guint32 compiled_claws_version,
				 const gchar *plugin_name,
				 gchar **error);
#endif
