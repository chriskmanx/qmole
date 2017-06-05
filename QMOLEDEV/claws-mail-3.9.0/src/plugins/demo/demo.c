/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 the Claws Mail Team
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

#include <glib.h>
#include <glib/gi18n.h>


#include "version.h"
#include "claws.h"
#include "plugin.h"
#include "utils.h"
#include "hooks.h"
#include "log.h"

#define PLUGIN_NAME (_("Demo"))

gboolean my_log_hook(gpointer source, gpointer data)
{
	LogText *logtext = (LogText *)source;

	g_print("*** Demo Plugin log: %s\n", logtext->text);

	return FALSE;
}

static guint hook_id;

gint plugin_init(gchar **error)
{
	if (!check_plugin_version(MAKE_NUMERIC_VERSION(2,9,2,72),
				VERSION_NUMERIC, PLUGIN_NAME, error))
		return -1;

	hook_id = hooks_register_hook(LOG_APPEND_TEXT_HOOKLIST, my_log_hook, NULL);
	if (hook_id == -1) {
		*error = g_strdup(_("Failed to register log text hook"));
		return -1;
	}

	g_print("Demo plugin loaded\n");

	return 0;
}

gboolean plugin_done(void)
{
	hooks_unregister_hook(LOG_APPEND_TEXT_HOOKLIST, hook_id);

	g_print("Demo plugin unloaded\n");
	return TRUE;
}

const gchar *plugin_name(void)
{
	return PLUGIN_NAME;
}

const gchar *plugin_desc(void)
{
	return _("This Plugin is only a demo of how to write plugins for Claws Mail. "
	         "It installs a hook for new log output and writes it to stdout."
	         "\n\n"
	         "It is not really useful.");
}

const gchar *plugin_type(void)
{
	return "Common";
}

const gchar *plugin_licence(void)
{
	return "GPL3+";
}

const gchar *plugin_version(void)
{
	return VERSION;
}

struct PluginFeature *plugin_provides(void)
{
	static struct PluginFeature features[] = 
		{ {PLUGIN_OTHER, N_("Demo")},
		  {PLUGIN_NOTHING, NULL}};
	return features;
}
