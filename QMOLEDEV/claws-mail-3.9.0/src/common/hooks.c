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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include <glib.h>

#include "utils.h"
#include "hooks.h"

static GHashTable *hooklist_table;

static GHookList *hooks_get_hooklist(const gchar *hooklist_name)
{
	GHookList *hooklist;

	if (hooklist_table == NULL)
		hooklist_table = g_hash_table_new(g_str_hash, g_str_equal);
	
	hooklist = (GHookList *) g_hash_table_lookup(hooklist_table, hooklist_name);
	if (hooklist != NULL)
		return hooklist;
	
	hooklist = g_new0(GHookList, 1);
	g_hook_list_init(hooklist, sizeof(GHook));
	g_hash_table_insert(hooklist_table, g_strdup(hooklist_name), hooklist);
	
	return hooklist;
}

guint hooks_register_hook(const gchar *hooklist_name,
			  SylpheedHookFunction hook_func,
			  gpointer userdata)
{
	GHookList *hooklist;
	GHook *hook;

	cm_return_val_if_fail(hooklist_name != NULL, (guint)-1);
	cm_return_val_if_fail(hook_func != NULL, (guint)-1);
	
	hooklist = hooks_get_hooklist(hooklist_name);
	cm_return_val_if_fail(hooklist != NULL, (guint)-1);

	hook = g_hook_alloc(hooklist);
	cm_return_val_if_fail(hook != NULL, (guint)-1);

	hook->func = hook_func;
	hook->data = userdata;

	g_hook_append(hooklist, hook);

	debug_print("registed new hook for '%s' as id %lu\n", hooklist_name, hook->hook_id);

	return hook->hook_id;
}

void hooks_unregister_hook(const gchar *hooklist_name,
			   guint hook_id)
{
	GHookList *hooklist;
	GHook *hook;

	cm_return_if_fail(hooklist_name != NULL);

	hooklist = hooks_get_hooklist(hooklist_name);
	cm_return_if_fail(hooklist != NULL);

	hook = g_hook_get(hooklist, hook_id);
	cm_return_if_fail(hook != NULL);

	debug_print("unregisted hook %lu in '%s'\n", hook->hook_id, hooklist_name);

	g_hook_destroy(hooklist, hook_id);
}

struct MarshalData
{
	gpointer	source;
	gboolean	abort;
};

static void hooks_marshal(GHook *hook, gpointer data)
{
	gboolean (*func) (gpointer source, gpointer data);
	struct MarshalData *marshal_data = (struct MarshalData *)data;

	if (!marshal_data->abort) {
		func = hook->func;
		marshal_data->abort = func(marshal_data->source, hook->data);
	}
}

gboolean hooks_invoke(const gchar *hooklist_name,
		  gpointer source)
{
	GHookList *hooklist;
	struct MarshalData marshal_data;
	
	cm_return_val_if_fail(hooklist_name != NULL, FALSE);

	hooklist = hooks_get_hooklist(hooklist_name);
	cm_return_val_if_fail(hooklist != NULL, FALSE);

	marshal_data.source = source;
	marshal_data.abort = FALSE;

	g_hook_list_marshal(hooklist, TRUE, hooks_marshal, &marshal_data);

	return marshal_data.abort;
}
