/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-sub-process.h - spawn a subprocess and perform IO

   Copyright (C) 2009 Stefan Walter

   Gnome keyring is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   Gnome keyring is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#ifndef EGG_SPAWN_H_
#define EGG_SUB_PROCESS_H_

#include <glib-object.h>

G_BEGIN_DECLS

typedef struct _EggSpawnCallbacks {
	gboolean (*standard_input) (int fd, gpointer user_data);
	gboolean (*standard_output) (int fd, gpointer user_data);
	gboolean (*standard_error) (int fd, gpointer user_data);
	void (*completed) (gpointer user_data);
	GDestroyNotify finalize_func;
	GSpawnChildSetupFunc child_setup;
} EggSpawnCallbacks;

guint              egg_spawn_async_with_callbacks (const gchar *working_directory,
                                                   gchar **argv,
                                                   gchar **envp,
                                                   GSpawnFlags flags,
                                                   GPid *child_pid,
                                                   EggSpawnCallbacks *callbacks,
                                                   gpointer user_data,
                                                   GMainContext *context,
                                                   GError **error);

gboolean           egg_spawn_sync_with_callbacks  (const gchar *working_directory,
                                                   gchar **argv,
                                                   gchar **envp,
                                                   GSpawnFlags flags,
                                                   GPid *child_pid,
                                                   EggSpawnCallbacks *callbacks,
                                                   gpointer user_data,
                                                   gint *exit_status,
                                                   GError **error);

gssize              egg_spawn_write_input          (int fd, gconstpointer data, gsize n_data);

gssize              egg_spawn_read_output          (int fd, gpointer data, gsize n_data);

G_END_DECLS

#endif /*EGG_SPAWN_H_*/
