/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef REMOTE_H
#define REMOTE_H


typedef struct _RemoteConnection RemoteConnection;

typedef void RemoteReadFunc(RemoteConnection *rc, const gchar *text, GIOChannel *channel, gpointer data);

struct _RemoteConnection {
	gint server;
	gint fd;
	gchar *path;

	gint channel_id;
	RemoteReadFunc *read_func;
	gpointer read_data;

	GList *clients;
};


void remote_close(RemoteConnection *rc);
GList *remote_build_list(GList *list, gint argc, gchar *argv[], GList **errors);
void remote_help(void);
void remote_control(const gchar *arg_exec, GList *remote_list, const gchar *path,
		    GList *cmd_list, GList *collection_list);

RemoteConnection *remote_server_init(gchar *path, CollectionData *command_collection);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
