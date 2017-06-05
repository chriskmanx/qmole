/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* session.c - XSMP client support */

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <gtk/gtk.h>
#include <X11/SM/SMlib.h>
#include <pwd.h>
#include "global.h"
#include "filer.h"
#include "main.h"
#include "pinboard.h"
#include "panel.h"
#include "sc.h"
#include "session.h"

#define ROX_FILER_URI "http://rox.sourceforge.net/2005/interfaces/ROX-Filer"

static gboolean use_0launch;
gboolean session_auto_respawn = FALSE;	/* If we were started as 'rox -S' */

static void save_state(SmClient *client)
{
	FilerWindow *filer_window;
	Panel *panel;
	Pinboard *pinboard = current_pinboard;
	GList *list;
	GPtrArray *restart_cmd = g_ptr_array_new();
	SmPropValue *program;
	gchar *types[] = { "-t", "-B", "-l", "-r" };
	gint i, nvals;
	
	if (use_0launch)
	{
		g_ptr_array_add(restart_cmd, "0launch");
		g_ptr_array_add(restart_cmd, ROX_FILER_URI);
	}
	else
	{
		sc_get_prop_value(client, SmProgram, &program, &nvals);
		g_ptr_array_add(restart_cmd, program->value);
	}
	
	g_ptr_array_add(restart_cmd, "-c");
	g_ptr_array_add(restart_cmd, client->id);
	
	for (list = all_filer_windows; list; list = list->next)
	{
		filer_window = (FilerWindow *)list->data;
		gdk_window_set_role(filer_window->window->window,
				    filer_window->sym_path);
		g_ptr_array_add(restart_cmd, "-d");
		g_ptr_array_add(restart_cmd, filer_window->sym_path);
	}
	
	if (session_auto_respawn)
	{
		for(i = 0; i < PANEL_NUMBER_OF_SIDES; i++)
		{
			panel = current_panel[i];
			if(!panel)
				continue;
			g_ptr_array_add(restart_cmd, types[panel->side]);
			g_ptr_array_add(restart_cmd, panel->name);
		}
		
		if (pinboard)
		{
			g_ptr_array_add(restart_cmd, "-p");
			g_ptr_array_add(restart_cmd, (gchar *) pinboard_get_name());
		}
	}
	else
	{
		g_ptr_array_add(restart_cmd, "-S");
	}
	
	sc_set_list_of_array_prop(client, SmRestartCommand, 
			(const gchar **) restart_cmd->pdata, restart_cmd->len);

	g_ptr_array_free(restart_cmd, TRUE);
}

/* Callbacks for various SM messages */

static gboolean save_yourself(SmClient *client)
{
	save_state(client);
	return TRUE;
}

static void die(SmClient *client)
{
	gtk_main_quit();
}

void session_init(const gchar *client_id)
{
	SmClient *client;
	struct passwd *pw;
	gchar *bin_path;
	gchar *clone_cmd[3];
	gchar *zerolaunch;

	if (!sc_session_up())
		return;

	pw = getpwuid(euid);

	zerolaunch = g_find_program_in_path("0launch");
	use_0launch = (zerolaunch != NULL);
	g_free(zerolaunch);

	if (use_0launch)
	{
		bin_path = "0launch";
		clone_cmd[0] = bin_path;
		clone_cmd[1] = ROX_FILER_URI,
		clone_cmd[2] = "-n";
	}
	else
	{
		bin_path = g_strconcat(app_dir, "/AppRun", NULL);
		clone_cmd[0] = bin_path;
		clone_cmd[1] = "-n";
		clone_cmd[2] = NULL;
	}

	client = sc_new(client_id);
	
	if (!sc_connect(client))
	{
		sc_destroy(client);
		return;
	}
	
	sc_set_array_prop(client, SmProgram, bin_path);
	sc_set_array_prop(client, SmUserID, pw->pw_name);
	sc_set_list_of_array_prop(client, SmCloneCommand,
			(const gchar **) clone_cmd,
			clone_cmd[2] == NULL ? 2 : 3);
	sc_set_card_prop(client, SmRestartStyleHint,
			session_auto_respawn ? SmRestartImmediately : SmRestartIfRunning);
	
	client->save_yourself_fn = &save_yourself;
	client->shutdown_cancelled_fn = NULL;
	client->save_complete_fn = NULL;
	client->die_fn = &die;
}
