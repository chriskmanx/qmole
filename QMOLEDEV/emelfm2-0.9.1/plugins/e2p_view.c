/* $Id: e2p_view.c 2840 2013-10-24 10:02:23Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file plugins/e2p_view.c
@brief plugin for view first selected item using the internal viewer
*/

#include "emelfm2.h"
#include "e2_plugins.h"
#include "e2_task.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "view"

static PluginIface iface;

static gboolean _e2p_viewQ (E2_ActionTaskData *qed);

/**
@brief view first selected item using the internal viewer

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_view (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_do_task (E2_TASK_VIEW, art, from, _e2p_viewQ, NULL));
}
static gboolean _e2p_viewQ (E2_ActionTaskData *qed)
{
	gboolean retval;
	GPtrArray *names = qed->names;
	if (names == NULL)
		return FALSE;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//".." entries filtered when names compiled
	gchar *localpath = e2_utils_strcat (qed->currdir, (*iterator)->filename);
#ifdef E2_VFS
	VPATH ddata = { localpath, qed->currspace };
#endif
	CLOSEBGL
#ifdef E2_VFS
	retval = e2_view_dialog_create (&ddata);
#else
	retval = e2_view_dialog_create (localpath);
#endif
	OPENBGL
	g_free (localpath);
	return retval;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	gchar *tip = g_strdup_printf (_("Open the first selected item with the %s text-file viewer"), PROGNAME);

	PLUGINIT_ONE_START(_A(6),_("view_with_plugin"),_e2p_view,
		_("_View"),
		tip,
		NULL)

	pa->flags = E2PA_CLEANTIP;

	PLUGINIT_ONE_MIDDLE

	g_free (tip);

	PLUGINIT_ONE_END
}

/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)
	return ret;
}
