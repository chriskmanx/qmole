/* $Id: e2p_clone.c 2978 2013-11-30 02:56:32Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark.

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
@file plugins/e2p_clone.c
@brief plugin for copying selected items, each with a new name, to their orignal location
*/

#include "emelfm2.h"
//#include <unistd.h>
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_task.h"
#include "e2_filelist.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "clone"

static PluginIface iface;

static gboolean _e2p_cloneQ (E2_ActionTaskData *qed);

/**
@brief copy selected item(s) each with a new name

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2p_clone (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_CLONE, art, from,
		_e2p_cloneQ, e2_task_refresh_lists));
}
static gboolean _e2p_cloneQ (E2_ActionTaskData *qed)
{
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
	GString *prompt = g_string_sized_new (NAME_MAX+64);
#ifdef E2_VFS
	VPATH sdata;
	VPATH ddata;
	sdata.spacedata = qed->currspace;
	ddata.spacedata = qed->currspace;
#endif
	GString *src = g_string_sized_new (PATH_MAX+NAME_MAX);
	GString *dest = g_string_sized_new (PATH_MAX+NAME_MAX);
	gchar *converted, *new_name;
	//curr_view->dir is utf-8
	gboolean check = e2_option_bool_get ("confirm-overwrite");
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//setup for tailoring over-write dialog
	gboolean multisrc =  (check) ? names->len > 1 : FALSE;
	OW_ButtonFlags extras = (multisrc) ? NOALL : NONE;

	e2_task_advise ();
	e2_filelist_disable_refresh ();

	for (count=0; count < names->len; count++, iterator++)
	{
		//".." entries filtered when names compiled
		converted = F_FILENAME_FROM_LOCALE ((*iterator)->filename);
		g_string_printf (prompt, "%s: <b>%s</b>", _("Enter new name for"), converted);
		DialogButtons result = 0;
		e2_filelist_enable_refresh ();  //allow updates while we wait
		CLOSEBGL
		DialogButtons result2 = e2_dialog_line_input (_("clone"), prompt->str,
			converted, extras, FALSE, &new_name);
		OPENBGL
		F_FREE (converted, (*iterator)->filename);
		e2_filelist_disable_refresh ();
		if (result2 == OK)
		{
			g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);
			converted = F_FILENAME_TO_LOCALE (new_name);
			g_string_printf (dest, "%s%s", curr_local, converted);
			g_free (new_name);
			F_FREE (converted, new_name);
			if (!strcmp (src->str, dest->str)) continue;

#ifdef E2_VFS
			ddata.path = dest->str;
			if (check && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
			if (check && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0)
#endif
			{	//item with the new name already exists
				*qed->status = E2_TASK_PAUSED;
				result = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
					&ddata,
#else
					dest->str,
#endif
					extras);
				*qed->status = E2_TASK_RUNNING;
				if (result == OK)
				{
#ifdef E2_VFS
					sdata.path = src->str;
					e2_task_backend_copy (&sdata, &ddata, E2_FTM_NORMAL);
#else
					e2_task_backend_copy (src->str, dest->str, E2_FTM_NORMAL);
#endif
				}
/*				else if (result == YES_TO_ALL)
				{
					do something smart about multiple-renames
				} */
				else if (result == NO_TO_ALL)
					break;
			}
			else
			{
#ifdef E2_VFS
				sdata.path = src->str;
				e2_task_backend_copy (&sdata, &ddata, E2_FTM_NORMAL);
#else
				e2_task_backend_copy (src->str, dest->str, E2_FTM_NORMAL);
#endif
			}
		}
		else if (result2 == NO_TO_ALL)
			break;
	}
	e2_window_clear_status_message ();
	e2_filelist_request_refresh (curr_view->dir, TRUE);	//other pane refreshed normally
	g_string_free (prompt,TRUE);
	g_string_free (src,TRUE);
	g_string_free (dest,TRUE);
	e2_filelist_enable_refresh ();
	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(6),_("clone"),_e2p_clone,
		_("C_lone.."),
		_("Copy selected items, each with new name, to the current directory"),
		"plugin_"ANAME E2ICONTB)
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
