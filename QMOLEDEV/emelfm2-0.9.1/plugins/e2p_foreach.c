/* $Id: e2p_foreach.c 2840 2013-10-24 10:02:23Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

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
@file plugins/e2p_glob.c
@brief plugin for running a command on each selecting item, individually
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_plugins.h"
#include "e2_task.h"
#include "e2_dialog.h"
#include "e2_filelist.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "foreach"

static PluginIface iface;

//hackish mechanism to make each command non-static
//FIXME find a way to do this which eliminates _any_ chance of a race
static GList *each_command_list = NULL;
//mutex for added security with static list of commands
#ifdef USE_GLIB2_32
static GRecMutex eachcmd_mutex;
#else
static GStaticRecMutex eachcmd_mutex;
#endif

/**
@brief run a command on each selected item
This is called from a "run immediately" task thread
@param qed pointer to action data for the task

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2p_foreachQ (E2_ActionTaskData *qed)
{
#ifdef USE_GLIB2_32
	g_rec_mutex_lock (&eachcmd_mutex);
#else
	g_static_rec_mutex_lock (&eachcmd_mutex);
#endif
	if (each_command_list == NULL)
	{
#ifdef USE_GLIB2_32
		g_rec_mutex_unlock (&eachcmd_mutex);
#else
		g_static_rec_mutex_unlock (&eachcmd_mutex);
#endif
		return FALSE;	//possible race-condition ??
	}
	//get the command to run from the back of the commands list
	//(possible race if another foreach was initiated while this thread was being established !)
	GList *this = g_list_last (each_command_list);
	each_command_list = g_list_remove_link (each_command_list, this);
#ifdef USE_GLIB2_32
	g_rec_mutex_unlock (&eachcmd_mutex);
#else
	g_static_rec_mutex_unlock (&eachcmd_mutex);
#endif
	gint res;
	guint count;
	gboolean retval = TRUE;
	gchar *this_cmd, *utf;
	gchar *each_cmd = (gchar *) this->data;
	gchar *utfd = F_FILENAME_FROM_LOCALE (qed->currdir);
	GString *fullpath = g_string_sized_new (PATH_MAX+NAME_MAX);
	GPtrArray *names = qed->names;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;

	e2_filelist_disable_refresh ();

	for (count = 0; count < names->len; count++, iterator++)
	{
		utf = F_FILENAME_FROM_LOCALE ((*iterator)->filename);
		g_string_printf (fullpath, "%s%s", utfd, utf);  //separator comes with dir
		this_cmd = e2_utils_replace_name_macros (each_cmd, fullpath->str);
		if (this_cmd != each_cmd)
		{
			CLOSEBGL
			res = e2_command_run (this_cmd, E2_COMMAND_RANGE_DEFAULT, curr_view->treeview
#ifdef E2_COMMANDQ
			, TRUE
#endif
			);
			OPENBGL
			g_free (this_cmd);
		}
		else
			res = 1;	//no replacement of maccro in each_cmd, force error, quit

		F_FREE (utf, (*iterator)->filename);
		if (res != 0)
		{
			//FIXME advise user
			retval = FALSE;
			break;
		}
	}

	e2_filelist_enable_refresh ();

	g_free (each_cmd);
	g_list_free (this);
	g_string_free (fullpath, TRUE);

	return retval;
}
/**
@brief initiate repeated command from action data or user choice
This action takes its command from action runtime data, or if the latter is
NULL, will ask for the command here
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action was initiated, else FALSE
*/
static gboolean _e2p_foreach (gpointer from, E2_ActionRuntime *art)
{
	DialogButtons result;
	gchar *each_command;
	gpointer saved_command = NULL;

	if (art->data == NULL)
	{	//we don't yet know what to run for each item
		result = e2_dialog_line_input (_("repeat action"),
			_("Action to run for each selected item:"),
			"", 0, FALSE, &each_command);
		//a blank entry will not return OK
	}
	else
	{	//the action has been initiated from the commandline
		each_command = g_strdup ((gchar *)art->data);
		//but we do NOT want the command to be considered as names of items !
		saved_command = art->data;
		art->data = NULL;
		result = OK;
	}

	if (result == OK)
	{
		//FIXME support macros other than with english letters
		if (strstr (each_command, "%f") == NULL && strstr (each_command, "%p") == NULL)
		{
			gchar *freeme = each_command;
			each_command = g_strconcat (each_command, " %f", NULL);
			g_free (freeme);
		}
#ifdef USE_GLIB2_32
		g_rec_mutex_lock (&eachcmd_mutex);
#else
		g_static_rec_mutex_lock (&eachcmd_mutex);
#endif
		each_command_list = g_list_append (each_command_list, each_command);
#ifdef USE_GLIB2_32
		g_rec_mutex_unlock (&eachcmd_mutex);
#else
		g_static_rec_mutex_unlock (&eachcmd_mutex);
#endif
		gboolean success = e2_task_do_task (E2_TASK_FOREACH, art, from,
			_e2p_foreachQ, NULL);
		if (saved_command != NULL)
			art->data = saved_command;
		if (!success)
		{
			g_free (each_command);
#ifdef USE_GLIB2_32
			g_rec_mutex_lock (&eachcmd_mutex);
#else
			g_static_rec_mutex_lock (&eachcmd_mutex);
#endif
			each_command_list = g_list_delete_link (each_command_list,
				g_list_last (each_command_list));
#ifdef USE_GLIB2_32
			g_rec_mutex_unlock (&eachcmd_mutex);
#else
			g_static_rec_mutex_unlock (&eachcmd_mutex);
#endif
		}
		return success;
	}
	return FALSE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONE_START(_A(6),_("foreach"),_e2p_foreach,
		_("For _each.."),
		_("Execute an entered command on each selected item separately"),
		"plugin_"ANAME E2ICONTB)

	//setup mutex to protect threaded access to foreach functionality
#ifdef USE_GLIB2_32
	g_rec_mutex_init (&eachcmd_mutex); //maybe not needed for static data
#else
	g_static_rec_mutex_init (&eachcmd_mutex);
#endif

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

	if (ret && each_command_list != NULL)
	{
#ifdef USE_GLIB2_32
		g_rec_mutex_lock (&eachcmd_mutex);
		e2_list_free_with_data (&each_command_list);
		g_rec_mutex_unlock (&eachcmd_mutex);
#else
		g_static_rec_mutex_lock (&eachcmd_mutex);
		e2_list_free_with_data (&each_command_list);
		g_static_rec_mutex_unlock (&eachcmd_mutex);
#endif
	}
	return ret;
}
