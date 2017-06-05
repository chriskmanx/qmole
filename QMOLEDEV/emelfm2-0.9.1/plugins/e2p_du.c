/* $Id: e2p_du.c 2840 2013-10-24 10:02:23Z tpgww $

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
@file plugins/e2p_du.c
@brief plugin for determining the disk usage of selected items
*/

#include "emelfm2.h"
//#include <unistd.h>
#include <langinfo.h>
#include <dirent.h>
#include <string.h>
#include <fnmatch.h>
#include "e2_filelist.h"
#include "e2_plugins.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "du"

// binary factor
#define BFAC 1024.0
#define BFAC2 1048576.0
#define BFAC3 1073741824.0

typedef struct _E2_Du
{
	guint64 total;
	guint64 files;
	guint64 dirs;
	gboolean hidden;
} E2_Du;

static PluginIface iface;

/**
@brief update du counters
This is a callback for the treewalk function
Error message expects BGL to be open/off
@param localpath absolute path of item reported by the walker, localised string
@param statptr pointer to struct stat with data about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data pointer to data struct with total, files, dirs counters

@return E2TW_CONTINUE always
*/
static E2_TwResult _e2p_du_twcb (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, E2_Du *user_data)
{
	guint64 thissize;
	const gchar *ptr;
	ptr = strrchr (VPSTR (localpath), G_DIR_SEPARATOR);
	if (ptr == NULL)
		ptr = VPCSTR(localpath);
	else
		ptr++;	//skip the /
	if (ITEM_ISHIDDEN (ptr))
		user_data->hidden = TRUE;

	switch (status)
	{
		case E2TW_F:	//not directory or link
		case E2TW_SL:	//symbolic link
		case E2TW_SLN:	//symbolic link naming non-existing file
			user_data->files++;
			if (statptr->st_dev > 0)	//CHECKME
			{
				thissize = statptr->st_blocks * statptr->st_blksize;
				if (thissize > statptr->st_size)
					thissize = statptr->st_size;
				user_data->total += thissize;
			}
			break;
		case E2TW_DM:	//directory, not opened due to different file system (reported upstream)
		case E2TW_DL:	//directory, not opened due to tree-depth limit (reported upstream)
		case E2TW_DNR:	//unreadable directory (for which, error is reported upstream)
		case E2TW_DRR:	//directory now readable
		case E2TW_D:	//directory
			user_data->dirs++;
			if (statptr->st_dev > 0)	//CHECKME
			{
				thissize = statptr->st_blocks * statptr->st_blksize;
				if (thissize > statptr->st_size)
					thissize = statptr->st_size;
				user_data->total += thissize;
			}
			break;
//		case E2TW_DP:	//dir finished
//		case E2TW_NS:	//un-stattable item (for which, error is reported upstream)
		default:
			break;
	}
	return E2TW_CONTINUE;
}
/**
@brief thread function to iterate over active pane file list to get sizes
@param localpattern An allocated, localised, string, filename-pattern
 (possibly-wildcarded) to be matched before sizing a selected item or within a
 selected dir, or NULL to check all items
@return NULL
*/
static gpointer _e2p_du_all (gchar *localpattern)
{
//FIXME with E2_ASYNC, curr_view etc is not necessarily relevant
	TABLOG (curr_tab); //pointer to tab which was current when the thread was initiated
	FileInfo *info;
	guint64 total, files, dirs;
	gboolean hashidden;
	GString *text;
	gchar *curr_local, *local_path;
#ifdef E2_VFS
	VPATH ddata;
#endif

	//all counters initialized to 0
	E2_Du *cbdata = ALLOCATE0 (E2_Du);
	CHECKALLOCATEDWARN (cbdata, return NULL;)

	e2_filelist_disable_one_refresh (PANEACTIVE);

#ifdef E2_VFS
	ddata.spacedata = curr_view->spacedata;
#endif
	curr_local = D_FILENAME_TO_LOCALE (curr_view->dir);//always dup, to avoid dirchange race

	if (localpattern == NULL)
	{
		GList *base, *tmp;

		base = e2_fileview_get_selected_local (curr_view);
		for (tmp = base; tmp != NULL; tmp = tmp->next)
		{
			info = tmp->data;
//			printd (DEBUG, "fn: %s", info->filename);
			local_path = e2_utils_strcat (curr_local, info->filename);
#ifdef E2_VFS
			ddata.path = local_path;
#endif
			//if (!
#ifdef E2_VFS
			e2_fs_tw (&ddata, _e2p_du_twcb, cbdata, -1, E2TW_PHYS E2_ERR_NONE());
#else
			e2_fs_tw (local_path, _e2p_du_twcb, cbdata, -1, E2TW_PHYS E2_ERR_NONE());
#endif
			//)
			//{
				//FIXME handle error
			//}
			g_free (local_path);
		}
		g_list_free (base);
	}
	else //localpattern != NULL
	{
		GtkTreeModel *mdl;
		GtkTreeIter iter;
		if (GTK_IS_TREE_MODEL_FILTER (curr_view->model))
			mdl = gtk_tree_model_filter_get_model (GTK_TREE_MODEL_FILTER (curr_view->model));
		else
			mdl = curr_view->model;
		if (gtk_tree_model_get_iter_first (mdl, &iter))
		{
			do
			{
				gtk_tree_model_get (mdl, &iter, FINFO, &info, -1);
				if (strcmp (info->filename, "..") == 0) //the user may have chosen to have this item in the list
					continue;
				gint result = fnmatch (localpattern, info->filename, FNM_PATHNAME | FNM_PERIOD);
				if (result == FNM_NOMATCH)
					continue;
				else if (result != 0)
				{
//					CLOSEBGL
					//TODO display error
//					OPENBGL
					continue;
				}
				local_path = e2_utils_strcat (curr_local, info->filename);
#ifdef E2_VFS
				ddata.path = local_path;
				e2_fs_tw (&ddata, _e2p_du_twcb, cbdata, -1, E2TW_PHYS E2_ERR_NONE());
#else
				e2_fs_tw (local_path, _e2p_du_twcb, cbdata, -1, E2TW_PHYS E2_ERR_NONE());
#endif
				g_free (local_path);
			} while (gtk_tree_model_iter_next (mdl, &iter));
		}
	}

	e2_filelist_enable_one_refresh (PANEACTIVE);
	g_free (curr_local);
	total = cbdata->total;
	files = cbdata->files;
	dirs = cbdata->dirs;
	hashidden = cbdata->hidden;
	DEALLOCATE (E2_Du, cbdata);

	static gchar big[3] = { '1', ',', '\0' };
	gchar *comma = nl_langinfo (THOUSEP);
	if (comma != NULL && *comma != '\0')
		big[1] = *comma;
	text = g_string_new(_("total size: "));
	gint fwidth;

	if (total < BFAC)
	{
		gchar *b = _("bytes");
		if (total < 1000)
			g_string_append_printf (text, "%"PRIu64" %s", total, b);
		else
		{
			total -= 1000;
	    	g_string_append_printf (text, "%s%03"PRIu64" %s", big, total, b);
		}
	}
	else if (total < BFAC2)
	{
		gchar *kb = _("kilobytes");
		fwidth = (total < 10 * BFAC) ? 3 : 2;

		if ((total/BFAC) < 1000)
			g_string_append_printf (text, "%.*f %s", fwidth, (total / BFAC), kb);
		else
		{
			total -= (1000 * BFAC);
	    	g_string_append_printf (text, "%s%04.1f %s", big, (total / BFAC), kb);
		}
	}
	else if (total < BFAC3)
	{
		gchar *mb = _("Megabytes");
		fwidth = (total < 10 * BFAC2) ? 3 : 1;

		if ((total/BFAC2) < 1000)
			g_string_append_printf (text, "%.*f %s", fwidth, (total / BFAC2), mb);
		else
		{
			total -= (1000 * BFAC2);
			g_string_append_printf (text, "%s%04.1f %s", big, (total / BFAC2), mb);
		}
	}
	else
	{
		gchar *gb = _("gigabytes");
		fwidth = (total < 10 * BFAC3) ? 3 : 1;

		if ((total/BFAC3) < 1000)
			g_string_append_printf (text, "%.*f %s", fwidth, (total / BFAC3), gb);
		else
		{
			total -= (1000 * BFAC3);
			g_string_append_printf (text, "%s%04.1f %s", big, (total / BFAC3), gb);
		}
	}

	gchar *filetext = (files == 1) ? _("file") : _("files");
	gchar *dirtext = (dirs == 1) ? _("directory") : _("directories");
	g_string_append_printf (text, "\n%s %"PRIu64" %s %s %"PRIu64" %s",
		_("in"), files, filetext, _("and"), dirs, dirtext);
	if (localpattern != NULL)
	{
		g_string_append_printf (text, _(" named or in directories named '%s'\n"), localpattern);
	}
	else
	{
		if (hashidden && (files > 0 || dirs > 0))
			g_string_append_printf (text, " %s\n", _("(one or more are hidden)"));
		else
			text = g_string_append_c (text, '\n');
	}

	CLOSEBGL
	e2_output_print_same (text->str);
	OPENBGL
	E2_OutputTabRuntime *tab = (_e2t1_ == curr_tab) ? &app.tab : _e2t1_;
	CLOSEBGL
	e2_output_print_end (tab, FALSE);
	OPENBGL
	g_string_free (text, TRUE);
	if (localpattern != NULL)
		g_free (localpattern);
	return NULL;
}

/**
@brief du plugin action
This creates a thread to iterate over active pane selected items to recursively determine their disk usage
Action-data may be a (UTF-8) string representing the name or wildcarded-pattern
of items to be included in the scan. In that case, non-matching directories are
not counted per se, but any matching contents are counted.

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2p_du (gpointer from, E2_ActionRuntime *art)
{
	gchar *pattern; //file name-pattern to match

	if (art->data != NULL)
	{
		pattern = e2_utils_unquote_string ((const gchar *) art->data); //cleared in thread-func
		if (pattern != NULL)
		{
			if (G_LIKELY (strcmp (pattern, "*") != 0))
			{
				gchar *freeme = pattern;
#if 0
				//TODO make sense of macros e.g. multiple names, or a directory path
				//interpret any macros
				pattern = e2_utils_expand_macros (freeme, NULL);
				g_free (freeme);
				if (pattern == NULL)
				{
					e2_output_print_error (_("Failed to expand macros"), FALSE); //current tab
					return FALSE;
				}
				else if (pattern == GINT_TO_POINTER(1))
				{	//the user cancelled in a prompt macro
					return FALSE;
				}
				//replace ~ and $variable[s]
				freeme = pattern;
				pattern = e2_utils_replace_vars (freeme, TRUE);
				g_free (freeme);
				freeme = pattern;
#endif
				pattern = D_FILENAME_TO_LOCALE (pattern);
				g_free (freeme);
			}
			else
			{
				g_free (pattern);
				pattern = NULL;
			}
		}
	}
	else
		pattern = NULL;
#ifdef USE_GLIB2_32
	g_thread_new ("", (GThreadFunc)_e2p_du_all, pattern);
#else
	g_thread_create ((GThreadFunc)_e2p_du_all, pattern, FALSE, NULL);
#endif
	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(6),_("du"),_e2p_du,
		_("_Disk usage"),
		_("Calculate the disk usage of selected items"),
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
