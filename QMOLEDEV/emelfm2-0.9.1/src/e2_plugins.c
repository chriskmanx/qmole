/* $Id: e2_plugins.c 2982 2013-11-30 22:21:28Z tpgww $

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
@file src/e2_plugins.c
@brief plugins-related functions

This file contains infrastructure for running plugins, but no actual plugins.
*/
/**
\page plugin_writing creating plugins

Here's an outline of what a plugin file must contain.\n\n
<em>1.</em> A suitable header. To enable versioning by subversion, the first header line \b must have just $ Id $ (without the spaces between the $'s, they're presented here just to prevent subversion from putting version info into this text). You should also provide a copyright-assertion statement, if not for yourself, then for tooar. Finally, a licence statement. EmelFM2 is licensed under the GPL. Get advice if you need to use something else.\n\n
<em>2.</em> Includes, to access required external functions and data. The first of these is mandatory.
\code
#include "e2_plugins.h"
#include "e2_other-needed-stuff.h"
#include <other-needed-stuff.h>
\endcode \n
<em>3.</em> The function that performs what you want the plugin to do when activated. This type of function \b must take two parameters:
 - a pointer to the button, menu item etc which was activated to initiate the action
 - a pointer to an <tt>E2_ActionRuntime</tt> data struct which will provide any action argument etc

and \b must return a \c gboolean indicating whether the action succeeded. The function need not be static if it's to be used from outside the plugin.\n
For example, if you want a plugin that prints "Hello World", then you would write this function.
\code
static gboolean _e2p_hello_world (gpointer from, E2_ActionRuntime *art)
{
	e2_output_print (&app.tab, _("Hello World"), NULL, TRUE, NULL);
	return TRUE;	//or FALSE if the action was not completed successfully
}
\endcode \n
<em>4.</em> An initialisation function like the following. This function \b must be called \c init_plugin,
take a <tt>E2PInit</tt> flags argument, and return a <tt>Plugin*</tt>.
\code
//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "HelloWorld"  //no spaces, not translated

Plugin *init_plugin (E2PInit mode)
{
	p->signature = ANAME VERSION;  //for detecting whether the plugin is loaded

	//sometimes we load the plugin only to get the data above, but don't want to run it ...
	if (mode & E2P_UIDATA)
	{
		//setup UI data to send back to the caller
		some->label = _("_Hello World");  //the name for the plugins menu item, capitalize according to HIG is best
		some->description = _("prints \"Hello World\" on the output window");  //the tooltip for the plugins menu item
		some->icon = "plugin_" ANAME E2ICONTB;  //a non-standard path may be prepended. Just put "" for no icon
	}
	//sometimes we want to operate the plugin, and that needs some setup ...
	if (mode & E2P_SETUP)
	{
		const gchar *aname = _("demonstration");
		//No need to free this string, that's done as part of the registration
		/ *If the action is to apply to active-pane selected items, the first part
		  of the name must be _A(6) which is (in english) "file", in order to make
		  the plugins context-menu work properly. And the converse, do not use
		  _A(6) unless that condition applies * /
		E2_Action plugact =
		{g_strconcat (_A(14),".",aname,NULL),_e2p_hello_world,FALSE,0,NULL,NULL};
		some->action = e2_plugins_action_register (&plugact);
		if (some->action != NULL)
		{
			p->refcount = 1; //signal success to the caller
			some->aname = plugact.name;
			//other initialization stuff, as appropriate ...
		}
		else
		{
			g_free (plugact.name);
			some->label = NULL;
			some->description = NULL;
			some->icon = NULL;
		}
	}
	return (Plugin*)&iface;
}
\endcode \n
<em>5.</em> A cleanup function like the following. This function \b must be called \c unload_plugin, take a <tt>Plugin *</tt> argument, and return a \c gboolean which indicates whether the cleanup succeeded.
\code
static gboolean unload_plugin (Plugin *p)
{
	gchar *action_name = g_strconcat (_A(14),"."aname,NULL); //the same name as was registered
	gboolean ret = e2_plugins_action_unregister (action_name);
	g_free (action_name);
	if (ret)
	{
		//other stuff, as appropriate ...
	}
	return ret;
}
\endcode \n
Plugins may have more than one action, which requires some extra detail in the \c init_plugin and \c unload_plugin functions. Refer to the cpbar plugin code for an example of this.
*/
/**
\page plugins plugins

A specific interface is required to anable plugins to be loaded and unloaded - see \ref plugin_writing

In general, plugins use functions and data from core e2, and therefore, such plugins are specific to the version of e2 used for building ??

Plugins may use bits of each other if appropriate (but this has never been tested, current plugins have little need for this). A desired-but-not-loaded plugin will be loaded if its path/name are in config data (the code is not smart enough to find the missing plugin, otherwise). Plugins are not smart enough to auto-unload other(s), if the plugin has previously auto-loaded those other(s).

In case there is some need for a plugin that the user is not aware of, loaded plugins are ref-counted, and any user instruction to unload is obeyed only when the count allows.

Any plugin can include one or more actions. Or none - plugins don't have to be user-action-oriented. For example, new-version automatic upgrades of config data are performed using a plugin that is discarded immediately after use.

PluginActions' configuration data are in part loaded into the corresponding config treestore at session-start, and that store is incrementally updated after any plugin change via a config dialog. In addition, those data are stored in array (app.plugacts). That array enables storage of data (e.g. plugin signature and action name) not stored in the config treestore (all data there are user-editable via a config dialog, we don't want the signature or action altered, it'd be better to fix the config treeview ...). The array is also used for plugins-menu creation (but that could readily be converted to treestore data if all relevant data were there).
*/

#include "e2_plugins.h"
#include <string.h>
#include "e2_action.h"
#include "e2_output.h"
#include "e2_dialog.h"
#include "e2_filelist.h"

//show plugin file path and name in plugins GUI
//#define SHOW_PLUGPATHS

typedef struct _E2P_StoreRow
{
	gboolean load;
	gboolean menu;
	gchar *label;
	gchar *icon;
	gchar *description;
	gchar *signature;
	gchar *filename;
	gchar *filedir;
} E2P_StoreRow;

static gchar *_e2_plugins_derive_filename (const gchar *signature);

/**
@brief value-destructor for plugin @a p recorded in app.plugins

If @a p 's module is loaded, call its cleanup function then unload it.

@param p pointer to plugin data struct
@return
*/
static void _e2_plugins_clean1 (Plugin *p)
{
	if (p->module != NULL) //plugin is loaded
	{
		gboolean (*cleaner)(Plugin*) = NULL;
/*		if (p->cleaner)
			cleaner = p->cleaner;
		else
*/
			g_module_symbol (p->module, "clean_plugin", (gpointer) &cleaner);

		if (cleaner != NULL)
			cleaner (p);
		else
			printd (ERROR, "No cleanup-function for plugin %s", p->signature);

		g_module_close (p->module);
	}
}
/**
@brief plugins hashtable foreach remove helper for unloading all items in app.plugins where possible
@param key UNUSED plugin signature
@param p pointer to plugin data struct in a loaded module
@param force pointerised gboolean whether to force removal
@return TRUE if the key/value pair should be removed from the table
*/
static gboolean _e2_plugins_unload1_ofall (const gchar *key, Plugin *p, gpointer force)
{
	if (!(p->flags & E2P_LOCKIN) || force != NULL)
	{
		if (--p->refcount == 0)
		{
			//release associated runtime data (in-module resources are cleared by clean_plugin())
			guint8 i;
			for (i = 0; i < p->actscount; i++)
				g_ptr_array_remove (app.plugacts, p->actsarray + i);

			return TRUE;
		}
	}
	return FALSE;
}

/**
@brief load the plugin with the specified @a signature

@param sigvers unique ID string for the desired plugin, like ANAME VERSION,
 where ANAME matches the 'core' of the plugin filename

@return pointer to data struct for plugin which has the specified ID, else NULL
*/
static Plugin *_e2_plugins_load_named (const gchar *sigvers)
{
	Plugin *p;
	E2P_InitData data;
	GtkTreeIter iter;

	gchar *pname = _e2_plugins_derive_filename (sigvers);
	if (pname == NULL)
		return NULL;

	gboolean inconfig = FALSE;
	gchar *ppath = NULL;
	E2_OptionSet *set = e2_option_get_simple ("plugins");
	GtkTreeModel *mdl = set->ex.tree.model;

	//check whether a custom path has been recorded for this plugin
	gboolean notempty = gtk_tree_model_get_iter_first (mdl, &iter);
	if (notempty)
	{
		if (e2_tree_find_iter_from_str_same (mdl, FILE_COL, pname, &iter))
		{
			inconfig = TRUE;
			gtk_tree_model_get (mdl, &iter, PATH_COL, &ppath, -1);
			if (ppath != NULL && *ppath != '\0')
			{	//plugin is recorded in config data
				gchar *s = ppath;
#ifdef SHOW_PLUGPATHS
				gchar *local = F_FILENAME_TO_LOCALE (s);
				ppath = g_build_filename (local, pname, NULL); //create path from config data
				F_FREE (local, s);
#else
				ppath = g_build_filename (s, pname, NULL); //create path from config data
#endif
				g_free (s);
			}
			else if (ppath != NULL)
			{
				g_free (ppath);
				ppath = NULL;
			}
		}
	}

	if (ppath == NULL) //nope - fall back to the default dir
		ppath = e2_utils_strcat (PLUGINS_DIR G_DIR_SEPARATOR_S, pname);	//localised path

	if (e2_plugins_open_module (ppath, &data))
	{
		p = (*data.init) (E2P_INITALL);
		if (p->refcount == 1) //success
		{
			p->module = data.module; //flag it's loaded, ready for unload
			g_hash_table_replace (app.plugins, g_strdup (sigvers), p);
			if (inconfig)
				gtk_tree_store_set (GTK_TREE_STORE (set->ex.tree.model), &iter,
					LOAD_COL, TRUE, SIG_COL, p->signature, -1);
			else
			{
				//add to config data
				if (notempty)
				{
					gint count = gtk_tree_model_iter_n_children
						(set->ex.tree.model, NULL);
					gtk_tree_model_iter_nth_child (set->ex.tree.model,
							&iter, NULL, count - 1);
				}
				e2_plugins_store_config_data (set->ex.tree.model, &iter,
					p, FALSE, ppath); //TODO inmenu - should be action-specific
			}
		}
		else
			printd (ERROR, "Can't initialize plugin %s", pname);
	}
	else
	{
		p = NULL;
		printd (ERROR, "Can't find plugin %s", pname);
	}

	g_free (pname);
	g_free (ppath);
	return p;
}


  /*************************/
 /** load/update helpers **/
/*************************/

/**
@brief get hash-table key string for a plugin, derived from @a signature
@param signature plugin or plugin-action signature, like ANAME or like N@ANAME
 for an action in a multi-action plugin
@return allocated string like ANAME VERSION
*/
static gchar *_e2_plugins_get_key (const gchar *signature)
{
	const gchar *s = strchr (signature, '@');
	if (s == NULL)
		s = signature;
	return e2_utils_strcat (s, VERSION);
}
/**
@brief try to derive a plugin hash-table key string from @a filename, or else from @a signature

@a filename is the preferred basis for the returned key, othewise @a signature

@param filename a plugin basename string supplied in config data, normally with
 prefix "e2p_", possibly empty or NULL, or otherwise with suffix ".so"
@param signature a plugin signature string supplied in config data,
 with or without 'version-suffix', or possibly empty or NULL
@return allocated signature string like ANAME VERSION, or NULL
*/
static gchar *_e2_plugins_derive_key (gchar *filename, const gchar *signature)
{
	gchar *s, *sigvers;

	if (filename != NULL && *filename != '\0')
	{
		s = strstr (filename, ".so");
		if (s == NULL)
			return NULL;

		*s = '\0';
		gchar *s1 = (strncmp (filename, "e2p_", 4) == 0) ? filename + 4 : filename;
		sigvers = g_strconcat (s1, VERSION, NULL);
		*s = '.';
		return sigvers;
	}
	else if (signature != NULL && *signature != '\0')
	{
		//assume any '.' in the signature is part of a version-identifier at the end
		s = strchr (signature, '.');
		if (s == NULL)
			return _e2_plugins_get_key (signature);
		if (s > signature)
		{
			gchar c;
			while ((c = *--s) >= '0' && c <= '9') {}
			c = *++s;
			*s = '\0';
			sigvers = _e2_plugins_get_key (signature);
			*s = c;
			return sigvers;
		}
	}
	return NULL;
}
/**
@brief try to derive a plugin filename string from @a signature

@param signature a plugin signature string supplied in config data,
 with or without 'version-suffix', possibly empty or NULL

@return allocated string or NULL
*/
static gchar *_e2_plugins_derive_filename (const gchar *signature)
{
	if (signature != NULL && *signature != '\0')
	{
		const gchar *s = strchr (signature, '.');
		if (s == NULL)
		{
			s = strchr (signature, '@');
			if (s != NULL)
				s++;
			else
				s = signature;
			return g_strconcat ("e2p_", s, ".so", NULL);
		}
		else if (s > signature)
		{
			gchar *s2 = strchr (signature, '@');
			if (s2 != NULL)
				s2++;
			else
				s2 = (gchar*) signature;

			gchar c;
			while ((c = *--s) >= '0' && c <= '9'){}
			s2 = g_strndup (s2, s-s2+1);

			gchar *s3 = g_strconcat ("e2p_", s2, ".so", NULL);
			g_free (s2);
			return s3;
		}
	}
	return NULL;
}
/**
@brief construct filesystem path for loading a plugin
If @a dirpath is not absolute, NOTHING is prepended, the subsequent load will fail.
The directory of the returned path is confirmed to exist
@param dirpath string, maybe UTF-8, absolute or "" or NULL
@param filename name of plugin file, string maybe UTF-8 (or, if the config file
  is corrupt, "" or NULL)
@return allocated absolute path string, or NULL
*/
static gchar *_e2_plugins_get_path (gchar *dirpath, const gchar *filename)
{
	gchar *localdir, *localpath;

	if (filename == NULL || *filename == '\0')
		return NULL;

	if (dirpath == NULL || *dirpath == '\0')
		//no path specified, so use the default plugins path
		localdir = PLUGINS_DIR;	//ASCII == localised
	else
	{
#ifdef SHOW_PLUGPATHS
		localdir = F_FILENAME_TO_LOCALE (dirpath);
#else
		localdir = dirpath;
#endif
	}

#ifdef E2_VFS
	VPATH data = { localdir, NULL };	//only local dirs for plugins
	if (e2_fs_is_dir3 (&data E2_ERR_NONE()))	//only local places for plugins
#else
	if (e2_fs_is_dir3 (localdir E2_ERR_NONE()))	//only local places for plugins
#endif
		localpath = g_build_filename (localdir, filename, NULL);
	else
		localpath = NULL;
#ifdef SHOW_PLUGPATHS
	F_FREE (localdir, dirpath);
#endif
	return localpath;
}
/**
@brief get non-action UI data for a plugin if its signature matches @a signature

If @a signature is NULL or "", no signature check is done.

@param p a plugin to be interrogated
@param signature string to be matched, maybe NULL or ""
@param pa data struct in which to record the results
@return TRUE if the contents of @a pa have valid data
*/
static gboolean _e2_plugins_get_whole_description (Plugin *p,
	const gchar *signature, PluginAction *pa)
{
	if (p->flags & E2P_CANCFG)
	{
		if (signature == NULL || *signature == '\0'
		 || strcmp (p->signature, signature) == 0)
		{
			pa->signature = p->signature;
			if (p->title != NULL)
				pa->label = p->title; //maybe empty
			else
				pa->label = (p->flags & E2P_OFFMENU) ? "" : "<Missing name>" ; //CHECKME translate this?
			pa->description = p->tip; //maybe NULL or empty
			pa->flags = E2PA_NONACT;
			return TRUE;
		}
	}
	return FALSE;
}
/**
@brief get action-data in @a p ->actlist which matches @a signature

If @a signature is NULL or "", the first-recorded (if any) action will be returned.

@param p a plugin to be interrogated
@param signature string to be matched, maybe NULL or ""
@return matching data, or NULL
*/
PluginAction *_e2_plugins_get_matching_action (Plugin *p, const gchar *signature)
{
	if (signature == NULL || *signature == '\0')
		return p->actsarray; //the first action (if any) will do

	//we have a usable signature, try to match it to an action
	guint8 i;
	for (i = 0; i < p->actscount; i++)
	{
		PluginAction *pa = p->actsarray + i;
		if (strcmp (pa->signature, signature) == 0)
			return pa;
	}
	return NULL;
}
/**
@brief find PluginAction in @a acts whose signature matches @a signature
@param acts array of PluginAction*'s
@param signature PluginAction signature string
@return array index of matching item, or -1 if no match
*/
static gint _e2_plugins_get_matching_index (GPtrArray *acts, const gchar *signature)
{
	if (signature != NULL) //can't compare NULL, no match
	{
		gint i;
		gpointer *dp;

		for (i = 0, dp = acts->pdata; i < acts->len; i++, dp++)
		{
			PluginAction *pa = *((PluginAction**)dp);
			if (strcmp (pa->signature, signature) == 0)
				return i;
		}
	}
	return -1;
}
/**
@brief set config-store and/or runtime data to "best available" values
@param model the plugins config treestore model
@param iter the iter in @a model being processed
@param row data struct with values retrieved from each relevant field now in the row
 (all strings are allocated)
@param pa data struct created/populated by the 'owner' plugin as part of action initialisation
@return
*/
static void _e2_plugins_refresh_alldata (GtkTreeModel *model,
	GtkTreeIter *iter, E2P_StoreRow *row, PluginAction *pa)
{
	GtkTreeStore *store = GTK_TREE_STORE (model);

	if (row->load)
		SET_LOADED (pa);
	else
		SET_UNLOADED (pa);
	if (row->menu)
		SET_SHOWN (pa);
	else
		SET_UNSHOWN (pa);
	gtk_tree_store_set (store, iter, LOAD_COL, row->load, MENU_COL, row->menu, -1);

	if (row->label == NULL || *(row->label) == '\0')
	{
		if (pa->label != NULL && *pa->label != '\0')
			gtk_tree_store_set (store, iter, LABEL_COL, pa->label, -1);
	}
	else
	{
		if (pa->flags & E2PA_CLEANLABEL)
			g_free (pa->label);
		pa->label = row->label;
		pa->flags |= E2PA_CLEANLABEL;
		row->label = NULL; //don't free this one
	}

	if (row->description == NULL || *(row->description) == '\0')
	{
		if (pa->description != NULL && *pa->description != '\0')
			gtk_tree_store_set (store, iter, TIP_COL, pa->description, -1);
	}
	else
	{
		if (pa->flags & E2PA_CLEANTIP)
			g_free (pa->description);
		pa->description = row->description;
		pa->flags |= E2PA_CLEANTIP;
		row->description = NULL; //don't free
	}

	if (row->icon == NULL || *(row->icon) == '\0')
	{
		if (pa->icon != NULL && *pa->icon != '\0')
			gtk_tree_store_set (store, iter, ICON_COL, pa->icon, -1);
	}
	else
	{
		if (pa->flags & E2PA_CLEANICON)
			g_free (pa->icon);
		pa->icon = row->icon;
		pa->flags |= E2PA_CLEANICON;
		row->icon = NULL;
	}

	//always update signature (just in case)
	gtk_tree_store_set (store, iter, SIG_COL, pa->signature, -1);
	//updated path or filename not needed
}
/**
@brief load the Plugin/PluginAction described in plugins config-data @a model
 at the row represented by @a iter

Called from e2_plugins_load_configured() etc i.e. several contexts.
If the plugin file for the specified action is not already loaded, and is found
(at custom or default path), it is loaded, initialised, relevant action data are
recorded in app.plugacts, then if the plugin is not wanted, it's discarded.
This tolerates 'quirks' in config data where possible.

@param model treemodel for plugins config data
@param iter pointer to iter for top-level model-row to be processed
@param new TRUE when loading from start, FALSE when updating
@param removes array in which to store treepath of @a iter if the latter is to
 be removed from its treestore, after the treewalk is concluded.

@return
*/
static void _e2_plugins_process_iter (GtkTreeModel *model, GtkTreeIter *iter,
	gboolean new, GPtrArray *removes)
{
	PluginAction *pa;
	GtkTreePath *tp;
	E2P_InitData data;
	E2P_StoreRow row;

	memset (&data, 0, sizeof (E2P_InitData));
	gtk_tree_model_get (model, iter,
		LOAD_COL, &row.load,
		MENU_COL, &row.menu,
		LABEL_COL, &row.label,
		ICON_COL, &row.icon,
		TIP_COL, &row.description,
		FILE_COL, &row.filename,
		PATH_COL, &row.filedir,
		SIG_COL, &row.signature,
		-1);

	//formulate plugin key, from supplied filename or signature
	gchar *sigvers = _e2_plugins_derive_key (row.filename, row.signature);
	if (sigvers == NULL)
	{
		printd (DEBUG, "Plugin not identifiable: file %s signature %s",
			row.filename, row.signature);
		goto dump; //unrecognisable, remove it
	}

	Plugin *p = g_hash_table_lookup (app.plugins, sigvers);
	if (p == NULL) //plugin not already loaded
	{
		//formulate plugin filepath, from supplied path and name
		gchar *plugpath = _e2_plugins_get_path (row.filedir, row.filename);
		if (plugpath == NULL)
		{
			printd (DEBUG, "No recognised filepath for plugin %s: path: %s file: %s",
				sigvers, row.filedir, row.filename);
			goto dump;
		}
		if (e2_plugins_open_module (plugpath, &data)) //load it if possible
		{
			g_free (plugpath);
			if (row.load) //module wanted
			{
				p = (*data.init) (E2P_INITALL);
				if (p->refcount == 0) //setup failed
				{
					printd (WARN, "Initialisation of plugin $s failed", sigvers);
					goto unload;
				}
			}
			else
				p = (*data.init) (E2P_UIDATA);
			p->module = data.module; //enable unloading, now or later
			if (p->flags & E2P_NOCFG) //not allowed in config
				goto unload;
			if (row.load)
			{
				if (g_hash_table_lookup (app.plugins, sigvers) == NULL)
					g_hash_table_insert (app.plugins, g_strdup (sigvers), p);
			}
			goto checkit;
		}
		else //failed to open module
		{
			g_free (plugpath);
			goto dump;
		}
	}
	else //plugin loaded before or still
		if (!new)
	{
		if (!row.load || (p->flags & E2P_NOCFG)) //not wanted now, or not allowed in config
			goto unload;
checkit:
		if (p->actscount == 0)
		{
			PluginAction fake;
			memset (&fake, 0, sizeof (PluginAction));
			if (_e2_plugins_get_whole_description (p, row.signature, &fake))
			{
				//store this one in config, but not in runtime
				E2P_StoreRow xrow = { row.load, FALSE, NULL, NULL, NULL, NULL, NULL, NULL };
				_e2_plugins_refresh_alldata (model, iter, &xrow, &fake);
				goto cleanup;
			}
			else
				goto unload;
		}
		else if (p->actscount == 1)
		{
			pa = p->actsarray;
			if (!(row.signature == NULL || *row.signature == '\0'))
			{
				if (strcmp (row.signature, pa->signature) != 0)
				{
					gchar *tmp = e2_utils_strcat (pa->signature, VERSION);
					gint res = strcmp (row.signature, tmp);
					g_free (tmp);
					if (res != 0) //signatures don't match
						goto unload;
				}
			}
		}
		else // > 1
		{
			pa = _e2_plugins_get_matching_action (p, row.signature); //find data for this signature, if any
			if (pa == NULL)
				goto unload;
		}
	}
	else //loaded before & new
	{
		if (p->actscount < 2) //no more actions to get, assume a duplication error
		{
			printd (DEBUG, "Ignoring repeated plugin %s", sigvers);
			goto dump;
		}
		//find unused action data for this signature, if any
		pa = _e2_plugins_get_matching_action (p, row.signature);
		if (pa == NULL)
			goto cleanup;
		if (_e2_plugins_get_matching_index (app.plugacts, row.signature) >= 0)
			goto cleanup;
	}

	//update stored config data and/or runtime data to 'best available'
	_e2_plugins_refresh_alldata (model, iter, &row, pa);
	if (row.load) //module wanted
	{
		if (row.menu)
			g_ptr_array_add (app.plugacts, pa);
	}
	else
	{
		if (data.module != NULL)
			p->module = data.module;
		_e2_plugins_clean1 (p);
	}
	goto cleanup;

unload:
	if (!g_hash_table_remove (app.plugins, sigvers))
		_e2_plugins_clean1 (p);
dump:
	tp = gtk_tree_model_get_path (model, iter);
	g_ptr_array_add (removes, tp);
cleanup:
	//some of these strings may be owned now, in which case NULL
	g_free (sigvers);
	g_free (row.label);
	g_free (row.icon);
	g_free (row.description);
	g_free (row.signature);
	g_free (row.filename);
	g_free (row.filedir);
}
/**
@brief remove unwanted rows from plugins config store
@param model plugins config data treestore model
@param iter treemodel iter for use here, not set to anything
@param removes array of GtkTreePaths for store iters to be removed from @a model
@return
*/
static void _e2_plugins_remove_rows (GtkTreeModel *model, GtkTreeIter *iter,
	GPtrArray *removes)
{
	guint i;
	gpointer *dp;
	GtkTreeStore *store = GTK_TREE_STORE (model);
	//process iters in reverse order, cuz we work with treepaths, not treerowreferences
	for (i = removes->len, dp = removes->pdata + (i - 1); i > 0 ; i--, dp--)
	{
		GtkTreePath *tpath = *((GtkTreePath**)dp);
		gtk_tree_model_get_iter (model, iter, tpath);
		gtk_tree_store_remove (store, iter);
		gtk_tree_path_free (tpath);
	}
}

#ifndef USE_GLIB2_16

typedef struct _PluginAdder
{
	GtkTreeModel *model;
	gboolean force;
} PluginAdder;

/**
@brief plugins hash table foreach func to append any still-unrecorded action to plugins config store
@param key plugins hashtable key, string like ANAME VERSION
@param value a Plugin*, plugins hashtable value
@param data PluginAdder* with data for this operation
@return
*/
static void _e2_plugins_append1 (gchar *key, Plugin *value, PluginAdder *data)
{
	guint8 i;
	//check all actions, cuz' any of them could be gone
	for (i = 0; i < value->actscount; i++)
	{
		GtkTreeIter iter;
		PluginAction *pa = value->actsarray + i;
		gboolean reload = data->force || !IS_LOADED (pa);
		if (!reload)
		{
			//always search from start of treestore
			if (gtk_tree_model_get_iter_first (data->model, &iter))
				reload = !e2_tree_find_iter_from_str_simple (data->model,
					SIG_COL, pa->signature, &iter, FALSE);
		}

		if (reload)
		{
			SET_LOADED (pa);
			SET_UNSHOWN (pa);
			gchar *filename = _e2_plugins_derive_filename (key);
#ifdef USE_GTK2_10
			gtk_tree_store_insert_with_values (GTK_TREE_STORE (data->model), NULL, NULL, -1,
#else
			gtk_tree_store_insert (GTK_TREE_STORE (data->model), &iter, -1);
			gtk_tree_store_set (GTK_TREE_STORE (data->model), &iter,
#endif
				LOAD_COL, TRUE,
				MENU_COL, FALSE,
				LABEL_COL, pa->label ? pa->label : "",
				ICON_COL, pa->icon ? pa->icon : "" ,
				TIP_COL, pa->description ? pa->description : "",
				FILE_COL, filename,
#ifdef SHOW_PLUGPATHS
				PATH_COL, "", //TODO
#else
				PATH_COL, "",
#endif
				SIG_COL, pa->signature,
				-1);
			g_free (filename);
		}
	}
}
#endif

/**
@brief append any still-unrecorded loaded-plugin-action to plugins config store
@param model plugins config data treestore model
@param force TRUE to force reloading everything in the plugins hashtable
@return
*/
static void _e2_plugins_append_loaded_actions (GtkTreeModel *model,
	gboolean force)
{
#ifdef USE_GLIB2_16
	gpointer key;
	gpointer value;
	GHashTableIter hti;

	g_hash_table_iter_init (&hti, app.plugins);
	while (g_hash_table_iter_next (&hti, &key, &value))
	{
		guint8 i;
		Plugin *p = (Plugin *)value;
		//check all actions, cuz' any of them could be gone
		for (i = 0; i < p->actscount; i++)
		{
			GtkTreeIter iter;
			PluginAction *pa = p->actsarray + i;
			gboolean reload = force || !IS_LOADED (pa);
			if (!reload)
			{
				//always search from start of treestore
				if (gtk_tree_model_get_iter_first (model, &iter))
					reload = !e2_tree_find_iter_from_str_simple (model,
						SIG_COL, pa->signature, &iter, FALSE);
			}

			if (reload)
			{
				SET_LOADED (pa);
				SET_UNSHOWN (pa);
				gchar *filename = _e2_plugins_derive_filename ((gchar*)key);
#ifdef USE_GTK2_10
				gtk_tree_store_insert_with_values (GTK_TREE_STORE (model), NULL, NULL, -1,
#else
				gtk_tree_store_insert (GTK_TREE_STORE (model), &iter, -1);
				gtk_tree_store_set (GTK_TREE_STORE (model), &iter,
#endif
					LOAD_COL, TRUE,
					MENU_COL, FALSE,
					LABEL_COL, pa->label ? pa->label : "",
					ICON_COL, pa->icon ? pa->icon : "" ,
					TIP_COL, pa->description ? pa->description : "",
					FILE_COL, filename,
#ifdef SHOW_PLUGPATHS
					PATH_COL,"", //TODO
#else
					PATH_COL, "",
#endif
					SIG_COL, pa->signature,
					-1);
				g_free (filename);
			}
		}
	}

#else //ndef USE_GLIB2_16

	PluginAdder data = { model, force };
	g_hash_table_foreach (app.plugins, (GHFunc)_e2_plugins_append1, &data);

#endif //ndef USE_GLIB2_16
}
/**
@brief unresolved plugins hashtable foreach-remove helper

Unload @a p if it's not locked in and has no actions marked as loaded in the
plugins config treestore.

@param key plugin signature
@param p pointer to plugin data struct in a loaded module
@param model plugins config store treemodel
@return TRUE if the key/value pair should be removed from the table
*/
static gboolean _e2_plugins_unload1_unresolved (const gchar *key, Plugin *p,
	GtkTreeModel *model)
{
	if (!(p->flags & E2P_LOCKIN))
	{
		//if any plugin action is in the treestore, marked as loaded, don't remove the plugin
		if (p->actsarray != NULL)
		{
			GtkTreeIter iter;
			if (gtk_tree_model_get_iter_first (model, &iter))
			{
				guint8 i;
				for (i = 0; i < p->actscount; i++)
				{
					PluginAction *pa = p->actsarray + i;
					if (e2_tree_find_iter_from_str_simple (model,
						SIG_COL, pa->signature, &iter, FALSE))
					{
						gboolean loaded = FALSE;
						gtk_tree_model_get (model, &iter, LOAD_COL, &loaded, -1);
						if (loaded)
							return FALSE;
					}
					//always search from start of treestore
					gtk_tree_model_get_iter_first (model, &iter);
				}
			}
		}
		if (--p->refcount == 0)
			return TRUE;
	}
	return FALSE;
}

  /****************/
 /**** public ****/
/****************/

/**
@brief clean up the allocated resources inside @a pa

Effectively a plugin actsarray foreach callback, called during plugin destruction

@param pa a PluginAction* in the plugin's actsarray
@return
*/
void e2_plugins_actiondata_clear (PluginAction *pa)
{
	//pa->signature is const
	if (pa->flags & E2PA_CLEANLABEL)
		g_free (pa->label);
	if (pa->flags & E2PA_CLEANTIP)
		g_free (pa->description);
	if (pa->flags & E2PA_CLEANICON)
		g_free (pa->icon);
	if (pa->aname)
	{
		if (!e2_plugins_action_unregister (pa->aname))
			g_free (pa->aname);
	}
	if (pa->action_data != NULL)
		g_free (pa->action_data); //TODO more flexible cleaner
}

/**
@brief obliterate specified plugin

All the plugin's actions are unregistered, any 'unload' function in the plugin
is called, then the module is dumped and memory freed, and the plugin removed
from app.plugins.
Any related config treestore data are not affected - any update of those
must be done before coming here
Expects BGL on/closed (for any error message display)

@param p plugin data struct
@param force TRUE to force removal even if not cleaned up by the plugin

@return TRUE if the plugin was unloaded
*/
gboolean e2_plugins_unload1 (Plugin *p, gboolean force)
{
	printd (DEBUG, "unload plugin: %s", p->signature);
	if (p->module != NULL) //this is a loaded plugin
	{
		if (!(p->flags & E2P_LOCKIN) || force)
		{
			if (--p->refcount == 0)
			{
				if (g_hash_table_remove (app.plugins, p->signature))
					return TRUE;
				printd (DEBUG, "unload plugin %s failed", p->signature);
				gchar *msg = g_strdup_printf ("%s \"%s\"", _("Cannot unload plugin"),
					p->signature);
				e2_output_print_error (msg, TRUE);
				p->refcount = 1;
			}
		}
	}
	return FALSE;
}
/**
@brief unload all plugins
To the extent possible, each plugin recorded in app.plugins is removed.
Related config treestore data for plugins are not affected.
@param force TRUE to force removal even if not cleaned up by the plugin

@return
*/
void e2_plugins_unload_all (gboolean force)
{
	if (app.plugins != NULL)
	{
		g_hash_table_foreach_remove (app.plugins,
			(GHRFunc)_e2_plugins_unload1_ofall, GINT_TO_POINTER (force));
	}
}
/**
@brief end-of-session cleanup all plugins data

@return
*/
void e2_plugins_clean (void)
{
	if (app.plugins != NULL)
	{
		g_ptr_array_free (app.plugacts, TRUE);
		g_hash_table_destroy (app.plugins);
	}
}
/**
@brief check for and open a module for a specified plugin, if possible

@param filepath path+name string for the plugin, probably localised (API doc is silent)

@return TRUE, with @a data populated, or FALSE if plugin file @a filepath isn't found
 or has no initialize function
*/
gboolean e2_plugins_open_module (gchar *filepath, E2P_InitData *data)
{
	printd (DEBUG, "e2_plugins_open_module: %s", filepath);

	data->module = g_module_open (filepath, 0); //CHECKME lazy instead ?
	if (data->module == NULL)
	{
		printd (DEBUG, "failed to open plugin file: %s", g_module_error());
		return FALSE;
	}

	if (!g_module_symbol (data->module, "init_plugin", (gpointer)&(data->init)))
	{
		printd (DEBUG, "no initialise-function in plugin file: %s", g_module_error());
		g_module_close (data->module);
		return FALSE;
	}

	return TRUE;
}

/**
@brief process a plugin's UI data into config store

This does not affect app.plugins. @a sibling is updated if >1 row is added.

@param model model for plugins config treestore
@param sibling pointer to @a model iter after which to insert extra iter(s)
 as needed for each new item to go into the store. May be NULL.
@param p pointer to data struct for the plugin
@param inmenu setting for the plugin's loaded/in-menu data
@param localpath localized absolute path-string for the plugin file
@return
*/
void e2_plugins_store_config_data (GtkTreeModel *model, GtkTreeIter *sibling,
	Plugin *p, gboolean inmenu, gchar *localpath)
{
	GtkTreeIter iter, work;
	GtkTreeIter *worker;
	GtkTreeStore *store;
	const gchar *label, *icon, *tip;
	gchar *showdir, *sep = NULL;
	gint len = strlen (PLUGINS_DIR); //no trailing /
	if (strncmp (localpath, PLUGINS_DIR, len) == 0)
	{
		sep = strchr (localpath + len + 1, G_DIR_SEPARATOR);
		if (sep == NULL)
		{
			sep = localpath + len;
			showdir = "";
		}
		else
			showdir = localpath;
		*sep = '\0';
	}
	else
	{
		sep = strrchr (localpath, G_DIR_SEPARATOR);
		if (sep > localpath)
			*sep = '\0';
		showdir = localpath;
	}
#ifdef SHOW_PLUGPATHS
	utfdir = F_FILENAME_FROM_LOCALE (showdir);
#endif

	if (sibling != NULL)
	{
		work = *sibling;
		worker = &work;
	}
	else
		worker = NULL;

	store = GTK_TREE_STORE (model);
	if (p->actsarray != NULL) //action(s) exist for this plugin
	{
		guint8 i;
		for (i = 0; i < p->actscount; i++)
		{
			PluginAction *pa = p->actsarray + i;
			label = (pa->label != NULL) ? pa->label : "";
			icon = (pa->icon != NULL) ? pa->icon : "";
			tip = (pa->description != NULL) ? pa->description : "";

			if (worker != NULL)
				gtk_tree_store_insert_after (store, &iter, NULL, worker);
			else
				gtk_tree_store_insert_before (store, &iter, NULL, worker);
			gtk_tree_store_set (store, &iter,
				LOAD_COL, TRUE,
				MENU_COL, FALSE,
				LABEL_COL, label,
				ICON_COL, icon,
				TIP_COL, tip,
				FILE_COL, sep + 1,
#ifdef SHOW_PLUGPATHS
				PATH_COL, utfdir,
#else
				PATH_COL, showdir,
#endif
				SIG_COL, pa->signature,
				-1);
			work = iter;
			if (worker == NULL)
				worker = &work;
		}
	}
	else //no actions
		if (p->flags & E2P_CANCFG)
	{
		label = (p->title != NULL) ? p->title: "";
		tip = (p->tip != NULL) ? p->tip : "";

		if (worker != NULL)
			gtk_tree_store_insert_after (store, &iter, NULL, worker);
		else
			gtk_tree_store_insert_before (store, &iter, NULL, worker);
		gtk_tree_store_set (store, &iter,
			LOAD_COL, TRUE,
			MENU_COL, FALSE,
			LABEL_COL, label,
			ICON_COL, "",
			TIP_COL, tip,
			FILE_COL, sep + 1,
#ifdef SHOW_PLUGPATHS
			PATH_COL, utfdir,
#else
			PATH_COL, showdir,
#endif
			SIG_COL, p->signature,
			-1);
		work = iter;
		if (worker == NULL)
			worker = &work;
	}
	else //no actions, and this-one's a no-show
	{
		label = ""; //DEBUG
	}
#ifdef SHOW_PLUGPATHS
	F_FREE (utfdir, showdir);
#endif
	*sep = G_DIR_SEPARATOR;
	if (sibling != NULL)
		*sibling = work;
}
/**
@brief determine the number of actions for a plugin represented in config data @a iter

@a iter may represent a plugin action or the whole plugin. The plugin may be
loaded or not. If loaded and there are actions, all will be loaded, some or all
may be unshown.

@param model model for plugins config treestore
@param iter pointer to @a model iter
@return the number of actions
*/
gint e2_plugins_count_actions (GtkTreeModel *model, GtkTreeIter *iter)
{
	gint count;
	gchar *filename, *signature;
	gtk_tree_model_get (model, iter, FILE_COL, &filename, SIG_COL, &signature, -1);
	//formulate plugin key, from supplied filename or signature
	gchar *sigvers = _e2_plugins_derive_key (filename, signature);
	if (sigvers == NULL)
	{
		printd (DEBUG, "Plugin not identifiable: file %s signature %s",
			filename, signature);
		count = 0;
	}
	else
	{
		gchar *sep;
		Plugin *p = g_hash_table_lookup (app.plugins, sigvers);
		g_free (sigvers);
		if (p != NULL) //plugin loaded
			count = (gint)p->actscount;
		else if ((sep = strchr (signature, '@')) == NULL)
			count = 1;
		else
		{
			//revert to matching in the treestore
			GtkTreeIter scan;
			count = 0;
			sep++;
			gtk_tree_model_get_iter_first (model, &scan);
			do
			{
				gchar *onesig;
				gtk_tree_model_get (model, &scan, SIG_COL, &onesig, -1);
				if (onesig != NULL)
				{
					gchar *start = strchr (onesig, '@');
					if (start != NULL && strcmp (start+1, sep) == 0)
						count++;
					g_free (onesig);
				}
			} while (gtk_tree_model_iter_next (model, &scan));
		}
	}
	g_free (filename);
	g_free (signature);
	return count;
}
/**
@brief load all plugins that are recorded in plugins config data

Plugins that are marked for loading are registered and kept.
For the rest, just get missing data (if any) to show in config dialogs, then
discard.
This is called at session-start, among other times. app.plugins and app.plugacts
are created if need be, or if app.plugins already exists, it and app.plugacts
should be in a fit state to accept new additions i.e. no duplication.

@return
*/
void e2_plugins_load_configured (void)
{
	GtkTreeIter iter;

	E2_OptionSet *set = e2_option_get_simple ("plugins");

	if (set != NULL //should always be TRUE
		&& gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
	{
		GPtrArray *removes = g_ptr_array_new ();
		if (app.plugins == NULL)
		{
			app.plugins = g_hash_table_new_full (g_str_hash, g_str_equal,
				g_free, (GDestroyNotify)_e2_plugins_clean1);
			app.plugacts = g_ptr_array_sized_new (8);
		}

		do
		{
			_e2_plugins_process_iter (set->ex.tree.model, &iter, TRUE, removes);
		} while (gtk_tree_model_iter_next (set->ex.tree.model, &iter));
		//eliminate rows that were un-processable
		if (removes->len > 0)
			_e2_plugins_remove_rows (set->ex.tree.model, &iter, removes);
		g_ptr_array_free (removes, TRUE);
		//add back rows for 'partially-loaded' plugins
		_e2_plugins_append_loaded_actions (set->ex.tree.model, FALSE);
	}
	else	//nothing in the store
		printd (WARN, "plugins config is empty");
}
/**
@brief update loaded plugins in accordance with current config data

This is the 'apply' fn for the plugins dialog, also used in general config dialog.
It performs a differential load/unload/data-update relative to the status quo.

@return
*/
void e2_plugins_update_configured (void)
{
	GtkTreeIter iter;

	E2_OptionSet *set = e2_option_get_simple ("plugins");
	GtkTreeModel *model = (set != NULL) ? //should always be TRUE
		set->ex.tree.model : NULL;

	if (model != NULL && gtk_tree_model_get_iter_first (model, &iter))
	{
		if (app.plugins == NULL)
		{
			app.plugins = g_hash_table_new_full (g_str_hash, g_str_equal,
				g_free, (GDestroyNotify)_e2_plugins_clean1);
			app.plugacts = g_ptr_array_sized_new (8);
		}
		//Config data may have more or fewer rows, and/or in different order,
		//and/or with revised content in any row(s)
		//So we first populate a replacement PluginActions array, in
		//new-config-data order, the same as if loading at session-start
		GPtrArray *removes = g_ptr_array_new ();
		GPtrArray *new = g_ptr_array_sized_new (app.plugacts->len);
		//minimal race-risk here
		GPtrArray *old = app.plugacts;
		app.plugacts = new;

		do
		{
			_e2_plugins_process_iter (model, &iter, FALSE, removes);
		} while (gtk_tree_model_iter_next (model, &iter));

		//next, eliminate rows that were un-processable
		if (removes->len > 0)
			_e2_plugins_remove_rows (model, &iter, removes);
		g_ptr_array_free (removes, TRUE);
		//next, add back rows for 'partially-loaded' plugins
		_e2_plugins_append_loaded_actions (model, FALSE);
		//next, eliminate old runtime data which are not represented in the new app.plugacts
		if (old->len > 0)
		{
			guint i;
			gpointer *dp;
			PluginAction *pa;
			GHashTable *more = g_hash_table_new_full (g_str_hash, g_str_equal,
				g_free, NULL);

			for (i = 0, dp = old->pdata; i < old->len; i++, dp++)
			{
				pa = *((PluginAction**)dp);
				if (_e2_plugins_get_matching_index (new, pa->signature) < 0)
				{
					//maybe this one is to go
					gchar *sigvers = _e2_plugins_derive_key (NULL, pa->signature);
					if (sigvers != NULL)
					{
						Plugin *p = g_hash_table_lookup (app.plugins, sigvers);
						if (p != NULL)
						{
							//single-action plugins can safely be removed immediately
							if (p->actscount > 1 || !e2_plugins_unload1 (p, FALSE))
							{
								//multi-action plugin, or single-action but unload failed
								g_hash_table_replace (more, sigvers, p);
								sigvers = NULL;	//don't free the table key
							}
						}
						else //somehow got lost
						{
							printd (DEBUG, "plugin %s not found, can't be removed from hashtable", sigvers);
						}
						g_free (sigvers);
					}
				}
			}
			//finally, process any still-unresolved plugins
			if (g_hash_table_size (more) > 0)
			{
				g_hash_table_foreach_remove (more,
					(GHRFunc) _e2_plugins_unload1_unresolved, model);
				if (g_hash_table_size (more) > 0) //not all could be unloaded
					_e2_plugins_append_loaded_actions (model, FALSE);
			}
			g_hash_table_destroy (more);
		}
		g_ptr_array_free (old, TRUE);
	}
	else	//no store now (impossible?), or nothing in it
	{
		e2_plugins_unload_all (FALSE);	//cleanup existing data, where possible
		if (g_hash_table_size (app.plugins) > 0) //not all could be unloaded
		{
			if (model != NULL)
				_e2_plugins_append_loaded_actions (model, TRUE);
		}
	}
}
/**
@brief get the plugin with the specified @a signature

If the plugin is not present already, try to load it. Otherwise, bump its refcount.
This is intended mainly for plugins' interactions with each other

@param sigvers unique ID string for the desired plugin, like ANAME VERSION,
 where ANAME matches the 'core' of the plugin filename
@param with_ref TRUE to bump plugin refcount if it's already loaded

@return pointer to matching plugin data struct, else NULL
*/
Plugin *e2_plugins_get_plugin (const gchar *sigvers, gboolean with_ref)
{
	Plugin *p = e2_plugins_get_installed (sigvers);
	if (p != NULL)
	{
		if (with_ref)
			p->refcount++;
	}
	else
		p = _e2_plugins_load_named (sigvers);

	return p;
}

/**
@brief check whether plugin with specified @a signature is loaded

This is intended mainly for plugins' interactions with each other

@param sigvers unique ID string for the desired plugin, like ANAME VERSION,
 where ANAME matches the 'core' of the plugin filename

@return pointer to data struct for plugin which has the specified ID, else NULL
*/
Plugin *e2_plugins_get_installed (const gchar *sigvers)
{
	if (app.plugins != NULL)
		return ((Plugin*)g_hash_table_lookup (app.plugins, sigvers));
	return NULL;
}

/**
@brief find address of a function in a plugin identified by @a sigvers

This is intended to be used by main code or other plugins which want to interact.
If the plugin with the desired function is not presently loaded, any unloaded
plugins recorded in config data are polled for a matching signature, and if so,
the plugin will be loaded

@param sigvers unique ID string for the desired plugin, like ANAME VERSION,
 where ANAME matches the 'core' of the plugin filename
@param func_name the name of the function to find
@param address store for the located address

@return TRUE if the address is found
*/
gboolean e2_plugins_find_function (const gchar *sigvers, const gchar *func_name,
	gpointer *address)
{
	Plugin *p = e2_plugins_get_installed (sigvers);
	if (p == NULL)
	{
		p = _e2_plugins_load_named (sigvers);
		if (p == NULL)
			return FALSE;
	}

	if (g_module_symbol (p->module, func_name, address))
		return TRUE;

	printd (WARN, "couldn't find %s in module %s: %s", func_name, sigvers,
		g_module_error());
	return FALSE;
}
/**
@brief run a dialog showing plugins config data

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the dialog was established
*/
gboolean e2_plugins_configure (gpointer from, E2_ActionRuntime *art)
{
	return (
	e2_config_dialog_single ("plugins", e2_plugins_update_configured, TRUE) //internal name, no translation
	!= NULL);
}
/**
@brief register plugin action

This is a wrapper for the standard action registration function
It also manages a refcount for the action, and may free the name of @a newaction
@param newaction pointer to original action data to be copied and heaped

@return registered action E2_Action
*/
E2_Action *e2_plugins_action_register (const E2_Action *newaction)
{
	E2_Action *action = e2_action_get (newaction->name);
	if (action == NULL)
		action = e2_action_register (newaction);
	else
		g_free (newaction->name);

	return action;
}
/**
@brief unregister plugin action named @a name
This is a wrapper for the standard action deregistration function.
It also manages a refcount for the action.
@param name action name string

@return TRUE if the action was actually unregistered
*/
gboolean e2_plugins_action_unregister (const gchar *name)
{
	return (e2_action_unregister (name));
}
/**
@brief register a plugin config option
This is a wrapper for the standard option registration function
It also manages a refcount for the option
NOTE caller needs special care for tree-options, if those are supported (not ATM)

@param type flag for the type of set that it is
@param name name of the option, a constant string
@param group group the option belongs to, used in config dialog, a r-t string  FREEME
@param desc textual description of the option used in config dialog, a r-t _() string FREEME ?
@param tip tooltip used when displaying the config dialog, a _() string, or NULL
@param depends name of another option this one depends on, or NULL
@param ex pointer to type-specific data for initialisation
@param flags bitflags determining how the option data will be handled

@return the option data struct
*/
E2_OptionSet *e2_plugins_option_register (E2_OptionType type, gchar *name,
	gchar *group, gchar *desc, gchar *tip, gchar *depends,
	E2_OptionSetupExtra *ex, E2_OptionFlags flags)
{
	E2_OptionSet *set = e2_option_get (name);
	if (set == NULL)
	{
		switch (type)
		{
		 case E2_OPTION_TYPE_BOOL:
			set = e2_option_bool_register (name, group, desc, tip, depends,
				ex->exbool, flags);
			break;
		 case E2_OPTION_TYPE_INT:
			set = e2_option_int_register (name, group, desc, tip, depends,
				ex->exint.def, ex->exint.min, ex->exint.max, flags);
			break;
		 case E2_OPTION_TYPE_SEL:
			set = e2_option_sel_register (name, group, desc, tip, depends,
				ex->exsel.def, ex->exsel.values, flags);
			break;
		 case E2_OPTION_TYPE_STR:
			set = e2_option_str_register (name, group, desc, tip, depends,
				ex->exstr, flags);
			break;
//		 case E2_OPTION_TYPE_FONT:
//			set = e2_option_font_register (name, group, desc, tip, depends,
//				ex->exstr, flags);
//			break;
//		 case E2_OPTION_TYPE_COLOR:
//			set = e2_option_color_register (name, group, desc, tip, depends,
//				ex->exstr, flags);
//			break;
//		 case E2_OPTION_TYPE_ICON:
//			set =
//			break;
//		 case E2_OPTION_TYPE_TREE:
//			set =
//			break;
		 default:
			printd (ERROR, "BAD attempt to register unsupported plugin option");
			break;
		}
	}
	return set;
}
/**
@brief unregister plugin option named @a name
This is a wrapper for the standard option deregistration function
It also manages a refcount for the option
@param name option name string

@return TRUE if the option was actually unregistered
*/
gboolean e2_plugins_option_unregister (gchar *name)
{
	return (e2_option_backup (name));
}
/* *
@brief (de)sensitize option tree buttons for selected option tree row
Config dialog page buttons are de-sensitized if the row is not a 'child' plugin
@param selection pointer to selection
@param model UNUSED
@param path
@param path_currently_selected
@param set data struct for the plugins option UNUSED
@return TRUE always (the row is always selectable)
*/
/*static gboolean _e2_plugin_tree_selection_check_cb (
	GtkTreeSelection *selection, GtkTreeModel *model, GtkTreePath *path,
	gboolean path_currently_selected, E2_OptionSet *set)
{
	GtkTreeView *view = gtk_tree_selection_get_tree_view (selection);
	gboolean result = gtk_tree_path_get_depth (path) == 1;
	NEEDCLOSEBGL
	e2_option_tree_adjust_buttons (view, result);
	NEEDOPENBGL
	return TRUE;
}
*/
/* *
@brief decide whether tree row is draggable
Checks whether row depth is 1 i.e. not a child plugin. If so, the row is draggable
@param drag_source GtkTreeDragSource data struct
@param path tree path to a row on which user is initiating a drag
@return TRUE if the row can be dragged
*/
/*static gboolean _e2_plugin_tree_draggable_check_cb (
	GtkTreeDragSource *drag_source, GtkTreePath *path)
{
	return (gtk_tree_path_get_depth (path) == 1);
}
*/
/**
@brief install default tree options for plugins
This function is called only if the default is missing from the config file
This is essentially a list of all the expected plugin filenames
The menu name, icon and tip are retrieved from the plugin file
The 'path' field near the end is empty if the plugins default dir is to be used
@param set pointer to set data

@return
*/
static void _e2_plugins_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("plugins=<"), //internal name
	g_strdup ("true|true||||e2p_glob.so||"),
	g_strdup ("false|false||||e2p_selmatch.so||"),
	g_strdup ("false|false||||e2p_tag.so||"),
	g_strdup ("true|true||||e2p_foreach.so||"),
	g_strdup ("true|true||||e2p_rename.so||"),
	g_strdup ("true|true||||e2p_find.so||"),
#ifdef E2_TRACKER
	g_strdup ("false|false||||e2p_track.so||"),
#endif
#ifdef E2_THUMBNAILS
	g_strdup ("true|true||||e2p_thumbnail.so||"),
#endif
	g_strdup ("true|true||||e2p_pack.so||"),
	g_strdup ("true|false||||e2p_unpack.so||"),
	g_strdup ("true|true||||e2p_clipnames.so||"),
	g_strdup ("false|false||||e2p_copy.so||"),
	g_strdup ("false|false||||e2p_move.so||"),
	g_strdup ("false|false||||e2p_timeset.so||"),
	g_strdup ("false|false||||e2p_dircmp.so||"),
#ifdef E2_ACL
	g_strdup ("false|false||||e2p_acl.so||"),
#endif
	g_strdup ("false|false||||e2p_config.so||"),
	g_strdup ("false|false||||e2p_clone.so||"),
	g_strdup ("false|false||||e2p_view.so||"),
	g_strdup ("false|false||||e2p_extsort.so||"),
	g_strdup ("true|true||||e2p_du.so||"),
#ifdef E2_VFS
	g_strdup ("false|false||||e2p_vfs.so||"),
	g_strdup ("false|false||||e2p_gvfs.so||"),
#endif
	g_strdup(">"),
	NULL);
}
/**
@brief register plugins actions

@return
*/
void e2_plugins_options_register (void)
{
	//no screen rebuilds needed after any change to this option
	gchar *group_name = _C(34); //_("plugins")
	E2_OptionSet *set = e2_option_tree_register ("plugins", group_name,
		group_name, NULL, NULL, NULL, //_e2_plugin_tree_selection_check_cb, _e2_plugin_tree_draggable_check_cb,
		E2_OPTION_TREE_LIST | E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDPLUGS);
	//must conform to *.COL enum
	e2_option_tree_add_column (set, _("Loaded"), E2_OPTION_TREE_TYPE_BOOL, TRUE, "true",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Menu"), E2_OPTION_TREE_TYPE_BOOL, TRUE, "true",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Label"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Icon"), E2_OPTION_TREE_TYPE_ICON, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Tooltip"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
#ifdef SHOW_PLUGPATHS
	e2_option_tree_add_column (set, _("Filename"), E2_OPTION_TREE_TYPE_STR, 0, "?.so",
		0, NULL, NULL); //file-name E2_OPTION_TREE_COL_NOT_EDITABLE flag ?
	e2_option_tree_add_column (set, _("Location"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL); //file-path
#else
	e2_option_tree_add_column (set, "", E2_OPTION_TREE_TYPE_HIDDENSTR, 0, "",
		0, NULL, NULL); //file-name E2_OPTION_TREE_COL_NOT_EDITABLE flag ?
	e2_option_tree_add_column (set, "", E2_OPTION_TREE_TYPE_HIDDENSTR, 0, "",
		0, NULL, NULL); //file-path
#endif
	e2_option_tree_add_column (set, "", E2_OPTION_TREE_TYPE_HIDDENSTR, 0, "",
		0, NULL, NULL); //signature
	e2_option_tree_create_store (set);

	e2_option_tree_prepare_defaults (set, _e2_plugins_tree_defaults);
}
