/* $Id: e2_option.c 3051 2014-02-08 21:59:43Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>

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
@file src/config/e2_option.c
@brief general option handling

This file contains general functions to register, unregister and set
options.
*/
/**
\page options the configuration system

ToDo - description of how options work
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_fs.h"
#include "e2_option.h"
#include "e2_option_tree.h"
#include "e2_action.h"
#include "e2_task.h"
#include "e2_filetype.h"
#include "e2_plugins.h"
#include "e2_cl_option.h"
#include "e2_filelist.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif

static void _e2_option_clean1 (E2_OptionSet *set);
//disable-refresh-counter for config file
volatile gint cfg_refresh_refcount = 0;
#ifdef E2_FAM_KERNEL
volatile
#endif
 gint cfgdirty; //<>0 when some change to config-data file has been detected
extern time_t last_work_time;

static gboolean _e2_option_repoll (GtkWidget *widget, GdkEvent *event,
	gpointer userdata)
{
	g_signal_handlers_disconnect_by_func ((gpointer)app.main_window,
		_e2_option_repoll, userdata);
	last_work_time = time (NULL);
	printd (DEBUG, "Resume operation of timer CONFIG_T");
	app.timers[CONFIG_T] =
#ifdef USE_GLIB2_14
		g_timeout_add_seconds (E2_CONFIGCHECK_INTERVAL_S,
#else
		g_timeout_add (E2_CONFIGCHECK_INTERVAL,
#endif
		(GSourceFunc) e2_option_check_config_files, NULL);
#ifdef E2_FAM_KERNEL
	e2_fs_FAM_resume ();
#endif
	return FALSE;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief Initialize the config directory pointer
It's intended that config dirs be native, not virtual.
The config directory pointer (e2_cl_options.config_dir) is set from command line
or default dir. If the dir doesn't exist, create it if possible.
Any problem results in an appropriate error message.

@return TRUE if dir exists or is successfully created, else FALSE
*/
gboolean e2_option_set_config_dir (void)
{
	gboolean retval = TRUE;
	//check if the config dir is really there
	gchar *local = F_FILENAME_TO_LOCALE (e2_cl_options.config_dir);
#ifdef E2_VFS
	VPATH ddata = { local, NULL };	//only local config data
	if (!e2_fs_is_dir3 (&ddata E2_ERR_NONE()))
#else
	if (!e2_fs_is_dir3 (local E2_ERR_NONE()))
#endif
	{	//if not, try to create it
		printd (INFO, "creating config directory '%s'", e2_cl_options.config_dir);
#ifdef E2_VFS
		if (e2_fs_recurse_mkdir (&ddata, 0755 E2_ERR_NONE()))
#else
		if (e2_fs_recurse_mkdir (local, 0755 E2_ERR_NONE()))
#endif
		{
			printd (WARN, "could not create config directory '%s'",
				e2_cl_options.config_dir);
			retval = FALSE;
		}
	}
	//it's there, but can we use it ?
#ifdef E2_VFS
	else if (e2_fs_access (&ddata, R_OK | W_OK | X_OK E2_ERR_NONE()))
#else
	else if (e2_fs_access (local, R_OK | W_OK | X_OK E2_ERR_NONE()))
#endif
	{	//nope
		printd (WARN, "cannot access config directory '%s'",
			e2_cl_options.config_dir);
		retval = FALSE;
	}
	F_FREE (local, e2_cl_options.config_dir);
	return retval;
}
/**
@brief Helper for making chilren of trash dir
Assumes the user will be the owner of any created dir
@param localpath fully-qualified path of dir to create, or parent of dir to create
@param childdir name of subdir in @a localpath, or NULL

@return TRUE if dir created successfully
*/
static gboolean _e2_option_make_trash (const gchar *localpath, const gchar *childdir)
{
	gboolean success;
	gchar *dir = (childdir != NULL) ? g_build_filename (localpath, childdir, NULL) : (gchar*)localpath;
	printd (INFO, "creating trash directory '%s'", dir);
#ifdef E2_VFS
	VPATH ddata = { dir, NULL };	//only local config data
	success = !e2_fs_recurse_mkdir (&ddata, 0755 E2_ERR_NONE());
#else
	success = !e2_fs_recurse_mkdir (dir, 0755 E2_ERR_NONE());
#endif
	if (childdir != NULL)
		g_free (dir);
	return success;
}
/**
@brief Initialize the default trash directories and a pointer to the topmost
It is intended that this default be native, not virtual, and owned by the current user
Uses command-line option if that was provided.
Otherwise, tries to use XDG standard, or if that is not available, tries
the e2 config dir.
Creates dir(s) that don't exist, or print error message.
Sets e2_cl_options.trash_dir to the path (with a trailing '/'), or it stays NULL
if dir not available.
Needs to be called after the config dir is checked/created
Expects BGL off/open
@return
*/
void e2_option_set_trash_dir (void)
{
	gboolean success = TRUE;
	gchar *local = F_FILENAME_TO_LOCALE (e2_cl_options.trash_dir);
	//make the trash dir if it doesn't exist
#ifdef E2_VFS
	VPATH ddata = { local, NULL };	//only local config data
	if (!e2_fs_is_dir3 (&ddata E2_ERR_NONE()))
#else
	if (!e2_fs_is_dir3 (local E2_ERR_NONE()))
#endif
	{
		success = _e2_option_make_trash (local, "files");
		if (success)
			success = _e2_option_make_trash (local, "info");
		if (!success)
		{
			gchar *message = g_strdup_printf (_("Cannot create trash directory %s"),
				e2_cl_options.trash_dir);
			CLOSEBGL
			e2_output_print_error (message, TRUE);
			OPENBGL
		}
	}
	//it's there, but can we use it ?
#ifdef E2_VFS
	else if (e2_fs_access (&ddata, R_OK | W_OK | X_OK E2_ERR_NONE()))
#else
	else if (e2_fs_access (local, R_OK | W_OK | X_OK E2_ERR_NONE()))
#endif
	{	//nope
		success = FALSE;
		printd (WARN, "cannot use trash directory '%s'",
			e2_cl_options.trash_dir);
	}
	else
	{	//yup, the 'parent' is ok
		gchar *subdir = g_build_filename (local, "files", NULL);
#ifdef E2_VFS
		ddata.path = subdir;
		if (e2_fs_access (&ddata, F_OK E2_ERR_NONE()))
#else
		if (e2_fs_access (subdir, F_OK E2_ERR_NONE()))
#endif
		{
			success = _e2_option_make_trash (subdir, NULL);
		}
		else
		{
#ifdef E2_VFS
			success = !e2_fs_access (&ddata, R_OK | W_OK | X_OK E2_ERR_NONE());
#else
			success = !e2_fs_access (subdir, R_OK | W_OK | X_OK E2_ERR_NONE());
#endif
		}
		g_free (subdir);
		subdir = g_build_filename (local, "info", NULL);
#ifdef E2_VFS
		ddata.path = subdir;
		if (e2_fs_access (&ddata, F_OK E2_ERR_NONE()))
#else
		if (e2_fs_access (subdir, F_OK E2_ERR_NONE()))
#endif
		{
			success = success && _e2_option_make_trash (subdir, NULL);
		}
		else
		{
#ifdef E2_VFS
			success = success && !e2_fs_access (&ddata, R_OK | W_OK | X_OK E2_ERR_NONE());
#else
			success = success && !e2_fs_access (subdir, R_OK | W_OK | X_OK E2_ERR_NONE());
#endif
		}
		g_free (subdir);
	}

	F_FREE (local, e2_cl_options.trash_dir);
	if (success)
	{
		//cleanup, add trailer
		e2_cl_options.trash_dir = e2_utils_path_clean (e2_cl_options.trash_dir);
	}
	else
	{
		g_free (e2_cl_options.trash_dir);
		e2_cl_options.trash_dir = NULL;
	}
}
/**
@brief dump all current configuration data and recreate it, with window content updates as appropriate
This may be called from within a timer function, or manually
It must not be run when there is an open config dialog (closing that causes crash)
Probably needs BGL closed upon arrival here.
@param reload TRUE to reload the config file before recreating config data
@param recreate TRUE to recreate main window in accord with updated data,
  FALSE to just update a few things

@return
*/
void e2_option_refresh (gboolean reload, gboolean recreate)
{
	e2_option_disable_config_checks (); //block recursion
	//this is the one option that we want to keep ...
	gboolean advanced = e2_option_bool_get ("advanced-config");
	e2_keybinding_clean ();
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_clean ();
#endif
	g_hash_table_destroy (app.filetypes);
	e2_plugins_unload_all (FALSE);	//do this before data are cleared
	//clear relevant current information
	e2_option_clear_data ();
	//now re-create things, in the same order as at session-start
	e2_option_default_register ();
	e2_option_date_style (); //customise date-format option

	if (reload)
	{
		printd (INFO, "reloading config due to external change");
		e2_option_file_read (NULL);
	}

	e2_option_tree_install_defaults ();
	e2_plugins_load_configured (); // load plugins (if any)

	//re-initialise things that are not done in normal 'recreate' functions
	e2_pane_create_option_data (&app.pane1);
	e2_pane_create_option_data (&app.pane2);

	if (recreate)
		e2_window_recreate (&app.window); //this also recreates bindings
	else
	{
		e2_toolbar_create (&app.pane1.toolbar);
		e2_toolbar_rebadge (&app.pane1.toolbar);
		e2_toolbar_create (&app.pane2.toolbar);
		e2_toolbar_rebadge (&app.pane2.toolbar);
		e2_toolbar_create (&app.toolbar);
		e2_toolbar_create (&app.commandbar);

		e2_keybinding_register_all ();
#ifdef E2_MOUSECUSTOM
		e2_mousebinding_register_all ();
#endif
	}

	e2_filetype_add_all ();

	e2_option_bool_set ("advanced-config", advanced);

	e2_option_enable_config_checks (); //unblock
}
/**
@brief disable checking for changed config file, if that is in effect
As this is suspension, not "setup", the poll-timer is not handled here, nor is
the backend monitoring if any

@return
*/
void e2_option_disable_config_checks (void)
{
	if (e2_option_bool_get ("auto-refresh-config"))
		g_atomic_int_add (&cfg_refresh_refcount, 1);
}
/**
@brief enable checking for changed config file, if that is in effect
As this is end of suspension, the poll-timer is not handled here, nor is
the backend monitoring if any

@return
*/
void e2_option_enable_config_checks (void)
{
	if (e2_option_bool_get ("auto-refresh-config"))
	{
		if (g_atomic_int_exchange_and_add (&cfg_refresh_refcount, -1) < 1)
			g_atomic_int_set (&cfg_refresh_refcount, 0);
	}
}
/**
@brief stop change-monitoring
Stop the timer which polls for config file 'dirty' indicator, and if relevant
stop the 'kernel-backend' process(es)
No check here for "auto-refresh-config" option status

@return
*/
void e2_option_stop_config_checks (void)
{
	if (app.timers[CONFIG_T] != 0)
	{
# ifdef E2_REFRESH_DEBUG
		printd (DEBUG, "stop timer CONFIG_T");
# endif
		g_source_remove (app.timers[CONFIG_T]);
		app.timers[CONFIG_T] = 0;
	}
#ifdef E2_FAM_KERNEL
	e2_fs_FAM_cancel_monitor_config (); //stop backend
#endif
	g_atomic_int_set (&cfg_refresh_refcount, 0);
}
/**
@brief start change-monitoring
Start a timer which polls for config file 'dirty' indicator, and if relevant
start the 'kernel-backend' process(es).
No check here for "auto-refresh-config" option status
The timer may later be stopped permanently if change-monitoring is abandoned,
probably via a config dialog. But it won't be stopped merely if
change-monitoring is disabled i.e. suspended

@return
*/
void e2_option_start_config_checks (void)
{
	g_atomic_int_set (&cfg_refresh_refcount, 0);
	if (app.timers[CONFIG_T] == 0)
	{
# ifdef E2_REFRESH_DEBUG
		printd (DEBUG, "start timer CONFIG_T");
# endif
		app.timers[CONFIG_T] =
#ifdef USE_GLIB2_14
			g_timeout_add_seconds (E2_CONFIGCHECK_INTERVAL_S,
#else
			g_timeout_add (E2_CONFIGCHECK_INTERVAL,
#endif
			(GSourceFunc) e2_option_check_config_files, NULL);
	}
#ifdef E2_FAM_KERNEL
	e2_fs_FAM_resume ();	//ensure it's polled there too
	e2_fs_FAM_monitor_config (); //start backend
#endif
}
/**
@brief check for and respond to changed config file

This is called by the check-config-files timer, and may be called directly.
With E2_FAM, update is normally detected and flagged as part of the filepane
monitoring, as all change-data arrives via the same stream.
Otherwise, here we check config file mtime against a cached value.
If update is indicated, re-create window after re-reading config data.
@param user_data UNUSED pointer specified when the timer was initiated
@return TRUE, always, so timer continues
*/
gboolean e2_option_check_config_files (gpointer user_data)
{
	static gboolean cfgdeferred = FALSE;
	printd (DEBUG, "In e2_option_check_config_files()");
#ifdef E2_FAM
	if (app.monitor_type != E2_MONITOR_DEFAULT)
	{
		if (cfgdeferred || g_atomic_int_get (&cfgdirty))
		{
			if (g_atomic_int_get (&cfg_refresh_refcount))
			{
				cfgdeferred = TRUE;
				//don't clear the main flag, if set
				printd (DEBUG, "config refresh deferred");
			}
			else
			{
				//force default settings
				cfgdeferred = FALSE;
				g_atomic_int_set (&cfgdirty, 1);
				printd (DEBUG, "config refresh flag set");
			}
		}
	}
	else
	{	//only polling available
#endif
		struct stat stat_buf;
		gchar *cfg_file = g_build_filename (e2_cl_options.config_dir, default_config_file, NULL);
		gchar *local = F_FILENAME_TO_LOCALE (cfg_file);
		cfgdirty = 0;
#ifdef E2_VFS
		VPATH ddata = { local, NULL }; //config data always local
		if (!e2_fs_stat (&ddata, &stat_buf E2_ERR_NONE())
#else
		if (!e2_fs_stat (local, &stat_buf E2_ERR_NONE())
#endif
			&& stat_buf.st_size >= 20	//first line of config file should be "# emelFM2 (v x.y.z)"
			&& !S_ISDIR (stat_buf.st_mode))
		{
			if (stat_buf.st_mtime != app.config_mtime)
			{
				if (cfg_refresh_refcount == 0)
				{
					cfgdirty = 1;
					cfgdeferred = FALSE;
					app.config_mtime = stat_buf.st_mtime;
				}
				else
					cfgdeferred = TRUE;
			}
		}
		g_free (cfg_file);
		F_FREE (local, cfg_file);
#ifdef E2_FAM
	}
	if (!cfgdeferred && g_atomic_int_get (&cfgdirty))
#else
	if (!cfgdeferred && cfgdirty)
#endif
	{
		//CHECKME e2_option_refresh() probably needs BGL closed
//		printd (DEBUG, "At e2_option_check_config_files 1, goto e2_option_refresh");
		CLOSEBGL
		e2_option_refresh (TRUE, TRUE);
		e2_output_print (&app.tab, _("Configuration data re-loaded"), NULL, TRUE, NULL);
		OPENBGL
		g_atomic_int_set (&cfgdirty, 0);
		last_work_time = time (NULL);
	}
	else
#ifdef E2_FAM
		if (!cfgdeferred)
#endif
	{
		if (time (NULL) >= last_work_time + QUIET_SECONDS)
		{
			printd (DEBUG, "Suspend operation of timer CONFIG_T");
			g_source_remove (app.timers[CONFIG_T]);
			app.timers[CONFIG_T] = 0;
#ifdef E2_FAM_KERNEL
			//backend shutdown too
			e2_fs_FAM_suspend ();
#endif
			//arrange to restart upon activity
			g_signal_connect (G_OBJECT (app.main_window), "event",
				G_CALLBACK (_e2_option_repoll), GUINT_TO_POINTER (CONFIG_T));
		}
	}
	return TRUE;
}
/**
@brief register a config option
String-paramteters are not copied! Upstream needs to ensure continuing validity
(careful with plugins!)
This does not return if there's an allocation failure.
@param type flag for the type of set that it is
@param name name of the option
@param group group the option belongs to, used in config dialog
@param desc textual description of the option used in config dialog
@param tip tooltip used when displaying the config dialog, or NULL
@param depends name of another option this one depends on, or NULL
@param flags bitflags determining how the option data is handled

@return the option data struct
*/
E2_OptionSet *e2_option_register (E2_OptionType type, gchar *name, gchar *group,
	gchar *desc, gchar *tip, gchar *depends, E2_OptionFlags flags)
{
	E2_OptionSet *set = e2_option_get_simple (name);
	if (set == NULL)
	{
#ifdef USE_GLIB2_10
		set = (E2_OptionSet *) g_slice_alloc0 (sizeof (E2_OptionSet));
//		set = ALLOCATE0 (E2_OptionSet);
#else
		set = ALLOCATE0 (E2_OptionSet);
#endif
#if (CHECKALLOCATEDFATAL)
		CHECKALLOCATEDFATAL (set)
#else
		if (set != NULL)
		{
#endif
			g_ptr_array_add (options_array, set);
			g_hash_table_insert (options_hash, name, set);	//replace not valid
			//non-freeable parameters set only once
			set->type = type;
			set->flags = flags;
//			set->hook_freezed = FALSE; zero'd struct
//			set->widget = NULL;
#if !(CHECKALLOCATEDFATAL)
		}
#endif
	}
	//CHECKME handle re-registration of a different type ?
	//string parameters set each registration, so clear any old versions elsewhere
	if (set != NULL)
	{
		set->name = name;
		set->group = group;
		set->desc = desc;
		set->tip = tip;
		set->depends = depends;
		g_hook_list_init (&set->hook_value_changed, sizeof (GHook));
	}
	else
	{
		//FIXME advise user and cleanup
		exit (1);
	}
	return set;
}
/* *
@brief unregister option named @a name
This is for plugin options, essentially
@param name option name string

@return TRUE if the option was unregistered
*/
/*gboolean e2_option_unregister (gchar *name)
{
	E2_OptionSet *set = g_hash_table_lookup (options_hash, name);
	if (set == NULL)
		return FALSE;

	g_ptr_array_remove (options_array, set);
	g_hash_table_remove (options_hash, name);
	return TRUE;
}
*/
/**
@brief unregister and backup option named @a name
This is for plugin options, essentially
FIXME ATM is only for non-tree options
@param name option name string
@return TRUE if @a option exists
*/
gboolean e2_option_backup (gchar *name)
{
	E2_OptionSet *set = g_hash_table_lookup (options_hash, name);
	if (set != NULL)
	{
		gchar *saveme;
		//reinstate the form of the config line(s)
		switch (set->type)
		{
			case E2_OPTION_TYPE_INT:
				saveme = g_strdup_printf ("%s=%d", name, set->ival);
				break;
			case E2_OPTION_TYPE_BOOL:
			case E2_OPTION_TYPE_STR:
			case E2_OPTION_TYPE_FONT:
			case E2_OPTION_TYPE_COLOR:
			case E2_OPTION_TYPE_SEL:
				saveme = g_strconcat (name, "=", set->sval, NULL);
				break;
/*			case E2_OPTION_TYPE_TREE:
				saveme = %s=<, option
				GtkTreeIter iter;
				if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
					e2_option_tree_write_to_file (f, set, &iter, 0);	//error handling ?
				if (e2_fs_file_write (f, ">\n") == 0)
					goto error_handler;
				break;
*/
			default:
				printd (WARN, "don't know how to backup option '%s'", name);
				return FALSE;
		}
		//log the freeable strings
		e2_option_unknown_record (g_strdup (name), saveme);

		g_ptr_array_remove (options_array, set);
		g_hash_table_remove (options_hash, name);
		return TRUE;
	}
	return FALSE;
}
/**
@brief get E2_OptionSet with warning

@param option name of the option

@return option or NULL if it cannot be found
*/
E2_OptionSet *e2_option_get (const gchar *option)
{
	E2_OptionSet *set = (E2_OptionSet *) g_hash_table_lookup (options_hash, option);
#ifdef DEBUG_MESSAGES
	if (set == NULL)
		printd (WARN, "trying to get option '%s' which doesn't exist", option);
#endif
	return set;
}
/**
@brief get E2_OptionSet without warning

@param option name of the option

@return option or NULL if the option cannot be found
*/
E2_OptionSet *e2_option_get_simple (const gchar *option)
{
	return (E2_OptionSet *) g_hash_table_lookup (options_hash, option);
}

/* UNUSED
void *e2_option_void_get (E2_OptionSet *set, gchar *option)
{
	switch (set->type)
	{
		case E2_OPTION_TYPE_BOOL:
			if (!strcmp (option, "true")) return (void *) TRUE;
			else return (void *) FALSE;
			break;
		case E2_OPTION_TYPE_STR:
		case E2_OPTION_TYPE_FONT:
		case E2_OPTION_TYPE_ICON:
		case E2_OPTION_TYPE_SEL:
			return (void *) option;
		default:
			return NULL;
			break;
	}
	return NULL;
}
*/

E2_OptionSet *e2_option_attach_value_changed (gchar *option, GtkWidget *widget,
	gboolean (*func)(gpointer,gpointer), gpointer data)
{
	E2_OptionSet *set = e2_option_get (option);
	if (set != NULL)
		e2_option_attach_value_changed_simple (set, widget, func, data);
	return set;
}

void e2_option_attach_value_changed_simple (E2_OptionSet *set, GtkWidget *widget,
	gboolean (*func)(gpointer,gpointer), gpointer data)
{
	GHook *hook = e2_hook_register (&set->hook_value_changed, func, data);
	E2_Duo *duo = MALLOCATE (E2_Duo);	//too small for slice
	duo->a = hook;
	duo->b = &set->hook_value_changed;
	g_object_set_data_full (G_OBJECT (widget), "e2-option-attach-value-changed",
		duo, (GDestroyNotify) e2_hook_unattach);
}

void e2_option_connect (GtkWidget *controller, gboolean active)
{
	g_object_set_data (G_OBJECT (controller), "e2-controller-blocked",
		GINT_TO_POINTER (!active));
}
/**
@brief clear all relevant option data prior to a refresh of all options
@return
*/
void e2_option_clear_data (void)
{
	g_ptr_array_set_size (options_array, 0); //no need for data clearing
#ifdef USE_GLIB2_12
	g_hash_table_remove_all (options_hash);	//data cleared
#else
//	g_hash_table_foreach_remove (options_hash, (GHRFunc) <return TRUE>, NULL);
	g_hash_table_destroy (options_hash);
	options_hash = g_hash_table_new_full (g_str_hash, g_str_equal, NULL,
		(GDestroyNotify) _e2_option_clean1);
#endif
}
/**
@brief setup date-format array value
@return
*/
void e2_option_date_style (void)
{
	//replicate part of e2_option_sel_register()
	E2_OptionSet *set = e2_option_get_simple ("date-string");
	gchar **title = &set->ex.sel.def[5];
	g_free (*title);

	if (e2_option_bool_get ("advanced-config"))
	{
		extern const gchar *custom_date_label;
		*title = g_strdup (custom_date_label);
		set->ex.sel.def_count = 6;
	}
	else
	{
		if (e2_option_sel_get_direct (set) == 5)
			e2_option_sel_set_direct (set, 4); //revert to localised
		*title = NULL;
		set->ex.sel.def_count = 5;
	}
}

/*void e2_option_tree_stores_clear (void)
{
	guint i;
	gpointer *walker;
	E2_OptionSet *set;
	for (i = 0, walker = options_array->pdata; i < options_array->len; i++, walker++)
	{
		set = *walker;
		if (set->type == E2_OPTION_TYPE_TREE)
			gtk_tree_store_clear (set->ex.tree.model);
	}
} */
/* this was used only at session end, so don't bother to cleanup
void e2_option_destroy_config ()
{
	g_ptr_array_free (options_array, TRUE);
	g_hash_table_destroy (options_hash);
	g_hash_table_destroy (options_queue);
} */

gboolean write_error = FALSE;
static void _e2_option_file_write_unknowns (gpointer key, gpointer value, E2_FILE *f)
{
	if (e2_fs_file_write (f, "%s\n", (gchar *)value) == 0)
		write_error = TRUE;
}
/**
@brief write current configuration to native file @a utfpath
The current configuration will be written to the default config file 'config'.
Expects BGL to be on/closed
@param utfpath absolute path of file to write (utf-8 string), or NULL to use default

@return
*/
void e2_option_file_write (const gchar *utfpath)
{
	if (e2_cl_options.original)
		return;
#ifdef E2_VFS
	VPATH ddata;
	VPATH tdata;
#endif
	gboolean freepath = (utfpath == NULL);
	gchar *cfg_file = (freepath) ?
		g_build_filename (e2_cl_options.config_dir, default_config_file, NULL):
		(gchar *)utfpath;
	gchar *local = F_FILENAME_TO_LOCALE (cfg_file);
	gchar *tempname = e2_utils_get_tempname (local);

	E2_FILE *f = e2_fs_open_writestream (tempname E2_ERR_NONE());
	if (f != NULL)
	{
		printd (DEBUG, "write config file: %s", cfg_file);
		if (e2_fs_file_write (f,
			//the 1st line is language-independent, for version verification
			"# "PROGNAME" (v "VERSION RELEASE")\n\n" ) == 0)
				goto error_handler;
		if (e2_fs_file_write (f,
		 //FIXME = remind translators not to remove # from line starts
		  _("# This is the %s configuration data file.\n"
			"# It will be overwritten each time the program is run!\n\n"
			"# If you're inclined to edit the file between program sessions, note this:\n"
			"# for tree options, you have to use \\| to escape | and you have to use \\< to escape <,\n"
			"#  if that is the first non-space character on a line.\n\n"),
			PROGNAME
		) == 0)
				goto error_handler;

		guint i;
		gpointer *walker;
		E2_OptionSet *set;
		for (i = 0, walker = options_array->pdata; i < options_array->len; i++, walker++)
		{
			set = *walker;
			switch (set->type)
			{
				case E2_OPTION_TYPE_INT:
					if (e2_fs_file_write (f, "# %s\n%s=%d\n",
						set->desc, set->name, set->ival) == 0)
							goto error_handler;
					break;
				case E2_OPTION_TYPE_BOOL:
				case E2_OPTION_TYPE_STR:
				case E2_OPTION_TYPE_FONT:
				case E2_OPTION_TYPE_COLOR:
				case E2_OPTION_TYPE_SEL:
					if (e2_fs_file_write (f, "# %s\n%s=%s\n",
						set->desc, set->name, set->sval) == 0)
							goto error_handler;
					break;
				case E2_OPTION_TYPE_TREE:
					if (e2_fs_file_write (f, "# %s\n%s=<\n",
						set->name, set->name) == 0)
							goto error_handler;
					GtkTreeIter iter;
					if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
						e2_option_tree_write_to_file (f, set, &iter, 0);	//error handling ?
					if (e2_fs_file_write (f, ">\n") == 0)
						goto error_handler;
					break;
				default:
					printd (WARN, "don't know how to write option '%s' to config file",
						set->name);
					break;
			}
		}
		//now the unknown options, if any
		//need an error flag
		write_error = FALSE;
		g_hash_table_foreach (options_queue,
			(GHFunc) _e2_option_file_write_unknowns, f);
		if (write_error)
			goto error_handler;

		//fs rename operation may be not reliably atomic (ext4)
		gint fdesc = fileno (f);
		if (fdesc == -1)
			goto error_handler;
		if (e2_fs_writeflush (fdesc) != 0)
			goto error_handler;

		e2_fs_close_stream (f);

		printd (DEBUG, "before renaming replacement config file, suspend monitoring");
		e2_option_disable_config_checks ();	//suspend monitoring to prevent iterated reloads
#ifdef E2_VFS
		tdata.path = tempname;
		tdata.spacedata = NULL;
		ddata.path = local;
		ddata.spacedata = NULL;
#endif
		OPENBGL	//downstream errors invoke local mutex locking
#ifdef E2_VFS
		e2_task_backend_rename (&tdata, &ddata);
		e2_fs_chmod (&ddata, 0600 E2_ERR_NONE());
#else
		e2_task_backend_rename (tempname, local);
		e2_fs_chmod (local, 0600 E2_ERR_NONE());
#endif
		CLOSEBGL
		printd (DEBUG, "after renaming replacement config file, resume monitoring");
		e2_option_enable_config_checks ();	//resume monitoring

		goto cleanup;
	}
error_handler:
	if (f != NULL)
	{
		e2_fs_close_stream (f);
		OPENBGL	//downstream errors invoke local mutex locking
#ifdef E2_VFS
		e2_task_backend_delete (&tdata);
#else
		e2_task_backend_delete (tempname);
#endif
		CLOSEBGL
	}
	gchar *msg = g_strdup_printf (_("Cannot write config file %s - %s"),
		cfg_file, g_strerror (errno));	//ok for native-only config file
	e2_output_print_error (msg, TRUE);
	sleep (1);
cleanup:
	if (freepath)
		g_free (cfg_file);
	F_FREE (local, cfg_file);
	g_free (tempname);
}
/**
@brief Set value of "single-valued" option named @a option

This is called from the config file line parser.
If the set data exists already, its value is set according to @a str.
If the set data does not exist already, the set is logged in the
'unknown' queue

@param option name of option to be set
@param str string with value to be stored for option named @a option

@return TRUE if @a option was set successfully, even if in the unknown options queue
*/
gboolean e2_option_set_from_string (gchar *option, gchar *str)
{
	E2_OptionSet *set = e2_option_get_simple (option);
	if (set == NULL)
	{
		//reinstate the form of the config line
		gchar *saveme = g_strconcat (option, "=", str, NULL);
		//and log the freeable strings
		e2_option_unknown_record (g_strdup (option), saveme);
		return FALSE;
	}
	return e2_option_set_value_from_string (set, str);
}
/**
@brief Setup the default value for @a set in accord with parameter @a str

This sets the appropriate set->sval, depending on the type of option
There is no check for existence of @a set

@param set pointer to data struct for the option being processed
@param str string form of value to be stored for @a set

@return TRUE if the value was valid and stored
*/
gboolean e2_option_set_value_from_string (E2_OptionSet *set, gchar *str)
{
	gboolean retval = TRUE;
	switch (set->type)
	{
		case E2_OPTION_TYPE_BOOL:
			set->hook_freezed = TRUE;
			e2_option_bool_set_direct (set, !strcmp (str, "true") ? TRUE : FALSE);
			set->hook_freezed = FALSE;
			break;
		case E2_OPTION_TYPE_INT:
		{
			//FIXME: check for conversion success
			gint i = (gint) g_ascii_strtoull (str, NULL, 10);
			set->hook_freezed = TRUE;
			e2_option_int_set_direct (set, i);
			set->hook_freezed = FALSE;
			break;
		}
		case E2_OPTION_TYPE_STR:
		case E2_OPTION_TYPE_FONT:
			e2_option_str_set_direct (set, str);
			break;
		case E2_OPTION_TYPE_COLOR:
			return e2_option_color_set_str_direct (set, str);
			break;
		case E2_OPTION_TYPE_SEL:
		{
			const gchar *val;
			gint i = 0;
			set->ival = -1;
			while ((val = set->ex.sel.def[i]) != NULL)
			{
				if (!strcmp (str, val))
				{
					set->ival = i;
					set->sval = (gchar *) val;
					break;
				}
				i++;
			}
			if (set->ival == -1)
			{
				printd (WARN, "bad value for sel option '%s'", set->name);
				retval = FALSE;
				set->ival = 0;
				set->sval = (gchar *) set->ex.sel.def[0];
			}
//			if (!set->hook_freezed)
//				e2_hook_list_run (&set->hook_value_changed, GINT_TO_POINTER (set->ival));
		}
			break;
		default:
			break;
	}
	return retval;
}
/**
@brief Read config options from @a f and parse them

Critical characters in @a f are assumed to be ascii
This function is more-or-less replicated in the config plugin
@param f NULL-terminated array of strings in config file format

@return
*/
void e2_option_read_array (gchar *f[])
{
	gint i = -1;	//array index
	gchar *line; //pointer to the current line
	gchar **split;

	while ((line = f[++i]) != NULL)
	{
		g_strchomp (line);
		//ignore empty lines and comments
		if (*line == '\0') continue;
		if (line[0] == '#') continue;

		split = g_strsplit (line, "=", 2);
		if (split[1] != NULL)
		{
			if (strcmp (split[1], "<")) //not a tree set
			{
			 	if (!e2_option_set_from_string (split[0], split[1]))
					printd (WARN, "could not set option '%s'", split[0]);
			}
			else //a tree set
				if (!e2_option_tree_set_from_array (split[0], f, &i, NULL))
					printd (WARN, "could not set tree option '%s'", split[0]);
		}
		g_strfreev (split);
	}
}
/**
@brief read config file into memory at @a contents
It's intended that a config file be native, not virtual.
If the file is read successfully, log the post-read config dir timestamp
@param config_dir absolute path of directory containing config files, UTF-8 string
@param contents pointer to place to store the loaded file content

@return TRUE if the file was read
*/
static gboolean _e2_option_config_file_read (const gchar *config_dir, gpointer *contents)
{
	//find absolute path to config file
	gchar *cfg_file = g_build_filename (config_dir, default_config_file, NULL);
	gchar *local = F_FILENAME_TO_LOCALE (cfg_file);
#ifdef E2_VFS
	VPATH ddata = { local, NULL };	//only local config data
	if (e2_fs_access (&ddata, R_OK E2_ERR_NONE()))
#else
	if (e2_fs_access (local, R_OK E2_ERR_NONE()))
#endif
	{	//try for the old-style name
		g_free (cfg_file);
		F_FREE (local, cfg_file);
		cfg_file = g_build_filename (config_dir, "config", NULL);
		local = F_FILENAME_TO_LOCALE (cfg_file);
#ifdef E2_VFS
		ddata.path = local;
		if (e2_fs_access (&ddata, R_OK E2_ERR_NONE()))
#else
		if (e2_fs_access (local, R_OK E2_ERR_NONE()))
#endif
		{
			g_free (cfg_file);
			F_FREE (local, cfg_file);
			return FALSE;
		}
	}

	gboolean retval;
	//get file
#ifdef E2_VFS
	if (e2_fs_get_file_contents (&ddata, contents, NULL, TRUE E2_ERR_NONE()))
#else
	if (e2_fs_get_file_contents (local, contents, NULL, TRUE E2_ERR_NONE()))
#endif
	{
//#ifndef E2_FAM do this anyway ...
		//log the current timestamp
		struct stat stat_buf;
#ifdef E2_VFS
		e2_fs_stat (&ddata, &stat_buf E2_ERR_NONE());
#else
		e2_fs_stat (local, &stat_buf E2_ERR_NONE());
#endif
		if (stat_buf.st_size > 5)
		{
			app.config_mtime = stat_buf.st_mtime;
//#endif
			printd (DEBUG, "config file '%s' read", cfg_file);
			retval = TRUE;
		}
		else
		{
			printd (WARN, "bad config file '%s'", cfg_file);
			if (contents != NULL)
				g_free (*contents);
			retval = FALSE;
		}
	}
	else
	{
		printd (WARN, "could not read config file '%s'", cfg_file);
		retval = FALSE;
	}
	g_free (cfg_file);
	F_FREE (local, cfg_file);
	return retval;
}
/**
@brief read config file

First, the configuration is read, then its content is parsed with
e2_option_read_array(). If tree options have no configuration,
they are initialized with a default one.
(All other options have been intialized when they were registered.)
@param config_dir absolute path of dir containing config files, UTF-8 string,
 or NULL to use default
@return TRUE if the config file was read
*/
gboolean e2_option_file_read (const gchar *config_dir)
{
	gpointer contents;
	if (config_dir == NULL)
		config_dir = e2_cl_options.config_dir;
	if (_e2_option_config_file_read (config_dir, &contents))
	{
		gboolean retval;
		//everything goes into an array
		gchar **split = g_strsplit ((gchar *)contents, "\n", -1);
		if (split[0] != NULL)
		{	//there is something to work with, so parse it
			//get the config file version (always ascii) from 1st line of file
			//(done here to avoid problems when line(s) not from the file are processed
			if (strstr (split[0], "(v") != NULL )
			{
				gchar *sp = g_strrstr (split[0], ")");
				if (sp != NULL)
					*sp = '\0';
				sp = g_strstrip (strstr (split[0], "(v") + 2);
				g_strlcpy (app.cfgfile_version, sp, sizeof(app.cfgfile_version));
			}
			e2_option_read_array (split);
			retval = TRUE;
		}
		else
			retval = FALSE;

		g_strfreev (split);
		g_free (contents);	//need free() if file buffer allocated by malloc()
		return retval;
	}
	else
		return FALSE;
}
/**
@brief cleanup data for an item in the options hash

@set pointer to data struct to be processed

@return
*/
static void _e2_option_clean1 (E2_OptionSet *set)
{
//	printd (DEBUG, "destroying set %s", set->name);
	E2_OptionFlags flags = set->flags;
	if (flags & E2_OPTION_FLAG_FREENAME)
		g_free (set->name);
	if (flags & E2_OPTION_FLAG_FREEGROUP)
		g_free (set->group);
	if (flags & E2_OPTION_FLAG_FREEDESC)
		g_free (set->desc);
	if (flags & E2_OPTION_FLAG_FREETIP)
		g_free (set->tip);
	if (flags & E2_OPTION_FLAG_FREEDEPENDS)
		g_free (set->depends);

	E2_OptionType type = set->type;
	if (type &
		( E2_OPTION_TYPE_INT | E2_OPTION_TYPE_STR
		| E2_OPTION_TYPE_FONT | E2_OPTION_TYPE_COLOR ))
			g_free (set->sval);
	else if (type & E2_OPTION_TYPE_SEL)
		g_strfreev (set->ex.sel.def);
	else if (type & E2_OPTION_TYPE_TREE)
	{
		if (set->ex.tree.def.tree_strings != NULL)
		{
			g_strfreev (set->ex.tree.def.tree_strings);
			set->ex.tree.def.tree_strings = NULL;
		}
		GList *member;
		for (member = set->ex.tree.columns; member != NULL; member = member->next)
		{
			E2_OptionTreeColumn *opt = member->data;
			if (opt->flags & E2_OPTION_TREE_COL_FREENAME)
				g_free (opt->label);
			if (opt->flags & E2_OPTION_TREE_COL_FREESTRING)
				g_free (opt->sdef);
//			if (opt->data_cleaner != NULL)
//				(opt->data_cleaner) (opt->visible_check_data);
			DEALLOCATE (E2_OptionTreeColumn, opt);
		}
		g_object_unref (G_OBJECT (set->ex.tree.model));
	}
	//CHECKME set-related hooks are un-registered when config dialog is destroyed
	//so this may be redundant, or at least, needs to happen after that destruction
	//or we may want to run the hooks after this change
	if (set->hook_value_changed.is_setup)
		g_hook_list_clear (&set->hook_value_changed);
#ifdef USE_GLIB2_10
	g_slice_free1 (sizeof (E2_OptionSet), set);
//	DEALLOCATE (E2_OptionSet, set);
#else
	DEALLOCATE (E2_OptionSet, set);
#endif
}
/**
@brief initialize option system

internal data structures of the option system will be initialized.
this includes creation of a hash table for fast option lookup.
this function is called on startup, by the main function

@return
*/
void e2_option_init (void)
{
/* "core" options are registered before a config file (if any) is read.
For those that are non-tree, a default value is registered during the
registration process. For core tree options, a pointer to the default
initialisation function is stored, to reduce needless parsing.
When a config file is read, option values from that are installed,
replacing defaults or installing trees. Finally, missing tree values
are installed.

"Transient" (con-core) options are not registered at session-start, so
any such that's found in a config file is simply listed as is. Any later
registration of a transient option should be followed by a check of
that list, and installation of the config file values if found there,
and then, for a tree option, installation of default values if nothing
was queued. The list has strings in the same format as was in the config
file. Hence tree-options are multi-lined. Installation requires parsing.

Writing a config file finishes with the then-current contents of the
unknown-list. The listed strings do not need to be reformatted before
writing */
	const gchar *lang = g_getenv ("LC_MESSAGES");
	if (lang == NULL)
		lang = g_getenv ("LANG");
	if (lang == NULL)
		lang = g_getenv ("LC_ALL");
	if (lang == NULL)
		lang = "C";
	default_config_file = g_strconcat ("config-", lang, NULL);  //DO NOT TRANSLATE
	options_array = g_ptr_array_new ();
	//any non-constant key (option name) is cleared in _e2_option_clean1()
	options_hash = g_hash_table_new_full (g_str_hash, g_str_equal, NULL,
		(GDestroyNotify) _e2_option_clean1);
	options_queue = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
		g_free);
	//before config loaded, setup all option types, default values, tips etc
	//tree option default values are not set yet
	e2_option_default_register ();
#ifndef E2_FAM
	e2_fs_FAM_config_stamp ();	//set baseline time, for config refreshing
#endif
}
/**
@brief setup array of i18n config dialog labels

Some of these are the same as action labels
Array size defined in e2_config_dialog.h

@return
*/
void e2_option_setup_labels (void)
{
	config_labels[0]=_("aliases");
	config_labels[1]=_("bookmarks");
	config_labels[2]=_("colors");
	config_labels[3]=_("columns");
	config_labels[4]=_("command toolbar");
	config_labels[5]=_("command line");
	config_labels[6]=_("commands");
	config_labels[7]=_("confirmation");
	config_labels[8]=_("context menu");
	config_labels[9]=_("custom menus");
//	config_labels[10]=_("delays"); UNUSED
	config_labels[11]=_("dialogs");
	config_labels[12]=_("directory lines");
	config_labels[13]=_("extensions");
	config_labels[14]=_("file actions");
	config_labels[15]=_("filetypes");
	config_labels[16]=_("fonts");
	config_labels[17]=_("general");
	config_labels[18]=_("history");
	config_labels[19]=_("icons");
	config_labels[20]=_("interface");
	config_labels[21]=_("item types");
	config_labels[22]=_("key bindings");
	config_labels[23]=_("main"); //for bindings
	config_labels[24]=_("make directory");
//	config_labels[25]=_("menus"); UNUSED
	config_labels[26]=_("miscellaneous");
	config_labels[27]=_("options");
	config_labels[28]=_("output");
	config_labels[29]=_("pane 1");
	config_labels[30]=_("pane1 toolbar");
	config_labels[31]=_("pane 2");
	config_labels[32]=_("pane2 toolbar");
	config_labels[33]=_("panes");
	config_labels[34]=_("plugins");
#ifdef E2_MOUSECUSTOM
	config_labels[35]=_("pointer buttons");
#endif
	config_labels[36]=_("position");
	config_labels[37]=_("search");
	config_labels[38]=_("startup");
	config_labels[39]=_("style");
	config_labels[40]=_("tab completion");
	config_labels[41]=_("task toolbar");
	config_labels[42]=_("view");
//maybe more space here, see array definition in header file
}
