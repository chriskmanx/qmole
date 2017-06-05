/* $Id: e2p_config.c 3047 2014-02-08 21:56:04Z tpgww $

Copyright (C) 2006-2013 tooar <tooar@emelfm2.net>

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
@file plugins/e2p_config.c
@brief emelfm2 config-data export/import plugin
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_task.h"
#include "e2_filetype.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif
#include "e2_icons.h"
#include "e2p_upgrade.h" //get OLDEST_UPGRADE

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "config"

#define OLDEST_IMPORT "0.4.0"

typedef struct _E2P_ConfigData
{
	GtkWidget *dialog;
	GtkWidget *save_entry;
	GtkWidget *open_entry;
	GtkWidget *expander;
	GtkWidget *icondir_entry;
	GtkWidget *iconsavedir_entry;
} E2P_ConfigData;

typedef enum
{
	ALL_P = 0,
	NONTREE_P, ALLTREE_P, CUSTOM_P,
	MARKS_P, FILETYPES_P,
	ALIASES_P, KEYS_P,
#ifdef E2_MOUSECUSTOM
	BUTTONS_P, GESTURES_P,
#endif
	PLUGINS_P, MENU_P, CUSTMENU_P,
	PANEBAR1_P, PANEBAR2_P, TASKBAR_P, CMDBAR_P,
	MAX_CONFFLAGS	//no. of entries in the array
} flag_t;

typedef enum
{
	E2PC_ALL       = 1 << ALL_P,
	E2PC_NONTREE   = 1 << NONTREE_P,
	E2PC_ALLTREE   = 1 << ALLTREE_P,
	E2PC_CUSTOM    = 1 << CUSTOM_P,
	E2PC_MARKS     = 1 << MARKS_P,
	E2PC_FILETYPES = 1 << FILETYPES_P,
	E2PC_ALIASES   = 1 << ALIASES_P,
	E2PC_KEYS      = 1 << KEYS_P,
#ifdef E2_MOUSECUSTOM
	E2PC_BUTTONS   = 1 << BUTTONS_P,
	E2PC_GESTURES  = 1 << GESTURES_P,
#endif
	E2PC_PLUGINS   = 1 << PLUGINS_P,
	E2PC_MENU      = 1 << MENU_P,
	E2PC_CUSTMENU  = 1 << CUSTMENU_P,
	E2PC_PANEBAR1  = 1 << PANEBAR1_P,
	E2PC_PANEBAR2  = 1 << PANEBAR2_P,
	E2PC_TASKBAR   = 1 << TASKBAR_P,
	E2PC_CMDBAR    = 1 << CMDBAR_P,
} bitflag_t;

#ifdef E2_MOUSECUSTOM
#define E2PC_ALLTREEMASK E2PC_FILETYPES | E2PC_ALIASES | E2PC_KEYS \
	| E2PC_BUTTONS | E2PC_GESTURES \
	| E2PC_PLUGINS | E2PC_MENU | E2PC_CUSTMENU | E2PC_PANEBAR1 \
	| E2PC_PANEBAR2 | E2PC_TASKBAR | E2PC_CMDBAR
#else
#define E2PC_ALLTREEMASK E2PC_FILETYPES | E2PC_ALIASES | E2PC_KEYS \
	| E2PC_PLUGINS | E2PC_MENU | E2PC_CUSTMENU | E2PC_PANEBAR1 \
	| E2PC_PANEBAR2 | E2PC_TASKBAR | E2PC_CMDBAR
#endif
//for determining index of some specific tree
#define FIRSTTREE_P MARKS_P

static PluginIface iface;

//these treeset "internal" names are in same order as the flag_t enumerator
static gchar *set_private_names [] =
{
	"bookmarks",
	"filetypes",
	"command-aliases",
	"keybindings",
#ifdef E2_MOUSECUSTOM
	"mousebuttons",
	"mousedrags",
#endif
	"plugins",
	"context-menu",
	"custom-menus",
	"panebar1",
	"panebar2",
	"taskbar",
	"commandbar",
};

//cache for toggle values, static for the session
static gboolean flags[MAX_CONFFLAGS];
static GPtrArray *treeset_names = NULL;	//names to use when checking importable tree options
static gboolean rebuild_needed;
static E2P_ConfigData *srt;	//copy of runtime ptr for func(s) that don't get rt directly

  /*********************/
 /***** utilities *****/
/*********************/

/**
@brief get mnemonic'ed label for set corresponding to @a f

@param f enumerated value of option to be processed

@return newly-allocated utf8 label
*/
static gchar *_e2pc_get_setlabel (flag_t f)
{
	//no label-mnemonic that competes with the close button
	//nicer to do this lookup just once !
	gunichar close_mnemonic[2] = {0};
	close_mnemonic[0] = e2_utils_get_mnemonic_char (E2_BUTTON_CLOSE.label);

	gchar *private_name = set_private_names [f - FIRSTTREE_P];
	E2_OptionSet *set = e2_option_get (private_name);
	//quick, flawed approach to underscoring for keyboard speedup
	gchar *label;
	if (close_mnemonic[0] != (gunichar)'\0' &&
		g_str_has_prefix (set->desc, (gchar *) close_mnemonic))
		label = g_strdup (set->desc);
	else
		label = g_strconcat ("_", set->desc, NULL);
	return label;
}
/**
@brief set specified flag to T/F

The relevant array value is set

@param f enumerated value of flag to be set
@param value new value for the flag

@return
*/
static void _e2pc_set_flag (flag_t f, gboolean value)
{
	if (f < MAX_CONFFLAGS)
		flags[ (gint) f] = value;
}
/**
@brief return the value of a specified flag

@param f enumerated value of flag to be interrogated
@param rt UNUSED pointer to dialog data struct

@return flag value, T/F, or FALSE if the value is not recognised
*/
static gboolean _e2pc_get_flag (flag_t f)
{
	if (f < MAX_CONFFLAGS)
		return (flags[(gint) f]);
	else
		return (FALSE);
}
/**
@brief toggle specified option flag

@param widget clicked button, UNUSED
@param flagnum pointerized number of the flag to be toggled

@return
*/
static void _e2pc_toggle_cb (GtkWidget *widget, gpointer flagnum)
{
	flag_t flg = (flag_t) flagnum;
	gboolean newflag = ! _e2pc_get_flag (flg);
	_e2pc_set_flag (flg, newflag);
	//handle here all the 'special cases', if any
/*	if (newflag)
	{
	  switch (GPOINTER_TO_INT(flagnum))
	  {
		  default:
			break;
	  }
	} */
	if (flg == CUSTOM_P)
	{
		NEEDCLOSEBGL
		gtk_expander_set_expanded (GTK_EXPANDER (srt->expander), newflag);
		NEEDOPENBGL
	}
}
/**
@brief create and show a button in a specified container

@param container the widget into which the button is to be placed
@param f enumerated value of flag to be associated with the button
@param state T/F initial state of the toggle
@param label  translated string for the button label
@param callback "toggled" signal callback function to be connected

@return the button widget
*/
/*static GtkWidget *_e2pc_create_option_button (GtkWidget *container,
	flag_t f, gboolean state, gchar *label,
	void (*callback)(GtkToggleButton*,gpointer))
{
	GtkWidget *btn = gtk_check_button_new_with_mnemonic (label);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (btn), state);
	g_signal_connect (G_OBJECT (btn), "toggled", G_CALLBACK (callback),
		(gpointer) f);  //CHECKME = correct value?
	gtk_container_add (GTK_CONTAINER (container), btn);
	return btn;
} */
/**
@brief create and show a check button in a specified container

@param container the widget into which the button is to be placed
@param f enumerated value of flag to be associated with the button
@param state T/F initial state of the toggle
@param label  translated string for the button label

@return the button widget (UNUSED, now)
*/
static GtkWidget *_e2pc_create_check_button (GtkWidget *container,
	flag_t f, gboolean state, gchar *label)
{
	GtkWidget *btn = gtk_check_button_new_with_mnemonic (label);
	g_signal_connect (G_OBJECT (btn), "toggled", G_CALLBACK (_e2pc_toggle_cb),
		(gpointer) f);  //CHECKME = correct value?
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (btn), state);
	gtk_container_add (GTK_CONTAINER (container), btn);
	return btn;
}
/**
@brief create and show a radio btn in a specified container
The leader of a group is initialized to TRUE, other group members
may cause that to be changed
@param container the widget into which the button is to be placed
@param f enumerated value of flag to be associated with the button
@param label  translated string for the button label
@param rt

@return the button widget
*/
static GtkWidget *_e2pc_create_radio_button (GtkWidget *container,
	flag_t f, gchar *label)
{
	_e2pc_set_flag (f, TRUE);
	//group leader needs to be TRUE at start, may be toggled by other members
	GtkWidget *btn = gtk_radio_button_new_with_mnemonic (NULL, label);
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (btn), TRUE);
	g_signal_connect (G_OBJECT (btn), "toggled", G_CALLBACK (_e2pc_toggle_cb),
		(gpointer) f);
	gtk_container_add (GTK_CONTAINER (container), btn);
	return btn;
}
/**
@brief create and show a radio btn in a specified container

@param container the widget into which the button is to be placed
@param group the radio button widget that 'leads' the group
@param f enumerated value of flag to be associated with the button
@param state T/F initial state of the toggle
@param label  translated string for the button label
@param rt

@return the button widget
*/
static GtkWidget *_e2pc_create_radio_grouped_button (GtkWidget *container, GtkWidget *group,
	flag_t f, gboolean state, gchar *label)
{
  GSList *list = gtk_radio_button_get_group (GTK_RADIO_BUTTON (group));
  GtkWidget *btn = gtk_radio_button_new_with_mnemonic (list, label);
  g_signal_connect (G_OBJECT (btn), "toggled", G_CALLBACK (_e2pc_toggle_cb),
		(gpointer) f);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (btn), state);
  gtk_container_add (GTK_CONTAINER (container), btn);
  return btn;
}
/**
@brief check whether a tree option is to be imported

@param setname internal name of set read from config file
@return TRUE if this set is one we want
*/
static gboolean _e2pc_match_tree (gchar *setname)
{
	guint i;
	gchar **iterator;
	for (i = 0, iterator = (gchar **)treeset_names->pdata;
			i < treeset_names->len;
			i++, iterator++)
	{
		if (!strcmp (*iterator, setname))
		{
			g_ptr_array_remove_index_fast (treeset_names, i);
			return TRUE;
		}
	}
	return FALSE;
}
/**
@brief apply requested config data
No backup of existing data, so mid-read failure might be ornery!
@param contents ptr to config file after loading into memory
@param flags bitflags indicating which options to import
@return
*/
static void _e2pc_filter_options (gchar *contents, bitflag_t flags)
{
	gint i = -1;	//array index
	gchar *line; //pointer to the current line
	gchar **lsplit;
	//everything goes into an array
	//as for e2_option_read_array(), contents are assumed to be parsable as ascii
	gchar **split = g_strsplit (contents, "\n", -1);

	while ((line = split[++i]) != NULL)
	{
		g_strchomp (line);
		//ignore empty lines and comments
		if (*line == '\0' || line[0] == '#')
			continue;

		lsplit = g_strsplit (line, "=", 2);
		if (lsplit[1] != NULL)
		{
			if (strcmp (lsplit[1], "<")) //not a tree set
			{
				if (flags & (E2PC_NONTREE | E2PC_ALL))
				{

					if (e2_option_set_from_string (lsplit[0], lsplit[1]))
						rebuild_needed = TRUE;
					else
						printd (WARN, "could not set option '%s'", lsplit[0]);
				}
			}
			else //a tree set
			{
				if ((flags & (E2PC_ALLTREE | E2PC_ALL)) || _e2pc_match_tree (lsplit[0]))
				{
					E2_OptionSet *set = e2_option_tree_get (lsplit[0]);
					if (set != NULL)
					{
						e2_option_tree_backup (set);
						gtk_tree_store_clear (GTK_TREE_STORE (set->ex.tree.model));
						if (e2_option_tree_set_from_array (lsplit[0], split, &i, NULL))
						{
							rebuild_needed = TRUE;
							e2_option_tree_unbackup (set, FALSE);
						}
						else
						{
							//reinstate backup
							e2_option_tree_unbackup (set, TRUE);
							set = NULL;	//trigger message
						}
					}
					if (set == NULL)
					{
						gchar *msg = g_strdup_printf (
						_("Bad configuration data for %s, not installed"), lsplit[0]);
						e2_output_print_error (msg, TRUE);
					}
				}
				else	//skip the set
					while ((line = split[++i]) != NULL)
				{
					g_strchomp (line);
					if (line[0] == '>')
						break;
				}
			}
		}
		g_strfreev (lsplit);
	}
	g_strfreev (split);
}
  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief get config data and apply them

@param button UNUSED the clicked widget
@param rt dialog runtime struct

@return
*/
static void _e2pc_import_cb (GtkButton *button, E2P_ConfigData *rt)
{
	//convert import flags to bitflags that we can play with
	bitflag_t import_flags = 0;
	gint i;
	for (i = 0; i < MAX_CONFFLAGS; i++)
	{
		if (flags[i])
			import_flags |= 1 << i;
	}
	import_flags &= ~E2PC_CUSTOM;	//this one is just an indicator for specific tree(s)
	if (!import_flags)
		return;	//nothing chosen
	//don't want to separately test the "all" flags
	if (import_flags & E2PC_ALL)
		import_flags |= E2PC_NONTREE;
	if (import_flags & (E2PC_ALL | E2PC_ALLTREE))
		import_flags |= E2PC_ALLTREEMASK;
	//setup tree-option private (untranslated) names, for comparison when parsing
	treeset_names = g_ptr_array_sized_new (MAX_CONFFLAGS);	//plenty of space, even with trailing NULL
	//FIXME make this dynamic
	if (import_flags & E2PC_MARKS)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[MARKS_P - FIRSTTREE_P]);
	if (import_flags & E2PC_FILETYPES)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[FILETYPES_P - FIRSTTREE_P]);
	if (import_flags & E2PC_ALIASES)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[ALIASES_P - FIRSTTREE_P]);
	if (import_flags & E2PC_KEYS)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[KEYS_P - FIRSTTREE_P]);
#ifdef E2_MOUSECUSTOM
	if (import_flags & E2PC_BUTTONS)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[BUTTONS_P - FIRSTTREE_P]);
	if (import_flags & E2PC_GESTURES)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[GESTURES_P - FIRSTTREE_P]);
#endif
	if (import_flags & E2PC_PLUGINS)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[PLUGINS_P - FIRSTTREE_P]);
	if (import_flags & E2PC_MENU)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[MENU_P - FIRSTTREE_P]);
	if (import_flags & E2PC_CUSTMENU)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[CUSTMENU_P - FIRSTTREE_P]);
	if (import_flags & E2PC_PANEBAR1)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[PANEBAR1_P - FIRSTTREE_P]);
	if (import_flags & E2PC_PANEBAR2)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[PANEBAR2_P - FIRSTTREE_P]);
	if (import_flags & E2PC_TASKBAR)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[TASKBAR_P - FIRSTTREE_P]);
	if (import_flags & E2PC_CMDBAR)
		g_ptr_array_add (treeset_names, (gpointer) set_private_names[CMDBAR_P - FIRSTTREE_P]);

	gboolean upgrade = FALSE;	//TRUE when importing an old config file that probably needs upgrade
	NEEDCLOSEBGL
	const gchar *filepath = gtk_entry_get_text (GTK_ENTRY (rt->open_entry));
	gchar *realpath = (*filepath != '\0') ? (gchar *) filepath :
		g_build_filename (e2_cl_options.config_dir, default_config_file, NULL);
	gchar *localpath = F_FILENAME_TO_LOCALE (realpath);
#ifdef E2_VFS
	VPATH ddata = { localpath, NULL };	//only allow local config data
#endif
	gpointer contents;
	//get file
//read_file:
#ifdef E2_VFS
	if (e2_fs_get_file_contents (&ddata, &contents, NULL, TRUE E2_ERR_NONE()))
#else
	if (e2_fs_get_file_contents (localpath, &contents, NULL, TRUE E2_ERR_NONE()))
#endif
	{
		//check for conforming version, hence format for tree options
		gchar *sp, *sq, *st;
		sp = strchr ((gchar *)contents, '\n');
		if (sp != NULL)
		{
			*sp = '\0';
			sq = strstr ((gchar *)contents, "(v");
			if (sq != NULL)
			{
				st = g_strrstr (sq, ")");
				if (st != NULL)
				{
					*st = '\0';
					sq = g_strdup (sq + 2);
					g_strstrip (sq);
					upgrade = (strcmp (sq, VERSION RELEASE) < 0);
					if (upgrade)
					{
						if (strcmp (sq, OLDEST_IMPORT) >= 0)
						{
							//do any interim content-transforms here
/*							gchar *command;
							gchar *sed = g_find_program_in_path ("sed");
							if (sed != NULL)
							{
								if (strcmp (sq,"0.1.6.3") < 0 && (import_flags & (E2PC_ALLTREEMASK)))
								{
									//re-configure all tree-option name lines
									//this format change is no longer supported in the upgrade plugin
									command = g_strconcat (
									"cp -f ", realpath, " ", realpath, ".save;",
									sed, " -e 's/^<\\(.*\\)/\\1=</'",
									 " -e 's/^\\(\t*\\)[\\]</\\1</' ",
									realpath, ".save >", realpath, NULL);
									system (command);
									g_free (command);
									g_free (sed);
									g_free (sq);
									if (realpath != filepath)
										g_free (realpath);
									g_free (contents);	//need free() if file buffer allocated by malloc()
									goto read_file;
								}
							}
*/
						}
						else
						{
							gchar *msg = g_strdup_printf (_("Incompatible format - %s"), realpath);
							e2_output_print_error (msg, TRUE);
							g_free (sq);
							if (realpath != filepath)
								g_free (realpath);
							g_free (contents);	//need free() if file buffer allocated by malloc()
							return;
						}
					}
					*st = ')';
				}
			}
			*sp = '\n';
		}
		else
			sq = NULL;

		rebuild_needed = FALSE;
		//process the file
		_e2pc_filter_options ((gchar *)contents, import_flags);
		g_free (contents);	//need free() if file buffer allocated by malloc()
		if (rebuild_needed)
		{
			if (upgrade)
			{
				//backup current config file in case the upgrade is not so good
				gchar *path1 = g_build_filename (e2_cl_options.config_dir,
					default_config_file, NULL);
				gchar *local1 = F_FILENAME_TO_LOCALE (path1);
				gchar *name2 = g_strconcat (default_config_file, "-before-import", NULL);
				gchar *path2 = g_build_filename (e2_cl_options.config_dir, name2, NULL);
				gchar *local2 = F_FILENAME_TO_LOCALE (path2);
				gchar *savepath = e2_utils_get_tempname (local2);
				OPENBGL	//downstream errors invoke local mutex locking
#ifdef E2_VFS
				VPATH ddata = { local1, NULL };	//local config files only
				VPATH tdata = { savepath, NULL };	//local config files only
				e2_task_backend_rename (&ddata, &tdata);
#else
				e2_task_backend_rename (local1, savepath);
#endif
				CLOSEBGL
				g_free (path1);
				F_FREE (local1, path1);
				g_free (name2);
				g_free (path2);
				F_FREE (local2, path2);
				g_free (savepath);
				//save updated config file
				e2_option_file_write (NULL);
				//do any upgrades needed
				E2P_InitData data;
				if (e2_plugins_open_module (PLUGINS_DIR G_DIR_SEPARATOR_S UPGRADE_PNAME, &data))	//localised path
				{
					//fake the config version for the plugin
					//NOTE this needs to be the minimum actionable version in the
					//upgrade plugin or else that will just install defaults
					gchar *sv;
					if (sq != NULL)
						sv = (strcmp (sq, OLDEST_UPGRADE) < 0) ? OLDEST_UPGRADE: sq;
					else
						sv = OLDEST_UPGRADE;
					g_strlcpy (app.cfgfile_version, sv, sizeof (app.cfgfile_version));
					//app.cfgfile_version is reverted in plugin
					Plugin *p = (*data.init) (E2P_SETUP);	//do any upgrades
					if (p->refcount == 1) //success
					{
						p->module = data.module;
						e2_plugins_unload1 (p, FALSE); //cleanup then dump
					}
					else
					{
						printd (ERROR, "Can't initialize upgrade plugin: "UPGRADE_PNAME);
						g_strlcpy (app.cfgfile_version, VERSION RELEASE, sizeof (app.cfgfile_version));
					}
				}
				else
					printd (ERROR, "Can't find upgrade plugin "UPGRADE_PNAME", so can't upgrade the imported config data");

				if (sq != NULL)
					g_free (sq);
			}

			//recreate window and runtime data structs as appropriate
			//this is like e2_option_refresh (FALSE, TRUE) but omits some fatal
			//parts of that
			e2_option_disable_config_checks ();	//block recursion

			if (import_flags & E2PC_KEYS)
				e2_keybinding_clean ();
#ifdef E2_MOUSECUSTOM
			if (import_flags & E2PC_BUTTONS)
				e2_mousebinding_clean ();
			if (import_flags & E2PC_GESTURES)
				e2_mousegesture_clean ();
#endif
			if (import_flags & E2PC_FILETYPES)
				g_hash_table_destroy (app.filetypes);

			if (import_flags & E2PC_PLUGINS)
			{
				if (app.plugins != NULL)
				{
					//take data for this plugin out of harm's way
					PluginAction *pa = iface.actsarray;
					g_ptr_array_remove_fast (app.plugacts, pa);
					g_hash_table_steal (app.plugins, ANAME VERSION); //the allocated key leaks
					//unload all plugins except this one
					e2_plugins_unload_all (TRUE);
					g_hash_table_replace (app.plugins, g_strdup(ANAME VERSION), &iface);
					g_ptr_array_add (app.plugacts, pa);
				}
			}
			//clear relevant current information
//			e2_option_clear_data ();
			//now re-create things, in the same order as at session-start
//			e2_option_default_register ();
//			e2_option_date_style (); //customise date-format option

//			if (reload)
//			{
//				printd (INFO, "reloading config due to external change");
//				e2_option_file_read (NULL);
//			}

//			e2_option_tree_install_defaults ();

			if (import_flags & E2PC_PLUGINS)
				e2_plugins_load_configured (); // load plugins (if any)

			//re-initialise things that are not done in normal 'recreate' functions
			e2_pane_create_option_data (&app.pane1);
			e2_pane_create_option_data (&app.pane2);

//			if (recreate)
				e2_window_recreate (&app.window); //this also recreates key bindings
/*			else
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
*/
			if (import_flags & E2PC_FILETYPES)
				e2_filetype_add_all ();

			e2_option_enable_config_checks ();
		}
	}
	else
	{
		gchar *msg = g_strdup_printf (_("Error reading file %s"), realpath);
		e2_output_print_error (msg, TRUE);
	}

	NEEDOPENBGL

	if (*filepath == '\0')
		g_free (realpath);
	F_FREE (localpath, realpath);
	g_ptr_array_free (treeset_names, TRUE);
}
/**
@brief get name of config file to open, via a gtkfilechooser

@param button UNUSED the clicked widget
@param rt dialog runtime struct

@return
*/
static void _e2pc_select_config_cb (GtkButton *button, E2P_ConfigData *rt)
{
	NEEDCLOSEBGL
	GtkWidget *dialog = gtk_file_chooser_dialog_new (NULL,
		GTK_WINDOW (rt->dialog), GTK_FILE_CHOOSER_ACTION_OPEN, NULL, NULL);

	e2_dialog_setup_chooser (dialog,
		_("select configuration data file"),
		gtk_entry_get_text (GTK_ENTRY (rt->open_entry)),
		GTK_FILE_CHOOSER_ACTION_OPEN,
		TRUE,	//show hidden
		FALSE,	//single-selection
		GTK_RESPONSE_OK,	//default response
		STOCK_NAME_CANCEL, GTK_RESPONSE_CANCEL,
		STOCK_NAME_OPEN, GTK_RESPONSE_OK,
		NULL);

	gint response;
	while ((response = e2_dialog_run_simple (dialog, app.main_window)) == E2_RESPONSE_USER1)
	{}

	if (response == GTK_RESPONSE_OK)
	{
		gchar *local = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
		gchar *openpath = F_FILENAME_FROM_LOCALE (local);
		gtk_entry_set_text (GTK_ENTRY (rt->open_entry), openpath);
		g_free (local);
		F_FREE (openpath, local);
	}
	gtk_widget_destroy (dialog);
	NEEDOPENBGL
}
/**
@brief save config file with specified name

@param button UNUSED the clicked widget
@param rt dialog runtime struct

@return
*/
static void _e2pc_save_cb (GtkButton *button, E2P_ConfigData *rt)
{
	NEEDCLOSEBGL
	const gchar *savepath = gtk_entry_get_text (GTK_ENTRY (rt->save_entry));
	if (*savepath != '\0')
	{
		if (e2_option_bool_get ("confirm-overwrite"))
		{
			gchar *dlocal = D_FILENAME_TO_LOCALE (savepath);
			g_strstrip (dlocal);
#ifdef E2_VFS
			VPATH ddata = { dlocal, NULL };
			if (e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
			if (e2_fs_access2 (dlocal E2_ERR_NONE()) == 0)
#endif
			{
				OPENBGL
				DialogButtons choice = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
					&ddata,
#else
					dlocal,
#endif
					NONE);
				CLOSEBGL
				if (choice != OK)
				{
					NEEDOPENBGL
					g_free (dlocal);
					return;
				}
			}
			g_free (dlocal);
		}
		e2_option_file_write (savepath);
	}
	NEEDOPENBGL
}
/**
@brief save config file with new name

@param button UNUSED the clicked widget
@param rt dialog runtime struct

@return
*/
static void _e2pc_saveas_cb (GtkButton *button, E2P_ConfigData *rt)
{
	NEEDCLOSEBGL
	GtkWidget *dialog = gtk_file_chooser_dialog_new (NULL,
		GTK_WINDOW (rt->dialog), GTK_FILE_CHOOSER_ACTION_SAVE, NULL, NULL);

	e2_dialog_setup_chooser (dialog,
		_("save configuration data file"),
		gtk_entry_get_text (GTK_ENTRY (rt->save_entry)),
		GTK_FILE_CHOOSER_ACTION_SAVE,
		FALSE,	//hide hidden
		FALSE,	//single selection
		GTK_RESPONSE_OK,	//default response
		STOCK_NAME_CANCEL, GTK_RESPONSE_CANCEL,
		STOCK_NAME_SAVE, GTK_RESPONSE_OK,
		NULL);

	gint response;
	while ((response = e2_dialog_run_simple (dialog, app.main_window)) == E2_RESPONSE_USER1)
	{}

	if (response == GTK_RESPONSE_OK)
	{
		gchar *local = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
#ifndef USE_GTK2_8
		//check for O/W if not done by the dialog itself
#ifdef E2_VFS
		VPATH ddata = { local, NULL };
#endif
		if (e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
		&& e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
		&& e2_fs_access2 (local E2_ERR_NONE()) == 0)
#endif
		{
			OPENBGL
			DialogButtons choice = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
				&ddata,
#else
				local,
#endif
				NONE);
			CLOSEBGL
			if (choice != OK)
			{
				gtk_widget_destroy (dialog);
				NEEDOPENBGL
				g_free (local);
				return;
			}
		}
#endif
		gchar *savepath = F_FILENAME_FROM_LOCALE (local);
		gtk_entry_set_text (GTK_ENTRY (rt->save_entry), savepath);
		g_free (local);
		F_FREE (savepath, local);
	}
	gtk_widget_destroy (dialog);
	NEEDOPENBGL
}
/**
@brief select directory containing custom icons to be used

@param button clicked button, UNUSED
@param rt dialog runtime struct

@return
*/
static void _e2pc_select_icondir_cb (GtkButton *button, E2P_ConfigData *rt)
{
	NEEDCLOSEBGL
	GtkWidget *dialog = gtk_file_chooser_dialog_new (NULL,
		GTK_WINDOW (rt->dialog), GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, NULL, NULL);

	e2_dialog_setup_chooser (dialog,
		_("select icons directory"),
		gtk_entry_get_text (GTK_ENTRY (rt->icondir_entry)),
		GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
		TRUE,	//show hidden
		FALSE,	//single-selection
		GTK_RESPONSE_OK,	//default response
		STOCK_NAME_CANCEL, GTK_RESPONSE_CANCEL,
		STOCK_NAME_OPEN, GTK_RESPONSE_OK,
		NULL);

	gint response;
	while ((response = e2_dialog_run_simple (dialog, app.main_window)) == E2_RESPONSE_USER1)
	{}

	if (response == GTK_RESPONSE_OK)
	{
		gchar *local = gtk_file_chooser_get_current_folder (GTK_FILE_CHOOSER (dialog));
//		if (strlen (local) > 0)
//		{
			gchar *opendir = F_FILENAME_FROM_LOCALE (local);
			gtk_entry_set_text (GTK_ENTRY (rt->icondir_entry), opendir);
			F_FREE (opendir, local);
//		}
		g_free (local);
	}
	gtk_widget_destroy (dialog);
	NEEDOPENBGL
}
/**
@brief apply icon directory

@param button clicked button, UNUSED
@param rt dialog runtime struct

@return
*/
static void _e2pc_apply_icondir_cb (GtkButton *button, E2P_ConfigData *rt)
{
	NEEDCLOSEBGL
	gchar *utfpath = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->icondir_entry)));
	if (g_str_has_suffix (utfpath, G_DIR_SEPARATOR_S))
		*(utfpath + strlen (utfpath) - sizeof(gchar)) = '\0';

	gchar *localpath = F_FILENAME_TO_LOCALE (utfpath);
	if (!strcmp (localpath, ICON_DIR))
		e2_option_bool_set ("use-icon-dir", FALSE);
	else
	{
		e2_option_bool_set ("use-icon-dir", TRUE);
		E2_OptionSet *set = e2_option_get ("icon-dir");
		e2_option_str_set_direct (set, utfpath);
		e2_toolbar_recreate_all ();
	}
	NEEDOPENBGL
	g_free (utfpath);
	F_FREE (localpath, utfpath);
}
/**
@brief select directory in which to save custom icons now in use

@param button clicked button, UNUSED
@param rt dialog runtime struct

@return
*/
static void _e2pc_select_iconsavedir_cb (GtkButton *button, E2P_ConfigData *rt)
{
	NEEDCLOSEBGL
	GtkWidget *dialog = gtk_file_chooser_dialog_new (NULL,
		GTK_WINDOW (rt->dialog), GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER, NULL, NULL);

	e2_dialog_setup_chooser (dialog,
		_("select icons directory"),
		gtk_entry_get_text (GTK_ENTRY (rt->iconsavedir_entry)),
		GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER,
		FALSE,	//hide-hidden
		FALSE,	//single-selection
		GTK_RESPONSE_OK,	//default response
		STOCK_NAME_CANCEL, GTK_RESPONSE_CANCEL,
		STOCK_NAME_SAVE, GTK_RESPONSE_OK,
		NULL);

	gint response;
	while ((response = e2_dialog_run_simple (dialog, app.main_window)) == E2_RESPONSE_USER1)
	{}

	if (response == GTK_RESPONSE_OK)
	{
		gchar *local = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
#ifndef USE_GTK2_8
		//check for O/W if not done by the dialog itself
#ifdef E2_VFS
		VPATH ddata = { local, NULL };
#endif
		if (e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
			&& e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
			&& e2_fs_access2 (local E2_ERR_NONE()) == 0)
#endif
		{
			OPENBGL
			DialogButtons choice = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
				&ddata,
#else
				local,
#endif
				NONE);
			CLOSEBGL
			if (choice != OK)
			{
				gtk_widget_destroy (dialog);
				g_free (local);
				NEEDOPENBGL
				return;
			}
		}
#endif
		gchar *openpath = F_FILENAME_FROM_LOCALE (local);
		gtk_entry_set_text (GTK_ENTRY (rt->iconsavedir_entry), openpath);
		g_free (local);
		F_FREE (openpath, local);
	}
	gtk_widget_destroy (dialog);
	NEEDOPENBGL
}
/**
@brief copy icons to specified directory
If the dir is not absolute, curr_view->dir is prepended
@param button clicked button, UNUSED
@param rt dialog runtime struct

@return
*/
static void _e2pc_apply_iconsavedir_cb (GtkButton *button, E2P_ConfigData *rt)
{
	const gchar *path;
	gchar *slocal, *dest, *dlocal;

	//can't have trailing / for copy func
	slocal = e2_icons_get_custom_path (FALSE);

	NEEDCLOSEBGL
	path = gtk_entry_get_text (GTK_ENTRY (rt->iconsavedir_entry));
	if (!g_path_is_absolute (path))
//E2_VFSTMPOK
		dest = e2_utils_dircat (curr_view, path, FALSE);
	else
		dest = g_strdup (path);
	if (g_str_has_suffix (dest, G_DIR_SEPARATOR_S))
		*(dest + strlen (dest) - sizeof (gchar)) = '\0';
	dlocal = F_FILENAME_TO_LOCALE (dest);

	DialogButtons result;
#ifdef E2_VFS
	VPATH ddata = { dlocal, NULL };
#endif
	if (e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
		&& e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
		&& e2_fs_access2 (dlocal E2_ERR_NONE()) == 0)
#endif
	{
#ifdef E2_VFS
		VPATH sdata = { slocal, NULL };
#endif
		OPENBGL
		result = e2_dialog_ow_check (
#ifdef E2_VFS
			&sdata, &ddata,
#else
			slocal, dlocal,
#endif
			NONE);
		CLOSEBGL
	}
	else
		result = OK;

	if (result == OK)
	{
#ifdef E2_VFS
		VPATH sdata = { slocal, NULL };
#endif
		OPENBGL	//downstream errors invoke local mutex locking
#ifdef E2_VFS
		e2_task_backend_copy (&sdata, &ddata, E2_FTM_NORMAL);
#else
		e2_task_backend_copy (slocal, dlocal, E2_FTM_NORMAL);
#endif
		CLOSEBGL
	}

	NEEDOPENBGL
	g_free (slocal);
	g_free (dest);
	F_FREE (dlocal, dest);
}

  /***************************/
 /***** widget creation *****/
/***************************/

/**
@brief  create and show notebook page with config export widgets

@param notebook the notebook widget
@param rt ptr to dialog data struct

@return
*/
static void	_e2pc_make_export_tab (GtkWidget *notebook, E2P_ConfigData *rt)
{
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
#endif
#ifdef E2_ASSISTED
	GtkWidget *label =
#endif
	e2_widget_add_label (vbox, _("Save configuration data in"),
		0.5, 0.5, FALSE, E2_PADDING);

	const gchar *savedir;
	gchar *local = F_FILENAME_TO_LOCALE (e2_cl_options.config_dir);
	//default export to config dir if it's usable
#ifdef E2_VFS
	VPATH ddata = { local, NULL };
	if (e2_fs_is_dir3 (&ddata E2_ERR_NONE()) && !e2_fs_access (&ddata, R_OK | W_OK | X_OK E2_ERR_NONE()))
#else
	if (e2_fs_is_dir3 (local E2_ERR_NONE()) && !e2_fs_access (local, R_OK | W_OK | X_OK E2_ERR_NONE()))
#endif
		savedir = e2_cl_options.config_dir;
	else
		savedir = g_get_home_dir ();
	F_FREE (local, e2_cl_options.config_dir);

	gchar *savepath = g_build_filename (savedir, default_config_file, NULL);
	local = F_FILENAME_TO_LOCALE (savepath);
	gchar *temppath, *tempext, *local2;
	guint i = 0;
	while (TRUE)
	{
		tempext = g_strdup_printf (".%s~%d", _("backup"), i);
		local2 = F_FILENAME_TO_LOCALE (tempext);
		temppath = e2_utils_strcat (local, local2);
		g_free (tempext);
		F_FREE (local2, tempext);
		E2_ERR_DECLARE
#ifdef E2_VFS
		ddata.path = temppath;
		if (e2_fs_access2 (&ddata E2_ERR_PTR())	//checkme need for vfs ?
#else
		if (e2_fs_access2 (temppath E2_ERR_PTR())	//checkme need for vfs ?
#endif
				&& E2_ERR_IS (ENOENT))
		{
			E2_ERR_CLEAR
			break;
		}
		E2_ERR_CLEAR
		g_free (temppath);
		i++;
	}
	g_free (savepath);
	F_FREE (local, savepath);

	savepath = F_FILENAME_FROM_LOCALE (temppath);

	rt->save_entry = e2_widget_add_entry (vbox, (gchar *) savepath, TRUE, TRUE);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label, rt->save_entry);
#endif
	gtk_widget_set_size_request (rt->save_entry, 400, -1);

	g_free (temppath);
	F_FREE (savepath, temppath);

#ifdef USE_GTK3_2
	GtkWidget *hbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
#else
	GtkWidget *hbox = gtk_hbutton_box_new ();
#endif
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, E2_PADDING);

	gtk_box_set_spacing (GTK_BOX (hbox), E2_PADDING_LARGE);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (hbox), GTK_BUTTONBOX_END);
	GtkWidget *btn = e2_button_get (_("Se_lect"), STOCK_NAME_SAVE_AS,
		_("Select the file in which to store the config data"), _e2pc_saveas_cb, rt);
	gtk_container_add (GTK_CONTAINER (hbox), btn);
	btn = e2_button_get (_("_Save"), STOCK_NAME_SAVE,
		_("Save current config data in the specified file"), _e2pc_save_cb, rt);
	gtk_container_add (GTK_CONTAINER (hbox), btn);

#ifndef E2_ASSISTED
	GtkWidget *
#endif
	label = gtk_label_new (_("export"));
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
/**
@brief  create and show notebook page with config import widgets

@param notebook the notebook widget
@param rt ptr to dialog data struct

@return
*/
static void	_e2pc_make_import_tab (GtkWidget *notebook, E2P_ConfigData *rt)
{
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
#endif
#ifdef E2_ASSISTED
	GtkWidget *label =
#endif
	e2_widget_add_label (vbox, _("Get configuration data from"),
		0.5, 0.5, FALSE, E2_PADDING);

	const gchar *openpath;
	gchar *local = F_FILENAME_TO_LOCALE (e2_cl_options.config_dir);
	//default export to config dir if it's usable
#ifdef E2_VFS
	VPATH ddata = { local, NULL };
	if (e2_fs_is_dir3 (&ddata E2_ERR_NONE()) && !e2_fs_access (&ddata, R_OK | X_OK E2_ERR_NONE()))
#else
	if (e2_fs_is_dir3 (local E2_ERR_NONE()) && !e2_fs_access (local, R_OK | X_OK E2_ERR_NONE()))
#endif
		openpath = e2_cl_options.config_dir;
	else
		openpath = g_get_home_dir ();
	F_FREE (local, e2_cl_options.config_dir);

	gchar *openname = g_build_filename (openpath, default_config_file, NULL);

	rt->open_entry = e2_widget_add_entry (vbox, (gchar *) openname, TRUE, TRUE);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label, rt->open_entry);
#endif
	gtk_widget_set_size_request (rt->open_entry, 400, -1);

#ifdef USE_GTK3_2
	GtkWidget *hbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
#else
	GtkWidget *hbox = gtk_hbutton_box_new ();
#endif
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, E2_PADDING);

	gtk_box_set_spacing (GTK_BOX (hbox), E2_PADDING_LARGE);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (hbox), GTK_BUTTONBOX_END);
	GtkWidget *btn = e2_button_get (_("Se_lect"), STOCK_NAME_OPEN,
		_("Select the config file from which to get the data"),
		_e2pc_select_config_cb, rt);
	gtk_box_pack_start (GTK_BOX (hbox), btn, FALSE, FALSE, 0);
	btn = e2_button_get (_("_Apply"), STOCK_NAME_APPLY,
		_("Import config data in accord with choices below"), _e2pc_import_cb, rt);
	gtk_box_pack_start (GTK_BOX (hbox), btn, FALSE, FALSE, 0);

	e2_widget_add_separator (vbox, FALSE, E2_PADDING_SMALL);

	//now the import options
	hbox = e2_widget_add_box (vbox, TRUE, E2_PADDING_SMALL,
		FALSE, TRUE, E2_PADDING);
	GtkWidget *leader =
	_e2pc_create_radio_button (hbox, ALL_P, _("_all options"));
	_e2pc_create_radio_grouped_button (hbox, leader, NONTREE_P,
		FALSE, _("all '_non-group' options"));
	hbox = e2_widget_add_box (vbox, TRUE, E2_PADDING_SMALL,
		FALSE, TRUE, E2_PADDING);
	_e2pc_create_radio_grouped_button (hbox, leader, ALLTREE_P,
		FALSE, _("all 'g_roup' options"));
	_e2pc_create_radio_grouped_button (hbox, leader, CUSTOM_P,
		FALSE, _("_specific group option(s)"));
	rt->expander = gtk_expander_new_with_mnemonic (_("_groups"));
	gtk_box_pack_start (GTK_BOX (vbox), rt->expander, FALSE, FALSE, 0);
#ifdef USE_GTK3_0
	GtkWidget *expvbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	GtkWidget *expvbox = gtk_vbox_new (FALSE, 0);
#endif
	gtk_container_add (GTK_CONTAINER (rt->expander), expvbox);
	hbox = e2_widget_add_box (expvbox, TRUE, E2_PADDING_SMALL,
		FALSE, TRUE, E2_PADDING);
	gchar *labeltxt = _e2pc_get_setlabel (PANEBAR1_P);
	_e2pc_create_check_button (hbox, PANEBAR1_P, FALSE, labeltxt);
	g_free (labeltxt);
	labeltxt = _e2pc_get_setlabel (PANEBAR2_P);
	_e2pc_create_check_button (hbox, PANEBAR2_P, FALSE, labeltxt);
	g_free (labeltxt);
	hbox = e2_widget_add_box (expvbox, TRUE, E2_PADDING_SMALL,
		FALSE, TRUE, E2_PADDING);
	labeltxt = _e2pc_get_setlabel (TASKBAR_P);
	_e2pc_create_check_button (hbox, TASKBAR_P, FALSE, labeltxt);
	g_free (labeltxt);
	labeltxt = _e2pc_get_setlabel (CMDBAR_P);
	_e2pc_create_check_button (hbox, CMDBAR_P, FALSE, labeltxt);
	g_free (labeltxt);
	hbox = e2_widget_add_box (expvbox, TRUE, E2_PADDING_SMALL,
		FALSE, TRUE, E2_PADDING);
	labeltxt =  _e2pc_get_setlabel (MARKS_P);
	_e2pc_create_check_button (hbox, MARKS_P, FALSE, labeltxt);
	g_free (labeltxt);
	labeltxt = _e2pc_get_setlabel (FILETYPES_P);
	_e2pc_create_check_button (hbox, FILETYPES_P, FALSE, labeltxt);
	g_free (labeltxt);
	hbox = e2_widget_add_box (expvbox, TRUE, E2_PADDING_SMALL,
		FALSE, TRUE, E2_PADDING);
	labeltxt = _e2pc_get_setlabel (ALIASES_P);
	_e2pc_create_check_button (hbox, ALIASES_P, FALSE, labeltxt);
	g_free (labeltxt);
	labeltxt = _e2pc_get_setlabel (KEYS_P);
	_e2pc_create_check_button (hbox, KEYS_P, FALSE, labeltxt);
	g_free (labeltxt);
#ifdef E2_MOUSECUSTOM
	hbox = e2_widget_add_box (expvbox, TRUE, E2_PADDING_SMALL,
		FALSE, TRUE, E2_PADDING);
	labeltxt = _e2pc_get_setlabel (BUTTONS_P);
	_e2pc_create_check_button (hbox, BUTTONS_P, FALSE, labeltxt);
	g_free (labeltxt);
	labeltxt = _e2pc_get_setlabel (GESTURES_P);
	_e2pc_create_check_button (hbox, GESTURES_P, FALSE, labeltxt);
	g_free (labeltxt);
#endif
	hbox = e2_widget_add_box (expvbox, TRUE, E2_PADDING_SMALL,
		FALSE, TRUE, E2_PADDING);
	labeltxt = _e2pc_get_setlabel (MENU_P);
	_e2pc_create_check_button (hbox, MENU_P, FALSE, labeltxt);
	g_free (labeltxt);
	labeltxt = _e2pc_get_setlabel (PLUGINS_P);
	_e2pc_create_check_button (hbox, PLUGINS_P, FALSE, labeltxt);
	g_free (labeltxt);
	hbox = e2_widget_add_box (expvbox, TRUE, E2_PADDING_SMALL,
		FALSE, TRUE, E2_PADDING);
	labeltxt = _e2pc_get_setlabel (CUSTMENU_P);
	_e2pc_create_check_button (hbox, CUSTMENU_P, FALSE, labeltxt);
	g_free (labeltxt);

#ifndef E2_ASSISTED
	GtkWidget *
#endif
	label = gtk_label_new (_("import"));
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
/**
@brief  create and show notebook page with config import widgets

@param notebook the notebook widget
@param rt ptr to dialog data struct

@return
*/
static void	_e2pc_make_icons_tab (GtkWidget *notebook, E2P_ConfigData *rt)
{
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
#endif
#ifdef E2_ASSISTED
	GtkWidget *label =
#endif
	e2_widget_add_label (vbox, _("Use icons in"),
		0.5, 0.5, FALSE, E2_PADDING);

	gchar *openpath;
	gchar *localpath = e2_icons_get_custom_path (FALSE);
	//default icons in config dir if that's usable
#ifdef E2_VFS
	VPATH ddata = { localpath, NULL };
	if (!e2_fs_is_dir3 (&ddata E2_ERR_NONE()) || e2_fs_access (&ddata, R_OK | X_OK E2_ERR_NONE()))
#else
	if (!e2_fs_is_dir3 (localpath E2_ERR_NONE()) || e2_fs_access (localpath, R_OK | X_OK E2_ERR_NONE()))
#endif
		openpath = g_build_filename (e2_cl_options.config_dir, _("icons"), NULL);
	else
		openpath = D_FILENAME_FROM_LOCALE (localpath);
	g_free (localpath);

	rt->icondir_entry = e2_widget_add_entry (vbox, openpath, TRUE, TRUE);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label, rt->icondir_entry);
#endif
	g_free (openpath);
	gtk_widget_set_size_request (rt->icondir_entry, 400, -1);

#ifdef USE_GTK3_2
	GtkWidget *hbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
#else
	GtkWidget *hbox = gtk_hbutton_box_new ();
#endif
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, E2_PADDING);

	gtk_box_set_spacing (GTK_BOX (hbox), E2_PADDING_LARGE);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (hbox), GTK_BUTTONBOX_END);
	GtkWidget *btn = e2_button_get (_("Se_lect"), STOCK_NAME_OPEN,
		_("Select the directory where the icons are"),
		_e2pc_select_icondir_cb, rt);
	gtk_box_pack_start (GTK_BOX (hbox), btn, FALSE, FALSE, 0);
	btn = e2_button_get (_("_Apply"), STOCK_NAME_APPLY,
		_("Apply the chosen icon directory"), _e2pc_apply_icondir_cb, rt);
	gtk_box_pack_start (GTK_BOX (hbox), btn, FALSE, FALSE, 0);

	e2_widget_add_separator (vbox, FALSE, E2_PADDING_SMALL);
	//now the exporting
#ifdef E2_ASSISTED
	label =
#endif
	e2_widget_add_label (vbox, _("Copy current icons to"),
		0.5, 0.5, FALSE, E2_PADDING);

	gchar *basename = e2_utils_strcat (BINNAME"-", _("icons"));
	openpath = g_build_filename (e2_cl_options.config_dir, basename, NULL);
	g_free (basename);
	rt->iconsavedir_entry = e2_widget_add_entry (vbox, openpath, TRUE, TRUE);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label, rt->iconsavedir_entry);
#endif
	g_free (openpath);
	gtk_widget_set_size_request (rt->iconsavedir_entry, 400, -1);

#ifdef USE_GTK3_2
	hbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
#else
	hbox = gtk_hbutton_box_new ();
#endif
	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, E2_PADDING);

	gtk_box_set_spacing (GTK_BOX (hbox), E2_PADDING_LARGE);
	gtk_button_box_set_layout (GTK_BUTTON_BOX (hbox), GTK_BUTTONBOX_END);
	btn = e2_button_get (_("Se_lect"), STOCK_NAME_OPEN,
		_("Select the directory where the icons will be saved"),
		_e2pc_select_iconsavedir_cb, rt);
	gtk_box_pack_start (GTK_BOX (hbox), btn, FALSE, FALSE, 0);
	btn = e2_button_get (_("C_opy"), STOCK_NAME_COPY,
		_("Copy the icons to the chosen directory"), _e2pc_apply_iconsavedir_cb, rt);
	gtk_box_pack_start (GTK_BOX (hbox), btn, FALSE, FALSE, 0);

	GtkWidget *pagelabel = gtk_label_new (_("icons"));
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, pagelabel);
}
/**
@brief establish and show the dialog

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2p_config_dialog_create (gpointer from, E2_ActionRuntime *art)
{
	if (srt == NULL)
	{
		E2P_ConfigData rt;

		srt = &rt; //toggle-callback needs extra data

		rt.dialog = e2_dialog_create (NULL, NULL, _("manage configuration data"),
			DUMMY_RESPONSE_CB, NULL);
		GtkWidget *vbox =
#ifdef USE_GTK2_14
			gtk_dialog_get_content_area (GTK_DIALOG (rt.dialog));
#else
			GTK_DIALOG (rt.dialog)->vbox;
#endif
		//populate it with widgets
		GtkWidget *notebook = e2_widget_add_notebook
			(vbox, TRUE, 0, NULL, NULL);
		_e2pc_make_export_tab (notebook, srt);	//page 0
		_e2pc_make_import_tab (notebook, srt);	//page 1
		_e2pc_make_icons_tab (notebook, srt);	//page 2

		e2_dialog_set_negative_response (rt.dialog, GTK_RESPONSE_CLOSE);
		//block until user selects
		e2_dialog_show (rt.dialog, app.main_window,
			E2_DIALOG_BLOCKED /*| E2_DIALOG_DONT_SHOW_ALL*/ | E2_DIALOG_FREE,
			&E2_BUTTON_CLOSE, NULL);	//FIXME close btn is not suitable here

		srt = NULL;
	}
	else
		gtk_window_present (GTK_WINDOW (srt->dialog));

	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(3),_("manage"),_e2p_config_dialog_create,
		_("_Configure.."),
		_("Export or import configuration data"),
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
