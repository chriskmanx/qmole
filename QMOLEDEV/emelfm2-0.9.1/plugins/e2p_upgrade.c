/* $Id: e2p_upgrade.c 3047 2014-02-08 21:56:04Z tpgww $

Copyright (C) 2005-2013 tooar <tooar@emelfm2.net>

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
@file plugins/e2p_upgrade.c
@brief plugin for updating config files when a new emelFM2 version so requires

This file contains functions that help upgrading the default configuration
data file to the current version. Note that upgrading may also be needed for
imported config data - refer to the config plugin
*/

#include "emelfm2.h"
#include <string.h>
#include <glib/gi18n.h>
#include "e2p_upgrade.h"
#include "e2_option.h"
#include "e2_output.h"
#include "e2_dialog.h"
#include "e2_task.h"
#include "e2_icons.h"
#include "e2_complete.h"

//signature component, must match 'core' of this file name
#define ANAME "upgrade"

UpgradeIface iface;

static gchar *default_msg =
	N_("Configuration arrangements for this version %s of %s are considerably "
	"different from those of old versions. To reliably ensure access to the "
	"program's current features, it is best to start with fresh settings.\n"
	"If you proceed, the superseded configuration files in\n %s will have '.save' "
	"appended to their names.\nFeel free to delete them."
	);
#if 0
static gchar *option_msg =
	N _ ("Several default configuration settings of this version %s of %s"
	" are different from those of recent versions (see changelog).\n"
	"If you click OK, those settings will be updated where possible.\n"
	"Or else you can Cancel, and later, via the configuration dialog, manually"
	"change individual settings, or change all settings to current defaults."
	);
#endif //0

static void _e2p_upgrade_reload (gboolean read)
{
	guint i;
	gpointer *walker;
	//prevent attempts to clean non-existent backup data (dunno how the pointers get bad)
	for (i = 0, walker = options_array->pdata; i < options_array->len; i++, walker++)
	{
		E2_OptionSet *set;
		set = *walker;
		if (set->type == E2_OPTION_TYPE_TREE)
			set->ex.tree.def.tree_strings = NULL;
	}
	e2_option_clear_data ();	//clear current option values
	e2_option_default_register ();//install defaults
	e2_option_date_style (); //customise date-format option
	if (read)
		e2_option_file_read (NULL);
}

//assumes BGL exists and is closed, native config-file, returns FALSE if failed
static gboolean _e2p_upgrade_backup (const gchar *localcfg)
{
#ifdef E2_VFS
	VPATH sdata = { localcfg, NULL };
	if (e2_fs_access (&sdata, R_OK E2_ERR_NONE()) == 0)	//traverse link, if any
#else
	if (e2_fs_access (localcfg, R_OK E2_ERR_NONE()) == 0)	//traverse link, if any
#endif
	{
		gchar *saved_file = g_strconcat (localcfg, ".save", NULL);
		OPENBGL	//downstream errors invoke local mutex locking
#ifdef E2_VFS
		VPATH ddata = { saved_file, NULL };
		gboolean success = e2_task_backend_copy (&sdata, &ddata, E2_FTM_BACKUP);
#else
		gboolean success = e2_task_backend_copy (localcfg, saved_file, E2_FTM_BACKUP);
#endif
		CLOSEBGL
		g_free (saved_file);
		return success;
	}
	return FALSE;
}

//expects BGL closed, or running in main thread, returns gtk enum GTK_RESPONSE_YES etc
static gint _e2p_upgrade_dialog (const gchar *msg)
{
	//main button structs not created yet
	E2_Button yes_btn =
		{ _("_Apply"),
#ifdef E2_ICONCACHE
	//FIXME gtk images not yet available
		NULL,
#else
		STOCK_NAME_YES,
#endif
		NULL, E2_BTN_DEFAULT, E2_BTN_DEFAULT, GTK_RESPONSE_YES };
	E2_Button no_btn =
		{ _("_Cancel"),
#ifdef E2_ICONCACHE
	//FIXME gtk images not yet available
		NULL,
#else
		STOCK_NAME_NO,
#endif
		NULL, E2_BTN_DEFAULT, E2_BTN_DEFAULT, GTK_RESPONSE_NO };

	GtkWidget *dialog = e2_dialog_create (
#ifdef E2_ICONCACHE
	//FIXME gtk images not yet available
		NULL,
#else
		STOCK_NAME_DIALOG_INFO,
#endif
		msg, _("update information"), DEFAULT_RESPONSE_CB, NULL);
	e2_dialog_show (dialog, NULL, 0, &yes_btn, &no_btn, NULL);
	//can't run the dialog in a local loop, as main-window (for fake event) & options not yet available
	gint choice = gtk_dialog_run (GTK_DIALOG (dialog));
	gtk_widget_destroy (dialog);
	return choice;
}

static gboolean _e2p_upgrade_version_numbers (const gchar *localcfg, const gchar *sed)
{
	if (sed != NULL)
	{
		gchar *command = g_strconcat (sed,
			" -i"
			" -e '1s/",app.cfgfile_version,"/"VERSION RELEASE"/'"
			" -e '2,$s/0\\.[0-9]\\.[0-9]/"VERSION"/'"
			" ",localcfg,NULL);
		gboolean success = (system (command) == 0);
		if (!success)
			printd (WARN, "failed to execute command to upgrade config version no's");
		g_free (command);
		return success;
	}
	return FALSE;
}

#ifdef E2_POLKIT
/**
@brief upgrade su-related strings
@return TRUE if the upgrade was done
*/
static gboolean _e2p_upgrade_su (const gchar *localcfg, const gchar *sed)
{
	if (sed != NULL)
	{
		const gchar *prompt1 = _("Enter command:");
		const gchar *prompt2 = _("Done. Press enter ");
		//commandbar item
		gchar *oldstr1 = g_strconcat("\\|xterm\\|-e 'su -c \\\"%\\{\\(root-commands\\)@",prompt1,"\\}\\\";echo -n \\\"",prompt2,"\\\";read'",NULL);
		gchar *newstr1 = g_strconcat("|pkexec|%{(root-commands)@",prompt1,"}",NULL);
		//alias
		//2nd (.*) around "\2" so this can work (and be inserted in the replacement string)
		gchar *oldstr2 = g_strconcat("\\|xterm -e sh -c 'su -c \\\"(.*)\\\";echo -n \\\"",prompt2,"\\\";read'",NULL);
		const gchar *newstr2 = "|pkexec \\2";

		gchar *command = g_strconcat (sed,
			" -i"
			" -e \"s~\\(.*\\)",oldstr1,"~\\1",newstr1,"~\""
			" -e \"s~\\(.*\\)",oldstr2,"~\\1",newstr2,"~\""
			" ",localcfg,NULL);

		gboolean success = (system (command) == 0);
		if (!success)
			printd (WARN, "failed to execute command to do su-upgrade");
		g_free (oldstr1);
		g_free (newstr1);
		g_free (oldstr2);
		g_free (command);
		return success;
	}
	return FALSE;
}
#endif

//returns TRUE if old config data are replaced by current defaults
static gboolean _e2p_upgrade_too_old (const gchar *localcfg)
{
	gchar *msg = g_strdup_printf (_(default_msg), VERSION, PROGNAME,
		e2_cl_options.config_dir);
	gint choice = _e2p_upgrade_dialog (msg);
	g_free (msg);

	if (choice == GTK_RESPONSE_YES)
	{
		_e2p_upgrade_backup (localcfg);
		e2_option_clear_data ();
		e2_option_default_register ();
		e2_option_date_style (); //customise date-format option
		return TRUE;
	}
	return FALSE;
}

static void _e2p_upgrade_0_4_1 (void)
{
	//get rid of outdated cache flags for find plugin, which hasn't been loaded yet
	e2_cache_clean1 ("find-plugin-flags");
}

//assumes config-file backup already done, returns TRUE if defaults are applied
static gboolean _e2p_upgrade_0_4_5 (void)
{	//one dialog only !
	gchar *msg = g_strdup_printf (_(default_msg), VERSION, PROGNAME,
		e2_cl_options.config_dir);
	gint choice = _e2p_upgrade_dialog (msg);
	g_free (msg);

	if (choice == GTK_RESPONSE_YES)
	{
		_e2p_upgrade_reload (FALSE); //no re-read
		e2_option_file_write (NULL); //save updated config file
		return TRUE;
	}
	return FALSE;
}

static gboolean _e2p_upgrade_0_5_1 (const gchar *localcfg, const gchar *sed)
{
	if (sed != NULL)
	{
		//keybinding to add
		gchar *oldstr1 = g_strconcat("\t\t\t|<Control>i|false|",_A(7),".",_A(60),"|",NULL);
		gchar *newstr1 = g_strconcat("\t\t\t|<Control>d|false|",_A(7),".",_A(83),"|",NULL);

		gchar *command = g_strconcat (sed,
			" -i"
			" -e '/",oldstr1,"$/a\\\n",newstr1,"'"
			" ",localcfg,NULL);

		gboolean success = (system (command) == 0);
		if (!success)
			printd (WARN, "failed to execute command to do upgrade 0.5.1");
		g_free (oldstr1);
		g_free (newstr1);
		g_free (command);
		return success;
	}
	else
		return _e2p_upgrade_0_4_5 ();	//suggest default
}

static gboolean _e2p_upgrade_0_5_1_1 (const gchar *localcfg, const gchar *sed)
{
	if (sed != NULL)
	{
		//keybindings to change
		gchar *oldstr1 = g_strconcat(_A(10),".",_A(33),"|*,1",NULL);
		gchar *newstr1 = g_strconcat("!",_A(10),".",_A(33),"|1,*",NULL);
		gchar *oldstr2 = g_strconcat(_A(10),".",_A(33),"|*,0",NULL);
		gchar *newstr2 = g_strconcat("!",_A(10),".",_A(33),"|0,*",NULL);
		gchar *oldstr3 = g_strconcat(_A(14),".",_A(33),"|*,1",NULL);
		gchar *newstr3 = g_strconcat("!",_A(14),".",_A(33),"|0,*",NULL);
		gchar *oldstr4 = g_strconcat(_A(14),".",_A(33),"|*,0",NULL);
		gchar *newstr4 = g_strconcat("!",_A(14),".",_A(33),"|1,*",NULL);

		gchar *command = g_strconcat (sed,
			" -i"
			" -e 's/",oldstr1,"/",newstr1,"/'"
			" -e 's/",oldstr2,"/",newstr2,"/'"
			" -e 's/",oldstr3,"/",newstr3,"/'"
			" -e 's/",oldstr4,"/",newstr4,"/'"
			" ",localcfg,NULL);

		gboolean success = (system (command) == 0);
		if (!success)
			printd (WARN, "failed to execute command to do upgrade 0.5.1.1");
		g_free (oldstr1);
		g_free (newstr1);
		g_free (oldstr2);
		g_free (newstr2);
		g_free (oldstr3);
		g_free (newstr3);
		g_free (oldstr4);
		g_free (newstr4);
		g_free (command);
		return success;
	}
	else
		return _e2p_upgrade_0_4_5 ();	//suggest default
}

static void _e2p_upgrade_0_6_0 (void)
{
	//update key-shortcut translations
	E2_OptionSet *set = e2_option_get ("keybindings");
	if (set->ex.tree.synced) //this set's data processed already
	{
		GtkTreeIter iter;
		if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
			e2_keybinding_localise (set->ex.tree.model, &iter);
	}
	//else FIXME
}

static gboolean _e2p_upgrade_0_7_2 (const gchar *localcfg, const gchar *sed)
{
	if (sed != NULL)
	{
		//context menu item add
		gchar *oldstr1 = g_strconcat("\t",_("_Edit bookmarks.."),"|gtk-preferences|false|false|",_A(3),".",_C(1),"|",NULL);
		gchar *newstr1 = g_strconcat(_("_History"),"|gtk-jump-to|false|false|",_A(8),".",_A(28),"|",NULL);

		gchar *command = g_strconcat (sed,
			" - i"
			" -e '/",oldstr1,"$/a\\\n",newstr1,"'"
			" ",localcfg,NULL);

		gboolean success = (system (command) == 0);
		if (!success)
			printd (WARN, "failed to execute command to do upgrade 0.7.2");
		g_free (oldstr1);
		g_free (newstr1);
		g_free (command);
		return success;
	}
	else
		return _e2p_upgrade_0_4_5 ();	//suggest default
}

static gboolean _e2p_upgrade_0_8_2 (const gchar *localcfg, const gchar *sed)
{
	if (sed != NULL)
	{
		//context menu item add
		gchar *oldstr1 = g_strconcat("||false|false|",_A(6),".",_A(23),"|",NULL);
		gchar *newstr1 = g_strconcat("||false|false|",_A(6),".",_A(26),"|",NULL);
		gchar *command = g_strconcat (sed,
			" -i"
			" -e '/",oldstr1,"$/a\\\n",newstr1,"'"
			" ",localcfg,NULL);

		gboolean success = (system (command) == 0);
		if (!success)
			printd (WARN, "failed to execute command to do upgrade 0.7.2");
		g_free (oldstr1);
		g_free (newstr1);
		g_free (command);
		return success;
	}
	else
		return _e2p_upgrade_0_4_5 ();	//suggest default
}

static gboolean _e2p_upgrade_0_9_0 (const gchar *localcfg, const gchar *sed)
{
	if (sed != NULL)
	{
		//change & add keybindings
		//oldstr1 is duplicated, only the first needs fix, see restriction in sed command
		gchar *oldstr1 = g_strconcat("\t\t\t|<Control>Delete|false|",_A(1),".",_A(36),"|",NULL);
		//NOTE escaped \n to get a valid newline
		gchar *newstr1 = g_strconcat("\t\t\t|<Control>Delete|false|",_A(5),".",_A(36),"|\\n",
			"\t\t\t|<Shift>Insert|false|",_A(5),".",_A(32),"|",NULL);
		gchar *oldstr2 = g_strconcat("\t\t\t|<Control>k|false|",_A(5),".",_A(37),"|",NULL);
		gchar *newstr2 = g_strconcat("\t\t\t|<Alt>Delete|false|",_A(5),".",_A(44),"|",NULL);
		//see comment above re oldstr1, applies to oldstr3 too
		gchar *oldstr3 = g_strconcat("\t\t\t|<Alt>Delete|false|",_A(1),".",_A(37),"|",NULL);
		gchar *newstr3 = g_strconcat("\t\t\t|<Shift><Alt>Delete|false|",_A(5),".",_A(37),"|",NULL);
		gchar *oldstr4 = g_strconcat("\t\t\t|<Alt>Delete|false|",_A(1),".",_A(37),"|",NULL);
		gchar *newstr4 = g_strconcat("\t\t\t|<Alt>Delete|false|",_A(1),".",_A(44),"|\\n",
			"\t\t\t|<Shift><Alt>Delete|false|",_A(1),".",_A(37),"|",NULL);
#ifdef E2_MOUSECUSTOM
		//change buttonbindings
		gchar *oldstr5 = g_strconcat("\t\t\t|<Control>2|false|false|",_A(13),".",_A(54),"|",NULL);
		gchar *newstr5 = g_strconcat("\t\t\t|<Control>2|false|false|",_A(13),".",_A(67),"|",NULL);
		gchar *oldstr6 = g_strconcat("\t\t\t|<Alt>2|false|false|",_A(13),".",_A(67),"|",NULL);
		gchar *newstr6 = g_strconcat("\t\t\t|<Alt>2|false|false|",_A(13),".",_A(54),"|",NULL);
#endif
		gchar *command = g_strconcat (sed,
			" -i"
			" -e '0,/",oldstr1,"/s/",oldstr1,"/",newstr1,"/'"
			" -e 's/",oldstr2,"/",newstr2,"/'"
			" -e '0,/",oldstr3,"/s/",oldstr3,"/",newstr3,"/'"
			" -e 's/",oldstr4,"/",newstr4,"/'"
#ifdef E2_MOUSECUSTOM
			" -e 's/",oldstr5,"/",newstr5,"/'"
			" -e 's/",oldstr6,"/",newstr6,"/'"
#endif
			" ",localcfg,NULL);

		gboolean success = (system (command) == 0);
		if (!success)
			printd (WARN, "failed to execute command to do upgrade 0.9.0");
		g_free (oldstr1);
		g_free (newstr1);
		g_free (oldstr2);
		g_free (newstr2);
		g_free (oldstr3);
		g_free (newstr3);
		g_free (oldstr4);
		g_free (newstr4);
#ifdef E2_MOUSECUSTOM
		g_free (oldstr5);
		g_free (newstr5);
		g_free (oldstr6);
		g_free (newstr6);
#endif
		g_free (command);
		return success;
	}
	else
		return _e2p_upgrade_0_4_5 ();	//suggest default
}

static gboolean _e2p_upgrade_0_9_1 (const gchar *localcfg, const gchar *sed)
{
	if (sed != NULL)
	{
		//change plugins
		gchar *oldstr1 = "e2p_for_each\\(.*\\)|[a-z_0-9.]*$";
		gchar *newstr1 = "e2p_foreach\\1|foreach";

		gchar *oldstr2 = "e2p_mvbar\\(.*\\)|[a-z0-9.]*$";
		gchar *newstr2 = "e2p_move\\1|move";

		gchar *oldstr3 = "e2p_names_clip\\(.*\\)|[a-z_0-9.]*$";
		gchar *newstr3 = "e2p_clipnames\\1|clipnames";

		gchar *oldstr4 = "e2p_sort_by_ext\\(.*\\)|[a-z_0-9.]*$";
		gchar *newstr4 = "e2p_extsort\\1|extsort";

		gchar *oldstr5 = "e2p_thumbs\\(.*\\)|[a-z0-9.]*$";
		gchar *newstr5 = "e2p_thumbnail\\1|thumbnail";

		gchar *oldstr6 = "e2p_times\\(.*\\)|[a-z0-9.]*$";
		gchar *newstr6 = "e2p_timeset\\1|timeset";

		gchar *oldstr10 = "^\\t\\(.*\\)|||1-cpbar";
		gchar *newstr10 = "\\1|e2p_copy.so||2\\@copy";
		gchar *oldstr11 = "^\\t\\(.*\\)||\\(.*\\)|||0-cpbar";
		gchar *newstr11 = "\\1|plugin_copy_48.png|\\2|e2p_copy.so||1\\@copy";

		gchar *oldstr12 = g_strconcat("^\\t\\(.*\\)",_("_Partial"),"\\(.*\\)|||1-selmatch", NULL);
		gchar *newstr12 = g_strconcat("\\1",_("Select _like"),"\\2|e2p_selmatch.so||2\\@selmatch", NULL);
		gchar *oldstr13 = g_strconcat("^\\t\\(.*\\)",_("_Whole"),"||\\(.*\\)|||0-selmatch", NULL);
		gchar *newstr13 = g_strconcat("\\1",_("_Select same"),"|plugin_selmatch_48.png|\\2|e2p_selmatch.so||1\\@selmatch", NULL);

		gchar *oldstr14 = "^\\t\\(.*\\)|||1-tag";
		gchar *newstr14 = "\\1|e2p_tag.so||2\\@tag";
		gchar *oldstr15 = "^\\t\\(.*\\)||\\(.*\\)|||0-tag";
		gchar *newstr15 = "\\1|plugin_tag_48.png|\\2|e2p_tag.so||1\\@tag";

		gchar *oldstr16 = "^\\t\\(.*\\)|||1-acl";
		gchar *newstr16 = "\\1|e2p_acl.so||2\\@acl";
		gchar *oldstr17 = "^\\t\\(.*\\)||\\(.*\\)|||0-acl";
		gchar *newstr17 = "\\1|plugin_acl_48.png|\\2|e2p_acl.so||1\\@acl";
		
		gchar *oldstr18 = "^\\(.*\\)0\\.[5-9]\\.[0-6]$";
		gchar *newstr18 = "\\1";

		gchar *command = g_strconcat (sed,
			" -i"
			" -e '/e2p_acl/d'"
			" -e '/e2p_cpbar/d'"
			" -e '/e2p_selmatch/d'"
			" -e '/e2p_tag/d'",
			" -e 's/",oldstr1,"/",newstr1,"/'"
			" -e 's/",oldstr2,"/",newstr2,"/'"
			" -e 's/",oldstr3,"/",newstr3,"/'"
			" -e 's/",oldstr4,"/",newstr4,"/'"
			" -e 's/",oldstr5,"/",newstr5,"/'"
			" -e 's/",oldstr6,"/",newstr6,"/'"
			" -e 's/",oldstr10,"/",newstr10,"/'"
			" -e 's/",oldstr11,"/",newstr11,"/'"
			" -e 's/",oldstr12,"/",newstr12,"/'"
			" -e 's/",oldstr13,"/",newstr13,"/'"
			" -e 's/",oldstr14,"/",newstr14,"/'"
			" -e 's/",oldstr15,"/",newstr15,"/'"
			" -e 's/",oldstr16,"/",newstr16,"/'"
			" -e 's/",oldstr17,"/",newstr17,"/'"
			" -e 's/",oldstr18,"/",newstr18,"/'"
			" ",localcfg,NULL);

		gboolean success = (system (command) == 0);
		if (!success)
			printd (WARN, "failed to execute command to do upgrade 0.9.1");
		g_free (oldstr12);
		g_free (newstr12);
		g_free (oldstr13);
		g_free (newstr13);
		g_free (command);
		return success;
	}
	else
		return _e2p_upgrade_0_4_5 ();	//suggest default
}

/**
@brief interface func to serially perform all relevant updates to config file
 contents and already-cached config data

Some choices made by the user may result in exit() calls

@return TRUE upon successful completion
*/
static gboolean _e2p_upgrade_all (void)
{
	gboolean success = TRUE;
	gboolean saved = FALSE;

	gchar *cfg_file = g_build_filename (e2_cl_options.config_dir,
			default_config_file, NULL);
	gchar *localcfg = F_FILENAME_TO_LOCALE (cfg_file);

	if (strcmp (app.cfgfile_version, OLDEST_UPGRADE) < 0)
	{
		success = _e2p_upgrade_too_old (localcfg); //offer to start afresh
		F_FREE (localcfg, cfg_file);
		g_free (cfg_file);
		if (success)
			return TRUE;
		else
			exit (1);
	}

	if (strcmp (app.cfgfile_version,"0.5.0") < 0)
	{
		_e2p_upgrade_backup (localcfg);
		saved = TRUE;

		if (!_e2p_upgrade_0_4_5 ())	//suggest default
			exit(1);
		_e2p_upgrade_0_4_1 ();	//change the find-plugin ABI
	}
	else
	{
		gchar *sed = g_find_program_in_path ("sed");
		if (sed == NULL)
		{
			//FIXME should always warn the user about this
			printd (ERROR, "can't find 'sed' so i can't upgrade the config file");
		}

#ifdef E2_POLKIT
		//this define can be changed for any build
		if (!saved)
		{
			_e2p_upgrade_backup (localcfg);
			saved = TRUE;
		}
		if (success && !_e2p_upgrade_su (localcfg, sed))
			success = FALSE;
#endif
		if (success && strcmp (app.cfgfile_version,"0.5.1") < 0)
		{
			if (!saved)
			{
				_e2p_upgrade_backup (localcfg);
				saved = TRUE;
			}
			if (!_e2p_upgrade_0_5_1 (localcfg, sed))
				success = FALSE;
		}
		if (success && strcmp (app.cfgfile_version,"0.5.1.1") < 0)
		{
			if (!saved)
			{
				_e2p_upgrade_backup (localcfg);
				saved = TRUE;
			}
			if (!_e2p_upgrade_0_5_1_1 (localcfg, sed))
				success = FALSE;
		}
		if (success && strcmp (app.cfgfile_version,"0.7.2") < 0)
		{
			if (!saved)
			{
				_e2p_upgrade_backup (localcfg);
				saved = TRUE;
			}
			if (!_e2p_upgrade_0_7_2 (localcfg, sed))
				success = FALSE;
		}
		if (success && strcmp (app.cfgfile_version,"0.8.2") < 0)
		{
			if (!saved)
			{
				_e2p_upgrade_backup (localcfg);
				saved = TRUE;
			}
			if (!_e2p_upgrade_0_8_2 (localcfg, sed))
				success = FALSE;
		}
		if (success && strcmp (app.cfgfile_version,"0.9.0") < 0)
		{
			if (!saved)
			{
				_e2p_upgrade_backup (localcfg);
				saved = TRUE;
			}
			if (!_e2p_upgrade_0_9_0 (localcfg, sed))
				success = FALSE;
		}
		if (success && strcmp (app.cfgfile_version,"0.9.0.1") < 0)
		{
			if (!saved)
			{
				_e2p_upgrade_backup (localcfg);
				saved = TRUE;
			}
			if (strcmp (app.cfgfile_version,"0.9.0") == 0)
			{
				if (!_e2p_upgrade_0_9_0 (localcfg, sed)) //this was mistakenly disabled in the 0.9.0 release
					success = FALSE;
			}
			if (success && !_e2p_upgrade_0_9_1 (localcfg, sed))
				success = FALSE;
		}

		//update config file version, docs version etc (no prior file-backup)
		if (success && !_e2p_upgrade_version_numbers (localcfg, sed))
			success = FALSE;

		g_free (sed);
	}

	if (success)
		_e2p_upgrade_reload (TRUE);	//implement all updates

	F_FREE (localcfg, cfg_file);
	g_free (cfg_file);

	//after any config-file-based updates, work on [newly-]stored data ...

	if (strcmp (app.cfgfile_version,"0.6.0") < 0)
		_e2p_upgrade_0_6_0 ();

	return success;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1
*/
Plugin *init_plugin (E2PInit mode)
{
	Plugin *p = (Plugin*)&iface;
	p->signature = ANAME VERSION;
	p->flags = E2P_NOCFG | E2P_OFFMENU;
//	p->cleaner = clean_plugin;

	if (mode & E2P_SETUP)
	{
		p->refcount = 1;
		iface.update_config = _e2p_upgrade_all;
	}
	return p;
}

gboolean clean_plugin (Plugin *p)
{
	return TRUE;
}
