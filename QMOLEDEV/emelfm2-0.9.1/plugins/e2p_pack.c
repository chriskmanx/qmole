/* $Id: e2p_pack.c 2978 2013-11-30 02:56:32Z tpgww $

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
@file plugins/e2p_pack.c
@brief plugin for interfacting with several archive managers, to pack selected item(s)
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_command.h"
#include "e2_task.h"
#include "e2_filelist.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "pack"

//same enum as in unpack plugin, though it need not be so
enum { TAR_GZ, TAR_BZ2, TAR_LZMA, TAR_XZ, TAR, /*DEB, RPM, */ZIP, Z7Z, RAR, ARJ, ZOO, MAXTYPES };

typedef struct _E2_PackDlgRunTime
{
	GtkWidget *dialog;
	GtkWidget *filename_entry;
	GtkWidget *pkgtype_combo;
	gchar *curr_dir;	//utf8 form of qed->currdir (with trailer)
} E2_PackDlgRunTime;

static PluginIface iface;

static gint pkg_type = TAR_GZ;	//in theory, we should protect this by mutex

//these extension strings are in same order as enum
static gchar *ext_str [MAXTYPES] =
{
	".tar.gz",
	".tar.bz2",
	".tar.lzma",
	".tar.xz",
	".tar",
//	".deb",
//	".rpm",
	".zip",
	".7z",
	".rar",
	".arj",
	".zoo"
};

static gboolean _e2p_packQ (E2_ActionTaskData *qed);

/**
@brief implement user's choice to pack the archive
BGL on
*/
static void _e2p_pack_yes (E2_PackDlgRunTime *rt)
{
	static gchar *cmd_str [MAXTYPES] =
	{	//these command strings are in same order as enum
		//NOTE: %%f is converted to %f when the command is constructed with g_strdup_printf ()
		">tar cvf - %%f | gzip - > %s",	//run in separate shell
		">tar cvf - %%f | bzip2 - > %s",//ditto default block size -9 is not significantly better, maybe use -3 instead ?
		">tar cvf - %%f | lzma - > %s", //ditto default compresssion -7, maybe use -2 instead ?
		">tar cvf - %%f | xz - > %s",
		"tar cvf %s %%f",
//		FIXME deb pack command
//		FIXME rpm "pack" command does not exist. rpmbuild ?
//		"zip -r %s %%f", adds to any existing archive
		">zip -r - %%f > %s",

		"7za a -t7z %s %%f", //adds to any existing archive
//		">7za a -t7z %%f > %s", BAD

		"rar a -r -ol -tl %s %%f",

//		"arj a -a1 -r %s %%f",
		"arj a -a -r -s -2s %s %%f", //CHECKME

		"zoo ahP %s %%f"
	};
/*
compress ANSI files: 7za a -tzip archive.zip file1 file2 ... fileN
compress ANSI dir:   7za a -tzip archive.zip dirnametocompress\
compress UNICODE files: 7za a -t7z archive.7z file1 file2 ... fileN
compress UNICODE dir:   7za a -t7z archive.7z dirnametocompress\
decompress ANSI:    7za x archive.zip -odirname -aoa
decompress UNICODE: 7za x archive.7z -odirname -aoa
*/
	pkg_type = gtk_combo_box_get_active (GTK_COMBO_BOX (rt->pkgtype_combo));
	if (pkg_type != -1)
	{
		gchar *full_name;
		const gchar *chosen_name = gtk_entry_get_text (GTK_ENTRY (rt->filename_entry));
		gboolean flag = (*chosen_name != '\0');
		if (flag)
		{
			full_name = g_strconcat (chosen_name, ext_str [pkg_type], NULL);

			if (e2_option_bool_get ("confirm-overwrite"))
			{
#ifdef E2_VFSTMP
				//FIXME dir when not mounted local
#else
				gchar *utf = g_strconcat (rt->curr_dir, full_name, NULL);  //separator comes with dir
#endif
				gchar *dlocal = F_FILENAME_TO_LOCALE (utf);
#ifdef E2_VFS
				VPATH ddata = { dlocal, NULL };	//running a command is only local
				if (e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
				if (e2_fs_access2 (dlocal E2_ERR_NONE()) == 0)
#endif
				{	//same-named item exists already
					//FIXME some apps allow addition to existing archive
					OPENBGL
					DialogButtons result = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
						&ddata,
#else
						dlocal,
#endif
						NONE);
					CLOSEBGL
					if (result != OK)
					{
						flag = FALSE;	//signal that we will not proceed
						g_free (full_name);
					}
				}
				g_free (utf);
				F_FREE (dlocal, utf);
			}
		}

		if (flag)
		{
//tag E2_BADQUOTES
			gchar *qp = e2_utils_quote_string (full_name);
			gchar *command = g_strdup_printf (cmd_str [pkg_type], qp);
			g_free (qp);
//			e2_filelist_disable_one_refresh (PANEACTIVE);	//prevent changes to selected item data
//			gint res =
			e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, rt->dialog
#ifdef E2_COMMANDQ
			, FALSE
#endif
			);
//			e2_filelist_enable_one_refresh (PANEACTIVE); //probably nothing reported yet, to trigger a refresh
//			flag = (res == 0);
			g_free (full_name);
			g_free (command);
		}
	}
	else
		pkg_type = TAR_GZ;
}
/**
@brief callback for dialog's "response" signal

@param dialog the dialog where the response was triggered
@param response the response for the clicked button
@param rt pointer to data for dialog

@return
*/
static void _e2p_pack_response_cb (GtkDialog *dialog, gint response,
	E2_PackDlgRunTime *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_APPLY:
			gtk_widget_hide (rt->dialog);
			_e2p_pack_yes (rt);
			break;
		default:
			break;
	}
	gtk_widget_destroy (rt->dialog);
	NEEDOPENBGL
	g_free (rt->curr_dir);
	DEALLOCATE (E2_PackDlgRunTime, rt);
}
/**
@brief handle activation (<Return> keypresses) in the query entry

@param entry UNUSED the entry widget for the combo box
@param rt pointer to dialog data struct
@return
*/
static void _e2p_pack_activated_cb (GtkEntry *entry, E2_PackDlgRunTime *rt)
{
//	NEEDOPENBGL
	_e2p_pack_response_cb (GTK_DIALOG (rt->dialog), E2_RESPONSE_APPLY, rt);
//	NEEDCLOSEBGL
}
/**
@brief create and run dialog

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_pack (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_PACK, art, from, _e2p_packQ, NULL));
}
static gboolean _e2p_packQ (E2_ActionTaskData *qed)
{
	E2_PackDlgRunTime *rt = ALLOCATE (E2_PackDlgRunTime);
	CHECKALLOCATEDWARNT (rt, return FALSE;)

	CLOSEBGL
	rt->dialog = e2_dialog_create (NULL, _("Filename:"), _("archive creation"),
		(ResponseFunc)_e2p_pack_response_cb, rt);

	GtkWidget *hbox = g_object_get_data (G_OBJECT (rt->dialog), "e2-dialog-hbox");

	GPtrArray *names = qed->names;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	gchar *suggested_name = F_FILENAME_FROM_LOCALE ((*iterator)->filename);

	rt->filename_entry = e2_widget_add_entry (hbox, suggested_name, TRUE, TRUE);
	F_FREE (suggested_name, (*iterator)->filename);
#ifdef E2_ASSISTED
	GtkWidget *label = (GtkWidget *) g_object_get_data (G_OBJECT (rt->dialog),
		"e2-dialog-label");
	e2_widget_set_label_relations (label, rt->filename_entry);
#endif
	g_signal_connect (G_OBJECT (rt->filename_entry), "activate",
		G_CALLBACK (_e2p_pack_activated_cb), rt);

	rt->pkgtype_combo = e2_combobox_add (hbox, FALSE, 0, NULL, NULL, NULL,
		E2_COMBOBOX_MENU_STYLE);
	//these applied package-extension strings are in same order as enum
	e2_combobox_append_history_counted (rt->pkgtype_combo, MAXTYPES, ext_str);

	gtk_combo_box_set_active (GTK_COMBO_BOX (rt->pkgtype_combo), pkg_type);

	rt->curr_dir = D_FILENAME_FROM_LOCALE (qed->currdir);

	E2_Button local_btn;
	e2_button_derive (&local_btn, &E2_BUTTON_APPLY, BTN_YES_CONTINUE);

	e2_dialog_show (rt->dialog, app.main_window, E2_DIALOG_CLOSELOCK,
		&E2_BUTTON_CANCEL, &local_btn, NULL);

	OPENBGL
	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(6),_("pack"),_e2p_pack,
		_("_Pack.."),
		_("Build an archive containing the selected items"),
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
