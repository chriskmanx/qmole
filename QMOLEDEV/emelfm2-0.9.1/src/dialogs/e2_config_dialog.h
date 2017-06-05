/* $Id: e2_config_dialog.h 2743 2013-09-19 22:29:00Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

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

#ifndef __E2_CONFIG_DIALOG_H__
#define __E2_CONFIG_DIALOG_H__

enum
{
	E2_CFGDLG_SINGLE=1,
	E2_CFGDLG_MAIN,
	E2_CFGDLG_BASIC,
	E2_CFGDLG_ADVANCED,
};

typedef struct _E2_ConfigDialogRuntime
{
	GtkTreeStore *store;	//page-names store
	GtkWidget *treeview;	//page-names treeview
	GtkTreePath *openpath;	//path of startup-name in page-names treeview
	gint openpage;	//index of startup notebook page
	GtkNotebook *notebook;
	GHashTable *opthash;	//sizegroups hash
	gboolean refresh_files; //value of "auto-refresh" option when dialog was started
	gboolean refresh_cfg;	//value of "auto-refresh-config" option when dialog was started
} E2_ConfigDialogRuntime;

void e2_confdlg_menu_set_position (GtkMenu *menu,
	gint *x, gint *y, gboolean *push_in, GtkWidget *treeview);
gboolean e2_confdlg_key_press_cb (GtkWidget *widget,
	GdkEventKey *event, gpointer data);
void e2_config_dialog_create (gchar *page);
void e2_config_dialog_setup_labels (void);
void e2_config_dialog_actions_register (void);

void e2_confdlg_choose_plugins_cb (GtkButton *button, E2_OptionSet *set);
#ifdef E2_RAINBOW
void e2_confdlg_extcolorpick_cb (GtkButton *button, E2_OptionSet *set);
#endif

// things for single-page config dialogs

typedef struct _E2_SpecificConfDialogRuntime
{
	GtkWidget *dialog;
	E2_OptionSet *set;
	void (*apply_function)(void);
} E2_SpecificConfDialogRuntime;

E2_SpecificConfDialogRuntime *e2_config_dialog_single (gchar *set_name,
	void (*apply_function)(void), gboolean showit) G_GNUC_MALLOC;

#endif //ndef __E2_CONFIG_DIALOG_H__
