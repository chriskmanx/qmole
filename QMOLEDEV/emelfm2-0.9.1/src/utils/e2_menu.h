/* $Id: e2_menu.h 2801 2013-10-12 08:08:31Z tpgww $

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

/**
@file src/utils/e2_menu.h
@brief Menu utilitiy functions header.

This is the header file for the menu utility functions.
*/

#ifndef __E2_MENU_H__
#define __E2_MENU_H__

typedef enum _E2_ChildMenuType
{
	E2_CHILD_ALL,	//everything in the tasks history list into a check-menu
	E2_CHILD_ACTIVE, //active child processes i.e. commands
	E2_CHILD_ACTIVETASKS, //active child processes and action(s) (in Q, normally)
	E2_CHILD_OUTPUT, //child processes which have printed to output pane
} E2_ChildMenuType;

//callbacks
void e2_menu_action_activated_cb (GtkMenuItem *menuitem, gpointer data);
void e2_menu_action_activated_cb2 (GtkMenuItem *menu_item, gpointer data);
void e2_menu_selection_done_cb (GtkWidget *menu, gpointer data);
void e2_menu_control_cb (GtkMenuItem *menu_item, gpointer data);
gboolean e2_menu_destroy (GtkWidget *menu);

//public
#define e2_menu_get gtk_menu_new

void e2_menu_create_plugins_menu (GtkWidget *menu, gboolean onetime, gboolean is_selection);
GtkWidget *e2_menu_create_child_menu (E2_ChildMenuType type,
	void (*activate_cb)()) G_GNUC_MALLOC; //func args (GtkMenuItem*,E2_TaskRuntime*)
#ifdef E2_FS_MOUNTABLE
gboolean e2_menu_create_mounts_menu (gpointer from, E2_ActionRuntime *art);
#endif
GtkWidget *e2_menu_create_filter_menu (ViewInfo *view) G_GNUC_MALLOC;
GtkWidget *e2_menu_create_custom_menu (gchar *name) G_GNUC_MALLOC;

GtkWidget *e2_menu_add (GtkWidget *menu, const gchar *label, const gchar *icon,
	const gchar *tip, void (*func)(), gpointer data) G_GNUC_MALLOC; //func args GtkMenuItem*,gpointer
GtkWidget *e2_menu_add_check (GtkWidget *menu, const gchar *label, gboolean state,
	void (*func)(), gpointer data) G_GNUC_MALLOC; //func args GtkCheckMenuItem*,gpointer
GtkWidget *e2_menu_add_radio (GtkWidget *menu, GSList **group, const gchar *label,
	gboolean state, void (*func)(), gpointer data) G_GNUC_MALLOC; //func args GtkMenuItem*,gpointer
GtkWidget *e2_menu_add_action (GtkWidget *menu, const gchar *label, const gchar *icon,
	const gchar *tip, const gchar *action, const gchar *arg);
GtkWidget *e2_menu_add_separator (GtkWidget *menu) G_GNUC_MALLOC;
#ifndef USE_GTK3_4
GtkWidget *e2_menu_add_tear_off (GtkWidget *menu) G_GNUC_MALLOC;
#endif
GtkWidget *e2_menu_add_submenu (GtkWidget *menu, const gchar *label_text,
	const gchar *icon) G_GNUC_MALLOC;
GtkWidget *e2_menu_add_toggle (GtkWidget *menu, gboolean destroy,
	const gchar *label, const gchar *icon, const gchar *tip, const gchar *toggle,
	const gchar *cmd) G_GNUC_MALLOC;
GtkWidget *e2_menu_create_options_menu (GtkWidget *controller,
	GtkWidget *menu, E2_OptionSet *set, ...)
#ifdef G_GNUC_NULL_TERMINATED
    G_GNUC_NULL_TERMINATED
#endif
	;
void e2_menu_add_bookmark_items (GtkWidget *menu, GtkWidget *top_menu,
	E2_Action *action, guint32 markflags, gint32 pane, GtkTreeModel *mdl, GtkTreeIter *iter);
void e2_menu_add_desktop_actions (GtkWidget *menu, const gchar *appname);
void e2_menu_add_desktop_mime (GtkWidget *menu, const gchar *mimetype);
void e2_menu_add_filehandlers (GtkWidget *menu, const gchar *localpath);
void e2_menu_add_filetype_items (GtkWidget *menu, ViewInfo *view);

void e2_menu_mark_clear (gpointer data, GClosure *closure);
void e2_menu_connect (GtkWidget *menu, gboolean  active);
void e2_menu_popup (GtkWidget *menu, gint button, guint32 time);

void e2_menu_custom_option_register (void);

#endif //ndef __E2_MENU_H__
