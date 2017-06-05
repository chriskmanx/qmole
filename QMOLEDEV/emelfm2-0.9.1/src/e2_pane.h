/* $Id: e2_pane.h 2742 2013-09-11 00:37:42Z tpgww $

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

#ifndef __E2_PANE_H__
 #define __E2_PANE_H__

#include "emelfm2.h"
#include "e2_toolbar.h"
//#include "e2_option.h" in toolbar.h

//E2PANENATIVE for using native CWD regardless whether panes 1 & 2 are mounted or virtual
enum { E2PANECUR, E2PANE1, E2PANE2, E2PANENATIVE, E2PANECONF } ;

typedef struct _E2_PaneRuntime
{
	ViewInfo view;	//at start of struct to support casting between pane and view
	gchar *path;	//UTF-8 string, generally = view->dir, for use when allocated version is needed
	gchar *name;	//"pane1" or "pane2", not translated, for constructing set names
	GtkWidget *outer_box;	//hbox, parent of inner_box
	GtkWidget *inner_box; //vbox, parent of pane_sw (& often, the toolbar)
	GtkWidget *pane_sw; //scroller, parent of filelist treeview
//	GtkWidget *focus_widget;
	E2_ToolbarRuntime toolbar;
#ifdef E2_FAM
	gint FAMreq;	//FAM request id used for this pane
#endif
	E2_OptionSet *opt_transparent;	//for quick checks whether to interpret relative path strings
	GList *opendirs;	/* effectively a list of paths of dirs previously opened (UTF-8,
						absolute for current namespace, with trailer). Used for
						"goto-buttons" menus. Actually, data are pointers belonging
						to app.dir_history. Unlike the latter, this list may have
						multiple entries for the same dir */
	guint opendir_cur;	//0-based index of current position in opendirs history
						//NOTE reverse of list-index, 0 corresponds to _last_
						//member of list, (list-length - 1) is for _first_ member
	GHookList hook_change_dir;	//data for functions to run during change-dir function,
								//for the pane
} E2_PaneRuntime;

gboolean e2_pane_choose_new_dir (E2_PaneRuntime *rt, GtkWidget *entry);
gboolean e2_pane_cd_checks (gchar *path);
#ifdef E2_VFS
gboolean e2_pane_change_space (E2_PaneRuntime *rt, VPATH *utfpath);
gboolean e2_pane_change_space_byuri (E2_PaneRuntime *rt, const gchar *spacedescriptor);
#endif
E2_PaneRuntime *e2_pane_get_runtime (gpointer from, gpointer actiondata,
	const gchar **multidata);
void e2_pane_activate_other (void);
void e2_pane_change_dir (E2_PaneRuntime *rt, const gchar *path);
gboolean e2_pane_goto_accessible_path (E2_PaneRuntime *rt);
//void e2_pane_change_dir_sync (E2_PaneRuntime *rt, gchar *path, E2_CDType *completed_flag);
void e2_pane_create (E2_PaneRuntime *rt);
void e2_pane_create_part (E2_PaneRuntime *rt);
void e2_pane_create_option_data (E2_PaneRuntime *rt);
//void e2_pane_destroy (E2_PaneRuntime *rt);
//void e2_pane_recreate (E2_PaneRuntime *rt);
// initialisation things
void e2_pane_flag_active (void);
//void e2_pane_flag_inactive (void);
//void e2_pane_flag_history (E2_PaneRuntime *rt, gboolean value);
void e2_pane_trim_history (E2_PaneRuntime *rt);
void e2_pane_actions_register (void);
void e2_pane_options_register (gint num);

GtkWidget *e2_pane_visited_menu (E2_PaneRuntime *rt);

#endif //ndef __E2_PANE_H__
