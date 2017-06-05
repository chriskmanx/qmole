/* $Id: e2_mousebinding.h 2743 2013-09-19 22:29:00Z tpgww $

Copyright (C) 2008-2013 tooar <tooar@emelfm2.net>

This file is part of emelfm2.
emelfm2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelfm2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_BTNBINDING_H__
#define __E2_BTNBINDING_H__

#include "emelfm2.h"

//enable data conversion at idle-time, instead of when a binding is registered
//? CHECKME it interferes with config dialog edit-cancellation
#define E2_IDLE_BTNSYNC

//button-bindings config data treestore column numbers
enum
{
	MCOL_CAT,
	MCOL_BTN,
	MCOL_DBL,
	MCOL_TRP,
//releases not supported (too much hassle with multi-clicks on nested widgets)
//NOTE lot of stuff depends on this define
//	MCOL_REL,
	MCOL_ACT,
	MCOL_ARG
};

//fake event types
#define E2_2BUTTON_RELEASE GDK_NOTHING-1
#define E2_3BUTTON_RELEASE GDK_NOTHING-2

//some funcs shared with gestures file
gboolean e2_mousebinding_tree_selection_check_cb
	(GtkTreeSelection *selection, GtkTreeModel *model, GtkTreePath *path,
	gboolean path_currently_selected, E2_OptionSet *set);
gboolean e2_mousebinding_tree_draggable_check_cb
	(GtkTreeDragSource *drag_source, GtkTreePath *path);
gboolean e2_mousebinding_visible_check_cb (GtkTreeModel *model,
	GtkTreeIter *iter, GtkCellRenderer *cell, gpointer data);

gboolean e2_mousebinding_parse_name (gchar *button, guint *number,
	GdkModifierType *state, guint *count, gboolean multi);

void e2_mousebinding_register_all (void);
void e2_mousebinding_enrol (GtkWidget *widget, const gchar *category,
	void (*defaults_func)(E2_OptionSet*));
void e2_mousebinding_disrol (GtkWidget *widget, const gchar *category);
void e2_mousebinding_block (GtkWidget *widget, const gchar *category);
void e2_mousebinding_unblock (GtkWidget *widget, const gchar *category);
void e2_mousebinding_output_help (gchar *section);
void e2_mousebinding_clean (void);
void e2_mousebinding_free_all (void);
#ifdef WITH_BUTTONFAKE
void e2_mousebinding_actions_register (void);
#endif
void e2_mousebinding_options_register (void);

#ifdef E2_PTRGESTURES
void e2_mousegesture_enrol (GtkWidget *widget, const gchar *category,
	void (*defaults_func)(E2_OptionSet*));
void e2_mousegesture_disrol (GtkWidget *widget, const gchar *category);
void e2_mousegesture_block (GtkWidget *widget, const gchar *category);
void e2_mousegesture_unblock (GtkWidget *widget, const gchar *category);
void e2_mousegesture_clean (void);
void e2_mousegesture_free_all (void);
void e2_mousegesture_options_register (void);

GtkWidget *e2_gesture_dialog_create (GtkWidget *parent, const gchar *initial_sequence) G_GNUC_MALLOC;
#endif

#endif //ndef __E2_BTNBINDING_H__
