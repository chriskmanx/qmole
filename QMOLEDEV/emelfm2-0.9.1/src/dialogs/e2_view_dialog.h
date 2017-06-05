/* $Id: e2_view_dialog.h 2743 2013-09-19 22:29:00Z tpgww $

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

#ifndef __E2_VIEW_DIALOG_H__
#define __E2_VIEW_DIALOG_H__

#ifdef E2_SPELLCHECK
//NOTE to access internals for the dialog's context-menu-creation, we must
//use a recent, or fairly-recent and patched, gtk-spell
#include <gtkspell/gtkspell.h>
#endif

//include redo capability
#define E2_REDO_ENABLED
//include highlight capability
#define E2_MARK_FINDS

enum
{
	INS = 0,
	BS,
	DEL
};

typedef struct _E2_UndoInfo
{
	gchar command;	//one of INS, BS, DEL
	gchar *str;	//buffer text that has been changed in accord with command
	//guint offsets allow for reasonably large files ...
	guint start;	//text-buffer offset to start of str
	guint end;	//text-buffer offset to end of str
	gboolean seq; // sequency flag ??
} E2_UndoInfo;

//this struct is used for viewing and editing, to enable shared functions
typedef struct _E2_ViewDialogRuntime
{
	GtkWidget *dialog;
	GtkWidget *textview;
	GtkTextBuffer *textbuffer;
	gchar *localpath;	//localised absolute path string
#ifdef E2_VFS
	PlaceInfo *spacedata;
#endif
	gint window_width;
	gint window_height;
//	guint idle_id;
	//all search dialogs in session use same values for these, hence static
	gboolean case_sensitive;
	gboolean search_backward;
	gboolean whole_words;
	gboolean search_wrap;
	gboolean textwrap;
	//search things
	GtkWidget *panel;
	GtkWidget *combo;
	GtkWidget *info_label;
	GtkWidget *hidebtn;
	GtkWidget *findbtn;
	GtkSizeGroup *sgroup;
	GList *history;
	gint history_cur;
	gboolean is_hidden;	//reflects hidden property of panel widget;
	gboolean release_blocked;//flag to manage key-release events which are not for the expected widget
	//edit things
	GtkWidget *replacebtn;
	GtkWidget *savebtn;
	GtkWidget *combo2;	//replacements
	GtkWidget *replacebar; //for hiding
	gint linebreak;	//type of line-separator in loaded file, 10, 13 or 23 (=CR+lF)
	const gchar *charset;	//name of file's character encoding (for internal conversion)
	GList *rephistory;
	gint rephistory_cur;
#ifdef E2_MARK_FINDS
	gboolean research;	//TRUE when whole_words or case_sensitive has been changed, until next highlight update
	gboolean is_lit;
#endif
	gboolean is_dirty;
	gboolean saved_ok;
	gboolean saveas;	//TRUE when saving to a different name
	const gchar *newlocalpath; //used for saving as ..
	gboolean ow_mode;	//T = overwrite, F = insert
	guint blink_id;
	guint blink_init_id;
	GList *undo_list;	//init to NULL;	//list of UndoInfo's for changes made to buffer
	gboolean undo_enabled;	//widget sensitivity controller
#ifdef E2_REDO_ENABLED
	GList *redo_list;	//init to NULL; //list of UndoInfo's for buffer changes undone
	gboolean redo_enabled;
#endif
	GString *undo_gstr;	//buffer for accumulating sequentially-entered characters
	E2_UndoInfo *ui_tmp;	//template undo-data struct
	gboolean seq_reserve;	//init to FALSE;	//default value for ui->seq ??
	guint changes_count;	//no. of changes recorded in the undo list
	guint keyval;	//keycode from the keypress callback
	guint prev_keyval;	//remembered previous key, for sequencing
#ifdef E2_SPELLCHECK
	GtkSpell *spelldata;
#endif
} E2_ViewDialogRuntime;

typedef struct _E2_ViewHistory
{
	gchar *localpath;	//localised string, absolute path of a viewed item
	guint topline;	//buffer y coordinate of top visible row when internal viewer was closed
} E2_ViewHistory;

//functions shared between view & edit dialogs
#ifdef E2_MARK_FINDS
void e2_view_dialog_init_hilites (E2_ViewDialogRuntime *rt);
void e2_view_dialog_clear_hilites (E2_ViewDialogRuntime *rt);
#endif
gboolean e2_view_dialog_read_text (VPATH *localfile, E2_ViewDialogRuntime *rt);
void e2_view_dialog_set_font (gint *char_width, gint *char_height,
	E2_ViewDialogRuntime *rt);
void e2_view_dialog_set_menu_position (GtkWidget *menu,
	gint *x, gint *y, gboolean *push_in, E2_ViewDialogRuntime *rt);
void e2_view_dialog_update_combo (GtkWidget *combo);
gboolean e2_view_dialog_combokey_cb (GtkWidget *entry, GdkEventKey *event,
	E2_ViewDialogRuntime *rt);
void e2_view_dialog_toggled_cb (GtkToggleButton *button, gboolean *store);
GtkWidget *e2_view_dialog_create_searchbar (E2_ViewDialogRuntime *rt) G_GNUC_MALLOC;
gboolean e2_view_dialog_search (gboolean first, gboolean incremental,
	E2_ViewDialogRuntime *rt);
void e2_view_dialog_destroy (E2_ViewDialogRuntime *rt);
void e2_view_dialog_show_atlast (VPATH *localpath, E2_ViewDialogRuntime *rt);

gboolean e2_view_dialog_create (VPATH *localpath);
gboolean e2_view_dialog_create_immediate (VPATH *view_this);
void e2_view_dialog_actions_register (void);
void e2_view_dialog_options_register (void);

//FIXME location of these
gboolean e2_edit_dialog_create (VPATH *localpath, GtkTextBuffer *buf);
gboolean e2_edit_dialog_key_press_cb (GtkWidget *textview, GdkEventKey *event,
	E2_ViewDialogRuntime *rt);
void e2_edit_dialog_save_selected (GtkTextBuffer *buffer,
#ifdef E2_VFS
	PlaceInfo *spacedata,
#endif
	GtkWidget *parent);
void e2_edit_dialog_actions_register (void);
void e2_edit_dialog_options_register (void);

#endif //ndef __E2_VIEW_DIALOG_H__
