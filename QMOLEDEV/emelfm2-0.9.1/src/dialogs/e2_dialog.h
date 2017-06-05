/* $Id: e2_dialog.h 2831 2013-10-23 07:59:22Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

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
@file  src/dialogs/e2_dialog.h
@brief  dialog utilities header file

This file is the header for the dialog utility functions.
*/

#ifndef __E2_DIALOG_H__
#define __E2_DIALOG_H__

#include "emelfm2.h"
#include <pwd.h>
#include "e2_about_dialog.h"
#include "e2_config_dialog.h"
//#include "e2_mkdir_dialog.h"
#include "e2_select_image_dialog.h"
#include "e2_view_dialog.h"
//button signals from here
#include "e2_button.h"
#include "e2_fileview.h"

//this supports one-time dialog setup (not yet implemented)
//#define LATESHOW

typedef enum
{
	E2_DIALOG_MODAL        =1,		//DO NOT use with MAINLOOP
//	E2_DIALOG_NONMODAL     =1<<1,	//not implemented
	E2_DIALOG_BLOCKED      =1<<2,	//start a local main-loop after showing the dialog DO NOT use with MODAL
	E2_DIALOG_FREE         =1<<3,	//only works for MODAL or MAINLOOP
	E2_DIALOG_DONT_SHOW_ALL=1<<4,
	E2_DIALOG_CLOSELOCK    =1<<5,	//threads_enter/leave around showing the dialog
	E2_DIALOG_MULTI        =1<<6,	//dialog relates to multiple selected items
	E2_DIALOG_POS          =1<<7	//remember horz and vert position for reuse next time
} E2_DialogFlags;

typedef enum
{
	IGNORE = -1,
	OK,
	CANCEL,
	APPLY,
	APPLY_TO_ALL,
	YES,
	YES_TO_ALL,
	NO,
	NO_TO_ALL
} DialogButtons;

typedef enum
{
	NONE   = 0,
	YESALL = 1,
	NOALL  = 1 << 1,
	BOTHALL= 1 << 2,
	PERMITTED = 1 << 6 //add "no permission" label to dialog and adjust buttons accordingly
} OW_ButtonFlags;

enum { E2_TYPE_CANCELLED, E2_TYPE_HANDLED, E2_TYPE_NOTHANDLED };

#define DEFAULT_RESPONSE_CB (ResponseFunc)NULL
#define DUMMY_RESPONSE_CB (ResponseFunc)1	//non-NULL

typedef struct _E2_Wait
{
	DialogButtons choice;
	E2_MainLoop *loopdata;
} E2_Wait;

DialogButtons e2_dialog_response_decode (gint response);
DialogButtons e2_dialog_wait (GtkWidget *dialog,
	gboolean locked, gboolean maincontext, gboolean multi, gboolean getresult);
//void e2_dialog_response_decode_cb (GtkDialog *dialog, gint response,
//	DialogButtons *dialog_result);
gboolean e2_dialog_cancel_cb (GtkWidget *widget, GtkWidget *dialog);
#ifdef USE_GTK3_0
gboolean e2_dialog_key_cb (GtkWidget *widget, GdkEventKey *event,
	gpointer user_data);
#endif
gboolean e2_dialog_key_neg_cb (GtkWidget *widget, GdkEventKey *event,
	gpointer user_data);
//gboolean e2_dialog_key_press_cb
//	(GtkWidget *widget, GdkEventKey *event, GtkWidget *dialog);
void e2_dialog_show_cb (GtkWidget *dialog, GtkScrolledWindow *scrolled);
void e2_dialog_show_notebook_cb (GtkWidget *dialog, GtkNotebook *book);
/* tag PASSWORDINPUT
DialogButtons e2_dialog_password_input (gchar* window_title, gchar *prompt,
	gchar **input); */
//generic dialog for line input
DialogButtons e2_dialog_line_input (gchar* window_title, gchar *prompt,
	gchar *suggestion, OW_ButtonFlags extras, gboolean select_text,
	gchar **input);
DialogButtons e2_dialog_positioned_input (gchar* window_title, gchar *prompt,
	gchar *suggestion, OW_ButtonFlags extras, gboolean select_text, gboolean permitted,
	gint *horz, gint *vert, gchar **input);
DialogButtons e2_dialog_combo_input (gchar* window_title, gchar *prompt,
	gchar *suggestion, OW_ButtonFlags extras, GList **history_list, gchar **input);
//delete confirmation dialog
DialogButtons e2_dialog_delete_check (VPATH *localpath, gboolean multi,
	gboolean permitted,	gint *horz, gint *vert);
//overwrite check dialog
DialogButtons e2_dialog_ow_check (VPATH *slocal, VPATH *dlocal, OW_ButtonFlags extras);
DialogButtons e2_dialog_warning (gchar *prompt, const gchar *yes_label);
GtkWidget *e2_dialog_slow (gchar *prompt_type, gchar *tip_type,
	void(*response_func) (GtkDialog*,gint,gpointer), gpointer data) G_GNUC_MALLOC;
void e2_dialog_setup_chooser (GtkWidget *dialog, const gchar *title,
	const gchar *startpath, GtkFileChooserAction action, gboolean showhidden,
	gboolean multi, gint defresponse, const gchar *first_button, ...)
#ifdef G_GNUC_NULL_TERMINATED
    G_GNUC_NULL_TERMINATED
#endif
	;

void e2_dialog_setup_auth (GtkWidget *box);
void e2_dialog_setup (GtkWidget *dialog, GtkWidget *parent);
gint e2_dialog_show (GtkWidget *dialog, GtkWidget *parent, E2_DialogFlags flags,
	E2_Button *button, ...);
gint e2_dialog_run (GtkWidget *dialog, GtkWidget *parent, E2_DialogFlags flags);
//GtkWidget *get_dialog (GtkWidget *parent, GtkMessageType type, gchar *label_text);
GtkWidget *e2_dialog_create (const gchar *stock, const gchar *label_text,
	const gchar *title,	void (*callback) (GtkDialog*,gint,gpointer), gpointer data) G_GNUC_MALLOC;
GtkWidget *e2_dialog_add_sw (GtkWidget *dialog);
GtkWidget *e2_dialog_add_defined_button (GtkWidget *dialog, E2_Button *button) G_GNUC_MALLOC;
GtkWidget *e2_dialog_add_simple_button (GtkWidget *dialog, const gchar *stock,
	const gchar *label, gint response) G_GNUC_MALLOC;
GtkWidget *e2_dialog_add_custom_button (GtkWidget *dialog, E2_Button *button,
	gboolean is_default, const gchar *tip, void *callback, void *data);
GtkWidget *e2_dialog_add_custom_button_full
	(GtkWidget *dialog, gboolean is_default, guint response,
	  const gchar *label, const gchar *stock, const gchar *tip,
	  void (*callback)(/*GtkButton*,gpointer*/), gpointer data) G_GNUC_MALLOC;
GtkWidget *e2_dialog_add_toggle_button (GtkWidget *dialog, gboolean value,
	gchar *label, gchar *tip, gint response) G_GNUC_MALLOC;
GtkWidget *e2_dialog_add_check_button (GtkWidget *dialog, gboolean value,
	gchar *label, gchar *tip, gint response) G_GNUC_MALLOC;
//void e2_dialog_set_positive_response (GtkWidget *dialog, gint response);
void e2_dialog_set_negative_response (GtkWidget *dialog, gint response);
//void e2_dialog_set_responses (GtkWidget *dialog, gint pos, gint neg);
void e2_dialog_resize (GtkWidget *dialog, gfloat factor);
void e2_dialog_set_cursor (GtkWidget *dialog, GdkCursorType type);
void e2_dialog_options_register (void);

//dialog setup functions from other files
void e2_name_filter_dialog_create_cb (GtkCheckMenuItem *item, ViewInfo *view);
void e2_size_filter_dialog_create_cb (GtkCheckMenuItem *item, ViewInfo *view);
void e2_date_filter_dialog_create_cb (GtkCheckMenuItem *item, ViewInfo *view);
time_t e2_date_filter_dialog_get_time (const gchar *datestring, gchar *buf,
	gint bufsize, gint date_format_index);
void e2_select_items_byname_dialog_create (GtkMenuItem *item);
gint e2_filetype_dialog_create (VPATH *localpath, gboolean text,
	gboolean ambig, gboolean newtype);
DialogButtons e2_file_info_dialog_run (VPATH *localpath, gboolean multi);
void e2_file_info_dialog_options_register (void);
gint e2_dialog_run_simple (GtkWidget *dialog, GtkWidget *parent);
void e2_filetype_dialog_edit_create (gchar *category);
void e2_opendir_dialog_create (ViewInfo *view, gchar *oldpath, gchar **newpath);

#endif //ndef __E2_DIALOG_H__
