/* $Id: e2_button.h 2748 2013-09-20 09:41:43Z tpgww $

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

#ifndef __E2_BUTTON_H__
#define __E2_BUTTON_H__

//for older gtk's
#ifndef GTK_STOCK_DISCARD
# define GTK_STOCK_DISCARD "gtk-discard"
#endif

//index of button-labels and corresponding tips, see defined_button_labels[]
typedef enum
{
	//+ve choices
	BTN_YES_COMMIT,
	BTN_YES_CONTINUE,
	BTN_YES_DELETE,
	//-ve choices
	BTN_NO_CANCEL,
	BTN_NO_SKIP,
	BTN_NO_KEEP,
} E2_ButtonText;

//#define	BTN_YES_DEFAULT BTN_YES_COMMIT
//#define	BTN_NO_DEFAULT BTN_NO_CANCEL

typedef enum
{
	E2_BTN_DEFAULT = 1,
	E2_BTN_GREY    = 1 << 1,
	E2_BTN_TIPPED  = 1 << 2,	//use the button's tip, if any
} E2_ButtonType;
//data for setting up a dialog-button
typedef struct _E2_Button
{
	const gchar *label;
	const gchar *name; //name of icon file (without .png extension)
	gchar *tip;	//custom tip or NULL, sometimes non-constant
	E2_ButtonType showflags;
	E2_ButtonType default_flags;
	gint response;
} E2_Button;

typedef enum
{
	E2_BUTTON_FREE_DATA         = 1,
	E2_BUTTON_ICON_ABOVE_LABEL  = 1 << 1,
	E2_BUTTON_CAN_FOCUS         = 1 << 2,
	E2_BUTTON_CAN_DEFAULT       = 1 << 3,
	E2_BUTTON_SHOW_MISSING_ICON = 1 << 4
} E2_ButtonFlags;

// custom button signal responses, used in dialogs
#define E2_RESPONSE_APPLY		GTK_RESPONSE_YES	//yes and apply buttons work same
#define E2_RESPONSE_NOTOALL		110
#define E2_RESPONSE_APPLYTOALL	111
#define E2_RESPONSE_CREATE		112
#define E2_RESPONSE_REFRESH		113
#define E2_RESPONSE_REMOVE		114
#define E2_RESPONSE_DISCARD		115
#define E2_RESPONSE_FIND		116
#define E2_RESPONSE_MORE		117

#define E2_RESPONSE_USER1		120
#define E2_RESPONSE_USER2		121
#define E2_RESPONSE_USER3		122
#define E2_RESPONSE_USER4		123
#define E2_RESPONSE_USER5		124
#define E2_RESPONSE_USER6		125

//static button templates, when relevant these are copied locally and amended for use
E2_Button E2_BUTTON_YES;
E2_Button E2_BUTTON_NO;
E2_Button E2_BUTTON_CANCEL;
E2_Button E2_BUTTON_APPLY;
E2_Button E2_BUTTON_APPLYTOALL;
E2_Button E2_BUTTON_REFRESH;
E2_Button E2_BUTTON_CLOSE;
E2_Button E2_BUTTON_CREATE;
E2_Button E2_BUTTON_REMOVE;
E2_Button E2_BUTTON_DISCARD;
E2_Button E2_BUTTON_MORE;

void e2_button_setup_labels (void);
void e2_button_derive (E2_Button *button, E2_Button *base, E2_ButtonText type);
void e2_button_set_indexed_text (GtkWidget *button, E2_ButtonText type);
void e2_button_set_label (GtkWidget *button, const gchar *label);
void e2_button_set_image (GtkWidget *button, gchar *stock);
GtkWidget *e2_button_get (const gchar *label, const gchar *stock, const gchar *tip,
	void (*callback)(/*GtkButton*,gpointer*/), gpointer data);
GtkWidget *e2_button_get_full (const gchar *label, const gchar *stock, GtkIconSize size,
	const gchar *tip, void (*callback)(/*GtkButton*,gpointer*/), gpointer data,
	E2_ButtonFlags flags) G_GNUC_MALLOC;
GtkWidget *e2_button_add (GtkWidget *box, gboolean exp, guint pad, gchar *label,
	gchar *stock, gchar *tip, void (*callback)(/*GtkButton*,gpointer*/), gpointer data) G_GNUC_MALLOC;
GtkWidget *e2_button_add_end (GtkWidget *box, gboolean exp, guint pad,gchar *label,
	gchar *stock, gchar *tip, void (*callback)(/*GtkButton*,gpointer*/), gpointer data) G_GNUC_MALLOC;
GtkWidget *e2_button_get_toggle (gboolean check, gboolean state, gchar *label,
	gchar *tip, void (*callback)(GtkToggleButton*,gpointer), gpointer data) G_GNUC_MALLOC;
GtkWidget *e2_button_add_toggle (GtkWidget *box,  gboolean check, gboolean state,
	gchar *label, gchar *tip, gboolean exp, guint pad,
	void (*callback)(/*GtkToggleButton*,gpointer*/), gpointer data) G_GNUC_MALLOC;
GtkWidget *e2_button_add_radio (GtkWidget *box, gchar *label, GSList *group,
	gboolean state, gboolean exp, guint pad,
	void (*callback)(GtkToggleButton*,gpointer), gpointer data) G_GNUC_MALLOC;
GtkWidget *e2_button_add_to_table (GtkWidget *table, gchar *label,
	void (*callback)(GtkButton*,gpointer), gpointer data,
	gint left, gint right, gint top, gint bottom) G_GNUC_MALLOC;
GtkWidget *e2_button_add_toggle_to_table(GtkWidget *table, gchar *label,
	gboolean state, void (*callback)(/*GtkToggleButton*,gpointer*/), gpointer data,
	gint left, gint right, gint top, gint bottom) G_GNUC_MALLOC;
GtkWidget *e2_button_add_radio_to_table (GtkWidget *table, gchar *label,
	GSList *group, gboolean state, void (*callback)(/*GtkToggleButton*,gpointer*/),
	gpointer data, gint left, gint right, gint top, gint bottom) G_GNUC_MALLOC;

#endif //ndef __E2_BUTTON_H__
