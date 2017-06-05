/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __PREFS_GTK_H__
#define __PREFS_GTK_H__

#include <glib.h>
#include <gtk/gtk.h>
#include <stdio.h>

typedef struct _PrefParam	PrefParam;
typedef struct _PrefsDialog	PrefsDialog;

#include "prefs.h"
#include "gtk/prefswindow.h"

#define PREFSBUFSIZE		32768

typedef enum
{
	P_STRING,
	P_INT,
	P_BOOL,
	P_ENUM,
	P_USHORT,
	P_COLOR,
	P_PASSWORD,
	P_OTHER
} PrefType;

typedef void (*DataSetFunc)   (PrefParam *pparam);
typedef void (*WidgetSetFunc) (PrefParam *pparam);

struct _PrefParam {
	gchar	      *name;
	gchar	      *defval;
	gpointer       data;
	PrefType       type;
	GtkWidget    **widget;
	DataSetFunc    data_set_func;
	WidgetSetFunc  widget_set_func;
};

struct _PrefsDialog 
{
	GtkWidget *window;
	GtkWidget *notebook;

	GtkWidget *ok_btn;
	GtkWidget *cancel_btn;
	GtkWidget *apply_btn;
};

#define SET_NOTEBOOK_LABEL(notebook, str, page_num) \
{ \
	GtkWidget *label; \
	gint i = page_num;	\
  \
	label = gtk_label_new_with_mnemonic (str); \
	gtk_widget_show (label); \
	gtk_notebook_set_tab_label \
		(GTK_NOTEBOOK (notebook), \
		 gtk_notebook_get_nth_page \
			(GTK_NOTEBOOK (notebook), i), \
		 label); \
	gtk_notebook_set_menu_label_text \
		(GTK_NOTEBOOK (notebook), \
		 gtk_notebook_get_nth_page \
			(GTK_NOTEBOOK (notebook), i), \
		 str);\
}

#define PACK_CHECK_BUTTON(box, checkbtn, label) \
{ \
	checkbtn = gtk_check_button_new_with_label(label); \
	gtk_widget_show(checkbtn); \
	gtk_box_pack_start(GTK_BOX(box), checkbtn, FALSE, TRUE, 0); \
}

#define PACK_END_CHECK_BUTTON(box, checkbtn, label) \
{ \
	checkbtn = gtk_check_button_new_with_label(label); \
	gtk_widget_show(checkbtn); \
	gtk_box_pack_end(GTK_BOX(box), checkbtn, FALSE, TRUE, 0); \
}

#define PACK_FRAME(box, frame, label) \
{ \
	frame = gtk_frame_new(label); \
	gtk_widget_show(frame); \
	gtk_box_pack_start(GTK_BOX(box), frame, FALSE, TRUE, 0); \
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.01, 0.5); \
}

#define PACK_VSPACER(box, vbox, spacing) \
{ \
	vbox = gtk_vbox_new(FALSE, 0); \
	gtk_widget_show(vbox); \
	gtk_box_pack_start(GTK_BOX(box), vbox, FALSE, TRUE, spacing); \
}

#define SET_TOGGLE_SENSITIVITY(togglewid, targetwid) \
{ \
	gtk_widget_set_sensitive(targetwid, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(togglewid))); \
	g_signal_connect(G_OBJECT(togglewid), "toggled", \
			 G_CALLBACK(prefs_button_toggled), targetwid); \
}

#define SET_TOGGLE_SENSITIVITY_REVERSE(togglewid, targetwid) \
{ \
	gtk_widget_set_sensitive(targetwid, !gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(togglewid))); \
	g_signal_connect(G_OBJECT(togglewid), "toggled", \
			 G_CALLBACK(prefs_button_toggled_reverse), targetwid); \
}

void prefs_read_config		(PrefParam	*param,
				 const gchar	*label,
				 const gchar	*rcfile,
				 const gchar	*encoding);
void prefs_write_config		(PrefParam	*param,
				 const gchar	*label,
				 const gchar	*rcfile);
gint prefs_write_param		(PrefParam	*param,
				 FILE		*fp);

PrefFile *prefs_write_open	(const gchar	*path);
gint prefs_write_close		(PrefFile	*pfile);
gint prefs_write_close_revert	(PrefFile	*pfile);

void prefs_set_default		(PrefParam	*param);
void prefs_free			(PrefParam	*param);

void prefs_button_toggled	(GtkToggleButton	*toggle_btn,
				 GtkWidget		*widget);
void prefs_button_toggled_reverse	(GtkToggleButton	*toggle_btn,
				 GtkWidget		*widget);

void prefs_set_dialog		(PrefParam	*param);
void prefs_set_data_from_dialog	(PrefParam	*param);
void prefs_set_dialog_to_default(PrefParam	*param);

void prefs_set_data_from_entry	(PrefParam	*pparam);
void prefs_set_escaped_data_from_entry	(PrefParam	*pparam);
void prefs_set_entry		(PrefParam	*pparam);
void prefs_set_entry_from_escaped	(PrefParam	*pparam);
void prefs_set_data_from_text	(PrefParam	*pparam);
void prefs_set_escaped_data_from_text	(PrefParam	*pparam);
void prefs_set_text		(PrefParam	*pparam);
void prefs_set_text_from_escaped(PrefParam *pparam);
void prefs_set_data_from_toggle	(PrefParam	*pparam);
void prefs_set_toggle		(PrefParam	*pparam);
void prefs_set_data_from_spinbtn(PrefParam	*pparam);
void prefs_set_spinbtn		(PrefParam	*pparam);

void prefs_gtk_open		(void);
void prefs_gtk_register_page	(PrefsPage 	*page);
void prefs_gtk_unregister_page	(PrefsPage 	*page);

void prefs_prepare_cache(void);
void prefs_destroy_cache(void);

#endif /* __PREFS_H__ */
