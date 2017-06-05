/* $Id: e2_widget.h 2743 2013-09-19 22:29:00Z tpgww $

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
@file src/utils/e2_widget.h
@brief miscelleanous GtkWidget utility function headers

This file is the header file for the miscelleanous GtkWidget utility
functions.
*/

#ifndef __E2_WIDGET_H__
#define __E2_WIDGET_H__

#ifdef USE_GTK2_12TIPS
void e2_widget_set_toggletip (GtkWidget *widget, const gchar *initialtip,
	const gchar *othertip);
void e2_widget_set_safetip (GtkWidget *widget, const gchar *tiptext);
#else
void e2_widget_set_toggletip (GtkWidget *widget, const gchar *initialtip,
	const gchar *othertip);
void e2_widget_set_tip (GtkWidget *widget, const gchar *tiptext1, const gchar *tiptext2);
# define e2_widget_set_safetip(w,t) e2_widget_set_tip(w,t,NULL)
#endif
void e2_widget_swap_tooltip (GtkWidget *widget);
GtkWidget *e2_widget_get_icon (const gchar *icon, GtkIconSize size) G_GNUC_MALLOC;

GtkWidget *e2_widget_add_mid_label (GtkWidget *box, const gchar *text, gfloat align,
	gboolean exp, guint pad) G_GNUC_MALLOC;
GtkWidget *e2_widget_add_label (GtkWidget *box, const gchar *text,
	gfloat xalign, gfloat yalign, gboolean exp, guint pad) G_GNUC_MALLOC;

GtkWidget *e2_widget_add_entry (GtkWidget *box, gchar *init_text, gboolean exp,
	gboolean select_text) G_GNUC_MALLOC;

GtkWidget *e2_widget_get_box (gboolean vertical, gboolean homogen, gint spacing) G_GNUC_MALLOC;
GtkWidget *e2_widget_add_box (GtkWidget *parent, gboolean exp, guint pad,
	gboolean vertical, gboolean homogen, gint spacing) G_GNUC_MALLOC;

GtkWidget *e2_widget_add_sw (GtkWidget *box, GtkPolicyType h_policy,
	GtkPolicyType v_policy, gboolean exp, guint pad) G_GNUC_MALLOC;
GtkWidget *e2_widget_get_sw_plain (GtkPolicyType h_policy, GtkPolicyType v_policy);
GtkWidget *e2_widget_get_sw (GtkPolicyType h_policy, GtkPolicyType v_policy,
	GtkShadowType shadow) G_GNUC_MALLOC;
void e2_widget_sw_add_with_viewport (GtkWidget *sw, GtkWidget *child);

GtkWidget *e2_widget_add_separator (GtkWidget *box, gboolean exp, guint pad) G_GNUC_MALLOC;
GtkWidget *e2_widget_add_table (GtkWidget *box, gint rows, gint cols,
	gboolean homogen, gboolean exp, guint pad) G_GNUC_MALLOC;
GtkWidget *e2_widget_add_mid_label_to_table (GtkWidget *table, gchar *text,
	gfloat align, gint left, gint right, gint top, gint bottom) G_GNUC_MALLOC;
//GtkWidget *e2_widget_add_entry_to_table (GtkWidget *table, gchar *init_text,
//	gint left, gint right, gint top, gint bottom);
//GtkWidget *e2_widget_add_framed_table (GtkWidget *box, gchar *title, gint rows,
//	gint cols, gboolean exp, guint pad);
//GtkWidget *e2_widget_add_framed_widget(GtkWidget *box, gchar *title,
//	GtkWidget *widget, gboolean exp, guint pad);
GtkWidget *e2_widget_add_frame (GtkWidget *box, gboolean fill, guint pad,
	gchar *title, gboolean strech) G_GNUC_MALLOC;
GtkWidget *e2_widget_add_eventbox (GtkWidget *box, gboolean fill, guint pad) G_GNUC_MALLOC;
GtkWidget *e2_widget_get_notebook (
	void (*func)(/*GtkNotebook*,GtkNotebookPage*(2)|GtkWidget*(3),guint,gpointer*/), gpointer data) G_GNUC_MALLOC;
GtkWidget *e2_widget_add_notebook (GtkWidget *box, gboolean fill, guint pad,
	void (*func)(/*GtkNotebook*,GtkNotebookPage*(2)|GtkWidget*(3),guint,gpointer*/), gpointer data) G_GNUC_MALLOC;
GtkWidget *e2_widget_add_notebook_page (//GtkWidget *dialog,
	GtkWidget *notebook, gchar *name, GtkPolicyType swpolicy) G_GNUC_MALLOC;
void e2_widget_get_font_pixels (GtkWidget *widget, int *width, int *height);
void e2_widget_set_font (GtkWidget *widget, const gchar *font_string);
GtkWidget *e2_widget_add_tied_check_button
	(GtkWidget *box, E2_OptionSet *boolset, GtkWidget *controller) G_GNUC_MALLOC;
void e2_widget_handle_depends (GtkWidget *widget, E2_OptionSet *boolset);

#ifdef E2_ASSISTED
void e2_widget_set_label_relations (GtkWidget *label, GtkWidget *widget);
AtkObject *e2_widget_get_accessible (GtkWidget *widget, const gchar *name,
	const gchar *desc, AtkRole role);
#endif

#endif //ndef __E2_WIDGET_H__
