/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef __GTKUTILS_H__
#define __GTKUTILS_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <glib.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <stdlib.h>
#if HAVE_WCHAR_H
#  include <wchar.h>
#endif

#ifdef MAEMO
#ifdef CHINOOK
#include <hildon/hildon-program.h>
#else
#include <hildon-widgets/hildon-program.h>
#include <hildon-widgets/hildon-window.h>
#endif
#include "main.h"
#endif

#include "gtkcmctree.h"

#define GTK_EVENTS_FLUSH() \
{ \
	while (gtk_events_pending()) \
		gtk_main_iteration(); \
}

#define GTK_WIDGET_PTR(wid)	(*(GtkWidget **)wid)

#define GTKUT_CTREE_NODE_SET_ROW_DATA(node, d) \
{ \
	GTK_CMCTREE_ROW(node)->row.data = d; \
}

#define GTKUT_CTREE_NODE_GET_ROW_DATA(node) \
	(GTK_CMCTREE_ROW(node)->row.data)

#define GTKUT_CTREE_REFRESH(clist) \
	GTK_CMCLIST_GET_CLASS(clist)->refresh(clist)

gboolean gtkut_get_font_size		(GtkWidget	*widget,
					 gint		*width,
					 gint		*height);

void gtkut_convert_int_to_gdk_color	(gint		 rgbvalue,
					 GdkColor	*color);
gint gtkut_convert_gdk_color_to_int	(GdkColor 	*color);

void gtkut_stock_button_add_help(GtkWidget *bbox, GtkWidget **help_btn);

void gtkut_stock_button_set_create_with_help(GtkWidget **bbox,
		GtkWidget **help_button,
		GtkWidget **button1, const gchar *label1,
		GtkWidget **button2, const gchar *label2,
		GtkWidget **button3, const gchar *label3);

void gtkut_stock_button_set_create	(GtkWidget	**bbox,
					 GtkWidget	**button1,
					 const gchar	 *label1,
					 GtkWidget	**button2,
					 const gchar	 *label2,
					 GtkWidget	**button3,
					 const gchar	 *label3);

void gtkut_stock_with_text_button_set_create(GtkWidget **bbox,
				   GtkWidget **button1, const gchar *label1, const gchar *text1,
				   GtkWidget **button2, const gchar *label2, const gchar *text2,
				   GtkWidget **button3, const gchar *label3, const gchar *text3);

void gtkut_ctree_node_move_if_on_the_edge
					(GtkCMCTree	*ctree,
					 GtkCMCTreeNode	*node,
					 gint		 _row);
gint gtkut_ctree_get_nth_from_node	(GtkCMCTree	*ctree,
					 GtkCMCTreeNode	*node);
GtkCMCTreeNode *gtkut_ctree_node_next	(GtkCMCTree	*ctree,
					 GtkCMCTreeNode	*node);
GtkCMCTreeNode *gtkut_ctree_node_prev	(GtkCMCTree	*ctree,
					 GtkCMCTreeNode	*node);
gboolean gtkut_ctree_node_is_selected	(GtkCMCTree	*ctree,
					 GtkCMCTreeNode	*node);
GtkCMCTreeNode *gtkut_ctree_find_collapsed_parent
					(GtkCMCTree	*ctree,
					 GtkCMCTreeNode	*node);
void gtkut_ctree_expand_parent_all	(GtkCMCTree	*ctree,
					 GtkCMCTreeNode	*node);
gboolean gtkut_ctree_node_is_parent	(GtkCMCTreeNode 	*parent, 
					 GtkCMCTreeNode 	*node);
void gtkut_ctree_set_focus_row		(GtkCMCTree	*ctree,
					 GtkCMCTreeNode	*node);

void gtkut_clist_set_focus_row		(GtkCMCList	*clist,
					 gint		 row);

void gtkut_container_remove		(GtkContainer	*container,
					 GtkWidget	*widget);

gchar *gtkut_text_view_get_selection	(GtkTextView	*textview);
void gtkut_text_view_set_position		(GtkTextView *text, gint pos);
gboolean gtkut_text_view_search_string	(GtkTextView *text, const gchar *str,
					gboolean case_sens);
gboolean gtkut_text_view_search_string_backward	(GtkTextView *text, const gchar *str,
					gboolean case_sens);

GtkWidget *label_window_create(const gchar *str);
void label_window_destroy(GtkWidget *widget);

void gtkut_window_popup			(GtkWidget	*window);
GtkWidget *gtkut_window_new		(GtkWindowType	 type,
					 const gchar	*class);


void gtkut_widget_get_uposition		(GtkWidget	*widget,
					 gint		*px,
					 gint		*py);
void gtkut_widget_draw_now		(GtkWidget	*widget);
void gtkut_widget_init			(void);

void gtkut_widget_set_app_icon		(GtkWidget	*widget);
void gtkut_widget_set_composer_icon	(GtkWidget	*widget);

gboolean gtkut_widget_get_app_paintable	(GtkWidget	*widget);
gboolean gtkut_widget_get_can_focus	(GtkWidget	*widget);
gboolean gtkut_widget_get_has_window	(GtkWidget	*widget);
gboolean gtkut_widget_get_mapped	(GtkWidget	*widget);
gboolean gtkut_widget_get_realized	(GtkWidget	*widget);
gboolean gtkut_widget_get_sensitive	(GtkWidget	*widget);
GtkStateType gtkut_widget_get_state	(GtkWidget	*widget);
gboolean gtkut_widget_get_visible	(GtkWidget	*widget);
gboolean gtkut_widget_has_grab		(GtkWidget	*widget);
gboolean gtkut_widget_has_focus		(GtkWidget	*widget);
gboolean gtkut_widget_is_drawable	(GtkWidget	*widget);
gboolean gtkut_widget_is_sensitive	(GtkWidget	*widget);
void gtkut_widget_set_mapped            (GtkWidget *widget, gboolean mapped);
void gtkut_widget_set_realized          (GtkWidget *widget, gboolean realized);
void gtkut_widget_set_can_default       (GtkWidget *widget, gboolean can_default);
void gtkut_widget_set_receives_default  (GtkWidget *widget, gboolean receives_default);
void gtkut_widget_set_can_focus         (GtkWidget *widget, gboolean can_focus);
void gtkut_widget_set_has_window        (GtkWidget *widget, gboolean has_window);


GtkWidget *gtkut_account_menu_new	(GList			*ac_list,
				  	 GCallback	 	 callback,
					 gpointer		 data);

void gtkut_set_widget_bgcolor_rgb	(GtkWidget 	*widget,
					 guint 		 rgbvalue);

void gtkut_widget_set_small_font_size(GtkWidget *widget);
GtkWidget *gtkut_get_focused_child	(GtkContainer 	*parent);

GtkWidget *gtkut_get_browse_file_btn(const gchar *label);
GtkWidget *gtkut_get_browse_directory_btn(const gchar *label);
GtkWidget *gtkut_get_replace_btn(const gchar *label);
GtkWidget *gtkut_get_options_frame(GtkWidget *box, GtkWidget **frame, const gchar *frame_label);
#if HAVE_LIBCOMPFACE
GtkWidget *xface_get_from_header(const gchar *o_xface, GdkColor *background,
				 GdkWindow *window);
#endif
gboolean get_tag_range(GtkTextIter *iter,
				       GtkTextTag *tag,
				       GtkTextIter *start_iter,
				       GtkTextIter *end_iter);

GtkWidget *face_get_from_header(const gchar *o_face);
GtkWidget *gtkut_get_link_btn(GtkWidget *window, const gchar *url, const gchar *label);

GtkWidget *gtkut_sc_combobox_create(GtkWidget *eventbox, gboolean focus_on_click);
void gtkutils_scroll_one_line	(GtkWidget *widget, 
				 GtkAdjustment *vadj, 
				 gboolean up);
gboolean gtkutils_scroll_page	(GtkWidget *widget, 
				 GtkAdjustment *vadj, 
				 gboolean up);

gboolean gtkut_tree_model_text_iter_prev(GtkTreeModel *model,
				 GtkTreeIter *iter,
				 const gchar* text);
gboolean gtkut_tree_model_get_iter_last(GtkTreeModel *model,
				 GtkTreeIter *iter);

gint gtkut_list_view_get_selected_row(GtkWidget *list_view);
gboolean gtkut_list_view_select_row(GtkWidget *list, gint row);

GtkUIManager *gtkut_create_ui_manager(void);
GtkUIManager *gtkut_ui_manager(void);

gint
claws_input_add    (gint	      source,
		    GdkInputCondition condition,
		    GdkInputFunction  function,
		    gpointer	      data,
		    gboolean          is_sock);
#if GTK_CHECK_VERSION(2,12,0)
#define CLAWS_TIP_DECL() {}
#define CLAWS_SET_TIP(widget,tip) { 					\
	if (tip != NULL)						\
		gtk_widget_set_tooltip_text(GTK_WIDGET(widget), tip); 	\
	else								\
		gtk_widget_set_has_tooltip(GTK_WIDGET(widget), FALSE);	\
}

#else
#define CLAWS_TIP_DECL() \
	GtkTooltips *tips = gtk_tooltips_new();

#define CLAWS_SET_TIP(widget,tip) { 				\
	gtk_tooltips_set_tip(GTK_TOOLTIPS(tips), widget, 	\
			    tip, NULL);				\
}

#endif
#endif /* __GTKUTILS_H__ */
