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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#if !GTK_CHECK_VERSION(3, 0, 0)
#include "gtk/gtksctree.h"
#endif
#include <stdlib.h>
#include <stdarg.h>
#include <sys/stat.h>

#include "combobox.h"

#if HAVE_LIBCOMPFACE
#  include <compface.h>
#endif

#if HAVE_LIBCOMPFACE
#define XPM_XFACE_HEIGHT	(HEIGHT + 3)  /* 3 = 1 header + 2 colors */
#endif

#if (HAVE_WCTYPE_H && HAVE_WCHAR_H)
#  include <wchar.h>
#  include <wctype.h>
#endif

#include "defs.h"
#include "gtkutils.h"
#include "utils.h"
#if !GTK_CHECK_VERSION(3, 0, 0)
#include "gtksctree.h"
#endif
#include "codeconv.h"
#include "stock_pixmap.h"
#include "menu.h"
#include "prefs_account.h"
#include "prefs_common.h"
#include "manage_window.h"
#include "base64.h"
#include "manual.h"
#include "combobox.h"

gboolean gtkut_get_font_size(GtkWidget *widget,
			     gint *width, gint *height)
{
	PangoLayout *layout;
	const gchar *str = "Abcdef";

	cm_return_val_if_fail(GTK_IS_WIDGET(widget), FALSE);

	layout = gtk_widget_create_pango_layout(widget, str);
	cm_return_val_if_fail(layout, FALSE);
	pango_layout_get_pixel_size(layout, width, height);
	if (width)
		*width = *width / g_utf8_strlen(str, -1);
	g_object_unref(layout);

	return TRUE;
}

void gtkut_widget_set_small_font_size(GtkWidget *widget)
{
	PangoFontDescription *font_desc;
	gint size;

	cm_return_if_fail(widget != NULL);
	cm_return_if_fail(gtk_widget_get_style(widget) != NULL);

	if (prefs_common.derive_from_normal_font || !SMALL_FONT) {
		font_desc = pango_font_description_from_string(NORMAL_FONT);
		size = pango_font_description_get_size(font_desc);
		pango_font_description_set_size(font_desc, size * PANGO_SCALE_SMALL);
		gtk_widget_modify_font(widget, font_desc);
		pango_font_description_free(font_desc);
	} else {
		font_desc = pango_font_description_from_string(SMALL_FONT);
		gtk_widget_modify_font(widget, font_desc);
		pango_font_description_free(font_desc);
	}
}

void gtkut_convert_int_to_gdk_color(gint rgbvalue, GdkColor *color)
{
	cm_return_if_fail(color != NULL);

	color->pixel = 0L;
	color->red   = (int) (((gdouble)((rgbvalue & 0xff0000) >> 16) / 255.0) * 65535.0);
	color->green = (int) (((gdouble)((rgbvalue & 0x00ff00) >>  8) / 255.0) * 65535.0);
	color->blue  = (int) (((gdouble) (rgbvalue & 0x0000ff)        / 255.0) * 65535.0);
}

#define CL(x)	(((gulong) (x) >> (gulong) 8) & 0xFFUL)
#define RGB_FROM_GDK_COLOR(c) \
	((CL(c->red)   << (gulong) 16) | \
	 (CL(c->green) << (gulong)  8) | \
	 (CL(c->blue)))

gint gtkut_convert_gdk_color_to_int(GdkColor *color)
{
	return RGB_FROM_GDK_COLOR(color);
}

void gtkut_stock_button_add_help(GtkWidget *bbox, GtkWidget **help_btn)
{
	cm_return_if_fail(bbox != NULL);

	*help_btn = gtk_button_new_from_stock(GTK_STOCK_HELP);

	gtkut_widget_set_can_default(*help_btn, TRUE);
	gtk_box_pack_end(GTK_BOX (bbox), *help_btn, TRUE, TRUE, 0);
	gtk_button_box_set_child_secondary(GTK_BUTTON_BOX (bbox),
			*help_btn, TRUE);
	gtk_widget_set_sensitive(*help_btn,
			manual_available(MANUAL_MANUAL_CLAWS));
	gtk_widget_show(*help_btn);
}

void gtkut_stock_button_set_create_with_help(GtkWidget **bbox,
		GtkWidget **help_button,
		GtkWidget **button1, const gchar *label1,
		GtkWidget **button2, const gchar *label2,
		GtkWidget **button3, const gchar *label3)
{
	cm_return_if_fail(bbox != NULL);
	cm_return_if_fail(button1 != NULL);

	gtkut_stock_button_set_create(bbox, button1, label1,
			button2, label2, button3, label3);

	gtkut_stock_button_add_help(*bbox, help_button);
}

void gtkut_stock_button_set_create(GtkWidget **bbox,
				   GtkWidget **button1, const gchar *label1,
				   GtkWidget **button2, const gchar *label2,
				   GtkWidget **button3, const gchar *label3)
{
	cm_return_if_fail(bbox != NULL);
	cm_return_if_fail(button1 != NULL);

	*bbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(*bbox), GTK_BUTTONBOX_END);
	gtk_box_set_spacing(GTK_BOX(*bbox), 5);

	*button1 = gtk_button_new_from_stock(label1);
	gtkut_widget_set_can_default(*button1, TRUE);
	gtk_box_pack_start(GTK_BOX(*bbox), *button1, TRUE, TRUE, 0);
	gtk_widget_show(*button1);

	if (button2) {
		*button2 = gtk_button_new_from_stock(label2);
		gtkut_widget_set_can_default(*button2, TRUE);
		gtk_box_pack_start(GTK_BOX(*bbox), *button2, TRUE, TRUE, 0);
		gtk_widget_show(*button2);
	}

	if (button3) {
		*button3 = gtk_button_new_from_stock(label3);
		gtkut_widget_set_can_default(*button3, TRUE);
		gtk_box_pack_start(GTK_BOX(*bbox), *button3, TRUE, TRUE, 0);
		gtk_widget_show(*button3);
	}
}

void gtkut_stock_with_text_button_set_create(GtkWidget **bbox,
				   GtkWidget **button1, const gchar *label1, const gchar *text1,
				   GtkWidget **button2, const gchar *label2, const gchar *text2,
				   GtkWidget **button3, const gchar *label3, const gchar *text3)
{
	cm_return_if_fail(bbox != NULL);
	cm_return_if_fail(button1 != NULL);

	*bbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(*bbox), GTK_BUTTONBOX_END);
	gtk_box_set_spacing(GTK_BOX(*bbox), 5);

	*button1 = gtk_button_new_with_mnemonic(text1);
	gtk_button_set_image(GTK_BUTTON(*button1),
		gtk_image_new_from_stock(label1, GTK_ICON_SIZE_BUTTON));
	gtkut_widget_set_can_default(*button1, TRUE);
	gtk_box_pack_start(GTK_BOX(*bbox), *button1, TRUE, TRUE, 0);
	gtk_widget_show(*button1);

	if (button2) {
		*button2 = gtk_button_new_with_mnemonic(text2);
		gtk_button_set_image(GTK_BUTTON(*button2),
			gtk_image_new_from_stock(label2, GTK_ICON_SIZE_BUTTON));
		gtkut_widget_set_can_default(*button2, TRUE);
		gtk_box_pack_start(GTK_BOX(*bbox), *button2, TRUE, TRUE, 0);
		gtk_widget_show(*button2);
	}

	if (button3) {
		*button3 = gtk_button_new_with_mnemonic(text3);
		gtk_button_set_image(GTK_BUTTON(*button3),
			gtk_image_new_from_stock(label3, GTK_ICON_SIZE_BUTTON));
		gtkut_widget_set_can_default(*button3, TRUE);
		gtk_box_pack_start(GTK_BOX(*bbox), *button3, TRUE, TRUE, 0);
		gtk_widget_show(*button3);
	}
}

#define CELL_SPACING 1
#define ROW_TOP_YPIXEL(clist, row) (((clist)->row_height * (row)) + \
				    (((row) + 1) * CELL_SPACING) + \
				    (clist)->voffset)
#define ROW_FROM_YPIXEL(clist, y) (((y) - (clist)->voffset) / \
				   ((clist)->row_height + CELL_SPACING))

void gtkut_ctree_node_move_if_on_the_edge(GtkCMCTree *ctree, GtkCMCTreeNode *node, gint _row)
{
	GtkCMCList *clist = GTK_CMCLIST(ctree);
	gint row;
	GtkVisibility row_visibility, prev_row_visibility, next_row_visibility;

	cm_return_if_fail(ctree != NULL);
	cm_return_if_fail(node != NULL);

	row = (_row != -1 ? _row : g_list_position(clist->row_list, (GList *)node));

	if (row < 0 || row >= clist->rows || clist->row_height == 0) return;
	row_visibility = gtk_cmclist_row_is_visible(clist, row);
	prev_row_visibility = gtk_cmclist_row_is_visible(clist, row - 1);
	next_row_visibility = gtk_cmclist_row_is_visible(clist, row + 1);

	if (row_visibility == GTK_VISIBILITY_NONE) {
		gtk_cmclist_moveto(clist, row, -1, 0.5, 0);
		return;
	}
	if (row_visibility == GTK_VISIBILITY_FULL &&
	    prev_row_visibility == GTK_VISIBILITY_FULL &&
	    next_row_visibility == GTK_VISIBILITY_FULL)
		return;
	if (prev_row_visibility != GTK_VISIBILITY_FULL &&
	    next_row_visibility != GTK_VISIBILITY_FULL)
		return;

	if (prev_row_visibility != GTK_VISIBILITY_FULL) {
		gtk_cmclist_moveto(clist, row, -1, 0.2, 0);
		return;
	}
	if (next_row_visibility != GTK_VISIBILITY_FULL) {
		gtk_cmclist_moveto(clist, row, -1, 0.8, 0);
		return;
	}
}

#undef CELL_SPACING
#undef ROW_TOP_YPIXEL
#undef ROW_FROM_YPIXEL

gint gtkut_ctree_get_nth_from_node(GtkCMCTree *ctree, GtkCMCTreeNode *node)
{
	cm_return_val_if_fail(ctree != NULL, -1);
	cm_return_val_if_fail(node != NULL, -1);

	return g_list_position(GTK_CMCLIST(ctree)->row_list, (GList *)node);
}

/* get the next node, including the invisible one */
GtkCMCTreeNode *gtkut_ctree_node_next(GtkCMCTree *ctree, GtkCMCTreeNode *node)
{
	GtkCMCTreeNode *parent;

	if (!node) return NULL;

	if (GTK_CMCTREE_ROW(node)->children)
		return GTK_CMCTREE_ROW(node)->children;

	if (GTK_CMCTREE_ROW(node)->sibling)
		return GTK_CMCTREE_ROW(node)->sibling;

	for (parent = GTK_CMCTREE_ROW(node)->parent; parent != NULL;
	     parent = GTK_CMCTREE_ROW(parent)->parent) {
		if (GTK_CMCTREE_ROW(parent)->sibling)
			return GTK_CMCTREE_ROW(parent)->sibling;
	}

	return NULL;
}

/* get the previous node, including the invisible one */
GtkCMCTreeNode *gtkut_ctree_node_prev(GtkCMCTree *ctree, GtkCMCTreeNode *node)
{
	GtkCMCTreeNode *prev;
	GtkCMCTreeNode *child;

	if (!node) return NULL;

	prev = GTK_CMCTREE_NODE_PREV(node);
	if (prev == GTK_CMCTREE_ROW(node)->parent)
		return prev;

	child = prev;
	while (GTK_CMCTREE_ROW(child)->children != NULL) {
		child = GTK_CMCTREE_ROW(child)->children;
		while (GTK_CMCTREE_ROW(child)->sibling != NULL)
			child = GTK_CMCTREE_ROW(child)->sibling;
	}

	return child;
}

gboolean gtkut_ctree_node_is_selected(GtkCMCTree *ctree, GtkCMCTreeNode *node)
{
	GtkCMCList *clist = GTK_CMCLIST(ctree);
	GList *cur;

	for (cur = clist->selection; cur != NULL; cur = cur->next) {
		if (node == GTK_CMCTREE_NODE(cur->data))
			return TRUE;
	}

	return FALSE;
}

GtkCMCTreeNode *gtkut_ctree_find_collapsed_parent(GtkCMCTree *ctree,
						GtkCMCTreeNode *node)
{
	if (!node) return NULL;

	while ((node = GTK_CMCTREE_ROW(node)->parent) != NULL) {
		if (!GTK_CMCTREE_ROW(node)->expanded)
			return node;
	}

	return NULL;
}

void gtkut_ctree_expand_parent_all(GtkCMCTree *ctree, GtkCMCTreeNode *node)
{
	while ((node = gtkut_ctree_find_collapsed_parent(ctree, node)) != NULL)
		gtk_cmctree_expand(ctree, node);
}

gboolean gtkut_ctree_node_is_parent(GtkCMCTreeNode *parent, GtkCMCTreeNode *node)
{
	GtkCMCTreeNode *tmp;
	cm_return_val_if_fail(node != NULL, FALSE);
	cm_return_val_if_fail(parent != NULL, FALSE);
	tmp = node;
	
	while (tmp) {
		if(GTK_CMCTREE_ROW(tmp)->parent && GTK_CMCTREE_ROW(tmp)->parent == parent)
			return TRUE;
		tmp = GTK_CMCTREE_ROW(tmp)->parent;
	}
	
	return FALSE;
}

void gtkut_ctree_set_focus_row(GtkCMCTree *ctree, GtkCMCTreeNode *node)
{
	if (node == NULL)
		return;
	gtkut_clist_set_focus_row(GTK_CMCLIST(ctree),
				  gtkut_ctree_get_nth_from_node(ctree, node));
}

void gtkut_clist_set_focus_row(GtkCMCList *clist, gint row)
{
	clist->focus_row = row;
	GTKUT_CTREE_REFRESH(clist);
}

void gtkut_container_remove(GtkContainer *container, GtkWidget *widget)
{
	gtk_container_remove(container, widget);
}

static gboolean gtkut_text_buffer_match_string(GtkTextBuffer *textbuf,
					const GtkTextIter *iter,
					gunichar *wcs, gint len,
					gboolean case_sens)
{
	GtkTextIter start_iter, end_iter;
	gchar *utf8str, *p;
	gint match_count;

	start_iter = end_iter = *iter;
	gtk_text_iter_forward_chars(&end_iter, len);

	utf8str = gtk_text_buffer_get_text(textbuf, &start_iter, &end_iter,
					   FALSE);
	if (!utf8str) return FALSE;

	if ((gint)g_utf8_strlen(utf8str, -1) != len) {
		g_free(utf8str);
		return FALSE;
	}

	for (p = utf8str, match_count = 0;
	     *p != '\0' && match_count < len;
	     p = g_utf8_next_char(p), match_count++) {
		gunichar wc;

		wc = g_utf8_get_char(p);

		if (case_sens) {
			if (wc != wcs[match_count])
				break;
		} else {
			if (g_unichar_tolower(wc) !=
			    g_unichar_tolower(wcs[match_count]))
				break;
		}
	}

	g_free(utf8str);

	if (match_count == len)
		return TRUE;
	else
		return FALSE;
}

static gboolean gtkut_text_buffer_find(GtkTextBuffer *buffer, const GtkTextIter *iter,
				const gchar *str, gboolean case_sens,
				GtkTextIter *match_pos)
{
	gunichar *wcs;
	gint len;
	glong items_read = 0, items_written = 0;
	GError *error = NULL;
	GtkTextIter iter_;
	gboolean found = FALSE;

	wcs = g_utf8_to_ucs4(str, -1, &items_read, &items_written, &error);
	if (error != NULL) {
		g_warning("An error occurred while converting a string from UTF-8 to UCS-4: %s\n",
			  error->message);
		g_error_free(error);
	}
	if (!wcs || items_written <= 0) return FALSE;
	len = (gint)items_written;

	iter_ = *iter;
	do {
		found = gtkut_text_buffer_match_string
			(buffer, &iter_, wcs, len, case_sens);
		if (found) {
			*match_pos = iter_;
			break;
		}
	} while (gtk_text_iter_forward_char(&iter_));

	g_free(wcs);

	return found;
}

static gboolean gtkut_text_buffer_find_backward(GtkTextBuffer *buffer,
					 const GtkTextIter *iter,
					 const gchar *str, gboolean case_sens,
					 GtkTextIter *match_pos)
{
	gunichar *wcs;
	gint len;
	glong items_read = 0, items_written = 0;
	GError *error = NULL;
	GtkTextIter iter_;
	gboolean found = FALSE;

	wcs = g_utf8_to_ucs4(str, -1, &items_read, &items_written, &error);
	if (error != NULL) {
		g_warning("An error occurred while converting a string from UTF-8 to UCS-4: %s\n",
			  error->message);
		g_error_free(error);
	}
	if (!wcs || items_written <= 0) return FALSE;
	len = (gint)items_written;

	iter_ = *iter;
	while (gtk_text_iter_backward_char(&iter_)) {
		found = gtkut_text_buffer_match_string
			(buffer, &iter_, wcs, len, case_sens);
		if (found) {
			*match_pos = iter_;
			break;
		}
	}

	g_free(wcs);

	return found;
}

gchar *gtkut_text_view_get_selection(GtkTextView *textview)
{
	GtkTextBuffer *buffer;
	GtkTextIter start_iter, end_iter;
	gboolean found;

	cm_return_val_if_fail(GTK_IS_TEXT_VIEW(textview), NULL);

	buffer = gtk_text_view_get_buffer(textview);
	found = gtk_text_buffer_get_selection_bounds(buffer,
						     &start_iter,
						     &end_iter);
	if (found)
		return gtk_text_buffer_get_text(buffer, &start_iter, &end_iter,
						FALSE);
	else
		return NULL;
}


void gtkut_text_view_set_position(GtkTextView *text, gint pos)
{
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	GtkTextMark *mark;

	cm_return_if_fail(text != NULL);

	buffer = gtk_text_view_get_buffer(text);

	gtk_text_buffer_get_iter_at_offset(buffer, &iter, pos);
	gtk_text_buffer_place_cursor(buffer, &iter);
	mark = gtk_text_buffer_create_mark(buffer, NULL, &iter, TRUE);
	gtk_text_view_scroll_to_mark(text, mark, 0.0, FALSE, 0.0, 0.0);
}

gboolean gtkut_text_view_search_string(GtkTextView *text, const gchar *str,
					gboolean case_sens)
{
	GtkTextBuffer *buffer;
	GtkTextIter iter, match_pos;
	GtkTextMark *mark;
	gint len;

	cm_return_val_if_fail(text != NULL, FALSE);
	cm_return_val_if_fail(str != NULL, FALSE);

	buffer = gtk_text_view_get_buffer(text);

	len = g_utf8_strlen(str, -1);
	cm_return_val_if_fail(len >= 0, FALSE);

	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);

	if (gtkut_text_buffer_find(buffer, &iter, str, case_sens,
				   &match_pos)) {
		GtkTextIter end = match_pos;

		gtk_text_iter_forward_chars(&end, len);
		/* place "insert" at the last character */
		gtk_text_buffer_select_range(buffer, &end, &match_pos);
		gtk_text_view_scroll_to_mark(text, mark, 0.0, TRUE, 0.0, 0.5);
		return TRUE;
	}

	return FALSE;
}

gboolean gtkut_text_view_search_string_backward(GtkTextView *text, const gchar *str,
					gboolean case_sens)
{
	GtkTextBuffer *buffer;
	GtkTextIter iter, match_pos;
	GtkTextMark *mark;
	gint len;

	cm_return_val_if_fail(text != NULL, FALSE);
	cm_return_val_if_fail(str != NULL, FALSE);

	buffer = gtk_text_view_get_buffer(text);

	len = g_utf8_strlen(str, -1);
	cm_return_val_if_fail(len >= 0, FALSE);

	mark = gtk_text_buffer_get_insert(buffer);
	gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);

	if (gtkut_text_buffer_find_backward(buffer, &iter, str, case_sens,
					    &match_pos)) {
		GtkTextIter end = match_pos;

		gtk_text_iter_forward_chars(&end, len);
		gtk_text_buffer_select_range(buffer, &match_pos, &end);
		gtk_text_view_scroll_to_mark(text, mark, 0.0, TRUE, 0.0, 0.5);
		return TRUE;
	}

	return FALSE;
}

void gtkut_window_popup(GtkWidget *window)
{
	GdkWindow *gdkwin;
	gint x, y, sx, sy, new_x, new_y;

	gdkwin = gtk_widget_get_window(window);

	cm_return_if_fail(window != NULL);
	cm_return_if_fail(gdkwin != NULL);

	sx = gdk_screen_width();
	sy = gdk_screen_height();

	gdk_window_get_origin(gdkwin, &x, &y);
	new_x = x % sx; if (new_x < 0) new_x = 0;
	new_y = y % sy; if (new_y < 0) new_y = 0;
	if (new_x != x || new_y != y)
		gdk_window_move(gdkwin, new_x, new_y);

	gtk_window_set_skip_taskbar_hint(GTK_WINDOW(window), FALSE);
	gtk_window_present_with_time(GTK_WINDOW(window), time(NULL));
}

void gtkut_widget_get_uposition(GtkWidget *widget, gint *px, gint *py)
{
	GdkWindow *gdkwin;
	gint x, y;
	gint sx, sy;

	gdkwin = gtk_widget_get_window(widget);

	cm_return_if_fail(widget != NULL);
	cm_return_if_fail(gdkwin != NULL);

	sx = gdk_screen_width();
	sy = gdk_screen_height();

	/* gdk_window_get_root_origin ever return *rootwindow*'s position */
	gdk_window_get_root_origin(gdkwin, &x, &y);

	x %= sx; if (x < 0) x = 0;
	y %= sy; if (y < 0) y = 0;
	*px = x;
	*py = y;
}

void gtkut_widget_draw_now(GtkWidget *widget)
{
	if (widget && gtk_widget_get_visible(widget) && gtk_widget_is_drawable(widget))
		gdk_window_process_updates(gtk_widget_get_window(widget), FALSE);
}

static void gtkut_clist_bindings_add(GtkWidget *clist)
{
	GtkBindingSet *binding_set;

	binding_set = gtk_binding_set_by_class
		(GTK_CMCLIST_GET_CLASS(clist));

	gtk_binding_entry_add_signal(binding_set, GDK_KEY_n, GDK_CONTROL_MASK,
				     "scroll_vertical", 2,
				     G_TYPE_ENUM, GTK_SCROLL_STEP_FORWARD,
				     G_TYPE_FLOAT, 0.0);
	gtk_binding_entry_add_signal(binding_set, GDK_p, GDK_CONTROL_MASK,
				     "scroll_vertical", 2,
				     G_TYPE_ENUM, GTK_SCROLL_STEP_BACKWARD,
				     G_TYPE_FLOAT, 0.0);
}

void gtkut_widget_init(void)
{
	GtkWidget *clist;

	clist = gtk_cmclist_new(1);
	g_object_ref(G_OBJECT(clist));
#if GLIB_CHECK_VERSION(2,10,0)
	g_object_ref_sink (G_OBJECT(clist));
#else
	gtk_object_ref (G_OBJECT(clist));
	gtk_object_sink (G_OBJECT(clist));
#endif
	gtkut_clist_bindings_add(clist);
	g_object_unref(G_OBJECT(clist));

	clist = gtk_cmctree_new(1, 0);
	g_object_ref(G_OBJECT(clist));
#if GLIB_CHECK_VERSION(2,10,0)
	g_object_ref_sink (G_OBJECT(clist));
#else
	gtk_object_ref (G_OBJECT(clist));
	gtk_object_sink (G_OBJECT(clist));
#endif
	gtkut_clist_bindings_add(clist);
	g_object_unref(G_OBJECT(clist));

	clist = gtk_sctree_new_with_titles(1, 0, NULL);
	g_object_ref(G_OBJECT(clist));
#if GLIB_CHECK_VERSION(2,10,0)
	g_object_ref_sink (G_OBJECT(clist));
#else
	gtk_object_ref (G_OBJECT(clist));
	gtk_object_sink (G_OBJECT(clist));
#endif
	gtkut_clist_bindings_add(clist);
	g_object_unref(G_OBJECT(clist));
}

void gtkut_widget_set_app_icon(GtkWidget *widget)
{
	static GdkPixbuf *icon = NULL;
	
	cm_return_if_fail(widget != NULL);
	cm_return_if_fail(gtk_widget_get_window(widget) != NULL);
	if (!icon) {
		stock_pixbuf_gdk(widget, STOCK_PIXMAP_CLAWS_MAIL_ICON, &icon);
	}		
	if (icon)
		gtk_window_set_icon(GTK_WINDOW(widget), icon);
}

void gtkut_widget_set_composer_icon(GtkWidget *widget)
{
	static GdkPixbuf *icon = NULL;
	
	cm_return_if_fail(widget != NULL);
	cm_return_if_fail(gtk_widget_get_window(widget) != NULL);
	if (!icon) {
		stock_pixbuf_gdk(widget, STOCK_PIXMAP_MAIL_COMPOSE, &icon);
	}		
	if (icon)
		gtk_window_set_icon(GTK_WINDOW(widget), icon);
}

static gboolean move_bar = FALSE;
static gint move_bar_id = -1;

static gboolean move_bar_cb(gpointer data)
{
	GtkWidget *w = (GtkWidget *)data;
	if (!move_bar)
		return FALSE;

	if (!GTK_IS_PROGRESS_BAR(w)) {
		return FALSE;
	}

	gtk_progress_bar_pulse(GTK_PROGRESS_BAR(w));
	GTK_EVENTS_FLUSH();
	return TRUE;
}

GtkWidget *label_window_create(const gchar *str)
{
	GtkWidget *window;
	GtkWidget *label, *vbox, *hbox;
	GtkWidget *wait_progress = gtk_progress_bar_new();

	window = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "gtkutils");
	gtk_widget_set_size_request(window, 380, 70);
	gtk_container_set_border_width(GTK_CONTAINER(window), 8);
	gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
	gtk_window_set_title(GTK_WINDOW(window), str);
	gtk_window_set_modal(GTK_WINDOW(window), TRUE);
	gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
	manage_window_set_transient(GTK_WINDOW(window));

	label = gtk_label_new(str);
	
	vbox = gtk_vbox_new(FALSE, 6);
	hbox = gtk_hbox_new(FALSE, 6);
	gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, FALSE, 0);
	hbox = gtk_hbox_new(FALSE, 6);
	gtk_box_pack_start(GTK_BOX(hbox), wait_progress, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	
	gtk_container_add(GTK_CONTAINER(window), vbox);
	gtk_label_set_line_wrap(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment(GTK_MISC(label), 0.5, 0.5);
	gtk_widget_show_all(vbox);

	gtk_widget_show_now(window);
	
	if (move_bar_id == -1) {
		move_bar_id = g_timeout_add(200, move_bar_cb, wait_progress);
		move_bar = TRUE;
	}

	GTK_EVENTS_FLUSH();

	return window;
}

void label_window_destroy(GtkWidget *window)
{
	move_bar = FALSE;
	g_source_remove(move_bar_id);
	move_bar_id = -1;
	GTK_EVENTS_FLUSH();
	gtk_widget_destroy(window);	
}

GtkWidget *gtkut_account_menu_new(GList			*ac_list,
					GCallback		callback,
				  gpointer		data)
{
	GList *cur_ac;
	GtkWidget *optmenu;
	GtkListStore *menu;
	GtkTreeIter iter;
	PrefsAccount *account;
	gchar *name;
	
	cm_return_val_if_fail(ac_list != NULL, NULL);

	optmenu = gtkut_sc_combobox_create(NULL, FALSE);
	menu = GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(optmenu)));

	for (cur_ac = ac_list; cur_ac != NULL; cur_ac = cur_ac->next) {
		account = (PrefsAccount *) cur_ac->data;
		if (account->name)
			name = g_strdup_printf("%s: %s <%s>",
					       account->account_name,
					       account->name,
					       account->address);
		else
			name = g_strdup_printf("%s: %s",
					       account->account_name,
					       account->address);
		COMBOBOX_ADD_ESCAPED(menu, name, account->account_id);
		g_free(name);
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX(optmenu), 0);

	if( callback != NULL )
		g_signal_connect(G_OBJECT(optmenu), "changed", callback, data);

	return optmenu;
}

void gtkut_set_widget_bgcolor_rgb(GtkWidget *widget, guint rgbvalue)
{
	GtkStyle *newstyle;
	GdkColor gdk_color;

	gtkut_convert_int_to_gdk_color(rgbvalue, &gdk_color);
	newstyle = gtk_style_copy(gtk_widget_get_default_style());
	newstyle->bg[GTK_STATE_NORMAL]   = gdk_color;
	newstyle->bg[GTK_STATE_PRELIGHT] = gdk_color;
	newstyle->bg[GTK_STATE_ACTIVE]   = gdk_color;
	gtk_widget_set_style(widget, newstyle);
}
  
/*!
 *\brief	Tries to find a focused child using a lame strategy
 */
GtkWidget *gtkut_get_focused_child(GtkContainer *parent)
{
	GtkWidget *result = NULL;
	GList *child_list = NULL;
	GList *c;

	cm_return_val_if_fail(parent, NULL);

	/* Get children list and see which has the focus. */
	child_list = gtk_container_get_children(parent);
	if (!child_list)
		return NULL;

	for (c = child_list; c != NULL; c = g_list_next(c)) {
		if (c->data && GTK_IS_WIDGET(c->data)) {
			if (gtk_widget_has_focus(GTK_WIDGET(c->data))) {
				result = GTK_WIDGET(c->data);
				break;
			}
		}
	}
	
	/* See if the returned widget is a container itself; if it is,
	 * see if one of its children is focused. If the focused 
	 * container has no focused child, it is itself a focusable 
	 * child, and has focus. */
	if (result && GTK_IS_CONTAINER(result)) {
		GtkWidget *tmp =  gtkut_get_focused_child(GTK_CONTAINER(result)); 
		
		if (tmp) 
			result = tmp;
	} else {
		/* Try the same for each container in the chain */
		for (c = child_list; c != NULL && !result; c = g_list_next(c)) {
			if (c->data && GTK_IS_WIDGET(c->data) 
			&&  GTK_IS_CONTAINER(c->data)) {
				result = gtkut_get_focused_child
					(GTK_CONTAINER(c->data));
			}
		}
	
	}
	
	g_list_free(child_list);
		
	return result;
}

/*!
 *\brief	Create a Browse (file) button based on GTK+ stock
 */
GtkWidget *gtkut_get_browse_file_btn(const gchar *button_label)
{
	GtkWidget *button;

	button = gtk_button_new_with_mnemonic(button_label);
	gtk_button_set_image(GTK_BUTTON(button),
		gtk_image_new_from_stock(GTK_STOCK_DIRECTORY, GTK_ICON_SIZE_BUTTON));

	return button;
}

/*!
 *\brief	Create a Browse (directory) button based on GTK+ stock
 */
GtkWidget *gtkut_get_browse_directory_btn(const gchar *button_label)
{
	GtkWidget *button;

	button = gtk_button_new_with_mnemonic(button_label);
	gtk_button_set_image(GTK_BUTTON(button),
		gtk_image_new_from_stock(GTK_STOCK_DIRECTORY, GTK_ICON_SIZE_BUTTON));

	return button;
}

GtkWidget *gtkut_get_replace_btn(const gchar *button_label)
{
	GtkWidget *button;

	button = gtk_button_new_with_mnemonic(button_label);
	gtk_button_set_image(GTK_BUTTON(button),
		gtk_image_new_from_stock(GTK_STOCK_REFRESH, GTK_ICON_SIZE_BUTTON));

	return button;
}

/**
 * merge some part of code into one function : it creates a frame and add
 *	these into gtk box widget passed in param.
 * \param box gtk box where adding new created frame.
 * \param pframe pointer with which to assign the frame. If NULL, no pointer
 *	is assigned but the frame is anyway created and added to @box.
 * \param frame_label frame label of new created frame.
 */
GtkWidget *gtkut_get_options_frame(GtkWidget *box, GtkWidget **pframe,
		const gchar *frame_label)
{
	GtkWidget *vbox;
	GtkWidget *frame;

	frame = gtk_frame_new(frame_label);
	gtk_widget_show(frame);
	gtk_box_pack_start(GTK_BOX(box), frame, FALSE, TRUE, 0);
	gtk_frame_set_label_align(GTK_FRAME(frame), 0.01, 0.5);

	vbox = gtk_vbox_new (FALSE, 4);
	gtk_widget_show(vbox);
	gtk_container_add(GTK_CONTAINER (frame), vbox);
	gtk_container_set_border_width (GTK_CONTAINER (vbox), 8);

	if (pframe != NULL)
		*pframe = frame;

	return vbox;
}

#if HAVE_LIBCOMPFACE
static gint create_xpm_from_xface(gchar *xpm[], const gchar *xface)
{
	static gchar *bit_pattern[] = {
		"....",
		"...#",
		"..#.",
		"..##",
		".#..",
		".#.#",
		".##.",
		".###",
		"#...",
		"#..#",
		"#.#.",
		"#.##",
		"##..",
		"##.#",
		"###.",
		"####"
	};

	static gchar *xface_header = "48 48 2 1";
	static gchar *xface_black  = "# c #000000";
	static gchar *xface_white  = ". c #ffffff";

	gint i, line = 0;
	const guchar *p;
	gchar buf[WIDTH * 4 + 1];  /* 4 = strlen("0x0000") */

	p = xface;

	strcpy(xpm[line++], xface_header);
	strcpy(xpm[line++], xface_black);
	strcpy(xpm[line++], xface_white);

	for (i = 0; i < HEIGHT; i++) {
		gint col;

		buf[0] = '\0';
     
		for (col = 0; col < 3; col++) {
			gint figure;

			p += 2;  /* skip '0x' */

			for (figure = 0; figure < 4; figure++) {
				gint n = 0;

				if ('0' <= *p && *p <= '9') {
					n = *p - '0';
				} else if ('a' <= *p && *p <= 'f') {
					n = *p - 'a' + 10;
				} else if ('A' <= *p && *p <= 'F') {
					n = *p - 'A' + 10;
				}

				strcat(buf, bit_pattern[n]);
				p++;  /* skip ',' */
			}

			p++;  /* skip '\n' */
		}

		strcpy(xpm[line++], buf);
		p++;
	}

	return 0;
}
#endif

gboolean get_tag_range(GtkTextIter *iter,
				       GtkTextTag *tag,
				       GtkTextIter *start_iter,
				       GtkTextIter *end_iter)
{
	GtkTextIter _start_iter, _end_iter;

	_end_iter = *iter;
	if (!gtk_text_iter_forward_to_tag_toggle(&_end_iter, tag)) {
		debug_print("Can't find end");
		return FALSE;
	}

	_start_iter = _end_iter;
	if (!gtk_text_iter_backward_to_tag_toggle(&_start_iter, tag)) {
		debug_print("Can't find start.");
		return FALSE;
	}

	*start_iter = _start_iter;
	*end_iter = _end_iter;

	return TRUE;
}

#if HAVE_LIBCOMPFACE
GtkWidget *xface_get_from_header(const gchar *o_xface, GdkColor *background,
				 GdkWindow *window)
{
	static gchar *xpm_xface[XPM_XFACE_HEIGHT];
	static gboolean xpm_xface_init = TRUE;
	gchar xface[2048];
	strncpy(xface, o_xface, sizeof(xface));

	if (!window) {
		g_warning("no window\n");
		return NULL;
	}
	if (uncompface(xface) < 0) {
		g_warning("uncompface failed\n");
		return NULL;
	}

	if (xpm_xface_init) {
		gint i;

		for (i = 0; i < XPM_XFACE_HEIGHT; i++) {
			xpm_xface[i] = g_malloc(WIDTH + 1);
			*xpm_xface[i] = '\0';
		}
		xpm_xface_init = FALSE;
	}

	create_xpm_from_xface(xpm_xface, xface);

	return gtk_image_new_from_pixbuf(
		gdk_pixbuf_new_from_xpm_data((const char **)xpm_xface));
}
#endif

GtkWidget *face_get_from_header(const gchar *o_face)
{
	gchar face[2048];
	gchar face_png[2048];
	gint pngsize;
	GdkPixbuf *pixbuf;
	GError *error = NULL;
	GdkPixbufLoader *loader = gdk_pixbuf_loader_new ();
	GtkWidget *image;
	
	if (o_face == NULL || strlen(o_face) == 0)
		return NULL;

	strncpy2(face, o_face, sizeof(face));

	unfold_line(face); /* strip all whitespace and linebreaks */
	remove_space(face);

	pngsize = base64_decode(face_png, face, strlen(face));

	if (!gdk_pixbuf_loader_write (loader, face_png, pngsize, &error) ||
	    !gdk_pixbuf_loader_close (loader, &error)) {
		g_warning("loading face failed\n");
		g_object_unref(loader);
		return NULL;
	}

	pixbuf = g_object_ref(gdk_pixbuf_loader_get_pixbuf(loader));

	g_object_unref(loader);

	if ((gdk_pixbuf_get_width(pixbuf) != 48) || (gdk_pixbuf_get_height(pixbuf) != 48)) {
		g_object_unref(pixbuf);
		g_warning("wrong_size\n");
		return NULL;
	}

	image = gtk_image_new_from_pixbuf(pixbuf);
	g_object_unref(pixbuf);
	return image;
}

static GdkCursor *hand_cursor = NULL;

static void link_btn_enter(GtkButton *button, gpointer data)
{
	GdkWindow *gdkwin;
	GtkWidget *window = (GtkWidget *)data;

	gdkwin = gtk_widget_get_window(window);

	if (!hand_cursor)
		hand_cursor = gdk_cursor_new(GDK_HAND2);
	if (window && gdkwin)
		gdk_window_set_cursor(gdkwin, hand_cursor);

	gtk_button_set_relief(button, GTK_RELIEF_NONE);
	gtk_widget_set_state(GTK_WIDGET(button), GTK_STATE_NORMAL);
	
}

static void link_btn_leave(GtkButton *button, gpointer data)
{
	GdkWindow *gdkwin;
	GtkWidget *window = (GtkWidget *)data;

	gdkwin = gtk_widget_get_window(window);

	if (window && gdkwin)
		gdk_window_set_cursor(gdkwin, NULL);

	gtk_button_set_relief(button, GTK_RELIEF_NONE);
	gtk_widget_set_state(GTK_WIDGET(button), GTK_STATE_NORMAL);
}

static void link_btn_pressed(GtkButton *button, gpointer data)
{
	gtk_button_set_relief(button, GTK_RELIEF_NONE);
	gtk_widget_set_state(GTK_WIDGET(button), GTK_STATE_NORMAL);
}

static void link_btn_released(GtkButton *button, gpointer data)
{
	gtk_button_set_relief(button, GTK_RELIEF_NONE);
	gtk_widget_set_state(GTK_WIDGET(button), GTK_STATE_NORMAL);
}

static void link_btn_clicked(GtkButton *button, gpointer data)
{
	gchar *url = (gchar *)data;
	gtk_button_set_relief(button, GTK_RELIEF_NONE);
	gtk_widget_set_state(GTK_WIDGET(button), GTK_STATE_NORMAL);
	open_uri(url, prefs_common_get_uri_cmd());
}

static void link_btn_unrealize(GtkButton *button, gpointer data)
{
	gchar *url = (gchar *)data;
	g_signal_handlers_disconnect_by_func(G_OBJECT(button), 
			 G_CALLBACK(link_btn_clicked), url);
	g_free(url);
}

GtkWidget *gtkut_get_link_btn(GtkWidget *window, const gchar *url, const gchar *label)
{
	GtkWidget *btn;
	GtkWidget *btn_label;
#if !GTK_CHECK_VERSION(3, 0, 0)
	GdkColormap *cmap;
	gboolean success[2];
#endif
	GdkColor uri_color[2] = {{0, 0, 0, 0xffff}, {0, 0xffff, 0, 0}};
	gchar *local_url = NULL;
	if (!url)
		return NULL;

	gtkut_convert_int_to_gdk_color(prefs_common.uri_col,
					       &uri_color[0]);
	gtkut_convert_int_to_gdk_color(prefs_common.uri_col,
					       &uri_color[1]);

	btn = gtk_button_new_with_label(label?label:url);
	gtk_button_set_relief(GTK_BUTTON(btn), GTK_RELIEF_NONE);
	btn_label = gtk_bin_get_child(GTK_BIN((btn)));
#if !GTK_CHECK_VERSION(3, 0, 0)
	cmap = gdk_drawable_get_colormap(gtk_widget_get_window(window));
	gdk_colormap_alloc_colors(cmap, uri_color, 2, FALSE, TRUE, success);
	if (success[0] == TRUE && success[1] == TRUE) {
#endif
		GtkStyle *style;
		gtk_widget_ensure_style(btn_label);
		style = gtk_style_copy
			(gtk_widget_get_style(btn_label));
		style->fg[GTK_STATE_NORMAL]   = uri_color[0];
		style->fg[GTK_STATE_ACTIVE]   = uri_color[1];
		style->fg[GTK_STATE_PRELIGHT] = uri_color[0];
		gtk_widget_set_style(btn_label, style);
#if !GTK_CHECK_VERSION(3, 0, 0)
	} else
		g_warning("color allocation failed\n");
#endif

	g_signal_connect(G_OBJECT(btn), "enter",
			 G_CALLBACK(link_btn_enter), window);
	g_signal_connect(G_OBJECT(btn), "leave",
			 G_CALLBACK(link_btn_leave), window);
	g_signal_connect(G_OBJECT(btn), "pressed",
			 G_CALLBACK(link_btn_pressed), window);
	g_signal_connect(G_OBJECT(btn), "released",
			 G_CALLBACK(link_btn_released), window);
			 
	local_url = g_strdup(url);
	g_signal_connect(G_OBJECT(btn), "clicked",
			 G_CALLBACK(link_btn_clicked), local_url);
	g_signal_connect(G_OBJECT(btn), "unrealize",
			 G_CALLBACK(link_btn_unrealize), local_url);
	return btn;
}

static gboolean _combobox_separator_func(GtkTreeModel *model,
		GtkTreeIter *iter, gpointer data)
{
	gchar *txt = NULL;

	cm_return_val_if_fail(model != NULL, FALSE);

	gtk_tree_model_get(model, iter, COMBOBOX_TEXT, &txt, -1);

	if( txt == NULL )
		return TRUE;
	return FALSE;
}

GtkWidget *gtkut_sc_combobox_create(GtkWidget *eventbox, gboolean focus_on_click)
{
	GtkWidget *combobox;
	GtkListStore *menu;
	GtkCellRenderer *rend;

	menu = gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_INT, G_TYPE_BOOLEAN);

	combobox = gtk_combo_box_new_with_model(GTK_TREE_MODEL(menu));

	rend = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox), rend, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox), rend,
			"markup", COMBOBOX_TEXT,
			"sensitive", COMBOBOX_SENS,
			NULL);

	if( eventbox != NULL )
		gtk_container_add(GTK_CONTAINER(eventbox), combobox);
	gtk_combo_box_set_focus_on_click(GTK_COMBO_BOX(combobox), focus_on_click);

	gtk_combo_box_set_row_separator_func(GTK_COMBO_BOX(combobox),
			(GtkTreeViewRowSeparatorFunc)_combobox_separator_func, NULL, NULL);

	return combobox;
}

static void gtkutils_smooth_scroll_do(GtkWidget *widget, GtkAdjustment *vadj,
				      gfloat old_value, gfloat last_value,
				      gint step)
{
	gint change_value;
	gboolean up;
	gint i;

	if (old_value < last_value) {
		change_value = last_value - old_value;
		up = FALSE;
	} else {
		change_value = old_value - last_value;
		up = TRUE;
	}

	for (i = step; i <= change_value; i += step) {
		gtk_adjustment_set_value(vadj, old_value + (up ? -i : i));
		g_signal_emit_by_name(G_OBJECT(vadj),
				      "value_changed", 0);
	}

	gtk_adjustment_set_value(vadj, last_value);
	g_signal_emit_by_name(G_OBJECT(vadj), "value_changed", 0);

	gtk_widget_queue_draw(widget);
}

static gboolean gtkutils_smooth_scroll_page(GtkWidget *widget, GtkAdjustment *vadj, gboolean up)
{
	gfloat upper;
	gfloat page_incr;
	gfloat old_value;
	gfloat last_value;

	page_incr = gtk_adjustment_get_page_increment(vadj);
	if (prefs_common.scroll_halfpage)
		page_incr /= 2;

	old_value = gtk_adjustment_get_value(vadj);
	if (!up) {
		upper = gtk_adjustment_get_upper(vadj) - gtk_adjustment_get_page_size(vadj);
		if (old_value < upper) {
			last_value = old_value + page_incr;
			last_value = MIN(last_value, upper);

			gtkutils_smooth_scroll_do(widget, vadj, old_value,
						  last_value,
						  prefs_common.scroll_step);
		} else
			return FALSE;
	} else {
		if (old_value > 0.0) {
			last_value = old_value - page_incr;
			last_value = MAX(last_value, 0.0);

			gtkutils_smooth_scroll_do(widget, vadj, old_value,
						  last_value,
						  prefs_common.scroll_step);
		} else
			return FALSE;
	}

	return TRUE;
}

gboolean gtkutils_scroll_page(GtkWidget *widget, GtkAdjustment *vadj, gboolean up)
{
	gfloat upper;
	gfloat page_incr;
	gfloat old_value;

	if (prefs_common.enable_smooth_scroll)
		return gtkutils_smooth_scroll_page(widget, vadj, up);

	page_incr = gtk_adjustment_get_page_increment(vadj);
	if (prefs_common.scroll_halfpage)
		page_incr /= 2;

	old_value = gtk_adjustment_get_value(vadj);
	if (!up) {
		upper = gtk_adjustment_get_upper(vadj) - gtk_adjustment_get_page_size(vadj);
		if (old_value < upper) {
			old_value += page_incr;
			old_value = MIN(old_value, upper);
			gtk_adjustment_set_value(vadj, old_value);
			g_signal_emit_by_name(G_OBJECT(vadj),
					      "value_changed", 0);
		} else
			return FALSE;
	} else {
		if (old_value > 0.0) {
			old_value -= page_incr;
			old_value = MAX(old_value, 0.0);
			gtk_adjustment_set_value(vadj, old_value);
			g_signal_emit_by_name(G_OBJECT(vadj),
					      "value_changed", 0);
		} else
			return FALSE;
	}
	return TRUE;
}

static void gtkutils_smooth_scroll_one_line(GtkWidget *widget, GtkAdjustment *vadj, gboolean up)
{
	gfloat upper;
	gfloat old_value;
	gfloat last_value;

	old_value = gtk_adjustment_get_value(vadj);
	if (!up) {
		upper = gtk_adjustment_get_upper(vadj) - gtk_adjustment_get_page_size(vadj);
		if (old_value < upper) {
			last_value = old_value + gtk_adjustment_get_step_increment(vadj);
			last_value = MIN(last_value, upper);

			gtkutils_smooth_scroll_do(widget, vadj, old_value,
						  last_value,
						  prefs_common.scroll_step);
		}
	} else {
		if (old_value > 0.0) {
			last_value = old_value - gtk_adjustment_get_step_increment(vadj);
			last_value = MAX(last_value, 0.0);

			gtkutils_smooth_scroll_do(widget, vadj, old_value,
						  last_value,
						  prefs_common.scroll_step);
		}
	}
}

void gtkutils_scroll_one_line(GtkWidget *widget, GtkAdjustment *vadj, gboolean up)
{
	gfloat upper;
	gfloat old_value;

	if (prefs_common.enable_smooth_scroll) {
		gtkutils_smooth_scroll_one_line(widget, vadj, up);
		return;
	}

	old_value = gtk_adjustment_get_value(vadj);
	if (!up) {
		upper = gtk_adjustment_get_upper(vadj) - gtk_adjustment_get_page_size(vadj);
		if (old_value < upper) {
			old_value += gtk_adjustment_get_step_increment(vadj);
			old_value = MIN(old_value, upper);
			gtk_adjustment_set_value(vadj, old_value);
			g_signal_emit_by_name(G_OBJECT(vadj),
					      "value_changed", 0);
		}
	} else {
		if (old_value > 0.0) {
			old_value -= gtk_adjustment_get_step_increment(vadj);
			old_value = MAX(old_value, 0.0);
			gtk_adjustment_set_value(vadj, old_value);
			g_signal_emit_by_name(G_OBJECT(vadj),
					      "value_changed", 0);
		}
	}
}

gboolean gtkut_tree_model_text_iter_prev(GtkTreeModel *model,
				 GtkTreeIter *iter,
				 const gchar* text)
/* do the same as gtk_tree_model_iter_next, but _prev instead.
   to use with widgets with one text column (gtk_combo_box_new_text()
   and with GtkComboBoxEntry's for instance),
*/
{
	GtkTreeIter cur_iter;
	gchar *cur_value;
	gboolean valid;
	gint count;

	cm_return_val_if_fail(model != NULL, FALSE);
	cm_return_val_if_fail(iter != NULL, FALSE);

	if (text == NULL || *text == '\0')
		return FALSE;

	valid = gtk_tree_model_get_iter_first(model, &cur_iter);
	count = 0;
	while (valid) {
		gtk_tree_model_get(model, &cur_iter, 0, &cur_value, -1);

		if (strcmp(text, cur_value) == 0) {
			if (count <= 0)
				return FALSE;

			return gtk_tree_model_iter_nth_child(model, iter, NULL, count - 1);
		}

		valid = gtk_tree_model_iter_next(model, &cur_iter);
		count++;
	}
	return FALSE;		
}

gboolean gtkut_tree_model_get_iter_last(GtkTreeModel *model,
				 GtkTreeIter *iter)
/* do the same as gtk_tree_model_get_iter_first, but _last instead.
*/
{
	gint count;

	cm_return_val_if_fail(model != NULL, FALSE);
	cm_return_val_if_fail(iter != NULL, FALSE);

	count = gtk_tree_model_iter_n_children(model, NULL);

	if (count <= 0)
		return FALSE;

	return gtk_tree_model_iter_nth_child(model, iter, NULL, count - 1);
}

GtkWidget *gtkut_window_new		(GtkWindowType	 type,
					 const gchar	*class)
{
#ifndef MAEMO
	GtkWidget *window = gtk_window_new(type);
#else
	GtkWidget *window = hildon_window_new();
	hildon_program_add_window(hildon_program, HILDON_WINDOW(window));
#endif
	gtk_window_set_role(GTK_WINDOW(window), class);
	return window;
}

static gboolean gtkut_tree_iter_comp(GtkTreeModel *model, 
				     GtkTreeIter *iter1, 
				     GtkTreeIter *iter2)
{
	GtkTreePath *path1 = gtk_tree_model_get_path(model, iter1);
	GtkTreePath *path2 = gtk_tree_model_get_path(model, iter2);
	gboolean result;

	result = gtk_tree_path_compare(path1, path2) == 0;

	gtk_tree_path_free(path1);
	gtk_tree_path_free(path2);
	
	return result;
}

/*!
 *\brief	Get selected row number.
 */
gint gtkut_list_view_get_selected_row(GtkWidget *list_view)
{
	GtkTreeView *view = GTK_TREE_VIEW(list_view);
	GtkTreeModel *model = gtk_tree_view_get_model(view);
	int n_rows = gtk_tree_model_iter_n_children(model, NULL);
	GtkTreeSelection *selection;
	GtkTreeIter iter;
	int row;

	if (n_rows == 0) 
		return -1;
	
	selection = gtk_tree_view_get_selection(view);
	if (!gtk_tree_selection_get_selected(selection, &model, &iter))
		return -1;
	
	/* get all iterators and compare them... */
	for (row = 0; row < n_rows; row++) {
		GtkTreeIter itern;

		gtk_tree_model_iter_nth_child(model, &itern, NULL, row);
		if (gtkut_tree_iter_comp(model, &iter, &itern))
			return row;
	}
	
	return -1;
}

/*!
 *\brief	Select a row by its number.
 */
gboolean gtkut_list_view_select_row(GtkWidget *list, gint row)
{
	GtkTreeView *list_view = GTK_TREE_VIEW(list);
	GtkTreeSelection *selection = gtk_tree_view_get_selection(list_view);
	GtkTreeModel *model = gtk_tree_view_get_model(list_view);
	GtkTreeIter iter;
	GtkTreePath *path;

	if (!gtk_tree_model_iter_nth_child(model, &iter, NULL, row))
		return FALSE;
	
	gtk_tree_selection_select_iter(selection, &iter);

	path = gtk_tree_model_get_path(model, &iter);
	gtk_tree_view_set_cursor(list_view, path, NULL, FALSE);
	gtk_tree_path_free(path);
	
	return TRUE;
}

static GtkUIManager *gui_manager = NULL;

GtkUIManager *gtkut_create_ui_manager(void)
{
	cm_return_val_if_fail(gui_manager == NULL, gui_manager);
	return (gui_manager = gtk_ui_manager_new());
}

GtkUIManager *gtkut_ui_manager(void)
{
	return gui_manager;
}

typedef struct _ClawsIOClosure ClawsIOClosure;

struct _ClawsIOClosure
{
  ClawsIOFunc function;
  GIOCondition condition;
  GDestroyNotify notify;
  gpointer data;
};

static gboolean  
claws_io_invoke (GIOChannel   *source,
	         GIOCondition  condition,
	         gpointer      data)
{
  ClawsIOClosure *closure = data;
  int fd;
#ifndef G_OS_WIN32
  fd = g_io_channel_unix_get_fd (source);
#else
  fd = g_io_channel_win32_get_fd (source);
#endif
  if (closure->condition & condition)
    closure->function (closure->data, fd, condition);

  return TRUE;
}

static void
claws_io_destroy (gpointer data)
{
  ClawsIOClosure *closure = data;

  if (closure->notify)
    closure->notify (closure->data);

  g_free (closure);
}

gint
claws_input_add    (gint	      source,
		    GIOCondition      condition,
		    ClawsIOFunc       function,
		    gpointer	      data,
		    gboolean	      is_sock)
{
  guint result;
  ClawsIOClosure *closure = g_new (ClawsIOClosure, 1);
  GIOChannel *channel;

  closure->function = function;
  closure->condition = condition;
  closure->notify = NULL;
  closure->data = data;

#ifndef G_OS_WIN32
  channel = g_io_channel_unix_new (source);
#else
  if (is_sock)
    channel = g_io_channel_win32_new_socket(source);
  else
    channel = g_io_channel_win32_new_fd(source);
#endif
  result = g_io_add_watch_full (channel, G_PRIORITY_DEFAULT, condition, 
				claws_io_invoke,
				closure, claws_io_destroy);
  g_io_channel_unref (channel);

  return result;
}

void gtkut_widget_set_mapped(GtkWidget *widget, gboolean mapped)
{
#if GTK_CHECK_VERSION(2,20,0)
	gtk_widget_set_mapped(widget, mapped);
#else
	if (mapped)
		GTK_WIDGET_SET_FLAGS(widget, GTK_MAPPED);
	else
		GTK_WIDGET_UNSET_FLAGS(widget, GTK_MAPPED);
#endif
}

void gtkut_widget_set_realized(GtkWidget *widget, gboolean realized)
{
#if GTK_CHECK_VERSION(2,20,0)
	gtk_widget_set_realized(widget, realized);
#else
	if (realized)
		GTK_WIDGET_SET_FLAGS(widget, GTK_REALIZED);
	else
		GTK_WIDGET_UNSET_FLAGS(widget, GTK_REALIZED);
#endif
}

void gtkut_widget_set_can_default(GtkWidget *widget, gboolean can_default)
{
#if GTK_CHECK_VERSION(2,20,0)
	gtk_widget_set_can_default(widget, can_default);
#else
	if (can_default)
		GTK_WIDGET_SET_FLAGS(widget, GTK_CAN_DEFAULT);
	else
		GTK_WIDGET_UNSET_FLAGS(widget, GTK_CAN_DEFAULT);
#endif
}

void gtkut_widget_set_receives_default(GtkWidget *widget, gboolean receives_default)
{
#if GTK_CHECK_VERSION(2,20,0)
	gtk_widget_set_receives_default(widget, receives_default);
#else
	if (receives_default)
		GTK_WIDGET_SET_FLAGS(widget, GTK_RECEIVES_DEFAULT);
	else
		GTK_WIDGET_UNSET_FLAGS(widget, GTK_RECEIVES_DEFAULT);
#endif
}

void gtkut_widget_set_can_focus(GtkWidget *widget, gboolean can_focus)
{
#if GTK_CHECK_VERSION(2,20,0)
	gtk_widget_set_can_focus(widget, can_focus);
#else
	if (can_focus)
		GTK_WIDGET_SET_FLAGS(widget, GTK_CAN_FOCUS);
	else
		GTK_WIDGET_UNSET_FLAGS(widget, GTK_CAN_FOCUS);
#endif
}

void gtkut_widget_set_has_window(GtkWidget *widget, gboolean has_window)
{
#if GTK_CHECK_VERSION(2,20,0)
	gtk_widget_set_has_window(widget, has_window);
#else
	if (has_window) /* Inverted logic there */
		GTK_WIDGET_UNSET_FLAGS(widget, GTK_NO_WINDOW);
	else
		GTK_WIDGET_SET_FLAGS(widget, GTK_NO_WINDOW);
#endif
}

/**
 * Load a pixbuf fitting inside the specified size. EXIF orientation is
 * respected if available.
 *
 * @param[in] filename		the file to load
 * @param[in] box_width		the max width (-1 for no resize)
 * @param[in] box_height	the max height (-1 for no resize)
 * @param[out] error		the possible load error
 *
 * @return a GdkPixbuf
 */
GdkPixbuf *claws_load_pixbuf_fitting(GdkPixbuf *src_pixbuf, int box_width,
				     int box_height)
{
	gint w, h, orientation, angle;
	gint avail_width, avail_height;
	gboolean flip_horiz, flip_vert;
	const gchar *orient_str;
	GdkPixbuf *pixbuf, *t_pixbuf;

	pixbuf = src_pixbuf;

	if (pixbuf == NULL)
		return NULL;

	angle = 0;
	flip_horiz = flip_vert = FALSE;

	/* EXIF orientation */
	orient_str = gdk_pixbuf_get_option(pixbuf, "orientation");
	if (orient_str != NULL && *orient_str != '\0') {
		orientation = atoi(orient_str);
		switch(orientation) {
			/* See EXIF standard for different values */
			case 1:	break;
			case 2:	flip_horiz = 1;
				break;
			case 3:	angle = 180;
				break;
			case 4:	flip_vert = 1;
				break;
			case 5:	angle = 90;
				flip_horiz = 1;
				break;
			case 6:	angle = 270;
				break;
			case 7:	angle = 90;
				flip_vert = 1;
				break;
			case 8:	angle = 90;
				break;
		}
	}

	w = gdk_pixbuf_get_width(pixbuf);
	h = gdk_pixbuf_get_height(pixbuf);

	if (angle == 90 || angle == 270) {
		avail_height = box_width;
		avail_width = box_height;
	} else {
		avail_width = box_width;
		avail_height = box_height;
	}

	/* Scale first */
	if (box_width != -1 && box_height != -1 && avail_width - 100 > 0) {
		if (w > avail_width) {
			h = (avail_width * h) / w;
			w = avail_width;
		}
		if (h > avail_height) {
			w = (avail_height * w) / h;
			h = avail_height;
		}
		t_pixbuf = gdk_pixbuf_scale_simple(pixbuf, 
			w, h, GDK_INTERP_BILINEAR);
		g_object_unref(pixbuf);
		pixbuf = t_pixbuf;
	}

	/* Rotate if needed */
	if (angle != 0) {
		t_pixbuf = gdk_pixbuf_rotate_simple(pixbuf, angle);
		g_object_unref(pixbuf);
		pixbuf = t_pixbuf;
	}

	/* Flip horizontally if needed */
	if (flip_horiz) {
		t_pixbuf = gdk_pixbuf_flip(pixbuf, TRUE);
		g_object_unref(pixbuf);
		pixbuf = t_pixbuf;
	}

	/* Flip vertically if needed */
	if (flip_vert) {
		t_pixbuf = gdk_pixbuf_flip(pixbuf, FALSE);
		g_object_unref(pixbuf);
		pixbuf = t_pixbuf;
	}

	return pixbuf;
}
