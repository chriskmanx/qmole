/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2007 Tarot Osuji
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <gtk/gtk.h>

#if GTK_CHECK_VERSION(2, 10, 0)

static PangoLayout *layout;
static PangoFontDescription *font_desc;
static gint line_count, lines_per_page, text_height;
static gint n_pages;
static gdouble page_width, page_height;
static const gchar *page_title = NULL;

static void get_tab_array(PangoTabArray **tabs,
	GtkPrintContext *ctx, GtkTextView *text_view)
{
	gint xft_dpi, loc;
	GtkSettings *settings = gtk_settings_get_default();
	
	g_object_get(settings, "gtk-xft-dpi", &xft_dpi, NULL);
	if ((*tabs = gtk_text_view_get_tabs(text_view))) {
		pango_tab_array_get_tab(*tabs, 0, NULL, &loc);
		pango_tab_array_set_tab(*tabs, 0, PANGO_TAB_LEFT,
			loc * gtk_print_context_get_dpi_x(ctx) / (xft_dpi / PANGO_SCALE));
	}
}

static void cb_begin_print(GtkPrintOperation *op,
		GtkPrintContext *ctx, gpointer data)
{
	gint layout_height;
	gchar *text;
	GtkTextIter start, end;
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(data);
	PangoTabArray *tabs;
	
	gtk_text_buffer_get_bounds(buffer, &start, &end);
	text = g_strchomp(gtk_text_buffer_get_text(buffer, &start, &end, FALSE));
	
	page_width = gtk_print_context_get_width(ctx);
	page_height = gtk_print_context_get_height(ctx);
	font_desc = gtk_widget_get_style(data)->font_desc;
	layout = gtk_print_context_create_pango_layout(ctx);
	pango_layout_set_width(layout, page_width * PANGO_SCALE);
	pango_layout_set_font_description(layout, font_desc);
	pango_layout_set_text(layout, text, -1);
	
	get_tab_array(&tabs, ctx, data);
	if (tabs) {
		pango_layout_set_tabs(layout, tabs);
		pango_tab_array_free(tabs);
	}
	pango_layout_get_size(layout, NULL, &layout_height);
	
	line_count = pango_layout_get_line_count(layout);
	text_height = pango_font_description_get_size(font_desc) / PANGO_SCALE;
	lines_per_page = page_height / text_height;
	
	n_pages = (line_count - 1) / lines_per_page + 1;
	gtk_print_operation_set_n_pages(op, n_pages);
	
	g_free(text);
}

static void cb_draw_page(GtkPrintOperation *op,
		GtkPrintContext *ctx, gint page_nr, gpointer data)
{
	cairo_t *cr;
	PangoLayoutLine *line;
	gint n_line, i, j = 0;
	
	PangoLayout *layout_lh, *layout_rh;
	gchar *page_text;
	gint layout_width;
	
	cr = gtk_print_context_get_cairo_context(ctx);
	
	layout_lh = gtk_print_context_create_pango_layout(ctx);
	pango_layout_set_font_description(layout_lh, font_desc);
	pango_layout_set_text(layout_lh, page_title, -1);
	cairo_move_to(cr, 0, - 72 / 25.4 * 10);
	pango_cairo_show_layout(cr, layout_lh);
	
	page_text = g_strdup_printf("%d / %d", page_nr + 1, n_pages);
	layout_rh = gtk_print_context_create_pango_layout(ctx);
	pango_layout_set_font_description(layout_rh, font_desc);
	pango_layout_set_text(layout_rh, page_text, -1);
//	pango_layout_set_alignment(layout_rh, PANGO_ALIGN_RIGHT);
	pango_layout_get_size(layout_rh, &layout_width, NULL);
	cairo_move_to(cr,
		page_width - layout_width / PANGO_SCALE, - 72 / 25.4 * 10);
	pango_cairo_show_layout(cr, layout_rh);
	g_free(page_text);
	
	if (line_count > lines_per_page * (page_nr + 1))
		n_line = lines_per_page * (page_nr + 1);
	else
		n_line = line_count;
	
	for (i = lines_per_page * page_nr; i < n_line; i++) {
		line = pango_layout_get_line(layout, i);
		cairo_move_to(cr, 0, text_height * (j + 1));
		pango_cairo_show_layout_line(cr, line);
		j++;
	}	
}

static void cb_end_print(GtkPrintOperation *op,
		GtkPrintContext *ctx, gpointer data)
{
	g_object_unref(layout);
}

static GtkPrintSettings *settings = NULL;

static GtkPrintOperation *create_print_operation(GtkTextView *text_view)
{
	GtkPrintOperation *op;
	static GtkPageSetup *page_setup = NULL;
	
	op = gtk_print_operation_new();
	
	if (settings)
		gtk_print_operation_set_print_settings(op, settings);
	
	if (!page_setup) {
		page_setup = gtk_page_setup_new();
		gtk_page_setup_set_top_margin(page_setup, 25.0, GTK_UNIT_MM);
		gtk_page_setup_set_bottom_margin(page_setup, 20.0, GTK_UNIT_MM);
		gtk_page_setup_set_left_margin(page_setup, 20.0, GTK_UNIT_MM);
		gtk_page_setup_set_right_margin(page_setup, 20.0, GTK_UNIT_MM);
	}
	gtk_print_operation_set_default_page_setup(op, page_setup);
	
	g_signal_connect(op, "begin-print", G_CALLBACK(cb_begin_print), text_view);
	g_signal_connect(op, "draw-page", G_CALLBACK(cb_draw_page), NULL);
	g_signal_connect(op, "end-print", G_CALLBACK(cb_end_print), NULL);
	
	return op;
}

static void create_error_dialog(GtkTextView *text_view, gchar *message)
{
	GtkWidget *dialog;
	
	dialog = gtk_message_dialog_new(
		GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(text_view))),
		GTK_DIALOG_DESTROY_WITH_PARENT,
		GTK_MESSAGE_ERROR,
		GTK_BUTTONS_NONE,
		message);
	gtk_window_set_resizable(GTK_WINDOW(dialog), FALSE);
	gtk_dialog_add_buttons(GTK_DIALOG(dialog),
		GTK_STOCK_OK, GTK_RESPONSE_CANCEL, NULL);
	gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);
	gtk_dialog_run(GTK_DIALOG(dialog));
	gtk_widget_destroy(dialog);
}

void create_gtkprint_session(GtkTextView *text_view, const gchar *title)
{
	GtkPrintOperation *op;
	GtkPrintOperationResult res;
	GError *err = NULL;
	
	page_title = title;
	op = create_print_operation(text_view);
	
	res = gtk_print_operation_run(op, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
		GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(text_view))), &err);
	switch (res) {
	case GTK_PRINT_OPERATION_RESULT_ERROR:
		create_error_dialog(text_view, err->message);
		g_error_free(err);
	case GTK_PRINT_OPERATION_RESULT_APPLY:
		if (settings)
			g_object_unref(settings);
		settings = g_object_ref(gtk_print_operation_get_print_settings(op));
	default:
		break;
	}
	
	g_object_unref(op);
}

void create_gtkprint_preview_session(GtkTextView *text_view, const gchar *title)
{
	GtkPrintOperation *op;
	GtkPrintOperationResult res;
	GError *err = NULL;
	
	page_title = title;
	op = create_print_operation(text_view);
	
	res = gtk_print_operation_run(op, GTK_PRINT_OPERATION_ACTION_PREVIEW,
		GTK_WINDOW(gtk_widget_get_toplevel(GTK_WIDGET(text_view))), &err);
	if (res == GTK_PRINT_OPERATION_RESULT_ERROR) {
		create_error_dialog(text_view, err->message);
		g_error_free(err);
	}
	
	g_object_unref(op);
}

#endif

