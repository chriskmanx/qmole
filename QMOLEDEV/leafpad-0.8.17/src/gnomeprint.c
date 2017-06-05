/*
 *  Leafpad - GTK+ based simple text editor
 *  Copyright (C) 2004-2005 Tarot Osuji
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "leafpad.h"

#if !GTK_CHECK_VERSION(2, 10, 0)

#ifdef ENABLE_PRINT

#include <libgnomeprint/gnome-print.h>
#include <libgnomeprintui/gnome-print-dialog.h>
#include <libgnomeprintui/gnome-print-job-preview.h>

#define DV(x)
#define FONT_SIZE 10

static GnomePrintJob *job;
static GnomePrintContext *gpx;
static GnomeFont *font;
static GnomeGlyphList *gl;
static gdouble margin_left, margin_right, margin_top, margin_bottom;
static gdouble page_height, page_width, page_x, page_y;
static gdouble line_height, tab_width, gl_width;

static void move_pen_to_nextpage(void)
{
	gnome_print_showpage(gpx);
	gnome_print_context_close(gpx);
	g_object_unref(gpx);
	gpx = gnome_print_job_get_context(job);
	gnome_print_beginpage(gpx, NULL);
	gnome_print_setfont(gpx, font);
	page_y = page_height - margin_top - line_height;
	gnome_print_moveto(gpx, page_x, page_y);
}

static void move_pen_to_newline(void)
{
	page_x = margin_left;
	page_y -= line_height * 1.2;
	if (page_y < margin_bottom)
		move_pen_to_nextpage();
	else
		gnome_print_moveto(gpx, page_x, page_y);
}

static void print_glyph_list(gboolean is_newline)
{
	if (is_newline)
		move_pen_to_newline();
	gnome_print_glyphlist(gpx, gl);
	page_x += gl_width;
	gnome_print_moveto(gpx, page_x, page_y);
	gl = gnome_glyphlist_from_text_dumb(font, 0x000000ff, 0, 0, (guchar *)"");
	gl_width = 0;
}

static GnomeFont *find_proper_font(gunichar ch)
{
//	GnomeFont *gfont;
	GnomeFontFace *gface;
	PangoFont *pfont;
	PangoFontset *pfontset;
	
	pfontset = pango_context_load_fontset(
		gtk_widget_get_pango_context(pub->mw->view),
		gtk_widget_get_style(pub->mw->view)->font_desc,
		gtk_get_default_language());
	pfont = pango_fontset_get_font(pfontset, ch);
/*	
	g_print("\n%s\n",
		pango_font_description_to_string(
			pango_font_describe(pfont)
		)
	);
*/	
	gface =
		gnome_font_face_find_closest_from_pango_font(pfont);
	g_object_unref(pfont);
	
	return gnome_font_face_get_font_default(gface, FONT_SIZE);
}

static GnomePrintJob *create_job(GnomePrintConfig *gpc)
{
	GnomeFontFace *font_face;
	PangoFontDescription *font_desc;
	GtkTextIter start, end;
	gchar *text, *p;
	
	/* Get contents of TextBuffer */
	GtkTextBuffer *buffer = pub->mw->buffer;
	gtk_text_buffer_get_bounds(buffer, &start, &end);
	text = g_strchomp(gtk_text_buffer_get_text(buffer, &start, &end, FALSE));
	
	/* Initialize job */
	job = gnome_print_job_new(gpc);
	gnome_print_job_get_page_size_from_config(gpc, &page_width, &page_height);
	gnome_print_config_get_length(gpc,
		(guchar *)GNOME_PRINT_KEY_PAGE_MARGIN_LEFT, &margin_left, NULL);
	gnome_print_config_get_length(gpc,
		(guchar *)GNOME_PRINT_KEY_PAGE_MARGIN_RIGHT, &margin_right, NULL);
	gnome_print_config_get_length(gpc,
		(guchar *)GNOME_PRINT_KEY_PAGE_MARGIN_TOP, &margin_top, NULL);
	gnome_print_config_get_length(gpc,
		(guchar *)GNOME_PRINT_KEY_PAGE_MARGIN_BOTTOM, &margin_bottom, NULL);
/*
g_print("margin_left   = %f\n", margin_left);
g_print("margin_right  = %f\n", margin_right);
g_print("margin_top    = %f\n", margin_top);
g_print("margin_bottom = %f\n", margin_bottom);
	margin_top = margin_left;
	margin_bottom = margin_left;
*/	
	/* Initialize font */
	font_desc = gtk_widget_get_style(pub->mw->view)->font_desc;
	font_face = gnome_font_face_find_closest_from_pango_description(font_desc);
	font = gnome_font_face_get_font_default(font_face, FONT_SIZE);
//		pango_font_description_get_size(font_desc) / PANGO_SCALE);
//	g_print("PANGO_SCALE = %d\n", PANGO_SCALE);
//	g_print("font_size = %d\n", pango_font_description_get_size(font_desc));
	line_height = gnome_font_get_size(font);
	tab_width = gnome_font_face_get_glyph_width(font_face,
		gnome_font_face_lookup_default(font_face, g_utf8_get_char(" ")))
		/ 1000 * FONT_SIZE * get_current_tab_width();
DV(	g_print("tab_width = %f\n", tab_width));
	
	/* Draw texts to canvas */
	gpx = gnome_print_job_get_context(job);
	gnome_print_beginpage(gpx, NULL);
	gnome_print_setfont(gpx, font);
	
	page_x = margin_left;	// TODO RTL
	page_y = page_height - margin_top - line_height;
	gnome_print_moveto(gpx, page_x, page_y);
	
	gl = gnome_glyphlist_from_text_dumb(font, 0x000000ff, 0, 0, (guchar *)"");
	gl_width = 0;
	
	p = text;
	
	while (*p != '\0') {
		gunichar ch = g_utf8_get_char(p);
		gint glyph = gnome_font_lookup_default(font, ch);
		if (!glyph) {
			if (ch == '\n') {
				print_glyph_list(page_x + gl_width > page_width - margin_right);
				move_pen_to_newline();
DV(				g_print("LF\n"));
			} else if (ch == '\t') {
				print_glyph_list(page_x + gl_width > page_width - margin_right);
/*				page_x = page_x + tab_width
					- ((page_x - margin_left) % tab_width); */
				gdouble tmp_x = margin_left;
				do {
					tmp_x += tab_width;
				} while (tmp_x < page_x + 0.000001); // FIXME
DV(				g_print("HT "));
DV(				g_print("%f -> %f\n", page_x, tmp_x));
				page_x = tmp_x;
				gnome_print_moveto(gpx, page_x, page_y);
/*			} else if (ch == '\f') {
DV(				g_print("FF\n"));
				print_glyph_list(page_x + gl_width > page_width - margin_right);
				move_pen_to_nextpage();
*/			} else {
				GnomeFont *tmp_font = find_proper_font(ch);
				GnomeFontFace *tmp_face = gnome_font_get_face(tmp_font);
				gdouble g_width;
				
				glyph = gnome_font_lookup_default(tmp_font, ch);
				g_width = gnome_font_face_get_glyph_width(tmp_face, glyph)
					/ 1000 * FONT_SIZE;
				if (page_x + gl_width + g_width > page_width - margin_right) {
					if (g_unichar_iswide(ch)) {
						print_glyph_list(FALSE);
						move_pen_to_newline();
					} else
						print_glyph_list(TRUE);
				}
				gnome_glyphlist_font(gl, tmp_font);
				gnome_glyphlist_glyph(gl, glyph);
				gnome_glyphlist_font(gl, font);
				gl_width += g_width;
DV(				g_print("** "));
			}
		} else {
//			if (ch == ' ') {
			if (g_unichar_isspace(ch)) {
DV(				g_print("SP "));
DV(				g_print("\n"));
				print_glyph_list(page_x + gl_width > page_width - margin_right);
				page_x +=
					gnome_font_face_get_glyph_width(font_face, glyph) / 1000 * FONT_SIZE;
				gnome_print_moveto(gpx, page_x, page_y);
			} else {
				gdouble g_width
					= gnome_font_face_get_glyph_width(font_face, glyph)
						/ 1000 * FONT_SIZE;
				if (page_x + gl_width + g_width > page_width - margin_right) {
					if (g_unichar_iswide(ch)) {
						print_glyph_list(FALSE);
						move_pen_to_newline();
					} else
						print_glyph_list(TRUE);
				}
				gnome_glyphlist_glyph(gl, glyph);
				gl_width += g_width;
DV(				g_print("%02X ", glyph));
DV(				g_print("%f (%f)\n", gl_width, gnome_font_get_glyph_width(font, glyph)));
			}
		}
		p = g_utf8_next_char(p);
	}
	print_glyph_list(page_x + gl_width > page_width - margin_right);
DV(	g_print("\n[EOT]\n"));
	
	gnome_print_showpage(gpx);
	gnome_print_context_close(gpx);
	g_object_unref(gpx);
	
	gnome_glyphlist_unref(gl);
	gnome_font_unref(font);
	gnome_font_face_unref(font_face);
	gnome_print_job_close(job);
	
	return job;
}

gint create_gnomeprint_session(void)
{
	GnomePrintJob *job = NULL;
	static GnomePrintConfig *gpc = NULL;
	GtkWidget *dialog, *preview;
	gint res, page_num = 0;
	GtkWidget *notebook, *page;
	
	if (gpc == NULL)
		gpc = gnome_print_config_default();
	
	job = gnome_print_job_new(gpc);
	if (job == NULL)
		return -1;
	
	dialog = gnome_print_dialog_new(job, (guchar *)_("Print"), 0);
	notebook = gtk_container_get_children(
		GTK_CONTAINER(GTK_DIALOG(dialog)->vbox))->data;
	gtk_widget_hide(notebook);
	if (!GTK_WIDGET_VISIBLE(gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), page_num)))
		page_num++;
	page = gtk_widget_ref(
		gtk_notebook_get_nth_page(GTK_NOTEBOOK(notebook), page_num));
	gtk_notebook_remove_page(GTK_NOTEBOOK(notebook), page_num);
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox),
		page, FALSE, FALSE, 0);
	gtk_widget_set_size_request(
		gtk_container_get_children(GTK_CONTAINER(page))->data, -1, 240);
	gtk_window_set_transient_for(GTK_WINDOW(dialog),
		GTK_WINDOW(pub->mw->window));
	
	do {
		res = gtk_dialog_run(GTK_DIALOG(dialog));
		gnome_print_config_unref(gpc);
		gpc = gnome_print_dialog_get_config(GNOME_PRINT_DIALOG(dialog));
		switch (res) {
		case GNOME_PRINT_DIALOG_RESPONSE_PRINT:
			g_object_unref(job);
			job = create_job(gpc);
			gnome_print_job_print(job);
			break;
		case GNOME_PRINT_DIALOG_RESPONSE_PREVIEW:
			g_object_unref(job);
			job = create_job(gpc);
			preview = gnome_print_job_preview_new(job,
				(guchar *)_("Print Preview"));
//			gtk_window_set_transient_for(GTK_WINDOW(preview), GTK_WINDOW(dialog));
			gtk_window_set_modal(GTK_WINDOW(preview), TRUE);
			gtk_widget_show(preview);
			g_signal_connect(G_OBJECT(preview), "destroy",
				G_CALLBACK(gtk_main_quit), NULL);
			gtk_main();
			break;
		}
	} while (res == GNOME_PRINT_DIALOG_RESPONSE_PREVIEW);
	gtk_widget_destroy(dialog);
	g_object_unref(job);
	
	return res;
}

#endif

#endif

