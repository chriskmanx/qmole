/* Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2007-2012 Holger Berndt <hb@claws-mail.org>,
 * Colin Leroy <colin@colino.net>, and the Claws Mail team
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

#include "defs.h"

#include "printing.h"
#include "image_viewer.h"

#include "gtkutils.h"
#include "toolbar.h"
#include "prefs_common.h"

#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <pango/pango.h>
#include <string.h>
#include <math.h>

struct _PrintData {
	PrintRenderer *renderer;
	gpointer renderer_data;
	PangoLayout *layout;
	PangoContext *pango_context;
	gpointer to_print;
	GList *page_breaks;
	guint npages;
	gint sel_start;
	gint sel_end;
	GHashTable *images;
	gint img_cnt;
	gdouble zoom;
	gdouble ypos_line;
};

typedef struct {
	GtkPrintOperation *op;
	GtkPrintOperationPreview *preview;
	GtkWidget         *dialog;
	GtkWidget         *scrolled_window;
	GtkWidget         *area;
	PrintData         *print_data;
	GtkWidget         *page_nr_label;
	GList             *pages_to_print;
	GList             *current_page;
	GtkWidget *first;
	GtkWidget *next;
	GtkWidget *previous;
	GtkWidget *last;
	GtkWidget *close;
	GtkWidget *zoom_100;
	GtkWidget *zoom_fit;
	GtkWidget *zoom_in;
	GtkWidget *zoom_out;
	gboolean rendering;
	gint page_width;
	gint page_height;
	GtkPrintContext *context;
} PreviewData;

/* callbacks */
static void     printing_textview_cb_begin_print(GtkPrintOperation*,
						 GtkPrintContext*, gpointer);
static void     printing_textview_cb_draw_page(GtkPrintOperation*, 
					       GtkPrintContext*, gint,
			     		       gpointer);
static gboolean cb_preview(GtkPrintOperation*, GtkPrintOperationPreview*,
			   GtkPrintContext*, GtkWindow*, gpointer);
static void     cb_preview_destroy(GtkWindow*, gpointer);
static gboolean cb_preview_close(GtkWidget*, GdkEventAny*, gpointer);
static void     cb_preview_size_allocate(GtkWidget*, GtkAllocation*);
static void     cb_preview_ready(GtkPrintOperationPreview*,
				 GtkPrintContext*, gpointer);
#if !GTK_CHECK_VERSION(3, 0, 0)
static gboolean cb_preview_expose(GtkWidget*, GdkEventExpose*, gpointer);
#else
static gboolean cb_preview_expose(GtkWidget*, cairo_t*, gpointer);
#endif
static void     cb_preview_got_page_size(GtkPrintOperationPreview*,
					 GtkPrintContext*,
					 GtkPageSetup*, gpointer);
static void     cb_preview_go_first(GtkButton*, gpointer);
static void     cb_preview_go_previous(GtkButton*, gpointer);
static void     cb_preview_go_next(GtkButton*, gpointer);
static void     cb_preview_go_last(GtkButton*, gpointer);
static void     cb_preview_btn_close(GtkButton*, gpointer);
static void     cb_preview_zoom_100(GtkButton*, gpointer);
static void     cb_preview_zoom_fit(GtkButton*, gpointer);
static void     cb_preview_zoom_in(GtkButton*, gpointer);
static void     cb_preview_zoom_out(GtkButton*, gpointer);
static void     cb_preview_request_page_setup(GtkPrintOperation*,
					      GtkPrintContext*,
 					      gint,GtkPageSetup*,gpointer);

static void     printing_preview_update_zoom_sensitivity(PreviewData*);

/* variables */
static GtkPrintSettings *settings   = NULL;
static GtkPageSetup     *page_setup = NULL;

/* other static functions */
static void     printing_layout_set_text_attributes(PrintData*, GtkPrintContext *, gboolean *);
static gboolean printing_is_pango_gdk_color_equal(PangoColor*, GdkColor*); 
static gint     printing_text_iter_get_offset_bytes(PrintData *, const GtkTextIter*);

#define PAGE_MARGIN_STORAGE_UNIT GTK_UNIT_MM
#define PREVIEW_SCALE 72
#define PREVIEW_SHADOW_OFFSET 3
#define PREVIEW_ZOOM_FAC 1.41
#define PREVIEW_ZOOM_MAX 10.
#define PREVIEW_ZOOM_MIN 0.2

static void free_pixbuf(gpointer key, gpointer value, gpointer data)
{
	PangoAttrShape *attr = (PangoAttrShape *) value;
	g_object_unref(G_OBJECT(attr->data));
}

gpointer printing_get_renderer_data(PrintData *print_data)
{
	if (!print_data)
		return NULL;
	return print_data->renderer_data;
}

gdouble printing_get_zoom(PrintData *print_data)
{
	if (!print_data)
		return 1.0;
	return print_data->zoom;
}

void printing_set_n_pages(PrintData *print_data, gint n_pages)
{
	if (!print_data)
		return;
	print_data->npages = n_pages;
}

GtkPrintSettings *printing_get_settings(void)
{
	if (settings == NULL) {
		settings = gtk_print_settings_new();
		gtk_print_settings_set_use_color(settings, prefs_common.print_use_color);
		gtk_print_settings_set_collate(settings, prefs_common.print_use_collate);
		gtk_print_settings_set_reverse(settings, prefs_common.print_use_reverse);
		gtk_print_settings_set_duplex(settings, prefs_common.print_use_duplex);
	}
	return settings;
}

void printing_store_settings(GtkPrintSettings *new_settings)
{
	if (settings != NULL)
		g_object_unref(settings);

	settings = g_object_ref(new_settings);
	prefs_common.print_use_color = gtk_print_settings_get_use_color(settings);
	prefs_common.print_use_collate = gtk_print_settings_get_collate(settings);
	prefs_common.print_use_reverse = gtk_print_settings_get_reverse(settings);
	prefs_common.print_use_duplex = gtk_print_settings_get_duplex(settings);
}

GtkPageSetup *printing_get_page_setup(void)
{
	if (page_setup == NULL) {
		gboolean read_from_file;
		char *page_setup_filename;
		GKeyFile *keyfile;
		gboolean key_file_read;

		page_setup = gtk_page_setup_new();

		read_from_file = FALSE;

		/* try reading the page setup from file */
		page_setup_filename = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, 
						  PRINTING_PAGE_SETUP_STORAGE_FILE, NULL);
		keyfile = g_key_file_new();
		key_file_read = g_key_file_load_from_file(keyfile, page_setup_filename,
							  0, NULL);
		g_free(page_setup_filename);

		if (key_file_read)
			read_from_file = gtk_page_setup_load_key_file(page_setup, keyfile,
								      NULL, NULL);
		g_key_file_free(keyfile);

		if (read_from_file) {
			debug_print("Printing: Read page setup from key file\n");
		} else {
			debug_print("Printing: Could not read page setup from key file\n");
		}

		/* if reading from file did not work, or has not been tried (GTK+ < 2.14), use prefs */
		if (!read_from_file) {
			/* paper size */
			if (prefs_common.print_paper_type &&
			    *prefs_common.print_paper_type) {
				GtkPaperSize *paper = gtk_paper_size_new(prefs_common.print_paper_type);
				gtk_page_setup_set_paper_size(page_setup, paper);
				gtk_paper_size_free(paper);
			}
			/* orientation */
			gtk_page_setup_set_orientation(page_setup,
						       prefs_common.print_paper_orientation);
			/* margins */
			if (prefs_common.print_margin_top != -1)
				gtk_page_setup_set_top_margin(page_setup,
							      0.01*prefs_common.print_margin_top,
						              PAGE_MARGIN_STORAGE_UNIT);
			if (prefs_common.print_margin_bottom != -1)
				gtk_page_setup_set_bottom_margin(page_setup,
								 0.01*prefs_common.print_margin_bottom,
								 PAGE_MARGIN_STORAGE_UNIT);
			if (prefs_common.print_margin_left != -1)
				gtk_page_setup_set_left_margin(page_setup,
							       0.01*prefs_common.print_margin_left,
							       PAGE_MARGIN_STORAGE_UNIT);
			if (prefs_common.print_margin_right != -1)
				gtk_page_setup_set_right_margin(page_setup,
								0.01*prefs_common.print_margin_right,
								PAGE_MARGIN_STORAGE_UNIT);
		}
	}
	return page_setup;
}

void printing_print_full(GtkWindow *parent, PrintRenderer *renderer, gpointer renderer_data, 
			 gint sel_start, gint sel_end)
{			 
	GtkPrintOperation *op;
	GtkPrintOperationResult res;
	PrintData *print_data;

	op = gtk_print_operation_new();

	print_data = g_new0(PrintData,1);

	print_data->renderer = renderer;
	print_data->renderer_data = renderer_data;
	print_data->sel_start = sel_start;
	print_data->sel_end = sel_end;

	print_data->zoom = 1.;

	print_data->images = g_hash_table_new(g_direct_hash, g_direct_equal);

	print_data->pango_context = renderer->get_pango_context(renderer_data);

	print_data->to_print = renderer->get_data_to_print(renderer_data, sel_start, sel_end);

	printing_get_settings();
	printing_get_page_setup();

	/* Config for printing */
	gtk_print_operation_set_print_settings(op, settings);
	gtk_print_operation_set_default_page_setup(op, page_setup);
#if GTK_CHECK_VERSION(2,18,0)
        /* enable Page Size and Orientation in the print dialog */
	gtk_print_operation_set_embed_page_setup(op, TRUE);
#endif
	/* signals */
	g_signal_connect(op, "begin_print", G_CALLBACK(renderer->cb_begin_print), print_data);
	g_signal_connect(op, "draw_page", G_CALLBACK(renderer->cb_draw_page), print_data);
	g_signal_connect(op, "preview", G_CALLBACK(cb_preview), print_data);

	/* Start printing process */
	res = gtk_print_operation_run(op, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
				      parent, NULL);

	if (res == GTK_PRINT_OPERATION_RESULT_ERROR) {
		GError *error = NULL;
		gtk_print_operation_get_error(op, &error);
		debug_print("Error printing message: %s\n",
			    error ? error->message : "no details");
	} else if (res == GTK_PRINT_OPERATION_RESULT_APPLY) {
		/* store settings for next printing session */
		printing_store_settings(gtk_print_operation_get_print_settings(op));
	}

	g_hash_table_foreach(print_data->images, free_pixbuf, NULL);
	g_hash_table_destroy(print_data->images);
	if (print_data->to_print)
		g_free(print_data->to_print);
	g_list_free(print_data->page_breaks);
	if (print_data->layout)
		g_object_unref(print_data->layout);

	g_free(print_data);

	g_object_unref(op);
	debug_print("printing_print finished\n");
}

static PangoContext *printing_textview_get_pango_context(gpointer data)
{
	return gtk_widget_get_pango_context(GTK_WIDGET(data));
}

static gpointer printing_textview_get_data_to_print(gpointer data, gint sel_start, gint sel_end)
{
	GtkTextView *text_view = GTK_TEXT_VIEW(data);
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(text_view);
	GtkTextIter start, end;

	if (sel_start < 0 || sel_end <= sel_start) {
		gtk_text_buffer_get_start_iter(buffer, &start);
		gtk_text_buffer_get_end_iter(buffer, &end);
	} else {
		gtk_text_buffer_get_iter_at_offset(buffer, &start, sel_start);
		gtk_text_buffer_get_iter_at_offset(buffer, &end, sel_end);
	}

	return gtk_text_buffer_get_text(buffer, &start, &end, FALSE);
}

void printing_print(GtkTextView *text_view, GtkWindow *parent, gint sel_start, gint sel_end)
{
	PrintRenderer *textview_renderer = g_new0(PrintRenderer, 1);
	
	textview_renderer->get_pango_context = printing_textview_get_pango_context;
	textview_renderer->get_data_to_print = printing_textview_get_data_to_print;
	textview_renderer->cb_begin_print    = printing_textview_cb_begin_print;
	textview_renderer->cb_draw_page      = printing_textview_cb_draw_page;

	printing_print_full(parent, textview_renderer, text_view, sel_start, sel_end);
	
	g_free(textview_renderer);
}

void printing_page_setup(GtkWindow *parent)
{
	GtkPageSetup *new_page_setup;
	char *keyfile;

	keyfile = NULL;

	printing_get_settings();
	printing_get_page_setup();

	new_page_setup = gtk_print_run_page_setup_dialog(parent,page_setup,settings);

	if (page_setup)
		g_object_unref(page_setup);

	page_setup = new_page_setup;

	g_free(prefs_common.print_paper_type);
	prefs_common.print_paper_type = g_strdup(gtk_paper_size_get_name(
						gtk_page_setup_get_paper_size(page_setup)));
	prefs_common.print_paper_orientation = gtk_page_setup_get_orientation(page_setup);
	/* store 100th millimeters */
	prefs_common.print_margin_top    = (int) (100*gtk_page_setup_get_top_margin(page_setup, 
								PAGE_MARGIN_STORAGE_UNIT));
	prefs_common.print_margin_bottom = (int) (100*gtk_page_setup_get_bottom_margin(page_setup,
								PAGE_MARGIN_STORAGE_UNIT));
	prefs_common.print_margin_left   = (int) (100*gtk_page_setup_get_left_margin(page_setup,
								PAGE_MARGIN_STORAGE_UNIT));
	prefs_common.print_margin_right  = (int) (100*gtk_page_setup_get_right_margin(page_setup,
								PAGE_MARGIN_STORAGE_UNIT));

	/* save to file */
	keyfile = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
			      PRINTING_PAGE_SETUP_STORAGE_FILE, NULL);
	if (!gtk_page_setup_to_file(page_setup, keyfile, NULL)) {
		debug_print("Printing: Could not store page setup in file `%s'\n", keyfile);
	}
	g_free(keyfile);
}

static gboolean cb_preview(GtkPrintOperation        *operation,
			   GtkPrintOperationPreview *preview,
			   GtkPrintContext          *context,
			   GtkWindow                *parent,
			   gpointer                 data)
{
	PrintData *print_data;
	cairo_t *cr;
	PreviewData *preview_data;
	GtkWidget *vbox;
	GtkWidget *toolbar;
	GtkWidget *da;
	GtkWidget *sw;
	GtkWidget *page;
	GtkToolItem *separator;
	static GdkGeometry geometry;
	GtkWidget *dialog = NULL;
	GtkWidget *statusbar = gtk_hbox_new(2, FALSE);

	debug_print("Creating internal print preview\n");

	print_data = (PrintData*) data;

	preview_data = g_new0(PreviewData,1);
	preview_data->print_data = print_data;
	preview_data->op = g_object_ref(operation);
	preview_data->preview = preview;

	/* Window */
	dialog = gtkut_window_new(GTK_WINDOW_TOPLEVEL, "print_preview");
	preview_data->dialog = dialog;
	if (!geometry.min_height) {
		geometry.min_width = 600;
		geometry.min_height = 400;
	}
	gtk_window_set_geometry_hints(GTK_WINDOW(dialog), NULL, &geometry,
				      GDK_HINT_MIN_SIZE);
	gtk_widget_set_size_request(dialog, prefs_common.print_previewwin_width,
				    prefs_common.print_previewwin_height);
	gtk_window_set_title(GTK_WINDOW(dialog), _("Print preview"));

	/* vbox */
	vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(dialog), vbox);
  
	/* toolbar */
	toolbar = gtk_toolbar_new();
#if (GTK_CHECK_VERSION(2,16,0))
  	gtk_orientable_set_orientation(GTK_ORIENTABLE(toolbar), GTK_ORIENTATION_HORIZONTAL);
#else
	gtk_toolbar_set_orientation(GTK_TOOLBAR(toolbar), GTK_ORIENTATION_HORIZONTAL);
#endif
	switch (prefs_common.toolbar_style) {
		case TOOLBAR_ICON:
			gtk_toolbar_set_style(GTK_TOOLBAR(toolbar), GTK_TOOLBAR_ICONS);
			break;
		case TOOLBAR_TEXT:
			gtk_toolbar_set_style(GTK_TOOLBAR(toolbar), GTK_TOOLBAR_TEXT);
			break;
		case TOOLBAR_BOTH_HORIZ:
			gtk_toolbar_set_style(GTK_TOOLBAR(toolbar), GTK_TOOLBAR_BOTH_HORIZ);
			break;
		case TOOLBAR_BOTH:
			default:	  
			gtk_toolbar_set_style(GTK_TOOLBAR(toolbar), GTK_TOOLBAR_BOTH);
	}
	gtk_toolbar_set_show_arrow(GTK_TOOLBAR(toolbar), TRUE);

	gtk_box_pack_start(GTK_BOX(vbox), toolbar, FALSE, FALSE, 0);

#if !(GTK_CHECK_VERSION(2,12,0))
#define CLAWS_SET_TOOL_ITEM_TIP(widget,tip) { \
	gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(widget), GTK_TOOLTIPS(tips),			\
			tip, NULL);								\
}
#else
#define CLAWS_SET_TOOL_ITEM_TIP(widget,tip) { \
	gtk_tool_item_set_tooltip_text(GTK_TOOL_ITEM(widget), tip);				\
}
#endif	

#define TOOLBAR_ITEM(item,text,tooltip,cb,cbdata) {								\
	item = GTK_WIDGET(gtk_tool_button_new_from_stock(text));					\
	gtk_tool_item_set_homogeneous(GTK_TOOL_ITEM(item), FALSE);					\
	gtk_tool_item_set_is_important(GTK_TOOL_ITEM(item), TRUE);					\
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), GTK_TOOL_ITEM(item), -1);				\
	g_signal_connect (G_OBJECT(item), "clicked", G_CALLBACK(cb), cbdata);				\
	CLAWS_SET_TOOL_ITEM_TIP(GTK_TOOL_ITEM(item),		\
			tooltip);									\
}

	TOOLBAR_ITEM(preview_data->first, GTK_STOCK_GOTO_FIRST,
		     _("First page"), cb_preview_go_first, preview_data);
	TOOLBAR_ITEM(preview_data->previous, GTK_STOCK_GO_BACK,
		     _("Previous page"), cb_preview_go_previous, preview_data);

	page = gtk_label_new("");
	gtk_widget_set_size_request(page, 100, -1);
	preview_data->page_nr_label = page;

	TOOLBAR_ITEM(preview_data->next, GTK_STOCK_GO_FORWARD,
		     _("Next page"), cb_preview_go_next, preview_data);
	TOOLBAR_ITEM(preview_data->last, GTK_STOCK_GOTO_LAST,
		     _("Last page"), cb_preview_go_last, preview_data);

	separator = gtk_separator_tool_item_new();
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), GTK_TOOL_ITEM(separator), -1);

	TOOLBAR_ITEM(preview_data->zoom_100, GTK_STOCK_ZOOM_100,
		     _("Zoom 100%"), cb_preview_zoom_100, preview_data);
	TOOLBAR_ITEM(preview_data->zoom_fit, GTK_STOCK_ZOOM_FIT,
		     _("Zoom fit"), cb_preview_zoom_fit, preview_data);
	TOOLBAR_ITEM(preview_data->zoom_in, GTK_STOCK_ZOOM_IN,
		     _("Zoom in"), cb_preview_zoom_in, preview_data);
	TOOLBAR_ITEM(preview_data->zoom_out, GTK_STOCK_ZOOM_OUT,
		     _("Zoom out"), cb_preview_zoom_out, preview_data);

	separator = gtk_separator_tool_item_new();
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), GTK_TOOL_ITEM(separator), -1);

	/* tooltip has to be NULL else it triggers an expose_event */
	TOOLBAR_ITEM(preview_data->close, GTK_STOCK_CLOSE, NULL,
		     cb_preview_btn_close, preview_data);

	gtk_widget_show(statusbar);
	gtk_box_pack_start(GTK_BOX(vbox), statusbar, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(statusbar), page, FALSE, FALSE, 0);
	/* Drawing area */
	sw = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw),
				       GTK_POLICY_AUTOMATIC,
				       GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(vbox), sw, TRUE, TRUE, 0);
	da = gtk_drawing_area_new();
	gtk_widget_set_double_buffered(da, FALSE);
	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(sw),
					      da);
	gtk_widget_realize(da);
	preview_data->scrolled_window = sw;
	preview_data->area = da;

	/* cairo context */
	cr = gdk_cairo_create(gtk_widget_get_window(da));
	gtk_print_context_set_cairo_context(context, cr, PREVIEW_SCALE, PREVIEW_SCALE);
	cairo_destroy(cr);

	/* signals */
	g_signal_connect(dialog, "key_press_event",
			 G_CALLBACK(cb_preview_close), preview_data);
	g_signal_connect(dialog, "size_allocate",
			 G_CALLBACK(cb_preview_size_allocate), NULL);
	g_signal_connect(dialog, "destroy",G_CALLBACK(cb_preview_destroy),
			 preview_data);
	g_signal_connect(preview, "ready", G_CALLBACK(cb_preview_ready),
			 preview_data);
	g_signal_connect(preview, "got-page-size",
			 G_CALLBACK(cb_preview_got_page_size), preview_data);

	g_signal_connect(operation, "request-page-setup",
			 G_CALLBACK(cb_preview_request_page_setup), preview_data);

	gtk_widget_show_all(dialog);
	return TRUE;
}

static void cb_preview_destroy(GtkWindow *window, gpointer data)
{
	PreviewData *preview_data;
	preview_data = (PreviewData*) data;

	if (preview_data->rendering)
  		return;
	debug_print("Preview window destroyed\n");

	gtk_print_operation_preview_end_preview(preview_data->preview);
	g_object_unref(preview_data->op);
	g_list_free(preview_data->pages_to_print);

	g_free(preview_data);
}

static gboolean cb_preview_close(GtkWidget *widget, GdkEventAny *event,
				 gpointer data)
{
	PreviewData *preview_data = (PreviewData *)data;
	if (event->type == GDK_KEY_PRESS)
 		 if (((GdkEventKey *)event)->keyval != GDK_KEY_Escape)
    			return FALSE;
	if (preview_data->rendering)
		return FALSE; 
	gtk_widget_destroy(widget);
	return FALSE;
}

static void cb_preview_size_allocate(GtkWidget *widget,
				     GtkAllocation *allocation)
{
	cm_return_if_fail(allocation != NULL);

	prefs_common.print_previewwin_width = allocation->width;
	prefs_common.print_previewwin_height = allocation->height;
}

static void cb_preview_ready(GtkPrintOperationPreview *preview,
			     GtkPrintContext          *context,
			     gpointer                  data)
{
	PreviewData *preview_data;
	gint iPage;
	preview_data = (PreviewData*) data;
	debug_print("preview_ready %d\n", preview_data->print_data->npages);

	for (iPage = 0; iPage < (preview_data->print_data->npages); iPage++) {
		if (gtk_print_operation_preview_is_selected(preview_data->preview, iPage)) {
			preview_data->pages_to_print = 
				g_list_prepend(preview_data->pages_to_print,
					       GINT_TO_POINTER(iPage));
			debug_print("want to print page %d\n",iPage+1);
		}
	}

	preview_data->pages_to_print = g_list_reverse(preview_data->pages_to_print);
	preview_data->current_page = preview_data->pages_to_print;
	preview_data->context = context;

#if !GTK_CHECK_VERSION(3, 0, 0)
	g_signal_connect(preview_data->area, "expose_event",
			 G_CALLBACK(cb_preview_expose),
			 preview_data);
#else
	g_signal_connect(preview_data->area, "draw",
			 G_CALLBACK(cb_preview_expose),
			 preview_data);
#endif

	gtk_widget_queue_draw(preview_data->area);
}

static void cb_preview_got_page_size(GtkPrintOperationPreview *preview,
				     GtkPrintContext          *context,
				     GtkPageSetup             *page_setup,
				     gpointer                  data)
{
	PreviewData *preview_data;
	GtkPaperSize *paper_size;
	gint paper_width;
	gint paper_height;

	preview_data = (PreviewData*) data;
	debug_print("got_page_size\n");
	paper_size   = gtk_page_setup_get_paper_size(page_setup);
	paper_width  = (gint)(gtk_paper_size_get_width(paper_size, GTK_UNIT_INCH)  
			      * PREVIEW_SCALE);
	paper_height = (gint)(gtk_paper_size_get_height(paper_size,  GTK_UNIT_INCH)
			      * PREVIEW_SCALE);

	preview_data->page_width  = paper_width;
	preview_data->page_height = paper_height;

	debug_print("w/h %d/%d\n", paper_width, paper_height);
	gtk_widget_set_size_request(GTK_WIDGET(preview_data->area), 
				    paper_width, paper_height);
}

#if !GTK_CHECK_VERSION(3, 0, 0)
static gboolean cb_preview_expose(GtkWidget *widget, GdkEventExpose *event,
				  gpointer data)
#else
static gboolean cb_preview_expose(GtkWidget *widget, cairo_t *event,
				  gpointer data)
#endif
{
	PreviewData *preview_data = data;
#if !GTK_CHECK_VERSION(3, 0, 0)
	cairo_t *cr;
#endif

	debug_print("preview_expose (current %p)\n", preview_data->current_page);

#if !GTK_CHECK_VERSION(3, 0, 0)
	cr = gdk_cairo_create(gtk_widget_get_window(preview_data->area));
#endif

	/* background */
	cairo_set_source_rgb(cr, 0.5, 0.5, 0.5);
#if !GTK_CHECK_VERSION(3, 0, 0)
	cairo_rectangle(cr, event->area.x, event->area.y, event->area.width, event->area.height);
#else
	cairo_rectangle(cr, 0, 0, event->area.width, event->area.height);
#endif
	cairo_fill(cr);

	/* shadow */
	cairo_set_source_rgb(cr, 0., 0., 0.);
	cairo_rectangle(cr, PREVIEW_SHADOW_OFFSET, PREVIEW_SHADOW_OFFSET,
	    preview_data->page_width+PREVIEW_SHADOW_OFFSET,
	    preview_data->page_height+PREVIEW_SHADOW_OFFSET);
	cairo_fill(cr);

	/* paper */
	cairo_set_source_rgb(cr, 1., 1., 1.);
	cairo_rectangle(cr, 0, 0,
	    preview_data->page_width,
	    preview_data->page_height);
	cairo_fill(cr);

	gtk_print_context_set_cairo_context(preview_data->context, cr, PREVIEW_SCALE, PREVIEW_SCALE);
#if !GTK_CHECK_VERSION(3, 0, 0)
	cairo_destroy(cr);
#endif

	if (preview_data->current_page) {
		preview_data->rendering = TRUE;
		gtk_widget_set_sensitive(preview_data->close, FALSE);
		int cur = GPOINTER_TO_INT(preview_data->current_page->data);
		gchar *str;
		str = g_strdup_printf(_("Page %d"), cur+1);
		gtk_label_set_text(GTK_LABEL(preview_data->page_nr_label), str);
		g_free(str);
		gtk_print_operation_preview_render_page(preview_data->preview,
							GPOINTER_TO_INT
							(preview_data->current_page->data));

		gtk_widget_set_sensitive(preview_data->first,
					 preview_data->current_page->prev != NULL);
		gtk_widget_set_sensitive(preview_data->previous,
					 preview_data->current_page->prev != NULL);
		gtk_widget_set_sensitive(preview_data->next,
					 preview_data->current_page->next != NULL);
		gtk_widget_set_sensitive(preview_data->last,
					 preview_data->current_page->next != NULL);
		gtk_widget_set_sensitive(preview_data->close, TRUE);
		preview_data->rendering = FALSE;
	}
	return TRUE;
}

static void cb_preview_go_first(GtkButton *button, gpointer data)
{
	PreviewData *preview_data = (PreviewData*) data;
	preview_data->current_page = preview_data->pages_to_print;
	gtk_widget_queue_draw(preview_data->area);
}

static void cb_preview_go_previous(GtkButton *button, gpointer data)
{
	GList *next;
	PreviewData *preview_data = (PreviewData*) data;
	next = g_list_previous(preview_data->current_page);
	if (next)
		preview_data->current_page = next;
	gtk_widget_queue_draw(preview_data->area);
}

static void cb_preview_go_next(GtkButton *button, gpointer data)
{
	GList *next;
	PreviewData *preview_data = (PreviewData*) data;
	next = g_list_next(preview_data->current_page);
	if (next)
		preview_data->current_page = next;
	gtk_widget_queue_draw(preview_data->area);
}

static void cb_preview_go_last(GtkButton *button, gpointer data)
{
	PreviewData *preview_data = (PreviewData*) data;
	preview_data->current_page = g_list_last(preview_data->current_page);
	gtk_widget_queue_draw(preview_data->area);
}

static void cb_preview_btn_close(GtkButton *button, gpointer data)
{
	PreviewData *preview_data = (PreviewData *)data;
	if (preview_data->rendering)
		return; 
	gtk_widget_destroy(preview_data->dialog);
}

static void cb_preview_zoom_100(GtkButton *button, gpointer data)
{
	PreviewData *preview_data = (PreviewData*) data;
	if (preview_data->print_data->zoom != 1.) {
		preview_data->print_data->zoom = 1.;
		gtk_widget_queue_draw(preview_data->area);
		printing_preview_update_zoom_sensitivity(preview_data);
	}
}

static void cb_preview_zoom_fit(GtkButton *button, gpointer data)
{
	PreviewData *preview_data = (PreviewData*) data;
	GtkAllocation allocation;
	gdouble zoom_w;
	gdouble zoom_h;

	gtk_widget_get_allocation(preview_data->scrolled_window, &allocation);
	zoom_w = ((gdouble)allocation.width) /
		 ((gdouble)preview_data->page_width/preview_data->print_data->zoom +
		  PREVIEW_SHADOW_OFFSET);
	zoom_h = ((gdouble)allocation.height) /
		 ((gdouble)preview_data->page_height/preview_data->print_data->zoom +
		  PREVIEW_SHADOW_OFFSET);

	preview_data->print_data->zoom = MIN(zoom_w,zoom_h) - 0.01;

	if (preview_data->print_data->zoom > PREVIEW_ZOOM_MAX)
		preview_data->print_data->zoom = PREVIEW_ZOOM_MAX;
	else if (preview_data->print_data->zoom < PREVIEW_ZOOM_MIN)
		preview_data->print_data->zoom = PREVIEW_ZOOM_MIN;

	printing_preview_update_zoom_sensitivity(preview_data);
	gtk_widget_queue_draw(preview_data->area);
}

static void cb_preview_zoom_in(GtkButton *button, gpointer data)
{
	PreviewData *preview_data = (PreviewData*) data;
	gdouble new_zoom;
	new_zoom =  preview_data->print_data->zoom * PREVIEW_ZOOM_FAC;
	if (new_zoom <= PREVIEW_ZOOM_MAX) {
		preview_data->print_data->zoom = new_zoom;
		printing_preview_update_zoom_sensitivity(preview_data);
		gtk_widget_queue_draw(preview_data->area);
	}
}

static void cb_preview_zoom_out(GtkButton *button, gpointer data)
{
	PreviewData *preview_data = (PreviewData*) data;
	gdouble new_zoom;
	new_zoom =  preview_data->print_data->zoom / PREVIEW_ZOOM_FAC;
	if (new_zoom >= PREVIEW_ZOOM_MIN) {
		preview_data->print_data->zoom = new_zoom;
		printing_preview_update_zoom_sensitivity(preview_data);
		gtk_widget_queue_draw(preview_data->area);
	}
}

static void cb_preview_request_page_setup(GtkPrintOperation *op,
					  GtkPrintContext *context,
					  gint page_nr,
					  GtkPageSetup *setup,gpointer data)
{
	GtkPaperSize *paper_size;
	GtkPaperSize *old_size;
	gdouble width;
	gdouble height;
	gdouble top_margin;
	gdouble bottom_margin;
	gdouble left_margin;
	gdouble right_margin;

	PreviewData *preview_data = (PreviewData*) data;

	old_size = gtk_page_setup_get_paper_size(setup);
	width  = gtk_paper_size_get_width(old_size,GTK_UNIT_INCH);
	height = gtk_paper_size_get_height(old_size,GTK_UNIT_INCH);

	top_margin    = gtk_page_setup_get_top_margin(setup,GTK_UNIT_INCH);
	bottom_margin = gtk_page_setup_get_bottom_margin(setup,GTK_UNIT_INCH);
	left_margin   = gtk_page_setup_get_left_margin(setup,GTK_UNIT_INCH);
	right_margin  = gtk_page_setup_get_right_margin(setup,GTK_UNIT_INCH);

	paper_size = gtk_paper_size_new_custom("preview paper", "preview_paper",
					       width*preview_data->print_data->zoom,
					       height*preview_data->print_data->zoom,
					       GTK_UNIT_INCH);
	gtk_page_setup_set_paper_size(setup, paper_size);
	gtk_paper_size_free(paper_size);

	gtk_page_setup_set_top_margin(setup,top_margin*preview_data->print_data->zoom,
				      GTK_UNIT_INCH);
	gtk_page_setup_set_bottom_margin(setup,bottom_margin*preview_data->print_data->zoom,
					 GTK_UNIT_INCH);
	gtk_page_setup_set_left_margin(setup,left_margin*preview_data->print_data->zoom,
				       GTK_UNIT_INCH);
	gtk_page_setup_set_right_margin(setup,right_margin*preview_data->print_data->zoom,
					GTK_UNIT_INCH);
}

static void printing_textview_cb_begin_print(GtkPrintOperation *op, GtkPrintContext *context,
			   gpointer user_data)
{
	double width, height;
	int num_lines;
	double page_height;
	GList *page_breaks;
	PrintData *print_data;
	PangoFontDescription *desc;
	int start, ii;
	PangoLayoutIter *iter;
	gint header_end_pos;
	gint num_header_lines;
	gint dummy;
	gboolean header_done;
	gboolean has_headers = FALSE;
	const gchar *text;
	double line_height =0.;

	print_data = (PrintData*) user_data;

	debug_print("Preparing print job...\n");

	width  = gtk_print_context_get_width(context);
	height = gtk_print_context_get_height(context);

	if (print_data->layout == NULL)
  		print_data->layout = gtk_print_context_create_pango_layout(context);

	if (prefs_common.use_different_print_font)
  		desc = pango_font_description_from_string(prefs_common.printfont);
	else
		desc = pango_font_description_copy(
	pango_context_get_font_description(print_data->pango_context));

	pango_layout_set_font_description(print_data->layout, desc);
	pango_font_description_free(desc);

	pango_layout_set_width(print_data->layout, width * PANGO_SCALE);
	pango_layout_set_text(print_data->layout, (char *)print_data->to_print, -1);

	printing_layout_set_text_attributes(print_data, context, &has_headers);

	num_lines = pango_layout_get_line_count(print_data->layout);

	page_breaks = NULL;
	page_height = 0;
	start = 0;
	ii = 0;
	iter = pango_layout_get_iter(print_data->layout);

	/* find the last character of the header */
	header_end_pos = 0;
	header_done = FALSE;
	text = pango_layout_get_text(print_data->layout);

	if (has_headers) {
		if (text && *text && *text != '\n') {
			do {
				if (text[0] == '\n' && (text[1] != '\0') && (text[1] == '\n'))
					header_done = TRUE;
				else
					header_end_pos++;
				text++;
			} while(*text && !header_done);
		}
		/* find line number for header end */
		pango_layout_index_to_line_x(print_data->layout, header_end_pos, 1,
					     &num_header_lines, &dummy);
		/* line count is zero-based */
		num_header_lines++;
  	} else {
		print_data->ypos_line = -1.0;
	}

	do {
		PangoRectangle logical_rect;
		PangoAttrShape *attr = NULL;

		if (ii >= start) {
			pango_layout_iter_get_line_extents(iter, NULL, &logical_rect);

			if ((attr = g_hash_table_lookup(print_data->images,
						GINT_TO_POINTER(pango_layout_iter_get_index(iter)))) != NULL) {
				line_height = (double)gdk_pixbuf_get_height(GDK_PIXBUF(attr->data));
			} else {
				line_height = ((double)logical_rect.height) / PANGO_SCALE;
			}
		}
		if ((page_height + line_height) > height) {
			page_breaks = g_list_prepend(page_breaks, GINT_TO_POINTER(ii));
			page_height = 0;
		}

		if (has_headers && ii == num_header_lines) {
			int y0, y1;
			pango_layout_iter_get_line_yrange(iter,&y0,&y1);
			print_data->ypos_line = (double)y0 + 1./3.*((double)(y1 - y0))/2.;
		}

		page_height += line_height;
		ii++;
	} while(ii < num_lines && pango_layout_iter_next_line(iter));
	pango_layout_iter_free(iter);

	page_breaks = g_list_reverse(page_breaks);
	print_data->npages = g_list_length(page_breaks) + 1;	
	print_data->page_breaks = page_breaks;

	gtk_print_operation_set_n_pages(op, print_data->npages);

	debug_print("Starting print job...\n");
}

static cairo_surface_t *pixbuf_to_surface(GdkPixbuf *pixbuf)
{
	  cairo_surface_t *surface;
	  cairo_format_t format;
	  static const cairo_user_data_key_t key;
	  guchar *pixels = g_malloc(
	  			4*
				gdk_pixbuf_get_width(pixbuf)*
				gdk_pixbuf_get_height(pixbuf));
          guchar *src_pixels = gdk_pixbuf_get_pixels (pixbuf);
	  gint width = gdk_pixbuf_get_width(pixbuf);
	  gint height = gdk_pixbuf_get_height(pixbuf);
          gint nchans = gdk_pixbuf_get_n_channels (pixbuf);
	  gint stride = gdk_pixbuf_get_rowstride (pixbuf);
	  gint j;

	  if (nchans == 3)
	    format = CAIRO_FORMAT_RGB24;
	  else
	    format = CAIRO_FORMAT_ARGB32;
	  surface = cairo_image_surface_create_for_data (pixels, 	 
						format, width, height, 4*width);
          cairo_surface_set_user_data (surface, &key, 
	  	pixels, (cairo_destroy_func_t)g_free);

	  for (j = height; j; j--) {
		guchar *p = src_pixels;
		guchar *q = pixels;

		if (nchans == 3) {
			guchar *end = p + 3 * width;

			while (p < end) {
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
				q[0] = p[2];
				q[1] = p[1];
				q[2] = p[0];
#else
				q[1] = p[0];
				q[2] = p[1];
				q[3] = p[2];
#endif
				p += 3;
				q += 4;
			}
		} else {
			guchar *end = p + 4 * width;
			guint t1,t2,t3;

#define MULT(d,c,a,t) G_STMT_START { t = c * a + 0x7f; d = ((t >> 8) + t) >> 8; } G_STMT_END

			while (p < end) {
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
				MULT(q[0], p[2], p[3], t1);
				MULT(q[1], p[1], p[3], t2);
				MULT(q[2], p[0], p[3], t3);
				q[3] = p[3];
#else
				q[0] = p[3];
				MULT(q[1], p[0], p[3], t1);
				MULT(q[2], p[1], p[3], t2);
				MULT(q[3], p[2], p[3], t3);
#endif

				p += 4;
				q += 4;
			}

#undef MULT
		}

		src_pixels += stride;
		pixels += 4 * width;
	}
		
	return surface;
}

static void printing_textview_cb_draw_page(GtkPrintOperation *op, GtkPrintContext *context,
			 int page_nr, gpointer user_data)
{
	cairo_t *cr;
	PrintData *print_data;
	int start, end, ii;
	GList *pagebreak;
	PangoLayoutIter *iter;
	double start_pos;
	gboolean notlast = TRUE;

	print_data = (PrintData*) user_data;

	if (page_nr == 0) {
		start = 0;
	} else {
		pagebreak = g_list_nth(print_data->page_breaks, page_nr - 1);
		start = GPOINTER_TO_INT(pagebreak->data);
	}

	pagebreak = g_list_nth(print_data->page_breaks, page_nr);
	if (pagebreak == NULL)
		end = pango_layout_get_line_count(print_data->layout);
	else
		end = GPOINTER_TO_INT(pagebreak->data);

	cr = gtk_print_context_get_cairo_context(context);
	cairo_scale(cr, print_data->zoom, print_data->zoom);
	cairo_set_source_rgb(cr, 0., 0., 0.);

	ii = 0;
	start_pos = 0.;
	iter = pango_layout_get_iter(print_data->layout);
	do {
		PangoRectangle logical_rect;
		PangoLayoutLine *line;
		PangoAttrShape *attr = NULL;
		int baseline;

		if (ii >= start) {
			line = pango_layout_iter_get_line(iter);

			pango_layout_iter_get_line_extents(iter, NULL, &logical_rect);
			baseline = pango_layout_iter_get_baseline(iter);

			if (ii == start)
				start_pos = ((double)logical_rect.y) / PANGO_SCALE;

			/* Draw header separator line */
			if (ii == 0 && print_data->ypos_line >= 0) {
				cairo_surface_t *surface;
				surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
				     		gtk_print_context_get_width(context)/print_data->zoom,
				     		gtk_print_context_get_height(context)/print_data->zoom);
				cairo_set_line_width(cr, .5);
				cairo_set_source_surface(cr, surface,
							 ((double)logical_rect.x) / PANGO_SCALE, 
							 ((double)baseline) / PANGO_SCALE - start_pos);
				cairo_move_to(cr, ((double)logical_rect.x) / PANGO_SCALE,
					      (double)print_data->ypos_line / PANGO_SCALE);
				cairo_rel_line_to(cr, gtk_print_context_get_width(context)/print_data->zoom, 0);
				cairo_set_source_rgb(cr, 0., 0., 0.);
				cairo_stroke(cr);
				cairo_surface_destroy(surface);
			}

			cairo_move_to(cr,
				      ((double)logical_rect.x) / PANGO_SCALE,
				      ((double)baseline) / PANGO_SCALE - start_pos);

			if ((attr = g_hash_table_lookup(print_data->images,
			     GINT_TO_POINTER(pango_layout_iter_get_index(iter)))) != NULL) {
				cairo_surface_t *surface;

				surface = pixbuf_to_surface(GDK_PIXBUF(attr->data));
				cairo_set_source_surface (cr, surface, 
							  ((double)logical_rect.x) / PANGO_SCALE, 
							  ((double)baseline) / PANGO_SCALE - start_pos);
				cairo_paint (cr);
				cairo_surface_destroy (surface);
			} else {
				pango_cairo_show_layout_line(cr, line);
			}
		}
		ii++;
	} while(ii < end && (notlast = pango_layout_iter_next_line(iter)));
	pango_layout_iter_free(iter);
	debug_print("Sent page %d to printer\n", page_nr+1);
}

static void printing_layout_set_text_attributes(PrintData *print_data,
						GtkPrintContext *context,
						gboolean *has_headers)
{
	GtkTextIter iter;
	PangoAttrList *attr_list;
	PangoAttribute *attr;
	GSList *open_attrs, *attr_walk;
	GtkTextView *text_view = GTK_TEXT_VIEW(print_data->renderer_data);
	GtkTextBuffer *buffer = gtk_text_view_get_buffer(text_view);

	*has_headers = FALSE;

	attr_list = pango_attr_list_new();
	if (print_data->sel_start < 0 || print_data->sel_end <= print_data->sel_start) {
		gtk_text_buffer_get_start_iter(buffer, &iter);
	} else {
		gtk_text_buffer_get_iter_at_offset(buffer, &iter, print_data->sel_start);
	}

	open_attrs = NULL;
	do {
		gboolean fg_set, bg_set, under_set, strike_set, weight_set;
		GSList *tags, *tag_walk;
		GtkTextTag *tag;
		GdkColor *color = NULL;
		PangoUnderline underline;
		gboolean strikethrough;
		gint weight;
		GdkPixbuf *image;

		if (prefs_common.print_imgs && (image = gtk_text_iter_get_pixbuf(&iter)) != NULL) {
			PangoRectangle rect = {0, 0, 0, 0};
			gint startpos = printing_text_iter_get_offset_bytes(print_data, &iter);
			gint h = gdk_pixbuf_get_height(image);
			gint w = gdk_pixbuf_get_width(image);
			gint a_h = gtk_print_context_get_height(context);
			gint a_w = gtk_print_context_get_width(context);
			gint r_h, r_w;
			GdkPixbuf *scaled = NULL;
			image_viewer_get_resized_size(w, h, a_w, a_h, &r_w, &r_h);
			rect.x = 0;
			rect.y = 0;
			rect.width = r_w * PANGO_SCALE;
			rect.height = r_h * PANGO_SCALE;
      
			scaled = gdk_pixbuf_scale_simple(image, r_w, r_h, GDK_INTERP_BILINEAR);
			attr = pango_attr_shape_new_with_data (&rect, &rect,
							       scaled, NULL, NULL);
			attr->start_index = startpos;
			attr->end_index = startpos+1;
			pango_attr_list_insert(attr_list, attr);
			g_hash_table_insert(print_data->images, GINT_TO_POINTER(startpos), attr);
			print_data->img_cnt++;
		}

		if (gtk_text_iter_ends_tag(&iter, NULL)) {
			PangoAttrColor *attr_color;
			PangoAttrInt   *attr_int;

			tags = gtk_text_iter_get_toggled_tags(&iter, FALSE);
			for (tag_walk = tags; tag_walk != NULL; tag_walk = tag_walk->next) {
				gboolean found;

				tag = GTK_TEXT_TAG(tag_walk->data);
				g_object_get(G_OBJECT(tag),
					     "background-set", &bg_set,
					     "foreground-set", &fg_set,
					     "underline-set",&under_set,
					     "strikethrough-set", &strike_set,
					     "weight-set", &weight_set,
					     NULL);

				if (fg_set) {
					found = FALSE;
					for (attr_walk = open_attrs; attr_walk != NULL;
					     attr_walk = attr_walk->next) {
						attr = (PangoAttribute*)attr_walk->data;
						if (attr->klass->type == PANGO_ATTR_FOREGROUND) {
							attr_color = (PangoAttrColor*) attr;
							g_object_get(G_OBJECT(tag), "foreground_gdk",
								     &color, NULL);
							if (color && 
							    printing_is_pango_gdk_color_equal(&(attr_color->color),
							    color)) {
								attr->end_index = printing_text_iter_get_offset_bytes(print_data, &iter);
								pango_attr_list_insert(attr_list, attr);
								found = TRUE;
								open_attrs = g_slist_delete_link(open_attrs, attr_walk);
								break;
							}
							if (color)
								gdk_color_free(color);
						}
					}
					if (!found)
						debug_print("Error generating attribute list.\n");
				}

				if (bg_set) {
					found = FALSE;
					for (attr_walk = open_attrs; attr_walk != NULL;
					     attr_walk = attr_walk->next) {
						attr = (PangoAttribute*)attr_walk->data;
						if (attr->klass->type == PANGO_ATTR_BACKGROUND) {
							attr_color = (PangoAttrColor*) attr;
							g_object_get(G_OBJECT(tag), "background-gdk",
								     &color, NULL);
							if (printing_is_pango_gdk_color_equal(&(attr_color->color),
							    color)) {
								attr->end_index = printing_text_iter_get_offset_bytes(print_data, &iter);
								pango_attr_list_insert(attr_list, attr);
								found = TRUE;
								open_attrs = g_slist_delete_link(open_attrs, attr_walk);
								break;
							}
							if (color)
								gdk_color_free(color);
	    					}
					}
					if (!found)
						debug_print("Error generating attribute list.\n");
				}

				if (under_set) {
					found = FALSE;
					for (attr_walk = open_attrs; attr_walk != NULL;
					     attr_walk = attr_walk->next) {
						attr = (PangoAttribute*)attr_walk->data;
						if (attr->klass->type == PANGO_ATTR_UNDERLINE) {
							attr_int = (PangoAttrInt*)attr;
							g_object_get(G_OBJECT(tag), "underline",
								     &underline, NULL);
							if (attr_int->value == underline) {
								attr->end_index = printing_text_iter_get_offset_bytes(print_data, &iter);
								pango_attr_list_insert(attr_list, attr);
								found = TRUE;
								open_attrs = g_slist_delete_link(open_attrs, attr_walk);
								break;
							}
						}
					}
					if (!found)
						debug_print("Error generating attribute list.\n");
				}

				if (strike_set) {
					found = FALSE;
					for (attr_walk = open_attrs; attr_walk != NULL;
					     attr_walk = attr_walk->next) {
						attr = (PangoAttribute*)attr_walk->data;
						if (attr->klass->type == PANGO_ATTR_STRIKETHROUGH) {
							attr_int = (PangoAttrInt*)attr;
							g_object_get(G_OBJECT(tag), "strikethrough",
								     &strikethrough, NULL);
							if (attr_int->value == strikethrough) {
								attr->end_index = printing_text_iter_get_offset_bytes(print_data, &iter);
								pango_attr_list_insert(attr_list, attr);
								found = TRUE;
								open_attrs = g_slist_delete_link(open_attrs, attr_walk);
								break;
							}
						}
					}
					if (!found)
						debug_print("Error generating attribute list.\n");
				}

				if (weight_set) {
					found = FALSE;
					for (attr_walk = open_attrs; attr_walk != NULL;
					    attr_walk = attr_walk->next) {
						attr = (PangoAttribute*)attr_walk->data;
						if (attr->klass->type == PANGO_ATTR_WEIGHT) {
								attr_int = (PangoAttrInt*)attr;
							g_object_get(G_OBJECT(tag), "weight", &weight, NULL);
							if (attr_int->value == weight) {
								attr->end_index = printing_text_iter_get_offset_bytes(print_data, &iter);
								pango_attr_list_insert(attr_list, attr);
								found = TRUE;
								open_attrs = g_slist_delete_link(open_attrs, attr_walk);
								break;
							}
						}
					}
					if (!found)
						debug_print("Error generating attribute list.\n");
				}
			}
			g_slist_free(tags);
		}

		if (gtk_text_iter_begins_tag(&iter, NULL)) {
			tags = gtk_text_iter_get_toggled_tags(&iter, TRUE);
			/* Sometimes, an iter has several weights. Use only the first in this case */
			gboolean weight_set_for_this_iter;
			weight_set_for_this_iter = FALSE;
			for (tag_walk = tags; tag_walk != NULL; tag_walk = tag_walk->next) {
				tag = GTK_TEXT_TAG(tag_walk->data);
				g_object_get(G_OBJECT(tag),
					     "background-set", &bg_set,
					     "foreground-set", &fg_set,
					     "underline-set", &under_set,
					     "strikethrough-set", &strike_set,
					     "weight-set", &weight_set,
					     NULL);
				if (fg_set) {
					g_object_get(G_OBJECT(tag), "foreground-gdk", &color, NULL);
					attr = pango_attr_foreground_new(color->red,color->green,color->blue);
					attr->start_index = printing_text_iter_get_offset_bytes(print_data, &iter);
					open_attrs = g_slist_prepend(open_attrs, attr);
				}
				if (bg_set) {
					g_object_get(G_OBJECT(tag), "background-gdk", &color, NULL);
					attr = pango_attr_background_new(color->red,color->green,color->blue);
					attr->start_index = printing_text_iter_get_offset_bytes(print_data, &iter);
					open_attrs = g_slist_prepend(open_attrs, attr);
				}
				if (under_set) {
					g_object_get(G_OBJECT(tag), "underline", &underline, NULL);
					attr = pango_attr_underline_new(underline);
					attr->start_index = printing_text_iter_get_offset_bytes(print_data, &iter);
					open_attrs = g_slist_prepend(open_attrs, attr);	  
				}
				if (strike_set) {
					g_object_get(G_OBJECT(tag), "strikethrough", &strikethrough, NULL);
					attr = pango_attr_strikethrough_new(strikethrough);
					attr->start_index = printing_text_iter_get_offset_bytes(print_data, &iter);
					open_attrs = g_slist_prepend(open_attrs, attr);	  
				}
				if (weight_set && !weight_set_for_this_iter) {
					weight_set_for_this_iter = TRUE;
					g_object_get(G_OBJECT(tag), "weight", &weight, NULL);
					attr = pango_attr_weight_new(weight);
					attr->start_index = printing_text_iter_get_offset_bytes(print_data, &iter);
					open_attrs = g_slist_prepend(open_attrs, attr); 
					/* Hack to see if the first char is bold -- indicates header */
					if (attr->start_index == 0 && weight == PANGO_WEIGHT_BOLD) {
						*has_headers = TRUE;
					}
				}
			}
			g_slist_free(tags);
		}
    
	} while(!gtk_text_iter_is_end(&iter) && gtk_text_iter_forward_to_tag_toggle(&iter, NULL));
	  
	/* close all open attributes */
	for (attr_walk = open_attrs; attr_walk != NULL; attr_walk = attr_walk->next) {
		attr = (PangoAttribute*) attr_walk->data;
		attr->end_index = printing_text_iter_get_offset_bytes(print_data, &iter);
		pango_attr_list_insert(attr_list, attr);
	}
	g_slist_free(open_attrs);

	pango_layout_set_attributes(print_data->layout, attr_list);
	pango_attr_list_unref(attr_list);
}

static gboolean printing_is_pango_gdk_color_equal(PangoColor *p, GdkColor *g)
{
	return ((p->red == g->red) && (p->green == g->green) && (p->blue == g->blue));
}

/* Pango has it's attribute in bytes, but GtkTextIter gets only an offset
 * in characters, so here we're returning an offset in bytes. 
 */
static gint printing_text_iter_get_offset_bytes(PrintData *print_data, const GtkTextIter *iter)
{
	gint off_bytes;
	gchar *text;
	GtkTextIter start;

	if (print_data->sel_start < 0 || print_data->sel_end <= print_data->sel_start) {
		gtk_text_buffer_get_start_iter(gtk_text_iter_get_buffer(iter), &start);
	} else {
		gtk_text_buffer_get_iter_at_offset(gtk_text_iter_get_buffer(iter), &start, print_data->sel_start);
	}
	text = gtk_text_iter_get_text(&start, iter);
	off_bytes = strlen(text);
	g_free(text);
	return off_bytes;
}

static void printing_preview_update_zoom_sensitivity(PreviewData *preview_data)
{
	if((preview_data->print_data->zoom * PREVIEW_ZOOM_FAC) > PREVIEW_ZOOM_MAX)
  		gtk_widget_set_sensitive(preview_data->zoom_in, FALSE);
	else
		gtk_widget_set_sensitive(preview_data->zoom_in, TRUE);

	if ((preview_data->print_data->zoom / PREVIEW_ZOOM_FAC) < PREVIEW_ZOOM_MIN)
		gtk_widget_set_sensitive(preview_data->zoom_out, FALSE);
	else
		gtk_widget_set_sensitive(preview_data->zoom_out, TRUE);
}
