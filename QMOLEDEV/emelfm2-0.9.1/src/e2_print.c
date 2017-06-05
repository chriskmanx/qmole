/* $Id: e2_print.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2010-2013 tooar <tooar@emelfm2.net>

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

#include "emelfm2.h"

#ifdef USE_GTK2_10
#include "e2_print.h"

//#define SIMPLEPRINT
#ifdef SIMPLEPRINT

/**
@brief callback for "begin-print" signal on print-operation for selected or all text

@param op the print operation
@param context print context for the operation
@param rt runtime data struct for the dialog

@return
*/
static void _e2_view_dialog_begin_print_cb (GtkPrintOperation *op,
		GtkPrintContext *context, E2_ViewDialogRuntime *rt)
{
	GtkTextIter start, end;
	NEEDCLOSEBGL
	if (gtk_text_buffer_get_has_selection (rt->textbuffer))
		gtk_text_buffer_get_selection_bounds (rt->textbuffer, &start, &end);
	else
		gtk_text_buffer_get_bounds (rt->textbuffer, &start, &end);

	gchar *seltext = gtk_text_buffer_get_text (rt->textbuffer, &start, &end, FALSE);
	NEEDOPENBGL
	g_strstrip (seltext);
	if (*seltext != '\0')
	{
		PangoLayout *layout = gtk_print_context_create_pango_layout (context);
		//remember it for other uses
		g_object_set_data_full (G_OBJECT (context), "e2-print-layout", layout,
			(GDestroyNotify) g_object_unref);	//cleanup when context is destroyed
#ifdef USE_GTK3_0
		GtkStyleContext *sc = gtk_widget_get_style_context (rt->textview);
		PangoFontDescription *font_desc = gtk_style_context_get_font (sc,
			GTK_STATE_NORMAL);
#else
		PangoFontDescription *font_desc = gtk_widget_get_style (rt->textview)->font_desc;
#endif
		pango_layout_set_font_description (layout, font_desc);
		gdouble page_width = gtk_print_context_get_width (context); //in pixels
		//don't assume printer device-unit is one point
		gdouble dpi = gtk_print_context_get_dpi_x (context);
		page_width = page_width * 72 / dpi; //now in points
		pango_layout_set_width (layout, (gint) page_width * PANGO_SCALE);
		pango_layout_set_text (layout, seltext, -1);

		gdouble page_height = gtk_print_context_get_height (context); //in pixels
		if (!pango_font_description_get_size_is_absolute (font_desc))
		{
			dpi = gtk_print_context_get_dpi_y (context);
			page_height = page_height * 72 / dpi; //now in points
		}
		gint text_height = pango_font_description_get_size (font_desc) / PANGO_SCALE; //in points or pixels
		gint lines_per_page = page_height / text_height;
		gint line_count = pango_layout_get_line_count (layout);
		gint page_count = (line_count - 1) / lines_per_page + 1;	//round upwards
		gtk_print_operation_set_n_pages (op, page_count);

		//setup page header with filename
		PangoLayout *layout_name = gtk_print_context_create_pango_layout (context);
		g_object_set_data_full (G_OBJECT (context), "e2-print-name", layout_name,
			(GDestroyNotify) g_object_unref);	//cleanup when context is destroyed
		pango_layout_set_font_description (layout_name, font_desc);
		pango_layout_set_width (layout_name, (gint) page_width * PANGO_SCALE);
		gchar *base = g_path_get_basename (rt->localpath);
		gchar *page_title = F_FILENAME_FROM_LOCALE (base);
		pango_layout_set_text (layout_name, page_title, -1);
		g_free (base);
		F_FREE (page_title, base);
		//and for page counter, if > 1 page
		if (page_count > 1)
		{
			PangoLayout *layout_num = gtk_print_context_create_pango_layout (context);
			pango_layout_set_font_description (layout_num, font_desc);
//			pango_layout_set_alignment (layout_right, PANGO_ALIGN_RIGHT);
			g_object_set_data_full (G_OBJECT (context), "e2-print-page", layout_num,
				(GDestroyNotify) g_object_unref);	//cleanup when context is destroyed
		}
	}
	g_free (seltext);
}
/* *
@brief callback for "end-print" signal on print-operation for selected or all text

@param op UNUSED the print operation
@param context print context for the operation
@param data UNUSED data specified when callback was connected

@return
*/
/*static void _e2_view_dialog_end_print_cb (GtkPrintOperation *op,
	GtkPrintContext *context, gpointer data)
{
	NEEDCLOSEBGL
	NEEDOPENBGL
} */
/**
@brief callback for "draw-page" signal on print-operation for selected or all text

@param op UNUSED the print operation
@param context print context for the operation
@param page_num 0-based index of printed pages count
@param rt runtime data struct for the dialog

@return
*/
static void _e2_view_dialog_draw_page_cb (GtkPrintOperation *op,
	GtkPrintContext *context, gint page_num, E2_ViewDialogRuntime *rt)
{
//	NEEDCLOSEBGL
	cairo_t *cr = gtk_print_context_get_cairo_context (context);
	if (cr != NULL)
	{
//		PangoLayoutLine *line, *layout_lh, *layout_rh;
		gdouble dpi;
		//print header, comprising filename, and page-number if > 1 page
#ifdef USE_GTK3_0
		GtkStyleContext *sc = gtk_widget_get_style_context (rt->textview);
		PangoFontDescription *font_desc = gtk_style_context_get_font (sc,
			GTK_STATE_NORMAL);
#else
		PangoFontDescription *font_desc = gtk_widget_get_style (rt->textview)->font_desc;
#endif
		//CHECKME dpi_v instead of 72 ?
		cairo_move_to (cr, 0.0, - 10 * 72 / 25.4);	//left margin, up 10 mm from top of text
		PangoLayout *layout = g_object_get_data (G_OBJECT (context), "e2-print-name");
		pango_cairo_show_layout (cr, layout);

		gint n_pages;
		g_object_get (G_OBJECT (op), "n-pages", &n_pages, NULL);
		if (n_pages > 1)
		{
			PangoLayout *layout_right = g_object_get_data (G_OBJECT (context),
				"e2-print-page");
			gchar *page_text = g_strdup_printf ("%d / %d", page_num + 1, n_pages);
			pango_layout_set_text (layout_right, page_text, -1);
			g_free (page_text);
			gdouble page_width = gtk_print_context_get_width (context); //in pixels
			dpi = gtk_print_context_get_dpi_x (context);
			page_width = page_width * 72 / dpi; //in points
			gint layout_width;
			pango_layout_get_size (layout_right, &layout_width, NULL);
			//CHECKME dpi_v instead of 72 ?
			cairo_move_to (cr,
				page_width - layout_width / PANGO_SCALE, - 10 * 72 / 25.4); //up 10 mm from top of text
			pango_cairo_show_layout (cr, layout_right);
		}

		layout = g_object_get_data (G_OBJECT (context), "e2-print-layout");
		gdouble page_height = gtk_print_context_get_height (context);	//in pixels
		if (!pango_font_description_get_size_is_absolute (font_desc))
		{
			dpi = gtk_print_context_get_dpi_y (context);
			page_height = page_height / dpi * 72; //now in points
		}
		gint text_height = pango_font_description_get_size (font_desc) / PANGO_SCALE; //in pixels or points
		guint lines_per_page = page_height / text_height;
		gint line_count = pango_layout_get_line_count (layout);
		guint last_line = (line_count > lines_per_page * (page_num + 1)) ?
			lines_per_page * (page_num + 1) : line_count;
		guint pageline, fileline;
		for (pageline = 1, fileline = lines_per_page * page_num;	//CHECKME pageline=0 (move to tops of lines?)
				fileline < last_line; pageline++, fileline++)
		{
			PangoLayoutLine *linelay = pango_layout_get_line (layout, fileline);
			cairo_move_to (cr, 0, pageline * text_height);
			pango_cairo_show_layout_line (cr, linelay);
		}
	}
//	NEEDOPENBGL
}
/**
@brief callback for "status-changed" signal on print-operation for selected or all text

@param op the print operation
@param data UNUSED data specified when callback was connected

@return
*/
static void _e2_view_dialog_print_status_cb (GtkPrintOperation *op,
	gpointer data)
{
	NEEDCLOSEBGL
	GtkPrintStatus pstat = gtk_print_operation_get_status (op);
	NEEDOPENBGL
	switch (pstat)
	{
		case GTK_PRINT_STATUS_PENDING_ISSUE:
		case GTK_PRINT_STATUS_FINISHED:	//also for end of preview
		case GTK_PRINT_STATUS_FINISHED_ABORTED: //BAD for sync, maybe OK for async
			g_object_unref (G_OBJECT (op));	//cleanup, ref => 2
		default:
			break;
	}
}
/**
@brief asynchronously print currently-selected text or all text if no selection

@param menuitem UNUSED the activated widget in a dialog context-menu, or NULL
@param rt runtime struct to work on

@return
*/
void e2_dialog_print_cb (GtkMenuItem *menuitem, E2_ViewDialogRuntime *rt)
{
/*#ifdef USE_GTK2_18
	static GtkPrintSettings *settings = NULL;
#endif */
	static GtkPageSetup *page_setup = NULL;

	NEEDCLOSEBGL
	GtkPrintOperation *op = gtk_print_operation_new ();
/*#ifdef USE_GTK2_18
	if (settings == NULL)
		settings = gtk_print_settings_new ();
	gtk_print_operation_set_print_settings (op, settings);
#endif */
	if (page_setup == NULL)
	{
		page_setup = gtk_page_setup_new ();	//never cleaned
		gtk_page_setup_set_top_margin (page_setup, 25.0, GTK_UNIT_MM);
		gtk_page_setup_set_bottom_margin (page_setup, 20.0, GTK_UNIT_MM);
		gtk_page_setup_set_left_margin (page_setup, 20.0, GTK_UNIT_MM);
		gtk_page_setup_set_right_margin (page_setup, 20.0, GTK_UNIT_MM);
	}

	gtk_print_operation_set_default_page_setup (op, page_setup);
//	gtk_print_operation_set_current_page (op, 0); //enable current-page selection
#ifdef USE_GTK2_18
	gtk_print_operation_set_support_selection (op, TRUE);
	gtk_print_operation_set_has_selection (op, TRUE); //widget always enabled
	if (gtk_text_buffer_get_has_selection (rt->textbuffer))
	{
		GtkPrintSettings *settings = gtk_print_operation_get_print_settings (op);
		gtk_print_settings_set_print_pages (settings, GTK_PRINT_PAGES_SELECTION);
	}
	gtk_print_operation_set_embed_page_setup (op, TRUE);
#endif
//	gtk_print_operation_set_unit (op, GtkUnit unit); default is GTK_UNIT_PIXEL
	gtk_print_operation_set_allow_async (op, TRUE);

	g_signal_connect (G_OBJECT (op), "begin-print",
		G_CALLBACK (_e2_view_dialog_begin_print_cb), rt);
//	g_signal_connect (G_OBJECT (op), "end-print",
//		G_CALLBACK(_e2_view_dialog_end_print_cb), NULL);
	g_signal_connect (G_OBJECT (op), "draw-page",
		G_CALLBACK (_e2_view_dialog_draw_page_cb), rt);
	g_signal_connect (op, "status-changed",
		G_CALLBACK (_e2_view_dialog_print_status_cb), NULL);
	GError *error = NULL;
	GtkPrintOperationResult result = gtk_print_operation_run (op,
		GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG, GTK_WINDOW (rt->dialog), &error);

	switch (result)
	{
//		GObject *debug2;
		case GTK_PRINT_OPERATION_RESULT_ERROR:
			e2_output_print_error (error->message, FALSE);
			g_error_free (error);
/*		case GTK_PRINT_OPERATION_RESULT_APPLY:	//e.g. after sync preview
		case GTK_PRINT_OPERATION_RESULT_IN_PROGRESS:
	TOO HARD TO PROPERLY MANAGE SETTINGS REFCOUNT IN ALL USAGE CASES
*/
		default:
//			if (result != GTK_PRINT_OPERATION_RESULT_APPLY)//this for after sync preview
//				debug2 = G_OBJECT (op);	//refcount 2 in async mode
			if (!(result == GTK_PRINT_OPERATION_RESULT_IN_PROGRESS
				|| result == GTK_PRINT_OPERATION_RESULT_APPLY))	//e.g. after a sync preview cancelled
				g_object_unref (G_OBJECT (op));
			break;
	}
	NEEDOPENBGL
}
#endif //def SIMPLEPRINT

#ifndef SIMPLEPRINT
/* multiple of raw line-height to use for 'double' spacing */
#define SPACING_FACTOR 1.5

#ifndef GTK_PRINT_PAGES_SELECTION
# define GTK_PRINT_PAGES_SELECTION GTK_PRINT_PAGES_RANGES + 1
#endif

typedef struct _PrintData
{
	/* printed-file details */
	GtkWidget *textview;
	GtkTextBuffer *buf;
	const gchar *localpath;

	guint16 curr_page;	/* for header - no. of page being/to be printed, 1-based */
	guint16 n_pages;	/* for header - no. of pages in the print-job,
							maybe != pages served by GtkPrintOperation  */
	guint16 page_lines;	/* no. of printable lines between top and bottom margins */
	guint16 page_last_lines; /* 'forecast' no. of lines to be printed on last page of job */
	gfloat page_height;	/* points-height of each page of printed text (between top and bottom margins) */
	gfloat page_width;	/* ditto for width */
	gfloat line_height;	/* points-height of each un-wrapped line of printed text,
							includes padding if doublespace is TRUE. Wrapped lines
							have only 1 padding per layout */
	GtkPrintPages printpages; /* indicator of which part(s) of document are to be printed */
	GtkPageSet pageset; /* another indicator of which part(s) of document are to be printed */
	GArray *range_pages;/* array of guint16 page-numbers for each nomimated page */
	guint16 rangeindx;	/* 0-based index of page being processed, in page_ranges */
	guint16 rangecount;	/* no. of pages in page_ranges */
	guint startline;	/* 1-based count of 1st line of the file to be printed */
	guint endline;		/* 1-based count of last line to be printed */
	GtkTextMark *carry_start; /* mark at start of wrapped text carried from a
								page to the next one */
	/* cached layouts */
	PangoLayout *lo_name; /* for file-path, in page-header if any */
	PangoLayout *lo_pagenum; /* for page-number, in header if any */
	PangoLayout *lo_line; /* for lines of text */
	/* widgets used in (and mainly owned by) preferences-tab in print-dialog */
	GtkWidget *invertbtn;
	GtkWidget *headerbtn;
	GtkWidget *wrapbtn;
	GtkWidget *doublebtn;
	GtkWidget *tabspin;
} PrintData;

/* session-parameters */
static gboolean printheader = TRUE;	/* put header on each page */
static gboolean printinvert = FALSE;/* invert colours */
static gboolean wraplines = TRUE;	//CHECKME reconcile with option "dialog-view-wrap"
static gboolean doublespace = FALSE;/* TRUE to expand the space between lines */
static gint tabsize = 8;
static GtkPrintSettings *print_settings = NULL;
static GtkPageSetup *page_setup = NULL;

/**
@brief get an allocated, 0-filled, data struct for a print job

@return pointer to the struct, or NULL
*/
static inline PrintData *_e2_print_data_new (void)
{
	return ((PrintData *) g_slice_new0 (PrintData));
}
/**
@brief cleanup allocated contents of @a pd, and the struct itself

@param pd data struct for the print job

@return
*/
static void _e2_print_data_free (PrintData *pd)
{
	if (pd->range_pages) g_array_free (pd->range_pages, TRUE);
	if (pd->lo_name) g_object_unref (G_OBJECT (pd->lo_name));
	if (pd->lo_pagenum) g_object_unref (G_OBJECT (pd->lo_pagenum));
	if (pd->lo_line) g_object_unref (G_OBJECT (pd->lo_line));
	g_slice_free1 (sizeof (PrintData), pd);
}
/**
@brief invert @a color
@param color pointer to colour-data to be inverted
@return
*/
#ifdef USE_GTK3_0
static void _e2_print_color_invert (GdkRGBA *color)
{
	gdouble red, green, blue, added;

	red = color->red;
	green = color->green;
	blue = color->blue;
	added = (red + green + blue);
	if (added < 0.00005)
	{
		red = 1.0;
		green = 1.0;
		blue = 1.0;
	}
	else
	{
		gdouble opposed = 1.0 - (added / 3);
		red *= opposed;
		green *= opposed;
		blue *= opposed;
	}
	color->red = red;
	color->green = green;
	color->blue = blue;
}
#else
static void _e2_print_color_invert (GdkColor *color)
{
	guint16 red, green, blue, added;

	red = color->red;
	green = color->green;
	blue = color->blue;
	added = (red + green + blue) / 3;
	if (added == 0)
	{
		red = 0xffff;
		green = 0xffff;
		blue = 0xffff;
	}
	else
	{
		guint16 opposed = 0xffff - added;
		red = red * opposed / added;
		green = green * opposed / added;
		blue = blue * opposed / added;
	}
	color->red = red;
	color->green = green;
	color->blue = blue;
}
#endif
/**
@brief helper for _e2_print_begin_cb()
Sets pd->startline, pd->endline, pd->page_last_lines, according to pd->blocktype

@param pd data struct for the print job

@return the no. of lines to be printed for the job
*/
static gint _e2_print_set_lines (PrintData *pd)
{
	switch (pd->printpages)
	{
		case GTK_PRINT_PAGES_ALL:
//			pd->rangeindx = 0;
//			pd->rangecount = 1;
			pd->startline = 1;
			pd->endline = gtk_text_buffer_get_line_count (pd->buf);
			pd->page_last_lines = pd->endline % pd->page_lines;
			return pd->endline;

		case GTK_PRINT_PAGES_RANGES:
//UPSTREAM	pd->rangeindx = 0;
//UPSTREAM	pd->rangecount = ?;
			pd->startline = 1;
			pd->endline = gtk_text_buffer_get_line_count (pd->buf);
			pd->page_last_lines = pd->page_lines; /* whole page(s), last page is full */
			return pd->endline;

		case GTK_PRINT_PAGES_CURRENT:
		{
//			pd->rangeindx = 0;
//			pd->rangecount = 1;
			GtkTextMark *current = gtk_text_buffer_get_insert (pd->buf);
			GtkTextIter iter;
			gtk_text_buffer_get_iter_at_mark (pd->buf, &iter, current);
			gint line = gtk_text_iter_get_line (&iter);
			gint last = gtk_text_buffer_get_line_count (pd->buf);

			line = (line - 1) / pd->page_lines * pd->page_lines;
			if (line + pd->page_lines <= last)
			{
				pd->startline = line + 1;
				pd->endline = line + pd->page_lines;
			}
			else
			{
				pd->startline = last - pd->page_lines + 1;
				if (pd->startline < 1)
					pd->startline = 1;
				pd->endline = last;
			}
			pd->page_last_lines = pd->endline - pd->startline + 1;
			return pd->page_last_lines;
		}

		case GTK_PRINT_PAGES_SELECTION:
		{
			GtkTextIter start, end;
			if (gtk_text_buffer_get_selection_bounds (pd->buf, &start, &end))
			{
				pd->startline = gtk_text_iter_get_line (&start);
				pd->endline = gtk_text_iter_get_line (&end);
				gint n_lines = pd->endline - pd->startline + 1;
				pd->page_last_lines = n_lines % pd->page_lines;
				return n_lines;
			}
			else
				return 0;
		}

		default:
			return 0;	/* error signal */
	}
}

static gint space_width;
/**
@brief set 'raw' pd->line_height i.e. no allowance for double-spacing

@param context print context for the operation
@param pd data struct for the print job

@return
*/
static void _e2_print_set_line_height (GtkPrintContext *context, PrintData *pd)
{
	gint space_height;

	pango_layout_set_text (pd->lo_line, " ", 1);
	pango_layout_get_size (pd->lo_line, &space_width, &space_height);  //in device units (points) * PANGO_SCALE
	printd (DEBUG, "Space width %5.3f", (gfloat)space_width / PANGO_SCALE);
	pd->line_height = (gfloat)space_height / PANGO_SCALE;
}
/**
@brief get points-width of a ' ' character
Must be called after _e2_print_set_line_height()

@return the width
*/
static inline gfloat _e2_print_get_space_width (void)
{
	return ((gfloat)space_width / PANGO_SCALE);
}
/**
@brief comparison function for sorting pages specified for a range-job

@param a pointer to a page number in array
@param b pointer to a page number in array

@return <0, 0 or >0 according to whether the page for @a a is <, = or > the page for @a b
*/
static gint _e2_print_range_pages_compare (gconstpointer a, gconstpointer b)
{
	guint16 pa, pb;

	pa = *((guint16*)a);
	pb = *((guint16*)b);
	return (pa - pb);
}
/**
@brief "create-custom-widget" signal callback on @a po

@param po the print operation data
@param pd data struct for the print job

@return
*/
static GObject *_e2_print_get_custom_tab_cb (GtkPrintOperation *po, PrintData *pd)
{
	NEEDCLOSEBGL
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING);
#endif

	GtkWidget *btn = gtk_check_button_new_with_mnemonic (_("Add page _header"));
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (btn), printheader);
	gtk_box_pack_start (GTK_BOX (vbox), btn, FALSE, FALSE, 0);
	pd->headerbtn = btn;

	btn = gtk_check_button_new_with_mnemonic (_("_Wrap text"));
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (btn), wraplines);
	gtk_box_pack_start (GTK_BOX (vbox), btn, FALSE, FALSE, 0);
	pd->wrapbtn = btn;

	btn = gtk_check_button_new_with_mnemonic (_("_Double-space lines"));
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (btn), doublespace);
	gtk_box_pack_start (GTK_BOX (vbox), btn, FALSE, FALSE, 0);
	pd->doublebtn = btn;

	btn = gtk_check_button_new_with_mnemonic (_("_Invert colours"));
	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (btn), printinvert);
	gtk_box_pack_start (GTK_BOX (vbox), btn, FALSE, FALSE, 0);
	pd->invertbtn = btn;

#ifdef USE_GTK3_0
	GtkWidget *hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING_SMALL);
#else
	GtkWidget *hbox = gtk_hbox_new (FALSE, E2_PADDING_SMALL);
#endif
	GtkWidget *lbl = gtk_label_new (_("Tab size"));
	gtk_box_pack_start (GTK_BOX (hbox), lbl, FALSE, FALSE, 0);
	GtkWidget *spin = gtk_spin_button_new_with_range (1.0, 20.0, 1.0);
	gtk_spin_button_set_increments (GTK_SPIN_BUTTON (spin), 1.0, 4.0);
	gtk_spin_button_set_numeric (GTK_SPIN_BUTTON (spin), TRUE);
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (spin), (gdouble) tabsize);
	gtk_box_pack_start (GTK_BOX (hbox), spin, FALSE, FALSE, 0);
	pd->tabspin = spin;
	lbl = gtk_label_new (_("spaces"));
	gtk_box_pack_start (GTK_BOX (hbox), lbl, FALSE, FALSE, 0);

	gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	gtk_box_pack_start (GTK_BOX (hbox), vbox, FALSE, FALSE, E2_PADDING);
	gtk_widget_show_all (hbox);

	NEEDOPENBGL
	return G_OBJECT(hbox);
}
/**
@brief "custom-widget-apply" signal callback on @a po

@param po the print operation data
@param tabchild UNUSED the page-widget for the print dialog's custom tab
@param pd data struct for the print job

@return
*/
static void _e2_print_apply_extras_cb (GtkPrintOperation *po,
	GtkWidget *tabchild, PrintData *pd)
{
//	NEEDCLOSEBGL
	printinvert = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (pd->invertbtn));
	printheader = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (pd->headerbtn));
	wraplines = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (pd->wrapbtn));
	doublespace = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (pd->doublebtn));
	tabsize = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (pd->tabspin));
//	NEEDOPENBGL
}
/**
@brief callback for "begin-print" signal on @a po
Setup printing generally, plus some specifics for the first page to be printed

@param po the print operation data
@param context print context for the job
@param pd data struct for the print job

@return
*/
static void _e2_print_begin_cb (GtkPrintOperation *po, GtkPrintContext *context,
	PrintData *pd)
{
	GtkPrintSettings *current_settings;
	guint n_lines;
	gint served_pages, i;

	if (gtk_text_buffer_get_line_count (pd->buf) == 0)
	{
		gtk_print_operation_cancel (po); /* file is empty */
		return;
	}

	NEEDCLOSEBGL
	PangoAttrList *attrs = pango_attr_list_new ();
#ifdef USE_GTK3_0
	GtkStyleContext *sc = gtk_widget_get_style_context (pd->textview);
# ifdef USE_GTK3_8
	const PangoFontDescription *font_desc;
	gtk_style_context_get (sc, GTK_STATE_FLAG_NORMAL,
		GTK_STYLE_PROPERTY_FONT, &font_desc, NULL);
# else
	const PangoFontDescription *font_desc = gtk_style_context_get_font (sc,
		GTK_STATE_NORMAL);
# endif
#else
	PangoFontDescription *font_desc = gtk_widget_get_style (pd->textview)->font_desc;
#endif
	const gchar *family = pango_font_description_get_family (font_desc);
	PangoAttribute *at = pango_attr_family_new (family);
	pango_attr_list_insert (attrs, at);
	gint count = pango_font_description_get_size (font_desc);
	if (!pango_font_description_get_size_is_absolute (font_desc))
	{
		gdouble dpi = gtk_print_context_get_dpi_y (context);
		count = count * 72 / dpi; //now in points
	}
//	printd (DEBUG, "%s %d font used for printing", family, count / PANGO_SCALE);
	at = pango_attr_size_new (count);
	pango_attr_list_insert (attrs, at);
	if (printinvert)
	{
#ifdef USE_GTK3_0
		GdkRGBA *fore, *back;
		gtk_style_context_get (sc, GTK_STATE_NORMAL,
			GTK_STYLE_PROPERTY_COLOR, &fore,
			GTK_STYLE_PROPERTY_BACKGROUND_COLOR, &back,
			NULL);
		_e2_print_color_invert (fore);
		at = pango_attr_foreground_new ((guint16)(65535 * fore->red),
			(guint16)(65535 * fore->green), (guint16)(65535 * fore->blue));
		pango_attr_list_insert (attrs, at);
		gdk_rgba_free (fore);
		_e2_print_color_invert (back);
		at = pango_attr_background_new ((guint16)(65535 * back->red),
			(guint16)(65535 * back->green), (guint16)(65535 * back->blue));
		pango_attr_list_insert (attrs, at);
		gdk_rgba_free (back);
#else
		GtkStyle *style = gtk_rc_get_style (pd->textview);
		GdkColor c = style->text[GTK_STATE_NORMAL];
		_e2_print_color_invert (&c);
		at = pango_attr_foreground_new (c.red, c.green, c.blue);
		pango_attr_list_insert (attrs, at);

		c = style->bg[GTK_STATE_NORMAL];
		_e2_print_color_invert (&c);
		at = pango_attr_background_new (c.red, c.green, c.blue);
		pango_attr_list_insert (attrs, at);
#endif
	}

	/* layout for each line of printed text */
	pd->lo_line = gtk_print_context_create_pango_layout (context);
	pango_layout_set_attributes (pd->lo_line, attrs);
	pango_attr_list_unref (attrs);
//	pango_layout_set_spacing (pd->lo_line, 0); probably default (pango code not explicit)
//	pango_layout_set_alignment (pd->lo_line, PANGO_ALIGN_LEFT); default
	pango_layout_set_auto_dir (pd->lo_line, FALSE);
	if (wraplines)
	{
		pango_layout_set_wrap (pd->lo_line, PANGO_WRAP_WORD_CHAR); //default is WORD
//		pango_layout_set_ellipsize (pd->lo_line, (pd->wraplines), PANGO_ELLIPSIZE_NONE); //default
	}
	else
	{
		pango_layout_set_wrap (pd->lo_line, PANGO_WRAP_CHAR);
		pango_layout_set_ellipsize (pd->lo_line, PANGO_ELLIPSIZE_END);
	}

	_e2_print_set_line_height (context, pd);

	gint tabcount = 80/tabsize;
	PangoTabArray *tabs = pango_tab_array_new (tabcount, TRUE); //positions in pixels
	/* populate tab positions for tabsize spaces per tab */
	gfloat tab_points = _e2_print_get_space_width ();
	for (i = 1; i <= tabcount; i++)
	{
		gint location;
		location = tabsize * tab_points * i; /* NOTE no PANGO_SCALE! */
		pango_tab_array_set_tab (tabs, i, PANGO_TAB_LEFT, location);
	}
	pango_layout_set_tabs (pd->lo_line, tabs);
	pango_tab_array_free (tabs);

	pd->page_height = gtk_print_context_get_height (context); //in pixels (or device-units = points?)
	pd->page_width = gtk_print_context_get_width (context); //in pixels (or device-units = points?)

	if (doublespace)
		pd->page_lines = pd->page_height / pd->line_height / SPACING_FACTOR;
	else
		pd->page_lines = pd->page_height / pd->line_height; //assuming no wrapping
	pd->page_lines++; //TODO explain this!!

	current_settings = gtk_print_operation_get_print_settings (po);
	pd->printpages = gtk_print_settings_get_print_pages (current_settings);
	if (pd->printpages == GTK_PRINT_PAGES_CURRENT)
		pd->pageset = GTK_PAGE_SET_ALL;
	else
#ifdef USE_GTK2_18
		pd->pageset = gtk_print_settings_get_page_set (current_settings);
#else
	{
		if (gtk_text_buffer_get_has_selection (pd->buf))
			pd->pageset = GTK_PRINT_PAGES_SELECTION;
		else
			pd->pageset = gtk_print_settings_get_page_set (current_settings);
	}
#endif

	switch (pd->printpages)
	{
//	case GTK_PRINT_PAGES_ALL:
	default:
		n_lines = _e2_print_set_lines (pd);
		pd->n_pages = n_lines / pd->page_lines; /* if wrapping happens, may be increased */
		if (n_lines % pd->page_lines > 0)
			pd->n_pages++;
		break;
	case GTK_PRINT_PAGES_RANGES:
	{
		gint n_ranges;
		GtkPageRange *ranges;
		ranges = gtk_print_settings_get_page_ranges (current_settings, &n_ranges);
		if (n_ranges > 0)
		{
			GtkPageRange *this;
			gint i;
			guint16 pn;

			pd->range_pages = g_array_sized_new (FALSE, FALSE, sizeof (gint16),
								n_ranges * 2);
			for (i = 0, this = ranges; i < n_ranges; i++, this++)
			{
				gint j;
				/* cache the specified pages NOTE: supplied page-numbers are
					0-based, not 1-base counts that we use */
				for (j = this->start; j <= this->end; j++)
				{
					if (pd->pageset == GTK_PAGE_SET_ALL
					|| (pd->pageset == GTK_PAGE_SET_ODD && (j&1) == 0)
					|| (pd->pageset == GTK_PAGE_SET_EVEN && (j&1) > 0))
					{
						pn = j+1;
						g_array_append_vals (pd->range_pages, &pn, 1);
					}
				}
			}
			g_free (ranges);
			if (pd->range_pages->len > 0)
			{
				/* in case the chosen pages are not in ascending order ... */
				g_array_sort (pd->range_pages, _e2_print_range_pages_compare);
				/* chosen-page(s) that are too big are handled when printing page(s)
				   cuz the no. of pages can grow during printing, due to wrapping */
				n_lines = _e2_print_set_lines (pd);
				pd->n_pages = n_lines / pd->page_lines; /* if wrapping happens, may be increased */
				if (n_lines % pd->page_lines > 0)
					pd->n_pages++;
				pd->rangeindx = 0;
				pd->rangecount = pd->range_pages->len;
				break;
			}
			else
			{
				gtk_print_operation_cancel (po);
				NEEDOPENBGL
				return;
			}
		}
		else /* some problem with the page-range(s) entered */
		{
			gtk_print_operation_cancel (po);
			NEEDOPENBGL
			return;
		}
	}
	case GTK_PRINT_PAGES_CURRENT:
		_e2_print_set_lines (pd); /* just setup parameters */
		pd->n_pages = 1;
		break;
	case GTK_PRINT_PAGES_SELECTION:
		if (gtk_text_buffer_get_has_selection (pd->buf))
		{
			n_lines = _e2_print_set_lines (pd);
			pd->n_pages = n_lines / pd->page_lines;
			if (n_lines % pd->page_lines > 0)
				pd->n_pages++;
			break;
		}
		else
		{
			gtk_print_operation_cancel (po);
			NEEDOPENBGL
			return;
		}
	}

	if (pd->printpages == GTK_PRINT_PAGES_CURRENT)
		served_pages = 1;
	else if (pd->printpages == GTK_PRINT_PAGES_RANGES)
		served_pages = pd->rangecount;
	else
	{
		switch (pd->pageset)
		{
		case GTK_PAGE_SET_ODD:
			served_pages = pd->n_pages / 2;
			if ((pd->n_pages & 0x1) != 0)
				served_pages++;
			break;
		case GTK_PAGE_SET_EVEN:
			served_pages = pd->n_pages / 2;
			break;
		default:
			served_pages = pd->n_pages;
			break;
		}
	}
	gtk_print_operation_set_n_pages (po, served_pages);

	pd->curr_page = 1; /* default start-page for the print job */
	if (printheader)
	{
		gchar *file_path;

		pd->lo_name = gtk_print_context_create_pango_layout (context);
		pango_layout_set_attributes (pd->lo_name, attrs); //no further unref
		pango_layout_set_auto_dir (pd->lo_name, FALSE);

		file_path = F_FILENAME_FROM_LOCALE (pd->localpath);
		pango_layout_set_text (pd->lo_name, file_path, -1);
		F_FREE (file_path, pd->localpath);
		if (pd->n_pages > 1)
		{
			pd->lo_pagenum = gtk_print_context_create_pango_layout (context);
			pango_layout_set_attributes (pd->lo_pagenum, attrs); //no further unref
			pango_layout_set_auto_dir (pd->lo_pagenum, FALSE);
			pango_layout_set_alignment (pd->lo_pagenum, PANGO_ALIGN_RIGHT);
		}
		else
			pd->lo_pagenum = NULL;
	}
	else
	{
		pd->lo_name = NULL;
		pd->lo_pagenum = NULL;
	}
	NEEDOPENBGL
}
/**
@brief callback for "draw-page" signal on @a po
Renders a page onto the cairo context obtained from @a context. Expects several
page-parameters to have been set, in _e2_print_begin_cb() or at the end of a
previous call to this function

@param po the print operation data
@param context print context for the operation
@param page_num 0-based index of pages 'served' by gtk
@param pd data struct for the print job

@return
*/
static void _e2_print_draw_page_cb (GtkPrintOperation *po,
	GtkPrintContext *context, gint page_num, PrintData *pd)
{
	cairo_t *cr;
	PangoLayout *layout;
	gboolean show;
	gdouble line_top; /* offset (points) of top of next line's layout */
	guint printed_wraps;
	guint16 fullpages;
	gint bufferlineindex;

	NEEDCLOSEBGL
	cr = gtk_print_context_get_cairo_context (context);
next_page:
	if (!GTK_IS_TEXT_BUFFER (pd->buf)) //TODO (if relevant) check its selection unchanged
		gtk_print_operation_cancel (po);
	/* When printing odd/even/ranged page(s), we render all of the document,
	   but don't show unwanted pages. Otherwise, it's too complicated to maintain
	   consistency amont these different page-sets when line-wrapping is enabled
	*/
	if (pd->printpages == GTK_PRINT_PAGES_RANGES)
		show = (pd->curr_page == g_array_index (pd->range_pages, guint16, pd->rangeindx));
	else if (pd->pageset == GTK_PAGE_SET_ODD)
		show = ((pd->curr_page & 1) > 0);
	else if (pd->pageset == GTK_PAGE_SET_EVEN)
		show = ((pd->curr_page & 1) == 0);
	else
		show = TRUE;

	if (show && printheader)
	{
		/* print header */
		cairo_move_to (cr, 0.0, - 10 * 72 / 25.4);	/* left margin, up 10 mm from top of text */
		pango_cairo_show_layout (cr, pd->lo_name);

		if (pd->n_pages > 1)
		{
			PangoLayout *layout2 = pd->lo_pagenum;
			if (layout2 != NULL)
			{
				gint layout_width;
				gchar *page_text = g_strdup_printf ("%d / %d", pd->curr_page, pd->n_pages);
				pango_layout_set_text (layout2, page_text, -1);
				g_free (page_text);
				pango_layout_get_size (layout2, &layout_width, NULL); /* in device-units (points) * PANGO_SCALE */
				cairo_move_to (cr,
					pd->page_width - layout_width / PANGO_SCALE, - 10 * 72 / 25.4); /* up 10 mm from top of text */
				pango_cairo_show_layout (cr, layout2);
			}
		}
	}

	printed_wraps = 0;
	line_top = 0.0;
	layout = pd->lo_line;
	GtkTextIter start, end;

	if (pd->carry_start != NULL)
	{
		/* process previous-page linewrap carry-over */
		gtk_text_buffer_get_iter_at_mark (pd->buf, &start, pd->carry_start);
		end = start;
		gtk_text_iter_forward_to_line_end (&end);
		gtk_text_buffer_delete_mark (pd->buf, pd->carry_start);
		pd->carry_start = NULL;
		bufferlineindex = pd->startline - 1; /* offset end-of-loop increment */
		goto carry_on;
	}

	bufferlineindex = pd->startline;
	do
	{
		/* get line-text to be printed */
		if (pd->printpages == GTK_PRINT_PAGES_SELECTION)
		{
			if (bufferlineindex == pd->startline)
			{
				gtk_text_buffer_get_selection_bounds (pd->buf, &start, NULL);
				end = start;
				gtk_text_iter_forward_to_line_end (&end);
			}
			else if (bufferlineindex == pd->endline)
			{
				gtk_text_buffer_get_iter_at_line (pd->buf, &start, bufferlineindex-1);
				gtk_text_buffer_get_selection_bounds (pd->buf, NULL, &end);
			}
			else
			{
				gtk_text_buffer_get_iter_at_line (pd->buf, &start, bufferlineindex-1);
				end = start;
				gtk_text_iter_forward_to_line_end (&end);
			}
		}
		else
		{
			gtk_text_buffer_get_iter_at_line (pd->buf, &start, bufferlineindex-1);
			end = start;
			gtk_text_iter_forward_to_line_end (&end);
		}
carry_on:
		if (gtk_text_iter_compare (&start, &end) < 0)
		{
			gchar *linetext = gtk_text_buffer_get_text (pd->buf, &start, &end, FALSE);
			pango_layout_set_text (layout, linetext, -1);

			/* must reset width after text applied */
			pango_layout_set_width (layout, pd->page_width * PANGO_SCALE);

			if (show)
				cairo_move_to (cr, 0.0, line_top); /* the top-position from last time */

			if (wraplines && pango_layout_is_wrapped (layout))
			{
				gint layout_lines, j;

				layout_lines = pango_layout_get_line_count (layout);
				line_top += pd->line_height; /* we can always print at least 1 line */
				for (j = 1; j < layout_lines; j++)
				{
					if (line_top <= pd->page_height) /* page isn't full yet */
					{
						printed_wraps++;
						line_top += pd->line_height;
					}
					else /* page is full before all of layout is printed */
					{
						PangoLayoutLine *line = pango_layout_get_line_readonly (layout, j);
						gint tail_index = line->start_index;
						pango_layout_set_text (layout, linetext, tail_index);
						pango_layout_set_width (layout, pd->page_width * PANGO_SCALE);
						if (show)
							pango_cairo_show_layout (cr, layout);
						/* remember what to process at top of next page */
						gtk_text_iter_set_line_index (&start, tail_index);
						pd->carry_start = gtk_text_buffer_create_mark (pd->buf,
							NULL, &start, TRUE);
						/* at least one extra line to print (any more will be counted as wraps on next page) */
						pd->page_last_lines++;
//						printd (DEBUG, "Page-carry, add 1 line to last-page count");
						break;
					}
				}
				if (show)
					pango_cairo_show_layout (cr, layout);

				g_free (linetext);
				bufferlineindex++;

				if (doublespace)
					line_top += pd->line_height * (SPACING_FACTOR  - 1);
				if (line_top > pd->page_height) /* not enough room for another line */
					break;
			}
			else /* no wrapping */
			{
				if (show)
					pango_cairo_show_layout (cr, layout);
				g_free (linetext);
				goto checkspace2;
			}
		}
		else /* empty line */
		{
checkspace2:
			bufferlineindex++;
			if (doublespace)
				line_top += pd->line_height * SPACING_FACTOR;
			else
				line_top += pd->line_height;
			if (line_top >pd->page_height) //page full
				break;
		}
	} while (bufferlineindex < pd->endline); /* end of lines-per-page loop */

	/* line(s) might have been added to this page due to (unflagged) carry from
		previous page and/or wrap(s) on this page */
	pd->page_last_lines += printed_wraps;
	fullpages = pd->page_last_lines / pd->page_lines;
	if (fullpages > 0)
	{
		gint served_pages;
		pd->n_pages += fullpages;
		printd (DEBUG, "Bump pages count to %d", pd->n_pages);
		pd->page_last_lines %= pd->page_lines;

		g_object_get (G_OBJECT (po), "n-pages", &served_pages, NULL);
		switch (pd->pageset)
		{
		case GTK_PAGE_SET_ODD:
			if ((pd->n_pages & 1) > 0)
				g_object_set (G_OBJECT (po), "n-pages", served_pages + 1, NULL);
			break;
		case GTK_PAGE_SET_EVEN:
			if ((pd->n_pages & 1) == 0)
				g_object_set (G_OBJECT (po), "n-pages", served_pages + 1, NULL);
			break;
		default:
			g_object_set (G_OBJECT (po), "n-pages", served_pages + fullpages, NULL);
			break;
		}
	}
	/* setup for next page (tho' not actually needed for last page) */
	if (pd->n_pages > 1)
	{
		pd->startline = bufferlineindex;
		pd->curr_page++;

		if (pd->printpages == GTK_PRINT_PAGES_RANGES)
		{
			if (!show)
				goto next_page;
			else if (pd->rangeindx + 1 < pd->rangecount)
			{
				guint16 pn;

				pd->rangeindx++;
				pn = g_array_index (pd->range_pages, guint16, pd->rangeindx);
				if (pn > pd->n_pages)
					/* force end now, or else we may loop infinitely */
					g_object_set (G_OBJECT (po), "n-pages", page_num + 1, NULL);
			}
			else /* finished all chosen pages */
			{
				/* force end now, or else we may loop infinitely */
				g_object_set (G_OBJECT (po), "n-pages", page_num + 1, NULL);
			}
		}
		else if (pd->pageset == GTK_PAGE_SET_ODD
			  || pd->pageset == GTK_PAGE_SET_EVEN)
		{
			if (!show)
				goto next_page;
		}
	}
	NEEDOPENBGL
}
/**
@brief callback for "done" signal on @a po

@param po the print operation data
@param result enumerator of result of the print job
@param pd data struct for the print job

@return
*/
static void _e2_print_done_cb (GtkPrintOperation *po,
	GtkPrintOperationResult result, PrintData *pd)
{
	if (result == GTK_PRINT_OPERATION_RESULT_APPLY)
	{
		g_object_unref (G_OBJECT (print_settings));
		print_settings = gtk_print_operation_get_print_settings (po);
		g_object_ref (G_OBJECT (print_settings));
	}
	else if (result == GTK_PRINT_OPERATION_RESULT_ERROR)
	{
		GError *error;
		gtk_print_operation_get_error (po, &error);
		NEEDCLOSEBGL
		e2_output_print_error (error->message, FALSE);
		NEEDOPENBGL
		g_error_free (error);
	}
	g_object_unref (G_OBJECT (po));
	//CHECKME other object cleanups ?
	_e2_print_data_free (pd);
}
/**
@brief asynchronously print currently-selected text or all text if no selection

@param menuitem UNUSED the activated widget in a dialog context-menu, or NULL
@param rt runtime struct to work on

@return
*/
void e2_dialog_print_cb (GtkMenuItem *menuitem, E2_ViewDialogRuntime *rt)
{
	PrintData *pd;
	GtkPrintOperation *po;
	GtkPrintOperationResult res;
	gchar *base, *utf;

	pd = _e2_print_data_new ();
#if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (pd, return;)
#else
	if (pd == NULL) return;
#endif

	pd->buf = rt->textbuffer;
	pd->textview = rt->textview;
	pd->localpath = rt->localpath;

	if (print_settings == NULL)
		print_settings = gtk_print_settings_new (); //cleaned in e2_print_clear()
	if (page_setup == NULL)
	{
		page_setup = gtk_page_setup_new ();	/* never cleaned */
		gtk_page_setup_set_top_margin (page_setup, 25.0, GTK_UNIT_MM);
		gtk_page_setup_set_bottom_margin (page_setup, 20.0, GTK_UNIT_MM);
		gtk_page_setup_set_left_margin (page_setup, 20.0, GTK_UNIT_MM); /* may be altered later */
		gtk_page_setup_set_right_margin (page_setup, 20.0, GTK_UNIT_MM);
	}

	po = gtk_print_operation_new ();
	gtk_print_operation_set_print_settings (po, print_settings);
	gtk_print_operation_set_default_page_setup (po, page_setup);
	gtk_print_operation_set_allow_async (po, TRUE);
//	gtk_print_operation_set_use_full_page (po, gboolean use_full); default FALSE
	gtk_print_operation_set_current_page (po, 0); /* enable current-page selection */

#ifdef USE_GTK2_18
	gtk_print_operation_set_embed_page_setup (po, TRUE);
	gtk_print_operation_set_support_selection (po, TRUE);
	gtk_print_operation_set_has_selection (po, TRUE); /* always selectable */

	NEEDCLOSEBGL

	if (gtk_text_buffer_get_has_selection (rt->textbuffer))
		gtk_print_settings_set_print_pages (print_settings, GTK_PRINT_PAGES_SELECTION);
#else
	NEEDCLOSEBGL

	if (gtk_text_buffer_get_has_selection (rt->textbuffer))
		pd->printpages = GTK_PRINT_PAGES_SELECTION;
#endif
//	gtk_print_operation_set_unit (po, GTK_UNIT_POINTS); default GTK_UNIT_PIXEL (is actually points, for printing ?)
	base = g_path_get_basename (rt->localpath);
	utf = F_FILENAME_FROM_LOCALE (base);
	gtk_print_operation_set_job_name (po, utf);
	F_FREE (utf, base);
	g_free (base);

	gtk_print_operation_set_custom_tab_label (po, _("Preferences"));

	g_signal_connect (G_OBJECT (po), "create-custom-widget",
		G_CALLBACK (_e2_print_get_custom_tab_cb), pd);
	g_signal_connect (G_OBJECT (po), "custom-widget-apply",
		G_CALLBACK (_e2_print_apply_extras_cb), pd);
	g_signal_connect (G_OBJECT (po), "begin-print",
		G_CALLBACK (_e2_print_begin_cb), pd);
	g_signal_connect (G_OBJECT (po), "draw-page",
		G_CALLBACK (_e2_print_draw_page_cb), pd);
//	g_signal_connect (G_OBJECT (po), "status-changed",
//		G_CALLBACK (_e2_print_status_change_cb), pd);

	res = gtk_print_operation_run (po,
		GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
		GTK_WINDOW (rt->dialog), NULL);
	switch (res)
	{
	case GTK_PRINT_OPERATION_RESULT_IN_PROGRESS:
		g_signal_connect (G_OBJECT (po), "done",
			G_CALLBACK (_e2_print_done_cb), pd);
		return;
	case GTK_PRINT_OPERATION_RESULT_APPLY:
		g_object_unref (G_OBJECT (print_settings));
		print_settings = gtk_print_operation_get_print_settings (po);
		g_object_ref (G_OBJECT (print_settings));
		break;
	case GTK_PRINT_OPERATION_RESULT_ERROR:
	{
		GError *error;
		gtk_print_operation_get_error (po, &error);
		e2_output_print_error (error->message, FALSE);
		g_error_free (error);
	}
		break;
	default:
		break;
	}

	NEEDOPENBGL
	g_object_unref (G_OBJECT (po));
	_e2_print_data_free (pd);
}

void e2_print_clear (void)
{
	if (print_settings != NULL)
		g_object_unref (G_OBJECT (print_settings));
	if (page_setup != NULL)
		g_object_unref (G_OBJECT (page_setup));
}

#endif //ndef SIMPLEPRINT
#endif //def USE_GTK2_10
