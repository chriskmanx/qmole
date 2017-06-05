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

#include <gtk/gtk.h>

#define	DV(x)

static gint min_number_window_width;
static gboolean line_number_visible = FALSE;
#define	margin 5
#define	submargin 2

static gint calculate_min_number_window_width(GtkWidget *widget)
{
	PangoLayout *layout;
	gchar *str;
	gint width, col = 4;

	str = g_strnfill(col, 0x20);
	layout = gtk_widget_create_pango_layout(widget, str);
	g_free (str);

	pango_layout_get_pixel_size(layout, &width, NULL);
	g_object_unref(G_OBJECT(layout));

	return width;
}

/* taken from gedit and gtksourceview */
/* originated from gtk+/tests/testtext.c */

static void
get_lines (GtkTextView  *text_view,
           gint          y1,
           gint          y2,
           GArray       *buffer_coords,
           GArray       *numbers,
           gint         *countp)
{
	GtkTextIter iter;
	gint count;
	gint size;
		gint last_line_num;
	
	g_array_set_size (buffer_coords, 0);
	g_array_set_size (numbers, 0);
	
	/* Get iter at first y */
	gtk_text_view_get_line_at_y (text_view, &iter, y1, NULL);
	
	/* For each iter, get its location and add it to the arrays.
	 * Stop when we pass y2
	 */
	count = 0;
	size = 0;
	
	while (!gtk_text_iter_is_end (&iter))
	{
		gint y, height;
		
		gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);
		
		g_array_append_val (buffer_coords, y);
		last_line_num = gtk_text_iter_get_line (&iter);
		g_array_append_val (numbers, last_line_num);
		
		++count;
		
		if ((y + height) >= y2)
		break;
		
		gtk_text_iter_forward_line (&iter);
	}
	
	if (gtk_text_iter_is_end (&iter))
	{
		gint y, height;
		gint line_num;
		
		gtk_text_view_get_line_yrange (text_view, &iter, &y, &height);
		
		line_num = gtk_text_iter_get_line (&iter);
		
		if (line_num != last_line_num) {
			g_array_append_val (buffer_coords, y);
			g_array_append_val (numbers, line_num);
			++count;
		}
	}
	
	*countp = count;
}

static gint
line_numbers_expose (GtkWidget      *widget,
                     GdkEventExpose *event)
{
	GtkTextView *text_view;
	GdkWindow *win;
//	GtkStyle *style;
	PangoLayout *layout;
	PangoAttrList *alist;
	PangoAttribute *attr;
	GArray *numbers;
	GArray *pixels;
	gint y1, y2;
	gint count;
	gint layout_width;
	gint justify_width = 0;
	gint i;
//	gchar *str;
	gchar str [8];  /* we don't expect more than ten million lines */
	GdkGC *gc;
	gint height;
	
	if (line_number_visible){{{{{	// omit calculation
	
	text_view = GTK_TEXT_VIEW (widget);
	
	/* See if this expose is on the line numbers window */
/*	left_win = gtk_text_view_get_window (text_view,
	                                     GTK_TEXT_WINDOW_LEFT);
	right_win = gtk_text_view_get_window (text_view,
	                                      GTK_TEXT_WINDOW_RIGHT);
	
	if (event->window == left_win)
	{
		type = GTK_TEXT_WINDOW_LEFT;
		target = event->window;
	}
	else if (event->window == right_win)
	{
		type = GTK_TEXT_WINDOW_RIGHT;
		target = right_win;
	}
	else
		return FALSE;
*/	
	win = gtk_text_view_get_window (text_view,
	                                GTK_TEXT_WINDOW_LEFT);
	if (event->window != win)
		return FALSE;
	
//	style = gtk_style_copy (widget->style);
//	style = gtk_style_copy (gtk_widget_get_default_style());
	
	y1 = event->area.y;
	y2 = y1 + event->area.height;
	
	gtk_text_view_window_to_buffer_coords (text_view,
	                                       GTK_TEXT_WINDOW_LEFT,
	                                       0,
	                                       y1,
	                                       NULL,
	                                       &y1);
	
	gtk_text_view_window_to_buffer_coords (text_view,
	                                       GTK_TEXT_WINDOW_LEFT,
	                                       0,
	                                       y2,
	                                       NULL,
	                                       &y2);
	
	numbers = g_array_new (FALSE, FALSE, sizeof (gint));
	pixels = g_array_new (FALSE, FALSE, sizeof (gint));
	
	get_lines (text_view,
	           y1,
	           y2,
	           pixels,
	           numbers,
	           &count);
	
	/* a zero-lined document should display a "1"; we don't need to worry about
	scrolling effects of the text widget in this special case */
	
	if (count == 0)
	{
		gint y = 0;
		gint n = 0;
		count = 1;
		g_array_append_val (pixels, y);
		g_array_append_val (numbers, n);
	}
	
DV({g_print("Painting line numbers %d - %d\n",
			g_array_index(numbers, gint, 0),
			g_array_index(numbers, gint, count - 1));	});
	
	layout = gtk_widget_create_pango_layout (widget, "");
	
//	str = g_strdup_printf ("%d", gtk_text_buffer_get_line_count(text_view->buffer));
	g_snprintf (str, sizeof (str),
			"%d", MAX (99, gtk_text_buffer_get_line_count(text_view->buffer)));
	pango_layout_set_text (layout, str, -1);
//	g_free (str);
	
	pango_layout_get_pixel_size (layout, &layout_width, NULL);
	
	min_number_window_width = calculate_min_number_window_width(widget);
	if (layout_width > min_number_window_width)
		gtk_text_view_set_border_window_size (text_view,
			GTK_TEXT_WINDOW_LEFT, layout_width + margin + submargin);
	else {
//		if ((gtk_text_view_get_border_window_size (text_view, GTK_TEXT_WINDOW_LEFT) - 5) > layout_width) {
			gtk_text_view_set_border_window_size (text_view,
				GTK_TEXT_WINDOW_LEFT, min_number_window_width + margin + submargin);
//		}
		justify_width = min_number_window_width - layout_width;
	}
	
	pango_layout_set_width (layout, layout_width);
	pango_layout_set_alignment (layout, PANGO_ALIGN_RIGHT);
	
	alist = pango_attr_list_new();
	attr = pango_attr_foreground_new(
		widget->style->text_aa->red,
		widget->style->text_aa->green,
		widget->style->text_aa->blue);
	attr->start_index = 0;
	attr->end_index = G_MAXUINT;
	pango_attr_list_insert(alist, attr);
	pango_layout_set_attributes(layout, alist);
	
	/* Draw fully internationalized numbers! */
	
	i = 0;
	while (i < count)
	{
		gint pos;
		
		gtk_text_view_buffer_to_window_coords (text_view,
		                                       GTK_TEXT_WINDOW_LEFT,
		                                       0,
		                                       g_array_index (pixels, gint, i),
		                                       NULL,
		                                       &pos);
		
//		str = g_strdup_printf ("%d", g_array_index (numbers, gint, i) + 1);
		g_snprintf (str, sizeof (str),
				"%d", g_array_index (numbers, gint, i) + 1);
		
		pango_layout_set_text (layout, str, -1);
		
		gtk_paint_layout (widget->style,
		                  win,
		                  GTK_WIDGET_STATE (widget),
		                  FALSE,
		                  NULL,
		                  widget,
		                  NULL,
#if GTK_CHECK_VERSION(2, 6, 0)  // Is this solution???
		                  layout_width + justify_width + margin / 2 + 1,
#else
		                  layout_width + justify_width + margin / 2,
#endif
		                  pos,
		                  layout);
//		g_free (str);
		
		++i;
	}
	
	g_array_free (pixels, TRUE);
	g_array_free (numbers, TRUE);
	
	g_object_unref (G_OBJECT (layout));
//	g_object_ref (G_OBJECT (style));
	
	/* don't stop emission, need to draw children */
	
	}}}}}
	
	gc = gdk_gc_new(event->window);
	gdk_gc_set_foreground(gc, widget->style->base);
	gdk_window_get_geometry(event->window, NULL, NULL, NULL, &height, NULL);
	gdk_draw_rectangle(event->window, gc, TRUE,
		line_number_visible ?
		layout_width + justify_width + margin : 0,
		0, submargin,
		height);
	
	g_object_unref(gc);
	
	return FALSE;
}

void show_line_numbers(GtkWidget *text_view, gboolean visible)
{
	line_number_visible = visible;
	if (visible) {
		gtk_text_view_set_border_window_size(
			GTK_TEXT_VIEW(text_view),
			GTK_TEXT_WINDOW_LEFT,
			min_number_window_width + margin + submargin);
	} else {
		gtk_text_view_set_border_window_size(
			GTK_TEXT_VIEW(text_view),
			GTK_TEXT_WINDOW_LEFT,
			submargin);
	}
}

void linenum_init(GtkWidget *text_view)
{
	min_number_window_width = calculate_min_number_window_width(text_view);
	g_signal_connect(
		G_OBJECT(text_view),
		"expose_event",
		G_CALLBACK(line_numbers_expose),
		NULL);
	show_line_numbers(text_view, FALSE);
}
/*
static void show_line_numbers(GtkWidget *text_view, gboolean visible)
{
	gtk_text_view_set_border_window_size(
		GTK_TEXT_VIEW(text_view),
		GTK_TEXT_WINDOW_LEFT,
		submargin);
	if (visible) {
		min_number_window_width = calculate_min_number_window_width(text_view);
//		gtk_text_view_set_border_window_size(
//			GTK_TEXT_VIEW(text_view),
//			GTK_TEXT_WINDOW_LEFT,
//			min_number_window_width + margin + submargin);
//			submargin);
		g_signal_connect(
			G_OBJECT(text_view),
			"expose_event",
			G_CALLBACK(line_numbers_expose),
			NULL);
	} else {
//		gtk_text_view_set_border_window_size(
//			GTK_TEXT_VIEW(text_view),
//			GTK_TEXT_WINDOW_LEFT,
//			0);
//			submargin);
		g_signal_handlers_disconnect_by_func(
			G_OBJECT(text_view),
			G_CALLBACK(line_numbers_expose),
			NULL);
	}
}
*/
