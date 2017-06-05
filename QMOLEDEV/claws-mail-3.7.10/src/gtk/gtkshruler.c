/* GtkSHRuler
 *
 *  Copyright (C) 2000-2011 Alfons Hoogervorst & The Claws Mail Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */
 
/* I derived this class from hruler. S in HRuler could be read as
 * Sylpheed (sylpheed.good-day.net), but also [S]ettable HRuler.
 * I basically ripped apart the draw_ticks member of HRuler; it
 * now draws the ticks at ruler->max_size. so gtk_ruler_set_range's
 * last parameter has the distance between two ticks (which is
 * the width of the fixed font character!
 * 
 * -- Alfons
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <gtk/gtk.h>
#include "gtkshruler.h"
#include "utils.h"
#include "gtkutils.h"

#define RULER_HEIGHT          14
#define MINIMUM_INCR          5
#define MAXIMUM_SUBDIVIDE     5
#define MAXIMUM_SCALES        10

#define ROUND(x) ((int) ((x) + 0.5))

static void gtk_shruler_class_init   	(GtkSHRulerClass *klass);
static void gtk_shruler_init         	(GtkSHRuler      *hruler);
static void gtk_shruler_draw_ticks 	(GtkRuler        *ruler);

GType
gtk_shruler_get_type(void)
{
	static GType shruler_type = 0;

  	if ( !shruler_type ) {
   		static const GTypeInfo shruler_info = {
			sizeof (GtkSHRulerClass),

			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,

			(GClassInitFunc) gtk_shruler_class_init,
			(GClassFinalizeFunc) NULL,
			NULL,	/* class_data */

			sizeof (GtkSHRuler),
			0,	/* n_preallocs */
			(GInstanceInitFunc) gtk_shruler_init,
		};
		/* inherit from GtkHRuler */
		shruler_type = g_type_register_static (GTK_TYPE_HRULER, "GtkSHRuler", &shruler_info, (GTypeFlags)0);
	}
	return shruler_type;
}

static void
gtk_shruler_class_init(GtkSHRulerClass * klass)
{
 	GtkWidgetClass * widget_class;
  	GtkRulerClass * hruler_class;

  	widget_class = GTK_WIDGET_CLASS(klass);
  	hruler_class = GTK_RULER_CLASS(klass);

	/* just neglect motion notify events */
  	widget_class->motion_notify_event = NULL /* gtk_shruler_motion_notify */;

  	/* we want the old ruler draw ticks... */
  	/* ruler_class->draw_ticks = gtk_hruler_draw_ticks; */
	hruler_class->draw_ticks = gtk_shruler_draw_ticks;
	
	/* unimplemented draw pos */
	hruler_class->draw_pos = NULL;
/*
  	hruler_class->draw_pos = gtk_shruler_draw_pos;
*/
}

static void
gtk_shruler_init (GtkSHRuler * shruler)
{
	GtkWidget * widget;
	
	widget = GTK_WIDGET (shruler);
	widget->requisition.width = widget->style->xthickness * 2 + 1;
	widget->requisition.height = widget->style->ythickness * 2 + RULER_HEIGHT;
}


GtkWidget*
gtk_shruler_new(void)
{
	return GTK_WIDGET( g_object_new( gtk_shruler_get_type(), NULL ) );
}

static void
gtk_shruler_draw_ticks(GtkRuler *ruler)
{
	GtkWidget *widget;
	cairo_t *cr;
	gint i;
	gint width, height;
	gint xthickness;
	gint ythickness;
	gint pos;

	cm_return_if_fail (ruler != NULL);
	cm_return_if_fail (GTK_IS_HRULER (ruler));

	if (!gtkut_widget_is_drawable (GTK_WIDGET(ruler))) 
		return;

	widget = GTK_WIDGET (ruler);
	
	cr = gdk_cairo_create(ruler->backing_store);
	cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
	cairo_set_line_width(cr, 1.);
	gdk_cairo_set_source_color(cr, &gtk_widget_get_style(widget)->text[GTK_STATE_NORMAL]);

	xthickness = widget->style->xthickness;
	ythickness = widget->style->ythickness;

	width = widget->allocation.width;
	height = widget->allocation.height - ythickness * 2;
  
	gtk_paint_box (widget->style, ruler->backing_store,
		       GTK_STATE_NORMAL, GTK_SHADOW_OUT, 
		       NULL, widget, "hruler",
		       0, 0, 
		       widget->allocation.width, widget->allocation.height);

	/* assume ruler->max_size has the char width */
	/* i is increment of char_width,  pos is label number
	 * y position is based on height of widget itself */
	for ( i = 0, pos = 0; i < widget->allocation.width - xthickness; i += ruler->max_size, pos++ ) {	
		gint length = height / 8;
	
		if ( pos % 10 == 0 ) length = ( 2 * height / 3 );
		else if ( pos % 5 == 0 ) length = ( height / 3 );
		
		cairo_move_to(cr, i, height + ythickness);
		cairo_line_to(cr, i, height - length);
		cairo_stroke(cr);
		
		if ( pos % 10 == 0 ) {
			gchar buf[8];
			PangoLayout *layout;

			/* draw label */
			g_snprintf(buf, sizeof buf, "%d", pos);

			layout = gtk_widget_create_pango_layout
				(GTK_WIDGET(ruler), buf);

			cairo_move_to(cr, i+2, 0);
			pango_cairo_show_layout(cr, layout);

			g_object_unref(layout);
		}
	}

	cairo_destroy(cr);
}
