/*
 *  Copyright (C) 2007 Neil Jagdish Patel <njpatel@gmail.com>
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
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301  USA.
 *
 *  Author : Neil Jagdish Patel <njpatel@gmail.com>
 *
 *  Notes : Thanks to MacSlow (macslow.thepimp.net) for the transparent & shaped
 *	    window code.
*/

#include "config.h"

#include "awn-window.h"

G_DEFINE_TYPE (AwnWindow, awn_window, GTK_TYPE_WINDOW)

#define AWN_WINDOW_DEFAULT_WIDTH		300
#define AWN_WINDOW_DEFAULT_HEIGHT		100

static AwnSettings *settings		= NULL;

static void awn_window_destroy (GtkObject *object);
static void _on_alpha_screen_changed (GtkWidget* pWidget, GdkScreen* pOldScreen, GtkWidget* pLabel);

static void _update_input_shape (GtkWidget* window, int width, int height);
static gboolean  _on_configure (GtkWidget* pWidget, GdkEventConfigure* pEvent, gpointer userData);
static void _position_window (GtkWidget *window);
static gboolean _on_expose (GtkWidget *widget, GdkEventExpose *expose);


static void
awn_window_class_init( AwnWindowClass *this_class )
{
        GObjectClass *g_obj_class;
        GtkObjectClass *object_class;
        GtkWidgetClass *widget_class;
        
        g_obj_class = G_OBJECT_CLASS( this_class );
        object_class = (GtkObjectClass*) this_class;
        widget_class = GTK_WIDGET_CLASS( this_class );
        
}

static void
awn_window_init( AwnWindow *window )
{

}

GtkWidget *
awn_window_new( AwnSettings *set )
{
        settings = set;
        AwnWindow *this = g_object_new(AWN_WINDOW_TYPE, 
        			    "type", GTK_WINDOW_TOPLEVEL,
        			    "type-hint", GDK_WINDOW_TYPE_HINT_DOCK,
        			    NULL);
        _on_alpha_screen_changed (GTK_WIDGET(this), NULL, NULL);
        gtk_widget_set_app_paintable (GTK_WIDGET(this), TRUE);
        gtk_window_resize (GTK_WIDGET(this), AWN_WINDOW_DEFAULT_WIDTH, AWN_WINDOW_DEFAULT_HEIGHT);
        g_signal_connect (G_OBJECT (this), "expose-event",
			  G_CALLBACK (_on_expose), NULL);
	
	g_signal_connect (G_OBJECT (this), "configure-event",
			  G_CALLBACK (_on_configure), NULL);       
        
        _update_input_shape (GTK_WIDGET(this), AWN_WINDOW_DEFAULT_WIDTH, AWN_WINDOW_DEFAULT_HEIGHT);
        
               
        
        return GTK_WIDGET(this);
}


static void 
_on_alpha_screen_changed (GtkWidget* pWidget, GdkScreen* pOldScreen, GtkWidget* pLabel)
{                       
	GdkScreen* pScreen = gtk_widget_get_screen (pWidget);
	GdkColormap* pColormap = gdk_screen_get_rgba_colormap (pScreen);
      
	if (!pColormap)
		pColormap = gdk_screen_get_rgb_colormap (pScreen);

	gtk_widget_set_colormap (pWidget, pColormap);
}

static void
render_rect (cairo_t *cr, double x, double y, double width, double height  )
{
	if (settings->rounded_corners) {
		/* modified from cairo snippets page */
		double x0  = x ,  	
		y0	   = y ,
		rect_width  = width,
		rect_height = height,
		radius = 10.5; 

		double x1,y1;

		x1=x0+rect_width;
		y1=y0+rect_height;
	
		cairo_move_to  (cr, x0, y0 + radius);
		cairo_curve_to (cr, x0 , y0, x0 , y0, x0 + radius, y0);
		cairo_line_to (cr, x1 - radius, y0);
		cairo_curve_to (cr, x1, y0, x1, y0, x1, y0 + radius);
		cairo_line_to (cr, x1 , y1 );
		cairo_line_to (cr, x0 , y1);
	
        	cairo_close_path (cr);

	} else 
		cairo_rectangle(cr, x, y, width, height);
}

static void
glass_engine (cairo_t *cr, gint width, gint height)
{
	cairo_pattern_t *pat;
	cairo_set_operator (cr, CAIRO_OPERATOR_OVER);

	/* main gradient */
	pat = cairo_pattern_create_linear (0.0, 0.0, 0.0, height);
	cairo_pattern_add_color_stop_rgba (pat, 0.5, 1.0, 1.0, 1.0, 0.3);
	cairo_pattern_add_color_stop_rgba (pat, 1, 0.0, 0.0, 0.0, 0.3);
	render_rect (cr, 0, height/2, width, height/2);
	cairo_set_source(cr, pat);
	cairo_fill(cr);
	cairo_pattern_destroy(pat);
	
	/* hilight gradient */
	pat = cairo_pattern_create_linear (0.0, 0.0, 0.0, height);
	cairo_pattern_add_color_stop_rgba (pat, 0.5, 1, 1, 1, 0.15);
	cairo_pattern_add_color_stop_rgba (pat, 1, 0.7, 0.7, 0.7, 0.02);
	render_rect (cr, 1, height/2, width-2, height/5);
	cairo_set_source(cr, pat);
	cairo_fill(cr);
	cairo_pattern_destroy(pat);

}

static void
pattern_engine (cairo_t *cr, gint width, gint height)
{
	cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
	cairo_surface_t *image;
        cairo_pattern_t *pattern;
        
	//image = cairo_image_surface_create_from_png ("/usr/share/nautilus/patterns/terracotta.png");
        image = cairo_image_surface_create_from_png (settings->pattern_uri);
        pattern = cairo_pattern_create_for_surface (image);
        cairo_pattern_set_extend (pattern, CAIRO_EXTEND_REPEAT);

        cairo_set_source (cr, pattern);
	render_rect  (cr, 0, (height/2), width, (height/2));
        
        cairo_save(cr);
        	cairo_clip(cr);
		cairo_paint_with_alpha(cr, settings->pattern_alpha);
        cairo_restore(cr);

        cairo_pattern_destroy (pattern);
        cairo_surface_destroy (image);
}

static void 
render (cairo_t *cr, gint width, gint height)
{
	cairo_set_source_rgba (cr, 1.0f, 1.0f, 1.0f, 0.0f);
	cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
	cairo_paint (cr);
	return;
	cairo_set_line_width(cr, 1.0);
	
	if (settings->render_pattern)
		pattern_engine(cr, width, height);
	
	glass_engine(cr, width, height);
	
	/* internal border */
	cairo_set_source_rgba (cr, 1.0f, 1.0f, 1.0f, 0.2f);
	render_rect (cr, 1.5, (height/2)+1.5, width-2.5, (height/2)-2);
	cairo_stroke(cr);
	
	/* glow
	gfloat alpha = 0.2;
	for (int i =1; i < 1; i++) {
		alpha -=0.2/5;
		cairo_set_source_rgba (cr, 1.0f, 1.0f, 1.0f, alpha);
		
		gfloat x = 1.5 + i;
		gfloat w = (2*x)-0.5;
		render_rect (cr, x, (height/2)+x, width-0.5-w, (height/2)-2-(2*i));
		cairo_stroke(cr);
	}
	*/
	/* border */
	cairo_set_source_rgba (cr, 0.0f, 0.0f, 0.0f, 1.0);
	render_rect (cr, 0.5, (height/2)+0.5, width-0.5, (height/2));
	cairo_stroke(cr);
}

static gboolean 
_on_expose (GtkWidget *widget, GdkEventExpose *expose)
{
	static oWidth;
	static oHeight;
	gint width;
	gint height;
	cairo_t *cr = NULL;

	cr = gdk_cairo_create (widget->window);
	if (!cr)
		return FALSE;

	gtk_window_get_size (GTK_WINDOW (widget), &width, &height);
	render (cr, width, height);
	//render3 (cr, width, height);
	cairo_destroy (cr);
	
	if ( oWidth != width || oHeight != height)
		_position_window(GTK_WIDGET(widget));
	oWidth = width;
	oHeight = height;
	return FALSE;
}

static void 
render_pixmap (cairo_t *cr, gint width, gint height)
{
	
	cairo_scale (cr, (double) width, (double) height);
	cairo_set_source_rgba (cr, 1.0f, 1.0f, 1.0f, 0.0f);
	cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
	cairo_paint (cr);
	
	cairo_pattern_t *pat;
	
	cairo_set_line_width(cr, 0.05);
	pat = cairo_pattern_create_linear (0.0, 0.0, 0.0, 1.0);
	cairo_pattern_add_color_stop_rgba (pat, 0.5, 1.0, 1.0, 1.0, 1);
	cairo_pattern_add_color_stop_rgba (pat, 1, 0.8, 0.8, 0.8, 1);
	cairo_rectangle (cr, 0, 0.5, 1, 1);
	cairo_set_source(cr, pat);
	cairo_fill(cr);
	cairo_pattern_destroy(pat);
	
	
}

#if !GTK_CHECK_VERSION(2,9,0)
/* this is piece by piece taken from gtk+ 2.9.0 (CVS-head with a patch applied
regarding XShape's input-masks) so people without gtk+ >= 2.9.0 can compile and
run input_shape_test.c */
void do_shape_combine_mask (GdkWindow* window,
							GdkBitmap* mask,
							gint x,
							gint y)
{
	Pixmap pixmap;
	int ignore;
	int maj;
	int min;

	if (!XShapeQueryExtension (GDK_WINDOW_XDISPLAY (window), &ignore, &ignore))
		return;

	if (!XShapeQueryVersion (GDK_WINDOW_XDISPLAY (window), &maj, &min))
		return;

	/* for shaped input we need at least XShape 1.1 */
	if (maj != 1 && min < 1)
		return;

	if (mask)
		pixmap = GDK_DRAWABLE_XID (mask);
	else
	{
		x = 0;
		y = 0;
		pixmap = None;
	}

	XShapeCombineMask (GDK_WINDOW_XDISPLAY (window),
					   GDK_DRAWABLE_XID (window),
					   ShapeInput,
					   x,
					   y,
					   pixmap,
					   ShapeSet);
}
#endif

static void 
_update_input_shape (GtkWidget* window, int width, int height)
{
	static GdkBitmap* pShapeBitmap = NULL;
	static cairo_t* pCairoContext = NULL;

	pShapeBitmap = (GdkBitmap*) gdk_pixmap_new (NULL, width, height, 1);
	if (pShapeBitmap)
	{
		pCairoContext = gdk_cairo_create (pShapeBitmap);
		if (cairo_status (pCairoContext) == CAIRO_STATUS_SUCCESS)
		{
			render_pixmap (pCairoContext, width, height);
			cairo_destroy (pCairoContext);


#if !GTK_CHECK_VERSION(2,9,0)
			do_shape_combine_mask (window->window, NULL, 0, 0);
			do_shape_combine_mask (window->window, pShapeBitmap, 0, 0);
#else
			gtk_widget_input_shape_combine_mask (window, NULL, 0, 0);
			gtk_widget_input_shape_combine_mask (window, pShapeBitmap, 0, 0);
#endif
		}
		g_object_unref ((gpointer) pShapeBitmap);
	}
}

static gboolean 
_on_configure (GtkWidget* pWidget, GdkEventConfigure* pEvent, gpointer userData)
{
	gint iNewWidth = pEvent->width;
	gint iNewHeight = pEvent->height;

	if (1)
	{
		_update_input_shape (pWidget, iNewWidth, iNewHeight);

	}

	return FALSE;
}

static void
_position_window (GtkWidget *window)
{
	GdkScreen *screen;
	gint ww, wh;
	gint sw, sh;
	gint x, y;
	
	gtk_window_get_size(GTK_WINDOW(window), &ww, &wh);
	screen = gtk_window_get_screen(GTK_WINDOW(window));
	sw = gdk_screen_get_width(screen);
	sh = gdk_screen_get_height(screen);
	
	x = (int) ((sw - ww) / 2);
	y = (int) (sh-wh);
	
	gtk_window_move(GTK_WINDOW(window), x, y);
}


void 
awn_window_resize(AwnWindow *window)
{
	gtk_window_resize(GTK_WINDOW(window), 1, 100);
}
















