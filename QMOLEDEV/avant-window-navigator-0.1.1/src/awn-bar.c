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

#include "awn-bar.h"

G_DEFINE_TYPE (AwnBar, awn_bar, GTK_TYPE_WINDOW)

#define AWN_BAR_DEFAULT_WIDTH		1024
#define AWN_BAR_DEFAULT_HEIGHT		100

static AwnSettings *settings		= NULL;
static gint dest_width			= 0;
static current_width 			= 400;
static gint screen_width		= 1024;

static void awn_bar_destroy (GtkObject *object);
static void _on_alpha_screen_changed (GtkWidget* pWidget, GdkScreen* pOldScreen, GtkWidget* pLabel);

static void _update_input_shape (GtkWidget* window, int width, int height);
static gboolean  _on_configure (GtkWidget* pWidget, GdkEventConfigure* pEvent, gpointer userData);
static void _position_window (GtkWidget *window);


static void
awn_bar_class_init( AwnBarClass *this_class )
{
        GObjectClass *g_obj_class;
        GtkObjectClass *object_class;
        GtkWidgetClass *widget_class;
        
        g_obj_class = G_OBJECT_CLASS( this_class );
        object_class = (GtkObjectClass*) this_class;
        widget_class = GTK_WIDGET_CLASS( this_class );


        parent_class = gtk_type_class (gtk_widget_get_type ());
        
}

static void
awn_bar_init( AwnBar *bar )
{

}

GtkWidget *
awn_bar_new( AwnSettings *set )
{
        settings = set;
        AwnBar *this = g_object_new(AWN_BAR_TYPE, 
        			    "type", GTK_WINDOW_TOPLEVEL,
        			    "type-hint", GDK_WINDOW_TYPE_HINT_DOCK,
        			    NULL);
        _on_alpha_screen_changed (GTK_WIDGET(this), NULL, NULL);
        gtk_widget_set_app_paintable (GTK_WIDGET(this), TRUE);
        
        _position_window(GTK_WINDOW(this));
        
        g_signal_connect (G_OBJECT (this), "expose-event",
			  G_CALLBACK (_on_expose), NULL);
	
	g_signal_connect (G_OBJECT (this), "configure-event",
			  G_CALLBACK (_on_configure), NULL);       
        
        _update_input_shape (GTK_WIDGET(this), AWN_BAR_DEFAULT_WIDTH, AWN_BAR_DEFAULT_HEIGHT);
        
               
        
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
render_rect (cairo_t *cr, double x, double y, double width, double height, double offset  )
{
	if (settings->rounded_corners) {
		/* modified from cairo snippets page */
		double x0  = x ,  	
		y0	   = y ,
		rect_width  = width,
		rect_height = height,
		radius = settings->corner_radius + offset; 

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
glass_engine (cairo_t *cr, double x, gint width, gint height)
{
	cairo_pattern_t *pat;
	cairo_set_operator (cr, CAIRO_OPERATOR_OVER);

	/* main gradient */
	pat = cairo_pattern_create_linear (0.0, 0.0, 0.0, height);
	cairo_pattern_add_color_stop_rgba (pat, 0.5, 
						settings->g_step_1.red, 
				   		settings->g_step_1.green, 
				   		settings->g_step_1.blue,
				   		settings->g_step_1.alpha);
	cairo_pattern_add_color_stop_rgba (pat, 1, 
						settings->g_step_2.red, 
				   		settings->g_step_2.green, 
				   		settings->g_step_2.blue,
				   		settings->g_step_2.alpha);
	render_rect (cr, x, height/2, width, height/2, 0);
	cairo_set_source(cr, pat);
	cairo_fill(cr);
	cairo_pattern_destroy(pat);
	


}

static void
pattern_engine (cairo_t *cr, double x, gint width, gint height)
{
	cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
	cairo_surface_t *image;
        cairo_pattern_t *pattern;
        
	//image = cairo_image_surface_create_from_png ("/usr/share/nautilus/patterns/terracotta.png");
        image = cairo_image_surface_create_from_png (settings->pattern_uri);
        pattern = cairo_pattern_create_for_surface (image);
        cairo_pattern_set_extend (pattern, CAIRO_EXTEND_REPEAT);

        cairo_set_source (cr, pattern);
	render_rect  (cr, x, (height/2), width, (height/2), 0);
        
        cairo_save(cr);
        	cairo_clip(cr);
		cairo_paint_with_alpha(cr, settings->pattern_alpha);
        cairo_restore(cr);

        cairo_pattern_destroy (pattern);
        cairo_surface_destroy (image);
}

static void 
render (cairo_t *cr, gint x_width, gint height)
{
	gint width = current_width;
	cairo_set_source_rgba (cr, 1.0f, 1.0f, 1.0f, 0.0f);
	cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
	cairo_paint (cr);
	
	double x = (screen_width-width)/2;
	
	cairo_move_to(cr, x, 0);
	cairo_set_line_width(cr, 1.0);
	
	cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
	
	glass_engine(cr, x, width, height);
	
	if (settings->render_pattern)
		pattern_engine(cr, x, width, height);
	
	/* hilight gradient */
	cairo_pattern_t *pat;
	
	pat = cairo_pattern_create_linear (0.0, 0.0, 0.0, height);
	cairo_pattern_add_color_stop_rgba (pat, 0.5,  
						settings->g_histep_1.red, 
				   		settings->g_histep_1.green, 
				   		settings->g_histep_1.blue,
				   		settings->g_histep_1.alpha);
	cairo_pattern_add_color_stop_rgba (pat, 1,   
						settings->g_histep_2.red, 
				   		settings->g_histep_2.green, 
				   		settings->g_histep_2.blue,
				   		settings->g_histep_2.alpha);
	render_rect (cr, x+1, height/2, width-2, height/5, 0);
	cairo_set_source(cr, pat);
	cairo_fill(cr);
	cairo_pattern_destroy(pat);
	
	/* internal hi-lightborder */
	cairo_set_source_rgba (cr, settings->hilight_color.red, 
				   settings->hilight_color.green, 
				   settings->hilight_color.blue,
				   settings->hilight_color.alpha);
	render_rect (cr, x+1.5, (height/2)+1.5, width-2, height, 1);
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
	cairo_set_source_rgba (cr, settings->border_color.red, 
				   settings->border_color.green, 
				   settings->border_color.blue,
				   settings->border_color.alpha);
	render_rect (cr, x +0.5, (height/2)+0.5, width, (height/2)+1, 0);
	cairo_stroke(cr);
}

gboolean 
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
	
	_position_window(GTK_WIDGET(widget));
	return FALSE;
}

static void 
render_pixmap (cairo_t *cr, gint width, gint height)
{
	
	//cairo_scale (cr, (double) width, (double) height);
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

	/* if size has changed, update shape */
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
	screen_width = sw;
	x = (int) ((sw - ww) / 2);
	y = (int) (sh-wh);
	
	if (ww != sw)
		gtk_window_resize(GTK_WINDOW(window), sw, 100);
	gtk_window_move(GTK_WINDOW(window), x, y);
}



static gint resizing = 0;


static gint step     = 2;

static void
redraw (GtkWidget *widget, gint width, gint height)
{
	cairo_t *cr = NULL;

	cr = gdk_cairo_create (widget->window);
	if (!cr)
		return ;

	render (cr, width, height);
	//render3 (cr, width, height);
	cairo_destroy (cr);
	
	return ;
}

static gboolean
resize( GtkWindow *window)
{
        if ( dest_width == current_width ) {
                resizing = 0;
                //g_print("resized\n");
                return FALSE;
        }
        
        if ( dest_width > current_width) {
                current_width += step;
                //redraw(GTK_WIDGET(window), current_width, 100);
                gtk_widget_queue_draw(GTK_WIDGET(window));
        } else {
        	current_width -= step;
                //redraw(GTK_WIDGET(window), current_width, 100);
                gtk_widget_queue_draw(GTK_WIDGET(window));
        }
        //_position_window(window);
        return TRUE;
}

void 
awn_bar_resize(GtkWindow *window, gint width)
{
        //gtk_window_resize(window, width, 100);
	dest_width = width+(settings->corner_radius*2);        
        //redraw(GTK_WIDGET(window), width, 100);
        //return;
        
        
        if (resizing) {
                return;
                             
        } else {
                resizing = 1;
                g_timeout_add(18, (GSourceFunc*)resize, (gpointer)window);
        }
                
}
















