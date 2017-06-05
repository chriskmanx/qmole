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

#include "awn-title.h"

G_DEFINE_TYPE (AwnTitle, awn_title, GTK_TYPE_WINDOW)

static gint AWN_TITLE_DEFAULT_WIDTH		= 1024;
static gint AWN_TITLE_DEFAULT_HEIGHT		= 40;

static AwnSettings *settings = NULL;


static void awn_title_destroy (GtkObject *object);
static void _on_alpha_screen_changed (GtkWidget* pWidget, GdkScreen* pOldScreen, GtkWidget* pLabel);
static gboolean _on_expose (GtkWidget *widget, GdkEventExpose *expose, AwnTitle *title);
static void _update_input_shape (GtkWidget* window, int width, int height);
static gboolean  _on_configure (GtkWidget* pWidget, GdkEventConfigure* pEvent, gpointer userData);
static void _position_window (GtkWidget *window);


static void
awn_title_class_init( AwnTitleClass *this_class )
{
        GObjectClass *g_obj_class;
        GtkObjectClass *object_class;
        GtkWidgetClass *widget_class;
        
        g_obj_class = G_OBJECT_CLASS( this_class );
        object_class = (GtkObjectClass*) this_class;
        widget_class = GTK_WIDGET_CLASS( this_class );
        
        object_class->destroy =awn_title_destroy;
        //parent_class = gtk_type_class (gtk_widget_get_type ());
        
}

static void
awn_title_init( AwnTitle *title )
{

}

GtkWidget *
awn_title_new( AwnSettings *sets )
{
        settings = sets;
        AwnTitle *this = g_object_new(AWN_TITLE_TYPE, 
        			    "type", GTK_WINDOW_TOPLEVEL,
        			    "type-hint", GDK_WINDOW_TYPE_HINT_DOCK,
        			    NULL);
        _on_alpha_screen_changed (GTK_WIDGET(this), NULL, NULL);
        gtk_widget_set_app_paintable (GTK_WIDGET(this), TRUE);
        
        gint sw, sh;
	
	GdkScreen *screen = gtk_window_get_screen(GTK_WINDOW(this));
	AWN_TITLE_DEFAULT_WIDTH = gdk_screen_get_width(screen);
	
	        
        gtk_window_resize (GTK_WIDGET(this), AWN_TITLE_DEFAULT_WIDTH, AWN_TITLE_DEFAULT_HEIGHT);
        g_signal_connect (G_OBJECT (this),"destroy",
			  G_CALLBACK (gtk_main_quit), NULL);
	
	g_signal_connect (G_OBJECT (this), "expose-event",
			  G_CALLBACK (_on_expose), (gpointer)this);
	
	
	g_signal_connect (G_OBJECT (this), "configure-event",
			  G_CALLBACK (_on_configure), NULL);       
        
        _update_input_shape (GTK_WIDGET(this), AWN_TITLE_DEFAULT_WIDTH, AWN_TITLE_DEFAULT_HEIGHT);
        
               
        _position_window(GTK_WIDGET(this));
        
        this->text = NULL;
        this->x_pos = 0;
        
        return GTK_WIDGET(this);
}

static void   
awn_title_destroy       (GtkObject   *object)
{
  g_print("Destroyed\n");
  g_return_if_fail(object != NULL);
  g_return_if_fail(IS_AWN_TITLE(object));

  AwnTitle *title = AWN_TITLE(object);

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
render_bg (cairo_t *cr, double width, double height, double x, double y )
{
		/* a custom shape, that could be wrapped in a function */
	double x0  = x +0.5,   /*< parameters like cairo_rectangle */	
	y0	   = y +0.5,
	rect_width  = width,
	rect_height = height,
	radius = height;   /*< and an approximate curvature radius */

	double x1,y1;

	x1=x0+rect_width;
	y1=y0+rect_height;
	
	cairo_move_to  (cr, x0, y0 + radius);
	cairo_curve_to (cr, x0 , y0, x0 , y0, x0 + radius, y0);
	cairo_line_to (cr, x1 - radius, y0);
	cairo_curve_to (cr, x1, y0, x1, y0, x1, y0 + radius);
	cairo_line_to (cr, x1 , y1 - radius);
	cairo_curve_to (cr, x1, y1, x1, y1, x1 - radius, y1);
	cairo_line_to (cr, x0 + radius, y1);
	cairo_curve_to (cr, x0, y1, x0, y1, x0, y1- radius);
	

	cairo_close_path (cr);

	//cairo_set_source_rgb (cr, 0.5,0.5,1);
	cairo_fill_preserve (cr);
}

static void 
render (cairo_t *cr, const char *utf8, gint width, gint height, gint x_pos)
{
	cairo_set_source_rgba (cr, 1.0f, 1.0f, 1.0f, 0.0f);
	cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
	cairo_paint (cr);
	
	if (!utf8)
		return;
	cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
	
	cairo_text_extents_t extents;

	double x,y;
	
	int font_slant = CAIRO_FONT_SLANT_NORMAL;
	int font_weight = CAIRO_FONT_WEIGHT_NORMAL;
	
	if (settings->italic)
		font_slant = CAIRO_FONT_SLANT_ITALIC;
	
	if (settings->bold)
		font_weight = CAIRO_FONT_WEIGHT_BOLD;
	
	cairo_select_font_face (cr, "Sans",font_slant, font_weight);

	cairo_set_font_size (cr, settings->font_size);
	
	cairo_text_extents (cr, utf8, &extents);
	x = (width/2)-(extents.width/2 + extents.x_bearing);
	y = (height/2)-(extents.height/2 + extents.y_bearing);
	
	x = x_pos - (extents.width/2)+ extents.x_bearing;
	x += (settings->corner_radius/2);
	
	if (x <0 )
		x = 0;
	
	/* background 
	cairo_set_source_rgba (cr, 0.0f, 0.0f, 0.0f, 0.3f);
	render_bg (cr, (double) extents.width+12, (double) extents.height+12, 
		       (double) x-6, (double) y-extents.height-5 );
	*/
	/* shadow */
	cairo_set_source_rgba (cr, settings->shadow_color.red, 
				   settings->shadow_color.green, 
				   settings->shadow_color.blue,
				   settings->shadow_color.alpha);
	cairo_move_to (cr, x+1, y+1);
	cairo_show_text (cr, utf8);
	
	/* text */
	//cairo_set_source_rgba (cr, 1.0f, 1.0f, 1.0f, 1.0f);
	cairo_set_source_rgba (cr, settings->text_color.red, 
				   settings->text_color.green, 
				   settings->text_color.blue,
				   settings->text_color.alpha);
	cairo_move_to (cr, x, y);
	cairo_show_text (cr, utf8);

	/*
	cairo_text_path (cr, utf8);
	cairo_fill_preserve (cr);
	cairo_set_source_rgba (cr, settings->shadow_color.red, 
				   settings->shadow_color.green, 
				   settings->shadow_color.blue,
				   1.0);
	cairo_set_line_width (cr, 0.7);
	cairo_stroke (cr);	
	*/
}

static void 
render_pixmap (cairo_t *cr, gint width, gint height)
{
	
	cairo_scale (cr, (double) width, (double) height);
	cairo_set_source_rgba (cr, 1.0f, 1.0f, 1.0f, 0.0f);
	cairo_set_operator (cr, CAIRO_OPERATOR_SOURCE);
	cairo_paint (cr);
	

}

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

			gtk_widget_input_shape_combine_mask (window, NULL, 0, 0);
			gtk_widget_input_shape_combine_mask (window, pShapeBitmap, 0, 0);
		}
		g_object_unref ((gpointer) pShapeBitmap);
	}
}

static gboolean 
_on_expose (GtkWidget *widget, GdkEventExpose *expose, AwnTitle *title)
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
	
	if (title->text)
		render (cr, title->text, width, height, title->x_pos);
	cairo_destroy (cr);
	
	_position_window(GTK_WIDGET(widget));
	oWidth = width;
	oHeight = height;
	return FALSE;
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
	y = (int) (sh-85);
	
	gtk_window_move(GTK_WINDOW(window), x, y);
	
	
}


void 
awn_title_resize(AwnTitle *title)
{
	gtk_window_resize(GTK_WINDOW(title), 1, 100);
}

static char * name		= NULL;
static gint current_x		= 0;
static gint dest_x		= 0;
static gint step		= 100;

static gboolean
show_text( AwnTitle *title)
{
	/*
	if ( strcmp(name, title->text) != 0 )
		return FALSE;
	*/
	if (current_x == dest_x) {
		return FALSE;
	}
	
	if (current_x < dest_x) {
		if ( (dest_x - current_x) < step) 
			current_x += step/4;
		else
			current_x += step;
	} else {
		if ( (current_x - dest_x) < step) 
			current_x -= step/4;
		else
			current_x -= step;
	}
		
	if ( current_x > dest_x ) {
		title->x_pos = dest_x;
		current_x = dest_x;
	} else 
		title->x_pos = current_x;
	
	_on_expose(GTK_WIDGET(title), NULL, title);
	return TRUE;
}



void 
awn_title_show (AwnTitle *title, const char *name, gint x, gint y)
{
	if (title->text) {
		g_free(title->text);
		title->text = NULL;
	}
	
	title->text = g_strdup(name);
	name = title->text;
	dest_x = x;
	
	//g_timeout_add(60, (GSourceFunc*)show_text, (gpointer)title);
	title->x_pos = x;
	_on_expose(GTK_WIDGET(title), NULL, title);
}

void 
awn_title_hide (AwnTitle *title)
{
	if (title->text) {
		g_free(title->text);
		title->text = NULL;
		_on_expose(GTK_WIDGET(title), NULL, title);
	}
	_on_expose(GTK_WIDGET(title), NULL, title);
	//gtk_widget_hide(GTK_WIDGET (title) );
}














