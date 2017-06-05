/***************************************************************************
 *            cv_drawing.c
 *
 *  Sun Jun  7 11:31:18 2009
 *  Copyright  2009  rogerio
 *  <rogerio@<host>>
 ****************************************************************************/

/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301,  USA
 */
 
 
#include "cv_drawing.h"
#include "cv_resize.h"
#include "cv_color_pick_tool.h"
#include "cv_flood_fill_tool.h"
#include "cv_line_tool.h"
#include "cv_pencil_tool.h"
#include "cv_rectangle_tool.h"
#include "cv_ellipse_tool.h"
#include "cv_polygon_tool.h"
#include "cv_paintbrush_tool.h"
#include "cv_rounded_rectangle_tool.h"
#include "cv_airbrush_tool.h"
#include "cv_curve_tool.h"
#include "cv_rect_select.h"
#include "undo.h"
#include "color-picker.h"


#include <glib/gi18n.h>
#include <gtk/gtk.h>

/*Member functions*/
static GdkGC * 	cv_create_new_gc	( char * name );
static void		cv_create_pixmap	(gint width, gint height, gboolean b_resize);
static void		cv_print_pos		( gint x, gint y );


/* private data  */
static gp_canvas	cv;
static gp_tool		*cv_tool		=	NULL;
static GdkColor 	white_color		=	{ 0, 0xffff, 0xffff, 0xffff  };
static GdkColor 	black_color		=	{ 0, 0x0000, 0x0000, 0x0000  };
static GtkWidget	*lb_pos			=	NULL;
static gboolean		b_pos_pressed	=	FALSE;
static gint			x_pos,y_pos;


/*
 *   CODE
 */


void
cv_redraw ( void )
{
    gtk_widget_queue_draw (cv.widget);
}

void
cv_set_color_bg	( GdkColor *color )
{
	gdk_gc_set_rgb_bg_color ( cv.gc_fg, color );		
	gdk_gc_set_rgb_fg_color ( cv.gc_bg, color );		
	gdk_gc_set_rgb_bg_color ( cv.gc_fg_pencil, color );		
	gdk_gc_set_rgb_fg_color ( cv.gc_bg_pencil, color );
	gtk_widget_queue_draw ( cv.widget );
	gdk_window_process_updates (gtk_widget_get_parent_window(cv.widget), FALSE);
}

void
cv_set_color_fg	( GdkColor *color )
{
	gdk_gc_set_rgb_fg_color ( cv.gc_fg, color );
	gdk_gc_set_rgb_bg_color ( cv.gc_bg, color );
	gdk_gc_set_rgb_fg_color ( cv.gc_fg_pencil, color );
	gdk_gc_set_rgb_bg_color ( cv.gc_bg_pencil, color );
	gtk_widget_queue_draw ( cv.widget );
	gdk_window_process_updates (gtk_widget_get_parent_window(cv.widget), FALSE);
}

void
cv_set_line_width	( gint width )
{
	gdk_gc_set_line_attributes ( cv.gc_fg, width, GDK_LINE_SOLID, 
	                             GDK_CAP_ROUND, GDK_JOIN_ROUND );
	gdk_gc_set_line_attributes ( cv.gc_bg, width, GDK_LINE_SOLID, 
	                             GDK_CAP_ROUND, GDK_JOIN_ROUND );
	gtk_widget_queue_draw ( cv.widget );
	gdk_window_process_updates (gtk_widget_get_parent_window(cv.widget), FALSE);
	cv.line_width = width;
}

void
cv_set_filled ( gp_filled filled )
{
	cv.filled	=	filled;
	gtk_widget_queue_draw ( cv.widget );
	gdk_window_process_updates (gtk_widget_get_parent_window(cv.widget), FALSE);
}


void
cv_set_tool ( gp_tool_enum tool )
{
	if (cv_tool != NULL) cv_tool->destroy(NULL);
    switch ( tool )
    {
        default:
        case TOOL_NONE:
	        cv_tool = NULL;
            break;
        case TOOL_FREE_SELECT:
	        cv_tool = NULL;
            break;
        case TOOL_RECT_SELECT:
	        cv_tool = tool_rect_select_init ( &cv );
            break;
        case TOOL_ERASER:
	        cv_tool = NULL;
            break;
        case TOOL_COLOR_PICKER:
        	//cv_tool = tool_color_pick_init ( &cv );
            cv_tool = NULL;
            break;
        case TOOL_PENCIL:
            cv_tool = tool_pencil_init ( &cv );
            break;
        case TOOL_AIRBRUSH:
	        cv_tool = tool_airbrush_init ( &cv );
            break;
        case TOOL_BUCKET_FILL:
            cv_tool = tool_flood_fill_init ( &cv );
            break;
        case TOOL_ZOOM:
	        cv_tool = NULL;
            break;
        case TOOL_PAINTBRUSH:
	        cv_tool = tool_paintbrush_init ( &cv );
            break;
        case TOOL_TEXT:
	        cv_tool = NULL;
            break;
        case TOOL_LINE:
            cv_tool = tool_line_init ( &cv );
            break;
        case TOOL_RECTANGLE:
            cv_tool = tool_rectangle_init ( &cv );
            break;
        case TOOL_ELLIPSE:
            cv_tool = tool_ellipse_init ( &cv );
            break;
        case TOOL_CURVE:
	        cv_tool = tool_curve_init ( &cv );
            break;
        case TOOL_POLYGON:
            cv_tool = tool_polygon_init ( &cv );
            break;
        case TOOL_ROUNDED_RECTANGLE:
	        cv_tool = tool_rounded_rectangle_init ( &cv );
            break;
    }
	if (cv_tool == NULL) 
        gdk_window_set_cursor ( cv.drawing, NULL);
    else
        cv_tool->reset();
}


void  my_g_object_unref(gpointer data)
{
	g_print("g_object_unref\n");
	g_object_unref( G_OBJECT(data) );
}


void
cv_resize_pixmap ( gint width, gint height )
{
	cv_create_pixmap (width, height, TRUE); 
}


void
cv_set_pixbuf	(const GdkPixbuf	*pixbuf)
{
	if (pixbuf != NULL)
	{
	    GdkPixbuf *tmp ;
	    tmp = gdk_pixbuf_add_alpha(pixbuf, FALSE, 0, 0, 0);
		gint width	=	gdk_pixbuf_get_width (pixbuf);
		gint height	=	gdk_pixbuf_get_height (pixbuf);
		cv_create_pixmap (width, height, FALSE);
		gdk_draw_pixbuf	(   cv.pixmap,
					        cv.gc_fg,
						    tmp,//pixbuf,
						    0, 0,
						    0, 0,
						    width, height,
					        GDK_RGB_DITHER_NORMAL,
                            0, 0);
        g_object_unref(tmp);
		gtk_widget_queue_draw (cv.widget);
	}	
}

GdkPixbuf *
cv_get_pixbuf ( void )
{
	GdkPixbuf * pixbuf	=	NULL;
	if ( cv.pixmap != NULL )
	{
		gint w,h;
		gdk_drawable_get_size ( cv.pixmap, &w, &h );
		pixbuf = gdk_pixbuf_get_from_drawable ( NULL, 
		                                       cv.pixmap,
		                                       gdk_drawable_get_colormap (cv.pixmap),
		                                       0,0,
		                                       0,0,
		                                       w,h);
	}
	return pixbuf;
}

gp_canvas *
cv_get_canvas ( void )
{
	return &cv;
}

void        
cv_get_rect_size ( GdkRectangle *rectangle )
{
    rectangle->x = 0;
    rectangle->y = 0;
    gdk_drawable_get_size ( cv.pixmap, 
                            &rectangle->width, 
                            &rectangle->height );
    return;
}


/* GUI CallBacks */

void
on_cv_drawing_realize (GtkWidget *widget, gpointer user_data)
{
	cv.widget		=	widget;
	cv.toplevel		=	gtk_widget_get_toplevel( widget );
	cv.drawing		=	cv.widget->window;
	cv.gc_fg		=	cv_create_new_gc( "cv_gc_fg" );
	cv.gc_bg		=	cv_create_new_gc( "cv_gc_bg" );
	cv.gc_fg_pencil	=	cv_create_new_gc( "cv_gc_fg_pencil" );
	cv.gc_bg_pencil	=	cv_create_new_gc( "cv_gc_bg_pencil" );
	cv.pixmap		=	NULL;
	cv_set_color_fg ( &black_color );
	cv_set_color_bg ( &white_color );
	cv_set_line_width ( 1 );
	gdk_gc_set_line_attributes ( cv.gc_fg_pencil, 1, GDK_LINE_SOLID, 
	                             GDK_CAP_ROUND, GDK_JOIN_ROUND );
	gdk_gc_set_line_attributes ( cv.gc_bg_pencil, 1, GDK_LINE_SOLID, 
	                             GDK_CAP_ROUND, GDK_JOIN_ROUND );
    
	cv_set_filled ( FILLED_NONE );
	cv_resize_set_canvas ( &cv );
	cv_create_pixmap ( 320, 200, TRUE);
}

void 
on_cv_drawing_unrealize	(GtkWidget *widget, gpointer user_data)
{
	/*free all private data*/
	g_print("unrealize canvas\n");
	cv_set_tool ( TOOL_NONE );
}

void 
on_lb_pos_realize (GtkWidget *widget, gpointer user_data)
{
	lb_pos	=	widget;
}

/* events */
gboolean
on_cv_drawing_button_press_event (	GtkWidget	   *widget, 
                               		GdkEventButton *event,
                                	gpointer       user_data )
{
	gboolean ret	=	TRUE;
	b_pos_pressed	=	TRUE;
	x_pos	= (gint)event->x;
	y_pos	= (gint)event->y;
	cv_print_pos ( event->x, event->y );

	if ( cv_tool != NULL )
	{
		ret = cv_tool->button_press( event );
	}

	return ret;
}

gboolean
on_cv_drawing_button_release_event (	GtkWidget	   *widget, 
                                    	GdkEventButton *event,
                                    	gpointer       user_data )
{
	gboolean ret	=	TRUE;
	b_pos_pressed	=	FALSE;
	cv_print_pos ( event->x, event->y );

	if ( cv_tool != NULL )
	{
		ret = cv_tool->button_release( event );
	}

	return ret;
}
gboolean 
on_cv_drawing_leave_notify_event ( GtkWidget        *widget,
                                 GdkEventCrossing *event,
                                 gpointer          user_data)
{
	gtk_label_set_text( GTK_LABEL(lb_pos), "" );
}
									
gboolean
on_cv_drawing_motion_notify_event (	GtkWidget      *widget,
		                        	GdkEventMotion *event,
                                	gpointer        user_data)
{
	gboolean ret = TRUE;
	if ( cv_tool != NULL )
	{
		ret = cv_tool->button_motion( event );
	}
	cv_print_pos ( event->x, event->y );
	return ret;
}

gboolean 
on_cv_drawing_expose_event	(   GtkWidget	   *widget, 
								GdkEventExpose *event,
               					gpointer       user_data )
{
    gdk_draw_drawable (	widget->window,
                    	widget->style->fg_gc[gtk_widget_get_state(widget)],
    	                cv.pixmap,
    	                event->area.x, event->area.y,
    	                event->area.x, event->area.y,
    	                event->area.width, event->area.height);	
	
	if ( cv_tool != NULL )
	{
		cv_tool->draw();
	}

	cv_resize_draw();
    return TRUE;
}

/*private functions*/

static GdkGC *
cv_create_new_gc ( char * name )
{
	GdkGC  * gc;
	gc	=	gdk_gc_new ( cv.widget->window );
	g_assert( gc );
	/*set data to be destroyed*/
	g_object_set_data_full (	G_OBJECT( cv.widget ), name, 
	                       		(gpointer)gc , 
	                        	(GDestroyNotify)g_object_unref );
	return gc;
}


void  destroy_pixmap ( gpointer data )
{
    g_object_unref ( data );
    g_print ("destroy pixmap\n");
}

static void
cv_create_pixmap ( gint width, gint height, gboolean b_resize )
{
	GdkPixmap *		px;
	px = gdk_pixmap_new ( cv.drawing, width, height, -1);
	g_assert( px );
	if (b_resize)
	{
		/* initial drawing is filled with background color */ 
		gdk_draw_rectangle( px, cv.gc_bg, TRUE, 0, 0, width, height );
		if ( cv.pixmap != NULL )
		{
			gint w,h;
			gdk_drawable_get_size ( cv.pixmap, &w, &h );
			if ( width < w ) w = width;
			if ( height < h ) h = height;
			gdk_draw_drawable (	px,
				            	cv.gc_fg,
					            cv.pixmap,
					            0, 0,
					            0, 0,
					            w, h );
		}
	}
	/*set new data to be destroyed and destroy old data*/
	g_object_set_data_full (	G_OBJECT(cv.widget), "cv_pixmap", 
	                       		(gpointer)px, 
	                        	(GDestroyNotify)destroy_pixmap );
	cv.pixmap	=	px;

	gtk_widget_set_size_request ( cv.widget, width, height );
	cv_resize_adjust_box_size (width, height);
}

static void		
cv_print_pos ( gint x, gint y )
{
	GString *str = g_string_new("");
	if ( b_pos_pressed )
	{
		gint w	=	x - x_pos;
		gint h	=	y - y_pos;
		w = ( w < 0 )?(w-1):(w+1);
		h = ( h < 0 )?(h-1):(h+1);
		g_string_printf (str, "%d,%d->%d,%d (%dx%d)", x_pos, y_pos, x, y, w, h);
	}
	else
	{
		g_string_printf (str, "%d,%d", x, y );
	}
	gtk_label_set_text( GTK_LABEL(lb_pos), str->str );
	g_string_free( str, TRUE);
}
