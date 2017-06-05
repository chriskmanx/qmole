/***************************************************************************
 *            cv_resize.c
 *
 *  Sun Jun  7 09:05:56 2009
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
 
#include <gtk/gtk.h>

#include "common.h"
#include "cv_resize.h"
#include "cv_drawing.h"
#include "file.h"


#define BOX_EDGE_SIZE	4


/* private functions */
static void cv_resize_start		( void );
static void cv_resize_move		( gdouble x,  gdouble y);
static void cv_resize_stop		( gdouble x,  gdouble y);
static void cv_resize_cancel	( void );

/* private data  */
static gp_canvas	*cv				=	NULL;
static GtkWidget	*cv_ev_box		=	NULL;
static GtkWidget	*cv_top_edge	=	NULL;
static GtkWidget	*cv_bottom_edge	=	NULL;
static GtkWidget	*cv_left_edge	=	NULL;
static GtkWidget	*cv_right_edge	=	NULL;
static GtkWidget	*lb_size		=	NULL;
static GdkGC 		*gc_resize		=	NULL;
static GdkColor 	edge_color		=	{ 0, 0x2f00, 0x3600, 0x9800  };
static gboolean		b_resize		=	FALSE;
static gboolean		b_rz_init		=	FALSE;
static gint			x_res			=	0;
static gint			y_res			=	0;

/*
 *   CODE
 */

void
cv_resize_set_canvas ( gp_canvas * canvas )
{
	cv = canvas;
}


void
cv_resize_draw ( void )
{
	GString *str = g_string_new("");
	gint x,y;

	if (b_resize)
	{
		gdk_draw_line ( cv->drawing, gc_resize, 0, 0, x_res, 0 );
		gdk_draw_line ( cv->drawing, gc_resize, 0, y_res, x_res, y_res );
		gdk_draw_line ( cv->drawing, gc_resize, x_res, 0, x_res, y_res );
		gdk_draw_line ( cv->drawing, gc_resize, 0, 0, 0, y_res );
		x = x_res;
		y = y_res;
	}
	else
	{
		x = cv->widget->allocation.width;
		y = cv->widget->allocation.height;
	}
	g_string_printf (str, "%dx%d", x, y );
	gtk_label_set_text( GTK_LABEL(lb_size), str->str );
	g_string_free( str, TRUE);
}

void
cv_resize_adjust_box_size (gint width, gint height)
{
	gint w, h;
	w = ( width  < BOX_EDGE_SIZE )?width :BOX_EDGE_SIZE;
    h = ( height < BOX_EDGE_SIZE )?height:BOX_EDGE_SIZE;
	gtk_widget_set_size_request ( cv_top_edge		, w, BOX_EDGE_SIZE );
	gtk_widget_set_size_request ( cv_bottom_edge	, w, BOX_EDGE_SIZE );
	gtk_widget_set_size_request ( cv_left_edge		, BOX_EDGE_SIZE, h );
	gtk_widget_set_size_request ( cv_right_edge		, BOX_EDGE_SIZE, h );
}

/* GUI CallBacks */
void
on_cv_ev_box_realize (GtkWidget *widget, gpointer user_data)
{
	gint8 dash_list[]	=	{ 1, 1 };
	cv_ev_box	=	widget;	
	gc_resize	=	gdk_gc_new ( widget->window );
	g_assert( gc_resize );
	/*set data to be destroyed*/
	g_object_set_data_full (	G_OBJECT(widget), "gc_resize", 
	                       		(gpointer)gc_resize , 
	                        	(GDestroyNotify)g_object_unref );
	gdk_gc_set_function ( gc_resize, GDK_INVERT );
	gdk_gc_set_dashes ( gc_resize, 0, dash_list, 2 );
	gdk_gc_set_line_attributes ( gc_resize, 1, GDK_LINE_ON_OFF_DASH,
	                             GDK_CAP_NOT_LAST, GDK_JOIN_ROUND );
}

void
on_lb_size_realize (GtkWidget *widget, gpointer user_data)
{
	lb_size	=	widget;
}


void
on_cv_right_realize (GtkWidget *widget, gpointer user_data)
{
	GdkCursor *cursor;
	cv_right_edge = widget;
	gtk_widget_set_size_request ( widget, BOX_EDGE_SIZE, BOX_EDGE_SIZE );
	gtk_widget_modify_bg ( widget, GTK_STATE_NORMAL , &edge_color );
	cursor = gdk_cursor_new_for_display ( gtk_widget_get_display(widget) , GDK_RIGHT_SIDE );
	g_assert (cursor);
	gdk_window_set_cursor ( widget->window, cursor );
	gdk_cursor_unref ( cursor );
}

void 
on_cv_bottom_right_realize (GtkWidget *widget, gpointer user_data)
{
	GdkCursor *cursor;
	gtk_widget_set_size_request ( widget, BOX_EDGE_SIZE, BOX_EDGE_SIZE );
	gtk_widget_modify_bg ( widget, GTK_STATE_NORMAL , &edge_color );
	cursor = gdk_cursor_new_for_display ( gtk_widget_get_display(widget) , GDK_BOTTOM_RIGHT_CORNER );
	g_assert (cursor);
	gdk_window_set_cursor ( widget->window, cursor );
	gdk_cursor_unref ( cursor );
}

void
on_cv_bottom_realize (GtkWidget *widget, gpointer user_data)
{
	GdkCursor *cursor;
	cv_bottom_edge = widget;
	gtk_widget_set_size_request ( widget, BOX_EDGE_SIZE, BOX_EDGE_SIZE );
	gtk_widget_modify_bg ( widget, GTK_STATE_NORMAL , &edge_color );
	cursor = gdk_cursor_new_for_display ( gtk_widget_get_display(widget) , GDK_BOTTOM_SIDE );
	g_assert (cursor);
	gdk_window_set_cursor ( widget->window, cursor );
	gdk_cursor_unref ( cursor );
}

void
on_cv_top_realize (GtkWidget *widget, gpointer user_data)
{
	cv_top_edge = widget;
	on_cv_other_edge_realize( widget, user_data );
}

void
on_cv_left_realize (GtkWidget *widget, gpointer user_data)
{
	cv_left_edge = widget;
	on_cv_other_edge_realize( widget, user_data );
}


void
on_cv_other_edge_realize (GtkWidget *widget, gpointer user_data)
{
	gtk_widget_modify_fg ( widget, GTK_STATE_NORMAL , &edge_color  );
	gtk_widget_set_size_request ( widget, BOX_EDGE_SIZE, BOX_EDGE_SIZE );
}


/* events */
gboolean 
on_cv_other_edge_expose_event	(   GtkWidget	   *widget, 
									GdkEventExpose *event,
                                    gpointer       user_data )
{
	gdk_draw_line ( widget->window,
                    widget->style->fg_gc[gtk_widget_get_state(widget)],
                    0,0,0,widget->allocation.height);
	gdk_draw_line ( widget->window,
                    widget->style->fg_gc[gtk_widget_get_state(widget)],
                    0,0,widget->allocation.width,0);
	return TRUE;
}

gboolean 
on_cv_ev_box_expose_event (	GtkWidget	   *widget, 
							GdkEventExpose *event,
                        	gpointer       user_data )
{
	if (b_resize)
	{
		gint x_offset = cv->widget->allocation.x - cv_ev_box->allocation.x;
		gint y_offset = cv->widget->allocation.y - cv_ev_box->allocation.y;
		gint x = x_res + x_offset;
		gint y = y_res + y_offset;
		gdk_draw_line ( cv_ev_box->window, gc_resize, x_offset, y_offset, x, y_offset );
		gdk_draw_line ( cv_ev_box->window, gc_resize, x_offset, y, x, y );
		gdk_draw_line ( cv_ev_box->window, gc_resize, x, y_offset, x, y );
		gdk_draw_line ( cv_ev_box->window, gc_resize, x_offset, y_offset, x_offset, y );
	}
	gtk_widget_set_app_paintable ( cv_ev_box, b_resize );
	return TRUE;
}

gboolean 
on_cv_bottom_right_button_press_event	(   GtkWidget	   *widget, 
											GdkEventButton *event,
                                            gpointer       user_data )
{
	if ( event->type == GDK_BUTTON_PRESS )
	{
		if ( event->button == LEFT_BUTTON )
		{
			cv_resize_start();
		}
		else if ( event->button == RIGHT_BUTTON )
		{
			cv_resize_cancel();
		}
	}
	return TRUE;
}


gboolean
on_cv_bottom_right_motion_notify_event (	GtkWidget      *widget,
                                     		GdkEventMotion *event,
                                            gpointer        user_data)
{
	cv_resize_move( event->x, event->y );
	return TRUE;
}

gboolean 
on_cv_bottom_right_button_release_event (   GtkWidget	   *widget, 
                                            GdkEventButton *event,
                                            gpointer       user_data )
{
	if ( (event->type == GDK_BUTTON_RELEASE) && (event->button == LEFT_BUTTON) )
	{
		cv_resize_stop ( event->x,  event->y );
	}
	return TRUE;
}


gboolean 
on_cv_bottom_button_press_event (   GtkWidget	   *widget, 
                                    GdkEventButton *event,
                                    gpointer       user_data )
{
	return on_cv_bottom_right_button_press_event( widget, event, user_data );
}

gboolean 
on_cv_bottom_motion_notify_event (  GtkWidget      *widget,
		                            GdkEventMotion *event,
                                    gpointer        user_data)
{
	cv_resize_move( 0.0, event->y );
	return TRUE;
}

gboolean
on_cv_bottom_button_release_event ( GtkWidget	   *widget, 
                                    GdkEventButton *event,
                                  	gpointer       user_data )
{
	if ( (event->type == GDK_BUTTON_RELEASE) && (event->button == LEFT_BUTTON) )
	{
		cv_resize_stop ( 0.0,  event->y );
	}
	return TRUE;
}

gboolean
on_cv_right_button_press_event (	GtkWidget	   *widget, 
                                    GdkEventButton *event,
                                    gpointer       user_data )
{
	return on_cv_bottom_right_button_press_event( widget, event, user_data );
}

gboolean
on_cv_right_motion_notify_event (   GtkWidget      *widget,
		                            GdkEventMotion *event,
                                    gpointer        user_data)
{
	cv_resize_move( event->x, 0.0 );
	return TRUE;
}

gboolean
on_cv_right_button_release_event (  GtkWidget	   *widget, 
                                    GdkEventButton *event,
                                    gpointer       user_data )
{
	if ( (event->type == GDK_BUTTON_RELEASE) && (event->button == LEFT_BUTTON) )
	{
		cv_resize_stop ( event->x,  0.0 );
	}
	return TRUE;
}

/* private */
static void
cv_resize_start ( void )
{
	b_rz_init	=	TRUE;
}

static void
cv_resize_move ( gdouble x,  gdouble y)
{
	if( b_rz_init )
	{
		b_resize = TRUE;
		x_res = cv->widget->allocation.width + (gint)x;
		y_res = cv->widget->allocation.height + (gint)y;
		x_res	= (x_res<1)?1:x_res;
		y_res	= (y_res<1)?1:y_res;
		gtk_widget_queue_draw (cv_ev_box);
	}
}

static void
cv_resize_stop ( gdouble x,  gdouble y)
{
	if( b_resize )
	{
		gint width, height;
		width	= cv->widget->allocation.width + (gint)x;
		width	= (width<1)?1:width;
		height	= cv->widget->allocation.height + (gint)y;
		height	= (height<1)?1:height;

        undo_add_resize ( width, height );
        cv_resize_pixmap ( width, height );
		file_set_unsave ();
	}
	cv_resize_cancel();
}

static void
cv_resize_cancel ( void )
{
	b_rz_init		= FALSE;
	b_resize 	= FALSE;
	gtk_widget_queue_draw (cv_ev_box);
}
