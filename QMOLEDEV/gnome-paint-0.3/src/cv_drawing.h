/***************************************************************************
 *            cv_drawing.h
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
 
#include "common.h"
#include "cv_resize.h"
#include "toolbar.h"

#include <gdk-pixbuf/gdk-pixbuf.h>


void		cv_set_color_bg			( GdkColor *color );
void		cv_set_color_fg			( GdkColor *color );
void		cv_set_line_width		( gint width );
void		cv_set_filled			( gp_filled filled );
void        cv_set_tool             ( gp_tool_enum tool );
void		cv_resize_pixmap		(gint width, gint height);
void		cv_set_pixbuf			(const GdkPixbuf *pixbuf);
GdkPixbuf *	cv_get_pixbuf			( void );
gp_canvas * cv_get_canvas			( void );
void        cv_get_rect_size        ( GdkRectangle *rectangle );
void        cv_redraw               ( void );



/* GUI CallBacks */
void on_cv_drawing_realize			(GtkWidget *widget, gpointer user_data);
void on_cv_drawing_unrealize		(GtkWidget *widget, gpointer user_data);
void on_lb_pos_realize				(GtkWidget *widget, gpointer user_data);


/* GUI events */
gboolean on_cv_drawing_leave_notify_event		(GtkWidget        *widget,
                                         		GdkEventCrossing *event,
                                         		gpointer          user_data);
gboolean on_cv_drawing_button_press_event  			(GtkWidget	   *widget, 
                                                 GdkEventButton *event,
                                                 gpointer       user_data );
gboolean on_cv_drawing_button_release_event			(GtkWidget	   *widget, 
                                                 GdkEventButton *event,
                                                 gpointer       user_data );
gboolean on_cv_drawing_motion_notify_event 			(GtkWidget      *widget,
		                                         GdkEventMotion *event,
                                                 gpointer        user_data);

gboolean on_cv_drawing_expose_event					(GtkWidget	   *widget, 
												 GdkEventExpose *event,
                                                 gpointer       user_data );
