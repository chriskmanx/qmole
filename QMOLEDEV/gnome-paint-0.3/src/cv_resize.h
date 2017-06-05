/***************************************************************************
 *            cv_resize.h
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

#include "common.h"

void cv_resize_set_canvas		( gp_canvas * canvas );
void cv_resize_draw				( void );
void cv_resize_adjust_box_size	(gint width, gint height);




/* GUI CallBacks */
void on_cv_top_realize			(GtkWidget *widget, gpointer user_data);
void on_cv_right_realize		(GtkWidget *widget, gpointer user_data);
void on_cv_bottom_right_realize (GtkWidget *widget, gpointer user_data);
void on_cv_bottom_realize		(GtkWidget *widget, gpointer user_data);
void on_cv_left_realize			(GtkWidget *widget, gpointer user_data);
void on_cv_other_edge_realize	(GtkWidget *widget, gpointer user_data);
void on_cv_ev_box_realize		(GtkWidget *widget, gpointer user_data);
void on_lb_size_realize			(GtkWidget *widget, gpointer user_data);



/* GUI events */
gboolean on_cv_other_edge_expose_event			(GtkWidget	   *widget, 
												 GdkEventExpose *event,
                                                 gpointer       user_data );
gboolean on_cv_ev_box_expose_event				(GtkWidget	   *widget, 
												 GdkEventExpose *event,
                                                 gpointer       user_data );

gboolean on_cv_bottom_right_button_press_event  (GtkWidget	   *widget, 
                                                 GdkEventButton *event,
                                                 gpointer       user_data );
gboolean on_cv_bottom_right_motion_notify_event (GtkWidget      *widget,
		                                         GdkEventMotion *event,
                                                 gpointer        user_data);
gboolean on_cv_bottom_right_button_release_event(GtkWidget	   *widget, 
                                                 GdkEventButton *event,
                                                 gpointer       user_data );


gboolean on_cv_bottom_button_press_event		(GtkWidget	   *widget, 
                                                 GdkEventButton *event,
                                                 gpointer       user_data );
gboolean on_cv_bottom_motion_notify_event		(GtkWidget      *widget,
		                                         GdkEventMotion *event,
                                                 gpointer        user_data);
gboolean on_cv_bottom_button_release_event		(GtkWidget	   *widget, 
                                                 GdkEventButton *event,
                                                 gpointer       user_data );



gboolean on_cv_right_button_press_event			(GtkWidget	   *widget, 
                                                 GdkEventButton *event,
                                                 gpointer       user_data );
gboolean on_cv_right_motion_notify_event		(GtkWidget      *widget,
		                                         GdkEventMotion *event,
                                                 gpointer        user_data);
gboolean on_cv_right_button_release_event		(GtkWidget	   *widget, 
                                                 GdkEventButton *event,
                                                 gpointer       user_data );
