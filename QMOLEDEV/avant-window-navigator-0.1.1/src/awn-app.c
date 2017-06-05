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
*/

#include "awn-app.h"

#include "config.h"

#define  AWN_FRAME_RATE	25

static AwnSettings *settings = NULL;

static gboolean on_button_press_event  (GtkWidget *widget, GdkEventButton *event, WnckWindow *win);
static void on_name_changed (WnckWindow *window, AwnApp *app);
static void on_icon_changed (WnckWindow *window, AwnApp *app);
static gboolean on_proximity_in_event (GtkWidget *eb, GdkEventCrossing *event, AwnApp *app);
static gboolean on_proximity_out_event (GtkWidget *eb, GdkEventCrossing *event, AwnApp *app);
static gboolean on_drag_motion_event (GtkWidget *widget, GdkDragContext *context, gint x, gint y, guint t, AwnApp *app);
static gboolean opening_effect (AwnApp *app);

static const GtkTargetEntry drop_types[] = {
	{ "text/uri-list", 0, 0 }
};
static const gint n_drop_types = G_N_ELEMENTS (drop_types);

AwnApp*
awn_app_new (WnckWindow *window, AwnSettings *sets)
{
	settings = sets;
	AwnApp *app = g_new0 (AwnApp, 1);
	
	app->window = window;
	app->xid =wnck_window_get_xid(app->window);
	app->alignment = gtk_alignment_new (0.5, 1, 0, 0);
	gtk_alignment_set_padding(GTK_ALIGNMENT (app->alignment), 0 ,0, 0, 0);
	gtk_widget_set_size_request(app->alignment, 60, 100);
	app->event_box = gtk_event_box_new();
	gtk_event_box_set_visible_window (GTK_EVENT_BOX (app->event_box), FALSE);
	app->image = gtk_image_new ();
	
	app->wnck_icon = gdk_pixbuf_copy (wnck_window_get_icon (app->window));
	app->current_icon = app->wnck_icon;
	gtk_image_set_from_pixbuf(GTK_IMAGE (app->image), app->current_icon);
	app->active_icon = NULL;
	
	gtk_container_add (GTK_CONTAINER (app->alignment), app->event_box);
	gtk_container_add (GTK_CONTAINER (app->event_box), app->image);
	
	gtk_drag_dest_set (GTK_WIDGET (app->event_box),
                           GTK_DEST_DEFAULT_ALL,
                           drop_types, n_drop_types,
                           GDK_ACTION_MOVE);
	gtk_drag_dest_set_track_motion  (GTK_WIDGET (app->event_box),TRUE);
	g_signal_connect (G_OBJECT (app->event_box), "button-press-event",
        		  G_CALLBACK (on_button_press_event), (gpointer)app->window);
        
        g_signal_connect (G_OBJECT (app->window), "name_changed",
        		  G_CALLBACK (on_name_changed), (gpointer)app);
        
        g_signal_connect (G_OBJECT (app->window), "icon_changed",
        		  G_CALLBACK (on_icon_changed), (gpointer)app);
        		  
        g_signal_connect (G_OBJECT (app->event_box), "enter-notify-event",
        		  G_CALLBACK (on_proximity_in_event), (gpointer)app);
     	
     	g_signal_connect (G_OBJECT (app->event_box), "leave-notify-event",
        		  G_CALLBACK (on_proximity_out_event), (gpointer)app);
        
        g_signal_connect( G_OBJECT( app->event_box ), "drag-motion",
			  G_CALLBACK( on_drag_motion_event), (gpointer) app );
	
	app->current_state = AWN_APP_STATE_NORMAL;
	app->effect_lock = FALSE;
	app->mouse_in = FALSE;
	if ( wnck_window_is_skip_tasklist(window) )
		return app;
	g_timeout_add(AWN_FRAME_RATE, (GSourceFunc)opening_effect, (gpointer)app);
	return app;
}

static gboolean
opening_effect (AwnApp *app)
{
	if (app->current_state == AWN_APP_STATE_CLOSING)
		return FALSE;
	if (app->effect_lock) {
		if ( app->current_effect != AWN_APP_EFFECT_OPENING)
			return TRUE;
	} else {
		app->effect_lock = TRUE;
		app->current_effect = AWN_APP_EFFECT_OPENING;
		app->current_state = AWN_APP_STATE_OPENING;
		app->current_step = 0;
		app->current_scale = 0.5;
		app->effect_direction = AWN_APP_EFFECT_DIRECTION_UP;
		
	}
	
	GtkWidget *align = app->alignment;
	gint max = 10;
	
	app->current_step++;
	app->current_scale += (gfloat)0.5/max;
	gtk_alignment_set(GTK_ALIGNMENT(align), 0.5, app->current_scale, 0, 0.0);
	
	if ( app->current_step == max ) {
		app->current_state = AWN_APP_STATE_NORMAL;
		app->effect_lock = FALSE;
		return FALSE;
	}
	
	return TRUE;
}

static gboolean    
on_button_press_event  (GtkWidget *widget, GdkEventButton *event, WnckWindow *win)
{
	g_return_val_if_fail (WNCK_IS_WINDOW (win), TRUE);
	GtkWidget *menu = NULL;
	
	switch (event->button) {
		case 1:
			if ( wnck_window_is_active( win ) ) {
				wnck_window_minimize( win );
				return TRUE;
			}
			wnck_window_activate( win, 
						gtk_get_current_event_time() );
			break;
		
		case 3:
			menu = wnck_create_window_action_menu(win);
			gtk_menu_popup(GTK_MENU(menu), NULL, NULL, NULL, 
						       NULL, 3, event->time);
			break;
		
		default:
			return FALSE;
	}
	 
	return TRUE;
}

static void 
on_name_changed (WnckWindow *window, AwnApp *app)
{
        //g_print("Name Changed : %s\n", wnck_window_get_name(window)); 
}

static void 
on_icon_changed (WnckWindow *window, AwnApp *app)
{
        //g_print("Icon Changed : %s\n", wnck_window_get_name(window));
        GdkPixbuf *old = NULL;
        
        old = app->wnck_icon;
        app->wnck_icon = gdk_pixbuf_copy(wnck_window_get_icon(window));
        app->current_icon = app->wnck_icon;
        gtk_image_set_from_pixbuf(GTK_IMAGE(app->image), app->current_icon);
        gdk_pixbuf_unref(old);
}

static gboolean
urgent_effect (AwnApp *app)
{
        if (app == NULL )
                return FALSE;
	if (app->current_state == AWN_APP_STATE_CLOSING)
		return FALSE;
	if (app->effect_lock) {
		if ( app->current_effect != AWN_APP_EFFECT_NEEDS_ATTENTION) {
			return TRUE;
		}
	} else {
		app->effect_lock = TRUE;
		app->current_effect = AWN_APP_EFFECT_NEEDS_ATTENTION;
		app->current_state = AWN_APP_STATE_NEEDS_ATTENTION;
		app->current_step = 0;
		app->current_scale = 1.0;
		app->effect_direction = AWN_APP_EFFECT_DIRECTION_UP;
	}
	
	GtkWidget *align = app->alignment;
	gint max = 10;
	
	app->current_step++;
	
	if ( app->effect_direction == AWN_APP_EFFECT_DIRECTION_UP) {
		app->current_scale -= (gfloat)0.5/max;
		gtk_alignment_set(GTK_ALIGNMENT(align), 0.5, app->current_scale, 0, 0);
		if ( app->current_step == max ) {
			app->current_step = 0;
			app->effect_direction = AWN_APP_EFFECT_DIRECTION_DOWN;
		}
	
	} else {
		app->current_scale += (gfloat)0.5/max;
		gtk_alignment_set(GTK_ALIGNMENT(align), 0.5, app->current_scale, 0, 0);
		
		if ( app->current_step == max ) {
			if (wnck_window_or_transient_needs_attention( app->window )) {
				app->current_step = 0;
				app->effect_direction = AWN_APP_EFFECT_DIRECTION_UP;
			} else {
				gtk_alignment_set(GTK_ALIGNMENT(align), 0.5, 1.0, 0, 0);
				app->effect_lock = FALSE;
				return FALSE;
			}
			
			
		}
	}
	
	
	return TRUE;
}

void 
awn_app_set_needs_attention(AwnApp *app, gboolean needs_attention)
{
	if (app->current_state == AWN_APP_STATE_CLOSING)
		return;
        if (app == NULL )
                return;
        if (wnck_window_or_transient_needs_attention( app->window ))
        	g_timeout_add(AWN_FRAME_RATE+20, (GSourceFunc)urgent_effect, (gpointer)app);
        
}

static void
awn_app_create_active_icon(AwnApp *app)
{
	GError *err = NULL;
       	app->active_icon = gdk_pixbuf_new_from_file(settings->active_png, &err);
        	
        	
       	int width = gdk_pixbuf_get_width(app->current_icon);
       	int pad = (int) ( (60 - width ) / 2);
       	
       	gdk_pixbuf_composite (app->current_icon, app->active_icon, 
       				pad, 12 ,48, 48, 
       				pad, 12.0, 1.0, 1.0,
                                       GDK_INTERP_HYPER, 255);
	g_object_ref(G_OBJECT(app->active_icon));
}

void 
awn_app_set_active(AwnApp *app, gboolean active)
{
	if (!app)
		return;
	if (app->current_state == AWN_APP_STATE_CLOSING)
		return;
	
	g_return_if_fail(GTK_IS_IMAGE(app->image));
	
	if (!app->active_icon) 
		awn_app_create_active_icon(app);
	
	
       	if (active)
       		gtk_image_set_from_pixbuf(GTK_IMAGE(app->image), app->active_icon);
       	else 
       		gtk_image_set_from_pixbuf(GTK_IMAGE(app->image), app->current_icon);
       	
}

static gboolean
mouse_over_effect (AwnApp *app)
{
	if (app == NULL)
		return FALSE;
	if (app->current_state == AWN_APP_STATE_CLOSING)
		return FALSE;
	if (app->effect_lock) {
		if ( app->current_effect != AWN_APP_EFFECT_MOUSE_HOVER) 
			return TRUE;
	} else {
		app->effect_lock = TRUE;
		app->current_state = AWN_APP_STATE_MOUSE_HOVER;
		app->current_effect = AWN_APP_EFFECT_MOUSE_HOVER;
		app->current_step = 0;
		app->current_scale = 1.0;
		app->effect_direction = AWN_APP_EFFECT_DIRECTION_UP;
		
	}
	
	GtkWidget *align = app->alignment;
	gint max = 10;
	
	app->current_step++;
	
	if ( app->effect_direction == AWN_APP_EFFECT_DIRECTION_UP) {
		app->current_scale -= (gfloat)0.1/max;
		gtk_alignment_set(GTK_ALIGNMENT(align), 0.5, app->current_scale, 0, 0);
		if ( app->current_step == max ) {
			app->current_step = 0;
			app->effect_direction = AWN_APP_EFFECT_DIRECTION_DOWN;
		}
	
	} else {
		app->current_scale += (gfloat)0.1/max;
		gtk_alignment_set(GTK_ALIGNMENT(align), 0.5, app->current_scale, 0, 0);
		
		if ( app->current_step == max ) {
			if (!app->mouse_in) {
				gtk_alignment_set(GTK_ALIGNMENT(align), 0.5, 1.0, 0, 0);
				app->current_state = AWN_APP_STATE_NORMAL;
				app->effect_lock = FALSE;
				return FALSE;
			}
			
			app->current_step = 0;
			app->effect_direction = AWN_APP_EFFECT_DIRECTION_UP;
		}
	}
	
	
	return TRUE;

}

static gboolean 
on_proximity_in_event (GtkWidget *eb, GdkEventCrossing *event, AwnApp *app)
{
	if (app->current_state == AWN_APP_STATE_CLOSING)
		return TRUE;
	g_return_val_if_fail (WNCK_IS_WINDOW (app->window), TRUE);
	
	if (!app->mouse_in && app->current_state != AWN_APP_STATE_MOUSE_HOVER) {
		app->mouse_in = TRUE;
		g_timeout_add(AWN_FRAME_RATE, (GSourceFunc)mouse_over_effect, (gpointer)app);
	}
	return FALSE;
}

static gboolean 
on_proximity_out_event (GtkWidget *eb, GdkEventCrossing *event, AwnApp *app)
{
	if (app->current_state == AWN_APP_STATE_CLOSING)
		return TRUE;
	if (app->mouse_in) {
		app->mouse_in = FALSE;
		
	}
	return FALSE;
}


static gboolean 
on_drag_motion_event (GtkWidget *widget, GdkDragContext *context, gint x, gint y, guint t, AwnApp *app)
{
        g_return_val_if_fail (WNCK_IS_WINDOW (app->window), FALSE);
	if (app->current_state == AWN_APP_STATE_CLOSING)
		return TRUE;
	if ( wnck_window_is_active( app->window ) ) {
		return FALSE;
	}
	wnck_window_activate( app->window, gtk_get_current_event_time() );
	return FALSE;
}

void
awn_app_close(AwnApp *app)
{
	app->current_state = AWN_APP_STATE_CLOSING;
	app->current_effect = AWN_APP_EFFECT_CLOSING;
	gtk_widget_destroy(app->alignment);
	if (app->active_icon)
		g_object_unref(G_OBJECT(app->active_icon));
	g_free(app);
	app = NULL;
}

