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
 *  Notes : This manages the icons on the bar. Opens, closes, hides, and shows 
 *          the icons according to the preferences.
*/

#include "awn-win-manager.h"

#include "config.h"

#include "awn-bar.h"
#include "awn-app.h"
#include "awn-title.h"

static GList *apps		= NULL; /* I know, I know, globals */
static AwnApp *active		= NULL;
static GtkWidget *bar		= NULL;
static GtkWidget *title		= NULL;
static AwnSettings *settings	= NULL;
static AwnApp *last_active 	= NULL;

static void awn_win_mgr_window_opened (WnckScreen *screen, WnckWindow *window, GtkWidget *hbox);
static void awn_win_mgr_window_closed (WnckScreen *screen, WnckWindow *window, GtkWidget *hbox);
static void awn_win_mgr_window_activate (WnckScreen *screen, GtkWidget *hbox);
static void awn_win_mgr_workspace_changed (WnckScreen *screen, GtkWidget *hbox);
static void _refresh (AwnApp *app, WnckWorkspace *space);

static void on_state_changed (WnckWindow *window, WnckWindowState  changed,    
                            WnckWindowState  new, AwnApp *app);
static void on_workspace_changed (WnckWindow *window, AwnApp *app);
static gboolean on_proximity_in_event (GtkWidget *eb, GdkEventCrossing *event, AwnApp *app);
static gboolean on_proximity_out_event (GtkWidget *eb, GdkEventCrossing *event, AwnApp *app);

GtkWidget* 
awn_win_mgr_new (AwnSettings *set)
{
	GtkWidget *hbox = NULL;
	WnckScreen *screen = NULL;
	
	settings = set;
	hbox = gtk_hbox_new(FALSE, 0);
	screen = wnck_screen_get_default ();
	
	/* LIBWNCK SIGNALS */
	g_signal_connect (G_OBJECT(screen), "window_opened",
	                  G_CALLBACK (awn_win_mgr_window_opened), 
	                  (gpointer)hbox);
	
	g_signal_connect (G_OBJECT(screen), "window_closed",
	                  G_CALLBACK(awn_win_mgr_window_closed), 
	                  (gpointer)hbox);
	
	g_signal_connect (G_OBJECT(screen), "active_window_changed",
	                  G_CALLBACK(awn_win_mgr_window_activate), 
	                  (gpointer)hbox);
	
	g_signal_connect (G_OBJECT(screen), "active_workspace_changed",
	                  G_CALLBACK(awn_win_mgr_workspace_changed), 
	                  (gpointer)hbox);
	
	title = awn_title_new(settings);
	awn_title_show(AWN_TITLE (title), " ", 0, 0);
	gtk_widget_show(title);
	
	return hbox;
	
}

static void 
awn_win_mgr_window_opened (WnckScreen *screen, WnckWindow *window, GtkWidget *hbox)
{
	AwnApp *app = NULL;
	WnckWorkspace *space = NULL;
	
	app  = awn_app_new(window, settings);
	gtk_box_pack_start(GTK_BOX(hbox), app->alignment, FALSE, FALSE, 0);
	apps = g_list_append(apps, (gpointer)app);
	
	g_signal_connect (G_OBJECT (app->window), "state_changed",
        		  G_CALLBACK (on_state_changed), (gpointer)app);
        
        g_signal_connect (G_OBJECT (app->window), "workspace_changed",
        		  G_CALLBACK (on_workspace_changed), (gpointer)app);
        		  
	g_signal_connect (G_OBJECT (app->event_box), "enter-notify-event",
        		  G_CALLBACK (on_proximity_in_event), (gpointer)app);
     	
     	g_signal_connect (G_OBJECT (app->event_box), "leave-notify-event",
        		  G_CALLBACK (on_proximity_out_event), (gpointer)app);
	
	space = wnck_screen_get_active_workspace(screen);
	g_list_foreach(apps, _refresh, (gpointer)space);
}

static void
_destroy (AwnApp *app, gpointer xid) 
{
	guint id;
	
	id = GPOINTER_TO_UINT(xid);
	if (app->xid == id) {
		if (last_active)
			if (last_active->xid == id)
				last_active = NULL;
		app->window = NULL;
		gtk_widget_hide(app->alignment);
		apps = g_list_remove(apps, app);
		awn_app_close(app);
	
	}
}

static void 
awn_win_mgr_window_closed (WnckScreen *screen, WnckWindow *window, GtkWidget *hbox)
{
	g_return_if_fail (WNCK_IS_WINDOW (window));
	WnckWorkspace *space = NULL;
	guint xid;
	
	xid = wnck_window_get_xid(window);
	space = wnck_screen_get_active_workspace(screen);
	g_list_foreach(apps, _destroy, GUINT_TO_POINTER(xid) );
	
	
	space = wnck_screen_get_active_workspace(screen);
	g_list_foreach(apps, _refresh, (gpointer)space);
}



static void
_activate (AwnApp *app, gpointer xid) 
{
	if (!app)
		return;
	g_return_if_fail (WNCK_IS_WINDOW (app->window));
	guint id;
	
	id = GPOINTER_TO_UINT(xid);
	if (app->xid == id) {
		if (wnck_window_is_skip_tasklist(app->window))
			return;
		awn_app_set_active(app, TRUE);		
		if (last_active) 
			awn_app_set_active(last_active, FALSE);
		last_active = app;
	
	}
}

static void 
awn_win_mgr_window_activate (WnckScreen *screen, GtkWidget *hbox)
{
	WnckWorkspace *space = NULL;
	guint xid;
	WnckWindow *window;
	
	window = wnck_screen_get_active_window(screen);
	if (!window)
		return;
	g_return_if_fail (WNCK_IS_WINDOW (window));
	
	xid = wnck_window_get_xid(window);
	g_list_foreach(apps, _activate, GUINT_TO_POINTER(xid) );
	
	space = wnck_screen_get_active_workspace(screen);
	g_list_foreach(apps, _refresh, (gpointer)space);
}

static void 
awn_win_mgr_workspace_changed (WnckScreen *screen, GtkWidget *hbox)
{
	WnckWorkspace *space = NULL;
	
	space = wnck_screen_get_active_workspace(screen);
	g_list_foreach(apps, _refresh, (gpointer)space);
}

static void
_refresh (AwnApp *app, WnckWorkspace *space)
{
	g_return_if_fail (WNCK_IS_WINDOW (app->window));
	if (!space)
		return;
	g_return_if_fail (WNCK_IS_WORKSPACE (space));
		
	if ( wnck_window_is_skip_tasklist( app->window ) ){
		gtk_widget_hide(app->alignment);
		return;
	}
	
	if (settings->show_all_windows) {
		gtk_widget_show_all(app->alignment);
		return;
	}
	
	if (wnck_window_is_in_viewport(app->window, space)) {
		gtk_widget_show_all(app->alignment);
	} else {
		gtk_widget_hide(app->alignment);
	}
}

static void 
on_state_changed (WnckWindow *window, WnckWindowState  old, 
                                WnckWindowState  new, AwnApp *app)
{
	g_return_if_fail (WNCK_IS_WINDOW (window));
	
	if ( wnck_window_is_skip_tasklist( window ) ){
		WnckWorkspace *space = NULL;
	        space = wnck_window_get_workspace(window);
	        g_list_foreach(apps, _refresh, (gpointer)space);
	
	} 
	
	if ( wnck_window_or_transient_needs_attention( window ) ) {
		
		awn_app_set_needs_attention (app, TRUE);
	} else {
		
	}
	
        
}

static void 
on_workspace_changed (WnckWindow *window, AwnApp *app)
{
        g_print("Workspace Changed : %s\n", wnck_window_get_name(window));
}

static gboolean 
on_proximity_in_event (GtkWidget *eb, GdkEventCrossing *event, AwnApp *app)
{
	g_return_if_fail (WNCK_IS_WINDOW (app->window));
	
	//g_print("Co-ords : %f , %f\n", event->x_root, event->y_root);
	gint i = (int)event->x_root;
	gint x, y;
	gdk_window_get_position (app->event_box->window, &x, &y);
	
	/* find middle of event_box */
	do
	        x+=60;
	while (x<i)
	        ;
	x -=30;
	
	awn_title_show(AWN_TITLE (title), wnck_window_get_name(WNCK_WINDOW(app->window)), x, 0);
	
	return FALSE;
}

static gboolean 
on_proximity_out_event (GtkWidget *eb, GdkEventCrossing *event, AwnApp *app)
{
	awn_title_show(AWN_TITLE (title), " ", 20, 0);
	
	return FALSE;
}














