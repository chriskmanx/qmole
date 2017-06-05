/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
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

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

 /*
  * Simple composite widget to provide vertical scrolling, based
  * on GtkRange widget code.
  * Modified by the Sylpheed Team and others 2003
  */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include <glib.h>
#include <gtk/gtk.h>
#include "utils.h"
#include "gtkvscrollbutton.h"
#include "gtkutils.h"

#define SCROLL_TIMER_LENGTH  20
#define SCROLL_INITIAL_DELAY 100	/* must hold button this long before ... */
#define SCROLL_LATER_DELAY   20	/* ... it starts repeating at this rate  */
#define SCROLL_DELAY_LENGTH  300


enum {
    ARG_0,
    ARG_ADJUSTMENT
};

static void gtk_vscrollbutton_class_init(GtkVScrollbuttonClass * klass);
static void gtk_vscrollbutton_init(GtkVScrollbutton * vscrollbutton);

GType gtk_vscrollbutton_get_type		(void);
GtkWidget *gtk_vscrollbutton_new		(GtkAdjustment 	  *adjustment);

static gint gtk_vscrollbutton_button_release	(GtkWidget 	  *widget,
					     	 GdkEventButton   *event,
					     	 GtkVScrollbutton *scrollbutton);

static void gtk_vscrollbutton_set_adjustment	(GtkVScrollbutton *scrollbutton,
					     	 GtkAdjustment 	  *adjustment);

static gint gtk_vscrollbutton_button_press	(GtkWidget 	  *widget,
					   	 GdkEventButton   *event,
					   	 GtkVScrollbutton *scrollbutton);

static gint gtk_vscrollbutton_button_release	(GtkWidget 	  *widget,
					     	 GdkEventButton   *event,
					     	 GtkVScrollbutton *scrollbutton);

gint gtk_vscrollbutton_scroll		(GtkVScrollbutton *scrollbutton);

static gboolean gtk_vscrollbutton_timer_1st_time(GtkVScrollbutton *scrollbutton);

static void gtk_vscrollbutton_add_timer		(GtkVScrollbutton *scrollbutton);

static void gtk_vscrollbutton_remove_timer	(GtkVScrollbutton *scrollbutton);

static gboolean gtk_real_vscrollbutton_timer	(GtkVScrollbutton *scrollbutton);

static void gtk_vscrollbutton_set_sensitivity   (GtkAdjustment    *adjustment,
						 GtkVScrollbutton *scrollbutton);

GType gtk_vscrollbutton_get_type(void)
{
    static GType vscrollbutton_type = 0;

    if (!vscrollbutton_type) {
	static const GTypeInfo vscrollbutton_info = {
			sizeof (GtkVScrollbuttonClass),

			(GBaseInitFunc) NULL,
			(GBaseFinalizeFunc) NULL,

			(GClassInitFunc) gtk_vscrollbutton_class_init,
			(GClassFinalizeFunc) NULL,
			NULL,	/* class_data */

			sizeof (GtkVScrollbutton),
			0,	/* n_preallocs */
			(GInstanceInitFunc) gtk_vscrollbutton_init,
	};

	vscrollbutton_type = g_type_register_static (GTK_TYPE_VBOX, "GtkVScrollbutton", &vscrollbutton_info, (GTypeFlags)0);
    }

    return vscrollbutton_type;
}

static void gtk_vscrollbutton_class_init(GtkVScrollbuttonClass *class)
{
}

static GdkCursor *hand_cursor = NULL;

static gboolean vscroll_visi_notify(GtkWidget *widget,
				       GdkEventVisibility *event,
				       gpointer data)
{
	gdk_window_set_cursor(gtk_widget_get_window(widget), hand_cursor);
	return FALSE;
}

static gboolean vscroll_leave_notify(GtkWidget *widget,
				      GdkEventCrossing *event,
				       gpointer data)
{
	gdk_window_set_cursor(gtk_widget_get_window(widget), NULL);
	return FALSE;
}

static gboolean vscroll_enter_notify(GtkWidget *widget,
				      GdkEventCrossing *event,
				       gpointer data)
{
	gdk_window_set_cursor(gtk_widget_get_window(widget), hand_cursor);
	return FALSE;
}


static void gtk_vscrollbutton_init(GtkVScrollbutton *scrollbutton)
{
    GtkWidget *arrow;

    if (!hand_cursor)
	    hand_cursor = gdk_cursor_new(GDK_HAND2);

    scrollbutton->upbutton = gtk_event_box_new();
    scrollbutton->downbutton = gtk_event_box_new();
    arrow = gtk_arrow_new(GTK_ARROW_UP, GTK_SHADOW_NONE);
    gtk_widget_show(arrow);
    gtk_container_add(GTK_CONTAINER(scrollbutton->upbutton), arrow);
    gtk_widget_set_size_request(scrollbutton->upbutton, -1, 16);
    arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_NONE);
    gtk_widget_show(arrow);
    gtk_container_add(GTK_CONTAINER(scrollbutton->downbutton), arrow);
    gtk_widget_set_size_request(scrollbutton->downbutton, -1, 16);
    gtkut_widget_set_can_focus(scrollbutton->upbutton, FALSE);
    gtkut_widget_set_can_focus(scrollbutton->downbutton, FALSE);
    gtk_widget_show(scrollbutton->downbutton);
    gtk_widget_show(scrollbutton->upbutton);

    g_signal_connect(G_OBJECT(scrollbutton->upbutton), "motion-notify-event",
		     G_CALLBACK(vscroll_visi_notify), NULL);
    g_signal_connect(G_OBJECT(scrollbutton->upbutton), "leave-notify-event",
		     G_CALLBACK(vscroll_leave_notify), NULL);
    g_signal_connect(G_OBJECT(scrollbutton->upbutton), "enter-notify-event",
		     G_CALLBACK(vscroll_enter_notify), NULL);
    g_signal_connect(G_OBJECT(scrollbutton->downbutton), "motion-notify-event",
		     G_CALLBACK(vscroll_visi_notify), NULL);
    g_signal_connect(G_OBJECT(scrollbutton->downbutton), "leave-notify-event",
		     G_CALLBACK(vscroll_leave_notify), NULL);
    g_signal_connect(G_OBJECT(scrollbutton->downbutton), "enter-notify-event",
		     G_CALLBACK(vscroll_enter_notify), NULL);

    g_signal_connect(G_OBJECT(scrollbutton->upbutton),
		       "button_press_event",
		       G_CALLBACK(gtk_vscrollbutton_button_press),
		       scrollbutton);
    g_signal_connect(G_OBJECT(scrollbutton->downbutton),
		       "button_press_event",
		       G_CALLBACK(gtk_vscrollbutton_button_press),
		       scrollbutton);
    g_signal_connect(G_OBJECT(scrollbutton->upbutton),
		     "button_release_event",
		     G_CALLBACK
		     (gtk_vscrollbutton_button_release), scrollbutton);
    g_signal_connect(G_OBJECT(scrollbutton->downbutton),
		     "button_release_event",
		     G_CALLBACK
		     (gtk_vscrollbutton_button_release), scrollbutton);
    gtk_box_pack_start(GTK_BOX(&scrollbutton->vbox),
		       scrollbutton->upbutton, TRUE, TRUE, 0);
    gtk_box_pack_end(GTK_BOX(&scrollbutton->vbox),
		     scrollbutton->downbutton, TRUE, TRUE, 0);
    scrollbutton->timer = 0;
}

GtkWidget *gtk_vscrollbutton_new(GtkAdjustment *adjustment)
{
    GtkWidget *vscrollbutton;
    vscrollbutton = g_object_new (gtk_vscrollbutton_get_type(),
			NULL);
    gtk_vscrollbutton_set_adjustment(GTK_VSCROLLBUTTON(vscrollbutton),
				     adjustment);
    g_signal_connect(G_OBJECT(GTK_VSCROLLBUTTON(vscrollbutton)->adjustment),
		       "value_changed",
		       G_CALLBACK
		       (gtk_vscrollbutton_set_sensitivity), vscrollbutton);
    g_signal_connect(G_OBJECT(GTK_VSCROLLBUTTON(vscrollbutton)->adjustment),
		       "changed",
		       G_CALLBACK
		       (gtk_vscrollbutton_set_sensitivity), vscrollbutton);
    return vscrollbutton;
}


void gtk_vscrollbutton_set_adjustment(GtkVScrollbutton *scrollbutton,
				      GtkAdjustment *adjustment)
{
    cm_return_if_fail(scrollbutton != NULL);
    cm_return_if_fail(GTK_IS_VSCROLLBUTTON(scrollbutton));

    if (!adjustment)
	    adjustment =
	    GTK_ADJUSTMENT(gtk_adjustment_new(0.0, 0.0, 0.0, 0.0, 0.0, 0.0));
    else
	cm_return_if_fail(GTK_IS_ADJUSTMENT(adjustment));

    if (scrollbutton->adjustment != adjustment) {
	if (scrollbutton->adjustment) {
	    g_signal_handlers_disconnect_matched(scrollbutton->adjustment,
	    					 G_SIGNAL_MATCH_DATA,
	    					 0, 0, NULL, NULL, 
						 (gpointer) scrollbutton);
	    g_object_unref(G_OBJECT(scrollbutton->adjustment));
	}

	scrollbutton->adjustment = adjustment;
	g_object_ref(G_OBJECT(adjustment));
#if GLIB_CHECK_VERSION(2,10,0)
	g_object_ref_sink (G_OBJECT(adjustment));
#else
	gtk_object_ref (G_OBJECT (adjustment));
	gtk_object_sink (G_OBJECT (adjustment));
#endif
    }
}

static gint gtk_vscrollbutton_button_press(GtkWidget *widget,
					   GdkEventButton *event,
					   GtkVScrollbutton *scrollbutton)
{
    if (!gtk_widget_has_focus(widget))
	gtk_widget_grab_focus(widget);

    if (scrollbutton->button == 0) {
	gtk_grab_add(widget);
	scrollbutton->button = event->button;

	if (widget == scrollbutton->downbutton)
	    scrollbutton->scroll_type = GTK_SCROLL_STEP_FORWARD;
	else
	    scrollbutton->scroll_type = GTK_SCROLL_STEP_BACKWARD;
	gtk_vscrollbutton_scroll(scrollbutton);
	gtk_vscrollbutton_add_timer(scrollbutton);
    }
    return TRUE;
}


static gint gtk_vscrollbutton_button_release(GtkWidget *widget,
					     GdkEventButton *event,
					     GtkVScrollbutton *scrollbutton)
{
    if (!gtk_widget_has_focus(widget))
	gtk_widget_grab_focus(widget);

    if (scrollbutton->button == event->button) {
	gtk_grab_remove(widget);

	scrollbutton->button = 0;
	gtk_vscrollbutton_remove_timer(scrollbutton);
	gtk_vscrollbutton_set_sensitivity(scrollbutton->adjustment, scrollbutton);
    }
    return TRUE;
}

gboolean gtk_vscrollbutton_scroll(GtkVScrollbutton *scrollbutton)
{
    gfloat bound;
    gfloat new_value;
    gfloat page_size;
    gfloat value;
    gboolean return_val;

    cm_return_val_if_fail(scrollbutton != NULL, FALSE);
    cm_return_val_if_fail(GTK_IS_VSCROLLBUTTON(scrollbutton), FALSE);

    new_value = value = gtk_adjustment_get_value(scrollbutton->adjustment);
    return_val = TRUE;

    switch (scrollbutton->scroll_type) {

    case GTK_SCROLL_STEP_BACKWARD:
	new_value = value - gtk_adjustment_get_step_increment(scrollbutton->adjustment);
	bound = gtk_adjustment_get_lower(scrollbutton->adjustment);
	if (new_value <= bound) {
	    new_value = bound;
	    return_val = FALSE;
	    scrollbutton->timer = 0;
	}
	break;

    case GTK_SCROLL_STEP_FORWARD:
	new_value = value + gtk_adjustment_get_step_increment(scrollbutton->adjustment);
	bound = gtk_adjustment_get_upper(scrollbutton->adjustment);
	page_size = gtk_adjustment_get_page_size(scrollbutton->adjustment);
	if (new_value >= (bound - page_size)) {
	    new_value = bound - page_size;
	    return_val = FALSE;
	    scrollbutton->timer = 0;
	}
	break;
    
    default:
	break;
    
    }

	if (new_value != value) {
	gtk_adjustment_set_value(scrollbutton->adjustment, new_value);
	g_signal_emit_by_name(G_OBJECT
				(scrollbutton->adjustment),
				"value_changed");
	gtk_widget_queue_resize(GTK_WIDGET(scrollbutton)); /* ensure resize */
    }

    return return_val;
}

static gboolean
gtk_vscrollbutton_timer_1st_time(GtkVScrollbutton *scrollbutton)
{
    /*
     * If the real timeout function succeeds and the timeout is still set,
     * replace it with a quicker one so successive scrolling goes faster.
     */
    g_object_ref(G_OBJECT(scrollbutton));
    if (scrollbutton->timer) {
	/* We explicitely remove ourselves here in the paranoia
	 * that due to things happening above in the callback
	 * above, we might have been removed, and another added.
	 */
	g_source_remove(scrollbutton->timer);
	scrollbutton->timer = g_timeout_add(SCROLL_LATER_DELAY,
					    (GSourceFunc)
					    gtk_real_vscrollbutton_timer,
					    scrollbutton);
    }
    g_object_unref(G_OBJECT(scrollbutton));
    return FALSE;		/* don't keep calling this function */
}


static void gtk_vscrollbutton_add_timer(GtkVScrollbutton *scrollbutton)
{
    cm_return_if_fail(scrollbutton != NULL);
    cm_return_if_fail(GTK_IS_VSCROLLBUTTON(scrollbutton));

    if (!scrollbutton->timer) {
	scrollbutton->need_timer = TRUE;
	scrollbutton->timer = g_timeout_add(SCROLL_INITIAL_DELAY,
					    (GSourceFunc)
					    gtk_vscrollbutton_timer_1st_time,
					    scrollbutton);
    }
}

static void gtk_vscrollbutton_remove_timer(GtkVScrollbutton *scrollbutton)
{
    cm_return_if_fail(scrollbutton != NULL);
    cm_return_if_fail(GTK_IS_VSCROLLBUTTON(scrollbutton));

    if (scrollbutton->timer) {
	g_source_remove(scrollbutton->timer);
	scrollbutton->timer = 0;
    }
    scrollbutton->need_timer = FALSE;
}

static gboolean gtk_real_vscrollbutton_timer(GtkVScrollbutton *scrollbutton)
{
    gboolean return_val;

    GDK_THREADS_ENTER();

    return_val = TRUE;
    if (!scrollbutton->timer) {
	return_val = FALSE;
	if (scrollbutton->need_timer)
	    scrollbutton->timer =
		g_timeout_add(SCROLL_TIMER_LENGTH, 
			      (GSourceFunc) gtk_real_vscrollbutton_timer,
			      (gpointer) scrollbutton);
	else {
	    GDK_THREADS_LEAVE();
	    return FALSE;
	}
	scrollbutton->need_timer = FALSE;
    }
    GDK_THREADS_LEAVE();
    return_val = gtk_vscrollbutton_scroll(scrollbutton);
    return return_val;
}

static void gtk_vscrollbutton_set_sensitivity   (GtkAdjustment    *adjustment,
						 GtkVScrollbutton *scrollbutton)
{
	gfloat value;
	if (!gtk_widget_get_realized(GTK_WIDGET(scrollbutton))) return;
	if (scrollbutton->button != 0) return; /* not while something is pressed */
	
	value = gtk_adjustment_get_value(adjustment);
	gtk_widget_set_sensitive(scrollbutton->upbutton, 
				 (value > gtk_adjustment_get_lower(adjustment)));
	gtk_widget_set_sensitive(scrollbutton->downbutton, 
				 (value < (gtk_adjustment_get_upper(adjustment) -
                           gtk_adjustment_get_page_size(adjustment))));
}
