/* $Id: e2_gesture_dialog.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2008-2013 tooar <tooar@emelfm2.net>

This file is part of emelfm2.
emelfm2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelfm2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/dialogs/e2_gesture_dialog.c
@brief dialog for showing/editing pointer (mouse) gestures
*/

/*
TODO
better initial size hints
*/

#include "e2_mousebinding.h"

#ifdef E2_MOUSECUSTOM
#ifdef E2_PTRGESTURES
#include "e2_dialog.h"
#include "stroke.h"
#include <string.h>
#ifdef USE_GTK3_0
# include <cairo.h>
#endif
#include "e2_icons.h"

typedef struct _E2_GestureDialogRuntime
{
	GtkWidget *dialog;
	GtkWidget *from;	//the drawing-areas widget where the pointer is being tracked, logged when sequence ends
	GtkWidget *entry;
#ifndef USE_GTK3_0
	GdkGC *gc;			//parameters for point/line drawing
#endif
	gchar *sequence;	//malloc()'d grid-sequence string generally from handle, free() not g_free() to clean
	gdouble prevx, prevy;//px coordinates of previous reported position
	gboolean is_drag;	//TRUE while a button is pressed on 'from'
	Stroke *handle;		//for logging moves, getting stroke-strings
} E2_GestureDialogRuntime;

#ifdef USE_GTK3_0
/**
@brief setup @a cr for usage
@param cr cairo context for @a widget
@param widget to be drawn upon

@return
*/
static void _e2_gesture_dialog_cairo_setup (cairo_t *cr, GtkWidget *widget)
{
	GdkRGBA *color;
	GtkStyleContext *context;
	cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND); //enable display of dots ?
	cairo_set_line_width (cr, 1.0); //approx 1 pixel
	cairo_set_line_join (cr, CAIRO_LINE_JOIN_ROUND);
	context = gtk_widget_get_style_context (widget);
	gtk_style_context_get (context, GTK_STATE_NORMAL, GTK_STYLE_PROPERTY_COLOR,
		&color, NULL);
	cairo_set_source_rgb (cr, color->red, color->green, color->blue);
	gdk_rgba_free (color);
}
#endif
/**
@brief "configure-event" signal callback
Take any appropriate action(s) when @a widget changes size
@param widget where the signal was initiated
@param event pointer to event data
@param rt pointer to runtime data

@return FALSE always
*/
static gboolean _e2_gesture_dialog_configured_cb (GtkWidget *widget,
	GdkEventConfigure *event, E2_GestureDialogRuntime *rt)
{
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	/* this is the first callback when dialog is created,
		& also arrives later when re-sized */
	if (rt->handle == NULL)
	{
		printd (DEBUG, "drawing area first configured to %d X %d", event->width, event->height);
		rt->handle = stroke_new (event->width, event->height);
	}
	else
	{
		printd (DEBUG, "drawing area re-configured to %d X %d", event->width, event->height);
		stroke_scale (rt->handle, event->width, event->height);
	}
	return FALSE;
}
/**
@brief "expose-event"/"draw" signal callback
@param widget where the signal was initiated
@param cr pointer to cairo data OR event pointer to event data
@param rt pointer to runtime data

@return FALSE always
*/
static gboolean _e2_gesture_dialog_exposed_cb (GtkWidget *widget,
#ifdef USE_GTK3_0
 cairo_t *cr,
#else
 GdkEventExpose *event,
#endif
 E2_GestureDialogRuntime *rt)
{
	//this arrives third during dialog creation, and also later
	printd (DEBUG, "drawing area exposed");

	NEEDCLOSEBGL
#ifdef USE_GTK2_18
	if (gtk_widget_get_visible (widget))
#else
	if (GTK_WIDGET_VISIBLE (widget))
#endif
	{
		guint indx;
		gpointer points_array;
		GdkPoint *pd;
#ifndef USE_GTK3_0
		if (rt->gc == NULL)
		{
			GtkStyle *style = gtk_widget_get_style (widget);
			rt->gc = style->black_gc;
		}
#endif
		gint npoints = stroke_get_count (rt->handle);

		if (npoints == 0)
		{
			if (rt->sequence != NULL)
			{
				printd (DEBUG, "recontruct stroke from runtime content");
				stroke_fake (rt->handle, rt->sequence);
				npoints = stroke_replay (rt->handle, &points_array);
			}
			else
			{
				const gchar *sequence = gtk_entry_get_text (GTK_ENTRY (rt->entry));
				if (*sequence != '\0')
				{
					printd (DEBUG, "recontruct stroke from entry content");
					stroke_fake (rt->handle, sequence);
					npoints = stroke_replay (rt->handle, &points_array);
				}
			}

			if (npoints > 0)
			{
#ifdef USE_GTK3_0
				_e2_gesture_dialog_cairo_setup (cr, widget);
				for (pd = (GdkPoint *)points_array, indx = 0; indx < npoints; pd++, indx++)
					cairo_line_to (cr, (gdouble)pd->x, (gdouble)pd->y);
				cairo_stroke (cr);
#else
				GtkAllocation alloc;
# ifdef USE_GTK2_18
				gtk_widget_get_allocation (widget, &alloc);
# else
				alloc = widget->allocation;
# endif
				stroke_get_scale (rt->handle, &alloc.x, &alloc.y);

				gfloat xfactor = (gfloat)alloc.width / alloc.x;
				gfloat yfactor = (gfloat)alloc.height / alloc.y;
				if (xfactor == 1. && yfactor == 1.)
				{
					//CHECKME what's happening here?
					gdk_draw_points (GDK_DRAWABLE (event->window), rt->gc,
						(GdkPoint *)points_array, npoints);
				}
				else
				{
					for (pd = (GdkPoint *)points_array, indx = 0; indx < npoints; pd++, indx++)
					{
						pd->x *= xfactor;
						pd->y *= yfactor;
					}
					gdk_draw_lines (GDK_DRAWABLE (event->window), rt->gc,
						(GdkPoint *)points_array, npoints);
				}
#endif
				free (points_array); //not g_free()
				stroke_clear (rt->handle);
			}
		}
		else
		{
			npoints = stroke_replay (rt->handle, &points_array);
			printd (DEBUG, "exposure - redraw existing stroke with %d points", npoints);
#ifdef USE_GTK3_0
			_e2_gesture_dialog_cairo_setup (cr, widget);
			for (pd = (GdkPoint *)points_array, indx = 0; indx < npoints; pd++, indx++)
				cairo_line_to (cr, (gdouble)pd->x, (gdouble)pd->y);
			cairo_stroke (cr);
#else
			gdk_draw_points (GDK_DRAWABLE (event->window), rt->gc,
				(GdkPoint *)points_array, npoints);
#endif
			free (points_array); //not g_free()
		}
	}

	NEEDOPENBGL
	return FALSE;
}
/**
@brief "button-press-event" signal callback
@param widget where the signal was initiated
@param event pointer to event data
@param rt pointer to runtime data

@return FALSE always
*/
static gboolean _e2_gesture_dialog_button_press_cb (GtkWidget *widget,
	GdkEventButton *event, E2_GestureDialogRuntime *rt)
{
	rt->is_drag = TRUE;
	rt->prevx = event->x;
	rt->prevy = event->y;
	return FALSE;
}
/**
@brief "button-release-event" signal callback
cleanup and grab sequence for later report if not cancelled by user
@param widget where the signal was initiated
@param event pointer to event data
@param rt pointer to runtime data

@return
*/
static gboolean _e2_gesture_dialog_button_release_cb (GtkWidget *widget,
	GdkEventButton *event, E2_GestureDialogRuntime *rt)
{
	rt->is_drag = FALSE;

	gchar *ascii;
	gint bins = stroke_translate (rt->handle, TRUE, TRUE, &ascii);
//	printd (DEBUG, "mouse gestures sequence %s", ascii);
	if (bins > 0)
	{
//		printd (DEBUG, "add mouse gestures sequence %s to runtime content", ascii);
		if (rt->sequence != NULL)
			free (rt->sequence); //not g_free, this was allocated by malloc
		rt->sequence = ascii;

		NEEDCLOSEBGL
		GdkWindow *win =
#ifdef USE_GTK2_14
			gtk_widget_get_window (rt->from);
#else
			rt->from->window;
#endif
		GdkRectangle rect;
		gdk_window_get_position (win, &rect.x, &rect.y);
#ifdef USE_GTK3_0
		rect.width = gdk_window_get_width (win);
		rect.height = gdk_window_get_height (win);
#else
		gdk_drawable_get_size (GDK_DRAWABLE (win), &rect.width, &rect.height);
#endif
		gdk_window_invalidate_rect (win, &rect, FALSE);
		gdk_window_process_updates (win, FALSE);

		gtk_entry_set_text (GTK_ENTRY (rt->entry), ascii);
		NEEDOPENBGL
	}

	return FALSE;
}
/**
@brief "motion-notify" signal callback
@param widget where the signal was initiated
@param event pointer to event data
@param rt pointer to runtime data

@return FALSE always
*/
static gboolean _e2_gesture_dialog_moved_cb (GtkWidget *widget,
	GdkEventMotion *event, E2_GestureDialogRuntime *rt)
{
	if (rt->handle == NULL)
		g_return_val_if_reached (FALSE);

	if (rt->is_drag)
	{
		GdkWindow *win;
//		if (rt->handle != NULL) see test above
			stroke_record (rt->handle, (gint)event->x, (gint)event->y);
		NEEDCLOSEBGL
#ifdef USE_GTK3_0
//* FIXME make this work with less overhead. Changes are often just a pixel or two
		GdkRectangle r;
		r.width = (gint)(rt->prevx - event->x); if (r.width < 0) r.width = -r.width;
		r.height = (gint)(rt->prevy - event->y); if (r.height < 0) r.height = -r.height;
		if (r.width <= 2 && r.height <= 2)
			return FALSE;
		r.x = (gint)MIN(rt->prevx, event->x);
		r.y = (gint)MIN(rt->prevy, event->y);
		win = gtk_widget_get_window (widget);
		cairo_t *cr = gdk_cairo_create (win);
		_e2_gesture_dialog_cairo_setup (cr, widget);
		cairo_move_to (cr, rt->prevx, rt->prevy);
		cairo_line_to (cr, event->x, event->y);
		cairo_stroke (cr);
		gdk_window_invalidate_rect (win, &r, FALSE);
		gdk_window_process_updates (win, FALSE);
		cairo_destroy (cr);
#else
		if (rt->gc == NULL)
		{
			GtkStyle *style = gtk_widget_get_style (widget);
			rt->gc = style->black_gc;
		}
		win =
# ifdef USE_GTK2_14
			gtk_widget_get_window (widget); //CHECKME cache this ?
# else
			widget->window;
# endif
		gdk_draw_line (GDK_DRAWABLE (win), rt->gc, (gint)rt->prevx, (gint)rt->prevy,
			(gint)event->x, (gint)event->y);
#endif
		rt->prevx = event->x;
		rt->prevy = event->y;

		NEEDOPENBGL
	}
	return FALSE;
}

/**
@brief setup stroke-input widget

@return the drawing area widget
*/
static GtkWidget *_e2_gesture_dialog_create_canvas (E2_GestureDialogRuntime *rt)
{
	GtkWidget *canvas = gtk_drawing_area_new ();
	//set min size corresponding to smallest count used by strokes
	gtk_widget_set_size_request (canvas, 25, 25);
#ifdef USE_GTK2_18
	gtk_widget_set_can_focus (canvas, TRUE);
#else
	GTK_WIDGET_SET_FLAGS (canvas, GTK_CAN_FOCUS);
#endif
	gtk_widget_add_events (canvas,
		GDK_EXPOSURE_MASK |
		GDK_POINTER_MOTION_MASK	| //or GDK_POINTER_MOTION_HINT_MASK ?
		GDK_BUTTON_PRESS_MASK |
		GDK_BUTTON_RELEASE_MASK |
		GDK_STRUCTURE_MASK
	);

	g_signal_connect (G_OBJECT (canvas), "configure-event",
		G_CALLBACK (_e2_gesture_dialog_configured_cb), rt);
	g_signal_connect (G_OBJECT (canvas),
#ifdef USE_GTK3_0
	"draw",
#else
	"expose-event",
#endif
		G_CALLBACK (_e2_gesture_dialog_exposed_cb), rt);
	g_signal_connect (G_OBJECT (canvas), "motion-notify-event",
		G_CALLBACK (_e2_gesture_dialog_moved_cb), rt);
	g_signal_connect (G_OBJECT (canvas), "button-press-event",
		G_CALLBACK (_e2_gesture_dialog_button_press_cb), rt);
	g_signal_connect (G_OBJECT (canvas), "button-release-event",
		G_CALLBACK (_e2_gesture_dialog_button_release_cb), rt);

	return canvas;
}

/**
@brief One of 2 "response" signal callbacks for the dialog
@param dialog the widget where the response was initiated
@param response enum refecting user's choice
@param rt pointer to dialog runtime data
See also _e2_option_tree_gesture_column_set_cb()
@return
*/
static void _e2_gesture_dialog_response_cb (GtkDialog *dialog, gint response,
	E2_GestureDialogRuntime *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_USER1: //clear
		{
			GdkWindow *win =
#ifdef USE_GTK2_14
				gtk_widget_get_window (rt->from);
#else
				rt->from->window;
#endif
#ifdef USE_GTK3_0
			GdkRectangle rect;
			gdk_window_get_position (win, &rect.x, &rect.y);
			rect.width = gdk_window_get_width (win);
			rect.height = gdk_window_get_height (win);
			cairo_region_t *reg = cairo_region_create_rectangle (&rect);
			gdk_window_begin_paint_region (win, reg);
			cairo_region_destroy (reg);
			gdk_window_end_paint (win);
#else
			gdk_window_clear (win);
#endif
			gtk_entry_set_text (GTK_ENTRY (rt->entry), "");
			if (rt->sequence != NULL)
			{
				free (rt->sequence);
				rt->sequence = NULL;
			}
			stroke_clear (rt->handle);
		}
			break;
		case E2_RESPONSE_APPLY:
		{
			gchar *retval;
			const gchar *readable = gtk_entry_get_text (GTK_ENTRY (rt->entry));
			if (readable == NULL || *readable == '\0')
			{
				retval = g_strdup ("");
			}
			else
			{
				retval = g_strdup (readable);
				g_strstrip (retval);
				//check that string is ok
				gchar *valid = stroke_verify_sequence (retval);
				if (valid == NULL)
				{
					g_free (retval);
					//stay alive by blocking other response
					g_signal_stop_emission_by_name ((gpointer)dialog, "response");
					break;
				}
				if (strcmp (valid, retval))
				{
					g_free (retval);
					//arrange to show the amended stoke
					if (rt->sequence != NULL)
						free (rt->sequence);
					rt->sequence = valid;
					GdkWindow *win =
#ifdef USE_GTK2_14
						gtk_widget_get_window (rt->from);
#else
						rt->from->window;
#endif
					GdkRectangle rect;
					gdk_window_get_position (win, &rect.x, &rect.y);
#ifdef USE_GTK3_0
					rect.width = gdk_window_get_width (win);
					rect.height = gdk_window_get_height (win);
#else
					gdk_drawable_get_size (GDK_DRAWABLE (win), &rect.width, &rect.height);
#endif
					gdk_window_invalidate_rect (win, &rect, FALSE);
//					gdk_window_process_updates (win, FALSE);
					gtk_entry_set_text (GTK_ENTRY (rt->entry), valid);
					g_signal_stop_emission_by_name ((gpointer)dialog, "response"); //stay alive by blocking other response
					break;
				}
				free (valid);
			}
			//send back the results
			g_object_set_data_full (G_OBJECT (dialog), "sequence", retval, g_free);
		}
		default:
			if (rt->sequence != NULL)
				free (rt->sequence);
			if (rt->handle != NULL)
				stroke_destroy (rt->handle);
			DEALLOCATE (E2_GestureDialogRuntime, rt);
			//dialog is destroyed in other response callback
			break;
	}
	NEEDOPENBGL
}
/**
@brief "activate" signal callback for the dialog's entry
@param entry the widget where the response was initiated
@param rt pointer to dialog runtime data

@return
*/
static void _e2_gesture_dialog_activated_cb (GtkEntry *entry, E2_GestureDialogRuntime *rt)
{
//	NEEDOPENBGL
	g_signal_emit_by_name (G_OBJECT (rt->dialog), "response", E2_RESPONSE_APPLY);
//	NEEDCLOSEBGL
}

/**
@brief create gesture-input dialog
Expects BGL closed.
@param parent the main config-dialog widget
@param initial_sequence string with series of comma-separated grid-numbers, may be empty or NULL

@return the dialog widget, or NULL if error occurred
*/
GtkWidget *e2_gesture_dialog_create (GtkWidget *parent,
	const gchar *initial_sequence)
{
	//init runtime object (0's to ensure edit-dialog things are NULL)
	E2_GestureDialogRuntime *rt = ALLOCATE0 (E2_GestureDialogRuntime);
	CHECKALLOCATEDWARN (rt, return NULL;)

	rt->dialog = e2_dialog_create (NULL, NULL, _("gesture"),
		(ResponseFunc)_e2_gesture_dialog_response_cb, rt);

	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt->dialog));
#else
		GTK_DIALOG (rt->dialog)->vbox;
#endif
	rt->from = _e2_gesture_dialog_create_canvas (rt);
	e2_widget_set_safetip (rt->from, _("Draw gesture"));
	gtk_box_pack_start (GTK_BOX (dialog_vbox), rt->from,
		TRUE, TRUE, E2_PADDING_XSMALL);

	if (G_LIKELY (initial_sequence != NULL && *initial_sequence != '\0'))
	{
		gchar *valid = stroke_verify_sequence (initial_sequence);
		if (valid != NULL)
		{
			g_strstrip (valid);
			rt->sequence = valid;
		}
	}

	//don't use e2_widget_add_entry(), that expands and does other useless things
	rt->entry = gtk_entry_new ();
	gtk_box_pack_start (GTK_BOX (dialog_vbox), rt->entry, FALSE, FALSE,
		E2_PADDING_XSMALL);
	if (rt->sequence != NULL)
		gtk_entry_set_text (GTK_ENTRY (rt->entry), rt->sequence);
	e2_widget_set_safetip (rt->entry, _("Comma-separated grid-positions, each 1-9"));
	g_signal_connect (G_OBJECT (rt->entry), "activate",
		G_CALLBACK (_e2_gesture_dialog_activated_cb), rt);

//	gtk_window_set_type_hint (GTK_WINDOW (rt->dialog), GDK_WINDOW_TYPE_HINT_NORMAL);
	GdkGeometry params =
	{
		50, //min_width;
		50, //min_height;
		0, //max_width;
		0, //max_height;
		0, //base_width;
		0, //base_height;
		5, //width_inc;
		5, //height_inc;
		0.9, //min_aspect; CHECKME values ?
		1.0, //max_aspect;
		0  //win_gravity;
	};

	gtk_window_set_geometry_hints (GTK_WINDOW (rt->dialog), rt->from, &params,
		GDK_HINT_MIN_SIZE | GDK_HINT_ASPECT | GDK_HINT_RESIZE_INC);

	//don't want typical treatment of buttons or gestures
	e2_mousebinding_disrol (rt->dialog, NULL);
	e2_mousegesture_disrol (rt->dialog, NULL);
/*CHECKME
	//or of keys
	e2_keybinding_unregister (NULL, rt->dialog);
*/
	e2_dialog_set_negative_response (rt->dialog, E2_RESPONSE_NOTOALL);

	E2_Button local_btn =
	{
		_("Clea_r"), STOCK_NAME_CLEAR, NULL, 0, 0, E2_RESPONSE_USER1
	};

	e2_dialog_show (rt->dialog, parent, 0,
		&local_btn, &E2_BUTTON_CANCEL, &E2_BUTTON_APPLY, NULL);

	return rt->dialog;
}

#endif //def E2_PTRGESTURES
#endif //def E2_MOUSECUSTOM
