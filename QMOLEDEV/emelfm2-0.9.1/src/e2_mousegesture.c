/* $Id: e2_mousegesture.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2008-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2, which is free software. You can redistribute it
and/or modify it under the terms of the GNU General Public License as published
by the Free Software Foundation - either version 3, or (at your option) any
later version.

emelfm2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/e2_mousegesture.c
@brief functions for handling pointer (mouse) gesture bindings
*/

#include "e2_mousebinding.h"

#ifdef E2_MOUSECUSTOM
#ifdef E2_PTRGESTURES

#include "e2_option_tree.h"
#include <string.h>

//#define EMBEDDED
#include "stroke.h"
//#undef EMBEDDED

/* widget-specific object-data-keys */
#define WATCHES_DATA_KEY "__watches-active__"
#define BUTTONS_DATA_KEY "__watch-buttons-mask__"
#define GESTURE_BINDINGS_KEY "__bound-gesturecats__"

/*this is GDK_MODIFIER_MASK (0x5c001fff) less
  GDK_LOCK_MASK	    = 1 << 1,
  GDK_MOD2_MASK	    = 1 << 4, (numlock)
  GDK_BUTTON1_MASK  = 1 << 8,
  GDK_BUTTON2_MASK  = 1 << 9,
  GDK_BUTTON3_MASK  = 1 << 10,
  GDK_BUTTON4_MASK  = 1 << 11,
  GDK_BUTTON5_MASK  = 1 << 12,
  GDK_RELEASE_MASK  = 1 << 30,
  i.e.
    1 << 0
  | 1 << 2
  | 1 << 3

  | 1 << 5
  | 1 << 6
  | 1 << 7

  | 1 << 26
  | 1 << 27

  | 1 << 28
*/
#define WATCHER_MODMASK 0x1c0000ed

//config data treestore columns (some of them assumed to be same as MCOL_... for buttons store)
enum {
	GCOL_CAT,
	GCOL_BTN,
	GCOL_NAME,
	GCOL_SEQ,
	GCOL_ACT,
	GCOL_ARG
};

/* sizeof (gesture_id) must be a multiple of 4 (32-bits) */
typedef struct _gesture_id
{
	//next 2 must be adjacent for checks
	guint32 button_flags;	//flags for 32 active buttons
	GdkModifierType state;	//flags for mod-keys (GDK_BUTTON1 etc ignored)
	GQuark sequark;			//quarked ascii string, non-static
} gesture_id;

typedef struct _gesture_watch
{
	gesture_id id;
	GtkWidget *from;	//the watched widget
	gpointer device;	//pointer device signature
	Stroke *handle;
	//next 2 must be adjacent for checks against part of a gesture_id
	guint32 transition_button_flags;	//flags for 32 active buttons
	GdkModifierType transition_state;	//flags for mod-keys (GDK_BUTTON1 etc ignored)
	gboolean is_stroke;
	gboolean maybe_stroke;	//transition flag
	//coordinates of previous reported position, for incremental checking during transitions
	gdouble prevx;
	gdouble prevy;
} gesture_watch;

typedef struct _gesture_runtime
{
	gesture_id id;
	GtkWidget *from;//where a pointer is/was being watched (used for widget-specific watches)
	void (*func) (GtkWidget*, gpointer);	//after a recognised stroke, call func (from, data)
	gpointer data;	//callback data
	GDestroyNotify destroyer;  //data cleaner, or NULL
} gesture_runtime;

typedef struct _E2_GestureCatRuntime
{
	GQuark name_quark;	//quark for the category-name (which is structured like level1[.level2.level3 ...]
	GNode *node;		//pointer to treenode for which this struct is data
	guint level;		//level in nodes-tree and bindings-tree, effectively the
						//no. of '.'-separated parts of the category name - 1
	GtkTreeRowReference *ref;	//reference to config data model iter that's the root for this category
//#ifdef E2_IDLE_BTNSYNC
//	gboolean synced;	//TRUE when config treestore data have been converted to runtime format
//#endif
	GPtrArray *gestures;//gesture_runtime's for each gesture in the category
	GSList *instances;	//list of widgets bound to this binding group
} E2_GestureCatRuntime;


static void _e2_mousegesture_run_task (GtkWidget *from,
	gpointer action_string);

//tree of E2_GestureCatRuntime's
static GNode *movebindings = NULL;

/**
@brief Find the first if any gesture_runtime for the widget described in @a gw and that conforms
 to: any device, current device-btns & mod-keys-state & stroke-quark
@param gw pointer to watch data for item to match
@param rt pointer to bindings category data

@return runtime data for matching gesture or NULL if no match
*/
gesture_runtime *_stroke_watcher_get_stroke (gesture_watch *gw, E2_GestureCatRuntime *rt)
{
	if (rt->gestures != NULL)
	{
		guint indx;
		for (indx = 0; indx < rt->gestures->len; indx++)
		{
			gesture_runtime *this;
			this = (gesture_runtime*) g_ptr_array_index (rt->gestures, indx);
			if (memcmp (&this->id, &gw->id, sizeof (gesture_id)) == 0)
				return this;
		}
	}
	return NULL;
}
/**
@brief Find any watch on @a from that conforms to current device + btns + modes
@param watches array of watch-data to check, or NULL
@param from widget that is being watched
@param device pointer device identifier
@param buttonsmask flags for pressed button(s)
@param state masked mmodifier-keys state from current event

@return pointer to matching data or NULL if no match
*/
gesture_watch *_stroke_watcher_find_watch (GPtrArray *watches, GtkWidget *from,
	gpointer device, guint32 buttonsmask, GdkModifierType state)
{
	if (watches != NULL)
	{
		guint this;
		for (this = 0; this < watches->len; this++)
		{
			gesture_watch *gw;
			gw = (gesture_watch *) g_ptr_array_index (watches, this);
			if (gw->from == from
			 && gw->id.button_flags == buttonsmask
			 && gw->id.state == state
			 && gw->device == device
				)
				return gw;
		}
	}
	return NULL;
}
/**
@brief cleanup watches array during widget destruction etc
@param watches array of gesture_watch structs or NULL

@return
*/
static void _stroke_watcher_clear_current_watches (GPtrArray *watches)
{
	if (watches != NULL)
	{
		printd (DEBUG, "clear all gesture_watch's for a widget");
		guint count;
		for (count = 0; count < watches->len; count++)
		{
			gesture_watch *gw = (gesture_watch *) g_ptr_array_index (watches, count);
			if (gw->handle != NULL)
			{
				Stroke *tmp = gw->handle;
				gw->handle = NULL;
				stroke_destroy (tmp);
			}
			DEALLOCATE (gesture_watch, gw)
		}
		g_ptr_array_free (watches, TRUE);
	}
}
/* *
@brief cleanup watches array during widget destruction etc
@param watches array of gesture_watch structs or NULL

@return
*/
/* for stand-alone usage
static void _stroke_watcher_clear_bindings (GPtrArray *bindings)
{
	if (bindings != NULL)
		g_ptr_array_free (bindings, TRUE);
}
*/
/**
@brief "configure-event" signal callback
Take any appropriate action(s) when @a widget changes size
@param widget where the signal was initiated
@param event pointer to event data
@param gw pointer to watch data

@return FALSE always
*/
static gboolean _stroke_watcher_configured_cb (GtkWidget *widget,
	GdkEventConfigure *event, gesture_watch *gw)
{
	/* this is the first callback when dialog is created, & also arrives later
	when re-sized */
	if (gw->handle == NULL)
	{
		printd (DEBUG, "watched widget first configured to %d X %d", event->width, event->height);
		gw->handle = stroke_new (event->width, event->height);
	}
	else
	{
		printd (DEBUG, "watched widget re-configured to %d X %d", event->width, event->height);
		stroke_scale (gw->handle, event->width, event->height);
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
static gboolean _stroke_watcher_pointermove_cb (GtkWidget *widget,
	GdkEventMotion *event, gesture_watch *gw)
{
	if (event->device != gw->device)
		return FALSE; //ignore cb for a different pointer device

//	printd (DEBUG, "_stroke_watcher_pointermove_cb data: %x widget: %x at %.0f, %.0f",
//		gw, widget, event->x, event->y);

	if (gw->maybe_stroke) //data in gw indicates a transitional state
	{
		//check how much pointer movement
		gint diffx, diffy;
		diffx = ABS (event->x - gw->prevx);
		diffy = ABS (event->y - gw->prevy);
		if (diffx > 8 || diffy > 8 || (diffx + diffy) > 15)
		{	//it is moved far enough e.g. drag threshold ? / 2 ?
		// (? and for race-insurance, check btns & mods too ?)
			if (gw->handle == NULL)	//WHERE IS BEST FOR THIS ?
			{
				GtkAllocation alloc;
#ifdef USE_GTK2_18
				gtk_widget_get_allocation (widget, &alloc);
#else
				alloc = widget->allocation;
#endif
				printd (DEBUG, "create new %d X %d stroke handle for widget: %x",
					alloc.width, alloc.height, widget);
				gw->handle = stroke_new (alloc.width, alloc.height);
			}
			else
				//just replace existing watch-data where possible
				stroke_clear (gw->handle);	//clear stroke backend
			//log 2 pts
			stroke_record (gw->handle, (gint)gw->prevx, (gint)gw->prevy);
			stroke_record (gw->handle, (gint)event->x, (gint)event->y);
			//real data = threshold data
			gw->id.button_flags = gw->transition_button_flags;
			gw->id.state = gw->transition_state;
			gw->transition_button_flags = 0;
			gw->transition_state = 0;
			gw->is_stroke = TRUE;
			gw->maybe_stroke = FALSE;
			gw->prevx = 0;
			gw->prevy = 0;
			printd (DEBUG, "_stroke_watcher_pointermove_cb: real drag started");
		}
		else //ignore it ? or log it if (gw->is_event) ?
			printd (DEBUG, "_stroke_watcher_pointermove_cb: drag distance too small");
	}
	else if (gw->is_stroke)
		stroke_record (gw->handle, (gint)event->x, (gint)event->y);

	return FALSE;
}
/**
@brief "key-press-event" and "key-release-event" signals callback
@param widget where the signal was initiated
@param event pointer to event data
@param gw pointer to runtime watch data

@return FALSE = empower further handlers
*/
static gboolean _stroke_watcher_key_event_cb (GtkWidget *widget,
	GdkEventKey *event, gesture_watch *gw)
{
#ifdef USE_GTK2_10
	if (event->is_modifier)
#else
	if (e2_utils_key_is_modifier (event))
#endif
	{
		gint x, y;
		NEEDCLOSEBGL
#ifdef USE_GTK3_0 //CHECKME pointer position
		if (gdk_window_get_device_position (event->window, gw->device, &x, &y, NULL)
			== event->window)
#else
		if (gdk_window_get_pointer (event->window, &x, &y, NULL) == event->window)
#endif
		{
/* CHECKME
			GPtrArray *watches = (GPtrArray *) g_object_get_data (G_OBJECT (widget), WATCHES_DATA_KEY); //must exist here
			guint32 last_buttons = (guint32) g_object_get_data (G_OBJECT (widget), BUTTONS_DATA_KEY);
			if (_stroke_watcher_find_watch (watches, widget, gw->device, last_buttons,
				event->state & WATCHER_MODMASK) != NULL)
			{
*/
				gw->prevx = x;
				gw->prevy = y;
				gw->maybe_stroke = TRUE;
/*			}
			else
			{
			}
*/
		}
		NEEDOPENBGL
	}
	return FALSE;
}
/**
@brief "button-press-event" and "button-release-event" signals callback
@param widget where the signal was initiated
@param event pointer to event data
@param user_data UNUSED pointer specified when cb was connected

@return FALSE to empower further handlers
*/
static gboolean _stroke_watcher_button_event_cb (GtkWidget *widget,
	GdkEventButton *event, gpointer user_data)
{
	gboolean new;
	gboolean retval;
	guint32 bmask, last_buttons;
	GPtrArray *watches;
	gesture_watch *gw;

	printd (DEBUG,"_stroke_watcher_button_event_cb begins: event-type: %d", event->type);
	new = FALSE;
	bmask = 1 << (event->button - 1);
	//double-cast to prevent compiler warning
	last_buttons = (guint32)(gulong)g_object_get_data (G_OBJECT (widget), BUTTONS_DATA_KEY);
	watches = (GPtrArray *) g_object_get_data (G_OBJECT (widget), WATCHES_DATA_KEY);
	if (watches != NULL)
	{
		printd (DEBUG,"current watch on widget");
		//if no current watch on widget that conforms to prior (device + btns + modes) combo
		gw = _stroke_watcher_find_watch (watches, widget, event->device, last_buttons,
				event->state & WATCHER_MODMASK);
		//setup for watching current combo
		if (gw == NULL && event->type == GDK_BUTTON_PRESS)
		{
			gw = ALLOCATE0 (gesture_watch);
			CHECKALLOCATEDWARN (gw, return FALSE;);
			new = TRUE;
			printd (DEBUG,"BUT watch is different so new watch data created");
		}
	}
	else //no watch yet
		if (event->type == GDK_BUTTON_PRESS)
	{
		printd (DEBUG,"_stroke_watcher_button_event_cb, UNWATCHED PRESS");
		watches = g_ptr_array_new ();
		g_object_set_data_full (G_OBJECT (widget), WATCHES_DATA_KEY, watches,
			(GDestroyNotify) _stroke_watcher_clear_current_watches);
		gw = ALLOCATE0 (gesture_watch);
		CHECKALLOCATEDWARN (gw, return FALSE;);
		new = TRUE;
	}
	else
	{
		printd (DEBUG, "_stroke_watcher_button_event_cb, UNWATCHED RELEASE");
		gw = NULL;	//do nothing for new releases
	}

	if (gw == NULL)
	{
		printd (DEBUG,"_stroke_watcher_button_event_cb, nothing to do");
		if (event->type == GDK_BUTTON_PRESS)
			last_buttons |= bmask;
		else
		{
			last_buttons &= ~bmask;
			if (last_buttons == 0)
			{
				g_signal_handlers_disconnect_matched ((gpointer) widget,
					G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_key_event_cb, NULL);
				g_signal_handlers_disconnect_matched ((gpointer) widget,
					G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_pointermove_cb, NULL);
				g_signal_handlers_disconnect_matched ((gpointer) widget,
					G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_configured_cb, NULL);
				//kill any watches-array
				g_object_set_data (G_OBJECT (widget), WATCHES_DATA_KEY, NULL);
			}
		}
		g_object_set_data (G_OBJECT (widget), BUTTONS_DATA_KEY,
				GUINT_TO_POINTER (last_buttons));
//		printd (DEBUG,"_stroke_watcher_button_event_cb returns FALSE");
		return FALSE;
	}

	retval = FALSE;

	if (event->type == GDK_BUTTON_PRESS)
	{
//		printd (DEBUG, "_stroke_watcher_button_event_cb - PRESS");
		//remember and make accessible up to 32 buttons
		last_buttons |= bmask;
		g_object_set_data (G_OBJECT (widget), BUTTONS_DATA_KEY,
			GUINT_TO_POINTER (last_buttons));

		if (new) //no existing watch or confirmed drag yet
		{
			gw->from = widget;
			gw->device = event->device;
			gw->transition_state = event->state & WATCHER_MODMASK;
			gw->transition_button_flags = last_buttons;
			gw->maybe_stroke = TRUE;
			gw->prevx = event->x;
			gw->prevy = event->y;
			g_ptr_array_add (watches, gw);

			//probably not needed, but insurance ?
//			gtk_widget_add_events (widget, GDK_POINTER_MOTION_MASK);

			//duplicated callbacks are a potential problem
			g_signal_handlers_disconnect_matched ((gpointer) widget,
				G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_key_event_cb, NULL);
			g_signal_handlers_disconnect_matched ((gpointer) widget,
				G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_pointermove_cb, NULL);
			g_signal_handlers_disconnect_matched ((gpointer) widget,
				G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_configured_cb, NULL);

			g_signal_connect (G_OBJECT (widget), "key-press-event",
				G_CALLBACK (_stroke_watcher_key_event_cb), gw);
			g_signal_connect (G_OBJECT (widget), "key-release-event",
				G_CALLBACK (_stroke_watcher_key_event_cb), gw);
			g_signal_connect (G_OBJECT (widget), "motion-notify-event",
				G_CALLBACK (_stroke_watcher_pointermove_cb), gw);
			g_signal_connect (G_OBJECT (widget), "configure-event",
				G_CALLBACK (_stroke_watcher_configured_cb), gw);
		}
		else	//a watch is in progress but this new button may invalidate it
		{
			GPtrArray *boundto = (GPtrArray *)
				g_object_get_data (G_OBJECT (widget), GESTURE_BINDINGS_KEY);
			guint indx;
			for (indx = 0; indx < boundto->len; indx++)
			{
				GNode *node = ((E2_GestureCatRuntime *) g_ptr_array_index (boundto, indx))->node;
				while (1)
				{
					if (_stroke_watcher_get_stroke (gw, (E2_GestureCatRuntime *)node->data) != NULL)
					{	//new button is start of a potential or actual new drag
						if (gw->transition_button_flags == 0)
							gw->transition_button_flags = gw->id.button_flags;
						gw->transition_button_flags |= bmask;
						gw->maybe_stroke = TRUE;
						gw->prevx = event->x;
						gw->prevy = event->y;
						break;
					}
					else if (G_NODE_IS_ROOT (node))
						break;
					else
						node = node->parent;
				}
				if (gw->maybe_stroke)
					break;
			}
//			if (!gw->maybe_stroke) //no matching alternative gesture
//			{
				//ignore ? OR abort existing stroke ? CHECKME
//			}
		}
	}
	else if (event->type == GDK_BUTTON_RELEASE)
	{
//		printd (DEBUG, "_stroke_watcher_button_event_cb - RELEASE");
		//remember and make accessible up to 32 buttons
		last_buttons &= ~bmask;
		g_object_set_data (G_OBJECT (widget), BUTTONS_DATA_KEY,
			GUINT_TO_POINTER (last_buttons));

		if (gw->transition_button_flags == 0)
			gw->transition_button_flags = gw->id.button_flags;
		gw->transition_button_flags &= ~bmask;

		if (gw->transition_button_flags == 0) //all buttons gone now
		{
			printd (DEBUG, "btns all gone for widget: %x", widget);
			//clear redundant things before any complications arising from a match and therefore callback task
			//can't be any more release cb's to clear more, so zap it all now
/*
#ifdef USE_GTK2_20
			if (!gtk_widget_get_realized(widget)) //FIXME
#else
			if (!GTK_WIDGET_REALIZED (widget)) //FIXME
#endif
			{
				gint mask = gtk_widget_get_events (widget);
				mask &= ~GDK_POINTER_MOTION_MASK;
				gtk_widget_set_events (widget, mask);
			}
*/
			g_signal_handlers_disconnect_matched ((gpointer) widget,
				G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_key_event_cb, NULL);
			g_signal_handlers_disconnect_matched ((gpointer) widget,
				G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_pointermove_cb, NULL);
			g_signal_handlers_disconnect_matched ((gpointer) widget,
				G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_configured_cb, NULL);

			if (gw->handle != NULL)
			{
				gchar *sequence;
				if (stroke_translate (gw->handle, TRUE, TRUE, &sequence))
				{
					gw->id.sequark = g_quark_try_string (sequence);
					if (gw->id.sequark > 0)	//no quark must mean no registration so no match
					{
//==============
						GPtrArray *boundto = (GPtrArray *)
							g_object_get_data (G_OBJECT (widget), GESTURE_BINDINGS_KEY);
						GNode *topnode = NULL;
						guint indx;
						for (indx = 0; indx < boundto->len; indx++)
						{
							GNode *node = ((E2_GestureCatRuntime *) g_ptr_array_index (boundto, indx))->node;
							while (1)
							{
								GPtrArray *gestures = ((E2_GestureCatRuntime *)node->data)->gestures;
								guint member;
								for (member = 0; member < gestures->len; member++)
								{
//=============
									gesture_runtime *grt = _stroke_watcher_get_stroke (
										gw, (E2_GestureCatRuntime *)node->data);
									if (grt != NULL)
									{
										(*grt->func) (widget, grt->data);
										retval = TRUE;
										break;
									}
//=============
								}
								if (member < gestures->len || G_NODE_IS_ROOT (node))
									break;
								else
								{
									node = node->parent;
									if (node == topnode)
										break;
								}
							}
						}
//============
					}
				}
			}
			g_object_set_data (G_OBJECT (widget), WATCHES_DATA_KEY, NULL);	//also kills the array
		}
		else //if still some btn(s) down
		{
			printd (DEBUG, "_stroke_watcher_button_event_cb - more btn(s)");
			//the new context is potentially a different stroke ...
// ?		if (_stroke_watcher_find_watch (watches, widget, event->device, last_buttons,
//			event->state & WATCHER_MODMASK) != NULL)
//			{
				gw->prevx = event->x;
				gw->prevy = event->y;
				gw->maybe_stroke = TRUE;
//			}
//			else
//			{
//			}
		}
	}
//	printd (DEBUG,"_stroke_watcher_button_event_cb returns %s", (retval) ? "TRUE":"FALSE");
	return retval;
}
/**
@brief cleanup bound-gestures data in array @a gestures

@param gestures pointer to an array of gesture_runtime's, or NULL

@return
*/
static void _e2_mousegesture_free_gestures (GPtrArray *gestures)
{
	if (gestures != NULL)
	{
		gint indx;
		for  (indx = 0; indx < gestures->len; indx++)
		{
			 //FIXME faster to use incrementing pointer directly
			gesture_runtime *rt = (gesture_runtime *) g_ptr_array_index (gestures, indx);
			if (rt->destroyer != NULL)
				(*rt->destroyer) (rt->data);
			//g_free (rt->data);
			DEALLOCATE (gesture_runtime, rt);
		}
		g_ptr_array_set_size (gestures, 0);
	}
}
/**
@brief cleanup data for the bound gestures for the category associated with @a rt

@param rt pointer to device gestures category data struct

@return
*/
static void _e2_mousegesture_free_category (E2_GestureCatRuntime *rt)
{
	if (rt->gestures != NULL && rt->gestures->len > 0)
	{
		_e2_mousegesture_free_gestures (rt->gestures);
		g_ptr_array_free (rt->gestures, TRUE);
		rt->gestures = NULL;
	}
}
/**
@brief recursively translate a gestures treestore branch into an arrayed runtme data struct
This is for "core" widgets with a standard E2_BtnWidget value.
Stored data are validated before listing
@param model model for the bindings treestore
@param iter pointer to iter to be used to interrogate @a model
@param gestures pointer to initialised array to which the created struct(s) will be appended

@return
*/
static void _e2_mousegesture_sync_one (GtkTreeModel *model,
	GtkTreeIter *iter, GPtrArray *gestures)
{
	gchar *cat, *button, *sequence, *action, *action_data;
	guint32 buttonsmask = 0;
	GdkModifierType state = 0;
	gboolean valid;
	gesture_runtime *g;

	gtk_tree_model_get (model, iter,
		GCOL_CAT, &cat, GCOL_BTN, &button, GCOL_SEQ, &sequence,
		GCOL_ACT, &action, GCOL_ARG, &action_data, -1);

	valid = (*cat == '\0'//for a gesture row, the category should be empty
			 && *button != '\0'
			 && *sequence != '\0'
			 && *action != '\0');	//data may be empty, but not this

	if (valid)
	{
		valid = e2_mousebinding_parse_name (button, &buttonsmask, &state, NULL, TRUE);
	}
	if (valid)
	{
		E2_Action *act = e2_action_check (action);
		valid = (act == NULL || !(act->exclude & E2_ACTION_EXCLUDE_POINTER));
	}
	if (valid)
	{
		g = ALLOCATE0 (gesture_runtime);
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (g, valid = FALSE;)
#else
		if (g == NULL)
			valid = FALSE;
#endif
	}
	else
		g = NULL; //warning prevention

	if (valid)
	{
		g->id.sequark = g_quark_from_string (sequence);
		g->id.button_flags = buttonsmask;
		g->id.state = state & WATCHER_MODMASK;
		g->func = _e2_mousegesture_run_task;
		g->data = (*action_data == '\0') ?
			g_strdup (action) :
			g_strconcat (action, " ", action_data, NULL);
		g->destroyer = g_free;

		g_ptr_array_add (gestures, g);
	}
	g_free (cat);
	g_free (button);
	g_free (sequence);
	g_free (action);
	g_free (action_data);
}
/**
@brief translate, from primary treestore to runtime list, the data for a "core"
widget gestures category
This can be called at any time, including when there are no bindings for the
category in the treestore. Pre-existing data for the category are cleared,
so this cannot be used to _add_ gestures to the category
@param rt pointer to data struct for the button-binding category to be processed

@return if E2_IDLE_BTNSYNC applies, TRUE if the process completed properly
*/
static
#ifdef E2_IDLE_BTNSYNC
gboolean
#else
void
#endif
 _e2_mousegesture_sync_category (E2_GestureCatRuntime *rt)
{
	if (!gtk_tree_row_reference_valid (rt->ref))
	{
		printd (WARN, "internal gesturebinding error, row reference not valid anymore");
#ifdef E2_IDLE_BTNSYNC
		return FALSE;
#else
		return;
#endif
	}
	GtkTreePath *path = gtk_tree_row_reference_get_path (rt->ref);
	if (path == NULL)
	{
		printd (WARN, "internal gestursbinding error, path is NULL");
#ifdef E2_IDLE_BTNSYNC
		return FALSE;
#else
		return;
#endif
	}

	printd (DEBUG, "%s e2_mousebinding_gestures_sync", g_quark_to_string (rt->name_quark));
	_e2_mousegesture_free_category (rt);	//get rid of any old gesture data for this category
	rt->gestures = g_ptr_array_new ();

	E2_OptionSet *set = e2_option_get ("mousedrags");
	GtkTreeIter iter;
	gtk_tree_path_down (path);	//down to the buttons level
	while (gtk_tree_model_get_iter (set->ex.tree.model, &iter, path))
	{
		//convert current button to runtime form
		_e2_mousegesture_sync_one (set->ex.tree.model, &iter, rt->gestures);
		gtk_tree_path_next (path);
	}
	gtk_tree_path_free (path);
	//CHECKME walk the array and set data->next ptrs ?
#ifdef E2_IDLE_BTNSYNC
//	rt->synced = TRUE;
	return TRUE;
#else
	return;
#endif
}

#ifdef E2_IDLE_BTNSYNC
static gboolean _e2_mousegesture_walk_sync (GNode *node, gpointer data)
{
	_e2_mousegesture_sync_category ((E2_GestureCatRuntime *) node->data);
	return FALSE;
}
/**
@brief convert data for one or all binding category(ies) from treestore to runtime arrays
(if any category remains to be processed)
This func is mainloop idle callback CHECKME why sync that way?
@param data pointer to binding to be processed, or NULL to do whole list

@return FALSE (so the idle is cancelled)
*/
static gboolean _e2_mousegesture_idle_sync_cb (E2_GestureCatRuntime *data)
{
	if (data != NULL)
		//sync a single BtnCatRuntime
		_e2_mousegesture_sync_category (data);
	else
	{
		g_node_traverse (movebindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_mousegesture_walk_sync, NULL);
	}
	return FALSE;	//remove this fn from event sources
}
#endif
/**
@brief find the first if any category whose name-quark matches the one in @a data
@param tree node data
@param data pointer to walker data struct
@return TRUE when a match is found
*/
static gboolean _e2_mousegesture_walk_names (GNode *node, E2_NodeMatch *data)
{
	if (data->qname == ((E2_GestureCatRuntime *) node->data)->name_quark)
	{
		data->match = node;
		return TRUE;
	}
	return FALSE;
}
/**
@brief get a gesture-binding category treenode for @a name
@param name the name of the binding category to search for, non-NULL and non-empty
Any missing ancestor nodes are created and partially-populated
@return the node for @a name, or NULL upon error
*/
static GNode *_e2_mousegesture_get_category (const gchar *name)
{
	const gchar *e;
	guint level;
	GNode *retval = NULL;

	for (e = name, level = 0; e != NULL; e++, level++)
	{
		e = strchr (e, '.');
		gchar *levelname = (e == NULL) ? (gchar *)name : g_strndup (name, e - name);
		GQuark qname = g_quark_from_string (levelname);
		if (levelname != name)
			g_free (levelname);
		E2_NodeMatch data = { qname, NULL };
		if (movebindings != NULL)
			g_node_traverse (movebindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
				(GNodeTraverseFunc) _e2_mousegesture_walk_names, &data);
		if (data.match == NULL)
		{
			//init runtime data
			E2_GestureCatRuntime *rt = ALLOCATE0 (E2_GestureCatRuntime);
			CHECKALLOCATEDWARN (rt, return NULL;)
			rt->name_quark = qname;
			rt->node = g_node_new (rt);
			if (movebindings == NULL)
			{
//				rt->node = g_node_new (rt);
				movebindings = rt->node;
			}
			else
			{
				//pity about doing this scan again ...
				g_node_traverse (movebindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
					(GNodeTraverseFunc) e2_keybinding_walk_toparent, &data);
/*				if (data.match == NULL) can't fail
				{
					DEALLOCATE (E2_GestureCatRuntime, rt);
					return NULL;
				}
*/
//				rt->node = g_node_new (rt);
				g_node_append (data.match, rt->node);
			}
			retval = rt->node; //in case we're finished now
			rt->level = level;
		}
		else
			retval = data.match;
		if (e == NULL)
			break;
	}
	return retval;
}
/**
@brief do something after a mouse gesture has been detected
This is called from inside an event callback, so BGL is on
@param from the button, menu item etc where the gesture occurred
@param action_string (const gchar*) instruction supplied when the gesture was registered

@return
*/
static void _e2_mousegesture_run_task (GtkWidget *from, gpointer action_string)
{
	gchar *command = g_strdup ((const gchar *)action_string);
	gchar *arg = e2_utils_bare_strchr (command, ' ');
	if (arg != NULL)
	{
		*arg = '\0';
		arg = e2_utils_pass_whitespace (++arg);
	}

	E2_Action *action = e2_action_check (command);
	if (action != NULL)
	{
		OPENBGL
		e2_action_run_simple_from (command, arg, from);
		CLOSEBGL
	}
	else
	{
		const gchar *workdir = NULL;
		if (arg != NULL)
		{
			E2_PaneRuntime *rt = e2_pane_get_runtime (from, arg, &workdir);
			if (rt == NULL || workdir == NULL)
				workdir = curr_view->dir;
			else
				workdir = rt->view.dir;
#ifdef E2_VFSTMP
	CHECKME
#endif
		}
		else
			workdir = curr_view->dir;
		e2_command_run_at (command, workdir, E2_COMMAND_RANGE_DEFAULT, from);
	}
	g_free (command);
}
/**
@brief apply gesture-binding category @a category to "core" widget @a widget
If E2_TREEINCREMENT applies and @a category doesn't exist in the bindings treestore,
it (and any missing ancestor(s)) is added to that store.
If @a category hasn't been used before, its data are setup for runtime usage, or
if E2_IDLE_BTNSYNC applies, such setup is arranged (to be done at idle time).
If non-NULL, the function named @a defaults_func must have the same form as
_e2_keybinding_tree_defaults (). But there is no enrolment if @a category is
already in the keybindings treestore (from config file or a prior instance of a
transient item that uses the category)
@param widget the widget to which the binding will be attached, or NULL
@param category name of the binding, structured like level1[.level2.level3 ...]
@param defaults_func pointer to function which creates a tree-option array, or NULL

@return
*/
void e2_mousegesture_enrol (GtkWidget *widget, const gchar *category,
	void (*defaults_func)(E2_OptionSet*))
{
	GNode *member = _e2_mousegesture_get_category (category);
	if (member == NULL)
		return;
	E2_GestureCatRuntime *rt = (E2_GestureCatRuntime *)member->data;
	gboolean first = (rt->ref == NULL);	//first-time usage of category
	if (first)
	{
		//add iter to bindings config treestore if not already present
		E2_OptionSet *set = e2_option_get ("mousedrags");
		GtkTreeIter iter;
#ifdef E2_TRANSIENTBINDINGS
		gboolean old =
#endif
		e2_tree_get_lowest_iter_for_str (set->ex.tree.model, GCOL_CAT, &iter, category);
		GtkTreePath *path = gtk_tree_model_get_path (set->ex.tree.model, &iter);
		rt->ref = gtk_tree_row_reference_new (set->ex.tree.model, path);
		gtk_tree_path_free (path);
#ifdef E2_TRANSIENTBINDINGS
		if (defaults_func != (void(*)(E2_OptionSet*))NULL && !old)
		{
			//run the defaults-install function, which in turn calls
			//e2_option_tree_setup_defaults() which sets up a pointer array at
			//dummy_set.ex.tree.def.tree_strings
			E2_OptionSet dummyset;
			(*defaults_func) (&dummyset);
			gchar **split = dummyset.ex.tree.def.tree_strings;

			//adjust start-index of parsed array
			const gchar *p = strrchr (category, '.');
			p = (p == NULL) ? category : p + sizeof (gchar);
			gint i;
			for (i = 0; split[i] != NULL && strstr (split[i], p) == NULL; i++);
			if (G_UNLIKELY (split[i] == NULL))
				i = 0;
			//parse the data & add it to the bindings store
			e2_option_tree_set_from_array (set->name, &split[i], NULL, &iter);

			g_strfreev (split);
		}
#endif
#ifdef E2_IDLE_BTNSYNC
		g_idle_add ((GSourceFunc) _e2_mousegesture_idle_sync_cb, rt);
		printd (DEBUG, "arranged gestures bindings idle-sync for %s", category);
#else
		//translate the option tree data to rt form, now
		_e2_mousegesture_sync_category (rt);
#endif
	}

	if (G_LIKELY (widget != NULL))
	{
		printd (DEBUG, "register widget %x for gestures category '%s'", widget, category);
		GPtrArray *boundto = (GPtrArray *)
			g_object_get_data (G_OBJECT (widget), GESTURE_BINDINGS_KEY);
		if (boundto == NULL) //this is the first time widget is enrolled
		{
			//the model is: just one cb for all bindings

			g_signal_connect (G_OBJECT (widget), "button-press-event",
				G_CALLBACK (_stroke_watcher_button_event_cb), NULL);
			g_signal_connect (G_OBJECT (widget), "button-release-event",
				G_CALLBACK (_stroke_watcher_button_event_cb), NULL);

			boundto = g_ptr_array_new ();
			g_object_set_data_full (G_OBJECT (widget), GESTURE_BINDINGS_KEY, boundto,
				(GDestroyNotify) e2_keybinding_clear_widget_bindings); //or _stroke_watcher_clear_bindings);
			//widget remembers it's in the category
			g_ptr_array_add (boundto, rt);
			//and vice versa
			rt->instances = g_slist_append (rt->instances, widget);
		}
		else	//already in category(ies)
		{
			//ensure widget is in only 1 category per categories-branch
			gboolean descendant = FALSE;
			guint indx;
			for (indx = 0; indx < boundto->len; indx++)
			{
				E2_GestureCatRuntime *this = (E2_GestureCatRuntime *)g_ptr_array_index (boundto, indx);
				if (g_node_is_ancestor (this->node, rt->node))
				{
					g_ptr_array_remove (boundto, rt);
					this->instances = g_slist_remove_all (this->instances, widget);
					break;
				}
				else if (this->node == rt->node
						|| g_node_is_ancestor (rt->node, this->node))
				{
					descendant = TRUE;
					break;
				}
			}

			if (!descendant)	//no same- or lower-level binding yet for this widget
			{
				//widget remembers it's in the category
				g_ptr_array_add (boundto, rt);
				//and vice versa
				rt->instances = g_slist_append (rt->instances, widget);
			}
		}
	}
}
/**
@brief unregister a widget from a category
@param node node in categories tree
@param data pointer to data for widget to be unregistered
@return FALSE to keep the walk going
*/
static gboolean _e2_mousegesture_walk_unregister (GNode *node, E2_NodeData *data)
{
	E2_GestureCatRuntime *rt = node->data;
	if (data->qname == rt->name_quark || data->qname == 0)
	{
		GSList *widgets;
		for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
		{
			if (data->widget == widgets->data || data->widget == NULL)
			{
				if (GTK_IS_WIDGET (widgets->data))
				{
					//in case widget isn't to be destroyed
					g_signal_handlers_disconnect_matched (widgets->data,
						G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_button_event_cb, NULL);
					printd (DEBUG, "UNregister gesture-binding for widget");
					gpointer boundto = g_object_get_data (G_OBJECT (widgets->data),
							GESTURE_BINDINGS_KEY);
					if (boundto != NULL)
					{
						while (g_ptr_array_remove_fast ((GPtrArray *)boundto, rt)); //probably only one
					}
				}
				else
					printd (DEBUG, "No UNregister gesture-binding for destroyed widget");

				widgets->data = NULL;	//don't remove, list pointer cannot change yet
			}
		}
		rt->instances = g_slist_remove_all (rt->instances, NULL);
	}
	return FALSE;
}
/**
@brief cancel binding of @a category to @a widget
@param widget the widget which is to be unbound, or NULL for all widgets
@param category name of binding to be processed, or NULL for all categories

@return
*/
void e2_mousegesture_disrol (GtkWidget *widget, const gchar *category)
{
	printd (DEBUG, "e2_mousegesture_disrol, category: %s, widget: _",
		(category) ? category : "ALL");
	GQuark qname;
	if (category != NULL)
	{
		qname = g_quark_try_string (category);
		if (qname == 0)
			return;	//nothing will match, so nothing to do
	}
	else
		qname = 0;

	E2_NodeData data = { qname, widget };
	g_node_traverse (movebindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_mousegesture_walk_unregister, &data);
}
/**
@brief block all gesture callbacks for a widget
@param node node in categories tree
@param data pointer to data for widget to be blocked
@return FALSE
*/
static gboolean _e2_mousegesture_walk_block (GNode *node, E2_NodeData *data)
{
	E2_GestureCatRuntime *rt = node->data;
	if (data->qname == rt->name_quark || data->qname == 0)
	{
		GSList *widgets;
		for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
		{
			if (data->widget == widgets->data || data->widget == NULL)
				g_signal_handlers_block_matched (widgets->data,
					G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_button_event_cb, NULL);
		}
	}
	return FALSE;
}
/**
@brief block operation of binding @a category to @a widget
@param widget the widget which is to be blocked, or NULL for all widgets
@param category name of binding to be processed, or NULL for all categories

@return
*/
void e2_mousegesture_block (GtkWidget *widget, const gchar *category)
{
	printd (DEBUG, "e2_mousegesture_block, category: %s, widget: _",
		(category) ? category : "ALL");
	GQuark qname;
	if (category != NULL)
	{
		qname = g_quark_try_string (category);
		if (qname == 0)
			return;	//nothing will match, so nothing to do
	}
	else
		qname = 0;

	E2_NodeData data = { qname, widget };
	g_node_traverse (movebindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_mousegesture_walk_block, &data);
}
/**
@brief unblock all gesture callbacks for a widget
@param node node in categories tree
@param data pointer to data for widget to be unblocked
@return FALSE to keep the walk going
*/
static gboolean _e2_mousegesture_walk_unblock (GNode *node, E2_NodeData *data)
{
	E2_GestureCatRuntime *rt = node->data;
	if (data->qname == rt->name_quark || data->qname == 0)
	{
		GSList *widgets;
		for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
		{
			if (data->widget == widgets->data || data->widget == NULL)
				g_signal_handlers_unblock_matched (widgets->data,
					G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _stroke_watcher_button_event_cb, NULL);
		}
	}
	return FALSE;
}
/**
@brief unblock operation of binding @a category to @a widget
@param widget the widget which is to be unblocked, or NULL for all widgets
@param category name of binding to be processed, or NULL for all categories

@return
*/
void e2_mousegesture_unblock (GtkWidget *widget, const gchar *category)
{
	printd (DEBUG, "e2_mousegesture_unblock, category: %s, widget: _",
		(category) ? category : "ALL");
	GQuark qname;
	if (category != NULL)
	{
		qname = g_quark_try_string (category);
		if (qname == 0)
			return;	//nothing will match, so nothing to do
	}
	else
		qname = 0;

	E2_NodeData data = { qname, widget };
	g_node_traverse (movebindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_mousegesture_walk_unblock, &data);
}
/**
@brief cleanup runtime data for a binding
@param node node in categories tree
@param data pointerised TRUE/FALSE, TRUE to fully kill the node data
@return FALSE to keep the walk going
*/
static gboolean _e2_mousegesture_walk_categories (GNode *node, gpointer data)
{
	E2_GestureCatRuntime *rt = node->data;
	gtk_tree_row_reference_free (rt->ref);
	rt->ref = NULL;

	_e2_mousegesture_free_category (rt);

	if (rt->instances != NULL)
	{
/*		if (data != NULL)
		{
			//if bound widgets are not being destroyed, disconnect signals with data = rt
			GSList *widgets;
			for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
			{
				if (GTK_IS_WIDGET (widgets->data))	//the bound widget may have been destroyed
				{
					g_signal_handlers_disconnect_matched (widgets->data,
						G_SIGNAL_MATCH_FUNC, //| G_SIGNAL_MATCH_DATA,
						0, 0, NULL, _stroke_watcher_button_event_cb, NULL);
				}
			}
		}
*/
		g_slist_free (rt->instances);
		rt->instances = NULL;
	}
	if (data != NULL)
	{
		DEALLOCATE (E2_GestureCatRuntime, rt);
		g_node_destroy (node);
	}
	return FALSE;
}
/**
@brief cleanup some gestures bindings' runtime data
The bindings data list is not affected
Callbacks are checked before re-connection

@return
*/
void e2_mousegesture_clean (void)
{
	g_node_traverse (movebindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
		(GNodeTraverseFunc) _e2_mousegesture_walk_categories, NULL);
}
/* *
@brief cleanup all gestures data
Presently unused
@return
*/
/*void e2_mousegesture_free_all (void)
{
	g_node_traverse (movebindings, G_POST_ORDER, G_TRAVERSE_ALL, -1,
		(GNodeTraverseFunc) _e2_mousegesture_walk_categories, GINT_TO_POINTER (1));
}
*/
/**
@brief install default tree options for gesture bindings
This function is called only if set data is missing from the config file
@param set pointer to set data

@return
*/
static void _e2_mousegesture_tree_defaults (E2_OptionSet *set)
{
	//the button name strings are parsed by gtk, no translation is possible
	//button codes may be 1 ... whatever
	//columns: 0 cat, 1 button, 2 identifier, 3 sequence, 4 action, 5 action_data
	e2_option_tree_setup_defaults (set,
	g_strdup("mousedrags=<"),  //internal name
	g_strconcat(_C(17),"|||||",NULL),  //_("general"
	g_strconcat("\t",_C(23),"|||||",NULL),  //_("main"
	g_strconcat("\t\t",_C(33),"|||||",NULL),  //_("panes"
	g_strconcat("\t\t\t|<Alt>3|^|8,2|",_A(13),".",_A(54),"|",NULL), //"go up
	g_strconcat("\t\t\t|<Alt>3|<-|6,4|",_A(13),".",_A(52),"|",NULL), //"go back
	g_strconcat("\t\t\t|<Alt>3|->|4,6|",_A(13),".",_A(53),"|",NULL), //"go forward
	g_strconcat("\t\t",_C(12),"|||||",NULL),  //_"directory line"
	g_strconcat("\t\t",_C(5),"|||||",NULL),  //_("command line"
	g_strconcat("\t\t",_C(28),"|||||",NULL),  //_("output"
	g_strconcat("\t\t\t|<Alt>3||2,8|",_A(10),".",_A(33),"|1",NULL), //"adjust ratio
	g_strconcat("\t\t\t|<Alt>3|^|8,2|",_A(10),".",_A(72),"|",NULL), //"page up"
	g_strconcat("\t",_C(11),"|||||",NULL),  //_("dialogs"
	g_strdup(">"),
	NULL);
}
/**
@brief initialize gestures tree-option

@return
*/
void e2_mousegesture_options_register (void)
{
	gchar *title = _("pointer gestures");
	//no screen rebuilds needed after any change to these options
	gchar *group_name = g_strconcat(_C(20),".",title,NULL);
	E2_OptionSet *set = e2_option_tree_register ("mousedrags", group_name, title,
		NULL, e2_mousebinding_tree_selection_check_cb, e2_mousebinding_tree_draggable_check_cb,
		E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_BUILDBUTTONS);
	e2_option_tree_add_column (set, _("Category"), E2_OPTION_TREE_TYPE_STR, 0, "",
		E2_OPTION_TREE_COL_NOT_EDITABLE, NULL, NULL);
	e2_option_tree_add_column (set, _("Button"), E2_OPTION_TREE_TYPE_BUTTON, 0, "",
		0, e2_mousebinding_visible_check_cb, NULL);
	e2_option_tree_add_column (set, _("Identifier"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, e2_mousebinding_visible_check_cb, NULL);
	e2_option_tree_add_column (set, _("Sequence"), E2_OPTION_TREE_TYPE_GESTURE, 0, "",
		0, e2_mousebinding_visible_check_cb, NULL);
	e2_option_tree_add_column (set, _("Action"), E2_OPTION_TREE_TYPE_SEL, 0, "",
		0, e2_mousebinding_visible_check_cb,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_LAYOUT));
	e2_option_tree_add_column (set, _("Argument"), E2_OPTION_TREE_TYPE_SEL , 0, "",
		0, e2_mousebinding_visible_check_cb,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_ACCEL //CHECKME EXCLUDE POINTER ?
		| E2_ACTION_EXCLUDE_LAYOUT | E2_ACTION_EXCLUDE_TOGGLE));
	e2_option_tree_create_store (set);

	e2_option_tree_prepare_defaults (set, _e2_mousegesture_tree_defaults);
}

#endif //def E2_PTRGESTURES
#endif //def E2_MOUSECUSTOM
