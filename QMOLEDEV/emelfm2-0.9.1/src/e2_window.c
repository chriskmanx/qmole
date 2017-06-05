/* $Id: e2_window.c 3060 2014-02-14 23:40:44Z tpgww $

Copyright (C) 2004-2014 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark.

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/e2_window.c
@brief main window functions

main window functions, including actions on panes
*/
/**
\page status the status line

ToDo - description of how this works
*/

#include "emelfm2.h"
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <time.h>
#include "e2_window.h"
#include "e2_option.h"
#include "e2_action.h"
#include "e2_context_menu.h"
#include "e2_toolbar.h"
#include "e2_filelist.h"
#include "e2_task.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif
#include "e2_icons.h"

static gboolean _e2_window_unadvise (gpointer data);

extern GList *cols_data;
extern gint col_width_store[2][MAX_COLUMNS];
extern gint stored_col_order[2][MAX_COLUMNS];
//main-window size before fullscreen enacted
gint real_width;
gint real_height;

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief create (but not show) paned widget to contain the file panes, and surrounding containers

Depending on whether the file panes are horiz or vert, a vert or horiz paned
widget is packed inside two boxes

@param rt ptr to window data struct

@return
*/
static void _e2_window_create_pane_boxes (E2_WindowRuntime *rt)
{
#ifdef USE_GTK3_0
	rt->panes_horizontal = e2_option_bool_get ("panes-horizontal");
	GtkOrientation or = (rt->panes_horizontal) ? GTK_ORIENTATION_HORIZONTAL : GTK_ORIENTATION_VERTICAL;
	rt->panes_outer_box = gtk_box_new (or, 0);
	or = (rt->panes_horizontal) ? GTK_ORIENTATION_VERTICAL : GTK_ORIENTATION_HORIZONTAL;
	rt->panes_inner_box = gtk_box_new (or, 0);
	rt->panes_paned = gtk_paned_new (or);
#else
	if (e2_option_bool_get ("panes-horizontal"))
	{
		rt->panes_horizontal = TRUE;
		rt->panes_outer_box = gtk_hbox_new (FALSE, 0);
		rt->panes_inner_box = gtk_vbox_new (FALSE, 0);
		rt->panes_paned = gtk_vpaned_new ();
	}
	else
	{
		rt->panes_horizontal = FALSE;
		rt->panes_outer_box = gtk_vbox_new (FALSE, 0);
		rt->panes_inner_box = gtk_hbox_new (FALSE, 0);
		rt->panes_paned = gtk_hpaned_new ();
	}
#endif
	gtk_box_pack_start (GTK_BOX (rt->panes_inner_box), rt->panes_paned, TRUE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (rt->panes_outer_box), rt->panes_inner_box, TRUE, TRUE, 0);
}
/**
@brief get ratio value for position of paned widget divider

@param paned the widget for which the ratio is to be calculated

@return ratio: paned current position /  _paned->max_position
*/
static gdouble _e2_window_get_pos (GtkWidget *paned)
{
#ifdef USE_GTK3_0
	gint pos, maxpos;
	g_object_get (G_OBJECT (paned), "position", &pos, "max-position", &maxpos, NULL);
	if (G_LIKELY (maxpos > 0))
		return ((gdouble) pos / maxpos);
#else
	GtkPaned *_paned = GTK_PANED (paned);
	gint pos = gtk_paned_get_position (_paned);
	if (G_LIKELY (_paned->max_position > 0))
		return ((gdouble) pos / _paned->max_position);
#endif
	else
		return 1.0;
}
/**
@brief change position of the divider for @a paned between filelists or lists and output

@a arg must be like one of: "X","*","X,Y","X,*" "*,X" or "*,*" where X and Y are
string representations of values in range [0.0..1.0] or may be just "0" or "1",
* represents any string containing that char
The first or only value sets the new position, a second value (if [0.01..0.99])
is logged as a 'previous' ratio
X,Y values < 0.01 are rounded down, values > 0.99 are rounded up
Expects BGL to be on/closed on arrival here
@param paned ptr to paned widget to be adjusted
@param pos pointer to store for paned position value
@param ratio_last pointer to store for the prior value of the 'paned ratio' (position / max. position)
@param arg action argument string

@return FALSE upon numerical conversion error or value outside of [0..1]
*/
static gboolean _e2_window_adjust_panes_ratio (GtkPaned *paned,
	gdouble *ratio_last, const gchar *arg)
{
	gfloat res, ratio_new, ratio_log;
	gchar *p1, *p2, *tail;
	gboolean retval, adjust_output;

	if (arg == NULL || *arg == '\0')
		return FALSE;

	retval = TRUE;
	adjust_output = (paned == GTK_PANED (app.window.output_paned));

	p1 = e2_utils_unquote_string (arg); //quotes may be there to protect '*'
	p2 = strchr (p1,','); //is there a pair of args ?
	if (p2 != NULL)
	{
		*p2 = '\0';
		p2++;
	}
	else
		p2 = p1;	//only 1
	if (strchr (p1, '*') != NULL)
		ratio_new = -1.0;
	else
	{
		g_strstrip (p1);
#ifdef __USE_GNU
		res = strtof (p1, &tail);
#else
		res = (gfloat) strtod (p1, &tail);
#endif
		if (*tail != '\0' //conversion error
			|| res < -0.0001 || res > 1.0001) //range error
		{
			retval = FALSE;
			ratio_new = 0.0;	//warning prevention
		}
		else
		{
			if (adjust_output) res = 1.0 - res;
			if (res < 0.01) res = 0.0;
			else if (res > 0.99) res = 1.0;
			ratio_new = res;
		}
	}
	if (p2 != p1)
	{
		if (strchr (p2, '*') != NULL)
			ratio_log = -1.0;
		else
		{
			g_strstrip (p2);
#ifdef __USE_GNU
			res = strtof (p2, &tail);
#else
			res = (gfloat) strtod (p2, &tail);
#endif
			if (*tail != '\0' //conversion error
			   || res < -0.0001 || res > 1.0001) //range error
			{
				retval = FALSE;
				ratio_log = 0.0;	//warning prevention
			}
			else
			{
				if (adjust_output) res = 1.0 - res;
				if (res < 0.01) res = 0.0;
				else if (res > 0.99) res = 1.0;
				ratio_log = res;
			}
		}
	}
	else
		ratio_log = ratio_new;

	g_free (p1);
	if (!retval)
		return retval;

	printd (DEBUG, "_e2_window_adjust_panes_ratio (paned:,ratio_last:%f3.2,ratio_new:%f3.2,ratio_log:%f3.2)",
		*ratio_last, ratio_new, ratio_log);

#ifdef USE_GTK3_0
	gint pos, maxpos;
	g_object_get (G_OBJECT (paned), "position", &pos, "max-position", &maxpos, NULL);
#endif
	if (p1 != p2 && (ratio_new < -0.0001 || ratio_log < -0.0001))
	{	// '*' in either or both parts of 2-part arg
		if (ratio_new >= -0.0001)
		{
#ifdef USE_GTK3_0
			res = (gfloat)pos/maxpos;
#else
			res = (gfloat)gtk_paned_get_position(paned)/paned->max_position;
#endif
			ratio_log = (gfloat)*ratio_last;
			if (ratio_log < (res - 0.0001) || ratio_log > (res + 0.0001)) //floats !=
				ratio_new = ratio_log;
		}
		else if (ratio_log >= -0.0001)
		{
#ifdef USE_GTK3_0
			res = (gfloat)pos/maxpos;
#else
			res = (gfloat)gtk_paned_get_position(paned)/paned->max_position;
#endif
			if (ratio_log < (res - 0.0001) || ratio_log > (res + 0.0001)) //floats !=
				ratio_new = ratio_log;
			ratio_log = (gfloat)*ratio_last;
		}
		else // '*' in both parts
		{
#ifdef USE_GTK3_0
			ratio_new = ratio_log = maxpos * (gfloat)*ratio_last;
#else
			ratio_new = ratio_log = paned->max_position * (gfloat)*ratio_last;
#endif
		}
	}

	if (ratio_new < -0.0001)
		ratio_new = *ratio_last;
#ifdef USE_GTK3_0
	gtk_paned_set_position (paned, (gint)(maxpos * ratio_new));
#else
	gtk_paned_set_position (paned, paned->max_position * ratio_new);
#endif
	//typically arg has 1 part, and ratio_log == ratio_new
	if (ratio_log > 0.01 && ratio_log < 0.99)
		*ratio_last = (gdouble)ratio_log;

	if (adjust_output) //adjusting output divider
	{
		if (ratio_new > 0.99 &&
#ifdef USE_GTK2_18
			gtk_widget_has_focus (GTK_WIDGET (curr_tab->text))
#else
			GTK_WIDGET_HAS_FOCUS (curr_tab->text)
#endif
		)
		{
			WAIT_FOR_EVENTS
			gtk_widget_grab_focus (app.pane1.view.treeview); //but this might also be hidden
		}
		else if (ratio_new < 0.01 &&
#ifdef USE_GTK2_18
			(gtk_widget_has_focus (app.pane2.view.treeview) ||
			 gtk_widget_has_focus (app.pane1.view.treeview)))
#else
			(GTK_WIDGET_HAS_FOCUS (app.pane2.view.treeview) ||
			 GTK_WIDGET_HAS_FOCUS (app.pane1.view.treeview)))
#endif
		{
			WAIT_FOR_EVENTS
			gtk_widget_grab_focus (GTK_WIDGET(curr_tab->text));
		}
	}
	else
	{	//do this only for filelist divider changes (may hang for output pane divider)
		if (ratio_new > 0.99 &&
#ifdef USE_GTK2_18
			gtk_widget_has_focus (app.pane2.view.treeview))
#else
			GTK_WIDGET_HAS_FOCUS (app.pane2.view.treeview))
#endif
		{
			WAIT_FOR_EVENTS	//note - expects BGL closed
			gtk_widget_grab_focus (app.pane1.view.treeview);
		}
		else if (ratio_new < 0.01 &&
#ifdef USE_GTK2_18
			gtk_widget_has_focus (app.pane1.view.treeview))
#else
			GTK_WIDGET_HAS_FOCUS (app.pane1.view.treeview))
#endif
		{
			WAIT_FOR_EVENTS
			gtk_widget_grab_focus (app.pane2.view.treeview);
		}
	}

	return TRUE;
}
/**
@brief change position of file-panes separator

This is a wrapper for _e2_window_adjust_panes_ratio ()
Downstream expects BGL closed
@param arg string with new ratio to be set

@return
*/
void e2_window_adjust_pane_ratio (const gchar *arg)
{
	_e2_window_adjust_panes_ratio (GTK_PANED (app.window.panes_paned),
		&app.window.panes_paned_ratio_last, arg);
}
/**
@brief set default window icon list

Set default window icon list from files in configured icon_dir.
That dir is assumed native
Each with name starts with "emelfm2_", and has trailing 'xx.png' where xx are digits

@param base filename base of the icon files

@return
*/
static void _e2_window_set_icon (void)
{
	gchar *icons_dir = e2_icons_get_custom_path (FALSE);
	DIR *d = e2_fs_dir_open (icons_dir);
	if (d == NULL)
	{
		printd (WARN, "could not open window icon directory '%s' (%s)",
			icons_dir, g_strerror (errno));
		g_free (icons_dir);
		gtk_window_set_default_icon_name (BINNAME);
		return;
	}
	GList *list = NULL;
	const gchar *name;	//name is like "emelfm2_24.png"
	gchar *base = BINNAME"_";	//this is the 'prefix' for window icon filenames
	guint len = strlen (base) + 6;	//assuming ascii, +6 allows for xx.png, filters out the 'alternates'
	struct dirent entry;
	struct dirent *entryptr;
	while (TRUE)
	{
		if (e2_fs_dir_read (d, &entry, &entryptr) || entryptr == NULL)
				break;
		name = entry.d_name;
		if (g_str_has_prefix (name, base) && g_str_has_suffix (name, ".png") &&
			strlen (name) == len)
		{
			gchar *file = g_build_filename (icons_dir, name, NULL);
			GError *error = NULL;
			GdkPixbuf *pixbuf = gdk_pixbuf_new_from_file (file, &error);
			if (error != NULL)
			{
				printd (WARN, "could not open image file '%s' (%s)", file, error->message);
				g_free (file);
				g_error_free (error);
				continue;
			}
			g_free (file);
			list = g_list_append (list, pixbuf);
		}
	}
	if (list != NULL)
	{
		gtk_window_set_default_icon_list (list);
		//gtk copies list and ref's its items, so cleanup here
		g_list_foreach (list, (GFunc) g_object_unref, NULL);
		g_list_free (list);
	}
	g_free (icons_dir);
	e2_fs_dir_close (d);
}

  /*********************/
 /***** callbacks *****/
/*********************/
/**
@brief callback for "size-allocate" signal on app.pane1.outer_box
This happens when file panes divider moves (among other instances)
@param widget UNUSED the resized widget, app.pane1.outer_box
@param alloc the allocation for @a widget
@param rt pointer to window data struct

@return
*/
static void _e2_window_pane1box_allocated_cb (GtkWidget *widget,
	GtkAllocation *alloc, E2_WindowRuntime *rt)
{
	static gint prev_size = -1;
	if ((rt->panes_horizontal && alloc->height != prev_size)
	 || (!rt->panes_horizontal && alloc->width != prev_size))
	{	//this is not just some gtk housekeeping
//		printd (DEBUG, "_e2_window_pane1box_allocated_cb (widget:,alloc:x=%d,y=%d,w=%d,h=%d,rt:_)", alloc->x, alloc->y, alloc->width, alloc->height);
		prev_size = (rt->panes_horizontal) ? alloc->height : alloc->width;
		NEEDCLOSEBGL
		rt->panes_paned_ratio = _e2_window_get_pos (rt->panes_paned);
		NEEDOPENBGL
//		printd (DEBUG, "new panes ratio: %f", rt->panes_paned_ratio);
		if (rt->panes_paned_ratio > 0.001 && rt->panes_paned_ratio < 0.999)
			rt->panes_paned_ratio_last = rt->panes_paned_ratio;
	}
}
/**
@brief after a file pane is mapped or unmapped, ensure the correct toggle button is visible in the other pane's toolbar

This fixes the toggle button when the panes divider is dragged

@param widget ptr to scrolled window for the pane that has just been mapped
@param state TRUE for map signal, FALSE for unmap

@return
*/
static void _e2_window_map_pane_cb (GtkWidget *widget, gpointer state)
{
//	printd (DEBUG, "_e2_window_map_pane_cb (widget:,state:%s)", state ? "true" : "false");
	E2_ToggleType num = (widget == app.pane1.pane_sw) ?
		E2_TOGGLE_PANE2FULL : E2_TOGGLE_PANE1FULL;
	NEEDCLOSEBGL
	if (e2_toolbar_toggle_button_get_state (toggles_array [num])
			== GPOINTER_TO_INT (state))
		e2_toolbar_button_toggle (toggles_array [num]);
	NEEDOPENBGL
}
/**
@brief callback for signal emitted when output-pane size is allocated
This is called when output content change varies the size of @a widget, and also
when the output-paned divider is moved, either by the user (drag or action) or
as a result of window resizing
@param widget UNUSED the resized widget, app.outbook
@param alloc the allocation for @a widget
@param rt pointer to window data struct

@return
*/
static void _e2_window_panesbox_allocated_cb (GtkWidget *widget,
	GtkAllocation *alloc, E2_WindowRuntime *rt)
{
	static gint prev_height = -1;
//	printd (DEBUG, "_e2_window_panesbox_allocated_cb (widget:,alloc:x=%d,y=%d,w=%d,h=%d,rt:_)", alloc->x, alloc->y, alloc->width, alloc->height);
	if (alloc->height != prev_height)
	{
		NEEDCLOSEBGL
		if (prev_height > 1 && alloc->height > 1)
		{	//not [un]hiding the pane
			//adjust scroll-positions of all tabs, to make it seem that textview
			//content remains stationary within the displayed window
			GtkAdjustment *vadj;
			vadj = gtk_scrolled_window_get_vadjustment
				(GTK_SCROLLED_WINDOW (app.tab.scroll));
			gdouble val = gtk_adjustment_get_value (vadj);
			gtk_adjustment_set_value (vadj, val + prev_height - alloc->height);
			//TODO other attached tabs at idle time
			GList *node;
			for (node = app.tabslist; node != NULL; node = node->next)
			{
				E2_OutputTabRuntime *tab = (E2_OutputTabRuntime *)node->data;
				if (tab != curr_tab
#ifdef E2_TABS_DETACH
					&& !tab->detached
#endif
				)
				{
					vadj = gtk_scrolled_window_get_vadjustment (
						GTK_SCROLLED_WINDOW (tab->scroll));
					gtk_adjustment_set_value (vadj, val + prev_height - alloc->height);
				}
			}
		}
		prev_height = alloc->height;
		rt->output_paned_ratio = _e2_window_get_pos (rt->output_paned);
//		printd (DEBUG, "new output ratio: %f", rt->output_paned_ratio);
		//remember visibility state
		app.output.visible = (rt->output_paned_ratio != 1.0);
		if (rt->output_paned_ratio > 0.001 && rt->output_paned_ratio < 0.999)
			rt->output_paned_ratio_last = rt->output_paned_ratio;
		//make sure the correct toggle btn is shown
		gboolean truenow =
		e2_toolbar_toggle_button_get_state (toggles_array [E2_TOGGLE_OUTPUTFULL]);
		if ((!truenow && rt->output_paned_ratio < 0.001)
		 ||(truenow && rt->output_paned_ratio >= 0.001))
			e2_toolbar_button_toggle (toggles_array [E2_TOGGLE_OUTPUTFULL]);
		NEEDOPENBGL
	}
}
/**
@brief after the output pane is mapped or unmapped, ensure the correct toggle button is visible in the commandbar

This fixes the toggle button when the panes divider is dragged

@param widget ptr to output pane scrolled window that has just been mapped
@param state TRUE when mapped, FALSE when unmapped

@return
*/
static void _e2_window_map_output_cb (GtkWidget *widget, gpointer state)
{
//	printd (DEBUG, "_e2_window_map_output_cb (widget:,state:%s)", state ? "true" : "false");
	NEEDCLOSEBGL
	//use toolbar runtime and its .option because that string is not translated
	//make sure the toggle for the other pane is not 'split' form
	if (e2_toolbar_toggle_button_get_state (toggles_array [E2_TOGGLE_OUTPUTSHADE])
		== GPOINTER_TO_INT (state))
		e2_toolbar_button_toggle (toggles_array [E2_TOGGLE_OUTPUTSHADE]);
	NEEDOPENBGL
}
/**
@brief show output pane, at its previous size

This is a callback for ouptput pane "focus-in-event",
and also used generally
Runs output adjust ratio action
Downstream expects BGL to be on/closed

@param widget UNUSED the newly-focused entry widget or NULL
@param data UNUSED

@return FALSE to propagate the event to other handlers
*/
gboolean e2_window_output_show (GtkWidget *widget, gpointer data)
{
	if (!app.output.visible)
		_e2_window_adjust_panes_ratio (GTK_PANED (app.window.output_paned),
			&app.window.output_paned_ratio_last, "*");

	return FALSE;
}
/**
@brief hide output pane

This is a callback for command-line "focus-out-event",
and also used generally
Runs output adjust ratio action
Downstream expects BGL to be on/closed

@param widget UNUSED newly-departed widget or NULL
@param event UNUSED pointer to event data struct
@param user_data UNUSED data specified when callback was connected

@return FALSE to propagate the event to other handlers
*/
gboolean e2_window_output_hide (GtkWidget *widget, GdkEventFocus *event,
	gpointer user_data)
{
	if (app.output.visible)
		_e2_window_adjust_panes_ratio (GTK_PANED (app.window.output_paned),
			&app.window.output_paned_ratio_last, "0");

	gtk_widget_grab_focus (curr_view->treeview);
	return FALSE;
}
/**
@brief callback for "window-state-event" signal emitted when the main window changes
@param widget the main-window object
@param event the GdkEventWindowState which triggered this signal
@param user_data UNUSED data specified when the signal handler was connected

@return FALSE always, so event propogates
*/
static gboolean _e2_window_change_cb (GtkWidget *widget, GdkEventWindowState *event,
	gpointer user_data)
{
	//GdkWindowState event->changed_mask;
	//GdkWindowState event->new_window_state
	if (event->changed_mask & GDK_WINDOW_STATE_FULLSCREEN)
	{
		//toggling fullscreen on or off will grab the "unfull" size each time
#ifdef USE_GTK2_18
		GtkAllocation alloc;
		NEEDCLOSEBGL
		gtk_widget_get_allocation (app.main_window, &alloc);
		NEEDOPENBGL
		real_width = alloc.width;
		real_height = alloc.height;
#else
		real_width = app.main_window->allocation.width;
		real_height = app.main_window->allocation.height;
#endif
		printd (DEBUG, "window-change cb main window size: W %d H %d", real_width, real_height);
	}
	app.mainwindow_state = event->new_window_state;
	return FALSE;
}
/**
@brief callback for signal emitted when the main window is first shown

Also called directly.
paned dividers' positions are 'crudely' set when those widgets are created,
but now all things are setup properly, the sizes are fine-tuned, here
Pane resize etc callbacks are then connected, so that we don't get unnecessary
callbacks during session setup
After all is stable, the toolbars space handling arrangements are initialized,
again, to avoid unnecessary callbacks

@param window UNUSED, the window widget being shown
@param rt pointer to window data struct

@return TRUE, always
*/
static void _e2_window_show_cb (GtkWidget *window, E2_WindowRuntime *rt)
{
	printd (DEBUG, "show main window cb");
	NEEDCLOSEBGL
	//initialize window flags
	app.mainwindow_state = gdk_window_get_state (
#ifdef USE_GTK2_14
		gtk_widget_get_window (app.main_window));
#else
		app.main_window->window);
#endif
	//notice future window-state changes
	g_signal_connect (G_OBJECT(app.main_window), "window-state-event",
		G_CALLBACK(_e2_window_change_cb), NULL);

	//ensure specified startup dir is visible, in preference-order
	if (e2_cl_options.force_path)
	{
		if (rt->panes_paned_ratio > 0.9)
			rt->panes_paned_ratio = 0.9;  //show a bit of pane 2
	}
	else if (e2_cl_options.pane1_path != NULL)
	{
		if (rt->panes_paned_ratio < 0.1)
			rt->panes_paned_ratio = 0.1;  //show a bit of pane 1
	}
	else if (e2_cl_options.pane2_path != NULL)
	{
		if (rt->panes_paned_ratio > 0.9)
			rt->panes_paned_ratio = 0.9;
	}

#ifdef USE_GTK3_0
	gint maxpos;
	g_object_get (G_OBJECT (rt->panes_paned), "max-position", &maxpos, NULL);
	gtk_paned_set_position (GTK_PANED (rt->panes_paned),
		(gint)(maxpos * rt->panes_paned_ratio));
	g_object_get (G_OBJECT (rt->output_paned), "max-position", &maxpos, NULL);
	gtk_paned_set_position (GTK_PANED (rt->output_paned),
		(gint)(maxpos * rt->output_paned_ratio));
#else
	gtk_paned_set_position (GTK_PANED (rt->panes_paned),
		GTK_PANED (rt->panes_paned)->max_position * rt->panes_paned_ratio);
	gtk_paned_set_position (GTK_PANED (rt->output_paned),
		GTK_PANED (rt->output_paned)->max_position * rt->output_paned_ratio);
#endif
	//ensure inter-list divider-position is logged when divider is moved
	g_signal_connect (G_OBJECT (app.pane1.outer_box), "size-allocate",
			G_CALLBACK (_e2_window_pane1box_allocated_cb), rt);
	//ensure correct toggle button is displayed when pane divider is dragged
	g_signal_connect (G_OBJECT (app.pane1.pane_sw), "map",
			G_CALLBACK (_e2_window_map_pane_cb), GINT_TO_POINTER (TRUE));
	g_signal_connect (G_OBJECT (app.pane1.pane_sw), "unmap",
			G_CALLBACK (_e2_window_map_pane_cb), GINT_TO_POINTER (FALSE));
	g_signal_connect (G_OBJECT (app.pane2.pane_sw), "map",
			G_CALLBACK (_e2_window_map_pane_cb), GINT_TO_POINTER (TRUE));
	g_signal_connect (G_OBJECT (app.pane2.pane_sw), "unmap",
			G_CALLBACK (_e2_window_map_pane_cb), GINT_TO_POINTER (FALSE));
	//ensure inter-pane divider-position is logged when divider is moved
	//("notify::position" signal doesn't work !)
//	g_signal_connect (G_OBJECT (app.outbook), "size-allocate",
	g_signal_connect (G_OBJECT (rt->panes_outer_box), "size-allocate",
			G_CALLBACK (_e2_window_panesbox_allocated_cb), rt);
	//ensure correct toggle button is displayed when pane divider is dragged
	g_signal_connect (G_OBJECT (app.outbook), "map",
			G_CALLBACK (_e2_window_map_output_cb), GINT_TO_POINTER (TRUE));
	g_signal_connect (G_OBJECT (app.outbook), "unmap",
			G_CALLBACK (_e2_window_map_output_cb), GINT_TO_POINTER (FALSE));

	gtk_notebook_set_current_page (GTK_NOTEBOOK (app.outbook), 0);

	WAIT_FOR_EVENTS; //faster window completion if we hide buttons now, if
					//needed (i.e. don't wait till main loop)
	//now that window is established, we are ready to initialise all toolbars' overflow handling
	e2_toolbar_initialise_space_handler (NULL);
	NEEDOPENBGL
}
/**
@brief callback for main window "destroy-event", "delete-event" signals

@param widget UNUSED the window button which was activated
@param event UNUSED pointer to event data
@param forced pointerised gboolean, NULL if the user is permitted to choose to keep the window open

@return TRUE if the user cancelled the close, or else it does not return at all
*/
static gboolean _e2_window_gone_cb (GtkWidget *widget, GdkEvent *event, gpointer forced)
{
	NEEDCLOSEBGL
	e2_main_closedown (GPOINTER_TO_INT (forced), TRUE, TRUE);
	NEEDOPENBGL
	return TRUE;
}
#ifdef USE_GTK3_0
/**
@brief callback for several windows' "key-press-event" & "key-release-event" signals

@param widget the window widget
@param event pointer to event data
@param user_data pointer specified when callback was connected

@return FALSE always
*/
gboolean e2_window_key_cb (GtkWidget *widget, GdkEventKey *event,
	gpointer user_data)
{
	if (event->is_modifier)
	{
		NEEDCLOSEBGL
		e2_utils_save_state (widget);
		NEEDOPENBGL
	}
	else
		e2_utils_key_translate_cb (widget, event, user_data);
	return FALSE;
}
#endif

  /*******************/
 /***** actions *****/
/*******************/
/**
@brief toggle between horizontal and vertical file panes

@param from UNUSED the button, menu item etc which was activated
@param art UNUSED action runtime data

@return TRUE
*/
static gboolean _e2_window_toggle_panes_direction
	(gpointer from, E2_ActionRuntime *art)
{
	//FIXME Q this
	e2_option_bool_toggle ("panes-horizontal");
	e2_window_recreate (&app.window);
	return TRUE;
}
/**
@brief change the position of the filepanes separator

This is a wrapper for _e2_window_adjust_panes_ratio()
Downstream expects BGL to be on/closed

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the change was done
*/
static gboolean _e2_window_adjust_pane_ratio_action
	(gpointer from, E2_ActionRuntime *art)
{
	return (_e2_window_adjust_panes_ratio (GTK_PANED (app.window.panes_paned),
		&app.window.panes_paned_ratio_last, (const gchar *) art->data));
}
/**
@brief respond to pane 1 full-width toggle button

The visible toggle button is swapped, and the panes-ajust func is called with
either "1" or "*"
Expects BGL closed

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the change was done
*/
static gboolean _e2_window_toggle_full_pane1 (gpointer from, E2_ActionRuntime *art)
{
	const gchar *arg =
	(e2_toolbar_button_toggle (toggles_array [E2_TOGGLE_PANE1FULL])) ?
		"1" : "*";	//no translation
	return (_e2_window_adjust_panes_ratio (GTK_PANED (app.window.panes_paned),
		&app.window.panes_paned_ratio_last, arg));
}
/**
@brief respond to pane2 full-width toggle button

The visible toggle button is swapped, and the panes-ajust func is called with
either "0" or "*"
Expects BGL to be on/closed

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the change was done
*/
static gboolean _e2_window_toggle_full_pane2 (gpointer from, E2_ActionRuntime *art)
{
	const gchar *arg =
	(e2_toolbar_button_toggle (toggles_array [E2_TOGGLE_PANE2FULL])) ?
		"0" : "*";	//no translation
	return (_e2_window_adjust_panes_ratio (GTK_PANED (app.window.panes_paned),
		&app.window.panes_paned_ratio_last, arg));
}
/**
@brief respond to output pane full-window toggle button

The visible toggle button is swapped, and the panes-adjust func is called with
either "1" or "*"
Expects BGL to be on/closed

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the change was done
*/
static gboolean _e2_window_toggle_full_output (gpointer from, E2_ActionRuntime *art)
{
	gboolean next_state =
	!e2_toolbar_button_toggle (toggles_array [E2_TOGGLE_OUTPUTFULL]);

	if (next_state)
		e2_output_mark_end (&app.tab);	//set mark for current bottom position

	gboolean retval =
	_e2_window_adjust_panes_ratio (GTK_PANED (app.window.output_paned),
		&app.window.output_paned_ratio_last, (next_state) ? "*":"1");

	if (retval && next_state)
	{
		//cancelling full-window reverts to "last" split ratio, never to hidden
		//if appropriate, adjust scroll position to show the stuff at the bottom of the former view
		e2_output_scroll_to_end (&app.tab);	//get back to the bottom portion
/*		E2_OutputTabRuntime *trt = &app.tab;
		GtkAdjustment *vadj;
		vadj = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (trt->scroll));
		gdouble value = gtk_adjustment_get_value (vadj);
		if (value >= (vadj->upper - vadj->page_size))
		{	//we're showing the end of the text
			value += vadj->page_increment; //CHECKME find text actually at top of pane ?
			gtk_adjustment_set_value (vadj, value);
		}
*/
		//make sure the other toggle btn is correct
		if (app.window.output_paned_ratio_last < 0.999)
			e2_toolbar_toggle_button_set_state
				(toggles_array [E2_TOGGLE_OUTPUTSHADE], FALSE);
	}
	return retval;
}
/**
@brief respond to output pane visible toggle button

The visible toggle button is swapped, and the panes-ajust fn
is called with either "1" or "*"
Expects BGL to be on/closed

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the change was done
*/
static gboolean _e2_window_toggle_visible_output (gpointer from, E2_ActionRuntime *art)
{
	const gchar *arg =
	(e2_toolbar_button_toggle (toggles_array [E2_TOGGLE_OUTPUTSHADE])) ?
		"0" : "*";	//no translation
	return (_e2_window_adjust_panes_ratio (GTK_PANED (app.window.output_paned),
		&app.window.output_paned_ratio_last, arg));
}
/**
@brief implement adjust-output-ratio action

This is a wrapper for _e2_window_toggle etc
The visible toggle button is swapped, and the panes-ajust fn
is called with "*" ",1" or ",0"
Expects BGL to be on/closed

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if the change was done
*/
static gboolean _e2_window_adjust_output_ratio_action
	(gpointer from, E2_ActionRuntime *art)
{
	const gchar *s, *t, *arg = (const gchar *) art->data;
	//'*' in first or only part of arg ...
	gboolean star = ((s = strchr(arg,'*')) != NULL &&
					((t = strchr(arg,',')) == NULL || t > s));
	gboolean shade = (star && app.window.output_paned_ratio < 0.01);
	if (shade)
		e2_output_mark_end (&app.tab);	//set mark at end-position

	gboolean retval =
	_e2_window_adjust_panes_ratio (GTK_PANED (app.window.output_paned),
		&app.window.output_paned_ratio_last, arg);

	if (retval && shade)
		e2_output_scroll_to_end (&app.tab);	//get back to the bottom portion of the former view

	return retval;
}
/**
@brief window-fullscreen toggle action
This can apply to any application window
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if @a from has atop-level window
*/
static gboolean _e2_window_toggle_full_screen (gpointer from, E2_ActionRuntime *art)
{
	GtkWidget *top = gtk_widget_get_toplevel (from);
#ifdef USE_GTK2_18
	if (gtk_widget_is_toplevel (top))
#else
	if (GTK_WIDGET_TOPLEVEL (top))
#endif
	{
		GdkWindow *win =
#ifdef USE_GTK2_14
			gtk_widget_get_window (top);
#else
			top->window;
#endif
		GdkWindowState state = gdk_window_get_state (win);
		if (state & GDK_WINDOW_STATE_FULLSCREEN)
			gdk_window_unfullscreen (win);
		else
			gdk_window_fullscreen (win);
		return TRUE;
	}
	return FALSE;
}

  /******************/
 /***** public *****/
/******************/

#ifdef E2_COMPOSIT
/**
@brief set or reset translucency of top-level window @a window
Levels < 10 are treated as 0 and > 95 is ignored
Expects BGL on/active
@param window the widget for the window to be processed
@param level opacity %, 50 (faint) to 100 (opaque), or -1 (config default level)

@return
*/
void e2_window_set_opacity (GtkWidget *window, gint level)
{
/*	GdkScreen *screen;
#ifndef USE_GTK2_10
	GdkColormap *colormap;
#endif
*/
	gint deflevel = e2_option_int_get ("window-opacity");	//50=faint...100=opaque
	if (deflevel < 100)	//compositing is active
	{
		if (level < 0)
			level = deflevel;
		//non-top-level windows more opaque than the default setting
		GtkWindowType wtype;
		g_object_get (G_OBJECT (window), "type", &wtype, NULL);
		if (wtype != GTK_WINDOW_TOPLEVEL)
			level = MAX (deflevel + 5, level);
		if (level < 30)	//ignore unusably-invisible levels
			level = 30;
		else if (level > 96)	//ignore nearly-opaque or > 100 levels
			level = 100;

#ifdef USE_GTK3_8
		gtk_widget_set_opacity (window, (gdouble) level / 100.0);
#elif defined(USE_GTK2_12)
		gtk_window_set_opacity (GTK_WINDOW (window), (gdouble) level / 100.0);
#endif

/* FIXME
		//turn off translucence if it was on, or turn on or adjust translucence
#ifdef USE_GTK2_12
		if (gtk_widget_is_composited (window))
#elif defined (USE_GTK2_10)
		screen = gtk_widget_get_screen (window);
		if (gdk_screen_is_composited (screen))
#else
		screen = gtk_widget_get_screen (window);
		colormap = gdk_screen_get_rgba_colormap (screen);
		if (colormap != NULL)	//not entirely sufficient
#endif
			g_signal_connect (G_OBJECT (window), "screen-changed",
				G_CALLBACK (_e2_window_screen_changed_cb), GINT_TO_POINTER (level));
		else
			g_signal_handlers_disconnect_matched (G_OBJECT (window),
				G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_window_screen_changed_cb, NULL);
#ifdef USE_GTK2_12
		screen = gtk_widget_get_screen (window);
#endif
		//FIXME with same screen, this does not change settings!
		//AND NOT CORRECT, even if it did change settings
		NEEDOPENBGL
		_e2_window_screen_changed_cb (window, screen, GINT_TO_POINTER (level));
		NEEDCLOSEBGL
	}
	else	//compositing option not in effect
	{
		//check for tranlucent on now, turn it off if so
#ifdef USE_GTK2_12
		if (!gtk_widget_is_composited (window))
#elif defined (USE_GTK2_10)
		screen = gtk_widget_get_screen (window);
		if (!gdk_screen_is_composited (screen))
#else
		screen = gtk_widget_get_screen (window);
		colormap = gdk_screen_get_rgba_colormap (screen);
		if (colormap == NULL)	//not entirely sufficient
#endif
		{
			//turn off translucence
			g_signal_handlers_disconnect_matched (G_OBJECT (window),
				G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_window_screen_changed_cb, NULL);
#ifdef USE_GTK2_12
			screen = gtk_widget_get_screen (window);
#endif
			NEEDOPENBGL
			_e2_window_screen_changed_cb (window, screen, GINT_TO_POINTER (100));
			NEEDCLOSEBGL
		}
*/
	}
}
#endif
/**
@brief setup custom window title
This probably needs BGL to be closed upon arrival
@param wid dialog or window widget
@param title custom string to append to window titlebar, or NULL
@return
*/
void e2_window_set_title (GtkWidget *wid, const gchar *title)
{
	if (title != NULL)
	{
		gchar *tt = e2_utils_strcat (PROGNAME ": ", title);
		gtk_window_set_title (GTK_WINDOW (wid), tt);	//needs BGL ?
		g_free (tt);
	}
	else
		gtk_window_set_title (GTK_WINDOW (wid), PROGNAME);
}
/**
@brief setup custom window title with appended directgory path
Does nothing if relevant option is not in force.
This probably needs BGL to be closed upon arrival.
@param wid dialog or window widget
@param view pointer to view whose path is to be displayed
@return
*/
void e2_window_set_title_path (GtkWidget *wid, ViewInfo *view)
{
	gint choice = e2_option_sel_get ("title-type");
	if ((choice == 1 && view == &app.pane1.view)
	 || (choice == 2 && view == &app.pane2.view)
	 || (choice == 3 && view == curr_view))
	{
		gchar *tt = e2_utils_strcat (PROGNAME ": ", view->dir);
		if (view->dir[1] != '\0')
		{
			gint len = strlen (tt);
			*(tt + len - 1) = '\0'; //strip trailing / for display
		}
		gtk_window_set_title (GTK_WINDOW (wid), tt);	//needs BGL ?
		g_free (tt);
	}
}
/**
@brief set app window cursor to @a type
Expects BGL on/active
@param type enumerator of desired cursor type

@return
*/
void e2_window_set_cursor (GdkCursorType type)
{
	GdkCursor *cursor = gdk_cursor_new (type);
	gdk_window_set_cursor (
#ifdef USE_GTK2_14
		gtk_widget_get_window (app.main_window),
#else
		app.main_window->window,
#endif
		cursor);
#ifdef USE_GTK3_0
	g_object_unref (G_OBJECT (cursor));
#else
	gdk_cursor_unref (cursor);
#endif
	gdk_flush ();
}
/* *
@brief set output pane size  UNUSED

for internal use, not a toolbar toggle
Downstream expects BGL to be on/closed

@param rt window rt data structure
@param arg string indicating the desired state of the pane e.g "*" or "0"

@return
*/
/* void e2_window_adjust_output_pane_ratio (E2_WindowRuntime *rt, const gchar *arg)
{
	_e2_window_adjust_panes_ratio (GTK_PANED (rt->output_paned),
		&rt->output_paned_ratio_last, arg);
}*/

guint last_selected_rows = -1;	//-1 ensures it always reports at session start
static guint last_total_rows;

/**
@brief update status line message about selected and total items counts

This is called periodically by the status line timer function, when that's
not suspended
Any ".." entry is filtered out from the count
Expects BGL off/open

@param userdata UNUSED

@return TRUE, always (so the timer is never cancelled)
*/
gboolean e2_window_update_status_bar (gpointer userdata)
{
	//CHECKME make this happen after specific events (refresh, refilter, cd, swap panes)
	//not on a timer
#ifdef E2_STATUS_BLOCK
	if (g_atomic_int_get (&app.status_working))
	{
		printd (DEBUG, "update status bar BLOCKED");
		return TRUE;
	}
#endif
//	printd (DEBUG, "e2_window_update_status_bar");
#ifdef E2_STATUS_BLOCK
	g_atomic_int_set (&app.status_working, 1);
#endif
	gint selected_rows, total_rows;
	GtkTreeModel *model = gtk_tree_view_get_model (GTK_TREE_VIEW (curr_view->treeview));
	GtkTreeSelection *sel = curr_view->selection;

	selected_rows = gtk_tree_selection_count_selected_rows (sel);
	total_rows = gtk_tree_model_iter_n_children (model, NULL);
	if ((selected_rows != last_selected_rows) || (total_rows != last_total_rows))
	{
		if (e2_option_bool_get ("show-updir-entry"))
		{	//make status-line counts ignore the updir entry
			//the treeview sorting functions always put any "../" entry at the
			//start of the list, so if such entry is selected, it will be the 1st
			GtkTreeIter iter;
			if (gtk_tree_model_get_iter_first (model, &iter))
			{ //really, there will always be an iter
				total_rows--;
				if (gtk_tree_selection_iter_is_selected (sel, &iter))
					selected_rows--;
			}
		}
		last_selected_rows = selected_rows;
		last_total_rows = total_rows;
		gchar *status_text1 = (curr_view->total_items > total_rows) ?
			g_strdup_printf (_("displayed & %d concealed "), curr_view->total_items-total_rows) :
			"" ;
		gchar *status_text2 = g_strdup_printf (
			_("%s%d selected item(s) of %d %sin %s"), ":::  ",
#ifdef E2_VFSTMP
	//FIXME tip when not mounted local
#else
			selected_rows, total_rows, status_text1, curr_view->dir);
#endif

		//generally remove trailing / (ascii)
#ifdef E2_VFSTMP
	//FIXME tip when not mounted local
#else
		if (*(curr_view->dir + sizeof (gchar)) != '\0')	//dir is a single char when at root dir
#endif
		{
			gint len = strlen (status_text2);
			*(status_text2 + len - sizeof (gchar)) = '\0';
		}
		CLOSEBGL
		gtk_label_set_text (GTK_LABEL (app.status_bar_label2), status_text2);
		OPENBGL
		if (*status_text1 != '\0')
			g_free (status_text1);
		g_free (status_text2);
	}
#ifdef E2_STATUS_BLOCK
	g_atomic_int_set (&app.status_working, 0);
#endif
//	printd (DEBUG, "e2_window_update_status_bar ends");
	return TRUE;  //never turn this off
}
#ifdef E2_STATUS_REF
gint statref_count = 0;
#endif
gint lastinterval = E2_STATUSREPORT_INTERVAL;
#ifndef E2_STATUS_DEMAND
/**
@brief turn on status bar 'selected files' message refreshing

@param interval no of milliseconds between updates, or -1 for default

@return
*/
void e2_window_enable_status_update (gint interval)
{
	if (interval < 0)
		interval = lastinterval;
	else
		lastinterval = interval;
#ifdef E2_STATUS_REF
	statref_count--;
	if (statref_count == 0)
	{
#endif
#ifdef E2_STATUS_BLOCK
		g_atomic_int_set (&app.status_working, 0);
#endif
		app.timers[STATUS_T] = g_timeout_add (interval, e2_window_update_status_bar, NULL);
#ifdef E2_STATUS_REF
	}
	else if (statref_count < 0)
		statref_count = 0;
#endif
}
/**
@brief turn off status bar 'selected files' message refreshing

@return
*/
void e2_window_disable_status_update (void)
{
#ifdef E2_STATUS_REF
	statref_count++;
	if (statref_count == 1)
	{
//		printd (BEBUG, "block update status bar cb");
#endif
		if (app.timers[STATUS_T] > 0)
		{
			g_source_remove (app.timers[STATUS_T]);
			app.timers[STATUS_T] = 0;
#ifdef E2_STATUS_REF
		}
#endif
	}
}
#endif	//ndef E2_STATUS_DEMAND
/* *
@brief cancel display of status bar custom message

@return
*/
//FIXME the logic here is crap - needs to be thread-safe
/* UNUSED
void e2_window_remove_status_message (void)
{
	GList *children = GTK_BOX (app.status_bar_box3)->children;
	gint num = g_list_length (children);
	if (num > 1)
	{	//show the next label in the queue (stupid !!)
		GtkWidget *label = ((GtkBoxChild *) children->next->data)->widget;
		gtk_widget_show_all (label);
	}
	if (num ==1)
		gtk_widget_hide (app.status_bar_box3);
	if (num > 0)
	{
		GtkWidget *label = ((GtkBoxChild *) children->data)->widget;
		gtk_container_remove (GTK_CONTAINER (app.status_bar_box3), label);
	}
} */
#ifdef USE_GTK2_20
static GtkWidget *mover = NULL;
#endif
/**
@brief show statusline message @a labeltext
Any existing message is simply replaced.
Gtk's BGL is expected to be open/off
@param message UTF-8 string, which may include pango markup
#ifdef USE_GTK2_20
@param with_spinner TRUE to append a GtkSpinner to the displayed message
#endif
@return
*/
void e2_window_show_status_message (const gchar *message
#ifdef USE_GTK2_20
	, gboolean with_spinner
#endif
)
{
	printd (DEBUG, "In e2_window_show_status_message()");
	//abort idle(s) which may remove the message being added
	while (g_idle_remove_by_data ((gpointer) _e2_window_unadvise))
		printd (DEBUG, "Killed status idle");
	CLOSEBGL
	//kill the current label FIXME make this a more-friendly Q
	GList *children = gtk_container_get_children (GTK_CONTAINER (app.status_bar_box3));
	if (children != NULL)
	{
		if (GTK_IS_LABEL (children->data))
			gtk_widget_destroy (children->data);
		g_list_free (children);
	}
//	OPENBGL
	GtkWidget *label = gtk_label_new (NULL);
	gchar *public = g_markup_escape_text (message, -1);
	gtk_label_set_markup (GTK_LABEL (label), public);
	g_free (public);
//	CLOSEBGL
	gtk_container_add (GTK_CONTAINER (app.status_bar_box3), label);
#ifdef USE_GTK2_20
	if (with_spinner)
	{
		if (mover == NULL)
		{
			mover = gtk_spinner_new ();
			gtk_box_pack_start (GTK_BOX(app.status_bar_box3), mover, TRUE, TRUE, E2_PADDING);
			gtk_widget_show (mover);
			gtk_spinner_start ((GtkSpinner*) mover);
		}
	}
	else	//no spinner from now
		if (mover != NULL)
	{
		gtk_widget_destroy (mover);
		mover = NULL;
	}
#endif
	gtk_widget_show_all (app.status_bar_box3);
	OPENBGL
}
/**
@brief idle function to clear statusline message
@param data UNUSED data specified when idle was setup
@return FALSE always
*/
static gboolean _e2_window_unadvise (gpointer data)
{
	printd (DEBUG, "In _e2_window_unadvise()");
	CLOSEBGL
	gtk_widget_hide (app.status_bar_box3);

	GList *children = gtk_container_get_children (GTK_CONTAINER (app.status_bar_box3));
	if (children != NULL)
	{
		GList *member;
		for (member = children; member != NULL; member = member->next)
		{
			if (GTK_IS_WIDGET (member->data))
				gtk_widget_destroy (member->data);	//FIXME make this a more-friendly Q
		}
		g_list_free (children);
	}
#ifdef USE_GTK2_20
	mover = NULL;
#endif
	OPENBGL
	return FALSE;
}
/**
@brief remove statusline message
In case this change is done at a busy time for X/gdk, it's done asynchronously
@return
*/
void e2_window_clear_status_message (void)
{
	//data pointer may be used for killing the idle
	g_idle_add ((GSourceFunc) _e2_window_unadvise, (gpointer) _e2_window_unadvise);
}
/**
@brief create and show main window
This is called only at session-start, from main(), with BGL closed

@param rt ptr to window data struct

@return
*/
void e2_window_create (E2_WindowRuntime *rt)
{
	printd (DEBUG, "create main window");
	_e2_window_set_icon ();

	//setup main window
	app.main_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title (GTK_WINDOW (app.main_window), PROGNAME);
	gtk_window_set_role (GTK_WINDOW (app.main_window), "main");
//	gtk_window_set_wmclass (GTK_WINDOW (app.main_window), "main", BINNAME);
	gtk_widget_set_size_request (app.main_window, 0, 0);	//window as small as possible
	//default size 640 x 480
	//FIXME these are wrong when window is maximised
#ifdef USE_GTK2_18
	e2_cache_int_register ("window-width", &app.main_alloc.width, 640);
	e2_cache_int_register ("window-height", &app.main_alloc.height, 480);
#else
	e2_cache_int_register ("window-width", &app.main_window->allocation.width, 640);
	e2_cache_int_register ("window-height", &app.main_window->allocation.height, 480);
#endif
	gtk_window_set_default_size (GTK_WINDOW (app.main_window),
#ifdef USE_GTK2_18
		app.main_alloc.width, app.main_alloc.height);
#else
		app.main_window->allocation.width, app.main_window->allocation.height);
#endif
	gtk_window_set_resizable (GTK_WINDOW (app.main_window), TRUE);
#ifdef E2_COMPOSIT
	e2_window_set_opacity (app.main_window, -1);
#endif
#ifdef USE_GTK3_0
	g_signal_connect (G_OBJECT (app.main_window), "key-press-event",
		G_CALLBACK (e2_window_key_cb), GUINT_TO_POINTER(1));	//includes translation, non-NULL data to avoid disconnection during bindings setup
	g_signal_connect (G_OBJECT (app.main_window), "key-release-event",
		G_CALLBACK (e2_window_key_cb), NULL); //includes translation
#else
	//arrange for translation of mod-alphabetic-keycodes from locale to ascii
	g_signal_connect (G_OBJECT (app.main_window), "key-press-event",
		G_CALLBACK (e2_utils_key_translate_cb), GUINT_TO_POINTER(1));	//non-NULL data to avoid disconnection during bindings setup
	g_signal_connect (G_OBJECT (app.main_window), "key-release-event",
		G_CALLBACK (e2_utils_key_translate_cb), NULL);
#endif
/*	//create top-level bindings
	e2_keybinding_enrol (app.main_window, _C(17), (void(*)(E2_OptionSet*))NULL);
	//(must be done before file- or output-pane contents are created)
	gchar *category = g_strconcat (_C(17), ".", _C(23),NULL);
	e2_keybinding_enrol (app.main_window, category, (void(*)(E2_OptionSet*))NULL);
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_enrol (app.main_window, _C(17), (void(*)(E2_OptionSet*))NULL);
	e2_mousebinding_enrol (app.main_window, category, (void(*)(E2_OptionSet*))NULL);
# ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (app.main_window, _C(17), (void(*)(E2_OptionSet*))NULL);
	e2_mousegesture_enrol (app.main_window, category, (void(*)(E2_OptionSet*))NULL);
# endif
#endif
	g_free (category);
*/
	g_signal_connect (G_OBJECT (app.main_window), "show",
		G_CALLBACK (_e2_window_show_cb), rt);
	//arrange to clean up gracefully
	g_signal_connect (G_OBJECT (app.main_window), "delete-event",
		G_CALLBACK (_e2_window_gone_cb), NULL); //NULL data = optional cancellation of window-close
/*FIXME if the system needs shutdown-feedback. support
	different processes for user- and system-initiated shutdowns?
	g_signal_connect (G_OBJECT (app.main_window),  ??,
		G_CALLBACK (system_shutdown), NULL); */
	//trap signals that might (but probably don't) get issued when a
	//session-manager initiates a shutdown
//	g_signal_connect (GTK_OBJECT (app.main_window), "destroy", gtk2 only
//		G_CALLBACK (_e2_window_destroy_cb), NULL);
	g_signal_connect (G_OBJECT (app.main_window), "destroy-event",
		G_CALLBACK (_e2_window_gone_cb), (gpointer)TRUE);

#ifdef USE_GTK3_0
	app.hbox_main = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
	app.vbox_main = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	app.hbox_main = gtk_hbox_new (FALSE, 0);
	app.vbox_main = gtk_vbox_new (FALSE, 0);
#endif
	gtk_container_add (GTK_CONTAINER (app.main_window), app.hbox_main);
	gtk_box_pack_start (GTK_BOX (app.hbox_main), app.vbox_main, TRUE, TRUE, 0);

#ifdef E2_RAINBOW
	e2_option_color_filetypes_sync ();
#endif

	//file panes infrastructure
	e2_cache_double_register ("file-pane-ratio-last", &rt->panes_paned_ratio_last, 0.55);
	e2_cache_double_register ("file-pane-ratio", &rt->panes_paned_ratio, 0.55);
	_e2_window_create_pane_boxes (rt);	//setup outer boxes and paned widget for filelists
	//output pane (which can be completed relatively quickly)
	e2_cache_double_register ("output-pane-ratio-last", &rt->output_paned_ratio_last, 0.85);
	e2_cache_double_register ("output-pane-ratio", &rt->output_paned_ratio, 0.85);
	e2_cache_int_register ("output-pane-tabs", &app.tabcount, 1);
	GtkWidget *wid = e2_output_initialise ();
	//set visibility state
	app.output.visible = (rt->output_paned_ratio != 1.0);
#ifdef USE_GTK3_2
	rt->output_paned = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
#else
	rt->output_paned = gtk_vpaned_new ();
#endif
	//CHECKME why block focus here?
/*#ifdef USE_GTK2_18
	gtk_widget_set_can_focus (rt->output_paned, FALSE);
#else
	GTK_WIDGET_UNSET_FLAGS (rt->output_paned, GTK_CAN_FOCUS);
#endif
*/
	gtk_paned_pack1 (GTK_PANED (rt->output_paned), rt->panes_outer_box, TRUE, TRUE);
	gtk_paned_pack2 (GTK_PANED (rt->output_paned), wid, TRUE, TRUE);
	gtk_box_pack_start (GTK_BOX (app.vbox_main), rt->output_paned, TRUE, TRUE, 0);

	//before pane content is added, set separators between panes to approximately
	//their correct position (so that un-necessary content mapping is minimized).
 	//Final positions set later, in _e2_window_show_cb()
	gtk_paned_set_position (GTK_PANED (rt->panes_paned),
#ifdef USE_GTK2_18
		app.main_alloc.width
#else
		app.main_window->allocation.width
#endif
		* rt->panes_paned_ratio);
	gtk_paned_set_position (GTK_PANED (rt->output_paned),
#ifdef USE_GTK2_18
		app.main_alloc.height
#else
		app.main_window->allocation.height
#endif
		* rt->output_paned_ratio);

	gtk_widget_show_all (app.hbox_main); //all, before panes (with toggle-buttons)

	//do this registration here, rather that in the pane context,
	//as it covers both panes, and to avoid attempted re-registers
	//whenever the main window is re-created
/*	gint i; gint j;
	cols_data = NULL;
	E2_Cache *cache = e2_cache_list_register ("columns-data", &cols_data);
	//before writing cache, this will update list data
	cache->sync_func = e2_fileview_update_col_cachedata;
	if (cols_data == NULL)
	{  //there was no cache found, so setup defaults
		for (j = 0; j < 2; j++)
		{ for (i = 0; i < MAX_COLUMNS; i++) {
			stored_col_order[j][i] = i;
			col_width_store[j][i] = e2_all_columns[i].size;
		}}
	}
	else //column data found in cache
	{
		//get data from cached list (no data checks!!)
		//list format = pane1 { order, width, ...} pane2 {order, width, ... }
		GList *iterator = g_list_first (cols_data);
		for (j = 0; j < 2; j++)
		{ for (i = 0; i < MAX_COLUMNS; i++) {
			stored_col_order[j][i] = atoi (iterator->data);
			iterator = iterator->next;
			col_width_store[j][i] = atoi (iterator->data);
			iterator = iterator->next;
		}}
	}
*/
	//register columns data, with 2 panes sequential
#ifdef E2_EXTCOL
	gint orders[] = {0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8};
#else
	gint orders[] = {0,1,2,3,4,5,6,7,0,1,2,3,4,5,6,7};
#endif
	e2_cache_array_register ("columns-order", MAX_COLUMNS*2, (gint *)stored_col_order, orders);
	gint widths [] = {
		e2_all_columns[0].size,
		e2_all_columns[1].size,
		e2_all_columns[2].size,
		e2_all_columns[3].size,
		e2_all_columns[4].size,
		e2_all_columns[5].size,
		e2_all_columns[6].size,
		e2_all_columns[7].size,
#ifdef E2_EXTCOL
		e2_all_columns[8].size,
#endif
		e2_all_columns[0].size,
		e2_all_columns[1].size,
		e2_all_columns[2].size,
		e2_all_columns[3].size,
		e2_all_columns[4].size,
		e2_all_columns[5].size,
		e2_all_columns[6].size,
		e2_all_columns[7].size
#ifdef E2_EXTCOL
		,e2_all_columns[8].size
#endif
		};
	e2_cache_array_register ("columns-width", MAX_COLUMNS*2, (gint *)col_width_store, widths);

	e2_cache_int_register ("pane1-sort-column", &app.pane1.view.sort_column, FILENAME);
	if (app.pane1.view.sort_column < 0)	//may've been cached when columns were unsorted
		app.pane1.view.sort_column = FILENAME;
	e2_cache_int_register ("pane2-sort-column", &app.pane2.view.sort_column, FILENAME);
	if (app.pane2.view.sort_column < 0)
		app.pane2.view.sort_column = FILENAME;
	e2_cache_int_register ("pane1-sort-direction", (gint *) &app.pane1.view.sort_order,
		GTK_SORT_ASCENDING);
	e2_cache_int_register ("pane2-sort-direction", (gint *) &app.pane2.view.sort_order,
		GTK_SORT_ASCENDING);
	e2_cache_bool_register ("pane1-show-hidden", &app.pane1.view.show_hidden, FALSE);
	e2_cache_bool_register ("pane2-show-hidden", &app.pane2.view.show_hidden, FALSE);
	//to avoid slow startup, any use of cached vfs data (including button state)
	//is not done here, but is deferred until an idle when main loop is first started

//#if sizeof(time_t) == 8
//#define LONGTIME
//#endif
	time_t _now = time (NULL);
	if (_now == (time_t) -1)
		_now = 0;

	e2_cache_bool_register ("pane1-filter-names", &app.pane1.view.name_filter.active, FALSE);
	e2_cache_str_register ("pane1-filter-nametype", &app.pane1.view.name_filter.patternptr, "*");
	e2_cache_bool_register ("pane1-filter-nameinvert", &app.pane1.view.name_filter.invert_mask, FALSE);
	e2_cache_bool_register ("pane1-filter-namecase", &app.pane1.view.name_filter.case_sensitive, TRUE);

	e2_cache_bool_register ("pane1-filter-dates", &app.pane1.view.date_filter.active, FALSE);
//#ifdef LONGTIME
	e2_cache_time_register ("pane1-filter-datebase", &app.pane1.view.date_filter.time, _now);
//#else
//	e2_cache_int_register ("pane1-filter-datebase", (gint *) &app.pane1.view.date_filter.time, _now);
//#endif
	e2_cache_int_register ("pane1-filter-daterel", (gint *) &app.pane1.view.date_filter.op, GT);
	e2_cache_int_register ("pane1-filter-datetype", (gint *)&app.pane1.view.date_filter.time_type, MTIME);

	e2_cache_bool_register ("pane1-filter-sizes", &app.pane1.view.size_filter.active, FALSE);
	e2_cache_int_register ("pane1-filter-sizebase", (gint *) &app.pane1.view.size_filter.size, 0);	//size_t=guint
	e2_cache_int_register ("pane1-filter-sizerel", (gint *) &app.pane1.view.size_filter.op, GT);

	e2_cache_bool_register ("pane1-filter-dirs", &app.pane1.view.filter_directories, FALSE);

	e2_cache_bool_register ("pane2-filter-names", &app.pane2.view.name_filter.active, FALSE);
	e2_cache_str_register ("pane2-filter-nametype", &app.pane2.view.name_filter.patternptr, "*");
	e2_cache_bool_register ("pane2-filter-nameinvert", &app.pane2.view.name_filter.invert_mask, FALSE);
	e2_cache_bool_register ("pane2-filter-namecase", &app.pane2.view.name_filter.case_sensitive, TRUE);

	e2_cache_bool_register ("pane2-filter-dates", &app.pane2.view.date_filter.active, FALSE);
//#ifdef LONGTIME
	e2_cache_time_register ("pane2-filter-datebase", &app.pane2.view.date_filter.time, _now);
//#else
//	e2_cache_int_register ("pane2-filter-datebase", (gint *) &app.pane2.view.date_filter.time, _now);
//#endif
	e2_cache_int_register ("pane2-filter-daterel", (gint *) &app.pane2.view.date_filter.op, GT);
	e2_cache_int_register ("pane2-filter-datetype", (gint *) &app.pane2.view.date_filter.time_type, MTIME);

	e2_cache_bool_register ("pane2-filter-sizes", &app.pane2.view.size_filter.active, FALSE);
	e2_cache_int_register ("pane2-filter-sizebase", (gint *) &app.pane2.view.size_filter.size, 0);
	e2_cache_int_register ("pane2-filter-sizerel", (gint *) &app.pane2.view.size_filter.op, GT);

	e2_cache_bool_register ("pane2-filter-dirs", &app.pane2.view.filter_directories, FALSE);

	//get working copies
/*	g_strlcpy (app.pane1.view.name_filter.pattern,
		app.pane1.view.name_filter.patternptr,
		sizeof (app.pane1.view.name_filter.pattern));
	g_strlcpy (app.pane2.view.name_filter.pattern,
		app.pane2.view.name_filter.patternptr,
		sizeof (app.pane2.view.name_filter.pattern)); */

	g_hook_list_init (&app.hook_pane_focus_changed, sizeof (GHook));

	e2_icons_register_stocks (); //register with gtk any installed icons
#ifdef E2_ADD_STOCKS
	//after mainwindow created & before any icons needed, setup cached-icons data
	e2_icons_cache_stocks ();
#endif //def E2_ADD_STOCKS

	e2_pane_create (&app.pane1);
	e2_pane_create (&app.pane2);

	gtk_paned_pack1 (GTK_PANED (rt->panes_paned), app.pane1.outer_box, TRUE, TRUE);
	gtk_paned_pack2 (GTK_PANED (rt->panes_paned), app.pane2.outer_box, TRUE, TRUE);

	if (rt->panes_paned_ratio > 0.05)
	{
		curr_pane = &app.pane1;
		other_pane = &app.pane2;
		curr_view = &app.pane1.view;
		other_view = &app.pane2.view;
	}
	else
	{
		curr_pane = &app.pane2;
		other_pane = &app.pane1;
		curr_view = &app.pane2.view;
		other_view = &app.pane1.view;
	}
	//show which pane is active
	e2_pane_flag_active ();
	//setup correct focus
	gtk_widget_grab_focus (curr_view->treeview);

//	CLOSEBGL //CHECKME needeed for tooltips etc?
	//other toolbars
	e2_toolbar_create (&app.toolbar);
	e2_toolbar_create (&app.commandbar);
//	OPENBGL

	if (e2_option_bool_get_direct (app.commandbar.show)) //FIXME button could be on any bar
	{
		//show corrrect output pane toggle buttons according to cached pane ratio
		if (app.window.output_paned_ratio < 0.001)
			e2_toolbar_toggle_button_set_state
				(toggles_array [E2_TOGGLE_OUTPUTFULL], TRUE);
		else if (app.window.output_paned_ratio > 0.999)
			e2_toolbar_toggle_button_set_state
				(toggles_array [E2_TOGGLE_OUTPUTSHADE], TRUE);
	}

	//status bar
#ifdef USE_GTK3_0
	wid = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING_SMALL);
#else
	wid = gtk_hbox_new (FALSE, E2_PADDING_SMALL);
#endif
	gtk_box_pack_start (GTK_BOX (app.vbox_main), wid, FALSE, FALSE, 0);
	gtk_widget_show (wid);

	//find current user name (can't use environment string ...) once per session
	GString *who_where = g_string_new ("");
	uid_t myuid = getuid ();
	struct passwd *pw_buf = getpwuid (myuid);
	gboolean markup;
	if (pw_buf != NULL)
	{
		 //pw_buf->pw_name assumed UTF-8 or compatible
		if (e2_cl_options.session_user)
		{
			markup = TRUE;
			g_string_append_printf (who_where,
			"<span foreground=\"%s\" weight=\"bold\">%s</span>",
				e2_option_str_get ("color-negative"), pw_buf->pw_name);
		}
		else
		{
			markup = FALSE;
			g_string_assign (who_where, pw_buf->pw_name);
		}
	}
	else
	{
		markup = TRUE;
		g_string_append_printf (who_where,
		"<span foreground=\"%s\" weight=\"bold\"><?%s?></span>",
			e2_option_str_get ("color-negative"), _("WhoAmI"));
	}
	endpwent ();
	//find current machine name
	const gchar *env = g_getenv ("HOSTNAME");
	if (env != NULL)
		g_string_append_printf (who_where, " @ %s", env);	//no translation

	GtkWidget *wid2 = gtk_label_new (NULL);
	if (markup)
		gtk_label_set_markup (GTK_LABEL(wid2), who_where->str);
	else
		gtk_label_set_text (GTK_LABEL(wid2), who_where->str);
	g_string_free (who_where, TRUE);

/*	gtk_misc_set_alignment (GTK_MISC (wid2), 0.0, 0.5);
#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID(wid), wid2, 0, 0, 1, 1);
	QQQ
#else
	gtk_table_attach (GTK_TABLE(wid), wid2, 0, 1, 0, 1,
		GTK_SHRINK, GTK_SHRINK,
		E2_PADDING_SMALL, 0);
#endif
*/
	gtk_box_pack_start (GTK_BOX (wid), wid2, FALSE, FALSE, 10);
	gtk_widget_show (wid2);
//#ifdef E2_POLKIT
//# undef wid2
//#endif
	app.status_bar_label2 = gtk_label_new (" ");
/*	gtk_misc_set_alignment (GTK_MISC (app.status_bar_label2), 0.5, 0.5);
	gtk_box_pack_start (GTK_BOX (app.vbox_main), app.status_bar_label2, FALSE, TRUE, 0);
#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID(wid), app.status_bar_label2, 1, 0, 1, 1);
	QQQ
#else
	gtk_table_attach (GTK_TABLE(wid), app.status_bar_label2, 1, 2, 0, 1,
		GTK_EXPAND, GTK_SHRINK,
		E2_PADDING_SMALL, 0);
#endif
*/
	gtk_box_pack_start (GTK_BOX (wid), app.status_bar_label2, FALSE, FALSE, 0);
	gtk_widget_show (app.status_bar_label2);
	//create hbox for any other status indicator eg for progress bars
	//not shown until needed
#ifdef USE_GTK3_0
	app.status_bar_box3 = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING_SMALL);
#else
	app.status_bar_box3 = gtk_hbox_new (FALSE, E2_PADDING_SMALL);
#endif
	gtk_box_pack_end (GTK_BOX (wid), app.status_bar_box3, FALSE, TRUE, E2_PADDING_LARGE);
/*#ifdef USE_GTK3_2
	gtk_grid_attach (GTK_GRID(wid), app.status_bar_box3, 2, 0, 1, 1);
	QQQ
#else
	gtk_table_attach (GTK_TABLE(wid), app.status_bar_box3, 2, 3, 0, 1,
		GTK_EXPAND, GTK_SHRINK,
		E2_PADDING_SMALL, 0);
#endif
*/
	CLOSEBGL //close BGL, even at session start, signal-callback expects it
	gtk_widget_show (app.main_window);
	OPENBGL
}
/**
@brief re-create main window

This is used after config dialog, or detection of a config file that is updated
(e.g. by another instance of e2), or a change of file-panes direction

@param rt ptr to window data struct

@return
*/
void e2_window_recreate (E2_WindowRuntime *rt)
{
	printd (DEBUG, "recreate main window");

	//prevent any new refresh from starting during this rebuild
	e2_filelist_disable_refresh ();

	//wait until any in-progress filelist process is completed
	while (TRUE)
	{
		gboolean busy;
//		LISTS_LOCK
		busy = g_atomic_int_get (&curr_view->listcontrols.cd_working)
			|| g_atomic_int_get (&curr_view->listcontrols.refresh_working)
			|| g_atomic_int_get (&other_view->listcontrols.cd_working)
			|| g_atomic_int_get (&other_view->listcontrols.refresh_working);
//		LISTS_UNLOCK
		if (busy)
			usleep (50000);
		else
			break;
	}

	//save current selections
	GHashTable *selnames1 = e2_fileview_log_selected_names (curr_view);
	GHashTable *selnames2 = e2_fileview_log_selected_names (other_view);

	//clear hooks
	if (app.pane1.hook_change_dir.is_setup)
		g_hook_list_clear (&app.pane1.hook_change_dir);
	if (app.pane2.hook_change_dir.is_setup)
		g_hook_list_clear (&app.pane2.hook_change_dir);
	//CHECKME
	if (app.pane1.view.hook_refresh.is_setup)
		g_hook_list_clear (&app.pane1.view.hook_refresh);
	if (app.pane2.view.hook_refresh.is_setup)
		g_hook_list_clear (&app.pane2.view.hook_refresh);

	//make sure current cols data are used for the window recreate
	e2_fileview_update_col_cachedata ();

	//save current liststores so data can be reinstated for updating
	GtkListStore *curr_store = curr_view->store;
	g_object_ref (G_OBJECT (curr_store));
	GtkListStore *other_store = other_view->store;
	g_object_ref (G_OBJECT (other_store));

	//save current scroll positions
	gint curr_xscroll, other_xscroll;
	gint curr_yscroll, other_yscroll;
	//FIXME this func always finds column 0
	e2_fileview_get_scroll_data (curr_view, &curr_xscroll, &curr_yscroll);
	e2_fileview_get_scroll_data (other_view, &other_xscroll, &other_yscroll);

	GtkAdjustment *adj;
	gdouble curr_thumbx, curr_upperx, other_thumbx, other_upperx;
	adj = gtk_scrolled_window_get_hadjustment
		(GTK_SCROLLED_WINDOW (curr_pane->pane_sw));
	curr_thumbx = gtk_adjustment_get_value (adj);
#ifdef USE_GTK2_14
	curr_upperx = gtk_adjustment_get_upper (adj);
#else
	curr_upperx = adj->upper;
#endif
	adj = gtk_scrolled_window_get_hadjustment
		(GTK_SCROLLED_WINDOW (other_pane->pane_sw));
	other_thumbx = gtk_adjustment_get_value (adj);
#ifdef USE_GTK2_14
	other_upperx = gtk_adjustment_get_upper (adj);
#else
	other_upperx = adj->upper;
#endif

	//ensure all toolbar toggle buttons are recreated with their current state
//downstream e2_toolbar_toggle_buttons_set_destroyed (NULL);
	//destroy the widgets
	//toolbar(s) may be outside rt->panes_outer_box and we want to zap their data,
	//so destroy them independently
	e2_toolbar_destroy (&app.pane1.toolbar);
	e2_toolbar_destroy (&app.pane2.toolbar);
	e2_toolbar_destroy (&app.toolbar);
	e2_toolbar_destroy (&app.commandbar);
	gtk_widget_destroy (rt->panes_outer_box);

#ifdef E2_COMPOSIT
	e2_window_set_opacity (app.main_window, -1);
#endif

/*	//create top-level bindings
	e2_keybinding_enrol (app.main_window, _C(17), (void(*)(E2_OptionSet*))NULL);
	gchar *category = g_strconcat (_C(17), ".", _C(23),NULL);
	e2_keybinding_enrol (app.main_window, category, (void(*)(E2_OptionSet*))NULL);
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_enrol (app.main_window, _C(17), (void(*)(E2_OptionSet*))NULL);
	e2_mousebinding_enrol (app.main_window, category, (void(*)(E2_OptionSet*))NULL);
# ifdef E2_PTRGESTURES
	e2_mousegesture_enrol (app.main_window, _C(17), (void(*)(E2_OptionSet*))NULL);
	e2_mousegesture_enrol (app.main_window, category, (void(*)(E2_OptionSet*))NULL);
# endif
#endif
	g_free (category);
*/
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (app.main_window, &app.main_alloc);
#endif
	_e2_window_create_pane_boxes (rt);//setup outer boxes and paned widget for filelists
	gtk_widget_show_all (rt->panes_outer_box); //before bars with toggles

	e2_pane_create_part (&app.pane1);
	e2_toolbar_rebadge (&app.pane1.toolbar);
	e2_pane_create_part (&app.pane2);
	e2_toolbar_rebadge (&app.pane2.toolbar);

	e2_toolbar_create (&app.toolbar);
	e2_toolbar_create (&app.commandbar);

	gtk_paned_pack1 (GTK_PANED (rt->panes_paned), app.pane1.outer_box, TRUE, TRUE);
	gtk_paned_pack2 (GTK_PANED (rt->panes_paned), app.pane2.outer_box, TRUE, TRUE);
	gtk_paned_pack1(GTK_PANED (rt->output_paned), rt->panes_outer_box, TRUE, TRUE);

	//update window title if wanted
	if (e2_option_sel_get ("title-type") == 0)
		e2_window_set_title (app.main_window, NULL);
	else
	{
		e2_window_set_title_path (app.main_window, &app.pane1.view);
		e2_window_set_title_path (app.main_window, &app.pane2.view);
	}
	//mark the active pane
	e2_pane_flag_active ();

	WAIT_FOR_EVENTS; //make sure pane parameters are set, before the cb
	NEEDOPENBGL
	_e2_window_show_cb (app.main_window, rt);
	NEEDCLOSEBGL

	e2_keybinding_register_all ();
#ifdef E2_MOUSECUSTOM
	e2_mousebinding_register_all (); //includes gestures if relevant
#endif
	e2_alias_sync (&app.aliases);

	//reinstate former liststore, so its data can be recovered
	gtk_tree_view_set_model (GTK_TREE_VIEW (curr_view->treeview), NULL);
	curr_view->store = curr_store;
	//FIXME block new filelist if currently busy
//	LISTS_LOCK
	curr_view->listcontrols.refreshtype = E2_RECREATE;
//	LISTS_UNLOCK
	e2_fileview_prepare_list (curr_view);	//apply a rebuilt liststore

	//revert the vert scroll
	//needs to be scroll-to-position  - maybe treeview is not
	//immediately ready to accept vertical adjustment change ?
	if (e2_fileview_scroll_to_position (curr_view, curr_xscroll, curr_yscroll))
	{
		//revert the horiz scroll too
		adj = gtk_scrolled_window_get_hadjustment
			(GTK_SCROLLED_WINDOW (curr_pane->pane_sw));
#ifdef USE_GTK2_14
		gtk_adjustment_set_upper (adj, curr_upperx);
#else
		adj->upper = curr_upperx;
#endif
		gtk_adjustment_set_value (adj, curr_thumbx);
		gtk_adjustment_changed (adj);
	}
	//try to reselect items, and at least clean the hash
	e2_fileview_reselect_names (curr_view, selnames1, TRUE);
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
//	printd (DEBUG, "finished new selection for %s", view->dir);
#endif
	gtk_tree_view_set_model (GTK_TREE_VIEW (other_view->treeview), NULL);
	other_view->store = other_store;
	//FIXME block new filelist if currently busy
//	LISTS_LOCK
	other_view->listcontrols.refreshtype = E2_RECREATE;
//	LISTS_UNLOCK
	e2_fileview_prepare_list (other_view);	//apply a rebuilt liststore

	if (e2_fileview_scroll_to_position (other_view, other_xscroll, other_yscroll))
	{
		adj = gtk_scrolled_window_get_hadjustment
			(GTK_SCROLLED_WINDOW (other_pane->pane_sw));
#ifdef USE_GTK2_14
		gtk_adjustment_set_upper (adj, other_upperx);
#else
		adj->upper = other_upperx;
#endif
		gtk_adjustment_set_value (adj, other_thumbx);
		gtk_adjustment_changed (adj);
	}
	//try to reselect items, then clean the hash
	e2_fileview_reselect_names (other_view, selnames2, TRUE);
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
//	printd (DEBUG, "finished new selection for %s", view->dir);
#endif
	//set output visibility flag
	app.output.visible = (rt->output_paned_ratio != 1.0);
	GtkCornerType where = (GtkCornerType) e2_option_sel_get ("scrollbar-position");
	//update each bar position
	GList *member;
	for (member = app.tabslist; member != NULL; member = member->next)
		gtk_scrolled_window_set_placement (
		GTK_SCROLLED_WINDOW (((E2_OutputTabRuntime *)member->data)->scroll), where);
#ifdef ADJACENT_TABS
	if (where == GTK_CORNER_TOP_LEFT || where == GTK_CORNER_BOTTOM_LEFT)
		gtk_notebook_set_tab_pos (GTK_NOTEBOOK (app.outbook), GTK_POS_RIGHT);
	else
		gtk_notebook_set_tab_pos (GTK_NOTEBOOK (app.outbook), GTK_POS_LEFT);
#else
	if (where == GTK_CORNER_TOP_LEFT || where == GTK_CORNER_BOTTOM_LEFT)
		gtk_notebook_set_tab_pos (GTK_NOTEBOOK (app.outbook), GTK_POS_LEFT);
	else
		gtk_notebook_set_tab_pos (GTK_NOTEBOOK (app.outbook), GTK_POS_RIGHT);
#endif
	e2_output_update_style ();
	e2_filelist_enable_refresh ();
	gtk_widget_grab_focus (curr_view->treeview);
}
/**
@brief register main-window-related actions

@return
*/
void e2_window_actions_register (void)
{	//all action-name strings freeable
	E2_Action actions[] =
	{
	{g_strconcat(_A(10),".",_A(33),NULL),_e2_window_adjust_output_ratio_action,TRUE, E2_ACTION_TYPE_ITEM,0,&app.window, NULL},
	{g_strconcat(_A(14),".",_A(33),NULL),_e2_window_adjust_pane_ratio_action,  TRUE, E2_ACTION_TYPE_ITEM,0,&app.window, NULL},
	{g_strconcat(_A(17),".",_A(51),NULL),_e2_window_toggle_full_screen,        FALSE,E2_ACTION_TYPE_ITEM,0,NULL, NULL},
	{g_strconcat(_A(14),".",_A(102),NULL),_e2_window_toggle_panes_direction,    FALSE,E2_ACTION_TYPE_ITEM,0,NULL, NULL},
	//these are "self-managed" toggle actions, with names already logged
	{g_strdup (toggles_array[E2_TOGGLE_OUTPUTSHADE]),
										_e2_window_toggle_visible_output,      FALSE,E2_ACTION_TYPE_ITEM,0,NULL, NULL},
	{g_strdup (toggles_array[E2_TOGGLE_OUTPUTFULL]),
										_e2_window_toggle_full_output,         FALSE,E2_ACTION_TYPE_ITEM,0,NULL, NULL},
 	{g_strdup (toggles_array[E2_TOGGLE_PANE1FULL]),
										_e2_window_toggle_full_pane1,          FALSE,E2_ACTION_TYPE_ITEM,0,NULL, NULL},
 	{g_strdup (toggles_array[E2_TOGGLE_PANE2FULL]),
										_e2_window_toggle_full_pane2,          FALSE,E2_ACTION_TYPE_ITEM,0,NULL, NULL},
	};
	guint i, count = sizeof (actions)/sizeof (E2_Action);
	for (i = 0; i < count; i++)
		e2_action_register (&actions[i]);
}
