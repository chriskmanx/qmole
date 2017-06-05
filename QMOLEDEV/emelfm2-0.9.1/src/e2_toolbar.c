/* $Id: e2_toolbar.c 3069 2014-02-16 06:33:06Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>

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
@file src/e2_toolbar.c
@brief toolbar functions

This file contains the functions for creation, re-creation and
manipuation of toolbars and their contents, and bar context menu.
Except that initialisation of panebars is largely handled
in e2_pane.c
*/
/**
\page toolbar toolbars

ToDo

\section buttons buttons

ToDo

\section address address entries

ToDo
*/

#include "emelfm2.h"
#include <string.h>
#include <time.h>
#include "e2_toolbar.h"
#include "e2_dialog.h"
#include "e2_menu.h"
#include "e2_filelist.h"
#include "e2_complete.h"
#include "e2_icons.h"

//static void resize_cb (GtkWidget *handlebox, GtkAllocation *alloc, E2_ToolbarRuntime *rt);
static GtkWidget *_e2_toolbar_add_button (E2_ToolbarRuntime *rt, gint bstyle,
	gchar *name, gchar *icon, gchar *tip, E2_ActionRuntime *data);
static void _e2_toolbar_populate_menu (GtkWidget *menu, GtkTreeModel *model,
	GtkTreeIter *iter, gint level, gboolean forwards, E2_ToolbarRuntime *rt);
//static void e2_toolbar_add_items (GtkTreeModel *model, GtkTreeIter *iter, gint level,
//	E2_ToolbarRuntime *rt);

  /**********************************/
 /*** bar context-menu callbacks ***/
/**********************************/

/**
@brief edit the contents of a toolbar, via a config dialog

This is performed when the toolbar context-menu item 'edit bar' is selected

@param menuitem UNUSED the activated menu widget
@param rt ptr to E2_ToolbarRuntime for the bar

@return
*/
static void _e2_toolbar_edit_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	//FIXME just recreate the edited bar (need data for the 'apply' function
	NEEDCLOSEBGL
	e2_config_dialog_single (rt->set->name, e2_toolbar_recreate_all, TRUE);
	NEEDOPENBGL
}
/**
@brief hide specified toolbar

This is performed when the toolbar context-menu item 'show bar' is selected

@param menuitem UNUSED the activated menu widget
@param rt ptr to E2_ToolbarRuntime for the bar

@return
*/
static void _e2_toolbar_hide_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0)  //bar's menu is not now being created
	{
		e2_option_bool_toggle_direct (rt->show);  //now FALSE
		NEEDCLOSEBGL
		e2_toolbar_recreate (rt);
		NEEDOPENBGL
	}
}
/**
@brief toggle display of tooltips for widgets in a specified bar

This is performed when the toolbar context-menu item 'show tips' is selected

@param menuitem UNUSED the activated menu widget
@param rt ptr to E2_ToolbarRuntime for the bar

@return
*/
static void _e2_toolbar_toggle_tt_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{
		e2_option_bool_toggle_direct (rt->tooltips);
		NEEDCLOSEBGL
		e2_toolbar_recreate (rt);
		NEEDOPENBGL
	}
}
/**
@brief change a toolbar's space-handling mechanism

This is performed when any of the options on the space handling
sub-menu of the bar's context-menu is selected

@param menuitem the activated sub-menu widget
@param rt ptr to E2_ToolbarRuntime for the bar

@return
*/
static void _e2_toolbar_update_space_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{
		GSList *group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
		gint index = 2 - g_slist_index (group, menuitem);
		if (e2_option_int_get_direct (rt->space) != index)
		{
			e2_option_sel_set_direct (rt->space, index);
			NEEDCLOSEBGL
			e2_toolbar_recreate (rt);
			NEEDOPENBGL
		}
	}
}
/**
@brief toggle toolbar between horizontal and vertical layout

This is performed when the 'horizontal' option on the 'type'
sub-menu of the bar's context-menu is selected

@param menuitem UNUSED the activated menu widget
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_toggle_hori_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{
		e2_option_bool_toggle_direct (rt->hori);
		NEEDCLOSEBGL
		e2_toolbar_recreate (rt);
		NEEDOPENBGL
	}
}
/**
@brief change type of toolbar

type = main toolbar, big panebar etc
This is performed when any of the 'types' on the 'type'
sub-menu of the bar's context-menu is selected

@param menuitem the activated menu widget
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_update_type_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{
		GSList *group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
		gint index = 3 - g_slist_index (group, menuitem);
		if (e2_option_int_get_direct (rt->type) != index)
		{
			e2_option_sel_set_direct (rt->type, index);
			NEEDCLOSEBGL
			e2_toolbar_recreate (rt);
			NEEDOPENBGL
		}
	}
}
/**
@brief  move bar 1 place right or down in its container

This is performed when the corresponding choice is made
from the 'type' sub-menu of the bar's context-menu

@param menuitem UNUSED the activated menu widget
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_increase_priority_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{
		gint i = e2_option_int_get_direct (rt->priority);
		if (e2_option_int_set_direct (rt->priority, i + 1) != i)
		{
			NEEDCLOSEBGL
			e2_toolbar_recreate (rt);
			NEEDOPENBGL
		}
	}
}
/**
@brief move bar 1 place left or up in its container

This is performed when the corresponding choice is made
from the 'type' sub-menu of the bar's context-menu
It causes the bar to be moved left or up in its container

@param menuitem UNUSED the activated menu widget
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_decrease_priority_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{
		gint i = e2_option_int_get_direct (rt->priority);
		if (e2_option_int_set_direct (rt->priority, i - 1) != i)
		{
			NEEDCLOSEBGL
			e2_toolbar_recreate (rt);
			NEEDOPENBGL
		}
	}
}
/**
* @brief move toolbar to the top or left of its container

This is performed when the corresponding choice is made
from the 'type' sub-menu of the bar's context-menu

@param menuitem UNUSED the activated menu widget
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_reset_priority_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0 //bar's menu is not now being created
	&& (e2_option_int_get_direct (rt->priority) != 0))
	{
		e2_option_int_set_direct (rt->priority, 0);
		NEEDCLOSEBGL
		e2_toolbar_recreate (rt);
		NEEDOPENBGL
	}
}
/**
@brief change whether bar buttons are shown with relief

This is performed when the corresponding choice is made
from the bar's context-menu

@param menuitem UNUSED the activated menu widget
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_toggle_relief_cb (GtkCheckMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{
		e2_option_bool_toggle_direct (rt->relief);
		NEEDCLOSEBGL
		e2_toolbar_recreate (rt);
		NEEDOPENBGL
	}
}
/**
@brief change whether bar buttons are shown all with the same width

This is performed when the corresponding choice is made
from the bar's context-menu

@param menuitem UNUSED the activated menu widget
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_toggle_same_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{
		e2_option_bool_toggle_direct (rt->same);
		NEEDCLOSEBGL
		e2_toolbar_recreate (rt);
		NEEDOPENBGL
	}
}
/**
@brief change the way bar buttons are presented

This is performed when the corresponding choice is made
from the bar's context-menu
options include: with/without icon, with/without label,
label below/beside icon

@param menuitem the activated menu widget
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_update_style_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{
		GSList *group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
		gint index = 4 - g_slist_index (group, menuitem);
		if (e2_option_int_get_direct (rt->style) != index)
		{
			e2_option_sel_set_direct (rt->style, index);
			NEEDCLOSEBGL
			e2_toolbar_recreate (rt);
			NEEDOPENBGL
		}
	}
}
/**
@brief change the size of bar button icons

This is performed when the corresponding choice is made
from the icon size sub-menu of the bar's context-menu

@param menuitem the activated menu widget
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_update_isize_cb (GtkMenuItem *menuitem, E2_ToolbarRuntime *rt)
{
	if (g_atomic_int_get (&rt->blocked) == 0) //bar's menu is not now being created
	{

		GSList *group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menuitem));
		gint index = 6 - g_slist_index (group, menuitem);
		if (e2_option_int_get_direct (rt->isize) != index)
		{
			e2_option_sel_set_direct (rt->isize, index);
			NEEDCLOSEBGL
			e2_toolbar_recreate (rt);
			NEEDOPENBGL
		}
	}
}
  /*****************/
 /***** menus *****/
/*****************/

/**
@brief create and pop up non-persistent context-menu for a toolbar

This is the callback for a "button-press-event" signal on a toolbar container.
Unmodified button-3 initiates a context-menu.

@param widget UNUSED the bar container where the button was pressed
@param event Gdk event data struct
@param rt ptr to E2_ToolbarRuntime for the bar

@return TRUE (ie handled) for btn-3, else FALSE
*/
static gboolean _e2_toolbar_click_cb (GtkWidget *widget, GdkEventButton *event,
	E2_ToolbarRuntime *rt)
{
	if (event->button == 3
#ifdef E2_MOUSECUSTOM
	 && (event->state & E2_MODIFIER_MASK) == 0
#endif
	   )
	{
		GtkWidget *menu, *submenu;
		//block local callbacks while the menu is being constructed
		if (!g_atomic_int_compare_and_exchange (&rt->blocked, 0, 1))
			return FALSE;
		NEEDCLOSEBGL
		menu = e2_menu_get ();
		e2_menu_add_check (menu, _("Show _bar"),
			e2_option_bool_get_direct (rt->show), _e2_toolbar_hide_cb, rt);
		e2_menu_add_check (menu, _("Show _tooltips"),
			e2_option_bool_get_direct (rt->tooltips), _e2_toolbar_toggle_tt_cb, rt);
		submenu = e2_menu_add_submenu (menu, _("Space _handling"), NULL);
		GSList *group = NULL;
		gint i = e2_option_int_get_direct (rt->space);
		e2_menu_add_radio (submenu, &group, _("_none"),
			(i == 0), _e2_toolbar_update_space_cb, rt);
		e2_menu_add_radio (submenu, &group, _("_scrollbars"),
			(i == 1), _e2_toolbar_update_space_cb, rt);
		e2_menu_add_radio (submenu, &group, _("_rest button"),
			(i == 2), _e2_toolbar_update_space_cb, rt);
		submenu = e2_menu_add_submenu (menu, _("_Placement"), NULL);
		e2_menu_add_check (submenu, _("_horizontal"),
			e2_option_bool_get_direct (rt->hori), _e2_toolbar_toggle_hori_cb, rt);
		GtkWidget *submenu2 = e2_menu_add_submenu (submenu, _("_Container"), NULL);
		group = NULL;
		i = e2_option_int_get_direct (rt->type);
		e2_menu_add_radio (submenu2, &group, _("_main window"),
			(i == 0), _e2_toolbar_update_type_cb, rt);
		e2_menu_add_radio (submenu2, &group, _("_both panes"),
			(i == 1), _e2_toolbar_update_type_cb, rt);
		e2_menu_add_radio (submenu2, &group, _("file-pane _1"),
			(i == 2), _e2_toolbar_update_type_cb, rt);
		e2_menu_add_radio (submenu2, &group, _("flle-pane _2"),
			(i == 3), _e2_toolbar_update_type_cb, rt);
		if (e2_option_bool_get_direct (rt->hori))
		{
			e2_menu_add (submenu, _("_Up"),
				STOCK_NAME_GO_UP, NULL, _e2_toolbar_decrease_priority_cb, rt);
			e2_menu_add (submenu, _("_Down"),
				STOCK_NAME_GO_DOWN, NULL, _e2_toolbar_increase_priority_cb, rt);
		}
		else
		{
			e2_menu_add (submenu, _("_Left"),
				STOCK_NAME_GO_BACK, NULL, _e2_toolbar_decrease_priority_cb, rt);
			e2_menu_add (submenu, _("_Right"),
				STOCK_NAME_GO_FORWARD, NULL, _e2_toolbar_increase_priority_cb, rt);
		}
		e2_menu_add (submenu, _("_Reset order"), NULL, NULL,
			_e2_toolbar_reset_priority_cb, rt);
		e2_menu_add_check (menu, _("Buttons _relief"),
			e2_option_bool_get_direct (rt->relief), _e2_toolbar_toggle_relief_cb, rt);
		e2_menu_add_check (menu, _("Buttons _same size"),
			e2_option_bool_get_direct (rt->same), _e2_toolbar_toggle_same_cb, rt);
		submenu = e2_menu_add_submenu (menu, _("_Button style"), NULL);
		group = NULL;
		i = e2_option_int_get_direct (rt->style);
		//conform these labels with those in the config setup, below
		e2_menu_add_radio (submenu, &group, _("_theme"),
			(i == 0), _e2_toolbar_update_style_cb, rt);
		e2_menu_add_radio (submenu, &group, _("icon _only"),
			(i == 1), _e2_toolbar_update_style_cb, rt);
		e2_menu_add_radio (submenu, &group, _("_label only"),
			(i == 2), _e2_toolbar_update_style_cb, rt);
		e2_menu_add_radio (submenu, &group, _("icon _above label"),
			(i == 3), _e2_toolbar_update_style_cb, rt);
		e2_menu_add_radio (submenu, &group, _("icon _beside label"),
			(i == 4), _e2_toolbar_update_style_cb, rt);
		submenu = e2_menu_add_submenu (menu, _("_Icon size"), NULL);
		group = NULL;
		i = e2_option_int_get_direct (rt->isize);
		//conform these labels with those in the config setup, below
		e2_menu_add_radio (submenu, &group, _("_theme"),
			(i == 0), _e2_toolbar_update_isize_cb, rt);
		e2_menu_add_radio (submenu, &group, _("_menu"),
			(i == 1), _e2_toolbar_update_isize_cb, rt);
		e2_menu_add_radio (submenu, &group, _("toolbar _small"),
			(i == 2), _e2_toolbar_update_isize_cb, rt);
		e2_menu_add_radio (submenu, &group, _("toolbar _large"),
			(i == 3), _e2_toolbar_update_isize_cb, rt);
		e2_menu_add_radio (submenu, &group, _("_button"),
			(i == 4), _e2_toolbar_update_isize_cb, rt);
		e2_menu_add_radio (submenu, &group, _("drag '_n' drop"),
			(i == 5), _e2_toolbar_update_isize_cb, rt);
		e2_menu_add_radio (submenu, &group, _("_dialog"),
			(i == 6), _e2_toolbar_update_isize_cb, rt);
//		e2_menu_add_separator (menu);
		e2_menu_add (menu, _("Bar i_tems"), STOCK_NAME_PREFERENCES,
			NULL, _e2_toolbar_edit_cb, rt);

		e2_menu_popup (menu, 3, event->time);
		NEEDOPENBGL
		g_atomic_int_set (&rt->blocked, 0);
		return TRUE;
	}
	return FALSE;
}

#ifdef USE_GTK3_0
/**
@brief create and pop up non-persistent context-menu for a toolbar

This is the callback for a "button-press-event" signal on an event-box packed at
the start of a toolbar container.
Unmodified button-3 initiates a context-menu.

@param widget the box where the button was pressed
@param event Gdk event data struct
@param rt ptr to E2_ToolbarRuntime for the bar

@return TRUE (ie handled) for btn-3, else FALSE
*/
static gboolean _e2_toolbareb_click_cb (GtkWidget *eventbox, GdkEventButton *event,
	E2_ToolbarRuntime *rt)
{
//	GtkWidget *barbox = gtk_widget_get_parent (eventbox);
	NEEDOPENBGL
	gboolean retval = _e2_toolbar_click_cb (NULL, event, rt);
	NEEDCLOSEBGL
	return retval;
}
#endif

/* *
@brief destroy rest-menu after a choice has been made

@param menu the rest-menu widget
@param rt pointer to data struct for the toolbar that has the rest button

@return
*/
/*static void _e2_toolbar_destroyrest_menu_cb (GtkWidget *menu, E2_ToolbarRuntime *rt)
{
	NEEDOPENBGL
	e2_menu_selection_done_cb (menu, NULL); or gtk_widget_destroy ?
//	gtk_button_released (GTK_BUTTON (rt->button_rest));	//pop the button back up
	NEEDCLOSEBGL
}
*/
/**
@brief create, but not pop up, non-persistent rest-menu for a toolbar

Menu content is determined by data "bar-model-path" of bar item rt->menu_starter

@param rt ptr to E2_ToolbarRuntime for the toolbar

@return the menu widget
*/
static GtkWidget *_e2_toolbar_createrest_menu (E2_ToolbarRuntime *rt)
{
	GtkTreePath *path = (GtkTreePath *) g_object_get_data (
		G_OBJECT (rt->menu_starter), "bar-model-path");
	if (path != NULL)
	{
		GtkTreeIter iter;
		if (gtk_tree_model_get_iter (rt->set->ex.tree.model, &iter, path))
		{
			GtkWidget *menu = e2_menu_get ();
			_e2_toolbar_populate_menu (menu, rt->set->ex.tree.model, &iter, 1,
				!rt->reversed, rt);
			g_signal_connect (G_OBJECT (menu), "selection-done",
				G_CALLBACK (e2_menu_selection_done_cb), NULL);
			return menu;
		}
	}
	return NULL;
}
/* *
@brief pop up the button pressed to initiate a toolbar submenu

This is the callback for 'selection done' signal for the respective menu
and in the bookmarks setup, and a general submenu setup ...

@param menu UNUSED the menu that is now being closed
@param button the widget clicked to initiate the menu

@return
*/
/*static void _e2_toolbar_menu_finished_cb (GtkWidget *menu, GtkWidget *button)
{
	//permanent menus use this, do not destroy == LEAKS ??
	NEEDOPENBGL
	e2_menu_selection_done_cb (menu, NULL); or gtk_widget_destroy () ?
	gtk_button_released (GTK_BUTTON (button));
	NEEDCLOSEBGL
}
*/
/**
@brief set popup menu position

This function is supplied when calling gtk_menu_popup(), to position the
displayed @a menu.
set @a push_in to TRUE for menu completely inside the screen, FALSE for menu
clamped to screen size

@param menu the GtkMenu to be positioned
@param x place to store gint representing the menu left
@param y place to store gint representing the menu top
@param push_in place to store pushin flag
@param button the activated widget on the toolbar

@return
*/
void e2_toolbar_set_menu_position (GtkMenu *menu, gint *x, gint *y,
	gboolean *push_in, GtkWidget *button)
{
	gint button_x, button_y, left, top;
	GtkRequisition menu_req;
	GtkAllocation alloc;

	E2_ToolbarRuntime *rt = g_object_get_data (G_OBJECT (button), "bar-runtime");

#ifdef USE_GTK2_18
	gtk_widget_get_allocation (rt->toolbar_container_box, &alloc);
#else
	alloc = rt->toolbar_container_box->allocation;
#endif
//	printd (DEBUG, "toolbar container box (@ %x) allocation: left %i top %i width %i height %i",
//		GPOINTER_TO_UINT(rt->toolbar_container_box), alloc.x, alloc.y, alloc.width, alloc.height);

#ifdef USE_GTK3_0
	gtk_widget_get_preferred_size (GTK_WIDGET (menu), NULL, &menu_req);
#else
	gtk_widget_size_request (GTK_WIDGET (menu), &menu_req);
#endif

#ifdef USE_GTK3_0
	e2_utils_get_abs_pos (button, &button_x, &button_y);
#else
# ifdef USE_GTK2_14
	if (gtk_handle_box_get_child_detached(GTK_HANDLE_BOX (rt->toolbar_container)))
# else
	if ((GTK_HANDLE_BOX (rt->toolbar_container))->child_detached)
# endif
	{
		GdkWindow *win = ((GtkHandleBox*)rt->toolbar_container)->float_window;
		gdk_window_get_position (win, &button_x, &button_y);
//		printd (DEBUG, "detatched toolbar window position: left %i top %i", button_x, button_y);
#ifdef USE_GTK2_18
		GtkAllocation alloc2;
		gtk_widget_get_allocation (button, &alloc2);
//		printd (DEBUG, "widget allocation 0: left %i top %i width %i height %i", alloc2.x, alloc2.y, alloc2.width, alloc2.height);
		button_x += alloc2.x;
		button_y += alloc2.y;
#else
//		printd (DEBUG, "widget allocation: left %i top %i", button->allocation.x, button->allocation.y);
		button_x += button->allocation.x;
		button_y += button->allocation.y;
#endif
	}
	else
	{
		e2_utils_get_abs_pos (button, &button_x, &button_y);
	}
#endif //ndef USE_GTK3_0

	if (e2_option_bool_get_direct (rt->hori))	//horizontal bar
	{	//place below or above button, left-aligned
#ifdef USE_GTK3_0
		//hack to work around gtk lack of 'widget-left' allocation up the hierarchy
		if (rt == app.bars[E2_BAR_PANE2]->rt)
		{
			//TODO style-specific handle-width, don't assume 5
			button_x += gtk_paned_get_position (GTK_PANED(app.window.panes_paned)) + 5;
		}
#endif
		left = button_x;
		top = button_y + alloc.height;
		if (top + menu_req.height > gdk_screen_height ())
			top = button_y - menu_req.height;
		*push_in = (top < 0);
	}
	else	//vertical bar
	{	//place right or left of button, top-aligned
		left = button_x + alloc.width;
		if (left + menu_req.width > gdk_screen_width ())
			left = button_x - menu_req.width;
		top = button_y;
		*push_in = (left < 0);
	}
//	printd (DEBUG, "menu position: left %d, top %d", left, top);

	*x = left;
	*y = top;
}
/**
@brief pop up a menu, if it's a 'rest' menu, create it first

If it's a rest-menu, @a button will have data "menu" = NULL, otherwise "menu"
points to an existing menu widget

@param button widget clicked to initiate the menu
@param x coordinate of left of window relative to left of screen
@param y coordinate of top of window relative to screen (WAS + button height)
@param mouse_button no. of mouse button which was pressed, or 0 for mmnemonic
@param time time at which the activation event occurred
@param rt ptr to E2_ToolbarRuntime for the bar

@return
*/
static void _e2_toolbar_menu_popup (GtkWidget *button,
	gint x, gint y, gint mouse_button, guint32 time, E2_ToolbarRuntime *rt)
{
	GtkWidget *menu = (GtkWidget *)g_object_get_data (G_OBJECT (button), "menu");
	if (menu == NULL //button is a 'rest-button'
		&& rt != NULL) //should never fail
			menu = _e2_toolbar_createrest_menu (rt); //create non-persistent menu

	if (menu != NULL)
	{
		if (GTK_IS_BUTTON(button))
			gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
				(GtkMenuPositionFunc) e2_toolbar_set_menu_position, button, mouse_button, time);
		else
			gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
				NULL, button, mouse_button, time);
	}
}
/**
@brief if @a group_cycling is FALSE, create menu and pop-up at a position related to position of @a button

This is the callback for "mnemonic-activate" signal for general sub-menu buttons,
rest-menu button, and bookmarks button
See also the special-case filters toggle-button in _e2_toolbar_button_mnemonic_cb()

@param button activated widget
@param group_cycling TRUE if > 1 widget has the mnemonic being processed now
@param rt ptr to E2_ToolbarRuntime

@return TRUE always (FALSE prevents the menu from showing)
*/
static gboolean _e2_toolbar_menu_popup_mnemonic_cb (GtkWidget *button,
	gboolean group_cycling, E2_ToolbarRuntime *rt)
{
	if (!group_cycling)
	{
		gint x, y;
		NEEDCLOSEBGL
		e2_utils_get_abs_pos (button, &x, &y);
		_e2_toolbar_menu_popup (button, x, y, 0, 0, rt);
		NEEDOPENBGL
		//returning group_cycling, to just grab the focus, does not show the menu
		return TRUE;
	}
	return group_cycling;
}
/**
@brief create menu and pop-up at a position related to the mouse pos (inside @a button)
This is the callback for a "button-release-event" signal on a submenu button,
rest, and bookmarks buttons

@param button clicked button widget
@param event Gdk event data struct
@param rt ptr to E2_ToolbarRuntime

@return FALSE always
*/
static gboolean _e2_toolbar_menu_popup_cb (GtkWidget *button,
	GdkEventButton *event, E2_ToolbarRuntime *rt)
{
	if (
#ifdef E2_MOUSECUSTOM
		(event->button == 1 //CHECKME ctrl or unmodified only ?
		|| (event->button == 3 && (event->state & E2_MODIFIER_MASK) == 0))
#else
		(event->button == 1 || event->button == 3) //CHECKME any modifiers ?
#endif
	 	&& e2_utils_check_release (event)
	   )
	{
		NEEDCLOSEBGL
		//check mouse has not moved from the clicked button
#ifdef USE_GTK2_18
		GtkAllocation alloc;
		gtk_widget_get_allocation (button, &alloc);
		if (event->x <= alloc.width && event->y <= alloc.height)
#else
		if (event->x <= button->allocation.width && event->y <= button->allocation.height)
#endif
		{
			gint x = event->x_root - event->x;
			gint y = event->y_root - event->y;
			_e2_toolbar_menu_popup (button, x, y, event->button, event->time, rt);
		}
		NEEDOPENBGL
	}
	return FALSE;
}
/* *
@brief UNUSED resize all command lines in a toolbar

This is called when a bar is resized, so that commandlines ...
Commandlines are recognised by a data field "e2-command-line"
CHECKME = this is needed only if max width of command line is implemented

@param child toolbar-container child widget
@param rt ptr to E2_ToolbarRuntime for the bar

@return
*/
/*static void _e2_toolbar_resize_combo (GtkWidget *child, E2_ToolbarRuntime *rt)
{
	GtkWidget *command_line = g_object_get_data (G_OBJECT (child), "e2-command-line");
	if (command_line != NULL)
	{
		gint max_width = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (command_line), "cl_max_width"));
		if (max_width == -1)  return;
		GtkRequisition req_cl, req_hb;
#ifdef USE_GTK3_0
		gtk_widget_get_preferred_size (GTK_WIDGET (rt->toolbar), &req_hb, NULL); CHECKME min ?
#else
		gtk_widget_size_request (GTK_WIDGET (rt->toolbar), &req_hb);
#endif
		if (req_hb.width == rt->alloc->width)
			return;
		printd (DEBUG, "resizing command line for %s", rt->option);

#ifdef USE_GTK3_0
		gtk_widget_get_preferred_size (command_line, &req_cl, NULL); CHECKME min ?
#else
		gtk_widget_size_request (command_line, &req_cl);
#endif
		//req_hb.width _should_ be bigger than req_cl.width
		gint new_width = rt->alloc->width - req_hb.width + req_cl.width;
		//check upper limit
		if (new_width > max_width)
			new_width = max_width;
		if (new_width > rt->alloc->width)  //bug, hb width < cl width !!
			new_width = rt->alloc->width;
		//check lower limit
		if (new_width < 1)
			new_width = 1;
		gint min_width = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (command_line), "cl_min_width"));
		if ((min_width != -1) && (new_width < min_width))
			new_width = min_width;

		if (req_cl.width != new_width)
			gtk_widget_set_size_request (GTK_WIDGET (child), new_width, -1);
	}
}
*/
/**
@brief show 'rest' button at end of toolbar

If not done before, create a 'rest' button, add it to end of bar container,
and show the button. Or if button already created, just show it.
The button's visibility is indicated by rt->restbtn_shown

@param rt ptr to data struct for toolbar that has been resized (=rt->toolbar)

@return
*/
static void _e2_toolbar_add_rest_button (E2_ToolbarRuntime *rt)
{
	if (rt->restbtn_shown)
	{	//button is already displayed
//		printd (DEBUG, "rest button already displayed");
		return;
	}
	if (rt->button_rest != NULL)
	{	//button is already created
		gtk_widget_show (rt->button_rest);
		rt->restbtn_shown = TRUE;
//		printd (DEBUG, "re-displayed existing rest button");
		return;
	}
	//create button styled according to options in force
	gint bstyle = e2_option_int_get_direct (rt->style);
	if (bstyle == 0)
	{ //convert default bar style to corresponding local enumerator
		GtkSettings* defs = gtk_settings_get_default ();
        g_object_get (G_OBJECT (defs), "gtk-toolbar-style", &bstyle, NULL);
		bstyle++;	//adjust for 0=default
	}
	//can't use the general toolbar button-adder, that does more and different things
	rt->button_rest = e2_button_get_full (
		(bstyle == 1) ? NULL : _("_Rest"),
		(bstyle == 2) ? NULL : (e2_option_bool_get_direct (rt->hori)) ? STOCK_NAME_GOTO_LAST : STOCK_NAME_GOTO_BOTTOM,
		rt->icon_size, _("Show a menu of hidden items"), NULL, NULL,
		(bstyle == 1) ? E2_BUTTON_SHOW_MISSING_ICON : (bstyle == 3) ? E2_BUTTON_ICON_ABOVE_LABEL : 0
	);
	gtk_button_set_relief (GTK_BUTTON (rt->button_rest),
		e2_option_bool_get_direct (rt->relief) ? GTK_RELIEF_NORMAL : GTK_RELIEF_NONE);
	g_object_set_data (G_OBJECT (rt->button_rest), "bar-runtime", rt);
	g_signal_connect (G_OBJECT (rt->button_rest), "button-press-event",
		G_CALLBACK (e2_utils_generic_press_cb), NULL);
	g_signal_connect (G_OBJECT (rt->button_rest), "button-release-event",
		G_CALLBACK (_e2_toolbar_menu_popup_cb), rt);
	g_signal_connect (G_OBJECT (rt->button_rest), "mnemonic-activate",
		G_CALLBACK (_e2_toolbar_menu_popup_mnemonic_cb), rt);
	//this one always goes at the end, regardless of bar layout
	gtk_box_pack_end (GTK_BOX (rt->toolbar_container_box), rt->button_rest, FALSE, FALSE, 0);
	gtk_widget_show_all (rt->button_rest);
	rt->restbtn_shown = TRUE;
	printd (DEBUG, "rest button created and added to %s",rt->name);
}
/**
@brief after a toolbar item is mapped, adjust the rest-menu start item or hide the rest button

This callback is connected to toolitems (other than command-lines) only when
the toolbar space-handling mode is "rest-menu"

@param tool ptr to toolitem that has just been mapped
@param rt data struct for the toolbar

@return
*/
static void _e2_toolbar_mapitem_cb (GtkToolItem *tool, E2_ToolbarRuntime *rt)
{
/*#ifdef DEBUG_MESSAGES
	gint index = g_list_index (rt->bar_items_list, tool);
	printd (DEBUG, "%s item %d MAPPED", rt->name, index);
#endif
*/
	GList *item = g_list_find (rt->bar_items_list, tool);
	//find the next item on the bar
#ifdef FOLDBARS
	if (rt->reversed)
		item = item->prev;
	else
#endif
		item = item->next;

	if (item != NULL)
		rt->menu_starter =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (item->data));
#else
			GTK_BIN (item->data)->child;
#endif
	else //last item on the bar is now mapped
		if (rt->button_rest != NULL)
	{
		NEEDCLOSEBGL
		gtk_widget_hide (rt->button_rest);
		NEEDOPENBGL
		rt->restbtn_shown = FALSE;
	}
}
/**
@brief after a toolbar item is unmapped, adjust the space-handing arrangements accordingly

This callback is connected to toolitems (other than command-lines) only when
the toolbar space-handling mode is "rest-menu"
As appropriate, the rest-menu start item will be altered, the rest button displayed

@param tool ptr to toolitem that has just been unmapped
@param rt data struct for the toolbar

@return
*/
static void _e2_toolbar_unmapitem_cb (GtkToolItem *tool, E2_ToolbarRuntime *rt)
{
/*#ifdef DEBUG_MESSAGES
	gint index = g_list_index (rt->bar_items_list, tool);
	printd (DEBUG, "%s item %d UNMAPPED", rt->name, index);
#endif
*/
	//flag the initial item in the rest-menu
	rt->menu_starter =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (tool));
#else
		GTK_BIN (tool)->child;
#endif
	NEEDCLOSEBGL
	_e2_toolbar_add_rest_button (rt);  //create/(re)display rest menu btn
	NEEDOPENBGL
}
#ifdef FOLDBARS
static gboolean _e2_toolbar_resize_idle (E2_ToolbarRuntime *rt);
/**
@brief if necessary, adjust things when a toolbar's size changes
If scrollbar space-handling is in force, then a size request is issued,
locking the minimum bar size and displaying the scrollbar
Or, if rest-menu is in force for a complying panebar, this takes
the commandbar out of the toolbar, and into a box stacked underneath
@param UNUSED toolbar ptr to widget that's being resized (=rt->toolbar)
@param alloc ptr to allocation struct for the toolbar (=&(GTK_WIDGET (toolbar))->allocation)
@param rt data struct for the toolbar

@return
*/
static void _e2_toolbar_alloc_cb (GtkWidget *toolbar, GtkAllocation *alloc,
	E2_ToolbarRuntime *rt)
{
//	printd (DEBUG, "%s resize signal cb", rt->name);
	gboolean horiz = e2_option_bool_get_direct (rt->hori);
	/*Buttons can be of variable size and number, and the rest-button may
	  need to be accounted for, but it can't be actually sized if hidden

	  We cannot simply check some aggregate allocation, or even perimeter-item
	  allocation, because the command-line or padder size is most likely
	  smaller or larger than the command-line minimum size. So we must aggregate
	  the actual sizes of the other items, to get a "rest" size, then add that
	  to the minimum command-line size, to get the fold threshold.
	  This needs to be done each time the bar is resized, because
	  button sizes can change on the fly (in the case of a swapped
	  toggle button with a different sized label)

	  The non-command-line tools are logged in the toolbar's items list.
	  After the bar is mapped, the button sizes will be relevant */

	GList *iterator;
	GtkAllocation item_alloc;
	gint button_total = 0;

	NEEDCLOSEBGL

	for (iterator = rt->bar_items_list; iterator != NULL; iterator = iterator->next)
	{
#ifdef USE_GTK2_18
		gtk_widget_get_allocation (GTK_WIDGET (iterator->data), &item_alloc);
#else
		item_alloc = GTK_WIDGET (iterator->data)->allocation;
#endif
		button_total += (horiz) ? item_alloc.width : item_alloc.height;
	}

	gint clmin;
	if (rt->has_command_line)
	{
		GtkToolItem *tool = rt->dirline_tool;
		if (tool != NULL)
		{	//this is a dirline
			if (horiz)
				clmin = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (tool), "cl_swap_width"));
			else
			{
#ifdef USE_GTK2_18
				gtk_widget_get_allocation (GTK_WIDGET(tool), &item_alloc);
				clmin = item_alloc.height;
#else
				clmin = GTK_WIDGET(tool)->allocation.height;
#endif
			}
		}
		else
			clmin = 0; //command lines can be entirely hidden
	}
	else
		clmin = 0;

	gint handlertype = e2_option_int_get_direct (rt->space);
	if (handlertype == 1)
	{	//space handling by scroll bar
		if (horiz)
			//dunno why the +20 fudge works ...
			gtk_widget_set_size_request (GTK_WIDGET (rt->toolbar),
				button_total + clmin + 20, -1);
		else
			gtk_widget_set_size_request (GTK_WIDGET (rt->toolbar),
				-1, button_total + clmin + 20);
		rt->size_queued = FALSE;
		return; // FALSE;
	}

	if (horiz
		 && handlertype == 2	//rest-btn for space-handling
		 && rt->has_command_line
//#ifndef USE_GTK3_0
//CHECKME && !(GTK_HANDLE_BOX (rt->toolbar_container)->child_detached)
//#endif
		)
	{
		//determine current fold-threshold
		gint threshold = button_total + clmin;
		if (rt->button_rest == NULL)
			rt->restbtn_width = 0;
		else
		{
#ifdef USE_GTK2_18
			gtk_widget_get_allocation (rt->button_rest, &item_alloc);
			rt->restbtn_width = item_alloc.width;
#else
			rt->restbtn_width = rt->button_rest->allocation.width;
#endif
			threshold += rt->restbtn_width;
		}
//		printd (DEBUG, "%s resize, widget requirement %d, allocated width %d", rt->name, threshold, width);
		gboolean change = FALSE;
		if (rt->folded)	//is folded already
		{
			if (threshold <= alloc->width)
			{
				change = TRUE;
				rt->folded = FALSE;
			}
		}
		else	//haven't already split off the dirline
		{
			if (threshold > alloc->width)
			{
				change = TRUE;
				rt->folded = TRUE;
			}
		}
		if (change && !rt->size_queued)
		{
			rt->size_queued = TRUE;
			g_idle_add_full (G_PRIORITY_DEFAULT - 10,
				(GSourceFunc) _e2_toolbar_resize_idle, rt, NULL);
		}
	}
	NEEDOPENBGL
}
/**
@brief idle callback to rearrange toolbar widgets after a resize
(with a consistent BGL state)
@param rt pointer to bar data struct
@return FALSE always to cancel the timer
*/
static gboolean _e2_toolbar_resize_idle (E2_ToolbarRuntime *rt)
{
	rt->size_queued = FALSE; //now it's safe to start another timer
	printd (DEBUG, "_e2_toolbar_resize_timer for %s", rt->name);
	GtkToolItem *padder, *tool = rt->dirline_tool;
	CLOSEBGL
	if (rt->folded)
	{	//transfer the commandline to the toolbar_foldbox
		printd (DEBUG, "export commandline ");
		padder = gtk_separator_tool_item_new ();
		gtk_tool_item_set_expand (padder, TRUE);
		gtk_separator_tool_item_set_draw (GTK_SEPARATOR_TOOL_ITEM (padder), FALSE);

		g_object_ref (G_OBJECT (tool));
		//FIXME this is a crasher problably due to dowstream
		//_e2_command_line_unreal_cb ()
		gtk_container_remove (GTK_CONTAINER (rt->toolbar), GTK_WIDGET (tool));
//		gtk_widget_reparent (GTK_WIDGET (tool), rt->toolbar_foldbox); puts it where?
		gtk_box_pack_end (GTK_BOX (rt->toolbar_foldbox), GTK_WIDGET (tool), TRUE, TRUE, 0);
		gtk_toolbar_insert (rt->toolbar, padder, rt->dirline_index);
		gtk_widget_show (GTK_WIDGET (padder));
		g_object_unref (G_OBJECT (tool));
	}
	else
	{	//replace the padder with the commandline
//		printd (DEBUG, "import commandline ");
		padder = gtk_toolbar_get_nth_item (rt->toolbar, rt->dirline_index);
		gtk_container_remove (GTK_CONTAINER (rt->toolbar), GTK_WIDGET (padder));
		g_object_ref (G_OBJECT (tool));
		gtk_container_remove (GTK_CONTAINER (rt->toolbar_foldbox), GTK_WIDGET (tool));
		gtk_toolbar_insert (rt->toolbar, tool, rt->dirline_index);
		g_object_unref (G_OBJECT (tool));
	}
	OPENBGL
//	printd (DEBUG, "_e2_toolbar_resize_timer ends");
	return FALSE;
}
#endif

#ifndef USE_GTK3_0
/**
@brief after a toolbar is torn-off, retain its size so that its contents are still displayed

@param handlebox the object which received the signal = rt->toolbar_container
@param widget the child widget of the handlebox = rt->toolbar_container_box
@param rt data struct for the toolbar

@return
*/
static void _e2_toolbar_detach_cb (GtkHandleBox *handlebox, GtkWidget *widget,
	E2_ToolbarRuntime *rt)
{
	gboolean horiz;
	gint barsize, clmin, barmin = 0;
	GList *iterator;
	GtkAllocation item_alloc;
	printd (DEBUG, "_e2_toolbar_detach_cb");

	horiz = e2_option_bool_get_direct (rt->hori);
	NEEDCLOSEBGL

	//determine the "full" bar size which will be the minimum when torn off
	for (iterator = rt->bar_items_list; iterator != NULL; iterator = iterator->next)
	{
#ifdef USE_GTK2_18
		gtk_widget_get_allocation (GTK_WIDGET (iterator->data), &item_alloc);
#else
		item_alloc = GTK_WIDGET (iterator->data)->allocation;
#endif
		barmin += (horiz) ? item_alloc.width : item_alloc.height;
	}

	if (rt->has_command_line)
	{
#ifdef FOLDBARS
		GtkToolItem *tool = rt->dirline_tool;
		if (tool != NULL)
		{	//this is a dirline
			if (horiz)
			{
				clmin = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (tool), "cl_swap_width"));
				if (clmin == 0)
					clmin = 100;
			}
			else
			{
#ifdef USE_GTK2_18
				gtk_widget_get_allocation (GTK_WIDGET (tool), &item_alloc);
				clmin = item_alloc.height;
#else
				clmin = GTK_WIDGET(tool)->allocation.height;
#endif
			}
		}
		else
			clmin = 50; //keep at least a small command line visible
#else
		gboolean FIXMEminwidth;
#endif
	}
	else
		clmin = 0;
	barmin += clmin;
	if (rt->button_rest != NULL)
	{
#ifdef USE_GTK2_18
		gtk_widget_get_allocation (rt->button_rest, &item_alloc);
		//CHECKME why is +20 needed to show the last item ?
		barmin += (horiz) ? item_alloc.width : item_alloc.height + 20;
#else
		barmin += (horiz) ? rt->button_rest->allocation.width :
			rt->button_rest->allocation.height + 20;	//CHECKME why is +20 needed to show the last item ?
#endif
	}
#ifdef USE_GTK2_18
	gtk_widget_get_allocation (widget, &item_alloc);
#endif
	if (horiz)
	{
		barsize =
#ifdef USE_GTK2_18
		item_alloc.x + item_alloc.width;
#else
		widget->allocation.x + widget->allocation.width;
#endif
		barsize = MAX (barsize, barmin);
		gtk_widget_set_size_request (widget, barsize, -1);
	}
	else
	{
		barsize =
#ifdef USE_GTK2_18
		item_alloc.y + item_alloc.height;
#else
		widget->allocation.y + widget->allocation.height;
#endif
		barsize = MAX (barsize, barmin);
		gtk_widget_set_size_request (widget, -1, barsize);
	}
#ifdef FOLDBARS
# ifdef USE_GTK2_18
	gtk_widget_get_allocation (GTK_WIDGET(rt->toolbar), &item_alloc);
# endif

	NEEDOPENBGL

	_e2_toolbar_alloc_cb (GTK_WIDGET(rt->toolbar),
# ifdef USE_GTK2_18
		&item_alloc,
# else
		&GTK_WIDGET(rt->toolbar)->allocation,
# endif
		rt);
#endif
}
/**
@brief after a torn-off toolbar is re-connected, allow its size to be adjusted as normal

@param handlebox the object which received the signal = rt->toolbar_container
@param widget the child widget of the handlebox = rt->toolbar_container_box
@param user_data UNUSED pointer to data specified when callback was connected

@return
*/
static void _e2_toolbar_attach_cb (GtkHandleBox *handlebox, GtkWidget *widget,
	gpointer user_data)
{
	printd (DEBUG, "_e2_toolbar_attach_cb");
	NEEDCLOSEBGL
	gtk_widget_set_size_request (widget, -1, -1); //revert to natural size
	NEEDOPENBGL
}
#endif //ndef USE_GTK3_0

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief hover-data cleaner
@param data the data to clean
@return
*/
static void _e2_toolbar_clean_hover (E2HoverData *data)
{
	if (data->timer_id > 0)
		g_source_remove (data->timer_id);
	DEALLOCATE (E2HoverData, data);
}
/**
@brief during re-creation of the bar represented by @a rt, freshen dirline appearance if appropriate
Needed because there will be no cd to freshen during re-creation.
May be called for a bar that's not a panebar.
@param rt pointer to toolbar data struct

@return
*/
void e2_toolbar_rebadge (E2_ToolbarRuntime *rt)
{
	if (rt->has_command_line)
	{
		E2_CommandLineRuntime *clrt;
		GList *line;
		for (line = app.command_lines; line != NULL ; line = line->next)
		{
			clrt = (E2_CommandLineRuntime *)line->data;
			if (!clrt->original && clrt->pane != NULL //these tests are ~same
				&& &(clrt->pane->toolbar) == rt)
			{
				GtkWidget *entry =
#ifdef USE_GTK2_14
					gtk_bin_get_child (GTK_BIN (clrt->combo));
#else
					GTK_BIN (clrt->combo)->child;
#endif
				gtk_entry_set_text (GTK_ENTRY(entry), clrt->pane->view.dir);
				e2_command_line_update_highlight (entry, clrt->pane->view.dir);
			}
		}
	}
}
/**
@brief initialise one or all (4) toolbars' space-handing arrangements

This is called at session-start, when the main window is displayed, and after
window or toolbar re-creations during which toolbars are destroyed
Since this is after the bars and their contents are mapped, the map, unmap,
resize signals for bar buttons can be connected to their callbacks without
irrelevant calls (hence delays).
As appropriate, the rest-menu start item will be set, and the rest button displayed
@param rt pointer to data struct for bar to be processed, or NULL for all bars

@return
*/
//FIXME rationalise the walks
void e2_toolbar_initialise_space_handler (E2_ToolbarRuntime *rt)
{
	gint i, count;
	gint barsize, fullsize;

	if (rt != NULL)
		count = 1;
	else
	{
		count = 0;
		E2_ToolbarData **thisbar;
		for (thisbar = app.bars; *thisbar != NULL; thisbar++)
			count++;
	}

	E2_ToolbarRuntime *barsrt [count];

	if (rt != NULL)
		barsrt [0] = rt;
	else
	{
		for (i = 0; i < count; i++)
			barsrt [i] = app.bars[i]->rt;
	}

	for (i=0; i<count; i++)
	{
		if (!e2_option_bool_get_direct (barsrt[i]->show))
			continue;	//don't do anything if the bar is hidden
		gint htype = e2_option_int_get_direct (barsrt[i]->space);
		if (htype != 0)
		{	//there is some sort of space-handling
			GList *iterator;
			//find the currently-allocated end of the bar
			GtkToolbar *tbar = barsrt[i]->toolbar;
			GtkWidget *wid = GTK_WIDGET (tbar);
#ifdef USE_GTK3_0
			GtkOrientation bar_direction = gtk_orientable_get_orientation (GTK_ORIENTABLE (tbar));
#else
			GtkOrientation bar_direction = gtk_toolbar_get_orientation (tbar);
#endif
#ifdef USE_GTK2_18
			GtkAllocation alloc;
			gtk_widget_get_allocation (wid, &alloc);
#endif
			barsize = (bar_direction == GTK_ORIENTATION_HORIZONTAL) ?
#ifdef USE_GTK2_18
				alloc.x + alloc.width : alloc.y + alloc.height;
#else
				wid->allocation.x + wid->allocation.width:
				wid->allocation.y + wid->allocation.height;
#endif
			//find the last visible item on the bar
			//note: after a menu-initiated change of handler-type,
			//barsize = 0 and all .x's = -1
			gboolean visflg = (gtk_toolbar_get_n_items (tbar) > 0);
			if (visflg)
			{
				//find the notional (no-space-constraint) end of that last item
				//CHECKME need to account for fold-threshold effect ?
				fullsize = (bar_direction == GTK_ORIENTATION_HORIZONTAL) ?
#ifdef USE_GTK2_18
					alloc.x + alloc.width : alloc.y + alloc.height;
#else
					wid->allocation.x + wid->allocation.width:
					wid->allocation.y + wid->allocation.height;
#endif
			}
			else
			//nominal size when no item is visible on bar
			//(this includes afer a space-handler change)
				fullsize = 1;

			//checking barsize against fullsize can cause last btn to be hidden
			//without any rest btn FIXME find a way to show that btn (show_all, size_request N/A)
			if (barsize < fullsize + 1)
			{ //there is a restriction, initial handler setup is needed
				if (htype == 2)
				{	//space handling by rest menu
					//find the first (if any) unmapped button on the bar
					for (iterator = barsrt[i]->bar_items_list;
						 iterator != NULL; iterator = iterator->next)
					{
#ifdef USE_GTK2_20
						if (!gtk_widget_get_mapped (GTK_WIDGET (iterator->data)))
#else
						if (!GTK_WIDGET_MAPPED (iterator->data))
#endif
						{
							barsrt[i]->menu_starter =
#ifdef USE_GTK2_14
								gtk_bin_get_child (GTK_BIN (iterator->data));
#else
								GTK_BIN (iterator->data)->child;
#endif
							//create/display rest menu btn
							//this will also decrease the bar size accordingly,
							//and therefore hide other button(s) !!
							_e2_toolbar_add_rest_button (barsrt[i]);
							break;
						}
					}
					if ( iterator == NULL
						// probably after a space-handler change
						&& barsize < fullsize )
						printd (DEBUG, "no index for menu starter !!");
				}
				else  //not rest-menu
					if (htype == 1) //scrollbar used for overflow handling
				{  //make sure scrollbar will be visible, if bar starts too small
					if (e2_option_bool_get_direct (barsrt [i]->hori))
						// + .. to ensure the last item is actually shown FIXME use actual button size
						gtk_widget_set_size_request (GTK_WIDGET (tbar), fullsize + 26, -1);
					else
						gtk_widget_set_size_request (GTK_WIDGET (tbar), -1, fullsize + 26);
				}
			}
			if (htype == 2)	//rest-menu applies
			{
				printd (DEBUG, "connecting [un]maps for %s", barsrt[i]->name);
				//space handlers need to know when things change
				for (iterator = barsrt[i]->bar_items_list; iterator != NULL ;
						iterator = iterator->next)
				{
	/*				//cleanup any old callback CHECKME needed ?
					g_signal_handlers_disconnect_matched (G_OBJECT (iterator->data),
						G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_toolbar_mapitem_cb, NULL);
					g_signal_handlers_disconnect_matched (G_OBJECT (iterator->data),
						G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_toolbar_unmapitem_cb, NULL);
	*/
					g_signal_connect (G_OBJECT (iterator->data), "map", G_CALLBACK (_e2_toolbar_mapitem_cb), barsrt[i]);
					g_signal_connect (G_OBJECT (iterator->data), "unmap", G_CALLBACK (_e2_toolbar_unmapitem_cb), barsrt[i]);
				}
			}
#ifdef FOLDBARS
			//if appropriate
			if (htype == 1	//scrollbar space-handling
				|| (htype == 2
				&& g_str_has_prefix (barsrt[i]->name, "panebar")	//no translation
				&& barsrt[i]->has_command_line
				&& e2_option_bool_get_direct (barsrt[i]->hori))
			)
			{
				//immediately [un]fold if appropriate (BGL doesn't matter)
				NEEDOPENBGL
				_e2_toolbar_alloc_cb (wid,
# ifdef USE_GTK2_18
					&alloc,
# else
					&wid->allocation,
# endif
					barsrt[i]);
				NEEDCLOSEBGL
				//now connect to dirline-shifter cb
				//CHECKME prior cleanup for any old callback ?
				g_signal_connect (G_OBJECT (tbar), "size-allocate",
					G_CALLBACK (_e2_toolbar_alloc_cb), barsrt[i]);
			}
#endif
		}
	}
}
/* *
@brief UNUSED pack toolbar into a container

@param box container widget into which the bar will be packed
@param rt ptr to E2_ToolbarRuntime

@return
*/
/*static void _e2_toolbar_pack_and_prio (GtkWidget *box, E2_ToolbarRuntime *rt)
{
	gtk_box_pack_start (GTK_BOX (box), rt->toolbar_container, FALSE, FALSE, 0);
	gtk_box_reorder_child (GTK_BOX (box), rt->toolbar_container, e2_option_int_get_direct (rt->priority));
} */

#ifdef USE_GTK3_0
/**
@brief add spacer to start of toolbar-box, to facilitate context-menu

@param rt pointer to toolbar runtime data struct

@return
*/
static void _e2_toolbar_add_clickspace (E2_ToolbarRuntime *rt)
{
	GtkWidget *eb = e2_widget_add_eventbox (rt->toolbar_container, FALSE, 0);
	//TODO allow this 'reserved' space to fall to 0 when divider moves to extreme
	if (e2_option_bool_get_direct (rt->hori))
		gtk_widget_set_size_request (eb, E2_PADDING_LARGE, -1);
	else
		gtk_widget_set_size_request (eb, -1, E2_PADDING_LARGE);

	gtk_event_box_set_visible_window ((GtkEventBox*)eb, FALSE);
	gtk_widget_add_events (eb, GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);

	gtk_widget_show (eb);
	//hookup the context menu
	g_signal_connect (G_OBJECT (eb), "button-press-event",
		G_CALLBACK (_e2_toolbareb_click_cb), rt);
}
#endif

/**
@brief get option data from a row in the relevant treestore

@param rt ptr to E2_ToolbarRuntime
@param model model for the treestore
@param iter pointer to iter to use for interrogating the model
@param name pointer to location to store the retrieved name
@param icon pointer to location to store the retrieved icon identifier string
@param tip pointer to location to store the retrieved tooltip string
@param type pointer to location to store the retrieved type string
@param arg pointer to location to store the retrieved argument(s) string

@return
*/
static void _e2_toolbar_get_option_row (E2_ToolbarRuntime *rt,
	GtkTreeModel *model, GtkTreeIter *iter, gchar **name,
	gchar **icon, gchar **tip, gchar **type, gchar **arg)
{
//	gchar *_name, *_icon, *_tip, *_type, *_arg;
	gchar *_tip;
	if (type == NULL)
		gtk_tree_model_get (model, iter, 0, name, 1, icon, 2, &_tip, 3, arg, -1);
	else
	{
		gtk_tree_model_get (model, iter, 0, name, 1, icon, 2, &_tip, 3, type,
		4, arg, -1);
	}
//	*name = e2_utils_replace_vars (_name, FALSE?);
//	*icon = _icon;
	*tip = e2_utils_replace_vars (_tip, FALSE);
//	*arg = e2_utils_replace_vars (_arg, FALSE?); DON'T EXPAND UNTIL USED
	if ((*tip != NULL) && (**tip == '\0'))
	{
		g_free (*tip);
		*tip = NULL;
	}
//	g_free (_name);
	g_free (_tip);
//	g_free (_arg);
}
/* *
@brief get a toolbar-configured button
This gets a 'bare' button, intended to be cannibalized for a toggle.
So some aspects of the button layout are effectively irrelevant

@param rt toolbar data struct
@param style index of toolbar button style
@param name translated string used for button label
@param icon path/name string of button icon (may be name of a stock icon)

@return the button widget (not shown)
*/
/*static GtkWidget *_e2_toolbar_get_button (E2_ToolbarRuntime *rt, gint bstyle,
	gchar *name, gchar *icon)
{
	GtkWidget *button = e2_button_get_full (
		(bstyle == 1) ? NULL : name,
		(bstyle == 2) ? NULL : icon,
		rt->icon_size, NULL, NULL, NULL,
		(bstyle == 1) ? E2_BUTTON_SHOW_MISSING_ICON : 0);
	return button;
}*/
/**
@brief toolbar "mnemonic-activate" signal callback
@param button released button widget
@param group_cycling TRUE if > 1 widget has the mnemonic being processed now
@param art pointer to action data

@return @a group_cycling, just grab the focus if TRUE (doesn't work ATM?)
*/
static gboolean _e2_toolbar_button_mnemonic_cb (GtkWidget *button,
	gboolean group_cycling, E2_ActionRuntime *art)
{
	if (!group_cycling)
	{
//		NEEDCLOSEBGLX attention needed
		OPENBGL
		e2_action_run (button, art);
		CLOSEBGL
		//filters buttons show popup menus, which need TRUE in order to display
		if (strstr((gchar *)art->data, _A(25)) != NULL) //<filters>
			return TRUE;
	}
	return group_cycling;
}
/**
@brief toolbar "button-release-event" signal callback to process a clicked button
We use this button-release-event, not "clicked", in order to also handle buttons 2,3 ...
@param button released button widget
@param event pointer to gdk event data
@param art pointer to action data

@return FALSE to empower other handlers
*/
static gboolean _e2_toolbar_button_release_cb (GtkWidget *button, GdkEventButton *event,
	E2_ActionRuntime *art)
{
	if (e2_utils_check_release (event))
	{
//		NEEDCLOSEBGLX attention needed
		OPENBGL
		e2_action_run (button, art);
		CLOSEBGL
	}
	return FALSE; //want the button image to be 'un-pressed'
}
/**
@brief create a button and add it (inside a tool item) to the start or end of toolbar associated with @a rt

Button content and style are determined by options in force, including whether
buttons are same-size or not
A NULL label prevents any mnemonic action (applying a non-NULL but hidden label
won't make a mnemonic work)

@param rt toolbar data struct
@param bstyle enumerator of button-layout 1..4 in accord with corresponding config-data index
@param label translated string used for button label, or NULL
@param icon path/name string of button icon, may be name of a stock icon, or NULL
@param tip translated string for button tooltip, or NULL
@param art pointer to allocated data for the button callbacks, or NULL to omit callbacks connection

@return the (shown) button widget
*/
static GtkWidget *_e2_toolbar_add_button (E2_ToolbarRuntime *rt, gint bstyle,
	gchar *label, gchar *icon, gchar *tip, E2_ActionRuntime *art)
{
	if (!e2_option_bool_get_direct (rt->tooltips))
		tip = NULL;
	GtkWidget *button = e2_button_get_full (
		(bstyle == 1) ? NULL : label,
		(bstyle == 2) ? NULL : icon,
		rt->icon_size, tip, NULL, NULL, //cb is processed here, we don't use "clicked"
		(bstyle == 1) ? E2_BUTTON_SHOW_MISSING_ICON : (bstyle == 3) ? E2_BUTTON_ICON_ABOVE_LABEL : 0
	);
	gtk_button_set_relief (GTK_BUTTON (button),
		e2_option_bool_get_direct (rt->relief) ? GTK_RELIEF_NORMAL : GTK_RELIEF_NONE);

	if (art != NULL) //this is not a sub-menu or rest button (signals for which are connected upstream)
	{
		//not "clicked", we want to also process buttons 2,3...
		g_signal_connect (G_OBJECT (button), "button-press-event",
			G_CALLBACK (e2_utils_generic_press_cb), NULL);
		g_signal_connect (G_OBJECT (button), "button-release-event",
			G_CALLBACK (_e2_toolbar_button_release_cb), art);
		g_signal_connect_data (G_OBJECT (button), "mnemonic-activate",
			G_CALLBACK (_e2_toolbar_button_mnemonic_cb), art,
			(GClosureNotify) e2_action_free_runtime, 0);
	}

	GtkToolItem *tool = gtk_tool_item_new ();
	//this is based on default homogenous state being FALSE, which seems to be
	//correct, but is undocumented
	if (e2_option_bool_get_direct (rt->same))  // if buttons are to be same size
		gtk_tool_item_set_homogeneous (tool, TRUE);
	gtk_container_add (GTK_CONTAINER (tool), button);
	gtk_toolbar_insert (rt->toolbar, tool, (rt->reversed) ? 0:-1);
	gtk_widget_show_all (GTK_WIDGET (tool));
	//add to bar-items cache
	rt->bar_items_list = g_list_append (rt->bar_items_list, tool);

	return button;
}

  /*******************************/
 /*** toggle-button functions ***/
/*******************************/
/**
@brief swap the state of toggle button associated with @a action_name

For a specified action, this hides the current toggle button,
and shows the alternate button for its new state
@param action_name string with (translated) name of the action initiated by the button

@return boolean T/F, the state of the toggle button after this toggle
*/
gboolean e2_toolbar_button_toggle (gchar *action_name)
{
	gchar *hashkey = g_strconcat (action_name, ".", action_name, NULL);
	gboolean retval = e2_toolbar_button_toggle_custom (hashkey);
	g_free (hashkey);
	return retval;
}
/**
@brief swap the state of toggle button associated with @a hashkey
This hides the current toggle button, and shows the alternate button for its
new state
For a predefined toggle, the key will be like action.action.
Otherwise it will be like action1.action2 or cmd1.cmd2
@param hashkey toggles hash key, (translated) string

@return boolean T/F, the state of the toggle button after this toggle
*/
gboolean e2_toolbar_button_toggle_custom (gchar *hashkey)
{
	E2_ToggleData *data = g_hash_table_lookup (toggles_hash, hashkey);
	if (data == NULL)	//but allow toggle if there are no buttons
		return FALSE;
	data->current_state = ! data->current_state;
	gboolean state_new = data->current_state;
	GList *member;
	for (member = data->boxes; member != NULL; member = member->next)
	{
		E2_ToggleBox *ex = member->data;
		if (state_new)
		{
			if (ex->false_image != NULL) gtk_widget_hide (ex->false_image);
			if (ex->false_label != NULL) gtk_widget_hide (ex->false_label);
	if (ex->true_image != NULL)
		printd (DEBUG, "showing TRUE image");
			if (ex->true_image != NULL) gtk_widget_show (ex->true_image);
			if (ex->true_label != NULL) gtk_widget_show (ex->true_label);
#ifdef USE_GTK2_12TIPS
			if (ex->tipped && !ex->trueactive)
			{
		printd (DEBUG, "applying TRUE tip");
				gtk_tool_item_set_tooltip_text (ex->tool, ex->true_tip);
				ex->trueactive = TRUE;
			}
#endif
		}
		else //false now
		{
			if (ex->true_image != NULL) gtk_widget_hide (ex->true_image);
			if (ex->true_label != NULL) gtk_widget_hide (ex->true_label);
	if (ex->false_image != NULL)
		printd (DEBUG, "showing FALSE image");
			if (ex->false_image != NULL) gtk_widget_show (ex->false_image);
			if (ex->false_label != NULL) gtk_widget_show (ex->false_label);
#ifdef USE_GTK2_12TIPS
			if (ex->tipped && ex->trueactive)
			{
		printd (DEBUG, "applying FALSE tip");
				gtk_tool_item_set_tooltip_text (ex->tool, ex->false_tip);
				ex->trueactive = FALSE;
			}
#endif
		}

#ifndef USE_GTK2_12TIPS
		if (ex->tips != NULL)
		{	//tips are enabled for the bar where the button is located
# if GTK_CHECK_VERSION (2,12,0)
			gchar *tmp = g_strdup (ex->tips->tip_text);
			gchar *tmp2 = g_strdup (ex->tips->tip_private);
			//ex->tips address remains correct after this
			gtk_tooltips_set_tip (app.tooltips, ex->tips->widget, tmp2, tmp);
			g_free (tmp);
			g_free (tmp2);
# else
			//CHECKME does this still work ?
			gchar *tmp = ex->tips->tip_text;
			ex->tips->tip_text = ex->tips->tip_private;
			ex->tips->tip_private = tmp;
# endif
			printd (DEBUG, "using old tip process, tip is now %s", ex->tips->tip_text);
		}
#endif
	}
	printd (DEBUG, "state for toggle action %s changed to %s",
			data->true_action, (state_new) ? "true" : "false" );
	return state_new;
}
/**
@brief set state of toggle button associated with @a action

For a specified action, this shows the appropriate toggle button.
Buttons must have been created before this is called

@param action_name string with (translated) name of the action initiated by the button
@param state value to be set, T or F

@return
*/
void e2_toolbar_toggle_button_set_state (gchar *action_name, gboolean state)
{
	gchar *hashkey = g_strconcat (action_name, ".", action_name, NULL);
	E2_ToggleData *data = g_hash_table_lookup (toggles_hash, hashkey);
	g_free (hashkey);
	if (data == NULL)	//but allow set if there are no buttons
		return;
	if (state == data->current_state)
	{
		printd (DEBUG, "SAME state for toggle '%s' (%s)", action_name, (state) ? "true" : "false" );
		return;
	}

	printd (DEBUG, "set state for toggle '%s' to %s", action_name, (state) ? "true" : "false" );
	data->current_state = state;
	GList *member;
	for (member = data->boxes; member != NULL; member = member->next)
	{
		E2_ToggleBox *ex = member->data;
		if (state)
		{
			if (ex->false_image != NULL) gtk_widget_hide (ex->false_image);
			if (ex->false_label != NULL) gtk_widget_hide (ex->false_label);
			if (ex->true_image != NULL) gtk_widget_show (ex->true_image);
			if (ex->true_label != NULL) gtk_widget_show (ex->true_label);
#ifdef USE_GTK2_12TIPS
			if (ex->tipped && !ex->trueactive)
			{
				gtk_tool_item_set_tooltip_text (ex->tool, ex->true_tip);
				ex->trueactive = TRUE;
			}
#endif
		}
		else //false now
		{
			if (ex->true_image != NULL) gtk_widget_hide (ex->true_image);
			if (ex->true_label != NULL) gtk_widget_hide (ex->true_label);
			if (ex->false_image != NULL) gtk_widget_show (ex->false_image);
			if (ex->false_label != NULL) gtk_widget_show (ex->false_label);
#ifdef USE_GTK2_12TIPS
			if (ex->tipped && ex->trueactive)
			{
				gtk_tool_item_set_tooltip_text (ex->tool, ex->false_tip);
				ex->trueactive = FALSE;
			}
#endif
		}

#ifndef USE_GTK2_12TIPS
		if (ex->tips != NULL)
		{	//tips are enabled for the bar where the button is located
# if GTK_CHECK_VERSION (2,12,0)
			gchar *tmp = g_strdup (ex->tips->tip_text);
			gchar *tmp2 = g_strdup (ex->tips->tip_private);
			//ex->tips address remains correct after this
			gtk_tooltips_set_tip (app.tooltips, ex->tips->widget, tmp2, tmp);
			g_free (tmp);
			g_free (tmp2);
# else
			//CHECKME does this still work ?
			gchar *tmp = ex->tips->tip_text;
			ex->tips->tip_text = ex->tips->tip_private;
			ex->tips->tip_private = tmp;
# endif
//			printd (DEBUG, "tip is now %s", ex->tips->tip_text);
		}
#endif
	}
}
/**
@brief get current state of toggle button associated with @a action

Buttons must have been created before this is called

@param action_name string with (translated) name of the action initiated by the button

@return current state of the button, T or F, of F if there is no data
*/
gboolean e2_toolbar_toggle_button_get_state (gchar *action_name)
{
	gchar *hashkey = g_strconcat (action_name, ".", action_name, NULL);
	E2_ToggleData *data = g_hash_table_lookup (toggles_hash, hashkey);
	g_free (hashkey);
	return ((data != NULL) ? data->current_state : FALSE);
}
/**
@brief clear toggle buttons data for all on toolbar associated with @a rt
@param key UNUSED hash key comprising joined action names
@param ex pointer to toggle data struct
@param rt ptr to E2_ToolbarRuntime for a bar

@return
*/
static void _e2_toolbar_toggle_button_clear (gchar *key, E2_ToggleData *ex,
	E2_ToolbarRuntime *rt)
{
	GList *member;
	for (member = ex->boxes; member != NULL; member = member->next)
	{
		E2_ToggleBox *box = member->data;
		if (box->bar_rt == rt)
		{
			//CHECKME tooltip text cleanup ?
			DEALLOCATE (E2_ToggleBox, box);
			if (member->prev != NULL)
			{
				member = member->prev;
				ex->boxes = g_list_delete_link (ex->boxes, member->next);
			}
			else
			{
				//can't iterate if we kill the 1st link
				//clean that up later
				member->data = NULL;
			}
		}
	}
	//if the first entry is now unused ...
	if (ex->boxes != NULL && ex->boxes->data == NULL)
		ex->boxes = g_list_delete_link (ex->boxes, ex->boxes);
}
/* *
@brief helper to clear toggle hash entries that have no content
@param key UNUSED hash key comprising joined action names
@param ex pointer to toggle data struct
@param data UNUSED ptr to data specified when setup

@return TRUE if the table item is to be removed
*/
/*static gboolean _e2_toolbar_toggle_hash_clean (gchar *key, E2_ToggleData *ex,
	gpointer data)
{
	return (ex->boxes == NULL);
} */
/**
@brief clear all toggle button default flags for a bar, or for all bars

To ensure that each toggle button (pair) is recreated,
clear each toggle-button's 'default_set' flag

@param rt ptr to rt data struct for a bar, or NULL for all bars

@return
*/
void e2_toolbar_toggle_buttons_set_destroyed (E2_ToolbarRuntime *rt)
{
	if (rt == NULL)
	{
		E2_BarType barnum;
		for (barnum = 0; barnum<E2_BAR_COUNT; barnum++)
		{
			if (app.bars[barnum]->rt->has_toggle)
			{
				g_hash_table_foreach (toggles_hash,
					(GHFunc) _e2_toolbar_toggle_button_clear,
					app.bars[barnum]->rt);
			}
		}
	}
	else if (rt->has_toggle)
		g_hash_table_foreach (toggles_hash,
			(GHFunc) _e2_toolbar_toggle_button_clear, rt);

//	g_hash_table_foreach_remove (toggles_hash,
//		(GHRFunc) _e2_toolbar_toggle_hash_clean, NULL);
}
/**
@brief update filter toggle button for @a view, and if appropriate, update tooltip to reflect current filters

@param view data structure for view being processed

@return
*/
void e2_toolbar_toggle_filter_button (ViewInfo *view)
{
	E2_ToggleType num = (view == &app.pane1.view) ?
		E2_TOGGLE_PANE1FILTERS : E2_TOGGLE_PANE2FILTERS;
	gchar *hashkey = g_strconcat (toggles_array[num], ".", toggles_array[num], NULL);
	E2_ToggleData *data = g_hash_table_lookup (toggles_hash, hashkey);
	g_free (hashkey);
	if (data == NULL)
		return;

	gboolean filter_on =
			view->name_filter.active
		 || view->size_filter.active
		 || view->date_filter.active;

	//filter_state = TRUE is 'normal' un-filtered state
	gboolean filter_state = e2_toolbar_toggle_button_get_state (toggles_array [num]);
	if (filter_on && filter_state)
		e2_toolbar_toggle_button_set_state (toggles_array [num], FALSE);
	else if (!filter_on && !filter_state)
		e2_toolbar_toggle_button_set_state (toggles_array [num], TRUE);

	if (!e2_toolbar_toggle_button_get_state (toggles_array [num]))
	{
		//construct 2nd line of tip
		GString *filters = g_string_sized_new (64);
		filters = g_string_append_c (filters, '(');	//ascii ok
		gboolean previous = FALSE;

		if (view->name_filter.active)
		{
			if (view->name_filter.invert_mask)
			{
				filters = g_string_append (filters, _("not"));
				filters = g_string_append_c (filters, ' ');
			}
			//FIXME case-sensitive
			filters = g_string_append (filters, view->name_filter.patternptr);
			previous = TRUE;
		}
		if (view->size_filter.active)
		{
			if (previous)
				filters = g_string_append (filters, ", ");
			filters = g_string_append_c (filters, '<' + view->size_filter.op);
			filters = g_string_append_c (filters, ' ');
			if (view->size_filter.size < 10240) // less than 10k
				g_string_append_printf (filters, "%u", (guint)view->size_filter.size);
			else if (view->size_filter.size < 1048576) // less than a meg
			{
				g_string_append_printf (filters, "%.1f%s",
					(gfloat) view->size_filter.size / 1024.0, _("k"));
			}
			else  // more than a meg
			{
				g_string_append_printf (filters, "%.1f%s",
					(gfloat) view->size_filter.size / 1048576.0, _("M"));
			}
			previous = TRUE;
		}
		if (view->date_filter.active)
		{
			if (previous)
				filters = g_string_append (filters, ", ");
			gchar *timenames [3] = {_("modified"), _("accessed"), _("changed")};
			filters = g_string_append (filters, timenames [view->date_filter.time_type]);
			filters = g_string_append_c (filters, ' ');
			filters = g_string_append_c (filters, '<' + view->date_filter.op);
			filters = g_string_append_c (filters, ' ');

			//get which date format to use
			gint date_index = e2_option_int_get ("date-string");
			if (date_index > 5)
				date_index = 0;	//out of range, use default format (should never happen)
			extern gchar *date_format [6];
			gchar date_buf[25];
			struct tm *tm_ptr = localtime (&(view->date_filter.time));
			strftime (date_buf, sizeof(date_buf), date_format[date_index], tm_ptr);
			gchar *utf = e2_utf8_from_locale (date_buf);
			filters = g_string_append (filters, utf);
			g_free (utf);
		}

		filters = g_string_append_c (filters, ')');
		//update 2nd line of current tooltip to reflect current filter settings
		GList *tmp;
		for (tmp = data->boxes; tmp != NULL; tmp = tmp->next)
		{
			E2_ToggleBox *ex = tmp->data;
#ifdef USE_GTK2_12TIPS
			if (ex->tipped && ex->false_tip != NULL)
			{
				gchar *current_tip = ex->false_tip;
				gchar *s = strchr (current_tip, '\n');
				if (s != NULL)
					*s = '\0';
				gchar *newtip = g_strconcat (current_tip, "\n", filters->str, NULL);
				g_free (current_tip);
				ex->false_tip = newtip;
//				if (status is active)
					gtk_tool_item_set_tooltip_text (ex->tool, newtip);
			}
#else
			if (ex->tips != NULL)
			{	//tips are enabled for the bar where the button is located
				gchar *current_tip = ex->tips->tip_text;
				gchar *s = strchr (current_tip, '\n');
				if (s != NULL)
					*s = '\0';
				gchar *newtip = g_strconcat (current_tip, "\n", filters->str, NULL);
# if GTK_CHECK_VERSION (2,12,0)
				current_tip = g_strdup (ex->tips->tip_private);
				//ex->tips address remains correct after this
				gtk_tooltips_set_tip (app.tooltips, ex->tips->widget, newtip, current_tip);
				g_free (newtip);
				g_free (current_tip);
# else
				//CHECKME does this still work ?
				g_free (current_tip);
				ex->tips->tip_text = newtip;
# endif
			}
#endif
		}

		g_string_free (filters, TRUE);
	}
}

  /**************************/
 /**** toolbar creation ****/
/**************************/

/**
@brief add bookmarks to toolbar specified by @a rt

This is the backend for bookmark.<menu> pseudo-action in a toolbar. It adds
buttons to the bar, each with a menu of 'children' if they exist. So YUK the
available marks are not dynamically updated when the user changes marks in-session.
The bookmark 'context menu' is disabled for each bookmark button (but not for
any children)
@param action action to be run when a mark is activated
@param bstyle enumerator of button-style
@param set data struct for the bookmarks option
@param iter pointer to iter to be used for interrogating the set model
@param rt ptr to E2_ToolbarRuntime

@return
*/
static void _e2_toolbar_add_bookmarks (E2_Action *action, gint bstyle,
	E2_OptionSet *set, GtkTreeIter *iter, E2_ToolbarRuntime *rt)
{
	do
	{
		E2_MarkData *mdata = ALLOCATE (E2_MarkData);
		CHECKALLOCATEDWARN (mdata, return;)

		if (rt == app.bars[E2_BAR_PANE1]->rt)
			mdata->pane_num = E2PANE1;
		else if (rt == app.bars[E2_BAR_PANE2]->rt)
			mdata->pane_num = E2PANE2;
		else
			mdata->pane_num = E2PANECUR;
		//flags for persistent-menu (if any) cuz it's destroyed with the button,
		//and no add/remove actions-menu, for this button
		mdata->mark_flags = 0;
		mdata->tpath = NULL; //not needed cuz no actions-menu

		gchar *label, *icon, *tip, *markpath;
		_e2_toolbar_get_option_row (rt, set->ex.tree.model,
			iter, &label, &icon, &tip, NULL, &markpath);
		mdata->mark_path = e2_utils_replace_vars (markpath, TRUE);

		GtkWidget *button = _e2_toolbar_add_button (rt, bstyle, label, icon, tip,
			e2_action_pack_runtime (action, mdata->mark_path, NULL));

		g_signal_connect_data (G_OBJECT (button), "button-press-event",
			G_CALLBACK (e2_bookmark_button_press_cb),
			mdata, (GClosureNotify) e2_menu_mark_clear, 0);

		if (gtk_tree_model_iter_has_child (set->ex.tree.model, iter))
		{
			g_signal_connect (G_OBJECT (button), "button-press-event",
				G_CALLBACK (e2_utils_generic_press_cb), NULL);
			g_signal_connect (G_OBJECT (button), "button-release-event",
				G_CALLBACK (_e2_toolbar_menu_popup_cb), rt);
			g_signal_connect (G_OBJECT (button), "mnemonic-activate",
				G_CALLBACK (_e2_toolbar_menu_popup_mnemonic_cb), rt);

			GtkTreeIter iter2;
			gtk_tree_model_iter_children (set->ex.tree.model, &iter2, iter);
			//create persistent menu of child marks
			GtkWidget *menu = e2_menu_get ();
			mdata->top_menu = menu;
			e2_menu_add_bookmark_items (menu, menu, action, 0,
				mdata->pane_num, set->ex.tree.model, &iter2);
			//signal it's not a 'rest-menu' & destroy menu with the button
			g_object_set_data_full (G_OBJECT (button), "menu", menu,
				(GDestroyNotify) gtk_widget_destroy);
		}
		else
			mdata->top_menu = NULL;

		g_free (label);
		g_free (icon);
		g_free (tip);
	} while (gtk_tree_model_iter_next (set->ex.tree.model, iter));
}
/**
@brief recursively add items to @a menu, in accord with data in @a model

@param menu widget to which the items will be added
@param model pointer to model for treeestore with the menu's contents
@param iter pointer to iter to be used for interrogating @a model
@param level treestore depth
@param forwards TRUE to scan @a model forwards, FALSE for backwards scan
@param rt ptr to toolbar data struct

@return
*/
static void _e2_toolbar_populate_menu (GtkWidget *menu, GtkTreeModel *model,
	GtkTreeIter *iter, gint level, gboolean forwards, E2_ToolbarRuntime *rt)
{
	gboolean (*nextfunc) (GtkTreeModel*,  GtkTreeIter*) = (forwards) ?
		gtk_tree_model_iter_next : e2_tree_iter_previous;
	do
	{
		gchar *name, *icon, *tip, *type, *arg, *realarg;
		_e2_toolbar_get_option_row (rt, model, iter, &name, &icon, &tip, &type, &arg);

		E2_Action *action = e2_action_get_with_custom (type, arg, &realarg);
		switch (action->type)
		{
			case E2_ACTION_TYPE_SUBMENU:
			{
				GtkWidget *submenu;
				if (arg == NULL || *arg == '\0') //no data == not a custom sub-menu
				{
					GtkTreeIter iter2;
					if (gtk_tree_model_iter_children (model, &iter2, iter))
					{
						submenu = e2_menu_get ();
						_e2_toolbar_populate_menu (submenu, model, &iter2, level + 1, TRUE, rt);
					}
					else
						submenu = NULL;
				}
				else	//custom submenu
					submenu = e2_menu_create_custom_menu (arg);

				GtkWidget *item = e2_menu_add (menu, name, icon, tip, NULL, NULL);
				if (submenu != NULL)
				{
//					GtkWidget *item = e2_menu_add (menu, name, icon, tip, NULL, NULL);
					gtk_menu_item_set_submenu (GTK_MENU_ITEM (item), submenu);
				}

				break;
			}
			case E2_ACTION_TYPE_BOOKMARKS:
			{
				GtkTreeIter iter;
				gint32 pane;
				if (rt == app.bars[E2_BAR_PANE1]->rt)
					pane = E2PANE1;
				else if (rt == app.bars[E2_BAR_PANE2]->rt)
					pane = E2PANE2;
				else
					pane = E2PANECUR;
				E2_OptionSet *set = e2_option_get ("bookmarks");

				if ((arg != NULL) && (*arg != '\0'))
				{
					if (e2_tree_find_iter_from_str (set->ex.tree.model, 0, arg, &iter, TRUE))
					{
						GtkTreeIter iter2;
						if (gtk_tree_model_iter_children (set->ex.tree.model, &iter2, &iter))
						{
							//persistent menu requires total reconstruction when marks change
							e2_menu_add_bookmark_items (menu, menu, action, 0,
									pane, set->ex.tree.model, &iter2);
							rt->has_bookmarks = TRUE;
						}
					}
				}
				else if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
				{
					//persistent menu requires total reconstruction when marks change
					e2_menu_add_bookmark_items (menu, menu, action, 0, pane,
							set->ex.tree.model, &iter);
 					rt->has_bookmarks = TRUE;
				}
			}
				break;
			case E2_ACTION_TYPE_PLUGINS:
				e2_menu_create_plugins_menu (menu, FALSE, TRUE);	//FIXME use real value of is_selection
				break;
			case E2_ACTION_TYPE_SEPARATOR:
				e2_menu_add_separator (menu);
				break;
			case E2_ACTION_TYPE_TOGGLE:
				e2_menu_add_toggle (menu, FALSE, name, icon, tip, type, arg);
				break;
			case E2_ACTION_TYPE_ITEM:
				{
					E2_ActionRuntime *art =
						e2_action_pack_runtime (action, realarg, g_free);
					if (art != NULL)
					{
						GtkWidget *item = e2_menu_add (menu, name, icon, tip,
							e2_menu_action_activated_cb2, NULL);
						g_object_set_data_full (G_OBJECT (item), "e2-actruntime",
							art, (GDestroyNotify) e2_action_free_runtime);
					}
				}
				break;
			default:
				break;
		}
		//cleanup
		g_free (name);
		g_free (icon);
		g_free (tip);
		g_free (type);
		if (action->type != E2_ACTION_TYPE_TOGGLE)
			g_free (arg);
		if (action->type != E2_ACTION_TYPE_ITEM)
			g_free (realarg);
	} while ((*nextfunc)(model, iter));
}
/**
@brief add items to the toolbar specified by @a rt

@param model pointer to model for treeestore with the bar's contents
@param iter pointer to iter to be used for interrogating @a model
@param level treestore depth
@param rt ptr to toolbar data struct

@return
*/
static void _e2_toolbar_add_items (GtkTreeModel *model,
	GtkTreeIter *iter, gint level, E2_ToolbarRuntime *rt)
{
	//determine button-style for use downstream
	gint bstyle = e2_option_int_get_direct (rt->style);
	if (bstyle == 0)
	{ //convert default bar style to corresponding local enumerator
		GtkSettings* defs = gtk_settings_get_default ();
        g_object_get (G_OBJECT (defs), "gtk-toolbar-style", &bstyle, NULL);
		bstyle++;	//adjust for 0=default
	}

	gboolean dirline = FALSE;
	do
	{
		gchar *label, *icon, *tip, *task, *arg, *realarg;
		_e2_toolbar_get_option_row (rt, model, iter, &label, &icon, &tip, &task, &arg);
		//when a row is added to a config dialog treeview, and no string entered
		//for a cell, its value can be NULL FIXME prevent that at source
		if (label == NULL) label = g_strdup ("");
		if (icon == NULL) icon = g_strdup ("");
		if (tip == NULL) tip = g_strdup ("");
		//CHECKME need to verify task ?
		if (arg == NULL) arg = g_strdup ("");

		E2_Action *action = e2_action_get_with_custom (task, arg, &realarg);
		GtkWidget *button = NULL;
		switch (action->type)
		{
			case E2_ACTION_TYPE_COMMAND_LINE:
			{
				//NOTE - end-of-setup check for dirline. So setting this here
				//works only if 1 command line per toolbar
				dirline = action->data != NULL; //NULL for cmdline, pane rt for a dirline
				E2_CommandLineRuntime *cl_rt = e2_command_line_create (!dirline, action->data);
//				cl_rt->bar = rt;	//so we can find the parent bar for this cl_rt
				button = cl_rt->combo;	//confusing? "button" actually a combo
				GtkToolItem *tool = gtk_tool_item_new ();
				gtk_container_add (GTK_CONTAINER (tool), button);
/*#ifdef FOLDBARS
				//we want to always but a dirline in its own box
				if (dirline)
					//this is a dir line
					gtk_box_pack_start (GTK_BOX (rt->dirlinebox), GTK_WIDGET (tool),
						TRUE, TRUE, 0);
				else
				{
					gtk_toolbar_insert (GTK_TOOLBAR (rt->toolbar), tool, (rt->reversed) ? 0:-1);
				}
				FIXME
#endif */
#ifdef FOLDBARS
				if (dirline)
				{	//this is a dir line
					rt->dirline_tool = tool;	//assuming only 1 dirline, we cache it
					//for a reversed bar, this index is wrong, but we must wait
					//until the total itemcount is known before correcting it
					rt->dirline_index = gtk_toolbar_get_n_items (GTK_TOOLBAR (rt->toolbar));
				}
#endif
				gtk_toolbar_insert (GTK_TOOLBAR (rt->toolbar), tool, (rt->reversed) ? 0:-1);
				gtk_widget_show_all (GTK_WIDGET (tool));
				//get the width of the box
				gint min, max;
				if (strlen (arg) > 0)
				{
					gchar **split = g_strsplit (arg, ",", 2);
					gchar *end = NULL;
					gint i;
					if (split[0])
					{
						i = (gint) g_ascii_strtoull (split[0], &end, 10);
						if (end != split[0])
							min = i;
						else
							min = 100;	//fallback value
					}
					else
						min = 100;

/*						if (e2_option_bool_get_direct (rt->hori))
//							gtk_widget_set_size_request (command_line, min, -1);
						gtk_widget_set_size_request (GTK_WIDGET (tool), min, -1);
					else
					//with horiz min = -1, GTK makes vertical bars too wide
						gtk_widget_set_size_request (GTK_WIDGET (tool), -1, min);
					40 or so gives a reasonable width, but GTK combo-box
					doesn't do drop-across, so when the line is that narrow,
					you can't read drop-down items
						gtk_widget_set_size_request (command_line, 50, min);
						gtk_widget_set_size_request (GTK_WIDGET (tool), 50, min);
*/
#ifdef FOLDBARS
					g_object_set_data (G_OBJECT (tool), "cl_swap_width", GINT_TO_POINTER(min));
					if (e2_option_int_get_direct (rt->space) == 1	//scrollbar space handling
						&& e2_option_bool_get_direct (rt->hori))
							gtk_widget_set_size_request (GTK_WIDGET (tool), min, -1);
#else
					gtk_widget_set_size_request (GTK_WIDGET (tool), min, -1);
#endif
					if (split[1])
					{
						if (*split[1] == '\0'
							|| !strcmp (split[1], "*")
//								|| !strcmp (split[1], _A(116))	//_("expand"
						)
						{
							max = -1;
							gtk_tool_item_set_expand (tool, TRUE);
						}
						else
						{
							i = (gint) strtoull (split[1], &end, 10);
							if (end != split[0])
								max = i;
							else
								max = 200;	//fallback value
							//min is also needed when max applies
//FIXME distinguish horiz & vert bars	if (e2_option_bool_get_direct (rt->hori))
#ifndef FOLDBARS
							g_object_set_data (G_OBJECT (button), "cl_min_width", GINT_TO_POINTER(min));
#endif
							g_object_set_data (G_OBJECT (button), "cl_max_width", GINT_TO_POINTER(max));
						}
					}
					g_strfreev (split);
				}
//				gtk_widget_set_size_request (button, max, -1);  no point if max = -1, bug if max != -1
//				gtk_widget_set_size_request (button, min, -1);

				rt->has_command_line = TRUE;
			}
				break;
			case E2_ACTION_TYPE_SUBMENU: //persistent submenu for <submenu> config item
			{
				GtkWidget *submenu;

				if (arg == NULL || *arg == '\0') //no data == not a custom sub-menu
				{
					GtkTreeIter iter2;
					if (gtk_tree_model_iter_children (model, &iter2, iter))
					{
						submenu = e2_menu_get ();
						_e2_toolbar_populate_menu (submenu, model, &iter2, level + 1, TRUE, rt);
					}
					else
						submenu = NULL;
				}
				else	//custom menu requested
					submenu = e2_menu_create_custom_menu (arg);

				button = _e2_toolbar_add_button (rt, bstyle, label, icon, tip, NULL);
				if (submenu != NULL)
				{
					g_object_set_data_full (G_OBJECT (button), "menu", submenu,
						(GDestroyNotify) gtk_widget_destroy);
					g_signal_connect (G_OBJECT (button), "button-press-event",
						G_CALLBACK (e2_utils_generic_press_cb), NULL);
					g_signal_connect (G_OBJECT (button), "button-release-event",
						G_CALLBACK (_e2_toolbar_menu_popup_cb), rt);
					g_signal_connect (G_OBJECT (button), "mnemonic-activate",
						G_CALLBACK (_e2_toolbar_menu_popup_mnemonic_cb), rt);
				}
				//else
				//	gtk_widget_set_sensitive (button. FALSE);
			}
				break;
			case E2_ACTION_TYPE_BOOKMARKS:
				{
					E2_OptionSet *set = e2_option_get ("bookmarks");
					GtkTreeIter iter;
					if ((arg != NULL) && (*arg != '\0'))
					{
						if (e2_tree_find_iter_from_str (set->ex.tree.model, 0, arg, &iter, TRUE))
						{
							GtkTreeIter iter2;
							if (gtk_tree_model_iter_children (set->ex.tree.model, &iter2, &iter))
							{
								rt->has_bookmarks = TRUE;
								_e2_toolbar_add_bookmarks (action, bstyle, set, &iter2, rt);
							}
						}
					}
					else if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
					{
						rt->has_bookmarks = TRUE;
						_e2_toolbar_add_bookmarks (action, bstyle, set, &iter, rt);
					}
				}
				break;
			case E2_ACTION_TYPE_HISTORY:
				{
					E2_PaneRuntime *prt;
					button = _e2_toolbar_add_button (rt, bstyle, label, icon, tip, NULL);
					if (rt == app.bars[E2_BAR_PANE1]->rt)
						prt = &app.pane1;
					else if (rt == app.bars[E2_BAR_PANE2]->rt)
						prt = &app.pane2;
					else
						prt = curr_pane;
					GtkWidget *submenu = e2_pane_visited_menu (prt);
					if (submenu != NULL)
					{
						g_object_set_data_full (G_OBJECT (button), "menu", submenu,
							(GDestroyNotify) gtk_widget_destroy);
						g_signal_connect (G_OBJECT (button), "button-press-event",
							G_CALLBACK (e2_utils_generic_press_cb), NULL);
						g_signal_connect (G_OBJECT (button), "button-release-event",
							G_CALLBACK (_e2_toolbar_menu_popup_cb), rt);
						g_signal_connect (G_OBJECT (button), "mnemonic-activate",
							G_CALLBACK (_e2_toolbar_menu_popup_mnemonic_cb), rt);
					}
				}
				break;
			case E2_ACTION_TYPE_TOGGLE:
				{
				 /* a toggle button is like a normal button, but with pair(s)
					of images and/or labels (in accord with the toolbar's
					button style). At any time, one of each pair is hidden.
					Tooltips are paired by using both pointers in the
					GtkTooltipsData struct for the tool widget which contains
					the button */
					static gboolean first = TRUE;
					static gboolean firststate;
					static gchar *firstlabel = NULL;
					static gchar *firsticon = NULL;
					static gchar *firsttip = NULL;
					static gchar *firstcmd = NULL;
					static GtkTreePath *firstpath = NULL;
					if (first)
					{	//process 1st of a pair
						first = FALSE;
						//park until we get the paired name, so we can make hash key
						firststate = g_str_has_suffix (task, _A(119));	//_(on
						firstlabel = g_strdup (label);
						firsticon = g_strdup (icon);
						firsttip = g_strdup (tip); //SOMETIMES FREE THIS
						firstcmd = g_strdup (arg);//NEVER FREE THIS
						//for constructing an overflow menu, we need the path of
						//the 1st of the pair
						firstpath = gtk_tree_model_get_path (model, iter); //NEVER FREE THIS
					}
					else
					{	//process 2nd of the pair
						first = TRUE;
						rt->has_toggle = TRUE;	//remember that this bar has toggle(s)
						//hash table key is the joined action strings
						gchar *hashkey = g_strconcat (firstcmd, ".", arg, NULL);
						//get toggle data, if any
						E2_ToggleData *data = g_hash_table_lookup (toggles_hash, hashkey);
						if (data == NULL)
						{
							data = ALLOCATE (E2_ToggleData);
							CHECKALLOCATEDWARN (data, break;)
							g_hash_table_insert (toggles_hash, g_strdup (hashkey), data);
							data->current_state = firststate;
							data->boxes = NULL;
							if (firststate)
							{
								data->true_action = firstcmd;	//action command string
								data->false_action = g_strdup (arg); //ditto
							}
							else
							{
								data->false_action = firstcmd;
								data->true_action = g_strdup (arg);
							}
						}

						//setup and populate extended toggle data
						E2_ToggleBox *ex = ALLOCATE (E2_ToggleBox);
						CHECKALLOCATEDWARN (ex, break;)
						data->boxes = g_list_append (data->boxes, ex);
						ex->bar_rt = rt;	//remember the bar, for specific cleanups
						ex->button_style = bstyle;
#ifdef USE_GTK2_12TIPS
						button = _e2_toolbar_add_button (rt, bstyle, firstlabel,
							firsticon, NULL, e2_action_pack_runtime (action, hashkey, g_free));
						ex->tool =
#ifdef USE_GTK2_14
							GTK_TOOL_ITEM (gtk_widget_get_parent (button));
#else
							GTK_TOOL_ITEM (button->parent);
#endif
						if (e2_option_bool_get_direct (rt->tooltips))
						{
							ex->true_tip = (firststate) ? firsttip : g_strdup (tip);
							ex->false_tip = (firststate) ? g_strdup (tip) : firsttip;
							if (firststate)
							{
								gtk_tool_item_set_tooltip_text (ex->tool, ex->true_tip);
								ex->trueactive = TRUE;
							}
							else
							{
								gtk_tool_item_set_tooltip_text (ex->tool, ex->false_tip);
								ex->trueactive = FALSE;
							}
							ex->tipped = TRUE;
						}
						else
							ex->tipped = FALSE;
#else
						gchar *usetip;
						if (e2_option_bool_get_direct (rt->tooltips))
						{
							usetip = (data->current_state == firststate) ?
								firsttip : tip;
						}
						else
							usetip = NULL;
						button = _e2_toolbar_add_button (rt, bstyle, firstlabel,
							firsticon, usetip, e2_action_pack_runtime (action, hashkey, g_free));
						if (e2_option_bool_get_direct (rt->tooltips))
						{
							ex->tips = gtk_tooltips_data_get (button);
							if (ex->tips != NULL)
								ex->tips->tip_private = (data->current_state != firststate) ?
									firsttip : g_strdup (tip);
						}
						else
							ex->tips = NULL;
#endif
						g_free (firstlabel);
						g_free (firsticon);
#ifndef USE_GTK2_12TIPS
						if (usetip == firsttip)
							g_free (firsttip);
#endif
						//setup alternate image, label
						GtkWidget *btn2 = e2_button_get_full (
							(bstyle == 1) ? NULL : label,
							(bstyle == 2) ? NULL : icon,
							rt->icon_size, NULL, NULL, NULL,
							(bstyle == 1) ? E2_BUTTON_SHOW_MISSING_ICON : 0);
						//move image &/| label to the real button
						//provided that one or both of name, icon != NULL,
						//button contains alignment which contains a box we want
						GtkWidget *bbox1 =
#ifdef USE_GTK2_14
							gtk_bin_get_child (GTK_BIN (button));
						bbox1 = gtk_bin_get_child (GTK_BIN (bbox1));
#else
							GTK_BIN (GTK_BIN (button)->child)->child;
#endif
						GList *firstchildren = gtk_container_get_children (GTK_CONTAINER (bbox1));
						GtkWidget *bbox2 =
#ifdef USE_GTK2_14
							gtk_bin_get_child (GTK_BIN (btn2));
						bbox2 = gtk_bin_get_child (GTK_BIN (bbox2));
#else
							GTK_BIN (GTK_BIN (btn2)->child)->child;
#endif
						GList *children = gtk_container_get_children (GTK_CONTAINER (bbox2));
						GList *tmp;
						for (tmp = children; tmp != NULL; tmp = tmp->next)
						{
							gtk_widget_reparent ((GtkWidget *)tmp->data, bbox1);
							gtk_widget_show_all ((GtkWidget*)tmp->data);
						}
						gtk_widget_destroy (btn2);
						ex->true_image = (ex->button_style == 2 || firstchildren == NULL) ? //there is no icon
							NULL : firstchildren->data;
						ex->false_image = (ex->button_style == 2 || children == NULL) ?
							NULL : children->data;
						if (ex->button_style == 1) //there is no label
						{
							ex->true_label = NULL;
							ex->false_label = NULL;
						}
						else if (ex->button_style == 2) //there is no icon
						{
							ex->true_label = firstchildren->data;
							ex->false_label = children->data;
						}
						else
						{
							ex->true_label = firstchildren->next->data;
							ex->false_label = children->next->data;
						}

						if (!firststate)
						{
							GtkWidget *tmp;
							tmp = ex->true_image;
							ex->true_image = ex->false_image;
							ex->false_image = tmp;
							tmp = ex->true_label;
							ex->true_label = ex->false_label;
							ex->false_label = tmp;
						}
						//setup initial display
						if (ex->button_style != 2) //there is an icon
							gtk_widget_hide ((data->current_state) ?
								ex->false_image : ex->true_image);
						if (ex->button_style != 1)	//there is a label
							gtk_widget_hide ((data->current_state) ?
								ex->false_label : ex->true_label);

						g_list_free (firstchildren);
						g_list_free (children);

						//replicate the normal end-of-switch button data-assigments
						g_object_set_data (G_OBJECT (button), "bar-runtime", rt);
						g_object_set_data_full (G_OBJECT (button), "bar-model-path",
							firstpath, (GDestroyNotify) gtk_tree_path_free);
						button = NULL;
					}
				}
				break;
			case E2_ACTION_TYPE_ITEM:
			case E2_ACTION_TYPE_HOVER:
				button = _e2_toolbar_add_button (rt, bstyle, label, icon, tip,
					e2_action_pack_runtime (action, realarg, g_free));
				if (action->type == E2_ACTION_TYPE_HOVER && action->data != NULL)
				{
					E2HoverData *hdata = (E2HoverData *)ALLOCATE0 (E2HoverData);
					CHECKALLOCATEDWARN (hdata, break;);
					hdata->hovered = button;
					hdata->callback = ((E2_Callback*)action->data)->callback;
					hdata->data = ((E2_Callback*)action->data)->data;
					g_object_set_data_full (G_OBJECT (button), "_freeme_",
						hdata, (GDestroyNotify) _e2_toolbar_clean_hover);
					g_signal_connect (G_OBJECT (button), "enter-notify-event",
						G_CALLBACK (e2_action_start_hover_cb), hdata);
					g_signal_connect (G_OBJECT (button), "leave-notify-event",
						G_CALLBACK (e2_action_end_hover_cb), hdata);
				}
				break;
			case E2_ACTION_TYPE_SEPARATOR:
			{
				GtkWidget *sep, *align;
				if (e2_option_bool_get_direct (rt->hori))
				{
#ifdef USE_GTK3_2
					sep = gtk_separator_new (GTK_ORIENTATION_VERTICAL);
#else
					sep = gtk_vseparator_new ();
#endif
					align = gtk_alignment_new (0.5, 0.5, 1.0, 0.6);
				}
				else
				{
#ifdef USE_GTK3_2
					sep = gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
#else
					sep = gtk_hseparator_new ();
#endif
					align = gtk_alignment_new (0.5, 0.5, 0.6, 1.0);
				}
				gtk_container_add (GTK_CONTAINER (align), sep);
				gtk_container_set_border_width (GTK_CONTAINER (align), E2_PADDING_XSMALL);
				GtkToolItem *tool = gtk_tool_item_new();
				gtk_container_add (GTK_CONTAINER (tool), align);
				gtk_toolbar_insert (GTK_TOOLBAR (rt->toolbar), tool, (rt->reversed) ? 0:-1);
				gtk_widget_show_all (GTK_WIDGET (tool));
			}
				break;
			default:
				break;
		}

		if (button != NULL)
		{
			g_object_set_data (G_OBJECT (button), "bar-runtime", rt);
			g_object_set_data_full (G_OBJECT (button), "bar-model-path",
				gtk_tree_model_get_path (model, iter),
				(GDestroyNotify) gtk_tree_path_free);
		}

		//cleanup
		g_free (label);
		g_free (task);
		g_free (icon);
		g_free (tip);
		g_free (arg);
		if (action->type != E2_ACTION_TYPE_ITEM
		 && action->type != E2_ACTION_TYPE_HOVER)
			g_free (realarg);
	} while (gtk_tree_model_iter_next (model, iter));
#ifdef FOLDBARS
	if (rt->has_command_line
		&& dirline
		&& rt->reversed)
	{	//this is a reversed dir line, now we can correct the index
		rt->dirline_index = gtk_toolbar_get_n_items (GTK_TOOLBAR (rt->toolbar))
			- rt->dirline_index - 1;
	}
#endif
}
/**
@brief populate toolbar associated with @a rt
Used at session start and in various re-creations
@param rt pointer to toolbar data struct

@return
*/
void e2_toolbar_create (E2_ToolbarRuntime *rt)
{
//	e2_toolbar_initialise (barnum);
	E2_BarType barnum;
	for (barnum = 0; barnum < E2_BAR_COUNT; barnum++)
	{
		if (app.bars[barnum]->rt == rt)
			break;
	}
#ifdef DEBUG_MESSAGES
	if (barnum == E2_BAR_COUNT)
	{
		printd (ERROR, "Fatal toolbar data error - no match for %x", rt);
		return;
	}
#endif
	const gchar *barname = app.bars[barnum]->name;	//internal (untranslated) bar identifier, "taskbar" etc
	//cache pointers to registered options (no error checking)
	gchar *option_name = g_strconcat (barname, "-show", NULL);
	rt->show = e2_option_get (option_name);
	g_free (option_name);
	if (!e2_option_bool_get_direct (rt->show))
		return;
	rt->set = e2_option_get (barname);
	option_name = g_strconcat (barname, "-tooltips", NULL);
	rt->tooltips = e2_option_get (option_name);
	g_free (option_name);
	option_name = g_strconcat (barname, "-space", NULL);
	rt->space =  e2_option_get (option_name);
	g_free (option_name);
	option_name = g_strconcat (barname, "-type", NULL);
	rt->type = e2_option_get (option_name);
	g_free (option_name);
	option_name = g_strconcat (barname, "-hori", NULL);
	rt->hori = e2_option_get (option_name);
	g_free (option_name);
	option_name = g_strconcat (barname, "-priority", NULL);
	rt->priority = e2_option_get (option_name);
	g_free (option_name);
	option_name = g_strconcat (barname, "-relief", NULL);
	rt->relief = e2_option_get (option_name);
	g_free (option_name);
	option_name = g_strconcat (barname, "-same", NULL);
	rt->same = e2_option_get (option_name);
	g_free (option_name);
	option_name = g_strconcat (barname, "-style", NULL);
	rt->style = e2_option_get (option_name);
	g_free (option_name);
	option_name = g_strconcat (barname, "-isize", NULL);
	rt->isize = e2_option_get (option_name);
	g_free (option_name);

	rt->name = barname;
//	rt->public_name = app.bars[barnum]->public_name;
	rt->blocked = 0;
	rt->hidden = FALSE;
	rt->has_toggle = FALSE;
	rt->has_bookmarks = FALSE;
	rt->has_command_line = FALSE;
	rt->restbtn_shown = FALSE;
	rt->button_rest = NULL;
	rt->toolbar = GTK_TOOLBAR (gtk_toolbar_new ());
#ifdef FOLDBARS
	//horizontal panebar2 items are in reverse order
	rt->reversed = (
	  !strcmp (rt->name, "panebar2")	//not translated
	  && e2_option_bool_get_direct (rt->hori)
	  && !e2_option_bool_get ("panes-horizontal")
	);
	gboolean foldable = (
		g_str_has_prefix (rt->name, "panebar")
//		&& e2_option_bool_get_direct (rt->hori) maybe horizontal later, after an orientation change
	);
	if (foldable)
#ifdef USE_GTK3_0
		rt->toolbar_foldbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
		rt->toolbar_foldbox = gtk_vbox_new (FALSE, 0);	//=flag for presence
#endif
	else
		rt->toolbar_foldbox = NULL ;
	rt->folded = FALSE;	//always clear this
#endif

	//setup toolbar
	//strictly, we are not supposed to set style/size - the theme should do it
	gtk_toolbar_set_style (rt->toolbar, e2_option_int_get_direct (rt->style) - 1);	//-1 corrects for our "default" = 0
	//don't want to change the option size if it's 'default', so keep a working copy
	//(can't use rt->toolbar->icon_size, it reverts to default whenever the bar changes size)
	//e2 enumerator has same order as gtk enumerator
	rt->icon_size = e2_option_int_get_direct (rt->isize);
	if (rt->icon_size == 0)	//default
	{
/* gtk 2.6.8 spits warning about gtk-toolbar-icon-size property
	not existing, tho' API doco says it does !
		GtkSettings* defs = gtk_settings_get_default ();
        g_object_get (G_OBJECT (defs), "gtk-toolbar-icon-size", &(rt->icon_size), NULL);
		if (rt->icon_size == 0) */
			rt->icon_size = GTK_ICON_SIZE_LARGE_TOOLBAR;
	}

#ifndef USE_GTK3_0
	//handlebox as toolbar container
	rt->toolbar_container = gtk_handle_box_new ();
	gtk_handle_box_set_shadow_type (GTK_HANDLE_BOX (rt->toolbar_container), GTK_SHADOW_NONE);
#endif
//don't want "size-allocate" signals before window is displayed, they are redundant
//until then, so we connect to that in toolbar space-handler initializer

	//setup toolbar orientation
	if (e2_option_bool_get_direct (rt->hori))
	{
#ifdef USE_GTK3_0
		gtk_orientable_set_orientation (GTK_ORIENTABLE (rt->toolbar), GTK_ORIENTATION_HORIZONTAL);
		rt->toolbar_container = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
		rt->toolbar_container_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
		gtk_toolbar_set_orientation (rt->toolbar, GTK_ORIENTATION_HORIZONTAL);
		gtk_handle_box_set_handle_position ((GtkHandleBox*)rt->toolbar_container, GTK_POS_LEFT);
		gtk_handle_box_set_snap_edge ((GtkHandleBox*)rt->toolbar_container, GTK_POS_TOP);
		rt->toolbar_container_box = gtk_hbox_new (FALSE, 0);
#endif
/*#ifdef FOLDBARS
		if (isdirline && (strchr (rt->name, '2') != NULL))
		{
FIXME make box right-justified, box-pack-end no works !
		}
#endif */
	}
	else
	{
#ifdef USE_GTK3_0
		gtk_orientable_set_orientation (GTK_ORIENTABLE (rt->toolbar), GTK_ORIENTATION_VERTICAL);
		rt->toolbar_container = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
		rt->toolbar_container_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
		gtk_toolbar_set_orientation (rt->toolbar, GTK_ORIENTATION_VERTICAL);
		gtk_handle_box_set_handle_position ((GtkHandleBox*)rt->toolbar_container, GTK_POS_TOP);
		gtk_handle_box_set_snap_edge ((GtkHandleBox*)rt->toolbar_container, GTK_POS_LEFT);
		rt->toolbar_container_box = gtk_vbox_new (FALSE, 0);
#endif
	}
#ifdef USE_GTK3_0
	//reserve space for activating context-menu
	_e2_toolbar_add_clickspace (rt);
#endif

#ifdef FOLDBARS
	if (foldable)
	{
# ifdef USE_GTK3_0
		gtk_box_pack_start (GTK_BOX (rt->toolbar_container),
			rt->toolbar_foldbox, TRUE, TRUE, 0);
# else
		gtk_container_add (GTK_CONTAINER (rt->toolbar_container),
			rt->toolbar_foldbox);
# endif
		gtk_box_pack_start (GTK_BOX (rt->toolbar_foldbox),
			rt->toolbar_container_box, FALSE, TRUE, 0);
	}
	else
	{
#endif
#ifdef USE_GTK3_0
		gtk_box_pack_start (GTK_BOX (rt->toolbar_container),
			rt->toolbar_container_box, TRUE, TRUE, 0);
#else
		gtk_container_add (GTK_CONTAINER (rt->toolbar_container),
			rt->toolbar_container_box);
#endif
	}

	//scrollbars in force
	if (e2_option_int_get_direct (rt->space) == 1)
	{
		GtkWidget *sw;
		if (e2_option_bool_get_direct (rt->hori))
			sw = e2_widget_get_sw_plain (GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
		else
			sw = e2_widget_get_sw_plain (GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
#ifdef USE_GTK3_8
		gtk_container_add (GTK_CONTAINER (sw), (GtkWidget*)rt->toolbar);
#else
		gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW (sw),
			(GtkWidget*)rt->toolbar);
#endif
		gtk_box_pack_start (GTK_BOX (rt->toolbar_container_box), sw, TRUE, TRUE, 0);
	}
	else
	{
		gtk_box_pack_start (GTK_BOX (rt->toolbar_container_box),
			(GtkWidget*)rt->toolbar, TRUE, TRUE, 0);
	}

	//put toolbar into its parent
	GtkWidget *box;
	gboolean hor_now =  e2_option_bool_get_direct (rt->hori);
	switch (e2_option_int_get_direct (rt->type))
	{
		default:	//main window
			box = (hor_now) ? app.vbox_main : app.hbox_main;
			break;
		case 1:	//both panes
			{
			gboolean hor_cfg = e2_option_bool_get ("panes-horizontal");
			if (hor_now)
				box = (hor_cfg) ? app.window.panes_inner_box : app.window.panes_outer_box;
			else
				box = (hor_cfg) ? app.window.panes_outer_box : app.window.panes_inner_box;
			}
			break;
		case 2:	//pane 1
			box = (hor_now) ? app.pane1.inner_box : app.pane1.outer_box;
			break;
		case 3:	//pane 2
			box = (hor_now) ? app.pane2.inner_box : app.pane2.outer_box;
			break;
	}
	gtk_box_pack_start (GTK_BOX (box), rt->toolbar_container, FALSE, FALSE, 0);
	gtk_box_reorder_child (GTK_BOX (box), rt->toolbar_container, e2_option_int_get_direct (rt->priority));

	//hookup the context menu
	g_signal_connect (G_OBJECT (rt->toolbar_container), "button-press-event",
		G_CALLBACK (_e2_toolbar_click_cb), rt);
#ifndef USE_GTK3_0
	//toolbars are in handle boxes, which can be de/attached, and if so,
 	//the size needs to be manipulated to preserve the contents display
	g_signal_connect (G_OBJECT (rt->toolbar_container), "child-detached",
		G_CALLBACK (_e2_toolbar_detach_cb), rt);
	g_signal_connect (G_OBJECT (rt->toolbar_container), "child-attached",
		G_CALLBACK (_e2_toolbar_attach_cb), NULL);
#endif
	//show bar before adding items so its space handler can be properly initialized
	gtk_widget_show_all (rt->toolbar_container);

	//at last, the bar items
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_first (rt->set->ex.tree.model, &iter))
		_e2_toolbar_add_items (rt->set->ex.tree.model, &iter, 0, rt);
}
/**
@brief re-create all toolbars

@return
*/
void e2_toolbar_recreate_all (void)
{
	E2_ToolbarData **thisbar;
	for (thisbar = app.bars; *thisbar != NULL; thisbar++)
		e2_toolbar_recreate ((*thisbar)->rt);
}
/**
@brief destroy and re-create toolbar specified by @a rt

@param rt ptr to toolbar data struct

@return
*/
void e2_toolbar_recreate (E2_ToolbarRuntime *rt)
{
	e2_toolbar_destroy (rt);
	e2_toolbar_create (rt);
	e2_toolbar_rebadge (rt);
	e2_toolbar_initialise_space_handler (rt);
	//CHECKME set appropriate toggle buttons ?
}
/**
@brief destroy widgets and related data for toolbar specified by @a rt
Destroys widgets only if @a rt ->toolbar_container is non-NULL
@param rt ptr to toolbar data struct

@return
*/
void e2_toolbar_destroy (E2_ToolbarRuntime *rt)
{
	if (rt->toolbar_container != NULL)
	{
		//ensure any and all toggle buttons are rebuilt with their current state
		if (rt->has_toggle)
			e2_toolbar_toggle_buttons_set_destroyed (rt);
		//destroy the outer-most container widget of the bar
		gtk_widget_destroy (rt->toolbar_container);
		//nested widgets also destroyed
	}
	if (rt->bar_items_list != NULL)
	{
		g_list_free (rt->bar_items_list) ;
	}
	memset (rt, 0, sizeof (E2_ToolbarRuntime));
}
/**
@brief setup data for all toolbars
TODO error message assumes BGL closed

@return
*/
void e2_toolbar_data_create (void)
{
	static const gchar *barnames [E2_BAR_COUNT] =
	{
		"taskbar",
		"panebar1",
		"panebar2",
		"commandbar"
	};
	const gchar *transnames [E2_BAR_COUNT];
	transnames[E2_BAR_TASK] = _C(41);
	transnames[E2_BAR_PANE1] = _C(30);
	transnames[E2_BAR_PANE2] = _C(32);
	transnames[E2_BAR_COMMAND] = _C(4);
	E2_ToolbarRuntime *barsrt [E2_BAR_COUNT] =
	{
		&app.toolbar,
		&app.pane1.toolbar,
		&app.pane2.toolbar,
		&app.commandbar
	};

	//space for bars and trailing NULL
#ifdef USE_GLIB2_10
	app.bars = g_slice_alloc (sizeof (E2_ToolbarData*) * (E2_BAR_COUNT+1));
#else
	app.bars = g_try_malloc (sizeof (E2_ToolbarData*) * (E2_BAR_COUNT+1));
#endif
#if (CHECKALLOCATEDFATAL)
	CHECKALLOCATEDFATAL (app.bars); //TODO error message assumes BGL closed
#else
	if (app.bars == NULL)
		e2_main_closedown (TRUE, FALSE, TRUE);
#endif
	E2_ToolbarData **thisbar = app.bars;
	gint i;
	for (i = 0; i < E2_BAR_COUNT; i++)
	{
		*thisbar = ALLOCATE (E2_ToolbarData);	//FIXME never deallocated
		CHECKALLOCATEDFATAL (thisbar);
		(*thisbar)->type = (E2_BarType) i;
		(*thisbar)->name = barnames [i];
		(*thisbar)->public_name = transnames [i];
		(*thisbar)->rt = barsrt[i];
		thisbar++;
	}
	*thisbar = NULL;
}
/**
@brief cleanup toolbar-related allocated resources

@return
*/
void e2_toolbar_data_clean (void)
{
	E2_ToolbarData **thisbar;
	gint i;
	for (i = 0, thisbar = app.bars; i < E2_BAR_COUNT; i++, thisbar++)
		DEALLOCATE (E2_ToolbarData, *thisbar);
#ifdef USE_GLIB2_10
	g_slice_free1 (sizeof (E2_ToolbarData*) * (E2_BAR_COUNT+1), app.bars);
#else
	g_free (app.bars);
#endif
	if (toggles_hash != NULL)
		g_hash_table_destroy (toggles_hash);
}

  /*******************/
 /***** actions *****/
/*******************/
/* *
@brief toggle whether specified toolbar exists or not

This is the action for 'toggle <toolbar>'

@param rt ptr to E2_ToolbarRuntime

@return
*/
/*static void _e2_toolbar_toggle (E2_ToolbarRuntime *rt)
{
	if (e2_option_bool_toggle_direct (rt->show))
	{
		e2_toolbar_create (rt);
		e2_toolbar_rebadge (rt);
	}
	else
		e2_toolbar_destroy (rt);
}*/
/* *
@brief register actions for toolbars

@return
*/
/* UNUSED
void e2_toolbar_actions_register (void)
{
} */
/**
@brief set up options for toolbar enumerated by @a barnum

@param barnum code which identifies the bar

@return
*/
void e2_toolbar_options_register (E2_BarType barnum)
{
	const gchar *name = app.bars[barnum]->name;	//internal (untranslated) bar identifier, "taskbar" etc
	const gchar *public_name = app.bars[barnum]->public_name;
	//hacks to make panebars' config pages work
	gchar *shared_name = g_strconcat (public_name,".",_C(27),":",NULL);
	gchar *dep_name;
	if (barnum == E2_BAR_PANE1)
		dep_name = "!pane1-uses-other";	//no translation, over-ride option name
	else if (barnum == E2_BAR_PANE2)
		dep_name = "!pane2-uses-other";	//no translation, over-ride option name
	else
		dep_name = NULL;

	gint i = (gint) barnum;
	//these must be in the same order as the E2_BarType enumerator
	gint data[4][10] = {  //defaults for ...
		{ TRUE, FALSE,TRUE, 2, FALSE, TRUE,  1, 3, 2, 1 },  //taskbar
		{ TRUE, TRUE, TRUE, 2, FALSE, FALSE, 1, 2, 2, 0 },  //panebar1
		{ TRUE, TRUE, TRUE, 2, FALSE, FALSE, 1, 2, 3, 0 },  //panebar2
		{ TRUE, TRUE, TRUE, 2, FALSE, FALSE, 1, 2, 0, 1 },  //commandbar
		};

	//show the bar, j=0
	gchar *group_name = g_strconcat (shared_name, _C(26), NULL); //_("<barname>:miscellaneous"
	gchar *option_name = g_strconcat (name, "-show", NULL);
	gchar *desc = g_strconcat (_("show the "), public_name, NULL);
	gchar *tip = g_strdup_printf (_("This determines whether the %s is displayed or hidden"), public_name);
	e2_option_bool_register (option_name, group_name, desc, tip, dep_name, data[i][0],
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP
		| E2_OPTION_FLAG_FREENAME | E2_OPTION_FLAG_FREEDESC
		| E2_OPTION_FLAG_FREETIP | E2_OPTION_FLAG_BUILDBARS);
	//bar is horizontal, j=1
	option_name = g_strconcat (name, "-hori", NULL);
	desc =	g_strconcat (public_name, _(" horizontal"), NULL);
	tip = g_strdup_printf (_("This determines whether the %s is displayed horizontally or vertically"), public_name);
	e2_option_bool_register (option_name, group_name, desc, tip, dep_name, data[i][1],
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREENAME
		| E2_OPTION_FLAG_FREEDESC | E2_OPTION_FLAG_FREETIP
		| E2_OPTION_FLAG_BUILDBARS);
	//show tooltips, j=2
	option_name = g_strconcat (name, "-tooltips", NULL);
	desc = g_strdup_printf (_("show tooltips for %s buttons"), public_name);
	tip = g_strdup_printf(_("If deactivated, tooltips will not be displayed for %s buttons"), public_name);
	e2_option_bool_register (option_name, group_name, desc, tip, dep_name, data[i][2],
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREENAME
		| E2_OPTION_FLAG_FREEDESC | E2_OPTION_FLAG_FREETIP
		| E2_OPTION_FLAG_BUILDBARS);
	//restricted-space handling, j=3
	option_name = g_strconcat (name, "-space", NULL);
	tip = g_strdup_printf (_("This determines the method for accessing %s elements that are hidden due to lack of screen-space"), public_name);
	const gchar *opt_toolbar_space[] =
		{_("none"), _("use scrollbars"), _("use rest button"), NULL};
	e2_option_sel_register (option_name, group_name, _("space handling"), tip, dep_name, data[i][3], opt_toolbar_space,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREENAME
		| E2_OPTION_FLAG_FREETIP | E2_OPTION_FLAG_BUILDBARS);

	group_name = g_strconcat (shared_name, _C(39),  NULL);  //_(":style"
	//buttons always have relief, j=4
	option_name = g_strconcat (name, "-relief", NULL);
	desc = g_strconcat (public_name, _(" buttons have relief"), NULL);
	e2_option_bool_register (option_name, group_name, desc,
		_("Buttons with relief show their outline continually, not just when 'moused'"),
		dep_name, data[i][4],
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP
		| E2_OPTION_FLAG_FREENAME | E2_OPTION_FLAG_FREEDESC
		| E2_OPTION_FLAG_BUILDBARS);
	//buttons are same size, j=5
	option_name = g_strconcat (name, "-same", NULL);
	desc = g_strconcat (public_name, _(" buttons are the same size"), NULL);
	e2_option_bool_register (option_name, group_name, desc,
		_("Equal-width buttons look good on a vertical bar, bad on a horizontal bar"),
		dep_name, data[i][5],
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREENAME
		| E2_OPTION_FLAG_FREEDESC | E2_OPTION_FLAG_BUILDBARS);
	//buttons style, j=6
	option_name = g_strconcat (name, "-style", NULL);
	const gchar *opt_toolbar_style[] =
		{_("theme"),_("icon only"), _("label only"), _("icon above label"), _("icon beside label"), NULL};
	tip = g_strdup_printf (_("'%s' uses the Gtk default, '%s' leaves most space for other things"),
		opt_toolbar_style[0], opt_toolbar_style[1]);
	e2_option_sel_register (option_name, group_name, _("button style"), tip, dep_name,
		data[i][6], opt_toolbar_style,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREENAME
		| E2_OPTION_FLAG_FREETIP | E2_OPTION_FLAG_BUILDBARS);
	//buttons icon size, j=7
	option_name = g_strconcat (name, "-isize", NULL);
	const gchar *opt_toolbar_isize[] =
		{_("theme"),_("menu"), _("toolbar small"), _("toolbar large"), _("button"), _("dnd"), _("dialog"),NULL};
	tip = g_strdup_printf (_("'%s' uses the Gtk default, '%s' is smallest, '%s' is largest"),
		opt_toolbar_isize[0], opt_toolbar_isize[1], opt_toolbar_isize[6]);
		e2_option_sel_register (option_name, group_name, _("icon size"), tip,
		dep_name, data[i][7], opt_toolbar_isize,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREENAME
		| E2_OPTION_FLAG_FREETIP | E2_OPTION_FLAG_BUILDBARS);
	//bar type, j=8
	group_name = g_strconcat (shared_name,_C(36), NULL);  //_("<barname>:position"
	option_name = g_strconcat (name, "-type", NULL);
	desc = g_strconcat (public_name, _(" container"), NULL);
	tip = g_strdup_printf (_("This determines the 'box' into which the %s is placed"), public_name);
	const gchar *opt_toolbar_type[] =
		{_("main window"), _("both panes"), _("file-pane 1"), _("file-pane 2"), NULL};
	//this option cannot properly be conformed for both panes
	e2_option_sel_register (option_name, group_name, desc, tip, NULL, data[i][8], opt_toolbar_type,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP
		| E2_OPTION_FLAG_FREENAME  | E2_OPTION_FLAG_FREEDESC
		| E2_OPTION_FLAG_FREETIP | E2_OPTION_FLAG_BUILDALL);
	//bar placement, j=9
	option_name = g_strconcat (name, "-priority", NULL);
	desc = g_strconcat (public_name, _(" priority"), NULL);
	tip = _("This determines the order of toolbars (0 = left or top)");
	//this option cannot properly be conformed for both panes
	e2_option_int_register (option_name, group_name, desc, tip, NULL, data[i][9], 0, 10,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREENAME
		| E2_OPTION_FLAG_FREEDESC | E2_OPTION_FLAG_BUILDALL);

	g_free (shared_name);
}
/**
@brief install default tree options for taskbar
This function is called only if the default is missing from the config file
@param set pointer to set data
@return
*/
static void _e2_toolbar_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strconcat(app.bars[E2_BAR_TASK]->name,"=<",NULL),
	g_strconcat("|||",_A(0),".",_A(28),"|",_C(41),NULL),	//this arg needs to be same as in bookmarks.c
	g_strconcat("|||",_A(21),"|",NULL),
	g_strconcat(_("_Copy"),"|gtk-copy|",
		_("Copy items selected in the active pane to the other one"),"|",_A(6),".",_A(39),"|",NULL),
	g_strconcat(_("_Move"),"|move"E2ICONTB"|",
		_("Move items selected in the active pane to the other one"),"|",_A(6),".",_A(65),"|",NULL),
	g_strconcat(_("_Link"),"|symlink"E2ICONTB"|",
		_("Symlink items selected in the active pane to the other one"),"|",_A(6),".",_A(99),"|",NULL),
	g_strconcat(_("Re_name.."),"|gtk-convert|",
		_("Rename items selected in the active pane"),"|",_A(6),".",_A(79),"|",NULL),
	g_strconcat(_("_Trash"),"|trash"E2ICONTB"|",
		_("Move items selected in the active pane to a trashbin"),"|",_A(6),".",_A(18),"|",NULL),
	g_strconcat(_("Ma_ke dir.."),"|gtk-directory|",_("Create new directory(ies)"),"|",_A(1),".",_A(63),"|",NULL),
//	g_strconcat("|||",_A(21),"|",NULL),
//	g_strconcat("\\<span foreground='red'><b>",_("_Delete"),"</b></span>|gtk-delete|",
//		_("Delete items selected in the active pane"),"|",_A(6),".",_A(45),"|",NULL),
//	g_strconcat("|||",_A(21),"|",NULL),
	g_strconcat("|||",_A(21),"|",NULL),
	g_strconcat(_("Re_fresh"),"|gtk-refresh|",_("Update pane contents"),"|",_A(14),".",_A(76),"|",NULL),
	g_strconcat(_("_Switch"),"|switch"E2ICONTB"|",
		_("Toggle the active pane"),"|",_A(14),".",_A(98),"|",NULL),
	g_strdup(">"),
	NULL);
}
/**
@brief setup config options specific to the toolbar
In particular, this sets the default content of the bar
@return
*/
void e2_toolbar_toolbaroptions_register (void)
{
	const gchar *internal_name = app.bars[E2_BAR_TASK]->name;	//"taskbar", no translation
	const gchar *public_name = app.bars[E2_BAR_TASK]->public_name;
	E2_OptionSet *set = e2_option_tree_register ((gchar *)internal_name,
		(gchar *)public_name, (gchar *)public_name,
		NULL, NULL, NULL, E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDBARS);
	e2_option_tree_add_column (set, _("Label"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Icon"), E2_OPTION_TREE_TYPE_ICON, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Tooltip"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Action"), E2_OPTION_TREE_TYPE_SEL, 0, _A(21),
		0, NULL,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_TOOLBAR));
	e2_option_tree_add_column (set, _("Argument"), E2_OPTION_TREE_TYPE_SEL, 0, "",
		0, NULL,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_TOOLBAR
		| E2_ACTION_EXCLUDE_TOGGLE));
	e2_option_tree_create_store (set);

	e2_option_tree_prepare_defaults (set, _e2_toolbar_tree_defaults);

	e2_toolbar_options_register (E2_BAR_TASK);  //setup options relevant to all toolbars
}
/**
@brief install default tree options for commandbar
This function is called only if the default is missing from the config file
@param set pointer to set data
@return
*/
static void _e2_toolbar_cmdbar_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strconcat(app.bars[E2_BAR_COMMAND]->name,"=<",NULL),
	g_strconcat(_("Full"),"|gtk-zoom-fit|",_("Maximize output pane"),"|",_A(17),".",_A(118),"|",_A(10),".",_A(116),NULL),
	g_strconcat(_("Shrink"),"|gtk-zoom-out|",_("Un-maximize output pane"),"|",_A(17),".",_A(119),"|",_A(10),".",_A(116),NULL),
	g_strconcat(_("Hide"),"|output_hide"E2ICONTB"|",_("Hide output pane"),"|",_A(17),".",_A(118),"|",_A(10),".",_A(86),NULL),
	g_strconcat(_("Show"),"|output_show"E2ICONTB"|",_("Show output pane"),"|",_A(17),".",_A(119),"|",_A(10),".",_A(86),NULL),
	g_strconcat(_("Clear"),"|gtk-clear|",_("Clear output pane"),"|",_A(10),".",_A(36),"|",NULL),
	g_strconcat("|||",_A(1),".",_A(27),"|","100,*",NULL),  //1st arg is min size, integer, do not translate
	g_strconcat(_("cl"),"|cl_clear"E2ICONTB"|",_("Clear command line"),"|",_A(1),".",_A(36),"|",NULL),
	g_strconcat(_("ps"),"|ps"E2ICONTB"|",_("Child processes"),"|",_A(2),".",_A(28),"|",NULL),
	g_strconcat(_("du"),"|plugin_du"E2ICONTB"|",_("Calculate disk usage of selected items"),"|",_A(6),".",_("du"),"|",NULL),  //conformed to new internal name
	g_strconcat(_("_Find.."),"|gtk-find|",_("Find item in active pane, by name"),"|",_A(6),".",_A(49),"|",NULL),
	g_strconcat(_("_X"),"|terminal"E2ICONTB"|",_("Open terminal at the active directory"),"|$[command-xterm] &|",NULL), //_A(20)
#ifdef E2_POLKIT
	//see also _e2p_upgrade_su()
	g_strconcat(_("su.."),"|su"E2ICONTB"|",_("Run command as root"),"|pkexec|%{(root-commands)@",_("Enter command:"),"}",NULL),
#else
	//this command works ok with xterm, but not with gnome-terminal
//	g_strconcat(_("su"),"|su"E2ICONTB"|",_("Run command as root"),"|$[command-xterm]|-e 'su -c \"%{(root-commands)@",_("Enter command:"),"}\";echo -n \"",_("Done, Press enter..."),"\";read'",NULL), //_A(20)
	g_strconcat(_("su.."),"|su"E2ICONTB"|",_("Run command as root"),"|xterm|-e 'su -c \"%{(root-commands)@",_("Enter command:"),"}\";echo -n \"",_("Done. Press enter "),"\";read'",NULL), //_A(20)
#endif
#ifdef E2_FS_MOUNTABLE
	g_strconcat(_("mts.."),"|mounts"E2ICONTB"|",_("Mount or unmount a device"),"|",_A(64),".",_A(28),"|",_C(4),NULL),
#endif
	g_strconcat("|||",_A(21),"|",NULL),
	g_strconcat(_("_Settings.."),"|gtk-preferences|",_("View/change configuration settings for this program"),"|",_A(3),".",_A(34),"|",NULL),
	g_strconcat(_("_Help"),"|gtk-help|",_("Get information about this program"),"|",_A(4),".",_A(29),"|",NULL),
	g_strconcat("|||",_A(21),"|",NULL),
	g_strconcat(_("_Quit"),"|gtk-quit|",_("Close this program"),"|",_A(1),".",_A(75),"|",NULL),
	g_strdup(">"),
	NULL);
}
/**
@brief setup config options specific to the commandbar
In particular, this sets the default content of the bar
@return
*/
void e2_toolbar_commandbaroptions_register (void)
{
	const gchar *internal_name = app.bars[E2_BAR_COMMAND]->name;	// "commandbar", no translation
	const gchar *public_name = app.bars[E2_BAR_COMMAND]->public_name;
	E2_OptionSet *set = e2_option_tree_register ((gchar *)internal_name,
		(gchar *)public_name, (gchar *)public_name,
		 NULL, NULL, NULL, E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_BUILDBARS);
	e2_option_tree_add_column (set, _("Label"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Icon"), E2_OPTION_TREE_TYPE_ICON, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Tooltip"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Action"), E2_OPTION_TREE_TYPE_SEL, 0, _A(21),
		0, NULL,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_TOOLBAR));
	e2_option_tree_add_column (set, _("Argument"), E2_OPTION_TREE_TYPE_SEL, 0, "",
		0, NULL,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_TOOLBAR
		| E2_ACTION_EXCLUDE_TOGGLE));
	e2_option_tree_create_store (set);

	e2_option_tree_prepare_defaults (set, _e2_toolbar_cmdbar_tree_defaults);

	e2_toolbar_options_register (E2_BAR_COMMAND);  //setup options relevant to all toolbars
}
/**
@brief install default tree options for pane 1 toolbar
This function is called only if the default is missing from the config file
@param set pointer to set data
@return
*/
static void _e2_toolbar_bar1_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strconcat(app.bars[E2_BAR_PANE1]->name,"=<",NULL), // panebar1,
//NOTE - these 2 btns and their corresponding T/F flags are expected to be present
// (their labels/icons can vary) see code for selecting appropriate one of the pair at session start
	g_strconcat(_("_Panes"),"|gtk-zoom-fit|",_("Hide other pane"),"|",_A(17),".",_A(118),"|",_A(11),".",_A(116),NULL),	//no arg translation
#ifdef E2_PANES_HORIZONTAL
	g_strconcat(_("_Panes"),"|split_vert"E2ICONTB"|",
		_("Show other pane"),"|",_A(17),".",_A(119),"|",_A(11),".",_A(116),NULL),
#else
	g_strconcat(_("_Panes"),"|split_horiz"E2ICONTB"|",
		_("Show other pane"),"|",_A(17),".",_A(119),"|",_A(11),".",_A(116),NULL),
#endif
	g_strconcat(_("Show _hidden"),"|hidden_show"E2ICONTB"|",
		_("Display hidden items in this directory"),"|",_A(17),".",_A(118),"|",_A(11),".",_A(87),NULL),
	g_strconcat(_("Hide _hidden"),"|hidden_noshow"E2ICONTB"|",
		_("Do not display hidden items in this directory"),"|",_A(17),".",_A(119),"|",_A(11),".",_A(87),NULL),
	g_strconcat(_("Fil_ters"),"|filter"E2ICONTB"|",
		_("Set rules for the items to be displayed"),"|",_A(17),".",_A(119),"|",_A(11),".",_A(25),NULL),
	g_strconcat(_("Fil_ters"),"|filter_off"E2ICONTB"|",
		_("Set/remove rules for the items to be displayed"),"|",_A(17),".",_A(118),"|",_A(11),".", _A(25),NULL),
#ifdef E2_VFS
	g_strconcat(_("_VFS"),"|vfs_off"E2ICONTB"|",
		_("Show a virtual directory in this pane"),"|",_A(17),".",_A(119),"|",_A(11),".",_A(123),NULL),
	g_strconcat(_("_LocalFS"),"|vfs_on"E2ICONTB"|",
		"","|",_A(17),".",_A(118),"|",_A(11),".", _A(123),NULL),
#endif
#ifdef FOLDBARS
	g_strconcat("|||",_A(11),".",_A(27),"|","300,*",NULL),  //1st arg is min size, integer,
#else
	g_strconcat("|||",_A(11),".",_A(27),"|,*",NULL),
#endif
	g_strconcat(_("_Marks"),"|bookmark"E2ICONTB"|",
		_("Bookmarks"),"|",_A(22),"|",NULL),
	g_strconcat("\t",_("Add _top"),"|add_mark_top"E2ICONTB"|",
		_("Add the current directory to the top of the bookmarks list"),
		"|",_A(0),".",_A(31),"|1,",_A(122),NULL),
	g_strconcat("\t|||",_A(0),".",_A(28),"|",NULL),
	g_strconcat("\t",_("Add _bottom"),"|add_mark_bottom"E2ICONTB"|",
		_("Add the current directory to the bottom of the bookmarks list"),
		"|",_A(0),".",_A(31),"|1",NULL),
	g_strconcat("\t|||",_A(21),"|",NULL),
	g_strconcat("\t",_("_Edit bookmarks"),"|gtk-preferences|",
	_("Open the bookmarks configuration dialog"),"|",_A(3),".",_C(1),"|",NULL),

#ifdef E2_PANES_HORIZONTAL
	g_strconcat(_("M_irror"),"|mirror_vert"E2ICONTB"|",
		_("Go to directory shown in other pane"),"|",_A(13),".",_A(62),"|1",NULL),
#else
	g_strconcat(_("M_irror"),"|mirror_horiz"E2ICONTB"|",
		_("Go to directory shown in other pane"),"|",_A(13),".",_A(62),"|1",NULL),
#endif
	g_strconcat(_("_Back"),"|gtk-go-back|",_("Go to previous directory in history"),"|",_A(13),".",_A(52),"|1",NULL),
	g_strconcat(_("_Up"),"|gtk-go-up|",_("Go up to parent directory"),"|",_A(13),".",_A(54),"|1",NULL),
	g_strconcat(_("_Forward"),"|gtk-go-forward|",_("Go to next directory in history"),"|",_A(13),".",_A(53),"|1",NULL),
	g_strdup(">"),
	NULL);
}
/**
@brief install default tree options for pane 2 toolbar
This function is called only if the default is missing from the config file
@param set pointer to set data
@return
*/
static void _e2_toolbar_bar2_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strconcat(app.bars[E2_BAR_PANE2]->name,"=<",NULL), // panebar2,
//NOTE - these 2 btns and their corresponding T/F flags are expected to be present
// (text/icons can vary) see code for selecting appropriate one of the pair at session start
	g_strconcat(_("_Panes"),"|gtk-zoom-fit|",_("Hide other pane"),"|",_A(17),".",_A(118),"|",_A(12),".",_A(116),NULL),
#ifdef E2_PANES_HORIZONTAL
	g_strconcat(_("_Panes"),"|split_vert"E2ICONTB"|",
		_("Show other pane"),"|",_A(17),".",_A(119),"|",_A(12),".",_A(116),NULL),
#else
	g_strconcat(_("_Panes"),"|split_vert"E2ICONTB"|",
		_("Show other pane"),"|",_A(17),".",_A(119),"|",_A(12),".",_A(116),NULL),
#endif
	g_strconcat(_("Show _hidden"),"|hidden_show"E2ICONTB"|",
		_("Display hidden items in this directory"),"|",_A(17),".",_A(118),"|",_A(12),".",_A(87),NULL),
	g_strconcat(_("Hide _hidden"),"|hidden_noshow"E2ICONTB"|",
		_("Do not display hidden items in this directory"),"|",_A(17),".",_A(119),"|",_A(12),".",_A(87),NULL),
	g_strconcat(_("Fil_ters"),"|filter"E2ICONTB"|",
		_("Set rules for the items to be displayed"),"|",_A(17),".",_A(119),"|",_A(12),".",_A(25),NULL),
	g_strconcat(_("Fil_ters"),"|filter_off"E2ICONTB"|",
		_("Set/remove rules for the items to be displayed"),"|",_A(17),".",_A(118),"|",_A(12),".", _A(25),NULL),
#ifdef E2_VFS
	g_strconcat(_("_VFS"),"|vfs_off"E2ICONTB"|",
		_("Show a virtual directory in this pane"),"|",_A(17),".",_A(119),"|",_A(12),".",_A(123),NULL),
	g_strconcat(_("_LocalFS"),"|vfs_on"E2ICONTB"|",
		"","|",_A(17),".",_A(118),"|",_A(12),".", _A(123),NULL),
#endif
#ifdef FOLDBARS
	g_strconcat("|||",_A(12),".",_A(27),"|","300,*",NULL),  //1st arg is min size, integer,
#else
	g_strconcat("|||",_A(12),".",_A(27),"|,*",NULL),
#endif
	g_strconcat(_("_Marks"),"|bookmark"E2ICONTB"|",
		_("Bookmarks"),"|",_A(22),"|",NULL),
	g_strconcat("\t",_("Add _top"),"|add_mark_top"E2ICONTB"|",
		_("Add the current directory to the top of the bookmarks list"),
		"|",_A(0),".",_A(31),"|2,",_A(122),NULL),
	g_strconcat("\t|||",_A(0),".",_A(28),"|",NULL),
	g_strconcat("\t",_("Add _bottom"),"|add_mark_bottom"E2ICONTB"|",
		_("Add the current directory to the bottom of the bookmarks list"),
		"|",_A(0),".",_A(31),"|2",NULL),
	g_strconcat("\t|||",_A(21),"|",NULL),
	g_strconcat("\t",_("_Edit bookmarks"),"|gtk-preferences|",
	_("Open the bookmarks configuration dialog"),"|",_A(3),".",_C(1),"|",NULL),

#ifdef E2_PANES_HORIZONTAL
	g_strconcat(_("M_irror"),"|mirror_vert"E2ICONTB"|",
		_("Go to directory shown in other pane"),"|",_A(13),".",_A(62),"|2",NULL),
	g_strconcat(_("_Back"),"|gtk-go-back|",_("Go to previous directory in history"),"|",_A(13),".",_A(52),"|2",NULL),
	g_strconcat(_("_Up"),"|gtk-go-up|",_("Go up to parent directory"),"|",_A(13),".",_A(54),"|2",NULL),
	g_strconcat(_("_Forward"),"|gtk-go-forward|",_("Go to next directory in history"),"|",_A(13),".",_A(53),"|2",NULL),
#else
	g_strconcat(_("M_irror"),"|mirror_horiz"E2ICONTB"|",
		_("Go to directory shown in other pane"),"|",_A(13),".",_A(62),"|2",NULL),
	g_strconcat(_("_Forward"),"|gtk-go-forward|",_("Go to next directory in history"),"|",_A(13),".",_A(53),"|2",NULL),
	g_strconcat(_("_Up"),"|gtk-go-up|",_("Go up to parent directory"),"|",_A(13),".",_A(54),"|2",NULL),
	g_strconcat(_("_Back"),"|gtk-go-back|",_("Go to previous directory in history"),"|",_A(13),".",_A(52),"|2",NULL),
#endif
	g_strdup(">"),
	NULL);
}
/**
@brief setup tree option for a pane bar

@return
*/
void e2_toolbar_panebar_register (gint num)
{
	gchar *internal_name = g_strdup_printf ("panebar%d", num);
	gchar *public_name = (num == 1) ? _C(30) : _C(32);
	gchar *group_name = public_name;
	E2_OptionSet *set = e2_option_tree_register (internal_name, group_name, public_name,
		NULL, NULL, NULL, E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREENAME
		| E2_OPTION_FLAG_BUILDBARS);
	e2_option_tree_add_column (set, _("Label"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Icon"), E2_OPTION_TREE_TYPE_ICON, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Tooltip"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_add_column (set, _("Action"), E2_OPTION_TREE_TYPE_SEL, 0, _A(21),
		0, NULL,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_TOOLBAR));
	e2_option_tree_add_column (set, _("Argument"), E2_OPTION_TREE_TYPE_SEL, 0, "",
		0, NULL,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_TOOLBAR
		| E2_ACTION_EXCLUDE_TOGGLE));
	e2_option_tree_create_store (set);

	void (*func) = (num == 1) ? _e2_toolbar_bar1_tree_defaults :
		_e2_toolbar_bar2_tree_defaults ;
	e2_option_tree_prepare_defaults (set, func);
}
