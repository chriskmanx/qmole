/* $Id: e2_toolbar.h 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2003-2010 tooar <tooar@emelfm2.net>

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

#ifndef __E2_TOOLBAR_H__
#define __E2_TOOLBAR_H__

#include "emelfm2.h"
#include "e2_action.h"
#include "e2_option.h"
#include "e2_fileview.h"

#define FOLDBARS

typedef enum
{	//these are ordered for decreasing visual impact when changes are made
	//if order is changed, correspondingly change order of array rows in
	//e2_toolbar_options_register()
	E2_BAR_TASK,
	E2_BAR_PANE1,
	E2_BAR_PANE2,
	E2_BAR_COMMAND,
	E2_BAR_COUNT	//the no. of bars
} E2_BarType;

typedef struct _E2_ToolbarRuntime
{
	const gchar *name;	//'internal' name of bar, for referencing
//	const gchar *public_name;	//translated form of name
	GtkToolbar *toolbar;
	GtkWidget *toolbar_container;
	GtkWidget *toolbar_container_box;
#ifdef FOLDBARS
	GtkWidget *toolbar_foldbox;	//vbox holding toolbar_container
								// & sometimes a separate commandline tool
#endif
	volatile gint blocked;  //non-0 when bar's context menu is being created
	gboolean hidden;
	E2_OptionSet *set;
	E2_OptionSet *show;
	E2_OptionSet *tooltips;
	E2_OptionSet *space;
	E2_OptionSet *hori;
	E2_OptionSet *type;
	E2_OptionSet *priority;
	E2_OptionSet *relief;
	E2_OptionSet *same;
	E2_OptionSet *style;
	E2_OptionSet *isize;
	GtkIconSize icon_size;	//working copy of size enumerator (supports 'default')
	GtkAllocation *alloc;
	GtkRequisition req;
	gboolean has_toggle;
	gboolean has_bookmarks;
	gboolean has_command_line;
	gboolean restbtn_shown;
	GtkWidget *button_rest;
#ifdef FOLDBARS
	gint restbtn_width;	//size to use if folded bars in force
	GtkToolItem *dirline_tool;	//tool item for the movable dirline widget
	gint dirline_index;	//index of dirline_tool in the toolbar
	gboolean folded;	//TRUE when dirline_tool is in separate box
//	gint threshold;	//pixel size when we swap to/from separate box
	gboolean size_queued;	//TRUE to block repeated resizes before redrew
#endif
	GList *bar_items_list;  //list of toolbar buttons (not spacers etc), including hidden toggles
	GtkWidget *menu_starter;	//bar item that is first in the overflow menu
	gboolean reversed;	//TRUE if the config-order of bar items is reversed onscreen
} E2_ToolbarRuntime;

typedef struct _E2_ToolbarData
{
	E2_BarType type;
	const gchar *name;	//internal name, not translated
	const gchar *public_name;	//translated name
	E2_ToolbarRuntime *rt;	//data struct for the bar
} E2_ToolbarData;

// this is 'extended' data for toggle actions, stored at action.data2
typedef struct _E2_ToggleData
{
	gboolean current_state;
	GList *boxes;	//list of E2_ToggleBox's for this action
	gchar *true_action;	//action command string
	gchar *false_action; //ditto
} E2_ToggleData;

typedef struct _E2_ToggleBox
{
	E2_ToolbarRuntime *bar_rt;  //ptr to data for the bar that includes this (instance of a toggle
	GtkWidget *true_image;
	GtkWidget *false_image;
	GtkWidget *true_label;
	GtkWidget *false_label;
#ifdef USE_GTK2_12TIPS
	GtkToolItem *tool;
	gboolean tipped;
	gboolean trueactive;
	gchar *true_tip;
	gchar *false_tip;
#else
	GtkTooltipsData *tips;
#endif
	gint button_style;
} E2_ToggleBox;

//index for "self-managed" toggle actions
typedef enum
{	E2_TOGGLE_PANE1FULL,
	E2_TOGGLE_PANE2FULL,
	E2_TOGGLE_PANE1HIDDEN,
	E2_TOGGLE_PANE2HIDDEN,
	E2_TOGGLE_PANE1FILTERS,
	E2_TOGGLE_PANE2FILTERS,
#ifdef E2_VFS
	E2_TOGGLE_PANE1SPACE,
	E2_TOGGLE_PANE2SPACE,
#endif
	E2_TOGGLE_OUTPUTFULL,
	E2_TOGGLE_OUTPUTSHADE,
	E2_TOGGLE_COUNT	//this is the no. of actions, not a 'type'
} E2_ToggleType;

gchar *toggles_array [E2_TOGGLE_COUNT];

void e2_toolbar_rebadge (E2_ToolbarRuntime *rt);
void e2_toolbar_initialise_space_handler (E2_ToolbarRuntime *rt);
void e2_toolbar_set_menu_position (GtkMenu *menu, gint *x, gint *y,
	gboolean *push_in, GtkWidget *button);
gboolean e2_toolbar_button_toggle (gchar *action_name);
gboolean e2_toolbar_button_toggle_custom (gchar *hashkey);
gboolean e2_toolbar_toggle_button_get_state (gchar *action_name);
void e2_toolbar_toggle_button_set_state (gchar *action_name, gboolean state);
void e2_toolbar_toggle_buttons_set_destroyed (E2_ToolbarRuntime *rt);
void e2_toolbar_toggle_filter_button (ViewInfo *view);
//void e2_toolbar_initialise (E2_BarType barnum);
void e2_toolbar_create (E2_ToolbarRuntime *rt);
void e2_toolbar_recreate (E2_ToolbarRuntime *rt);
void e2_toolbar_recreate_all (void);
void e2_toolbar_destroy (E2_ToolbarRuntime *rt);
void e2_toolbar_data_create (void);
void e2_toolbar_data_clean (void);
void e2_toolbar_actions_register (void);
void e2_toolbar_options_register (E2_BarType barnum);
void e2_toolbar_toolbaroptions_register (void);
void e2_toolbar_commandbaroptions_register (void);
void e2_toolbar_panebar_register (gint num);

#endif //ndef __E2_TOOLBAR_H__
