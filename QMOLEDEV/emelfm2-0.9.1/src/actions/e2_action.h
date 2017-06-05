/* $Id: e2_action.h 2914 2013-11-13 01:06:23Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

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
@file src/actions/e2_action.h
@brief action system header

This is the header file for the action system.
*/

#ifndef __E2_ACTION_H__
#define __E2_ACTION_H__

//these guide the creation of toolbars and menus
typedef enum
{
	E2_ACTION_TYPE_ITEM,	//all "non-special" (pseudo)actions default to this
	E2_ACTION_TYPE_SUBMENU,
	E2_ACTION_TYPE_FILE_ACTIONS,	//this is a context-menu widget
	E2_ACTION_TYPE_FILE_HANDLERS,	//this is another context-menu widget
	E2_ACTION_TYPE_CUSTOM_COMMAND,	//UNUSED
	E2_ACTION_TYPE_PLUGINS,
	E2_ACTION_TYPE_SEPARATOR,
	E2_ACTION_TYPE_TEAR_OFF_MENU,
	E2_ACTION_TYPE_TOGGLE,
	E2_ACTION_TYPE_COMMAND_LINE,
	E2_ACTION_TYPE_BOOKMARKS,
	E2_ACTION_TYPE_HISTORY,
	E2_ACTION_TYPE_CHECK_ITEM,  //UNUSED
	E2_ACTION_TYPE_FILTERS,	//UNUSED
	E2_ACTION_TYPE_HOVER,	//see notes below
	E2_ACTION_TYPE_DUMMY
} E2_ACTION_TYPE;

/*
E2_ACTION_TYPE_HOVER is the same as E2_ACTION_TYPE_ITEM except that when used
with a toolbar-button, that supports a callback when hovered-over.
For such actions, the action data must be a E2Callback*, in which the function
must be a HoverCallback*, and the action cleaner (if actions support that) must
be suitable for E2Callback.
*/

//flags for controlling display and/or use of actions
typedef enum
{
	E2_ACTION_EXCLUDE_GENERAL = 1 << 0,
	E2_ACTION_EXCLUDE_TOOLBAR = 1 << 1,	//not allowed as a toolitem action
	E2_ACTION_EXCLUDE_MENU = 1 << 2,	//not allowed as a menu-item action
	E2_ACTION_EXCLUDE_LAYOUT = 1 << 3,	//this is one of the pseudo-actions
	E2_ACTION_EXCLUDE_ACCEL = 1 << 4,	//not allowed as a key-binding or command
	E2_ACTION_EXCLUDE_POINTER = 1 << 5,	//not allowed as a button- or gesture-binding
	E2_ACTION_EXCLUDE_TOGGLE = 1 << 6,

	E2_ACTION_INCLUDE_FILES = 1 << 16,
} E2_ACTION_EXCLUDE;
//all of the above
#define E2_ACTION_EXCLUDE_ALL 0x3f

//MALLOCATE these (too small for ALLOCATE)
typedef struct _E2_Callback
{
	void (*callback) ();
	gpointer data;
} E2_Callback;

//details for E2_ACTION_TYPE_HOVER actions
typedef struct _E2HoverData E2HoverData;
typedef void (HoverCallback) (GtkWidget*, E2HoverData*);

struct _E2HoverData
{
	guint timer_id; //source for timeout until the hover is acted upon
	GtkWidget *hovered; //the widget which has triggered the hover, or NULL
	HoverCallback *callback;
	gpointer data;	//callback data
	GtkWidget *menu;  //menu created, and possibly shown, by callback
};

typedef enum
{
	E2_TOGGLE_NONE,
	E2_TOGGLE_INIT,
	E2_TOGGLE_DESTROYED,
	E2_TOGGLE_REINIT,
} E2_TOGGLE_STATUS;

typedef struct _E2_ActionRuntime E2_ActionRuntime;

typedef struct _E2_Action
{
	gchar *name;	//string UTF-8, translated, (typically like A.B) which
					//identifies (i.e. actions hash key) and/or runs the action
	gboolean (*func) (gpointer, E2_ActionRuntime*);	//function to run when action is initiated
	gboolean has_arg;	//TRUE if action-string, in config data etc, must or may
						//include context-specific argument(s) UNUSED!
	E2_ACTION_TYPE type;//enumerator which instructs toolbar creation
	E2_ACTION_EXCLUDE exclude;	//flags controlling use and display of the action
	gpointer data;	//data specified when action was registered
//	gpointer data2; //more such data eg for toolbar usage
	GtkTreeRowReference *ref;	//reference to action's row in actions_store
//	GDestroyNotify cleaner; //func to cleanup data
} E2_Action;

//overloaded event state with embedded button-number
typedef GdkModifierType E2_ActionState;

struct _E2_ActionRuntime
{
	E2_Action *action;
	gpointer data;
	void (*data_free)(gpointer);
	E2_ActionState state;
};

//macros for manipulating action state
//bits 8-14 (in gtk these are GDK_BUTTON1_MASK etc and used by XKB) are used for
//number 0 to 127, of which 0 is for a key-event, 1-127 are available for buttons etc
#define ACTION_BUTTON(a,n) (((a->state>>8)&0x7f)==n)
#define ACTION_SETSTATE(s,b) s&=~(0x7f<<8);s|=(b&0x7f)<<8

#define ACTION_MASK(a,f) ((a->state&E2_MODIFIER_MASK)==f)
#define ACTION_INMASK(a,f) ((a->state&f)>0)
//bare button 1 or 3
#define ACTION_CLICK(a) (a->state==GDK_BUTTON1_MASK || a->state==(GDK_BUTTON1_MASK|GDK_BUTTON2_MASK))

GHashTable *toggles_hash;
#ifdef E2_TREEDIALOG
//make sure that the array size works for the extra action label(s)
//_A(109) may be used
#endif
#ifdef WITH_BUTTONFAKE
//KEYFAKE number + 1
# define ACTION_LABEL_COUNT 130
#elif defined(WITH_KEYFAKE)
//make sure this is big enough for all labels, 2 extra for this option
# define ACTION_LABEL_COUNT 130
//124
#else
//make sure this is big enough for all labels
# define ACTION_LABEL_COUNT 130
//122
#endif
#define ALLOCATED_NAMES 130
typedef gchar *_action_labels[ACTION_LABEL_COUNT];
_action_labels action_labels;
#define _A(d) action_labels[d]

GtkTreeModel *e2_action_filter_store (gpointer data);
E2_Action *e2_action_register (const E2_Action *newaction) G_GNUC_MALLOC;
gboolean e2_action_unregister (const gchar *name);
E2_Action *e2_action_get (const gchar *name);
E2_Action *e2_action_get_with_custom (const gchar *name, const gchar *arg, gchar **use_arg);
E2_Action* e2_action_check (gchar *command);
#ifdef E2_VFS
gboolean e2_action_inaction (gpointer from, E2_ActionRuntime *art);
#endif
E2_ActionState e2_action_get_current_state (gpointer from);
//void e2_action_run_cb (GtkWidget *from, E2_ActionRuntime *rt);
//#define e2_action_run_cb e2_action_run
gboolean e2_action_run_simple_from (const gchar *name, gpointer arg, gpointer from);
gboolean e2_action_run (gpointer from, E2_ActionRuntime *rt);
E2_ActionRuntime *e2_action_pack_runtime (E2_Action *action, gpointer data,
	void(*data_free)(gpointer)) G_GNUC_MALLOC;
void e2_action_free_runtime (E2_ActionRuntime *rt);
gboolean e2_action_start_hover_cb (GtkWidget *widget, GdkEventCrossing *event,
		E2HoverData *data);
gboolean e2_action_end_hover_cb (GtkWidget *widget, GdkEventCrossing *event,
		E2HoverData *data);

//initialization things
void e2_action_setup_labels(void);
void e2_actions_init (void);

#endif //ndef __E2_ACTION_H__
