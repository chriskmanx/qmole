/* $Id: e2_option.h 3046 2014-02-08 21:40:24Z tpgww $

Copyright (C) 2003-2011 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#ifndef __E2_OPTION_H__
#define __E2_OPTION_H__

#include "emelfm2.h"

typedef enum
{
	E2_TREE_STARTED,
	E2_TREE_COMPLETED,
	E2_TREE_ABORTED,
	E2_TREE_IGNORED,
	E2_TREE_UNKNOWN
} E2_TreeStatus;

//simple types, (bits, for easier checks of multiple types)
typedef enum
{
	E2_OPTION_TYPE_BOOL  = 1,
	E2_OPTION_TYPE_INT   = 1 << 1,
	E2_OPTION_TYPE_STR   = 1 << 2,
	E2_OPTION_TYPE_SEL   = 1 << 3,
	E2_OPTION_TYPE_FONT  = 1 << 4,
	E2_OPTION_TYPE_COLOR = 1 << 5,
	E2_OPTION_TYPE_FILE  = 1 << 6,	//UNUSED
	E2_OPTION_TYPE_ICON  = 1 << 7,
	E2_OPTION_TYPE_TREE  = 1 << 8,
} E2_OptionType;

//these are flags that may be set when a tree option is registered
//stored at set->ex.tree.flags
typedef enum
{
	E2_OPTION_TREE_UP_DOWN = 1, //!< enable config dialog buttons for moving a row up or down
	E2_OPTION_TREE_ADD_DEL = 1 << 1,  //!< enable config dialog buttons for adding and deleting a row
	E2_OPTION_TREE_LAST_COL_EMPTY = 1 << 2, //!< add an untitled column as the last one in a config dialog page treeview 
	E2_OPTION_TREE_LIST = 1 << 3, //!< pretend the config data are in a liststore - no buttons for adding children etc

	//this is set/cleared in or around a config dialog, as a signal
	//that the set has been edited
	E2_OPTION_TREE_SET_EDITED = 1<< 8,

} E2_OptionTreeTypeFlags;

typedef enum
{
	E2_OPTION_FLAG_BASIC = 1,
	E2_OPTION_FLAG_BASICONLY = 1 << 1,
	E2_OPTION_FLAG_ADVANCED = 1 << 2,
	E2_OPTION_FLAG_HIDDEN = 1 << 3,	//for items not shown in config dialog
	//signals for things to free when config data is dumped
	E2_OPTION_FLAG_FREENAME = 1 << 4,
	E2_OPTION_FLAG_FREEGROUP = 1 << 5,
	E2_OPTION_FLAG_FREEDESC = 1 << 6,
	E2_OPTION_FLAG_FREETIP = 1 << 7,
	E2_OPTION_FLAG_FREEDEPENDS = 1 << 8,
	//flags for things to rebuild when config dialog is completed
	E2_OPTION_FLAG_BUILDICONS = 1 << 9, //clear icons cache
	E2_OPTION_FLAG_BUILDALL = 1 << 10,
	E2_OPTION_FLAG_BUILDPANES = 1 << 11,
	E2_OPTION_FLAG_BUILDLISTS = 1 << 12,
	E2_OPTION_FLAG_BUILDOUT = 1 << 13,  //change output param without losing content
	E2_OPTION_FLAG_BUILDBARS = 1 << 14,
	E2_OPTION_FLAG_BUILDSAMEBARS = 1 << 15,
	//flags for 'extra' data structures to possibly rebuild when config dialog is completed
	E2_OPTION_FLAG_BUILDFILES = 1 << 16, //dirty filetypes tree-option
	E2_OPTION_FLAG_BUILDPLUGS = 1 << 17, //dirty plugins tree-option
	E2_OPTION_FLAG_BUILDKEYS = 1 << 18, //dirty keybindings tree-option
	E2_OPTION_FLAG_BUILDBUTTONS = 1 << 19, //dirty button-bindings tree option
	E2_OPTION_FLAG_BUILDALIAS = 1 << 20, //dirty aliases tree-option
} E2_OptionFlags;

typedef struct _E2_OptionTypeExtraColor
{
	GdkColor value;
} E2_OptionTypeExtraColor;

typedef struct _E2_OptionTypeExtraInt
{
	gint min;
	gint max;
} E2_OptionTypeExtraInt;

typedef struct _E2_OptionTypeExtraSel
{
	gchar **def;
	gint def_count;
} E2_OptionTypeExtraSel;

typedef struct _E2_OptionSet E2_OptionSet;

//stored as set->ex.tree
typedef struct _E2_OptionTypeExtraTree
{
	gpointer model;
	//shared store: at session start, set to func which is called to install
	//default tree values if needed.
	//Later, can be used for tree_strings, a NULL-terminated array of
	//string-pointers, each addressing a line of backed-up values for the tree
	union
	{
		void (*func)(E2_OptionSet*);
		gchar **tree_strings;
	} def;
	//holds tree values in case the config file has been read before the tree
	//option was registered
//	gchar **unknown;
//	gint def_num;
	gboolean synced;	//TRUE when string data converted to treestore
	gint columns_num;
	GList *columns;
	gpointer selection_check_func;
	gpointer draggable_check_func;
	E2_OptionTreeTypeFlags flags; //flags set when registering a tree option
} E2_OptionTypeExtraTree;

struct _E2_OptionSet
{
	E2_OptionType type;
	gchar *name;	//'internal' name of option, used for checking, finding
	gchar *group;	//config dialog group
	gchar *desc;	//config dialog label
	gchar *tip;		//config dialog tooltip
	gchar *depends;	//'internal' name of an option that must be true for this
					//one to be 'changeable', or can start with "!" for false precedent
	gint ival;
	gchar *sval;

	//extra option data
	union _E2_OptionTypeExtra
	{
		E2_OptionTypeExtraInt num;
		E2_OptionTypeExtraSel sel;
		E2_OptionTypeExtraTree tree;
		E2_OptionTypeExtraColor color;
	} ex;
	GtkWidget *widget;	//config dialog widget with data for this option
	E2_OptionFlags flags;	//flags for destroying, rebuilding stuff related to the option
	gboolean hook_freezed;	//TRUE to block running of anything in hooklist
	GHookList hook_value_changed;
};

typedef	union _E2_OptionSetupExtra
{
	gboolean exbool;
	gchar *exstr;
//	GdkColor excol;
	struct
	{
		gint def;
		gint min;
		gint max;
	} exint;
	struct
	{
		gint def;
		const gchar **values;
	} exsel;
//	? extree;
} E2_OptionSetupExtra;

typedef gchar *_config_labels[44];	//make sure there's enough for all array items
_config_labels config_labels;
#define _C(d) config_labels[d]

gchar *default_config_file;

GPtrArray *options_array;	//for processing options in order they were registered
GHashTable *options_hash;	//for quick lookups, data shared with options_array
GHashTable *options_queue;	//for not-yet-registed options

//void e2_option_clean1 (E2_OptionSet *set);
void e2_option_setup_labels (void);
gboolean e2_option_set_config_dir (void);
gboolean e2_option_check_config_files (gpointer user_data);
void e2_option_stop_config_checks (void);
void e2_option_start_config_checks (void);
void e2_option_disable_config_checks (void);
void e2_option_enable_config_checks (void);
void e2_option_refresh (gboolean reload, gboolean recreate);
void e2_option_set_trash_dir (void);
void e2_option_init (void);
gboolean e2_option_set_from_string (gchar *option, gchar *str);
gboolean e2_option_set_value_from_string (E2_OptionSet *set, gchar *str);
void e2_option_read_array (gchar *f[]);
//void e2_option_tree_stores_clear (void);
void e2_option_clear_data (void);
E2_OptionSet *e2_option_register (E2_OptionType type, gchar *name, gchar *group,
	gchar *desc, gchar *tip, gchar *depends, E2_OptionFlags flags);
//gboolean e2_option_unregister (gchar *name);
gboolean e2_option_backup (gchar *name);
E2_OptionSet *e2_option_get (const gchar *option);
E2_OptionSet *e2_option_get_simple (const gchar *option);
//void *e2_option_void_get (E2_OptionSet *set, gchar *option);
E2_OptionSet *e2_option_attach_value_changed (gchar *option, GtkWidget *widget,
	gboolean (*func)(gpointer,gpointer), gpointer data);
void e2_option_attach_value_changed_simple (E2_OptionSet *set, GtkWidget *widget,
	gboolean (*func)(gpointer,gpointer), gpointer data);
void e2_option_connect (GtkWidget *controller, gboolean  active);
gboolean e2_option_file_read (const gchar *config_dir);
void e2_option_file_write (const gchar *utfpath);

//void e2_option_destroy_config ();
void e2_option_date_style (void);

#include "e2_option_bool.h"
#include "e2_option_int.h"
#include "e2_option_str.h"
#include "e2_option_sel.h"
#include "e2_option_color.h"
#include "e2_option_tree.h"
#include "e2_option_unknown.h"

  /***********************************************/
 /***** functions from e2_option__default.c *****/
/***********************************************/

void e2_option_default_register (void);

#endif //ndef __E2_OPTION_H__
