/* $Id: e2_option_tree.h 2469 2012-03-16 00:19:24Z tpgww $

Copyright (C) 2004-2012 tooar <tooar@emelfm2.net>

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

#ifndef __E2_OPTION_TREE_H__
#define __E2_OPTION_TREE_H__

#include "emelfm2.h"
#include "e2_option.h"

typedef enum
{
//	E2_OPTION_TREE_TYPE_INT,
	E2_OPTION_TREE_TYPE_BOOL,
	E2_OPTION_TREE_TYPE_STR,
	E2_OPTION_TREE_TYPE_ICON,
	E2_OPTION_TREE_TYPE_SEL,
	E2_OPTION_TREE_TYPE_KEY,
#ifdef E2_MOUSECUSTOM
	E2_OPTION_TREE_TYPE_BUTTON,
# ifdef E2_PTRGESTURES
	E2_OPTION_TREE_TYPE_GESTURE,
# endif
#endif
//	E2_OPTION_TREE_TYPE_HIDDENINT,
	E2_OPTION_TREE_TYPE_HIDDENBOOL, //stored but not displayed in option treeview
	E2_OPTION_TREE_TYPE_HIDDENSTR //ditto
} E2_OptionTreeType;

//these are flags that may be set for a tree-option column when that column is
//added to the tree option data, stored in GList at respective
//((set->ex.tree.columns)->data)->flags
typedef enum
{
	E2_OPTION_TREE_COL_NOT_EDITABLE = 1<< 0,
	//flags for things to free when the config data is dumped
	E2_OPTION_TREE_COL_FREENAME = 1 << 8, //set to free label
	E2_OPTION_TREE_COL_FREESTRING = 1 << 9, //set to free sdef
//	E2_OPTION_TREE_COL_FREEEXTRA = 1 << 10, //UNUSED
//	E2_OPTION_TREE_COL_FREEXTRADATA = 1 << 11, //UNUSED
} E2_OptionTreeColFlags;
//data struct for each tree-option column.
typedef struct _E2_OptionTreeColumn
{
	gchar *label;	//config-dialog column header label
	E2_OptionTreeType type; //enumerator of type of data in the column
	gint idef;	//default boolean value for the column, if relevant
	gchar *sdef;	//default string for the column, if relevant
	//flags that may be set when a column is added to a tree option
	E2_OptionTreeColFlags flags;
	//function to call for checking whether to display a cell in the column
	//gboolean (*fun) (GtkTreeModel *, GtkTreeIter *, GtkCellRenderer *, gpointer)
	//for sel-columns, NULL
	gpointer visible_check_func;
	//data to supply to visible_check_func()
	//OR for sel-columns, a set of flags to supply to the filtermodel visibility func
	gpointer visible_check_data;
//	GDestroyNotify data_cleaner;
} E2_OptionTreeColumn;

void e2_option_tree_flag_change (E2_OptionSet *set);
void e2_option_tree_adjust_buttons (GtkTreeView *view, gboolean value);
void e2_option_tree_add_widget (GtkWidget *parent, gboolean single, GtkWidget *box,
	E2_OptionSet *set);
E2_OptionSet *e2_option_tree_register (gchar *name, gchar *group, gchar *desc,
	gchar *depends, gpointer selection_check_func, gpointer draggable_check_func,
	E2_OptionTreeTypeFlags flags, E2_OptionFlags flags2);
void e2_option_tree_add_column (E2_OptionSet *set, gchar *name,
	E2_OptionTreeType type, gint idef, gchar *sdef,
	E2_OptionTreeColFlags flags,
	gpointer visible_check_func, gpointer visible_check_data);
void e2_option_tree_create_store (E2_OptionSet *set);
void e2_option_tree_connect_mnemonics (GtkWidget *button_box);
void e2_option_tree_revert (gchar *name);
E2_OptionSet *e2_option_tree_get (gchar *option);
void e2_option_tree_add_simple (gchar *option, GtkTreeIter *iter, gint n_options,
	void *options[]);
void e2_option_tree_add (E2_OptionSet *set, GtkTreeIter *iter,
	GtkTreeIter *parent, gboolean sibling, gboolean before, gint n_options,
	void *options[]);
void e2_option_tree_add_default (gchar *option, GtkTreeIter *iter,
	GtkTreeIter *parent, gboolean sibling);

void e2_option_tree_del (gchar *option, GtkTreeIter *iter);
void e2_option_tree_del_direct (E2_OptionSet *set, GtkTreeIter *iter);

void e2_option_tree_set (gchar *option, GtkTreeIter *iter, gint col,
	void *data);
void e2_option_tree_backup (E2_OptionSet *set);
void e2_option_tree_unbackup (E2_OptionSet *set, gboolean revert);
void e2_option_tree_restore_all (void);
void e2_option_tree_prepare_defaults (E2_OptionSet *set, void (*func)(E2_OptionSet*));
void e2_option_tree_setup_defaults (E2_OptionSet *set, gchar *first, ...)
#ifdef G_GNUC_NULL_TERMINATED
    G_GNUC_NULL_TERMINATED
#endif
;
void e2_option_tree_install_defaults (void);
gboolean e2_option_tree_set_from_array (gchar *name, gchar *f[], gint *index,
	GtkTreeIter *root_iter);
gchar *e2_option_tree_row_write_to_string (E2_OptionSet *set,
	GtkTreeIter *iter, gint level);

//this needs to be in header e2_fs.h, to prevent build problems
//related to E2_FILE
//void e2_option_tree_write_to_file (E2_FILE *f, E2_OptionSet *set,
//	GtkTreeIter *iter, gint level);

// context menu things

typedef enum
{
	E2_TREE_CONTEXT_DEFAULT=1<<0,
	E2_TREE_CONTEXT_CP_PST=1<<1,
	E2_TREE_CONTEXT_EXP_COL=1<<2,
} E2_TreeContextMenuFlags;

GHashTable *tree_view_buffer_hash;

void e2_option_tree_menu_hash_clean (GList *list);
void e2_option_tree_menu_set_sensitive (GtkWidget *menu,
	GtkWidget *treeview);

#endif //ndef __E2_OPTION_TREE_H__
