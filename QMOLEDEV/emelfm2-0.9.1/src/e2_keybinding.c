/* $Id: e2_keybinding.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

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
@file src/e2_keybinding.c
@brief functions for handling key bindings
*/

/**
\page bindings key bindings

The primary data for key-bindings are stored in a treestore established
at session-start. Its hierarchy currently provides for 3 levels of "category"
(more levels can be defined at compile-time). One or more widgets can be bound
to any category. One or more categories can be bound to any widget. Any level
can be bound to the widget. Any key can be assigned to any level. Keys assigned
to a category apply to all widgets in that category, and to all widgets in any
descendant category, unless and until pre-empted by another assignment of the
same key in a descendant category.

After the last level of categories in any tree branch, the next level of the
hierarchy has string-form key descriptions and related data. Any key may have
child(ren), grand-child(ren) etc. Such descendant-keys are treated as chained,
i.e key-sequences can be created. Any key in a sequence, or more than one of
them (i.e. not just the last one) can have its own bound action.

Categories can be added to the keys treestore at any time and in any order,
from strings like "1.2.3", with or without any keys. A category must, and its
contents may, be added to the treestore before the category is bound to any
widget. When a category is first bound to any widget, the keys in the category
(if any) are prepared for use - see below. Keys can be added to a category
later, provided they are specifically converted to runtime format.

Treestore data are converted to a tree (keybindings) for runtime access. Each
member of keybindings corresponds to a binding category. Among other things,
the member has a list of bound widgets, and a list of key-data structs. In
the latter, each member has specific key data and the bound action/command.
A category can be "registered" (added to runtime tree) at any time, and any
prior rt data for the category will then be destroyed.

Category data for "core" widgets are destroyed and rebuilt as appropriate
during a config dialog. As of now, there are no "transient" bindings. They
probably don't need to be rebuilt, anyhow.

Category names must be set at compile time, as the relevant widgets need to know
the names to which to bind. In theory at least, category names could be made
configurable. If so, it would be reasonable to also allow creation and deletion
of categories in a config dialog.

Binding a category to a widget means that a key-press-event on the widget will
callback to scan the keys in the bound group and all its ancestor bindings in
turn, initiate or continue a chaining process if appropriate, or otherwise act
on the first-found matched binding.

Look for code tagged with #ifdef E2_TRANSIENTBINDINGS, and in particular a
dummy set of bindings and their initialization and cancellation, in
e2_edit_dialog.c

One or more keypresses may be 'aliased' by binding them to the action 'key.fake'.
The argument for that action must be a string containing one or more key-names
in a form gtk can parse, like <Ctrl><Alt>m, or just a letter. If more than
one key is to be faked, they must be separated by a ' ' char (a substituted
space-character is identified by: space (not Space))
*/
/**
\page bindings_ui key bindings interface

ToDo - describe how this works
*/

#include "e2_keybinding.h"
#include "e2_option_tree.h"
#include <string.h>
//enable data conversion at idle-time, instead of when a binding is registered
//don't do that, it interferes with config dialog edit-cancellation
#define E2_IDLE_KEYSYNC

#define KEY_BINDINGS_KEY "__bound-keycats__"
#define CHAINTIMER_DATA_KEY "__keychain-timer__"
#define CHAINKEY_DATA_KEY "__chained-key__"

typedef struct _E2_KeyRuntime
{
	guint keyval;	//gdk key code
	GdkModifierType mask;
	gchar *action;	//name of action or command, maybe ""
	gchar *action_data;	//action data, maybe ""
	gboolean cont;	//TRUE to keep looking for other actions to activate
	GSList *chained;//list of E2_KeyRuntime's for immediate-child key(s)
					//i.e. chained directly to this one
} E2_KeyRuntime;

typedef struct _E2_KeyCatRuntime
{
	GQuark name_quark;	//quark for the category-name (which is structured like level1[.level2.level3 ...]
	GNode *node;		//pointer to treenode for which this struct is data
	guint level;		//level in nodes-tree and bindings-tree, effectively the
						//no. of '.'-separated parts of the category name - 1
	GtkTreeRowReference *ref;	//reference to model iter that's the root for the category
//#ifdef E2_IDLE_KEYSYNC
//	gint syncstate;		//0 = new, 1 = requested, 2 = treestore data have been converted to runtime list
//#endif
	GSList *keys;		//E2_KeyRuntime's for each key in the category
	GSList *instances;	//list of widgets bound to this category
//	GSList *curr_chain;	//the chained list of the most-recent activated key
} E2_KeyCatRuntime;

//tree of E2_KeyCatRuntime structs, each for a 'category' of bindings
GNode *keybindings = NULL;
#ifdef E2_IDLE_KEYSYNC
//pointer for use with the keybindings list
//GNode *binding_cur = NULL;
#endif

  /*****************/
 /***** utils *****/
/*****************/

/**
@brief translate any alphabetic keycode in the string @a original
app.keyslocal need not be pre-created, but no checking here for NULL or empty
@a original, faster to do that upstream
@param original keystring in format that gtk understands, like <Control>a
@return newly allocated replacement-keystring, or NULL if no change is needed
*/
static gchar *_e2_keybinding_translate_keycode (const gchar *original)
{
	const gchar *keystring, *translated;
	gchar *replace = NULL;

	if (app.keyslocal == NULL)
		e2_utils_translate_keys (FALSE, TRUE);

	keystring = strrchr (original, '>');
	if (keystring == NULL)
	{
		if (*(original+1) == '\0') //only interested in single-letter key-names
		{
			translated = (const gchar *)g_hash_table_lookup (app.keyslocal, original);
			if (!(translated == NULL || *translated == '\0'
				 || !strcmp (translated, original)))
				replace = g_strdup (translated);
		}
	}
	else
	{
		keystring++;
		if (*(keystring+1) == '\0') //only interested in single-letter key-names
		{
			translated = (const gchar *)g_hash_table_lookup (app.keyslocal, keystring);
			if (!(translated == NULL || *translated == '\0'
				|| !strcmp (translated, keystring)))
			{
				gchar *prefix;
				prefix = g_strndup (original, keystring - original);
				replace = g_strconcat (prefix, translated, NULL);
				g_free (prefix);
			}
		}
	}
	return replace;
}
/**
@brief recursively walk all or part of the keybindings store to localise all keycodes
@param mdl pointer to bindings store treemodel
@param iter pointer to iter to use for interrogating @a mdl

@return
*/
static void _e2_keystring_walk (GtkTreeModel *mdl, GtkTreeIter *iter)
{
	gchar *keystring;
	GtkTreeIter child_iter;
	gtk_tree_model_get (mdl, iter, 1, &keystring, -1);
	if (keystring != NULL && *keystring != '\0')
	{
		gchar *replace = _e2_keybinding_translate_keycode (keystring);
		if (replace != NULL)
		{
			gtk_list_store_set (GTK_LIST_STORE(mdl), iter, 1, replace, -1);
			g_free (replace);
		}
	}
	g_free (keystring);
	if (gtk_tree_model_iter_children (mdl, &child_iter, iter))
	{
		do
		{
			_e2_keystring_walk (mdl, &child_iter);
		} while (gtk_tree_model_iter_next (mdl, &child_iter));
	}
}
/**
@brief convert all (if any) stored alphabetic key-bindings to localised form

@param mdl pointer to keybindings store treemodel
@param iter pointer to a toplevel iter, to use for interrogating @a mdl

@return
*/
void e2_keybinding_localise (GtkTreeModel *mdl, GtkTreeIter *iter)
{
	printd (DEBUG, "localise all key bindings or for a category");
	if (gtk_tree_model_get_iter_first (mdl, iter))
	{
		do
		{
			_e2_keystring_walk (mdl, iter);
		} while (gtk_tree_model_iter_next(mdl, iter));
	}
}
/**
@brief cleanup bound-keys data in list @a keys
Recurses to clean chained keys, if any
@param keys pointer to a bound-keys list

@return
*/
static void _e2_keybinding_free_keyslist (GSList *usedkeys)
{
	GSList *member;
	for (member = usedkeys; member != NULL; member = member->next)
	{
		E2_KeyRuntime *k = member->data;
		if (k != NULL)
		{
			if (k->chained != NULL)
				_e2_keybinding_free_keyslist (k->chained);
			g_free (k->action);
			g_free (k->action_data);
			DEALLOCATE (E2_KeyRuntime, k);
		}
	}
	g_slist_free (usedkeys);
}
/**
@brief cleanup data for the bound keys for the category associated with @a rt
@param rt pointer to keybinding category data struct

@return
*/
static void _e2_keybinding_free_category_keys (E2_KeyCatRuntime *rt)
{
	if (rt->keys != NULL)
	{
		_e2_keybinding_free_keyslist (rt->keys);
		rt->keys = NULL;
	}
}
/**
@brief recursively translate a keybinding treestore branch into a listed struct
Stored mod-letter-keycodes are translated back to asciified equivalents,
if appropriate, to conform to the same sort of translation by UI widgets.
@param model model for the bindings treestore
@param iter pointer to iter to be used to interrogage @a model
@param keys pointer to initialised list to which the created struct(s) will be appended

@return
*/
static void _e2_keybinding_sync_one (GtkTreeModel *model, GtkTreeIter *iter, GSList **keys)
{
	E2_KeyRuntime *k;
	gchar *cat, *key, *action, *action_data;
	gboolean cont, valid;

	gtk_tree_model_get (model, iter, 0, &cat, 1, &key, 2, &cont, 3 , &action,
		4, &action_data, -1);

	valid = (*cat == '\0'//for a key row, the category should be empty
				&& *key != '\0'
				&& *action != '\0');	//data may be empty but not this
	if (valid)
	{
		E2_Action *act = e2_action_check (action);
		valid = (act == NULL || !(act->exclude & E2_ACTION_EXCLUDE_ACCEL));
	}
	if (valid)
	{
		k = ALLOCATE (E2_KeyRuntime);
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (k, valid = FALSE;)
#else
		if (k == NULL)
			valid = FALSE;
#endif
	}
	else
		k = NULL; //warning prevention
	if (!valid)
	{
		g_free (cat);
		g_free (key);
		g_free (action);
		g_free (action_data);
		return;
	}
	GdkModifierType mask;
	guint keyval;
	//this converts ucase to lcase, when there is no modifier
	gtk_accelerator_parse (key, &keyval, &mask);
	if (keyval != 0)
	{
//		printd (DEBUG, "key - %s - %d - %d", key, keyval, mask);
		if (keyval < 0xF000 || keyval > 0xFFFF)
		{
			if (mask == 0)
			{
				/*for letters, gtk event-data includes the upper-case keycode
				when relevant, but the conversion above got a lower-case one.
				We need to fix that */
				const gchar *realkey = gdk_keyval_name (keyval);
				if (strcmp (key, realkey))
					keyval = gdk_keyval_to_upper (keyval);
			}
			else	//modified
			{
				/* via e2_utils_translate_key_event(), UI widgets do translation
				from 'locale'-specific mod-letter-codes to ascii-equivalent,
				before processing. We need to conform the bindings to that */
				guint asciicode = GPOINTER_TO_UINT (g_hash_table_lookup (app.keysnative,
					GUINT_TO_POINTER (keyval)));
				if (asciicode != 0)
					keyval = asciicode;
			}
		}
		k->keyval = keyval;
		k->mask = mask;
		k->action = action;	//may be ""
		k->action_data = action_data;	//may be ""
		k->cont = cont;
		k->chained = NULL;

		*keys = g_slist_append (*keys, k); //list this before any children

		GtkTreeIter child_iter;
		if (gtk_tree_model_iter_children (model, &child_iter, iter))
		{
			do
			{
				_e2_keybinding_sync_one (model, &child_iter, &k->chained);
			} while (gtk_tree_model_iter_next (model, &child_iter));
		}
	}
	else
	{
		g_free (action);
		g_free (action_data);
	}
	g_free (cat);
	g_free (key);
}
/**
@brief translate, from primary treestore to runtime list, the data for a
key-binding category
This can be called at any time, including when there are no keys for the
category in the treestore. Pre-existing data for the category are cleared,
so this cannot be used to _add_ keys to the group
Keys can be nested to any level i.e key-chains can be as long as desired
@param rt pointer to data struct for the keybinding category to be processed

@return if E2_IDLE_KEYSYNC applies, TRUE if the process completed properly
*/
static
#ifdef E2_IDLE_KEYSYNC
gboolean
#else
void
#endif
 _e2_keybinding_sync_category (E2_KeyCatRuntime *rt)
{
	if (!gtk_tree_row_reference_valid (rt->ref))
	{
		printd (WARN, "internal keybinding error, row reference not valid anymore");
#ifdef E2_IDLE_KEYSYNC
		return FALSE;
#else
		return;
#endif
	}
	GtkTreePath *path = gtk_tree_row_reference_get_path (rt->ref);
	if (path == NULL)
	{
		printd (WARN, "internal keybinding error, path is NULL");
#ifdef E2_IDLE_KEYSYNC
		return FALSE;
#else
		return;
#endif
	}
	printd (DEBUG, "%s e2_keybinding_sync", g_quark_to_string (rt->name_quark));
	_e2_keybinding_free_category_keys (rt);	//get rid of any old key data for this category

	E2_OptionSet *set = e2_option_get ("keybindings");
	GtkTreeIter iter;
	gtk_tree_path_down (path);	//down to the keys level
	while (gtk_tree_model_get_iter (set->ex.tree.model, &iter, path))
	{
		//convert current key and all descendants to runtime form
		_e2_keybinding_sync_one (set->ex.tree.model, &iter, &rt->keys);
		gtk_tree_path_next (path);
	}
	gtk_tree_path_free (path);
#ifdef E2_IDLE_KEYSYNC
//	rt->syncstate = 2;
	return TRUE;
#else
	return;
#endif
}
/**
@brief find the first if any category whose name-quark matches the one in @a data
@param tree node data
@param data pointer to walker data struct
@return TRUE when a match is found
*/
static gboolean _e2_keybinding_find_name_helper (GNode *node, E2_NodeMatch *data)
{
	if (data->qname == ((E2_KeyCatRuntime *) node->data)->name_quark)
	{
		data->match = node;
		return TRUE;
	}
	return FALSE;
}
/**
@brief get a keybinding category treenode for @a name
@param name the name of the binding category to search for, non-NULL and non-empty
Any missing ancestor nodes are created and partially-populated
@return the node for @a name, or NULL upon error
*/
static GNode *_e2_keybinding_get_category (const gchar *name)
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
		if (keybindings != NULL)
			g_node_traverse (keybindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
				(GNodeTraverseFunc) _e2_keybinding_find_name_helper, &data);
		if (data.match == NULL)
		{
			//init runtime data
			E2_KeyCatRuntime *rt = ALLOCATE0 (E2_KeyCatRuntime);
			CHECKALLOCATEDWARN (rt, return NULL;)
			rt->name_quark = qname;
			rt->node = g_node_new (rt);
			if (keybindings == NULL)
			{
//				rt->node = g_node_new (rt);
				keybindings = rt->node;
			}
			else
			{
				//pity about doing this scan again ...
				g_node_traverse (keybindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
					(GNodeTraverseFunc) e2_keybinding_walk_toparent, &data);
/*				if (data.match == NULL) can't fail
				{
					DEALLOCATE (E2_KeyCatRuntime, rt);
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
@brief cleanup watches array during widget destruction etc
@param bindings array of binding category structs or NULL

@return
*/
void e2_keybinding_clear_widget_bindings (GPtrArray *bindings)
{
	if (bindings != NULL)
		g_ptr_array_free (bindings, TRUE);
}

  /*********************/
 /***** callbacks *****/
/*********************/

#ifdef E2_IDLE_KEYSYNC
static gboolean _e2_keybinding_walk_sync (GNode *node, gpointer data)
{
	_e2_keybinding_sync_category ((E2_KeyCatRuntime *) node->data);
	return FALSE;
}
/**
@brief convert data for one or all binding category(ies) from treestore to runtime list
(if any category remains to be processed)
This func is mainloop idle callback CHECKME why sync that way?
@param data pointer to binding to be processed, or NULL to do whole list

@return FALSE (so the idle-source is cancelled)
*/
static gboolean _e2_keybinding_idle_sync_cb (E2_KeyCatRuntime *data)
{
//	GtkTreeIter iter;

//	E2_OptionSet *set = e2_option_get ("keybindings");
//	GtkTreeModel *mdl = set->ex.tree.model;

	if (data != NULL)
	{
/*		//sync a single KeyCatRuntime
		if (app.keytrans)
		{
			//translate this part of store before any usage
			GtkTreePath *tp = gtk_tree_row_reference_get_path (data->ref);
			if (tp != NULL)
			{
				gtk_tree_model_get_iter (mdl, &iter, tp);
				gtk_tree_path_free (tp);
				e2_keybinding_localise (mdl, &iter);
			}
		}
*/
		_e2_keybinding_sync_category (data);
	}
	else
	{
/*		if (app.keytrans)
		{
			gtk_tree_model_get_iter_first (mdl, &iter);
			e2_keybinding_localise (mdl, &iter);	//translate all store before any usage
		}
*/
		g_node_traverse (keybindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_keybinding_walk_sync, NULL);
	}
	return FALSE;
}
/* *
@brief setup for converting keybinding data, after the bindindgs treestore is altered
@param model UNUSED
@param arg1  UNUSED
@param arg2  UNUSED
@param data  UNUSED
@return
*/
/*static void _e2_keybinding_model_changed_cb
		(GtkTreeModel *model,
		GtkTreePath *arg1,
		GtkTreeIter *arg2,
		gpointer data)
{
	keys_cur = NULL;
FIXME new approach timer does one category only
	g_idle_add ((GSourceFunc) _e2_keybinding_idle_sync_cb, NULL);
} */
#endif  //def E2_IDLE_KEYSYNC
/**
@brief (de)sensitize option tree buttons for selected option tree row
Config dialog page buttons are de-sensitized if the row is a category.
@param selection pointer to selection
@param model
@param path
@param path_currently_selected
@param set data struct for the keybings option

@return TRUE always (the row is always selectable)
*/
static gboolean _e2_keybinding_tree_selection_check_cb (
	GtkTreeSelection *selection, GtkTreeModel *model, GtkTreePath *path,
	gboolean path_currently_selected, E2_OptionSet *set)
{
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (set->ex.tree.model, &iter, path))
	{
		gchar *cat;
		gtk_tree_model_get (set->ex.tree.model, &iter, 0, &cat, -1);
		gboolean retval = (cat[0] == '\0');
		g_free (cat);
		GtkTreeView *view = gtk_tree_selection_get_tree_view (selection);
		NEEDCLOSEBGL
		e2_option_tree_adjust_buttons (view, retval);
		NEEDOPENBGL
	}
	return TRUE;
}
/**
@brief ensure that cells in the same row as any category name are hidden

Checks whether @a iter in @a model has an empty string in column 0.
If not (i.e. it is a category row), the cell content is hidden,

@param model treemodel for the keybindings option tree
@param iter pointer to iter with data for the current model row
@param cell cell renderer UNUSED
@param data pointer to data UNUSED

@return TRUE (cell is visible) if row is not a category name
*/
static gboolean _e2_keybinding_visible_check_cb (GtkTreeModel *model,
	GtkTreeIter *iter, GtkCellRenderer *cell, gpointer data)
{
	gchar *cat;
	gtk_tree_model_get (model, iter, 0, &cat, -1);
	gboolean retval = (cat[0] == '\0');
	g_free (cat);
	return retval;
}
/**
@brief decide whether tree row is draggable
Checks whether column 0 of the current row has a null string
ie the row is not a category. If so, the row is draggable
@param drag_source GtkTreeDragSource data struct
@param path tree path to a row on which user is initiating a drag

@return TRUE if the row can be dragged
*/
static gboolean _e2_keybinding_tree_draggable_check_cb (
	GtkTreeDragSource *drag_source, GtkTreePath *path)
{
	if (!GTK_IS_TREE_MODEL (drag_source))
		return TRUE;
	GtkTreeModel *model = GTK_TREE_MODEL (drag_source);
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (model, &iter, path))
	{
		gchar *cat;
		gtk_tree_model_get (model, &iter, 0, &cat, -1);
		gboolean retval = (cat[0] == '\0');
		g_free (cat);
		return retval;
	}
	return TRUE;
}
/**
@brief cancel the chained keys specified in category binding detailed by @a rt
This func is a mainloop idle callbacK
@param widget widget being processed

@return FALSE always to cancel the timer
*/
static gboolean _e2_keybinding_remove_chained_idle_cb (GtkWidget *widget)
{
	g_object_set_data (G_OBJECT (widget), CHAINTIMER_DATA_KEY, NULL); //also clears current source
	g_object_set_data (G_OBJECT (widget), CHAINKEY_DATA_KEY, NULL);
	return FALSE;
}

void _e2_keybinding_destroy_chain_idle (gpointer timeout_id)
{
	g_source_remove (GPOINTER_TO_UINT (timeout_id));
}

/**
@brief find and run the action that's bound to a key
Due to the order of callback connection, the key-data in @a event will not yet
have been de-localised by e2_utils_translate_key_event() if that's relevant.
@param widget a widget bound to the category represented by @a rt
@param event data struct for the event
@param user_data UNUSED data specified when cb was connected

@return TRUE (i.e. prevent other handlers) if any key has been acted upon (even if the action failed)
*/
static gboolean _e2_keybinding_key_press_cb (GtkWidget *widget,
	GdkEventKey *event, gpointer user_data)
{
	//not interested in modifier keys
#ifdef USE_GTK2_10
	if (event->is_modifier)
#else
	if (e2_utils_key_is_modifier (event))
#endif
		return FALSE;
	guint keyval = event->keyval;
	guint modifiers = gtk_accelerator_get_default_mod_mask ();
	modifiers &= event->state;
//	printd (DEBUG, "key 1: %d - modifiers: %x", keyval, modifiers);
	/*workaround for matching our storage of <Shift>-letter with gtk's approach
	  to returning keycodes
	  See comments in _e2_keybinding_sync_one() */
	if (keyval < 0xF000 || keyval > 0xFFFF)
	{
		//get rid of shift flag if that's the only one
		if ((modifiers & E2_MODIFIER_MASK) == GDK_SHIFT_MASK)
			modifiers &= ~(GDK_SHIFT_MASK);
		//lcase keycode if shift + other flag(s)
		else if (modifiers & GDK_SHIFT_MASK)
			keyval = gdk_keyval_to_lower (keyval);
	}

	gboolean retval = FALSE;
	//if there was a chain from the last key, check if this one is a valid
	//part of the chain. If so we need to update a pointer and adjust the timer
	gpointer timeout_id = g_object_get_data (G_OBJECT (widget), CHAINTIMER_DATA_KEY);
	if (timeout_id != NULL)
	{
		g_object_set_data (G_OBJECT (widget), CHAINTIMER_DATA_KEY, NULL); //also clears current source
		gpointer chainkey = g_object_get_data (G_OBJECT (widget), CHAINKEY_DATA_KEY);
		if (chainkey != NULL) //there is some chain left
		{
			g_object_set_data (G_OBJECT (widget), CHAINKEY_DATA_KEY, NULL); //in case no more
			GSList *member;
			for (member = (GSList *)chainkey; member != NULL; member = member->next)
			{
				E2_KeyRuntime *k = (E2_KeyRuntime *)member->data;
				if ((k->keyval == keyval) && (k->mask == modifiers))
				{
					if (k->chained != NULL)
					{
						g_object_set_data (G_OBJECT (widget), CHAINKEY_DATA_KEY, k->chained);
						//CHECKME should we run any action before starting timer ?
						//CHECKME single timer when k->cont ?
						guint id = g_timeout_add (e2_option_int_get ("keybindings-timeout"),
							(GSourceFunc) _e2_keybinding_remove_chained_idle_cb, widget);
						g_object_set_data_full (G_OBJECT (widget), CHAINTIMER_DATA_KEY,
							GUINT_TO_POINTER (id),
							(GDestroyNotify) _e2_keybinding_destroy_chain_idle);
					}
					if (*k->action != '\0')
					{	//there is an action to run
//						NEEDCLOSEBGLX
						OPENBGL
						e2_action_run_simple_from (k->action, k->action_data, widget);
						CLOSEBGL
						if (!k->cont)
						{
//							printd (DEBUG,"key binding cb ends, returned value = TRUE");
							return TRUE;
						}
						else
							retval = TRUE;
					}
				}
//				else
//				{
					//abort the chain ?
//				}
			}
		}
	}

	//check whether the key is in a category bound to the widget, or any ancestor of same
	GPtrArray *boundto = (GPtrArray *)
		g_object_get_data (G_OBJECT (widget), KEY_BINDINGS_KEY);
	GNode *topnode = NULL;
	guint indx;
	for (indx = 0; indx < boundto->len; indx++)
	{
		GNode *node = ((E2_KeyCatRuntime *) g_ptr_array_index (boundto, indx))->node;
		while (1)
		{
			GSList *member;
			for (member = ((E2_KeyCatRuntime *)node->data)->keys; member != NULL;
				 member = member->next)
			{
				E2_KeyRuntime *k = member->data;
				if ((k->keyval == keyval) && (k->mask == modifiers))
				{
					g_object_set_data (G_OBJECT (widget), CHAINKEY_DATA_KEY, k->chained);
					if (k->chained != NULL)
					{	//setup for chaining
						//CHECKME should we run any action before starting timer ?
						//CHECKME single timer when k->cont ?
						guint id = g_timeout_add (e2_option_int_get ("keybindings-timeout"),
							(GSourceFunc) _e2_keybinding_remove_chained_idle_cb, widget);
						g_object_set_data_full (G_OBJECT (widget), CHAINTIMER_DATA_KEY,
							GUINT_TO_POINTER (id),
							(GDestroyNotify) _e2_keybinding_destroy_chain_idle);
					}
					if (*k->action != '\0')
					{
//						NEEDCLOSEBGLX
						OPENBGL
						e2_action_run_simple_from (k->action, k->action_data, widget);
						CLOSEBGL
						//remember how far up this branch we've gone, to prevent repeats for other categories
//CHECKME				if (topnode == NULL || g_node_depth (node) < g_node_depth (topnode))
							topnode = node;
						retval = TRUE;
						if (!k->cont)
							break;
					}
				}
			}
			if (member != NULL || G_NODE_IS_ROOT (node))
				break;
			else
			{
				node = node->parent;
				if (node == topnode)
					break;
			}
		}
	}
/*	if (retval)
	{
CHECKME	any normal press-callback won't happen i.e. no delocalisation of the event's
		key data, so what happens when a corresponding release-callback does a delocalisation ?
??		e2_utils_translate_key_event (event);
	}
*/
//	printd (DEBUG,"bindings key-press cb ends, returned value = %s", (retval) ? "TRUE":"FALSE");
	return retval;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief register all main-window widgets' key bindings, lowest-first order

@return
*/
void e2_keybinding_register_all (void)
{
	//there's no generic press-handler for the whole window, so no need to muck
	//about with the order of callback connections
	//CHECKME specific binding cb for app.main_window ?
	e2_keybinding_enrol (app.main_window, _C(17), (void(*)(E2_OptionSet*))NULL);
	gchar *category = g_strconcat(_C(17),".",_C(23),NULL);  //_("general.main
	e2_keybinding_enrol (app.main_window, category, (void(*)(E2_OptionSet*))NULL);
	g_free (category);

	e2_fileview_register_keybindings (app.pane1.view.treeview, &app.pane1.view);
	e2_fileview_register_keybindings (app.pane2.view.treeview, &app.pane2.view);

	GList *member;
	for (member = app.command_lines; member != NULL; member = g_list_next (member))
	{
		E2_CommandLineRuntime *rt = (E2_CommandLineRuntime *) member->data;
		e2_command_line_register_keybindings (rt);
	}
	//tab bindings are also set when the tab is created - so that new ones are covered
	for (member = app.tabslist; member != NULL ; member = g_list_next (member))
	{
		GtkTextView *tvw = ((E2_OutputTabRuntime *)member->data)->text;
		e2_output_register_keybindings (GTK_WIDGET (tvw));
	}
}
/**
@brief find tree parent of node whose name-quark is provided in @a data
@param node a node in the bindings tree being traversed
@param data pointer to data struct with quark and match-pointer

@return TRUE if/when a parent is found
*/
gboolean e2_keybinding_walk_toparent (GNode *node, E2_NodeMatch *data)
{
	const gchar *childname = g_quark_to_string (data->qname);
	const gchar *s = strrchr (childname, '.');
	if (s == NULL)	//not a child, so can't find a parent
	{
//		data->match = NULL;
		return FALSE;
	}
	const gchar *nname = g_quark_to_string (*((GQuark*)node->data));
	if (/*nname == NULL ||*/ strncmp (nname, childname, s - childname)
		|| *(nname + (s - childname)) != '\0')
	{
//		data->match = NULL;
		return FALSE;
	}
	data->match = node;
	return TRUE;
}
/**
@brief apply key-binding category @a category to "core" widget @a widget
If E2_TREEINCREMENT applies and @a category doesn't exist in the bindings treestore,
it (and any missing ancestor(s)) is added to that store.
If @a category hasn't been used before, its data are setup for runtime usage, or
if E2_IDLE_BTNSYNC applies, such setup is arranged (to be done at idle time)
If non-NULL, the function named @a defaults_func must have the same form as
_e2_keybinding_tree_defaults (). But there is no enrolment if @a category is
already in the keybindings treestore (from config file or a prior instance of a
transient item that uses the category)
@param widget the widget to which the binding will be attached, or NULL
@param category name of the keybinding, structured like level1[.level2.level3 ...]
@param defaults_func pointer to function which creates a tree-option array, or NULL

@return
*/
void e2_keybinding_enrol (GtkWidget *widget, const gchar *category,
	void (*defaults_func)(E2_OptionSet*))
{
	GNode *member = _e2_keybinding_get_category (category);
	if (member == NULL)
	{
		printd (WARN, "e2_keybinding_enrol: category %s not found", category);
		return;
	}

	E2_KeyCatRuntime *rt = (E2_KeyCatRuntime *)member->data;
	gboolean first = (rt->ref == NULL);	//first-time usage of category
	if (first)
	{
		//add iter to bindings config treestore if not already present
		E2_OptionSet *set = e2_option_get ("keybindings");
		GtkTreeIter iter;
#ifdef E2_TRANSIENTBINDINGS
		gboolean old =
#endif
		e2_tree_get_lowest_iter_for_str (set->ex.tree.model, 0, &iter, category);
		//the model may change between registrations, so remember where this cat starts
		GtkTreePath *tp = gtk_tree_model_get_path (set->ex.tree.model, &iter);
		rt->ref = gtk_tree_row_reference_new (set->ex.tree.model, tp);
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
#ifdef E2_IDLE_KEYSYNC
		g_idle_add ((GSourceFunc) _e2_keybinding_idle_sync_cb, rt);
		printd (DEBUG, "arranged keybinding idle-sync for %s", category);
#else
		//translate the option tree data to rt form, now
		//update store translation before any usage
		gtk_tree_model_get_iter (set->ex.tree.model, &iter, tp); //back to where we started
		gtk_tree_path_free (tp);
/*		if (app.keytrans)
			e2_keybinding_localise (set->ex.tree.model, &iter);
*/
		_e2_keybinding_sync_category (rt);
#endif
	}

	if (G_LIKELY (widget != NULL))
	{
		printd (DEBUG, "register widget %x in keybinding category '%s'", widget, category);
		GPtrArray *boundto = (GPtrArray *)
			g_object_get_data (G_OBJECT (widget), KEY_BINDINGS_KEY);
		if (boundto == NULL)	//this is the first time widget is enrolled
		{
			//the model is: just one cb for all bindings
			//CHECKME how is main-window handled ?
			g_signal_connect (G_OBJECT (widget), "key-press-event",
				G_CALLBACK (_e2_keybinding_key_press_cb), NULL);

			boundto = g_ptr_array_new ();
			g_object_set_data_full (G_OBJECT (widget), KEY_BINDINGS_KEY, boundto,
				(GDestroyNotify) e2_keybinding_clear_widget_bindings);
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
				E2_KeyCatRuntime *this = (E2_KeyCatRuntime *)g_ptr_array_index (boundto, indx);
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
@brief unregister widget(s) from a category
@param node node in categories tree
@param data pointer to data for widget to be blocked
@return FALSE so the walk continues
*/
static gboolean _e2_keybinding_walk_unregister (GNode *node, E2_NodeData *data)
{
	E2_KeyCatRuntime *rt = node->data;
	if (data->qname == rt->name_quark || data->qname == 0)
	{
		GSList *widgets;
		for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
		{
			if (data->widget == widgets->data || data->widget == NULL)
			{
				if (GTK_IS_WIDGET(widgets->data))
				{
					//in case widget isn't to be destroyed
					g_signal_handlers_disconnect_matched (widgets->data,
						G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_keybinding_key_press_cb, NULL);
					printd (DEBUG, "UNregister keybinding for widget");
					gpointer boundto = g_object_get_data (G_OBJECT (widgets->data),
							KEY_BINDINGS_KEY);
					if (boundto != NULL)
					{
						while (g_ptr_array_remove_fast ((GPtrArray *)boundto, rt)); //probably only one
					}
				}
				else
					printd (DEBUG, "No UNregister key-binding for destroyed widget");
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
void e2_keybinding_disrol (GtkWidget *widget, const gchar *category)
{
	printd (DEBUG, "e2_keybinding_disrol, category: %s, widget: _",
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
	g_node_traverse (keybindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_keybinding_walk_unregister, &data);
}
/**
@brief block all keybinding callbacks for widget(s) in a category
@param node node in categories tree
@param data pointer to data for widget to be blocked
@return FALSE so the walk continues
*/
static gboolean _e2_keybinding_walk_block (GNode *node, E2_NodeData *data)
{
	E2_KeyCatRuntime *rt = node->data;
	if (data->qname == rt->name_quark || data->qname == 0)
	{
		GSList *widgets;
		for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
		{
			if (data->widget == widgets->data || data->widget == NULL)
			{
				g_signal_handlers_block_matched (widgets->data,
					G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_keybinding_key_press_cb, NULL);
			}
		}
	}
	return FALSE;
}
/**
@brief block binding of @a category to @a widget
@param widget the widget which is to be unbound, or NULL for all widgets
@param category name of binding to be processed, or NULL for all categories

@return
*/
void e2_keybinding_block (GtkWidget *widget, const gchar *category)
{
	printd (DEBUG, "e2_keybinding_block, category: %s, widget: _",
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
	g_node_traverse (keybindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_keybinding_walk_block, &data);
}
/**
@brief unblock all keybinding callbacks for widget(s) in a category
@param node node in categories tree
@param data pointer to data for widget to be unblocked
@return FALSE so the walk continues
*/
static gboolean _e2_keybinding_walk_unblock (GNode *node, E2_NodeData *data)
{
	E2_KeyCatRuntime *rt = node->data;
	if (data->qname == rt->name_quark || data->qname == 0)
	{
		GSList *widgets;
		for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
		{
			if (data->widget == widgets->data || data->widget == NULL)
			{
				g_signal_handlers_unblock_matched (widgets->data,
					G_SIGNAL_MATCH_FUNC, 0, 0, NULL, _e2_keybinding_key_press_cb, NULL);
			}
		}
	}
	return FALSE;
}
/**
@brief unblock binding of @a category to @a widget
@param widget the widget which is to be unbound, or NULL for all widgets
@param category name of binding to be processed, or NULL for all categories

@return
*/
void e2_keybinding_unblock (GtkWidget *widget, const gchar *category)
{
	printd (DEBUG, "e2_keybinding_unblock, category: %s, widget: _",
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
	g_node_traverse (keybindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
			(GNodeTraverseFunc) _e2_keybinding_walk_unblock, &data);
}
/**
@brief cleanup data for a keybindings category
@param node a node in the categories tree
@param pointerised TRUE/FALSE, TRUE to completely clean the data (session-end)
@return FALSE so the walk continues
*/
static gboolean _e2_keybinding_walk_categories (GNode *node, gpointer data)
{
	E2_KeyCatRuntime *rt = node->data;
	gtk_tree_row_reference_free (rt->ref);
	rt->ref = NULL;
	if (rt->keys != NULL)
	{
		_e2_keybinding_free_keyslist (rt->keys);
		rt->keys = NULL;
	}
	if (rt->instances != NULL)
	{
/*cb data is NULL now
		if (data != NULL)
		{
			//if bound widgets are not being destroyed, disconnect signals with data = rt
			GSList *widgets;
			for (widgets = rt->instances; widgets != NULL; widgets = widgets->next)
			{
				if (GTK_IS_WIDGET (widgets->data))	//the bound widget may have been destroyed
				{
					g_signal_handlers_disconnect_matched (widgets->data,
						G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_DATA,
						0, 0, NULL, _e2_keybinding_key_press_cb, rt);
				}
			}
		}
*/
		g_slist_free (rt->instances);
		rt->instances = NULL;
	}
	if (data != NULL)
	{
		DEALLOCATE (E2_KeyCatRuntime, rt);
		g_node_destroy (node);
	}
	return FALSE;
}
/**
@brief cleanup some keybindings runtime data
The bindings data list is not affected
Key lists are not cleared - that happens when the first widget is re-registered
Callbacks are checked before re-connection

@return
*/
void e2_keybinding_clean (void)
{
	g_node_traverse (keybindings, G_PRE_ORDER, G_TRAVERSE_ALL, -1,
		(GNodeTraverseFunc) _e2_keybinding_walk_categories, NULL);
}
/* *
@brief cleanup all bindings data
Presently unused
@return
*/
/*void e2_keybinding_free_all (void)
{
	g_node_traverse (keybindings, G_POST_ORDER, G_TRAVERSE_ALL, -1,
		(GNodeTraverseFunc) _e2_keybinding_walk_categories, GINT_TO_POINTER (1));
}
*/
/**
@brief display current key bindings, all or a nominated section
Expects BGL on/closed
@param parameters argument(s) to help command, starting with _(keys)

@return
*/
void e2_keybinding_output_help (gchar *parameters)
{
	//decide which section of the bindings to report
	//NULL reports all (general)
	gchar *section = e2_utils_find_whitespace (parameters);
	if (section != NULL)
	{
		section =  g_strstrip (section);
		if (*section == '\0')
			section = NULL;
	}
	// get treestore iter for start of bindings to report
	GtkTreeIter iter;
	E2_OptionSet *opt_keys = e2_option_get ("keybindings");
	GtkTreeModel *mdl = opt_keys->ex.tree.model;
	if (section == NULL)
		gtk_tree_model_get_iter_first (mdl, &iter);
	else if (!e2_tree_find_iter_from_str (mdl, 0, section, &iter, FALSE))
	{
		gchar *msg = g_strdup_printf (_("Cannot find a key binding named %s"),
			section);
		e2_output_print_error (msg, TRUE);
		return;
		//revert to showing all
//		gtk_tree_model_get_iter_first (mdl, &iter);
	}

	GString *str = g_string_sized_new (64);
	g_string_printf (str,
		"\t  "PROGNAME" %s\n"
		"\t+------------------------+\n",
		_("key bindings"));
	e2_output_print (&app.tab, str->str, NULL, FALSE, NULL);
	str = g_string_truncate (str, 0);

	GtkTreeIter iter2;
	do
	{
		if (gtk_tree_model_iter_children (mdl, &iter2, &iter))
		{
			do
			{
				gchar *cat, *key, *act, *arg;
				// get data from treestore
				//columns: 0 = category, 1 = key, 3 = action, 4 = argument
				gtk_tree_model_get (mdl, &iter2, 0, &cat, 1, &key, 3, &act, 4, &arg, -1);
				if (*cat != '\0')
				{
					GtkTreeIter iter3;
					g_string_append_printf (str, "\t%s\n", cat);
					g_free (cat);
					if (gtk_tree_model_iter_children (mdl, &iter3, &iter2))
					{
						do
						{
							gtk_tree_model_get (mdl, &iter3, 0, &cat, 1, &key, 3, &act, 4, &arg, -1);
							if (*cat != '\0')
							{
								GtkTreeIter iter4;
								g_string_append_printf (str, "\t\t%s\n", cat);
								g_free (cat);
								if (gtk_tree_model_iter_children (mdl, &iter4, &iter3))
								{
									do
									{
										gtk_tree_model_get (mdl, &iter4, 1, &key, 3, &act, 4, &arg, -1);
										g_string_append_printf (str, "\t\t\t%s\t%s\t%s\n", key, act, arg);
										g_free (key);
										g_free (act);
										g_free (arg);
									} while (gtk_tree_model_iter_next (mdl, &iter4));
								}
							}
							else
							{
								g_string_append_printf (str, "\t\t%s\t%s\t%s\n", key, act, arg);
								g_free (cat);
								g_free (key);
								g_free (act);
								g_free (arg);
							}
						} while (gtk_tree_model_iter_next (mdl, &iter3));
					}
				}
				else
				{
					g_string_append_printf (str, "\t%s\t%s\t%s\n", key, act, arg);
					g_free (cat);
					g_free (key);
					g_free (act);
					g_free (arg);
				}
			} while (gtk_tree_model_iter_next (mdl, &iter2));
		}
	} while (gtk_tree_model_iter_next (mdl, &iter) && section == NULL);

	e2_output_print (&app.tab, str->str, NULL, FALSE, NULL);

	g_string_free (str, TRUE);
}
#ifdef WITH_KEYFAKE
typedef struct _E2_KeyFake
{
	gpointer from;
	GdkWindow *window;
	const gchar *keys_string;
} E2_KeyFake;
/**
@brief issue fake gdk key-press event(s) in accord with data
@param data pointer to data for the events
@return
*/
static void _e2_keybinding_fake_events (E2_KeyFake *data)
{
//	NOTE THIS IS NOT MUCH TESTED
	//CHECKME corresponding release-event(s)
	if (GTK_IS_WIDGET (data->from))
	{
		GdkEvent *event2 = gdk_event_new (GDK_KEY_PRESS);
		event2->any.window = data->window;
//		event2->any.send_event = '\1';	//TRUE;
		//multi-key separator is space char (comma etc might be part of fake sequence)
		gchar **split = g_strsplit (data->keys_string, " ", -1);
		guint fakekey, i, j = g_strv_length (split);
		GdkModifierType fakestate;
		for (i = 0; i < j; i++)
		{
			if (split[i] == NULL || *split[i] == '\0')
				continue;
/*			//handle instances of "\," which are not a separator
			s = split[i] + strlen (split[i]) - 1;
			if (*s == '\\' && s > split[i] && *(s-1) != '\\')
				*s = ',';
*/
			gtk_accelerator_parse (split[i], &fakekey, &fakestate);
			if (fakekey == 0 && fakestate == 0)
				continue;
			printd (DEBUG, "constructed fake key for %s", split[i]);
			event2->key.keyval = fakekey;
//			event2->key.state = event2->key.state & ~GDK_MODIFIER_MASK;
//			event2->key.state |= fakestate;
			event2->key.state = fakestate;
/*			switch (fakestate)
			{
				case 0:
					event2->key.length = strlen (split[i]);
					event2->key.string = g_strdup (split[i]);
					break;
				case GDK_CONTROL_MASK:
					event2->key.length = 1;
					gchar fake[2] = { fakekey - 96, 0 };
					event2->key.string = g_strdup (fake);
					break;
				default:
					event2->key.length = 0;
					event2->key.string = g_strdup ("");
					break;
			}
*/
			//hardware_keycode must be correct for fake keycode
			gint n_keys;
			GdkKeymapKey *keys;
			if (!gdk_keymap_get_entries_for_keyval (NULL, fakekey, &keys, &n_keys)
				|| n_keys == 0)
				continue;
			event2->key.hardware_keycode = (guint16) keys->keycode;
			g_free (keys);
//			event2->key.time = event->key.time + i;
			gtk_propagate_event ((GtkWidget *)data->from, event2);
//			printd (DEBUG, "signal handler cb returned %s", (success) ? "TRUE":"FALSE");
			//if this is set above - g_free (event2->key.string);
		}
		event2->any.window = NULL; //prevent destruction of the 'real' event window during cleanup
//if this is set above - event2->key.string = NULL;	//prevent crash when clearing event
		gdk_event_free (event2);
		g_strfreev (split);
	}
	printd (DEBUG, "_e2_keybinding_fake_events ends");
}
/**
@brief arrange to issue fake gdk key-press event(s) in accord with action data
This expects as action-data a string indicating which key(s) are to be issued
Each key name in the string must be in a form parsable by gtk e.g. <Ctrl><Alt>m.
Sequential keys must be separated by a space char (a fake space key is 'space'
without quotes)
@param from the widget where the action was initiated - must not be NULL
@param art action runtime data

@return TRUE if timer is set up to issue fake event(s)
*/
static gboolean _e2_keybinding_issue (gpointer from, E2_ActionRuntime *art)
{
	printd (DEBUG, "_e2_keybinding_issue from:_ art->data: %s", art->data);
	const gchar *fakes = (const gchar *) art->data;
	if (fakes == NULL || *fakes == '\0')
		return FALSE;	//user goofed the config data
	GdkEvent *event = gtk_get_current_event ();
	if (event == NULL)
		return FALSE;	//should never happen
/*	if (event->type != GDK_KEY_PRESS)
	{
		gdk_event_free (event);
		return FALSE;	//should never happen
	}
*/
	E2_KeyFake fake_data;
	fake_data.from = (from != NULL) ? from : app.main_window;	//insurance
	fake_data.window = event->any.window;	//survives event cleanup
	//CHECKME any other event data to re-use ?
	fake_data.keys_string = fakes;

	gdk_event_free (event);
	_e2_keybinding_fake_events (&fake_data);
	WAIT_FOR_EVENTS;
	return TRUE;
}
/**
@brief register actions related to keybindings

@return
*/
void e2_keybinding_actions_register (void)
{
	E2_Action action =
	{g_strconcat(_A(127),".",_A(128),NULL),_e2_keybinding_issue,TRUE,E2_ACTION_TYPE_ITEM,0,NULL,NULL};
	e2_action_register (&action);
}
#endif	//def WITH_KEYFAKE
/**
@brief install default tree options for keybindings
This function is called only if the default is missing from the config file
@param set pointer to set data

@return
*/
static void _e2_keybinding_tree_defaults (E2_OptionSet *set)
{
	//the key name strings are parsed by gtk, and except for simple letters
	//(which might reasonably change for different languages) no translation
	//is possible
	e2_option_tree_setup_defaults (set,
	g_strdup("keybindings=<"),  //internal name
	g_strconcat(_C(17),"||||",NULL),  //_("general"
	g_strconcat("\t|F10||",_A(1),".",_A(75),"|",NULL), //"quit"
	g_strconcat("\t|F11||",_A(17),".",_A(51),"|",NULL), //"toggle.fullscreen"
	g_strconcat("\t|F12||",_A(3),".",_A(34),"|",NULL),  //"configure"

	g_strconcat("\t",_C(23),"||||",NULL),  //_("main"
	g_strconcat("\t\t|F1||",_A(14),".",_A(76),"|",NULL), //"refresh"
	g_strconcat("\t\t|<Shift>F1||",_A(8),".",_A(61),"|",NULL), //"history.list
	g_strconcat("\t\t|<Control>F1||",_A(15),".",_A(61),"|",NULL), //"pending.list
	g_strconcat("\t\t|<Alt>F1||",_A(2),".",_A(61),"|",NULL), //"children.list
	g_strconcat("\t\t|F2||",_A(6),".",_A(79),"|",NULL), //"rename"
	g_strconcat("\t\t|F3||",_A(6),".",_A(109),"|",NULL), //"view"
	g_strconcat("\t\t|<Shift>F3||",_A(6),".",_A(110),"|",NULL), //"view_again"
	g_strconcat("\t\t|<Control>F3||",_A(6),".",_A(49),"|",NULL), //"find" (was F4)
	g_strconcat("\t\t|F4||",_A(6),".",_A(46),"|",NULL), //"edit"
	g_strconcat("\t\t|<Shift>F4||",_A(6),".",_A(47),"|",NULL), //"edit_again"
	g_strconcat("\t\t|F5||",_A(6),".",_A(39),"|",NULL),  //"copy"
	g_strconcat("\t\t|<Shift>F5||",_A(6),".",_A(40),"|",NULL),  //"copy_as"
	g_strconcat("\t\t|<Control>F5||",_A(6),".",_A(42),"|",NULL),  //"copy_with_time"
	g_strconcat("\t\t|<Alt>F5||",_A(6),".",_A(41),"|",NULL),  //"copy_merge"
	g_strconcat("\t\t|F6||",_A(6),".",_A(65),"|",NULL), //"move"
	g_strconcat("\t\t|<Shift>F6||",_A(6),".",_A(66),"|",NULL), //"move_as"
	g_strconcat("\t\t|F7||",_A(1),".",_A(63),"|",NULL), //"mkdir"
	g_strconcat("\t\t|F8||",_A(6),".",_A(18),"|",NULL), //"trash"
	g_strconcat("\t\t|<Control>F8||",_A(18),".",_A(67),"|",NULL), //"trash.open"  modifiers not translated, gtk needs english to parse
	g_strconcat("\t\t|<Shift><Control>F8||",_A(6),".",_A(45),"|",NULL), //"delete"
	g_strconcat("\t\t|F9||",_A(6),".",_A(58),"|",NULL), //"file.info"
#ifdef E2_TREEDIALOG
	g_strconcat("\t\t|<Shift>F9||",_A(13),".",_A(106),"|",NULL), //"pane_active.tree" popup directories tree dialog
#endif
#ifdef E2_VFS
	g_strconcat("\t\t|<Control>F9||",_A(4),".",_A(125),"|",NULL), //"dialog.namespace"
	g_strconcat("\t\t|<Alt>F9||",_A(13),".",_A(123),"|",NULL), //"pane.<spacemenu>"
#endif
	g_strconcat("\t\t|<Control>F11||",_A(10),".",_A(31),"|",NULL), //"output.add
	g_strconcat("\t\t|<Shift><Control>F11||",_A(10),".",_A(45),"|",NULL), //"output.delete
	g_strconcat("\t\t|<Shift>F12||",_A(14),".",_A(102),"|",NULL), //"panes.toggle direction"
//	_I( single letters in the following not worth translating ?
	//conform this help string to the following 'jump-keys'
	g_strconcat("\t\t|<Control>g||",_A(10),".",_A(74),"|",_("Now press one of h,m,d\\n"),NULL), //"print"
	//these 3 children are the so-called "directory jump keys"
	g_strconcat("\t\t\t|h||cd|$HOME",NULL), //_A(20)
	g_strconcat("\t\t\t|m||cd|/mnt",NULL),  //_A(20)
	g_strconcat("\t\t\t|d||cd|$HOME/downloads",NULL),  //_A(20)
	g_strconcat("\t\t|<Control>w||!",_A(10),".",_A(33),"|1,*",NULL),  //"adjust_ratio" no arg translation NOTE blocked * expansion
	g_strconcat("\t\t|<Control>e||!",_A(10),".",_A(33),"|0,*",NULL),  //"adjust_ratio" no arg translation ditto
	g_strconcat("\t\t|<Control>z||",_A(1),".",_A(103),"|",NULL), //"focus toggle"
	g_strconcat("\t\t|<Control>r||",_A(14),".",_A(76),"|",NULL), //"refresh"
	g_strconcat("\t\t|<Control>1||",_A(5),".",_A(103),"|1",NULL), //"focus toggle" no arg translation
	g_strconcat("\t\t|<Control>2||",_A(5),".",_A(103),"|2",NULL), //"focus toggle"no arg translation
	g_strconcat("\t\t|<Control>Insert||",_A(1),".",_A(59),"|",NULL), //"insert selection"
	g_strconcat("\t\t|<Alt>Insert||",_A(1),".",_A(59),"|",_A(120),NULL), //"insert selection, quote"
	g_strconcat("\t\t",_C(33),"||||",NULL),  //_("panes"
	//note "Space" not recognised, must be "space"
//	g_strconcat("\t\t\t|space||",_A(14),".",_A(98),"|",NULL), //"switch"  something else makes this 'open' the selected item
	g_strconcat("\t\t\t|space||",_A(7),".",_A(105),"|",NULL), //"toggle selected"
	g_strconcat("\t\t\t|Tab||",_A(14),".",_A(98),"|",NULL), //"switch"
	g_strconcat("\t\t\t|Return||",_A(6),".",_A(67),"|",NULL), //"open"
	g_strconcat("\t\t\t|<Shift>Return||",_A(6),".",_A(68),"|",NULL), //"open in other"
	g_strconcat("\t\t\t|<Control>Return|||",NULL), //nothing
	//these 2 children are examples, 1 to 6 are allowed
	g_strconcat("\t\t\t\t|","2","||",_A(6),".",_A(69),"2|",NULL),//"open with2"
	g_strconcat("\t\t\t\t|","3","||",_A(6),".",_A(69),"3|",NULL),//"open with3"
	g_strconcat("\t\t\t|Left||",_A(13),".",_A(52),"|",NULL), //"go back"
	g_strconcat("\t\t\t|Right||",_A(13),".",_A(53),"|",NULL), //"go forward"
	g_strconcat("\t\t\t|<Alt>Up||cd|..",NULL),  //_A(20)
	g_strconcat("\t\t\t|BackSpace||cd|..",NULL),  //_A(20)
	g_strconcat("\t\t\t|Delete||",_A(6),".",_A(45),"|",NULL), //"delete"
	g_strconcat("\t\t\t|<Shift>Delete||",_A(18),".",_A(45),"|",NULL), //"trash.delete"
//	g_strconcat("\t\t\t|Home||cd|$HOME",NULL),   //_A(20)
	g_strconcat("\t\t\t|<Control>a||",_A(7),".",_A(104),"|",NULL), //"toggle select all"
	g_strconcat("\t\t\t|<Control>i||",_A(7),".",_A(60),"|",NULL), //"invert selection"
	g_strconcat("\t\t\t|<Control>d||",_A(7),".",_A(83),"|",NULL), //"select_type"
	g_strconcat("\t\t\t|<Control>f||",_A(6),".",_A(49),"|",NULL), //"find"
	g_strconcat("\t\t\t|<Control>x||",_A(14),".",_A(101),"|",NULL), //"sync"
	g_strconcat("\t\t\t|<Control>h||",_A(13),".",_A(87),"|",NULL),  //"toggle hidden"
	g_strconcat("\t\t\t|<Control>m||",_A(13),".",_A(24),"|",NULL), //"<bookmarks>"
	g_strconcat("\t\t\t|<Control>t||",_A(13),".",_A(25),"|",NULL), //"<filters>"
//	g_strconcat("\t\t\t|/||",_A(6),".",_A(49),"|",NULL),  //"find"
	g_strconcat("\t\t\t|<Control>Left||!",_A(14),".",_A(33),"|0,*",NULL), //"adjust ratio" no arg translation NOTE blocked * expansion
	g_strconcat("\t\t\t|<Control>Right||!",_A(14),".",_A(33),"|1,*",NULL), //"adjust ratio" no arg translation ditto
	g_strconcat("\t\t\t|<Control>p||",_A(13),".",_A(88),"|",NULL),  //"show menu"
	g_strconcat("\t\t\t|<Shift><Control>p||",_A(13),".",_A(88),"|",_A(121),NULL), //"show menu, shift"
	g_strconcat("\t\t\t|<Control><Alt>p||",_A(13),".",_A(88),"|",_A(113),NULL),  //"show menu, ctrl"
	g_strconcat("\t\t\t|<Control><Alt>n||",_A(7),".",_A(94),"|",NULL),  //"filelist.sortname"
	g_strconcat("\t\t\t|<Control><Alt>m||",_A(7),".",_A(93),"|",NULL),  //"filelist.sortmodified"
	g_strconcat("\t\t\t|<Control><Alt>s||",_A(7),".",_A(96),"|",NULL),  //"filelist.sortsize"
	g_strconcat("\t\t",_C(12),"||||",NULL),  //_"directory line"
	g_strconcat("\t\t\t|Tab||",_A(1),".",_A(38),"|",_A(114),NULL), //"complete dirs"
	g_strconcat("\t\t\t|<Control>Tab||",_A(13),".",_A(67),"|",NULL), //"open"
	g_strconcat("\t\t\t|<Control>Delete||",_A(5),".",_A(36),"|",NULL),  //"clear"
	g_strconcat("\t\t\t|<Shift>Insert||",_A(5),".",_A(32),"|",NULL),  //"add_history"
	g_strconcat("\t\t\t|<Alt>Delete||",_A(5),".",_A(44),"|",NULL),  //"del_history"
	g_strconcat("\t\t\t|<Shift><Alt>Delete||",_A(5),".",_A(37),"|",NULL),  //"clear_history"
	g_strconcat("\t\t",_C(5),"||||",NULL),  //_("command line"
	g_strconcat("\t\t\t|Tab||",_A(1),".",_A(38),"|",NULL), //"complete"
	g_strconcat("\t\t\t|<Shift>Return||",_A(1),".",_A(84),"|",NULL), //"command.send"
	g_strconcat("\t\t\t|<Control>Delete||",_A(1),".",_A(36),"|",NULL), //"clear"
	g_strconcat("\t\t\t|<Alt>Delete||",_A(1),".",_A(44),"|",NULL),  //"del_history"
	g_strconcat("\t\t\t|<Shift><Alt>Delete||",_A(1),".",_A(37),"|",NULL),  //"clear_history"
	g_strconcat("\t\t\t|<Alt>Insert||",_A(2),".",_A(28),"|",NULL), //"children<menu>"
//	g_strconcat("\t\t",_C(28),"||||",NULL),  //_("output"
//CHECKME these stay as command-line bindings ??
	g_strconcat("\t\t\t|Page_Up||",_A(10),".",_A(72),"|",NULL), //"page up"
	g_strconcat("\t\t\t|Page_Down||",_A(10),".",_A(71),"|",NULL), //"page down"
	g_strconcat("\t\t\t|<Shift>Page_Up||",_A(10),".",_A(56),"|",NULL), //"goto top"
	g_strconcat("\t\t\t|<Shift>Page_Down||",_A(10),".",_A(55),"|",NULL), //"goto bottom"
	g_strconcat("\t\t\t|<Shift>Up||",_A(10),".",_A(81),"|",NULL),  //"scroll up"
	g_strconcat("\t\t\t|<Shift>Down||",_A(10),".",_A(80),"|",NULL),  //"scroll down"
	g_strconcat("\t\t",_C(28),"||||",NULL),  //_("output"

	g_strconcat("\t",_C(11),"||||",NULL),  //_("dialogs"
//	g_strconcat("\t",_C(34),"||||",NULL),  //_("plugins"
	g_strdup(">"),
	NULL);
}
/**
@brief initialize key-related options, and init keys pointer

@return
*/
void e2_keybinding_options_register (void)
{
	//no screen rebuilds needed after any change to these options
	gchar *group_name = g_strconcat(_C(20),".",_C(22),NULL);
	E2_OptionSet *set = e2_option_tree_register ("keybindings", group_name, _C(22),  //_("keybindings"
		NULL, _e2_keybinding_tree_selection_check_cb, _e2_keybinding_tree_draggable_check_cb,
		E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_BUILDKEYS);
	e2_option_tree_add_column (set, _("Category"), E2_OPTION_TREE_TYPE_STR, 0, "",
		E2_OPTION_TREE_COL_NOT_EDITABLE, NULL, NULL);
	e2_option_tree_add_column (set, _("Key"), E2_OPTION_TREE_TYPE_KEY, 0, "",
		0, _e2_keybinding_visible_check_cb, NULL);
	e2_option_tree_add_column (set, _("Continue"), E2_OPTION_TREE_TYPE_BOOL, FALSE, "false", //no translation
		0, _e2_keybinding_visible_check_cb, NULL);
	e2_option_tree_add_column (set, _("Action"), E2_OPTION_TREE_TYPE_SEL, 0, "",
		0, _e2_keybinding_visible_check_cb,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_ACCEL
		| E2_ACTION_EXCLUDE_LAYOUT));
	e2_option_tree_add_column (set, _("Argument"), E2_OPTION_TREE_TYPE_SEL , 0, "",
		0, _e2_keybinding_visible_check_cb,
		GINT_TO_POINTER (E2_ACTION_EXCLUDE_GENERAL | E2_ACTION_EXCLUDE_ACCEL
		| E2_ACTION_EXCLUDE_LAYOUT | E2_ACTION_EXCLUDE_TOGGLE));
	e2_option_tree_create_store (set);

	e2_option_tree_prepare_defaults (set, _e2_keybinding_tree_defaults);

	group_name = g_strconcat(_C(20),":",_C(26),NULL); //_("interface:miscellaneous"
	e2_option_int_register ("keybindings-timeout", group_name, _("chained keybindings timeout (ms)"),
		_("This sets the time limit (in milliseconds) for accepting 'chained' keybindings"),
		NULL, 2000, 1, 1000000,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP);
}
