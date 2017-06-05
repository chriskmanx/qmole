/* $Id: e2_combobox.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/utils/e2_combobox.c
@brief GtkComboBox utilities

This file contains utility functions for the GtkComboBox widget
*/

#include "emelfm2.h"
#include "e2_combobox.h"
#include "e2_pane.h"
#include "e2_command_line.h"

/**
@brief block the issue of "activate" signal during "changed" signal callback on @a combo

@param combo the entry text combo box to be configured

@return
*/
void e2_combobox_block_changed (GtkWidget *combo)
{
	g_object_set_data (G_OBJECT (combo), "e2_changed_blocked",
		GINT_TO_POINTER (TRUE));
}

static void _e2_combobox_unblock_changed (GtkWidget *combo)
{
	g_object_set_data (G_OBJECT (combo), "e2_changed_blocked",
		GINT_TO_POINTER (FALSE));	//= NULL
}
/**
@brief determine the index of the last row in the model of @a combo

@param combo the entry text combo box to be interrogated

@return the index of the last item in the model, -1 if there's nothing
*/
static gint _e2_combobox_last_index (GtkComboBox *combo)
{
	GtkTreeModel *model = gtk_combo_box_get_model (combo);
	return ((gint)gtk_tree_model_iter_n_children (model, NULL) - 1);
}

  /*******************/
 /**** callbacks ****/
/*******************/

/**
@brief "key-press-event" signal callback to handle accessing combobox history items and translate relevant keycodes
This applies to _all_ created comboboxes whether or not they have a text-entry
@param combo a combobox widget where the keypress occurred
@param event pointer to key event data struct
@param data pointerised gboolean, TRUE to cycle to other end of history when relevant

@return TRUE if pressed key was <Up> or <Down> and cycling not-necessary or allowed
*/
static gboolean _e2_combobox_key_press_cb (GtkWidget *combo, GdkEventKey *event,
	gpointer data)
{
	printd (DEBUG, "_e2_combobox_key_press_cb (combo:,wvent,data:)");
	if (!(event->keyval == GDK_Up || event->keyval == GDK_Down))
		return FALSE;

	NEEDCLOSEBGL
	if (event->keyval == GDK_Down &&
		(event->state & gtk_accelerator_get_default_mod_mask ()) == GDK_CONTROL_MASK)
	{
		gtk_combo_box_popup (GTK_COMBO_BOX (combo));
		NEEDOPENBGL
		return TRUE;
	}

	if (data == NULL)
	{
		NEEDOPENBGL
		return FALSE;	//no cycling allowed
	}

	gint pos = gtk_combo_box_get_active (GTK_COMBO_BOX (combo));
	if (event->keyval == GDK_Down)
	{
		if (pos < _e2_combobox_last_index ((GtkComboBox*)combo))
			pos++;	//assumes -1 can go to 0 always
		else
			pos = 0;
	}
	else //(event->keyval == GDK_Up)
	{
		if (pos > 0)
			pos--;
		else if (pos == 0 || pos == -1)
			pos = _e2_combobox_last_index ((GtkComboBox*)combo);
	}

	e2_combobox_set_active (combo, pos);
#ifdef USE_GTK2_24
	if (gtk_combo_box_get_has_entry ((GtkComboBox*)combo))
#else
	if (GTK_IS_COMBO_BOX_ENTRY (combo))
#endif
	{
		GtkWidget *entry =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (combo));
#else
			GTK_BIN (combo)->child;
#endif
		if (pos == -1)
			gtk_entry_set_text (GTK_ENTRY (entry), "");
		else
			gtk_editable_set_position (GTK_EDITABLE (entry), -1);
	}
	NEEDOPENBGL
	return TRUE;
}
/**
@brief "key-press-event" signal callback
These are default mechanisms, relevant only if not 'trumped' by a specific
key-binding e.g. in main-window dirlines and commandline.
<Alt>Delete keypress in the combobox's entry triggers deletion of current
string and any matching item(s) in @a history.
<Shift><Alt>Delete keypress in the combobox entry triggers deletion of
current string and the whole history list @a history (hopefully that's not
static if also used by other widgets)
<Shift>Delete clears text from cursor to end
@param entry the entry widget for the combo box
@param event pointer to event data struct
@param history pointer to history list for the combo box, or NULL

@return TRUE if the keypress was processed
*/
static gboolean _e2_combobox_key_press_cb2 (GtkWidget *entry, GdkEventKey *event,
	GList **history)
{
	gboolean retval = FALSE;
	if (event->keyval == GDK_Delete)
	{
		OPENBGL //TODO do this if NEEDCLOSEBGL is empty
		//prevent sporadic bad latency in UI updates
		gtk_widget_queue_draw (entry);
		CLOSEBGL

		NEEDCLOSEBGLX //do this if NEEDCLOSEBGL is empty
		guint modifiers = gtk_accelerator_get_default_mod_mask ();
		if ((event->state & modifiers) == GDK_SHIFT_MASK)
		{
			gint start = gtk_editable_get_position (GTK_EDITABLE (entry));
			gtk_editable_delete_text (GTK_EDITABLE (entry), start, -1);
			if (g_object_get_data (G_OBJECT (entry), "e2-dir-line") != NULL)
				e2_command_line_highlight (entry, FALSE);
			retval = TRUE;
		}
		else if ((event->state & modifiers) == GDK_MOD1_MASK)
		{
			const gchar *this = gtk_entry_get_text (GTK_ENTRY (entry));
			if (this != NULL)	//cannot happen ?
			{
				e2_combobox_clear_value (
#ifdef USE_GTK2_14
					gtk_widget_get_parent (entry),
#else
					entry->parent,
#endif
					this, TRUE);
			}
			retval = TRUE;
		}
		else if ((event->state & modifiers) == (GDK_SHIFT_MASK | GDK_MOD1_MASK))
		{
			GtkComboBox *combo;
			GtkTreeModel *model;

			gtk_entry_set_text (GTK_ENTRY (entry), "");
			combo = GTK_COMBO_BOX (
#ifdef USE_GTK2_14
				gtk_widget_get_parent (entry)
#else
				entry->parent
#endif
			);
			model = gtk_combo_box_get_model (combo);
			gtk_list_store_clear (GTK_LIST_STORE (model));
			if (history != NULL && *history != NULL)
				//too bad for any other widget using the history, if that's static
				e2_list_free_with_data (history);
			retval = TRUE;
		}
		NEEDOPENBGL
	}
	return retval;
}
/**
@brief "scroll-event" signal callback for the entry belonging to a GtkComboBoxEntry

@param entry the entry widget for the combo box
@param event pointer to event data struct
@param combo the parent widget

@return
*/
static gboolean _e2_combobox_scroll_cb (GtkWidget *entry, GdkEventScroll *event,
	GtkWidget *combo)
{
	gint pos;
	guint modifiers;
//	printd (DEBUG, "_e2_combobox_scroll_cb");
	if (event->direction == GDK_SCROLL_UP)
	{
		NEEDCLOSEBGL
		pos = gtk_combo_box_get_active (GTK_COMBO_BOX (combo));
		if (pos > 0)
		{
			modifiers = gtk_accelerator_get_default_mod_mask ();
			if ((event->state & modifiers) == GDK_CONTROL_MASK)
				e2_combobox_set_active (combo, 0);
			else
				e2_combobox_set_active (combo, pos-1);
		}
		NEEDOPENBGL
		return TRUE;
	}
	else if (event->direction == GDK_SCROLL_DOWN)
	{
		NEEDCLOSEBGL
		pos = gtk_combo_box_get_active (GTK_COMBO_BOX (combo));
		guint lastpos = _e2_combobox_last_index (GTK_COMBO_BOX (combo));
		if (pos == -1 || pos < lastpos)
		{
			modifiers = gtk_accelerator_get_default_mod_mask ();
			if ((event->state & modifiers) == GDK_CONTROL_MASK)
				e2_combobox_set_active (combo, lastpos);
			else
				e2_combobox_set_active (combo, pos+1);
		}
		NEEDOPENBGL
		return TRUE;
	}
	return FALSE;
}
/**
@brief "changed" signal callback for a GtkComboBoxEntry
When a history item is selected, if @a data is non-NULL (TRUE) the combobox's
entry is focused. If @a data is NULL (FALSE) then an "activated" signal is
emitted on the entry.
When the combobox entry is simply edited, nothing is done.
@param combo the combobox widget whose entry was changed
@param data pointerised gboolean, non-NULL (TRUE) if @a combo was created with
 the E2_COMBOBOX_FOCUS_ON_CHANGE flag set
@return
*/
static void _e2_combobox_changed_cb (GtkWidget *combo, gpointer data)
{
	printd (DEBUG, "_e2_combobox_changed_cb: combo:_ data: %s", (data == NULL) ? "FALSE":"TRUE");
	if (g_object_get_data (G_OBJECT (combo), "e2_changed_blocked") != NULL)
	{
//		printd (DEBUG, "_e2_combobox_changed_cb BLOCKED");
		return;
	}
	NEEDCLOSEBGL
	//active item will be -1 whenever an item is edited
	if (
#ifdef USE_GTK2_18
		gtk_widget_get_visible (combo)
#else
		GTK_WIDGET_VISIBLE (combo)
#endif
		&& gtk_combo_box_get_active (GTK_COMBO_BOX (combo)) != -1)
	{
		gboolean focus = GPOINTER_TO_INT (data);
		GtkWidget *entry =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (combo));
#else
			GTK_BIN (combo)->child;
#endif
		if (focus)
		{
			gtk_widget_grab_focus (entry);
			gtk_editable_set_position (GTK_EDITABLE (entry), -1);
		}
		else
		{
			printd (DEBUG, "issue activate signal for combobox entry");
			NEEDOPENBGL
			g_signal_emit_by_name (G_OBJECT (entry), "activate");
			return;
		}
	}
	NEEDOPENBGL
}

/**
@brief "activate" signal callback for the GtkEntry of a GtkComboBoxEntry combobox

This cb is not connected if the parent combobox is created with flag
E2_COMBOBOX_NO_AUTO_HISTORY.

@param entry entry of a GtkComboBoxEntry
@param data pointerised gboolean specified when callback was connected,
  non-NULL/TRUE if repeated history-entries are allowed

@return
*/
void e2_combobox_activated_cb (GtkWidget *entry, gpointer data)
{
	printd (DEBUG, "e2_combobox_activated_cb (entry: data:%x", data);
	NEEDCLOSEBGL
	GtkWidget *combo =
#ifdef USE_GTK2_14
		gtk_widget_get_parent (entry);
#else
		entry->parent;
#endif
	const gchar *text = gtk_entry_get_text (GTK_ENTRY (entry));
	if (text != NULL && *text != '\0')
	{
		gchar *newtext;
		gint limit;
		if (g_object_get_data (G_OBJECT (entry), "e2-dir-line") != NULL)
		{
			newtext = g_strdup (text);
			newtext = e2_utils_path_clean (newtext);
			gtk_entry_set_text ((GtkEntry*)entry, newtext);
			limit = -2;
		}
		else if (g_object_get_data (G_OBJECT (entry), "e2-command-line") != NULL)
		{
			newtext = (gchar*)text;
			limit = -1;
		}
		else
		{
			newtext = (gchar*)text;
			limit = 0;
		}
		e2_combobox_prepend_history (combo, newtext, limit, GPOINTER_TO_INT (data));
		if (newtext != (gchar*)text)
			g_free (newtext);
	}
	NEEDOPENBGL
}

  /******************/
 /***** public *****/
/******************/

/**
@brief determine whether the history model of @a combo has any content

@param combo the combo box to be interrogated

@return TRUE if the model has something
*/
gboolean e2_combobox_has_history (GtkComboBox *combo)
{
	GtkTreeModel *model = gtk_combo_box_get_model (combo);
	return (gtk_tree_model_iter_n_children (model, NULL) > 0);
}
/* *
@brief activate the last history item of combo box @a combo
This also puts the contents of the history line into the combo
entry, and moves the cursor to the end
@param combo  GtkComboBox to work on
@return
*/
/*void e2_combobox_select_last (GtkWidget *combo)
{
	GtkWidget *entry =
#ifdef USE_GTK2_14
		gtk_bin_get_child (GTK_BIN (combo));
#else
		GTK_BIN (combo)->child;
#endif
//	GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX (combo));
	gint count = _e2_combobox_last_index ((GTK_COMBO_BOX (combo));
	gchar *text = g_strdup (gtk_entry_get_text (GTK_ENTRY (entry)));
	gint pos = gtk_editable_get_position (GTK_EDITABLE (entry));

//	e2_combobox_block_changed (combo);	//CHECKME
	e2_combobox_set_active (combo, count);
//	_e2_combobox_unblock_changed (combo);	//CHECKME

	gtk_entry_set_text (GTK_ENTRY (entry), text);
	gtk_editable_set_position (GTK_EDITABLE (entry), pos);
	g_free (text);
} */
/* *
@brief get the string from the last row in the model of @a combo

@param combo the entry text combo box to be interrogated

@return newly-allocated copy of the string, or NULL if there's nothing
*/
/*gchar *e2_combobox_last_text (GtkComboBox *combo)
{
	gchar *text = NULL;
	GtkTreeModel *mdl = gtk_combo_box_get_model (combo);
	GtkTreeIter iter;
	guint count = gtk_tree_model_iter_n_children (mdl, NULL);
	if (count > 0
		&& gtk_tree_model_iter_nth_child (mdl, &iter, NULL, count-1))
		gtk_tree_model_get (mdl, &iter, 0, &text, -1);
	return text;
} */
/**
@brief get the string from the first row in the model of @a combo

@param combo the GtkComboBox or GtkComboBoxEntry to be interrogated

@return newly-allocated copy of the string, or NULL if there's nothing
*/
gchar *e2_combobox_first_text (GtkComboBox *combo)
{
	gchar *text = NULL;
	GtkTreeModel *mdl = gtk_combo_box_get_model (combo);
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_first (mdl, &iter))
		gtk_tree_model_get (mdl, &iter, 0, &text, -1);
	return text;
}
/**
@brief activate history item @a num of combo box @a combo

@param combo GtkComboBox or GtkComboBoxEntry to work on
@param num the integer index of the item to activate
@return
*/
void e2_combobox_set_active (GtkWidget *combo, gint num)
{
	printd (DEBUG, "e2_combobox_set_active: %d", num);
	e2_combobox_block_changed (combo);
	gtk_combo_box_set_active (GTK_COMBO_BOX (combo), num);
	_e2_combobox_unblock_changed (combo);
}
/**
@brief return currently-selected string from history
At this time, this func is only used for GtkComboBoxEntry's
@param combo GtkComboBox or GtkComboBoxEntry to be interrogated

@return newly-allocated string from history or NULL if nothing is selected
*/
gchar *e2_combobox_get_active_text (GtkWidget *combo)
{
	//we don't want to return entry-content, if that's inconsistent with selection
	if (gtk_combo_box_get_active (GTK_COMBO_BOX (combo)) != -1)
#ifdef USE_GTK2_24 //TODO
		return (gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (combo)));
#else
		return (gtk_combo_box_get_active_text (GTK_COMBO_BOX (combo)));
#endif
	return NULL;
}
/**
@brief clear any history item of @a combo corresponding to @a value, and if relevant, the displayed value
@param combo a combo widget with a text entry
@param value the string to be cleared
@param with_entry TRUE to also clear the displayed value
@return
*/
void e2_combobox_clear_value (GtkWidget *combo, const gchar *value, gboolean with_entry)
{
	GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX (combo));
	GtkTreeIter iter;

	if (gtk_tree_model_get_iter_first (model, &iter))
	{
		while (e2_tree_find_iter_from_str (model, 0, value, &iter, FALSE))
			gtk_list_store_remove (GTK_LIST_STORE (model), &iter);
	}

	GList **history = (GList**) g_object_get_data (G_OBJECT(combo), "e2-combo-history");
	if (history != NULL && *history != NULL)
	{
		GList *tmp;
		while ((tmp = g_list_find_custom (*history, value,
				(GCompareFunc) e2_list_strcmp)) != NULL)
		{
			g_free (tmp->data);
			*history = g_list_delete_link (*history, tmp);
		}
	}

	if (with_entry)
	{
		//now it's safe to change entry content (and therefore, possibly value)
		GtkWidget *entry =
#ifdef USE_GTK2_14
			gtk_bin_get_child (GTK_BIN (combo));
#else
			GTK_BIN(combo)->child;
#endif
		const gchar *now = gtk_entry_get_text (GTK_ENTRY(entry));
		if (!strcmp (now, value))
		{
			gtk_entry_set_text (GTK_ENTRY(entry), "");
			if (g_object_get_data (G_OBJECT (entry), "e2-dir-line") != NULL)
				e2_command_line_highlight (entry, FALSE);
		}
	}
}
/* *
@brief backup all of the history data of @a combo into @a history

@param combo the combo box to be interrogated
@parm list list of history-items

@return
*/
/*void e2_combobox_save_history (GtkWidget *combo, GList **list)
{
	if (*list != NULL)
		e2_list_free_with_data (list);
	GtkTreeIter iter;
	GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX (combo));
	if (gtk_tree_model_get_iter_first (model, &iter))
	{
		gchar *text;
		do
		{
			gtk_tree_model_get (model, &iter, 0, &text, -1);
			*list = g_list_append (*list, text);
		} while (gtk_tree_model_iter_next (model, &iter));
	}
}
*/
/**
@brief prepend @a newtext to history of @a combo

Does nothing if @a newtext is NULL or empty. Trailing whitespace is omitted from the entry.
After processing, the history will be shortened if needed.

@param combo a GtkComboBox, possibly one of those in a toolbar command-line
@param newtext the content to be added to the history
@param limit integer: -2 for dirline, -1 for commandline, 0 for no length-limit, N for limit N > 0
@param multi TRUE if repeated history-entries are allowed

@return
*/
void e2_combobox_prepend_history (GtkWidget *combo, const gchar *newtext,
	gint limit, gboolean multi)
{
	if (newtext != NULL)
	{
		gchar *text = g_strdup (newtext);
		//a directory path may properly have trailing whitespace, so no strip
		g_strchug (text);
		if (*text != '\0')
		{
			gboolean add = TRUE;
			GtkTreeIter iter;
			GtkTreeModel *model = gtk_combo_box_get_model (GTK_COMBO_BOX (combo));
			if (gtk_tree_model_get_iter_first (model, &iter))
			{
				if (multi)
				{
					//just prevent adjacent matching items
					gchar *first;
					gtk_tree_model_get (model, &iter, 0, &first, -1);
					if (first != NULL)
					{
						add = (strcmp (first, text) != 0);
						g_free (first);
					}
				}
				else
				{
					//remove all existing matching history-item(s)
					//actually, there should only ever be 1 match at most
					while (e2_tree_find_iter_from_str_simple (model, 0, text, &iter, FALSE))
					{
						GtkTreeIter test;
						test = iter;
						gboolean more = gtk_tree_model_iter_next (model, &test);
						gtk_list_store_remove (GTK_LIST_STORE (model), &iter); //after this, iter has data for next item, if any
						if (!more)
							break;
					}
				}
			}
			if (add)
			{
#ifdef USE_GTK2_24
				gtk_combo_box_text_prepend_text (GTK_COMBO_BOX_TEXT (combo), text);
#else
				gtk_combo_box_prepend_text (GTK_COMBO_BOX (combo), text);
#endif
			}
			e2_combobox_set_active (GTK_WIDGET (combo), 0);
			
			gint max;
			if (limit == -2)
				max = e2_option_int_get ("dir-line-history-max");
			else if (limit == -1) 
				max = e2_option_int_get ("command-line-history-max");
			else
				max = limit;
			if (max > 1) //ensure at least 1 entry
			{
				if (gtk_tree_model_iter_nth_child (model, &iter, NULL, max))
				{
					while (gtk_list_store_remove (GTK_LIST_STORE(model), &iter))
					{
//debugging				now--; 
					}
				}
			}
		}
		g_free (text);
	}
}
/**
@brief append a GList to a GtkComboBox's history

This function appends a GList of strings (@a list) to the history of a GtkComboBoxEntry
NOTE only for boxes created with gtk_combo_box_new_text ()
@param combo GtkComboBoxEntry to work on
@param list GList of utf8 strings

@return
*/
void e2_combobox_append_history (GtkWidget *combo, GList *list)
{
	GList *tmp;
	for (tmp = list; tmp != NULL; tmp = g_list_next (tmp))
#ifdef USE_GTK2_24
		gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (combo), tmp->data);
#else
		gtk_combo_box_append_text (GTK_COMBO_BOX (combo), tmp->data);
#endif
}
/**
@brief append string array to a GtkComboBox's history

NOTE only for boxes created with gtk_combo_box_new_text ()
@param combo combobox
@param num number of strings in @a array
@param array array of utf8 strings

@return
*/
void e2_combobox_append_history_counted (GtkWidget *combo, guint num, gchar **array)
{
	guint i;
	for (i = 0; i < num; i++)
#ifdef USE_GTK2_24
		gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (combo), array[i]);
#else
		gtk_combo_box_append_text (GTK_COMBO_BOX (combo), array[i]);
#endif
}
/**
@brief append NULL-terminated string array to a GtkComboBox's history

This function appends array of strings @a strv to the history of a GtkComboBox.
NOTE only for boxes created with gtk_combo_box_new_text ()
@param combo combobox
@param strv a null-terminated array of utf8 strings

@return
*/
void e2_combobox_append_history_strv (GtkWidget *combo, gchar **strv)
{
	guint i;
	for (i = 0; strv[i] != NULL; i++)
#ifdef USE_GTK2_24
		gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (combo), strv[i]);
#else
		gtk_combo_box_append_text (GTK_COMBO_BOX (combo), strv[i]);
#endif
}
/**
@brief create a GtkComboBox

This function creates a GtkComboBox or GtkComboBoxEntry, in either case for text-
only, respecting the @a flags, and then for a GtkComboBoxEntry, connects
@a activated_cb to the "activate" signal of the GtkComboBox entry.
@a activate_data will be a pointer to E2_CommandLineRuntime struct for the
command line, or generally NULL for an ad-hoc combobox
@a history size figures prominently in any (downstream) clearing of the combobox
model, so it should not be static if there's a prospect of multiple widgets
clearing the history
If E2_COMBOBOX_MENU_STYLE flag is not set, on gtk2, calls gtk_widget_set_name()
which MAY? need BGL closed
The returned widget is not shown.

@param activate_cb "activate" callback for the combobox entry, or NULL
@param activate_data data to provide to the activate callback
@param history history-list pointer, or NULL
@param flags flags to influence the usage of the GtkComboBox

@return the created GtkComboBox
*/
GtkWidget *e2_combobox_get (void (*activate_cb)(GtkEntry*,gpointer),
	gpointer activate_data, GList **history, E2_ComboBoxFlags flags)
{
	GtkWidget *combo = (flags & E2_COMBOBOX_HAS_ENTRY) ?
#ifdef USE_GTK2_24
		gtk_combo_box_text_new_with_entry () : gtk_combo_box_text_new ();
#else
		gtk_combo_box_entry_new_text () : gtk_combo_box_new_text ();
#endif

	if (!(flags & E2_COMBOBOX_MENU_STYLE))
	{
		//early gtk3 cannot cope with combobox styling ?
#ifdef USE_GTK3_6
		//use distinct style-object for each combo, cuz all are destroyed during any config window-recreation
		GtkCssProvider *liststyle = gtk_css_provider_new ();
		const gchar *cssdata = "GtkComboBox { -GtkComboBox-appears-as-list:1; }";
		GError *err = NULL;
		if (gtk_css_provider_load_from_data (liststyle, cssdata, -1, &err))
		{
			GtkStyleContext *sc = gtk_widget_get_style_context (combo);
			gtk_style_context_add_provider (sc, GTK_STYLE_PROVIDER(liststyle),
				GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
		}
		else
		{
			printd (WARN, "Combobox styling failure: %s", err->message);
			g_error_free (err);
		}
		g_object_unref (G_OBJECT (liststyle)); //back to refcount=1, or 0
#elif !defined(USE_GTK3_0)
		static gboolean style_parsed = FALSE;
		if (!style_parsed)
		{
			gtk_rc_parse_string (
			"style \"list-style-style\" { GtkComboBox::appears-as-list = 1 } "
			"widget \"*.list-style\" style \"list-style-style\"");
			style_parsed = TRUE;
		}
		gtk_widget_set_name (combo, "list-style");
#endif
	}
	if (history != NULL)
		e2_combobox_append_history (combo, *history);

	if (flags & E2_COMBOBOX_HAS_ENTRY)
	{
#ifdef USE_GTK2_14
		GtkWidget *child = gtk_bin_get_child (GTK_BIN (combo));
#endif
		 //<Delete> processing, no key translation needed, NULL history ok
		g_signal_connect (G_OBJECT
#ifdef USE_GTK2_14
			(child),
#else
			(GTK_BIN (combo)->child),
#endif
			"key-press-event",
			G_CALLBACK (_e2_combobox_key_press_cb2), history);

		if (history != NULL)
		{
			g_object_set_data (G_OBJECT(combo), "e2-combo-history", history);
			//enable prompt UI updates
			g_signal_connect (G_OBJECT
#ifdef USE_GTK2_14
				(child),
#else
				(GTK_BIN (combo)->child),
#endif
				"scroll-event",
				G_CALLBACK (_e2_combobox_scroll_cb), combo);

			if (!(flags & E2_COMBOBOX_NO_AUTO_HISTORY))
				g_signal_connect (G_OBJECT
#ifdef USE_GTK2_14
					(child),
#else
					(GTK_BIN (combo)->child),
#endif
					"activate",
					G_CALLBACK (e2_combobox_activated_cb),
					GINT_TO_POINTER (flags & E2_COMBOBOX_ALLOW_DOUBLE));
		}
		//connect caller-specified callback for "activate" signal to combobox's entry
		if (activate_cb != NULL)
			g_signal_connect (G_OBJECT
#ifdef USE_GTK2_14
				(child),
#else
				(GTK_BIN (combo)->child),
#endif
				"activate",
				G_CALLBACK (activate_cb), activate_data);
		//enable focusing etc
		g_signal_connect (G_OBJECT (combo), "changed",
			G_CALLBACK (_e2_combobox_changed_cb),
			GINT_TO_POINTER (flags & E2_COMBOBOX_FOCUS_ON_CHANGE));
	}

	g_signal_connect (G_OBJECT ((combo)), "key-press-event",
		G_CALLBACK (_e2_combobox_key_press_cb),
		GUINT_TO_POINTER (flags & E2_COMBOBOX_CYCLE_HISTORY));

	return combo;
}
/**
@brief pack a new GtkComboBox into @a box

Creates a GtkComboBox using e2_combobox_get(), and then packs it into @a box
and connects @a func to the "activate" signal.
If E2_COMBOBOX_MENU_STYLE flag is not set, on gtk2, downstream calls
gtk_widget_set_name() which MAY? need BGL closed.
None of the nested widgets is shown.

@param box parent box to add the GtkComboBox to
@param expand packing property
@param padding packing property
@param activate_cb entry activate-signal callback, or NULL
@param activate_data callback data
@param history combo history list, or NULL
@param flags flags to influence the usage of the GtkComboBox

@return the GtkComboBox that was created and packed
*/
GtkWidget *e2_combobox_add (GtkWidget *box, gboolean expand, guint padding,
	void (*activate_cb)(GtkEntry*,gpointer), gpointer activate_data,
	GList **history, E2_ComboBoxFlags flags)
{
	GtkWidget *combo = e2_combobox_get (activate_cb, activate_data, history, flags);
	//add padding to the left & right sides of the combobox
	GtkWidget *pad_box;
#ifdef USE_GTK3_0
	if (gtk_orientable_get_orientation(GTK_ORIENTABLE (box)) == GTK_ORIENTATION_HORIZONTAL)
		pad_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, padding);
	else
		pad_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, padding);
#else
	if (GTK_IS_HBOX (box))
		pad_box = gtk_vbox_new (FALSE, padding);
	else
		pad_box = gtk_hbox_new (FALSE, padding);
#endif

	//now add the combo to the padding container
	gtk_box_pack_start (GTK_BOX (pad_box), combo, TRUE, TRUE, padding);
	//add padding container to parent container
	gtk_box_pack_start (GTK_BOX (box), pad_box, expand, expand, padding);
	return combo;
}
