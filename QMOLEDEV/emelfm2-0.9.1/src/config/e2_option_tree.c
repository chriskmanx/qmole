/* $Id: e2_option_tree.c 3036 2014-01-26 20:43:35Z tpgww $

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
@file src/config/e2_option_tree.c
@brief functions to handle tree options

Functions to handle tree options
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_option_tree.h"
#include "e2_dialog.h"
#ifdef E2_MOUSECUSTOM
# include "e2_mousebinding.h"
#endif
#include "e2_keybinding.h"
#include "e2_plugins.h"
#include "e2_icons.h"

//data struct for a deferred tree-option overwrite dialog
typedef struct _E2_OptionTreeCheck
{
	E2_OptionSet *set; //pointer to set data struct
	GtkTreeRowReference *ref;
	gint colnum;
	gboolean *lock;
	gchar *newvalue;
	gchar *prompt;
} E2_OptionTreeCheck;

//data struct for key-binding change
typedef struct _E2_KeyChangeData
{
	E2_OptionSet *set; //pointer to set data struct
	gchar *path_string; //string form of gtk tree path to the row to be amended
	GtkCellRenderer *renderer;
	gchar *newtext;
} E2_KeyChangeData;

#ifdef E2_MOUSECUSTOM
//data struct for button-binding change, may eventually differ from keys data struct
typedef struct _E2_BtnChangeData
{
	E2_OptionSet *set; //pointer to set data struct
	gchar *path_string; //string form of gtk tree path to the row to be amended
	GtkCellRenderer *renderer;
	gchar *newtext;
} E2_BtnChangeData;
#endif

static void _e2_option_tree_set (E2_OptionSet *set, GtkTreeIter *iter, gint col,
	void *data);

extern GtkWidget *config_dialog;
static void _e2_option_tree_add_default (E2_OptionSet *set, GtkTreeIter *iter,
	GtkTreeIter *parent, gboolean sibling);
static void _e2_option_tree_move (E2_OptionSet *set, GtkTreeIter *iter,
	GtkTreePath *path_dest, gboolean sibling, gboolean before);
static gboolean _e2_option_tree_add_below_cb (GtkWidget *widget, E2_OptionSet *set);
static gboolean _e2_option_tree_add_child_cb (GtkWidget *widget, E2_OptionSet *set);
static gboolean _e2_option_tree_move_up_cb (GtkWidget *widget, E2_OptionSet *set);
static gboolean _e2_option_tree_move_down_cb (GtkWidget *widget, E2_OptionSet *set);
static gboolean _e2_option_tree_button_press_cb (GtkWidget *treeview,
	GdkEventButton *event, E2_OptionSet *set);
static void *_e2_option_tree_get_void_simple (E2_OptionTreeColumn *col, gchar *option);
static void _e2_option_tree_key_edit_start_cb (GtkCellRenderer *renderer,
	GtkCellEditable *editable, gchar *path_string, E2_OptionSet *set);
#ifdef E2_MOUSECUSTOM
static void _e2_option_tree_btn_edit_start_cb (GtkCellRenderer *renderer,
	GtkCellEditable *editable, gchar *path_string, E2_OptionSet *set);
#endif
static void _e2_option_tree_unblock_mnemonics (void);

extern GtkTreeStore *actions_store;

  /************************/
 /***** context menu *****/
/************************/

#define INCLUDED_IN_PARENT
#include "e2_option_tree_context_menu.c"
#undef INCLUDED_IN_PARENT

/**
@brief create flag for the config dialog, to signal that the set's tree data has been modified

@param set pointer to data struct for the option that the dialog configures

@return
*/
void e2_option_tree_flag_change (E2_OptionSet *set)
{
	set->ex.tree.flags |= E2_OPTION_TREE_SET_EDITED;
}

  /**************************/
 /***** data functions *****/
/**************************/
/*These functions set some properties of a treeview cell instead of using the
straight mapping between the cell and the treemodel. They are called whenever
the cell is rendered, and at the end of (not during) cell-editing */
static void _e2_option_tree_bool_data_func (GtkTreeViewColumn *tree_column,
	GtkCellRenderer *cell, GtkTreeModel *model, GtkTreeIter *iter,
	E2_OptionTreeColumn *col)
{
	gint column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column"));

	gboolean b;
	gtk_tree_model_get (model, iter, column, &b, -1);

	gboolean visible;
	if (col->visible_check_func != NULL)
	{
		gboolean (*fun) (GtkTreeModel *, GtkTreeIter *, GtkCellRenderer *,
			gpointer) = col->visible_check_func;
		visible = fun (model, iter, cell,  col->visible_check_data);
	}
	else
		visible = TRUE;
	//in spite of gtk API doc, setting "visible" property to a bool value spits an error message
	g_object_set (G_OBJECT (cell), "visible", (visible)?1:0, "active", b, NULL);
}

static void _e2_option_tree_str_data_func (GtkTreeViewColumn *tree_column,
	GtkCellRenderer *cell, GtkTreeModel *model, GtkTreeIter *iter,
	E2_OptionTreeColumn *col)
{
	gint column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column"));

	gchar *str;
	gtk_tree_model_get (model, iter, column, &str, -1);

	gboolean visible;
	if (col->visible_check_func != NULL)
	{
		gboolean (*fun) (GtkTreeModel *, GtkTreeIter *, GtkCellRenderer *,
			gpointer) = col->visible_check_func;
		visible = fun (model, iter, cell,  col->visible_check_data);
	}
	else
		visible = TRUE;
	//in spite of gtk API doc, setting "visible" property to a bool value spits an error message
	g_object_set (G_OBJECT (cell), "visible", (visible)?1:0, "text", str, NULL);
	g_free (str);
}

  /*********************/
 /***** callbacks *****/
/*********************/

/* the relevant one of funcs is called whenever an edited cell is re-rendered,
and when the edit is finished, and when Esc is pressed (in the latter case,
as part of the widget_destroy process for the config dialog
They set treestore data in accord with the renderer */

/**
@brief save edited text value in the underlying treestore

@param renderer the renderer for the cell
@param path_string string form of gtk tree path to the row to be amended
@param new_text replacement text string for the cell
@param set pointer to set data struct

@return
*/
static void _e2_option_tree_string_edited_cb (GtkCellRendererText *cell,
	gchar *path_string, gchar *new_text, E2_OptionSet *set)
{
	if (new_text == NULL)	//this probably can't happen
		return;
	printd (DEBUG, "tree-option edited cb, new text is %s", new_text);
	if (!strcmp (set->name, "keybindings")
		&& !strcmp (new_text, _("Press key")))
	{
		e2_keybinding_unblock (config_dialog, NULL);
		_e2_option_tree_unblock_mnemonics ();
		return;
	}

	NEEDCLOSEBGL
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (set->ex.tree.model),
		&iter, path_string))
	{
		gint column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column"));
		_e2_option_tree_set (set, &iter, column, (gchar *)new_text);
	}
	//clean up
	//revert focus to edited row
//CHECKME when editing done, focus the cancel button ??
	gtk_widget_grab_focus (set->widget);
	NEEDOPENBGL
}
/**
@brief save toggled boolean value in the underlying treestore

@param renderer the renderer for the cell
@param path_string string form of gtk tree path to the row to be amended
@param set pointer to set data struct

@return
*/
static void _e2_option_tree_toggle_cb (GtkCellRendererToggle *cell,
	const gchar *path_str, E2_OptionSet *set)
{
	//find out where
	GtkTreePath *path = gtk_tree_path_new_from_string (path_str);
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (set->ex.tree.model, &iter, path))
	{
		gint column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column"));
		//invert current value
		gboolean toggle_item;
		gtk_tree_model_get (set->ex.tree.model, &iter, column, &toggle_item, -1);
		toggle_item ^= 1;
		NEEDCLOSEBGL
		//change model
		gtk_tree_store_set (GTK_TREE_STORE (set->ex.tree.model), &iter, column, toggle_item, -1);
		NEEDOPENBGL
		e2_option_tree_flag_change (set);
	}
	//clean up
	gtk_tree_path_free (path);
}

static void _e2_option_tree_cell_pixbuf_data_func (GtkTreeViewColumn *tree_column,
	GtkCellRenderer *cell, GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
#ifdef E2_ICONCACHE
	GdkPixbuf *pxb;
#endif
	gint column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (tree_column), "column"));
	gchar *icon;
	gtk_tree_model_get (model, iter, column, &icon, -1);
	if (icon != NULL && *icon != '\0')
	{
#ifdef E2_ICONCACHE
		GtkIconSize sz = GPOINTER_TO_INT (data);
		pxb = e2_icons_get_puxbuf (icon, sz, TRUE);
#else
		if (e2_icons_check_stock (icon))
		{
			g_object_set (G_OBJECT (cell), "stock-id", icon, NULL);
			g_object_set (G_OBJECT (cell), "pixbuf", NULL, NULL);
		}
		else	//not a stock item
		{
			GdkPixbuf *pixbuf;
			GdkPixbuf *pixbuf2;
			if ((pixbuf = gdk_pixbuf_new_from_file (icon, NULL)) != NULL)
			{
				pixbuf2 = pixbuf;
				pixbuf = gdk_pixbuf_scale_simple (pixbuf2, 16, 16, GDK_INTERP_BILINEAR);
				g_object_unref (pixbuf2);
			}
			g_object_set (G_OBJECT (cell), "stock-id", NULL, NULL);
			g_object_set (G_OBJECT (cell), "pixbuf", pixbuf, NULL);
			if (pixbuf != NULL)
				g_object_unref (pixbuf);
		}
#endif
	}
	else
#ifdef E2_ICONCACHE
		pxb = NULL;

	g_object_set (G_OBJECT (cell), "pixbuf", pxb, NULL);
#else
	{
		g_object_set (G_OBJECT (cell), "stock-id", NULL, NULL);
		g_object_set (G_OBJECT (cell), "pixbuf", NULL, NULL);
	}
#endif
	g_free (icon);
}
/**
@brief dummy edit-done handler for key- and button-cells
@param editable the cell-renderer's editable data struct
This must exist, even if it does nothing
@return
*/
static void _e2_option_tree_fake_edit_done (GtkCellEditable *editable)
{
//	printd (DEBUG, "_e2_option_tree_fake_edit_done");
}
#ifdef E2_MOUSECUSTOM
/**
@brief timer callback to check for and warn about foolish or duplicate button-binding
This is outside other cell-related cb's to prevent crash
@param data pointer to keyvhange data struct

@return FALSE always to cancel timer
*/
static gboolean _e2_option_tree_btnchange_check (E2_BtnChangeData *data)
{
	gchar *prompt;
	DialogButtons result;
	guint btnsmask;
	GdkModifierType state;

	if (!e2_mousebinding_parse_name (data->newtext, &btnsmask, &state, NULL, TRUE))
		result = CANCEL;
	else if ((btnsmask == 1 && (state & ~E2_OVERRIDE_MASK) == 0) //button 1 without some 'unusual' mod
		  || (btnsmask == 4 && state == 0))  //button 3 without _any_ mod
	{
		prompt = g_markup_printf_escaped (
			_("Changing button %s may block normal operation"), data->newtext);
		CLOSEBGL
		result = e2_dialog_warning (prompt, NULL);
		OPENBGL
		g_free (prompt);
	}
	else
		result = OK;

	if (result == OK)
	{
		GtkTreeView *treeview = GTK_TREE_VIEW (data->set->widget);
		GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
		GtkTreeIter iter;
		if (gtk_tree_selection_get_selected (sel, NULL, &iter))
		{
			GtkTreeIter parent, srch_iter;
			GtkTreeModel *mdl = data->set->ex.tree.model;
			gchar *new_seq, *new_btnname = data->newtext;
			gboolean new_dbl, new_trpl;
			gboolean button = !strcmp (data->set->name, "mousebuttons");
			gboolean stroke = !strcmp (data->set->name, "mousedrags");

			if (button)
			{
				new_seq = NULL;
				gtk_tree_model_get (mdl, &iter,
					MCOL_DBL, &new_dbl, MCOL_TRP, &new_trpl, -1);
#ifdef MCOL_REL
  FIXME include release
#endif
			}
			else if (stroke)
			{
				new_dbl = new_trpl = FALSE;
				gtk_tree_model_get (mdl, &iter,
					3, &new_seq, -1);
			}
			else
			{
				g_return_val_if_reached (FALSE);
			}

			gtk_tree_model_iter_parent (mdl, &parent, &iter);
			gtk_tree_model_iter_children (mdl, &srch_iter, &parent);
			do
			{
				gchar *storedbtn;
				gtk_tree_model_get (mdl, &srch_iter, MCOL_BTN, &storedbtn, -1);
				if (!strcmp (storedbtn, new_btnname)
					&& (srch_iter.stamp != iter.stamp
					 || srch_iter.user_data != iter.user_data
					 || srch_iter.user_data2 != iter.user_data2
					 || srch_iter.user_data3 != iter.user_data3
						))
				{
					gboolean matched;
					gchar *action, *arg;
					if (stroke)
					{
						gchar *sequence;
						gtk_tree_model_get (mdl, &srch_iter,
							3, &sequence, 4, &action, 5, &arg, -1);
						matched = !strcmp (new_seq, sequence);
						g_free (sequence);
					}
					else
					{
						gboolean dbl, trpl;
						gtk_tree_model_get (mdl, &srch_iter,
							MCOL_DBL, &dbl, MCOL_TRP, &trpl,
							MCOL_ACT, &action, MCOL_ARG, &arg, -1);
#ifdef MCOL_REL
  FIXME include release
#endif
						matched = (dbl == new_dbl && trpl == new_trpl);
					}

					if (matched)
					{
						gchar *fmt = _("That combination is assigned to '%s'");
						if (*action != '\0' && *arg != '\0')
						{
							gchar *fmt2 = g_strdup_printf (fmt, "%s %s"); //show arg too
							prompt = g_markup_printf_escaped (fmt2, action, arg);
							g_free (fmt2);
						}
						else
							prompt = g_markup_printf_escaped (fmt, action);

						CLOSEBGL
						result = e2_dialog_warning (prompt, NULL);
						OPENBGL
						g_free (prompt);
						g_free (storedbtn);
						g_free (action);
						g_free (arg);
						break;
					}
					g_free (action);
					g_free (arg);
				}
				g_free (storedbtn);
			} while (gtk_tree_model_iter_next (mdl, &srch_iter));

			if (new_seq != NULL)
				g_free (new_seq);
			if (result == OK)
			{
				g_signal_handlers_block_by_func (data->renderer,
					_e2_option_tree_btn_edit_start_cb, data->set);
				_e2_option_tree_string_edited_cb (GTK_CELL_RENDERER_TEXT (data->renderer),
					data->path_string, new_btnname, data->set);
				g_signal_handlers_unblock_by_func (data->renderer,
					_e2_option_tree_btn_edit_start_cb, data->set);
			}
		}
	}
	//CHECKME cancel the original btn press cb on the renderer
//	g_signal_handlers_disconnect_by_func ((gpointer)data->entry,
//		_e2_option_tree_btnchange_cb, data);
	g_free (data->path_string);
	g_free (data->newtext);
	DEALLOCATE (E2_BtnChangeData, data);
	return FALSE;
}
/**
@brief set button name determined from button press event @a event

This is a callback after a button cell edit begins

@param entry the entry widget associated with the cell renderer
@param event pointer to event data struct for the pressed button
@param data pointer to data needed to update the cell the renderer

@return TRUE always to prevent other btnpress handlers
*/
static gboolean _e2_option_tree_btnchange_cb (GtkEntry *entry,
	GdkEventButton *event, E2_BtnChangeData *data)
{
	printd (DEBUG, "_e2_option_tree_btnchange_cb");
#ifdef DEBUG_MESSAGES
	if (event->type == GDK_BUTTON_RELEASE)
		printd (DEBUG, "release event");
	else
	{
		guint count;
		switch (event->type)
		{
			case GDK_2BUTTON_PRESS:
				count = 2;
				break;
			case GDK_3BUTTON_PRESS:
				count = 3;
				break;
			default:
				count = 1;
				break;
		}
		printd (DEBUG, "%u-press event", count);
	}
#endif
	guint32 bmask = 1 << (event->button - 1);
	guint32 last_buttons = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (entry), "__BMASK__"));
	NEEDCLOSEBGL
	if (event->type == GDK_BUTTON_PRESS)
	{
		bmask |= last_buttons;
		g_object_set_data (G_OBJECT (entry), "__BMASK__", GUINT_TO_POINTER (bmask));

		guint32 last_state = GPOINTER_TO_UINT (g_object_get_data (G_OBJECT (entry), "__KMASK__"));
		GdkModifierType state = (event->state & E2_MODIFIER_MASK)	//ignore Mod2 (numlock) etc
							| last_state;

		GString *joined = g_string_sized_new (32);
		//convert button and modifiers to ascii
		guint keyval = GDK_0;
		gchar *tmp = gtk_accelerator_name (keyval, state);
		if (tmp != NULL)
		{
			joined = g_string_assign (joined, tmp);
			//ignore trailing "0"
			joined = g_string_truncate (joined, joined->len - 1);
			g_free (tmp);
			g_object_set_data (G_OBJECT (entry), "__KMASK__", GUINT_TO_POINTER (state));
		}
		//parse the buttons mask
		guint walker;
		for (walker = 0; walker < 32; walker++)
		{
			if (bmask & 0x1)
				g_string_append_printf (joined, "%u+", walker + 1);
			bmask >>= 1;
		}
		//ignore trailing "+"
		joined = g_string_truncate (joined, joined->len - 1);
		if (data->newtext != NULL)
			g_free (data->newtext);
		data->newtext = g_string_free (joined, FALSE);

		printd (DEBUG, "stored %s in cb data", data->newtext);
		gtk_entry_set_text (entry, data->newtext);
	}
	else if (event->type == GDK_2BUTTON_PRESS || event->type == GDK_2BUTTON_PRESS)
	{
		//FIXME this changes focus and therefore ends the edit
		if (!strcmp (data->set->name, "mousebuttons"))
		{
			GtkTreeIter iter;
			GtkTreeModel *mdl = data->set->ex.tree.model;
			if (gtk_tree_model_get_iter_from_string (mdl, &iter, data->path_string))
			{
				gboolean dbl = (event->type == GDK_2BUTTON_PRESS);
				gtk_tree_store_set (GTK_TREE_STORE (mdl), &iter,
					MCOL_DBL, dbl, MCOL_TRP, !dbl,
#ifdef MCOL_REL
					//MCOL_REL, FALSE, //? release is allowed for multi-clicks
#endif
					-1);
			}
		}
	}
	else if (event->type == GDK_BUTTON_RELEASE)
	{
		bmask = ~bmask & GPOINTER_TO_UINT (last_buttons);
		if (bmask > 0)
		{
			g_object_set_data (G_OBJECT (entry), "__BMASK__", GUINT_TO_POINTER (bmask));
		}
		else
		{
//			printd (DEBUG, "all buttons gone");
			//FIXME storing new data ends the edit, so prevents double-clicks
			_e2_option_tree_btnchange_check (data); //verify before storing
			//FIXME also unblock bindings if the user cancels or changes focus etc
//			if (!strcmp (data->set->name, "mousebuttons"))
				e2_mousebinding_unblock (config_dialog, NULL);
//			else //"mousedrags"
//				e2_mousegesture_unblock (config_dialog, NULL); insurance ?
		}
	}
	NEEDOPENBGL
//	printd (DEBUG, "_e2_option_tree_btnchange_cb ends");
	return TRUE;
}
/**
@brief key-press callback for button being edited

@param entry the entry widget associated with the cell renderer
@param event pointer to event data struct for the pressed button
@param data UNUSED pointer to data specified when callback connected

@return TRUE to prevent other keypress handlers unless key is <Enter> or <Esc>
*/
static gboolean _e2_option_tree_btnkey_cb (GtkWidget *entry,
	GdkEventKey *event, gpointer data)
{
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	return (!
	  ( event->keyval == GDK_Escape
/*return press just ends, no content change
	 || event->keyval == GDK_Return
	 || event->keyval == GDK_KP_Enter
	 || event->keyval == GDK_ISO_Enter
	 || event->keyval == GDK_3270_Enter */ ));
}
/**
@brief setup to amend the button name in a treeview cell

@param renderer the renderer for the cell
@param editable the interface to @a cell
@param path_string string form of gtk tree path to the row to be amended
@param set pointer to set data struct

@return
*/
static void _e2_option_tree_btn_edit_start_cb (GtkCellRenderer *renderer,
	GtkCellEditable *editable, gchar *path_string, E2_OptionSet *set)
{
//	printd (DEBUG, "start button edit cb");
	if (GTK_IS_ENTRY (editable))
    {
		E2_BtnChangeData *data = ALLOCATE (E2_BtnChangeData);
		CHECKALLOCATEDWARN (data, return;)
		data->set = set;
		data->path_string = g_strdup (path_string);
		data->renderer = renderer;
		data->newtext = NULL;	//in case it's free'd
		GtkEntry *entry = GTK_ENTRY (editable);
		NEEDCLOSEBGL
		gtk_entry_set_text (entry, _("Press button"));
		NEEDOPENBGL
		//assign a dummy func to prevent the cell-edit from ending upon 1st btn-press
		GtkCellEditableIface *api = GTK_CELL_EDITABLE_GET_IFACE (editable);
		api->editing_done = _e2_option_tree_fake_edit_done;

		//prevent operation of any aberrant current binding
//		if (!strcmp (set->name, "mousebuttons"))
			e2_mousebinding_block (config_dialog, NULL);
//		else //"mousedrags"
//			e2_mousegesture_block (config_dialog, NULL); insurance ?

		g_signal_connect (G_OBJECT (entry), "button-press-event",
			G_CALLBACK (_e2_option_tree_btnchange_cb), data);
		g_signal_connect (G_OBJECT (entry), "button-release-event",
			G_CALLBACK (_e2_option_tree_btnchange_cb), data);
		//block most keypresses
		g_signal_connect (G_OBJECT (entry), "key-press-event",
			G_CALLBACK (_e2_option_tree_btnkey_cb), NULL);
    }
}
#endif //def E2_MOUSECUSTOM
/**
@brief check for and warn about a duplicate keybinding
Called only from within callbacks
@param data pointer to keychange data struct

@return
*/
static void _e2_option_tree_keychange_check (E2_KeyChangeData *data)
{
	GtkTreeView *treeview = GTK_TREE_VIEW (data->set->widget);
	GtkTreeSelection *sel = gtk_tree_view_get_selection (treeview);
	GtkTreeIter iter;
	if (gtk_tree_selection_get_selected (sel, NULL, &iter))
	{
		DialogButtons result = OK;
		GtkTreeIter parent, srch_iter;
		GtkTreeModel *mdl = data->set->ex.tree.model;
		gchar *keyname = data->newtext;
		gtk_tree_model_iter_parent (mdl, &parent, &iter);
		gtk_tree_model_iter_children (mdl, &srch_iter, &parent);
		do
		{
			gchar *storedkey, *action, *arg;
			gtk_tree_model_get (mdl, &srch_iter, 1, &storedkey,
				3, &action, 4, &arg, -1);
			if (!strcmp (storedkey, keyname)
				&& (srch_iter.stamp != iter.stamp
				 || srch_iter.user_data != iter.user_data
				 || srch_iter.user_data2 != iter.user_data2
				 || srch_iter.user_data3 != iter.user_data3
					))
			{
				gchar *prompt, *fmt = _("%s is assigned to '%s'");
				if (*action != '\0' && *arg != '\0')
				{
					gchar *fmt2 = g_strdup_printf (fmt, keyname, "%s %s"); //show arg too
					prompt = g_markup_printf_escaped (fmt2, action, arg);
					g_free (fmt2);
				}
				else
					prompt = g_markup_printf_escaped (fmt, keyname, action);

//in cb			CLOSEBGL
				result = e2_dialog_warning (prompt, NULL);
//				OPENBGL
				g_free (prompt);
				g_free (storedkey);
				g_free (action);
				g_free (arg);
				break;
			}
			g_free (storedkey);
			g_free (action);
			g_free (arg);
		} while (gtk_tree_model_iter_next (mdl, &srch_iter));

		if (result == OK)
		{
			g_signal_handlers_block_by_func (data->renderer,
				_e2_option_tree_key_edit_start_cb, data->set);
			NEEDOPENBGL
			g_signal_emit_by_name (data->renderer, "edited", data->path_string,
				keyname, data->set);
			NEEDCLOSEBGL
			g_signal_handlers_unblock_by_func (data->renderer,
				_e2_option_tree_key_edit_start_cb, data->set);
		}
	}
}
/**
@brief interpret key name determined from key press event @a event
This is a callback after a key cell edit begins.
Stored value is not changed until after check in timer callback
@param entry the entry widget associated with the cell renderer
@param event pointer to event data struct for the pressed key
@param data pointer to data needed to update the cell the renderer

@return TRUE always, to prevent any other key-press-event handler
*/
static gboolean _e2_option_tree_keychange_cb (GtkEntry *entry,
	GdkEventKey *event, E2_KeyChangeData *data)
{
//	printd (DEBUG, "key change cb");
#ifdef USE_GTK2_10
	if (event->is_modifier)
#else
	if (e2_utils_key_is_modifier (event))
#endif
		return FALSE;	//keep waiting

	//finished with this cb now that we have a real key
	g_signal_handlers_disconnect_by_func ((gpointer)entry,
		_e2_option_tree_keychange_cb, data);

	guint keyval = event->keyval;
	GdkModifierType mask = event->state;
	if (!gtk_accelerator_valid (keyval, mask))
	{
		NEEDCLOSEBGL
		gtk_widget_grab_focus (data->set->widget);
		NEEDOPENBGL
		g_free (data->path_string);
		DEALLOCATE (E2_KeyChangeData, data);
		return FALSE;
	}
	mask &= ~(GDK_MOD2_MASK);	//ignore Mod2 (numlock)
	//<Esc> always cancels (so unmodified, it can't be set as a binding here)
	if (keyval == GDK_Escape && (mask & E2_MODIFIER_MASK) == 0)
	{
		NEEDCLOSEBGL
		gtk_widget_grab_focus (data->set->widget);
		NEEDOPENBGL
		g_free (data->path_string);
		DEALLOCATE (E2_KeyChangeData, data);
		return TRUE;
	}
	gchar *keyname;
	if (keyval < 0xF000 || keyval > 0xFFFF)
	{
		if ((mask & E2_MODIFIER_MASK) == GDK_SHIFT_MASK)
		{	//shft only, no ctrl alt etc
			if (mask & GDK_LOCK_MASK)
			{
				//lcase already, we don't want <Shift>
			}
			else
			{	//nocaps + shift >> ucase
				keyval = gdk_keyval_to_upper (keyval);
			}
			keyname = g_strdup (gdk_keyval_name (keyval));
		}
		else if ((mask & GDK_LOCK_MASK) && (mask & E2_MODIFIER_MASK) == 0)
			//only lock is active
			keyname = g_strdup (gdk_keyval_name (gdk_keyval_to_upper (keyval))); //3
		else
			keyname = gtk_accelerator_name (keyval, mask); //e.g. <Shift><Control>g for keyval 0x67 4
	}
	else
		keyname = gtk_accelerator_name (keyval, mask);

	data->newtext = keyname;
	NEEDCLOSEBGL
	//NOTE: keyval changed as appropriate in _e2_keybinding_sync_one() ?
	_e2_option_tree_keychange_check (data);
	NEEDOPENBGL
	//CHECKME also unblock bindings if the user cancels, changes focus etc
	e2_keybinding_unblock (config_dialog, NULL);
	_e2_option_tree_unblock_mnemonics ();
	return TRUE;
}

/* block mnemonic activation of dialog buttons while doing a key-binding */

typedef struct _E2_KeyBlock
{
	gint blocked; //flag for preventing re-entrance
	E2_KeyChangeData *data;
} E2_KeyBlock;
//static data used cuz only one config dialog can exist
//Otherwise need to send variable data to callback via each relevant button
E2_KeyBlock blocknow = {0, NULL};

#define KEYCHANGEKEY "_e2-binding-key_"
/**
@brief "mnemonic-activate" signal callback for some config dialog buttons

Block further handling of the button-press, during a keybindings configuration.
In that case, determine and process the mnemonic keyval for @a button here, as
there will be no button-press-cb later

@param button the activated button
@param group_cycling UNUSED TRUE if > 1 widget has the mnemonic being processed now
@param bdata pointer to data to use in the callback

@return FALSE when further (i.e. normal) processing of the mnemonic keypress is ok
*/
static gboolean _e2_option_tree_mnemonic_cb (GtkWidget *button, gboolean group_cycling,
	E2_KeyBlock *bdata)
{
	printd (DEBUG, "_e2_option_tree_mnemonic_cb");
//	E2_KeyBlock *bdata = g_object_get_data (G_OBJECT(button), KEYCHANGEKEY);
	if (bdata != NULL && bdata->data != NULL && g_atomic_int_get (&bdata->blocked) != 0)
	{
		NEEDCLOSEBGL
		GList *child, *all = gtk_widget_list_mnemonic_labels (button);
		for (child = all; child != NULL; child = child->next)
		{
			GtkWidget *wid = child->data;
			if (GTK_IS_LABEL(wid))
			{
				guint code = gtk_label_get_mnemonic_keyval (GTK_LABEL(wid));
				gchar *keyname = gtk_accelerator_name (code, GDK_MOD1_MASK); //CHECKME get actual modifiers ?
				//replicate _e2_option_tree_keychange_cb()
				bdata->data->newtext = keyname;
				_e2_option_tree_keychange_check (bdata->data);
				//CHECKME also unblock bindings if the user cancels, changes focus etc
				e2_keybinding_unblock (config_dialog, NULL);
				//unblock all button-mnemonics
				g_atomic_int_set (&bdata->blocked, 0);
				bdata->data = NULL; //will have been cleared downstream
				break;
			}
		}
		NEEDOPENBGL
		g_list_free (all);
		return TRUE;
	}
	else if (bdata != NULL)
		return (g_atomic_int_get (&bdata->blocked) != 0);
	else
		return FALSE;
}

void e2_option_tree_connect_mnemonics (GtkWidget *button_box)
{
//	printd (DEBUG, "e2_option_tree_connect_mnemonics");
	GList *btn, *all = gtk_container_get_children (GTK_CONTAINER(button_box));
	for (btn = all; btn != NULL; btn = btn->next)
	{
		g_signal_connect (G_OBJECT(btn->data), "mnemonic-activate",
			G_CALLBACK(_e2_option_tree_mnemonic_cb), &blocknow);
	}
	g_list_free (all);
}
/*
static void _e2_option_tree_set_mnemonics_data (GtkWidget **buttons, guint count,
	E2_KeyChangeData *data)
{
	guint i;
	printd (DEBUG, "_e2_option_tree_set_mnemonics_data");

	blocknow.data = data;

	for (i = 0; i < count; i++, buttons++)
	{
		if (*buttons != NULL)
			g_object_set_data_full (G_OBJECT(*buttons), KEYCHANGEKEY, &blocknow, NULL);
	}
}
*/
/**
@brief re-start button mnemonics after a keybinding config

@return
*/
static void _e2_option_tree_unblock_mnemonics (void)
{
//	printd (DEBUG, "_e2_option_tree_unblock_mnemonics");
	g_atomic_int_set (&blocknow.blocked, 0);
	if (blocknow.data != NULL)
	{
		g_free (blocknow.data->path_string);
		g_free (blocknow.data->newtext);
		DEALLOCATE(E2_KeyChangeData, blocknow.data);
		blocknow.data = NULL;
	}
}

/**
@brief setup to amend the key name in a treeview cell

@param renderer the renderer for the cell
@param editable the interface to @a cell
@param path_string string form of gtk tree path to the row to be amended
@param set pointer to set data struct

@return
*/
static void _e2_option_tree_key_edit_start_cb (GtkCellRenderer *renderer,
	GtkCellEditable *editable, gchar *path_string, E2_OptionSet *set)
{
//	printd (DEBUG, "start key edit cb");
	if (GTK_IS_ENTRY (editable))
    {
		E2_KeyChangeData *data = ALLOCATE (E2_KeyChangeData);
		CHECKALLOCATEDWARN (data, return;)
		data->set = set;
		data->path_string = g_strdup (path_string);
		data->renderer = renderer;
		data->newtext = NULL;	//in case it's free'd
		GtkEntry *entry = GTK_ENTRY (editable);

		NEEDCLOSEBGL
		gtk_entry_set_text (entry, _("Press key"));
		NEEDOPENBGL

		//assign a dummy func to prevent the cell-edit from ending upon 1st key-press
		GtkCellEditableIface *api = GTK_CELL_EDITABLE_GET_IFACE (editable);
		api->editing_done = _e2_option_tree_fake_edit_done;

		//prevent operation of any aberrant current binding
		e2_keybinding_block (config_dialog, NULL);
		//or of any dialog-button mnemonic
		g_atomic_int_set (&blocknow.blocked, 1);
		blocknow.data = data;
/* if static data not viable
		//for main dialog buttons
		E2_Sextet *st = ?
		_e2_option_tree_set_mnemonics_data ((GtkWidget**)st, 6, &blocknow-replacement);
		//for current page buttons
		st = g_object_get_data (G_OBJECT(set->widget), "e2-config-buttons");
		_e2_option_tree_set_mnemonics_data ((GtkWidget**)st, 6, &blocknow-replacement);
*/
		g_signal_connect (G_OBJECT (entry), "key-press-event",
			G_CALLBACK (_e2_option_tree_keychange_cb), data);
    }
}
/**
@brief change <Esc> key handling when cell editing starts

@param renderer the renderer for the cell
@param editable the interface to @a renderer
@param path_string string form of gtk tree path to the row to be amended
@param user_data UNUSED pointer to data specified when callback was connected

@return
*/
static void _e2_option_tree_cell_edit_start_cb (GtkCellRenderer *renderer,
	GtkCellEditable *editable, gchar *path_string, gpointer user_data)
{
//	printd (DEBUG, "start cell edit cb");
//	NEEDCLOSEBGL
	g_signal_handlers_block_by_func (G_OBJECT (config_dialog),
		e2_dialog_key_neg_cb, config_dialog);
/*#ifdef DEBUG_MESSAGES
	if (GTK_IS_ENTRY (editable))
		printd (DEBUG, "editable text id %s", gtk_entry_get_text (GTK_ENTRY (editable)));
#endif
*/
//	NEEDOPENBGL
}
/**
@brief revert <Esc> key handling when cell editing is finished

@param renderer the renderer for the cell
@param user_data UNUSED pointer to data specified when callback was connected

@return
*/
static void _e2_option_tree_cell_edit_stop_cb (GtkCellRenderer *renderer,
	gpointer user_data)
{
	printd (DEBUG, "stop cell edit cb");
//	NEEDCLOSEBGL
	g_signal_handlers_unblock_by_func (G_OBJECT (config_dialog),
		e2_dialog_key_neg_cb, config_dialog);
	//CHECKME cancel other key blocking
	//e2_keybinding_unblock (config_dialog, NULL);
	//_e2_option_tree_unblock_mnemonics ();
//	NEEDOPENBGL
}
/**
@brief config page treeview "realize" signal callback
Backup @a set data when the page is opened (if it's not done already)

@param widget the treeview widget
@param set data for the set represented in the treeview

@return
*/
static void _e2_option_tree_page_opened_cb (GtkWidget *widget, E2_OptionSet *set)
{
//	NEEDCLOSEBGL
	e2_option_tree_backup (set);
//	NEEDOPENBGL
}
/**
@brief row draggable-check function for config-page treeview's GtkTreeDragSourceIface

@param drag_source treemodel DnD interface ptr
@param path treepath for the row to be dragged

@return TRUE if dragging the row is allowed
*/
static gboolean _e2_option_tree_draggable_check_cb (GtkTreeDragSource *drag_source,
	GtkTreePath *path)
{
	if (!GTK_IS_TREE_MODEL (drag_source))
		return TRUE;
	GtkTreeModel *model = GTK_TREE_MODEL (drag_source);
	E2_OptionSet *set = g_object_get_data (G_OBJECT (model), "e2-option-set");
	if (set == NULL)
		return TRUE;
	if (set->ex.tree.draggable_check_func != NULL)
	{
		gboolean (*fun) (GtkTreeDragSource *, GtkTreePath *) =
			set->ex.tree.draggable_check_func;
//		NEEDCLOSEBGL
		return fun (drag_source, path);
//		NEEDOPENBGL
	}
	else
		return TRUE;
}
/**
@brief row droppable-check function for config-page treeview's GtkTreeDragDestIface
Prevent creating child-rows in treeviews that are treated as 'flat'.

@param drag_dest treemodel DnD interface ptr
@param dest_path treepath for the position the row would be dropped
@param selection_data

@return TRUE if dropping is allowed
*/
static gboolean _e2_option_tree_droppable_check_cb (GtkTreeDragDest *drag_dest,
	GtkTreePath *dest_path, GtkSelectionData *selection_data)
{
	GtkTreeModel *model = GTK_TREE_MODEL (drag_dest);
	E2_OptionSet *set = g_object_get_data (G_OBJECT (model), "e2-option-set");
	if (set->ex.tree.flags & E2_OPTION_TREE_LIST)
		return (gtk_tree_path_get_depth (dest_path) == 1);
	else
		return TRUE;
}
/**
@brief delete the row at @a path, because it was moved somewhere else via drag-and-drop.

@param drag_source drag source data struct
@param path	gtk tree path to the row to be deleted

@return FALSE if the deletion fails because path no longer exists, or for some model-specific reason
*/
static gboolean _e2_option_tree_drag_delete_cb (GtkTreeDragSource *drag_source,
	GtkTreePath *path)
{
	if (!GTK_IS_TREE_MODEL (drag_source))
		return FALSE;
	GtkTreeModel *model = GTK_TREE_MODEL (drag_source);
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (model, &iter, path))
	{
		NEEDCLOSEBGL
		gtk_tree_store_remove (GTK_TREE_STORE (model), &iter);
		NEEDOPENBGL
		E2_OptionSet *set = g_object_get_data (G_OBJECT (model), "e2-option-set");
		e2_option_tree_flag_change (set);
		return TRUE;
	}
	else
		return FALSE;
}
/* Shared callback for button "clicked" and menu-item "activate" */
static gboolean _e2_option_tree_move_up_cb (GtkWidget *widget, E2_OptionSet *set)
{
	GtkTreeModel *model;
	GtkTreeIter iter;

	NEEDCLOSEBGL
	if (gtk_tree_selection_get_selected (gtk_tree_view_get_selection (GTK_TREE_VIEW (set->widget)), &model, &iter))
	{
		GtkTreePath *path;
		path = gtk_tree_model_get_path (model, &iter);
		if (gtk_tree_path_prev (path))
		{
			_e2_option_tree_move (set, &iter, path, TRUE, TRUE);	//this sets the dirty-flag
		}
		if (path)
	    	gtk_tree_path_free (path);
	}
	NEEDOPENBGL
	return TRUE;
}
/* Shared callback for button "clicked" and menu-item "activate" */
static gboolean _e2_option_tree_move_down_cb (GtkWidget *widget, E2_OptionSet *set)
{
	GtkTreeModel *model;
	GtkTreeIter iter;

	NEEDCLOSEBGL
	if (gtk_tree_selection_get_selected (gtk_tree_view_get_selection (GTK_TREE_VIEW (set->widget)), &model, &iter))
	{
		GtkTreePath *path;
		path = gtk_tree_model_get_path (model, &iter);
		gtk_tree_path_next (path);
		_e2_option_tree_move (set, &iter, path, TRUE, FALSE);	//this sets the dirty-flag
		e2_option_tree_flag_change (set);
		if (path)
	    	gtk_tree_path_free (path);
	}
	NEEDOPENBGL
	return TRUE;
}

/**
@brief "response" signal callback for an icon-selector dialog
@param dialog the widget where the callback was initiated
@param response enumerator of the user's choice
@param set pointer to set data

@return
*/
static void _e2_option_tree_icon_column_set_cb (GtkDialog *dialog,
	gint response, E2_OptionSet *set)
{
	NEEDCLOSEBGL
	if ((response == E2_RESPONSE_APPLY) || (response == E2_RESPONSE_MORE) ||
		(response == E2_RESPONSE_REMOVE))	//CHECKME is this ever available here ?
	{
		GtkTreeIter iter;
		if (gtk_tree_selection_get_selected (gtk_tree_view_get_selection
			(GTK_TREE_VIEW (set->widget)), NULL, &iter))
		{
			gpointer icon = g_object_get_data (G_OBJECT (dialog), "image");
			if (icon != NULL) //user didn't cancel or do something silly
			{
				gpointer column = g_object_get_data (G_OBJECT (dialog), "column");
				_e2_option_tree_set (set, &iter, GPOINTER_TO_INT (column),
					(gchar *)icon);	//this sets the dirty-flag
			}
		}
	}
	if ((response != E2_RESPONSE_MORE) && (response != E2_RESPONSE_REMOVE))
		gtk_widget_destroy (GTK_WIDGET (dialog));
	NEEDOPENBGL
}

#ifdef E2_PTRGESTURES
/**
@brief a 2nd "response" signal callback for a pointer-gestures dialog
@param dialog the widget where the callback was initiated
@param response enumerator of the user's choice
@param set pointer to set data

@return
*/
static void _e2_option_tree_gesture_column_set_cb (GtkDialog *dialog,
	gint response, E2_OptionSet *set)
{
	NEEDCLOSEBGL
	if (response == E2_RESPONSE_APPLY)
	{
		GtkTreeIter iter;
		if (gtk_tree_selection_get_selected (gtk_tree_view_get_selection
			(GTK_TREE_VIEW (set->widget)), NULL, &iter))
		{
			gpointer sequence = g_object_get_data (G_OBJECT (dialog), "sequence");
			if (sequence != NULL) //user didn't cancel or do something silly
			{
				gpointer column = g_object_get_data (G_OBJECT (dialog), "column");
				_e2_option_tree_set (set, &iter, GPOINTER_TO_INT (column),
					(gchar *)sequence);	//this sets the dirty-flag
			}
		}
	}
	if (response != E2_RESPONSE_USER1) //not a clear-button click
		gtk_widget_destroy (GTK_WIDGET (dialog));
	NEEDOPENBGL
}
#endif

static gboolean _e2_option_tree_popup_menu_cb (GtkWidget *treeview,
	GtkWidget *menu)
{
	NEEDCLOSEBGL
	//set appropriate item sensitivities
	e2_option_tree_menu_set_sensitive (menu, treeview);

	guint32 event_time = gtk_get_current_event_time ();
	gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
		(GtkMenuPositionFunc) e2_confdlg_menu_set_position,
		treeview, 0, event_time);	//button code = 0 as this was a menu-button press
	NEEDOPENBGL
	return TRUE;
}
/**
@brief create context menu

Context menu is created when mouse button-3 is pressed

@param treeview widget where the button was pressed
@param event gdk event data
@param menu the context-menu widget

@return TRUE (to prevent further handlers) for a button=3 press, else FALSE
*/
static gboolean _e2_option_tree_button_press_cb2 (GtkWidget *treeview,
	GdkEventButton *event, GtkWidget *menu)
{
	if (event->button != 3)
		return FALSE;
	//set appropriate item sensitivities
	e2_option_tree_menu_set_sensitive (menu, treeview);

	gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, event->button, event->time);
	return TRUE;
}

static gboolean _e2_option_tree_button_press_cb (GtkWidget *treeview,
	GdkEventButton *event, E2_OptionSet *set)
{
	if (event->button != 1)
		return FALSE;
	GtkTreePath *path;
	GtkTreeViewColumn *tree_column;
	NEEDCLOSEBGL
	if (!gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (treeview),//CHECKME BGL ?
		event->x, event->y, &path, &tree_column, NULL, NULL))
	{
		NEEDOPENBGL
		return FALSE;
	}
	GtkTreeSelection *selection = gtk_tree_view_get_selection
		(GTK_TREE_VIEW (treeview));
	if (!gtk_tree_selection_path_is_selected (selection, path)) //CHECKME BGL ?
	{
		NEEDOPENBGL
		return FALSE;
	}
	gboolean retval = FALSE;
	gint column = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (tree_column), "column"));
	E2_OptionTreeColumn *opt = g_list_nth_data (set->ex.tree.columns, column);
	if (opt->type == E2_OPTION_TREE_TYPE_ICON)
	{
		//get the current value
		gchar *icon;
		GtkTreeIter iter;
		gtk_tree_model_get_iter (set->ex.tree.model, &iter, path);
		gtk_tree_model_get (set->ex.tree.model, &iter, column, &icon, -1);
		gchar *dialog_name = g_strdup_printf (_("select icon for %s"), set->name);
		GtkWidget *dialog = e2_sid_create (config_dialog, dialog_name, icon, event);
		g_free (icon);
		g_free (dialog_name);
		if (dialog != NULL)
		{
			//local 2nd "response" cb for handling set data
			g_signal_connect (G_OBJECT (dialog), "response",
				G_CALLBACK (_e2_option_tree_icon_column_set_cb), set);
			g_object_set_data (G_OBJECT (dialog), "column", GINT_TO_POINTER (column));
			retval = TRUE;
		}
	}
#ifdef E2_PTRGESTURES
	else if (opt->type == E2_OPTION_TREE_TYPE_GESTURE)
	{
		//get the current value
		gchar *sequence;
		GtkTreeIter iter;
		gtk_tree_model_get_iter (set->ex.tree.model, &iter, path);
		gtk_tree_model_get (set->ex.tree.model, &iter, column, &sequence, -1);
		GtkWidget *dialog = e2_gesture_dialog_create (config_dialog, sequence);
		g_free (sequence);
		if (dialog != NULL)
		{
			//local 2nd "response" cb for handling set data
			g_signal_connect (G_OBJECT (dialog), "response",
				G_CALLBACK (_e2_option_tree_gesture_column_set_cb), set);
			g_object_set_data (G_OBJECT (dialog), "column", GINT_TO_POINTER (column));
			retval = TRUE;
		}
	}
#endif
	NEEDOPENBGL
	return retval;
}

static gboolean _e2_option_tree_help_cb (GtkButton *button, E2_OptionSet *set)
{
	//get the config dialog page label, from set->group
	// (after a parent separator '.' if any)
	gchar *page, *realpage;
	page = set->group;	//FIXME this is translated, but the help doc is not (yet?)
	if ((realpage = strchr (page, '.')) != NULL)	//if always ascii ',', don't need g_utf8_strchr()
		page = realpage + sizeof (gchar);

	//relevant section label is "[page]"
	gchar *command = g_strconcat(_A(6),".",_A(111)," ",
		e2_option_str_get ("config-help-doc")," [",page,"]",NULL);	//file.view_at;
	NEEDCLOSEBGL
	e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, button
#ifdef E2_COMMANDQ
	, FALSE
#endif
	);
	NEEDOPENBGL
	g_free (command);
	return TRUE;
}
/**
@brief translate all current keybindings

@param button activated widget, UNUSED
@param set ptr to keybindings option data struct

@return
*/
static void _e2_option_tree_translate_cb (GtkWidget *button, E2_OptionSet *set)
{
	GtkTreeIter iter;
//	NEEDCLOSEBGL
	if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
	{
		e2_keybinding_localise (set->ex.tree.model, &iter);
		e2_option_tree_flag_change (set);
	}
//	NEEDOPENBGL
}

/**
@brief recursively replace sub-string(s) in toolbar item argument string(s) and maybe swap 'go' buttons
@param model treemodel for the store being copied
@param iter pointer to 'parent' treeiter in @a model, NULL for root
@param oldarg1 one of the substrings to be replaced
@param oldarg2 one of the substrings to be replaced
@param newarg1 one of the replacement substrings
@param newarg2 one of the replacement substrings
@param actforwd name of action to swap
@param actback name of action to swap

@return
*/
static void _e2_option_tree_update_bar_descendants (
	GtkTreeModel *model, GtkTreeIter *iter,
	const gchar *oldarg1, const gchar *oldarg2,
	const gchar *newarg1, const gchar *newarg2,
	const gchar *actforwd, const gchar *actback)
{
	GtkTreeIter child;
	if (gtk_tree_model_iter_children (model, &child, iter))
	{
		GtkTreePath *gopath = NULL;
		do
		{
			gchar *tmp1, *tmp2 ,*actstr, *argstr;
			gboolean swap = FALSE;

			gtk_tree_model_get (model, &child, 3, &actstr, 4, &argstr, -1);
			if (actstr)
			{
				if (actforwd && actback
					&& (!strcmp (actstr, actforwd) || !strcmp (actstr, actback)))
					swap = TRUE;
				tmp1 = e2_utils_str_replace (actstr, oldarg1, newarg1);
				gtk_tree_store_set (GTK_TREE_STORE(model), &child, 3, tmp1, -1);
				g_free (tmp1);
				g_free (actstr);
			}
			if (argstr)
			{
				tmp1 = e2_utils_str_replace (argstr, oldarg1, newarg1);
				tmp2 = e2_utils_str_replace (tmp1, oldarg2, newarg2);
				g_free (tmp1);
				gtk_tree_store_set (GTK_TREE_STORE(model), &child, 4, tmp2, -1);
				g_free (tmp2);
				g_free (argstr);
			}
			if (gtk_tree_model_iter_has_child (model, &child))
				_e2_option_tree_update_bar_descendants (model, &child,
					oldarg1, oldarg2, newarg1, newarg2, actforwd, actback);
			if (swap)
			{
				if (gopath == NULL)
					//crudely log the first of a potential pair
					gopath = gtk_tree_model_get_path (model, &child);
				else
				{
					GtkTreeIter goiter;
					gtk_tree_model_get_iter (model, &goiter, gopath);
					gtk_tree_store_swap (GTK_TREE_STORE (model), &goiter, &child);
					child = goiter;
					gtk_tree_path_free (gopath);
					gopath = NULL;
				}
			}
		} while (gtk_tree_model_iter_next (model, &child));

		if (gopath != NULL)
			gtk_tree_path_free (gopath);
	}
}
/**
@brief replicate all bar data

@param button UNUSED clicked widget
@param set ptr to keybindings option data struct for the bar to be changed

@return
*/
static void _e2_option_tree_barcopy_cb (GtkButton *button, E2_OptionSet *set)
{
	gint srcp;
	//get the 'source' set
	E2_OptionSet *srcset = e2_option_get_simple ("panebar2");
	if (srcset == set)
	{
		srcp = 1;
		srcset = e2_option_get_simple ("panebar1");
	}
	else
		srcp = 2;

	NEEDCLOSEBGL
	//replicate all data
	gpointer newstore;
	e2_tree_store_copy (srcset->ex.tree.model, TRUE, &newstore);
	//update relevant data
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL(newstore), &iter))
	{
		const gchar *oldarg1, *oldarg2, *newarg1, *newarg2;
		gchar *actforwd, *actback;
		gboolean hori = !e2_option_bool_get ("panes-horizontal");

		if (hori)
		{
			//names of actions to be swapped when filelists are side-by-side
			actforwd = g_strconcat (_A(13), ".", _A(53), NULL);
			actback = g_strconcat (_A(13), ".", _A(52), NULL);
		}
		else
		{
			actforwd = actback = NULL;
		}

		if (srcp == 1)
		{
			oldarg1 = _A(11); //_("pane1")
			newarg1 = _A(12); //_("pane2");
			oldarg2 = "1";
			newarg2 = "2";
		}
		else
		{
			oldarg1 = _A(12);
			newarg1 = _A(11);
			oldarg2 = "2";
			newarg2 = "1";
		}

		_e2_option_tree_update_bar_descendants (GTK_TREE_MODEL(newstore), NULL,
					oldarg1, oldarg2, newarg1, newarg2, actforwd, actback);

		if (hori)
		{
			g_free (actforwd);
			g_free (actback);
		}
	}

	gtk_tree_view_set_model (GTK_TREE_VIEW (set->widget), GTK_TREE_MODEL(newstore));
//	g_object_unref (G_OBJECT (newstore)); problem when dialog is closed
	g_object_unref (G_OBJECT (GTK_TREE_STORE (set->ex.tree.model)));
	set->ex.tree.model = (GTK_TREE_MODEL (newstore));

	gtk_tree_view_expand_all (GTK_TREE_VIEW (set->widget));
	NEEDOPENBGL
	e2_option_tree_flag_change (set);

}

static void e2_option_tree_del_direct_cb (GtkButton *button, E2_OptionSet *set)
{
	GtkTreeIter iter;
	NEEDCLOSEBGL
	if (gtk_tree_selection_get_selected (gtk_tree_view_get_selection
			(GTK_TREE_VIEW (set->widget)), NULL, &iter))
	{
		gint children;
		DialogButtons choice;
		gboolean do_plugin = (e2_option_get_simple ("plugins") == set);
		gchar *question;

		if (do_plugin)
		{
			children = e2_plugins_count_actions (set->ex.tree.model, &iter);
			if (children < 2)
			{
				do_plugin = FALSE;	//ensure simple deletion
				choice = OK;
			}
			else
			{
				--children;
				question = g_strdup_printf (
					_("Are you sure that you want to delete this row and <b>%d</b> other related %s ?"),
					children, children == 1 ? _("action") : _("actions"));
				goto setdialog;
			}
		}
		else
		{
			children = gtk_tree_model_iter_n_children (set->ex.tree.model, &iter);
			if (children > 0)
			{
				GtkWidget *dialog;
				question = g_strdup_printf (
					_("Are you sure that you want to delete this row and <b>%d</b> %s?"),
					children, children == 1 ? _("child") : _("children"));
setdialog:
				dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION, question,
					_("confirm row delete"), DUMMY_RESPONSE_CB, NULL);
				g_free (question);

				e2_dialog_set_negative_response (dialog, GTK_RESPONSE_NO);

				E2_Button no_btn;
				e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_KEEP);

				E2_Button yes_btn;
				e2_button_derive (&yes_btn, &E2_BUTTON_YES, BTN_YES_DELETE);
				yes_btn.name = STOCK_NAME_DELETE;

				choice = e2_dialog_show (dialog, config_dialog,	//de-sensitize main dialog
					E2_DIALOG_MODAL | E2_DIALOG_FREE, &no_btn, &yes_btn, NULL);
			}
			else
				choice = OK;
		}

		if (choice == OK)
		{
			GtkTreePath *path = gtk_tree_model_get_path (set->ex.tree.model, &iter);
			if (do_plugin)
			{
				//delete matching rows except the currently selected one
				gchar *needle, *sep;
				GtkTreeIter scan;
				GtkTreeModel *model = set->ex.tree.model;
				GtkTreeRowReference *ref = gtk_tree_row_reference_new (model, path);
				gtk_tree_path_free (path);

				gtk_tree_model_get (model, &iter, SIG_COL, &needle, -1);
				sep = strchr (needle, '@') + 1;
				gtk_tree_model_get_iter_first (model, &scan);
				do
				{
					gchar *onesig;
next:
					gtk_tree_model_get (model, &scan, SIG_COL, &onesig, -1);
					if (onesig != NULL)
					{
						gchar *start = strchr (onesig, '@');
						if (start != NULL && strcmp (start+1, sep) == 0)
						{
							//skip the original selection so we know where to re-select
							if (strcmp (needle, onesig) != 0 &&
								gtk_tree_store_remove (GTK_TREE_STORE (model), &scan))
							{
								g_free (onesig);
								goto next;
							}
						}
						g_free (onesig);
					}
				} while (gtk_tree_model_iter_next (model, &scan));

				g_free (needle);
				path = gtk_tree_row_reference_get_path (ref); //freed below
				gtk_tree_model_get_iter (model, &iter, path);
				gtk_tree_row_reference_free (ref);
				//fall into deleting the selected row
			}

			e2_option_tree_del_direct (set, &iter);

			if (iter.user_data == NULL)
			{
				if (!gtk_tree_path_prev (path) && !gtk_tree_path_up (path))
				{
					gtk_tree_path_free (path);
					path = NULL;
				}
			}
			else
			{
				gtk_tree_path_free (path);
				path = gtk_tree_model_get_path
					(GTK_TREE_MODEL (set->ex.tree.model), &iter);
			}

			if ((path != NULL) &&
				(gtk_tree_model_get_iter_first (set->ex.tree.model, &iter)))
			{
				gtk_tree_view_set_cursor (GTK_TREE_VIEW (set->widget), path,
					gtk_tree_view_get_column (GTK_TREE_VIEW (set->widget), 0),
					FALSE);
				gtk_tree_path_free (path);
			}
		}
	}
	NEEDOPENBGL
}
/* Shared callback for button "clicked" and menu-item "activate" */
static gboolean _e2_option_tree_add_child_cb (GtkWidget *wid, E2_OptionSet *set)
{
	GtkTreeIter iter;
	NEEDCLOSEBGL
	if (gtk_tree_selection_get_selected (gtk_tree_view_get_selection (GTK_TREE_VIEW (set->widget)), NULL, &iter))
	{
		if (iter.user_data != NULL)
		{
			GtkTreeIter iter2;
			_e2_option_tree_add_default (set, &iter2, &iter, FALSE);
		}
	}
	else
	{
		_e2_option_tree_add_default (set, &iter, NULL, FALSE);
	}
	NEEDOPENBGL
	return TRUE;
}
/* Shared callback for button "clicked" and menu-item "activate" */
static gboolean _e2_option_tree_add_below_cb (GtkWidget *wid, E2_OptionSet *set)
{
	GtkTreeIter iter;
	NEEDCLOSEBGL
	if (gtk_tree_selection_get_selected (
		gtk_tree_view_get_selection (GTK_TREE_VIEW (set->widget)), NULL, &iter))
	{
		if (iter.user_data != NULL)
		{
			GtkTreeIter iter2;
			_e2_option_tree_add_default (set, &iter2, &iter, TRUE);
		}
	}
	else
	{
		_e2_option_tree_add_default (set, &iter, NULL, TRUE);
	}
	NEEDOPENBGL
	return TRUE;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief (de)sensitize config dialog buttons that are not relevant to 'category' lines
@param view UNUSED the treeview to which the buttons relate
@param value TRUE to senstitize the buttons, FALSE to desensitize

@return
*/
void e2_option_tree_adjust_buttons (GtkTreeView *view, gboolean value)
{
	E2_Sextet *st = g_object_get_data (G_OBJECT (view), "e2-config-buttons");
	if (st != NULL)
	{
		if (st->b != NULL)
			gtk_widget_set_sensitive (st->b, value);
		if (st->c != NULL)
			gtk_widget_set_sensitive (st->c, value);
		if (st->d != NULL)
			gtk_widget_set_sensitive (st->d, value);
		if (st->f != NULL)
			gtk_widget_set_sensitive (st->f, value);
	}
}
/**
@brief create, and add to @a box, a config-data treeview for tree-option @a set
This also backs up the current option data
@param parent UNUSED the parent config-dialog widget
@param single TRUE for single-page dialog, FALSE for general config dialog
@param box the widget into which the treeview and related things will be packed
@param set data struct for the config option being processed

@return
*/
void e2_option_tree_add_widget (GtkWidget *parent, gboolean single, GtkWidget *box,
	E2_OptionSet *set)
{
	//make the set accessible downstream
	g_object_set_data (G_OBJECT (set->ex.tree.model), "e2-option-set", set);
	
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
#endif
	gtk_box_pack_start (GTK_BOX (box), vbox, TRUE, TRUE, 0);
	GtkWidget *sw = e2_widget_add_sw (vbox, GTK_POLICY_AUTOMATIC,
		GTK_POLICY_AUTOMATIC, TRUE, E2_PADDING);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (sw), GTK_SHADOW_IN);

	//setup treeview
	set->widget = gtk_tree_view_new_with_model (set->ex.tree.model);
	//don't unref set->ex.tree.model here !
	gtk_container_add (GTK_CONTAINER (sw), set->widget);
	GtkTreeView *treeview = GTK_TREE_VIEW (set->widget);
	gtk_tree_view_set_reorderable (treeview, TRUE);
//	gtk_tree_view_set_headers_clickable (treeview, FALSE);  =default
//	gtk_tree_view_set_rules_hint (treeview, FALSE);  =default
#ifdef USE_GTK2_10
	gtk_tree_view_set_enable_tree_lines (treeview, TRUE);
#endif

#ifdef E2_ICONCACHE
	//setup for appropriate icon size
	gint phigh;
	e2_widget_get_font_pixels (set->widget, NULL, &phigh);
	GtkIconSize isize = e2_icons_get_size (phigh + 2);
#endif

	GtkTreeSelection *selection = gtk_tree_view_get_selection (treeview);
	gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);
//	gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);
	if (set->ex.tree.selection_check_func != NULL)
		gtk_tree_selection_set_select_function (selection,
			(GtkTreeSelectionFunc) set->ex.tree.selection_check_func,
			set, NULL);

	GtkTreeDragSourceIface *siface = GTK_TREE_DRAG_SOURCE_GET_IFACE (set->ex.tree.model);
	siface->row_draggable = _e2_option_tree_draggable_check_cb;
	//enable setting the set 'dirty-flag' when any row is dragged
	siface->drag_data_delete = _e2_option_tree_drag_delete_cb;
	//checking for 'flat' treestores here is useless, cuz if applied to any of them
	//the callback happens for all
	//instead, check applicability downstream
	GtkTreeDragDestIface *diface = GTK_TREE_DRAG_DEST_GET_IFACE (set->ex.tree.model);
	diface->row_drop_possible = _e2_option_tree_droppable_check_cb;

	//add columns
	int j = 0;
	GList *member;
	for (member = set->ex.tree.columns; member != NULL; member = member->next)
	{
		GtkCellRenderer *renderer;
		GtkTreeViewColumn *column;
		E2_OptionTreeColumn *opt = member->data;
		gboolean editable = !(opt->flags & E2_OPTION_TREE_COL_NOT_EDITABLE);
		switch (opt->type)
		{
//			case E2_OPTION_TREE_TYPE_INT:
//				break;
			case E2_OPTION_TREE_TYPE_BOOL:
				renderer = gtk_cell_renderer_toggle_new ();
				g_signal_connect (G_OBJECT (renderer), "toggled",
					G_CALLBACK (_e2_option_tree_toggle_cb), set);
				column = gtk_tree_view_column_new_with_attributes
					(opt->label, renderer, "active", j, NULL);
//				gtk_tree_view_column_set_resizable (GTK_TREE_VIEW_COLUMN (column), TRUE);
				gtk_tree_view_column_set_cell_data_func (column, renderer,
					(GtkTreeCellDataFunc) _e2_option_tree_bool_data_func, opt, NULL);
				gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
				break;
			case E2_OPTION_TREE_TYPE_STR:
			case E2_OPTION_TREE_TYPE_KEY:
#ifdef E2_MOUSECUSTOM
			case E2_OPTION_TREE_TYPE_BUTTON:
# ifdef E2_PTRGESTURES
	        case E2_OPTION_TREE_TYPE_GESTURE:
# endif
#endif
				renderer = gtk_cell_renderer_text_new ();
				g_object_set (G_OBJECT (renderer), "editable", editable, NULL);

//				g_signal_connect (G_OBJECT (?), "key-press-event",
//					G_CALLBACK (_e2_option_tree_cell_keypress_cb), renderer); ?translation
				g_signal_connect (G_OBJECT (renderer), "editing-started",
					G_CALLBACK (_e2_option_tree_cell_edit_start_cb), NULL);
				if (opt->type == E2_OPTION_TREE_TYPE_KEY)
					g_signal_connect (G_OBJECT (renderer), "editing-started",
						G_CALLBACK (_e2_option_tree_key_edit_start_cb), set);
#ifdef E2_MOUSECUSTOM
				else if (opt->type == E2_OPTION_TREE_TYPE_BUTTON)
					g_signal_connect (G_OBJECT (renderer), "editing-started",
						G_CALLBACK (_e2_option_tree_btn_edit_start_cb), set);
#endif
				g_signal_connect (G_OBJECT (renderer), "editing-canceled",
					G_CALLBACK (_e2_option_tree_cell_edit_stop_cb), NULL);
#ifdef E2_MOUSECUSTOM
				//button-changing needs special care to handle focus-change
				if (opt->type != E2_OPTION_TREE_TYPE_BUTTON)
#endif
				g_signal_connect (G_OBJECT (renderer), "edited",
					G_CALLBACK (_e2_option_tree_string_edited_cb), set);

				column = gtk_tree_view_column_new_with_attributes
					(opt->label, renderer, "text", j, NULL);
				gtk_tree_view_column_set_sizing (GTK_TREE_VIEW_COLUMN (column),
					GTK_TREE_VIEW_COLUMN_GROW_ONLY);
				gtk_tree_view_column_set_resizable (GTK_TREE_VIEW_COLUMN (column), TRUE);
				gtk_tree_view_column_set_cell_data_func (column, renderer,
					(GtkTreeCellDataFunc) _e2_option_tree_str_data_func, opt, NULL);
				gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
				break;
			case E2_OPTION_TREE_TYPE_ICON:
				renderer = gtk_cell_renderer_pixbuf_new ();
				g_object_set (G_OBJECT (renderer), "stock-size", GTK_ICON_SIZE_MENU, NULL);
				column = gtk_tree_view_column_new_with_attributes
					(opt->label, renderer, NULL);
				gtk_tree_view_column_set_cell_data_func (column, renderer,
#ifdef E2_ICONCACHE
					_e2_option_tree_cell_pixbuf_data_func, GINT_TO_POINTER (isize), NULL);
#else
					_e2_option_tree_cell_pixbuf_data_func, NULL, NULL);
#endif
//				gtk_tree_view_column_set_resizable (GTK_TREE_VIEW_COLUMN (column), TRUE);
//				gtk_tree_view_column_set_sizing (GTK_TREE_VIEW_COLUMN (column),
//					GTK_TREE_VIEW_COLUMN_GROW_ONLY);
				gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
				break;
			case E2_OPTION_TREE_TYPE_SEL:
				renderer = gtk_cell_renderer_combo_new ();
				GtkTreeModel *mdl = (opt->visible_check_data == NULL) ?
					GTK_TREE_MODEL (actions_store):
					e2_action_filter_store (opt->visible_check_data);
				g_object_set (G_OBJECT (renderer), "model", mdl,
					"text-column", 0, "editable", editable, NULL);

//				g_signal_connect (G_OBJECT (?), "key-press-event",
//					G_CALLBACK (_e2_option_tree_cell_keypress_cb), renderer);
				g_signal_connect (G_OBJECT (renderer), "editing-started",
					G_CALLBACK (_e2_option_tree_cell_edit_start_cb), NULL);
				g_signal_connect (G_OBJECT (renderer), "editing-canceled",
					G_CALLBACK (_e2_option_tree_cell_edit_stop_cb), NULL);

				g_signal_connect (G_OBJECT (renderer), "edited",
					G_CALLBACK (_e2_option_tree_string_edited_cb), set);

//CHECKME is it possible to open the combo at the current spot?
				column = gtk_tree_view_column_new_with_attributes
					(opt->label, renderer, NULL);
				gtk_tree_view_column_set_cell_data_func (column, renderer,
					(GtkTreeCellDataFunc)  _e2_option_tree_str_data_func, opt, NULL);
/* does not work
				gint colwidth;
				gtk_tree_view_column_cell_get_size (column, NULL, NULL, NULL,
					&colwidth, NULL);
				g_object_set (G_OBJECT (column), "min-width", colwidth+20, NULL);
*/
				gtk_tree_view_column_set_resizable (GTK_TREE_VIEW_COLUMN (column), TRUE);
				gtk_tree_view_column_set_sizing (GTK_TREE_VIEW_COLUMN (column),
					GTK_TREE_VIEW_COLUMN_AUTOSIZE);	//GROW_ONLY);
				gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
				break;
			default:
				printd (WARN, "don't know how to render column %d of %s", j, set->name);
//			case E2_OPTION_TREE_TYPE_HIDDENINT:
			case E2_OPTION_TREE_TYPE_HIDDENBOOL:
			case E2_OPTION_TREE_TYPE_HIDDENSTR:
				renderer = NULL;
				column = NULL;  //warning prevention
				break;
		}
		if (renderer != NULL)
		{
			g_object_set_data (G_OBJECT (renderer), "column", GINT_TO_POINTER (j));
			g_object_set_data (G_OBJECT (column), "column", GINT_TO_POINTER (j));
		}
		j++;
	}

	//spacer column
	if (set->ex.tree.flags & E2_OPTION_TREE_LAST_COL_EMPTY)
	{
		GtkCellRenderer *renderer = gtk_cell_renderer_text_new ();
		GtkTreeViewColumn *column = gtk_tree_view_column_new_with_attributes
			("", renderer, NULL);
		gtk_tree_view_append_column (treeview, column);
	}

	//now expand everything
	gtk_tree_view_expand_all (treeview);
//	gtk_tree_view_set_hover_expand (treeview, TRUE); //annoying ?

	//add control buttons
	E2_Sextet *st = (E2_Sextet *) e2_utils_sextet_new ();
	if (st != NULL)
	{
		g_object_set_data_full (G_OBJECT (treeview), "e2-config-buttons", st,
			(GDestroyNotify) e2_utils_sextet_destroy);

#ifdef USE_GTK3_0
		GtkWidget *bbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING);
#else
		GtkWidget *bbox = gtk_hbox_new (FALSE, E2_PADDING);
#endif
		if (set->ex.tree.flags & E2_OPTION_TREE_ADD_DEL)
		{
			st->f = e2_button_add_end (bbox, FALSE, 0, _("_Remove"), STOCK_NAME_REMOVE,
				_("Remove the selected row"),  e2_option_tree_del_direct_cb, set);
			if (e2_option_get_simple ("plugins") == set)
			{
				st->e = e2_button_add_end (bbox, FALSE, 0, _("_Select"), STOCK_NAME_INDEX,
				_("Select one or more plugins to be added"), e2_confdlg_choose_plugins_cb, set);
			}
			else
			{
				if (! (set->ex.tree.flags & E2_OPTION_TREE_LIST))
					st->e = e2_button_add_end (bbox, FALSE, 0, _("_Child"), STOCK_NAME_INDENT,
						_("Add a child row to the currently selected one"),
						(void(*)())_e2_option_tree_add_child_cb, set);
				st->d = e2_button_add_end (bbox, FALSE, 0, _("_Add"), STOCK_NAME_ADD,
					_("Add a row after the currently selected row in the tree"),
					(void(*)())_e2_option_tree_add_below_cb, set);
			}
		}
		if (set->ex.tree.flags & E2_OPTION_TREE_UP_DOWN)
		{
			st->c = e2_button_add_end (bbox, FALSE, 0, _("_Down"), STOCK_NAME_GO_DOWN,
				_("Move the selected row one place down"),
				(void(*)())_e2_option_tree_move_down_cb, set);
			st->b = e2_button_add_end (bbox, FALSE, 0, _("_Up"), STOCK_NAME_GO_UP,
				_("Move the selected row one place up"),
				(void(*)())_e2_option_tree_move_up_cb, set);
		}
		//insert an extra button on some pages
		if (e2_option_get_simple ("keybindings") == set)
			e2_button_add_end (bbox, FALSE, 0, _("Tra_nslate"), STOCK_NAME_CONVERT,
			_("Set all single-letter bindings to current locale"),
			_e2_option_tree_translate_cb, set);
		else if (e2_option_get_simple ("panebar1") == set ||
				 e2_option_get_simple ("panebar2") == set)
			e2_button_add_end (bbox, FALSE, 0, _("Re_plicate"), STOCK_NAME_COPY,
			_("Import data of the other toolbar"),
			_e2_option_tree_barcopy_cb, set);
#ifdef E2_RAINBOW
		else if (e2_option_get_simple ("filetypes") == set)
			e2_button_add_end (bbox, FALSE, 0, _("Co_lor"), STOCK_NAME_SELECT_COLOR,
			_("Show color-selection dialog"), e2_confdlg_extcolorpick_cb, set);
#endif
		st->a = e2_button_add_end (bbox, FALSE, 0, _("_Help"), STOCK_NAME_HELP,
			_("Get help on this option"), (void(*)())_e2_option_tree_help_cb, set);
		//setup to process button-mnemonic keyvals for keybindings
		if (e2_option_get_simple ("keybindings") == set)
			e2_option_tree_connect_mnemonics (bbox);

		if (single)
			//single-page dialog doesn't need extra scrolling
			gtk_box_pack_start (GTK_BOX (vbox), bbox, FALSE, FALSE, 0);
		else
		{
			//can't find a way to reduce sw height when no bar needed !
			//so AUTOMATIC horz. policy resizes buttons, or leaves gap ...
#if 0
			sw = e2_widget_get_sw_plain (GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
			e2_widget_sw_add_with_viewport (sw, bbox);
#else
# ifdef USE_GTK3_0
	 		GtkWidget *vbox2 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
# else
			GtkWidget *vbox2 = gtk_vbox_new (FALSE, 0);
# endif
			gtk_box_pack_start (GTK_BOX (vbox2), bbox, FALSE, FALSE, 0);
//			sw = e2_widget_get_sw_plain (GTK_POLICY_ALWAYS, GTK_POLICY_NEVER);
			sw = e2_widget_get_sw_plain (GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
			e2_widget_sw_add_with_viewport (sw, vbox2);
#endif
			gtk_box_pack_start (GTK_BOX (vbox), sw, FALSE, FALSE, 0);
		}
	}
	//add persistent context-menu
	E2_TreeContextMenuFlags context_flags = E2_TREE_CONTEXT_DEFAULT;
	if (!((gboolean) set->ex.tree.flags & E2_OPTION_TREE_LIST))
		context_flags |= E2_TREE_CONTEXT_EXP_COL;
	GtkWidget *context_menu = e2_menu_get ();
	_e2_option_tree_populate_menu (context_menu, treeview, context_flags, set);

	//connect callbacks
	//no longer needed it seems
//	g_signal_connect (G_OBJECT (treeview), "drag-drop", G_CALLBACK (drop_cb), set);
//	g_signal_connect (G_OBJECT (treeview), "drag-end", G_CALLBACK (drag_end_cb), set);
	//cleanup menu with the treeview
	g_object_set_data_full (G_OBJECT(treeview), "destroy-menu", context_menu,
		(GDestroyNotify)gtk_widget_destroy);
	//arrange for data backup if/when the page is opened
	g_signal_connect (G_OBJECT (treeview), "realize",
		G_CALLBACK (_e2_option_tree_page_opened_cb), set);
	g_signal_connect (G_OBJECT (treeview), "popup-menu",
		G_CALLBACK (_e2_option_tree_popup_menu_cb), context_menu);
/* the dialog's "negative response" handler takes care of this
	g_signal_connect_after (G_OBJECT (treeview), "key-press-event",
		G_CALLBACK (e2_confdlg_key_press_cb), NULL); */
	//this one handles left-button clicks
	g_signal_connect (G_OBJECT (treeview), "button-press-event",
		G_CALLBACK (_e2_option_tree_button_press_cb), set);
	//this one handles right-button clicks
	g_signal_connect (G_OBJECT (treeview), "button-press-event",
		G_CALLBACK (_e2_option_tree_button_press_cb2), context_menu);

	//goto first row & column
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL (set->ex.tree.model), &iter))
	{
		GtkTreePath *path = gtk_tree_model_get_path (GTK_TREE_MODEL
			(set->ex.tree.model), &iter);
		gtk_tree_view_set_cursor (treeview, path,
			gtk_tree_view_get_column (treeview, 0), FALSE);
		gtk_tree_path_free (path);
	}
}
/**
@brief begin registration of a tree option
Complete registration involves this func, then e2_option_tree_add_column() for
each column in the treestore, tnen e2_option_tree_create_store() to finish things
@param name name of the option, generally a constant string but sometimes runtime-created
@param group group the option belongs to, used in config dialog, can be a r-t string FREEME or a _()
@param desc textual description of the option used in config dialog, a r-t _() string
@param depends name of another option this one depends on
@param selection_check_func function to check whether row is selectable
@param draggable_check_func function to check whether selected row is draggable
@param flags flags set when registering a tree option
@param flags2 bitflags determining how the option data is to be handled

@return E2_OptionSet data struct for the option
*/
E2_OptionSet *e2_option_tree_register (gchar *name, gchar *group, gchar *desc,
	gchar *depends, gpointer selection_check_func, gpointer draggable_check_func,
	E2_OptionTreeTypeFlags flags, E2_OptionFlags flags2)
{
	E2_OptionSet *set = e2_option_register (E2_OPTION_TYPE_TREE, name, group,
		desc, NULL, depends, flags2);
	set->ival = -1;
	set->sval = NULL;
	set->ex.tree.synced = FALSE;
	set->ex.tree.def.func = NULL;
//	set->ex.tree.columns_num = 0;	//set when all cols processed
	set->ex.tree.columns = NULL;
	set->ex.tree.flags = flags;
//	set->ex.tree.unknown = NULL;
	set->ex.tree.selection_check_func = selection_check_func;
	set->ex.tree.draggable_check_func = draggable_check_func;
	return set;
}
/**
@brief setup data for a column of a tree-set
This establishes some parameters for regular use, and others for config-dialog
use. String-parameters (@a name, @a sdef) are NOT COPIED
@param set pointer to data struct for the tree-set
@param name column label string
@param type indicator of the type of data in the column
@param idef default value expessed as an integer (0 for string-options)
@param sdef default value expressed as a string
@param flags flags relating to data editability, cleanup
@param visible_check_func function to check whether a cell in the column is displayed in a config dialog
@param visible_check_data data to provide to @a visible_check_func

@return
*/
void e2_option_tree_add_column (E2_OptionSet *set, gchar *name,
	E2_OptionTreeType type, gint idef, gchar *sdef,
	E2_OptionTreeColFlags flags,
	gpointer visible_check_func, gpointer visible_check_data)
{
	E2_OptionTreeColumn *col = ALLOCATE (E2_OptionTreeColumn);
	CHECKALLOCATEDFATAL (col);
	set->ex.tree.columns = g_list_append (set->ex.tree.columns, col);
	col->label = name;
	col->type = type;
	col->idef = idef;
	col->sdef = sdef;
	col->flags = flags;
	col->visible_check_func = visible_check_func;
	col->visible_check_data = visible_check_data;
//	col->data_cleaner = ?;
}
/**
@brief create a treestore for the data of a tree-set

Treestore is used for all sets, even when there are no descendants.
This works from listed columns' data at set->ex.tree.columns.
@param set pointer to data struct for the tree-set

@return
*/
void e2_option_tree_create_store (E2_OptionSet *set)
{
	set->ex.tree.columns_num = g_list_length (set->ex.tree.columns);
	GType t[set->ex.tree.columns_num + 1];	//space for registered columns + 1
	//the extra trailing column is for visibility flag
	t[set->ex.tree.columns_num] = G_TYPE_BOOLEAN;

	GList *member;
	gint i;
	for (member = set->ex.tree.columns, i = 0; member != NULL; member = member->next, i++)
	{
		E2_OptionTreeColumn *tcol = member->data;
		switch (tcol->type)
		{
//			case E2_OPTION_TREE_TYPE_INT:
//			case E2_OPTION_TREE_TYPE_HIDDENINT:
			case E2_OPTION_TREE_TYPE_BOOL:
			case E2_OPTION_TREE_TYPE_HIDDENBOOL:
				t[i] = G_TYPE_INT;
				break;
			case E2_OPTION_TREE_TYPE_STR:
			case E2_OPTION_TREE_TYPE_HIDDENSTR:
			case E2_OPTION_TREE_TYPE_ICON:
			case E2_OPTION_TREE_TYPE_SEL:
			case E2_OPTION_TREE_TYPE_KEY:
#ifdef E2_MOUSECUSTOM
			case E2_OPTION_TREE_TYPE_BUTTON:
# ifdef E2_PTRGESTURES
	        case E2_OPTION_TREE_TYPE_GESTURE:
# endif
#endif
				t[i] = G_TYPE_STRING;
				break;
			default:
				printd (ERROR,
					"internal error, tree options may only have bool or string values");
				break;
		}
	}

	set->ex.tree.model = gtk_tree_store_new (1, t[0]);
	gtk_tree_store_set_column_types (set->ex.tree.model, set->ex.tree.columns_num + 1, t);
}
/**
@brief create a string representing a tree row

This is a helper fn for backup and config file writing
Depth is represented by leading tabs
Columns are separated by '|'
Any other embedded '|', or an initial '>', is escaped with '\'
c.f. e2_tree_row_to_string() which does not store bools as "true" or "false"

@param set set data structure
@param iter treeiter used for scanning the treemodel
@param level tree path-depth of the processed line

@return  the constructed string
*/
gchar *e2_option_tree_row_write_to_string (E2_OptionSet *set,
	GtkTreeIter *iter, gint level)
{
	GString *treerow = g_string_sized_new (128);
	gint j;
	for (j = 0; j < level; j++)
		treerow = g_string_append_c (treerow, '\t');
	GList *column;
	j = 0;
	for (column = set->ex.tree.columns; column != NULL; column = g_list_next (column))
	{
		E2_OptionTreeColumn *opt = column->data;
		switch (opt->type)
		{
//			case E2_OPTION_TREE_TYPE_INT:
//			case E2_OPTION_TREE_TYPE_HIDDENINT:
//				break;
			case E2_OPTION_TREE_TYPE_BOOL:
			case E2_OPTION_TREE_TYPE_HIDDENBOOL:
			{
				gboolean int_data;
				gtk_tree_model_get (set->ex.tree.model, iter, j, &int_data, -1);
				if (int_data)
					treerow = g_string_append (treerow, "true");
				else
					treerow = g_string_append (treerow, "false");
			}
			break;
			case E2_OPTION_TREE_TYPE_STR:
			case E2_OPTION_TREE_TYPE_HIDDENSTR:
			case E2_OPTION_TREE_TYPE_ICON:
			case E2_OPTION_TREE_TYPE_SEL:
			case E2_OPTION_TREE_TYPE_KEY:
#ifdef E2_MOUSECUSTOM
			case E2_OPTION_TREE_TYPE_BUTTON:
# ifdef E2_PTRGESTURES
	        case E2_OPTION_TREE_TYPE_GESTURE:
# endif
#endif
			{
				gchar *str_data;
				gtk_tree_model_get (set->ex.tree.model, iter,  j, &str_data, -1);

				if (str_data != NULL)
				{
					if (j == 0 && str_data[0] == '>')
						treerow = g_string_append (treerow, "\\");
					if ((str_data[0] != '\0') && ((strchr (str_data, '|')) != NULL))	//if always ascii |, don't need g_utf8_strchr()
					{
						gchar **split = g_strsplit (str_data, "|", -1);
						g_free (str_data);
						str_data = g_strjoinv ("\\|", split);
						g_strfreev (split);
					}
					treerow = g_string_append (treerow, str_data);
					g_free (str_data);
				}
			}
			break;
			default:
			break;
		}
		treerow = g_string_append_c (treerow, '|');
		j++;
	}
	//get rid of superfluous trailing '|'
	treerow = g_string_truncate (treerow, treerow->len -1);

	gchar *result = g_string_free (treerow, FALSE);
	return result;
}
/**
@brief write a string-representation of tree set @a set to @a file

This is a helper fn for wrting config file data
There is no error checking
It is recursive, to deal with descendants

@param f pointer to file descriptor
@param set the set data structure
@param iter treeiter used for scanning the treemodel
@param level the current path depth in the tree

@return
*/
void e2_option_tree_write_to_file (E2_FILE *f, E2_OptionSet *set,
	GtkTreeIter *iter, gint level)
{
	do
	{
		gchar *rowstring = e2_option_tree_row_write_to_string (set, iter, level);
		gchar *writestring = g_strconcat (rowstring, "\n", NULL);
//		if (
		e2_fs_put_stream (f, writestring, "config" E2_ERR_NONE());
//		== EOF)
//		{ FIXME handle error }
		g_free (rowstring);
		g_free (writestring);

		GtkTreeIter iter2;
		if (gtk_tree_model_iter_children (set->ex.tree.model, &iter2, iter))
			e2_option_tree_write_to_file (f, set, &iter2, level + 1);

	} while (gtk_tree_model_iter_next (set->ex.tree.model, iter));
}
/**
@brief create a string array of option-tree row data for a specified optionset

This is a helper fn for backup
It is recursive, to deal with descendants

@param lines ptr-array where the strings are recorded
@param set the set data structure
@param iter treeiter used for scanning the treemodel
@param level the current path depth in the tree

@return
*/
static void e2_option_tree_write_to_storage (GPtrArray *lines, E2_OptionSet *set,
	GtkTreeIter *iter, gint level)
{
	do
	{
		gchar *rowstring = e2_option_tree_row_write_to_string (set, iter, level);
		g_ptr_array_add (lines, (gpointer) rowstring);

		GtkTreeIter iter2;
		if (gtk_tree_model_iter_children (set->ex.tree.model, &iter2, iter))
			e2_option_tree_write_to_storage (lines, set, &iter2, level + 1);

	} while (gtk_tree_model_iter_next (set->ex.tree.model, iter));
}
/**
@brief create backup data for a specified treeoption set

Data are stored as strings, one for each tree row, in the same format
as used for the config file
Each row is referred to in a NULL-terminated pointer array
This fn does nothing if there is already backup data for the set

@param set the set data structure

@return
*/
void e2_option_tree_backup (E2_OptionSet *set)
{
	if (set->ex.tree.def.tree_strings == NULL)
	{	//backup data does not exist now, so ok to create it
		GPtrArray *lines = g_ptr_array_sized_new (21);
		g_ptr_array_add (lines, (gpointer) g_strdup_printf ("%s=<", set->name));
		GtkTreeIter iter;
		if (gtk_tree_model_get_iter_first (set->ex.tree.model, &iter))
			e2_option_tree_write_to_storage (lines, set, &iter, 0);
		g_ptr_array_add (lines, (gpointer) g_strdup (">"));
//		set->ex.tree.def_num = (gint) lines->len;
		g_ptr_array_add (lines, NULL);
		set->ex.tree.def.tree_strings = (gchar **)lines->pdata;
		g_ptr_array_free (lines, FALSE);
	}
}
/**
@brief revert tree option backup data

@param set pointer to set data structure
@param empty_check TRUE to check for backup data only if loaded config data is missing

@return
*/
static void _e2_option_tree_revert (E2_OptionSet *set, gboolean empty_check)
{
	if ((!set->ex.tree.synced || !empty_check)
		&& set->ex.tree.def.tree_strings != NULL) //CHECKME this test
	{
		g_object_unref (set->ex.tree.model);
		e2_option_tree_create_store (set);
		//option needs default configuration which is stored in array at
		//set->ex.tree.def.tree_strings
		e2_option_tree_set_from_array (set->name, set->ex.tree.def.tree_strings,
			NULL, NULL);
		//make sure the set is recognised as clean
		set->ex.tree.flags &= ~E2_OPTION_TREE_SET_EDITED;
	}
}
/**
@brief revert tree option backup data

@param option_name the name of the set

@return
*/
void e2_option_tree_revert (gchar *option_name)
{
	E2_OptionSet *set = e2_option_tree_get (option_name);
	_e2_option_tree_revert (set, FALSE);
}
/**
@brief optionally revert, then clean, tree option backup data

@param set the set data structure
@param revert TRUE to restore the backup data to the tree, FALSE to cleanup only

@return
*/
void e2_option_tree_unbackup (E2_OptionSet *set, gboolean revert)
{
	if (set->ex.tree.def.tree_strings != NULL)
	{	//backup data exists
		if (revert)
			_e2_option_tree_revert (set, FALSE);
		//cleanup
		g_strfreev (set->ex.tree.def.tree_strings);
		set->ex.tree.def.tree_strings = NULL;
	}
}
/**
@brief iterate over all option trees, reverting any with backup data

@return
*/
void e2_option_tree_restore_all (void)
{
	guint i;
	gpointer *walker;
	E2_OptionSet *set;
	for (i = 0, walker = options_array->pdata; i < options_array->len; i++, walker++)
	{
		set = *walker;
		if (set->type == E2_OPTION_TYPE_TREE)
			e2_option_tree_unbackup (set, TRUE);
	}
}
/**
@brief log a function to be called to setup tree-option defaults if needed

@a func needs to iclude a call to e2_option_tree_setup_defaults ()

@param set the set data structure
@param func pointer to function which will install default tree data

@return
*/
void e2_option_tree_prepare_defaults (E2_OptionSet *set, void (*func)(E2_OptionSet*))
{
	set->ex.tree.def.func = func;
}
/**
@brief setup default config tree contents, ready for installation

This creates a NULL-terminated vector of tree-row data strings, which in
aggregate (are expected to) have the same format as the config file data for
the set being processed.

@param set the set data structure
@param first start of a null-terminated series of arguments,
 each a newly-allocated string which is to be converted to a tree row

@return
*/
void e2_option_tree_setup_defaults (E2_OptionSet *set, gchar *first, ...)
{
	if (first != NULL)
	{
	//	printd (DEBUG, "preparing default option tree for %s>", first);
		va_list args;
		va_start (args, first);
		gchar *cur = first;
		GPtrArray *lines = g_ptr_array_sized_new (32);
		while (cur)
		{
	//		cur = g_strdup (cur);
			//store ptr to constant string (do not free, later)
			g_ptr_array_add (lines, (gpointer) cur);
			cur = va_arg (args, gchar *);
		}
		va_end (args);
	//	set->ex.tree.def_num = (gint) lines->len; UNUSED
		g_ptr_array_add (lines, NULL);  //null signals end, to parser
		set->ex.tree.def.tree_strings = (gchar **)lines->pdata;
		g_ptr_array_free (lines, FALSE);
	}
}
/**
@brief install default config tree data where needed

after loading the config file (if any) at session start, check if any tree
option needs a default config, (stored in arrays), install it if so, then
free the default tree 'branches'

@return
*/
void e2_option_tree_install_defaults (void)
{
	guint i;
	gpointer *walker;
	E2_OptionSet *set;
	for (i = 0, walker = options_array->pdata; i < options_array->len; i++, walker++)
	{
		set = *walker;
		if (set->type == E2_OPTION_TYPE_TREE)
		{
			if (!set->ex.tree.synced //option needs default data
				&& set->ex.tree.def.func != NULL)	//setup func has been logged
			{
//				printd (DEBUG, "installing default option tree %s>", set->name);
				//call the defaults-install function, which in turn calls
				// e2_option_tree_setup_defaults () which sets up a
				// pointer array at set->ex.tree.def.tree_strings again
				(*set->ex.tree.def.func) (set);
				//parse the data
				e2_option_tree_set_from_array (set->name,
					set->ex.tree.def.tree_strings, NULL, NULL);
				//cleanup
				g_strfreev (set->ex.tree.def.tree_strings);
			}
			set->ex.tree.def.tree_strings = NULL;	//no more use for function, always zap the vector pointer
		}
	}
}
/* *
@brief Append top-level tree iter for @a name, and populate if from @a options

@param name name of option
@param iter pointer to iter which stores the result
@param n_options number of treestore columns
@param options array of values to store in the new iter

@return
*/
/* UNUSED
void e2_option_tree_add_simple (gchar *name, GtkTreeIter *iter, gint n_options,
	void *options[])
{
	E2_OptionSet *set = e2_option_tree_get (name);
	if (set != NULL)
		e2_option_tree_add (set, iter, NULL, FALSE, TRUE, n_options, options);
} */
/**
@brief Add tree iter to @a set, and populate if from @a options
Any NULL value in @a options is ignored, its stored value is undefined
@param set data struct for the set
@param iter pointer to iter which stores the result
@param parent pointer the iter which is the "base" for relative insertion
@param sibling TRUE to add new iter at same level as @a parent, FALSE to add a child
@param before TRUE to add iter before @a parent
@param n_options number of treestore columns
@param options array of values to store in the new iter

@return
*/
void e2_option_tree_add (E2_OptionSet *set, GtkTreeIter *iter, GtkTreeIter *parent,
	gboolean sibling, gboolean before, gint n_options, void *options[])
{
	if (sibling)
	{	//new iter at same level
		if (before)
			//before parent, or appended to toplevel if parent == NULL
			gtk_tree_store_insert_before (set->ex.tree.model, iter, NULL, parent);
		else
			//after parent, or prepended to toplevel if parent == NULL
			gtk_tree_store_insert_after (set->ex.tree.model, iter, NULL, parent);
	}
	else
	{	//new child iter
		if (before)
			//append to parent's children
			gtk_tree_store_insert_before (set->ex.tree.model, iter, parent, NULL);
		else
			//prepend to parent's children
			gtk_tree_store_insert_after (set->ex.tree.model, iter, parent, NULL);
	}

	gint i;
	for (i = 0; i < n_options; i++)
	{
		if (options[i] != NULL)
			gtk_tree_store_set (set->ex.tree.model, iter, i, options[i], -1);
	}
	gtk_tree_store_set (set->ex.tree.model, iter, set->ex.tree.columns_num, TRUE, -1);

	e2_option_tree_flag_change (set);
}

void e2_option_tree_add_default (gchar *option, GtkTreeIter *iter, GtkTreeIter *parent, gboolean sibling)
{
	E2_OptionSet *set = e2_option_tree_get (option);
	if (set != NULL)
	_e2_option_tree_add_default (set, iter, parent, sibling);
}

static void _e2_option_tree_add_default (E2_OptionSet *set, GtkTreeIter *iter, GtkTreeIter *parent, gboolean sibling)
{
	void *arg[set->ex.tree.columns_num];
	GList *column;
	gint i = 0;
	for (column = set->ex.tree.columns; column != NULL; column = g_list_next (column))
	{
		E2_OptionTreeColumn *opt = column->data;
		switch (opt->type)
		{
//			case E2_OPTION_TREE_TYPE_INT:
//			case E2_OPTION_TREE_TYPE_HIDDENINT:
			case E2_OPTION_TREE_TYPE_BOOL:
			case E2_OPTION_TREE_TYPE_HIDDENBOOL:
				arg[i] = GINT_TO_POINTER (opt->idef);
				break;
			case E2_OPTION_TREE_TYPE_SEL:
			case E2_OPTION_TREE_TYPE_STR:
			case E2_OPTION_TREE_TYPE_HIDDENSTR:
			case E2_OPTION_TREE_TYPE_ICON:
			case E2_OPTION_TREE_TYPE_KEY:
#ifdef E2_MOUSECUSTOM
			case E2_OPTION_TREE_TYPE_BUTTON:
# ifdef E2_PTRGESTURES
	        case E2_OPTION_TREE_TYPE_GESTURE:
# endif
#endif
				arg[i] = (void *)opt->sdef;
				break;
			default:
				break;
		}
		i++;
	}

	e2_option_tree_add (set, iter, parent, sibling, FALSE, set->ex.tree.columns_num, arg);

	if ((parent != NULL) && (sibling == FALSE))
		gtk_tree_view_expand_all (GTK_TREE_VIEW (set->widget));
	gtk_tree_view_set_cursor  (GTK_TREE_VIEW (set->widget),
		gtk_tree_model_get_path (GTK_TREE_MODEL (set->ex.tree.model), iter),
		gtk_tree_view_get_column (GTK_TREE_VIEW (set->widget), 0), FALSE);

	if (set->widget)
	{
		gtk_widget_grab_focus (set->widget);
	}
}

void e2_option_tree_del (gchar *option, GtkTreeIter *iter)
{
	E2_OptionSet *set = e2_option_tree_get (option);
	if (set != NULL)
		e2_option_tree_del_direct (set, iter);
}

void e2_option_tree_del_direct (E2_OptionSet *set, GtkTreeIter *iter)
{
	gtk_tree_store_remove (set->ex.tree.model, iter);
	e2_option_tree_flag_change (set);
}

static void _e2_option_tree_move (E2_OptionSet *set, GtkTreeIter *iter, GtkTreePath *path_dest, gboolean sibling, gboolean before)
{
	//find out real number of columns, we also want to move the meta data
	gint n = gtk_tree_model_get_n_columns (set->ex.tree.model);
	gpointer values[n];
	gint i;
	//save old row contents
	for (i = 0; i < n; i++)
	{
		gtk_tree_model_get (set->ex.tree.model, iter, i, values + i, -1);
	}
	GtkTreePath *path_old = gtk_tree_model_get_path (set->ex.tree.model, iter);
	GtkTreeRowReference *ref_old = gtk_tree_row_reference_new (set->ex.tree.model, path_old);
	GtkTreeIter iter2;
	if (gtk_tree_model_get_iter (set->ex.tree.model, &iter2, path_dest))
	{
		GtkTreeIter iter_new;
		e2_option_tree_add (set, &iter_new, &iter2, sibling, before, n, values);

		gtk_tree_path_free (path_old);
		path_old = gtk_tree_row_reference_get_path (ref_old);
		if (gtk_tree_model_get_iter (set->ex.tree.model, &iter2, path_old))
		{
			if (gtk_tree_model_iter_has_child (set->ex.tree.model, &iter2))
			{
				gboolean expand = FALSE;
				if (gtk_tree_view_row_expanded (GTK_TREE_VIEW (set->widget), path_old))
				{
					expand = TRUE;
					gtk_tree_view_collapse_row (GTK_TREE_VIEW (set->widget), path_old);
				}

				GtkTreePath *path_new = gtk_tree_model_get_path (set->ex.tree.model, &iter_new);
				GtkTreeIter iter_child;
				while (gtk_tree_model_iter_children (set->ex.tree.model, &iter_child, &iter2))
					_e2_option_tree_move (set, &iter_child, path_new, FALSE, TRUE);

				if (expand)
					gtk_tree_view_expand_row (GTK_TREE_VIEW (set->widget), path_new, FALSE);
				gtk_tree_path_free (path_new);
			}
			e2_option_tree_del_direct (set, &iter2);
		}

		gtk_tree_view_set_cursor  (GTK_TREE_VIEW (set->widget),
			path_dest, gtk_tree_view_get_column (GTK_TREE_VIEW (set->widget), 0), FALSE);
	}
	gtk_tree_path_free (path_old);
	gtk_tree_row_reference_free (ref_old);
}

static void _e2_option_tree_set (E2_OptionSet *set, GtkTreeIter *iter, gint col, void *data)
{
	gtk_tree_store_set (GTK_TREE_STORE (set->ex.tree.model), iter, col, data, -1);
	e2_option_tree_flag_change (set);
}
/**
@brief Read tree config data from @a f and setup treestore and model
If @a index is non-NULL, the value initially stored there index is the index
of the start of the set data, or of the data corresponding to @a root_iter
if that is not at the start of the set. The fist line is skipped (unless
we need to log the lot as an unknown set)
Critical characters in @a f are assumed to be ascii
Unknown options are logged as appropriate
If @a root_iter is NULL, the root node is used, of course.
This does not cope with badly-formed trees e.g. with missing level

@param name set name string
@param f NULL-terminated array of strings in config file format
@param index pointer to store for index of @a f, or NULL
@param root_iter pointer to treestore iter under which the data are to be added

@return TRUE if the option was installed properly
*/
gboolean e2_option_tree_set_from_array (gchar *name, gchar *f[], gint *index,
	GtkTreeIter *root_iter)
{
	gchar *line; //pointer to the current line
	gint idx = (index == NULL) ? 0 : *index;	//array index

	E2_OptionSet *set = e2_option_get_simple (name);
	if (set == NULL)
	{	//we've found an unknown tree option
//		printd (DEBUG, "found config tree for unknown option %s", name);
		//accumulate the lines
		GPtrArray *unknown_lines = g_ptr_array_sized_new (11);
		gboolean freefirst;
		if (strstr (f[idx], "=<") == NULL)
		{	//incomplete tree, from transient store-updates
			line = g_strconcat (name, "-part=<", NULL);
			g_ptr_array_add (unknown_lines, line);
			freefirst = TRUE;
		}
		else
			freefirst = FALSE;
		while ((line = f[idx]) != NULL)
		{
			idx++;	//now we know array's not finished
			g_strchomp (line);
			g_ptr_array_add (unknown_lines, line);
			if (line[0] == '>')
			{
				g_ptr_array_add (unknown_lines, NULL);
				gchar *value = g_strjoinv ("\n", (gchar **)unknown_lines->pdata);
				e2_option_unknown_record (g_strdup (name), value);
				break;
			}
		}
		if (freefirst)
			g_free (g_ptr_array_index (unknown_lines, 0));
		g_ptr_array_free (unknown_lines, TRUE);
		if (index != NULL)
			*index = idx;
		return FALSE;
	}

	//process known tree option
	idx++;	//pass initial line like "name=<" or something else like
			//"\tdialogs||||" for transient bindings store update
	//flag for whether the tree data is still valid
	E2_TreeStatus tree_mode = E2_TREE_STARTED;
	//list of iters for managing parent-child dependencies
	GList *parents = g_list_append (NULL, root_iter);
	GList *member;
	gboolean firstline = TRUE;
	gint firstdepth = 0;

	while ((line = f[idx]) != NULL)
	{
		idx++;	//now we know array's not finished
		g_strchomp (line);
		if (line[0] == '>') break;
		//ignore empty lines and comments
		if (*line == '\0') continue;
		if (line[0] == '#') continue;
		if (tree_mode == E2_TREE_ABORTED) continue;
		//path-depth of the current tree option row
		//determined by tabs at the start of the line
		gint level = 0;
		while (line[level] == '\t')
			level++;
		line += level;
		if (firstline)
		{
			//1st valid line corresponds to 1st child of root_iter,
			//not necessarily at depth 0 of the tree
			firstdepth = level;
			firstline = FALSE;
		}
		//"unescape" the first character if need be
		if (line[0] == '\\' && line[1] == '>')
			line++;

		//strsplit() isn't a great approach because it makes
		//unescaping complicated, but ...
		gchar **split = g_strsplit (line, "|", -1);
		void *tmp[set->ex.tree.columns_num];
		//split counter, increased when stepping through **split
		gint i = 0;
		//tree column counter
		gint j = 0;
		//tree column pointer
		GList *columns;
		//unescape helper for concatting, later on
		GList *freecats = NULL;
		//transform splitted values to void pointers and add default values
		//in case of a missing value/field
		for (columns = set->ex.tree.columns; columns != NULL; columns = columns->next)
		{
			if (split[i] == NULL)
			{
				//not enough separators in the line = bad config data
				tree_mode = E2_TREE_ABORTED;
				break;
			}
			//current tree column-data pointer
			E2_OptionTreeColumn *coldata = columns->data;
			if ( *(split[i]) != '\0')
			{
				gchar *value = split[i];
				//unescape | and concat values
				while (split[i + 1] != NULL)
				{
					gint len = strlen (value);
					if ((len > 0) && (value[len - 1] == '\\'))
					{
						value[len - 1] = '\0';
						value = g_strconcat (value, "|", split[i + 1], NULL);
						//save pointer to free it after
						//knowing how long this really gets
						//(there could be several escaped | in one line)
						freecats = g_list_append (freecats, value);
						i++;
					} else
						break;
				}
				//get void pointer to add it to the model
				tmp[j] = _e2_option_tree_get_void_simple (coldata, value);
			}
			else
				//empty string found found, use default value
				tmp[j] = _e2_option_tree_get_void_simple (coldata, (void *)coldata->sdef);
			j++;
			i++;
		}

		if (tree_mode != E2_TREE_ABORTED)
		{
			//append row to children of parent, and populate it
			GtkTreeIter iter;
			i = level - firstdepth;
			e2_option_tree_add (set, &iter, g_list_nth_data (parents, i),
				FALSE, TRUE, set->ex.tree.columns_num, tmp);
			//add iter pointer to be able to add a child to this row later on
			GList *member = g_list_nth (parents, i+1);
			if (member == NULL)
				parents = g_list_append (parents, gtk_tree_iter_copy (&iter));
			else
			{
				gtk_tree_iter_free (member->data);
				member->data = gtk_tree_iter_copy (&iter);
			}
		}
		else
		{
			//clear any data recorded already
			gtk_tree_store_clear (set->ex.tree.model);
//			set->ex.tree.synced = FALSE; redundant
			printd (WARN, "bad config data for %s, using default", set->name);
		}

		//cleanup
		g_strfreev (split);
		for (member = freecats; member != NULL; member = member->next)
			g_free (member->data);
		g_list_free (freecats);
	}

	if (line != NULL && tree_mode != E2_TREE_ABORTED)
	{
		//found config for this tree option, so don't init it with the default
		//config later on
		set->ex.tree.synced = TRUE;
//		set->ex.tree.def.tree_strings = NULL;
		//make sure the set is recognised as clean
		set->ex.tree.flags &= ~E2_OPTION_TREE_SET_EDITED;
	}
	//cleanup
//	parents = g_list_remove (parents, NULL);
//	g_list_foreach (parents, (GFunc) gtk_tree_iter_free, NULL);
	for (member = parents->next; member != NULL; member = member->next)
		gtk_tree_iter_free (member->data);
	g_list_free (parents);

	if (index != NULL)
		*index = idx;
	return (tree_mode != E2_TREE_ABORTED);
}

E2_OptionSet *e2_option_tree_get (gchar *option)
{
	E2_OptionSet *set;
	set = (E2_OptionSet *) g_hash_table_lookup (options_hash, option);
	if (set == NULL)
	{
		printd (WARN, "trying to access option '%s' which doesn't exist", option);
		return NULL;
	}
	if (set->type != E2_OPTION_TYPE_TREE)
	{
		printd (WARN, "trying to access tree option '%s' which isn't a tree", set->name);
		return NULL;
	}
	return set;
}

void *_e2_option_tree_get_void_simple (E2_OptionTreeColumn *col, gchar *option)
{
	switch (col->type)
	{
		case E2_OPTION_TREE_TYPE_BOOL:
		case E2_OPTION_TREE_TYPE_HIDDENBOOL:
			if (!strcmp (option, "true"))
				return (void *) TRUE;
			else
				return (void *) FALSE;
			break;
		case E2_OPTION_TREE_TYPE_STR:
		case E2_OPTION_TREE_TYPE_HIDDENSTR:
		case E2_OPTION_TREE_TYPE_ICON:
		case E2_OPTION_TREE_TYPE_SEL:
		case E2_OPTION_TREE_TYPE_KEY:
#ifdef E2_MOUSECUSTOM
		case E2_OPTION_TREE_TYPE_BUTTON:
# ifdef E2_PTRGESTURES
        case E2_OPTION_TREE_TYPE_GESTURE:
# endif
#endif
			return (void *) option;
		default:
			return NULL;
			break;
	}
	return NULL;
}
