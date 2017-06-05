/* $Id: e2_option_sel.c 2815 2013-10-13 07:00:55Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

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

#include "emelfm2.h"
#include "e2_option.h"
#include "e2_option_sel.h"

static void _e2_option_sel_activated_cb (GtkMenuItem *menu_item, E2_OptionSet *set)
{
	GtkWidget *controller = g_object_get_data (G_OBJECT (menu_item),
		"e2-controller-widget");
	if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (controller),
		"e2-controller-blocked")))
			return;
	if (!gtk_check_menu_item_get_active (GTK_CHECK_MENU_ITEM (menu_item)))
		return;
//	NEEDCLOSEBGL
	GSList *group = gtk_radio_menu_item_get_group (GTK_RADIO_MENU_ITEM (menu_item));
//	NEEDOPENBGL
	gint index = 2 - g_slist_index (group, menu_item);
	e2_option_sel_set_direct (set, index);
}
/**
@brief conform a check-menu item state to the corresponding option value
This is a hook function
@param data menu index
@param submenu widget containing item to be updated

@return TRUE always, so func is not delisted after run
*/
static gboolean _e2_option_sel_value_changed_hook (gpointer data, GtkWidget *submenu)
{
	GtkWidget *controller = g_object_get_data (G_OBJECT (submenu),
		"e2-controller-widget");
	if (GPOINTER_TO_INT (g_object_get_data (G_OBJECT (controller),
		"e2-controller-blocked")))
			return TRUE;
	gint value = GPOINTER_TO_INT (data);
	GList *children = gtk_container_get_children (GTK_CONTAINER (submenu));
	GtkWidget *item = g_list_nth_data (children, value);
	gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (item), TRUE);
	return TRUE;
}

  /******************/
 /***** public *****/
/******************/

/**
@brief create a sub-menu of radio items reflecting the alternatives in @a set
Args for @a func should be (GtkMenuItem*,some pointer)
@param controller
@param menu the parent menu for the new submenu
@param set data for the set whose values are to be presented in the submenu
@param func callback for "activate" signal on each radio-menu-item
@param data data to be supplied to @a func

@return the submenu widget
*/
GtkWidget *e2_option_sel_add_menu_widget (GtkWidget *controller, GtkWidget *menu,
	E2_OptionSet *set, void (*func)(), gpointer data)
{
	//note set->tip not effective for an item with a submenu
	GtkWidget *submenu = e2_menu_add_submenu (menu, set->desc, NULL);
	g_object_set_data (G_OBJECT (submenu), "e2-controller-widget", controller);
	//this will update the menu in case the option value changes
	e2_option_attach_value_changed_simple (set, submenu,
		(HookFunc)_e2_option_sel_value_changed_hook, submenu);

	GSList *group = NULL;
	gint i = 0;
	while (set->ex.sel.def[i] != NULL)
	{
		GtkWidget *item = e2_menu_add_radio (submenu, &group,
			(gchar *)set->ex.sel.def[i], (e2_option_int_get_direct (set) == i),
			func, data);
		e2_widget_set_safetip (item, set->tip);
		g_object_set_data (G_OBJECT (item), "e2-controller-widget", controller);
		g_signal_connect (G_OBJECT (item), "activate",
			G_CALLBACK (_e2_option_sel_activated_cb), set);
		i++;
	}
	return submenu;
}

void e2_option_sel_add_widget (GtkWidget *dialog, GtkWidget *box,
	GtkSizeGroup *size_group, E2_OptionSet *set)
{
	GtkWidget *event = e2_widget_add_eventbox (box, FALSE, 0);
#ifdef USE_GTK3_0
	GtkWidget *hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING);
#else
	GtkWidget *hbox = gtk_hbox_new (FALSE, E2_PADDING);
#endif
	gtk_container_add (GTK_CONTAINER (event), hbox);
	gchar *label_text = g_strconcat (set->desc, ":", NULL);
	GtkWidget *label = e2_widget_add_mid_label (hbox, label_text, 0.0, FALSE, 0);
	g_free (label_text);
	gtk_size_group_add_widget (size_group, label);
	GtkWidget *combo = e2_combobox_add (hbox, FALSE, E2_PADDING, NULL, NULL, NULL,
		E2_COMBOBOX_MENU_STYLE);
	e2_combobox_append_history_strv (combo, set->ex.sel.def);
	gtk_combo_box_set_active (GTK_COMBO_BOX (combo), set->ival);
	e2_widget_set_safetip (event, set->tip);
	set->widget = combo;
	gtk_widget_show_all (event);
}

E2_OptionSet *e2_option_sel_register (gchar *name, gchar *group, gchar *desc,
	gchar *tip, gchar *depends, gint def, const gchar **values,
	E2_OptionFlags flags)
{
	E2_OptionSet *set = e2_option_register (E2_OPTION_TYPE_SEL, name, group,
		desc, tip, depends, flags);
	set->ex.sel.def = g_strdupv ((gchar **)values);
	gint i = 0;
	while (set->ex.sel.def[i++] != NULL);
	set->ex.sel.def_count = i - 1;
	//set s/ival
	set->hook_freezed = TRUE;
	e2_option_sel_set_direct (set, def);
	set->hook_freezed = FALSE;
	return set;
}

gint e2_option_sel_set (gchar *option, gint value)
{
	E2_OptionSet *set;
	set = e2_option_get (option);
	if (set != NULL && set->type == E2_OPTION_TYPE_SEL)
		return e2_option_sel_set_direct (set, value);
	else
	{
		printd (WARN, "trying to set sel option '%s' which isn't a sel", option);
		return 0;
	}
}

gint e2_option_sel_set_direct (E2_OptionSet *set, gint value)
{
	if (value < 0)
		value = 0;
	if (value > set->ex.sel.def_count)
		value = set->ex.sel.def_count;
	set->ival = value;
	set->sval = (gchar *) set->ex.sel.def[value];
	if (!set->hook_freezed)
		e2_hook_list_run (&set->hook_value_changed, GINT_TO_POINTER (value));
	return value;
}

gint e2_option_sel_get (gchar *option)
{
	E2_OptionSet *set;
	set = e2_option_get (option);
	if (set != NULL && set->type == E2_OPTION_TYPE_SEL)
		return e2_option_sel_get_direct (set);
	return -1;
}

gint e2_option_sel_get_direct (E2_OptionSet *set)
{
	return set->ival;
}
