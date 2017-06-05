/* $Id: e2p_clipnames.c 2865 2013-10-27 09:18:13Z tpgww $

Copyright (C) 2003-2010 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Michael Clark

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
@file plugins/e2p_names_clip.c
@brief plugin to copy name and perhaps path of each selected item to the clipboard
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_plugins.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "clipnames"

static PluginIface iface;

/**
@brief copy name and perhaps path of each selected item to the clipboard

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_copy_to_clipboard (gpointer from, E2_ActionRuntime *art)
{
	GString *text;
	gchar *names;
	gboolean full =
			(ACTION_BUTTON (art,1) && ACTION_MASK (art,GDK_SHIFT_MASK))
		 || (art->data != NULL && strstr ((gchar*)art->data, _("shift")) != NULL);
	gboolean lined =
			(ACTION_BUTTON (art,1) && ACTION_MASK (art,GDK_CONTROL_MASK))
		 || (art->data != NULL && strstr ((gchar*)art->data, _("ctrl")) != NULL);
	//art was probably packed during context-menu creation - which is too early
	//for pressing mod-keys
	if ((!full || !lined) /*&& (GTK_IS_BUTTON (from) || GTK_IS_MENU_ITEM (from))*/)
	{
		GdkModifierType state = art->state; //can ignore the custom button-number
		full = full || (state & GDK_SHIFT_MASK);
		lined = lined || (state & GDK_CONTROL_MASK);
	}

	if (full || lined)
	{
		//get quoted list of selected-item-names, to facilitate their separation
		names = e2_utils_expand_macros ("%f", NULL);
		if (names == NULL)
			return FALSE;

		gchar sep = (lined) ? '\n' : ' ';

		gchar **split = g_strsplit (names, "\"", -1);
		gchar **tmp = split;
		text = g_string_new ("");
		while (*tmp != NULL)
		{
			if (**tmp == '\0')	//empty string before 1st " and after last "
			{
				tmp++;
				continue;
			}
			else if (**tmp == ' ')
				text = g_string_append_c (text, sep);
			else
			{
				if (full)
//E2_VFSTMPOK
					text = g_string_append (text, curr_view->dir);
				text = g_string_append (text, *tmp);
			}
			tmp++;
		}
		g_free (names);
		g_strfreev (split);
	}
	else
	{
		//get un-quoted list of selected item names
		names = e2_utils_expand_macros ("%%f", NULL);
		if (names == NULL)
			return FALSE;
		text = g_string_new (names);
	}

	GtkClipboard *clip = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);
  	gtk_clipboard_set_text (clip, text->str, text->len);
	g_string_free (text, TRUE);
	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(6),_("copy_name"),_e2p_copy_to_clipboard,
		_("Copy _names"),
		_("Copy path or name of each selected item to the clipboard"),
		"plugin_clip"E2ICONTB)
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)
	return ret;
}
