/* $Id: e2_option_unknown.c 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2004-2010 tooar <tooar@emelfm2.net>

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
//#include "e2_option_unknown.h"

/**
@brief record a transient option string

Any already-registered option with the same name will be removed first.
For single-valued options, @a string will be like name=value
For tree-options, @a string will be multi-lined, like
<name\\nstring1\\nstring2\\n....>
DO NOT FREE the argument-strings in the calling code.
@param option string with the name of the option
@param value option string, in config file format

@return
*/
void e2_option_unknown_record (gchar *option, gchar *value)
{
	g_hash_table_replace (options_queue, option, value);
}
/**
@brief get option value(s) from un-registered options list

@param set option data struct, including default values

@return
*/
void e2_option_transient_value_get (E2_OptionSet *set)
{
	gchar *value;
	gchar **split;
	if ((value = g_hash_table_lookup (options_queue, set->name)) != NULL)
	{
		if (set->type == E2_OPTION_TYPE_TREE)
		{
			split = g_strsplit (value, "\n", -1);
			//parse the data
			e2_option_tree_set_from_array (set->name, split, NULL, NULL);
		}
		else
		{
			split = g_strsplit (value, "=", 2);
			//store value for the set
			if (split[1] != NULL)
				e2_option_set_value_from_string (set, split[1]);
			else
				printd (WARN, "could not find value for option '%s'", set->name);
		}
		g_strfreev (split);
		g_hash_table_remove (options_queue, set->name);
	}
}

/* model for a transient tree option

static void _e2_cfgdlg_tree_defaults (E2_OptionSet *set)
{
	e2_option_tree_setup_defaults (set,
	g_strdup("unknown-tree=<", NULL),
	g_strdup("STRING 10"),
	g_strdup("string 20"),
	g_strdup("STRING 30"),
	g_strdup(">"),
	NULL);
}
	E2_OptionSet *set = e2_option_tree_register ("unknown-tree", "unknown-tree", "unknown-tree", //no translation
		NULL, NULL, NULL, E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL,
		E2_OPTION_FLAG_BASIC | E2_OPTION_FLAG_BUILDBARS);
	e2_option_tree_add_column (set, _("Label"), E2_OPTION_TREE_TYPE_STR, 0, "",
		NULL, NULL, 0, NULL, NULL);
	[more columns]
	e2_option_tree_create_store (set);
//	e2_option_tree_prepare_defaults (set, _e2_cfgdlg_tree_defaults);
	e2_option_transient_value_get (set);
	if (!set->ex.tree.synced) //option needs default data
	{
//		(*set->ex.tree.def.func) (set);
		_e2_cfgdlg_tree_defaults (set);
		//parse the data
		e2_option_tree_set_from_array (set->name, set->ex.tree.def.tree_strings,
			NULL, NULL);
		//cleanup
		g_strfreev (set->ex.tree.def.tree_strings);
	}
	set->ex.tree.def.tree_strings = NULL;	//always zap the function/vector pointer
}
  model for a transient int option
{
	e2_option_int_register ("unknown-int", group_name, _("label"),
		_("Tip"),
		NULL, 50, 0, 10000,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP); //no rebuild
	e2_option_transient_value_get (set);
}
*/
