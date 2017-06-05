/* $Id: e2_option_bool.c 2064 2010-03-12 13:15:36Z tpgww $

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

/**
@file src/config/e2_option_bool.c
@brief boolean options handling

boolean options handling

*/

#include "emelfm2.h"
#include "e2_option.h"
#include "e2_option_bool.h"

/**
@brief register new boolean option

register new boolean option.

@param name    name of the option, generally a constant string but sometimes runtime-created
@param group   group the option belongs to, used in config dialog, a r-t string
@param desc    textual description of the option used in config dialog, a r-t _() string
@param tip     tooltip used when displaying the config dialog, a _() string
@param depends name of another option this one depends on
@param value   value for the option
@param flags   bitflags determining how the option data is to be handled

@return  E2_OptionSet of the new option
*/
E2_OptionSet *e2_option_bool_register (gchar *name, gchar *group, gchar *desc,
	gchar *tip, gchar *depends, gboolean value, E2_OptionFlags flags)
{
	E2_OptionSet *set = e2_option_register (E2_OPTION_TYPE_BOOL, name, group,
		desc, tip, depends, flags);
	//set ->i/sval
	set->hook_freezed = TRUE;
	e2_option_bool_set_direct (set, value);
	set->hook_freezed = FALSE;
	return set;
}
/**
@brief get current value of boolean option

@param option name of the option

@return current value (T/F) of the option
*/
gboolean e2_option_bool_get (gchar *option)
{
	E2_OptionSet *set = e2_option_get (option);
	if (set == NULL)
	{
		printd (WARN, "trying to get option '%s' which doesn't exist", option);
		return FALSE;
	}
	if (set->type == E2_OPTION_TYPE_BOOL)
		return e2_option_bool_get_direct (set);
	else
	{
		printd (WARN, "trying to get option '%s' as boolean", option);
		return FALSE;
	}
}
/**
@brief get current value of boolean option

@param set pointer to data struct for the option

@return current value (T/F) of the option
*/
gboolean e2_option_bool_get_direct (E2_OptionSet *set)
{
	return set->ival;
}
/**
@brief toggle value of boolean option

@param option name of the option

@return the new value (T/F) of the option
*/
gboolean e2_option_bool_toggle (gchar *option)
{
	E2_OptionSet *set = e2_option_get (option);
	if (set == NULL)
		return FALSE;
	if (set->type == E2_OPTION_TYPE_BOOL)
		return e2_option_bool_toggle_direct (set);
	else
	{
		printd (WARN, "trying to toggle bool option '%s', which isn't a bool",
			set->name);
		return FALSE;
	}
}
/**
@brief toggle value of boolean option

@param set pointer to data struct for the option

@return the new value (T/F) of the option
*/
gboolean e2_option_bool_toggle_direct (E2_OptionSet *set)
{
	if (e2_option_bool_get_direct (set))
	{
		e2_option_bool_set_direct (set, FALSE);
		return FALSE;
	} else
	{
		e2_option_bool_set_direct (set, TRUE);
		return TRUE;
	}
}
/**
@brief set current value of boolean option

@param option name of the option
@param value the state (T/F) to be set

@return the new value (T/F) of the option, FALSE if it doesn't exist
*/
gboolean e2_option_bool_set (gchar *option, gboolean value)
{
	E2_OptionSet *set = e2_option_get (option);
	if (set == NULL)
		return FALSE;
	if (set->type == E2_OPTION_TYPE_BOOL)
		e2_option_bool_set_direct (set, value);
	else
	{
		printd (WARN, "trying to bool set option '%s' which isn't a bool",
			option);
		return FALSE;
	}
	return value;
}
/**
@brief set current value of boolean option

@param set pointer to data struct for the option
@param value the state (T/F) to be set

@return the new value (T/F) of the option
*/
gboolean e2_option_bool_set_direct (E2_OptionSet *set, gboolean value)
{
	set->ival = value;
	set->sval = (value) ? "true" : "false";
	if (!set->hook_freezed)
		e2_hook_list_run (&set->hook_value_changed, GINT_TO_POINTER (value));
	return value;
}
