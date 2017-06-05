/* $Id: e2_action_option.c 2827 2013-10-23 07:53:39Z tpgww $

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

#include "emelfm2.h"
#include "e2_action_option.h"

/**
@brief set the value of a non-tree config option
requires arg string in config-file format like "option-name=option_value"
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_action_option_set (gpointer from, E2_ActionRuntime *art)
{
	gchar *tmp[2] = { (gchar *)art->data, NULL };
	e2_option_read_array (tmp);
	return TRUE;
}

void e2_action_option_actions_register ()
{
	E2_Action action =
	{g_strconcat(_A(9),".",_A(85),NULL),_e2_action_option_set,FALSE,E2_ACTION_TYPE_ITEM,0,NULL,NULL};
	e2_action_register (&action);
}
