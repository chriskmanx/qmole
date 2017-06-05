/* $Id: e2_hook.c 2659 2013-08-10 06:37:09Z tpgww $

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

/**
@file src/utils/e2_hook.c
@brief Hook utility functions

This file contains hook utility functions to register hooks in a
hook_list and to run the hook list.
*/

#include "emelfm2.h"
#include "e2_hook.h"
#include "e2_utils.h"

/**
@brief remove @a hook from @a hook_list
@param hook_list pointer to struct related to list of hook functions
@param hook data struct for the hook func to unregister
@return TRUE if @a hook != NULL and was destroyed
*/
static gboolean _e2_hook_unregister (GHookList *hook_list, GHook *hook)
{
	if (hook != NULL)
	{
		g_hook_destroy_link (hook_list, hook);
		return (!G_HOOK_IS_VALID (hook));
	}
	return FALSE;
}
/**
@brief run hook function of @a hook
@param hook data struct for the hook func
@param data pointer to data supplied to e2_hook_list_run()
@return the value returned by the hook func
*/
static gboolean _e2_hook_list_run (GHook *hook, gpointer data)
{
//	printd (DEBUG, "_e2_hook_list_run (hook:,data:)");
	gboolean (*func) (gpointer, gpointer) = hook->func;
	return func (data, hook->data);
}

  /*********************/
 /***** callbacks *****/
/*********************/

/**
@brief unattach hook function referred to in @a duo
@param duo pointer to data struct with GHookList* and GHook*
@return
*/
void e2_hook_unattach (E2_Duo *duo)
{
	_e2_hook_unregister (duo->b, duo->a);
	DEMALLOCATE (E2_Duo, duo);
}

  /******************/
 /***** public *****/
/******************/

/**
@brief add a hook for running @a func to @a hook_list
@param hook_list pointer to struct related to list of hook functions
@param func pointer to function to run
@param data pointer to data for @a func
@return the data struct for the hook func
*/
GHook *e2_hook_register (GHookList *hook_list, gboolean (*func)(gpointer,gpointer),
	gpointer data)
{
	if (!hook_list->is_setup)
		g_hook_list_init (hook_list, sizeof (GHook));
	GHook *hook = g_hook_alloc (hook_list);
	hook->data = data;
	hook->func = func;
	g_hook_append (hook_list, hook);
	return hook;
}
/**
@brief remove the hook in @a hook_list which runs @a func
@param hook_list pointer to struct related to list of hook functions
@param func pointer to function to find
@param data pointer to data to find if @a use_data is TRUE
@param use_data TRUE to find a hook with data matching @a data
@return TRUE if the hook was found and destroyed
*/
gboolean e2_hook_unregister (GHookList *hook_list, gboolean (*func)(gpointer,gpointer),
	gpointer data, gboolean use_data)
{
	GHook *hook = (use_data) ?
		g_hook_find_func_data (hook_list, TRUE, func, data):
		g_hook_find_func (hook_list, TRUE, func);
	return _e2_hook_unregister (hook_list, hook);
}
/**
@brief run functions in @a hook_list, supplying them @a data
Currently-running functions in the list are skipped
Any hook func that returns FALSE will be removed from the hooklist
@param hook_list pointer to struct related to list of hook functions
@param data data supplied as 1st argument to each hook func
@return
*/
void e2_hook_list_run (GHookList *hook_list, gpointer data)
{
//	printd (DEBUG, "e2_hook_list_run (hook_list: %x,data: %x)", hook_list, data);
	if (hook_list->is_setup)
		g_hook_list_marshal_check (hook_list, FALSE, _e2_hook_list_run, data);
}
