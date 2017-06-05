/* $Id: e2_hook.h 2743 2013-09-19 22:29:00Z tpgww $

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

#ifndef __E2_HOOK_H__
#define __E2_HOOK_H__

#include "e2_utils.h"

void e2_hook_unattach (E2_Duo *duo);

GHook *e2_hook_register (GHookList *hook_list, gboolean (*func)(gpointer,gpointer), gpointer data) G_GNUC_MALLOC;
gboolean e2_hook_unregister (GHookList *hook_list, gboolean (*func)(gpointer,gpointer),
	gpointer data, gboolean use_data);
#define e2_hook_unregister_simple(hook_list, func) \
	e2_hook_unregister (hook_list, func, NULL, FALSE)
void e2_hook_list_run (GHookList *hook_list, gpointer data);

#endif //ndef __E2_HOOK_H__
