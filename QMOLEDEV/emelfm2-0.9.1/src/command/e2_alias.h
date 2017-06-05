/* $Id: e2_alias.h 2743 2013-09-19 22:29:00Z tpgww $

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

#ifndef __E2_ALIAS_H__
#define __E2_ALIAS_H__

#include "emelfm2.h"
#include <regex.h>
#include "e2_option.h"

typedef struct _E2_Alias
{
	gchar *match;
	gchar *replace;
	regex_t preg;
	gboolean stop;
	gboolean has_back_reference;
	gboolean stop_on_success;
} E2_Alias;

typedef struct _E2_AliasRuntime
{
	GList *aliases;
	E2_OptionSet *set;
	gboolean sync;
} E2_AliasRuntime;

gchar *e2_alias_apply (gchar *str, E2_AliasRuntime *rt) G_GNUC_MALLOC;
void e2_alias_sync (E2_AliasRuntime *rt);
void e2_alias_init (void);
void e2_alias_clean (void);

#endif //ndef __E2_ALIAS_H__
