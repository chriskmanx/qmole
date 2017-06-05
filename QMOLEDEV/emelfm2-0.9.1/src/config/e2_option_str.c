/* $Id: e2_option_str.c 2903 2013-10-31 22:32:24Z tpgww $

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
#include <string.h>
#include "e2_option.h"
#include "e2_option_str.h"

E2_OptionSet *e2_option_str_register (gchar *name, gchar *group, gchar *desc,
	gchar *tip, gchar *depends, gchar *value, E2_OptionFlags flags)
{
	E2_OptionSet *set = e2_option_register (E2_OPTION_TYPE_STR, name, group,
		desc, tip, depends, flags);
	set->ival = -1;
	if (value == NULL)
		value = "";
	set->sval = g_strdup (value);
	return set;
}

E2_OptionSet *e2_option_font_register (gchar *name, gchar *group, gchar *desc,
	gchar *tip, gchar *depends, gchar *def, E2_OptionFlags flags)
{
	E2_OptionSet *set = e2_option_str_register (name, group, desc, tip,
		depends, def, flags);
	set->type = E2_OPTION_TYPE_FONT;
	return set;
}

gchar *e2_option_str_get (gchar *option)
{
	E2_OptionSet *set = e2_option_get (option);
	return set->sval;
}

gchar *e2_option_str_get_direct (E2_OptionSet *set)
{
	return set->sval;
}

void e2_option_str_set_direct (E2_OptionSet *set, gchar *value)
{
	g_free (set->sval);
	if (value == NULL)
		value = "";
	set->sval = g_strdup (value);
}
