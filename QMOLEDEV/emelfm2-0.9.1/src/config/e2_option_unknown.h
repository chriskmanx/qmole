/* $Id: e2_option_unknown.h 2064 2010-03-12 13:15:36Z tpgww $

Copyright (C) 2004-2009 tooar <tooar@emelfm2.net>

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

#ifndef __E2_OPTION_UNKNOWN_H__
#define __E2_OPTION_UNKNOWN_H__

#include "emelfm2.h"

void e2_option_unknown_record (gchar *option, gchar *value);
void e2_option_transient_value_get (E2_OptionSet *set);

#endif // ndef __E2_OPTION_UNKNOWN_H__
