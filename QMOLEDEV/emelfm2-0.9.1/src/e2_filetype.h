/* $Id: e2_filetype.h 2743 2013-09-19 22:29:00Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

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

#ifndef __E2_FILETYPE_H__
#define __E2_FILETYPE_H__

#include "emelfm2.h"

void e2_filetype_config_show (gchar *extension);
void e2_filetype_apply_allnew (void);
const gchar **e2_filetype_get_actions (gchar *ext);
//gchar **e2_filetype_get_extensions (gchar *category);
gchar *e2_filetype_get_default_action (gchar *ext) G_GNUC_MALLOC;
// UNUSED gchar **e2_filetype_get (gchar *ext);
// OLD void add_filetype(gchar *ext, gchar *prog, gchar *desc);
void e2_filetype_add_all (void);
void e2_filetype_exec_action (const gchar *action);
void e2_filetype_actions_register (void);
void e2_filetype_options_register (void);

#endif //ndef __E2_FILETYPE_H__
