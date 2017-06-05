/* $Id: e2_cl_option.h 3050 2014-02-08 21:58:02Z tpgww $

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

#ifndef __E2_CL_OPTION_H__
#define __E2_CL_OPTION_H__

#include "emelfm2.h"

typedef struct _E2_CommandLineOptions
{
	gchar *pane1_path;	//absolute path string, converted before use to UTF-8 if needed
	gchar *pane2_path;	//ditto
	gchar *config_dir;	//UTF-8 string, no trailing /
	gchar *sharedconfig_dir;	//UTF-8 string, no trailing /
	gchar *trash_dir;	//UTF-8 string, no trailing /, typically trailing "Trash"
	gchar *encoding;
	gchar *fallback_encoding;
#ifdef DEBUG_MESSAGES
	gint debug_level;
#endif
	gboolean force_path; //use pane2_path, without -2
	gboolean detached;
	gboolean original;
	gboolean verbose;
	gboolean suppress_gtk_log;
	gboolean ignore_problems;
	gboolean session_user;
	GList *option_overrides; //not GSList
	GSList *startup_commands;
} E2_CommandLineOptions;

E2_CommandLineOptions e2_cl_options; //FIXME cleanup when session ends ?

void e2_cl_option_process (gint argc, gchar *argv[]);
void e2_cl_option_clear (void);

#endif //ndef __E2_CL_OPTION_H__
