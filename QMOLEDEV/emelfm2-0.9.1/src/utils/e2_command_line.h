/* $Id: e2_command_line.h 2743 2013-09-19 22:29:00Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/command/e2_command_line.h
@brief command line header

This is the header file for the command line functions.
*/

#ifndef __E2_COMMAND_LINE_H__
#define __E2_COMMAND_LINE_H__

#include "emelfm2.h"
#include "e2_pane.h"

typedef enum
{
	E2_COMMAND_LINE_ORIGINAL = 1<<0,
	E2_COMMAND_LINE_DIR_LINE = 1<<1
} E2_CommandLineFlags;

typedef struct _E2_CommandLineRuntime
{
	gchar *name;  //unique private name for cache-matching etc
	GtkWidget *combo;
	GtkTreeModel *model; //model for combo's history
	GList *history; //list of strings, each a member of model, used for cacheing
	E2_PaneRuntime *pane; //relevant only for dir lines, data for the corresponding pane
//	E2_ToolbarRuntime *bar; //for finding where the commandline is placed
	E2_OptionSet *opt_history_last;
	gboolean original;  //TRUE for command line(s), FALSE for dir lines
} E2_CommandLineRuntime;

gboolean e2_command_line_insert (const gchar *newtext);
void e2_command_line_register_keybindings (E2_CommandLineRuntime *rt);
#ifdef E2_MOUSECUSTOM
void e2_command_line_register_pointerbindings (E2_CommandLineRuntime *rt);
#endif
E2_CommandLineRuntime *e2_command_line_create (gboolean commands,
	E2_PaneRuntime *pane_rt) G_GNUC_MALLOC;
void e2_command_line_clean_all (void);
void e2_command_line_change_dir (gchar *utfpath, E2_PaneRuntime *rt);
void e2_command_line_highlight (GtkWidget *entry, gboolean on);
void e2_command_line_update_highlight (GtkWidget *entry, const gchar *newtext);
void e2_command_line_actions_register (void);
void e2_command_line_options_register (void);

#endif // ndef __E2_COMMAND_LINE_H__
