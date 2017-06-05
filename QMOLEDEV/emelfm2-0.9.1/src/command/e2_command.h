/* $Id: e2_command.h 2964 2013-11-19 11:01:13Z tpgww $

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

#ifndef __E2_COMMAND_H__
#define __E2_COMMAND_H__

#include "emelfm2.h"
#include "e2_output.h"

//enable queueing of entered commands
//#define E2_COMMANDQ

//flags governing the way a command is interpreted and run
typedef enum
{
	E2_COMMAND_RANGE_DEFAULT = 1,
	E2_COMMAND_RANGE_FILE_ACTION = 1 << 1,
	E2_COMMAND_RANGE_FILE_OPERATION = 1 << 2,	//add names of selected items
	E2_COMMAND_RANGE_SUBSHELL = 1 << 3,
	E2_COMMAND_RANGE_TOCHILD = 1 << 4,	//send string to last-started child
	E2_COMMAND_RANGE_CUSTOM = 1 << 5,
#ifdef E2_SU_ACTIONS
	E2_COMMAND_RANGE_USER = 1 << 6, //change to 'alternate' user before command
	E2_COMMAND_RANGE_NOTUSER = 1 << 7 //revert alternate user after command
#endif
} E2_CommandRange;

#ifdef E2_SU_ACTIONS
//no. of places to << or >> the id-number embedded in E2_CommandRange flags and E2_RunFlags
# define E2_COMMAND_RANGE_SHIFT 16
#endif

typedef struct _E2_CommandTaskData
{
	VOLATILE gchar *command;	//command string
	VOLATILE gchar *currdir;	//the CWD when command is initiated (UTF-8)
#ifdef E2_COMMANDQ
	//data to allow later interpretation (for %D, %f, %F etc) and execution
	VOLATILE gchar *othrdir;	//the inactive pane dir when command is initiated (UTF-8)
# ifdef E2_VFS
	VOLATILE PlaceInfo *currspace;
	VOLATILE PlaceInfo *othrspace;
# endif
	VOLATILE GPtrArray *names;	//selected items array
	VOLATILE GPtrArray *othernames;	//selected items array
	VOLATILE E2_CommandRange range;
#endif
#ifdef E2_NEW_COMMAND
	VOLATILE gint child_stdin_fd;	//pipe file descriptors, parent-side
	VOLATILE gint child_stdout_fd;
	VOLATILE gint child_stderr_fd;
#else
	VOLATILE GIOChannel *to_child;	//channel used for messages to an async child (= child's stdin)
#endif
	VOLATILE gint exit;	//command exit code, 0 = success
} E2_CommandTaskData;

void e2_command_block_childsignal (void);
void e2_command_unblock_childsignal (void);
gboolean e2_command_clear_pending (gpointer from, E2_ActionRuntime *art);
gboolean e2_command_find_process (guint pid);
gint e2_command_run_at (gchar *command, const gchar *workdir,
	E2_CommandRange range, gpointer from);
//gint e2_command_run (gchar *command, E2_CommandRange range);
#define e2_command_run(c,r,f) e2_command_run_at(c,NULL,r,f)
guint e2_command_count_running_tasks (gboolean countcmds, gboolean countpaused);
void e2_command_retab_children (E2_OutputTabRuntime *currenttab,
	E2_OutputTabRuntime *replacetab);
void e2_command_retab2_children (E2_OutputTabRuntime *currenttab,
	E2_OutputTabRuntime *replacetab);
gboolean e2_command_kill_child (guint pid);
const gchar *e2_command_get_variable_value (gchar *var_name, const gchar **tailstore);
void e2_command_output_help (void);
void e2_command_actions_register (void);
void e2_command_options_register (void);

#endif //ndef __E2_COMMAND_H__
