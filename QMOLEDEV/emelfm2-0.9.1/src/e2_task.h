/* $Id: e2_task.h 3042 2014-02-08 07:08:20Z tpgww $

Copyright (C) 2003-2011 tooar <tooar@emelfm2.net>

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

#ifndef __E2_TASK_H__
#define __E2_TASK_H__

#include "emelfm2.h"
#include "e2_permissions_dialog.h"

#ifndef ALLPERMS
#define ALLPERMS (S_ISUID|S_ISGID|S_ISVTX|S_IRWXU|S_IRWXG|S_IRWXO)
#endif

//flags for special treatment of copy (and move, link?) actions
typedef enum
{
	E2_FTM_NORMAL   = 0,	  //this must have no bits set
	E2_FTM_BACKUP   = 1,      //backup overwritten items
	E2_FTM_TRASH    = 1 << 1, //trash overwritten items
	E2_FTM_MERGE    = 1 << 2, //merge (not replace) the contents of processed directories
	E2_FTM_RENAME   = 1 << 3, //copy-as UNUSED
	E2_FTM_CHECK    = 1 << 4, //perform backend overwrite checking (used when merging)
	E2_FTM_PAUSABLE = 1 << 5, //UNUSED check for pause requests during file copy
	E2_FTM_SAMETIME = 1 << 8, //preserve times of processed items
	E2_FTM_SAMEACL  = 1 << 9, //preserve ACL's of processed items
} E2_FileTaskMode;

//code to facilitate undoing (not implemented yet)
typedef enum
{
	E2_TASK_COPY,
	E2_TASK_COPYAS,
	E2_TASK_MOVE,
	E2_TASK_MOVEAS,
	E2_TASK_LINK,
	E2_TASK_LINKAS,
	E2_TASK_DELETE,
	E2_TASK_RENAME,
	E2_TASK_TRASH,
	E2_TASK_TRASHEMPTY,
	E2_TASK_CHMOD,
	E2_TASK_CHOWN,
	E2_TASK_INFO,
	E2_TASK_VIEW,
	E2_TASK_EDIT,
	E2_TASK_OPEN,
	E2_TASK_OPENWITH,
	E2_TASK_DROP,
	E2_TASK_MKDIR,
	//plugin tasks
	E2_TASK_CLONE,
	E2_TASK_TIMESET,
	E2_TASK_PACK,
	E2_TASK_UNPACK,
	E2_TASK_FOREACH,
	E2_TASK_CHACL,
	E2_TASK_CRYPT,
	E2_TASK_LAST	//this is just a code for other plugins etc
} E2_TaskType;

//the order here matters - some code checks for > E2_TASK_RUNNING
//or > E2_TASK_COMPLETED or <= E2_TASK_QUEUED
typedef enum
{
	E2_TASK_NONE,	//not yet specified
	E2_TASK_QUEUED,	//waiting to be processed
	E2_TASK_PAUSED, //a running action is paused, eg while waiting for user input
	E2_TASK_RUNNING,	//task function has been called
	E2_TASK_COMPLETED,	//task function has returned (any completion status)
	E2_TASK_INCOMPLETE,	//the task has timed out
	E2_TASK_ABORTED,	//the task was stopped by the user
	E2_TASK_DONE,	//UNUSED
	E2_TASK_FAILED	//command execution failed for some reason
} E2_TaskStatus;

typedef enum
{
	E2_TASKTYPE_ASYNC,
	E2_TASKTYPE_SYNC,
	E2_TASKTYPE_ACTION
} E2_TaskDataType;

typedef struct _E2_ActionTaskData E2_ActionTaskData;

struct _E2_ActionTaskData
{
	VOLATILE E2_TaskType tasktype;	//what type of task
	VOLATILE gboolean result;	//result from the function performing the task
	VOLATILE gchar *currdir;	//the active dir when action initiated, localised string with trailer
	VOLATILE gchar *othrdir;	//the inactive dir when action initiated, localised string with trailer
#ifdef E2_VFS
	VOLATILE PlaceInfo *currspace;	//pointers to space data, -1 if unknown
	VOLATILE PlaceInfo *othrspace;
#endif
	VOLATILE GPtrArray *names;	//selected items array
	VOLATILE E2_Action *action;	//action (hence, name and setup data)
	VOLATILE E2_ActionState state;
	VOLATILE gpointer rt_data;	//action runtime data (usually string or NULL, maybe flags)
	VOLATILE GtkWidget *initiator;	//the widget which was activated to initiate the task
	VOLATILE gboolean (*taskfunc) (E2_ActionTaskData*);	//the function to perform the task
	VOLATILE void (*callback) ();	//the function to call upon completion (maybe NULL)
	//to prevent races, only 1 actioner and 1 monitor at any time - hence static ID's
//	VOLATILE pthread_t athreadID;	//id of the action-thread assigned to run the task
//	VOLATILE pthread_t mthreadID;	//id of the monitor-thread assigned to run the task
	VOLATILE time_t timelimit;	//if not finished by this, abort the task
	VOLATILE E2_TaskStatus *status;	//pointer to status in E2_TaskRuntime (for changes)
};

typedef enum _E2_RunFlags
{
	E2_RUN_EXT   = 1,		//whether to run task in external shell
	E2_RUN_SYNC  = 1 << 1,	//whether to run task synchronously (nothing else until done)
	E2_RUN_SHOW  = 1 << 2,	//whether to print task and its exit status in the output pane
#ifdef E2_SU_ACTIONS
	E2_RUN_AS    = 1 << 3,	//whether to change to alternate user before running
	E2_RUN_NOTAS = 1 << 4,	//whether to revert from alternate user after running
#endif
} E2_RunFlags;

//task-queue data for commands and actions
typedef struct _E2_TaskRuntime
{
	VOLATILE gboolean action;	//TRUE when this data applies to an internal command
	VOLATILE glong pid;	//id assigned to the child (process or thread, real or fake, unsigned for a thread)
	VOLATILE gchar *pidstr;	//string form of pid (any leading "-" replaced by "a" or "s")
	//ordinal enumerator of the task's position in the (possibly-queued) execution sequence
	VOLATILE E2_RunFlags flags;
	VOLATILE E2_TaskStatus status;
	union _E2_TaskTypeExtra
	{
		E2_CommandTaskData command;	//for a command
		E2_ActionTaskData action;	//for an action
	} ex;

	//=background_tab or &app.tab if this command is currently running in the focused tab
	VOLATILE E2_OutputTabRuntime *current_tab;
	//the output data to use for this command when running in a non-focused tab
	VOLATILE E2_OutputTabRuntime *background_tab;
	VOLATILE GtkWidget *dialog;	//timeout dialog, or NULL;
} E2_TaskRuntime;

typedef struct _E2_DirEnt
{
	gchar *path;
	mode_t mode;
	time_t modtime;
	time_t axstime;
} E2_DirEnt;

typedef struct _E2_CopyData
{
#ifdef E2_VFS
	GError **operr;
#endif
	E2_FileTaskMode taskmode;	//flags to guide the features of the copying
	gboolean continued_after_problem;	//TRUE when a problem is found, but the walk is not aborted
	gint oldroot_len;	//length of source item path, less its last element
	gchar *newroot;	//destination item path, less its last element, no trailing /, localised string
#ifdef E2_VFS
	PlaceInfo *destspace;
#endif
	GList *dirdata;	//list of directories copied
} E2_CopyData;

typedef struct _E2_ChmodData
{
#ifdef E2_VFS
	GError **operr;
#endif
	gboolean continued_after_problem;
	E2_RecurseType scope;
	mode_t setmask;
	mode_t clearmask;
	GList *dirdata;
} E2_ChmodData;

typedef struct _E2_ChownData
{
#ifdef E2_VFS
	GError **operr;
#endif
	gboolean continued_after_problem;
	uid_t new_uid;
	gid_t new_gid;
	GList *dirdata;
} E2_ChownData;

const gchar *e2_task_find_prior_command (gint backcount, gchar *begins);
E2_TaskRuntime *e2_task_find_running_task (glong pid);
E2_TaskRuntime *e2_task_find_last_running_child (gboolean anytab);
E2_TaskRuntime *e2_task_set_data (glong pid, E2_TaskDataType mode, gchar *command);

void e2_task_cleanup (gboolean stay, pthread_t mainID);

gboolean e2_task_run_task (E2_TaskType type, E2_ActionRuntime *art,
	gpointer from, gboolean (*taskfunc) (E2_ActionTaskData *),
	void (*callback) (), gboolean immediate);
#define e2_task_do_task(type,art,from,taskfunc,callback) \
	e2_task_run_task (type, art, from, taskfunc, callback, TRUE)
#define e2_task_enqueue_task(type,art,from,taskfunc,callback) \
	e2_task_run_task (type, art, from, taskfunc, callback, FALSE)

gboolean e2_task_run_task_custom (E2_TaskType type, E2_ActionRuntime *art,
	gpointer from, gboolean (*taskfunc) (E2_ActionTaskData *),
	void (*callback) (), gchar *srcdir, gchar *destdir, GPtrArray *names,
	gboolean immediate);

void e2_task_refresh_lists (E2_ActionTaskData *qed);
void e2_task_advise (void);

gchar *e2_task_tempname (const gchar *path);
gchar *e2_task_system_command (const gchar *command, const gchar *filepath);
gboolean e2_task_drop (E2_TaskType type, const gchar *destdir, gchar * const *uris);
gboolean e2_task_backend_copy (VPATH *src, VPATH *dest, E2_FileTaskMode mode);
gboolean e2_task_backend_move (VPATH *src, VPATH *dest);
gboolean e2_task_backend_link (VPATH *target, VPATH *name);
gboolean e2_task_backend_rename (VPATH *oldsrc, VPATH*newsrc);
gboolean e2_task_backend_delete (VPATH *localpath);
gboolean e2_task_backend_chmod (VPATH *localpath, E2_ChmodType optype,
	mode_t newmode, E2_RecurseType recurse);
gboolean e2_task_backend_chown (VPATH *localpath, uid_t owner_id, gid_t group_id,
	gboolean recurse);
gboolean e2_task_backend_open (VPATH *localpath, gboolean ask);
gboolean e2_task_backend_view (VPATH *localpath);
void e2_task_actions_register (void);

#ifdef E2_SU_ACTIONS
gboolean e2_task_become_user (E2_RunFlags flags, gpointer *envp);
gboolean e2_task_revert_user (E2_RunFlags flags);
#endif


#endif //ndef __E2_TASK_H__
