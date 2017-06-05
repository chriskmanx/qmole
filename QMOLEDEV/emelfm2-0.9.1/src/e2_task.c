/* $Id: e2_task.c 3042 2014-02-08 07:08:20Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>

This file is part of emelFM2, which is free software. You can redistribute it
and/or modify it under the terms of the GNU General Public License as published
by the Free Software Foundation - either version 3, or (at your option) any
later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/* TODO 25/10/04
some functions do not need TreeRowReferences
 (as of now, none do - no on-the-fly store content changes)
*/

#include "e2_task.h"
#include <string.h>
#include <pthread.h>
#include <signal.h>
#include "e2_dialog.h"
#include "e2_option.h"
#include "e2_ownership_dialog.h"
#include "e2_filelist.h"
#include "e2_filetype.h"
#include "e2_context_menu.h"
#ifdef E2_SU_ACTIONS
# include "../plugins/optional/e2p_privilege.h"
#endif

//a thread id that will not be assigned by the system
#define UNUSED_THREADID -1

GList *open_history = NULL;  //history list for open-with dialog

pthread_mutex_t task_mutex = PTHREAD_MUTEX_INITIALIZER; //PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP not needed
static pthread_t athreadID = 0;	//action thread ID, 0 when stopped
static pthread_t mthreadID = 0;	//monitor thread ID, 0 when stopped

static gpointer _e2_task_progress_monitor (E2_TaskRuntime *rt);
static gboolean __e2_task_move (E2_ActionTaskData *qed, gchar *trashpath);
static gboolean _e2_task_copyQ (E2_ActionTaskData *qed);
static gboolean _e2_task_copy_asQ (E2_ActionTaskData *qed);
#ifdef WITH_UNTRASH
static gboolean _e2_task_untrashQ (E2_ActionTaskData *qed);
#endif
static gboolean _e2_task_trashitQ (E2_ActionTaskData *qed);
static gboolean _e2_task_moveQ (E2_ActionTaskData *qed);
static gboolean _e2_task_move_asQ (E2_ActionTaskData *qed);
static gboolean _e2_task_symlinkQ (E2_ActionTaskData *qed);
static gboolean _e2_task_symlink_asQ (E2_ActionTaskData *qed);
static gboolean _e2_task_deleteQ (E2_ActionTaskData *qed);
static gboolean _e2_task_renameQ (E2_ActionTaskData *qed);
static gboolean _e2_task_permissionsQ (E2_ActionTaskData *qed);
static gboolean _e2_task_ownershipQ (E2_ActionTaskData *qed);
static gboolean _e2_task_file_infoQ (E2_ActionTaskData *qed);
static gboolean _e2_task_viewQ (E2_ActionTaskData *qed);
static gboolean _e2_task_editQ (E2_ActionTaskData *qed);
static gboolean _e2_task_openQ (E2_ActionTaskData *qed);
static gboolean _e2_task_open_withQ (E2_ActionTaskData *qed);
//CHECKME need this ?static gboolean _e2_task_open_inotherQ (E2_ActionTaskData *qed);

#ifdef E2_INCLIST
/**
@brief remove a row from active-pane filelist

@param ref pointer to reference in current view

@return
*/
static void _e2_task_treeview_line_remove (GtkTreeRowReference *ref)
{
	GtkTreeIter iter;
	GtkTreePath *path = gtk_tree_row_reference_get_path (ref);
	if (path != NULL)
	{
		if (gtk_tree_model_get_iter (curr_view->model, &iter, path))
			gtk_list_store_remove (curr_view->store, &iter);
		gtk_tree_path_free (path);
	}
}
/**
@brief change the itemname for a row in active-pane filelist

@param ref pointer to reference in current view
@param newname utf-8 string with replacement name for an item

@return
*/
static void _e2_task_treeview_line_rename (GtkTreeRowReference *ref, gchar *newname)
{
	gchar *savename;
	gchar *local = F_FILENAME_TO_LOCALE (newname);
	if (e2_fs_is_dir3 (local E2_ERR_NONE()))
		savename = g_strconcat (newname, G_DIR_SEPARATOR_S, NULL);
	else
		savename = newname;
	F_FREE (local, newname);

	GtkTreeIter iter;
	GtkTreePath *path = gtk_tree_row_reference_get_path (ref);
	if (path != NULL)
	{
		if (gtk_tree_model_get_iter (curr_view->model, &iter, path))
			gtk_list_store_set (curr_view->store, &iter, FILENAME, savename, -1);
		gtk_tree_path_free (path);
	}
	if (savename != newname)
		g_free (savename);
}
//FIXME add fn to change permissions field in a row
#endif	//def E2_INCLIST

  /***********************/
 /** support functions **/
/***********************/

/**
@brief if @a command includes "<system default>", substitute for that if possible

@param command the string to be processed
@param filepath localised absolute path of item for which the default 'processor' is to be determined

@return newly-allocated (possibly bogus) command string, or NULL if no substitution is needed
*/
gchar *e2_task_system_command (const gchar *command, const gchar *filepath)
{
	gchar *ret = NULL;
	const gchar *s, *token = _("<system default>");
	if ((s = strstr (command, token)) != NULL)
	{
		//hack to get default action using existing code
		GtkWidget *dummy = gtk_menu_new ();
		e2_menu_add_filehandlers (dummy, filepath);
		GList *items = gtk_container_get_children (GTK_CONTAINER (dummy));
		if (items != NULL)
		{
			//1st menu-item assumed to represent the default
			ret = (gchar*) g_object_get_data (G_OBJECT(items->data), "action-cmd-key");
			g_list_free (items);
			if (ret != NULL)
			{
				if (s > command)
				{
					gchar *t = g_strndup (command, s-command);
					ret = g_strconcat (t, ret, s + strlen (token), NULL);
					g_free (t);
				}
				else
					ret = e2_utils_strcat (ret, command + strlen (token));
			}
		}
		gtk_widget_destroy (dummy);
		if (ret == NULL)
			//provoke an error message
			ret = g_strdup ("\"default command for %f\""); //don't translate (yet?)
	}
	return ret;
}
/**
@brief find a previous command
@param backcount value to get the nth previous command
param begins string at start of desired command, or NULL to use @a backcount

@return command string or NULL if no match was found
*/
const gchar *e2_task_find_prior_command (gint backcount, gchar *begins)
{
	GList *member;
	gint count = 0;

	pthread_mutex_lock (&task_mutex);
	member = g_list_last (app.taskhistory);
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		E2_TaskRuntime *rt = (E2_TaskRuntime *) member->data;
		if (!rt->action)
		{
			const gchar *cmdstring = rt->ex.command.command;
			if (begins != NULL)
			{
				if (g_str_has_prefix (cmdstring, begins))
					return cmdstring;
			}
			else
			{
				if (++count == backcount)
					return cmdstring;
			}
		}
		pthread_mutex_lock (&task_mutex);
		member = member->prev;
		pthread_mutex_unlock (&task_mutex);
	}
	return NULL;
}
/**
@brief find the data for a running or paused task with pid @a id
This does not distinguish between commands and actions
@param pid the id of the process we're looking for
@return the data struct for the task, or NULL if not found
*/
E2_TaskRuntime *e2_task_find_running_task (glong pid)
{
	E2_TaskRuntime *rt;
	GList *member;
	pthread_mutex_lock (&task_mutex);
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	while (member != NULL)
	{
		rt = (E2_TaskRuntime *)member->data;
		if (rt != NULL && rt->pid == pid &&
			(rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED))
			return rt;
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
	return NULL;
}
/**
@brief find the pid of the last-started child process that's still running
This doesn't distinguish between commands and actions
@param anytab TRUE to find a match in any tab, FALSE in the currently-displayed tab
@return pointer to task data or NULL if not found
*/
E2_TaskRuntime *e2_task_find_last_running_child (gboolean anytab)
{
	E2_TaskRuntime *rt;
	GList *member;
	pthread_mutex_lock (&task_mutex);
	member = g_list_last (app.taskhistory);
	pthread_mutex_unlock (&task_mutex);
	//backwards scan, don't bother locking at end of loop
	for (; member != NULL; member = member->prev)
	{
		rt = (E2_TaskRuntime *)member->data;
		if (rt != NULL && rt->status == E2_TASK_RUNNING && !rt->action
			&& (anytab || rt->current_tab == &app.tab))
			return rt;
	}
	return NULL;
}
/**
@brief setup task data for a command or action
This is called for a command just commenced (async) or about to commence (sync)
or for a file-action about to be commenced or queued.
Task data is added to the task Q if not already there, and initialised as
appropriate for @a mode.
@a mode = one of:
 E2_TASKTYPE_ASYNC when called from _e2_command_run_async()
 E2_TASKTYPE_SYNC when called from _e2_command_run_sync()
 E2_TASKTYPE_ACTION when called from e2_task_enqueue_task()

@param pid the id of the process we're starting (0 if not yet known)
@param mode task type enumerator
@param command executed command string or NULL for an action
@return pointer to command data struct, or NULL if this fn is busy, or 1 on error
*/
E2_TaskRuntime *e2_task_set_data (glong pid, E2_TaskDataType mode, gchar *command)
{
	static gint blocked = 0;

	if (!g_atomic_int_compare_and_exchange (&blocked, 0, 1))
		return NULL;

	E2_TaskRuntime *rt = e2_task_find_running_task (pid);
	if (rt == NULL)
	{
		printd (DEBUG, "e2_task_set_data (pid:%d,mode:%d,command:%s)",
			pid, mode, command);
		rt = ALLOCATE (E2_TaskRuntime);	//FIXME never deallocated
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (rt, FIXME;)
#else
		if (rt == NULL)
		{
			printd (WARN, "not enough memory for task history data");
			g_atomic_int_set (&blocked, 0);
			return GINT_TO_POINTER (1);
		}
#endif
		if (mode == E2_TASKTYPE_ASYNC || mode == E2_TASKTYPE_SYNC)
		{
			rt->action = FALSE;
#ifndef E2_NEW_COMMAND
			rt->pid = pid;
#endif
			rt->ex.command.command = g_strdup (command);
//E2_VFSTMPOK
			rt->ex.command.currdir = g_strdup (curr_view->dir);
#ifdef E2_COMMANDQ
			rt->ex.command.othrdir = g_strdup (other_view->dir);
# ifdef E2_VFS
			rt->ex.command.currspace = curr_view->spacedata;
			rt->ex.command.othrspace = other_view->spacedata;
# endif
			rt->ex.command.names = ;	//active pane selected items array
			rt->ex.command.othernames = ;	//inactive pane selected items array
//			rt->ex.command.range = SET EXTERNALLY;
#endif
#ifndef E2_NEW_COMMAND
			if (mode == E2_TASKTYPE_ASYNC)
			{
				rt->pidstr = g_strdup_printf ("%ld", rt->pid);
			}
			else	//(mode == E2_TASKTYPE_SYNC)
			{
				//pid will be -ve, a fake assigned by e2, not the os, as we never
				//get the real one, nor ever get opportunity to kill the sync process
				rt->pidstr = g_strdup_printf ("s%ld", -pid);
			}
			rt->status = E2_TASK_RUNNING;
#endif
		}
		else if (mode == E2_TASKTYPE_ACTION)
		{
			rt->action = TRUE;
			rt->status = E2_TASK_NONE;	//prevent immediate Q processing
			//setup for on-the-fly status changes coz often just rt->ex.action is known
			rt->ex.action.status = &rt->status;
		}
		else
		{
			printd (WARN, "tried to register a process with bad mode flag");
			DEALLOCATE (E2_TaskRuntime, rt);
			g_atomic_int_set (&blocked, 0);
			return GINT_TO_POINTER (1);
		}

		//setup for foreground running
		rt->current_tab = &app.tab;
		rt->background_tab = curr_tab;

		rt->dialog = NULL;	//no dialog to cleanup yet

		pthread_mutex_lock (&task_mutex);
		app.taskhistory = g_list_append (app.taskhistory, rt);
		pthread_mutex_unlock (&task_mutex);
	}
	else
	{	//found an entry with matching pid
		printd (DEBUG, "tried to re-register a process");
		rt = GINT_TO_POINTER (1);
	}

	g_atomic_int_set (&blocked, 0);
	return rt;
}
#ifdef E2_SU_ACTIONS
/**
@brief turn on an alternate user, if relevant
@param flags task-handler flags
@param envp store for environment vars array, or NULL if environment not wanted
@return TRUE if change succeeded or not needed
*/
gboolean e2_task_become_user (E2_RunFlags flags, gpointer *envp)
{
	if (flags & E2_RUN_AS)
	{
		PrivilegeIface *iface = PRIVILEGE_IFACE (
			e2_plugins_get_plugin ("privilege" VERSION, FALSE));
		if (iface != NULL)
		{
			//get context-id
			guint8 cxtid = (guint8) flags >> E2_COMMAND_RANGE_SHIFT;
			gboolean success = (*iface->context_change) (cxtid);
			if (envp != NULL)
				*envp = (*iface->get_environment) (cxtid);
			return success;
		}
		return FALSE;
	}
	return TRUE;
}
/**
@brief turn off the alternate user, if relevant
@param flags task-handler flags
@return TRUE if reversion succeeded or not needed
*/
gboolean e2_task_revert_user (E2_RunFlags flags)
{
	if (flags & E2_RUN_NOTAS)
	{
		PrivilegeIface *iface = PRIVILEGE_IFACE (
			e2_plugins_get_plugin ("privilege" VERSION, FALSE));
		if (iface != NULL)
		{
			//get context-id
			guint8 cxtid = (guint8) flags >> E2_COMMAND_RANGE_SHIFT;
			return (*iface->context_revert) (cxtid);
		}
		return FALSE;
	}
	return TRUE;
}
#endif //def E2_SU_ACTIONS
/**
@brief get name(s) of item(s) to process, from @a arg

@a arg comprises (notionally at least) one or more concatenated filenames
or filepaths, with or without quotes, to be processed.
Such string is converted into array form, with source path set from the
first item.

@param arg utf8 string (action rt data)
@param path store for source path (if any) retrieved from 1st item in @a arg

@return array of E2_SelectedItemInfo's, or NULL if nothing relevant is available
*/
static GPtrArray *_e2_task_get_names (gchar *arg, gchar **path)
{
	GPtrArray *names;
	E2_SelectedItemInfo *seldata;
	gint len;
	gchar *s, *name, *local;
	gchar *item = e2_utils_get_first_part (arg, FALSE);
	if (item == NULL)
		return NULL;

	//check first argument for a source path
	s = g_path_get_dirname (item);
	if (strcmp (s, "."))
	{
		if (!strcmp (s, G_DIR_SEPARATOR_S))
			*path = g_strdup (G_DIR_SEPARATOR_S);
		else
		{
			local = F_FILENAME_TO_LOCALE (s);
			*path = e2_utils_strcat (local, G_DIR_SEPARATOR_S);
			F_FREE (local, s);
		}
	}
	g_free (item);
	g_free (s);
	//now get names of all items
	names = g_ptr_array_new ();
	s = arg;
	while (s != NULL)
	{
		item = e2_utils_get_first_part (s, FALSE);
		if (item == NULL || *item == '\0') //if s has trailing whitespace, item may be empty
			break;
		seldata = ALLOCATE (E2_SelectedItemInfo);
		//CHECKME brackets inside macro
		CHECKALLOCATEDWARN (seldata, g_ptr_array_free (names, TRUE);g_free (item);return NULL;);
		name = g_path_get_basename (item);
		local = F_FILENAME_TO_LOCALE (name);
		//strip any trailer
		len = strlen (local) - sizeof(gchar);
		if (len > 0 && *(local + len) == G_DIR_SEPARATOR)
			*(local + len) = '\0';
		g_strlcpy (seldata->filename, local, sizeof (seldata->filename));
		g_ptr_array_add (names, seldata);
		g_free (name);
		F_FREE (local, name);
		//point after the current argument
		len = strlen (item);
		//CHECKME this might be not quite correct (preceding whitespace or quotes)
		s += len;
		s = e2_utils_find_whitespace (s);
		g_free (item);
	}

	return names;
}
/**
@brief save info for a trashed item
fdo trash spec requires adding to directory $trash/info an “information file”
for every file and directory in $trash/files.
This file must have exactly the same name as the file or directory in $trash/files,
plus the extension “.trashinfo”
The format of this file is like:
[Trash Info]
Path=foo/bar/meow.bow-wow
DeletionDate=20040831T22:32:08
The path string is localised (as used by the filesystem, with characters escaped
as in URLs (as defined by RFC 2396, section 2).
The date string is the date and time when the item was trashed, in
YYYY-MM-DDThh:mm:ss format (see RFC 3339). The time zone is the user's (or
filesystem's) local time.
@param src GString used in the move operation, reused here for convenience
@param dest GString used in the move operation, with path to ...Trash/files/savename

@return
*/
static void _e2_task_trash_info (const GString *src, const GString *dest)
{
	gchar time_buf[30];
	time_t now = time (NULL);
	struct tm *tm_ptr = localtime (&now);
	strftime (time_buf, sizeof(time_buf), "%Y-%m-%dT%H:%M:%S", tm_ptr);

	gchar *s = g_strescape (src->str, NULL);
	g_string_printf ((GString *)src, "[Trash Info]\nPath=%s\nDeletionDate=%s\n", s, time_buf);
	g_free (s);
	gchar *dir = g_path_get_dirname (dest->str);	//...Trash/files
	s = strrchr (dir, G_DIR_SEPARATOR);
	if (s != NULL && s > dir)
	{
		s++;	//keep the separator
		*s = '\0';
	}
	gchar *base = g_path_get_basename (dest->str);
	g_string_printf ((GString *)dest, "%sinfo"G_DIR_SEPARATOR_S"%s.trashinfo", dir, base);
#ifdef E2_VFS
	VPATH tdata = { dest->str, NULL };
#endif
	e2_fs_set_file_contents (
#ifdef E2_VFS
		&tdata,
#else
		dest->str,
#endif
		src->str, src->len, 0644 E2_ERR_NONE ());
	g_free (dir);
	g_free (base);
}
/**
@brief after successful rename, move, moveas or delete a trashed item, adjust any corresponding info file
@param src GString used in the operation, containing localised path
@param dest GString used in the operation, containing localised path, or NULL for move, delete

@return
*/
static void _e2_task_update_trash_info (const GString *src, const GString *dest)
{
	gchar *dir = g_path_get_dirname (src->str);	//can't use src->str for trash search, it's gone now
	gchar *tp = e2_utils_get_trash_path (dir, FALSE); //UTF-8 string, "Trash/" at end
	if (tp != NULL)
	{
		gboolean gone;
		if (dest == NULL)
			gone = TRUE;
		else
		{
			gchar *newdir = g_path_get_dirname (dest->str);
			gchar *files = g_build_filename (tp, "files", NULL);
			if (!strcmp (newdir, files))
			{	//move in
				gone = FALSE;	//warning prevention
				g_free (dir);
				g_free (tp);
				g_free (newdir);
				g_free (files);
				return _e2_task_trash_info (src, dest);
			}
			//move out
			gone = strcmp (dir, newdir);
			g_free (newdir);
			g_free (files);
		}
		gchar *local = F_FILENAME_TO_LOCALE (tp);
		if (g_str_has_prefix (src->str, local))
		{
			gchar *base = g_path_get_basename (src->str);
			g_string_printf ((GString *)src,
				"%sinfo"G_DIR_SEPARATOR_S"%s.trashinfo", local, base);
#ifdef E2_VFS
			VPATH tdata = { src->str, NULL };
#endif
			if (e2_fs_access (
#ifdef E2_VFS
				&tdata,
#else
				src->str,
#endif
				W_OK E2_ERR_NONE()) == 0)
			{
				if (gone)
					e2_task_backend_delete
#ifdef E2_VFS
						(&tdata);
#else
						(src->str);
#endif
				else
				{
					//rename it in trash
					g_free (base);
					base = g_path_get_basename (dest->str);
					g_string_printf ((GString *)dest,
						"%sinfo"G_DIR_SEPARATOR_S"%s.trashinfo", local, base);
#ifdef E2_VFS
					VPATH ddata = { dest->str, NULL };
					e2_task_backend_rename (&tdata, &ddata);
#else
					e2_task_backend_rename (src->str, dest->str);
#endif
				}
			}
			g_free (base);
		}
		g_free (tp);
		F_FREE (local, tp);
	}
	g_free (dir);
}
#ifdef WITH_UNTRASH
/**
@brief get original path of a trashed item from corresponding info file
@param src GString containing localised path of item in trash dir (contents not altered)
@param dest store for GString to hold detected localised path

@return TRUE if @a dest has been populated with accessible, writable path
*/
static gboolean _e2_task_get_trasheditem_path (const GString *src, GString **dest)
{
	gboolean retval = FALSE;
	gchar *tp = e2_utils_get_trash_path (src->str, FALSE); //UTF-8 string, "Trash/" at end
	if (tp != NULL)
	{
		gchar *local = F_FILENAME_TO_LOCALE (tp);
		if (g_str_has_prefix (src->str, local))
		{
			gchar *base = src->str + strlen (tp);
			g_string_printf (*dest,
				"%sinfo"G_DIR_SEPARATOR_S"%s.trashinfo", local, base);
#ifdef E2_VFS
			VPATH tdata = { NULL, (*dest)->str };
#endif
			if (e2_fs_access (
#ifdef E2_VFS
				&tdata,
#else
				(*dest)->str,
#endif
				R_OK E2_ERR_NONE()) == 0)
			{
				gchar *contents;
				if (e2_fs_get_file_contents (
#ifdef E2_VFS
					&tdata,
#else
					(*dest)->str,
#endif
					(gpointer *) &contents, NULL, TRUE E2_ERR_NONE()))
				{
					gchar *st = strstr (contents, "Path="); //no translation
					if (st != NULL)
					{
						st += 5 * sizeof (gchar);
						gchar *nd = strchr (st, '\n');
						if (nd != NULL)
							*nd = '\0';
						st = g_strcompress (st);
						if (st != NULL)
						{
							nd = g_path_get_dirname (st);
#ifdef E2_VFS
							tdata.path = nd;
#endif
							retval = (e2_fs_access (
#ifdef E2_VFS
							&tdata,
#else
							nd,
#endif
							W_OK | X_OK E2_ERR_NONE()) == 0);
							if (retval)
								*dest = g_string_assign (*dest, st);
							g_free (st);
							g_free (nd);
						}
					}
					g_free (contents);
				}
			}
		}
		g_free (tp);
		F_FREE (local, tp);
	}
	return retval;
}
#endif
/**
@brief task-Q thread cleanup locked mutex
@param mutex pointer to mutex to open
@return
*/
static void _e2_task_thread_tryunlock (pthread_mutex_t *mutex)
{
	//FIXME try
	pthread_mutex_unlock (mutex);
}
/**
@brief task-Q thread cleanup any monitoring
This expects BGL open/off
@param rt pointer to task data
@return
*/
static void _e2_task_thread_demonitor (E2_TaskRuntime *rt)
{
	//promptly signal completion to monitor thread
	if (mthreadID > 0)
	{
		pthread_mutex_lock (&task_mutex);
		if (rt->dialog != NULL)
		{
			CLOSEBGL
			gtk_widget_destroy (rt->dialog);
			OPENBGL
			rt->dialog = NULL;	//not really necessary
		}
		pthread_mutex_unlock (&task_mutex);

		pthread_cancel (mthreadID);
		printd (DEBUG,"monitor-thread (ID=%lu) stopped by action-thread", mthreadID);
		mthreadID = 0;
	}
	printd (DEBUG,"post-task monitor-thread cleanup by thread %u completed", pthread_self ());
}
/**
@brief task-Q thread post-action processing and cleanup
@param rt pointer to task data
@return
*/
static void _e2_task_thread_finish (E2_TaskRuntime *rt)
{
	//report as required (BGL open)
	E2_ActionTaskData *qed = &rt->ex.action;
	if (qed->callback != NULL && qed->callback != e2_task_refresh_lists)
	{
		void (*cb) (gboolean) = qed->callback;
		(*cb) (qed->result);
	}
	//setup refresh Q as required
	if (qed->callback == e2_task_refresh_lists)
	{
		e2_task_refresh_lists (qed);
	}

	printd (DEBUG, "In _e2_task_thread_finish: names array %x:", qed->names);
	//just 1 cleanup
	if (qed->names != NULL)
		e2_fileview_clean_selected (qed->names);
	printd (DEBUG,"taskQ-thread post-task operations done by %u", pthread_self());
}
/**
@brief thread function to process tasks-pending queue
The queue is scanned from its start, in turn processing each item with status
E2_TASK_QUEUED
When each task is finished, task-related data are (in general) not cleaned -
allowing a history. The selection names are cleared (lot of storage & not yet
used for history purposes)
Any completion-report function is called with BGL open/off !
@param data UNUSED ptr to data specifiedf when thread created

@return NULL
*/
static gpointer _e2_task_processQ (gpointer data)
{
	GList *member;

	e2_utils_block_thread_signals ();	//block all allowed signals to this thread

	pthread_cleanup_push ((gpointer)_e2_task_thread_tryunlock, &task_mutex);	//0
	pthread_mutex_lock (&task_mutex);
	//FIXME move the scan-start position to ignore completed items
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);
	pthread_cleanup_pop (0);

	while (member != NULL)
	{
//		printd (DEBUG, "_e2_task_processQ - check next item in list");
		E2_TaskRuntime *rt = (E2_TaskRuntime *)member->data;
		if (rt->action && rt->status == E2_TASK_QUEUED)
		{
			printd (DEBUG, "_e2_task_processQ - process queud item");
			if (e2_option_bool_get ("task-timeout-checks"))
			{
				//create progress monitor thread, providing a mechanism to abort
				//slow activities
//				qed->timelimit = time (NULL) + e2_option_int_get ("task-timeout-interval");
//				printd (DEBUG,"task period is %d seconds", data->timeout);
				pthread_attr_t attr;
				//we don't use glib thread funcs, as we may need to kill threads
				pthread_attr_init (&attr);
				pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);

				if (pthread_create (&mthreadID, &attr,
					(gpointer(*)(gpointer))_e2_task_progress_monitor, rt) == 0)
				{
					printd (DEBUG,"monitor-thread (ID=%lu) started", mthreadID);
					rt->dialog = NULL;
				}
				else
				{
					//FIXME message to user
					printd (WARN,"monitor-thread-create error!");
					mthreadID = 0;
				}
				pthread_attr_destroy (&attr);
			}
			else
				mthreadID = 0;
			//arrange to cleanup monitoring if aborted
			pthread_cleanup_push ((gpointer) _e2_task_thread_demonitor, rt);	//2

			/* this is a workaround for systems that run this thread before the
			   thread-creation function sets athreadID
			   but here we don't adjust athreadID yet */
			pthread_t thisID = pthread_self ();
			pthread_detach (thisID);	//cleanup at end of joinable thread

			rt->pid = (glong) thisID;	//all tasks in the Q will get same pid
			rt->pidstr = g_strdup_printf ("%lu", (gulong) thisID);

			E2_ActionTaskData *qed = &rt->ex.action;
			gboolean (*actionfunc) (E2_ActionTaskData *) = qed->taskfunc;

			e2_utf8_set_name_conversion_if_requested ();

			rt->status = E2_TASK_RUNNING;	//FIXME enable downstream status change for pauses
//			printd (DEBUG,"queued action starts");
			//arrange to do reporting and cleanup if aborted
			pthread_cleanup_push ((gpointer)_e2_task_thread_finish, rt);	//3
			//do it
			//typically, more cleanup push/pop downstream
			qed->result = (*actionfunc) (qed);
			rt->status = E2_TASK_COMPLETED;	//done if not aborted
			//if thread not aborted, cleanup monitoring first
			_e2_task_thread_demonitor (rt);
			_e2_task_thread_finish (rt);
			pthread_cleanup_pop (0);	//3
			pthread_cleanup_pop (0);	//2
		}
		pthread_cleanup_push ((gpointer)_e2_task_thread_tryunlock, &task_mutex);	//0
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
		pthread_cleanup_pop (0);	//0
	}
	pthread_cleanup_push ((gpointer)_e2_task_thread_tryunlock, &task_mutex);	//0
	pthread_mutex_lock (&task_mutex);
	if (athreadID == 0)	//we're finishing before the thread-starter has returned
		athreadID = UNUSED_THREADID;	//set quick-finish signal for the starter
	else
		athreadID = 0;	//just clear it
	pthread_mutex_unlock (&task_mutex);
	pthread_cleanup_pop (0);	//0
//	printd (DEBUG, "task-Q-process-thread finished");
	return NULL;
}
/**
@brief timer callback to initiate a queued-task thread with BGL definitely off/open

@param rt pointer to data for added task, or NULL when just restarting

@return FALSE to stop the timer
*/
static gboolean _e2_task_run_processQ (E2_TaskRuntime *rt)
{
	printd (DEBUG,"_e2_task_run_processQ timer callback, thread %u", pthread_self());
 /* this callback uses the main thread, so signals cannot be permanently
	blocked, but we don't want an ending child to interfere with the interplay
	between this func and the actual processor thread */
	e2_command_block_childsignal ();

	if (rt != NULL)
		rt->status = E2_TASK_QUEUED;	//enable processing by a running thread

	pthread_mutex_lock (&task_mutex);
	gboolean not_running = (athreadID == 0 || athreadID == UNUSED_THREADID);
	pthread_mutex_unlock (&task_mutex);
	if (not_running)
	{
		pthread_t thisID;
		printd (DEBUG,"_e2_task_run_processQ create Q thread");
		//create joinable thread, may need to join during a forced shutdown
		//NOTE in some systems the created thread will run and maybe even finish,
		//before this func returns !!
		if (pthread_create (&thisID, NULL, _e2_task_processQ, NULL) == 0)	//no data ?
		{
			pthread_mutex_lock (&task_mutex);
			if (athreadID == UNUSED_THREADID)	//the task thread has finished already
			{
				athreadID = 0;
				printd (DEBUG,"Q-process-thread (ID=%lu) finished immediately", (gulong) thisID);
			}
			else
			{
				athreadID = thisID;
				printd (DEBUG,"Q-process-thread (ID=%lu) started", (gulong) thisID);
			}
			pthread_mutex_unlock (&task_mutex);
		}
		else
		{
/*			CLOSEBGL
			e2_output_print_error (_("Cannot create thread to run task"), FALSE);
			OPENBGL
*/
			printd (WARN,"Q-process-thread creation error!");
		}
	}
	else if (rt != NULL)	//adding a task
	{
		printd (DEBUG, "_e2_task_run_processQ Q thread busy");
		//task start may be deferred, advise user
#ifdef E2_COMMANDQ
		gchar *s, *p;
		if (rt->action)
		{
			s = rt->ex.action.action->name;
			p = NULL;
		}
		else
		{
			s = rt->ex.command.command;
			p = e2_utils_find_whitespace (s);
			if (p != NULL)
				*p = '\0';
		}
#else
		gchar *s = rt->ex.action.action->name;
#endif
		gchar *msg = g_strdup_printf (_("%s added to tasks queue"), s);
		CLOSEBGL
		e2_output_print (&app.tab, msg, NULL, TRUE, "bold", "cmand", NULL);
		OPENBGL
		g_free (msg);
#ifdef E2_COMMANDQ
		if (p != NULL)
			*p = ' ';	//too bad if it was a tab before
#endif
		//if message not visible, make a noise
		if (app.window.output_paned_ratio > 0.99)
			e2_utils_beep ();
	}
#ifdef DEBUG_MESSAGES
	else
		printd (DEBUG, "_e2_task_run_processQ Q ignore NULL task data");
#endif
	e2_command_unblock_childsignal ();

	printd (DEBUG,"_e2_task_run_processQ timer callback FINISH");
	return FALSE;
}
/**
@brief thread function to process immediate task
Data are (in general) not cleaned when a task is finished - so we have a
history. The selection names are cleared (lot of storage & not yet used for
history purposes)
@param rt ptr to data struct for the task to be performed

@return NULL
*/
static gpointer _e2_task_processnow (E2_TaskRuntime *rt)
{
	e2_utils_block_thread_signals ();	//block all allowed signals to this thread

	rt->pid = (glong) pthread_self ();
	pthread_detach (rt->pid);	//cleanup at end of joinable thread

	rt->pidstr = g_strdup_printf ("%lu", (gulong)rt->pid);
	E2_ActionTaskData *qed = &rt->ex.action;
	gboolean (*actionfunc) (E2_ActionTaskData *) = qed->taskfunc;

	e2_utf8_set_name_conversion_if_requested ();

	rt->status = E2_TASK_RUNNING;
//	printd (DEBUG,"immediate action starts");
	//do it
	qed->result = (*actionfunc) (qed);
	rt->status = E2_TASK_COMPLETED;

	//report as required
	if (qed->callback != NULL && qed->callback != e2_task_refresh_lists)
	{
		void (*cb) (gboolean) = qed->callback;
		(*cb) (qed->result);
	}
	//setup refresh Q as required
	if (qed->callback == e2_task_refresh_lists)
	{
		e2_task_refresh_lists (qed);
	}
	//just 1 cleanup
	if (qed->names != NULL)
		e2_fileview_clean_selected (qed->names);

	printd (DEBUG,"immediate-action-thread (ID=%lu) finished", rt->pid);
	return NULL;
}
/**
@brief timer callback to initiate an immediate-task thread with BGL definitely off/open

@param rt pointer to task data struct

@return FALSE to stop the timer
*/
static gboolean _e2_task_run_processnow (E2_TaskRuntime *rt)
{
	//no glib thread funcs here, as we generally need the option to cancel thread(s)
	pthread_t threadID;
	//create joinable thread, may need to join during a forced shutdown
	if (pthread_create (&threadID, NULL,
		(gpointer(*)(gpointer))_e2_task_processnow, rt) == 0)
		printd (DEBUG,"immediate-action-thread (ID=%lu) started", (gulong) threadID);
	else
	{
/*		CLOSEBGL
		e2_output_print_error (_("Cannot create thread to run task"), FALSE);
		OPENBGL
*/
		printd (WARN,"action-thread creation error!");
	}

	return FALSE;
}
/**
@brief change status of some queued tasks to manipulate operation of Q thread
Pending tasks are [un]hidden from the Q processor by changing their status
@param enable TRUE to make pending tasks visible to Q-thread again, FALSE to hide Q's tasks

@return
*/
static void _e2_task_change_status (gboolean enable)
{
	GList *member;

	pthread_mutex_lock (&task_mutex);
	member = app.taskhistory;
	pthread_mutex_unlock (&task_mutex);

	while (member != NULL)
	{
		E2_TaskRuntime *rt = (E2_TaskRuntime *) member->data;
		if (enable)
		{
			if (rt->status == E2_TASK_NONE)
				rt->status = E2_TASK_QUEUED; //make this one ready for Q thread again
		}
		else
		{
			if (rt->status == E2_TASK_QUEUED)
				rt->status = E2_TASK_NONE;	//trick Q thread into ignoring this one
		}
		pthread_mutex_lock (&task_mutex);
		member = member->next;
		pthread_mutex_unlock (&task_mutex);
	}
}
/**
@brief terminate execution of current action
Used in kill-child action
This expects BGL to be closed
@param rt pointer to data struct for the task to kill

@return
*/
static void _e2_task_kill_action (E2_TaskRuntime *rt)
{
	pthread_mutex_lock (&task_mutex);
	GList *member = g_list_find (app.taskhistory, rt);
	pthread_mutex_unlock (&task_mutex);
	if (member != NULL	//command data still exists
		&& (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED))
	{
		rt->status = E2_TASK_ABORTED;
		rt->ex.action.result = FALSE;

		pthread_mutex_lock (&task_mutex);
		gboolean queued = (athreadID == (pthread_t) rt->pid);
		pthread_mutex_unlock (&task_mutex);

		//prevent any subsequent task from being started while waiting for this one to close
		if (queued)
			_e2_task_change_status (FALSE);
		//before killing Q-thread, to allow any dialog cleanup in that thread,
		//kill all local mainloops for this action
		if (e2_main_loop_abort ((pthread_t) rt->pid))
		{
			//wait for any consequent UI change
			usleep (5000);
			WAIT_FOR_EVENTS
			gdk_flush ();
		}

		//initiate DEFERRED cancellation of the Q thread
		pthread_mutex_lock (&task_mutex);
		if (athreadID == (pthread_t) rt->pid)
		{
			pthread_cancel (athreadID);
			athreadID = 0;
		}
		else
		{
			pthread_cancel ((pthread_t)rt->pid);
		}
		pthread_mutex_unlock (&task_mutex);

		if (queued)
		{
			//re-enable any subsequent task
			_e2_task_change_status (TRUE);
			//restart action thread on next item in Q, if any
//			g_timeout_add_full (G_PRIORITY_HIGH, 0,
			g_timeout_add_full (G_PRIORITY_LOW, 100,
				(GSourceFunc) _e2_task_run_processQ, NULL, NULL);
		}
	}
}
/**
@brief terminate execution of current and pending queued tasks
Expects BGL on/closed
@param stay TRUE to reinitialise some things ready for further operations
@param mainID caller-thread (which can't be killed)

@return
*/
void e2_task_cleanup (gboolean stay, pthread_t mainID)
{
	guint kills = 0;
	e2_filelist_disable_refresh (); //minimise BGL contention
	if (stay)
		_e2_task_change_status (FALSE);	//prevent anything else in Q starting during cleanup
	else
		e2_command_clear_pending (NULL, NULL);	//make sure Q is clear

	OPENBGL

	//shutdown any monitor thread
	if (mthreadID > 0)
	{
		pthread_cancel (mthreadID);
		mthreadID = 0;
		kills++;
	}
	//shutdown any action thread
	if (athreadID > 0)
	{
		pthread_cancel (athreadID);
		//clear ID after finished checking all tasks
		kills++;
	}

	//cleanups
	gboolean killchildren = !e2_option_bool_get ("command-persist");
	E2_TaskRuntime *rt;
	GList *member;
	//any Q-thread is stopped, no need for mutex
	for (member = app.taskhistory; member != NULL; member = member->next)
	{
		rt = (E2_TaskRuntime *)member->data;
		if (rt != NULL &&
			(rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED))
		{
			if (rt->action)
			{
				if (rt->pid != (glong) athreadID //no duplication
				 && rt->pid != (glong) mainID)	//no killing caller
				{ //action may still be running in an immediate task thread or in main thread
					printd (DEBUG, "e2_task_cleanup, cancel thread: %u", rt->pid);
					pthread_cancel ((pthread_t) rt->pid);
					pthread_join ((pthread_t) rt->pid, NULL);
					kills++;
				}
				E2_ActionTaskData *qed = &rt->ex.action;
				if (stay)
				{
					qed->result = FALSE;
					if (qed->callback != NULL && qed->callback != e2_task_refresh_lists)
					{
						void (*cb) (gboolean) = qed->callback;
						(*cb) (FALSE);
					}

					if (rt->dialog != NULL)	//too-slow dialog still in play
						gtk_widget_destroy (rt->dialog);	//expect BGL closed
				}
				else
				{
					//FIXME race: can do a double-cleanup
					//e2_fileview_clean_selected (qed->names);
				}
			}
			else
			{
				if (killchildren)
					kill (
//#ifdef E2_NEW_COMMAND
//if process group is used when forking ...
//					-
//#endif
					rt->pid, SIGTERM);	//cleanup command
				/*else
				 allowing running children to continue is consistent with other
				 FM's, but the child's stdin, stdout, stderr will be disabled
				 FIND A WAY TO REVERT CHILD'S STDIO FD'S TO DEFAULTS 0,1,2 */
				kills++;
			}
			rt->status = E2_TASK_ABORTED;
		}
	}

	athreadID = 0;

	//clear any left-over blockages
	if (app.mainloops != NULL)
	{
		GSList *member = app.mainloops;
		while (member != NULL)
		{
			GSList *next = member->next;	//loop quit removes member from list
//			if (((E2_MainLoop *)member->data)->threadID == mainID)
//			{
				printd (DEBUG, "e2_task_cleanup, quit mainloop: %x", member->data);
				e2_main_loop_quit ((E2_MainLoop *)member->data);
				kills++;
//			}
			member = next;
		}
	}

	if (kills > 0)
	{
//		printd (DEBUG, "e2_task_cleanup, sleep: 0.1 sec");
		usleep (100000);	//tolerate any post-process cleanups etc
	}

	if (stay)
	{
		e2_filelist_reset_refresh ();

		_e2_task_change_status (TRUE);	//unhide anything else in Q
		//restart action thread on next item in Q, if any
		g_timeout_add_full (G_PRIORITY_HIGH, 20,
			(GSourceFunc) _e2_task_run_processQ, NULL, NULL);
	}

	if (kills > 0)
	{
		//recreate BGL mutex, it may still be locked by any aborted thread
#ifdef NATIVE_BGL
		gboolean FIXME_BGLcleanup;
		//if actual gtk mutex for BGL, use gtk_quit_add() or something to cleanup
#else
		printd (DEBUG, "e2_task_cleanup, replace BGL");
		e2_main_init_uilock (TRUE); //must cleanup the mess
#endif
	}
	CLOSEBGL
//	printd (DEBUG, "End e2_task_cleanup, BGL back on");
}
/* *
@brief thread function to block while a task is running or paused

@param status ptr to store of monitored status-flag

@return NULL
*/
/*static gpointer _e2_task_watch_status (E2_TaskRuntime *rt)
{
	e2_utils_block_thread_signals ();	//block all allowed signals to this thread

	while (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED)
	{
		usleep (66000);	//15Hz checks for status-change
	}
	return NULL;
} */
/**
@brief wait for a task to complete, and return its completion status
This is for task-functions that must return the actual task status
There is no timeout check performed here.
@param rt ptr to data for the task to monitor, NULL to use task at end of Q

@return TRUE if the task completed successfully
*/
static gboolean _e2_task_wait (E2_TaskRuntime *rt)
{
	if (rt == NULL)
	{
		GList *member;
		pthread_mutex_lock (&task_mutex);
		member = g_list_last (app.taskhistory);
		pthread_mutex_unlock (&task_mutex);
		if (member != NULL)
			rt = (E2_TaskRuntime *)member->data;
	}
	if (rt == NULL)
		return FALSE;

/*	pthread_t wthreadID;
	if (pthread_create (&wthreadID, NULL,
		(gpointer(*)(gpointer))_e2_task_watch_status, rt) == 0)
		printd (DEBUG,"watch-thread (ID=%lu) started", wthreadID);
	else
	{
		//FIXME warn the user
		printd (WARN,"watch-thread creation error!");
		return FALSE;
	}
	pthread_join (wthreadID, NULL);
*/
	//poll for status-change (15Hz to miss other timer callbacks)
	//FIXME do this with less impact, handle failure
	while (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED)
		usleep (66000);

	gboolean retval;
	if (rt->status == E2_TASK_COMPLETED)
		retval = (rt->action) ? rt->ex.action.result : (rt->ex.command.exit == 0);
	else
		retval = FALSE;
	printd (DEBUG,"wait finished, task status is %s", (retval) ? "true" : "false");
	return retval;
}
/**
@brief callback for responses from too-slow-dialog @a dialog
This approach eliminates gtk_main() (which hates being aborted) from the dialog
@param dialog the dialog from which the response was initiated
@param response the response enumerator
@param rt task-data specified when the callback was connected
@return
*/
static void _e2_task_progress_monitor_slow_response_cb (GtkDialog *dialog,
	gint response, E2_TaskRuntime *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case GTK_RESPONSE_NO:	//abort the operation
		case E2_RESPONSE_USER1:	//no more reminders
			pthread_mutex_lock (&task_mutex);
			if (rt->dialog != NULL)	//race-management
			{
				gtk_widget_destroy (rt->dialog);	//== (GTK_WIDGET(dialog)
//				WAIT_FOR_EVENTS;	//make the window repaint work
				rt->dialog = NULL;
			}
			pthread_mutex_unlock (&task_mutex);
			if (mthreadID > 0) //race-management
			{
				pthread_cancel (mthreadID);
				//if the racy action-thread ends about now, signal to it that it
				//does not need to abort this thread
				mthreadID = 0;
			}
			if (response == GTK_RESPONSE_NO	//not just cancelling reminders
				&& athreadID > 0)	//operation still not finished
			{
				_e2_task_kill_action (rt);
				rt->status = E2_TASK_INCOMPLETE;
			}
		default:
//		case GTK_RESPONSE_YES:	//keep waiting
			pthread_mutex_lock (&task_mutex);
			if (rt->dialog != NULL)	//race-management
			{
				gtk_widget_hide (rt->dialog);
//				WAIT_FOR_EVENTS;	//make the hide work
			}
			pthread_mutex_unlock (&task_mutex);
			break;
	}
	NEEDOPENBGL
}
/**
@brief thread function to enforce timeout on processing a queued task
Expects valid athreadID for the action thread
@param rt ptr to data for the currently-running task

@return NULL
*/
static gpointer _e2_task_progress_monitor (E2_TaskRuntime *rt)
{
	e2_utils_block_thread_signals ();	//block all allowed signals to this thread

	//on some systems this func may be called before the thread-creation
	//function sets mthreadID, but that should not matter ...
	rt->dialog = NULL;

	while (TRUE)	//loop until user kills this thread (via dialog) or the
					//action-thread kills this thread
	{
		gchar *msg;
//		gboolean refreshed = FALSE;
		gint seconds = e2_option_int_get ("task-timeout-interval");
		if (seconds < 1)	//should never happen, but ...
			seconds = 1;
		//1 Hz check for thread stopped
		while (seconds > 0)
		{
			sleep (1);
			if (rt->status == E2_TASK_RUNNING)
				seconds--;
		}
		//if the task is still paused, probably a dialog is showing,
		//and we don't want to interfere with that (BUT potential hang ?!)
		while (rt->status == E2_TASK_PAUSED)
			sleep (1);
		//if we get to here, it's timed out
		if (athreadID > 0)
		{
			printd (DEBUG,"action-thread (ID=%lu) timed out", (gulong) athreadID);
			if (rt->dialog == NULL)
			{
				//once-only, create (and show all) the dialog
				msg = g_strdup_printf (_("The current operation (%s)"),
					rt->ex.action.action->name);
				CLOSEBGL
				rt->dialog = e2_dialog_slow (msg, _("operation"),
					(ResponseFunc)_e2_task_progress_monitor_slow_response_cb, rt);
				OPENBGL
				g_free (msg);
			}
			//if dialog was hidden from last iteration, show it again
			if (rt->dialog != NULL &&
#ifdef USE_GTK2_18
				!gtk_widget_get_visible (rt->dialog))
#else
				!GTK_WIDGET_VISIBLE (rt->dialog))
#endif
			{
				CLOSEBGL
				gtk_widget_show (rt->dialog);
				gtk_window_present (GTK_WINDOW (rt->dialog));
				OPENBGL
			}
		}
	}
	return NULL;	//should never get to here
}
/**
@brief
@param immediate TRUE to run the task in its own thread, FALSE to Q the task
@param rt pointer to task runtime data struct
@return
*/
static void _e2_task_run_task (gboolean immediate, E2_TaskRuntime *rt)
{
	if (immediate)
	{	//process this one independently
		g_timeout_add_full (G_PRIORITY_HIGH, 0,
			(GSourceFunc) _e2_task_run_processnow, rt, NULL);
	}
	else
	{	//process as part of Q
		//Q-thread needs to be started with BGL off
		g_timeout_add_full (G_PRIORITY_HIGH, 0,
			(GSourceFunc) _e2_task_run_processQ, rt, NULL);
	}
}
/**
@brief initialise immediate or queued task after determining items to process, if any
For file.* actions the current-pane selected items are logged only if art->data
is NULL or ""
@param type code indicating the type of task to be performed
@param art pointer to action runtime data struct sent to the action
@param from the widget activated to trigger the action, possibly NULL
@param taskfunc function to be called to perform the task
@param callback function to be called when task completed (may be NULL)
@param immediate TRUE to run the task in its own thread, FALSE to Q the task

@return TRUE if setup was successful
*/
gboolean e2_task_run_task (E2_TaskType type, E2_ActionRuntime *art,
	gpointer from, gboolean (*taskfunc) (E2_ActionTaskData *),
	void (*callback) (), gboolean immediate)
{
	GPtrArray *names;
	gchar *specific_path = NULL;
	if (g_str_has_prefix (art->action->name, _A(6)))
	{	//this is an action potentially applying to selected item(s)
		if (art->data != NULL && *((gchar *)art->data) != '\0')
			 //use items in argument string
			names = _e2_task_get_names ((gchar *)art->data, &specific_path);
		else
			//no specific data, use selected items
			names = e2_fileview_get_selected (curr_view);
		if (names == NULL)
			return FALSE;
	}
	else	//e.g. command.mkdir
		names = NULL;

	E2_TaskRuntime *rt;
	//setup task-data in queue, keep trying this until done
	//the real pid is set elsewhere, when the task is performed
	while ((rt = e2_task_set_data (0, E2_TASKTYPE_ACTION, NULL)) == NULL) usleep (1000);

	if (rt == GINT_TO_POINTER (1))
	{
		//FIXME error message for user
		if (names != NULL)
			e2_fileview_clean_selected (names);
		return FALSE;
	}

	E2_ActionTaskData *qed = &rt->ex.action;
	qed->tasktype = type;
	qed->result = FALSE;
	//all item-name strings are localized
	qed->currdir = (specific_path == NULL) ?
		D_FILENAME_TO_LOCALE (curr_view->dir) : specific_path;
	qed->othrdir = D_FILENAME_TO_LOCALE (other_view->dir);
#ifdef E2_VFS
	qed->currspace = curr_view->spacedata;
	qed->othrspace = other_view->spacedata;
#endif
	qed->names = names;
	qed->action = art->action;
	qed->state = e2_action_get_current_state (from);
	qed->rt_data = (art->data == NULL) ? NULL : g_strdup (art->data);
	qed->initiator = (GtkWidget *)from;
	qed->taskfunc = taskfunc;
	qed->callback = callback;
	_e2_task_run_task (immediate, rt);
	return TRUE;
}
/**
@brief initialise immediate or queued task for items in @a names
@a srcdir, @a destdir and @a names are cleared here or in related cleanup func
@param type code indicating the type of task to be performed
@param art pointer to action runtime data struct sent to the action
@param from the widget activated to trigger the action, possibly NULL
@param taskfunc function to be called to perform the task
@param callback function to be called when task completed (may be NULL)
@param names array of E2_SelectedItemInfo's to process
@param srcdir absolute path of source-dir of items to process (localised with trailing /), or NULL
@param destdir absolute path of dest-dir of items processed (localised with trailing /), or NULL
@param immediate TRUE to run the task in its own thread, FALSE to Q the task

@return TRUE if setup was successful
*/
gboolean e2_task_run_task_custom (E2_TaskType type, E2_ActionRuntime *art,
	gpointer from, gboolean (*taskfunc) (E2_ActionTaskData *),
	void (*callback) (), gchar *srcdir, gchar *destdir,
	GPtrArray *names, gboolean immediate)
{
	E2_TaskRuntime *rt;
	//setup task-data in queue, keep trying this until done
	//the real pid is set elsewhere, when the task is performed
	while ((rt = e2_task_set_data (0, E2_TASKTYPE_ACTION, NULL)) == NULL) usleep (1000);

	if (rt == GINT_TO_POINTER (1))
	{
		//FIXME error message for user
		printd (WARN, "failed to setup task data");
		g_free (srcdir);
		g_free (destdir);
		if (names != NULL)
			e2_fileview_clean_selected (names);
		return FALSE;
	}

	E2_ActionTaskData *qed = &rt->ex.action;
	qed->tasktype = type;
	qed->result = FALSE;
	qed->currdir = srcdir;
	qed->othrdir = destdir;
#ifdef E2_VFS
	qed->currspace = curr_view->spacedata;
	qed->othrspace = other_view->spacedata;
#endif
	qed->names = names;
	qed->action = art->action;
	qed->state = e2_action_get_current_state (from);
	qed->rt_data = (art->data == NULL) ? NULL : g_strdup (art->data);
	qed->initiator = (GtkWidget *)from;
	qed->taskfunc = taskfunc;
	qed->callback = callback;
	_e2_task_run_task (immediate, rt);
	return TRUE;
}
/**
@brief refresh both filelists
This is primarily for post-task filelists update.
Both displayed filelists will be refreshed as soon as any in-progress
re-list (cd or refresh) is completed
@param qed pointer to data struct for the task being processed

@return
*/
void e2_task_refresh_lists (E2_ActionTaskData *qed)
{
//	printd (DEBUG, "initiate refresh of both filelists");
#ifdef E2_FAM
	gchar *utf;
	utf = F_FILENAME_FROM_LOCALE (qed->currdir);
	e2_filelist_request_refresh (utf, FALSE);
	F_FREE (utf, qed->currdir);
	utf = F_FILENAME_FROM_LOCALE (qed->othrdir);
	e2_filelist_request_refresh (utf, TRUE);
	F_FREE (utf, qed->othrdir);
#else
	e2_filelist_check_dirty (GINT_TO_POINTER (1));
#endif
}
/**
@brief show statusline message about task-in-progress
@return
*/
inline void e2_task_advise (void)
{
	e2_window_show_status_message //clears any existing message and spinner
#ifdef USE_GTK2_20
	(_("Busy"), TRUE);
#else
	(_("File operation in progress"));
#endif
}
/**
@brief get temp item name if needed
@param localpath localised path of item to check
@return pointer to name string, = @a path, or newly-allocated replacement
*/
gchar *e2_task_tempname (const gchar *localpath)
{
	struct stat dest_sb;
	E2_ERR_DECLARE
#ifdef E2_VFS
	VPATH data = { localpath, NULL };

	if (e2_fs_lstat (&data, &dest_sb E2_ERR_PTR()) && E2_ERR_IS (ENOENT))
#else
	if (e2_fs_lstat (localpath, &dest_sb E2_ERR_PTR()) && E2_ERR_IS (ENOENT))
#endif
	{
		E2_ERR_CLEAR
		return (gchar*)localpath;
	}
	E2_ERR_CLEAR
	return (e2_utils_get_tempname (localpath));
}
/* *
@brief dialog response callback
@param dialog the dialog which generated the response signal
@param response the response enumerator
@param dialog_result pointer to store for @a response
@return
*/
/*static void _e2_task_response_cb (GtkDialog *dialog, gint response,
	gint *dialog_result)
{
//	NEEDCLOSEBGL
	*dialog_result = response;
//	NEEDOPENBGL
} */
/**
@brief helper function for checking recursive merge item-count
This is a callback for a treewalk function
@param local_name path of item reported by the walker, localised string
@param statbuf pointer to struct stat with data about @a local_name
@param status code from the walker, indicating what type of item is being reported
@param count pointer to store for items count
@return E2TW_CONTINUE until @a count is > 1, then E2TW_STOP
*/
static E2_TwResult _e2_task_count_twcb (VPATH *local_name,
	const struct stat *statbuf, E2_TwStatus status, guint *count)
{
	E2_TwResult result;
	switch (status)
	{
		case E2TW_F:	//not directory or link
		case E2TW_SL:	//symbolic link
		case E2TW_SLN:	//symbolic link naming non-existing file
			(*count)++;
			result = (*count > 1) ? E2TW_STOP : E2TW_CONTINUE ;
			break;
		default:
			result = E2TW_CONTINUE;
			break;
	}
	return result;
}
/**
@brief check whether a recursive merge task will process multiple items
This counts only non-dirs, i.e ignores dirs that are missing from the
destination tree
Assumes BGL is open
@param sdir_local path of dir containing items to be processed, localised string
@param names_local array of localised item-names
@return TRUE if multiple items will be processed
*/
static gboolean _e2_task_count_recursive (VPATH *sdir_local, GPtrArray *names_local)
{
	guint count, items = 0;
#ifdef E2_VFS
	VPATH sdata;
	sdata.spacedata = sdir_local->spacedata;
#endif
	GString *src = g_string_sized_new (PATH_MAX);
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names_local->pdata;
	for (count=0; count < names_local->len; count++, iterator++)
	{
		//if (!
#ifdef E2_VFS
		g_string_printf (src, "%s%s", VPSTR(sdir_local), (*iterator)->filename);
		sdata.path = src->str;
		e2_fs_tw (&sdata, _e2_task_count_twcb, &items, -1,
#else
		g_string_printf (src, "%s%s", sdir_local, (*iterator)->filename);
		e2_fs_tw (src->str, _e2_task_count_twcb, &items, -1,
#endif
			E2TW_PHYS | E2TW_NODIR E2_ERR_NONE());
		//)
		//{
			//FIXME handle error
		//}
		if (items > 1)
			break;
	}
	return (items > 1);
}
/**
@brief check whether a move|copy task would put a dir into any of its own descendants
@param srcpath local path of source item
@param destpath local path of source item
Links can't be checked as the name doesn't exist yet, but its parent does
@return TRUE if the operation is NOT ok
*/
static gboolean _e2_task_circular (
#ifdef E2_VFS
	VPATH *sdata, VPATH *ddata
#else
	const gchar *srcpath, const gchar *destpath
#endif
)
{
#ifdef E2_VFS
	if (sdata->spacedata != ddata->spacedata
		|| !e2_fs_is_dir3 (sdata E2_ERR_NONE()))
#else
	if (!e2_fs_is_dir3 (srcpath E2_ERR_NONE()))
#endif
		return FALSE;

	//get real paths of dirs
	gchar *real_src = g_strdup
#ifdef E2_VFS
		(sdata->path);
#else
		(srcpath);
#endif
	e2_fs_walk_link (&real_src E2_ERR_NONE());
	if (!g_str_has_suffix (real_src, G_DIR_SEPARATOR_S))
	{
		gchar *freeme = real_src;
		real_src = g_strconcat (real_src, G_DIR_SEPARATOR_S, NULL);
		g_free (freeme);
	}
	gchar *real_dest = g_strdup
#ifdef E2_VFS
		(ddata->path);
#else
		(destpath);
#endif
	e2_fs_walk_link (&real_dest E2_ERR_NONE());
	gboolean retval = (strncmp (real_src, real_dest, strlen (real_src)) == 0);
	if (retval)
		e2_output_print_error (_("Operation not permitted - Circular directories"), FALSE);
	g_free (real_src);
	g_free (real_dest);
	return retval;
}

  /************************/
 /**** task functions ****/
/************************/

#ifdef WITH_TASKABORT
/**
@brief task-abort action menu selection callback

@param item UNUSED the selected item from a children-menu
@param rt pointer to data for the command related to @a item, or NULL for "no children" item

@return
*/
static void _e2_task_abort_cb (GtkMenuItem *item, E2_TaskRuntime *rt)
{
	if (rt != NULL)
	{	//we have a child
		NEEDCLOSEBGL
		if (rt->action)
			_e2_task_kill_action (rt);
		else
			e2_command_kill_child (rt->pid);
		NEEDOPENBGL
	}
}
/**
@brief allow user to brute-force-terminate a selected running command or action
@param from the activated widget, maybe a toolbar button
@param art UNUSED runtime data sent to this action
@return TRUE if menu is created
*/
static gboolean _e2_task_abort (gpointer from, E2_ActionRuntime *art)
{
	GtkWidget *menu = e2_menu_create_child_menu (E2_CHILD_ACTIVETASKS,
		_e2_task_abort_cb);
	if (menu != NULL)
	{
		//determine the menu's popup position
		if (GTK_IS_BUTTON (from))
			gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
				(GtkMenuPositionFunc) e2_toolbar_set_menu_position, from, 1, 0);
		else
			gtk_menu_popup (GTK_MENU (menu), NULL, NULL,
				(GtkMenuPositionFunc) e2_output_set_menu_position, app.tab.text, 0, 0);
		return TRUE;
	}
	return FALSE;
}
#endif
/**
@brief copy selected items from active to inactive pane

Overwrite checking is performed, if that option is enabled
The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_copy (gpointer from, E2_ActionRuntime *art)
{
//	printd (DEBUG, "task: copy");
	return (e2_task_enqueue_task (E2_TASK_COPY, art, from,
		_e2_task_copyQ, e2_task_refresh_lists));
}
static gboolean _e2_task_copyQ (E2_ActionTaskData *qed)
{
//	printd (DEBUG, "task: copyQ");
	if (!strcmp (qed->currdir, qed->othrdir))
	{
		//display some message ??
		return FALSE;
	}
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
	gchar *other_local = qed->othrdir;
#ifdef E2_VFS
	VPATH sdata = { NULL, qed->currspace };
	VPATH ddata = { NULL, qed->othrspace };
#endif
	E2_FileTaskMode mode = GPOINTER_TO_INT (qed->action->data);
	GString *src = g_string_sized_new (PATH_MAX);
	GString *dest = g_string_sized_new (PATH_MAX);
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	gboolean check = e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
		&& qed->currspace == qed->othrspace
#endif
	;
	//setup for tailoring over-write dialog
	gboolean multisrc;
	if (!check)
		multisrc = FALSE;
	else
	{
#ifdef E2_VFS
		sdata.path = curr_local;
#endif
		multisrc = (mode & E2_FTM_MERGE) ?
#ifdef E2_VFS
			_e2_task_count_recursive (&sdata, names)
#else
			_e2_task_count_recursive (curr_local, names)
#endif
		: names->len > 1;
	}
	OW_ButtonFlags extras = (multisrc) ? BOTHALL : NONE;

	e2_task_advise ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, copy task");
#endif
	e2_filelist_disable_refresh ();  //avoid pauses in the copy process

	for (count=0; count < names->len; count++, iterator++)
	{
		//".." entries filtered when names compiled;
		g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);  //separator comes with dir
		g_string_printf (dest, "%s%s", other_local, (*iterator)->filename);

#ifdef E2_VFS
		sdata.path = src->str;
		ddata.path = dest->str;
		if (_e2_task_circular (&sdata, &ddata))
			continue;
		if ((!(mode & E2_FTM_MERGE)	//normal copy
				|| !e2_fs_is_dir3 (&sdata E2_ERR_NONE())
				|| !e2_fs_is_dir3 (&ddata E2_ERR_NONE())	//not merging 2 dirs
			   )
			&& check && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0
		   )
#else
		if (_e2_task_circular (src->str, dest->str))
			continue;
		if ((!(mode & E2_FTM_MERGE)	//normal copy
				|| !e2_fs_is_dir3 (src->str E2_ERR_NONE())
				|| !e2_fs_is_dir3 (dest->str E2_ERR_NONE())	//not merging 2 dirs
			   )
			&& check && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0
		   )
#endif
		{
			*qed->status = E2_TASK_PAUSED;
			DialogButtons result = e2_dialog_ow_check (
#ifdef E2_VFS
				&sdata, &ddata,
#else
				src->str, dest->str,
#endif
				extras);
			*qed->status = E2_TASK_RUNNING;
			switch (result)
			{
				case YES_TO_ALL:
					check = FALSE;
				case OK:
					if (mode & E2_FTM_MERGE)
					{	//pass the check flag to backend, in case there is no
						//confirmation at top level
						if (check)
							mode |= E2_FTM_CHECK;
						else
							mode &= ~E2_FTM_CHECK;
					}
					e2_task_backend_copy (
#ifdef E2_VFS
						&sdata, &ddata,
#else
						src->str, dest->str,
#endif
						mode);
					//update local copy of check flag
					if ((mode & E2_FTM_MERGE) && check && !(mode & E2_FTM_CHECK))
						check = FALSE;
				case CANCEL:
					break;
				default:
					result = NO_TO_ALL;
					break;
			}
			if (result == NO_TO_ALL)
				break;
		}
		else  //file doesn't exist, or don't care
		{
			if (mode & E2_FTM_MERGE)
			{	//pass the check flag to backend, in case there is no
				//confirmation at top level
				if (check)
					mode |= E2_FTM_CHECK;
				else
					mode &= ~E2_FTM_CHECK;
			}
			e2_task_backend_copy
#ifdef E2_VFS
			(&sdata, &ddata, mode);
#else
			(src->str, dest->str, mode);
#endif
			if ((mode & E2_FTM_MERGE) && check && !(mode & E2_FTM_CHECK))
				check = FALSE;
		}
	}
	g_string_free (src, TRUE);
	g_string_free (dest, TRUE);
	e2_window_clear_status_message ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, copy task");
#endif
	e2_filelist_enable_refresh ();
//	printd (DEBUG, "task: copyQ FINISHED");
	return TRUE;
}
/**
@brief copy selected items from active to inactive pane, with renaming

Overwrite checking is performed, if that option is enabled
The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_copy_as (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_COPYAS, art, from,
		_e2_task_copy_asQ, e2_task_refresh_lists));
}
static gboolean _e2_task_copy_asQ (E2_ActionTaskData *qed)
{
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
	gchar *other_local = qed->othrdir;
#ifdef E2_VFS
	VPATH sdata = { NULL, qed->currspace };
	VPATH ddata = { NULL, qed->othrspace };
#endif
	GString *prompt = g_string_sized_new (NAME_MAX+64);
	GString *src = g_string_sized_new (PATH_MAX);
	GString *dest = g_string_sized_new (PATH_MAX);
	gchar *converted, *new_name, *public;
	gboolean check = e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
		&& qed->currspace == qed->othrspace
#endif
	;
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//setup for tailoring over-write dialog
	gboolean multisrc =  (check) ? names->len > 1 : FALSE;
	// setup for showing stop button in rename dialog
	OW_ButtonFlags extras = (multisrc)? NOALL : NONE;
	gint hpos = -1, vpos = -1; //start at default position

	e2_task_advise ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, copy as task");
#endif
	e2_filelist_disable_refresh ();  //avoid pauses in the copy process

	for (count=0; count < names->len; count++, iterator++)
	{
		gboolean permitted;
		g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);  //separator comes with dir
#ifdef E2_VFS
		sdata.path = src->str;
#endif
		permitted = (e2_fs_access (
#ifdef E2_VFS
			&sdata,
#else
			src->str,
#endif
			R_OK E2_ERR_NONE()) == 0); //FIXME W permission for dest dir

		//".." entries filtered when names compiled
		converted = F_FILENAME_FROM_LOCALE ((*iterator)->filename);
		public = g_markup_escape_text (converted, -1);
		g_string_printf (prompt, "%s: <b>%s</b>", _("Enter new name for"), public);
		g_free (public);

		*qed->status = E2_TASK_PAUSED;
		DialogButtons result2 = e2_dialog_positioned_input (_("copy"), prompt->str,
			converted, extras, FALSE, permitted, &hpos, &vpos, &new_name);
		*qed->status = E2_TASK_RUNNING;

		F_FREE (converted, (*iterator)->filename);
		if (result2 == OK)
		{
			converted = F_FILENAME_TO_LOCALE (new_name);
			g_string_printf (dest, "%s%s", other_local, converted);
			g_free (new_name);
			F_FREE (converted, new_name);
			if (strcmp(src->str, dest->str) == 0) continue;

#ifdef E2_VFS
			ddata.path = dest->str;
			if (_e2_task_circular (&sdata, &ddata))
				continue;
			if (check && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
			if (_e2_task_circular (src->str, dest->str))
				continue;
			if (check && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0)
#endif
			{
				*qed->status = E2_TASK_PAUSED;
				DialogButtons result = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
					&ddata,
#else
					dest->str,
#endif
					extras);
				*qed->status = E2_TASK_RUNNING;
				if (result == OK)
					e2_task_backend_copy (
#ifdef E2_VFS
					&sdata, &ddata,
#else
					src->str, dest->str,
#endif
					E2_FTM_NORMAL);
/*				else if (result == YES_TO_ALL)
				{
					do something smart about multiple-renames
				} */
				else if (result == NO_TO_ALL)
					break;
			}
			else
				e2_task_backend_copy
#ifdef E2_VFS
				(&sdata, &ddata, E2_FTM_NORMAL);
#else
				(src->str, dest->str, E2_FTM_NORMAL);
#endif
		}
		else if (result2 == NO_TO_ALL)
			break;
	}
	g_string_free (prompt,TRUE);
	g_string_free (src,TRUE);
	g_string_free (dest,TRUE);
	e2_window_clear_status_message ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, copy as task");
#endif
	e2_filelist_enable_refresh ();
	return TRUE;
}
#ifdef WITH_UNTRASH
/**
@brief move selected items from trash location back to original place or to inactive pane
Inactive pane is used if <Ctrl> key is pressed or "ctrl" supplied as action argument
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_untrash (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_TRASH, art, from,
		_e2_task_untrashQ, e2_task_refresh_lists));
}
static gboolean _e2_task_untrashQ (E2_ActionTaskData *qed)
{
//	printd (DEBUG, "task: untrash");
	gchar *tp = e2_utils_get_trash_path (qed->currdir, TRUE);
	if (tp == NULL)
	{
		CLOSEBGL
		e2_output_print_error (_("No trash directory is available"), FALSE);
		OPENBGL
		return FALSE;
	}
	//do nothing if we're not in the trash place
	gchar *local = F_FILENAME_TO_LOCALE (tp);
	if (strcmp (local, qed->currdir)
#ifdef E2_VFS
			|| qed->currspace != NULL
#endif
		)
	{
		g_free (tp);
		F_FREE (local, tp);
		//FIXME display some message
		return FALSE;
	}
	F_FREE (local, tp);

	gboolean to_other, success, retval = TRUE;
	//force move to inactive pane ?
	gboolean ignore_path;
	if (qed->rt_data != NULL && strstr ((gchar *)qed->rt_data, _("ctrl")) != NULL)
		ignore_path = TRUE;
	else if (GTK_IS_BUTTON (qed->initiator) || GTK_IS_MENU_ITEM (qed->initiator))
	{
		ignore_path = ACTION_MASK(qed,GDK_CONTROL_MASK);
	}
	else
		ignore_path = FALSE;

	GPtrArray *names = qed->names;
	GString *src = g_string_sized_new (PATH_MAX);
	GString *dest = g_string_sized_new (PATH_MAX);
	gchar *curr_local = qed->currdir;
#ifdef E2_VFS
	VPATH sdata = { NULL, qed->currspace }; //local trash only
	VPATH ddata;
#endif
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	gboolean check = e2_option_bool_get ("confirm-overwrite")
//#ifdef E2_VFS
//		&& qed->currspace == qed->othrspace not relevant for untrashing, local only
//#endif
	;
	//setup for tailoring over-write dialog
	gboolean multisrc = (check) ? names->len > 1 : FALSE;
	OW_ButtonFlags extras = (multisrc) ? BOTHALL : NONE;

	e2_task_advise ();
	e2_filelist_disable_refresh ();  //avoid pauses in the move process

	for (count=0; count < names->len; count++, iterator++)
	{
		//".." entries filtered when names compiled
		g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);  //separator comes with dir
		//get restore path from info file if possible
		if (ignore_path || !_e2_task_get_trasheditem_path (src, &dest))
		{
			//info file was not wanted, not found or not valid, use inactive pane for destination
			//FIXME message to user, maybe ask what to do
			to_other = TRUE;
			g_string_printf (dest, "%s%s", qed->othrdir, (*iterator)->filename);  //separator comes with dir
#ifdef E2_VFS
			ddata.spacedata = qed->othrspace;
#endif
		}
		else
		{
			to_other = FALSE;
#ifdef E2_VFS
			ddata.spacedata = NULL;	//no spacedata in trash info
#endif
		}
#ifdef E2_VFS
		ddata.path = dest->str;
		sdata.path = src->str;
		if (_e2_task_circular (&sdata, &ddata))
			continue;
#else
		if (_e2_task_circular (src->str, dest->str))
			continue;
#endif
#ifdef E2_VFS
		if (check && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
		if (check && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0)
#endif
		{
			*qed->status = E2_TASK_PAUSED;
			DialogButtons result = e2_dialog_ow_check (
#ifdef E2_VFS
				&sdata, &ddata,
#else
				src->str, dest->str,
#endif
				extras);
			*qed->status = E2_TASK_RUNNING;
			switch (result)
			{
				case YES_TO_ALL:
					check = FALSE;
				case OK:
					success = e2_task_backend_move
#ifdef E2_VFS
					(&sdata, &ddata);
#else
					(src->str, dest->str);
#endif
					if (success)
						_e2_task_update_trash_info (src, NULL);
					retval = retval && success;
				case CANCEL:
					break;
				default:
					result = NO_TO_ALL;
					break;
			}
			if (result == NO_TO_ALL)
				break;
		}
		else  //file doesn't exist, or don't care
		{
			success = e2_task_backend_move
#ifdef E2_VFS
			(&sdata, &ddata);
#else
			(src->str, dest->str);
#endif
			if (success)
			{
				if (!to_other)
				{
					//advise where it went, if not inactive pane
					//FIXME path encoding
					gchar *utf1 = F_DISPLAYNAME_FROM_LOCALE (src->str);
					gchar *utf2 = F_DISPLAYNAME_FROM_LOCALE (dest->str);
					gchar *msg = g_strdup_printf (_("Trashed item %s reinstated to %s"),
						utf1, utf2);
					F_FREE (utf1, src->str);
					F_FREE (utf2, dest->str);
					CLOSEBGL
					e2_output_print (&app.tab, msg, NULL, TRUE, NULL);
					OPENBGL
					g_free (msg);
				}
				_e2_task_update_trash_info (src, NULL);
			}
			else
			{
				//warning
				e2_fs_error_simple (_("Cannot reinstate %s"),
#ifdef E2_VFS
					&sdata);
#else
					src->str);
#endif
			}
			retval = retval && success;
		}
	}

	g_free (tp);
	g_string_free (src, TRUE);
	g_string_free (dest, TRUE);
	e2_window_clear_status_message ();
	e2_filelist_enable_refresh ();

	return retval;
}
#endif
/**
@brief move selected items from active pane to trash location
Corresponding info files are created
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_trashit (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_TRASH, art, from,
		_e2_task_trashitQ, e2_task_refresh_lists));
}
static gboolean _e2_task_trashitQ (E2_ActionTaskData *qed)
{
//	printd (DEBUG, "task: trash");
	gchar *tp = e2_utils_get_trash_path (qed->currdir, FALSE);
	if (tp == NULL)
	{
		CLOSEBGL
		e2_output_print_error (_("No trash directory is available"), FALSE);
		OPENBGL
		return FALSE;
	}
	gboolean retval;
	//do nothing if we're in the local trash place already
	gchar *local = F_FILENAME_TO_LOCALE (tp);
	if (g_str_has_prefix (qed->currdir, local)
#ifdef E2_VFS
			&& qed->currspace == NULL
#endif
		)
	{
		//CLOSEBGL
		//e2_output_print_error (_(FIXME), FALSE);
		//OPENBGL
		retval = FALSE;
	}
	else
	{
		gchar *full = g_build_filename (local, "files"G_DIR_SEPARATOR_S, NULL);
		retval = __e2_task_move (qed, full);
		g_free (full);
	}

	g_free (tp);
	F_FREE (local, tp);
	return retval;
}
/**
@brief move selected items from active to inactive pane

Overwrite checking is performed, if that option is enabled
The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_move (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_MOVE, art, from,
		_e2_task_moveQ, e2_task_refresh_lists));
}
static gboolean _e2_task_moveQ (E2_ActionTaskData *qed)
{
//	printd (DEBUG, "task: move");
	if (!strcmp (qed->currdir, qed->othrdir))
	{
		//display some message
		return FALSE;
	}
	else
		return (__e2_task_move (qed, NULL));
}
/**
@brief move selected items

@param from the button, menu item etc which was activated
@param art action runtime data
@param trashpath NULL to move to inactive pane, or string with localised trashpath "...Trash/files/"

@return TRUE if task completed successfully, else FALSE
*/
static gboolean __e2_task_move (E2_ActionTaskData *qed, gchar *trashpath)
{
	gboolean success, retval = TRUE;
	GPtrArray *names = qed->names;
	GString *src = g_string_sized_new (PATH_MAX);
	GString *dest = g_string_sized_new (PATH_MAX);
	gboolean trash = (trashpath != NULL);
	//curr_view->dir, other_view->dir are utf-8
	gchar *curr_local = qed->currdir;
	gchar *other_local = (trash) ? trashpath : qed->othrdir; //trashpath has trailer
#ifdef E2_VFS
	VPATH sdata = { NULL, qed->currspace };
	VPATH ddata;
	ddata.spacedata = (trash) ? NULL : qed->othrspace;	//local trash only
#endif
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	gboolean check = (trash) ? FALSE : e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
		&& qed->currspace == qed->othrspace
#endif
	;
	//setup for tailoring over-write dialog
	gboolean multisrc = (check) ? names->len > 1 : FALSE;
	OW_ButtonFlags extras = (multisrc) ? BOTHALL : NONE;

	e2_task_advise ();
	e2_filelist_disable_refresh ();  //avoid pauses in the move process

	for (count=0; count < names->len; count++, iterator++)
	{
		//".." entries filtered when names compiled
		g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);  //separator comes with dir
		g_string_printf (dest, "%s%s", other_local, (*iterator)->filename);

#ifdef E2_VFS
		sdata.path = src->str;
		ddata.path = dest->str;
		if (_e2_task_circular (&sdata, &ddata))
		{
			retval = FALSE;
			continue;
		}
		if (trash && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
		if (_e2_task_circular (src->str, dest->str))
		{
			retval = FALSE;
			continue;
		}
		if (trash && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0)
#endif
		{
			//trash without destroying existing file there
			gchar *tlocal = e2_utils_get_tempname (dest->str);
			dest = g_string_assign (dest, tlocal);
			g_free (tlocal);
		}
#ifdef E2_VFS
		ddata.path = dest->str;
		if (check && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
		if (check && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0)
#endif
		{
			*qed->status = E2_TASK_PAUSED;
			DialogButtons result = e2_dialog_ow_check (
#ifdef E2_VFS
				&sdata, &ddata,
#else
				src->str, dest->str,
#endif
				extras);
			*qed->status = E2_TASK_RUNNING;
			switch (result)
			{
				case YES_TO_ALL:
					check = FALSE;
				case OK:
					success = e2_task_backend_move
#ifdef E2_VFS
					(&sdata, &ddata);
#else
					(src->str, dest->str);
#endif
					if (success)
					{
						if (trash)
							_e2_task_trash_info (src, dest); //changes src, dest contents
						else
							_e2_task_update_trash_info (src, dest);
					}
					retval = retval && success;
				case CANCEL:
					break;
				default:
					result = NO_TO_ALL;
					break;
			}
			if (result == NO_TO_ALL)
			{
//				retval = FALSE;
				break;
			}
		}
		else  //file doesn't exist, or don't care
		{
			success = e2_task_backend_move
#ifdef E2_VFS
			(&sdata, &ddata);
#else
			(src->str, dest->str);
#endif
			if (success)
			{
				if (trash)
					_e2_task_trash_info (src, dest); //changes src, dest contents
				else
					_e2_task_update_trash_info (src, dest);
			}
			retval = retval && success;
		}
	}
	g_string_free (src, TRUE);
	g_string_free (dest, TRUE);
	e2_window_clear_status_message ();
	e2_filelist_enable_refresh ();

	return retval;
}
/**
@brief move selected items from active to inactive pane, with renaming

Overwrite checking is performed, if that option is enabled
The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_move_as (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_MOVEAS, art, from,
		_e2_task_move_asQ, e2_task_refresh_lists));
}
static gboolean _e2_task_move_asQ (E2_ActionTaskData *qed)
{
	gboolean success, retval = TRUE;
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
	gchar *other_local = qed->othrdir;
#ifdef E2_VFS
	VPATH sdata = { NULL, qed->currspace };
	VPATH ddata = { NULL, qed->othrspace };
#endif
	GString *prompt = g_string_sized_new (NAME_MAX+64);
	GString *src = g_string_sized_new (PATH_MAX);
	GString *dest = g_string_sized_new (PATH_MAX);
	gchar *converted, *new_name, *public;
	gboolean check = e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
		&& qed->currspace == qed->othrspace
#endif
	;
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	gboolean multisrc =  (check) ? names->len > 1 : FALSE;
	OW_ButtonFlags extras = (multisrc)? NOALL : NONE;
	gint horz = -1, vert = -1;	//start at default window position

	e2_task_advise ();
	e2_filelist_disable_refresh ();  //avoid pauses in the move process

	for (count = 0; count < names->len; count++, iterator++)
	{
		gboolean permitted;
		g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);  //separator comes with dir
#ifdef E2_VFS
		sdata.path = src->str;
#endif
		permitted = (e2_fs_access (
#ifdef E2_VFS
			&sdata,
#else
			src->str,
#endif
			R_OK|W_OK E2_ERR_NONE()) == 0); //FIXME for a dir, need X too, and W for the dest dir

		//".." entries filtered when names compiled
		converted = F_FILENAME_FROM_LOCALE ((*iterator)->filename);
		public = g_markup_escape_text (converted, -1);
		g_string_printf (prompt, "%s: <b>%s</b>", _("Enter new name for"), public);
		g_free (public);

		*qed->status = E2_TASK_PAUSED;
		DialogButtons result2 = e2_dialog_positioned_input (_("move"), prompt->str,
			converted, extras, FALSE, permitted, &horz, &vert, &new_name);
		*qed->status = E2_TASK_RUNNING;

		F_FREE (converted, (*iterator)->filename);
		if (result2 == OK && e2_utils_pass_whitespace (new_name) != NULL)
		{
			converted = F_FILENAME_TO_LOCALE (new_name);
			g_string_printf (dest, "%s%s", other_local, converted);
			g_free (new_name);
			F_FREE (converted, new_name);
			if (strcmp (src->str, dest->str) == 0) continue;

#ifdef E2_VFS
			ddata.path = dest->str;
			if (_e2_task_circular (&sdata, &ddata))
			{
				retval = FALSE;
				continue;
			}
			if (check && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
			if (_e2_task_circular (src->str, dest->str))
			{
				retval = FALSE;
				continue;
			}
			if (check && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0)
#endif
			{
				*qed->status = E2_TASK_PAUSED;
				DialogButtons result = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
					&ddata,
#else
					dest->str,
#endif
					extras);
				*qed->status = E2_TASK_RUNNING;
				if (result == OK)
				{
					success = e2_task_backend_move
#ifdef E2_VFS
					(&sdata, &ddata);
#else
					(src->str, dest->str);
#endif
					if (success)
						_e2_task_update_trash_info (src, dest); //changes src, dest contents
					retval = retval && success;
				}
/*				else if (result == YES_TO_ALL)
				{
					do something smart about multiple-renames ??
				} */
				else if (result == NO_TO_ALL)
					break;
			}
			else
			{
				success = e2_task_backend_move
#ifdef E2_VFS
				(&sdata, &ddata);
#else
				(src->str, dest->str);
#endif
				if (success)
					_e2_task_update_trash_info (src, dest);
				retval = retval && success;
			}
		}
		else if (result2 == NO_TO_ALL)
		{
//			retval = FALSE;
			break;
		}
	}
	g_string_free (prompt,TRUE);
	g_string_free (src,TRUE);
	g_string_free (dest,TRUE);
	e2_window_clear_status_message ();
	e2_filelist_enable_refresh ();

	return retval;
}
/**
@brief symlink selected items in active pane to the inactive pane

Overwrite checking is performed, if that option is enabled
The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_symlink (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_LINK, art, from,
		_e2_task_symlinkQ, e2_task_refresh_lists));
}
static gboolean _e2_task_symlinkQ (E2_ActionTaskData *qed)
{
	if (!strcmp (qed->currdir, qed->othrdir))
	{
		//display some message
		return FALSE;
	}
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
	gchar *other_local = qed->othrdir;
#ifdef E2_VFS
	VPATH sdata = { NULL, qed->currspace };
	VPATH ddata = { NULL, qed->othrspace };
#endif
	GString *src = g_string_sized_new (PATH_MAX);
	GString *dest = g_string_sized_new (PATH_MAX);

	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	gboolean check = e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
		&& qed->currspace == qed->othrspace
#endif
	;
	//setup for tailoring over-write dialog
	gboolean multisrc = (check) ? names->len > 1 : FALSE;
	OW_ButtonFlags extras = (multisrc) ? BOTHALL : NONE;

	//no window desensitize ...
	e2_task_advise ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, copy task");
#endif
	e2_filelist_disable_refresh ();  //avoid pauses in the link process

	for (count=0; count < names->len; count++, iterator++)
	{
		//".." entries filtered when names compiled;
		g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);  //separator comes with dir
		g_string_printf (dest, "%s%s", other_local, (*iterator)->filename);

#ifdef E2_VFS
		sdata.path = src->str;
		ddata.path = dest->str;
//		if (_e2_task_circular (&sdata, &ddata)) FIXME parent of name
//			continue;
		if (check && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
//		if (_e2_task_circular (src->str, dest->str))
//			continue;
		if (check && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0)
#endif
		{
			*qed->status = E2_TASK_PAUSED;
			DialogButtons result = e2_dialog_ow_check (
#ifdef E2_VFS
				&sdata, &ddata,
#else
				src->str, dest->str,
#endif
				extras);
			*qed->status = E2_TASK_RUNNING;
			switch (result)
			{
				case YES_TO_ALL:
					check = FALSE;
				case OK:
					e2_task_backend_link
#ifdef E2_VFS
					(&sdata, &ddata);
#else
					(src->str, dest->str);
#endif
				case CANCEL:
					break;
				default:
					result = NO_TO_ALL;
					break;
			}
			if (result == NO_TO_ALL)
				break;
		}
		else  //file doesn't exist, or don't care
		{
			e2_task_backend_link
#ifdef E2_VFS
			(&sdata, &ddata);
#else
			(src->str, dest->str);
#endif
		}
	}
	g_string_free (src, TRUE);
	g_string_free (dest, TRUE);
	e2_window_clear_status_message ();
	e2_filelist_enable_refresh ();
	return TRUE;
}
/**
@brief symlink selected items in active pane to the inactive pane, with renaming

Overwrite checking is performed, if that option is enabled
The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_symlink_as (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_LINKAS, art, from,
		_e2_task_symlink_asQ, e2_task_refresh_lists));
}
static gboolean _e2_task_symlink_asQ (E2_ActionTaskData *qed)
{
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
	gchar *other_local = qed->othrdir;
#ifdef E2_VFS
	VPATH sdata = { NULL, qed->currspace };
	VPATH ddata = { NULL, qed->othrspace };
#endif
	GString *prompt = g_string_sized_new (NAME_MAX+64);
	GString *src = g_string_sized_new (PATH_MAX);
	GString *dest = g_string_sized_new (PATH_MAX);
	gchar *converted, *new_name, *public;
	gboolean check = e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
		&& qed->currspace == qed->othrspace
#endif
	;
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//setup for tailoring over-write dialog
	gboolean multisrc =  (check) ? names->len > 1 : FALSE;
	// setup for showing stop button in rename dialog
	OW_ButtonFlags extras = (multisrc)? NOALL : NONE;
	gint horz = -1, vert = -1; //start at default window position

	e2_task_advise ();
	e2_filelist_disable_refresh ();  //avoid pauses in the link process

	for (count=0; count < names->len; count++, iterator++)
	{
		gboolean permitted;
		permitted = TRUE;	//CHECKME AND W permission for dest dir

		//".." entries filtered when names compiled
		converted = F_FILENAME_FROM_LOCALE ((*iterator)->filename);
		public = g_markup_escape_text (converted, -1);
		g_string_printf (prompt, "%s: <b>%s</b>", _("Enter new name for"), public);
		g_free (public);

		*qed->status = E2_TASK_PAUSED;
		DialogButtons result2 = e2_dialog_positioned_input (_("link"), prompt->str,
			converted, extras, FALSE, permitted, &horz, &vert, &new_name);
		*qed->status = E2_TASK_RUNNING;

		F_FREE (converted, (*iterator)->filename);
		if (result2 == OK)
		{
			g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);  //separator comes with dir
			converted = F_FILENAME_TO_LOCALE (new_name);
			g_string_printf (dest, "%s%s", other_local, converted);
			g_free (new_name);
			F_FREE (converted, new_name);
			if (strcmp(src->str, dest->str) == 0) continue;

#ifdef E2_VFS
			sdata.path = src->str;
			ddata.path = dest->str;
//			if (_e2_task_circular (&sdata, &ddata)) FIXME parent of name
//				continue;
			if (check && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
//			if (_e2_task_circular (src->str, dest->str))
//				continue;
			if (check && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0)
#endif
			{
				*qed->status = E2_TASK_PAUSED;
				DialogButtons result = e2_dialog_ow_check (NULL,
#ifdef E2_VFS
					&ddata,
#else
					dest->str,
#endif
					extras);
				*qed->status = E2_TASK_RUNNING;
				if (result == OK)
				{
					e2_task_backend_link
#ifdef E2_VFS
					(&sdata, &ddata);
#else
					(src->str, dest->str);
#endif
				}
/*				else if (result == YES_TO_ALL)
				{
					do something smart about multiple-renames
				} */
				else if (result == NO_TO_ALL)
					break;
			}
			else
			{
				e2_task_backend_link
#ifdef E2_VFS
				(&sdata, &ddata);
#else
				(src->str, dest->str);
#endif
			}
		}
		else if (result2 == NO_TO_ALL)
		{
			break;
		}
	}
	g_string_free (prompt,TRUE);
	g_string_free (src,TRUE);
	g_string_free (dest,TRUE);
	e2_window_clear_status_message ();
	e2_filelist_enable_refresh ();
	return TRUE;
}
/**
@brief delete selected items in the active pane

Confirmation is sought, if that option is enabled
The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_delete (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_DELETE, art, from,
		_e2_task_deleteQ, e2_task_refresh_lists));
}
static gboolean _e2_task_deleteQ (E2_ActionTaskData *qed)
{
	printd (DEBUG, "task: delete");
	gboolean retval = TRUE;
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
#ifdef E2_VFS
	VPATH sdata = { NULL, qed->currspace };
#endif
	GString *prompt = g_string_sized_new (NAME_MAX + 64);
	GString *src = g_string_sized_new (NAME_MAX);

	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	gboolean check = e2_option_bool_get ("confirm-delete");
	gboolean multisrc =  (check) ? names->len > 1 : FALSE;
	gint horz = -1, vert = -1;	//1st dialog at default position

	//no refresh to avoid pauses in the delete process
	if (!check)
		e2_filelist_disable_refresh ();
	e2_task_advise ();

	for (count = 0; count < names->len; count++, iterator++)
	{
		gboolean permitted, success = FALSE;

		//".." entries filtered when names compiled
		g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);
		//check if we have permission to delete this item
		//this tests _all_ W permissions, access() is not so sophisticated ...
		//FIXME for a dir, also need X and R to process any contents
#ifdef E2_VFS
		sdata.path = src->str;
		permitted = e2_fs_check_write_permission (&sdata E2_ERR_NONE());
#else
		permitted = e2_fs_check_write_permission (src->str E2_ERR_NONE());
#endif
		if (check)
		{
			*qed->status = E2_TASK_PAUSED;
			//filelist refreshing is enabled downstream, while waiting for user input
			DialogButtons result = e2_dialog_delete_check (
#ifdef E2_VFS
			&sdata,
#else
			src->str,
#endif
			multisrc, permitted, &horz, &vert);
			*qed->status = E2_TASK_RUNNING;

			switch (result)
			{
				case YES_TO_ALL:
					check = FALSE;
					e2_filelist_disable_refresh ();  //avoid pauses in the delete process
				case OK:
					success = e2_task_backend_delete
#ifdef E2_VFS
					(&sdata);
#else
					(src->str);
#endif
					if (success)
						_e2_task_update_trash_info (src, NULL);
					retval = retval && success;
				case CANCEL:
					break;
//				case NO_TO_ALL:
				default:
					result = NO_TO_ALL;
					break;
			}
			if (success && count > 0 && check)
				//show updated filelist during pauses
				e2_filelist_request_refresh (curr_view->dir, FALSE);

			if (result == NO_TO_ALL)
				break;
		}
		else  //no confirmation
		{
//			if (permitted)
//			{
				success = e2_task_backend_delete
#ifdef E2_VFS
				(&sdata);
#else
				(src->str);
#endif
				if (success)
					_e2_task_update_trash_info (src, NULL);
				retval = retval && success;
/*			}
			else
			{
				e2_fs_error_simple (
					_("You do not have authority to delete %s"),
#ifdef E2_VFS
					&sdata);
#else
					(*iterator)->filename);
#endif
			}
*/
		}
	}
	g_string_free (prompt, TRUE);
	g_string_free (src, TRUE);
	e2_window_clear_status_message ();
	if (!check)
		e2_filelist_enable_refresh ();

	return retval;
}
/**
@brief delete all items from trash location
There are no warnings for any deletion, here
Assumes BGL is closed on arrival
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_trashempty (gpointer from, E2_ActionRuntime *art)
{
//	printd (DEBUG, "task: empty trash");
	gchar *trashpath = e2_utils_get_trash_path (NULL, TRUE);	//has trailer
	if (trashpath != NULL)
	{
		GList *entries, *member;
		gchar *dlocal, *localpath, *itemname;
		e2_filelist_disable_refresh ();
		dlocal = F_FILENAME_TO_LOCALE (trashpath);
#ifdef E2_VFS
		VPATH ddata = { dlocal, NULL }; //only local spaces for trash
#endif
		OPENBGL
 		entries = (GList *)e2_fs_dir_foreach (
#ifdef E2_VFS
			&ddata,
#else
 			dlocal,
#endif
			E2_DIRWATCH_NO,	//trash is local, fast read
			NULL, NULL, NULL  E2_ERR_NONE());

		if (E2DREAD_FAILED (entries))
		{
			CLOSEBGL
			//FIXME warn user about error
			e2_filelist_enable_refresh ();
			F_FREE (dlocal, trashpath);
			g_free (trashpath);
			return FALSE;
		}
		for (member = entries; member != NULL; member=member->next)
		{
			itemname = (gchar *)member->data;
			if (strcmp (itemname, ".."))
			{
				localpath = e2_utils_strcat (dlocal, itemname);
#ifdef E2_VFS
				ddata.path = localpath;
				e2_task_backend_delete (&ddata);	//self-manages BGL
#else
				e2_task_backend_delete (localpath);
#endif
				g_free (localpath);
			}
			g_free (itemname);
		}
		if (entries != NULL)
			g_list_free (entries);
		F_FREE (dlocal, trashpath);
		//there may also be a fdo-compliant data dir to clean
		localpath = g_strrstr (trashpath, G_DIR_SEPARATOR_S"files");	//no need for UTF-8 scan
		if (localpath != NULL) //should always be TRUE
		{
			localpath++;	//include the separator
			*localpath = '\0';
			gchar *sp = e2_utils_strcat (trashpath, "info"G_DIR_SEPARATOR_S);	//no translation needed
			dlocal = F_FILENAME_TO_LOCALE (sp);
#ifdef E2_VFS
			ddata.path = dlocal;
			if (!e2_fs_access (&ddata, W_OK E2_ERR_NONE()))
#else
			if (!e2_fs_access (dlocal, W_OK E2_ERR_NONE()))
#endif
			{
#ifdef E2_VFS
				entries = (GList *)e2_fs_dir_foreach (&ddata,
#else
				entries = (GList *)e2_fs_dir_foreach (dlocal,
#endif
					E2_DIRWATCH_NO,	//trash is local, fast read
					NULL, NULL, NULL E2_ERR_NONE());
				if (!E2DREAD_FAILED (entries))
				{
					for (member = entries; member != NULL; member=member->next)
					{
						itemname = (gchar *)member->data;
						if (strcmp (itemname, ".."))
						{
							localpath = e2_utils_strcat (dlocal, itemname);
#ifdef E2_VFS
							ddata.path = localpath;
							e2_task_backend_delete (&ddata);
#else
							e2_task_backend_delete (localpath);	//self-manages BGL
#endif
							g_free (localpath);
						}
						g_free (itemname);
					}
					if (entries != NULL)
						g_list_free (entries);
				}
			}
			F_FREE (dlocal, sp);
			g_free (sp);
		}
		CLOSEBGL
		if (!g_str_has_prefix (curr_view->dir, trashpath)
		 && !g_str_has_prefix (other_view->dir, trashpath))
		{
			gchar *msg = g_strdup_printf (_("Removed all trash items from '%s'"), trashpath);
			e2_output_print (&app.tab, msg,	NULL, TRUE, NULL);
			g_free (msg);
		}
		g_free (trashpath);
		e2_filelist_enable_refresh ();
		return TRUE;
	}
	e2_output_print_error (_("No trash directory is available"), FALSE);
	return FALSE;
}
/**
@brief rename selected items in active pane to the inactive pane

Overwrite checking is performed, if that option is enabled
The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_rename (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_RENAME, art, from,
		_e2_task_renameQ, e2_task_refresh_lists));
}
static gboolean _e2_task_renameQ (E2_ActionTaskData *qed)
{
	gboolean success, retval = TRUE;
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
#ifdef E2_VFS
	VPATH sdata = { NULL, qed->currspace };
	VPATH ddata = { NULL, qed->currspace };
	VPATH tdata = { NULL, qed->currspace };
#endif
	GString *prompt = g_string_sized_new (NAME_MAX+64);
	GString *src = g_string_sized_new (PATH_MAX);
	GString *dest = g_string_sized_new (PATH_MAX);
	gchar *s, *converted, *new_name, *public;
	gboolean check = e2_option_bool_get ("confirm-overwrite");
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//setup for tailoring over-write dialog
	gboolean multisrc =  (check) ? names->len > 1 : FALSE;
	// setup for showing stop button in rename dialog
	OW_ButtonFlags extras = (multisrc)? NOALL : NONE;
	gint horz = -1, vert = -1;

	e2_task_advise ();

	for (count = 0; count < names->len; count++, iterator++)
	{
		gboolean permitted;
		g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);  //separator comes with dir
#ifdef E2_VFS
		sdata.path = src->str;
#endif
		permitted = e2_fs_check_write_permission (
#ifdef E2_VFS
			&sdata E2_ERR_NONE());
#else
			src->str E2_ERR_NONE());
#endif

		//".." entries filtered when names compiled
		converted = F_FILENAME_FROM_LOCALE ((*iterator)->filename);
		public = g_markup_escape_text (converted, -1);
		g_string_printf (prompt, "%s: <b>%s</b>", _("Enter new name for"), public);
		g_free (public);

		*qed->status = E2_TASK_PAUSED;
		DialogButtons result2 = e2_dialog_positioned_input (_("rename"), prompt->str,
			converted, extras, FALSE, permitted, &horz, &vert, &new_name);
		*qed->status = E2_TASK_RUNNING;

		if (result2 == OK && (s = e2_utils_pass_whitespace (new_name)) != NULL)
		{
			//trim any leading whitespace
			if (s > new_name)
				memcpy (new_name, s, strlen (s) + 1);
			if (!strcmp (converted, new_name))
			{
				g_free (new_name);
				F_FREE (converted, (*iterator)->filename);
//				retval = FALSE;
				continue;
			}
			gchar *new_local = F_FILENAME_TO_LOCALE (new_name);
			//it's possible to do a 'lite' move by putting separator(s) in new name
			gchar *sep = strrchr (new_local, G_DIR_SEPARATOR);
			if (sep == NULL)
				//in this case, can do 'on-the-fly' treeview rename
				g_string_printf (dest, "%s%s", curr_local, new_local);
			else
			{
				//in these cases, cannot do 'on-the-fly' treeview rename
				if (*new_local != G_DIR_SEPARATOR)
					g_string_printf (dest, "%s%s", curr_local, new_local);
				else
					g_string_printf (dest, "%s%s", curr_local, new_local+1);
				if (sep == new_local + strlen (new_local) - sizeof (gchar))
					dest = g_string_append (dest, (*iterator)->filename);
			}

#ifdef E2_VFS
			ddata.path = dest->str;
			gboolean exists = !e2_fs_access2 (&ddata E2_ERR_NONE());
#else
			gboolean exists = !e2_fs_access2 (dest->str E2_ERR_NONE());
#endif
			if (exists)
			{	//maybe new name exists, or maybe it's just the old name with some different case
				//FIXME use curr_view->case_sensitive - but view may have altered now
				gchar *old_name_lc = g_utf8_casefold (converted, -1);
				gchar *new_name_lc = g_utf8_casefold (new_name, -1);
				gboolean case_change = !strcmp (old_name_lc, new_name_lc);
				g_free (old_name_lc);
				g_free (new_name_lc);
				if (case_change)
				{	//we have just a case-difference that looks the same to the kernel
					//do interim name change to check, & also to ensure case-change happens
					gchar *tempname = e2_utils_get_tempname (src->str);
//					if (e2_task_backend_rename (slocal, tempname)
//						&& e2_task_backend_rename (tempname, dlocal))
#ifdef E2_VFS
					tdata.path = tempname;
					if (e2_task_backend_rename (&sdata, &tdata))
#else
					if (e2_task_backend_rename (src->str, tempname))
#endif
					{
#ifdef E2_VFS
						exists = !e2_fs_access2 (&ddata E2_ERR_NONE());
#else
						exists = !e2_fs_access2 (dest->str E2_ERR_NONE());
#endif
						if (exists)
						{	//the new name really exists
							//revert and process normally
#ifdef E2_VFS
							if (e2_task_backend_rename (&tdata, &sdata)
#else
							if (e2_task_backend_rename (tempname, src->str)
#endif
								&& sep == NULL)
									e2_fileview_adjust_name (curr_view,
										(*iterator)->filename, new_local, converted, new_name);

							g_free (tempname);
						}
						else
						{	//the former detection was fake, just do the rest of the rename
							//CHECKME this may be irrelevant
#ifdef E2_VFS
							if (e2_task_backend_rename (&tdata, &ddata)
#else
							if (e2_task_backend_rename (tempname, dest->str)
#endif
								&& sep == NULL)
									e2_fileview_adjust_name (curr_view,
										(*iterator)->filename, new_local, converted, new_name);
							g_free (tempname);
							F_FREE (new_local, new_name);
							g_free (new_name);
							F_FREE (converted, (*iterator)->filename);
							continue;
						}
					}
					else
					{
#ifdef E2_VFS
						sdata.path = (*iterator)->filename;
#endif
						e2_fs_error_simple (_("Cannot rename %s"),
#ifdef E2_VFS
							&sdata);
#else
							(*iterator)->filename);
#endif
						g_free (tempname);
						F_FREE (new_local, new_name);
						g_free (new_name);
						F_FREE (converted, (*iterator)->filename);
						continue;
					}
				}
			}
			if (check && exists)
			{
				*qed->status = E2_TASK_PAUSED;
				DialogButtons result = e2_dialog_ow_check (
#ifdef E2_VFS
					&sdata, &ddata,
#else
					src->str, dest->str,
#endif
					extras);
				*qed->status = E2_TASK_RUNNING;
				if (result == OK)
				{
					success = e2_task_backend_rename
#ifdef E2_VFS
					(&sdata, &ddata);
#else
					(src->str, dest->str);
#endif
					if (success)
					{
						if (sep == NULL)
							e2_fileview_adjust_name (curr_view,
								(*iterator)->filename, new_local, converted, new_name);
						_e2_task_update_trash_info (src, dest);
					}
					retval = retval && success;
				}
				else if (result == NO_TO_ALL)
				{
					F_FREE (new_local, new_name);
					g_free (new_name);
					F_FREE (converted, (*iterator)->filename);
					break;
				}
			}
			else
			{
				success = e2_task_backend_rename
#ifdef E2_VFS
				(&sdata, &ddata);
#else
				(src->str, dest->str);
#endif
				if (success)
				{
					if (sep == NULL)
						e2_fileview_adjust_name (curr_view,
							(*iterator)->filename, new_local, converted, new_name);
					_e2_task_update_trash_info (src, dest);
				}
				retval = retval && success;
			}
			F_FREE (new_local, new_name);
			g_free (new_name);
			F_FREE (converted, (*iterator)->filename);
		}
		else
		{
			F_FREE (converted, (*iterator)->filename);
			if (result2 == NO_TO_ALL)
				break;
		}
		if (retval && count > 0)
			//show renamed items while waiting
			e2_filelist_request_refresh (curr_view->dir, TRUE); //NB refresh clears curr_view->selected_names
	}

	g_string_free (prompt,TRUE);
	g_string_free (src,TRUE);
	g_string_free (dest,TRUE);

	e2_window_clear_status_message ();

	return retval;
}
/**
@brief show/change permission of selected items in active pane

The actual operation is performed by a separate back-end function
Directories are treated separately, to support recursive changes to
directories only
View is not updated immediately as the change does not trigger an auto-refresh  FIXME

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_permissions (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_CHMOD, art, from,
		_e2_task_permissionsQ, e2_task_refresh_lists));
}
static gboolean _e2_task_permissionsQ (E2_ActionTaskData *qed)
{
//	printd (DEBUG, "task: permissions");
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
#ifdef E2_VFS
	VPATH sdata;
	sdata.spacedata = qed->currspace;
#endif
	guint count;
	gint hpos = -1, vpos = -1;
	gboolean multisrc = names->len > 1;
	gboolean all = FALSE;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	GString *path = g_string_sized_new (PATH_MAX);

/* out-of-loop setup = FIXME
	GtkWidget *dialog;
	dialog = e2_permissions_dialog_setup (info, &recurse);
	e2_dialog_add_buttons_simple (dialog, buttons ..., NULL);
*/

	e2_task_advise ();

	for (count = 0; count < names->len; count++, iterator++)
	{
		DialogButtons choice;
		E2_RecurseType recurse;
		E2_ChmodType optype;
		mode_t newmode;
		gboolean permitted;
		//".." entries filtered when names compiled
//FIXME for single-setup: instead of the following, adjust file details in dialog, reset default button
		g_string_printf (path, "%s%s", curr_local, (*iterator)->filename); //separator comes with dir

		//check if we have permission to change this item
		//this tests _all_ W permissions, access() is not so sophisticated ...
#ifdef E2_VFS
		sdata.path = path->str;
		permitted = e2_fs_check_write_permission (&sdata E2_ERR_NONE());
#else
		permitted = e2_fs_check_write_permission (path->str E2_ERR_NONE());
#endif
		if (all)
		{
			if (permitted)
				choice = OK;
			else
			{
#ifdef E2_VFS
				sdata.path = (*iterator)->filename;
#endif
				e2_fs_error_simple (
					_("You do not have authority to change permission(s) of %s"),
#ifdef E2_VFS
					&sdata);
#else
					(*iterator)->filename);
#endif
				choice = CANCEL;
			}
		}
		else
		{
			//filelist refreshing enabled downstream, to prevent window popup delay
			*qed->status = E2_TASK_PAUSED;
			choice = e2_permissions_dialog_run (
#ifdef E2_VFS
				&sdata,
#else
				path->str,
#endif
				multisrc, permitted, &optype, &newmode, &recurse, &hpos, &vpos);
			*qed->status = E2_TASK_RUNNING;
		}

		switch (choice)
		{
		  case YES_TO_ALL:
			all = TRUE;
			e2_filelist_disable_refresh ();
			choice = OK;
		  case OK:
			if (permitted && optype != E2_CHMOD_NONE)
			{
#ifdef E2_INCLIST
				if (e2_task_backend_chmod (path->str, optype, newmode, recurse))
				{
					//FIXME update line in treeview
				}
#else
# ifdef E2_VFS
				sdata.path = path->str;
# endif
# ifdef E2_FAM
#  ifdef E2_VFS
				e2_task_backend_chmod (&sdata, optype, newmode, recurse);
#  else
				e2_task_backend_chmod (path->str, optype, newmode, recurse);
#  endif
# else
#  ifdef E2_VFS
				if (e2_task_backend_chmod (&sdata, optype, newmode, recurse))
				{
					if (sdata.spacedata == NULL)
					{
						sdata.path = qed->currdir;
						e2_fs_touchnow (&sdata E2_ERR_NONE());
					}
				}
#  else
				if (e2_task_backend_chmod (path->str, optype, newmode, recurse))
					//make the file-list refresher notice successful change
					e2_fs_touchnow (qed->currdir E2_ERR_NONE());
#  endif
# endif
#endif
			}
		  case CANCEL:
			break;
		  default:
			choice = NO_TO_ALL;  // break flag;
			break;
		}
		if (choice == NO_TO_ALL)
			break;
	}

	g_string_free (path, TRUE);
	e2_window_clear_status_message ();
	if (all)
		e2_filelist_enable_refresh ();

	return TRUE;
}
/**
@brief show/change user and/or group of selected items in active pane

The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_ownership (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_CHOWN, art, from,
		_e2_task_ownershipQ, e2_task_refresh_lists));
}
static gboolean _e2_task_ownershipQ (E2_ActionTaskData *qed)
{
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
#ifdef E2_VFS
	VPATH sdata;
	sdata.spacedata = qed->currspace;
#endif
	GString *path = g_string_sized_new (PATH_MAX);
	guint count;
	DialogButtons choice;
	gint hpos = -1, vpos = -1;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	gboolean multisrc = names->len > 1;
	gboolean all = FALSE;

	e2_task_advise ();

	for (count = 0; count < names->len; count++, iterator++)
	{
		gboolean permitted;
		uid_t owner_id;
		gid_t group_id;
		gboolean recurse;
		//".." entries filtered when names compiled
		g_string_printf (path, "%s%s", curr_local, (*iterator)->filename); //separator comes with dir
#ifdef E2_VFS
		sdata.path = path->str;
#endif
		//check if we have permission to change this item
		permitted = e2_fs_check_write_permission (
#ifdef E2_VFS
 			&sdata E2_ERR_NONE());
#else
 			path->str E2_ERR_NONE());
#endif
		if (all)
		{
			if (!permitted)
			{
#ifdef E2_VFS
				sdata.path = (*iterator)->filename;
#endif
				e2_fs_error_simple (
					_("You do not have authority to change owner(s) of %s"),
#ifdef E2_VFS
					&sdata);
#else
					(*iterator)->filename);
#endif
			}
			choice = (permitted) ? OK : CANCEL;
		}
		else
		{
			owner_id=-1;  //CHECKME = should these be set outside loop ?
			group_id=-1;
			recurse = FALSE;
			//leave dialog open as long as possible to prevent 'shadow' prior to screen refresh
			//filelist refreshing enabled downstream, to prevent popup delay
			*qed->status = E2_TASK_PAUSED;
			choice = e2_ownership_dialog_run (
#ifdef E2_VFS
			&sdata,
#else
			path->str,
#endif
			multisrc, permitted, &owner_id, &group_id, &recurse, &hpos, &vpos);
			*qed->status = E2_TASK_RUNNING;
		}

		switch (choice)
		{
		  case YES_TO_ALL:
			all = TRUE;
			e2_filelist_disable_refresh ();
			choice = OK;
		  case OK:
			if (permitted)
			{
//				printd (DEBUG, "ownership task: ok");
//				printd (DEBUG, "new owner id is: %d",  owner_id);
#ifdef E2_INCLIST
				if (e2_task_backend_chown (path->str, owner_id, group_id, recurse))
				{
					//FIXME update line in treeview
				}
#else
# ifdef E2_FAM
#  ifdef E2_VFS
				e2_task_backend_chown (&sdata, owner_id, group_id, recurse);
#  else
				e2_task_backend_chown (path->str, owner_id, group_id, recurse);
#  endif
# else
#  ifdef E2_VFS
				if (e2_task_backend_chown (&sdata, owner_id, group_id, recurse))
				{
					if (sdata.spacedata == NULL)
					{
						sdata.path = qed->currdir;
						e2_fs_touchnow (&sdata E2_ERR_NONE());
					}
				}
#  else
				if (e2_task_backend_chown (path->str, owner_id, group_id, recurse))
					//make the file-list refresher notice successful change
					e2_fs_touchnow (qed->currdir E2_ERR_NONE());
#  endif
# endif
#endif
			}
		  case CANCEL:
			break;
		  default:
			choice = NO_TO_ALL;  // break flag;
			break;
		}
		if (choice == NO_TO_ALL)
			break;
	}

	g_string_free (path,TRUE);
	e2_window_clear_status_message ();
	if (all)
		e2_filelist_enable_refresh ();

	return TRUE;
}
/**
@brief execute the operation specified from the drag menu, on all dragged items

It is assumed all items have the same source path, reflected in each member of
@a uris.
Nothing will be done if that source-path and @a destdir are the same.
Item overwrite checks are performed if that option is in force.
Processing of dropped items stops if any problem occurs
This is called from inside gtk's BGL
@param type code for function to be performed on dragged items
@param destdir destination-directory path, UTF-8 string with trailing /
@param uris NULL-terminated array of items to be processed

@return TRUE if the items were all processed successfully (this must be synchronous)
*/
gboolean e2_task_drop (E2_TaskType type, const gchar *destdir, gchar * const *uris)
{
//	printd (DEBUG, "callback: drop task");
	//FIXME vfs
	gint path_len = -1;
	gchar *srcdir_local = NULL;	//path of each dropped item, localised with trailing /
	gchar * const *tmp;
	GPtrArray *names = g_ptr_array_new ();
	//convert uris vector to dest path and names array, for processing
	for (tmp = uris; *tmp != NULL; tmp++)
	{
		gchar *path_local = g_filename_from_uri (*tmp, NULL, NULL);	//&error);
		if (path_local == NULL)
		{	//check if we have a bad file URI
			gchar *check = g_filename_to_uri (*tmp, NULL, NULL);
			if (check != NULL)
			{
				path_local = g_filename_from_uri (check, NULL, NULL);
				g_free (check);
			}
		}
		if (path_local != NULL)
		{
			if (path_len == -1)
			{	//once-only, setup some things
				//assumes at least 1 separator and no trailing separator CHECKME ok ?
				gchar *sep = strrchr (path_local, G_DIR_SEPARATOR);
				path_len = sep - path_local + 1;
				srcdir_local = g_strndup (path_local, path_len);
			}
			E2_SelectedItemInfo *seldata = ALLOCATE (E2_SelectedItemInfo);
			CHECKALLOCATEDWARN (seldata, continue);
			//scrub the path, want only the basename in the array
			g_strlcpy (seldata->filename, path_local + path_len, sizeof (seldata->filename));
			g_free (path_local);
			g_ptr_array_add (names, seldata);
		}
/*		else
		{
			//FIXME warn the user
			g_error_free (error);
		}
*/
	}
	gboolean result;
	//check for malformed uris
	if (names->len == 0)
	{
		g_free (srcdir_local); //in case ...
		g_ptr_array_free (names, TRUE);
		result = FALSE;
	}
	else if (srcdir_local == NULL)
	{
		e2_fileview_clean_selected (names);
		result = FALSE;
	}
	else
	{
		gchar *destdir_local = D_FILENAME_TO_LOCALE (destdir);
		if (strcmp (destdir_local, srcdir_local) == 0)
		{
			g_free (srcdir_local);
			g_free (destdir_local);
			e2_fileview_clean_selected (names);
			result = FALSE;
		}
		else
		{
			gchar *action_name;
			gboolean (*taskfunc) (E2_ActionTaskData *);
			switch (type)
			{
				case E2_TASK_LINK:
					action_name = g_strconcat (_A(6),".",_A(99),NULL);
					taskfunc = _e2_task_symlinkQ;
					break;
				case E2_TASK_MOVE:
					action_name = g_strconcat (_A(6),".",_A(65),NULL);
					taskfunc = _e2_task_moveQ;
					break;
				default:
//				case E2_TASK_COPY:
					action_name = g_strconcat (_A(6),".",_A(39),NULL);
					taskfunc = _e2_task_copyQ;
					break;
			}

			E2_Action *action = e2_action_get (action_name);
			g_free (action_name);
			E2_ActionRuntime *art = e2_action_pack_runtime (action, NULL, NULL);

			//even tho' we want real result of action, Q'd so that task-order is proper
			result = e2_task_run_task_custom (type, art, NULL, taskfunc,
				e2_task_refresh_lists, srcdir_local, destdir_local, names, FALSE);

			e2_action_free_runtime (art);

			if (result)
				result = _e2_task_wait (NULL); //wait for Q'd action real result
												//to send back to drag source
			//do NOT clear srcdir_local, destdir_local, names here,
			//that's done downstream, when finished processing the Q'd action
		}
	}
	return result;
}
/**
@brief show info about each select item in active pane

NB the active pane directory is not necessarily the current file system
dir, so the path is added to the filename
FIXME on this ?

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_file_info (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_do_task (E2_TASK_INFO, art, from, _e2_task_file_infoQ, NULL));
}
static gboolean _e2_task_file_infoQ (E2_ActionTaskData *qed)
{
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
#ifdef E2_VFS
	VPATH sdata;
	sdata.spacedata = qed->currspace;
#endif
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;

	e2_filelist_disable_refresh ();	//prevent access-related refresh

	for (count=0; count < names->len; count++, iterator++)
	{
		//".." entries filtered when names compiled
		gchar *localpath = e2_utils_strcat (curr_local, (*iterator)->filename);
#ifdef E2_VFS
		sdata.path = localpath;
		if (!e2_fs_access2 (&sdata E2_ERR_NONE()))
#else
		if (!e2_fs_access2 (localpath E2_ERR_NONE()))
#endif
		{
			gboolean multi = (names->len > 1 && count < (names->len - 1));
			//filelist refreshing enabled downstream, while waiting for dialog to close
			*qed->status = E2_TASK_PAUSED;
			DialogButtons choice = e2_file_info_dialog_run (
#ifdef E2_VFS
			&sdata,
#else
			localpath,
#endif
			 multi);
			*qed->status = E2_TASK_RUNNING;
			if (multi && (choice == NO_TO_ALL))
				break;
		}
		g_free (localpath);
	}

	e2_filelist_enable_refresh ();

	return TRUE;
}
/**
@brief view content of nomimated file or 1st selected item in active pane
ATM assume that non-local files can also be edited
The actual operation is performed by internal or extenal viewer,
depending on the option specfied

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_view (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_do_task (E2_TASK_VIEW, art, from, _e2_task_viewQ, NULL));
}
static gboolean _e2_task_viewQ (E2_ActionTaskData *qed)
{
//	printd (DEBUG, "task: view");
	GPtrArray *names = qed->names;
	if (names == NULL)
		return FALSE;

	gboolean retval = TRUE;
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//".." entries filtered when names compiled
	for (count=0; count < names->len; count++, iterator++)
	{
		gchar *local = e2_utils_strcat (qed->currdir, (*iterator)->filename);
#ifdef E2_VFS
		VPATH sdata = { local, qed->currspace };
#endif

		//grab mutex in case of error or other output message
		CLOSEBGL
		retval = retval && e2_task_backend_view
#ifdef E2_VFS
		 (&sdata);
#else
		(local);
#endif
		OPENBGL

		g_free (local);
	}
	return retval;
}
/**
@brief edit nomimated file or 1st selected item in active pane
ATM assume that non-local files can also be edited
@param from the button, menu item etc which was activated
@param art ptr to data assigned when action struct created at session start

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_edit (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_do_task (E2_TASK_EDIT, art, from, _e2_task_editQ, NULL));
}
static gboolean _e2_task_editQ (E2_ActionTaskData *qed)
{
//	printd (DEBUG, "task: edit");
	GPtrArray *names = qed->names;
	if (names == NULL)
		return FALSE;

	gchar *editor;
	if (e2_option_bool_get ("use-external-editor"))
	{
		editor = e2_option_str_get ("command-editor");
		if (*editor == '\0')
		{
			//FIXME warn user
			return FALSE;
		}
	}
	else
		editor = NULL;

	gboolean retval = TRUE;
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//".." entries filtered when names compiled
	for (count=0; count < names->len; count++, iterator++)
	{
		gchar *localpath = e2_utils_strcat (qed->currdir, (*iterator)->filename);
		if (editor != NULL)
		{
			gchar *utf = F_FILENAME_FROM_LOCALE (localpath);
			gchar *command = e2_utils_replace_name_macros (editor, utf);
			if (command == editor)	//no replacement of macro in editor
			{
//tag E2_BADQUOTES
				//CHECKME may be bad if a file.edit action is supplied
				gchar *qp = e2_utils_quote_string (utf);
				command = g_strconcat (editor, " ", qp, NULL);
				g_free (qp);
			}
			CLOSEBGL
			gint res = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, curr_view->treeview
#ifdef E2_COMMANDQ
			, TRUE
#endif
			);
			OPENBGL
			F_FREE (utf, localpath);
			g_free (command);
			retval = retval && (res == 0);
		}
		else
		{
#ifdef E2_VFS
			VPATH sdata = { localpath, qed->currspace };
#endif
			CLOSEBGL
			retval = retval && e2_edit_dialog_create (
#ifdef E2_VFS
				&sdata, NULL);
#else
				localpath, NULL);
#endif
			OPENBGL
		}
		g_free (localpath);
	}

	return retval;
}
/**
@brief perform the default action for nominated item, or for the first selected item in active pane

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_open (gpointer from, E2_ActionRuntime *art)
{
	//need to support opening a specified item, maybe with path != %d
	if (from == (gpointer) curr_view->treeview || from == (gpointer) other_view->treeview)
	{
		ViewInfo *view = (from == (gpointer) curr_view->treeview) ? curr_view : other_view;
		FileInfo *infoptr = e2_fileview_get_selected_first_local (view, TRUE);
		if (infoptr == NULL)
			return FALSE;
		//parse ".." here (for cd ".." and similar), as ".." is excluded from
		//normal selections
		if (!strcmp (infoptr->filename, "..")
	//		&& gtk_tree_selection_count_selected_rows (view->selection) == 1
		)
		{
			if (art->data != NULL)
				g_free (art->data);
//E2_VFSTMPOK
			art->data = g_strdup (view->dir);
			gint len = strlen ((gchar *)art->data);
			if (len > 1)
			{
				*((gchar *)art->data + len - 1) = '\0';	//strip trailing separator
				gchar *s = strrchr ((gchar *)art->data, G_DIR_SEPARATOR);
				if (s != NULL)
				{
					if (s > (gchar *)art->data)
						*s = '\0';
					else	//at start of string, so parent is root dir
						*(s+sizeof(gchar)) = '\0';
				}
			}
		}
	}
#ifdef E2_VFSTMP
	FIXME may be any space if a specific item was nominated
#endif
	return (e2_task_do_task (E2_TASK_OPEN, art, from, _e2_task_openQ, NULL));
}

static gboolean _e2_task_openQ (E2_ActionTaskData *qed)
{
//	printd (DEBUG, "task: open");
	gboolean retval;
	GPtrArray *names = qed->names;
	if (names == NULL)
		return FALSE;

	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	//".." entries filtered when names compiled, unless ".." is the only item selected
	//when opening a specific item, the statbuf will not be completed
	gchar *local = e2_utils_strcat (qed->currdir, (*iterator)->filename);
#ifdef E2_VFS
	VPATH sdata = { local, qed->currspace };
# ifdef E2_VFSTMP
	FIXME may be any space if a specific item was nominated
# endif
#endif

	CLOSEBGL
#ifdef E2_VFS
	retval = e2_task_backend_open (&sdata, TRUE);
#else
	retval = e2_task_backend_open (local, TRUE);
#endif
	OPENBGL

	g_free (local);

	return retval;
}
/**
@brief run a specified or chosen application on selected item(s) in active pane
If action arg is pointerised 1...6, the corresponding default action is used.
Otherwise the specified command is obtained via a dialog. If the commmand does
not include "%f" or "%p", all selected items (each with absolute path) are
concatenated, and provided as a single argument to the application. Otherwise,
the entered command is executed as is.

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_open_with (gpointer from, E2_ActionRuntime *art)
{
#ifdef E2_VFSTMP
	FIXME may be any space if a specific item was nominated
#endif
	return (e2_task_do_task (E2_TASK_OPENWITH, art, from, _e2_task_open_withQ, NULL));
}
static gboolean _e2_task_open_withQ (E2_ActionTaskData *qed)
{
	gchar *command, *defcmd, *ext, *localpath;
	GPtrArray *names;
	DialogButtons result;
	gboolean ask;

	if ((names = qed->names) == NULL)
		return FALSE;

	ask = TRUE;
	if (qed->action->data != NULL)
	{	//a "run-with" parameter was supplied
		gint which = GPOINTER_TO_INT (qed->action->data);

		//this sad hack is to avoid lots of code duplication and
		//to exactly replicate what the user would see in a context menu
		localpath = NULL;
		GtkWidget *dummy = e2_menu_get ();
		e2_filelist_disable_refresh ();  //downstream may provoke a refresh
		if (qed->initiator == (gpointer) curr_view->treeview)
		{
			e2_menu_add_filetype_items (dummy, curr_view);
			localpath = g_build_filename (qed->currdir, (gchar*)qed->names->pdata[0], NULL);
		}
		else if (qed->initiator == (gpointer) other_view->treeview)
		{
			e2_menu_add_filetype_items (dummy, other_view);
			localpath = g_build_filename (qed->othrdir, (gchar*)qed->names->pdata[0], NULL);
		}
/* if we'll allow the supplied 'index' to also cover system commands ...
		if (localpath != NULL)
		{
			e2_menu_add_filehandlers (dummy, localpath);
			g_free (localpath);
		}
*/
		e2_filelist_enable_refresh ();
		GList *items = gtk_container_get_children (GTK_CONTAINER (dummy));
		if (g_list_length (items) >= (guint)which)
		{
			GtkWidget *menu_item = (GtkWidget *)g_list_nth_data (items, which - 1);
			command = (gchar *)g_object_get_data (G_OBJECT(menu_item), "action-cmd-key");
			if (command != NULL)
			{
				gchar *sys = e2_task_system_command (command, localpath);
				if (sys != NULL)
					command = sys;
				else
					command = g_strdup (command);
				result = OK;	//run if there's a valid argument selected
				ask = FALSE;
			}
		}
		g_list_free (items);
		gtk_widget_destroy (dummy);
	}

	if (ask)
	{	//no valid "run-with" parameter provided, so ask what to do
		//determine a suggested command, the default for first-selected item
		E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
#ifdef E2_VFS
		//".." entries filtered when names compiled
		localpath = e2_utils_strcat (qed->currdir, (*iterator)->filename);
		VPATH sdata = { localpath, qed->currspace };
		//checking a virtual dir in the process of being mounted may cause bad delay
		//so we fake a rough guess at what is intended ...
		//FIXME how ?? (also needed in _e2_pane_change_dir())
		printd (DEBUG, "NEED fake check whether %s is a directory", localpath);
		if (
		//	1 ||
			e2_fs_is_dir3 (&sdata E2_ERR_NONE()))
#else
		//".." entries filtered when names compiled
		localpath = e2_utils_strcat (qed->currdir, (*iterator)->filename);
		if (e2_fs_is_dir3 (localpath E2_ERR_NONE()))
#endif
			ext = _("<directory>");
#ifdef E2_VFS
		else if (!e2_fs_access (&sdata, X_OK E2_ERR_NONE()))
#else
		else if (!e2_fs_access (localpath, X_OK E2_ERR_NONE()))
#endif
			ext = _("<executable>");
		else
		{
			ext = strchr ((*iterator)->filename, '.');	//assumes '.' is ascii
			if (ext == NULL //no extension
				|| ext == (*iterator)->filename) //hidden file
			{
#ifdef E2_VFS
				if (e2_fs_is_text (&sdata E2_ERR_NONE()))
#else
				if (e2_fs_is_text (localpath E2_ERR_NONE()))
#endif
					ext = "txt"; //too bad if this is not a recognised text extension in filetypes
				else
					ext = NULL;	//clear value from hidden file
			}
			else
			{
				do
				{
					//skip leading dot "."
					ext += sizeof (gchar); //ascii '.' always single char
					if (*ext == '\0')
					{
						ext = NULL;
						break;
					}
					if (e2_filetype_get_actions (ext) != NULL)
						break;
				} while ((ext = strchr (ext, '.')) != NULL);	//always ascii '.', don't need g_utf8_strchr()
			}
		}
		g_free (localpath);
		command = (ext == NULL) ? NULL : (gchar*)e2_filetype_get_default_action (ext);	//maybe NULL
		if (command != NULL)
		{
			defcmd = strchr (command, '@');
			if (defcmd != NULL)
				defcmd++;
			else
				defcmd = command;
			//exclude commands which start by opening another dialog
			if (g_str_has_prefix (defcmd, "%{") && strchr (defcmd+2, '}') != NULL)
				defcmd = NULL;
		}
		else
			defcmd = NULL;

		CLOSEBGL
		*qed->status = E2_TASK_PAUSED;
		result = e2_dialog_combo_input (_("open with"),
			_("Enter command:"), defcmd, 0, &open_history, &command);
		*qed->status = E2_TASK_RUNNING;
		OPENBGL
	}

	gboolean retval;
	if (result == OK)
	{
		gchar *freeme;
		if (strstr (command, "%f") == NULL && strstr (command, "%p") == NULL
#ifdef E2_BADQUOTES
			&& strstr (command, "%e") == NULL
#endif
			)
		{
			freeme = command;
			command = g_strconcat (command, " %p", NULL);
			g_free (freeme);
		}
		//substitute [%]%p and/or [%]%e and/or [%]%f from qed, as CWD and selected items may be different now
		//too bad about %d, %D, %F, %P if pane dir(s) now different
//E2_VFSTMP how to specify namespace in command arg(s) ?
		freeme = command;
		gchar *utf = F_FILENAME_FROM_LOCALE (qed->currdir);
		command = e2_utils_replace_multiname (command, utf, names, FALSE);
		g_free (freeme);
		F_FREE (utf, qed->currdir);
//		if (command == freeme)	//no replacment done, cannot actually happen
//			return FALSE;

		//grab mutex in case of error or other output message
		CLOSEBGL
		gint res = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, curr_view->treeview
#ifdef E2_COMMANDQ
		, TRUE
#endif
		);
		OPENBGL
		retval = (res == 0);

		g_free (command);
	}
	else
		retval = FALSE;

	return retval;
}
/**
@brief open directory which is currently selected, in the other pane
Downstream function expects BGL to be closed
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2_task_open_in_other_pane (gpointer from, E2_ActionRuntime *art)
{
	//handle case of specific data instead of selection??
	//FIXME do this with localised data
	//FIXME check item is a dir
//	The last-activated row in the other pane is focused - this is not
//	necessarily sensible
	//CHECKME Q this
	e2_filelist_disable_refresh ();
	gchar *name = e2_fileview_get_row_name (curr_view, curr_view->row);
	if (name == NULL)
	{
		e2_filelist_enable_refresh ();
		return FALSE;  //the model got mixed up, somehow
	}
	gchar *path = e2_utils_strcat (curr_view->dir, name); //separator comes with dir
#ifdef E2_VFS
	if (other_view->spacedata != curr_view->spacedata)
	{
		VPATH ddata = { path, curr_view->spacedata };	//utf8 path expected
		e2_pane_change_space (other_pane, &ddata);	//should never fail
	}
	else
#endif
		e2_pane_change_dir (other_pane, path);
	g_free (name);
	g_free (path);
	e2_filelist_enable_refresh ();
	return TRUE;
}
/**
@brief find and select named item in active pane
This differs from treeview type-ahead searching because the supplied pattern
does not need to be at the start of the line
The search starts from the current line, forward to end, then cycles
back to the start

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2_task_find (gpointer from, E2_ActionRuntime *art)
{
	gchar *s, *pattern;
	gint i, last, rowcount;

	//find the no. of rows in the store
	rowcount = gtk_tree_model_iter_n_children (curr_view->model, NULL);
	if (rowcount < 1)
		return FALSE;  //no message ...

	//FIXME nicer to use a dialog that stays open until specifically closed,
	//with this stuff in its "find" callback
	DialogButtons choice = e2_dialog_line_input (_("find"),
		_("Enter a name or partial name to find:"), curr_view->last_find, 0,
		TRUE, &s);

	if (choice == OK)
	{
		g_strlcpy (curr_view->last_find, s, sizeof(curr_view->last_find));
		pattern = g_strdup_printf ("*%s*", s);

		GtkTreeModel *model = curr_view->model;
		GtkTreePath *path;
		GtkTreeIter iter;
		if (gtk_tree_model_iter_nth_child (model, &iter, NULL, curr_view->row))
			path = gtk_tree_model_get_path (model, &iter);
		else
			//try fallback to the row of the current cursor position
			gtk_tree_view_get_cursor (GTK_TREE_VIEW (curr_view->treeview),
				&path, NULL);

		if (path == NULL)
		{
			last = rowcount - 1;
			//no defined start pos, start from beginning
			i = 0;
		}
		else
		{
			last = *gtk_tree_path_get_indices (path);
			if (last == rowcount - 1)
				i = 0;	//roll around to the start
			else
				i = last + 1;	//start at the next line

			gtk_tree_path_free (path);
		}

		//iterate through the rows
		gchar *thisrowname;
		gboolean matched = FALSE;
		while (i != last)
		{
			thisrowname = e2_fileview_get_row_name (curr_view, i);
			if (g_pattern_match_simple (pattern, thisrowname))
				matched = TRUE;
			g_free (thisrowname);
			if (matched)
				break;
			if (++i >= rowcount)
				i = 0;  //cycle to start
		}
		if (i == last)
		{	//nothing found sofar, but maybe it's in the "last" row, where the scan started
			thisrowname = e2_fileview_get_row_name (curr_view, i);
			if (g_pattern_match_simple (pattern, thisrowname))
				matched = TRUE;
			g_free (thisrowname);
		}
		if (matched)
		{
			gtk_tree_model_iter_nth_child (model, &iter, NULL, i);
			path = gtk_tree_model_get_path (model, &iter);
			gtk_tree_view_set_cursor (GTK_TREE_VIEW (curr_view->treeview),
				path, NULL, FALSE);
		}

		g_free(pattern);
		g_free(s);
	}

//	gtk_widget_grab_focus (curr_view->treeview);
	return TRUE;
}
/**
@brief register task-related actions

@return
*/
void e2_task_actions_register (void)
{
	E2_Action actions[] =
	{	//we don't bother explicitly setting each element's last parameters
		{ g_strconcat(_A(6),".",_A(67),NULL), _e2_task_open, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(68),NULL), _e2_task_open_in_other_pane, FALSE, E2_ACTION_TYPE_ITEM, 0, },
/*2*/	{ g_strconcat(_A(6),".",_A(69),NULL), _e2_task_open_with, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(109),NULL),_e2_task_view, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(46),NULL), _e2_task_edit, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(58),NULL), _e2_task_file_info, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(73),NULL), _e2_task_permissions, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(70),NULL), _e2_task_ownership, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(79),NULL), _e2_task_rename, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(39),NULL), _e2_task_copy, FALSE, E2_ACTION_TYPE_ITEM, 0, },	//NULL data == E2_FTM_NORMAL
		{ g_strconcat(_A(6),".",_A(41),NULL), _e2_task_copy, FALSE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (E2_FTM_MERGE), NULL},
		{ g_strconcat(_A(6),".",_A(42),NULL), _e2_task_copy, FALSE,E2_ACTION_TYPE_ITEM, 0, GINT_TO_POINTER (E2_FTM_SAMETIME), NULL},
		{ g_strconcat(_A(6),".",_A(40),NULL), _e2_task_copy_as, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(65),NULL), _e2_task_move, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(66),NULL), _e2_task_move_as, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(99),NULL), _e2_task_symlink, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(100),NULL), _e2_task_symlink_as, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(45),NULL), _e2_task_delete, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(49),NULL), _e2_task_find, FALSE, E2_ACTION_TYPE_ITEM, 0, },
		{ g_strconcat(_A(6),".",_A(18),NULL), _e2_task_trashit, FALSE, E2_ACTION_TYPE_ITEM, 0, },
#ifdef WITH_UNTRASH
		{ g_strconcat(_A(6),".",_A(108),NULL),_e2_task_untrash, TRUE, E2_ACTION_TYPE_ITEM, 0, },	//argument is optional
#endif
		//these are not applied to selected items
		{ g_strconcat(_A(18),".",_A(45),NULL), _e2_task_trashempty, FALSE, E2_ACTION_TYPE_ITEM, 0, },
#ifdef WITH_TASKABORT
		{ g_strconcat(_("task"),".",_("abort"),NULL),_e2_task_abort,FALSE, E2_ACTION_TYPE_ITEM, 0, },
#endif
	};
	guint i, count = sizeof (actions) / sizeof (E2_Action);
	for (i = 0; i < count; i++)
		e2_action_register (&actions[i]);
	//6 handler-specific "open-with" actions
	E2_Action action =
	{NULL,_e2_task_open_with,TRUE,E2_ACTION_TYPE_ITEM,0,NULL,NULL};
	for (i = 1; i < 7; i++)
	{
		action.name = g_strdup_printf ("%s%d", actions[2].name, i);
		action.data = GINT_TO_POINTER(i);
		e2_action_register (&action);
	}
}

#ifdef WITH_TASKABORT
# undef WITH_TASKABORT
#endif
#ifdef WITH_UNTRASH
# undef WITH_UNTRASH
#endif
