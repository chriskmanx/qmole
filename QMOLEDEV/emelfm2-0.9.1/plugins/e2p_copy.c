/* $Id: e2p_copy.c 2846 2013-10-26 03:31:23Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 2004 Florian Zaehringer <flo.zaehringer@web.de>
Portions copyright (C) 1999 Michael Clark

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

/**
@file plugins/e2p_cpbar.c
@brief plugin for copying selected items, with a progress-bar
*/

#include "emelfm2.h"
#include <string.h>
#include <pthread.h>
#include <signal.h>
#include <sys/shm.h>
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_option.h"
#include "e2_filelist.h"
#include "e2_task.h"
#include "e2_icons.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "copy"

//max. no. of _bytes_, at the end of a copied item's source-path
//and dest-path, shown in the progress dialog
//paths longer than this will be 'ellipsized'
#define MAX_CHAR 55
//size of buffer for progress dialog
//make this >= (MAX_CHAR+4)*2 + space for rest of message text
//#define MAX_MSG 220
//interval (usec) between progress window updates
//for items < and > 10MB resepcively
//#define MIN_UPDATE_INTERVAL 100000
//#define MAX_UPDATE_INTERVAL 200000

typedef enum
{
	E2_BARTASK_STOPPED   = 1,
	E2_BARTASK_PAUSEREQ  = 1 << 1,	//pause requested
	E2_BARTASK_PAUSED    = 1 << 2,	//actually paused
	E2_BARTASK_COMPLETED = 1 << 3,
	E2_BARTASK_SUCCEEDED = 1 << 4,
} E2_BarFlags;

typedef struct _E2_BarData
{
	guint64 count;	//count of items to be processed, in active pane
	guint64 totalsize;	//aggregate apparent size of items to be processed
} E2_BarData;

typedef struct _E2_ProgressData
{
	VPATH *dlocal;	//includes localised destination path (maybe temp) of item copied
	guint64 done_size;
} E2_ProgressData;

typedef struct _E2_BarWindowData
{
	GtkWidget *dialog;
	GtkWidget *label;
	GtkWidget *progbar;
	GtkWidget *pause_btn;
	GtkWidget *resume_btn;
	GtkWidget *stop_btn;
	E2_BarFlags bflags;
	E2_MainLoop *loop;
} E2_BarWindowData;

static PluginIface iface;

static gboolean _e2p_cpbarQ (E2_ActionTaskData *qed);

/**
@brief update items' count and total size
This is a callback for a treewalk function
Error message expects BGL to be open
@param localpath absolute path of item reported by the walker, localised string
@param statptr pointer to struct stat with data about @a localpath
@param status code from the walker, indicating what type of report it is
@param twdata pointer to tw data struct

@return E2TW_CONTINUE always
*/
static E2_TwResult
_e2p_cpbar_twcb (VPATH *localpath, const struct stat *statptr,
	E2_TwStatus status, E2_BarData *twdata)
{
	switch (status)
	{
		case E2TW_F:	//not dir or link
		case E2TW_SL:	//symbolic link
		case E2TW_SLN:	//symbolic link naming non-existing file
			twdata->totalsize += statptr->st_size;
		case E2TW_DL:	//dir, not opened due to tree-depth limit (reported upstream)
		case E2TW_DM:	//dir, not opened due to different file system (reported upstream)
		case E2TW_D:	//dir (don't care about its reported size)
		case E2TW_DRR:	//dir now readable
		case E2TW_DNR:	//unreadable dir (reported upstream)
		case E2TW_NS:	//un-statable item (reported upstream)
			twdata->count++;
//		case E2TW_DP:	//dir, finished
		default:
			break;
	}
	return E2TW_CONTINUE;
}
/**
@brief determine how much progress has been made on the current item
This provides data for progressive updates during the course of copying an item.
BGL will be open (though no UI change here)
@param data pointer to struct with data which is needed here

@return
*/
static void
_e2p_cpbar_progress (E2_ProgressData *data)
{
	gchar *localpath;
	struct stat sb;
#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = data->dlocal->spacedata;
#endif
	localpath = VPSTR (data->dlocal);

	//determine how much of current item has been copied
	E2_BarData pdata;
	pdata.totalsize = 0;
#ifdef E2_VFS
	ddata.path = localpath;
	if (!e2_fs_lstat (&ddata, &sb E2_ERR_NONE()))
#else
	if (!e2_fs_lstat (localpath, &sb E2_ERR_NONE()))
#endif
	{
		if (S_ISDIR (sb.st_mode))
		{
			//if (!
#ifdef E2_VFS
			e2_fs_tw (&ddata,
#else
			e2_fs_tw (localpath,
#endif
				_e2p_cpbar_twcb, &pdata, -1, E2TW_PHYS E2_ERR_NONE());
			//)
			//{
				//FIXME handle error
			//}
		}
		else
			pdata.totalsize = sb.st_size;
	}

	data->done_size = pdata.totalsize;
//	printd (DEBUG, "progress is %llu", pdata.totalsize);
}
/**
@brief cleanup after this task's thread is aborted
@param pid pointer to store for pid of child process doing the copying
@return
*/
static void
_e2p_cpbar_abort_clean_process (pid_t *pid)
{
	if (*pid != 0)
		kill (*pid, SIGSTOP);
	//FIXME cleanup shared memory, temp file
}
/*static void
_e2p_cpbar_abort_clean_memory (gboolean **shared)
{
	if (*shared != NULL && shmdt (*shared) == 0)
		shmctl (shmid, IPC_RMID, NULL);
}
*/
/**
@brief cleanup after this task's thread is aborted
Expects BGL open
@param dialog the progress dialog
@return
*/
static void
_e2p_cpbar_abort_clean_dialog (GtkWidget **dialog)
{
	if (GTK_IS_DIALOG (*dialog))
	{
		CLOSEBGL
		gtk_widget_destroy (*dialog);
		OPENBGL
	}
}
/**
@brief copy item @a slocal to @a dlocal, while updating progress-window details
This is called with BGL open
@param slocal path of item to be copied, localised string
@param dlocal new path of copied item, localised string
@param flags bitflags indicating task parameters
@param bdata pointer to bar data struct holding progress data
@param tdata pointer to bar data struct holding totals data
@param wdata pointer to info-window data struct

@return enumerator of user's request, if any, to abort
*/
static DialogButtons
_e2p_cpbar_exec (VPATH *slocal, VPATH *dlocal, E2_FileTaskMode flags,
	E2_BarData *bdata, E2_BarData *tdata, E2_BarWindowData *wdata)
{
	gchar progresstext[64];	//utf-8 string, middle line of progress dialog
	//localised strings
	gchar *src = F_FILENAME_FROM_LOCALE (VPSTR (slocal));
//	gchar *dest = F_FILENAME_FROM_LOCALE (VPSTR (dlocal));
	gchar *dest_dir = g_path_get_dirname (VPSTR (dlocal));

	//before copy starts, get size of item to be copied
	E2_BarData pdata = { 0,0 };
	//if (!
		e2_fs_tw (slocal, _e2p_cpbar_twcb, &pdata, -1, E2TW_PHYS E2_ERR_NONE());
	//)
	//{
		//FIXME handle error
	//}

	//work with a temp name so that the backend doesn't create its own
	//(which would prevent destination-monitoring)
	gchar *templocal = e2_utils_get_tempname (VPSTR (dlocal));
#ifdef E2_VFS
	VPATH tempdata = { templocal, dlocal->spacedata };
#endif
	key_t key = ftok (templocal, pthread_self ());
	gint shmid = shmget (key, 2*sizeof(gboolean), 0600 | IPC_CREAT);
//	gboolean *results = NULL;
//	pthread_cleanup_push ((gpointer)_e2p_cpbar_abort_clean_memory, &results); FIXME
	gboolean *results = shmat (shmid, NULL, 0);
	results[0] = FALSE;	//result-value returned by backend task function
	results[1] = FALSE;	//set TRUE when the backend task is completed (valid or not)
	pid_t pid = fork ();
	if (pid == 0)
	{	//child
		//CHECKME BGL mutex here ?
		//threads enter/leave don't hang, at least, but any message printed here
		//generates a SIGCHILD but no actual message ?
		//no background-copy option in this context
		results[0] = e2_task_backend_copy (slocal,
#ifdef E2_VFS
			&tempdata,
#else
			templocal,
#endif
			flags);
		results[1] = TRUE;	//finished now
		_exit (0);
	}
	if (pid < 0)
	{	//fork error
		printd (WARN,"action-process-create error!");
		g_free (templocal);
		//cleanup shared memory
		if (shmdt (results) == 0)
			shmctl (shmid, IPC_RMID, NULL);
		return CANCEL;
	}
	//parent process
	//for very quick operation, don't bother with anything fancy
	usleep (50000);
	if (!results[1])
	{	//task not finished already, do progress reporting
		//to reduce race-risk, this push should be before the fork, but in that case
		//we can't pop in case of fork error
		pthread_cleanup_push ((gpointer)_e2p_cpbar_abort_clean_process, &pid);

		gchar *shortsrc = e2_utils_str_shorten (src, MAX_CHAR, E2_DOTS_START);
		gchar *shortdest = e2_utils_str_shorten (dest_dir, MAX_CHAR, E2_DOTS_START);
		gchar *num1 = g_strdup_printf ("%"PRIu64, bdata->count);	//gettext workaround
		gchar *num2 = g_strdup_printf ("%"PRIu64, tdata->count);
		gchar *labeltext = g_strdup_printf (
			_("copying %s\nto %s\nthis is item %s of %s"),
			shortsrc, shortdest, num1, num2);
		CLOSEBGL
		gtk_label_set_text (GTK_LABEL (wdata->label), labeltext);
#ifdef USE_GTK2_18
		if (!gtk_widget_get_visible (wdata->dialog))
#else
		if (!GTK_WIDGET_VISIBLE (wdata->dialog))
#endif
			gtk_widget_show (wdata->dialog);
		OPENBGL
		g_free (shortsrc);
		g_free (shortdest);
		g_free (num1);
		g_free (num2);
		g_free (labeltext);

		gchar *progress_format = _("%.2f MB of %.2f MB  (%.0f\%%)");
		gfloat fraction;
		guint64 progress;

		E2_ProgressData m_data;
#ifdef E2_VFS
		m_data.dlocal = &tempdata;
#else
		m_data.dlocal = templocal;
#endif
		m_data.done_size = 0;
/*		//rough approach to setting the reporting interval (usec)
		guint refresh_interval;
		if (pdata.totalsize < 10000000)
			refresh_interval = MIN_UPDATE_INTERVAL;
		else
			refresh_interval = MAX_UPDATE_INTERVAL;	//with a cap
*/
		while (!results[1])	//loop until the action is completed
		{
			if (wdata->bflags & E2_BARTASK_STOPPED)	//user wants to abort
			{
				//cancel the task process
				kill (pid, SIGKILL);
				//CHECKME properly abort the whole process
				CLOSEBGL
				gtk_widget_destroy (wdata->dialog);
				OPENBGL
				//cleanup anything part-copied
#ifdef E2_VFS
				e2_task_backend_delete (&tempdata);
#else
				e2_task_backend_delete (templocal);
#endif
				g_free (templocal);
				//FIXME cleanup cancellation push(es)
				//cleanup shared memory
				if (shmdt (results) == 0)
					shmctl (shmid, IPC_RMID, NULL);
				return NO_TO_ALL;
			}

			_e2p_cpbar_progress (&m_data);
			progress = m_data.done_size + bdata->totalsize;

			fraction = (gdouble) progress / tdata->totalsize;
			//deal with rounding errors
			if (fraction > 1.0)
				fraction = 1.0;
			g_snprintf (progresstext, sizeof (progresstext), progress_format,
				progress / 1048576.0, tdata->totalsize / 1048576.0,
				fraction * 100.0);
			CLOSEBGL
			gtk_progress_bar_set_text (GTK_PROGRESS_BAR (wdata->progbar), progresstext);
			gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR (wdata->progbar), fraction);
			OPENBGL

			if (wdata->bflags & E2_BARTASK_PAUSEREQ //pause was requested
				&& GTK_IS_DIALOG (wdata->dialog) &&
#ifdef USE_GTK2_18
				gtk_widget_get_visible (wdata->dialog))
#else
				GTK_WIDGET_VISIBLE (wdata->dialog))
#endif
			{
				wdata->bflags &= ~E2_BARTASK_PAUSEREQ;
				wdata->loop = e2_main_loop_new (FALSE);
				if (wdata->loop != NULL)
				{
					kill (pid, SIGSTOP);
					wdata->bflags |= E2_BARTASK_PAUSED;
					e2_filelist_enable_refresh ();
					pthread_cleanup_push ((gpointer)OPENBGL_NAME,
#ifdef DEBUG_MESSAGES
						 NULL
#else
						 &display_mutex
#endif
					);
					CLOSEBGL
					e2_main_loop_run (wdata->loop);
					pthread_cleanup_pop (1);
					kill (pid, SIGCONT);	//don't get to here if stopped or aborted
				}
			}
			else
				usleep (100000);	//no need to hog CPU by refreshing too fast (cancellation point)
		}

		//show the full-time score ASAP
		if (results[0]	//copy succeeded
			&& GTK_IS_WIDGET (wdata->dialog) &&
#ifdef USE_GTK2_18
			gtk_widget_get_visible (wdata->dialog))
#else
			GTK_WIDGET_VISIBLE (wdata->dialog))
#endif
		{
			progress = pdata.totalsize + bdata->totalsize;
			g_snprintf (progresstext, sizeof (progresstext), progress_format,
				progress / 1048576.0, tdata->totalsize / 1048576.0,
				100.0);
			CLOSEBGL
			gtk_progress_bar_set_text (GTK_PROGRESS_BAR (wdata->progbar), progresstext);
			gtk_progress_bar_set_fraction (GTK_PROGRESS_BAR (wdata->progbar),
				(gdouble) progress / tdata->totalsize);
			OPENBGL
			//update progressive total
			bdata->totalsize = progress;
		}
		pthread_cleanup_pop (0);
	}
	else
		bdata->totalsize += pdata.totalsize;

	if (results[0]) //copy succeeded
		e2_task_backend_rename (
#ifdef E2_VFS
		&tempdata, dlocal);
#else
		templocal, dlocal);
#endif
	else
		//cleanup anything part-copied
		e2_task_backend_delete (
#ifdef E2_VFS
		&tempdata);
#else
		templocal);
#endif

//	pthread_cleanup_pop (0);
	g_free (templocal);
	//cleanup shared memory
	if (shmdt (results) == 0)
		shmctl (shmid, IPC_RMID, NULL);

	return OK;
}
/**
@brief "response" signal callback for progress dialog
@param dialog UNUSED the dialog where the response was generated
@param response the response returned from the dialog
@param wdata pointer to data struct for the dialog

@return
*/
static void _e2p_cpbar_response_cb (GtkDialog *dialog, gint response,
	E2_BarWindowData *wdata)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_USER1:	//pause
			if ((wdata->bflags & E2_BARTASK_PAUSED) == 0)
			{	//not paused already
				gtk_widget_set_sensitive (wdata->pause_btn, FALSE);
				gtk_widget_set_sensitive (wdata->resume_btn, TRUE);
				gtk_widget_grab_focus (wdata->resume_btn);
				wdata->bflags |= E2_BARTASK_PAUSEREQ;
				//actual pause is started near end of _e2p_mvbar_exec()
			}
			break;
		case E2_RESPONSE_USER2:	//resume
			wdata->bflags &= ~E2_BARTASK_PAUSEREQ;	//insurance
			if (wdata->bflags & E2_BARTASK_PAUSED)
			{
				gtk_widget_set_sensitive (wdata->resume_btn, FALSE);
				gtk_widget_set_sensitive (wdata->pause_btn, TRUE);
				gtk_widget_grab_focus (wdata->pause_btn);
				wdata->bflags &= ~E2_BARTASK_PAUSED;
				e2_filelist_disable_refresh ();
				e2_main_loop_quit (wdata->loop);
				wdata->loop = NULL;
			}
			break;
		case E2_RESPONSE_NOTOALL:	//cancel
		case GTK_RESPONSE_NONE:	//dialog hidden or destroyed
		case GTK_RESPONSE_DELETE_EVENT:
			wdata->bflags |= E2_BARTASK_STOPPED;
			wdata->bflags &= ~E2_BARTASK_PAUSEREQ;	//insurance
			//handle paused copying
			if (wdata->bflags & E2_BARTASK_PAUSED)
			{
				wdata->bflags &= ~E2_BARTASK_PAUSED;
				e2_filelist_disable_refresh ();
				e2_main_loop_quit (wdata->loop);
				wdata->loop = NULL;
			}
		default:
			break;
	}
	NEEDOPENBGL
}
/**
@brief cpbar action
This sets up progress display window, and processes each selected item

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean
_e2p_cpbar (gpointer from, E2_ActionRuntime *art)
{
	//this is the default, art->action->data is NULL
	return (e2_task_enqueue_task (E2_TASK_COPY, art, from,
		_e2p_cpbarQ, e2_task_refresh_lists));
}
/**
@brief as for cpbar, except preserve the mtime of copied items

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean
_e2p_cpbar_sametime (gpointer from, E2_ActionRuntime *art)
{
	art->action->data = GINT_TO_POINTER (E2_FTM_SAMETIME); //trigger the different process
	return (e2_task_enqueue_task (E2_TASK_COPY, art, from,
		_e2p_cpbarQ, e2_task_refresh_lists));
}

static gboolean
_e2p_cpbarQ (E2_ActionTaskData *qed)
{
	//handle case of specific data instead of selection??
	if (!strcmp (qed->currdir, qed->othrdir))
	{
		//display some message ??
		return FALSE;
	}
	E2_ERR_DECLARE
	//FIXME change dir permission if possible ?
#ifdef E2_VFS
	VPATH sdata = { qed->currdir, qed->currspace };
	VPATH ddata = { qed->othrdir, qed->othrspace };
	if (e2_fs_access (&ddata, W_OK E2_ERR_PTR()))
#else
	if (e2_fs_access (qed->othrdir, W_OK E2_ERR_PTR()))
#endif
	{
		e2_fs_error_local (_("Cannot put anything in %s"),
#ifdef E2_VFS
			&ddata E2_ERR_MSGL());
#else
			qed->othrdir E2_ERR_MSGL());
#endif
		E2_ERR_CLEAR
		return FALSE;
	}

	GPtrArray *names = qed->names;
	GtkWidget *dialog_vbox;	//, *hbox;
	GString *src = g_string_sized_new (1024);
	GString *dest = g_string_sized_new (1024);

	//setup the information window
	E2_BarWindowData windowdata;
	windowdata.bflags = 0;
	windowdata.loop = NULL;	//no pause yet
	windowdata.dialog = NULL;	//setup for abort
	pthread_cleanup_push ((gpointer)_e2p_cpbar_abort_clean_dialog, &windowdata.dialog);
	CLOSEBGL
	windowdata.dialog = e2_dialog_create (NULL, NULL, _("copying"),
		(ResponseFunc)_e2p_cpbar_response_cb, &windowdata);
	e2_dialog_setup (windowdata.dialog, app.main_window);
	OPENBGL

#ifndef USE_GTK3_0
	gtk_dialog_set_has_separator (GTK_DIALOG (windowdata.dialog), FALSE);
#endif
	dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (windowdata.dialog));
#else
		GTK_DIALOG (windowdata.dialog)->vbox;
#endif
	windowdata.label = e2_widget_add_mid_label (dialog_vbox, "", 0.0, FALSE, 0);

//	hbox = e2_widget_add_box (vbox, FALSE, 0, TRUE, FALSE, 0);
//	windowdata.label2 = e2_widget_add_mid_label (hbox, "", 0.5, TRUE, 0);

	windowdata.progbar = gtk_progress_bar_new ();
	gtk_box_pack_start (GTK_BOX (dialog_vbox), windowdata.progbar, TRUE, TRUE, E2_PADDING_LARGE);

	//buttons
	E2_Button local_btn = { _("_Pause"), STOCK_NAME_MEDIA_PAUSE,
		//_("Suspend copying"), E2_BTN_TIPPED, E2_BTN_TIPPED,
		NULL, 0, 0,
		E2_RESPONSE_USER1 };
	windowdata.pause_btn = e2_dialog_add_defined_button (windowdata.dialog, &local_btn);

	local_btn.label = _("_Resume");
	local_btn.name = STOCK_NAME_MEDIA_PLAY;
	//local_btn.tip = _("Resume copying after pause");
	local_btn.response = E2_RESPONSE_USER2;
	windowdata.resume_btn = e2_dialog_add_defined_button (windowdata.dialog, &local_btn);
	//this one is disabled for now
	gtk_widget_set_sensitive (windowdata.resume_btn, FALSE);

	local_btn = E2_BUTTON_CANCEL;
	local_btn.showflags |= E2_BTN_DEFAULT;
	windowdata.stop_btn = e2_dialog_add_defined_button (windowdata.dialog, &local_btn);
	//show everything but the dialog itself
	gtk_widget_show_all (dialog_vbox);

	e2_dialog_set_negative_response (windowdata.dialog, E2_BUTTON_CANCEL.response);

	//accumulate non-recursed count and total size of src item(s)
	E2_BarData totaldata = { 0,0 };
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	for (count=0; count < names->len; count++, iterator++)
	{
		g_string_printf (src, "%s%s", qed->currdir, (*iterator)->filename);  //separator comes with dir
#ifdef E2_VFS
		sdata.path = src->str;
#endif
		//if (!
			e2_fs_tw (
#ifdef E2_VFS
			&sdata,
#else
			src->str,
#endif
			_e2p_cpbar_twcb, &totaldata, -1, E2TW_PHYS E2_ERR_NONE());
		//)
		//{
			//FIXME handle error
		//}
	}
	totaldata.count = names->len;	//not interested in nested count

	gboolean check = e2_option_bool_get ("confirm-overwrite")
#ifdef E2_VFS
		&& qed->currspace == qed->othrspace
#endif
	;
	E2_BarData progressdata = { 1,0 };
	OW_ButtonFlags extras = (totaldata.count > 1) ? BOTHALL : NONE;
	iterator = (E2_SelectedItemInfo **) names->pdata;

//	e2_task_advise ();
	e2_filelist_disable_refresh ();

	for (count=0; count < names->len; count++, iterator++)
	{
		if (windowdata.bflags & E2_BARTASK_STOPPED)
			break;	//user pressed stop btn or closed info window
		//src_dir, dest_dir have trailing "/"
		g_string_printf (src, "%s%s", qed->currdir, (*iterator)->filename);  //separator comes with dir
		g_string_printf (dest, "%s%s", qed->othrdir, (*iterator)->filename);

		DialogButtons result;
#ifdef E2_VFS
		sdata.path = src->str;
		ddata.path = dest->str;
		if (check && e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
#else
		if (check && e2_fs_access2 (dest->str E2_ERR_NONE()) == 0)
#endif
		{
			*qed->status = E2_TASK_PAUSED;
			result = e2_dialog_ow_check (
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
					result = _e2p_cpbar_exec (
#ifdef E2_VFS
					&sdata, &ddata,
#else
					src->str, dest->str,
#endif
					GPOINTER_TO_INT (qed->action->data),
					&progressdata, &totaldata, &windowdata);
					break;
				case CANCEL:
					break;
				default:
					result = NO_TO_ALL;
					break;
			}
		}
		else  //no overwrite, or don't care
		{
			result = _e2p_cpbar_exec (
#ifdef E2_VFS
			&sdata, &ddata,
#else
			src->str, dest->str,
#endif
			GPOINTER_TO_INT (qed->action->data),
			&progressdata, &totaldata, &windowdata);
		}
		if (result == NO_TO_ALL)
		{
			break;
		}
		progressdata.count++;
	}

	pthread_cleanup_pop (1);	//always cleanup any dialog
	g_string_free (src, TRUE);
	g_string_free (dest, TRUE);
	e2_filelist_request_refresh (other_view->dir, TRUE); //src pane refreshed normally
	e2_filelist_enable_refresh ();
	return TRUE;
}

/**
@brief initialize this plugin

@param p pointer to plugin data struct to be populated
@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_INTRO
	PLUGINIT_NUMBERED_ALLOCATE(2)

	PLUGINIT_NUMBERED_ACTION(1,_A(6),_("cpbar"),_e2p_cpbar,
		_("_Copy"),
		_("Copy selected items, with displayed progress details"),
		"plugin_" ANAME E2ICONTB)
	PLUGINIT_NUMBERED_ACTION(2,_A(6),_("cpbar_with_time"),_e2p_cpbar_sametime,
		_("Copy with _times"),
		_("Copy selected items, with preserved time-properties and displayed progress details"),
		NULL)

	PLUGINIT_NUMBERED_END
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)
	return ret;
}
