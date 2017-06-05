/* $Id: e2p_timeset.c 2841 2013-10-24 10:04:36Z tpgww $

Copyright (C) 2006-2013 tooar <tooar@emelfm2.net>

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
@file plugins/e2p_timeset.c
@brief plugin for setting item times, and related functions
This is NOT thread-safe
*/

#include "emelfm2.h"
#include <time.h>
#include <sys/time.h>
#include <string.h>
#include "e2_plugins.h"
#include "e2_filelist.h"
#include "e2_dialog.h"
#include "e2_ownership_dialog.h"
#include "e2_task.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "timeset"

typedef struct _E2_TouchData
{
	gboolean continued_after_problem;
	time_t mtime;
	time_t atime;
	time_t ctime;
	GList *dirdata;
} E2_TouchData;

typedef struct _E2_TimesDlgRuntime
{
	GtkWidget *mcurrent;
	GtkWidget *acurrent;
	GtkWidget *ccurrent;
	GtkWidget *mdate_combo;
	GtkWidget *adate_combo;
	GtkWidget *cdate_combo;
	GtkWidget *mtime_combo;
	GtkWidget *atime_combo;
	GtkWidget *ctime_combo;
	GtkWidget *set_mtime_button;
	GtkWidget *set_atime_button;
	GtkWidget *set_ctime_button;
	GtkWidget *recurse_button;
	gboolean *recurse;
	gboolean permission;
	DialogButtons real_choice;	//store for over-ride dialog choice
	E2_TouchData *data;
} E2_TimesDlgRuntime;

static PluginIface iface;

static GList *mdate_history = NULL;
static GList *mtime_history = NULL;
static GList *adate_history = NULL;
static GList *atime_history = NULL;
static GList *cdate_history = NULL;
static GList *ctime_history = NULL;

static gboolean _e2p_task_timesQ (E2_ActionTaskData *qed);

/**
@brief change times of @a path

@param path localised string, absolute path of item to change
@param sb pointer to struct stat for with info about the current item
@param data pointer to replacement time data

@return TRUE if the change was completed
*/
static gboolean _e2pt_touch1 (VPATH *path, const struct stat *statptr, E2_TouchData *data)
{
	struct utimbuf tb;
	struct timeval systime1, systime2, strt, nd;
	struct timezone tz;
	//old or new mod time
	tb.modtime = (data->mtime == (time_t ) -1) ? statptr->st_mtime : data->mtime;
	//old or new access time
	tb.actime = (data->atime == (time_t ) -1) ? statptr->st_atime : data->atime;

	gboolean retval = TRUE;
	struct tm *tmptr;
	gboolean fixc = (data->ctime != (time_t) -1);
	if (fixc)
	{	//store current sys time
		gettimeofday (&systime1, &tz);
		//set sys time to required ctime approx.
		time_t _now = time (NULL);
		strt.tv_sec = data->ctime;
		strt.tv_usec = 0;
		tmptr = localtime (&_now);
		if (tmptr->tm_isdst > 0)
		{  //daylight savings is in effect
			strt.tv_sec -= 3600; //FIXME correct offset
		}
		else
			printd (DEBUG, "DST hack NOT done");

		settimeofday (&strt, NULL);
	}
	if (fixc || tb.modtime != statptr->st_mtime || tb.actime != statptr->st_atime)
	{
		E2_ERR_DECLARE
		if (e2_fs_utime (path, &tb E2_ERR_PTR()))
		{
#ifdef E2_VFSTMP
			//FIXME handle error
#endif
			E2_ERR_CLEAR
			retval = FALSE;
		}
	}
	if (fixc)
	{ //find elapsed time since sys time was reset
		gettimeofday (&nd, NULL);
		gint nsec;
/*		if (nd.tv_usec < strt.tv_usec)
		{
		    nsec = (strt.tv_usec - nd.tv_usec) / 1000000 + 1;
    		strt.tv_usec -= 1000000 * nsec;
    		strt.tv_sec += nsec;
		}
		if (nd.tv_usec - strt.tv_usec > 1000000)
		{
			nsec = (strt.tv_usec - nd.tv_usec) / 1000000;
			strt.tv_usec += 1000000 * nsec;
			strt.tv_sec -= nsec;
		} */
		systime2.tv_sec = systime1.tv_sec + nd.tv_sec - strt.tv_sec;
		systime2.tv_usec = systime1.tv_usec + nd.tv_usec - strt.tv_usec;
		if (systime2.tv_usec > 1000000)
		{
			nsec = systime2.tv_usec / 1000000 + 1;
			systime2.tv_usec -= 1000000 * nsec;
			systime2.tv_sec += nsec;
		}
		//set sys time = old sys time + elapsed time
		//no DST correction needed
		settimeofday (&systime2, &tz);
   }
   return retval;
}
/**
@brief callback function for recursive directory touches

Tree is being walked breadth-first, not physical.
Dirs are made accessible and writable if not already
so and it's permitted, dirs are added to a list to be
processed after all the tree has been traversed,
Other items are changed as requested (if possible)
Error messages expect BGL off
@param localpath absolute path of item to change, localised string
@param statptr pointer to struct stat with info about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data pointer to user-specified data

@return E2TW_CONTINUE on success, others as appropriate
*/
static E2_TwResult _e2_task_twcb_touch (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, E2_TouchData *user_data)
{
	E2_TwResult retval = E2TW_CONTINUE;	//default error code = none
	E2_ERR_DECLARE

	switch (status)
	{
		mode_t mode, newmode;
		E2P_DirEnt *dirfix;
		GList *member;
#ifdef E2_VFS
		VPATH ddata;
#endif
		case E2TW_DP:	//dir completion, touch, revert mode if needed, cleanup
#ifdef E2_VFS
			ddata.spacedata = localpath->spacedata;
#endif
			for (member = user_data->dirdata; member != NULL; member = member->next)
			{
				dirfix = member->data;
				if (dirfix != NULL)
				{
					if (!strcmp (dirfix->path, localpath))
					{
#ifdef E2_VFS
						ddata.path = dirfix->path;
						if (!_e2pt_touch1 (&ddata, statptr, user_data))
#else
						if (!_e2pt_touch1 (dirfix->path, statptr, user_data))
#endif
							retval = E2TW_FIXME;
#ifdef E2_VFS
						if (e2_fs_chmod (&ddata, dirfix->mode E2_ERR_PTR())
#else
						if (e2_fs_chmod (dirfix->path, dirfix->mode E2_ERR_PTR())
#endif
							&& E2_ERR_ISNOT (ENOENT))	//CHECKME ctime effect
						{
							e2_fs_error_local (_("Cannot change times of %s"),
								localpath E2_ERR_MSGL());
							retval = E2TW_FIXME;
						}
						g_free (dirfix->path);
						DEALLOCATE (E2P_DirEnt, dirfix);
						user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
						break;
					}
				}
//				else //should never happen CHECKME ok when walking list ?
//					user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
			}
			break;
		case E2TW_DRR:	//directory now readable
//CHECKME	retval |= E2TW_DRKEEP;	//no permission reversion in walker, as that stuffs ctime?
		case E2TW_D:
			//ensure dir is writable, if we can
			if (e2_fs_tw_adjust_dirmode (localpath, statptr, (W_OK | X_OK)) == 0)
			{
				//take a shot at doing the change, anyhow, probably fails
				_e2pt_touch1 (localpath, statptr, user_data);
				//FIXME warn user about failure
				retval |= E2TW_SKIPSUB;	//don't try to do any descendant
			}
			else	//dir can be processed
			{
				//add this dir to list of items to touch afterwards
				dirfix = ALLOCATE (E2P_DirEnt);
				CHECKALLOCATEDWARNT (dirfix, retval=E2TW_STOP;break;)
				dirfix->path = g_strdup (VPSTR (localpath));
				dirfix->mode = statptr->st_mode & ALLPERMS;	//want to restore the original value
				user_data->dirdata = g_list_prepend (user_data->dirdata, dirfix);
			}
			break;
		case E2TW_DM:	//dir not opened (reported upstream)
		case E2TW_DL:	//ditto
		case E2TW_DNR:	//unreadable directory (for which, error is reported upstream)
						//touch for this will probably fail, but try anyhow
			mode = statptr->st_mode;
			//ensure dir is writable, if we can, don't need X permission
			newmode = e2_fs_tw_adjust_dirmode (localpath, statptr, W_OK);
			if (newmode == 0)
			{
				//take a shot at doing the change, anyhow, probably fails
				_e2pt_touch1 (localpath, statptr, user_data);
				//FIXME warn user about failure
				retval = E2TW_FIXME;
			}
			else	//dir can be processed
			{
				if (!_e2pt_touch1 (localpath, statptr, user_data))
					retval = E2TW_FIXME;
				if (newmode != mode)
					e2_fs_chmod (localpath, mode E2_ERR_NONE());	//CHECKME ctime effect
			}
			break;
		case E2TW_SL:
		case E2TW_SLN:
		case E2TW_F:
			if (!_e2pt_touch1 (localpath, statptr, user_data))
				retval = E2TW_FIXME;
			break;
//		case E2TW_NS:	//un-statable item (for which, error is reported upstream)
		default:
			retval = E2TW_STOP;
			break;
	}

	if (retval & E2TW_SKIPSUB)
		user_data->continued_after_problem = TRUE;
	if (retval & E2TW_FIXME)
	{
		user_data->continued_after_problem = TRUE;
		retval &= ~E2TW_FIXME;	//continue after bad item
	}
	return retval;
}
/**
@brief change time(s) of item @a path to values stored in @a statbuf

If invoked on a non-dir, or a dir without recursion, it is processed
here. If recursion is invoked on a dir, a ntfw funtion is invoked.
By that, all nested dir access permissons will be set to include x,
non-dir items will be have their new times set when
they are 'reported'. Finally, here, dirs will be set to their original
permissions (bottom-up order) at the end of the process
Recursive touch works on the host filesystem only.
Links are not affected, their target is processed. (OK ??)
Error messages expect BGL open
@param path localised string, absolute path of item to process
@param statbuf ptr to data struct with times to set
@param recurse TRUE to do recursive change (ignored if @a path is not a dir)

@return TRUE if operation succeeds
*/
static gboolean _e2p_touch (VPATH *localpath, E2_TouchData *data, gboolean recurse)
{
	struct stat statbuf;
	E2_ERR_DECLARE

	if (recurse)
	{
		//confirm whether path is a dir or not
		if (e2_fs_stat (localpath, &statbuf E2_ERR_PTR()))  //look _through_ links
		{
			//abort if we can't find the item
			e2_fs_error_local (_("Cannot get current data for %s"),
				localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
		if (S_ISDIR (statbuf.st_mode))
		{	//recursive touch

			//no processed dirs yet
			data->dirdata = NULL;

			gboolean retval = e2_fs_tw (localpath, _e2_task_twcb_touch, data, -1,
			//flags for fix-DNR, thru-links, this filesystem only, breadth-first
				E2TW_FIXDIR | E2TW_MOUNT E2_ERR_PTR());

			if (data->dirdata != NULL)
			{	//touch and revert dir permissions, LIFO (=bottom-up) order
#ifdef E2_VFS
				VPATH ddata;
				ddata.spacedata = localpath->spacedata;
#endif
				GList *member;
				for (member = data->dirdata; member != NULL; member = member->next)
				{
					E2P_DirEnt *dirfix = member->data;
#ifdef E2_VFS
					ddata.path = dirfix->path;
					if (e2_fs_lstat (&ddata, &statbuf E2_ERR_NONE())  //no link pass-thru
						|| !_e2pt_touch1 (&ddata, &statbuf, data))
#else
					if (e2_fs_lstat (dirfix->path, &statbuf E2_ERR_NONE())  //no link pass-thru
						|| !_e2pt_touch1 (dirfix->path, &statbuf, data))
#endif
						retval = FALSE;
#ifdef E2_VFS
					if (e2_fs_chmod (&ddata, dirfix->mode E2_ERR_PTR())
#else
					if (e2_fs_chmod (dirfix->path, dirfix->mode E2_ERR_PTR())
#endif
						&& E2_ERR_ISNOT (ENOENT))
						e2_fs_error_local (_("Cannot change permissions of %s"),
							localpath E2_ERR_MSGL());
					E2_ERR_CLEAR
					g_free (dirfix->path);
					DEALLOCATE (E2P_DirEnt, dirfix);
				}
				g_list_free (data->dirdata);
			}
//			g_free (data); stacked, not heaped
			if (!retval)
			{
				//FIXME handle error
				E2_ERR_CLEAR
			}
			return retval;
		}
		else //not dir, handle without recurse
			recurse = FALSE;
	}
	if (!recurse)
	{
		//get current times
		if (e2_fs_lstat (localpath, &statbuf E2_ERR_PTR()))  //no link pass-thru
		{
			e2_fs_error_local (_("Cannot get current times of %s"), localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
		return (_e2pt_touch1 (localpath, &statbuf, data));
	}
	return FALSE; //warning prevention only
}
/**
@brief convert strings in @a date_combo and/or @a time_combo to corresponding time variable
This is called only from within callback, BGL closed
@param current label containing string form of item's current date-time
@param date_combo widget with string for new date
@param time_combo widget with string for new time
@param newdt store for new timevalue (set to -1 if problem occurs)

@return TRUE if new time_t is determined successfully
*/
static gboolean _e2p_times_parse_time (GtkWidget *current, GtkWidget *date_combo,
	GtkWidget *time_combo, time_t *newdt)
{
	struct tm tm;
	//clear result structure
	memset (&tm, '\0', sizeof (tm));
	const gchar *date = gtk_entry_get_text (
#ifdef USE_GTK2_14
		GTK_ENTRY (gtk_bin_get_child (GTK_BIN (date_combo)))
#else
		GTK_ENTRY (GTK_BIN (date_combo)->child)
#endif
	);
	const gchar *time = gtk_entry_get_text (
#ifdef USE_GTK2_14
		GTK_ENTRY (gtk_bin_get_child (GTK_BIN (time_combo)))
#else
		GTK_ENTRY (GTK_BIN (time_combo)->child)
#endif
	);
	const gchar *currdate = gtk_label_get_text (GTK_LABEL (current));
	const gchar *currtime = strchr (currdate, ' ') + 1;	//ascii scanning for space char
	const gchar *cp;
	gchar *newvalue;
	if (*date != '\0')
	{
		if (*time != '\0')
			newvalue = g_strconcat (date, " ", time, NULL);
		else
			newvalue = g_strconcat (date, " ", currtime, NULL);
	}
	else if (*time != '\0')
	{
		gchar *freeme = g_strndup (currdate, currtime - currdate - 1);
		newvalue = g_strconcat (freeme, " ", time, NULL);
		g_free (freeme);
	}
	else
	{
		newvalue = g_strdup (currdate);
	}

	cp = strptime (newvalue, "%x %X", &tm);
	if (cp != NULL && *cp == '\0')	//all of the string was processed
	{
		tm.tm_isdst = -1;	//don't correct for daylight saving
		*newdt = mktime (&tm);	//may be set to -1
	}
	else
		*newdt = (time_t) -1;

	if (*newdt == (time_t) -1)
	{
		gchar *msg = g_strdup_printf (_("Cannot interpret date-time %s"), newvalue);
		e2_output_print_error (msg, TRUE);
	}
	g_free (newvalue);
	return (*newdt != (time_t) -1);
}
/**
@brief handle apply or apply-to-all button press
Converts all entered date-time strings to calendar values
If there's a problem, it may set a flag to produce a cancel
response for the dialog as a whole

@param widget UNUSED the clicked button widget
@param rt pointer to dialog data struct

@return
*/
static void _e2p_times_apply_cb (GtkWidget *widget, E2_TimesDlgRuntime *rt)
{
	if (rt->permission)
	{
		gboolean success = TRUE;	//default condition-flags
		rt->real_choice = OK;
//		GtkWidget *entry;
		const gchar *entrytext;
		NEEDCLOSEBGL
		//store any valid new dates/times
		if (
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->set_mtime_button))
#else
			GTK_TOGGLE_BUTTON (rt->set_mtime_button)->active
#endif
		)
		{
			success = _e2p_times_parse_time (rt->mcurrent, rt->mdate_combo,
				rt->mtime_combo, &rt->data->mtime);
			if (success)
			{
/*as dialog is destroyed each iteration, no point in deferring backup until later
				//update combo history
				entry =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->mdate_combo));
#else
				GTK_BIN (rt->mdate_combo)->child;
#endif
				entrytext = gtk_entry_get_text (GTK_ENTRY (entry));
				if (entrytext != NULL && *entrytext != '\0')
					e2_combobox_prepend_history (rt->mdate_combo, entrytext, 10, FALSE);
				entry =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->mtime_combo));
#else
				GTK_BIN (rt->mtime_combo)->child;
#endif
				entrytext = gtk_entry_get_text (GTK_ENTRY (entry));
				if (entrytext != NULL && *entrytext != '\0')
					e2_combobox_prepend_history (rt->mtime_combo, entrytext, 10, FALSE);
*/
				entrytext = gtk_entry_get_text (
#ifdef USE_GTK2_14
					GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->mdate_combo)))
#else
					GTK_ENTRY (GTK_BIN (rt->mdate_combo)->child)
#endif
				);
				if (*entrytext != '\0')
					e2_list_update_history (&mdate_history, entrytext, NULL, 0, FALSE);
				entrytext = gtk_entry_get_text (
#ifdef USE_GTK2_14
					GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->mtime_combo)))
#else
					GTK_ENTRY (GTK_BIN (rt->mtime_combo)->child)
#endif
				);
				if (*entrytext != '\0')
					e2_list_update_history (&mtime_history, entrytext, NULL, 0, FALSE);
			}
			else
				rt->real_choice = NO;	//signal parse-failure
		}
		else
			rt->data->mtime = (time_t) -1;

		if (success &&
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->set_atime_button))
#else
			GTK_TOGGLE_BUTTON (rt->set_atime_button)->active
#endif
		)
		{
			success = _e2p_times_parse_time (rt->acurrent, rt->adate_combo,
				rt->atime_combo, &rt->data->atime);
			if (success)
			{
/*				//update combo history
				entry =
#ifdef USE_GTK2_14
					gtk_bin_get_child (GTK_BIN (rt->adate_combo));
#else
					GTK_BIN (rt->adate_combo)->child;
#endif
				entrytext = gtk_entry_get_text (GTK_ENTRY (entry));
				if (entrytext != NULL && *entrytext != '\0')
					e2_combobox_prepend_history (rt->adate_combo, entrytext, 10, FALSE);
				entry =
#ifdef USE_GTK2_14
					gtk_bin_get_child (GTK_BIN (rt->atime_combo));
#else
					GTK_BIN (rt->atime_combo)->child;
#endif
				entrytext = gtk_entry_get_text (GTK_ENTRY (entry));
				if (entrytext != NULL && *entrytext != '\0')
					e2_combobox_prepend_history (rt->atime_combo, entrytext, 10, FALSE);
*/
				entrytext = gtk_entry_get_text (
#ifdef USE_GTK2_14
					GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->adate_combo)))
#else
					GTK_ENTRY (GTK_BIN (rt->adate_combo)->child)
#endif
				);
				if (*entrytext != '\0')
					e2_list_update_history (&adate_history, entrytext, NULL, 0, FALSE);
				entrytext = gtk_entry_get_text (
#ifdef USE_GTK2_14
					GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->atime_combo)))
#else
					GTK_ENTRY (GTK_BIN (rt->atime_combo)->child)
#endif
				);
				if (*entrytext != '\0')
					e2_list_update_history (&atime_history, entrytext, NULL, 0, FALSE);
			}
			else
				rt->real_choice = NO;	//signal parse-failure
		}
		else
			rt->data->atime = (time_t) -1;

		if (success &&
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->set_ctime_button))
#else
			GTK_TOGGLE_BUTTON (rt->set_ctime_button)->active
#endif
		)
		{
			success = _e2p_times_parse_time (rt->ccurrent, rt->cdate_combo,
				rt->ctime_combo, &rt->data->ctime);
			if (success)
			{	//warn about system clock changes
				gchar *prompt =
				_("Changing 'ctime' requires temporary changes to the system clock."
				  " That is normally unwise, as typically, other things rely on"
				  " system time.");
				DialogButtons choice = e2_dialog_warning (prompt, NULL);
				if (choice != OK)
				{
					success = FALSE;
					rt->data->ctime = (time_t) -1;
				}
			}
			else
				rt->real_choice = NO;	//signal parse-failure

			if (success)
			{
/*				//update combo history
				entry =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->cdate_combo));
#else
				GTK_BIN (rt->cdate_combo)->child;
#endif
				entrytext = gtk_entry_get_text (GTK_ENTRY (entry));
				if (entrytext != NULL && *entrytext != '\0')
					e2_combobox_prepend_history (rt->cdate_combo, entrytext, 10, FALSE);
				entry =
#ifdef USE_GTK2_14
				gtk_bin_get_child (GTK_BIN (rt->ctime_combo));
#else
				GTK_BIN (rt->ctime_combo)->child;
#endif
				entrytext = gtk_entry_get_text (GTK_ENTRY (entry));
				if (entrytext != NULL && *entrytext != '\0')
					e2_combobox_prepend_history (rt->ctime_combo, entrytext, 10, FALSE);
*/
				entrytext = gtk_entry_get_text (
#ifdef USE_GTK2_14
					GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->cdate_combo)))
#else
					GTK_ENTRY (GTK_BIN (rt->cdate_combo)->child)
#endif
				);
				if (*entrytext != '\0')
					e2_list_update_history (&cdate_history, entrytext, NULL, 0, FALSE);
				entrytext = gtk_entry_get_text (
#ifdef USE_GTK2_14
					GTK_ENTRY (gtk_bin_get_child (GTK_BIN (rt->ctime_combo)))
#else
					GTK_ENTRY (GTK_BIN (rt->ctime_combo)->child)
#endif
				);
				if (*entrytext != '\0')
					e2_list_update_history (&ctime_history, entrytext, NULL, 0, FALSE);
			}
		}
		else
			rt->data->ctime = (time_t) -1;

		NEEDOPENBGL

		if (success)
		{	//anything valid to change ?
			if (rt->data->mtime == (time_t) -1
				&& rt->data->atime == (time_t) -1
				&& rt->data->ctime == (time_t) -1 )
				rt->real_choice = CANCEL;
			else
			{
				rt->real_choice = OK; //the entered choice stands confirmed
				if (rt->recurse_button != NULL)
					*rt->recurse =
#ifdef USE_GTK2_14
						gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_button));
#else
						GTK_TOGGLE_BUTTON (rt->recurse_button)->active;
#endif
			}
//			return;
		}
		else
			if (rt->real_choice != NO)
			//error message already printed by the date interpeter
				rt->real_choice = CANCEL;
	}
	else
	{  //no permisson, the ok click was pointless ...
	  rt->real_choice = CANCEL;
	}
}
/**
@brief create comboboxentry widget for the dialog
@param history history list for the widget (static for the session)
@return the widget
*/
static GtkWidget *_e2p_times_create_combo (GList *history)
{
	//func MAY need BGL closed on gtk2
	GtkWidget *combo = e2_combobox_get ((ActivateFunc)NULL, NULL, &history,
		E2_COMBOBOX_HAS_ENTRY | E2_COMBOBOX_NO_AUTO_HISTORY |
		E2_COMBOBOX_FOCUS_ON_CHANGE | E2_COMBOBOX_CYCLE_HISTORY);
	//show the last-used value, if any
	if (e2_combobox_has_history (GTK_COMBO_BOX (combo)))
		e2_combobox_set_active (combo, 0);
//	gtk_entry_set_activates_default (GTK_ENTRY
//#ifdef USE_GTK2_14
//	(gtk_bin_get_child (GTK_BIN (combo))),
//#else
//	(GTK_BIN (combo)->child),
//#endif
//	TRUE);
//	gtk_widget_set_size_request (combo,  40, -1);
	return combo;
}
/**
@brief create and run time-change dialog
Expects BGL open/off.
@param local_dir path of dir containing item to process, localised string with trailer
@param spacedata pointer to space data for active pane
@param info pointer to selection data struct for the item being processed
@param recurse_ret pointer to store for user's choice for a recursive change
@param statbuf pointer to filesystem data struct for the item being processed
@param multi TRUE if the change is part of a multi-item selection

@return enumerator corresponing to the clicked dialog button
*/
static DialogButtons _e2p_times_dialog_run (gchar *local_dir,
#ifdef E2_VFS
	PlaceInfo *spacedata,
#endif
	E2_SelectedItemInfo *info, gboolean *recurse_ret, E2_TouchData *data, gboolean multi)
{
	DialogButtons choice;
	GtkWidget *times_dialog;
	GtkWidget *dialog_vbox, *sub_vbox;
	GtkWidget *hbox;
	GtkWidget *table;
	struct stat statbuf;
	struct tm *tm_ptr;
	gchar date_string[32];
	E2_TimesDlgRuntime rt;

	gchar *local = e2_utils_strcat (local_dir, info->filename);
#ifdef E2_VFS
	VPATH tdata = { local, spacedata };
	if (e2_fs_lstat (&tdata, &statbuf E2_ERR_NONE()))
#else
	if (e2_fs_lstat (local, &statbuf E2_ERR_NONE()))
#endif
	{
		g_free (local);
		return CANCEL;
	}
	E2_ERR_DECLARE
	rt.permission =
#ifdef E2_VFS
	e2_fs_check_write_permission (&tdata E2_ERR_PTR());
#else
	e2_fs_check_write_permission (local E2_ERR_PTR());
#endif
//FIXME handle error
	gboolean thisis_dir =
#ifdef E2_VFS
	e2_fs_is_dir3 (&tdata E2_ERR_PTR());
#else
	e2_fs_is_dir3 (local E2_ERR_PTR());
#endif
//FIXME handle error
	g_free (local);
	//copy pointers
	rt.data = data;
	rt.recurse = recurse_ret;

	CLOSEBGL
	times_dialog = e2_dialog_create (NULL, NULL, _("times"), DUMMY_RESPONSE_CB, NULL);
	OPENBGL
	dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (times_dialog));
#else
		GTK_DIALOG (times_dialog)->vbox;
#endif
	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);

#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	gtk_box_pack_start (GTK_BOX (dialog_vbox), hbox, TRUE, TRUE, E2_PADDING); //top, bottom padding
	gchar *labeltype = (thisis_dir) ? _("Directory name") : _("Filename") ;
	gchar *utf = F_DISPLAYNAME_FROM_LOCALE (info->filename);
	gchar *label_text = g_strdup_printf ("%s: <b>%s</b>", labeltype, utf);
	e2_widget_add_mid_label (hbox, label_text, 0, TRUE, E2_PADDING); //L, R padding
	F_FREE (utf, info->filename);
	g_free (label_text);

#ifdef USE_GTK3_0
	sub_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING);
#else
	sub_vbox = gtk_vbox_new (FALSE, E2_PADDING);
#endif
	gtk_box_pack_start (GTK_BOX (dialog_vbox), sub_vbox, TRUE, TRUE, E2_PADDING_LARGE);

	gint rows = (thisis_dir) ? 5 : 4;
	table = e2_widget_add_table (sub_vbox, rows, 5, FALSE, TRUE, E2_PADDING);  //4 rows, 5 cols, !homogen, expand
#ifdef USE_GTK3_2
	gtk_grid_set_column_spacing (GTK_GRID (table), E2_PADDING);
#else
	gtk_table_set_col_spacings (GTK_TABLE (table), E2_PADDING);
#endif
	e2_widget_add_mid_label_to_table (table, _("Current values"), 0.5, 1,2,0,1);
	e2_widget_add_mid_label_to_table (table, _("New date"),		  0.5, 2,3,0,1);
	e2_widget_add_mid_label_to_table (table, _("New time"),		  0.5, 3,4,0,1);

	e2_widget_add_mid_label_to_table (table, _("Accessed"),		    0.0, 0,1,1,2);
	e2_widget_add_mid_label_to_table (table, _("Content Modified"), 0.0, 0,1,2,3);
	e2_widget_add_mid_label_to_table (table, _("Inode Changed"),    0.0, 0,1,3,4);

	tm_ptr = localtime (&statbuf.st_atime);
	strftime (date_string, sizeof (date_string), "%x %X", tm_ptr);
	utf = e2_utf8_from_locale (date_string);
	rt.acurrent = e2_widget_add_mid_label_to_table (table, utf, 0.0, 1,2,1,2);
	g_free (utf);
	tm_ptr = localtime (&statbuf.st_mtime);
	strftime (date_string, sizeof (date_string), "%x %X", tm_ptr);
	utf = e2_utf8_from_locale (date_string);
	rt.mcurrent = e2_widget_add_mid_label_to_table (table, utf, 0.0, 1,2,2,3);
	g_free (utf);
	tm_ptr = localtime (&statbuf.st_ctime);
	strftime (date_string, sizeof (date_string), "%x %X", tm_ptr);
	utf = e2_utf8_from_locale (date_string);
	rt.ccurrent = e2_widget_add_mid_label_to_table (table, utf, 0.0, 1,2,3,4);
	g_free (utf);
	rt.adate_combo = _e2p_times_create_combo (adate_history);
	rt.mdate_combo = _e2p_times_create_combo (mdate_history);
	rt.cdate_combo = _e2p_times_create_combo (cdate_history);
	rt.atime_combo = _e2p_times_create_combo (atime_history);
	rt.mtime_combo = _e2p_times_create_combo (mtime_history);
	rt.ctime_combo = _e2p_times_create_combo (ctime_history);
#ifdef USE_GTK3_2
	gtk_grid_attach ((GtkGrid*)table, rt.adate_combo, 2,1,1,1);
	gtk_grid_attach ((GtkGrid*)table, rt.mdate_combo, 2,2,1,1);
	gtk_grid_attach ((GtkGrid*)table, rt.cdate_combo, 2,3,1,1);
	gtk_grid_attach ((GtkGrid*)table, rt.atime_combo, 3,1,1,1);
	gtk_grid_attach ((GtkGrid*)table, rt.mtime_combo, 3,2,1,1);
	gtk_grid_attach ((GtkGrid*)table, rt.ctime_combo, 3,3,1,1);
#else
	gtk_table_attach_defaults ((GtkTable*)table, rt.adate_combo, 2,3,1,2);
	gtk_table_attach_defaults ((GtkTable*)table, rt.mdate_combo, 2,3,2,3);
	gtk_table_attach_defaults ((GtkTable*)table, rt.cdate_combo, 2,3,3,4);
	gtk_table_attach_defaults ((GtkTable*)table, rt.atime_combo, 3,4,1,2);
	gtk_table_attach_defaults ((GtkTable*)table, rt.mtime_combo, 3,4,2,3);
	gtk_table_attach_defaults ((GtkTable*)table, rt.ctime_combo, 3,4,3,4);
#endif

	rt.set_atime_button = e2_button_add_toggle_to_table (table,
		_("_Set"), FALSE, NULL, NULL, 4,5,1,2);//gint left, gint right, gint top, gint bottom
	rt.set_mtime_button = e2_button_add_toggle_to_table (table,
		_("_Set"), FALSE, NULL, NULL, 4,5,2,3 );
	rt.set_ctime_button = e2_button_add_toggle_to_table (table,
		_("_Set"), FALSE, NULL, NULL, 4,5,3,4);
	if (thisis_dir)  //no longer do this for 'multi'
		rt.recurse_button = e2_button_add_toggle_to_table (table, _("R_ecurse:"),
			FALSE, NULL, NULL, 4,5,4,5 );
	else
		rt.recurse_button = NULL;

	if (!rt.permission) //no right to change item times
	{
		gtk_widget_set_sensitive (rt.mdate_combo, FALSE);
		gtk_widget_set_sensitive (rt.adate_combo, FALSE);
		gtk_widget_set_sensitive (rt.cdate_combo, FALSE);
		gtk_widget_set_sensitive (rt.mtime_combo, FALSE);
		gtk_widget_set_sensitive (rt.atime_combo, FALSE);
		gtk_widget_set_sensitive (rt.ctime_combo, FALSE);

		gtk_widget_set_sensitive (rt.set_mtime_button, FALSE);
		gtk_widget_set_sensitive (rt.set_atime_button, FALSE);
		gtk_widget_set_sensitive (rt.set_ctime_button, FALSE);
		gtk_widget_set_sensitive (rt.recurse_button, FALSE);

		e2_dialog_setup_auth (dialog_vbox);
	}
	else if (getuid () != 0)
	{
		//only root can alter system time, which is needed to change ctime
		gtk_widget_set_sensitive (rt.ctime_combo, FALSE);
		gtk_widget_set_sensitive (rt.cdate_combo, FALSE);
		gtk_widget_set_sensitive (rt.set_ctime_button, FALSE);
	}

	//block until user responds
	E2_DialogFlags flags = E2_DIALOG_BLOCKED;

	// add buttons in the order that they will appear
	GtkWidget *btn;
	E2_Button no_btn;
	if (multi)
	{
		e2_dialog_set_negative_response (times_dialog, E2_RESPONSE_NOTOALL);
		e2_dialog_add_defined_button (times_dialog, &E2_BUTTON_CANCEL);
		btn = e2_dialog_add_custom_button (times_dialog, &E2_BUTTON_APPLYTOALL,
			FALSE, E2_BUTTON_APPLYTOALL.tip, _e2p_times_apply_cb, &rt);
		if (!rt.permission)
			gtk_widget_set_sensitive (btn, FALSE);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_SKIP);
		flags |= E2_DIALOG_MULTI;
	}
	else
	{
		e2_dialog_set_negative_response (times_dialog, GTK_RESPONSE_NO);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);
	}

	e2_dialog_add_defined_button (times_dialog, &no_btn);
	btn = e2_dialog_add_custom_button (times_dialog, &E2_BUTTON_APPLY, TRUE,
		NULL, _e2p_times_apply_cb, &rt);
	if (!rt.permission)
		gtk_widget_set_sensitive (btn, FALSE);

	CLOSEBGL

	e2_dialog_setup (times_dialog, app.main_window);
	do
	{
		choice = e2_dialog_run (times_dialog, NULL, flags);
	} while (rt.real_choice == NO); //after date/time parse error, keep trying
/*as dialog is destroyed each iteration, no gain in deferring this from the
  response callback
	//backup combo histories before dialog destruction
	e2_combobox_save_history (rt.mdate_combo, &mdate_history);
	e2_combobox_save_history (rt.mtime_combo, &mtime_history);
	e2_combobox_save_history (rt.adate_combo, &adate_history);
	e2_combobox_save_history (rt.atime_combo, &atime_history);
	e2_combobox_save_history (rt.cdate_combo, &cdate_history);
	e2_combobox_save_history (rt.ctime_combo, &ctime_history);
*/
	gtk_widget_destroy (times_dialog);
	OPENBGL
	if (rt.real_choice == CANCEL)
		choice = CANCEL;
	return choice;
}
/**
@brief show/change time(s) of selected items in active pane

The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_task_times (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_TIMESET, art, from,
		_e2p_task_timesQ, e2_task_refresh_lists));
}
static gboolean _e2p_task_timesQ (E2_ActionTaskData *qed)
{
	//printd (DEBUG, "task: times");
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
	GString *path = g_string_sized_new (PATH_MAX+NAME_MAX);
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	gboolean multisrc = names->len > 1;
	gboolean all = FALSE;
	E2_TouchData data;
#ifdef E2_VFS
	VPATH sdata;
	sdata.spacedata = qed->currspace;
#endif

/* out-of-loop setup = FIXME
	GtkWidget *dialog;
	dialog = e2_permissions_dialog_setup (info, &recurse);
	e2_dialog_add_buttons_simple (dialog, buttons ..., NULL);
*/

	e2_task_advise ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, times task");
#endif
	e2_filelist_disable_refresh ();

	for (count=0; count < names->len; count++, iterator++)
	{
		DialogButtons choice;
 		gboolean permission;
		gboolean recurse;
		//".." entries filtered when names compiled
		g_string_printf (path, "%s%s", curr_local, (*iterator)->filename); //separator comes with dir
#ifdef E2_VFS
		sdata.path = path->str;
#endif
		if (all)
		{
			//check if we have permission to change this item
			//NB this tests _all_ w permissions, touch not so sophisticated ...
#ifdef E2_VFS
			permission = e2_fs_check_write_permission (&sdata E2_ERR_NONE());
#else
			permission = e2_fs_check_write_permission (path->str E2_ERR_NONE());
#endif
			if (!permission)
			{
#ifdef E2_VFS
				sdata.path = (*iterator)->filename;
#endif
				e2_fs_error_simple (
					_("You do not have authority to change time(s) for %s"),
#ifdef E2_VFS
				&sdata);
#else
				(*iterator)->filename);
#endif
			}
			choice = (permission) ? OK : CANCEL;
		}
		else
		{
			recurse = FALSE;
			data.continued_after_problem = FALSE;
			data.mtime = (time_t) -1;
			data.atime = (time_t) -1;
			data.ctime = (time_t) -1;
			data.dirdata = NULL;
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, times dialog");
#endif
			e2_filelist_enable_refresh ();  //allow updates while we wait

			*qed->status = E2_TASK_PAUSED;
			choice = _e2p_times_dialog_run (curr_local,
#ifdef E2_VFS
			qed->currspace,
#endif
			*iterator, &recurse, &data, multisrc);
			*qed->status = E2_TASK_RUNNING;

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, permissions dialog");
#endif
			e2_filelist_disable_refresh ();
		}

		switch (choice)
		{
		  case YES_TO_ALL:
			all = TRUE;
			choice = OK;
		  case OK:
#ifdef E2_INCLIST
# ifdef E2_VFS
			if (_e2p_touch (&sdata, &data, recurse))
# else
			if (_e2p_touch (path->str, &data, recurse))
# endif
			{
				//FIXME update line in treeview
			}
#else
# ifdef E2_FAM
#  ifdef E2_VFS
			_e2p_touch (&sdata, &data, recurse);
#  else
			_e2p_touch (path->str, &data, recurse);
#  endif
# else
#  ifdef E2_VFS
			if (_e2p_touch (&sdata, &data, recurse))
			{
				sdata.path = curr_local;
				//make the file-list refresher notice successful change
				e2_fs_utime (&sdata, NULL E2_ERR_NONE());
			}
#  else
			if (_e2p_touch (path->str, &data, recurse))
			//make the file-list refresher notice successful change
				e2_fs_utime (curr_local, NULL E2_ERR_NONE());
#  endif
# endif
#endif
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

	//FIXME E2_INCLIST - just do other view if it's same
	e2_window_clear_status_message ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, permissions task");
#endif
	e2_filelist_enable_refresh ();
	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(6),_("timeset"),_e2p_task_times,
		_("Change _times.."),
		_("Change any of the time properties of selected items"),
		"plugin_"ANAME E2ICONTB)
}

/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)
	if (ret)
	{
		if (mdate_history != NULL)
			e2_list_free_with_data (&mdate_history);
		if (mtime_history != NULL)
			e2_list_free_with_data (&mtime_history);
		if (adate_history != NULL)
			e2_list_free_with_data (&adate_history);
		if (atime_history != NULL)
			e2_list_free_with_data (&atime_history);
		if (cdate_history != NULL)
			e2_list_free_with_data (&cdate_history);
		if (ctime_history != NULL)
			e2_list_free_with_data (&ctime_history);
	}
	return ret;
}
