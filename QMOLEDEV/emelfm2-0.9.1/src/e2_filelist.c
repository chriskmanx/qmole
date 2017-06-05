/* $Id: e2_filelist.c 3026 2014-01-22 23:30:42Z tpgww $

Copyright (C) 2004-2014 tooar <tooar@emelfm2.net>.

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
@file src/e2_filelist.c
@brief directory content liststore functions

This file contains functions related to creation, filling and emptying a
liststore of directory content data, and funcs for the associated model.
Also filtering of store content, and other related utilties.
*/

/**
\page refresh filelist refreshing

\section automatic automatic refreshing

Automatic refreshing of filelists occurs if the value of configuration option
"auto-refresh" is TRUE.

Core infrastructure is a timer - app.timers[DIRTYCHECK_T]. This is used for either
'default' change-monitoring (a polling process), or enhanced monitoring (using
gamin, or a relevant kernel capability) if such is built-in and actually working.

The default mechanism for this is periodic polling of the mtime and ctime of
the directories currently displayed in the filelists.

If enhanced monitoring is used, suitable polling is handled by the relevant backend.

The kernel-backend, or the poller (via e2_fs_FAM_check_dirty()), sets dirty-flag(s)
for filelist(s), as appropriate. Then for both panes:
  If its dirty-flag is set:
    Clear its dirty-flag, ready for downstream use again
    If function-argument (userdata) is non-NULL (the usual for direct calls)
		or a repeat-count (via _e2_filelist_refresh_ok()) is suitable:
      Set refresh-requested flag for the pane
Then if either refresh-requested flag is now set (maybe from elsewhere, see below):
  If timer REFRESHBEGIN_T is active, abort it
  Start timer REFRESHBEGIN_T with callback to _e2_filelist_refresh_manage()

_e2_filelist_refresh_manage() polls both panes' refresh-requested flags, and, if
and when not blocked by some other process that affects the pane's content,
initiates (and joins to) a thread to refresh the relevant filelist. This process
iterates until all requests have been handled, so there can be > 1 refresh before
the function returns.

With enhanced monitoring, the same interface is used to gather information about
filelists and the config file, if the latter is being monitored. However any
config-file-change is merely flagged, ready for processing by the corresponding
process.

\section manual manual refreshing

Either or both panes' refresh-requested flag can be set at any time via a call to
e2_filelist_request_refresh(). If the immediate flag is not set, that request will
be acted upon next time e2_filelist_check_dirty() is called, manually or as a
timer callback.

e2_filelist_check_dirty() can be called manually, usually with a non-NULL argument.
This will check for and refresh pane(s) flagged as 'dirty' or 'refresh-requested'.

/section blocking refresh blocking

Refreshing of either or both panes is contstrained by corresponding ref-counts,
which can be increased or decreased by calls to e2_filelist_[dis|en]able_refresh()
and/or to e2_filelist_[dis|en]able_one_refresh()

Refreshing of either pane is blocked while either pane is still being refreshed
as a result of a prior update

\section config configuration file changes

Automatic refreshing of config-file changes happens if the value of
configuration option "auto-refresh-config" is TRUE.

Core infrastructure is a timer - app.timers[CONFIG_T]. It has a longer intervals
than the filelist timer. It is used for either 'default' change-monitoring, or
enhanced monitoring (using gamin, or a relevant kernel capability) if such is
built-in and actually working.

The default mechanism is periodic polling of the mtime of the config file (the
default or as specified in a startup parameter).

If enhanced monitoring is used, suitable polling is handled by the relevant backend.
*/

#include "e2_filelist.h"
#include <string.h>
#include <sys/time.h>
#include <langinfo.h>
#include <pwd.h>
#include <time.h>
#include <grp.h>
#include "e2_option.h"
#include "e2_task.h"
#include "e2_dialog.h"

//#ifdef E2_NEWREFRESH
typedef enum
{
	REFRESH_ADD = 0,
	REFRESH_DELETE,	//unused
	REFRESH_CHANGE,
	REFRESH_KEEP,
} E2_RefreshMode;

//one of these for each entry in a new list of FileInfo's created to check
//against current FileInfo's in a view
typedef struct _E2_RefreshInfo
{
	guint newindx;	//index of row in changes liststore
	guint oldindx;	//index of row in current liststore (ex. when adding)
	E2_RefreshMode oldmode; //what to do with that row
	gboolean selected;
} E2_RefreshInfo;
//#endif

static gpointer _e2_filelist_refresh_store (ViewInfo *view);

#ifdef E2_SELTXT_RECOLOR
extern GdkColor selectedtext;
#endif

#ifdef E2_FAM_DNOTIFY
#include <signal.h>
extern sigset_t dnotify_signal_set;
#endif

//filelist-needs-refresh flags, used by timers and threads, always do atomic access
#ifdef E2_FAM_KERNEL
volatile
#endif
 gint p1dirty = 0;
#ifdef E2_FAM_KERNEL
volatile
#endif
 gint p2dirty = 0;
//refresh deferral counters
static gint pane1repeats = 0;
static gint pane2repeats = 0;
//for managing timer suspension
time_t last_work_time = 0;

extern
#ifdef E2_FAM_KERNEL
 volatile
#endif
 gint cfgdirty;
extern volatile gint cfg_refresh_refcount;
/**
@brief "event" signal callback which resumes polling for whether a refresh is needed
@param widget pointer to main-window widget
@param UNUSED event pointer to event data
@param userdata parameter specified when callback was connected, helps to identify the signal

@return FALSE always, so the event will propogate
*/
gboolean e2_filelist_repoll (GtkWidget *widget, GdkEvent *event,
	gpointer userdata)
{
	g_signal_handlers_disconnect_by_func ((gpointer)widget,
		e2_filelist_repoll, userdata);
	last_work_time = time (NULL);
	printd (DEBUG, "Resume operation of DIRTYCHECK_T timer");
#ifdef E2_FAM_KERNEL
	if (app.monitor_type == E2_MONITOR_DEFAULT)
	{
#endif
		app.timers[DIRTYCHECK_T] =
#ifdef USE_GLIB2_14
			g_timeout_add_seconds (E2_FILESCHECK_INTERVAL_S,
#else
			g_timeout_add (E2_FILESCHECK_INTERVAL,
#endif
				(GSourceFunc) e2_filelist_check_dirty, NULL);
#ifdef E2_FAM_KERNEL
	}
	else
		e2_fs_FAM_resume ();
#endif
	return FALSE;
}
/**
@brief determine whether pane defined by @a pane is the active one
@param pane enumerator for pane to be checked
@return TRUE if @a pane is the active one
*/
static gboolean _e2_filelist_check_active_pane (E2_ListChoice pane)
{
	gboolean active;
	switch (pane)
	{
		case PANE1:
			active = (curr_view == &app.pane1.view);
			break;
		case PANE2:
			active = (curr_view == &app.pane2.view);
			break;
		case PANEINACTIVE:
			active = FALSE;
			break;
		default:
			active = TRUE;
			break;
	}
	return active;
}
/**
@brief disable refreshing of a single pane filelist
This is mainly intended to block changes of filesystem CWD when a process is
working in a displayed directory
@param pane enumerator for pane to be blocked
@return
*/
void e2_filelist_disable_one_refresh (E2_ListChoice pane)
{
	if (_e2_filelist_check_active_pane (pane))
		g_atomic_int_add (&curr_view->listcontrols.refresh_refcount, 1);
	else
		g_atomic_int_add (&other_view->listcontrols.refresh_refcount, 1);
}
/**
@brief enable refreshing of a single pane filelist

@param pane enumerator for pane to be unblocked

@return
*/
void e2_filelist_enable_one_refresh (E2_ListChoice pane)
{
	if (_e2_filelist_check_active_pane (pane))
	{
		if (g_atomic_int_exchange_and_add (&curr_view->listcontrols.refresh_refcount, -1) < 1)
			g_atomic_int_set (&curr_view->listcontrols.refresh_refcount, 0);
	}
	else
	{
		if (g_atomic_int_exchange_and_add (&other_view->listcontrols.refresh_refcount, -1) < 1)
			g_atomic_int_set (&other_view->listcontrols.refresh_refcount, 0);
	}
}
/**
@brief disable -refresh action

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE always
*/
gboolean e2_filelist_disable_refresh_action (gpointer from, E2_ActionRuntime *art)
{
	e2_filelist_disable_refresh ();
	return TRUE;
}
/**
@brief enable -refresh action

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE always
*/
gboolean e2_filelist_enable_refresh_action (gpointer from, E2_ActionRuntime *art)
{
	e2_filelist_enable_refresh ();
	return TRUE;
}
/**
@brief reset polling of config data and pane filelists

@return
*/
void e2_filelist_reset_refresh (void)
{
	g_atomic_int_set (&app.pane1.view.listcontrols.refresh_refcount, 0);
	g_atomic_int_set (&app.pane2.view.listcontrols.refresh_refcount, 0);
	g_atomic_int_set (&cfg_refresh_refcount, 0);
}
/* These functions must encapsulate any instructions that access the FileInfo
structs produced when a selection is interrogated, because a refresh in the
middle of such an operation would free FileInfo structs as a side-effect.
*/
/**
@brief  disable refreshing of pane filelists and config data
Increases all refresh ref-counts, which will prevent any refresh from happening
@return
*/
void e2_filelist_disable_refresh (void)
{
	if (e2_option_bool_get ("auto-refresh"))
	{
		g_atomic_int_add (&curr_view->listcontrols.refresh_refcount, 1);
		g_atomic_int_add (&other_view->listcontrols.refresh_refcount, 1);
	}
	//same as e2_option_disable_config_checks()
	if (e2_option_bool_get ("auto-refresh-config"))
		g_atomic_int_add (&cfg_refresh_refcount, 1);
}
/**
@brief re-enable polling of pane filelists and config data
Decreases all ref-counts, which will enable refresh for any that are now 0
@return
*/
void e2_filelist_enable_refresh (void)
{
	if (e2_option_bool_get ("auto-refresh"))
	{
		if (g_atomic_int_exchange_and_add (&curr_view->listcontrols.refresh_refcount, -1) < 1)
			g_atomic_int_set (&curr_view->listcontrols.refresh_refcount, 0);
		if (g_atomic_int_exchange_and_add (&other_view->listcontrols.refresh_refcount, -1) < 1)
			g_atomic_int_set (&other_view->listcontrols.refresh_refcount, 0);
	}
	//same as e2_option_enable_config_checks()
	if (e2_option_bool_get ("auto-refresh-config"))
	{
		if (g_atomic_int_exchange_and_add (&cfg_refresh_refcount, -1) < 1)
			g_atomic_int_set (&cfg_refresh_refcount, 0);
	}
}
/**
@brief idle function to run the refresh-hooklist of @s view
This happens after the end of the refresh timer callback, so in principle,
another refresh, or a cd, could start before the hook funcs are called.
So they should check for those races, and respond accordingly.
@param view pointer to view data struct

@return FALSE to abort the source
*/
static gboolean _e2_filelist_propogate_refresh (ViewInfo *view)
{
	e2_hook_list_run (&view->hook_refresh, view);
	return FALSE;
}
/**
@brief timer destroy function

@param data pointerised index of the timer that ended

@return
*/
void e2_filelist_timer_shutdown (gpointer data)
{
	guint index = GPOINTER_TO_UINT (data);
	app.timers[index] = 0;
#ifdef E2_REFRESH_DEBUG
	if (index == REFRESHBEGIN_T)
		printd (DEBUG, "timer REFRESHBEGIN_T id set to 0");
#endif
}

#if 1
/**
@brief thread-function which performs filelist refresh if it can
If refreshing is blocked, there is no deferral
@param view pointer to data struct for the filelist to be refreshed
@return NULL always
*/
static gpointer _e2_filelist_refresh_view (ViewInfo *view)
{
	e2_utils_block_thread_signals ();

	if (g_atomic_int_get (&view->listcontrols.refresh_refcount) == 0)
	{	//this refresh is not disabled
		if (g_atomic_int_compare_and_exchange (&view->listcontrols.refresh_requested, 1, 0))
		{
			gboolean hook = FALSE;
			gboolean ready;
retest:
			ready = !(
			g_atomic_int_get (&view->listcontrols.refresh_working) ||	//or busy
			g_atomic_int_get (&view->listcontrols.cd_working)
#ifdef E2_STATUS_BLOCK
			|| g_atomic_int_get (&app.status_working)
#endif
			);

			if (ready)
			{
#ifdef E2_VFSTMP
				//FIXME dir when not mounted local
#else
				printd (DEBUG, "accepting request to refresh %s", view->dir);
#endif
				//refresh function expects BGL open
				if (_e2_filelist_refresh_store (view) == GINT_TO_POINTER(1))
				{
					hook = TRUE;	//send it downstream
					//maybe again/now/still want to do this pane
					if (g_atomic_int_get (&view->listcontrols.refresh_refcount) == 0
						&&
						g_atomic_int_compare_and_exchange (&view->listcontrols.refresh_requested, 1, 0))
					{
						printd (DEBUG, "Sequential refresh requested for %s", view->dir);
						usleep (10000);
						goto retest;
					}
				}
			}
			//initiate refresh hooklist here, not downstream, so that refreshes are batched,
			//and in idle to avoid any UI impact
			//(i.e. small risk of cd / another refresh starting before hook is called)
			if (hook)
				g_idle_add ((GSourceFunc)_e2_filelist_propogate_refresh, view);
		}
	}

	return NULL;
}
#else //original approach to refreshing
/**
@brief timer callback function which refreshes filelist(s) as appropriate, as soon as any block is gone

@param unused_data UNUSED pointer set when timer was initiated (actually, it has the timer index, REFRESHBEGIN_T)

@return FALSE when there's nothing left to refresh or more waiting is needed
*/
static gboolean _e2_filelist_refresh_manage (gpointer unused_data)
{
	ViewInfo *cv, *ov;
	pthread_t thisID;
	gpointer result;
	gboolean cvreq, ovreq, cvdone, ovdone, cvhook, ovhook;
	gint ovsave;

	LISTS_LOCK
	//log these, in case active pane is changed during this process
	cv = curr_view;
	ov = other_view;
	//ensure that refresh is started if either pane needs it now
	cvreq = g_atomic_int_get (&cv->listcontrols.refresh_requested);
	ovreq = g_atomic_int_get (&ov->listcontrols.refresh_requested);

	cvhook = ovhook = FALSE;
	ovsave = 0;

	e2_utf8_set_name_conversion_if_requested ();

	if (cvreq)
	{
retestc:
		cvdone = !(
		g_atomic_int_get (&cv->listcontrols.refresh_refcount) || //this refresh is disabled
		g_atomic_int_get (&cv->listcontrols.refresh_working) ||	//or busy
		g_atomic_int_get (&cv->listcontrols.cd_working)
#ifdef E2_STATUS_BLOCK
		|| g_atomic_int_get (&app.status_working)
#endif
		);
		if (cvdone)
		{
			LISTS_UNLOCK
#ifdef E2_VFSTMP
			//FIXME dir when not mounted local
#else
			printd (DEBUG, "accepting request to refresh %s", cv->dir);
#endif
			//refresh function expects BGL open
			if (pthread_create (&thisID, NULL,
				(gpointer(*)(gpointer))_e2_filelist_refresh_store, cv) == 0)
			{
				g_atomic_int_set (&cv->listcontrols.refresh_requested, 0); //not needed downstream
				pthread_join (thisID, &result); //only 1 refresh at a time
				if (result == GINT_TO_POINTER (1))
					cvhook = TRUE;	//send it downstream
				else if (result == NULL) //try again
					g_atomic_int_set (&cv->listcontrols.refresh_requested, 1); //revert
			}
			else
				printd (WARN, "Failed to create list refresh thread");

			LISTS_LOCK
		}
	}
	else
		cvdone = FALSE;

	if (ovreq)
	{
retesto:
		ovdone = !(
		g_atomic_int_get (&ov->listcontrols.refresh_refcount) ||
		g_atomic_int_get (&ov->listcontrols.refresh_working) ||
		g_atomic_int_get (&ov->listcontrols.cd_working)
#ifdef E2_STATUS_BLOCK
		|| g_atomic_int_get (&app.status_working)
#endif
		);
		if (ovdone)
		{
			LISTS_UNLOCK
#ifdef E2_VFSTMP
		//FIXME dir when not mounted local
#else
			printd (DEBUG, "accepting request to refresh %s", ov->dir);
#endif
			if (pthread_create (&thisID, NULL,
				(gpointer(*)(gpointer))_e2_filelist_refresh_store, ov) == 0)
			{
				g_atomic_int_set (&ov->listcontrols.refresh_requested, 0); //not needed downstream
				pthread_join (thisID, &result); //only 1 refresh at a time
				if (result == GINT_TO_POINTER (1))
					ovhook = TRUE;
				else if (result == NULL) //try again
					g_atomic_int_set (&ov->listcontrols.refresh_requested, 1);
			}
			else
				printd (WARN, "Failed to create list refresh thread");

			LISTS_LOCK
		}
	}
	else
		ovdone = FALSE;

	if (cvreq || ovreq)
	{
		if (ovdone)
			ovsave++;	//save satus
		//maybe again/now/still want to do one or both panes
		//but only go back now if not blocked before
		if (cvdone && g_atomic_int_get (&cv->listcontrols.refresh_requested))
		{
			usleep (10000);
			ovreq = FALSE; //don't (yet) repeat ov (ok cuz cvreq is still TRUE)
			goto retestc; //sets ovdone FALSE
		}

		if (ovsave > 0 && g_atomic_int_get (&ov->listcontrols.refresh_requested))
		{
			usleep (10000);
			ovreq = TRUE; //reinstate after any cv repeat
			ovsave = 0;
			goto retesto;
		}

#ifdef E2_REFRESH_DEBUG
		printd (DEBUG, "start timer REFRESHBEGIN_T");
#endif
		app.timers[REFRESHBEGIN_T] = g_timeout_add_full (G_PRIORITY_HIGH, 200,
			(GSourceFunc) _e2_filelist_refresh_manage,
			GUINT_TO_POINTER (REFRESHBEGIN_T),
			(GDestroyNotify) e2_filelist_timer_shutdown);
	}

	LISTS_UNLOCK
	//initiate refresh hooklists here, not downstream, so that refreshes are batched,
	//and in idle to avoid any UI impact
	//(i.e. small risk of cd / another refresh starting before hook is called)
	if (cvhook)
		g_idle_add ((GSourceFunc)_e2_filelist_propogate_refresh, curr_view);
	if (ovhook)
		g_idle_add ((GSourceFunc)_e2_filelist_propogate_refresh, other_view);

	return FALSE;
}
#endif
/**
@brief log request, and possibly initiate, filelist(s) refresh if it/they currently show @a dir
If a refresh is initiated, either or both flagged lists will be refreshed
as soon as any in-progress re-list (either or both panes) is completed
@param dir the dir to be refreshed (utf8 string)
@param immediate TRUE to intiate a refresh

@return TRUE if a refresh was initiated
*/
gboolean e2_filelist_request_refresh (gchar *dir, gboolean immediate)
{
	gboolean matched = FALSE;
#ifdef E2_VFSTMP
	//CHECKME v-dir ok
#endif
	if (strcmp (app.pane1.view.dir, dir) == 0)
	{
		matched = TRUE;
		g_atomic_int_set (&app.pane1.view.listcontrols.refresh_requested, 1);
	}
#ifdef E2_VFSTMP
	//CHECKME v-dir ok
#endif
	if (strcmp (app.pane2.view.dir, dir) == 0)
	{
		matched = TRUE;
		g_atomic_int_set (&app.pane2.view.listcontrols.refresh_requested, 1);
	}

	if (matched && immediate)
	{
#ifdef E2_REFRESH_DEBUG
		printd (DEBUG, "In e2_filelist_request_refresh, goto e2_filelist_check_dirty");
#endif
		e2_filelist_check_dirty (GINT_TO_POINTER (1));
	}
#ifdef E2_REFRESH_DEBUG
	else
		printd (DEBUG, "In e2_filelist_request_refresh, NO e2_filelist_check_dirty");
#endif

	return matched;
}
/**
@brief decide whether to lengthen the interval between filelist refresh requests
This is called when a 'dirty' flag has been detected

@param pane enumerator for pane 1 or pane 2, which was reported dirty

@return TRUE to process the current request now
*/
static gboolean _e2_filelist_refresh_ok (E2_ListChoice pane)
{
	gboolean retval;
	/* Ideally this would check CPU resource-usage for this app and all children,
	   and only proceed if there's enough(?) idle capacity
	   Instead we assume small dirs can be processed without much adverse effect */
	gint times, itemcount = (pane == PANE1) ?
		//these are counts of total items, not filtered items
		gtk_tree_model_iter_n_children (GTK_TREE_MODEL (app.pane1.view.store), NULL):
		gtk_tree_model_iter_n_children (GTK_TREE_MODEL (app.pane2.view.store), NULL);
	if (itemcount < 51)
		times = 1;
	else if (itemcount < 251)
		times = 2;
	else
		times = 3;

	if (pane == PANE1)
	{
		if (++pane1repeats >= times)
		{
			printd (DEBUG, "Pane 1 refresh interval count = %d", times);
			pane1repeats = 0;
			retval = TRUE;
		}
		else
			retval = FALSE;
	}
	else
	{
		if (++pane2repeats >= times)
		{
			printd (DEBUG, "Pane 2 refresh interval count = %d", times);
			pane2repeats = 0;
			retval = TRUE;
		}
		else
			retval = FALSE;
	}
	return retval;
}
/**
@brief initiate refresh of filepane(s) contents if requested (elsewhere)
 or if changed content has been, or is now, detected

Among other uses, this is the timer callback function for filelists auto refresh
In certain circumstances any dirty flag might be ignored, or deferred to lengthen
the effective interval between refreshes.
A directory that is 'gone' will have been flagged as 'dirty', and later the
list-refesh function handles choosing a replacement dir.
Non-NULL @a userdata forces refresh of both panes ASAP.
If this is a timer callback, the cfgdirty flag may be refreshed, but not otherwise
acted upon.
If there has been no refresh during the last QUIET_SECONDS, then
app.timers[DIRTYCHECK_T] is shut down. Otherwise, the time of the last refresh is
updated.
This expects gtk's BGL to be off/open

@param userdata data specified when the timer was initialised (NULL), or when called directly, usually non-NULL

@return TRUE so the timer keeps working
*/
gboolean e2_filelist_check_dirty (gpointer userdata)
{
	static gboolean busy = FALSE;
	gboolean dop1, dop2;

	if (busy)
	{
		if (userdata == NULL)
		{	//timer callback, not a specific request
#ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "e2_filelist_check_dirty is busy, exit immediately");
#endif
			return TRUE;	//no re-entrant usage, run the timer again
		}
		else
		{
			while (busy)
				usleep (50000);
		}
	}
	busy = TRUE;

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "In e2_filelist_check_dirty()");
#endif

	if (userdata == NULL)
	{	//a timer callback
	/* if E2_FAM_KERNEL backend is working, that will have already set *dirty
		flag(s) as appropriate*/
#ifdef E2_FAM_KERNEL
		if (app.monitor_type == E2_MONITOR_DEFAULT)
		{
#endif
			e2_fs_FAM_check_dirty (&p1dirty, &p2dirty
#ifdef E2_FAM
			//cfgdirty flag is logged, but not otherwise processed here
			, &cfgdirty
#endif
			);
#ifdef E2_FAM_KERNEL
		}
#endif
	}
	else
	{	//specific request, start fresh
		pane1repeats = 0;
		pane2repeats = 0;
	}

	//ASAP check and clear flags used by backend
	dop1 = g_atomic_int_compare_and_exchange (&p1dirty, 1, 0);
	dop2 = g_atomic_int_compare_and_exchange (&p2dirty, 1, 0);

	if (dop1 && (userdata != NULL || _e2_filelist_refresh_ok (PANE1)))
	{
		g_atomic_int_set (&app.pane1.view.listcontrols.refresh_requested, 1);
#ifdef E2_REFRESH_DEBUG
		printd (DEBUG,"refresh-requested flag set for pane 1");
#endif
	}
	else
	{
#ifdef E2_REFRESH_DEBUG
		if (dop1)
			printd (DEBUG,"dirty set FALSE for pane 1");
# ifdef E2_FAM_KERNEL
		else
			printd (DEBUG,"pane 1 is clean");
# endif
#endif
		dop1 = g_atomic_int_get (&app.pane1.view.listcontrols.refresh_requested);
	}

	if (dop2 && (userdata != NULL || _e2_filelist_refresh_ok (PANE2)))
	{
		g_atomic_int_set (&app.pane2.view.listcontrols.refresh_requested, 1);
#ifdef E2_REFRESH_DEBUG
		printd (DEBUG,"refresh-requested flag set for pane 2");
#endif
	}
	else
	{
#ifdef E2_REFRESH_DEBUG
		if (dop2)
			printd (DEBUG,"dirty set FALSE for pane 2");
# ifdef E2_FAM_KERNEL
		else
			printd (DEBUG,"pane 2 is clean");
# endif
#endif
		dop2 = g_atomic_int_get (&app.pane2.view.listcontrols.refresh_requested);
	}

	if (dop1 || dop2)
	{
		last_work_time = time (NULL);
#if 1
		pthread_t thisID;
		pthread_attr_t attr;

		e2_utf8_set_name_conversion_if_requested ();

		pthread_attr_init (&attr);
		pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);
		//priority for active pane
		if (g_atomic_int_get (&curr_view->listcontrols.refresh_requested))
		{
			if (pthread_create (&thisID, &attr,
				(gpointer(*)(gpointer))_e2_filelist_refresh_view, curr_view) != 0)
			{
				//TODO handle error
				printd (WARN,"refresh-dir-thread-create error!");
			}
		}
		if (g_atomic_int_get (&other_view->listcontrols.refresh_requested))
		{
			if (pthread_create (&thisID, &attr,
				(gpointer(*)(gpointer))_e2_filelist_refresh_view, other_view) != 0)
			{
				//TODO handle error
				printd (WARN,"refresh-dir-thread-create error!");
			}
		}

		pthread_attr_destroy (&attr);
#else
		if (app.timers[REFRESHBEGIN_T] > 0)
		{
			//clear any incomplete refresh request
#ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "stop timer REFRESHBEGIN_T");
#endif
			g_source_remove (app.timers[REFRESHBEGIN_T]);
		}
		//the delay between attempts will be lengthened later, if appropriate
//CHECKME we may already be in a timer callback here - are nested timers acceptable to glib/gtk ?
#ifdef E2_REFRESH_DEBUG
		printd (DEBUG, "Start timer REFRESHBEGIN_T to initiate refresh");
#endif
		app.timers[REFRESHBEGIN_T] = g_timeout_add_full (G_PRIORITY_HIGH, 5,
			(GSourceFunc) _e2_filelist_refresh_manage,
			GUINT_TO_POINTER (REFRESHBEGIN_T),
			(GDestroyNotify) e2_filelist_timer_shutdown);
#endif
	}
	else
	{
		if (time (NULL) >= last_work_time + QUIET_SECONDS)
		{
			printd (DEBUG, "Suspend operation of timer DIRTYCHECK_T");
			g_source_remove (app.timers[DIRTYCHECK_T]);
			app.timers[DIRTYCHECK_T] = 0;
			//arrange to restart upon activity
			g_signal_connect (G_OBJECT (app.main_window), "event",
				G_CALLBACK (e2_filelist_repoll), GUINT_TO_POINTER (DIRTYCHECK_T));
		}
	}

	busy = FALSE;
	return TRUE;
}
/**
@brief stop change-monitoring
Either stop a timer which polls for various 'dirty' indicator(s), or stop
the 'kernel-backend' process(es)  in which case it expects app.monitor_type
to be set appropriately
No check here for "auto-refresh" option status

@return
*/
void e2_filelist_stop_refresh_checks (void)
{
#ifdef E2_FAM_KERNEL
	//stop backend working
	e2_fs_FAM_cancel_monitor_dir (&app.pane1);
	e2_fs_FAM_cancel_monitor_dir (&app.pane2);
#endif
	//this timer is used whatever app.monitor_type is, with E2_FAM_KERNEL
	//TODO stopping it also shuts down cfg monitoring, which shares the mechanism
	if (app.timers[DIRTYCHECK_T] != 0)
	{
#ifdef E2_REFRESH_DEBUG
		printd (DEBUG, "stop timer DIRTYCHECK_T");
#endif
		g_source_remove (app.timers[DIRTYCHECK_T]);
		app.timers[DIRTYCHECK_T] = 0;
	}
	g_atomic_int_set (&app.pane1.view.listcontrols.refresh_refcount, 0);
	g_atomic_int_set (&app.pane2.view.listcontrols.refresh_refcount, 0);
}
/**
@brief start change-monitoring
Either start a timer which polls for various 'dirty' indicator(s), or start
the 'kernel-backend' process(es)- in which case it expects app.monitor_type
to be set appropriately
No check here for "auto-refresh" option status
The timer (if any) may later be stopped permanently if change-monitoring is
abandoned, probably via a config dialog. But it won't be stopped merely if
change-monitoring is disabled i.e. suspended

@return
*/
void e2_filelist_start_refresh_checks (void)
{
	g_atomic_int_set (&app.pane1.view.listcontrols.refresh_refcount, 0);
	g_atomic_int_set (&app.pane2.view.listcontrols.refresh_refcount, 0);
#ifdef E2_FAM_KERNEL
/* app.monitor_type setup needs a prior call to setup kernel-monitoring if
	that's in the build */
	if (app.monitor_type == E2_MONITOR_DEFAULT)
	{
		if (app.timers[DIRTYCHECK_T] == 0)
		{
# ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "start timer DIRTYCHECK_T");
# endif
			app.timers[DIRTYCHECK_T] =
# ifdef USE_GLIB2_14
				g_timeout_add_seconds (E2_FILESCHECK_INTERVAL_S,
# else
				g_timeout_add (E2_FILESCHECK_INTERVAL,
# endif
					(GSourceFunc) e2_filelist_check_dirty, NULL);
		}
	}
	else
	{
		//start backend working
		e2_fs_FAM_change (app.pane1.path, &app.pane1);
		e2_fs_FAM_change (app.pane2.path, &app.pane2);
		e2_fs_FAM_resume ();
	}
#else //ndef E2_FAM_KERNEL
	if (app.timers[DIRTYCHECK_T] == 0)
	{
# ifdef E2_REFRESH_DEBUG
		printd (DEBUG, "start timer DIRTYCHECK_T");
# endif
		app.timers[DIRTYCHECK_T] =
# ifdef USE_GLIB2_14
			g_timeout_add_seconds (E2_FILESCHECK_INTERVAL_S,
# else
			g_timeout_add (E2_FILESCHECK_INTERVAL,
# endif
				(GSourceFunc) e2_filelist_check_dirty, NULL);
	}
#endif
}
/**
@brief idle function to clear old liststores

@param user_data UNUSED data specified when the idle was setup

@return FALSE, to stop the callbacks
*/
gboolean e2_filelist_clear_old_stores (gpointer user_data)
{
	GSList *tmp;
#ifdef DEBUG_MESSAGES
	gint debug = g_slist_length (app.used_stores);
#endif
	for (tmp = app.used_stores; tmp != NULL; tmp = tmp->next)
	{
		GtkListStore *store = tmp->data;
		GtkTreeModel *mdl = GTK_TREE_MODEL (store);
		GtkTreeIter iter;
		if (gtk_tree_model_get_iter_first (mdl, &iter))
		{	//it's not empty already
			//clear file info data structs referred to in the store
			//CHECKME need to clear anything else?
			do
			{
				FileInfo *info;
				gtk_tree_model_get (mdl, &iter, FINFO, &info, -1);
				if (G_LIKELY(info != NULL))
#ifdef USE_GLIB2_10
					g_slice_free1 (sizeof(FileInfo), info);
					//DEALLOCATE (FileInfo, info);
#else
					DEALLOCATE (FileInfo, info);
#endif
			} while (gtk_tree_model_iter_next (mdl, &iter));
		}
		//CHECKME clear filtermodel, if any ?
		g_object_unref (G_OBJECT (store));
	}
	g_slist_free (app.used_stores);
	app.used_stores = NULL;
	printd (DEBUG, "%d old liststore(s) cleared", debug);
	return FALSE;
}
/**
@brief create and populate filelist-compatible list store with rows for each item in @a entries
This does not attach the created store to anything.
@param entries list of FileInfo's to be processed
@param view pointer to data struct for the view in which the filled store data will be used

@return pointer to the liststore, or NULL if a problem occurs
*/
GtkListStore *e2_filelist_fill_store (GList *entries, ViewInfo *view)
{
//	printd (DEBUG, "start store fill");
	GtkListStore *store = e2_filelist_make_store ();
	if (store == NULL)
		return NULL;

	guint i;
	GList *tmp;
	FileInfo *infoptr;
	GtkTreeIter iter;
	struct tm *tm_ptr;
	struct passwd *pwd_buf;
	struct group *grp_buf;
	gchar size_buf[20];	//enough for 999 Tb
	gchar modified_buf[25];
	gchar accessed_buf[25];
	gchar changed_buf[25];
//	gchar perm_buf[11];
	gchar uid_buf[20];
	gchar gid_buf[20];
	gchar *buf[NAMEKEY+1];	//pointers to strings to be inserted into each row
	gchar *freeme;
	gchar *__utf__;	//for e2_utf8_from_locale_fast() macro

	//get format parameters

	//format for dates display
	gchar *strf_string;
	switch (e2_option_int_get ("date-string"))
	{
		case 1:
			strf_string = "%d/%m/%y %H:%M";	//standard
			break;
		case 2:
			strf_string = "%m/%d/%y %H:%M";	//american
			break;
		case 3:
			strf_string = "%Y-%m-%dT%H:%M";	//ISO8601
			break;
		case 4:
			strf_string = "%x %X";	//localised
			break;
		case 5:
			strf_string = e2_option_str_get ("custom-date-format");
			if (strf_string != NULL && *strf_string != '\0')
				//no reliable check for format validity ?
				break;
		default:
			strf_string = "%b %d %H:%M";
			break;
	}
	//get format for size display
	gchar *comma;
	switch	(e2_option_int_get ("size-string"))
	{
		case 1:
		{
			comma = nl_langinfo (THOUSEP);
			if (comma == NULL || *comma == '\0')
				comma = ",";
			break;
		}
		default:
			comma = NULL;	//signal to use the condensed version
			break;
	}

	gboolean anyconvert = FALSE;
	gboolean caseignore = ! e2_option_bool_get ("namesort-case-sensitive");
#ifdef E2_RAINBOW
	gboolean anycase = e2_option_bool_get ("anycase-filetypes");
#endif
/*
#ifdef USE_GTK3_0
	GdkRBGA *normal;
	GtkStyleContext *context;
	context = gtk_widget_get_style_context (curr_view->treeview);
	gtk_style_context_get (context, GTK_STATE_NORMAL, GTK_STYLE_PROPERTY_COLOR,
		&normal, NULL);
	GdkColor normal2 = {0,  (guint16)(65535 * normal->red),
							(guint16)(65535 * normal->green),
							(guint16)(65535 * normal->blue) } ;
	GdkColor *default_color = &normal2;
	gdk_rgba_free (normal);
#else
	GtkStyle *style = gtk_rc_get_style (curr_view->treeview);
	GdkColor *default_color = &style->text[GTK_STATE_NORMAL];
#endif
*/
	GdkColor *default_color = NULL;
#ifdef E2_ASSISTED
	GdkColor *back_color = (e2_option_bool_get ("color-background-set")) ?
		e2_option_color_get ("color-background") : NULL;
#endif
	GdkColor *link_color = e2_option_color_get ("color-ft-link");
	GdkColor *dir_color = e2_option_color_get ("color-ft-dir");
	GdkColor *dev_color = e2_option_color_get ("color-ft-dev");
	GdkColor *sock_color = e2_option_color_get ("color-ft-socket");
	GdkColor *exec_color = e2_option_color_get ("color-ft-exec");

	//iterate through the listed FileInfo's
	for (tmp = entries; tmp != NULL; tmp = tmp->next)
	{
		gboolean convert; //also a cleanup flag
		gchar *s;
		register char c;

		infoptr = (FileInfo *) tmp->data;
		//with bad unmount behaviour, there can be no actual data
		if (infoptr == NULL)
			continue; //CHECKME remove the item from list ?
		//scan the namestring for non-ASCII char(s)
		convert = FALSE;
		for (s = infoptr->filename; (c = *s) != '\0'; s++)
		{
			if (c < 0) //non-ASCII
			{
				convert = TRUE;
				anyconvert = TRUE;
				break;
			}
		}
		//convert to UTF-8 if appropriate
		if (convert)
		{
			buf[FILENAME] = g_filename_display_name (infoptr->filename);
/*			if (g_utf8_validate (infoptr->filename, -1, NULL))
			{
				convert = FALSE;
				buf[FILENAME] = infoptr->filename;
			}
			else
			{	//_not display_ so that it can be casefolded and/or collated properly ??
				buf[FILENAME] = g_locale_to_utf8 (infoptr->filename, -1, NULL, NULL, NULL);
				if (buf[FILENAME] == NULL)
					buf[FILENAME] = e2_utf8_from_locale_fallback (infoptr->filename);
			}
*/
		}
		else
			buf[FILENAME] = infoptr->filename;

#ifdef E2_EXTCOL
		gchar *ext = buf[FILENAME];
		s = ext;
		while ((s = strchr (s, '.')) != NULL)
		{
			if (s > ext)
				ext = ++s;	//skip discovered dot, ascii '.'. always single char
			else //leading '.' i.e. hidden item
				s++;
		}
		if (ext > buf[FILENAME] && *ext != '\0')
			buf[EXTN] = g_strdup (ext);
		else
			buf[EXTN] = g_strdup ("");
#endif

		if (caseignore)
		{
			freeme = g_utf8_casefold (buf[FILENAME], -1);
#ifdef USE_GTK2_8
			buf[NAMEKEY] = g_utf8_collate_key_for_filename (freeme, -1);
#else
			buf[NAMEKEY] = g_utf8_collate_key (freeme, -1);
#endif
			g_free (freeme);
		}
		else
#ifdef USE_GTK2_8
			buf[NAMEKEY] = g_utf8_collate_key_for_filename (buf[FILENAME], -1);
#else
			buf[NAMEKEY] = g_utf8_collate_key (buf[FILENAME], -1);
#endif

		//dirlink check done when info was created
		if (infoptr->statbuf.st_mode & (S_IFDIR | E2_DIRLNK))
		{
			//directories (and links to dirs) get a trailing separator
			s = buf[FILENAME];
			if (convert)
			{
				guint l = strlen (s);
				if ((s = (gchar *)realloc (s, l+2)) != NULL)
				{
					buf[FILENAME] = s;
					s += l;
					*s++ = G_DIR_SEPARATOR;
					*s = '\0';
				}
			}
			else
			{
				convert = TRUE;	//cleanup this one too
				buf[FILENAME] = e2_utils_strcat (s, G_DIR_SEPARATOR_S);
			}
		}

		if (comma == NULL)
		{	//use condensed size format
			if (infoptr->statbuf.st_size < 1024) //less than 1k
			{
			  g_snprintf(size_buf, sizeof(size_buf), "%"PRIu64,
					infoptr->statbuf.st_size);
			}
			else if (infoptr->statbuf.st_size < 1048576) //less than a meg
			{
			  g_snprintf(size_buf, sizeof(size_buf), "%.1f%s",
				(gfloat) infoptr->statbuf.st_size / 1024.0, _("k"));
			}
			else  //a meg or more  if (infoptr->statbuf.st_size < 1073741824)
			{
			  g_snprintf(size_buf, sizeof(size_buf), "%.1f%s",
				(gfloat) infoptr->statbuf.st_size / 1048576.0, _("M"));
			}
/*			else //a gig or more
			{
			  g_snprintf(size_buf, sizeof(size_buf), "%.1f%s",
				(gfloat) infoptr->statbuf.st_size / 1073741824.0, _("G"));
			} */
		}
		else
		{	//use actual size, with commas
			g_snprintf(size_buf, sizeof(size_buf), "%"PRIu64,
				infoptr->statbuf.st_size);

			guint len = strlen (size_buf);
			guint ths = len-1;  //0-based index
			while (ths > 2 && len < sizeof(size_buf))
			{
				for (i = len-1; i > ths-3; i--)
					size_buf[i+1] = size_buf[i];

				size_buf[i+1] = *comma;
				size_buf[++len] = '\0';
				ths = i;
			}
		}
		buf[SIZE] = size_buf;	//content is ascii/utf-8, no need to convert or free

		buf[PERM] = e2_fs_get_perm_string (infoptr->statbuf.st_mode); //string is already utf

		if ((pwd_buf = getpwuid (infoptr->statbuf.st_uid)) == NULL)
		{
		  g_snprintf (uid_buf, sizeof(uid_buf), "%d",
		     (guint) infoptr->statbuf.st_uid);
		  buf[OWNER] = g_strdup (uid_buf); //content is ascii number, no need to convert to utf
		}
		else
		{
		  buf[OWNER] = e2_utf8_from_locale_fast (pwd_buf->pw_name);
		}

		if ((grp_buf = getgrgid (infoptr->statbuf.st_gid)) == NULL)
		{
		  g_snprintf(gid_buf, sizeof(gid_buf), "%d",
		       (guint) infoptr->statbuf.st_gid);
		  buf[GROUP] = g_strdup (gid_buf); //content is ascii number, no need to convert to utf
		}
		else
		{
		  buf[GROUP] = e2_utf8_from_locale_fast (grp_buf->gr_name);
		}
		tm_ptr = localtime (&(infoptr->statbuf.st_mtime));
		strftime (modified_buf, sizeof(modified_buf), strf_string, tm_ptr);
		buf[MODIFIED] = e2_utf8_from_locale_fast (modified_buf);

		tm_ptr = localtime (&(infoptr->statbuf.st_atime));
		strftime (accessed_buf, sizeof(accessed_buf), strf_string, tm_ptr);
		buf[ACCESSED] = e2_utf8_from_locale_fast (accessed_buf);

		tm_ptr = localtime(&(infoptr->statbuf.st_ctime));
		strftime (changed_buf, sizeof(changed_buf), strf_string, tm_ptr);
		buf[CHANGED] = e2_utf8_from_locale_fast (changed_buf);

		GdkColor *foreground;
		switch (infoptr->statbuf.st_mode & S_IFMT)
		{
		  case S_IFLNK:
		    foreground = link_color;
		    break;
		  case S_IFDIR:
		    foreground = dir_color;
		    break;
		  case S_IFCHR:
		  case S_IFBLK:
		    foreground = dev_color;
		    break;
		  case S_IFSOCK:
		    foreground = sock_color;
		    break;
		  case S_IFREG:
			  //show as executable if _anyone_ has that permission
		    if ((S_IXUSR & infoptr->statbuf.st_mode)
		  		|| (S_IXGRP & infoptr->statbuf.st_mode)
		  		|| (S_IXOTH & infoptr->statbuf.st_mode))
			  foreground = exec_color;
			else
			{
#ifdef E2_RAINBOW
				//find extension of current item
				//localised text, but assumes . is ascii
				//hashed extension-strings used for comparison have been localised
				foreground = NULL;	//in case there's no '.' at all
				gchar *ext = infoptr->filename + 1;	//+1 in case item is hidden
				while ((ext = strchr (ext,'.')) != NULL)
				{
					ext++;	//pass the '.'
					//use its color, if any
					if (anycase)
					{ //infoptr->filename is localised text
						gchar *utf = F_FILENAME_FROM_LOCALE (ext);
						gchar *ext2 = g_utf8_casefold (utf, -1);
						foreground = g_hash_table_lookup (app.colors, ext2);
						g_free (ext2);
						F_FREE (utf, ext);
					}
					else
						foreground = g_hash_table_lookup (app.colors, ext);
					if (foreground != NULL)
						break;
				}
				if (foreground == NULL)
					foreground = default_color;
#else
				foreground = default_color;
#endif
			}
		    break;
		  default:
			foreground = default_color;
		    break;
		}
		//gtk >= 2.10 can handle &iter = NULL
		gtk_list_store_insert_with_values (store, &iter, -1,
			FILENAME, buf[FILENAME],
#ifdef E2_EXTCOL
			EXTN, buf[EXTN],
#endif
			SIZE, buf[SIZE],
			PERM, buf[PERM],
			OWNER, buf[OWNER],
			GROUP, buf[GROUP],
			MODIFIED, buf[MODIFIED],
			ACCESSED, buf[ACCESSED],
			CHANGED, buf[CHANGED],
			NAMEKEY, buf[NAMEKEY],
			FINFO, infoptr,
			FORECOLOR, foreground,
#ifdef E2_ASSISTED
			BACKCOLOR, back_color,
#endif
//			VISIBLE, TRUE,
			-1);

		//cleanup
		if (convert)
			g_free (buf[FILENAME]);
		for (i = PERM; i <= NAMEKEY; i++)
			g_free (buf[i]);

	}

	if (anyconvert != view->convert)
	{
		view->convert = anyconvert;
		//flag need for change of conversion funcs at a suitable time
		app.reconvert_requested = TRUE;
	}
/*
//	gettimeofday (&strt, NULL); //FOR DEBUGGING, check time this load takes
//	gettimeofday (&nd, NULL);
	if (nd.tv_usec < strt.tv_usec)
	{
		int nsec = (strt.tv_usec - nd.tv_usec) / 1000000 + 1;
		strt.tv_usec -= 1000000 * nsec;
		strt.tv_sec += nsec;
	}
	if (nd.tv_usec - strt.tv_usec > 1000000)
	{
		int nsec = (strt.tv_usec - nd.tv_usec) / 1000000;
		strt.tv_usec += 1000000 * nsec;
		strt.tv_sec -= nsec;
	}
	result.tv_sec = result.tv_sec + nd.tv_sec - strt.tv_sec;
	result.tv_usec = result.tv_usec + nd.tv_usec - strt.tv_usec;
	if (result.tv_usec > 1000000)
	{
		int nsec = result.tv_usec / 1000000 + 1;
		result.tv_usec -= 1000000 * nsec;
		result.tv_sec += nsec;
	}

//	printd (DEBUG, "populating rows took %f seconds", result.tv_sec + result.tv_usec / 1000000.0 );
*/
//	printd (DEBUG, "populating rows finished");
	return store;
}
/**
@brief cleanup helper function

@param info data item to clean
@param data UNUSED

@return
*/
void e2_filelist_cleaninfo (FileInfo *info, gpointer data)
{
#ifdef USE_GLIB2_10
	g_slice_free1 (sizeof (FileInfo), info);
#else
	DEALLOCATE (FileInfo, info);
#endif
}
/**
@brief convert @a entries from a list of localised heaped strings to a list of FileInfo's
Some 'custom' flags (dirlinks etc) are set if appropriate.
Upon failure, the supplied list is cleared and its ptr set to relevant error code
@param parentpath absolute path of dir containing items in @a list, localised string
@param list store for ptr to Glist of heaped item names

@return TRUE if the list was converted successfully
*/
gboolean e2_filelist_make_all_infos (gchar *parentpath, GList **list)
{
	gint len1, len2, trailer;
	gchar *item;
	FileInfo *info;
	GList *member;
	guint errval = E2DREAD_ENOENT;	//assign to prevent compiler error
#ifdef E2_VFS
	VPATH data;
# ifdef E2_VFSTMP
	//get proper spacedata
# endif
	data.spacedata = NULL; //FIXME
# ifdef E2_VFSTMP
	//check if #'d vpath for parentpath exists, or else make one
# endif
#endif
	gboolean mingle = ! e2_option_bool_get ("sort-dirs-first");

	len1 = strlen (parentpath);
	trailer = (*(parentpath + len1 - sizeof (gchar)) == G_DIR_SEPARATOR) ? 0 : sizeof (gchar); //ascii ok
	len2 = (len1 + trailer + NAME_MAX)/8*8 + 8;

	gchar localpath[len2];
	g_strlcpy (localpath, parentpath, sizeof (localpath));
	if (trailer)
	{
		*(parentpath + len1 - sizeof (gchar)) = G_DIR_SEPARATOR;
		len1 += sizeof (gchar);
	}
	len2 -= len1;	//or NAME_MAX ?
	item = localpath + len1;

	//retval = TRUE;
	for (member = *list; member != NULL; member = member->next)
	{
#ifdef USE_GLIB2_10
		info = (FileInfo *) g_slice_alloc (sizeof (FileInfo));
//		info = ALLOCATE (FileInfo);
#else
		info = ALLOCATE (FileInfo);
#endif
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (info, errval = E2DREAD_ENOMEM;goto errexit;)
#else
		if (info == NULL)
		{
			errval = E2DREAD_ENOMEM;
			goto errexit;
		}
#endif
nextmember:
		g_strlcpy (item, (gchar *)member->data, len2);
#ifdef E2_VFS
		data.path = localpath;
		if (e2_fs_lstat (&data, &info->statbuf E2_ERR_NONE()))
#else
		if (e2_fs_lstat (localpath, &info->statbuf E2_ERR_NONE()))
#endif
		{
			//FIXME use error code from prior lstat to do this parent-check better
			*item = '\0';
			if (e2_fs_lstat (
#ifdef E2_VFS
			&data, &info->statbuf, NULL))
#else
			localpath, &info->statbuf))
#endif
			{
				errval = E2DREAD_NS;
				goto errexit;
			}
			else
			{
				//keep going if just this one item is N/A
				GList *tmp = member;
				member = member->next;
				*list = g_list_delete_link (*list, tmp);
				if (member != NULL)
					goto nextmember;
				DEALLOCATE (FileInfo, info);
				break;
			}
		}

#ifdef S_IFLNK
		if (S_ISLNK(info->statbuf.st_mode) && !S_ISDIR (info->statbuf.st_mode))
		{
			struct stat statbuf;
			if (!e2_fs_stat
# ifdef E2_VFS
				(&data, &statbuf E2_ERR_NONE())
# else
				(localpath, &statbuf E2_ERR_NONE())
# endif
				&& S_ISDIR (statbuf.st_mode))
			{
# ifndef E2_VFS
				printd (DEBUG, "%s is a link not flagged as a dir", localpath);
# endif
				info->statbuf.st_mode |= E2_DIRLNK;
				if (mingle)
					info->statbuf.st_mode |= E2_MINGLEDIRS; //remember to NOT sort 1st
			}
		}
#endif
		if (S_ISDIR (info->statbuf.st_mode))
		{
#ifdef S_IFLNK
			if (S_ISLNK (info->statbuf.st_mode))
			{
				//probably this never happens
# ifndef E2_VFS
				printd (DEBUG, "%s is a dir flagged as a link", localpath);
# endif
				info->statbuf.st_mode |= E2_DIRLNK;	//remember this item is link to a dir
			}
#endif
			if (mingle)
				info->statbuf.st_mode |= E2_MINGLEDIRS; //remember to NOT sort 1st
		}

		//any truncation here (>NAME_MAX+1) means it will never be able to be stat'd again
		g_strlcpy (info->filename, (gchar *)member->data, sizeof (info->filename));
#ifdef E2_VFSTMP
		populate info->dir if needed
#endif
		g_free (member->data);
		member->data = info;
	}

	return TRUE;

errexit:
	//FIXME warning
	e2_list_free_with_data (&member);	//get rid of remaining string data
	g_list_foreach (*list, (GFunc) e2_filelist_cleaninfo, NULL);
	g_list_free (*list);
	*list = GINT_TO_POINTER (errval);
	return FALSE;
}
/**
@brief incrementally refresh list store for @a view

If the directory associated with @a view is no longer accessible, a suitable
alternative is found and displayed.
This is a thread function.
Expects BGL to be open, on arrival here.

@param view data structure for view being processed

@return NULL (FALSE) if the refresh fails for some unspecified reason,
  or pointerised 1 if it succeeds,
  or pointerised 2 after cd to elsewhere,
  or pointerised 3 after stat(view->dir) or some other failure
*/
static gpointer _e2_filelist_refresh_store (ViewInfo *view)
{
	//no check for cd-working
	gboolean busy =
	   g_atomic_int_get (&view->listcontrols.refresh_refcount)
	|| g_atomic_int_get (&view->listcontrols.refresh_working);
#ifdef E2_STATUS_BLOCK
	if (!busy)
		busy = (view == curr_view && g_atomic_int_get (&app.status_working));
#endif
	if (busy)
	{
		printd (DEBUG, "BLOCKED start refresh filelist for %s", view->dir);
		return NULL;	//refresh is disabled, or the view is still being processed from last time
	}
	printd (DEBUG, "start refresh filelist for %s", view->dir);
#ifdef E2_VFSTMP
	if ((curr_view->spacedata == other_view->spacedata)
		&& !strcmp (curr_view->dir, other_view->dir))
	{
		//setup to process both views in sequence, and zero any refresh_requested flag(s)
	}
#endif

	//<for view 1 and later, maybe, view 2 also ...>

	gchar *utf = g_strdup (view->dir);	//don't clobber view->dir
#ifdef E2_VFSTMP
	//FIXME valid path check for v-dirs
	//FIXME get path to goto when when dir is v-dir, may include a space-change
#endif
	CLOSEBGL	//in case of downstream error message
	if (e2_fs_get_valid_path (&utf, TRUE E2_ERR_NONE()))
		g_free (utf);
	else
	{	//desired path not accessible, go to somewhere that is ..
		gchar *msg = g_strdup_printf (_("Cannot access %s, going to %s instead"),
			view->dir, utf);
#ifdef E2_VFSTMP
	//FIXME cater for any space-change message
#endif
		e2_output_print_error (msg, TRUE);
		OPENBGL

		E2_PaneRuntime *rt = (view == curr_view) ? curr_pane : other_pane;
#ifdef E2_VFSTMP
		//FIXME if needed
		PlaceInfo *spacedata = NULL;
		e2_pane_change_space_pointer (rt, spacedata);
#endif
		e2_pane_change_dir (rt, utf);
		g_free (utf);
		return GINT_TO_POINTER (2);	//non-NULL to signal no more refresh needed
	}

	g_atomic_int_set (&view->listcontrols.refresh_working, 1);
	E2_ListChoice pnum = (view == &app.pane1.view) ? PANE1:PANE2;
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable one refresh, fileview_refresh list");
#endif
	e2_filelist_disable_one_refresh (pnum);
	e2_window_set_cursor (GDK_WATCH);
	OPENBGL
#ifndef E2_STATUS_BLOCK
	if (view == curr_view)
		e2_window_disable_status_update ();	//prevents bad behaviour ??
#endif
	gchar *local = D_FILENAME_TO_LOCALE (view->dir); //always dup, to avoid dirchange race

	//first, get a fresh set of item-names from source directory
//	printd (DEBUG, "grab directory names data");
#ifdef E2_VFS
	VPATH ddata = { local, view->spacedata };
	GList *entries = (GList *)e2_fs_dir_foreach (&ddata,
#else
	GList *entries = (GList *)e2_fs_dir_foreach (local,
#endif
		E2_DIRWATCH_CHECK,	//this is irrelevant for non-local dirs
		NULL, NULL, NULL E2_ERR_NONE());

	gint retval; //pointerised version of this is returned

	if (!E2DREAD_FAILED (entries))
	{
/*		entries is now a list of allocated localised item-name strings.
		They're cleared when no longer needed. Some go into replacement
		liststore data and are cleared with the store. Others that belong to
		unchanged items are cleared when it's confirmed that they are the same.

		now we construct array to be populated with corresponding 'mode' data,
		indicating whether and how the item needs to be processed in this refresh.
*/
		guint i, indx, itemcount = g_list_length (entries);
		GList *member;
		E2_RefreshInfo *modes = (E2_RefreshInfo *)
#ifdef USE_GLIB2_10
			//can't use ALLOCATE0 cuz > 1 item
			g_slice_alloc0 (sizeof (E2_RefreshInfo) * itemcount);
#elif defined (USE_GLIB2_8)
			g_try_malloc0 (sizeof (E2_RefreshInfo) * itemcount);
#else
			//don't use calloc() so that g_free() is ok to clean up
			g_try_malloc (sizeof (E2_RefreshInfo) * itemcount);
#endif
#if (CHECKALLOCATEDWARNT)
		CHECKALLOCATEDWARNT (modes, {});
#endif
		if (modes != NULL)
		{
			retval = 1; //initially assume successful completion
#ifndef USE_GLIB2_8
			memset (modes, 0, sizeof (E2_RefreshInfo) * itemcount);
#endif
//			Now we convert item-names list into FileInfo's list

			//prevent downstream stat()'s causing ACCESS reports and perpetuate the refreshes
			e2_fs_FAM_less_monitor_dir (view->dir);
			//FIXME rationalise this with modes creation
			if (!e2_filelist_make_all_infos (local, &entries))
			{
				//cleanup of entries (possibly with mixed data types) done downstream
				//don't goto accessible path until next refresh, if any
				g_free (local);
#ifdef USE_GLIB2_10
				//can't use DEALLOCATE cuz > 1 item
				g_slice_free1 (sizeof (E2_RefreshInfo) * itemcount, modes);
#else
				g_free (modes);
#endif
				g_atomic_int_set (&view->listcontrols.refresh_working, 0);
				CLOSEBGL
				e2_window_set_cursor (GDK_LEFT_PTR);
				OPENBGL
				e2_fs_FAM_more_monitor_dir (view->dir);
				e2_filelist_enable_one_refresh (pnum);
				return NULL;
			}
			//even when FAM is working, some things (e.g. treedialog) must poll
			//for changes, so set times reflecting the completed poll
			struct stat sb;
#ifdef E2_VFS
			VPATH data = { local, view->spacedata };
			if (e2_fs_stat (&data, &sb E2_ERR_NONE()))	//through links
#else
			if (e2_fs_stat (local, &sb E2_ERR_NONE()))	//through links
#endif
			{
				printd (WARN, "Unable to stat directory: %s", view->dir);
				g_free (local);
#ifdef USE_GLIB2_10
				//can't use DEALLOCATE cuz > 1 item
				g_slice_free1 (sizeof (E2_RefreshInfo) * itemcount, modes);
#else
				g_free (modes);
#endif
				g_atomic_int_set (&view->listcontrols.refresh_working, 0);
				CLOSEBGL
				e2_window_set_cursor (GDK_LEFT_PTR);
				OPENBGL
				e2_fs_FAM_more_monitor_dir (view->dir);
				e2_filelist_enable_one_refresh (pnum);
				return GINT_TO_POINTER (3);	//no more refresh needed
			}
			view->dir_mtime = sb.st_mtime;
			view->dir_ctime = sb.st_ctime;

			e2_fs_FAM_more_monitor_dir (view->dir);

			//construct hash for faster matching current and new items
			//do not free keys when destroying, they're not copies
			GHashTable *newlookup = g_hash_table_new_full
				(g_str_hash, g_str_equal, NULL, NULL);
			i = 0;
			for (member = entries; member != NULL; member = member->next)
			{
				g_hash_table_insert (newlookup, ((FileInfo *)member->data)->filename,
					GUINT_TO_POINTER (i));
				i++;
			}

			//this bit may separately apply to both views when they're the same
			i = 0;
			guint newindx = 0;
			gpointer pindx, orig_key;
			GList *updates = NULL; //list of FileInfo's for changed or added items
			FileInfo *currinfoptr, *newinfoptr;
			GtkTreeIter iter;
			GtkTreeModel *mdl = GTK_TREE_MODEL (view->store);

			//walk current FileInfo's, reconciling with new ones in entries list,
			//setting modes[] data, removing gone items from current store,
			//cleaning up unused data in entries
			gtk_tree_model_get_iter_first (mdl, &iter); //there will always be at least one entry, ".."
			do
			{
loopstart:
				gtk_tree_model_get (mdl, &iter, FINFO, &currinfoptr, -1);
				if (g_hash_table_lookup_extended (newlookup, currinfoptr->filename,
					&orig_key, &pindx))
				{
					indx = GPOINTER_TO_UINT (pindx);
					newinfoptr = (FileInfo *) g_list_nth_data (entries, indx);
					//FIXME vfs may not have same data as local
					//FIXME make this faster
					if (newinfoptr->statbuf.st_atime != currinfoptr->statbuf.st_atime
					 ||	newinfoptr->statbuf.st_size != currinfoptr->statbuf.st_size
					 || newinfoptr->statbuf.st_mtime != currinfoptr->statbuf.st_mtime
					 || newinfoptr->statbuf.st_ctime != currinfoptr->statbuf.st_ctime
					 || newinfoptr->statbuf.st_mode != currinfoptr->statbuf.st_mode
					 || newinfoptr->statbuf.st_uid != currinfoptr->statbuf.st_uid
					 || newinfoptr->statbuf.st_gid != currinfoptr->statbuf.st_gid
					)
					{
						modes [indx].oldindx = i;
						modes [indx].oldmode = REFRESH_CHANGE;
						modes [indx].newindx = newindx++;
						updates = g_list_append (updates, newinfoptr);
					}
					else //all relevant data are unchanged
					{
						//we don't want to use or add this one
						modes [indx].oldmode = REFRESH_KEEP;
						//so now get rid of data to prevent leakage
#ifdef USE_GLIB2_10
						g_slice_free1 (sizeof (FileInfo), newinfoptr);
//						DEALLOCATE (FileInfo, newinfoptr);
#else
						DEALLOCATE (FileInfo, newinfoptr);
#endif
					}
					i++;
				}
				else //this stored item is not hashed i.e. gone from dir
				{
#ifdef USE_GLIB2_10
					g_slice_free1 (sizeof (FileInfo), currinfoptr);
#else
					DEALLOCATE (FileInfo, currinfoptr);
#endif
					CLOSEBGL
					gboolean more = gtk_list_store_remove (view->store, &iter);
					OPENBGL
					if (more)
						goto loopstart; //prevent double-iter-next, i unchanged
					else
						break;
				}
			} while (gtk_tree_model_iter_next (mdl, &iter));

			//tag and list any new/added items
			for (i = 0; i < itemcount; i++)
			{
				if (modes[i].oldmode == REFRESH_ADD)	//not previously detected = default (addition)
				{
					modes [i].newindx = newindx++;	//this will match the store index when created
					updates = g_list_append (updates, g_list_nth_data (entries, i));
				}
			}

			if (updates != NULL)
			{
//		printd (DEBUG, "create new filelist liststore with additions & replacements");
				//make store just with update information
				GtkListStore *newstore = e2_filelist_fill_store (updates, view);
				if (newstore != NULL)
				{
					//remember the current sorting arrangements
					GtkTreeSortable *sortable = GTK_TREE_SORTABLE (view->store);
					gtk_tree_sortable_get_sort_column_id (sortable, &view->sort_column,
						&view->sort_order);
//		printd (DEBUG, "update current store");
					//turn off model sorting before any rows are changed/added
					CLOSEBGL
					gtk_tree_sortable_set_sort_column_id (sortable,
						GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
					OPENBGL
					//walk modes array amending/appending relevant rows
					gchar *name, *size, *perm, *owner, *group, *mod, *access, *change, *key;
#ifdef E2_EXTCOL
					gchar *extn;
#endif
					GdkColor *foreground;
					for (i = 0; i < itemcount; i++)
					{
						if (modes[i].oldmode == REFRESH_CHANGE
							|| modes[i].oldmode == REFRESH_ADD)
						{
							//with bad unmounting, there may be no valid data
							if (!gtk_tree_model_iter_nth_child (GTK_TREE_MODEL (newstore),
								&iter, NULL, modes[i].newindx))
								continue;
							gtk_tree_model_get (GTK_TREE_MODEL (newstore), &iter,
								FILENAME, &name,
#ifdef E2_EXTCOL
								EXTN, &extn,
#endif
								SIZE, &size,
								PERM, &perm,
								OWNER, &owner,
								GROUP, &group,
								MODIFIED, &mod,
								ACCESSED, &access,
								CHANGED, &change,
								NAMEKEY, &key,
								FINFO, &newinfoptr,
								FORECOLOR, &foreground,
								-1);
							if (modes[i].oldmode == REFRESH_CHANGE)
							{
								gtk_tree_model_iter_nth_child (GTK_TREE_MODEL (view->store),
									&iter, NULL, modes[i].oldindx);
								gtk_tree_model_get (GTK_TREE_MODEL (view->store),
									&iter, FINFO, &currinfoptr, -1);
								//simply replace all changeable contents
								CLOSEBGL
								gtk_list_store_set (view->store, &iter,
									SIZE, size,
									PERM, perm,
									OWNER, owner,
									GROUP, group,
									MODIFIED, mod,
									ACCESSED, access,
									CHANGED, change,
									FORECOLOR, foreground,	//maybe changed executable status
									-1);
								OPENBGL
								//copy this so original can be cleared
								currinfoptr->statbuf = newinfoptr->statbuf;
							}
							else
							{	//append
								gtk_list_store_set (newstore, &iter, FINFO, NULL, -1); //no data clear with store
								CLOSEBGL
								//gtk >= 2.10 can handle &iter = NULL
								gtk_list_store_insert_with_values (view->store, &iter, -1,
									FILENAME, name,
#ifdef E2_EXTCOL
									EXTN, extn,
#endif
									SIZE, size,
									PERM, perm,
									OWNER, owner,
									GROUP, group,
									MODIFIED, mod,
									ACCESSED, access,
									CHANGED, change,
									NAMEKEY, key,
									FINFO, newinfoptr,
									FORECOLOR, foreground,
									-1);
								OPENBGL
							}

							g_free (name);
#ifdef E2_EXTCOL
							g_free (extn);
#endif
							g_free (size);
							g_free (perm);
							g_free (owner);
							g_free (group);
							g_free (mod);
							g_free (access);
							g_free (change);
							g_free (key);
						}
					}

					CLOSEBGL
					gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (view->model));
					//re-sort the view using the current sort column & direction
					gtk_tree_sortable_set_sort_column_id (sortable, view->sort_column,
						view->sort_order);
					OPENBGL
					gboolean newtimer = (app.used_stores == NULL);
			//CHECKME not all entries' member->data are cleared with newstore
					app.used_stores = g_slist_append (app.used_stores, newstore);
					if (newtimer)
					{
						printd (DEBUG, "setup to clear stores later");
						g_idle_add (e2_filelist_clear_old_stores, NULL);
					}
				}
				else //newstore = NULL
				{
					//store creation failed
					retval = 3; //CHECKME some other code ?
				}
				//no data free, the data are same as newstore FINFO data, and cleared with that
				g_list_free (updates);
			}
			else //updates = NULL (but there may have been deletion(s))
			{
//		printd (DEBUG, "no need to change any current item or add any item to filelist");
				guint filtered_rows = gtk_tree_model_iter_n_children (view->model, NULL);
				if (filtered_rows > 0)
				{
					if (view->row > filtered_rows - 1)
					{
						view->row = filtered_rows - 1;	//set to last line of list
						//CHECKME scroll treeview ?
					}
				}
				else
					view->row = 0;
			}

			view->total_items = gtk_tree_model_iter_n_children
				(GTK_TREE_MODEL (view->store), NULL) - 1; //omit the ".." entry from the count

			g_list_free (entries); //all data cleared or used before here
#ifdef USE_GLIB2_10
			//can't use DEALLOCATE cuz > 1 item
			g_slice_free1 (sizeof (E2_RefreshInfo) * itemcount, modes);
#else
			g_free (modes);
#endif
			g_hash_table_destroy (newlookup);
		}
		else //modes = NULL
		{
			printd (WARN, "Memory allocation problem, can't refresh %s", view->dir);
			e2_list_free_with_data (&entries);
			retval = 3; //error code CHECKME 4?
		}

#ifdef SYNC_DEBUG
		//don't wait for gtk to update at idle-time
		CLOSEBGL
		gdk_window_process_updates (
# ifdef USE_GTK2_14
			gtk_widget_get_window (view->treeview),
# else
			view->treeview->window,
# endif
			TRUE);
		OPENBGL
#endif
	}
	else //E2DREAD_FAILED (entries)
	{
		printd (WARN, "Failed to collect data about items is %s", view->dir);
/* TODO
		gchar *msg = g_strdup_printf (_ ("Failed to collect data about items in %s"), view->dir);
		CLOSEBGL
		e2_output_print_error (msg, TRUE);
		OPENBGL
*/
		retval = 3; //error code CHECKME 0? 4?
	}
	g_free (local);

	//signal to the world ...
#ifndef E2_STATUS_BLOCK
	if (view == curr_view)
		e2_window_enable_status_update (-1);	//revert disable which prevents bad behaviour ??
#endif
	g_atomic_int_set (&view->listcontrols.refresh_working, 0);
	CLOSEBGL
	e2_window_set_cursor (GDK_LEFT_PTR);
	OPENBGL

#ifdef DEBUG_MESSAGES
	if (app.reconvert_requested)
		printd (DEBUG, "repoint encoding conversion funcs if necessary, after frefresh");
	else
		printd (DEBUG, "NO need for encoding conversion change after refresh");
#endif

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, fileview_refresh list");
#endif
	e2_filelist_enable_one_refresh (pnum);
//	printd (DEBUG, "finish refresh filelist for %s", view->dir);
	return GINT_TO_POINTER (retval);	//non-NULL exit if no more refresh
}
/**
@brief create list store structure

Creates basic structure of list store, for panes filelists
No rows or data are added.

@return the new liststore
*/
GtkListStore *e2_filelist_make_store (void)
{
	GtkListStore *store = gtk_list_store_new (MODEL_COLUMNS,
		G_TYPE_STRING,  //FILENAME
#ifdef E2_EXTCOL
		G_TYPE_STRING,  //EXTN
#endif
		G_TYPE_STRING,  //SIZE
		G_TYPE_STRING,  //PERM
		G_TYPE_STRING,  //OWNER
		G_TYPE_STRING,  //GROUP
		G_TYPE_STRING,  //MODIFIED
		G_TYPE_STRING,  //ACCESSED
		G_TYPE_STRING,  //CHANGED
		// the rest are not displayed
		G_TYPE_STRING,  //NAMEKEY for i18n name sorts
		G_TYPE_POINTER,  //FINFO pr to FileInfo for the item
		GDK_TYPE_COLOR,  //FORECOLOR line colour
		GDK_TYPE_COLOR  //BACKCOLOR line colour
	//	G_TYPE_BOOLEAN	//VISIBLE whether filtered or not
		);
	return store;
}

#ifdef STORECOPY
/**
@brief helper function to copy each FileInfo stored in a filelist store

@param model the store to be updated
@param tpath UNUSED tree path of iter being processed ?
@param iter pointer to iter being processed
@param user_data UNUSED data specified when foreach was called

@return FALSE if the process can continue
*/
static gboolean _e2_filelist_copy_storeinfos (GtkTreeModel *model,
	GtkTreePath *tpath, GtkTreeIter *iter, gpointer user_data)
{
	FileInfo *info;
	gtk_tree_model_get (model, iter, FINFO, &info, -1);
	//FIXME duplicate the data
#ifdef USE_GLIB2_14
	info = (FileInfo *) g_slice_copy (sizeof (FileInfo), info);
	gtk_list_store_set (GTK_LIST_STORE (model), iter, FINFO, info, -1);
#else
	FileInfo *info2 = ALLOCATE (FileInfo);
	CHECKALLOCATEDWARN (info2, return TRUE;)
	memcpy (info2, info, sizeof (FileInfo));
	gtk_list_store_set (GTK_LIST_STORE (model), iter, FINFO, info2, -1);
#endif
//	gtk_tree_path_free (tpath); CRASHER
	return FALSE;
}
/**
@brief copy a filelist liststore with all its contents

@param original the store to be copied

@return the new liststore
*/
GtkListStore *e2_filelist_copy_store (GtkListStore *original)
{
	gpointer copied;
	e2_tree_store_copy (GTK_TREE_MODEL (original), FALSE, &copied);
	gtk_tree_model_foreach (GTK_TREE_MODEL ((GtkListStore *)copied),
		(GtkTreeModelForeachFunc) _e2_filelist_copy_storeinfos, NULL);
	return ((GtkListStore *)copied);
}
#endif
