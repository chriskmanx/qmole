/* $Id: e2_fs_FAM.c 2473 2012-03-16 02:58:12Z tpgww $

Copyright (C) 2005-2012 tooar <tooar@emelfm2.net>.

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
@file src/filesystem/e2_fs_FAM.c
@brief directory/file content change management functions

This file contains functions related to detection of changes to the content of
directory(ies) displayed in the file panes.
Some of the code relates to monitoring with 'gamin' or with the original FAM.
Kernel-specific monitoring code is elsewhere.
*/

#include "emelfm2.h"

#if defined(E2_FAM) && !defined(E2_FAM_KERNEL)
//using gamin for monitoring
/**
@brief initialise gamin connection

The gamin-specific part of this is FAMNoExists()
This creates a shared event data struct for both panes (toggle the request no &
userdata as appropriate)
Sets app.monitor_type, E2_MONITOR_GAMIN if the connection succeeded, else
E2_MONITOR_DEFAULT

@return
*/
void e2_fs_FAM_connect (void)
{
	app.fcp = ALLOCATE0 (FAMConnection);
	CHECKALLOCATEDWARN (app.fcp, return;)
	if (FAMOpen2 (app.fcp, BINNAME))
	{
//		printd (WARN, "Cannot initialise files monitor");
		DEALLOCATE (FAMConnection, app.fcp);
		app.monitor_type = E2_MONITOR_DEFAULT;
		return;
	}
	//	CHECKME any merit in runtime detection whether FAM or GAMIN ??
	app.monitor_type = E2_MONITOR_GAMIN;

#ifdef E2_GAMIN
	//block existence confirmation events - for dirs, not for files
	//gamin (0.1.5 at least) blocks Exist etc EVENTS ONLY after FAMMonitorDirectory[2] ();
	FAMNoExists (app.fcp);
#endif

	app.fep = ALLOCATE0 (FAMEvent);
	CHECKALLOCATEDWARN (app.fep, return;)
	app.fep->fc = app.fcp;

	return;
}
/**
@brief terminate FAM/gamin connection at session end or when monitoring is cancelled

gamin itself not affected

@return
*/
void e2_fs_FAM_disconnect (void)
{
	if (app.monitor_type == E2_MONITOR_FAM
		|| app.monitor_type == E2_MONITOR_GAMIN)
	{
		FAMClose (app.fcp);
		DEALLOCATE (FAMConnection, app.fcp);
		DEALLOCATE (FAMEvent, app.fep);
	}
}
/**
@brief change monitored directory

if old-dir not also in the other pane, cancel old-dir monitoring
if new-dir not also in other pane, start monitoring new-dir
If new-dir is not monitored (FAM error or dir is already monitored in other pane)
the FAM request for the pane is set to -1
if new-dir is a link (which should not be able to be traversed) then we monitor
its target

@a olddir needs trailing '/', for valid comparison with rt->view.dir etc

@param olddir utf-8 string with absolute path of dir to stop monitoring
@param rt data struct for the pane to which the cd applies, including the new dir in rt->view.dir

@return
*/
void e2_fs_FAM_change (gchar *olddir, E2_PaneRuntime *rt)
{
	if (app.monitor_type == E2_MONITOR_DEFAULT) //monitor mechanism not (yet) set up
		return;
	//cancel monitoring of current dir if that was happening
	//at session-start, both panes have rt->FAMreq == -1
	if (rt->FAMreq != -1)
	{
		//use the request for this pane
		app.fep->fr.reqnum = rt->FAMreq;
		if (FAMCancelMonitor (app.fcp, &app.fep->fr))
		{
			printd (DEBUG, "FAM cancel for %s failed", olddir);
			//FIXME handle error;
		}
		else
		{
			printd (DEBUG, "FAM cancelled for %s", olddir);
//#ifndef E2_GAMIN
			//FAMAcknowledge event ??
			//gamin < 0.1 did not do this, tho' FAM API doco says there is one
			if (FAMPending (app.fcp) > 0)
			{
				FAMNextEvent (app.fcp, app.fep);
				if (app.fep->code == FAMAcknowledge)
					printd (DEBUG, "FAMAcknowledge received for %s", olddir);
			}
//#endif
		}
		//default flag = no-monitoring this pane
		rt->FAMreq = -1;
	}
	E2_PaneRuntime *ort = (rt == curr_pane) ? other_pane : curr_pane;
#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#else
	if (!strcmp (olddir, ort->view.dir))
#endif
	{	//panes are showing same dir now
		//connect to the other pane if need be, to get correct pane id
		//when monitoring
		if (ort->FAMreq == -1)
		{
			guint watch_id = (ort == &app.pane1) ? E2PANE1 : E2PANE2;
#ifdef E2_VFSTMP
			//FIXME path for non-mounted dirs
#else
			gchar *local = D_FILENAME_TO_LOCALE (ort->view.dir);
#endif
			e2_fs_walk_link (&local E2_ERR_NONE());

			if (FAMMonitorDirectory (app.fcp, local, &app.fep->fr, GUINT_TO_POINTER (watch_id)))
			{
				printd (DEBUG, "FAM init for %s failed", ort->view.dir);
				//FIXME handle error;
			}
			else
			{
				//remember the request no. for this pane
				ort->FAMreq = FAMREQUEST_GETREQNUM(&app.fep->fr);
				printd (DEBUG, "FAM now monitoring %s", ort->view.dir);
			}
			g_free (local);
		}
	}
	//now hoookup to the new dir, if it's not going already
#ifdef E2_VFSTMP
	//FIXME path for non-mounted dirs
#else
	if (strcmp (ort->view.dir, rt->view.dir))	//NB panes may be same at session start
#endif
	{	//new dir not already monitored
		guint watch_id = (rt == &app.pane1) ? E2PANE1 : E2PANE2;
#ifdef E2_VFSTMP
		//FIXME path for non-mounted dirs
#else
		gchar *local = D_FILENAME_TO_LOCALE (rt->view.dir);
#endif
		e2_fs_walk_link (&local E2_ERR_NONE());

		if (FAMMonitorDirectory (app.fcp, local, &app.fep->fr, GUINT_TO_POINTER (watch_id)))
		{
			printd (DEBUG, "FAM init for %s failed", rt->view.dir);
			//FIXME handle error;
		}
		else
		{
			//remember the request no. for this pane
			rt->FAMreq = FAMREQUEST_GETREQNUM(&app.fep->fr);
			printd (DEBUG, "FAM now monitoring %s", rt->view.dir);
		}
		g_free (local);
	}
	else
		rt->FAMreq = -1;
}
/**
@brief setup monitoring of config file
Set up with data = E2PANECONF, to distinguish this from panes 1 & 2 monitoring

@return TRUE if the connection was successfully established
*/
gboolean e2_fs_FAM_monitor_config (void)
{
	gchar *cfg_file = g_build_filename (e2_cl_options.config_dir, default_config_file, NULL);
	gchar *local = F_FILENAME_TO_LOCALE (cfg_file);

	if (FAMMonitorFile (app.fcp, local, &app.fep->fr, GUINT_TO_POINTER (E2PANECONF)))
	{
		printd (DEBUG, "FAM init for %s failed", cfg_file);
		g_free (cfg_file);
		F_FREE (local, cfg_file);
		return FALSE;
	}
	printd (DEBUG, "FAM init for %s succeeded", cfg_file);
//#ifndef E2_GAMIN
	//siphon off FAMAcknowledge, FAMExists and FAMEndExist events
	while (FAMPending (app.fcp) > 0)
	{
		FAMNextEvent (app.fcp, app.fep);
		//FAMAcknowledge event ??
//		if (app.fep->code == FAMAcknowledge)
//			printd (DEBUG, "FAMAcknowledge received for %s", config_file);
	}
//#endif
	g_free (cfg_file);
	F_FREE (local, cfg_file);
	//remember the request no. for this file
	app.FAMreq = FAMREQUEST_GETREQNUM(&app.fep->fr);
	return TRUE;
}
/**
@brief cancel monitoring of config file

@return TRUE if the connection was successfully removed
*/
gboolean e2_fs_FAM_cancel_monitor_config (void)
{
#ifdef DEBUG_MESSAGES
	gchar *config_file = g_build_filename (e2_cl_options.config_dir, default_config_file, NULL);
#endif
	//use the request for this file
	app.fep->fr.reqnum = app.FAMreq;

	if (FAMCancelMonitor (app.fcp, &app.fep->fr))
	{
#ifdef DEBUG_MESSAGES
		printd (DEBUG, "FAM cancel for %s failed", config_file);
		g_free (config_file);
#endif
		return FALSE;
	}
	printd (DEBUG, "FAM cancel for %s succeeded", config_file);
//#ifndef E2_GAMIN
	//siphon off FAMAcknowledge, FAMExists and FAMEndExist events
	while (FAMPending (app.fcp) > 0)
	{
		FAMNextEvent (app.fcp, app.fep);
		//FAMAcknowledge event ??
//		if (app.fep->code == FAMAcknowledge)
//			printd (DEBUG, "FAMAcknowledge received for %s", config_file);
	}
//#endif
#ifdef DEBUG_MESSAGES
	g_free (config_file);
#endif
	return TRUE;
}
/**
@brief poll monitor to check if any monitored thing has changed

Called from e2_filelist_check_dirty() via e2_fs_FAM_check_dirty().
This updates flags to signal that refresh is needed. For a pane, update is
needed if any directory content has changed, or if the dir is not
accessible/readable or gone
A missing config file is not reported (as we can't reload it anyway)

@param p1dirty pointer to location to store 1|0 for whether pane 1 needs refresh
@param p2dirty pointer to location to store 1|0 for whether pane 2 needs refresh
@param cfgdirty pointer to location to store 1|0 for whether config needs refresh

@return
*/
static void _e2_fs_FAM_poll (volatile gint *p1dirty, volatile gint *p2dirty,
	volatile gint *cfgdirty)
{
	gint dop1, dop2, docfg;
	//make sure the monitored dir(s) are still valid
	//gamin (0.1.5 at least) will not have reported inaccessible/renamed/deleted dir(s)
	//access () traverses links (ok) and does not change atime (ok)
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
	gchar *local = D_FILENAME_TO_LOCALE (app.pane1.path); //always dup, to avoid dirchange race
#endif
	dop1 = (e2_fs_access (local, R_OK | X_OK E2_ERR_PTR())) ? 1:0;
	g_free (local);
#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
#else
	local = D_FILENAME_TO_LOCALE (app.pane2.path);
#endif
	dop2 = (e2_fs_access (local, R_OK | X_OK E2_ERR_PTR())) ? 1:0;
	g_free (local);
	//not interested whether config is still present
	docfg = 0;

	if (FAMPending (app.fcp) >= 1)
	{
		//new files trigger a Created event then a Changed event
		while (FAMPending (app.fcp) > 0)
		{
			FAMNextEvent (app.fcp, app.fep);	//no error check
			//FAM (when starting to monitor any dir or file) and gamin
			//(when starting to monitor config file) generate events
			//we're not interested in
			if (app.fep->code == FAMExists
				|| app.fep->code == FAMEndExist)
					continue;
			guint watch_id = GPOINTER_TO_UINT (app.fep->userdata);
			//CHECKME this reports if a monitored dir is renamed/deleted ?
			switch (watch_id)
			{
				case E2PANE1:
					dop1 = 1;
				//mirrored panes need to get the same status
					if (app.pane2.FAMreq == -1)
						dop2 = 1;
					break;
				case E2PANE2:
					dop2 = 1;
					if (app.pane1.FAMreq == -1)
						dop1 = 1;
					break;
				case E2PANECONF:
					docfg = 1;
					break;
				default:
					break;
			}
		}
	}
	g_atomic_int_set (p1dirty, dop1);
	g_atomic_int_set (p2dirty, dop2);
	g_atomic_int_set (cfgdirty, docfg);
}
#endif  //defined(E2_FAM) && !defined(E2_FAM_KERNEL)

/**
@brief check whether contents of either filepane (and with kernel-type FAM, config too) need to be refreshed
Filepane(s) are marked 'dirty' if dir is no longer present, or if its contents
are changed. The latter will be determined by the applicable fam, which at a
minimum involves comparing dir modification and change times against stored values
Those times are updated if refresh is needed and not 'blocked'

@param p1dirty pointer to flag, set 1 if pane 1 needs refreshing or is gone
@param p2dirty pointer to flag, set 1 if pane 2 needs refreshing or is gone
@if FAM
@param cfgdirty pointer to flag, set 1 if config file needs refreshing, only used if FAM/gamin in use
@endif

@return
*/
void e2_fs_FAM_check_dirty (volatile gint *p1dirty, volatile gint *p2dirty
#ifdef E2_FAM
	, volatile gint *cfgdirty
#endif
	)
{
#ifdef E2_FAM
	if (app.monitor_type != E2_MONITOR_DEFAULT)
	{
# ifndef E2_FAM_KERNEL
		_e2_fs_FAM_poll (p1dirty, p2dirty, cfgdirty);
# endif
	}
	else
	{
#endif

#ifdef E2_VFS
		VPATH ddata;
#endif
		struct stat statbuf;
		gboolean state;
#ifndef E2_FAM_KERNEL
		gboolean busy;
#endif
		gchar *local = D_FILENAME_TO_LOCALE (app.pane1.view.dir); //always dup, to avoid dirchange race
#ifdef E2_VFS
		ddata.path = local;
		ddata.spacedata = app.pane1.view.spacedata;
		state = e2_fs_access (&ddata, R_OK | X_OK E2_ERR_NONE()); //signal dir is gone now !
#else
		state = e2_fs_access (local, R_OK | X_OK E2_ERR_NONE()); //signal dir is gone now !
#endif
		if (!state)
		{
#ifdef E2_VFS
			if (!e2_fs_stat (&ddata, &statbuf E2_ERR_NONE()))
#else
			if (!e2_fs_stat (local, &statbuf E2_ERR_NONE()))
#endif
				state =
					statbuf.st_mtime != app.pane1.view.dir_mtime
				 || statbuf.st_ctime != app.pane1.view.dir_ctime;
			else
				state = TRUE;
		}
#ifndef E2_FAM_KERNEL
		if (state)
		{
//			LISTS_LOCK
			busy = g_atomic_int_get (&app.pane1.view.listcontrols.cd_working)
				|| g_atomic_int_get (&app.pane1.view.listcontrols.refresh_working);
//			LISTS_UNLOCK
			if (!busy)
				g_atomic_int_set (&app.pane1.view.listcontrols.refresh_requested, 1);
		}
#endif
		g_atomic_int_set (p1dirty, state ? 1:0);
		g_free (local);

		local = D_FILENAME_TO_LOCALE (app.pane2.view.dir); //always dup, to avoid dirchange race
#ifdef E2_VFS
		ddata.path = local;
		ddata.spacedata = app.pane2.view.spacedata;
		state = e2_fs_access (&ddata, R_OK | X_OK E2_ERR_NONE());
#else
		state = e2_fs_access (local, R_OK | X_OK E2_ERR_NONE());
#endif
		if (!state)
		{
#ifdef E2_VFS
			if (!e2_fs_stat (&ddata, &statbuf E2_ERR_NONE()))
#else
			if (!e2_fs_stat (local, &statbuf E2_ERR_NONE()))
#endif
				state =
					statbuf.st_mtime != app.pane2.view.dir_mtime
				 || statbuf.st_ctime != app.pane2.view.dir_ctime;
			else
				state = TRUE;
		}
#ifndef E2_FAM_KERNEL
		if (state)
		{
//			LISTS_LOCK
			busy = g_atomic_int_get (&app.pane2.view.listcontrols.cd_working)
				|| g_atomic_int_get (&app.pane2.view.listcontrols.refresh_working);
//			LISTS_UNLOCK
			if (!busy)
				g_atomic_int_set (&app.pane2.view.listcontrols.refresh_requested, 1);
		}
#endif
		g_atomic_int_set (p2dirty, state ? 1:0);
		g_free (local);

#ifdef E2_FAM
		if (e2_option_bool_get ("auto-refresh-config")
//			&& e2_option_check_config_dir ()
		)
		{
			gchar *cfg_file = g_build_filename (e2_cl_options.config_dir, default_config_file, NULL);
			local = F_FILENAME_TO_LOCALE (cfg_file);
			//CHECKME check for R_OK (conservative)?
#ifdef E2_VFS
			ddata.path = local;
			ddata.spacedata = NULL;	//local config files only
			if (!e2_fs_stat (&ddata, &statbuf E2_ERR_NONE()))
#else
			if (!e2_fs_stat (local, &statbuf E2_ERR_NONE()))
#endif
			{
				state = (statbuf.st_mtime != app.config_mtime);
			}
			else
				//don't report if the config file is missing
				state = FALSE;
			if (state)
				g_atomic_int_set (cfgdirty, 1);

			g_free (cfg_file);
			F_FREE (local, cfg_file);
		}
	}
#endif
}

#ifndef E2_FAM
/**
@brief initilise config file refresh baseline timestamp

@return
*/
void e2_fs_FAM_config_stamp (void)
{
	gchar *cfg_file = g_build_filename (e2_cl_options.config_dir, default_config_file, NULL);
	gchar *local = F_FILENAME_TO_LOCALE (cfg_file);
#ifdef E2_VFS
	VPATH ddata = { local, NULL };	//only local config data supported
#endif
	struct stat stat_buf;
#ifdef E2_VFS
	if (!e2_fs_stat (&ddata, &stat_buf E2_ERR_NONE()))
#else
	if (!e2_fs_stat (local, &stat_buf E2_ERR_NONE()))
#endif
		app.config_mtime = stat_buf.st_mtime;
	g_free (cfg_file);
	F_FREE (local, cfg_file);
}
#endif
