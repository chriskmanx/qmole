/* $Id: e2_fs_walk.c 2746 2013-09-19 22:59:03Z tpgww $

Copyright (C) 2005-2013 tooar <tooar@emelfm2.net>

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
@file src/filesystem/e2_fs_walk.c
@brief tree-walk functionality, like nftw() but with user-data and additional flags
*/

/**
\page treewalks recursive file operations

Function e2_fs_tw() is comparable to nftw(), but with user data (to avoid global
variables in callback functions, so threads can be used) and extra parameters
(types of status-report provided to callback functions and types of return flags
from callback functions).

e2_fs_tw() may be called recursively, from any downstream function.

To instruct e2_fs_tw() how to perform its function, you may supply any one or more
of these flags:
E2TW_PHYS     perform physical walk, don't look though symlinks
E2TW_MOUNT    report only items on same file system as the starting item
E2TW_DEPTH    open and process any subdir as soon as it's found, instead of after
				all non-subdirs have been processed (UNUSED)
E2TW_ONLYDIR  no F, SL or SLN reports - report only directories and stat() fails
E2TW_NODIR    no D, DRR or DP reports - report only non-directories and
				stat() fails (NS) and dirs not-opened (DL, DM, DNR)
E2TW_FIXDIR   if possible, repair DNR situations and then report DRR instead of DNR
E2TW_QT       suppress in-walker error messages e.g. after intentional DL
E2TW_XQT      suppress ALL in-walker error messages
E2TW_XERR     (when E2_VFS is defined) the first item in user_data is a GError**
				for returning downstream error data
E2TW_DC       issue a DP report after some sorts of in-walker errors, and after
			  a callback returns STOP or a nested call returns STOP

Only the first 3 of these are in nftw(). Unlike nftw(), there is no CHDIR option,
as that's bad for multi-app and multi-thread contexts. So absolute paths must
always be used.

The effect of E2TW_DEPTH is really only on the order of "discovery", and so, on which
items have been processed after an error causes premature termination of a walk.
At this time, no walk is depth-first, and the code for that is disabled.

As appropriate, status report code(s) provided to the specified callback function
will be:
E2TW_F item is not a directory or link
E2TW_SL symbolic link to a file other than a directory
E2TW_SLN symbolic link whose target file is non-existent
E2TW_D	directory
E2TW_DL directory, not opened due to tree-depth limit
E2TW_DM directory, not opened due to different file system
E2TW_DNR unreadable directory
E2TW_DP directory processed (all its contents have been reported)
E2TW_NS un-stattable item
E2TW_DRR formerly-unreadable directory, now repaired

E2TW_D or E2TW_DRR, and E2TW_DP, will both be reported for each _opened_ directory
(unless processing is aborted by a STOP or SKIP_SUBTREE message from the callback,
from the D or DRR report, or aborted by some error in the walker itself).
E2TW_DP will not be reported after E2TW_DL, E2TW_DM or E2TW_DNR reports.
After a E2TW_DRR report, a DRRKEEP message has the same effect as CONTINUE

If a walk is aborted without a E2TW_DP report, any data parked by the callback
will leak unless it's cleaned by the parent routine, after the walk is finished.
Callbacks can report E2TW_CLEAN, which (after a F, SL, SLN, D or DRR report) will
trigger a DP callback and then stop. In a depth-first walk at least, that will
probably still leak unless there is a post-walk cleanup.
[Any??] in-walker error during a breadth-first walk will trigger a [DP] report
to allow local cleanup, and then stop.

Since the order of opening items in a dir is arbitrary, in breadth-first mode the
the walker defers processing of all subdirs until all non-subdirs are done.

e2_fs_tw() assumes that gtk's BGL is open/off, for the purposes of managing
display updating (error messages etc). Downstream callbacks must assume that too.

Error messages are displayed by the walker when E2TW_DL, E2TW_DM, E2TW_DNR,
E2TW_NS conditions arise, among other things, a relevant "quiet" flag applies
to the walk.
*/

#include "e2_fs.h"
#include <string.h>

/* twstat pointer to function to be used for stat oprations
(e2_fs_lstat() for E2TW_PHYS walks, e2_fs_stat() for others)
cbfunc pointer to the function to be called for each located item
user_data void pointer to user-specified data to be supplied to cbunc each
   time it's called (with GError** as its first member if E2TW_XERR is set)
max_depth the largest number of tree levels to descend, or -1 for no limit
exec_flags flags dictating the way the walk is to be performed
device the system on which the starting item resides
original_path newly-allocated localised path string, = CWD when the walk is started
*/
typedef struct _E2_TwData
{
#ifdef E2_VFS
	gint (*twstat) (VPATH *, struct stat *, GError **);
#else
	gint (*twstat) (const gchar *, struct stat *);
#endif
	E2_TwResult (*cbfunc) (VPATH *, const struct stat *, E2_TwStatus, gpointer);
	gpointer user_data;
	gint max_depth;	//-1 for unlimited
	E2_FsReadWatch watchdir;
	E2_TwFlags exec_flags;
	dev_t device;
} E2_TwData;

/**
@brief add to @a localpath any permission(s) in @a howflags that do not already apply

Intended mainly for dirs but will work for non-dirs too
Any error message expects BGL open
NOTE for GRP and OTH this does not work, for dirs at least
@param localpath absolute path of item to process, localised string
@param statptr pointer to statbuf for @a localpath, as provided to the treewalk cb func
@param howflags desired permission flags, R_OK W_OK and/or X_OK

@return mode of @a localpath, not masked with ALLPERMS, maybe same as current mode, 0 after error
*/
mode_t e2_fs_tw_adjust_dirmode (VPATH *localpath, const struct stat *statptr,
	gint howflags)
{
	E2_ERR_DECLARE
	if (!e2_fs_access3 (localpath, howflags E2_ERR_PTR()))
		return statptr->st_mode;
	E2_ERR_CLEAR

	mode_t pflags = 0;
	uid_t id = getuid ();
	if (id == 0 || id == statptr->st_uid)
	{
		if (howflags & R_OK) pflags |= S_IRUSR;
		if (howflags & W_OK) pflags |= S_IWUSR;
		if (howflags & X_OK) pflags |= S_IXUSR;
	}
	//NB for GRP and OTH this does not work, for dirs at least
	else if ((statptr->st_mode & S_IWGRP) //must already be group-writable for a member to change it
		&& e2_fs_ingroup (statptr->st_gid))
	{
		if (howflags & R_OK) pflags |= S_IRGRP;
		if (howflags & X_OK) pflags |= S_IXGRP;
	}
	else if (statptr->st_mode & S_IWOTH)//must already be other-writable for another to change it
	{
		if (howflags & R_OK) pflags |= S_IROTH;
		if (howflags & X_OK) pflags |= S_IXOTH;
	}

	mode_t newmode = 0;	//assignment for warning prevention
	if (pflags != 0)
		newmode = statptr->st_mode | pflags;
	if (pflags == 0	//nothing to change
		|| e2_fs_chmod (localpath, newmode & ALLPERMS E2_ERR_PTR()))
	{
		//X permission on dir when trying to process its contents
		gchar *fmt = ((howflags & X_OK) && S_ISDIR (statptr->st_mode)) ?
			_("Cannot change anything in %s") : _("Cannot change permissions of %s");
		e2_fs_error_local (fmt, localpath E2_ERR_MSGL());
		E2_ERR_CLEAR
		newmode = 0;
	}
	return newmode;
}
/**
@brief determine the appropriate status report code for link @a local

With relative symlinks, stat() generally fails, and anyway, can't distinguish
valid links from hanging ones. Nor can lstat().
So this uses a kludgy but robust alternative approach

@param localpath absolute path of link-item to process, localised string

@return callbck status code in accordance with whether the link is valid
*/
static E2_TwResult _e2_fs_tw_link (VPATH *localpath)
{
	E2_TwStatus code;
	gchar buf[PATH_MAX];
	gint read = e2_fs_readlink (localpath, buf, sizeof (buf) E2_ERR_NONE());
	if (read == -1)
	{
		code = E2TW_SLN;
	}
	else
	{
		buf[read] = '\0';
		//CHECKME should not matter that localpath is not necessarily a dir
		gchar *targetpath = e2_utils_translate_relative_path (VPSTR(localpath), buf);
		//targetpath has a trailing /, with which access() can't cope
		gint len = strlen (targetpath);
		if (len > 1)
			*(targetpath + len - sizeof (gchar)) = '\0';

		//access() looks through links
#ifdef E2_VFS
		VPATH data = { targetpath, localpath->spacedata };
		code = (e2_fs_access (&data, F_OK E2_ERR_NONE())) ? E2TW_SLN : E2TW_SL;
#else
		code = (e2_fs_access (targetpath, F_OK E2_ERR_NONE())) ? E2TW_SLN : E2TW_SL;
#endif
		g_free (targetpath);
	}
	return code;
}
/**
@brief process 'non-directory' item @a localpath, as part of a treewalk

This determines the appropriate status report code for the callback, then calls it
@a localpath may be the path of a link, regular file, etc
@a localpath will have been stat'd or lstat'd (depending on walk-flag E2TW_PHYS)
into @a statptr, before this function is called

@param localpath absolute path of item to process, localised string
@param statptr pointer to struct stat used for stating @a local
@param twdata pointer to struct with 'constant' data for each call here

@return result code in accordance with callback-function response
*/
static E2_TwResult _e2_fs_tw_nondir (VPATH *localpath, const struct stat *statptr,
	E2_TwData *twdata)
{
	E2_TwStatus code;
	if (twdata->exec_flags & E2TW_PHYS)
	{
		//the prior lstat did not look through a link
		//or the local lstat failed
		//in general, relative links (valid or not) will end up here
		code = (S_ISLNK (statptr->st_mode)) ? _e2_fs_tw_link (localpath) : E2TW_F;
	}
	else
	{
		//the prior stat() processed a non-link,
		//or might, or might not, have successfully processed a link
		//check if it was a link
		struct stat statbuf2;	//don't disturb statptr
		if (e2_fs_lstat (localpath, &statbuf2 E2_ERR_NONE()))
			code = E2TW_NS; //lstat failed
		else
			code = (S_ISLNK (statbuf2.st_mode)) ? _e2_fs_tw_link (localpath) : E2TW_F;
	}

	return ((*twdata->cbfunc) (localpath, statptr, code, twdata->user_data));
}
/**
@brief process directory @a localpath, as part of a treewalk

if possible, and in accord with the walk-mode determined by flags in @a twdata,
@a localpath is opened and its contents are listed and each listed item is
'reported' to the callback function recorded in @a twdata.
Any subdir found among the contents will recurse into this function.
Non-dirs are handled by another function, before coming here or when found here.
Assumes BGL open, for error messages.

@param localpath absolute path of _directory_ to process, localised string, with or without trailing "/"
@param depth enumator of current parent's level in the tree (relative
     to the original parent), 0 to max_depth (if the latter is not -1)
@param twdata pointer to struct with walk-parameters including callback and its data

@return result code in accordance with callback-function returns, E2TW_STOP or E2TW_CONTINUE if all was well
*/
static E2_TwResult _e2_fs_tw_dir (VPATH *localpath, gint depth, E2_TwData *twdata)
{
	//FIXME support an exit-code for cleanup-this-level then stop
	E2_TwResult result;
	gint newdepth, localpathlen;
	guint pathbuflen, bufspace;
	//CHECKME want these flags to be variable by any callback function ?
	gboolean reportdirs = !(twdata->exec_flags & E2TW_NODIR);
	gboolean reportnondirs = !(twdata->exec_flags & E2TW_ONLYDIR);

	gboolean repaired = FALSE;	//unreadable dir has been fixed
	mode_t firstmode = 0;	//original mode of an un-processable dir
	mode_t newmode = 0;	//fixed mode of an un-processable dir, for possible reinstatement

	gchar *itempath, *itemname;
	GList *entries, *parked_dirs, *member;
#ifdef E2_VFS
	VPATH ddata;
	GError **callerr;
	E2_ERR_DECLARE
#endif
	struct stat statbuf;	//assume it's ok to recursively stack this much data

	//populate a local statbuf, if we can
	if ((*twdata->twstat) (localpath, &statbuf E2_ERR_PTR()))
	{	//failed (statbuf will be garbage for this call)
		//since cbfunc doesn't have access to the error data, as a
		//convenience we report to the user unless instructed otherwise
		if (!(twdata->exec_flags & E2TW_XQT))
			e2_fs_error_local (_("Cannot get information about %s"),
				localpath E2_ERR_MSGL());

		result = (*twdata->cbfunc) (localpath, &statbuf, E2TW_NS, twdata->user_data);

#ifdef E2_VFS
		if (twdata->exec_flags & E2TW_XERR)
		{
			callerr = (GError **)twdata->user_data;
			if (callerr != NULL && *callerr == NULL)	//valid, unused error data
				*callerr = E2_ERR_NAME;
		}
		else
			E2_ERR_CLEAR
#endif
		//for an un-stattable item, bias to STOP
		return ((result == E2TW_CONTINUE) ? E2TW_CONTINUE : E2TW_STOP);
	}
	//check whether it's ok to open the dir
	if ((twdata->exec_flags & E2TW_MOUNT) && twdata->device != statbuf.st_dev)
	{
		if (!(twdata->exec_flags & (E2TW_QT | E2TW_XQT)))
			e2_fs_error_simple (_("Directory %s not opened"), localpath);
		result = (*twdata->cbfunc) (localpath, &statbuf, E2TW_DM, twdata->user_data);
		return ((result & E2TW_STOP) ? E2TW_STOP : E2TW_CONTINUE); //unwanted filesystem, would expect normally to continue
	}
	newdepth = depth + 1;
	if (twdata->max_depth != -1 && newdepth > twdata->max_depth)
	{
		if (!(twdata->exec_flags & (E2TW_QT | E2TW_XQT)))
			e2_fs_error_simple (_("Directory %s not opened"), localpath);
		result = (*twdata->cbfunc) (localpath, &statbuf, E2TW_DL, twdata->user_data);
		return ((result & E2TW_STOP) ? E2TW_STOP : E2TW_CONTINUE); //max depth reached, would expect normally to continue
	}

	//create a re-usable path-string buffer
	//since this function may recurse indefinitely, buffer is on heap, not stack
	localpathlen = strlen (VPSTR(localpath));
	//space to append a FileInfo->filename (NAME_MAX+1), plus a few chars (/ or 0)
	pathbuflen = (localpathlen + NAME_MAX + 1)/8*8 + 8;
#ifdef USE_GLIB2_10
	itempath = g_slice_alloc ((gulong) pathbuflen);
#else
	itempath = g_try_malloc ((gulong) pathbuflen);
#endif
//CHECKME support more-complex response(s) to this condition ?
#if (CHECKALLOCATEDWARNT)
	CHECKALLOCATEDWARNT (itempath, return E2TW_STOP;)
#else
	if (itempath == NULL)
	{
		CLOSEBGL
		e2_utils_show_memory_message ();
		OPENBGL
		return E2TW_STOP;
	}
#endif
	//get list of item-names in the dir
	entries = (GList *)e2_fs_dir_foreach (localpath, twdata->watchdir,
		NULL, NULL, NULL E2_ERR_PTR());
	//check whether it was possible to open the dir
	if (E2DREAD_FAILED (entries))
	{
		if (twdata->exec_flags & E2TW_FIXDIR)
		{	//we want to try fixing DNR instances
			firstmode = statbuf.st_mode;
			newmode = e2_fs_tw_adjust_dirmode (localpath, &statbuf, (R_OK | X_OK));
			if (newmode != firstmode && newmode != 0)
			{	//try again
				entries = (GList *)e2_fs_dir_foreach (localpath, twdata->watchdir,
					NULL, NULL, NULL E2_ERR_PTR());
			}
		}
		//was 1st or possible 2nd attempted read successful ?
		if (E2DREAD_FAILED (entries))
		{
			if (!(twdata->exec_flags & E2TW_XQT))
				e2_fs_error_local (_("Cannot open directory %s"),
					localpath E2_ERR_MSGL());

			result = (*twdata->cbfunc) (localpath, &statbuf, E2TW_DNR, twdata->user_data);

#ifdef E2_VFS
			if (twdata->exec_flags & E2TW_XERR)
			{
				callerr = (GError **)twdata->user_data;
				if (callerr != NULL && *callerr == NULL)	//valid, unused error data
					*callerr = E2_ERR_NAME;
			}
			else
				E2_ERR_CLEAR
#endif
#ifdef USE_GLIB2_10
			g_slice_free1 (pathbuflen, itempath);
#else
			g_free (itempath);
#endif
			return ((result & E2TW_STOP) ? E2TW_STOP : E2TW_CONTINUE);
		}
		repaired = TRUE;
	}

	if (reportdirs)
	{
		result = (*twdata->cbfunc) (localpath, &statbuf,
			(repaired) ? E2TW_DRR : E2TW_D, twdata->user_data);
/*		if (result & E2TW_CLEAN)	//CHECKME is this reasonable after a D/DRR ?
		{
			result |=
			((*twdata->cbfunc) (localpath, &statbuf, E2TW_DP, twdata->user_data));
			result |= E2TW_STOP;	//ensure a STOP
		}
*/
		if (repaired && (result & E2TW_DRKEEP))
			repaired = FALSE;	//prevent any later mode reversion

		if (result & (E2TW_STOP | E2TW_SKIPSUB))
		{
			if (repaired)
				e2_fs_chmod (localpath, firstmode E2_ERR_NONE());
			e2_list_free_with_data (&entries);
			return (result & E2TW_STOP) ? E2TW_STOP : E2TW_CONTINUE;
		}
	}
//==== DIR HAS BEEN READ & REPORTED AS SUCH

#ifdef __USE_GNU
	itemname = mempcpy (itempath, VPSTR (localpath), localpathlen);
#else
	memcpy (itempath, VPSTR (localpath), localpathlen);
	itemname = itempath + localpathlen;
#endif
	bufspace = pathbuflen - localpathlen;
	if (*(itemname - sizeof (gchar)) != G_DIR_SEPARATOR)	//ascii ok
	{
		*itemname = G_DIR_SEPARATOR;	//ascii ok
		itemname++;
		bufspace--;
	}
	//for logging dirs in breadth-first mode, to be processed when we're ready
	//to open them, after all non-dirs at this level have been processed
	parked_dirs = NULL;
#ifdef E2_VFS
	ddata.path = itempath;
	ddata.spacedata = localpath->spacedata;
#endif
	//walk the list of entries in the dir (tree-depth now = newdepth)
	for (member = entries; member != NULL ; member = member->next)
	{
		if (strcmp ((gchar *)member->data, "..") == 0)
			continue;

		g_strlcpy (itemname, (gchar *) member->data, bufspace);

#ifdef E2_VFS
		if ((*twdata->twstat) (&ddata, &statbuf E2_ERR_PTR()))
#else
		if ((*twdata->twstat) (itempath, &statbuf E2_ERR_PTR()))
#endif
		{	//process stat failure
			//NOTE statting a hanging link to a dir does NOT fail here, glib BUG ?
			if (!(twdata->exec_flags & E2TW_XQT))
				e2_fs_error_local (_("Cannot get information about %s"),
#ifdef E2_VFS
					&ddata E2_ERR_MSGL());
#else
					itempath E2_ERR_MSGL());
#endif
#ifdef E2_VFS
			result = (*twdata->cbfunc) (&ddata, &statbuf, E2TW_NS, twdata->user_data);
			if (twdata->exec_flags & E2TW_XERR)
			{
				callerr = (GError **)twdata->user_data;
				if (callerr != NULL && *callerr == NULL)	//valid, unused error data
					*callerr = E2_ERR_NAME;
			}
			else
				E2_ERR_CLEAR
#else
			result = (*twdata->cbfunc) (itempath, &statbuf, E2TW_NS, twdata->user_data);
#endif
			if ((result & E2TW_CLEAN) && reportdirs)
			{
				result |=
				((*twdata->cbfunc) (localpath, &statbuf, E2TW_DP, twdata->user_data));
				result |= E2TW_STOP;	//ensure a STOP
			}
			if (repaired && (result & E2TW_DRKEEP))
				repaired = FALSE;	//prevent any later mode reversion
			if (result & (E2TW_STOP | E2TW_SKIPSUB))
			{
				if (repaired)
					e2_fs_chmod (localpath, firstmode E2_ERR_NONE());
				if (parked_dirs != NULL)
					g_list_free (parked_dirs);
				e2_list_free_with_data (&entries);
#ifdef USE_GLIB2_10
				g_slice_free1 (pathbuflen, itempath);
#else
				g_free (itempath);
#endif
				return (result & E2TW_STOP) ? E2TW_STOP : E2TW_CONTINUE;
			}
		}
		else if (S_ISDIR (statbuf.st_mode))
		{	//stattable dir or (present) link-target that's a dir
/*			if (twdata->exec_flags & E2TW_DEPTH)
			{	//depth-first walk, open the dir now
				//(start- and end-reports issued in the recursed function)
				if (_e2_fs_tw (itempath, newdepth, twdata) != E2TW_CONTINUE)
				{
					//NOTE no DP report for this dir if aborted downstream
					//i.e. NO CLEAN CURRENT LEVEL
					//no parks to cleanup if depth-1st walk
					e2_list_free_with_data (&entries);
#ifdef USE_GLIB2_10
//					g_slice_free1 (itempathbuflen, itempath);
					g_slice_free1 (buflen, itempath);
#else
					g_free (itempath);
#endif
			FIXME more flags processing
					return E2TW_STOP;
				}
			}
			else
			{
*/
				//breadth-first walk
				//log the subdir for processing after all non-subdirs
				parked_dirs = g_list_prepend (parked_dirs,
					GINT_TO_POINTER (g_list_position (entries, member)));
//			}
		}
		else	//not a dir (it could be a hanging link to a dir)
			if (reportnondirs)
		{
#ifdef E2_VFS
			result = _e2_fs_tw_nondir (&ddata, &statbuf, twdata);
#else
			result = _e2_fs_tw_nondir (itempath, &statbuf, twdata);
#endif
			if ((result & E2TW_CLEAN) && reportdirs)
			{
				result |=
				((*twdata->cbfunc) (localpath, &statbuf, E2TW_DP, twdata->user_data));
				result |= E2TW_STOP;	//ensure a STOP
			}
			if (repaired && (result & E2TW_DRKEEP))
				repaired = FALSE;	//prevent later mode reversion
			if (result & E2TW_STOP)	//no SKIPSUB recognised here
			{
				if (repaired)
					e2_fs_chmod (localpath, firstmode E2_ERR_NONE());
				if (parked_dirs != NULL)
					g_list_free (parked_dirs);
				e2_list_free_with_data (&entries);
#ifdef USE_GLIB2_10
				g_slice_free1 (pathbuflen, itempath);
#else
				g_free (itempath);
#endif
				return E2TW_STOP;
			}
		}
	}
	//in a breadth-first walk, process the dirs that haven't yet been opened
	//(all of them, as their order is arbitrary)
	result = 0;
	for (member = parked_dirs ; member != NULL ; member = member->next)
	{
		gint indx = GPOINTER_TO_INT (member->data);
		gchar *parkname = g_list_nth_data (entries, indx);
		g_strlcpy (itemname, parkname, bufspace);
#ifdef E2_VFS
		result |= (_e2_fs_tw_dir (&ddata, newdepth, twdata));
#else
		result |= (_e2_fs_tw_dir (itempath, newdepth, twdata));
#endif
	}
	if (parked_dirs != NULL)
		g_list_free (parked_dirs);
	e2_list_free_with_data (&entries);
#ifdef USE_GLIB2_10
	g_slice_free1 (pathbuflen, itempath);
#else
	g_free (itempath);
#endif
	gboolean downfailed = (result & E2TW_STOP);
/*	//FIXME support cleanup then stop
	if (result & ?)
		....
	if (result & E2TW_STOP)
	{
		if (repaired)
			e2_fs_chmod (localpath, firstmode E2_ERR_NONE());
		return E2TW_STOP;
	}
*/
//==== FINISHED DIR-WALK
	//freshen the statbuf
	//(all downstream uses of the buffer are const, so shouldn't be necessary
	//except as another existence check ?)
	if ((*twdata->twstat) (localpath, &statbuf E2_ERR_PTR()))
	{	//stat failed (and advised as such)
		//can't issue valid DP report CHECKME do an invalid one ?
		if (reportdirs && (twdata->exec_flags & E2TW_DC))
			(*twdata->cbfunc) (localpath, &statbuf, E2TW_DP, twdata->user_data);

		if (repaired)	//perms changed prior to DRR report and cb did not want this ignored
		{
			//after stat() failure, this will probably fail too
			e2_fs_chmod (localpath, firstmode E2_ERR_NONE());
		}
#ifdef E2_VFS
		if (twdata->exec_flags & E2TW_XERR)
		{
			callerr = (GError **)twdata->user_data;
			if (callerr != NULL && *callerr == NULL)	//valid, unused error data
				*callerr = E2_ERR_NAME;
		}
		else
			E2_ERR_CLEAR
#endif
		return E2TW_STOP;
	}
	else	//final stat succeeded
	{
		if (reportdirs)
		{
			//issue dir-end report
			result = (*twdata->cbfunc) (localpath, &statbuf, E2TW_DP, twdata->user_data);
			if (repaired && (result & E2TW_DRKEEP))
				repaired = FALSE;	//prevent later mode reversion
		}
		else
			result = (downfailed) ? E2TW_STOP : E2TW_CONTINUE;

		if (repaired)	//perms changed prior to DRR report and no cb wanted this ignored
		{
			if (e2_fs_chmod (localpath, firstmode E2_ERR_PTR()))
			{
				if (!(twdata->exec_flags & E2TW_XQT))
					e2_fs_error_local (_("Cannot change permissions of %s"),
						localpath E2_ERR_MSGL());
#ifdef E2_VFS
				if (twdata->exec_flags & E2TW_XERR)
				{
					callerr = (GError **)twdata->user_data;
					if (callerr != NULL && *callerr == NULL)	//valid, unused error data
						*callerr = E2_ERR_NAME;
				}
				else
					E2_ERR_CLEAR
#endif
				return E2TW_STOP;
			}
		}
	}
	return (result & E2TW_STOP) ? E2TW_STOP : E2TW_CONTINUE;
}
/**
@brief locate and report all items in directory tree under @a start_dir

Potential error message expects gtk's BGL to be open. Downstream callbacks
must assume that too.

@param start_item absolute path of item (dir or non-dir) on|in which to start the walk, localised string
@param callback pointer to the function to be called for each located item
@param user_data pointer to user-specified data to be supplied to each call to @a callback
@param max_depth the largest number of tree levels to descend, or -1 for no limit
@param exec_flags flags indicating the way the walk is to be performed

@return TRUE if @a start_item (and any descendants) was processed in way(s) that @a callback considered ok
*/
gboolean e2_fs_tw (VPATH *start_item, E2_TwResult (*callback) (),
	gpointer user_data, gint max_depth, E2_TwFlags exec_flags E2_ERR_ARG())
{
	E2_TwResult result;
	struct stat statbuf;
	gint (*twstat) (VPATH *, struct stat *
#ifdef E2_VFS
		, GError **
#endif
	);
	E2_ERR_BACKUP (localerr);

	twstat = (exec_flags & E2TW_PHYS) ? e2_fs_lstat : e2_fs_stat;
	if ((twstat) (start_item, &statbuf E2_ERR_SAMEARG()))
	{
		if (!(exec_flags & E2TW_XQT))
			e2_fs_error_local (_("Cannot get information about %s"),
				start_item E2_ERR_MSGC());
		result = E2TW_STOP;
	}
	else
	{
		E2_FsReadWatch watchdir =
			(S_ISDIR (statbuf.st_mode) && e2_fs_mount_is_fusepoint (start_item)) ?
			E2_DIRWATCH_YES : E2_DIRWATCH_NO;
		//setup walk-data for downstream functions
		E2_TwData data =
		{ twstat, callback, user_data, max_depth, watchdir, exec_flags, statbuf.st_dev };

		result = (S_ISDIR (statbuf.st_mode)) ?
			_e2_fs_tw_dir (start_item, 0, &data):
			_e2_fs_tw_nondir (start_item, &statbuf, &data); //should never be needed, but ...
	}

#ifdef E2_VFS
	if (exec_flags & E2TW_XERR)
	{
		GError **callerr = (GError **)user_data;
		if (callerr != NULL && *callerr != NULL)	//valid, unused error data
			*E2_ERR_NAME = *callerr;
	}
#endif
	E2_ERR_CLEARBACKUP (localerr);

	return (result == E2TW_CONTINUE);
}
