/* $Id: e2p_shred.c 2979 2013-11-30 05:42:37Z tpgww $

Copyright (C) 2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; for the most part you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option) any
later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

#include "emelfm2.h"

#include <fcntl.h>

#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_task.h"
#include "e2_fs.h"
#include "e2_filelist.h"

#ifdef _LARGEFILE_SOURCE	//if we cater for "large" files (DEFAULT)
# define csize_t guint64
#else //if we cater only for files up to 2 GB (NOT the default)
# define csize_t guint32
#endif

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "shred"

static PluginIface iface;

static gboolean _e2p_shredQ (E2_ActionTaskData *qed);

/**
@brief get a random value in range 0 .. 255

@param retval pointer to store for value

@return TRUE if the value was filled
*/
static guint8 _e2p_shred_getrandom (void)
{
	guint8 retval;
	E2_FILE *randFile = e2_fs_open_stream
#if defined(__linux__) || defined(__solaris__) || defined(darwin) || defined(__OpenBSD__) || defined(AIX)
	//CHECKME which other OS's ?
	("/dev/urandom", "r");
#else
	("/dev/random", "r");
#endif
	if (randFile != NULL)
	{
		retval = getc (randFile);
		e2_fs_close_stream (randFile);
	}
	else
	{
		printd (DEBUG, "cannot open random-number source");
		retval = (guint8)((gulong)&retval >> 8);
	}
	return retval;
}
/**
@brief produce a plausible fake modify-time for @a localpath

@param localpath

@return
*/
static time_t _e2p_shred_gettime (VPATH *localpath)
{
	time_t faketime;
	struct stat statbuf;

	guint8 random = _e2p_shred_getrandom ();
	const gchar *root = g_get_home_dir ();
	if (!g_str_has_prefix (VPCSTR(localpath), root))
		root = G_DIR_SEPARATOR_S;
	//get current times
#ifdef E2_VFSTMP
	VPATH sdata = { root, NULL };
	if (e2_fs_stat (&sdata, &statbuf E2_ERR_NONE()))
#else
	if (e2_fs_stat (root, &statbuf E2_ERR_NONE()))
#endif
	{
		printd (WARN, "Cannot get time-properties of %s", VPCSTR(localpath));
//		E2_ERR_CLEAR
		faketime = time (NULL) - 3600 * random * random;
	}
	else
	{
		//get random time between likely earliest, and now
		time_t then = statbuf.st_ctime;
		printd (DEBUG, "Base time %i for path '%s'", then, root);
		time_t now = time (NULL);
		printd (DEBUG, "Current time %i", now);
		faketime = then + (now - then) * random / 256;
	}
	return faketime;
}
/**
@brief produce a plausible fake path & name for @a localpath

@param localpath absolute path of item to be processed, localised string

@return allocated path string (localised)
*/
static gchar *_e2p_shred_getpath (VPATH *localpath)
{
	//TODO try to ensure root is on same device as localpath
	gchar *usertmp = NULL;
	const gchar *root = g_get_home_dir ();
	if (g_str_has_prefix (VPCSTR(localpath), root))
	{
		//try for ~/tmp
		usertmp = g_build_filename (root, "tmp", NULL);
#ifdef E2_VFS
		VPATH sdata = { usertmp, NULL};
		if (e2_fs_access3 (&sdata, W_OK E2_ERR_NONE()) == 0)
#else
		if (e2_fs_access3 (usertmp, W_OK E2_ERR_NONE()) == 0)
#endif
			root = (gchar*)usertmp;
		else
			root = g_get_user_data_dir ();
	}
	else
		//TODO check this root is writable
		root = g_get_tmp_dir ();

	guint8 randomval = _e2p_shred_getrandom ();
	guchar *p;
	gchar *base = g_path_get_basename (VPCSTR(localpath));

	guint8 extralen = randomval / 64 + 1;
	gchar e[extralen+1];
	memset (e, 'A', extralen);
	e[extralen] = '\0';

	gchar *tmp = base;
	base = e2_utils_strcat (tmp, e);
	g_free (tmp);

	for (p = (guchar *)base; *p != '\0'; p++)
	{
		guchar c = *p;
		c = ((c & randomval) + 'a') & 0x7f;
		while (c == G_DIR_SEPARATOR || c == '"' || c == '\'')
			c = ((c & randomval) + '0') & 0x7f;
		*p = c;
	}

	gchar *fakename = g_build_filename (root, base, NULL);
	g_free (base);
	g_free (usertmp);

	return fakename;
}
/**
@brief change name & some times & some permissions of @a localpath, then delete it

@param localpath

@return TRUE if deletion succeeds (or is not attempted, during debugging)
*/
static gboolean _e2p_shred_hide_item (VPATH *localpath E2_ERR_ARG())
{
	gchar *newpath = _e2p_shred_getpath (localpath);
	//rename item (which changes its ctime to now)
#ifdef E2_VFS
	VPATH ddata = { newpath, localpath->spacedata };
	e2_task_backend_move (localpath, &ddata);
#else
	e2_task_backend_move (localpath, newpath);
#endif
	//cuz we probably overwrote file with content from executable(s)
	if (e2_fs_chmod (localpath, 0700 E2_ERR_NONE()))
	{
		printd (WARN, "Cannot change permissions of %s", VPCSTR(localpath));
	}
	//mask file m, atimes - random dates in suitable range
	time_t now = time (NULL);
//	printd (DEBUG, "Time now %i", now);
	time_t fake = _e2p_shred_gettime (localpath);
//	printd (DEBUG, "Fake mtime %i", fake);
	guint8 randomval = _e2p_shred_getrandom ();
	struct utimbuf tb;
	tb.modtime = fake;
	tb.actime = fake + 24 * 3600 * (time_t)randomval / 256 + (time_t)randomval;
	while (tb.actime > now)
		tb.actime -= 3600;
//	printd (DEBUG, "Fake atime %i", tb.actime);
#ifdef E2_VFS
	e2_fs_utime (&ddata, &tb E2_ERR_NONE());
#else
	e2_fs_utime (newpath, &tb E2_ERR_NONE());
#endif
	//ctime can only be changed by temporarily altering system time,
	//probably not a good thing and needs superuser privilege

	gboolean retval;
#if 1 //delete - maybe disable while debugging
	//delete
#ifdef E2_VFS
	retval = e2_task_backend_delete (&ddata);
#else
	retval = e2_task_backend_delete (newpath);
#endif
#else
	retval = TRUE;
#endif
	g_free (newpath);
	return retval;
}
/**
@brief write @a buffer out to storage
This allows full or partial writing of a file, to support streaming and
otherwise-segemented output
Any error message expects BGL to be open
@param localpath localised name of item being read, used only for error messages
@param descriptor file descriptor
@param buffer store for pointer to buffer holding data to write
@param buffersize size of @a filebuffer, > 0

@return TRUE if the write was completed
*/
static gboolean _e2p_shred_write_buffer (VPATH *localpath, gint descriptor,
	gpointer buffer, /*size_t*/ gulong buffersize)
{
	if (buffersize > 0)
	{
		E2_ERR_DECLARE
		ssize_t bytes_written = e2_fs_write (descriptor, buffer,
			buffersize E2_ERR_PTR());
		if ((gulong)bytes_written < buffersize)
		{
#ifdef E2_VFS
			e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Error writing file %s"), localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
	}
	return TRUE;
}
/**
@brief fill @a buffer with the contents of some file from $PATH
This is an alternative to storing some sequence of data that is readily
recognisable as over-written data
Expects BGL to be open on arrival here
@param buffer pointer to buffer to be overwritten
@param buffersize size of @a buffer
@param the number of times to wipe

@return TRUE if the process was completed
*/
static gboolean _e2p_shred_randomise_buffer (gpointer buffer, size_t buffersize,
	guint times)
{
	gboolean retval = FALSE;
	gchar *sep;
	gchar *found = NULL;
	gchar *execpath = (gchar *)g_getenv ("PATH");
	if (execpath == NULL)
	{
		found = g_find_program_in_path (BINNAME);
		if (found != NULL)
		{
			gchar *s = strrchr (found, G_DIR_SEPARATOR);
			*s = '\0';
			execpath = found;
		}
		else
			execpath = "/bin";
		sep = NULL;
	}
	else
	{
		sep = strchr (execpath, ':');	//ascii scan ok
		if (sep != NULL)
			execpath = g_strndup (execpath, sep-execpath);
		//FIXME preserve execpath so that later members can be used
	}
#ifdef E2_VFS
	VPATH ddata = { execpath, NULL };	//files in $PATH must be local
	GList *entries = (GList *)e2_fs_dir_foreach (&ddata,
#else
	GList *entries = (GList *)e2_fs_dir_foreach (execpath,
#endif
		E2_DIRWATCH_NO,	//local = fast read
		NULL, NULL, NULL E2_ERR_NONE());

	if (E2DREAD_FAILED (entries))
	{
		//FIXME try another dir in PATH, or ...
		//FIXME warn user
//		e2_fs_error_simple (
//			_("You do not have authority to read %s"), execpath);
		if (sep != NULL)
			g_free (execpath);
		if (found != NULL)
			g_free (found);
		return FALSE;
	}
	guint count = g_list_length (entries);
	while (times > 0)
	{
		guint8 c;
restart:
		c = _e2p_shred_getrandom ();
		guint first = count * c / 256;
		guint i = 0;
		gchar *filename, *filepath = NULL;
		GList *member;
reloop:
		for (member = g_list_nth (entries, first); member != NULL; member = member->next)
		{
			filename = (gchar *)member->data;
			if (strcmp (filename, ".."))
			{
				filepath = g_build_filename (execpath, filename, NULL);
#ifdef E2_VFS
				ddata.path = filepath;
				if (!e2_fs_access (&ddata, R_OK E2_ERR_NONE()))
#else
				if (!e2_fs_access (filepath, R_OK E2_ERR_NONE()))
#endif
					break;
				g_free (filepath);
			}
			filepath = NULL;

			if (++i == count);
			{
				//try with next dir from PATH or ...
				printd (DEBUG, "cannot find a file for data source");
				//FIXME warn user
//				e2_fs_error_simple (
//					_("You do not have authority to read anything in %s"), execpath);
				goto cleanup;
			}
		}
		if (member == NULL && i < count)
		{	//reached end of list, cycle back to start
			first = 0;
			goto reloop;
		}
		if (filepath == NULL)
			goto cleanup;

		E2_ERR_DECLARE
		gint fdesc = e2_fs_safeopen (filepath, O_RDONLY, 0);
		if (fdesc < 0)
		{
			printd (DEBUG, "Cannot open data source file");
			goto restart;	//try with another file from list
/*  or ...
#ifdef E2_VFS
			e2_fs_set_error_from_errno (E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Cannot open '%s' for reading"), filepath E2_ERR_MSGL());
			E2_ERR_CLEAR
			goto cleanup;
*/
		}

		struct stat sb;
#ifdef E2_VFS
		e2_fs_stat (&ddata, &sb E2_ERR_NONE());
#else
		e2_fs_stat (filepath, &sb E2_ERR_NONE());
#endif
		csize_t masksize = (csize_t) sb.st_size;
		ssize_t n_read;

		if (masksize >= buffersize)
		{
			n_read = e2_fs_read (fdesc, buffer, buffersize E2_ERR_PTR());
			if (n_read < buffersize)
			{
//#ifdef E2_VFS
//				e2_fs_set_error_from_errno (&E2_ERR_NAME);
//#endif
//				e2_fs_error_local (_("Error reading file %s"), localpath E2_ERR_MSGL());
				E2_ERR_CLEAR
				//FIXME handle shortfall
			}
		}
		else
		{	//mask-file is smaller than the buffer, read repeatedly until buffer is full
			csize_t readsofar = 0;
			guchar *readPtr = buffer;
			while (readsofar < buffersize)
			{
				n_read = e2_fs_read (fdesc, readPtr, masksize E2_ERR_PTR());
				if (n_read < masksize)
				{
//#ifdef E2_VFS
//					e2_fs_set_error_from_errno (&E2_ERR_NAME);
//#endif
//					e2_fs_error_local (_("Error reading file %s"), localpath E2_ERR_MSGL());
					E2_ERR_CLEAR
					//FIXME handle shortfall
				}
				lseek (fdesc, 0, SEEK_SET);	//FIXME vfs
				readsofar += masksize;
				readPtr += masksize;
				if (readsofar > (buffersize - masksize))
					masksize = buffersize - readsofar;
			}
		}

		//FIXME page buffer to disk, to mask any swap storage

		e2_fs_safeclose (fdesc);
		times--;
	}
	retval = TRUE;
cleanup:
	if (sep != NULL)
		g_free (execpath);
	e2_list_free_with_data (&entries);

	if (found != NULL)
		g_free (found);
	return retval;
}
/**
@brief delete @a localpath after overwriting it with the contents of some file from /bin
Any error message here expects BGL to be open
@param localpath absolute path of item to be processed, localised string

@return TRUE if the process was completed
*/
static gboolean _e2p_shred_flush_file (VPATH *localpath E2_ERR_ARG())
{
	struct stat sb;
	if (e2_fs_stat (localpath, &sb E2_ERR_SAMEARG()))
	{
		e2_fs_error_local (_("Cannot get current data for %s"),
			localpath E2_ERR_MSGC());
		E2_ERR_PCLEAR
		return FALSE;
	}

	guint8 randomval = _e2p_shred_getrandom ();	//fudge the size
	csize_t wipesize = (csize_t) sb.st_size + (csize_t) randomval;

	//find a buffer up to 64 times file's block-size
	csize_t buffersize = sb.st_blksize * 64;
	while (buffersize > wipesize)
		buffersize /= 2;
	if (buffersize < wipesize && buffersize < sb.st_blksize)
		buffersize = wipesize;
	gpointer buffer;
	while ((buffer = malloc (buffersize)) == NULL)
	{
		if (buffersize < sb.st_blksize)
		{
			CLOSEBGL
			e2_utils_show_memory_message ();
			OPENBGL
			return FALSE;
		}
		buffersize /= 2;
	}
	//open file for writing without truncation
	gint fdesc = e2_fs_safeopen (VPCSTR (localpath), O_RDWR | O_NONBLOCK, 0);
	if (fdesc < 0)
	{
		g_free (buffer);
#ifdef E2_VFS
		e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
		e2_fs_error_local (_("Cannot open '%s' for writing"), localpath E2_ERR_MSGL());
		E2_ERR_CLEAR
		return FALSE;
	}

	gboolean retval = FALSE;
//	flockfile (outputFile);
	if (buffersize == wipesize)
	{
		if (!_e2p_shred_randomise_buffer (buffer, buffersize, 2)
		 || !_e2p_shred_write_buffer (localpath, fdesc, buffer, buffersize))
		{
			//FIXME error message
//			e2_fs_error_simple (
//				_("You do not have authority to modify %s"), (*iterator)->filename);
			goto cleanup;
		}
	}
	else
	{
		csize_t writesofar = 0;
		csize_t bsize = buffersize;
		while (writesofar < wipesize)
		{
			if (_e2p_shred_randomise_buffer (buffer, bsize, 2)
			 && _e2p_shred_write_buffer (localpath, fdesc, buffer, bsize))
			{
				writesofar += bsize;
				if (writesofar > (wipesize - buffersize))
					bsize = wipesize - writesofar;
			}
			else
			{
				//FIXME error message
//				e2_fs_error_simple (
//					_("You do not have authority to modify %s"), (*iterator)->filename);
				goto cleanup;
			}
		}
	}

	e2_fs_writeflush (fdesc); //should never fail

	retval = TRUE;
cleanup:
	g_free (buffer);
//	funlockfile (outputFile);
	e2_fs_safeclose (fdesc);

	return retval;
}
/**
@brief obfuscate and delete file @a localpath after faking its contents

@param localpath absolute path of item to be processed, localised string

@return TRUE if wipe is completed
*/
static gboolean _e2p_shred_file1 (VPATH *localpath E2_ERR_ARG())
{
	return (_e2p_shred_flush_file (localpath E2_ERR_SAMEARG())
		 && _e2p_shred_hide_item (localpath E2_ERR_SAMEARG()));
}
/**
@brief obfuscate and delete directory @a localpath

@a localpath is assumed to be empty

@param localpath absolute path of item to be processed, localised string

@return TRUE if wipe is completed
*/
static gboolean _e2p_shred_dir1 (VPATH *localpath E2_ERR_ARG())
{
	return (_e2p_shred_hide_item (localpath E2_ERR_SAMEARG()));
}
/**
@brief obfuscate and delete link @a localpath after faking its target

@param localpath absolute path of item to be processed, localised string

@return TRUE if wipe is completed
*/
static gboolean _e2p_shred_link1 (VPATH *localpath E2_ERR_ARG())
{
//	E2_ERR_BACKUP (localerr);
	gchar *newpath = _e2p_shred_getpath (localpath);
	gchar *base = g_path_get_basename (newpath);
	g_free (newpath);
	newpath = e2_utils_strcat ("../../", base);
	g_free (base);
#ifdef E2_VFS
	VPATH tdata;
	tdata.path = newpath; tdata.spacedata = localpath->spacedata;
	if (e2_fs_symlink (localpath, &tdata E2_ERR_NONE()))
#else
	if (e2_fs_symlink (localpath, newpath E2_ERR_NONE()))
#endif
	{
		printd (WARN, "Failed to re-target link %s", VPCSTR(localpath));
	}
	g_free (newpath);

	return (_e2p_shred_hide_item (localpath E2_ERR_SAMEARG()));
}
/**
@brief obfuscate and delete device @a localpath

@param localpath absolute path of item to be processed, localised string

@return TRUE if wipe is completed
*/
static gboolean _e2p_shred_device1 (VPATH *localpath E2_ERR_ARG())
{
	//TODO change st_mode if possible
	//c.f. mknod (>> length 0) mkfifo
	return (_e2p_shred_hide_item (localpath E2_ERR_SAMEARG()));
}
/**
@brief delete item as part of recursive directory delete
This is a callback for the treewalk function
The return value does not change after a failure to delete, so that everything
possible will be deleted, The ultimate parent-deletion will generate an
appropriate error message after any failure
Downstream error messasge expects BGL open
@param localpath absolute path of item reported by the walker, localised string
@param statptr pointer to struct stat with data about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data UNUSED NULL pointer unless E2_VFS defined, in which case maybe a GError**

@return E2TW_CONTINUE or E2TW_SKIPSUB
*/
static E2_TwResult _e2p_twcb_shred (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, gpointer user_data)
{
	E2_TwResult retval = E2TW_CONTINUE;
	E2_ERR_DECLARE

	switch (status)
	{
		case E2TW_DP:	//dir completed
			if (!_e2p_shred_dir1 (localpath E2_ERR_PTR()))
			{
				e2_fs_error_local (_("Cannot delete %s"),
					localpath E2_ERR_MSGL());
#ifndef E2_VFS
				E2_ERR_CLEAR
#endif
			}
			break;
		case E2TW_DRR:
			retval = E2TW_DRKEEP;	//no need for walker to revert mode
		case E2TW_D:	//directory
			if (e2_fs_tw_adjust_dirmode (localpath, statptr, (W_OK | X_OK)) == 0)
			{
				//failed to set W and/or X perm, can't process any item in the dir
				//no DP report after skip, so try to delete the dir, probably fails
				if (!_e2p_shred_dir1 (localpath E2_ERR_PTR()))
				{
					e2_fs_error_local (_("Cannot delete %s"),
						localpath E2_ERR_MSGL());
#ifndef E2_VFS
					E2_ERR_CLEAR
#endif
				}
				retval |= E2TW_SKIPSUB;
			}
			break;
/*		case E2TW_F:	//not directory or link
		case E2TW_SL:	//symbolic link
		case E2TW_SLN:	//symbolic link naming non-existing file
		try to delete these, fail if not empty etc
		case E2TW_DL:	//dir, not opened due to tree-depth limit (reported upstream)
		case E2TW_DM:	//dir, not opened due to different file system (reported upstream)
		case E2TW_DNR:	//unreadable dir (for which, error is reported upstream)
*/
		default:
			//don't care if this fails
			e2_fs_tw_adjust_dirmode (localpath, statptr, W_OK);
		case E2TW_NS:	//un-stattable item (error reported upstream)
			if (!_e2p_shred_device1 (localpath E2_ERR_PTR()))
				retval |= E2TW_STOP;
			break;
	}

#ifdef E2_VFS
	if (user_data != NULL)
	{
//		if (exec_flags & E2TW_XERR)
//		{
		GError **callerr = (GError **)user_data;
		if (*callerr == NULL)
			*callerr = E2_ERR_NAME;
		else
			E2_ERR_CLEAR
//		}
	}
	else
		E2_ERR_CLEAR
#endif

	return retval;
}
/**
@brief delete item @a itempath
When applied to a link, that is NOT traversed, so if it is a link
to a directory (which requires a recursive delete) that dir
will be untouched (unless/until processed in its own right, anyway).
For dirs, errors other than ENOTEMPTY return FALSE.
Not-empty dirs trigger a recursive delete, bottom-up,
again, no link traverses (dir's in the same tree will be processed
at some stage, in their own right)
No error processing other than ENOTEMPTY (OK?)
Assumes BGL is open
CHECKME always return TRUE so the process tries to continue?

@param localpath localised string, relative path of item to delete

@return TRUE if succeeds
*/
static gboolean _e2p_backend_shred (VPATH *localpath E2_ERR_ARG())
{
	if (e2_fs_mount_is_mountpoint (localpath))
		return FALSE;
	//decide whether or not item is a dir, not looking through links
	struct stat statbuf;
#ifdef E2_VFS
	gboolean retval;
#endif
	if (e2_fs_lstat (localpath, &statbuf E2_ERR_SAMEARG()))
	{
#ifdef E2_VFS
		retval = E2_ERR_PIS (ENOENT);	//ok if nothing to delete, otherwise error
		if (retval)
		{
			E2_ERR_PCLEAR
		}
		return retval;
#else
		return (E2_ERR_PIS (ENOENT));	//ok if nothing to delete, otherwise error
#endif
	}
	if (S_ISDIR (statbuf.st_mode))
	{	//dir, recursively delete its contents
		//FIXME which errors to report and/or ignore
		return (e2_fs_tw (localpath, _e2p_twcb_shred,
#ifdef E2_VFS
		E2_ERR_NAME
#else
		NULL
#endif
		, -1,
			//flags for: cb-error, no link follow, depth-first
#ifdef E2_VFS
			E2TW_XERR |
#endif
		E2TW_PHYS E2_ERR_SAMEARG()));
	}
	else if (S_ISREG (statbuf.st_mode))
		return (_e2p_shred_file1 (localpath E2_ERR_SAMEARG()));
	else if (S_ISLNK (statbuf.st_mode))
		return _e2p_shred_link1 (localpath E2_ERR_SAMEARG());
	else if (S_ISBLK (statbuf.st_mode)
		  || S_ISCHR (statbuf.st_mode)
		  || S_ISSOCK (statbuf.st_mode)
		  || S_ISFIFO (statbuf.st_mode))
		return _e2p_shred_device1 (localpath E2_ERR_SAMEARG());
	else
		return FALSE;
}

/**
@brief delete item @a localpath
@param localpath data ibncluding localised string, absolute path of item to delete
@return TRUE if delete succeeds
*/
static gboolean _e2p_dowipe (VPATH *localpath)
{
	E2_ERR_DECLARE
	if (_e2p_backend_shred (localpath E2_ERR_PTR()))
		return TRUE;

	e2_fs_error_local (_("Failed to remove %s"), localpath E2_ERR_MSGL());
	E2_ERR_CLEAR
	return FALSE;
}

/**
@brief delete selected items in the active pane

Confirmation is sought, if that option is enabled
The actual operation is performed by a separate back-end function

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if task completed successfully, else FALSE
*/
static gboolean _e2p_shred (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_DELETE, art, from,
		_e2p_shredQ, e2_task_refresh_lists));
}
static gboolean _e2p_shredQ (E2_ActionTaskData *qed)
{
	printd (DEBUG, "plugin: wipe");
	gboolean success, retval = TRUE;
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

	e2_filelist_disable_refresh ();  //avoid pauses in the delete process
	e2_task_advise ();

	for (count = 0; count < names->len; count++, iterator++)
	{
		gboolean permitted;

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
				case OK:
					success = _e2p_dowipe
#ifdef E2_VFS
					(&sdata);
#else
					(src->str);
#endif
					retval = retval && success;
				case CANCEL:
					break;
//				case NO_TO_ALL:
				default:
					result = NO_TO_ALL;
					break;
			}
			if (result == NO_TO_ALL)
				break;
		}
		else  //no confirmation
		{
			success = _e2p_dowipe
#ifdef E2_VFS
				(&sdata);
#else
				(src->str);
#endif
			retval = retval && success;
		}
	}
	g_string_free (prompt, TRUE);
	g_string_free (src, TRUE);
	e2_window_clear_status_message ();
	e2_filelist_enable_refresh ();

	return retval;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(6),_("shred"),_e2p_shred,
		_("_Shred"),
		_("Thoroughly delete selected items"),
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
	return ret;
}
