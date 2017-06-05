/* $Id: e2_task_backend.c 3044 2014-02-08 07:11:47Z tpgww $

Copyright (C) 2005-2014 tooar <tooar@emelfm2.net>

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
@file src/e2_task_backend.c
@brief task backend functions

This file contains functions used to handle different file operations
like copying, deleting, symlinking, etc.
The 'public' functions are all named like e2_task_<...>_backend ()
They return TRUE on successful completion
Private functions are named like _e2_task_backend_<...> ()
These functions are thread-safe, when E2_TREEWALK is defind
*/

#include "e2_task.h"
#include <string.h>
#include <pthread.h>
#include "e2_filetype.h"
#include "e2_filelist.h"
#include "e2_dialog.h"

//try to use acl-plugin functionality (if available) to replicate item permissions when copying
#define ACL_PERMS
//a (*plugin-func) (const gchar*, const struct stat *, const gchar *), for
//preserving acl's when copying (actually, _e2p_acl_copyacls())
//some fanciness-hacks used to avoid a mutex lock/unlock each time this func is used
//only used when acl-plugin is loaded, static variable is ok
gpointer copyaclfunc = NULL;

/**
@brief store operation-specific flags in @a data
@param optype enum for the type of operation being performed
@param opmode mode flags to be used in the operation
@param data pointer to data struct
@return
*/
static void _e2_task_backend_parse_mode (E2_ChmodType optype, mode_t opmode,
	E2_ChmodData *data)
{
	switch (optype)
	{
		case E2_CHMOD_SET:
			data->clearmask = ~ALLPERMS; //clear everything
			data->setmask = opmode;
			break;
		case E2_CHMOD_ADD:
			data->clearmask = ALLPERMS; //clear nothing
			data->setmask = opmode;
			break;
		case E2_CHMOD_REM:
			data->clearmask = ~opmode;
			data->setmask = 0;
			break;
		default:	//should never happen
			printd (DEBUG, "Ignoring bad mode string for chmod");
			data->clearmask = ALLPERMS;
			data->setmask = 0;
			break;
	}
}
/**
@brief set permissions of item @a path to @a mode
This is used when changing a single item, or recursively changing items.
Possible errors:
Name errors - see _e2_task_nftwfunc_delete documentation
 *ENOENT
 The named file doesn't exist.
 *EPERM
 This process does not have permission to change the access permissions
of this file. Only the file's owner (as judged by the effective user ID of the process)
or a privileged user can change them.
 *EROFS
 The file resides on a read-only file system.
 *EFTYPE
mode has the S_ISVTX ("sticky") bit set, and the named file is not a directory.

@param localpath data including localised string, absolute path of item to be processed
@param mode mode_t to be set
@return TRUE if succeeds
*/
static gboolean _e2_task_backend_chmod1 (VPATH *localpath, mode_t mode E2_ERR_ARG())
{
	E2_ERR_BACKUP (localerr);

	if (e2_fs_chmod (localpath, mode E2_ERR_SAMEARG())
		&& E2_ERR_PISNOT (ENOENT))	//CHECKME any other errors to ignore ??
	{
		e2_fs_error_local (_("Cannot change permissions of %s"), localpath E2_ERR_MSGC());
		E2_ERR_CLEARBACKUP (localerr);
		return FALSE;
	}
	E2_ERR_CLEARBACKUP (localerr);
	return TRUE;
}
/**
@brief callback function for recursive directory chmods

Tree is being walked breadth-first, not physical.
Dirs are made accessible and writable if not already so and it's permitted,
dirs are added to a list to be processed after all the tree has been traversed,
other items are changed as requested (if possible)
Error messages expect BGL open
@param localpath absolute path of item to change, localised string
@param statptr pointer to struct stat with info about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data pointer to user-specified data

@return E2TW_CONTINUE if succeeds, others as appropriate
*/
static E2_TwResult _e2_task_twcb_chmod (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, E2_ChmodData *user_data)
{
	E2_TwResult retval = E2TW_CONTINUE;	//default error code = none
	E2_ERR_DECLARE

	switch (status)
	{
		mode_t mode, newmode;
		E2_DirEnt *dirfix;
		GList *member;

		case E2TW_DP:	//dir completed
			//change/revert dir permissions, cleanup
			for (member = g_list_last (user_data->dirdata); member != NULL; member = member->prev)
			{
				dirfix = member->data;
				if (dirfix != NULL)
				{
					if (!strcmp (dirfix->path, localpath))
					{
						//try to set new or original mode
						if (!_e2_task_backend_chmod1 (localpath, dirfix->mode E2_ERR_PTR()))
							retval = E2TW_FIXME;
						g_free (dirfix->path);
						DEALLOCATE (E2_DirEnt, dirfix);
						user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
						break;
					}
				}
//				else //should never happen CHECKME ok when walking list ?
//					user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
			}
			break;
		case E2TW_DRR:	//directory now readable
			if (user_data->scope & E2_RECURSE_DIRS)
				retval |= E2TW_DRKEEP;	//no permission reversion in walker
		case E2TW_D:
			//ensure dir is writable, if we can
			if (e2_fs_tw_adjust_dirmode (localpath, statptr, (W_OK | X_OK)) == 0)
			{
				if (user_data->scope & E2_RECURSE_DIRS)
				{
					//failed to set missing W and/or X perm
					//take a shot at doing the change, anyhow, probably fails
					mode = ((statptr->st_mode & user_data->clearmask) | user_data->setmask) & ALLPERMS;
					e2_fs_chmod (localpath, mode E2_ERR_NONE());
					//FIXME warn user about failure
					retval |= E2TW_SKIPSUB;	//don't try to do any descendant
				}
			}
			else	//dir can be processed
			{
				//add this dir to list of items to chmod afterwards
				dirfix = ALLOCATE (E2_DirEnt);
				CHECKALLOCATEDWARNT (dirfix, retval=E2TW_STOP;break;)
				dirfix->path = g_strdup (VPSTR(localpath));
				if (user_data->scope & E2_RECURSE_DIRS)
					dirfix->mode = ((statptr->st_mode & user_data->clearmask)
					| user_data->setmask) & ALLPERMS;
				else
					dirfix->mode = statptr->st_mode & ALLPERMS;	//want to restore the original value
				user_data->dirdata = g_list_append (user_data->dirdata, dirfix);
			}
			break;
		case E2TW_DM:	//dir not opened (reported upstream)
		case E2TW_DL:	//ditto
		case E2TW_DNR:	//unreadable directory (for which, error is reported upstream)
			if (user_data->scope & E2_RECURSE_DIRS)
			{
				//chmod for this will probably fail, but try anyhow
//				mode = statptr->st_mode;
				//ensure dir is writable, if we can
				//we don't need X permission to handle these
				newmode = e2_fs_tw_adjust_dirmode (localpath, statptr, W_OK);
				mode = ((statptr->st_mode & user_data->clearmask) | user_data->setmask) & ALLPERMS;
				if (newmode == 0)
				{
					//take a shot at doing the change, anyhow, probably fails
					e2_fs_chmod (localpath, mode E2_ERR_NONE());
					//FIXME warn user about failure
					retval = E2TW_FIXME;
				}
				else	//dir can be processed
				{
					if (!_e2_task_backend_chmod1 (localpath, mode E2_ERR_PTR()))
						retval = E2TW_FIXME;
					//FIXME warn user about failure
				}
			}
			break;
		case E2TW_F:
			if (user_data->scope & E2_RECURSE_OTHER)
			{  //change mode now, if we can
//				mode = (statptr->st_mode & user_data->clearmask) | user_data->setmask;
//				if (S_ISREG (statptr->st_mode))
//					mode &= ALLPERMS;
				mode = ((statptr->st_mode & user_data->clearmask) | user_data->setmask) & ALLPERMS;
				if (!_e2_task_backend_chmod1 (localpath, mode E2_ERR_PTR()))
					retval = E2TW_FIXME;
			}
		case E2TW_SL:	//no mode changes for links
		case E2TW_SLN:
			break;
		case E2TW_NS:	//un-statable item (for which, error is reported upstream)
			retval = E2TW_FIXME;
			break;
		default:
			retval = E2TW_STOP;
			break;
	}

#ifdef E2_VFS
	if (user_data->operr != NULL && *(user_data->operr) == NULL)
		*(user_data->operr) = E2_ERR_NAME;
	else
		E2_ERR_CLEAR
#endif

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
@brief set owners of item @a path
This is used when changing a single item, or recursively changing items.
Possible errors:
Name errors - see _e2_task_nftwfunc_delete documentation
 *EPERM
 This process lacks permission to make the requested change.
 Only privileged users or the file's owner can change the file's group.
 On most file systems, only privileged users can change the file owner;
 some file systems allow you to change the owner if you are currently the owner.
 *EROFS
  The file is on a read-only file system.

@param localpath localised string, absolute path of item to be processed
@param statbuf pointer to a 'completed' stat struct for @a path
@param owner_id owner id to be set
@param group_id group id to be set

@return TRUE if succeeds
*/
static gboolean _e2_task_backend_chown1 (VPATH *localpath, struct stat *statbuf,
	uid_t owner_id, gid_t group_id E2_ERR_ARG())
{
	gint (*chown_func) () = (S_ISLNK (statbuf->st_mode)) ?
		e2_fs_lchown : e2_fs_chown;

	E2_ERR_BACKUP (localerr);

	if (chown_func (localpath, owner_id, group_id E2_ERR_SAMEARG())
		&& E2_ERR_PISNOT (ENOENT))	//don't care if the item is not there
	{
		e2_fs_error_local (_("Cannot change ownership of %s"), localpath E2_ERR_MSGC());
		E2_ERR_CLEARBACKUP (localerr);
		return FALSE;
	}
	E2_ERR_CLEARBACKUP (localerr);
	return TRUE;
}
/**
@brief helper function for recursive directory chowns
The tree is being scanned breadth-first, no link-through.
Dirs are made accessible if not already so and it's permitted,
dirs are added to a list to be processed after all the tree has
been traversed, other items are changed as requested (if possible)
Error messages expect BGL open
@param localpath absolute path of item to change, localised string
@param statptr pointer to struct stat with info about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data pointer to user-specified data

@return completion code: E2TW_CONTINUE if succeeds, others as appropriate
*/
static E2_TwResult _e2_task_twcb_chown (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status,  E2_ChownData *user_data)
{
	E2_TwResult retval = E2TW_CONTINUE;	//default error code = none
	E2_ERR_DECLARE

	switch (status)
	{
		mode_t mode, newmode;
		E2_DirEnt *dirfix;
		GList *member;

 		case E2TW_DP:	//dir completed
			//chown and revert dir's permissions, cleanup
			for (member = g_list_last (user_data->dirdata); member != NULL; member = member->prev)
			{
				dirfix = (E2_DirEnt *)member->data;
				if (dirfix != NULL)
				{
					if (!strcmp (dirfix->path, localpath))
					{
						if (!_e2_task_backend_chown1 (localpath, (struct stat *)statptr,
							user_data->new_uid, user_data->new_gid E2_ERR_PTR()))
								retval = E2TW_FIXME;
						else if (!_e2_task_backend_chmod1 (localpath,
								dirfix->mode E2_ERR_PTR()))
									retval = E2TW_FIXME;
						g_free (dirfix->path);
						DEALLOCATE (E2_DirEnt, dirfix);
						user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
						break;
					}
				}
//				else //should never happen CHECKME ok when walking list ?
//					user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
			}
			break;
		case E2TW_DRR:	//directory now readable
		case E2TW_D:
			//ensure dir is writable, if we can
			if (e2_fs_tw_adjust_dirmode (localpath, statptr, (W_OK | X_OK)) == 0)
			{
				//take a shot at doing the change, anyhow, probably fails
				_e2_task_backend_chown1 (localpath, (struct stat *) statptr,
					user_data->new_uid, user_data->new_gid E2_ERR_PTR());
				//FIXME warn user about failure
				retval = E2TW_SKIPSUB;	//don't try to do any descendant
			}
			else	//dir can be processed
			{
				//add this dir to list of items to chmod afterwards
				dirfix = ALLOCATE (E2_DirEnt);
				CHECKALLOCATEDWARNT (dirfix, retval=E2TW_STOP;break;)
				dirfix->path = g_strdup (VPSTR(localpath));
				dirfix->mode = statptr->st_mode & ALLPERMS;	//want to restore the original value
				user_data->dirdata = g_list_append (user_data->dirdata, dirfix);
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
				_e2_task_backend_chown1 (localpath, (struct stat *) statptr,
					user_data->new_uid, user_data->new_gid E2_ERR_PTR());
				//FIXME warn user about failure
				retval = E2TW_FIXME;
			}
			else	//dir can be processed
			{
				if (!_e2_task_backend_chown1 (localpath, (struct stat *) statptr,
					user_data->new_uid, user_data->new_gid E2_ERR_PTR()))
						retval = E2TW_FIXME;	//E2TW_STOP;	//prefer continue ??
				if (newmode != mode)
					e2_fs_chmod (localpath, mode & ALLPERMS E2_ERR_NONE());
			}
			break;
		case E2TW_F:
		case E2TW_SL:  //valid and invalid links
		case E2TW_SLN: //broken links (CHECKME not reported as E2TW_PHYS is used)
			if (!_e2_task_backend_chown1 (localpath, (struct stat *) statptr,
					user_data->new_uid, user_data->new_gid E2_ERR_PTR()))
				retval = E2TW_FIXME;	//E2TW_STOP;	//prefer continue ??
			break;
		case E2TW_NS:	//un-statable item (for which, error is reported upstream)
						//(note - this is a physical walk, no link-through problem here)
			retval = E2TW_FIXME;
			break;
		default:
			retval = E2TW_STOP;
			break;
	}

#ifdef E2_VFS
	if (user_data->operr != NULL && *(user_data->operr) == NULL)
		*(user_data->operr) = E2_ERR_NAME;
	else
		E2_ERR_CLEAR
#endif

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
static E2_TwResult _e2_task_twcb_delete (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, gpointer user_data)
{
	E2_TwResult retval = E2TW_CONTINUE;
	E2_ERR_DECLARE

	switch (status)
	{
		case E2TW_DP:	//dir completed
			if (e2_fs_remove (localpath E2_ERR_PTR()))
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
				if (e2_fs_remove (localpath E2_ERR_PTR()))
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
			e2_fs_remove (localpath E2_ERR_PTR());	//no need for error check
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
This is for deleting any existing item that is to be replaced
as part of a copy, move, rename, link task.
It is called at the start of each of those tasks.
When applied to a link, that is NOT traversed, so if it is a link
to a directory (which requires a recursive delete) that dir
will be untouched (unless/until processed in its own right, anyway).
For dirs, errors other than ENOTEMPTY return FALSE.
Not-empty dirs trigger a recursive delete, bottom-up,
again, no link traverses (dir's in the same tree will be processed
at some stage, in their own right)
No error processing other than ENOTEMPTY (OK?)
Assumes BGL is open
@param localpath localised string, relative path of item to delete
@return TRUE if succeeds
*/
//CHECKME always return TRUE so the process tries to continue?
static gboolean _e2_task_backend_delete (VPATH *localpath E2_ERR_ARG())
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
	{	//dir
/*
		//subject to permission, make sure we can traverse the dir
		//this fails if there is not sufficient write permission to delete the dir's contents
		if (!(statbuf.st_mode & S_IXUSR))
		{
			mode_t thismode = (statbuf.st_mode | S_IXUSR | S_IWUSR) & ALLPERMS;
			if (!_e2_task_backend_chmod1 (localpath, thismode E2_ERR_SAMEARG()))
				return FALSE;
		}
		E2_ERR_CLEAR
		//try for simple deletion first
		if (! e2_fs_rmdir (localpath E2_ERR_SAMEARG()))
		{
			return TRUE;
		}
		else if (!(E2_ERR_PIS (ENOTEMPTY) || E2_ERR_PIS (EEXIST)))
		{
			return FALSE;	//error if failed for some reason other than not-empty
		}
		E2_ERR_CLEAR
*/
		//dir not empty, recursively delete its contents
		//FIXME which errors to report and/or ignore
		return (e2_fs_tw (localpath, _e2_task_twcb_delete,
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
	else	//not dir
		//FIXME which errors to report and/or ignore
		return (!e2_fs_unlink (localpath E2_ERR_SAMEARG()));
}
/**
@brief replace @a replace with @a dest, with error message if not possible
@param dest localised path of item to be deleted
@param replace localised path of item to be replaced by @a dest
@return TRUE if the replacement was completed
*/
static gboolean _e2_task_backend_overwrite (VPATH *dest, VPATH *replace)
{
	E2_ERR_DECLARE
	if (!_e2_task_backend_delete (dest E2_ERR_PTR()))
	{
/*		gchar *msg = (e2_fs_is_dir3 (dest E2_ERR_NONE())) ?
		_("Cannot delete part or all of existing %s"):
		_("Cannot delete existing %s");
		e2_fs_error_local (msg, dest E2_ERR_MSGL());
*/
		e2_fs_error_local (_("Cannot delete existing %s"), dest E2_ERR_MSGL());
		E2_ERR_CLEAR
		return FALSE;
	}
	else
		e2_fs_rename (replace, dest E2_ERR_NONE());

	return TRUE;
}
/**
@brief copy the regular file|fifo|socket|device @a src to @a dest
If the item is a regular file, this copies the actual file
- links are traversed to find the file
Prints error message if @a src can't be read, or @a dest can't
be created, then terminates
@param src localised string, absolute path of item to copy
@param dest localised string, absolute path of copy destination
@param copyflags enumerator of copy mode
@param srcstat pointer to stat struct for @a src
@return TRUE if succeeds
*/
static gboolean _e2_task_backend_filecopy (VPATH *src, VPATH *dest,
	E2_FileTaskMode copyflags, const struct stat *srcstat)
{
	//work with a temp name only if the dest exists already
	gchar *temp = e2_task_tempname (VPSTR(dest));
#ifdef E2_VFS
	VPATH tdata = { temp, dest->spacedata };
#endif
//	printd (DEBUG, "dest path is %s", temp);
	if (S_ISREG (srcstat->st_mode))
	{
#ifdef E2_VFS
		if (!e2_fs_copy_file (src, srcstat, &tdata E2_ERR_NONE()))
#else
		if (!e2_fs_copy_file (src, srcstat, temp E2_ERR_NONE()))
#endif
		{
			if (temp != VPSTR(dest))
				g_free (temp);
			return FALSE;
		}
	}
	/*the following mimic the 'cp' app.
	  we do not clear the type-flags when creating these, as that would revert
	  the item to a regular file, but we do re-set the permissions explicitly,
	  to circumvent the mode mask */
	else if (S_ISBLK (srcstat->st_mode)
			|| S_ISCHR (srcstat->st_mode)
			|| S_ISSOCK (srcstat->st_mode))	//CHECKME mknod() doco says nothing about handling sockets this way
	{
		if (mknod (temp, srcstat->st_mode, srcstat->st_rdev))	//CHECKME no need for any vfs treatment
		{
			if (temp != VPSTR(dest))
				g_free (temp);
			E2_ERR_DECLARE
#ifdef E2_VFS
			e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Cannot create special file %s"), dest E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
	}
	else if (S_ISFIFO (srcstat->st_mode))
	{
		if (mkfifo (temp, srcstat->st_mode))	//CHECKME no need for any vfs treatment
		{
			if (temp != VPSTR(dest))
				g_free (temp);
			E2_ERR_DECLARE
#ifdef E2_VFS
			e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Cannot create FIFO %s"), dest E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
	}

#ifdef E2_VFS
	if (temp != VPSTR(dest) && !_e2_task_backend_overwrite (dest, &tdata))
#else
	if (temp != dest && !_e2_task_backend_overwrite ((gchar *)dest, temp))
#endif
	{
		g_free (temp);
		return FALSE;
	}
	if (temp != VPSTR(dest))
		g_free (temp);

#ifdef ACL_PERMS
	gboolean (*realfunc) (VPATH *, const struct stat *, VPATH *);
	realfunc = g_atomic_pointer_get (&copyaclfunc);	//atomic alternative to mutex
	if (realfunc == NULL	//no acl-copy plugin available
		|| ! realfunc (src, srcstat, dest)) //chacl failed, revert to chmod
	{
#endif
		mode_t mode = (S_ISREG (srcstat->st_mode)) ?
			srcstat->st_mode & ALLPERMS : srcstat->st_mode;
		if (!_e2_task_backend_chmod1 (dest, mode E2_ERR_NONE()))
			return FALSE;
#ifdef ACL_PERMS
	}
#endif
	if (copyflags & E2_FTM_SAMETIME) // && !e2_fs_lstat (src, srcstat E2_ERR_NONE()))
	{
		struct utimbuf tb;
		tb.modtime = srcstat->st_mtime;
		tb.actime = srcstat->st_atime;
//		e2_fs_utime (src, &tb E2_ERR_NONE());	//FIXME vfs
		e2_fs_utime (dest, &tb E2_ERR_NONE());
	}

	return TRUE;
}
/**
@brief 'copy' soft link itself (ie no traverse to link target)
This creates a new symlink to the original target (which may be relative).
It may be used during a direct or recursive copy.
Possible errors:
 *EEXIST
 There is already an existing item named newname
 (meaning, here, some unknown process created that item since we
deleted one of that same name !)
 *EROFS
 The new link item can't be created on a read-only file system.
 *ENOSPC
  The directory or file system cannot be extended to make the new link.
 *EIO
  A hardware error occurred while reading or writing data on the disk.

Any error messages expect BGL to be open
@param src localised string, absolute path of link to copy (a link "name")
@param dest localised string, absolute path of new link name (also a link "name")
@param copyflags enumerator of copy mode (E2_FTM_SAMETIME may be used)
# * @param srcstat pointer to stat struct for @a src, UNUSED unless E2_FTM_SAMETIME is in effect
@return TRUE if succeeds
*/
static gboolean _e2_task_backend_linkcopy1 (VPATH *src, VPATH *dest)
{
	gchar linkto[PATH_MAX];	//+NAME_MAX];
#ifdef E2_VFS
	VPATH sdata;
	VPATH tdata;
#endif
	E2_ERR_DECLARE

	//get target of link to copy
	gint len = e2_fs_readlink (src, linkto, sizeof (linkto) E2_ERR_PTR());
	if (len > 0)
	{
		if (e2_option_bool_get ("relative-symlinks"))
		{
			gchar *freeme;
			if (!(linkto[0] == G_DIR_SEPARATOR
				  || g_str_has_prefix (linkto, ".."G_DIR_SEPARATOR_S)
				  || g_str_has_prefix (linkto, "."G_DIR_SEPARATOR_S)))
			{	//the target is not a relative or absolute path
				freeme = g_path_get_dirname (VPSTR(src));
				if (strcmp (freeme, "."))
				{
					gchar *temp = g_build_filename (freeme, linkto, NULL);
					g_free (freeme);
#ifdef E2_VFS
					sdata.path = temp; sdata.spacedata = src->spacedata;
					freeme = e2_utils_create_relative_path (&sdata, dest);
#else
					freeme = e2_utils_create_relative_path (temp, dest);
#endif
					g_free (temp);
					g_strlcpy (linkto, freeme, sizeof(linkto));
				}
				g_free (freeme);
			}
			else
			{
#ifdef E2_VFS
				sdata.path = linkto; sdata.spacedata = src->spacedata;
				freeme = e2_utils_create_relative_path (&sdata, dest);
#else
				freeme = e2_utils_create_relative_path (linkto, dest);
#endif
				g_strlcpy (linkto, freeme, sizeof(linkto));
				g_free (freeme);
			}
		}

		gchar *temp = e2_task_tempname (VPSTR(dest));
#ifdef E2_VFS
		sdata.path = linkto; sdata.spacedata = src->spacedata;
		tdata.path = temp; tdata.spacedata = dest->spacedata;
		if (!e2_fs_symlink (&sdata, &tdata E2_ERR_NONE()))
#else
		if (!e2_fs_symlink (linkto, temp E2_ERR_NONE()))
#endif
		{
			gboolean retval = TRUE;
			if (temp != VPSTR(dest))
			{
#ifdef E2_VFS
				retval = _e2_task_backend_overwrite (dest, &tdata);
#else
				retval = _e2_task_backend_overwrite (dest, temp);
#endif
				g_free (temp);
			}
/*FIXME this does nothing to a symlink, it changes the link target
there is no lutime ..
			if (retval && (copyflags & E2_FTM_SAMETIME))
			{
				struct utimbuf tb;
				tb.modtime = srcstat->st_mtime;
				tb.actime = srcstat->st_atime;
				e2_fs_utime (dest, &tb E2_ERR_NONE());
			}
*/
			return retval;
		}
		if (temp != VPSTR(dest))
			g_free (temp);
		e2_fs_error_local (_("Cannot create new link to %s"),
#ifdef E2_VFS
			&sdata E2_ERR_MSGL());
#else
			linkto E2_ERR_MSGL());
#endif
		E2_ERR_CLEAR
		return FALSE;
	}
	else
	{
		e2_fs_error_local (_("Cannot get target info for link %s"), src E2_ERR_MSGL());
		E2_ERR_CLEAR
		return FALSE;
	}
}
/**
@brief make a directory @a dest during a recursive dir copy task
This creates the dir always with o+x,o+w permissions, then
if need be stores the path and 'real' mode, for later setting of that
real mode.
An error message is printed if the task fails for any reason.
Possible errors:
Name errors - see _e2_task_nftwfunc_delete documentation
 *EACCES
No write permission for the parent directory to which the new
directory is to be added.
 *EEXIST
A file named filename already exists (but in this context, it
should have been deleted already)
  *EMLINK
The parent directory has too many links (entries).
 *ENOSPC
The file system doesn't have enough room to create the
new directory.
 *EROFS
The parent directory of the directory being created is on
a read-only file system

@param dest localised string, absolute path of dir to create
@param mode stat mode flags of the dir being reproduced

@return TRUE if succeeds
*/
static gboolean _e2_task_backend_mkdir1 (VPATH *dest, mode_t mode)
{
	E2_ERR_DECLARE
	//need to set USR bits for mkdir to accept the change ?
	mode_t thismode = (mode | S_IXUSR | S_IWUSR) & ALLPERMS;
	if (e2_fs_mkdir (dest, thismode E2_ERR_PTR()))
	{
		e2_fs_error_local (_("Cannot create directory %s"), dest E2_ERR_MSGL());
		E2_ERR_CLEAR
		return FALSE;
	}
	return TRUE;
}
/**
@brief check whether is ok to overwrite @a dest
@param src virtual path with localised absolute path of item to copy/move
@param dest virtual path with localised absolute path existing item to be overwritten
@param mode pointer to store for flag to update
@param retval pointer to store for treewalk exit code to update
@return code corresponding to user's choice
*/
static DialogButtons _e2_task_backend_confirm (VPATH *src, VPATH *dest,
	E2_FileTaskMode *mode, E2_TwResult *retval)
{
	//note: no change to task status while waiting
	DialogButtons choice = e2_dialog_ow_check (src, dest, BOTHALL);
	switch (choice)
	{
		case YES_TO_ALL:
			*mode &= ~E2_FTM_CHECK;
		case OK:
		case CANCEL:
			*retval = E2TW_CONTINUE;
			break;
		default:
			*retval = E2TW_STOP;
			break;
	}
	return choice;
}
/**
@brief callback function for recursive directory copying
This is called for each item in the directory to be copied.
Treewalk is breadth-first, not physical
@param localpath absolute path of item to copy, localised string
@param statptr pointer to struct stat with info about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data pointer to user-specified data

@return completion code: E2TW_CONTINUE if succeeds, others as appropriate
*/
static E2_TwResult _e2_task_twcb_copy (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, E2_CopyData *user_data)
{
	E2_TwResult retval = E2TW_CONTINUE;
	mode_t mode = statptr->st_mode;
	E2_ERR_DECLARE
	gchar *dest = e2_utils_strcat (user_data->newroot,
		VPSTR(localpath) + user_data->oldroot_len);
#ifdef E2_VFS
	VPATH ddata = { dest, user_data->destspace };
#endif

	switch (status)
	{
		DialogButtons choice;
		E2_DirEnt *dirfix;
		GList *member;

		case E2TW_DP:	//dir completed
			//revert and set dir permissions, times if needed
			for (member = g_list_last (user_data->dirdata); member != NULL; member = member->prev)
			{
				dirfix = member->data;
				if (dirfix != NULL)
				{
					if (!strcmp (dirfix->path, localpath))
					{
#ifdef ACL_PERMS
						gboolean (*realfunc) (VPATH *, const struct stat *, VPATH  *);
						realfunc = g_atomic_pointer_get (&copyaclfunc);//atomic alternative to mutex
						if (realfunc == NULL	//no acl-copy plugin available
						||
# ifdef E2_VFS
							! realfunc (localpath, statptr, &ddata)) //chacl failed, try reverting to chmod
# else
							! realfunc (localpath, statptr, dest))
# endif
						{
#endif
							if ((statptr->st_mode & ALLPERMS) != dirfix->mode
								&& !_e2_task_backend_chmod1 (localpath,
									dirfix->mode E2_ERR_PTR()))
								retval = E2TW_FIXME;	//CHECKME - want cleanup of copied file to continue
#ifdef ACL_PERMS
						}
#endif
						if (user_data->taskmode & E2_FTM_SAMETIME)
						{
							struct utimbuf tb;
							tb.modtime = statptr->st_mtime;	//dirfix->modtime;
							tb.actime = statptr->st_atime;	//dirfix->axstime;
	//						if (e2_fs_utime ((gchar *)filename, &tb E2_ERR_NONE()))
	//							retval = E2TW_FIXME;
#ifdef E2_VFS
							if (e2_fs_utime (&ddata, &tb E2_ERR_NONE()))	//FIXME use E2_ERR_PTR if no error yet
#else
							if (e2_fs_utime (dest, &tb E2_ERR_NONE()))	//FIXME use E2_ERR_PTR if no error yet
#endif
								retval = E2TW_FIXME;
						}
#ifdef E2_VFS
						if (!_e2_task_backend_chmod1 (&ddata, dirfix->mode E2_ERR_PTR()))
#else
						if (!_e2_task_backend_chmod1 (dest, dirfix->mode E2_ERR_PTR()))
#endif
							retval = E2TW_FIXME;
						g_free (dirfix->path);
						DEALLOCATE (E2_DirEnt, dirfix);
						user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
						break;
					}
				}
//				else //should never happen CHECKME ok to delete while interating ?
//					user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
			}
			break;
		case E2TW_DRR:	//dir now readable
		case E2TW_D:  //dir
			if ((user_data->taskmode & E2_FTM_MERGE) //doing a merge-copy
#ifdef E2_VFS
				&& e2_fs_access2 (&ddata E2_ERR_PTR()) == 0)	//destination item exists already
#else
				&& e2_fs_access2 (dest E2_ERR_PTR()) == 0)	//destination item exists already
#endif
			{
#ifdef E2_VFS
				if (e2_fs_is_dir3 (&ddata E2_ERR_PTR())) //destination is a dir also
#else
				if (e2_fs_is_dir3 (dest E2_ERR_PTR())) //destination is a dir also
#endif
					//CHECKME equal-base-name check ? (for copy-as task)
					break;
				if (user_data->taskmode & E2_FTM_CHECK)
				{
					choice = _e2_task_backend_confirm (localpath,
#ifdef E2_VFS
						&ddata,
#else
						dest,
#endif
						&user_data->taskmode, &retval);
					if (choice == CANCEL || choice == NO_TO_ALL)
					{
						retval |= E2TW_SKIPSUB;
						break;
					}
				}
			}

			if (e2_fs_tw_adjust_dirmode (localpath, statptr, X_OK) != 0
#ifdef E2_VFS
				&& _e2_task_backend_mkdir1 (&ddata, 0777))
#else
				&& _e2_task_backend_mkdir1 (dest, 0777))
#endif
			{
				//store data about the dir, for later cleanup
				dirfix = ALLOCATE (E2_DirEnt);
				CHECKALLOCATEDWARNT (dirfix, retval = E2TW_STOP; break;)
				dirfix->path = g_strdup (VPSTR(localpath));
				dirfix->mode = mode & ALLPERMS;
				if (user_data->taskmode & E2_FTM_SAMETIME)
				{ //arrange to set times later, if the DP report doesn't happen
					dirfix->modtime = statptr->st_mtime;
					dirfix->axstime = statptr->st_atime;
				}
				user_data->dirdata = g_list_append (user_data->dirdata, dirfix);
			}
			else
				retval |= E2TW_SKIPSUB;
			break;
		case E2TW_DM:	//dir, not opened due to different file system
		case E2TW_DL:	//dir, not opened due to tree-depth limit (reported upstream)
		case E2TW_DNR:	//unreadable dir (for which, error is reported upstream)
			if ((user_data->taskmode & E2_FTM_MERGE) //doing a merge-copy
#ifdef E2_VFS
				&& e2_fs_access2 (&ddata E2_ERR_PTR()) == 0)	//destination item exists already
#else
				&& e2_fs_access2 (dest E2_ERR_PTR()) == 0)	//destination item exists already
#endif
			{
#ifdef E2_VFS
				if (e2_fs_is_dir3 (&ddata E2_ERR_PTR())) //destination is a dir also
#else
				if (e2_fs_is_dir3 (dest E2_ERR_PTR())) //destination is a dir also
#endif
					//CHECKME equal-base-name check ? (for copy-as task)
				{
					retval = E2TW_FIXME;
					break;
				}
				if (user_data->taskmode & E2_FTM_CHECK)
				{
					choice = _e2_task_backend_confirm (localpath,
#ifdef E2_VFS
						&ddata,
#else
						dest,
#endif
						&user_data->taskmode, &retval);
					if (choice == CANCEL || choice == NO_TO_ALL)
					{
						retval = E2TW_FIXME;
						break;
					}
				}
			}

#ifdef E2_VFS
			if (_e2_task_backend_mkdir1 (&ddata, 0777))
#else
			if (_e2_task_backend_mkdir1 (dest, 0777))
#endif
			{
				if (user_data->taskmode & E2_FTM_SAMETIME)
				{
					struct utimbuf tb;
					tb.modtime = statptr->st_mtime;
					tb.actime = statptr->st_atime;
#ifdef E2_VFS
					e2_fs_utime (&ddata, &tb E2_ERR_NONE());	//FIXME use E2_ERR_PTR if no error yet
#else
					e2_fs_utime (dest, &tb E2_ERR_NONE());	//FIXME use E2_ERR_PTR if no error yet
#endif
				}
				if ((mode & ALLPERMS) != 0777)
#ifdef E2_VFS
					_e2_task_backend_chmod1 (&ddata, mode & ALLPERMS E2_ERR_PTR());
#else
					_e2_task_backend_chmod1 (dest, mode & ALLPERMS E2_ERR_PTR());
#endif
			}
			retval = E2TW_FIXME;
			break;
		case E2TW_F:  //file
		case E2TW_SL:  //link
		case E2TW_SLN:  //bad link
			if ((user_data->taskmode & E2_FTM_MERGE) //doing a merge-copy
				&& (user_data->taskmode & E2_FTM_CHECK)	//confirm overwrite
#ifdef E2_VFS
				&& e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)	//destination item exists already
#else
				&& e2_fs_access2 (dest E2_ERR_NONE()) == 0)	//destination item exists already
#endif
			{
				choice = _e2_task_backend_confirm (localpath,
#ifdef E2_VFS
					&ddata,
#else
					dest,
#endif
					&user_data->taskmode, &retval);
				if (choice == CANCEL || choice == NO_TO_ALL)
					break;
			}
			if (status == E2TW_F)
			{
#ifdef E2_VFS
				if (!_e2_task_backend_filecopy (localpath, &ddata,
#else
				if (!_e2_task_backend_filecopy (localpath, dest,
#endif
						user_data->taskmode, statptr))
							retval = E2TW_FIXME;	//E2TW_STOP;
			}
			else	//a link
			{
#ifdef E2_VFS
				if (!_e2_task_backend_linkcopy1 (localpath, &ddata))
#else
				if (!_e2_task_backend_linkcopy1 ((gchar *)localpath, dest))
#endif
					retval =  E2TW_FIXME;	//FTW_STOP;
/* changing link times ALWAYS fails
				if (retval == E2TW_CONTINUE && user_data->taskmode & E2_FTM_SAMETIME)
				{
					struct utimbuf tb;
					tb.modtime = statptr->st_mtime;	//dirfix->modtime;
					tb.actime = statptr->st_atime;	//dirfix->axstime;
					if (e2_fs_utime (dest, &tb E2_ERR_NONE()))
						retval = E2TW_FIXME;
				}
*/
			}
			break;
//		case E2TW_NS:	//un-statable item (error message upstream
		default:
			retval = E2TW_FIXME;	//E2TW_STOP;
			break;
	}

	g_free (dest);

#ifdef DEBUG_MESSAGES
	if (retval != E2TW_CONTINUE)
		printd (DEBUG, "Error code for %s is %d", localpath,
# ifdef E2_VFS
		E2_ERR_NAME->code
# else
		errno
# endif
	);
#endif

#ifdef E2_VFS
	if (user_data->operr != NULL && *(user_data->operr) == NULL)
		*(user_data->operr) = E2_ERR_NAME;
	else
		E2_ERR_CLEAR
#endif

	if (retval & E2TW_SKIPSUB)
		user_data->continued_after_problem = TRUE;
	if (retval & E2TW_FIXME)
	{
		user_data->continued_after_problem = TRUE;
		retval &= ~E2TW_FIXME;	//continue after bad item
	}

	return retval;  //CHECKME report (some/all?) errors, else it keeps trying & hangs
}

  /******************/
 /***** public *****/
/******************/

/**
@brief copy item @a src to @a dest
Directories, links, others are handled distinctly
Assumes BGL is open
@param src localised string, absolute path of item to copy
@param dest localised string, absolute path of copy destination
@param mode flags determining specific features for the copying
@return TRUE if copy succeeds entirely
*/
gboolean e2_task_backend_copy (VPATH *src, VPATH *dest, E2_FileTaskMode mode)
{
	//decide whether src is a dir, link or other
	struct stat statbuf;
	E2_ERR_DECLARE

	if (e2_fs_lstat (src, &statbuf E2_ERR_PTR()))
	{
		e2_fs_error_local (_("Cannot get information about %s"), src E2_ERR_MSGL());
		E2_ERR_CLEAR
		return FALSE;
	}

	if (S_ISDIR (statbuf.st_mode))
	{	//recursive copy dir contents
		//park parameters where they can be accessed by helpers
		E2_CopyData data;
#ifdef E2_VFS
		data.operr = &E2_ERR_NAME;
		data.destspace = dest->spacedata;
#endif
		data.taskmode = mode;
		data.continued_after_problem = FALSE;
		data.oldroot_len = strlen (VPSTR(src));
		if (mode & E2_FTM_MERGE)
			data.newroot = VPSTR(dest);
		else
			//work with a temp name (top-level dir) only if the dest exists already
			data.newroot = e2_task_tempname (VPSTR(dest));
		data.dirdata = NULL;

		gboolean retval = e2_fs_tw (src, _e2_task_twcb_copy, &data, -1,
		//flags for: cb-errors, no thru-links, breadth-first
#ifdef E2_VFS
			E2TW_XERR |
#endif
			E2TW_PHYS E2_ERR_PTR());

		//cleanups mostly done in twcb, but there may have been a DNR fix,
		//or an error which aborted the walk
		if (data.dirdata != NULL)
		{	//revert old and new dir permissions etc, LIFO order
#ifdef E2_VFS
			VPATH fixdata;
			fixdata.spacedata = dest->spacedata;
#endif
			GList *member;
			for (member = g_list_last (data.dirdata); member != NULL; member = member->prev)
			{
				E2_DirEnt *dirfix = member->data;

				gchar *dstr = e2_utils_strcat (data.newroot,
					dirfix->path + data.oldroot_len);
#ifdef E2_VFS
				fixdata.path = dstr;
#endif
				if (mode & E2_FTM_SAMETIME)
				{
					struct utimbuf tb;
					tb.modtime = dirfix->modtime;
					tb.actime = dirfix->axstime;
/*					if (e2_fs_utime (dirfix->path, &tb E2_ERR_PTR()))
					{
						//FIXME vfs handle error
						E2_ERR_CLEAR
						data.continued_after_problem = TRUE;	//FIXME a more useful message from this
					}
*/
#ifdef E2_VFS
					if (e2_fs_utime (&fixdata, &tb E2_ERR_PTR()))
#else
					if (e2_fs_utime (dstr, &tb E2_ERR_PTR()))
#endif
					{
						//FIXME vfs handle error
						E2_ERR_CLEAR
						data.continued_after_problem = TRUE;	//FIXME a more useful message from this
					}
				}

#ifdef E2_VFS
				if (!_e2_task_backend_chmod1 (&fixdata, dirfix->mode E2_ERR_PTR()))
#else
				if (!_e2_task_backend_chmod1 (dirfix->path, dirfix->mode E2_ERR_PTR()))
#endif
				{
					E2_ERR_CLEAR
					data.continued_after_problem = TRUE;	//FIXME a more useful message from this
				}
				if (!_e2_task_backend_chmod1 (dest, dirfix->mode E2_ERR_PTR()))
				{
					E2_ERR_CLEAR
					data.continued_after_problem = TRUE;	//FIXME a more useful message from this
				}

				g_free (dstr);
				g_free (dirfix->path);
				DEALLOCATE (E2_DirEnt, dirfix);
			}
			g_list_free (data.dirdata);
		}

		if (data.continued_after_problem)
		{
			e2_fs_error_simple (_("Cannot copy all of %s"), src);
		}
		//after reverting modes (cuz the temp names are stored in the list
		if (retval)
		{
#ifdef E2_VFS
			VPATH tdata = { data.newroot, dest->spacedata };
#endif
			if (data.continued_after_problem)
			{
#ifdef E2_VFS
				if (!e2_fs_access2 (&tdata E2_ERR_NONE()))
#else
				if (!e2_fs_access2 (data.newroot E2_ERR_NONE()))
#endif
				{
					gchar *temp2, *temp3;
					temp2 = g_strconcat (VPSTR(dest), "-",_("incomplete"), NULL);
					temp3 = e2_task_tempname (temp2);
#ifdef E2_VFS
					VPATH t3data = { temp3, dest->spacedata };
					retval = e2_task_backend_rename (&tdata, &t3data);
#else
					retval = e2_task_backend_rename (data.newroot, temp3);
#endif
					if (temp3 != temp2)
						g_free (temp3);
					g_free (temp2);
				}
				retval = FALSE; //cuz we 'continued after problem'
			}
			else //!data.continued...
				if (data.newroot != VPSTR(dest))
			{
/*				if (mode & E2_FTM_BACKUP)
				{
					get backup name
					retval = e2_task_backend_move (dest, backup)
					rename
				}
				else if (mode & E2_FTM_TRASH)
				{
					pack art, get from
					retval = e2_task_trashit (gpointer from, E2_ActionRuntime *art)
					free art
					rename
				}
				else
*/
				if (!(mode & E2_FTM_MERGE))
#ifdef E2_VFS
					retval = _e2_task_backend_overwrite (dest, &tdata);
#else
					retval = _e2_task_backend_overwrite (dest, data.newroot);
#endif
			}
		}
		else //!retval
		{
			E2_ERR_CLEAR
		}
		if (data.newroot != VPSTR(dest))
			g_free (data.newroot);
		return retval;
	}
	else //not a dir
		if (S_ISLNK (statbuf.st_mode))
			//handle links separately, to avoid 'look-through'
			return (_e2_task_backend_linkcopy1 (src, dest));
	else	//not dir or link
		return (_e2_task_backend_filecopy (src, dest, mode, &statbuf));
}
/**
@brief move file @a src to @a dest
@param src localised string, absolute path of item to move
@param dest localised string, absolute path of move destination
@return TRUE if move succeeds
*/
gboolean e2_task_backend_move (VPATH *src, VPATH *dest)
{
	if (e2_fs_mount_is_mountpoint (src))
		return FALSE;
	//work with a temp name only if the dest exists already
	gchar *temp = e2_task_tempname (VPSTR(dest));
#ifdef E2_VFS
	VPATH tdata = { temp, dest->spacedata };
#endif

	E2_ERR_DECLARE
#ifdef E2_VFS
	if (e2_fs_rename (src, &tdata E2_ERR_PTR()))
#else
	if (e2_fs_rename (src, temp E2_ERR_PTR()))
#endif
	{
		if (temp != VPSTR(dest))
			g_free (temp);
		if (E2_ERR_IS (EXDEV))
		{	//have to copy to another device
			E2_ERR_CLEAR
			return ( e2_task_backend_copy (src, dest, E2_FTM_SAMETIME)
				&& e2_task_backend_delete (src) );
		}
		else
		{
			gchar *utf = F_DISPLAYNAME_FROM_LOCALE (VPSTR(src));
			gchar *utf2 = F_DISPLAYNAME_FROM_LOCALE (VPSTR(dest));
			gchar *msg = g_strdup_printf (_("Cannot move %s to %s"), utf, utf2);
			e2_fs_error (msg E2_ERR_MSGL());
			E2_ERR_CLEAR
			F_FREE (utf, VPSTR(src));
			F_FREE (utf2, VPSTR(dest));
			g_free (msg);
			return FALSE;
		}
	}
#ifdef E2_VFS
	else if (temp != VPSTR(dest) && !_e2_task_backend_overwrite (dest, &tdata))
#else
	else if (temp != dest && !_e2_task_backend_overwrite (dest, temp))
#endif
	{
		g_free (temp);
		return FALSE;
	}
	if (temp != VPSTR(dest))
		g_free (temp);
	return TRUE;
}
/**
@brief create soft link named @a name to @a target
@param target localised string, absolute path of item to link to
@param name localised string, absolute or relative path of link
@return TRUE if creation succeeds
*/
gboolean e2_task_backend_link (VPATH *target, VPATH *name)
{
	gchar *freeme;
	//work with a temp name only if the dest exists already
	gchar *temp = e2_task_tempname (VPSTR(name));
#ifdef E2_VFS
	VPATH tdata = { temp, name->spacedata };
	VPATH fdata;
#endif
	if (e2_option_bool_get ("relative-symlinks"))
	{
		freeme = e2_utils_create_relative_path (target, name);
#ifdef E2_VFS
		fdata.path = freeme;
		fdata.spacedata = name->spacedata;
#endif
	}
	else
		freeme = NULL;

	E2_ERR_DECLARE
#ifdef E2_VFS
	if (e2_fs_symlink ((freeme == NULL) ? target : &fdata, &tdata E2_ERR_PTR()))
#else
	if (e2_fs_symlink ((freeme == NULL) ? target : freeme, temp E2_ERR_PTR()))
#endif
	{
		e2_fs_error_local (_("Cannot create link to %s"), target E2_ERR_MSGL());
		if (temp != VPSTR(name))
			g_free (temp);
		if (freeme != NULL)
			g_free (freeme);
		E2_ERR_CLEAR
		return FALSE;
	}
#ifdef E2_VFS
	else if (temp != VPSTR(name) && !_e2_task_backend_overwrite (name, &tdata))
#else
	else if (temp != name && !_e2_task_backend_overwrite (name, temp))
#endif
	{
		g_free (temp);
		if (freeme != NULL)
			g_free (freeme);
		return FALSE;
	}
	if (temp != VPSTR(name))
		g_free (temp);
	if (freeme != NULL)
		g_free (freeme);
	return TRUE;
}
/**
@brief rename item named @a oldsrc to @a newsrc
Assumes BGL is off/open, for any error message
@param oldsrc localised string, absolute path of item to rename
@param newsrc localised string, absolute path of new name
@return TRUE if rename succeeds
*/
gboolean e2_task_backend_rename (VPATH *oldsrc, VPATH *newsrc)
{
	if (e2_fs_mount_is_mountpoint (oldsrc))
		return FALSE;
	E2_ERR_DECLARE
	//work with a temp name only if the dest exists already
	gchar *temp = e2_task_tempname (VPSTR(newsrc));
#ifdef E2_VFS
	VPATH tdata = { temp, newsrc->spacedata };
	if (e2_fs_rename (oldsrc, &tdata E2_ERR_PTR()))
#else
	if (e2_fs_rename (oldsrc, temp E2_ERR_PTR()))
#endif
	{
		gchar *utf = F_DISPLAYNAME_FROM_LOCALE (VPSTR(oldsrc));
		gchar *utf2 = F_DISPLAYNAME_FROM_LOCALE (VPSTR(newsrc));
		gchar *msg = g_strdup_printf (_("Cannot rename %s to %s"), utf, utf2);
		e2_fs_error (msg E2_ERR_MSGL());
		E2_ERR_CLEAR
		F_FREE (utf, VPSTR(oldsrc));
		F_FREE (utf2, VPSTR(newsrc));
		g_free (msg);
		if (temp != VPSTR(newsrc))
			g_free (temp);
		return FALSE;
	}
	else if (temp != VPSTR(newsrc) && !_e2_task_backend_delete (newsrc E2_ERR_PTR()))
	{
		e2_fs_error_local (_("Cannot delete existing %s"), newsrc E2_ERR_MSGL());
		g_free (temp);
		E2_ERR_CLEAR
		return FALSE;
	}
#ifdef E2_VFS
	if (e2_fs_rename (&tdata, newsrc E2_ERR_NONE()))
#else
	if (e2_fs_rename (temp, newsrc E2_ERR_NONE()))
#endif
	{
		if (temp != VPSTR(newsrc))
			g_free (temp);
		return FALSE;
	}
	if (temp != VPSTR(newsrc))
		g_free (temp);
	return TRUE;
}
/**
@brief delete item @a localpath
@param localpath data ibncluding localised string, absolute path of item to delete
@return TRUE if delete succeeds
*/
gboolean e2_task_backend_delete (VPATH *localpath)
{
	E2_ERR_DECLARE
	if (_e2_task_backend_delete (localpath E2_ERR_PTR()))
		return TRUE;

/*	gchar *msg = (e2_fs_is_dir3 (path E2_ERR_NONE())) ?
		_("Cannot delete part or all of %s"):
		_("Cannot delete %s");
	e2_fs_error_local (msg, path E2_ERR_MSGL());
*/
	e2_fs_error_local (_("Cannot delete %s"), localpath E2_ERR_MSGL());
	E2_ERR_CLEAR
	return FALSE;
}
/**
@brief change permissions of item @a path to @a mode

Only an item's owner (as judged by the effective uid of the process) or a
privileged user, can change item permissions. Symlinks are never changed, nor
(unless explicitly included in the process) are link targets.
If invoked on a non-dir, or a dir without recursion, it is processed here.
If recursion is invoked on a dir, a treewalk function is invoked. By that, all
nested dir access permissons will be set to include x, non-dir items will be
have their new permissions set when they are 'reported'. Finally, here, dirs
will be set to their proper permissions (bottom-up order) at the end of the process.
Recursive chmod works on the host filesystem only.
Error messages expect BGL open
@param localpath data including localised string, absolute path of item to process
@param optype enumerator for the type of operation - set add or remove
@param opmode permissions flags to be set, added or removed
@param recurse code for recurse type (ignored if @a path is not a dir)
@return TRUE if operation succeeds
*/
gboolean e2_task_backend_chmod (VPATH *localpath, E2_ChmodType optype,
	mode_t opmode, E2_RecurseType recurse)
{
	E2_ChmodData data;
	struct stat statbuf;
	E2_ERR_DECLARE

	if (recurse != E2_RECURSE_NONE)
	{
		//decide whether src is a dir or not
		if (e2_fs_stat (localpath, &statbuf E2_ERR_PTR()))  //looks _through_ links
		{
			//abort if we can't find the item
			e2_fs_error_local (_("Cannot get current data for %s"), localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
		if (S_ISDIR (statbuf.st_mode))
		{	//recursive chmod
			//park parameters where they can be accessed by helpers
#ifdef E2_VFS
			data.operr = &E2_ERR_NAME;
#endif
			data.continued_after_problem = FALSE;
			data.scope = recurse;
			_e2_task_backend_parse_mode (optype, opmode, &data);
			//no processed dirs yet
			data.dirdata = NULL;

			gboolean retval = e2_fs_tw (localpath, _e2_task_twcb_chmod, &data, -1,
				//flags for: cb-errors, no thru-links, this filesystem only, breadth-first
#ifdef E2_VFS
				E2TW_XERR |
#endif
				E2TW_MOUNT | E2TW_PHYS E2_ERR_PTR());

			//cleanups mostly done in twcb, but there may have been a DNR fix,
			//or an error which aborted the walk
			if (data.dirdata != NULL)
			{	//change/revert dir permissions, LIFO (=bottom-up) order
#ifdef E2_VFS
				VPATH fixdata;
				fixdata.spacedata = localpath->spacedata;
#endif
				GList *member;
				for (member = g_list_last (data.dirdata); member != NULL; member = member->prev)
				{
					E2_ERR_CLEAR	//should really not do this, but use E2_ERR_NONE arg after any error happens
					E2_DirEnt *dirfix = member->data;
#ifdef E2_VFS
					fixdata.path = dirfix->path;
					if (!_e2_task_backend_chmod1 (&fixdata, dirfix->mode E2_ERR_PTR()))
#else
					if (!_e2_task_backend_chmod1 (dirfix->path, dirfix->mode E2_ERR_PTR()))
#endif
						data.continued_after_problem = TRUE;
					g_free (dirfix->path);
					DEALLOCATE (E2_DirEnt, dirfix);
				}
				g_list_free (data.dirdata);
			}

			if (data.continued_after_problem)
			{
				e2_fs_error_simple (_("Cannot change permission(s) of all of %s"), localpath);
				retval = FALSE;
			}
#ifdef E2_VFS
			if (!retval)
			{
				E2_ERR_CLEAR
			}
#endif
			return retval;
		}
		else //not dir, handle without recurse
			recurse = E2_RECURSE_NONE;
	}
	if (recurse == E2_RECURSE_NONE)
	{
		//get current mode
		if (e2_fs_lstat (localpath, &statbuf E2_ERR_PTR()))  //no link pass-thru
		{
			e2_fs_error_local (_("Cannot get current permissions of %s"), localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
		if (S_ISLNK (statbuf.st_mode))
		{
			return FALSE;	//link permissions can't be changed
		}
		//decode the mode masks
		_e2_task_backend_parse_mode (optype, opmode, &data);
		//get new mode
		mode_t new_mode = ((statbuf.st_mode & data.clearmask) | data.setmask) & ALLPERMS;
		if (_e2_task_backend_chmod1 (localpath, new_mode E2_ERR_PTR()))
			return TRUE;
		E2_ERR_CLEAR
	}
	return FALSE; //warning prevention only
}
/**
@brief change ownership of item @a localpath to @a owner_id and @a group_id

If invoked on a non-dir, or a dir without recursion, it is processed
here. If recursion is invoked on a dir, a ntfw funtion is invoked.
By that, all nested dir access permissons will be set (breadth-first walk)
to include x, non-dir items will be have their new permissions set when
they are 'discovered'. Finally, here, dirs will be reverted to their proper
permissions (bottom-up order) at the end of the process
Recursive chown works on the host filesystem only.
Links are not affected, their target is processed. (CHECKME ok??)
Only an item's owner (as judged by the effective uid of the process)
or a privileged user, can change item permissions.
Assumes BGL is open
@param localpath data including localised string, absolute path of item to process
@param owner_id uid to be applied
@param group_id gid to be applied
@param recurse_dirs TRUE to recurse (ignored if @a path is not a dir)
@return TRUE if the operation succeeds
*/
gboolean e2_task_backend_chown (VPATH *localpath, uid_t owner_id,
	gid_t group_id, gboolean recurse_dirs)
{
	struct stat statbuf;
	E2_ERR_DECLARE
//	if (e2_fs_stat (path, &statbuf E2_ERR_PTR()))  //looking _through_ links
	if (e2_fs_lstat (localpath, &statbuf E2_ERR_PTR()))  //_not_ looking through links
	{
		e2_fs_error_local (_("Cannot get current data for %s"), localpath E2_ERR_MSGL());
		E2_ERR_CLEAR
		return FALSE;
	}
	if (recurse_dirs)
	{
		//decide whether or not item is dir
		if (S_ISDIR (statbuf.st_mode))
		{	//dir
			//park the data where they can be accessed by the helper
			E2_ChownData data;
#ifdef E2_VFS
			data.operr = &E2_ERR_NAME;
#endif
			data.continued_after_problem = FALSE;
			data.new_uid = owner_id;
			data.new_gid =  group_id;
			data.dirdata = NULL;

			gboolean retval = e2_fs_tw (localpath, _e2_task_twcb_chown, &data, -1,
			//flags for: cb-errors, fix-DNR, breadth-first, this filesystem only, no link pass-thru
#ifdef E2_VFS
				E2TW_XERR |
#endif
				E2TW_FIXDIR | E2TW_MOUNT | E2TW_PHYS E2_ERR_PTR());

			//cleanups mostly done in twcb, but there may have been a DNR fix,
			//or an error which aborted the walk
			if (data.dirdata != NULL)
			{	//chmod and revert all dirs' permissions, LIFO order
#ifdef E2_VFS
				VPATH fixdata;
				fixdata.spacedata = localpath->spacedata;
#endif
				GList *member;
				for (member=g_list_last (data.dirdata); member!=NULL; member=member->prev)
				{
					E2_DirEnt *dirfix = member->data;
#ifdef E2_VFS
					fixdata.path = dirfix->path;
					if (!_e2_task_backend_chown1 (&fixdata, &statbuf, owner_id, group_id E2_ERR_PTR()))
#else
					if (!_e2_task_backend_chown1 (dirfix->path, &statbuf, owner_id, group_id E2_ERR_PTR()))
#endif
					{
						data.continued_after_problem = TRUE;
						//prevent duplication of error
#ifdef E2_VFS
						_e2_task_backend_chmod1 (&fixdata, dirfix->mode E2_ERR_NONE());
#else
						_e2_task_backend_chmod1 (dirfix->path, dirfix->mode E2_ERR_NONE());
#endif
					}
#ifdef E2_VFS
					else if (!_e2_task_backend_chmod1 (&fixdata, dirfix->mode E2_ERR_PTR()))
#else
					else if (!_e2_task_backend_chmod1 (dirfix->path, dirfix->mode E2_ERR_PTR()))
#endif
					{
						E2_ERR_CLEAR
						data.continued_after_problem = TRUE;	//FIXME a more helpful message from this
					}
					g_free (dirfix->path);
					DEALLOCATE (E2_DirEnt, dirfix);
				}
				g_list_free (data.dirdata);
			}

			if (data.continued_after_problem)
			{
				e2_fs_error_simple (_("Cannot change ownership of all of %s"), localpath);
				retval = FALSE;
			}
#ifdef E2_VFS
			if (!retval)
			{
				E2_ERR_CLEAR
			}
#endif
			return retval;
		}
		else	//not dir, ignore the recurse flag
			recurse_dirs = FALSE;
	}
	if (!recurse_dirs)
	{
		return (_e2_task_backend_chown1 (localpath, &statbuf, owner_id, group_id E2_ERR_NONE()));
	}
	return FALSE;  //this can never happen, but it prevents warning
}

/**
@brief perform the default action for a specfied item

If no default action exists, the user is invited to create one
No thread-protection is done here or downstream, the caller must handle that
if needed.

@param localpath data including localised string, absolute or relative path of the item to be 'opened'
@param ask TRUE to invite user to choose some actions

@return TRUE if the operation succeeded
*/
gboolean e2_task_backend_open (VPATH *localpath, gboolean ask)
{
//	printd (DEBUG, "e2_task_backend_open ()");
	gboolean addpath, retval, exec;
	gint res;
	gchar *command, *ext, *freeme, *utf, *usepath, *base = NULL;
	//sometimes eg if filelist refresh is still underway, the CWD is not set
	//to active pane dir, so always prepend that if no other dir is nominated
	//and if not just activating updir-entry
#ifdef E2_VFS
	VPATH tdata;
#endif

	usepath = VPSTR (localpath);
	if (g_path_is_absolute (usepath) || !strcmp (usepath, ".."))
		addpath = FALSE;
	else
	{
		usepath = e2_utils_dircat (curr_view, usepath, TRUE);
		addpath = TRUE;
	}

	utf = F_FILENAME_FROM_LOCALE (usepath);	//not DISPLAY

/*	localpath = g_strdup (localpath);
don't need real path yet
#ifdef E2_VFS
	//checking a virtual dir in the process of being mounted may cause bad delay
	//so we fake a rough guess at what is intended ...
	//FIXME how ?? (also needed in _e2_pane_change_dir())
	printd (DEBUG, "NEED fake check whether %s is a link", path);
	E2_ERR_DECLARE
#endif
	if (e2_fs_walk_link (&path E2_ERR_NONE()))	//if item is a link, get target path
	{
		if (!g_path_is_absolute (path))
		{
			//FIXME a returned relative path is relative to some part of old path ?
//E2_VFSTMPOK
			freeme = F_FILENAME_TO_LOCALE (curr_view->dir);
			base = path;
			path = e2_utils_strcat (freeme, path);
			F_FREE (freeme);
			g_free (base);
		}
	}
*/

#ifdef E2_VFS
	tdata.path = usepath;
	tdata.spacedata = localpath->spacedata;
	//checking a virtual dir in the process of being mounted may cause bad delay
	//so we fake a rough guess at what is intended ...
	//FIXME how ?? (also needed in _e2_pane_change_dir())
	printd (DEBUG, "NEED fake check whether %s is a directory", usepath);
	if (
	//	1 ||
		e2_fs_is_dir3 (&tdata E2_ERR_NONE()))
#else
	if (e2_fs_is_dir3 (usepath))
#endif
	{
		exec = FALSE;	//no special treatment for dirs
		freeme = ext = g_strconcat (".", _("<directory>"), NULL);
	}
	else
	{
//	printd (DEBUG, "check '%s' for executable", path);
		exec = e2_fs_is_exec2 (
#ifdef E2_VFS
			&tdata E2_ERR_NONE());
#else
			usepath E2_ERR_NONE());
#endif
		freeme = NULL; //not a constructed extension, no free needed
		base = g_path_get_basename (utf);
/*		//FIXME generalise detection of hidden items
		if (!e2_fs_is_hidden (&tdata, E2_ERR_NONE()))
			ext = base; //look for extension from 1st byte
//USELESS else if (*base != '.')
//			ext = base;
//UNREAL else if (*base == '\0')
//			ext = base;
		else // *base == '.', a *NIX-kind of hidden file
			ext = base + sizeof (gchar); //look for extension from 2nd byte
*/
		ext = (*base == '.') ? base + sizeof (gchar) : base;
		ext = strchr (ext, '.');
	}

	if (ext == NULL //no extension (".<none>" can apply to one type only, probably executables)
		|| *(ext + sizeof (gchar)) == '\0')
	{
		if (exec)	//executable non-dir
		{
/*			if (!strcmp (dir, ".")
				&& ! g_str_has_prefix (path, E2_COMMAND_PREFIX))
					command = g_strdup_printf ("%s%s", E2_COMMAND_PREFIX, utf);
			else
				command = path;
			res = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, ?
#ifdef E2_COMMANDQ
			, TRUE
#endif
			);
			if (command != path)
				g_free (command);
*/
			//FIXME better aesthetic, shorten the command string where appropriate
			//CHECKME quote the path ?
			res = e2_command_run (utf, E2_COMMAND_RANGE_DEFAULT, app.main_window
#ifdef E2_COMMANDQ
			, TRUE
#endif
			);
			retval = (res == 0);
		}
		else if (e2_fs_is_text (
#ifdef E2_VFS
			&tdata E2_ERR_NONE()))
#else
			usepath))
#endif
		{
			retval = e2_task_backend_view (
#ifdef E2_VFS
			&tdata);
#else
			usepath);
#endif
		}
		else //non-exec && non-text
		{
			if (ask)
			{
				res = e2_filetype_dialog_create (
#ifdef E2_VFS
				 &tdata, FALSE, FALSE, FALSE); //enter command or view
#else
				 usepath, FALSE, FALSE, FALSE); //enter command or view
#endif
				retval = (res == E2_TYPE_HANDLED);
			}
			else
				retval = FALSE;
		}
	}
	else // the file has an extension of some sort
	{
		gchar *action, *real_action;
		do
		{
			ext += sizeof (gchar);	//skip the . prefix
			action = e2_filetype_get_default_action (ext);
			if (action != NULL)
			{
				real_action = strchr (action, '@');
				if (real_action != NULL)
					real_action++;
				else
					real_action = action;
				//user may have prepended "./" to command, but we don't need
				//that cuz we use absolute path
				if (exec && g_str_has_prefix (real_action, E2_COMMAND_PREFIX))
					real_action += 2;	//sizeof (E2_COMMAND_PREFIX);	//skip leading ascii chars
				//interpret <system default> if relevant
				gchar *sys = e2_task_system_command (real_action, usepath);
				if (sys != NULL)
				{
					g_free (action);
					action = sys;
					real_action = action;
				}

				if (exec)	//exec as well as extension
				{
					if (ask
						//hack to exclude valid exec extension like .sh
						&& (!(g_str_has_prefix (real_action, "%f")
						   || g_str_has_prefix (real_action, "%%f")
						   || g_str_has_prefix (real_action, "%p")
						   || g_str_has_prefix (real_action, "%%p"))))
					{
#ifdef E2_VFS
						res = e2_filetype_dialog_create (&tdata, FALSE, TRUE, FALSE);
#else
						res = e2_filetype_dialog_create (usepath, FALSE, TRUE, FALSE);
#endif
						if (res == E2_TYPE_CANCELLED || res == E2_TYPE_HANDLED)
						{
							retval = (res == E2_TYPE_HANDLED);
							g_free (action);
							break;
						}
					}
				}
				//not exec, or not ask, or exec+ask but user chose default action
				command = e2_utils_replace_name_macros (real_action, utf);
				if (command == real_action)	//no replaced macro in real_action
				{
//tag E2_BADQUOTES
					gchar *qp = e2_utils_quote_string (utf);
					command = g_strconcat (real_action, " ", qp, NULL);
					g_free (qp);
				}
				res = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, app.main_window
#ifdef E2_COMMANDQ
				, TRUE
#endif
				);
				g_free (command);
				retval = (res == 0);
				g_free (action);
				break;
			}
		} while ((ext = strchr (ext, '.')) != NULL);

		if (ext == NULL) //extension not recognised
		{
			if (exec)	//just execute it
			{
/*				dir = g_path_get_dirname (path);
				if (!strcmp (dir, ".")
					&& ! g_str_has_prefix (path, E2_COMMAND_PREFIX))
						command = g_strdup_printf ("%s%s", E2_COMMAND_PREFIX, utf);
				else
					command = path;
				res = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, ?
#ifdef E2_COMMANDQ
				, TRUE
#endif
				);
				g_free (dir);
				if (command != path)
					g_free (command);
*/
				res = e2_command_run (utf, E2_COMMAND_RANGE_DEFAULT, app.main_window
#ifdef E2_COMMANDQ
				, TRUE
#endif
				);
				retval = (res == 0);
			}
			else if (ask)
			{
#ifdef E2_VFS
				res = e2_filetype_dialog_create (&tdata, TRUE, FALSE, TRUE); //enter filetype, command or view
#else
				res = e2_filetype_dialog_create (usepath, TRUE, FALSE, TRUE); //enter filetype, command or view
#endif
				retval = (res == E2_TYPE_HANDLED);
			}
			else
				retval = FALSE;
		}
	}

	if (addpath)
		g_free (usepath);
	F_FREE (utf, usepath);
	if (freeme != NULL)
		g_free (freeme);
	if (base != NULL)
		g_free (base);

	return retval;
}
/**
@brief open text file @a localpath using internal or external viewer
This does no mutex management, so that must be handled by the caller.
@param localpath localised string, path of file to be viewed
@return TRUE if the viewer task was completed successfully
*/
gboolean e2_task_backend_view (VPATH *localpath)
{
	if (e2_option_bool_get ("use-external-viewer"))
	{
		gchar *viewer = e2_option_str_get ("command-viewer");
		if (*viewer != '\0')
		{
			gchar *utf = F_FILENAME_FROM_LOCALE (VPSTR (localpath));
			gchar *command = e2_utils_replace_name_macros (viewer, utf);
			if (command == viewer)	//no replacement of macro in viewer
			{
//tag E2_BADQUOTES
				//CHECKME may be bad if a file.view action is supplied
				gchar *qp = e2_utils_quote_string (utf);
				command = g_strconcat (viewer, " ", qp, NULL);
				g_free (qp);
			}
			gint res = e2_command_run (command, E2_COMMAND_RANGE_DEFAULT, curr_view->treeview
#ifdef E2_COMMANDQ
			, TRUE
#endif
			);
			F_FREE (utf, VPSTR (localpath));
			g_free (command);
			return (res == 0);
		}
		else
			return FALSE;
	}
	else
		return (e2_view_dialog_create (localpath));
}
