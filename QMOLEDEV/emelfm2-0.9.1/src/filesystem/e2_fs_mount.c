/* $Id: e2_fs_mount.c 2693 2013-08-25 04:38:17Z tpgww $

Copyright (C) 2007-2013 tooar <tooar@emelfm2.net>
Copyright (C) 2004-2007 Florian Zaehringer <flo.zaehringer@web.de>
Copyright (C) 1999-2004 Bill Wilson <bill@gkrellm.net>

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
@file src/filesystem/e2_fs_mount.c
@brief filesystem mountpoint related functions

Functions dealing with fstab / mtab have in part been sourced from gkrellm
(http://gkrellm.net) written by Bill Wilson.
*/

#include "emelfm2.h"

#ifdef E2_FS_MOUNTABLE

#include <string.h>
#include <unistd.h>

//in general, we support [u]mount completion
#define E2_FS_MOUNTABLE
// OSX 10.4 ("Tiger") automounts all devices. Unlike its predecessors, it uses statvfs()
#if defined(darwin)
#include <sys/types.h>
#include <sys/param.h>
#ifdef HAVE_SYS_STATVFS_H
#undef E2_FS_MOUNTABLE
#endif
#endif
//other exclusions go here ...

# if defined(__linux__)
#  include <sys/param.h>
#  include <sys/mount.h>
#  include <fstab.h>

# elif defined(__E2BSD__)	//maybe this should just be defined(__FreeBSD__) || defined(__OpenBSD__)
#  include <sys/param.h>
#  include <sys/mount.h>
#  include <fstab.h>

# elif defined(__solaris__) || defined(sco)
//CHECKME sco ?? svr5 in general ??
#  include <sys/mnttab.h>
#  include <vfstab.h>

# else
#  include <sys/param.h>
#  include <sys/mount.h>
#  include <fstab.h>

/*Some systems use statfs() to provide information about mounted
file systems, other systems use statvfs(). The header files used with
these functions vary between systems.
e.g.
HPUX 	  statfs() with sys/vfs.h
SunOS   statfs() with sys/vfs.h
Solaris  statvfs() with sys/statvfs.h */

#  include <sys/types.h>
#  include <sys/param.h>
#  ifdef HAVE_SYS_STATVFS_H
#   include <sys/statvfs.h>
#  endif
#  ifdef HAVE_SYS_VFS_H
#   include <sys/vfs.h>
#  endif

/*#ifdef HAVE_SYS_STATVFS_H
#define STATFS statvfs
#else
#define STATFS statfs
#endif

...
     struct STATFS sfs;
...
     rc = STATFS(filename, &sfs);
*/
# endif	//which os-specifis includes

//finesse mounted device name before inclusion in the list of unmountable devices
# define E2_PROCESS_MOUNTED_DEVICE
//strip any trailing / from the mountpoint name
//			s = strrchr (dir, G_DIR_SEPARATOR);
//			if (s != NULL && s != dir && *(s+1) == '\0')
//				*s = '\0';

//finesse unmounted device name before inclusion in the list of mountable devices
# define E2_PROCESS_UNMOUNTED_DEVICE
			//strip any trailing / from the mountpoint name
//			s = strrchr (dir, G_DIR_SEPARATOR);
//			if (s != NULL && s != dir && *(s+1) == '\0')
//				*s = '\0';

/**
@brief test whether a device should be included in the list of unmountable ones
@param dev
@param type
@param dir

@return TRUE if so
*/
static gboolean _e2_fs_mount_is_unmountable (const gchar *dev, const gchar *type, const gchar *dir)
{
#if defined(__linux__) || defined(__E2BSD__)
	const gchar *devices[] = { "none" };
	const gchar *types[] = { "proc", "dev", "sysfs", "tmpfs", "nfsd", "fuse", "fusectl", "fuse.bindfs"};
	const gchar *dirs[] = { G_DIR_SEPARATOR_S, "/sys", "/proc" };
/*	these should be covered by device "none"
	type "devpts"
	type "proc"
	type "usbdevfs"
	type "usbfs"
	type "sysfs"
	dir "/dev/"
*/
#else
	//CHECKME
	const gchar *devices = { "none" };
	const gchar *types = { "proc", "sysfs", "tmpfs", "nfsd" };
	const gchar *dirs = { G_DIR_SEPARATOR_S };
#endif
	gint i, j;
	j = sizeof (devices) / sizeof (gchar*);
	for (i = 0; i < j; i++)
	{
		if (strncmp (dev, devices[i], strlen(devices[i])) == 0)
		{
			printd (DEBUG, "device %s not unmountable", dev);
			return FALSE;
		}
	}
	j = sizeof (types) / sizeof (gchar*);
	for (i = 0; i < j; i++)
	{
		if (strncmp (type, types[i], strlen(types[i])) == 0)
		{
			printd (DEBUG, "type %s not unmountable", type);
			return FALSE;
		}
	}
	j = sizeof (dirs) / sizeof (gchar*);
	for (i = 0; i < j; i++)
	{
		if (i == 0)
		{
			if (strcmp (dir, dirs[0]) == 0)
			{
				printd (DEBUG, "dir %s not unmountable", dir);
				return FALSE;
			}
		}
		else if (strncmp (dir, dirs[i], strlen(dirs[i])) == 0)
		{
			printd (DEBUG, "dir %s not unmountable", dir);
			return FALSE;
		}
	}
/* there may be things blocking an unmount eg monitoring with dnotify */
/* permission-checking disabled pending doing it properly, if not forever
			gchar *opt = mounted->mnt_opts;
			if (!permcheck || _e2_complete_mount_permission (dir, opt)) */
/*for bsd's, which mount options are relevant ? how do we check them ?
		mounted[i].f_flags	//mount flags
		mounted[i].f_owner	//owner
*/
	printd (DEBUG, "dev %s, type %s, dir %s IS UNmountable", dev, type, dir);
	return TRUE;
}

/**
@brief test whether a device should be included in the list of mountable ones
@param dev
@param type
@param dir

@return TRUE if so
*/
static gboolean _e2_fs_mount_is_mountable (const gchar *dev, const gchar *type, const gchar *dir)
{
//CHECKME does this need to be os-specific ? if so, how ?
	const gchar *devices[] = { "none" };
	const gchar *types[] = { "swap", "proc", "sysfs", "tmpfs", "nfsd", "ignore", "", "fuse", "fusectl", "fuse.bindfs" };
	const gchar *dirs[] = { G_DIR_SEPARATOR_S, "/boot" };
	gint i, j;
	j = sizeof (devices) / sizeof (gchar*);
	for (i = 0; i < j; i++)
	{
		if (strncmp (dev, devices[i], strlen(devices[i])) == 0)
		{
			printd (DEBUG, "device %s not mountable", dev);
			return FALSE;
		}
	}
	j = sizeof (types) / sizeof (gchar*);
	for (i = 0; i < j; i++)
	{
		if (*types[i] != '\0')
		{
			if (strncmp (type, types[i], strlen(types[i])) == 0)
			{
				printd (DEBUG, "type %s not mountable", type);
				return FALSE;
			}
		}
		else if (*dev == '\0') return FALSE;
	}
	j = sizeof (dirs) / sizeof (gchar*);
	for (i = 0; i < j; i++)
	{
		if (i == 0)
		{
			if (strcmp (dir, dirs[0]) == 0)
			{
				printd (DEBUG, "dir %s not unmountable", dir);
				return FALSE;
			}
		}
		else if (strncmp (dir, dirs[i], strlen(dirs[i])) == 0)
		{
			printd (DEBUG, "dir %s not mountable", dir);
			return FALSE;
		}
	}
/*
	g_file_test (dir, G_FILE_TEST_IS_DIR) don't bother checking for this (slow)
    these should be handled by dev "none"
	type "devpts"
	type "proc"
	type "sysfs"
	type "usbdevfs"
	type "usbfs"
	dir "/dev/"
CHECKME sometimes, test may need to include
	strncmp (dev, "/dev", 4)
	realpath (dev, realdev) != NULL
	gchar realdev[PATH_MAX];
*/
/* permission-checking disabled pending doing it properly, if not forever
	gchar *opt = fs.vfs_mntopts;
	if (!permcheck || _e2_complete_mount_permission (dir, opt))
*/
	printd (DEBUG, "dev %s, type %s, dir %s IS mountable", dev, type, dir);
	return TRUE;
}

#ifdef E2_HAL
/**
@brief find mountpoint dir whose device is @a device
@param device localised string with device (i.e. not volume with trailing number) of mountpoint to be found

@return newly allocated string with mountpoint, or NULL upon error or no match
*/
gchar *e2_fs_mount_get_mountpoint_for_device (const gchar *device)
{
	//FIXME this is useless if cached data (in fstab etc) omits device path
	//FIXME convert this func to check & get any related string(s):
	//	device, mountpoint, mount options
	gchar *mountpath = NULL;
	//CHECKME lock data file while accessing it ?? re-entrant forms of functions ?
#if defined(__E2BSD__) || defined(__linux__)
	//FIXME better to use the _last-mounted_ device when >1 share the mountpoint
    struct fstab *fs;

	if (!setfsent ())
		return NULL;

	while ((fs = getfsent ()) != NULL)
	{
		if (strcmp (fs->fs_spec, device) == 0)
		{
			mountpath = g_strdup (fs->fs_file);
			break;
		}
    }
    endfsent ();
#elif defined(__solaris__) || defined(sco)
	//CHECKME sco
    struct vfstab fs;
	E2_FILE *f;

	if ((f = e2_fs_open_stream ("/etc/vfstab", "r")) == NULL)
		return NULL;

	while (getvfsent (f, &fs) == 0)
	{
		if (strcmp (fs.vfs_special, device) == 0)
		{
			mountpath = g_strdup (fs.vfs_mountp);
			break;
		}
	}
	e2_fs_close_stream (f);
#elif defined(hpux)
    struct mntent *mountable;
	E2_FILE *f;

    //handle PFS if necessary
    const gchar *fstab = (access("/etc/pfs_fstab", F_OK)) ?
		"/etc/fstab" : "/etc/pfs_fstab";

	if ((f = setmntent (fstab, "r")) == NULL)
		return NULL;

	while ((mountable = getmntent (f)) != NULL)
	{
		if (strcmp (mountable->mnt_fsname, device) == 0)
		{
			mountpath = g_strdup (mounted->mnt_dir);
			break;
		}
	}
	endmntent (f);
#elif defined(__svr4__)
	struct mnttab mountable;
	E2_FILE *f;

	if ((f = e2_fs_open_stream ("/etc/fstab", "r")) == NULL)
		return NULL;

	while (getmntent (f, &mountable) == 0)
	{
		if (strcmp (mountable.mnt_special, device) == 0)
		{
			mountpath = g_strdup (mountable.mnt_mountp);
			break;
		}
	}
	e2_fs_close_stream (f);
#else
	struct mntent *mountable;
	E2_FILE *f;

	if ((f = setmntent ("/etc/fstab", "r" )) == NULL)
		return NULL;

	while ((mountable = getmntent (f)) != NULL)	//data is not thread- or process-safe
	{
		if (strcmp (mountable->mnt_fsname, device) == 0)
		{
			mountpath = g_strdup (mountable->mnt_dir);
			break;
		}
	}
	endmntent (f);
#endif	//which OS
	return mountpath;
}
#endif //def E2_HAL

#if defined (E2_HAL) || defined (E2_DEVKIT)
/**
@brief get device corresponding to mounted directory @a utfpath
@param absolute path, UTF-8, may have trailing separator
This gets the device for the mounted point with the longest match between
the point's path and @a utfpath. Only works properly when @a utfpath is mounted.
Simple string comparisons are performed. @a utfpath may be quoted.
@return newly-allocated string, or NULL
*/
static gchar *_e2_fs_mount_get_device_for_mountpoint (const gchar *utfpath)
{
	gint local_length, current_length, longest_sofar;
	gchar *clean, *local, *thisdev, *bestdev;

//=========== OS-specific stuff =============
#if defined(__linux__)
	struct mntent *mounted;
	E2_FILE *f;
/* When the linux proc filesystem is mounted, the files /etc/mtab and /proc/mounts
	have very similar contents. The former has somewhat more information (CHECKME),
	such as the mount options used, but is not necessarily up-to-date */
	if ((f = setmntent ("/proc/mounts", "r")) == NULL
//	 && (f = setmntent ("/etc/mtab", "r")) == NULL
	)
		return NULL;

	clean = e2_utils_unquote_string (utfpath);
	if (clean == NULL)
		return NULL;
	local = F_FILENAME_TO_LOCALE (clean);
	local_length = strlen (local);	//length includes trailing /
	longest_sofar = 0;
	bestdev = NULL;

	while ((mounted = getmntent (f)) != NULL)
	{
		current_length = strlen (mounted->mnt_dir);
		if (strncmp (local, mounted->mnt_dir, current_length) == 0)
		{
			thisdev = mounted->mnt_fsname;
			if (current_length == local_length //they'll both be 1
				|| current_length == local_length - 1)	//same except for trailer
			{
				//we can stop immediately if this type matches
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
				break;
			}
			else if (current_length > longest_sofar)
			{
				longest_sofar = current_length;
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
			}
			else if (current_length == longest_sofar)
			{
				//FIXME decide which one is better to keep
			}
		}
	}
	endmntent (f);
#elif defined(__E2BSD__)
	struct statfs *mounted;

	clean = e2_utils_unquote_string (utfpath);
	if (clean == NULL)
		return NULL;
	local = F_FILENAME_TO_LOCALE (clean);
	local_length = strlen (local);	//length includes trailing /
	longest_sofar = 0;
	bestdev = NULL;

	//NOTE: data provided by this is not process- or thread-safe
	gint i, count = getmntinfo (&mounted, MNT_NOWAIT);

	for (i=0; i<count; i++)
	{
		current_length = strlen (mounted[i].f_mntonname);
		if (strncmp (local, mounted[i].f_mntonname, current_length) == 0)
		{
			thisdev = mounted[i].f_mntfromname;
			if (current_length == local_length //they'll both be 1
				|| current_length == local_length - 1)	//same except for trailer
			{
				//we can stop immediately if this type matches
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
				break;
			}
			else if (current_length > longest_sofar)
			{
				longest_sofar = current_length;
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
			}
			else if (current_length == longest_sofar)
			{
				//FIXME decide which one is better to keep
			}
		}
	}
#elif defined(__solaris__) || defined(sco)
	//CHECKME sco?
	struct mnttab mounted;
	E2_FILE *f;
	gint result;

	if ((f = e2_fs_open_stream ("/etc/mnttab", "r")) == NULL)
		return NULL;

	clean = e2_utils_unquote_string (utfpath);
	if (clean == NULL)
		return NULL;
	local = F_FILENAME_TO_LOCALE (clean);
	local_length = strlen (local);	//length includes trailing /
	longest_sofar = 0;
	bestdev = NULL;

	while ((result = getmntent (f, &mounted)) == 0)
	{
		current_length = strlen (mounted.mnt_mountp);
		if (strncmp (local, mounted.mnt_mountp, current_length) == 0)
		{
			thisdev = mounted.mnt_special;
			if (current_length == local_length //they'll both be 1
				|| current_length == local_length - 1)	//same except for trailer
			{
				//we can stop immediately if this type matches
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
				break;
			}
			else if (current_length > longest_sofar)
			{
				longest_sofar = current_length;
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
			}
			else if (current_length == longest_sofar)
			{
				//FIXME decide which one is better to keep
			}
		}
	}
	e2_fs_close_stream (f);
#elif defined(hpux)
	struct mntent *mounted;
	E2_FILE *f;

	const gchar *mtab = (e2_fs_access ("/etc/pfs_mtab", F_OK E2_ERR_PTR())) ?
		"/etc/mtab" : "/etc/pfs_mtab";
	if ((f = setmntent (mtab, "r")) == NULL)
		return NULL;

	clean = e2_utils_unquote_string (utfpath);
	if (clean == NULL)
		return NULL;
	local = F_FILENAME_TO_LOCALE (clean);
	local_length = strlen (local);	//length includes trailing /
	longest_sofar = 0;
	bestdev = NULL;

	while ((mounted = getmntent (f)) != NULL)
	{
		current_length = strlen (mounted->mnt_dir);
		if (strncmp (local, mounted->mnt_dir, current_length) == 0)
		{
			thisdev = mounted->mnt_fsname;
			if (current_length == local_length //they'll both be 1
				|| current_length == local_length - 1)	//same except for trailer
			{
				//we can stop immediately if this type matches
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
				break;
			}
			else if (current_length > longest_sofar)
			{
				longest_sofar = current_length;
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
			}
			else if (current_length == longest_sofar)
			{
				//FIXME decide which one is better to keep
			}
		}
	}
	endmntent (f);
#elif defined(__svr4__)
	struct mnttab mounted;
	E2_FILE *f;
	gint result;

	if ((f = e2_fs_open_stream ("/etc/mtab", "r") == NULL)
		return NULL;

	clean = e2_utils_unquote_string (utfpath);
	if (clean == NULL)
		return NULL;
	local = F_FILENAME_TO_LOCALE (clean);
	local_length = strlen (local);	//length includes trailing /
	longest_sofar = 0;
	bestdev = NULL;

	while ((result = getmntent (f, &mounted)) == 0)
	{
		current_length = strlen (mounted.f_mntonname);
		if (strncmp (local, mounted.f_mntonname, current_length) == 0)
		{
			thisdev = mounted.f_mntfromname;
			if (current_length == local_length //they'll both be 1
				|| current_length == local_length - 1)	//same except for trailer
			{
				//we can stop immediately if this type matches
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
				break;
			}
			else if (current_length > longest_sofar)
			{
				longest_sofar = current_length;
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
			}
			else if (current_length == longest_sofar)
			{
				//FIXME decide which one is better to keep
			}
		}
	}
	e2_fs_close_stream (f);
#else
//other unix-like os's do it like linux, but without /proc/mount
//CHECKME sometimes with a different filename ??
	struct mntent *mounted;
	E2_FILE *f;

	if ((f = setmntent ("/etc/mtab", "r")) == NULL)
		return NULL;

	clean = e2_utils_unquote_string (utfpath);
	if (clean == NULL)
		return NULL;
	local = F_FILENAME_TO_LOCALE (clean);
	local_length = strlen (local);	//length includes trailing /
	longest_sofar = 0;
	bestdev = NULL;

	while ((mounted = getmntent (f)) != NULL)
	{
		current_length = strlen (mounted->mnt_dir);
		if (strncmp (local, mounted->mnt_dir, current_length) == 0)
		{
			thisdev = mounted->mnt_fsname;
			if (current_length == local_length //they'll both be 1
				|| current_length == local_length - 1)	//same except for trailer
			{
				//we can stop immediately if this type matches
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
				break;
			}
			else if (current_length > longest_sofar)
			{
				longest_sofar = current_length;
				g_free (bestdev);
				bestdev = g_strdup (thisdev);
			}
			else if (current_length == longest_sofar)
			{
				//FIXME decide which one is better to keep
			}
		}
	}
	endmntent (f);
#endif	//which OS
//=========================
	g_free (clean);
	F_FREE (local, clean);
	return bestdev;
}
#endif	//def (E2_HAL) || def (E2_DEVKIT)
/**
@brief check whether @a localpath is a filesystem mountpoint

@param localpath localised absolute path string

@return TRUE if @a localpath is a mountpoint
*/
gboolean e2_fs_mount_is_mountpoint (VPATH *localpath)
{
	struct stat sb;
#ifdef E2_VFS
	if (localpath->spacedata != NULL)
		return FALSE;	//mountpoints are always local
#endif
	//root path is not detected by mountpoints lister
	if (!strcmp (VPSTR (localpath), G_DIR_SEPARATOR_S))
		return TRUE;

	if (e2_fs_lstat (localpath, &sb E2_ERR_NONE()) || !S_ISDIR (sb.st_mode))
		return FALSE;

	gboolean matched = FALSE;
	gchar *utf = F_FILENAME_FROM_LOCALE (VPSTR(localpath));
	GList *points = e2_fs_mount_get_mounts_list ();
	if (points != NULL)
	{
		matched = (g_list_find_custom (points, utf, (GCompareFunc) e2_list_strcmp) != NULL);
		e2_list_free_with_data (&points);
	}
#if defined(__linux__) || defined(__FreeBSD__)
	if (!matched)
	{
		points = e2_fs_mount_get_fusemounts_list ();
		if (points != NULL)
		{
			matched = (g_list_find_custom (points, utf, (GCompareFunc) e2_list_strcmp) != NULL);
			e2_list_free_with_data (&points);
		}
	}
#endif
	F_FREE (utf, VPSTR(localpath));
	return matched;
}
/**
@brief add @a dir to list @a mounts

The string is copied, with conversion to utf-8 if need be

@param dir localised string with path of mountpoint to be added
@param points store of pointer to the list to be updated

@return
*/
static void _e2_fs_mount_add_to_list (gchar *dir, GList **points)
{
	gchar *utf = D_FILENAME_FROM_LOCALE (dir);
	*points = g_list_append (*points, utf);
}
/**
@brief check whether @a localpath is a fuse mountpoint

@param localpath localised absolute path string

@return TRUE if @a localpath is a fuse mountpoint
*/
gboolean e2_fs_mount_is_fusepoint (VPATH *localpath)
{
#if defined(__linux__) || defined(__FreeBSD__)
# ifdef E2_VFS
	if (localpath->spacedata != NULL)
		return FALSE;	//mountpoints are always local
# endif
	gboolean retval;
#ifdef __linux__
	struct mntent *mounted;
	E2_FILE *f;
/* When the linux proc filesystem is mounted, the files /etc/mtab and /proc/mounts
	have very similar contents. The former has somewhat more information (CHECKME),
	such as the mount options used, but is not necessarily up-to-date */
	if ((f = setmntent ("/proc/mounts", "r")) == NULL
		&& (f = setmntent ("/etc/mtab", "r")) == NULL)
			return FALSE;

	retval = FALSE;
	while ((mounted = getmntent (f)) != NULL)
	{
		if (!strcmp (mounted->mnt_type, "fuse")
			&& g_str_has_prefix (VPCSTR (localpath), mounted->mnt_dir))
		{
			retval = TRUE;
			break;
		}
	}
	endmntent (f);
#elif defined(__FreeBSD__)
	retval = FALSE;
	struct statfs *mounted;
	//NOTE: data provided by this is not process- or thread-safe
	gint i, count = getmntinfo (&mounted, MNT_NOWAIT);

	for (i=0; i<count; i++)
	{
		if (!strcmp (mounted[i].f_fstypename, "fuse")
			&& g_str_has_prefix (VPCSTR (localpath), mounted[i].f_mntonname))
		{
			retval = TRUE;
			break;
		}
	}
#endif
	return retval;
#else
	return FALSE;
#endif
}
#if defined(__linux__) || defined(__FreeBSD__)
/**
@brief create list of fuse mountpoints
The list is created by the appropriate os-specifc protocol.
Mountpoints are stored as utf8 strings

@return list of utf-8 mountpoints, or NULL
*/
GList *e2_fs_mount_get_fusemounts_list (void)
{
	GList *mounts_list = NULL;
	//FIXME lock the data source while accessing it
#if defined(__linux__)
	struct mntent *mounted;
	E2_FILE *f;
/* When the linux proc filesystem is mounted, the files /etc/mtab and /proc/mounts
	have very similar contents. The former has somewhat more information (CHECKME),
	such as the mount options used, but is not necessarily up-to-date */
	if ((f = setmntent ("/proc/mounts", "r")) == NULL)
		if ((f = setmntent ("/etc/mtab", "r")) == NULL)
			return NULL;	//FIXME report an error

	while ((mounted = getmntent (f)) != NULL)
	{
		if (!strcmp (mounted->mnt_type, "fuse"))
			_e2_fs_mount_add_to_list (mounted->mnt_dir, &mounts_list);
	}
	endmntent (f);
#elif defined(__FreeBSD__)
	struct statfs *mounted;

	//NOTE: data provided by this is not process- or thread-safe
	gint i, count = getmntinfo (&mounted, MNT_NOWAIT);

	for (i=0; i<count; i++)
	{
		if (!strcmp (mounted[i].f_fstypename, "fuse"))
			_e2_fs_mount_add_to_list (mounted[i].f_mntonname, &mounts_list);
	}
#endif
	return mounts_list;
}
#endif //which OS
/* *
@brief replace escaped character codes in @a buf
@param buf buffer with string from fs/mtab, presumably locale-encoded
@return
*/
/* UNUSED
static void _e2_fs_mount_fix_fstab_name (gchar *buf)
{
	gchar *rp, *wp;

	if (buf[0] == '\0')
		return;
	rp = buf;
	wp = buf;
	do	// This loop same as in libc6 getmntent()
		if (rp[0] == '\\' && rp[1] == '0' && rp[2] == '4' && rp[3] == '0')
		{
			*wp++ = ' ';		// \040 is a SPACE
			rp += 3;
		}
		else if (rp[0] == '\\' && rp[1] == '0' && rp[2] == '1' && rp[3] == '2')
		{
			*wp++ = '\t';		// \012 is a TAB
			rp += 3;
		}
		else if (rp[0] == '\\' && rp[1] == '\\')
		{
			*wp++ = '\\';		// \\ is a \
			rp += 1;
		}
		else
			*wp++ = *rp;
	while (*rp++ != '\0');
} */
/* *
@brief check whether current user is permitted to [u]mount mountpoint @a device
@param device mountpoint path-string, presumably locale-encoded
@param options is a string with the (4th) parameters string from a line in fs/mtab
@return TRUE if permitted
*/
/* UNUSED FIXME, if this is used, needs to account for sudo & group membership etc
 and work properly for all os's
static gboolean _e2_fs_mount_permission (gchar* device, gchar *options)
{
	if (uid == 0 	//root can always do it
		|| strstr (options, "dev,") != NULL //CHECKME _I(
		|| (strstr (options, "user") != NULL && strstr (options, "nouser") == NULL)) //CHECKME _I(
		return TRUE;
#if defined (__linux__)
	struct stat my_stat;
	//no need to localise *device before stat
	return (e2_fs_stat (device, &my_stat E2_ERR_NONE()) == 0
		&& my_stat.st_uid == uid
		&& strstr (options, "owner") != NULL //CHECKME _I(
	   );
#else
	return FALSE;
#endif
} */

/**
@brief create list of mounted partitions (with some exception) in the native filesystem

The list is created by the appropriate os-specifc protocol.
@return list of partitions (UTF-8 strings), or NULL
*/
GList *e2_fs_mount_get_mounts_list (void)	//gboolean permcheck)
{
	GList *mounts_list = NULL;
	//FIXME lock the data source while accessing it
#if defined(__linux__)
	struct mntent *mounted;
	E2_FILE *f;
/* When the linux proc filesystem is mounted, the files /etc/mtab and /proc/mounts
	have very similar contents. The former has somewhat more information (CHECKME),
	such as the mount options used, but is not necessarily up-to-date */
	if ((f = setmntent ("/proc/mounts", "r")) == NULL)
		if ((f = setmntent ("/etc/mtab", "r")) == NULL)
			return NULL;

	while ((mounted = getmntent (f)) != NULL)
	{
		//check the mounted device meets our needs
		//need to take a copy if string is altered
		gchar *dev = mounted->mnt_fsname;
		gchar *type = mounted->mnt_type;
		gchar *dir = mounted->mnt_dir;
		if (_e2_fs_mount_is_unmountable (dev, type, dir))
		{
			E2_PROCESS_MOUNTED_DEVICE
			_e2_fs_mount_add_to_list (dir, &mounts_list);
		}
	}
	endmntent (f);
#elif defined(TRUE)
	struct statfs *mounted;

	//NOTE: data provided by this is not process- or thread-safe
	gint i, count = getmntinfo (&mounted, MNT_NOWAIT);

	for (i=0; i<count; i++)
	{
		//check the mounted device meets our needs
		//need to take a copy if string is altered
		gchar *dev = mounted[i].f_mntfromname;
		gchar *type = mounted[i].f_fstypename;
		gchar *dir = mounted[i].f_mntonname;
		if (_e2_fs_mount_is_unmountable (dev, type, dir))
		{
			E2_PROCESS_MOUNTED_DEVICE
			_e2_fs_mount_add_to_list (dir, &mounts_list);
		}
	}
#else
//other unix-like os's do it like linux, but without /proc/mount
//CHECKME sometimes with a different filename ??
	struct mntent *mounted;
	E2_FILE *f;

	if ((f = setmntent ("/etc/mtab", "r")) == NULL)
		return NULL;

	while ((mounted = getmntent (f)) != NULL)
	{
		//check the mounted device meets our needs
		//need to take a copy if string is altered
		gchar *dev = mounted->mnt_fsname;
		gchar *type = " "; //TODO mounted->mnt_type;
		gchar *dir = mounted->mnt_dir;
		if (_e2_fs_mount_is_unmountable (dev, type, dir))
		{
			E2_PROCESS_MOUNTED_DEVICE
			_e2_fs_mount_add_to_list (dir, &mounts_list);
		}
	}
	endmntent (f);
#endif	//which OS
	return mounts_list;
}
/**
@brief create list of mountable partitions (other than "/") in the native filesystem

The list is created by the appropriate os-specifc protocol.
Root dir is exlcluded because we don't ever need to mount that (? some other device chroot?)

@return list of partitions (utf-8 strings), or NULL
*/
GList *e2_fs_mount_get_mountable_list (void)	//gboolean permcheck)
{
	GList *fstab_list = NULL;
	//CHECKME lock data file while accessing it ??
#if defined(TRUE) || defined(__linux__)
    struct fstab *fs;

	if (!setfsent ())
		return NULL;

	while ((fs = getfsent ()) != NULL)
	{
		gchar *dev = fs->fs_spec;
		gchar *type = fs->fs_vfstype;
		gchar *dir = fs->fs_file;
/*		copy strings if they need to be modified
		g_strlcpy (dev, fs->fs_spec, sizeof(dev));
		g_strlcpy (dir, fs->fs_file, sizeof(dir));
		g_strlcpy (type, fs->fs_vfstype, sizeof(type));
		_e2_complete_mount_fix_fstab_name (dev);
		_e2_complete_mount_fix_fstab_name (dir);
		_e2_complete_mount_fix_fstab_name (type);
 */
		if (_e2_fs_mount_is_mountable (dev, type, dir))
		{
			E2_PROCESS_UNMOUNTED_DEVICE
			_e2_fs_mount_add_to_list (dir, &fstab_list);
		}
    }
    endfsent ();
#elif defined(__solaris__) || defined(sco)
	//CHECKME sco
    struct vfstab fs;
	E2_FILE *f;

	if ((f = e2_fs_open_stream ("/etc/vfstab", "r")) == NULL)
		return NULL;

	while (getvfsent (f, &fs) == 0)
	{
		gchar *dev = fs.vfs_special;
		gchar *type = " "; //TODO fs.vfs_fstype;
		gchar *dir = fs.vfs_mountp;
		if (_e2_fs_mount_is_mountable (dev, type, dir))
		{
			E2_PROCESS_UNMOUNTED_DEVICE
			_e2_fs_mount_add_to_list (dir, &fstab_list);
		}
	}
	e2_fs_close_stream (f);
#elif defined(hpux)
    struct mntent *mountable;
	E2_FILE *f;

    //handle PFS if necessary
    const gchar *fstab = (access("/etc/pfs_fstab", F_OK)) ?
		"/etc/fstab" : "/etc/pfs_fstab";

	if ((f = setmntent (fstab, "r")) == NULL)
		return NULL;

	while ((mountable = getmntent (f)) != NULL)
	{
		gchar *dev = mountable->mnt_fsname;
		gchar *type = " "; //TODO
		gchar *type = mountable->mnt_type;
		if (_e2_fs_mount_is_mountable (dev, type, dir))
		{
			E2_PROCESS_UNMOUNTED_DEVICE
			_e2_fs_mount_add_to_list (dir, &fstab_list);
		}
	}
	endmntent (f);
#elif defined(__svr4__)
	struct mnttab mountable;
	E2_FILE *f;

	if ((f = e2_fs_open_stream ("/etc/fstab", "r")) == NULL)
		return NULL;

	while (getmntent (f, &mountable) == 0)
	{
		gchar *dev = mountable.mnt_special;
		gchar *type = " "; //TODO
		gchar *dir = mountable.mnt_mountp;
		if (_e2_fs_mount_is_mountable (dev, type, dir))
		{
			E2_PROCESS_UNMOUNTED_DEVICE
			_e2_fs_mount_add_to_list (dir, &fstab_list);
		}
	}
	e2_fs_close_stream (f);
#else
	struct mntent *mountable;
	E2_FILE *f;

	if ((f = setmntent ("/etc/fstab", "r" )) == NULL)
		return NULL;

	while ((mountable = getmntent (f)) != NULL)	//data is not thread- or process-safe
	{
		gchar *dev = mountable->mnt_fsname;
		gchar *type = " "; //TODO mountable->mnt_type;
		gchar *dir = mountable->mnt_dir;
//		gchar *opts = mountable->mnt_opts;
		if (_e2_fs_mount_is_mountable (dev, type, dir))
		{
			E2_PROCESS_UNMOUNTED_DEVICE
			_e2_fs_mount_add_to_list (dir, &fstab_list);
		}
	}
	endmntent (f);
#endif	//which OS
	return fstab_list;
}
/**
@brief find the top dir of the device where @a localpath is
This is intended only for native dirs, so no VPATH involved
It analyses mounted-devices data, so is only meaningful when @a localpath is mounted
@param localpath absolute localised path of an item in local filesystem, may have trailing separator
@return newly allocated localised path, defaults to "/"
*/
gchar *e2_fs_mount_get_enclosing_point (const gchar *localpath)
{
	GList *mounts = e2_fs_mount_get_mounts_list ();	//list of UTF-8 paths other than /
	if (mounts != NULL)
	{
		GList *member;
		guint tlen = 0;
		const gchar *thispoint = NULL;
		gchar *converted = F_FILENAME_FROM_LOCALE (localpath);
		//assume that the longest matching mountpoint path is the one we want
		for (member = mounts; member != NULL; member = member->next)
		{
			gchar *s = (gchar *)member->data;
			if (g_str_has_prefix (converted, s))
			{
				guint len = 0;	//warning prevention
				if (thispoint == NULL || tlen <= (len = strlen (s)))
				{
					thispoint = s;
					tlen = len;
				}
			}
		}
		F_FREE (converted, localpath);
		if (thispoint != NULL)
			converted = D_FILENAME_TO_LOCALE (thispoint);
		else
			converted = g_strdup (G_DIR_SEPARATOR_S);	//NULL;
		e2_list_free_with_data (&mounts);
		return converted;
	}
	return g_strdup (G_DIR_SEPARATOR_S);	//NULL;
}
#if defined (E2_HAL) || defined (E2_DEVKIT)
/**
@brief check whether device on which @a utfpath resides is ejectable
This is intended only for native dirs, so no VPATH involved
It analyses mounted-devices data, so can only be used when @a utfpath is mounted
@param utfpath absolute path string, utf8, may be quoted, and/or have spaces
and/or trailing separator
@return TRUE if the dir is on a removable device
*/
gboolean e2_fs_mount_is_ejectable (const gchar *utfpath)
{
	gboolean retval;
	gchar *device = _e2_fs_mount_get_device_for_mountpoint (utfpath);
	if (device != NULL)
	{
		retval =
# ifdef E2_HAL
		e2_hal_device_is_ejectable (device);
# else
		e2_devkit_device_is_ejectable (device);
# endif
		g_free (device);
	}
	else
		retval = FALSE;
	return retval;
}
#endif //defined (E2_HAL) || defined (E2_DEVKIT)
/**
@brief check whether device on which @a utfpath resides is removable
@param utfpath absolute path string, utf8, may be quoted, and/or have spaces
and/or trailing separator
This is intended only for native dirs, so no VPATH involved
It analyses mounted-devices data, so can only be used when @a utfpath is mounted
@return TRUE if the dir is on a removable device
*/
gboolean e2_fs_mount_is_removable (const gchar *utfpath)
{
	gboolean retval;
#if defined (E2_HAL) || defined (E2_DEVKIT)
	gchar *device = _e2_fs_mount_get_device_for_mountpoint (utfpath);
	if (device != NULL)
	{
		retval =
# ifdef E2_HAL
		e2_hal_device_is_removable (device);
# else
		e2_devkit_device_is_removable (device);
# endif
		g_free (device);
	}
	else
		retval = FALSE;
#else
	//FIXME
//=========== OS-specific stuff =============
/*#if defined(__linux__)
#elif defined(__E2BSD__)
#elif defined(__solaris__) || defined(sco)
#elif defined(hpux)
#elif defined(__svr4__)
#else
#endif	//which OS
*/
//=========================
	retval = FALSE;	//default
#endif
	return retval;
}
/**
@brief register mountpoint-related actions
@return
*/
void e2_fs_mount_actions_register (void)
{
	E2_Action action =
	{g_strconcat(_A(64),".",_A(28),NULL),e2_menu_create_mounts_menu,TRUE,E2_ACTION_TYPE_ITEM,E2_ACTION_EXCLUDE_MENU,NULL,NULL};
	e2_action_register (&action);
}
#endif //def E2_FS_MOUNTABLE
