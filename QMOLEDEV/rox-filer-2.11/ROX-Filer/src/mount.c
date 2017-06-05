/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* mount.c - code for handling mount points */

#include "config.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_MNTENT_H
  /* Linux, etc */
# include <mntent.h>
#elif HAVE_SYS_UCRED_H
  /* NetBSD, OSF1, etc */
# include <fstab.h>
# include <sys/types.h>
# include <sys/param.h>
# include <sys/ucred.h>
# include <sys/mount.h>
# include <stdlib.h>
#elif HAVE_SYS_MNTENT_H
  /* SunOS */
# include <sys/mntent.h>
# include <sys/mnttab.h>
#endif
#include <sys/time.h>
#ifdef HAVE_SYS_VFS_H
#include <sys/vfs.h>
#endif
#ifdef HAVE_SYS_STATVFS_H
#include <sys/statvfs.h>
#endif

#include <gtk/gtk.h>

#include "global.h"

#include "mount.h"
#include "support.h"

/* Map mount points to mntent structures */
GHashTable *fstab_mounts = NULL;
time_t fstab_time;

/* Keys are mount points that the user mounted. Values are ignored. */
static GHashTable *user_mounts = NULL;

#ifdef HAVE_SYS_MNTENT_H
#define THE_FSTAB VFSTAB
#else
#define THE_FSTAB "/etc/fstab"
#endif

/* Static prototypes */
#ifdef DO_MOUNT_POINTS
static void read_table(void);
static void clear_table(void);
static time_t read_time(char *path);
static gboolean free_mp(gpointer key, gpointer value, gpointer data);
#endif


/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/

void mount_init(void)
{
	fstab_mounts = g_hash_table_new(g_str_hash, g_str_equal);
	user_mounts = g_hash_table_new_full(g_str_hash, g_str_equal,
					    g_free, NULL);

#ifdef DO_MOUNT_POINTS
	if(file_exists(THE_FSTAB))
	{
		fstab_time = read_time(THE_FSTAB);

	} else {
		fstab_time = 0;
		
#if defined(HAVE_MNTENT_H) || defined(HAVE_SYS_MNTENT_H)
		/* We need THE_FSTAB for these implementations, but
		 * it is missing */
		g_warning(_("File system table \"%s\" not found, cannot monitor system mounts"), THE_FSTAB);
#endif
	}
	read_table();
#endif
}

/* If force is true then ignore the timestamps */
void mount_update(gboolean force)
{
#ifdef DO_MOUNT_POINTS
	time_t	time;

	time = read_time(THE_FSTAB);
	if (force || time != fstab_time)
	{
		fstab_time = time;
		read_table();
	}
#endif /* DO_MOUNT_POINTS */
}

/* The user has just finished mounting/unmounting this path.
 * Update the list of user-mounted filesystems.
 */
void mount_user_mount(const char *path)
{
	if (mount_is_mounted(path, NULL, NULL))
		g_hash_table_insert(user_mounts, pathdup(path), "yes");
	else
		g_hash_table_remove(user_mounts, path);
}

/* TRUE iff this directory is a mount point. Uses python's method to
 * check:
 * The function checks whether path's parent, path/.., is on a different device
 * than path, or whether path/.. and path point to the same i-node on the same
 * device -- this should detect mount points for all Unix and POSIX variants.
 *
 * 'info' and 'parent' are both optional, saving one stat() each.
 */
gboolean mount_is_mounted(const guchar *path, struct stat *info,
					      struct stat *parent)
{
	struct stat info_path, info_parent;

	if (!info)
	{
		info = &info_path;
		if (stat(path, &info_path))
			return FALSE; /* Doesn't exist => not mount point :-) */
	}

	if (!parent)
	{
		guchar *tmp;
		parent = &info_parent;
		tmp = g_strconcat(path, "/..", NULL);
		if (stat(tmp, &info_parent))
		{
			g_free(tmp);
			return FALSE;
		}
		g_free(tmp);
	}

	if (info->st_dev != parent->st_dev)
		return TRUE;

	if (info->st_ino == parent->st_ino)
		return TRUE;	/* Same device and inode */
		
	return FALSE;
}

/* TRUE if this mount point was mounted by the user, and still is */
gboolean mount_is_user_mounted(const gchar *path)
{
	gboolean retval;
	gchar *real;

	real = pathdup(path);

	retval = g_hash_table_lookup(user_mounts, path) != NULL;

	if (retval)
	{
		/* Check the status is up-to-date */
		mount_user_mount(real);
		retval = g_hash_table_lookup(user_mounts, path) != NULL;
	}

	g_free(real);

	return retval;
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/


#ifdef DO_MOUNT_POINTS

static gboolean free_mp(gpointer key, gpointer value, gpointer data)
{
	MountPoint	*mp = (MountPoint *) value;

	g_free(mp->name);
	g_free(mp->dir);
	g_free(mp);

	return TRUE;
}

/* Remove all entries from mounts table, freeing them as we go */
static void clear_table(void)
{
	g_hash_table_foreach_remove(fstab_mounts, free_mp, NULL);
}

/* Return the mtime of a file - only used for the fstab file */
static time_t read_time(char *path)
{
	struct stat info;
	int err = 0;

	err = stat(path, &info);

	/* Don't print an error if the file is missing, just return the
	 * epoch (change if used for more than the fstab?) */
	if(err && errno==ENOENT)
		return 0;
	g_return_val_if_fail(err == 0, 0);

	return info.st_mtime;
}

# ifdef HAVE_MNTENT_H
static void read_table(void)
{
	FILE		*tab;
	struct mntent	*ent;
	MountPoint	*mp;
#  ifdef HAVE_FCNTL_H
	struct flock	lb;
#  endif

	clear_table();
	if(!file_exists(THE_FSTAB))
		return;

	tab = setmntent(THE_FSTAB, "r");
	g_return_if_fail(tab != NULL);

#  ifdef HAVE_FCNTL_H
	lb.l_type = F_RDLCK;
	lb.l_whence = 0;
	lb.l_start = 0;
	lb.l_len = 0;
	fcntl(fileno(tab), F_SETLKW, &lb);
#  endif

	while ((ent = getmntent(tab)))
	{
		if (strcmp(ent->mnt_dir, "swap") == 0)
			continue;

		mp = g_malloc(sizeof(MountPoint));
		mp->name = g_strdup(ent->mnt_fsname);
		mp->dir  = g_strdup(ent->mnt_dir);

		g_hash_table_insert(fstab_mounts, mp->dir, mp);
	}

	endmntent(tab);
}

# elif HAVE_SYS_MNTENT_H
static void read_table(void)
{
	FILE		*tab;
	struct mnttab	ent;
	MountPoint	*mp;
#  ifdef HAVE_FCNTL_H
	struct flock	lb;
#  endif

	clear_table();
	if(!file_exists(THE_FSTAB))
		return;

	tab = fopen(THE_FSTAB, "r");
	g_return_if_fail(tab != NULL);

#  ifdef HAVE_FCNTL_H
	lb.l_type = F_RDLCK;
	lb.l_whence = 0;
	lb.l_start = 0;
	lb.l_len = 0;
	fcntl(fileno(tab), F_SETLKW, &lb);
#  endif

	while (getmntent(tab, &ent)==0)
	{
		if (strcmp(ent.mnt_special, "swap") == 0)
			continue;

		mp = g_malloc(sizeof(MountPoint));
		mp->dir = g_strdup(ent.mnt_mountp);
		mp->name = g_strdup(ent.mnt_special);

		g_hash_table_insert(fstab_mounts, mp->dir, mp);
	}

	fclose(tab);
}

# elif HAVE_SYS_UCRED_H	/* We don't have getmntent(), etc */

static void read_table(void)
{
	int		tab;
	struct fstab	*ent;
	MountPoint	*mp;

	clear_table();

	tab = setfsent();
	g_return_if_fail(tab != 0);

	while ((ent = getfsent()))
	{
		if (strcmp(ent->fs_vfstype, "swap") == 0)
			continue;
		if (strcmp(ent->fs_vfstype, "kernfs") == 0)
			continue;

		mp = g_malloc(sizeof(MountPoint));
		mp->name = g_strdup(ent->fs_spec);	/* block special device name */
		mp->dir  = g_strdup(ent->fs_file);	/* file system path prefix */

		g_hash_table_insert(fstab_mounts, mp->dir, mp);
	}

	endfsent();
}

# endif /* HAVE_MNTENT_H */

#endif /* DO_MOUNT_POINTS */

gchar *mount_get_fs_size(const gchar *dir)
{
  int ok=FALSE;

#if defined(HAVE_STATVFS)
    struct statvfs buf;
#elif defined(HAVE_STATFS)
    struct statfs buf;
#endif
    unsigned long long total, used, avail;
    gdouble fused;
    gchar *str;
    gchar *tmp1, *tmp2;
    
#if defined(HAVE_STATVFS)
    ok=statvfs(dir, &buf)==0;
#elif defined(HAVE_STATFS)
    ok=statfs(dir, &buf)==0;
#endif
    if(!ok)
	    return NULL;
    
#if defined(HAVE_STATVFS)
    total=buf.f_frsize*(unsigned long long) buf.f_blocks;
    used=buf.f_frsize*(unsigned long long) (buf.f_blocks-buf.f_bfree);
    avail=buf.f_frsize*(unsigned long long) buf.f_bavail;
#elif defined(HAVE_STATFS)
    total=buf.f_bsize*(unsigned long long) buf.f_blocks;
    used=buf.f_bsize*(unsigned long long) (buf.f_blocks-buf.f_bfree);
    avail=buf.f_bsize*(unsigned long long) buf.f_bavail;
#endif
    if(total>0)
	    fused=100.*(total-used)/((gdouble) total);
    else
	    fused=0.0;

    tmp1=g_strdup(format_size(total));
    tmp2=g_strdup(format_size(used));
    str=g_strdup_printf(_("%s total, %s used, %s free (%.1f %%)"),
			tmp1, tmp2, format_size(avail), fused);

    g_free(tmp1);
    g_free(tmp2);

    return str;
}

