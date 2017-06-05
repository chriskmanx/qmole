/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

#ifndef _MY_VFS_H
#define _MY_VFS_H

#ifdef HAVE_LIBVFS

/* We have the VFS library. Include the header file if possible, or define
 * stuff ourselves if not.
 */

#  define VFS_STANDALONE 1
#  ifdef HAVE_VFS_H
#    include "vfs.h"
#  else
#    include <sys/stat.h>
#    include <unistd.h>
#    include <sys/types.h>
#    include <dirent.h>
void mc_vfs_init(void);
DIR  *mc_opendir (char *dirname);
struct dirent *mc_readdir(DIR *dirp);
int mc_closedir (DIR *dir);
int mc_telldir (DIR *dir);
void mc_seekdir (DIR *dir, int offset);

int mc_open  (const char *filename, int flags, ...);
int mc_close (int handle);
int mc_read  (int handle, char *buffer, int count);
int mc_write (int hanlde, char *buffer, int count);

int mc_stat  (char *path, struct stat *buf);
int mc_lstat (char *path, struct stat *buf);
int mc_fstat (int fd, struct stat *buf);
#  endif

#else	/* HAVE_LIBVFS */

/* We don't have VFS installed. Just use the normal functions instead. */

/* Include these here so that we don't get code that compiles with
 * VFS but not without.
 */
#  include <sys/stat.h>
#  include <unistd.h>
#  include <sys/types.h>
#  include <dirent.h>

#  define mc_open open
#  define mc_close close
#  define mc_read read
#  define mc_write write

#  define mc_stat(x, y) stat(x, y)
#  define mc_lstat(x, y) lstat(x, y)
#  define mc_fstat(x, y) fstat(x, y)
#  define mc_opendir(x) opendir(x)
#  define mc_closedir(x) closedir(x)
#  define mc_readdir(x) readdir(x)
#  define mc_seekdir(x, o) seekdir(x, o)
#  define mc_telldir(x) telldir(x)

#endif

#endif /* _MY_VFS_H */
