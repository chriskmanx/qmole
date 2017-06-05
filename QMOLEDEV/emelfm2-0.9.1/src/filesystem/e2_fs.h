/* $Id: e2_fs.h 2902 2013-10-31 22:31:52Z tpgww $

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
@file src/filesystem/e2_fs.h
@brief header for filesystem io functions
*/

#ifndef __E2_FS_H__
#define __E2_FS_H__

#include "emelfm2.h"
#include <unistd.h>
#include <dirent.h>
#include <utime.h>

#define E2_FILE FILE

typedef enum { E2_DIRWATCH_NO, E2_DIRWATCH_YES, E2_DIRWATCH_CHECK } E2_FsReadWatch;

#ifdef E2_VFS
#include "e2_vfs.h"

gint e2_fs_stat (VPATH *localpath, struct stat *buf, GError **error);
gint e2_fs_access2 (VPATH *localpath, GError **error);
gint e2_fs_access3 (VPATH *localpath, gint how, GError **error);
gboolean e2_fs_check_write_permission (VPATH *localpath, GError **error);
gboolean e2_fs_is_text (VPATH *localpath, GError **error);
gboolean e2_fs_is_exec2 (VPATH *localpath, GError **error);
//gboolean e2_fs_is_hidden (VPATH *localpath, GError **error);
//gboolean e2_fs_is_link (gchar *localpath, GError **error);
gboolean e2_fs_walk_link (gchar **local_path, GError **error);
gboolean e2_fs_is_dir3 (VPATH *localpath, GError **error);
gboolean e2_fs_recurse_mkdir (VPATH *localpath, gint mode, GError **error);
gboolean e2_fs_chdir (gchar *path, GError **error);
gboolean e2_fs_get_valid_path (gchar **utfpath, gboolean accessible, GError **error);
gboolean e2_fs_cd_isok (gchar *path, GError **error);
gboolean e2_fs_path_is_cased (VPATH *localpath);
E2_FSSensitive uint e2_fs_path_case (VPATH *localpath);
gint e2_fs_path_exists (VPATH *localpath);

//FIXME make this work for vfs
ssize_t e2_fs_read (gint descriptor, gpointer buffer, /*size_t*/ gulong bufsize,
	GError **error);
gboolean e2_fs_get_file_contents (VPATH *localpath, gpointer *contents,
	/*size_t*/gulong *contlength, gboolean terminate, GError **error);
//gboolean e2_fs_get_file_start (VPATH *localpath, gpointer *contents,
//	/*size_t*/gulong bufsize, gboolean terminate, GError **error);
ssize_t e2_fs_write (gint descriptor, gpointer buffer, /*size_t*/ gulong bufsize,
	GError **error);
gboolean e2_fs_set_file_contents (VPATH *localpath, gpointer contents,
	size_t contlength, mode_t mode, GError **error);
gpointer e2_fs_dir_foreach (VPATH *localpath, E2_FsReadWatch monitor,
	gpointer filterfunc, gpointer cb_data, GDestroyNotify free_data_func , GError **error);
gboolean e2_fs_copy_file (VPATH *src, const struct stat *src_sb, VPATH *dest,
	GError **error);

gboolean e2_fs_vfsfunc_ready (gpointer *func);

#else	//ndef E2_VFS

//access() looks through links, use e2_fs_access3 to prevent that
#define e2_fs_access access
#define e2_fs_lstat lstat
#define e2_fs_unlink unlink
#define e2_fs_remove remove
#define e2_fs_rmdir rmdir
#define e2_fs_mkdir mkdir
//no error check or report with this chdir
#define e2_fs_chdir_local chdir
//#define e2_fs_dir_open opendir
//#define e2_fs_dir_close closedir
//#define e2_fs_dir_read readdir_r
#define e2_fs_rename rename
#define e2_fs_chmod chmod
#define e2_fs_chown chown
#define e2_fs_lchown lchown
#define e2_fs_utime utime
#define e2_fs_symlink symlink

gint e2_fs_stat (VPATH *localpath, struct stat *buf);
gint e2_fs_access2 (VPATH *localpath);
gint e2_fs_access3 (VPATH *localpath, gint how);
gboolean e2_fs_check_write_permission (VPATH *localpath);
gboolean e2_fs_is_text (VPATH *localpath);
gboolean e2_fs_is_exec2 (VPATH *localpath);
//gboolean e2_fs_is_hidden (VPATH *localpath);
//gboolean e2_fs_is_link (VPATH *localpath);
gint e2_fs_readlink (VPATH *localpath, gchar *targetbuf, size_t targetsize);
gboolean e2_fs_walk_link (gchar **local_path);
gboolean e2_fs_is_dir3 (VPATH *localpath);
gboolean e2_fs_recurse_mkdir (VPATH *localpath, gint mode);
gboolean e2_fs_chdir (gchar *path);
gboolean e2_fs_get_valid_path (gchar **utfpath, gboolean accessible);
gboolean e2_fs_cd_isok (gchar *path);
gboolean e2_fs_path_is_cased (VPATH *localpath);
E2_FSSensitive e2_fs_path_case (VPATH *localpath);
gint e2_fs_path_exists (VPATH *localpath);

ssize_t e2_fs_read (gint descriptor, gpointer buffer, /*size_t*/ gulong bufsize);
gboolean e2_fs_get_file_contents (VPATH *localpath, gpointer *contents,
	/*size_t*/gulong *contlength, gboolean terminate);
//gboolean e2_fs_get_file_start (VPATH *localpath, gpointer *contents,
//	/*size_t*/gulong bufsize, gboolean terminate);
ssize_t e2_fs_write (gint descriptor, gpointer buffer, /*size_t*/ gulong bufsize);
gboolean e2_fs_set_file_contents (VPATH *localpath, gpointer contents,
	size_t contlength, mode_t mode);
gpointer e2_fs_dir_foreach (VPATH *localpath, E2_FsReadWatch monitor,
	gpointer filterfunc, gpointer cb_data, GDestroyNotify free_data_func);
gboolean e2_fs_copy_file (const gchar *src, const struct stat *src_sb,
	const gchar *dest);

#endif	//def E2_VFS

//functions that only make sense for local operations
#define e2_fs_fstat fstat
#define e2_fs_open_stream fopen
#define e2_fs_close_stream fclose
#define e2_fs_dir_open opendir
#define e2_fs_dir_read readdir_r
#define e2_fs_dir_close closedir

gboolean e2_fs_touchnow (VPATH *localpath
#ifdef E2_VFS
	, GError **error
#endif
	);

E2_FILE *e2_fs_open_writestream (const gchar *localpath
#ifdef E2_VFS
	, GError **error
#endif
	);
gint e2_fs_put_stream (E2_FILE *stream, const gchar *string, gchar *localpath
#ifdef E2_VFS
	, GError **error
#endif
	);

gboolean e2_fs_error (gchar *msg
#ifdef E2_VFS
	, gchar *reason
#endif
	);
gboolean e2_fs_error_local (const gchar *format, VPATH *local
#ifdef E2_VFS
		, gchar *reason
#endif
);
gboolean e2_fs_error_simple (const gchar *format, VPATH *local);

void e2_fs_check_coding (void);
gboolean e2_fs_ingroup (gid_t gid);
//formatted file write, used locally for config and cache writing
#define e2_fs_file_write fprintf
//pipes can only be used locally
#define e2_fs_pipe_close pclose
E2_FILE *e2_fs_open_pipe (gchar *command);	//always native

gchar *e2_fs_get_perm_string (mode_t mode) G_GNUC_MALLOC;	//string utility only
gboolean e2_fs_dir_is_native (const gchar *utfpath, gboolean descend);	//native comparisons only
gboolean e2_fs_complete_dir (GtkWidget *entry, guint keyval, guint pane);

gboolean e2_fs_is_executable (VPATH *infopath, FileInfo *info);
gboolean e2_fs_is_dir (VPATH *infopath, FileInfo *info);

//these flags are not standard, their position is > those used in S_IFMT (0170000)
//FIXME generalise this
//flag added to cached statbufs to turn off sorting dirs ahead of non-dirs
#define E2_MINGLEDIRS 0400000
#ifdef S_IFLNK
//flag added to cached statbufs to sort dir-links like dirs
# define E2_DIRLNK 0200000
# define ISDIR(mode) ((S_ISDIR(mode)||(mode&E2_DIRLNK)==E2_DIRLNK) && !(mode & E2_MINGLEDIRS))
#else
# define ISDIR(mode) (S_ISDIR(mode) && !(mode & E2_MINGLEDIRS))
#endif

//inline gboolean e2_fs_is_dir_fast (FileInfo *info, struct stat *statbuf);
//make this always inline ...
//here we use stat() because we want to traverse the link
//NOTE no path provided for the stat command, so CWD needs to be local and valid!!
/*#define e2_fs_is_dir_fast(a,b) \
  S_ISDIR (((FileInfo *)a)->statbuf.st_mode) \
	|| (S_ISLNK (((FileInfo *)a)->statbuf.st_mode) \
		&& !stat (((FileInfo *)a)->filename, (struct stat *)b) \
		&& S_ISDIR (((struct stat *)b)->st_mode))
there's no convenient and quick way to mimic the full path test used when
populating the treestore, but the results of that test are a trailing "/" for the
items of interest ...
*/
//#define e2_fs_is_dir_fast(a) g_str_has_suffix(a,G_DIR_SEPARATOR_S)
#define e2_fs_is_dir_fast(a) *(a+strlen(a)-sizeof(gchar))==G_DIR_SEPARATOR

gboolean e2_fs_get_command_output (gchar *command, gpointer *contents);	//always native

gint e2_fs_safeopen (const gchar *localpath, gint openflags, mode_t mode);
gint e2_fs_safeclose (gint file_desc);
gint e2_fs_writeflush (gint file_desc);
void e2_fs_touch_config_dir (void);	//always native

//away-from-normal declaration here, to avoid build problems !!
void e2_option_tree_write_to_file (E2_FILE *f, E2_OptionSet *set,
	GtkTreeIter *iter, gint level);

#ifdef E2_FAM
//these apply whether or not kernel-based fam is being used
void e2_fs_FAM_connect (void);
void e2_fs_FAM_disconnect (void);
void e2_fs_FAM_suspend (void);
void e2_fs_FAM_resume (void);
void e2_fs_FAM_change (gchar *olddir, E2_PaneRuntime *rt);
gboolean e2_fs_FAM_monitor_config (void);
gboolean e2_fs_FAM_cancel_monitor_config (void);
void e2_fs_FAM_check_dirty (volatile gint *p1dirty, volatile gint *p2dirty,
	volatile gint *cfgdirty);
#else
//just time-change polling
void e2_fs_FAM_check_dirty (volatile gint *p1dirty, volatile gint *p2dirty);
void e2_fs_FAM_config_stamp (void);
#endif

#ifdef E2_FAM_KERNEL
//void e2_fs_FAM_cancel_monitor_dir (gchar *path);
//void e2_fs_FAM_monitor_dir (gchar *path);
//void e2_fs_FAM_clean_reports (gchar *path);
gboolean e2_fs_FAM_cancel_monitor_dir (E2_PaneRuntime *rt);
void e2_fs_FAM_less_monitor_dir (gchar *path);
void e2_fs_FAM_more_monitor_dir (gchar *path);
#else
# define e2_fs_FAM_less_monitor_dir(p)
# define e2_fs_FAM_more_monitor_dir(p)
#endif

//flags for instructing e2_fs_tw how to perform its function
typedef enum
{
	E2TW_DEFAULT  = 0,
	E2TW_PHYS     = 1,		//perform physical walk, don't look though symlinks
	E2TW_MOUNT    = 1 << 1,	//report only items on same file system as the argument
//	E2TW_CHDIR    = 1 << 2,	//BAD chdir to each subdir after it is opened (probably for native dirs only)
//	E2TW_DEPTH    = 1 << 3,	//DISABLED open and process any subdir as soon as it's found,
							//instead of after all non-subdirs have been processed
	E2TW_ONLYDIR  = 1 << 4,	//report only directories and stat() fails
	E2TW_NODIR    = 1 << 5,	//no E2TW_D or E2TW_DRR reports - only non-directories
							//and stat() fails and dirs not-opened (E2TW_DL, E2TW_DM, E2TW_DNR)
	E2TW_FIXDIR   = 1 << 6,	//if possible, repair DNR situations and then report DRR instead of DNR
	E2TW_DC       = 1 << 7, //to allow immediate cleanup, issue a fake DP report
							//after some sorts of in-walker errors and after a
							//callback func initiates a STOP or a nested dir returns
							//STOP (not relevant with E2TW_NODIR)
	E2TW_QT       = 1 << 8,	//suppress some in-walker error messages e.g. after intentional DL
	E2TW_XQT      = 1 << 9,	//suppress ALL in-walker error messages
#ifdef E2_VFS
	E2TW_XERR     = 1 << 10 //tells the walker func that the _FIRST_ item in user_data
							//is a GError** for returning specific error data
#endif
} E2_TwFlags;

//codes for status reports to tw callback functions
//NOTE a hanging link to a dir is not identifed by glibc as a dir,
//but it is stattable, so it's treated as a non-dir, not E2TW_NS = BUG ??
typedef enum
{
	E2TW_F,		//not-directory or link
	E2TW_SL,	//symbolic link to a non-directory
	E2TW_SLN,	//symbolic link to a non-existent non-directory
	E2TW_D,		//opened a directory or symlink to a directory
	E2TW_DL,	//directory, not opened due to tree-depth limit
	E2TW_DM,	//directory, not opened due to different file system
	E2TW_DP,	//directory, all subdirs have been visited
	E2TW_DNR,	//unreadable directory that could not be remediated
	E2TW_NS,	//un-statable item (can be a link to unstattable dir)
	E2TW_DRR	//opened a formerly unreadable directory, now remediated
} E2_TwStatus;

//codes for feedback from tw callback functions
//some of them are not mutually-exclusive
typedef enum
{
	E2TW_CONTINUE = 0, //continue processing the items in the dir
	E2TW_STOP     = 1, //abort the walk, return FALSE from e2_fs_tw();
	E2TW_SKIPSUB  = 1 << 1,	//don't open the subdir (only meaningful after E2TW_D or E2TW_DRR)
	E2TW_CLEAN    = 1 << 2, //instruct the walker to issue a DP report, then stop (cleanup for breadth-first walks)
	E2TW_DRKEEP   = 1 << 3, //dir mode-changes prior to a DRR report don't need to be reverted
	E2TW_FIXME    = 1 << 8  //interim value for use inside callback, changed before returning
} E2_TwResult;

mode_t e2_fs_tw_adjust_dirmode (VPATH *localpath, const struct stat *statptr,
	gint howflags);
gboolean e2_fs_tw (VPATH *start_item, E2_TwResult (*callback) (),
	gpointer user_data, gint max_depth, E2_TwFlags exec_flags E2_ERR_ARG());

//a limit on level of nested dirs that will be processed in a treewalk
#define E2_DIRNEST_LIMIT 32

gchar *e2_fs_mount_get_enclosing_point (const gchar *localpath) G_GNUC_MALLOC;
#ifdef E2_HAL
gchar *e2_fs_mount_get_mountpoint_for_device (const gchar *device) G_GNUC_MALLOC;
#endif
gboolean e2_fs_mount_is_mountpoint (VPATH *localpath);
gboolean e2_fs_mount_is_removable (const gchar *utfpath);
gboolean e2_fs_mount_is_ejectable (const gchar *utfpath);
GList *e2_fs_mount_get_mounts_list (void) G_GNUC_MALLOC;	//gboolean permcheck)
GList *e2_fs_mount_get_mountable_list (void) G_GNUC_MALLOC;	//gboolean permcheck)
GList *e2_fs_mount_get_fusemounts_list (void) G_GNUC_MALLOC;
gboolean e2_fs_mount_is_fusepoint (VPATH *localpath);
#ifdef E2_FS_MOUNTABLE
void e2_fs_mount_actions_register (void);
#endif

#define E2_MOUNTCOMMAND "mount"
#define E2_UNMOUNTCOMMAND "umount"
//unless HAL code is patched, this is the only viable automount directory-path
#define E2_MOUNTPLACE G_DIR_SEPARATOR_S "media"

//shorthand for bsd-like os's supported here
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#define __E2BSD__
#endif

#ifdef E2_HAL
gboolean e2_hal_device_is_removable (const gchar *devpath);
gboolean e2_hal_device_is_ejectable (const gchar *devpath);
void e2_hal_init (void);
void e2_hal_disconnect (void);
#endif
#ifdef E2_DEVKIT
gboolean e2_devkit_device_is_removable (const gchar *devpath);
gboolean e2_devkit_device_is_ejectable (const gchar *devpath);
void e2_devkit_init (void);
void e2_devkit_disconnect (void);
#endif

#endif //ndef __E2_FS_H__
