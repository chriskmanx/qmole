/* $Id: e2_fs.c 3062 2014-02-15 00:47:11Z tpgww $

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
 @file src/filesystem/e2_fs.c
 @brief filesystem I/O functions
*/
/**
\page filesystem interactions with the file system

ToDo - describe how this happens

\section vfs the virtual filesystem

Not implemented yet.
*/

#include "emelfm2.h"
#include <string.h>
#include <fcntl.h>
#include <dirent.h>
#include <pthread.h>
#include <langinfo.h>
#include <pwd.h>
#include <grp.h>
#if defined(__FreeBSD__) || defined(__OpenBSD__)
# include <sys/param.h>
# include <sys/mount.h>
#else
# include <sys/param.h>
# include <sys/mount.h>
#endif
#ifndef MNT_LOCAL
# include <sys/statvfs.h>
#endif
#ifdef E2_MAGIC
# include <magic.h>
# include <dlfcn.h>
#endif
#include "e2_fs.h"
#include "e2_dialog.h"
#ifdef E2_VFS
#include "e2_plugins.h"
#endif
#include "e2_complete.h"

//some file-system identifiers (from <linux/magic.h>, which isn't generally available)
#ifndef MSDOS_SUPER_MAGIC
# define MSDOS_SUPER_MAGIC 0x4d44
#endif
#ifndef SMB_SUPER_MAGIC
# define SMB_SUPER_MAGIC 0x517B
#endif
#ifndef USBDEVICE_SUPER_MAGIC
# define USBDEVICE_SUPER_MAGIC 0x9fa2
#endif

typedef struct _E2_DRFuncArgs
{
	const gchar *localpath;	//absolute path of dir to process, localised string FIXME
	gboolean slowdir;	//TRUE to process dir with thread and timeout monitoring
	gpointer callback;	//function to call for each processed item, or NULL to append item's name to list
	gpointer cb_data;	//pointer to data to send to @a callback
	GDestroyNotify free_data_func;	//function to call at end of scan, to clean @a cb_data, or NULL
} E2_DRFuncArgs;

#if 0
/**
@brief check if any x permission flag is set in @a mode
@param mode
@return TRUE if any x flag is set
*/
//FIXME this test should be for current user only, cf writable test
static gboolean _e2_fs_S_ISEXE (mode_t mode)
{
	if ((S_IXUSR & mode) || (S_IXGRP & mode) || (S_IXOTH & mode))
		return TRUE;
	return FALSE;
}
#endif //0

/* #ifndef E2_FILES_UTF8ONLY
//check whether a character encoding is compatible with utf
static gboolean _e2_fs_charset_is_utf (gchar *filecoding)
{
	return (!strcmp (filecoding, "UTF-8") ||
			strstr (filecoding, "ASCII") != NULL ||
			!strcmp (filecoding, "ANSI_X3.4-1968") ||
			!strcmp (filecoding, "646") ||
			!strcmp (filecoding, "ISO646") ||
			!strcmp (filecoding, "ISO_646.IRV"));
}
//check whether the locale language is one of the english variants
static gboolean _e2_fs_language_is_english (void)
{
	const gchar *lang = g_getenv ("LANGUAGE");
	if (lang == NULL)
		lang = g_getenv ("LANG");
	return (lang != NULL && strstr (lang, "en_") != NULL);
}
*/
/* *
@brief check whether the native filesystem encoding is utf-8 (or ascii), and setup for conversion accordingly

@return
*/
/*void e2_fs_check_coding (void)
{
	gchar *filecoding;
	app.utf8_filenames = FALSE;
	//1st, check what the user specified about encoding
	if (e2_cl_options.encoding != NULL)
		app.utf8_filenames = _e2_fs_charset_is_utf (e2_cl_options.encoding);
	//otherwise, try to find something from the environment ...
	else
	{
		//for glib, $G_FILENAME_ENCODING has priority over $G_BROKEN_FILENAMES
		filecoding = (gchar *) g_getenv ("G_FILENAME_ENCODING");
		if (filecoding == NULL)
		{
			//check what the locale data knows about the encoding
			filecoding = (gchar *) g_getenv ("G_BROKEN_FILENAMES");
			if (filecoding != NULL)
			{
				filecoding = nl_langinfo (CODESET);
				app.utf8_filenames = _e2_fs_charset_is_utf (filecoding);
			}
			else
				app.utf8_filenames = _e2_fs_language_is_english ();
		}
		else if (!strcmp (filecoding, "@locale"))
		{
			const gchar *charset;
			app.utf8_filenames = g_get_charset (&charset)
			//we assume locales using english language have ascii filenames
			//(which are compatible with utf)
				|| _e2_fs_language_is_english ();
		}
		else
		{
		//FIXME maybe a list, check only its 1st member
			app.utf8_filenames = _e2_fs_charset_is_utf (filecoding)
				|| _e2_fs_language_is_english ();
		}
	}
	//set pointers to conversion functions
	if (app.utf8_filenames)
	{
		e2_display_from_locale = e2_fname_to_locale = e2_fname_from_locale
			= e2_utf8_not_converted;
 		e2_fname_dupto_locale = e2_fname_dupfrom_locale = g_strdup;
		e2_fname_free = e2_utf8_not_freed;
	}
	else
	{
		e2_display_from_locale = g_filename_display_name;
		e2_fname_to_locale = e2_fname_dupto_locale = e2_utf8_filename_to_locale;
		e2_fname_from_locale = e2_fname_dupfrom_locale = e2_utf8_filename_from_locale;
		e2_fname_free = g_free;
	}
} */
//#endif //ndef E2_FILES_UTF8ONLY

/**
@brief check whether @a localpath exists
Can't use the function access(), which (for glibc at least) looks through links
Errors other than non-readable are ignored
NOTE probably this does not work properly on FAT*, which are case-insensitive
@param localpath item to test, absolute localised string

@return 0 (FALSE) if @a localpath exists (like access(F_OK)), else -1
*/
gint e2_fs_access2 (VPATH *localpath E2_ERR_ARG())
{
	struct stat statbuf;
//	if (e2_fs_path_is_cased (localpath, NULL))
		return (e2_fs_lstat (localpath, &statbuf E2_ERR_SAMEARG()));
/*	else
	{
		can't determine combinations of ucase and lcase chars to stat
		what else ??
	} */
}
/**
@brief check whether @a localpath can be 'used' in a manner consistent with flags in @a howflags
This mimics the stdio function access(), except that it does not look through
links, as does access() (in glibc at least).
@param localpath localised name of file to test
@param howflags or'd combination of R_OK, W_OK, X_OK and/or F_OK

@return 0 (FALSE) if all @a howflags condition(s) are satisfied (like access()), else -1
*/
gint e2_fs_access3 (VPATH *localpath, gint howflags E2_ERR_ARG())
{
	struct stat statbuf;
	if (e2_fs_lstat (localpath, &statbuf E2_ERR_SAMEARG()))
		return -1;
	else if (! S_ISLNK (statbuf.st_mode))
		return (e2_fs_access (localpath, howflags E2_ERR_SAMEARG()));

	static uid_t myuid = -1;
	static gid_t mygid = -1;

	if (myuid == -1)
		myuid = getuid ();
	if (mygid == -1)
		mygid = getgid ();
	gboolean result = FALSE;

	if (howflags & R_OK)
	{
		if (statbuf.st_mode & S_IROTH)
			result = TRUE;
		else if (statbuf.st_mode & S_IRUSR)
			result = ((myuid == 0) || (myuid == statbuf.st_uid));
		else if (statbuf.st_mode & S_IRGRP)
			result = ((myuid == 0) || e2_fs_ingroup (statbuf.st_gid));
		if (!result)
			return -1;
	}
	if (howflags & W_OK)
	{
		if (statbuf.st_mode & S_IWOTH)
			result = TRUE;
		else if (statbuf.st_mode & S_IWUSR)
			result = ((myuid == 0) || (myuid == statbuf.st_uid));
		else if (statbuf.st_mode & S_IWGRP)
			result = ((myuid == 0) || e2_fs_ingroup (statbuf.st_gid));
		if (!result)
			return -1;
	}
	if (howflags & X_OK)
	{
		if (statbuf.st_mode & S_IXOTH)
			result = TRUE;
		else if (statbuf.st_mode & S_IXUSR)
			result = ((myuid == 0) || (myuid == statbuf.st_uid));
		else if (statbuf.st_mode & S_IXGRP)
			result = ((myuid == 0) || e2_fs_ingroup (statbuf.st_gid));
		if (!result)
			return -1;
	}
	//we already know the link exists, no need for F_OK test
	return 0;
}
/**
@brief check whether the current user may modify @a localpath
This is more than just a simple X-flag check
This does not look through links (target may not exist)

@param localpath localised string with full path of item to check

@return TRUE if the user has write authority for @a localpath
*/
gboolean e2_fs_check_write_permission (VPATH *localpath E2_ERR_ARG())
{
#ifdef E2_VFS
	if (!e2_fs_item_is_mounted (localpath))
	{
# ifdef E2_VFSTMP
	//detect virtual items that are never writable by any user
	//e.g. a local archive without write/change functionality
	//FIXME handle archive where can write file but cannot chmod etc
# endif
		return FALSE;
	}
#endif
	if (!e2_fs_access3 (localpath, W_OK E2_ERR_SAMEARG()))
		return TRUE;	//write permission is actually set
	//otherwise, check whether we can change it anyway
	struct stat statbuf;
	if (e2_fs_lstat (localpath, &statbuf E2_ERR_SAMEARG()))
		return FALSE;
	else if (S_ISLNK (statbuf.st_mode))
		return FALSE;	//link permissions are thoroughly evaluated in access3(), no point in repeating
	//CHECKME which of these are actually checked in access() ??
	uid_t id = getuid ();
	if (id == 0 || id == statbuf.st_uid)
		return TRUE;
	//NB for GRP and OTH this does not work, for dirs at least
	else if ((statbuf.st_mode & S_IWGRP) //must already be group-writable for a member to change it
		&& e2_fs_ingroup (statbuf.st_gid))
		return TRUE;
	else if (statbuf.st_mode & S_IWOTH)//must already be other-writable for another to change it
		return TRUE;
	return FALSE;
}
/**
@brief determine whether the current user is a member of group whose id is @a gid
This is mainly to check whether to manipulate USR, GRP or OTH permissions
@param gid a group id
@return TRUE if current user is part of the group
*/
gboolean e2_fs_ingroup (gid_t gid)
{
	gid_t mygid = getgid ();
	if (mygid == gid)
		return TRUE;
	struct passwd *pw_buf;
	struct group *grp_buf;
	gboolean ingroup = FALSE;
	pw_buf = getpwuid (getuid ());
	if (pw_buf != NULL)
	{
		grp_buf = getgrgid (gid);
		if (grp_buf != NULL && grp_buf->gr_mem != NULL)
		{
			gchar *myname = g_strdup (pw_buf->pw_name);
			gint i = 0;
	        while (grp_buf->gr_mem [i] != NULL)
			{
				if (!strcmp (grp_buf->gr_mem [i], myname))
				{
					ingroup = TRUE;
					break;
				}
				i++;
            }
			g_free (myname);
		}
	}
	return ingroup;
}
/**
@brief execute the file(1) command on @a localpath, and search each line of its output for @a string
Used in e2_fs_is_text, e2_fs_is_exec2, and e2_fs_is_executable
If @a localpath is a symlink, it will be traversed.
TODO error message assumes BGL closed.

@param localpath localised name of file to test
@param string ascii string for which to search, in the output from file command

@return TRUE if @a string is found, FALSE if not found, or some sort of error happened
*/
static gboolean _e2_fs_grep_file_output (VPATH *localpath, gchar *string E2_ERR_ARG())
{
	E2_ERR_PCLEAR

#ifdef E2_VFS
	if (e2_fs_item_is_mounted (localpath))
	{
#endif
#ifdef E2_MAGIC
		MagicIface ifc;

		if (e2_utils_fill_magic_iface (&ifc))
		{
			magic_t handle = ifc.open (
				MAGIC_PRESERVE_ATIME | MAGIC_RAW | MAGIC_ERROR | MAGIC_SYMLINK);
			if (handle != NULL)
			{
				ifc.load (handle, NULL); //load failure will result in NULL msg
				const gchar *msg = ifc.file (handle, VPCSTR (localpath));
				if (msg != NULL)
				{
					gboolean retval = (strstr (msg, string) != NULL);
					ifc.close (handle);
					dlclose (ifc.libhandle);
					return retval;
				}
				else
				{
					//FIXME warning
# ifdef E2_VFSTMP
					if (E2_ERR_NAME != NULL)
					{
						populate *E2_ERR_NAME
						X = ifc.error (handle);
					}
# endif
					ifc.close (handle);
				}
			}
			else
			{
				//FIXME warning
# ifdef E2_VFSTMP
				if (E2_ERR_NAME != NULL)
				{
					populate *E2_ERR_NAME
				}
# endif
			}
			dlclose (ifc.libhandle);
		}
#else
		//CHECKME weirdness when > 1 opening of the same file,
		//avoided by setting the cwd before opening the pipe
		gchar *local = D_FILENAME_TO_LOCALE (curr_view->dir); //always dup, to avoid dirchange race
# ifdef E2_VFS
		VPATH data = { local, NULL };	//always local
		if (e2_fs_chdir_local (&data E2_ERR_SAMEARG()))
# else
		if (e2_fs_chdir_local (local E2_ERR_NONE()))
# endif
		{
		// FIXME warn user
			g_free (local);
			return FALSE;
		}
		g_free (local);

		size_t bsize = E2_MAX_LEN;
# ifdef USE_GLIB2_10
		gchar *buf = (gchar *) g_slice_alloc ((gulong) bsize);
# else
		gchar *buf = (gchar *) g_try_malloc ((gulong) bsize);
# endif
#if (CHECKALLOCATEDWARN)
		CHECKALLOCATEDWARN (buf, return FALSE;); //TODO assumes BGL closed
#else
		if (buf == NULL)
			return FALSE;
#endif

//tag E2_BADQUOTES
		gchar *qp = e2_utils_quote_string (VPCSTR (localpath));
		gchar *command = e2_utils_strcat ("file -Lpr ", qp);	//want localised
		g_free (qp);
		E2_FILE *pipe = e2_fs_open_pipe (command);
		g_free (command);
		if (pipe == NULL)
		{
# ifdef USE_GLIB2_10
			g_slice_free1 (bsize, buf);
# else
			g_free (buf);
# endif
			return FALSE;
		}
		gboolean retval = FALSE;
# ifdef __USE_GNU
		while (getdelim (&buf, &bsize, '\n', pipe) > 1)	//not an empty line
# else
		while (fgets (buf, bsize, pipe) != NULL && *buf != '\n')
# endif
		{
			if (strstr (buf, string))
			{
				retval = TRUE;
				break;
			}
		}
		e2_fs_pipe_close (pipe);
# ifdef USE_GLIB2_10
		g_slice_free1 (bsize, buf);
# else
		g_free (buf);
# endif
		return retval;
#endif
#ifdef E2_VFS
	}
	else	//item is virtual
	{
# ifdef E2_VFSTMP
		//get information about virtual item, how ??
# endif
	}
#endif
	return FALSE;
}
/**
@brief test whether @a localpath is a text file

@param localpath localised name of file to test

@return TRUE if it is a text file
*/
gboolean e2_fs_is_text (VPATH *localpath E2_ERR_ARG())
{
#ifdef E2_VFS
	if (e2_fs_item_is_mounted (localpath))
		return _e2_fs_grep_file_output (localpath, "text" E2_ERR_SAMEARG()); //do not translate
	else	//item is virtual
	{
# ifdef E2_VFSTMP
	//FIXME do this some other way for virtual items e.g. test mimetype
# endif
		return FALSE;
	}
#else
	return _e2_fs_grep_file_output (localpath, "text" E2_ERR_SAMEARG()); //do not translate
#endif
}
/**
@brief test whether @a localpath is an executable item

@param localpath localised name of item to test

@return TRUE if it is an executable file (or at least, looks like one)
*/
gboolean e2_fs_is_exec2 (VPATH *localpath E2_ERR_ARG())
{
	if (e2_fs_access (localpath, X_OK E2_ERR_NONE()) == 0)
	{
#ifdef E2_VFS
		if (e2_fs_item_is_mounted (localpath))
			//FIXME "executable" doesn't grab all script files, but "application/" is no good
			return _e2_fs_grep_file_output (localpath, "executable" E2_ERR_SAMEARG()); //do not translate
		else	//item is virtual
		{
# ifdef E2_VFSTMP
		//FIXME do this some other way for virtual items e.g. test mimetype
# endif
			return FALSE;
		}
#else
		return _e2_fs_grep_file_output (localpath, "executable" E2_ERR_SAMEARG()); //do not translate
#endif
	}
	return FALSE;
}
/**
@brief test whether @a info belongs to an executable item
If the item is a symlink, its target will be tested
@param info pointer to a FileInfo data struct
@param infopath pointer to path data for @a info, with localised string

@return TRUE if system reports x permission, and the file(1) command says it's an application file
*/
gboolean e2_fs_is_executable (VPATH *infopath, FileInfo *info)
{
	gboolean retval = FALSE;
	gchar *local = g_build_filename (VPCSTR(infopath), info->filename, NULL);
#ifdef E2_VFS
	VPATH ddata =  { local, infopath->spacedata };
	VPATH *localpath = &ddata;
#else
	VPATH *localpath = local;
#endif
	if (!e2_fs_access (localpath, X_OK E2_ERR_NONE()))
	{	//permission is ok
		if (S_ISLNK (info->statbuf.st_mode))
			retval = TRUE;
		else
		{
			//non-link, double check ...
#ifdef E2_VFS
			if (e2_fs_item_is_mounted (localpath))
			{
#endif
				//see comment above about "executable" scan here
				retval = _e2_fs_grep_file_output (localpath, "executable" E2_ERR_NONE ());  //string not to be translated
#ifdef E2_VFS
			}
			else	//item is virtual
			{
# ifdef E2_VFSTMP
			//FIXME full path for vfs item
			//FIXME do this some other way for virtual items e.g. test mimetype
# endif
				retval = FALSE;
			}
#endif
		}
	}

	g_free (local);
	return retval;
}
/* *
@brief check whether @a path is a link

@param path localised path string

@return TRUE if @a info belongs to a directory or a link to a directory
*/
/*gboolean e2_fs_is_link (gchar *local_path)
{
	struct stat statbuf;
	return (!e2_fs_lstat (local_path, &statbuf E2_ERR_NONE()) && S_ISLNK (statbuf.st_mode));
} */
/**
@brief check whether item in active pane is a directory or a link to one
This assumes that the item's mode flags have been gathered using lstat(),
not stat() (i.e. no link look-through)
@param info ptr to FileInfo struct for the item
@param infopath pointer to path info for @a info, with localised string

@return TRUE if @a info belongs to a directory or a link to a directory
*/
gboolean e2_fs_is_dir (VPATH *infopath, FileInfo *info)
{
	//CHECKME want to return any vfs error ?
	if ( S_ISDIR (info->statbuf.st_mode) )
		return TRUE;
	if ( S_ISLNK(info->statbuf.st_mode) )
	{
		struct stat statbuf;

		gchar *localpath = g_build_filename (VPCSTR(infopath), info->filename, NULL);
#ifdef E2_VFS
		VPATH data = { localpath, infopath->spacedata };
		gboolean found = !e2_fs_stat (&data, &statbuf E2_ERR_NONE());
#else
		gboolean found = !e2_fs_stat (localpath, &statbuf E2_ERR_NONE());
#endif
		g_free (localpath);
		if (found && S_ISDIR (statbuf.st_mode))
			return TRUE;
	}
	return FALSE;
}
/* *
@brief inline version of func that checks whether item in active pane is a directory or a link to one
NOW CONVERTED TO AN EQUIVALENT DEFINE
This assumes that the mode flags have been gathered using
lstat, not stat (ie no link look-through)

Item needs to be in active pane because no path is added to info->filename

@param info FileInfo structure
@param statbuf pointer to statbuf to use for statting @a info ->filename

@return TRUE if @a info belongs to a directory or a link to a directory
*/
/*inline gboolean e2_fs_is_dir_fast (FileInfo *info, struct stat *statbuf)
{
	if (S_ISDIR (info->statbuf.st_mode) )
		return TRUE;
	if (S_ISLNK(info->statbuf.st_mode) )
	{
FIXME absolute path for stat()
		if (!stat (info->filename, statbuf)	//look thru the link
			&& S_ISDIR (statbuf->st_mode))
		  return TRUE;
	}
	return FALSE;
} */
/**
@brief check whether @a localpath is a directory or a link to one

@param localpath localised absolute path string

@return TRUE if @a localpath is a directory or a link to a directory
*/
gboolean e2_fs_is_dir3 (VPATH *localpath E2_ERR_ARG())
{
	struct stat statbuf;
	return (!e2_fs_stat (localpath, &statbuf E2_ERR_SAMEARG())
			&& S_ISDIR (statbuf.st_mode));
}
/**
@brief make directory @a localpath regardless of whether all ancestors exist
This only works for single-byte path-separator characters
@param localpath localised absolute path string
@param mode octal permissions of new dir e.g. 0777 or 0644

@return FALSE if @a creation succeeds (like mkdir)
*/
gboolean e2_fs_recurse_mkdir (VPATH *localpath, gint mode E2_ERR_ARG())
{
	struct stat sb;
	gchar c;
	gchar *s;
#ifdef E2_VFS
	VPATH data;
	data = *localpath;
#endif
	s = VPSTR(localpath); //skips check on fs root dir

	while ((s = strchr (s+1, G_DIR_SEPARATOR)) != NULL)
	{
		c = *s;
		*s = '\0';
#ifdef E2_VFS
		if (e2_fs_stat (&data, &sb E2_ERR_SAMEARG()))	//thru links, not e2_fs_, FIXME vfs
#else
		if (e2_fs_stat (localpath, &sb E2_ERR_SAMEARG()))	//thru links, not e2_fs_, FIXME vfs
#endif
		{
			if (E2_ERR_PISNOT (ENOENT) || e2_fs_mkdir (localpath, mode E2_ERR_NONE()))	//FIXME vfs
			{
				*s = c;
				E2_ERR_PCLEAR
				return TRUE;
			}
		}
		else if (! S_ISDIR (sb.st_mode))
		{
			*s = c;
			return TRUE;
		}
		*s = c;
	}
	//check last or only segment
	if (e2_fs_stat (localpath, &sb E2_ERR_SAMEARG()))	//thru links, not e2_fs_,
	{
		if (E2_ERR_PISNOT (ENOENT) || e2_fs_mkdir (localpath, mode E2_ERR_NONE()))	//FIXME vfs
		{
			E2_ERR_PCLEAR
			return TRUE;
		}
	}
	return FALSE;
}
/**
@brief change system directory to @a utfpath, if possible

@a utfpath may be a link, in which case it will be traversed
Error message expects BGL to be closed
@param path utf-8 string with path to directory to be opened

@return TRUE if @a path is opened ok
*/
gboolean e2_fs_chdir (gchar *utfpath E2_ERR_ARG())
{
#ifdef E2_VFSTMP
	//what about incomplete mount of fuse-dir
#endif
	gboolean retval;
	E2_ERR_BACKUP (localerr);
	gchar *local = F_FILENAME_TO_LOCALE (utfpath);
#ifdef E2_VFS
	VPATH data = { local, NULL };
	if (e2_fs_chdir_local (&data E2_ERR_SAMEARG()))
#else
	if (e2_fs_chdir_local (local))
#endif
	{	//go there failed
#ifdef E2_VFS
		e2_output_print_error ((*E2_ERR_NAME)->message, FALSE);
#else
		e2_output_print_strerrno ();
#endif
		retval = FALSE;
	}
	else
		retval = TRUE;

	E2_ERR_CLEARBACKUP (localerr);
	F_FREE (local, utfpath);
	return retval;
}
/**
@brief check whether the user is able to change CWD to the directory @a utfpath

This tests @a utfpath for existence, is-a-directory, is-executable and readable
If @a utfpath is a link, it will be traversed
Error message expects BGL to be closed
@param utfpath utf-8 string with path of directory to be checked

@return TRUE if @a path is ok to cd to, or is not mounted-local
*/
gboolean e2_fs_cd_isok (gchar *utfpath E2_ERR_ARG())
{
	gchar *message;
#ifdef E2_VFSTMP
	//FIXME relevant spacedata to open ?
#endif
	gchar *local = F_FILENAME_TO_LOCALE (utfpath);
#ifdef E2_VFS
	VPATH data = { local, NULL };
	gboolean success = !e2_fs_access (&data, F_OK E2_ERR_SAMEARG());
#else
	gboolean success = !e2_fs_access (local, F_OK E2_ERR_SAMEARG());
#endif
	if (success)
	{
#ifdef E2_VFS
		if (!e2_fs_is_dir3 (&data E2_ERR_SAMEARG()))
#else
		if (!e2_fs_is_dir3 (local E2_ERR_SAMEARG()))
#endif
		{
			message = g_strdup_printf (_("'%s' is not a directory"), utfpath);
			success = FALSE;
		}
#ifdef E2_VFS
		else if (e2_fs_access (&data, R_OK | X_OK E2_ERR_SAMEARG()))
#else
		else if (e2_fs_access (local, R_OK | X_OK E2_ERR_SAMEARG()))
#endif
		{
			message = g_strdup_printf (_("Cannot access directory '%s' - No permission"), utfpath);
			success = FALSE;
		}
	}
	else
	{
		//dir doesn't exist
		if (e2_fs_dir_is_native (utfpath, TRUE))
		{
			message = g_strdup_printf (_("Directory '%s' does not exist"), utfpath);
		}
		else
		{
			E2_ERR_PCLEAR
			success = TRUE;	//missing path ok when it's non-mounted
		}
	}

	if (!success)
	{
		e2_output_print_error (message, TRUE);
	}
	F_FREE (local, utfpath);
	return success;
}
/**
@brief check whether directory @a utfpath is valid, and optionally, accessible, and if not, pick a fallback
If @a utfpath refers to a link, it will be traversed.
If test(s) are not satisfied, @a path is set to a different path string,
pointing to a place up the tree branch that does pass the test(s)
If a different path string is returned, the old one is freed, of course

@param utfpath store for utf-8 path string which may be invalid, must be freeable
@param accessible TRUE to find a path that has X and R permissions, FALSE if don't care about that

@return TRUE if @a path meets the specified test(s), FALSE if a replacement has been provided
*/
gboolean e2_fs_get_valid_path (gchar **utfpath, gboolean accessible E2_ERR_ARG())
{
	gboolean sub = FALSE;
#ifdef E2_VFSTMP
	//FIXME relevant path to open ? walkup *utfpath, past its root if vdir,
	//revert to local space if vfs fails
	//caller will want a PlaceInfo * and a message about any change of place
	get relevant spacedata
#endif
#ifdef E2_VFS
	VPATH data;
#endif
	gchar *local = D_FILENAME_TO_LOCALE (*utfpath);
	gchar *freeme, *p;
#ifdef E2_VFSTMP
	//what about incomplete mount of fuse-dir
#endif
	g_strchug (local);
	if (*local != '\0')
	{
		//CHECKME do a full interpretation here ?
		if (local[0] == '~') //home dir check
		{
			if (local[1] == '\0')
			{
				g_free (local);
				local = g_strdup (g_get_home_dir ());
				sub = TRUE;
			}
			else
				if (local[1] == G_DIR_SEPARATOR)
			{
				freeme = local;
				local = g_strconcat (g_get_home_dir (), local + sizeof(gchar), NULL);
				g_free (freeme);
				sub = TRUE;
			}
			else
			{	//try to get some user's home
				freeme = e2_utils_get_home_path (*utfpath); //FIXME local was chug'd
				if (freeme != NULL)
				{
					g_free (local);
					local = D_FILENAME_TO_LOCALE (freeme);
					g_free (freeme);
					sub = TRUE;
				}
			}
		}
		else if (local[0] != G_DIR_SEPARATOR) //relative path check
		{
			p = g_get_current_dir ();
			freeme = local;
			local = e2_utils_translate_relative_path (p, freeme);
			g_free (freeme);
			g_free (p);
			sub = TRUE;
		}

#ifdef E2_VFS
		data.path = local;
		data.spacedata = NULL;
		if (e2_fs_is_dir3 (&data E2_ERR_NONE())
			&& (!accessible || !e2_fs_access (&data, R_OK | X_OK E2_ERR_NONE())))
#else
		if (e2_fs_is_dir3 (local E2_ERR_NONE())
			&& (!accessible || !e2_fs_access (local, R_OK | X_OK E2_ERR_NONE())))
#endif
		{
			if (sub)
			{
				g_free (*utfpath);
				*utfpath = local;
			}
			else
				g_free (local);
			return TRUE;
		}
	}
	//walk back up the tree branch until we find an acceptable dir
	while (e2_utils_get_parent_path (local, TRUE))
	{
#ifdef E2_VFSTMP
	//FIXME new path may be virtual item eg archive ?
#endif
#ifdef E2_VFS
//		data.path = local; e2_utils_get_parent_path() doesn't change path
		if (e2_fs_is_dir3 (&data E2_ERR_NONE())
			&& (!accessible || !e2_fs_access (&data, R_OK | X_OK E2_ERR_NONE())))
#else
		if (e2_fs_is_dir3 (local E2_ERR_NONE())
			&& (!accessible || !e2_fs_access (local, R_OK | X_OK E2_ERR_NONE())))
#endif
		{	//replace the invalid path string with the valid one
			p = D_FILENAME_FROM_LOCALE (local);	//copy in case not changed by macro
			g_free (*utfpath);
			*utfpath = p;
			g_free (local);
			return FALSE;
		}
	}
	//whole branch bad, revert to default
#ifdef E2_VFSTMP
	//FIXME relevant default path to open ?
#endif
//	p = g_strdup (G_DIR_SEPARATOR_S);
	p = g_strconcat (g_get_home_dir (), G_DIR_SEPARATOR_S, NULL);
	g_free (*utfpath);
	*utfpath = D_FILENAME_FROM_LOCALE (p);	//copy in case not changed by macro
	g_free (p);
	g_free (local);
	return FALSE;
}
/**
@brief complete a directory string in @a entry after a keypress @a pressed_char
This works only for native directories
@param entry the entry widget to be updated
@param pressed_char UTF-8 string-form of pressed key which triggered the completion
@param pane enumerator of pane to use for default dir, 1,2,0=current

@return TRUE if completion was performed
*/
static gboolean _e2_fs_complete_dir (GtkWidget *entry, gchar *pressed_char, guint pane)
{
	gboolean retval;
	//start is characters, not bytes
	gint start = gtk_editable_get_position (GTK_EDITABLE (entry));
	gint pos = start + 1;	//+1 to include the key just pressed
	gchar *entrytext = gtk_editable_get_chars (GTK_EDITABLE (entry), 0, start);
	gchar *text = g_strconcat (entrytext, pressed_char, NULL);

	//do macros/variables replacement CHECKME worth it ?
	printd (DEBUG, "_e2_fs_complete_dir, macro/var interpretation");
	gchar *tmp = text;
	gchar *freeme = e2_utils_expand_macros (text, NULL); //note %% changes to %
	if (freeme == NULL || freeme == GINT_TO_POINTER (0x1))
		freeme = tmp;
	text = e2_utils_replace_vars (freeme, TRUE);
	if (freeme != tmp)
		g_free (freeme);
	if (strcmp (tmp, text))
		pos = g_utf8_strlen (text, -1);
	g_free (tmp);

	gchar *utfdir;
	if (g_path_is_absolute (text))
		utfdir = g_path_get_dirname (text);
	else
	{
		switch (pane)
		{
			case E2PANE1:
				utfdir = app.pane1.view.dir;
				break;
			case E2PANE2:
				utfdir = app.pane2.view.dir;
				break;
//			case E2PANECUR:
			default:
				utfdir = curr_view->dir;
				break;
		}
		freeme = g_strconcat (utfdir, text, NULL);
		utfdir = g_path_get_dirname (freeme);
		g_free (freeme);
	}
#ifdef E2_VFSTMP
	//CHECKME enable vfs completion ??
#endif
	if (e2_fs_dir_is_native (utfdir, FALSE))	//CHECKME parent-checking too?
	{
		GList *found = NULL;
		retval = (e2_complete_str (&text, &pos, &found, E2_COMPLETE_FLAG_DIRS, pane) > 0);
		if (retval)
		{
			g_free (entrytext);
			entrytext = gtk_editable_get_chars (GTK_EDITABLE (entry), start, -1);
			gchar *newtext = g_strconcat (text, entrytext, NULL);
			gtk_entry_set_text (GTK_ENTRY (entry), newtext);
			gtk_editable_set_position (GTK_EDITABLE (entry), pos);
			e2_command_line_update_highlight (entry, newtext);
			g_free (newtext);
			e2_list_free_with_data (&found);
		}
	}
	else
		retval = FALSE;

	g_free (utfdir);
	g_free (entrytext);
	g_free (text);
	return retval;
}
/**
@brief auto-complete a directory path string in @a entry after a keypress @a keyval
Completion is performed (or not) according to the relevant config option
This does not check for a valid key (non-modifier < 0xF000, >0xFFFF) - that
should be done before calling here
@param entry the entry widget to be updated
@param keyval code of pressed key which triggered the completion
@param pane enumerator of pane to use for default dir, 1,2,0=current

@return TRUE if completion was performed
*/
gboolean e2_fs_complete_dir (GtkWidget *entry, guint keyval, guint pane)
{
	guint32 unikey = gdk_keyval_to_unicode (keyval);
	if (unikey > 0)
	{
		gint start, end;
		gchar keystring[8];
		gint bytes = g_unichar_to_utf8 (unikey, keystring);
		*(keystring+bytes) = '\0';
		gint type = e2_option_sel_get ("dir-line-completion");
		switch (type)
		{
			case 1:	//insert completed text, and move after it
				if (gtk_editable_get_selection_bounds (GTK_EDITABLE (entry), &start, &end))
					gtk_editable_delete_text (GTK_EDITABLE (entry), start, end);
				if (_e2_fs_complete_dir (entry, keystring, pane))
					return TRUE;
				break;
			case 2:	//select completed text
			{
				gint newpos, oldpos;
				if (gtk_editable_get_selection_bounds (GTK_EDITABLE (entry), &start, &end))
				{
					//when there's a selection, cursor position = end, fix that
					oldpos = start;
					gtk_editable_delete_text (GTK_EDITABLE (entry), start, end);
				}
				else
					oldpos = gtk_editable_get_position (GTK_EDITABLE (entry));

				if (_e2_fs_complete_dir (entry, keystring, pane))
				{
					newpos = gtk_editable_get_position (GTK_EDITABLE (entry));
					gtk_editable_select_region (GTK_EDITABLE (entry), oldpos+1, newpos);
					return TRUE;
				}
			}
				break;
			default:	//no auto-completion
				break;
		}
	}
	return FALSE;
}
/**
@brief evaluate whether @a utfpath is a native dir or optionally, a descendant of one
Native means local and mounted, not fuse or other vfs
This is for tailoring pre-change-dir and pre-refresh-dir checking
@param utfpath absolute path of dir to check, utf-8 string
@param descend TRUE to check for a descendant

@return TRUE if @a utfpath is or seems to be native
*/
gboolean e2_fs_dir_is_native (const gchar *utfpath, gboolean descend)
{
#if defined(__linux__) || defined(__FreeBSD__)
	GList *fusemounts, *member;
	fusemounts = e2_fs_mount_get_fusemounts_list ();
	for (member = fusemounts ; member != NULL; member = member->next)
	{
		//member->data is utf with no trailing "/"
		if (g_str_has_prefix (utfpath, (gchar *)member->data))
			break;	//path is a descendant of a fuse mountpoint
	}
	if (fusemounts != NULL)
		e2_list_free_with_data (&fusemounts);
	if (member != NULL)
		return FALSE;
#endif
#ifdef MNT_LOCAL
	gint result;
	gchar *localpath;
	struct statfs fstatus;

	localpath = D_FILENAME_TO_LOCALE (utfpath);
retry:
	result = statfs (localpath, &fstatus);
	if (result)
	{
		if (result == ESTALE)
		{
			printd (DEBUG, "%s resides in an unmounted VFS", utfpath);
			//CHECKME mount it ?
		}
		g_free (localpath);
		return TRUE;	//any operation on utfpath will fail ASAP
	}
	if (!(fstatus.f_flags & MNT_LOCAL))
	{
//		if (errno == EINTR) CHECKME relevance ?
//			goto retry;
//		else
			if (errno == ENOENT && descend && e2_utils_get_parent_path (localpath, TRUE))
			goto retry;
		g_free (localpath);
		return (errno == EACCES);	//no permission implies a local dir
	}
#else
	struct statvfs fstatus;
	gchar *localpath;
	localpath = D_FILENAME_TO_LOCALE (utfpath);
retry:
	if (statvfs (localpath, &fstatus))
	{
		if (errno == EINTR)
			goto retry;
//		else if (errno == ENOLINK)
//			re-mount it ??
		else if (errno == ENOENT && descend && e2_utils_get_parent_path (localpath, TRUE))
			goto retry;
		g_free (localpath);
		return (errno == EACCES);	//no permission implies a local dir
	}
#endif
	//FIXME MORE TESTS HERE e.g.
	//from string contents e.g. "//:"
	//from plugin data
	//from vfs history data
	//from other e2 data
	g_free (localpath);
	return TRUE;
}
/**
@brief get enumerator describing whether @a localpath's file-system is case-sensitive

MS-DOS/Windows FAT* are case-insensitive, but so-called "long" names are
case-preserving.
NTFS can be fully case sensitive or just case preserving.
Classic Mac OS was case-insensitive but case-preserving. HFS+ on Mac OS X is
probably the same. (its tag is "hfsplus") Mac HFSX is fully case senstive.
Samba servers usually exhibit case-insensitive behaviour, regardless of the
actual fs properties.
For our purposes here, probably best to treat treat -preserving as -sensitive.

@param localpath localised name of file/dir to test

@return the value
*/
E2_FSSensitive e2_fs_path_case (VPATH *localpath)
{
	E2_FSSensitive case_sensitive_names;

#ifdef E2_VFSTMP
	if (localpath->spacedata == NULL)
	{
#endif
		gchar *local = VPSTR(localpath);
//=========== OS-specific stuff =============
#if defined(__linux__)
#include <sys/vfs.h>
//#include <linux/magic.h> only in glibc-devel
		struct statfs buf;
	retry:
		if (statfs (local, &buf))
		{
			if (errno == EINTR)
				goto retry;
			case_sensitive_names = E2FSCASE_NEVER;
		}
		else
		{
			switch (buf.f_type)
			{
				case MSDOS_SUPER_MAGIC:
				case SMB_SUPER_MAGIC:
					case_sensitive_names = E2FSCASE_ANY;
					break;
				default:
					case_sensitive_names = E2FSCASE_SENSITIVE;
					break;
			}
		}
#elif defined(__E2BSD__)
#include <sys/param.h>
#include <sys/mount.h>
		struct statfs buf;

		if (statfs (local, &buf))
		{
			case_sensitive_names = E2FSCASE_NEVER;
		}
		else
		{
			gchar *type = buf.f_fstypename;
			if (strcmp (type, "vfat") == 0)
				case_sensitive_names = E2FSCASE_ANY;
			else
				case_sensitive_names = E2FSCASE_SENSITIVE;
		}
#elif defined(__solaris__) || defined(sco) || defined(hpux) || defined(__svr4__)
#include <sys/types.h>
#include <sys/statvfs.h>
		struct statvfs buf;

		if (statvfs (local, &buf))
		{
			case_sensitive_names = E2FSCASE_NEVER;
		}
		else
		{
			gchar *type = buf.f_basetype;
			if (strcmp (type, "vfat") == 0)
				case_sensitive_names = E2FSCASE_ANY;
			else
				case_sensitive_names = E2FSCASE_SENSITIVE;
		}
#else
#include <sys/param.h>
#include <sys/mount.h>
		struct statfs buf;

		if (statfs (local, &buf))
		{
			case_sensitive_names = E2FSCASE_NEVER;
		}
		else
		{
			gchar *type = buf.f_fstypename;
			if (strcmp (type, "vfat") == 0)
				case_sensitive_names = E2FSCASE_ANY;
			else
				case_sensitive_names = E2FSCASE_SENSITIVE;
		}
#endif	//which OS
//=========================
#ifdef E2_VFSTMP
	}
	else
	{
		figure it out for virtual dir
	}
#endif
	return case_sensitive_names;
}
/**
@brief test whether @a localpath's file-system is case-sensitive

@param localpath localised name of file/dir to test

@return TRUE if path/name is case-sensitive
*/
gboolean e2_fs_path_is_cased (VPATH *localpath)
{
	E2_FSSensitive cased = e2_fs_path_case (localpath);
	return (cased == E2FSCASE_SENSITIVE ||
			cased == E2FSCASE_SENSITIVENOW);
}
/**
@brief check whether @a localpath exists

@brief localpath localised path string

@return 2 if it exists, 1 if cannot be sure (due to possible name-case-difference), 0 if not exists
*/
gint e2_fs_path_exists (VPATH *localpath)
{
	if (e2_fs_access2 (localpath E2_ERR_NONE()) == 0)
	{
		//BUT maybe we've only found a case-difference that looks the same to the kernel
		return (e2_fs_path_is_cased (localpath)) ? 2 : 1;
	}
	return 0;
}
#ifdef E2_VFS
/* *
@brief thread-function to read all of a virtual directory
@a view ->dir has the target path
To eliminate BGL-racing, no UI-change from here
@param view ptr to data struct for the view to be updated

@return list of FileInfo's for entries (maybe NULL), or error code if a problem occurred
*/
/*static gpointer _e2_fs_read_virtual_dir (E2_DRFuncArgs *data)
{
	e2_utils_block_thread_signals ();	//block all allowed signals to this thread

	gint oldtype = 0;
	pthread_setcanceltype (PTHREAD_CANCEL_ASYNCHRONOUS, &oldtype);

	gchar *itemname;
	gboolean (*processor) (gchar *, gchar *, GList **, gpointer) = data->callback;
	GList *member, *entries, *wanted;
	//this will leak list data and cb_data if that's supposed to be cleared here
	pthread_cleanup_push ((gpointer)g_list_free, (gpointer)entries);
//	E2_BLOCK	//don't want this interrupted

#ifdef E2_VFSTMP
	THIS NEEDS WORK
	//populate list using vfs read dir function
	entries = (GList *)e2_gvfs_dir_foreach (data->localpath, data->callback,
		data->cb_data);
#else
	entries = NULL;
#endif
	if (!E2DREAD_FAILED(entries))
	{
		wanted = NULL;
		for (member = entries; member != NULL; member = member->next)
		{
			itemname = (gchar *)member->data;
			//one item we're not interested in
			if (itemname[0] != '.' || itemname[1] != '\0')
			{
				if (processor != NULL)
				{
					if (!processor (data->localpath, itemname, &entries, data->cb_data))
						break;
				}
				else
					wanted = g_list_append (entries, g_strdup (itemname));
			}
		}
	}
//	E2_UNBLOCK
	e2_list_free_with_data (&entries);

	if (data->free_data_func != NULL)
		data->free_data_func (data->cb_data);

	pthread_cleanup_pop (0);	//free entries list
	pthread_setcanceltype (oldtype, NULL);

	return wanted;
} */
#endif
/**
@brief synchronous or thread-function to record some or all the items in a mounted directory

This is called synchronously when the fstype of the directory being processed is
FS_LOCAL, or as a thread if the fstype is FS_FUSE, or the parent function was
called with argument E2_DIRWATCH_YES.
To eliminate BGL-racing, no BGL- or UI-change here

@param data pointer to operation data detailing directory path, filterer etc

@return GList of allocated names of items (maybe NULL) in the directory,
 or pointerised error code if a problem occurred
*/
static gpointer _e2_fs_read_mounted_dir (E2_DRFuncArgs *data)
{
/*	Would be nice to prevent access-time updates purely due to refreshing, but
	the atime really does change as a result of the refresh process, and if we
	were to revert it, the ctime would be changed instead
	Times are immediately shown in the other pane if it's the parent of this one
	If we could suspend monitoring of such parent dir, we must not lose any
	prior reports for there
	QUERY can the changed atime itself trigger a report which causes a subsequent
	refresh, creating an endless cycle?
	Especially with multiple threads and/or processors ?
	Assuming it can do so, we clear the reports 'queue' after the dir is opened
	Would be nice if we could just clear any report(s) due to the refresh per se
	- but the reporting is not that detailed
*/

	/*#ifdef E2_FAM_KERNEL
	if (view->refresh)
		e2_fs_FAM_cancel_monitor_dir (view->dir);
#endif */
/*#if defined (E2_FAM_DNOTIFY)
	//cut down on spurious context changes
	//block DNOTIFY_SIGNAL
	sigprocmask (SIG_BLOCK, &dnotify_signal_set, NULL);
#endif */

	gboolean threaded = data->slowdir;	//monitoring needed, so this func is a thread
	if (threaded)
		e2_utils_block_thread_signals ();	//block all allowed signals to this thread

	DIR *dp = opendir (data->localpath);
	if (dp == NULL)
	{
		printd (WARN, "Unable to open directory: %s", data->localpath);
		return GINT_TO_POINTER (E2DREAD_DNR);
	}

	gint oldtype = 0;
	if (threaded)
	{
		pthread_setcanceltype (PTHREAD_CANCEL_ASYNCHRONOUS, &oldtype);
//		pthread_cleanup_push ((gpointer)closedir, (gpointer)dp); can't be inside braces without corresponding pop
	}

	struct dirent *entryptr;
	//reportedly, some systems do not provide enough name-space
	union
	{
		struct dirent entry;
		gchar b [offsetof (struct dirent, d_name) + NAME_MAX + 1];
	} u;
#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = NULL;	//this func only handles local dirs
#endif
	gboolean (*processor) (VPATH *, const gchar *, GList **, gpointer) = data->callback;
	GList *entries = NULL;
//	if (threaded)
//		pthread_cleanup_push ((gpointer)g_list_free, (gpointer)entries);	//this will leak list data

	errno = 0;
	while (!readdir_r (dp, &u.entry, &entryptr) && entryptr != NULL)
	{
//#ifdef _DIRENT_HAVE_D_RECLEN
		//reportedly, some systems do not terminate the name string
		//BUT this is not liked by the compiler ...
//		*((gchar *) (&u.entry + u.entry.d_reclen)) = '\0';
//maybe this is better ?
//		gpointer s = &u.entry;
//		s += u.entry.d_reclen;
//		*(gchar *)s = '\0';
//#endif
		//one item we're not interested in
		if (u.entry.d_name[0] != '.' || u.entry.d_name[1] != '\0')
		{
			if (processor != NULL)
			{
#ifdef E2_VFS
				ddata.path = data->localpath;
				if (!processor (&ddata, u.entry.d_name, &entries, data->cb_data))
#else
				if (!processor (data->localpath, u.entry.d_name, &entries, data->cb_data))
#endif
				break;
			}
			else
				//order is irrelevant, prepend is faster
				entries = g_list_prepend (entries, g_strdup (u.entry.d_name));
		}
	}
	if (errno == EBADF)
	{
		e2_list_free_with_data (&entries);
		entries = GINT_TO_POINTER (E2DREAD_NS);
	}
	closedir (dp);
//	E2_UNBLOCK

	if (data->free_data_func != NULL)
		data->free_data_func (data->cb_data);

	if (threaded)	//this func is a thread
	{
//		pthread_cleanup_pop (0);	//free entries list
//		pthread_cleanup_pop (0);	//close dp
		pthread_setcanceltype (oldtype, NULL);
	}

	return entries;
}
/**
@brief callback for responses from too-slow-dialog @a dialog
This approach eliminates gtk_main() (which hates being aborted) from the dialog
@param dialog the dialog from which the response was initiated
@param response the response enumerator
@param data task-data specified when the callback was connected
@return
*/
static void _e2_fs_slowread_response_cb (GtkDialog *dialog, gint response,
	E2_DRead *data)
{
	pthread_t ID;
	GtkWidget *wid;
	CLOSEBGL
	switch (response)
	{
		case GTK_RESPONSE_NO:	//abort the operation
			ID = data->aid;
			data->aid = 0;
			if (ID > 0)	//operation still not finished
			{
				pthread_cancel (ID); //shutdown action thread
				printd (DEBUG,"dir read thread aborted by user");
				//after this cancellation, the main thread will cleanup
			}
			break;
		case E2_RESPONSE_USER1:	//no more reminders
//CHECKME pthread_mutex_lock (&?_mutex);
			wid = data->dialog; 	//race-management
			data->dialog = NULL;
			if (GTK_IS_WIDGET (wid))
				gtk_widget_destroy (wid);	//== (GTK_WIDGET(dialog)
//			pthread_mutex_unlock (&?_mutex);
			//if the racy action-thread ends about now, signal to main
			//thread that it doesn't need to abort the monitor thread
			ID = data->mid;
			data->mid = 0;
			if (ID > 0) //race-management
				pthread_cancel (ID);
			break;
		default:
//		case GTK_RESPONSE_YES:	//keep waiting
//			pthread_mutex_lock (&?_mutex);
			wid = data->dialog; //race-management
			data->dialog = NULL;
			if (wid != NULL)
			{
				gtk_widget_hide (wid);
				data->dialog = wid;
//				WAIT_FOR_EVENTS;	//make the hide work
			}
//			pthread_mutex_unlock (&?_mutex);
			break;
	}
	OPENBGL
}
/**
@brief thread function to manage timeout when reading a directory
Assumes BGL is open, and @a data ->dialog is NULLED before 1st use
@param data pointer to data struct with thread id's etc

@return never happens - this must be cancelled by some other thread
*/
static gpointer _e2_fs_progress_monitor (E2_DRead *data)
{
	struct timespec timeout; //data for timeout value for the wait function

	e2_utils_block_thread_signals ();	//block all allowed signals to this thread

	//these are not used externally, but are needed for the wait
	pthread_mutex_t condition_mutex = PTHREAD_MUTEX_INITIALIZER;	//no recurse
	pthread_cond_t wait_cond = PTHREAD_COND_INITIALIZER;

	gint secs = 10;	//initial wait = 10 secs

	while (TRUE)
	{
//		sleep (secs);	//CHECKME more effective to use cond wait with timeout ?
		pthread_mutex_lock (&condition_mutex);
		//clock_gettime (CLOCK_REALTIME, &timeout);
		//timeout.tv_sec += secs;
		//pthread_cond_timedwait (&wait_cond, &condition_mutex, &timeout);
		pthread_cond_timedwait (&wait_cond, &condition_mutex, &timeout);
		pthread_mutex_unlock (&condition_mutex);
		printd (DEBUG,"dir-monitor-thread (ID=%lu) %d-sec sleep ended", (gulong) data->mid, secs);

		gboolean shown = (data->dialog != NULL &&
#ifdef USE_GTK2_18
			gtk_widget_get_visible (data->dialog));
#else
			GTK_WIDGET_VISIBLE (data->dialog));
#endif
		if (data->dialog == NULL) //once-only, create the dialog
		{
			CLOSEBGL
			data->dialog = e2_dialog_slow (_("Reading directory data"),
				_("directory read"),
				(ResponseFunc)_e2_fs_slowread_response_cb, data);
			OPENBGL
		}

		if (data->dialog != NULL &&
#ifdef USE_GTK2_18
			!gtk_widget_get_visible (data->dialog));
#else
			!GTK_WIDGET_VISIBLE (data->dialog))
#endif
		{
			CLOSEBGL
			gtk_widget_show (data->dialog);
			gtk_window_present (GTK_WINDOW (data->dialog));
//			WAIT_FOR_EVENTS;
			OPENBGL
			WAIT_FOR_EVENTS_UNLOCKED_SLOWLY;
			printd (DEBUG,"dir-monitor-thread dialog presented");
		}

		//initially, progressively lengthen interval between dialog popups
		if (!shown //dialog was not shown already when this loop was traversed
				&& secs < 30)
			secs += 10;
	}
	//never get to here
	return NULL;
}
/**
@brief create a list of some or almost all items in the directory @a localpath

Items in the directory can be passed to @a filterfunc to determine whether the
respective item should be listed. Otherwise, all items other than "." are listed.
CWD is temporarily changed to the directory, so threads cannot assume that CWD
always matches curr_view->dir or whatever for non-local dirs.
If the read is lengthy, a downstream dialog will be shown. That locks gdk threads mutex ?
This assumes BGL is open. Any @a filterfunc is called with BGL open

@param localpath absolute path of dir to process, localised string
@param monitor enumerator signalling how to handle threaded dir reading
@param filterfunc function to call for each processed item, or NULL to append each item's name to returned list
@param cb_data pointer to data to send to @a filterfunc
@param free_data_func function to call at end of scan, to clean @a cb_data, or NULL

@return GList of allocated names (maybe NULL) for matching items in @a localpath,
 or pointerised error code if a problem occurred
*/
gpointer e2_fs_dir_foreach (VPATH *localpath, E2_FsReadWatch monitor,
	gpointer filterfunc, gpointer cb_data, GDestroyNotify free_data_func E2_ERR_ARG())
{
	gpointer entries;
#ifdef E2_VFS
	if (e2_fs_item_is_mounted (localpath))
	{
#endif
		gboolean slowdir;
		switch (monitor)
		{
			case E2_DIRWATCH_NO:
				slowdir = FALSE;
				break;
			case E2_DIRWATCH_YES:
				slowdir = TRUE;
				break;
			default:
//			case E2_DIRWATCH_CHECK:
				slowdir = e2_fs_mount_is_fusepoint (localpath);
				break;
		}

		E2_DRFuncArgs args = { VPCSTR (localpath), slowdir, filterfunc, cb_data, free_data_func };
		if (!slowdir)	//no timeout-monitoring needed
			return (_e2_fs_read_mounted_dir (&args));

		//slow (FUSE) dirs use same func as native dirs, but with timeout checking
		//create joinable read thread, not with glib thread funcs, as we need to
		//kill thread(s)
		pthread_t ID;
		if (pthread_create (&ID, NULL,
			(gpointer(*)(gpointer))_e2_fs_read_mounted_dir, &args) == 0)
				printd (DEBUG,"read-dir-thread (ID=%lu) started", ID);
		else
		{
			//FIXME message to user
			printd (WARN,"read-dir-thread create error!");
			printd (DEBUG,"exiting function e2_fs_dir_foreach () with error result");
			return GINT_TO_POINTER (E2DREAD_ENOTH);
		}

		//FIXME use a E2_DRead already produced elsewhere
		//or since this thread joins the others, it can be stacked ...
//		E2_DRead *data = ALLOCATE0 (E2_DRead);
//		CHECKALLOCATEDWARN (data, return (GINT_TO_POINTER (E2DREAD_ENOMEM));)
//		data->aid = ID;

		e2_window_show_status_message (_("Reading directory data")
#ifdef USE_GTK2_20
			, FALSE
#endif
		);
		//make status message show, at session start at least
		WAIT_FOR_EVENTS_UNLOCKED	//_SLOWLY

		//create monitor thread
		//redundant if read-thread has finished already, but then it gets killed
		//immediately, anyhow
		E2_DRead data = { ID, 0, NULL };

		pthread_attr_t attr;
		pthread_attr_init (&attr);
		pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);

		if (pthread_create (&data.mid, &attr,
			(gpointer(*)(gpointer))_e2_fs_progress_monitor, &data) == 0)
				printd (DEBUG,"dir-monitor-thread (ID=%lu) started", data.mid);
		else
		{
			//FIXME message to user with BGL management
			printd (WARN,"read-dir-thread-create error!");
		}
		//block until read is finished or cancelled
		printd (DEBUG,"main thread blocking until read ends or times out");
		pthread_join (ID, &entries);

		pthread_attr_destroy (&attr);

		e2_window_clear_status_message ();	//no BGL management neeeded
		EXTRA_WAIT_FOR_EVENTS_UNLOCKED;
		//some race-minimisation here
		GtkWidget *wid = data.dialog;
		data.dialog = NULL;
		if (GTK_IS_WIDGET (wid))
		{
			CLOSEBGL
			gtk_widget_destroy (wid);
			OPENBGL
		}
		ID = data.mid;
		data.mid = 0;
		if (ID > 0)
			pthread_cancel (ID);
		printd (DEBUG,"exiting function e2_fs_dir_foreach () with read result");
//		DEALLOCATE (E2_DRead, data);
		printd (DEBUG,"dir read data cleaned up in main thread");
		if (entries == PTHREAD_CANCELED)
			entries = GINT_TO_POINTER (E2DREAD_DNF);
#ifdef E2_VFS
	}
	else	//read vdir
	{
		entries = GINT_TO_POINTER (E2DREAD_DNR);
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (readdir), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need fof local error here
				entries = opdata.iface->readdir (localpath, filterfunc,
						cb_data, free_data_func, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
#endif
	return entries;
}

/**
@brief get the target of symbolic link represented by @a localpath
The string in targetbuf will be 0-terminated and stripped of irrelevant leading
relativities
@param localpath descriptor for the symlink, with localised path
@param targetbuf buffer into which the result can be copied
@param targetsize the length of @a targetbuf

@return the length of the string in @a targetbuf, or < 0 upon error
*/
gint e2_fs_readlink (VPATH *localpath, gchar *targetbuf, size_t targetsize E2_ERR_ARG())
{
	gint len;
	const gchar *t;
#ifdef E2_VFS
	VPATH ddata;
#endif

#ifdef E2_VFSTMP
	if (e2_fs_item_is_mounted (localpath))
	{
#endif
		len = readlink (VPCSTR(localpath), targetbuf, targetsize);
		if (len < 0)
		{
			*targetbuf = '\0';
			return len;
		}
//		else if (len == targetsize)
//			FIXME can't tell if target is longer
#ifdef E2_VFSTMP
	}
	else	//need to do virtual
	{
		FIXME vfs version
		if (len < 0)
		{
			e2_fs_set_error_from_errno (E2_ERR_NAME);
			*targetbuf = '\0';
			return len;
		}
	}
#endif
	if (len == targetsize)
		len--;
	targetbuf[len] = '\0';

#ifdef E2_VFS
	ddata.path = targetbuf;
	ddata.spacedata = localpath->spacedata;
	t = e2_utils_skip_relative_path (&ddata);
#else
	t = e2_utils_skip_relative_path (targetbuf);
#endif
	if (t > targetbuf)
	{
		memmove (targetbuf, t, len + targetbuf - t + 1);
		len -= (t-targetbuf);
	}

	return len;
}

/**
@brief get real path for @a local_path if it's a link

@param local_path store for pointer to freeable, absolute, localised, path string which may be a link name

@return TRUE if a newly-allocated replacement path is provided in @a local_path
*/
gboolean e2_fs_walk_link (gchar **local_path E2_ERR_ARG())
{
//CHECKME use e2_utils_skip_relative_path (VPATH *path); here ?
	struct stat sb;
#ifdef E2_VFSTMP
	FIXME get proper spacedata
#endif
#ifdef E2_VFS
	VPATH data = { *local_path, NULL };
	if (!e2_fs_lstat (&data, &sb E2_ERR_SAMEARG()) && S_ISLNK (sb.st_mode))
#else
	if (!e2_fs_lstat (*local_path, &sb E2_ERR_SAMEARG()) && S_ISLNK (sb.st_mode))
#endif
	{
#ifdef E2_VFS
		if (e2_fs_item_is_mounted ((&data)))
		{
#endif

#ifdef __USE_GNU
			gchar *resolved_path = canonicalize_file_name (*local_path);
			if (resolved_path != NULL)
			{
				g_free (*local_path);
				*local_path = resolved_path;
				return TRUE;
			}
#else
			gchar resolved_path [PATH_MAX+1];
			if (realpath (*local_path, resolved_path) != NULL)
			{
				*(resolved_path + PATH_MAX) = '\0'; //ensure an end
				g_free (*local_path);
				*local_path = g_strdup (resolved_path);
				return TRUE;
			}
#endif
#ifdef E2_VFS
		}
		else	//item is virtual
		{
			E2_ERR_PCLEAR
# ifdef E2_VFSTMP
			//FIXME vfs if relevant, get information about virtual item, how ??
# endif
			return FALSE;
		}
#endif
	}
	return FALSE;
}
/**
@brief stat item @a localpath, put results into @a buf

This looks through links, but does not fail for hanging links (not sure about
failure for virtual items)

@param localpath relative or absolute path of item to stat, localised string
@param buf pointer to buffer to hold the results

@return 0 (FALSE) if the stat succeeded, else -1 (TRUE)
*/
gint e2_fs_stat (VPATH *localpath, struct stat *buf E2_ERR_ARG())
{
#ifdef E2_VFS
# ifdef E2_VFSTMP
	//what about incomplete mount of fuse-dir
# endif
	if (e2_fs_item_is_mounted (localpath))
	{
#endif
		if (stat (VPCSTR(localpath), buf))
		{	//stat failed, but it could be just due to a hanging symlink
			struct stat statbuf;	//don't disturb the caller's buffer
			if (!lstat (VPCSTR(localpath), &statbuf)	//stat link succeeded
				//CHECKME more checks for hanging link?
				&& (S_ISLNK (statbuf.st_mode) && errno == ENOENT))
			{
				*buf = statbuf;
				return 0;
			}
			return -1;
		}
		return 0;
#ifdef E2_VFS
	}
	else	//item is virtual
	{
		gint result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (stat), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->stat (localpath, buf, &opdata);
				vfs.finish_operation (&opdata);	//also handles cleanups when iface func is N/A
			}
		}
		return result;
	}
#endif
}

#ifdef E2_VFS
/******** REPLACEMENTS FOR STANDARD FS FUNCTIONS *********/

gint e2_fs_lstat (VPATH *localpath, struct stat *buf, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = lstat (VPCSTR(localpath), buf);
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//need to do virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (lstat), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->lstat (localpath, buf, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_access (VPATH *localpath, gint mode, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = access (VPCSTR(localpath), mode);
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//need to do virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (access), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->access (localpath, mode, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

//CHECKME any need for this sort of stream for non-local files ?
E2_FILE *e2_fs_file_open (VPATH *localpath, const gchar *how, GError **E2_ERR_NAME)
{
	E2_FILE *result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = e2_fs_open_stream (VPCSTR(localpath), how);
		if (result != NULL)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
		return result;
	}
	else	//need to do virtual
	{
#ifdef E2_VFSTMP
		//FIXME get virtual stream
#endif
		return NULL;
	}
}

gint e2_fs_symlink (VPATH *target, VPATH *name, GError **E2_ERR_NAME)
{
	gint result;
	//links never possible between namespaces
	if (target->spacedata != name->spacedata)
	{
		e2_fs_set_error_custom (E2_ERR_NAME, 126,
			"%s and %s are not in the same namespace",
			VPSTR(target), VPSTR(name), NULL);
		return -1;
	}

	if (e2_fs_item_is_mounted (target))	//&& e2_fs_item_is_mounted (name))
	{
		result = symlink (VPCSTR(target), VPCSTR(name));	//CHECKME relative links ok ?
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//need to do virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (target->spacedata, IFACE_OFFSET (symlink), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->symlink (target, name, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_unlink (VPATH *localpath, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = unlink (VPCSTR(localpath));
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//need to do virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (delete), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->delete (localpath, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_remove (VPATH *localpath, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = remove (VPCSTR(localpath));
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (delete), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->delete (localpath, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_rmdir (VPATH *localpath, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = rmdir (VPCSTR(localpath));
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//need to do virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (delete), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->delete (localpath, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_mkdir (VPATH *localpath, mode_t mode, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = mkdir (VPCSTR(localpath), mode);
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//need to do virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (mkdir), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->mkdir (localpath, mode, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_chdir_local (VPATH *localpath, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = chdir (VPCSTR(localpath));
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
		return result;
	}
	else	//need to do virtual
	{
#ifdef E2_VFSTMP
		//CHECKME never relevant to do virtual change ??
#endif
		return -1;
	}
}

gint e2_fs_rename (VPATH *oldpath, VPATH *newpath, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (oldpath) && e2_fs_item_is_mounted (newpath))
	{
		result = rename (VPCSTR(oldpath), VPCSTR(newpath));
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//need to do virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (oldpath->spacedata, IFACE_OFFSET (rename), &opdata))	//CHECKME newpath too ??
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->rename (oldpath, newpath, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_chmod (VPATH *localpath, mode_t mode, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = chmod (VPCSTR(localpath), mode);
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (chmod), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->chmod (localpath, mode, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_chown (VPATH *localpath, uid_t owner, gid_t group, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = chown (VPCSTR(localpath), owner, group);
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//need to do virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (chown), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->chown (localpath, owner, group, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_lchown (VPATH *localpath, uid_t owner, gid_t group, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = lchown (VPCSTR(localpath), owner, group);
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else	//need to do virtual
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (lchown), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->lchown (localpath, owner, group, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

gint e2_fs_utime (VPATH *localpath, const struct utimbuf *buf, GError **E2_ERR_NAME)
{
	gint result;
	if (e2_fs_item_is_mounted (localpath))
	{
		result = utime (VPCSTR(localpath), buf);
		if (result != 0)
			e2_fs_set_error_from_errno (E2_ERR_NAME);
	}
	else
	{
		result = -1;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
//E2_VFSTMP hanle NULL buf
			E2_VFSMonitor opdata;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
			if (vfs.start_operation (localpath->spacedata, IFACE_OFFSET (touch), &opdata))
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				result = opdata.iface->touch (localpath, buf, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
	return result;
}

#endif //def E2_VFS

/**
@brief change time(s) of item @a localpath to current system time

@param localpath localised string with absolute path of item to touch

@return TRUE if the operation succeeded
*/
gboolean e2_fs_touchnow (VPATH *localpath E2_ERR_ARG())
{
	return (e2_fs_utime (localpath, NULL E2_ERR_SAMEARG()) == 0);
}
/**
@brief update m/a/c times of config dir

This should be called after writing config/cache files, to let any other
instance know that it should re-read those files (if that option is in force).
Error message expects BGL to be closed

@return
*/
void e2_fs_touch_config_dir (void)
{
#ifdef E2_FAM
	if (app.monitor_type != E2_MONITOR_DEFAULT)
		return;
#endif
	struct stat stat_buf;
	E2_ERR_DECLARE

	gchar *local = F_FILENAME_TO_LOCALE (e2_cl_options.config_dir);
#ifdef E2_VFS
	VPATH data = { local, NULL };	//only local config data
	if (!e2_fs_utime (&data, NULL E2_ERR_PTR()))
#else
	if (!e2_fs_utime (local, NULL E2_ERR_PTR()))
#endif
	{
#ifdef E2_VFS
		if (!e2_fs_stat (&data, &stat_buf E2_ERR_PTR()))
#else
		if (!e2_fs_stat (local, &stat_buf E2_ERR_PTR()))
#endif
			app.config_mtime = stat_buf.st_mtime;
		else
		{
			printd (WARN, "couldn't stat the config dir");
#ifdef E2_VFS
			e2_output_print_error (E2_ERR_NAME->message, FALSE);
#else
			e2_output_print_strerrno ();
#endif
			E2_ERR_CLEAR
		}
	}
	else
	{
		printd (WARN, "couldn't touch the config dir");
#ifdef E2_VFS
		e2_output_print_error (E2_ERR_NAME->message, FALSE);
#else
		e2_output_print_strerrno ();
#endif
		E2_ERR_CLEAR
	}
	F_FREE (local, e2_cl_options.config_dir);
}
/**
@brief tolerantly open file @a localpath and return its descriptor
File descriptors are only relevant for local files, so no vfs treatment
is needed
@param localpath absolute path of file to open, localised string
@param openflags bitflags to provide to open()
@param mode mode used by open() when creating a file, -1 to get and use default

@return descriptor of opened file, -1 on error
*/
gint e2_fs_safeopen (const gchar *localpath, gint openflags, mode_t mode)
{
	gint res;
	if (mode == (mode_t)-1)
	{
		mode = umask (0);
		umask (mode);
		mode = ~mode & ALLPERMS;
	}
	do res = open (localpath, openflags, mode);
		while (res == -1 && errno == EINTR);
	return res;
}
/**
@brief open file @a filepath for writing
Error message expects BGL to be closed
@param utfpath path of file to open, utf8 string

@return pointer to data struct for opened file, or NULL if the open failed
*/
E2_FILE *e2_fs_open_writestream (const gchar *localpath E2_ERR_ARG())
{
	E2_FILE *f;
#ifdef E2_VFSTMP
	FIXME get proper spacedata if this is to be used for non-local files
#endif
#ifdef E2_VFS
	VPATH ddata = { localpath, NULL };
	if (e2_fs_item_is_mounted ((&ddata)))
	{
#endif
		f = e2_fs_open_stream (localpath, "w");

		if (f == NULL)
		{
			gchar *utf = F_FILENAME_FROM_LOCALE (localpath);
			gchar *msg = g_strdup_printf (_("Cannot open '%s' for writing - %s"),
				utf, g_strerror (errno));
			e2_output_print_error (msg, TRUE);
			F_FREE (utf, localpath);
		}
#ifdef E2_VFS
	}
	else	//non-native open
	{
# ifdef E2_VFSTMP
		//FIXME
		f = NULL;
# endif
	}
#endif
	return f;
}
/**
@brief write @a string to local file @a localpath
Error message expects BGL to be closed
@param stream handle for file
@param string NULL-terminated string to be written
@param localpath file to be written, localised string, for error reporting

@return EOF if a write error occurs, otherwise a non-negative value
*/
gint e2_fs_put_stream (E2_FILE *stream, const gchar *string, gchar *localpath
	E2_ERR_ARG())
{
	gint retval;
#ifdef E2_VFSTMP
	FIXME get proper spacedata if virtual streaming is supported
	localpath is sometimes nominal, not a usable path
	VPATH data = { localpath, NULL };	//local space
	if (e2_fs_item_is_mounted ((&data)))
	{
#endif
		retval = fputs (string, stream);	//local only
		if (retval == EOF)
		{
#ifdef E2_VFS
			E2_ERR_BACKUP (localerr);
			e2_fs_set_error_from_errno (E2_ERR_NAME);	//apply relevant error data
#endif
			gchar *utf = F_FILENAME_FROM_LOCALE (localpath);
			gchar *msg = g_strdup_printf (_("Error writing file '%s' - %s"),
				utf
#ifdef E2_VFS
				E2_ERR_MSGC()
#else
				, g_strerror (errno)
#endif
				);
			e2_output_print_error (msg, TRUE);
			F_FREE (utf, localpath);
			E2_ERR_CLEARBACKUP (localerr);
		}
#ifdef E2_VFSTMP
	}
	else	//non-native write
	{
		//FIXME
		retval = EOF;
	}
#endif
	return retval;
}
/**
@brief read from file into buffer

@param descriptor file descriptor for the file to read
@param buffer start of memory to hold the read data
@param bufsize size of @a buffer

@return no. of bytes read, or -1 on error
*/
ssize_t e2_fs_read (gint descriptor, gpointer buffer, /*size_t*/ gulong bufsize E2_ERR_ARG())
{
#ifdef E2_VFSTMP
	no version of this available yet
#endif
/*	ssize_t nread;
	do nread = read (descriptor, buffer, bufsize);
		while (nread < 0 && E2_ERR_IS (EINTR));
	return nread; */

	ssize_t nread = -1;	//error if buffer size is 0
	size_t space = (size_t) bufsize;
	size_t total = 0;
	gpointer target = buffer;
	while (space > 0)
	{
		nread = read (descriptor, target, space);
		if (nread > 0)
		{
			total += nread;
			target += nread;	//irrelevant if we're finished now
			space -= nread;
		}
		else if (nread == 0	//EOF, no error
				||			//nread < 0
				errno != EINTR) //un-recoverable error
			break;
	}
	if (nread < 0)
		return -1;
	return (ssize_t)total;
}
/**
@brief read file @a localpath into memory
Note the file needs to be size-limited, for local files at least, to what can be
represented by a gulong, i.e. to ULONG_MAX. This is to allow allocation by g_...,
to simplify later cleanups (not supposed to mix glib and glibc mamaory-management)
Memory is allocated by g_try_malloc, not malloc, and needs to be cleared by g_free, not free
Error messages expect BGL open/off
@param localpath path of file to be read, localised string
@param contents store for ptr start of the allocated contents
@param contlength store for no. of bytes written, or NULL if not interested
@param terminate TRUE to add a trailing 0 if that's not present in file as read

@return TRUE if @a localpath was read successfully
*/
gboolean e2_fs_get_file_contents (VPATH *localpath, gpointer *contents,
	/*size_t*/gulong *contlength, gboolean terminate E2_ERR_ARG())
{
	gboolean retval;
	const gchar *fmt;
#ifdef E2_VFS
	if (e2_fs_item_is_mounted (localpath))
	{
#endif
		gint fdesc = e2_fs_safeopen (VPCSTR(localpath), O_RDONLY, 0);
		if (fdesc >= 0)
		{
			struct stat sb;
			if (!e2_fs_fstat (fdesc, &sb))
			{
				size_t nread, len = sb.st_size;
				if (terminate)
					len += sizeof (gchar);	//in case we need to append a 0

//				*contents = malloc (len);	//not size-limited, but imposes cleanup hassles
				*contents = g_try_malloc (len); //allows up to ULONG_MAX
#if (CHECKALLOCATEDWARNT)
				CHECKALLOCATEDWARNT (*contents, e2_fs_safeclose (fdesc); return FALSE;);
#else
				if (*contents == NULL)
				{
					CLOSEBGL
					e2_utils_show_memory_message ();
					OPENBGL
# ifdef E2_VFSTMP
//					*E2_ERR_NAME = g_error_new_literal (G_FILE_ERROR, errno, g_strerror (errno));
//					E2_ERR_CLEARBACKUP (localerr);
# endif
					e2_fs_safeclose (fdesc);	//bypass the normal cleanup process
					return FALSE;
				}
#endif

				retval = TRUE;
				fmt = NULL;
				if (sb.st_size > 0)
				{
					nread = e2_fs_read (fdesc, *contents, sb.st_size E2_ERR_SAMEARG());
					if (nread < 0 //|| nread < (gulong)sb.st_size
					   )
					{
						g_free (*contents);	//or free() if needed
						*contents = NULL;
						nread = 0;
						retval = FALSE;
						fmt = _("Error reading file %s");
					}
					else if (terminate)
					{
						gchar *s;
						if (nread >= sizeof (gchar))
						{
							s = *contents + nread - sizeof (gchar);
							if (*s == '\0')
								nread -= sizeof (gchar);	//OK ?
							else
							{
								s += sizeof (gchar);
								*s = '\0';
							}
						}
						else	//nothing read
						{
							s = *contents;
							*s = '\0';
						}
					}
				}
				else
				{
					nread = 0;
					if (terminate)
						*contents = '\0';
				}
				if (contlength != NULL)
					*contlength = nread;
			}
			else	//stat failed
			{
				retval = FALSE;
				fmt = _("Cannot get information about %s");
			}
			e2_fs_safeclose (fdesc);
		}
		else	//open failed
		{
			retval = FALSE;
			fmt = _("Cannot open file %s");
		}
#ifdef E2_VFS
	}
	else	//item is virtual
	{
# ifdef E2_VFSTMP
		//get information from virtual item
# endif
		retval = FALSE;
		fmt = _("Error reading file %s");	//FIXME
	}
#endif
	if (!retval
		&& app.output.opt_show_on_new != NULL)	//at session-start, when reading config
												//etc, we can't yet show any error
	{
		E2_ERR_BACKUP (localerr);
#ifdef E2_VFS
		e2_fs_set_error_from_errno (E2_ERR_NAME);
#endif
		e2_fs_error_local (fmt, localpath E2_ERR_MSGC());
		E2_ERR_CLEARBACKUP (localerr);
	}
	return retval;
}

/* *
@brief read the start of file @a localpath into memory
Error messages expect BGL open/off
@param localpath path of file to be read, localised string
@param contents store for ptr to allocated buffer
@param bufsize length of buffer, max. amount of the file to be read
@param terminate TRUE to add a trailing 0 if that's not present in file as read

@return TRUE if @a localpath was read successfully
*/
/*gboolean e2_fs_get_file_start (VPATH *localpath, gpointer *contents,
	/ *size_t* /gulong bufsize, gboolean terminate E2_ERR_ARG())
{
#ifdef USE_GLIB2_10
	*contents = g_slice_alloc (bufsize);
#else
	*contents = g_try_malloc (bufsize);
#endif
#if (CHECKALLOCATEDWARNT)
	CHECKALLOCATEDWARNT (*contents, return FALSE;);
#else
	if (*contents == NULL)
		return FALSE;
#endif

#ifdef VFSTMP
	TODO read start of non-local file
#else
	gint fdesc = e2_fs_safeopen (VPCSTR(localpath), O_RDONLY, 0);
	if (fdesc >= 0)
	{
		ssize_t length = e2_fs_read (fdesc, *contents, bufsize E2_ERR_PTR());
		e2_fs_safeclose (fdesc);
		if (length >= 0)
		{
			if (terminate)
			{
				if (length < bufsize)
					*((gchar*)*contents + length) = '\0';
				else
					*((gchar *)*contents + bufsize - 1) = '\0';
			}
			return TRUE;
		}
	}
#endif

#ifdef USE_GLIB2_10
	g_slice_free1 (bufsize, *contents);
#else
	g_free (*contents);
#endif
	*contents = NULL;
	return FALSE;
}
*/
/**
@brief write from buffer to file

@param descriptor file descriptor of file to write
@param buffer start of memory holding the write data
@param bufsize size of @a buffer

@return no. of bytes written, < 0 on error
*/
ssize_t e2_fs_write (gint descriptor, gpointer buffer, /*size_t*/ gulong bufsize E2_ERR_ARG())
{
#ifdef E2_VFSTMP
	//no version of this available yet
#endif
	ssize_t written;
	if (bufsize == 0)
		return 0;
	do written = write (descriptor, buffer, bufsize);
		while ((written < bufsize && written >= 0) || errno == EINTR);
	return written;
}
/**
@brief write whole file @a localpath, from memory starting at @a contents
No overwrite checking is done, no data syncing is done
@param localpath path of file to be write, localised string
@param contents ptr to the start of the data to save
@param contlength byte-length of data to save
@param mode permissions to apply, or -1 for default

@return TRUE if @a locapath was written successfully
*/
gboolean e2_fs_set_file_contents (VPATH *localpath, gpointer contents,
	size_t contlength, mode_t mode E2_ERR_ARG())
{
	gboolean retval;
#ifdef E2_VFS
	if (e2_fs_item_is_mounted (localpath))
	{
#endif
		retval = TRUE;
		const gchar *fmt = NULL;
		gint fdesc = e2_fs_safeopen (VPCSTR(localpath), O_WRONLY | O_CREAT, mode);
		if (fdesc < 0)
		{
			retval = FALSE;
			fmt = _("Cannot create file %s");
		}

		if (retval)
		{
			ssize_t written = 0;
			while (written < contlength)
			{
				written = TEMP_FAILURE_RETRY (write (fdesc, contents, contlength));
				if (written < 0)
				{
					retval = FALSE;
					fmt = _("Error writing file %s");
					break;
				}
			}
			if (retval)
			{
				//CHECKME e2_fs_writeflush (fdesc);
				TEMP_FAILURE_RETRY (ftruncate (fdesc, written));
			}
			e2_fs_safeclose (fdesc);
		}

		if (!retval)
		{
//			unlink (VPCSTR(localpath));
			E2_ERR_BACKUP (localerr);
#ifdef E2_VFS
			e2_fs_set_error_from_errno (E2_ERR_NAME);
#endif
			e2_fs_error_local (fmt, localpath E2_ERR_MSGC());
			E2_ERR_CLEARBACKUP (localerr);
			unlink (VPCSTR(localpath));
		}
#ifdef E2_VFS
	}
	else	//item is virtual
	{
# ifdef E2_VFSTMP
		//send information to virtual item
# endif
		retval = FALSE;
	}
#endif
	return retval;
}
/**
@brief tolerantly close file descriptor
File descriptors are only relevant for local files, so no vfs treatment
is needed
@param file_desc file descriptor

@return 0 on success, -1 on error
*/
gint e2_fs_safeclose (gint file_desc)
{
	gint res;
	do res = (gint) close (file_desc);
		while (res == -1 && errno == EINTR);
	return res;
}
/**
@brief flush file contents to storage
File descriptors are only relevant for local files, so no vfs treatment
is needed
@param file_desc file descriptor

@return 0 on success, -1 on error
*/
gint e2_fs_writeflush (gint file_desc)
{
	gint res;
	errno = 0;
#ifdef __E2BSD__
	res = fsync (file_desc);
#else
	res = fdatasync (file_desc);
#endif
	if (res == -1 && errno == EINVAL)
		res = 0;
	return res;
}
/**
@brief perform a native, blockwise, file copy
Error messages assume BGL is open
@param vsrc localised string, absolute path of item to copy
@param src_sb pointer to valid struct stat for @a src
@param vdest localised string, absolute path of copy destination

@return TRUE if the copy was completed successfully
*/
gboolean e2_fs_copy_file (VPATH *src, const struct stat *src_sb,
	VPATH *dest E2_ERR_ARG())
{
	gboolean retval;
#ifdef E2_VFS
	if (e2_fs_item_is_mounted (src) && e2_fs_item_is_mounted (dest))
	{
#endif
//		struct stat src_sb;
		struct stat dest_sb;
		E2_ERR_BACKUP (localerr);
		//CHECKME vfs e2_fs_open|close ever needed for closing while native copying ?

		//O_NOATIME is GNU-specific, != 'cp', and requires write-permission on item
		//O_NOFOLLOW causes spurious error

		gint src_desc = e2_fs_safeopen (VPCSTR(src), O_RDONLY, 0);
		if (src_desc < 0)
		{
#ifdef E2_VFS
			e2_fs_set_error_from_errno (E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Cannot open '%s' for reading"),
				src E2_ERR_MSGC());
			E2_ERR_CLEARBACKUP (localerr);
			return FALSE;
		}
		mode_t dest_mode = (src_sb->st_mode | S_IWUSR) & ALLPERMS;

		gint dest_desc = e2_fs_safeopen (VPCSTR(dest),
			O_WRONLY | O_CREAT | O_EXCL, dest_mode);
		if (dest_desc < 0)
		{
#ifdef E2_VFS
			e2_fs_set_error_from_errno (E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Cannot create file %s"),
				dest E2_ERR_MSGC());
			E2_ERR_CLEARBACKUP (localerr);
			TEMP_FAILURE_RETRY (close (src_desc));
			return FALSE;
		}
		if (e2_fs_fstat (dest_desc, &dest_sb))
		{
#ifdef E2_VFS
			e2_fs_set_error_from_errno (E2_ERR_NAME);
#endif
			e2_fs_error_local (_("Cannot get information about %s"),
				dest E2_ERR_MSGC());
			E2_ERR_CLEARBACKUP (localerr);
			TEMP_FAILURE_RETRY (close (src_desc));
			TEMP_FAILURE_RETRY (close (dest_desc));
			return FALSE;
		}
	//	use blocks the same size as dest block (like cp)
	//		blksize_t buf_size = dest_sb.st_blksize;  //or src ??
	//		gchar *buf = g_alloca (buf_size);
		//find a buffer size up to 1 MB (!= the 'cp' approach)
	//		blksize_t buf_size = 1048576;
		//find a buffer up to 16 times dest block
		//(compromise between accesses and multi-tasking latency)
		blksize_t buf_size = dest_sb.st_blksize * 16;
		size_t src_size = src_sb->st_size * 2;
		while (buf_size > src_size && buf_size > dest_sb.st_blksize)
			buf_size /= 2;
		gpointer buf;
#ifdef USE_GLIB2_10
		while ((buf = g_slice_alloc (buf_size)) == NULL)
#else
		while ((buf = g_try_malloc (buf_size)) == NULL)
#endif
		{
			if (buf_size < dest_sb.st_blksize)
			{
				CLOSEBGL
				e2_utils_show_memory_message ();
				OPENBGL
				TEMP_FAILURE_RETRY (close (src_desc));
				TEMP_FAILURE_RETRY (close (dest_desc));
				return FALSE;
			}
			buf_size /= 2;
		}
		//CHECKME suspend access-monitoring here ...
		retval = TRUE;
		while (1)
		{
			ssize_t n_read = TEMP_FAILURE_RETRY (read (src_desc, buf, buf_size));	//(potential cancellation point, LEAKS)
			if (n_read == 0)
				break;

			if (n_read < 0)
			{
#ifdef E2_VFS
				e2_fs_set_error_from_errno (E2_ERR_NAME);
#endif
				e2_fs_error_local (_("Error reading file %s"),
					src E2_ERR_MSGC());
				retval = FALSE;
				break;
			}

			ssize_t n_write = 0;
			while (n_write < n_read)
			{
				n_write = TEMP_FAILURE_RETRY (write (dest_desc, buf, n_read));	//(potential cancellation point, LEAKS)
				if (n_write < 0)
				{
#ifdef E2_VFS
					e2_fs_set_error_from_errno (E2_ERR_NAME);
#endif
					e2_fs_error_local (_("Error writing file %s"),
						dest E2_ERR_MSGC());
					retval = FALSE;
					break;
				}
			}
			if (!retval)
				break;
//			pthread_testcancel ();	//swap threads, cancel if instructed (leaks if so)
		}
		TEMP_FAILURE_RETRY (close (src_desc));
		TEMP_FAILURE_RETRY (close (dest_desc));
		if (!retval)
		{
			E2_ERR_CLEARBACKUP (localerr);
			unlink (VPCSTR(dest));
		}
#ifdef USE_GLIB2_10
		g_slice_free1 (buf_size, buf);
#else
		g_free (buf);
#endif
#ifdef E2_VFS
	}
	else	//one or both of src/dest is non-native
	{
		retval = FALSE;
		if (e2_fs_vfsfunc_ready ((gpointer *)&vfs.start_operation))
		{
			E2_VFSMonitor opdata;
			gboolean with_progress = FALSE;	//FIXME API
			opdata.flags = E2VFS_TIMER | E2VFS_CANCEL;
//E2_VFSTMP if progress is shown, set opdata message and title and flag
			if (vfs.start_operation (src->spacedata, IFACE_OFFSET(copy), &opdata))	//CHECKME dest may be different
			{
				opdata.error = E2_ERR_NAME;	//no need for local error here
				retval = opdata.iface->copy (src, dest, with_progress, &opdata);
				vfs.finish_operation (&opdata);
			}
		}
	}
#endif
	return retval;
}
/**
@brief open pipe to read output from @a command
Error message expects BGL to be closed
@param command localised command string

@return pointer to opened pipe, or NULL if open failed
*/
E2_FILE *e2_fs_open_pipe (gchar *command)
{
	E2_FILE *pipe;
	if ((pipe = popen (command, "r")) == NULL)	//no vfs command-execution for piping
	{
		gchar *s, *utf;
		if ((s = e2_utils_find_whitespace (command)) != NULL)
			*s = '\0';
		utf = e2_utf8_from_locale (command);
		s = g_strdup_printf (_("Cannot open pipe for command '%s'"), utf);
		e2_output_print_error (s, TRUE);
		g_free (utf);
	}
	return pipe;
}
/**
@brief read piped command output into memory
A terminating 0 is always added
Error message expects BGL to be closed
@param command localised command string to be executed
@param output store for ptr to newly-allocated command output string

@return TRUE if @a command result was read successfully and byte-length of output > 0
*/
gboolean e2_fs_get_command_output (gchar *command, gpointer *output)
{
	E2_FILE *pipe = e2_fs_open_pipe (command);
	if (pipe == NULL)
		return FALSE;
	gchar *msg = g_strdup_printf (
			_("Command %s failed: not enough memory"), command);
	size_t total = 0;
	size_t bsize = 4096;	//block size
	gpointer buf = g_try_malloc ((gulong) bsize);
	if (buf == NULL)
	{
		e2_output_print_error (msg, TRUE);
		e2_fs_pipe_close (pipe);
		return FALSE;
	}
	gpointer store = buf;
	ssize_t bytes_read;
	while ((bytes_read = fread (store, 1, bsize, pipe)) > 0)
	{
		if (bytes_read == bsize)
		{
			total += bsize;
			buf = g_try_realloc (buf, total+bsize);
			if (buf == NULL)
			{
				e2_output_print_error (msg, TRUE);
				e2_fs_pipe_close (pipe);
				return FALSE;
			}
			store = buf + total;
		}
		else
		{
			*((gchar *)store+bytes_read) = '\0';
			bytes_read++;
			total += bytes_read;
			buf = g_try_realloc (buf, total);
			if (buf == NULL)
			{
				e2_output_print_error (msg, TRUE);
				e2_fs_pipe_close (pipe);
				return FALSE;
			}
			break;
		}
	}

	g_free (msg);
	e2_fs_pipe_close (pipe);
	if (total == 0)
		g_free (buf);
	else
	{
		store = g_try_realloc (buf, total);	//maybe we can make it smaller
		if (store != NULL)
			buf = store;
		*output = buf;
	}
	return (total > 0);
}
/* *
@brief blockwize read from stdin into memory
A terminating 0 is always added
Error message expects BGL to be closed
@param contents ptr to set to where the input is stored

@return TRUE if stdin was read successfully
*/
/*UNUSED
gboolean e2_fs_read_stdin (gchar **contents)
{
	fd_set fds;
	FD_ZERO (&fds);
	FD_SET (STDIN_FILENO, &fds);

	struct timeval wait;
	wait.tv_sec = 0;
	wait.tv_usec = 100000;

	if (select (STDIN_FILENO+1, &fds, NULL, NULL, &wait) != 1)
		return FALSE;

	size_t total = 0;
	size_t bsize = 1024;	//block size
	gchar *buf = (gchar *) g_try_malloc ((gulong) bsize);
#if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (buf, return FALSE;);
#else
	if (buf == NULL)
		return FALSE;
#endif
	gchar *store = buf;
	ssize_t bytes_read;
	while (feof (stdin) == 0)
	{
		bytes_read = fread (buf, 1, bsize, stdin);
		if (ferror (stdin) != 0)
		{
			g_free (buf);
			return FALSE;	//error msg ??
		}
		if (bytes_read == bsize)
		{
			total += bsize;
			buf = g_try_realloc (buf, total+bsize);
			if (buf == NULL)
			{
				e2_output_print_error (_("Failed stdin read: not enough memory"), FALSE);
				e2_fs_close_stream (stdin);
				return FALSE;
			}
			store = buf + total;
		}
		else
		{
			*(store+bytes_read) = '\0';
			bytes_read++;
			buf = g_try_realloc (buf, total + bytes_read);
			if (buf == NULL)
			{
				e2_fs_close_stream (stdin);
				return FALSE;
			}
			break;
		}
	}
	e2_fs_close_stream (stdin);
	*contents = buf;
	return TRUE;
} */
/* *
@brief write a sequence of bytes from memory to stdout

@param contents ptr to 0-terminated sequence of bytes to be written

@return TRUE if @a contents was sent successfully
*/
/*UNUSED
gboolean e2_fs_write_stdout (gchar *contents)
{
	fd_set fds;
	FD_ZERO (&fds);
	FD_SET (STDOUT_FILENO, &fds);

	struct timeval wait;
	wait.tv_sec = 0;
	wait.tv_usec = 100000;

	if (select (STDOUT_FILENO+1, NULL, &fds, NULL, &wait) != 1)
		return FALSE;

	return (e2_fs_put_stream (stdout, contents, "stdout" E2_ERR_NONE()) != EOF);
} */

static gunichar realperms[10];
static gunichar ftypes[6];
static gunichar fbits[4];

/**
@brief convert file mode_t to a readable permissions string
Get string representing the permissions in @a mode. The order of displayed
permissions is the same as the corresponding bitflags in @a mode, typically
rwx for each of owner, group, other, in turn
@param mode mode_t property to be parsed

@return newly-allocated translated utf8 permissions string
*/
gchar *e2_fs_get_perm_string (mode_t mode)
{
	//inspiration here from David Jensen
	gunichar thisperms[11];

	//once-only, get a local copy of processed strings, to eliminate repeated [de]allocations
	if (realperms[0] == 0)
	{
		//NOTE for translators - this string has single-letters indicating
		// read write execute permissions. Their order is significant
		gunichar *temp = g_utf8_to_ucs4_fast (_("-rwxrwxrwx"), -1, NULL);
		memcpy (realperms, temp, sizeof (realperms));
		g_free (temp);
		//NOTE for translators - this string has single-letters indicating
		// link, dir, block, char, fifo, socket filetypes. Their order is significant
		temp = g_utf8_to_ucs4_fast (_("ldbcfs"), -1, NULL);
		memcpy (ftypes, temp, sizeof (ftypes));
		g_free (temp);
		//NOTE for translators - this string has single-letters indicating
		// sticky, suid, sgid permissions. Their order is significant
		temp = g_utf8_to_ucs4_fast (_("TtSs"), -1, NULL);
		memcpy (fbits, temp, sizeof (fbits));
		g_free (temp);
	}

	memcpy (thisperms, realperms, sizeof (realperms));
	thisperms [10] = 0;

	gint mask = 1;
	gint i;
	//assuming that mode flags are in the same order as the perms string ...
	for (i = 9; i > 0; --i)
	{
		if (!(mode & mask))
			thisperms[i] = thisperms[0];
		mask = mask << 1;
	}

	if (mode & S_ISVTX)
		thisperms[9] = (thisperms[9] == thisperms[0]) ? fbits[0] : fbits[1];
	if (mode & S_ISGID)
		thisperms[6] = (thisperms[6] == thisperms[0]) ? fbits[2] : fbits[3];
	if (mode & S_ISUID)
		thisperms[3] = (thisperms[3] == thisperms[0]) ? fbits[2] : fbits[3];

	switch (mode & S_IFMT)
	{
#ifdef S_IFLNK
		case S_IFLNK:
			i = 0;
			break;
#else
# warning "SYMLINK type not supported"
#endif
		case S_IFDIR:
			i = 1;
			break;
		case S_IFBLK:
			i = 2;
			break;
		case S_IFCHR:
			i = 3;
			break;
#ifdef S_IFIFO
		case S_IFIFO:
			i = 4;
			break;
#else
# warning "FIFO type not supported"
#endif
#ifdef S_IFSOCK
		case S_IFSOCK:
			i = 5;
			break;
#else
# warning "SOCKET type not supported"
#endif
		default:
			i = -1;
			break;
	}
	if (i >= 0)
		thisperms[0] = ftypes[i];

	gchar *retval = g_ucs4_to_utf8 (thisperms, -1, NULL, NULL, NULL);

	return retval;
}
/**
@brief display error message @a msg, with current system error description
Assumes BGL is open
@param msg utf-8 string message to display

@return FALSE (==error signal) always
*/
gboolean e2_fs_error (gchar *msg
#ifdef E2_VFS
	, gchar *reason
#endif
	)
{
	gchar *long_msg;
#ifdef E2_VFS
	long_msg = (reason != NULL) ?
		g_strconcat (msg, " - ", reason, NULL) : g_strdup (msg);
#else
	long_msg = (errno != 0) ?
		g_strconcat (msg, " - ", (gchar *) g_strerror (errno), NULL) : g_strdup (msg);
#endif
	CLOSEBGL
	e2_output_print_error (long_msg, TRUE);
	OPENBGL
	return FALSE;
}
/**
@brief construct and display error message, with current system error description
Assumes BGL is open
@param format utf-8 format string (with a "%s") for the message to display
@param local localised itemname or path string to be incorporated into the message

@return FALSE (==error signal) always
*/
gboolean e2_fs_error_local (const gchar *format, VPATH *local
#ifdef E2_VFS
		, gchar *reason
#endif
	)
{
	gchar *utf = F_DISPLAYNAME_FROM_LOCALE (VPSTR(local));
	//for output pane printing, no need to escape any pango-annoying component
	//of the itemname
	gchar *msg = g_strdup_printf (format, utf);
	e2_fs_error (msg
#ifdef E2_VFS
		, reason
#endif
		);
	F_FREE (utf, VPSTR(local));
	g_free (msg);
	return FALSE;
}
/**
@brief construct and display error message using @a format and @a local
Assumes BGL is open
@param format utf-8 string, format (with a "%s") for the message to display
@param local localised itemname or path string to be incorporated into the message

@return FALSE (==error signal) always
*/
gboolean e2_fs_error_simple (const gchar *format, VPATH *local)
{
	gchar *utf = F_DISPLAYNAME_FROM_LOCALE (VPSTR(local));
	//no need to escape any pango-annoying component of the itemname
	gchar *msg = g_strdup_printf (format, utf);
	CLOSEBGL
	e2_output_print_error (msg, TRUE);
	OPENBGL
	F_FREE (utf, VPSTR(local));
	return FALSE;
}

#ifdef E2_VFS
/**
@brief create filesystem error based on errno

@param error pointer to error data struct, or NULL

@return
*/
void e2_fs_set_error_from_errno (GError **error)
{
	if (error != NULL)
	{
		g_clear_error (error);
//		GFileError err_no = g_file_error_from_errno (errno);
//		*error = g_error_new_literal (G_FILE_ERROR, err_no, g_strerror (errno));
		*error = g_error_new_literal (G_FILE_ERROR, errno, g_strerror (errno));
	}
}
/**
@brief create custom error

@param error pointer to error data struct, or NULL
@param code error code
@param format printf()-style format string for the error message

@return
*/
void e2_fs_set_error_custom (GError **error, gint code, const gchar *format, ...)
{
	if (error != NULL)
	{
		gchar *built;

		g_clear_error (error);

		if (format != NULL)
		{
			const gchar *name = format;
			gchar *freeme;
			built = g_strdup (format);
			va_list args;
			va_start (args, format);
			while (name != NULL)
			{
				freeme = built;
				built = g_strconcat (freeme, ", ", name, NULL);
				g_free (freeme);
				name = va_arg (args, const gchar*);
			}
			va_end (args);
		}
		else
			built = NULL;

		g_set_error (error, G_FILE_ERROR, code, built);

		if (built != NULL)
			g_free (built);
	}
}
/**
@brief check whether @a func, a member of the vfs functions interface, is available
Availability is indicated by non-NULL pointer. The vfs plugin will be loaded if possible
@param func store for function-ptr to check

@return TRUE if the function was, or now is, available
*/
gboolean e2_fs_vfsfunc_ready (gpointer *func)
{
	if (!vfs.loaded)
	{
		//e2_window_set_cursor (GDK_WATCH); may freeze, probably unnecessary
		if (!e2_plugins_get_plugin ("vfs"VERSION))	//this will set the function ptr if it succeeds
			return FALSE;
		//e2_window_set_cursor (GDK_LEFT_PTR);
	}
	return (*func != NULL);
}
#endif
