/* $Id: e2p_unpack.c 3039 2014-02-02 21:37:42Z tpgww $

Copyright (C) 2003-2014 tooar <tooar@emelfm2.net>

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
@file plugins/e2p_unpack.c
@brief plugin for interfacting with several archive managers, to unpack selected item(s)
*/

#include "emelfm2.h"
#include <string.h>
#include <glob.h>
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_task.h"
#include "e2_filelist.h"
#include "e2_icons.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "unpack"

//NOT same enum as in pack plugin
typedef enum
{
	UPTYPENONE = -1,
	//NOTE some handler-string-arrays are assumed to be arranged in same order as this enum
	TAR_GZ, TAR_BZ2, TAR_XZ, ZIP, TAR_7Z, Z7Z, TAR_LZMA, TAR, DEB, RPM, RAR, ARJ, ZOO, ISO,
	UPTYPESCOUNT
} E2_UnPackType;

typedef struct _E2P_Unpackdata
{
	gchar *package;	//absolute path of source archive, UTF-8 string
	gchar *workdir;	//absolute path of dir used to unpack this archive,
					//== unpack_tmp or a tmp variant of that, UTF-8, no trailer
	gchar *last_dir;	//dir to go back to after starting a repack
	glong thispid;	//id of process which is re-packing an archive
//	guint chdir_id;	//id of timer which checks for whether to ask user what to do
	guint pack_id;	//id of timer which checks for repack completion
	E2_UnPackType type; //enum for type of package being processed
	guint compress_type; //enum for type of tarball compression in .deb package
	gpointer from;	//widget which initiated the unpack action
	E2_CDType cd_completed;	//flag set when cd to temp dir is completed
	gchar *command;	//the pack (and later, unpack) command to be run, UTF-8 string
	gboolean departing;	//TRUE when the temp dir is waiting to be processed after
						//the user selects from a what-to-do dialog
						//also used to block re-entrant use of the hook function
} E2P_Unpackdata;

static gboolean _e2p_unpack_delete_dir (E2P_Unpackdata *data);
static gboolean _e2p_unpack_change_dir_hook (gchar *path, E2P_Unpackdata *data);

/*
//strings used in processing compressed items in .deb files
//each row has a suffix (to be appended to ...tar) and associated uncompress-to-stdout command
static gchar *unsqueezers[5][2] =
{
	{"",	"cat"},
	{".gz",	"gzip -cd"},
	{".bz2","bzip2 -cd"},
	{".lzma","lzma -cd"},
	{".xz",	"xz -cd"}
};
*/

static PluginIface iface;

static gchar *unpack_tmp = NULL;	//absolute path of 'base working' directory (no trailer), UTF-8 string

/**
@brief cleanup plugin data
This is executed with BGL open or closed
@param data pointer to plugin data

@return
*/
static void _e2p_unpack_cleanup (E2P_Unpackdata *data)
{
	g_free (data->package);
	g_free (data->workdir);
	g_free (data->command);
	if (data->last_dir != NULL)
		g_free (data->last_dir);
	DEALLOCATE (E2P_Unpackdata, data);
}
/**
@brief select the relevant archive-enumerator corresponding to to @a localpath

@param localpath pointer to archive data, with localised path

@return the number, or UPTYPENONE upon no match
*/
static E2_UnPackType _e2p_unpack_match_type (VPATH *localpath)
{
	gint i, count;
	E2_UnPackType thistype = UPTYPENONE;
	gchar *thismime = e2_utils_get_mimetype (localpath);

	if (thismime != NULL)
	{
		const gchar *mimes [] =
		{
			"x-gzip",
			"x-bzip2",
			"x-xz-compressed-tar",
			"x-tar",
			"zip",
			"x-7z-compressed-tar",	//this is probably N/A ATM
			"x-7z-compressed",		//this may be reported for .tar.7z as well as .7z - further check needed
			"x-lzma-compressed-tar",//CHECKME not sure about lzma mimetype yet
			"x-deb",
			"x-rpm",
			"x-rar-compressed",
			"arj",
			"x-arj",
			"zoo",
			"x-zoo",
			"x-cd-image",
			"iso-image"
		};
		E2_UnPackType mimecodes [] =
		{
			TAR_GZ,
			TAR_BZ2,
			TAR_XZ,
			TAR,
			ZIP,
			TAR_7Z,
			Z7Z,
			TAR_LZMA,
			DEB,
			RPM,
			RAR,
			ARJ,
			ARJ,
			ZOO,
			ZOO,
			ISO,
			ISO
		};

		if (g_str_has_prefix (thismime, "application/"))
		{
			gchar *s = thismime + sizeof (gchar) * 12; //skip the prefix
			count = sizeof (mimes) / sizeof (gchar *);
			for (i = 0; i < count; i++)
			{
				if (strcmp (s, mimes [i]) == 0)
				{
					if (i == 6)
					{
						if (g_str_has_suffix (VPCSTR(localpath), ".tar.7z")
						 || g_str_has_suffix (VPCSTR(localpath), ".t7z"))
						i = 5;
					}
					thistype = mimecodes [i];
					break;
				}
			}
		}
		g_free (thismime);
	}

	if (thistype == UPTYPENONE)
	{
		//mime-check by xdg-utils(1) or file(1) not available, try extension-match
		const gchar *extensions [] =
		{
			".tar.gz", //compress(1) does ".taz", ".tar.z",
			".tgz",
			".tar.bz2",
			".tbz2",
			".tar.xz",
			".tar",
			".zip",
			".tar.7z",
			".t7z",
			".7z",
			".tar.lzma",
			".tlz",
			".deb",
			".rpm",
			".rar",
			".arj",
			".zoo",
			".iso"
		};
		//codes corresponding to extensions array, above
		E2_UnPackType extcodes [] =
		{
			TAR_GZ,
			TAR_GZ,
			TAR_BZ2,
			TAR_BZ2,
			TAR_XZ,
			TAR,
			ZIP,
			TAR_7Z,
			TAR_7Z,
			Z7Z,
			TAR_LZMA,
			TAR_LZMA,
			DEB,
			RPM,
			RAR,
			ARJ,
			ZOO,
			ISO
		};

		//CHECKME matched string not at end of name - ok ?
		count = sizeof (extensions) / sizeof (gchar *);
		for (i = 0; i < count; i++)
		{
			if (g_str_has_suffix (VPSTR (localpath), extensions[i]))
			{
				thistype = extcodes [i];
				break;
			}
		}
	}
	return thistype;
}
/**
@brief check for, and if found, decompress, instances of a type of compressed file

Tarballs need to be handled by 2 calls here.

@param ext compressed file extension ".gz" ... ".xz", ".tar" or ""
@param handler command to be run to process each individual item matching @a ext
@param from the button, menu item etc which was activated to initiate this action
@param localpath localised path string of temporary workdir containing
 now-unpacked but still-compressed items (if any) to be processed

@return 0 on success, command exit-code or glob error-code otherwise
*/
static gint _e2p_unpack_decompress_helper (const gchar *ext, const gchar *handler,
	gpointer from, const gchar *localpath)
{
	gint retval;
	gchar *pattern;
	gchar base[16];
	glob_t matches;

	snprintf (base, sizeof(base), "*%s", ext);
	pattern = g_build_filename (localpath, base, NULL);

	retval = glob (pattern, GLOB_NOSORT, NULL, &matches);
	if (retval == 0)
	{
		size_t i;

		CLOSEBGL
		for (i = 0; i < matches.gl_pathc; i++)
		{
			gchar *cmd = g_strdup_printf (handler, matches.gl_pathv[i]);
			retval = e2_command_run_at (cmd, localpath, E2_COMMAND_RANGE_DEFAULT,
				from
#ifdef E2_COMMANDQ
				, T/F ?
#endif
			);
			g_free (cmd);
			if (retval != 0)
				break;
		}
		OPENBGL
	}
	g_free (pattern);
	globfree (&matches);

	return retval;
}
/**
@brief decompress any compressed file(s) in @a localpath

Assumes BGL open upon arrival. Downstream relies on that.
Return value is per (now-unused) strings-array row-index, 0 for uncompressed tar
... 4 for xz i.e. NOT a E2_UnPackType.

@param from the button, menu item etc which was activated
@param localpath temporary workdir containing unpacked items

@return enumerator 0... representing 'latest-compression-type' of files processed,
 or -1 upon error
*/
static gint _e2p_unpack_decompress (gpointer from, const gchar *localpath)
{
	gint retval = -1;

#ifdef DEBUG_MESSAGES
	gchar *output = NULL;
	e2_fs_get_command_output ("pwd", (gpointer *)&output);
	printd (DEBUG, "Decompressing unpacked archives in %s", output);
	g_free (output);
#endif
	//cuz decompressions may be in series, the first ones block till done
	//process all gz's
	if (_e2p_unpack_decompress_helper (".gz", "|gzip -d %s", from, localpath) == 0)
		retval = 1;
	//process all bz2's
	if (_e2p_unpack_decompress_helper (".bz2", "|bzip2 -d %s", from, localpath) == 0)
		retval = 2;
	//process all lzma's
	if (_e2p_unpack_decompress_helper (".lzma", "|lzma -d %s", from, localpath) == 0)
		retval = 3;
	//process all xz's
	if (_e2p_unpack_decompress_helper (".xz", "|xz -d %s", from, localpath) == 0)
		retval = 4;
	//prior unpack may be incomplete
	if (retval != -1)
	{
		WAIT_FOR_EVENTS_UNLOCKED_SLOWLY
	}
	//process all tar's, perhaps created by prior call to helper
	if (_e2p_unpack_decompress_helper (".tar",
		"TARFILE=%s;tar -xpf $TARFILE && rm -f $TARFILE", from,	localpath) == 0
		&& retval == -1)
			retval = 0;

	//clear assigned internal variable
	gchar *cmd = g_strdup ("TARFILE=");
	e2_command_run_at (cmd, localpath, E2_COMMAND_RANGE_DEFAULT, from
#ifdef E2_COMMANDQ
		, T/F ?
#endif
	);
	g_free (cmd);

#ifdef E2_FAM
	e2_filelist_request_refresh (curr_view->dir, FALSE);
//	e2_filelist_request_refresh (other_view->dir, TRUE);
#else
	e2_filelist_check_dirty (GINT_TO_POINTER (1));
#endif

	return retval;
}
/**
@brief timer callback to complete unpacking of some sorts of item

@param data pointer to unpack data struct

@return FALSE (to cancel timer) after processing has been completed
*/
static gboolean _e2p_unpack_expand_contents (E2P_Unpackdata *data)
{
	if (!g_str_has_prefix (curr_view->dir, data->workdir))
		return TRUE; //not at unpack dir yet, wait some more
//	if (0) //TODO check for not ready yet
//		return TRUE;

	if (data->type == DEB)
	{
		data->compress_type = _e2p_unpack_decompress (data->from, data->workdir);
	}
	else if (data->type == RPM)
	{
		_e2p_unpack_decompress (data->from, data->workdir);
	}

	CLOSEBGL
	e2_window_set_cursor (GDK_LEFT_PTR);
	OPENBGL

	return FALSE;
}
/**
@brief timer callback to resume idle checking

@param data pointer to unpack data struct

@return FALSE always
*/
static gboolean _e2p_unpack_pause (E2P_Unpackdata *data)
{
	data->pack_id = g_idle_add_full (G_PRIORITY_LOW,
		(GSourceFunc)_e2p_unpack_delete_dir, data, NULL);
	return FALSE;
}
/**
@brief idle callback to check whether temp-dir deletion can proceed safely

We do this check at an idle time to reduce risk of conflict, but interpose a
timer between idle callbacks to reduce their frequency

@param data pointer to unpack data struct

@return FALSE always
*/
static gboolean _e2p_unpack_delete_dir (E2P_Unpackdata *data)
{
	LISTS_LOCK
	//conservative approach - no deletion while any chance of the temp-dir being used
	if (g_atomic_int_get (&curr_view->listcontrols.cd_working) ||
		g_atomic_int_get (&curr_view->listcontrols.refresh_working) || (
#ifdef E2_VFS
		curr_view->spacedata == NULL &&	//local temp dirs
#endif
		g_str_has_prefix (curr_view->dir, data->workdir)))
	{
		LISTS_UNLOCK
		//wait before checking for an idle again
		data->pack_id = g_timeout_add (500, (GSourceFunc)_e2p_unpack_pause, data);
		return FALSE;
	}
	if (g_atomic_int_get (&other_view->listcontrols.cd_working) ||
		g_atomic_int_get (&other_view->listcontrols.refresh_working) || (
#ifdef E2_VFS
		other_view->spacedata == NULL &&	//local temp dirs
#endif
		g_str_has_prefix (other_view->dir, data->workdir)))
	{
		LISTS_UNLOCK
		//wait before checking for an idle again
		data->pack_id = g_timeout_add (500, (GSourceFunc)_e2p_unpack_pause, data);
		return FALSE;
	}
	LISTS_UNLOCK
	//kill the idle now
//	if (data->pack_id > 0)
//		g_source_remove (data->pack_id);
//	else
//		while (g_source_remove_by_user_data (data)) {}
	//now we're ready for cleanup
	e2_filelist_disable_refresh ();
	gchar *local = F_FILENAME_TO_LOCALE (data->workdir);
#ifdef E2_VFS
	VPATH ddata = { local, NULL };	//local unpacking only
	if (e2_fs_access2 (&ddata E2_ERR_NONE()) == 0)
	{
		if (data->type == ISO)
		{
			//must unmount before dir can be deleted
			gchar *cmd = e2_utils_strcat ("umount ", data->workdir);
			e2_command_run_at (cmd, NULL,
				E2_COMMAND_RANGE_DEFAULT, data->from
#ifdef E2_COMMANDQ
				, T/F ?
#endif
				);
			g_free (cmd);
		}
		e2_task_backend_delete (&ddata);
	}
#else
	if (e2_fs_access2 (local E2_ERR_NONE()) == 0)
	{
		if (data->type == ISO)
		{
			//must unmount before dir can be deleted
			gchar *cmd = e2_utils_strcat ("umount ", data->workdir);
			e2_command_run_at (cmd, NULL,
				E2_COMMAND_RANGE_DEFAULT, data->from
#ifdef E2_COMMANDQ
				, T/F ?
#endif
				);
			g_free (cmd);
		}
		e2_task_backend_delete (local);
	}
#endif

	e2_filelist_enable_refresh ();
	F_FREE (local, data->workdir);
	_e2p_unpack_cleanup (data);
	//FIXME different refresh approach with E2_ASYNC
	//in case a pane shows parent of temp dir
#ifdef E2_FAM
	e2_filelist_request_refresh (curr_view->dir, FALSE);
	e2_filelist_request_refresh (other_view->dir, TRUE);
#else
	e2_filelist_check_dirty (GINT_TO_POINTER (1));
#endif
	return FALSE;
}
/**
@brief delete the temp dir
This is executed with BGL open or closed
@param data pointer to plugin data

@return
*/
static void _e2p_unpack_clear (E2P_Unpackdata *data)
{
	//ensure BGL is open and manage any race with refresh function
	data->pack_id = g_idle_add_full (G_PRIORITY_LOW,
		(GSourceFunc)_e2p_unpack_delete_dir, data, NULL);
}
/* *
@brief timer callback to periodically check whether a repack is completed
Needed if repack command does not include cleanup
@param data pointer to plugin data struct

@return TRUE if the repack process is still running
*/
/*static gboolean _e2p_unpack_clean_dir (E2P_Unpackdata *data)
{
	if (e2_command_find_process ((guint)data->thispid))
		return TRUE;	//wait some more
	//FIXME only delete dir if repack command succeeded, or else don't delete
	//original archive until repack succeeded

	//now we're ready for cleanup
//	g_source_remove (data->pack_id);
//	if (_e2p_unpack_delete_dir (data))
		//keep trying until the deletion is done
		g_idle_add_full (G_PRIORITY_LOW,
			(GSourceFunc)_e2p_unpack_delete_dir, data, NULL);
	return FALSE;
}
*/
/**
@brief repack the temp dir
This is executed inside a callback with BGL closed
@param data pointer to plugin data
@param from the dialog where the action was initiated

@return
*/
static void _e2p_unpack_repack (E2P_Unpackdata *data, gpointer from)
{
#ifdef E2_VFSTMP
	if (curr_view->spacedata != NULL)	//CHECKME fuse ok?
	{
		_e2p_unpack_cleanup (data);
		return;
	}
#endif

	const gchar *cmd_str [UPTYPESCOUNT] =
	{	//these command strings are in same order as enum
		//they are all designed to be executed from the unpack temp dir, and
		//on all its contents (recursive if appropriate)
		//CHECKME "." always remains correct during repack execution ?
		">tar cf - . | gzip - > %s",
		">tar cf - . | bzip2 - > %s",	//default block size -9 is not significantly better, maybe use -3 instead ?
		">tar cf - . | xz - > %s",		//default compresssion -6, maybe use -3 instead ?
		">zip -r - . > %s",
		">tar cf - . | 7za a -si %s 1>/dev/null", //default compresssion type 7z
		">7za a %s . 1>/dev/null",
		">tar cf - . | lzma - > %s",
		"tar cf %s .",
//		"echo \".deb package re-packing is not supported\"",	//deb 'pack' process N/A here
		NULL,	//.deb pack command involves custom treatment - see below
//		"echo \".rpm package re-packing is not supported\"",	//rpm 'pack' process does not exist, in general
		NULL,	//.rpm pack command involves custom treatment - see below
		//updates of archives probably don't capture renames,deletes
		"rar u -as -ol -tl -r %s .", //OK
		"arj a -u -r -s -a -2s %s .", //CHECKME
		"zoo unP %s .",	//CHECKME replace, not update ? deletions ?
		"mkisofs -o %s . && umount -lf %s" //NOTE extra %s, special handling needed
	};
/*
a deb top-level package contains "debian-binary", a small file with a version
number, and two archives - one (control.tar.gz|bz2...) contains the package description,
MD5 checksums and install/uninstall scripts (none of which have a path),
the other (data.tar.gz|bz2....) holds the files to be installed, each with its OS path
(i.e. unpacked relative to the unpack directory).
so to repack:
find . -maxdepth 1 \! -type d \! -name debian-binary -exec tar -c --owner=0 --group=0 '{}' + > control.tar.gz;
find . -maxdepth 1 -type d \! -name . -exec tar -c --owner=0 --group=0 '{}' + > data.tar.gz;
ar -rD %s debian-binary control.tar.gz data.tar.gz
*/
	gint res;
	gchar *fmt, *qp;
	gchar *package = data->package;
	gchar *local = F_FILENAME_TO_LOCALE (package);

	g_free (data->command);

	E2_UnPackType uptype = data->type;
	switch (uptype)
	{
#if 0
		case DEB:
		/* Assume lzma compression is ok for tarballs in current deb packages.
		   Or else, cache the unpack-type and customise this command accordingly */
			cmd_str [index] =
/*
">find . -maxdepth 1 \\! -type d \\! -name debian-binary -exec tar -c --owner=0 --group=0 '{}' + | lzma - > control.tar.lzma;"
">find . -maxdepth 1 -type d \\! -name . -exec tar -c --owner=0 --group=0 '{}' + | lzma - > data.tar.lzma;"
"chown +0:+0 debian-binary control.tar.lzma data.tar.lzma;"
"ar -r %s debian-binary control.tar.lzma data.tar.lzma"; //archive content-order matters
*/
			"echo \".deb package re-packing is not supported\"";
#endif
		case TAR_GZ:
		case TAR_BZ2:
		case TAR_XZ:
		case ZIP:
		case TAR_7Z:
		case Z7Z:
		case TAR_LZMA:
		case TAR:
			//these types of archive are overwritten
			fmt = g_strconcat (cmd_str [uptype], " && mv -f %s %s && rm -rfd %s", NULL);
			qp = e2_utils_quote_string (package);
			gchar *tmp = e2_utils_get_tempname
#ifdef E2_VFSTMP
				(ddata.path);
#else
				(local);
#endif
			gchar *utftmp = F_FILENAME_FROM_LOCALE (tmp);
//tag E2_BADQUOTES
			gchar *qpt = e2_utils_quote_string (utftmp);
			g_free (tmp);
			F_FREE (utftmp, tmp);
			data->command = g_strdup_printf (fmt, qpt, qpt, qp, data->workdir);
			g_free (qpt);
			break;
		case RAR:
		case ARJ:
		case ZOO:
		case ISO:
			//these types of archive are just updated
			fmt = g_strconcat (cmd_str [uptype], " && rm -rfd %s", NULL);
			qp = e2_utils_quote_string (package);
			if (uptype != ISO)
				data->command = g_strdup_printf (fmt, qp, data->workdir, NULL);
			else
				data->command = g_strdup_printf (fmt, qp, data->workdir, data->workdir, NULL);
//			printd (DEBUG, "repack command %s", data->command);
			break;
		case DEB:
		case RPM:
			fmt = qp = NULL;	//warning prevention
			data->command = NULL;
			break;
		default:
//		case UPNONE:	//should never happen
			fmt = qp = NULL;	//warning prevention
			_e2p_unpack_cleanup (data);
			return;
	}

	g_free (fmt);
	g_free (qp);
	F_FREE (local, package);

	if (data->command != NULL)
	{
		res = e2_command_run_at (data->command, data->workdir,
		E2_COMMAND_RANGE_DEFAULT, from
#ifdef E2_COMMANDQ
		, T/F ?
#endif
		);
	}
	else
		res = 0;

	if (res == 0)
	{
/* use this if command doesn't include cleanups i.e. rm workdir
	//FIXME race here if something else is run at a bad time, so find the pid
	//with matching command-string instead
		E2_TaskRuntime *td = e2_task_find_last_running_child (TRUE);
		data->thispid = (td != NULL) ? td->pid : 0;
		//periodically check whether re-build finished, when so, cleanup the temp dir
		//CHECKME make this timer cancellable at session end
		data->pack_id = g_timeout_add (500, (GSourceFunc) _e2p_unpack_clean_dir, data);
		//CHECKME refreshing etc may move CWD away from the temp dir while the repack is underway
*/
		_e2p_unpack_cleanup (data); //or else, just cleanup data
	}
	else
	{
//		data->thispid = 0;
		printd (WARN, "repack command cannot be run");
		//WARN user ?
		_e2p_unpack_cleanup (data);
	}
}
/**
@brief callback for "what-to-do" dialog's "response" signal

@param dialog UNUSED the dialog where the response was triggered
@param response the response for the clicked button
@param rt pointer to data for dialog

@return
*/
static void _e2p_unpack_response_decode_cb (GtkDialog *dialog, gint response,
	E2P_Unpackdata *data)
{
	gtk_widget_destroy (GTK_WIDGET(dialog));
	//do this outside of current hook func, as we need to check both panes and data
	e2_hook_unregister (&app.pane1.hook_change_dir,
		(HookFunc)_e2p_unpack_change_dir_hook, data, TRUE);
	e2_hook_unregister (&app.pane2.hook_change_dir,
		(HookFunc)_e2p_unpack_change_dir_hook, data, TRUE);

	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_USER1:	//repack the temp dir
			_e2p_unpack_repack (data, (gpointer)dialog);
			break;
		case E2_RESPONSE_USER2: //keep the unpacked archive
			_e2p_unpack_cleanup (data);
		//	case GTK_RESPONSE_CANCEL:
			break;
		//case E2_RESPONSE_REMOVE:
		default: //this will pick up GTK_RESPONSE_NONE or GTK_RESPONSE_DELETE_EVENT
			_e2p_unpack_clear (data);
			break;
	}
	NEEDOPENBGL
}
/**
@brief hook function for cd in either pane (app.paneX.hook_change_dir)
This is initiated from a cd thread, and with BGL open/off
@param path UNUSED path of an opened directory, utf-8 string
@param data pointer to operation data struct
@return TRUE always so hook remains active
*/
static gboolean _e2p_unpack_change_dir_hook (gchar *path, E2P_Unpackdata *data)
{
	if (data->departing)
		return TRUE;	//a callback has begun, ignore this other cd
	data->departing = TRUE;	//temp block on nested checking
	/* this hookfunc is called toward the end of a cd process, there's no need
	   to check for various "busy" flags before proceeding
	   the first cd will be into the temp dir, so path will be that dir with
	   trailing separator */
	if (
#ifdef E2_VFSTMP
		//FIXME dirs when not mounted local
		curr_view->spacedata == NULL //local temp dirs
		other_view->spacedata == NULL //local temp dirs
#else
		( g_str_has_prefix (curr_view->dir, data->workdir)
		|| g_str_has_prefix (other_view->dir, data->workdir))
#endif
	   )
	{
		data->departing = FALSE;	//unblock
		return TRUE;
	}

	//user changed dir, now neither pane is for anywhere in unpack-dir tree
	printd (DEBUG, "ready to cleanup unpack dir");

	//ask user what to do with the unpacked items
	CLOSEBGL
	GtkWidget *dialog = e2_dialog_create (STOCK_NAME_DIALOG_QUESTION,
		_("What do you want to do with the unpacked items ?"), NULL,
		(ResponseFunc)_e2p_unpack_response_decode_cb, data);
	OPENBGL

	//re-pack button for types that allow it
	//we haven't yet decoded which re-pack commands are un-available, so need
	//to hard-code the un-recoverable ones
	if (!(data->type == DEB || data->type == RPM))
		e2_dialog_add_simple_button (dialog, STOCK_NAME_CLEAR,
			_("Re_pack"), E2_RESPONSE_USER1);

	e2_dialog_add_simple_button (dialog, STOCK_NAME_APPLY,
		_("_Retain"), E2_RESPONSE_USER2);
	GtkWidget *button = e2_dialog_add_simple_button (dialog, STOCK_NAME_DELETE,
		_("_Delete"), E2_RESPONSE_REMOVE);

	CLOSEBGL
	e2_dialog_setup (dialog, app.main_window);
	gtk_widget_show_all (dialog);
	gtk_widget_grab_focus (button);
	gtk_window_present (GTK_WINDOW (dialog));
	OPENBGL

	return TRUE; 	//no hook cleanup here
					//done in response cb, as we need to check both panes and data
}
/**
@brief unpack plugin action : unpack a supported archive into a temp dir
Best if refreshing is disabled when this action is initiated
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_unpack (gpointer from, E2_ActionRuntime *art)
{
	//these unpack-command strings are in same order as enum
	//all are executed from the temp dir (as some can only do that)
	//all are ASCII (no conversion to UTF-8 before execution)
	static gchar *cmd_str [UPTYPESCOUNT] =
	{
		//each unpack goes into a separate newly-created work dir, so there's
		//no need to worry manage over-writing when unpacking with these commands
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(darwin)
		//tar decompression options -z, -j, --lzma are GNU extensions
		//ditto for --overwrite, and for -C, if that's wanted
		">gzip -cd %s | tar -xpf -",
		">bzip2 -cd %s | tar -xpf -",
		">xz -cd %s | tar -xpf -",
#else
		//note: an --atime-preserve in these tar commands prevents the
		//file-monitoring process from noticing anything in the temp dir
		//--overwrite not needed
		"tar -xpzf %s",
		"tar -xpjf %s",
		"tar --xz -xpf %s",
#endif
		"unzip -o %s",  //or "unzip -o -d %s %s"
		">7za x -so %s | tar -xpf -",
		"7za x %s",
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(darwin)
		">lzma -cd %s | tar -xpf -",
#else
		"tar --lzma -xpf %s",
#endif
		"tar -xpf %s",
		"|ar -x %s",  //start the unpack, with block until done, to allow further processing
		">|rpm2cpio %s | cpio -id", //ditto
		"rar x -o+ %s", //rar will only extract to current dir
		"arj x -y %s", //or "arj x -y "UNPACKPATH" "UNPACKPATH, //NOTE swapped order of archive & path
		"zoo xO %s",	//zoo will only extract to current dir
		"mount -o loop %s ." //auto-detect FS-type, option -o loop needs superuser
	};

	//CHECKME 	e2_filelist_disable_refresh ();
	FileInfo *info = e2_fileview_get_selected_first_local (curr_view, FALSE);
	if (info == NULL)
	{
//		e2_filelist_enable_refresh ();
		return FALSE;	//nothing selected
	}

#ifdef E2_VFSTMP
	VPATH ddata;
	ddata.path = e2_utils_dircat (curr_view, info->filename, TRUE);
	ddata.spacedata = curr_view->spacedata;
	E2_UnPackType uptype = _e2p_unpack_match_type (&ddata);
	g_free (ddata.path);
#else
	gchar *local = e2_utils_dircat (curr_view, info->filename, TRUE);
	E2_UnPackType uptype = _e2p_unpack_match_type ((VPATH*)local);
	g_free (local);
#endif
	if (uptype == UPTYPENONE)
	{
		e2_output_print_error (_("Selected item is not a supported archive"), FALSE);
//		e2_filelist_enable_refresh ();
		return FALSE;
	}

	//the current temp dir may be deleted when the user exits that dir, so it
	//would be very bad to open an archive inside the temp dir
	//CHECKME recursive unpacking would be ok (with different temp dirs for each
	//archive) if there's a way to handle (prevent?) repacking when the 'parent'
	//temp dir is alredy gone
#ifdef E2_VFSTMP
	//FIXME handle space-change too
	if (curr_view->spacedata == ? || strstr (curr_view->dir, unpack_tmp) != NULL)
#else
	if (strstr (curr_view->dir, unpack_tmp) != NULL)
#endif
	{
		e2_output_print_error (_("Recursive unpack is not supported"), FALSE);
//		e2_filelist_enable_refresh ();
		return FALSE;
	}

	gchar *converted = F_FILENAME_TO_LOCALE (unpack_tmp);
	gchar *workdir = e2_utils_get_tempname (converted);
#ifdef E2_VFS
	VPATH ddata = { workdir, NULL };	//local unpacking only
#endif
	F_FREE (converted, unpack_tmp);
	//(re)make it
#ifdef E2_VFS
	if (e2_fs_recurse_mkdir (&ddata, 0777 E2_ERR_NONE()))
#else
	if (e2_fs_recurse_mkdir (workdir, 0777 E2_ERR_NONE()))
#endif
	{
		converted = F_DISPLAYNAME_FROM_LOCALE (workdir);
		gchar *msg = g_strdup_printf ("Could not create working directory '%s'",
			converted);
		e2_output_print_error (msg, TRUE);
		F_FREE (converted, workdir);
		g_free (workdir);
//		e2_filelist_enable_refresh ();
		return FALSE;
	}

	E2P_Unpackdata *data = ALLOCATE0 (E2P_Unpackdata);
	CHECKALLOCATEDWARN (data, return FALSE;)
	data->workdir = D_FILENAME_FROM_LOCALE (workdir);
	g_free (workdir);

	data->type = uptype;
	converted = F_FILENAME_FROM_LOCALE (info->filename); //not much race chance, don't D_...
#ifdef E2_VFSTMP
	FIXME dir when not mounted local
#else
	data->package = e2_utils_strcat (curr_view->dir, converted);  //dir has trailing /
	F_FREE (converted, info->filename);
//tag E2_BADQUOTES
	gchar *qp = e2_utils_quote_string (data->package);
#endif
	//need no conversion of command encoding
	data->command = g_strdup_printf (cmd_str [uptype], qp); //CHECKME append 1>/dev/null
	g_free (qp);
	e2_window_set_cursor (GDK_WATCH);
	//unpack the archive into the temp directory
	gint result = e2_command_run_at (data->command, data->workdir,
		E2_COMMAND_RANGE_DEFAULT, from
#ifdef E2_COMMANDQ
		, T/F ?
#endif
		);

	if (result != 0)
	{
		workdir = F_FILENAME_TO_LOCALE (data->workdir);
#ifdef E2_VFS
		ddata.path = workdir;
		e2_task_backend_delete (&ddata);
#else
		e2_task_backend_delete (workdir);
#endif
		F_FREE (workdir, data->workdir);
		_e2p_unpack_cleanup (data);
//		e2_filelist_enable_refresh ();
		e2_window_set_cursor (GDK_LEFT_PTR);
		return FALSE;
	}
	//now go see it
	e2_pane_change_dir (NULL, data->workdir);

	if (uptype == DEB || uptype == RPM)
	{
		//wait for async cd before finishing de-compression from there
		data->from = from;
		g_timeout_add (50, (GSourceFunc)_e2p_unpack_expand_contents, data);
	}
	else
		e2_window_set_cursor (GDK_LEFT_PTR);

	//setup to clean when temp dir not open anymore
	e2_hook_register (&app.pane1.hook_change_dir,
		(HookFunc)_e2p_unpack_change_dir_hook, data);
	e2_hook_register (&app.pane2.hook_change_dir,
		(HookFunc)_e2p_unpack_change_dir_hook, data);

//	e2_filelist_enable_refresh ();
	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	gchar *name;
#ifdef E2_VFS
	name = _("unpack_with_plugin");
#else
	name = _A(107);
#endif

	PLUGINIT_ONE_START(_A(6),name,_e2p_unpack,
		_("_Unpack"),
		_("Unpack archive file into a temporary directory"),
		"plugin_"ANAME E2ICONTB)

	//setup the working dir name
	unpack_tmp = e2_utils_get_temp_path ("-unpack");
	//strip the trailing ".tmp~0" as we will append a suffix like that for each unpack
	gchar *s = strrchr (unpack_tmp, '.');
	*s = '\0';

	PLUGINIT_ONE_END
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	//FIXME prevent unload when an unpack is underway
	//clear any current hook(s)
	while (e2_hook_unregister (&app.pane1.hook_change_dir,
		(HookFunc)_e2p_unpack_change_dir_hook, NULL, FALSE)) {}
	while (e2_hook_unregister (&app.pane2.hook_change_dir,
		(HookFunc)_e2p_unpack_change_dir_hook, NULL, FALSE)) {}

	PLUGIN_CLEAR_ACTIONS (p)

	g_free (unpack_tmp);
	unpack_tmp = NULL; //CHECKME fix crash when repeated load/unload ?

	return ret;
}
