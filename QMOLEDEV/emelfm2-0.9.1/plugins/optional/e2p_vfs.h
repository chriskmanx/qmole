/* $Id:$

Copyright (C) 2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2, which is free software. You can redistribute it
and/or modify it under the terms of the GNU General Public License as published
by the Free Software Foundation - either version 3, or (at your option) any
later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file plugins/optional/e2p_vfs.h
@brief header for vfs plugin
*/

#ifndef __E2P_VFS_H__
#define __E2P_VFS_H__

#include "emelfm2.h"
#include "e2_plugins.h"

//interface type, a pointer to which is castable to/from Plugin[Iface]*
typedef struct _VfsIface
{
	PluginIface pdata;
	//data
	//TODO enable these data caches to also be validly updated from any inheriting interface e.g. gvfs plugin
	gint pane1_spacecurrent;	//index of current space in pane1_spacehistory
	GList *pane1_spacehistory;	//list of used-in-session PlaceInfo's, data shared with vmtab
	gint pane2_spacecurrent;
	GList *pane2_spacehistory;	//list of used-in-session PlaceInfo's, data shared with vmtab
	GList *vmtab;				//list of PlaceInfo's for mounted or mounting points
								//not cached as the data is too complex
	GHashTable *vdir_cache;		//table of VPATH's for places used in the session,
								//	each with 0x1 data
	GtkListStore *vtab;			//store for history/bookmarks data
	//methods
	void (*show_adjustdialog) (E2_VFSDialogType, ViewInfo *);
	void (*show_historydialog) (ViewInfo *);
	void (*create_placesmenu) (GtkWidget *, ViewInfo *, gboolean);
	gboolean (*set_space) (ViewInfo *, PlaceInfo *, gboolean);
	gboolean (*set_namedspace) (ViewInfo *, const gchar *);
	gboolean (*start_operation) (PlaceInfo *, off_t, E2_VFSMonitor *);
	gboolean (*finish_operation) (E2_VFSMonitor *);
	void (*clean_history) (void);
} VfsIface;

#define VFS_IFACE(d) (VfsIface*)(d)

//for checking presence of a function in the interface, after the latter has been populated
#define IFACE_OFFSET(d,func) ((gpointer)&((VfsIface*)d)->func - (gpointer)d)

/* operations for each specific handler's interface
	gboolean (*mount) (VPATH *, E2_VFSMonitor *);
	gboolean (*unmount) (VPATH *, E2_VFSMonitor *);
//	gboolean (*monitor) (VPATH *);	CHECK is this only useful for local files ?
//	gboolean (*demonitor) (VPATH *);
	gint (*stat) (VPATH *,  struct stat *, E2_VFSMonitor *); //looks through links
	gint (*lstat) (VPATH *,  struct stat *, E2_VFSMonitor *); //?? valid for vfs ?
//	gpointer fstat; invalid without handles
	gint (*access) (VPATH *, gint, E2_VFSMonitor *);	//looks through links
	gint (*laccess) (VPATH *, gint, E2_VFSMonitor *);	//no look through links
//	gpointer open; useless without handles and/or streams
//	gpointer fopen; invalid without handles and/or streams (local = open stream)
//	gpointer close; useless without handles and/or streams
//	gpointer fclose; invalid without handles (local = close stream)
	gboolean (*readfile) (VPATH *, gpointer *, gulong *, gboolean, E2_VFSMonitor *);	//aka get (into memory)
//	gpointer readblock;	//get a buffer's worth (covers sniff) useless without handles and/or streams
	gboolean (*writefile) (VPATH *, gpointer, size_t, mode_t, E2_VFSMonitor *);	//aka put (from memory)
//	gpointer writeblock;	//write a buffer's worth useless without handles and/or streams
//	gpointer writestring;	//write string useless without handles and/or streams
//	gpointer chdir;		//what "default" dir for non-native-filesystems ?
//	gpointer opendir; useless when can only get whole dir at once
//	gpointer closedir; useless when can only get whole dir at once
	gpointer (*readdir) (VPATH *, gpointer, gpointer, GDestroyNotify, E2_VFSMonitor *);	//enumerate dir contents into a list of names
	gboolean (*mkdir) (VPATH *, mode_t, E2_VFSMonitor *);
	gboolean (*parentdir) (VPATH *);	//get parent path for cd .. etc
	gboolean (*copy) (VPATH *, VPATH *, gboolean, E2_VFSMonitor *);	//can be 2 namespaces
	gboolean (*move) (VPATH *, VPATH *, gboolean, E2_VFSMonitor *);	//can be 2 namespaces
	gboolean (*rename) (VPATH *, VPATH *, E2_VFSMonitor *);	//NOT same as move
	gboolean (*symlink) (VPATH *, VPATH *, E2_VFSMonitor *);
	gboolean (*readlink) ();	//CHECKME ultimate target too ?
	gboolean (*chmod) (VPATH *, mode_t, E2_VFSMonitor *);
	gboolean (*chown) (VPATH *, uid_t, gid_t, E2_VFSMonitor *);
	gboolean (*lchown) (VPATH *, uid_t, gid_t, E2_VFSMonitor *);	//CHECKME valid for vfs ?
	gboolean (*touch) (VPATH *, const struct utimbuf *, E2_VFSMonitor *);		//aka utime
	gboolean (*delete) (VPATH *, E2_VFSMonitor *);	//covers unlink, remove, rmdir
	void	 (*display) (ViewInfo *);	//pop up dialog showing fs layout (tree etc) generalise _e2_tree_dialog_run()
	gboolean (*check) (PlaceInfo *, guint);		//check if this handler supports a nominated protocol and/or operation
	gboolean (*prefunc) (void);	//gboolean (*starter) (void) if present, to be called before any other function except postfunc
	gboolean (*postfunc) (void);	//gboolean (*ender) (void) if present, to be called after any other function except prefunc
*/

#endif //ndef __E2P_VFS_H__
