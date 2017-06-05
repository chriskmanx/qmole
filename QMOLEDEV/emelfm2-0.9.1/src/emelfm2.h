/* $Id: emelfm2.h 2999 2014-01-17 09:29:12Z tpgww $

Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>

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

#ifndef __EMELFM2_H__
#define __EMELFM2_H__
//see also: Makefile has build-time conditional #defines and compiler parameters
#define _GNU_SOURCE
//default 64-bit functions (handle files > 2GB) (also in Makefile)
#define _FILE_OFFSET_BITS 64
//some of the Makefile define's are transferred in this header
#include "build.h"

#include <sys/types.h>
#include <inttypes.h>
//needed for various statbuf declarations
#include <sys/stat.h>
#include <limits.h>
//some general things ...
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

//#define GTK_DISABLE_DEPRECATED see Makefile
#include <gtk/gtk.h>

//#define E2_ASSISTED see build.h
#ifdef E2_ASSISTED
# include <atk/atk.h>
#endif

#ifdef E2_CURRENTLIBS
//include extra/different code valid only for glib/gtk versions > minimum standard
//(2.6 or 3.0), according to the version in use when compiling
# if GLIB_CHECK_VERSION (2,8,0)
#  define USE_GLIB2_8
# endif
# if GLIB_CHECK_VERSION (2,10,0)
#  define USE_GLIB2_10
# endif
# if GLIB_CHECK_VERSION (2,12,0)
#  define USE_GLIB2_12
# endif
# if GLIB_CHECK_VERSION (2,14,0)
#  define USE_GLIB2_14
# endif
# if GLIB_CHECK_VERSION (2,16,0)
#  define USE_GLIB2_16
# endif
# if GLIB_CHECK_VERSION (2,18,0)
#  define USE_GLIB2_18
# endif
# if GLIB_CHECK_VERSION (2,20,0)
#  define USE_GLIB2_20
# endif
# if GLIB_CHECK_VERSION (2,22,0)
#  define USE_GLIB2_22
# endif
# if GLIB_CHECK_VERSION (2,24,0)
#  define USE_GLIB2_24
# endif
# if GLIB_CHECK_VERSION (2,26,0)
#  define USE_GLIB2_26
# endif
# if GLIB_CHECK_VERSION (2,28,0)
#  define USE_GLIB2_28
# endif
# if GLIB_CHECK_VERSION (2,30,0)
#  define USE_GLIB2_30
# endif
# if GLIB_CHECK_VERSION (2,32,0)
#  define USE_GLIB2_32
# endif
# if GLIB_CHECK_VERSION (2,34,0)
#  define USE_GLIB2_34
# endif
# if GLIB_CHECK_VERSION (2,36,0)
#  define USE_GLIB2_36
# endif
# if GLIB_CHECK_VERSION (2,38,0)
#  define USE_GLIB2_38
# endif
# if GTK_CHECK_VERSION (2,8,0)
#  define USE_GTK2_8
# endif
# if GTK_CHECK_VERSION (2,10,0)
#  define USE_GTK2_10
# endif
# if GTK_CHECK_VERSION (2,12,0)
#  define USE_GTK2_12
# endif
# if GTK_CHECK_VERSION (2,14,0)
#  define USE_GTK2_14
# endif
# if GTK_CHECK_VERSION (2,16,0)
#  define USE_GTK2_16
# endif
# if GTK_CHECK_VERSION (2,18,0)
#  define USE_GTK2_18
# endif
# if GTK_CHECK_VERSION (2,20,0)
#  define USE_GTK2_20
# endif
# if GTK_CHECK_VERSION (2,22,0)
#  define USE_GTK2_22
# endif
# if GTK_CHECK_VERSION (2,24,0)
#  define USE_GTK2_24
# endif
# if GTK_CHECK_VERSION (3,0,0)
#  define USE_GTK3_0
# endif
# if GTK_CHECK_VERSION (3,2,0)
#  define USE_GTK3_2
# endif
# if GTK_CHECK_VERSION (3,4,0)
#  define USE_GTK3_4
# endif
# if GTK_CHECK_VERSION (3,6,0)
#  define USE_GTK3_6
# endif
# if GTK_CHECK_VERSION (3,8,0)
#  define USE_GTK3_8
# endif
# if GTK_CHECK_VERSION (3,10,0)
#  define USE_GTK3_10
# endif
# if GTK_CHECK_VERSION (3,12,0)
#  define USE_GTK3_12
# endif
# if GTK_CHECK_VERSION (3,14,0)
#  define USE_GTK3_14
# endif
# if GTK_CHECK_VERSION (3,16,0)
#  define USE_GTK3_16
# endif

#elif defined(E2_MIN_GTK3)
//GTK3 is available and either: explicitly requested or GTK2 is not available
//include extra/different code valid only for gtk 3.0 and corresponding glib 2.28
# define USE_GLIB2_8
# define USE_GLIB2_10
# define USE_GLIB2_12
# define USE_GLIB2_14
# define USE_GLIB2_16
# define USE_GLIB2_18
# define USE_GLIB2_20
# define USE_GLIB2_22
# define USE_GLIB2_24
# define USE_GLIB2_26
# define USE_GLIB2_28
# define USE_GTK2_8
# define USE_GTK2_10
# define USE_GTK2_12
# define USE_GTK2_14
# define USE_GTK2_16
# define USE_GTK2_18
# define USE_GTK2_20
# define USE_GTK2_22
# define USE_GTK2_24
# define USE_GTK3_0
#endif

#define GETTEXT_PACKAGE BINNAME //before gi18n-lib.h

#ifdef USE_GLIB2_32
# include <glib/gi18n.h>
#else
# include <gmodule.h>
# include <glib/gi18n-lib.h>
#endif

#ifdef USE_GTK2_12
//this tag governs use of gtk 2.12 tooltips
# define USE_GTK2_12TIPS
//this tag governs use of gtk 2.12 notebook-tab DnD
# define USE_GTK2_12DND
#endif

#ifdef USE_GTK3_0
# include <gdk/gdkkeysyms-compat.h>
# define GTK3_COMBO_FIX //include workarounds for gtk3.0.x bad behaviour
#else
# include <gdk/gdkkeysyms.h>
#endif

//install stock icons only for gtk 3.10+
#ifdef E2_ADD_STOCKS
# ifndef USE_GTK3_10
#  undef E2_ADD_STOCKS
# endif
#endif

//longest item name (bytes, NOT chars) is a *NIX define
//#define NAME_MAX 255
//length of various buffers
#define E2_MAX_LEN 1024
//msec interval between pane-directory polls
//NOTE that the actual refresh interval may be a multiple (up to 3) times this
//to avoid too many refreshes during e.g. archive creation
#define E2_FILESCHECK_INTERVAL 1000
#define E2_FILESCHECK_INTERVAL_S 1
//msec interval between config-directory polls
//this is only used if E2_FAM not defined
#define E2_CONFIGCHECK_INTERVAL 5000
#define E2_CONFIGCHECK_INTERVAL_S 5
//msec interval between status line updates
#define E2_STATUSREPORT_INTERVAL 1500

//allowed msec interval between clicks to be treated as a double
//if gtk's value is smaller, then it is set to this instead
#define E2_CLICKINTERVAL 330

//padding for various widgets
#define E2_PADDING_LARGE 8
#define E2_PADDING 5
#define E2_PADDING_SMALL 3
#define E2_PADDING_XSMALL 2

#define E2_COMMAND_PREFIX "."G_DIR_SEPARATOR_S
#define PLUGIN action_labels[16]
// in these, "home" is a pointer determined at runtime
#define E2_CONFIG_DIR home,"."BINNAME
//#define E2_TRASH_DIR E2_CONFIG_DIR, "Trash"
//#define SYSTEM_TRASH_DIR home, ".Trash"
//#define E2_CONFIG_FILE "config"  no good for gettext ?? do not translate

//default build with horizontal panes - see Makefile
//#define E2_PANES_HORIZONTAL
//default pointsize and file-extension for custom toolbar-icon files
#define E2ICONTB "_48.png"
//default pointsize and file-extension for custom dialog-button-icon files
#define E2ICOND "_32.png"

//tag for development of on-demand status-bar updates
//NOTE need to check all BGL effects
//#define E2_STATUS_DEMAND
#ifndef E2_STATUS_DEMAND
//tag for refcounting enable/disable status-bar updates
//#define E2_STATUS_REF
//allow checking for status-in progess when updating filelists
#define E2_STATUS_BLOCK
#endif

//include error-message code regardless of debug state
//#define DEBUG_MESSAGES_ALWAYS

#if defined(DEBUG_MESSAGES) || defined(DEBUG_MESSAGES_ALWAYS)
# define printall(str, args...)
#else
# define printall(str, args...) printf("e2: "str"\n", ## args)
#endif

#ifndef PRIu64
//for systems that don't define PRI??? macros
# if defined(__WORDSIZE) && __WORDSIZE == 64
#  define PRIu64 "lu"
# else
//guess it's a 32-bit platform
#  define PRIu64 "llu"
# endif
#endif

//output-pane tabs on the same side as scrollbars
//#define ADJACENT_TABS

//setup for kernel-based file monitoring, as opposed to fam/gamin
//see Makefile & build.h for definition of these variables
#ifdef E2_GAMIN
//include code for file-change detection using gamin
# include <fam.h>
#endif
#if defined(E2_FAM_INOTIFY) || defined(E2_FAM_KQUEUE) || defined(E2_FAM_PORTEVENT)
# define E2_FAM_KERNEL
#endif
#if defined(E2_GAMIN) || defined(E2_FAM_KERNEL)
//this is needed too, ATM
# define E2_FAM
//usec interval after file events before FAM is polled
//needs to be long enough for most/all changes to be noticed
//by the monitor, but not so long that the user perceives a delay
# define E2_FAMWAIT 100000
#endif

//include code for making incremental changes to
//file lists, instead of dump & refill
//DOES NOT WORK
//#define E2_INCLIST

#define E2_BLOCK
#define E2_UNBLOCK

//include debug messages about refreshing
#define E2_REFRESH_DEBUG
//include debug messages about mkdir dialog shutdown
//#define RACE_CHECK

//#ifndef USE_GTK2_10
//flag for code which enables something like the pre-0.1 approach to selecting
//items by dragging
//for gtk >= 2.10, this could be done by rubber-banding, but that's not so flexible
#define E2_ALTLEFTMOUSE
//#endif

//include code for user-defined pointer buttons (and in future, gestures)
//#define E2_MOUSECUSTOM see Makefile and build.h
#ifdef E2_MOUSECUSTOM
//enable button.fake action NOT TESTED
//# define WITH_BUTTONFAKE
//suport pointer gestures
#define E2_PTRGESTURES
#endif

//enable workaround for bad behaviour of [g]libc symlink() with relative links
#define RELLINK_FIX

//enable key.fake action LITTLE TESTED
//#define WITH_KEYFAKE

//support addition of categories to bindings tree option data
//#define E2_TRANSIENTBINDINGS see Makefile and build.h for this
//support addition of treestore iters from strings like 1.2.3, esp for transient bindings
//#define E2_TREEINCREMENT

//enable action to brute-force-terminate a selected running command or action
//#define WITH_TASKABORT
//enable file.untrash action LITTLE TESTED
//#define WITH_UNTRASH

//multi-colored filelists
#define E2_RAINBOW

#ifdef E2_RAINBOW
//no. of color values in each allocated block of memory
#define ATOMSPERCHUNK 10
# ifndef USE_GLIB2_10
typedef struct _E2_ColorData
{
	GMemChunk *chunk;	//data structure for a block of ATOMSPERCHUNK GdkColor structs
	GdkColor **pointers;	//allocated space for ATOMSPERCHUNK pointers
} E2_ColorData;
# endif
#endif

//basename of upgrade plugin
#define UPGRADE_PNAME "e2p_upgrade.so"

//maybe useful for multi-processor optimisation ?
//#define VOLATILE volatile
#define VOLATILE

//interval between last-detected 'dirty' and suspension of refresh-poll timers
//20 mins = 1200 seconds
#define QUIET_SECONDS 1200

//include code for handling [%]%e [%]%E macros
#define E2_BADQUOTES

//include code for providing an additional filelist-column showing extension
//#define E2_EXTCOL

//support mountpoint-related capabilities
#define E2_FS_MOUNTABLE

//support detachable tabs for gtk >= 2.10
#ifdef USE_GTK2_10
#define E2_TABS_DETACH
#endif

#define E2_OVERRIDE_MASK (GDK_SHIFT_MASK|GDK_CONTROL_MASK|GDK_MOD1_MASK)
#ifdef USE_GTK2_10
# define E2_MODIFIER_MASK \
	(GDK_SHIFT_MASK|GDK_CONTROL_MASK|GDK_MOD1_MASK\
	|GDK_MOD3_MASK|GDK_MOD4_MASK|GDK_MOD5_MASK\
	|GDK_SUPER_MASK|GDK_HYPER_MASK|GDK_META_MASK)
#else
# define E2_MODIFIER_MASK \
	(GDK_SHIFT_MASK|GDK_CONTROL_MASK|GDK_MOD1_MASK\
	|GDK_MOD3_MASK|GDK_MOD4_MASK|GDK_MOD5_MASK)
#endif

//use replacement command-running (see Makefile)
//NOT FINISHED
//#ifndef E2_NEW_COMMAND
//#define E2_NEW_COMMAND
//#endif

//enable filesystem-tree-navigation dialogs
#define E2_TREEDIALOG

//support app-specific window translucence NOT WORKING FOR gtk < 2.12
//anyhow, good enough for the window-manager to do this equally for all apps
//#define E2_COMPOSIT see Makefile
#ifdef E2_COMPOSIT
# ifdef USE_GTK2_12
//minimum opacity % for dialogs
#  define DIALOG_OPACITY_LEVEL 95
# else
#  undef E2_COMPOSIT
# endif
#endif

//tailor UI things for very small screen
//#define E2_SMALLSCREEN

//support vfs for file/dir processing
//NOT FINISHED
//needs cacheing of view data, backend data structs, history, marks
//lib interrogation, task backends etc etc
//#define E2_VFS see Makefile
//transition management
#ifdef E2_VFS
//# define E2_VFSTMP
# ifdef E2_VFSTMP
#  define E2_VFSTMPOK
# endif
#endif

//use libmagic instead of file(1)
#define E2_MAGIC
#ifdef E2_MAGIC
//for use with dlopen()
# define MAGIC_LIB_NAME "libmagic.so.1"
#endif

//enable filelist liststore copying instead of getting fresh data from source
#ifdef E2_VFS
#define STORECOPY
#endif

//support for custom error handling with vfs
#ifdef E2_VFS
# define E2_ERR_NAME __sys_err
# define E2_ERR_INIT __sys_err = NULL;
# define E2_ERR_DECLARE GError *__sys_err = NULL;
# define E2_ERR_CLEARBACKUP(a) if (a != NULL) g_error_free (a);
//in these, there should never be any "a", we just need to get the leading comma
# define E2_ERR_NONE(a) a, NULL
# define E2_ERR_ARG(a) a, GError **__sys_err
# define E2_ERR_SAMEARG(a) a, __sys_err
# define E2_ERR_PTR(a) a, &__sys_err
# define E2_ERR_MSGC(a) a, (*__sys_err)->message
//these are for GError *, not for GError **
# define E2_ERR_MSGL(a) a, __sys_err->message
# define E2_ERR_IS(num) (__sys_err != NULL && __sys_err->code == num)
# define E2_ERR_ISNOT(num) (__sys_err == NULL || __sys_err->code != num)
# define E2_ERR_CLEAR if (__sys_err != NULL) { g_error_free (__sys_err); __sys_err = NULL; }
//these are for use in a func with GError ** argument
# define E2_ERR_PIS(num) (__sys_err != NULL && (*__sys_err)->code == num)
# define E2_ERR_PISNOT(num) (__sys_err == NULL || (*__sys_err)->code != num)
# define E2_ERR_BACKUP(a) GError *a = NULL;if (__sys_err == NULL) __sys_err = &a;
# define E2_ERR_PCLEAR if (__sys_err != NULL && *__sys_err != NULL) { g_error_free (*__sys_err); *__sys_err = NULL; }
#else
# define E2_ERR_NAME
# define E2_ERR_INIT
# define E2_ERR_DECLARE
# define E2_ERR_CLEARBACKUP(a)
# define E2_ERR_NONE(a)
# define E2_ERR_PTR(a)
# define E2_ERR_ARG(a)
# define E2_ERR_SAMEARG(a)
# define E2_ERR_MSGC(a)
# define E2_ERR_MSGL(a)
# define E2_ERR_IS(num) (errno==num)
# define E2_ERR_ISNOT(num) (errno!=num)
# define E2_ERR_CLEAR
# define E2_ERR_PIS(num) (errno==num)
# define E2_ERR_PISNOT(num) (errno!=num)
# define E2_ERR_PCLEAR
# define E2_ERR_BACKUP(a)
#endif

//support for tailored argument(s) for fs functions
#ifdef E2_VFS
# define VPATH vpath
# define VPCSTR(p) *((const gchar**)p)
# define VPSTR(p) *((gchar**)p)
#else
# define VPATH const gchar
# define VPCSTR(p) p
# define VPSTR(p) (gchar*)p
#endif

#ifdef E2_VFS
# define CURRDIR curr_view->dir.path
# define OTHRDIR other_view->dir.path
# define VIEWDIR view->dir.path
#else
# define CURRDIR curr_view->dir
# define OTHRDIR other_view->dir
# define VIEWDIR view->dir
#endif

#define ActivateFunc void(*)(GtkEntry*,gpointer)
#define ResponseFunc void(*)(GtkDialog*,gint,gpointer)
#define HookFunc gboolean(*)(gpointer,gpointer)

//type of "dir" in a pane
typedef enum
{
	FS_LOCAL   = 0,		//default, mounted-local dir
	FS_FUSE    = 1,		//virtual local dir that needs special handling
	FS_REMOTE  = 1 << 1,//virtual non-mounted remote dir
	FS_ARCHIVE = 1 << 2,//archive (can be local or remote)
	FS_KERNEL  = 1 << 3,//a namespace in a multi-space local filesystem
	FS_SYNTH   = 1 << 4,//a synthesised virtual dir e.g. from a search process or metadata derived
} E2_FSType;
//types of virtual dir that are non-native, and warrant timeout if misbehaved
#define FS_SLOW (FS_FUSE | FS_REMOTE | FS_ARCHIVE)

#ifdef E2_VFS

typedef enum { E2PLACE_UNMOUNTED, E2PLACE_MOUNTING, E2PLACE_MOUNTED } PlaceReady;
typedef struct _E2_PlaceInfo
{
	struct _E2_PlaceInfo* parentplace;	//for cd .. past root, checking availability of full path etc
	gchar *priv_name; /* for mounted dir NULL CHECKME synth dir
	   for remote site escaped utf8, no trailer, login-ready URI before the filepath proper with p/w etc
	   for archive localised absolute path, with trailer, of temp dir used for unpacking whole or part
	   constructed from vtab data (not stored there, it may have P/W text in URI)
	*/
	gchar *tip; /* for mounted dir: NULL CHECKME synth dir
	   for remote site: utf8, no trailer, public components of 1st part of URI, before the filepath proper
	   for archive: utf8, no trailer, pseudo-path of archive, with all ancestor(s) including remote URI and other archive
	*/
	gchar *plain_pw;	//edit-ready password, utf8, or NULL if no P/W
	gchar *alias;	//user-friendly short-form utf8 for menu label etc, NULL or "" if not in use
	gchar *workplace;	//path of local fs dir, absolute localised no trailer
	E2_FSType dirtype;	//flags for broad type of namespace this is
	//CHECKME data for specific type e.g. scheme
//UNUSED	E2_FSHandler handler;	//enumerator of lib/plugin which handles fs-operations for this space
									//CHECK could be op-specific
//UNUSED	E2_VFSOpFlags cando;	//flags for the sorts of operations that can be performed
	PlaceReady mountstate;	//this place has been initialised by its backend handler
						//(maybe redundant if only mounted places can have a PlaceInfo)
	gboolean monitored;	//alteration monitoring in effect for this place (even if just polling)
	//directory histories for this namespace
	GList *pane1_entered; //pane 1 dirline history, specific to current namespace
	GList *pane2_entered; //pane 2 dirline history, specific to current namespace
	GList *pane1_history; //pane1 dir_history, but specific to this namespace
	GList *pane2_history; //pane2 dir_history, but specific to this namespace
	GList *pane1_visited; //pane1 visited, opendirs, but specific to this namespace
	GList *pane2_visited; //pane2 visited, opendirs, but specific to this namespace
	//etc
} PlaceInfo;

//data for vfs operations uning path-argument(s)
typedef struct _E2_Vpath
{
	const gchar *path; /*first in the struct, so that vpath* == gchar **
						typically an absolute path string
						encoding may be localised or UTF-8 */
	PlaceInfo *spacedata;
//	GError *err; //replacement for error args in fs functions
} vpath;
#endif

#include "debug.h"
#include "e2_pane.h"
#include "e2_fileview.h"
#include "e2_alias.h"
#include "e2_bookmark.h"
#include "e2_button.h"
#include "e2_cache.h"
#include "e2_cl_option.h"
#include "e2_combobox.h"
#include "e2_command.h"
#include "e2_command_line.h"
#include "e2_fs.h"
#include "e2_hook.h"
#include "e2_keybinding.h"
#include "e2_list.h"
#include "e2_menu.h"
#include "e2_output.h"
#include "e2_toolbar.h"
#include "e2_tree.h"
#include "e2_utf8.h"
#include "e2_utils.h"
#include "e2_widget.h"
#include "e2_window.h"

//helper/utility macros

#define ITEM_ISHIDDEN(expr) (expr[0]=='.')
//#define ITEM_ISHIDDEN(expr) (*expr=='.')

// development-phase string tagger
// search for _I( for general strings that need attention
#define _I(d) d

#define RUN_ONCE_CHECK(ret) \
	static gboolean run_once_check_init = FALSE; \
	if (run_once_check_init) \
		return ret; \
	else \
		run_once_check_init = TRUE;

// TEMP_FAILURE_RETRY for systems which do not have it
//uses errno so is valid for local operations only
#ifndef TEMP_FAILURE_RETRY
#define TEMP_FAILURE_RETRY(expr) \
    ({ glong _res; \
       do _res = (glong) (expr); while (_res == -1L && errno == EINTR); \
       _res; })
#endif

//do not implement slices everywhere until comprehensive session-end cleanups are done
#ifdef USE_GLIB2_10
# define ALLOCATE(type) (type*)g_slice_alloc(sizeof(type));
# define ALLOCATE0(type) (type*) g_slice_alloc0(sizeof(type));
# define DEALLOCATE(type,ptr) g_slice_free1(sizeof(type),ptr);
#elif defined (USE_GLIB2_8)
# define ALLOCATE(type) (type*)g_try_malloc(sizeof(type));
# define ALLOCATE0(type) (type*)g_try_malloc0(sizeof(type));
# define DEALLOCATE(type,ptr) g_free(ptr);
#else
# define ALLOCATE(type) (type*)malloc(sizeof(type));
# define ALLOCATE0(type) (type*)calloc(1,sizeof(type));
# define DEALLOCATE(type,ptr) free(ptr);
#endif
//for ALLOCATE's that are too small for a slice
#ifdef USE_GLIB2_8
# define MALLOCATE(type) (type*)g_try_malloc(sizeof(type));
# define MALLOCATE0(type) (type*)g_try_malloc0(sizeof(type));
# define DEMALLOCATE(type,ptr) g_free(ptr);
#else
# define MALLOCATE(type) (type*)malloc(sizeof(type));
# define MALLOCATE0(type) (type*)calloc(1,sizeof(type));
# define DEMALLOCATE(type,ptr) free(ptr);
#endif

#ifdef USE_GLIB2_8
# define NEW(type,n) g_try_new(type,n)
# define NEW0(type,n) g_try_new0(type,n)
# define FREENEW g_free
#else
# define NEW(type,n) (type*)malloc(sizeof(type)*n)
# define NEW0(type,n) (type*)calloc(n,sizeof(type))
# define FREENEW free
#endif

#ifdef USE_GLIB2_30
# define g_atomic_int_exchange_and_add g_atomic_int_add
#endif

//survive or exit gracefully when allocation fails
//can't implement this now, some contexts wrongly assume BGL closed - check g_try_malloc() uses
#if 0
# define CHECKALLOCATEDFATAL(p) if (p==NULL) e2_utils_memory_error ();
# define CHECKALLOCATEDWARN(p,more) if (p==NULL) {e2_utils_show_memory_message (); more;}
# define CHECKALLOCATEDFATALT(p) if (p==NULL) {CLOSEBGL e2_utils_memory_error(); OPENBGL}
# define CHECKALLOCATEDWARNT(p,more) if (p==NULL) {CLOSEBGL e2_utils_show_memory_message();OPENBGL more;}
#else
# define CHECKALLOCATEDFATAL(p)
# define CHECKALLOCATEDWARN(p,more)
# define CHECKALLOCATEDFATALT(p)
# define CHECKALLOCATEDWARNT(p,more)
#endif

//index enumerator for cancellable timers
//CHECKME some other repeating, non-trivial-delay, timers are non-static
//NOTE also need to kill any [un]mount timers for devices(s) in hal/devkit
enum
{
	DIRTYCHECK_T,	//for polling whether filelists are in need of refresh
	CONFIG_T,	//for config-file change polling
	REFRESHBEGIN_T,	//for ensuring open BGL when calling _e2_filelist_refresh_manage()
	REFRESHWAIT_T,	//for pausing until a prior refresh is completed
	CDWAIT_T,	//for pausing until a prior cd is completed
	STATUS_T,	//for status-line updates
	ASCROLL_T,	//DnD auto-scroll timer
	MAX_TIMERS
};

#ifdef E2_FAM
typedef enum
{
	E2_MONITOR_DEFAULT,
	E2_MONITOR_FAM,	//unused
	E2_MONITOR_GAMIN
} E2_FAMonitor;
#endif

//define to use local polling instead of full-blown main loop for blocking dialogs
#define WAIT_POLL

typedef struct _E2_MainLoop
{
	pthread_t threadID;	//thread which started this loop
#ifdef WAIT_POLL
	gboolean maincontext; //TRUE when wait was initiated from main thread, in
						// which case the watching is done by separate, dedicated, thread
	gboolean finished;	//TRUE when the wait is ready to be ended
#else
	GMainLoop *loop;
#endif
} E2_MainLoop;

typedef struct _E2_MainData
{
	GtkWidget *main_window;
	//pair of main boxes that can be surrounded by toolbars
	GtkWidget *hbox_main;
	GtkWidget *vbox_main;
	GtkWidget *outbook; //notebook container for tabbed output-panes
	GtkWidget *status_bar_label2;
	//this is a hbox, at the right of the status bar, for general usage
	// needs to be shown when used
	GtkWidget *status_bar_box3;
	E2_WindowRuntime window;
	E2_PaneRuntime pane1;	//includes ViewInfo
	E2_PaneRuntime pane2;	//ditto
	E2_ToolbarRuntime toolbar;
	E2_ToolbarRuntime commandbar;
	E2_AliasRuntime aliases;
	E2_OutputRuntime output;//non-tab-specific data for output pane
	E2_OutputTabRuntime tab;//tab-specific data for output pane
	GList *tabslist;		//list of E2_OutputTabRuntime's for each tab
	gint tabcount;			//no. of tabs (cached)
	E2_ToolbarData **bars;	//NULL-terminated array of pointers to toolbar data
	GList *command_lines;	//list of rt data structs of command-lines

#if defined(E2_FAM) && !defined(E2_FAM_KERNEL)
	FAMConnection *fcp;
	FAMEvent *fep;
#endif
#ifdef E2_FAM
	E2_FAMonitor monitor_type;
	gint FAMreq;			//request no used for monitoring config file changes
#endif
	guint timers[MAX_TIMERS];//glib timer id's
	time_t config_mtime;	//last-logged (seconds) timestamp of config file
#ifndef USE_GTK2_12TIPS
	GtkTooltips *tooltips;
	gint gtkversion;		//runtime version, = major * 10000 + minor * 100 + micro
#endif
	GHashTable *dir_history;//table of E2_DirHistoryEntry's for dirs opened in session
	GHashTable *keysnative;	//table of keycodes for asciifying localised alphabetic
							//bindings, each index is a pointerised locale-specific
							//keycode, each corresponding value is a pointerized
							//lowercase code GDK_a .. GDK_z
	GHashTable *keyslocal;	//table of strings for localising alphabetic key-bindings,
							//each index is a lowercase letter string "a" .. "z",
							//each corresponding value is a UTF-8 string
							//representing the corresponding localised letter
							//This table is not often used
	GHashTable *plugins;	//table of PluginIface's of loaded plugins
	GPtrArray *plugacts;	//array of PluginAction's for all loaded plugins, in menu-order
	GHashTable *filetypes;
	GSList *typelist;		//list of string arrays, needed tor cleanup
#ifdef E2_RAINBOW
	GHashTable *colors;
	GList *colorchunks;
#endif
	GSList *mainloops;			//local mainloop information in E2_MainLoop's
	GSList *used_stores;		//for deferred liststore clearing
	GList *taskhistory;
//#ifndef E2_FILES_UTF8ONLY
//	gboolean utf8_filenames;	//TRUE when filesystem coding is utf-8 or ascii only
//#endif
	GdkWindowState mainwindow_state;	//flags indicating changes to main_window
	GtkWidget *context_menu;	//main context-menu when it is popped up
#ifdef USE_GTK2_18
	GtkAllocation main_alloc;	//main window dimensions for cache
#endif
	GtkAllocation cfg_alloc;	//config dialog dimensions for cache
	gchar cfgfile_version[20];	//store for config version string eg "0.1.9", used only for upgrades
//	gboolean keytrans;			//TRUE for automatic translation next time keybindings are sync'd
	gboolean reconvert_requested; //TRUE when a change of encoding-status has been logged during a filelist refill
#ifdef E2_STATUS_BLOCK
	volatile gint status_working;	//1 to block reentrant status-checking etc
#endif
	GHookList hook_pane_focus_changed;
} E2_MainData;

E2_MainData app;
E2_PaneRuntime *curr_pane;
E2_PaneRuntime *other_pane;
ViewInfo *curr_view;
ViewInfo *other_view;
E2_OutputTabRuntime *curr_tab;	//currently-focused member of tabslist

pthread_mutex_t list_mutex;
#define LISTS_LOCK pthread_mutex_lock (&list_mutex);
#define LISTS_UNLOCK pthread_mutex_unlock (&list_mutex);
//CHECKME worth having a separate mutex for this ?
pthread_mutex_t history_mutex;
#define HISTORY_LOCK pthread_mutex_lock (&history_mutex);
#define HISTORY_UNLOCK pthread_mutex_unlock (&history_mutex);

//macros related to blocking simultaneous access to thread-unsafe UI backend (notably xlib)

//local management of gdk mutex, to enable tolerant re-locking by the same thread
//direct mutex-manipulation
pthread_mutex_t display_mutex;	//BGL replacement

#ifdef DEBUG_MESSAGES
# define CLOSEBGL e2_main_close_uilock ();
# define OPENBGL e2_main_open_uilock ();
# define OPENBGL_NAME e2_main_open_uilock
#else
# define CLOSEBGL pthread_mutex_lock (&display_mutex);
# define OPENBGL pthread_mutex_unlock (&display_mutex);
# define OPENBGL_NAME pthread_mutex_unlock
#endif

#ifdef USE_GTK3_6
//workaround for deprecated/lack-of internal X11 lock in gtk3.6+
//# define LOCAL_BGL
# ifdef LOCAL_BGL
#  define NEEDCLOSEBGL CLOSEBGL
#  define NEEDCLOSEBGLX CLOSEBGL //locks needing further attention
#  define NEEDOPENBGL OPENBGL
# endif
#endif

#ifndef NEEDCLOSEBGL
# define NEEDCLOSEBGL
# define NEEDCLOSEBGLX
# define NEEDOPENBGL
#endif

//not locked now and not already locked - this should never happen
//prevent unlock
#define CLOSEBGL_IF_OPEN \
	gint _lockres; \
	_lockres = pthread_mutex_trylock (&display_mutex); \
	printd (DEBUG, "%s BGL trylock result %d", __PRETTY_FUNCTION__, _lockres); \
	if (!(_lockres == 0 || _lockres == EBUSY)) \
	{ \
		printd (WARN, "close BGL failed"); \
		WAIT_FOR_EVENTS_UNLOCKED \
		_lockres = 1; \
	}

#define OPENBGL_IF_CLOSED \
	if (_lockres == 0) \
		pthread_mutex_unlock (&display_mutex);

//#define NEWLOOP
#ifdef NEWLOOP
extern GMainContext *localctx;
# define DEFAULT_CONTEXT localctx
#else
# define DEFAULT_CONTEXT NULL
#endif
	
//gtk-versions of these macros block until at least one event is processed

#if 1
//this version of the macros seem be faster at session-start

//these 2 must be called with BGL closed
#define WAIT_FOR_EVENTS \
GMainContext *__ctx = g_main_context_default (); \
OPENBGL \
while (g_main_context_pending (__ctx)) { g_main_context_iteration (__ctx, TRUE); } \
CLOSEBGL
#define WAIT_FOR_EVENTS_SLOWLY \
GMainContext *__ctx = g_main_context_default (); \
OPENBGL \
while (g_main_context_pending (__ctx)) { g_main_context_iteration (__ctx, TRUE); usleep(5000); } \
CLOSEBGL
#define EXTRA_WAIT_FOR_EVENTS \
OPENBGL \
while (g_main_context_pending (__ctx)) { g_main_context_iteration (__ctx, TRUE); usleep(5000); } \
CLOSEBGL

//and these 2 are the corresponding ones with BGL open
#define WAIT_FOR_EVENTS_UNLOCKED \
GMainContext *__ctx = g_main_context_default (); \
while (g_main_context_pending (__ctx)) { g_main_context_iteration (__ctx, TRUE); }
#define WAIT_FOR_EVENTS_UNLOCKED_SLOWLY \
GMainContext *__ctx = g_main_context_default (); \
while (g_main_context_pending (__ctx)) { g_main_context_iteration (__ctx, TRUE); usleep(5000); }
#define GET_EVENTS_CONTEXT GMainContext *__ctx = g_main_context_default ();
#define EXTRA_WAIT_FOR_EVENTS_UNLOCKED \
while (g_main_context_pending (__ctx)) { g_main_context_iteration (__ctx, TRUE); usleep(5000); }



#else //other version

#define WAIT_FOR_EVENTS \
GMainContext *__ctx = g_main_context_default (); \
OPENBGL \
while (g_main_context_iteration (__ctx, FALSE)){} \
CLOSEBGL
#define WAIT_FOR_EVENTS_SLOWLY \
GMainContext *__ctx = g_main_context_default (); \
OPENBGL \
while (g_main_context_iteration (__ctx, FALSE)){usleep(5000);} \
CLOSEBGL

//and these 2 are the corresponding ones with BGL open
#define WAIT_FOR_EVENTS_UNLOCKED GMainContext *__ctx = g_main_context_default (); while (g_main_context_iteration (__ctx, FALSE)){}
#define WAIT_FOR_EVENTS_UNLOCKED_SLOWLY GMainContext *__ctx = g_main_context_default (); while (g_main_context_iteration (__ctx, FALSE)){usleep(5000);}
#define GET_EVENTS_CONTEXT GMainContext *__ctx = g_main_context_default ();
#define EXTRA_WAIT_FOR_EVENTS_UNLOCKED while (g_main_context_iteration (__ctx, FALSE)){}

#endif

void e2_main_init_uilock (gboolean free);
//void e2_main_cleanup_uilock (gpointer data);
#ifdef DEBUG_MESSAGES
void e2_main_close_uilock (void);
void e2_main_open_uilock (void);
#endif

#ifdef NEWLOOP
# define DISPLAYLOCK_PARENT GUINT_TO_POINTER(1)
# define DISPLAYLOCK_DEPREC GUINT_TO_POINTER(2)
void e2_main_level_run (GSourceLock *data);
void e2_main_level_quit (void);
#endif

E2_MainLoop *e2_main_loop_new (gboolean mainctx);
void e2_main_loop_run (E2_MainLoop *loopdata);
void e2_main_loop_quit (E2_MainLoop *loopdata);
gboolean e2_main_loop_abort (pthread_t ID);
gboolean e2_main_closedown (gboolean compulsory, gboolean saveconfig, gboolean doexit);
gboolean e2_main_user_shutdown (gpointer from, E2_ActionRuntime *art);

#endif //ndef __EMELFM2_H__
