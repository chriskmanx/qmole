/* $Id: e2p_find.c 2928 2013-11-14 07:29:25Z tpgww $

Copyright (C) 2005-2013 tooar <tooar@emelfm2.net>
Portions copyright (C) 1999 Matthew Grossman <mattg@oz.net>

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
// much of this code was originally sourced from gtkfind 1.1 by Matthew Grossman
/**
@file plugins/e2p_find.c
@brief file-find plugin

This file contains functions related to creation of a file-find dialog, and
execution of a find task in accord with options selected in that dialog.
Back-end capability is provided by shell commands, specifically
GNU grep and/or file. If available, some tracker search funtionality is provided.
This will not work on any virtual file system, unless that is mounted
*/

/* TODO
FIXME's
when API is stable and used for a while, remove checks for correct no. of
cached entry-strings
check tracker usage
*/

//mimetypes listed at www.iana.org/assignments/media-types

#include "emelfm2.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <grp.h>
#include <fnmatch.h>
#include <regex.h>
#include <time.h>
#ifdef TM_IN_SYS_TIME
# include <sys/time.h>
#endif
#ifdef E2_MAGIC
# include <magic.h>
# include <dlfcn.h>
#endif
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_task.h"
#include "e2_icons.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "find"

#define PAGE_DATAKEY "__book-child"
#define LABEL_DATAKEY "__tab-label"

//support mime-type search criteria
#define MIMEFIND
//miminum similarity % which is acceptable as a fuzzy match
#define E2_ENOUGHLIKE 50.0
//support finding content using tracker database, if that's available
#define TRACKERFIND

#ifdef E2_SMALLSCREEN
//notebook tabs at top, lef-align labels
# define LABEL_ALIGN 0.02
#else
# define LABEL_ALIGN 0.5
#endif

typedef enum
{
	SEARCH_ALL_P = 0, SEARCH_TRASH_P,
	SEARCH_ALLACTIVE_P, SEARCH_ALLINACTIVE_P, //for variable namespaces
 	SEARCH_CURRENT_P, SEARCH_OTHER_P, SEARCH_THIS_P,
	SEARCH_SUBDIRS_P, SEARCH_LINKS_P,

	STRING_FILENAME_P, WILDCARD_FILENAME_P, REGEXP_FILENAME_P, NOT_FILENAME_P,
	ANYCASE_FILENAME_P,

#ifdef MIMEFIND
	STRING_MIME_P, WILDCARD_MIME_P, NOT_MIME_P,
#endif

	STRING_CONTENT_P, WILDCARD_CONTENT_P, REGEXP_CONTENT_P, ANYCASE_CONTENT_P,
	TRACK_CONTENT_P,

	MODE_IS_P, MODE_OR_P, MODE_NOT_P,
//for decoding, these need to be in the same order as the flags in the permissions dialog
//"u+s", "g+s", "o+t", "u+r", "u+w", "u+x", "g+r", "g+w", "g+x", "o+r", "o+w", "o+x"
	SETUID_P, SETGID_P, STICKY_P,
	OWNER_READ_P, OWNER_WRITE_P, OWNER_EXEC_P,
	GROUP_READ_P, GROUP_WRITE_P, GROUP_EXEC_P,
	WORLD_READ_P, WORLD_WRITE_P, WORLD_EXEC_P,

	REGULAR_P, DIRECTORY_P, RAW_DEVICE_P, BLOCK_DEVICE_P, SYMLINK_P, SOCKET_P,
	FIFO_P, TYPE_IS_P, TYPE_NOT_P,

	UID_ANY_P, UID_SPECIFIC_P, UID_NONE_P, UID_LOGIN_P, UID_NOT_LOGIN_P,
	GID_ANY_P, GID_SPECIFIC_P, GID_NONE_P, GID_LOGIN_P, GID_NOT_LOGIN_P,

	FSIZE_LT_P, FSIZE_EQ_P, FSIZE_GT_P,
	FSIZE_B_P, FSIZE_KB_P, FSIZE_MB_P,

	MTIME_LT_P, MTIME_EQ_P, MTIME_GT_P,	MTIME_REL_P,
	ATIME_LT_P, ATIME_EQ_P, ATIME_GT_P,	ATIME_REL_P,
	CTIME_LT_P, CTIME_EQ_P, CTIME_GT_P,	CTIME_REL_P,

	MAX_FLAGS	//no. of flags in the array
} findflag_t;
//this is the value which starts the permissions flags
//used for decoding the flags to a mode number
#define PERMISSIONS1 SETUID_P

//we don't bother to cache the start-directory entry
enum
{
	NAME_ENTRY, CONTENT_ENTRY,
#ifdef TRACKERFIND
	CONTENT_ENTRY2,
#endif
	MIME_ENTRY, SIZE_ENTRY, UID_ENTRY, GID_ENTRY,
	MREL_ENTRY, AREL_ENTRY, CREL_ENTRY,
	MAX_ENTRIES
};

typedef enum
{
	ISNA, ISLT, ISLE, ISEQ, ISGE, ISGT, ISFROM, ISNE, WILD, LIKE, REGX, TRAK
} findoperator;

typedef struct _E2P_FindTargets
{
	gchar *nametarget;	//localised string for various uses
#ifdef MIMEFIND
	gchar *mimetarget;
#endif
	gchar *contenttarget; //UTF-8 encoding, from GtkEntry
	guint64 sizetarget;
	mode_t permtarget;
	uid_t usertarget;
	gid_t grouptarget;
	time_t mtimtarget;
	time_t atimtarget;
	time_t ctimtarget;
	guint typetarget;//can't store choices in a mode_t, that has shared bits

	findoperator nameop;
#ifdef MIMEFIND
	findoperator mimeop;
#endif
	findoperator contentop;
	findoperator sizeop;
	findoperator permop;
	findoperator userop;
	findoperator groupop;
	findoperator mtimop;
	findoperator atimop;
	findoperator ctimop;
	findoperator typeop;

	regex_t compiledname;
//	regex_t compiledcontent;
	guint preplen;	//length of nametarget when doing a fuzzy match
	gboolean name_anycase;	//for grep searches only
	gboolean content_anycase;	//ditto
	gboolean content_only;	//not looking for anything else
	gint searchdepth;
#ifdef TRACKERFIND
	gint tracker_service;	//index of service in cmd_str[]
#endif
#ifdef E2_VFS
	VPATH sdata;
#else
	gchar *localstartpath;	//NULL when searching for trash
#endif
	GList *dirdata;
#ifdef E2_VFS
	GError **operr;
#endif
	pthread_t findID;
	gboolean aborted;	//TRUE after STOP button clicked
} findtargets;

typedef struct _E2P_Spinners
{
	GtkWidget *day_spin;
	GtkWidget *month_spin;
	GtkWidget *year_spin;
	GtkWidget *hour_spin;
	GtkWidget *minute_spin;
} spinners;

typedef struct _E2_FindDialogRuntime
{
	GtkWidget *dialog;
	GtkWidget *notebook; //notebook, gui for the various search criteria
	GtkWidget *active_button;//radio-button for searching in current directory
	GtkWidget *thisdir_button; //radio-button for searching in directory-entry path
	GtkWidget *chooser_button;	//dir-chooser-button
	GtkWidget *recurse_button;	//check-button for searching in subdirs
	GtkWidget *inlink_button;	//check-button for searching in linked subdirs
	GtkWidget *directory; //entry with text for directory to search in/from
	GtkWidget *pattern;  //entry with text for file name to search for
#ifdef MIMEFIND
	GtkWidget *mime_entry;	//entry with user-specified mimetype
#endif
	GtkWidget *content_pattern; //entry with text for grepped file content
#ifdef TRACKERFIND
	GtkWidget *content_pattern2; //entry with text for tracked file content
	GtkWidget *service_combo;	//tracker-service picker
#endif
	GtkWidget *curr_user;
	GtkWidget *choose_user;
	GtkWidget *user_entry;	//entry with user-specified uid
	GtkWidget *curr_group;
	GtkWidget *choose_group;
	GtkWidget *group_entry;	//entry with user-specified uid
	GtkWidget *size_entry;	//entry with user-specified filesize
	GtkWidget *mrel_entry;
	GtkWidget *mrel_combo;
	GtkWidget *arel_entry;
	GtkWidget *arel_combo;
	GtkWidget *crel_entry;
	GtkWidget *crel_combo;
	GtkWidget *stop_button;  //button widget, remebered for changing sensitivity
	GtkWidget *start_button; //ditto
	GtkWidget *help_button;  //ditto
//	gboolean stop_flag;	//TRUE when stop button clicked in matchdata
	//spin buttons for file date/time parameters
	spinners mtime;
	spinners atime;
	spinners ctime;
	GSList *groups;  //list of lists of grouped toggle btns
	findtargets *matchdata;
//now static	gboolean flags[MAX_FLAGS];	//cache for toggle values
} E2_FindDialogRuntime;

typedef struct _E2P_TimeOffset
{
	glong days;
	gint minutes;
} offset;

typedef struct _E2P_DateTime
{
	gfloat day;
	gfloat month;
	gfloat year;
	gfloat hour;
	gfloat minute;
//	gfloat second;
} local_dt;

//tracker stuff, from various tracker 0.6.0 headers
/*
enum {
	SERVICE_FILES,
	SERVICE_FOLDERS,
	SERVICE_DOCUMENTS,
	SERVICE_IMAGES,
	SERVICE_MUSIC,
	SERVICE_VIDEOS,
	SERVICE_TEXT_FILES,
	SERVICE_DEVELOPMENT_FILES,
	SERVICE_OTHER_FILES,
	SERVICE_VFS_FILES,
	SERVICE_VFS_FOLDERS,
	SERVICE_VFS_DOCUMENTS,
	SERVICE_VFS_IMAGES,
	SERVICE_VFS_MUSIC,
	SERVICE_VFS_VIDEOS,
	SERVICE_VFS_TEXT_FILES,
	SERVICE_VFS_DEVELOPMENT_FILES,
	SERVICE_VFS_OTHER_FILES,
	SERVICE_CONVERSATIONS,
	SERVICE_PLAYLISTS,
	SERVICE_APPLICATIONS,
	SERVICE_CONTACTS,
	SERVICE_EMAILS,
	SERVICE_EMAILATTACHMENTS,
	SERVICE_APPOINTMENTS,
	SERVICE_TASKS,
	SERVICE_BOOKMARKS,
	SERVICE_WEBHISTORY,
	SERVICE_PROJECTS,
	MAXSERVICES
};
*/

static void _e2p_find_reset_spin_button (GtkWidget *widget);
static void _e2p_find_reset_entry (GtkWidget *widget);
static void _e2p_find_reset_combo (GtkWidget *widget);
static void _e2p_find_set_toggle_button_on (GtkToggleButton *button);
static void _e2p_find_set_toggle_button_off (GtkToggleButton *button);

#ifdef TRACKERFIND
#define ACTIVE_TRACKER_SERVICES 12

static PluginIface iface;

static gchar *cmd_str [ACTIVE_TRACKER_SERVICES] =
{	//these service names are hardcoded in tracker-files, and not translated
	//same order here as enum
	"Files",	//we're not really interested in files, this is used as a proxy for "anywhere"
//	NULL,	//not interested in "Folders",
	"Documents",
	"Images",
	"Music",
	"Videos",
	"Text",
	"Development",
	"Other",
//	NULL,	//SERVICE_VFS_FILES,
//	NULL,	//SERVICE_VFS_FOLDERS,
//	NULL,	//SERVICE_VFS_DOCUMENTS,
//	NULL,	//SERVICE_VFS_IMAGES,
//	NULL,	//SERVICE_VFS_MUSIC,
//	NULL,	//SERVICE_VFS_VIDEOS,
//	NULL,	//SERVICE_VFS_TEXT_FILES,
//	NULL,	//SERVICE_VFS_DEVELOPMENT_FILES,
//	NULL,	//SERVICE_VFS_OTHER_FILES,
	"Conversations",	//"GaimConversations",
//	NULL,	//SERVICE_PLAYLISTS,
	"Applications",
//	NULL,	//SERVICE_CONTACTS,
	"Emails",	//"EvolutionEmails" "KMailEmails",
	"EmailAttachments",	//	"EvolutionAttachments" 	"KMailAttachments",
//	NULL,	//SERVICE_APPOINTMENTS,
//	NULL,	//SERVICE_TASKS,
//	NULL,	//SERVICE_BOOKMARKS,
//	NULL,	//SERVICE_WEBHISTORY,
//	NULL,	//SERVICE_PROJECTS,
};

static gchar *object_names [ACTIVE_TRACKER_SERVICES] =
{ //these service strings are in same order as enum
	N_("all files"),	//not actually interested in "Files", this is a proxy
//	NULL,	//not interested in "Folders",
	N_("office documents"),
	N_("images"),
	N_("music"),
	N_("videos"),
	N_("text files"),
	N_("development files"),
	N_("other files"),
//	NULL,	//SERVICE_VFS_FILES,
//	NULL,	//SERVICE_VFS_FOLDERS,
//	NULL,	//SERVICE_VFS_DOCUMENTS,
//	NULL,	//SERVICE_VFS_IMAGES,
//	NULL,	//SERVICE_VFS_MUSIC,
//	NULL,	//SERVICE_VFS_VIDEOS,
//	NULL,	//SERVICE_VFS_TEXT_FILES,
//	NULL,	//SERVICE_VFS_DEVELOPMENT_FILES,
//	NULL,	//SERVICE_VFS_OTHER_FILES,
	N_("conversations"),
//	NULL,	//N_("playlists") SERVICE_PLAYLISTS,
	N_("applications"),
//	NULL,	//N_("people") SERVICE_CONTACTS,
	N_("emails"),	//"EvolutionEmails" "KMailEmails",
	N_("email attachments"),	//"EvolutionAttachments" 	"KMailAttachments",
//	NULL,	//N_("appointments") SERVICE_APPOINTMENTS,
//	NULL,	//N_("tasks") SERVICE_TASKS,
//	NULL,	//N_("bookmarks") SERVICE_BOOKMARKS,
//	NULL,	//N_("visited")SERVICE_WEBHISTORY,
//	NULL,	//N_("projects") SERVICE_PROJECTS,
};

static gint service_index = -1;	//undefined selection
//static GList *query_history = NULL;
#endif //def TRACKERFIND

#define PERIODCOUNT 4
static gchar *periods[PERIODCOUNT] =
{
	N_("hours"),
	N_("days"),
	N_("months"),
	N_("years")
};

//mutex to prevent racing when dialog is closed
pthread_mutex_t find_mutex;
static E2_FindDialogRuntime *find_rt;
static local_dt current;
static gboolean nocacheflags;
//session-cache for last-focused notebook page
static gint page_store;
//ditto for toggle values
static gboolean flags[MAX_FLAGS];
//ditto for entries' text (except the start-directory entry)
static gchar *entries[MAX_ENTRIES];
//real-cache data for entries' text
static GList *strings = NULL;

static gboolean _e2p_find_match1 (VPATH *localpath, const struct stat *statptr,
	findtargets *data);
static gpointer _e2p_find_dofind (E2_FindDialogRuntime *rt);
static void _e2p_find_cleanfind (E2_FindDialogRuntime *rt);
static void _e2p_find_make_name_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);
static void _e2p_find_make_size_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);
#ifdef MIMEFIND
static void _e2p_find_make_mimetype_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);
#endif
static void _e2p_find_make_content_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);
static void _e2p_find_make_atime_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);
static void _e2p_find_make_ctime_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);
static void _e2p_find_make_mtime_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);
static void _e2p_find_make_mode_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);
static void _e2p_find_make_type_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);
static void _e2p_find_make_owner_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt);

#define _e2p_find_create_vbox(box) e2_widget_add_box(box,TRUE,0,TRUE,FALSE,0)
#define _e2p_find_create_label(box,text) e2_widget_add_mid_label(box,text,0.5,FALSE,E2_PADDING_XSMALL)

static GtkWidget *_e2p_find_create_hbox (GtkWidget *box);
static GtkWidget *_e2p_find_create_combo (GtkWidget *box, gchar **history,
	gint histcount, gint histdefault);
static GtkWidget *_e2p_find_create_entry (GtkWidget *box, gchar *text);
//static GtkWidget *_e2p_find_create_button (GtkWidget *container,
//	void (*callback) (), gpointer callback_data);
static GtkWidget *_e2p_find_create_toggle_grouped_button (GtkWidget *container,
	findflag_t f, gboolean state, gchar *label, GtkWidget *leader,
	E2_FindDialogRuntime *rt);
static GtkWidget *_e2p_find_create_toggle_button (GtkWidget *box,
	findflag_t f, gboolean state, gchar *label, E2_FindDialogRuntime *rt);
static GtkWidget *_e2p_find_create_radio_button (GtkWidget *box, GtkWidget *leader,
	findflag_t f, gboolean state, gchar *label, E2_FindDialogRuntime *rt);
//static GtkWidget *_e2p_find_create_radio_grouped_button (GtkWidget *box, GtkWidget *leader,
//	findflag_t f, gboolean state, gchar *label, E2_FindDialogRuntime *rt);
static GtkWidget *_e2p_find_create_spin_button (gfloat *default_value, gdouble min_value,
	gdouble max_value);

static gboolean _e2p_find_check_leapyear (gint year);
static gboolean _e2p_find_get_datetime (time_t *result, spinners *times);
static void _e2p_find_get_current_datetime (local_dt *current);
static gboolean _e2p_find_get_flag (findflag_t f);	//, E2_FindDialogRuntime *rt);
static void _e2p_find_set_flag (findflag_t f, gboolean value);	//, E2_FindDialogRuntime *rt);
static void _e2p_find_make_all_spinners (GtkWidget *box, spinners *times);
static void _e2p_find_reset_flags (void);	//E2_FindDialogRuntime *rt);
static void _e2p_find_reset_entry (GtkWidget *widget);
static void _e2p_find_reset_spin_button (GtkWidget *widget);

  /*******************/
 /**** utilities ****/
/*******************/

#ifdef TRACKERFIND
/**
@brief conduct a search using tracker data

@param startpath localised absolute path of directory in which to "start searching"
@param searchdepth 1 when just looking in @a startpath, -1 otherwise
@param data pointer to match-data

@return
*/
static void _e2p_find_tracker_find (VPATH *startpath, gint searchdepth,
	findtargets *data)
{
	gpointer matches;
	gchar *s, *e, *command;
	/*tracker-files -s ServiceType gets all files in that type*/
	/*tracker-meta-folder path returns the list of files indexed by tracker and
       stored in path*/
	/*tracker-query  searches  documents  using  a query file (in rdf format)
       provided as parameter, and returns the documents matching the  criteria
       of the query file*/
	/* tracker-search searches for the expression provided as parameter in the
       list of documents indexed by trackerd*/

	if (data->contentop == TRAK)
	{
		//CHECKME is localising the UTF-8 UI string sufficient ?
		s = e2_utf8_to_locale (data->contenttarget);
		//CHECKME single-quoting of content target (as used for grep)
		command = g_strdup_printf ("tracker-search '%s'", s);
		g_free (s);
	}
	else //data->typeop == TRAK
		command = e2_utils_strcat ("tracker-files -s ", cmd_str [data->tracker_service]);

	if (e2_fs_get_command_output (command, &matches))
	{
		E2_ERR_DECLARE;
		struct stat sb;
#ifdef E2_VFS
		VPATH ddata;
		ddata.spacedata = startpath->spacedata;
#endif
		//for checking whether or not matched items are in startpath
		gint skip = (searchdepth == 1) ? strlen (VPSTR (startpath)) + sizeof (gchar):-1;
		s = (gchar *)matches;
		while (*s != '\0' && (e = strchr (s, '\n')) != NULL)
		{
			*e = '\0';
			if (g_str_has_prefix (s, VPSTR (startpath)) &&
				(skip == -1 || strchr (s+skip, G_DIR_SEPARATOR) == NULL))
			{
#ifdef E2_VFS
				ddata.path = s;
				if (!e2_fs_lstat (&ddata, &sb E2_ERR_PTR()))
#else
				if (!e2_fs_lstat (s, &sb E2_ERR_PTR()))
#endif
				{
					if (S_ISREG (sb.st_mode))
#ifdef E2_VFS
						_e2p_find_match1 (&ddata, &sb, data);
#else
						_e2p_find_match1 (s, &sb, data);
#endif
				}
				else if (!E2_ERR_IS (ENOENT))
#ifdef E2_VFS
					_e2p_find_match1 (&ddata, NULL, data);
#else
					_e2p_find_match1 (s, NULL, data);
#endif
			}
			s = e + sizeof (gchar);
		}
		g_free (matches);
	}
/*	//make a temporary rdf file
	gchar *localpath = "FIXME";
	E2_FILE *stream = e2_fs_open_writestream (localpath E2_ERR_NONE());
	gchar *rdfstring = "RULES";
	//write it
	e2_fs_put_stream (stream, rdfstring, localpath, E2_ERR_NONE());
	//close it, CHECKME pre-flush it ?
	e2_fs_close_stream (stream);
	//run it
#ifdef E2_BADQUOTES NEEDED if unfriendly filename used ?
	qp = e2_utils_quote_string (localpath);
	gchar *command = e2_utils_strcat ("tracker-query ", qp);
	g_free (qp);
#else
	gchar *command = g_strdup_printf ("tracker-query \"%s\"", localpath);
#endif
	gchar *matches;
	if (e2_fs_get_command_output (command, &matches))
	{
		struct stat sb;
		while (can get another matching filepath)
		{
			if (filepath has good prefix)
			{
				if (!e2_fs_lstat (filepath, &sb E2_ERR_PTR()))
				{
					if (S_ISREG (sb.st_mode))
						_e2p_find_match1 (filepath, &sb, data);
				}
				else if (!E2_ERR_IS (ENOENT))
					_e2p_find_match1 (filepath, NULL, data);
			}
		}
		g_free (matches);
	}
	//cleanup
	gchar *localpath = "FIXME";
	e2_task_backend_delete (localpath);
	F_FREE (localpath, );
	g_free (utfpath);
	g_free (rdfstring);
*/
	g_free (command);
}
#endif
/**
@brief convert @a string to a form suitable for n-gram scanning
Changes upper-case to lower-case, punctuation-marks and some special characters
to spaces
@param string string to be processed, if not utf-8 it's parsed as ascii

@return newly-allocated converted string
*/
static gchar *_e2p_find_prepare_like_string (gchar *string)
{
	gchar *tmp, *retval;
	if (g_utf8_validate (string, -1, NULL))
	{
		glong j, len;
		gunichar ch;
		gunichar *converted;
		tmp = g_utf8_strdown (string, -1);
		converted = g_utf8_to_ucs4_fast (tmp, -1, &len);
		g_free (tmp);

		for (j = 0; j < len; j++)
		{
			ch = converted [j];
			if (!g_unichar_isalnum (ch)
#ifdef USE_GLIB2_14
				&& !g_unichar_ismark (ch)
#endif
			)
				ch = (gunichar)' ';
			else if (g_unichar_isupper (ch))
				ch = g_unichar_tolower (ch);
			else
			{
				switch ((guint8)ch)
				{
					default: break;
					case 196: ch = (gunichar)228; break; //ANSI-umlaut
					case 214: ch = (gunichar)246; break;
					case 220: ch = (gunichar)252; break;
					case 142: ch = (gunichar)132; break; //ASCII-umlaut
					case 153: ch = (gunichar)148; break;
					case 154: ch = (gunichar)129; break;
				}
			}
		}
		tmp = g_ucs4_to_utf8 (converted, -1, NULL, NULL, NULL);
		g_free (converted);
	}
	else	//not utf8, treat as ascii anyway
	{
		guchar c;
		gchar *s;
		tmp = g_strdup (string);
		s = tmp;
		while ((c = (guchar)*s) != '\0')
		{
			if (c < '0' || (c > '9' && c < 'A') || (c > 'Z' && c < 'a')) //'_' ok ??
				*s = ' ';
			else if (g_ascii_isupper (c))
				*s = g_ascii_tolower (c);
			else
			{
				switch (c)
				{
					default: break;
					case 196: *s = 228; break; //ANSI-umlaut
					case 214: *s = 246; break;
					case 220: *s = 252; break;
					case 142: *s = 132; break; //ASCII-umlaut
					case 153: *s = 148; break;
					case 154: *s = 129; break;
				}
			}
		}
	}
	//surround by "space marks"
	retval = g_strdup_printf (" %s ", tmp);
	g_free (tmp);
	return retval;
}
/**
@brief count matching and total n-grams in @a haystack

@param prepared_hay pre-processed haystack string
@param prepared_ndl pre-processed needle string
@param needlelen byte-length of @a prepared_ndl
@param ngramlen byte-length of n-gram for this scan, 2, 3 or 5 (must be < needlelen and < 8)
@param maxmatches pointer to store for count of weighted no. of n-grams

@return weighted count of matching n-grams
*/
static guint _e2p_find_match_ngrams (gchar *prepared_hay, gchar *prepared_ndl,
	guint needlelen, guint ngramlen, guint *maxmatches)
{
	//FIXME use chars, not bytes
#ifdef __USE_GNU
	gchar NGram [ngramlen + 1];
#else
	gchar NGram [8];
#endif
	NGram [ngramlen] = '\0';
//	*maxmatch = 0; needed if not using all n-grams
	//search in the target string for each n-gram in the search string
	guint i, matches = 0;
	guint ngrams = needlelen - ngramlen + 1;
	for (i = 0; i < ngrams; i++)
	{
		memcpy (NGram, &prepared_ndl[i], ngramlen);	//FIXME chars, not bytes
		//ignore included whitespace ?? CHECKME
//		if (NGram [ngramlen - 2] == ' ' && NGram [0] != ' ')
//			i += ngramlen - 3;
//		else
//		{
//			*maxmatch += ngramlen;	//weighted n-gram count needed if not using all n-grams
			if (strstr (prepared_hay, NGram)) //FIXME chars
				matches++;
//		}
	}
	*maxmatches = ngrams * ngramlen;
	return matches * ngramlen;  //more significance for longer n-grams
}
/**
@brief perform a fuzzy search on @a haystack using pre-compiled search data

@param haystack candidate string to check against the needle
@param needle the pre-converted target string to match in @a haystack
@param needlelen byte-length of @a needle

@return TRUE if a match was found
*/
static gboolean _e2p_find_fuzzy_match (gchar *haystack, gchar *needle, gint needlelen)
{
	gchar *prepared = _e2p_find_prepare_like_string (haystack);
	if (needlelen < 3)	//short strings only match exactly
	{
		gboolean match = !strcmp (prepared, needle);
		g_free (prepared);
		return match;
	}

	guint maxmatches1, maxmatches2;
	//length of n-grams depends on the search string
	guint matchcount1 = _e2p_find_match_ngrams (prepared, needle, needlelen,
		(needlelen < 7) ? 2 : 3, &maxmatches1);
	/*short n-grams can be too indiscriminate, and long ones decrease the tolerance.
	  It's best to use > 1 short n-grams with different lengths */
	guint matchcount2 = _e2p_find_match_ngrams (prepared, needle, needlelen,
		(needlelen < 7) ? 3 : 5, &maxmatches2);

	gfloat similarity = 100.0 * (gfloat)(matchcount1 + matchcount2)
				     / (maxmatches1 + maxmatches2);
	printd (DEBUG, "similarity between%sand%sis %f percent",
		prepared, needle, similarity);

	g_free (prepared);
	return (similarity >= E2_ENOUGHLIKE);
}
/**
@brief determine whether @a localpath matches all relevant search paramters in @a data
Uses file(1) to get mime string, uses grep(1) to check content
The path of each item that passes all tests is printed
@param localpath absolute localised path of an item as provided to the twcb funtion
@param statptr ptr to buffer as provided to the twcb funtion, or NULL for _NS items
@param data ptr to search parameters

@return TRUE if a match was found
*/
static gboolean _e2p_find_match1 (VPATH *localpath, const struct stat *statptr,
	findtargets *data)
{
	gboolean matched = FALSE;

	//name checking
	if (data->nameop != ISNA)
	{
		gchar *basename = g_path_get_basename (VPSTR(localpath));
		if (data->name_anycase)
		{
			gchar *tmp = basename;
			basename =  g_ascii_strdown (tmp, -1);	//FIXME ascii may not be enough
			g_free (tmp);
		}

		switch (data->nameop)
		{
			case ISEQ:
				matched = !strcmp (data->nametarget, basename);
				break;
			case ISNE:
				matched = strcmp (data->nametarget, basename);
				break;
			case WILD:
				matched = (fnmatch (data->nametarget, basename, FNM_PERIOD) == 0);
				break;
			case LIKE:
				matched = _e2p_find_fuzzy_match (basename, data->nametarget, data->preplen);
				break;
			case REGX:
				//CHECKME encoding ok ?
				matched = (regexec (&data->compiledname, basename, 0, NULL, 0) == 0);
			default:
				break;
		}
		g_free (basename);
		if (!matched)
			return FALSE;
	}

	//statbuf-related checking - easiest, done first
	if (statptr == NULL)
	{	//fail if we're trying to do any of them
		if (
			data->sizeop != ISNA
		 || data->permop != ISNA
		 || data->mtimop != ISNA
		 || data->atimop != ISNA
		 || data->ctimop != ISNA
		 || data->userop != ISNA
		 ||	data->groupop != ISNA
		 ||	data->typeop != ISNA
		)
			return FALSE;
	}
	else	//we can check the statbuf-related parameters
	{
		if (data->sizeop != ISNA)
		{
			if (data->sizeop == ISLT && data->sizetarget <= (guint64) statptr->st_size)
				return FALSE;
			if (data->sizeop == ISGT && data->sizetarget >= (guint64) statptr->st_size)
				return FALSE;
			if (data->sizeop == ISEQ && data->sizetarget != (guint64) statptr->st_size)
				return FALSE;
		}

		if (data->permop != ISNA)
		{
			mode_t masked = statptr->st_mode & ALLPERMS;
			if (data->permop == ISEQ && data->permtarget != masked)
				return FALSE;
			if (data->permop == ISNE /*exclude*/ && (masked & data->permtarget) != 0)
				return FALSE;
			if (data->permop == ISGE /*include*/ && (masked & data->permtarget) != data->permtarget)
				return FALSE;
		}

		time_t nowtime = time (NULL);

		if (data->mtimop != ISNA)
		{
			if (data->mtimop == ISLT && data->mtimtarget >= statptr->st_mtime)
				return FALSE;
			if (data->mtimop == ISGE && data->mtimtarget < statptr->st_mtime)	//times are rounded up to next minute
				return FALSE;
			if (data->mtimop == ISEQ && data->mtimtarget != statptr->st_mtime)
				return FALSE;
			if (data->mtimop == ISFROM &&
				(statptr->st_mtime < data->mtimtarget || statptr->st_mtime > nowtime))
				return FALSE;
		}

		if (data->atimop != ISNA)
		{
			if (data->atimop == ISLT && data->atimtarget >= statptr->st_atime)
				return FALSE;
			if (data->atimop == ISGE && data->atimtarget < statptr->st_atime)	//times are rounded up to next minute
				return FALSE;
			if (data->atimop == ISEQ && data->atimtarget != statptr->st_atime)
				return FALSE;
			if (data->atimop == ISFROM &&
				(statptr->st_atime < data->atimtarget || statptr->st_atime > nowtime))
				return FALSE;
		}

		if (data->ctimop != ISNA)
		{
			if (data->ctimop == ISLT && data->ctimtarget >= statptr->st_ctime)
				return FALSE;
			if (data->ctimop == ISGE && data->ctimtarget < statptr->st_ctime)	//times are rounded up to next minute
				return FALSE;
			if (data->ctimop == ISEQ && data->ctimtarget != statptr->st_ctime)
				return FALSE;
			if (data->ctimop == ISFROM &&
				(statptr->st_ctime < data->ctimtarget || statptr->st_ctime > nowtime))
				return FALSE;
		}

		if (data->userop != ISNA)
		{
			if (data->userop == ISEQ && data->usertarget != statptr->st_uid)
				return FALSE;
			if (data->userop == ISNE && data->usertarget == statptr->st_uid)
				return FALSE;
			if (data->userop == ISLT	//this item's user is not registered
				&& getpwuid (statptr->st_uid) != NULL)
				return FALSE;
		}

		if (data->groupop != ISNA)
		{
			if (data->groupop == ISEQ && data->grouptarget != statptr->st_gid)
				return FALSE;
			if (data->groupop == ISNE && data->grouptarget == statptr->st_gid)
				return FALSE;
			if (data->groupop == ISLT	//this item's group is not registered
				&& getgrgid (statptr->st_gid) != NULL)
				return FALSE;
		}

		if (data->typeop != ISNA)
		{
			guint i, c;
//flag-order is
//REGULAR_P, DIRECTORY_P, RAW_DEVICE_P, BLOCK_DEVICE_P, SYMLINK_P, SOCKET_P, FIFO_P
			guint masks [7] =
			{ S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK,
#ifdef S_IFLNK
				S_IFLNK,
#else
				0,
#endif
#ifdef S_IFSOCK
				S_IFSOCK,
#else
				0,
#endif
#ifdef S_IFIFO
				S_IFIFO,
#else
				0,
#endif
				};
			mode_t masked = statptr->st_mode & S_IFMT;
			for	(i = 0; i < 7 ; i++)
			{
				c = 1 << i;
				if (data->typetarget & c)
				{
					if (data->typeop == ISEQ && masked != masks [i])
						return FALSE;
					if (data->typeop == ISNE && masked == masks [i])
						return FALSE;
				}
			}
		}
	}	//end of statbuf-related checks

	if (data->mimeop != ISNA)
	{
#ifdef E2_MAGIC
		MagicIface ifc;

		if (e2_utils_fill_magic_iface (&ifc))
		{
			magic_t handle = ifc.open (
				MAGIC_PRESERVE_ATIME | MAGIC_RAW | MAGIC_ERROR |
				MAGIC_DEVICES | MAGIC_MIME_TYPE);
			if (handle != NULL)
			{
				ifc.load (handle, NULL); //load failure will result in NULL msg
				const gchar *msg = ifc.file (handle, VPCSTR (localpath));
				if (msg != NULL)
				{
					gchar *mime = g_strdup (msg);
					ifc.close (handle);
					dlclose (ifc.libhandle);

					g_strstrip (mime);
					if (data->mimeop == ISEQ && strcmp (mime, data->mimetarget))
						matched = FALSE;
					else if (data->mimeop == ISNE && !strcmp (mime, data->mimetarget))
						matched = FALSE;
					else if (data->mimeop == WILD)
					{
						gchar *s = strchr (mime, G_DIR_SEPARATOR);
						if (s != NULL)
						{
							//separately check strings before and after separator
							*s = '\0';
							s++;
							//temporarily split target
							gchar *p =  strchr (data->mimetarget, G_DIR_SEPARATOR);
							if (/*p != NULL && */p > data->mimetarget)
								*p = '\0';
							matched = !fnmatch (data->mimetarget, (gchar *)mime,
								FNM_LEADING_DIR | FNM_FILE_NAME | FNM_CASEFOLD);
							if (matched)
								matched = !fnmatch ((p == NULL) ? data->mimetarget : p+sizeof(gchar),
									s, FNM_FILE_NAME | FNM_CASEFOLD);
							if (/*p != NULL && */p > data->mimetarget)
								*p = G_DIR_SEPARATOR;
						}
						else
							matched = !fnmatch (data->mimetarget, (gchar *)mime,
								FNM_FILE_NAME | FNM_CASEFOLD);
					}
					g_free (mime);
					if (!matched)
						return FALSE;
				}
				else //no magic msg
				{
					//FIXME warning
					//X = ifc.error (handle);
					ifc.close (handle);
					dlclose (ifc.libhandle);
					return FALSE;
				}
			}
			else //no magic connection
			{
				//FIXME warning
				printd (WARN, "no libmagic connection");
				dlclose (ifc.libhandle);
				return FALSE;
			}
		}
		else //no dlopen
		{
			//FIXME warning
			printd (WARN, "no libmagic found");
			return FALSE;
		}
#else
//tag E2_BADQUOTES
		gchar *qp = e2_utils_quote_string (VPCSTR (localpath));
		gchar *command = e2_utils_strcat ("file -bhprs --mime-type ", qp);
		g_free (qp);
		gpointer mime;
		if (e2_fs_get_command_output (command, &mime))
		{
			gchar *s;
			//output is a string like application/x-bzip2 or text/plain; charset=us-ascii
			g_strstrip ((gchar *)mime);
			if (data->mimeop == ISEQ && strcmp ((gchar *)mime, data->mimetarget))
				matched = FALSE;
			else if (data->mimeop == ISNE && !strcmp ((gchar *)mime, data->mimetarget))
				matched = FALSE;
			else if (data->mimeop == WILD)
			{
				s = strchr ((gchar *)mime, G_DIR_SEPARATOR);
				if (s != NULL)
				{
					//separately check strings before and after separator
					*s = '\0';
					s++;
					//temporarily split target
					gchar *p =  strchr (data->mimetarget, G_DIR_SEPARATOR);
					if (/*p != NULL && */p > data->mimetarget)
						*p = '\0';
					matched = !fnmatch (data->mimetarget, (gchar *)mime,
						FNM_LEADING_DIR | FNM_FILE_NAME | FNM_CASEFOLD);
					if (matched)
						matched = !fnmatch ((p == NULL) ? data->mimetarget : p+sizeof(gchar),
							s, FNM_FILE_NAME | FNM_CASEFOLD);
					if (/*p != NULL && */p > data->mimetarget)
						*p = G_DIR_SEPARATOR;
				}
				else
					matched = !fnmatch (data->mimetarget, (gchar *)mime,
						FNM_FILE_NAME | FNM_CASEFOLD);
			}
			g_free (command);
			g_free (mime);
			if (!matched)
				return FALSE;
		}
		else
		{
			//FIXME warning
			g_free (command);
			return FALSE;
		}
#endif
	}

	if (data->contentop != ISNA)
	{
		const gchar *tmp3 = (data->content_anycase) ? "i":"";
		//CHECKME is localising the UTF-8 UI string sufficient ?
		gchar *tmp4 = e2_utf8_to_locale (data->contenttarget);
//tag E2_BADQUOTES
		gchar *qp = e2_utils_quote_string (VPCSTR (localpath));
		gchar *command = g_strdup_printf (
			"grep -El%ssZ -d skip -D skip -e '%s' %s", tmp3, tmp4, qp);
		g_free (tmp4);
		g_free (qp);
		gpointer output;
		if (e2_fs_get_command_output (command, &output))
		{
			g_free (command);
			//this will probably always succeed, mismatches should be failed commands
			matched = (*(gchar *)output != '\0');
			g_free (output);
			if (!matched)
				return FALSE;
		}
		else
		{
			g_free (command);
			return FALSE;
		}
	}

	gchar *utf = F_FILENAME_FROM_LOCALE (VPSTR(localpath));
	CLOSEBGL
	e2_output_print (&app.tab, utf, NULL, TRUE, NULL);
	OPENBGL
	F_FREE (utf, VPSTR(localpath));

	return TRUE;
}
/* *
@brief cleanup find parameters in @a data, and @a data itself
This grabs find_mutex lock
@param data ptr to search parameters data struct, may be NULL

@return
*/
/* now inline
static void _e2p_find_clear_match_data (findtargets *data)
{
	pthread_mutex_lock (&find_mutex);
	if (data != NULL)
	{
		if (data->nametarget != NULL)
			g_free (data->nametarget);
		if (data->nameop == REGX)
			regfree (&data->compiledname);
#ifdef MIMEFIND
		if (data->mimetarget != NULL)
			g_free (data->mimetarget);
#endif
		if (data->contenttarget != NULL)
			g_free (data->contenttarget);
	//	if (data->contentop == REGX)
	//		regfree (&data.compiledcontent);
#ifdef E2_VFS
		g_free (VPSTR(data->sdata));
#else
		g_free (data->localstartpath);
#endif
		DEALLOCATE (findtargets, data);
	}
	pthread_mutex_unlock (&find_mutex);
}
*/
/**
@brief populate find parameters in @a data with settings from dialog represented by @a rt

@param data ptr to search parameters
@param rt pointer to dialog data struct

@return
*/
static void _e2p_find_get_match_data (findtargets *data, E2_FindDialogRuntime *rt)
{
	const gchar *tmp;
	gchar *tmp2, *tmp3;

	memset (data, 0, sizeof (findtargets));

	//ITEMNAME
	tmp = gtk_entry_get_text (GTK_ENTRY (rt->pattern));
	if (*tmp != '\0')
	{
		FileInfo *info;
		if (!strcmp (tmp, "%f"))
		{
			info = e2_fileview_get_selected_first_local (curr_view, FALSE);
			if (info != NULL)
				tmp2 = g_strdup (info->filename);
			else
				return;
		}
		else if (!strcmp (tmp, "%F"))
		{
			info = e2_fileview_get_selected_first_local (other_view, FALSE);
			if (info != NULL)
				tmp2 = g_strdup (info->filename);
			else
				return;
		}
		else
		{
			tmp2 = D_FILENAME_TO_LOCALE (tmp);
			g_strstrip (tmp2);
			if (*tmp2 == '\0')
			{
				g_free (tmp2);
				return;
			}
		}

		data->name_anycase = _e2p_find_get_flag (ANYCASE_FILENAME_P);	//, rt)
		if (data->name_anycase)
		{
			tmp3 = tmp2;
			tmp2 =  g_ascii_strdown (tmp3, -1);	//FIXME ascii may not be enough
			g_free (tmp3);
		}
		data->nametarget = tmp2;

		if (_e2p_find_get_flag (STRING_FILENAME_P))	//, rt))
		{
			if (strchr (tmp2, '*') != NULL || strchr (tmp2, '?') != NULL)
				data->nameop = WILD;
			else
				data->nameop = ISEQ;
		}
		else if (_e2p_find_get_flag (WILDCARD_FILENAME_P))	//, rt))
		{
			if (strchr (tmp2, '*') != NULL || strchr (tmp2, '?') != NULL)
				data->nameop = WILD;
			else
			{
				data->nameop = LIKE;
				data->nametarget = _e2p_find_prepare_like_string (tmp2);
				data->preplen = strlen (data->nametarget);
				g_free (tmp2);
			}
		}
		else	//regex
		{
			tmp3 = (*tmp2 == '^') ? tmp2 : g_strconcat ("^", tmp2, NULL);
			gint cflags = REG_EXTENDED | REG_NOSUB;
			if (data->name_anycase)
				cflags |= REG_ICASE;
			if (regcomp (&data->compiledname, tmp3, cflags))
			{
				//FIXME warn user
				data->nameop = ISNA;
			}
			else
			{
				g_free (tmp2);
				data->nameop = REGX;
				data->nametarget = NULL;
			}
			if (tmp3 != tmp2)
				g_free (tmp3);
		}
	}
	else
		data->nameop = ISNA;

#ifdef MIMEFIND
	//MIME
	tmp = gtk_entry_get_text (GTK_ENTRY (rt->mime_entry));
	if (*tmp != '\0')
	{
		//mime strings should be lower-case
		tmp2 = g_utf8_strdown (tmp, -1);
		g_strstrip (tmp2);
		if (*tmp2 != '\0')
		{
			gchar *local;
			//matcher compares file(1) output, CHECKME that's localised text
			local = g_locale_from_utf8 (tmp2, -1, NULL, NULL, NULL);
			//FIXME support exact match
			tmp3 = strchr (local, G_DIR_SEPARATOR);
			if (tmp3 == NULL)
			{
				tmp3 = local;
				local = e2_utils_strcat ("*/", local);
				g_free (tmp3);
			}
			else if (tmp3 == local)
			{
				local = e2_utils_strcat ("*", local);
				g_free (tmp3);
			}
			data->mimetarget = local;
			//FIXME support not WILD
			//STRING_MIME_P, WILDCARD_MIME_P, NOT_MIME_P
			if (strchr (local, '*') != NULL || strchr (local, '?') != NULL)
				data->mimeop = WILD;
			else
			{
				//FIXME support ISNE in UI
				if (!_e2p_find_get_flag (NOT_MIME_P))	//, rt))
					data->mimeop = ISEQ;
				else
					data->mimeop = ISNE;
			}
		}
		else
			data->mimeop = ISNA;

		g_free (tmp2);
	}
	else
		data->mimeop = ISNA;
#endif

	//CONTENT
#ifdef TRACKERFIND
	if (rt->content_pattern2 != NULL //maybe want search by tracker
		&& _e2p_find_get_flag (TRACK_CONTENT_P))
	{
		tmp = gtk_entry_get_text (GTK_ENTRY (rt->content_pattern2));
		if (*tmp != '\0')
		{
			data->contentop = TRAK;
			data->contenttarget = g_strdup (tmp);
		}
		else
			data->contentop = ISNA;
	}
	else
	{
#endif
		tmp = gtk_entry_get_text (GTK_ENTRY (rt->content_pattern));
		if (*tmp != '\0')
		{
			data->content_anycase = _e2p_find_get_flag (ANYCASE_CONTENT_P);
			//FIXME support fuzzy content matching
			//FIXME what encoding for content ? (but executed command expects UTF8 and converts that)
			//(matcher uses grep(1) which expects encoding determined from environment variables)
			tmp2 = g_strdup (tmp);
			if (_e2p_find_get_flag (STRING_CONTENT_P))
				data->contentop = ISEQ; //no need to check for entered wildcard, grep handles that anyway
			else if (_e2p_find_get_flag (WILDCARD_CONTENT_P))
			{
				if (strchr (tmp, '*') == NULL && strchr (tmp, '?') == NULL)
				{
					g_free (tmp2);
					tmp2 = g_strconcat ("*", tmp, "*", NULL);
				}
				data->contentop = ISEQ;	//WILD not needed, grep is ok with wildcards
			}
			else
			{
				data->contenttarget = tmp2;
				data->contentop = REGX;
			}

			if (data->contentop == ISEQ)	//exact or wild
			{	//matcher grep(1) does comparison using regular expressions
				//adjust the find pattern, by replacing all '.', '*' and '?' with
				//their extended regex equivalents
				gchar **split = g_strsplit (tmp2, ".", -1);
				g_free (tmp2);
				tmp2 = g_strjoinv ("\\.", split);
				g_strfreev (split);
				split = g_strsplit (tmp2, "?", -1);
				g_free (tmp2);
				tmp2 = g_strjoinv (".", split);
				g_strfreev (split);
				split = g_strsplit (tmp2, "*", -1);
				g_free (tmp2);
				data->contenttarget = g_strjoinv (".*", split);	//freeme later lazy-star to prevent greedy matching ?
				g_strfreev (split);
			}
		}
		else
			data->contentop = ISNA;
#ifdef TRACKERFIND
	}
#endif

	gint indx;
	gint date_format_index = e2_option_int_get ("date-string");
	if (date_format_index > 5)
		date_format_index = 0;	//out of range, use default format (should never happen)

	//MTIME
	if (_e2p_find_get_flag (MTIME_LT_P))	//, rt)
		data->mtimop = ISLT;
	else if (_e2p_find_get_flag (MTIME_EQ_P))		//, rt)
		data->mtimop = ISEQ;
	else if (_e2p_find_get_flag (MTIME_GT_P))	//, rt)
		data->mtimop = ISGT;
	else if (_e2p_find_get_flag (MTIME_REL_P))	//, rt)
	{
		indx = gtk_combo_box_get_active (GTK_COMBO_BOX(rt->mrel_combo));
		tmp2 = g_strconcat (gtk_entry_get_text (GTK_ENTRY (rt->mrel_entry)), " ",
			periods[indx], NULL);
		data->mtimtarget = e2_date_filter_dialog_get_time (tmp2, NULL, 0, date_format_index);
		g_free (tmp2);
		if (data->mtimtarget != (time_t)-1)
			data->mtimop = ISFROM;
		else
		{
			//FIXME warn user
			data->mtimop = ISNA;
		}
	}
	else
		data->mtimop = ISNA;
	if (data->mtimop != ISNA && data->mtimop != ISFROM)
	{
		if (_e2p_find_get_datetime (&data->mtimtarget, &rt->mtime))
		{
			if (data->mtimop == ISGT)
			{
				data->mtimtarget += 60;	//compensate for minutes-rounding
				data->mtimop = ISGE;
			}
		}
		else
		{
			//FIXME warn user
			data->mtimop = ISNA;
		}
	}
	//ATIME
	if (_e2p_find_get_flag (ATIME_LT_P))	//, rt)
		data->atimop = ISLT;
	else if (_e2p_find_get_flag (ATIME_EQ_P))		//, rt)
		data->atimop = ISEQ;
	else if (_e2p_find_get_flag (ATIME_GT_P))	//, rt)
		data->atimop = ISGT;
	else if (_e2p_find_get_flag (ATIME_REL_P))	//, rt)
	{
		indx = gtk_combo_box_get_active (GTK_COMBO_BOX(rt->arel_combo));
		tmp2 = g_strconcat (gtk_entry_get_text (GTK_ENTRY (rt->arel_entry)), " ",
			periods[indx], NULL);
		data->atimtarget = e2_date_filter_dialog_get_time (tmp2, NULL, 0, date_format_index);
		g_free (tmp2);
		if (data->atimtarget != (time_t)-1)
			data->atimop = ISFROM;
		else
		{
			//FIXME warn user
			data->atimop = ISNA;
		}
	}
	else
		data->atimop = ISNA;
	if (data->atimop != ISNA && data->atimop != ISFROM)
	{
		if (_e2p_find_get_datetime (&data->atimtarget, &rt->atime))
		{
			if (data->atimop == ISGT)
			{
				data->atimtarget += 60;	//compensate for minutes-rounding
				data->atimop = ISGE;
			}
		}
		else
		{
			//FIXME warn user
			data->atimop = ISNA;
		}
	}
	//CTIME
	if (_e2p_find_get_flag (CTIME_LT_P))	//, rt)
		data->ctimop = ISLT;
	else if (_e2p_find_get_flag (CTIME_EQ_P))		//, rt)
		data->ctimop = ISEQ;
	else if (_e2p_find_get_flag (CTIME_GT_P))	//, rt)
		data->ctimop = ISGT;
	else if (_e2p_find_get_flag (CTIME_REL_P))	//, rt)
	{
		indx = gtk_combo_box_get_active (GTK_COMBO_BOX(rt->crel_combo));
		tmp2 = g_strconcat (gtk_entry_get_text (GTK_ENTRY (rt->crel_entry)), " ",
			periods[indx], NULL);
		data->ctimtarget = e2_date_filter_dialog_get_time (tmp2, NULL, 0, date_format_index);
		g_free (tmp2);
		if (data->ctimtarget != (time_t)-1)
			data->ctimop = ISFROM;
		else
		{
			//FIXME warn user
			data->ctimop = ISNA;
		}
	}
	else
		data->ctimop = ISNA;
	if (data->ctimop != ISNA && data->ctimop != ISFROM)
	{
		if (_e2p_find_get_datetime (&data->ctimtarget, &rt->ctime))
		{
			if (data->ctimop == ISGT)
			{
				data->ctimtarget += 60;	//compensate for minutes-rounding
				data->ctimop = ISGE;
			}
		}
		else
		{
			//FIXME warn user
			data->ctimop = ISNA;
		}
	}
	//USER
	if (_e2p_find_get_flag (UID_ANY_P))
	{
		data->usertarget = 0;
		data->userop = ISNA;
	}
	else if (_e2p_find_get_flag (UID_LOGIN_P))	//, rt))
	{
		data->usertarget = getuid ();
		data->userop = ISEQ;
	}
	else if (_e2p_find_get_flag (UID_SPECIFIC_P))	//, rt))
	{
		tmp = gtk_entry_get_text (GTK_ENTRY (rt->user_entry));
		if (*tmp != '\0')
		{
			struct passwd *pwptr;
			tmp2 = g_locale_from_utf8 (tmp, -1, NULL, NULL, NULL);	//, rt))
			//errno = 0;
			pwptr = getpwnam (tmp2);
			if (pwptr != NULL)
			{
				data->usertarget = pwptr->pw_uid;
				data->userop = ISEQ;
			}
			else
			{
				//FIXME warn user
				data->usertarget = 0;
				data->userop = ISNA;
			}
			g_free (tmp2);
		}
		else
		{
			data->usertarget = 0;
			data->userop = ISNA;
		}
	}
	else if (_e2p_find_get_flag (UID_NONE_P))	//, rt))
	{
		data->usertarget = 0;
		data->userop = ISLT;	//special use of this
	}
	//GROUP
	if (_e2p_find_get_flag (GID_ANY_P))
	{
		data->grouptarget = 0;
		data->groupop = ISNA;
	}
	else if (_e2p_find_get_flag (GID_LOGIN_P))	//, rt))
	{
		data->grouptarget = getgid ();
		data->groupop = ISEQ;
	}
	else if (_e2p_find_get_flag (GID_SPECIFIC_P))	//, rt))
	{
		tmp = gtk_entry_get_text (GTK_ENTRY (rt->group_entry));
		if (*tmp != '\0')
		{
			struct group *grptr;
			tmp2 = g_locale_from_utf8 (tmp, -1, NULL, NULL, NULL);	//, rt))
			grptr = getgrnam (tmp2);
			if (grptr != NULL)
			{
				data->grouptarget = grptr->gr_gid;
				data->groupop = ISEQ;
			}
			else
			{
				data->grouptarget = 0;
				data->groupop = ISNA;
			}
			g_free (tmp2);
		}
		else
		{
			data->grouptarget = 0;
			data->groupop = ISNA;
		}
	}
	else if (_e2p_find_get_flag (GID_NONE_P))	//, rt))
	{
		data->grouptarget = 0;
		data->groupop = ISLT;	//special use of this
	}
	//SIZE
	tmp = gtk_entry_get_text (GTK_ENTRY (rt->size_entry));
	if (*tmp != '\0')
	{
		if (_e2p_find_get_flag (FSIZE_LT_P))	//, rt))
			data->sizeop = ISLT;
		else if (_e2p_find_get_flag (FSIZE_GT_P))	//, rt))
			data->sizeop = ISGT;
		else
			data->sizeop = ISEQ;

		tmp2 = g_locale_from_utf8 (tmp, -1, NULL, NULL, NULL);
		gchar *endptr;
		data->sizetarget = g_ascii_strtoull (tmp2, &endptr, 10);
		if (*endptr == '\0')
		{
			if (!_e2p_find_get_flag (FSIZE_B_P))	//, rt))
			{
				data->sizetarget *= 1024;	//bump up the accuracy for finding big files with a fractional size
				if (_e2p_find_get_flag (FSIZE_MB_P))	//, rt))
				data->sizetarget *= 1024;
			}
		}
		else
		{
			data->sizetarget = 0;
			data->sizeop = ISNA;
		}
		g_free (tmp2);
	}
	else
	{
		data->sizetarget = 0;
		data->sizeop = ISNA;
	}
	//PERMISSIONS
	if (_e2p_find_get_flag (MODE_NOT_P))	//, rt))
		data->permop = ISNE;
	else if (_e2p_find_get_flag (MODE_IS_P))	//, rt))
		data->permop = ISEQ;
	else
		data->permop = ISGE;	//"include" mode

	mode_t mask[12] = {S_ISUID, S_ISGID, S_ISVTX, S_IRUSR, S_IWUSR, S_IXUSR,
			S_IRGRP, S_IWGRP, S_IXGRP, S_IROTH, S_IWOTH, S_IXOTH};
	mode_t mode = 0;
	gint i;
	for (i = 0; i < 12; i++)
	{
		if (_e2p_find_get_flag (i + (gint) PERMISSIONS1))	//, rt))
				mode |= mask[i];
		data->permtarget = mode;
	}
	if (data->permop == ISGE && mode == 0)
		data->permop = ISNA; //include nothing is the "ignore" setting

	//FILETYPES
	//FIXME support negation
	//can't store choices in a mode_t, that has shared bits
	guint c = 0;
	gint p = (gint) REGULAR_P;
//REGULAR_P, DIRECTORY_P, RAW_DEVICE_P, BLOCK_DEVICE_P, SYMLINK_P, SOCKET_P, FIFO_P
	for	(i = 0; i < 7 ; i++)
	{
		if (_e2p_find_get_flag (i + p))	//, rt))
		c |= 1 << i;
	}
	data->typetarget = c;
	if (c > 0)
	{
#ifdef TRACKERFIND
		data->tracker_service = -1;
		if (c == 1)	//only want regular files
		{
			data->tracker_service = gtk_combo_box_get_active
				(GTK_COMBO_BOX (rt->service_combo));
			if (data->tracker_service != -1
				&& data->tracker_service != 0)	//all files setting
			{
				data->typeop = TRAK;
			}
		}
		if (data->tracker_service <= 0)
		{
#endif
			if (_e2p_find_get_flag (TYPE_NOT_P))
				data->typeop = ISNE;
			else
				data->typeop = ISEQ;
#ifdef TRACKERFIND
		}
#endif
	}
	else
	{
		data->typeop = ISNA;
	}
}
/* *
@brief add surrounding quotes to @a original if needed
@a original must be freeable, as it is replaced if need be
@param original the string to check

@return @a original, or a newly-allocated replacement
*/
/*static gchar *_e2p_find_quote_string (gchar *original)
{
#ifdef E2_BADQUOTES CHECKME for shell usage ?
	return (e2_utils_quote_string (original));
#else
	if (e2_utils_find_whitespace (original) == NULL)
		return original;
	if (*original == '"' && *(original + strlen (original) - 1) == '"')
		return original;
	gchar *quoted = g_strconcat ("\"", original, "\"", NULL);
	g_free (original);
	return quoted;
#endif
} */
/**
@brief check if a specified year is a leap year
This was 'imported' from utils

@param year gint value of year to be checked

@return TRUE if @a year is a leap year
*/
static gboolean _e2p_find_check_leapyear (gint year)
{
	return (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0));
}
/**
@brief set specified flag to T/F

The relevant array value is set

@param f enumerated value of flag to be set
@param value new value for the flag
@param rt UNUSED pointer to dialog data struct

@return
*/
static void _e2p_find_set_flag (findflag_t f, gboolean value)	//, E2_FindDialogRuntime *rt)
{
	if (f < MAX_FLAGS)
		flags [(gint) f] = value;
}
/**
@brief return the value of a specified flag

@param f enumerated value of flag to be interrogated
@param rt UNUSED pointer to dialog data struct

@return flag value, T/F, or FALSE if the value is not recognised
*/
static gboolean _e2p_find_get_flag (findflag_t f)	//, E2_FindDialogRuntime *rt)
{
	 if (f < MAX_FLAGS)
		return (flags[(gint) f]);
	 else
		 return (FALSE);
}
/**
@brief set all flags to FALSE

@param rt UNUSED pointer to dialog data struct

@return
*/
static void _e2p_find_reset_flags (void)	//E2_FindDialogRuntime *rt)
{
	gint i;
	for (i = 0; i < MAX_FLAGS; i++)
	{
/*		if (i == (gint) SEARCH_SUBDIRS_P
		 || i == (gint) PRINT_TO_WINDOW_P
		 || i == (gint) SHELL_COMMAND_P || i ==	(gint) PRINT_TO_STDOUT_P
		 || i == (gint) PRINT_FILENAME_ANYWAY_P || i ==	(gint) WARNING_WINDOW_P
		 || i == (gint) LONG_OUTPUT_P
		  )
		 ; // do nothing
		else
*/
			flags[i] = FALSE;
	}
}
/**
@brief determine date/time value from a specified set of spinners
@param choice ptr to store for the calculated timeval
@param times ptr to spinners data struct, with time to be interrogated

@return
*/
static gboolean _e2p_find_get_datetime (time_t *choice, spinners *times)
{
	gboolean retval;
	GDate *target = g_date_new ();
	g_date_set_dmy (target,
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (times->day_spin)),
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (times->month_spin)),
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (times->year_spin)) );

	if (g_date_valid (target))
	{
		struct tm tm;
		g_date_to_struct_tm (target, &tm);
     	tm.tm_hour = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (times->hour_spin));
     	tm.tm_min = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (times->minute_spin));
		tm.tm_sec = 0;
		*choice = mktime (&tm);
		retval = TRUE;
	}
	else
	{
		*choice = 0;
		retval = FALSE;
	}
	g_date_free (target);
	return retval;
}
/**
@brief create local store of current date parameters

@param current ptr to struct for storage of current date/time data

@return
*/
static void _e2p_find_get_current_datetime (local_dt *current)
{
/* portable, but buggy, version (hours not correct)
	g_date_clear (&cur_date, 1);
	g_date_set_time (&cur_date, time (NULL));
	g_get_current_time (&cur_time);
	current->day = (gfloat) g_date_get_day (&cur_date);
	current->month = (gfloat) g_date_get_month (&cur_date);
	current->year = (gfloat) g_date_get_year (&cur_date);
	gint h1 = cur_time.tv_sec/3600;
	gint m1 = (cur_time.tv_sec - h1*3600)/60;
	gint s1 = cur_time.tv_sec - h1*3600 - m1 *60;
	current->hour = (gfloat) h1;
	current->minute = (gfloat) m1;
	current->second = (gfloat) s1;
*/
	struct tm tm_time;
	time_t now = time (NULL);
	localtime_r (&now, &tm_time);

	current->day = (gfloat) tm_time.tm_mday;
	current->month = (gfloat) tm_time.tm_mon + 1;
	current->year = (gfloat) tm_time.tm_year + 1900;
	current->hour = (gfloat) tm_time.tm_hour;
	current->minute = (gfloat) tm_time.tm_min;
//	current->second = (gfloat) tm_time.tm_sec;
}

/**
@brief recursively check whether all modifiable widgets inside @a widget currently
  hold their respective default value
@param widget when first called, this is a page-child for the dialog notebook
@param clean pointer to boolean value to hold the result. Should hold TRUE when
  first called

@return nothing, the result is communicated via @a clean
*/
static void _e2p_find_whether_page_is_clean (GtkWidget *widget, gboolean *clean)
{
	gboolean state;

	if (GTK_IS_CONTAINER (widget))
		gtk_container_foreach ((GtkContainer*)widget,
			(GtkCallback)_e2p_find_whether_page_is_clean, clean);

	void (*reset_function) () =
		g_object_get_data (G_OBJECT (widget), "reset_yourself");

	if (reset_function == _e2p_find_reset_combo)
	{
		gint indx = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (widget), "default_index"));
		state = (gtk_combo_box_get_active (GTK_COMBO_BOX (widget)) == indx);
	}
	else if (reset_function == _e2p_find_reset_entry)
		state = (*gtk_entry_get_text (GTK_ENTRY (widget)) == '\0');
	else if (reset_function == _e2p_find_set_toggle_button_on)
		state = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
	else if (reset_function == _e2p_find_set_toggle_button_off)
		state = !gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
	else if (reset_function == _e2p_find_reset_spin_button)
	{
		gfloat *value = g_object_get_data (G_OBJECT (widget), "default_value");
		state = (gtk_spin_button_get_value (GTK_SPIN_BUTTON (widget)) == *value);
	}
	else
		state = TRUE;

	if (!state)
		*clean = FALSE;
}

/**
@brief a shared callback for each widget's "changed" signal (or the like)
@param widget the widget whose value has changed
@user_data UNUSED Note that for some types of callback the 2nd arg is something
  else e.g. GdkEvent *

@return
*/
static void _e2p_find_widget_changed_cb (GtkWidget *widget, gpointer user_data)
{
	gboolean clean;
	GtkWidget *box, *label;

	box = (GtkWidget *) g_object_get_data (G_OBJECT(widget), PAGE_DATAKEY);
	NEEDCLOSEBGL
	clean = TRUE;
	_e2p_find_whether_page_is_clean (box, &clean);

	label = (GtkWidget *) g_object_get_data (G_OBJECT(box), LABEL_DATAKEY);
#ifdef USE_GTK3_0
	if (clean)
	{
		gtk_widget_override_color (label, GTK_STATE_NORMAL, NULL);
		gtk_widget_override_color (label, GTK_STATE_ACTIVE, NULL);
	}
	else
	{
		GdkRGBA dirty;
		e2_option_color_get_RGBA ("color-negative", &dirty);
		gtk_widget_override_color (label, GTK_STATE_NORMAL, &dirty);
		gtk_widget_override_color (label, GTK_STATE_ACTIVE, &dirty);
	}
#else
	if (clean)
	{
		gtk_widget_modify_fg (label, GTK_STATE_NORMAL, NULL);
		gtk_widget_modify_fg (label, GTK_STATE_ACTIVE, NULL);
	}
	else
	{
		GdkColor *dirty = e2_option_color_get ("color-negative");
		gtk_widget_modify_fg (label, GTK_STATE_NORMAL, dirty);
		gtk_widget_modify_fg (label, GTK_STATE_ACTIVE, dirty);
	}
#endif
	NEEDOPENBGL
}

/**
@brief update display of all tab-labels in @a notebook, according to whether
  all modifiable widgets in the correspoding page hold their default value
@param notebook the notebook widget of the find-dialog

@return
*/
static void _e2p_find_update_tablabels (GtkWidget *notebook)
{
	gint i, count;
#ifdef USE_GTK3_0
	GdkRGBA dirty;
	e2_option_color_get_RGBA ("color-negative", &dirty);
#else
	GdkColor *dirty;
	dirty = e2_option_color_get ("color-negative");
#endif
	count = gtk_notebook_get_n_pages (GTK_NOTEBOOK (notebook));
	for (i = 0; i < count; i++)
	{
		gboolean clean;
		GtkWidget *child, *label;

		child = gtk_notebook_get_nth_page (GTK_NOTEBOOK (notebook), i);
		label = gtk_notebook_get_tab_label (GTK_NOTEBOOK (notebook), child);
		clean = TRUE;
		_e2p_find_whether_page_is_clean (child, &clean);
#ifdef USE_GTK3_0
		if (clean)
		{
			gtk_widget_override_color (label, GTK_STATE_NORMAL, NULL);
			gtk_widget_override_color (label, GTK_STATE_ACTIVE, NULL);
		}
		else
		{
			gtk_widget_override_color (label, GTK_STATE_NORMAL, &dirty);
			gtk_widget_override_color (label, GTK_STATE_ACTIVE, &dirty);
		}
#else
		if (clean)
		{
			gtk_widget_modify_fg (label, GTK_STATE_NORMAL, NULL);
			gtk_widget_modify_fg (label, GTK_STATE_ACTIVE, NULL);
		}
		else
		{
			gtk_widget_modify_fg (label, GTK_STATE_NORMAL, dirty);
			gtk_widget_modify_fg (label, GTK_STATE_ACTIVE, dirty);
		}
#endif
	}
}
/**
@brief make each modifiable widget in notebook page-widget aware of the
  page-widget in which it's sitting
@param widget when first called, this is a page-widget of the dialog's notebook
@param page_widget pointer to the widget to be recorded

@return
*/
static void _e2p_find_notify_all_widgets (GtkWidget *widget, GtkWidget *page_widget)
{
	if (GTK_IS_CONTAINER (widget))
		gtk_container_foreach (GTK_CONTAINER (widget),
			(GtkCallback)_e2p_find_notify_all_widgets, page_widget);
	//only some widgets are relevant
	void (*reset_function) (GtkWidget *) =
		g_object_get_data (G_OBJECT (widget), "reset_yourself");
	if (reset_function != NULL)
		g_object_set_data (G_OBJECT (widget), PAGE_DATAKEY, page_widget);
}
/**
@brief reset each resettable widget in the dialog

This applies recursively to all container widgets
The reset fn is determined from the widget's associated
"reset_yourself" data, if any

@param widget to be processed
@param data data supplied to the foreach func

@return
*/
static void _e2p_find_reset_all_widgets (GtkWidget *widget, gpointer user_data)
{
	if (GTK_IS_CONTAINER (widget))
		gtk_container_foreach (GTK_CONTAINER (widget),
			_e2p_find_reset_all_widgets, user_data);

	void (*reset_function) (GtkWidget *) =
		g_object_get_data (G_OBJECT (widget), "reset_yourself");
	if (reset_function != NULL)
		 (*reset_function) (widget);
}
/**
@brief reset an entry widget content to ""

@param widget the entry to be cleared

@return
*/
static void _e2p_find_reset_entry (GtkWidget *widget)
{
	gtk_entry_set_text (GTK_ENTRY (widget), "");
}
/**
@brief reset a combobox widget to its default history-item

@param widget the combobox to be changed

@return
*/
static void _e2p_find_reset_combo (GtkWidget *widget)
{
	gint indx = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (widget), "default_index"));
	gtk_combo_box_set_active (GTK_COMBO_BOX (widget), indx);
}
/**
@brief set toggle-button widget state to FALSE

@param button the widget to be changed

@return
*/
static void _e2p_find_set_toggle_button_off (GtkToggleButton *button)
{
	gtk_toggle_button_set_active (button, FALSE);
}
/**
@brief set toggle-button widget state to TRUE

@param button the widget to be changed

@return
*/
static void _e2p_find_set_toggle_button_on (GtkToggleButton *button)
{
	gtk_toggle_button_set_active (button, TRUE);
}
/**
@brief set spin-button widget state to its default value

The value is determined from the widget's associated
"default_value" data

@param widget the widget to be changed

@return
*/
static void _e2p_find_reset_spin_button (GtkWidget *widget)
{
	gfloat *value = g_object_get_data (G_OBJECT (widget), "default_value");
	gtk_spin_button_set_value (GTK_SPIN_BUTTON (widget), *value);
}
/* *
@brief

@param w
@param filesel

@return
*/
/*OMIT? static void save_search_ok (GtkWidget *w, GtkFileSelection *filesel)
{
	const gchar *s = gtk_file_selection_get_filename (GTK_FILE_SELECTION (filesel));
	if (strlen (s) > 0)
	{
		save_search_command (s);
		gtk_widget_destroy (GTK_WIDGET (filesel));
	}
} */
/* *
@brief

@param w
@param filesel

@return
*/
/*OMIT ? static void save_search_cancel (GtkWidget *w, GtkFileSelection *filesel)
{
	gtk_widget_destroy (GTK_WIDGET (filesel));
} */

  /*****************/
 /*** callbacks ***/
/*****************/

/**
@brief helper function for recursive directory finds
The tree is being scanned breadth-first, through-links.
Dirs are made accessible if not already so and it's permitted,
Altered dirs are added to a list to be reverted after all the tree has
been traversed, other items are changed as requested (if possible)
Error messages expect BGL open
@param localpath absolute path of item to change, localised string
@param statptr pointer to struct stat with info about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data pointer to user-specified data for for the operation

@return completion code: E2TW_CONTINUE if succeeds, others as appropriate
*/
static E2_TwResult _e2p_find_twcb (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, findtargets *user_data)
{
	if (user_data->aborted)
		return E2TW_STOP;	//probably irrelevant now, with thread cancellation

	E2_TwResult retval;
	E2_ERR_DECLARE
	retval = E2TW_CONTINUE;	//default error code = none

	switch (status)
	{
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
						if (e2_fs_chmod (localpath, dirfix->mode E2_ERR_PTR())
							&& E2_ERR_ISNOT (ENOENT))
						{
							e2_fs_error_local (_("Cannot change permissions of %s"),
								localpath E2_ERR_MSGL());
							retval = E2TW_FIXME;
						}
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
			//before changing permissions
			//(though they've already been changed for a DRR case)
			_e2p_find_match1 (localpath, statptr, user_data);
			//ensure dir is writable, if we can
			if (e2_fs_tw_adjust_dirmode (localpath, statptr, (W_OK | X_OK)) == 0)
			{
				//FIXME warn user about failure
				retval = E2TW_SKIPSUB;	//don't try to do any descendant
			}
			else	//dir can be processed
			{
				//add this dir to list of items to revert afterwards
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
		case E2TW_F:
		case E2TW_SL:  //valid and invalid links
		case E2TW_SLN: //broken links (CHECKME not reported if E2TW_PHYS is used)
			_e2p_find_match1 (localpath, statptr, user_data);
			break;
		case E2TW_NS:	//un-statable item (for which, error is reported upstream)
						//(note - if this is a physical walk, no link-through problem here)
			_e2p_find_match1 (localpath, NULL, user_data);
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

	if (user_data->aborted)
		return E2TW_STOP;	//probably irrelevant now, with thread cancellation
//	if (retval & E2TW_SKIPSUB)
//		user_data->continued_after_problem = TRUE;
	if (retval & E2TW_FIXME)
	{
//		user_data->continued_after_problem = TRUE;
		retval &= ~E2TW_FIXME;	//continue after bad item
	}
	return retval;
}
/**
@brief adjust directory string in @a entry
After a keypress, this clears any selection and completes the path.
If the current content is not an absolute path, the active-pane directory
is referenced for completion.
@param entry the entry widget for directory data
@param event pointer to event data struct
@param data UNUSED data specified when callback was connnected

@return TRUE if the key was non-modified and a textkey
*/
static gboolean _e2p_find_key_press_cb (GtkWidget *entry, GdkEventKey *event,
	gpointer data)
{
	if ((event->state & (GDK_CONTROL_MASK | GDK_MOD1_MASK
		| GDK_MOD3_MASK | GDK_MOD4_MASK | GDK_MOD5_MASK	//CHECKME
#ifdef USE_GTK2_10
		| GDK_SUPER_MASK | GDK_HYPER_MASK | GDK_META_MASK
#endif
	)) == 0
		&& (event->keyval < 0xF000 || event->keyval > 0xFFFF))
	{
		NEEDCLOSEBGL
		gboolean ret = e2_fs_complete_dir (entry, event->keyval, 0); //default is active pane
		NEEDOPENBGL
		return ret;
	}
	return FALSE;
}
/**
@brief idle-callack for deferred cleanup of runtime data

@param rt pointer to data to be cleaned

@return FALSE after cleanup has been done
*/
static gboolean _e2p_find_deferclean (E2_FindDialogRuntime *rt)
{
	if (rt->matchdata != NULL)	//search thread still going
	{
		usleep (50000);
		return TRUE;
	}
	DEALLOCATE (E2_FindDialogRuntime, rt);
	return FALSE;
}
/**
@brief cleanup after cancel button is pressed. or window has been closed
Dialog destruction requires BGL closed upon arrival here
Note that when closing the dialog during a running search, this func can
operate in parallel with _e2p_find_cleanfind()
@param widget clicked button, UNUSED
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_quit_cb (GtkWidget *widget, E2_FindDialogRuntime *rt)
{
	//clear current strings array and cache data list
	e2_list_free_with_data (&strings);
	guint i;
	for (i = 0; i < MAX_ENTRIES; i++)
		g_free (entries[i]);

	NEEDCLOSEBGL
	//update array and list
	entries [NAME_ENTRY] = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->pattern)));
	entries [CONTENT_ENTRY] = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->content_pattern)));
#ifdef TRACKERFIND
	entries [CONTENT_ENTRY2] = (rt->content_pattern2 != NULL) ?
		g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->content_pattern2))):
		g_strdup ("");	//must survive freeing inside loop
#endif
	entries [MIME_ENTRY] = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->mime_entry)));
	entries [SIZE_ENTRY] = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->size_entry)));
	entries [UID_ENTRY] = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->user_entry)));
	entries [GID_ENTRY] = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->group_entry)));
	entries [MREL_ENTRY] = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->mrel_entry)));
	entries [AREL_ENTRY] = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->arel_entry)));
	entries [CREL_ENTRY] = g_strdup (gtk_entry_get_text (GTK_ENTRY (rt->crel_entry)));
	gchar *cachethis;
	for (i = 0; i < MAX_ENTRIES; i++)
	{
		//empty or space-only strings don't get saved
		if (i != NAME_ENTRY)
			g_strstrip (entries[i]);
		if (*entries[i] == '\0')
			cachethis = g_strdup (".");	//so we use this instead
		else
			cachethis = g_strdup (entries[i]);
		strings = g_list_append (strings, cachethis);
	}

	if (rt->groups != NULL)
	{	//cleanup data stored with some buttons
		GSList *tmp, *members;
		for (tmp = rt->groups; tmp != NULL; tmp=tmp->next)
		{
			members = (GSList *) g_object_get_data (G_OBJECT (tmp->data),
				"group_members");
			g_slist_free (members);
		}
		g_slist_free (rt->groups);
	}
	pthread_mutex_lock (&find_mutex);
	if (GTK_IS_WIDGET (rt->dialog))	//maybe the dialog window has been closed already
		gtk_widget_destroy (rt->dialog);
	if (rt->matchdata == NULL)	//search thread has finished cleaning up
	{
		DEALLOCATE (E2_FindDialogRuntime, rt);
	}
	else
		g_idle_add ((GSourceFunc) _e2p_find_deferclean, rt);	//cleanup later
//	rt = NULL;	useless
	find_rt = NULL;
	pthread_mutex_unlock (&find_mutex);
//	gtk_widget_grab_focus (curr_view->treeview);
	NEEDOPENBGL
}
/**
@brief dialog response callback

@param dialog the dialog where the response was initiated, UNUSED
@param response the number assigned to the widget which triggered the response
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_response_cb (GtkDialog *dialog, gint response, E2_FindDialogRuntime *rt)
{
//	printd (DEBUG, "Find plugin dialog response cb, response %d", response);
	switch (response)
	{
		case GTK_RESPONSE_CLOSE:
		case GTK_RESPONSE_DELETE_EVENT:
		case GTK_RESPONSE_NONE:
			NEEDCLOSEBGL
			pthread_mutex_lock (&find_mutex);
			//cancel running search, if any, no buttons update
			if (rt->matchdata != NULL)
			{	//a search is in progress
				rt->matchdata->aborted = TRUE;	//probably irrelevant now, with thread cancellation
				pthread_cancel (rt->matchdata->findID);
			}
			pthread_mutex_unlock (&find_mutex);
			NEEDOPENBGL
			_e2p_find_quit_cb (NULL, rt);	//cleanup rt data
		default:
			break;
	}
}
/**
@brief toggle specified option flag after a radio or normal toggle button is clicked

@param widget toggled button widget
@param flagnum pointerized number of the flag to be toggled

@return
*/
static void _e2p_find_toggle_cb (GtkToggleButton *button, gpointer flagnum)
{
	NEEDCLOSEBGL
	//if this if this is during setup, before a widget is created ...
#ifdef USE_GTK2_20
	if (!gtk_widget_get_mapped (find_rt->dialog))
#else
	if (!GTK_WIDGET_MAPPED (find_rt->dialog))
#endif
	{
		NEEDOPENBGL
		return;
	}

	findflag_t flg = (findflag_t) flagnum;
//	gboolean newflag = ! _e2p_find_get_flag (flg);	//, find_rt);
	gboolean newflag =  //safer, with cached flags
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (button);
#else
		button->active;
#endif
	_e2p_find_set_flag (flg, newflag);	//, find_rt);

	//handle here all the 'special cases', if any
	if (flg == SEARCH_THIS_P)
	{
		gtk_widget_set_sensitive (find_rt->directory, newflag);
		gtk_widget_set_sensitive (find_rt->chooser_button, newflag);
	}
	else if (flg == SEARCH_SUBDIRS_P)
	{
		gtk_widget_set_sensitive (find_rt->inlink_button, newflag);
	}
#ifdef TRACKERFIND
	else if (flg == REGULAR_P)
	{
		if (find_rt->service_combo != NULL && _e2p_find_get_flag (TYPE_IS_P))
			gtk_widget_set_sensitive (find_rt->service_combo, newflag);
	}
#endif
	else if (newflag)
	{
		switch (flg)
		{
			case SEARCH_ALL_P:
				//for convenience, enable descent
				gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (find_rt->recurse_button),
						TRUE);
				//when searching everywhere, no need to go into linked dirs
				gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (find_rt->inlink_button),
						FALSE);
				break;
			case UID_ANY_P:
			case UID_NONE_P:
			case UID_LOGIN_P:
				gtk_widget_set_sensitive (find_rt->curr_user, FALSE);
				gtk_widget_set_sensitive (find_rt->choose_user, FALSE);
				gtk_widget_set_sensitive (find_rt->user_entry, FALSE);
				break;
			case UID_SPECIFIC_P:
				gtk_widget_set_sensitive (find_rt->curr_user, TRUE);
				gtk_widget_set_sensitive (find_rt->choose_user, TRUE);
				gtk_widget_set_sensitive (find_rt->user_entry,
					_e2p_find_get_flag (UID_NOT_LOGIN_P));	//, find_rt));
				break;
			case GID_ANY_P:
			case GID_NONE_P:
			case GID_LOGIN_P:
				gtk_widget_set_sensitive (find_rt->curr_group, FALSE);
				gtk_widget_set_sensitive (find_rt->choose_group, FALSE);
				gtk_widget_set_sensitive (find_rt->group_entry, FALSE);
				break;
			case GID_SPECIFIC_P:
				gtk_widget_set_sensitive (find_rt->curr_group, TRUE);
				gtk_widget_set_sensitive (find_rt->choose_group, TRUE);
				gtk_widget_set_sensitive (find_rt->group_entry,
					_e2p_find_get_flag (GID_NOT_LOGIN_P));	//, find_rt));
#ifdef TRACKERFIND
			case TYPE_IS_P:
				if (find_rt->service_combo != NULL && _e2p_find_get_flag (REGULAR_P))
					gtk_widget_set_sensitive (find_rt->service_combo, TRUE);
				break;
			case TYPE_NOT_P:
				if (find_rt->service_combo != NULL)
					gtk_widget_set_sensitive (find_rt->service_combo, FALSE);
#endif
			default:
				break;
		}
	}
	NEEDOPENBGL
	_e2p_find_widget_changed_cb (GTK_WIDGET(button), NULL);
}
/**
@brief toggle specified option flag(s) after a grouped toggle button is clicked

@param button toggled button widget
@param flagnum pointerized number of the flag to be toggled

@return
*/
static void _e2p_find_grouptoggle_cb (GtkToggleButton *button, gpointer flagnum)
{
	findflag_t flg = (findflag_t) flagnum;
//	gboolean newflag = ! _e2p_find_get_flag (flg);	//, find_rt);
	gboolean newflag = //safer, with cached flags
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (button);
#else
		button->active;
#endif
	_e2p_find_set_flag (flg, newflag);	//, find_rt);

	NEEDCLOSEBGL

	if (newflag)
	{	//clear all other members of the group
		GtkToggleButton *leader = (GtkToggleButton *)g_object_get_data
			(G_OBJECT (button), "group_leader");
		GSList *member;
		for (member = g_object_get_data (G_OBJECT (leader), "group_members");
			 member != NULL; member = member->next)
		{
			leader = (GtkToggleButton *) member->data;
			if (leader != button)
			{
				_e2p_find_set_toggle_button_off (leader); //triggers recursive call here, clearing the relevant flag
			}
		}
	}
	//handle here all the 'special cases', if any
	 switch (GPOINTER_TO_INT(flagnum))
	{
		case UID_LOGIN_P:
			newflag = (newflag) ? FALSE :
				_e2p_find_get_flag (UID_SPECIFIC_P) &&
				!_e2p_find_get_flag (UID_NOT_LOGIN_P);	//, find_rt) ;
			gtk_widget_set_sensitive (find_rt->user_entry, newflag);
			break;
		case UID_NOT_LOGIN_P:
			gtk_widget_set_sensitive (find_rt->user_entry, newflag);
			break;
		case GID_LOGIN_P:
			newflag = (newflag) ? FALSE :
				_e2p_find_get_flag (GID_SPECIFIC_P) &&
				!_e2p_find_get_flag (GID_NOT_LOGIN_P);	//, find_rt) ;
			gtk_widget_set_sensitive (find_rt->group_entry, newflag);
			break;
		case GID_NOT_LOGIN_P:
			gtk_widget_set_sensitive (find_rt->group_entry, newflag);
		default:
			break;
	}
	NEEDOPENBGL
	_e2p_find_widget_changed_cb (GTK_WIDGET(button), NULL);
}
/**
@brief change search directory

@param chooser the selection object
@param rt pointer to dialog data

@return
*/
static void _e2p_find_choose_directory_cb (GtkFileChooser *chooser, E2_FindDialogRuntime *rt)
{
	NEEDCLOSEBGL
	gchar *uri = gtk_file_chooser_get_uri (chooser);
	if (uri != NULL)
	{
		gchar *dirpath = g_filename_from_uri (uri, NULL, NULL);
		if (dirpath != NULL)
		{
			if (*dirpath != '\0')
			{
				gchar *opendir = F_FILENAME_FROM_LOCALE (dirpath);
				gtk_entry_set_text (GTK_ENTRY (rt->directory), opendir);
				F_FREE (opendir, dirpath);
			}
			g_free (dirpath);
		}
		g_free (uri);
	}
	NEEDOPENBGL
}
/**
@brief callback for find clear action

the "clear" button will reset the search pattern	(ie the notebook
stuff), and not change anything else

@param w activated widget, UNUSED
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_clear_find_cb (GtkWidget *w, E2_FindDialogRuntime *rt)
{
	NEEDCLOSEBGL
	_e2p_find_reset_all_widgets (rt->dialog, NULL);
	_e2p_find_update_tablabels (rt->notebook);
	NEEDOPENBGL
}
/**
@brief revert dialog widgets ready for a new search
Expects BGL open on arrival here
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_reset_widgets (E2_FindDialogRuntime *rt)
{
	if (GTK_IS_WIDGET (rt->dialog))	//sometimes we're cleaning up after a close-button click
	{
		CLOSEBGL
		e2_dialog_set_cursor (rt->dialog, GDK_LEFT_PTR);
		gtk_widget_set_sensitive (rt->help_button, TRUE);
		gtk_widget_set_sensitive (rt->start_button, TRUE);
		gtk_widget_set_sensitive (rt->stop_button, FALSE);
		OPENBGL
	}
}
/**
@brief callback for find button click

Parses the selected options and initiates search in a separate thread

@param button clicked widget
@param rt ptr to search dialog data struct (in case of a bad race, rt could be NULL)

@return
*/
static void _e2p_find_find_cb (GtkButton *button, E2_FindDialogRuntime *rt)
{
	const gchar *utfdir;
	gchar *dlocal;
	findtargets data;

	pthread_mutex_lock (&find_mutex);
	if (rt == NULL)
	{
		pthread_mutex_unlock (&find_mutex);
		return;
	}

	NEEDCLOSEBGL
	//setup search parameters
	_e2p_find_get_match_data (&data, rt);
	if (
		data.contentop == ISNA
		&& data.nameop == ISNA
#ifdef MIMEFIND
		&& data.mimeop == ISNA
#endif
		&& data.sizeop == ISNA
		&& data.permop == ISNA
		&& data.mtimop == ISNA
		&& data.atimop == ISNA
		&& data.ctimop == ISNA
		&& data.userop == ISNA
		&& data.groupop == ISNA
		&& data.typeop == ISNA
		)
	{
		NEEDOPENBGL
		pthread_mutex_unlock (&find_mutex);
		return;
	}

#ifdef E2_VFSTMP
 FIXME decide how to handle namespaces esp. when startpath is not active or inactive dir
 confirm walker is ok for v-dirs
 warn about content finding for v-dirs
#endif
	//decide where to start the search
	if (_e2p_find_get_flag (SEARCH_ALL_P))	//, rt))
	{
		utfdir = G_DIR_SEPARATOR_S;
#ifdef E2_VFS
		data.sdata.spacedata = curr_view->spacedata;
#endif
	}
	else
	{
		if (_e2p_find_get_flag (SEARCH_CURRENT_P))	//, rt))
		{
			utfdir = curr_view->dir;
#ifdef E2_VFS
			data.sdata.spacedata = curr_view->spacedata;
#endif
		}
		else if (_e2p_find_get_flag (SEARCH_OTHER_P))	//, rt))
		{
			utfdir = other_view->dir;
#ifdef E2_VFS
			data.sdata.spacedata = other_view->spacedata;
#endif
		}
		else if (_e2p_find_get_flag (SEARCH_TRASH_P))	//, rt))
		{
			utfdir = NULL;	//no specific start-place
#ifdef E2_VFS
			data.sdata.spacedata = NULL;	//only local trash
#endif
		}
		else	//specified dir
		{
			utfdir = gtk_entry_get_text (GTK_ENTRY (rt->directory));
			utfdir = e2_utils_pass_whitespace ((gchar *)utfdir);	//ignore leading whitespace in entry
			if (utfdir == NULL)
				 return;
			//CHECKME best way to handle non-absolute path ?
			if (*utfdir != G_DIR_SEPARATOR)
			{
				dlocal = e2_utils_strcat (curr_view->dir, utfdir);
				gtk_entry_set_text (GTK_ENTRY (rt->directory), dlocal);
				g_free (dlocal);
				utfdir = gtk_entry_get_text (GTK_ENTRY (rt->directory));
			}
#ifdef E2_VFS
			data.sdata.spacedata = curr_view->spacedata;
#endif
		}
	}

	if (utfdir != NULL)
	{
		dlocal = D_FILENAME_TO_LOCALE (utfdir);
		//scrub any trailing separator from the start path
		gchar *tmp2 = dlocal + strlen (dlocal) - sizeof (gchar);
		if (tmp2 > dlocal && *tmp2 == G_DIR_SEPARATOR)
			*tmp2 = '\0';
	}
	else
		dlocal = NULL;
#ifdef E2_VFS
	data.sdata.path = dlocal;
#else
	data.localstartpath = dlocal;
#endif

	if (_e2p_find_get_flag (SEARCH_SUBDIRS_P))	//, rt))
		data.searchdepth = -1;	//unlimited
	else
		data.searchdepth = 1;

	pthread_mutex_unlock (&find_mutex);
	rt->matchdata = ALLOCATE (findtargets);
	CHECKALLOCATEDWARN (rt->matchdata, g_free (dlocal);return;)
	*rt->matchdata = data;	//save data for use while searching
	gtk_widget_set_sensitive (rt->help_button, FALSE);
	gtk_widget_set_sensitive (rt->start_button, FALSE);
	gtk_widget_set_sensitive (rt->stop_button, TRUE);
	e2_dialog_set_cursor (rt->dialog, GDK_WATCH);

	NEEDOPENBGL

	//do it - separate thread allows the search to proceed without blocking the UI
	pthread_attr_t attr;
	pthread_attr_init (&attr);
	pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_DETACHED);
	pthread_create (&rt->matchdata->findID, &attr,
		(gpointer(*)(gpointer))_e2p_find_dofind, rt);
	pthread_attr_destroy (&attr);
}
/**
@brief helper function to perform searching
@param fdata pointer to search data
@return
*/
static void _e2p_find_work (findtargets *fdata)
{
#ifdef TRACKERFIND
	if (fdata->contentop == TRAK || fdata->typeop == TRAK)
	{	//tracker can't do place-specific matching
		_e2p_find_tracker_find (
#ifdef E2_VFS
		&fdata->sdata,
#else
		fdata->localstartpath,
#endif
		fdata->searchdepth, fdata);
	}
	else
	{
		//default through links, to search in linked dirs (circularity risk)
		E2_TwFlags walkflags = E2TW_XQT | E2TW_FIXDIR;
		if (!_e2p_find_get_flag (DIRECTORY_P))	//FIXME static flag
			walkflags |= E2TW_NODIR;
		if (!_e2p_find_get_flag (SEARCH_SUBDIRS_P) || !_e2p_find_get_flag (SEARCH_LINKS_P))
			walkflags |= E2TW_PHYS;
#endif
		e2_fs_tw (
#ifdef E2_VFS
		&fdata->sdata,
#else
		fdata->localstartpath,
#endif
		_e2p_find_twcb, fdata, fdata->searchdepth, walkflags E2_ERR_NONE());
		if (fdata->dirdata != NULL)
		{	//cleanup chmod data left over after a stop or tw problem
			GList *member;
			E2_DirEnt *dirfix;
			for (member = g_list_last (fdata->dirdata); member != NULL; member = member->prev)
			{
				dirfix = (E2_DirEnt *)member->data;
				if (dirfix != NULL)
				{
					E2_ERR_DECLARE;
#ifdef E2_VFS
					fdata->sdata.path = dirfix->path;
					if (e2_fs_chmod (&fdata->sdata, dirfix->mode E2_ERR_PTR())
#else
					if (e2_fs_chmod (dirfix->path, dirfix->mode E2_ERR_PTR())
#endif
							&& E2_ERR_ISNOT (ENOENT))
					{
						e2_fs_error_local (_("Cannot change permissions of %s"),
#ifdef E2_VFS
								&fdata->sdata E2_ERR_MSGL());
#else
								dirfix->path E2_ERR_MSGL());
#endif
					}
					g_free (dirfix->path);
					DEALLOCATE (E2_DirEnt, dirfix);
				}
			}
			g_list_free (fdata->dirdata);
		}
#ifdef TRACKERFIND
	}
#endif
}
/**
@brief thread function to perform search
Uses grep and/or ?? for mimetype, content
@param rt ptr to search dialog data struct (in case of a bad race, rt could be NULL)

@return NULL
*/
static gpointer _e2p_find_dofind (E2_FindDialogRuntime *rt)
{
	if (rt == NULL)
		return NULL;

	pthread_cleanup_push ((gpointer)_e2p_find_cleanfind, (gpointer)rt);
	pthread_setcanceltype (PTHREAD_CANCEL_ASYNCHRONOUS, NULL);
	e2_utils_block_thread_signals ();	//block all allowed signals to this thread

	findtargets *fdata = rt->matchdata;	//get a copy in case rt goes during search

	//is this a search in trash ?
	//could also re-check SEARCH_SUBDIRS_P flag but that's static
#ifdef E2_VFS
	if (fdata->sdata.path == NULL)
#else
	if (fdata->localstartpath == NULL)
#endif
	{
		GList *trashes = e2_utils_get_trash_all ();
		if (trashes != NULL)
		{
			GList *member;
			for (member = trashes; member != NULL; member = member->next)
			{
#ifdef E2_VFS
				fdata->sdata.path =
#else
				fdata->localstartpath =
#endif
					g_build_filename ((gchar*)member->data, "files", NULL);
				_e2p_find_work (fdata);
				printd (DEBUG, "finished searching %s",
#ifdef E2_VFS
				fdata->sdata.path);
#else
				fdata->localstartpath);
#endif
				g_free (
#ifdef E2_VFS
				(gchar *)fdata->sdata.path);
#else
				fdata->localstartpath);
#endif
			}
			e2_list_free_with_data (&trashes);
			//no need to clean this later
#ifdef E2_VFS
			fdata->sdata.path = NULL;
#else
			fdata->localstartpath = NULL;
#endif
		}
	}
	else
		_e2p_find_work (fdata);

	CLOSEBGL
	e2_output_print_end (&app.tab, FALSE);
	OPENBGL
	_e2p_find_reset_widgets (rt);	//if thread not aborted, get the buttons etc back to normal

	pthread_cleanup_pop (1);	//cleanup match data

	return NULL;
}
/**
@brief end-of-thread function to cleanup match data
Note that when closing the dialog during a running search, this func can
operate in parallel with _e2p_find_quit_cb()
@param rt ptr to search dialog data struct (in case of a bad race, rt could be NULL)

@return
*/
static void _e2p_find_cleanfind (E2_FindDialogRuntime *rt)
{
	printd (DEBUG, "cleanup after find thread finish");
	pthread_mutex_lock (&find_mutex);
	if (rt != NULL)
	{	//dialog not closed during the search
		//clear data ptr ASAP, minimise misuse of data by cleanup function
		findtargets *fdata = rt->matchdata;
		rt->matchdata = NULL;
//		_e2p_find_clear_match_data (fdata);
		if (fdata != NULL)
		{
			if (fdata->nametarget != NULL)
				g_free (fdata->nametarget);
			if (fdata->nameop == REGX)
				regfree (&fdata->compiledname);
#ifdef MIMEFIND
			if (fdata->mimetarget != NULL)
				g_free (fdata->mimetarget);
#endif
			if (fdata->contenttarget != NULL)
				g_free (fdata->contenttarget);
		//	if (fdata->contentop == REGX)
		//		regfree (&fdata->compiledcontent);
#ifdef E2_VFS
			g_free (VPSTR(&fdata->sdata));
#else
			g_free (fdata->localstartpath);
#endif
			DEALLOCATE (findtargets, fdata);
		}
	}
	pthread_mutex_unlock (&find_mutex);
}
/**
@brief callback for stop find button click

@param w activated widget, UNUSED
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_stop_find_cb (GtkWidget *w, E2_FindDialogRuntime *rt)
{
	pthread_mutex_lock (&find_mutex);
	if (rt->matchdata != NULL)
	{	//a search is in progress
		rt->matchdata->aborted = TRUE;	//probably irrelevant now, with thread cancellation
		pthread_cancel (rt->matchdata->findID);
	}
	NEEDOPENBGL
	_e2p_find_reset_widgets (rt);
	NEEDCLOSEBGL
	pthread_mutex_unlock (&find_mutex);
}
/**
@brief callback for help button

executes external command "man gtkfind"

@param button the clicked widget, UNUSED
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_help_cb (GtkButton *button, E2_FindDialogRuntime *rt)
{
	//these are components of help-file headings
	//_I( no translation until help docs are translated
 static gchar *msg[10] =
 {
	 "name",	//page 0
	 "content",	//page 1
#ifdef MIMEFIND
	 "mime",	//page 2
#endif
	 "mtime",	//page 3
	 "atime",	//page 4
	 "ctime",	//page 5
	 "size",	//page 6
	 "permission",	//page 7
	 "owner",	//page 8
	 "type"	//page 9
 };

	NEEDCLOSEBGL
	gint page = gtk_notebook_get_current_page (GTK_NOTEBOOK (rt->notebook));
 //_I( no translation until help docs are translated
 	gchar *title = g_strconcat ("find by ", msg[page], NULL);
	e2_utils_show_help (title);
	NEEDOPENBGL
	g_free (title);
}
/* *
@brief

@return
*/
/*static void _e2p_find_save_search_cb (E2_FindDialogRuntime *rt)
{
	NEEDCLOSEBGL
//	GtkWidget *filesel =
	_e2p_find_create_filesel (_("Save Search"), save_search_ok, save_search_cancel, rt);
	NEEDOPENBGL
} */
/**
@brief callback for year changed signal

This is needed to handle any February in a leap year

@param widget year spin-button widget
@param callback_data ptr to one of the day_spin widgets

@return
*/
static void _e2p_find_year_changed_cb (GtkWidget *widget, spinners *times)
{
	gint max_day;
	NEEDCLOSEBGL
	gint month = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (times->month_spin));

	if (month == 2)
	{	//we only need to do something about February
		if (_e2p_find_check_leapyear (gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (widget))))
			max_day = 29;
		else
			max_day = 28;

		gint day = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (times->day_spin));
		if (day > max_day)
		{
			day = max_day;
			gtk_spin_button_set_value (GTK_SPIN_BUTTON (times->day_spin), (gfloat) day);
		}
		GtkAdjustment* nadj = (GtkAdjustment *) gtk_adjustment_new
			((gdouble) day, 1.0, (gdouble) max_day, 1.0, 2.0, 0.0);
		gtk_spin_button_set_adjustment (GTK_SPIN_BUTTON (times->day_spin), nadj);
	}
	NEEDOPENBGL
}
/**
@brief callback for month changed signal

This is needed to handle any February in a leap year

@param widget month spin-button widget
@param callback_data ptr to one of the day_spin widgets

@return
*/
static void _e2p_find_month_changed_cb (GtkWidget *widget, spinners *times)
{
	gint nvalue;
	gint max_date;

	NEEDCLOSEBGL
	gint month = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (widget));
	gint ovalue = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (times->day_spin));
	if (month == 2)
	{
		if (_e2p_find_check_leapyear (gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (times->year_spin))))
			max_date = 29;
		else
			max_date = 28;
	}
	else if (month == 4 || month == 6 || month == 9 || month == 11)
		max_date = 30;
	else
		max_date = 31;

	if (ovalue > max_date)
	{
		nvalue = max_date;
		gtk_spin_button_set_value (GTK_SPIN_BUTTON (times->day_spin), (gfloat) nvalue);
	}
	else
		nvalue = ovalue;

	GtkAdjustment *nadj = (GtkAdjustment *) gtk_adjustment_new
			((gdouble) nvalue, 1.0, (gdouble)max_date, 1.0, 2.0, 0.0);
	gtk_spin_button_set_adjustment (GTK_SPIN_BUTTON (times->day_spin), nadj);
	NEEDOPENBGL
}

/**
@brief callback for notebook page-switched signal

@param notebook UNUSED the book whose page has changed
@param page UNUSED the new page
@param page_num the index of the new page
@param page_store pointer to store for @a page_num
@return
*/
static void _e2p_find_pagechange_cb (GtkNotebook *notebook,
#ifdef USE_GTK3_0
	GtkWidget *page,
#else
	GtkNotebookPage *page,
#endif
	guint page_num, gint *page_store)
{
	*page_store = page_num;
}

  /***********************/
 /*** widget creation ***/
/***********************/

#ifdef USE_SHELL
/**
@brief make the widgets associated with running shell commands

@param parent the widget into which the new widgets wll be packed

@return
*/
static void _e2p_find_make_shell_widgets (GtkWidget *parent)
{
	// output format selection
	GtkWidget *hbox = _e2p_find_create_hbox (parent);
	GtkWidget *radio = _e2p_find_create_radio_button (hbox, FIXME
//no _()		SHORT_OUTPUT_P, TRUE, _("Print only filename"));
//	GSList *list = gtk_radio_button_get_group (GTK_RADIO_BUTTON (radio));
//no _()	radio = gtk_radio_button_new_with_label (list, _("Print extra data"));
	g_signal_connect (G_OBJECT (radio), "toggled",
		G_CALLBACK (_e2p_find_toggle_cb), (gpointer) LONG_OUTPUT_P);
//need a reset for radio ??
	gtk_container_add (GTK_CONTAINER (hbox), radio);

	// shell command stuff

	_e2p_find_create_toggle_button (parent,
//no _()		SHELL_COMMAND_P, FALSE, _("Run a shell command?"));

	hbox = _e2p_find_create_hbox (parent);

//no _() GtkWidget *label = _e2p_find_create_label (hbox, _("Shell command:"));
	shell_command = _e2p_find_create_entry (hbox. "");
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (radio, shell_command);
#endif

	hbox = _e2p_find_create_hbox (parent);

	_e2p_find_create_toggle_button (hbox,
//no _()		PRINT_TO_STDOUT_P, FALSE, _("Print to stdout"));
	_e2p_find_create_toggle_button (hbox,
//no _()		PRINT_TO_WINDOW_P, TRUE, _("Print to window"));
	_e2p_find_create_toggle_button (parent,
//no _()		PRINT_FILENAME_ANYWAY_P, FALSE, _("Always print filename"));
}
#endif	//def USE_SHELL
/**
@brief make the widgets involved with choosing the directory to search

@param box the widget into which the new widgets wll be packed
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_directory_widgets (GtkWidget *box, E2_FindDialogRuntime *rt)
{
#ifdef E2_ASSISTED
	GtkWidget *label2 =
#endif
	e2_widget_add_mid_label (box, _("Find items:"), LABEL_ALIGN, FALSE, E2_PADDING_SMALL);
	GtkWidget *hbox = e2_widget_add_box (box, TRUE, 0, FALSE, TRUE, E2_PADDING);
	GtkWidget *radio =
	_e2p_find_create_radio_button (hbox, NULL, SEARCH_ALL_P, FALSE,
		_("any_where"), rt);
	_e2p_find_create_radio_button (hbox, radio, SEARCH_TRASH_P, FALSE,
		_("in _trash"), rt);
	hbox = e2_widget_add_box (box, TRUE, 0, FALSE, TRUE, E2_PADDING);
	rt->active_button =
	_e2p_find_create_radio_button (hbox, radio, SEARCH_CURRENT_P, TRUE,
		_("in _active directory"), rt);
#ifdef E2_VFSTMP
	if (curr_view->spacedata != NULL)
		gtk_widget_set_sensitive (rt->active_button, FALSE);
	//FIXME later, change sensitivity if curr_view->fs changes
	rt->something =
#endif
	_e2p_find_create_radio_button (hbox, radio, SEARCH_OTHER_P, FALSE,
		_("in _other directory"), rt);
#ifdef E2_VFSTMP
	if (other_view->spacedata != NULL)
		gtk_widget_set_sensitive (rt->something, FALSE);
	//FIXME later, change sensitivity if other_view->fs changes
#endif
	hbox = e2_widget_add_box (box, TRUE, 0, FALSE, TRUE, E2_PADDING);
	rt->thisdir_button =
	_e2p_find_create_radio_button (hbox, radio, SEARCH_THIS_P, FALSE,
		_("in _directory"), rt);
	const gchar *message = _("Choose directory");
	rt->chooser_button =
	gtk_file_chooser_button_new (message, GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
	gtk_file_chooser_set_show_hidden (GTK_FILE_CHOOSER (rt->chooser_button), TRUE);

	gchar *local = D_FILENAME_TO_LOCALE (curr_view->dir);
	gchar *s = local + strlen (local) - sizeof(gchar);
	if (s > local && *s == G_DIR_SEPARATOR)
		*s = '\0';
	gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (rt->chooser_button), local);
	g_free (local);
	g_signal_connect (G_OBJECT (rt->chooser_button), "current-folder-changed",
		G_CALLBACK (_e2p_find_choose_directory_cb), rt);
	e2_widget_set_safetip (rt->chooser_button, message);
	gtk_box_pack_end (GTK_BOX (hbox), rt->chooser_button, FALSE, FALSE, 0);

	rt->directory = _e2p_find_create_entry (box, "");
//#ifdef E2_VFSTMP
	//FIXME dir when not mounted local
//#else
//	gtk_entry_set_text (GTK_ENTRY (rt->directory), curr_view->dir);
//#endif
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label2, rt->directory);
#endif
//	if (nocacheflags)
//	gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->active_button), TRUE);
//	gboolean state = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->thisdir_button));
//	gtk_widget_set_sensitive (rt->directory, state);

	//need to interpret keypresses for completion
	g_signal_connect (G_OBJECT (rt->directory), "key-press-event",
		G_CALLBACK (_e2p_find_key_press_cb), NULL);
	hbox = e2_widget_add_box (box, FALSE, 0, FALSE, FALSE, E2_PADDING);
	rt->recurse_button =
	_e2p_find_create_toggle_button (hbox, SEARCH_SUBDIRS_P, TRUE,
		_("_Recurse subdirectories"), rt);
	rt->inlink_button =
	_e2p_find_create_toggle_button (hbox, SEARCH_LINKS_P, TRUE,
		_("Include _linked subdirectories"), rt);
	e2_widget_set_safetip (rt->inlink_button, _("Careful about circular links"));
}
/**
@brief make the file search-criteria notebook and its subwidgets

@param box the widget into which the notebook wll be packed
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_notebook (GtkWidget *box, E2_FindDialogRuntime *rt)
{
	rt->notebook = e2_widget_get_notebook (_e2p_find_pagechange_cb, &page_store);
#ifdef E2_SMALLSCREEN
	gtk_notebook_set_scrollable (GTK_NOTEBOOK (rt->notebook), TRUE);
#else
	gtk_notebook_set_tab_pos (GTK_NOTEBOOK (rt->notebook), GTK_POS_LEFT);
#endif
	gtk_box_pack_start (GTK_BOX (box), rt->notebook, TRUE, TRUE, 0);

	_e2p_find_make_name_tab (rt->notebook, rt);	//page 0
	_e2p_find_make_content_tab (rt->notebook, rt);	//page 1
#ifdef MIMEFIND
	_e2p_find_make_mimetype_tab (rt->notebook, rt);	//page 2
#endif
	_e2p_find_get_current_datetime (&current);

	_e2p_find_make_mtime_tab (rt->notebook, rt);	//page 3
	_e2p_find_make_atime_tab (rt->notebook, rt);	//page 4
	_e2p_find_make_ctime_tab (rt->notebook, rt);	//page 5

	_e2p_find_make_size_tab (rt->notebook, rt);	//page 6
	_e2p_find_make_mode_tab (rt->notebook, rt);		//page 7
	_e2p_find_make_owner_tab (rt->notebook, rt);	//page 8
	_e2p_find_make_type_tab (rt->notebook, rt);		//page 9

	_e2p_find_update_tablabels (rt->notebook);
}
/**
@brief make notebook tab with itemname search options

@param notebook the widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_name_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("name"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
#ifdef E2_ASSISTED
	GtkWidget *label2 =
#endif
	e2_widget_add_mid_label (vbox, _("and whose name:"), LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);

	GtkWidget *hbox = _e2p_find_create_hbox (vbox);
	GtkWidget *radio =
	_e2p_find_create_radio_button (hbox, NULL, STRING_FILENAME_P, TRUE,
		_("is"), rt);
	_e2p_find_create_radio_button (hbox, radio, WILDCARD_FILENAME_P, FALSE,
		_("is like"), rt);
	_e2p_find_create_radio_button (hbox, radio, REGEXP_FILENAME_P, FALSE,
		_("matches this regex"), rt);
	_e2p_find_create_toggle_button (hbox, ANYCASE_FILENAME_P, FALSE,
		_("ignore case"), rt);

	hbox = _e2p_find_create_hbox (vbox);
	rt->pattern = _e2p_find_create_entry (hbox, entries [NAME_ENTRY]);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label2, rt->pattern);
#endif

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT(vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
/**
@brief make notebook tab with itemsize search options

@param notebook notebook widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_size_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("size"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
#ifdef E2_ASSISTED
	GtkWidget *label2 =
#endif
	e2_widget_add_mid_label (vbox, _("and whose size is:"), LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);

	GtkWidget *hbox = _e2p_find_create_hbox (vbox);
	GtkWidget *leader = _e2p_find_create_toggle_grouped_button
		(hbox, FSIZE_LT_P, TRUE, _("less than:"), NULL, rt);
	_e2p_find_create_toggle_button (hbox,
		FSIZE_EQ_P, FALSE, _("equal to:"), rt);
	_e2p_find_create_toggle_grouped_button (hbox,
		FSIZE_GT_P, FALSE, _("more than"), leader, rt);

	hbox = _e2p_find_create_hbox (vbox);
	rt->size_entry = _e2p_find_create_entry (hbox, entries [SIZE_ENTRY]);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label2, rt->size_entry);
#endif

	GtkWidget *radio = _e2p_find_create_radio_button (hbox, NULL,
		FSIZE_B_P, TRUE, _("bytes"), rt);
	_e2p_find_create_radio_button (hbox, radio,
		FSIZE_KB_P, FALSE, _("kbytes"), rt);
	_e2p_find_create_radio_button (hbox, radio,
		FSIZE_MB_P, FALSE, _("Mbytes"), rt);

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT (vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
#ifdef MIMEFIND
/**
@brief make notebook tab with mimetype search options

@param notebook notebook widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_mimetype_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("mime"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
#ifdef E2_ASSISTED
	GtkWidget *label2 =
#endif
	e2_widget_add_mid_label (vbox, _("and whose mimetype is like this:"), LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);
	GtkWidget *hbox = _e2p_find_create_hbox (vbox);
	rt->mime_entry = _e2p_find_create_entry (hbox, entries [MIME_ENTRY]);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label2, rt->mime_entry);
#endif

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT (vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
#endif	//def MIMEFIND
/**
@brief make notebook tab with file mtime search options

Options are: earlier than and/or equal to, later than and/or equal to.
By default, nothing is selected, so all mtimes will be matched

@param notebook notebook widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_mtime_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("save"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
	e2_widget_add_mid_label (vbox, _("and which were most-recently saved:"), LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);
	GtkWidget *hbox = _e2p_find_create_hbox (vbox);
	GtkWidget *leader = _e2p_find_create_toggle_grouped_button
		(hbox, MTIME_LT_P, FALSE, _("before:"), NULL, rt);
	_e2p_find_create_toggle_button (hbox, MTIME_EQ_P, FALSE, _("on/at:"), rt);
	_e2p_find_create_toggle_grouped_button
		(hbox, MTIME_GT_P, FALSE, _("after:"), leader, rt);

	_e2p_find_make_all_spinners (vbox, &rt->mtime);

	hbox = _e2p_find_create_hbox (vbox);
#ifdef E2_ASSISTED
	GtkWidget *button =
#endif
	_e2p_find_create_toggle_grouped_button
		(hbox, MTIME_REL_P, FALSE, _("during the preceding:"), leader, rt);
	rt->mrel_entry = _e2p_find_create_entry (hbox, entries [MREL_ENTRY]);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (button, rt->mrel_entry);
#endif
	rt->mrel_combo = _e2p_find_create_combo (hbox, periods, PERIODCOUNT, 1); //default days

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT (vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
/**
@brief make notebook tab with file atime search options

Options are: earlier than and/or equal to, later than and/or equal to.
By default, nothing is selected, so all atimes will be matched

@param notebook widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_atime_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("access"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
	e2_widget_add_mid_label (vbox, _("and which were most-recently opened or executed:"), LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);
	GtkWidget *hbox = _e2p_find_create_hbox (vbox);
	GtkWidget *leader = _e2p_find_create_toggle_grouped_button
		(hbox, ATIME_LT_P, FALSE, _("before:"), NULL, rt);
	_e2p_find_create_toggle_button (hbox, ATIME_EQ_P, FALSE, _("on/at:"), rt);
	_e2p_find_create_toggle_grouped_button
		(hbox, ATIME_GT_P, FALSE, _("after:"), leader, rt);

	_e2p_find_make_all_spinners (vbox, &rt->atime);
//	.day_spin, &rt->atime.month_spin, &rt->atime.year_spin,
//				&rt->atime.hour_spin, &rt->atime.minute_spin); //, &rt->atime.second_spin);

	hbox = _e2p_find_create_hbox (vbox);
#ifdef E2_ASSISTED
	GtkWidget *button =
#endif
	_e2p_find_create_toggle_grouped_button
		(hbox, ATIME_REL_P, FALSE, _("during the preceding:"), leader, rt);
	rt->arel_entry = _e2p_find_create_entry (hbox, entries[AREL_ENTRY]);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (button, rt->arel_entry);
#endif
	rt->arel_combo = _e2p_find_create_combo (hbox, periods, PERIODCOUNT, 1); //default days

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT (vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
/**
@brief make notebook tab with filen ctime search options

Options are: earlier than and/or equal to, later than and/or equal to.
By default, nothing is selected, so all ctimes will be matched

@param notebook notebook widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_ctime_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("property"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
	e2_widget_add_mid_label (vbox, _("and which had any property (inode) change:"), LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);
	GtkWidget *hbox = _e2p_find_create_hbox (vbox);
	GtkWidget *leader = _e2p_find_create_toggle_grouped_button
		(hbox, CTIME_LT_P, FALSE, _("before:"), NULL, rt);
	_e2p_find_create_toggle_button (hbox, CTIME_EQ_P, FALSE, _("on/at:"), rt);
	_e2p_find_create_toggle_grouped_button
		(hbox, CTIME_GT_P, FALSE, _("after"), leader, rt);

	_e2p_find_make_all_spinners (vbox, &rt->ctime);
//	.day_spin, &rt->ctime.month_spin, &rt->ctime.year_spin,
//				&rt->ctime.hour_spin, &rt->ctime.minute_spin); //, &rt->ctime_second_spin);

	hbox = _e2p_find_create_hbox (vbox);
#ifdef E2_ASSISTED
	GtkWidget *button =
#endif
	_e2p_find_create_toggle_grouped_button
		(hbox, CTIME_REL_P, FALSE, _("during the preceding:"), leader, rt);
	rt->crel_entry = _e2p_find_create_entry (hbox, entries[CREL_ENTRY]);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (button, rt->crel_entry);
#endif
	rt->crel_combo = _e2p_find_create_combo (hbox, periods, PERIODCOUNT, 1); //default days

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT (vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
/**
@brief make notebook tab with file permissions search options

By default, nothing is selected, so any permissions will be matched

@param notebook widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_mode_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("permission"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
	e2_widget_add_mid_label (vbox, _("and whose permissions:"), LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);

	GtkWidget *hbox = _e2p_find_create_hbox (vbox);
	GtkWidget *radio = _e2p_find_create_radio_button (hbox, NULL,
		MODE_IS_P, FALSE, _("are"), rt);
	_e2p_find_create_radio_button (hbox, radio,
		MODE_OR_P, TRUE, _("include"), rt);	//include nothing = default setting
	_e2p_find_create_radio_button (hbox, radio,
		MODE_NOT_P, FALSE, _("exclude"), rt);

	hbox = _e2p_find_create_hbox (vbox);
	// read
	GtkWidget *vbox2 = _e2p_find_create_vbox (hbox);
	_e2p_find_create_toggle_button (vbox2,
		OWNER_READ_P, FALSE, _("owner read"), rt);
	_e2p_find_create_toggle_button (vbox2,
		GROUP_READ_P, FALSE, _("group read"), rt);
	_e2p_find_create_toggle_button (vbox2,
		WORLD_READ_P, FALSE, _("anyone read"), rt);
	// write
	vbox2 = _e2p_find_create_vbox (hbox);
	_e2p_find_create_toggle_button (vbox2,
			OWNER_WRITE_P, FALSE, _("owner write"), rt);
	_e2p_find_create_toggle_button (vbox2,
		GROUP_WRITE_P, FALSE, _("group write"), rt);
	_e2p_find_create_toggle_button (vbox2,
		WORLD_WRITE_P, FALSE, _("anyone write"), rt);
	// exec
	vbox2 = _e2p_find_create_vbox (hbox);
	_e2p_find_create_toggle_button (vbox2,
		OWNER_EXEC_P, FALSE, _("owner execute"), rt);
	_e2p_find_create_toggle_button (vbox2,
		GROUP_EXEC_P, FALSE, _("group execute"), rt);
	_e2p_find_create_toggle_button (vbox2,
		WORLD_EXEC_P,	FALSE, _("anyone execute"), rt);
	// extra bits
	vbox2 = _e2p_find_create_vbox (hbox);
	_e2p_find_create_toggle_button (vbox2,
		SETUID_P, FALSE, _("setuid"), rt);
	_e2p_find_create_toggle_button (vbox2,
		SETGID_P, FALSE, _("setgid"), rt);
	_e2p_find_create_toggle_button (vbox2,
		STICKY_P, FALSE, _("sticky"), rt);

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT (vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
/**
@brief make notebook tab with file type search options

By default, nothing is selected, so all types will be matched

@param notebook widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_type_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("type"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
	e2_widget_add_mid_label (vbox, _("and which:"), LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);
	GtkWidget *hbox = _e2p_find_create_hbox (vbox);
	GtkWidget *radio = _e2p_find_create_radio_button (hbox, NULL,
		TYPE_IS_P, TRUE, _("are"), rt);
	_e2p_find_create_radio_button (hbox, radio,
		TYPE_NOT_P, FALSE, _("are not"), rt);

	hbox = _e2p_find_create_hbox (vbox);
	GtkWidget *subvbox = _e2p_find_create_vbox (hbox);

	//FIXME use a table to align items horizontally as well
	_e2p_find_create_toggle_button (subvbox,
		REGULAR_P, FALSE, _("regular"), rt);
	_e2p_find_create_toggle_button (subvbox,
		DIRECTORY_P, FALSE, _("directory"), rt);
	_e2p_find_create_toggle_button (subvbox,
		SYMLINK_P, FALSE, _("symbolic link"), rt);
#ifdef TRACKERFIND
	_e2p_find_create_toggle_button (subvbox,
		BLOCK_DEVICE_P, FALSE, _("block device"), rt);
#endif

	subvbox = _e2p_find_create_vbox (hbox);
#ifdef TRACKERFIND
	if (rt->content_pattern2 != NULL)
	{	//tracker's presence was successfully tested when content tab was created
		rt->service_combo = _e2p_find_create_combo (subvbox, object_names,
			ACTIVE_TRACKER_SERVICES, service_index);
		if (_e2p_find_get_flag (TYPE_NOT_P) || !_e2p_find_get_flag (REGULAR_P))
			gtk_widget_set_sensitive (rt->service_combo, FALSE);
	}
	else
		rt->service_combo = NULL;
#else
	_e2p_find_create_toggle_button (subvbox,
		BLOCK_DEVICE_P, FALSE, _("block device"), rt);
#endif
	_e2p_find_create_toggle_button (subvbox,
		RAW_DEVICE_P, FALSE, _("raw device"), rt);
	_e2p_find_create_toggle_button (subvbox,
		SOCKET_P, FALSE, _("socket"), rt);
	_e2p_find_create_toggle_button (subvbox,
		FIFO_P, FALSE, _("fifo"), rt);

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT (vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
/**
@brief make notebook tab with file user/group search options

By default, any user and any group are selected

@param notebook widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_owner_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("owners"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
#ifdef E2_ASSISTED
	GtkWidget *label2 =
#endif
	e2_widget_add_mid_label (vbox, _("and with:"), LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);

	GtkWidget *hbox = _e2p_find_create_hbox (vbox);

	//user
	GtkWidget *vbox2 = _e2p_find_create_vbox (hbox);
	GtkWidget *radio =
	_e2p_find_create_radio_button (vbox2, NULL, UID_ANY_P, TRUE,
		_("any user id"), rt);
	_e2p_find_create_radio_button (vbox2, radio, UID_SPECIFIC_P, FALSE,
		_("specific user id"), rt);
	find_rt->curr_user =
	_e2p_find_create_toggle_grouped_button (vbox2, UID_LOGIN_P, FALSE,
		_("current user's uid"), NULL, rt);
	find_rt->choose_user =
	_e2p_find_create_toggle_grouped_button (vbox2, UID_NOT_LOGIN_P, FALSE,
		_("this user id"), find_rt->curr_user, rt);
	//entry for specified uid
	rt->user_entry = _e2p_find_create_entry (vbox2, entries [UID_ENTRY]);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label2, rt->user_entry);
#endif
	_e2p_find_create_radio_button (vbox2, radio, UID_NONE_P, FALSE,
		_("unregistered user"), rt);
	gboolean status = (_e2p_find_get_flag (UID_ANY_P) || _e2p_find_get_flag (UID_NONE_P));	//, rt));
	if (status)
	{
		gtk_widget_set_sensitive (find_rt->curr_user, FALSE);
		gtk_widget_set_sensitive (find_rt->choose_user, FALSE);
		gtk_widget_set_sensitive (find_rt->user_entry, FALSE);
	}
	else	//UID_SPECIFIC_P
	{
		gtk_widget_set_sensitive (find_rt->curr_user, TRUE);
		gtk_widget_set_sensitive (find_rt->choose_user, TRUE);
		gtk_widget_set_sensitive (find_rt->user_entry,
			_e2p_find_get_flag (UID_NOT_LOGIN_P));	//, find_rt));
	}
	// group
	vbox2 = _e2p_find_create_vbox (hbox);
	radio =
	_e2p_find_create_radio_button (vbox2, NULL, GID_ANY_P, TRUE,
		_("any group id"), rt);
	_e2p_find_create_radio_button (vbox2, radio, GID_SPECIFIC_P, FALSE,
		_("specific group id"), rt);
	find_rt->curr_group =
	_e2p_find_create_toggle_grouped_button (vbox2, GID_LOGIN_P, FALSE,
		_("current user's gid"), NULL, rt);
	find_rt->choose_group =
	_e2p_find_create_toggle_grouped_button (vbox2, GID_NOT_LOGIN_P, FALSE,
		_("this group id"), find_rt->curr_group, rt);
	//entry for specified gid
	rt->group_entry = _e2p_find_create_entry (vbox2, entries [GID_ENTRY]);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label2, rt->group_entry);
#endif
	_e2p_find_create_radio_button (vbox2, radio, GID_NONE_P, FALSE,
		_("unregistered group"), rt);
	status = (_e2p_find_get_flag (GID_ANY_P) || _e2p_find_get_flag (GID_NONE_P));	//, rt));
	if (status)
	{
		gtk_widget_set_sensitive (rt->curr_group, FALSE);
		gtk_widget_set_sensitive (rt->choose_group, FALSE);
		gtk_widget_set_sensitive (rt->group_entry, FALSE);
	}
	else	//GID_SPECIFIC_P
	{
		gtk_widget_set_sensitive (find_rt->curr_group, TRUE);
		gtk_widget_set_sensitive (find_rt->choose_group, TRUE);
		gtk_widget_set_sensitive (find_rt->group_entry,
			_e2p_find_get_flag (GID_NOT_LOGIN_P));	//, find_rt));
	}

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT (vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}
/**
@brief make notebook tab with file content search options

matches simple substrings by default, but may match wildcards.
If you want regexps, use grep !!

@param notebook widget to which the tab wiil be added
@param rt ptr to dialog data struct

@return
*/
static void _e2p_find_make_content_tab (GtkWidget *notebook, E2_FindDialogRuntime *rt)
{
	GtkWidget *label = gtk_label_new (_("content"));
#ifdef USE_GTK3_0
	GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, E2_PADDING_XSMALL);
#else
	GtkWidget *vbox = gtk_vbox_new (FALSE, E2_PADDING_XSMALL);
#endif
#ifdef E2_ASSISTED
	GtkWidget *label2 =
#endif
	e2_widget_add_mid_label (vbox,
#ifdef TRACKERFIND
		_("Using grep, find files with content that:"),
#else
		_("and with content that:"),
#endif
		LABEL_ALIGN, FALSE, E2_PADDING_XSMALL);

	GtkWidget *hbox = _e2p_find_create_hbox (vbox);
	GtkWidget *radio = _e2p_find_create_radio_button (hbox, NULL,
		STRING_CONTENT_P, TRUE, _("is"), rt);
	_e2p_find_create_radio_button (hbox, radio,
		WILDCARD_CONTENT_P, FALSE, _("is like"), rt);
	_e2p_find_create_radio_button (hbox, radio,
		REGEXP_CONTENT_P, FALSE, _("matches this regex"), rt);
	_e2p_find_create_toggle_button (hbox,
		ANYCASE_CONTENT_P, FALSE, _("ignore case"), rt);

	hbox = _e2p_find_create_hbox (vbox);
//	hbox = e2_widget_add_box (vbox, TRUE, 0, TRUE, FALSE, E2_PADDING);
	rt->content_pattern = _e2p_find_create_entry (hbox, entries [CONTENT_ENTRY]);
#ifdef E2_ASSISTED
	e2_widget_set_label_relations (label2, rt->content_pattern);
#endif

//	hbox = _e2p_find_create_hbox (vbox);
//	_e2p_find_create_toggle_button (hbox,
//		ANYCASE_CONTENT_P, FALSE, _("ignore case"), rt);

#ifdef TRACKERFIND
	//check whether tracker is available for content-matching
	/*tracker-status command prints:
		Tracker status is 'status'
		where status is any of:
		Initializing,Watching,Indexing,Pending,Optimizing,Idle,Shutdown
	*/
	gpointer found;
	if (e2_fs_get_command_output ("ps -C trackerd -o pid=", &found))
	{
		gchar *s = e2_utils_pass_whitespace ((gchar *)found);
		if (s != NULL && *s >= '0' && *s <= '9')
		{
#ifdef USE_GTK3_0
	 		hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING);
#else
			hbox = gtk_hbox_new (FALSE, E2_PADDING);
#endif
			GtkWidget *align = gtk_alignment_new (LABEL_ALIGN, 0.0, 0.0, 0.6);
			gtk_container_add (GTK_CONTAINER (align), hbox);
			gtk_box_pack_start (GTK_BOX (vbox), align, FALSE, FALSE, E2_PADDING_XSMALL);
			e2_widget_add_mid_label (hbox, _("Using"), 0.0, FALSE, E2_PADDING_SMALL);
			GtkWidget *button = _e2p_find_create_radio_button (hbox, radio,
				TRACK_CONTENT_P, FALSE, "tracker", rt);	//no translation
			//over-ride default packing arrangement
			gtk_box_set_child_packing (GTK_BOX (hbox), button, FALSE, FALSE, 0,
				 GTK_PACK_START);
#ifdef E2_ASSISTED
			label2 =
#endif
			e2_widget_add_mid_label (hbox, _("find files with content that is:"), 0.0, FALSE, E2_PADDING_SMALL);
			hbox = _e2p_find_create_hbox (vbox);
			rt->content_pattern2 =
			_e2p_find_create_entry (hbox, entries [CONTENT_ENTRY2]);
#ifdef E2_ASSISTED
			e2_widget_set_label_relations (label2, rt->content_pattern2);
#endif
		}
		else
			rt->content_pattern2 = NULL;
		g_free (found);
	}
	else
		rt->content_pattern2 = NULL;
#endif

	_e2p_find_notify_all_widgets (vbox, vbox);
	g_object_set_data (G_OBJECT (vbox), LABEL_DATAKEY, label);
	gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);
}

  /****************************/
 /** lesser widget creation **/
/****************************/

/**
@brief create and show a combobox widget in a specified box
Called only from context where BGL closed
@param box widget into which the combo is to be packed
@param history array of UTF-8 history items for the combo, or NULL
@param histcount no. of members in @a history
@param histdefault 0-based index of default history item, or -1

@return the combo widget
*/
static GtkWidget *_e2p_find_create_combo (GtkWidget *box, gchar **history,
	gint histcount, gint histdefault)
{
	GtkWidget *rv = e2_combobox_add (box, FALSE, E2_PADDING_XSMALL, NULL, NULL,
		NULL, E2_COMBOBOX_MENU_STYLE);
	if (history != NULL && histcount > 0)
	{
		e2_combobox_append_history_counted (rv, histcount, history);
		gtk_combo_box_set_active (GTK_COMBO_BOX (rv), histdefault);
	}
	else
		histdefault = -1;
	g_signal_connect (G_OBJECT (rv), "changed", G_CALLBACK (_e2p_find_widget_changed_cb), NULL);
	g_object_set_data (G_OBJECT (rv), "default_index", GINT_TO_POINTER (histdefault));
	g_object_set_data (G_OBJECT (rv), "reset_yourself", _e2p_find_reset_combo);
	return rv;
}
/**
@brief create and show an entry widget in a specified box

@param box widget into which the entry is to be packed
@param text the initial text to show in the entry

@return the entry widget
*/
static GtkWidget *_e2p_find_create_entry (GtkWidget *box, gchar *text)
{
	GtkWidget *rv = e2_widget_add_entry (box, text, TRUE, FALSE);
	g_signal_connect_after (G_OBJECT (rv), "key-release-event", G_CALLBACK (_e2p_find_widget_changed_cb), NULL);
	g_object_set_data (G_OBJECT (rv), "reset_yourself", _e2p_find_reset_entry);
	return rv;
}
/**
@brief create a hbox in @a box

@param box the widget into which the hbox is to be placed

@return the created box widget
*/
static GtkWidget *_e2p_find_create_hbox (GtkWidget *box)
{
#ifdef USE_GTK3_0
	GtkWidget *hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, E2_PADDING);
#else
	GtkWidget *hbox = gtk_hbox_new (FALSE, E2_PADDING);
#endif
	gtk_box_pack_start (GTK_BOX (box), hbox, FALSE, FALSE, E2_PADDING);
	return hbox;
}
/**
@brief create and show a button in a specified container

@param box the widget into which the button is to be placed
@param f enumerated value of flag to be associated with the button
@param state T/F default initial state of the toggle
@param label translated string for the button label
@param callback the "toggled"-signal callback for the button
@param rt ptr to dialog data struct

@return the button widget
*/
static GtkWidget *_e2p_find_create_toggle_button_real (GtkWidget *box,
	findflag_t f, gboolean state, gchar *label,
	void (*callback)(GtkToggleButton*,gpointer), E2_FindDialogRuntime *rt)
{
	gboolean first;
	if (nocacheflags)
	{
		first = state;
		//newly-initialised flags default to FALSE, we don't want that
		if (state)
			_e2p_find_set_flag (f, TRUE);	//, rt);
	}
	else
		first = _e2p_find_get_flag (f);	//, rt);

	GtkWidget *button = e2_button_add_toggle (box, TRUE, first,
		label, NULL, TRUE, 1, callback, (gpointer) f);
	g_object_set_data (G_OBJECT (button), "reset_yourself",
		state ? _e2p_find_set_toggle_button_on : _e2p_find_set_toggle_button_off );
	return button;
}
/**
@brief create and show a grouped toggle in a specified box

@param box the widget into which the button is to be placed
@param f enumerated value of flag to be associated with the button
@param state T/F default initial state of the toggle
@param label translated string for the button label
@param leader widget for the 'leader' of the group, or NULL if creating the leader
@param rt ptr to dialog data struct

@return the button widget (UNUSED, now)
*/
static GtkWidget *_e2p_find_create_toggle_grouped_button (GtkWidget *box,
	findflag_t f, gboolean state, gchar *label, GtkWidget *leader,
	E2_FindDialogRuntime *rt)
{
	GtkWidget *button = _e2p_find_create_toggle_button_real (box, f, state, label,
		_e2p_find_grouptoggle_cb, rt);
	GtkWidget *ptr;
	GSList *members;
	if (leader == NULL)
	{	//this is the leader of a new group
		ptr =	button;	//point to self
		members = NULL;
		rt->groups = g_slist_append (rt->groups, button);	//remember it, for cleaning up
	}
	else
	{	//this is a group member
		ptr = leader;	//point to group leader, which has list
		members = g_object_get_data (G_OBJECT (leader), "group_members");
	}
	g_object_set_data (G_OBJECT (button), "group_leader", ptr);
	members = g_slist_append (members, button);
	g_object_set_data (G_OBJECT (ptr), "group_members", members);
	return button;
}
/**
@brief create and show a toggle in a specified container

@param box the widget into which the button is to be placed
@param f enumerated value of flag to be associated with the button
@param state T/F initial state of the toggle
@param label translated string for the button label

@return the button widget (UNUSED, now)
*/
static GtkWidget *_e2p_find_create_toggle_button (GtkWidget *box, findflag_t f,
	gboolean state, gchar *label, E2_FindDialogRuntime *rt)
{
	GtkWidget *button = _e2p_find_create_toggle_button_real
		(box, f, state, label, _e2p_find_toggle_cb, rt);
	return button;
}
/**
@brief create and show a radio btn in a specified box
The leader of a group is initialized to TRUE, other group members
may cause that to be changed
@param box the widget into which the button is to be placed
@param leader the leader of the radio group, or NULL if this is the leader
@param f enumerated value of flag to be associated with the button
@param state the default state of the button T/F
@param label translated string for the button label
@param rt ptr to dialog data struct

@return the button widget
*/
static GtkWidget *_e2p_find_create_radio_button (GtkWidget *box, GtkWidget *leader,
	findflag_t f, gboolean state, gchar *label, E2_FindDialogRuntime *rt)
{
	gboolean first;
	if (nocacheflags)
	{
		first = state;
		//newly-initialised flags default to FALSE, we don't want that
		if (state)
			_e2p_find_set_flag (f, TRUE);	//, rt);
	}
	else
	{
		first = _e2p_find_get_flag (f);	//, rt);
	}

	GSList *group = (leader == NULL) ?
		NULL : gtk_radio_button_get_group (GTK_RADIO_BUTTON (leader));
	GtkWidget *button = e2_button_add_radio (box, label, group, first, TRUE, 1,
		_e2p_find_toggle_cb, (gpointer) f);

	g_object_set_data (G_OBJECT (button), "reset_yourself",
		(state) ? _e2p_find_set_toggle_button_on : _e2p_find_set_toggle_button_off );
	return button;
}
/* *
@brief create and show a radio btn in a specified container

@param container the widget into which the button is to be placed
@param leader the radio button widget that 'leads' the group
@param f enumerated value of flag to be associated with the button
@param state default state of the toggle T/F
@param label translated string for the button label
@param rt ptr to dialog data struct

@return the button widget
*/
/*static GtkWidget *_e2p_find_create_radio_grouped_button (GtkWidget *box,
	GtkWidget *leader, findflag_t f, gboolean state, gchar *label,
	E2_FindDialogRuntime *rt)
{
	gboolean first;
	if (nocacheflags)
	{
		first = state;
		//newly-initialised flags default to FALSE, we don't want that
		if (state)
			_e2p_find_set_flag (f, TRUE);	//, rt);
	}
	else
		first = _e2p_find_get_flag (f);	//, rt);

	GSList *group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (leader));
	//this packs box with expand and fill set ...
	GtkWidget *button = e2_button_add_radio (box, label, group, first, TRUE, 0,
		_e2p_find_toggle_cb, (gpointer) f);
	g_object_set_data (G_OBJECT (button), "reset_yourself",
		(state) ? _e2p_find_set_toggle_button_on : _e2p_find_set_toggle_button_off );
	return button;
} */
/**
@brief create a spin button

@param default_value ptr to value to be stored
@param min_value minimum allowed value for the button
@param max_value maximum allowed value for the button

@return the button widget
*/
static GtkWidget *_e2p_find_create_spin_button (gfloat *default_value,
	gdouble min_value, gdouble max_value)
{
#ifdef USE_GTK3_0
	GtkAdjustment *adj
#else
	GtkObject *adj
#endif
	= gtk_adjustment_new ((gdouble)*default_value, min_value,
		max_value, 1.0, 2.0, 0.0);
#ifdef USE_GTK3_0
	GtkWidget *button = gtk_spin_button_new (adj, 1.0, 0);
#else
	GtkWidget *button = gtk_spin_button_new (GTK_ADJUSTMENT (adj), 1.0, 0);
#endif
	gtk_spin_button_set_wrap (GTK_SPIN_BUTTON (button), TRUE);
	g_signal_connect (G_OBJECT (button), "value-changed", G_CALLBACK (_e2p_find_widget_changed_cb), NULL);
	g_object_set_data (G_OBJECT (button), "default_value", default_value);
	g_object_set_data (G_OBJECT (button), "reset_yourself", _e2p_find_reset_spin_button);
	return button;
}
/**
@brief add date and time spinners to specified container

@param box widget to which the spinners are to be added
@param time ptr to spinners data struct for mtime, atime or ctime

@return
*/
static void _e2p_find_make_all_spinners (GtkWidget *box, spinners *time)
{
	GtkWidget *hbox = _e2p_find_create_hbox (box);
	//day widget
	GtkWidget *vbox = _e2p_find_create_vbox (hbox);
	_e2p_find_create_label (vbox, _("Day"));
//	*day
	time->day_spin = _e2p_find_create_spin_button (&current.day, 1.0, 31.0);
	gtk_box_pack_start (GTK_BOX (vbox), time->day_spin, FALSE, FALSE, E2_PADDING_XSMALL);

	//month widget
	vbox = _e2p_find_create_vbox (hbox);
	_e2p_find_create_label (vbox, _("Month"));
//	*month
	time->month_spin = _e2p_find_create_spin_button (&current.month, 1.0, 12.0);
	gtk_box_pack_start (GTK_BOX (vbox), time->month_spin, FALSE, FALSE, E2_PADDING_XSMALL);
	g_signal_connect_after (G_OBJECT (time->month_spin), "changed",
		G_CALLBACK (_e2p_find_month_changed_cb), time);

	//year widget
	vbox = _e2p_find_create_vbox (hbox);
	_e2p_find_create_label (vbox, _("Year"));
//	*year
	time->year_spin = _e2p_find_create_spin_button (&current.year, 0.0, 9999.0);
//	gtk_widget_set_size_size_request (*year, 55, -1); // make it 4 digits wide
	gtk_box_pack_start (GTK_BOX (vbox), time->year_spin, FALSE, FALSE, E2_PADDING_XSMALL);
	g_signal_connect_after (G_OBJECT (time->year_spin), "changed",
		G_CALLBACK (_e2p_find_year_changed_cb), time);

	//hour widget
	vbox = _e2p_find_create_vbox (hbox);
	_e2p_find_create_label (vbox, _("Hour"));
//	*hour
	time->hour_spin = _e2p_find_create_spin_button (&current.hour, 0.0, 23.0);
	gtk_box_pack_start (GTK_BOX (vbox), time->hour_spin, FALSE, FALSE, E2_PADDING_XSMALL);

	//minute widget
	vbox = _e2p_find_create_vbox (hbox);
	_e2p_find_create_label (vbox, _("Minute"));
//	*minute
	time->minute_spin = _e2p_find_create_spin_button (&current.minute, 0.0, 59.0);
	gtk_box_pack_start (GTK_BOX (vbox), time->minute_spin, FALSE, FALSE, E2_PADDING_XSMALL);

/*	//second widget
	vbox = _e2p_find_create_vbox (hbox);
	label = _e2p_find_create_label (vbox, _("Second"));
	GtkWidget *second = _e2p_find_create_spin_button (&current.second, 0.0, 59.0);
	gtk_box_pack_start (GTK_BOX (vbox), second, FALSE, FALSE, E2_PADDING_XSMALL); */

	NEEDOPENBGL
	//init proper days per month
	_e2p_find_month_changed_cb (time->month_spin, time);
	NEEDCLOSEBGL
}
/**
@brief establish and show find dialog
There is no compelling reason to queue this, it does not necessarily work on
either displayed filelist
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2p_find_dialog_create (gpointer from, E2_ActionRuntime *art)
{
	//because we use static var's, check if there is already a config dialog opened
	pthread_mutex_lock (&find_mutex);
	if (find_rt != NULL)
	{
		gtk_window_present (GTK_WINDOW (find_rt->dialog));
		pthread_mutex_unlock (&find_mutex);
		return TRUE;
	}
	//init runtime object
	find_rt = ALLOCATE (E2_FindDialogRuntime);
	pthread_mutex_unlock (&find_mutex);
	CHECKALLOCATEDWARN (find_rt, return FALSE;)
	find_rt->groups = NULL;
	find_rt->matchdata = NULL;	//for cleanups before any search is run

	gint startpage = page_store;	//preserve this
	//create dialog
	find_rt->dialog = e2_dialog_create (NULL, NULL, _("find"),
		(ResponseFunc)_e2p_find_response_cb, find_rt);
	OPENBGL
	GtkWidget *dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (find_rt->dialog));
#else
		GTK_DIALOG (find_rt->dialog)->vbox;
#endif
	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);
	//populate it
	//first with things for selecting the seach starting place
	_e2p_find_make_directory_widgets (dialog_vbox, find_rt);
	e2_widget_add_separator (dialog_vbox, FALSE, E2_PADDING_SMALL);
	//then with things to identify the files of interest
	_e2p_find_make_notebook (dialog_vbox, find_rt);

	//open at same page as last time
	if (startpage > 0)
		gtk_notebook_set_current_page (GTK_NOTEBOOK (find_rt->notebook), startpage);

	//add buttons in the order that they will appear
	find_rt->help_button =
	e2_dialog_add_custom_button_full
		(find_rt->dialog, FALSE, E2_RESPONSE_USER2, _("_Help"), STOCK_NAME_HELP,
		_("Get advice on search options on displayed tab"), _e2p_find_help_cb, find_rt);
/*	//omit this
	button = _e2p_find_create_button (hbox, _e2p_find_save_search_cb, NULL);
	label = _e2p_find_create_label (button, _("_Save"));
*/
	e2_dialog_add_custom_button_full (find_rt->dialog, FALSE, E2_RESPONSE_USER1,
		_("Clea_r"), STOCK_NAME_CLEAR, _("Clear all search parameters"),
		_e2p_find_clear_find_cb, find_rt);
	E2_Button local_btn = { _("_Stop"), STOCK_NAME_STOP, NULL, E2_BTN_TIPPED, 0, E2_RESPONSE_NOTOALL };
	find_rt->stop_button =
	e2_dialog_add_custom_button (find_rt->dialog, &local_btn, FALSE,
		_("Stop the current search"), _e2p_find_stop_find_cb, find_rt);
	//de-sensitize stop btn, at this stage
	gtk_widget_set_sensitive (find_rt->stop_button, FALSE);
	e2_dialog_add_defined_button (find_rt->dialog, &E2_BUTTON_CLOSE);
	find_rt->start_button =
	e2_dialog_add_custom_button_full (find_rt->dialog, FALSE,
		E2_RESPONSE_FIND, _("_Find"), STOCK_NAME_FIND, _("Begin searching"),
		_e2p_find_find_cb, find_rt);
	e2_dialog_set_negative_response (find_rt->dialog, GTK_RESPONSE_CLOSE);

	gboolean state;
	if (nocacheflags)
	{	//no cached flags were used in this se
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (find_rt->active_button), TRUE);
		state = FALSE;
		nocacheflags = FALSE;	//from now on, use static/cached flag values
	}
	else
		state = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (find_rt->thisdir_button));
	gtk_widget_set_sensitive (find_rt->directory, state);
	gtk_widget_set_sensitive (find_rt->chooser_button, state);

	state = _e2p_find_get_flag (SEARCH_SUBDIRS_P);
	gtk_widget_set_sensitive (find_rt->inlink_button, state);

	CLOSEBGL
	e2_dialog_setup (find_rt->dialog, app.main_window);
	gtk_widget_show_all (find_rt->dialog);

	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONE_START(_A(1),_("detfind"),_e2p_find_dialog_create,
		_("_Find.."),
		_("Find and list items, using detailed criteria"),
		"plugin_" ANAME E2ICONTB)

	nocacheflags = !e2_cache_check ("find-plugin-flags");
	if (nocacheflags)
		//initialise all flags to FALSE
		//(some will be changed when specific widgets are created)
		_e2p_find_reset_flags ();	//find_rt);
	e2_cache_array_register ("find-plugin-flags", MAX_FLAGS, flags, flags);

	guint i;
	e2_cache_list_register ("find-plugin-strings", &strings);
	if (strings == NULL)
	{	//no cache found for this item
		//init entry strings, with fillers in case session ends before dialog is closed
		for (i = 0; i < MAX_ENTRIES; i++)
			strings = g_list_append (strings, g_strdup ("."));
	}
	else	//REMOVEME when things are stable
	{	//when mucking around with API's we don't want to crash due to
		//too-few cached strings
		guint n = g_list_length (strings);
		if (n != MAX_ENTRIES)
		{
			e2_list_free_with_data (&strings);
			for (i = 0; i < MAX_ENTRIES; i++)
				strings = g_list_append (strings, g_strdup ("."));
		}
	}

	for (i = 0; i < MAX_ENTRIES; i++)
	{
		//allocate new strings because string and array may be cleared separately
		gchar *str = (gchar *)g_list_nth_data (strings, i);
		//remove cached filler string
		if (!strcmp (str, "."))
			str = "";
		entries[i] = g_strdup (str);
	}

	for (i = 0; i < PERIODCOUNT; i++)
		periods [i] = gettext (periods[i]);
#ifdef TRACKERFIND
	for (i = 0; i < ACTIVE_TRACKER_SERVICES; i++)
		object_names [i] = gettext (object_names[i]);
#endif

	//setup mutex to protect threaded access to find data
	pthread_mutexattr_t attr;
	pthread_mutexattr_init (&attr);
	pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init (&find_mutex, &attr);

	PLUGINIT_ONE_END
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)
	if (ret)
	{
		//backup the cache data
		e2_cache_unregister ("find-plugin-flags");
		e2_cache_unregister ("find-plugin-strings");
		//cleanup
		e2_list_free_with_data (&strings);	//this also clears the array
	}
	return ret;
}
