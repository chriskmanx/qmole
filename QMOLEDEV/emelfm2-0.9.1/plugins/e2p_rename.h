/* $Id: e2p_rename.h 3009 2014-01-21 07:09:21Z tpgww $

Copyright (C) 2005-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software. You can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file plugins/e2p_rename.h
@brief header for file-rename plugin
*/

#ifndef __E2P_RENAME_H__
#define __E2P_RENAME_H__

#include "emelfm2.h"
#include "e2_task.h"

//enable file-chooser button (depends on resolution of older? gtk bug
//gtk_file_chooser_button_new() always crashes in non-main thread
#define E2_RENCHOOSER

typedef enum
{
	SEARCH_ALL_P = 0,
	SEARCH_ALLACTIVE_P,
	SEARCH_ALLINACTIVE_P, //for variable namespaces
	SEARCH_CURRENT_P,
	SEARCH_OTHER_P,
	SEARCH_THIS_P,
	SEARCH_SUBDIRS_P,
	OLD_SEL_P,
	OLD_WILD_P,
	OLD_REGEX_P,
	NEW_UPPER_P,
	NEW_LOWER_P,
	NEW_THIS_P,
	CONFIRM_P,
	MAX_RENFLAGS	//no. of entries in the array
} renflag_t ;

typedef enum
{
	E2PR_NORMAL  = 1,
	E2PR_SEL     = 1 << 1,
	E2PR_WILD    = 1 << 2,
	E2PR_REGEX   = 1 << 3,
	E2PR_RECURSE = 1 << 4,
	E2PR_LOWER   = 1 << 5,//for case conversions
	E2PR_UPPER   = 1 << 6,
	E2PR_PATTERN = 1 << 7,//some replacement pattern provided
	E2PR_NEWALL  = 1 << 8,//replacememt has nothing wild in it
	E2PR_WHOLE   = 1 << 9,//replacement with one or more "\0"
	E2PR_COUNTER = 1 << 10 //replacement with one or more %c[n[,m]
} E2P_RenFlags;

typedef struct _E2_RenDialogRuntime
{
	GtkWidget *dialog;
	GtkWidget *directory;	// entry with text for directory to search in/from
	GtkWidget *pattern;		// entry with text for file name to search for
	GtkWidget *newpattern;	// entry with text for file name to substitute
#ifdef E2_RENCHOOSER
	GtkWidget *chooser_box;	//hbox containing chooser_button
	GtkWidget *chooser_button;	//file chooser
	gchar *chooser_startdir;	//localised path to initialize chooser
#endif
	GtkWidget *stop_button;  // button widget, remembered for changing sensitivity
	GtkWidget *start_button;  // ditto
	GtkWidget *help_button;  // ditto
	GtkWidget *active_button;
	GtkWidget *recurse_button;
	GtkWidget *wild_button;
	GSList *groups;  //list of lists of grouped toggle btns
	gboolean parsed;	//flag for whether the replacemment pattern has been converted to chunks
	gboolean abort;  //flag for stop btn pressed when querying a rename
	E2_TaskStatus *status;	//store for changing queued task status
	//Parts of replacement-name which are 'between backrefs', or 'between wildchars'
	//and so substitued as is
	//When replacement includes "\0" or has nothing wild, the first part holds the whole replacement
	GPtrArray *nchunks;
	E2P_RenFlags modeflags;
} E2_RenDialogRuntime;

#endif //ndef __E2P_RENAME_H__
