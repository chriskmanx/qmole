/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 2001-2012 Match Grun and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

/*
 * Definitions for accessing JPilot database files.
 * JPilot is Copyright(c) by Judd Montgomery.
 * Visit http://www.jpilot.org for more details.
 */

#ifndef __JPILOT_H__
#define __JPILOT_H__

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#ifdef USE_JPILOT

#include <glib.h>
#include <stdio.h>

#ifdef HAVE_LIBPISOCK_PI_ADDRESS_H
#  include <libpisock/pi-address.h>
#else
#  include <pi-address.h>
#endif

#include "addritem.h"
#include "addrcache.h"
#include "adbookbase.h"

typedef struct _JPilotFile JPilotFile;

struct _JPilotFile {
	AddressBookType type;
	AddressCache *addressCache;
	gint     retVal;
	FILE     *file;
	gchar    *path;
	struct AddressAppInfo addrInfo;
	gboolean readMetadata;
	GList    *customLabels;
	GList    *labelInd;
	gboolean havePC3;
	time_t   pc3ModifyTime;
};

/* Limits */
#define JPILOT_NUM_LABELS	22	/* Number of labels */
#define JPILOT_NUM_PHONELABELS	8 	/* Number of phone number labels */
#define JPILOT_NUM_CATEG	16	/* Number of categories */
#define JPILOT_LEN_LABEL	15	/* Max length of label */
#define JPILOT_LEN_CATEG	15	/* Max length of category */
#define JPILOT_NUM_ADDR_PHONE	5	/* Number of phone entries a person */
					/* can have */

/* Function prototypes */
JPilotFile *jpilot_create		( void );
JPilotFile *jpilot_create_path		( const gchar *path );
void jpilot_set_name			( JPilotFile* pilotFile, const gchar *value );
void jpilot_set_file			( JPilotFile* pilotFile, const gchar *value );
void jpilot_free			( JPilotFile *pilotFile );
gint jpilot_get_status			( JPilotFile *pilotFile );
gboolean jpilot_get_modified		( JPilotFile *pilotFile );
gboolean jpilot_get_accessed		( JPilotFile *pilotFile );
void jpilot_set_accessed		( JPilotFile *pilotFile, const gboolean value );
gboolean jpilot_get_read_flag		( JPilotFile *pilotFile );
ItemFolder *jpilot_get_root_folder	( JPilotFile *pilotFile );
gchar *jpilot_get_name			( JPilotFile *pilotFile );

gint jpilot_read_data			( JPilotFile *pilotFile );
GList *jpilot_get_list_person		( JPilotFile *pilotFile );
GList *jpilot_get_list_folder		( JPilotFile *pilotFile );
GList *jpilot_get_all_persons		( JPilotFile *pilotFile );

GList *jpilot_load_custom_label		( JPilotFile *pilotFile, GList *labelList );

gchar *jpilot_find_pilotdb		( void );


void jpilot_clear_custom_labels		( JPilotFile *pilotFile );
void jpilot_add_custom_label		( JPilotFile *pilotFile, const gchar *labelName );
GList *jpilot_get_custom_labels		( JPilotFile *pilotFile );
gboolean jpilot_test_custom_label	( JPilotFile *pilotFile, const gchar *labelName );
gboolean jpilot_test_pilot_lib		( void );

gint jpilot_read_modified		( JPilotFile *pilotFile );

#endif /* USE_JPILOT */

#endif /* __JPILOT_H__ */
