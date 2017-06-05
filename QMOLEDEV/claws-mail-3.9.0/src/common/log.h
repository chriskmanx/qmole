/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef LOG_H
#define LOG_H

#ifdef HAVE_CONFIG_H
#include "claws-features.h"
#endif

#include <glib.h>

#define LOG_APPEND_TEXT_HOOKLIST "log_append_text"
#define DEBUG_FILTERING_APPEND_TEXT_HOOKLIST "debug_append_text"

#define LOG_TIME_LEN 11

typedef enum
{
	LOG_PROTOCOL = 0,
	LOG_DEBUG_FILTERING,
	/* reserved */
	LOG_INSTANCE_MAX
} LogInstance;

typedef enum
{
	LOG_NORMAL,
	LOG_MSG,
	LOG_WARN,
	LOG_ERROR,
	LOG_STATUS_OK,
	LOG_STATUS_NOK,
	LOG_STATUS_SKIP
} LogType;

typedef struct _LogText LogText;

struct _LogText
{
	LogInstance  instance;
	gchar		*text;
	LogType		 type;	
};

/* logging */
void set_log_file	(LogInstance instance, const gchar *filename);
void close_log_file	(LogInstance instance);
const char *get_log_hook(LogInstance instance);
void set_log_title(LogInstance instance, gchar *title);
gchar *get_log_title(LogInstance instance);
void set_log_prefs(LogInstance instance, int* logwin_width, int* logwin_height);
void get_log_prefs(LogInstance instance, int** logwin_width, int** logwin_height);
void log_print		(LogInstance instance, const gchar *format, ...) G_GNUC_PRINTF(2, 3);
void log_message	(LogInstance instance, const gchar *format, ...) G_GNUC_PRINTF(2, 3);
void log_warning	(LogInstance instance, const gchar *format, ...) G_GNUC_PRINTF(2, 3);
void log_error		(LogInstance instance, const gchar *format, ...) G_GNUC_PRINTF(2, 3);
void log_status_ok	(LogInstance instance, const gchar *format, ...) G_GNUC_PRINTF(2, 3);
void log_status_nok	(LogInstance instance, const gchar *format, ...) G_GNUC_PRINTF(2, 3);
void log_status_skip	(LogInstance instance, const gchar *format, ...) G_GNUC_PRINTF(2, 3);

#endif /* LOG_H */
