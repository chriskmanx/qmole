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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#include "defs.h"

#ifdef G_OS_WIN32
#  include <w32lib.h>
#endif

#include <stdio.h>
#include <glib.h>
#include <glib/gi18n.h>

#include "utils.h"
#include "log.h"
#include "hooks.h"

#define FWRITE(_b,_s,_n,_f)	if (fwrite(_b,_s,_n,_f) != _n) { \
					g_message("log fwrite failed!\n"); \
					return; \
				}
#define FPUTS(_b,_f)		if (fputs(_b,_f) == EOF) { \
					g_message("log fputs failed!\n"); \
					return; \
				}
#define FFLUSH(_f)		if (fflush(_f) != 0) { \
					g_message("log fflush failed!\n"); \
					return; \
				}

static FILE *log_fp[LOG_INSTANCE_MAX] = {
	NULL,
	NULL
};

static size_t log_size[LOG_INSTANCE_MAX] = {
	0,
	0
};

static gchar *log_filename[LOG_INSTANCE_MAX] = {
	NULL,
	NULL
};

typedef struct _LogInstanceData LogInstanceData;

struct _LogInstanceData {
	const char *hook;
	gchar *title;
	int *prefs_logwin_width;
	int *prefs_logwin_height;
};

static LogInstanceData log_instances[LOG_INSTANCE_MAX] = {
	{ LOG_APPEND_TEXT_HOOKLIST, NULL, NULL, NULL },
	{ DEBUG_FILTERING_APPEND_TEXT_HOOKLIST, NULL, NULL, NULL }
};

gboolean prefs_common_enable_log_standard(void);
gboolean prefs_common_enable_log_warning(void);
gboolean prefs_common_enable_log_error(void);
gboolean prefs_common_enable_log_status(void);

static gboolean invoke_hook_cb (gpointer data)
{
	LogText *logtext = (LogText *)data;
	hooks_invoke(get_log_hook(logtext->instance), logtext);
	g_free(logtext->text);
	g_free(logtext);
	return FALSE;
}

void set_log_file(LogInstance instance, const gchar *filename)
{
	gchar *fullname = NULL;
	if (log_fp[instance])
		return;

	if (!g_path_is_absolute(filename)) {
		fullname = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S,
					filename, NULL);
	} else {
		fullname = g_strdup(filename);
	}
	/* backup old logfile if existing */
	if (is_file_exist(fullname)) {
		gchar *backupname;
		
		backupname = g_strconcat(fullname, ".bak", NULL);
		claws_unlink(backupname);
		if (g_rename(fullname, backupname) < 0)
			FILE_OP_ERROR(fullname, "rename");
		g_free(backupname);
	}

	log_fp[instance] = g_fopen(fullname, "wb");
	if (!log_fp[instance]) {
		FILE_OP_ERROR(fullname, "fopen");
		log_filename[instance] = NULL;
		g_free(fullname);
		return;
	}
	log_filename[instance] = g_strdup(fullname);
	log_size[instance] = 0;
	g_free(fullname);
}

void close_log_file(LogInstance instance)
{
	if (log_fp[instance]) {
		fclose(log_fp[instance]);
		log_fp[instance] = NULL;
		log_size[instance] = 0;
		g_free(log_filename[instance]);
		log_filename[instance] = NULL;
	}
}

static void rotate_log(LogInstance instance)
{
	if (log_size[instance] > 10 * 1024* 1024) {
		gchar *filename = g_strdup(log_filename[instance]);
		debug_print("rotating %s\n", filename);
		close_log_file(instance);
		set_log_file(instance, filename);
		g_free(filename);
	}
}

const char *get_log_hook(LogInstance instance)
{
	return log_instances[instance].hook;
}

void set_log_title(LogInstance instance, gchar *title)
{
	log_instances[instance].title = title;
}

gchar *get_log_title(LogInstance instance)
{
	return log_instances[instance].title;
}

void set_log_prefs(LogInstance instance, int* logwin_width, int* logwin_height)
{
	log_instances[instance].prefs_logwin_width = logwin_width;
	log_instances[instance].prefs_logwin_height = logwin_height;
}

void get_log_prefs(LogInstance instance, int** logwin_width, int** logwin_height)
{
	if (logwin_width)
		*logwin_width = log_instances[instance].prefs_logwin_width;
	if (logwin_height)
		*logwin_height = log_instances[instance].prefs_logwin_height;
}

void log_print(LogInstance instance, const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE + LOG_TIME_LEN];
	time_t t;
	LogText *logtext = g_new0(LogText, 1);
	struct tm buft;

	time(&t);
	strftime(buf, LOG_TIME_LEN + 1, "[%H:%M:%S] ", localtime_r(&t, &buft));

	va_start(args, format);
	g_vsnprintf(buf + LOG_TIME_LEN, BUFFSIZE, format, args);
	va_end(args);

	if (debug_get_mode()) g_print("%s", buf);

	logtext->instance = instance;
	logtext->text = g_strdup(buf);
	logtext->type = LOG_NORMAL;
	
	g_timeout_add(0, invoke_hook_cb, logtext);

	if (log_fp[instance] && prefs_common_enable_log_standard()) {
		FPUTS(buf, log_fp[instance])
		log_size[instance] += strlen(buf);
		FFLUSH(log_fp[instance])
		rotate_log(instance);
	}
}

void log_message(LogInstance instance, const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE + LOG_TIME_LEN];
	time_t t;
	LogText *logtext = g_new0(LogText, 1);
	struct tm buft;

	time(&t);
	strftime(buf, LOG_TIME_LEN + 1, "[%H:%M:%S] ", localtime_r(&t, &buft));

	va_start(args, format);
	g_vsnprintf(buf + LOG_TIME_LEN, BUFFSIZE, format, args);
	va_end(args);

	if (debug_get_mode()) g_message("%s", buf + LOG_TIME_LEN);

	logtext->instance = instance;
	logtext->text = g_strdup(buf + LOG_TIME_LEN);
	logtext->type = LOG_MSG;
	
	g_timeout_add(0, invoke_hook_cb, logtext);

	if (log_fp[instance] && prefs_common_enable_log_standard()) {
		FWRITE(buf, 1, LOG_TIME_LEN, log_fp[instance])
		FPUTS("* message: ", log_fp[instance])
		log_size[instance] += strlen("* message: ");
		FPUTS(buf + LOG_TIME_LEN, log_fp[instance])
		log_size[instance] += strlen(buf);
		FFLUSH(log_fp[instance])
		rotate_log(instance);
	}
}

void log_warning(LogInstance instance, const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE + LOG_TIME_LEN];
	time_t t;
	LogText *logtext = g_new0(LogText, 1);
	struct tm buft;

	time(&t);
	strftime(buf, LOG_TIME_LEN + 1, "[%H:%M:%S] ", localtime_r(&t, &buft));

	va_start(args, format);
	g_vsnprintf(buf + LOG_TIME_LEN, BUFFSIZE, format, args);
	va_end(args);

	g_warning("%s", buf);

	logtext->instance = instance;
	logtext->text = g_strdup(buf + LOG_TIME_LEN);
	logtext->type = LOG_WARN;
	
	g_timeout_add(0, invoke_hook_cb, logtext);

	if (log_fp[instance] && prefs_common_enable_log_warning()) {
		FWRITE(buf, 1, LOG_TIME_LEN, log_fp[instance])
		FPUTS("** warning: ", log_fp[instance])
		log_size[instance] += strlen("** warning: ");
		FPUTS(buf + LOG_TIME_LEN, log_fp[instance])
		log_size[instance] += strlen(buf);
		FFLUSH(log_fp[instance])
		rotate_log(instance);
	}
}

void log_error(LogInstance instance, const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE + LOG_TIME_LEN];
	time_t t;
	LogText *logtext = g_new0(LogText, 1);
	struct tm buft;

	time(&t);
	strftime(buf, LOG_TIME_LEN + 1, "[%H:%M:%S] ", localtime_r(&t, &buft));

	va_start(args, format);
	g_vsnprintf(buf + LOG_TIME_LEN, BUFFSIZE, format, args);
	va_end(args);

	g_warning("%s", buf);

	logtext->instance = instance;
	logtext->text = g_strdup(buf + LOG_TIME_LEN);
	logtext->type = LOG_ERROR;
	
	g_timeout_add(0, invoke_hook_cb, logtext);

	if (log_fp[instance] && prefs_common_enable_log_error()) {
		FWRITE(buf, 1, LOG_TIME_LEN, log_fp[instance])
		FPUTS("*** error: ", log_fp[instance])
		log_size[instance] += strlen("*** error: ");
		FPUTS(buf + LOG_TIME_LEN, log_fp[instance])
		log_size[instance] += strlen(buf);
		FFLUSH(log_fp[instance])
		rotate_log(instance);
	}
}

void log_status_ok(LogInstance instance, const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE + LOG_TIME_LEN];
	time_t t;
	LogText *logtext = g_new0(LogText, 1);
	struct tm buft;

	time(&t);
	strftime(buf, LOG_TIME_LEN + 1, "[%H:%M:%S] ", localtime_r(&t, &buft));

	va_start(args, format);
	g_vsnprintf(buf + LOG_TIME_LEN, BUFFSIZE, format, args);
	va_end(args);

	if (debug_get_mode()) g_message("%s", buf + LOG_TIME_LEN);

	logtext->instance = instance;
	logtext->text = g_strdup(buf + LOG_TIME_LEN);
	logtext->type = LOG_STATUS_OK;
	
	g_timeout_add(0, invoke_hook_cb, logtext);

	if (log_fp[instance] && prefs_common_enable_log_status()) {
		FWRITE(buf, 1, LOG_TIME_LEN, log_fp[instance])
		FPUTS("* OK: ", log_fp[instance])
		log_size[instance] += strlen("* OK: ");
		FPUTS(buf + LOG_TIME_LEN, log_fp[instance])
		log_size[instance] += strlen(buf);
		FFLUSH(log_fp[instance])
		rotate_log(instance);
	}
}

void log_status_nok(LogInstance instance, const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE + LOG_TIME_LEN];
	time_t t;
	LogText *logtext = g_new0(LogText, 1);
	struct tm buft;

	time(&t);
	strftime(buf, LOG_TIME_LEN + 1, "[%H:%M:%S] ", localtime_r(&t, &buft));

	va_start(args, format);
	g_vsnprintf(buf + LOG_TIME_LEN, BUFFSIZE, format, args);
	va_end(args);

	if (debug_get_mode()) g_message("%s", buf + LOG_TIME_LEN);

	logtext->instance = instance;
	logtext->text = g_strdup(buf + LOG_TIME_LEN);
	logtext->type = LOG_STATUS_NOK;
	
	g_timeout_add(0, invoke_hook_cb, logtext);

	if (log_fp[instance] && prefs_common_enable_log_status()) {
		FWRITE(buf, 1, LOG_TIME_LEN, log_fp[instance])
		FPUTS("* NOT OK: ", log_fp[instance])
		log_size[instance] += strlen("* NOT OK: ");
		FPUTS(buf + LOG_TIME_LEN, log_fp[instance])
		log_size[instance] += strlen(buf);
		FFLUSH(log_fp[instance])
		rotate_log(instance);
	}
}

void log_status_skip(LogInstance instance, const gchar *format, ...)
{
	va_list args;
	gchar buf[BUFFSIZE + LOG_TIME_LEN];
	time_t t;
	LogText *logtext = g_new0(LogText, 1);
	struct tm buft;

	time(&t);
	strftime(buf, LOG_TIME_LEN + 1, "[%H:%M:%S] ", localtime_r(&t, &buft));

	va_start(args, format);
	g_vsnprintf(buf + LOG_TIME_LEN, BUFFSIZE, format, args);
	va_end(args);

	if (debug_get_mode()) g_message("%s", buf + LOG_TIME_LEN);

	logtext->instance = instance;
	logtext->text = g_strdup(buf + LOG_TIME_LEN);
	logtext->type = LOG_STATUS_SKIP;
	
	g_timeout_add(0, invoke_hook_cb, logtext);

	if (log_fp[instance] && prefs_common_enable_log_status()) {
		FWRITE(buf, 1, LOG_TIME_LEN, log_fp[instance])
		FPUTS("* SKIPPED: ", log_fp[instance])
		log_size[instance] += strlen("* SKIPPED: ");
		FPUTS(buf + LOG_TIME_LEN, log_fp[instance])
		log_size[instance] += strlen(buf);
		FFLUSH(log_fp[instance])
		rotate_log(instance);
	}
}

#undef FWRITE
#undef FPUTS
#undef FFLUSH

