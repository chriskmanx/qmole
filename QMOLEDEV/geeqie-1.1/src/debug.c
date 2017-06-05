/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Authors: Vladimir Nadvornik, Laurent Monin
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#include "main.h"
#include "debug.h"

#include "logwindow.h"
#include "ui_fileops.h"

#include <glib/gprintf.h>

/*
 * Logging functions
 */

static gboolean log_msg_cb(gpointer data)
{
	gchar *buf = data;
	log_window_append(buf, LOG_MSG);
	g_free(buf);
	return FALSE;
}

static gboolean log_normal_cb(gpointer data)
{
	gchar *buf = data;
	log_window_append(buf, LOG_NORMAL);
	g_free(buf);
	return FALSE;
}

void log_domain_printf(const gchar *domain, const gchar *format, ...)
{
	va_list ap;
	gchar *buf;

	va_start(ap, format);
	buf = g_strdup_vprintf(format, ap);
	va_end(ap);

	print_term(buf);
	if (strcmp(domain, DOMAIN_INFO) == 0)
		g_idle_add(log_normal_cb, buf);
	else
		g_idle_add(log_msg_cb, buf);

}

/*
 * Debugging only functions
 */

#ifdef DEBUG

static gint debug_level = DEBUG_LEVEL_MIN;


gint get_debug_level(void)
{
	return debug_level;
}

void set_debug_level(gint new_level)
{
	debug_level = CLAMP(new_level, DEBUG_LEVEL_MIN, DEBUG_LEVEL_MAX);
}

void debug_level_add(gint delta)
{
	set_debug_level(debug_level + delta);
}

gint required_debug_level(gint level)
{
	return (debug_level >= level);
}

static gint timeval_delta(struct timeval *result, struct timeval *x, struct timeval *y)
{
	if (x->tv_usec < y->tv_usec)
		{
		gint nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
		y->tv_usec -= 1000000 * nsec;
		y->tv_sec += nsec;
		}

	if (x->tv_usec - y->tv_usec > 1000000)
		{
		gint nsec = (x->tv_usec - y->tv_usec) / 1000000;
		y->tv_usec += 1000000 * nsec;
		y->tv_sec -= nsec;
	}

	result->tv_sec = x->tv_sec - y->tv_sec;
	result->tv_usec = x->tv_usec - y->tv_usec;

	return x->tv_sec < y->tv_sec;
}

const gchar *get_exec_time(void)
{
	static gchar timestr[30];
	static struct timeval start_tv = {0, 0};
	static struct timeval previous = {0, 0};
	static gint started = 0;

	struct timeval tv = {0, 0};
	static struct timeval delta = {0, 0};

	gettimeofday(&tv, NULL);

	if (start_tv.tv_sec == 0) start_tv = tv;

	tv.tv_sec -= start_tv.tv_sec;
	if (tv.tv_usec >= start_tv.tv_usec)
		tv.tv_usec -= start_tv.tv_usec;
	else
		{
		tv.tv_usec += 1000000 - start_tv.tv_usec;
		tv.tv_sec -= 1;
		}

	if (started) timeval_delta(&delta, &tv, &previous);

	previous = tv;
	started = 1;

	g_snprintf(timestr, sizeof(timestr), "%5d.%06d (+%05d.%06d)", (gint)tv.tv_sec, (gint)tv.tv_usec, (gint)delta.tv_sec, (gint)delta.tv_usec);

	return timestr;
}

void init_exec_time(void)
{
	get_exec_time();
}

#endif /* DEBUG */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
