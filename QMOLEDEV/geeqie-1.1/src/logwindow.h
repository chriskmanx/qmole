/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: Vladimir Nadvornik, Laurent Monin
 * based on logwindow.[ch] from Sylpheed 2.4.7 (C) Hiroyuki Yamamoto
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef LOGWINDOW_H
#define LOGWINDOW_H

typedef enum
{
	LOG_NORMAL = 0,
	LOG_MSG,
	LOG_WARN,
	LOG_ERROR,
	LOG_COUNT
} LogType;

void log_window_new(void);

void log_window_append(const gchar *str, LogType type);

#endif /* LOGWINDOW_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
