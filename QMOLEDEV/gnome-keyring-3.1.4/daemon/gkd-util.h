/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkd-util.h - Helper utilities for the daemon

   Copyright (C) 2008, Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#ifndef GKD_UTIL_H_
#define GKD_UTIL_H_

#include <glib.h>

#define         GKD_UTIL_ENV_CONTROL             "GNOME_KEYRING_CONTROL"

extern const gchar *GKD_UTIL_OUT_ENVIRONMENT[];
extern const gchar *GKD_UTIL_IN_ENVIRONMENT[];

void            gkd_util_init_master_directory   (const gchar *replace);

const gchar*    gkd_util_get_master_directory    (void);

void            gkd_util_push_environment        (const gchar *name,
                                                  const gchar *value);

void            gkd_util_push_environment_full   (const gchar *env);

void            gkd_util_watch_environment       (GFunc func,
                                                  gpointer user_data,
                                                  GDestroyNotify destroy_notify);

const gchar**   gkd_util_get_environment         (void);

gchar**         gkd_util_build_environment       (const gchar **names);

#endif /*GKD_UTIL_H_*/
