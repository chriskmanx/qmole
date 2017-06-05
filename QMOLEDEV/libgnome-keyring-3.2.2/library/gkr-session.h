/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkr-callback.h - callbacks similar to closures

   Copyright (C) 2009 Stefan Walter

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

#ifndef GKR_SESSION_H
#define GKR_SESSION_H

#include "gkr-operation.h"

void                 gkr_session_negotiate            (GkrOperation *op);

void                 gkr_session_clear                (void);

GkrSession*          gkr_session_ref                  (GkrSession *session);

void                 gkr_session_unref                (gpointer data);

const gchar*         gkr_session_get_path             (GkrSession *session);

gboolean             gkr_session_encode_secret        (GkrSession *session,
                                                       DBusMessageIter *iter,
                                                       const gchar* secret);

gboolean             gkr_session_decode_secret        (GkrSession *session,
                                                       DBusMessageIter *iter,
                                                       gchar** secret);

#endif /* GKR_SESSION_H */
