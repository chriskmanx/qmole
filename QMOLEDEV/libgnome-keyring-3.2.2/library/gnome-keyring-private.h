/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gnome-keyring-private.h - private header for keyring

   Copyright (C) 2003 Red Hat, Inc

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

   Author: Alexander Larsson <alexl@redhat.com>
*/

#ifndef GNOME_KEYRING_PRIVATE_H
#define GNOME_KEYRING_PRIVATE_H

#include "gnome-keyring.h"

#include "gkr-callback.h"
#include "gkr-operation.h"

#include <dbus/dbus.h>

struct GnomeKeyringApplicationRef {
	char *display_name;
	char *pathname;
};

struct GnomeKeyringAccessControl {
	GnomeKeyringApplicationRef *application; /* null for all */
	GnomeKeyringAccessType types_allowed;
};

struct GnomeKeyringInfo {
	/* <private> */
	gboolean lock_on_idle;
	guint32 lock_timeout;
	time_t mtime;
	time_t ctime;
	gboolean is_locked;
};

struct GnomeKeyringItemInfo {
	GnomeKeyringItemType type;
	char *display_name;
	char *secret;
	time_t mtime;
	time_t ctime;
};

void   _gnome_keyring_memory_dump (void);
extern gboolean gnome_keyring_memory_warning;

#define BROKEN                         GNOME_KEYRING_RESULT_IO_ERROR

#define SECRETS_SERVICE                "org.freedesktop.secrets"
#define SERVICE_PATH                   "/org/freedesktop/secrets"
#define COLLECTION_INTERFACE           "org.freedesktop.Secret.Collection"
#define ITEM_INTERFACE                 "org.freedesktop.Secret.Item"
#define PROMPT_INTERFACE               "org.freedesktop.Secret.Prompt"
#define SERVICE_INTERFACE              "org.freedesktop.Secret.Service"
#define COLLECTION_PREFIX              "/org/freedesktop/secrets/collection/"
#define COLLECTION_DEFAULT             "/org/freedesktop/secrets/aliases/default"

#define ERROR_IS_LOCKED                "org.freedesktop.Secret.Error.IsLocked"
#define ERROR_NO_SESSION               "org.freedesktop.Secret.Error.NoSession"
#define ERROR_NO_SUCH_OBJECT           "org.freedesktop.Secret.Error.NoSuchObject"

#define NORMAL_ALLOCATOR  ((EggBufferAllocator)g_realloc)
#define SECURE_ALLOCATOR  ((EggBufferAllocator)gnome_keyring_memory_realloc)

#endif /* GNOME_KEYRING_PRIVATE_H */
