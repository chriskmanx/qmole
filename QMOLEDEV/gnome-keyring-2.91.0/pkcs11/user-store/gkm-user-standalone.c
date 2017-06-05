/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkm-user-standalone.h - The user-store PKCS#11 code as a standalone module

   Copyright (C) 2008, Stef Walter

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

#include "config.h"

#include "gkm-user-store.h"

#include "gkm/gkm-crypto.h"

#include "egg/egg-secure-memory.h"

#include "pkcs11/pkcs11.h"

#include <glib-object.h>

/* Module callbacks for secure memory */
EGG_SECURE_GLIB_DEFINITIONS ();

CK_RV
C_GetFunctionList (CK_FUNCTION_LIST_PTR_PTR list)
{
	if (!list)
		return CKR_ARGUMENTS_BAD;

	g_type_init ();
	if (!g_thread_supported ())
		g_thread_init (NULL);

	*list = gkm_user_store_get_functions ();
	return CKR_OK;
}
