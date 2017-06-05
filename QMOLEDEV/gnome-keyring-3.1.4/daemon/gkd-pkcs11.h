/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *
 * You should have received a copy of the GNU Lesser General
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef GKD_PKCS11_H_
#define GKD_PKCS11_H_

#include <glib.h>

#include "pkcs11/pkcs11.h"

gboolean               gkd_pkcs11_initialize           (void);

gboolean               gkd_pkcs11_startup_pkcs11       (void);

gboolean               gkd_pkcs11_startup_ssh          (void);

CK_FUNCTION_LIST_PTR   gkd_pkcs11_get_functions        (void);

CK_FUNCTION_LIST_PTR   gkd_pkcs11_get_base_functions   (void);

#endif /* GKD_PKCS11_H_ */
