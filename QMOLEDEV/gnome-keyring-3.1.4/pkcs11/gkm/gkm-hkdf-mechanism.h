/*
 * gnome-keyring
 *
 * Copyright (C) 2011 Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#ifndef GKM_HKDF_MECHANISM_H_
#define GKM_HKDF_MECHANISM_H_

#include "gkm-types.h"

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11i.h"

#include <glib.h>

static const CK_MECHANISM_TYPE GKM_HKDF_MECHANISMS[] = {
	CKM_G_HKDF_SHA256_DERIVE
};

CK_RV                    gkm_hkdf_mechanism_derive                     (GkmSession *session,
                                                                        const gchar *algo,
                                                                        CK_MECHANISM_PTR mech,
                                                                        GkmObject *base,
                                                                        CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        GkmObject **derived);

#endif /* GKM_HKDF_MECHANISM_H_ */
