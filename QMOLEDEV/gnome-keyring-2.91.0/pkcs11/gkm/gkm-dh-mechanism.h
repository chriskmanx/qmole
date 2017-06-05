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

#ifndef GKM_DH_MECHANISM_H_
#define GKM_DH_MECHANISM_H_

#include "gkm-types.h"

#include "pkcs11/pkcs11.h"

#include <glib.h>

#include <gcrypt.h>

static const CK_MECHANISM_TYPE GKM_DH_MECHANISMS[] = {
	CKM_DH_PKCS_DERIVE
};

CK_RV                    gkm_dh_mechanism_generate                     (GkmSession *session,
                                                                        CK_ATTRIBUTE_PTR pub_atts,
                                                                        CK_ULONG n_pub_atts,
                                                                        CK_ATTRIBUTE_PTR priv_atts,
                                                                        CK_ULONG n_priv_atts,
                                                                        GkmObject **pub_key,
                                                                        GkmObject **priv_key);

CK_RV                    gkm_dh_mechanism_derive                       (GkmSession *session,
                                                                        CK_MECHANISM_PTR mech,
                                                                        GkmObject *base,
                                                                        CK_ATTRIBUTE_PTR attrs,
                                                                        CK_ULONG n_attrs,
                                                                        GkmObject **derived);

#endif /* GKM_DH_MECHANISM_H_ */
