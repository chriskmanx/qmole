/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#ifndef GKM_AES_MECHANISM_H_
#define GKM_AES_MECHANISM_H_

#include "gkm-types.h"

#include "pkcs11/pkcs11.h"

#include <glib.h>

#define GKM_AES_MECHANISM_MIN_LENGTH     16
#define GKM_AES_MECHANISM_MAX_LENGTH     32

static const CK_MECHANISM_TYPE GKM_AES_MECHANISMS[] = {
	CKM_AES_CBC_PAD
};

CK_RV                   gkm_aes_mechanism_wrap                 (GkmSession *session,
                                                                CK_MECHANISM_PTR mech,
                                                                GkmObject *wrapper,
                                                                GkmObject *wrapped,
                                                                CK_BYTE_PTR output,
                                                                CK_ULONG_PTR n_output);

CK_RV                   gkm_aes_mechanism_unwrap               (GkmSession *session,
                                                                CK_MECHANISM_PTR mech,
                                                                GkmObject *wrapper,
                                                                CK_VOID_PTR input,
                                                                CK_ULONG n_input,
                                                                CK_ATTRIBUTE_PTR attrs,
                                                                CK_ULONG n_attrs,
                                                                GkmObject **unwrapped);

#endif /* GKM_AES_MECHANISM_H_ */
