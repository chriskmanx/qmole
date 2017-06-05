/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* pkcs11i.h - GNOME internal definitions to PKCS#11

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

#ifndef PKCS11I_H
#define PKCS11I_H

#include "pkcs11.h"

#define CKA_GNOME   (CKA_VENDOR_DEFINED | 0x474E4D45UL /* GNME */ )
#define CKO_GNOME   (CKO_VENDOR_DEFINED | 0x474E4D45UL /* GNME */ )
#define CKR_GNOME   (CKR_VENDOR_DEFINED | 0x474E4D45UL /* GNME */ )
#define CKM_GNOME   (CKR_VENDOR_DEFINED | 0x474E4D45UL /* GNME */ )
#define CKK_GNOME   (CKR_VENDOR_DEFINED | 0x474E4D45UL /* GNME */ )

/* -------------------------------------------------------------------
 * OBJECT UNIQUE IDENTIFIER
 */

/* A string unique among all objects on a given machine */
#define CKA_GNOME_UNIQUE                            (CKA_GNOME + 350)

/* -------------------------------------------------------------------
 */

#define CKA_GNOME_TRANSIENT                      (CKA_GNOME + 201)

/* Signifies that nobody is logged in */
#define CKU_NONE G_MAXULONG

#define CK_GNOME_MAX_SLOT                           (0x000000FFUL)
#define CK_GNOME_MAX_HANDLE                         (((CK_ULONG)-1UL) >> 10)

/* -------------------------------------------------------------------
 * OBJECT HASH
 */

#define CKA_GNOME_INTERNAL_SHA1                      (CKA_GNOME + 1000)


/* -------------------------------------------------------------------
 * APPLICATION
 */

/* Flag for CK_INFO when applications are supported */
#define CKF_G_APPLICATIONS                       0x40000000UL

/* Call C_OpenSession with this when passing CK_G_APPLICATION */
#define CKF_G_APPLICATION_SESSION                0x40000000UL

typedef CK_ULONG CK_G_APPLICATION_ID;

typedef struct CK_G_APPLICATION {
	CK_VOID_PTR applicationData;
	CK_G_APPLICATION_ID applicationId;
} CK_G_APPLICATION;

typedef CK_G_APPLICATION* CK_G_APPLICATION_PTR;

#define CKR_G_APPLICATION_ID_INVALID             (CKR_GNOME + 10)


/* -------------------------------------------------------------------
 * SECRETS
 */

#define CKO_G_COLLECTION                     (CKO_GNOME + 110)

#define CKK_G_SECRET_ITEM                    (CKK_GNOME + 101)

#define CKO_G_SEARCH                         (CKO_GNOME + 111)

#define CKA_G_LOCKED                         (CKA_GNOME + 210)

#define CKA_G_CREATED                        (CKA_GNOME + 211)

#define CKA_G_MODIFIED                       (CKA_GNOME + 212)

#define CKA_G_FIELDS                         (CKA_GNOME + 213)

#define CKA_G_COLLECTION                     (CKA_GNOME + 214)

#define CKA_G_MATCHED                        (CKA_GNOME + 215)

#define CKA_G_SCHEMA                         (CKA_GNOME + 216)

#define CKA_G_LOGIN_COLLECTION               (CKA_GNOME + 218)

/* -------------------------------------------------------------------
 * MECHANISMS
 */

/* Used for wrapping and unwrapping as null */
#define CKM_G_NULL                           (CKM_GNOME + 100)

#define CKM_G_HKDF_SHA256_DERIVE             (CKM_GNOME + 101)

#define CKK_G_NULL                           (CKK_GNOME + 100)

/* -------------------------------------------------------------------
 * AUTO DESTRUCT
 */

#define CKA_G_DESTRUCT_IDLE                  (CKA_GNOME + 190)

#define CKA_G_DESTRUCT_AFTER                 (CKA_GNOME + 191)

#define CKA_G_DESTRUCT_USES                  (CKA_GNOME + 192)

/* -------------------------------------------------------------------
 * CREDENTIAL
 */

#define CKO_G_CREDENTIAL                         (CKO_GNOME + 100)

#define CKA_G_OBJECT                             (CKA_GNOME + 202)

#define CKA_G_CREDENTIAL                         (CKA_GNOME + 204)

#define CKA_G_CREDENTIAL_TEMPLATE                (CKA_GNOME + 205)

#endif /* PKCS11I_H */
