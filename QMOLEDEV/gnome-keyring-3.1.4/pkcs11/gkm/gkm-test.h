/*
 * gnome-keyring
 *
 * Copyright (C) 2010 Stefan Walter
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include <glib.h>

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11i.h"

#ifndef GKM_TEST_H
#define GKM_TEST_H

#define         gkm_assert_cmprv(v1, cmp, v2) \
		do { CK_RV __v1 = (v1), __v2 = (v2); \
			if (__v1 cmp __v2) ; else \
				gkm_assertion_message_cmprv (G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, \
				                             #v1 " " #cmp " " #v2, __v1, #cmp, __v2); \
		} while (0)

#define         gkm_assert_cmpulong(v1, cmp, v2) \
		do { CK_RV __v1 = (v1), __v2 = (v2); \
			if (__v1 cmp __v2) ; else \
				gkm_assertion_message_cmpulong (G_LOG_DOMAIN, __FILE__, __LINE__, G_STRFUNC, \
				                                #v1 " " #cmp " " #v2, __v1, #cmp, __v2); \
		} while (0)

void            gkm_assertion_message_cmprv        (const gchar *domain,
                                                    const gchar *file,
                                                    int line,
                                                    const gchar *func,
                                                    const gchar *expr,
                                                    CK_RV arg1,
                                                    const gchar *cmp,
                                                    CK_RV arg2);

void            gkm_assertion_message_cmpulong     (const gchar *domain,
                                                    const gchar *file,
                                                    gint line,
                                                    const gchar *func,
                                                    const gchar *expr,
                                                    CK_ULONG arg1,
                                                    const gchar *cmp,
                                                    CK_ULONG arg2);

#endif /* GKM_TEST_H */
