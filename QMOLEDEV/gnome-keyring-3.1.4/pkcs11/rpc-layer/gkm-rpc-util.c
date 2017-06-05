/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* p11-rpc-util.c - utilities for module and dispatcher

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

#include "gkm-rpc-layer.h"
#include "gkm-rpc-private.h"

#include <stdarg.h>
#include <string.h>
#include <stdio.h>

static void
do_log (const char *pref, const char *msg, va_list va)
{
	char buffer[1024];
	size_t len = 0;

	if (pref) {
		snprintf (buffer, sizeof (buffer), "%s: ", pref);
		len = strlen (buffer);
	}

	vsnprintf (buffer + len, sizeof (buffer) - len, msg, va);
	gkm_rpc_log (buffer);
}

void
gkm_rpc_warn (const char* msg, ...)
{
	va_list va;
	va_start (va, msg);
	do_log ("WARNING", msg, va);
	va_end (va);
}

void
gkm_rpc_debug (const char* msg, ...)
{
	va_list va;
	va_start (va, msg);
	do_log ("DEBUG", msg, va);
	va_end (va);
}

int
gkm_rpc_mechanism_is_supported (CK_MECHANISM_TYPE mech)
{
	if (gkm_rpc_mechanism_has_no_parameters (mech) ||
	    gkm_rpc_mechanism_has_sane_parameters (mech))
		return 1;
	return 0;
}
void
gkm_rpc_mechanism_list_purge (CK_MECHANISM_TYPE_PTR mechs, CK_ULONG* n_mechs)
{
	int i;

	assert (mechs);
	assert (n_mechs);

	for (i = 0; i < (int)(*n_mechs); ++i) {
		if (!gkm_rpc_mechanism_has_no_parameters (mechs[i]) &&
		    !gkm_rpc_mechanism_has_sane_parameters (mechs[i])) {

			/* Remove the mechanism from the list */
			memmove (&mechs[i], &mechs[i + 1], (*n_mechs - i) * sizeof (CK_MECHANISM_TYPE));

			--(*n_mechs);
			--i;
		}
	}
}

int
gkm_rpc_mechanism_has_sane_parameters (CK_MECHANISM_TYPE type)
{
	/* This list is incomplete */
	switch (type) {
	case CKM_RSA_PKCS_OAEP:
	case CKM_RSA_PKCS_PSS:
		return 1;
	default:
		return 0;
	}
}

int
gkm_rpc_mechanism_has_no_parameters (CK_MECHANISM_TYPE mech)
{
	/* This list is incomplete */

	switch (mech) {
	case CKM_RSA_PKCS_KEY_PAIR_GEN:
	case CKM_RSA_X9_31_KEY_PAIR_GEN:
	case CKM_RSA_PKCS:
	case CKM_RSA_9796:
	case CKM_RSA_X_509:
	case CKM_RSA_X9_31:
	case CKM_MD2_RSA_PKCS:
	case CKM_MD5_RSA_PKCS:
	case CKM_SHA1_RSA_PKCS:
	case CKM_SHA256_RSA_PKCS:
	case CKM_SHA384_RSA_PKCS:
	case CKM_SHA512_RSA_PKCS:
	case CKM_RIPEMD128_RSA_PKCS:
	case CKM_RIPEMD160_RSA_PKCS:
	case CKM_SHA1_RSA_X9_31:
	case CKM_DSA_KEY_PAIR_GEN:
	case CKM_DSA_PARAMETER_GEN:
	case CKM_DSA:
	case CKM_DSA_SHA1:
	case CKM_FORTEZZA_TIMESTAMP:
	case CKM_EC_KEY_PAIR_GEN:
	case CKM_ECDSA:
	case CKM_ECDSA_SHA1:
	case CKM_DH_PKCS_KEY_PAIR_GEN:
	case CKM_DH_PKCS_PARAMETER_GEN:
	case CKM_X9_42_DH_KEY_PAIR_GEN:
	case CKM_X9_42_DH_PARAMETER_GEN:
	case CKM_KEA_KEY_PAIR_GEN:
	case CKM_GENERIC_SECRET_KEY_GEN:
	case CKM_RC2_KEY_GEN:
	case CKM_RC4_KEY_GEN:
	case CKM_RC4:
	case CKM_RC5_KEY_GEN:
	case CKM_AES_KEY_GEN:
	case CKM_AES_ECB:
	case CKM_AES_MAC:
	case CKM_DES_KEY_GEN:
	case CKM_DES2_KEY_GEN:
	case CKM_DES3_KEY_GEN:
	case CKM_CDMF_KEY_GEN:
	case CKM_CAST_KEY_GEN:
	case CKM_CAST3_KEY_GEN:
	case CKM_CAST128_KEY_GEN:
	case CKM_IDEA_KEY_GEN:
	case CKM_SSL3_PRE_MASTER_KEY_GEN:
	case CKM_TLS_PRE_MASTER_KEY_GEN:
	case CKM_SKIPJACK_KEY_GEN:
	case CKM_BATON_KEY_GEN:
	case CKM_JUNIPER_KEY_GEN:
	case CKM_RC2_ECB:
	case CKM_DES_ECB:
	case CKM_DES3_ECB:
	case CKM_CDMF_ECB:
	case CKM_CAST_ECB:
	case CKM_CAST3_ECB:
	case CKM_CAST128_ECB:
	case CKM_RC5_ECB:
	case CKM_IDEA_ECB:
	case CKM_RC2_MAC:
	case CKM_DES_MAC:
	case CKM_DES3_MAC:
	case CKM_CDMF_MAC:
	case CKM_CAST_MAC:
	case CKM_CAST3_MAC:
	case CKM_RC5_MAC:
	case CKM_IDEA_MAC:
	case CKM_SSL3_MD5_MAC:
	case CKM_SSL3_SHA1_MAC:
	case CKM_SKIPJACK_WRAP:
	case CKM_BATON_WRAP:
	case CKM_JUNIPER_WRAP:
	case CKM_MD2:
	case CKM_MD2_HMAC:
	case CKM_MD5:
	case CKM_MD5_HMAC:
	case CKM_SHA_1:
	case CKM_SHA_1_HMAC:
	case CKM_SHA256:
	case CKM_SHA256_HMAC:
	case CKM_SHA384:
	case CKM_SHA384_HMAC:
	case CKM_SHA512:
	case CKM_SHA512_HMAC:
	case CKM_FASTHASH:
	case CKM_RIPEMD128:
	case CKM_RIPEMD128_HMAC:
	case CKM_RIPEMD160:
	case CKM_RIPEMD160_HMAC:
	case CKM_KEY_WRAP_LYNKS:
		return 1;
	default:
		return 0;
	};
}
