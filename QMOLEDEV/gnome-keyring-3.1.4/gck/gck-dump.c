/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gck-dump - the GObject PKCS#11 wrapper library

   Copyright (C) 2010 Collabora Ltd

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

   Author: Stef Walter <stefw@collabora.co.uk>
*/

#include "config.h"

#include "gck.h"
#include "gck-private.h"

#include "egg/egg-hex.h"

#include <stdlib.h>
#include <string.h>

#include "pkcs11/pkcs11i.h"
#include "pkcs11/pkcs11x.h"

static void
dump_class_value (gulong klass)
{
	switch (klass) {
	#define DX(x) case x: g_printerr ("%s", #x); break;
	DX(CKO_DATA);
	DX(CKO_CERTIFICATE);
	DX(CKO_PUBLIC_KEY);
	DX(CKO_PRIVATE_KEY);
	DX(CKO_SECRET_KEY);
	DX(CKO_HW_FEATURE);
	DX(CKO_DOMAIN_PARAMETERS);
	DX(CKO_MECHANISM);
	DX(CKO_G_COLLECTION);
	DX(CKO_G_SEARCH);
	DX(CKO_G_CREDENTIAL);
	DX(CKO_X_TRUST_ASSERTION);
	#undef DX

	default:
		g_printerr ("%s0x%08x",
		            (klass & CKO_VENDOR_DEFINED) == CKA_VENDOR_DEFINED ?
		                     "CKO_VENDOR_DEFINED|" : "",
		            (unsigned int)klass);
		break;
	}
}

static void
dump_assertion_type_value (gulong type)
{
	switch (type) {
	#define DX(x) case x: g_printerr ("%s", #x); break;
	DX(CKT_X_DISTRUSTED_CERTIFICATE);
	DX(CKT_X_PINNED_CERTIFICATE);
	DX(CKT_X_ANCHORED_CERTIFICATE);
	#undef DX

	default:
		g_printerr ("%u", (unsigned int)type);
		break;
	}
}

static void
dump_attribute_value (GckAttribute *attr)
{
	gchar *data;
	gsize len;

	g_assert (attr->length != G_MAXULONG);

	if (attr->value == NULL) {
		g_printerr ("[null]");
		return;
	}

	switch (attr->type) {
	case CKA_CLASS:
		if (attr->length == sizeof (CK_OBJECT_CLASS)) {
			dump_class_value (*(CK_ULONG_PTR)attr->value);
			return;
		}
		break;

	case CKA_X_ASSERTION_TYPE:
		if (attr->length == sizeof (CK_X_ASSERTION_TYPE)) {
			dump_assertion_type_value (*(CK_X_ASSERTION_TYPE*)attr->value);
			return;
		}
		break;

	case CKA_CERTIFICATE_TYPE:
	case CKA_CERTIFICATE_CATEGORY:
	case CKA_JAVA_MIDP_SECURITY_DOMAIN:
	case CKA_KEY_TYPE:
	case CKA_PRIME_BITS:
	case CKA_SUB_PRIME_BITS:
	case CKA_VALUE_BITS:
	case CKA_VALUE_LEN:
	case CKA_KEY_GEN_MECHANISM:
	case CKA_HW_FEATURE_TYPE:
	case CKA_PIXEL_X:
	case CKA_PIXEL_Y:
	case CKA_RESOLUTION:
	case CKA_CHAR_ROWS:
	case CKA_CHAR_COLUMNS:
	case CKA_BITS_PER_PIXEL:
	case CKA_MECHANISM_TYPE:
	case CKA_G_DESTRUCT_IDLE:
	case CKA_G_DESTRUCT_AFTER:
	case CKA_G_DESTRUCT_USES:
	case CKA_G_OBJECT:
	case CKA_G_CREDENTIAL:
		if (attr->length == sizeof (CK_ULONG)) {
			g_printerr ("%llu", (unsigned long long)*(CK_ULONG_PTR)attr->value);
			return;
		}
		break;

	case CKA_TOKEN:
	case CKA_PRIVATE:
	case CKA_TRUSTED:
	case CKA_SENSITIVE:
	case CKA_ENCRYPT:
	case CKA_DECRYPT:
	case CKA_WRAP:
	case CKA_UNWRAP:
	case CKA_SIGN:
	case CKA_SIGN_RECOVER:
	case CKA_VERIFY:
	case CKA_VERIFY_RECOVER:
	case CKA_DERIVE:
	case CKA_EXTRACTABLE:
	case CKA_LOCAL:
	case CKA_NEVER_EXTRACTABLE:
	case CKA_ALWAYS_SENSITIVE:
	case CKA_MODIFIABLE:
	case CKA_ALWAYS_AUTHENTICATE:
	case CKA_WRAP_WITH_TRUSTED:
	case CKA_RESET_ON_INIT:
	case CKA_HAS_RESET:
	case CKA_COLOR:
	case CKA_G_LOCKED:
	case CKA_G_LOGIN_COLLECTION:
		if (attr->length == sizeof (CK_BBOOL)) {
			g_printerr ("%s", (*(CK_BBOOL*)attr->value) ? "TRUE" : "FALSE");
			return;
		}
		break;

	case CKA_LABEL:
	case CKA_URL:
	case CKA_CHAR_SETS:
	case CKA_ENCODING_METHODS:
	case CKA_MIME_TYPES:
	case CKA_G_COLLECTION:
	case CKA_G_SCHEMA:
	case CKA_X_PURPOSE:
	case CKA_X_PEER:
		if (g_utf8_validate (attr->value, attr->length, NULL)) {
			int length = MIN (32, attr->length);
			g_printerr ("%.*s%s", length, (gchar*)attr->value,
			            length < attr->length ? "..." : "");
			return;
		}
		break;

	case CKA_START_DATE:
	case CKA_END_DATE:
	case CKA_G_CREATED:
	case CKA_G_MODIFIED:
		if (attr->length == sizeof (CK_DATE)) {
			const CK_DATE* date = attr->value;
			g_printerr ("%.4s-%.2s-%.2s", date->year, date->month, date->day);
			return;
		}
		break;

	default:
		break;
	};

	len = MIN (20, attr->length);
	data = egg_hex_encode_full (attr->value, len, TRUE, ':', 1);
	g_printerr ("%s%s", data, len < attr->length ? "..." : "");
	g_free (data);
}

static void
dump_attribute_type (GckAttribute *attr)
{
	switch (attr->type) {
	#define DX(x) case x: g_printerr ("%s", #x); break;
	DX(CKA_CLASS);
	DX(CKA_TOKEN);
	DX(CKA_PRIVATE);
	DX(CKA_LABEL);
	DX(CKA_APPLICATION);
	DX(CKA_VALUE);
	DX(CKA_OBJECT_ID);
	DX(CKA_CERTIFICATE_TYPE);
	DX(CKA_ISSUER);
	DX(CKA_SERIAL_NUMBER);
	DX(CKA_AC_ISSUER);
	DX(CKA_OWNER);
	DX(CKA_ATTR_TYPES);
	DX(CKA_TRUSTED);
	DX(CKA_CERTIFICATE_CATEGORY);
	DX(CKA_JAVA_MIDP_SECURITY_DOMAIN);
	DX(CKA_URL);
	DX(CKA_HASH_OF_SUBJECT_PUBLIC_KEY);
	DX(CKA_HASH_OF_ISSUER_PUBLIC_KEY);
	DX(CKA_CHECK_VALUE);
	DX(CKA_KEY_TYPE);
	DX(CKA_SUBJECT);
	DX(CKA_ID);
	DX(CKA_SENSITIVE);
	DX(CKA_ENCRYPT);
	DX(CKA_DECRYPT);
	DX(CKA_WRAP);
	DX(CKA_UNWRAP);
	DX(CKA_SIGN);
	DX(CKA_SIGN_RECOVER);
	DX(CKA_VERIFY);
	DX(CKA_VERIFY_RECOVER);
	DX(CKA_DERIVE);
	DX(CKA_START_DATE);
	DX(CKA_END_DATE);
	DX(CKA_MODULUS);
	DX(CKA_MODULUS_BITS);
	DX(CKA_PUBLIC_EXPONENT);
	DX(CKA_PRIVATE_EXPONENT);
	DX(CKA_PRIME_1);
	DX(CKA_PRIME_2);
	DX(CKA_EXPONENT_1);
	DX(CKA_EXPONENT_2);
	DX(CKA_COEFFICIENT);
	DX(CKA_PRIME);
	DX(CKA_SUBPRIME);
	DX(CKA_BASE);
	DX(CKA_PRIME_BITS);
	DX(CKA_SUB_PRIME_BITS);
	DX(CKA_VALUE_BITS);
	DX(CKA_VALUE_LEN);
	DX(CKA_EXTRACTABLE);
	DX(CKA_LOCAL);
	DX(CKA_NEVER_EXTRACTABLE);
	DX(CKA_ALWAYS_SENSITIVE);
	DX(CKA_KEY_GEN_MECHANISM);
	DX(CKA_MODIFIABLE);
	/* DX(CKA_ECDSA_PARAMS); */
	DX(CKA_EC_PARAMS);
	DX(CKA_EC_POINT);
	DX(CKA_SECONDARY_AUTH);
	DX(CKA_AUTH_PIN_FLAGS);
	DX(CKA_ALWAYS_AUTHENTICATE);
	DX(CKA_WRAP_WITH_TRUSTED);
	DX(CKA_HW_FEATURE_TYPE);
	DX(CKA_RESET_ON_INIT);
	DX(CKA_HAS_RESET);
	DX(CKA_PIXEL_X);
	DX(CKA_PIXEL_Y);
	DX(CKA_RESOLUTION);
	DX(CKA_CHAR_ROWS);
	DX(CKA_CHAR_COLUMNS);
	DX(CKA_COLOR);
	DX(CKA_BITS_PER_PIXEL);
	DX(CKA_CHAR_SETS);
	DX(CKA_ENCODING_METHODS);
	DX(CKA_MIME_TYPES);
	DX(CKA_MECHANISM_TYPE);
	DX(CKA_REQUIRED_CMS_ATTRIBUTES);
	DX(CKA_DEFAULT_CMS_ATTRIBUTES);
	DX(CKA_SUPPORTED_CMS_ATTRIBUTES);
	DX(CKA_WRAP_TEMPLATE);
	DX(CKA_UNWRAP_TEMPLATE);
	DX(CKA_ALLOWED_MECHANISMS);

	/* GNOME */
	DX(CKA_G_LOCKED);
	DX(CKA_G_CREATED);
	DX(CKA_G_MODIFIED);
	DX(CKA_G_FIELDS);
	DX(CKA_G_COLLECTION);
	DX(CKA_G_MATCHED);
	DX(CKA_G_SCHEMA);
	DX(CKA_G_LOGIN_COLLECTION);
	DX(CKA_G_DESTRUCT_IDLE);
	DX(CKA_G_DESTRUCT_AFTER);
	DX(CKA_G_DESTRUCT_USES);
	DX(CKA_G_OBJECT);
	DX(CKA_G_CREDENTIAL);
	DX(CKA_G_CREDENTIAL_TEMPLATE);
	DX(CKA_X_ASSERTION_TYPE);
	DX(CKA_X_CERTIFICATE_VALUE);
	DX(CKA_X_PURPOSE);
	DX(CKA_X_PEER);
	#undef DX

	default:
		g_printerr ("%s0x%08x",
		            (attr->type & CKA_VENDOR_DEFINED) == CKA_VENDOR_DEFINED ?
		                     "CKA_VENDOR_DEFINED|" : "",
		            (unsigned int)attr->type);
		break;
	}
}

/**
 * gck_attribute_dump:
 * @attr: The attribute
 *
 * Dump the specified attribute using g_printerr().
 */
void
gck_attribute_dump (GckAttribute *attr)
{
	dump_attribute_type (attr);
	if (attr->length == G_MAXULONG) {
		g_printerr ("\n    [invalid]\n");
	} else {
		g_printerr ("\n    [%lu] ", (unsigned long)attr->length);
		dump_attribute_value (attr);
		g_printerr ("\n");
	}
}

/**
 * gck_attributes_dump:
 * @attrs: The attributes
 *
 * Dump the attributes using g_printerr().
 */
void
gck_attributes_dump (GckAttributes *attrs)
{
	GckAttribute *attr;
	guint i, count;

	for (i = 0, count = gck_attributes_count (attrs); i < count; ++i) {
		attr = gck_attributes_at (attrs, i);
		gck_attribute_dump (attr);
	}
}
