/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-oid.c - OID helper routines

   Copyright (C) 2007 Stefan Walter

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

#include "egg-oid.h"

#include <string.h>

#include <glib/gi18n-lib.h>

typedef struct _OidInfo {
	GQuark oid;
	const gchar *oidstr;
	const gchar *attr;
	const gchar *description;
	guint flags;
} OidInfo;

static OidInfo oid_info[] = {
	{ 0, "0.9.2342.19200300.100.1.25", "DC", N_("Domain Component"), 
		EGG_OID_PRINTABLE },
	{ 0, "0.9.2342.19200300.100.1.1", "UID", N_("User ID"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },

	{ 0, "1.2.840.113549.1.9.1", "EMAIL", N_("Email Address"),
		EGG_OID_PRINTABLE },
	{ 0, "1.2.840.113549.1.9.7", NULL, NULL, 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
		
	{ 0, "1.2.840.113549.1.9.20", NULL, NULL, 
		EGG_OID_PRINTABLE },
	
	{ 0, "1.3.6.1.5.5.7.9.1", "dateOfBirth", N_("Date of Birth"), 
		EGG_OID_PRINTABLE },
	{ 0, "1.3.6.1.5.5.7.9.2", "placeOfBirth", N_("Place of Birth"), 
		EGG_OID_PRINTABLE },
	{ 0, "1.3.6.1.5.5.7.9.3", "gender", N_("Gender"), 
		EGG_OID_PRINTABLE },
	{ 0, "1.3.6.1.5.5.7.9.4", "countryOfCitizenship", N_("Country of Citizenship"),
		EGG_OID_PRINTABLE },
	{ 0, "1.3.6.1.5.5.7.9.5", "countryOfResidence", N_("Country of Residence"),
		EGG_OID_PRINTABLE },

	{ 0, "2.5.4.3", "CN", N_("Common Name"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.4", "surName", N_("Surname"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.5", "serialNumber", N_("Serial Number"), 
		EGG_OID_PRINTABLE },
	{ 0, "2.5.4.6", "C", N_("Country"), 
		EGG_OID_PRINTABLE, },
	{ 0, "2.5.4.7", "L", N_("Locality"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.8", "ST", N_("State"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.9", "STREET", N_("Street"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.10", "O", N_("Organization"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.11", "OU", N_("Organizational Unit"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.12", "T", N_("Title"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.20", "telephoneNumber", N_("Telephone Number"), 
		EGG_OID_PRINTABLE },
	{ 0, "2.5.4.42", "givenName", N_("Given Name"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.43", "initials", N_("Initials"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.44", "generationQualifier", N_("Generation Qualifier"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },
	{ 0, "2.5.4.46", "dnQualifier", N_("DN Qualifier"), 
		EGG_OID_PRINTABLE },
	{ 0, "2.5.4.65", "pseudonym", N_("Pseudonym"), 
		EGG_OID_PRINTABLE | EGG_OID_IS_CHOICE },

	{ 0, "1.2.840.113549.1.1.1", "rsaEncryption", N_("RSA"), 0 },
	{ 0, "1.2.840.113549.1.1.2", "md2WithRSAEncryption", N_("MD2 with RSA"), 0 },
	{ 0, "1.2.840.113549.1.1.4", "md5WithRSAEncryption", N_("MD5 with RSA"), 0 },
	{ 0, "1.2.840.113549.1.1.5", "sha1WithRSAEncryption", N_("SHA1 with RSA"), 0 },

	{ 0, "1.2.840.10040.4.1", "dsa", N_("DSA"), 0 },
	{ 0, "1.2.840.10040.4.3", "sha1WithDSA", N_("SHA1 with DSA"), 0 },

	/* Extended Key Usages */
	{ 0, "1.3.6.1.5.5.7.3.1", NULL, N_("Server Authentication"), 0 },
	{ 0, "1.3.6.1.5.5.7.3.2", NULL, N_("Client Authentication"), 0 },
	{ 0, "1.3.6.1.5.5.7.3.3", NULL, N_("Code Signing"), 0 },
	{ 0, "1.3.6.1.5.5.7.3.4", NULL, N_("Email Protection"), 0 },
	{ 0, "1.3.6.1.5.5.7.3.8", NULL, N_("Time Stamping"), 0 },

	{ 0, NULL, NULL, NULL, FALSE }
};

static OidInfo*
find_oid_info (GQuark oid)
{
	static volatile gsize inited_oids = 0;
	int i;
	
	g_return_val_if_fail (oid != 0, NULL);

	/* Initialize first time around */
	if (g_once_init_enter (&inited_oids)) {
		for (i = 0; oid_info[i].oidstr != NULL; ++i)
			oid_info[i].oid = g_quark_from_static_string (oid_info[i].oidstr);
		g_once_init_leave (&inited_oids, 1);
	}
	
	for (i = 0; oid_info[i].oidstr != NULL; ++i) {
		if (oid_info[i].oid == oid)
			return &oid_info[i];
	}
	
	return NULL;
}

const gchar*
egg_oid_get_name (GQuark oid)
{
	OidInfo *info;
	
	g_return_val_if_fail (oid, NULL);
	
	info = find_oid_info (oid);
	if (info == NULL)
		return g_quark_to_string (oid);
	
	return info->attr;	
}

const gchar*
egg_oid_get_description (GQuark oid)
{
	OidInfo *info;
	
	g_return_val_if_fail (oid, NULL);
	
	info = find_oid_info (oid);
	if (info == NULL)
		return g_quark_to_string (oid);
	
	return gettext (info->description);
}

guint
egg_oid_get_flags (GQuark oid)
{
	OidInfo *info;
	
	g_return_val_if_fail (oid, 0);
	
	info = find_oid_info (oid);
	if (info == NULL)
		return 0;
	
	return info->flags;	
}
