/*
 * gnome-keyring
 *
 * Copyright 2010 (C) Collabora Ltd.
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
 *
 * Author: Stef Walter <stefw@collabora.co.uk>
 */

#include "config.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-dn.h"
#include "egg/egg-error.h"
#include "egg/egg-asn1-defs.h"

#include <libtasn1.h>
#include <stdlib.h>

/* Bring in the relevant definitions */
#include "../asn1-def-xdg.c"

static void
barf_and_die (const gchar *msg, const gchar *detail)
{
	if (detail)
		g_printerr ("diddle-trust-file: %s: %s\n", msg, detail);
	else
		g_printerr ("diddle-trust-file: %s\n", msg);
	exit (1);
}

static void
create_trust_file_for_certificate (const gchar *filename, const gchar *certificate)
{
	GError *err = NULL;
	GNode *asn, *cert, *choice, *ref;
	gchar *data, *result;
	gsize n_data, n_result;

	if (!g_file_get_contents (certificate, &data, &n_data, &err))
		barf_and_die ("couldn't read certificate file", egg_error_message (err));

	/* Make sure the certificate is */
	cert = egg_asn1x_create (pkix_asn1_tab, "Certificate");
	g_return_if_fail (cert);
	if (!egg_asn1x_decode (cert, data, n_data))
		barf_and_die ("couldn't parse der certificate file", egg_asn1x_message (cert));

	asn = egg_asn1x_create (xdg_asn1_tab, "trust-1");
	g_return_if_fail (asn);

	ref = egg_asn1x_node (asn, "reference", NULL);
	choice = egg_asn1x_node (ref, "certComplete", NULL);

	if (!egg_asn1x_set_choice (ref, choice) ||
	    !egg_asn1x_set_raw_element (choice, data, n_data, g_free))
		g_return_if_reached ();

	result = egg_asn1x_encode (asn, NULL, &n_result);
	if (result == NULL)
		barf_and_die ("couldn't encode the trust file", egg_asn1x_message (asn));

	egg_asn1x_destroy (asn);
	egg_asn1x_destroy (cert);

	if (!g_file_set_contents (filename, result, n_result, &err))
		barf_and_die ("couldn't write trust file", egg_error_message (err));
}

static void
create_trust_file_for_issuer_and_serial (const gchar *filename, const gchar *certificate)
{
	GError *err = NULL;
	GNode *asn, *cert, *choice, *ref;
	GNode *issuer, *serial;
	gchar *data, *result;
	gpointer value;
	gconstpointer element;
	gsize n_data, n_result, n_element, n_value;

	if (!g_file_get_contents (certificate, &data, &n_data, &err))
		barf_and_die ("couldn't read certificate file", egg_error_message (err));

	/* Make sure the certificate is */
	cert = egg_asn1x_create (pkix_asn1_tab, "Certificate");
	g_return_if_fail (cert);
	if (!egg_asn1x_decode (cert, data, n_data))
		barf_and_die ("couldn't parse der certificate file", egg_asn1x_message (cert));

	/* Dig out the issuer and serial */
	issuer = egg_asn1x_node (cert, "tbsCertificate", "issuer", NULL);
	serial = egg_asn1x_node (cert, "tbsCertificate", "serialNumber", NULL);
	g_return_if_fail (issuer && serial);

	/* Create up the trust structure */
	asn = egg_asn1x_create (xdg_asn1_tab, "trust-1");
	g_return_if_fail (asn);

	/* Setup the type of trust assertion */
	ref = egg_asn1x_node (asn, "reference", NULL);
	choice = egg_asn1x_node (ref, "certReference", NULL);
	if (!egg_asn1x_set_choice (ref, choice))
		g_return_if_reached ();

	/* Copy over the serial and issuer */
	element = egg_asn1x_get_raw_element (issuer, &n_element);
	if (!egg_asn1x_set_raw_element (egg_asn1x_node (choice, "issuer", NULL),
	                                g_memdup (element, n_element), n_element, g_free))
		g_return_if_reached ();
	value = egg_asn1x_get_integer_as_raw (serial, NULL, &n_value);
	if (!egg_asn1x_set_integer_as_raw (egg_asn1x_node (choice, "serialNumber", NULL), value, n_value, g_free))
		g_return_if_reached ();

	result = egg_asn1x_encode (asn, NULL, &n_result);
	if (result == NULL)
		barf_and_die ("couldn't encode the trust file", egg_asn1x_message (asn));

	g_free (data);
	egg_asn1x_destroy (cert);
	egg_asn1x_destroy (asn);

	if (!g_file_set_contents (filename, result, n_result, &err))
		barf_and_die ("couldn't write trust file", egg_error_message (err));

	g_free (result);
}

static void
add_trust_purpose_to_file (const gchar *filename, const gchar *purpose)
{
	GError *err = NULL;
	gchar *data, *result;
	gsize n_data, n_result;
	GNode *asn, *assertion;

	if (!g_file_get_contents (filename, &data, &n_data, &err))
		barf_and_die ("couldn't read trust file", egg_error_message (err));

	/* Create up the trust structure */
	asn = egg_asn1x_create (xdg_asn1_tab, "trust-1");
	g_return_if_fail (asn);

	/* And parse it */
	if (!egg_asn1x_decode (asn, data, n_data))
		barf_and_die ("couldn't parse trust file", egg_asn1x_message (asn));

	assertion = egg_asn1x_append (egg_asn1x_node (asn, "assertions", NULL));
	g_return_if_fail (assertion);

	if (!egg_asn1x_set_string_as_utf8 (egg_asn1x_node (assertion, "purpose", NULL), g_strdup (purpose), g_free) ||
	    !egg_asn1x_set_enumerated (egg_asn1x_node (assertion, "level", NULL), g_quark_from_string ("trusted")))
		g_return_if_reached ();

	result = egg_asn1x_encode (asn, NULL, &n_result);
	if (result == NULL)
		barf_and_die ("couldn't encode trust file", egg_asn1x_message (asn));

	g_free (data);
	egg_asn1x_destroy (asn);

	if (!g_file_set_contents (filename, result, n_result, &err))
		barf_and_die ("couldn't write trust file", egg_error_message (err));

	g_free (result);
}

/* --------------------------------------------------------------------------------
 * MAIN
 */

static gchar *create_for_file = NULL;
static gchar *refer_for_file = NULL;
static gchar *add_trust_purpose = NULL;

static GOptionEntry option_entries[] = {
	{ "create", '\0', 0, G_OPTION_ARG_FILENAME, &create_for_file,
	  "Create trust file for full certificate.", "certificate" },
	{ "refer", '\0', 0, G_OPTION_ARG_FILENAME, &refer_for_file,
	  "Create trust file for issuer+serial certificate", "certificate" },
	{ "add-trust", '\0', 0, G_OPTION_ARG_STRING, &add_trust_purpose,
	  "Add trust purpose to trust file", "purpose" },
	{ NULL }
};

int
main(int argc, char* argv[])
{
	GError *err = NULL;
	GOptionContext *context;

	context = g_option_context_new ("trust-file");
	g_option_context_add_main_entries (context, option_entries, GETTEXT_PACKAGE);
	if (!g_option_context_parse (context, &argc, &argv, &err))
		barf_and_die (egg_error_message (err), NULL);

	g_option_context_free (context);

	if (argc != 2)
		barf_and_die ("specify trust-file", NULL);

	if (((create_for_file ? 1 : 0) +
	     (refer_for_file ? 1 : 0) +
	     (add_trust_purpose ? 1 : 0)) > 1)
		barf_and_die ("incompatible options specified", NULL);

	if (create_for_file)
		create_trust_file_for_certificate (argv[1], create_for_file);
	else if (refer_for_file)
		create_trust_file_for_issuer_and_serial (argv[1], refer_for_file);
	else if (add_trust_purpose)
		add_trust_purpose_to_file (argv[1], add_trust_purpose);

	g_free (create_for_file);
	g_free (refer_for_file);
	g_free (add_trust_purpose);

	return 0;
}
