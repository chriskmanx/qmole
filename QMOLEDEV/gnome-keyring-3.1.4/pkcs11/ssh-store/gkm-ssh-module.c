/*
 * gnome-keyring
 *
 * Copyright (C) 2008 Stefan Walter
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

#include "config.h"

#include "gkm-ssh-module.h"
#include "gkm-ssh-private-key.h"
#include "gkm-ssh-public-key.h"

#include "egg/egg-error.h"

#include "gkm/gkm-file-tracker.h"

#include <string.h>

struct _GkmSshModule {
	GkmModule parent;
	GkmFileTracker *tracker;
	gchar *directory;
	GHashTable *keys_by_path;
};

static const CK_SLOT_INFO gkm_ssh_module_slot_info = {
	"SSH Keys",
	"Gnome Keyring",
	CKF_TOKEN_PRESENT,
	{ 0, 0 },
	{ 0, 0 }
};

static const CK_TOKEN_INFO gkm_ssh_module_token_info = {
	"SSH Keys",
	"Gnome Keyring",
	"1.0",
	"1:SSH:HOME", /* Unique serial number for manufacturer */
	CKF_TOKEN_INITIALIZED | CKF_WRITE_PROTECTED | CKF_USER_PIN_INITIALIZED,
	CK_EFFECTIVELY_INFINITE,
	CK_EFFECTIVELY_INFINITE,
	CK_EFFECTIVELY_INFINITE,
	CK_EFFECTIVELY_INFINITE,
	1024,
	1,
	CK_UNAVAILABLE_INFORMATION,
	CK_UNAVAILABLE_INFORMATION,
	CK_UNAVAILABLE_INFORMATION,
	CK_UNAVAILABLE_INFORMATION,
	{ 0, 0 },
	{ 0, 0 },
	""
};


G_DEFINE_TYPE (GkmSshModule, gkm_ssh_module, GKM_TYPE_MODULE);

GkmModule*  _gkm_ssh_store_get_module_for_testing (void);

/* -----------------------------------------------------------------------------
 * ACTUAL PKCS#11 Module Implementation
 */

/* Include all the module entry points */
#include "gkm/gkm-module-ep.h"
GKM_DEFINE_MODULE (gkm_ssh_module, GKM_TYPE_SSH_MODULE);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static gchar*
private_path_for_public (const gchar *public_path)
{
	gsize length;

	g_assert (public_path);

	length = strlen (public_path);
	if (length > 4 && g_str_equal (public_path + (length - 4), ".pub"))
		return g_strndup (public_path, length - 4);

	return NULL;
}

static void
file_load (GkmFileTracker *tracker, const gchar *path, GkmSshModule *self)
{
	GkmSshPrivateKey *key;
	gchar *private_path;
	GError *error = NULL;
	gchar *unique;

	g_return_if_fail (path);
	g_return_if_fail (GKM_IS_SSH_MODULE (self));

	private_path = private_path_for_public (path);
	if (!private_path || !g_file_test (private_path, G_FILE_TEST_IS_REGULAR)) {
		g_message ("no private key present for public key: %s", path);
		g_free (private_path);
		return;
	}

	/* Create a key if necessary */
	key = g_hash_table_lookup (self->keys_by_path, path);
	if (key == NULL) {
		unique = g_strdup_printf ("ssh-store:%s", private_path);
		key = gkm_ssh_private_key_new (GKM_MODULE (self), unique);
		g_free (unique);

		g_hash_table_replace (self->keys_by_path, g_strdup (path), key);
	}

	/* Parse the data into the key */
	if (!gkm_ssh_private_key_parse (key, path, private_path, &error)) {
		if (error) {
			g_message ("couldn't parse data: %s: %s", path, egg_error_message (error));
			g_clear_error (&error);
		}
		gkm_object_expose (GKM_OBJECT (key), FALSE);

	/* When successful register with the object manager */
	} else {
		gkm_object_expose (GKM_OBJECT (key), TRUE);
	}

	g_free (private_path);
}

static void
file_remove (GkmFileTracker *tracker, const gchar *path, GkmSshModule *self)
{
	g_return_if_fail (path);
	g_return_if_fail (GKM_IS_SSH_MODULE (self));
	g_hash_table_remove (self->keys_by_path, path);
}


/* -----------------------------------------------------------------------------
 * OBJECT
 */

static const CK_SLOT_INFO*
gkm_ssh_module_real_get_slot_info (GkmModule *self)
{
	return &gkm_ssh_module_slot_info;
}

static const CK_TOKEN_INFO*
gkm_ssh_module_real_get_token_info (GkmModule *self)
{
	return &gkm_ssh_module_token_info;
}

static void
gkm_ssh_module_real_parse_argument (GkmModule *base, const gchar *name, const gchar *value)
{
	GkmSshModule *self = GKM_SSH_MODULE (base);
	if (g_str_equal (name, "directory")) {
		g_free (self->directory);
		self->directory = g_strdup (value);
	}
}

static CK_RV
gkm_ssh_module_real_refresh_token (GkmModule *base)
{
	GkmSshModule *self = GKM_SSH_MODULE (base);
	gkm_file_tracker_refresh (self->tracker, FALSE);
	return CKR_OK;
}

static GObject*
gkm_ssh_module_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmSshModule *self = GKM_SSH_MODULE (G_OBJECT_CLASS (gkm_ssh_module_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	if (!self->directory)
		self->directory = g_strdup ("~/.ssh");
	self->tracker = gkm_file_tracker_new (self->directory, "*.pub", NULL);
	g_signal_connect (self->tracker, "file-added", G_CALLBACK (file_load), self);
	g_signal_connect (self->tracker, "file-changed", G_CALLBACK (file_load), self);
	g_signal_connect (self->tracker, "file-removed", G_CALLBACK (file_remove), self);

	return G_OBJECT (self);
}

static void
gkm_ssh_module_init (GkmSshModule *self)
{
	self->keys_by_path = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_object_unref);
}

static void
gkm_ssh_module_dispose (GObject *obj)
{
	GkmSshModule *self = GKM_SSH_MODULE (obj);

	if (self->tracker)
		g_object_unref (self->tracker);
	self->tracker = NULL;

	g_hash_table_remove_all (self->keys_by_path);

	G_OBJECT_CLASS (gkm_ssh_module_parent_class)->dispose (obj);
}

static void
gkm_ssh_module_finalize (GObject *obj)
{
	GkmSshModule *self = GKM_SSH_MODULE (obj);

	g_assert (self->tracker == NULL);

	g_hash_table_destroy (self->keys_by_path);
	self->keys_by_path = NULL;

	g_free (self->directory);
	self->directory = NULL;

	G_OBJECT_CLASS (gkm_ssh_module_parent_class)->finalize (obj);
}

static void
gkm_ssh_module_class_init (GkmSshModuleClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmModuleClass *module_class = GKM_MODULE_CLASS (klass);

	gobject_class->constructor = gkm_ssh_module_constructor;
	gobject_class->dispose = gkm_ssh_module_dispose;
	gobject_class->finalize = gkm_ssh_module_finalize;

	module_class->get_slot_info = gkm_ssh_module_real_get_slot_info;
	module_class->get_token_info = gkm_ssh_module_real_get_token_info;
	module_class->parse_argument = gkm_ssh_module_real_parse_argument;
	module_class->refresh_token = gkm_ssh_module_real_refresh_token;
}

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

CK_FUNCTION_LIST_PTR
gkm_ssh_store_get_functions (void)
{
	gkm_crypto_initialize ();
	return gkm_ssh_module_function_list;
}

GkmModule*
_gkm_ssh_store_get_module_for_testing (void)
{
	return pkcs11_module;
}
