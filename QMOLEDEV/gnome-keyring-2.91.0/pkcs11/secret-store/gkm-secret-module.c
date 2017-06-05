/*
 * gnome-keyring
 *
 * Copyright (C) 2009 Stefan Walter
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

#include "gkm-secret-collection.h"
#include "gkm-secret-item.h"
#include "gkm-secret-module.h"
#include "gkm-secret-search.h"
#include "gkm-secret-store.h"

#include "gkm/gkm-credential.h"
#include "gkm/gkm-file-tracker.h"
#include "gkm/gkm-transaction.h"

#include <glib/gstdio.h>

#include <errno.h>
#include <fcntl.h>
#include <string.h>

struct _GkmSecretModule {
	GkmModule parent;
	GkmFileTracker *tracker;
	GHashTable *collections;
	gchar *directory;
	GkmCredential *session_credential;
};

static const CK_SLOT_INFO gkm_secret_module_slot_info = {
	"Secret Store",
	"Gnome Keyring",
	CKF_TOKEN_PRESENT,
	{ 0, 0 },
	{ 0, 0 }
};

static const CK_TOKEN_INFO gkm_secret_module_token_info = {
	"Secret Store",
	"Gnome Keyring",
	"1.0",
	"1:SECRET:MAIN", /* Unique serial number for manufacturer */
	CKF_TOKEN_INITIALIZED | CKF_USER_PIN_INITIALIZED | CKF_LOGIN_REQUIRED,
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

G_DEFINE_TYPE (GkmSecretModule, gkm_secret_module, GKM_TYPE_MODULE);

GkmModule*  _gkm_secret_store_get_module_for_testing (void);

/* Forward declarations */
static void add_collection (GkmSecretModule *, GkmTransaction *, GkmSecretCollection *);
static void remove_collection (GkmSecretModule *, GkmTransaction *, GkmSecretCollection *);

/* -----------------------------------------------------------------------------
 * ACTUAL PKCS#11 Module Implementation
 */

/* Include all the module entry points */
#include "gkm/gkm-module-ep.h"
GKM_DEFINE_MODULE (gkm_secret_module, GKM_TYPE_SECRET_MODULE);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static gboolean
complete_add (GkmTransaction *transaction, GObject *obj, gpointer user_data)
{
	GkmSecretCollection *collection = GKM_SECRET_COLLECTION (user_data);
	if (gkm_transaction_get_failed (transaction))
		remove_collection (GKM_SECRET_MODULE (obj), NULL, collection);
	g_object_unref (collection);
	return TRUE;
}

static void
add_collection (GkmSecretModule *self, GkmTransaction *transaction, GkmSecretCollection  *collection)
{
	const gchar *filename;

	g_assert (GKM_IS_SECRET_MODULE(self));
	g_assert (GKM_IS_SECRET_COLLECTION (collection));

	filename = gkm_secret_collection_get_filename (collection);
	g_return_if_fail (filename);

	g_hash_table_replace (self->collections, g_strdup (filename), g_object_ref (collection));

	gkm_object_expose_full (GKM_OBJECT (collection), transaction, TRUE);
	if (transaction)
		gkm_transaction_add (transaction, self, complete_add, g_object_ref (collection));
}

static gboolean
complete_remove (GkmTransaction *transaction, GObject *obj, gpointer user_data)
{
	GkmSecretCollection *collection = GKM_SECRET_COLLECTION (user_data);
	if (gkm_transaction_get_failed (transaction))
		add_collection (GKM_SECRET_MODULE (obj), NULL, collection);
	g_object_unref (collection);
	return TRUE;
}

static void
remove_collection (GkmSecretModule *self, GkmTransaction *transaction, GkmSecretCollection *collection)
{
	const gchar *filename;

	g_assert (GKM_IS_SECRET_MODULE (self));
	g_assert (GKM_IS_SECRET_COLLECTION (collection));

	filename = gkm_secret_collection_get_filename (collection);
	g_return_if_fail (filename);

	g_hash_table_remove (self->collections, filename);

	gkm_object_expose_full (GKM_OBJECT (collection), transaction, FALSE);
	if (transaction)
		gkm_transaction_add (transaction, self, complete_remove, g_object_ref (collection));
}

static gchar*
identifier_from_filename (GkmSecretModule *self, const gchar *filename)
{
	gchar *identifier;

	/* Do we have one for this path yet? */
	identifier = g_path_get_basename (filename);

	/* Remove the keyring suffix */
	if (g_str_has_suffix (identifier, ".keyring"))
		identifier[strlen(identifier) - 8] = 0;

	return identifier;
}

static gchar*
identifier_to_new_filename (GkmSecretModule *self, const gchar *identifier)
{
	gchar *filename;
	gint i;
	int fd;

	for (i = 0; i < G_MAXINT; ++i) {
		if (i == 0)
			filename = g_strdup_printf ("%s/%s.keyring", self->directory, identifier);
		else
			filename = g_strdup_printf ("%s/%s_%d.keyring", self->directory, identifier, i);

		/* Try to create the file, and check that it doesn't exist */
		fd = g_open (filename, O_RDONLY | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
		if (fd == -1) {
			if (errno != EEXIST)
				break;
		} else {
			close (fd);
			break;
		}

		g_free (filename);
	}

	return filename;
}


static void
on_file_load (GkmFileTracker *tracker, const gchar *path, GkmSecretModule *self)
{
	GkmSecretCollection *collection;
	GkmManager *manager;
	GkmDataResult res;
	gboolean created;
	gchar *identifier;

	manager = gkm_module_get_manager (GKM_MODULE (self));
	g_return_if_fail (manager);

	/* Do we have one for this path yet? */
	identifier = identifier_from_filename (self, path);
	collection = g_hash_table_lookup (self->collections, path);

	if (collection == NULL) {
		created = TRUE;
		collection = g_object_new (GKM_TYPE_SECRET_COLLECTION,
		                           "module", self,
		                           "identifier", identifier,
		                           "filename", path,
		                           "manager", manager,
		                           NULL);
	} else {
		created = FALSE;
		g_object_ref (collection);
	}

	res = gkm_secret_collection_load (collection);

	switch (res) {
	case GKM_DATA_SUCCESS:
		if (created)
			add_collection (self, NULL, collection);
		break;
	case GKM_DATA_LOCKED:
		g_message ("master password for keyring changed without our knowledge: %s", path);
		gkm_secret_collection_unlocked_clear (collection);
		break;
	case GKM_DATA_UNRECOGNIZED:
		g_message ("keyring was in an invalid or unrecognized format: %s", path);
		break;
	case GKM_DATA_FAILURE:
		g_message ("failed to parse keyring: %s", path);
		break;
	default:
		g_assert_not_reached ();
	}

	g_object_unref (collection);
	g_free (identifier);
}

static void
on_file_remove (GkmFileTracker *tracker, const gchar *path, GkmSecretModule *self)
{
	GkmSecretCollection *collection;

	g_return_if_fail (path);
	g_return_if_fail (GKM_IS_SECRET_MODULE (self));

	collection = g_hash_table_lookup (self->collections, path);
	if (collection)
		remove_collection (self, NULL, collection);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static const CK_SLOT_INFO*
gkm_secret_module_real_get_slot_info (GkmModule *self)
{
	return &gkm_secret_module_slot_info;
}

static const CK_TOKEN_INFO*
gkm_secret_module_real_get_token_info (GkmModule *self)
{
	return &gkm_secret_module_token_info;
}

static void
gkm_secret_module_real_parse_argument (GkmModule *base, const gchar *name, const gchar *value)
{
	GkmSecretModule *self = GKM_SECRET_MODULE (base);
	if (g_str_equal (name, "directory")) {
		g_free (self->directory);
		self->directory = g_strdup (value);
	}
}

static CK_RV
gkm_secret_module_real_refresh_token (GkmModule *base)
{
	GkmSecretModule *self = GKM_SECRET_MODULE (base);
	if (self->tracker)
		gkm_file_tracker_refresh (self->tracker, FALSE);
	return CKR_OK;
}

static void
gkm_secret_module_real_add_object (GkmModule *module, GkmTransaction *transaction,
                                   GkmObject *object)
{
	GkmSecretModule *self = GKM_SECRET_MODULE (module);
	GkmSecretCollection *collection;
	const gchar *identifier;
	gchar *filename;

	g_return_if_fail (!gkm_transaction_get_failed (transaction));

	if (GKM_IS_SECRET_COLLECTION (object)) {
		collection = GKM_SECRET_COLLECTION (object);

		/* Setup a filename for this collection */
		identifier = gkm_secret_object_get_identifier (GKM_SECRET_OBJECT (collection));
		filename = identifier_to_new_filename (self, identifier);
		gkm_secret_collection_set_filename (collection, filename);
		g_free (filename);

		add_collection (self, transaction, collection);
	}
}

static void
gkm_secret_module_real_store_object (GkmModule *module, GkmTransaction *transaction,
                                     GkmObject *object)
{
	GkmSecretModule *self = GKM_SECRET_MODULE (module);
	GkmSecretCollection *collection = NULL;

	/* Store the item's collection */
	if (GKM_IS_SECRET_ITEM (object)) {
		collection = gkm_secret_item_get_collection (GKM_SECRET_ITEM (object));
		g_return_if_fail (GKM_IS_SECRET_COLLECTION (collection));
		gkm_module_store_token_object (GKM_MODULE (self), transaction, GKM_OBJECT (collection));

	/* Storing a collection */
	} else if (GKM_IS_SECRET_COLLECTION (object)) {
		collection = GKM_SECRET_COLLECTION (object);
		gkm_secret_collection_save (collection, transaction);

	/* No other kind of token object */
	} else {
		g_warning ("can't store object of type '%s' on secret token", G_OBJECT_TYPE_NAME (object));
		gkm_transaction_fail (transaction, CKR_GENERAL_ERROR);
	}
}

static void
gkm_secret_module_real_remove_object (GkmModule *module, GkmTransaction *transaction,
                                      GkmObject *object)
{
	GkmSecretModule *self = GKM_SECRET_MODULE (module);
	GkmSecretCollection *collection;

	/* Ignore the session keyring credentials */
	if (self->session_credential != NULL &&
	    GKM_OBJECT (self->session_credential) == object)
		return;

	/* Removing an item */
	if (GKM_IS_SECRET_ITEM (object)) {
		collection = gkm_secret_item_get_collection (GKM_SECRET_ITEM (object));
		g_return_if_fail (GKM_IS_SECRET_COLLECTION (collection));
		gkm_secret_collection_destroy_item (collection, transaction, GKM_SECRET_ITEM (object));
		if (!gkm_transaction_get_failed (transaction))
			gkm_secret_collection_save (collection, transaction);

	/* Removing a collection */
	} else if (GKM_IS_SECRET_COLLECTION (object)) {
		collection = GKM_SECRET_COLLECTION (object);
		gkm_secret_collection_destroy (collection, transaction);
		if (!gkm_transaction_get_failed (transaction))
			remove_collection (self, transaction, collection);

	/* No other token objects */
	} else {
		g_warning ("Trying to remove token object of type '%s' from secret "
		           "module, but that type is not supported.", G_OBJECT_TYPE_NAME (object));
		gkm_transaction_fail (transaction, CKR_FUNCTION_NOT_SUPPORTED);
	}
}

static GObject*
gkm_secret_module_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmSecretModule *self = GKM_SECRET_MODULE (G_OBJECT_CLASS (gkm_secret_module_parent_class)->constructor(type, n_props, props));
	GkmManager *manager;
	GkmObject *collection;
	CK_RV rv;

	g_return_val_if_fail (self, NULL);

	if (!self->directory) {
		self->directory = g_build_filename (g_get_home_dir (), ".gnome2", "keyrings", NULL);
		if (g_mkdir_with_parents (self->directory, S_IRWXU) < 0)
			g_warning ("unable to create keyring dir: %s", self->directory);
	}

	self->tracker = gkm_file_tracker_new (self->directory, "*.keyring", NULL);
	g_signal_connect (self->tracker, "file-added", G_CALLBACK (on_file_load), self);
	g_signal_connect (self->tracker, "file-changed", G_CALLBACK (on_file_load), self);
	g_signal_connect (self->tracker, "file-removed", G_CALLBACK (on_file_remove), self);

	manager = gkm_module_get_manager (GKM_MODULE (self));

	collection = g_object_new (GKM_TYPE_SECRET_COLLECTION,
	                           "module", self,
	                           "identifier", "session",
	                           "manager", manager,
	                           "transient", TRUE,
	                           NULL);

	/* Create the 'session' keyring, which is not stored to disk */
	g_return_val_if_fail (gkm_object_is_transient (collection), NULL);
	gkm_module_add_token_object (GKM_MODULE (self), NULL, collection);
	gkm_object_expose (collection, TRUE);

	/* Unlock the 'session' keyring */
	rv = gkm_credential_create (GKM_MODULE (self), manager, GKM_OBJECT (collection),
	                            NULL, 0, &self->session_credential);
	if (rv == CKR_OK)
		gkm_object_expose (GKM_OBJECT (self->session_credential), TRUE);
	else
		g_warning ("couldn't unlock the 'session' keyring");

	g_object_unref (collection);
	return G_OBJECT (self);
}

static void
gkm_secret_module_init (GkmSecretModule *self)
{
	self->collections = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_object_unref);
	gkm_module_register_factory (GKM_MODULE (self), GKM_FACTORY_SECRET_SEARCH);
	gkm_module_register_factory (GKM_MODULE (self), GKM_FACTORY_SECRET_ITEM);
	gkm_module_register_factory (GKM_MODULE (self), GKM_FACTORY_SECRET_COLLECTION);
}

static void
gkm_secret_module_dispose (GObject *obj)
{
	GkmSecretModule *self = GKM_SECRET_MODULE (obj);

	if (self->tracker)
		g_object_unref (self->tracker);
	self->tracker = NULL;

	if (self->session_credential)
		g_object_unref (self->session_credential);
	self->session_credential = NULL;

	g_hash_table_remove_all (self->collections);

	G_OBJECT_CLASS (gkm_secret_module_parent_class)->dispose (obj);
}

static void
gkm_secret_module_finalize (GObject *obj)
{
	GkmSecretModule *self = GKM_SECRET_MODULE (obj);

	g_assert (self->tracker == NULL);

	g_hash_table_destroy (self->collections);
	self->collections = NULL;

	g_free (self->directory);
	self->directory = NULL;

	g_assert (!self->session_credential);

	G_OBJECT_CLASS (gkm_secret_module_parent_class)->finalize (obj);
}

static void
gkm_secret_module_class_init (GkmSecretModuleClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmModuleClass *module_class = GKM_MODULE_CLASS (klass);

	gobject_class->constructor = gkm_secret_module_constructor;
	gobject_class->dispose = gkm_secret_module_dispose;
	gobject_class->finalize = gkm_secret_module_finalize;

	module_class->get_slot_info = gkm_secret_module_real_get_slot_info;
	module_class->get_token_info = gkm_secret_module_real_get_token_info;
	module_class->parse_argument = gkm_secret_module_real_parse_argument;
	module_class->refresh_token = gkm_secret_module_real_refresh_token;
	module_class->add_token_object = gkm_secret_module_real_add_object;
	module_class->store_token_object = gkm_secret_module_real_store_object;
	module_class->remove_token_object = gkm_secret_module_real_remove_object;
}

/* ---------------------------------------------------------------------------------------
 * PUBLIC
 */

CK_FUNCTION_LIST_PTR
gkm_secret_store_get_functions (void)
{
	gkm_crypto_initialize ();
	return gkm_secret_module_function_list;
}

GkmModule*
_gkm_secret_store_get_module_for_testing (void)
{
	return pkcs11_module;
}
