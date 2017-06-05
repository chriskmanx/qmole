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

#include "config.h"

#include "gkm-xdg-assertion.h"
#include "gkm-xdg-module.h"
#include "gkm-xdg-store.h"
#include "gkm-xdg-trust.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-asn1-defs.h"
#include "egg/egg-dn.h"
#include "egg/egg-error.h"
#include "egg/egg-hex.h"

#include "gkm/gkm-assertion.h"
#include "gkm/gkm-certificate.h"
#include "gkm/gkm-file-tracker.h"
#include "gkm/gkm-serializable.h"
#include "gkm/gkm-transaction.h"
#include "gkm/gkm-util.h"

#include <string.h>

struct _GkmXdgModule {
	GkmModule parent;
	gchar *directory;
	GHashTable *objects_by_path;
	GkmFileTracker *tracker;
	CK_TOKEN_INFO token_info;
};

static const CK_SLOT_INFO user_module_slot_info = {
	"User Key Storage",
	"Gnome Keyring",
	CKF_TOKEN_PRESENT,
	{ 0, 0 },
	{ 0, 0 }
};

static const CK_TOKEN_INFO user_module_token_info = {
	"User Key Storage",
	"Gnome Keyring",
	"1.0",
	"1:XDG:DEFAULT", /* Unique serial number for manufacturer */
	CKF_TOKEN_INITIALIZED,
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

#define UNUSED_VALUE (GUINT_TO_POINTER (1))

#define UNWANTED_FILENAME_CHARS  ":/\\<>|\t\n\r\v "

G_DEFINE_TYPE (GkmXdgModule, gkm_xdg_module, GKM_TYPE_MODULE);

GkmModule*  _gkm_xdg_store_get_module_for_testing (void);

/* Forward declarations */
static void  remove_object_from_module (GkmXdgModule *self, GkmObject *object,
                                        const gchar *filename, GkmTransaction *transaction);
static void  add_object_to_module (GkmXdgModule *self, GkmObject *object,
                                   const gchar *filename, GkmTransaction *transaction);

/* -----------------------------------------------------------------------------
 * ACTUAL PKCS#11 Module Implementation
 */

/* Include all the module entry points */
#include "gkm/gkm-module-ep.h"
GKM_DEFINE_MODULE (gkm_xdg_module, GKM_TYPE_XDG_MODULE);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static GType
type_from_path (const gchar *path)
{
	const gchar *ext;

	ext = strrchr (path, '.');

	/* The file tracker doesn't match files without exts */
	g_return_val_if_fail (ext, 0);

	if (g_str_equal (ext, ".trust"))
		return GKM_XDG_TYPE_TRUST;
	else if (strcmp (ext, ".cer") == 0)
		return GKM_TYPE_CERTIFICATE;

	return 0;
}

static const gchar*
lookup_filename_for_object (GkmObject *object)
{
	return g_object_get_data (G_OBJECT (object), "xdg-module-filename");
}

static gboolean
complete_add_object (GkmTransaction *transaction, GObject *module, gpointer user_data)
{
	GkmXdgModule *self = GKM_XDG_MODULE (module);
	GkmObject *object = GKM_OBJECT (user_data);
	const gchar *filename;

	/* If the transaction failed, revert it */
	if (gkm_transaction_get_failed (transaction)) {
		filename = g_object_get_data (G_OBJECT (object), "xdg-module-filename");
		g_return_val_if_fail (filename, FALSE);
		remove_object_from_module (self, object, filename, NULL);
	}

	g_object_unref (object);
	return TRUE;
}

static void
add_object_to_module (GkmXdgModule *self, GkmObject *object,
                      const gchar *filename, GkmTransaction *transaction)
{
	g_assert (!g_hash_table_lookup (self->objects_by_path, filename));
	g_hash_table_insert (self->objects_by_path, g_strdup (filename), g_object_ref (object));

	g_assert (!lookup_filename_for_object (object));
	g_object_set_data_full (G_OBJECT (object), "xdg-module-filename",
	                        g_strdup (filename), g_free);

	gkm_object_expose (object, TRUE);

	if (transaction != NULL)
		gkm_transaction_add (transaction, self, complete_add_object, g_object_ref (object));
}

static gboolean
complete_remove_object (GkmTransaction *transaction, GObject *module, gpointer user_data)
{
	GkmXdgModule *self = GKM_XDG_MODULE (module);
	GkmObject *object = GKM_OBJECT (user_data);
	const gchar *filename;

	/* If the transaction failed, revert it */
	if (gkm_transaction_get_failed (transaction)) {
		filename = g_object_get_data (G_OBJECT (object), "xdg-module-filename");
		g_return_val_if_fail (filename, FALSE);
		add_object_to_module (self, object, filename, NULL);
	}

	g_object_unref (object);
	return TRUE;
}

static void
remove_object_from_module (GkmXdgModule *self, GkmObject *object,
                           const gchar *filename, GkmTransaction *transaction)
{
	gkm_object_expose (object, FALSE);

	if (transaction != NULL)
		gkm_transaction_add (transaction, self, complete_remove_object, g_object_ref (object));

	g_assert (g_hash_table_lookup (self->objects_by_path, filename) == object);
	g_hash_table_remove (self->objects_by_path, filename);
}

static void
file_load (GkmFileTracker *tracker, const gchar *path, GkmXdgModule *self)
{
	GkmObject *object;
	GkmManager *manager;
	gboolean added = FALSE;
	GError *error = NULL;
	GType type;
	guchar *data;
	gsize n_data;

	g_return_if_fail (path);
	g_return_if_fail (GKM_IS_XDG_MODULE (self));

	manager = gkm_module_get_manager (GKM_MODULE (self));

	/* Already have this object? */
	object = g_hash_table_lookup (self->objects_by_path, path);
	if (object == NULL) {

		/* Figure out what type of object we're dealing with */
		type = type_from_path (path);
		if (type == 0) {
			g_message ("don't know how to load file in key store: %s", path);
			return;
		}

		/* Create a new object for this identifier */
		object = g_object_new (type,
		                       "module", GKM_MODULE (self),
		                       "manager", manager, NULL);
		g_return_if_fail (GKM_IS_SERIALIZABLE (object));
		g_return_if_fail (GKM_SERIALIZABLE_GET_INTERFACE (object)->extension);

		added = TRUE;

	} else {
		g_object_ref (object);
	}

	/* Read the file in */
	if (!g_file_get_contents (path, (gchar**)&data, &n_data, &error)) {
		g_warning ("couldn't read file in key store: %s: %s", path,
		           egg_error_message (error));
		g_object_unref (object);
		g_clear_error (&error);
		return;

	/* And load the data into it */
	} else if (gkm_serializable_load (GKM_SERIALIZABLE (object), NULL, data, n_data)) {
		if (added)
			add_object_to_module (self, object, path, NULL);
		gkm_object_expose (object, TRUE);

	} else {
		g_message ("failed to load file in user store: %s", path);
		if (!added) {
			gkm_object_expose (object, FALSE);
			remove_object_from_module (self, object, path, NULL);
		}
	}

	g_object_unref (object);
}

static void
file_remove (GkmFileTracker *tracker, const gchar *path, GkmXdgModule *self)
{
	GkmObject *object;

	g_return_if_fail (path);
	g_return_if_fail (GKM_IS_XDG_MODULE (self));

	object = g_hash_table_lookup (self->objects_by_path, path);
	if (object != NULL)
		remove_object_from_module (self, object, path, NULL);
}

static gchar*
name_for_subject (gconstpointer subject, gsize n_subject)
{
	GNode *asn;
	gchar *name;

	g_assert (subject);
	g_assert (n_subject);

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "Name", subject, n_subject);
	g_return_val_if_fail (asn, NULL);

	name = egg_dn_read_part (egg_asn1x_node (asn, "rdnSequence", NULL), "CN");
	egg_asn1x_destroy (asn);

	return name;
}

static gchar*
guess_basename_for_object (GkmObject *object)
{
	GkmSerializableIface *serial;
	const gchar *ext;
	gchar *filename;
	gchar *name = NULL;
	guchar *data;
	gsize n_data;

	g_assert (GKM_IS_OBJECT (object));
	g_assert (GKM_IS_SERIALIZABLE (object));

	/* Figure out the extension and prefix */
	serial = GKM_SERIALIZABLE_GET_INTERFACE (object);
	ext = serial->extension;
	g_return_val_if_fail (ext, NULL);

	/* First we try to use the CN of a subject */
	data = gkm_object_get_attribute_data (object, NULL, CKA_SUBJECT, &n_data);
	if (data && n_data)
		name = name_for_subject (data, n_data);
	g_free (data);

	/* Next we try hex encoding the ID */
	if (name == NULL) {
		data = gkm_object_get_attribute_data (object, NULL, CKA_ID, &n_data);
		if (data && n_data)
			name = egg_hex_encode (data, n_data);
		g_free (data);
	}

	if (name == NULL)
		name = g_strdup_printf ("object-%08x", ABS (g_random_int ()));

	/* Build up the identifier */
	filename = g_strconcat (name, ext, NULL);
	g_strdelimit (filename, UNWANTED_FILENAME_CHARS, '_');

	g_free (name);
	return filename;
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static const CK_SLOT_INFO*
gkm_xdg_module_real_get_slot_info (GkmModule *base)
{
	return &user_module_slot_info;
}

static const CK_TOKEN_INFO*
gkm_xdg_module_real_get_token_info (GkmModule *base)
{
	GkmXdgModule *self = GKM_XDG_MODULE (base);

	/* TODO: Update the info with current info */
	return &self->token_info;
}

static void
gkm_xdg_module_real_parse_argument (GkmModule *base, const gchar *name, const gchar *value)
{
	GkmXdgModule *self = GKM_XDG_MODULE (base);
	if (g_str_equal (name, "directory")) {
		g_free (self->directory);
		self->directory = g_strdup (value);
	}
}

static CK_RV
gkm_xdg_module_real_refresh_token (GkmModule *base)
{
	GkmXdgModule *self = GKM_XDG_MODULE (base);
	gkm_file_tracker_refresh (self->tracker, FALSE);
	return CKR_OK;
}

static void
gkm_xdg_module_real_add_token_object (GkmModule *module, GkmTransaction *transaction,
                                      GkmObject *object)
{
	gchar *filename = NULL;
	GkmXdgModule *self;
	GkmTrust *trust;
	gchar *basename;
	gchar *actual;

	self = GKM_XDG_MODULE (module);

	/* Always serialize the trust object for each assertion */
	if (GKM_XDG_IS_ASSERTION (object)) {
		trust = gkm_assertion_get_trust_object (GKM_ASSERTION (object));
		object = GKM_OBJECT (trust);

		/* If this trust object has already been added, then ignore */
		if (lookup_filename_for_object (object))
			return;
	}

	/* Double check that the object is in fact serializable */
	if (!GKM_IS_SERIALIZABLE (object)) {
		g_message ("can't store object of type '%s' on token", G_OBJECT_TYPE_NAME (object));
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
		return;
	}

	g_return_if_fail (lookup_filename_for_object (object) == NULL);

	basename = guess_basename_for_object (object);
	g_return_if_fail (basename);

	actual = gkm_transaction_unique_file (transaction, self->directory, basename);
	if (!gkm_transaction_get_failed (transaction)) {
		filename = g_build_filename (self->directory, actual, NULL);
		add_object_to_module (self, object, filename, transaction);
		g_free (filename);
	}

	g_free (actual);
	g_free (basename);
}

static void
gkm_xdg_module_real_store_token_object (GkmModule *module, GkmTransaction *transaction,
                                        GkmObject *object)
{
	GkmXdgModule *self = GKM_XDG_MODULE (module);
	GkmTrust *trust;
	const gchar *filename;
	gpointer data;
	gsize n_data;

	/* Always serialize the trust object for each assertion */
	if (GKM_XDG_IS_ASSERTION (object)) {
		trust = gkm_assertion_get_trust_object (GKM_ASSERTION (object));
		object = GKM_OBJECT (trust);
	}

	/* Double check that the object is in fact serializable */
	if (!GKM_IS_SERIALIZABLE (object)) {
		g_message ("can't store object of type '%s' on token", G_OBJECT_TYPE_NAME (object));
		gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
		return;
	}

	/* Serialize the object in question */
	if (!gkm_serializable_save (GKM_SERIALIZABLE (object), NULL, &data, &n_data)) {
		gkm_transaction_fail (transaction, CKR_FUNCTION_FAILED);
		g_return_if_reached ();
	}

	filename = lookup_filename_for_object (object);
	g_return_if_fail (filename != NULL);
	g_return_if_fail (g_hash_table_lookup (self->objects_by_path, filename) == object);

	gkm_transaction_write_file (transaction, filename, data, n_data);
	g_free (data);
}

static void
gkm_xdg_module_real_remove_token_object (GkmModule *module, GkmTransaction *transaction,
                                         GkmObject *object)
{
	GkmXdgModule *self = GKM_XDG_MODULE (module);
	const gchar *filename;
	GkmXdgTrust *trust;

	/* Always serialize the trust object for each assertion */
	if (GKM_XDG_IS_ASSERTION (object)) {
		trust = GKM_XDG_TRUST (gkm_assertion_get_trust_object (GKM_ASSERTION (object)));
		gkm_xdg_trust_remove_assertion (trust, GKM_ASSERTION (object), transaction);

		/* Remove the trust object if it has no assertions */
		if (!gkm_xdg_trust_have_assertion (trust))
			object = GKM_OBJECT (trust);
		else
			object = NULL;
	}

	if (object && !gkm_transaction_get_failed (transaction)) {
		filename = lookup_filename_for_object (object);
		g_return_if_fail (filename != NULL);
		g_return_if_fail (g_hash_table_lookup (self->objects_by_path, filename) == object);

		gkm_transaction_remove_file (transaction, filename);
		remove_object_from_module (self, object, filename, transaction);
	}
}

static GObject*
gkm_xdg_module_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmXdgModule *self = GKM_XDG_MODULE (G_OBJECT_CLASS (gkm_xdg_module_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	if (!self->directory)
		self->directory = g_build_filename (g_get_user_data_dir (), "keystore", NULL);

	self->tracker = gkm_file_tracker_new (self->directory, "*.*", NULL);
	g_signal_connect (self->tracker, "file-added", G_CALLBACK (file_load), self);
	g_signal_connect (self->tracker, "file-changed", G_CALLBACK (file_load), self);
	g_signal_connect (self->tracker, "file-removed", G_CALLBACK (file_remove), self);

	return G_OBJECT (self);
}

static void
gkm_xdg_module_init (GkmXdgModule *self)
{
	self->objects_by_path = g_hash_table_new_full (g_str_hash, g_str_equal,
	                                               g_free, gkm_util_dispose_unref);

	/* Our default token info, updated as module runs */
	memcpy (&self->token_info, &user_module_token_info, sizeof (CK_TOKEN_INFO));

	/* For creating stored objects */
	gkm_module_register_factory (GKM_MODULE (self), GKM_XDG_FACTORY_ASSERTION);
	gkm_module_register_factory (GKM_MODULE (self), GKM_FACTORY_CERTIFICATE);
}

static void
gkm_xdg_module_dispose (GObject *obj)
{
	GkmXdgModule *self = GKM_XDG_MODULE (obj);

	if (self->tracker)
		g_object_unref (self->tracker);
	self->tracker = NULL;

	g_hash_table_remove_all (self->objects_by_path);

	G_OBJECT_CLASS (gkm_xdg_module_parent_class)->dispose (obj);
}

static void
gkm_xdg_module_finalize (GObject *obj)
{
	GkmXdgModule *self = GKM_XDG_MODULE (obj);

	g_assert (self->tracker == NULL);

	g_hash_table_destroy (self->objects_by_path);
	self->objects_by_path = NULL;

	g_free (self->directory);
	self->directory = NULL;

	G_OBJECT_CLASS (gkm_xdg_module_parent_class)->finalize (obj);
}

static void
gkm_xdg_module_class_init (GkmXdgModuleClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmModuleClass *module_class = GKM_MODULE_CLASS (klass);

	gobject_class->constructor = gkm_xdg_module_constructor;
	gobject_class->dispose = gkm_xdg_module_dispose;
	gobject_class->finalize = gkm_xdg_module_finalize;

	module_class->get_slot_info = gkm_xdg_module_real_get_slot_info;
	module_class->get_token_info = gkm_xdg_module_real_get_token_info;
	module_class->parse_argument = gkm_xdg_module_real_parse_argument;
	module_class->refresh_token = gkm_xdg_module_real_refresh_token;
	module_class->add_token_object = gkm_xdg_module_real_add_token_object;
	module_class->store_token_object = gkm_xdg_module_real_store_token_object;
	module_class->remove_token_object = gkm_xdg_module_real_remove_token_object;
}

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

CK_FUNCTION_LIST_PTR
gkm_xdg_store_get_functions (void)
{
	gkm_crypto_initialize ();
	return gkm_xdg_module_function_list;
}

GkmModule*
_gkm_xdg_store_get_module_for_testing (void)
{
	return pkcs11_module;
}
