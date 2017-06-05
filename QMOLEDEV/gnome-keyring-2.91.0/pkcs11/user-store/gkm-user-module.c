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

#include "gkm-user-module.h"
#include "gkm-user-private-key.h"
#include "gkm-user-public-key.h"
#include "gkm-user-storage.h"
#include "gkm-user-store.h"

#include "gkm/gkm-certificate.h"
#include "gkm/gkm-data-asn1.h"
#include "gkm/gkm-manager.h"
#include "gkm/gkm-secret.h"
#include "gkm/gkm-transaction.h"
#include "gkm/gkm-util.h"

#include <string.h>

struct _GkmUserModule {
	GkmModule parent;
	GkmUserStorage *storage;
	gchar *directory;
	GHashTable *unlocked_apps;
	CK_TOKEN_INFO token_info;
};

static const CK_SLOT_INFO user_module_slot_info = {
	"User Keys",
	"Gnome Keyring",
	CKF_TOKEN_PRESENT,
	{ 0, 0 },
	{ 0, 0 }
};

static const CK_TOKEN_INFO user_module_token_info = {
	"User Keys",
	"Gnome Keyring",
	"1.0",
	"1:USER:DEFAULT", /* Unique serial number for manufacturer */
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

#define UNUSED_VALUE (GUINT_TO_POINTER (1))

G_DEFINE_TYPE (GkmUserModule, gkm_user_module, GKM_TYPE_MODULE);

/* -----------------------------------------------------------------------------
 * ACTUAL PKCS#11 Module Implementation
 */

/* Include all the module entry points */
#include "gkm/gkm-module-ep.h"
GKM_DEFINE_MODULE (gkm_user_module, GKM_TYPE_USER_MODULE);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static const CK_SLOT_INFO*
gkm_user_module_real_get_slot_info (GkmModule *base)
{
	return &user_module_slot_info;
}

static const CK_TOKEN_INFO*
gkm_user_module_real_get_token_info (GkmModule *base)
{
	GkmUserModule *self = GKM_USER_MODULE (base);

	/* Update the info with current info */
	self->token_info.flags = gkm_user_storage_token_flags (self->storage);

	return &self->token_info;
}

static void
gkm_user_module_real_parse_argument (GkmModule *base, const gchar *name, const gchar *value)
{
	GkmUserModule *self = GKM_USER_MODULE (base);
	if (g_str_equal (name, "directory")) {
		g_free (self->directory);
		self->directory = g_strdup (value);
	}
}

static CK_RV
gkm_user_module_real_refresh_token (GkmModule *base)
{
	GkmUserModule *self = GKM_USER_MODULE (base);
	gkm_user_storage_refresh (self->storage);
	return CKR_OK;
}

static void
gkm_user_module_real_add_token_object (GkmModule *base, GkmTransaction *transaction, GkmObject *object)
{
	GkmUserModule *self = GKM_USER_MODULE (base);
	gkm_user_storage_create (self->storage, transaction, object);
}

static void
gkm_user_module_real_store_token_object (GkmModule *base, GkmTransaction *transaction, GkmObject *object)
{
	/* Not necessary */
}

static void
gkm_user_module_real_remove_token_object (GkmModule *base, GkmTransaction *transaction, GkmObject *object)
{
	GkmUserModule *self = GKM_USER_MODULE (base);
	gkm_user_storage_destroy (self->storage, transaction, object);
}

static CK_RV
gkm_user_module_real_login_change (GkmModule *base, CK_SLOT_ID slot_id, CK_UTF8CHAR_PTR old_pin,
                                   CK_ULONG n_old_pin, CK_UTF8CHAR_PTR new_pin, CK_ULONG n_new_pin)
{
	GkmUserModule *self = GKM_USER_MODULE (base);
	GkmSecret *old_login, *new_login;
	GkmTransaction *transaction;
	CK_RV rv;

	/*
	 * Remember this doesn't affect the currently logged in user. Logged in
	 * sessions will remain logged in, and vice versa.
	 */

	old_login = gkm_secret_new_from_login (old_pin, n_old_pin);
	new_login = gkm_secret_new_from_login (new_pin, n_new_pin);

	transaction = gkm_transaction_new ();

	gkm_user_storage_relock (self->storage, transaction, old_login, new_login);

	g_object_unref (old_login);
	g_object_unref (new_login);

	gkm_transaction_complete (transaction);
	rv = gkm_transaction_get_result (transaction);
	g_object_unref (transaction);

	return rv;
}

static CK_RV
gkm_user_module_real_login_user (GkmModule *base, CK_SLOT_ID slot_id, CK_UTF8CHAR_PTR pin, CK_ULONG n_pin)
{
	GkmUserModule *self = GKM_USER_MODULE (base);
	GkmSecret *login;
	CK_RV rv;

	/* See if this application has logged in */
	if (g_hash_table_lookup (self->unlocked_apps, &slot_id))
		return CKR_USER_ALREADY_LOGGED_IN;

	login = gkm_user_storage_get_login (self->storage);

	/* No application is logged in */
	if (g_hash_table_size (self->unlocked_apps) == 0) {

		g_return_val_if_fail (login == NULL, CKR_GENERAL_ERROR);

		/* So actually unlock the store */
		login = gkm_secret_new_from_login (pin, n_pin);
		rv = gkm_user_storage_unlock (self->storage, login);
		g_object_unref (login);

	/* An application is already logged in */
	} else {

		g_return_val_if_fail (login != NULL, CKR_GENERAL_ERROR);

		/* Compare our pin to the one used originally */
		if (!gkm_secret_equals (login, pin, n_pin))
			rv = CKR_PIN_INCORRECT;
		else
			rv = CKR_OK;
	}

	/* Note that this application logged in */
	if (rv == CKR_OK) {
		g_hash_table_insert (self->unlocked_apps, gkm_util_ulong_alloc (slot_id), UNUSED_VALUE);
		rv = GKM_MODULE_CLASS (gkm_user_module_parent_class)->login_user (base, slot_id, pin, n_pin);
	}

	return rv;
}

static CK_RV
gkm_user_module_real_login_so (GkmModule *base, CK_SLOT_ID slot_id, CK_UTF8CHAR_PTR pin, CK_ULONG n_pin)
{
	GkmUserModule *self = GKM_USER_MODULE (base);

	/* See if this application has unlocked, in which case we can't login */
	if (g_hash_table_lookup (self->unlocked_apps, &slot_id))
		return CKR_USER_ALREADY_LOGGED_IN;

	/* Note that for an SO login, we don't actually unlock, and pin is always blank */
	if (n_pin != 0)
		return CKR_PIN_INCORRECT;

	return GKM_MODULE_CLASS (gkm_user_module_parent_class)->login_so (base, slot_id, pin, n_pin);
}

static CK_RV
gkm_user_module_real_logout_user (GkmModule *base, CK_SLOT_ID slot_id)
{
	GkmUserModule *self = GKM_USER_MODULE (base);
	CK_RV rv;

	if (!g_hash_table_remove (self->unlocked_apps, &slot_id))
		return CKR_USER_NOT_LOGGED_IN;

	if (g_hash_table_size (self->unlocked_apps) > 0)
		return CKR_OK;

	rv = gkm_user_storage_lock (self->storage);
	if (rv == CKR_OK)
		rv = GKM_MODULE_CLASS (gkm_user_module_parent_class)->logout_user (base, slot_id);

	return rv;
}

static GObject*
gkm_user_module_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmUserModule *self = GKM_USER_MODULE (G_OBJECT_CLASS (gkm_user_module_parent_class)->constructor(type, n_props, props));
	g_return_val_if_fail (self, NULL);

	if (!self->directory)
		self->directory = g_build_filename (g_get_home_dir (), ".gnome2", "keyrings", NULL);
	self->storage = gkm_user_storage_new (GKM_MODULE (self), self->directory);

	return G_OBJECT (self);
}

static void
gkm_user_module_init (GkmUserModule *self)
{
	self->unlocked_apps = g_hash_table_new_full (gkm_util_ulong_hash, gkm_util_ulong_equal, gkm_util_ulong_free, NULL);

	/* Our default token info, updated as module runs */
	memcpy (&self->token_info, &user_module_token_info, sizeof (CK_TOKEN_INFO));

	/* For creating stored keys */
	gkm_module_register_factory (GKM_MODULE (self), GKM_FACTORY_USER_PRIVATE_KEY);
	gkm_module_register_factory (GKM_MODULE (self), GKM_FACTORY_USER_PUBLIC_KEY);
}

static void
gkm_user_module_dispose (GObject *obj)
{
	GkmUserModule *self = GKM_USER_MODULE (obj);

	if (self->storage)
		g_object_unref (self->storage);
	self->storage = NULL;

	g_hash_table_remove_all (self->unlocked_apps);

	G_OBJECT_CLASS (gkm_user_module_parent_class)->dispose (obj);
}

static void
gkm_user_module_finalize (GObject *obj)
{
	GkmUserModule *self = GKM_USER_MODULE (obj);

	g_assert (self->storage == NULL);

	g_assert (self->unlocked_apps);
	g_hash_table_destroy (self->unlocked_apps);
	self->unlocked_apps = NULL;

	g_free (self->directory);
	self->directory = NULL;

	G_OBJECT_CLASS (gkm_user_module_parent_class)->finalize (obj);
}

static void
gkm_user_module_class_init (GkmUserModuleClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
	GkmModuleClass *module_class = GKM_MODULE_CLASS (klass);

	gobject_class->constructor = gkm_user_module_constructor;
	gobject_class->dispose = gkm_user_module_dispose;
	gobject_class->finalize = gkm_user_module_finalize;

	module_class->get_slot_info = gkm_user_module_real_get_slot_info;
	module_class->get_token_info = gkm_user_module_real_get_token_info;
	module_class->parse_argument = gkm_user_module_real_parse_argument;
	module_class->refresh_token = gkm_user_module_real_refresh_token;
	module_class->add_token_object = gkm_user_module_real_add_token_object;
	module_class->store_token_object = gkm_user_module_real_store_token_object;
	module_class->remove_token_object = gkm_user_module_real_remove_token_object;
	module_class->login_user = gkm_user_module_real_login_user;
	module_class->login_so = gkm_user_module_real_login_so;
	module_class->logout_user = gkm_user_module_real_logout_user;
	module_class->login_change = gkm_user_module_real_login_change;
}

/* ----------------------------------------------------------------------------
 * PUBLIC
 */

CK_FUNCTION_LIST_PTR
gkm_user_store_get_functions (void)
{
	gkm_crypto_initialize ();
	return gkm_user_module_function_list;
}
