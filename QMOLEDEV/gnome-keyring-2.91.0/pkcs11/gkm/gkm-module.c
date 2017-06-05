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

#include "pkcs11/pkcs11.h"
#include "pkcs11/pkcs11g.h"
#include "pkcs11/pkcs11i.h"

#include "gkm-aes-key.h"
#include "gkm-aes-mechanism.h"
#include "gkm-attributes.h"
#include "gkm-certificate.h"
#include "gkm-credential.h"
#include "gkm-factory.h"
#include "gkm-manager.h"
#include "gkm-memory-store.h"
#include "gkm-module.h"
#include "gkm-null-key.h"
#include "gkm-null-mechanism.h"
#include "gkm-dh-private-key.h"
#include "gkm-private-xsa-key.h"
#include "gkm-dh-public-key.h"
#include "gkm-public-xsa-key.h"
#include "gkm-session.h"
#include "gkm-store.h"
#include "gkm-timer.h"
#include "gkm-transaction.h"
#include "gkm-util.h"

enum {
	PROP_0,
	PROP_MANAGER,
	PROP_WRITE_PROTECTED,
	PROP_INITIALIZE_ARGS,
	PROP_MUTEX
};

#define APARTMENT_APP(apt) \
	((apt) & ~CK_GNOME_MAX_SLOT)
#define APARTMENT_SLOT(apt) \
	((apt) & CK_GNOME_MAX_SLOT)
#define APARTMENT_ID(slot, app) \
	(((slot) & CK_GNOME_MAX_SLOT) | ((app) & ~CK_GNOME_MAX_SLOT))

struct _GkmModulePrivate {
	GMutex *mutex;                          /* The mutex controlling entry to this module */

	GkmManager *token_manager;
	GHashTable *apartments_by_id;           /* Apartment (slot + application) by their id */
	GHashTable *sessions_by_handle;         /* Mapping of handle to all open sessions */
	gulong handle_counter;                  /* Constantly incrementing counter for handles and the like */
	GArray *factories;                      /* Various registered object factories */
	gboolean factories_sorted;              /* Whether we need to sort the object factories */

	GHashTable *transient_objects;          /* Token objects that are not stored permanently. */
	GkmStore *transient_store;              /* Store for trantsient objects. */
};

typedef struct _Apartment {
	CK_ULONG apt_id;
	CK_SLOT_ID slot_id;
	CK_G_APPLICATION_ID app_id;
	CK_G_APPLICATION_PTR app_ptr;
	GkmManager *session_manager;
	GList *sessions;
	CK_USER_TYPE logged_in;
} Apartment;

/* Our slot identifier is 1 */
#define GKM_SLOT_ID  1

G_DEFINE_TYPE (GkmModule, gkm_module, G_TYPE_OBJECT);

/* These info blocks are used unless derived class overrides */

static const CK_INFO default_module_info = {
	{ CRYPTOKI_VERSION_MAJOR, CRYPTOKI_VERSION_MINOR },
	"Gnome Keyring",
	CKF_G_APPLICATIONS,
	"Gnome Keyring Module",
	{ 1, 1 },
};

static const CK_SLOT_INFO default_slot_info = {
	"Unnamed Slot",
	"Gnome Keyring",
	CKF_TOKEN_PRESENT,
	{ 0, 0 },
	{ 0, 0 }
};

static const CK_TOKEN_INFO default_token_info = {
	"Unnamed Token",
	"Gnome Keyring",
	"1.0",
	"1",
	CKF_TOKEN_INITIALIZED | CKF_WRITE_PROTECTED,
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

typedef struct _MechanismAndInfo {
	CK_MECHANISM_TYPE mechanism;
	CK_MECHANISM_INFO info;
} MechanismAndInfo;

static const MechanismAndInfo mechanism_list[] = {
	/*
	 * CKM_RSA_PKCS
	 * For RSA, min and max are the minimum and maximum modulus in bits
	 */
	{ CKM_RSA_PKCS, { 256, 32768, CKF_ENCRYPT | CKF_DECRYPT | CKF_SIGN | CKF_VERIFY } },

	/*
	 * CKM_RSA_X509
	 * For RSA, min and max are the minimum and maximum modulus in bits
	 */
	{ CKM_RSA_X_509, { 256, 32768, CKF_ENCRYPT | CKF_DECRYPT | CKF_SIGN | CKF_VERIFY } },

	/*
	 * CKM_DSA
	 * For DSA, min and max are the minimum and maximum modulus in bits
	 */
	{ CKM_DSA, { 512, 1024, CKF_SIGN | CKF_VERIFY } },

	/*
	 * CKM_DH_PKCS_KEY_PAIR_GEN
	 * For DH derivation the min and max are sizes of prime in bits.
	 */
	{ CKM_DH_PKCS_KEY_PAIR_GEN, { 768, 8192, CKF_GENERATE_KEY_PAIR } },

	/*
	 * CKM_DH_PKCS_DERIVE
	 * For DH derivation the min and max are sizes of prime in bits.
	 */
	{ CKM_DH_PKCS_DERIVE, { 768, 8192, CKF_DERIVE } },

	/*
	 * CKM_AES_CBC_PAD
	 * For AES the min and max are sizes of key in bytes.
	 */
	{ CKM_AES_CBC_PAD, { GKM_AES_MECHANISM_MIN_LENGTH, GKM_AES_MECHANISM_MAX_LENGTH, CKF_WRAP | CKF_UNWRAP } },

	/*
	 * CKM_G_NULL
	 * For NULL min and max are zero
	 */
	{ CKM_G_NULL, { GKM_NULL_MECHANISM_MIN_LENGTH, GKM_NULL_MECHANISM_MAX_LENGTH, CKF_WRAP | CKF_UNWRAP } },
};

/* Hidden function that you should not use */
GMutex* _gkm_module_get_scary_mutex_that_you_should_not_touch (GkmModule *self);

static void  remove_transient_object (GkmModule *self, GkmTransaction *transaction, GkmObject *object);

static void  add_transient_object    (GkmModule *self, GkmTransaction *transaction, GkmObject *object);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static gint
sort_factory_by_n_attrs (gconstpointer a, gconstpointer b)
{
	const GkmFactory *fa = a;
	const GkmFactory *fb = b;

	g_assert (a);
	g_assert (b);

	/* Note we're sorting in reverse order */
	if (fa->n_attrs < fb->n_attrs)
		return 1;
	return (fa->n_attrs == fb->n_attrs) ? 0 : -1;
}

static void
extend_space_string (CK_UTF8CHAR_PTR string, gsize length)
{
	CK_UTF8CHAR_PTR at;

	/* Find a null pointer in the string */
	at = memchr (string, 0, length);
	g_assert (at != NULL && at < string + length);
	for (; at < string + length; ++at)
		*at = ' ';
}

static void
apartment_free (gpointer data)
{
	Apartment *apt;
	GList *l;

	g_assert (data != NULL);
	apt = (Apartment*)data;

	g_return_if_fail (GKM_IS_MANAGER (apt->session_manager));

	/* Unreference all the sessions */
	for (l = apt->sessions; l; l = g_list_next (l)) {

		/* Some sanity checks to make sure things have remained as expected */
		g_return_if_fail (GKM_IS_SESSION (l->data));
		g_return_if_fail (gkm_session_get_apartment (l->data) == apt->apt_id);
		g_return_if_fail (gkm_session_get_manager (l->data) == apt->session_manager);
		g_return_if_fail (gkm_session_get_logged_in (l->data) == apt->logged_in);

		g_object_unref (l->data);
	}

	g_list_free (apt->sessions);
	g_object_unref (apt->session_manager);

	g_slice_free (Apartment, apt);
}

static Apartment*
apartment_new (GkmModuleClass *klass, CK_SLOT_ID slot_id, CK_G_APPLICATION_PTR app)
{
	Apartment *apt;

	apt = g_slice_new0 (Apartment);
	apt->session_manager = g_object_new (GKM_TYPE_MANAGER, "for-token", FALSE, NULL);
	apt->logged_in = CKU_NONE;
	apt->sessions = NULL;
	apt->slot_id = slot_id;

	if (app) {
		if (!app->applicationId)
			app->applicationId = gkm_util_next_handle () << 8;
		apt->app_id = app->applicationId;
		apt->app_ptr = app;
	} else {
		apt->app_id = 0;
		apt->app_ptr = NULL;
	}

	apt->apt_id = APARTMENT_ID (apt->slot_id, apt->app_id);

	return apt;
}

static Apartment*
lookup_apartment (GkmModule *self, CK_ULONG apartment)
{
	g_assert (GKM_IS_MODULE (self));
	return g_hash_table_lookup (self->pv->apartments_by_id, &apartment);
}

static void
register_apartment (GkmModule *self, Apartment *apt)
{
	g_assert (apt);
	g_assert (GKM_IS_MODULE (self));
	g_assert (!g_hash_table_lookup (self->pv->apartments_by_id, &(apt->apt_id)));

	g_hash_table_insert (self->pv->apartments_by_id,
	                     gkm_util_ulong_alloc (apt->apt_id), apt);
}

static void
unregister_apartment (GkmModule *self, Apartment *apt)
{
	g_assert (apt);
	g_assert (GKM_IS_MODULE (self));

	if (!g_hash_table_remove (self->pv->apartments_by_id, &(apt->apt_id)))
		g_assert_not_reached ();
}

static void
mark_login_apartment (GkmModule *self, Apartment *apt, CK_USER_TYPE user)
{
	GList *l;

	g_assert (apt);
	g_assert (GKM_IS_MODULE (self));

	/* Mark all sessions in the partition as logged in */
	for (l = apt->sessions; l; l = g_list_next (l))
		gkm_session_set_logged_in (l->data, user);
	apt->logged_in = user;
}

static void
parse_argument (GkmModule *self, char *arg)
{
	gchar *value;

	g_assert (GKM_IS_MODULE (self));

	value = arg + strcspn (arg, ":=");
	if (!*value)
		value = NULL;
	else
		*(value++) = 0;

	g_strstrip (arg);
	if (value)
		g_strstrip (value);

	g_return_if_fail (GKM_MODULE_GET_CLASS (self)->parse_argument);
	GKM_MODULE_GET_CLASS (self)->parse_argument (self, arg, value);
}

static void
parse_arguments (GkmModule *self, const gchar *string)
{
	gchar quote = '\0';
	gchar *src, *dup, *at, *arg;

	g_assert (GKM_IS_MODULE (self));

	if (!string)
		return;

	src = dup = g_strdup (string);

	arg = at = src;
	for (src = dup; *src; src++) {

		/* Matching quote */
		if (quote == *src) {
			quote = '\0';

		/* Inside of quotes */
		} else if (quote != '\0') {
			if (*src == '\\') {
				*at++ = *src++;
				if (!*src) {
					g_warning ("couldn't parse module argument string");
					goto done;
				}
				if (*src != quote)
					*at++ = '\\';
			}
			*at++ = *src;

		/* Space, not inside of quotes */
		} else if (g_ascii_isspace(*src)) {
			*at = 0;
			parse_argument (self, arg);
			arg = at;

		/* Other character outside of quotes */
		} else {
			switch (*src) {
			case '\'':
			case '"':
				quote = *src;
				break;
			case '\\':
				*at++ = *src++;
				if (!*src) {
					g_warning ("couldn't parse module argument string");
					goto done;
				}
				/* fall through */
			default:
				*at++ = *src;
				break;
			}
		}
	}


	if (at != arg) {
		*at = 0;
		parse_argument (self, arg);
	}

done:
	g_free (dup);
}


static gboolean
complete_transient_remove (GkmTransaction *transaction, GkmModule *self, GkmObject *object)
{
	if (gkm_transaction_get_failed (transaction))
		add_transient_object (self, NULL, object);
	g_object_unref (object);
	return TRUE;
}

static void
remove_transient_object (GkmModule *self, GkmTransaction *transaction, GkmObject *object)
{
	g_assert (GKM_IS_MODULE (self));
	g_assert (GKM_IS_OBJECT (object));

	g_object_ref (object);

	gkm_object_expose (object, FALSE);
	if (!g_hash_table_remove (self->pv->transient_objects, object))
		g_return_if_reached ();
	g_object_set (object, "store", NULL, NULL);

	if (transaction) {
		gkm_transaction_add (transaction, self,
		                     (GkmTransactionFunc)complete_transient_remove,
		                     g_object_ref (object));
	}

	g_object_unref (object);
}

static gboolean
complete_transient_add (GkmTransaction *transaction, GkmModule *self, GkmObject *object)
{
	if (gkm_transaction_get_failed (transaction))
		remove_transient_object (self, NULL, object);
	g_object_unref (object);
	return TRUE;
}

static void
add_transient_object (GkmModule *self, GkmTransaction *transaction, GkmObject *object)
{
	g_assert (GKM_IS_MODULE (self));
	g_assert (GKM_IS_OBJECT (object));

	/* Must not already be associated with a session or manager */
	g_return_if_fail (gkm_object_get_manager (object) == self->pv->token_manager);
	g_return_if_fail (g_hash_table_lookup (self->pv->transient_objects, object) == NULL);

	g_hash_table_insert (self->pv->transient_objects, object, g_object_ref (object));
	g_object_set (object, "store", self->pv->transient_store, NULL);
	gkm_object_expose (object, TRUE);

	if (transaction) {
		gkm_transaction_add (transaction, self,
		                     (GkmTransactionFunc)complete_transient_add,
		                     g_object_ref (object));
	}
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static const CK_SLOT_INFO*
gkm_module_real_get_slot_info (GkmModule *self)
{
	return &default_slot_info;
}

static const CK_TOKEN_INFO*
gkm_module_real_get_token_info (GkmModule *self)
{
	return &default_token_info;
}

static void
gkm_module_real_parse_argument (GkmModule *self, const gchar *name, const gchar *value)
{
	/* Derived classes should do something interesting */
}

static CK_RV
gkm_module_real_refresh_token (GkmModule *self)
{
	/* Derived classes should do something interesting */
	return CKR_OK;
}

static void
gkm_module_real_add_token_object (GkmModule *self, GkmTransaction *transaction, GkmObject *object)
{
	/* Derived class should override, default does nothing */
}

static void
gkm_module_real_store_token_object (GkmModule *self, GkmTransaction *transaction, GkmObject *object)
{
	/* Derived classes should do something interesting */
	gkm_transaction_fail (transaction, CKR_FUNCTION_NOT_SUPPORTED);
}

static void
gkm_module_real_remove_token_object (GkmModule *self, GkmTransaction *transaction, GkmObject *object)
{
	/* Derived classes should do something interesting */
	gkm_transaction_fail (transaction, CKR_FUNCTION_NOT_SUPPORTED);
}

static CK_RV
gkm_module_real_login_change (GkmModule *self, CK_SLOT_ID slot_id, CK_UTF8CHAR_PTR old_pin,
                              CK_ULONG n_old_pin, CK_UTF8CHAR_PTR new_pin, CK_ULONG n_new_pin)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

static CK_RV
gkm_module_real_login_user (GkmModule *self, CK_ULONG apartment, CK_UTF8CHAR_PTR pin, CK_ULONG n_pin)
{
	Apartment *apt;

	apt = lookup_apartment (self, apartment);
	g_return_val_if_fail (apt, CKR_GENERAL_ERROR);

	mark_login_apartment (self, apt, CKU_USER);
	return CKR_OK;
}

static CK_RV
gkm_module_real_login_so (GkmModule *self, CK_ULONG apartment, CK_UTF8CHAR_PTR pin, CK_ULONG n_pin)
{
	Apartment *apt;

	apt = lookup_apartment (self, apartment);
	g_return_val_if_fail (apt, CKR_GENERAL_ERROR);

	mark_login_apartment (self, apt, CKU_SO);
	return CKR_OK;
}

static CK_RV
gkm_module_real_logout_any (GkmModule *self, CK_ULONG apartment)
{
	Apartment *apt;

	/* Calculate the partition identifier */
	apt = lookup_apartment (self, apartment);
	g_return_val_if_fail (apt, CKR_GENERAL_ERROR);

	mark_login_apartment (self, apt, CKU_NONE);
	return CKR_OK;
}

static GObject*
gkm_module_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmModule *self = GKM_MODULE (G_OBJECT_CLASS (gkm_module_parent_class)->constructor(type, n_props, props));
	CK_ATTRIBUTE attr;

	g_return_val_if_fail (self, NULL);

	/* Register store attributes */
	attr.type = CKA_LABEL;
	attr.pValue = "";
	attr.ulValueLen = 0;
	gkm_store_register_schema (self->pv->transient_store, &attr, NULL, 0);

	return G_OBJECT (self);
}

static void
gkm_module_init (GkmModule *self)
{
	gkm_timer_initialize ();

	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_MODULE, GkmModulePrivate);
	self->pv->token_manager = g_object_new (GKM_TYPE_MANAGER, "for-token", TRUE, NULL);
	self->pv->sessions_by_handle = g_hash_table_new_full (gkm_util_ulong_hash, gkm_util_ulong_equal,
	                                                      gkm_util_ulong_free, g_object_unref);
	self->pv->apartments_by_id = g_hash_table_new_full (gkm_util_ulong_hash, gkm_util_ulong_equal,
	                                                    gkm_util_ulong_free, apartment_free);
	self->pv->factories = g_array_new (FALSE, TRUE, sizeof (GkmFactory));

	self->pv->handle_counter = 1;

	/* Create the store for transient objects */
	self->pv->transient_store = GKM_STORE (gkm_memory_store_new ());
	self->pv->transient_objects = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL, gkm_util_dispose_unref);

	/* Register session object factories */
	gkm_module_register_factory (self, GKM_FACTORY_AES_KEY);
	gkm_module_register_factory (self, GKM_FACTORY_CERTIFICATE);
	gkm_module_register_factory (self, GKM_FACTORY_CREDENTIAL);
	gkm_module_register_factory (self, GKM_FACTORY_NULL_KEY);
	gkm_module_register_factory (self, GKM_FACTORY_DH_PRIVATE_KEY);
	gkm_module_register_factory (self, GKM_FACTORY_PRIVATE_XSA_KEY);
	gkm_module_register_factory (self, GKM_FACTORY_DH_PUBLIC_KEY);
	gkm_module_register_factory (self, GKM_FACTORY_PUBLIC_XSA_KEY);
}

static void
gkm_module_dispose (GObject *obj)
{
	GkmModule *self = GKM_MODULE (obj);

	g_hash_table_remove_all (self->pv->transient_objects);
	g_hash_table_remove_all (self->pv->sessions_by_handle);
	g_hash_table_remove_all (self->pv->apartments_by_id);

	if (self->pv->token_manager)
		g_object_unref (self->pv->token_manager);
	self->pv->token_manager = NULL;

	g_array_set_size (self->pv->factories, 0);

	G_OBJECT_CLASS (gkm_module_parent_class)->dispose (obj);
}

static void
gkm_module_finalize (GObject *obj)
{
	GkmModule *self = GKM_MODULE (obj);

	g_hash_table_destroy (self->pv->transient_objects);
	self->pv->transient_objects = NULL;

	g_object_unref (self->pv->transient_store);
	self->pv->transient_store = NULL;

	g_assert (self->pv->token_manager == NULL);

	g_assert (g_hash_table_size (self->pv->apartments_by_id) == 0);
	g_hash_table_destroy (self->pv->apartments_by_id);
	self->pv->apartments_by_id = NULL;

	g_assert (g_hash_table_size (self->pv->sessions_by_handle) == 0);
	g_hash_table_destroy (self->pv->sessions_by_handle);
	self->pv->sessions_by_handle = NULL;

	g_array_free (self->pv->factories, TRUE);
	self->pv->factories = NULL;

	gkm_timer_shutdown ();

	G_OBJECT_CLASS (gkm_module_parent_class)->finalize (obj);
}

static void
gkm_module_set_property (GObject *obj, guint prop_id, const GValue *value,
                         GParamSpec *pspec)
{
	GkmModule *self = GKM_MODULE (obj);
	CK_C_INITIALIZE_ARGS_PTR args;

	switch (prop_id) {
	case PROP_INITIALIZE_ARGS:
		args = g_value_get_pointer (value);
		if (args != NULL && args->pReserved != NULL)
			parse_arguments (self, args->pReserved);
		break;
	case PROP_MUTEX:
		self->pv->mutex = g_value_get_pointer (value);
		g_return_if_fail (self->pv->mutex);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_module_get_property (GObject *obj, guint prop_id, GValue *value,
                         GParamSpec *pspec)
{
	GkmModule *self = GKM_MODULE (obj);

	switch (prop_id) {
	case PROP_MANAGER:
		g_value_set_object (value, gkm_module_get_manager (self));
		break;
	case PROP_WRITE_PROTECTED:
		g_value_set_boolean (value, gkm_module_get_write_protected (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_module_class_init (GkmModuleClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gkm_module_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GkmModulePrivate));

	gobject_class->constructor = gkm_module_constructor;
	gobject_class->dispose = gkm_module_dispose;
	gobject_class->finalize = gkm_module_finalize;
	gobject_class->set_property = gkm_module_set_property;
	gobject_class->get_property = gkm_module_get_property;

	klass->get_slot_info = gkm_module_real_get_slot_info;
	klass->get_token_info = gkm_module_real_get_token_info;
	klass->parse_argument = gkm_module_real_parse_argument;
	klass->refresh_token = gkm_module_real_refresh_token;
	klass->add_token_object = gkm_module_real_add_token_object;
	klass->store_token_object = gkm_module_real_store_token_object;
	klass->remove_token_object = gkm_module_real_remove_token_object;
	klass->login_change = gkm_module_real_login_change;
	klass->login_user = gkm_module_real_login_user;
	klass->logout_user = gkm_module_real_logout_any;
	klass->login_so = gkm_module_real_login_so;
	klass->logout_so = gkm_module_real_logout_any;

	g_object_class_install_property (gobject_class, PROP_MANAGER,
	           g_param_spec_object ("manager", "Manager", "Token object manager",
	                                GKM_TYPE_MANAGER, G_PARAM_READABLE));

	g_object_class_install_property (gobject_class, PROP_WRITE_PROTECTED,
	           g_param_spec_boolean ("write-protected", "Write Protected", "Token is write protected",
	                                 TRUE, G_PARAM_READABLE));

	g_object_class_install_property (gobject_class, PROP_INITIALIZE_ARGS,
	           g_param_spec_pointer ("initialize-args", "Initialize Args", "Arguments passed to C_Initialize",
	                                 G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_MUTEX,
	           g_param_spec_pointer ("mutex", "Mutex", "Module mutex",
	                                 G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY));
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmManager*
gkm_module_get_manager (GkmModule *self)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), NULL);
	g_return_val_if_fail (GKM_IS_MANAGER (self->pv->token_manager), NULL);
	return self->pv->token_manager;
}

gboolean
gkm_module_get_write_protected (GkmModule *self)
{
	const CK_TOKEN_INFO* info;

	g_return_val_if_fail (GKM_IS_MODULE (self), TRUE);
	g_return_val_if_fail (GKM_MODULE_GET_CLASS (self)->get_token_info, TRUE);

	info = (GKM_MODULE_GET_CLASS (self)->get_token_info) (self);
	g_return_val_if_fail (info, TRUE);

	return info->flags & CKF_WRITE_PROTECTED;
}

GkmSession*
gkm_module_lookup_session (GkmModule *self, CK_SESSION_HANDLE handle)
{
	GkmSession *session;

	g_return_val_if_fail (GKM_IS_MODULE (self), NULL);

	session = g_hash_table_lookup (self->pv->sessions_by_handle, &handle);
	if (!session)
		return NULL;

	g_return_val_if_fail (GKM_IS_SESSION (session), NULL);
	return session;
}

CK_RV
gkm_module_login_change (GkmModule *self, CK_SLOT_ID slot_id, CK_UTF8CHAR_PTR old_pin,
                         CK_ULONG n_old_pin, CK_UTF8CHAR_PTR new_pin, CK_ULONG n_new_pin)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_GENERAL_ERROR);
	g_assert (GKM_MODULE_GET_CLASS (self)->login_change);
	return GKM_MODULE_GET_CLASS (self)->login_change (self, slot_id, old_pin, n_old_pin, new_pin, n_new_pin);
}

CK_RV
gkm_module_login_user (GkmModule *self, CK_SLOT_ID slot_id, CK_UTF8CHAR_PTR pin, CK_ULONG n_pin)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_GENERAL_ERROR);
	g_assert (GKM_MODULE_GET_CLASS (self)->login_user);
	return GKM_MODULE_GET_CLASS (self)->login_user (self, slot_id, pin, n_pin);
}

CK_RV
gkm_module_logout_user (GkmModule *self, CK_SLOT_ID slot_id)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_GENERAL_ERROR);
	g_assert (GKM_MODULE_GET_CLASS (self)->logout_user);
	return GKM_MODULE_GET_CLASS (self)->logout_user (self, slot_id);
}

CK_RV
gkm_module_login_so (GkmModule *self, CK_SLOT_ID slot_id, CK_UTF8CHAR_PTR pin, CK_ULONG n_pin)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_GENERAL_ERROR);
	g_assert (GKM_MODULE_GET_CLASS (self)->login_so);
	return GKM_MODULE_GET_CLASS (self)->login_so (self, slot_id, pin, n_pin);
}

CK_RV
gkm_module_logout_so (GkmModule *self, CK_SLOT_ID slot_id)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_GENERAL_ERROR);
	g_assert (GKM_MODULE_GET_CLASS (self)->logout_so);
	return GKM_MODULE_GET_CLASS (self)->logout_so (self, slot_id);
}

CK_ULONG
gkm_module_next_handle (GkmModule *self)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), 0);
	if (self->pv->handle_counter == CK_GNOME_MAX_HANDLE) {
		g_warning ("handle counter wrapped");
		self->pv->handle_counter = 0;
	}
	return (self->pv->handle_counter)++;
}

CK_RV
gkm_module_refresh_token (GkmModule *self)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_GENERAL_ERROR);
	g_assert (GKM_MODULE_GET_CLASS (self)->refresh_token);
	return GKM_MODULE_GET_CLASS (self)->refresh_token (self);
}

void
gkm_module_add_token_object (GkmModule *self, GkmTransaction *transaction, GkmObject *object)
{
	g_return_if_fail (GKM_IS_MODULE (self));
	g_return_if_fail (GKM_IS_OBJECT (object));
	g_assert (GKM_MODULE_GET_CLASS (self)->add_token_object);

	if (gkm_object_is_transient (object)) {
		if (g_hash_table_lookup (self->pv->transient_objects, object) == NULL)
			add_transient_object (self, transaction, object);
	} else {
		GKM_MODULE_GET_CLASS (self)->add_token_object (self, transaction, object);
	}
}

void
gkm_module_store_token_object (GkmModule *self, GkmTransaction *transaction, GkmObject *object)
{
	g_return_if_fail (GKM_IS_MODULE (self));
	g_return_if_fail (GKM_IS_OBJECT (object));
	g_assert (GKM_MODULE_GET_CLASS (self)->store_token_object);

	if (!gkm_object_is_transient (object))
		GKM_MODULE_GET_CLASS (self)->store_token_object (self, transaction, object);
}

void
gkm_module_remove_token_object (GkmModule *self, GkmTransaction *transaction, GkmObject *object)
{
	g_return_if_fail (GKM_IS_MODULE (self));
	g_return_if_fail (GKM_IS_OBJECT (object));
	g_assert (GKM_MODULE_GET_CLASS (self)->remove_token_object);

	if (gkm_object_is_transient (object))
		remove_transient_object (self, transaction, object);
	else
		GKM_MODULE_GET_CLASS (self)->remove_token_object (self, transaction, object);
}

void
gkm_module_register_factory (GkmModule *self, GkmFactory *factory)
{
	g_return_if_fail (GKM_IS_MODULE (self));
	g_return_if_fail (factory);
	g_return_if_fail (factory->attrs || !factory->n_attrs);
	g_return_if_fail (factory->func);

	g_array_append_val (self->pv->factories, *factory);
	self->pv->factories_sorted = FALSE;
}

GkmFactory*
gkm_module_find_factory (GkmModule *self, CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	GkmFactory *factory;
	gboolean matched;
	gulong j;
	gsize i;

	g_return_val_if_fail (GKM_IS_MODULE (self), NULL);
	g_return_val_if_fail (attrs || !n_attrs, NULL);

	if (!self->pv->factories_sorted) {
		g_array_sort (self->pv->factories, sort_factory_by_n_attrs);
		self->pv->factories_sorted = TRUE;
	}

	for (i = 0; i < self->pv->factories->len; ++i) {
		factory = &(g_array_index (self->pv->factories, GkmFactory, i));

		matched = TRUE;
		for (j = 0; j < factory->n_attrs; ++j) {
			if (!gkm_attributes_contains (attrs, n_attrs, &factory->attrs[j])) {
				matched = FALSE;
				break;
			}
		}

		if (matched)
			return factory;
	}

	return NULL;
}

/*
 * Hidden method to get the mutex for a module. This is for timers to be
 * able to reenter the module. Don't use this method.
 */

GMutex*
_gkm_module_get_scary_mutex_that_you_should_not_touch (GkmModule *self)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), NULL);
	return self->pv->mutex;
}

/* -----------------------------------------------------------------------------
 * PKCS#11
 */

CK_RV
gkm_module_C_GetInfo (GkmModule *self, CK_INFO_PTR info)
{
	GkmModuleClass *klass;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	if (!info)
		return CKR_ARGUMENTS_BAD;

	klass = GKM_MODULE_GET_CLASS (self);
	g_return_val_if_fail (klass, CKR_GENERAL_ERROR);

	memcpy (info, &default_module_info, sizeof (CK_INFO));

	/* Extend all the strings appropriately */
	extend_space_string (info->libraryDescription, sizeof (info->libraryDescription));
	extend_space_string (info->manufacturerID, sizeof (info->manufacturerID));

	return CKR_OK;
}

CK_RV
gkm_module_C_GetSlotList (GkmModule *self, CK_BBOOL token_present, CK_SLOT_ID_PTR slot_list, CK_ULONG_PTR count)
{
	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	if (!count)
		return CKR_ARGUMENTS_BAD;

	/* Just want to get the count */
	if (slot_list == NULL) {
		*count = 1;
		return CKR_OK;
	}

	/* Buffer too small? */
	if (*count == 0) {
		*count = 1;
		return CKR_BUFFER_TOO_SMALL;
	}

	g_return_val_if_fail (slot_list, CKR_ARGUMENTS_BAD);

	/* Answer C_GetSlotList with 0 for app */
	slot_list[0] = GKM_SLOT_ID;
	*count = 1;
	return CKR_OK;
}

CK_RV
gkm_module_C_GetSlotInfo (GkmModule *self, CK_SLOT_ID id, CK_SLOT_INFO_PTR info)
{
	const CK_SLOT_INFO *original;
	GkmModuleClass *klass;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	if (id != GKM_SLOT_ID)
		return CKR_SLOT_ID_INVALID;
	if (info == NULL)
		return CKR_ARGUMENTS_BAD;

	/* Any slot ID is valid for partitioned module */

	klass = GKM_MODULE_GET_CLASS (self);
	g_return_val_if_fail (klass, CKR_GENERAL_ERROR);
	g_return_val_if_fail (klass->get_slot_info, CKR_GENERAL_ERROR);

	original = (klass->get_slot_info) (self);
	g_return_val_if_fail (original, CKR_GENERAL_ERROR);

	memcpy (info, original, sizeof (CK_SLOT_INFO));

	/* Extend all the strings appropriately */
	extend_space_string (info->manufacturerID, sizeof (info->manufacturerID));
	extend_space_string (info->slotDescription, sizeof (info->slotDescription));

	return CKR_OK;
}

CK_RV
gkm_module_C_GetTokenInfo (GkmModule *self, CK_SLOT_ID id, CK_TOKEN_INFO_PTR info)
{
	const CK_TOKEN_INFO *original;
	GkmModuleClass *klass;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	if (id != GKM_SLOT_ID)
		return CKR_SLOT_ID_INVALID;
	if (info == NULL)
		return CKR_ARGUMENTS_BAD;

	/* Any slot ID is valid for partitioned module */

	klass = GKM_MODULE_GET_CLASS (self);
	g_return_val_if_fail (klass, CKR_GENERAL_ERROR);
	g_return_val_if_fail (klass->get_token_info, CKR_GENERAL_ERROR);

	original = (klass->get_token_info) (self);
	g_return_val_if_fail (original, CKR_GENERAL_ERROR);

	memcpy (info, original, sizeof (CK_TOKEN_INFO));

	/* Extend all the strings appropriately */
	extend_space_string (info->label, sizeof (info->label));
	extend_space_string (info->manufacturerID, sizeof (info->manufacturerID));
	extend_space_string (info->model, sizeof (info->model));
	extend_space_string (info->serialNumber, sizeof (info->serialNumber));

	return CKR_OK;
}

CK_RV
gkm_module_C_GetMechanismList (GkmModule *self, CK_SLOT_ID id,
                               CK_MECHANISM_TYPE_PTR mech_list, CK_ULONG_PTR count)
{
	const guint n_mechanisms = G_N_ELEMENTS (mechanism_list);
	guint i;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	if (id != GKM_SLOT_ID)
		return CKR_SLOT_ID_INVALID;
	if (count == NULL)
		return CKR_ARGUMENTS_BAD;

	/* Just want to get the count */
	if (mech_list == NULL) {
		*count = n_mechanisms;
		return CKR_OK;
	}

	/* Buffer too small? */
	if (*count < n_mechanisms) {
		*count = n_mechanisms;
		return CKR_BUFFER_TOO_SMALL;
	}

	*count = n_mechanisms;
	for (i = 0; i < n_mechanisms; ++i)
		mech_list[i] = mechanism_list[i].mechanism;

	return CKR_OK;
}

CK_RV
gkm_module_C_GetMechanismInfo (GkmModule *self, CK_SLOT_ID id,
                               CK_MECHANISM_TYPE type, CK_MECHANISM_INFO_PTR info)
{
	const guint n_mechanisms = G_N_ELEMENTS (mechanism_list);
	guint index;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	if (id != GKM_SLOT_ID)
		return CKR_SLOT_ID_INVALID;
	if (info == NULL)
		return CKR_ARGUMENTS_BAD;

	for (index = 0; index < n_mechanisms; ++index) {
		if (mechanism_list[index].mechanism == type)
			break;
	}

	if (index == n_mechanisms)
		return CKR_MECHANISM_INVALID;

	memcpy (info, &mechanism_list[index].info, sizeof (CK_MECHANISM_INFO));
	return CKR_OK;
}

CK_RV
gkm_module_C_InitToken (GkmModule *self, CK_SLOT_ID id, CK_UTF8CHAR_PTR pin,
                        CK_ULONG pin_len, CK_UTF8CHAR_PTR label)
{
	return CKR_FUNCTION_NOT_SUPPORTED;
}

CK_RV
gkm_module_C_OpenSession (GkmModule *self, CK_SLOT_ID id, CK_FLAGS flags, CK_VOID_PTR user_data,
                          CK_NOTIFY callback, CK_SESSION_HANDLE_PTR result)
{
	CK_G_APPLICATION_PTR app;
	CK_SESSION_HANDLE handle;
	gboolean read_only;
	GkmSession *session;
	Apartment *apt = NULL;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	if (APARTMENT_SLOT (id) != GKM_SLOT_ID)
		return CKR_SLOT_ID_INVALID;
	if (!result)
		return CKR_ARGUMENTS_BAD;

	if (!(flags & CKF_SERIAL_SESSION))
		return CKR_SESSION_PARALLEL_NOT_SUPPORTED;

	/*
	 * If they're calling us with the 'application' extension, then
	 * allocate or use our application identifier.
	 */
	if (flags & CKF_G_APPLICATION_SESSION) {
		app = user_data;
		if (app == NULL)
			return CKR_ARGUMENTS_BAD;
		if (app->applicationId)
			apt = lookup_apartment (self, APARTMENT_ID (id, app->applicationId));
	} else {
		app = NULL;
		apt = lookup_apartment (self, APARTMENT_ID (id, 0));
	}

	/* The first time this application is accessing, or closed all sessions, allocate new */
	if (apt == NULL) {
		apt = apartment_new (GKM_MODULE_GET_CLASS (self), id, app);
		register_apartment (self, apt);
	}

	/* Can't open read only session if SO login */
	if (apt->logged_in == CKU_SO && !(flags & CKF_RW_SESSION))
		return CKR_SESSION_READ_WRITE_SO_EXISTS;

	/* Make and register a new session */
	handle = gkm_module_next_handle (self);
	read_only = !(flags & CKF_RW_SESSION);
	session = g_object_new (GKM_TYPE_SESSION, "slot-id", apt->slot_id, "apartment", apt->apt_id,
	                        "read-only", read_only, "handle", handle, "module", self,
	                        "manager", apt->session_manager, "logged-in", apt->logged_in, NULL);
	apt->sessions = g_list_prepend (apt->sessions, session);

	/* Track the session by handle */
	g_hash_table_insert (self->pv->sessions_by_handle,
	                     gkm_util_ulong_alloc (handle),
	                     g_object_ref (session));

	*result = handle;
	return CKR_OK;
}

CK_RV
gkm_module_C_CloseSession (GkmModule *self, CK_SESSION_HANDLE handle)
{
	GkmSession *session;
	CK_ULONG apt_id;
	Apartment *apt;
	GList *link;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	session = gkm_module_lookup_session (self, handle);
	if (session == NULL)
		return CKR_SESSION_HANDLE_INVALID;

	/* Calculate the virtual slot */
	apt_id = gkm_session_get_apartment (session);
	apt = lookup_apartment (self, apt_id);
	g_return_val_if_fail (apt, CKR_GENERAL_ERROR);

	link = g_list_find (apt->sessions, session);
	g_return_val_if_fail (link, CKR_GENERAL_ERROR);
	apt->sessions = g_list_delete_link (apt->sessions, link);
	g_object_unref (session);
	if (!apt->sessions)
		unregister_apartment (self, apt);

	if (!g_hash_table_remove (self->pv->sessions_by_handle, &handle))
		g_assert_not_reached ();

	return CKR_OK;
}

CK_RV
gkm_module_C_CloseAllSessions (GkmModule *self, CK_SLOT_ID id)
{
	Apartment *apt;
	CK_SESSION_HANDLE handle;
	GList *l;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	if (APARTMENT_SLOT (id) != GKM_SLOT_ID)
		return CKR_SLOT_ID_INVALID;

	apt = lookup_apartment (self, id);
	if (apt == NULL)
		return CKR_OK;

	/* Unregister all its sessions */
	for (l = apt->sessions; l; l = g_list_next (l)) {
		handle = gkm_session_get_handle (l->data);
		if (!g_hash_table_remove (self->pv->sessions_by_handle, &handle))
			g_assert_not_reached ();
	}

	unregister_apartment (self, apt);
	return CKR_OK;
}

CK_RV
gkm_module_C_InitPIN (GkmModule* self, CK_SESSION_HANDLE handle,
                      CK_UTF8CHAR_PTR pin, CK_ULONG n_pin)
{
	GkmSession *session;
	Apartment *apt;
	CK_ULONG apt_id;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	session = gkm_module_lookup_session (self, handle);
	if (session == NULL)
		return CKR_SESSION_HANDLE_INVALID;

	/* Calculate the virtual slot */
	apt_id = gkm_session_get_apartment (session);
	apt = lookup_apartment (self, apt_id);
	g_return_val_if_fail (apt, CKR_GENERAL_ERROR);

	if (apt->logged_in != CKU_SO)
		return CKR_USER_NOT_LOGGED_IN;

	/* Our InitPIN assumes an uninitialized PIN */
	return gkm_module_login_change (self, apt_id, NULL, 0, pin, n_pin);
}

CK_RV
gkm_module_C_SetPIN (GkmModule* self, CK_SESSION_HANDLE handle, CK_UTF8CHAR_PTR old_pin,
                     CK_ULONG old_pin_len, CK_UTF8CHAR_PTR new_pin, CK_ULONG new_pin_len)
{
	GkmSession *session;
	Apartment *apt;
	CK_ULONG apt_id;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	session = gkm_module_lookup_session (self, handle);
	if (session == NULL)
		return CKR_SESSION_HANDLE_INVALID;

	/* Calculate the virtual slot */
	apt_id = gkm_session_get_apartment (session);
	apt = lookup_apartment (self, apt_id);
	g_return_val_if_fail (apt, CKR_GENERAL_ERROR);

	return gkm_module_login_change (self, apt_id, old_pin, old_pin_len, new_pin, new_pin_len);
}

CK_RV
gkm_module_C_Login (GkmModule *self, CK_SESSION_HANDLE handle, CK_USER_TYPE user_type,
                    CK_UTF8CHAR_PTR pin, CK_ULONG pin_len)
{
	CK_ULONG apt_id;
	GkmSession *session;
	Apartment *apt;
	GList *l;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	session = gkm_module_lookup_session (self, handle);
	if (session == NULL)
		return CKR_SESSION_HANDLE_INVALID;

	/* Pass off context specifc logins to appropriate place */
	if (user_type == CKU_CONTEXT_SPECIFIC)
		return gkm_session_login_context_specific (session, pin, pin_len);

	/* Some random crap... */
	if (user_type != CKU_USER && user_type != CKU_SO)
		return CKR_USER_TYPE_INVALID;

	/* Calculate the virtual slot */
	apt_id = gkm_session_get_apartment (session);
	apt = lookup_apartment (self, apt_id);
	g_return_val_if_fail (apt, CKR_GENERAL_ERROR);

	if (apt->logged_in == user_type)
		return CKR_USER_ALREADY_LOGGED_IN;
	if (apt->logged_in != CKU_NONE)
		return CKR_USER_ANOTHER_ALREADY_LOGGED_IN;

	if (user_type == CKU_SO) {

		/* Can't login as SO if read-only sessions exist */
		for (l = apt->sessions; l; l = g_list_next (l)) {
			if (gkm_session_get_read_only (l->data))
				return CKR_SESSION_READ_ONLY_EXISTS;
		}

		return gkm_module_login_so (self, apt_id, pin, pin_len);

	} else if (user_type == CKU_USER) {
		return gkm_module_login_user (self, apt_id, pin, pin_len);

	} else {
		return CKR_USER_TYPE_INVALID;
	}
}

CK_RV
gkm_module_C_Logout (GkmModule *self, CK_SESSION_HANDLE handle)
{
	CK_ULONG apt_id;
	Apartment *apt;
	GkmSession *session;

	g_return_val_if_fail (GKM_IS_MODULE (self), CKR_CRYPTOKI_NOT_INITIALIZED);

	session = gkm_module_lookup_session (self, handle);
	if (session == NULL)
		return CKR_SESSION_HANDLE_INVALID;

	apt_id = gkm_session_get_apartment (session);
	apt = lookup_apartment (self, apt_id);
	g_return_val_if_fail (apt, CKR_GENERAL_ERROR);

	if (apt->logged_in == CKU_NONE)
		return CKR_USER_NOT_LOGGED_IN;

	else if (apt->logged_in == CKU_USER)
		return gkm_module_logout_user (self, apt_id);

	else if (apt->logged_in == CKU_SO)
		return gkm_module_logout_so (self, apt_id);

	else
		g_return_val_if_reached (CKR_GENERAL_ERROR);
}
