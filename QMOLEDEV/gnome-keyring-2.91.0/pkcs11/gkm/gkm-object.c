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

#include "gkm-attributes.h"
#include "gkm-credential.h"
#include "gkm-manager.h"
#include "gkm-object.h"
#include "gkm-transaction.h"
#include "gkm-session.h"
#include "gkm-store.h"
#include "gkm-timer.h"
#include "gkm-util.h"

enum {
	PROP_0,
	PROP_HANDLE,
	PROP_MODULE,
	PROP_MANAGER,
	PROP_STORE,
	PROP_UNIQUE,
	PROP_TRANSIENT
};

enum {
	EXPOSE_OBJECT,
	NOTIFY_ATTRIBUTE,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

typedef struct _GkmObjectTransient {
	GkmTimer *timer;
	gulong timed_after;
	gulong timed_idle;
	glong stamp_used;
	glong stamp_created;
	gulong uses_remaining;
} GkmObjectTransient;

struct _GkmObjectPrivate {
	CK_OBJECT_HANDLE handle;
	GkmModule *module;
	GkmManager *manager;
	GkmStore *store;
	gchar *unique;
	gboolean exposed;
	GkmObjectTransient *transient;
};

G_DEFINE_TYPE (GkmObject, gkm_object, G_TYPE_OBJECT);

/* Private friend functions from the manager */
void  _gkm_manager_register_object   (GkmManager *self, GkmObject *object);
void  _gkm_manager_unregister_object (GkmManager *self, GkmObject *object);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

static void
self_destruct (GkmObject *self)
{
	GkmTransaction *transaction;
	CK_RV rv;

	transaction = gkm_transaction_new ();

	gkm_object_destroy (self, transaction);

	gkm_transaction_complete (transaction);
	rv = gkm_transaction_get_result (transaction);
	g_object_unref (transaction);
	if (rv != CKR_OK)
		g_warning ("Unexpected failure to auto destruct object (code: %lu)", (gulong)rv);
}

static void
timer_callback (GkmTimer *timer, gpointer user_data)
{
	GkmObject *self = user_data;
	glong after, idle, offset;
	GkmObjectTransient *transient;
	GTimeVal tv;

	g_return_if_fail (GKM_IS_OBJECT (self));

	g_object_ref (self);

	g_return_if_fail (self->pv->transient);
	transient = self->pv->transient;
	g_return_if_fail (timer == transient->timer);
	transient->timer = NULL;

	g_get_current_time (&tv);
	idle = after = G_MAXLONG;

	/* Are we supposed to be destroyed after a certain time? */
	if (transient->timed_after) {
		g_return_if_fail (transient->stamp_created);
		after = (transient->stamp_created + transient->timed_after) - tv.tv_sec;
	}

	/* Are we supposed to be destroyed after an idle time? */
	if (transient->timed_idle) {
		g_return_if_fail (transient->stamp_used);
		idle = (transient->stamp_used + transient->timed_idle) - tv.tv_sec;
	}

	/* Okay, time to destroy? */
	offset = MIN (after, idle);
	if (offset <= 0)
		self_destruct (self);

	/* Setup the next timer */
	else
		transient->timer = gkm_timer_start (self->pv->module, offset, timer_callback, self);

	g_object_unref (self);
}

static gboolean
start_callback (GkmTransaction *transaction, GObject *obj, gpointer user_data)
{
	GkmObject *self = GKM_OBJECT (obj);
	GkmObjectTransient *transient;
	GTimeVal tv;

	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (self->pv->transient, FALSE);
	transient = self->pv->transient;
	g_return_val_if_fail (!transient->timer, FALSE);

	g_get_current_time (&tv);
	transient->stamp_created = tv.tv_sec;
	transient->stamp_used = tv.tv_sec;

	/* Start the timer going */
	timer_callback (NULL, self);
	return TRUE;
}

static void
module_went_away (gpointer data, GObject *old_module)
{
	GkmObject *self = GKM_OBJECT (data);
	g_return_if_fail (self->pv->module);
	g_warning ("module destroyed before %s that module contained",
	           G_OBJECT_TYPE_NAME (self));
	self->pv->module = NULL;
}

static gboolean
complete_destroy (GkmTransaction *transaction, GObject *unused, gpointer user_data)
{
	gkm_util_dispose_unref (user_data);
	return TRUE;
}

static gboolean
complete_expose (GkmTransaction *transaction, GObject *obj, gpointer user_data)
{
	GkmObject *self = GKM_OBJECT (obj);
	gboolean expose = GPOINTER_TO_UINT (user_data);

	if (gkm_transaction_get_failed (transaction))
		gkm_object_expose (self, !expose);

	return TRUE;
}

static gboolean
find_credential (GkmCredential *cred, GkmObject *object, gpointer user_data)
{
	CK_OBJECT_HANDLE *result = user_data;
	g_return_val_if_fail (!*result, FALSE);
	*result = gkm_object_get_handle (GKM_OBJECT (cred));
	return TRUE;
}

static void
mark_object_transient (GkmObject *self)
{
	if (!self->pv->transient)
		self->pv->transient = g_slice_new0 (GkmObjectTransient);
}

/* -----------------------------------------------------------------------------
 * OBJECT
 */

static CK_RV
gkm_object_real_get_attribute (GkmObject *self, GkmSession *session, CK_ATTRIBUTE* attr)
{
	CK_OBJECT_HANDLE handle = 0;
	CK_RV rv;

	switch (attr->type)
	{
	case CKA_CLASS:
		g_warning ("Derived class should have overridden CKA_CLASS");
		return CKR_GENERAL_ERROR;
	case CKA_MODIFIABLE:
		return gkm_attribute_set_bool (attr, self->pv->store ? TRUE : FALSE);
	case CKA_PRIVATE:
		return gkm_attribute_set_bool (attr, FALSE);
	case CKA_TOKEN:
		return gkm_attribute_set_bool (attr, gkm_object_is_token (self));
	case CKA_G_CREDENTIAL:
		gkm_credential_for_each (session, GKM_OBJECT (self), find_credential, &handle);
		return gkm_attribute_set_ulong (attr, handle);
	case CKA_GNOME_UNIQUE:
		if (self->pv->unique)
			return gkm_attribute_set_string (attr, self->pv->unique);
		return CKR_ATTRIBUTE_TYPE_INVALID;
	case CKA_GNOME_TRANSIENT:
		return gkm_attribute_set_bool (attr, self->pv->transient ? TRUE : FALSE);
	case CKA_G_DESTRUCT_AFTER:
		return gkm_attribute_set_ulong (attr, self->pv->transient ?
		                                      self->pv->transient->timed_after : 0);
	case CKA_G_DESTRUCT_IDLE:
		return gkm_attribute_set_ulong (attr, self->pv->transient ?
		                                      self->pv->transient->timed_idle : 0);
	case CKA_G_DESTRUCT_USES:
		return gkm_attribute_set_ulong (attr, self->pv->transient ?
		                                      self->pv->transient->uses_remaining : 0);
	};

	/* Give store a shot */
	if (self->pv->store) {
		rv = gkm_store_get_attribute (self->pv->store, self, attr);
		if (rv != CKR_ATTRIBUTE_TYPE_INVALID)
			return rv;
	}

	/* Now some more defaults */
	switch (attr->type) {
	case CKA_LABEL:
		return gkm_attribute_set_data (attr, "", 0);
	}

	return CKR_ATTRIBUTE_TYPE_INVALID;
}

static void
gkm_object_real_set_attribute (GkmObject *self, GkmSession *session,
                               GkmTransaction* transaction, CK_ATTRIBUTE* attr)
{
	CK_ATTRIBUTE check;
	CK_RV rv;

	switch (attr->type) {
	case CKA_TOKEN:
	case CKA_PRIVATE:
	case CKA_MODIFIABLE:
	case CKA_CLASS:
		gkm_transaction_fail (transaction, CKR_ATTRIBUTE_READ_ONLY);
		return;
	case CKA_GNOME_UNIQUE:
		gkm_transaction_fail (transaction, self->pv->unique ?
		                                       CKR_ATTRIBUTE_READ_ONLY :
		                                       CKR_ATTRIBUTE_TYPE_INVALID);
		return;
	};

	/* Give store a shot */
	if (self->pv->store) {
		gkm_store_set_attribute (self->pv->store, transaction, self, attr);
		return;
	}

	/* Now some more defaults */
	switch (attr->type) {
	case CKA_LABEL:
		gkm_transaction_fail (transaction, CKR_ATTRIBUTE_READ_ONLY);
		return;
	}

	/* Check if this attribute exists */
	check.type = attr->type;
	check.pValue = 0;
	check.ulValueLen = 0;
	rv = gkm_object_get_attribute (self, session, &check);
	if (rv == CKR_ATTRIBUTE_TYPE_INVALID)
		gkm_transaction_fail (transaction, CKR_ATTRIBUTE_TYPE_INVALID);
	else
		gkm_transaction_fail (transaction, CKR_ATTRIBUTE_READ_ONLY);
}

static void
gkm_object_real_create_attributes (GkmObject *self, GkmSession *session,
                                   GkmTransaction *transaction, CK_ATTRIBUTE *attrs, CK_ULONG n_attrs)
{
	CK_ATTRIBUTE_PTR transient_attr;
	gboolean transient = FALSE;
	gulong after = 0;
	gulong idle = 0;
	CK_RV rv;

	/* Parse the transient attribute */
	transient_attr = gkm_attributes_find (attrs, n_attrs, CKA_GNOME_TRANSIENT);
	if (transient_attr) {
		rv = gkm_attribute_get_bool (transient_attr, &transient);
		if (rv != CKR_OK) {
			gkm_transaction_fail (transaction, rv);
			return;
		}
	}

	/* Parse the auto destruct attribute */
	if (!gkm_attributes_find_ulong (attrs, n_attrs, CKA_G_DESTRUCT_AFTER, &after))
		after = 0;
	if (!gkm_attributes_find_ulong (attrs, n_attrs, CKA_G_DESTRUCT_IDLE, &idle))
		idle = 0;
	/* Default for the transient attribute */
	if (!transient_attr && (idle || after))
		transient = TRUE;

	/* Used up these attributes */
	gkm_attributes_consume (attrs, n_attrs, CKA_G_DESTRUCT_AFTER,
	                        CKA_G_DESTRUCT_IDLE, CKA_GNOME_TRANSIENT, G_MAXULONG);

	if (transient) {
		mark_object_transient (self);
		self->pv->transient->timed_after = after;
		self->pv->transient->timed_idle = idle;
	}

	if (after || idle) {
		if (!self->pv->transient) {
			gkm_transaction_fail (transaction, CKR_TEMPLATE_INCONSISTENT);
			return;
		}

		gkm_transaction_add (transaction, self, start_callback, NULL);
	}
}

static CK_RV
gkm_object_real_unlock (GkmObject *self, GkmCredential *cred)
{
	/* A derived class should have overridden this */
	return CKR_FUNCTION_FAILED;
}

static void
gkm_object_real_expose_object (GkmObject *self, gboolean expose)
{
	g_return_if_fail (expose != self->pv->exposed);
	g_return_if_fail (self->pv->manager);

	self->pv->exposed = expose;
	if (expose)
		_gkm_manager_register_object (self->pv->manager, self);
	else
		_gkm_manager_unregister_object (self->pv->manager, self);
}

static GObject*
gkm_object_constructor (GType type, guint n_props, GObjectConstructParam *props)
{
	GkmObject *self = GKM_OBJECT (G_OBJECT_CLASS (gkm_object_parent_class)->constructor(type, n_props, props));

	g_return_val_if_fail (self, NULL);
	g_return_val_if_fail (GKM_IS_MODULE (self->pv->module), NULL);

	return G_OBJECT (self);
}

static void
gkm_object_init (GkmObject *self)
{
	self->pv = G_TYPE_INSTANCE_GET_PRIVATE (self, GKM_TYPE_OBJECT, GkmObjectPrivate);

}

static void
gkm_object_dispose (GObject *obj)
{
	GkmObject *self = GKM_OBJECT (obj);
	GkmObjectTransient *transient;

	if (self->pv->manager) {
		if (self->pv->exposed)
			gkm_object_expose (self, FALSE);
		g_return_if_fail (!self->pv->exposed);
		g_object_remove_weak_pointer (G_OBJECT (self->pv->manager),
		                              (gpointer*)&(self->pv->manager));
		self->pv->manager = NULL;
	}

	g_object_set (self, "store", NULL, NULL);
	g_assert (self->pv->store == NULL);

	if (self->pv->transient) {
		transient = self->pv->transient;
		if (transient->timer)
			gkm_timer_cancel (transient->timer);
		transient->timer = NULL;
	}

	G_OBJECT_CLASS (gkm_object_parent_class)->dispose (obj);
}

static void
gkm_object_finalize (GObject *obj)
{
	GkmObject *self = GKM_OBJECT (obj);

	g_assert (self->pv->manager == NULL);
	g_free (self->pv->unique);

	/* This is done here, as an object must have a module even after dispose */
	g_object_weak_unref (G_OBJECT (self->pv->module), module_went_away, self);
	self->pv->module = NULL;

	if (self->pv->transient) {
		g_slice_free (GkmObjectTransient, self->pv->transient);
		self->pv->transient = NULL;
	}

	G_OBJECT_CLASS (gkm_object_parent_class)->finalize (obj);
}

static void
gkm_object_set_property (GObject *obj, guint prop_id, const GValue *value,
                           GParamSpec *pspec)
{
	GkmObject *self = GKM_OBJECT (obj);
	GkmStore *store;

	switch (prop_id) {
	case PROP_HANDLE:
		gkm_object_set_handle (self, g_value_get_ulong (value));
		break;
	case PROP_MODULE:
		g_return_if_fail (!self->pv->module);
		self->pv->module = g_value_get_object (value);
		g_return_if_fail (GKM_IS_MODULE (self->pv->module));
		g_object_weak_ref (G_OBJECT (self->pv->module), module_went_away, self);
		break;
	case PROP_MANAGER:
		g_return_if_fail (!self->pv->manager);
		self->pv->manager = g_value_get_object (value);
		if (self->pv->manager) {
			g_object_add_weak_pointer (G_OBJECT (self->pv->manager),
			                           (gpointer*)&(self->pv->manager));
		}
		break;
	case PROP_STORE:
		store = g_value_get_object (value);
		if (self->pv->store) {
			g_return_if_fail (!store);
			g_object_remove_weak_pointer (G_OBJECT (self->pv->store),
			                              (gpointer*)&(self->pv->store));
		}
		self->pv->store = store;
		if (self->pv->store)
			g_object_add_weak_pointer (G_OBJECT (self->pv->store),
			                           (gpointer*)&(self->pv->store));

		g_object_notify (G_OBJECT (self), "store");
		break;
	case PROP_UNIQUE:
		g_return_if_fail (!self->pv->unique);
		self->pv->unique = g_value_dup_string (value);
		break;
	case PROP_TRANSIENT:
		g_return_if_fail (!self->pv->transient);
		if (g_value_get_boolean (value))
			mark_object_transient (self);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_object_get_property (GObject *obj, guint prop_id, GValue *value,
                           GParamSpec *pspec)
{
	GkmObject *self = GKM_OBJECT (obj);

	switch (prop_id) {
	case PROP_HANDLE:
		g_value_set_ulong (value, gkm_object_get_handle (self));
		break;
	case PROP_MODULE:
		g_return_if_fail (GKM_IS_MODULE (self->pv->module));
		g_value_set_object (value, gkm_object_get_module (self));
		break;
	case PROP_MANAGER:
		g_value_set_object (value, gkm_object_get_manager (self));
		break;
	case PROP_STORE:
		g_value_set_object (value, self->pv->store);
		break;
	case PROP_UNIQUE:
		g_value_set_string (value, gkm_object_get_unique (self));
		break;
	case PROP_TRANSIENT:
		g_value_set_boolean (value, gkm_object_is_transient (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_object_class_init (GkmObjectClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gkm_object_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GkmObjectPrivate));

	gobject_class->constructor = gkm_object_constructor;
	gobject_class->dispose = gkm_object_dispose;
	gobject_class->finalize = gkm_object_finalize;
	gobject_class->set_property = gkm_object_set_property;
	gobject_class->get_property = gkm_object_get_property;

	klass->unlock = gkm_object_real_unlock;
	klass->get_attribute = gkm_object_real_get_attribute;
	klass->set_attribute = gkm_object_real_set_attribute;
	klass->create_attributes = gkm_object_real_create_attributes;

	klass->expose_object = gkm_object_real_expose_object;

	g_object_class_install_property (gobject_class, PROP_HANDLE,
	           g_param_spec_ulong ("handle", "Handle", "Object handle",
	                               0, G_MAXULONG, 0, G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_MODULE,
	           g_param_spec_object ("module", "Module", "Object module",
	                                GKM_TYPE_MODULE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_MANAGER,
	           g_param_spec_object ("manager", "Manager", "Object manager",
	                                GKM_TYPE_MANAGER, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_STORE,
	           g_param_spec_object ("store", "Store", "Object store",
	                                GKM_TYPE_STORE, G_PARAM_READWRITE));

	g_object_class_install_property (gobject_class, PROP_UNIQUE,
	           g_param_spec_string ("unique", "Unique Identifer", "Machine unique identifier",
	                                NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	g_object_class_install_property (gobject_class, PROP_TRANSIENT,
	           g_param_spec_boolean ("transient", "Transient Object", "Transient Object",
	                                 FALSE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

	signals[EXPOSE_OBJECT] = g_signal_new ("expose-object", GKM_TYPE_OBJECT,
	                                       G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GkmObjectClass, expose_object),
		                               NULL, NULL, g_cclosure_marshal_VOID__BOOLEAN,
		                               G_TYPE_NONE, 1, G_TYPE_BOOLEAN);

	signals[NOTIFY_ATTRIBUTE] = g_signal_new ("notify-attribute", GKM_TYPE_OBJECT,
	                                G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GkmObjectClass, notify_attribute),
	                                NULL, NULL, g_cclosure_marshal_VOID__ULONG,
	                                G_TYPE_NONE, 1, G_TYPE_ULONG);
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

CK_RV
gkm_object_get_attribute (GkmObject *self, GkmSession *session, CK_ATTRIBUTE_PTR attr)
{
	g_return_val_if_fail (GKM_IS_OBJECT (self), CKR_GENERAL_ERROR);
	g_return_val_if_fail (attr, CKR_GENERAL_ERROR);
	g_assert (GKM_OBJECT_GET_CLASS (self)->get_attribute);
	return GKM_OBJECT_GET_CLASS (self)->get_attribute (self, session, attr);
}

void
gkm_object_set_attribute (GkmObject *self, GkmSession *session,
                          GkmTransaction *transaction, CK_ATTRIBUTE_PTR attr)
{
	g_return_if_fail (GKM_IS_OBJECT (self));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (!gkm_transaction_get_failed (transaction));
	g_return_if_fail (attr);

	g_assert (GKM_OBJECT_GET_CLASS (self)->set_attribute);

	/* Check that the value will actually change */
	if (!gkm_object_match (self, session, attr))
		GKM_OBJECT_GET_CLASS (self)->set_attribute (self, session, transaction, attr);
}

void
gkm_object_create_attributes (GkmObject *self, GkmSession *session, GkmTransaction *transaction,
                              CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	g_return_if_fail (GKM_IS_OBJECT (self));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (!gkm_transaction_get_failed (transaction));
	g_return_if_fail (GKM_IS_SESSION (session));
	g_return_if_fail (attrs);

	g_assert (GKM_OBJECT_GET_CLASS (self)->create_attributes);

	/* Check that the value will actually change */
	GKM_OBJECT_GET_CLASS (self)->create_attributes (self, session, transaction, attrs, n_attrs);
}

void
gkm_object_notify_attribute  (GkmObject *self, CK_ATTRIBUTE_TYPE attr_type)
{
	g_return_if_fail (GKM_IS_OBJECT (self));
	g_signal_emit (self, signals[NOTIFY_ATTRIBUTE], 0, attr_type);
}

gboolean
gkm_object_match (GkmObject *self, GkmSession *session, CK_ATTRIBUTE_PTR match)
{
	CK_ATTRIBUTE attr;
	gboolean matched = FALSE;
	CK_RV rv;

	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);

	if (!match->pValue)
		return FALSE;

	attr.type = match->type;
	attr.pValue = g_malloc0 (match->ulValueLen > 4 ? match->ulValueLen : 4);
	attr.ulValueLen = match->ulValueLen;

	matched = FALSE;

	rv = gkm_object_get_attribute (self, session, &attr);
	matched = (rv == CKR_OK) &&
	          (match->ulValueLen == attr.ulValueLen) &&
	          (memcmp (match->pValue, attr.pValue, attr.ulValueLen) == 0);

	g_free (attr.pValue);
	return matched;
}

gboolean
gkm_object_match_all (GkmObject *self, GkmSession *session,
                      CK_ATTRIBUTE_PTR attrs, CK_ULONG n_attrs)
{
	CK_ULONG i;

	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);

	for (i = 0; i < n_attrs; ++i) {
		if (!gkm_object_match (self, session, &attrs[i]))
			return FALSE;
	}

	return TRUE;
}

CK_OBJECT_HANDLE
gkm_object_get_handle (GkmObject *self)
{
	g_return_val_if_fail (GKM_IS_OBJECT (self), 0);
	return self->pv->handle;
}

void
gkm_object_set_handle (GkmObject *self, CK_OBJECT_HANDLE handle)
{
	g_return_if_fail (GKM_IS_OBJECT (self));
	g_return_if_fail (handle != 0);
	g_return_if_fail (self->pv->handle == 0);

	self->pv->handle = handle;
	g_object_notify (G_OBJECT (self), "handle");
}

GkmManager*
gkm_object_get_manager (GkmObject *self)
{
	g_return_val_if_fail (GKM_IS_OBJECT (self), NULL);
	return self->pv->manager;
}

GkmModule*
gkm_object_get_module (GkmObject *self)
{
	g_return_val_if_fail (GKM_IS_OBJECT (self), NULL);
	g_return_val_if_fail (GKM_IS_MODULE (self->pv->module), NULL);
	return self->pv->module;
}

const gchar*
gkm_object_get_unique (GkmObject *self)
{
	g_return_val_if_fail (GKM_IS_OBJECT (self), NULL);
	return self->pv->unique;
}

gboolean
gkm_object_is_token (GkmObject *self)
{
	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);
	if (!self->pv->manager)
		return FALSE;
	return gkm_manager_get_for_token (self->pv->manager);
}

gboolean
gkm_object_is_transient (GkmObject *self)
{
	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);
	return self->pv->transient ? TRUE : FALSE;
}

void
gkm_object_mark_used (GkmObject *self)
{
	GkmObjectTransient *transient;
	GTimeVal tv;

	g_return_if_fail (GKM_IS_OBJECT (self));
	transient = self->pv->transient;

	if (transient) {
		if (transient->timed_idle) {
			g_get_current_time (&tv);
			transient->stamp_used = tv.tv_sec;
		}
		if (transient->uses_remaining) {
			--(transient->uses_remaining);
			if (transient->uses_remaining == 0)
				self_destruct (self);
		}
	}
}

CK_RV
gkm_object_unlock (GkmObject *self, GkmCredential *cred)
{
	g_return_val_if_fail (GKM_IS_OBJECT (self), CKR_GENERAL_ERROR);
	g_return_val_if_fail (GKM_OBJECT_GET_CLASS (self)->unlock, CKR_GENERAL_ERROR);
	return GKM_OBJECT_GET_CLASS (self)->unlock (self, cred);
}


gboolean
gkm_object_get_attribute_boolean (GkmObject *self, GkmSession *session,
                                  CK_ATTRIBUTE_TYPE type, gboolean *value)
{
	CK_ATTRIBUTE attr;
	CK_BBOOL bvalue;

	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (value, FALSE);

	attr.type = type;
	attr.ulValueLen = sizeof (CK_BBOOL);
	attr.pValue = &bvalue;

	if (gkm_object_get_attribute (self, session, &attr) != CKR_OK)
		return FALSE;

	*value = (bvalue == CK_TRUE) ? TRUE : FALSE;
	return TRUE;
}

gboolean
gkm_object_get_attribute_ulong (GkmObject *self, GkmSession *session,
                                CK_ATTRIBUTE_TYPE type, gulong *value)
{
	CK_ATTRIBUTE attr;
	CK_ULONG uvalue;

	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (value, FALSE);

	attr.type = type;
	attr.ulValueLen = sizeof (CK_ULONG);
	attr.pValue = &uvalue;

	if (gkm_object_get_attribute (self, session, &attr) != CKR_OK)
		return FALSE;

	*value = uvalue;
	return TRUE;
}

void*
gkm_object_get_attribute_data (GkmObject *self, GkmSession *session,
                               CK_ATTRIBUTE_TYPE type, gsize *n_data)
{
	CK_ATTRIBUTE attr;

	g_return_val_if_fail (GKM_IS_OBJECT (self), NULL);
	g_return_val_if_fail (n_data, NULL);

	attr.type = type;
	attr.ulValueLen = 0;
	attr.pValue = NULL;

	if (gkm_object_get_attribute (self, session, &attr) != CKR_OK)
		return NULL;

	if (attr.ulValueLen == 0)
		attr.ulValueLen = 1;

	attr.pValue = g_malloc0 (attr.ulValueLen);

	if (gkm_object_get_attribute (self, session, &attr) != CKR_OK) {
		g_free (attr.pValue);
		return NULL;
	}

	*n_data = attr.ulValueLen;
	return attr.pValue;
}

gboolean
gkm_object_has_attribute_ulong (GkmObject *self, GkmSession *session,
                                CK_ATTRIBUTE_TYPE type, gulong value)
{
	gulong *data;
	gsize n_data, i;

	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GKM_IS_SESSION (session), FALSE);

	data = gkm_object_get_attribute_data (self, session, type, &n_data);
	if (data == NULL)
		return FALSE;

	g_return_val_if_fail (n_data % sizeof (gulong) == 0, FALSE);
	for (i = 0; i < n_data / sizeof (gulong); ++i) {
		if (data[i] == value) {
			g_free (data);
			return TRUE;
		}
	}

	g_free (data);
	return FALSE;
}

gboolean
gkm_object_has_attribute_boolean (GkmObject *self, GkmSession *session,
                                  CK_ATTRIBUTE_TYPE type, gboolean value)
{
	gboolean data;

	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);
	g_return_val_if_fail (GKM_IS_SESSION (session), FALSE);

	if (!gkm_object_get_attribute_boolean (self, session, type, &data))
		return FALSE;
	return data == value;
}

void
gkm_object_destroy (GkmObject *self, GkmTransaction *transaction)
{
	GkmSession *session;
	GkmManager *manager;
	GkmModule *module;

	g_return_if_fail (GKM_IS_OBJECT (self));
	g_return_if_fail (GKM_IS_TRANSACTION (transaction));
	g_return_if_fail (!gkm_transaction_get_failed (transaction));
	g_return_if_fail (self->pv->module);

	g_object_ref (self);

	session = gkm_session_for_session_object (self);
	if (session != NULL) {
		gkm_session_destroy_session_object (session, transaction, self);
	} else {
		manager = gkm_object_get_manager (self);
		module = gkm_object_get_module (self);
		if (manager == gkm_module_get_manager (module))
			gkm_module_remove_token_object (module, transaction, self);
	}

	/* Forcefully dispose of the object once the transaction completes */
	gkm_transaction_add (transaction, NULL, complete_destroy, g_object_ref (self));

	g_object_unref (self);
}

gboolean
gkm_object_is_exposed (GkmObject *self)
{
	g_return_val_if_fail (GKM_IS_OBJECT (self), FALSE);
	return self->pv->exposed;
}

void
gkm_object_expose (GkmObject *self, gboolean expose)
{
	if (!expose && !self)
		return;

	g_return_if_fail (GKM_IS_OBJECT (self));

	if (self->pv->exposed != expose)
		g_signal_emit (self, signals[EXPOSE_OBJECT], 0, expose);
}

void
gkm_object_expose_full (GkmObject *self, GkmTransaction *transaction, gboolean expose)
{
	if (!expose && !self)
		return;

	g_return_if_fail (GKM_IS_OBJECT (self));
	g_return_if_fail (!transaction || !gkm_transaction_get_failed (transaction));

	if (self->pv->exposed != expose) {
		if (transaction)
			gkm_transaction_add (transaction, self, complete_expose, GUINT_TO_POINTER (expose));
		gkm_object_expose (self, expose);
	}
}
