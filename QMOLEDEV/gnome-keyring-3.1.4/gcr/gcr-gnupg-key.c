/*
 * Copyright (C) 2011 Collabora Ltd.
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

#include "gcr-gnupg-key.h"
#include "gcr-record.h"
#include "gcr-memory-icon.h"

#include "gck/gck.h"

#include <gdk/gdk.h>
#include <glib/gi18n-lib.h>

enum {
	PROP_0,
	PROP_KEYID,
	PROP_PUBLIC_RECORDS,
	PROP_SECRET_RECORDS,
	PROP_LABEL,
	PROP_MARKUP,
	PROP_DESCRIPTION,
	PROP_SHORT_KEYID,
	PROP_ICON
};

struct _GcrGnupgKeyPrivate {
	GPtrArray *public_records;
	GPtrArray *secret_records;
	GIcon *icon;
};

G_DEFINE_TYPE (GcrGnupgKey, _gcr_gnupg_key, G_TYPE_OBJECT);

/* -----------------------------------------------------------------------------
 * INTERNAL
 */

/* Copied from GPGME */
static void
parse_user_id (const gchar *uid, gchar **name, gchar **email, gchar **comment)
{
	gchar *src, *tail, *x;
	int in_name = 0;
	int in_email = 0;
	int in_comment = 0;

	*name = NULL;
	*email = NULL;
	*comment = NULL;

	x = tail = src = g_strdup (uid);

	while (*src) {
		if (in_email) {
			/* Not legal but anyway.  */
			if (*src == '<')
				in_email++;
			else if (*src == '>') {
				if (!--in_email && !*email) {
					*email = tail;
					*src = 0;
					tail = src + 1;
				}
			}
		} else if (in_comment) {
			if (*src == '(')
				in_comment++;
			else if (*src == ')') {
				if (!--in_comment && !*comment) {
					*comment = tail;
					*src = 0;
					tail = src + 1;
				}
			}
		} else if (*src == '<') {
			if (in_name) {
				if (!*name) {
					*name = tail;
					*src = 0;
					tail = src + 1;
				}
				in_name = 0;
			} else
				tail = src + 1;

			in_email = 1;
		} else if (*src == '(') {
			if (in_name) {
				if (!*name) {
					*name = tail;
					*src = 0;
					tail = src + 1;
				}
				in_name = 0;
			}
			in_comment = 1;
		} else if (!in_name && *src != ' ' && *src != '\t') {
			in_name = 1;
		}
		src++;
	}

	if (in_name) {
		if (!*name) {
			*name = tail;
			*src = 0;
			tail = src + 1;
		}
	}

	/* Let unused parts point to an EOS.  */
	*name = g_strdup (*name ? *name : "");
	*email = g_strdup (*email ? *email : "");
	*comment = g_strdup (*comment ? *comment : "");

	g_strstrip (*name);
	g_strstrip (*email);
	g_strstrip (*comment);

	g_free (x);
}

static gchar *
calculate_name (GcrGnupgKey *self)
{
	GcrRecord* record;

	record = _gcr_record_find (self->pv->public_records, GCR_RECORD_SCHEMA_UID);
	g_return_val_if_fail (record, NULL);

	return _gcr_record_get_string (record, GCR_RECORD_UID_NAME);
}

static gchar *
calculate_markup (GcrGnupgKey *self)
{
	gchar *markup = NULL;
	gchar *uid, *name, *email, *comment;

	uid = calculate_name (self);
	if (uid == NULL)
		return NULL;

	parse_user_id (uid, &name, &email, &comment);
	if (comment != NULL && comment[0] != '\0')
		markup = g_markup_printf_escaped ("%s\n<small>%s \'%s\'</small>", name, email, comment);
	else
		markup = g_markup_printf_escaped ("%s\n<small>%s</small>", name, email);
	g_free (name);
	g_free (email);
	g_free (comment);
	g_free (uid);

	return markup;
}

static const gchar *
calculate_short_keyid (GcrGnupgKey *self)
{
	const gchar *keyid;
	gsize length;

	keyid = _gcr_gnupg_key_get_keyid_for_records (self->pv->public_records);
	if (keyid == NULL)
		return NULL;

	length = strlen (keyid);
	if (length > 8)
		keyid += (length - 8);

	return keyid;
}

static void
_gcr_gnupg_key_init (GcrGnupgKey *self)
{
	self->pv = (G_TYPE_INSTANCE_GET_PRIVATE (self, GCR_TYPE_GNUPG_KEY, GcrGnupgKeyPrivate));
}

static void
_gcr_gnupg_key_finalize (GObject *obj)
{
	GcrGnupgKey *self = GCR_GNUPG_KEY (obj);

	if (self->pv->public_records)
		g_ptr_array_free (self->pv->public_records, TRUE);
	if (self->pv->secret_records)
		g_ptr_array_free (self->pv->secret_records, TRUE);

	G_OBJECT_CLASS (_gcr_gnupg_key_parent_class)->finalize (obj);
}

static void
_gcr_gnupg_key_set_property (GObject *obj, guint prop_id, const GValue *value,
                             GParamSpec *pspec)
{
	GcrGnupgKey *self = GCR_GNUPG_KEY (obj);

	switch (prop_id) {
	case PROP_PUBLIC_RECORDS:
		_gcr_gnupg_key_set_public_records (self, g_value_get_boxed (value));
		break;
	case PROP_SECRET_RECORDS:
		_gcr_gnupg_key_set_secret_records (self, g_value_get_boxed (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
_gcr_gnupg_key_get_property (GObject *obj, guint prop_id, GValue *value,
                             GParamSpec *pspec)
{
	GcrGnupgKey *self = GCR_GNUPG_KEY (obj);

	switch (prop_id) {
	case PROP_PUBLIC_RECORDS:
		g_value_set_boxed (value, self->pv->public_records);
		break;
	case PROP_SECRET_RECORDS:
		g_value_set_boxed (value, self->pv->secret_records);
		break;
	case PROP_KEYID:
		g_value_set_string (value, _gcr_gnupg_key_get_keyid (self));
		break;
	case PROP_LABEL:
		g_value_take_string (value, calculate_name (self));
		break;
	case PROP_DESCRIPTION:
		g_value_set_string (value, _("PGP Key"));
		break;
	case PROP_MARKUP:
		g_value_take_string (value, calculate_markup (self));
		break;
	case PROP_SHORT_KEYID:
		g_value_set_string (value, calculate_short_keyid (self));
		break;
	case PROP_ICON:
		g_value_set_object (value, _gcr_gnupg_key_get_icon (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
_gcr_gnupg_key_class_init (GcrGnupgKeyClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	_gcr_gnupg_key_parent_class = g_type_class_peek_parent (klass);
	g_type_class_add_private (klass, sizeof (GcrGnupgKeyPrivate));

	gobject_class->finalize = _gcr_gnupg_key_finalize;
	gobject_class->set_property = _gcr_gnupg_key_set_property;
	gobject_class->get_property = _gcr_gnupg_key_get_property;

	/**
	 * GcrGnupgKey:public-records:
	 *
	 * Public key data. Should always be present.
	 */
	g_object_class_install_property (gobject_class, PROP_PUBLIC_RECORDS,
	         g_param_spec_boxed ("public-records", "Public Records", "Public Key Colon Records",
	                             G_TYPE_PTR_ARRAY, G_PARAM_READWRITE));

	/**
	 * GcrGnupgKey:secret-records:
	 *
	 * Secret key data. The keyid of this data must match public-dataset.
	 * If present, this key represents a secret key.
	 */
	g_object_class_install_property (gobject_class, PROP_SECRET_RECORDS,
	         g_param_spec_boxed ("secret-records", "Secret Records", "Secret Key Colon Records",
	                             G_TYPE_PTR_ARRAY, G_PARAM_READWRITE));

	/**
	 * GcrGnupgKey:keyid:
	 *
	 * Key identifier.
	 */
	g_object_class_install_property (gobject_class, PROP_KEYID,
	         g_param_spec_string ("keyid", "Key ID", "Key identifier",
	                              "", G_PARAM_READABLE));

	/**
	 * GcrGnupgKey:label:
	 *
	 * User readable label for this key.
	 */
	g_object_class_install_property (gobject_class, PROP_LABEL,
	         g_param_spec_string ("label", "Label", "Key label",
	                              "", G_PARAM_READABLE));

	/**
	 * GcrGnupgKey::description:
	 *
	 * Description of type of key.
	 */
	g_object_class_install_property (gobject_class, PROP_DESCRIPTION,
	         g_param_spec_string ("description", "Description", "Description of object type",
	                              "", G_PARAM_READABLE));

	/**
	 * GcrGnupgKey:markup:
	 *
	 * User readable markup which contains key label.
	 */
	g_object_class_install_property (gobject_class, PROP_MARKUP,
	         g_param_spec_string ("markup", "Markup", "Markup which describes key",
	                              "", G_PARAM_READABLE));

	/**
	 * GcrGnupgKey:short-keyid:
	 *
	 * User readable key identifier.
	 */
	g_object_class_install_property (gobject_class, PROP_SHORT_KEYID,
	         g_param_spec_string ("short-keyid", "Short Key ID", "Display key identifier",
	                              "", G_PARAM_READABLE));

	/**
	 * GcrGnupgKey:icon:
	 *
	 * Icon for this key.
	 */
	g_object_class_install_property (gobject_class, PROP_ICON,
	         g_param_spec_object ("icon", "Icon", "Icon for this key",
	                              G_TYPE_ICON, G_PARAM_READABLE));
}

/**
 * _gcr_gnupg_key_new:
 * @pubset: array of GcrRecord* representing public part of key
 * @secset: (allow-none): array of GcrRecord* representing secret part of key.
 *
 * Create a new GcrGnupgKey for the record data passed. If the secret part
 * of the key is set, then this represents a secret key; otherwise it represents
 * a public key.
 *
 * Returns: (transfer full): A newly allocated key.
 */
GcrGnupgKey*
_gcr_gnupg_key_new (GPtrArray *pubset, GPtrArray *secset)
{
	g_return_val_if_fail (pubset, NULL);
	return g_object_new (GCR_TYPE_GNUPG_KEY,
	                     "public-records", pubset,
	                     "secret-records", secset,
	                     NULL);
}

/**
 * _gcr_gnupg_key_get_public_records:
 * @self: The key
 *
 * Get the record data this key is based on.
 *
 * Returns: (transfer none): An array of GcrRecord*.
 */
GPtrArray*
_gcr_gnupg_key_get_public_records (GcrGnupgKey *self)
{
	g_return_val_if_fail (GCR_IS_GNUPG_KEY (self), NULL);
	return self->pv->public_records;
}

/**
 * _gcr_gnupg_key_set_public_records:
 * @self: The key
 * @records: The new array of GcrRecord*
 *
 * Change the record data that this key is based on.
 */
void
_gcr_gnupg_key_set_public_records (GcrGnupgKey *self, GPtrArray *records)
{
	GObject *obj;

	g_return_if_fail (GCR_IS_GNUPG_KEY (self));
	g_return_if_fail (records);

	/* Check that it matches previous */
	if (self->pv->public_records) {
		const gchar *old_keyid = _gcr_gnupg_key_get_keyid_for_records (self->pv->public_records);
		const gchar *new_keyid = _gcr_gnupg_key_get_keyid_for_records (records);

		if (g_strcmp0 (old_keyid, new_keyid) != 0) {
			g_warning ("it is an error to change a gnupg key so that the "
			           "fingerprint is no longer the same: %s != %s",
			           old_keyid, new_keyid);
			return;
		}
	}

	g_ptr_array_ref (records);
	if (self->pv->public_records)
		g_ptr_array_unref (self->pv->public_records);
	self->pv->public_records = records;

	obj = G_OBJECT (self);
	g_object_freeze_notify (obj);
	g_object_notify (obj, "public-records");
	g_object_notify (obj, "label");
	g_object_notify (obj, "markup");
	g_object_thaw_notify (obj);
}

/**
 * _gcr_gnupg_key_get_secret_records:
 * @self: The key
 *
 * Get the record secret data this key is based on. %NULL if a public key.
 *
 * Returns: (transfer none) (allow-none): An array of GcrColons*.
 */
GPtrArray*
_gcr_gnupg_key_get_secret_records (GcrGnupgKey *self)
{
	g_return_val_if_fail (GCR_IS_GNUPG_KEY (self), NULL);
	return self->pv->secret_records;
}

/**
 * _gcr_gnupg_key_set_secret_records:
 * @self: The key
 * @records: (allow-none): The new array of GcrRecord*
 *
 * Set the secret data for this key. %NULL if public key.
 */
void
_gcr_gnupg_key_set_secret_records (GcrGnupgKey *self, GPtrArray *records)
{
	GObject *obj;

	g_return_if_fail (GCR_IS_GNUPG_KEY (self));

	/* Check that it matches public key */
	if (self->pv->public_records && records) {
		const gchar *pub_keyid = _gcr_gnupg_key_get_keyid_for_records (self->pv->public_records);
		const gchar *sec_keyid = _gcr_gnupg_key_get_keyid_for_records (records);

		if (g_strcmp0 (pub_keyid, sec_keyid) != 0) {
			g_warning ("it is an error to create a gnupg key so that the "
			           "fingerprint of thet pub and sec parts are not the same: %s != %s",
			           pub_keyid, sec_keyid);
			return;
		}
	}

	if (records)
		g_ptr_array_ref (records);
	if (self->pv->secret_records)
		g_ptr_array_unref (self->pv->secret_records);
	self->pv->secret_records = records;

	obj = G_OBJECT (self);
	g_object_freeze_notify (obj);
	g_object_notify (obj, "secret-records");
	g_object_thaw_notify (obj);
}

/**
 * _gcr_gnupg_key_get_keyid:
 * @self: The key
 *
 * Get the keyid for this key.
 *
 * Returns: (transfer none): The keyid.
 */
const gchar*
_gcr_gnupg_key_get_keyid (GcrGnupgKey *self)
{
	g_return_val_if_fail (GCR_IS_GNUPG_KEY (self), NULL);
	return _gcr_gnupg_key_get_keyid_for_records (self->pv->public_records);
}

/**
 * _gcr_gnupg_key_get_keyid_for_records:
 * @records: Array of GcrRecord*
 *
 * Get the keyid for some record data.
 *
 * Returns: (transfer none): The keyid.
 */
const gchar*
_gcr_gnupg_key_get_keyid_for_records (GPtrArray *records)
{
	GcrRecord *record;

	record = _gcr_record_find (records, GCR_RECORD_SCHEMA_PUB);
	if (record != NULL)
		return _gcr_record_get_raw (record, GCR_RECORD_PUB_KEYID);
	record = _gcr_record_find (records, GCR_RECORD_SCHEMA_SEC);
	if (record != NULL)
		return _gcr_record_get_raw (record, GCR_RECORD_SEC_KEYID);
	return NULL;
}

/**
 * _gcr_gnupg_key_get_fingerprint_for_records:
 * @records: Array of GcrRecord*
 *
 * Get the fingerprint field for some record data:
 *
 * Returns: (transfer none): The fingerprint.
 */
const gchar*
_gcr_gnupg_key_get_fingerprint_for_records (GPtrArray *records)
{
	GcrRecord *record;

	record = _gcr_record_find (records, GCR_RECORD_SCHEMA_FPR);
	if (record != NULL)
		return _gcr_record_get_raw (record, GCR_RECORD_FPR_FINGERPRINT);
	return NULL;
}

#define TYPE_IMAGE 0x01
#define IMAGE_HEADER_LEN 0x10
#define IMAGE_JPEG_SIG "\x10\x00\x01\x01"
#define IMAGE_JPEG_SIG_LEN 4

static GIcon*
load_user_attribute_icon (GcrGnupgKey *self)
{
	GcrRecord *record;
	guchar *data;
	gsize n_data;
	guint type;
	guint i;

	for (i = 0; i < self->pv->public_records->len; i++) {
		record = self->pv->public_records->pdata[i];
		if (GCR_RECORD_SCHEMA_XA1 != _gcr_record_get_schema (record))
			continue;
		if (!_gcr_record_get_uint (record, GCR_RECORD_XA1_TYPE, &type))
			continue;
		if (type != TYPE_IMAGE)
			continue;

		/* TODO: Validity? */

		data = _gcr_record_get_base64 (record, GCR_RECORD_XA1_DATA, &n_data);
		g_return_val_if_fail (data != NULL, NULL);

		/* Header is 16 bytes long */
		if (n_data <= IMAGE_HEADER_LEN) {
			g_free (data);
			continue;
		}

		/* These are the header bytes. See gnupg doc/DETAILS */
		g_assert (IMAGE_JPEG_SIG_LEN < IMAGE_HEADER_LEN);
		if (memcmp (data, IMAGE_JPEG_SIG, IMAGE_JPEG_SIG_LEN) != 0) {
			g_free (data);
			continue;
		}

		/* We have a valid header */
		return G_ICON (_gcr_memory_icon_new_full ("image/jpeg", data,
		                                          n_data, IMAGE_HEADER_LEN,
		                                          g_free));
	}

	return NULL;
}

/**
 * _gcr_gnupg_key_get_icon:
 * @self: A gnupg key.
 *
 * Get the display icon for this key.
 *
 * Return value: (transfer none): The icon, owned by the key.
 */
GIcon*
_gcr_gnupg_key_get_icon (GcrGnupgKey *self)
{
	g_return_val_if_fail (GCR_IS_GNUPG_KEY (self), NULL);

	if (self->pv->icon == NULL) {
		self->pv->icon = load_user_attribute_icon (self);
		if (self->pv->icon == NULL) {
			if (self->pv->secret_records)
				self->pv->icon = g_themed_icon_new ("gcr-key-pair");
			else
				self->pv->icon = g_themed_icon_new ("gcr-key");
		}
	}

	return self->pv->icon;
}

/**
 * _gcr_gnupg_key_get_columns:
 *
 * Get the columns that we should display for gnupg keys.
 *
 * Returns: (transfer none): The columns, NULL terminated, should not be freed.
 */
const GcrColumn*
_gcr_gnupg_key_get_columns (void)
{
	static GcrColumn columns[] = {
		{ "icon", /* later */ 0, /* later */ 0, NULL, 0, NULL, 0 },
		{ "label", G_TYPE_STRING, G_TYPE_STRING, NC_("column", "Name"),
		  GCR_COLUMN_SORTABLE, NULL, 0 },
		{ "short-keyid", G_TYPE_STRING, G_TYPE_STRING, NC_("column", "Key ID"),
		  GCR_COLUMN_SORTABLE, NULL, 0 },
		{ NULL }
	};

	columns[0].property_type = columns[0].column_type = G_TYPE_ICON;
	return columns;
}
