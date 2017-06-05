/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* gkm-secret-binary.c - The binary encrypted format of a keyring

   Copyright (C) 2003 Red Hat, Inc
   Copyright (C) 2007, 2009 Stefan Walter

   Gnome keyring is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   Gnome keyring is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Alexander Larsson <alexl@redhat.com>
   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "gkm-secret-binary.h"
#include "gkm-secret-collection.h"
#include "gkm-secret-compat.h"
#include "gkm-secret-data.h"
#include "gkm-secret-fields.h"
#include "gkm-secret-item.h"

#include "egg/egg-buffer.h"
#include "egg/egg-symkey.h"
#include "egg/egg-secure-memory.h"

#include "gkm/gkm-secret.h"

#include <glib.h>

#include <gcrypt.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/* -----------------------------------------------------------------------------
 * DECLARATIONS
 */

enum {
	LOCK_ON_IDLE_FLAG = 1 << 0,
	LOCK_AFTER_FLAG = 1 << 1
};

typedef struct {
	/* unencrypted: */
	guint32 id;
	gchar *identifier;
	guint32 type;

	/* encrypted: */
	char *display_name;
	const guchar *ptr_secret;
	gsize n_secret;
	time_t ctime;
	time_t mtime;
	GHashTable *attributes;
	GList *acl;
} ItemInfo;

#define KEYRING_FILE_HEADER "GnomeKeyring\n\r\0\n"
#define KEYRING_FILE_HEADER_LEN 16

/* -----------------------------------------------------------------------------
 * BUFFER UTILITY FUNCTIONS
 */

static gboolean
buffer_get_bytes (EggBuffer *buffer, gsize offset, gsize *next_offset,
                  guchar *out, gsize n_bytes)
{
	if (buffer->len < n_bytes || offset > buffer->len - n_bytes)
		return FALSE;
	memcpy (out, buffer->buf + offset, n_bytes);
	*next_offset = offset + n_bytes;
	return TRUE;
}

static gboolean
buffer_add_time (EggBuffer *buffer, glong time)
{
	guint64 val = time;
	return egg_buffer_add_uint32 (buffer, ((val >> 32) & 0xffffffff)) &&
	       egg_buffer_add_uint32 (buffer, (val & 0xffffffff));
}

static gboolean
buffer_get_time (EggBuffer *buffer, gsize offset, gsize *next_offset, glong *time)
{
	guint32 a, b;
	guint64 val;

	if (!egg_buffer_get_uint32 (buffer, offset, &offset, &a) ||
	    !egg_buffer_get_uint32 (buffer, offset, &offset, &b))
		return FALSE;

	val = ((guint64)a) << 32 | b;
	*next_offset = offset;
	*time = (time_t) val;
	return TRUE;
}

static gboolean
buffer_add_utf8_string (EggBuffer *buffer, const char *str)
{
	if (str && !g_utf8_validate (str, -1, NULL))
		return FALSE;
	return egg_buffer_add_string (buffer, str);
}

static gboolean
buffer_get_utf8_string (EggBuffer *buffer, gsize offset, gsize *next_offset,
                        char **str_ret)
{
	gsize len;
	char *str;

	if (!egg_buffer_get_string (buffer, offset, &offset, &str,
	                            (EggBufferAllocator)g_realloc))
		return FALSE;
	len = str ? strlen (str) : 0;

	if (str != NULL) {
		if (!g_utf8_validate (str, len, NULL)) {
			g_free (str);
			return FALSE;
		}
	}

	if (next_offset != NULL) {
		*next_offset = offset;
	}
	if (str_ret != NULL) {
		*str_ret = str;
	} else {
		g_free (str);
	}
	return TRUE;
}

static gboolean
buffer_add_secret (EggBuffer *buffer, GkmSecret *secret)
{
	const guchar *data = NULL;
	gsize n_data = 0;
	if (secret != NULL)
		data = gkm_secret_get (secret, &n_data);
	return egg_buffer_add_byte_array (buffer, data, n_data);
}

static void
buffer_add_attribute (EggBuffer *buffer, GHashTable *attributes, const gchar *key)
{
	guint32 number;

	buffer_add_utf8_string (buffer, key);

	/*
	 * COMPATIBILITY:
	 *
	 * Our new Secrets API doesn't support integer attributes. However, to have
	 * compatibility with old keyring code reading this file, we need to set
	 * the uint32 type attribute appropriately where expected.
	 *
	 * If there's an extra compat-uint32 attribute and the name of this attribute
	 * is contained in that list, then write as a uint32.
	 */

	/* Determine if it's a uint32 compatible value, and store as such if it is */
	if (gkm_secret_fields_get_compat_uint32 (attributes, key, &number)) {
		egg_buffer_add_uint32 (buffer, 1);
		egg_buffer_add_uint32 (buffer, number);

	/* A normal string attribute */
	} else {
		egg_buffer_add_uint32 (buffer, 0);
		buffer_add_utf8_string (buffer, gkm_secret_fields_get (attributes, key));
	}
}

static void
buffer_add_hashed_attribute (EggBuffer *buffer, GHashTable *attributes, const gchar *key)
{
	guint32 number;
	gchar *value;

	buffer_add_utf8_string (buffer, key);

	/* See comments in buffer_add_attribute. */

	/* Determine if it's a uint32 compatible value, and store as such if it is */
	if (gkm_secret_fields_get_compat_hashed_uint32 (attributes, key, &number)) {
		egg_buffer_add_uint32 (buffer, 1);
		egg_buffer_add_uint32 (buffer, number);

	/* A standard string attribute */
	} else {
		if (!gkm_secret_fields_get_compat_hashed_string (attributes, key, &value))
			g_return_if_reached ();
		egg_buffer_add_uint32 (buffer, 0);
		buffer_add_utf8_string (buffer, value);
		g_free (value);
	}
}

static gboolean
buffer_add_attributes (EggBuffer *buffer, GHashTable *attributes, gboolean hashed)
{
	GList *names, *l;

	g_assert (buffer);

	if (attributes == NULL) {
		egg_buffer_add_uint32 (buffer, 0);
	} else {
		names = gkm_secret_fields_get_names (attributes);
		egg_buffer_add_uint32 (buffer, g_list_length (names));
		for (l = names; l; l = g_list_next (l)) {
			if (hashed)
				buffer_add_hashed_attribute (buffer, attributes, l->data);
			else
				buffer_add_attribute (buffer, attributes, l->data);
		}
		g_list_free (names);
	}

	return !egg_buffer_has_error (buffer);
}

static gboolean
buffer_get_attributes (EggBuffer *buffer, gsize offset, gsize *next_offset,
                       GHashTable **attributes_out, gboolean hashed)
{
	guint32 list_size;
	GHashTable *attributes;
	char *name;
	guint32 type;
	char *str;
	guint32 val;
	int i;

	attributes = NULL;

	if (!egg_buffer_get_uint32 (buffer, offset, &offset, &list_size))
		goto bail;

	attributes = gkm_secret_fields_new ();
	for (i = 0; i < list_size; i++) {
		if (!buffer_get_utf8_string (buffer, offset, &offset, &name))
			goto bail;
		if (!egg_buffer_get_uint32 (buffer, offset, &offset, &type)) {
			g_free (name);
			goto bail;
		}
		switch (type) {
		case 0: /* A string */
			if (!buffer_get_utf8_string (buffer, offset, &offset, &str)) {
				g_free (name);
				goto bail;
			}
			if (hashed) {
				gkm_secret_fields_add_compat_hashed_string (attributes, name, str);
				g_free (name);
				g_free (str);
			} else {
				gkm_secret_fields_take (attributes, name, str);
			}
			break;
		case 1: /* A uint32 */
			if (!egg_buffer_get_uint32 (buffer, offset, &offset, &val)) {
				g_free (name);
				goto bail;
			}
			if (hashed)
				gkm_secret_fields_add_compat_hashed_uint32 (attributes, name, val);
			else
				gkm_secret_fields_add_compat_uint32 (attributes, name, val);
			g_free (name);
			break;
		default:
			g_free (name);
			goto bail;
		}
	}

	*attributes_out = attributes;
	*next_offset = offset;

	return TRUE;

bail:
	g_hash_table_unref (attributes);
	return FALSE;
}

static gboolean
convert_to_integer (const gchar *string, guint32 *result)
{
	gchar *end;
	*result = strtoul (string, &end, 10);
	return *end == 0;
}

/* -----------------------------------------------------------------------------
 * BINARY ENCRYPTED FILE FORMAT
 */

static gboolean
encrypt_buffer (EggBuffer *buffer, GkmSecret *master,
		guchar salt[8], int iterations)
{
	const gchar *password;
	gcry_cipher_hd_t cih;
	gcry_error_t gerr;
        guchar *key, *iv;
	gsize n_password;
	size_t pos;

	g_assert (buffer->len % 16 == 0);
	g_assert (16 == gcry_cipher_get_algo_blklen (GCRY_CIPHER_AES128));
	g_assert (16 == gcry_cipher_get_algo_keylen (GCRY_CIPHER_AES128));

	password = gkm_secret_get_password (master, &n_password);
	if (!egg_symkey_generate_simple (GCRY_CIPHER_AES128, GCRY_MD_SHA256,
	                                 password, n_password, salt, 8, iterations, &key, &iv))
		return FALSE;

	gerr = gcry_cipher_open (&cih, GCRY_CIPHER_AES128, GCRY_CIPHER_MODE_CBC, 0);
	if (gerr) {
		g_warning ("couldn't create aes cipher context: %s",
			   gcry_strerror (gerr));
		egg_secure_free (key);
		g_free (iv);
		return FALSE;
	}

	/* 16 = 128 bits */
	gerr = gcry_cipher_setkey (cih, key, 16);
	g_return_val_if_fail (!gerr, FALSE);
	egg_secure_free (key);

	/* 16 = 128 bits */
	gerr = gcry_cipher_setiv (cih, iv, 16);
	g_return_val_if_fail (!gerr, FALSE);
	g_free (iv);

	for (pos = 0; pos < buffer->len; pos += 16) {
		/* In place encryption */
		gerr = gcry_cipher_encrypt (cih, buffer->buf + pos, 16, NULL, 0);
		g_return_val_if_fail (!gerr, FALSE);
	}

	gcry_cipher_close (cih);

	return TRUE;
}

static gboolean
decrypt_buffer (EggBuffer *buffer, GkmSecret *master,
		guchar salt[8], int iterations)
{
	const gchar *password = NULL;
	gcry_cipher_hd_t cih;
	gcry_error_t gerr;
        guchar *key, *iv;
        gsize n_password = 0;
	size_t pos;

	g_assert (buffer->len % 16 == 0);
	g_assert (16 == gcry_cipher_get_algo_blklen (GCRY_CIPHER_AES128));
	g_assert (16 == gcry_cipher_get_algo_keylen (GCRY_CIPHER_AES128));

	/* No password is set, try an null password */
	if (master == NULL) {
		password = NULL;
		n_password = 0;
	} else {
		password = gkm_secret_get_password (master, &n_password);
	}

	if (!egg_symkey_generate_simple (GCRY_CIPHER_AES128, GCRY_MD_SHA256,
	                                 password, n_password, salt, 8, iterations, &key, &iv))
		return FALSE;

	gerr = gcry_cipher_open (&cih, GCRY_CIPHER_AES128, GCRY_CIPHER_MODE_CBC, 0);
	if (gerr) {
		g_warning ("couldn't create aes cipher context: %s",
			   gcry_strerror (gerr));
		egg_secure_free (key);
		g_free (iv);
		return FALSE;
	}

	/* 16 = 128 bits */
	gerr = gcry_cipher_setkey (cih, key, 16);
	g_return_val_if_fail (!gerr, FALSE);
	egg_secure_free (key);

	/* 16 = 128 bits */
	gerr = gcry_cipher_setiv (cih, iv, 16);
	g_return_val_if_fail (!gerr, FALSE);
	g_free (iv);

	for (pos = 0; pos < buffer->len; pos += 16) {
		/* In place encryption */
		gerr = gcry_cipher_decrypt (cih, buffer->buf + pos, 16, NULL, 0);
		g_return_val_if_fail (!gerr, FALSE);
	}

	gcry_cipher_close (cih);

	return TRUE;
}

static gboolean
verify_decrypted_buffer (EggBuffer *buffer)
{
        guchar digest[16];

	/* In case the world changes on us... */
	g_return_val_if_fail (gcry_md_get_algo_dlen (GCRY_MD_MD5) == sizeof (digest), 0);

	gcry_md_hash_buffer (GCRY_MD_MD5, (void*)digest,
			     (guchar*)buffer->buf + 16, buffer->len - 16);

	return memcmp (buffer->buf, digest, 16) == 0;
}

static gboolean
generate_acl_data (EggBuffer *buffer, GList *acl)
{
	GList *l;
	GkmSecretAccess *ac;

	egg_buffer_add_uint32 (buffer, g_list_length (acl));

	for (l = acl; l != NULL; l = l->next) {
		ac = l->data;

		egg_buffer_add_uint32 (buffer, ac->types_allowed);
		if (!buffer_add_utf8_string (buffer, ac->display_name) ||
		    !buffer_add_utf8_string (buffer, ac->pathname))
			return FALSE;

		/* Reserved: */
		if (!buffer_add_utf8_string (buffer, NULL))
			return FALSE;
		egg_buffer_add_uint32 (buffer, 0);
	}

	return TRUE;
}

static gboolean
generate_encrypted_data (EggBuffer *buffer, GkmSecretCollection *collection,
                         GkmSecretData *data)
{
	GkmSecretObject *obj;
	GkmSecretItem *item;
	GList *items, *l;
	GHashTable *attributes;
	const gchar *label;
	GkmSecret *secret;
	GList *acl;
	int i;

	g_assert (buffer);
	g_assert (GKM_IS_SECRET_COLLECTION (collection));
	g_assert (GKM_IS_SECRET_DATA (data));

	/* Make sure we're using non-pageable memory */
	egg_buffer_set_allocator (buffer, egg_secure_realloc);

	items = gkm_secret_collection_get_items (collection);
	for (l = items; l && !egg_buffer_has_error(buffer); l = g_list_next (l)) {
		item = GKM_SECRET_ITEM (l->data);
		obj = GKM_SECRET_OBJECT (l->data);

		label = gkm_secret_object_get_label (obj);
		buffer_add_utf8_string (buffer, label);

		secret = gkm_secret_data_get_secret (data, gkm_secret_object_get_identifier (obj));
		buffer_add_secret (buffer, secret);

		if (!buffer_add_time (buffer, gkm_secret_object_get_created (obj)) ||
		    !buffer_add_time (buffer, gkm_secret_object_get_modified (obj)))
			break;

		/* reserved: */
		if (!buffer_add_utf8_string (buffer, NULL))
			break;
		for (i = 0; i < 4; i++)
			egg_buffer_add_uint32 (buffer, 0);

		attributes = gkm_secret_item_get_fields (item);
		if (!buffer_add_attributes (buffer, attributes, FALSE))
			break;

		acl = g_object_get_data (G_OBJECT (item), "compat-acl");
		if (!generate_acl_data (buffer, acl))
			break;
	}

	g_list_free (items);

	/* Iteration completed prematurely == fail */
	return (l == NULL);
}

static gboolean
generate_hashed_items (GkmSecretCollection *collection, EggBuffer *buffer)
{
	GHashTable *attributes;
	const gchar *value;
	GList *items, *l;
	guint32 id, type;

	items = gkm_secret_collection_get_items (collection);
	egg_buffer_add_uint32 (buffer, g_list_length (items));

	for (l = items; l; l = g_list_next (l)) {

		value = gkm_secret_object_get_identifier (l->data);
		if (!convert_to_integer (value, &id)) {
			g_warning ("trying to save a non-numeric item identifier '%s' into "
			           "the keyring file format which only supports numeric.", value);
			continue;
		}
		egg_buffer_add_uint32 (buffer, id);

		value = gkm_secret_item_get_schema (l->data);
		type = gkm_secret_compat_parse_item_type (value);
		egg_buffer_add_uint32 (buffer, type);

		attributes = gkm_secret_item_get_fields (l->data);
		buffer_add_attributes (buffer, attributes, TRUE);
	}

	g_list_free (items);
	return !egg_buffer_has_error (buffer);
}

GkmDataResult
gkm_secret_binary_write (GkmSecretCollection *collection, GkmSecretData *sdata,
                         guchar **data, gsize *n_data)
{
	GkmSecretObject *obj;
	EggBuffer to_encrypt;
	GkmSecret *master;
        guchar digest[16];
        EggBuffer buffer;
        gint hash_iterations;
        gint lock_timeout;
        guchar salt[8];
	guint flags = 0;
	int i;

	g_return_val_if_fail (GKM_IS_SECRET_COLLECTION (collection), GKM_DATA_FAILURE);
	g_return_val_if_fail (GKM_IS_SECRET_DATA (sdata), GKM_DATA_LOCKED);
	g_return_val_if_fail (data && n_data, GKM_DATA_FAILURE);
	g_return_val_if_fail (gcry_md_get_algo_dlen (GCRY_MD_MD5) == sizeof (digest), GKM_DATA_FAILURE);

	obj = GKM_SECRET_OBJECT (collection);

	egg_buffer_init_full (&buffer, 256, g_realloc);

	/* Prepare the keyring for encryption */
	hash_iterations = 1000 + (int) (1000.0 * rand() / (RAND_MAX + 1.0));
	gcry_create_nonce (salt, sizeof (salt));

	egg_buffer_append (&buffer, (guchar*)KEYRING_FILE_HEADER, KEYRING_FILE_HEADER_LEN);
	egg_buffer_add_byte (&buffer, 0); /* Major version */
	egg_buffer_add_byte (&buffer, 0); /* Minor version */
	egg_buffer_add_byte (&buffer, 0); /* crypto (0 == AES) */
	egg_buffer_add_byte (&buffer, 0); /* hash (0 == MD5) */

	buffer_add_utf8_string (&buffer, gkm_secret_object_get_label (obj));
	buffer_add_time (&buffer, gkm_secret_object_get_modified (obj));
	buffer_add_time (&buffer, gkm_secret_object_get_created (obj));

	lock_timeout = gkm_secret_collection_get_lock_idle (collection);
	if (lock_timeout) {
		flags |= LOCK_ON_IDLE_FLAG;
	} else {
		lock_timeout = gkm_secret_collection_get_lock_after (collection);
		if (lock_timeout)
			flags |= LOCK_AFTER_FLAG;
	}

	egg_buffer_add_uint32 (&buffer, flags);

	egg_buffer_add_uint32 (&buffer, lock_timeout);
	egg_buffer_add_uint32 (&buffer, hash_iterations);
	egg_buffer_append (&buffer, salt, 8);

	/* Reserved: */
	for (i = 0; i < 4; i++)
		egg_buffer_add_uint32 (&buffer, 0);

	/* Hashed items: */
	generate_hashed_items (collection, &buffer);

	/* Encrypted data. Use non-pageable memory */
	egg_buffer_init_full (&to_encrypt, 4096, egg_secure_realloc);

	egg_buffer_append (&to_encrypt, (guchar*)digest, 16); /* Space for hash */

	if (!generate_encrypted_data (&to_encrypt, collection, sdata)) {
		egg_buffer_uninit (&to_encrypt);
		egg_buffer_uninit (&buffer);
		return GKM_DATA_FAILURE;
	}

	/* Pad with zeros to multiple of 16 bytes */
	while (to_encrypt.len % 16 != 0)
		egg_buffer_add_byte (&to_encrypt, 0);

	gcry_md_hash_buffer (GCRY_MD_MD5, (void*)digest,
			     (guchar*)to_encrypt.buf + 16, to_encrypt.len - 16);
	memcpy (to_encrypt.buf, digest, 16);

	/* If no master password is set, we shouldn't be writing binary... */
	master = gkm_secret_data_get_master (sdata);
	g_return_val_if_fail (master, GKM_DATA_FAILURE);

	if (!encrypt_buffer (&to_encrypt, master, salt, hash_iterations)) {
		egg_buffer_uninit (&buffer);
		egg_buffer_uninit (&to_encrypt);
		return GKM_DATA_FAILURE;
	}

	if (egg_buffer_has_error (&to_encrypt) || egg_buffer_has_error (&buffer)) {
		egg_buffer_uninit (&buffer);
		egg_buffer_uninit (&to_encrypt);
		return GKM_DATA_FAILURE;
	}

	egg_buffer_add_uint32 (&buffer, to_encrypt.len);
	egg_buffer_append (&buffer, to_encrypt.buf, to_encrypt.len);
	egg_buffer_uninit (&to_encrypt);
	*data = egg_buffer_uninit_steal (&buffer, n_data);

	return GKM_DATA_SUCCESS;
}

static gboolean
decode_acl (EggBuffer *buffer, gsize offset, gsize *offset_out, GList **out)
{
	GList *acl;
	guint32 num_acs;
	guint32 x, y;
	int i;
	GkmSecretAccess *ac;
	char *name, *path, *reserved;

	acl = NULL;

	if (!egg_buffer_get_uint32 (buffer, offset, &offset, &num_acs))
		return FALSE;
	for (i = 0; i < num_acs; i++) {
		if (!egg_buffer_get_uint32 (buffer, offset, &offset, &x)) {
			goto bail;
		}
		if (!buffer_get_utf8_string (buffer, offset, &offset, &name)) {
			goto bail;
		}
		if (!buffer_get_utf8_string (buffer, offset, &offset, &path)) {
			g_free (name);
			goto bail;
		}
		reserved = NULL;
		if (!buffer_get_utf8_string (buffer, offset, &offset, &reserved)) {
			g_free (name);
			g_free (path);
			goto bail;
		}
		g_free (reserved);
		if (!egg_buffer_get_uint32 (buffer, offset, &offset, &y)) {
			g_free (name);
			g_free (path);
			goto bail;
		}

		ac = g_new0 (GkmSecretAccess, 1);
		ac->display_name = name;
		ac->pathname = path;
		ac->types_allowed = x;

		acl = g_list_prepend (acl, ac);
	}

	*offset_out = offset;
	*out = g_list_reverse (acl);
	return TRUE;

bail:
	gkm_secret_compat_acl_free (acl);
	return FALSE;
}

static void
remove_unavailable_item (gpointer key, gpointer dummy, gpointer user_data)
{
	/* Called to remove items from a keyring that no longer exist */

	GkmSecretCollection *collection = user_data;
	GkmSecretItem *item;

	g_assert (GKM_IS_SECRET_COLLECTION (collection));

	item = gkm_secret_collection_get_item (collection, key);
	if (item != NULL)
		gkm_secret_collection_remove_item (collection, item);
}

static void
setup_item_from_info (GkmSecretItem *item, GkmSecretData *data, ItemInfo *info)
{
	GkmSecretObject *obj = GKM_SECRET_OBJECT (item);
	GkmSecret *secret;
	const gchar *type;

	gkm_secret_object_set_label (obj, info->display_name);
	gkm_secret_object_set_created (obj, info->ctime);
	gkm_secret_object_set_modified (obj, info->mtime);

	type = gkm_secret_compat_format_item_type (info->type);
	gkm_secret_item_set_schema (item, type);
	gkm_secret_item_set_fields (item, info->attributes);

	/* Collection is locked */
	if (!data) {
		g_object_set_data (G_OBJECT (item), "compat-acl", NULL);

	} else {
		secret = gkm_secret_new (info->ptr_secret, info->n_secret);
		gkm_secret_data_set_secret (data, gkm_secret_object_get_identifier (obj), secret);
		g_object_unref (secret);
		g_object_set_data_full (G_OBJECT (item), "compat-acl", info->acl, gkm_secret_compat_acl_free);
		info->acl = NULL;
	}
}

static gboolean
read_hashed_item_info (EggBuffer *buffer, gsize *offset, ItemInfo *items, guint n_items)
{
	gint i;

	g_assert (buffer);
	g_assert (offset);
	g_assert (items);

	for (i = 0; i < n_items; i++) {
		if (!egg_buffer_get_uint32 (buffer, *offset, offset, &items[i].id) ||
		    !egg_buffer_get_uint32 (buffer, *offset, offset, &items[i].type) ||
		    !buffer_get_attributes (buffer, *offset, offset, &items[i].attributes, TRUE))
			return FALSE;
		items[i].identifier = g_strdup_printf ("%u", items[i].id);
	}

	return TRUE;
}

static gboolean
read_full_item_info (EggBuffer *buffer, gsize *offset, ItemInfo *items, guint n_items)
{
	gchar *reserved;
	guint32 tmp;
	gint i, j;

	g_assert (buffer);
	g_assert (offset);
	g_assert (items);

	for (i = 0; i < n_items; i++) {

		/* The display name */
		if (!buffer_get_utf8_string (buffer, *offset, offset,
		                             &items[i].display_name))
			return FALSE;

		/* The secret */
		if (!egg_buffer_get_byte_array (buffer, *offset, offset,
		                                &items[i].ptr_secret, &items[i].n_secret))
			return FALSE;

		/* The item times */
		if (!buffer_get_time (buffer, *offset, offset, &items[i].ctime) ||
		    !buffer_get_time (buffer, *offset, offset, &items[i].mtime))
			return FALSE;

		/* Reserved data */
		reserved = NULL;
		if (!buffer_get_utf8_string (buffer, *offset, offset, &reserved))
			return FALSE;
		g_free (reserved);
		for (j = 0; j < 4; j++) {
			if (!egg_buffer_get_uint32 (buffer, *offset, offset, &tmp))
				return FALSE;
		}

		/* The attributes */
		if (items[i].attributes)
			g_hash_table_unref (items[i].attributes);
		if (!buffer_get_attributes (buffer, *offset, offset, &items[i].attributes, FALSE))
			return FALSE;

		/* The ACLs */
		if (!decode_acl (buffer, *offset, offset, &items[i].acl))
			return FALSE;
	}

	return TRUE;
}

static void
free_item_info (ItemInfo *info)
{
	g_free (info->identifier);
	g_free (info->display_name);
	g_hash_table_unref (info->attributes);
	gkm_secret_compat_acl_free (info->acl);
}

gint
gkm_secret_binary_read (GkmSecretCollection *collection, GkmSecretData *sdata,
                        const guchar *data, gsize n_data)
{
	gsize offset;
	guchar major, minor, crypto, hash;
	guint32 flags;
	guint32 lock_timeout;
	time_t mtime, ctime;
	char *display_name;
	guint32 tmp;
	guint32 num_items;
	guint32 crypto_size;
	guint32 hash_iterations;
	guchar salt[8];
	ItemInfo *items;
	GkmSecret* master;
	GkmSecretObject *obj;
	EggBuffer to_decrypt = EGG_BUFFER_EMPTY;
	GkmDataResult res = GKM_DATA_FAILURE;
	GHashTable *checks = NULL;
	GkmSecretItem *item;
	EggBuffer buffer;
	GList *l, *iteml;
	int i;

	display_name = NULL;
	items = 0;
	obj = GKM_SECRET_OBJECT (collection);

	/* The buffer we read from */
	egg_buffer_init_static (&buffer, data, n_data);

	if (buffer.len < KEYRING_FILE_HEADER_LEN ||
	    memcmp (buffer.buf, KEYRING_FILE_HEADER, KEYRING_FILE_HEADER_LEN) != 0) {
		egg_buffer_uninit (&buffer);
		return GKM_DATA_UNRECOGNIZED;
	}

	offset = KEYRING_FILE_HEADER_LEN;
	major = buffer.buf[offset++];
	minor = buffer.buf[offset++];
	crypto = buffer.buf[offset++];
	hash = buffer.buf[offset++];

	if (major != 0 || minor != 0 || crypto != 0 || hash != 0) {
		egg_buffer_uninit (&buffer);
		return GKM_DATA_UNRECOGNIZED;
	}

	if (!buffer_get_utf8_string (&buffer, offset, &offset, &display_name) ||
	    !buffer_get_time (&buffer, offset, &offset, &ctime) ||
	    !buffer_get_time (&buffer, offset, &offset, &mtime) ||
	    !egg_buffer_get_uint32 (&buffer, offset, &offset, &flags) ||
	    !egg_buffer_get_uint32 (&buffer, offset, &offset, &lock_timeout) ||
	    !egg_buffer_get_uint32 (&buffer, offset, &offset, &hash_iterations) ||
	    !buffer_get_bytes (&buffer, offset, &offset, salt, 8))
		goto bail;

	for (i = 0; i < 4; i++) {
		if (!egg_buffer_get_uint32 (&buffer, offset, &offset, &tmp))
			goto bail;
	}

	if (!egg_buffer_get_uint32 (&buffer, offset, &offset, &num_items))
		goto bail;

	items = g_new0 (ItemInfo, num_items + 1);

	/* Hashed data, without secrets */
	if (!read_hashed_item_info (&buffer, &offset, items, num_items))
		goto bail;

	if (!egg_buffer_get_uint32 (&buffer, offset, &offset, &crypto_size))
		goto bail;

	/* Make the crypted part is the right size */
	if (crypto_size % 16 != 0)
		goto bail;

	/* Copy the data into to_decrypt into non-pageable memory */
	egg_buffer_set_allocator (&to_decrypt, egg_secure_realloc);
	egg_buffer_reserve (&to_decrypt, crypto_size);
	memcpy (to_decrypt.buf, buffer.buf + offset, crypto_size);
	to_decrypt.len = crypto_size;

	if (sdata != NULL) {
		master = gkm_secret_data_get_master (sdata);
		if (!decrypt_buffer (&to_decrypt, master, salt, hash_iterations))
			goto bail;
		if (!verify_decrypted_buffer (&to_decrypt)) {
			res = GKM_DATA_LOCKED;
			goto bail;
		} else {
			offset = 16; /* Skip hash */
			if (!read_full_item_info (&to_decrypt, &offset, items, num_items))
				goto bail;
		}
	}

	/* Correctly read all data, possibly including the decrypted data.
	 * Now update the keyring and items: */

	gkm_secret_object_set_label (obj, display_name);
	gkm_secret_object_set_modified (obj, mtime);
	gkm_secret_object_set_created (obj, ctime);
	if (flags & LOCK_ON_IDLE_FLAG)
		gkm_secret_collection_set_lock_idle (collection, lock_timeout);
	else if (flags & LOCK_AFTER_FLAG)
		gkm_secret_collection_set_lock_after (collection, lock_timeout);

	/* Build a Hash table where we can track ids we haven't yet seen */
	checks = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
	iteml = gkm_secret_collection_get_items (collection);
	for (l = iteml; l; l = g_list_next (l))
		g_hash_table_insert (checks, g_strdup (gkm_secret_object_get_identifier (l->data)), "unused");
	g_list_free (iteml);

	for (i = 0; i < num_items; i++) {

		/* We've seen this id */
		g_hash_table_remove (checks, items[i].identifier);

		item = gkm_secret_collection_get_item (collection, items[i].identifier);
		if (item == NULL)
			item = gkm_secret_collection_new_item (collection, items[i].identifier);

		setup_item_from_info (item, sdata, &items[i]);
	}

	g_hash_table_foreach (checks, remove_unavailable_item, collection);
	res = GKM_DATA_SUCCESS;

bail:
	egg_buffer_uninit (&to_decrypt);
	if (checks)
		g_hash_table_destroy (checks);
	g_free (display_name);

	for (i = 0; items && i < num_items; i++)
		free_item_info (&items[i]);
	g_free (items);

	return res;
}
