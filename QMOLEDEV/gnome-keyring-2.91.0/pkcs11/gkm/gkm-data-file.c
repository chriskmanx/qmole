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

#include "gkm-attributes.h"
#include "gkm-crypto.h"
#include "gkm-data-file.h"
#include "gkm-data-types.h"
#include "gkm-marshal.h"
#include "gkm-util.h"

#include "egg/egg-buffer.h"
#include "egg/egg-hex.h"
#include "egg/egg-secure-memory.h"
#include "egg/egg-symkey.h"

#include <glib/gstdio.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <gcrypt.h>

enum {
	ENTRY_ADDED,
	ENTRY_CHANGED,
	ENTRY_REMOVED,
	LAST_SIGNAL
};

static guint signals[LAST_SIGNAL] = { 0 };

struct _GkmDataFile {
	GObject parent;

	/* The data itself */
	GHashTable *identifiers;
	GHashTable *privates;
	GHashTable *publics;
	GList *unknowns;

	/* All the sections seen */
	guint sections;
	gboolean incomplete;

	/* Stuff notseen on this read */
	GHashTable *checks;
};

typedef struct _UnknownBlock {
	guint type;
	EggBuffer buffer;
} UnknownBlock;

G_DEFINE_TYPE (GkmDataFile, gkm_data_file, G_TYPE_OBJECT);

#define PUBLIC_ALLOC (EggBufferAllocator)g_realloc
#define PRIVATE_ALLOC (EggBufferAllocator)egg_secure_realloc

typedef GkmDataResult (*BlockFunc) (guint block, EggBuffer *buffer, GkmSecret *login, gpointer user_data);

#define FILE_HEADER ((const guchar*)"Gnome Keyring Store 2\n\r\0")
#define FILE_HEADER_LEN 24

#define FILE_BLOCK_INDEX    0x49445832  /* ie: "IDX2" */
#define FILE_BLOCK_PRIVATE  0x50525632  /* ie: "PRV2" */
#define FILE_BLOCK_PUBLIC   0x50554232  /* ie: "PUB2" */

#define UNUSED_VALUE  GUINT_TO_POINTER (1)

/* -----------------------------------------------------------------------------
 * HELPERS
 */

static void
attribute_free (gpointer data)
{
	CK_ATTRIBUTE_PTR attr = data;
	if (attr) {
		g_free (attr->pValue);
		g_slice_free (CK_ATTRIBUTE, attr);
	}
}

static CK_ATTRIBUTE_PTR
attribute_dup (CK_ATTRIBUTE_PTR attr)
{
	CK_ATTRIBUTE_PTR copy;
	g_assert (attr);
	copy = g_slice_new (CK_ATTRIBUTE);
	copy->ulValueLen = attr->ulValueLen;
	copy->pValue = g_memdup (attr->pValue, copy->ulValueLen);
	copy->type = attr->type;
	return copy;
}

static GHashTable*
attributes_new (void)
{
	return g_hash_table_new_full (gkm_util_ulong_hash, gkm_util_ulong_equal, NULL, attribute_free);
}

static GHashTable*
entries_new (void)
{
	return g_hash_table_new_full (g_str_hash, g_str_equal, NULL, (GDestroyNotify)g_hash_table_unref);
}

static gboolean
read_all_bytes (int fd, guchar *buf, gsize len)
{
	gsize all = len;
	int res;

	while (len > 0) {
		res = read (fd, buf, len);
		if (res < 0) {
			if (errno == EAGAIN || errno == EINTR)
				continue;
			g_warning ("couldn't read %u bytes from store file: %s",
			           (guint)all, g_strerror (errno));
			return FALSE;
		} else if (res == 0) {
			if (len != all)
				g_warning ("couldn't read %u bytes from store file", (guint)all);
			return FALSE;
		} else  {
			len -= res;
			buf += res;
		}
	}

	return TRUE;
}

static gboolean
write_all_bytes (int fd, const guchar *buf, gsize len)
{
	gsize all = len;
	int res;

	while (len > 0) {

		res = write (fd, buf, len);
		if (res < 0) {
			if (errno == EAGAIN || errno == EINTR)
				continue;
			g_warning ("couldn't write %u bytes to store file: %s",
			           (guint)all, g_strerror (errno));
			return FALSE;
		} else if (res == 0) {
			g_warning ("couldn't write %u bytes to store file", (guint)all);
			return FALSE;
		} else  {
			len -= res;
			buf += res;
		}
	}

	return TRUE;
}

static GkmDataResult
parse_file_blocks (int file, BlockFunc block_func, GkmSecret *login, gpointer user_data)
{
	gchar header[FILE_HEADER_LEN];
	GkmDataResult res;
	EggBuffer buffer;
	guint32 block;
	guint32 length;
	gsize offset;

	g_assert (file != -1);
	g_assert (block_func);

	/* Zero length file is valid */
	if (!read_all_bytes (file, (guchar*)header, FILE_HEADER_LEN))
		return TRUE;

	/* Check the header */
	if (memcmp (header, FILE_HEADER, FILE_HEADER_LEN) != 0) {
		g_message ("invalid header in store file");
		return FALSE;
	}

	egg_buffer_init_full (&buffer, 1024, (EggBufferAllocator)g_realloc);

	res = GKM_DATA_SUCCESS;
	for (;;) {

		egg_buffer_reset (&buffer);
		egg_buffer_resize (&buffer, 8);
		offset = 0;

		/* Read in a set of bytes */
		if (!read_all_bytes (file, buffer.buf, 8)) {
			res = GKM_DATA_SUCCESS; /* end of file */
			break;
		}

		/* Decode it as the number of bytes in the next section */
		if (!egg_buffer_get_uint32 (&buffer, offset, &offset, &length) ||
		    !egg_buffer_get_uint32 (&buffer, offset, &offset, &block) ||
		    length < 8) {
			res = GKM_DATA_FAILURE;
			g_message ("invalid block size or length in store file");
			break;
		}

		/* Read in that amount of bytes */
		egg_buffer_resize (&buffer, length - 8);
		if (!read_all_bytes (file, buffer.buf, length - 8)) {
			res = GKM_DATA_FAILURE;
			break;
		}

		res = (block_func) (block, &buffer, login, user_data);
		if (res != GKM_DATA_SUCCESS)
			break;
	}

	egg_buffer_uninit (&buffer);
	return res;
}

static gboolean
write_file_block (int file, guint block, EggBuffer *buffer)
{
	EggBuffer header;
	gboolean ret;

	g_assert (file != -1);
	g_assert (buffer);

	/* Write out the 8 bytes of header */
	egg_buffer_init_full (&header, 8, (EggBufferAllocator)g_realloc);
	egg_buffer_add_uint32 (&header, buffer->len + 8);
	egg_buffer_add_uint32 (&header, block);
	g_assert (!egg_buffer_has_error (&header));
	g_assert (header.len == 8);
	ret = write_all_bytes (file, header.buf, header.len);
	egg_buffer_uninit (&header);

	if (ret != TRUE)
		return FALSE;

	/* Now write out the remainder of the data */
	return write_all_bytes (file, buffer->buf, buffer->len);
}

static gboolean
hash_buffer (EggBuffer *buffer)
{
	const gchar *salgo;
	gsize length;
	guchar *hash;
	gsize n_hash;
	int algo;

	/* The length needs to be the first thing in the buffer */
	g_assert (buffer->len > 4);
	g_assert (egg_buffer_decode_uint32 (buffer->buf) == buffer->len);

	length = buffer->len;

	algo = GCRY_MD_SHA256;
	salgo = gcry_md_algo_name (algo);
	g_return_val_if_fail (salgo, FALSE);
	n_hash = gcry_md_get_algo_dlen (algo);
	g_return_val_if_fail (n_hash > 0, FALSE);

	egg_buffer_add_string (buffer, salgo);
	hash = egg_buffer_add_byte_array_empty (buffer, n_hash);
	g_return_val_if_fail (hash, FALSE);

	gcry_md_hash_buffer (algo, hash, buffer->buf, length);
	return TRUE;
}

static gboolean
validate_buffer (EggBuffer *buffer, gsize *offset)
{
	const guchar *hash;
	gchar *salgo, *check;
	gsize n_hash, hash_offset;
	guint32 length;
	int algo;

	g_assert (buffer);
	g_assert (offset);

	*offset = 0;

	if (!egg_buffer_get_uint32 (buffer, *offset, offset, &length) ||
	    !egg_buffer_get_string (buffer, length, &hash_offset, &salgo, PUBLIC_ALLOC))
		return FALSE;

	algo = gcry_md_map_name (salgo);
	if (algo == 0) {
		g_warning ("unsupported hash algorithm: %s", salgo);
		g_free (salgo);
		return FALSE;
	}
	g_free (salgo);

	if (!egg_buffer_get_byte_array (buffer, hash_offset, &hash_offset, &hash, &n_hash))
		return FALSE;

	if (n_hash != gcry_md_get_algo_dlen (algo)) {
		g_warning ("invalid hash length in store file");
		return FALSE;
	}

	check = g_malloc0 (n_hash);
	gcry_md_hash_buffer (algo, check, buffer->buf, length);
	if (memcmp (check, hash, n_hash) != 0)
		return FALSE;

	return TRUE;
}

static gboolean
create_cipher (GkmSecret *login, int calgo, int halgo, const guchar *salt,
               gsize n_salt, guint iterations, gcry_cipher_hd_t *cipher)
{
	gsize n_key, n_block;
	const gchar *password;
	gsize n_password;
	guchar *key, *iv;
	gcry_error_t gcry;

	g_assert (login);
	g_assert (salt);
	g_assert (cipher);

	n_key = gcry_cipher_get_algo_keylen (calgo);
	g_return_val_if_fail (n_key, FALSE);
	n_block = gcry_cipher_get_algo_blklen (calgo);
	g_return_val_if_fail (n_block, FALSE);

	/* Allocate memory for the keys */
	key = gcry_malloc_secure (n_key);
	g_return_val_if_fail (key, FALSE);
	iv = g_malloc0 (n_block);

	password = gkm_secret_get_password (login, &n_password);

	if (!egg_symkey_generate_simple (calgo, halgo, password, n_password,
	                                 salt, n_salt, iterations, &key, &iv)) {
		gcry_free (key);
		g_free (iv);
		return FALSE;
	}

	gcry = gcry_cipher_open (cipher, calgo, GCRY_CIPHER_MODE_CBC, 0);
	if (gcry) {
		g_warning ("couldn't create cipher context: %s", gcry_strerror (gcry));
		gcry_free (key);
		g_free (iv);
		return FALSE;
	}

	gcry = gcry_cipher_setkey (*cipher, key, n_key);
	g_return_val_if_fail (!gcry, FALSE);
	gcry_free (key);

	gcry = gcry_cipher_setiv (*cipher, iv, n_block);
	g_return_val_if_fail (!gcry, FALSE);
	g_free (iv);

	return TRUE;
}

static gboolean
encrypt_buffer (EggBuffer *input, GkmSecret *login, EggBuffer *output)
{
	gcry_cipher_hd_t cipher;
	gcry_error_t gcry;
	guchar salt[8];
	guint32 iterations;
	int calgo, halgo;
	const gchar *salgo;
	guchar *dest;
	gsize n_block;

	g_assert (input);
	g_assert (output);
	g_assert (login);

	/* The algorithms we're going to use */
	calgo = GCRY_CIPHER_AES128;
	halgo = GCRY_MD_SHA256;

	/* Prepare us some salt */
	gcry_create_nonce (salt, sizeof (salt));

	/* Prepare us the iterations */
	iterations = 1000 + (int) (1000.0 * rand() / (RAND_MAX + 1.0));

	/* Write out crypto algorithm */
	salgo = gcry_cipher_algo_name (calgo);
	g_return_val_if_fail (salgo, FALSE);
	egg_buffer_add_string (output, salgo);

	/* Write out the hash algorithm */
	salgo = gcry_md_algo_name (halgo);
	g_return_val_if_fail (halgo, FALSE);
	egg_buffer_add_string (output, salgo);

	/* Write out the iterations */
	egg_buffer_add_uint32 (output, iterations);

	/* And write out the salt */
	egg_buffer_add_byte_array (output, salt, sizeof (salt));

	/* Okay now use the above info to create our cipher context */
	if (!create_cipher (login, calgo, halgo, salt, sizeof (salt), iterations, &cipher))
		return FALSE;

	/* Significant block sizes */
	n_block = gcry_cipher_get_algo_blklen (calgo);
	g_return_val_if_fail (n_block, FALSE);

	/* Pad the buffer to a multiple of block length */
	while (input->len % n_block != 0)
		egg_buffer_add_byte (input, 0);

	/* Now reserve space for it in the output block, and encrypt */
	dest = egg_buffer_add_byte_array_empty (output, input->len);
	g_return_val_if_fail (dest, FALSE);

	gcry = gcry_cipher_encrypt (cipher, dest, input->len, input->buf, input->len);
	g_return_val_if_fail (!gcry, FALSE);

	gcry_cipher_close (cipher);

	return TRUE;
}

static gboolean
decrypt_buffer (EggBuffer *input, gsize *offset, GkmSecret *login, EggBuffer *output)
{
	gcry_cipher_hd_t cipher;
	gcry_error_t gcry;
	const guchar *salt, *data;
	gsize n_block, n_salt, n_data;
	guint32 iterations;
	int calgo, halgo;
	gchar *salgo;

	g_assert (input);
	g_assert (output);
	g_assert (offset);
	g_assert (login);

	/* Read in and interpret the cipher algorithm */
	if (!egg_buffer_get_string (input, *offset, offset, &salgo, NULL))
		return FALSE;
	calgo = gcry_cipher_map_name (salgo);
	if (!calgo) {
		g_warning ("unsupported crypto algorithm: %s", salgo);
		g_free (salgo);
		return FALSE;
	}
	g_free (salgo);

	/* Read in and interpret the hash algorithm */
	if (!egg_buffer_get_string (input, *offset, offset, &salgo, NULL))
		return FALSE;
	halgo = gcry_md_map_name (salgo);
	if (!halgo) {
		g_warning ("unsupported crypto algorithm: %s", salgo);
		g_free (salgo);
		return FALSE;
	}
	g_free (salgo);

	/* Read in the iterations, salt, and encrypted data */
	if (!egg_buffer_get_uint32 (input, *offset, offset, &iterations) ||
	    !egg_buffer_get_byte_array (input, *offset, offset, &salt, &n_salt) ||
	    !egg_buffer_get_byte_array (input, *offset, offset, &data, &n_data))
		return FALSE;

	/* Significant block sizes */
	n_block = gcry_cipher_get_algo_blklen (calgo);
	g_return_val_if_fail (n_block, FALSE);

	/* Make sure the encrypted data is of a good length */
	if (n_data % n_block != 0) {
		g_warning ("encrypted data in file store is of an invalid length for algorithm");
		return FALSE;
	}

	/* Create the cipher context */
	if (!create_cipher (login, calgo, halgo, salt, n_salt, iterations, &cipher))
		return FALSE;

	/* Now reserve space for it in the output block, and encrypt */
	egg_buffer_reset (output);
	egg_buffer_resize (output, n_data);

	gcry = gcry_cipher_decrypt (cipher, output->buf, output->len, data, n_data);
	g_return_val_if_fail (!gcry, FALSE);

	gcry_cipher_close (cipher);

	return TRUE;
}

/* ----------------------------------------------------------------------------------------
 * INTERNAL
 */

static GkmDataResult
update_entries_from_block (GkmDataFile *self, guint section, GHashTable *entries,
                           EggBuffer *buffer, gsize *offset)
{
	GHashTable *attributes;
	const gchar *identifier;
	gboolean added;
	CK_ATTRIBUTE_PTR at;
	CK_ATTRIBUTE attr;
	gpointer key, value;
	guint32 n_entries, i;
	guint32 n_attrs, j;
	gchar *str;
	guint sect;
	const guchar *data;
	gsize n_data;
	guint64 type;

	g_assert (GKM_IS_DATA_FILE (self));
	g_assert (entries);
	g_assert (buffer);
	g_assert (offset);

	/* The number of entries */
	if (!egg_buffer_get_uint32 (buffer, *offset, offset, &n_entries))
		return GKM_DATA_FAILURE;

	for (i = 0; i < n_entries; ++i) {

		added = FALSE;

		/* The attributes */
		if (!egg_buffer_get_string (buffer, *offset, offset, &str, (EggBufferAllocator)g_realloc))
			return GKM_DATA_FAILURE;

		/* Make sure we have this one */
		sect = GPOINTER_TO_UINT (g_hash_table_lookup (self->identifiers, str));
		if (sect != section) {
			g_message ("data file entry in wrong section: %s", str);
			g_free (str);
			return GKM_DATA_FAILURE;
		}

		/* Lookup or create a new table for it */
		if (!g_hash_table_lookup_extended (entries, str, &key, &value)) {
			added = TRUE;
			value = attributes_new ();
			key = g_strdup (str);
			g_hash_table_replace (entries, key, value);
		}

		g_free (str);
		identifier = key;
		attributes = value;

		if (!egg_buffer_get_uint32 (buffer, *offset, offset, &n_attrs))
			return GKM_DATA_FAILURE;

		for (j = 0; j < n_attrs; ++j) {
			if (!egg_buffer_get_uint64 (buffer, *offset, offset, &type) ||
			    !egg_buffer_get_byte_array (buffer, *offset, offset, &data, &n_data))
				return GKM_DATA_FAILURE;

			attr.type = type;
			attr.pValue = (CK_VOID_PTR)data;
			attr.ulValueLen = n_data;

			at = g_hash_table_lookup (attributes, &attr.type);
			if (at != NULL && gkm_attribute_equal (&attr, at))
				continue;

			at = attribute_dup (&attr);
			g_hash_table_replace (attributes, &(at->type), at);

			/* Only emit the changed signal if we haven't just added this one */
			if (added == FALSE)
				g_signal_emit (self, signals[ENTRY_CHANGED], 0, identifier, attr.type);
		}

		/* A new entry was loaded */
		if (added == TRUE)
			g_signal_emit (self, signals[ENTRY_ADDED], 0, identifier);
	}

	return GKM_DATA_SUCCESS;
}

static GkmDataResult
update_from_public_block (GkmDataFile *self, EggBuffer *buffer)
{
	gsize offset = 0;

	g_assert (GKM_IS_DATA_FILE (self));
	g_assert (buffer);

	self->sections |= GKM_DATA_FILE_SECTION_PUBLIC;

	/* Validate the buffer hash, failure in this case is corruption */
	if (!validate_buffer (buffer, &offset))
		return GKM_DATA_FAILURE;

	return update_entries_from_block (self, GKM_DATA_FILE_SECTION_PUBLIC,
	                                  self->publics, buffer, &offset);
}

static GkmDataResult
update_from_private_block (GkmDataFile *self, EggBuffer *buffer, GkmSecret *login)
{
	EggBuffer custom;
	GkmDataResult res;
	const gchar *password;
	gsize n_password;
	gsize offset;

	g_assert (GKM_IS_DATA_FILE (self));
	g_assert (buffer);

	self->sections |= GKM_DATA_FILE_SECTION_PRIVATE;

	/* Skip private blocks when not unlocked */
	if (login == NULL) {
		if (self->privates)
			g_hash_table_destroy (self->privates);
		self->privates = NULL;
		return GKM_DATA_UNRECOGNIZED;
	}

	offset = 0;
	egg_buffer_init_full (&custom, 1024, egg_secure_realloc);

	/* Decrypt the buffer */
	password = gkm_secret_get_password (login, &n_password);
	if (!decrypt_buffer (buffer, &offset, login, &custom)) {
		egg_buffer_uninit (&custom);
		return GKM_DATA_FAILURE;
	}

	offset = 0;

	/* Validate the buffer hash, failure is usually a bad password */
	if (!validate_buffer (&custom, &offset)) {
		egg_buffer_uninit (&custom);
		return GKM_DATA_LOCKED;
	}

	/* We're loading privates, so fill that in */
	if (!self->privates)
		self->privates = entries_new ();

	res = update_entries_from_block (self, GKM_DATA_FILE_SECTION_PRIVATE,
	                                 self->privates, &custom, &offset);
	egg_buffer_uninit (&custom);
	return res;
}

static void
copy_each_identifier (gpointer key, gpointer value, gpointer data)
{
	g_hash_table_insert (data, g_strdup (key), UNUSED_VALUE);
}

static void
remove_each_identifier (gpointer key, gpointer value, gpointer data)
{
	GkmDataFile *self = GKM_DATA_FILE (data);
	GHashTable *entries;
	guint section;

	g_assert (GKM_IS_DATA_FILE (self));
	g_assert (key);

	if (!gkm_data_file_lookup_entry (self, key, &section))
		g_assert_not_reached ();

	if (section == GKM_DATA_FILE_SECTION_PRIVATE)
		entries = self->privates;
	else
		entries = self->publics;

	if (!g_hash_table_remove (self->identifiers, key))
		g_assert_not_reached ();

	if (entries != NULL) {
		if (!g_hash_table_remove (entries, key))
			g_return_if_reached ();

		/*
		 * Note that we only fire the removed signal when the identifier
		 * was accessible. We don't fire removed for private items in
		 * a locked file.
		 */
		g_signal_emit (self, signals[ENTRY_REMOVED], 0, key);
	}
}

static GkmDataResult
update_from_index_block (GkmDataFile *self, EggBuffer *buffer)
{
	gchar *identifier;
	gsize offset;
	guint section;
	guint count, i;
	guint value;

	g_assert (GKM_IS_DATA_FILE (self));
	g_assert (buffer);

	offset = 0;

	/* The number of entries */
	if (!egg_buffer_get_uint32 (buffer, offset, &offset, &count))
		return FALSE;

	for (i = 0; i < count; ++i) {

		/* The identifier */
		if (!egg_buffer_get_string (buffer, offset, &offset, &identifier, (EggBufferAllocator)g_realloc))
			break;

		/* The section */
		if (!egg_buffer_get_uint32 (buffer, offset, &offset, &value)) {
			g_free (identifier);
			break;
		}

		section = value;
		g_hash_table_replace (self->identifiers, identifier, GUINT_TO_POINTER (section));

		/* Track that we've seen this identifier */
		g_hash_table_remove (self->checks, identifier);
	}

	/* Completed reading all */
	if (i == count)
		return GKM_DATA_SUCCESS;

	/* Failed for some reason, data is bad */
	return GKM_DATA_FAILURE;
}

static GkmDataResult
update_from_any_block (guint block, EggBuffer *buffer, GkmSecret *login, gpointer user_data)
{
	UnknownBlock *unknown;
	GkmDataFile *self;
	GkmDataResult res;

	g_assert (GKM_IS_DATA_FILE (user_data));
	self = GKM_DATA_FILE (user_data);

	switch (block) {
	case FILE_BLOCK_INDEX:
		res = update_from_index_block (self, buffer);
		break;
	case FILE_BLOCK_PRIVATE:
		res = update_from_private_block (self, buffer, login);
		break;
	case FILE_BLOCK_PUBLIC:
		res = update_from_public_block (self, buffer);
		break;
	default:
		res = GKM_DATA_UNRECOGNIZED;
		break;
	};

	/* If unrecognized data block, then stash as unknown */
	if (res == GKM_DATA_UNRECOGNIZED) {
		unknown = g_slice_new0 (UnknownBlock);
		unknown->type = block;
		egg_buffer_init_full (&unknown->buffer, buffer->len, PUBLIC_ALLOC);
		egg_buffer_append (&unknown->buffer, buffer->buf, buffer->len);
		self->unknowns = g_list_prepend (self->unknowns, unknown);
		res = GKM_DATA_SUCCESS;
	}

	return res;
}

static void
write_each_attribute (gpointer key, gpointer value, gpointer data)
{
	CK_ATTRIBUTE_PTR attr = value;
	EggBuffer *buffer = data;
	egg_buffer_add_uint64 (buffer, attr->type);
	g_assert (attr->ulValueLen != (gulong)-1);
	egg_buffer_add_byte_array (buffer, attr->pValue, attr->ulValueLen);
}

static void
write_each_entry (gpointer key, gpointer value, gpointer data)
{
	EggBuffer *buffer = data;
	const gchar *unique = key;
	GHashTable *attributes = value;

	egg_buffer_add_string (buffer, unique);
	egg_buffer_add_uint32 (buffer, g_hash_table_size (attributes));
	g_hash_table_foreach (attributes, write_each_attribute, buffer);
}

static GkmDataResult
write_entries_to_block (GkmDataFile *self, GHashTable *entries, EggBuffer *buffer)
{
	gsize offset;

	g_assert (GKM_DATA_FILE (self));
	g_assert (entries);
	g_assert (buffer);

	/* Reserve space for the length */
	offset = buffer->len;
	egg_buffer_add_uint32 (buffer, 0);

	/* The number of attributes we'll be encountering */
	egg_buffer_add_uint32 (buffer, g_hash_table_size (entries));

	/* Fill in the attributes */
	g_hash_table_foreach (entries, write_each_entry, buffer);

	g_return_val_if_fail (!egg_buffer_has_error (buffer), GKM_DATA_FAILURE);

	/* Fill in the length */
	egg_buffer_set_uint32 (buffer, offset, buffer->len);

	/* Hash the entire dealio */
	if (!hash_buffer (buffer))
		return GKM_DATA_FAILURE;

	return GKM_DATA_SUCCESS;
}

static GkmDataResult
write_private_to_block (GkmDataFile *self, EggBuffer *buffer, GkmSecret *login)
{
	EggBuffer secure;
	GkmDataResult res;

	g_assert (GKM_IS_DATA_FILE (self));
	g_assert (buffer);

	if (login == NULL) {
		/* Must lock the private data in some way */
		if (self->privates && g_hash_table_size (self->privates))
			return GKM_DATA_LOCKED;

		/* Not storing privates */
		else
			return GKM_DATA_UNRECOGNIZED;
	} else {
		/* We didn't load the privates, can't store them back */
		if (self->privates == NULL)
			return GKM_DATA_LOCKED;
	}

	egg_buffer_init_full (&secure, 1024, PRIVATE_ALLOC);

	res = write_entries_to_block (self, self->privates, &secure);
	if (res == GKM_DATA_SUCCESS)
		res = encrypt_buffer (&secure, login, buffer);

	egg_buffer_uninit (&secure);
	return res;
}

static GkmDataResult
write_public_to_block (GkmDataFile *self, EggBuffer *buffer)
{
	g_assert (GKM_IS_DATA_FILE (self));
	g_assert (buffer);

	return write_entries_to_block (self, self->publics, buffer);
}

static void
write_each_index_identifier (gpointer key, gpointer value, gpointer data)
{
	egg_buffer_add_string (data, key);
	egg_buffer_add_uint32 (data, GPOINTER_TO_UINT (value));
}

static GkmDataResult
write_index_to_block (GkmDataFile *self, EggBuffer *buffer)
{
	g_assert (GKM_IS_DATA_FILE (self));
	g_assert (buffer);

	/* The number of entries */
	egg_buffer_add_uint32 (buffer, g_hash_table_size (self->identifiers));

	/* Now write out all the entries */
	g_hash_table_foreach (self->identifiers, write_each_index_identifier, buffer);

	return egg_buffer_has_error (buffer) ? GKM_DATA_FAILURE : GKM_DATA_SUCCESS;
}

static GkmDataResult
identifier_to_attributes (GkmDataFile *self, const gchar *identifier, GHashTable **attributes)
{
	GHashTable *entries;
	gpointer value;
	guint section;

	g_assert (GKM_IS_DATA_FILE (self));
	g_assert (identifier);
	g_assert (attributes);

	if (!g_hash_table_lookup_extended (self->identifiers, identifier, NULL, &value))
		return GKM_DATA_UNRECOGNIZED;

	section = GPOINTER_TO_UINT (value);
	if (section == GKM_DATA_FILE_SECTION_PRIVATE)
		entries = self->privates;
	else
		entries = self->publics;

	if (entries == NULL)
		return GKM_DATA_LOCKED;

	*attributes = g_hash_table_lookup (entries, identifier);
	g_return_val_if_fail (*attributes, GKM_DATA_UNRECOGNIZED);

	return GKM_DATA_SUCCESS;
}

static void
free_unknown_block_list (GList *list)
{
	UnknownBlock *unknown;
	GList *l;

	for (l = list; l; l = g_list_next (l)) {
		unknown = l->data;
		g_assert (unknown);
		egg_buffer_uninit (&unknown->buffer);
		g_slice_free (UnknownBlock, unknown);
	}

	g_list_free (list);
}

static gint
sort_unknowns_by_type (gconstpointer a, gconstpointer b)
{
	const UnknownBlock *ua = a;
	const UnknownBlock *ub = b;

	g_assert (ua);
	g_assert (ub);

	if (ua->type == ub->type)
		return 0;

	return ua->type > ub->type ? 1 : -1;
}

typedef struct _ForeachArgs {
	GkmDataFile *self;
	GkmDataFileFunc func;
	gpointer user_data;
} ForeachArgs;

static void
foreach_identifier (gpointer key, gpointer value, gpointer data)
{
	ForeachArgs *args = data;
	g_assert (GKM_IS_DATA_FILE (args->self));
	(args->func) (args->self, key, args->user_data);
}

static void
dump_attributes (gpointer key, gpointer value, gpointer user_data)
{
	CK_ATTRIBUTE_PTR attr = value;
	gulong *type = key;
	gchar *text;

	g_assert (type);
	g_assert (value);

	if (attr->pValue == NULL)
		text = g_strdup ("NULL");
	else
		text = egg_hex_encode_full (attr->pValue, attr->ulValueLen, TRUE, ' ', 1);

	g_print ("\t0x%08x: %s\n", (guint)*type, text);
	g_free (text);
}

static void
dump_identifier_and_attributes (GkmDataFile *self, const gchar *identifier, gpointer user_data)
{
	GHashTable *attributes;
	guint section;

	g_assert (GKM_IS_DATA_FILE (self));

	if (!gkm_data_file_lookup_entry (self, identifier, &section))
		g_assert_not_reached ();

	if (GPOINTER_TO_UINT (user_data) == section) {
		g_print ("%s\n", identifier);
		if (identifier_to_attributes (self, identifier, &attributes) != GKM_DATA_SUCCESS)
			g_assert_not_reached ();
		g_hash_table_foreach (attributes, dump_attributes, NULL);
		g_print ("\n");
	}
}


/* -----------------------------------------------------------------------------
 * OBJECT
 */

static void
gkm_data_file_init (GkmDataFile *self)
{
	self->identifiers = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
	self->publics = entries_new ();
	self->privates = entries_new ();

	self->unknowns = NULL;

	self->checks = NULL;
}

static void
gkm_data_file_finalize (GObject *obj)
{
	GkmDataFile *self = GKM_DATA_FILE (obj);

	g_assert (self->identifiers);
	g_hash_table_destroy (self->identifiers);
	self->identifiers = NULL;

	g_assert (self->checks == NULL);

	g_assert (self->publics);
	g_hash_table_destroy (self->publics);
	self->publics = NULL;

	if (self->privates)
		g_hash_table_destroy (self->privates);
	self->privates = NULL;

	free_unknown_block_list (self->unknowns);
	self->unknowns = NULL;

	G_OBJECT_CLASS (gkm_data_file_parent_class)->finalize (obj);
}

static void
gkm_data_file_set_property (GObject *obj, guint prop_id, const GValue *value,
                                GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_data_file_get_property (GObject *obj, guint prop_id, GValue *value,
                                GParamSpec *pspec)
{
	switch (prop_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, prop_id, pspec);
		break;
	}
}

static void
gkm_data_file_class_init (GkmDataFileClass *klass)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->finalize = gkm_data_file_finalize;
	gobject_class->set_property = gkm_data_file_set_property;
	gobject_class->get_property = gkm_data_file_get_property;

	signals[ENTRY_ADDED] = g_signal_new ("entry-added", GKM_TYPE_DATA_FILE,
	                                G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GkmDataFileClass, entry_added),
	                                NULL, NULL, g_cclosure_marshal_VOID__STRING,
	                                G_TYPE_NONE, 1, G_TYPE_STRING);

	signals[ENTRY_CHANGED] = g_signal_new ("entry-changed", GKM_TYPE_DATA_FILE,
	                                G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GkmDataFileClass, entry_changed),
	                                NULL, NULL, gkm_marshal_VOID__STRING_ULONG,
	                                G_TYPE_NONE, 2, G_TYPE_STRING, G_TYPE_ULONG);

	signals[ENTRY_REMOVED] = g_signal_new ("entry-removed", GKM_TYPE_DATA_FILE,
	                                G_SIGNAL_RUN_FIRST, G_STRUCT_OFFSET (GkmDataFileClass, entry_removed),
	                                NULL, NULL, g_cclosure_marshal_VOID__STRING,
	                                G_TYPE_NONE, 1, G_TYPE_STRING);
}

/* -----------------------------------------------------------------------------
 * PUBLIC
 */

GkmDataFile*
gkm_data_file_new (void)
{
	return g_object_new (GKM_TYPE_DATA_FILE, NULL);
}

GkmDataResult
gkm_data_file_read_fd (GkmDataFile *self, int fd, GkmSecret *login)
{
	GkmDataResult res;

	g_return_val_if_fail (GKM_IS_DATA_FILE (self), GKM_DATA_FAILURE);

	/* Reads are not reentrant for a single data file */
	g_return_val_if_fail (self->checks == NULL, GKM_DATA_FAILURE);

	self->sections = 0;

	/* Free all the old unknowns */
	free_unknown_block_list (self->unknowns);
	self->unknowns = NULL;

	/* Setup a hash table to monitor the actual data read */
	self->checks = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
	g_hash_table_foreach (self->identifiers, copy_each_identifier, self->checks);

	res = parse_file_blocks (fd, update_from_any_block, login, self);
	if (res == GKM_DATA_SUCCESS) {

		/* Our last read was a success, can write */
		self->incomplete = FALSE;

		/* Remove the ones we didn't see */
		g_hash_table_foreach (self->checks, remove_each_identifier, self);

		/*
		 * There's a special where we've read a file without a private section.
		 * We should be ready to accept privates (and then lock them next
		 * time around).
		 */

		if (self->privates == NULL && !(self->sections & GKM_DATA_FILE_SECTION_PRIVATE))
			self->privates = entries_new ();

	/* Note that our last read failed */
	} else {
		self->incomplete = TRUE;
	}

	g_hash_table_destroy (self->checks);
	self->checks = NULL;

	return res;
}

GkmDataResult
gkm_data_file_write_fd (GkmDataFile *self, int fd, GkmSecret *login)
{
	guint types[3] = { FILE_BLOCK_INDEX, FILE_BLOCK_PRIVATE, FILE_BLOCK_PUBLIC };
	GList *unknowns, *unk;
	UnknownBlock *block;
	GkmDataResult res;
	EggBuffer buffer;
	guint type;
	gint i;

	g_return_val_if_fail (GKM_IS_DATA_FILE (self), GKM_DATA_FAILURE);
	g_return_val_if_fail (!self->incomplete, GKM_DATA_FAILURE);

	/* Write out the header */
	if (!write_all_bytes (fd, FILE_HEADER, FILE_HEADER_LEN))
		return GKM_DATA_FAILURE;

	unknowns = g_list_copy (self->unknowns);
	unknowns = g_list_sort (unknowns, sort_unknowns_by_type);
	egg_buffer_init_full (&buffer, 8192, PUBLIC_ALLOC);

	/*
	 * All blocks are written in sorted order by their block
	 * type. This includes unknown blocks.
	 */

	unk = unknowns;
	res = GKM_DATA_SUCCESS;

	for (i = 0; i < G_N_ELEMENTS(types); ++i) {
		type = types[i];

		/* Write out all the unknowns before this block */
		while (unk != NULL && res == GKM_DATA_SUCCESS) {
			block = (UnknownBlock*)unk->data;
			if (block->type > type)
				break;
			res = write_file_block (fd, block->type, &block->buffer);
			unk = g_list_next (unk);
		}

		if (res != GKM_DATA_SUCCESS)
			break;

		/* Prepare the block of this type */
		egg_buffer_reset (&buffer);
		switch (type) {
		case FILE_BLOCK_INDEX:
			res = write_index_to_block (self, &buffer);
			break;
		case FILE_BLOCK_PRIVATE:
			res = write_private_to_block (self, &buffer, login);
			break;
		case FILE_BLOCK_PUBLIC:
			res = write_public_to_block (self, &buffer);
			break;
		}

		/* Write it out, if we got anything */
		if (res == GKM_DATA_SUCCESS)
			res = write_file_block (fd, type, &buffer);
		else if (res == GKM_DATA_UNRECOGNIZED)
			res = GKM_DATA_SUCCESS;

		if (res != GKM_DATA_SUCCESS)
			break;
	}

	/* Write out all remaining unknowns */
	while (unk != NULL && res == GKM_DATA_SUCCESS) {
		block = (UnknownBlock*)unk->data;
		res = write_file_block (fd, block->type, &block->buffer);
		unk = g_list_next (unk);
	}

	g_list_free (unknowns);
	egg_buffer_uninit (&buffer);
	return res;
}

gboolean
gkm_data_file_lookup_entry (GkmDataFile *self, const gchar *identifier, guint *section)
{
	gpointer value;

	g_return_val_if_fail (GKM_IS_DATA_FILE (self), FALSE);
	g_return_val_if_fail (identifier, FALSE);

	if (!g_hash_table_lookup_extended (self->identifiers, identifier, NULL, &value))
		return FALSE;

	if (section != NULL)
		*section = GPOINTER_TO_UINT (value);

	return TRUE;
}

void
gkm_data_file_foreach_entry (GkmDataFile *self, GkmDataFileFunc func, gpointer user_data)
{
	ForeachArgs args = { self, func, user_data };

	g_return_if_fail (GKM_IS_DATA_FILE (self));
	g_return_if_fail (func);

	g_hash_table_foreach (self->identifiers, foreach_identifier, &args);
}

GkmDataResult
gkm_data_file_unique_entry (GkmDataFile *self, gchar **identifier)
{
	gchar *base, *ext;
	guint seed = 1;

	g_return_val_if_fail (GKM_IS_DATA_FILE (self), GKM_DATA_FAILURE);
	g_return_val_if_fail (identifier, GKM_DATA_FAILURE);

	/* Check if original is unique */
	if (*identifier != NULL) {
		if (!gkm_data_file_lookup_entry (self, *identifier, NULL))
			return GKM_DATA_SUCCESS;
	}

	if (*identifier == NULL)
		*identifier = g_strdup_printf ("object-%08x", ABS (g_random_int ()));

	/* Take ownership of the identifier, and out an extension */
	base = *identifier;
	*identifier = NULL;
	ext = strrchr (base, '.');
	if (ext != NULL)
		*(ext++) = '\0';

	for (seed = 0; TRUE; ++seed) {
		*identifier = g_strdup_printf ("%s-%d%s%s", base, seed, ext ? "." : "", ext ? ext : "");
		if (!gkm_data_file_lookup_entry (self, *identifier, NULL))
			break;

		if (seed < 1000000) {
			g_warning ("couldn't find a unique identifier in a %d tries", seed);
			g_free (base);
			return GKM_DATA_FAILURE;
		}

		g_free (*identifier);
		*identifier = NULL;
	}

	g_free (base);
	return GKM_DATA_SUCCESS;
}

GkmDataResult
gkm_data_file_create_entry (GkmDataFile *self, const gchar *identifier, guint section)
{
	GHashTable *attributes;
	GHashTable *entries;

	g_return_val_if_fail (GKM_IS_DATA_FILE (self), GKM_DATA_FAILURE);
	g_return_val_if_fail (identifier, GKM_DATA_FAILURE);

	if (section == GKM_DATA_FILE_SECTION_PRIVATE) {
		if (!self->privates)
			return GKM_DATA_LOCKED;
		entries = self->privates;
	} else {
		entries = self->publics;
	}

	/* Make sure it's not already here */
	g_return_val_if_fail (g_hash_table_lookup (entries, identifier) == NULL, GKM_DATA_FAILURE);

	/* Add the new entry to the appropriate table */
	attributes = attributes_new ();
	g_hash_table_replace (entries, g_strdup (identifier), attributes);
	g_hash_table_replace (self->identifiers, g_strdup (identifier), GUINT_TO_POINTER (section));

	g_signal_emit (self, signals[ENTRY_ADDED], 0, identifier);
	return GKM_DATA_SUCCESS;
}

GkmDataResult
gkm_data_file_destroy_entry (GkmDataFile *self, const gchar *identifier)
{
	GHashTable *entries;
	guint section;

	g_return_val_if_fail (GKM_IS_DATA_FILE (self), GKM_DATA_FAILURE);
	g_return_val_if_fail (identifier, GKM_DATA_FAILURE);

	if (!gkm_data_file_lookup_entry (self, identifier, &section))
		return GKM_DATA_UNRECOGNIZED;

	if (section == GKM_DATA_FILE_SECTION_PRIVATE) {
		if (!self->privates)
			return GKM_DATA_LOCKED;
		entries = self->privates;
	} else {
		entries = self->publics;
	}

	if (!g_hash_table_remove (self->identifiers, identifier))
		g_return_val_if_reached (GKM_DATA_UNRECOGNIZED);
	if (!g_hash_table_remove (entries, identifier))
		g_return_val_if_reached (GKM_DATA_UNRECOGNIZED);

	g_signal_emit (self, signals[ENTRY_REMOVED], 0, identifier);
	return GKM_DATA_SUCCESS;
}

GkmDataResult
gkm_data_file_write_value (GkmDataFile *self, const gchar *identifier,
                           gulong type, gconstpointer value, gsize n_value)
{
	GHashTable *attributes;
	CK_ATTRIBUTE_PTR at;
	CK_ATTRIBUTE attr;
	GkmDataResult res;

	g_return_val_if_fail (GKM_IS_DATA_FILE (self), GKM_DATA_FAILURE);
	g_return_val_if_fail (identifier, GKM_DATA_FAILURE);
	g_return_val_if_fail (value || !n_value, GKM_DATA_FAILURE);

	/* Find the right set of attributes */
	res = identifier_to_attributes (self, identifier, &attributes);
	if (res != GKM_DATA_SUCCESS)
		return res;

	attr.type = type;
	attr.pValue = (void*)value;
	attr.ulValueLen = n_value;

	at = g_hash_table_lookup (attributes, &type);
	if (at != NULL && gkm_attribute_equal (at, &attr))
		return GKM_DATA_SUCCESS;

	at = attribute_dup (&attr);
	g_hash_table_replace (attributes, &(at->type), at);

	g_signal_emit (self, signals[ENTRY_CHANGED], 0, identifier, type);
	return GKM_DATA_SUCCESS;
}

GkmDataResult
gkm_data_file_read_value (GkmDataFile *self, const gchar *identifier,
                          gulong type, gconstpointer *value, gsize *n_value)
{
	CK_ATTRIBUTE_PTR attr;
	GHashTable *attributes;
	GkmDataResult res;

	g_return_val_if_fail (GKM_IS_DATA_FILE (self), GKM_DATA_FAILURE);
	g_return_val_if_fail (identifier, GKM_DATA_FAILURE);
	g_return_val_if_fail (value, GKM_DATA_FAILURE);
	g_return_val_if_fail (n_value, GKM_DATA_FAILURE);

	/* Find the right set of attributes */
	res = identifier_to_attributes (self, identifier, &attributes);
	if (res != GKM_DATA_SUCCESS)
		return res;

	attr = g_hash_table_lookup (attributes, &type);
	if (attr == NULL)
		return GKM_DATA_UNRECOGNIZED;

	g_assert (attr->type == type);
	*value = attr->pValue;
	*n_value = attr->ulValueLen;
	return GKM_DATA_SUCCESS;
}

gboolean
gkm_data_file_have_section (GkmDataFile *self, guint section)
{
	return (self->sections & section) ? TRUE : FALSE;
}

void
gkm_data_file_dump (GkmDataFile *self)
{
	g_print ("PUBLIC:\n\n");
	gkm_data_file_foreach_entry (self, dump_identifier_and_attributes,
	                             GUINT_TO_POINTER (GKM_DATA_FILE_SECTION_PUBLIC));
	g_print ("PRIVATE:\n\n");
	gkm_data_file_foreach_entry (self, dump_identifier_and_attributes,
	                             GUINT_TO_POINTER (GKM_DATA_FILE_SECTION_PRIVATE));
}
