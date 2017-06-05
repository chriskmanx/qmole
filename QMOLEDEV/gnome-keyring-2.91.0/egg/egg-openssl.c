/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-openssl.c - OpenSSL compatibility functionality

   Copyright (C) 2007 Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#include "config.h"

#include "egg-hex.h"
#include "egg-openssl.h"
#include "egg-secure-memory.h"
#include "egg-symkey.h"

#include <gcrypt.h>

#include <glib.h>

#include <ctype.h>
#include <string.h>

/* 
 * PEM looks like:
 * 
 * 	-----BEGIN RSA PRIVATE KEY-----
 * 	Proc-Type: 4,ENCRYPTED
 * 	DEK-Info: DES-EDE3-CBC,704CFFD62FBA03E9
 * 
 * 	4AV/g0BiTeb07hzo4/Ct47HGhHEshMhBPGJ843QzuAinpZBbg3OxwPsQsLgoPhJL
 * 	Bg6Oxyz9M4UN1Xlx6Lyo2lRT908mBP6dl/OItLsVArqAzM+e29KHQVNjV1h7xN9F
 *	u84tOgZftKun+ZkQUOoRvMLLu4yV4CUraks9tgyXquugGba/tbeyj2MYsC8wwSJX
 * 	....
 * 	-----END RSA PRIVATE KEY-----
 */
 
#define PEM_SUFF          "-----"
#define PEM_SUFF_L        5
#define PEM_PREF_BEGIN    "-----BEGIN "
#define PEM_PREF_BEGIN_L  11
#define PEM_PREF_END      "-----END "
#define PEM_PREF_END_L    9

static void
parse_header_lines (const gchar *hbeg, const gchar *hend, GHashTable **result)
{
	gchar **lines, **l;
	gchar *line, *name, *value;
	gchar *copy;
	
	copy = g_strndup (hbeg, hend - hbeg);
	lines = g_strsplit (copy, "\n", 0);
	g_free (copy);
	
	for (l = lines; l && *l; ++l) {
		line = *l;
		g_strstrip (line);
		
        	/* Look for the break between name: value */
        	value = strchr (line, ':');
        	if (value == NULL)
        		continue;
        		
        	*value = 0;
        	value = g_strdup (value + 1);
        	g_strstrip (value);
        	
        	name = g_strdup (line);
        	g_strstrip (name);
        	
        	if (!*result)
        		*result = egg_openssl_headers_new ();
        	g_hash_table_replace (*result, name, value);
	}

	g_strfreev (lines);
} 

static const gchar*
pem_find_begin (const gchar *data, gsize n_data, GQuark *type)
{
	const gchar *pref, *suff;
	gchar *stype;
	
	/* Look for a prefix */
	pref = g_strstr_len ((gchar*)data, n_data, PEM_PREF_BEGIN);
	if (!pref)
		return NULL;
		
	n_data -= (pref - data) + PEM_PREF_BEGIN_L;
	data = pref + PEM_PREF_BEGIN_L;
		
	/* Look for the end of that begin */
	suff = g_strstr_len ((gchar*)data, n_data, PEM_SUFF);
	if (!suff)
		return NULL;
		
	/* Make sure on the same line */
	if (memchr (pref, '\n', suff - pref))
		return NULL;
		
	if (type) {
		*type = 0;
		pref += PEM_PREF_BEGIN_L;
		g_assert (suff > pref);
		stype = g_alloca (suff - pref + 1);
		memcpy (stype, pref, suff - pref);
		stype[suff - pref] = 0;
		*type = g_quark_from_string (stype);
	} 
	
	/* The byte after this ---BEGIN--- */
	return suff + PEM_SUFF_L;
}

static const gchar*
pem_find_end (const gchar *data, gsize n_data, GQuark type)
{
	const gchar *stype;
	const gchar *pref;
	gsize n_type;
	
	/* Look for a prefix */
	pref = g_strstr_len (data, n_data, PEM_PREF_END);
	if (!pref)
		return NULL;
		
	n_data -= (pref - data) + PEM_PREF_END_L;
	data = pref + PEM_PREF_END_L;
	
	/* Next comes the type string */
	stype = g_quark_to_string (type);
	n_type = strlen (stype);
	if (strncmp ((gchar*)data, stype, n_type) != 0)
		return NULL; 
		
	n_data -= n_type;
	data += n_type;
	
	/* Next comes the suffix */
	if (strncmp ((gchar*)data, PEM_SUFF, PEM_SUFF_L) != 0)
		return NULL;
		
	/* The beginning of this ---END--- */
	return pref;
}

static gboolean
pem_parse_block (const gchar *data, gsize n_data, guchar **decoded, gsize *n_decoded,
                 GHashTable **headers)
{
	const gchar *x, *hbeg, *hend;
	const gchar *p, *end;
	gint state = 0;
	guint save = 0;
	
	g_assert (data);
	g_assert (n_data);
	
	g_assert (decoded);
	g_assert (n_decoded);
	
	p = data;
	end = p + n_data;
	
	hbeg = hend = NULL;
	
	/* Try and find a pair of blank lines with only white space between */
	while (hend == NULL) {
		x = memchr (p, '\n', end - p);
		if (!x)
			break;
		++x;
		while (isspace (*x)) {
			/* Found a second line, with only spaces between */
			if (*x == '\n') {
				hbeg = data;
				hend = x;
				break;
			/* Found a space between two lines */
			} else {
				++x;
			}
		}
		
		/* Try next line */
		p = x;
	}

	/* Headers found? */	
	if (hbeg && hend) {
		data = hend;
		n_data = end - data;
	}
	
	*n_decoded = (n_data * 3) / 4 + 1;
	if (egg_secure_check (data))
		*decoded = egg_secure_alloc (*n_decoded);
	else
		*decoded = g_malloc0 (*n_decoded);
	g_return_val_if_fail (*decoded, FALSE);
	
	*n_decoded = g_base64_decode_step (data, n_data, *decoded, &state, &save);
	if (!*n_decoded) {
		egg_secure_free (*decoded);
		return FALSE;
	}
	
	if (headers && hbeg && hend) 
		parse_header_lines (hbeg, hend, headers);
	
	return TRUE;
}

GHashTable*
egg_openssl_headers_new (void)
{
	return g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
}

guint
egg_openssl_pem_parse (const guchar *data, gsize n_data, 
                       EggOpensslPemCallback callback, gpointer user_data)
{
	const gchar *beg, *end;
	guint nfound = 0;
	guchar *decoded = NULL;
	gsize n_decoded = 0;
	GHashTable *headers = NULL;
	GQuark type;
	
	g_return_val_if_fail (data, 0);
	g_return_val_if_fail (n_data, 0);
	g_return_val_if_fail (callback, 0);

	while (n_data > 0) {
		
		/* This returns the first character after the PEM BEGIN header */
		beg = pem_find_begin ((const gchar*)data, n_data, &type);
		if (!beg)
			break;
			
		g_assert (type);
		
		/* This returns the character position before the PEM END header */
		end = pem_find_end ((const gchar*)beg, n_data - ((const guchar*)beg - data), type);
		if (!end)
			break;

		if (beg != end) {
			if (pem_parse_block (beg, end - beg, &decoded, &n_decoded, &headers)) {
				(callback) (type, decoded, n_decoded, headers, user_data);
				++nfound;
				egg_secure_free (decoded);
				if (headers)
					g_hash_table_remove_all (headers);
			}
		}
                     
		/* Try for another block */
		end += PEM_SUFF_L;
		n_data -= (const guchar*)end - data; 
		data = (const guchar*)end;
	}
	
	if (headers)
		g_hash_table_destroy (headers);

	return nfound;
}

#ifdef UNTESTED_CODE

static void 
append_each_header (gpointer key, gpointer value, gpointer user_data)
{
	GString *string = (GString*)user_data;
	
	g_string_append (string, (gchar*)key);
	g_string_append (string, ": ");
	g_string_append (string, (gchar*)value);
	g_string_append_c (string, '\n');
}

guchar*
egg_openssl_pem_write (const guchar *data, gsize n_data, GQuark type, 
                    GHashTable *headers, gsize *n_result)
{
	GString *string;
	gint state, save;
	gsize length, n_prefix;
	
	g_return_val_if_fail (data || !n_data, NULL);
	g_return_val_if_fail (type, NULL);
	g_return_val_if_fail (n_result, NULL);

	string = g_string_sized_new (4096);
	
	/* The prefix */
	g_string_append_len (string, PEM_PREF_BEGIN, PEM_PREF_BEGIN_L);
	g_string_append (string, g_quark_to_string (type));
	g_string_append_len (string, PEM_SUFF, PEM_SUFF_L);
	g_string_append_c (string, '\n');
	
	/* The headers */
	if (headers && g_hash_table_size (headers) > 0) {
		g_hash_table_foreach (headers, append_each_header, string);
		g_string_append_c (string, '\n');
	}

	/* Resize string to fit the base64 data. Algorithm from Glib reference */
	length = n_data * 4 / 3 + n_data * 4 / (3 * 72) + 7;
	n_prefix = string->len;
	g_string_set_size (string, n_prefix + length);
	
	/* The actual base64 data */
	state = save = 0;
	length = g_base64_encode_step (data, n_data, TRUE, 
	                               string->str + string->len, &state, &save);
	g_string_set_size (string, n_prefix + length);
	
	/* The suffix */
	g_string_append_c (string, '\n');
	g_string_append_len (string, PEM_PREF_END, PEM_PREF_END_L);
	g_string_append (string, g_quark_to_string (type));
	g_string_append_len (string, PEM_SUFF, PEM_SUFF_L);
	g_string_append_c (string, '\n');
	
	*n_result = string->len;
	return (guchar*)g_string_free (string, FALSE);
}

#endif /* UNTESTED_CODE */

/* ----------------------------------------------------------------------------
 * DEFINITIONS
 */

static const struct {
	const gchar *desc;
	int algo;
	int mode;
} openssl_algos[] = {
	{ "DES-ECB", GCRY_CIPHER_DES, GCRY_CIPHER_MODE_ECB },
	{ "DES-CFB64", GCRY_CIPHER_DES, GCRY_CIPHER_MODE_CFB },
	{ "DES-CFB", GCRY_CIPHER_DES, GCRY_CIPHER_MODE_CFB },
	/* DES-CFB1 */
	/* DES-CFB8 */
	/* DESX-CBC */
	/* DES-EDE */
	/* DES-EDE-CBC */
	/* DES-EDE-ECB */
	/* DES-EDE-CFB64 DES-EDE-CFB */
	/* DES-EDE-CFB1 */
	/* DES-EDE-CFB8 */
	/* DES-EDE-OFB */
	/* DES-EDE3 */ 
	{ "DES-EDE3-ECB", GCRY_CIPHER_3DES, GCRY_CIPHER_MODE_ECB }, 
	{ "DES-EDE3-CFB64", GCRY_CIPHER_3DES, GCRY_CIPHER_MODE_CFB },
	{ "DES-EDE3-CFB", GCRY_CIPHER_3DES, GCRY_CIPHER_MODE_CFB },
	/* DES-EDE3-CFB1 */
	/* DES-EDE3-CFB8 */
	{ "DES-OFB", GCRY_CIPHER_DES, GCRY_CIPHER_MODE_OFB },
	{ "DES-EDE3-OFB", GCRY_CIPHER_3DES, GCRY_CIPHER_MODE_OFB },
	{ "DES-CBC", GCRY_CIPHER_DES, GCRY_CIPHER_MODE_CBC },
	{ "DES-EDE3-CBC", GCRY_CIPHER_3DES, GCRY_CIPHER_MODE_CBC },
	/* RC2-ECB */
	/* RC2-CBC */
	/* RC2-40-CBC */
	/* RC2-64-CBC */
	/* RC2-CFB64    RC2-CFB */
	/* RC2-OFB */
	{ "RC4", GCRY_CIPHER_ARCFOUR, GCRY_CIPHER_MODE_STREAM },
	{ "RC4-40", GCRY_CIPHER_ARCFOUR, GCRY_CIPHER_MODE_STREAM },
	{ "IDEA-ECB", GCRY_CIPHER_IDEA, GCRY_CIPHER_MODE_ECB },
	{ "IDEA-CFB64", GCRY_CIPHER_IDEA, GCRY_CIPHER_MODE_CFB },
	{ "IDEA-OFB", GCRY_CIPHER_IDEA, GCRY_CIPHER_MODE_OFB },
	{ "IDEA-CBC", GCRY_CIPHER_IDEA, GCRY_CIPHER_MODE_CBC },
	{ "BF-ECB", GCRY_CIPHER_BLOWFISH, GCRY_CIPHER_MODE_ECB },
	{ "BF-CBC", GCRY_CIPHER_BLOWFISH, GCRY_CIPHER_MODE_CBC },
	{ "BF-CFB64", GCRY_CIPHER_BLOWFISH, GCRY_CIPHER_MODE_CFB },
	{ "BF-CFB", GCRY_CIPHER_BLOWFISH, GCRY_CIPHER_MODE_CFB },
	{ "BF-OFB", GCRY_CIPHER_BLOWFISH, GCRY_CIPHER_MODE_OFB },
	{ "CAST5-ECB", GCRY_CIPHER_CAST5, GCRY_CIPHER_MODE_ECB },
	{ "CAST5-CBC", GCRY_CIPHER_CAST5, GCRY_CIPHER_MODE_CBC },
	{ "CAST5-CFB64", GCRY_CIPHER_CAST5, GCRY_CIPHER_MODE_CFB },
	{ "CAST5-CFB", GCRY_CIPHER_CAST5, GCRY_CIPHER_MODE_CFB },
	{ "CAST5-OFB", GCRY_CIPHER_CAST5, GCRY_CIPHER_MODE_OFB },
	/* RC5-32-12-16-CBC */ 
	/* RC5-32-12-16-ECB */
	/* RC5-32-12-16-CFB64  RC5-32-12-16-CFB */
	/* RC5-32-12-16-OFB */
	{ "AES-128-ECB", GCRY_CIPHER_AES128, GCRY_CIPHER_MODE_ECB },
	{ "AES-128-CBC", GCRY_CIPHER_AES128, GCRY_CIPHER_MODE_CBC },
	/* AES-128-CFB1 */
	/* AES-128-CFB8	*/
	{ "AES-128-CFB128", GCRY_CIPHER_AES128, GCRY_CIPHER_MODE_CFB },
	{ "AES-128-CFB", GCRY_CIPHER_AES128, GCRY_CIPHER_MODE_CFB },
	{ "AES-128-OFB", GCRY_CIPHER_AES128, GCRY_CIPHER_MODE_OFB },
	{ "AES-128-CTR", GCRY_CIPHER_AES128, GCRY_CIPHER_MODE_CTR },
	{ "AES-192-ECB", GCRY_CIPHER_AES192, GCRY_CIPHER_MODE_ECB },
	{ "AES-192-CBC", GCRY_CIPHER_AES192, GCRY_CIPHER_MODE_CBC },
	/* AES-192-CFB1 */
	/* AES-192-CFB8 */
	{ "AES-192-CFB128", GCRY_CIPHER_AES192, GCRY_CIPHER_MODE_CFB },
	{ "AES-192-CFB", GCRY_CIPHER_AES192, GCRY_CIPHER_MODE_CFB },
	{ "AES-192-OFB", GCRY_CIPHER_AES192, GCRY_CIPHER_MODE_OFB },
	{ "AES-192-CTR", GCRY_CIPHER_AES192, GCRY_CIPHER_MODE_CTR },
	{ "AES-256-ECB", GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_ECB },
	{ "AES-256-CBC", GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_CBC },
	/* AES-256-CFB1 */
	/* AES-256-CFB8 */
	{ "AES-256-CFB128", GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_CFB },
	{ "AES-256-CFB", GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_CFB },
	{ "AES-256-OFB", GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_OFB },
	{ "AES-256-CTR", GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_CTR },
	/* CAMELLIA-128-ECB */
	/* CAMELLIA-128-CBC */
	/* CAMELLIA-128-CFB1 */
	/* CAMELLIA-128-CFB8 */
	/* CAMELLIA-128-CFB128   CAMELLIA-128-CFB */
	/* CAMELLIA-128-OFB */
	/* CAMELLIA-192-ECB */
	/* CAMELLIA-192-CBC */
	/* CAMELLIA-192-CFB1 */
	/* CAMELLIA-192-CFB8 */
	/* CAMELLIA-192-CFB128   CAMELLIA-192-CFB */
	/* CAMELLIA-192_OFB */
	/* CAMELLIA-256-ECB */
	/* CAMELLIA-256-CBC */
	/* CAMELLIA-256-CFB1 */
	/* CAMELLIA-256-CFB8 */
	/* CAMELLIA-256-CFB128   CAMELLIA-256-CFB */
	/* CAMELLIA-256-OFB */
};

/* ------------------------------------------------------------------------- */

int
egg_openssl_parse_algo (const char *name, int *mode)
{
	static GQuark openssl_quarks[G_N_ELEMENTS(openssl_algos)] = { 0, };
	static gsize openssl_quarks_inited = 0;
	GQuark q;
	int i;

	if (g_once_init_enter (&openssl_quarks_inited)) {
		for (i = 0; i < G_N_ELEMENTS(openssl_algos); ++i)
			openssl_quarks[i] = g_quark_from_static_string (openssl_algos[i].desc);
		g_once_init_leave (&openssl_quarks_inited, 1);
	}
	
	q = g_quark_try_string (name);
	if (q) {
		for (i = 0; i < G_N_ELEMENTS(openssl_algos); ++i) {
			if (q == openssl_quarks[i]) {
				*mode = openssl_algos[i].mode;
				return openssl_algos[i].algo;
			}
		}
	}
	
	return 0;
}

static gboolean
parse_dekinfo (const gchar *dek, int *algo, int *mode, guchar **iv)
{
	gboolean success = FALSE;
	gchar **parts = NULL;
	gcry_error_t gcry;
	gsize ivlen, len;
	
	parts = g_strsplit (dek, ",", 2);
	if (!parts || !parts[0] || !parts[1]) 
		goto done;
		
	/* Parse the algorithm name */
	*algo = egg_openssl_parse_algo (parts[0], mode);
	if (!*algo)
		goto done;
	
	/* Make sure this is usable */
	gcry = gcry_cipher_test_algo (*algo);
	if (gcry)
		goto done;

	/* Parse the IV */
	ivlen = gcry_cipher_get_algo_blklen (*algo);
	
	*iv = egg_hex_decode (parts[1], strlen(parts[1]), &len);
	if (!*iv || ivlen != len) {
		g_free (*iv);
		goto done;
	}
		
	success = TRUE;

done:
	g_strfreev (parts);
	return success;
}

gboolean
egg_openssl_decrypt_block (const gchar *dekinfo, const gchar *password, 
                           gssize n_password, const guchar *data, gsize n_data, 
                           guchar **decrypted, gsize *n_decrypted)
{
	gcry_cipher_hd_t ch;
	guchar *key = NULL;
	guchar *iv = NULL;
	int gcry, ivlen;
	int algo = 0;
	int mode = 0;
	
	if (!parse_dekinfo (dekinfo, &algo, &mode, &iv))
		return FALSE;
		
	ivlen = gcry_cipher_get_algo_blklen (algo);

	/* We assume the iv is at least as long as at 8 byte salt */
	g_return_val_if_fail (ivlen >= 8, FALSE);
	
	/* IV is already set from the DEK info */
	if (!egg_symkey_generate_simple (algo, GCRY_MD_MD5, password, 
	                                        n_password, iv, 8, 1, &key, NULL)) {
		g_free (iv);
		return FALSE;
	}
	
	/* TODO: Use secure memory */
	gcry = gcry_cipher_open (&ch, algo, mode, 0);
	g_return_val_if_fail (!gcry, FALSE);
		
	gcry = gcry_cipher_setkey (ch, key, gcry_cipher_get_algo_keylen (algo));
	g_return_val_if_fail (!gcry, FALSE);
	egg_secure_free (key);

	/* 16 = 128 bits */
	gcry = gcry_cipher_setiv (ch, iv, ivlen);
	g_return_val_if_fail (!gcry, FALSE);
	g_free (iv);
	
	/* Allocate output area */
	*n_decrypted = n_data;
	*decrypted = egg_secure_alloc (n_data);

	gcry = gcry_cipher_decrypt (ch, *decrypted, *n_decrypted, (void*)data, n_data);
	if (gcry) {
		egg_secure_free (*decrypted);
		g_return_val_if_reached (FALSE);
	}
	
	gcry_cipher_close (ch);
	
	return TRUE;
}

gboolean
egg_openssl_encrypt_block (const gchar *dekinfo, const gchar *password, 
                                gssize n_password, const guchar *data, gsize n_data,
                                guchar **encrypted, gsize *n_encrypted)
{
	gsize n_overflow, n_batch, n_padding;
	gcry_cipher_hd_t ch;
	guchar *key = NULL;
	guchar *iv = NULL;
	guchar *padded = NULL;
	int gcry, ivlen;
	int algo = 0;
	int mode = 0;
	
	if (!parse_dekinfo (dekinfo, &algo, &mode, &iv))
		g_return_val_if_reached (FALSE);
		
	ivlen = gcry_cipher_get_algo_blklen (algo);

	/* We assume the iv is at least as long as at 8 byte salt */
	g_return_val_if_fail (ivlen >= 8, FALSE);
	
	/* IV is already set from the DEK info */
	if (!egg_symkey_generate_simple (algo, GCRY_MD_MD5, password, 
	                                        n_password, iv, 8, 1, &key, NULL))
		g_return_val_if_reached (FALSE);
	
	gcry = gcry_cipher_open (&ch, algo, mode, 0);
	g_return_val_if_fail (!gcry, FALSE);
		
	gcry = gcry_cipher_setkey (ch, key, gcry_cipher_get_algo_keylen (algo));
	g_return_val_if_fail (!gcry, FALSE);
	egg_secure_free (key);

	/* 16 = 128 bits */
	gcry = gcry_cipher_setiv (ch, iv, ivlen);
	g_return_val_if_fail (!gcry, FALSE);
	g_free (iv);
	
	/* Allocate output area */
	n_overflow = (n_data % ivlen);
	n_padding = n_overflow ? (ivlen - n_overflow) : 0;
	n_batch = n_data - n_overflow;
	*n_encrypted = n_data + n_padding;
	*encrypted = g_malloc0 (*n_encrypted);
	
	g_assert (*n_encrypted % ivlen == 0);
	g_assert (*n_encrypted >= n_data);
	g_assert (*n_encrypted == n_batch + n_overflow + n_padding);

	/* Encrypt everything but the last bit */
	gcry = gcry_cipher_encrypt (ch, *encrypted, n_batch, (void*)data, n_batch);
	if (gcry) {
		g_free (*encrypted);
		g_return_val_if_reached (FALSE);
	}
	
	/* Encrypt the padded block */
	if (n_overflow) {
		padded = egg_secure_alloc (ivlen);
		memset (padded, 0, ivlen);
		memcpy (padded, data + n_batch, n_overflow);
		gcry = gcry_cipher_encrypt (ch, *encrypted + n_batch, ivlen, padded, ivlen);
		egg_secure_free (padded);
		if (gcry) {
			g_free (*encrypted);
			g_return_val_if_reached (FALSE);
		}
	}

	gcry_cipher_close (ch);
	return TRUE;
}

const gchar*
egg_openssl_get_dekinfo (GHashTable *headers)
{
	const gchar *val;
	if (!headers)
		return NULL;
	val = g_hash_table_lookup (headers, "Proc-Type");
	if (!val || strcmp (val, "4,ENCRYPTED") != 0)
		return NULL;
	val = g_hash_table_lookup (headers, "DEK-Info");
	g_return_val_if_fail (val, NULL);
	return val;
}

const gchar*
egg_openssl_prep_dekinfo (GHashTable *headers)
{
	gchar *dekinfo, *hex;
	gsize ivlen;
	guchar *iv;
	
	/* Create the iv */
	ivlen = gcry_cipher_get_algo_blklen (GCRY_CIPHER_3DES);
	g_return_val_if_fail (ivlen, NULL);
	iv = g_malloc (ivlen);
	gcry_create_nonce (iv, ivlen);
	
	/* And encode it into the string */
	hex = egg_hex_encode (iv, ivlen);
	g_return_val_if_fail (hex, NULL);
	dekinfo = g_strdup_printf ("DES-EDE3-CBC,%s", hex);
	g_free (hex);

	g_hash_table_insert (headers, g_strdup ("DEK-Info"), (void*)dekinfo);
	g_hash_table_insert (headers, g_strdup ("Proc-Type"), g_strdup ("4,ENCRYPTED"));
	
	return dekinfo;
}
