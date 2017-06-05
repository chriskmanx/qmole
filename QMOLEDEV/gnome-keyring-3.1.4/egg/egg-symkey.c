/* 
 * gnome-keyring
 * 
 * Copyright (C) 2008 Stefan Walter
 * 
 * This program is free software; you can redistribute it and/or modify 
 * it under the terms of the GNU Lesser General  License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *  
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General  License for more details.
 *  
 * You should have received a copy of the GNU Lesser General 
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.  
 */

#include "config.h"

#include "egg-asn1-defs.h"
#include "egg-asn1x.h"
#include "egg-secure-memory.h"
#include "egg-symkey.h"

/* -----------------------------------------------------------------------------
 * QUARKS
 */

static GQuark OID_PBE_MD2_DES_CBC;
static GQuark OID_PBE_MD5_DES_CBC;
static GQuark OID_PBE_MD2_RC2_CBC;
static GQuark OID_PBE_MD5_RC2_CBC;
static GQuark OID_PBE_SHA1_DES_CBC;
static GQuark OID_PBE_SHA1_RC2_CBC;
static GQuark OID_PBES2;
static GQuark OID_PBKDF2;

static GQuark OID_DES_CBC;
static GQuark OID_DES_RC2_CBC;
static GQuark OID_DES_EDE3_CBC;
static GQuark OID_DES_RC5_CBC;

static GQuark OID_PKCS12_PBE_ARCFOUR_SHA1;
static GQuark OID_PKCS12_PBE_RC4_40_SHA1;
static GQuark OID_PKCS12_PBE_3DES_SHA1;
static GQuark OID_PKCS12_PBE_2DES_SHA1;
static GQuark OID_PKCS12_PBE_RC2_128_SHA1;
static GQuark OID_PKCS12_PBE_RC2_40_SHA1;

static void
init_quarks (void)
{
	static volatile gsize quarks_inited = 0;

	if (g_once_init_enter (&quarks_inited)) {

		#define QUARK(name, value) \
			name = g_quark_from_static_string(value)

		QUARK (OID_PBE_MD2_DES_CBC, "1.2.840.113549.1.5.1");
		QUARK (OID_PBE_MD5_DES_CBC, "1.2.840.113549.1.5.3");
		QUARK (OID_PBE_MD2_RC2_CBC, "1.2.840.113549.1.5.4");
		QUARK (OID_PBE_MD5_RC2_CBC, "1.2.840.113549.1.5.6");
		QUARK (OID_PBE_SHA1_DES_CBC, "1.2.840.113549.1.5.10");
		QUARK (OID_PBE_SHA1_RC2_CBC, "1.2.840.113549.1.5.11");
		
		QUARK (OID_PBES2, "1.2.840.113549.1.5.13");
		
		QUARK (OID_PBKDF2, "1.2.840.113549.1.5.12");
		
		QUARK (OID_DES_CBC, "1.3.14.3.2.7");
		QUARK (OID_DES_RC2_CBC, "1.2.840.113549.3.2");
		QUARK (OID_DES_EDE3_CBC, "1.2.840.113549.3.7");
		QUARK (OID_DES_RC5_CBC, "1.2.840.113549.3.9");
		
		QUARK (OID_PKCS12_PBE_ARCFOUR_SHA1, "1.2.840.113549.1.12.1.1");
		QUARK (OID_PKCS12_PBE_RC4_40_SHA1, "1.2.840.113549.1.12.1.2");
		QUARK (OID_PKCS12_PBE_3DES_SHA1, "1.2.840.113549.1.12.1.3");
		QUARK (OID_PKCS12_PBE_2DES_SHA1, "1.2.840.113549.1.12.1.4");
		QUARK (OID_PKCS12_PBE_RC2_128_SHA1, "1.2.840.113549.1.12.1.5");
		QUARK (OID_PKCS12_PBE_RC2_40_SHA1, "1.2.840.113549.1.12.1.6");
		
		#undef QUARK
		
		g_once_init_leave (&quarks_inited, 1);
	}
}

/* -----------------------------------------------------------------------------
 * PASSWORD TO KEY/IV
 */

gboolean
egg_symkey_generate_simple (int cipher_algo, int hash_algo, 
                            const gchar *password, gssize n_password, 
                            const guchar *salt, gsize n_salt, int iterations, 
                            guchar **key, guchar **iv)
{
	gcry_md_hd_t mdh;
	gcry_error_t gcry;
	guchar *digest;
	guchar *digested;
	guint n_digest;
	gint pass, i;
	gint needed_iv, needed_key;
	guchar *at_iv, *at_key;

	g_assert (cipher_algo);
	g_assert (hash_algo);

	g_return_val_if_fail (iterations >= 1, FALSE);
	
	if (!password)
		n_password = 0;
	if (n_password == -1)
		n_password = strlen (password);
	
	/* 
	 * If cipher algo needs more bytes than hash algo has available
	 * then the entire hashing process is done again (with the previous
	 * hash bytes as extra input), and so on until satisfied.
	 */ 
	
	needed_key = gcry_cipher_get_algo_keylen (cipher_algo);
	needed_iv = gcry_cipher_get_algo_blklen (cipher_algo);
	
	gcry = gcry_md_open (&mdh, hash_algo, 0);
	if (gcry) {
		g_warning ("couldn't create '%s' hash context: %s", 
			   gcry_md_algo_name (hash_algo), gcry_strerror (gcry));
		return FALSE;
	}

	n_digest = gcry_md_get_algo_dlen (hash_algo);
	g_return_val_if_fail (n_digest > 0, FALSE);
	
	digest = egg_secure_alloc (n_digest);
	g_return_val_if_fail (digest, FALSE);
	if (key) {
		*key = egg_secure_alloc (needed_key);
		g_return_val_if_fail (*key, FALSE);
	}
	if (iv) 
		*iv = g_new0 (guchar, needed_iv);

	at_key = key ? *key : NULL;
	at_iv = iv ? *iv : NULL;

	for (pass = 0; TRUE; ++pass) {
		gcry_md_reset (mdh);
		
		/* Hash in the previous buffer on later passes */
		if (pass > 0)
			gcry_md_write (mdh, digest, n_digest);

		if (password)
			gcry_md_write (mdh, password, n_password);
		if (salt && n_salt)
			gcry_md_write (mdh, salt, n_salt);
		gcry_md_final (mdh);
		digested = gcry_md_read (mdh, 0);
		g_return_val_if_fail (digested, FALSE);
		memcpy (digest, digested, n_digest);
		
		for (i = 1; i < iterations; ++i) {
			gcry_md_reset (mdh);
			gcry_md_write (mdh, digest, n_digest);
			gcry_md_final (mdh);
			digested = gcry_md_read (mdh, 0);
			g_return_val_if_fail (digested, FALSE);
			memcpy (digest, digested, n_digest);
		}
		
		/* Copy as much as possible into the destinations */
		i = 0; 
		while (needed_key && i < n_digest) {
			if (at_key)
				*(at_key++) = digest[i];
			needed_key--;
			i++;
		}
		while (needed_iv && i < n_digest) {
			if (at_iv) 
				*(at_iv++) = digest[i];
			needed_iv--;
			i++;
		}
		
		if (needed_key == 0 && needed_iv == 0)
			break;
	}

	egg_secure_free (digest);
	gcry_md_close (mdh);
	
	return TRUE;
}

gboolean
egg_symkey_generate_pbe (int cipher_algo, int hash_algo, const gchar *password, 
                         gssize n_password, const guchar *salt, gsize n_salt, int iterations, 
                         guchar **key, guchar **iv)
{
	gcry_md_hd_t mdh;
	gcry_error_t gcry;
	guchar *digest;
	guchar *digested;
	guint i, n_digest;
	gint needed_iv, needed_key;

	g_assert (cipher_algo);
	g_assert (hash_algo);

	g_return_val_if_fail (iterations >= 1, FALSE);
	
	if (!password)
		n_password = 0;
	if (n_password == -1)
		n_password = strlen (password);
	
	/* 
	 * We only do one pass here.
	 * 
	 * The key ends up as the first needed_key bytes of the hash buffer.
	 * The iv ends up as the last needed_iv bytes of the hash buffer. 
	 * 
	 * The IV may overlap the key (which is stupid) if the wrong pair of 
	 * hash/cipher algorithms are chosen.
	 */ 

	n_digest = gcry_md_get_algo_dlen (hash_algo);
	g_return_val_if_fail (n_digest > 0, FALSE);
	
	needed_key = gcry_cipher_get_algo_keylen (cipher_algo);
	needed_iv = gcry_cipher_get_algo_blklen (cipher_algo);
	if (needed_iv + needed_key > 16 || needed_iv + needed_key > n_digest) {
		g_warning ("using PBE symkey generation with %s using an algorithm that needs " 
		           "too many bytes of key and/or IV: %s",
		           gcry_cipher_algo_name (hash_algo), 
		           gcry_cipher_algo_name (cipher_algo));
		return FALSE;
	}
	
	gcry = gcry_md_open (&mdh, hash_algo, 0);
	if (gcry) {
		g_warning ("couldn't create '%s' hash context: %s", 
			   gcry_md_algo_name (hash_algo), gcry_strerror (gcry));
		return FALSE;
	}

	digest = egg_secure_alloc (n_digest);
	g_return_val_if_fail (digest, FALSE);
	if (key) {
		*key = egg_secure_alloc (needed_key);
		g_return_val_if_fail (*key, FALSE);
	}
	if (iv) 
		*iv = g_new0 (guchar, needed_iv);

	if (password)
		gcry_md_write (mdh, password, n_password);
	if (salt && n_salt)
		gcry_md_write (mdh, salt, n_salt);
	gcry_md_final (mdh);
	digested = gcry_md_read (mdh, 0);
	g_return_val_if_fail (digested, FALSE);
	memcpy (digest, digested, n_digest);
		
	for (i = 1; i < iterations; ++i)
		gcry_md_hash_buffer (hash_algo, digest, digest, n_digest);
	
	/* The first x bytes are the key */
	if (key) {
		g_assert (needed_key <= n_digest);
		memcpy (*key, digest, needed_key);
	}
	
	/* The last 16 - x bytes are the iv */
	if (iv) {
		g_assert (needed_iv <= n_digest && n_digest >= 16);
		memcpy (*iv, digest + (16 - needed_iv), needed_iv);
	}
		
	egg_secure_free (digest);
	gcry_md_close (mdh);
	
	return TRUE;	
}

static gboolean
generate_pkcs12 (int hash_algo, int type, const gchar *utf8_password, 
                 gssize n_password, const guchar *salt, gsize n_salt, 
                 int iterations, guchar *output, gsize n_output)
{
	gcry_mpi_t num_b1, num_ij;
	guchar *hash, *buf_i, *buf_b;
	const gchar *end_password;
	gcry_md_hd_t mdh;
	const gchar *p2;
	guchar *p;
	gsize n_hash, i;
	gunichar unich;
	gcry_error_t gcry;
	
	num_b1 = num_ij = NULL;
	
	n_hash = gcry_md_get_algo_dlen (hash_algo);
	g_return_val_if_fail (n_hash > 0, FALSE);
	
	if (!utf8_password)
		n_password = 0;
	if (n_password == -1) 
		end_password = utf8_password + strlen (utf8_password);
	else
		end_password = utf8_password + n_password;
	
	gcry = gcry_md_open (&mdh, hash_algo, 0);
	if (gcry) {
		g_warning ("couldn't create '%s' hash context: %s", 
		           gcry_md_algo_name (hash_algo), gcry_strerror (gcry));
		return FALSE;
	}

	/* Reqisition me a buffer */
	hash = egg_secure_alloc (n_hash);
	buf_i = egg_secure_alloc (128);
	buf_b = egg_secure_alloc (64);
	g_return_val_if_fail (hash && buf_i && buf_b, FALSE);
		
	/* Bring in the salt */
	p = buf_i;
	if (salt) {
		for (i = 0; i < 64; ++i)
			*(p++) = salt[i % n_salt];
	} else {
		memset (p, 0, 64);
		p += 64;
	}
	
	/* Bring in the password, as 16bits per character BMP string, ie: UCS2 */
	if (utf8_password) {
		p2 = utf8_password;
		for (i = 0; i < 64; i += 2) {
			
			/* Get a character from the string */
			if (p2 < end_password) {
				unich = g_utf8_get_char (p2);
				p2 = g_utf8_next_char (p2);

			/* Get zero null terminator, and loop back to beginning */
			} else {
				unich = 0;
				p2 = utf8_password;
			}

			/* Encode the bytes received */
			*(p++) = (unich & 0xFF00) >> 8;
			*(p++) = (unich & 0xFF);
		}
	} else {
		memset (p, 0, 64);
		p += 64;
	}
	
	/* Hash and bash */
	for (;;) {
		gcry_md_reset (mdh);

		/* Put in the PKCS#12 type of key */
		for (i = 0; i < 64; ++i)
			gcry_md_putc (mdh, type);
			
		/* Bring in the password */
		gcry_md_write (mdh, buf_i, utf8_password ? 128 : 64);
		
		/* First iteration done */
		memcpy (hash, gcry_md_read (mdh, hash_algo), n_hash);
		
		/* All the other iterations */
		for (i = 1; i < iterations; i++)
			gcry_md_hash_buffer (hash_algo, hash, hash, n_hash);
		
		/* Take out as much as we need */
		for (i = 0; i < n_hash && n_output; ++i) {
			*(output++) = hash[i];
			--n_output;
		}
		
		/* Is that enough generated keying material? */
		if (!n_output)
			break;
			
		/* Need more bytes, do some voodoo */
		for (i = 0; i < 64; ++i)
			buf_b[i] = hash[i % n_hash];
		gcry = gcry_mpi_scan (&num_b1, GCRYMPI_FMT_USG, buf_b, 64, NULL);
		g_return_val_if_fail (gcry == 0, FALSE);
		gcry_mpi_add_ui (num_b1, num_b1, 1);
		for (i = 0; i < 128; i += 64) {
			gcry = gcry_mpi_scan (&num_ij, GCRYMPI_FMT_USG, buf_i + i, 64, NULL);
			g_return_val_if_fail (gcry == 0, FALSE);
			gcry_mpi_add (num_ij, num_ij, num_b1);
			gcry_mpi_clear_highbit (num_ij, 64 * 8);
			gcry = gcry_mpi_print (GCRYMPI_FMT_USG, buf_i + i, 64, NULL, num_ij);
			g_return_val_if_fail (gcry == 0, FALSE);
			gcry_mpi_release (num_ij);
		}
	}  
	
	egg_secure_free (buf_i);
	egg_secure_free (buf_b);
	egg_secure_free (hash);
	gcry_mpi_release (num_b1);
	gcry_md_close (mdh);
	
	return TRUE;
}

gboolean
egg_symkey_generate_pkcs12 (int cipher_algo, int hash_algo, const gchar *password, 
                            gssize n_password, const guchar *salt, gsize n_salt,
                            int iterations, guchar **key, guchar **iv)
{
	gsize n_block, n_key;
	gboolean ret = TRUE;
	
	g_return_val_if_fail (cipher_algo, FALSE);
	g_return_val_if_fail (hash_algo, FALSE);
	g_return_val_if_fail (iterations > 0, FALSE);
	
	n_key = gcry_cipher_get_algo_keylen (cipher_algo);
	n_block = gcry_cipher_get_algo_blklen (cipher_algo);
	
	if (password && !g_utf8_validate (password, n_password, NULL)) {
		g_warning ("invalid non-UTF8 password");
		g_return_val_if_reached (FALSE);
	}
	
	if (key)
		*key = NULL;
	if (iv)
		*iv = NULL;
	
	/* Generate us an key */
	if (key) {
		*key = egg_secure_alloc (n_key);
		g_return_val_if_fail (*key != NULL, FALSE);
		ret = generate_pkcs12 (hash_algo, 1, password, n_password, salt, n_salt, 
		                       iterations, *key, n_key);
	} 
	
	/* Generate us an iv */
	if (ret && iv) {
		if (n_block > 1) {
			*iv = g_malloc (n_block);
			ret = generate_pkcs12 (hash_algo, 2, password, n_password, salt, n_salt, 
			                       iterations, *iv, n_block);
		} else {
			*iv = NULL;
		}
	}
	
	/* Cleanup in case of failure */
	if (!ret) {
		g_free (iv ? *iv : NULL);
		egg_secure_free (key ? *key : NULL);
	}
	
	return ret;
}

static gboolean
generate_pbkdf2 (int hash_algo, const gchar *password, gsize n_password,
		 const guchar *salt, gsize n_salt, guint iterations,
		 guchar *output, gsize n_output)
{
	gcry_md_hd_t mdh;
	guint u, l, r, i, k;
	gcry_error_t gcry;
	guchar *U, *T, *buf;
	gsize n_buf, n_hash;
	
	g_return_val_if_fail (hash_algo > 0, FALSE);
	g_return_val_if_fail (iterations > 0, FALSE);
	g_return_val_if_fail (n_output > 0, FALSE);
	g_return_val_if_fail (n_output < G_MAXUINT32, FALSE);

	n_hash = gcry_md_get_algo_dlen (hash_algo);
	g_return_val_if_fail (n_hash > 0, FALSE);
	
	gcry = gcry_md_open (&mdh, hash_algo, GCRY_MD_FLAG_HMAC);
	if (gcry != 0) {
		g_warning ("couldn't create '%s' hash context: %s", 
		           gcry_md_algo_name (hash_algo), gcry_strerror (gcry));
		return FALSE;
	}

	/* Get us a temporary buffers */
	T = egg_secure_alloc (n_hash);
	U = egg_secure_alloc (n_hash);
	n_buf = n_salt + 4;
	buf = egg_secure_alloc (n_buf);
	g_return_val_if_fail (buf && T && U, FALSE);

	/* n_hash blocks in output, rounding up */
	l = ((n_output - 1) / n_hash) + 1;
	
	/* number of bytes in last, rounded up, n_hash block */
	r = n_output - (l - 1) * n_hash;
	
	memcpy (buf, salt, n_salt);
	for (i = 1; i <= l; i++) {
		memset (T, 0, n_hash);
		for (u = 1; u <= iterations; u++) {
			gcry_md_reset (mdh);

			gcry = gcry_md_setkey (mdh, password, n_password);
			g_return_val_if_fail (gcry == 0, FALSE);
			
			/* For first iteration on each block add 4 extra bytes */
			if (u == 1) {
				buf[n_salt + 0] = (i & 0xff000000) >> 24;
				buf[n_salt + 1] = (i & 0x00ff0000) >> 16;
				buf[n_salt + 2] = (i & 0x0000ff00) >> 8;
				buf[n_salt + 3] = (i & 0x000000ff) >> 0;
				
				gcry_md_write (mdh, buf, n_buf);
		
			/* Other iterations, any block */
			} else {
				gcry_md_write (mdh, U, n_hash);
			}
			
			memcpy (U, gcry_md_read (mdh, hash_algo), n_hash);

			for (k = 0; k < n_hash; k++)
				T[k] ^= U[k];
		}

		memcpy (output + (i - 1) * n_hash, T, i == l ? r : n_hash);
	}
	
	egg_secure_free (T);
	egg_secure_free (U);
	egg_secure_free (buf);
	gcry_md_close (mdh);
	return TRUE;
}

gboolean
egg_symkey_generate_pbkdf2 (int cipher_algo, int hash_algo, 
                            const gchar *password, gssize n_password, 
                            const guchar *salt, gsize n_salt, int iterations, 
                            guchar **key, guchar **iv)
{
	gsize n_key, n_block;
	gboolean ret = TRUE;
	
	g_return_val_if_fail (hash_algo, FALSE);
	g_return_val_if_fail (cipher_algo, FALSE);
	g_return_val_if_fail (iterations > 0, FALSE);
	
	n_key = gcry_cipher_get_algo_keylen (cipher_algo);
	n_block = gcry_cipher_get_algo_blklen (cipher_algo);
	
	if (key)
		*key = NULL;
	if (iv)
		*iv = NULL;
	
	if (!password)
		n_password = 0;
	if (n_password == -1)
		n_password = strlen (password);
	
	/* Generate us an key */
	if (key) {
		*key = egg_secure_alloc (n_key);
		g_return_val_if_fail (*key != NULL, FALSE);
		ret = generate_pbkdf2 (hash_algo, password, n_password, salt, n_salt, 
		                       iterations, *key, n_key);
	} 
	
	/* Generate us an iv */
	if (ret && iv) {
		if (n_block > 1) {
			*iv = g_malloc (n_block);
			gcry_create_nonce (*iv, n_block);
		} else {
			*iv = NULL;
		}
	}
	
	/* Cleanup in case of failure */
	if (!ret) {
		g_free (iv ? *iv : NULL);
		egg_secure_free (key ? *key : NULL);
	}
	
	return ret;
}

/* ----------------------------------------------------------------------------
 * DER encoded cipher params
 */


static gboolean
read_cipher_pkcs5_pbe (int cipher_algo, int cipher_mode, int hash_algo, 
                       const gchar *password, gsize n_password, const guchar *data, 
                       gsize n_data, gcry_cipher_hd_t *cih)
{
	GNode *asn = NULL;
	gcry_error_t gcry;
	gconstpointer salt;
	gsize n_salt;
	gsize n_block, n_key;
	gulong iterations;
	guchar *key = NULL;
	guchar *iv = NULL;
	gboolean ret;

	g_return_val_if_fail (cipher_algo != 0 && cipher_mode != 0, FALSE);
	g_return_val_if_fail (cih != NULL, FALSE);
	g_return_val_if_fail (data != NULL && n_data != 0, FALSE);

	*cih = NULL;
	ret = FALSE;
	
	/* Check if we can use this algorithm */
	if (gcry_cipher_algo_info (cipher_algo, GCRYCTL_TEST_ALGO, NULL, 0) != 0 ||
	    gcry_md_test_algo (hash_algo) != 0)
		goto done;

	asn = egg_asn1x_create (pkix_asn1_tab, "pkcs-5-PBE-params");
	g_return_val_if_fail (asn, FALSE);

	if (!egg_asn1x_decode (asn, data, n_data))
		goto done;

	salt = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "salt", NULL), &n_salt);
	if (!salt)
		goto done;
	if (!egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "iterationCount", NULL), &iterations))
		iterations = 1;

	n_key = gcry_cipher_get_algo_keylen (cipher_algo);
	g_return_val_if_fail (n_key > 0, FALSE);
	n_block = gcry_cipher_get_algo_blklen (cipher_algo);
		
	if (!egg_symkey_generate_pbe (cipher_algo, hash_algo, password, n_password, salt,
	                              n_salt, iterations, &key, n_block > 1 ? &iv : NULL))
		goto done;
		
	gcry = gcry_cipher_open (cih, cipher_algo, cipher_mode, 0);
	if (gcry != 0) {
		g_warning ("couldn't create cipher: %s", gcry_strerror (gcry));
		goto done;
	}
	
	if (iv) 
		gcry_cipher_setiv (*cih, iv, n_block);
	gcry_cipher_setkey (*cih, key, n_key);
	
	ret = TRUE;

done:
	g_free (iv);
	egg_secure_free (key);
	egg_asn1x_destroy (asn);

	return ret;
}

static gboolean
setup_pkcs5_rc2_params (const guchar *data, guchar n_data, gcry_cipher_hd_t cih)
{
	GNode *asn = NULL;
	gcry_error_t gcry;
	const guchar *iv;
	gsize n_iv;
	gulong version;
	gboolean ret = FALSE;

	g_assert (data);

	asn = egg_asn1x_create (pkix_asn1_tab, "pkcs-5-rc2-CBC-params");
	g_return_val_if_fail (asn, FALSE);

	if (!egg_asn1x_decode (asn, data, n_data))
		goto done;

	if (!egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "rc2ParameterVersion", NULL), &version))
		goto done;

	iv = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "iv", NULL), &n_iv);
	if (!iv)
		goto done;

	gcry = gcry_cipher_setiv (cih, iv, n_iv);
	if (gcry != 0) {
		g_message ("couldn't set %lu byte iv on cipher", (gulong)n_iv);
		goto done;
	}

	ret = TRUE;

done:
	egg_asn1x_destroy (asn);
	return ret;
}

static gboolean
setup_pkcs5_des_params (const guchar *data, guchar n_data, gcry_cipher_hd_t cih)
{
	GNode *asn = NULL;
	gcry_error_t gcry;
	gconstpointer iv;
	gsize n_iv;

	g_assert (data);

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "pkcs-5-des-EDE3-CBC-params", data, n_data);
	if (!asn)
		asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "pkcs-5-des-CBC-params", data, n_data);
	if (!asn) 
		return FALSE;

	iv = egg_asn1x_get_raw_value (asn, &n_iv);
	egg_asn1x_destroy (asn);

	if (!iv)
		return FALSE;
		
	gcry = gcry_cipher_setiv (cih, iv, n_iv);
			
	if (gcry != 0) {
		g_message ("couldn't set %lu byte iv on cipher", (gulong)n_iv);
		return FALSE;
	}
	
	return TRUE;
}

static gboolean
setup_pkcs5_pbkdf2_params (const gchar *password, gsize n_password, const guchar *data, 
                           gsize n_data, int cipher_algo, gcry_cipher_hd_t cih)
{
	GNode *asn = NULL;
	gboolean ret;
	gcry_error_t gcry;
	guchar *key = NULL; 
	const guchar *salt;
	gsize n_salt, n_key;
	gulong iterations;

	g_assert (cipher_algo);
	g_assert (data);
	
	ret = FALSE;

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "pkcs-5-PBKDF2-params", data, n_data);
	if (!asn)
		goto done;

	if (!egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "iterationCount", NULL), &iterations))
		iterations = 1;
	salt = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "salt", "specified", NULL), &n_salt);
	if (!salt)
		goto done;

	if (!egg_symkey_generate_pbkdf2 (cipher_algo, GCRY_MD_SHA1, password, n_password, 
	                                 salt, n_salt, iterations, &key, NULL))
		goto done;

	n_key = gcry_cipher_get_algo_keylen (cipher_algo);
	g_return_val_if_fail (n_key > 0, FALSE);
	
	gcry = gcry_cipher_setkey (cih, key, n_key);
	if (gcry != 0) {
		g_message ("couldn't set %lu byte key on cipher", (gulong)n_key);
		goto done;
	}
	
	ret = TRUE;
	                                         
done:
	egg_secure_free (key);
	egg_asn1x_destroy (asn);
	return ret;
}

static gboolean
read_cipher_pkcs5_pbes2 (const gchar *password, gsize n_password, const guchar *data, 
                         gsize n_data, gcry_cipher_hd_t *cih)
{
	GNode *asn = NULL;
	gboolean r, ret;
	GQuark key_deriv_algo, enc_oid;
	gcry_error_t gcry;
	int algo, mode;
	gconstpointer params;
	gsize n_params;

	g_return_val_if_fail (cih != NULL, FALSE);
	g_return_val_if_fail (data != NULL && n_data != 0, FALSE);
	
	init_quarks ();
	
	*cih = NULL;
	ret = FALSE;

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "pkcs-5-PBES2-params", data, n_data);
	if (!asn)
		goto done;

	algo = mode = 0;
	
	/* Read in all the encryption type */
	enc_oid = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "encryptionScheme", "algorithm", NULL));
	if (!enc_oid)
		goto done;
	if (enc_oid == OID_DES_EDE3_CBC)
		algo = GCRY_CIPHER_3DES;
	else if (enc_oid == OID_DES_CBC)
		algo = GCRY_CIPHER_DES;
	else if (enc_oid == OID_DES_RC2_CBC)
		algo = GCRY_CIPHER_RFC2268_128;
	else if (enc_oid == OID_DES_RC5_CBC)
		/* RC5 doesn't exist in libgcrypt */;
	
	/* Unsupported? */
	if (algo == 0 || gcry_cipher_algo_info (algo, GCRYCTL_TEST_ALGO, NULL, 0) != 0)
		goto done;

	/* Instantiate our cipher */
	gcry = gcry_cipher_open (cih, algo, GCRY_CIPHER_MODE_CBC, 0);
	if (gcry != 0) {
		g_warning ("couldn't create cipher: %s", gcry_cipher_algo_name (algo));
		goto done;
	}
		
	/* Read out the parameters */
	params = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "encryptionScheme", "parameters", NULL), &n_params);
	if (!params)
		goto done;

	switch (algo) {
	case GCRY_CIPHER_3DES:
	case GCRY_CIPHER_DES:
		r = setup_pkcs5_des_params (params, n_params, *cih);
		break;
	case GCRY_CIPHER_RFC2268_128:
		r = setup_pkcs5_rc2_params (params, n_params, *cih);
		break;
	default:
		/* Should have been caught on the oid check above */
		g_assert_not_reached ();
		r = FALSE;
		break;
	};

	if (r != TRUE) 
		goto done;

	/* Read out the key creation paramaters */
	key_deriv_algo = egg_asn1x_get_oid_as_quark (egg_asn1x_node (asn, "keyDerivationFunc", "algorithm", NULL));
	if (!key_deriv_algo)
		goto done;
	if (key_deriv_algo != OID_PBKDF2) {
		g_message ("unsupported key derivation algorithm: %s", g_quark_to_string (key_deriv_algo));
		goto done;
	}

	params = egg_asn1x_get_raw_element (egg_asn1x_node (asn, "keyDerivationFunc", "parameters", NULL), &n_params);
	if (!params)
		goto done;

	ret = setup_pkcs5_pbkdf2_params (password, n_password, params, n_params, algo, *cih);

done:
	if (ret != TRUE && *cih) {
		gcry_cipher_close (*cih);
		*cih = NULL;
	}

	egg_asn1x_destroy (asn);
	return ret;
}

static gboolean
read_cipher_pkcs12_pbe (int cipher_algo, int cipher_mode, const gchar *password, 
                        gsize n_password, const guchar *data, gsize n_data, 
                        gcry_cipher_hd_t *cih)
{
	GNode *asn = NULL;
	gcry_error_t gcry;
	gboolean ret;
	const guchar *salt;
	gsize n_salt;
	gsize n_block, n_key;
	gulong iterations;
	guchar *key = NULL;
	guchar *iv = NULL;

	g_return_val_if_fail (cipher_algo != 0 && cipher_mode != 0, FALSE);
	g_return_val_if_fail (cih != NULL, FALSE);
	g_return_val_if_fail (data != NULL && n_data != 0, FALSE);
	
	*cih = NULL;
	ret = FALSE;
	
	/* Check if we can use this algorithm */
	if (gcry_cipher_algo_info (cipher_algo, GCRYCTL_TEST_ALGO, NULL, 0) != 0)
		goto done;

	asn = egg_asn1x_create_and_decode (pkix_asn1_tab, "pkcs-12-PbeParams", data, n_data);
	if (!asn)
		goto done;

	salt = egg_asn1x_get_raw_value (egg_asn1x_node (asn, "salt", NULL), &n_salt);
	if (!salt)
		goto done;
	if (!egg_asn1x_get_integer_as_ulong (egg_asn1x_node (asn, "iterations", NULL), &iterations))
		goto done;
	
	n_block = gcry_cipher_get_algo_blklen (cipher_algo);
	n_key = gcry_cipher_get_algo_keylen (cipher_algo);
	
	/* Generate IV and key using salt read above */
	if (!egg_symkey_generate_pkcs12 (cipher_algo, GCRY_MD_SHA1, password, 
	                                        n_password, salt, n_salt, iterations, &key, 
	                                        n_block > 1 ? &iv : NULL))
		goto done;
		
	gcry = gcry_cipher_open (cih, cipher_algo, cipher_mode, 0);
	if (gcry != 0) {
		g_warning ("couldn't create encryption cipher: %s", gcry_strerror (gcry));
		goto done;
	}
	
	if (iv) 
		gcry_cipher_setiv (*cih, iv, n_block);
	gcry_cipher_setkey (*cih, key, n_key);
	
	ret = TRUE;
	
done:
	if (ret != TRUE && *cih) {
		gcry_cipher_close (*cih);
		*cih = NULL;
	}
	
	g_free (iv);
	egg_secure_free (key);
	egg_asn1x_destroy (asn);
	return ret;
}

gboolean
egg_symkey_read_cipher (GQuark oid_scheme, const gchar *password, gsize n_password,
                        const guchar *data, gsize n_data, gcry_cipher_hd_t *cih)
{
	gboolean ret = FALSE;
	
	g_return_val_if_fail (oid_scheme != 0, FALSE);
	g_return_val_if_fail (cih != NULL, FALSE);
	g_return_val_if_fail (data != NULL && n_data != 0, FALSE);
	
	init_quarks ();
	
	/* PKCS#5 PBE */
	if (oid_scheme == OID_PBE_MD2_DES_CBC)
		ret = read_cipher_pkcs5_pbe (GCRY_CIPHER_DES, GCRY_CIPHER_MODE_CBC,
		                             GCRY_MD_MD2, password, n_password, data, n_data, cih);

	else if (oid_scheme == OID_PBE_MD2_RC2_CBC)
		/* RC2-64 has no implementation in libgcrypt */;

	else if (oid_scheme == OID_PBE_MD5_DES_CBC)
		ret = read_cipher_pkcs5_pbe (GCRY_CIPHER_DES, GCRY_CIPHER_MODE_CBC,
		                             GCRY_MD_MD5, password, n_password, data, n_data, cih);
	else if (oid_scheme == OID_PBE_MD5_RC2_CBC)
		/* RC2-64 has no implementation in libgcrypt */;

	else if (oid_scheme == OID_PBE_SHA1_DES_CBC)
		ret = read_cipher_pkcs5_pbe (GCRY_CIPHER_DES, GCRY_CIPHER_MODE_CBC,
		                             GCRY_MD_SHA1, password, n_password, data, n_data, cih);
	else if (oid_scheme == OID_PBE_SHA1_RC2_CBC)
		/* RC2-64 has no implementation in libgcrypt */;

	
	/* PKCS#5 PBES2 */
	else if (oid_scheme == OID_PBES2)
		ret = read_cipher_pkcs5_pbes2 (password, n_password, data, n_data, cih);

		
	/* PKCS#12 PBE */
	else if (oid_scheme == OID_PKCS12_PBE_ARCFOUR_SHA1)
		ret = read_cipher_pkcs12_pbe (GCRY_CIPHER_ARCFOUR, GCRY_CIPHER_MODE_STREAM, 
		                              password, n_password, data, n_data, cih);
	else if (oid_scheme == OID_PKCS12_PBE_RC4_40_SHA1)
		/* RC4-40 has no implementation in libgcrypt */;

	else if (oid_scheme == OID_PKCS12_PBE_3DES_SHA1)
		ret = read_cipher_pkcs12_pbe (GCRY_CIPHER_3DES, GCRY_CIPHER_MODE_CBC, 
		                              password, n_password, data, n_data, cih);
	else if (oid_scheme == OID_PKCS12_PBE_2DES_SHA1) 
		/* 2DES has no implementation in libgcrypt */;
		
	else if (oid_scheme == OID_PKCS12_PBE_RC2_128_SHA1)
		ret = read_cipher_pkcs12_pbe (GCRY_CIPHER_RFC2268_128, GCRY_CIPHER_MODE_CBC, 
		                              password, n_password, data, n_data, cih);

	else if (oid_scheme == OID_PKCS12_PBE_RC2_40_SHA1)
		ret = read_cipher_pkcs12_pbe (GCRY_CIPHER_RFC2268_40, GCRY_CIPHER_MODE_CBC, 
		                              password, n_password, data, n_data, cih);

	if (ret == FALSE)
    		g_message ("unsupported or invalid cipher: %s", g_quark_to_string (oid_scheme));
	
    	return ret;
}
