
#include "gkm-ssh-openssh.h"

#include "gkm/gkm-data-asn1.h"
#include "gkm/gkm-data-der.h"
#include "gkm/gkm-data-types.h"

#include "egg/egg-asn1x.h"
#include "egg/egg-buffer.h"
#include "egg/egg-openssl.h"
#include "egg/egg-secure-memory.h"

typedef struct _ParsePrivate {
	gcry_sexp_t sexp;
	gboolean seen;
	GkmDataResult result;
	const gchar *password;
	gssize n_password;
} ParsePrivate;

/* ------------------------------------------------------------------------------
 * INTERNAL
 */

static int
keytype_to_algo (const gchar *salgo)
{
	g_return_val_if_fail (salgo, 0);
	if (strcmp (salgo, "ssh-rsa") == 0)
		return GCRY_PK_RSA;
	else if (strcmp (salgo, "ssh-dss") == 0)
		return GCRY_PK_DSA;
	return 0;
}

static gboolean
read_mpi (EggBuffer *req, gsize *offset, gcry_mpi_t *mpi)
{
	const guchar *data;
	gsize len;
	gcry_error_t gcry;

	if (!egg_buffer_get_byte_array (req, *offset, offset, &data, &len))
		return FALSE;

	gcry = gcry_mpi_scan (mpi, GCRYMPI_FMT_USG, data, len, NULL);
	if (gcry)
		return FALSE;

	return TRUE;
}

#define SEXP_PUBLIC_DSA  \
	"(public-key"    \
	"  (dsa"         \
	"    (p %m)"     \
	"    (q %m)"     \
	"    (g %m)"     \
	"    (y %m)))"

static gboolean
read_public_dsa (EggBuffer *req, gsize *offset, gcry_sexp_t *sexp)
{
	gcry_mpi_t p, q, g, y;
	int gcry;

	if (!read_mpi (req, offset, &p) ||
	    !read_mpi (req, offset, &q) ||
	    !read_mpi (req, offset, &g) ||
	    !read_mpi (req, offset, &y))
		return FALSE;

	gcry = gcry_sexp_build (sexp, NULL, SEXP_PUBLIC_DSA, p, q, g, y);
	if (gcry) {
		g_warning ("couldn't parse incoming public DSA key: %s", gcry_strerror (gcry));
		return FALSE;
	}

	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);
	gcry_mpi_release (y);

	return TRUE;
}

#define SEXP_PUBLIC_RSA  \
	"(public-key"    \
	"  (rsa"         \
	"    (n %m)"     \
	"    (e %m)))"

static gboolean
read_public_rsa (EggBuffer *req, gsize *offset, gcry_sexp_t *sexp)
{
	gcry_mpi_t n, e;
	int gcry;

	if (!read_mpi (req, offset, &e) ||
	    !read_mpi (req, offset, &n))
		return FALSE;

	gcry = gcry_sexp_build (sexp, NULL, SEXP_PUBLIC_RSA, n, e);
	if (gcry) {
		g_warning ("couldn't parse incoming public RSA key: %s", gcry_strerror (gcry));
		return FALSE;
	}

	gcry_mpi_release (n);
	gcry_mpi_release (e);

	return TRUE;
}

static gboolean
read_public (EggBuffer *req, gsize *offset, gcry_sexp_t *key, int *algo)
{
	gboolean ret;
	gchar *stype;
	int alg;

	/* The string algorithm */
	if (!egg_buffer_get_string (req, *offset, offset, &stype, (EggBufferAllocator)g_realloc))
		return FALSE;

	alg = keytype_to_algo (stype);
	g_free (stype);

	if (!alg) {
		g_warning ("unsupported algorithm from SSH: %s", stype);
		return FALSE;
	}

	switch (alg) {
	case GCRY_PK_RSA:
		ret = read_public_rsa (req, offset, key);
		break;
	case GCRY_PK_DSA:
		ret = read_public_dsa (req, offset, key);
		break;
	default:
		g_assert_not_reached ();
		return FALSE;
	}

	if (!ret) {
		g_warning ("couldn't read incoming SSH private key");
		return FALSE;
	}

	if (algo)
		*algo = alg;
	return TRUE;
}

static GkmDataResult
load_encrypted_key (const guchar *data, gsize n_data, const gchar *dekinfo,
                    const gchar *password, gssize n_password, gcry_sexp_t *skey)
{
	guchar *decrypted = NULL;
	gsize n_decrypted = 0;
	GkmDataResult ret;
	gboolean res;
	gint length;

	/* Decrypt, this will result in garble if invalid password */
	res = egg_openssl_decrypt_block (dekinfo, password, n_password,
	                                      data, n_data, &decrypted, &n_decrypted);
	if (!res)
		return FALSE;

	g_assert (decrypted);

	/* Unpad the DER data */
	length = egg_asn1x_element_length (decrypted, n_decrypted);
	if (length > 0)
		n_decrypted = length;

	/* Try to parse */
	ret = gkm_data_der_read_private_key (decrypted, n_decrypted, skey);
	egg_secure_free (decrypted);

	if (ret != GKM_DATA_UNRECOGNIZED)
		return ret;

	return GKM_DATA_LOCKED;
}

static gboolean
is_private_key_type (GQuark type)
{
	static GQuark PEM_RSA_PRIVATE_KEY;
	static GQuark PEM_DSA_PRIVATE_KEY;
	static gsize quarks_inited = 0;

	/* Initialize the first time through */
	if (g_once_init_enter (&quarks_inited)) {
		PEM_RSA_PRIVATE_KEY = g_quark_from_static_string ("RSA PRIVATE KEY");
		PEM_DSA_PRIVATE_KEY = g_quark_from_static_string ("DSA PRIVATE KEY");
		g_once_init_leave (&quarks_inited, 1);
	}

	/* Only handle SSHv2 private keys */
	return (type == PEM_RSA_PRIVATE_KEY || type == PEM_DSA_PRIVATE_KEY);
}

static void
parsed_pem_block (GQuark type, const guchar *data, gsize n_data,
                  GHashTable *headers, gpointer user_data)
{
	ParsePrivate *ctx = (ParsePrivate*)user_data;
	const gchar *dekinfo;

	if (!is_private_key_type (type))
		return;

	ctx->seen = TRUE;

	/* Only parse first key in the file */
	if (ctx->sexp)
		return;

	/* If it's encrypted ... */
	dekinfo = egg_openssl_get_dekinfo (headers);
	if (dekinfo) {
		ctx->result = load_encrypted_key (data, n_data, dekinfo, ctx->password,
		                                  ctx->n_password, &ctx->sexp);

	/* not encryted, just load the data */
	} else {
		ctx->result = gkm_data_der_read_private_key (data, n_data, &ctx->sexp);
	}
}

static void
digest_pem_block (GQuark type, const guchar *data, gsize n_data,
                  GHashTable *headers, gpointer user_data)
{
	gchar **result = (gchar**)user_data;

	g_assert (result);

	if (!is_private_key_type (type))
		return;

	/* Only digest the first key in the file */
	if (*result != NULL)
		return;

	*result = g_compute_checksum_for_data (G_CHECKSUM_SHA1, data, n_data);
}

/* ------------------------------------------------------------------------------
 * PUBLIC
 */

GkmDataResult
gkm_ssh_openssh_parse_public_key (const guchar *data, gsize n_data,
                                  gcry_sexp_t *sexp, gchar **comment)
{
	EggBuffer buf;
	const guchar *at;
	guchar *decoded;
	gsize n_decoded;
	gsize offset;
	gchar *val;
	gboolean ret;
	gint state, algo;
	guint save;

	g_return_val_if_fail (data, FALSE);
	g_return_val_if_fail (sexp, FALSE);

	/* Look for a key line */
	for (;;) {
		/* Eat space at the front */
		while (n_data > 0 && g_ascii_isspace (data[0])) {
			++data;
			--n_data;
		}

		/* Not a comment or blank line? Then parse... */
		if (data[0] != '#')
			break;

		/* Skip to the next line */
		at = memchr (data, '\n', n_data);
		if (!at)
			return GKM_DATA_UNRECOGNIZED;
		at += 1;
		n_data -= (at - data);
		data = at;
	}

	/* Limit to use only the first line */
	at = memchr (data, '\n', n_data);
	if (at != NULL)
		n_data = at - data;

	/* Find the first space */
	at = memchr (data, ' ', n_data);
	if (!at) {
		g_message ("SSH public key missing space");
		return GKM_DATA_UNRECOGNIZED;
	}

	/* Parse the key type */
	val = g_strndup ((gchar*)data, at - data);
	algo = keytype_to_algo (val);
	if (!algo) {
		/* A number usually means an SSH1 key, just quietly ignore */
		if (atoi (val) == 0)
			g_message ("Unsupported or unknown SSH key algorithm: %s", val);
	}
	g_free (val);
	if (!algo)
		return GKM_DATA_UNRECOGNIZED;

	/* Skip more whitespace */
	n_data -= (at - data);
	data = at;
	while (n_data > 0 && (data[0] == ' ' || data[0] == '\t')) {
		++data;
		--n_data;
	}

	/* Find the next whitespace, or the end */
	at = memchr (data, ' ', n_data);
	if (at == NULL)
		at = data + n_data;

	/* Decode the base64 key */
	save = state = 0;
	decoded = g_malloc (n_data * 3 / 4);
	n_decoded = g_base64_decode_step ((gchar*)data, n_data, decoded, &state, &save);

	/* Parse the actual key */
	egg_buffer_init_static (&buf, decoded, n_decoded);
	offset = 0;
	ret = read_public (&buf, &offset, sexp, NULL);
	g_free (decoded);
	if (!ret) {
		g_message ("failed to parse base64 part of SSH key");
		return GKM_DATA_FAILURE;
	}

	/* Skip more whitespace */
	n_data -= (at - data);
	data = at;
	while (n_data > 0 && (data[0] == ' ' || data[0] == '\t')) {
		++data;
		--n_data;
	}

	/* If there's data left, its the comment */
	if (comment)
		*comment = n_data ? g_strndup ((gchar*)data, n_data) : NULL;

	return GKM_DATA_SUCCESS;
}

GkmDataResult
gkm_ssh_openssh_parse_private_key (const guchar *data, gsize n_data,
                                   const gchar *password, gssize n_password,
                                   gcry_sexp_t *sexp)
{
	ParsePrivate ctx;
	guint num;

	memset (&ctx, 0, sizeof (ctx));
	ctx.result = FALSE;
	ctx.seen = FALSE;
	ctx.sexp = NULL;
	ctx.password = password;
	ctx.n_password = n_password;

	num = egg_openssl_pem_parse (data, n_data, parsed_pem_block, &ctx);

	/* Didn't find any private key there */
	if (num == 0 || !ctx.seen) {
		g_message ("no private keys found in file");
		return GKM_DATA_UNRECOGNIZED;
	}

	*sexp = ctx.sexp;
	return ctx.result;
}

gchar*
gkm_ssh_openssh_digest_private_key (const guchar *data, gsize n_data)
{
	gchar *result = NULL;
	egg_openssl_pem_parse (data, n_data, digest_pem_block, &result);
	return result;
}
