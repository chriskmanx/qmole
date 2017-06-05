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

#include "gkm-sexp.h"

struct _GkmSexp {
	gint refs;
	gcry_sexp_t real;
};

GkmSexp*
gkm_sexp_new (gcry_sexp_t real)
{
	GkmSexp *sexp;
	g_return_val_if_fail (real, NULL);
	sexp = g_slice_new0 (GkmSexp);
	sexp->refs = 1;
	sexp->real = real;
	return sexp;
}

GkmSexp*
gkm_sexp_ref (GkmSexp *sexp)
{
	g_return_val_if_fail (sexp, NULL);
	++(sexp->refs);
	return sexp;
}

void
gkm_sexp_unref (gpointer data)
{
	GkmSexp *sexp = data;
	g_return_if_fail (sexp);
	if (--(sexp->refs) == 0) {
		g_assert (sexp->real);
		gcry_sexp_release (sexp->real);
		g_slice_free (GkmSexp, sexp);
	}
}

gcry_sexp_t
gkm_sexp_get (GkmSexp *sexp)
{
	g_return_val_if_fail (sexp, NULL);
	g_return_val_if_fail (sexp->real, NULL);
	return sexp->real;
}

GType
gkm_sexp_boxed_type (void)
{
	static GType type = 0;
	if (!type)
		type = g_boxed_type_register_static ("GkmSexp",
		                                     (GBoxedCopyFunc)gkm_sexp_ref,
		                                     (GBoxedFreeFunc)gkm_sexp_unref);
	return type;
}

#define PUBLIC_KEY "public-key"
#define PUBLIC_KEY_L 10
#define PRIVATE_KEY "private-key"
#define PRIVATE_KEY_L 11

gboolean
gkm_sexp_parse_key (gcry_sexp_t s_key, int *algorithm, gboolean *is_private,
                    gcry_sexp_t *numbers)
{
	gboolean ret = FALSE;
	gcry_sexp_t child = NULL;
	gchar *str = NULL;
	const gchar *data;
	gsize n_data;
	gboolean priv;
	int algo;

	data = gcry_sexp_nth_data (s_key, 0, &n_data);
	if (!data)
		goto done;

	if (n_data == PUBLIC_KEY_L && strncmp (data, PUBLIC_KEY, PUBLIC_KEY_L) == 0)
		priv = FALSE;
	else if (n_data == PRIVATE_KEY_L && strncmp (data, PRIVATE_KEY, PRIVATE_KEY_L) == 0)
		priv = TRUE;
	else
		goto done;

	child = gcry_sexp_nth (s_key, 1);
	if (!child)
		goto done;

	data = gcry_sexp_nth_data (child, 0, &n_data);
	if (!data)
		goto done;

	str = g_alloca (n_data + 1);
	memcpy (str, data, n_data);
	str[n_data] = 0;

	algo = gcry_pk_map_name (str);
	if (!algo)
		goto done;

	/* Yay all done */
	if (algorithm)
		*algorithm = algo;
	if (numbers) {
		*numbers = child;
		child = NULL;
	}
	if (is_private)
		*is_private = priv;

	ret = TRUE;

done:
	gcry_sexp_release (child);
	return ret;
}

static gcry_sexp_t
rsa_numbers_to_public (gcry_sexp_t rsa)
{
	gcry_sexp_t pubkey = NULL;
	gcry_mpi_t n, e;
	gcry_error_t gcry;

	n = e = NULL;

	if (!gkm_sexp_extract_mpi (rsa, &n, "n", NULL) ||
	    !gkm_sexp_extract_mpi (rsa, &e, "e", NULL))
		goto done;

	gcry = gcry_sexp_build (&pubkey, NULL, "(public-key (rsa (n %m) (e %m)))",
	                        n, e);
	if (gcry)
		goto done;
	g_assert (pubkey);

done:
	gcry_mpi_release (n);
	gcry_mpi_release (e);

	return pubkey;
}

static gcry_sexp_t
dsa_numbers_to_public (gcry_sexp_t dsa)
{
	gcry_mpi_t p, q, g, y;
	gcry_sexp_t pubkey = NULL;
	gcry_error_t gcry;

	p = q = g = y = NULL;

	if (!gkm_sexp_extract_mpi (dsa, &p, "p", NULL) ||
	    !gkm_sexp_extract_mpi (dsa, &q, "q", NULL) ||
	    !gkm_sexp_extract_mpi (dsa, &g, "g", NULL) ||
	    !gkm_sexp_extract_mpi (dsa, &y, "y", NULL))
		goto done;

	gcry = gcry_sexp_build (&pubkey, NULL, "(public-key (dsa (p %m) (q %m) (g %m) (y %m)))",
	                        p, q, g, y);
	if (gcry)
		goto done;
	g_assert (pubkey);

done:
	gcry_mpi_release (p);
	gcry_mpi_release (q);
	gcry_mpi_release (g);
	gcry_mpi_release (y);

	return pubkey;
}

gboolean
gkm_sexp_key_to_public (gcry_sexp_t privkey, gcry_sexp_t *pubkey)
{
	gcry_sexp_t numbers;
	int algorithm;

	if (!gkm_sexp_parse_key (privkey, &algorithm, NULL, &numbers))
		g_return_val_if_reached (FALSE);

	switch (algorithm) {
	case GCRY_PK_RSA:
		*pubkey = rsa_numbers_to_public (numbers);
		break;
	case GCRY_PK_DSA:
		*pubkey = dsa_numbers_to_public (numbers);
		break;
	default:
		g_return_val_if_reached (FALSE);
	}

	gcry_sexp_release (numbers);
	return *pubkey ? TRUE : FALSE;
}

gboolean
gkm_sexp_extract_mpi (gcry_sexp_t sexp, gcry_mpi_t *mpi, ...)
{
	gcry_sexp_t at = NULL;
	va_list va;

	g_assert (sexp);
	g_assert (mpi);

	va_start (va, mpi);
	at = gkm_sexp_get_childv (sexp, va);
	va_end (va);

	*mpi = NULL;
	if (at)
		*mpi = gcry_sexp_nth_mpi (at ? at : sexp, 1, GCRYMPI_FMT_USG);
	if (at)
		gcry_sexp_release (at);

	return (*mpi) ? TRUE : FALSE;
}

gcry_sexp_t
gkm_sexp_get_childv (gcry_sexp_t sexp, va_list va)
{
	gcry_sexp_t at = NULL;
	gcry_sexp_t child;
	const char *name;

	g_assert (sexp);

	for(;;) {
		name = va_arg (va, const char*);
		if (!name)
			break;

		child = gcry_sexp_find_token (at ? at : sexp, name, 0);
		gcry_sexp_release (at);
		at = child;
		if (at == NULL)
			break;
	}

	va_end (va);

	return at;
}

void
gkm_sexp_dump (gcry_sexp_t sexp)
{
	gsize len;
	gchar *buf;

	len = gcry_sexp_sprint (sexp, GCRYSEXP_FMT_ADVANCED, NULL, 0);
	buf = g_malloc (len);
	gcry_sexp_sprint (sexp, GCRYSEXP_FMT_ADVANCED, buf, len);
	g_printerr ("%s", buf);
	g_free (buf);
}
