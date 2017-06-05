/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* unit-test-sexp.c: Test sexp stuff

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "test-suite.h"

#include "gkm/gkm-crypto.h"
#include "gkm/gkm-sexp.h"

#include <gcrypt.h>

#define TEST_RSA \
"(private-key (rsa " \
"(n  #00B78758D55EBFFAB61D07D0DC49B5309A6F1DA2AE51C275DFC2370959BB81AC0C39093B1C618E396161A0DECEB8768D0FFB14F197B96C3DA14190EE0F20D51315#)" \
"(e #010001#)" \
"(d #108BCAC5FDD35812981E6EC5957D98E2AB76E4064C47B861D27C2CC322C50792313C852B4164A035B42D261F1A09F9FFE8F477F9F78FF2EABBDA6BA875C671D7#)" \
"(p #00C357F11B19A18C66573D25D1E466D9AB8BCDDCDFE0B2E80BD46712C4BEC18EB7#)" \
"(q #00F0843B90A60EF7034CA4BE80414ED9497CABCC685143B388013FF989CBB0E093#)" \
"(u #12F2555F52EB56329A991CF0404B51C68AC921AD370A797860F550415FF987BD#)" \
"))"

#define TEST_DSA \
"(private-key (dsa " \
"  (p #0090EC0B60735839C754EAF8F64BB03FC35398D69772BFAE540079DEA2D3A61FAFFB27630A038A01A3D0CD62A10745A574A27ECB462F4F0885B79C61BBE954A60A29668AD54BBA5C07A72FD8B1105249670B339DF2C59E64A47064EFCF0B7236C5C72CD55CEB32917430BEC9A003D4E484FBAA84D79571B38D6B5AC95BB73E3F7B#)" \
"  (q #00FA214A1385C21BFEBAADAB240A2430C607D56271#)" \
"  (g #2DE05751F5DAEE97F3D43C54595A3E94A080728F0C66C98AEBED5762F6AB155802D8359EAD1DE1EC36A459FBEEEA48E59B9E6A8CB4F5295936B3CC881A5D957C7339175E2CFFE0F30D3711E430DB6648C2EB474AA10A4A3297450531FF2C7C6951220C9D446B6B6B0F00262E1EBEB3CC861476AA518CC555C9ABF9E5F39023FC#)" \
"  (y #54734451DB79D4EEDF0BBCEBD43BB6CBB7B8584603B957080075DD318EB5B0266D4B20DC5EFF376BDFC4EA2983B1F7F02A39ED4C619ED68712729FFF3B7C696ADD1B6D748F56A4B4BEC5C4385E528423A3B88AE65E6D5500F97839E7A486255982189C3B4FA8D94338C76F0E5CAFC9A30A1ED728BB9F2091D594E3250A09EA00#)" \
"  (x #00876F84F709D51108DFB0CBFA1F1C569C09C413EC#)))"

gcry_sexp_t rsakey = NULL;
gcry_sexp_t dsakey = NULL;

DEFINE_SETUP(crypto_setup)
{
	gcry_error_t gcry;

	gkm_crypto_initialize ();

	gcry = gcry_sexp_new (&rsakey, TEST_RSA, strlen (TEST_RSA), 1);
	g_return_if_fail (gcry == 0);
	gcry = gcry_sexp_new (&dsakey, TEST_DSA, strlen (TEST_DSA), 1);
	g_return_if_fail (gcry == 0);
}

DEFINE_TEARDOWN(crypto_setup)
{
	gcry_sexp_release (rsakey);
	rsakey = NULL;
	gcry_sexp_release (dsakey);
	dsakey = NULL;
}

DEFINE_TEST(parse_key)
{
	gcry_sexp_t sexp = NULL;
	gcry_mpi_t mpi = NULL;
	gboolean ret;
	gboolean is_priv = FALSE;
	int algorithm = 0;

	/* Get the private key out */
	ret = gkm_sexp_parse_key (rsakey, &algorithm, &is_priv, &sexp);
	g_assert (ret);
	g_assert (algorithm == GCRY_PK_RSA);
	g_assert (is_priv == TRUE);
	g_assert (sexp != NULL);

	ret = gkm_sexp_extract_mpi (rsakey, &mpi, "p", NULL);
	g_assert (ret);
	g_assert (mpi != NULL);
}

DEFINE_TEST(sexp_key_to_public)
{
	gcry_sexp_t pubkey = NULL;
	guchar id1[20], id2[20];
	gboolean ret;
	guchar *p;

	/* RSA */
	ret = gkm_sexp_key_to_public (rsakey, &pubkey);
	g_assert (ret);
	g_assert (pubkey != NULL);

	p = gcry_pk_get_keygrip (rsakey, id1);
	g_return_if_fail (p == id1);
	p = gcry_pk_get_keygrip (pubkey, id2);
	g_return_if_fail (p == id2);

	g_assert (memcmp (id1, id2, sizeof (id1)) == 0);

	gcry_sexp_release (pubkey);


	/* DSA */
	ret = gkm_sexp_key_to_public (dsakey, &pubkey);
	g_assert (ret);
	g_assert (pubkey != NULL);

	p = gcry_pk_get_keygrip (dsakey, id1);
	g_return_if_fail (p == id1);
	p = gcry_pk_get_keygrip (pubkey, id2);
	g_return_if_fail (p == id2);

	g_assert (memcmp (id1, id2, sizeof (id1)) == 0);

	gcry_sexp_release (pubkey);

}
