/* rsa-keygen.c
 *
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002 Niels Möller
 *  
 * The nettle library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * The nettle library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with the nettle library; see the file COPYING.LIB.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "buffer.h"
#include "rsa.h"
#include "sexp.h"
#include "yarrow.h"

#include "io.h"

#include "getopt.h"

#define KEYSIZE 900
#define ESIZE 30

static void
progress(void *ctx, int c)
{
  (void) ctx;
  fputc(c, stderr);
}

int
main(int argc, char **argv)
{
  struct yarrow256_ctx yarrow;
  struct rsa_public_key pub;
  struct rsa_private_key priv;

  int c;
  char *pub_name = NULL;
  const char *priv_name = NULL;
  const char *random_name = NULL;
  
  struct nettle_buffer pub_buffer;
  struct nettle_buffer priv_buffer;

  enum { OPT_HELP = 300 };
  static const struct option options[] =
    {
      /* Name, args, flag, val */
      { "help", no_argument, NULL, OPT_HELP },
      { "random", required_argument, NULL, 'r' },
      { NULL, 0, NULL, 0}
    };
  
  while ( (c = getopt_long(argc, argv, "o:r:", options, NULL)) != -1)
    switch (c)
      {
      case 'o':
	priv_name = optarg;
	break;

      case 'r':
	random_name = optarg;
	break;

      case OPT_HELP:
	printf("FIXME: Usage information.\n");
	return EXIT_SUCCESS;

      case '?':
	return EXIT_FAILURE;

      default:
	abort();
      }

  if (!priv_name)
    {
      werror("No filename provided.\n");
      return EXIT_FAILURE;
    }

  pub_name = xalloc(strlen(priv_name) + 5);  
  sprintf(pub_name, "%s.pub", priv_name);

  /* NOTE: No sources */
  yarrow256_init(&yarrow, 0, NULL);

  /* Read some data to seed the generator */
  if (!simple_random(&yarrow, random_name))
    {
      werror("Initialization of randomness generator failed.\n");
      return EXIT_FAILURE;
    }

  rsa_public_key_init(&pub);
  rsa_private_key_init(&priv);

  if (!rsa_generate_keypair
      (&pub, &priv,
       (void *) &yarrow, (nettle_random_func *) yarrow256_random,
       NULL, progress,
       KEYSIZE, ESIZE))
    {
      werror("Key generation failed.\n");
      return EXIT_FAILURE;
    }

  nettle_buffer_init(&priv_buffer);
  nettle_buffer_init(&pub_buffer);
  
  if (!rsa_keypair_to_sexp(&pub_buffer, "rsa-pkcs1-sha1", &pub, NULL))
    {
      werror("Formatting public key failed.\n");
      return EXIT_FAILURE;
    }

  if (!rsa_keypair_to_sexp(&priv_buffer, "rsa-pkcs1-sha1", &pub, &priv))
    {
      werror("Formatting private key failed.\n");
      return EXIT_FAILURE;
    }
  
  if (!write_file(pub_name, pub_buffer.size, pub_buffer.contents))
    {
      werror("Failed to write public key: %s\n",
	      strerror(errno));
      return EXIT_FAILURE;
    }

  /* NOTE: This doesn't set up paranoid access restrictions on the
   * private key file, like a serious key generation tool would do. */
  if (!write_file(priv_name, priv_buffer.size, priv_buffer.contents))
    {
      werror("Failed to write private key: %s\n",
	      strerror(errno));
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
