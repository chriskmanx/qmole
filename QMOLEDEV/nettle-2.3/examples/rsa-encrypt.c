/* rsa-encrypt.c
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

/* string.h must be included before gmp.h */
#include "bignum.h"
#include "buffer.h"
#include "macros.h"
#include "rsa.h"
#include "yarrow.h"

#include "io.h"
#include "rsa-session.h"

#include "getopt.h"

void
rsa_session_set_encrypt_key(struct rsa_session *ctx,
			    const struct rsa_session_info *key)
{
  const uint8_t *aes_key = SESSION_AES_KEY(key);
  const uint8_t *iv = SESSION_IV(key);
  const uint8_t *hmac_key = SESSION_HMAC_KEY(key);
  
  aes_set_encrypt_key(&ctx->aes.ctx, AES_KEY_SIZE, aes_key);
  CBC_SET_IV(&ctx->aes, iv);
  hmac_sha1_set_key(&ctx->hmac, SHA1_DIGEST_SIZE, hmac_key);
}

static int
write_uint32(FILE *f, uint32_t n)
{
  uint8_t buffer[4];
  WRITE_UINT32(buffer, n);

  return write_string(f, sizeof(buffer), buffer);
}

static int
write_version(FILE *f)
{
  return write_uint32(f, 1);
}

static int
write_bignum(FILE *f, mpz_t x)
{
  unsigned size = nettle_mpz_sizeinbase_256_u(x);
  uint8_t *p;
  int res;
  
  if (!write_uint32(f, size))
    return 0;
  
  p = xalloc(size);
  nettle_mpz_get_str_256(size, p, x);

  res = write_string(f, size, p);
  free(p);
  return res;
}

static int
process_file(struct rsa_session *ctx,
	     FILE *in, FILE *out)
{
  uint8_t buffer[AES_BLOCK_SIZE * 100];
  unsigned leftover;
  unsigned padding;

  padding = leftover = 0;

  for (;;)
    {
      size_t size = fread(buffer, 1, sizeof(buffer), in);
      if (ferror(in))
	{
	  werror("Reading input failed: %s\n", strerror(errno));
	  return 0;
	}

      hmac_sha1_update(&ctx->hmac, size, buffer);
      if (size < sizeof(buffer))
	{
	  /* Setting padding != ends the loop */
	  leftover = size % AES_BLOCK_SIZE;
	  padding = AES_BLOCK_SIZE - leftover;
	  size -= leftover;

	  if (!size)
	    break;
	}

      CBC_ENCRYPT(&ctx->aes, aes_encrypt, size, buffer, buffer);
      if (!write_string(out, size, buffer))
	{
	  werror("Writing output failed: %s\n", strerror(errno));
	  return 0;
	}

      if (padding)
	{
	  if (leftover)
	    memcpy(buffer, buffer + size, leftover);

	  break;
	}
    }
  if (padding > 1)
    yarrow256_random(&ctx->yarrow, padding - 1, buffer + leftover);

  buffer[AES_BLOCK_SIZE - 1] = padding;
  CBC_ENCRYPT(&ctx->aes, aes_encrypt, AES_BLOCK_SIZE, buffer, buffer);
  hmac_sha1_digest(&ctx->hmac, SHA1_DIGEST_SIZE, buffer + AES_BLOCK_SIZE);

  if (!write_string(out, AES_BLOCK_SIZE + SHA1_DIGEST_SIZE, buffer))
    {
      werror("Writing output failed: %s\n", strerror(errno));
      return 0;
    }

  return 1;
}

int
main(int argc, char **argv)
{
  struct rsa_session ctx;
  struct rsa_session_info info;
  
  struct rsa_public_key key;
  mpz_t x;
  
  int c;
  const char *random_name = NULL;

  /* FIXME: --help option. */
  while ( (c = getopt(argc, argv, "o:r:")) != -1)
    switch (c)
      {
      case 'r':
	random_name = optarg;
	break;
	
      case '?':
	return EXIT_FAILURE;

      default:
	abort();
      }

  argv += optind;
  argc -= optind;

  if (argc != 1)
    {
      werror("Usage: rsa-encrypt [-r random-file] PUBLIC-KEY < cleartext\n");
      return EXIT_FAILURE;
    }

  rsa_public_key_init(&key);
  
  if (!read_rsa_key(argv[0], &key, NULL))
    {
      werror("Invalid key\n");
      return EXIT_FAILURE;
    }

  /* NOTE: No sources */
  yarrow256_init(&ctx.yarrow, 0, NULL);
  
  /* Read some data to seed the generator */
  if (!simple_random(&ctx.yarrow, random_name))
    {
      werror("Initialization of randomness generator failed.\n");
      return EXIT_FAILURE;
    }

  WRITE_UINT32(SESSION_VERSION(&info), RSA_VERSION);
  
  yarrow256_random(&ctx.yarrow, sizeof(info.key) - 4, info.key + 4);

  rsa_session_set_encrypt_key(&ctx, &info);
  
  write_version(stdout);
  
  mpz_init(x);

  if (!rsa_encrypt(&key,
		   &ctx.yarrow, (nettle_random_func *) yarrow256_random,
		   sizeof(info.key), info.key, 
		   x))
    {
      werror("RSA encryption failed.\n");
      return EXIT_FAILURE;
    }

  write_bignum(stdout, x);
  
  if (!process_file(&ctx,
		    stdin, stdout))
    return EXIT_FAILURE;
  
  rsa_public_key_clear(&key);

  return EXIT_SUCCESS;
}
