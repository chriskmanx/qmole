/* aes-set-encrypt-key.c
 *
 * Key setup for the aes/rijndael block cipher.
 */

/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2000, 2001, 2002 Rafael R. Sevilla, Niels Möller
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

/* Originally written by Rafael R. Sevilla <dido@pacific.net.ph> */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>

#include "aes-internal.h"

static unsigned
xtime(unsigned x)
{
  assert (x < 0x100);

  x <<= 1;
  if (x & 0x100)
    x ^= 0x11b;

  assert (x < 0x100);

  return x;
}

void
aes_set_encrypt_key(struct aes_ctx *ctx,
		    unsigned keysize, const uint8_t *key)
{
  unsigned nk, nr, i, lastkey;
  uint32_t temp, rcon;

  assert(keysize >= AES_MIN_KEY_SIZE);
  assert(keysize <= AES_MAX_KEY_SIZE);
  
  /* Truncate keysizes to the valid key sizes provided by Rijndael */
  if (keysize == 32) {
    nk = 8;
    nr = 14;
  } else if (keysize >= 24) {
    nk = 6;
    nr = 12;
  } else { /* must be 16 or more */
    nk = 4;
    nr = 10;
  }

  lastkey = (AES_BLOCK_SIZE/4) * (nr + 1);
  ctx->nrounds = nr;
  rcon = 1;
  for (i=0; i<nk; i++)
    {
      ctx->keys[i] = key[i*4] + (key[i*4+1]<<8) + (key[i*4+2]<<16) +
	(key[i*4+3]<<24);
    }

  for (i=nk; i<lastkey; i++)
    {
      temp = ctx->keys[i-1];
      if (i % nk == 0)
	{
	  temp = SUBBYTE(ROTBYTE(temp), aes_sbox) ^ rcon;
	  rcon = (uint32_t)xtime((uint8_t)rcon&0xff);
	}
      else if (nk > 6 && (i%nk) == 4)
	{
	  temp = SUBBYTE(temp, aes_sbox);
	}
      ctx->keys[i] = ctx->keys[i-nk] ^ temp;
    }
}
  
