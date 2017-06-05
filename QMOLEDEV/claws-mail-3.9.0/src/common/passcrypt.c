/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail Team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#if !defined (__FreeBSD__)
#define _XOPEN_SOURCE 600
#else
#define _XOPEN_SOURCE
#endif

#include <sys/types.h>
#include <stdio.h>
#include <memory.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>

#if defined (__FreeBSD__)
#include <rpc/des_crypt.h>
#endif

#include <glib.h>

#include "passcrypt.h"

static void crypt_cfb_buf(const char key[8], unsigned char *buf, unsigned len,
		   unsigned chunksize, int decrypt);

void passcrypt_encrypt(gchar *password, guint len)
{
	crypt_cfb_buf(PASSCRYPT_KEY, password, len, 1, 0 );
}

void passcrypt_decrypt(gchar *password, guint len)
{
	crypt_cfb_buf(PASSCRYPT_KEY, password, len, 1, 1 );
}

/*
* crypt_cfb_iv is the intermediate vector used for cypher feedback encryption
*/
unsigned char crypt_cfb_iv[64];
int crypt_cfb_blocksize = 8;	/* 8 for DES */

#if defined (__FreeBSD__)
static void
crypt_cfb_buf(const char key[8], unsigned char *buf, unsigned len,
	      unsigned chunksize, int decrypt)
{
	char des_key[8];
	
	strncpy(des_key, PASSCRYPT_KEY, 8);
	des_setparity(des_key);
	if (decrypt)
		ecb_crypt(des_key, buf, len, DES_DECRYPT);
	else
		ecb_crypt(des_key, buf, len, DES_ENCRYPT);
}
#else
static void crypt_cfb_shift(unsigned char *to,
			    const unsigned char *from, unsigned len);
static void crypt_cfb_xor(unsigned char *to, const unsigned char *from,
			  unsigned len);
static void crypt_unpack(unsigned char *a);

static void
crypt_cfb_buf(const char key[8], unsigned char *buf, unsigned len,
	      unsigned chunksize, int decrypt)
{
	unsigned char temp[64];

	memcpy(temp, key, 8);
	crypt_unpack(temp);
	setkey((const char *) temp);
	memset(temp, 0, sizeof(temp));

	memset(crypt_cfb_iv, 0, sizeof(crypt_cfb_iv));

	if (chunksize > crypt_cfb_blocksize)
		chunksize = crypt_cfb_blocksize;

	while (len) {
		memcpy(temp, crypt_cfb_iv, sizeof(temp));
		encrypt((char *) temp, 0);
		if (chunksize > len)
			chunksize = len;
		if (decrypt)
			crypt_cfb_shift(crypt_cfb_iv, buf, chunksize);
		crypt_cfb_xor((unsigned char *) buf, temp, chunksize);
		if (!decrypt)
			crypt_cfb_shift(crypt_cfb_iv, buf, chunksize);
		len -= chunksize;
		buf += chunksize;
	}
}

/*
* Shift len bytes from end of to buffer to beginning, then put len
* bytes from from at the end.  Caution: the to buffer is unpacked,
* but the from buffer is not.
*/
static void
crypt_cfb_shift(unsigned char *to, const unsigned char *from, unsigned len)
{
	unsigned i;
	unsigned j;
	unsigned k;

	if (len < crypt_cfb_blocksize) {
		i = len * 8;
		j = crypt_cfb_blocksize * 8;
		for (k = i; k < j; k++) {
			to[0] = to[i];
			++to;
		}
	}

	for (i = 0; i < len; i++) {
		j = *from++;
		for (k = 0x80; k; k >>= 1)
			*to++ = ((j & k) != 0);
	}
}

/*
* XOR len bytes from from into the data at to.  Caution: the from buffer
* is unpacked, but the to buffer is not.
*/
static void
crypt_cfb_xor(unsigned char *to, const unsigned char *from, unsigned len)
{
	unsigned i;
	unsigned j;
	unsigned char c;

	for (i = 0; i < len; i++) {
		c = 0;
		for (j = 0; j < 8; j++)
			c = (c << 1) | *from++;
		*to++ ^= c;
	}
}

/*
* Take the 8-byte array at *a (must be able to hold 64 bytes!) and unpack
* each bit into its own byte.
*/
static void crypt_unpack(unsigned char *a)
{
	int i, j;

	for (i = 7; i >= 0; --i)
		for (j = 7; j >= 0; --j)
			a[(i << 3) + j] = (a[i] & (0x80 >> j)) != 0;
}
#endif
