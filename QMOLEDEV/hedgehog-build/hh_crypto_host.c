/* This file is part of Hedgehog LISP.
 * Copyright (C) 2005 Kenneth Oksanen.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

/* This file includes concise implementations of the SHA-256 hash
   function and a slightly modified variant of the XXTEA for 128-bit
   blocks. */


#include "hh_common.h"
/* For HH_GET_UINT32 and HH_PUT_UINT32. */
#include "hh_interp.h"


/* I have noticed that in practice relatively recent versions of gcc
   will compile the below code to native rotate instructions.
   Therefore I won't place any heavy inline-assemblery here.  */
#define HH_ROR(x, shmt)  (((x) >> (shmt)) | ((x) << (32 - (shmt))))


static void hh_put_uint32s(unsigned char *s, hh_word_t *w, int n)
{
  hh_word_t *w_end = w + n;

  while (w < w_end) {
    HH_PUT_UINT32(s, *w);
    w++;
    s += 4;
  }
}


static void hh_get_uint32s(const unsigned char *s, hh_word_t *w, int n)
{
  hh_word_t *w_end = w + n;

  while (w < w_end) {
    *w = HH_GET_UINT32(s);
    w++;
    s += 4;
  }
}


/* A table of pseudo-random numbers for SHA-256 --- 2^32 times the cube
   root of the first 64 primes 2 .. 311. */
static const hh_word_t hh_sha_table[] = {
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 
  0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
  0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
  0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
  0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
  0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

/* The initial hash state of SHA-256 --- 2^32 times the square root of
   the first 8 primes 2..19 */
static hh_word_t hh_sha_init[] = {
  0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
  0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
};

void hh_sha256(unsigned char *data, unsigned long n_bytes,
	       unsigned char *digest)
{
  signed long n_bytes_left = (signed long) n_bytes, i;
  hh_word_t hash[8], w[64];
  hh_word_t a, b, c, d, e, f, g, h, t0, t1;
  unsigned char buf[64];

  memcpy(hash, hh_sha_init, 8 * sizeof(hh_word_t));

  /* Process the message in successive 512-bit (or in our case
     64-byte) chunks. */
  while (data) {

    memset(buf, 0, 64);
    if (n_bytes_left >= 64) {
      memcpy(buf, data, 64);
      data += 64;
      n_bytes_left -= 64;
    } else {
      /* Append one 1-bit and as many 0-bits as necessary to message
	 to reach a multiple of 512 bits minus the 64-bit length
	 indicator. */
      if (n_bytes_left >= 0) {
	memcpy(buf, data, n_bytes_left);
	buf[n_bytes_left] = 0x80;
      }
      if (n_bytes_left < 56) {
	/* Place the message length (in bits) to the last 64 bits.
	   Considering the audience of Hedgehog, we assume below 2^32
	   bits, i.e. 48 megabytes. */
	n_bytes <<= 3;
	HH_PUT_UINT32(&buf[60], n_bytes);
	data = NULL;
      }
      /* This assignment makes sure we don't place a second 1-bit in
	 the next iteration. */
      n_bytes_left = -1;
    }

    hh_get_uint32s(buf, w, 16);
    for (i = 16; i < 64; i++) {
      a = w[i-15];
      b = w[i-2];
      w[i] = w[i-16]
	+ w[i-7] 
	+ (HH_ROR(a, 7) ^ HH_ROR(a, 18) ^ (a >> 3))
	+ (HH_ROR(b, 17) ^ HH_ROR(b, 19) ^ (b >> 10));
    }

    a = hash[0];
    b = hash[1];
    c = hash[2];
    d = hash[3];
    e = hash[4];
    f = hash[5];
    g = hash[6];
    h = hash[7];

    for (i = 0; i < 64; i++) {
      t0 = (HH_ROR(a, 2) ^ HH_ROR(a, 13) ^ HH_ROR(a, 22))
	+ ((a & b) | (b & c) | (c & a));
      t1 = (HH_ROR(e, 6) ^ HH_ROR(e, 11) ^ HH_ROR(e, 25))
	+ (g ^ (e & (f ^ g))) + h + hh_sha_table[i] + w[i];
      h = g;
      g = f;
      f = e;
      e = d + t1;
      d = c;
      c = b;
      b = a;
      a = t0 + t1;
    }

    /* Add this chunk's hash to result */
    hash[0] += a;
    hash[1] += b;
    hash[2] += c;
    hash[3] += d;
    hash[4] += e;
    hash[5] += f;
    hash[6] += g;
    hash[7] += h;
  }

  hh_put_uint32s(digest, hash, 8);
}


/* Implementation of the XXTEA cipher by David Wheeler and Roger
   Needham from Cambridge University Computer Lab.  See
   http://www.cl.cam.ac.uk/ftp/users/djw3/xxtea.ps

   Personal comment: I (cessu) considered implementing AES
   (a.k.a. Rijndael) from quite some time, but the size of any
   reasonably efficient implementation is easily over 10 kB.  I refused
   to budget that much memory for a cipher in Hedgehog.  This
   implementation of XXTEA uses typically below 1 kB.

   While XXTEA hasn't been studied in great depth and although I
   consider it probable that XXTEA is weaker than AES, it should be
   sufficiently safe against any attacker short of a billion dollar
   security agency.  In any case I've fixed the block size from
   minimal 64 bits to 128 bits and increased the rounds from the
   original 19 to 32.  */

static void hh_xxtea_block(hh_word_t *data, int encrypt, hh_word_t *key)
{
#define HH_XXTEA_N_ROUNDS  32
  const hh_word_t delta = 0x9e3779b9;
  hh_word_t z, y = data[0], sum, e;
  hh_signed_word_t i, q;
  
  if (encrypt) {
    /* Encoding. */
    z = data[3];
    sum = 0;
    q = HH_XXTEA_N_ROUNDS;
    while (q-- > 0) {
      sum += delta;
      e = sum >> 2;
      for (i = 0; i < 4; i++) {
	y = data[(i + 1) & 0x3];
	z = data[i] += 
	  (((z >> 5) ^ (y << 2)) + ((y >> 3) ^ (z << 4)))
	  ^ ((sum ^ y) + (key[(i ^ e) & 0x3] ^ z));
      }
    }
  } else {
    /* Decoding */
    sum = HH_XXTEA_N_ROUNDS * delta;
    while (sum) {
      e = sum >> 2;
      for (i = 3; i >= 0; i--) {
	z = data[(i - 1) & 0x3];
	y = data[i] -= 
	  (((z >> 5) ^ (y << 2)) + ((y >> 3) ^ (z << 4)))
	  ^ ((sum ^ y) + (key[(i ^ e) & 0x3] ^ z));
      }
      sum -= delta;
    }
  }
}


/* Before encryption we prepend a word telling the total length (in
   bytes) of the original message, and append zero bits until the
   whole text to be encrypted (including the length information)
   becomes a multiple of block size of 128 bits (16 bytes, 4 words).
   Hence it is sufficient to allocate n_bytes + 19 bytes for the
   output buffer.  *n_bytes_out will be assigned the number of bytes
   written to out.  This function returns always one on encryption or
   on successful decryption.  However, should the given input buffer
   be of incorrect length for the decryption to finish, then the
   function returns zero.  The caller can assume this to indicate an
   attack.

   The password must be at least 16 bytes long.  Excess bytes are
   ignored. */

int hh_xxtea(unsigned char *in, hh_word_t n_bytes_in,
	     unsigned char *out, hh_word_t *n_bytes_out,
	     int encrypt,
	     const unsigned char *password)
{
  hh_word_t key[4], buf[4], backup[4], w = 0;
  unsigned char ch, *in_end = in + n_bytes_in;
  int i, j, is_first_block = 1;

  hh_get_uint32s(password, key, 4);

  if (encrypt) {

    buf[0] = n_bytes_in;
    *n_bytes_out = ((n_bytes_in + 19) >> 4) << 4;
    do {
      for (i = is_first_block; i < 4; i++) {
	for (j = 0; j < 4; j++) {
	  if (in < in_end)
	    ch = *in++;
	  else
	    ch = '\0';
	  w = (w << 8) | ch;
	}
	buf[i] = w;
      }
      hh_xxtea_block(buf, 1, key);
      hh_put_uint32s(out, buf, 4);
      out += 16;
      is_first_block = 0;
      for (j = 0; j < 4; j++)
	key[j] ^= buf[j];
    } while (in < in_end);

  } else {
    /* Decrypt. */

    if (!n_bytes_in)
      /* The prepending of the message length prevents zero-length
	 encrypted messages. */
      return 0;
    if (n_bytes_in & 0xF)
      /* The length of the input is not a multiple of block length. */
      return 0;

    do {
      hh_get_uint32s(in, buf, 4);
      in += 16;
      memcpy(backup, buf, sizeof(backup));
      hh_xxtea_block(buf, 0, key);
      if (is_first_block) {
	/* We're performing the first round.  Dig out the message
	   length. */
	if (((buf[0] + 19) >> 4) << 4 != n_bytes_in)
	  return 0;
	*n_bytes_out = buf[0];
	hh_put_uint32s(out, &buf[1], 3);
	out += 12;
      } else {
	hh_put_uint32s(out, buf, 4);
	out += 16;
      }
      is_first_block = 0;
      for (j = 0; j < 4; j++)
	key[j] ^= backup[j];
    } while (in < in_end);
  }

  return 1;
}


#ifdef TEST_SHA256

int main(void)
{
#define N_BYTES  2000
  unsigned char data[N_BYTES], digest[32], ref[32];
  int i, n_bytes, rnds;
  extern void sha256_ref(unsigned char *data, unsigned long n_bytes,
			 unsigned char *digest);

  for (rnds = 0; rnds < 1000000; rnds++) {
    n_bytes = random() % N_BYTES;
    for (i = 0; i < n_bytes; i++)
      data[i] = random() >> 10;
    hh_sha256(data, n_bytes, digest);
    sha256_ref(data, n_bytes, ref);
    if (memcmp(digest, ref, 32) != 0) {
      printf("n_bytes = %d\ndata = ", n_bytes);
      for (i = 0; i < n_bytes; i++)
	printf("%02x", data[i]);
      printf("\ndigest = ");
      for (i = 0; i < 32; i++)
	printf("%02x", digest[i]);
      printf("\nref    = ");
      for (i = 0; i < 32; i++)
	printf("%02x", ref[i]);
      printf("\n");
      return 1;
    }
  }

  return 0;
}

#endif

#ifdef TEST_XXTEA

#include <stdio.h>

int main(void)
{
#define N  200
#define MSG  "abcdefghijklmnopqrstuvxyz"  
#define PASS  "kukkakukkakukkak"
  unsigned char plain[N], enc[N + 19];
  hh_word_t n_bytes_out;
  int i, len;

  for (len = 0; len < N; len++) {

    printf("Testing length %d\n", len);
    for (i = 0; i < len; i++)
      plain[i] = MSG[i % strlen(MSG)];
    for (i = 0; i < len; i++)
      printf("%c", plain[i]);
    printf("\n");
    
    i = hh_xxtea(plain, len, enc, &n_bytes_out, 1, PASS);
    printf("hh_xxtea returned %d, n_bytes_out = %u\n",
	   i, n_bytes_out);
    if (n_bytes_out > len + 19) {
      printf("Oops...\n");
      return 1;
    }
    for (i = 0; i < n_bytes_out; i++)
      printf("%02x", enc[i]);
    printf("\n");

    memset(plain, 0, N);
    i = hh_xxtea(enc, n_bytes_out, plain, &n_bytes_out, 0, PASS);
    printf("hh_xxtea returned %d, n_bytes_out = %u\n",
	   i, n_bytes_out);
    if (n_bytes_out != len) {
      printf("Oops...\n");
      return 1;
    }
    for (i = 0; i < n_bytes_out; i++)
      printf("%c", plain[i]);
    printf("\n");
  }

  return 0;
}

#endif
