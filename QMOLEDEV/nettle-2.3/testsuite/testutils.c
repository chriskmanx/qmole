/* testutils.c */

#include "testutils.h"

#include "cbc.h"
#include "ctr.h"
#include "knuth-lfib.h"
#include "nettle-internal.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* -1 means invalid */
static const signed char hex_digits[0x100] =
  {
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
    -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  };

void *
xalloc(size_t size)
{
  void *p = malloc(size);
  if (size && !p)
    {
      fprintf(stderr, "Virtual memory exhausted.\n");
      abort();
    }

  return p;
}

unsigned
decode_hex_length(const char *h)
{
  const unsigned char *hex = (const unsigned char *) h;
  unsigned count;
  unsigned i;
  
  for (count = i = 0; hex[i]; i++)
    {
      if (isspace(hex[i]))
	continue;
      if (hex_digits[hex[i]] < 0)
	abort();
      count++;
    }

  if (count % 2)
    abort();
  return count / 2;  
}

int
decode_hex(uint8_t *dst, const char *h)
{  
  const unsigned char *hex = (const unsigned char *) h;
  unsigned i = 0;
  
  for (;;)
  {
    int high, low;
    
    while (*hex && isspace(*hex))
      hex++;

    if (!*hex)
      return 1;

    high = hex_digits[*hex++];
    if (high < 0)
      return 0;

    while (*hex && isspace(*hex))
      hex++;

    if (!*hex)
      return 0;

    low = hex_digits[*hex++];
    if (low < 0)
      return 0;

    dst[i++] = (high << 4) | low;
  }
}

const uint8_t *
decode_hex_dup(const char *hex)
{
  uint8_t *p;
  unsigned length = decode_hex_length(hex);

  p = xalloc(length);

  if (decode_hex(p, hex))
    return p;
  else
    {
      free(p);
      return NULL;
    }
}

void
print_hex(unsigned length, const uint8_t *data)
{
  unsigned i;
  
  for (i = 0; i < length; i++)
    {
      switch (i % 16)
	{
	default:
	  break;
	case 0:
	  printf("\n");
	  break;
	case 8:
	  printf(" ");
	  break;
	}
      printf("%02x", data[i]);
    }
  printf("\n");
}

int verbose = 0;

int
main(int argc, char **argv)
{
  if (argc > 1)
    {
      if (argc == 2 && !strcmp(argv[1], "-v"))
	verbose = 1;
      else
	{
	  fprintf(stderr, "Invalid argument `%s', only accepted option is `-v'.\n",
		  argv[1]);
	  return 1;
	}
    }

  return test_main();
}

void
test_cipher(const struct nettle_cipher *cipher,
	    unsigned key_length,
	    const uint8_t *key,
	    unsigned length,
	    const uint8_t *cleartext,
	    const uint8_t *ciphertext)
{
  void *ctx = xalloc(cipher->context_size);
  uint8_t *data = xalloc(length);

  cipher->set_encrypt_key(ctx, key_length, key);
  cipher->encrypt(ctx, length, data, cleartext);

  if (!MEMEQ(length, data, ciphertext))
    {
      fprintf(stderr, "Encrypt failed:\nInput:");
      print_hex(length, cleartext);
      fprintf(stderr, "\nOutput: ");
      print_hex(length, data);
      fprintf(stderr, "\nExpected:");
      print_hex(length, ciphertext);
      fprintf(stderr, "\n");
      FAIL();
    }
  cipher->set_decrypt_key(ctx, key_length, key);
  cipher->decrypt(ctx, length, data, data);

  if (!MEMEQ(length, data, cleartext))
    {
      fprintf(stderr, "Decrypt failed:\nInput:");
      print_hex(length, ciphertext);
      fprintf(stderr, "\nOutput: ");
      print_hex(length, data);
      fprintf(stderr, "\nExpected:");
      print_hex(length, cleartext);
      fprintf(stderr, "\n");
      FAIL();
    }

  free(ctx);
  free(data);
}

void
test_cipher_cbc(const struct nettle_cipher *cipher,
		unsigned key_length,
		const uint8_t *key,
		unsigned length,
		const uint8_t *cleartext,
		const uint8_t *ciphertext,
		const uint8_t *iiv)
{
  void *ctx = xalloc(cipher->context_size);
  uint8_t *data = xalloc(length);
  uint8_t *iv = xalloc(cipher->block_size);
  
  cipher->set_encrypt_key(ctx, key_length, key);
  memcpy(iv, iiv, cipher->block_size);

  cbc_encrypt(ctx, cipher->encrypt,
	      cipher->block_size, iv,
	      length, data, cleartext);

  if (!MEMEQ(length, data, ciphertext))
    {
      fprintf(stderr, "CBC encrypt failed:\nInput:");
      print_hex(length, cleartext);
      fprintf(stderr, "\nOutput: ");
      print_hex(length, data);
      fprintf(stderr, "\nExpected:");
      print_hex(length, ciphertext);
      fprintf(stderr, "\n");
      FAIL();
    }
  cipher->set_decrypt_key(ctx, key_length, key);
  memcpy(iv, iiv, cipher->block_size);

  cbc_decrypt(ctx, cipher->decrypt,
	      cipher->block_size, iv,
	      length, data, data);

  if (!MEMEQ(length, data, cleartext))
    {
      fprintf(stderr, "CBC decrypt failed:\nInput:");
      print_hex(length, ciphertext);
      fprintf(stderr, "\nOutput: ");
      print_hex(length, data);
      fprintf(stderr, "\nExpected:");
      print_hex(length, cleartext);
      fprintf(stderr, "\n");
      FAIL();
    }
  free(ctx);
  free(data);
  free(iv);
}

void
test_cipher_ctr(const struct nettle_cipher *cipher,
		unsigned key_length,
		const uint8_t *key,
		unsigned length,
		const uint8_t *cleartext,
		const uint8_t *ciphertext,
		const uint8_t *ictr)
{
  void *ctx = xalloc(cipher->context_size);
  uint8_t *data = xalloc(length);
  uint8_t *ctr = xalloc(cipher->block_size);
  
  cipher->set_encrypt_key(ctx, key_length, key);
  memcpy(ctr, ictr, cipher->block_size);

  ctr_crypt(ctx, cipher->encrypt,
	    cipher->block_size, ctr,
	    length, data, cleartext);

  if (!MEMEQ(length, data, ciphertext))
    {
      fprintf(stderr, "CTR encrypt failed:\nInput:");
      print_hex(length, cleartext);
      fprintf(stderr, "\nOutput: ");
      print_hex(length, data);
      fprintf(stderr, "\nExpected:");
      print_hex(length, ciphertext);
      fprintf(stderr, "\n");
      FAIL();
    }

  memcpy(ctr, ictr, cipher->block_size);

  ctr_crypt(ctx, cipher->encrypt,
	    cipher->block_size, ctr,
	    length, data, data);

  if (!MEMEQ(length, data, cleartext))
    {
      fprintf(stderr, "CTR decrypt failed:\nInput:");
      print_hex(length, ciphertext);
      fprintf(stderr, "\nOutput: ");
      print_hex(length, data);
      fprintf(stderr, "\nExpected:");
      print_hex(length, cleartext);
      fprintf(stderr, "\n");
      FAIL();
    }

  free(ctx);
  free(data);
  free(ctr);
}

void
test_cipher_stream(const struct nettle_cipher *cipher,
		   unsigned key_length,
		   const uint8_t *key,
		   unsigned length,
		   const uint8_t *cleartext,
		   const uint8_t *ciphertext)
{
  unsigned block;
  
  void *ctx = xalloc(cipher->context_size);
  uint8_t *data = xalloc(length + 1);
  
  for (block = 1; block <= length; block++)
    {
      unsigned i;

      memset(data, 0x17, length + 1);
      cipher->set_encrypt_key(ctx, key_length, key);

      for (i = 0; i + block < length; i += block)
	{
	  cipher->encrypt(ctx, block, data + i, cleartext + i);
	  if (data[i + block] != 0x17)
	    FAIL();
	}

      cipher->encrypt(ctx, length - i, data + i, cleartext + i);
      if (data[length] != 0x17)
	FAIL();
      
      if (!MEMEQ(length, data, ciphertext))
	{
	  fprintf(stderr, "Encrypt failed, block size %d\nInput:", block);
	  print_hex(length, cleartext);
	  fprintf(stderr, "\nOutput: ");
	  print_hex(length, data);
	  fprintf(stderr, "\nExpected:");
	  print_hex(length, ciphertext);
	  fprintf(stderr, "\n");
	  FAIL();	    
	}
    }
  
  cipher->set_decrypt_key(ctx, key_length, key);
  cipher->decrypt(ctx, length, data, data);

  if (data[length] != 0x17)
    FAIL();

  if (!MEMEQ(length, data, cleartext))
    {
      fprintf(stderr, "Decrypt failed\nInput:");
      print_hex(length, ciphertext);
      fprintf(stderr, "\nOutput: ");
      print_hex(length, data);
      fprintf(stderr, "\nExpected:");
      print_hex(length, cleartext);
      fprintf(stderr, "\n");
      FAIL();	    
    }

  free(ctx);
  free(data);
}

void
test_aead(const struct nettle_aead *aead,
	  unsigned key_length,
	  const uint8_t *key,
	  unsigned auth_length,
	  const uint8_t *authtext,
	  unsigned length,
	  const uint8_t *cleartext,
	  const uint8_t *ciphertext,
	  unsigned iv_length,
	  const uint8_t *iv,
	  const uint8_t *digest)
{
  void *ctx = xalloc(aead->context_size);
  uint8_t *data = xalloc(length);
  uint8_t *buffer = xalloc(aead->block_size);

  /* encryption */
  memset(buffer, 0, aead->block_size);
  aead->set_key(ctx, key_length, key);

  aead->set_iv(ctx, iv_length, iv);

  if (auth_length)
    aead->update(ctx, auth_length, authtext);
    
  if (length)
    aead->encrypt(ctx, length, data, cleartext);

  aead->digest(ctx, aead->block_size, buffer);

  if (!MEMEQ(length, data, ciphertext))
    FAIL();

  if (!MEMEQ(aead->block_size, buffer, digest))
    FAIL();

  /* decryption */
  memset(buffer, 0, aead->block_size);
  aead->set_iv(ctx, iv_length, iv);

  if (auth_length)
    aead->update(ctx, auth_length, authtext);
    
  if (length)
    aead->decrypt(ctx, length, data, data);

  aead->digest(ctx, aead->block_size, buffer);

  if (!MEMEQ(length, data, cleartext))
    FAIL();

  if (!MEMEQ(aead->block_size, buffer, digest))
    FAIL();

  free(ctx);
  free(data);
  free(buffer);
}

void
test_hash(const struct nettle_hash *hash,
	  unsigned length,
	  const uint8_t *data,
	  const uint8_t *digest)
{
  void *ctx = xalloc(hash->context_size);
  uint8_t *buffer = xalloc(hash->digest_size);

  hash->init(ctx);
  hash->update(ctx, length, data);
  hash->digest(ctx, hash->digest_size, buffer);

  if (!MEMEQ(hash->digest_size, digest, buffer))
    FAIL();

  memset(buffer, 0, hash->digest_size);

  hash->init(ctx);
  hash->update(ctx, length, data);
  hash->digest(ctx, hash->digest_size - 1, buffer);

  if (!MEMEQ(hash->digest_size - 1, digest, buffer))
    FAIL();

  if (buffer[hash->digest_size - 1])
    FAIL();

  free(ctx);
  free(buffer);
}

void
test_hash_large(const struct nettle_hash *hash,
		unsigned count, unsigned length,
		uint8_t c,
		const uint8_t *digest)
{
  void *ctx = xalloc(hash->context_size);
  uint8_t *buffer = xalloc(hash->digest_size);
  uint8_t *data = xalloc(length);
  unsigned i;

  memset(data, c, length);

  hash->init(ctx);
  for (i = 0; i < count; i++)
    hash->update(ctx, length, data);
  hash->digest(ctx, hash->digest_size, buffer);

  print_hex(hash->digest_size, buffer);

  if (!MEMEQ(hash->digest_size, digest, buffer))
    FAIL();

  free(ctx);
  free(buffer);
  free(data);
}

void
test_mac(const struct nettle_mac *mac,
	 unsigned key_length, const uint8_t *key,
	 unsigned msg_length, const uint8_t *msg,
	 const uint8_t *digest)
{
  void *ctx = xalloc(mac->context_size);
  uint8_t *buffer = xalloc(mac->digest_size);

  mac->set_key(ctx, key_length, key);
  mac->update(ctx, msg_length, msg);
  mac->digest(ctx, mac->digest_size, buffer);
  ASSERT(MEMEQ(mac->digest_size, digest, buffer));

  free(ctx);
  free(buffer);
}

void
test_armor(const struct nettle_armor *armor,
           unsigned data_length,
           const uint8_t *data,
           const uint8_t *ascii)
{
  unsigned ascii_length = strlen(ascii);
  uint8_t *buffer = xalloc(1 + ascii_length);
  uint8_t *check = xalloc(1 + armor->decode_length(ascii_length));
  void *encode = xalloc(armor->encode_context_size);
  void *decode = xalloc(armor->decode_context_size);
  unsigned done;

  ASSERT(ascii_length
	 <= (armor->encode_length(data_length) + armor->encode_final_length));
  ASSERT(data_length <= armor->decode_length(ascii_length));
  
  memset(buffer, 0x33, 1 + ascii_length);
  memset(check, 0x55, 1 + data_length);

  armor->encode_init(encode);
  
  done = armor->encode_update(encode, buffer, data_length, data);
  done += armor->encode_final(encode, buffer + done);
  ASSERT(done == ascii_length);

  if (!MEMEQ(ascii_length, buffer, ascii))
    FAIL();

  if (0x33 != buffer[strlen(ascii)])
    FAIL();  

  armor->decode_init(decode);
  done = armor->decode_length(ascii_length);

  ASSERT(armor->decode_update(decode, &done, check, ascii_length, buffer));
  ASSERT(done == data_length);
  ASSERT(armor->decode_final(decode));
  
  if (!MEMEQ(data_length, check, data))
    FAIL();

  if (0x55 != check[data_length])
    FAIL();

  free(buffer);
  free(check);
  free(encode);
  free(decode);
}

#if HAVE_LIBGMP
/* Missing in current gmp */
static void
mpz_togglebit (mpz_t x, unsigned long int bit)
{
  if (mpz_tstbit(x, bit))
    mpz_clrbit(x, bit);
  else
    mpz_setbit(x, bit);
}
#endif /* HAVE_LIBGMP */

#if WITH_HOGWEED
#define SIGN(key, hash, msg, signature) do {		\
  hash##_update(&hash, LDATA(msg));			\
  ASSERT(rsa_##hash##_sign(key, &hash, signature));	\
} while(0)

#define VERIFY(key, hash, msg, signature) (	\
  hash##_update(&hash, LDATA(msg)),		\
  rsa_##hash##_verify(key, &hash, signature)	\
)

void
test_rsa_set_key_1(struct rsa_public_key *pub,
		   struct rsa_private_key *key)
{
  /* Initialize key pair for test programs */
  /* 1000-bit key, generated by
   *
   *   lsh-keygen -a rsa -l 1000 -f advanced-hex
   *
   * (private-key (rsa-pkcs1 
   *        (n #69abd505285af665 36ddc7c8f027e6f0 ed435d6748b16088
   *            4fd60842b3a8d7fb bd8a3c98f0cc50ae 4f6a9f7dd73122cc
   *            ec8afa3f77134406 f53721973115fc2d 8cfbba23b145f28d
   *            84f81d3b6ae8ce1e 2850580c026e809b cfbb52566ea3a3b3
   *            df7edf52971872a7 e35c1451b8636d22 279a8fb299368238
   *            e545fbb4cf#)
   *        (e #0db2ad57#)
   *        (d #3240a56f4cd0dcc2 4a413eb4ea545259 5c83d771a1c2ba7b
   *            ec47c5b43eb4b374 09bd2aa1e236dd86 481eb1768811412f
   *            f8d91be3545912af b55c014cb55ceac6 54216af3b85d5c4f
   *            4a32894e3b5dfcde 5b2875aa4dc8d9a8 6afd0ca92ef50d35
   *            bd09f1c47efb4c8d c631e07698d362aa 4a83fd304e66d6c5
   *            468863c307#)
   *        (p #0a66399919be4b4d e5a78c5ea5c85bf9 aba8c013cb4a8732
   *            14557a12bd67711e bb4073fd39ad9a86 f4e80253ad809e5b
   *            f2fad3bc37f6f013 273c9552c9f489#)
   *        (q #0a294f069f118625 f5eae2538db9338c 776a298eae953329
   *            9fd1eed4eba04e82 b2593bc98ba8db27 de034da7daaea795
   *            2d55b07b5f9a5875 d1ca5f6dcab897#)
   *        (a #011b6c48eb592eee e85d1bb35cfb6e07 344ea0b5e5f03a28
   *            5b405396cbc78c5c 868e961db160ba8d 4b984250930cf79a
   *            1bf8a9f28963de53 128aa7d690eb87#)
   *        (b #0409ecf3d2557c88 214f1af5e1f17853 d8b2d63782fa5628
   *            60cf579b0833b7ff 5c0529f2a97c6452 2fa1a8878a9635ab
   *            ce56debf431bdec2 70b308fa5bf387#)
   *        (c #04e103ee925cb5e6 6653949fa5e1a462 c9e65e1adcd60058
   *            e2df9607cee95fa8 daec7a389a7d9afc 8dd21fef9d83805a
   *            40d46f49676a2f6b 2926f70c572c00#)))
   */
  
  mpz_set_str(pub->n,
	      "69abd505285af665" "36ddc7c8f027e6f0" "ed435d6748b16088"
	      "4fd60842b3a8d7fb" "bd8a3c98f0cc50ae" "4f6a9f7dd73122cc"
	      "ec8afa3f77134406" "f53721973115fc2d" "8cfbba23b145f28d"
	      "84f81d3b6ae8ce1e" "2850580c026e809b" "cfbb52566ea3a3b3"
	      "df7edf52971872a7" "e35c1451b8636d22" "279a8fb299368238"
	      "e545fbb4cf", 16);
  mpz_set_str(pub->e, "0db2ad57", 16);

  if (!rsa_public_key_prepare(pub))
    FAIL();
  
  /* d is not used */
#if 0  
  mpz_set_str(key->d,
	      "3240a56f4cd0dcc2" "4a413eb4ea545259" "5c83d771a1c2ba7b"
	      "ec47c5b43eb4b374" "09bd2aa1e236dd86" "481eb1768811412f"
	      "f8d91be3545912af" "b55c014cb55ceac6" "54216af3b85d5c4f"
	      "4a32894e3b5dfcde" "5b2875aa4dc8d9a8" "6afd0ca92ef50d35"
	      "bd09f1c47efb4c8d" "c631e07698d362aa" "4a83fd304e66d6c5"
	      "468863c307", 16);
#endif
  
  mpz_set_str(key->p,
	      "0a66399919be4b4d" "e5a78c5ea5c85bf9" "aba8c013cb4a8732"
	      "14557a12bd67711e" "bb4073fd39ad9a86" "f4e80253ad809e5b"
	      "f2fad3bc37f6f013" "273c9552c9f489", 16);

  mpz_set_str(key->q,
	      "0a294f069f118625" "f5eae2538db9338c" "776a298eae953329"
	      "9fd1eed4eba04e82" "b2593bc98ba8db27" "de034da7daaea795"
	      "2d55b07b5f9a5875" "d1ca5f6dcab897", 16);
  
  mpz_set_str(key->a,
	      "011b6c48eb592eee" "e85d1bb35cfb6e07" "344ea0b5e5f03a28"
	      "5b405396cbc78c5c" "868e961db160ba8d" "4b984250930cf79a"
	      "1bf8a9f28963de53" "128aa7d690eb87", 16);
  
  mpz_set_str(key->b,
	      "0409ecf3d2557c88" "214f1af5e1f17853" "d8b2d63782fa5628"
	      "60cf579b0833b7ff" "5c0529f2a97c6452" "2fa1a8878a9635ab"
	      "ce56debf431bdec2" "70b308fa5bf387", 16);
  
  mpz_set_str(key->c,
	      "04e103ee925cb5e6" "6653949fa5e1a462" "c9e65e1adcd60058"
	      "e2df9607cee95fa8" "daec7a389a7d9afc" "8dd21fef9d83805a"
	      "40d46f49676a2f6b" "2926f70c572c00", 16);

  if (!rsa_private_key_prepare(key))
    FAIL();

  if (pub->size != key->size)
    FAIL();
}

void
test_rsa_md5(struct rsa_public_key *pub,
	     struct rsa_private_key *key,
	     mpz_t expected)
{
  struct md5_ctx md5;
  mpz_t signature;

  md5_init(&md5);
  mpz_init(signature);
  
  SIGN(key, md5, "The magic words are squeamish ossifrage", signature);

  if (verbose)
    {
      fprintf(stderr, "rsa-md5 signature: ");
      mpz_out_str(stderr, 16, signature);
      fprintf(stderr, "\n");
    }

  if (mpz_cmp(signature, expected))
    FAIL();
  
  /* Try bad data */
  if (VERIFY(pub, md5,
	     "The magick words are squeamish ossifrage", signature))
    FAIL();

  /* Try correct data */
  if (!VERIFY(pub, md5,
	      "The magic words are squeamish ossifrage", signature))
    FAIL();

  /* Try bad signature */
  mpz_togglebit(signature, 17);

  if (VERIFY(pub, md5,
	     "The magic words are squeamish ossifrage", signature))
    FAIL();

  mpz_clear(signature);
}

void
test_rsa_sha1(struct rsa_public_key *pub,
	      struct rsa_private_key *key,
	      mpz_t expected)
{
  struct sha1_ctx sha1;
  mpz_t signature;

  sha1_init(&sha1);
  mpz_init(signature);

  SIGN(key, sha1, "The magic words are squeamish ossifrage", signature);

  if (verbose)
    {
      fprintf(stderr, "rsa-sha1 signature: ");
      mpz_out_str(stderr, 16, signature);
      fprintf(stderr, "\n");
    }

  if (mpz_cmp(signature, expected))
    FAIL();
  
  /* Try bad data */
  if (VERIFY(pub, sha1,
	     "The magick words are squeamish ossifrage", signature))
    FAIL();

  /* Try correct data */
  if (!VERIFY(pub, sha1,
	      "The magic words are squeamish ossifrage", signature))
    FAIL();

  /* Try bad signature */
  mpz_togglebit(signature, 17);

  if (VERIFY(pub, sha1,
	     "The magic words are squeamish ossifrage", signature))
    FAIL();

  mpz_clear(signature);
}

void
test_rsa_sha256(struct rsa_public_key *pub,
		struct rsa_private_key *key,
		mpz_t expected)
{
  struct sha256_ctx sha256;
  mpz_t signature;

  sha256_init(&sha256);
  mpz_init(signature);

  SIGN(key, sha256, "The magic words are squeamish ossifrage", signature);

  if (verbose)
    {
      fprintf(stderr, "rsa-sha256 signature: ");
      mpz_out_str(stderr, 16, signature);
      fprintf(stderr, "\n");
    }

  if (mpz_cmp(signature, expected))
    FAIL();
  
  /* Try bad data */
  if (VERIFY(pub, sha256,
	     "The magick words are squeamish ossifrage", signature))
    FAIL();

  /* Try correct data */
  if (!VERIFY(pub, sha256,
	      "The magic words are squeamish ossifrage", signature))
    FAIL();

  /* Try bad signature */
  mpz_togglebit(signature, 17);

  if (VERIFY(pub, sha256,
	     "The magic words are squeamish ossifrage", signature))
    FAIL();

  mpz_clear(signature);
}

void
test_rsa_sha512(struct rsa_public_key *pub,
		struct rsa_private_key *key,
		mpz_t expected)
{
  struct sha512_ctx sha512;
  mpz_t signature;

  sha512_init(&sha512);
  mpz_init(signature);

  SIGN(key, sha512, "The magic words are squeamish ossifrage", signature);

  if (verbose)
    {
      fprintf(stderr, "rsa-sha512 signature: ");
      mpz_out_str(stderr, 16, signature);
      fprintf(stderr, "\n");
    }

  if (mpz_cmp(signature, expected))
    FAIL();
  
  /* Try bad data */
  if (VERIFY(pub, sha512,
	     "The magick words are squeamish ossifrage", signature))
    FAIL();

  /* Try correct data */
  if (!VERIFY(pub, sha512,
	      "The magic words are squeamish ossifrage", signature))
    FAIL();

  /* Try bad signature */
  mpz_togglebit(signature, 17);

  if (VERIFY(pub, sha512,
	     "The magic words are squeamish ossifrage", signature))
    FAIL();

  mpz_clear(signature);
}

#undef SIGN
#undef VERIFY

void
test_rsa_key(struct rsa_public_key *pub,
	     struct rsa_private_key *key)
{
  mpz_t tmp;
  mpz_t phi;
  
  mpz_init(tmp); mpz_init(phi);
  
  if (verbose)
    {
      /* FIXME: Use gmp_printf */
      fprintf(stderr, "Public key: n=");
      mpz_out_str(stderr, 16, pub->n);
      fprintf(stderr, "\n    e=");
      mpz_out_str(stderr, 16, pub->e);

      fprintf(stderr, "\n\nPrivate key: d=");
      mpz_out_str(stderr, 16, key->d);
      fprintf(stderr, "\n    p=");
      mpz_out_str(stderr, 16, key->p);
      fprintf(stderr, "\n    q=");
      mpz_out_str(stderr, 16, key->q);
      fprintf(stderr, "\n    a=");
      mpz_out_str(stderr, 16, key->a);
      fprintf(stderr, "\n    b=");
      mpz_out_str(stderr, 16, key->b);
      fprintf(stderr, "\n    c=");
      mpz_out_str(stderr, 16, key->c);
      fprintf(stderr, "\n\n");
    }

  /* Check n = p q */
  mpz_mul(tmp, key->p, key->q);
  if (mpz_cmp(tmp, pub->n))
    FAIL();

  /* Check c q = 1 mod p */
  mpz_mul(tmp, key->c, key->q);
  mpz_fdiv_r(tmp, tmp, key->p);
  if (mpz_cmp_ui(tmp, 1))
    FAIL();

  /* Check ed = 1 (mod phi) */
  mpz_sub_ui(phi, key->p, 1);
  mpz_sub_ui(tmp, key->q, 1);

  mpz_mul(phi, phi, tmp);

  mpz_mul(tmp, pub->e, key->d);
  mpz_fdiv_r(tmp, tmp, phi);
  if (mpz_cmp_ui(tmp, 1))
    FAIL();

  /* Check a e = 1 (mod (p-1) ) */
  mpz_sub_ui(phi, key->p, 1);
  mpz_mul(tmp, pub->e, key->a);
  mpz_fdiv_r(tmp, tmp, phi);
  if (mpz_cmp_ui(tmp, 1))
    FAIL();
  
  /* Check b e = 1 (mod (q-1) ) */
  mpz_sub_ui(phi, key->q, 1);
  mpz_mul(tmp, pub->e, key->b);
  mpz_fdiv_r(tmp, tmp, phi);
  if (mpz_cmp_ui(tmp, 1))
    FAIL();
  
  mpz_clear(tmp); mpz_clear(phi);
}

/* Requires that the context is named like the hash algorithm. */
#define DSA_VERIFY(key, hash, msg, signature)	\
  (hash##_update(&hash, LDATA(msg)),		\
   dsa_##hash##_verify(key, &hash, signature))

void
test_dsa160(const struct dsa_public_key *pub,
	    const struct dsa_private_key *key,
	    const struct dsa_signature *expected)
{
  struct sha1_ctx sha1;
  struct dsa_signature signature;
  struct knuth_lfib_ctx lfib;
  
  sha1_init(&sha1);
  dsa_signature_init(&signature);
  knuth_lfib_init(&lfib, 1111);
  
  sha1_update(&sha1, LDATA("The magic words are squeamish ossifrage"));
  ASSERT (dsa_sha1_sign(pub, key,
			&lfib, (nettle_random_func *) knuth_lfib_random,
			&sha1, &signature));

  if (verbose)
    {
      fprintf(stderr, "dsa160 signature: ");
      mpz_out_str(stderr, 16, signature.r);
      fprintf(stderr, ", ");
      mpz_out_str(stderr, 16, signature.s);
      fprintf(stderr, "\n");
    }

  if (expected)
    if (mpz_cmp (signature.r, expected->r)
	|| mpz_cmp (signature.s, expected->s))
      FAIL();
  
  /* Try bad data */
  if (DSA_VERIFY(pub, sha1,
		 "The magick words are squeamish ossifrage", &signature))
    FAIL();

  /* Try correct data */
  if (!DSA_VERIFY(pub, sha1,
		 "The magic words are squeamish ossifrage", &signature))
    FAIL();

  /* Try bad signature */
  mpz_togglebit(signature.r, 17);

  if (DSA_VERIFY(pub, sha1,
		 "The magic words are squeamish ossifrage", &signature))
    FAIL();

  dsa_signature_clear(&signature);
}

void
test_dsa256(const struct dsa_public_key *pub,
	    const struct dsa_private_key *key,
	    const struct dsa_signature *expected)
{
  struct sha256_ctx sha256;
  struct dsa_signature signature;
  struct knuth_lfib_ctx lfib;
  
  sha256_init(&sha256);
  dsa_signature_init(&signature);
  knuth_lfib_init(&lfib, 1111);
  
  sha256_update(&sha256, LDATA("The magic words are squeamish ossifrage"));
  ASSERT (dsa_sha256_sign(pub, key,
			&lfib, (nettle_random_func *) knuth_lfib_random,
			&sha256, &signature));
  
  if (verbose)
    {
      fprintf(stderr, "dsa256 signature: ");
      mpz_out_str(stderr, 16, signature.r);
      fprintf(stderr, ", ");
      mpz_out_str(stderr, 16, signature.s);
      fprintf(stderr, "\n");
    }

  if (expected)
    if (mpz_cmp (signature.r, expected->r)
	|| mpz_cmp (signature.s, expected->s))
      FAIL();
  
  /* Try bad data */
  if (DSA_VERIFY(pub, sha256,
		 "The magick words are squeamish ossifrage", &signature))
    FAIL();

  /* Try correct data */
  if (!DSA_VERIFY(pub, sha256,
		 "The magic words are squeamish ossifrage", &signature))
    FAIL();

  /* Try bad signature */
  mpz_togglebit(signature.r, 17);

  if (DSA_VERIFY(pub, sha256,
		 "The magic words are squeamish ossifrage", &signature))
    FAIL();

  dsa_signature_clear(&signature);
}

void
test_dsa_key(struct dsa_public_key *pub,
	     struct dsa_private_key *key,
	     unsigned q_size)
{
  mpz_t t;

  mpz_init(t);

  ASSERT(mpz_sizeinbase(pub->q, 2) == q_size);
  ASSERT(mpz_sizeinbase(pub->p, 2) >= DSA_SHA1_MIN_P_BITS);
  
  ASSERT(mpz_probab_prime_p(pub->p, 10));

  ASSERT(mpz_probab_prime_p(pub->q, 10));

  mpz_fdiv_r(t, pub->p, pub->q);

  ASSERT(0 == mpz_cmp_ui(t, 1));

  ASSERT(mpz_cmp_ui(pub->g, 1) > 0);
  
  mpz_powm(t, pub->g, pub->q, pub->p);
  ASSERT(0 == mpz_cmp_ui(t, 1));
  
  mpz_powm(t, pub->g, key->x, pub->p);
  ASSERT(0 == mpz_cmp(t, pub->y));
};

#endif /* WITH_HOGWEED */

