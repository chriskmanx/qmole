#include "testutils.h"

#include "rsa.h"
#include "knuth-lfib.h"

int
test_main(void)
{
  struct rsa_public_key pub;
  struct rsa_private_key key;
  struct knuth_lfib_ctx lfib;

  /* FIXME: How is this spelled? */
  const uint8_t *msg = "Squemish ossifrage";
  unsigned msg_length;

  uint8_t *decrypted;
  unsigned decrypted_length;
  
  mpz_t gibberish;

  rsa_private_key_init(&key);
  rsa_public_key_init(&pub);
  mpz_init(gibberish);

  knuth_lfib_init(&lfib, 17);
  
  test_rsa_set_key_1(&pub, &key);
  msg_length = strlen(msg);

  if (verbose)
    fprintf(stderr, "msg: `%s', length = %d\n", msg, msg_length);
  
  ASSERT(rsa_encrypt(&pub,
		     &lfib, (nettle_random_func *) knuth_lfib_random,
		     msg_length, msg,
		     gibberish));

  if (verbose)
    {
      /* In which GMP version was gmp_fprintf introduced? */
      fprintf(stderr, "encrypted: ");
      mpz_out_str(stderr, 10, gibberish);
    }
  
  decrypted = xalloc(msg_length + 1);

  decrypted_length = msg_length - 1;
  ASSERT(!rsa_decrypt(&key, &decrypted_length, decrypted, gibberish));

  decrypted_length = msg_length;
  ASSERT(rsa_decrypt(&key, &decrypted_length, decrypted, gibberish));
  ASSERT(decrypted_length == msg_length);
  ASSERT(MEMEQ(msg_length, msg, decrypted));

  decrypted_length = key.size;
  ASSERT(rsa_decrypt(&key, &decrypted_length, decrypted, gibberish));
  ASSERT(decrypted_length == msg_length);
  ASSERT(MEMEQ(msg_length, msg, decrypted));
  
  rsa_private_key_clear(&key);
  rsa_public_key_clear(&pub);
  mpz_clear(gibberish);
  free(decrypted);
  SUCCESS();
}
  
