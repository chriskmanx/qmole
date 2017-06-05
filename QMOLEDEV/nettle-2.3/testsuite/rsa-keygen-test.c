#include "testutils.h"

#include "knuth-lfib.h"

static void
progress(void *ctx UNUSED, int c)
{
  fputc(c, stderr);
}

int
test_main(void)
{
  struct rsa_public_key pub;
  struct rsa_private_key key;
  
  struct knuth_lfib_ctx lfib;

  mpz_t expected;
  
  mpz_init(expected);
  
  rsa_private_key_init(&key);
  rsa_public_key_init(&pub);

  /* Generate a 1024 bit key with random e */
  knuth_lfib_init(&lfib, 13);

  if (!rsa_generate_keypair(&pub, &key,
			    &lfib, (nettle_random_func *) knuth_lfib_random,
			    NULL, verbose ? progress : NULL,
			    1024, 50))
    FAIL();

  test_rsa_key(&pub, &key);
  
  mpz_set_str(expected,
	      "2554579b857a9da3" "409dbd65c994b701" "aabf7347a78bd730"
	      "1525b5f511f326dd" "c05e1fd6c282faed" "6c79a4eb30539f10"
	      "46db024fe33174f5" "441da5fa175bf781" "8f7117c86cdacf9a"
	      "4589c048cc013eca" "7536d0868aca610a" "c20718e3fec3e835"
	      "f6c2fd920cba1be9" "e94a6c7238d9b2ee" "67ecc9f3d4a9a487"
	      "042190b66582b36a", 16);

  test_rsa_md5(&pub, &key, expected);

  /* Generate a 2000 bit key with fixed e */
  knuth_lfib_init(&lfib, 17);

  mpz_set_ui(pub.e, 17);
  if (!rsa_generate_keypair(&pub, &key,
			    &lfib, (nettle_random_func *) knuth_lfib_random,
			    NULL, verbose ? progress : NULL,
			    2000, 0))
    FAIL();

  test_rsa_key(&pub, &key);

  mpz_set_str(expected,
	      "8c57dfb754270179" "600aced45b45490a" "56715da51f8d029d"
	      "057b58187670f9e4" "1a2da64cd3435483" "16b26c860ca97aed"
	      "fd3ebf5e3dd97226" "1d1b5536da483863" "f9ad5f47437e7a2a"
	      "0119bb0045c64f5a" "cb634d7aff7051b4" "3d01b1c8bc13cc93"
	      "0799d57ada86394f" "1286b5ae9480d167" "8f48e9e78bf3ea4f"
	      "1ed9a9e381c8f046" "3a25bf30238e7c59" "1c9998255141f628"
	      "68e8a01871587152" "38df08281894a5b9" "e9026b55337f9f29"
	      "240793935b05162b" "8fa8d2390d7cb407" "52413dd5f7bccdad"
	      "232e1c27bae30548" "09797991e13dee5d" "c688985433e63050"
	      "6d9a171e144ef442" "eb001166bd95bcdd" "92b3c2e0bd4012a2"
	      "2dd05cb032f6ab6b" "dd7c", 16);

  test_rsa_sha1(&pub, &key, expected);
  
  rsa_private_key_clear(&key);
  rsa_public_key_clear(&pub);
  mpz_clear(expected);

  SUCCESS();
}
