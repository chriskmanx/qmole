#include "testutils.h"
#include "nettle-internal.h"
#include "des.h"

static void
test_des(const uint8_t *key, int expected_parity,
	 unsigned length,
	 const uint8_t *cleartext,
	 const uint8_t *ciphertext)
{
  struct des_ctx ctx;
  uint8_t *data = xalloc(length);

  if (des_check_parity(8, key) != expected_parity)
    FAIL();

  if (!des_set_key(&ctx, key))
    FAIL();

  des_encrypt(&ctx, length, data, cleartext);

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

  des_decrypt(&ctx, length, data, data);

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

  free(data);
}

static void
test_weak(const uint8_t *key)
{
  struct des_ctx ctx;

  if (des_set_key(&ctx, key))
    FAIL();
}

int
test_main(void)
{
  /* From Applied Cryptography */
  test_des(H("01234567 89ABCDEF"), 1,
	   HL("01234567 89ABCDE7"),
	   H("C9574425 6A5ED31D"));

  test_des(H("01 01 01 01 01 01 01 80"), 1,
	   HL("00 00 00 00 00 00 00 00"),
	   H("9C C6 2D F4 3B 6E ED 74"));

  test_des(H("80 01 01 01 01 01 01 01"), 1,
	   HL("00 00 00 00 00 00 00 40"),
	   H("A3 80 E0 2A 6B E5 46 96"));

  test_des(H("08 19 2A 3B 4C 5D 6E 7F"), 1,
	   HL("00 00 00 00 00 00 00 00"),
	   H("25 DD AC 3E 96 17 64 67"));

  test_des(H("01 23 45 67 89 AB CD EF"), 1,
	   DES_BLOCK_SIZE, "Now is t",
	   H("3F A4 0E 8A 98 4D 48 15"));

  /* Same key, but with one bad parity bit, */
  test_des(H("01 23 45 66 89 AB CD EF"), 0,
	   DES_BLOCK_SIZE, "Now is t",
	   H("3F A4 0E 8A 98 4D 48 15"));

  /* Parity check */
  if (des_check_parity(HL("01 01 01 01 01 01 01 00")))
    FAIL();

  /* The four weak keys */
  test_weak(H("01 01 01 01 01 01 01 01"));  
  test_weak(H("FE FE FE FE FE FE FE FE"));
  test_weak(H("1F 1F 1F 1F 0E 0E 0E 0E"));
  test_weak(H("E0 E0 E0 E0 F1 F1 F1 F1"));

  /* Same weak key, but different parity. */
  test_weak(H("E0 E0 E0 E0 F0 F1 F1 F1"));

  /* The six pairs of semiweak keys */
  test_weak(H("01 FE 01 FE 01 FE 01 FE"));
  test_weak(H("FE 01 FE 01 FE 01 FE 01"));

  test_weak(H("1F E0 1F E0 0E F1 0E F1"));
  test_weak(H("E0 1F E0 1F F1 0E F1 0E"));

  test_weak(H("01 E0 01 E0 01 F1 01 F1"));
  test_weak(H("E0 01 E0 01 F1 01 F1 01"));

  test_weak(H("1F FE 1F FE 0E FE 0E FE"));
  test_weak(H("FE 1F FE 1F FE 0E FE 0E"));

  test_weak(H("01 1F 01 1F 01 0E 01 0E"));
  test_weak(H("1F 01 1F 01 0E 01 0E 01"));

  test_weak(H("E0 FE E0 FE F1 FE F1 FE"));
  test_weak(H("FE E0 FE E0 FE F1 FE F1"));

  SUCCESS();
}
