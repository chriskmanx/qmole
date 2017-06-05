#include "testutils.h"
#include "ripemd160.h"

int
test_main(void)
{
  test_hash(&nettle_ripemd160, 0, "",
      H("9c1185a5c5e9fc54612808977ee8f548b2258d31"));

  test_hash(&nettle_ripemd160, 1, "a",
      H("0bdc9d2d256b3ee9daae347be6f4dc835a467ffe"));

  test_hash(&nettle_ripemd160, 3, "abc",
      H("8eb208f7e05d987a9b044a8e98c6b087f15a0bfc"));

  test_hash(&nettle_ripemd160, 26, "abcdefghijklmnopqrstuvwxyz",
      H("f71c27109c692c1b56bbdceb5b9d2865b3708dbc"));

  test_hash(&nettle_ripemd160, 14, "message digest",
      H("5d0689ef49d2fae572b881b123a85ffa21595f36"));

  test_hash(&nettle_ripemd160, 62,
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "abcdefghijklmnopqrstuvwxyz0123456789",
      H("b0e20b6e3116640286ed3a87a5713079b21f5189"));

  test_hash(&nettle_ripemd160,  80,
      "1234567890123456789012345678901234567890"
      "1234567890123456789012345678901234567890",
      H("9b752e45573d4b39f4dbd3323cab82bf63326bfb"));

  /* Additional test vector, from Daniel Kahn Gillmor */
  test_hash(&nettle_ripemd160, LDATA("38"),
      H("6b2d075b1cd34cd1c3e43a995f110c55649dad0e"));

  SUCCESS();
}
