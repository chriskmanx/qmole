#include "testutils.h"
#include "sha.h"

int
test_main(void)
{
  test_hash(&nettle_sha512, 3, "abc",
	    H("ddaf35a193617aba cc417349ae204131"
	      "12e6fa4e89a97ea2 0a9eeee64b55d39a"
	      "2192992a274fc1a8 36ba3c23a3feebbd"
	      "454d4423643ce80e 2a9ac94fa54ca49f"));
  
  test_hash(&nettle_sha512, 112,
	    "abcdefghbcdefghicdefghijdefg"
	    "hijkefghijklfghijklmghijklmn"
	    "hijklmnoijklmnopjklmnopqklmn"
	    "opqrlmnopqrsmnopqrstnopqrstu",
	    H("8e959b75dae313da 8cf4f72814fc143f"
	      "8f7779c6eb9f7fa1 7299aeadb6889018"
	      "501d289e4900f7e4 331b99dec4b5433a"
	      "c7d329eeb6dd2654 5e96e55b874be909"));

  /* Additional test vectors, from Daniel Kahn Gillmor */
  test_hash(&nettle_sha512, LDATA(""),
	    H("cf83e1357eefb8bd f1542850d66d8007"
	      "d620e4050b5715dc 83f4a921d36ce9ce"
	      "47d0d13c5d85f2b0 ff8318d2877eec2f"
	      "63b931bd47417a81 a538327af927da3e"));
  test_hash(&nettle_sha512, LDATA("a"),
	    H("1f40fc92da241694 750979ee6cf582f2"
	      "d5d7d28e18335de0 5abc54d0560e0f53"
	      "02860c652bf08d56 0252aa5e74210546"
	      "f369fbbbce8c12cf c7957b2652fe9a75"));
  test_hash(&nettle_sha512, LDATA("38"),
	    H("caae34a5e8103126 8bcdaf6f1d8c04d3"
	      "7b7f2c349afb705b 575966f63e2ebf0f"
	      "d910c3b05160ba08 7ab7af35d40b7c71"
	      "9c53cd8b947c9611 1f64105fd45cc1b2"));
  test_hash(&nettle_sha512, LDATA("message digest"),
	    H("107dbf389d9e9f71 a3a95f6c055b9251"
	      "bc5268c2be16d6c1 3492ea45b0199f33"
	      "09e16455ab1e9611 8e8a905d5597b720"
	      "38ddb372a8982604 6de66687bb420e7c"));
  test_hash(&nettle_sha512, LDATA("abcdefghijklmnopqrstuvwxyz"),
	    H("4dbff86cc2ca1bae 1e16468a05cb9881"
	      "c97f1753bce36190 34898faa1aabe429"
	      "955a1bf8ec483d74 21fe3c1646613a59"
	      "ed5441fb0f321389 f77f48a879c7b1f1"));
  test_hash(&nettle_sha512,
	    LDATA("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef"
		  "ghijklmnopqrstuvwxyz0123456789"),
	    H("1e07be23c26a86ea 37ea810c8ec78093"
	      "52515a970e9253c2 6f536cfc7a9996c4"
	      "5c8370583e0a78fa 4a90041d71a4ceab"
	      "7423f19c71b9d5a3 e01249f0bebd5894"));
  test_hash(&nettle_sha512,
	    LDATA("12345678901234567890123456789012"
		  "34567890123456789012345678901234"
		  "5678901234567890"),
	    H("72ec1ef1124a45b0 47e8b7c75a932195"
	      "135bb61de24ec0d1 914042246e0aec3a"
	      "2354e093d76f3048 b456764346900cb1"
	      "30d2a4fd5dd16abb 5e30bcb850dee843"));

  SUCCESS();
}
