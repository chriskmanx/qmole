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


/* Compute a 256-bit (32-byte) cryptographically strong hash value of
   the given `n_bytes' in `data'.  The `digest' must refer to at least
   32 bytes of memory which are overwritten by the hash. */

void hh_sha256(unsigned char *data, unsigned long n_bytes,
	       unsigned char *digest);


/* Encrypt or decrypt, depending on whether `encrypt' is non-zero or
   zero, the given `n_bytes_in' bytes in `in' with the given 16 bytes
   of password data in `password'.  The `out' must refer to at least
   `n_bytes' + 19 bytes of memory, some of which may be unused.  The
   `*n_bytes_out' is assigned the actual number of bytes in the
   en/decrypted message.

   This function returns always one on encryption or on successful
   decryption.  However, should the given input buffer be of incorrect
   length for the decryption to finish, then the function returns
   zero.  The caller can assume this to indicate an attack. */

int hh_xxtea(unsigned char *in, hh_word_t n_bytes_in,
	     unsigned char *out, hh_word_t *n_bytes_out,
	     int encrypt,
	     const unsigned char *password);
