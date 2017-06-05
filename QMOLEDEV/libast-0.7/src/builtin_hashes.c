/*
 * Copyright (C) 1997-2004, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

static const char __attribute__((unused)) cvs_ident[] = "$Id: builtin_hashes.c,v 1.3 2004/07/23 21:38:39 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

#define BUILTIN_RANDOM_SEED   (SPIF_CAST(uint32) 0xf721b64d)

/*
 * Bob Jenkins' hash algorithm as published in December 1996.  Public
 * domain.  See http://burtleburtle.net/bob/hash/
 */

/**
 * Hashes a variable-length key into a 32-bit unsigned integer value.
 *
 * This function hashes a bitstream of a given length into a 32-bit
 * hash value suitable for use in hash tables.  Note that this
 * function should NOT be used for cryptography.  About 6n+36
 * instructions for an n-byte key.
 *
 * For a hash value of w bits, the returned value should be
 * bitwise-AND'd with SPIFHASH_MASK(w), and the hash table should
 * have SPIFHASH_SIZE(w) buckets.
 *
 * @param key    Pointer to bitstream holding key.
 * @param length Number of bytes in bitstream.
 * @param seed   The last hash value returned, or an arbitrary seed
 *               value.
 * @return       A 32-bit hash value.
 *
 */
spif_uint32_t
spifhash_jenkins(register spif_uint8_t *key, register spif_uint32_t length, register spif_uint32_t seed)
{
    register spif_uint32_t a, b, c, len;

    len = length;
    a = b = BUILTIN_RANDOM_SEED;  /* This can be any 32-bit value. */
    c = seed;

    /* The loop below handles most of the key (all but the last
       length % 12 bytes). */
    while (len >= 12) {
        a += (key[0] + (SPIF_CAST(uint32) key[1] << 8) + (SPIF_CAST(uint32) key[2] << 16) + (SPIF_CAST(uint32) key[3] << 24));
        b += (key[4] + (SPIF_CAST(uint32) key[5] << 8) + (SPIF_CAST(uint32) key[6] << 16) + (SPIF_CAST(uint32) key[7] << 24));
        c += (key[8] + (SPIF_CAST(uint32) key[9] << 8) + (SPIF_CAST(uint32) key[10] << 16) + (SPIF_CAST(uint32) key[11] << 24));
        SPIFHASH_JENKINS_MIX(a, b, c);
        key += 12;
        len -= 12;
    }

    /* The switch below handles the last length % 12 (0 through 11)
       bytes.  All cases drop through to the next case. */
    c += length;
    switch (len) {
        case 11:  c += (SPIF_CAST(uint32) key[10] << 24);
        case 10:  c += (SPIF_CAST(uint32) key[9] << 16);
        case 9:   c += (SPIF_CAST(uint32) key[8] << 8);
        case 8:   b += (SPIF_CAST(uint32) key[7] << 24);
        case 7:   b += (SPIF_CAST(uint32) key[6] << 16);
        case 6:   b += (SPIF_CAST(uint32) key[5] << 8);
        case 5:   b += key[4];
        case 4:   a += (SPIF_CAST(uint32) key[3] << 24);
        case 3:   a += (SPIF_CAST(uint32) key[2] << 16);
        case 2:   a += (SPIF_CAST(uint32) key[1] << 8);
        case 1:   a += key[0];
        /* case 0: nothing left to add */
    }
    SPIFHASH_JENKINS_MIX(a, b, c);

    return c;
}

/**
 * Hashes a variable-length key into a 32-bit unsigned integer value.
 *
 * This function hashes a series of 32-bit unsigned integers of a
 * given length into a 32-bit hash value suitable for use in hash
 * tables.  This hash is basically identical to spifhash_jenkins(), except
 * that the key length must be a whole number of 32-bit dword's, and
 * the length is given in spif_uint32_t's, not bytes.  It is much
 * faster than spifhash_jenkins(), so if padding keys to 32 bit chunks is
 * inexpensive, this function is probably preferable.
 *
 * @param key    Pointer to bitstream holding key.
 * @param length Number of 32-bit integers in bitstream.
 * @param seed   The last hash value returned, or an arbitrary seed
 *               value.
 * @return       A 32-bit hash value.
 *
 */
spif_uint32_t
spifhash_jenkins32(spif_uint8_t *key, register spif_uint32_t length, register spif_uint32_t seed)
{
    register spif_uint32_t a, b, c, len;
    register spif_uint32_t *key_dword = SPIF_CAST_PTR(uint32) key;

    len = length;
    a = b = BUILTIN_RANDOM_SEED;  /* This can be any 32-bit value. */
    c = seed;

    /* The loop below handles most of the key (all but the last
       length % 3 uint32's). */
    while (len >= 3) {
        a += key_dword[0];
        b += key_dword[1];
        c += key_dword[2];
        SPIFHASH_JENKINS_MIX(a, b, c);
        key_dword += 3;
        len -= 3;
    }

    /* The switch below handles the last length % 3 (0 through 2)
       uint32's.  All cases drop through to the next case. */
    c += length;
    switch (len) {
        case 2:  b += key_dword[1];
        case 1:  a += key_dword[0];
        /* case 0: nothing left to add */
    }
    SPIFHASH_JENKINS_MIX(a, b, c);

    return c;
}

#if !(WORDS_BIGENDIAN)
/**
 * Hashes a variable-length key into a 32-bit unsigned integer value.
 *
 * This function hashes a bitstream of a given length into a 32-bit
 * hash value suitable for use in hash tables.  This hash is basically
 * identical to spifhash_jenkins(), except that it only works on
 * little-endian machines (e.g., Intel x86 and VAX).  It is faster
 * than spifhash_jenkins(), so it should be preferred on little-endian
 * systems.
 *
 * @param key    Pointer to bitstream holding key.
 * @param length Number of bytes in bitstream.
 * @param seed   The last hash value returned, or an arbitrary seed
 *               value.
 * @return       A 32-bit hash value.
 *
 */
spif_uint32_t
spifhash_jenkinsLE(register spif_uint8_t *key, register spif_uint32_t length, register spif_uint32_t seed)
{
    register spif_uint32_t a, b, c, len;

    len = length;
    a = b = BUILTIN_RANDOM_SEED;  /* This can be any 32-bit value. */
    c = seed;

    /* The loop below handles most of the key (all but the last
       length % 12 bytes). */
    if ((SPIF_CAST(uint32) key) & 3) {
        /* Not 32-bit aligned.  Use old method. */
        while (len >= 12) {
            a += (key[0] + (SPIF_CAST(uint32) key[1] << 8) + (SPIF_CAST(uint32) key[2] << 16) + (SPIF_CAST(uint32) key[3] << 24));
            b += (key[4] + (SPIF_CAST(uint32) key[5] << 8) + (SPIF_CAST(uint32) key[6] << 16) + (SPIF_CAST(uint32) key[7] << 24));
            c += (key[8] + (SPIF_CAST(uint32) key[9] << 8) + (SPIF_CAST(uint32) key[10] << 16) + (SPIF_CAST(uint32) key[11] << 24));
            SPIFHASH_JENKINS_MIX(a, b, c);
            key += 12;
            len -= 12;
        }
    } else {
        /* 32-bit aligned.  Use speedier method. */
        while (len >= 12) {
            /* These three lines are the only ones which differ from
               spifhash_jenkins(). */
            a += *(SPIF_CAST_PTR(uint32) key);
            b += *(SPIF_CAST_PTR(uint32) (key + 4));
            c += *(SPIF_CAST_PTR(uint32) (key + 8));
            SPIFHASH_JENKINS_MIX(a, b, c);
            key += 12;
            len -= 12;
        }
    }

    /* The switch below handles the last length % 12 (0 through 11)
       bytes.  All cases drop through to the next case. */
    c += length;
    switch (len) {
      case 11:
          c += (SPIF_CAST(uint32) key[10] << 24);
      case 10:
          c += (SPIF_CAST(uint32) key[9] << 16);
      case 9:
          c += (SPIF_CAST(uint32) key[8] << 8);
      case 8:
          b += (SPIF_CAST(uint32) key[7] << 24);
      case 7:
          b += (SPIF_CAST(uint32) key[6] << 16);
      case 6:
          b += (SPIF_CAST(uint32) key[5] << 8);
      case 5:
          b += key[4];
      case 4:
          a += (SPIF_CAST(uint32) key[3] << 24);
      case 3:
          a += (SPIF_CAST(uint32) key[2] << 16);
      case 2:
          a += (SPIF_CAST(uint32) key[1] << 8);
      case 1:
          a += key[0];
          /* case 0: nothing left to add */
    }
    SPIFHASH_JENKINS_MIX(a, b, c);

    return c;
}
#endif


/**
 * The rotating hash.
 *
 * This is an implementation of a rotating hash algorithm with a
 * slight modification so that it can be used with a 32-bit bitmask
 * and a power-of-two table size (so as to avoid the expensive
 * "hash % prime" step).
 *
 * This algorithm uses 8n+6 instructions to hash a key of n bytes.
 * The mix step is a left bitwise rotation by 4, so systems with a
 * rotate instruction will save 2 instructions per loop.
 *
 * @param key  The arbitrary-length key.
 * @param len  The length of the key, in bytes.
 * @param seed An arbitrary seed value.
 * @return     A 32-bit hash value.
 *
 */
spif_uint32_t
spifhash_rotating(spif_uint8_t *key, spif_uint32_t len, spif_uint32_t seed)
{
    spif_uint32_t hash, i;

    if (!seed) {
        seed = BUILTIN_RANDOM_SEED;
    }
    for (hash = seed, i = 0; i < len; i++) {
        hash = (hash << 4) ^ (hash >> 28) ^ key[i];
    }
    return (hash ^ (hash >> 10) ^ (hash >> 20));
}

/**
 * The one-at-a-time hash.
 *
 * This is an implementation of the one-at-a-time hash.  It is similar
 * to spifhash_rotating().  It uses 9n+9 instructions to hash a key of n
 * bytes.  A full 32-bit key is produced.  No funneling weaknesses are
 * known.
 *
 * @param key  The arbitrary-length key.
 * @param len  The length of the key, in bytes.
 * @param seed An arbitrary seed value.
 * @return     A 32-bit hash value.
 *
 */
spif_uint32_t
spifhash_one_at_a_time(spif_uint8_t *key, spif_uint32_t len, spif_uint32_t seed)
{
    spif_uint32_t hash, i;

    if (!seed) {
        seed = BUILTIN_RANDOM_SEED;
    }
    for (hash = seed, i = 0; i < len; i++) {
        hash += key[i];
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    return hash;
}

/**
 * The FNV hash.
 *
 * This is the Fowler/Noll/Vo (FNV) hash.  This code is a modified
 * version of that which appears at
 * http://www.isthe.com/chongo/tech/comp/fnv/
 *
 * @param key  The arbitrary-length key.
 * @param len  The length of the key, in bytes.
 * @param seed An arbitrary seed value.
 * @return     A 32-bit hash value.
 *
 */
spif_uint32_t
spifhash_fnv(spif_uint8_t *key, spif_uint32_t len, spif_uint32_t seed)
{
    spif_uint8_t *key_end = key + len;
    spif_uint32_t hash;

    if (!seed) {
        seed = SPIF_CAST(uint32) 0x811c9dc5;    /* FNV-1a 32-bit init. */
    }
    for (hash = seed; key < key_end; key++) {
        hash ^= SPIF_CAST(uint32) (*key);

#ifdef __GNUC__
        hash += (hash << 1) + (hash << 4) + (hash << 7) + (hash << 8) + (hash << 24);
#else
        hash *= SPIF_CAST(uint32) 0x01000193;   /* 32-bit FNV-1 prime. */
#endif
    }

    return hash;
}
