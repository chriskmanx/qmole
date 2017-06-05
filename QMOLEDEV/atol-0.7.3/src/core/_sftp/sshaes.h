////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

/*
 * aes.c - implementation of AES / Rijndael
 * 
 * AES is a flexible algorithm as regards endianness: it has no
 * inherent preference as to which way round you should form words
 * from the input byte stream. It talks endlessly of four-byte
 * _vectors_, but never of 32-bit _words_ - there's no 32-bit
 * addition at all, which would force an endianness by means of
 * which way the carries went. So it would be possible to write a
 * working AES that read words big-endian, and another working one
 * that read them little-endian, just by computing a different set
 * of tables - with no speed drop.
 * 
 * It's therefore tempting to do just that, and remove the overhead
 * of GET_32BIT_MSB_FIRST() et al, allowing every system to use its
 * own endianness-native code; but I decided not to, partly for
 * ease of testing, and mostly because I like the flexibility that
 * allows you to encrypt a non-word-aligned block of memory (which
 * many systems would stop being able to do if I went the
 * endianness-dependent route).
 * 
 * This implementation reads and stores words big-endian, but
 * that's a minor implementation detail. By flipping the endianness
 * of everything in the E0..E3, D0..D3 tables, and substituting
 * GET_32BIT_LSB_FIRST for GET_32BIT_MSB_FIRST, I could create an
 * implementation that worked internally little-endian and gave the
 * same answers at the same speed.
 */

#ifndef _SSHAES_H_INCLUDED
#define _SSHAES_H_INCLUDED

#define MAX_NR 14		       /* max no of rounds */
#define MAX_NK 8		       /* max no of words in input key */
#define MAX_NB 8		       /* max no of words in cipher blk */

typedef unsigned int word32;
typedef struct AESContext AESContext;

struct AESContext {
    word32 keysched[(MAX_NR + 1) * MAX_NB];
    word32 invkeysched[(MAX_NR + 1) * MAX_NB];
    void (*encrypt) (AESContext * ctx, word32 * block);
    void (*decrypt) (AESContext * ctx, word32 * block);
    word32 iv[MAX_NB];
    int Nb, Nr;
};

#endif // _SSHAES_H_INCLUDED

