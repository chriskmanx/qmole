////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef _SHA_H_INCLUDED
#define _SHA_H_INCLUDED

typedef unsigned int word32;
typedef unsigned int uint32;

typedef struct {
    uint32 h[5];
    unsigned char block[64];
    int blkused;
    uint32 lenhi, lenlo;
} SHA_State;

void SHA_Init(SHA_State * s);
void SHA_Bytes(SHA_State * s, void *p, int len);
void SHA_Final(SHA_State * s, unsigned char *output);
void SHA_Simple(void *p, int len, unsigned char *output);

#ifndef MSCRYPTOAPI
void SHATransform(word32 * digest, word32 * data);
#endif

void hmac_sha1_simple(void *key, int keylen, void *data, int datalen,
		      unsigned char *output);

#endif	// _SHA_H_INCLUDED
