////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

/*
 * Blowfish implementation for PuTTY.
 *
 * Coded from scratch from the algorithm description.
 */

#ifndef _SSH_BLOWFISH_H_
#define _SSH_BLOWFISH_H_

typedef struct {
    word32 S0[256], S1[256], S2[256], S3[256], P[18];
    word32 iv0, iv1;		       /* for CBC mode */
} BlowfishContext;

#endif  // _SSH_BLOWFISH_H_
