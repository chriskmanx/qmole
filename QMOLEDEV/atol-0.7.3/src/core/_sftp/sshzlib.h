////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef _ZLIB_H_INCLUDED
#define _ZLIB_H_INCLUDED

/*
 * zlib compression.
 */
void zlib_compress_init(CSshSession &session);
void zlib_decompress_init(CSshSession &session);
int zlib_compress_block(unsigned char *block, int len,
			unsigned char **outblock, int *outlen);
int zlib_decompress_block(unsigned char *block, int len,
			  unsigned char **outblock, int *outlen);

#endif //_ZLIB_H_INCLUDED