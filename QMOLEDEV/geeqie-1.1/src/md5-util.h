/*
 * This code implements the MD5 message-digest algorithm.
 * The algorithm is due to Ron Rivest.  This code was
 * written by Colin Plumb in 1993, no copyright is claimed.
 * This code is in the public domain; do with it what you wish.
 *
 * Equivalent code is available from RSA Data Security, Inc.
 * This code has been tested against that, and is equivalent,
 * except that you don't need to include two pages of legalese
 * with every copy.
 *
 * To compute the message digest of a chunk of bytes, declare an
 * MD5Context structure, pass it to rpmMD5Init, call rpmMD5Update as
 * needed on buffers full of bytes, and then call rpmMD5Final, which
 * will fill a supplied 16-byte array with the digest.
 */

/* parts of this file are :
 * Written March 1993 by Branko Lankester
 * Modified June 1993 by Colin Plumb for altered md5.c.
 * Modified October 1995 by Erik Troan for RPM
 */


#ifndef MD5_UTIL_H
#define MD5_UTIL_H

#include <glib.h>


typedef struct _MD5Context {
	guint32 buf[4];
	guint32 bits[2];
	guchar in[64];
	gint doByteReverse;
} MD5Context;


/* raw routines */
void md5_init(MD5Context *ctx);
void md5_update(MD5Context *ctx, const guchar *buf, guint32 len);
void md5_final(MD5Context *ctx, guchar digest[16]);

/* generate digest from memory buffer */
void md5_get_digest(const guchar *buffer, gint buffer_size, guchar digest[16]);

/* generate digest from file */
gboolean md5_get_digest_from_file(const gchar *path, guchar digest[16]);

/* convert digest to/from a NULL terminated text string, in ascii encoding */
gchar *md5_digest_to_text(guchar digest[16]);
gboolean md5_digest_from_text(const gchar *text, guchar digest[16]);


#endif	/* MD5_UTILS_H */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
