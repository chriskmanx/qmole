/* $Id: e2p_dircmp.c 2978 2013-11-30 02:56:32Z tpgww $

Portions copyright (C) 2006-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file plugins/e2p_dircmp.c
@brief plugin for showing differences between the contents of active and inactive directories
*/

#include "emelfm2.h"
#include <fcntl.h>
#include "e2_plugins.h"
#include "e2_fileview.h"
#include "e2_filelist.h"
#include "e2_task.h"

//signature component, must match 'core' of this file name and likewise for corresponding icon file name
#define ANAME "dircmp"

static PluginIface iface;

typedef struct _E2_CmpData
{
#ifdef E2_VFS
	GError **operr;
	PlaceInfo *otherspace;
#endif
	gint oldroot_len;
	gchar *newroot;
} E2_CmpData;

/* Bytes in MD5 hash */
#define HASH_LENGTH 16

#ifndef USE_GLIB2_16
/********* start of borrowed code *********/

/*
This code implements the MD5 message-digest algorithm, by Ron Rivest.
This code was written by Colin Plumb in 1993, our understanding is that no
copyright is claimed and that this code is in the public domain.

Equivalent code is available from RSA Data Security, Inc.
This code has been tested against that, and is functionally equivalent.
(It is also functionally equivalent to the approach used by Glib >= 2.16.)

To compute the message digest of a chunk of bytes, declare an MD5Context
structure, pass it to MD5Init, call MD5Update as needed on buffer[s] full of
bytes, and then call MD5Final, which will fill a supplied 16-byte array with
the digest.
*/

struct MD5Context {
	u_int32_t buf[4];
	u_int32_t bits[2];
	unsigned char in[64];
};

void MD5Init(struct MD5Context *context);
void MD5Update(struct MD5Context *context, unsigned char const *buf, unsigned len);
void MD5Final(unsigned char digest[16], struct MD5Context *context);
void MD5Transform(u_int32_t buf[4], u_int32_t const in[16]);

/* MD5 setup requires knowing if we're big- or little-endian */
#if defined(__linux) || defined(__linux__)
# include <endian.h>
# if !defined(BYTE_ORDER) && defined(__BYTE_ORDER)
#  define BYTE_ORDER __BYTE_ORDER
# endif

#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
# include <sys/endian.h>
# if !defined(BYTE_ORDER) && defined(_BYTE_ORDER)
#  define BYTE_ORDER _BYTE_ORDER
# endif

#elif defined(__LINUX) //CHECKME
# ifndef __USE_BSD
#  define __USE_BSD
# endif
# include <endian.h>
# if !defined(BYTE_ORDER) && defined(__BYTE_ORDER)
#  define BYTE_ORDER __BYTE_ORDER
# endif

#elif defined (__SOLARIS)

# include <sys/isa_defs.h>
# ifdef _BIG_ENDIAN
#  define BYTE_ORDER 4321
# else
#  define BYTE_ORDER 1234
# endif

#elif defined (__APPLE__)
# include <machine/endian.h>

#endif

#ifndef BIG_ENDIAN
# define BIG_ENDIAN    4321
#endif
#ifndef LITTLE_ENDIAN
# define LITTLE_ENDIAN 1234
#endif

#if BYTE_ORDER == BIG_ENDIAN
# define HIGHFIRST
// For Tiger
# define BIG_ENDIAN_HOST
#endif //BYTE_ORDER == BIG_ENDIAN

#ifdef HIGHFIRST
void byteReverse(unsigned char *buf, unsigned longs);
# ifndef ASM_MD5
/*
Note: this code is harmless on little-endian machines.
*/
void byteReverse(unsigned char *buf, unsigned longs)
{
	u_int32_t t;
	do {
	t = (u_int32_t) ((unsigned) buf[3] << 8 | buf[2]) << 16 |
		((unsigned) buf[1] << 8 | buf[0]);
	*(u_int32_t *) buf = t;
	buf += 4;
	} while (--longs);
}
# endif
#else
# define byteReverse(buf,len)	/* Nothing */
#endif

/*
Start MD5 accumulation.  Set bit count to 0 and buffer to mysterious
initialization constants.
*/
void MD5Init(struct MD5Context *ctx)
{
	ctx->buf[0] = 0x67452301;
	ctx->buf[1] = 0xefcdab89;
	ctx->buf[2] = 0x98badcfe;
	ctx->buf[3] = 0x10325476;

	ctx->bits[0] = 0;
	ctx->bits[1] = 0;
}

/*
Update context to reflect the concatenation of another buffer full of bytes.
*/
void MD5Update(struct MD5Context *ctx, unsigned char const *buf, unsigned len)
{
	u_int32_t t;

	/* Update bitcount */

	t = ctx->bits[0];
	if ((ctx->bits[0] = t + ((u_int32_t) len << 3)) < t)
	ctx->bits[1]++;		/* Carry from low to high */
	ctx->bits[1] += len >> 29;

	t = (t >> 3) & 0x3f;	/* Bytes already in shsInfo->data */

	/* Handle any leading odd-sized chunks */

	if (t) {
	unsigned char *p = (unsigned char *) ctx->in + t;

	t = 64 - t;
	if (len < t) {
		memcpy(p, buf, len);
		return;
	}
	memcpy(p, buf, t);
	byteReverse(ctx->in, 16);
	MD5Transform(ctx->buf, (u_int32_t *) ctx->in);
	buf += t;
	len -= t;
	}
	/* Process data in 64-byte chunks */

	while (len >= 64) {
	memcpy(ctx->in, buf, 64);
	byteReverse(ctx->in, 16);
	MD5Transform(ctx->buf, (u_int32_t *) ctx->in);
	buf += 64;
	len -= 64;
	}

	/* Handle any remaining bytes of data. */

	memcpy(ctx->in, buf, len);
}

/*
Final wrapup - pad to 64-byte boundary with the bit pattern 1 0*
(64-bit count of bits processed, MSB-first)
*/
void MD5Final(unsigned char digest[16], struct MD5Context *ctx)
{
	unsigned count;
	unsigned char *p;

	/* Compute number of bytes mod 64 */
	count = (ctx->bits[0] >> 3) & 0x3F;

	/* Set the first char of padding to 0x80.  This is safe since there is
	   always at least one byte free */
	p = ctx->in + count;
	*p++ = 0x80;

	/* Bytes of padding needed to make 64 bytes */
	count = 64 - 1 - count;

	/* Pad out to 56 mod 64 */
	if (count < 8) {
	/* Two lots of padding:  Pad the first block to 64 bytes */
	memset(p, 0, count);
	byteReverse(ctx->in, 16);
	MD5Transform(ctx->buf, (u_int32_t *) ctx->in);

	/* Now fill the next block with 56 bytes */
	memset(ctx->in, 0, 56);
	} else {
	/* Pad block to 56 bytes */
	memset(p, 0, count - 8);
	}
	byteReverse(ctx->in, 14);

	/* Append length in bits and transform */
	((u_int32_t *) ctx->in)[14] = ctx->bits[0];
	((u_int32_t *) ctx->in)[15] = ctx->bits[1];

	MD5Transform(ctx->buf, (u_int32_t *) ctx->in);
	byteReverse((unsigned char *) ctx->buf, 4);
	memcpy(digest, ctx->buf, 16);

	memset(ctx, 0, sizeof(* ctx));	/* In case it's sensitive */
	/* The original version of this code omitted the asterisk. In
	   effect, only the first part of ctx was wiped with zeros, not
	   the whole thing. Bug found by Derek Jones. Original line: */
	// memset(ctx, 0, sizeof(ctx));	/* In case it's sensitive */
}

#ifndef ASM_MD5

#define MD5STEP(f, w, x, y, z, data, s) \
	( w += f(x, y, z) + data,  w = w<<s | w>>(32-s),  w += x )
/* The four core functions - F1 is optimized somewhat */
/* #define F1(x, y, z) (x & y | ~x & z) */
#define F1(x, y, z) (z ^ (x & (y ^ z)))
#define F2(x, y, z) F1(z, x, y)
#define F3(x, y, z) (x ^ y ^ z)
#define F4(x, y, z) (y ^ (x | ~z))

/*
The core of the MD5 algorithm, this alters an existing MD5 hash to
reflect the addition of 16 longwords of new data.  MD5Update blocks
the data and converts bytes into longwords for this routine.
*/
void MD5Transform(u_int32_t buf[4], u_int32_t const in[16])
{
	register u_int32_t a, b, c, d;

	a = buf[0];
	b = buf[1];
	c = buf[2];
	d = buf[3];

	MD5STEP(F1, a, b, c, d, in[0] + 0xd76aa478, 7);
	MD5STEP(F1, d, a, b, c, in[1] + 0xe8c7b756, 12);
	MD5STEP(F1, c, d, a, b, in[2] + 0x242070db, 17);
	MD5STEP(F1, b, c, d, a, in[3] + 0xc1bdceee, 22);
	MD5STEP(F1, a, b, c, d, in[4] + 0xf57c0faf, 7);
	MD5STEP(F1, d, a, b, c, in[5] + 0x4787c62a, 12);
	MD5STEP(F1, c, d, a, b, in[6] + 0xa8304613, 17);
	MD5STEP(F1, b, c, d, a, in[7] + 0xfd469501, 22);
	MD5STEP(F1, a, b, c, d, in[8] + 0x698098d8, 7);
	MD5STEP(F1, d, a, b, c, in[9] + 0x8b44f7af, 12);
	MD5STEP(F1, c, d, a, b, in[10] + 0xffff5bb1, 17);
	MD5STEP(F1, b, c, d, a, in[11] + 0x895cd7be, 22);
	MD5STEP(F1, a, b, c, d, in[12] + 0x6b901122, 7);
	MD5STEP(F1, d, a, b, c, in[13] + 0xfd987193, 12);
	MD5STEP(F1, c, d, a, b, in[14] + 0xa679438e, 17);
	MD5STEP(F1, b, c, d, a, in[15] + 0x49b40821, 22);

	MD5STEP(F2, a, b, c, d, in[1] + 0xf61e2562, 5);
	MD5STEP(F2, d, a, b, c, in[6] + 0xc040b340, 9);
	MD5STEP(F2, c, d, a, b, in[11] + 0x265e5a51, 14);
	MD5STEP(F2, b, c, d, a, in[0] + 0xe9b6c7aa, 20);
	MD5STEP(F2, a, b, c, d, in[5] + 0xd62f105d, 5);
	MD5STEP(F2, d, a, b, c, in[10] + 0x02441453, 9);
	MD5STEP(F2, c, d, a, b, in[15] + 0xd8a1e681, 14);
	MD5STEP(F2, b, c, d, a, in[4] + 0xe7d3fbc8, 20);
	MD5STEP(F2, a, b, c, d, in[9] + 0x21e1cde6, 5);
	MD5STEP(F2, d, a, b, c, in[14] + 0xc33707d6, 9);
	MD5STEP(F2, c, d, a, b, in[3] + 0xf4d50d87, 14);
	MD5STEP(F2, b, c, d, a, in[8] + 0x455a14ed, 20);
	MD5STEP(F2, a, b, c, d, in[13] + 0xa9e3e905, 5);
	MD5STEP(F2, d, a, b, c, in[2] + 0xfcefa3f8, 9);
	MD5STEP(F2, c, d, a, b, in[7] + 0x676f02d9, 14);
	MD5STEP(F2, b, c, d, a, in[12] + 0x8d2a4c8a, 20);

	MD5STEP(F3, a, b, c, d, in[5] + 0xfffa3942, 4);
	MD5STEP(F3, d, a, b, c, in[8] + 0x8771f681, 11);
	MD5STEP(F3, c, d, a, b, in[11] + 0x6d9d6122, 16);
	MD5STEP(F3, b, c, d, a, in[14] + 0xfde5380c, 23);
	MD5STEP(F3, a, b, c, d, in[1] + 0xa4beea44, 4);
	MD5STEP(F3, d, a, b, c, in[4] + 0x4bdecfa9, 11);
	MD5STEP(F3, c, d, a, b, in[7] + 0xf6bb4b60, 16);
	MD5STEP(F3, b, c, d, a, in[10] + 0xbebfbc70, 23);
	MD5STEP(F3, a, b, c, d, in[13] + 0x289b7ec6, 4);
	MD5STEP(F3, d, a, b, c, in[0] + 0xeaa127fa, 11);
	MD5STEP(F3, c, d, a, b, in[3] + 0xd4ef3085, 16);
	MD5STEP(F3, b, c, d, a, in[6] + 0x04881d05, 23);
	MD5STEP(F3, a, b, c, d, in[9] + 0xd9d4d039, 4);
	MD5STEP(F3, d, a, b, c, in[12] + 0xe6db99e5, 11);
	MD5STEP(F3, c, d, a, b, in[15] + 0x1fa27cf8, 16);
	MD5STEP(F3, b, c, d, a, in[2] + 0xc4ac5665, 23);

	MD5STEP(F4, a, b, c, d, in[0] + 0xf4292244, 6);
	MD5STEP(F4, d, a, b, c, in[7] + 0x432aff97, 10);
	MD5STEP(F4, c, d, a, b, in[14] + 0xab9423a7, 15);
	MD5STEP(F4, b, c, d, a, in[5] + 0xfc93a039, 21);
	MD5STEP(F4, a, b, c, d, in[12] + 0x655b59c3, 6);
	MD5STEP(F4, d, a, b, c, in[3] + 0x8f0ccc92, 10);
	MD5STEP(F4, c, d, a, b, in[10] + 0xffeff47d, 15);
	MD5STEP(F4, b, c, d, a, in[1] + 0x85845dd1, 21);
	MD5STEP(F4, a, b, c, d, in[8] + 0x6fa87e4f, 6);
	MD5STEP(F4, d, a, b, c, in[15] + 0xfe2ce6e0, 10);
	MD5STEP(F4, c, d, a, b, in[6] + 0xa3014314, 15);
	MD5STEP(F4, b, c, d, a, in[13] + 0x4e0811a1, 21);
	MD5STEP(F4, a, b, c, d, in[4] + 0xf7537e82, 6);
	MD5STEP(F4, d, a, b, c, in[11] + 0xbd3af235, 10);
	MD5STEP(F4, c, d, a, b, in[2] + 0x2ad7d2bb, 15);
	MD5STEP(F4, b, c, d, a, in[9] + 0xeb86d391, 21);

	buf[0] += a;
	buf[1] += b;
	buf[2] += c;
	buf[3] += d;
}

#endif

/*************** end of borrowed code ***************/
#endif //ndef USE_GLIB2_16

/**
@brief blockwise read and hash the regular file @a filepath
Uses file descriptors, which means files must be real, local
Any error message here expects BGL to be open
Returned buffer may contain '\0' before its end, and has no trailing '\0'
@param filepath localised string, absolute path of item to read
@return pointer to newly-allocated 16-byte buffer containing the computed hash, or NULL on error
*/
static guchar *_e2p_diff_dohash (VPATH *filepath)
{
#ifdef USE_GLIB2_16
	GChecksum *chk;
#else
	struct MD5Context md;
#endif
	guchar *hash;
	guchar buffer [BUFSIZ];
	gint fdesc;
	E2_ERR_DECLARE

#if defined(__USE_GNU) && defined(O_NOATIME)
	gint flags = O_RDONLY;
	if (!e2_fs_access (filepath, W_OK E2_ERR_PTR()))
		flags |= O_NOATIME;
	else
	{
		//try to proceed without atime preservation
		E2_ERR_CLEAR
	}
	fdesc = e2_fs_safeopen (VPCSTR (filepath), flags, 0);
#else	//be strict about open-flags
	fdesc = e2_fs_safeopen (VPCSTR (filepath), O_RDONLY, 0);
#endif
	if (fdesc < 0)
	{
#ifdef E2_VFS
		e2_fs_set_error_from_errno (&E2_ERR_NAME);
#endif
		e2_fs_error_local (_("Cannot open '%s' for reading"), filepath E2_ERR_MSGL());
		E2_ERR_CLEAR
		return NULL;
	}

	guint64 bytes_read = 0;	//used for error recovery

#ifdef USE_GLIB2_16
	chk = g_checksum_new (G_CHECKSUM_MD5);
#else
	MD5Init (&md);
#endif

	while (TRUE)
	{
		// clear buffer in case we need to pad the hash
		memset (buffer, 0, BUFSIZ);
		ssize_t n_read = e2_fs_read (fdesc, buffer, BUFSIZ E2_ERR_PTR());
		if (n_read == BUFSIZ)
		{
#ifdef USE_GLIB2_16
			g_checksum_update (chk, buffer, BUFSIZ);
#else
			MD5Update (&md, buffer, BUFSIZ);
#endif
			bytes_read += n_read;
		}
		else if (n_read == 0)
			break;
		else if (n_read < 0)
		{
//			if (1)	//FIXME fatal error handling
			//these are the fatal errors used in md5deep
			if (   E2_ERR_IS (EACCES)	//permission denied (changed since opened ?)
				|| E2_ERR_IS (ENODEV)	//operation not supported (can't happen?)
				|| E2_ERR_IS (EBADF)	//bad file descriptor (can't happen?)
				|| E2_ERR_IS (EFBIG)	//file too big
				|| E2_ERR_IS (ETXTBSY)	//text file busy
				)
			{
				e2_fs_error_local (_("Error reading file %s"), filepath E2_ERR_MSGL());
				E2_ERR_CLEAR
				return NULL;
			}
			//non-fatal, we can continue
			E2_ERR_CLEAR
#ifdef USE_GLIB2_16
			g_checksum_update (chk, buffer, BUFSIZ);
#else
			MD5Update(&md, buffer, BUFSIZ);
#endif
			bytes_read += BUFSIZ;
		 /* but the file pointer's now undefined. So manually advance it to the
			next read position */
			lseek (fdesc, bytes_read, SEEK_SET);
		}
		else if (n_read < BUFSIZ)
		{
		  //end of the file, but not a full buffer
		  //update the hash accordingly
#ifdef USE_GLIB2_16
			g_checksum_update (chk, buffer, n_read);
#else
			MD5Update (&md, buffer, n_read);
#endif
			break;
		}
	}

	TEMP_FAILURE_RETRY (close (fdesc));	//FIXME vfs

	hash = NEW (guchar, HASH_LENGTH);
	if (hash != NULL)
	{
#ifdef USE_GLIB2_16
		gsize len = HASH_LENGTH;
		g_checksum_get_digest (chk, hash, &len);
		g_checksum_free (chk);
#else
		MD5Final (hash, &md);
#endif
#if 0 //def DEBUG_MESSAGES
		gchar hex [] = "0123456789abcdef";
		gchar result [2 * HASH_LENGTH + 1];
		gint i;
		for (i = 0; i < HASH_LENGTH; ++i)
		{
			result[2 * i] = hex[(hash[i] >> 4) & 0xf];
			result[2 * i + 1] = hex[hash[i] & 0xf];
		}
		result [2 * HASH_LENGTH] = '\0';
		printd (DEBUG, "MD5 # for %s is %s", VPCSTR (filepath), result);
#endif //0
	}
/*	else
	{
		gchar *utf = F_DISPLAYNAME_FROM_LOCALE (filename);
		gchar *msg = g_strdup_printf (
			_("Md5 calculation failed for %s - out of memory"), utf);
		CLOSEBGL
		e2_output_print_error (msg, TRUE);
		OPENBGL
		F_FREE (utf, filename);
	}
*/

	return hash;
}
/**
@brief determine equality of non-dir items described by data in @a data
BGL is expected to be open/off
@param localpath absolute path of non-directory item to be compared, localised string
@param statptr pointer to info from lstat() for @a localpath
@param localotherpath absolute path of item to be compared, localised string

@return TRUE if the items are equal, (also FALSE after memory shortage or path error)
*/
static gboolean _e2p_diff1 (VPATH *localpath, const struct stat *statptr,
	VPATH *localotherpath)
{
	gboolean retval;
	struct stat othersb;

	if (e2_fs_lstat (localotherpath, &othersb E2_ERR_NONE()))
		retval = FALSE;	//lstat() failed, assume this means we can't find a match
	else if ((statptr->st_mode & S_IFMT) != (othersb.st_mode & S_IFMT))
		retval = FALSE;	//unequal type = quick check
	else if (statptr->st_size != othersb.st_size)
		retval = FALSE;	//unequal size = quick check
	else if (S_ISREG (othersb.st_mode) && othersb.st_size > 0)
	{	//determine equality of items' md5 sums
		guchar *hash1 = _e2p_diff_dohash (localpath);
		if (hash1 == NULL)
			retval = FALSE;
		else
		{
			guchar *hash2 = _e2p_diff_dohash (localotherpath);
			if (hash2 == NULL)
			{
				g_free (hash1);
				retval = FALSE;
			}
			else
			{
				retval = !memcmp (hash1, hash2, HASH_LENGTH);
				g_free (hash1);
				g_free (hash2);
			}
		}
	}
	else if (S_ISLNK (othersb.st_mode))
	{
		//first, check for equality of link targets
		gchar *target1, *target2;
#ifdef USE_GLIB2_10
		target1 = g_slice_alloc (PATH_MAX);
#else
		target1 = g_try_malloc (PATH_MAX);
#endif
		CHECKALLOCATEDWARNT (target1, {});
		if (target1 != NULL)
		{
			gint len = e2_fs_readlink (localpath, target1, PATH_MAX E2_ERR_NONE());
			if (len < 1) //error or empty
			{
#ifdef USE_GLIB2_10
				g_slice_free1 (PATH_MAX, target1);
#else
				g_free (target1);
#endif
				return FALSE;
			}
#ifdef USE_GLIB2_10
			target2 = g_slice_alloc (PATH_MAX);
#else
			target2 = g_try_malloc (PATH_MAX);
#endif
			CHECKALLOCATEDWARNT (target2, {});
			if (target2 != NULL)
			{
				len = e2_fs_readlink (localotherpath, target2, PATH_MAX E2_ERR_NONE());
				if (len < 0) len = 0;  //CHECKME ok to ignore error ?
				retval = !strcmp (target1, target2);
/*
//the targets actually match ?
				if (!strcmp (target1, target2))
				{
					E2_ERR_DECLARE
					gchar *target1 = g_try_malloc (PATH_MAX);
					CHECKALLOCATEDWARNT (target1, return FALSE;)
					if (target1 != NULL)
					{
						g_strlcpy (target1, localpath, PATH_MAX);
						if (!e2_fs_walk_link (&target1 E2_ERR_ARG()))
						{
							//FIXME report error
							E2_ERR_CLEAR
							return FALSE;
						}
						gchar *target2 = g_try_malloc (PATH_MAX);
						CHECKALLOCATEDWARNT (target2, FIXME;)
						if (target2 != NULL)
						{
							g_strlcpy (target2, localotherpath, PATH_MAX);
							if (!e2_fs_walk_link (&target2 E2_ERR_ARG()))
							{
								//FIXME report error
								E2_ERR_CLEAR
								g_free (target1);
								g_free (target2);
								return FALSE;
							}
							if (e2_fs_lstat (target1, &othersb E2_ERR_NONE()))
								retval = FALSE;
							else
							{
							CHECK FOR DIR
								retval = _e2p_diff1 (target1, &othersb, target2);
							}
							g_free (target2);
						}
						g_free (target1);
					}
				}
				else
					retval = FALSE;
*/
#ifdef USE_GLIB2_10
				g_slice_free1 (PATH_MAX, target2);
#else
				g_free (target2);
#endif
			}
			else
				retval = FALSE;
#ifdef USE_GLIB2_10
			g_slice_free1 (PATH_MAX, target1);
#else
			g_free (target1);
#endif
		}
		else
			retval = FALSE;
	}
	else
	//at this point, empty regular files, or other types (various types of special file)
	//must be equal
		retval = TRUE;

	return retval;
}
/**
@brief helper function for checking directories item-count
This is a callback for a treewalk function.
Each dir should bump @a count by 2 more than the no. of items (due to the
DP or DRR and DP reports).
@param local_name path of item reported by the walker, localised string
@param statbuf pointer to struct stat with data about @a local_name
@param status code from the walker, indicating what type of item is being reported
@param count pointer to store for items count
@return E2TW_CONTINUE always
*/
static E2_TwResult _e2p_diff_count_twcb (VPATH *local_name,
	const struct stat *statbuf, E2_TwStatus status, guint *count)
{
	(*count)++;
	return E2TW_CONTINUE;
}
/**
@brief determine equality of dir items described by data in @a data
@param data ptr to data struct for items to be compared
This is a callback for a treewalk function
Error message expects BGL to be open/off
@param localpath absolute path of item reported by the walker, localised string
@param statbuf pointer to struct stat with data about @a localpath
@param status code from the walker, indicating what type of report it is
@param twdata pointer to tw data struct

@return E2TW_CONTINUE when items match, E2TW_STOP when they don't or can't decide
*/
static E2_TwResult _e2p_diff_twcb (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, E2_CmpData *twdata)
{
	gchar *localotherpath;
	E2_TwResult retval = E2TW_STOP;
#ifdef E2_VFS
	VPATH ddata;
#endif

	switch (status)
	{
		case E2TW_F:	//not directory or link
		case E2TW_SL:	//symbolic link
		case E2TW_SLN:	//symbolic link naming non-existing file
			localotherpath = e2_utils_strcat (twdata->newroot,
				(gchar *)localpath + twdata->oldroot_len);
#ifdef E2_VFS
			ddata.path = localotherpath;
			ddata.spacedata = twdata->otherspace;
			if (_e2p_diff1 (localpath, statptr, &ddata))
#else
			if (_e2p_diff1 (localpath, statptr, localotherpath))
#endif
				retval = E2TW_CONTINUE;
			g_free (localotherpath);
			break;
		case E2TW_D:	//directory
		case E2TW_DRR:	//directory now readable
		{
			struct stat othrstatbuf;
			localotherpath = e2_utils_strcat (twdata->newroot,
				(gchar *)localpath + twdata->oldroot_len);
#ifdef E2_VFS
			ddata.path = localotherpath;
			ddata.spacedata = twdata->otherspace;
			if (e2_fs_stat (&ddata, &othrstatbuf E2_ERR_NONE())
#else
			if (e2_fs_stat (localotherpath, &othrstatbuf E2_ERR_NONE())
#endif
				|| !S_ISDIR (othrstatbuf.st_mode)
				|| S_ISLNK (othrstatbuf.st_mode))
				break;
			//maybe there are more items in the other dir, or if this dir is empty
			//there will be no other check that the other even exists
			guint count = 0;
			e2_fs_tw (localpath, _e2p_diff_count_twcb, &count, 1,
				E2TW_XQT | E2TW_PHYS E2_ERR_NONE());
			guint count2 = 0;
			localotherpath = e2_utils_strcat (twdata->newroot,
				(gchar *)localpath + twdata->oldroot_len);
#ifdef E2_VFS
			e2_fs_tw (&ddata, _e2p_diff_count_twcb, &count2, 1,
#else
			e2_fs_tw (localotherpath, _e2p_diff_count_twcb, &count2, 1,
#endif
				E2TW_XQT | E2TW_PHYS E2_ERR_NONE());
			g_free (localotherpath);
			if (count != count2)
				break;
		}
		case E2TW_DP:	//directory, finished
			retval = E2TW_CONTINUE;	//effectively ignore these
			break;
//		case E2TW_DL:	//directory, not opened due to tree-depth limit
//		case E2TW_DM:	//directory, not opened due to different file system
//		case E2TW_DNR:	//unreadable directory (for which, error is reported upstream)
//		case E2TW_NS:	//un-statable item (for which, error is reported upstream)
		default:
			break;
	}
	return retval;
}
/**
@brief thread function to iterate over active pane file list to check for matches
BGL will be open
@param thread_data UNUSED NULL pointer specified when thread was established

@return NULL
*/
static gpointer _e2p_diff_all (gpointer thread_data)
{
	GtkTreeIter iter;
	GtkTreeModel *model = curr_view->model;
	if (gtk_tree_model_get_iter_first (model, &iter));
	{	//it's not empty
		e2_filelist_disable_refresh ();
		CLOSEBGL //prevent thread-related hang!
		e2_window_set_cursor (GDK_WATCH);
		OPENBGL

		WAIT_FOR_REFRESH(curr_view)

		gchar *curr_local, *other_local, *currpath;
		FileInfo *info;
		struct stat othersb;
		E2_CmpData data;
#ifdef E2_VFS
		data.operr = NULL;	//or something ...
		data.otherspace = other_view->spacedata;
		VPATH sdata;
		VPATH ddata;
		sdata.spacedata = curr_view->spacedata;
		ddata.spacedata = other_view->spacedata;
#endif
		curr_local = D_FILENAME_TO_LOCALE (curr_view->dir); //always dup, to avoid (unlikely) dirchange race
		other_local = D_FILENAME_TO_LOCALE (other_view->dir);
		gboolean matched;
		GtkTreeSelection *sel = curr_view->selection;
		CLOSEBGL
		gtk_tree_selection_unselect_all (sel);	//start with clean slate
		OPENBGL
		do
		{
#ifdef E2_VFSTMP
			//CLEAR any error data fom last loop
#endif
			gtk_tree_model_get (model, &iter, FINFO, &info, -1);
			data.newroot = e2_utils_strcat (other_local, info->filename);
#ifdef E2_VFS
			ddata.path = data.newroot;
			if (e2_fs_lstat (&ddata, &othersb E2_ERR_NONE()))
#else
			if (e2_fs_lstat (data.newroot, &othersb E2_ERR_NONE()))
#endif
				matched = FALSE;	//lstat() failed, crudely assume can't find anything to match
			else
			{
				currpath = e2_utils_strcat (curr_local, info->filename);
				if (S_ISDIR (info->statbuf.st_mode) && S_ISDIR (othersb.st_mode))
				{
					data.oldroot_len = strlen (currpath);
#ifdef E2_VFS
					sdata.path = currpath;
					matched = e2_fs_tw (&sdata, _e2p_diff_twcb, &data, -1,
#else
					matched = e2_fs_tw (currpath, _e2p_diff_twcb, &data, -1,
#endif
						E2TW_PHYS E2_ERR_NONE());
				}
				else if (!S_ISDIR (info->statbuf.st_mode) && !S_ISDIR (othersb.st_mode))
#ifdef E2_VFS
				{
					sdata.path = currpath;
					matched = _e2p_diff1 (&sdata, &info->statbuf, &ddata);
				}
#else
					matched = _e2p_diff1 (currpath, &info->statbuf, data.newroot);
#endif
				else
					matched = FALSE;	//mixture of dir and non-dir

				g_free (currpath);
			}

			if (matched)
			{
				CLOSEBGL
				gtk_tree_selection_select_iter (sel, &iter);
				OPENBGL
			}

			g_free (data.newroot);
		} while (gtk_tree_model_iter_next (model, &iter));

		g_free (curr_local);
		g_free (other_local);

#ifdef E2_VFS
		//cleanup data.operr	//or something ...
#endif
		CLOSEBGL
		e2_window_set_cursor (GDK_LEFT_PTR);
		OPENBGL
		e2_filelist_enable_refresh ();
	}
	return NULL;
}

/**
@brief directory-comparison plugin action
This creates a thread to iterate over active pane file list to check for matches

@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE
*/
static gboolean _e2p_diff (gpointer from, E2_ActionRuntime *art)
{
#ifdef USE_GLIB2_32
	g_thread_new ("", _e2p_diff_all, NULL);
#else
	g_thread_create (_e2p_diff_all, NULL, FALSE, NULL);
#endif
	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_ONEACTION_SIMPLE (_A(14),_("compare"),_e2p_diff,
		_("C_ompare"),
		_("Select active-pane items which are duplicated in the other pane"),
		"plugin_"ANAME E2ICONTB)
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	PLUGIN_CLEAR_ACTIONS (p)
	return ret;
}
