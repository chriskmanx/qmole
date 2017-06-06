/* Original code from the Linux C library */
/* Copyright (C) 2000,2001 Salvatore Sanfilippo <antirez@invece.org>
 * This code is under the original GNU C library license (GPL) */

/* $Id: bytesex.h,v 1.3 2003/07/28 09:00:55 njombart Exp $ */

#ifndef ARS_BYTESEX_H
#define ARS_BYTESEX_H

#if 	defined(__i386__) \
	|| defined(__alpha__) \
	|| (defined(__mips__) && (defined(MIPSEL) || defined (__MIPSEL__)))
#define BYTE_ORDER_LITTLE_ENDIAN
#elif 	defined(__mc68000__) \
	|| defined (__sparc__) \
	|| defined (__sparc) \
	|| defined (__PPC__) \
	|| defined (__BIG_ENDIAN__) \
	|| (defined(__mips__) && (defined(MIPSEB) || defined (__MIPSEB__)))
#define BYTE_ORDER_BIG_ENDIAN
#else
#define BYTE_ORDER_LITTLE_ENDIAN
#endif

#endif /* ARS_BYTESEX_H */
