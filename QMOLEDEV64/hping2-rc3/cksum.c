/* 
 * $smu-mark$ 
 * $name: cksum.c$ 
 * $author: Salvatore Sanfilippo <antirez@invece.org>$ 
 * $copyright: Copyright (C) 1999 by Salvatore Sanfilippo$ 
 * $license: This software is under GPL version 2 of license$ 
 * $date: Fri Nov  5 11:55:47 MET 1999$ 
 * $rev: 8$ 
 */ 

#include "hping2.h"	/* only for arch semi-indipendent data types */
#include "globals.h"

/*
 * from R. Stevens's Network Programming
 */
__u16 cksum(__u16 *buf, int nbytes)
{
	__u32 sum;
	__u16 oddbyte;

	sum = 0;
	while (nbytes > 1) {
		sum += *buf++;
		nbytes -= 2;
	}

	if (nbytes == 1) {
		oddbyte = 0;
		*((__u16 *) &oddbyte) = *(__u16 *) buf;
		sum += oddbyte;
	}

	sum = (sum >> 16) + (sum & 0xffff);
	sum += (sum >> 16);

	/* return a bad checksum with --badcksum option */
	if (opt_badcksum) sum ^= 0x5555;

	return (__u16) ~sum;
}
