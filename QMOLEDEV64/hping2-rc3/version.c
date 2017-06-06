/* 
 * $smu-mark$ 
 * $name: version.c$ 
 * $author: Salvatore Sanfilippo <antirez@invece.org>$ 
 * $copyright: Copyright (C) 1999 by Salvatore Sanfilippo$ 
 * $license: This software is under GPL version 2 of license$ 
 * $date: Fri Nov  5 11:55:50 MET 1999$ 
 * $rev: 8$ 
 */ 

#include <stdlib.h>
#include <stdio.h>

#include "release.h"
#include "hping2.h"

void show_version(void)
{
	printf("hping version %s (%s)\n", RELEASE_VERSION, RELEASE_DATE);
#if (!defined OSTYPE_LINUX) || (defined FORCE_LIBPCAP)
	printf("libpcap based binary\n");
#else
	printf("linux sockpacket based binary\n");
#endif /* !OSTYPE_LINUX || FORCE_LIBPCAP */
	exit(0);
}

