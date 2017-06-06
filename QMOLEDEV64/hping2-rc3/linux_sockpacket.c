/* 
 * $smu-mark$ 
 * $name: linux_sockpacket.c$ 
 * $author: Salvatore Sanfilippo <antirez@invece.org>$ 
 * $copyright: Copyright (C) 1999 by Salvatore Sanfilippo$ 
 * $license: This software is under GPL version 2 of license$ 
 * $date: Fri Nov  5 11:55:48 MET 1999$ 
 * $rev: 8$ 
 */ 

#include "hping2.h"

#if (defined OSTYPE_LINUX) && (!defined FORCE_LIBPCAP)
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>		/* close */
#include <stdio.h>

#include "globals.h"

static void enlarge_recvbuf(int fd)
{
	int val = 131070;
	int len = sizeof(val);

	/* Don't check the error: non fatal */
	setsockopt(fd, SOL_SOCKET, SO_RCVBUF, (const char *) &val, len);
}

int open_sockpacket()
{
	int s;

	if (opt_debug)
		printf("DEBUG: Trying to open PF_PACKET socket... ");

	s = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_IP));

	if (s == -1) {
		if (opt_debug) {
			printf("DEBUG: failed ( 2.0.x kernel? )\n");
			printf("DEBUG: Trying to open SOCK_PACKET socket... ");
		}
		s = socket(AF_INET, SOCK_PACKET, htons(ETH_P_IP));
	}

	if (s == -1) {
		perror("[open_sockpacket] socket()");
		return -1;
	}
	enlarge_recvbuf(s);

	if (opt_debug)
		printf("DEBUG: PF_PACKET, SOCK_RAW open OK\n");

	return s;
}

int close_sockpacket(int s)
{
	return close(s);
}
#endif /* OSTYPE_LINUX && !FORCE_LIBPCAP */
