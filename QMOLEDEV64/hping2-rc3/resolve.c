/* 
 * $smu-mark$ 
 * $name: resolve.c$ 
 * $author: Salvatore Sanfilippo <antirez@invece.org>$ 
 * $copyright: Copyright (C) 1999 by Salvatore Sanfilippo$ 
 * $license: This software is under GPL version 2 of license$ 
 * $date: Fri Nov  5 11:55:49 MET 1999$ 
 * $rev: 8$ 
 */ 

#include <stdlib.h>
#include <sys/types.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

void resolve (struct sockaddr * addr, char *hostname)
{
	struct  sockaddr_in *address;
	struct  hostent     *host;

	address = (struct sockaddr_in *)addr;

	memset(address, 0, sizeof(struct sockaddr_in));
	address->sin_family = AF_INET;
	address->sin_addr.s_addr = inet_addr(hostname);

	if ( (int)address->sin_addr.s_addr == -1) {
		host = gethostbyname(hostname);
		if (host) {
			memcpy(&address->sin_addr, host->h_addr,
				host->h_length);
		} else {
			perror("[resolve] Could not resolve address");
			exit(1);
		}
	}
}
