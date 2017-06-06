/* 
 * $smu-mark$ 
 * $name: opensockraw.c$ 
 * $author: Salvatore Sanfilippo <antirez@invece.org>$ 
 * $copyright: Copyright (C) 1999 by Salvatore Sanfilippo$ 
 * $license: This software is under GPL version 2 of license$ 
 * $date: Fri Nov  5 11:55:49 MET 1999$ 
 * $rev: 8$ 
 */ 

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h> /* IPPROTO_RAW def. */

int open_sockraw()
{
	int s;

	s = socket(AF_INET, SOCK_RAW, IPPROTO_RAW);
	if (s == -1) {
		perror("[open_sockraw] socket()");
		return -1;
	}

	return s;
}
