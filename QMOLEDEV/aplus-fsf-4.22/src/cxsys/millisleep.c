/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <stdio.h>
#include <sys/time.h>

millisleep(i)
int i;
{
	struct timeval tv;

	tv.tv_sec = 0;
	tv.tv_usec = i*1000;
	select(0, NULL, NULL, NULL, &tv);
}
