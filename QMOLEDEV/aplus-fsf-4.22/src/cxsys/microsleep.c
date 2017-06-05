/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <stdio.h>
#include <sys/time.h>

int microsleep(int i)
{
	struct timeval tv;

	if(i>1000000)
	  {
	    tv.tv_sec = (int) i/1000000;
	    tv.tv_usec = i - tv.tv_sec*1000000;
	  }
	else
	  {
	    tv.tv_sec = 0;
	    tv.tv_usec = i;
	  }

	return select(0, NULL, NULL, NULL, &tv);
}
