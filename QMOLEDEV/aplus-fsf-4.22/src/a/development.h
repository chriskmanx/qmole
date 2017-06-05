#ifndef included_a_development_h
#define included_a_development_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#if defined(HAVE_SVR4)
#  if defined(__sgi) || defined(SOLARIS_CSET) 
#    define DEV_STRARG	char *
#  else
#    if defined(__GNUC__) || defined(__SUNPRO_CC) || defined(__SUNPRO_C) || defined(__osf__)
#      define DEV_STRARG	const char *
#    else
#      define DEV_STRARG	unsigned char *
#    endif
#  endif

#  ifdef __cplusplus
#    ifndef __sgi
       extern "C" int microsleep(int);
#    endif     
#  else
#    ifndef __sgi
       extern int microsleep(int);
#    endif
#  endif

#  define bcopy(s1,s2,len) 	memmove(s2,s1,len)
#  define bcmp(s1,s2,len)  	(memcmp(s2,s1,len)==0?0:1)
#  define bzero(sp, len)   	memset(sp, 0, len)
/*
#  ifndef __sgi
#    define usleep(x)	 	microsleep(x)
#    define getdtablesize()	sysconf(_SC_OPEN_MAX)
#    define getpagesize()	sysconf(_SC_PAGESIZE)
#  endif
*/
#else
#  define DEV_STRARG	char *
#endif

#if defined(__VISUAL_C_2_0__)
#  define DEV_STRARG		char *
#  define bcopy(s1,s2,len) 	memmove(s2,s1,len)
#  define bcmp(s1,s2,len)  	(memcmp(s2,s1,len)==0?0:1)
#  define bzero(sp, len)   	memset(sp, 0, len)
#  define usleep(x)	 	microsleep(x)
extern 
#ifdef __cplusplus
"C" 
#endif
int microsleep(int);
#endif 

#endif
