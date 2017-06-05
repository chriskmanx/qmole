/*
 * Copyright (C) 2008 nsf
 */

#ifndef NSF_COMMON_H
#define NSF_COMMON_H

#include <stdlib.h>
#include <stdint.h>

/**************************************************************************
  typedefs and useful macros
**************************************************************************/

typedef unsigned char 		uchar;
typedef unsigned int 		uint;
typedef unsigned short 		ushort;
typedef unsigned long long 	ulonglong;

#define ARRAY_LENGTH(a) (sizeof(a) / sizeof((a)[0]))

/**************************************************************************
  local memory routines
**************************************************************************/

#ifndef MEMDEBUG
 void *impl__xmalloc(size_t size);
 void *impl__xmallocz(size_t size);
 void impl__xfree(void *ptr);
 char *impl__xstrdup(const char *str);
 	
 #define xmalloc(a) impl__xmalloc(a)
 #define xmallocz(a) impl__xmallocz(a)
 #define xfree(a) impl__xfree(a)
 #define xstrdup(a) impl__xstrdup(a)
#else
 void *impl__xmalloc(size_t size, const char *file, uint line);
 void *impl__xmallocz(size_t size, const char *file, uint line);
 void impl__xfree(void *ptr, const char *file, uint line);
 char *impl__xstrdup(const char *str, const char *file, uint line);
 
 #define xmalloc(a) impl__xmalloc(a, __FILE__, __LINE__)
 #define xmallocz(a) impl__xmallocz(a, __FILE__, __LINE__)
 #define xfree(a) impl__xfree(a, __FILE__, __LINE__)
 #define xstrdup(a) impl__xstrdup(a, __FILE__, __LINE__)
#endif

void xmemleaks();

#define XMALLOC(type, n) xmalloc(sizeof(type) * (n))
#define XMALLOCZ(type, n) xmallocz(sizeof(type) * (n))

#endif
