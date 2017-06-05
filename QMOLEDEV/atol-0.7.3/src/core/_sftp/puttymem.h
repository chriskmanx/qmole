////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

/*
 * PuTTY memory-handling header.
 */

#ifndef PUTTY_PUTTYMEM_H
#define PUTTY_PUTTYMEM_H

#include <stddef.h>		       /* for size_t */
#include <string.h>		       /* for memcpy() */

//test memory allocation in debug mode
#ifdef _DEBUG
	//#define MINEFIELD
	//#define MALLOC_LOG
#endif

/* #define MALLOC_LOG  do this if you suspect putty of leaking memory */
#ifdef MALLOC_LOG
#define smalloc(z) (mlog(__FILE__,__LINE__), safemalloc(z))
#define srealloc(y,z) (mlog(__FILE__,__LINE__), saferealloc(y,z))
#define sfree(z) (mlog(__FILE__,__LINE__), safefree(z), z=NULL)
void mlog(char *, int);
#else
#define smalloc safemalloc
#define srealloc saferealloc
//#define sfree safefree
#define sfree(z) (safefree(z), z=NULL)
#endif

void *safemalloc(size_t);
void *saferealloc(void *, size_t);
void safefree(void *);


/* smalloc a thing */
#define smalloca(type) ((type *) smalloc (sizeof (type)))
/* smalloc a copy of a thing */
#define smallocc(ptr) memcpy (smalloc (sizeof (*ptr)), ptr, sizeof (*ptr))
/* smalloc n things */
#define smallocn(n,type) ((type *) smalloc ((n) * sizeof (type)))


#endif
