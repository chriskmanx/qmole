#ifndef included_a_beam_h
#define included_a_beam_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <a/k.h>

/* Rules for determining if pointer */
#if (_MIPS_SZLONG == 64) || defined(__alpha) || defined(__sparcv9) || defined(__ia64) || defined(__x86_64)
#define HAS_64BIT_TYPE   1
typedef int		 INT32;
typedef long		 INT64; 
#elif (_MIPS_SZLONG == 32)
#define HAS_64BIT_TYPE   1
typedef int		 INT32;
typedef long long	 INT64;
#else
typedef int		 INT32;
typedef long             INT64;	/* Need to FIX !!! */
#endif

int cvtIfNeeded(void *src, A *dest, I ilen, I cvtInPlace);
int getItems(void *src, I *itemCount, I *rank, I *items, I ilen);
INT64 tr64(INT64 r, INT64 *d);


typedef struct
{
  INT32 c, t, r, n, d[MAXR], i, p[1];
} A32;

#define AH32 (sizeof(struct A32) - sizeof(INT32))


typedef struct
{
  INT64 c, t, r, n, d[MAXR], i, p[1];
} A64;

#define AH64 (sizeof(struct A64) - sizeof(INT64))

/* from k.h */
#define Tt64(t,x) ((x)<<((((t>>1)&1)+3)&3))
#define Tt32(t,x) ((x)<<(t+2&3))

#define TR32(x) ((x).r ? tr32( ((x).r-1), ((x).d+1)) : 1)
#define TR64(x) ((x).r ? tr64( ((x).r-1), ((x).d+1)) : 1)

#define ENDIAN_UNDEF  0
#define ENDIAN_LITTLE 1
#define ENDIAN_BIG    2

#define isAObject(AObject) \
      ((AObject).t >= 0)    && \
      ((AObject).t <= Ct)   && \
      ((AObject).r >= 0)    && \
      ((AObject).r <= MAXR) && \
      (((AObject).n > 0) || ((AObject).n ==0 && ((AObject).r >= 1))) && \
      (((AObject).i > 0) || ((AObject).i ==0 && ((AObject).r >= 1))) && \
      ((AObject).c == 0)

#define checkDims(AObject) \
       { \
	 I dims=1,i; \
	 if( (AObject).r==0 && (AObject).n!=1 ) \
	   dimsOK=0; \
	 else \
	   { \
	     for(i=0; i<(AObject).r; i++) \
	       dims *= (AObject).d[i]; \
	     if(dims!=(AObject).n) \
	       dimsOK=0; \
	   } \
       }

#define DO_FILE_SIZE_CHECK        0
#define SKIP_FILE_SIZE_CHECK      1

#define checkFileSize32(AObject) \
        (sizeof(A32) - sizeof(INT32) + \
        Tt32((AObject).t, (AObject).i * TR32(AObject)) + ((AObject).t == 2))

#define checkFileSize64(AObject) \
        (sizeof(A64) - sizeof(INT64) + \
        Tt64((AObject).t, (AObject).i * TR64(AObject)) + ((AObject).t == 2))

#endif
