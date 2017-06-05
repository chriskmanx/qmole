#ifndef included_a_fir_h
#define included_a_fir_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/*
 * Include file with defines and macros for use with A=>C interface.
 *
 * Malcolm Austin
 */

/* defines for error codes */

#define ERR_NONE 0
#define ERR_INTERRUPT 1
#define ERR_WSFULL 2
#define ERR_STACK 3
#define ERR_VALUE 4
#define ERR_VALENCE 5
#define ERR_TYPE 6
#define ERR_RANK 7
#define ERR_LENGTH 8
#define ERR_DOMAIN 9
#define ERR_INDEX 10
#define ERR_MISMATCH 11
#define ERR_NONCE 12
#define ERR_MAXRANK 13
#define ERR_NONFUNCTION 14
#define ERR_PARSE 15
#define ERR_MAXITEMS 16
#define ERR_INVALID 17
#define ERR_NONDATA 18

#define ERR_MESSAGE -1

/* two macros for error generation */

#define ERROUT(X) { q=(X); return(0); }
#define ERRMSG(MSG) {q=ERR_MESSAGE; qs=(MSG); return(0); }

#define SIZEOFITEM(t) ( ((Et == t) ? sizeof(A) : 	\
			 (It == t) ? sizeof(I) :        \
			 (Ft == t) ? sizeof(F) :   	\
			 (Ct == t) ? sizeof(C) :	\
			 (Xt <= t) ? sizeof(A) : sizeof(I) ))

/* macros for performing non-data checks */

#define NDC1(a)   if(!QA(a)||Et<(a)->t)ERROUT(ERR_NONDATA);
#define NDC2(a,w) if(!QA(a)||!QA(w)||Et<(a)->t||Et<(w)->t)ERROUT(ERR_NONDATA);

/* charma() can replace malloc() */

#define charma(X) ((char *) ma(((X)+(sizeof(int)-1))/sizeof(int)))

/* IAR() is used to reference an A object indirectly (as in a nested array) */

#define IAF(aobj,i,field) (((A)aobj->p[i])->field)
#define IAR(aobj, i) ((A)aobj->p[i])

#ifndef TRUE
#define	TRUE	(I)1
#endif

#ifndef FALSE
#define	FALSE	(I)0
#endif

/*
 * MIN and MAX macros
 */
#ifndef MIN
#define MIN(a,b)        ( ((a) < (b)) ? (a) : (b) )
#endif

#ifndef MAX
#define MAX(a,b)        ( ((a) > (b)) ? (a) : (b) )
#endif


/*
 * Better string macros (work on 8-bit chars)
 *
 * Malcolm Austin 8/90
 */

#ifndef isxdigit
#include <ctype.h>
#endif

#define ISalpha(c)      ( isascii(c) && isalpha(c) )
#define ISupper(c)	( isascii(c) && isupper(c) )
#define ISlower(c)      ( isascii(c) && islower(c) )
#define ISdigit(c)	( isascii(c) && isdigit(c) )
#define ISxdigit(c)	( isascii(c) && isxdigit(c) )
#define ISalnum(c)	( isascii(c) && isalnum(c) )
#ifdef ctrlLspace
#define ISspace(c)      ( isascii(c) && ('\014' == c ||isspace(c)) )
#else
#define ISspace(c)      ( isascii(c) && isspace(c) )
#endif
#define ISpunct(c)	( isascii(c) && ispunct(c) )
#define ISprint(c)      ( isascii(c) && isprint(c) )
#define IScntrl(c)	( isascii(c) && iscntrl(c) )
#define ISgraph(c)      ( isascii(c) && isgraph(c) )

/*
 * #defines to make debugging easier, can be overridden in code
 */

#define SUBROUTINE static
#define ENTRYPOINT

/*
 * Macros for working with A objects.
 */

/* IsSymbol returns TRUE if the idxth element of aobj is a symbol.  If match
is not-NULL, it also check for a match to a particular char string. */

#define IsSymbol(aobj, idx, match) (                     \
     ( Et==(aobj)->t && QS((aobj)->p[idx]) &&            \
      ( NULL==match || match==MS(si(match)))))

#define IsNull(aobj) ( NULL==aobj || Et==aobj->t && 0==aobj->n && 1==aobj->r)


/* Define iszero macro for AIX machines. */
#if defined(__osf__)
#define iszero(X)   (X==0.0)
#elif defined(_AIX) || defined(HAVE_SVR4)
#ifndef iszero
#define iszero(X)   (X==0.0)
#endif
#endif

#endif
