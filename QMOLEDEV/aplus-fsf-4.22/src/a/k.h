#ifndef included_a_k_h
#define included_a_k_h

/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <stdlib.h>
/* #include <a/arthur.h> */

typedef long I;
typedef double F;
typedef char C;
typedef unsigned char UC;

extern F aplusInfinity;
#define Inf aplusInfinity

#define MAXR 9
#define AH (sizeof(struct a) - sizeof(I))
typedef struct a *A;
typedef union{I *i;F *f;C *c;A *a;}P;
struct a{I c,t,r,n,d[MAXR],i,p[1];};
typedef struct _s{struct _s *s;C n[4];}*S;
#define CX_USED
typedef struct _ht{unsigned long nb,ni;void *b[1];}*HT;
typedef struct _htnode{I a;S s;struct _htnode *n;}*HTN;
#define HTHASH(ht,s) ((ht)->b+(((ht)->nb-1)&hafn(((unsigned long)(s))>>3)))
#define CxHTSIZE       (1<<9)

typedef struct _cx{HT ht;S s;struct _cx *n;I flag;}*CX;
typedef struct _att{I a;S s;struct _att *n;}*ATT;
typedef struct _v{I a;S s;struct _v *v;CX cx;I t,*l,e,r,o,f,c;
                void *attr;I z,p,q;A cd;I rff,rfc,rpf,rpc;A scd,i;HT atb;}*V;
typedef struct e{I n,f,a[2];}*E;

typedef I (*PFI_I)(I) ;
typedef I (*PFI)();
typedef I (*PFI_A)(A);
typedef I (*PFI_AA)(A,A);
typedef I (*PFI_AAI)(A,A,I);
typedef A (*PFA_A)(A);
typedef A (*PFA_AA)(A,A);

#if (_MIPS_SZLONG == 64) || defined(__alpha) || defined(__ia64) || defined(__x86_64)
typedef unsigned long * jmptype;
#else
typedef int * jmptype;
#endif

/* atmp Memory mode */
#define WS_ATMP_SHARED    (0)
#define WS_ATMP_NORESERVE (1)
#define WS_ATMP_PRIVATE   (2)
#define WS_ATMP_HEAP      (4)
#define WS_MALLOC         (8)
#define WS_MEM_STATS      (16)

#define Tt32(t,x) ((x)<<(t+2&3))
#define Tt64(t,x) ((x)<<((((t>>1)&1)+3)&3))
#if (_MIPS_SZLONG == 64) || defined(__alpha) || defined(__ia64) || defined(__x86_64)
#define Tt(t,x) ((x)<<((((t>>1)&1)+3)&3))
#else
#define Tt(t,x) ((x)<<(t+2&3))
#endif
#define Q(x,n) {if(x)R q=n,0;}
#define It 0L
#define Ft 1L
#define Ct 2L
#define Et 4L
#define Xt 8L
#define aplusMask 7
#define U(a) ((I)(a)>>3)
#define QA(a) (0==((I)(a)&aplusMask))
#define QV(a) (1==((I)(a)&aplusMask))
#define QS(a) (2==((I)(a)&aplusMask))
#define QE(a) (3==((I)(a)&aplusMask))
#define QN(a) (4==((I)(a)&aplusMask))
#define QL(a) (5==((I)(a)&aplusMask))
#define QP(a) (6==((I)(a)&aplusMask))
#define QX(a) (7==((I)(a)&aplusMask))
#define MV(a) (1|(I)(a))
#define MS(a) (2|(I)(a))
#define ME(a) (3|(I)(a))
#define MN(a) (4|(I)(a)<<3)
#define ML(a) (5|(I)(a)<<3)
#define MP(a) (6|(I)(a)<<3)
#define MX(a) (7|(I)(a)<<3)
#define XS(a) ((S)((I)(a)&~aplusMask))
#define XV(a) ((V)((I)(a)&~aplusMask))
#define XE(a) ((E)((I)(a)&~aplusMask))

/* MicroSoft visual C++ mangles these variable names*/
#if defined(_MSC_VER) && defined(__cplusplus)
extern "C" {
#endif

/* These variables are part of the A+ API. */
extern C *qs;
extern I APL,q;
extern A aplus_nl;
extern CX Rx,Cx;

#if defined(_MSC_VER) && defined(__cplusplus)
}
#endif

/*
 * APLUS_THREAD_SAFE_FUNCTIONS:  defined if "_r" (re-entrant) versions of system
 *                            functions are available on that architecture
 *                            (e.g., gmtime_r())
 */
#if (defined(__SUNPRO_CC) || defined(__GNUC__)) && defined(HAVE_SVR4) && !defined(__sgi)
#define APLUS_THREAD_SAFE_FUNCTIONS 1
#define APLUS_OLD_POSIX_THREAD_SAFE_FUNCTIONS 1
#endif

#if defined(_AIX) && defined(__xlC__) && defined(_AIX41)  /* xlC on AIX 4.1 */
#define APLUS_THREAD_SAFE_FUNCTIONS 1
#endif /* _AIX && __xlC__ && _AIX41 */

#if defined(__sgi)  /* SGI, all compilers */
#define APLUS_THREAD_SAFE_FUNCTIONS 1
#endif /* __sgi */

/*
 * Wrappers around system function that have mt-safe (_r) versions.
 * The macros take as many parameters as the _r versions. The regular
 * versions of these macros ignore extra parameters and thus they don't
 * have to be declared when APLUS_THREAD_SAFE_FUNCTIONS is not defined.
 * To declare a direntry struct for READDIR macro APLUS_DECLARE_DIRENT should be
 * used because it correctly allocates enough memory for dirEntry.
 */
#ifdef APLUS_THREAD_SAFE_FUNCTIONS
#define APLUS_DECLARE_DIRENT(dirEntry)  \
  char dirEntry##Buffer[1025+sizeof(struct dirent)];\
  struct dirent *dirEntry = (struct dirent *)dirEntry##Buffer;

#define APLUS_LOCALTIME(numSecs,pTm)                localtime_r(numSecs,pTm)
#define APLUS_GMTIME(numSecs,pTm)                   gmtime_r(numSecs,pTm)
#define APLUS_STRTOK(s1,s2,lasts)                   strtok_r(s1,s2,lasts)

#if defined(APLUS_OLD_POSIX_THREAD_SAFE_FUNCTIONS)
#define APLUS_ASCTIME(tp,charBuf,n)                 asctime_r(tp,charBuf,n)
#define APLUS_GETPWUID(uid,pStruct,charBuf,n,pwd)   (pwd=getpwuid_r(uid,pStruct,charBuf,n))
#if     defined(__EXTENSIONS__) || \
        (!defined(_POSIX_C_SOURCE) && !defined(_XOPEN_SOURCE)) || \
        (_POSIX_C_SOURCE - 0 >= 199506L) || defined(_POSIX_PTHREAD_SEMANTICS)
#if     defined(__STDC__)
#if     (_POSIX_C_SOURCE - 0 >= 199506L) || defined(_POSIX_PTHREAD_SEMANTICS)
#ifdef __PRAGMA_REDEFINE_EXTNAME
#define APLUS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (pwd=getpwnam_r(name,pStruct,charBuf,n))
#else  /* __PRAGMA_REDEFINE_EXTNAME */
#define APLUS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (getpwnam_r(name,pStruct,charBuf,n,&pwd),pwd)
#endif /* __PRAGMA_REDEFINE_EXTNAME */
#else  /* (_POSIX_C_SOURCE - 0 >= 199506L) || ... */
#define APLUS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (pwd=getpwnam_r(name,pStruct,charBuf,n))
#endif  /* (_POSIX_C_SOURCE - 0 >= 199506L) || ... */
#else  /* __STDC__ */
#if     (_POSIX_C_SOURCE - 0 >= 199506L) || defined(_POSIX_PTHREAD_SEMANTICS)
#ifdef __PRAGMA_REDEFINE_EXTNAME
#define APLUS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (pwd=getpwnam_r(name,pStruct,charBuf,n))
#else  /* __PRAGMA_REDEFINE_EXTNAME */
#define APLUS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (getpwnam_r(name,pStruct,charBuf,n,&pwd),pwd)
#endif /* __PRAGMA_REDEFINE_EXTNAME */
#else  /* (_POSIX_C_SOURCE - 0 >= 199506L) || ... */
#endif /* (_POSIX_C_SOURCE - 0 >= 199506L) || ... */
#endif /* __STDC__ */
#endif /* defined(__EXTENSIONS__) || (__STDC__ == 0 ... */

#if !defined(APLUS_GETPWNAM)
#define APLUS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (pwd=getpwnam_r(name,pStruct,charBuf,n))
#endif
#define APLUS_READDIR(dp,entry,res)                 (res=readdir_r(dp,entry))
#define APLUS_GETPROTOBYNAME(name,pStruct,charBuf,n,proto)    (proto=getprotobyname_r(name,pStruct,charBuf,n))
#define APLUS_GETHOSTBYNAME(name,pStruct,charBuf,n,pErr,host) (host=gethostbyname_r(name,pStruct,charBuf,n,pErr))
#else /* !APLUS_OLD_POSIX_THREAD_SAFE_FUNCTIONS */
#define APLUS_ASCTIME(tp,charBuf,n)                 asctime_r(tp,charBuf)
#define APLUS_GETPWUID(uid,pStruct,charBuf,n,pwd)   (getpwuid_r(uid,pStruct,charBuf,n,&pwd),pwd)
#define APLUS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (getpwnam_r(name,pStruct,charBuf,n,&pwd),pwd)
#define APLUS_READDIR(dp,entry,res)                 (readdir_r(dp,entry,&res),res)
#ifdef __sgi /* SGI does not have a thread-safe equivalent of getproto... and gethost... functions */
#define APLUS_GETPROTOBYNAME(name,pStruct,charBuf,n,proto)    (proto=getprotobyname(name))
#define APLUS_GETHOSTBYNAME(name,pStruct,charBuf,n,pErr,host) (host=gethostbyname(name))
#else /* !__sgi */
#define APLUS_GETPROTOBYNAME(name,pStruct,charBuf,n,proto)    (getprotobyname_r(name,pStruct,charBuf,n,&proto),proto)
#define APLUS_GETHOSTBYNAME(name,pStruct,charBuf,n,pErr,host) (gethostbyname_r(name,pStruct,charBuf,n,pErr,&host),host)
#endif /* __sgi */
#endif /* APLUS_OLD_POSIX_THREAD_SAFE_FUNCTIONS */

#else /* !APLUS_THREAD_SAFE_FUNCTIONS */

#define APLUS_DECLARE_DIRENT(dirEntry)
#define APLUS_LOCALTIME(numSecs,pTm)                localtime(numSecs)
#define APLUS_GMTIME(numSecs,pTm)                   gmtime(numSecs)
#define APLUS_STRTOK(s1,s2,lasts)                   strtok(s1,s2)
#define APLUS_ASCTIME(tp,charBuf,n)                 asctime(tp)
#define APLUS_GETPWUID(uid,pStruct,charBuf,n,pwd)   (pwd=getpwuid(uid))
#define APLUS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (pwd=getpwnam(name))
#define APLUS_READDIR(dp,entry,res)                 (res=readdir(dp))
#define APLUS_GETPROTOBYNAME(name,pStruct,charBuf,n,proto)    (proto=getprotobyname(name))
#define APLUS_GETHOSTBYNAME(name,pStruct,charBuf,n,pErr,host) (host=gethostbyname(name))
#endif /* APLUS_THREAD_SAFE_FUNCTIONS */

#endif
