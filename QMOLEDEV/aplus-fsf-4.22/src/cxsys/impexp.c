/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/development.h>
#include <stdio.h>

#include <dap/dap.h>

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>
#include <a/arthur.h>
#if defined(__cplusplus)
#include <strings.h>
#else 
#include <string.h>
#endif

#if defined(linux) || defined(__alpha) || defined(_WINDOWS) || defined(__i386) || defined(__ia64)
#define MS_LITTLE_ENDIAN  1
#endif

#if defined(__alpha) || ( defined(__sgi) && (_MIPS_SZLONG == 64)) || defined(__ia64) || defined(__x86_64)
#define MS_64BIT           1
#endif


/* Endian conversion routines */
static void ndn16copy(char *from, char *to, int n)
{
  for(;n--;from+=2,to+=2)
  {
    to[1]=from[0];to[0]=from[1];
  }
}

static void ndn32copy(char *from, char *to, int n)
{
  for(;n--;from+=4,to+=4)
  {
    to[3]=from[0];to[2]=from[1];to[1]=from[2];to[0]=from[3];
  }
}

static void ndn64copy(char *from, char *to, int n)
{
  for(;n--;from+=8,to+=8)
  {
    to[7]=from[0];to[6]=from[1];to[5]=from[2];to[4]=from[3];
    to[3]=from[4];to[2]=from[5];to[1]=from[6];to[0]=from[7];
  }
}


static long AbortNever=0;
static long *AbortFlagPtr=&AbortNever;

/* Description of ExportAObject() sizepass() and  fillpass()
 * -------------------------------------------------------
 * This is to convert from A format data to Msvp format data.
 * The A object is converted into a character vector suitable
 * for transmission through msvp.  The return is a 1 or 2 element
 * enclosed vector.  The first element is a scaler integer
 * completion code.  The second element, if present, is a character
 * vector.  The second element is only present when the first
 * element is the scalar integer zero.
 */

/* ImportAObject()
 *
 * In a single traversal of the object, it should be possible to
 * convert from the CDR format to A objects.  The header of the
 * CDR message should indicate that it is of dense form.  Pointer
 * form messages are rejected.  The CDRDLEN field is used to
 * index into the message and locate the data that will fill
 * the a objects described by the descriptors.  Each descriptor
 * is handled in the extract pass function.  If the extract pass
 * is called for a general array descriptor, then it will be called
 * recursively for each element of the general array.  The conversion
 * from APL2 types to A types is as follows: B1, B4 and B8 are
 * converted to It.  I2 and I4 are converted to It.  E4 and E8 are
 * converted to Ft.  E16 is rejected.  E4 and E8 are assumed to be in
 * IEEE floating point format.  J8, J16 and J32 are rejected.  C1
 * is converted to Ct.  C4 is rejected.  A8 is rejected.  Pn is
 * rejected.  Zn is rejected.  X0 is ignored.  G0 is converted into Et.
 */

/* addons assume CDRXRHOLEN and CDRRHOLEN less than or equal sizeof(I) */

#define CDRXRHOLEN 4
#define CDRRTLEN 1
#define CDRRLLEN 1
#define CDRRANKLEN 2
#define CDRRHOLEN 4
#define CDRFLAGSLEN 1
#define CDRLENLEN 3
#define CDRMEGAHDRLENLEN 4
#define CDRFLAGSMAGIC  0x80
#define CDRFLAGS64     0x01
#define CDRFLAGSLITTLE 0x02
#define CDRFLAGSMEGAHDR 0x04
#define MINIMPLEN 4
#define IMPRANK 1
#define ILEN sizeof(I)

#define ISMEGAHDR(hsz) ((hsz) & 0xff000000)

/*
 * This is a 64BIT-safe export struct header.
 */
typedef struct impexpHeader
{
  int   xrho;
  char  rt;
  char  rl;
  short rank;
  int   d[MAXR];
} impexpHeader;

/* 
   Export generates I-8 integers exclusively in 64-bit mode
   , however, Import will read
   I-2, I-4, or I-8. Thus Import should read byte streams exported
   by 32=bit versions of A+ 
*/
static I sizepass(a, hszp, dszp, long_bytes) 
     A a;
     I *hszp, *dszp;
     long long_bytes;
{
  I t;
  
  if (QS(a))
    {
      /* turn symbol into character vector */
      S s = XS(a);      
      *hszp += (I)(CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN+CDRRHOLEN);
      *dszp += strlen(s->n);
      return (I)0;
    }
  else if (QA(a))
    {
      *hszp += (I)(CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN) + (a->r * (I)CDRRHOLEN);
      if ((t = a->t) == Et)
	{
	  I rc, *p, n, i;
	  
	  if ((n = a->n) == (I)0)
	    {
	      /*descriptor length of prototype */
	      *hszp += (I)(2*(CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN)+CDRRHOLEN);
	      return (I)0;
	    }
	  p = a->p;
	  for (i = (I)0; i < n&&!*AbortFlagPtr; i++)
	    {
	      if (rc = sizepass((A)p[i], hszp, dszp, long_bytes))
		return rc;
	    }
	  return (I)(*AbortFlagPtr)?56:0;
	}
      switch (t)
	{
	case It: {
#ifdef MS_64BIT
	  /* we going to bail out of the export here if we don't like the
	     size of the values stuffed in 4 byte longs */
	  I j;
	  /* check for data overflow now */
	  if(long_bytes < sizeof(I)) {
	    for(j=0; j < a->n; j++)
	      if(a->p[j] > INT_MAX || a->p[j] < INT_MIN) 
		{
		  fprintf(stderr, 
			  "\343 Error export value > 32 bits (%ld)\n",
			  a->p[j]);
		  return (I)54;	/* exit(54) */
		}
	  }
#endif
	  *dszp += long_bytes * a->n; 
	  return (I)0;
	}
	case Ft: *dszp += (I)sizeof(F) * a->n; return (I)0;
	case Ct: *dszp += a->n; return (I)0;
	}
      return (I)54;
    }
  else
    {
      return (I)55;
    }
}

static I sizepasscover(a, hszp, dszp, long_bytes) 
     A a;
     I *hszp, *dszp;
     long long_bytes;
{
  I rc=sizepass(a, hszp, dszp, long_bytes);
  if (ISMEGAHDR(*hszp)) (*hszp) += CDRMEGAHDRLENLEN;
  return(rc);
}

static void fillpass(a, hpp, dpp, trp, for_a, long_bytes) 
     A a;
     char **hpp, **dpp;
     char *trp;
     I for_a;
     long long_bytes;
{
  /* cokelley - needed for little endian hack below */
  int i ;
	
  char *hp, *dp;
  I xrho;
  I t, dlen=0, hlen=0;
  char rt='\0', rl='\0';
  impexpHeader h;
  
  hp = *hpp;
  dp = *dpp;
  
  if (QS(a))
    {
      /* turn symbol into character vector */
      S s = XS(a);
      xrho   = strlen(s->n);

      /* write the header */
      h.xrho = htonl(xrho);
      h.rt   = (for_a ? 'S' : 'C');
      h.rl   = 1;
      h.rank = htons((short)1);
      h.d[0] = htonl(xrho);

      hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN+CDRRHOLEN;
      bcopy((char*)&h,hp,hlen); hp+=hlen;

      /* CDRDSECT */
      if (trp != (char *)(0))
	{
	  char *src, *end_dp;
	  
	  for (src = s->n, end_dp = dp + xrho;
	       dp != end_dp;
	       *dp++ = trp[(unsigned)(*src++ & 0xff)]);
	}
      else
	{
	  bcopy(s->n, dp, (I)xrho); 
	  dp += xrho;
	}
      
      *hpp = hp;
      *dpp = dp;
      return;
    }
  
  xrho = a->n;
  t = a->t;
  switch (t)
  {
    /* changed the rl value for type It */
  case It: rt = 'I'; rl = long_bytes; dlen = (I)rl * xrho; break;
  case Ft: rt = 'E'; rl = sizeof(F); dlen = (I)rl * xrho; break;
  case Ct: rt = 'C'; rl = sizeof(C); dlen = (I)rl * xrho; break;
  case Et: rt = 'G'; rl = 0; break;
  }
  
  h.xrho = htonl(xrho);
  h.rt   = rt;
  h.rl   = rl;
  h.rank = htons((short)a->r);
#if defined(MS_64BIT) || defined(MS_LITTLE_ENDIAN)
  /* CDRRHO */
  for(i=0; i< a->r; i++) {
    h.d[i] = htonl(a->d[i]);
  }
  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN+a->r*CDRRHOLEN;
  bcopy((char*)&h,hp,hlen); hp+=hlen;
#else  
  /* 32-bit big endian optimization for eficiency so that not to copy a->d twice.*/
  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN;
  bcopy((char*)&h,hp,hlen); hp+=hlen;
   
  hlen = (I)CDRRHOLEN * a->r;
  bcopy((char *)(a->d), hp, hlen); hp += hlen;
#endif
  if (t == Et)
    {
      I i;
      
      if (xrho == (I)0)
	{
	  h.xrho = htonl(1);
	  h.rt   = 'G';
	  h.rl   = 0;
	  h.rank = 0;
	  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN;
	  bcopy((char*)&h,hp,hlen); hp+=hlen;
	  
	  h.xrho = 0;
	  h.rt   = 'I';
	  h.rl   = (char)long_bytes;
	  h.rank = htons(1);
	  h.d[0] = htonl(xrho);
	  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN +CDRRHOLEN;
	  bcopy((char*)&h,hp,hlen); hp+=hlen;
	}
      *hpp = hp;
      *dpp = dp;
      for (i = 0; i < xrho&&!*AbortFlagPtr; i++)
	{
	  fillpass((A)a->p[i], hpp, dpp, trp, for_a,long_bytes);
	}
      return;
    }
  
  if ((t == Ct) && (trp != (char *)(0)))
    {
      char *src, *end_dp;
      
      for (src = (char *)(a->p), end_dp = dp + xrho;
	   dp != end_dp;
	   *dp++ = trp[(unsigned)(*src++ & 0xff)]);
    }
#ifdef MS_64BIT
   else if(t == It && (long_bytes < sizeof(I))) {
    /* 32 bit backward compatibility mode */
#ifndef MS_LITTLE_ENDIAN
      hlen = sizeof(I)-long_bytes;
#else
      hlen = 0;
#endif
    for(i=0; i < xrho; i++) {
      bcopy((char *)(a->p+i)+hlen, dp, long_bytes);
      dp += long_bytes;
    }
  }
#endif
  else
    {
      bcopy((char *)(a->p), dp, (I)dlen);
      dp += dlen;
    }
  *hpp = hp;
  *dpp = dp;
  return;
}

/* The extract pass function takes a pointer the end of the CDR
 * format buffer and pointers to pointers into the header and
 * data sections of the  CDR buffer.  It returns an A object that
 * is the importation of its part of the CDR format buffer.  If
 * the extraction fails, a zero pointer is returned.  The extract
 * pass handles general arrays by calling itself recursively.  If
 * One of these calls returns zero then extract pass frees its A
 * object and returns zero to its caller.
 */
static A extractpass(hpp, dpp, endp, erp, trp, swap)
     char **hpp;
     char **dpp;
     char *endp;
     I *erp;
     char *trp;
     int swap;
{
  char *hp, *dp;
  I xrho=0, rt, rl, rank, hlen;
  I i;
  A a;
  I d[MAXR];
  impexpHeader h;
  
  /* process descriptor into ga call */
  hp = *hpp;
  if ((endp - hp) < 8)
    {
      *erp = (I)1;
      return (A)0;
    }
  hlen = CDRXRHOLEN+CDRRTLEN+CDRRLLEN+CDRRANKLEN;
  bcopy(hp,(char*)&h,hlen); hp+=hlen;

  xrho = ntohl(h.xrho);
  rt   = h.rt;
  rl   = h.rl;
  rank = ntohs(h.rank); 

  if (rank > MAXR)
    {
      *erp = (I)1;
      return (A)0;
    }
  if ((endp - hp) < (CDRRHOLEN * rank))
    {
      *erp = (I)1;
      return (A)0;
    }
#if defined(MS_64BIT) || defined(MS_LITTLE_ENDIAN)
  for(i=0; i < rank; i++) {
    d[i] =ntohl((int)(*(int *)hp)); 
    hp += CDRRHOLEN;    
  }
  for(i=rank; i < MAXR; i++)    d[i] = 0;
#else
  /* this is more efficient in this case */
  bcopy(hp, (char *)(d), 4 * rank); hp += 4 * rank;
  bzero((char *)(d) + (4 * rank), 4 * (MAXR - rank));
#endif

  *hpp = hp;
  if ((rt == 'P')
      || (rt == 'J')
      || (rt == 'Z')
      || ((rt == 'C') && (rl == 4))
      || ((rt == 'E') && (rl == 16)))
    {
      /* nonce error */
      *erp = (I)1;
      return (A)0;
    }

  if (dpp == (char **)(0))
    {
      /* prototype */
      if ((rt == 'G') && (rl== 0))
	{
	  for (i = 0; i < xrho; i++)
	    {
	      (void)extractpass(hpp, dpp, endp, erp, trp,swap);
	      if (*erp != (I)0) break;
	    }
	  return (A)0;
	}
      if (((rt == 'X') && (rl == 0))
	  || ((rt == 'I') && ((rl == 2) || (rl == 4)
#ifdef MS_64BIT
			      || (rl == 8)
#endif
	      ))
	  || ((rt == 'B') && ((rl == 1) || (rl == 4) || (rl == 8)))
	  || ((rt == 'E') && ((rl == 4) || (rl == 8)))
	  || ((rt == 'C') && (rl == 1))
	  || ((rt == 'S') && (rl == 1)))
	{
	  return (A)0;
	}
      *erp = 1;
      return (A)0;
    }
  if ((rt == 'G') && (rl == 0))
    {
      A *ap;
      
      if (xrho == 0)
	{
	  /* do the prototype */
	  (void)extractpass(hpp, (char **)(0), endp, erp, trp,swap);
	  if (*erp != (I)0)
	    {
	      return (A)0;
	    }
	}
      a = ga(Et, rank, xrho, d);
      ap = (A *)(a->p);
      bzero((char *)ap, sizeof(I) * xrho);
      for (i = 0; i < xrho; i++)
	{
	  if ((ap[i] = extractpass(hpp, dpp, endp, erp, trp,swap)) == (A)0)
	    {
	      if (*erp != 0)
		{
		  dc(a);
		  return (A)0;
		}
	      /* type X0 filler */
	      i--;
	    }
	}
      return a;
    }
  dp = *dpp;

#ifdef MS_64BIT
  if ((rt == 'I') && (rl == 8))
    {
      C *ap;
      
      if ((endp - dp) < (rl * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (C *)(a->p);
      if(swap) ndn64copy(dp, ap, xrho);
      else bcopy(dp, ap, 8 * xrho); 
      dp += 8 * xrho;
      *dpp = dp;
     return a;
    }
  if ((rt == 'I') && (rl == 4))
    {
      I *ap;
      int itmp;
      
      if ((endp - dp) < (4 * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (i = 0; i < xrho; i++)
	{
	  if(swap) ndn32copy(dp, (char *)&itmp, 1);
	  else bcopy(dp, (char*)&itmp, 4);      
	  ap[i] = itmp;
	  dp += 4;
	}
      *dpp = dp;
      return a;
    }
#else
  if ((rt == 'I') && (rl == 4))
    {
      C *ap;
      
      if ((endp - dp) < (4 * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (C *)(a->p);
      if(swap) ndn32copy(dp, ap, xrho);
      else bcopy(dp, ap, 4 * xrho);      
      dp += 4 * xrho;
      *dpp = dp;
      return a;
    }
#endif
  if ((rt == 'I') && (rl == 2))
    {
      I *ap;
      short stmp;
      
      if ((endp - dp) < (2 * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (i = 0; i < xrho; i++)
	{
	  if(swap) ndn16copy(dp, (char *)&stmp, 1);
	  else bcopy(dp, (char*)&stmp, 2);      
	  ap[i] = stmp;
	  dp += 2;
	}
      *dpp = dp;
      return a;
    }
  if ((rt == 'E') && (rl == 4))
    {
      F *ap;
      float ftmp;
      
      if ((endp - dp) < (rl * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(Ft, rank, xrho, d);
      ap = (F *)(a->p);
      for (i = 0; i < xrho; i++)
	{
	  if(swap) ndn32copy(dp, (char *)&ftmp, 1);
	  else bcopy(dp, (char*)&ftmp, 4);      
	  ap[i] = ftmp;
	  dp += rl;
	}
      *dpp = dp;
      return a;
    }
  if ((rt == 'E') && (rl == 8))
    {
      C *ap;
      
      if ((endp - dp) < (rl * xrho))
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(Ft, rank, xrho, d);
      ap = (C *)(a->p);
      if(swap) ndn64copy(dp, ap, xrho);
      else bcopy(dp, ap, 8 * xrho);      
      dp += 8 * xrho;
      *dpp = dp;
      return a;
    }
  if ((rt == 'C') && (rl == 1))
    {
      C *ap;
      
      if ((endp - dp) < xrho)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(Ct, rank, xrho, d);
      ap = (C *)(a->p);
      if (trp != (char *)(0))
	{
	  for (i = 0; i < xrho; i++)
	    {
	      ap[i] = trp[(unsigned)(*dp++ & 0xff)];
	    }
	}
      else
	{
	  bcopy(dp, ap, xrho); dp += xrho;
	}
      *dpp = dp;
      return a;
    }
  if ((rt == 'S') && (rl == 1))
    {
      C *ap;
      I s;
      
      if ((endp - dp) < xrho)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(Ct, 1, xrho, d);
      ap = (C *)(a->p);
      if (trp != (char *)(0))
	{
	  for (i = 0; i < xrho; i++)
	    {
	      ap[i] = trp[(unsigned)(*dp++ & 0xff)];
	    }
	}
      else
	{
	  bcopy(dp, ap, xrho); dp += xrho;
	}
      *dpp = dp;
      s = MS(si(ap));
      dc(a);
      return (A)s;
    }
  if ((rt == 'B') && (rl == 8))
    {
      I *ap;
      
      if ((endp - dp) < xrho)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (i = 0; i < xrho; i++)
	{
	  ap[i] = *dp;
	  dp += 1;
	}
      *dpp = dp;
      return a;
    }
  if ((rt == 'B') && (rl == 4))
    {
      I *ap;
      I m;
      
      if ((endp - dp) < (xrho + 1)/2)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (m = 0, i = 0; i < xrho; i++)
	{
	  m = rl * (1 - (i % 2));
	  ap[i] = (*dp >> m) & 0x0f;
	  dp += (m == 0);
	}
      /* skip partially used byte */
      if (m != 0) dp++;
      *dpp = dp;
      return a;
    }
  if ((rt == 'B') && (rl == 1))
    {
      I *ap;
      I m;
      
      if ((endp - dp) < (xrho + 7)/8)
	{
	  *erp = 1;
	  return (A)0;
	}
      a = ga(It, rank, xrho, d);
      ap = (I *)(a->p);
      for (m = 0, i = 0; i < xrho; i++)
	{
	  m = 7 - (i % 8);
	  ap[i] = (*dp >> m) & 0x01;
	  dp += (m == 0);
	}
      /* skip partially used byte */
      if (m != 0) dp++;
      *dpp = dp;
      return a;
    }
  if ((rt == 'X') && (rl == 0))
    {
      if ((endp - dp) < xrho)
	{
	  *erp = 1;
	  return (A)0;
	}
      dp += xrho;
      *dpp = dp;
      return (A)0;
    }
  *erp = 1;
  return (A)0;
}


/* ExportAObject
 *
 * Creates a Msvp format char vector out of an A object.
 *
 * aobj is the A object to be abused.  trp is a translation vector, which 
 * must have length of 256, or be NULL.  for_a is a boolean flag, if 0 A
 * symbols are translated to character strings, 1 uses an extention to
 * Msvp format.
 *
 * Result is a pointer to the character vector created.  *plen is set to
 * the length of that vector, which may have embedded nulls.  In case of
 * error, the result is NULL, and *plen is an error code.
 *
 */
SUBROUTINE
void
FillExportBuffer(aobj, hp, hsz, trp, for_a, long_bytes)
     A aobj;
     char *hp, *trp;
     I hsz, for_a, long_bytes;
{
  char *dp = hp + hsz;
  int sz;
  unsigned char flag = CDRFLAGSMAGIC;


  /* CDRFLAGS mark export header indicating 64 or 32 bit longs and endiness */

  /* MEGAHDR flag.  hsz already contains extra space, if needed. */
  if (ISMEGAHDR(hsz)) flag |= CDRFLAGSMEGAHDR;

#ifdef MS_64BIT
  if(long_bytes == 8) flag |= CDRFLAGS64;
#endif
#ifdef MS_LITTLE_ENDIAN
  flag |= CDRFLAGSLITTLE;
#endif

  *hp = flag; 
  hp += CDRFLAGSLEN;

  if (ISMEGAHDR(hsz))
  {
    bzero(hp, CDRLENLEN);
    hp += CDRLENLEN;
    sz=htonl(hsz);
    bcopy((char *)(&sz), hp, CDRMEGAHDRLENLEN);
    hp += CDRMEGAHDRLENLEN;
  } else {
    sz = htonl(hsz);
    /* CDRDLEN */
    bcopy(((char *)(&sz))+1, hp, CDRLENLEN); 
    hp += CDRLENLEN;
  }
  fillpass(aobj, &hp, &dp, trp, for_a, long_bytes);
}

#ifdef __cplusplus
extern "C" char *ExportAObject(A,char *,I, I*);
#endif
char *
ExportAObject(aobj, trp, for_a, plen)
     A aobj;
     char *trp;
     I for_a;
     I *plen;
{
  char *cvp=NULL;	/* the character vector result */
/*  char *hp, *dp; */
  I hsz, dsz;

  AbortFlagPtr = &AbortNever; /* set flagptr to static, to not abort */

  hsz = CDRFLAGSLEN+CDRLENLEN;
  dsz = 0;
  /* the constant x last parameter in sizepass and fillExportBuffer
     forces exported longs to be x bytes */
  if ((*plen = sizepasscover(aobj, &hsz, &dsz, 4)) != 0) return(NULL);
  *plen=hsz+dsz;
  /*  cvp=(char *) malloc(*plen);*/
  cvp=(C *)balloc(*plen);
  FillExportBuffer(aobj, cvp, hsz, trp, for_a, 4);
  return(cvp);
}


I
ExportAObjectSizePass(aobj, trp, for_a, phsz, pdsz)
  A aobj;
  char *trp;
  I for_a;
  I *phsz, *pdsz;
{
  I rc;
  AbortFlagPtr = &AbortNever; /* set flagptr to static, to not abort */

  *phsz=4;
  *pdsz=0;
  /* the constant x last parameter in sizepass and fillExportBuffer
     forces exported longs to be x bytes */
  if ((rc = sizepasscover(aobj, phsz, pdsz,4)) != 0)
    {
      return(rc);
    }
  return(0);
}

I
ExportAObjectFillPass(aobj, trp, for_a, hsz, cvp)
  A aobj;
  char *trp;
  I for_a;
  int hsz;
  char *cvp;
{
  FillExportBuffer(aobj, cvp, hsz, trp, for_a, 4);
  return(0);
}

A
AExportAObject(aobj, trp, for_a, plen)
     A aobj;
     char *trp;
     I for_a;
     I *plen;
{
  A result=NULL;	/* the A object result */
  I hsz, dsz;
  
  AbortFlagPtr=&q;  /* set AbortFlagPtr to recognize SIGINT via global q. */
  hsz = CDRFLAGSLEN+CDRLENLEN;
  dsz = 0;  
  /* the constant x last parameter in sizepass and fillExportBuffer
     forces exported longs to be x bytes */
  if ((*plen = sizepasscover(aobj, &hsz, &dsz, 4)) != 0) return(NULL);
  *plen=hsz+dsz;
  result=gv(Ct,*plen);
  FillExportBuffer(aobj, (char *)result->p, hsz, trp, for_a, 4);
  if(*AbortFlagPtr){dc(result);return(NULL);}
  return(result);
}

#ifdef MS_64BIT
char *
Export64AObject(aobj, trp, for_a, plen)
     A aobj;
     char *trp;
     I for_a;
     I *plen;
{
  char *cvp=NULL;	/* the character vector result */
/*  char *hp, *dp; */
  I hsz, dsz;
  
  AbortFlagPtr = &AbortNever; /* set flagptr to static, to not abort */

  hsz = CDRFLAGSLEN+CDRLENLEN;
  dsz = 0;
  /* the constant x last parameter in sizepass and fillExportBuffer
     forces exported longs to be x bytes */
  if ((*plen = sizepasscover(aobj, &hsz, &dsz, 8)) != 0) return(NULL);
  *plen=hsz+dsz;
  cvp=(char *) malloc(*plen);
  FillExportBuffer(aobj, cvp, hsz, trp, for_a, 8);
  return(cvp);
}

A
AExport64AObject(aobj, trp, for_a, plen)
     A aobj;
     char *trp;
     I for_a;
     I *plen;
{
  A result=NULL;	/* the A object result */
  I hsz, dsz;
  
  AbortFlagPtr=&q;  /* set AbortFlagPtr to recognize SIGINT via global q. */

  hsz = CDRFLAGSLEN+CDRLENLEN;
  dsz = 0;  
  /* the constant x last parameter in sizepass and fillExportBuffer
     forces exported longs to be x bytes */
  if ((*plen = sizepasscover(aobj, &hsz, &dsz, 8)) != 0) return(NULL);
  *plen=hsz+dsz;
  result=gv(Ct,*plen);
  FillExportBuffer(aobj, result->p, hsz, trp, for_a, 8);
  if(*AbortFlagPtr){dc(result);return(NULL);}
  return(result);
}
#endif

/* ImportAObject
 *
 * Creates an A object out of Msvp format char vector
 *
 * cvp points to the beginning of the msvp vector.  cvlen is the length
 * of the vector.  trp is a translation vector, which must have length
 * of 256, or be NULL.
 *
 * Result is A object created, or (A)0 in case of error.
 *
 */
#ifdef __cplusplus
extern "C" A ImportAObject(C *,I,C*);
#endif
A ImportAObject(cvp, cvlen, trp)
     char *cvp;
     I cvlen;
     char *trp;
{
  A result;  /* will be (A)0 if an error is detected */
  char *endp;
  char *hp, *dp;
  I hsz, rc=0;
  char flag;
  int sz=0;
  int swap=0;

  if (MINIMPLEN>cvlen) return((A)0);
  hp = cvp;
  endp = cvp + cvlen;

  flag = *hp;
  if ((flag&0xf8) != CDRFLAGSMAGIC) return ((A)0);  /* 0xf8 == 11111000 */

  /* Do not accept 64 bit header if we are NOT 64 bit!. */
#if !defined(MS_64BIT)
  if (flag&CDRFLAGS64) return ((A)0);
#endif 
  /* Determine if we need to swap data bytes */
#if defined(MS_LITTLE_ENDIAN)
  if ((flag&CDRFLAGSLITTLE)==0) swap=1;
#else
  if (flag&CDRFLAGSLITTLE) swap=1;
#endif
  hp += CDRFLAGSLEN;

  /* Read header size.  This is in next CDRLENLEN (3) bytes unless MEGAHDR
   * flag is set.  In that case, skip CDRLENLEN bytes and read following
   * CDRMEGAHDRLENLEN (4) bytes.
   */
  
  if (flag&CDRFLAGSMEGAHDR)
  {
    hp+=CDRLENLEN;
    bcopy(hp, ((char *)(&sz)), CDRMEGAHDRLENLEN);
    hsz = ntohl(sz);
    hp+= CDRMEGAHDRLENLEN;
  } else  {
    bcopy(hp, ((char *)(&sz))+1, CDRLENLEN);
    hsz = ntohl(sz);
    hp += CDRLENLEN;
  }
  dp = cvp + hsz;
  result = extractpass(&hp, &dp, endp, &rc, trp, swap);
  return(result);
}

/***********************************************
 *
 * Entrypoints
 *
 */

ENTRYPOINT
A 
ep_ExportAObject(aobj, tr, for_a)
     A aobj;
     A tr;
     I for_a;
{
  char *trp;
  A z;	        /* the object that is returned */
  A cc;        /* the integer scalar completion code */
  A cv;        /* A object of export vector */
  I cvlen;              /* length of character vector result */
  
  cc = gi(1);
  z = gv(Et, (I)2);
  z->p[0] = (I)cc;
  z->n = z->d[0] = 1;
  
  if ((tr!=(A)0) && tr->n != 0) {
    if ((tr->t != Ct) || (tr->r != 1) || (tr->n != 256)) return(z);
    trp = (char *)(tr->p);
  } else trp = (char *)(0);
  
  if (NULL==(cv=AExportAObject(aobj, trp, for_a, &cvlen))) cc->p[0]=cvlen;
  else {
    z->n=z->d[0]=2;
    cc->p[0]=0;
    z->p[1]=(I)cv;
  }
  return(z);
}


ENTRYPOINT
A
ep_exp(aobj) A aobj;
{
  A z;
  I cvlen;

  if (0==(z=AExportAObject(aobj, (C *)0, 1, &cvlen))) 
    ERROUT((*AbortFlagPtr)?1:ERR_NONCE);
  R z;
}

#if defined(MS_64BIT)
ENTRYPOINT
A
ep_exp64(aobj) A aobj;
{
  A z;
  I cvlen;
  /* use this function from a machine with 64bit longs and you need to */
  /* export to a 64bit long machine */
  if (0==(z=(A)AExport64AObject(aobj, (C *)0, 1, &cvlen))) ERROUT(ERR_NONCE);
  R z;
}
#endif /* 64BIT */

ENTRYPOINT
A 
ep_ImportAObject(cv, tr)
     A cv;
     A tr;
{
  char *trp;
  A z;	/* the object that is returned */
  A cc;	/* the integer scalar completion code */
  A aobj;      /* the imported A object */
  
  cc = gi(1);
  z = gv(Et, (I)2);
  z->p[0] = (I)cc;
  z->n = z->d[0] = 1;
  
  if ((tr!=(A)0) && tr->n != 0) {
    if ((tr->t != Ct) || (tr->r != 1) || (tr->n != 256)) return(z);
    trp = (char *)(tr->p);
  } else trp = (char *)(0);
  
  if ((cv->t != Ct) || (cv->r != 1) || (cv->n < 4)) return(z); 
  
  if ((aobj=ImportAObject((char *)cv->p, cv->n, trp))!=(A)0) {
    cc->p[0]=0;
    z->n = z->d[0] = 2;
    z->p[1]=(I)aobj;
  }

  return(z);
}


ENTRYPOINT
A
ep_imp(acv) A acv;
{
  A z;
  if ( acv->t != Ct ) ERROUT(ERR_TYPE);
  if ( acv->r > 1 ) ERROUT(ERR_RANK);
  if ( acv->n < 4 ) ERROUT(ERR_DOMAIN);
  if ((A)0==(z=ImportAObject((char *)acv->p, acv->n, (C *)0))) ERROUT(ERR_DOMAIN);
  R z;
}


void impexpInstall()
{
  CX saveCx=Cx;
  Cx=cx("sys");

  install((PFI)ep_ExportAObject,"export", A_, 3, A_, A_, IV,0,0,0,0,0);
  install((PFI)ep_ImportAObject,"import", A_, 2, A_, A_,  0,0,0,0,0,0);
  install((PFI)ep_exp,             "exp", A_, 1, A_, 0,   0,0,0,0,0,0);
#if defined(MS_64BIT)
  install((PFI)ep_exp64,         "exp64", A_, 1, A_, 0,   0,0,0,0,0,0);
#endif /* MS_64BIT*/
  install((PFI)ep_imp,"imp", A_, 1, A_,0,0,0,0,0,0,0);

  Cx=saveCx;
  R;
}
