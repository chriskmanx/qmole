/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.
*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/* This file contains routines that identify and convertthe different
 * formats of mmap'd files used by A+
 */

#include <a/fncdcls.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <a/beam.h>

extern I dbg_tb;

static char *endianString(int endian)
{
  return (endian==ENDIAN_BIG)?"big":(endian==ENDIAN_LITTLE)?"little":
    (endian==ENDIAN_UNDEF)?"undef":"unknown";
}

typedef union {
  char *c;
  unsigned char *uc;
  u_int *u;
  long *i;
  double *f;
} EndianUnion;

static EndianUnion EndianTag={"abcd"};

static INT32 SwapEndianInt32(INT32 in)
{
  INT32 out;
  int i;
  unsigned char *to = (unsigned char *)&out;
  unsigned char *from = (unsigned char *)&in;
  for (i = 0; i < sizeof(INT32); i++)
    to[i] = from[3-i];
  return out;
}

static INT64 SwapEndianInt64(INT64 in)
{
  INT64 out;
  int i;
  unsigned char *to = (unsigned char *)&out;
  unsigned char *from = (unsigned char *)&in;
  for (i = 0; i < sizeof(INT64); i++)
    {
      to[i] = from[7-i];
    }
  return out;
}

static double SwapEndianDouble(double in)
{
  double out;
  int i;
  unsigned char *to = (unsigned char *)&out;
  unsigned char *from = (unsigned char *)&in;
  for (i = 0; i < sizeof(double); i++)
    {
      to[i] = from[(sizeof(double)-1)-i];
    }
  return out;
}

static double SwapEndianDouble32(double in)
{
  double out;
  int i;
  unsigned char *to = (unsigned char *)&out;
  unsigned char *from = (unsigned char *)&in;
  for (i = 0; i < sizeof(INT32); i++)
    {
      to[i] = from[3-i];
    }
  return out;
}

static void SwapEndianHeader64(A64 *header)
{
  int i;
  header->c = SwapEndianInt64(header->c);
  header->t = SwapEndianInt64(header->t);
  header->r = SwapEndianInt64(header->r);
  header->n = SwapEndianInt64(header->n);
  header->i = SwapEndianInt64(header->i);
  for (i = 0; i < 9; i++)
    header->d[i] = SwapEndianInt64(header->d[i]);
}
		
static void SwapEndianHeader32(A32 *header)
{
  int i;
  header->c = SwapEndianInt32(header->c);
  header->t = SwapEndianInt32(header->t);
  header->r = SwapEndianInt32(header->r);
  header->n = SwapEndianInt32(header->n);
  header->i = SwapEndianInt32(header->i);
  for (i = 0; i < 9; i++)
    header->d[i] = SwapEndianInt32(header->d[i]);
}
		
/* tr and tr32 -- from k.c */

INT64 tr64(INT64 r, INT64 *d)
{
  INT64 n=1;
  INT64 *t;
	
  if (r)
    for (t = d + r , n = *d; ++d < t; n *= *d);
  return n;
}

static INT32 tr32(INT32 r, INT32 *d)
{
  INT32 n=1;
  INT32 *t;
	
  if (r)
    for (t = d + r , n = *d; ++d < t; n *= *d);
  return n;
}

static void GetHostInformation(int *width, int *endian)
{
  /* 1. Determine what endian we are */
  if (0x61626364==*EndianTag.u)
    *endian = ENDIAN_BIG;
  else
    {
      if (0x64636261==*EndianTag.u)
	*endian = ENDIAN_LITTLE;
      else
	*endian = ENDIAN_UNDEF;
    }

  /* 2. Determine what width we are */
  *width = sizeof(long) * 8;
}

	
static int GetSrcInformation(void *src, I iBytesRead, I noSizeChk, 
		      int *width, int *endian,
		      int iHostWidth, int iHostEndian)
{
  long iPredLen;
  long iLen;
  int sizeOK=1;
  char caHeaderBuf[sizeof(A64)];
  A32 a32BigHeader;
  A64 a64BigHeader;
  A32 a32LittleHeader;
  A64 a64LittleHeader;
  struct stat statbuf;
	

  if( iBytesRead>0 && (iBytesRead<sizeof(A32)-sizeof(INT32)) )
    {
      printf("\343 Error:File too small to be a mapped file\n");
      return -1;
    }

  /* Read in the header - use the worst case (64-bit) */
  memcpy(caHeaderBuf, src, sizeof(A64));

  memcpy((char *)&a32BigHeader,    (char *)caHeaderBuf, sizeof(A32)); 
  memcpy((char *)&a32LittleHeader, (char *)caHeaderBuf, sizeof(A32)); 
  memcpy((char *)&a64BigHeader,    (char *)caHeaderBuf, sizeof(A64)); 
  memcpy((char *)&a64LittleHeader, (char *)caHeaderBuf, sizeof(A64)); 

  if (iHostEndian == ENDIAN_BIG)
    {
      SwapEndianHeader32(&a32LittleHeader);
      SwapEndianHeader64(&a64LittleHeader);
    }
  else
    {
      SwapEndianHeader32(&a32BigHeader);
      SwapEndianHeader64(&a64BigHeader);
    }
		
	
  iLen = iBytesRead;

  /* 3. Look at the file. 
   * Analyze the possibilities:
   * 		32-bit big endian
   *		32-bit little endian
   *		64-bit big endian
   *		64-bit little endian
   */

  /* The format of a 32-bit file is:
   * ccccttttrrrrnnnnd...iiiip...
   * big endian
   * 0000000t000rnnnnd...iiiip...
   * little endian
   * 0000t000r000nnnnd...iiiip...
   *
   * The format of a 64-bit file is:
   * ccccccccttttttttrrrrrrrrnnnnnnnnd...
   * big endian
   * 000000000000000t0000000rnnnnnnnnd...
   * little endian
   * 00000000t0000000r0000000nnnnnnnnd...
   *
   * Format		Type	Rank	Num
   * 32/big:		8		12		13-16
   * 32/little:	5		9		13-16
   * 64/big:		16		24		25-32
   * 64/little:	9		17		25-32
   * The conflict here is between 32 and 64 bit little-endian files;
   * an int-type 32/little may appear to be a 64/little file.
   * Fortunately in this case the num field will distinguish as the
   * 32-bit num field corresponds to 0 in the 64/little file.
   */

  /*
    printf("64 bit big endian:\n");
    printf("Count: %lx\n", a64BigHeader.c);
    printf("Type: %lx\n", a64BigHeader.t);
    printf("Rank: %lx\n", a64BigHeader.r);
    printf("Num: %lx\n", a64BigHeader.n);
    printf("Items: %lx\n", a64BigHeader.i);
    printf("Size: %ld\n", sizeof(A64) - sizeof(INT64) + Tt64(a64BigHeader.t,
    a64BigHeader.i) + (a64BigHeader.t == 2));
    printf("32 bit big endian:\n");
    printf("Count: %lx\n", a32BigHeader.c);
    printf("Type: %lx\n", a32BigHeader.t);
    printf("Rank: %lx\n", a32BigHeader.r);
    printf("Num: %lx\n", a32BigHeader.n);
    printf("Items: %lx\n", a32BigHeader.i);
    printf("Size: %ld\n", 
    sizeof(A32) - sizeof(INT32) + Tt32(a32BigHeader.t, a32BigHeader.i *
    tr32(a32BigHeader.r - 1, a32BigHeader.d + 1)) +
    (a32BigHeader.t == 2));
  */

#if defined(linux)

#if defined(__ia64) || defined(__x86_64)
  /* Try 64-bit little endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
  if ( isAObject(a64LittleHeader) )
    {
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a64LittleHeader);

      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize64(a64LittleHeader)<=iLen))
	{
	  *width = 64;
	  *endian = ENDIAN_LITTLE;
	  return 0;
	}
    }

  /* Try 64-bit big endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
  if ( isAObject(a64BigHeader) )
    {
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a64BigHeader);

      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize64(a64BigHeader)<=iLen))
	{
	  *width = 64;
	  *endian = ENDIAN_BIG;
	  return 0;
	}
    }
#endif

  /* Try 32-bit little endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
  if ( isAObject(a32LittleHeader) )
    {
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a32LittleHeader);

      /* Check file length */
       if (dimsOK && 
	   noSizeChk || (sizeOK=checkFileSize32(a32LittleHeader)<=iLen))
	{
	  *width = 32;
	  *endian = ENDIAN_LITTLE;
	  return 0;
	}
    }

  /* Try 32-bit big endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
     if( isAObject(a32BigHeader) )
     {
       
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a32BigHeader);

      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize32(a32BigHeader)<=iLen))
	{
	  *width = 32;
	  *endian = ENDIAN_BIG;
	  return 0;
	}
    }
		

#elif (_MIPS_SZLONG == 64) || defined(__alpha) || defined(__sparcv9) 
  /* Try 64-bit big endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
  if ( isAObject(a64BigHeader) )
    {

       int dimsOK=1;
       /* Check dimensions */
       checkDims(a64BigHeader);

      /*
	printf("In 64 bit big endian\n");
	printf("Count: %lx\n", a64BigHeader.c);
	printf("Type: %lx\n", a64BigHeader.t);
	printf("Rank: %lx\n", a64BigHeader.r);
	printf("Num: %lx\n", a64BigHeader.n);
	printf("Items: %lx\n", a64BigHeader.i);
	printf("Size: %ld\n", sizeof(A64) - sizeof(INT64) + Tt64(a64BigHeader.t,
	a64BigHeader.i) + (a64BigHeader.t == 2));
      */
      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize64(a32BigHeader)<=iLen))
	{
	  *width = 64;
	  *endian = ENDIAN_BIG;
	  return 0;
	}
    }
		

  /* Try 64-bit little endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
  if ( isAObject(a64LittleHeader) )
    {
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a64LittleHeader);

      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize64(a64LittleHeader)<=iLen))
	{
	  *width = 64;
	  *endian = ENDIAN_LITTLE;
	  return 0;
	}
    }

  /* Try 32-bit big endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
     if( isAObject(a32BigHeader) )
     {
       
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a32BigHeader);

      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize32(a32BigHeader)<=iLen))
	{
	  *width = 32;
	  *endian = ENDIAN_BIG;
	  return 0;
	}
    }
		

  /* Try 32-bit little endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
  if ( isAObject(a32LittleHeader) )
    {
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a32LittleHeader);

      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize32(a32LittleHeader)<=iLen))
	{
	  *width = 32;
	  *endian = ENDIAN_LITTLE;
	  return 0;
	}
    }

#else
  /* Try 32-bit big endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
     if( isAObject(a32BigHeader) )
     {
       
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a32BigHeader);

      /* Check file length */
      if (dimsOK && 
          noSizeChk || (sizeOK=checkFileSize32(a32BigHeader)<=iLen))
	{
	  *width = 32;
	  *endian = ENDIAN_BIG;
	  return 0;
	}
    }
		

  /* Try 32-bit little endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
  if ( isAObject(a32LittleHeader) )
    {
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a32LittleHeader);

      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize32(a32LittleHeader)<=iLen))
	{
	  *width = 32;
	  *endian = ENDIAN_LITTLE;
	  return 0;
	}
    }

  /* Try 64-bit big endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
  if ( isAObject(a64BigHeader) )
    {

       int dimsOK=1;
       /* Check dimensions */
       checkDims(a64BigHeader);

      /*
	printf("In 64 bit big endian\n");
	printf("Count: %lx\n", a64BigHeader.c);
	printf("Type: %lx\n", a64BigHeader.t);
	printf("Rank: %lx\n", a64BigHeader.r);
	printf("Num: %lx\n", a64BigHeader.n);
	printf("Items: %lx\n", a64BigHeader.i);
	printf("Size: %ld\n", sizeof(A64) - sizeof(INT64) + Tt64(a64BigHeader.t,
	a64BigHeader.i) + (a64BigHeader.t == 2));
      */
      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize64(a64BigHeader)<=iLen))
	{
	  *width = 64;
	  *endian = ENDIAN_BIG;
	  return 0;
	}
    }
		

  /* Try 64-bit little endian */
  /* Valid values: 0 <= type <= 8, 0 < rank <= 9, n > 0 */
  if ( isAObject(a64LittleHeader) )
    {
       int dimsOK=1;
       /* Check dimensions */
       checkDims(a64LittleHeader);

      /* Check file length */
      if (dimsOK && 
	  noSizeChk || (sizeOK=checkFileSize64(a64LittleHeader)<=iLen))
	{
	  *width = 64;
	  *endian = ENDIAN_LITTLE;
	  return 0;
	}
    }

#endif		
		
  if(!sizeOK)
    printf("\343 Error:File too small for items. use _items{} to query/fix.\n");
  else
    printf("\343 Error:File does not match any known type.\n");

  return -1;
}

static int Convert32to64(A32 *from, A64 *to)
{
  long items = from->n;
  int i;

  if(((void *)from) == ((void *)to) )
    {
      /* This conversion done in mapDotMFile() */
      return -1;
    }

  to->c = from->c;
  to->t = from->t;
  to->r = from->r;
  to->n = from->n;
  to->i = from->i;
  for (i = 0; i < 9; i++)
    to->d[i] = from->d[i];
  switch(from->t)
    {
    case 0: /* Int */
      for (i = 0; i < items; i++)
	{
	  to->p[i] = from->p[i];
	}
      break;
    case 1: /* Float */
      {
	double *a = (double *)from->p;
	double *b = (double *)to->p;
	for (i = 0; i < items; i++)
	  b[i] = a[i];
	break;
      }
    case 2: /* Char */
      {
	char *a = (char *)from->p;
	char *b = (char *)to->p;
	for (i = 0; i <= items; i++) /* Include null  '<=' vs '<'  */
	  b[i] = a[i];
	break;
      }
    default: /* Can't handle it */
      printf("\343 Error:Type %d not translatable.\n", from->t);
      return -1;
    }
  return 0;
}

static int ConvertEndian32(A32 *from, A32 *to)
{
  int i;
  long items;
  if(to!=from)
    memcpy(to, from, sizeof(A32) - sizeof(INT32));
  SwapEndianHeader32(to);
  items=to->n;
  switch(to->t)
    {
    case 0: /* Int */
      for (i = 0; i < items; i++)
	to->p[i] = SwapEndianInt32(from->p[i]);
      break; 
    case 1: /* Float */
      {
	double *out = (double *)to->p;
	double *in =  (double *)from->p;
	for (i = 0; i < items; i++)
	  out[i] = SwapEndianDouble(in[i]);
	break;
      }
    case 2: /* Char */
      {
	if(to!=from)
	  {
	    memcpy((char *)to->p, (char *)from->p, items+1); /* include null */
	  }
	break;
      }
    default: /* Bad type */
      printf("\343 Error:Type %d not translatable.\n", from->t);
      return -1;
    }
  return 0;
}

static int ConvertEndian64(A64 *from, A64 *to)
{
  int i;
  long items;
  if(to!=from)
    memcpy(to, from, sizeof(A64) - sizeof(INT64));
  SwapEndianHeader64(to);
  items=to->n;
  switch(to->t)
    {
    case 0: /* Int */
      for (i = 0; i < items; i++)
	to->p[i] = SwapEndianInt64(from->p[i]);
      break; 
    case 1: /* Float */
      {
	double *out = (double *)to->p;
	double *in =  (double *)from->p;
	for (i = 0; i < items; i++)
	  out[i] = SwapEndianDouble(in[i]);
	break;
      }
    case 2: /* Char */
      {
	if(to!=from)
	  {
	    memcpy((char *)to->p, (char *)from->p, items+1); /* include null */
	  }
	break;
      }
    default: /* Bad type */
      printf("\343 Error:Type %ld not translatable.\n", from->t);
      return -1;
    }
  return 0;
}
		

int cvtIfNeeded(void *src, A *dest, I ilen, I cvtInPlace)
{
  /* Converts the src object to the host format if needed   */
  /*  arguments:                                            */
  /*    src  - pointer to the source data                   */
  /*    dest - address of the pointer to the destination    */
  /*    ilen - if src is a mapped file then the file length */
  /*  Returns:                                              */
  /*    0 - no conversion required (dest==NULL)             */
  /*    1 - Conversion successful  (dest==*convertedAobj)   */
  /*   -1 - Conversion failed      (dest==NULL              */

  static int hostWidth = -1;
  static int hostEndian= -1;
  static int noSizeChk=0;	/* Flag to skip file size check */
  int srcWidth;
  int srcEndian;
  int rc;

  /* Determine host characteristics */
  if(hostWidth==-1)
    GetHostInformation(&hostWidth, &hostEndian);
  
  if(hostEndian == ENDIAN_UNDEF) 
    { 
      printf("\343 Error:Unable to determine host endian\n"); 
      dest=NULL;
      return rc=-1; 
    }

  /* Check and convert source */
  rc=GetSrcInformation(src, ilen, DO_FILE_SIZE_CHECK,
		       &srcWidth, &srcEndian, 
		       hostWidth, hostEndian);
  if(rc!=0)
    return -1;

  /* START CONVERSIONS */
  /* Case 1: No Conversions */
  if( srcEndian == hostEndian &&  srcWidth  == hostWidth)
    {
      *dest=src;      /* *dest=src; */
      return 0;
    }

  /* Case 2: Big/32 -> Big/64 or Little32 -> little64  */
  if ( srcEndian  == hostEndian && 
       srcWidth   == 32 && hostWidth  == 64 )
    {
      A32 *from = (A32 *)src;
      long items = from->n;
      long size = (from->t==2) + sizeof(A64) - sizeof(INT64) + Tt64(from->t, items);
      A64 *to = (A64 *)mab(size);

      rc = Convert32to64(from, to);
      if (rc)
	{
	  printf("\343 Error:Convert32to64 failed\n");
	  *dest=NULL;
	  mf((I*)to);
	  return -1;
	}
      else
	{
	  /* to->i= (to->r) ? to->d[0] : to->n;  fix items */ 
	  to->c=1;		/* set reference count */
	  *dest=(A)to;
	  return 1;
	}
    }

  /* Case 3: Big/32 <--> Little/32 */
  if ( srcEndian != hostEndian &&
       srcWidth  == 32         && hostWidth  == 32)
    {
      
      A32 *to, *from = (A32 *)src;
      A32 wrk32;
      long items,size32;

      if( cvtInPlace )
	{
	  static char t[]={"in place"};
	  if(dbg_tb) beamtrc(t,3,0); /* 3==Converting */
	  to = (A32 *)src;
	  to->c=1;		/* set invalid reference count */
	}
      else
	{
	  A32 wrk32;
	  memcpy(&wrk32, src, sizeof(A32) - sizeof(INT32) ); /* copy header */
	  SwapEndianHeader32(&wrk32);                     /* correct endian */
	  
	  items=wrk32.n;		              /* number of elements */
	  size32=(wrk32.t==2)+sizeof(A32)-sizeof(INT32)+Tt32(wrk32.t, items);
	  to = (A32 *)mab(size32);
	}

      rc = ConvertEndian32(from, to);
      if (rc)
	{
	  printf("\343 Error:ConvertEndian32 failed\n");
	  if( cvtInPlace )
	    printf("\343 File is most likely corrupted-in place conversion\n");
	  else
	    mf((I*)to);
	  
	  *dest=(A)NULL;
	  return -1;
	}
	
      else 
	{
	  if( cvtInPlace )
	    to->c=0;		/* restore valid reference count */
	  else
	    to->c=1;		/* set reference count */

	  /*   to->i= (to->r) ? to->d[0] : to->n;  fix items  */
	  *dest=(A)to;
	  return cvtInPlace?0:1;
	}
    }

  /* Case 4: Big/64 <--> Little/64 */
  if ( srcEndian != hostEndian &&
       srcWidth  == 64         && hostWidth  == 64)
    {
      
      A64 *to, *from = (A64 *)src;
      long items,size64;

      if( cvtInPlace )
	{
	  static char t[]={"in place"};
	  if(dbg_tb) beamtrc(t,3,0); /* 3==Converting */
	  to = (A64 *)src;
	  to->c=1;		/* set invalid reference count */
	}
      else
	{
	  A64 wrk64;
	  memcpy(&wrk64, src, sizeof(A64) - sizeof(INT64) ); /* copy header */
	  SwapEndianHeader64(&wrk64);                     /* correct endian */
	  
	  items=wrk64.n;		              /* number of elements */
	  size64=(wrk64.t==2)+sizeof(A64)-sizeof(INT64)+Tt64(wrk64.t, items);
	  to = (A64 *)mab(size64);
	}

      rc = ConvertEndian64(from, to);
      if (rc)
	{
	  printf("\343 Error:ConvertEndian64 failed\n");
	  if( cvtInPlace )
	    printf("\343 File is most likely corrupted in place conversion\n");
	  else
	    mf((I*)to);

	  *dest=(A)NULL;
	  return -1;
	}
	
      else 
	{
	  /* if( !cvtInPlace ) */
	  /*   to->i= (to->r) ? to->d[0] : to->n;  fix items  */
	  if( cvtInPlace )
	    to->c=0;		/* restore valid reference count */
	  else
	    to->c=1;		/* set reference count */

	  *dest=(A)to;
	  return  cvtInPlace?0:1;
	}
    }


  /* Case 5: Big/32 -> Little/64  or Little/32 -> Big64 */
  if ( srcEndian !=  hostEndian &&
       srcWidth  == 32          && hostWidth  == 64)
    {
      A32 *from = (A32 *)src;
      A32 wrk;
      A32 *toa;
      A64 *tob;
      long items;
      long size32,size64;

      memcpy(&wrk, src, sizeof(A32) - sizeof(INT32) ); /* copy header */
      SwapEndianHeader32(&wrk);                /* correct endian */

      items=wrk.n;		/* Get the number of elements */

      size32=(wrk.t==2) + sizeof(A32) - sizeof(INT32) + Tt32(wrk.t, items);
      size64=(wrk.t==2) + sizeof(A64) - sizeof(INT64) + Tt64(wrk.t, items);

      toa = (A32 *)mab(size32);
      tob = (A64 *)mab(size64);

      rc = ConvertEndian32(from, toa);
      if (rc)
	{
	  printf("\343 Error:ConvertEndian32 failed\n");
	  *dest=NULL;
	  mf((I*)toa);
	  mf((I*)tob);
	  return -1;
	}
      else
	{
	  rc = Convert32to64(toa, tob);
	  if (rc)
	    {
	      printf("\343 Error:Convert32to64 failed\n");
	      *dest=(A)NULL;
	      mf((I*)toa);
	      mf((I*)tob);
	      return -1;
	    }
	  else
	    {
	      mf((I*)toa);
	      /* tob->i= (tob->r) ? tob->d[0] : tob->n;  fix items */ 
	      tob->c=1;		/* set reference count */
	      *dest=(A)tob;
	      return 1;
	    }
	}
    }

  /* TODO: downward conversions 64 -> 32*/
  
  return -1;	
}

int getItems(void *src, I *itemCount, I *rank, I *items, I ilen)
{
  /* Converts the src object to the host format if needed   */
  /*  arguments:                                            */
  /*    src  - pointer to the source data                   */
  /*    itemCount - src->i                                  */
  /*    rank - src->r                                       */
  /*    items - src->d[0]                                   */
  /*    ilen - if src is a mapped file then the file length */
  /*  Returns:                                              */
  /*    0 - no conversion required (dest==NULL)             */
  /*    1 - Conversion successful  (dest==*convertedAobj)   */
  /*   -1 - Conversion failed      (dest==NULL              */

  static int hostWidth = -1;
  static int hostEndian= -1;
  int srcWidth;
  int srcEndian;
  int rc;

  /* Determine host characteristics */
  if(hostWidth==-1)
    GetHostInformation(&hostWidth, &hostEndian);
  
  if(hostEndian == ENDIAN_UNDEF) 
    { 
      printf("\343 Error:Unable to determine host endian\n"); 
      return rc=-1; 
    }

  /* Check and convert source */
  rc=GetSrcInformation(src, ilen, SKIP_FILE_SIZE_CHECK,
		       &srcWidth, &srcEndian, 
		       hostWidth, hostEndian);
  if(rc!=0)
    return -1;

  /* START CONVERSIONS */
  /* Case 1: No Conversions */
  if( srcEndian == hostEndian &&  srcWidth  == hostWidth)
    {
      *itemCount = ((A)src)->i;   
      *items     = ((A)src)->d[0];   
      *rank      = ((A)src)->r;   
      return 0;
    }

  /* Case 2: 32 -> 64 same endian*/
  if ( srcEndian  == hostEndian &&
       srcWidth   == 32         && hostWidth  == 64 )
    {
      *itemCount = ((A32 *)src)->i;
      *items     = ((A32 *)src)->d[0];
      *rank      = ((A32 *)src)->r;
      return 1;
    }

  /* Case 3: Big/32 <--> Little/32 */
  if ( srcEndian != hostEndian &&
       srcWidth  == 32         && hostWidth  == 32)
    {
      
      A32 *to, *from = (A32 *)src;
      A32 wrk32;

      memcpy(&wrk32, src, sizeof(A32) - sizeof(INT32) ); /* copy header */
      SwapEndianHeader32(&wrk32);                /* correct endian */
      *itemCount = wrk32.i;		
      *items     = wrk32.d[0];		
      *rank      = wrk32.r;		
      return 1;
    }

  /* Case 4: Big/64 <--> Little/64 */
  if ( srcEndian != hostEndian &&
       srcWidth  == 64         && hostWidth  == 64)
    {
      A64 *to, *from = (A64 *)src;
      A64 wrk64;

      memcpy(&wrk64, src, sizeof(A64) - sizeof(INT64) ); /* copy header */
      SwapEndianHeader64(&wrk64);                /* correct endian */
      *itemCount = wrk64.i;
      *items     = wrk64.d[0];
      *rank      = wrk64.r;
      return 1;
    }

  /* Case 5: Big/32 -> Little/64 */
  if ( srcEndian != hostEndian &&
       srcWidth  == 32         && hostWidth  == 64)
    {
      A32 *from = (A32 *)src;
      A32 wrk;
      A32 *toa;
      A64 *tob;

      memcpy(&wrk, src, sizeof(A32) - sizeof(INT32) ); /* copy header */
      SwapEndianHeader32(&wrk);                /* correct endian */

      *itemCount = wrk.n;	
      *items     = wrk.d[0];	
      *rank      = wrk.r;	

      return 1;
    }

  /* TODO: downward conversions 64->32 */
  
  return -1;	
}

	
	
