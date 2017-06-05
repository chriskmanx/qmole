/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/*
 * Binary search for A
 *
 * Malcolm Austin
 *
 */

#include <a/development.h>
#if defined(_AIX) || defined(HAVE_SVR4)
#define iszero(X)   (X==0.0)
#endif

#include <stdio.h>
#if defined(__cplusplus)
#include <strings.h>
#else 
#include <string.h>
#endif
#include <math.h>

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>

#undef ENTRYPOINT
#define ENTRYPOINT static

#define REG register


/************************************************************************
 *
 * Comparision routines
 *
 */

static int Itemcount;

static
int
bs_Ccmp( s1, s2) UC *s1, *s2; 
{
  REG int count=Itemcount, diff=0;
  while (count-- && !diff) diff = *s1++ - *s2++;
  return(diff);
}

static
int 
bs_Icmp( s1, s2) I *s1, *s2; 
{
  REG int count=Itemcount, diff=0;
  while (count-- && !diff) diff = *s1++ - *s2++;
  return(diff);
}


#define CT (1+1e-13)
#if defined(_AIX) || defined(HAVE_SVR4) || defined(__osf__) || defined(linux) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#define isnegative(x) (x<0.0)
#else
#define isnegative(x) (signbit(x)&&!iszero(x)) 
#endif
static
int
bs_FcmpCT( s1, s2) F *s1, *s2;
{
  REG int count=Itemcount;
  REG int result=0;
  REG F a, b;

  while(count-- && !result) {
    a=*s1++; b=*s2++;
    if (isnegative(b) != (result = isnegative(a))) {
      result = (result)?-1:1;
      break;
    }
    if (result) { a=-a; b=-b; }
    if (a<b) result = ((b<=CT*a)?0:((result)?1:-1));
    else result = ((a<=CT*b)?0:((result)?-1:1));
  }
  return(result);
}

/**/
/************************************************************************
 *
 * Binary search routines
 *
 */

static int inequality;
static int indexvector;
static int bothends;

static struct strrange {
  int flag;
  I hi, lo;
} Range_Setting;

#define RANGE_SEARCH_SET(LOW,HIGH)   { Range_Setting.lo = (LOW) ; \
				    Range_Setting.hi = (HIGH) ; \
				    Range_Setting.flag = 1; \
				  }

#define RANGE_SEARCH_UNSET        { Range_Setting.flag=0; }

/* bsu - binary search: works for uniques only */
SUBROUTINE
int
bsu( key, table, tabsize, itemsize, cmpfn)
     char *key, *table;
     int tabsize, itemsize;
     int (*cmpfn)();
{
  REG int comp, mid, lo=0, hi=tabsize;

  while (lo<hi) {
    mid = lo+(hi-lo)/2;
    comp = (*cmpfn)( key, table+(itemsize*mid));
    /*printf ("hi:%d lo:%d mid:%d comp:%d\n", hi, lo, mid, comp);*/
    if (0 > comp) hi = mid;
    else if (0 < comp) lo = mid+1;
    else return(mid);
  }
  return(tabsize);
} /* Program: bsu */



SUBROUTINE
int
bsi( key, table, tabsize, itemsize, cmpfn)   /* bsi: find first */
     char *key, *table;
     int tabsize, itemsize;
     int (*cmpfn)();
{
  REG int comp; REG int mid; REG int lo=0; REG int hi=tabsize; 
  REG int result=tabsize;

  if (Range_Setting.flag) { lo=Range_Setting.lo; hi=result=Range_Setting.hi; }

  while (lo<hi) {
    mid = lo+(hi-lo)/2;
    comp = (*cmpfn)( key, table+(itemsize*mid));
    if (0 < comp) lo = mid+1;
    else { if (inequality || 0 == comp) result = mid; hi = mid; }
/*
printf ("bsi hi:%d lo:%d mid:%d comp:%d res:%d\n", hi, lo, mid, comp, result);
*/
  }
  return(result);
} /* Program: bsi */


SUBROUTINE
int
bse( key, table, tabsize, itemsize, cmpfn)   /* bse: find last */
     char *key, *table;
     int tabsize, itemsize;
     int (*cmpfn)();
{
  REG int comp, mid, lo=0, hi=tabsize; 
  REG int result=(inequality)?-1:tabsize;

  if (Range_Setting.flag) { lo=Range_Setting.lo; hi=Range_Setting.hi; 
			  result=(inequality)?lo-1:hi;}

  while (lo<hi) {
    mid = lo+(hi-lo)/2;
    comp = (*cmpfn)( key, table+(itemsize*mid));
    if (0 > comp) hi = mid;
    else { if (inequality || 0 == comp) result = mid; lo = mid+1; }
/*
printf ("bse hi:%d lo:%d mid:%d comp:%d res:%d\n", hi, lo, mid, comp, result);
*/
  }
  return(result);
} /* Program: bse */



static int CoResult;
SUBROUTINE
int
bsr( key, table, tabsize, itemsize, cmpfn)   /* bsr: find range */
     char *key, *table;
     int tabsize, itemsize;
     int (*cmpfn)();
{
  REG int comp, mid, lo=0, hi=tabsize;
  REG int lo1, hi1, result=tabsize;

  CoResult = result-1;
  while (lo<hi) {
    mid = lo+(hi-lo)/2;
    comp = (*cmpfn)( key, table+(itemsize*mid));
/*
printf ("lo:%d mid:%d hi:%d comp:%d  res:%d\n", lo, mid, hi, comp, result);
*/
    if (0 > comp) hi = mid;
    else if (0 < comp) {
      lo = mid+1;
      if (inequality) result=mid;
    }
    else {
      result = CoResult = mid;
      lo1 = lo; hi1 = hi;
      while (lo<hi) {
	mid = lo+(hi-lo)/2;
	comp = (*cmpfn)( key, table+(itemsize*mid));
	if (0 < comp) lo = mid+1;
	else { if (inequality || 0 == comp) result = mid; hi=mid; }
      }
      lo = lo1; hi = hi1;
      while (lo<hi) {
	mid = lo+(hi-lo)/2;
	comp = (*cmpfn)( key, table+(itemsize*mid));
	if (0 > comp) hi = mid;
	else { if (0 == comp) CoResult = mid; lo = mid+1; }
      }
    }
  }

  CoResult -= result-1;
  return(result);
} /* Program: bsr */



static I *Indexes;
static int IdxCount;
SUBROUTINE
int
bsp( key, table, tabsize, itemsize, cmpfn)  /* bsp: with permutaion vector */
     char *key, *table;
     int tabsize, itemsize;
     int (*cmpfn)();
{
  REG int comp, mid, lo=0, hi=IdxCount;
  REG int result=IdxCount;

  if (Range_Setting.flag) { lo=Range_Setting.lo; hi=result=Range_Setting.hi; }

  while (lo<hi) {
    mid = lo+(hi-lo)/2;
    if (0 > Indexes[mid] || Indexes[mid] >= tabsize) ERROUT(ERR_INDEX); 
    comp = (*cmpfn)( key, table+(itemsize*Indexes[mid]));
    /*printf ("hi:%d lo:%d mid:%d comp:%d\n", hi, lo, mid, comp);*/
    if (0 < comp) lo = mid+1;
    else { if (inequality || 0 == comp) result = mid; hi = mid; }
  }
  return(result);
} /* Program: bsp */

SUBROUTINE
int
bspe( key, table, tabsize, itemsize, cmpfn)  /* bspe: last, w permute vec */
     char *key, *table;
     int tabsize, itemsize;
     int (*cmpfn)();
{
  REG int comp, mid, lo=0, hi=IdxCount;
  REG int result=(inequality)?-1:IdxCount;

  if (Range_Setting.flag) { lo=Range_Setting.lo; hi=Range_Setting.hi; 
			  result=(inequality)?lo-1:hi;}

  while (lo<hi) {
    mid = lo+(hi-lo)/2;
    if (0 > Indexes[mid] || Indexes[mid] >= tabsize) ERROUT(ERR_INDEX); 
    comp = (*cmpfn)( key, table+(itemsize*Indexes[mid]));
    /*printf ("hi:%d lo:%d mid:%d comp:%d\n", hi, lo, mid, comp);*/
    if (0 > comp) hi = mid;
    else { if (inequality || 0 == comp) result = mid; lo = mid+1; }
  }
  return(result);
} /* Program: bspe */

/* bspr - binary search with permutation and ranging */
SUBROUTINE
int
bspr( key, table, tabsize, itemsize, cmpfn)
     char *key, *table;
     int tabsize, itemsize;
     int (*cmpfn)();
{
  REG int comp, mid, lo=0, hi=IdxCount;
  REG int lo1, hi1, result=IdxCount;

  CoResult = result-1;
  while (lo<hi) {
    mid = lo+(hi-lo)/2;
    comp = (*cmpfn)( key, table+(itemsize*Indexes[mid]));
    /*printf ("hi:%d lo:%d mid:%d comp:%d\n", hi, lo, mid, comp);*/
    if (0 > comp) hi = mid;
    else if (0 < comp) lo = mid+1;
    else {
      result = CoResult = mid;
      lo1 = lo; hi1 = hi;
      while (lo<hi) {
	mid = lo+(hi-lo)/2;
	if (0 > Indexes[mid] || Indexes[mid] >= tabsize) ERROUT(ERR_INDEX); 
	comp = (*cmpfn)( key, table+(itemsize*Indexes[mid]));
	/*printf ("hi:%d lo:%d mid:%d comp:%d\n", hi, lo, mid, comp);*/
	if (0 < comp) lo = mid+1;
	else { if (inequality || 0 == comp) result = mid; hi=mid; }
      }
      lo = lo1; hi = hi1;
      while (lo<hi) {
	mid = lo+(hi-lo)/2;
	if (0 > Indexes[mid] || Indexes[mid] >= tabsize) ERROUT(ERR_INDEX); 
	comp = (*cmpfn)( key, table+(itemsize*Indexes[mid]));
	/*printf ("hi:%d lo:%d mid:%d comp:%d\n", hi, lo, mid, comp);*/
	if (0 > comp) hi = mid;
	else { if (inequality || 0 == comp) CoResult = mid; lo = mid+1; }
      }
    }
  }
  CoResult -= result-1;
  return(result);
} /* Program: bspr */

/**/
/************************************************************************
 *
 * Cover routines
 *
 */

static int Itemsize;

/* Binary Search Types -- these map one to one with Binary search routines */
#define BIN_I 1       /* return first occurance */
#define BIN_E 2       /* return last occurance */
#define BIN_R 3       /* return range: (first occurance, #occurances) */


SUBROUTINE
int
bin_validate( source, target, presult, pntarget, pnsource)
     A source, target, *presult;
     I *pnsource;
     int *pntarget;
{
  REG int res, i, ti;

  /* Validation */
  if ( !QA(source) || !QA(target) ) return(ERR_NONCE);
  /* if (0 == source->d[0]) return(gi(0)); */
  if (source->t >= Et) return(ERR_NONCE);
  if (source->t != target->t) return(ERR_TYPE);
  if (source->r > 1+target->r) return(ERR_RANK);
  
  Itemcount = 1;
  i=source->r;
  ti=target->r;
  while(i>1) {
    if( source->d[--i] != target->d[--ti]) return(ERR_LENGTH);
    Itemcount *= source->d[i];
  } 
  Itemsize = Itemcount * SIZEOFITEM(source->t);

  for(res=1, i=0; i<ti; ++i) res *= target->d[i];
  if (NULL != presult) {
    if (bothends) {
      (*presult) = ga( It, ti+1, res*2, target->d-1);
      (*presult)->d[0] = 2;
    }
    else (*presult) = ga( It, ti, res, target->d);
  }
  if (NULL != pntarget) (*pntarget) = res;
  if (NULL != pnsource) (*pnsource) = indexvector?IdxCount:source->d[0];
  return(0);

} /* Program: bin_validate */



SUBROUTINE
A
a_binary( source, target, bsfn)
     A source, target;
     int (*bsfn)();
{
  REG int i;
  REG int ti;
  int res;
  int itemsize;
  A result;

  RANGE_SEARCH_UNSET;
  /* Validation */
  /* if (0 == source->d[0]) return(gi(0)); */
  if (source->t >= Et) ERROUT(ERR_NONCE);
  if (source->t != target->t) ERROUT(ERR_TYPE);
  if (source->r > 1+target->r) ERROUT(ERR_RANK);
  
  Itemcount = 1;
  i=source->r;
  ti=target->r;
  while(i>1) {
    if( source->d[--i] != target->d[--ti]) ERROUT(ERR_LENGTH);
    Itemcount *= source->d[i];
  } 
  itemsize = Itemcount * SIZEOFITEM(source->t);

  for(res=1, i=0; i<ti; ++i) res *= target->d[i];
  if (bothends) {
    result = ga( It, ti+1, res*2, target->d-1);
    result->d[0] = 2;
  }
  else result = ga( It, ti, res, target->d);
  if (0 == source->d[0]) {
    bzero(result->p, result->n*sizeof(I));
    return(result);
  }

  /* search */
  for (i=0; i<res; ++i) {
    switch (source->t) {
    case Ct:
      result->p[i] = (*bsfn)((char *)target->p+(i*itemsize), 
			     (char *) source->p, source->d[0], itemsize, 
			     bs_Ccmp);
      break;
    case It:
      result->p[i] = (*bsfn)((char *)target->p+(i*itemsize), 
			     (char *) source->p, source->d[0], itemsize, 
			     bs_Icmp);
      break;
    case Ft:
      result->p[i] = (*bsfn)((char *)target->p+(i*itemsize), 
			     (char *) source->p, source->d[0], itemsize, 
			     bs_FcmpCT);
      break;
    }
    if (bothends) result->p[res+i] = CoResult;
  }

  return(result);
} /* Program: a_binary */


SUBROUTINE
int
range_search(source, target, nsource, ntarget, loes, hies, flag)
     A source, target;
     I nsource;
     int ntarget, flag;
     I *loes, *hies;
{
  REG int i, rc;
  int nexpected;
  I nsexpected;
  int (*bsfni)(), (*bsfne)();

  if (flag) {
    rc=bin_validate(source, target, NULL, &nexpected, &nsexpected);
    if (rc) return(rc);
    if (nexpected != ntarget) return(ERR_MISMATCH);
    if (nsexpected != nsource) return(ERR_MISMATCH);
  }

  /* search */
  if (indexvector) { bsfni=bsp; bsfne=bspe; } 
  else { bsfni=bsi; bsfne=bse; }

  for (i=0; i<ntarget; ++i) {
    if (loes[i]==-1) continue;
    RANGE_SEARCH_SET(loes[i], hies[i]);
    switch (source->t) {
    case Ct:
      loes[i] = (*bsfni)((char *)target->p+(i*Itemsize), 
			 (char *) source->p, source->d[0], Itemsize, 
			 bs_Ccmp);
      hies[i] = 1+(*bsfne)((char *)target->p+(i*Itemsize), 
			   (char *) source->p, source->d[0], Itemsize, 
			   bs_Ccmp);
      break;
    case It:
      loes[i] = (*bsfni)((char *)target->p+(i*Itemsize), 
			 (char *) source->p, source->d[0], Itemsize, 
			 bs_Icmp);
      hies[i] = 1+(*bsfne)((char *)target->p+(i*Itemsize), 
			   (char *) source->p, source->d[0], Itemsize, 
			   bs_Icmp);
      break;
    case Ft:
      loes[i] = (*bsfni)((char *)target->p+(i*Itemsize), 
			 (char *) source->p, source->d[0], Itemsize, 
			 bs_FcmpCT);
      hies[i] = 1+(*bsfne)((char *)target->p+(i*Itemsize), 
			   (char *) source->p, source->d[0], Itemsize, 
			   bs_FcmpCT);
      break;
    }
    if ((!inequality) && loes[i] == Range_Setting.hi) loes[i]=hies[i]=-1;

/*    printf("range loop:%d loes:%d hies:%d \n", i, loes[i], hies[i]); */
  }
  return(0);

} /* Program: range_search */


SUBROUTINE
A
nest_binary( source, target, btype)
     A source, target;
     int btype;
{
  I *loes, *hies;
  int ntarget;
  I nsource;
  A result=NULL;
  REG int i, rc;

  /* Top-level validation */
  if (source->t != Et || target->t != Et) ERROUT(ERR_TYPE);
  if ( 1 < source->r || 1 < target->r ) ERROUT(ERR_RANK);
  if (source->n < target->n) ERROUT(ERR_MISMATCH);

  rc=bin_validate( (A)*source->p, (A)*target->p, &result, &ntarget, &nsource);
  if (rc) ERROUT(rc);

  loes=ma(ntarget);
  hies=ma(ntarget);
  bzero(loes, ntarget*sizeof(I));
  for (i=0; i<ntarget; ++i) hies[i]=nsource;
  
  for(i=0; i<target->n && !rc; ++i) {
    rc = range_search( (A)source->p[i], (A)target->p[i], nsource, ntarget, 
		  loes, hies, (i)?1:0);
  /*  printf("check:loop:%d   lo:%d  hi:%d \n",i,loes[0], hies[0]); */
  }

  if (rc) {
    mf(loes);
    mf(hies);
    dc(result);
    ERROUT(rc);
  }
  
  switch (btype) {
  case BIN_I:
    for (i=0; i<ntarget; ++i) 
      result->p[i]=(inequality||loes[i]<hies[i])?loes[i]:nsource;
    break;
  case BIN_E:
    for (i=0; i<ntarget; ++i) 
      result->p[i]=(inequality||loes[i]<hies[i])?hies[i]-1:nsource;
    break;
  case BIN_R:
    for (i=0;i<ntarget; ++i) {
      result->p[i]=(inequality||loes[i]<hies[i])?loes[i]:nsource;
      result->p[ntarget+i] = (loes[i]<hies[i])?hies[i]-loes[i]:0;
    }
    break;
  }
  
  mf(loes);
  mf(hies);
  return(result);

} /* Program: nest_binary */


/**/
/************************************************************************
 *
 * Entrypoints for A 
 *
 */

ENTRYPOINT
A
ep_bu( source, target)
     A source, target;
{
  indexvector=FALSE;
  inequality=FALSE;
  bothends=FALSE;
  return( a_binary( source, target, bsu));
}


ENTRYPOINT
A
ep_bi( source, target)
     A source, target;
{
  indexvector=FALSE;
  inequality=FALSE;
  bothends=FALSE;
  if (Et==target->t) return(nest_binary(source, target, BIN_I));
  else return( a_binary( source, target, bsi));
}

ENTRYPOINT
A
ep_bge( source, target)
     A source, target;
{
  indexvector=FALSE;
  inequality=TRUE;
  bothends=FALSE;
  if (Et==target->t) return(nest_binary(source, target, BIN_I));
  else return( a_binary( source, target, bsi));
}


ENTRYPOINT
A
ep_be( source, target)
     A source, target;
{
  indexvector=FALSE;
  inequality=FALSE;
  bothends=FALSE;
  if (Et==target->t) return(nest_binary(source, target, BIN_E));
  else return( a_binary( source, target, bse));
}

ENTRYPOINT
A
ep_ble( source, target)
     A source, target;
{
  indexvector=FALSE;
  inequality=TRUE;
  bothends=FALSE;
  if (Et==target->t) return(nest_binary(source, target, BIN_E));
  else return( a_binary( source, target, bse));
}


ENTRYPOINT
A
ep_br( source, target)
     A source, target;
{
  indexvector=FALSE;
  inequality=FALSE;
  bothends=TRUE;
  if (Et==target->t) return( nest_binary( source, target, BIN_R));
  else return( a_binary( source, target, bsr));
}

ENTRYPOINT
A
ep_bp( source, ind, target)
     A source, ind, target;
{
  if (1 != ind->r) ERROUT(ERR_RANK);
  IdxCount = ind->n;
  Indexes = ind->p;
  indexvector=TRUE;
  inequality=FALSE;
  bothends=FALSE;
  if (Et==target->t) return(nest_binary(source, target, BIN_I));
  else return( a_binary( source, target, bsp));
}

ENTRYPOINT
A
ep_bpe( source, ind, target)
     A source, ind, target;
{
  if (1 != ind->r) ERROUT(ERR_RANK);
  IdxCount = ind->n;
  Indexes = ind->p;
  indexvector=TRUE;
  inequality=FALSE;
  bothends=FALSE;
  if (Et==target->t) return(nest_binary(source, target, BIN_E));
  else return( a_binary( source, target, bspe));
}

ENTRYPOINT
A
ep_bpr( source, ind, target)
     A source, ind, target;
{
  if (1 != ind->r) ERROUT(ERR_RANK);
  IdxCount = ind->n;
  Indexes = ind->p;
  indexvector=TRUE;
  inequality=FALSE;
  bothends=TRUE;
  if (Et==target->t) return(nest_binary(source, target, BIN_R));
  else return( a_binary( source, target, bspr));
}


ENTRYPOINT
A
ep_bpge( source, ind, target)
     A source, ind, target;
{
  if (1 != ind->r) ERROUT(ERR_RANK);
  IdxCount = ind->n;
  Indexes = ind->p;
  indexvector=TRUE;
  inequality=TRUE;
  bothends=FALSE;
  if (Et==target->t) return(nest_binary(source, target, BIN_I));
  else return( a_binary( source, target, bsp));
}


ENTRYPOINT
A
ep_bple( source, ind, target)
     A source, ind, target;
{
  if (1 != ind->r) ERROUT(ERR_RANK);
  IdxCount = ind->n;
  Indexes = ind->p;
  indexvector=TRUE;
  inequality=TRUE;
  bothends=FALSE;
  if (Et==target->t) return(nest_binary(source, target, BIN_E));
  else return( a_binary( source, target, bspe));
}


void binaryInstall()
{
  CX saveCx=Cx;
  Cx=cx("b");

  install((PFI)ep_bu,   "u",   A_,2,A_,A_, 0,0,0,0,0,0);
  install((PFI)ep_bi,   "i",   A_,2,A_,A_, 0,0,0,0,0,0);
  install((PFI)ep_be,   "e",   A_,2,A_,A_, 0,0,0,0,0,0);
  install((PFI)ep_br,   "r",   A_,2,A_,A_, 0,0,0,0,0,0);
  install((PFI)ep_bp,   "p",   A_,3,A_,IA,A_,0,0,0,0,0);
  install((PFI)ep_bpr,  "pr",  A_,3,A_,IA,A_,0,0,0,0,0);
  install((PFI)ep_bge,  "ge",  A_,2,A_,A_, 0,0,0,0,0,0);
  install((PFI)ep_ble,  "le",  A_,2,A_,A_, 0,0,0,0,0,0);
  install((PFI)ep_bpge, "pge", A_,3,A_,IA,A_,0,0,0,0,0);
  install((PFI)ep_bple, "ple", A_,3,A_,IA,A_,0,0,0,0,0);
  install((PFI)ep_bpe,  "pe",  A_,3,A_,IA,A_,0,0,0,0,0);
  
  Cx=saveCx;
  return;
}
