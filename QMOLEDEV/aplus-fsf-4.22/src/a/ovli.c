/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <limits.h>
#include <a/k.h>

#if !defined(USE_ASSEMBLER_OVLI)
extern I aw;

static F longMinAsDouble=LONG_MIN;
static F longMaxAsDouble=LONG_MAX;

I cOvliAdd(I *r, I *a, I *w, I n) {
  I *t = r + n;
  I  i=aw!=1, j=aw!=2;
      
  for (; r<t; a+=i, w+=j, r++) {
    *r = *a + *w;
    if( (*a>0 && *w>0 && *r<=0) || (*a<0 && *w<0 && *r>=0) )
      return q=-2, 1L;
  }
  return 0;
}
      
I cOvliTimes(I *r, I *a, I *w, I n) {
  I *t=r+n;
  I  i=aw!=1, j=aw!=2;
  F rd;

  for (; r<t; a+=i, w+=j, r++) {
    *r  = *a * *w;
    rd = (F)*a * (F)*w;
    if(rd>longMaxAsDouble || rd<longMinAsDouble)
      return q=-2, 1L;
  }
  return 0;
}
    
I cOvliSubtract(I *r, I *a, I *w, I n){
  I *t=r+n;
  I  i=aw!=1, j=aw!=2;

  for (; r<t; a+=i, w+=j, r++) {
    *r = *a - *w;
    if( (*a<0 && *w>0 && *r>0) || (*a>=0 && *w<0 && *r<0) )
      return q=-2, 1L;
  }
  return 0;
}

I cOvliPlusReduce(I *r, I *w, I n){
  I *t=w+n, a=0;
  
  *r=0;
  for (; w<t; a=*r, ++w) {
    *r = a + *w;
    if( (a>0 && *w>0 && *r<=0) || (a<0 && *w<0 && *r>=0) )
      return q=-2, 1L;	
  }
  return 0;
}

#if defined(_ULTRA_OvliTimesReduce_BUG)
static char junk[128];
#endif

I cOvliTimesReduce(I *r, I *w, I n){
  I s=1, *t=w+n;
  F rd=1.0;

/* Note times reduce 65536 65536 will result in *s==0 so can */
/* only check *w==0 for an early exit */

  for (; w<t; ++w) {
    if( 0 == *w ){ s=0; break; } /* done if *w==0 */
    s *= *w;
    rd *= (F)*w;
#if defined(_ULTRA_OvliTimesReduce_BUG)
    sprintf(junk,"%f",rd);
#endif
    if(rd>longMaxAsDouble || rd<longMinAsDouble)
      return q=-2, 1L;
  }
  *r = s;
  return 0;
}
#endif

void ovliInstall()
{
    extern PFI df[][2];
    extern PFI fr[][2];

#if defined(USE_ASSEMBLER_OVLI)
/* #if defined(__sgi) || defined(sparc) || defined(_IBMR2) */
    /* install Assembler overflow detecting primitives */
    extern ovliAdd(),ovliTimes(),ovliSubtract();
    extern ovliPlusReduce(),ovliTimesReduce();

    *df[2]=ovliAdd,*df[3]=ovliTimes,*df[6]=ovliSubtract,
    *fr[2]=ovliPlusReduce,*fr[3]=ovliTimesReduce;
#else
    /* install C overflow detecting primitives */
    *df[2]=cOvliAdd,*df[3]=cOvliTimes,*df[6]=cOvliSubtract,
    *fr[2]=cOvliPlusReduce,*fr[3]=cOvliTimesReduce;
#endif
	return;
}







