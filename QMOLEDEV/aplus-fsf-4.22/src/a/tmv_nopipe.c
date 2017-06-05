/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/k.h>
#include <a/fncdcls.h>

#ifdef HPC_PWR2_TMV
C *tmv(I t,I *d,I *s,I n){switch(t){
 CSR(It,d=(I*)hpc_mv_i(d,s,n);R(C*)d;)
 CSR(Et,DO(n,*d++=ic((A)(*s++)))R(C*)d;)
 CSR(Ft,{F *a=(F*)d;F *b=(F*)s;a=(F*)hpc_mv_f(a,b,n);R(C*)a;})
 CSR(Ct,{C *a=(C*)d;C *b=(C*)s;a=(C*)hpc_mv_c(a,b,n);R(C*)a;})} R 0;}
#else
#if defined(_ULTRA_SPARC_NEW_TMV)
C *tmv_nopipe(I t,I *d,I *s,I n)
#else
C *tmv(I t,I *d,I *s,I n)
#endif
{
  switch(t){
  case 0L :
    {
      I i = 0 , _i = ( n ) ;
      for ( ; i < _i ; ++ i ) {
    * d ++ = * s ++ ;
      }
    }
  return ( C * ) d ;
  ;
  break ;
  case 4L :
    {
      I i = 0 , _i = ( n ) ;
      for ( ; i < _i ; ++ i ) {
	if( QS(*s) )
	  * d ++ = * s ++ ;
	else
	  * d ++ = ic ( (A) * s ++ ) ;
      }
    }
  return ( C * ) d ;
  ;
  break ;
  case 1L :
    {
      F * a = ( F * ) d ;
      F * b = ( F * ) s ;
      {
    I i = 0 , _i = ( n ) ;
    for ( ; i < _i ; ++ i ) {
      * a ++ = * b ++ ;
    }
      }
      return ( C * ) a ;
    }; 
  break ;
  case 2L :
    {
      C * a = ( C * ) d ;
      C * b = ( C * ) s ;
      {
    I i = 0 , _i = ( n ) ;
    for ( ; i < _i ; ++ i ) {
      * a ++ = * b ++ ;
    }
      }
      return ( C * ) a ;
    }; 
  break ;
  }
}
#endif
