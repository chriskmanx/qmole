/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <stdio.h>
#include <a/ik.h>
#include <a/s.h>
#include <a/fncdcls.h>
#include <a/arthur.h>


extern I MY[];

static C *ks_buf[256];

A getRawKstack(void)
{
  I n=K-MY;
  A z=gv(It,n);
  DO(n,z->p[i]=MY[i+1]);
  R z;
}

A getSymKstack(void)
{
  I n=K-MY;I *zp,i,s;
  C **ns=get_primlist(1,1);
  A z=gv(Et,n);zp=z->p;
  i=1;
  while(i<=n)
  {
    s=MY[i++];
    if(0==s){*zp++=(I)aplus_nl;}
    else if(s>-9999&&s<6)
    {
      if(0>s)*zp++=(I)gi(-s);
      else *zp++=(I)gvi(Et,1,MS(si(ns[s])));
    }
    else if(QV(s)) 
    {
      *zp++=(I)gvi(Et,2,gsv(0,"file"),gsv(0,(C *)(s&~aplusMask)));
      *zp++=(I)gi(-MY[i++]);
    } 
    else if(QS(s))
    {
      *zp++=(I)gvi(Et,2,gsv(0,"expr"),gsv(0,(C *)(s&~aplusMask)));
    }
    else
    {
      A f=(A)s;
      sprintf((C *)ks_buf,"%s.%s",((CX)f->p[f->n+2])->s->n,XS(*f->d)->n);
      *zp++=(I)gvi(Et,2,gsv(0,"func"),gsv(0,(C *)ks_buf));
    }
  }
  R z;
}

static A lastSavedKstack=0;
static int recurseKstack=0;

void snapshotKstack()
{
  extern A sikAsAObj() ;
/* Consider changing signal handlers for segv and bus */
 if(recurseKstack==0)
    {
      recurseKstack=1;
      if( lastSavedKstack && QA(lastSavedKstack) )
	{
	  dc(lastSavedKstack);
	  lastSavedKstack=0;
	}
      lastSavedKstack=(A)sikAsAObj();
      recurseKstack=0;
    }
}

A showLastSavedKstack()
{ 
  if(recurseKstack==1)
    {
      H("\343 Internal Error _doErrorStack\n");
      return aplus_nl;
    }
  
  if( lastSavedKstack && QA(lastSavedKstack) )
    return (A)ic(lastSavedKstack);
  else
    return aplus_nl;
}
