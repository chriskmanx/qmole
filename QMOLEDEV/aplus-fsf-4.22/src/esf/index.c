/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>
#include <a/arthur.h>

Z I e;

#define G00(T,f) Z void f(r,a,w,na,n)T *r,*w,*na;I *a,n;
#define CI(T,f) G00(T,f)DO(n,if((unsigned long)(n=*a++)>=e)*r++=*na;else *r++=w[n];)

CI(I,jj0)CI(F,jj1)CI(C,jj2) 

Z void (*indf[])()={jj0,jj1,jj2};

ENTRYPOINT
A ep_from(aidx,asrc,ana)A aidx,asrc,ana;
{
  A z,na=0;I i,ic=1,idx=0,is;C *cp,*zcp;I zn=1,d[MAXR];NDC2(asrc,ana);
  if(asrc->t!=ana->t||Et==ana->t&&asrc->n&&sym(ana)!=sym(asrc))
    ERROUT(ERR_TYPE);
  if(0==asrc->r||0!=ana->r&&ana->r!=asrc->r-1)ERROUT(ERR_RANK);
  if(MAXR<aidx->r+asrc->r-1)ERROUT(ERR_MAXRANK);
  for(i=0;i<aidx->r;++i)zn*=d[idx++]=aidx->d[i];
  for(i=1;i<asrc->r;++i){
    zn*=d[idx++]=asrc->d[i];ic*=asrc->d[i];
    if(ana->r&&ana->d[i-1]!=asrc->d[i])ERROUT(ERR_LENGTH);
  }
  if(0==ana->r&&1<asrc->r){
    na=ga(asrc->t,asrc->r-1,ic,asrc->d+1);cp=(C *)na->p;
    for(i=0;i<ic;++i)tmv(asrc->t,(I *)(cp+i*SIZEOFITEM(asrc->t)),ana->p,1);
  }

  z=ga(asrc->t,aidx->r+asrc->r-1,zn,d);
  if(1==asrc->r&&Et>asrc->t) {
    e=asrc->n;(*indf[asrc->t])(z->p,aidx->p,asrc->p,ana->p,z->n);
  } else {
    is=ic*SIZEOFITEM(asrc->t);cp=(C *)asrc->p;zcp=(C *)z->p;
    for(i=0;i<aidx->n;++i){
      idx=aidx->p[i];
      tmv(asrc->t,(I *)(zcp+i*is),
	  (I *)((0<=idx&&idx<asrc->d[0])?cp+idx*is:(C *)((na?na:ana)->p)),ic);
    }
  }
  if(Ct==z->t)((C *)(z->p))[z->n]='\0';if(na)dc(na);R z;
}

ENTRYPOINT
A ep_nanfind(aobj)A aobj;
{
  A z;I count=0;F *fp;
  if (!QA(aobj)||Ft!=aobj->t) R gv(It,0);
  fp=(F *)aobj->p;
  DO(aobj->n,if(isnan(fp[i]))count++);
  z=gv(It,count);
  count=0;
  DO(aobj->n,if(isnan(fp[i]))z->p[count++]=i);
  R z;
}

ENTRYPOINT
I ep_index_of(a,w)A a, w;
{
  A ta,tw;
  I items,i;
  NDC2(a,w);
  /* a and w are both vectors with at least 2 elements */
  Q(a->t!=Et||w->t!=Et||a->r!=1||w->r!=1||a->n<2||w->n<2,ERR_DOMAIN);
  Q(a->n!=w->n,ERR_LENGTH);
  
  /* outer object type checks */
  for(i=0; i<a->n; i++)
    {
      Q(!(a->p[i]),ERR_DOMAIN);
      Q(!QA(a->p[i]),ERR_DOMAIN);
      Q(!(w->p[i]),ERR_DOMAIN);
      Q(!QA(w->p[i]),ERR_DOMAIN);
      Q(( ((A)a->p[i])->t != ((A)w->p[i])->t ),ERR_DOMAIN);
    }
 /* check inner objects */
  ta=(A)a->p[0];
  Q(!(ta),ERR_DOMAIN);
  Q(!QA(ta),ERR_DOMAIN);
  items=ta->r?ta->d[0]:ta->n;
  for(i=0;i<a->n;++i) 
    {
      ta=(A)a->p[i];
      Q(!(ta),ERR_DOMAIN);
      Q(!QA(ta),ERR_DOMAIN);
      Q(ta->r>1,ERR_DOMAIN);    /* Scalar or Vector */
      Q((items!=(ta->r?ta->d[0]:ta->n)),ERR_DOMAIN);      
      if(ta->t==Et && ta->n && ta->p[0] && QA(ta->p[0]))
	{
	  I i;
	  I aType=((A)ta->p[0])->t;
	  for(i=1; i<ta->n; i++)
	    {
	      Q(!(ta->p[i]),ERR_DOMAIN);
	      Q(!QA(ta->p[i]),ERR_DOMAIN);
	      Q(aType!=((A)ta->p[i])->t,ERR_DOMAIN);
	    }
	}
    }

  tw=(A)w->p[0];
  Q(!(tw),ERR_DOMAIN);
  Q(!QA(tw),ERR_DOMAIN);
  items=tw->r?tw->d[0]:tw->n;
  for(i=0;i<w->n;++i) 
    {
      tw=(A)w->p[i];
      Q(!(tw),ERR_DOMAIN);
      Q(!QA(tw),ERR_DOMAIN);
      Q(tw->r>1,ERR_DOMAIN);    /* Scalar or Vector */
      Q((items!=(tw->r?tw->d[0]:tw->n)),ERR_DOMAIN);      
      if(tw->t==Et && tw->n && tw->p[0] && QA(tw->p[0]))
	{
	  I i;
	  I aType=((A)tw->p[0])->t;
	  for(i=1; i<tw->n; i++)
	    {
	      Q(!(tw->p[i]),ERR_DOMAIN);
	      Q(!QA(tw->p[i]),ERR_DOMAIN);
	      Q(aType!=((A)tw->p[i])->t,ERR_DOMAIN);
	    }
	}
    }

  /*   index_of() is found in i.c since it needs many of the   */
  /*   same utilities used in dyadic iota */
  R (I) index_of(a,w);
}

void tInstall()
{
  install((PFI)ep_from, "_index", A_, 3, IA, A_, A_,0,0,0,0,0);
  install((PFI)ep_nanfind, "_nanfind", A_, 1, A_,0,0,0,0,0,0,0);
  install((PFI)ep_index_of, "_index_of", A_, 2, A_, A_,0,0,0,0,0,0);
  R;
}
