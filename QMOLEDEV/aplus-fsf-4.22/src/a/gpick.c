/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.
*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <math.h>

#include <dap/balloc.h>

#include <a/f.h>
#include <a/k.h>
#include <a/fncdcls.h>
#include <a/fir.h>
#include <a/arthur.h>

#undef ENTRYPOINT
#define ENTRYPOINT

Z I pcki(I,A);

Z I gpu_fillivec(I *ivec, A aobj)
{
  I i; F f,*fvec=(F *)aobj->p;
  
  for (i=0;i<aobj->n;++i) {
    if (fvec[i]!=(f=rint(fvec[i]))) R 1;
    ivec[i]=(I)f;
  }
  R 0;
}


Z I gpi_num(A apick, A aobj)
{
  I idx, i, ivec[9], *ip;
  if (Et != aobj->t) ERROUT(ERR_DOMAIN);
  if (2 <= apick->r || apick->n != aobj->r) ERROUT(ERR_RANK);
  if (Ft==apick->t) {
    if (gpu_fillivec(ivec,apick)) ERROUT(ERR_TYPE);
    ip=ivec;
  } else ip=apick->p;

  if (aobj->d[0]<=(I)(idx=ip[0])) ERROUT(ERR_INDEX);
  for(i=1;i<apick->n;++i) {
    if (aobj->d[i]<=(I)(ip[i])) ERROUT(ERR_INDEX);
    idx*=aobj->d[i]; idx+=ip[i];
  }
  R idx;
}


Z I gpi_sym(I key, A aobj)
{
  I i, tt;
  A t0, t1;
  if (Et != aobj->t || 2 != aobj->n) {q=ERR_DOMAIN;R -1;}
  t0=(A)aobj->p[0]; t1=(A)aobj->p[1];
  if ((!QA(t0))||(!QA(t1))||Et!=t1->t||t0->n!=t1->n) {q=ERR_DOMAIN;R -1;}
  for (i=0;i<t0->n;++i) {
    tt=t0->p[i];
    if (!QS(tt)) {q=ERR_DOMAIN;R -1;}
    if (key==tt) R i;
  }
  q=ERR_INDEX;R -1;
}


Z A *gpp_scalar(I idx, A *paobj)
{
  A aobj=un(paobj);
/*  H(" check: gpp_scalar idx:%d  aobj->n:%d\n", idx, aobj->n); */
  
  if (1 != aobj->r) ERROUT(ERR_RANK);
  if ((I)idx>=aobj->n) ERROUT(ERR_INDEX);
  if (QS(aobj)) ERROUT(ERR_DOMAIN);
  switch (aobj->t) {
  case It:
  case Ft:
  case Ct:
    ERROUT(ERR_TYPE);
  case Et:
    if(QF(aobj->p[idx])){ERROUT(ERR_TYPE);} R (A *)(aobj->p+idx);
  default:ERROUT(ERR_TYPE);
  }
}

Z A gp_nested(A apick, A aobj)
{
  I idx, i, issymbol=0;
  A atarg, ares=aobj;
  if (2 <= apick->r) ERROUT(ERR_RANK);
  if (0==apick->n) R (A)ic(ares); /* ic_or_copy(ares) */
  for (i=0;(0==q)&&i<apick->n;++i) {
    atarg=(A)apick->p[i];
    if ( (!QA(ares)) || Et != ares->t) ERROUT(ERR_DOMAIN);
    if (QS(atarg)) {
      if (-1==issymbol) ERROUT(ERR_DOMAIN);
      issymbol=1;
      idx=gpi_sym((I)atarg, ares);
      if (!q) ares=(A)ares->p[1];
    } else {
      if (1==issymbol) ERROUT(ERR_DOMAIN);
      issymbol=-1;
      if (It==atarg->t || Ft==atarg->t) {
	idx=gpi_num(atarg, ares); 
      }
      else if (Et==atarg->t) {
	if (0 == atarg->n && 1 == ares->n) { ares=(A)*ares->p; continue;} 
	if (QS(atarg) || 1 != atarg->n) ERROUT(ERR_DOMAIN);
	atarg=(A)*atarg->p;
	if (!QS(atarg)) ERROUT(ERR_TYPE);
	idx=gpi_sym((I)atarg, ares);
	if (!q) ares=(A)ares->p[1];
      }
      else ERROUT(ERR_DOMAIN);
    }
    if (q) ERROUT(q);
    ares=(A)ares->p[idx];
  }
  if (!QA(ares)) ERROUT(ERR_DOMAIN);
  R QF(ares)?gc(Et,0,1,0,(I *)&ares):(A)ic(ares); /* ic_or_copy(ares) */
}

Z A gpix_nested(A apick, A aobj)
{
  I idx=0, i;
  A atarg, apix;
  if (0==apick->n) R gv(It,0);
  apix=gv(It,2*apick->n);
  apix->n=apix->d[0]=0;
  for (i=0;i<apick->n;++i) {
    atarg=(A)apick->p[i];
    if (QS(atarg)) {
      idx=gpi_sym((I)atarg, aobj);
      if(q)R 0;
      aobj=(A)aobj->p[1]; apix->p[apix->n++]=1;
    } else {
      if (It==atarg->t || Ft==atarg->t) {
	idx=gpi_num(atarg, aobj); 
      }
      else if (Et==atarg->t) {
	if (0 == atarg->n && 1 == aobj->n) { 
	  aobj=(A)*aobj->p; apix->p[apix->n++]=0;
	  continue;
	} 
	atarg=(A)*atarg->p;
	idx=gpi_sym((I)atarg, aobj);
	if(q)R 0;
	aobj=(A)aobj->p[1]; apix->p[apix->n++]=1;
      }
    }
    aobj=(A)aobj->p[idx]; apix->p[apix->n++]=idx;
  }
  apix->d[0]=apix->n;
  R apix;
}

Z A *gpp_nested(A apick, A *paobj)
{
  I idx, i, issymbol=0;
  A atarg, *pares=paobj, ares;
  if (2 <= apick->r) ERROUT(ERR_RANK);
  if (0==apick->n) R pares;
  for (i=0;(0==q)&&i<apick->n;++i) {
    ares=un(pares);
    atarg=(A)apick->p[i];
    if ( (!QA(ares)) || Et != ares->t) ERROUT(ERR_DOMAIN);
    if (QS(atarg)) {
      if (-1==issymbol) ERROUT(ERR_DOMAIN);
      issymbol=1;
      idx=gpi_sym((I)atarg, ares);
      if (!q) pares=(A*)(ares->p+1);
    } else {
      if (1==issymbol) ERROUT(ERR_DOMAIN);
      issymbol=-1;
      if (It==atarg->t || Ft==atarg->t) {
	idx=gpi_num(atarg, ares); 
      }
      else if (Et==atarg->t) {
	if (0 == atarg->n && 1 == ares->n) 
	  { pares=(A*)(ares->p); continue;} 
	if (QS(atarg) || 1 != atarg->n) ERROUT(ERR_DOMAIN);
	atarg=(A)*atarg->p;
	if (!QS(atarg)) ERROUT(ERR_TYPE);
	idx=gpi_sym((I)atarg, ares);
	if (!q) pares=(A*)(ares->p+1);
      }
      else ERROUT(ERR_DOMAIN);
    }
    if (q) ERROUT(q);
    pares=(A*)((un(pares))->p+idx);
  }
  if (!QA(*pares)) ERROUT(ERR_DOMAIN);
  R pares;
}


Z A gp_num(A apick, A aobj)
{
  I i, *ivec=(I*)0, *ip;
  A ares=aobj;
  if (2 <= apick->r) ERROUT(ERR_RANK);
  if (0 == apick->n) R (A)ic(ares); /* ic_or_copy(ares) */
  if (Ft==apick->t) {
    ivec=(I *)balloc(apick->n * sizeof(I));
    if (gpu_fillivec(ivec,apick)) { bfree((C *)ivec); ERROUT(ERR_TYPE); }
    ip=ivec;
  } else ip=apick->p;
  for (i=0;(0==q)&&i<apick->n;++i) {
    if ( (!QA(ares)) || Et != ares->t) {bfree((C *)ivec);ERROUT(ERR_DOMAIN);}
    if ( 2 <= ares->r) {bfree((C *)ivec);ERROUT(ERR_RANK);}
    if ((I)(ip[i])>=ares->n) {bfree((C *)ivec);ERROUT(ERR_INDEX);}
    ares=(A)ares->p[ip[i]];
  }
  bfree((C *)ivec);
  if (!QA(ares)) ERROUT(ERR_DOMAIN);
  R QF(ares)?gc(Et,0,1,0,(I *)&ares):(A)ic(ares); /* ic_or_copy(ares) */
}

Z A *gpp_num(A apick, A *paobj)
{
  I i, *ivec=(I*)0, *ip;
  A *pares=paobj, aobj;
  if (2 <= apick->r) ERROUT(ERR_RANK);
  if (0 == apick->n) R pares;
  if (Ft==apick->t) {
    ivec=(I *)balloc(apick->n * sizeof(I));
    if (gpu_fillivec(ivec,apick)) { bfree((C *)ivec); ERROUT(ERR_TYPE);}
    ip=ivec;
  } else ip=apick->p;
  for (i=0;(0==q)&&i<apick->n;++i) {
    if(*pares&&QA(*pares))aobj=un(pares);
    else{bfree((C *)ivec);ERROUT(ERR_DOMAIN);}
    if ( (!QA(aobj)) || Et != aobj->t) {bfree((C *)ivec); ERROUT(ERR_DOMAIN);}
    if ( 2 <= aobj->r) {bfree((C *)ivec); ERROUT(ERR_RANK);}
    if ((I)(ip[i])>=aobj->n) {bfree((C *)ivec);ERROUT(ERR_INDEX);}
    pares=(A *)(aobj->p+ip[i]);
  }
  bfree((C *)ivec);
  if (!QA(*pares)) ERROUT(ERR_DOMAIN);
  R pares;
}


ENTRYPOINT
A ep_gp(A apick, A aobj)
{
  I idx;
  A z;
/*  H("check: ep_gp\n"); */
  switch(apick->t) {
  case It: case Ft: 
    if (1==apick->n) {
      if (1!=aobj->r) ERROUT(ERR_RANK);
      if (It==apick->t) z=(A)pcki(*apick->p, aobj);
      else if (gpu_fillivec(&idx,apick)) ERROUT(ERR_TYPE)
      else z=(A)pcki(idx, aobj);
    }
    else z=gp_num(apick, aobj); break;
  case Et: z=gp_nested(apick, aobj); break;
  default: ERROUT(ERR_TYPE);
  }
  R z;
}

ENTRYPOINT
A *ep_gpp(A apick, A *paobj)
{
  I idx;
/*  H("check: ep_gpp\n"); */
  switch(apick->t) {
  case It: case Ft: 
    if (1==apick->n) {
      if (It==apick->t) R gpp_scalar(*apick->p, paobj);
      if (gpu_fillivec(&idx,apick)) ERROUT(ERR_TYPE);
      R gpp_scalar(idx, paobj);
    }
    else R gpp_num(apick, paobj);
  case Et: R gpp_nested(apick, paobj);
  default: ERROUT(ERR_TYPE);
  }
}


ENTRYPOINT
A gpix(A apick, A aobj)
{
  A apix;
/*  H("check: ep_gp\n"); */
  switch(apick->t) {
  case It: R (A)ic(apick);
  case Ft:
    apix=gv(It,apick->n);
    if (gpu_fillivec(apix->p,apick)) {dc(apix); R (A)0;}
    R apix;
  case Et: R gpix_nested(apick, aobj);
  default: R (A)0;
  }
}

/* pcki checks for index error, pck does not */
Z I pcki(I i,A a){I z;I t=a->t;Q(!a->r,7)Q((I)i>=a->d[0],10);
 R t==Et&&(z=a->p[i],!QF(z))?ic((A)z):
   (I)gc(t,0,1,0,(I*)((C*)(a->p)+Tt(t,i)));}
I pck(I i,A a){I z;I t=a->t;
 R t==Et&&(z=a->p[i],!QF(z))?ic((A)z):
   (I)gc(t,0,1,0,(I *)((C*)(a->p)+Tt(t,i)));}

H2(pic){ND2 
	  if(w->r) 
	    R (I)ep_gp(a,w);
	  else
	    {			/* scalar case */
	      I r;
	      A ww=gc(w->t,w->r,w->n,w->d,w->p); /* Make a copy  */
	      ww->d[0]=ww->r=1;	/* Change to a 1 element vector */
	      r=(I)ep_gp(a,ww);
	      dc(ww);
	      R r;
	    }    
}

I pka(A p,A *v){Q(!QA(p)||!QA(*v)||Et<(p)->t||Et<(*v)->t,18)R (I)ep_gpp(p,v);}


