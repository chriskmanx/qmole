/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/development.h>
#include <stdio.h>
#include <string.h>

#include <dap/dap.h>

#include <a/f.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>
#include <a/arthur.h>

#undef ENTRYPOINT
#define ENTRYPOINT static

SUBROUTINE
I
flat_onepass( a, ptype) A a; I *ptype;
{
  I i, res;
  if (!QA(a)) ERROUT(ERR_DOMAIN);
  switch(a->t) {
  case Ct:
  case It:
  case Ft:
    if (-1==*ptype) *ptype=a->t;
    if (a->t != *ptype) ERROUT(ERR_DOMAIN);
    R a->n;
  case Et:
    for (i=res=0;(!q)&&i<a->n;++i)
      if (QA(a->p[i])) res += flat_onepass((A)a->p[i], ptype);
      else {
	if (-1==*ptype) *ptype=a->t;
	if (a->t != *ptype) ERROUT(ERR_DOMAIN); 
	++res;
      }
    R res;
  default:
    if (-1==*ptype) *ptype=Xt;
    if (Xt != *ptype) ERROUT(ERR_DOMAIN); 
    R 1;
  }
}

SUBROUTINE
C *flat_twopass(a, type, ptr) A a; I type; C *ptr;
{
  I i, n;

  switch(a->t) {
  case Ct:
  case It:
  case Ft:
    n=a->n*SIZEOFITEM(type);
    bcopy(a->p,ptr,n);
    R ptr+n;
  case Et:
    for (i=0;(!q)&&i<a->n;++i)
      if (QA(a->p[i])) ptr = flat_twopass((A)a->p[i], type, ptr);
      else {
	bcopy(a->p+i,ptr,sizeof(I));
	ptr+=sizeof(I);
      }
    R ptr;
  default:
    *(I *)ptr=ic(a);
    R ptr+sizeof(I);
  }
}
 

ENTRYPOINT
A
ep_flat( a) register A a;
{
  A z;
  I n, type=-1;
  n=flat_onepass(a, &type);
  if (q) R 0;
  if (-1==type) type=Et;
  z=gv((Xt==type)?Et:type,n);
  flat_twopass(a,type,(C *)z->p);
  if (Ct==type) ((C *)(z->p))[z->n]='\0';
  R z;
}

Z quasisf(a)A a;{A s,d;if(!(QA(a)&&Et==a->t&&2==a->n))R 0;
s=(A)*a->p;d=(A)a->p[1];
R (!QF(s))&&(!QF(d))&&(qz(s)&&qz(d)||sym(s)&&s->n==d->n&&s->r<2&&d->r<2);}

/* purebox() is and/QA(a), while fsy is and/QF(a) */
Z purebox(a)A a;{if(!QA(a)||Et!=a->t)R 0;DO(a->n,if(!QA(a->p[i]))R 0)R 1;}

Z nodups(a)A a;{
  A iota;
  I i,j,nodup=1,ni=a->n;
  if(50>=a->n) {
    for(i=0;i<a->n-1;++i)for(j=i+1;j<a->n;++j)if(a->p[i]==a->p[j])R 0;
    R 1;
  }
  iota=(A)fnd(a,a);
  for(i=1;nodup&&i<ni;++i)if(iota->p[i]!=i)nodup=0;
  dc(iota);
  R nodup;
}

Z issf(a)A a;{A s,d;if(!(QA(a)&&Et==a->t&&2==a->n))R 0;
s=(A)*a->p;d=(A)a->p[1];
R(!QF(s))&&(!QF(d))&&
  (qz(s)&&qz(d)||sym(s)&&s->n==d->n&&1>=s->r&&1>=d->r&&purebox(d)&&nodups(s));
}

/* this is a variation of issf() which allows duplicates. */
I issfdups(a)A a;{A s,d;if(!(QA(a)&&Et==a->t&&2==a->n))R 0;
s=(A)*a->p;d=(A)a->p[1];
R(!QF(s))&&(!QF(d))&&
  (qz(s)&&qz(d)||sym(s)&&s->n==d->n&&1>=s->r&&1>=d->r&&purebox(d));
}

ep_issf(a) register A a;{R issf(a)?1:0;}

ENTRYPOINT
A
ep_alsf(a) register A a;
{
  A z, t, t1; I ni, i, *ap, *z0p, *z1p;C *cp;
  if (quasisf(a)) { 
    t=(A)a->p[0]; t1=(A)a->p[1];
    z=gv(Et,2);
    z->p[0]=(I)gc(t->t,1,t->n,&t->n,t->p);
    z->p[1]=(I)gv(Et,t->n); z1p=((A)z->p[1])->p;
    /* For _alsf(`a;,1) or _alsf(`a;,`xxx) */
    if (Et==t1->t || (t1->t<Et && t1->n==1 && t1->r==1) ) {
      if ( t1->n==1 && t1->r==1 && (t1->t!=Et || QS(*t1->p)) ){
	for(i=0;i<t->n;++i)
	  z1p[i]=ic(t1);
      } else {
	ap=t1->p;
	for(i=0;i<t->n;++i){
	  if(QA(*ap))z1p[i]=ic((A)*ap++);
	  else{z1p[i]=(I)gs(Et);*((A)z1p[i])->p=*ap++;}
	}
      }
    } else {
      cp=(C *)t1->p; ni=SIZEOFITEM(t1->t);
      for(i=0;i<t->n;++i){
	z1p[i]=(I)gs(t1->t);bcopy(cp,((A)z1p[i])->p,ni);cp+=ni;
      }
    }
  }
  else {
    if ((!QA(a))||Et!=a->t) ERROUT(ERR_TYPE);
    if (1<a->r) ERROUT(ERR_RANK);
    if (1==a->n&&QS(*a->p)) {
      z=gv(Et,2);z->p[0]=(I)gv(Et,1);*((A)z->p[0])->p=*a->p;
      z->p[1]=(I)gv(Et,1);*((A)z->p[1])->p=(I)aplus_nl;R z;}
    z=gv(Et,2);
    if (1>=a->n) {z->p[0]=z->p[1]=(I)aplus_nl;R z;}
    ni=a->n/2+(((1&a->n)&&!qz((A)a->p[a->n-1]))?1:0);
    ap=a->p;z->p[0]=(I)gv(Et,ni);z->p[1]=(I)gv(Et,ni);
    z0p=((A)z->p[0])->p;z1p=((A)z->p[1])->p;
    for(i=0;i<ni;++i){
      if(QS(*ap)) z0p[i]=*ap++;
      else { 
	t=(A)*ap++;
	if((!QA(t))||1!=t->n||Et!=t->t||!QS(*t->p)){
	  A z0=(A)z->p[0],z1=(A)z->p[1];
	  z0->n=z0->d[0]=z1->n=z1->d[0]=i;
	  dc(z);ERROUT(ERR_DOMAIN);
	}
	z0p[i]=*t->p;
      }
      if(i>=a->n/2) z1p[i]=(I)aplus_nl;
      else if(QA(*ap))z1p[i]=ic((A)*ap++);
      else{z1p[i]=(I)gs(Et);*((A)z1p[i])->p=*ap++;}
    }
  }
#ifdef QQQ_REMDUP_YES
  /* remove duplicates, not implemented for now. */
  { A iota;I j;
    iota=(A)fnd(*z->p,*z->p);
    for(j=i=0;i<iota->n;++i,++j){
      if(i!=iota->p[i]){
	dc(z1p[i++]);
	--((A)z->p[0])->n;--((A)z->p[0])->d[0];
	--((A)z->p[1])->n;--((A)z->p[1])->d[0];
      } else if(j!=i){z0p[j]=z0p[i];z1p[j]=z1p[i];}
    }
    dc(iota);
  }
#endif
  R z;
}

  
void dotInstall()
{
  install((PFI)ep_flat,"_flat", A_, 1, A_,0,0,0,0,0,0,0);
  install((PFI)ep_issf,"_issf", IV, 1, A_,0,0,0,0,0,0,0);
  install((PFI)ep_alsf,"_alsf", A_, 1, A_,0,0,0,0,0,0,0);
  R;
}
