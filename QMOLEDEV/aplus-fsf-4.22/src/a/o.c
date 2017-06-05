/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/ik.h>
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/arthur.h>

I atOnExit, wtOnExit;	/* set in fa() used by o.c:rk() */

Z I dp(A a)
{
  I k,d=0;
  if(QF(a))R -1;
  if(a->t<Et)R 0;
  DO(a->n,if((k=1+dp((A)a->p[i]))>d)d=k);
  R d;
}

Z C *i2f(I t,F *d,I *s,I n){DO(n,*d++=*s++)R(C*)d;}

Z I dr(A a,I f)
{
  XA;
  A z,*ap=(A*)a->p,w;
  if(!an||at!=Et||fsy(a))R ic(a); /* ic_or_copy(a) */
  w=*ap;
  Q(QF(w),9);
  if(!ar)R ic(w);  /* ic_or_copy(w) */
  {
    XW;
    I i=an,n=0,t;
    C *p;
    if(f){Q(ar>1,7)V0 n=*wd;}
    else{Q(ar+wr>MAXR,13)mv(ad+ar,wd,wr);}
    for(;--i;)
    {
      Q(!QA(a=ap[i])||(t=a->t)>Et,9);
      if(wt!=t&&a->n)
	if(f&&!n)wt=t;
	else{Q(wt>Ft||t>Ft,6)wt=Ft;}
      if(wr!=a->r){Q(!f||wr>1||a->r,7)*a->d=1;}
      Q(cm(wd+f,a->d+f,wr-f),11);
      if(f)n+=*a->d;
    }
    W(ga(wt,f?wr:ar+wr,f?n*tr(wr-1,wd+1):an*wn,f?wd:ad));
    if(f)*z->d=n;
    p=(C*)z->p;
    DO(an,a=ap[i];
       p=(*(a->t==wt?(C *(*)(I,F *,I *,I))tmv:i2f))(wt,(F *)p,a->p,a->n));
    R(I)z;
  }
}
H1(dis){Q(!QA(a),18)R dr(a,0);}
H1(raz){Q(!QA(a),18)R dr(a,1);}
H1(dep){R(I)gi(dp(a));}Z I t,v;
H2(pen){
  A z,*p;
  ND2 I1
  {
    XW;
    I t=wt,d[9],*ap=a->p,an=a->n,j=an==1?*ap:0,k=*wd,n;
    C *wp=(C*)w->p;
    Q(!wr,7);
    DO(an,Q(ap[i]<0,9));
    n=tr(wr-1,wd+1);
    mv(d+1,wd+1,wr-1);
    if(j&&k!=(an=k/j)*j)++an;
    W(j?gv(Et,an):gd(Et,a));
    *--Y=zr(z),p=(A*)z->p;
    for(;an--;wp+=Tt(t,wn),k-=*d)
      *p++=gc(wt,wr,wn=n*(*d=j?j:*ap++,*d=*d>k?k:*d),d,(I *)wp);
    R ++Y,(I)z;
  }
}
GV0(C,lag)
{
  C *p=r+Tt(t,n);
  I j=Tt(t,v),k=v*-*a;
  for(;r<p;w+=j)r=tmv(t,(I *)r,(I *)w,k);
}
H2(prt)
{
  A z;ND2 I0
  {
    I d[9],j=*a->p,k=*w->d,r=w->r;
    Q(MAXR<=r,13);
    if(!r)R j==1?rsh(w,1,a->p):(q=7,0);
    mv(d+2,w->d+1,r-1);
    if(j>0)
    {
      d[1]=j,*d=k/j;
      Q(k!=*d*j,8);
      R rsh(w,r+1,d);
    }
    d[1]=-j,*d=k+j+1;
    Q(*d<0,8);
    W(ga(t=w->t,r+1,*d*d[1]*(v=tr(r-1,d+2)),d));
    C2(lag);
  }
}
H1(pct){A z;I i=0,j=0,k=0,n,*p;ND1 n=a->n;I1 W(a->c==1?(A)ic(a):gv(It,n))
 if(n){Q(a->r>1,7)Q(!*a->p,9)for(p=z->p;++i<n;)
  if(a->p[i])p[j++]=i-k,k=i;p[j++]=n-k,z->n=*z->d=j;}R(I)z;}

Z I gC(I t,I r,I n,I *d,I *p)
{
  A z=ga(t,r,n,d);
  p?tmv(t,z->p,p,n):zer(t,z->p,n);
  R(I)z;
}
Z I raw(I r,I i){R i<0?(-i>r?r:-i):i>r?0:r-i;}
I aw_c[2]={1,1};

I rk(I f,A r,A a,A w)
{
  A z=0,*p=0;
  if(w)ND2 else ND1;
  {
    XA;
    C *pp=0,*ap,*wp;
    I wt=0,wr=0,wn=0,*wd=0;
    I n=0,t=0,i,j,k,d[9],rw,ra,ri,ir,iw=0,ia=0,ii=0,
    e=!w&&f==MP(9),h=QP(f)&&f!=MP(71)&&!e;
    Q(!QA(r),9);
    Q(r->t,6);
    Q(r->n<1||r->n>3,8);
    ar-=ra=raw(ar,*r->p);
    if(!w) mv(d,ad,ra),ir=tr(ra,ad),ad+=ra;
    else 
    {
      wt=w->t,wr=w->r,wd=w->d;
      wr-=rw=raw(wr,r->p[r->n>1]),ri=r->n>2?r->p[2]:9;
      Q(ri<0,9);
      if(ri>ra)ri=ra;
      if(ri>rw)ri=rw;
      mv(d,ad,ra-=ri);
      ia=tr(ra,ad),mv(d+ra,wd,rw),iw=tr(rw-=ri,wd);
      Q(cm(ad+=ra,wd+=rw,ri),11);
      ii=tr(ri,ad),ra+=rw+ri,ir=ia*iw*ii,wn=tr(wr,wd+=ri),ad+=ri;
      if(h&&ir>iw&&(f==MP(21)||f==MP(25)||f==MP(26)||f==MP(32)||f==MP(33)))
	h=0;
    } 
    an=tr(ar,ad);
    if(h)
    {
      g=0;
      aw_c[0]=a->c;
      aw_c[1]=w&&w->c;
      r=(A)fa(f,gC(at,ar,an,ad,a->n?a->p:0),w?gC(wt,wr,wn,wd,w->n?w->p:0):0);
      aw_c[0]=aw_c[1]=1;
      if(!r)R 0;
      r=un(&r);
      mv(d+ra,r->d,j=r->r);
      if((j+=ra)>MAXR)R q=13,(I)r;
      n=r->n;t=r->t;
      if(ir<2) R mv(r->d,d,r->r=j),r->n*=ir,(I)r;
      dc(r);
      if(g==(I (*)())rsh) R rsh(w?w:a,j,d);
      if(!g){h=0;}
      else
      {
	if(at=atOnExit,w) wt=wtOnExit;
	if(at!=a->t&&!(a=at?ep_cf(1):ci(1))) R 0;
	if(w&&wt!=w->t&&!(w=wt?ep_cf(2):ci(2))) R 0;
	OF(i,ir,n);
	W(ga(t,j,i,d));
	pp=(C*)z->p;
      }
    }
    if(!h) 
    {
      W(ga(Et,ra,ir,d));
      *--Y=zr(z),p=(A*)z->p;
    }
    if(!w)
    {
      for(ap=(C*)a->p;ir--;ap+=Tt(at,an))
	if(h) (*(I(*)(C*,C*,I))g)(pp,ap,an),pp+=Tt(t,n);
	else a=gc(at,ar,an,ad,(I*)ap),*p++=e?a:(A)fa(f,(I)a,0);
    } 
    else
    {
      for(i=0;i<ia;++i)for(j=0;j<iw;++j)
	for(k=0;k<ii;++k){
	  ap=(C*)a->p+Tt(at,(i*ii+k)*an);
	  wp=(C*)w->p+Tt(wt,(j*ii+k)*wn);
	  if(h)
	  {
	    (*(I(*)(C*,C*,C*,I))g)(pp,ap,wp,n),pp+=Tt(t,n);
	    if(q==1)*--Y=(I)z,aplus_err(q,(A)Y[1]),++Y;
	  } 
	  else
	  { 
	    *p++=(A)fa(f,(I)gc(at,ar,an,ad,(I*)ap),(I)gc(wt,wr,wn,wd,(I*)wp));
	  }
	}
    }
    if(h)R(I)z;
    if(!e)z=(A)dis(r=z),dc(r);
    R ++Y,(I)z;
  }
}

I ea(I f,A a,A w)
{
  A z,*p;I at,k,wt=0,j=0;
  C *ap,*wp=0;
  if(w)ND2 else ND1;
  at=a->t,k=a->r?Tt(at,1):0,ap=(C*)a->p;
  if(w)
    if(wt=w->t,wp=(C*)w->p,j=w->r?Tt(wt,1):0,k&&j)
    {
      Q(a->r!=w->r,7);
      Q(cm(a->d,w->d,a->r),8);
    }
  W(gd(Et,j?w:a));
  *--Y=zr(z),p=(A*)z->p;
  DO(z->n,
     if(at<Et||(a=*(A*)ap,QF(a)))a=gc(at,0,1,0,(I*)ap);
     else ic(a);
     ap+=k;
     if(w)
     {
       if(wt<Et||(w=*(A*)wp,QF(w)))w=gc(wt,0,1,0,(I*)wp);
       else ic(w);
       wp+=j;
     }
     *p++=(A)fa(f,(I)a,(I)w)
     );
  R ++Y,(I)z;
}

/*******************************************************************
 *
 * Circle function (cir() and pif())
 *
 */

/* extern F sqrt(),sin(),cos(),tan(),asin(),acos(),atan(),sinh(),cosh(),
   tanh(),asinh(),acosh(),atanh(); */

#define CIRCFUNCCOUNT 15

Z char *CircleFuncList[CIRCFUNCCOUNT]={
  "arctanh","arccosh","arcsinh","tanarcsec","arctan","arccos","arcsin",
  "sinarccos","sin","cos","tan","secarctan","sinh","cosh","tanh"};

Z A circleFuncSyms = (A)0;

A getCircleFuncSyms(void)
{
  if((A)0==circleFuncSyms)
  {
    A aobj=gv(Et,CIRCFUNCCOUNT);
    DO(CIRCFUNCCOUNT,aobj->p[i]=MS(si(CircleFuncList[i])));
    aobj->c=0;
    circleFuncSyms=aobj;
  }
  R circleFuncSyms;
}

H2(cir)
{
  A tempa, z;
  if(!QA(a)||It==a->t||Ft==a->t)R ds(a,w,17);
  Q(Et!=a->t,6);
  tempa=(A)fnd(getCircleFuncSyms(),a);
  DO(tempa->n,if(CIRCFUNCCOUNT==tempa->p[i]){q=9;R 0;};tempa->p[i]-=7;);
  dc((A)Y[0]);Y[0]=(I)tempa;
  z=(A)ds(tempa,w,17);
  R(I)z;
}

F pif(I i,F x){
  switch(i){
  default:Q(1,9);
    CSR(0,R sqrt(1-x*x));
    CSR(4,R sqrt(1+x*x));
    CSR(-4,R sqrt(-1+x*x));
    CSR(8,R sqrt(-1-x*x));
    CSR(-8,R -sqrt(-1-x*x));
    CSR(1,R sin(x));
    CSR(2,R cos(x));
    CSR(3,R tan(x));
    CSR(-1,R asin(x));
    CSR(-2,R acos(x));
    CSR(-3,R atan(x));
    CSR(5,R sinh(x));
    CSR(6,R cosh(x));
    CSR(7,R tanh(x));
    CSR(-5,R asinh(x));
    CSR(-6,R acosh(x));
    CSR(-7,R atanh(x));
  }
}
