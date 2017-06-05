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
#if defined(__cplusplus)
#include <strings.h>
#else
#include <string.h>
#endif

extern I dbg_depth,dbg_txeq,dbg_tbwc;
Z I t,u,v;

I rsh(A a,I r,I *d){R g=(PFI)rsh,a->c==1?(mv(a->d,d,a->r=r),ic(a)):(I)gc(a->t,r,a->n,d,a->p);}
Z H1(rdc){I r,d[9];ND1 r=a->r-1;Q(r<1,7);R mv(d,a->d+1,r),*d*=*a->d,rsh(a,r,d);}
Z H1(monadicIota){A z;I1;{I r=a->n,*d=a->p,n=tr1(r,d);Q(n<0,9) Q(a->r>1,7)Q(r>MAXR,13)W(ga(It,r,n,d))d=z->p;DO(n,d[i]=i)R(I)z;}}
Z H1(rav){ND1 R rsh(a,1,&a->n);}
Z H1(sha){A z;ND1 W(gv(It,a->r))mv(z->p,a->d,a->r);R(I)z;}
GV0(C,m0){!u?zer(t,(I *)r,n):(w=tmv(t,(I *)r,(I *)w,n>u?u:n),n>u)?tmv(t,(I *)w,(I *)r,n-u):0;}
Z H2(rho){A z;ND2 I1{I wt=w->t,wn=w->n;I *d=a->p,r=a->n,n=tr1(r,d);Q(n<0,9)
 Q(r>MAXR,13)if(n==wn)R rsh(w,r,d);W(ga(t=wt,r,n,d))u=wn;C2(m0)}}
GV2(C,m2){r=u!=1?tmv(t,(I *)r,(I *)a,v):trp(t,(I *)r,(I *)a,v);u!=2?tmv(t,(I *)r,(I *)w,n-v):trp(t,(I *)r,(I *)w,n-v);}
#ifdef __lucid
Z I f_cl(A a,A w,I i)
 {
  A z;
  ND2
  X2
   {
    XA;
    XW;
    I r,n,d[9];

    i=!ar&&!wr||i==23;
    if(ar&&wr)
      if(ar==wr)
        Q(cm(ad+!i,wd+!i,ar-!i),8)
      else
       {
        Q(i||wr-ar!=1&&ar-wr!=1,7)
        if (wr < ar)
         {
          Q(cm(ad+1,wd,ar-1),8)
         }
        else
         {
          Q(cm(ad,wd+1,ar),8)
         }
       }
    if(wr<ar)
      n=1,mv(d+i,ad,r=ar);
    else
      mv(d+i,wd,r=wr),n=i||ar<wr?1:*ad;
    if(i)
      ++r,*d=1;
    *d+=n;
    Q(MAXR<r,13)
    u=!ar?(an=tr(r-1,d+1),1):!wr?(wn=tr(r-1,d+1),2):0;
    W(ga(t=wt,r,an+wn,d))
    v=an;
    C2(m2)
   }
 }
#else
Z I f_cl(A a,A w,I i){A z;ND2 X2{I ar=a->r,an=a->n,*ad=a->d;XW;I r,n,d[9];i=!ar&&!wr||i==23;
 if(ar&&wr)if(ar==wr)Q(cm(ad+!i,wd+!i,ar-!i),8)else{
  Q(i||wr-ar!=1&&ar-wr!=1,7)n=wr<ar;Q(cm(ad+n,wd+!n,ar-n),8)}
 if(wr<ar)n=1,mv(d+i,ad,r=ar);else mv(d+i,wd,r=wr),n=i||ar<wr?1:*ad;
 if(i)++r,*d=1;*d+=n;Q(MAXR<r,13)
 u=!ar?(an=tr(r-1,d+1),1):!wr?(wn=tr(r-1,d+1),2):0;
 W(ga(t=wt,r,an+wn,d))v=an;C2(m2)}}
#endif
GV1(I,r0){I *p=r+n;for(;r<p;)*--p=*w++;}
GV1(C,r1){C *p=w+Tt(t,n);for(;w<p;)r=tmv(t,(I *)r,(I *)(p-=Tt(t,v)),v);}
Z H1(rev){A z;I r;ND1 r=a->r;if(!r)R ic(a);W(gd(t=a->t,a))
 C1((PFI)(!--r&&!a->t?(PFI)r0:(PFI)(v=tr(r,a->d+1),r1)))} 
Z I m,d[9];
GV1(C,t1)
{
  I h=m,i,c[9];
  DO(h+1,c[i]=0);
  if(!n)R;
  for(;;)
  {
    for(n=c[i=h];i--;n=n*d[i]+c[i]);
    r=tst(t,(I *)r,1,(I *)(w+Tt(t,n)),v,u);
    for(i=h;++c[i]==d[i];)if(i)c[i--]=0;else R;
  }
}
Z H1(tra){A z;ND1{XA;if((m=ar-2)<0)R ic(a);DO(ar,d[i]=ad[ar-1-i])
 if(u= *ad,v=tr(ar-1,ad+1),v<2)R rsh(a,ar,d);W(ga(t=at,ar,an,d))C1(((PFI)t1))}}
GV0(C,k1){
  I tzer=(Et==t&&n-u&&QS(((I*)w)[0]))?Et+1:t;
  !u?tmv(t,(I *)r,(I *)(w+Tt(t,v)),n):
    u<0?tmv(t,(I *)zer(tzer,(I *)r,-u),(I *)w,n+u):
    zer(tzer,(I *)tmv(t,(I *)r,(I *)(w+Tt(t,v)),n-u),u);
}
Z I td(A a,A w,I i)
{
  A z;
  ND2 I0
    {
      I wt=w->t,wr=w->r,*wd=w->d;
      I j= *wd,k,m=*a->p;
      if(!wr)
	j=1,++wr; 
      if(i==26)
	m=m>0?(m>j?0:m-j):m<-j?0:m+j;
      k=tr(wr-1,wd+1);
      u=v=0;
      t=wt;
      if(m<0)
	if(m= -m,m>j)
	  u=(j-m)*k;
	else 
	  v=(j-m)*k;
      else 
	if(m>j)
	  u=(m-j)*k;
	else 
	  if(wt<Ct&&w->c==1&&m)
	    R g=(PFI)k1,w->n=(*w->d=m)*k,ic(w);
      W(ga(wt,wr,m*k,wd))*z->d=m;
      C2(k1)
    }
}
Z H1(siz){ND1 R(I)gi(a->r?*a->d:1);}
Z I b0(I *p,I n){I s=0,f=0;DO(n,if(~1&*p)if(f=1,*p<0)R -1;s+= *p++)R f?-s:s;}
extern I aw;
GV0(I,c0){I *p=r+n;I j=aw!=2;for(;r<p;w+=j)if(*a++)*r++=*w;}
GV0(F,c1){F *p=r+n;I j=aw!=2;for(;r<p;w+=j)if(*a++)*r++=*w;}
GV0(C,c2){C *p=r+n;I j=aw!=2;for(;r<p;w+=j)if(*a++)*r++=*w;}

GV0(C,c3){
  C *p=r+Tt(t,n),*s;
  I j,i=!u;

  for(;r<p;w+=(aw!=2)?Tt(t,v):0,a+=i) {
    j=*a;
    if(j)
      if(r=tmv(t,(I *)(s=r),(I *)w,v),--j)
        r=tmv(t,(I *)r,(I *)s,j*v);
  }
}

GV0(I,x0){I *p=r+n;I j=aw!=2;while(r<p)if(*a++){*r++=*w;w+=j;} else *r++=0;}
GV0(F,x1){F *p=r+n;I j=aw!=2;while(r<p)if(*a++){*r++=*w;w+=j;} else *r++=0.0;}
GV0(C,x2){C *p=r+n;I j=aw!=2;while(r<p)if(*a++){*r++=*w;w+=j;} else *r++=' ';}
GV0(C,x3){I tzer=(Et==t&&u&&QS(((I*)w)[0]))?Et+1:t;C *p=r+Tt(t,n);for(;r<p;)if(*a++)r=tmv(t,(I *)r,(I *)w,v),w+=(aw!=2)?Tt(t,v):0;else r=zer(tzer,(I *)r,v);}

/* expose ic_or_copy() so it can be used by other primitives */
H1(ic_or_copy){R !QA(a)?(I)a:a->c?ic(a):(I)gc(a->t,a->r,a->n,a->d,a->p);}

Z H2(cmp){
  A z;ND2 I1;
  {I ar=a->r,an=a->n;XW;
   I bn=b0(a->p,an),wl=wr?*wd:1;
   Q(bn==-1,9)
   aw=0;
   u=an==1;
   if((u)&&bn==1&&wr) R ic_or_copy(w);
   Q(ar>1,7)
   if(!wr) wl=wr=1;
   if(u) bn*=wl;
   else 
     if(wn==1) aw=2;
     else Q(wl!=an,8)
   if(wr==1&&wt!=Et&&bn>=0){
     W(gv(wt,bn))
     C2((!wt?(I(*)())c0:wt==Ft?(I(*)())c1:(I(*)())c2))
   }
   if(bn<0) bn=-bn;
   v=tr(wr-1,wd+1);
   W(ga(t=wt,wr,bn*v,wd))*z->d=bn;
   C2(c3)
}}

Z H2(xpn){
  A z;
  ND2 I1;
  {I ar=a->r,an=a->n;XW;
   I bn=b0(a->p,an),wl=wr?*wd:1;
   aw=0;
   Q(bn<0,9)
   Q(ar>1,7)
   if(!wr) wl=wr=1;
   if(wn==1) aw=2;
   else Q(wl!=bn,8)
   if(wr==1&&wt!=Et){
     W(gv(wt,an))
     C2((I(*)())(!wt?(I(*)())x0:wt==Ft?(I(*)())x1:(I(*)())x2))
   }
   v=tr(wr-1,wd+1);
   u=wn;
   W(ga(t=wt,wr,an*v,wd))*z->d=an;
   C2(x3)
}}

Z H1(ts){A z;Z C *t[]={"int","float","char","null","box","sym","func",
 "unknown"};
 W(gs(Et))*z->p=MS(si(t[QA(a)?(Et<a->t?6:Et>a->t?a->t:!a->n?3:
 Et==a->t?(QA(a=(A)*a->p)&&a->t<Xt?4:QS(a)?5:6):6):
 QP(a)?6:QX(a)?6:7]));R(I)z;}
Z H1(enc){A z;W(gs(Et))*z->p=ic(a);R(I)z;}
Z H2(n){R q=5,0;}
Z H1(e1){
  I z;
  I isWritableFile();
  ND1 Q(a->t!=Ct,6)Q(a->r>1,7);
  ++dbg_depth;if(dbg_txeq)xeqtrc((C *)a->p,1);
  if(a->c||isWritableFile((I)a))z=exm((C *)a->p,APL);
  else 
  {
    C *buf=(C *)mab(a->n+1);
    memmove(buf,a->p,a->n+1);
    z=exm(buf,APL);
    mf((I *)buf);
  }
  --dbg_depth;if(dbg_txeq)xeqtrc((C *)a->p,0);
  R z;
}
Z H2(e2){
  I z;
  I isWritableFile();
  CX cx=Cx;
  ND2;
  z=*a->p;
  if (It==a->t||qz(a)) 
  { 
    if(dbg_txeq)xeqtrc((C *)w->p,3);
    if(w->c||isWritableFile((I)w))z=pexm((I)w->p,APL);
    else 
    {
      C *buf=(C *)mab(w->n+1);
      memmove(buf,w->p,w->n+1);
      z=pexm((I)buf,APL);
      mf((I *)buf);
    }
    if(dbg_txeq)xeqtrc((C *)w->p,2);
    R z;
  }
  else R QS(z)?(Cx=cxi(XS(z)),z=e1(w),Cx=cx,z):(q=6,0);
}

Z H1(st){switch(sq){CS(1,aplus_err(0,0))CS(2,ff(a))}R ic(a);}
Z H1(out){R ff(a),ic(a);}
Z S as(A a){if(a->r)q=7;if(!sym(a))q=6;if(q)R(S)0;R XS(*a->p);}
Z H1(mrf){S v;ND1 v=as(a);if(!v)R 0;R ic((A)gt(sv(Cx,v)));}
Z H2(drf){S c,v;ND2 c=as(a),v=as(w);if(!c||!v)R 0;R ic((A)gt(sv(cxi(c),v)));}
Z I rake_1p(A a,I cd,I *pd){I z=0;if(QF(a)||Et!=a->t||fsy(a)&&a->n)R 1;
if(!a->n)R 0;if(cd>=*pd)*pd=cd+1;DO(a->n,z+=rake_1p((A)a->p[i],cd+1,pd));R z;}
Z void rake_2p(A a,A z){if(QF(a)||Et!=a->t||fsy(a)&&a->n)
{z->p[z->n++]=QA(a)?ic(a):(I)a;R;}if(!a->n)R;DO(a->n,rake_2p((A)a->p[i],z));R;}
Z H1(rake){A z;I n,d=0;n=rake_1p(a,0,&d);if(!n)R(I)aplus_nl;
 if(!d)R(I)gvi(Et,1,ic(a));z=gv(Et,n);z->n=0;rake_2p(a,z);R(I)z;}

Z I dbgand(A a,A w,I i){ND2;I2;if(dbg_tbwc)bitwisechk(a,w,i);R ds(a,w,i);}
Z I dbgscan(A a,I i){ND1;I1;if(dbg_tbwc)bitwisechk(a,0,i);R sc(a,i);}
Z I dbgslash(A a,I i){ND1;I1;if(dbg_tbwc)bitwisechk(a,0,i);R rs(a,i);}

extern I symToChar(A),aen(A),ran(A),not(A),neg(A),aab(A),sgn(A),cln(A),
  flr(A),rec(A),aln(A),pit(A),charToSym(A),dis(A),raz(A),pct(A),
  dep(A),upg(A),dng(A),monadicBeam(A),bwnot(A),sg(A),undot(A);
extern I ind(A,A),bin(A,A), dyadicBeam(A,A),cir(A,A),dtr(A,A),sqr(A,A),
  dea(A,A),mat(A,A),ncd(A,A),dcd(A,A),prt(A,A),pen(A,A),mem(A,A),
  pic(A,A),dth(A,A), castOr(A,A),rot(A,A),dot(A,A);
extern I os(A,A,I),is(A,A,I),bs(A,A,I);
extern A mmd(A);
extern A dmd(A,A);

PFI P1[]=
{
  (PFI)st,       (PFI)ts,       (PFI)ic_or_copy, (PFI)sgn,    (PFI)cln,
  (PFI)flr,      (PFI)neg,      (PFI)rec,        (PFI)aab,    (PFI)enc,
  (PFI)dis,      (PFI)n,        (PFI)n,          (PFI)n,      (PFI)n,
  (PFI)aen,      (PFI)aln,      (PFI)ran,        (PFI)tra,    (PFI)monadicIota,
  (PFI)siz,      (PFI)sha,      (PFI)rav,        (PFI)not,    (PFI)rev,
  (PFI)sg,       (PFI)out,      (PFI)upg,        (PFI)dng,    (PFI)dep,
  (PFI)charToSym,(PFI)symToChar,(PFI)n,  	 (PFI)n,      (PFI)rake,
  (PFI)pct,      (PFI)raz,      (PFI)mmd,        (PFI)rdc,    (PFI)pit, 
  (PFI)dbgscan,  (PFI)dbgscan,  (PFI)sc,         (PFI)sc,     (PFI)sc,
  (PFI)sc,       (PFI)dbgslash, (PFI)dbgslash,   (PFI)rs,     (PFI)rs,
  (PFI)rs,       (PFI)rs,       (PFI)n,  	 (PFI)n,      (PFI)n,
  (PFI)n,        (PFI)n,        (PFI)n,          (PFI)n,      (PFI)n,
  (PFI)n,        (PFI)n,	(PFI)n,  	 (PFI)n,      (PFI)n,  
  (PFI)n,        (PFI)n, 	(PFI)monadicBeam,(PFI)n,      (PFI)n, 
  (PFI)mth,      (PFI)e1,       (PFI)gz, 	 (PFI)ic_or_copy,(PFI)mrf,
  (PFI)undot,    (PFI)n,        (PFI)n,          (PFI)n,      (PFI)n,
  (PFI)n,        (PFI)n,        (PFI)n,          (PFI)n,      (PFI)bwnot
};
PFI P2[]=
{
  (PFI)dbgand, (PFI)castOr,   (PFI)ds,        (PFI)ds,      (PFI)ds,
  (PFI)ds,     (PFI)ds,       (PFI)ds,        (PFI)ds,      (PFI)ds,
  (PFI)ds,     (PFI)ds,       (PFI)ds,        (PFI)ds,      (PFI)ds,
  (PFI)sqr,    (PFI)ds,       (PFI)dea,       (PFI)dtr,     (PFI)fnd,
  (PFI)ind,    (PFI)rho,      (PFI)f_cl,      (PFI)f_cl,    (PFI)rot,
  (PFI)td,     (PFI)td,       (PFI)bin,       (PFI)n,       (PFI)mat,
  (PFI)ncd,    (PFI)dcd,      (PFI)cmp,       (PFI)xpn,     (PFI)mem,
  (PFI)pen,    (PFI)pic,      (PFI)dmd,       (PFI)prt,     (PFI)cir,
  (PFI)n,      (PFI)n,        (PFI)n,         (PFI)n,       (PFI)n,
  (PFI)n,      (PFI)n,        (PFI)n,         (PFI)n,       (PFI)n,
  (PFI)n,      (PFI)n,        (PFI)os,        (PFI)os,      (PFI)os,
  (PFI)os,     (PFI)os,       (PFI)os,        (PFI)os,      (PFI)os,
  (PFI)os,     (PFI)os,       (PFI)os,        (PFI)os,      (PFI)os,
  (PFI)os,     (PFI)is,       (PFI)dyadicBeam,(PFI)is,      (PFI)is,
  (PFI)dth,    (PFI)e2,       (PFI)ic,        (PFI)n,       (PFI)drf,
  (PFI)dot,    (PFI)bs,       (PFI)bs,        (PFI)bs,      (PFI)bs,
  (PFI)bs,     (PFI)bs,       (PFI)bs,        (PFI)bs,      (PFI)n
};

I sizeOfPrimArray(I type_)
{
  if(type_==1) R sizeof(P1)/sizeof(PFI);	/* Monadic */
  else R sizeof(P2)/sizeof(PFI);		/* Dyadic  */
}
