/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.
*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <string.h>
#include <stdio.h>
#include <a/ik.h>
#include <a/b.h>
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/fir.h>
#include <a/arthur.h>

Z I *tp,t,u,v;typedef struct h{struct h *__h;I i;}*HH;
/*I ne(F *f,F *g){R *f<*g&&*f<*g*(*g>0?CT1:CT2)||*f>*g*(*g>0?CT2:CT1);} */
 I ne(F *f,F *g){
   int c1;
   F c2, c3;
 
   c1 = *f<*g;
   if (*g > 0) {
     c2 = CT1;
     c3 = CT2;
   } else {
     c2 = CT2;
     c3 = CT1;
   }
 
   R c1&&*f<*g*c2||*f>*g*c3;
 }

I sym(A a){if(a->t!=Et||!a->n)R 0;DO(a->n,if(!QS(a->p[i]))R 0)R 1;}
I fsy(A a){DO(a->n,if(!QF(a->p[i]))R 0)R 1;}
I cm(I *d,I *s,I n){I *t=d+n;for(;d<t;++d,++s)if(*d!=*s)R 1;R 0;}
Z I cmf(F *d,F *s,I n){F *t=d+n;for(;d<t;++d,++s)if(ne(d,s))R 1;R 0;}
Z I cme(I *d,I *s,I n){I *t=d+n;for(;d<t;++d,++s)if(mt((A)*d,(A)*s))R 1;R 0;}
/*Z I (*ct(I t))(I *,I *,I){R (I(*)(I*,I*,I))(!t?(PFI)cm:t==Ft?(PFI)cmf:t==Ct?(PFI)memcmp:(PFI)cme);} */
 
 Z I (*ct(I t))(I *,I *,I){
   if (!t) {
     return (PFI)cm;
   } else {
     if (t==Ft) {
       return (PFI)cmf;
     } else {
       if (t == Ct) {
         return (PFI)memcmp;
       } else {
         return (PFI)cme;
       }
     }
   }
 }

Z I cfi(A a,A w){
  I n=a->n,i=a->t,j=w->t,*s;F *t,*d,f;
  /*if(!n)R 0;*/
  if(i==Ft&&j==It)s=w->p,d=(F*)a->p;
  else if(i==It&&j==Ft)s=a->p,d=(F*)w->p;
  else R 1;
  for(t=d+n;d<t;++d,++s)if(f=*s,ne(d,&f))R 1;
  R 0;
}
I mt(A a,A w){I i,j;R !QA(a)||!QA(w)||(i=a->t)>Et||(j=w->t)>Et?a!=w:
 cm(&a->r,&w->r,2+a->r)?1:i!=j?cfi(a,w):(ct(i))(a->p,w->p,a->n);}
GV2(I,m0){*r=!(ct(t))(a,w,u);}
H2(mat){A z;if(a==w)R(I)gi(1);if(!QA(a)||!QA(w))R(I)gi(0);
 if(cm(&a->r,&w->r,2+a->r))R(I)gi(0);
 if(a->t!=(t=w->t))R(I)gi(!cfi(a,w));W(gs(It))u=a->n;C2(m0)}
#define CF(f,t) Z I f(t *p,I i,I j,I n,I u){t*d=p+i*n,*s=p+j*n; \
				     DO(n,if(d[i]!=s[i])R u^d[i]<s[i])R i<j;}
CF(lf,F)
CF(li,I)
CF(lc,UC)
Z I ls(I *p,I i,I j,I n,I u){I*d=p+i*n,*s=p+j*n;
 DO(n,if(d[i]!=s[i])R u^(0>strcmp(XS(d[i])->n,XS(s[i])->n)))R i<j;}
Z I mrg(PFI f,I n,I z,I *p,I l,I h,I u){
  I t,m=(l+h+1)/2;
  if(h==m)R p[l]=-1,l;
  if((*(I(*)(I,I,I,I,I))f)(z,h=mrg(f,n,z,p,m,h,u),l=mrg(f,n,z,p,l,m,u),n,u))m=l,l=h,h=m;
  for(t=l;;l=p[l]=h,h=m){
  L:if(m=p[l],m==-1)R p[l]=h,t;
    if((*(I(*)(I,I,I,I,I))f)(z,m,h,n,u)){l=m;goto L;}
  }
}
Z I srt(A a,I u){
  A z;
  XA,n=*ad,*t,*p,j,y=0;
  Q(!ar,7);Q(Et==at&&an&&!(y=sym(a)),6);t=k_tm(n);W(gv(It,n));
  if(n){
    *(p=z->p)=j=mrg((PFI)(at==Ct?(PFI)lc:(Ft==at?(PFI)lf:(y?(PFI)ls:(PFI)li))),tr(ar-1,ad+1),(I)a->p,t,0,n,u);
    DO(n-1,*++p=j=t[j]);
  }
  R(I)z;
}
Z void u1(I *r,I w,I n){*r=mrg((PFI)lf,1,w,tp,0,n,v);DO(n-1,r[i+1]=tp[r[i]])}
Z void fsu(I *r,F *w,I n){
  F x,u=*w,y=u;
  I i,c=n*20,*p=tp;
  HH h=(HH)(p+n),j,k;
  DO(n,if(!finite(w[i])){u1(r,(I)w,n);R;})	/* Inf and NaN check */
  DO(n,if(p[i]=0,x=w[i],x<u)u=x;else if(x>y)y=x);
  if(!finite(y-=u)){q=0;u1(r,(I)w,n);R;} /* Inf and NaN check */
  if(y)y=n/(y*CT2);
  for(i=n;i--;){
    for(x=w[i],k=(HH)(p+(I)((x-u)*y));
	(j=k->__h)&&x>w[j->i];
	k=j)
      if(!--c){u1(r,(I)w,n);R;}
    h->__h=j;h->i=i;k->__h=h++;
  } 
  DO(n,for(h=(HH)p[i];h;h=h->__h)*r++=h->i);
}
Z void fsd(I *r,F *w,I n){
  F x,u=-*w,y=u;
  I i,c=n*20,*p=tp;
  HH h=(HH)(p+n),j,k;
  DO(n,if(!finite(-w[i])){u1(r,(I)w,n);R;}) /* Inf and NaN check */ 
  DO(n,if(p[i]=0,x=-w[i],x<u)u=x;else if(x>y)y=x);
  if(!finite(y-=u)){q=0;u1(r,(I)w,n);R;} /* Inf and NaN check */
  if(y)y=n/(y*CT2);
  for(i=n;i--;){
    for(x=-w[i],k=(HH)(p+(I)((x-u)*y));
	(j=k->__h)&&x>-w[j->i];
	k=j)
      if(!--c){u1(r,(I)w,n);R;}
    h->__h=j;h->i=i;k->__h=h++;
  } 
  DO(n,for(h=(HH)p[i];h;h=h->__h)*r++=h->i);
}
GV1(I,isu){
  unsigned long j,k=n*3,m=0;
  I *p=tp;*p=0;
  DO(n,
     if(m<(j=w[i])){
       if(j>=k){q=-2;R;}
       do p[++m]=0;while(m<j);
     }
     ++p[j]
     );
  j=0;
  DO(m,k=p[i];p[i]=j;j+=k);
  p[m]=j;
  DO(n,r[p[w[i]]++]=i);
}
GV1(I,isd){
  unsigned long j,k=n*3,m=0;
  I *p=tp;*p=0;
  DO(n,
     if(m<(j=n-w[i])){
       if (j>=k){q=-2;R;}
       do p[++m]=0;while(m<j);
     }
     ++p[j]
     );
  j=0;
  DO(m,k=p[i];p[i]=j;j+=k);
  p[m]=j;
  DO(n,r[p[n-w[i]]++]=i);
}
H1(upg){A z;I n,t;ND1 n=a->n,t=a->t;v=0;if(!n||a->r!=1||t>Ft)R srt(a,0);
 W(gv(It,n))*--Y=(I)z,tp=k_tm(n*3),++Y;C1((t==Ft?(PFI)fsu:(PFI)isu))}
H1(dng){A z;I n,t;ND1 n=a->n,t=a->t;v=1;if(!n||a->r!=1||t>Ft)R srt(a,1);
 W(gv(It,n))*--Y=(I)z,tp=k_tm(n*3),++Y;C1((t==Ft?(PFI)fsd:(PFI)isd))}
/* H1(dng){ND1 R srt(a,1);} */
     
Z I i_f0(I n,I k){register I m=n;do k^=k>>m;while(32>(m+=m));R k;}
/* !!! NOTEIT: shift 4 in ch() and ih() looks suspect !!! */
Z I ch(C *p,I n){I r=0;DO(n,r^=*p++<<4*(i&7))R r;}
Z I ih(I *p,I n){register I r=0,x;DO(n,r<<=4;x=*p++;r^=(0<=x)?x:x^0xAAAAAAAA)
		 R r;}

#if defined(__i386) || defined(__alpha) || defined(__ia64) || defined(__x86_64)
/* Z I ffh(F *xp,I n){F f;int *z=(int*)&f;while(--n&&!*xp)++xp;f=*xp; */
/* 		  R(z[1]&0x7FFFFFFF)^(z[0]&0xFFFFF000);} */
/* Z I fct1h(F *xp,I n){F f;int *z=(int*)&f;while(--n&&!*xp)++xp;f=CT1**xp; */
/* 		   R(z[1]&0x7FFFFFFF)^(z[0]&0xFFFFF000);} */
/* Z I fct2h(F *xp,I n){F f;int *z=(int*)&f;while(--n&&!*xp)++xp;f=CT2**xp; */
/* 		   R(z[1]&0x7FFFFFFF)^(z[0]&0xFFFFF000);} */
Z I ffh(F *xp,I n) {
  union {
    int z[2];
    F f;
  } u;

  while(--n&&!*xp) {
    ++xp;
  }
  u.f=*xp;
  R(u.z[1]&0x7FFFFFFF)^(u.z[0]&0xFFFFF000);
}

Z I fct1h(F *xp,I n) {
  union {
    int z[2];
    F f;
  } u;

  while(--n&&!*xp) {
    ++xp;
  }
  u.f=CT1**xp;
  R(u.z[1]&0x7FFFFFFF)^(u.z[0]&0xFFFFF000);
}

Z I fct2h(F *xp,I n) {
  union {
    int z[2];
    F f;
  } u;

  while(--n&&!*xp) {
    ++xp;
  }
  u.f=CT2**xp;
  R(u.z[1]&0x7FFFFFFF)^(u.z[0]&0xFFFFF000);
}
#else
/* Z I ffh(F *xp,I n){F f;int *z=(int*)&f;while(--n&&!*xp)++xp;f=*xp; */
/* 		  R(z[0]&0x7FFFFFFF)^(z[1]&0xFFFFF000);} */
/* Z I fct1h(F *xp,I n){F f;int *z=(int*)&f;while(--n&&!*xp)++xp;f=CT1**xp; */
/* 		   R(z[0]&0x7FFFFFFF)^(z[1]&0xFFFFF000);} */
/* Z I fct2h(F *xp,I n){F f;int *z=(int*)&f;while(--n&&!*xp)++xp;f=CT2**xp; */
/* 		   R(z[0]&0x7FFFFFFF)^(z[1]&0xFFFFF000);} */

Z I ffh(F *xp,I n) {
  union {
    int z[2];
    F f;
  } u;

  while(--n&&!*xp) {
    ++xp;
  }
  u.f=*xp;
  R(u.z[0]&0x7FFFFFFF)^(u.z[1]&0xFFFFF000);}
Z I fct1h(F *xp,I n) {
  union {
    int z[2];
    F f;
  } u;

  while(--n&&!*xp) {
    ++xp;
  }
  u.f=CT1**xp;
  R(u.z[0]&0x7FFFFFFF)^(u.z[1]&0xFFFFF000);}
Z I fct2h(F *xp,I n) {
  union {
    int z[2];
    F f;
  } u;

  while(--n&&!*xp) {
    ++xp;
  }
  u.f=CT2**xp;
  R(u.z[0]&0x7FFFFFFF)^(u.z[1]&0xFFFFF000);}

#endif

Z I eh(A *x,I n){
  A a=*x;
  R !QA(a)||a->t>Et?(I)a>>4:
  !a->n?0:
  a->t==Et?eh((A *)a->p,1):
  a->t==It?*a->p:
  a->t==Ct?ch((C *)a->p,a->n):
  *(F*)a->p*(1+2e-13);
}
extern I aw_c[];  /* aw_c set by rk(), else always {1,1}. */
#define G3(f) Z I f(I *r,UC *a,UC *w,I n)
#define GV3(f) Z void f(I *r,UC *a,UC *w,I n)
GV3(g0){I (*f)(I *,I *,I)=ct(t),j=u,k=v,i=0;for(n=Tt(t,k);i<j&&(*f)((I *)a,(I *)w,k);++i)a+=n;*r=i;}
Z I p2(I n){I z=0;while(n)++z,n>>=1;R z;}
G3(g1){
  I(*f)(I *,I *,I)=ct(t),(*hfn)(UC *,I),(*hfn1)(UC *,I);
#ifdef BSTUB
  I m=u,j,k=v,l=Tt(t,k),*p=tp,b=(p2(u*4))-2,bm=1<<b;
#else
  I m=u,j,k=v,l=Tt(t,k),*p=tp,b=((C)p[-1])-1,bm=1<<b;
#endif
  HH h=(HH)(p+bm);
  RESETXCP;
  DO(bm--,p[i]=0);
  hfn =(I(*)(UC*,I))(!t?(PFI)ih:t==Ct?(PFI)ch:t==Ft?(PFI)  ffh:(PFI)eh);
  hfn1=(I(*)(UC*,I))(!t?(PFI)ih:t==Ct?(PFI)ch:t==Ft?(PFI)fct2h:(PFI)eh);

  /* construct hash table */
  for(a+=m*l;m--;h->i=m,p[j]=(I)(h++))
    a-=l,h->__h=(HH)p[j=bm&i_f0(b,(*hfn)(a,k))];

  /* Note that we tried eliminating duplicate entries, but the odds
     of "useful" collision were so small that the additional cost of
     comparison far outweighed it.
   */
#ifdef HASHDUMP
  {
    I buck=0,tl=0,ml=0,cl=0,*pe=p+bm,*cp=p;V v=vi(si("fndh",Rx));A aobj;
    for(;cp<pe;++cp)
      if(*cp){++buck;cl=0;for(h=(HH)*cp;h;h=h->__h)++cl;tl+=cl;ml=(ml<cl)?cl:ml;}
    H("check: hash report:\n");
    H("\titems:%d\n\tslots:%d\n\tbuckets:%d\n\tstored:%d\n",u,pe-p,buck,tl);
    H("\tmax:%d\n\taverage:%lf\n",ml,(F)tl/(F)buck);
    dc(v->a);aobj=gv(It,buck);
    for(cl=ml=0;ml<pe-p;++ml)if(p[ml])aobj->p[cl++]=ml;
    v->a=(I)aobj;v->t=0;
  }
#endif
  /* use hash table */
  DO(n,
     CHECKXCP;if(q)R 0;
     j=bm&i_f0(b,((*hfn1))(w,k));
      for(h=(HH)p[j];h&&(*f)((I *)(a+l*h->i),(I *)w,k);h=h->__h);*r=h?h->i:u;
     if(f==(I(*)(I *,I *,I))cmf&&j!=(m=bm&i_f0(b,fct1h((F *)w,k)))){
       for(h=(HH)p[m];h&&cmf((F *)(a+l*h->i),(F *)w,k);h=h->__h);if(h&&h->i<*r)*r=h->i;}
     ++r;w+=l);
  R -1;
}
GV3(c0){UC *t=a+u,*p;DO(n,for(p=a;p!=t&&*p!=*w;++p);*r++=p-a;w++)}
Z I cT[256];
GV3(c1){I i;DO(256,cT[i]=u)for(i=u;i--;cT[a[i]]=i);DO(n,r[i]=cT[w[i]])}
GV2(I,i0){I *t=a+u,*p;DO(n,for(p=a;p!=t&&*p!=*w;++p);*r++=p-a;w++)}
GV2(I,i1){I j,k;DO(n,for(k=*w++,j=0;j<u&&a[j]!=k;++j);*r++=j)}
#ifdef BSTUB
G2(I,i2){
  I *p=tp,i,k=u,bm=u*4;
  DO(bm,p[i]=k);
  for(a+=i=k;i--;p[k+*a]=i){
    if(*--a<-k||bm<=k+*a)R g1(r,(UC *)(a-i),(UC *)w,n);
    if((k+*a)==~bm)H("check: end limit: *a:%ld\n",*a);
  }
  for(a=w+n;w<a;++w)*r++=(*w<-k||bm<=k+*w)?k:p[k+*w];
  R -1;
}
#else
G2(I,i2){
  I *p=tp,i,k=u,b=((C)p[-1]),bm=MZ[b];
  DO(bm,p[i]=k);
  for(a+=i=k;i--;p[k+*a]=i){
    if(*--a<-k||bm<=k+*a)R g1(r,(UC *)(a-i),(UC *)w,n);
    if((k+*a)==~bm)H("check: end limit: *a:%d\n",*a);
  }
  for(a=w+n;w<a;++w)*r++=(*w<-k||bm<=k+*w)?k:p[k+*w];
  R -1;
}
#endif
H2(fnd){
  A z;ND2 X2;
  {
    I at=a->t,ar=a->r,*ad=a->d;I wt=w->t,wr=w->r,*wd=w->d;I n;u=ar?(--ar,*ad++):1;v=tr(ar,ad);wr-=ar;
    Q(wr<0,7)Q(cm(ad,wd+wr,ar),8);
    if(wt==Et&&fsy(w))wt=It;
    W(ga(It,wr,n=tr(wr,wd),wd));
    if(!u||!v)R zr(z);
    if((!ar)&&(wt==It&&n<20||wt==Ct))
      C2(((I (*)())wt?(n<12&&(a->c&&aw_c[0])?(I (*)())c0:(I (*)())c1):(a->c&&aw_c[0])?(I (*)())i0:(I (*)())i1));
    t=wt;if(n==1)C2((I (*)())g0)*--Y=(I)z,tp=k_tm(u*4),++Y;
    C2((ar||at?(I (*)())g1:(I (*)())i2));
  }
}

static I index_of2(I hi,A aa,A ww,I n)	
{
  A a,w;
  I r=0,i=1,_i=ww->n;
  I *aip,*wip;
  F *afp,*wfp;
  
  for(;i<_i&&!r;++i) {
    a=(A)(aa->p[i]); w=(A)(ww->p[i]);
    Q(a->t!=w->t,ERR_TYPE)
    if (a->t==Ft){
      afp=(F *)a->p; afp+=hi; wfp=(F *)w->p; wfp+=n;
      r=(ct(a->t))((I *)afp,(I *)wfp,1);
    } else {
      aip=a->p; aip+=hi; wip=w->p; wip+=n;
      r=(ct(a->t))(aip,wip,1);
    }
  }
  R r;
}

static I index_of1(I *r,A aa,A ww,I n)
{
  UC *a=(UC *)((A)(aa->p[0]))->p;
  UC *w=(UC *)((A)(ww->p[0]))->p; 
  I(*f)(I *,I *,I)=ct(t),(*hfn)(UC *,I),(*hfn1)(UC *,I);
#ifdef BSTUB
  I m=u,j,k=v,l=Tt(t,k),*p=tp,b=(p2(u*4))-2,bm=1<<b;
#else
  I m=u,j,k=v,l=Tt(t,k),*p=tp,b=((C)p[-1])-1,bm=1<<b;
#endif
  HH h=(HH)(p+bm);
  RESETXCP;
  DO(bm--,p[i]=0);
  hfn= (I(*)(UC*,I))(!t?(PFI)ih:t==Ct?(PFI)ch:t==Ft?(PFI)  ffh:(PFI)eh);
  hfn1=(I(*)(UC*,I))(!t?(PFI)ih:t==Ct?(PFI)ch:t==Ft?(PFI)fct2h:(PFI)eh);

  /* construct hash table */
  for(a+=m*l;m--;h->i=m,p[j]=(I)(h++))
    a-=l,h->__h=(HH)p[j=bm&i_f0(b,(*hfn)(a,k))];

  /* use hash table. After a row is found index_of2 check the other
   * columns for the row that was found otherwise it is the same as
   * dyadic iota  "fnd()"
   */
#if 0
  DO(n,
     CHECKXCP;
     if(q)R 0;
     j = bm & i_f0(b, (*hfn1)(w,k) );
     for(h =   (HH) p[j];
	 h && ((*f)((I *)(a+l*h->i), (I *)w, k) || index_of2(h->i,aa,ww,i));
	 h =   h->__h);
     
     *r = h ? h->i:u;
     
     if(f == (I (*)(I *,I *,I))cmf && j != (m = bm & i_f0(b, fct1h((F *)w,k)))){
       for(h =  (HH) p[m];
	   h &&  (cmf((F *)(a+l*h->i), (F *)w, k) || index_of2(h->i,aa,ww,i));
	   h =  h->__h);
       if(h && h->i<*r) *r=h->i;
     }
     ++r;w += l);
#else
   
{ 
  I i=0, _i=(n) ;
  for(; i < _i ; ++i) 
    { 
      CHECKXCP;

      if(q) 
	return 0 ; 
      
      j=bm & i_f0(b, (*hfn1)(w, k)) ; 

      for(h=(HH) p [j] ; 
	  h && 
	    ((*f)((I *)(a + l *h->i), (I *)w, k) || 
	     index_of2(h->i, aa, ww, i)) ;
	   h=h->__h) ; 
      *r=h ? h->i : u ; 

      if(f ==(I(*)(I *, I *, I))cmf && 
	 j !=(m=bm & i_f0(b, fct1h((F *)w, k))) ) 
	{ 
	  for(h=(HH) p [m] ; 
	       h && (cmf((F *)(a + l *h->i), (F *) w, k) || 
		     index_of2(h->i, aa, ww, i)); 
	       h=h->__h) ; 
	  if(h && h->i < *r) 
	    *r=h->i ; 
	}
 
      ++r ; 
      w += l ; 
    } 
} ;

#endif



  R -1;
}

/*
 *  index_of() is called from ep_index_of() in index.c
 */
H2(index_of){ 
  A z,aa=a,ww=w;
  struct a ta,tw;
  a=(A)a->p[0],w=(A)w->p[0];
  memcpy(&ta,a,AH);
  memcpy(&tw,w,AH);

  /*make appear as a nx1 (tw&ta are copies)*/
  if(ta.r==0)
    {
      ta.d[0]=1;
      ta.d[1]=1;
    }
  else
    {
      ta.d[1]=1;
    }
  ta.r++;

  if(tw.r==0)
    {
      tw.d[0]=1;
      tw.d[1]=1;
    }
  else
    {
      tw.d[1]=1;
    }
  tw.r++;

 {
   I ar=ta.r,*ad=ta.d;I wt=tw.t,wr=tw.r,*wd=tw.d;
   u=ar?(--ar,*ad++):1;
   v=tr(ar,ad);
   wr-=ar;
   W(ga(It,wr,tr(wr,wd),wd));
   if(!u||!v) R zr(z);
   t=wt;
   a=aa,w=ww;
   *--Y=(I)z,tp=k_tm(u*4),++Y;
   index_of1(z->p,a,w,z->n);
 }
 R (I)z;
}

H2(mem){A z;ND2 X2 if(!(z=(A)fnd(w,a)))R 0;g=0;
	DO(z->n,z->p[i]=z->p[i]!=u)R(I)z;}

/* sun4/280  =(7,5) >(5) 
   f(27,20) m=0xFFFF0000 *f==*g?f[1]&m!=g[1]&m:f[1]&m&&g[1]&m&&... 
   NaN? remove order: ||t==  d<s     */
