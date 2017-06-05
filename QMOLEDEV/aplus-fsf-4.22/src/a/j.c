/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.
*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <a/ik.h>
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/arthur.h>

typedef struct{I n,*j,s;}HH;
typedef I (*G)(I,HH *);
Z I r,c[9],d[10],t,u,v,e;

#define J(f,t,x) Z void f(I p,HH *h){t *s=(t *)h->s;I *j=h->j;DO(h->n,x)h->s=(I)s;}

#define K(t,u,v,x,y) 	J(u,t,*s++=*(t *)p;p+=(I)j) J(v,t,*s++=*(t *)(p+*j++))\
	J(x,t,*(t *)p=*s;s+=r;p+=(I)j) J(y,t,*(t *)(p+*j++)=*s;s+=r)

K(I,i0,i1,i2,i3)
K(C,c0,c1,c2,c3)
K(F,j_f0,j_f1,f2,f3)

J(e0,I,*s++=ic((A)(*(I*)p));p+=(I)j)
J(e2,I,dc((A)(*(I*)p));*(I*)p=ic((A)(*s));s+=r;p+=(I)j) 
J(e1,I,*s++=ic((A)(*(I*)(p+*j++))))
J(e3,I,dc((A)(*(I*)(p+*j)));*(I*)(p+*j++)=ic((A)(*s));s+=r)

Z PFI f[][2][2]={
  (PFI)i0,(PFI)i1,(PFI)i2,(PFI)i3,
  (PFI)j_f0,(PFI)j_f1,(PFI)f2,(PFI)f3,
  (PFI)c0,(PFI)c1,(PFI)c2,(PFI)c3,
  0,0,0,0,
  (PFI)e0,(PFI)e1,(PFI)e2,(PFI)e3
  };
J(m0,I,(*(G)s)(p,h-1);p+=(I)j)
J(m1,I,(*(G)s)(p+*j++,h-1))

#define CK(n) if((unsigned long)(n)>=m)q=10;

/* Z I j_t2(A w){I *p=w->p,j=*p;DO(w->n-1,if(*++p!=++j)R 0)R 1;} */
Z I j_t2(A w)
{
  if(w->n>1)
    {
      I *p=w->p;
      I j=*p;
      I i=0,_i=w->n-1;
      for(;i<_i;++i)
	{
	  if(*++p!=++j)
	    R 0;
	}
    }
  R 1;
}

I xin(A a,I m,A z)
{
  A *w=(A*)Y+1;
  HH hh[9],*h=hh;
  C *p=(C*)a->p; 
  I t=a->t,ar=a->r,*ad=a->d,b=!z,l,u=0,i=0,j=0,k=0,n=1,d[9],y[9],*v,s=0;
  if(!m&&b)R ic(a);
  Q(m>ar,7);
  for(;i<ar;n*=j,++i)
    if(a=w[i],i>=m||qz(a)) {
	  j=d[k++]=ad[i],y[i]=0;
    } else
    {
      Q(!QA(a)||a->t&&!(a=ci(i+1)),6);
      j=a->r,mv(d+k,a->d,j),k+=j,j=a->n,y[i]=j==1?1:j_t2(a)?2:(u+=j,3);
    }
  if(b){Q(k>9,13)W(ga(t,k,n,d))} 
  else
  {
    if(r=z->n!=1){Q(k!=z->r,7)Q(cm(z->d,d,k),8)}
    if(!m)R tst(t,(I *)p,1,z->p,r,n),1;
  }
  if(!n)R(I)z;
#if (_MIPS_SZLONG == 64) || defined (__alpha) || defined(__sparcv9) || defined(__ia64) || defined(__x86_64)
  l=(((t>>1)&1)+3)&3,j=1<<l;
#else
  l=t+2&3,j=1<<l;
#endif
  v=k_tm(u);
  do
  {
    switch(a=w[--i],m=ad[i],u=y[i])
    {
      CS(0,for(;i&&!y[i-1];m*=ad[--i]);n=m);		/* elide  */
      CS(1,CK(k=*a->p)p+=j*k;if(i||s)continue;n=1);	/* single */
      CS(2,n=a->n; CK(k=*a->p)p+=j*k;CK(k+n-1));	/* iota   */
      CS(3,n=a->n; if(j!=1<<l)DO(n,CK(k=a->p[i])*v++=j*k) /* list   */
      else DO(n,CK(k=a->p[i])*v++=k<<l));
    }
    u=u==3,h->n=n,h->j=u?v-n:(I*)j,
    h++->s=s?s:(I)z->p,s=(I)(!s?(I)(f[t][!b][u]):(I)u?(I)m1:(I)m0);
  }
  while(j*=m,i);
  if(q){if(b)mf((I *)z);R 0;}
  R(*(G)s)((I)p,h-1),(I)z;
}

I xr(A z,A a,A w)
{
  I i,f=a->t==Et,n=f?a->n:1;
  if(f)for(i=n;i--;*--Y=ic((A)a->p[i]));
  *--Y=0,z=(A)xin(z,n,w),++Y;
  if(f)DO(n,dc((A)(*Y++)));
  R(I)z;
}

#define CI(T,f) G0(T,f){DO(n,Q((unsigned long)(n=*a++)>=e,10); *r++=w[n])R -1;}

CI(I,jj0)
CI(F,jj1)
CI(C,jj2)

G0(C,jj3)
{
  DO(u,Q((unsigned long)(n=*a++)>=e,10);
     tmv(t,(I *)(r+Tt(t,i*v)),(I *)(w+Tt(t,n*v)),v));
  R -1;
}
H2(ind)
{
  A z;ND2;
  if(a->t==Et)R xr(w,a,0);
  {
    I wt=w->t,wr=w->r,*wd=w->d;
    I1 Q(!wr,7)e=*wd;
    if(wr==1&&wt<Et){W(gd(wt,a))C2((!wt?(PFI)jj0:wt==1?(PFI)jj1:(PFI)jj2))}
    {
      I ar=a->r,an=a->n,*ad=a->d;t=wt,u=an;
      v=tr(--wr,++wd);
      Q(MAXR<ar+wr,13);
      W(ga(wt,ar+wr,an*v,ad));
      if(t==Et)zr(z);
      mv(z->d+ar,wd,wr);
      C2(jj3);
    }
  }
}

GV0(C,r2){I j=rm(v**a,n);tmv(t,(I *)tmv(t,(I *)r,(I *)(w+Tt(t,j)),n-j),(I *)w,j);}
GV0(C,r3)
{
  I j,k=Tt(t,1);
  n=u;
  DO(v,j=rm(*a++,n);
     tst(t,(I *)tst(t,(I *)r,v,(I *)(w+k*j*v),v,n-j),v,(I *)w,v,j);r+=k;w+=k);
}

H2(rot)
{
  A z;
  I *d,j,r;
  ND2;
  d=w->d,j=a->n!=1,r=j?a->r:w->r-1;
  I1 u=*d++,v=tr(r,d);
  if(0==w->r)R ic(w);
  if(j){Q(r!=w->r-1,7)Q(cm(a->d,d,r),8)}
  W(gd(t=w->t,w));
  if(!u)R(I)z;
  C2(((I(*)())(j?r3:r2)));
}

Z C *h(C *r,C *w,I j)
{
  I n=d[j],k=c[j];
  if(d[++j]<0)r=tst(t,(I *)r,1,(I *)w,k,n);
  else DO(n,r=h(r,w,j);w+=Tt(t,k));
  R r;
}

H2(dtr)
{
  A z;
  I k,r,i,n=1,m=0;
  unsigned long j;
  ND2;
  r=w->r;
  I1 Q(a->n!=r,7);
  DO(r+1,d[i]=-1);
  for(i=r;i--;n*=k)
  {
    j=a->p[i],k=w->d[i];
    Q(j>9,9);
    if(d[j]<0){if(c[j]=n,d[j]=k,j>m)m=j;}
    else if(c[j]+=n,--r,d[j]>k)d[j]=k;
  }
  Q(m>=r,9);
  W(ga(t=w->t,r,tr(r,(I *)d),(I *)d));
  R h((C *)z->p,(C *)w->p,0),(I)z;
}

#define GT(Ttype,f) Z I f(I b,Ttype *d,Ttype *s,I n){d+=n*b; \
					 DO(n,if(s[i]!=d[i])R s[i]>d[i])R 0;}
#define GB(Ttype,f) Z void f(I *r,Ttype *o,Ttype *p,I m,I n) \
{I b,l,h;DO(n,l=0;h=m; \
	    while(l<h)if(p[i]>o[b=(l+h)>>1])l=b+1;else h=b;r[i]=l)}

GB(I,b0)GB(F,b1)GT(I,bi)GT(F,bf)GT(UC,bc)

H2(bin)
{
  A z;
  ND2;
  if(!a->t&&w->t==Ft&&w->n==1&&ci(1))w=(A)Y[1];
  else{q=0;X2}
  {
    XA;
    I wr=w->r,wn=w->n,*wd=w->d;
    if(ar==1&&at<Ct)
    {
      W(ga(It,wr,wn,(I *)wd));
      (*(I(*)(I*,I*,I*,I,I))(at?(PFI)b1:(PFI)b0))(z->p,a->p,w->p,an,wn);
    }
    else
    {
      PFI f;
      I n,b,*r,u=ar?(--ar,*ad++):1,v=tr(ar,ad),t=at;
      C *p;
      Q(at>Ct,6)wr-=ar;
      Q(wr<0,7);
      Q(cm(ad,wd+wr,ar),8);
      W(ga(It,wr,n=tr(wr,wd),wd));
      r=z->p,p=(C*)w->p,f=!at?(PFI)bi:at==Ft?(PFI)bf:(PFI)bc;
      DO(n,
	 I l=0;I h=u;
	 while(l<h)
	 if((*(I(*)(I,I*,C*,I))f)(b=(l+h)>>1,a->p,p,v))l=b+1;
	 else h=b;r[i]=l;p+=Tt(t,v)
	 );
    }
    R(I)z;
  }
}
