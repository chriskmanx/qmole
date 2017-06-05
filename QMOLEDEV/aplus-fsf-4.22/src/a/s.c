/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <limits.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <a/ik.h>
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/fir.h>
#include <a/arthur.h>
#if !defined(BITSPERBYTE)
#define BITSPERBYTE 8
#endif
#if !defined(BITS)
#define BITS(x) (BITSPERBYTE * sizeof(x))
#endif

#if defined(_AIX)
fpflag_t fpe_bits=0;
int nan_bits=0;
int fp_checkerr(C *s)
{
  H("%s:",s);
  H("FP_INEXACT:%d ",fpe_bits&FP_INEXACT ? 1:0);     /* inexact result      */
  H("FP_DIV_BY_ZERO:%d ",fpe_bits&FP_DIV_BY_ZERO ? 1:0); /* divide by zero  */
  H("FP_UNDERFLOW:%d ",fpe_bits&FP_UNDERFLOW ? 1:0); /* underflow exception */
  H("FP_OVERFLOW:%d ",fpe_bits&FP_OVERFLOW ? 1:0);   /* overflow exception  */
  H("FP_INVALID:%d" ,fpe_bits&FP_INVALID ? 1:0);     /* invalid operation   */
  NL;
  return 0;
}
#elif defined(linux) || defined(_HP) || defined(__APPLE__)
fexcept_t fpe_bits=0;
int nan_bits=0;
int fp_checkerr(s)C *s;
{
  H("%s:",s);
#if !defined(__arm__)
  H("FE_INEXACT:%d ",fpe_bits&FE_INEXACT ? 1:0);     /* inexact result      */
#endif
  H("FE_DIVBYZERO:%d ",fpe_bits&FE_DIVBYZERO ? 1:0); /* division by zero    */
  H("FE_UNDERFLOW:%d ",fpe_bits&FE_UNDERFLOW ? 1:0); /* underflow exception */
  H("FE_OVERFLOW:%d ",fpe_bits&FE_OVERFLOW ? 1:0);   /* overflow exception  */
  H("FE_INVALID:%d" ,fpe_bits&FE_INVALID ? 1:0);     /* invalid operation   */
  NL;
  return 0;
}
#elif defined(__osf__)
unsigned long fpe_bits=0;
int nan_bits=0;
int fp_checkerr(s)C *s;
{
  H("%s:",s);
  H("IEEE_STATUS_UNF:%d ",fpe_bits&IEEE_STATUS_UNF ? 1:0); /* underflow      */
  H("IEEE_STATUS_OVF:%d ",fpe_bits&IEEE_STATUS_OVF ? 1:0); /* overflow       */
  H("IEEE_STATUS_DZE:%d ",fpe_bits&IEEE_STATUS_DZE ? 1:0); /* divide-by-zero */
  H("IEEE_STATUS_INV:%d" ,fpe_bits&IEEE_STATUS_INV ? 1:0); /* invalid        */
  NL;
  return 0;
}
#elif defined(HAVE_SVR4) || defined(__FreeBSD__) || defined(__NetBSD__)
fp_except fpe_bits=0;
int nan_bits=0;
int fp_checkerr(s)C *s;
{
  H("%s:",s);
  H("FP_X_IMP:%d ",fpe_bits&FP_X_IMP ? 1:0); /* imprecise (loss of precision)*/
  H("FP_X_UFL:%d ",fpe_bits&FP_X_UFL ? 1:0); /* underflow exception          */
  H("FP_X_OFL:%d ",fpe_bits&FP_X_OFL ? 1:0); /* overflow exception           */
  H("FP_X_DZ:%d " ,fpe_bits&FP_X_DZ  ? 1:0); /* divide-by-zero exception     */
  H("FP_X_INV:%d" ,fpe_bits&FP_X_INV ? 1:0); /* invalid operation exception  */
  NL;
  return 0;
}
#else
int fp_anyerr(void ){R 0;}
void fp_checkerr(void ){H("no fpe on Sun\n");}
#endif

Z I II[]={1,0,0,1,~0x7FFFFFFF,0x7FFFFFFF};
Z F FI[]={0,0,0,1,-123.0,123.0}; /* FI[4 5] filled in by infi(). */
F aplusInfinity=-1.0;
#if defined(__osf__)
int infi(void){FI[4]=-(FI[5]=aplusInfinity=DBL_INFINITY); R 0;}
#else
int infi(void){FI[4]=-(FI[5]=aplusInfinity=HUGE_VAL); R 0;}
#endif

#define DD(f,u,v,x) Z void f(u *r,v *a,v *w,I n) \
                   {u *t=r+n;I i=aw!=1,j=aw!=2; RESETXCP; \
                    for(;r<t;a+=i,w+=j)*r++= x;CHECKXCP;}

#define DDidx(f,u,v,x) Z void f(u *r,v *a,v *w,I n) \
                   {u *t=r+n;I i=aw!=1,j=aw!=2; I k; RESETXCP; \
                    for(k=0;k<n;k++,i+=i,j+=j) r[k]= x;CHECKXCP;}

#ifdef HPC_PWR2
#define DDHpc(f,u,v,x) Z void f(r,a,w,n)u *r;v *a,*w;I n; \
                   {RESETXCP; \
                   switch(aw){ \
                   case 0: hpc_vv##f(r,a,w,n); break;\
                   case 1: hpc_sv##f(r,a,w,n); break; \
                   case 2: hpc_vs##f(r,a,w,n); break; \
                   default: break; \
                   }; \
                   CHECKXCP;}
#define DDHpcNE(f,u,v,x) Z void f(r,a,w,n)u *r;v *a,*w;I n; \
                   {/*RESETXCP*/; \
                   switch(aw){ \
                   case 0: hpc_vv##f(r,a,w,n); break;\
                   case 1: hpc_sv##f(r,a,w,n); break; \
                   case 2: hpc_vs##f(r,a,w,n); break; \
                   default: break; \
                   }; \
                   /*CHECKXCP*/;}
#else
#define DDHpc(f,u,v,x) DD(f,u,v,x)
#define DDHpcNE(f,u,v,x) DD(f,u,v,x)
#endif
#define NANDD(f,u,v,x) Z void f(u *r,v *a,v *w,I n) \
                   {u *t=r+n;I i=aw!=1,j=aw!=2; RESETXCP; \
		    for(;r<t;a+=i,w+=j)NAN_CHECK(*r++, x)CHECKXCP;}
#ifdef INDEX_PRIMITIVES
  #define Di(x,f) DDidx(f,I,I,a[i] x w[j])
#else
  #define Di(x,f) DD(f,I,I,*a x *w)
#endif

#define DfHpc(x,f) DDHpc(f,F,F,*a x *w)
#define DfHpcNegZ(x,f) DDHpc(f,F,F,(*w?*a x *w:*a x 0.0))
#define DxHpc(x,f) DDHpcNE(f,I,F,*a x *w)

#define Bw(x,f) Z unsigned long f(unsigned long a,unsigned long w) \
                 {unsigned long t=1,i=0,r=0,_i=BITS(a); \
		  for(;i<_i;i++,a>>=1,w>>=1,t<<=1) \
		    if((a&1) x (w&1)) r|= t; \
		  R r;}



/* extern F pif(),floor(),log(),exp(),pow();I aw;Z(*f)(),t,u,v; */
I aw;Z PFI f;Z I t,u,v;
Z F fl(F x)
{
  F y=floor(.5+x);
  R y-x>CT*(y>1?y:y<-1?-y:1.0)?y-1:y;
}
Z F frm(F x,F y){F z;R!y?x:(z=fl(x/y))==-fl(-x/y)?0.0:x-z*y;}
I rm(I a,I b){I r;R a<b&&a>=0?a:!b?a:(r=a%b,b<0&&r>0||r<0&&b>0?r+b:r);}

#if defined(_ULTRA_SPARC_VECTOR_OPS) 
/* !! BUG !! !! BUG !! !! BUG !! !! BUG !! !! BUG !! */
/* vlog_ incorrectly sets FPE bits for INF           */
/* Once this bug is fixed we only need to check the  */
/* FPE flags once at the end of the primitive        */
/* For now we need to check each item of the array   */
/* to see if it really is a NaN or just Inf          */
/* !! BUG !! !! BUG !! !! BUG !! !! BUG !! !! BUG !! */
#define TEMPORARY_VLOG_BUG_WORKAROUND(xx)  \
  { int i=0,resetNeeded=0; \
    for(;i<n;i++) \
      if( !finite(xx[i]) ) { \
	resetNeeded=1; \
	if( isnan(xx[i]) ) {resetNeeded=0;break;} \
      } \
    if(resetNeeded) { RESETXCP; log(100.0); } }
#endif

#if defined(_ULTRA_SPARC_VECTOR_OPS) 
static s_pi ( r, a, w, n) 
F *r; 
F *a , *w; 
I n; 
{ 
  F *t=r+n;
  I i=aw!=1, j=aw!=2;

  if( n==0 ) return;		/* Empty result */

  RESETXCP; /* NOTEIT: add  RESETXCP */

  if( !i && ( *a==1 || *a==2) )
    {
      int incx=1, incy=1;
      if( *a==1 )		/* sin */
	vsin_(&n, w, &incx, r, &incy);
      else			/* cos */
	vcos_(&n, w, &incx, r, &incy);
    }
  else
    for( ; r<t; a+=i, w+=j) 
      *r++=pif((int)*a, *w);

  CHECKXCP; /* NOTEIT: add  NAN_CHECK and CHECKXCP */
}
#else
NANDD(s_pi,F,F,pif((I)*a,*w))
#endif

DD(i6,I,I,rm(*w,*a))
NANDD(f6,F,F,frm(*w,*a))
/*
DDHpcNE(h2,I,F,!ne(a,w))
*/
Z void h2(I *r,F *a,F *w,I n) {
  I *t=r+n;
  I i=aw!=1,j=aw!=2;
  RESETXCP;
  for(;r<t;a+=i,w+=j) {
    *r++= !ne(a,w);
    CHECKXCP;
  }
}

DDHpcNE(h3,I,F,ne(a,w))


#if defined(_ULTRA_SPARC_VECTOR_OPS) 
static lg ( r, a, w, n ) 
F * r; 
F *a , *w ; 
I n; 
{ 
  I i=aw!=1, j=aw!=2;
  F *wprime;
  F *aprime;
  int incx=j, incy=1, nx=n;

  if( n==0 ) return;		/* Empty result */

  RESETXCP; /* NOTEIT: add  RESETXCP */

  if( !(wprime=(F *) malloc(sizeof(F) * n)) ) 
    { q=ERR_INVALID;return;} /* error on malloc */
  if( !(aprime=(F *) malloc(sizeof(F) * n)) ) 
    { free(wprime);q=ERR_INVALID;return;} /* error on malloc */

  vlog_(&nx, w, &incx, wprime, &incy);
  /* !! BUG !! Temporary workaround !! BUG !! */
  TEMPORARY_VLOG_BUG_WORKAROUND(wprime);    

  CHECKXCP;                                 /**/
  if(q) {free(wprime);free(aprime);return;} /**/
  /* !! BUG !! Temporary workaround !! BUG !! */

  incx = i;
  incy = 1;
  vlog_(&nx, a, &incx, aprime, &incy);
  /* !! BUG !! Temporary workaround !! BUG !! */
  TEMPORARY_VLOG_BUG_WORKAROUND(aprime);    

  CHECKXCP;                                 /**/
  if(q) {free(wprime);free(aprime);return;} /**/
  /* !! BUG !! Temporary workaround !! BUG !! */

  for (nx = 0; nx < n; nx++)
    *r++ = wprime[nx] / aprime[nx];  

  CHECKXCP; /* NOTEIT: add  CHECKXCP  */

  free(wprime);
  free(aprime);
}
#else
NANDD(lg,F,F,log(*w)/log(*a))
#endif

#if defined(_ULTRA_SPARC_VECTOR_OPS)
static I ex ( r, a, w, n) 
F *r, *a, *w; 
I n ;
{
  I k=aw!=1,j=aw!=2;
  
  if( n==0 ) return;		/* Empty result */

  RESETXCP; /* NOTEIT: add  RESETXCP */

  if(!k&&*a>0){
    F t;
    t=log ( *a ) ;
    CHECKXCP; /* NOTEIT: add  NAN_CHECK and CHECKXCP  */
    { 
      I i=0;
      F *wprime;
      int incx=1, incy=1;

      if(!(wprime=(F *) malloc(n * sizeof(F)))  ) 
	{ q=ERR_INVALID;return;} /* error on malloc */
	
      for (i = 0; i < n; i++)
	wprime[i] = w[i] * t;
      
      vexp_(&n, wprime, &incx, r, &incy);

      CHECKXCP; /* NOTEIT: add  NAN_CHECK and CHECKXCP  */

      free(wprime);
      return;
    }
  }
  { 
    I i=0, i2=0;
    int incx = k, incy = 1;
    F *wprime;

    /* Special case for negative numbers in a ie. -1*0 1 2 3 */
    if( k==0 && 0>=*a ) {
      DO(n,NAN_CHECK(*r++, *a>0?exp(*w*log(*a)):pow(*a,*w));a+=k;w+=j);
      CHECKXCP;
      return;
    } else {
      for(i=0; i<n; i++)
	if( 0>=a[i] ) {
	  DO(n,NAN_CHECK(*r++, *a>0?exp(*w*log(*a)):pow(*a,*w));a+=k;w+=j);
	  CHECKXCP;
	  return;
	}
    }

    if(!(wprime=(F *) malloc(n * sizeof(F))) ) { 
      q=ERR_INVALID;return;	/*  error on malloc */
    }

    vlog_(&n, a, &incx, wprime, &incy);
    /* !! BUG !! Temporary workaround !! BUG !! */
    TEMPORARY_VLOG_BUG_WORKAROUND(wprime);    /**/
    CHECKXCP;                                 /**/
    if(q) {free(wprime);return;}              /**/
    /* !! BUG !! Temporary workaround !! BUG !! */

    for (i=0, i2=0; i<n; i++, i2+=j)
	wprime[i] *= w[i2];

    incx=incy=1;
    vexp_(&n, wprime, &incx, r, &incy);
    free(wprime);

    CHECKXCP;			/* NOTEIT: add NAN_CHECK */

    return;
  } 
}
#else
GV2(F,ex){I k=aw!=1,j=aw!=2;RESETXCP;
 if(!k&&*a>0){F t;NAN_CHECK(t,log(*a))
		DO(n,NAN_CHECK(*r++,exp(t**w++)))CHECKXCP;R;}
 DO(n,NAN_CHECK(*r++, *a>0?exp(*w*log(*a)):pow(*a,*w));a+=k;w+=j)CHECKXCP;}
#endif

/*DD(ex,F,F,*a>0?exp(*w*log(*a)):pow(*a,*w))*/
DxHpc(<(*w>0?CT1:CT2)*,h0)
DxHpc(>(*w>0?CT2:CT1)*,h1)
DxHpc(<=(*w>0?CT2:CT1)*,h4)
DxHpc(>=(*w>0?CT1:CT2)*,h5)
Di(>*w?*a:,i2)
DfHpc(>*w?*a:,f2)
Di(<*w?*a:,i3)
DfHpc(<*w?*a:,f3)

#ifdef INDEX_PRIMITIVES
  DDidx(ne0,I,I,0)
  DDidx(ne1,I,I,1)
  DDidx(c0,I,C,a[i]==w[j])
  DDidx(c1,I,C,a[i]!=w[j])
  DDidx(c2,I,C,a[i]<w[j])
  DDidx(c3,I,C,a[i]>w[j])
  DDidx(c4,I,C,a[i]<=w[j])
  DDidx(c5,I,C,a[i]>=w[j])
  DDidx(e0,I,I,!mt((A)a[i],(A)w[j]))
  DDidx(e1,I,I,!!mt((A)a[i],(A)w[j]))
  Z I sc2(S a,S w){ R strcmp(a->n, w->n) <  0; }  
  Z I sc3(S a,S w){ R strcmp(a->n, w->n) >  0; }  
  Z I sc4(S a,S w){ R strcmp(a->n, w->n) <= 0; }  
  Z I sc5(S a,S w){ R strcmp(a->n, w->n) >= 0; }  
  DDidx(e2,I,I,sc2(XS(a[i]),XS(w[j])))
  DDidx(e3,I,I,sc3(XS(a[i]),XS(w[j])))
  DDidx(e4,I,I,sc4(XS(a[i]),XS(w[j])))
  DDidx(e5,I,I,sc5(XS(a[i]),XS(w[j])))
#else
  DD(ne0,I,I,0) 
  DD(ne1,I,I,1)
  DD(c0,I,C,*a==*w)
  DD(c1,I,C,*a!=*w)
  DD(c2,I,C,*a<*w)
  DD(c3,I,C,*a>*w)
  DD(c4,I,C,*a<=*w)
  DD(c5,I,C,*a>=*w)
  DD(e0,I,I,!mt((A)*a,(A)*w))
  DD(e1,I,I,!!mt((A)*a,(A)*w))
  Z I sc2(S a,S w){ R strcmp(a->n, w->n) <  0; }  
  Z I sc3(S a,S w){ R strcmp(a->n, w->n) >  0; }  
  Z I sc4(S a,S w){ R strcmp(a->n, w->n) <= 0; }  
  Z I sc5(S a,S w){ R strcmp(a->n, w->n) >= 0; }  
  DD(e2,I,I,sc2(XS(*a),XS(*w)))
  DD(e3,I,I,sc3(XS(*a),XS(*w)))
  DD(e4,I,I,sc4(XS(*a),XS(*w)))
  DD(e5,I,I,sc5(XS(*a),XS(*w)))
#endif

Di(+,i0)
DfHpc(+,s_f0)
Di(*,i1)
DfHpc(*,s_f1)
Di(-,i4)
DfHpc(-,f4)
Di(/,i5)
DfHpcNegZ(/,f5)
Di(<,jj0)
Di(>,jj1)
Di(==,jj2)
Di(!=,jj3)
Di(<=,jj4)
Di(>=,jj5)

#ifdef INDEX_PRIMITIVES
  DDidx(z0,I,I,(a[i]&&w[j])?1:0) /* Di(&,z0) */
  DDidx(z1,I,I,(a[i]||w[j])?1:0) /* Di(|,z1) */
#else
  DD(z0,I,I,(*a&&*w)?1:0) /* Di(&,z0) */
  DD(z1,I,I,(*a||*w)?1:0) /* Di(|,z1) */
#endif

Di(&, bw0)
Di(|, bw1)
Bw(<, bx2)
DD(   bw2,I,I,bx2(*a,*w))
Bw(>, bx3)
DD(   bw3,I,I,bx3(*a,*w))
Bw(==,bx4)
DD(   bw4,I,I,bx4(*a,*w))
Di(^, bw5)
Bw(<=,bx6) 
DD(   bw6,I,I,bx6(*a,*w))
Bw(>=,bx7)  
DD(   bw7,I,I,bx7(*a,*w))
	              /* Integer      Float         i      (i is for FF) */
PFI df[][2]={  (PFI)z0,      0,         /*  0 ^ */
	       (PFI)z1,      0,  	/*  1 © */
	       (PFI)i0, (PFI)s_f0,	/*  2 + */
               (PFI)i1, (PFI)s_f1,	/*  3 « */
               (PFI)i2, (PFI)f2,	/*  4 Ó */
               (PFI)i3, (PFI)f3,	/*  5 Ä */
               (PFI)i4, (PFI)f4,	/*  6 - */
               (PFI)i5, (PFI)f5,	/*  7 ß */
               (PFI)i6, (PFI)f6,	/*  8 | */
               (PFI)jj0,(PFI)h0,	/*  9 < */
               (PFI)jj1,(PFI)h1,	/* 10 > */
               (PFI)jj2,(PFI)h2,	/* 11 = */
               (PFI)jj3,(PFI)h3,	/* 12 ¨ */
               (PFI)jj4,(PFI)h4,	/* 13 ¤ */
               (PFI)jj5,(PFI)h5,	/* 14 ¦ */
                          0,(PFI)ex,	/* 15 * */
                          0,(PFI)lg,	/* 16 ð */
                          0,(PFI)s_pi};	/* 17 Ï */

PFI dde[]={(PFI)e2,(PFI)e3,(PFI)e0,(PFI)e1,(PFI)e4,(PFI)e5};
PFI ddc[]={(PFI)c2,(PFI)c3,(PFI)c0,(PFI)c1,(PFI)c4,(PFI)c5};
PFI ddne[]={(PFI)ne0,(PFI)ne1};
PFI ddbw[]={(PFI)bw0,(PFI)bw1,(PFI)bw2,(PFI)bw3,(PFI)bw4,(PFI)bw5,(PFI)bw6,(PFI)bw7};
/* #define X0 Q(Xt<=wt||(j=wt>Ft)&&w->n&&i!=11&&i!=12,6) */
#define X0 Q(Xt<=wt||(j=wt>Ft||a->t>Ft)&&w->n&&(i<9||i>14),6)
#define X2_1 {if(a->t!=w->t){if(a->t+w->t==1)F2 else if(!a->n)a=(A)cn(0,w->t);\
              else if(!w->n)w=(A)cn(1,a->t);\
	      else { diff_t=1; if (i!=11 && i!=12) R q=6,0;}}\
              else if(w->t==Et && etNotOk(a,w,i) ) R q=6, 0;}
/* if(z->c==1&&t==wt)ic(z) can't reuse when possible errors */

#define FF !j?df[i][wt]:\
           (i<9||i>14)?\
              (i&1?(wt==Et?(PFI)e0:\
                   (PFI)c0):wt==Et?(PFI)e1:(PFI)c1):\
           (diff_t)?ddne[i-11]:\
           (wt==Et)?dde[i-9]:ddc[i-9]
Z I etNotOk(A a,A w,I i){
switch(i){
 case 9: case 10: case 13: case 14:
   DO(a->n,if(!QS(a->p[i])) R 1)
   DO(w->n,if(!QS(w->p[i])) R 1)
 break; default:
 break;
 }
 R 0;
}
I bs(A a,A w,I i){A z;ND2
 if(a->t==Et) R bwcv(a,w);
 Q(a->t!=w->t||a->t!=It,ERR_TYPE)
 if(aw=a->n==1&&(w->n!=1||w->r>=a->r))z=w;else if(w->n==1)aw=2,z=a;
 else{Q(a->r!=w->r,ERR_RANK)Q(cm(a->d,w->d,a->r),ERR_LENGTH)z=a->c>1?w:a;}
 W(gd(It,z))
 C2(ddbw[i-76])
}
I ds(A a,A w,I i){A z;I t,wt,j=0,diff_t=0;ND2
 if(i==7||i>14)F2 else if(i<2)I2 else X2_1 
 wt=w->t;X0 
 if(aw=a->n==1&&(w->n!=1||w->r>=a->r))z=w;else if(w->n==1)aw=2,z=a;
  else{Q(a->r!=w->r,7)Q(cm(a->d,w->d,a->r),8)z=a->c>1?w:a;}
 t=i<9?wt:i<15?It:Ft; W(gd(t,z))
 C2((unsigned long)(FF))
}
I os(A a,A w,I i)
{
  A z;
  I j=0,diff_t=0,n;
  i-=50;
  ND2;
  if(i>14||i==7)F2 else X2_1
  {
    PFI f;
    XA;
    XW,t,r=ar+wr;
    C *p,*ap;
    X0 Q(r>MAXR,13)
      OF(n,wn,an)W(ga(t=i<9||i>14?wt:It,wr+ar,n,ad))
	mv(z->d+ar,wd,wr);
    p=(C*)z->p;f=(PFI)(FF);
    aw=1,ap=(C*)a->p;
    DO(an,(*(I(*)(C*,C*,I*,I))f)(p,ap,w->p,wn);
       ap+=Tt(at,1);
       p+=Tt(t,wn));
    R(I)z;
  }
}
#define IP(f,x,y) GV2(F,f){I k=v;F *ap=a,*wp=w,s,t;DO(u,DO(k,a=ap;w=wp++;t=x;DO(aw,y;++a;w+=k)*r++=t)ap+=aw;wp-=k)}
IP(x4,FI[4],if(t<(s=*a+*w))t=s)
IP(x5,FI[5],if(t>(s=*a+*w))t=s)
#ifdef HPC_PWR2
GV2(F,dot){F *t=a+aw,s=0.0;hpc_dotproduct(r,a,w,aw);}
#else
GV2(F,dot){F *t=a+aw,s=0.0;for(;a<t;)s+=*a++**w++;*r=s;}
#endif
Z void m2(F *r,F *x,F *y,I l,I n){F *p=x+l,*q=y+1,a,b,c,d;a=b=c=d=0;
 DO(l,a+=*x**y;b+=*x++**q;c+=*p**y;y+=n;d+=*p++**q;q+=n)
 *r=a,r[1]=b,r[n]=c,r[n+1]=d;}
Z void m1(F *r,F *x,F *y,I l,I n){F *q=y+1,a,b;a=b=0;
 DO(l,a+=*x**y;y+=n;b+=*x++**q;q+=n)*r=a,r[1]=b;}
Z void m0(F *r,F *x,F *y,I l,I n){F *p=x+l,a,c;a=c=0;
 DO(l,a+=*x++**y;c+=*p++**y;y+=n)*r=a,r[n]=c;}
G2(F,mmu){
  I m=u,l=aw;n=v; RESETXCP;
  DO(m>>1,
     DO(n>>1,m2(r,a,w,l,n);w+=2;r+=2)
     if(n&1)m0(r++,a,w++,l,n);
     w-=n;a+=2*l;r+=n);
  CHECKXCP;if(q)R 0;
  if(m&1){
    DO(n>>1,m1(r,a,w,l,n);w+=2;r+=2);
    if(n&1){*r=0;DO(l,*r+=*a++**w;w+=n)}
  }
  CHECKXCP;
  R -1;
}
I is(A a,A w,I i){A z;i-=64;ND2 F2
 {I ar=a->r,*ad=a->d;I wt=w->t,wr=w->r,*wd=w->d,r,n;aw=*wd;Q(!ar||!wr,7)Q(ad[--ar]!=aw,8)
  if(i==2&&!ar&&wr==1){W(gs(Ft))C2(dot)}  u=tr(ar,ad),v=tr(--wr,++wd),r=ar+wr;
  Q(r>MAXR,13)OF(n,u,v)W(ga(wt,r,n,ad))mv(z->d+ar,wd,wr);
  C2((i==2?(PFI)mmu:i==4?(PFI)x4:(PFI)x5))}}
#define RG(q,f,x,y) Z void f(q *r,q *w,I n){q s= x,*t=w+n;RESETXCP; \
					 for(;w<t;++w)y;*r=s;CHECKXCP;}
#ifdef HPC_PWR2
#define RGHpc(q,f,x,y,fHpc) Z void f(q *r,q *w,I n){RESETXCP; \
					 fHpc(r,w,n);CHECKXCP;}
#else 
#define RGHpc(q,f,x,y,fHpc) RG(q,f,x,y)
#endif
RG(I,b0,1,if(!*w){*r=0;R;})		
RG(I,b1,0,if(*w){*r=1;R;})
RG(I,r0,0,s+= *w)
RGHpc(F,q0,0.0,s+= *w,hpc_vrsf0) /* sum of all elements */
RG(I,r1,1,s*= *w)
RGHpc(F,q1,1.0,s*= *w,hpc_vrsf1) /* product of all elements */
RG(I,r2,II[4],if(s<*w)s= *w)
RGHpc(F,q2,FI[4],if(s<*w)s= *w ,hpc_vrsf2)
RG(I,r3,II[5],if(s>*w)s= *w)
RGHpc(F,q3,FI[5],if(s>*w)s= *w ,hpc_vrsf3)

PFI fr[][2]={(PFI)b0,0,(PFI)b1,0,(PFI)r0,(PFI)q0,(PFI)r1,(PFI)q1,(PFI)r2,(PFI)q2,(PFI)r3,(PFI)q3};

GV1(I,s0){*r= *w++;(*(I(*)(I*,I*,I*,I))f)(r+1,r,w,n-1);}
GV1(F,s1){*r= *w++;(*(I(*)(F*,F*,F*,I))f)(r+1,r,w,n-1);}	
#ifdef HPC_PWR2
static void s1Scan(F *r,F *w,I n,I i){
 switch(i){ 
  case 2: hpc_vscf0(r,w,n);break;
  case 3: hpc_vscf1(r,w,n);break;
  case 4: hpc_vscf2(r,w,n);break;
  case 5: hpc_vscf3(r,w,n);break;
  };}
#endif
GV1(C,s3)
{
  I k=Tt(t,n=v);
  tmv(t,(I *)r,(I *)w,n);
  DO(u-1,(*(I(*)(C*,C*,C*,I))f)(r+k,r,w+=k,n);
     r+=k)
}
GV1(C,rr)
{
  I k=Tt(t,n=v);
  tmv(t,(I *)r,(I *)w,n);
  RESETXCP;
  DO(u-1,(*(I(*)(C*,C*,C*,I))f)(r,r,w+=k,n));
  CHECKXCP;
}
I rs(A a,I i){A z;i-=46;ND1 if(i<2)I1 else X1{I at=a->t,ar=a->r,*ad=a->d;
 if(ar){u=*ad++;if(!u&&(i==4||i==5))at=Ft;--ar;}
 if(!ar){W(gs(at))C1(fr[i][at])}W(ga(at,ar,v=tr(ar,ad),ad))
 if(!u)R trp(at,z->p,at?(I*)(FI+i):II+i,v),(I)z;f=df[i][t=at],aw=0;C1(rr)}}

#ifdef HPC_PWR2
I sc(A a,I i){A z;i-=40;ND1 if(i<2)I1 else X1{XA;if(!ar||!an)R ic(a);
 W(ga(at,ar,an,ad))f=df[i][at],aw=0;
 if((i>=2)&&(at==1)&&(ar==1)) {
       s1Scan((F *)z->p,(F *)a->p,a->n, i);return(I)z;}
 else {  
  C1(ar>1?(t=at,u= *ad,v=tr(ar-1,ad+1),(PFI)s3):at?(PFI)s1:(PFI)s0)}}
	    }
#else
I sc(A a,I i){A z;i-=40;ND1 if(i<2)I1 else X1{XA;if(!ar||!an)R ic(a);
 W(ga(at,ar,an,ad))f=df[i][at],aw=0;
 C1(ar>1?(t=at,u= *ad,v=tr(ar-1,ad+1),(PFI)s3):at?(PFI)s1:(PFI)s0)}}
#endif

#ifdef __lucid
G2(I,p0)
 {
  I *p=r,s;
  F fp;
  DO(n,*p++=*w++)
  DO(u-1,s=*(a+=v);
         p=r;
         DO(n,fp=((F)*p)*((F)s)+((F)*w++);
              Q(fp>(F)(LONG_MAX)||fp<(-1.0*fabs((F)(LONG_MIN))),-2)
              *p++=(I)fp
           )
    )
  R -1;
 }
#else
G2(I,p0)
 {
  I *p=r,s;
  F fp;
  DO(n,*p++=*w++)
  DO(u-1,s=*(a+=v);
         p=r;
         DO(n,fp=((F)*p)*((F)s)+((F)*w++);
              Q(fp>(F)(LONG_MAX)||fp<(F)(LONG_MIN),-2)
              *p++=(I)fp
           )
    )
  R -1;
 }
#endif
GV2(F,p1)
 {
  F *p=r,s;
  RESETXCP;
  DO(n,*p++=*w++)
  DO(u-1,s=*(a+=v);
         p=r;
         DO(n,*p=*p*s+*w++;
              p++
           )
    )
  CHECKXCP;
 }

H2(ncd){A z;I n;ND2 n=a->n;if(a->t||w->t)F2{
 I wt=w->t,wr=w->r,*wd=w->d;Q(!wr--||a->r>1,7)u=*wd++;
 Q(n!=1&&n!=u,8)v=n>1;W(ga(wt,wr,tr(wr,wd),wd))if(!u)R zr(z);C2(wt?(PFI)p1:(PFI)p0)}}

GV2(I,o0){I s;DO(v,s=*w++;r+=n;a+=u;DO(u,r-=v;*r=rm(s,*--a);s=*a?(s-*r)/ *a:0)r++)}
GV2(F,o1){F s;RESETXCP;DO(v,s=*w++;r+=n;a+=u;DO(u,r-=v;*r=frm(s,*--a);s=*a?(s-*r)/ *a:0.0)r++)CHECKXCP;}
H2(dcd){A z;ND2 if(a->t||w->t)F2{I ar=a->r,an=a->n;XW;u=an,v=wn;Q(ar>1,7)W(ga(wt,wr+ar,u*v,wd-ar))if(ar)*z->d=u;if(!u)R(I)z;C2(wt?(PFI)o1:(PFI)o0)}}

#ifndef TRUE 
#define TRUE (1)
#endif

#ifndef FALSE 
#define FALSE (0)
#endif

#define MF(u,v,x,qReset) { \
    u *r=(u*)z->p,*t=r+a->n; \
    v *w=(v*)a->p; \
    RESETXCP; \
    for(;r<t;++w)*r++= x; \
    CHECKXCP; \
    if(qReset==TRUE) q=0; \
    R(I)z;}
#define NANMF(u,v,x) { \
   u *r=(u*)z->p,*t=r+a->n; \
   v *w=(v*)a->p;RESETXCP; \
   for(;r<t;++w) NAN_CHECK(*r++, x)CHECKXCP; \
   R(I)z;}
#define NANMFhPC(u,v,x) { \
   u *r=(u*)z->p,*t=r+a->n; \
   v *w=(v*)a->p; \
   /*RESETXCP*/; \
   x; \
   /*CHECKXCP*/; \
   R(I)z;}
#define MT(Ttype) if(Ttype&&a->c==1)++(z=a)->c;else W(gd(Ttype?a->t:It,a))
H1(neg){ A z; ND1 
if(a->t==It) {              
  I *w=(I *)a->p, *t=w+a->n;
  for(;w<t;++w)
    if (LONG_MIN==*w)
      if ((a=ep_cf(0))) break;
      else  return 0;
  if(a->t==It) { MT(1) MF(I,I,-*w,FALSE) }
} F1 MT(1) MF(F,F,-*w,FALSE) }
H1(aab){A z;ND1
if(a->t==It) {
  I *w=(I *)a->p, *t=w+a->n;
  for(;w<t;++w)
    if (LONG_MIN==*w)
      if ((a=ep_cf(0))) break;
      else  return 0;
  if(a->t==It) { MT(1)  MF(I,I,*w<0?-*w:*w,FALSE) } 
} F1 MT(1) MF(F,F,*w<0?-*w:*w,FALSE) }
H1(sgn){A z;ND1 X1 MT(0)if(a->t==It)MF(I,I,*w<0?-1:*w>0,FALSE)else MF(I,F,*w<0?-1:*w>0,FALSE)}
H1(not){A z;ND1 I1 MT(1)MF(I,I,!*w,FALSE)}
H1(bwnot){A z;ND1 I1 MT(1)MF(I,I,~*w,FALSE)}
/* H1(cln){A z;ND1 F1 MT(1)MF(F,F,-fl(-*w),FALSE)}  */
H1(rec){A z;ND1 F1 MT(1)MF(F,F,(*w?1.0/ *w:aplusInfinity),FALSE)}
#ifdef HPC_PWR2
H1(aln){A z;ND1 F1 MT(1)NANMFhPC(F,F,hpc_InitNatLog(); hpc_mAlnFF(r,w,a->n))}
H1(aen){A z;ND1 F1 MT(1)NANMFhPC(F,F,hpc_InitExponential(); hpc_mAenFF(z->p,a->p,a->n))}
#else

#if defined(_ULTRA_SPARC_VECTOR_OPS)
I aln (a) 
A a;
{
  A z; 
  ND1 F1 MT(1)

  { 
    F *r=(F *)z->p;
    F *w=(F *)a->p;
    int incx=1, incy=1, n=a->n;
    
    
    if( n>0 )
      {
	/* NOTEIT: add  RESETXCP */
	RESETXCP;

	vlog_(&n, w, &incx, r, &incy);
	/* !! BUG !! Temporary workaround !! BUG !! */
	TEMPORARY_VLOG_BUG_WORKAROUND(r);      /* */
	/* !! BUG !! Temporary workaround !! BUG !! */
	/* NOTEIT: add NAN_CHECK */ 
    
	CHECKXCP; 
      }
    
    return (I)z; 
  }

}
#else
H1(aln){A z;ND1 F1 MT(1)NANMF(F,F,log(*w))} 
#endif

#if defined(_ULTRA_SPARC_VECTOR_OPS)
I aen (a) A a ;
{
  A z; 
  ND1 F1 MT(1)

  { 
    F *r=(F *)z->p, *t=r+a->n; 
    F *w=(F *)a->p; 
    int n=a->n, incx=1, incy=1;

    if( n>0 )
      {
	RESETXCP; /* NOTEIT: add  RESETXCP */
	vexp_(&n, w, &incx, r, &incy);
	CHECKXCP; /* NOTEIT: add NAN_CHECK */
      }

    return (I)z; 
  }

}
#else
H1(aen){A z;ND1 F1 MT(1)NANMF(F,F,exp(*w))}
#endif

#endif
H1(pit){A z;ND1 F1 MT(1)MF(F,F,3.14159265358979323846**w,FALSE)}
H2(sqr){A z;ND2 F1 
 if(!w->r&&It==w->t&&2==*w->p){MT(1)MF(F,F,*w**w,FALSE)}R ds(a,w,15);}
#if defined(_AIX) || defined(HAVE_SVR4) || defined(linux) || defined(__FreeBSD__) || defined (__NetBSD__) || defined(__APPLE__)
/* Z F imaxmin[2]={2147483648.0-CT,-2147483648.0}; */
Z H1(fli){A z;MT(0){
  I *r=(I*)z->p,*t=r+a->n;F x,*w=(F*)a->p;
  F imaxmin[2]; 
  imaxmin[0]=(((F)LONG_MAX)+1)-CT; imaxmin[1]=(F)LONG_MIN; 
  RESETXCP;
  for(;r<t;++w){
    x=*w<-CT?(*w*CT1)-1:(*w*CT2);
    if(x>imaxmin[0]||x<imaxmin[1]){q=9;break;}
    *r++=(I)x;}
    CHECKXCP;R(I)z;}}
Z H1(cli){A z;MT(0){
  I *r=(I*)z->p,*t=r+a->n;F x,*w=(F*)a->p;
  F imaxmin[2]; 
  imaxmin[0]=(((F)LONG_MAX)+1)-CT; imaxmin[1]=(F)LONG_MIN; 
  RESETXCP;
  for(;r<t;++w){
    x=(-*w)<-CT?((-*w)*CT1)-1:((-*w)*CT2);
    if(x>imaxmin[0]||x<imaxmin[1]){q=9;break;}
    *r++=-((I)x);}
    CHECKXCP;R(I)z;}}
#else
Z H1(fli){A z;MT(0)MF(I,F,*w<-CT?(I)(*w*CT1)-1:(I)(*w*CT2),FALSE)}
Z H1(cli){  A z;  MT(0)  MF(I,F,((-*w)<-CT?(I)(-((-*w)*CT1)+1):(I)(-(-*w)*CT2)),FALSE)}
#endif
H1(flr){A z;ND1 F1 if(z=(A)fli(a),!q)R(I)z;q=0,dc(z);MT(1)MF(F,F,fl(*w),TRUE)}
H1(cln){A z;ND1 F1 if(z=(A)cli(a),!q)R(I)z;q=0,dc(z);MT(1)MF(F,F,-fl(-*w),TRUE)}
#if defined(__VISUAL_C_2_0__)
Z I rnd(I n)
 {
  I d=rand(),r=(unsigned  long)0x8000%n;
  R r>d?rnd(n):d%n;
 }
#else
Z I rnd(I n){I d=random(),r=(unsigned)0x80000000%n;R r>d?rnd(n):d%n;}
#endif
H1(ran){A z;ND1 I1 MT(1)DO(a->n,if(a->p[i]<1){q=9;break;}
			   z->p[i]=rnd(a->p[i]))R(I)z;}
H2(dea){
  A z;I h,j,k,*p,*t,m,n;
  ND2 I2 m= *a->p,n= *w->p;
  Q(a->n!=1||w->n!=1||m<0||m>n,9);
  if(m>n/8){
    W(gv(It,n))p=z->p;
    DO(n,p[i]=i);
    for(t=p+m;p<t;*p++=j)j=p[k=rnd(n--)],p[k]= *p;R z->n=*z->d=m,(I)z;
  }
#if defined(BSTUB)
/* tm allocates "temporary" work areas under the old buddy system t-1 was */
/* the number of words were stored for the allocation */
  {
    unsigned long zz,ii;
    W(gv(It,m))
    if(!m) R(I)z;
    p=z->p;
    for (zz = (2*m) >> 1, ii = 1; zz != 0; zz >>= 1, ii++);
    zz=((2<<(ii-1))-1)+2;
    t=k_tm(zz);
    *t=ii;
    j=1<<(0xff&(h=*t));
  }
#else
  W(gv(It,m))if(!m)R(I)z;p=z->p,t=k_tm(2*m)-1,j=1<<(0xff&(h=*t));
#endif
  DO(j--,t[i]=-1);
   {
    long i, _i;
 
    _i = m;
 
    for (i = 0; i < _i; i++) {
      m=rnd(n);
      k=j&m;
      for(;t[k]!=-1;) {
        if (t[k]==m) {
        m=rnd(n);
        k = j&m;
        } else {
        ++k;
        k = j&k;
        }
      }
      t[k]=*p++=m;
    }
   }

	
/*
  DO(m,k=j&(m=rnd(n));for(;t[k]!=-1;)k=j&(t[k]==m?(m=rnd(n)):++k);t[k]=*p++=m);
*/
  R *t=h,(I)z;}


