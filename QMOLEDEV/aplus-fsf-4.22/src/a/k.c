/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.
*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <sys/param.h>
#include <unistd.h>
#include <sys/mman.h>
#include <limits.h>
#include <stdio.h>
#include <setjmp.h>
#include <a/ik.h>
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/fir.h>
#include <a/arthur.h>
I MY[2001];
CX Rx,Cx;
extern void *Glbrtn ;
extern I Xf;
extern I atOnExit, wtOnExit;	/* set in fa() used by i.c:rk() */
extern I DependencyEvaluationSemaphore;
A aplus_nl;
I mts;
I sq=2,q;
PFI g;
I *Y,*X,*XY,*K=MY;

Z I mr0(E);
Z A e0(E,A);

extern I dbg_tscb, dbg_tpcb, dbg_tprcb, dbg_trcb;

static I aplusPageMask=4096-1;
#define PAGE_ALIGN(k) ((void *)(((unsigned long)(k)) & ~aplusPageMask))

static int msyncFlag=MS_ASYNC;	/* Default flag for msync */

#if defined(linux) || defined(__sgi)
  static int msyncMode=1;		/* Default msyncMode is on*/
#else
  static int msyncMode=0;		/* Default msyncMode is off*/
#endif

void setBeamMSyncMode(A msyncMode_)
{
  int i;
  int saveFlag=msyncFlag;

  if(msyncMode_->t==It)		/* Allow explicit setting */
    {
      msyncFlag=*msyncMode_->p;
      msyncMode=1;
      R;
    }


  if(msyncMode_->t!=Et || !QS(*msyncMode_->p) )
    {
      q=ERR_DOMAIN;
      R;
    }
  
  msyncFlag=0;
  for(i=0; i<msyncMode_->n; i++)
    {
      if(msyncMode_->p[i]==MS(si("MS_ASYNC")))
	{
          if(msyncFlag&MS_SYNC)
	    {
	      q=ERR_DOMAIN;
	    }
	  else
	    {
	      msyncFlag|=MS_ASYNC;
	      msyncMode=1;
	    }
	}
      else if(msyncMode_->p[i]==MS(si("MS_SYNC")))
	{
          if(msyncFlag&MS_ASYNC)
	    {
	      q=ERR_DOMAIN;
	    }
	  else
	    {
	      msyncFlag|=MS_SYNC;
	      msyncMode=1;
	    }
	}
      else if(msyncMode_->p[i]==MS(si("MS_INVALIDATE")))
	{
	  msyncFlag|=MS_INVALIDATE;
	  msyncMode=1;
	}
      else if(msyncMode_->p[i]==MS(si("none")))
	{
	  msyncMode=0;
	}
      else
	q=ERR_DOMAIN;
    }

  if(q) msyncFlag=saveFlag;

}


A getBeamMSyncMode(void)
{
  if(msyncMode)
    {
      if(msyncFlag&MS_ASYNC && msyncFlag&MS_INVALIDATE)
	return gvi(Et,2,MS(si("MS_ASYNC")),MS(si("MS_INVALIDATE")));
      else if(msyncFlag&MS_SYNC && msyncFlag&MS_INVALIDATE)
	return gvi(Et,2,MS(si("MS_SYNC")),MS(si("MS_INVALIDATE")));
      else if(msyncFlag==MS_ASYNC)
	return gsym("MS_ASYNC");
      else if(msyncFlag==MS_SYNC)
	return gsym("MS_SYNC");
      else
	return gi(msyncFlag);
    }
  else
    return gsym("none");
}


void ki(void)
{
#if  defined(HAVE_SVR4)
  aplusPageMask=sysconf(_SC_PAGESIZE)-1;
#elif __VISUAL_C_2_0__
   aplusPageMask=CLBYTES;
#else
  aplusPageMask=getpagesize()-1;
#endif
  X=Y=XY=(K=MY)+2000;
  *X=*K=0;
  *++K=0;
  symhti();
  Rx=Cx=(CX)cxhti();
  aplus_nl=gv(Et,0);
  aplus_nl->c=0;
  p0hti();
  xshti();
  te();
  mts=MS(si(""));
  setPWD();
}

I profileExecute(n,P1,P2,i) I n,i; I (*P1[])(),(*P2[])();
{ /* _profile called from profileDyadic() profileMonadic()  */
  R n==2?(*P2[i])(*Y,Y[1],i):(*P1[i])(*Y,i);
}

I ic(A aobj)
{
/*
  R!(aobj&&QA(aobj))?(I)aobj:aobj->c?(++aobj->c,(I)aobj):im((I)aobj);
*/
	if (!(aobj && QA(aobj)))
		{
		return (I)aobj ;
		}
	else
		{
		if (aobj->c)
			{
			++aobj->c ;
			return (I)aobj ;
			}
		else
			return im((I)aobj) ;
		}
	}

void dc(A aobj)
{
  if(aobj&&aplus_nl!=aobj&&QA(aobj))
  {
    if((aplusPageMask&(I)aobj)&&aobj->c)
    {
      if(-1==aobj->c)
	{
	  H("memory violation flag dc(%ld) aborted \n",aobj);
	}
      else
	{ 
	  if(!--aobj->c) dec(aobj);
	}
    }
    else dm(aobj);
  }
}

void dec(A aobj)
{
  aobj->c=-1;
  if(aobj->t<Et){mf((I *)aobj);R;}
  if(aobj->t==Et)DO(aobj->n,dc((A)aobj->p[i]))
  else if(aobj->t==Xt)DO(aobj->r,dc((A)aobj->d[i]))
  else ef(*aobj->p),mf((I *)(aobj->p[aobj->n+1]));
  mf((I *)aobj);
}

void ef(I arg)
{
  E epr;
  if(!QE(arg)){dc((A)arg);R;}
  epr=XE(arg);
  DO(epr->n,ef(epr->a[i]));
  ef(epr->f);
  mf((I *)epr);
}

I *k_tm(I n){Z I *ta=0;if(ta)mf(ta);R ta=n?ma(n):0;}

void mv(I *dest,I *src,I n){DO(n,*dest++=*src++)}

C *tst(I t,I *d,I j,I *s,I k,I n){switch(t){
 CSR(It,DO(n,*d=*s;d+=j;s+=k)R(C*)d;)
 CSR(Et,DO(n,*d=ic((A)*s);d+=j;s+=k)R(C*)d;)
 CSR(Ft,{F *a=(F*)d;F *b=(F*)s;DO(n,*a=*b;a+=j;b+=k)R(C*)a;})
 CSR(Ct,{C *a=(C*)d;C *b=(C*)s;DO(n,*a=*b;a+=j;b+=k)R(C*)a;})} R 0;}
/* --- tmv() moved to tmp_nopipe.c ---- */
/* #ifdef HPC_PWR2_TMV */
/* C *tmv(I t,I *d,I *s,I n){switch(t){ */
/*  CSR(It,d=(I*)hpc_mv_i(d,s,n);R(C*)d;) */
/*  CSR(Et,DO(n,*d++=ic((A)(*s++)))R(C*)d;) */
/*  CSR(Ft,{F *a=(F*)d;F *b=(F*)s;a=(F*)hpc_mv_f(a,b,n);R(C*)a;}) */
/*  CSR(Ct,{C *a=(C*)d;C *b=(C*)s;a=(C*)hpc_mv_c(a,b,n);R(C*)a;})} R 0;} */
/* #else */
/* C *tmv(I t,I *d,I *s,I n){switch(t){ */
/*  CSR(It,DO(n,*d++=*s++)R(C*)d;) */
/*  CSR(Et,DO(n,*d++=ic((A)(*s++)))R(C*)d;) */
/*  CSR(Ft,{F *a=(F*)d;F *b=(F*)s;DO(n,*a++=*b++)R(C*)a;}) */
/*  CSR(Ct,{C *a=(C*)d;C *b=(C*)s;DO(n,*a++=*b++)R(C*)a;})} R 0;} */
/* #endif */
C *ttmv(I t,I *d,I *s,I n){C *x=tmv(t,d,s,n);if(Ct==t)*x='\0';R 0;}
C *trp(I t,I *d,I *s,I n){R tst(t,d,1,s,0,n);}
/*
 * The zer() function has been kludgified.  The arguments are (t,d,n),
 * where t is the type of data, d is a C* to the start of data, and n
 * is how many data to set to an "empty" value.  The kludge involves
 * t==Et+1, which indicates symbolic data, and fills with mts instead
 * of aplus_nl, which is used if Et==t.
 *
 * zer() is used in several functions, and it is possible that this
 * change has introduced a bug.  However, I am pretty sure that Et+1
 * is outside of the range of t, heretofar.
 */
Z F f=0.0;
Z C c=' ';
C *zer(I t,I *d,I n){
R trp((t==Et+1)?Et:t,d,t==Ct?(I *)&c:t==Et+1?&mts:t==Et?(I *)&aplus_nl:(I *)&f,n);}
I zr(A a){zer(a->t,(I *)a->p,a->n);R(I)a;}
I tr(I r,I *d){I n,*t;if(!r)R 1;for(t=d+r,n= *d;++d<t;n*= *d);R n;}
int tr32(int r,int *d){int n,*t;if(!r)R 1;for(t=d+r,n= *d;++d<t;n*= *d);R n;}

/* Replace 0x7FFFFFFF with LONG_MAX-1 */
/* I tr1(I r,I *d){F s=1;I t;if(r<0)DO(-r,s*=*d) */
/*  else DO(r,if(s*=t=*d++,t<0)R -1)R s>0x7FFFFFFF?-1:(I)s;} */
I tr1(I r,I *d) {
  F s=1;
  I t;
  if(r<0)
    DO(-r,s*=*d)
 else
    DO(r,if(s*=t=*d++,t<0)R -1)
      R s>(LONG_MAX-1)?-1:(I)s;
}                                     

/* #define GA(_t,_r,_n,x) {I _f=_t==Ct;A z=(A)mab(_f+AH+Tt(_t,_n)); \ */
/* 			  z->c=1,z->t=_t,z->r=_r,z->n=_n;x; \ */
/* 			  if(_f)((C*)z->p)[_n]=0;R z;} */

#define GA(_t,_r,_n,x) \
{\
  I _f=_t==Ct;A z=(A)mab(_f+AH+Tt(_t,_n)); \
  z->c=1,z->t=_t,z->r=_r,z->n=_n;x; \
  if(_f)((C*)z->p)[_n]=0; \
  R z;}

/* GA_OA overallocates by the max of 2*n or 10 meg */
#define GA_OA(_t,_r,_n,x) \
{\
  I aSize,_f=_t==Ct;\
  A z;\
  aSize=AH+Tt(_t,_n);\
  aSize=((2*aSize)<(10*1024*1024)) ? (2*aSize) : (aSize+(10*1024*1024));\
  z=(A)mab(_f+aSize); \
  z->c=1,z->t=_t,z->r=_r,z->n=_n;x; \
  if(_f)((C*)z->p)[_n]=0; \
  R z;}

#define GA3(_t,_r,_n ) {I _f=_t==Ct;A z=(A)mab(_f+AH+Tt(_t,_n)); \
			  z->c=1,z->t=_t,z->r=_r,z->n=_n; \
			  if(_f)((C*)z->p)[_n]=0;R z;}
A gm(I t,I m,I n)GA(t,2,m*n,*z->d=m;z->d[1]=n)	
A gv(I t,I n)GA(t,1,n,*z->d=n)
A gd(I t,A a)GA(t,a->r,a->n,mv(z->d,a->d,a->r))	
A ga(I t,I r,I n,I *d)GA(t,r,n,mv(z->d,d,r))
A ga_oa(I t,I r,I n,I *d)GA_OA(t,r,n,mv(z->d,d,r))
A gc(I t,I r,I n,I *d,I *p)GA(t,r,n,mv(z->d,d,r);ttmv(t,z->p,p,n))     
A gi(I i)GA(It,0,1,*z->p=i)
A gs(I t)GA3(t,0,1)
A gf(F f)GA(Ft,0,1,*(F*)z->p=f)
A ge(I x){A z=gs(Et);*z->p=x;R z;}

#define EV(z) {I t;switch(aplusMask&z){CS(0,ic((A)z))CS(3,z=ee(XE(z)))\
 CS(1,ic((A)(z=(I)gt(XV(z)))))CS(5,for(;!(t=X[U(z)]);)aplus_err(4,(A)z);ic((A)(z=t)))}}
#if 0
I ev(I z){if(q)aplus_err(q,(A)(QE(z)?XE(z)->f:z));EV(z) R z;}
#else
 I ev(I z){
   if(q)
     aplus_err(q,(A)(QE(z)?XE(z)->f:z));
 
   {
    I t;
    E etmp;
    I itmp;
 
 
    switch(aplusMask&z){
      CS(0,ic((A)z))
      case 3:
       itmp = (I)(z)&~aplusMask;
       etmp = (E)itmp;
       z=ee(etmp);
       break;
      CS(1,ic((A)(z=(I)gt(XV(z)))))
      CS(5,for(;!(t=X[U(z)]);)aplus_err(4,(A)z);ic((A)(z=t)))
    }
 }
 
   R z;
 }
#endif

extern I PX(I,I),(*PN[])(E),(*P1[])(I,I),(*P2[])(I,I,I);
I ee(E expr)
{
  I z = 0,i,n,fnc; 
	fnc = expr->f;
  if(QN(fnc))
    R(*PN[U(fnc)])(expr);
  for(i=n=expr->n;i;*--Y=z)
  {
    z=expr->a[--i];
    EV(z);
  }
  EV(fnc);
  if(QA(fnc))
  {
    ++n;
    *--Y=fnc;
    if(((A)fnc)->t>Xt+1)
      R z=(I)ga(Xt,n,0L,Y),Y+=n,z;
    z=af(n);
  }
  else
  {
    i=U(fnc);
    EQ(0,QX(fnc)?(i?PX(i,n):xin((A)*Y,n-1,(A)0)):
       n==2?(*P2[i])(*Y,Y[1],i):(*P1[i])(*Y,i));
  }
  DO(n,dc((A)(*Y++)));
  R z;
}

I fa(I fnc,I a,I w)
{
  I z,i,at,wt=0,n=w?2:1;
  if(w)*--Y=w,wt=(I)Y;
  *--Y=a,at=(I)Y;
  if(QA(fnc))ic((A)(*--Y=fnc)),z=af(++n);
  else
  {
    i=U(fnc);
    EQ(0,QX(fnc)?PX(i,n):n==2?(*P2[i])(*Y,Y[1],i):(*P1[i])(*Y,i));
  }
  if(w)wtOnExit=((A)*(I*)wt)->t;
  atOnExit=((A)*(I*)at)->t;
  DO(n,dc((A)(*Y++)));
  R z;
}

Z void es(E e,I n,I a){e->a[n]=a?a:(I)aplus_nl;}
Z I ms(I s){A z=gs(Et);R *z->p=MS(s),(I)z;}

A af4(A f,I a,I b,I c,I d,V v){
  I z=b?0:3,x=0,y=0,n=QA(f)&&f->t==Xt+1&&f->r-1<7-z?f->r-1:0;
  E e=(E)ma(2+n);
  e->n=n,e->f=(I)f;if(n>4-z)if(y=ms((I)v->s),x=ms((I)v->cx->s),z)b=x,c=y;
  switch(n){case 6:es(e,5,y);case 5:es(e,4,x);case 4:es(e,3,d);
    case 3:es(e,2,c);case 2:es(e,1,b);case 1:es(e,0,a);
  }
  R a=(I)ez(ME(e)),xrr(),mf((I *)e),dc((A)x),dc((A)y),(A)a;
}
A un(A *v){
  A a=*v;
  if(a->c>1||!a->c&&!isWritableFile((I)a))
    *v=gc(a->t,a->r,a->n,a->d,a->p),dc(a);
  R *v;}

#ifndef BSTUB
extern I MZ[];
#endif

Z I app(A *z,A w){
  A a=un(z);
  Q(!a->r||(w->r)>(a->r),ERR_RANK) 
  {
    XA;I wr=w->r,wn=w->n,*wd=w->d;
    I r=0,n=0,d[9],u;
    C *end;
    if(ar&&wr)
      if(ar==wr) { 
	Q(cm(ad+1,wd+1,ar-1),ERR_LENGTH)
	mv(d,wd,r=wr), n=*ad;
      }else{
	Q(ar-wr!=1,ERR_RANK)
	Q(cm(ad+1,wd,ar-1),ERR_LENGTH)
      }
    if(wr<ar)
      n=1, mv(d,ad,r=ar);
    *d+=n;
    Q(MAXR<r,ERR_MAXRANK)
    u=!wr?(wn=tr(r-1,d+1),1):0;
    n=an+wn;
    if(a && !a->c)
      Q(*d>a->i,ERR_MAXITEMS)
#ifdef BSTUB
    else if(AH+Tt(at,n)+(at==Ct)+sizeof(long)>(((I*)a)[-1]))
      {
        /* printf("ga(%ld,%ld,%ld,%ld)\n",at,ar,n,ad); */
        /* Use ga_oa to overallocate storage */
	*z=ga_oa(at,ar,n,ad),
	  tmv(at,(*z)->p,a->p,an),
	  dc(a),
	  a=*z;
/* 	printf("AH+Tt(at,n)+(at==Ct)+sizeof(long)=%d  z[-1]=%d\n", */
/* 	      (AH+Tt(at,n)+(at==Ct)+sizeof(long)),((I*)a)[-1]); */
      }
#else
    else if(AH+Tt(at,n)+(at==Ct)+sizeof(long)>sizeof(long)*MZ[255&((I*)a)[-1]])
      {
/*          printf("ga(%ld,%ld,%ld,%ld)\n",at,ar,n,ad); */
	*z=ga_oa(at,ar,n,ad),
	  tmv(at,(*z)->p,a->p,an),
	  dc(a),
	  a=*z;
      }
#endif
    end = (!u) ? tmv(at,(I *)(((C*)a->p)+Tt(at,an)),w->p, wn):
                 trp(at,(I *)(((C*)a->p)+Tt(at,an)),w->p, wn);
    if(Ct==at) *end='\0'; 
    a->n=n,*a->d=*d;
    R(I)w;
  }
}
Z I in(A *z,I g,I f,A a,A w,I r)
{
  A aobj;
  I j=(*z)->t;
  if(j!=w->t&&(!(w=j==Ft?ep_cf(2):j==It?ci(2):(q=6,(A)0))))R 0;
  if(g)
  {
    if(f){a=(A)*a->p;Q(cm(w->d,a->d,a->r),8)}
    R app(z,w);
  }
  aobj=un(z);
  if(!r)R xr(aobj,a,w);
  j = w->n!=1 ;
  if(j)
  {
    Q(w->r!=a->r,7);
    Q(cm(w->d,a->d,w->r),8);
  }
  I1{
    I *ap=a->p,t=w->t,n=a->n;
    P p;
    C *wp=(C*)w->p;
    p.i=aobj->p;
    j=Tt(t,j);
    DO(n,
       Q((unsigned long)(n=*ap++)>=aobj->n,10);
       switch(t)
       {
	 CS(It,p.i[n]=*(I*)wp);
	 CS(Ft,p.f[n]=*(F*)wp);
	 CS(Ct,p.c[n]=*wp);
	 CS(Et,dc(p.a[n]);p.i[n]=ic(*(A*)wp));
       }
       wp+=j;
       );
    R 1;
  }
}
I pcb(V v,I d,I i,I p){I a;if(!(Sf&&v->p))R d;if(dbg_tpcb)cbtrc(v,1);
		 R a=(I)af4((A)v->p,v->q,d,i,p,v),dc((A)d),a;}
I prcb(V v,I d,I i,I p){I a;if(!(Sf&&v->rpf))R d;if(dbg_tprcb)cbtrc(v,3);
		  R a=(I)af4((A)v->rpf,v->rpc,d,i,p,v),dc((A)d),a;}
Z I enc(I a){A z=gs(Et);R *z->p=a,(I)z;}
Z I gap(A a,A w){I v=a->r&&a->r==w->r,n=v?*w->d:1;A z=v?gv(It,n):gs(It);
 DO(z->n,z->p[i]=*a->d+i)R enc((I)z);}
Z I gia(A i,I r){R!r&&i&&i->t<Et?enc((I)i):(I)i;}
Z I upd(I x,I d,I i,A p,I r,I o){
  I f=QV(x),a,*z,g=i==MP(22);
  V v=f?XV(x):(V)(X+U(x));
  extern I Sf;
  Q(QP(i)&&!g,18);
  if(f){if(p||i)gt(v);}else Q((p||i)&&!v->a,4);
  if(p){z=(I*)pka(p,(A *)v);if(q)R aplus_err(q,(A)MP(36)),0;}else z=(I*)v;
  if(QE(i))Q(!(i=*Y=(I)e0(XE(i),(A)*z)),9);
  if(f)
  {
    Q(g&&0==((A)(*z))->r,7);
    i=*Y=g?gap((A)*z,(A)d):gia((A)i,r);
    Y[2]=d=prcb(v,d,i,(I)p);
    if(!d)R 0;
    Y[2]=d=pcb(v,d,i,(I)p);
    if(!d)R 0;
    Q(!p&&!i&&v->o&&!vfy(v,(A)d),17);
  }  
  if(!z)R 0;
  a=!i?(dc((A)*z),*z=ic((A)d)):in((A *)z,g,f,(A)i,(A)d,r);
  if( msyncMode && v && v->a && QA(v->a) && 
      ((A)(v->a))->c==0 && ((A)(v->a))->t<4)
    {
      A a=(A)v->a;
      I an=a->n;
      I at=a->t;
      if(g) 
	{			/* append assigment */
	  I  bytesAdded=Tt( ((A)d)->t, ((A)d)->n);
	  C *addr1=( ((C*)a->p) + Tt(at,an)) - bytesAdded;
	  C *addr2=PAGE_ALIGN(addr1);
	  bytesAdded += addr1-addr2;
	  
	  /* 	  printf("msync: v->a:0x%x actual:=0x%x aligned:=0x%x bytes=%u\n", */
	  /* 		 v->a, addr1, addr2, bytesAdded); */
	  
	  if(-1==msync( addr2, bytesAdded, msyncFlag)) /* data */
	    perror("upd data: msync");
	  if(-1==msync( (C*)a, AH, msyncFlag))         /* header */
	    perror("upd header: msync");
	} 
      else 
	{
	  /* msync data */
	  /* if(-1==msync((A)v->a, mf_length((A)v->a), msyncFlag)) */
	  if(-1==msync((C*)a, AH+Tt(at,an), msyncFlag))
	    perror("upd: msync");
	};
    }

  if(!a||!f)R a;
  i=*Y;d=Y[2];
  if(2>=v->z)inv(v,r?0:i,0);
  if(2>v->z) 
  {
    if(Sf&&v->f){
      if(dbg_tscb)cbtrc(v,0);v->z=2,dc(af4((A)v->f,v->c,d,i,(I)p,v));}
    if(Sf&&v->rff){
      if(dbg_trcb)cbtrc(v,2);v->z=2,dc(af4((A)v->rff,v->rfc,d,i,(I)p,v));}
    val(v);
  }
  if(o&&v->o)xup(v,(A)d,(A)i,p,r,DependencyEvaluationSemaphore?0:1); /*  call xup() unless in the middle of re-evaluation */
  R q?0:1;
}

I set(I x,I a,I o)
{
  I r;
  R *--Y=a,*--Y=0,*--Y=0,r=upd(x,a,0,0,0,o),dc((A)Y[2]),Y+=3,r;
}

I aset(I v,I d,I i,I p)
{
  I r;
  Y-=3,*Y=i?ic((A)i):0,r=upd(MV(v),Y[2]=d,i,(A)p,0,1);
  dc((A)Y[2]),dc((A)*Y),Y+=3;
  R xrr(),r;
}
Z I lst(I n,I *p,A w)
{
  Q(w->r>1,7)Q(w->r&&w->n!=n,8);
  DO(n,if(!set(p[i],pck(i*w->r,w),0))R 0);
  DO(n,if((!q)&&QV(p[i])&&(XV(p[i]))->o)xup(XV(p[i]),(A)(XV(p[i]))->a,0,0,0,1));
  if(q)R 0;
  DO(n,if(QV(p[i]))val(XV(p[i])));
  R 1;
}
#define Q1(x,n) if(x){R dc(c),dc(v),q=n,0;}
Z I pea(E e,A w)
{
  I f=e->n-1,n,*va,z=1;
  A c=(A)(f?ev(*e->a):0),v=(A)ev(e->a[f]);
  Q1(!QA(v)||Et<(v)->t,18);
  n=v->n;
  Q1(v->r>1,7);
  Q1(n&&!sym(v),9);
  if(f)
  {
    Q1(c->r>1,7);
    Q1(c->n&&!sym(c),9);
    Q1(v->r&&c->r&&c->n!=n,8);
    if(c->r)n=c->n;
  }
  if(n)
  {
    va=ma(n);
    DO(n,va[i]=MV(sv(f?cxi(XS(c->p[i*c->r])):Cx,XS(v->p[i*v->r]))));
    z=lst(n,va,w);
    mf(va);
  }
  R dc(c),dc(v),z;
}
Z I mrg(I e){R *--Y=0,*--Y=0,e=mr0((E)e),dc((A)*Y++),dc((A)(*Y++)),e;}
/* #if defined(__SVR4) || defined(_SYSTYPE_SVR4) || defined(__VISUAL_C_2_0__) */
#if 1
I xis(E e)
{
  I n=e->n-1,a=*e->a,w=e->a[n];
/*  EV(w) */
/*	printf("In xis, n = %ld, a = %ld, w = %ld\n", n, a, w) ; */
/*	printf("aplusMask&w = %ld\n", aplusMask&w) ; */
	{
    I t;
    E etmp;
    I itmp;
    switch(aplusMask&w){
      CS(0,ic((A)w))
      CS(1,ic((A)(w=(I)gt(XV(w)))))
      case 3:
       itmp = (I)(w)&~aplusMask;
       etmp = (E)itmp;
/*		printf("w = ee(%ld)\n", itmp) ; */
       w=ee(etmp);
/*	    printf("after ee(), w = %ld\n", w) ; */
       break;
	  case 5:
		while (!(t=X[U(w)]))
			aplus_err(4, (A)w) ;
		ic((A)(w=t)) ;
		break ;
/*      CS(5,for(;!(t=X[U(w)]);)aplus_err(4,(A)w);ic((A)(w=t))) */
	  }
	}
/*	printf("w = %ld\n", w) ; */
  if(!n) {
	Glbrtn = (void *)w ;
	longjmp(J,(int)w);
	}
  for(*--Y=w;
      !(!QE(a)?set(a,ic((A)w),1):
	(e=XE(a),e->f==MN(7))?lst(e->n,e->a,(A)w):
	peak(e->f)?pea(e,(A)w):
	mrg((I)e));)
    aplus_err(q,(A)MN(0));
  R *Y++;
}
#else
I xis(E e)
{
  I n=e->n-1,a=*e->a,w=e->a[n];
  EV(w)if(!n)_longjmp(J,w);
  for(*--Y=w;
      !(!QE(a)?set(a,ic((A)w),1):
	(e=XE(a),e->f==MN(7))?lst(e->n,e->a,(A)w):
	peak(e->f)?pea(e,(A)w):
	mrg((I)e));)
    aplus_err(q,(A)MN(0));
  R *Y++;
}
#endif
Z A e0(E e,A a)
{
  I *r=e->a+e->n-1;
  A z=gd(It,a);
  I t=*r;
  DO(z->n,z->p[i]=i);
  R *r=(I)z,a=(A)ez(ME(e)),dc(z),*r=t,a;
}
Z I xli(E e)
{
  A z;
  I n=e->n-1;
  W(gv(Et,n));
  *--Y=zr(z);
  for(;n--;)z->p[n]=ev(e->a[n+1]);
  R ++Y,(I)z;
}
Z S ss(I x){A a=(A)ev(x);S s=(!a->r&&sym(a))?XS(*a->p):0;dc(a);R s;}
Z I mr0(E e)
{
  I y=0,f=e->f,n,r=0;
  if(f!=MP(36)&&f!=MP(74))
  {
    n=e->n-1;
    if(QE(y=f==MX(0)?*e->a:e->a[n])&&(r=XE(y)->f==MP(22)))y=*XE(y)->a;
    *Y=f==MP(20)?ev(*e->a):f!=MX(0)?(r=1,ME(e)):n==1?ev(e->a[1]):xli(e);
    if(QE(y))e=XE(y),f=e->f;
  }
  if(f==MP(36))if(y=e->a[1],Y[1]=ev(*e->a),QE(y=e->a[1]))e=XE(y),f=e->f;
  if(f==MP(74))
  {
    S s=ss(e->a[n=e->n-1]),c=n?ss(*e->a):0;
    Q(!s||n&&!c,9);
    y=MV(sv(n?cxi(c):Cx,s));
  }
  R upd(y,Y[2],*Y,(A)Y[1],r,1);
}
