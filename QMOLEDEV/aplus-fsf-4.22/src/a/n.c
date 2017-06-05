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
#include <a/x.h>
#include <a/arthur.h>
#include <stdio.h>
#include <string.h>
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
# include <stdlib.h>
#else
# include <malloc.h>
#endif
#include <sys/types.h>
#include <sys/times.h>
#include <stdlib.h>
#include <unistd.h>

#if !defined(HAVE_SVR4) && !defined(_AIX)
#include <sys/time.h>
#define GET_MILLISEC(t)  ((t).tv_sec*1000+10*((t).tv_usec/10000))
#endif

#ifdef __VISUAL_C_2_0__
int	gettimeofday __P((struct timeval *, struct timezone *));
#define xfpechk(a1,a2)
#endif
extern I dbg_depth,dbg_tmdo,dbg_tx,dbg_ts,dbg_xfpe;
extern I nExternalFPE, suppressFpeDomain;
typedef struct{I u,s,e;}B;Z B t0;Z A ta;Z I tn,td[2]={0,4},**tp,tj;Z I e0;

/* On non SVR4 systems times() returns only a return code (0 or -1)   */
/* instead of the elapsed time. In this case gettimeofday() is called */
/* to get the elapsed time, which is derived from a struct timeval    */
Z B ti(void)
{
  Z struct tms r; B t; Z int clkTck=0;		                 
#if defined(HAVE_SVR4) || defined(_AIX)
  if(clkTck==0) clkTck=sysconf(_SC_CLK_TCK);     /* clock ticks per second */
  t.e=((1000*times(&r))/clkTck)-e0;              /* elapsed */
#else
  Z struct timeval tp;
  Z struct timezone tzp;
  if(clkTck==0) clkTck=sysconf(_SC_CLK_TCK);     /* clock ticks per second */
  times(&r);                    
  gettimeofday(&tp,&tzp);	                 /* need for elapsed */
  t.e=GET_MILLISEC(tp)-e0;	                 /* elapsed */
#endif
  t.u=(1000*r.tms_utime)/clkTck;                 /* user */
  t.s=(1000*r.tms_stime)/clkTck;                 /* system */
  R t;
}

void te(void){e0=ti().e;}
Z B tz(B x,B y){B t;R t.u=x.u-y.u,t.s=x.s-y.s,t.e=x.e-y.e,t;}
I t2(I n,I i){B t;I*p=((A)ta->p[1])->p;if(i)++p[4*n];t0=tz(t=ti(),t0);
 R p+=4*tn+1,*p+=t0.u,p[1]+=t0.s,p[2]+=t0.e,t0=t,i=tn,tn=n,i;}
Z I n_gt(B t){A z=gv(It,3);R(*(B*)z->p)=t,(I)z;}
Z H1(t1){
  I n=a->n;
  A z;
  if(tj){DO(tj,if(tp[i])*tp[i]=0)dc(ta),ta=0;}
  tj = n ;
  if(tj)
  {
    V v;
    *td=++n,z=(A)zr(ga(It,2,n*4+tj,td));
    tp=(I**)z->p+(z->n=n*4);
    ta=gv(Et,2),*ta->p=ic(a),ta->p[1]=(I)z;
    DO(tj,if(v=sv(Cx,XS(a->p[i])),(z=(A)(v->e?v->e:v->a))&&QA(z)&&Et<z->t)
       *(tp[i]=z->p+z->n)=i+1);
  }
  R t0=ti(),ic(a);
}

#define N(f) Z I f(E e)
Z I ni(A a){I i;for(*--Y=(I)a;a->n!=1||a->t&&!(a=ci(0));a=(A)*Y)aplus_err(9,(A)MN(*K));R i=*a->p,dc((A)*Y++),i;}
N(xpr){I z=(I)aplus_nl,n=e->n;++K;DO(n,*K=-1-i;dc((A)z);z=ev(e->a[i]))R --K,z;}
N(xif){I z;*++K=2;z=ni((A)ev(*e->a))?ev(e->a[1]):e->n>2?(*K=1,ev(e->a[2])):(I)aplus_nl;R --K,z;}
N(xwh){I z;*++K=3;for(*--Y=z=(I)aplus_nl;ni((A)ev(*e->a));*Y=ev(e->a[1]),dc((A)z),z=*Y);R --K,++Y,z;}
Z void n_set(A *a,I i){if((*a)->c!=1||(*a)->t!=It||(*a)->n)dc(*a),*a=gi(i);else *(*a)->p=i;}
N(xdo){
  I v=*e->a,i,z=(I)aplus_nl,f=1;
  A *a;
  E exp;
  if(e->n==1)
  {
    ++dbg_depth;
    if(dbg_tmdo) mdotrc(1);
    z=pev(v);
    if(dbg_tmdo)mdotrc(0);
    --dbg_depth;
    R z;
  }
  if( QE(v) )
  {
    exp=XE(v);
    if( exp->n==1 && exp->f==MP(15) ) v=*exp->a,f=0;
  }
  *++K=4;
  i=ni((A)ev(v));
  if(QE(v)&&MN(0)==XE(v)->f) v=*XE(v)->a;
  a = (A*)(QV(v)?&XV(v)->a:QL(v)?X+U(v):0);
  if(a) n_set(a,f?0:i-1);
  for(v=0;v<i;)
    if(dc((A)z),z=ev(e->a[1]),++v,a) n_set(a,f?v:i-1-v);
  if( !f && a ) n_set(a,i);
  R --K,z;
}
Z H2(in){ 
  I n=a->r?*a->d:1;
  I r=0;
  for(*--Y=ic(w),*--Y=(I)a;a=(A)fnd((A)*Y,(A)Y[1]),q;) aplus_err(q,(A)MP(19));
  dc((A)(*Y++));
  dc((A)(*Y++));
  DO(a->n,if(n>a->p[i]){r=1;break;})
  dc(a);
  R r;
}
N(xca){
  I *p,z;
  *++K=5,z=ev(*e->a),e=XE(e->a[1]),p=e->a;
  ++K;
  DO(e->n/2,*K=-1-2*i;if(in((A)ev(*p++),(A)z))break;++p);
  R --*K,dc((A)z),z=p<e->a+e->n?ev(*p):(I)aplus_nl,K-=2,z;
}
N(xti){
  B t;
  A a;
  if(!e->n)
    R ta?(t2(0,0),(I)gc(ta->t,ta->r,ta->n,ta->d,ta->p)):n_gt(ti());a=(A)*e->a;
  R QA(a)&&sym(a)||aplus_nl==a?t1(a):(t=ti(),dc((A)ev((I)a)),n_gt(tz(ti(),t)));
}
N(xli)
{
  A z;
  I n=e->n;
  W(gv(Et,n));
  *--Y=zr(z);
  for(;n--;)z->p[n]=ev(e->a[n]);
  R ++Y,(I)z;
}

Z I mx(I o,I f,I g){I d[3];*d=MN(o),d[1]=f,d[2]=g;R(I)ga(Xt,g?3:2,0L,d);}
N(n_rk){R mx(8,ev(*e->a),ev(e->a[1]));}
N(n_ea){A a=(A)ev(*e->a);I z;if(QF(a))R mx(9,(I)a,0L);
 R a->n==1&&a->t==Et&&QF(*a->p)&&!QS(*a->p)?(z=ic((A)(*a->p)),dc(a),z):(I)a;}
I(*PN[])(E)={xis,xpr,xif,xwh,xdo,xca,xti,xli,n_rk,n_ea};
#define XI 10000
#define XSHTSIZE 1024
C *xfs_name[XI]={"[]"},*xfs_desc[XI]={"??"};
I xfs_valence[XI];
Z PFI xfs_fp[XI];
I xfs_type[XI],xfs_argtypes[XI];
Z I xi=0,y[8];
HT xsht;
Z C *argtypes[]={"any","int","float","char","any","int","float","string",
"anyscalar","scalar int","floatsc","charsc","any","int","float","char"};

Z C *cxtdotname(V v){ C *res, *cxt=v->cx->s->n, *name=v->s->n;
  res=(C *)malloc(2+strlen(cxt)+strlen(name));sprintf(res,"%s.%s",cxt,name);R res;}
  
Z C *defaultdoc(const C *s,I t,I n,I *y){ C *z=(C *)malloc(128+strlen(s));
  strcpy(z,s);
  if(n) {
    strcat(z,"{");
    while(n--){strcat(z,argtypes[*y++]);strcat(z,";");}
    z[strlen(z)-1]='}';
  } else strcat(z,"{}");
  strcat(z," returns ");strcat(z,(8==t)?"null":argtypes[t]);R z;
}

Z C *installdoc(const C *s,C *d){C *z=(C*)malloc(2+strlen(s)+strlen(d));
 strcpy(z,s);strcat(z,"\n");strcat(z,d);R z;}

void xfs(void){C **s=xfs_name;for(;*++s;)if(**s=='_'&&(*s)[1]!='_')H("%s ",*s);NL;} 
void x_fs(void){C **s=xfs_name;for(;*++s;)if(**s=='_'&&(*s)[1]=='_')H("%s ",*s);NL;} 

I xinstall(PFI f, const C *s,I t,I n,I *y,C *d)
{
  I p=0,i;V v;C *nm=0;
  if(xi==XI-1)R H("too many installs\n"),0;
  xfs_fp[++xi]=f,xfs_type[xi]=t,xfs_valence[xi]=n;
  if(n&&*y==-1)R xfs_argtypes[xi]=-1;
  if(n<0)n= -n;if(n>8)R --xi,H("too many arguments for %s\n",s),0;
  for(i=n;i-->0;)p|=y[i]<<4*i;xfs_argtypes[xi]=p;
  if(*s!='_')v=sv(Cx,si(s)),v->a=MX(xi),v->t=5,nm=cxtdotname(v);
  if (d==(C *)0) d=defaultdoc(nm?nm:s,t,n,y); else d=installdoc(nm?nm:s,d);
  xfs_name[xi]=(nm?nm:strdup(s));
  xfs_desc[xi]=d?d:"";
  chtsi(xsht,xfs_name[xi],MX(xi));
  R 1;
}

void install(PFI f,const C *s,I t,I n,I t0,I t1,I t2,I t3,I t4,I t5,I t6,I t7)
{
  switch(abs(n)) 		/* xinstall() will take a negative n */
  {
  case 8:y[7]=t7;  /* intentional fall-thru switch statement */
  case 7:y[6]=t6;
  case 6:y[5]=t5;
  case 5:y[4]=t4;
  case 4:y[3]=t3;
  case 3:y[2]=t2;
  case 2:y[1]=t1;
  case 1:y[0]=t0;
  default:break;
  }
  xinstall(f,s,t,n,y,(C *)0);
}

I xfpeFlag=0;

#define XFIN  if(xfpeFlag){suppressFpeDomain=1;} \
              nExternalFPE=0; \
              ++dbg_depth; \
              if(dbg_tx||dbg_ts)xftrc(xfs_name[idx],0)
#define XFOUT if(dbg_xfpe)xfpechk(xfs_name[idx],nExternalFPE); \
              suppressFpeDomain=0; \
              if(dbg_tx)xftrc(xfs_name[idx],1); \
              --dbg_depth
#define RA(x,p) {XFIN;z=((p)(*f))x;XFOUT; \
  R !z&&q?0:t==A_?(z?(I)z:(I)aplus_nl):t==CP?(I)gsv(0,(C *)z):t==IV?(I)gi(z):(I)aplus_nl;}
I PX(I idx,I n){
  I i,t=xfs_type[idx],p=xfs_argtypes[idx];
  PFI f=xfs_fp[idx];
  I z;
  A a;
  Q(n!=xfs_valence[idx]&&xfs_valence[idx]>=0,5);
  if(p==-1)RA((Y,n),I(*)(I *,I));
  for(i=0;i<n;++i)
  {
    switch(a=(A)Y[i],z=p&15,p>>=4,z&3)
    {
      CS(CA,ND1 Q(Ct!=a->t,6));
      CS(IA,ND1 if(It!=a->t&&!(a=ci(i)))R 0);
	  CS(FA,ND1 if(Ft!=a->t&&!(a=ep_cf(i)))R 0);
    }
    switch(z&12)
    {
    case U_:ND1 a=(A)un((A *)(Y+i));
      CS(A_,y[i]=(I)a);
      CS(P_,ND1 y[i]=(I)a->p);
      CS(V_,ND1 Q(a->n!=1,8)y[i]=*a->p);
    }
  }
 switch(n)
 {
   CSR(0,RA((),I(*)(void)));
   CSR(1,RA((y[0]),I(*)(I)));
   CSR(2,RA((y[0],y[1]),I(*)(I,I)));
   CSR(3,RA((y[0],y[1],y[2]),I(*)(I,I,I)));
   CSR(4,RA((y[0],y[1],y[2],y[3]),I(*)(I,I,I,I)));
   CSR(5,RA((y[0],y[1],y[2],y[3],y[4]),I(*)(I,I,I,I,I)));
   CSR(6,RA((y[0],y[1],y[2],y[3],y[4],y[5]),I(*)(I,I,I,I,I,I)));
   CSR(7,RA((y[0],y[1],y[2],y[3],y[4],y[5],y[6]),I(*)(I,I,I,I,I,I,I)));
   CSR(8,RA((y[0],y[1],y[2],y[3],y[4],y[5],y[6],y[7]),I(*)(I,I,I,I,I,I,I,I)));
 }
 R -1;
}

void xshti(void){xsht=hti(XSHTSIZE);chtsi(xsht,xfs_name[xi],MX(xi));}

A ep_xfsinfo(void)
{
  A z=gv(Et,2);
  A z1,zt;
  z->p[0]=(I)gvi(Et,6,MS(si("xfs_name")),MS(si("xfs_desc")),
		 MS(si("xfs_valence")),MS(si("xfs_type")),
		 MS(si("xfs_argtypes")),MS(si("xfs_fp")));
  z1=gv(Et,6);
  zt=gv(Et,xi);DO(xi,zt->p[i]=(I)gsv(0,xfs_name[i]));z1->p[0]=(I)zt;
  zt=gv(Et,xi);DO(xi,zt->p[i]=(I)gsv(0,xfs_desc[i]));z1->p[1]=(I)zt;
  z1->p[2]=(I)gc(It,1,xi,&xi,xfs_valence);
  z1->p[3]=(I)gc(It,1,xi,&xi,xfs_type);
  z1->p[4]=(I)gc(It,1,xi,&xi,xfs_argtypes);
  z1->p[5]=(I)gc(It,1,xi,&xi,(I *)xfs_fp);
  z->p[1]=(I)z1;
  R z;
}
