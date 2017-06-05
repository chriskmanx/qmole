/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <a/development.h>
#ifdef __sgi /* need for param.h who include signal.h */
#define _BSD_COMPAT
#endif
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <string.h>
#ifdef SOLARIS_CSET
char *strdup();
#endif

#include <limits.h>

#include <sys/time.h>
#include <sys/resource.h>
#include <sys/param.h>
#include <sys/file.h>

#include <dap/dap.h>
#include <dap/buff.h>

#include <a/ik.h>
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/b.h>
#include <a/fir.h>
#include <a/arthur.h>
#undef min

#ifndef HAVE_STRERROR 
  extern int sys_nerr;
  extern char *sys_errlist[];
#endif

extern C Fs[];
extern C *xfs_name[], *cmdsList[], *es[];
extern A get_loadfile();
extern I xfpeFlag;

#undef ENTRYPOINT
#define ENTRYPOINT static

#define qv0(v) ((v->a || v->e || v->f || v->c || v->p || v->q ||     \
		  v->cd || v->rff || v->rfc || v->rpf || v->rpc ||   \
		  v->scd || v->atb)?1:0)

A gsym(const C *str){A z;if(!str)R(A)aplus_nl;z=gs(Et);z->p[0]=MS(si(str));R z;}

Z V av(A a)
{
  I n=a->n-1;
  if (!sym(a)||n>1) R (V)0;
  R sv(n?cxi(XS(*a->p)):Cx,XS(a->p[n]));
}

Z V avlu(A a,I *perr)
{
  I n=a->n-1;
  *perr=0;
  if (!sym(a)||n>1) R *perr=1,(V)0;
  R svlu(n?cxlu(XS(*a->p)):Cx,XS(a->p[n]));
}

S symjoin(S s1,S s2){S z;C *st1=s1->n,*st2=s2->n,*buf;
I n1=strlen((DEV_STRARG)st1),n2=strlen((DEV_STRARG)st2);if(strchr((DEV_STRARG)st2,'.'))R s2;
buf=(C*)balloc(2+n1+n2);bcopy(st1,buf,n1);buf[n1]='.';
bcopy(st2,buf+(n1+1),n2);buf[1+n1+n2]='\0';z=si(buf);bfree(buf);R z;}

S symsplit(S s,S *pcx)
{
  C *src=strdup(s->n);
  C *dot=(C *)strrchr((DEV_STRARG)src,'.');
  S z;
  if(dot)
  {
    *dot='\0';
    *pcx=si(src);
    z=si(dot+1);
  }
  else
  {
    *pcx=si("");
    z=s;
  }
  free(src);
  R z;
}

ENTRYPOINT
I ep_scb(A a,A x)
{
  V v;I f,c,i;
  NDC2(a,x);if(x->t!=Et||x->n!=2) ERROUT(ERR_DOMAIN);
  v=av(a);f=x->p[0];c=x->p[1];i=qz((A)f);
  if(!v) ERROUT(ERR_DOMAIN);
  if(!(QF(f)&&!QS(f))&&!i) ERROUT(ERR_NONFUNCTION);
  dc((A)v->f),v->f=i?0:ic((A)f),dc((A)v->c),v->c=ic((A)c);
  R 0;
}

ENTRYPOINT
A ep_gcb(A a)
{
  A z;V v;I err;
  NDC1(a);v=avlu(a,&err);
  if(!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  if(!v->f)R 0;
  R z=gv(Et,2),*z->p=ic((A)v->f),z->p[1]=ic((A)v->c),(A)z;
}

ENTRYPOINT
I ep_spcb(A a,A x)
{
  V v;I f,c,i;
  NDC2(a,x);if(x->t!=Et||x->n!=2) ERROUT(ERR_DOMAIN);
  v=av(a);f=x->p[0];c=x->p[1];i=qz((A)f);
  if(!v) ERROUT(ERR_DOMAIN);
  if(!(QF(f)&&!QS(f))&&!i) ERROUT(ERR_NONFUNCTION);
  dc((A)v->p),v->p=i?0:ic((A)f),dc((A)v->q),v->q=ic((A)c);
  R 0;
}

ENTRYPOINT
A ep_gpcb(A a)
{
  A z;V v;I err;
  NDC1(a);v=avlu(a,&err);
  if(!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  if(!v->p)R 0;
  R z=gv(Et,2),*z->p=ic((A)v->p),z->p[1]=ic((A)v->q),(A)z;
}

ENTRYPOINT
I ep__srcb(A a,A x)
{
  V v;I f,c,i;
  NDC2(a,x);if(x->t!=Et||x->n!=2) ERROUT(ERR_DOMAIN);
  v=av(a);f=x->p[0];c=x->p[1];i=qz((A)f);
  if(!v) ERROUT(ERR_DOMAIN);
  if(!(QF(f)&&!QS(f))&&!i) ERROUT(ERR_NONFUNCTION);
  dc((A)v->rff),v->rff=i?0:ic((A)f),dc((A)v->rfc),v->rfc=ic((A)c);
  R 0;
}

ENTRYPOINT
A ep__grcb(A a)
{
  A z;V v;I err;
  NDC1(a);v=avlu(a,&err);
  if(!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  if(!v->rff)R 0;
  R z=gv(Et,2),*z->p=ic((A)v->rff),z->p[1]=ic((A)v->rfc),(A)z;
}

ENTRYPOINT
I ep__sprcb(A a,A x)
{
  V v;I f,c,i;
  NDC2(a,x);if(x->t!=Et||x->n!=2) ERROUT(ERR_DOMAIN);
  v=av(a);f=x->p[0];c=x->p[1];i=qz((A)f);
  if(!v) ERROUT(ERR_DOMAIN);
  if(!(QF(f)&&!QS(f))&&!i) ERROUT(ERR_NONFUNCTION);
  dc((A)v->rpf),v->rpf=i?0:ic((A)f),dc((A)v->rpc),v->rpc=ic((A)c);
  R 0;
}

ENTRYPOINT
A ep__gprcb(A a)
{
  A z;V v;I err;
  NDC1(a);v=avlu(a,&err);
  if(!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  if(!v->rpf)R 0;
  R z=gv(Et,2),*z->p=ic((A)v->rpf),z->p[1]=ic((A)v->rpc),(A)z;
}


ENTRYPOINT
A ep_scd(A a,A cd)
{
  A z;V v;
  NDC1(a);v=av(a);
  if (!QA(cd)) ERROUT(ERR_DOMAIN);
  if(!v) ERROUT(ERR_DOMAIN);
  z=v->cd;
  v->cd=(A)ic(cd);
  R z;
}

ENTRYPOINT
A ep_gcd(A a)
{
  V v;I err;
  NDC1(a);v=avlu(a,&err);
  if(!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  R (A)(v->cd?ic(v->cd):0);
}

ENTRYPOINT
A ep__sscd(A a,A cd)
{
  A z;V v;
  NDC1(a);v=av(a);
  if (!QA(cd)) ERROUT(ERR_DOMAIN);
  if(!v) ERROUT(ERR_DOMAIN);
  z=v->scd;
  v->scd=(A)ic(cd);
  R z;
}

ENTRYPOINT
A ep__gscd(A a)
{
  V v;I err;
  NDC1(a);v=avlu(a,&err);
  if(!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  R (A)(v->scd?ic(v->scd):0);
}

ENTRYPOINT
I ep__utd(A a){
  V v;I err;
  NDC1(a);v=avlu(a,&err);
  if(!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  R v->z;
}

Z I nt[]={1,2,3,3,3,4};
Z C *vt[]={"cxs","vars","fns","ops","xfs"};

/* Z V ct(a)A a;{S s=a->n?XS(*a->p):0;R!s?Cx:cxi(s);} */
Z CX ct(A a,I *perr)
{
  S s=(QA(a)&&Et==a->t&&1==a->n)?(S)(*a->p):0;
  *perr=0;
  R(0==a->n)?Cx:(s&&QS(s))?cxlu(XS(s)):(*perr=1,(CX)0);
}

/*******************************************************************/

ENTRYPOINT
A ep_def(A a)
{
  V v;I err;
  NDC1(a);v=avlu(a,&err);
  if(!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  R (a=(A)v->e)?(A)gsv(0,(C *)a->p[a->n+1]):(A)gz();
}

ENTRYPOINT
A ep_dep(A a)
{
  V v,vl; I *l,n=0,err; A result;
  NDC1(a);v=avlu(a,&err);
  if (!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  for(l=v->l;l;l=(I*)*l)++n;
  result=gv(Et, n); n=0;
  for(l=v->l;l;l=(I*)*l)
    {vl=(V)l[1];result->p[n++]=MS(symjoin(vl->cx->s,vl->s));}
  R result;
}

ENTRYPOINT
A ep_alldep(A a)
{
  V v, xv; I *l, idx=0, count=0, j, err, notin, **lvec; 
  A result; struct buff *pb;
  NDC1(a);v=avlu(a,&err);
  if (!v) if(err){ERROUT(ERR_DOMAIN);}else R 0;
  if (0==(l=v->l)) R (A)gz();
  pb=buffalloc();
  for (l=v->l;l;l=(I*)*l) {
    buffputlong(pb,(I)l); count++;
  }
  lvec=(I **)(pb->min);
/* buffstuff(pb,l+1,sizeof(I)*(count=*l)); */
  while(idx<count) {
    l=lvec[idx++];
    if (v==(xv=(V)l[1])) continue;
    for(l=xv->l;l;l=(I*)*l) {
      for(j=0,notin=1;notin && j<count;++j) if(lvec[j][1]==l[1]) notin=0;
      if (notin) {
	buffputlong(pb,(I)l); count++; 
	lvec=(I **)(pb->min);
      }
    }
  }
  result=gv(Et, count);
  DO(count,xv=(V)lvec[i][1];result->p[i]=MS(symjoin(xv->cx->s,xv->s)));
  R result;
}

/*******************************************************************/

I coreLimGet(void)
{
  struct rlimit r;
  getrlimit(RLIMIT_CORE,&r);
  R r.rlim_cur;
}

#if (__sgi && ((_MIPS_SZLONG == 64) || (_MIPS_SIM == _ABIN32)))
void coreLimSet(long long n)
{
  struct rlimit64 r;
  getrlimit64(RLIMIT_CORE,&r);
  r.rlim_cur = (n<r.rlim_max)?n:r.rlim_max;
  setrlimit64(RLIMIT_CORE,&r);

  R;
}
#else
void coreLimSet(I n)
{
  struct rlimit r;
  getrlimit(RLIMIT_CORE,&r);
  r.rlim_cur = (n<r.rlim_max)?n:r.rlim_max;
  setrlimit(RLIMIT_CORE,&r);

  R;
}
#endif

/*******************************************************************/

Z I stdinFlag = 1;

void stdinFlagSet(I n)
{
  stdinFlag = n;
  if (stdinFlag) enable(); else disable();
  R;
}

/*******************************************************************/

Z I dymeVal = -1;

void dymeSet(I n)
{
	dymeVal = n;
	R;
}

/*******************************************************************/

Z I phaseOfReleaseVal = -1;

void phaseOfReleaseSet(I n)
{
  phaseOfReleaseVal = n;
  R;
}

/*******************************************************************/

Z I majorReleaseVal = -1;

void majorReleaseSet(I n)
{
  majorReleaseVal = n;
  R;
}

/*******************************************************************/

Z I minorReleaseVal = -1;

void minorReleaseSet(I n)
{
  minorReleaseVal = n;
  R;
}

/*******************************************************************/

Z A versVal=(A)0;

A versGet(void)
{
  R (versVal==(A)0)?gsv(0,"Version not set"):(A)ic(versVal);
}

void versSet(C *str)
{
  if (versVal!=(A)0) dc(versVal);
  versVal=(A)gsv(0,str);
  R;
}

/*******************************************************************/

Z A relCodeVal=(A)0;

A releaseCodeGet(void)
{
  R (relCodeVal==(A)0)?gsv(0,"Release Code not set"):(A)ic(relCodeVal);
}

void releaseCodeSet(C *str)
{
  if (relCodeVal!=(A)0) dc(relCodeVal);
  relCodeVal=(A)gsv(0,str);
  R;
}

/********************************************************************/
/*
 * System Variable get/set (_gsv/_ssv)
 *
 */


Z C *SysVarList[] = { "dyme", "vers", "pp", "mode", "stop", "Df", "Gf", 
		      "Sf", "Tf", "Xf", "cx", "rl", "stdin", "corelim",
		      "phaseOfRelease", "majorRelease", "minorRelease",
		      "releaseCode", "language", "Ef", "si", "segvexit",
		      "busexit", "loadfile", "Xfpef", "maplim",
		      "doErrorStack","CCID","autoBeamConvert",
                      "beamMSyncMode",
		      (char *)0 };

Z C *ModeList[] = { "ascii", "apl", "uni", (char *)0 };

Z C *Phase[] = { "unset", "devwork", "dev", "maintwork", "maint", 
		   "alpha", "beta", "prod", "offtrack", (char *)0};

#if defined(_CCID_none)
  Z C *CCID=0;
#elif defined(_CCID_lexa)
  Z C CCID[]="lexa";
#elif defined(_CCID_xlc)
  Z C CCID[]="xlc";
#elif defined(_CCID_suncc)
  Z C CCID[]="suncc";
#elif defined(_CCID_sgi32)
  Z C CCID[]="sgi32";
#elif defined(_CCID_sgin32)
  Z C CCID[]="sgin32";
#elif defined(_CCID_sgi64)
  Z C CCID[]="sgi64";
#elif defined(_CCID_cxx)
  Z C CCID[]="cxx" ;
#elif defined (_CCID_gcc)
  Z C CCID[]="gcc";
#elif defined(_CCID_hp32)
  Z C CCID[]="hp32";
#else
  Z C *CCID=0;
#endif

C *getaname(A aname)
{
  C *name;
  if (Ct==aname->t) name=(C *)aname->p;
  else if (Et==aname->t && QS(aname->p[0])) name=XS(aname->p[0])->n;
  else name=(C *)0;
  R name;
}

ENTRYPOINT
A ep_nc(A c,A s)
{
  V v;CX cv;A z;C *name;I err=1;
  NDC2(c,s);cv=ct(c,&err);name=getaname(s);
  if(!cv||name==(C *)0)if(err){ERROUT(ERR_DOMAIN);}else R gsym("null");
  z=gs(Et);v=svlu(cv,si(name));
  R *z->p=MS(si(!v?"null":v->e?"deps":v->a?vt[nt[v->t]]:"null")),z;
}

I MFALimitGet() ;

ENTRYPOINT
A ep_gsv(A aname)
{
  C *name;A z;
  NDC1(aname);
  name=getaname(aname);
  if (name==(C *)0) ERROUT(ERR_TYPE);

  switch(lu(name,SysVarList)) {
    CSR(1, R (A)gi(dymeVal));
    CSR(2, R versGet());
    CSR(3, R (A)gi(atol(Fs+3))); /* pp */
    CSR(4, R gsym(APLpick("apl","ascii","uni"))); /* mode */
    CSR(5, R (A)gi(sq)); /* stop */
    CSR(6, R (A)gi(Df));
    CSR(7, R (A)gi(Gf));
    CSR(8, R (A)gi(Sf));
    CSR(9, R (A)gi(Tf));
    CSR(10, R (A)gi(Xf));
    CSR(11, z=gs(Et); *z->p=MS(Cx->s); R z;);
    CSR(12, ERROUT(ERR_DOMAIN);); /* rl */
    CSR(13, R (A)gi(stdinFlag));
    CSR(14, R (A)gi(coreLimGet()));
    CSR(15, R (A)gsym(Phase[phaseOfReleaseVal]));
    CSR(16, R (A)gi(majorReleaseVal));
    CSR(17, R (A)gi(minorReleaseVal));
    CSR(18, R releaseCodeGet());
    CSR(19, R gsym("aplus")); /* language */
    CSR(20, R (A)gi(Ef));
    CSR(21, R (A)getSymKstack()); /* si */
    CSR(22, R getSigv()); /* segvexit */
    CSR(23, R getSigb()); /* busexit */
    CSR(24, R get_loadfile()); /* loadfile */
    CSR(25, R (A)gi(xfpeFlag)); /* Xfpef */
    CSR(26, R (A)gi(MFALimitGet())); /* maplim */
    CSR(27, R (A)gi(doErrorStack));
    CSR(28, R CCID ? gsym(CCID):aplus_nl);
    CSR(29, R (A)gi(getAutoBeamConvert()));
    CSR(30, R getBeamMSyncMode());              /* beamMSyncMode */
  default: ERROUT(ERR_DOMAIN);
  }
}

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#define InfItCHK(aval) \
 if (1!=aval->n) ERROUT(ERR_LENGTH); \
 if (Ft==aval->t) \
    corelim=(((double)RLIMIT_CORE)<(*(F *)(aval->p)))?RLIMIT_CORE:(int)rint(*(F *)(aval->p)); \
 else if (It==aval->t) corelim=(RLIMIT_CORE<*aval->p)?RLIMIT_CORE:*aval->p; \
 else ERROUT(ERR_TYPE); \
 if (RLIMIT_CORE!=corelim&&0>corelim) ERROUT(ERR_DOMAIN);
#else
#define InfItCHK(aval) \
 if (1!=aval->n) ERROUT(ERR_LENGTH); \
 if (Ft==aval->t) \
    corelim=(((double)INT_MAX)<(*(F *)(aval->p)))?RLIM_INFINITY:(int)rint(*(F *)(aval->p)); \
 else if (It==aval->t) corelim=*aval->p; \
 else ERROUT(ERR_TYPE); \
 if (RLIM_INFINITY!=corelim&&0>corelim) ERROUT(ERR_DOMAIN);
#endif
       
#define ItCHK(aval) if (It!=aval->t) ERROUT(ERR_TYPE); \
		       if (1!=aval->n) ERROUT(ERR_LENGTH); \
		       if (0>(ival=*aval->p)) ERROUT(ERR_DOMAIN);

#define RtCHK(aval,range) ItCHK(aval); if(range<ival)ERROUT(ERR_DOMAIN);

#define NmCHK(aval) if ((cval=getaname(aval))==(C *)0) ERROUT(ERR_TYPE);


ENTRYPOINT
A ep_ssv(A aname, A aval)
{
  C *name, *cval;I ival, rc;
  void MFALimitSet() ;
#if (__sgi && ((_MIPS_SZLONG == 64) || (_MIPS_SIM == _ABIN32)))
  long long corelim;
#else
  I corelim;
#endif
  NDC2(aname,aval);
  name=getaname(aname);
  if (name==(C *)0) ERROUT(ERR_TYPE);

  switch(lu(name,SysVarList)) {
    CSR(1, ERROUT(ERR_DOMAIN)); /* dyme */
    CSR(2, ERROUT(ERR_DOMAIN)); /* vers */
    CS(3, ItCHK(aval);Fs[3]='0'+(ival/10)%10;Fs[4]='0'+ival%10;); /* pp */
    CS(4,NmCHK(aval);if(rc=lu(cval,ModeList))APL=rc-1;else ERROUT(ERR_DOMAIN));
    CS(5, RtCHK(aval, 2); sq=ival;); /* stop */
    CS(6, RtCHK(aval, 1); Df=ival;);
    CS(7, RtCHK(aval, 2); Gf=ival;);
    CS(8, RtCHK(aval, 1); Sf=ival;);
    CS(9, RtCHK(aval, 1); Tf=ival; stdinFlagSet(Tf););
    CS(10, RtCHK(aval, 1); Xf=ival;);
    CS(11, NmCHK(aval); Cx=cx(cval););
#if defined(__VISUAL_C_2_0__)
    CS(12, ItCHK(aval);
           srand(ival);
      );
#else
    CS(12, ItCHK(aval); srandom((unsigned int)ival););
#endif
    CS(13, RtCHK(aval, 1); stdinFlagSet(ival););
    CS(14, InfItCHK(aval); coreLimSet(corelim););
    CSR(15, ERROUT(ERR_DOMAIN)); /* phaseOfRelease */
    CSR(16, ERROUT(ERR_DOMAIN)); /* majorRelease */
    CSR(17, ERROUT(ERR_DOMAIN)); /* minorRelease */
    CSR(18, ERROUT(ERR_DOMAIN)); /* releaseCode */
    CSR(19, ERROUT(ERR_DOMAIN)); /* language */
    CS(20, RtCHK(aval, 1); Ef=ival;);
    CSR(21, ERROUT(ERR_DOMAIN)); /* si */
    CS(22, RtCHK(aval, 2); setSigv(ival);); /* segvexit */
    CS(23, RtCHK(aval, 2); setSigb(ival);); /* busexit */
    CSR(24, ERROUT(ERR_DOMAIN));
    CS(25, RtCHK(aval, 1); xfpeFlag=ival;);
    CS(26, ItCHK(aval);MFALimitSet(ival););
    CS(27, RtCHK(aval, 1); doErrorStack=ival;);
    CSR(28, ERROUT(ERR_DOMAIN));               /* CCID */
    CS(29,  ItCHK(aval);setAutoBeamConvert (ival););
    CS(30, setBeamMSyncMode(aval));            /* beamMSyncMode */
  default: ERROUT(ERR_DOMAIN);
  }
  R 0;
}

#define FLG(v,def)  (((def)==(v))?' ':'*')
void dbg_flg(void){
  A a=versGet();I pp=atol(Fs+3);
  H("%s    Version:[%s]  Context:[%s]\n",CC,(char *)a->p,(Rx==Cx)?".":Cx->s->n);
  dc(a);
  H("%s   %cpp:[%-2ld]  %cmode:[%s  %cstop:[%ld]  %cstdin:[%ld]\n",CC,
    FLG(pp,10),pp,FLG(APL,1),APLpick("apl]  ","ascii]","uni]  "),
    FLG(sq,2),sq,FLG(stdinFlag,1),stdinFlag);
  H("%s   %cDf:[%ld]     %cEf:[%ld]        %cGf:[%ld]     %cSf:[%ld]\n\n",CC,
    FLG(Df,1),Df,FLG(Ef,1),Ef,FLG(Gf,1),Gf,FLG(Sf,1),Sf);
  if(1!=APL)H("%s *** Input mode set not apl.  mode:[%s]\n\n",CC,
	      ModeList[APL]);
  if(1==sq)H("%s *** stop flag not set to trace.  stop:[%ld]\n\n",CC,sq);
  if(0==sq)H("%s *** stop flag not set to trace.  stop:[%ld]\n\n",CC,sq);
  if(0==stdinFlag)H("%s *** Standard input disabled.  stdin:[%ld]\n\n",CC,
		    stdinFlag);
  if(0==Df)H("%s *** Dependencies disabled.  Df:[%ld]\n\n",CC,Df);
  if(0==Ef)H("%s *** Suspension on errors disabled.  Ef:[%ld]\n\n",CC,Ef);
  if(0==Gf)H("%s *** Protected execute (monadic do) disabled.  Gf:[%ld]\n\n",
	     CC,Gf);
  if(0==Sf)H("%s *** Callbacks disabled.  Sf:[%ld]\n\n",CC,Sf);
}

/********************************************************************/
/*
 *
 * Workspace management (_wa)
 *
 */

Z C *WaCmdList[] = { "coalesce", "size", "atmp", "avail", "fragcounts", 
		       "fragsizes", "info", (char *)0 };

#ifdef BSTUB

A ep_wa(A a)
{
  R aplus_nl;
}

#else

A ep_wa(A a)
{
  A z, z0;
  I k, n=0, len=MD, *p;
  C *name;
  NDC1(a);
  if (It==a->t || Ft==a->t) {
    if (1 != a->n) ERROUT(ERR_LENGTH);
    if (Ft==a->t) k=(I)rint(*(F *)(a->p)); else k=*a->p;
    if (-1 > k) ERROUT(ERR_DOMAIN);
    if (k>0) R gi(!tmp(k<<20));
    if (-1==k)mc();
    R gc(It,1,len,&len,(I *)mz());
  }
  else {
    if (((C *)0)==(name=getaname(a))) ERROUT(ERR_TYPE);
    switch(lu(name,WaCmdList)) {
      CSR(1, mc(); R gc(It,1,len,&len,(I *)mz())); /* coalesce */
      CSR(2, R gi(twGet())); /* size */
      CSR(3, R gi(ep_all())); /* atmp */
      CSR(4, p=(I *)mz();DO(MD,n+=p[i]*MZ[i])R gi(n<<2)); /* avail */
      CSR(5, R gc(It,1,len,&len,(I *)mz())); /* fragcounts */
      CSR(6, R gc(It,1,len,&len,(I *)MZ)); /* fragsizes */
      CSR(7, z=gv(Et,3);z0=gv(It,3);p=(I *)mz();DO(MD,n+=p[i]*MZ[i])
	 z->p[0]=(I)z0;
	 z->p[1]=(I)gc(It,1,len,&len,(I *)mz());z->p[2]=(I)gc(It,1,len,&len,(I *)MZ);
	 z0->p[0]=twGet();z0->p[1]=ep_all();z0->p[2]=n<<2;R z); /* info */
    default: ERROUT(ERR_DOMAIN);
    }
  }
}

#endif

/********************************************************************/
/*
 * namelists (_nl)
 *
 */

Z C *NlList[] = {"vars", "fns", "ops", "xfs", "sfs", "_sfs", "cxs", "cmds", 
		   "deps", "globs", "svs", "nl", "wa", "errors", "apl",
		   "ascii", "dbg", "circle", "uni", "mode", 0};

Z A nl_list(CX cxt, I n)
{
  V v; I i,count=0; A result;

  for(i=0;i<cxt->ht->nb;++i)
    for(v=(V)cxt->ht->b[i];v;v=v->v)if(nt[v->t]==n&&v->a) ++count;
  result=gv(Et,count);count=0;
  for(i=0;i<cxt->ht->nb;++i)for(v=(V)cxt->ht->b[i];v;v=v->v)
    if(nt[v->t]==n&&v->a) result->p[count++]=MS(v->s);
  R result;
}

Z A nl_deps(CX cxt)
{
  V v; I i,count=0; A result;

  for(i=0;i<cxt->ht->nb;++i)for(v=(V)cxt->ht->b[i];v;v=v->v)if(v->e) ++count;
  result=gv(Et,count);count=0;
  for(i=0;i<cxt->ht->nb;++i)for(v=(V)cxt->ht->b[i];v;v=v->v)
    if(v->e)result->p[count++]=MS(v->s);
  R result;
}

Z A nl_globs(CX cxt)
{
  V v; I i,count=0; A result;

  for(i=0;i<cxt->ht->nb;++i)for(v=(V)cxt->ht->b[i];v;v=v->v)if(qv0(v)) ++count;
  result=gv(Et,count);count=0;
  for(i=0;i<cxt->ht->nb;++i)for(v=(V)cxt->ht->b[i];v;v=v->v)
    if(qv0(v))result->p[count++]=MS(v->s);
  R result;
}

Z A nl_sfs(void)
{
  C **s=xfs_name; I count=0; A result;

  for(;*++s;)if(**s=='_'&&(*s)[1]!='_') ++count;
  result=gv(Et, count);
  for(count=0,s=xfs_name;*++s;) 
    if (**s=='_'&&(*s)[1]!='_') result->p[count++]=MS(si(*s));
  R result;
}

Z A nl__sfs(void)
{
  C **s=xfs_name; I count=0; A result;

  for(;*++s;)if(**s=='_'&&(*s)[1]=='_') ++count;
  result=gv(Et, count);
  for(count=0,s=xfs_name;*++s;) 
    if (**s=='_'&&(*s)[1]=='_') result->p[count++]=MS(si(*s));
  R result;
}

Z A nl_cxs(void)
{
  CX cx; I i,count=0; A result;

  for(i=0;i<CxTable->nb;++i)for(cx=(CX)CxTable->b[i];cx;cx=cx->n)
    if(cx->flag&1) ++count;
  result=gv(Et,count);count=0;
  for(i=0;i<CxTable->nb;++i)for(cx=(CX)CxTable->b[i];cx;cx=cx->n)
    if(cx->flag&1) result->p[count++]=MS(cx->s);
  R result;
}

Z A nl_names(C **list)
{
  C **s; I count; A result;

  for(count=0,s=list;*s;s++) ++count;
  result=gv(Et, count);
  for(count=0,s=list;*s;++s) result->p[count++]=MS(si(*s));
  R result;
}

Z A nl_primlist(int mode)
{
  A result; I count; C **ps, **ns, **s;
  ps=get_primlist(mode,0);
  ns=get_primlist(mode,1); 
  for(count=0,s=ns;*s;s++) ++count;
  for(s=ps;*s;s++) ++count;
  result=gv(Et, count);
  for(count=0,s=ns;*s;++s) result->p[count++]=MS(si(*s));
  for(s=ps;*s;++s) result->p[count++]=MS(si(*s));
  R result;
}

ENTRYPOINT
A ep_nl(A acontext, A aname)
{
  C *name;CX cxt;I err;

  if(MP(39)==(I)aname)R getCircleFuncSyms(); /* circle primitive */
  NDC2(acontext,aname);
  name=getaname(aname);
  cxt=ct(acontext,&err);
  if (name==(C *)0) ERROUT(ERR_TYPE);
  if (!cxt) if(err){ERROUT(ERR_DOMAIN);}else R(A)aplus_nl;

  switch(lu(name,NlList)) {
    CSR(1, R nl_list(cxt,1)); /* vars */
    CSR(2, R nl_list(cxt,2)); /* fns */
    CSR(3, R nl_list(cxt,3)); /* ops */
    CSR(4, R nl_list(cxt,4)); /* ext */
    CSR(5, if(cxt!=Rx)R(A)aplus_nl;R nl_sfs()); /* sfs */
    CSR(6, if(cxt!=Rx)R(A)aplus_nl;R nl__sfs()); /* _sfs */
    CSR(7, if(cxt!=Rx)R(A)aplus_nl;R nl_cxs()); /* cxs */
    CSR(8, if(cxt!=Rx)R(A)aplus_nl;R nl_names(cmdsList)); /* cmds */
    CSR(9, R nl_deps(cxt)); /* deps */
    CSR(10,R nl_globs(cxt)); /* globs */
    CSR(11,if(cxt!=Rx)R(A)aplus_nl;R nl_names(SysVarList)); /* sv */
    CSR(12,if(cxt!=Rx)R(A)aplus_nl;R nl_names(NlList)); /* nl */
    CSR(13,if(cxt!=Rx)R(A)aplus_nl;R nl_names(WaCmdList)); /* wa */
    CSR(14,if(cxt!=Rx)R(A)aplus_nl;R nl_names(es)); /* errors */
    CSR(15,if(cxt!=Rx)R(A)aplus_nl;R nl_primlist(1)); /* apl-mode primitives */
    CSR(16,if(cxt!=Rx)R(A)aplus_nl;R nl_primlist(0)); /* ascii-mode primitives */
    CSR(17,if(cxt!=Rx)R(A)aplus_nl;R nl_names(get_dbglist())); /* dbg */
    CSR(18,R(A)getCircleFuncSyms()); /* circle */
    CSR(19,if(cxt!=Rx)R(A)aplus_nl;R nl_primlist(2)); /* uni-mode primitives */
    CSR(20,if(cxt!=Rx)R(A)aplus_nl;R nl_names(ModeList)); /* mode */
  default: ERROUT(ERR_DOMAIN);
  }
}

/*******************************************************************/

ENTRYPOINT
A ep_load(A aname)
{
  C *name, *rname;
  A z;
  NDC1(aname);name=getaname(aname);
  if (name==(C*)0) ERROUT(ERR_TYPE);
  rname=doloadafile(name,0);
  if(rname==(C*)0) {
#ifdef HAVE_STRERROR
    char *errstr=strerror(errno);
    z=gv(Et,2); z->p[0]=(I)gsym("error");
    z->p[1]=(I)gsv(0,(errstr)?errstr:"unknown system error");
#else
    z=gv(Et,2); z->p[0]=(I)gsym("error");
    z->p[1]=(I)gsv(0,(errno<sys_nerr)?sys_errlist[errno]:"unknown system error");
#endif
  }
  else {z=gv(Et,2);z->p[0]=(I)gsym("ok");z->p[1]=(I)gsv(0,rname);free(rname);}
  R z;
}

ENTRYPOINT
A ep_loadrm(A aguard, A aname)
{
  C *name, *guard, *rname;
  A z;

  NDC2(aguard,aname);name=getaname(aname);guard=getaname(aguard);
  if (guard==(C *)0) ERROUT(ERR_TYPE);
  if (name==(C *)0) ERROUT(ERR_TYPE);
  if (strcmp((DEV_STRARG)guard,(DEV_STRARG)"delete")) {
    z=gv(Et,2); z->p[0]=(I)gsym("error");
    z->p[1]=(I)gsv(0,"loadrm called without specifying deletion");
    R z;
  }
  rname=doloadafile(name,1);
  if(rname==(C*)0) { 
#ifdef HAVE_STRERROR
    char *errstr=strerror(errno);
    z=gv(Et,2); z->p[0]=(I)gsym("error");
    z->p[1]=(I)gsv(0,(errstr)?errstr:"unknown system error");
#else
    z=gv(Et,2); z->p[0]=(I)gsym("error");
    z->p[1]=(I)gsv(0,(errno<sys_nerr)?sys_errlist[errno]:"unknown system error");
#endif
  }
  else {z=gv(Et,2);z->p[0]=(I)gsym("ok");z->p[1]=(I)gsv(0,rname);free(rname);}
  R z;
}

/*******************************************************************/


ENTRYPOINT
I ep_undef(A a)
{
  V v;I err;
  NDC1(a);v=avlu(a,&err);
  if(!v)if(err){ERROUT(ERR_DOMAIN);}else R 1;
  R v->e?(rmd(v),0):1;
}

ENTRYPOINT
I ep_ex(A a)
{
  V v;I err;
  I emptyV() ;
  NDC1(a);v=avlu(a,&err);
  if(!v)if(err){ERROUT(ERR_DOMAIN);}else R 1;
  R(qv0(v)&&!v->o)?(emptyV(v),0):1;
}

/* global name - core function for ep_excxt */
I excxt(CX cx)
{
  I i;V v;

  if (!cx||Rx==cx) R 1;
  for(i=0;i<cx->ht->nb;++i)for(v=(V)cx->ht->b[i];v;v=v->v)
    if(qv0(v))R 1;
  cx->flag&=~1;
  R 0;
}

ENTRYPOINT
I ep_excxt(A a){I err;NDC1(a);R excxt(ct(a,&err));}

ENTRYPOINT /* exitpoint? :^) */
void ep_exit(I rc){exit((int)rc);}	/* I -> int */

ENTRYPOINT
void ep_abortload(void){
	void setAbortLoad() ;

	setAbortLoad(1);
}

Z C pwd[MAXPATHLEN+8]="PWD=";
#if defined(HAVE_SVR4) || defined(__NetBSD__)
void setPWD(void){getcwd(pwd+4, MAXPATHLEN+8-4);putenv(pwd);}
#else
void setPWD(void){getwd(pwd+4);putenv(pwd);}
#endif

ENTRYPOINT
A ep_cd(A a)
{
  A z;
  C *name;

  NDC1(a);
  name=getaname(a);
  if (name==(C *)0) ERROUT(ERR_TYPE);
  if(chdir(*name?name:getenv("HOME"))) {
#ifdef HAVE_STRERROR
    char *errstr=strerror(errno);
    z=gv(Et,2); z->p[0]=(I)gsym("error");
    z->p[1]=(I)gsv(0,(errstr)?errstr:"unknown system error");
#else
    z=gv(Et,2); z->p[0]=(I)gsym("error");
    z->p[1]=(I)gsv(0,(errno<sys_nerr)?sys_errlist[errno]:"unknown system error");
#endif
  }
  else {setPWD();z=gv(Et,1);z->p[0]=(I)gsym("ok");}
  R z;
}

/*******************************************************************/

ENTRYPOINT
A ep_hashstat(A a)
{
  HT ht; A z; I i;V v;NDC1(a);
  if (qz(a)) R SymbolTableHashChainLengths();
  if (Et!=a->t||1!=a->n||!QS(*a->p)) ERROUT(ERR_DOMAIN);
  ht=cxi(XS(*a->p))->ht;
  z=gv(It,ht->nb);bzero(z->p,ht->nb*sizeof(I));
  for(i=0;i<ht->nb;++i)for(v=(V)ht->b[i];v;v=v->v)z->p[i]++;
  R z;
}

ENTRYPOINT
A ep_symstat(void)
{
  R SymbolTableBlockInfo();
}

extern A showLastSavedKstack();
ENTRYPOINT
A ep_doErrorStack()
{
  R showLastSavedKstack();
}

void nsfInstall(void)
{
/**/
  install((PFI)ep_nc,       "_nc",       A_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep_scb,      "_scb",      V_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep_gcb,      "_gcb",      A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_spcb,     "_spcb",     V_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep_gpcb,     "_gpcb",     A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep__srcb,    "__srcb",    V_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep__grcb,    "__grcb",    A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep__sprcb,   "__sprcb",   V_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep__gprcb,   "__gprcb",   A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_scd,      "_scd",      A_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep_gcd,      "_gcd",      A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep__sscd,    "__sscd",    A_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep__gscd,    "__gscd",    A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep__utd,     "__utd",     IV, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_gsv,      "_gsv",      A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_ssv,      "_ssv",      A_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep_nl,       "_nl",       A_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep_def,      "_def",      A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_dep,      "_dep",      A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_alldep,   "_alldep",   A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_load,     "_load",     A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_loadrm,   "_loadrm",   A_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep_undef,    "_undef",    IV, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_ex,       "_ex",       IV, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_excxt,    "_excxt",    IV, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_exit,     "_exit",     V_, 1, IV,  0,0,0,0,0,0,0);
  install((PFI)ep_abortload,"_abortload",V_, 0, 0,   0,0,0,0,0,0,0);
  install((PFI)ep_cd,       "_cd",       A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_hashstat, "_hashstat", A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_symstat,  "_symstat",  A_, 0, 0,   0,0,0,0,0,0,0);
  install((PFI)ep_wa,       "_wa",       A_, 1, A_,  0,0,0,0,0,0,0);
  install((PFI)ep_xfsinfo,  "__xinfo",   A_, 0, 0,   0,0,0,0,0,0,0);
  install((PFI)ep_doErrorStack,"_doErrorStack", A_, 0, 0,   0,0,0,0,0,0,0);
  R;
}


