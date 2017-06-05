/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.
*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/* This file contains debugging aids internal to A+. */

#include <stdio.h>
#include <string.h>
#include <a/ik.h>
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>
#include <a/arthur.h>


#define FFL  if(dbg_pr)fflush(stdout)
#define DH   if(dbg_pr)printf

I dbg_tl=0;       /* load trace flag */
I dbg_tb=0;       /* beam trace flag */
I dbg_tf=0;       /* function call trace flag */
I dbg_tx=0;       /* external function trace flag */
I dbg_ts=0;       /* system function trace flag */
I dbg_tbwc=0;     /* bitwise non-boolean checking flag */
I dbg_tdef=0;     /* func/dep definition tracing flag */
I dbg_tdep=0;     /* dependency evaluation tracing flag */
I dbg_tinv=0;     /* dependency invalidation tracing flag */
I dbg_tnan=0;     /* NaN checking flag */
I dbg_tpcb=0;     /* preset-callback trace flag */
I dbg_tscb=0;     /* set-callback tracing flag */
I dbg_tpack=0;    /* packfile trace flag */
I dbg_tprcb=0;    /* prereference-callback trace flag */
I dbg_trcb=0;     /* reference-callback trace flag */
I dbg_txeq=0;     /* execute trace flag */
I dbg_tmdo=0;     /* monadic-do trace flag */
I dbg_tdyld=0;    /* dynamic load tracking flag */
I dbg_depth=0;    /* function call and loading "depth" */
I dbg_tmstk=0;    /* toolkit trace flag */
I dbg_tkerr=0;    /* toolkit error trace flag */
I dbg_xfpe=0;     /* trace external floating-point errors */

I dbg_tfmt=0;     /* display fmt error messages */
I dbg_twa=0;      /* wa trace flag */
I dbg_tdoErrorStack=0; /* doErrorStack trace flag */

Z I dbg_hold=0;   /* internal flag to turn off tracing during _dbg calls */
Z I dbg_pr=1;     /* display messages? */
Z I dbg_levels=0; /* maximum tracing depth */
Z I dbg_ti=2;     /* trace indentation */
Z C dbg_ch=' ';   /* delimiting char in trace display */
Z A dbg_cbfunc=0; /* dbg-callback function */
Z A dbg_cbdata=0; /* dbg-callback fixed data */

#define CXL_SIZE 128

Z I cxl[CXL_SIZE]={0};

Z C trcb[128];

Z C *blurb[]={"enter","exit","abort"};

static void dbg_stat(void);

Z I nix(S s){S*l=(S*)cxl;I x;if(!*cxl)R 0;x=0>*cxl;
	     while(*++l)if(s==*l)R x;R!x;}

Z void callafunc(A func,A cbdata,A arg0,I n,A arg1,A arg2,A arg3) 
{
  E e;
  e = (E)(ma(7)); 
  e->n=5; e->f=(I)func; e->a[0]=(I)cbdata;
  e->a[1]=(I)arg0;
  e->a[2]=(I)((1<=n)?arg1:aplus_nl);
  e->a[3]=(I)((2<=n)?arg2:aplus_nl);
  e->a[4]=(I)((3<=n)?arg3:aplus_nl);
  dc((A)ez(ME(e))); mf((I *)e); 
}

Z void dbg_callcb(C *s,I n,A arg1,A arg2,A arg3)
{
  if(dbg_hold)R;
  dbg_hold=1;
  {
    A event=ge(MS(si(s)));
    callafunc(dbg_cbfunc,dbg_cbdata,event,n,arg1,arg2,arg3);
    dc(event);
  }
  

  /*   if(1<=n)dc(arg1);if(2<=n)dc(arg2);if(3<=n)dc(arg3); */

  if(n==1)      dc(arg1);
  else if(n==2) dc(arg1),dc(arg2);
  else if(n==3) dc(arg1),dc(arg2),dc(arg3);

  dbg_hold=0;
}

Z C *dbg_pfx(void){I idx=dbg_depth-1,i;
  idx=MIN(99,4+MAX(0,idx)*dbg_ti);memset(trcb,' ',idx);trcb[idx]='\0';
  if(dbg_ch!=' ')for(i=0;i<dbg_depth-1;++i)trcb[4+(i*dbg_ti)]=dbg_ch;
  R trcb;
}
#if defined(HAVE_SVR4)  && !defined(__sgi) && !defined(__osf__)
I nanbeamchk(C *f,A af)
{
  F *fp=(F *)af->p;
  I nans=0,infs=0,cl;
  if(Ft!=af->t)R 0;
  DO(af->n,cl=fpclass(fp[i]);if(cl==FP_NINF||cl==FP_PINF)++infs;if(cl==FP_SNAN |cl==FP_QNAN)++nans;);
  if(infs){DH("\343%sWarning: found %ld Inf%s in %s.\n",
	      dbg_pfx(),infs,(1==infs)?"":"s",f);FFL;}
  if(nans){DH("\343%sWarning: found %ld Nan%s in %s.\n",
	      dbg_pfx(),nans,(1==nans)?"":"s",f);FFL;}
  if(dbg_cbfunc&&(nans||infs))
    {
      dbg_callcb("nan",2,
		 gsv(0,f),
		 gvi(It,2,infs,nans),
		 (A)0);
    }
}
#else
#define FPCLASS(x) (NANMASK==((x)&NANMASK)? \
		     ((x)&0x000fffff?((x)&0x00080000?3:4):2): \
		     (x)&0x00080000?1:0)

#if defined(__sgi)
fpclass_t fpclass(F x)
#else
I fpclass(F x)
#endif
{
  BD fp;
  fp.d=x;
  R FPCLASS(fp.i[0]);
}

I nanbeamchk(C *f,A af)
{
  F *fp=(F *)af->p;
  I nans=0,infs=0,cl;
  if(dbg_hold||Ft!=af->t)R 0;
  DO(af->n,cl=fpclass(fp[i]);if(2==cl)++infs;if(3<=cl)++nans;);
  if(infs){DH("\343%sWarning: found %ld Inf%s in %s.\n",
	      dbg_pfx(),infs,(1==infs)?"":"s",f);FFL;}
  if(nans){DH("\343%sWarning: found %ld Nan%s in %s.\n",
	      dbg_pfx(),nans,(1==nans)?"":"s",f);FFL;}
  if(dbg_cbfunc&&(nans||infs))
    {
      dbg_callcb("nan",2,
		 (A)gsv(0,f),
		 gvi(It,2,infs,nans),
		 (A)0);
    }
  R -1;
}
#endif
I bitwisechk(A a,A w,I i)
{
  I anonbool=0, wnonbool=0, tot=0;
  if(dbg_hold)R 0;
  DO(a->n,if((unsigned long)(a->p[i])&(unsigned long)(~0x01))++anonbool;);
  if(w)DO(w->n,if((unsigned long)(w->p[i])&(unsigned long)(~0x01))++wnonbool;);
  if(anonbool||wnonbool)
  {
    C **primnames=get_primlist(APL,0); /* APL either 0 or 1 */
    tot=anonbool+wnonbool;
    if(anonbool&&wnonbool)
    {
      DH("\343%sWarning: found %ld non-boolean%s in both arguments of %s.\n",
	 dbg_pfx(),tot,(1<tot)?"s":"",primnames[i]);
    }
    else if (anonbool)
    {
      DH("\343%sWarning: found %ld non-boolean%s in%s argument of %s.\n",
	 dbg_pfx(),tot,(1<tot)?"s":"",(w)?" left":"",primnames[i]);
    }
    else
    {
      DH("\343%sWarning: found %ld non-boolean%s in right argument of %s.\n",
	 dbg_pfx(),tot,(1<tot)?"s":"",primnames[i]);
    }
  }
  FFL;
  R -1;
}

I xfpechk(C *xfname,I nerrors)
{
  if(nerrors)
  {
    DH("\343%sWarning: encountered %ld FPEs in execution of %s.\n",
       dbg_pfx(),nerrors,xfname);
    FFL;
  }
  R 0;
}

I functrc(A f,I x){
  CX cx=(CX)f->p[f->n+2];
  if(dbg_hold||nix(cx->s)||(dbg_levels&&dbg_levels<dbg_depth))R 0;
  DH("\343%s%s.%s %sed\n",dbg_pfx(),cx->s->n,(XS(*f->d))->n,blurb[x]);
  FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("func",2,
		 ge(MS(symjoin(cx->s,XS(*f->d)))),
		 ge(MS(si(blurb[x]))),
		 (A)0);
      R -1;
    }

  R -1;
}

I xftrc(C *f,I x){
  S cxsym;
  if(dbg_hold||(!(('_'==*f)?dbg_ts:dbg_tx))||
     (dbg_levels&&dbg_levels<dbg_depth))R 0;
  if('_'!=*f&&cxl[0]){symsplit(si(f),&cxsym);if(nix(cxsym))R 0;}
  DH("\343%s%s (%s function) %sed\n",dbg_pfx(),f,
     ('_'==*f)?"system":"external",blurb[x]);
  FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb('_'==*f?"sfs":"xfs",2,
		 ge(MS(si(f))),
		 ge(MS(si(blurb[x]))),
		 (A)0);
    }

  R -1;
}

I loadtrc(C *f,I x){
  if(dbg_hold)R 0;
  DH("\343%s%s %s %s\n",dbg_pfx(),
     x?"Load of":"Loading",f,!x?". . . ":1==x?"finished":"FAILED");
  FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("load",2,
		 (A)gsv(0,f),
		 ge(MS(si(blurb[x]))),
		 (A)0);
    }

  R -1;
}

I xeqtrc(C *str,I x){
  if(dbg_hold)R 0;
  DH("\343%s%s%s execute of string [%s%s\n",
     dbg_pfx(),(x&1)?"Entering":"Exiting",(x&2)?" protected":"",
     (40<strlen(str))?"<too long>":str,x?"] . . .":"]");
  FFL;
  if(dbg_cbfunc) 
    {
      dbg_callcb("xeq",2,
		 (A)gsv(0,str),
		 ge(MS(si(blurb[(x&1)?0:1]))),
		 (A)0);
    }

  R -1;
}

I mdotrc(I x){
  if(dbg_hold)R 0;
  DH("\343%s%s\n",
     dbg_pfx(),x?"Entering monadic do . . .":"Exiting monadic do");
  FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("mdo",1,
		 ge(MS(si(blurb[x?0:1]))),
		 (A)0,(A)0);
    }

  R -1;
}

static C *beamsym[]={"out","in","unmap"};

I beamtrc(C *f,I x,I i){ /* Note that i is used only if 1==x. */
  if(dbg_hold)R 0;
  f=f?f:"<?>";
  if (1==x){ DH("\343%sBeaming in (%ld) %s\n",dbg_pfx(),i,f);}
  else{DH("\343%s%s %s\n",dbg_pfx(),
	  (x==3)?"Converting":((x==2)?"Unmapping":"Beaming out"),
	  f);}
  FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("beam",(1==x)?3:2,
		 (A)gsv(0,f),
		 ge(MS(si(beamsym[x]))),
		 (1==x)?gi(i):0);
    }

  R -1;
}

Z C *dbgCbSymList[] = {"scb","pcb","rcb","prcb"};
Z C *dbgCbNameList[] = {"Set","Preset","Reference","Prereference"};

I packtrc(C *fname,C *cmdstr,I x)
{
  if(dbg_hold)R 0;
  if(!x)--dbg_depth;
  DH("\343%s%s p.%s on file %s\n",dbg_pfx(),
     x?"Entering":"Exiting",cmdstr,fname);
  FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("pack",2,
		 (A)gsv(0,fname),
		 ge(MS(si(blurb[x?0:1]))),
		 (A)0);
    }

  if(x)++dbg_depth;
R 0;
}

I cbtrc(V v,I x){
  if(dbg_hold||nix(v->cx->s))R 0;
  DH("\343%s%s-callback firing on %s.%s\n",dbg_pfx(),dbgCbNameList[x],
    v->cx->s->n,v->s->n);
  FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb(dbgCbSymList[x],1,
		 ge(MS(symjoin(v->cx->s,v->s))),
		 (A)0,(A)0);
    }

  R -1;
}

I deptrc(V v,I x){
  if(dbg_hold||nix(v->cx->s))R 0;
  ++dbg_depth;
  DH("\343%sDependency %s.%s evaluation %sed\n",dbg_pfx(),v->cx->s->n,v->s->n,
    blurb[x]);
  FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("dep",2,
		 ge(MS(symjoin(v->cx->s,v->s))),
		 ge(MS(si(blurb[x?0:1]))),(A)0);
    }

  --dbg_depth;
  R -1;
}

I invtrc(V v,I x){
  A arg2;
  if(dbg_hold||nix(v->cx->s))R 0;
  if(x)
  {
    DH("\343 ***%sDependency cycle detected: %s.%s\n",
       dbg_pfx(),v->cx->s->n,v->s->n);
    FFL;
    arg2=ge(MS(si("cycle")));
  } else {
    DH("\343%sDependency %s.%s invalidated\n",dbg_pfx(),v->cx->s->n,v->s->n);
    FFL;
    arg2=aplus_nl;
  }
  if(dbg_cbfunc)
    {
      dbg_callcb("inv",2,
		 ge(MS(symjoin(v->cx->s,v->s))),
		 arg2,
		 (A)0);
    }

  R -1;
}

void dyldtrc(C *f){
  DH("\343%sDynamic load from %s\n",dbg_pfx(),f);FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("dyld",1,
		 (A)gsv(0,f),
		 (A)0,(A)0);
    }

}

void doErrorStacktrc(I q_, A aStack_){
  C *lastLine=(C *)((A)aStack_->p[aStack_->n-1])->p;
  DH("\343%sdoErrorStack:(%ld) %.40s ...\n",dbg_pfx(),q_,lastLine);FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("doErrorStack",1,
		 (A)gvi(Et,2,gi(q_),
			ic(aStack_)),
		 (A)0,(A)0);
    }
}

void watrc(I w){
  DH("\343%sdoErrorStack %ld\n",dbg_pfx(),w);FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("wa",1,
		 (A)gi(w),
		 (A)0,(A)0);
    }
}

I deftrc(V v,I x){
  if(dbg_hold||nix(v->cx->s))R 0;
  DH("\343%s%s %s.%s defined\n",dbg_pfx(),x?"Dependency":"Function",
    v->cx->s->n,v->s->n);FFL;
  if(dbg_cbfunc)
    {
      dbg_callcb("def",2,
		 ge(MS(symjoin(v->cx->s,v->s))),
		 ge(MS(si(x?"dep":"func"))),
		 (A)0);
    }

  R -1;
}

Z void dbg_error(C *s,I idx)
{
  H("\343 dbg error: \"%s\" %s\n",s,s[idx]?"not recognized":"is ambiguous");
  FFL;
}

Z C *dbg_helpmsg0[]={
"General Syntax",
"  $dbg         - displays dbg flags turned on",
"  $dbg +flag   - turns a flag on",
"  $dbg -flag   - turns a flag off", 
"  Several $dbg commands can be strung together: $dbg +load +beam",
"  $dbg +cxt or -cxt must be the last command in a list.",
"FLAGS",
0};
Z C *dbg_helpflagsA[]={
"Function Tracing",
" +func  - function",
" +xfs   - external function",
" +sfs   - system function",
"Callback/Dependency Tracing",
" +dep   - dep evaluation",
" +inv   - dep invalidation",
" +scb   - set-callback",
" +pcb   - preset-callback",
" +_prcb - prereference-cb",
" +_rcb  - reference-cb",
"Execution Tracing",
" +do  - monadic-do",
" +xeq - execution",
"Definition Tracking",
" +def - func/dep definition",
0};
Z C *dbg_helpflagsB[]={
"File Tracking",
" +load - load",
" +dyld - dynamic load",
" +beam - beam",
" +pack - packfile",
"Error Checking",
" +nan     - NaN checking",
" +bitwise - non-boolean ands/ors",
" +xfpe    - external FPEs",
"Message Control",
" +fmt   - _fmt messages",
" +mstk  - MStk debug messages",
" +tkerr - MStk error messages",
"Special",
" +print - print dbg messages",
" +all   - all dbg flags on",
" -all   - all dbg flags off",
0};
Z C *dbg_helpmsg1[]={
"Other Dbg Options",
"  display beam   - displays list of beamed files.",
"  display flags  - displays setting of system vars.",
"  help        - prints this message.",
"  char <X>    - sets trace char to <X>. (e.g. $dbg char |)",
"  indent <#>  - sets trace indent to <#>.",
"  levels <#>  - sets max. depth of function tracing to <#>.  0=off.",
"  -cxt [list of cxts] - trace all contexts except those listed.",
"  +cxt [list of cxts] - trace only listed contexts. (e.g. $dbg +cxt s)",
0};

Z void dbg_help(void)
{
  C **sv,**svb;
  for(sv=dbg_helpmsg0;*sv;++sv) H("\343 %s\n",*sv);
  for(sv=dbg_helpflagsA,svb=dbg_helpflagsB;
      *sv||*svb;
      sv+=(*sv)?1:0,svb+=(*svb?1:0))
    H("\343 %-32s%s\n",*sv?*sv:"",*svb?*svb:"");
  for(sv=dbg_helpmsg1;*sv;++sv) H("\343 %s\n",*sv);
  H("\343 Current Settings\n");
  dbg_stat();
}

Z A dbghelp_vlist()
{
  C **s; I count; A result;

  for(count=0,s=dbg_helpmsg0;*s;s++) ++count;
  for(s=dbg_helpflagsA;*s;s++) ++count;
  for(s=dbg_helpflagsB;*s;s++) ++count;
  for(s=dbg_helpmsg1;*s;s++) ++count;
  result=gv(Et, count);
  for(count=0,s=dbg_helpmsg0;*s;++s) result->p[count++]=(I)gsv(0,*s);
  for(s=dbg_helpflagsA;*s;++s) result->p[count++]=(I)gsv(0,*s);
  for(s=dbg_helpflagsB;*s;++s) result->p[count++]=(I)gsv(0,*s);
  for(s=dbg_helpmsg1;*s;++s) result->p[count++]=(I)gsv(0,*s);
  R result;
}

#define FLAGSPERLINE 10

#define SHOWFLAG(flag,name) if(flag) \
                              { \
			       if(0==anyon++%FLAGSPERLINE) \
                                 H("%s\343 dbg: flags set:",FLAGSPERLINE<anyon?"\n":""); \
				 H(" +%s",name); \
			      }

Z void dbg_stat(void){
  S *sl=(S *)cxl;
  A cbf=dbg_cbfunc;
  I anyon=0;
  SHOWFLAG(dbg_tf,"func");
  SHOWFLAG(dbg_tx,"xfs");
  SHOWFLAG(dbg_ts,"sfs");
  SHOWFLAG(dbg_tdep,"dep");
  SHOWFLAG(dbg_tinv,"inv");
  SHOWFLAG(dbg_tscb,"scb");
  SHOWFLAG(dbg_tpcb,"pcb");
  SHOWFLAG(dbg_tdef,"def");
  SHOWFLAG(dbg_tmdo,"do");
  SHOWFLAG(dbg_txeq,"xeq");
  SHOWFLAG(dbg_trcb,"_rcb");
  SHOWFLAG(dbg_tprcb,"_prcb");
  SHOWFLAG(dbg_tl,"load");
  SHOWFLAG(dbg_tb,"beam");
  SHOWFLAG(dbg_tnan,"nan");
  SHOWFLAG(dbg_tdyld,"dyld");
  SHOWFLAG(dbg_tpack,"pack");
  SHOWFLAG(dbg_tfmt,"fmt");
  SHOWFLAG(dbg_twa,"wa");
  SHOWFLAG(dbg_tdoErrorStack,"doErrorStack");
  SHOWFLAG(dbg_tbwc,"bitwise");
  SHOWFLAG(dbg_xfpe,"xfpe");
  SHOWFLAG(dbg_tmstk,"mstk");
  SHOWFLAG(dbg_tkerr,"tkerr");
  H(anyon?"\n":"\343 dbg: No flags set.\n");
  H("\343 dbg: (%cprint indent:%ld char:[%c]",dbg_pr?'+':'-',dbg_ti,dbg_ch);
  if(dbg_levels)H(" levels:%ld)",dbg_levels);else H(")");
  if(*cxl){
    H("\n\343 dbg: %ccxt",(0<*cxl)?'+':'-');
    if(cxl[1])while(*++sl){if(Rx->s==*sl)H(" .");else H(" %s",(*sl)->n);}
    else H(" none");
  }
  NL;
  if(cbf){
    CX cx=(CX)cbf->p[cbf->n+2];
    H("\343 dbg: _dbg callback: %s.%s\n",cx->s->n,(XS(*cbf->d))->n);
  }
}

Z void dbg_all(C c)
{
  I val=('1'==c)?1:0;
  dbg_tf=dbg_tx=dbg_ts=dbg_tl=dbg_tb=dbg_tdep=dbg_tinv=dbg_tscb=
    dbg_tpcb=dbg_tdyld=dbg_tdef=dbg_tprcb=dbg_trcb=val;
}

Z I dbg_setcxt(I cxl0,C *s){
  I idx=0;C *w=bl(s),d=*w;*w=0;cxl[0]=cxl0;
  for(;*s;*w=d,w=bl(s=cl(w)),d=*w,*w=0)
    if(idx<CXL_SIZE-2)cxl[++idx]=(I)si(strcmp(s,".")?s:"");
    else{
      DH("\343 Bonk!  Hard limit hit: max [+/-]cxt #:%d\n",CXL_SIZE-2);FFL;
      cxl[0]=0;R 1;
    }
  cxl[idx+(idx?1:0)]=0;
R 0;
}

Z A dbg_acxt(A aarg)
{
  A a0,a1;S cmd;I cxl0=cxl[0];
  if(qz(aarg)){
    I count=0;A z;I *sl;
    if(0==cxl[0])R gvi(Et,2,ge(MS(si("all"))),0);
    for(sl=(I *)cxl;*++sl;++count);
    z=gv(Et,count);DO(count,z->p[i]=MS(cxl[i+1]));
    R gvi(Et,2,ge(MS(si(0<cxl[0]?"only":"not"))),z);
  }
  if(Et!=aarg->t)ERROUT(ERR_TYPE);if(2!=aarg->n)ERROUT(ERR_DOMAIN);
  a0=(A)aarg->p[0];a1=(A)aarg->p[1];
  if(Et!=a0->t||1!=a0->n||!QS(*a0->p))ERROUT(ERR_DOMAIN);
  cmd=XS(*a0->p);
  if(cmd==si("all")){if(!qz(a1))ERROUT(ERR_DOMAIN);cxl[0]=0;R 0;}
  else if(cmd==si("only"))cxl[0]=1;
  else if(cmd==si("not"))cxl[0]=-1;
  else ERROUT(ERR_DOMAIN);
  if((a1->n&&!sym(a1))||1<a1->r){cxl[0]=cxl0;ERROUT(ERR_DOMAIN);}
  if((CXL_SIZE-2)<a1->n){
    H("\343 Bonk!  Hard limit hit: max [+/-]cxt #:%d\n",CXL_SIZE-2);FFL;
    cxl[0]=cxl0;R 0;
  }
  DO(a1->n,cxl[i+1]=(I)XS(a1->p[i]));
  R 0;
}


Z A dbg_cb(A aarg)
{
  I f,c,z;
  if(qz(aarg))R gvi(Et,2,ic(dbg_cbfunc),ic(dbg_cbdata));
  if(aarg->t!=Et||aarg->n!=2) ERROUT(ERR_DOMAIN);
  f=aarg->p[0];c=aarg->p[1];
  z=qz((A)f);
  if(!(QF(f)&&!QS(f))&&!z) ERROUT(ERR_NONFUNCTION);
  dc(dbg_cbfunc);dc(dbg_cbdata);
  if(z){dbg_cbfunc=(A)0;dbg_cbdata=(A)0;}
  else{dbg_cbfunc=(A)ic((A)f);dbg_cbdata=(A)ic((A)c);}
  R 0;
}

#define SWDEF(s,idx) default:dbg_error(s,idx);break;

#define CF(i,f) CS(i,f=(!*x)?(!f):*x=='1'?1:0)

#define GETF(f)	    (1==arg)?1:(0==arg)?0:(1==pref)?1:(0==pref)?0:!f

#define CNF(i,f) CS(i, \
		    f=GETF(f); \
		    if(1==arg||0==arg)usearg=1)

Z void dbg_display(C *x)
{
  if(!*x){dbg_stat();R;}
  switch(*x){CS('b',dbg_mfr());CS('f',dbg_flg());CS('m',(void)dbg_mfa());SWDEF(x,0);}
}

extern A dbg_mfrsf();
Z A dbg_getdisplay(A aarg)
{
  if(!sym(aarg))ERROUT(ERR_DOMAIN);
  if(MS(si("beam"))==*aarg->p) R dbg_mfrsf();
  else ERROUT(ERR_DOMAIN);
}

long dbgproc(C *s, C *x)
{
  I pref, arg, usearg, temp=0;
  if(!*s)R 0;
  pref=('+'==*s)?1:('-'==*s)?0:-1;
  arg=(!x)?-1:(!*x)?-1:('1'==*x)?1:('0'==*x)?0:-2;
  if(-1!=pref)++s;  /* advance s past prefix */
  usearg=0;
  switch (*s){
    CS('a',temp=GETF(temp);dbg_all(temp?'1':'0'));
    CS('b',switch(s[1]){
      CNF('e',dbg_tb);
      CNF('i',dbg_tbwc);
      SWDEF(s,1);
    });
    CS('c',switch(s[1]){
      CS('h',dbg_ch=(!*x)?' ':*x;usearg=1);
      CS('x',dbg_setcxt((0==pref?-1:1),x);usearg=2);
      SWDEF(s,1);
    });
    CS('d',switch(s[1]){
      CS('e',switch(s[2]){
	CNF('f',dbg_tdef);
	CNF('p',dbg_tdep);
	SWDEF(s,2);
      });
      CS('i',dbg_display(x);usearg=1);
      CS('o',switch(s[2]){
	CNF('\0',dbg_tmdo);
        CNF('E',dbg_tdoErrorStack);
      });
      CNF('y',dbg_tdyld);
      SWDEF(s,1);
    });
    CS('f',switch(s[1]){
      CNF('u',dbg_tf);
      CNF('m',dbg_tfmt);
      SWDEF(s,1);
    });
    CS('h',dbg_help());
    CS('i',switch(s[1]){
      CS('n',switch(s[2]){
	CS('d',dbg_ti=(!*x)?2:('-'==*x)?0:atol(x);usearg=(*x)?1:0);
	CNF('v',dbg_tinv);
	SWDEF(s,2);
      });
      SWDEF(s,1);
    });
    CS('l',switch(s[1]){
      CS('e',dbg_levels=(!*x)?0:('-'==*x)?0:atol(x);usearg=(*x)?1:0;);
      CNF('o',dbg_tl);
      SWDEF(s,1);
    });
    CNF('m',dbg_tmstk);
    CNF('n',dbg_tnan);
    CS('p',switch(s[1]){
      CNF('a',dbg_tpack);
      CNF('c',dbg_tpcb);
      CNF('r',dbg_pr);
      SWDEF(s,1);
    });
    CS('s',switch(s[1]){
      CNF('c',dbg_tscb);   
      CNF('f',dbg_ts);
      SWDEF(s,1);
    });
    CNF('t',dbg_tkerr);
    CF('w',dbg_twa);
    CS('x',switch(s[1]){
      CNF('e',dbg_txeq);
      CS('f',switch(s[2]){
	CNF('p',dbg_xfpe);
	CNF('s',dbg_tx);
	SWDEF(s,2);
      });
      SWDEF(s,1);
    });
    CS('_',switch(s[1]){
      CNF('p',dbg_tprcb);
      CNF('r',dbg_trcb);
      SWDEF(s,1);
    });
    case '0':CS('1',dbg_all(*s));
    SWDEF(s,0);
  }
  R usearg;
}

#define ADVANCEARG(s,x) if (usearg) ADVANCETWO(s,x) else ADVANCEONE(s,x)
#define ADVANCEONE(s,x) { s=x;x=cl(w=bl(s)),*w=0; }
#define ADVANCETWO(s,x) { s=cl(u=bl(x)),*u=0,x=cl(w=bl(s)),*w=0; }

void dbg(C *s, C *x)
{
  I usearg=0;
  C *w, *u;
  if(!*s){dbg_stat();R;}
  if('1'==*s||'0'==*s){dbg_all(*s);ADVANCEARG(s,x);}
  while(*s)
  {
    usearg=dbgproc(s,x);
    if(2==usearg)break;
    ADVANCEARG(s,x);
  }
}  

Z C *dbgList[] = {"func", "xfs", "sfs", "dep", "inv", "scb", "pcb", "def",
		    "load", "dyld", "beam", "nan", "dbghelp", "char",
		    "indent", "cxt", "print", "cb", "fmt", "_prcb", "_rcb",
		    "xeq", "do", "levels", "bitwise", "pack", "mstk", 
		    "display", "xfpe", "tkerr", "wa", "doErrorStack", 0};

C **get_dbglist(void){R dbgList;}

#define DCF(x,v,f) CSR(x,if(isn)R gi(f); \
                      else if(v){f=iarg;R 0;} \
                      else ERROUT(ERR_DOMAIN));

#define DCFC(x,f) CSR(x,if(isn){A z=gs(Ct);(*(C*)(z->p))=dbg_ch;R z;} \
                      else if(isc){dbg_ch=carg;R 0;} \
                      else ERROUT(ERR_DOMAIN));

Z A ep_dbg(A asym,A aarg)
{
  C *cmd,carg=0;
  I isn,iarg=0,isi,isb,isc;
  NDC2(asym,aarg);cmd=getaname(asym);if(cmd==(C*)0)ERROUT(ERR_TYPE);
  isn=qz(aarg);
  isi=(It==aarg->t&&1==aarg->n);if(isi)iarg=*aarg->p;
  isb=(isi&&(0==iarg||1==iarg));
  isc=(Ct==aarg->t&&1==aarg->n);if(isc)carg=(*(C*)(aarg->p));
  switch(lu(cmd,dbgList)){
    DCF(1,isb,dbg_tf); /* func */
    DCF(2,isb,dbg_tx); /* xfs */
    DCF(3,isb,dbg_ts); /* sfs */
    DCF(4,isb,dbg_tdep); /* dep */
    DCF(5,isb,dbg_tinv); /* inv */
    DCF(6,isb,dbg_tscb); /* scb */
    DCF(7,isb,dbg_tpcb); /* pcb */
    DCF(8,isb,dbg_tdef); /* def */
    DCF(9,isb,dbg_tl); /* load */
    DCF(10,isb,dbg_tdyld); /* dyld */
    DCF(11,isb,dbg_tb); /* beam */
    DCF(12,isb,dbg_tnan); /* nan */
    CSR(13,R dbghelp_vlist()); /* dbghelp */
    DCFC(14,dbg_ch); /* char */
    DCF(15,isi,dbg_ti); /* indent */
    CSR(16,R dbg_acxt(aarg)); /* cxt */
    DCF(17,isb,dbg_pr); /* print */
    CSR(18,R dbg_cb(aarg)); /* cb */
    DCF(19,isb,dbg_tfmt); /* fmt */
    DCF(20,isb,dbg_tprcb); /* _prcb */
    DCF(21,isb,dbg_trcb); /* _rcb */
    DCF(22,isb,dbg_txeq); /* xeq */
    DCF(23,isb,dbg_tmdo); /* do */
    CSR(24,if(isn)R gvi(It,2,dbg_levels,dbg_depth);else if(isi)
       {dbg_levels=iarg;R 0;}else ERROUT(ERR_DOMAIN)); /* levels */
    DCF(25,isb,dbg_tbwc); /* bitwise */
    DCF(26,isb,dbg_tpack); /* pack */
    DCF(27,isb,dbg_tmstk); /* toolkit */
    CSR(28,R dbg_getdisplay(aarg)); /* display */
    DCF(29,isb,dbg_xfpe); /* xfpe */
    DCF(30,isb,dbg_tkerr); /* tkerr */
    DCF(31,isb,dbg_twa); /* wa */
    DCF(32,isb,dbg_tdoErrorStack); /* doErrorStack */
  default:ERROUT(ERR_DOMAIN);
  }
}

void dbgInstall(void)
{
  install((PFI)ep_dbg,"_dbg", A_, 2, A_, A_,0,0,0,0,0,0);
}
