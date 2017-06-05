/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#if defined(__VISUAL_C_2_0__)
#include <sys/param.h>
#define isan(n) (isal(n)||isdi(n))
#endif 

#include <a/development.h>

#include <stdlib.h>
#if defined(__cplusplus)
#include <strings.h>
#else 
#include <string.h>
#endif
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <sys/param.h>
#include <sys/file.h>
#include <dap/buff.h>
#include <a/f.h>
#include <a/fncdcls.h>
#include <a/ik.h>
#include <a/arthur.h>
#include <a/fir.h>
extern I dbg_tl,dbg_depth, dbg_tb, dbg_tnan, dbg_tbwc;

void libaInstall(void);
Z I f_j(A);

#define vInUse(v) ((v->a || v->e || v->f || v->c || v->p || v->q ||     \
		  v->cd || v->rff || v->rfc || v->rpf || v->rpc ||   \
		  v->scd || v->atb)?1:0)

I APL=1,Xf=0,Sf=1,Df=1,Gf=1,Tf=1,Ef=1,doErrorStack=0,oldDepModel=0;
extern C Fs[];
extern I xfpeFlag;

Z A currLoadFile=(A)0;

A get_loadfile()
{
  R(currLoadFile)?(A)ic(currLoadFile):aplus_nl;
}
  
C *findFileName(C *name,C *suffix)
{
/* This function means, "if the suffix ain't .suffix, zap it." */
  Z C buf[MAXPATHLEN];
  I len=strlen((DEV_STRARG)name);
  I suffixLen = strlen((DEV_STRARG)suffix) ;
#if 0
  C * dotLoc = strrchr(name, '.') ;
  C * slashLoc = strrchr(name, '/') ;
  

  if (dotLoc > slashLoc)
	{
	/* There is already a suffix. Better leave it. */
	return name ;
	}

  /* No suffix - add one */
  sprintf(buf, "%s.%s", name, suffix) ;
#else
/* Add suffix if it does not match */
  {
    I i, suffixMatch=1;
    for(i=1; suffixMatch && i<=suffixLen; i++)
      if( name[len-i] != suffix[suffixLen-i] )
	{
	  suffixMatch=0;
	  break;
	}

    if( suffixMatch==0 || name[len-i]!='.' )
      sprintf(buf, "%s.%s\0", name, suffix) ;
    else
      sprintf(buf, "%s\0", name);
  }
#endif
  R buf;
}

Z C *findAplusFileName(C *name)
{
  Z C ap[]=".:/usr/local/a+/lib:/usr/local/lib/a:/common/a:/u/a";
  C *t ,*r=pfind("APATH",ap,name,R_OK);
  if(!r)t=findFileName(name,"+"),r=pfind("APATH",ap,t,R_OK);
  if(!r)t=findFileName(name,"a"),r=pfind("APATH",ap,t,R_OK);
  R r;
}

C *findMapped64FileName(C *name,I mode)
{
  C *t=findFileName(name,"m64");
  I j=R_OK|(mode?W_OK:0);
  C *r=pfind("MPATH",".",t,j);
  if(!r)r=pfind("MPATH",".",name,j);
  if(!r)
  {
    C *em=(C *)malloc(3+strlen((DEV_STRARG)t));
    sprintf(em,"%s %s",CC,t);
    perr(em);
    free(em);
  }
  R r;
}

C *findMapped32FileName(C *name,I mode)
{
  C *t=findFileName(name,"m");
  I j=R_OK|(mode?W_OK:0);
  C *r=pfind("MPATH",".",t,j);
  if(!r)r=pfind("MPATH",".",name,j);
  if(!r)
  {
    C *em=(C *)malloc(3+strlen((DEV_STRARG)t));
    sprintf(em,"%s %s",CC,t);
    perr(em);
    free(em);
  }
  R r;
}

C *doloadafile(C *s,int u)	/* silent version of loadafile */
{
  C r[MAXPATHLEN],*t=findAplusFileName(s),*fn;
  CX x=Cx; I m=APL; int c; FILE *f;
  A oldLoadFile=currLoadFile;
  if(!t){if(dbg_tl&&!u)loadtrc(s,2);R (C *)0;}
  fn=strdup(t);
  currLoadFile=gsv(0,fn);
  if(!(f=fopen(t,"r"))){if(dbg_tl&&!u)loadtrc(t,2);R (C *)0;}
  if ('#'==fgetc(f)&&'!'==fgetc(f)) while (EOF!=(c=fgetc(f))&&'\n'!=c) ;
  else rewind(f);
  if(u)unlink(t);
  ++dbg_depth;if(dbg_tl&&!u)loadtrc(fn,0);
  gwd(r),APL=1,rf(u?0:s,f),Cx=x,APL=m,fclose(f),chdir(r);setPWD();
  if(dbg_tl&&!u)loadtrc(fn,1);--dbg_depth;
  dc(currLoadFile);
  currLoadFile=oldLoadFile;
  R fn;
}

I loadafile(C *s,int u)	/* now a cover for doloadafile */
{
  C *fn=doloadafile(s,u);
  if (0==fn){perr(s);R -1;}
  free(fn);
  R 0;
}

#define PIPEOP_BUFCHUNKSIZE   2048
#define PIPEOP_BLOCKS         10
#define PIPEOP_BLOCKUSLEEP    500000

Z C PipeMsgBuf[128];
#define PIPEPERR(s) {sprintf(PipeMsgBuf,s,CC);perr(PipeMsgBuf);}

Z void pipeOps(C *ss)
{
  V v;
  I appendflag=0,cxflag=0,i;
  FILE g,*h;
  C *varname,*cmdstr,*u,opc;
  opc=*ss++;
  if('>'==opc&&'>'==*ss)appendflag=1,++ss;
  cmdstr=cl(u=bl(varname=dlb(ss))),*u=0;
  for(i=0;varname[i];i++)
  {
    if( isal(varname[i]) || ( i && isan(varname[i]) ) ) continue;
    if( varname[i]=='.' && i!=(strlen((DEV_STRARG)varname)-1) && !cxflag) 
    {
      cxflag=1; continue;
    }
    H("incorrect\n");R;
  } 
  if(!*cmdstr){H("incorrect\n");R;}
  v=('<'==opc)?sv(Cx,si(varname)):svlu(Cx,si(varname));
  if(!v){H("bad varname\n");R;}
/* The #ifdef's fix a memory fault that occurs on Solaris when  */
/* we try to exchange the FILE pointer returned from popen with stdout. */
/* The new code uses dup to move the file descriptors around, but doesn't */
/* use pclose to close the file opened with popen. */
 
/* A better solution would be modify the print routines to write to a  */
/* specific stream, and not try to change file descriptor 1. */
/* ie. fprintf(h,"...",...); */
  switch(opc)
  {
  case '|':
    if(h=popen(cmdstr,"w"))
      {
        int fdStdOut,fd,rc;
        fflush(stdout);
        fdStdOut=dup(1);       /* copy stdout */
 
        fd=dup2(fileno(h),1);  /* 1 closed by dup2 */
        paf((A)gt(v),0,0);
        NL;
 
        fflush(stdout);
        rc=dup2(fdStdOut,1);   /* close 1 and Restore stdout 1 */
        rc=close(fdStdOut);    /* close duplicate of 1         */
	pclose(h);
      }
    else PIPEPERR("%s error during pipe operation");
    break;
  case '>':
    if(h=fopen(cmdstr,appendflag?"a":"w"))
      {
        int fdStdOut,fd,rc;
        fflush(stdout);
        fdStdOut=dup(1);       /* copy stdout */
 
        fd=dup2(fileno(h),1);  /* 1 closed by dup2 */
        paf(gt(v),0,0);
        NL;
 
        fflush(stdout);
        rc=dup2(fdStdOut,1);   /* close 1 and Restore stdout 1 */
        rc=close(fdStdOut);    /* close duplicate of 1         */
	pclose(h);
      }
    else PIPEPERR("%s error during pipe operation");
    break;
  case '<':
    if(h=popen(cmdstr,"r"))
    {
      int nblocks=0, n;
      struct buff *bufp=buffalloc();
      g=*stdin,*stdin=*h;
      while(0<(n=buffread(bufp, fileno(h), PIPEOP_BUFCHUNKSIZE)))
	if(0==n){
	  if(PIPEOP_BLOCKS>=++nblocks){
	    H("%s error: pipe operation blocked",CC);break;}
	  else usleep(PIPEOP_BLOCKUSLEEP);
	}
      buffputc(bufp,'\0');
      *h=*stdin,pclose(h),*stdin=g;
      dc((A)v->a);
      {
        I n=(bufp->put-bufp->min)-1;
        v->a=(I)gc(Ct,1,n,&n,(I *)bufp->min);
      }
      bufffree(bufp);
    }
    break;	
  default:
    H("%s error in pipe operation: unknown prefix [%c]\n",CC,opc);
    break;
  }
  R;
}

Z void listGlobals(I n,C *cxtname)
{
  I i;
  CX cxt=*cxtname?cxlu(si(cxtname)):Cx;
  V v;
  if(0==cxt) {NL; return;}
  for(i=0;i<cxt->ht->nb;++i)
    for(v=(V)cxt->ht->b[i];v;v=v->v)
      if(-2==n&&vInUse(v)||-1==n&&v->e||v->t==n&&v->a)H(" %s",v->s->n);
  NL;
}

Z void printCxt(CX cx){H(" %s",cx->s->n);}
Z void listContexts(void)
{
  I i;CX cx;
  for(i=0;i<CxTable->nb;++i)for(cx=(CX)CxTable->b[i];cx;cx=cx->n)
    if((cx->flag&1)&&cx!=Rx)printCxt(cx);
  NL;
}

#define EX(a) dc((A)v->a),v->a=0
I emptyV(V v)
{
  if (v->o)
    R 0;
  (EX(a),EX(f),EX(c),EX(p),EX(q),EX(cd),EX(rff),EX(rfc),
   EX(rpf),EX(rpc),EX(scd),rmd(v),rmatb(v),dst(v));
  R -1;
}
Z void expunge(V v){if (v->o) H("%s: is bound\n",v->s->n); else emptyV(v);}
Z void expungecxt(C *s)
{
  CX c=cxlu(si(s));
  if(c&&excxt(c))H("%s: not empty\n",s);
}
C *cmdsList[]={
  "vars","fns","ops","xfs","si","wa","cx","rl","load","cd",
  "off","mode","cxs","ex","pp","sfs","stop","vers","loadrm","cmds",
  "def","dep","Tf","Sf","Xf","Df","Gf","deps","Ef","_sfs",
  "dbg","excxt","globs","undef", "Xfpef", "laod", "maplim", "doErrorStack",
  "oldDepModel","su","+su",
  0};
Z C *ts[]={"0 off","1 on","2 trace"};
#define CF(i,f) CS(i,if(*s){f=*s=='2'?2:*s=='1';break;}H("%s\n",ts[f]))
void sys(C *s)
{
  C *v;
  I f=0;
  C c,d,*u,*w,*x;
  A a;
  v=strdup(s);
  if(strchr((DEV_STRARG)"|><",*v)){pipeOps(v);free(v);R;}
  s=cl(u=bl(v)),c=*u,*u=0,x=cl(w=bl(s)),d=*w,*w=0;
  
 /*  1 $vars  2 $fns     3 $ops           4 $xfs         5 $si    */  
 /*  6 $wa    7 $cx      8 $rl            9 $load       10 $cd    */ 
 /* 11 $off  12 $mode   13 $cxs          14 $ex         15 $pp    */
 /* 16 $sfs  17 $stop   18 $vers         19 $loadrm     20 $cmds  */ 
 /* 21 $def  22 $dep    23 $Tf           24 $Sf         25 $Xf    */ 
 /* 26 $Df   27 $Gf     28 $deps         29 $Ef         30 $_sfs  */ 
 /* 31 $dbg  32 $excxt  33 $globs        34 $undef      35 $Xfpef */
 /* 36 $laod 37 $maplim 38 $doErrorStack 39 $oldDepModel          */
 /* 40 $su   41 $+su                                              */

  switch(lu(v,cmdsList)){
    CS(16,xfs()); /* sfs */
    CS(30,x_fs()); /* _sfs */
    CS(11,exit(0)); /* off */
    CS(13,listContexts()); /* cxs */
    CS(9,loadafile(s,0)); /* load */
    CS(36,H("%s warning: you typed \"$laod\".  Loading anyway.\n",CC);
       fflush(stdout);loadafile(s,0)); /* laod */
    CS(1,listGlobals(0,s)); /* vars */
    CS(2,listGlobals(1,s)); /* fns */
    CS(3,listGlobals(2,s);listGlobals(3,s);listGlobals(4,s)); /* ops */
    CS(4,listGlobals(5,s)); /* xfs */
    CS(5,--K;sik();NL;++K); /* si */
    CS(6,wa(!*s?-1:*s=='-'?-2:atol(s))); /* wa */
    CS(7,if(*s)Cx=cx(s);else H("%s\n",Cx==Rx?".":Cx->s->n));
#if defined(__VISUAL_C_2_0__)
    CS(8,srand(atoi(s)));
#else
    CS(8,srandom(atoi(s)));
#endif
    CS(10,if((chdir(*s?s:getenv("HOME")))==-1){perr(s);break;}setPWD());
    CS(12,if(*s){APL=(s[1]=='p')?APMODE_APL:('n'==s[1])?APMODE_UNI:
		   APMODE_ASCII;break;}
       H(APLpick("apl\n","ascii\n","uni\n")));
    CS(14,for(;*s;*w=d,w=bl(s=cl(w)),d=*w,*w=0)expunge(sv(Cx,si(s))));
    CS(34,for(;*s;*w=d,w=bl(s=cl(w)),d=*w,*w=0)rmd(sv(Cx,si(s))));
    CS(15,if(!*s||!isdi(*s)||s[1]&&!isdi(s[1])){H("%c%c\n",Fs[3],Fs[4]);break;}
       Fs[3]=s[1]?*s++:'0';Fs[4]=*s);
    CF(17,sq);
    CS(18,H("%s\n",(C*)(a=versGet())->p);dc(a));
    CS(19,loadafile(s,1));
    CS(21,f0(s));
    CS(22,f1(s));
    CF(24,Sf);
    CF(25,Xf);
    CF(26,Df);
    CF(27,Gf);
    CF(29,Ef);
    CF(35,xfpeFlag);
    CS(31,dbg(s,x));
    CS(32,expungecxt(s)); /* excxt */
    CS(33,listGlobals(-2,s));  /* globs */
    CS(23,Tf=0;disable());
    CS(20,for(;cmdsList[f];++f)H(" %s",cmdsList[f]);NL);
    CS(28,listGlobals(-1,s));
    CS(37,MFALimitSysCmd(!*s?-1:atoi(s))); /* maplim */
    CF(38,doErrorStack);
    CF(39,oldDepModel);
    CS(40,*s?(*u=c,*w=d,syst(v)):H("\343 Warning you entered $su if you really want this then enter: $+su\n"));
    CS(41,(*u=c,*w=d,syst(v+1)));   /* $+su (Add 1 to skip over the "+") */
  default:*u=c,*w=d;syst(v);
  }
  if(v) free(v);
}

Z H1(charToInt){
  A z;
  Q(a->t!=Ct&&a->n,ERR_TYPE)
  W(gd(It,a))
  DO(a->n,z->p[i]=((UC*)a->p)[i])
  R(I)z;
}
Z H1(charToFloat){
  A z;
  Q(a->t!=Ct&&a->n,ERR_TYPE)
  W(gd(Ft,a))
  DO(a->n,((F*)z->p)[i]=((UC*)a->p)[i])
  R(I)z;
}
H1(charToSym){
  A z;
  ND1;
  {
    I ar=a->r,an=a->n,*ad=a->d;
    C *s=(C*)a->p;
    I n,j,d1=1;
    Q(a->t!=Ct,ERR_TYPE);
    if(ar)--ar;else ad=&d1;
    W(ga(Et,ar,n=tr(ar,ad),ad))an=ad[ar];
    DO(n,for(j=an;j--&&s[j]==' ';);z->p[i]=MS(si(sj(s,j+1)));s+=an);
    R(I)z;
  }
}

Z H1(intToChar){
  A z;
  Q(a->t!=It&&a->n,ERR_TYPE)
  W(gd(Ct,a))
  DO(a->n,((C*)z->p)[i]=a->p[i])
  R(I)z;
}
H1(intToFloat){
  A z;
  Q(a->t!=It&&a->n,ERR_TYPE)
  W(gd(Ft,a))
  DO(a->n,((F*)z->p)[i]=a->p[i])
  R(I)z;
}
Z H1(intToSym){
  A z, t;
  if(!(t=(A)intToChar(a))) R 0;
  z=(A)charToSym(t);
  R dc(t),(I)z;
}
Z H1(floatToChar){
  A z, t;
  if(!(t=(A)f_j(a))) R 0;
  z=(A)intToChar(t);
  R dc(t),(I)z;
} 
Z H1(floatToInt){
  A z; F f;
  I d;Q(a->t!=Ft&&a->n,ERR_TYPE);
  W(gd(It,a));
  DO(a->n,
     f=((F*)a->p)[i];
     if(d=f>0?f+.5:f-.5,(f>d?f-d:d-f)>=CT*(f>1?f:f<-1?-f:1))
     R(q=ERR_TYPE,dc(z),0);
     z->p[i]=d
     );
  R(I)z;
}

#if defined(_AIX) || defined(HAVE_SVR4) || defined(__sgi) || defined(__osf__) || defined(linux) || defined(_HP) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
Z H1(f_j){
  A z; F f;
  F imaxmin[2];
  imaxmin[0]=((F)LONG_MAX)+1.0; imaxmin[1]=((F)LONG_MIN)-1.0;
  Q(a->t!=Ft&&a->n,ERR_TYPE)
  W(gd(It,a))
  DO(a->n,
     f=(f=((F*)a->p)[i])>0?f+.5:f-.5;
     if(f>imaxmin[0]||f<imaxmin[1]){q=9;break;}z->p[i]=(I)f);
  R(I)z;
}
#else
Z H1(f_j){
  A z; F f;
  Q(a->t!=Ft&&a->n,ERR_TYPE)
  W(gd(It,a))
  DO(a->n,z->p[i]=(f=((F*)a->p)[i])>0?f+.5:f-.5)
  R(I)z;
}
#endif
Z H1(floatToSym){
  A z, t;
  if(!(t=(A)f_j(a))) R 0;
  z=(A)intToSym(t);
  R dc(t),(I)z;
} 

H1(symToChar){
  A z;
  ND1
  {
    I ar=a->r,an=a->n,*ad=a->d;
    I k,m=0;
    C *s;Q(an&&!sym(a),ERR_TYPE)
    Q(ar>=MAXR,ERR_MAXRANK)
    DO(an,if(m<(k=strlen((DEV_STRARG)XS(a->p[i])->n)))m=k)
    W(ga(Ct,ar+1,an*m,ad))z->d[ar]=m;s=(C*)z->p;
    DO(an,sprintf(s,"%-*s",m,XS(a->p[i])->n);s+=m)
    R(I)z;
  }
}
Z H1(symToInt){
  A z, t;
  if(!(t=(A)symToChar(a))) R 0;
  z=(A)charToInt(t);
  R dc(t),(I)z;
} 
Z H1(symToFloat){
  A z, t;
  if(!(t=(A)symToInt(a))) R 0;
  z=(A)intToFloat(t);
  R dc(t),(I)z;
} 

A ci(I i){I z;A a=(A)Y[i];R(A)((z=floatToInt(a))?(dc(a),Y[i]=z):0);}
A ep_cf(I i){I z;A a=(A)Y[i];R(A)((z=intToFloat(a))?(dc((A)a),Y[i]=z):0);}

I cn(I i,I t){I a=Y[i],z=(I)gd(t,(A)a);R dc((A)a),Y[i]=z;}

Z I dbgor(A a,A w,I i){ND2;I2;if(dbg_tbwc)bitwisechk(a,w,i);R ds(a,w,i);}

H2(castOr){
  I u,t;
  C c;
  I isNull=0;
  I (*castfn)();
  ND2;
  t=w->t;
  if(a->t!=Et||(a->r&&!a->n)) R dbgor(a,w,1);
  Q(!QS(*a->p),ERR_DOMAIN);
  Q(t>Et&&w->n||a->n!=1||t==Et&&!(w->n&&QS(*w->p))&& !(isNull=qz(w)),ERR_DOMAIN);
  c=*XS(*a->p)->n;
  u=c=='i'?It:c=='f'?Ft:c=='c'?Ct:c=='s'?Et:-1;
  Q(u==-1,ERR_DOMAIN);
  if (u==t) R ic_or_copy(w);
  switch(t)
  {
    CS(Ct,castfn=(u==It?charToInt:u==Ft?charToFloat:u==Et?charToSym:0));
    CS(It,castfn=(u==Ct?intToChar:u==Ft?intToFloat:u==Et?intToSym:0));
    CS(Ft,castfn=(u==Ct?floatToChar:u==It?f_j:u==Et?floatToSym:0));
    CS(Et,castfn=(u==Ct?symToChar:
		  u==It?(isNull?charToInt:symToInt):
		  u==Ft?(isNull?intToFloat:symToFloat):0));
  default:castfn=0;break;
  }
  if(0==castfn)R (q=ERR_TYPE,0);
  R (*castfn)(w);
}

#define DDbw(x1,nx1,t,ttype,f) I f(A w){A z;I wd[MAXR],wr=w->r, wn=w->n; \
                  I x=MAX(1,((nx1)/(x1))); \
                  F nx=(((F)x1)/((F)nx1)); \
		  Q( (wr ? w->d[wr-1] : wn) % x, ERR_LENGTH) \
		  wn *= nx; \
		  DO(wr,wd[i]=w->d[i]) \
		  if(wr) wd[wr-1]*=nx; \
		  else if(wn>1) wr=1, *wd=wn; \
		  z=ga(t,wr,wn,wd); \
		  memcpy(z->p,w->p,sizeof(ttype)*wn);\
		  R (I)z;}

DDbw(sizeof(I),      sizeof(F), Ft, F, bwi_f)
DDbw(sizeof(I),      sizeof(C), Ct, C, bwi_c)
DDbw(sizeof(F),      sizeof(I), It, I, bwf_i)
DDbw(sizeof(F),      sizeof(C), Ct, C, bwf_c)
DDbw(sizeof(C),      sizeof(I), It, I, bwc_i)
DDbw(sizeof(C),      sizeof(F), Ft, F, bwc_f)
DDbw(sizeof(void *), sizeof(I), It, I, bws_i)
DDbw(sizeof(void *), sizeof(F), Ft, F, bws_f)
DDbw(sizeof(void *), sizeof(C), Ct, C, bws_c) 

H2(bwcv){
  I u,t;
  C c;
  ND2 
  Q((t=w->t)>Et,ERR_DOMAIN);
  Q(w->t==Et&&!QS(*w->p),ERR_DOMAIN)
  Q(a->n!=1||!QS(*a->p),ERR_DOMAIN)
  c=*XS(*a->p)->n;
  Q(c=='s',ERR_DOMAIN)
  u=c=='i'?It:c=='f'?Ft:c=='c'?Ct:Et;
  if (u==t) R ic_or_copy(w);
  else if (t==Ct) R u==It?bwc_i(w):(u==Ft?bwc_f(w):(q=ERR_TYPE,0));
  else if (t==It) R u==Ct?bwi_c(w):(u==Ft?bwi_f(w):(q=ERR_TYPE,0));
  else if (t==Ft) R u==Ct?bwf_c(w):(u==It?bwf_i(w):(q=ERR_TYPE,0));
  else if (t==Et) R u==Ct?bws_c(w):(u==It?bws_i(w): 
				    (u==Ft?bws_f(w):(q=ERR_TYPE,0)));
  else R (q=ERR_TYPE,0);
}

void libaInstall(void){
  beamInstall();
  nsfInstall();
  attInstall();
  dbgInstall();
  memStatsInstall();
}

I ai(I n)
{
  infi(); /* s.c - initializes infinity values */
  siginit();
  mi();
  wi();
  if(!tmp(n<<20))R 0;
  ki();
  libaInstall();
  R 1;
}
