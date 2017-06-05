/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <pthread.h>            /* Needed for locking in si()  */
#include <string.h>
#include <a/ik.h>
#include <a/arthur.h>
#include <a/fncdcls.h>
#include <a/b.h>
#include <a/s.h>
#include <a/fir.h>
#include <dap/balloc.h>
#if !defined(__cfront)
#include <stdarg.h>
#endif
#include <setjmp.h>

/* strings not equal */
#define strneq(a,b) (*(a)!=*(b)||strcmp((a),(b))!=0)

void *Glbrtn ;
jmptype J;
I G=0, aplus_errno=0;
Z C *r,*s;
Z I *t,tb[190000];
static A zG;
static I Reset;

#ifdef BSTUB
#define SBS_DEFAULTBLKSZ   ((1<<12)-24)
#define SBS_MAXBLKSZ   (1<<26)
#else
#define SBS_DEFAULTMZIDX   12
#define SBS_MAXMZIDX       26
#endif
#define SBS_BLOCKTHRESHOLD  4
#define SBS_TRIGGERRATIO    4

#define HA(n)     (ha(n))
#define STARTHASHSHIFT (10)
#define STARTHASHSIZE  (1<<STARTHASHSHIFT)

typedef struct symblock_struct {
  I size; /* in words */
  I idx;
  struct symblock_struct *next;
  I count;
  I buf[2];
} SymBlock, *SBS;

static SymBlock SymBlockListHead = { 0, 0, 0, 0, {0, 0}};
static SBS FullSymBlockList = (SBS)0;
static HT SymHashTable=0;

Z I ha(const C *n)
{
	unsigned long h = 0, c;

	while ((c = (unsigned long)*n++) != 0)
	{
		h += (h << 5) + c;
	}
	R h;
}

Z void growSymHashTable(void)
{
  I i;
  S s0,s1,*sp;
  HT newtable=hti(4*SymHashTable->nb);
  for(i=0;i<SymHashTable->nb;++i)
    for(s0=(S)SymHashTable->b[i];s0;s0=s1)
    {
      s1=s0->s;
      sp=(S *)(newtable->b+((newtable->nb-1)&HA(s0->n)));
      s0->s=*sp;
      *sp=s0;
      ++newtable->ni;
    }
  bfree((char *)SymHashTable);
  SymHashTable=newtable;
}


Z SBS allocNewBlock(int mw)
{
  SBS cur;
  int blocksize;
  int mzidx=0;

  /* allocate new block, and add to head of list */
#ifdef BSTUB
  blocksize=SBS_DEFAULTBLKSZ;
  while(blocksize<mw){blocksize*=2;if(SBS_MAXBLKSZ<blocksize)R 0;}
#else
  mzidx=SBS_DEFAULTMZIDX;
  while((MZ[mzidx]-1)<mw)if(SBS_MAXMZIDX<++mzidx)R 0;
  blocksize=MZ[mzidx]-1;
#endif
  cur=(SBS)ma(blocksize);
  cur->size=blocksize-4;
  cur->idx=cur->count=0;
  cur->next=SymBlockListHead.next;
  SymBlockListHead.next=cur;
  return cur;
}

Z S newSymbol(const C *str,S prevsym)
{
  int mw;
  SBS cur,prev;
  S res;

  mw=((12+strlen(str))>>2)&~0x01;
  for(cur=&SymBlockListHead,prev=cur;cur=cur->next;prev=cur)
  {
    if (cur->size-cur->idx>=mw) break;
  }
  if(!cur)
  {
    allocNewBlock(mw);
    prev=&SymBlockListHead;
    cur=prev->next;
  }
  res=(S)(cur->buf+cur->idx);
  res->s=prevsym->s,prevsym->s=res,strcpy(res->n,str);

  /* See if we need to move block to Full list. */
  cur->idx += mw;
  ++cur->count;
  if(cur->size-cur->idx<SBS_BLOCKTHRESHOLD)
  {
    prev->next=cur->next;
    cur->next=FullSymBlockList;
    FullSymBlockList=cur;
  }

  /* See if we need to grow hash table. */
  if(SymHashTable->nb*SBS_TRIGGERRATIO<++SymHashTable->ni)growSymHashTable();

  R res;
}

void symhti(void){SymHashTable=hti(STARTHASHSIZE);}

/* The insertion of a new symbol needs to be guarded since si() can be  */
/* called from other threads */

static pthread_mutex_t newSymbol_lock = PTHREAD_MUTEX_INITIALIZER;

#if defined(linux)
static int _initMutex(void)
{
  int rc;
  pthread_mutexattr_t mattr;
  if(0!=(rc=pthread_mutexattr_settype(&mattr, PTHREAD_MUTEX_ADAPTIVE_NP)))
    {
      perror("initMutex():pthread_mutexattr_settype");
    }
  if (rc==0 && pthread_mutex_init(&newSymbol_lock, &mattr))
    {
      perror("initMutex():thread_mutex_init");
    }
  return 0;
}
#endif

S si(const C *n)
{
  S s,a=(S)(((S *)(SymHashTable->b))+((SymHashTable->nb-1)&HA(n)));

  for(;(s=a->s)&&strneq(n,s->n);a=s) ;
  if(s==0)
    {
      int rc;

#if defined(linux)
      static int initMutex=1;
      if(initMutex)
	initMutex=_initMutex();
#endif

      if(0!=(rc=pthread_mutex_lock(&newSymbol_lock)))
        {
          perror("si() pthread_mutex_lock");
        }
      {
        /* Need to re-check for Symbol after getting lock */
        S a=(S)(((S *)(SymHashTable->b))+((SymHashTable->nb-1)&HA(n)));
        for(;(s=a->s)&&strneq(n,s->n);a=s) ;
        if(s==0)
          {
            s=newSymbol(n,a);
          }
      }

      if(rc==0 && pthread_mutex_unlock(&newSymbol_lock))
        {
          perror("si() pthread_mutex_unlock");
        }
    }
  R s;
}

A SymbolTableHashChainLengths(void)
{
  A z = gv(It, SymHashTable->nb);
  I *p = z->p;
  I i, j;
  S a, s;
  
  for (i = 0; i < SymHashTable->nb; i++)
  {
    for (j=0,a=(S)(((S *)(SymHashTable->b)) + i); s = a->s; j++,a=s);
    p[i] = j;
  }
  R z;
}

A SymbolTableBlockInfo(void)
{
  A t,z = gv(Et,3);
  I count, *ip;
  SBS cur;
  
  z->p[0]=(I)gvi(It,2,SymHashTable->nb,SymHashTable->ni);
  for(count=0,cur=&SymBlockListHead;cur=cur->next;++count) ;
  t=gm(It,count,3),ip=t->p;
  for(cur=&SymBlockListHead;cur=cur->next;
      *ip++=cur->size,*ip++=cur->idx,*ip++=cur->count) ;
  z->p[1]=(I)t;
  for(count=0,cur=FullSymBlockList;cur;++count,cur=cur->next) ;
  t=gm(It,count,3),ip=t->p;
  for(cur=FullSymBlockList;cur;cur=cur->next)
    *ip++=cur->size,*ip++=cur->idx,*ip++=cur->count;
  z->p[2]=(I)t;
  R z;
}

V sv(CX c,S s){C *t=(C *)strchr(s->n,'.'),b[99];
 if(t)*t=0,strcpy(b,s->n),*t='.',c=cxi(si(b)),s=si(t+1);R vi(s,c);}
V svlu(CX c,S s){C *t=(C *)strchr(s->n,'.'),b[99];
 if(t)*t=0,strcpy(b,s->n),*t='.',c=cxlu(si(b)),s=si(t+1);R c?vlu(s,c):(V)0;}
I ispu(I c){R c==':'||c==';'||c=='('||c==')'||c=='{'||c=='}'||c=='['||c==']';}
Z I issp(I c){R c==' '||c=='\n'||c=='\t'||c==12;}
I isal(I c){R c>='a'&&c<='z'||c>='A'&&c<='Z'||c=='_';}
I isdi(I c){R c>='0'&&c<='9';}
I isan(I c){R isal(c)||isdi(c);}

/*
 * readDoubleFromString - s - pointer to parsing source - d - ptr to double
 * handles negatives (high or low minus, depending on mode) and "Inf".
 * Result: 1 on success, 0 on failure - number loaded in d, and s advanced
 */
Z I readDoubleFromString(C **s,F *d)
{
  C *r=*s;
  I i='\242'==*r||(1!=APL)&&'-'==*r,j;
  r+=i;
  j=strncmp(r,"Inf",3)||isal(r[3]);
  if(j&&!isdi(r['.'==*r]))R 0;
  if(!j)*s=r+3,*d=Inf;
#ifdef linux
/* Linux's strtod differs from other implementations in that it also */
/* converts hexadecimal values.  This causes problems for A+ code line */
/* y,@0x or y,@0x22 where it removes both the 0 and the x */
  else
    {
      if( r[0]=='0'&&(r[1]=='x'||r[1]=='X'))
	{
	  *d=0.0;
	  *s=r+1;
	}
      else 
	for(*d=strtod(r,s);**s=='0';++*s);
    }
#else
  else 
    for(*d=strtod(r,s);**s=='0';++*s);
#endif
  if(i)*d=-*d;
  R 1;
}

extern I dbg_txeq;

C *sy(C *s){for(;isan(*++s););R s;}
C *dlb(C *s){for(;issp(*s);++s);R s;}
C *ssym(C *s){for(;isan(*++s)||*s=='.';);R s;}
C *cl(C *s){for(;s=dlb(s),CCtest(s);)for(;*++s&&*s!='\n';);R s;}
C *bl(C *s){for(;*s&&!issp(*s);++s);R s;}
void tc(I *t){for(;t>tb;)if(!ispu(*--t))dc((A)*t);longjmp(J,-2);}
Z C *ts[]={"wsfull","stack","undefined","MAX args: 9","( nesting too deep"};

/* trr modified to replace c at *e, to restore string element which has
   been temporarily repalced by \0, after error message is displayed
*/
Z void trr(I q,C *s,C *e,C c)
{
  if(!G)H("%s[token]: %s %s\n",CC,q==2?s:"",ts[q]);
  if(e)*e=c;
  tc(t);
}
/* changing to char cast */
Z void brr(I c){if(!G)H("TOKEN: open %c\n",(char)c);tc(tb);}
Z I u_rs(C *v,I i)
{
  I t=0;
  C c=*s;
  *s=0;
  if(i&&(*v=='\312'||(t=aplus_pi(v))))
  {
    if(c=='/'||c=='\\'||c=='\256'||c=='.'&&!isan(s[1]))
    {
      *s++=c;
      if(c=='.'&&*s&&!ispu(*s)&&!issp(*s))++s;
      c=*s,*s=0,t=aplus_pi(v);
    }
    if(!t)trr(2,v,s,c);
  }
  else
  {
    if(i==1)trr(2,v,s,c);
    t=MS(si(v)); 
  }
  R *s=c,t;
}
Z I rs_uni(C *v)
{
  C c;I z,idx;
  idx=(v[2]&&'='==v[3])?4:3;
  c=v[idx],v[idx]='\0';
  z=aplus_pi(v);
  v[idx]=c;
  if(z) {s+=idx;R z;} else R 0;
}
Z I rq(I c){while(*++s&&((c=='"'?*s=='\\':*s==c&&s[1]==c)?(I)++s:*s!=c));R *s;}
Z I acp(C *d,const C *s){C *t=d;for(;*d=*s;++d,++s)if(*s=='\'')++s;R d-t;}
Z I ccp(C *d,const C *s)
{
  C *t=d;
  for(;*d=*s;++d,++s)
    if(*s=='\\')
      if(*++s=='n')*d=10;
      else if(!isdi(*s))*d=*s;
      else
      {
	I j=3,n=*s-'0';
	for(;isdi(*++s)&&--j;n=n*8+*s-'0');
	--s,*d=n;
      }
  R d-t;
}
A gsv(I i,const C *s)
{
  I n=s?strlen(s):0;
  A z=gv(Ct,n);
  if(n)
  { 
    if(!i)strcpy((C *)z->p,s);
    else n=z->n=*z->d=i==2?ccp((C *)z->p,s):acp((C *)z->p,s);
    if(n==1)z->r=0;
  }
  R z;
}
A gst(I x,C *s){A r=(A)gsv(x,s);r->r=1;return r;}
/* gst() is a cover for gsv() which always returns a vector. */
Z I gvs(I t,I n,I *s){R(I)gc(t,n!=1,n,&n,s);}
A gvi(I t, I n,...)
 {
  I x;
  A z;
  va_list ap;
  P zp;

  va_start(ap, n);
  z=gv(t,n);zp.i=z->p;
  switch(t)
   {
    CS(It,DO(n,*zp.i++=va_arg(ap,I)))
    CS(Ft,DO(n,*zp.f++=va_arg(ap,F)))
    CS(Ct,DO(n,*zp.c++=(C)va_arg(ap,I)))
    CS(Et,DO(n,*zp.i++=(x=va_arg(ap,I))?x:(I)aplus_nl))
    default:DO(n,*zp.i++=va_arg(ap,I))break;
   }
  va_end(ap);
  R z;
 }
Z I rw()
{
  I n=0,y;
  F d[9999];
  C c=*s,*v=s;
  extern I *XY;
  if(ispu(c)&&(c!=':'||s[1]!='='))R *s++;

  /* look for a si argument */
  if(c==SI_CHAR)
  {
    if(!isdi(*++s))R ML(0);
    n=*s++-'0';
    if(Y+n>=XY)trr(1,"",0,0);
    R ML(n-=X-Y);
  }

  /* look for a quoted string */
  if((n=c=='"')||c=='\'')R ++v,rq(c),*s=0,n=(I)gsv(n+1,v),*s++=c,n;

  /* look for a symbol constant */
  for(n=0;*s=='`';s=cl(s)){s=ssym(v=s);t[n++]=u_rs(v+1,0);}
  if(n)R gvs(Et,n,t);

  
  /* look for a numeric constant */
  for(;readDoubleFromString(&s,d+n);s=dlb(s))if(++n==9999)trr(1,"",0,0);
  /* doubles now loaded in d, n is howmany, s is at next token */
  if(n)
  {
    C c=*s;*s=0;y=(I)strpbrk(v,".Ee"),*s=c;
    if(!y)DO(n,if(y=d[i]!=(t[i]=d[i])){q=0;break;});
    R gvs(y?Ft:It,n,y?(I*)d:t);
  }

  /* look for a uni-mode compound (X.#) primitive */
  if(APMODE_UNI==APL&&isal(*v)&&'.'==v[1]&&(n=rs_uni(v)))R n;

  /* look for a name */
  if(s=cl(s),n=isal(*s)){s=sy(v=s);n=u_rs(v,2);}
  if(*s=='.'&&isal(s[1])&&(QS(n)||!n))
  {
    s=sy(v=s+1);
    y=u_rs(v,2);
    if (QS(y)) R MV(vi(XS(y),n?cxi(XS(n)):Rx));
    else trr(2,".",0,0);  /* trr does longjmp */
  }
  if(n)R n;

  if(*++s=='='||*s==':')++s;
  
  R u_rs(v,1);
}
Z I ra(I k)
{
  r=s;
  t=tb;
  if(k)*t++='{';
  for(;*s;++t,s=cl(s))*t=rw();
  if(k)*t++='}';
  *t=0;
  R rd(tb);
}

Z I u,c,v;void tfl(void){/*ioctl?*/fflush(stdout);}
void pr(void){q=0;if(!Reset){DO(u+v,H("*"))H("     "),tfl();}}
Z I chk(void){if(c)if(--s,!rq(c))R c;else --v,++s;
 for(;s=cl(s),c=*s;++s){if(c=='"'||c=='\'')if(!rq(c))R ++v;
  if(c=='{'||c=='(')++v; else if(c=='}'||c==')')--v;}
 R v<0?(v=0):v>0||s[-2]==':';}
void ff(A a){if(Tf&&!qz(a))paf(a,0,0),NL,tfl();}
Z I bal(I f)
{
  C c,b[999],*v=s;
  I i=0,j,k=0;
  for(;s=cl(s),c=*s;++s)
    switch(j=0,c)  /* lots of intentional fall-thru */
    {
      case'"':
      CS('\'',if(!rq(c))brr(c));
      CS(';',if(!i){if(f)R -1;k=1;});
    case '(':
    case '[':
      CS('{',if(i==999)trr(4,"",0,0);b[i++]=*s);
    case ')':
      ++j;
    case ']':
      ++j;
      CS('}',if(!i){if(f)R -1;brr(*s);}else if(b[--i]!="{[("[j])brr(b[i]));
    } 
  if(i)brr(b[i-1]);
  if(f)R -1;
  R s=v,k;
}
C *nx(C* t){R s=t,bal(1),s;}

Z void de(void)
{
  I a=exm(s,APL);
  if(q==-1&&J)u--,longjmp(J,-1);
  q=0;
  if(a)ff((A)a),dc((A)a);
  k_tm(0);
}

Z I EoF=0;
Z I AbortLoad=0;
Z C sb[99999],*b=sb;void sbi(void){b=sb,*b=c=v=0;}
C *sj(C *s,I j){R strncpy(sb,s,j),sb[j]=0,sb;}
Z I u_f1(FILE *f)
{
  I n=sb+sizeof(sb)-b;
  if(Reset) --Reset, --u, longjmp(J, -3); 
  if(EoF=!fgets(b,n,f?f:stdin)){if(f)R 0;exit(1);}
  if(v&&!b[2]&&(*b=='\375'||*b=='$'))R sbi(),0;
  if (n==strlen(b)+1)
  {
    H("buffer full\n");
    sbi();
    R -1;
  }
  s=b;
  if (chk())
  {
    b = s;
    R 1;
  }
  R 0;
}

Z I go(void)
{
  I r,rf=0;
  for(;issp(*--s););
  s[1]=0;
  s=cl(b=sb);
  if(!*s)R 0;
  if((r=*s=='\373'||*s==':')||(!s[1]&&(*s=='\375'||*s=='$'))||
     (*s=='$' && (rf=!strncmp(s+1,"reset",5))))
  {
    if(!J)R u;
    if(r)
    {
      if(!s[1])R 1;
      r=exm(s+1,APL);
      if(!r)R q=0;
    }
    if(rf) 
    {
      if(s[6]) Reset=MAX(0,MIN(u,atol(&s[6])));
      else Reset=u;
      R 0;
    }  
    else --u,longjmp(J,r?r:-3);
  } 
  R de(),0;
}

Z C *scp(C *s){R (C *)strcpy(mab(1+strlen(s)),s);}
void rf(C *s,FILE *f)
{
  if(s)*++K=MV(s=scp(s)),*++K=-1;
  AbortLoad=0;
  for(;;)
  {
    if(!u_f1(f))if(EoF||go()||AbortLoad)break;
    if(s)--*K;
  }
  AbortLoad=0;
	/* this was a char printf changed to cast */
  if(c||v)H("%s OPEN %c\n",b,(char)(c?c:'{')),c=v=0;if(s)K-=2,mf((I *)s);
}

I getAbortLoad(){R AbortLoad;}
void setAbortLoad(I val){AbortLoad=val;}

I tf(void){I r=u_f1(0)?0:go();if(!r)pr();R r;}
I ui(void)
{
  CX c=Cx;
  if(*X)
  {
    A f=(A)*X;
    Cx=(CX)f->p[f->n+2];
  }
  for(*++K=0,++u,pr();!tf(););
  R Cx=c,--u,--K,0;
}

C *es[]={"stop","interrupt","wsfull","stack","value","valence","type","rank",
	   "length","domain","index","mismatch","nonce","maxrank",
	   "non-function","parse","maxitems","invalid","non-data",(C *)0};
C *qs;
Z void qsrr(void){if(q>0)qs=es[q];}
void xrr(void){qsrr(); if(G==0) q=0;}
Z void prr(I i,A a)
{
  q=0;
  H("%s[error] ",CC);if(i==2)H("%lu",a);else pa((V)a);H(": %s\n",i<0?qs:es[i]);
}
I aplus_err(I i,A a){aplus_errno=q=i;if(!Ef||G&&i)longjmp(J,-3);
	       Tf=1;stdinFlagSet(Tf);prr(i,a);ui();R 0;}
void perr(C *s){perror(s),fflush(stdout);}

Z I tok(void)
{
  jmp_buf b;
  CX c=Cx;
/*
  jmptype j=J;
  I *k=K,z=setjmp(J=b)?0:ra(bal(0));
  R K=k,Cx=c,J=j,z;
*/
   jmptype j=J;
   I *k=K,z;
 
 /**
   z=setjmp(J=b)?0:ra(bal(0));
 */
   if (setjmp(J=b)) {
     z = 0;
   } else {
     z = ra(bal(0));
   }
 /**/
 
   K=k,Cx=c;
   J=j;
   R z;

}

I ez(I a)
{
/*
  jmp_buf b;jmptype j=J;I *k=K,*x=X,*y=Y,i;CX c=Cx;
*/
   jmp_buf b;
   jmptype j=J;
   I *k=K,*x=X,*y=Y,i;
   CX c=Cx;

  q=0;
  if(i=setjmp(J=b))
  {
    Cx=c;
    if(q && doErrorStack) 
      snapshotKstack();
    for(J=j,K=k,X=x;Y<y;)dc((A)(*Y++));
/*
    R q?0:i!=-3?i:0;}
*/
    if (q) {
      R 0;
    } else {
      if (i != -3) {
	i = (I)Glbrtn;
        R i;
      } else {
        R 0;
      }
    }
    }

  R a=ev(a),J=j,a;
}

I exm(C* expstr,I mode)
{
/*
 I e,z;s=cl(expstr);
 if(*s=='$')R sys(s+1),(I)aplus_nl;if(!*s)R(I)aplus_nl;Q(Y-K<30,3)
 z=APL,APL=mode,e=tok(),APL=z;
 Q(!e,15)*++K=MS(expstr=scp(expstr)),z=ez(e),mf((I*)expstr),K--;
 if(!z){if(!q)q=9;ef(e);R 0;}
*/
 I e,z;


 s=cl(expstr);

 if(*s=='$')
   R sys(s+1),(I)aplus_nl;
 if(!*s)
   R(I)aplus_nl;
 Q(Y-K<30,3)

 z=APL;
 APL=mode;
 e=tok();
 APL=z;

 Q(!e,15)
 *++K=MS(expstr=scp(expstr));
 z=ez(e);
 mf((I*)expstr);
 K--;
 if(!z){
  if(!q)
    q=9;
  ef(e);
  R 0;
 }

 R QE(e)&&XE(e)->f==MN(0)?(ef(e),dc((A)z),(I)aplus_nl):(ef(e),z);
}

extern I Gf,Xf,doErrorStack;
I pev(I a)
{
 I g=G;A z;
 G=Gf,a=ez(a),G=g;
 if(!a&&!q)
   longjmp(J,-3);
/*  Replace with call to gvi() */
/*  z=gv(Et,2); */
/*  *z->p=(I)gi(q); */
/*  z->p[1]=q?(I)gsv(0,q<0?qs:es[q]):a; */
 z=gvi(Et, 2, 
       gi(q), 
       q ? (I)gsv(0, q<0 ?qs : es[q]) : a);
 R q=0,(I)z;
}

I pexm(I a1,I m)
{
  I g=G;
  A z;
  G=Gf;
  a1=exm((C *)a1,m);
  G=g;
  if(a1){z=gs(Et);*z->p=a1;} else z=gi(q);
  if(Gf==2 && q) dc(zG);
  if(!a1) q=0; 
  R (I)z;
}

/* old entrypoints pex() and ex() included for compatibility with a_79 */
/* these two functions are not used by any native A+ code              */
I pex(I a) {R pexm(a,APL);}
I ex(CX c,C *s){I r;CX saveCx=Cx; Cx=c; r=exm(s,APL); Cx=saveCx; R r;}

extern I dbg_tf,dbg_depth;
#define FIN  ++dbg_depth;if(dbg_tf)functrc(fnc,0);if(i=fnc->p[fnc->n])t=t2(i,1)
#define FOUT(x) if(i)t2(t,0);if(dbg_tf)functrc(fnc,x);--dbg_depth

I af(I n){
  jmp_buf b;
  A fnc=(A)*Y;
  I i=0,*k,t=0,*x,z=0,h=fnc->t==Xt;
  jmptype j; /* ber says I *??? */
  I *d;
  if(h)
  {
    d=fnc->d;
    if(QN(*d)){
      EQ(1,n!=2&&n!=3?(q=5,0):
	 (z=n>2?Y[2]:0,fnc->r>2?rk(d[1],(A)d[2],(A)Y[1],(A)z):
	  ea(d[1],(A)Y[1],(A)z))
	 );
      R z;
    }
    for(h=i=fnc->r;i--;*--Y=ic((A)d[i]));L0:fnc=(A)*Y;
  }	
  if(fnc->t<=Xt)q=14;
  else if(Y-K<30)q=3;
  else if(n+h!=fnc->r)q=5;
  else if(!*fnc->p)q=4;
  else 
  {
    CX c=Cx;
    x=X,X=Y,j=J,k=K,*++K=(I)fnc;
    DO(fnc->n-1,*--Y=0);
    FIN;
    if(z=setjmp(J=b))
    {
      if(Cx=c,z==-3||z==-1)
	{
	  FOUT(2);
	  if(z==-3)longjmp(j,-3);
	} 
      else 
	{
	  if(Glbrtn)
	    z = (I)Glbrtn ;
	}
    }
    else z=ev(*fnc->p);
    for(;Y<X;dc((A)(*Y++)));
    K=k,J=j,X=x;
  }
  if(q)if(z=aplus_err(q,fnc),!z)goto L0;
  DO(h,dc((A)(*Y++)))FOUT(1);
  R z;
}

extern C *stringFromAobj();

H1(sg)
{
  C *t;
  ND1;
  if(qz(a))longjmp(J,-3);
  t=stringFromAobj(a);
  Q(!t||!a->c,9);
  qs=si(t)->n;
  longjmp(J,q=-1);
  R -1;
}
void frep(A f){strcpy((C *)(f->p[f->n+1]=(I)mab(1+strlen(r))),r);}
