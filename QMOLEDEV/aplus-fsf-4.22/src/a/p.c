/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <a/development.h>
#include <math.h>
#if defined(HAVE_SVR4)
#  include <ieeefp.h>
#endif
#include <string.h>
#include <a/ik.h>
#include <a/s.h>
#include <a/fir.h>
#include <a/fncdcls.h>
#include <a/arthur.h>
#include <dap/balloc.h>
#undef min
#undef max
#if defined(__VISUAL_C_2_0__) || defined(_HP) || defined(linux) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#define iszero(x) (x==0)
#endif

#define PC putchar
Z C *ns[]={":=","else","if","while","do","case","time","","@","each",0};
Z C *ps[]={"&","?","+","*","max","min","-","%","|","<",
	     ">","=","~=","<=",">=","^","log","rand","flip","iota",
	     "#","rho",",","~","rot","take","drop","upg","dng","==",
	     "pack","unpack","/","\\","in","bag","pick","mdiv","!","pi",
	     "&\\","?\\","+\\","*\\","max\\","min\\","&/","?/","+/","*/",
	     "max/","min/", "+.","*.","max.","min.","-.","%.","|.","<.",
	     ">.","=.","~=.","<=.",">=.", "^.","+.*","beam","max.+","min.+",
	     "form","eval","where","rtack","ref","dot","bwand","bwor",
	     "bwlt","bwgt",
	     "bweq","bwne","bwle","bwge","bwnot",0};
Z C *n0[]={"\373","else","if","while","do","case","time","","@","\241",0};
Z C *p0[]={"^","\251","+","\253","\323","\304","-","\337","|","<",
	     ">","=","\250","\244","\246","*","\360","?","\364","\311",
	     "#","\322",",","~","\367","\331","\325","\350","\347","\275",
	     "\302","\316","/","\\","\305","\332","\330","\255","!","\317",
	     "^\\","\251\\","+\\","\253\\","\323\\","\304\\","^/","\251/",
	     "+/","\253/",
	     "\323/","\304/","\312.+","\312.\253","\312.\323","\312.\304",
	     "\312.-","\312.\337","\312.|","\312.<",
	     "\312.>","\312.=","\312.\250","\312.\244","\312.\246","\312.*",
	     "+.\253","\340","\323.+","\304.+",
	     "\356","\342","\335","\333","%","\326","^\256","\251\256",
	     "<\256",">\256",
	     "=\256","\250\256","\244\256","\246\256","~\256",0};
Z C *n2[]={":=","else","if","while","do","case","time","","@","~",0};
Z C *p2[]={"&","|","+","*","M.+","M.-","-","%","M.|","<",
	     ">","=","!=","<=",">=","M.*","M.&","M.?","S.\\","I.#",
	     "#","S.?",",","!","S.|","S.+","S.-","I.+","I.-","==",
	     "M.<","M.>","/","\\","I.?","I.<","I.>","M.#","S.!","M.^",
	     "&\\","|\\","+\\","*\\","M.+\\","M.-\\","&/","|/","+/","*/",
	     "M.+/","M.-/", "O.+","O.*","Q.+","Q.-","O.-","O.%","Q.|","O.<",
	     "O.>","O.=","O.!=","O.<=","O.>=", "Q.*","P.*","F.!","P.+","P.-",
	     "E.%","E.*","A.<","A.>","^","Y.&","B.&","B.|","B.<","B.>",
	     "B.=","B.!=","B.<=","B.>=","B.!",0};

C **get_primlist(int mode,int flag)
{
  R (mode?((1==mode)?(flag?n0:p0):(flag?n2:p2)):(flag?ns:ps));
}

/*-------  Character Hash Table Functions  ----------*/

#define CHTHASH(ht,s) ((ht)->b+(((ht)->nb-1)&chafn(s)))
#define strneq(a,b) (*(a)!=*(b)||strcmp((a),(b))!=0)
#define streq(a,b) (*(a)==*(b)&&0==strcmp((a),(b)))

Z unsigned long chafn(C *n)
{
  unsigned long h = 0, c;
  while ((c = (unsigned long)*n++) != 0)
  {
    h += (h << 5) + c;
  }
  R h;
}

/* htgi - get val of key from ht.  If !found,R 0; */
Z HTN chtgi(HT ht,C *key)
{
  HTN *htn=(HTN*)CHTHASH(ht,key),n;
  for(n=*htn;n;n=n->n)if(streq((DEV_STRARG)key,(DEV_STRARG)n->s))R n;R(HTN)0;
}

Z HTN chtni(void){HTN z=(HTN)balloc(sizeof(*z));bzero(z,sizeof(*z));R z;}

/* htsi - set val of key in ht to d.  If no entry exists, create. */
I chtsi(HT ht,C *key,I a)
{
  HTN *htn=(HTN*)CHTHASH(ht,key),n,hd;
  for(n=*htn;n;n=n->n)if(streq((DEV_STRARG)key,(DEV_STRARG)n->s)){n->a=a;R 0;}
  n=chtni();if(!n)R 0;n->s=(S)key;n->a=a;
/*  if(hd=*htn){n->n=hd->n;hd->n=n;}else{*htn=n;n->n=0;}++ht->ni;R 1; */
  hd=*htn;
  if(hd){
    n->n=hd->n;hd->n=n;
  }else {
    *htn=n;n->n=0;
  }++ht->ni;
  R 1;

}

Z HT p0ht, psht, p2ht;

extern C *xfs_name[],*xfs_desc[],*nx(),*cl();
extern HT xsht;
I xslu(C *str){HTN z=chtgi(xsht,str);R z?z->a:0;}
I lu(C *s,C *t[]){I i=0;for(;t[i];)if(!strcmp((DEV_STRARG)s,(DEV_STRARG)t[i++]))R i;R 0;}
C*pp(I a)
{
  R QS(a) ? XS(a)->n :
    (QN(a)?(APLpick(n0,ns,n2)):QP(a)?(APLpick(p0,ps,p2)):xfs_name)[U(a)];
}
C*ppd(I a)
{
  R QS(a) ? XS(a)->n :
    (QN(a)?(APLpick(n0,ns,n2)):QP(a)?(APLpick(p0,ps,p2)):xfs_desc)[U(a)];
}
I aplus_pi(C *s){HTN z=chtgi(APLpick(p0ht,psht,p2ht),s);if(z)R z->a;
 if(*s=='_'&&(ISlower(*(s+1))||'_'==*(s+1)))R xslu(s);R 0;}
Z C*fn(C *s,I n){for(;--n;)s=1+nx(s);R s;}
Z C*ss(C *q,C *s){I n=strlen((DEV_STRARG)s);for(;strncmp(q,s,n);)++q;R q;}
Z C*sb(C *q){I i=0;C c;for(;c=*q++,i||c!='{';)i+=(c=='(')-(c==')');R q;}
void sik(void){
  I *p=K,s;
  for(;*p;--p);
  for(++p;p<=K;)
    if(QV(s=*p++))H("%s[ %ld]\n",(C*)(s&~aplusMask),-*p++);
    else
    { C c,*t,*q,*r;
      if(QS(s))q=(C*)(s&~aplusMask);
      else
      {
	A f=(A)s;
	q=1+(C *)strchr((DEV_STRARG)f->p[f->n+1],':');
	H("%s.%s: ",((CX)f->p[f->n+2])->s->n,XS(*f->d)->n);
      }
      for(;p<=K&&*p>-9999&&*p<6;++p,s=0)
	q=cl(0>*p?fn(QS(s)&&*q!='{'?q:sb(q),-*p):ss(q,ns[*p]));
      t=nx(q);
      r=(C *)strchr((DEV_STRARG)q,'\n');
      if(r&&r<t)t=r;
      c=*t,*t=0,H("%s\n",q),*t=c;
    }
}

/* many differences w/ ber here, leaving for now */
/* In version 4 M is aplusMask */
extern I dbg_tdoErrorStack;
#define wrkStrBufSize 1024
#define extraSpace      32
A sikAsAObj()
{
  I *p=K,s,i=0,n,cLen,lc=0;
  A z,aWrk;
  C b[wrkStrBufSize];
  *b='\0';
  for(;*p;--p);
  
  if(0>=(K-p))
    return aplus_nl;

  /* Allocate nested A+ object and initialize to nulls */
  z=gv(Et,K-p);
  for(i=0; i<z->n; i++)
    z->p[i]=(I)aplus_nl;

  for(++p;p<=K;)
    if(QV(s=*p++)){
      sprintf(b,"%.*s[ %ld]\0",(wrkStrBufSize-extraSpace),(C*)(s&~aplusMask),-*p++);
      n=1+strlen(b);aWrk=gv(Ct,n);memcpy(aWrk->p,b,n);z->p[lc++]=(I)aWrk;
    } else {
      C c,*t,*q,*r;
      if(QS(s))q=(C*)(s&~aplusMask);
      else
      {
	A f=(A)s;
	q=1+(C *)strchr((C *)f->p[f->n+1],':');
	sprintf(b,"%.*s\0",(wrkStrBufSize-extraSpace),
		((CX)f->p[f->n+2])->s->n);
	cLen=strlen(b);
	sprintf(b+cLen,".%.*s: \0",
		(wrkStrBufSize-(cLen+extraSpace)),XS(*f->d)->n);
      }
      for(;p<=K&&*p>-9999&&*p<6;++p,s=0)
	q=cl(0>*p?fn(QS(s)&&*q!='{'?q:sb(q),-*p):ss(q,ns[*p]));
      t=nx(q);
      r=(C *)strchr(q,'\n');
      if(r&&r<t)t=r;
      c=*t,*t=0;
      cLen=strlen(b);
      sprintf(b+cLen,"%.*s\0",(C*)(wrkStrBufSize-(cLen+extraSpace)),q);
      n=1+strlen(b);aWrk=gv(Ct,n);memcpy(aWrk->p,b,n);z->p[lc++]=(I)aWrk;
      *t=c;
    }

  if(lc==0)
    {
      aWrk=aplus_nl;
    }
  else
    {
      aWrk=gv(Et, lc);
      for(i=0; i<lc; i++)
	aWrk->p[i]=ic((A)z->p[i]);
    }
  dc(z);
  if(dbg_tdoErrorStack) doErrorStacktrc(q,aWrk);
  R aWrk;
}

I sik_exp(I tt){   /* experimental version of sik for monadic do */
  I *p=K,s,i=0;
  C b[999];
  A z=0;
  *b='\0';
  for(;*p;--p);
  if (tt) z=gv(Et,K-p);
  for(++p;p<=K;)
    if(QV(s=*p++)){
      sprintf(b,"%s[ %ld]\n",(C*)(s&~aplusMask),-*p++);
      if(!tt) H("%s",b);
      else z->p[i++]=(I)gsv(0,b);
    } else {
      C c,*t,*q,*r;
      if(QS(s))q=(C*)(s&~aplusMask);
      else
      {
	A f=(A)s;
	q=1+(C *)strchr((DEV_STRARG)f->p[f->n+1],':');
	sprintf(b,"%s.%s: ",((CX)f->p[f->n+2])->s->n,XS(*f->d)->n);
      }
      for(;p<=K&&*p>-9999&&*p<6;++p,s=0)
	q=cl(0>*p?fn(QS(s)&&*q!='{'?q:sb(q),-*p):ss(q,ns[*p]));
      t=nx(q);
      r=(C *)strchr((DEV_STRARG)q,'\n');
      if(r&&r<t)t=r;
      c=*t,*t=0;
      sprintf(&b[strlen((DEV_STRARG)b)],"%s",q);
      if(!tt) H("%s\n",b);
      else z->p[i++]=(I)gsv(0,b);
      *t=c;
    }
  if(tt) z->n=z->d[0]=i;
  R tt?(I)z:0;
}

void sk(void)
{
  I *p=K,s,inbraces=0;
  for(;*p;--p);
  for(;p++<K;)
  {
    s=*p;
    if (s<0&&s>-999) H("%ld ",-s);
    else if (s>0&&s<6) H("%s ",APLpick(n0,ns,n2)[s]);
    else
    {
      if(inbraces) H("]\n");
      inbraces=!QS(s);
      H(inbraces?"%s[":"%s\n",
	QS(s)||QV(s)?(C*)(s&~aplusMask):(((A)s)->d&&QS(*((A)s)->d)?XS(*((A)s)->d)->n:"!A+ sk() print Error!"));
    }
  }
  if(inbraces)H("]\n");
}

#define BRK {if(q==1)R 0;}

Z void in(I f){NL;DO(2*f,PC(' '))}
  void pcxv(V v){H(" %s.%s",v->cx->s->n,v->s->n);}

Z void pdfn(A a)
{
  switch(a->r)
  {
    CS(2,H("(");paf((A)a->d[1],(I)1,(I)1);paf((A)a->d[0],(I)1,(I)1);H(")"););
    CS(3,H("(");paf((A)a->d[1],(I)1,(I)1);paf((A)a->d[0],(I)1,(I)1);
       paf((A)a->d[2],(I)1,(I)1);H(")"););
  default:H("*derived fn*");break;
  }
}

Z I padata(A a,I f,I x)
{
  I t,dca=0;
  C *ix=x?"":" ";
  if(!a)R 1;
  t=a->t;
  if(t>Et)
  {
    if(t==Xt) pdfn(a);
    else if(f) H("%s.%s",((CX)(a->p[a->n+2]))->s->n,XS(*a->d)->n);
    else H("%s",(C*)a->p[a->n+1]);
    R 1;
  } else {
    I an=a->n,r=a->r,j=t==Et&&!sym(a),n,k,d[9],*p=0;
    C *s=0;
    if(!an)R 1;
    if(!j)
    {
      if(t!=Ct)a=(A)mth(a),dca=1;
      if(q){if(dca)dc(a);R 0;}
      s=(C*)a->p,an=a->n,r=a->r;
      if(dca&&x&&1>=r){s++,an--;}
    } else {
      p=a->p;
      if(r<2&&sym(a)){DO(an,H("%s`%s",ix,XS(*p++)->n))R 1;}
    }
    if(r>1)for(mv(d,a->d,r),n=d[k=r-1];--k;)d[k]*=d[k+1];
    else n=r?an:1;
    for(;;)
    {
      if(j)DO(n,H("< ");paf((A)(*p++),f+1,x);if(i<n-1||an>n)in(f))
      else DO((n<=an)?n:an,if(q==1){if(dca)dc(a);R 0;}PC(*s++));
      if(0>=(an-=n)){if(dca)dc(a);R 1;}
      for(k=r;--k&&!(an%d[k]);)in(f);
    }
  }
}

I paf(A a,I f,I x)
{
  I t;
  C *ix=x?"":" ";
  BRK;
  switch(aplusMask&(I)a) 
  {
    CS(2,H("%s`%s",ix,pp((I)a)));
  case 4:if(U(a)>9){H("[paf case 4!]");pcxv(XV(a));break;}
    /* conditional fall-thru from 4 */
  case 6:CS(7,H("%s%s",ix,(f)?pp((I)a):ppd((I)a)));
    CS(1,pcxv(XV(a)));
    CS(3,paf((A)(XE(a)->f),f+1,x);H("... "));
    CS(5,t=U(a);(a=(A)*X)&&t>-a->n&&t<a->r?paf((A)(t<0?a->p[-t]:a->d[t]),f+1,x):H(" &"));
    CS(0,padata(a,f,x));
  }
  R -1;
}

void pa(V v){paf((A)v,1,0);}

Z C b[256];C Fs[]=" %.10g";
Z I bd(void){I i=0;for(;b[i]&&b[i]!='.'&&b[i]!='e';++i);R i;}

/* low-to-high minus */
Z void h(C *s){if((1==APL)&&b[1]=='-')b[1]='\242';strncpy(s,b,strlen((DEV_STRARG)b));}

Z C *iin[]={""," Inf"," -Inf"," Na", " 0"};
Z I inf(F x){R /*x==-999999999?3:*/iszero(x)?4:finite(x)?0:isnan(x)?3:x>0?1:2;}
Z I mfmt(C *b,C *s,F x)
{
  I i=inf(x);
  R i?strlen(strcpy(b,(DEV_STRARG)iin[i])):SH(x);
}
#define MaxBufLength 128
Z char buf[MaxBufLength];
Z I dfmt(C *b,C *s,I m,I n,F x)
{
  I k,l,j=inf(x);
  if(!j)
   {
     I r;
     sprintf(buf,s,m,n,x); r=strlen(buf);
     buf[m<MaxBufLength?m:MaxBufLength-1]='\0';
     strcpy(b,(DEV_STRARG)buf);
     R r;
   }
  if(4==j)R sprintf(b,s,m,n,0.0),strlen(b);
  k=strlen((DEV_STRARG)iin[j]);
  l=' '==*s;
  DO(m+l,b[i]=' ')strncpy(' '==*s?b:b+m-k,iin[j],k);
R 0;
}

A mj(A a)
{
  P p;
  I m=0,j=a->t?2:1,l,k;C *s=a->t?Fs:" %d";p.i=a->p;
  DO(a->n,if(!a->t)k=SH(p.i[i]);
       else{l=mfmt(b,s,p.f[i]);k=l-bd();if(k>j)j=k<10?k:10;k=l-k;}if(k>m)m=k);
  R m+=3+--j,gf((F)m+(F)j/10);
}

H1(mth)
{
  A z;ND1;
  {
    XA;
    P p;
    C *d,*s=(It==at)?" %ld":(Ft==at)?Fs:" `%s";
    I j=0,k,m=0,l;
    if(at==Ct)R ic(a);
    if(at==Et&&!sym(a))
    {
      if(qz(a)) {		/* Handle null format as ` */
	z=(A)gv(Ct,2);
        d=(C*)z->p;
        d[0]=' ';
        d[1]='`';
	R(I)z;
      }
      Q(ar,7);
      Q((a=(A)*a->p,!QF(a)),6);
      R (I)gsv(0,!QA(a)?pp((I)a):a->t==Xt?"*derived*":(C*)a->p[a->n+1]);
    }
    p.i=a->p;

    if(1>=ar)
    {
      /* vector/scalar */
/*
      DO(an,BRK m+=(It==at)?SH(p.i[i]):
	 (Ft==at)?mfmt(b,s,p.f[i]):
	 2+strlen(XS(p.i[i])->n));
*/
       {
       long i=0,_i=an;
 
       for(;i<_i;++i){
         {if(q==1)R 0;}
         if (It==at) {
           m += SH(p.i[i]);
         }
         else if (Ft==at) m += mfmt(b,s,p.f[i]);
         else m += 2+strlen(XS(p.i[i])->n);
          }
       }
 

      W(gv(Ct,m));
      zr(z);
      d=(C*)z->p;
#ifdef OLDCODE
      DO(an,k=(It==at)?SH(*p.i++):(Ft==at)?mfmt(b,s,*p.f++):SH(XS(*p.i++)->n);
	 h(d);d+=k);
#else
/* Can't use SH(x), b[30] not large enough to hold long symbol names */
     if( It==at || Ft==at)
	{
	  DO(an,k=(It==at)?SH(*p.i++):mfmt(b,s,*p.f++); h(d);d+=k);
	}
      else
	{
	  C *symName;
	  I symLen;
	  long _i;
	  for(_i=0; _i<an; _i++)
	    {
	      *d++=' ';
	      *d++='`';
	      symName=XS(*p.i++)->n;
	      strncpy(d, symName, symLen=strlen(symName));
	      d+=symLen;
	    }
	}
#endif
    }
    else
    {
      /* matrix (rank >= 2) */
      if(It==at) {DO(an,BRK k=SH(p.i[i]);if(k>m)m=k);}
      else if (Ft==at)
      {
	DO(an,BRK l=mfmt(b,s,p.f[i]);k=bd();if(k>j)j=k;k=l-k;if(k>m)m=k;);
	m+=j;
      }
      else {DO(an,BRK k=2+strlen(XS(p.i[i])->n);if(k>m)m=k);}
      W(ga(Ct,ar,an*m,ad));
      z->d[ar-1]*=m;
      zr(z);
      d=(C*)z->p;
      if(It==at) {DO(an,h(d+m-SH(*p.i++));d+=m);}
      else if(Ft==at){DO(an,mfmt(b,s,*p.f++);h(d+j-bd());d+=m);}
#ifdef OLDCODE
      else {DO(an,SH(XS(*p.i++)->n);h(d);d+=m);}
#else
/* Can't use SH(x), b[30] not large enough to hold long symbol names */
      else
	{
	  C *symName;
	  long _i;
	  for(_i=0; _i<an; _i++)
	    {
	      d[0]=' ';
	      d[1]='`';
	      symName=XS(*p.i++)->n;
	      strncpy(&(d[2]), symName, strlen(symName));
	      d+=m;
	    }
	}
#endif
    }
    R(I)z;
  }
}

#ifdef OLDCODE
H1(mth)
{
  A z;ND1;
  {
    XA;
    P p;C *s=at?Fs:" %d",*d;
    I j=0,k,m=0,n,l;
    if(at==Ct)R ic(a);
    if(at==Et)
    {
      Q(ar,7);
      Q((a=(A)*a->p,!QF(a)),6);
      R (I)gsv(0,!QA(a)?pp((I)a):a->t==Xt?"*derived*":(C*)a->p[a->n+1]);
    }
    p.i=a->p;
    n=ar?ad[--ar]:1;
    if(ar)DO(an,BRK if(at){l=mfmt(b,s,p.f[i]);k=bd();if(k>j)j=k;k=l-k;}
                    else k=SH(p.i[i]);if(k>m)m=k)
    else DO(an,BRK m+=at?mfmt(b,s,p.f[i]):SH(p.i[i]));
    m+=j;
    W(ga(Ct,ar+1,ar?an*m:m,ad));
    z->d[ar]=ar?m*n:m;
    zr(z);
    d=(C*)z->p;
    if(ar)DO(an,at?(mfmt(b,s,*p.f++),h(d+j-bd())):h(d+m-SH(*p.i++));d+=m)
    else DO(an,k=at?mfmt(b,s,*p.f++):SH(*p.i++);h(d);d+=k);
    R(I)z;
  }
}
#endif


H2(dth)
{ 
  A z;ND2
  if(sym(w))F1 else F2
  {
    Z I f[99],g[99],h[99];
    I wt=w->t,wr=w->r,*wd=w->d;
    I n=a->n,u,v,j=0,k=n!=1,*r;
    F x,*p=(F*)a->p;C *s,*cp;
    if(!wr)u=v=wr=1;
    else u=tr(wr-1,wd),v=wd[wr-1];
    Q(n!=v&&k,8)
    Q(n>99,12)DO(n,x=p[i];if(f[i]=x<0)x=-x;j+=g[i]=x;h[i]=.5+10*(x-g[i]);)
    W(ga(Ct,wr,u*(j=k?j:j*v),wd))z->d[wr-1]=j,s=(C*)z->p,cp=s;
    for(p=(F*)(r=w->p);u--;)DO(v,
     if(j=k?i:0,wt==Et)sprintf(s,f[j]?" %-*s":"%*s",g[j],XS(*r++)->n);
     else dfmt(s,f[j]?" %- *.*e":"%*.*f",g[j],h[j],*p++);s+=g[j])
    cp[z->n]='\0';
    R(I)z;
  }
}

/*----------  initialization and loading  -----------*/

#define P0HTSIZE 256
#define PsHTSIZE 256
#define P2HTSIZE 256

void p0hti(void)
{I i;p0ht=hti(P0HTSIZE);psht=hti(PsHTSIZE);p2ht=hti(P2HTSIZE);
 for(i=0;n0[i];++i)chtsi(p0ht,n0[i],MN(i));
 for(i=0;p0[i];++i)chtsi(p0ht,p0[i],MP(i));
 for(i=0;ns[i];++i)chtsi(psht,ns[i],MN(i));
 for(i=0;ps[i];++i)chtsi(psht,ps[i],MP(i));
 for(i=0;n2[i];++i)chtsi(p2ht,n2[i],MN(i));
 for(i=0;p2[i];++i)chtsi(p2ht,p2[i],MP(i));
}

