/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <a/development.h>
#include <stdio.h>
#include <string.h>
#include <a/ik.h>
#include <a/arthur.h>
#include <a/fncdcls.h>
#include <dap/balloc.h>
static I rl(I),re(void);
extern I Df,dbg_tdep,dbg_tinv,dbg_tdef,dbg_depth,dbg_trcb;

Z I y,*t,*r,Qs;Z I vl(I a){R QL(a)||QV(a);}

#define CxtabHTSIZE (1<<8)
HT CxTable;

I DependencyEvaluationSemaphore=0;

CX cxhti(void){CxTable=hti(CxtabHTSIZE);R cxi(si(""));}

Z CX gcx(void)
 {
  CX cx=(CX)balloc(sizeof(*cx));

  memset(cx, 0, sizeof(*cx));
  cx->flag = 1;
  cx->ht = hti(CxHTSIZE);
  R cx;
 }

CX cxi(S s)
 {
  CX cx=(CX)htgi(CxTable,s,(PFI)gcx,0);

  cx->flag|=1;
  R cx;
 }

CX cxlu(S s){R (CX)htgi(CxTable,s,0,0);}
/* cxlu() is like cxi() exept if context does not exist, it returns 0
   instead of creating a new one */

V vi(S s,CX cx){HT ht=cx->ht;V v,vh;V *bp=(V *)HTHASH(ht,s);
for(v=*bp;v;v=v->v)if(s==v->s)R v;
v=(V)malloc(sizeof(*v));bzero(v,sizeof(*v));v->s=s;v->cx=cx;v->z=1;++ht->ni;
if (vh=*bp) {v->v=vh->v;vh->v=v;} else *bp=v;R v;}
V vlu(S s,CX cx){HT ht=cx->ht;V v;V *bp=(V *)HTHASH(ht,s);
for(v=*bp;v;v=v->v)if(s==v->s)R v;R 0;}
/* vlu() is like vi() except if v-structure doesn't exist, it returns 0
   instead of creating a new one. */

CX cx(const C *s){R *s!='.'?cxi(si(s)):Rx;}

I gz(void){R(I)aplus_nl;}
I qz(A aobj){R ((aobj==aplus_nl)||(QA(aobj)&&aobj->t==Et&&!aobj->n&&aobj->r>0));}

Z I cvl(I a0){R vl(a0)||QE(a0)&&XE(a0)->f==MP(74);}
Z I pvl(I a0){E e;R cvl(a0)||QE(a0)&&(e=XE(a0),e->n==2&&e->f==MP(36)&&cvl(e->a[1]));}
Z I rvl(I a0){E e;R pvl(a0)||QE(a0)&&(e=XE(a0),e->n==1&&e->f==MP(22)&&pvl(e->a[0]));}

Z C *ps[]={"ws?","op?","var?","fn?","finished?","assign?","naked [?",
 "max # args 9","valence?",":header?","list too long?","too many locals?"};

Z void prr(I i,I j)
{
  extern I G;
  if(!G)
   {
    H("%s[parse]",CC);
    if (i==2)
      pa((V)j);
    H(": %s\n",ps[i]);
    if(Qs)
      if(QS(Qs))
	H("%s\n",XS(Qs)->n);
      else sk(),dc((A)Qs);
   }
  for(;*r;)mf((I*)(*r--));
  tc(r);
}

Z E mm(I n){if(*++r=(I)ma(n))R(E)*r;--r,prr(0,0);R 0;}

#define ELSE MN(1) 
I ty(I a0)
{
  I t;
  if(!QL(a0))
    R QA(a0)?0:QV(a0)?(t=XV(a0)->t,t==5?1:t):a0==MN(8)?3:a0==MN(9)?2:1;
  if(0>(a0=U(a0)))R 0;
  t=Qs||*X?((A)(Qs?Qs:*X))->t-Xt:0;R!a0?t:a0==1&&t>1||a0==2&&t==4?1:0;
}

Z I me(I n,I f,I a,I b,I c)
{
/*  E e=mm(n+2);e->n=n,e->f=f,*e->a=a;if(n>1)e->a[1]=b;if(n>2)e->a[2]=c; 
  R QP(a)&&(f==MN(8)&&QA(b)||f==MN(9)&&a!=MP(74))?
    (a=ee(e),*r=a,ef(ME(e)),a):ME(e);
*/
   E e=mm(n+2);
   e->n=n,e->f=f,*e->a=a;
 
   if(n>1)e->a[1]=b;
   if(n>2)e->a[2]=c;
   if (QP(a)&&(f==MN(8)&&QA(b)||f==MN(9)&&a!=MP(74))) {
     a=ee(e);
     *r=a;
     ef(ME(e));
     return a;
   } else {
     return ME(e);
   }

}
Z I mr(void){R *t&&*t!=';'&&*t!=')'&&*t!=']'&&*t!='}'&&*t!=ELSE;}
Z I rt(void)
{
  I f,a=0,b,c=0;
  if(!mr())prr(4,0);
  switch(f= *t++)
  {
  case MN(5):
  case MN(3):
    CSR(MN(2),*++K=U(f);if(a=rt(),y)prr(3,0);b=mr()?re():(I)aplus_nl;
       if(*t==ELSE){++t;*K=1;c=re();}--K;R me(c?3:2,f,a,b,c));
    CS('[',prr(6,0))CS('(',--t;a=rl(MN(7)))CS('{',--t;a=rl(MN(1)));
  default:y=ty(a=f);
  } 
  if(!y)for(;*t=='[';)a=rl(a);
  R a;
}
#define RLBLEN 999
#define rlbf  bfree((char *)(b==rlb?0:b))
Z I rl(I f)
{
  E e;I rlb[RLBLEN], *b=rlb,n=0,blen=RLBLEN;
  if(*t++=='[')b[n++]=f,f=MX(0);*++K=0;
  for(;*t!=']'&&*t!='}'&&*t!=')'&&*t;)
  {
    if(n==blen) 
    { 
      blen*=2;
      if (b==rlb) { b=(I*)balloc(blen*sizeof(I)); bcopy(rlb,b,RLBLEN*sizeof(I));}
      else b=(I*)brealloc((char *)b,blen*sizeof(I)); 
    }
    if(--*K,b[n++]=*t==';'?(I)aplus_nl:(re()),*t==';')++t; 
  }
  if(t[-1]==';')b[n++]=(I)aplus_nl;
  ++t;
  --K;
  if(n==1&&(f==MN(7)||!t[-1])){n=*b;rlbf;R n;}
  y=0;
  if(!n&&f==MN(7)){R rlbf,(I)aplus_nl;}
  if(QP(f)&&n!=1&&n!=2)rlbf,prr(8,0);
/*  R e=mm(n+2),e->f=f,mv(e->a,b,e->n=n),rlbf,ME(e); */
   e = mm(n+2);
   e->f = f;
   mv(e->a,b,e->n=n);
   rlbf;
   return ME(e);

}
Z I r_rf(void){I a=rt(),f;for(;mr()&&ty(f= *t)>1;y=1)
 if(++t,a=ty(f)==2?me(1,f,a,0,0):me(2,f,a,rt(),0),y>1)prr(1,0);
 R *t=='{'&&(!QN(a)||MN(6)==a)?rl(a):a;}
Z I vs(E e){DO(e->n,if(!vl(e->a[i]))R 0)R 1;}
I peak(I f){E e=XE(f);R QE(f)&&e->f==MN(9)&&*e->a==MP(74);}
Z I as(I a){I z;E e;if(!(pvl(a)||QE(a)&&(e=XE(a),peak(e->f)||(e->f==MN(7)?vs(e):
  e->f!=MP(36)&&e->f!=MP(74)&&rvl(e->a[e->f==MX(0)?0:e->n-1])))))prr(5,0);
 ++t,z=me(2,MN(0),a,re(),0);if(QV(a))XV(a)->t=y;R z;}
/*
Z I re(void){I f,a,w;a=r_rf();if(!mr())R a;if(*t==MN(0))R as(a);if(y>1)prr(1,0);
       if(f=!y){f=r_rf();if(y!=1)prr(2,a);}w=re();if(y)prr(3,0);
       R f?me(2,f,a,w,0):me(1,a,w,0,0);}
*/
 Z I re(void){
   I f,a,w;
   I bertmp;
 
   a=r_rf();
   if(!mr())
     R a;
   if(*t==MN(0))
     R as(a);
   if(y>1)
     prr(1,0);
   if(f=!y){
     f=r_rf();
     if(y!=1)
       prr(2,a);
   }
   w=re();
   if(y)
     prr(3,0);
   if (f) {
     bertmp = me(2,f,a,w,0);
     return bertmp;
   } else {
     R me(1,a,w,0,0);
   }
 }

Z I lk(I s,A f){I i;if(!f)R 0;if(f->r>1)DO(f->r,if(f->d[i]==s)R ML(i))
 for(i=f->n;--i;)if(f->p[i]==s)R ML(-i);R 0;}
Z I rz(I *b0)
{
  I i;
  for(r=t=b0;*r;++r)
    if(QS(*r)&&!ispu(*r))*r=(i=lk(*r,(A)(Qs?Qs:*X)))?i:
    MV(vi(XS(*r),Cx)); R re();
}

void f0(C *s){A aobj=(A)sv(Cx,si(s))->e;if(aobj)H("%s\n",(C*)aobj->p[aobj->n+1]);}
void f1(C *s){I *l=sv(Cx,si(s))->l;for(;l;l=(I*)*l)H("%s ",((V)l[1])->s->n);NL;}

Z void app(A aobj,I k)
{
  DO(aobj->n,if(aobj->p[i]==k)R);
  aobj->p[aobj->n]=k;
  aobj->n=++*aobj->d;
}

Z I mrg(A aobj,A wobj){
  A z;I n=aobj->n;
  if(wobj&&wobj->t==Et&&wobj->n)wobj=(A)*wobj->p;
  if(!wobj||qz(wobj))R(I)aplus_nl;
  z=gv(It,n+wobj->n),tmv(It,z->p,aobj->p,n),*z->d=z->n=n;
  DO(wobj->n,app(z,wobj->t?(I)(.5+((F*)wobj->p)[i]):wobj->p[i]));
  R(I)z;
}

Z void inv_pass1_dbg(V v,I i,I d){
  if(d&&2<=v->z)invtrc(v,1);
  if(2>=v->z){
    I *l=v->l;A z=v->i;
    if(d){++dbg_depth;if(1==v->z)invtrc(v,0);}
    for(v->z|=8;l;l=(I*)*l)inv_pass1_dbg((V)l[1],l[2]?i:0,1);
    if(z&&!qz(z))i=mrg(z,(A)i),dc(z),v->i=(A)i;
    if(d)--dbg_depth;
  }
}
Z void inv_pass1(V v,I i){
  if(2>=v->z){
    I *l=v->l;A z=v->i;
    for(v->z|=8;l;l=(I*)*l)inv_pass1((V)l[1],l[2]?i:0);
    if(z&&!qz(z))i=mrg(z,(A)i),dc(z),v->i=(A)i;
  }
}
Z void inv_pass2(V v){
  if(8&v->z){
    I *l=v->l;
    for(v->z&=6;l;l=(I*)*l)inv_pass2((V)l[1]);
  }
}

void inv(V v,I i,I d)
{
  if(2>=v->z){
    if(dbg_tinv)inv_pass1_dbg(v,i,d); else inv_pass1(v,i);
    inv_pass2(v);
  }
}

void val(V v){if(v->z=1,v->i)dc(v->i),v->i=gv(It,0);}
I gt(V v){
  if(Df&&!v->z&&v->e){
    A iwv=v->i&&!v->a?aplus_nl:v->i;I z,appf=0;E e;
    if(iwv&&aplus_nl!=iwv&&iwv->n&&v->a&&QA(v->a))
    {
      A aobj=(A)v->a;
      I lim=aobj->r?*aobj->d:1,i;
      appf=(lim<=*iwv->p)?1:0;
      for(i=1;i<iwv->n;++i)
	if(appf!=((lim<=iwv->p[i])?1:0)){iwv=aplus_nl;appf=0;break;}
    }
    e=(E)ma(3);
/*    if(v->z=4,e->f=v->e,e->n=!!iwv)if(qz((A)(*e->a=(I)iwv)))iwv=0; */
     v->z = 4;
     e->f = v->e;
 
     
     if (e->n=!!iwv)
       if(qz((A)(*e->a=(I)iwv)))
         iwv=0;

    if(dbg_tdep)deptrc(v,0);
    
    /*  DependencyEvaluationSemaphore indicates the depth of the recursive dependency */
    /*  re-evaluation.  It's incremented every time dependency re-evaluation is started */
    /*  and decremented when it's finished. */
    
    if(!oldDepModel) ++DependencyEvaluationSemaphore;
    z=(I)ez(ME(e));
    if(!oldDepModel) --DependencyEvaluationSemaphore;
    if(z)
      {
	aset((I)v,ic((A)z),appf?MP(22):(I)iwv,0);
	if(Sf&&v->rff){
	  if(dbg_trcb)cbtrc(v,2);dc(af4((A)v->rff,v->rfc,z,(I)iwv,0,v));}
	dc((A)z);
      }
    if(dbg_tdep)deptrc(v,1);
    val(v),mf((I *)e);
  }  
  for(;!v->a;)aplus_err(4,(A)MV(v));
  R v->a;
}
Z void s1(V v,I a,I i){I *l=(I*)&v->l,*n;if(a==(I)v)R;for(;n=(I*)*l;l=n)if(n[1]==a)R;
		*l=(I)(n=ma(3)),*n=0,n[1]=a,n[2]=i;}
/*
Z void s0(V v,I a){I *l=(I*)&v->l,*n;for(;n=(I*)*l;l=n)if(n[1]==a){*l=*n,mf(n);R;}}
*/
Z void s0(V v,I a){
   I *l=(I*)&v->l,*n;
   for(;n=(I*)*l;l=n)
     if(n[1]==a){
       *l=*n,mf(n);
     R;
   }
}

Z I s2(V v,I a,I n){if(QV(a))n?s1(XV(a),(I)v,0):s0(XV(a),(I)v);
 else if(QE(a)){E e=XE(a); (void)s2(v,e->f,n);
  if(n>1&&e->f==MX(0)&&QV(*e->a)&&e->a[1]==ML(1)){s1(XV(*e->a),(I)v,1);R 0;}
  if(e->f==MN(0)&&e->n==2)R s2(v,e->a[1],n); DO(e->n,s2(v,e->a[i],n))}R -1;}
/*
void rmd(V v){A a;if(a=(A)v->e)s2(v,*a->p,0),dc(a),v->e=0,dc(v->i),v->i=0,v->z=1;}
*/
 void rmd(V v){
   A a;
 
   a=(A)v->e;
   if(a)
     (void)s2(v,*a->p,0);
     dc(a);
     v->e=0;
     dc(v->i);
     v->i=0;
     v->z=1;
 }

void sad(V v,A a){rmd(v),v->e=(I)a,s2(v,*a->p,a->r),inv(v,0,1);
		 if(a->r==2)v->i=aplus_nl;}

Z I in(I s,I *b,I r){for(;r--;)if(b[r]==s)R 1;R 0;}
Z I str(I *t,I r,I *b,I n,I *p)
{
  I f;
/*
  if(f=t[-1]==')'&&t[-3]==';')--t;
*/
   f=t[-1]==')'&&t[-3]==';';
   if(f)--t;

  do 
    if(QS(*--t)&&!in(*t,b,r)&&!in(*t,p,n))
    {
      if(n==999)prr(11,0);
      p[n++]=*t;
    }
  while(f&&*--t==';');
  R n;
}

I rd(I *b)
{
  Qs=0;
  for(t=b;*t&&*t!=':';++t);
  if(!*t)R rz(b);
  for(r=t;*++r;);
  {
    A f,z;
    V v=0;
    I i,p[999],n=1,r=b[1]=='[',*j,*k,y=1,d=r||b[1]==':';
    if(d){			/* dependencies */
      if(r){			/*   itemwise */
	if(b[3]!=']'||t!=b+4) prr(9,0);
	b[3]=b[2],b[2]=*b,b+=2;
      }
      goto L;
    }
    if(t[-1]=='}')		/* fcn def */
    {
      if(t==b+3)		/* niladic f{} */
	t[-1]=*b,b=t-1;		/* f{}=>f */
      else{			/* f{a;b;...} */
	for(j=k=t;j>b;) {       /* f{a;b;...}=>fab... */
	  *--k=*(j-=2); 
	  if(2<(j-b))
	    {if(j[-1]!=';') prr(9,0);} /* chk for delimiters */
	  else 
	    {if(0!=(j-b) && j[-1]!='{') prr(9,0);}
	}
	b=k;
      }
      goto L;
    }				/* traditional fcn def  */
    if(t-b<4){			/* f a:{} or a f b:{}   */
      if(t-b==3)r=*b,*b=b[1],b[1]=r; /* a f b:{} => fab */
      goto L;
    }				/* operators */
    if(y=*b!='('){		/* l (lo mop) r:{}   OR  l (lo dop ro) r:{}  */
      if(b[1]!='('  || t[-2]!=')') prr(9,0); 
      if(ispu(b[0]) || ispu(b[2]) || ispu(b[3]) || ispu(t[-1])) prr(9,0);
      if(7==t-b && ispu(b[4])) prr(9,0);
      b[1]=b[3];		
      b[3]=b[4];		
      t[-2]=*b;                 /* l mop lo) l r:{}  OR  l dop lo ro l r:{}  */
    } else {                    /* (lo mop) r:{}     OR  (lo dop ro) r:{}    */
      if(t[-2]!=')') prr(9,0);    
      if(ispu(b[1]) || ispu(b[2]) || ispu(t[-1])) prr(9,0);
      if(6==t-b && ispu(b[3])) prr(9,0);
      r=b[1];
      b[1]=b[2];	
      b[2]=r;			/* (mop lo) r:{}     OR  (dop lo ro ) r:{}   */
    }
    y=t-b++-y;                  /* y=5 for mop,y=6 for dop & point b to name */
    y=y==5?2:y==6?3:0;
    if(!y)prr(9,0);
  L:
    r=t-b;
    if(!r)prr(9,0);
    if(QV(*b))			
      v=XV(*b),Cx=v->cx,*b=MS(v->s);
    else 
      if(QS(*b)) v=vi(XS(*b),Cx);
    DO(r,if(!QS(b[i])&&b[i]!=')')prr(9,0))  
    Qs=*b;
    if(y==3&&*XS(b[2])->n=='g')++y;
    if(r-1>MAXR)prr(7,0); 
    for(*p=0,j=t;*++t;){
      if(*t==':')prr(9,0);
      if(*t==MN(0))n=str(t,j-b,b,n,p);
    }
    f=(A)(d?v->e:v->a); 
    i=f&&QA(f)&&f->t>Xt?f->p[f->n]:0;
    z=ga(Xt,r,n+3,b),z->t+=y,z->n-=3,frep(z),z->p[n]=i,z->p[n+2]=(I)Cx;
    Qs=(I)z;
    mv(z->p,p,n),*++K=(I)z,*z->p=rz(j+1),--K;
    if(dbg_tdef)(void)deftrc(v,d);
    if (d)
      sad(v,z);
    else
      v->t=y,set(MV(v),(I)z,1);
    R (I)aplus_nl;
  }
}
