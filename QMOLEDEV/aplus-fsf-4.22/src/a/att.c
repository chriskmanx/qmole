/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <string.h>
#include <a/development.h>
#include <dap/balloc.h>
#include <dap/buff.h>

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/f.h>
#include <a/fir.h>
#include <a/x.h>
#include <a/arthur.h>

/*-------  Hash Table Functions  ----------*/

unsigned long hafn(unsigned long key){R key^key>>11;}

HT hti(unsigned long nb){HT ht=(HT)balloc((2+nb)*sizeof(I));
ht->nb=nb;ht->ni=0;bzero(ht->b,nb*sizeof(I));R ht;}

void mvht(HT oht,HT nht){I i;HTN node,*bp;
  for(i=0;i<oht->nb;++i)for(node=(HTN)oht->b[i];node;node=node->n){
    bp=(HTN *)HTHASH(nht,node->s);node->n=*bp;*bp=node;}}

/* htgi - get val of key from ht.  If !found,(*gf)() to create, and *pnew=1.*/
I htgi(HT ht,S key,I (*gf)(void),I *pnew)
{
  HTN *htn=(HTN*)HTHASH(ht,key),n,hd;if(gf&&pnew)*pnew=0;
  for(n=*htn;n;n=n->n)if(key==n->s)R(I)n;
  if(!gf)R 0;if(pnew)*pnew=1;n=(HTN)((*gf)());if(!n)R 0;n->s=key;
	hd = *htn ;
  if(hd){n->n=hd->n;hd->n=n;}else{*htn=n;n->n=0;}++ht->ni;R(I)n;
}

/* htsi - set val of key in ht to d.  If no entry exists, create w gf. 
   if aq is true, use ic() and dc(). */
I htsi(HT ht,S key,I a,I aq,I (*gf)(void))
{
  HTN *htn=(HTN*)HTHASH(ht,key),n,hd;
  for(n=*htn;n;n=n->n)if(key==n->s){if(aq)dc((A)n->a);n->a=(aq)?ic((A)a):a;R 0;}
  if(!gf)R 0;n=(HTN)((*gf)());if(!n)R 0;n->s=key;n->a=(aq)?ic((A)a):a;
	hd = *htn ;
  if(hd){n->n=hd->n;hd->n=n;}else{*htn=n;n->n=0;}++ht->ni;R 1;
}

/* htxi - remove key in ht.  If aq is true dc() n->a before removal. */
I htxi(HT ht,S key,I aq)
{
  HTN *htn=(HTN*)HTHASH(ht,key),n,p;
  if(!*htn)R 1;
  if(!(n=*htn))R 1;
  if(key==n->s){if(aq)dc((A)n->a);*htn=n->n;bfree((char *)n);--ht->ni;R 0;}
  for(p=n;n=n->n;p=n)
    if(key==n->s){if(aq)dc((A)n->a);p->n=n->n;bfree((char *)n);--ht->ni;R 0;}
  R 1;
}


/*----------  utilities  -----------*/

Z V av(A a)
{
  I n=a->n-1;
  if (!sym(a)||n>1) R (V)0;
  R sv(n?cxi(XS(*a->p)):Cx,XS(a->p[n]));
}

#define AttHTSIZE  1<<5

Z HT atbi(void){R hti(AttHTSIZE);}

Z ATT atti(void){ATT att=(ATT)balloc(sizeof(*att));bzero(att,sizeof(*att));R att;}


/*--------- Entrypoints --------------*/

A ep_gatt(V v,S s){ATT z;
 R(A)((!v->atb)?0:(z=(ATT)htgi(v->atb,s,0,0))?ic((A)z->a):0);}

A ep_get(A var,A att){V v;NDC2(var,att);v=av(var);
 if(!v||!sym(att)||att->n!=1)ERROUT(ERR_DOMAIN);R(A)ep_gatt(v,XS(*att->p));}

I ep_satt(V v,S s,I val){I z=qz((A)val);if(!v->atb){if(z)R 1;else v->atb=atbi();}
 R (z?htxi(v->atb,s,1):htsi(v->atb,s,val,1,(I(*)())atti));}

I ep_set(A a,A w){V v;A att,val;NDC2(a,w);v=av(a);
Q(!v||QF(w)||Et!=w->t||2!=w->n,ERR_DOMAIN);att=(A)w->p[0];val=(A)w->p[1];
Q(!sym(att)||att->n!=1,ERR_DOMAIN);R ep_satt(v,XS(*att->p),(I)val);}

A ep_atts(A var,A pre)
{
  V v;I i,count=0;ATT att;A result;HT ht;S spre,st;I z;
  NDC2(var,pre);v=av(var);z=qz(pre);
  if(!v||(!sym(pre)||pre->n!=1)&&!z)ERROUT(ERR_DOMAIN);
if(!v->atb)R(A)aplus_nl;ht=v->atb;
  spre=XS(*pre->p);
  for(i=0;i<ht->nb;++i)for(att=(ATT)ht->b[i];att;att=att->n)
    if(z||symsplit(att->s,&st)&&spre==st)++count;
  result=gv(Et,count);count=0;
  for(i=0;i<ht->nb;++i)for(att=(ATT)ht->b[i];att;att=att->n)
    if(z||symsplit(att->s,&st)&&spre==st)result->p[count++]=MS(att->s);
  R result;
}

void rmatb(V v){HT ht=v->atb;HTN n;I i;if(!ht)R;
for(i=0;i<ht->nb;++i)for(n=(HTN)ht->b[i];n;n=n->n){dc((A)n->a);bfree((char *)n);}
bfree((char *)ht);v->atb=0;R;}

void attInstall(void)
{
  install((PFI)ep_get, "_get",  A_, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep_set, "_set",  IV, 2, A_, A_,0,0,0,0,0,0);
  install((PFI)ep_atts,"_atts", A_, 2, A_, A_,0,0,0,0,0,0);
}

