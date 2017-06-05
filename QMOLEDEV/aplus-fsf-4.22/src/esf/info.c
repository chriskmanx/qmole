/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/*************************************************************************
 *
 * "Info" functions for reporting on A entities.
 *
 * Malcolm Austin 
 *
 * Entry points: ep_name ep_valence
 *
 */

#if defined(__NetBSD__) || defined(__FreeBSD) || defined (__APPLE__)
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>
#include <a/arthur.h>

#undef ENTRYPOINT
#define ENTRYPOINT static

extern V sv();
extern C*xfs_name[];
extern I xfs_valence[];
extern I xfs_argtypes[];
extern A gsym();

Z V av(a)A a;
{
  I n=a->n-1;
  if (!sym(a)||n>1) R (V)0;
  R sv(n?cxi(XS(*a->p)):Cx,XS(a->p[n]));
}


extern C*pp();

ENTRYPOINT
A ep_name(aobj) A aobj;
{
  A a, z;
  if (!QA(aobj)||Et!=aobj->t||1!=aobj->n) ERROUT(ERR_DOMAIN);a=(A)*aobj->p;
  if (QA(a)) {
    if (Et==a->t) ERROUT(ERR_NONCE);if (Xt>=a->t) ERROUT(ERR_DOMAIN);
    z=gs(Et);z->p[0]=MS(symjoin(((CX)(a->p[a->n+2]))->s,XS(*a->d)));
  } 
  else if (QX(a)) z=gsym(xfs_name[U(a)]);
  else if (QP(a)) z=gsym(pp(a));
  else ERROUT(ERR_NONCE);
  R z;
}

ENTRYPOINT
I ep_valence(aobj) A aobj;
{
  A a=0; V v;
  if (QA(aobj)&&Xt+1==aobj->t) a=aobj;
  else {
    if (!QA(aobj)||Et!=aobj->t) ERROUT(ERR_DOMAIN);
    if (0!=(v=av(aobj))) a=(A)v->a;
    else if (1 == aobj->n) a=(A)*aobj->p;
    else ERROUT(ERR_DOMAIN);
  }
  if(0==a) ERROUT(ERR_DOMAIN);
  if (QA(a)) {
    if (Xt+1 == a->t) R a->r-1;
    if (Et==a->t || Xt<a->t) ERROUT(ERR_NONCE);
    ERROUT(ERR_DOMAIN);
  } else if (QX(a)) { R xfs_valence[U(a)]; }
  else ERROUT(ERR_NONCE);
}

ENTRYPOINT
A ep_locals(aobj) A aobj;
{
  A a=0,z,za; V v;I *zp, ux, xpa;
  if (QX(aobj)||QA(aobj)&&Xt+1==aobj->t) a=aobj;
  else {
    if (!QA(aobj)||Et!=aobj->t) ERROUT(ERR_DOMAIN);
    if (0!=(v=av(aobj))) a=(A)v->a;
    else if (1 == aobj->n) a=(A)*aobj->p;
    else ERROUT(ERR_DOMAIN);
  }
  if(0==a) ERROUT(ERR_DOMAIN);
  if (QA(a)) {
    if (Xt+1 == a->t) {
      z=gvi(Et,3,gs(Et),gv(Et,a->r-1),gv(Et,a->n-1));
      za=(A)z->p[0];za->p[0]=MS(symjoin(((CX)a->p[2+a->n])->s,XS(a->d[0])));
      za=(A)z->p[1];zp=za->p;DO(za->n,zp[i]=a->d[i+1]);
      za=(A)z->p[2];zp=za->p;DO(za->n,zp[i]=a->p[i+1]);
      R z;
    }
    if (Et==a->t || Xt<a->t) ERROUT(ERR_NONCE);
    ERROUT(ERR_DOMAIN);
  } else if (QX(a)) { 
    ux=U(a);
    z=gvi(Et,3,gsym(xfs_name[ux]),gv(It,xfs_valence[ux]),gz());
    za=(A)z->p[1];zp=za->p;
    xpa=xfs_argtypes[ux];DO(za->n,{zp[i]=xpa%16;xpa>>=4;});
    R z;
  }
  else ERROUT(ERR_NONCE);
}


void infoInstall()
{
  install((PFI)ep_name, "_name", A_, 1, A_,0,0,0,0,0,0,0);
  install((PFI)ep_valence, "_valence", IV, 1, A_,0,0,0,0,0,0,0);
  install((PFI)ep_locals, "_locals", A_, 1, A_,0,0,0,0,0,0,0);
  R;
}

