/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <a/k.h>
#include <fncdcls.h>
#include <a/arthur.h>

extern I nExternalFPE, suppressFpeDomain;
extern I dbg_xfpe;

Z void (*dstFP)(V) = NULL;
void setfunc_dst(void (*fp)(V))
{
  dstFP=fp;
}
void dst(V v_)
{
  if (NULL==dstFP) { H("WARNING: uninitialized dst called\n"); R; }
  suppressFpeDomain=1;nExternalFPE=0;
  if(v_!=0&&v_->attr!=0)(*dstFP)(v_);
  if(dbg_xfpe)(void)xfpechk("dst callout",nExternalFPE);
  suppressFpeDomain=0;
}


Z void (*xfFP)(void) = NULL;
void setfunc_xf(void (*fp)(void))
{
  xfFP=fp;
}
I xf(void)
{
  if (NULL==xfFP) { H("WARNING: uninitialized xf called\n"); R 0; }
  suppressFpeDomain=1;nExternalFPE=0;
  (*xfFP)();
  if(dbg_xfpe)xfpechk("xf callout",nExternalFPE);
  suppressFpeDomain=0;
  R -1;
}

Z void (*xupFP)(V,A,A,A,I,I) = NULL;
void setfunc_xup(void (*fp)(V,A,A,A,I,I))
{
  xupFP=fp;
}
void xup(V v,A d,A i,A p,I r,I uf)
{
  if (NULL==xupFP) { H("WARNING: uninitialized xup called\n"); R; }
  suppressFpeDomain=1;nExternalFPE=0;
  (*xupFP)(v,d,i,p,r,uf);
  if(dbg_xfpe)xfpechk("xup callout",nExternalFPE);
  suppressFpeDomain=0;
}

Z I (*vfyFP)(V,A) = NULL;
void setfunc_vfy(I (*fp)(V,A))
{
  vfyFP=fp;
}
I vfy(V v,A a)
{
  I z;
  if (NULL==vfyFP) { H("WARNING: uninitialized vfy called\n"); R -1; }
  suppressFpeDomain=1;nExternalFPE=0;
  z=(*vfyFP)(v,a);
  if(dbg_xfpe)xfpechk("vfy callout",nExternalFPE);
  suppressFpeDomain=0;
  R z;
}

Z void (*disableFP)(void) = NULL;
void setfunc_disable(void (*fp)())
{
  disableFP=fp;
}
void disable(void)
{
  if (NULL==disableFP) { H("WARNING: uninitialized disable called\n"); R; }
  (*disableFP)();
}

Z void (*enableFP)(void) = NULL;
void setfunc_enable(void (*fp)())
{
  enableFP=fp;
}
void enable(void)
{
  if (NULL==enableFP) { H("WARNING: uninitialized enable called\n"); R; }
  (*enableFP)();
}
