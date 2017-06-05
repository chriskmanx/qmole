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

#include <dap/dap.h>

#include <a/f.h>
#include <a/fncdcls.h>
#include <a/fir.h>
#include <a/arthur.h>

H2(dot)
{
  A z;
  I i,ia,iw, buflen=128, incra,incrw, lena=0, lenw;
  C *buf=(C *)0, *stra, *strw;
  ND2
  incra=(1==a->n)?0:1,incrw=(1==w->n)?0:1;
  if (Et != a->t || Et != w->t) ERROUT(ERR_TYPE);
  if (incra&&incrw) {
    if (w->r != a->r) ERROUT(ERR_RANK);
    if (w->n != a->n) ERROUT(ERR_LENGTH);
  }

  z=(A)gd(Et,incra?a:incrw?w:a->r>w->r?a:w);
  if (!incra) {
    if (!QS(*a->p)) ERROUT(ERR_TYPE);
#if defined(HAVE_SVR4)
    stra=XS(*a->p)->n; lena=strlen((DEV_STRARG)stra);
#else
    stra=XS(*a->p)->n; lena=strlen(stra);
#endif
    if (buflen<lena+32) buflen=lena+64;
    buf=(char *)balloc(buflen);
    bcopy(stra,buf,lena);
    buf[lena]='.';
  }

  for (i=ia=iw=0;i<z->n;++i) 
  {
    if (incra) {
      if (!QS(a->p[i])) 
      {
	for(;i<z->n;++i)z->p[i]=0;
	dc(z);
	if(buf)bfree(buf);
	ERROUT(ERR_TYPE);
      }
      if ( i==0 || a->p[ia] != a->p[ia-1] ) {
#if defined(HAVE_SVR4)
	stra=XS(a->p[ia])->n; lena=strlen((DEV_STRARG)stra);
#else
	stra=XS(a->p[ia])->n; lena=strlen(stra);
#endif
	if (buflen<lena+32) {
	  buflen=lena+64;
	  if (buf) buf=(char *)brealloc(buf,buflen=lena+64);else buf=(char *)balloc(buflen);
	}
	else if (!buf) buf=(char *)balloc(buflen);
	bcopy(stra,buf,lena);
	buf[lena]='.';
      }
    }
    if (!QS(w->p[iw])) 
    {
      for(;i<z->n;++i)z->p[i]=0;
      dc(z);
      bfree(buf);
      ERROUT(ERR_TYPE);
    }
#if defined(HAVE_SVR4)
    strw=XS(w->p[iw])->n; lenw=strlen((DEV_STRARG)strw);
    if (NULL==strchr((DEV_STRARG)strw,'.')) {
#else
    strw=XS(w->p[iw])->n; lenw=strlen(strw);
    if (NULL==strchr(strw,'.')) {
#endif
      if (buflen< (2+lena+lenw)) buf=(char *)brealloc(buf,buflen=2+lena+lenw);
      bcopy(strw,buf+(1+lena),lenw);
      buf[1+lena+lenw]='\0';
      z->p[i]=MS(si(buf));
    }
    else z->p[i]=w->p[iw];
    ia+=incra;iw+=incrw;
  }

  bfree(buf);
  R(I)z;
}

H1(undot)
{
  A z; 
  C *dot, *src, *tbuf;
  I *sp, i;

  ND1
  if (8 < a->r) ERROUT(ERR_MAXRANK);
  if (Et != a->t) ERROUT(ERR_TYPE);

  z = ga(Et, 1+a->r, 2*a->n, a->d); z->d[a->r]=2;
  sp = z->p;
  for ( i=0; i<a->n; ++i) {
    if(!QS(a->p[i])){for(;i<a->n;++i){*sp++=0;*sp++=0;}dc(z);ERROUT(ERR_TYPE);}
    src=XS(a->p[i])->n;
    if (NULL==(dot=(C *)strrchr((DEV_STRARG)src,'.'))) {
      *sp++=MS(si("")); *sp++=a->p[i];
    }
    else {
      tbuf=(C*)mab(2+dot-src);
      *dot='\0';
      strcpy(tbuf,(DEV_STRARG)src);
      *dot='.';
      *sp++=MS(si(tbuf));
      mf((I *)tbuf);
      *sp++=MS(si(dot+1));
    }
  }
  R(I)z;
}
