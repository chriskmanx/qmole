/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/*************************************************************************
 *
 * "Quad FI" functions for reading numbers from char matrix.
 *
 * Malcolm Austin 
 *
 * Entry points: ep_fi ep_cfi ep_sfi ep_scfi
 *
 *
 *  This function is designed to act as an input processor for A.  cm is a
 *  character matrix.  n is the number of numbers each line should contain.
 *  The result is a floating point matrix, of dimensions ((#cm),n+1), where 
 *  result[;0] is a boolean TRUE for valid inputs, and the rest of the row
 *  is the numeric values for that row.  (Invalid rows have junk contents.)
 *
 *  ep_cfi is a mutant version of ep_fi with the following changes to the
 *  way the entries are processsed:
 *  1) All commas and left parins are removed from the entry.
 *  2) Right parins and high minus are converted to low minus.
 *
 */

#include <a/development.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <memory.h>
#include <dap/dap.h>

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>

#undef ENTRYPOINT
#define ENTRYPOINT static

#ifdef FUNCNOTUSED
/* insert s2 at s[pos], pasting over len chars, and moving others to right */
SUBROUTINE
char *
strpaste(s, s2, pos, len)
  char *s, *s2;
  int pos, len;
{
  int i, shift;
  
  /* if( limit < strlen(s)+strlen(s2)-len ) return(NULL); */
  
  if ( NULL == s2) s2 = "";
  shift = strlen((DEV_STRARG)s2) - len;
  if (0 < shift)
    for (i=strlen((DEV_STRARG)s); i>=pos+len ; --i) *(s+(i+shift)) = *(s+i);
  else if (0 > shift)
    for (i=pos+len ; i<=strlen((DEV_STRARG)s) ; ++i) *(s+(i+shift)) = *(s+i);
  
  strncpy( s+pos, s2, strlen((DEV_STRARG)s2));
  return(s);
}
#endif

SUBROUTINE
void
strrubout( str, c)
     char *str, c;
{
  char *res = str;
  while( *str && c == *str) ++str;
  while( *res++ = *str++ ) if (c == *str) ++str;
}


/* strsub replaces all occurances of target in str with repl. */
/* was subroutine, made inline for speed. */
#define strsub( str, target, repl)  \
  {C *tstr;for(tstr=(str);*tstr;++tstr) if ((target)==*tstr) *tstr =(repl);}


#ifdef FUNCNOTUSED
/* strssub does the same for strings */
SUBROUTINE
void
strssub( str, target, repl)
     char *str, *target, *repl;
{
  int tlen=strlen((DEV_STRARG)target);
  while (NULL != (str=strstr(str,target))) strpaste( str, repl, 0, tlen);
}
#endif
	 

#define sizeofAdata(a) ( a->n * SIZEOFITEM(a->t) )
SUBROUTINE
int
memclearA( aobj)
     A aobj;
{
  memset(aobj->p, 0, sizeofAdata(aobj));
  return(0);
}

double strtod();
SUBROUTINE
void
matrix_quadfi( cm, nrows, ncol, outbool, outdata, olen, commas)
  char *cm;
  I *outbool;
  F *outdata;
  I nrows, ncol, olen, commas;
{
  double num;
  char *bstart, *buffer, *rowstart = cm, *ptrchar;
  int i, j, wasspace, secondone;

  bstart=charma(1+ncol);
  for( i=0 ; i < nrows ; ++i) 
  {
    buffer=bstart;
    strncpy(buffer, rowstart, ncol);
    buffer[ncol]='\0';
    rowstart += ncol;
    
    if (commas) 
    {
      strrubout(buffer, ',');
      strsub(buffer, '(', '-');
      strsub(buffer, ')', ' ');
    }
    strsub(buffer, '¢', '-');
    strsub(buffer, 'É', 'Q');
    
    /* ugly fix for strtod() not being able to parse "00" */
    wasspace=TRUE; secondone=FALSE;
    for ( ptrchar=buffer; *ptrchar; ++ptrchar) 
    {
      if (wasspace && '0'==*ptrchar) 
      { 
	if (secondone) *(ptrchar-1)=' ';
	secondone=TRUE;
      }
      else wasspace=secondone=FALSE;
      if (' '==*ptrchar) wasspace=TRUE;
    }

    outbool[i] = 1;
    for (j=0; j<olen ; ++j) 
    {
      num = strtod(buffer, &ptrchar);
      if (ptrchar==buffer) 
      {
	outbool[i] = 0;
	break;
      }
      *(outdata+(j+i*olen))= num;
      buffer = ptrchar;
    }
    while (ISspace(*buffer)) ++buffer;
    if ('\0' != *buffer) outbool[i] = 0;
  }
  mf((I *)bstart);
}

#define SFIBUFSIZE  256
static C sfibuf[SFIBUFSIZE];  

SUBROUTINE
commavet(s,se)char *s,*se;
{
  I intvl=3,dec_pt=0,coms=0,need_dec=0;
  /* H("check: commavet [%s] s:%d se:%d\n",s,s,se); */
  while(s<=--se) 
  {
    /* H("  *se:%c  intvl:%d dec_pt:%d need_dec:%d coms:%d\n",
     *se,intvl,dec_pt,need_dec,coms); */
    switch(*se)
    {
    case '.':
      if(dec_pt||coms)return 1;
      intvl=3;dec_pt=1;need_dec=0;
      break;
    case ',':
      if(intvl)
      {
	if(dec_pt)return 1;
	else need_dec=1;
      }
      coms=1;intvl=3;
      break;
    default:
      if (ISdigit(*se))
      {
	if(!intvl)
	{
	  if(dec_pt)return 1;
	  else need_dec=1;
	}
	--intvl;
      }
      break;
    }
  }
  /* H("check: need_dec:%d intvl:%d\n",need_dec,intvl); */
  return need_dec||3==intvl;
}
  

SUBROUTINE
A scalar_quadfi(ostr, commas, dyna) char *ostr; I commas;
{
  F num;I parin=FALSE,noerror=TRUE,coms=FALSE,hitnum=FALSE;
  C c,*str=dyna?(C *)balloc(1+strlen((DEV_STRARG)ostr)):sfibuf;
  C *s1=ostr, *s2=str;

  /* H("check: nsq: ostr:[%s]\n",ostr); */
  while(noerror&&(c=*s1++)) 
  {
    /* H("  c:%c\n",c); */
    switch(c) 
    {
    case '(':if(!commas)noerror=FALSE;else{parin=TRUE;*s2++='-';}break;
    case ')':if(parin){*s2++=' ';parin=FALSE;}else noerror=FALSE;break;
    case '\242':*s2++='-';break;   /* high minus */
    case '\311':*s2++='Q';break;   /* iota--gives strtod trouble */
    case ',':coms=TRUE;if(!commas)noerror=FALSE;break;
    default:
      if (ISdigit(c))hitnum=TRUE;
      if(hitnum||!ISspace(c))*s2++=c;break;
    }
  }
  if (parin||!noerror){if(dyna)bfree(str);ERROUT(ERR_DOMAIN);}
  *s2++='\0';
  /* H("check: str:[%s] commas:%d coms:%d \n",str,commas,coms); */
  if(coms&&commavet(ostr,--s1)){if(dyna)bfree(str);ERROUT(ERR_DOMAIN);}
  /* H("check: commavet passed\n"); */
  num=strtod(str, &s1);
  if (s1==str){if(dyna)bfree(str);ERROUT(ERR_DOMAIN);}
  while (ISspace(*s1)) ++s1;
  if ('\0'!=*s1){if(dyna)bfree(str);ERROUT(ERR_DOMAIN);}
  if(dyna)bfree(str);
  return gf(num);
}
  

ENTRYPOINT
A
ep_fi( cm, n)
     A cm;
     I n;
{
  I rlen=cm->d[1], nrows=cm->d[0];
  char *data=(char *)cm->p;
  A result, bool, outdata;
  I dimens[1+MAXR];

  if (2 < cm->r) ERROUT(ERR_RANK);
  if (1000 < n) ERROUT(ERR_NONCE);
  
  if (2 == cm->r) 
  {
    dimens[0] = nrows; dimens[1] = n;
    outdata = ga(Ft, 2, nrows*n, dimens);
    memclearA( outdata);
    bool = gv(It, nrows);
    matrix_quadfi(data, nrows, rlen, bool->p, (F *)outdata->p, n, FALSE);
  }
  else 
  {
    dimens[0] = n;
    outdata = ga(Ft, 1, n, dimens);
    memclearA( outdata);
    bool = gi(0);
    matrix_quadfi(data, (I)1, nrows, bool->p, (F *)outdata->p, n, FALSE);
  }

  result = gv(Et, 2);
  result->p[0] = (I) bool; 
  result->p[1] = (I) outdata;
  return(result);
}


ENTRYPOINT
A
ep_cfi( cm, n)
     A cm;
     I n;
{
  I rlen=cm->d[1], nrows=cm->d[0];
  char *data=(char *)cm->p;
  A result, bool, outdata;
  I dimens[1+MAXR];

  if (2 < cm->r) ERROUT(ERR_RANK);
  if (100 < n) ERROUT(ERR_NONCE);
  
  if (2 == cm->r) 
  {
    dimens[0] = nrows; dimens[1] = n;
    outdata = ga(Ft, 2, nrows*n, dimens);
    memclearA( outdata);
    bool = gv(It, nrows);
    matrix_quadfi(data, nrows, rlen, bool->p, (F *)outdata->p, n, TRUE);
  }
  else 
  {
    dimens[0] = n;
    outdata = ga(Ft, 1, n, dimens);
    memclearA( outdata);
    bool = gi(0);
    matrix_quadfi(data, (I)1, nrows, bool->p, (F *)outdata->p, n, TRUE);
  }

  result = gv(Et, 2);
  result->p[0] = (I) bool; 
  result->p[1] = (I) outdata;
  return(result);
}


ENTRYPOINT
A ep_sfi(aobj)A aobj;
{
  if(1<aobj->r)ERROUT(ERR_RANK);
  return scalar_quadfi((char *)aobj->p, FALSE, SFIBUFSIZE<=aobj->n);
}

ENTRYPOINT
  A ep_scfi(aobj)A aobj;
{
  if(1<aobj->r)ERROUT(ERR_RANK);
  return scalar_quadfi((char *)aobj->p, TRUE, SFIBUFSIZE<=aobj->n);
}


void fiInstall()
{
  install((PFI)ep_cfi, "_cfi", A_, 2, CA, IV,0,0,0,0,0,0);
  install((PFI)ep_fi, "_fi", A_, 2, CA, IV,0,0,0,0,0,0);
  install((PFI)ep_sfi, "_sfi", A_, 1, CA,0,0,0,0,0,0,0);
  install((PFI)ep_scfi, "_scfi", A_, 1, CA,0,0,0,0,0,0,0);
  return;
}

