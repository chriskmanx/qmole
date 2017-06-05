/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/*
 * This file contains the FSA machine parser for Steve's "d" system.
 */


#include <a/development.h>
#include <stdio.h>
#include <string.h>

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/x.h>
#include <a/fir.h>

#undef ENTRYPOINT
#define ENTRYPOINT static

SUBROUTINE
int
VetteMachine (machine) A machine;
{
  int i, j, len;
  A aobj, element, subelement, tokens;

  if (Et != machine->t || 1 != machine->r || 4 != machine->n ) return 1;
  for (i=0; i<machine->n; ++i) { if (!QA(machine->p[i])) return 2; }
  
/* names */
  aobj=(A) machine->p[0];
  if (0==(len=aobj->n)) return 3;
  if (Et != aobj->t || 1 < aobj->r ) return 4;
  for (i=0; i<aobj->n; ++i) { if (!QS(aobj->p[i])) return 5; }
  
/* tokens */
  aobj=tokens=(A) machine->p[1];
  if (Et != aobj->t || 1 < aobj->r || len != aobj->n ) return 6;
  for (i=0; i<aobj->n; ++i) { 
    if (!QA(element=(A) aobj->p[i])) return 7;
    if (Et != element->t || 0 == element->n ) return 8;
    for (j=0; j<element->n; ++j) {
      if (!QA(subelement=(A) element->p[j])) return 81;
      if (Ct != subelement->t || 0 == subelement->n ) return 82;
    }
  }

/* states */
  aobj=(A) machine->p[2];
  if (Et != aobj->t || 1 < aobj->r || len != aobj->n ) return 9;
  for (i=0; i<aobj->n; ++i) { 
    if (!QA(element=(A) aobj->p[i])) return 10;
    if (It != element->t || 2 != element->r || 0 == element->n ||
	element->d[1] != 1+((A)tokens->p[i])->n) return 11;
  }

/* statenames */
  /* Who cares?  They aren't used. */

  return 0;

}

/* gst
 * Cover for A entrypoint gsv() that *always* returns a vector.
 */
SUBROUTINE
A 
gst( x, s) I x; C *s; { A r=(A) gsv(x,s); r->r=1; return(r); }


SUBROUTINE
A
MakeEmptyResult( machine) A machine;
{
  A result;
  
  result = gv(Et,3);
  result->p[0] = ((A)(machine->p[0]))->p[0];
  result->p[1] = (I)gv(It, 0);
  result->p[2] = 0;

  return(result);
}

static char buffer[512];

SUBROUTINE
A
MakeResult( sym, s, toktype, len) I sym; char *s; I *toktype, len;
{
  I nparts, curtype, i, idx=0, bufidx;
  A result, aobj, typeobj;

  result=gv(Et, 3);
  aobj = gs(Et);
  aobj->p[0] = sym;
  result->p[0] = (I) aobj;

  nparts=0; curtype=-1;
  for (i=0; i<len; ++i ) 
    if (toktype[i] != curtype) { curtype=toktype[i]; ++nparts; }

  typeobj=gv(It,nparts);
  aobj=gv(Et,nparts);

  bufidx = 0;
  curtype=-1;
  for (i=0; i<=len; ++i) {
    if ( i == len || toktype[i] != curtype ) 
      if (i == len || -1 != curtype) {
	buffer[bufidx++]='\0';
	aobj->p[idx++] = (I) gst(0, buffer);
	bufidx=0;
      }
    typeobj->p[idx]=curtype=toktype[i];
    buffer[bufidx++] = s[i];
  }
  result->p[1] = (I) typeobj;
  result->p[2] = (I) aobj;

  return(result);
}


static char tokenstr[512];
static int  tokentype[512];
static int tokencount;

SUBROUTINE
A
RunMachine( s, forms, tokenobj, statesobj) 
     char *s; 
     A forms, tokenobj, statesobj;
{
  A result=NULL, curtoken;
  I *tokens, *statemat, slen, tklen, formno, i, j, idx, nr, nc, state;
  char *tokes, *s1;

  slen = strlen((DEV_STRARG)s);
  tokens = ma(slen);

  for (formno=0; NULL == result && formno<forms->n ; ++formno) {
    curtoken=(A) tokenobj->p[formno];
    statemat=((A) statesobj->p[formno])->p;
    nr=((A) statesobj->p[formno])->d[0];
    nc=((A) statesobj->p[formno])->d[1];
    tokencount=curtoken->n; 
    tokenstr[0]='\0'; 
    idx=0;
    for (i=0; i<tokencount ; ++i ) {
      tokes = (char *) ((A) curtoken->p[i])->p;
      tklen=strlen((DEV_STRARG)tokes);
      for (j=0; j<tklen; ++j) tokentype[idx++]=i;
      strcat( (DEV_STRARG)tokenstr, (DEV_STRARG)tokes);
    }

    state=0;
    for (i=0; -1 != state && i<slen; ++i) {
      if (NULL==(s1=(char *)strchr((DEV_STRARG)tokenstr, s[i]))) state=-1;
      else {
	tokens[i] = tokentype[s1-tokenstr];
	state = statemat[tokens[i]+state*nc];
	if (nr<=state) state=-1;
      }
    }
    if ( -1 != state && -1 != statemat[state*nc+nc-1]) 
      result=MakeResult( forms->p[formno], s, tokens, slen); 
      
  }

  if (NULL==result) {
    result=gv(Et,3);
    result->p[0]=(I) gv(Et,0);    
    result->p[1]=(I) gv(Et,0);    
    result->p[2]=(I) gv(Et,0);
  }
  mf(tokens);
  return(result);
}
	


ENTRYPOINT
A
ep_cform(s, machine) char *s; A machine;
{
  A result;

  if (VetteMachine( machine)) ERRMSG("machine");
  if (0==strlen((DEV_STRARG)s)) result=MakeEmptyResult(machine);
  else result=RunMachine(s,(A)machine->p[0],(A)machine->p[1],(A)machine->p[2]);

  return(result);
}

  
void cformInstall()
{
  CX saveCx=Cx;
  Cx=cx("c");

  install((PFI)ep_cform, "form", A_, 2, CP, A_,0,0,0,0,0,0);

  Cx=saveCx;
  return;
}
