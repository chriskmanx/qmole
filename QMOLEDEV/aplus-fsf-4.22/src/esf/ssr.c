/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/*************************************************************************
 *
 * String Search and Replace utilities for A
 *
 * Malcolm Austin - V1.2 - 10/25/90
 *
 * Entry points: ep_ns ep_nsr ep_ss ep_ssr ep_gsr
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
#include <a/arthur.h>

#undef ENTRYPOINT
#define ENTRYPOINT static

SUBROUTINE
A 
gst(C *s,I len)
{
  A z=(A)gc(Ct,1,len,(I *)&len,(I *)s);
  ((C *)(z->p))[len]='\0';
  R z;
}

/*****************************************************************/
/*
 *  Modified Boyer-Moore string search
 *
 */

I *kmp_table(p, tlen) UC *p;
{
  I *next;
  int i,j,o=tlen-1;
/*  H("target:[%s]\n",p); */
  next=ma(tlen+1);
  next[0]=-1;
  for(i=0,j=-1;i<tlen;i++,j++,next[i]=(p[o-i]==p[o-j])?next[j]:j)
    while((j>=0) && (p[o-i] !=p[o-j])) j = next[j];
  R next;
}

SUBROUTINE
I *bm_table(UC *p,I tlen)
{
  I *bm_array=ma(256);
  DO(256,bm_array[i]=tlen);
  DO(tlen,bm_array[p[i]]=tlen-(i+1));
  R bm_array;
}

SUBROUTINE
UC *bm_strstr( source, slen, target, tlen, t_bm)
  UC *source, *target; 
  I *t_bm;
  I slen, tlen;
{
  UC *s=source;
  I i,j,t;
  if(slen<tlen||0==tlen)R 0;
  i=j=tlen-1;
  while(!q){
    while(source[i]!=target[j]){
      t=t_bm[source[i]];
      i+=(tlen-j>t)?tlen-j:t;
      if(i>=slen)R 0;
      j = tlen-1;
    }
    if(0==j)R s+i;
    else i--,j--;
  }
  R 0;
}
  
SUBROUTINE
UC *bm_strmat(UC *source, I slen, UC *target, I tlen, I *t_bm, UC *startp,
		I oslen, I rowlen)
{
  UC *s=source, *z=0;
  I toofar=rowlen-tlen;
  z=bm_strstr(s,slen,target,tlen,t_bm);
  while (z&&toofar<(z-startp)%rowlen)
  {
    s=z+1;
    slen=oslen-(s-startp);
    z=bm_strstr(s,slen,target,tlen,t_bm);
  }
  return z;
}
  
#ifdef FUNCNOTUSED
ENTRYPOINT
ep_bms(asource, atarget)A asource, atarget;
{
  I *t_bm;
  t_bm=bm_table((UC *)atarget->p, atarget->n);
}
#endif

/*****************************************************************/

/* strname
 * finds first occurance of target in source.  Target must be a syntatic 
 * element, defined as surrounded by non-alphamerics, including '._¢'
 * NULL returned if not found, else first occurance.
 */

static int UseAlnum;
static char *DefaultBeyondZebra="._¢`";
static char *BeyondZebra;
#define ISnamechar(X) ((UseAlnum && ISalnum(X)) ||                   \
		       ((X) && NULL!=strchr((DEV_STRARG)BeyondZebra,X)))

#define ISalnumA(X) ( ISalnum(X) || '.'==(X) || '_'==(X) || '¢'==(X) )
#define INCODE 		(!(incomment || inquote || indquote))
SUBROUTINE
UC *
strname(UC *source, I slen, UC *target, I tlen)
{
  int inquote = FALSE;
  int indquote = FALSE;
  int incomment = FALSE;
  UC *s;
  UC *stop=source+(slen-tlen);

  for ( s=source ; s <= stop ; ++s) {
    switch(*s) {
    case '\"': if (!(incomment||inquote)) indquote = !indquote; break;
    case '\\': if (indquote) ++s; break;
    case '\'': if (!(incomment||indquote)) inquote = !inquote; break;
    case 'ã':  
      if (!(inquote||indquote)) incomment = TRUE;
      break;
    case '\n': 
      if (!(inquote||indquote)) incomment = FALSE;
      break;
    default:
      if ( INCODE && 0 == bcmp(s, target, tlen)) 
	if ((s==source || !ISnamechar(*(s-1))) && 
	    !ISnamechar(*(s+tlen))) return(s);
      break;
    }
  }
  return(NULL);
}

/* strsearch
 * finds all occurances of target in source.  Returns array of occurances 
 * found.  Indexes of occurance placed into ma'ed array.  *pcount is set
 * to number of hits.  If no hits are found, NULL is returned.
 */
SUBROUTINE
A
strsearch( source, slen, target, tlen, pcount, flags, rowlen)
     UC *source, *target;
     char *flags;
     I *pcount, slen, tlen, rowlen;
{
  UC *s=source;
  I qarray = 8,oslen=slen;
  I *indexes=0;
  int namesonly=FALSE;
  I *t_bm=0;
  A z=0,za;

  if (flags) {
    if (NULL != strchr((DEV_STRARG)flags, 'n')) namesonly = TRUE;
  }
  *pcount = 0;
  if (0==tlen) return(0);
  if (!namesonly){
    t_bm=bm_table(target,tlen);if(q)goto OUT;
  }
  z=gv(It,qarray);
  indexes=z->p;
  z->n=0;
  while (NULL!=s) {
    if (namesonly) s=strname(s,slen,target,tlen); 
    else if (rowlen) s=bm_strmat(s,slen,target,tlen,t_bm,source,oslen,rowlen);
    else s=bm_strstr(s,slen,target,tlen,t_bm);
    if(q)goto OUT;
    if (s) {
      if (qarray==*pcount) {
	qarray*=2;
	za=gv(It,qarray);
	tmv(It,za->p,z->p,*pcount);
	dc(z);
	z=za;
	indexes=z->p;
      }
      indexes[(*pcount)++]=s-source;
      s+=tlen;
      slen=oslen-(s-source);
    }
  }
 OUT:
  if(t_bm)mf(t_bm);
  z->d[0]=z->n=*pcount;
  return(z);
}

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
        for ( i = strlen((DEV_STRARG)s); i>=pos+len ; --i) *(s+(i+shift)) = *(s+i);
    else if (0 > shift)
        for ( i = pos+len ; i<=strlen((DEV_STRARG)s) ; ++i ) *(s+(i+shift)) = *(s+i);

    strncpy( s+pos, s2, strlen((DEV_STRARG)s2));
    return(s);
}
#endif

SUBROUTINE
UC *
ucapaste(s, s2, s2len, pos)
    UC *s, *s2;
    I s2len, pos;
{
  if (s2==(UC *)0) s2len=0;
  if(s2len)bcopy( s2, s+pos, s2len);
  return(s);
}

/* stralias
   This function takes a list of "names"--syntactic elements (targets), and
   replaces them with the corresponding element of repls.  targets and repls
   are A objects, either char strings or vlists.
 */
SUBROUTINE
A
stralias( asource, targetarg, replarg, flags)
     A asource;
     A targetarg, replarg;
     char *flags;
{
  int i=0, ni, slen=asource->n;
  int clen, sidx=0, nhits=0;
	I rlen = 0, ridx=0 ;
  UC *r1=0,*source=(UC *)(asource->p);
  A result, targets, repls;
  I *hits, **t_bms=0;
  int namesonly=FALSE,shapepreserved=(2<=asource->r)?TRUE:FALSE;
  int rowlen=asource->d[asource->r-1];

  if (0==slen) R(A)ic(asource);

  if (flags) {
    if (NULL != strchr((DEV_STRARG)flags, 'n')) namesonly = TRUE;
  }

  if (Ct==targetarg->t) {
    if (Ct!=replarg->t) ERROUT(ERR_TYPE);
    targets=gv(Et, 1); targets->p[0]=ic(targetarg);
    repls=gv(Et, 1); repls->p[0]=ic(replarg);
  }
  else {
    targets=(A)(ic(targetarg)); repls=(A)(ic(replarg));
  }
  
  ni=targets->n;
  if(shapepreserved)
    DO(ni,if(IAF(targets,i,n)!=IAF(repls,i,n))shapepreserved=FALSE);
  if (Et==targets->t) {
    if (!namesonly){
      if(ni)t_bms=(I **)ma(ni);
      DO(ni,t_bms[i]=0);
    }
    if ( ni != repls->n ){dc(targets);dc(repls);ERROUT(ERR_LENGTH);}
    hits = ma(slen+1);
    for(i=0; i<=slen; ++i) hits[i]=-1;
    for (i=0; i<ni; ++i) { 
      A target, repl;
      I tlen;
      
      target=(A)targets->p[i];
      repl=(A)repls->p[i];
      
      if (!QA(target)||!QA(repl)){dc(targets);dc(repls);ERROUT(ERR_TYPE);}
      tlen=target->n;
      if (0==tlen) continue;
      if (Ct!=target->t||Ct!=repl->t){dc(targets);dc(repls);ERROUT(ERR_TYPE);}
      r1=source;rlen=slen;
      if (namesonly) r1=strname(r1,rlen,(UC *)target->p,tlen);
      else {
	t_bms[i]=bm_table((UC *)target->p,tlen);
	if (shapepreserved) 
	  r1=bm_strmat(r1,rlen,(UC *)target->p,tlen,t_bms[i],source,slen,rowlen);
	else r1=bm_strstr(r1,rlen,(UC *)target->p,tlen,t_bms[i]);
      }
      while( NULL!=r1 && 0>hits[r1-source] ) {
	++nhits;
	hits[r1-source] = i;
	r1+=target->n;rlen=slen-(r1-source);
	if (namesonly) r1=strname(r1,rlen,(UC *)target->p,tlen);
	else if (shapepreserved) 
	  r1=bm_strmat(r1,rlen,(UC *)target->p,tlen,t_bms[i],source,slen,rowlen);
	else r1=bm_strstr(r1,rlen,(UC *)target->p,tlen,t_bms[i]);
      }
    }
    if (nhits&&shapepreserved) {
      I srlen=asource->d[asource->r-1];
      for(i=0;i<slen;++i)
	if(0<=hits[i]&&(srlen-IAF(targets,hits[i],n))<i%srlen)
	  {hits[i]=-1;--nhits;}
    }
    if (nhits) {
      rlen=clen=slen;
      for(i=0; i<slen; ++i) 
	if(0<=hits[i]) rlen+= IAF(repls,hits[i],n) - IAF(targets,hits[i],n);
      r1 = (UC *)mab(1+rlen);
      bzero(r1, 1+rlen);
      do {	
	if (0<=hits[sidx]) {
	  ucapaste(r1,(UC *)IAF(repls,hits[sidx],p),IAF(repls,hits[sidx],n),
		   ridx);
	  ridx += IAF(repls,hits[sidx],n);
	  clen += IAF(repls,hits[sidx],n)-IAF(targets,hits[sidx],n);
	  sidx += IAF(targets,hits[sidx],n);
	}
	else r1[ridx++]=source[sidx++];
      } while (slen>sidx); 
    }
    else {r1=source;rlen=slen;}
    mf(hits);
  }
  else {q=ERR_TYPE;goto OUT;}

 OUT:
  dc(targets); dc(repls);
  if(t_bms){DO(ni,if(t_bms[i])mf(t_bms[i]));mf((I *)t_bms);}
  if(q)R 0;
  result = gst((C *)r1,rlen);
  if(shapepreserved&&slen==rlen&&2<=asource->r){
    result->r=asource->r;DO(result->r,result->d[i]=asource->d[i]);
  } 
  if (r1 != source && NULL != r1) mf((I *)r1);
  return(result);
}

ENTRYPOINT
A
ep_ssr( asource, targets, repls)
     A asource; 
     A targets, repls;
{
  NDC2(targets,repls); UseAlnum=TRUE; BeyondZebra=DefaultBeyondZebra;
  return(stralias( asource, targets, repls, "")); 
}

ENTRYPOINT
A
ep_nsr( asource, targets, repls)
     A asource;
     A targets, repls;
{
  NDC2(targets,repls); UseAlnum=TRUE; BeyondZebra=DefaultBeyondZebra;
  return(stralias( asource, targets, repls, "n"));
}


SUBROUTINE
A
ss_matrix(A source, A hits, I tlen)
{
  A z;
  I count,i,j,idx,ld=source->r-1,good=source->d[ld]-tlen,e[MAXR];
  if(hits==(A)0)R gm(It,source->r,0);
  e[ld]=1;
  for(i=ld;0<=--i;)e[i]=source->d[i+1]*e[i+1];
  for(i=count=0;i<hits->n;++i)if(good>=(hits->p[i]%source->d[ld]))++count;
  z=gm(It,source->r,count);
  for(i=idx=0;i<hits->n;++i)if(good>=(hits->p[i]%source->d[ld])){
    for(j=ld;0<=j;--j)z->p[j*count+idx]=(hits->p[i]/e[j])%source->d[j];
    ++idx;
  }
  dc(hits); /* throw away old result */
  R z;
}

/* ssmain
 * A=>C cover for string search routine.  Returns vector of indexes.
 */
SUBROUTINE
A
ssmain( source, target, options)
     A source; A target;C *options;
{
  I count=0;
  A result;

  result=strsearch((UC *)source->p,source->n,(UC *)target->p,target->n,
		   &count,options,(2>source->r)?0:source->d[source->r-1]);
  if(q){dc(result);R 0;}
  if(2<=source->r)result=ss_matrix(source,result,target->n);
  return(result?result:gv(It,0));
}

ENTRYPOINT
A  
ep_ss( source, target)A source; A target;
{
  UseAlnum=TRUE; BeyondZebra=DefaultBeyondZebra;
  return(ssmain( source, target, ""));
}

ENTRYPOINT
A
ep_ns( source, target)A source, target;
{
  UseAlnum=TRUE; BeyondZebra=DefaultBeyondZebra;
  return(ssmain( source, target, "n"));
}

/*
 * cover function for generalized ssr routine
 */
SUBROUTINE
A
gsrmain(asource, targets, repls, namechars)
     A asource;
     A targets, repls, namechars;
{
  char *s;
  UseAlnum=TRUE; BeyondZebra=DefaultBeyondZebra;
  if (IsNull(namechars)) {
    if (IsNull(repls)) {
      if (Ct!=targets->t) ERROUT(ERR_TYPE);
      return(ssmain(asource, targets, ""));
    }
    else {
      return (stralias(asource, targets, repls, ""));
    }
  }
  else {
    if (Ct != namechars->t) ERROUT(ERR_TYPE);
    s=(char *)namechars->p;
    if (namechars->n &&'þ'==s[0]) UseAlnum=FALSE, BeyondZebra=1+s;
    else UseAlnum=TRUE, BeyondZebra=s;
    if (IsNull(repls)) {
      if (Ct!=targets->t) ERROUT(ERR_TYPE);
      return (ssmain(asource, targets, "n"));
    }
    else {
      return(stralias(asource, targets, repls, "n"));
    }
  }
}
		     
ENTRYPOINT
A
ep_gsr( asource, targets, repls, namechars)
     A asource;
     A targets, repls, namechars;
{
  NDC2(targets,repls);NDC1(namechars);
  return(gsrmain(asource, targets, repls, namechars));  
}

void ssrInstall()
{
  install((PFI)ep_ns, "_ns", A_, 2, CA, CA,0,0,0,0,0,0);
  install((PFI)ep_nsr, "_nsr", A_, 3, CA, A_, A_,0,0,0,0,0);
  install((PFI)ep_ss, "_ss", A_, 2, CA, CA,0,0,0,0,0,0);
  install((PFI)ep_ssr, "_ssr", A_, 3, CA, A_, A_,0,0,0,0,0);
  install((PFI)ep_gsr, "_gsr", A_, 4, CA, A_, A_, A_,0,0,0,0);
/*  install((PFI)ep_bms, "_bms", A_, 2, A_, A_,0,0,0,0,0,0); */
  R;
}
