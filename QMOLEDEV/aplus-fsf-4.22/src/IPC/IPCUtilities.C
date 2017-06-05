///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#if !defined(__cfront)
#include <stdarg.h>
#endif
#include <string.h>
#include <memory.h>
#include <math.h>
#include <sys/time.h>
#include <sys/types.h>
#include <netinet/in.h>

#include <dap/Warn.h>
#include <a/k.h>
#include <a/fncdcls.h>

#include <MSIPC/MSTv.H>

#include <arpa/inet.h>


/* ipcWarnFlag controls the behavior of ipcWarn() calls.  Its values are:
    -1 = no ipcWarn or Warn messages
    0 = ipcWarn messages only if the debug flag is turned on for a scb
    1 = like 0, but also print messages for ipcWarn() calls with scb==0
    2 = print all ipcWarn messages, regardless of scb's debug flag setting

    Iff ipcWarnFlag<0, then all Warn messages appear as well.

   The value of ipcWarnFlag is set through iDebug and iSetDebug.
*/
extern I ipcWarnFlag;

/*
    warnLevel is equal to -1, 0, or 1, and represents the "terseness"
    level.  ipcWarnFlag must exceed warnLevel for the message to
    be generated.
*/
void ipcWarn(int warnLevel_, const C *fmt_, ...)
{
  va_list ap;
  
  va_start(ap,fmt_);
  if(ipcWarnFlag>warnLevel_) vWarn((char *)fmt_, ap); // Note cast
  va_end(ap);
}

I longAt(C *c){
  int l;

  ipcWarn(0,"%t longAt\n"); 
  memmove(((C *)(&l)),c,sizeof(l));
  return ntohl(l);
}

I shortAt(C *c){
  short s;

  ipcWarn(0,"%t shortAt\n"); 
  memmove(((C *)(&s)),c,sizeof(s));
  return ntohs(s);
}

/*
 * atotv
 * This takes an A object and a pointer to an allocated timeval struct.
 * If the A object is valid, the values of the timeval struct are filled
 * in, and a pointer to it is returned.  Else NULL is returned.
 *
 * A valid aobj is either:
 * 1) A floating point object with one element.  The element
 *    is used to represent the time in seconds.
 * 2) An integer object with one element or two elements.  The first
 *    element represents a whole number of seconds.  If there is a
 *    second element, it represents the number of milliseconds.
 * 3) An integer object with three elements.  If the third element
 *    equals 1, the first two elements are treated as a termination
 *    time, otherwise they are treated as in case 2) above.
 *
 * In any event, the timeval struct is filled with the termination
 * time.
 *
 */
struct timeval *atotv(A aobj, struct timeval *tvp)
{
  struct timeval timeleft, now;
  F fseconds;

  ipcWarn(0,"%t atotv\n");
  if (Ft==aobj->t && 1==aobj->n) {
    fseconds = *(F *)(aobj->p);
    gettimeofday(&now,NULL);
    timeleft.tv_sec = (int)(floor(fseconds));
    timeleft.tv_usec = (int)(1000000.0*(fseconds-floor(fseconds)));
    tvsum(&now,&timeleft,tvp);
    return tvp;
  }
  else if (It==aobj->t && 1<=aobj->n && 3>=aobj->n) {
    if (3==aobj->n && 1==aobj->p[2]) {
      if (0>aobj->p[1]) return 0;
      tvp->tv_sec=aobj->p[0];
      tvp->tv_usec=aobj->p[1];
    } else {
      gettimeofday(&now,NULL);
      timeleft.tv_sec=aobj->p[0];
      timeleft.tv_usec=(2<=aobj->n)?aobj->p[1]:0;
      tvsum(&now,&timeleft,tvp);
    }
    return tvp;
  } else return 0;
}

A getAbsoluteTimeout(A aobj_)
{
  struct timeval gameover, *tvp;
  ipcWarn(0,"%t getAbsoluteTimeout\n");

  tvp = atotv(aobj_, &gameover);
  if (NULL==tvp) return(A)0;
  else return gvi(It,3,tvp->tv_sec,tvp->tv_usec,1);
}

