///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <MSIPC/MSTv.H>
#include <MSTypes/MSMessageLog.H>

struct timeval tod_l;

/* p1 is compared to p2.  If it is earlier,-1 is returned.
 * If it is latter 1 is returned.  If it is the same,0 is
 * returned.  It is assumed that the inputs are normalized.
 */

int tvcmp(struct timeval *p1,struct timeval *p2)
{
  if (p1->tv_sec<p2->tv_sec) return -1;
  else if (p1->tv_sec>p2->tv_sec) return 1;
  else if (p1->tv_usec<p2->tv_usec) return -1;
  else if (p1->tv_usec>p2->tv_usec) return 1;
  return 0;
}

/* p2 is subtracted from p1 and the result is placed in p3.
 * 0 is returned if the operation succeeds.  1 is
 * returned if a positive overflow was encountered
 * while preparing the result.  2 is returned if
 * negative overflow was encountered.  The inputs are
 * assumed to be normalized.
 */

int tvdiff(struct timeval *p1,struct timeval *p2,struct timeval *p3)
{
  if ((p1->tv_sec<(long)(0))&&(p2->tv_sec>(long)(0)))
   {
     p3->tv_sec=p1->tv_sec-p2->tv_sec;
     if (p3->tv_sec>=(long)(0))
      {
        MSMessageLog::warningMessage("tvdiff: Warning: negative overflow\n");
        p3->tv_sec=LONG_MIN;
	p3->tv_usec=(long)(0);
	return 2;
      }
     p3->tv_usec=p1->tv_usec-p2->tv_usec;
   }
  else if ((p1->tv_sec>(long)(0))&&(p2->tv_sec<(long)(0)))
   {
     p3->tv_sec=p1->tv_sec-1;
     p3->tv_sec-=p2->tv_sec+1;
     if (p3->tv_sec<(long)(0))
      {
        /* overflow positive */
        MSMessageLog::warningMessage("tvdiff: Warning: positive overflow\n");
	p3->tv_sec=LONG_MAX;
	p3->tv_usec=MILLION_USECS-1;
	return 1;
      }
     p3->tv_usec=p1->tv_usec+MILLION_USECS;
     p3->tv_usec-=p2->tv_usec-MILLION_USECS;
   }
  else
   {
     p3->tv_sec=p1->tv_sec-p2->tv_sec;
     p3->tv_usec=p1->tv_usec-p2->tv_usec;
   }
  return tvnorm(p3);
}


/* If sec_i and usec_i are the initial time value
 * members,and sec_f and usec_f are the final
 * time value members,then the normalized time value
 * satisfies
 *  1000000*sec_f+usec_f=1000000*sec_i+usec_i
 * and
 *  0<=usec_f<1000000
 * Normalization is optimized for normalizing sums
 * and differences of normalized time values.
 *
 * returns 0 if successful
 * returns 1 if normalization caused overflow of positive time
 * returns 2 if normalization caused overflow of negative time
 *
 * The present implementation assumes a signed 2's complement
 * integer representation for type time_t.  Furthermore,it
 * is assumed that integer addition overflow is not trapped.
 *
 * Upon overflow to either positive or negative values,
 * a warning message is logged and the maximum or minumum possible
 * normalized timeval is substituted.
 */

int tvnorm(struct timeval *p)
{
  register long usec=p->tv_usec;
  
  if (usec>=MILLION_USECS)
   {
     register long sec;
     
     if (usec>=(2*MILLION_USECS))
      {
	/* usec is not the result of sum of two normalized
	 *time values.  We therefore use slow division and
	 *remainder to normalize it.
	 */
	sec=p->tv_sec+usec / MILLION_USECS;
	usec %= MILLION_USECS;
      }
     else
      {
	/* we use fast reduction */
	sec=p->tv_sec+1;
	usec-=MILLION_USECS;
      }
     if (sec<p->tv_sec)
      {
	/* overflow positive */
        MSMessageLog::warningMessage("tvnorm: Warning: positive overflow\n");
	p->tv_sec=LONG_MAX;
	p->tv_usec=MILLION_USECS-1;
	return 1;
      }
     p->tv_sec=sec;
     p->tv_usec=usec;
   }
  else if (usec<(long)(0))
   {
     register long sec;
     
     if ((usec+MILLION_USECS)<(long)(0))
      {
	/* usec is not the result of diff of two normalized
	 *time values.  We therefore use slow division and
	 *remainder to normalize it.
	 */
	usec=- 1-usec;	 /* no overflow in two-complement */
	sec=(p->tv_sec-1)-(usec / MILLION_USECS);
	usec=(MILLION_USECS-1)-(usec % MILLION_USECS);
      }
     else
      {
	/* we use fast reduction */
	sec=p->tv_sec-1;
	usec+=MILLION_USECS;
      }
     if (sec>p->tv_sec)
      {
	/* overflow negative */
        MSMessageLog::warningMessage("tvnorm: Warning: negative overflow\n");
	p->tv_sec=LONG_MIN;
	p->tv_usec=(long)(0);
	return 2;
      }
     p->tv_sec=sec;
     p->tv_usec=usec;
   }
  return 0;
}

/* p1 is added to p2 and the result is placed in p3.
*0 is returned if the operation succeeds.  1 is
*returned if a positive overflow was encountered
*while preparing the result.  2 is returned if
*negative overflow was encountered.  The inputs are
*assumed to be normalized.
 */

/* external function definitions */
int tvsum(struct timeval *p1,struct timeval *p2,struct timeval *p3)
{
  if ((p1->tv_sec<(long)(0))&&(p2->tv_sec<(long)(0)))
   {
     p3->tv_sec=(p1->tv_sec+1);
     p3->tv_sec+=(p2->tv_sec+1);
     if (p3->tv_sec>=(long)(0))
      {
        MSMessageLog::warningMessage("tvsum: Warning: negative overflow\n");
	p3->tv_sec=LONG_MIN;
	p3->tv_usec=(long)(0);
	return 2;
      }
     p3->tv_usec=(p1->tv_usec-MILLION_USECS);
     p3->tv_usec+=(p2->tv_usec-MILLION_USECS);
   }
  else if ((p1->tv_sec>(long)(0))&&(p2->tv_sec>(long)(0)))
   {
     p3->tv_sec=p1->tv_sec+p2->tv_sec;
     if (p3->tv_sec<(long)(0))
      {
	/* overflow positive */
        MSMessageLog::warningMessage("tvsum: Warning: positive overflow\n");
	p3->tv_sec=LONG_MAX;
	p3->tv_usec=MILLION_USECS-1;
	return 1;
      }
     p3->tv_usec=p1->tv_usec+p2->tv_usec;
   }
  else
   {
     p3->tv_sec=p1->tv_sec+p2->tv_sec;
     p3->tv_usec=p1->tv_usec+p2->tv_usec;
   }
  return tvnorm(p3);
}

time_t todsec(void) { return (time_t)tod()->tv_sec; }
struct timeval *tod(void)
{
  static int first=0;
  struct timeval now;
  
  if (first==0) // JMB: done in dapterm
   {
     tod_l.tv_sec=(long)(0);
     tod_l.tv_usec=(long)(0);
     first=1;
   }
  if (gettimeofday(&now,(struct timezone *)(0))<0)
   {
     MSMessageLog::warningMessage("tod: Error: gettimeofday()\n");
   }
  else tod_l=now;
  return &tod_l;
}



   





