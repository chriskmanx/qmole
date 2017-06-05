/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.
*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/

/*
 * This file replaces b.c to remove all references to the "buddy system"
 * in m.c.  ma(), mab(), and mf() are replaced here with covers to
 * malloc() and free().  mi(), which initializes the buddy system, is
 * replaced with a no-op.
 *
 *
 */


#include <pthread.h>		/* Needed for locking in *_cover()  */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <a/k.h>
#include <a/fir.h>
#include <a/x.h>
#include <a/fncdcls.h>

#if (defined(__sgi) && _MIPS_SZLONG == 64) || defined(__sparcv9) || defined(__ia64) || defined(__x86_64)
#define MD      63
#else
#define MD      31
#endif

static long totalSize[MD];
static long totalObjs[MD];
static long totalSizeMax[MD];
static long totalObjsMax[MD];
static long memTrack=0;	
static long memTrackThreshold=0;	

static pthread_mutex_t memStats_lock = PTHREAD_MUTEX_INITIALIZER;

#if defined(linux)
static int initMutex=1;

static int _initMutex(void)
{
  int rc;
  pthread_mutexattr_t mattr;
  if(0!=(rc=pthread_mutexattr_settype(&mattr, PTHREAD_MUTEX_ADAPTIVE_NP)))
    {
      perror("initMutex():pthread_mutexattr_settype");
    }
  if (rc==0 && pthread_mutex_init(&memStats_lock, &mattr))
    {
      perror("initMutex():thread_mutex_init"); 
    }
  return 0;
}
#endif

char * _mab(unsigned long w)
{
  char *p;
  unsigned long *pl;

  if(0==w)
  {
    printf("\343 ma(): Warning: attempt to allocate 0 bytes\n");w=1;
  }

  if ((pl = (unsigned long *)malloc(w+2*sizeof(*pl))) == (unsigned long *)(0))
    {
      unsigned long aBytes=w+2*sizeof(*pl);
      printf("\343 ma(): malloc(%lu) failure: errno=%d\n",aBytes,errno);
      aplus_err(2, (A)aBytes);
      p=(char *)pl;
    }
  else
    {
      pl[0]=w;
      pl[1]=w;
      p=(char *)&(pl[2]);
    }
  return p;
}

char * _mab_cover(unsigned long w)
{
  unsigned long z,i;
  char *p=_mab(w);
  if(p)
    {
      int rc;
      unsigned long ww=w+2*sizeof(long *);

#if defined(linux)
      if(initMutex) 
	  initMutex=_initMutex();
#endif

      /* Test for 8bit alignment */
      if(!QA(p) )
       	printf("!! pointer not 8bit aligned 0x%x\n",p);

      if(memTrack && ww>=memTrackThreshold) 
	printf("0x%x malloc %lu bytes\n",p,ww);

      for (z = ww >> 1, i = 1; z != 0; z >>= 1, i++);
      if( i >= MD ) i=MD-1;

      if(0!=(rc=pthread_mutex_lock(&memStats_lock)))
	{
	  perror("si() pthread_mutex_lock");
	}

      totalObjs[i]++;
      totalSize[i] += ww;
      if(totalObjs[i]>totalObjsMax[i])
	totalObjsMax[i]=totalObjs[i];
      if(totalSize[i]>totalSizeMax[i])
	totalSizeMax[i]=totalSize[i];
      
      if(rc==0 && pthread_mutex_unlock(&memStats_lock))
	{
	  perror("si() pthread_mutex_unlock");
	}

    }
  return p;
}

static char *(*pf_mab)(unsigned long)=_mab;

char * mab(unsigned long w) 
{
  return pf_mab(w);
}

long *ma(unsigned long w)
/* w - number of words required */
{
  return (long *)(mab(w*sizeof(long)));
}

static void _mf(long *p)
{

  if(p && (*(p-1))==(*(p-2)))
    {
      free (p-2);
    }
  else
    {
      printf("!! Not an mab pointer !!\n");
      free(p);
    }
}

static void _mf_cover(long *p)
{
  unsigned long z,i,w;
  if(p && (w=*(p-1))==(*(p-2)))
    {
      int rc;

      unsigned long ww=(*(p-2))+2*sizeof(long *);

#if defined(linux)
      if(initMutex) 
	  initMutex=_initMutex();
#endif

      if(memTrack && ww>=memTrackThreshold) 
	printf("0x%x freeing %lu bytes\n",p,ww);

      for (z = ww >> 1, i = 1; z != 0; z >>= 1, i++);
      if( i >= MD ) i=MD-1;

      if(0!=(rc=pthread_mutex_lock(&memStats_lock)))
	{
	  perror("si() pthread_mutex_lock");
	}

      if(totalObjs[i]) totalObjs[i]--;
      if(totalSize[i]) totalSize[i] -= ww;

      if(rc==0 && pthread_mutex_unlock(&memStats_lock))
	{
	  perror("si() pthread_mutex_unlock");
	}

    }
  _mf(p);
}

void (*pf_mf)(long *)=_mf;

void mf(long *p)
{
  pf_mf(p);
}

void setAplusMemStatsMode(int mode_)
{
  if(mode_==1)			/* enable stats */
    {
      pf_mab=_mab_cover;
      pf_mf=_mf_cover;
    }
  else				/* disable stats */
    {
      pf_mab=_mab;
      pf_mf=_mf;
    }
}

static A ep_memStats(A aObj)
{
  if(aObj->n != 1) 
    return q=ERR_LENGTH,(A)0;
  
  if(QA(aObj) &&  aObj->t==It )
    {
      memTrackThreshold=aObj->p[0];
      return aplus_nl;
    }
  
  if( !QA(aObj) && aObj->t!=Et && !QS(aObj->p[0]) )
    return q=ERR_LENGTH,(A)0;

  if(aObj->p[0]==MS(si("on")))
    {
      setAplusMemStatsMode(1);
    }
  else if(aObj->p[0]==MS(si("verbose")))
    {
      memTrack=1;
    }
  else if(aObj->p[0]==MS(si("quiet")))
    {
      memTrack=0;
    }
  else if(aObj->p[0]==MS(si("off")))
    {
      setAplusMemStatsMode(0);
    }
  else if(aObj->p[0]==MS(si("info")))
    {
      I d[9]={MD};
      return (A)gvi(Et,4,
		    gc(It,1,MD,d,totalSize),
		    gc(It,1,MD,d,totalObjs),
		    gc(It,1,MD,d,totalSizeMax),
		    gc(It,1,MD,d,totalObjsMax));
    }
  else if(aObj->p[0]==MS(si("reset")))
    {
      memset(totalSize, 0, sizeof(totalSize)*sizeof(unsigned long));
      memset(totalObjs, 0, sizeof(totalSize)*sizeof(unsigned long));
    }
  return aplus_nl;
}

void memStatsInstall(void)
{ 
  install((PFI)ep_memStats,"_memStats",A_,1,A_,0,0,0,0,0,0,0);  
  return; 
}

void mi(void) {}

#if 0
/* Old Version of bstub.c */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <a/k.h>

char * mab(unsigned long w)
{
  char *p;
  unsigned long *pl;

  if(0==w)
  {
    printf("\343 ma(): Warning: attempt to allocate 0 bytes\n");w=1;
  }

  if ((pl = (unsigned long *)malloc(w+2*sizeof(*pl))) == (unsigned long *)(0))
    {
      unsigned long aBytes=w+2*sizeof(*pl);
      printf("\343 ma(): malloc(%lu) failure: errno=%d/n",aBytes,errno);
      aplus_err(2, aBytes);
      p=(char *)pl;
    }
  else
    {

      pl[0]=w;
      pl[1]=w;
      p=(char *)&(pl[2]);
    }
  return p;
}

long *ma(unsigned long w)
/* w - number of words required */
{
  return (long *)(mab(w*sizeof(long)));
}

void mf(long *p)
{

  if(p && (*(p-1))==(*(p-2)))
    free (p-2);
  else
    {
      printf("!! Not an mab pointer !!\n");
      free(p);
    }
}

void mi(void) {}
#endif
