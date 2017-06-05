/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <unistd.h>
#include <stdio.h>
#include <sys/times.h>
#include <limits.h>
#include <a/fir.h>
#include <a/k.h>
#include <a/x.h>

#define NUM_TYPES 4
#define NUM_BUCKETS 9
#define NUM_CPU_BUCKETS 2

#define MONADIC_ARRAY_SIZE ((sizeP1*NUM_TYPES*NUM_BUCKETS)*sizeof(PFI))
#define DYADIC_ARRAY_SIZE  ((sizeP2*NUM_TYPES*NUM_BUCKETS)*sizeof(PFI))

#define MONADIC_CPU_ARRAY_SIZE ((sizeP1*NUM_TYPES*NUM_CPU_BUCKETS)*sizeof(PFI))
#define DYADIC_CPU_ARRAY_SIZE  ((sizeP2*NUM_TYPES*NUM_CPU_BUCKETS)*sizeof(PFI))

extern PFI P1[];
extern PFI P2[];
extern I profileExecute();
extern I sizeOfPrimArray();
extern C **get_primlist();

static PFI *saveP1=NULL;
static PFI *saveP2=NULL;

static I sizeP1;
static I sizeP2;

static I (*dyadic)[][NUM_TYPES][NUM_BUCKETS]=NULL;
static I (*monadic)[][NUM_TYPES][NUM_BUCKETS]=NULL;

static I (*cpuDyadic)[][NUM_TYPES][NUM_CPU_BUCKETS]=NULL;
static I (*cpuMonadic)[][NUM_TYPES][NUM_CPU_BUCKETS]=NULL;

static I clockTicksPerSec=1;

I profileDyadic(A a, A w, I i)
{
  struct tms tStart, tEnd;
  I typeIdx=-1, sizeIdx;
  I z;

  if( i<sizeP2 )
    {
      if( QA(a) && a->t<=Et )
	{
	  typeIdx=(a->t!=Et)  ? a->t : 3;    /* Set type index */
	  sizeIdx=(a->n< 2       )? 1:       /* Set size index */
	    (a->n< 10      )? 2    :
	    (a->n< 100     )? 3    :
	    (a->n< 1000    )? 4    :
	    (a->n< 10000   )? 5    :
	    (a->n< 100000  )? 6    :
	    (a->n< 1000000 )? 7    : 8;
	  (*dyadic)[i][typeIdx][sizeIdx]++;     /* increment count */
	  (*dyadic)[i][typeIdx][0]=1;           /* isSet indicator */  
	}

      if( QA(w) && w->t<=Et  )
	{
	  typeIdx=(w->t!=Et)  ? w->t : 3;    /* Set type index */
	  sizeIdx=(w->n< 2       )? 1:       /* Set size index */
	    (w->n< 10      )? 2    :
	    (w->n< 100     )? 3    :
	    (w->n< 1000    )? 4    :
	    (w->n< 10000   )? 5    :
	    (w->n< 100000  )? 6    :
	    (w->n< 1000000 )? 7    : 8;
	  (*dyadic)[i][typeIdx][sizeIdx]++;     /* increment count */
	  (*dyadic)[i][typeIdx][0]=1;           /* isSet indicator */  
	}
    }

  times(&tStart);
  z=profileExecute(2,saveP1,saveP2,i) ;
  times(&tEnd);

  if(typeIdx!=-1 && i<sizeP2)
    {
      (*cpuDyadic)[i][typeIdx][0]+=
	(1000*(tEnd.tms_utime-tStart.tms_utime))/clockTicksPerSec;
      (*cpuDyadic)[i][typeIdx][1]+=
	(1000*(tEnd.tms_stime-tStart.tms_stime))/clockTicksPerSec;
    }

  return z;
}

I profileMonadic(A a, I i)
{
  struct tms tStart, tEnd;
  int typeIdx=-1, sizeIdx;
  I z;

  if(i<sizeP1 && QA(a) && a->t<=Et ) 
    {
      typeIdx=(a->t!=Et)  ? a->t : 3;	    /* Set type index */
      sizeIdx=(a->n< 2       )? 1 :         /* Set size index */
	(a->n< 10      )? 2    :
	(a->n< 100     )? 3    :
	(a->n< 1000    )? 4    :
	(a->n< 10000   )? 5    :
	(a->n< 100000  )? 6    :
	(a->n< 1000000 )? 7    : 8;
      (*monadic)[i][typeIdx][sizeIdx]++;       /* increment count */
      (*monadic)[i][typeIdx][0]=1;             /* isSet indicator */  
    }

  times(&tStart);
  z=profileExecute(1,saveP1,saveP2,i) ;
  times(&tEnd);
  
  if(typeIdx!=-1 && i<sizeP1)
    {
      (*cpuMonadic)[i][typeIdx][0]+=
	(1000*(tEnd.tms_utime-tStart.tms_utime))/clockTicksPerSec;
      (*cpuMonadic)[i][typeIdx][1]+=
	(1000*(tEnd.tms_stime-tStart.tms_stime))/clockTicksPerSec;
    }

  return z;
}

static void profileReport(void)
{
  int primitive, type, i;
  static char **primList=NULL;
  static char *types[]={"Int", "Float", "Char", "Nested"};

  if(primList==NULL)
    primList=get_primlist(1,0);

  printf("\n dyadic: size->%8d%8d%8d%8d%8d%8d%8d%8d+\n", 
	 1,10,100,1000,10000,100000,1000000,1000000);
  for(primitive=0; primitive<sizeP2; primitive++)
    if(1) 	/* dyadic[primitive]) */
      {
	for(type=0; type<NUM_TYPES; type++)
	  if( (*dyadic)[primitive][type][0] )
	    {
	      printf("%4s%10s ",primList[primitive],types[type]);
	      for(i=1; i<NUM_BUCKETS; i++)
		printf("%8ld",(*dyadic)[primitive][type][i]);
	      printf("\n");
	    }
      }
  
  printf("\nmonadic: size->%8d%8d%8d%8d%8d%8d%8d%8d+\n", 
	 1,10,100,1000,10000,100000,1000000,1000000);
  for(primitive=0; primitive<sizeP1; primitive++)
    if(1)			/* (monadic[primitive]) */
      {
	for(type=0; type<NUM_TYPES; type++)
	  if(  (*monadic)[primitive][type][0] )
	    {
	      printf("%4s%10s ",primList[primitive],types[type]);
	      for(i=1; i<NUM_BUCKETS; i++)
		printf("%8ld",(*monadic)[primitive][type][i]);
	      printf("\n");
	    }
      }

  printf("\nCPU  dyadic:        User  System   Total\n");
  for(primitive=0; primitive<sizeP2; primitive++)
    if(1) 	/* dyadic[primitive]) */
      {
	for(type=0; type<NUM_TYPES; type++)
	  if( (*dyadic)[primitive][type][0] )
	    {
	      long totalCPU=0;
	      printf("%4s%10s ",primList[primitive],types[type]);
	      for(i=0; i<NUM_CPU_BUCKETS; i++)
		{
		  totalCPU+=(*cpuDyadic)[primitive][type][i];
		  printf("%8ld",(*cpuDyadic)[primitive][type][i]);
		}
	      printf("%8ld",totalCPU);
	      printf("\n");
	    }
      }
  
  printf("\nCPU monadic:        User  System   Total\n");
  for(primitive=0; primitive<sizeP1; primitive++)
    if(1)			/* (monadic[primitive]) */
      {
	for(type=0; type<NUM_TYPES; type++)
	  if(  (*monadic)[primitive][type][0] )
	    {
	      long totalCPU=0;
	      printf("%4s%10s ",primList[primitive],types[type]);
	      for(i=0; i<NUM_CPU_BUCKETS; i++)
		{
		  totalCPU+=(*cpuMonadic)[primitive][type][i];
		  printf("%8ld",(*cpuMonadic)[primitive][type][i]);
		}
	      printf("%8ld",totalCPU);
	      printf("\n");
	    }
      }
}

A ep_profile(A aObj)
{
  int i;
  static char doInit=TRUE;
  S sym;
  static S on=NULL, off=NULL, data=NULL, report=NULL, reset=NULL;
  static S sMonadic, sDyadic, sCpuMonadic, sCpuDyadic;

  if(aObj->n != 1) 
    return q=ERR_LENGTH,(A)0;
  
  if( !QA(aObj) && aObj->t!=Et && !QS(aObj->p[0]) )
    return q=ERR_LENGTH,(A)0;

  if(doInit==TRUE)	/* static initialization */
    {
      clockTicksPerSec=sysconf(_SC_CLK_TCK);
      doInit       =FALSE;
      sMonadic     =(S)MS(si("monadic"));  
      sDyadic      =(S)MS(si("dyadic"));
      sCpuMonadic  =(S)MS(si("cpuMonadic"));
      sCpuDyadic   =(S)MS(si("cpuDyadic"));
      on           =(S)MS(si("on"));
      off          =(S)MS(si("off"));
      data         =(S)MS(si("data"));
      report       =(S)MS(si("report"));
      reset        =(S)MS(si("reset"));
      sizeP1       =sizeOfPrimArray(1);
      sizeP2       =sizeOfPrimArray(2);
    }

  sym=(S)aObj->p[0];
  if(sym==on && saveP1==NULL )
    {
      if(NULL==(saveP1=( PFI * )malloc(sizeP1*sizeof(PFI))))
	return (A)gi(1);
      if(NULL==(saveP2=( PFI * )malloc(sizeP2*sizeof(PFI))))
	return free(saveP1), (A)gi(1);
      
      memcpy(saveP1,P1,sizeP1*sizeof(PFI));
      memcpy(saveP2,P2,sizeP2*sizeof(PFI));

      for(i=0; i<sizeP1; i++)
	if( P1[i]!=P1[11])
	  P1[i]=profileMonadic;

      for(i=0; i<sizeP2; i++)
	if( P2[i]!=P2[40])
	  P2[i]=profileDyadic;

      monadic=(I (*)[][NUM_TYPES][NUM_BUCKETS])malloc( MONADIC_ARRAY_SIZE );
      if(NULL==monadic)
	return free(saveP1), free(saveP2), (A)gi(1);

      dyadic =(I (*)[][NUM_TYPES][NUM_BUCKETS])malloc( DYADIC_ARRAY_SIZE );
      if(NULL==dyadic)
	return free(saveP1), free(saveP2), free(monadic), (A)gi(1);

      cpuMonadic=(I (*)[][NUM_TYPES][NUM_CPU_BUCKETS])malloc( MONADIC_CPU_ARRAY_SIZE );
      if(NULL==monadic)
	return free(saveP1), free(saveP2), free(monadic), free(dyadic),(A)gi(1);

      cpuDyadic =(I (*)[][NUM_TYPES][NUM_CPU_BUCKETS])malloc( DYADIC_CPU_ARRAY_SIZE );
      if(NULL==dyadic)
	return free(saveP1), free(saveP2), free(monadic), free(dyadic), free(cpuMonadic),(A)gi(1);

      memset( (*monadic),0,MONADIC_ARRAY_SIZE );
      memset( (*dyadic), 0,DYADIC_ARRAY_SIZE );
      memset( (*cpuMonadic),0,MONADIC_CPU_ARRAY_SIZE );
      memset( (*cpuDyadic), 0,DYADIC_CPU_ARRAY_SIZE );
    }
  else if(sym==off && saveP1!=NULL)
    {
      memcpy(P1,saveP1,sizeP1*sizeof(PFI));
      memcpy(P2,saveP2,sizeP2*sizeof(PFI));
      free(saveP1);
      free(saveP2);
      saveP1=saveP2=NULL;
      free(monadic);
      free(dyadic);
      free(cpuMonadic);
      free(cpuDyadic);
      monadic=dyadic=NULL;
      cpuMonadic=cpuDyadic=NULL;
    }
  else if(sym==data && saveP1!=NULL)
    {
/*       I d1[NUM_BUCKETS],d2[NUM_BUCKETS]; */

/*       d1[0]=sizeP1; */
/*       d2[0]=sizeP2; */
/*       d1[1]=d2[1]=NUM_TYPES; */
/*       d1[2]=d2[2]=NUM_BUCKETS; */

/*       return (A)gvi(Et,2,sMonadic,sDyadic,sCpuMonadic,sCpuDyadic */
/* 		    gc(It,3,(d1[0]*d1[1]*d1[2]),d1,(*monadic)), */
/* 		    gc(It,3,(d2[0]*d2[1]*d2[2]),d2,(*dyadic))  ); */
      return aplus_nl;
    }
  else if(sym==report && saveP1!=NULL)
    {
      profileReport();
    }
  else if(sym==reset && saveP1!=NULL)
    {
      memset( (*monadic),0,MONADIC_ARRAY_SIZE );
      memset( (*dyadic), 0,DYADIC_ARRAY_SIZE );
    }
  return (A)gi(0);
}

void profileInstall()
{ install(ep_profile,"_profile",A_,1,A_,0,0,0,0,0,0,0);  return; }






