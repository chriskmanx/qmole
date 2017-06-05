///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <sys/types.h>
#include <netinet/in.h>
#include <a/k.h>
#include <a/fncdcls.h>
#include <cxsys/impexp.h>
#include <string.h>
#include <MSIPC/MSBuffer.H>

#include <arpa/inet.h>

/* bufftobuff retrieves up to n characters from buffer sp, and puts them
   into buffer dp.  Returns number of characters actually gotten.
 */
int bufftobuff(MSBuffer *sp,MSBuffer *dp,int n)
{
  if(sp&&dp) 
  {
    if(n>sp->put()-sp->get()) n=sp->put()-sp->get();
    dp->stuff(sp->get(),n);
    sp->get(sp->get()+n);
    return n;
  }
  return -1;
}

MSBuffer *createBufferFromAobj(const A &aobj)
{
  I headsize,datasize;
  int rc;
  rc=ExportAObjectSizePass(aobj,(char *)0,1,&headsize,&datasize);
  if(0!=rc) return NULL;
  long objsize=headsize+datasize;
  int nobjsize = htonl(objsize);;
  MSBuffer *bp=new MSBuffer(objsize+sizeof(long));
  bp->stuff((char *)(&nobjsize),sizeof(nobjsize));
  ExportAObjectFillPass(aobj,(char *)0,1,headsize,bp->put());
  bp->put(bp->put()+objsize);
  return(bp);
}

