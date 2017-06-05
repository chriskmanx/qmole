///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// pRaw_Connection
//
// This class is subclassed from pA_Connection.  It contains the
// code for the read and send functions using the `raw protocol.
// 
// The `A protocol is to send messages with two parts: the first part
// is a four-byte integer of the length of the second part, which is
// athe result of running sys.exp on an A-object.
// 

#include <errno.h>
#include <unistd.h>
#include <pRaw_Connection.H>

#if defined(_AIX) || defined(linux) || defined(__alpha)
extern "C" {
#include <sys/ioctl.h>
#include <sys/types.h>
}
#else
extern "C" {
#include <sys/ioctl.h>
#include <sys/filio.h>
}
#endif

A pRaw_Connection::readBurst(void)
{
  ipcWarn(wrnlvl(),"%t pRaw_Connection::readBurst\n");
  A d=readOne();
  R d?gvi(Et,1,d):(A)0;
}


A pRaw_Connection::readOne(void)
{
  ipcWarn(wrnlvl(),"%t pRaw_Connection::readOne\n");
  MSBuffer bbuff;
  A z;
  I slen=0,slen1,rlen;
  if(-1==ioctl(fd(),FIONREAD,&slen)) {
//     ipcWarn(wrnlvl(),"%t ioctl FIONREAD failed: %s\n", 
// 	  (errno<sys_nerr)?sys_errlist[errno]:"unknown error");
    ipcWarn(wrnlvl(),"%t ioctl FIONREAD failed: %d\n",errno); 
    resetWithError("ioctl");
    R(A)0;
  }
  if(0==slen)
  {
    Warn("\343 IPC warning: pRaw::readOne: read event with no data [%d]\n",
      handle());
    resetWithError("ioctl");
    R(A)0;
  }

  /* create buff to hold it.  Fill buffer */
  slen1=slen?slen:4;
  z=gv(Ct,slen1);
  bbuff.minofbuffer((C*)z->p);
  bbuff.maxofbuffer(bbuff.minofbuffer()+slen1);
  bbuff.reset();

  if(0>(rlen=readTheBuffer(&bbuff,slen))) {dc(z);z=(A)0;}
  else if(0>=bbuff.put()-bbuff.get()) {dc(z);z=(A)0;}
  else {z->n=z->d[0]=rlen;((C*)z->p)[rlen]='\0';}
  turnInReadOff();
  bbuff.minofbuffer(0);
  bbuff.maxofbuffer(0);
  bbuff.reset();
  R z;
}

int pRaw_Connection::send(const A &msg_)
{
  ipcWarn(wrnlvl(),"%t pRaw_Connection::send\n");
  if(isInReset()||readChannel()==0) return -1;
  if(Ct!=msg_->t||1!=msg_->r) R -1;
  
  MSBuffer *bp=new MSBuffer(msg_->n);
  bp->stuff((C*)msg_->p,msg_->n);
  sendTheBuffer(bp);
  if (MSFalse==isWritePause()) writeChannel()->enable();
  return doWrite(MSFalse);
}

