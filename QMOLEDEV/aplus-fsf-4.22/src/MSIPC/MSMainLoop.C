///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <errno.h>
#include <unistd.h>
#include <MSIPC/MSMainLoop.H>
#include <MSIPC/MSTv.H>
#include <MSIPC/MSTimer.H>
#include <MSIPC/MSChannel.H>
#include <MSTypes/MSMessageLog.H>

MSBoolean MSMainLoop::_zeroTimeOut=MSFalse;
MSBoolean MSMainLoop::_continueLoop=MSTrue;

// ctl support added by Kenny Chien
#ifdef MS_NEED_CTL

extern "C" int ctl_wait(int*,int*,char*,int*,int,int,int); 
extern "C" int ctl_get_timeout(void);
extern "C" char *ctl_global_init(void);

static char *_ctlBuffer=0;

// we really should include the file "ipc_const.h"
// to get the value of IPC_TCP.  But for ease of 
// compilation, we'll hard code this one

#define IPC_TCP  0x0080
#endif

MSMainLoop::MSMainLoop(void)
{
#ifdef MS_NEED_CTL
  if (_ctlBuffer==0) _ctlBuffer=ctl_global_init();
#endif
}

MSMainLoop::~MSMainLoop(void)
{}

void MSMainLoop::continueLoop(MSBoolean continueLoop_)
{ _continueLoop=continueLoop_; }  
MSBoolean MSMainLoop::continueLoop(void)
{ return _continueLoop; }

MSBoolean MSMainLoop::zeroTimeOut(void)
{ return _zeroTimeOut; }
void MSMainLoop::zeroTimeOut(MSBoolean zeroTimeOut_)
{ _zeroTimeOut=zeroTimeOut_; }  

MSBoolean MSMainLoop::processTimers(void)  
{ return MSTimer::processTimers(); }

MSBoolean MSMainLoop::flush(void)
{ return MSFalse; }

void MSMainLoop::flushAndProcess(MSBoolean blocking_)
{ innerLoop(blocking_); }

void MSMainLoop::quit(void)
{
  continueLoop(MSFalse);
}

void MSMainLoop::loop(void)
{
  outerLoop(); 
  terminateLoop();
}

void MSMainLoop::terminateLoop(void)
{}

void MSMainLoop::outerLoop(void)
{ while (_continueLoop==MSTrue) innerLoop(); }

void MSMainLoop::innerLoop(MSBoolean blocking_)  
{
  userProcessing();  
  innerLoopFlush(blocking_);
  selectAndProcess();
  userProcessing();
  processTimers();
}

void MSMainLoop::innerLoopFlush(MSBoolean blocking_)  
{
  if (blocking_==MSFalse) 
   {
     zeroTimeOut(MSTrue);
     flush();
   }
  else zeroTimeOut(flush());
}

void MSMainLoop::userProcessing(void)
{}

#ifdef MS_NEED_CTL
void MSMainLoop::selectAndProcess(void)
{
  struct timeval *tvpnext,tvp;
  int             chan,sub,blen,next,ctl_timeout;

  if (_zeroTimeOut==MSTrue) next=0;
  else
   {
     ctl_timeout=ctl_get_timeout();
     if ((tvpnext=MSTimer::nextTimeVal())==(struct timeval *)(0)) next=ctl_timeout;
     else
      {
	(void)tvdiff(tvpnext,tod(),&tvp);
	if (tvp.tv_sec<0) next=0;
	else next=tvp.tv_sec*1000000+tvp.tv_usec;
	next=(next<ctl_timeout?next:ctl_timeout);
      }
   }
  ctl_wait(&chan,&sub,_ctlBuffer,&blen,next,IPC_TCP,0);
}
#else
void MSMainLoop::selectAndProcess(void)
{
  struct timeval timeout;
  struct timeval *tvpnext;
  struct timeval *tvp;
  int rc;
  
  MSChannel::fdscopy();      // copy enable flags over able flags
  if (_zeroTimeOut==MSTrue) // set up timeout 
   {
     tvp=&timeout;
     tvp->tv_sec=tvp->tv_usec=(long)(0);
   }
  else if ((tvpnext=MSTimer::nextTimeVal())==(struct timeval *)(0))
   {
     tvp=(struct timeval *)(0);
   }
  else
   {
     tvp=&timeout;
     (void) tvdiff(tvpnext,tod(),tvp);
     if (tvp->tv_sec<0) tvp->tv_sec=tvp->tv_usec=(long)(0);
   }

  MSBoolean selectAgain;
  do
   {
     selectAgain=MSFalse;
     rc=MSChannel::select(tvp);
     if (rc==-1)
      {
        switch (errno)
         {
         case EINTR:
//           MSMessageLog::debugMessage("MSMainLoop: WARNING: select() was interrupted\n");
           break;
         case EBADF:
           MSMessageLog::warningMessage("MSMainLoop: WARNING: invalid fd removed in select()\n");
           selectAgain=MSChannel::removeBadFds();
           MSChannel::fdscopy();
           break;
         case EINVAL:
           MSMessageLog::errorMessage("MSMainLoop: ERROR: Invalid timeval in select()\n");
           break;
         default:
           MSMessageLog::errorMessage("MSMainLoop: ERROR: errno=%d from select()\n",errno);
           break;
         }	
      }
   }
  while(selectAgain==MSTrue);
  if (rc<=0) MSChannel::fdszero();
  (void)MSChannel::processChannels();
}

#endif
