///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSNodeList.H>
#include <MSIPC/MSTv.H>
#include <MSIPC/MSTimer.H>

MSNodeItem *MSTimer::_pTimerList=0;

MSTimer::MSTimer(TType type_,unsigned long interval_,MSCallback *pCallback_)
{
  time_t sec=interval_/1000;
  long usec=(interval_%1000)*1000;   
  init(type_,sec,usec,pCallback_);
}

MSTimer::MSTimer(TType type_,time_t sec_,long usec_,MSCallback *pCallback_)
{ init(type_,sec_,usec_,pCallback_); }

void MSTimer::init(TType type_,time_t sec_,long usec_,MSCallback *pCallback_)
{
  if (_pTimerList==0) _pTimerList=new MSNodeItem;
  
  struct timeval *tvp=expire();
  MSNodeItem *hp=_pTimerList;
  MSNodeItem *np;
  
  _pNode=new MSNodeItem((void *)this);
  _type=type_;  
  _pCallback=pCallback_;

  if (type()==Absolute)
   {
     tvp->tv_sec=(long)sec_;
     tvp->tv_usec=usec_;
     (void)tvnorm(tvp);
   }
  else
   {  
     interval()->tv_sec=(long)sec_;
     interval()->tv_usec=usec_;
     (void) tvnorm(interval());
     (void) tvsum(tod(),interval(),tvp);
   }

  for (np=hp->prev();np!=hp;np=np->prev())
   {
     MSTimer *pTimer=(MSTimer *)np->data();
     if (tvcmp(tvp,pTimer->expire())>=0) break;
   }
  _pNode->insert(np->next());
}

MSTimer::~MSTimer(void) 
{ 
  if (_pCallback!=0) delete _pCallback;  
  delete _pNode; 
}

void MSTimer::callback(MSCallback *pCallback_)
{
  if (_pCallback!=pCallback_)
   {
     if (_pCallback!=0) delete _pCallback;
     _pCallback=pCallback_;
   }
}

void MSTimer::process(void)
{ if (_pCallback!=0) _pCallback->process(); }

void MSTimer::stop(void)
{
  if (type()!=Interval) 
   {
     delete this;
     return;     
   }
  _pNode->remove();   
}

void MSTimer::reset(void)
{
  if (type()!=Interval) 
   {
     delete this;
     return;     
   }
  MSNodeItem *hp=_pTimerList;
  struct timeval *tvp=expire();
  MSNodeItem *np;

  (void) tvsum(tod(),interval(),tvp);

  _pNode->remove();
  for (np=hp->prev();np!=hp;np=np->prev())
   {
     MSTimer *pTimer=(MSTimer *) np->data();
     if (tvcmp(tvp,pTimer->expire())>=0) break;
   }
  _pNode->insert(np->next());
}

struct timeval *MSTimer::nextTimeVal(void)
{
  if (MSTimer::_pTimerList!=0)
   {
     MSTimer *pTimer;
     if ((pTimer=(MSTimer *)MSTimer::_pTimerList->next()->data())==(MSTimer *)(0))
      {
        return ((struct timeval *)(0));
      }
     return (pTimer->expire());
   }
  return ((struct timeval *)(0));
}

MSBoolean MSTimer::processTimers(void)
{
  MSBoolean didwork=MSFalse;
  if (_pTimerList!=0)
   {
     MSNodeItem *hp=_pTimerList;
     MSNodeItem *np;
     struct timeval now=*tod();
     
     while ((np=hp->next())!=hp)
      {
        MSTimer *pTimer=(MSTimer *) np->data();
        struct timeval *tvp=pTimer->expire();
        
        if (tvcmp(&now,tvp)>=0)
         {
           // cache this before calling process(), so that we do not have to access
           // the pTimer object after it is called. this allows a user to safely
           // delete an interval timer from within a process callback.
           MSBoolean deleteTimerAfterProcess=(pTimer->type()!=Interval)?MSTrue:MSFalse;
           
           if (pTimer->type()==Interval) pTimer->reset();
           else pTimer->_pNode->remove();
           pTimer->process();
           if (deleteTimerAfterProcess==MSTrue) delete pTimer;
         }
        else break;
        didwork=MSTrue;
      }
   }
  return didwork;
}











