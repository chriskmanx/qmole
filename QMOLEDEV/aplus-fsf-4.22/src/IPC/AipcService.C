///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// AipcService
//
// This class contains code common to all AIPC connections and listeners.
// It stores the callback function, and manages the Roster and handles.

#include <AipcService.H>

// Static members

MSNodeItem *AipcService::Roster=new MSNodeItem;
I AipcService::HandleSource=3210000;

AipcService::AipcService(A cbfunc_) : _debug(MSFalse)
{
  ipcWarn(0,"%t AipcService::AipcService\n");
  init(cbfunc_);
}

AipcService::~AipcService(void)
{
  ipcWarn(wrnlvl(),"%t AipcService::~AipcService\n");
  removeFromRoster();
  dc(_acbfunc);
  _acbfunc=(A)0;
}

void AipcService::init(A cbfunc_)
{
  ipcWarn(0,"%t AipcService::init\n");
  _handle=++HandleSource;
  _acbfunc=(A)ic(cbfunc_);
  addToRoster();
}

void AipcService::addToRoster(void)
{
  ipcWarn(0,"%t AipcService::addToRoster\n");
  _node=new MSNodeItem();
  _node->data(new AipcNodeItem(this,handle()));
  AipcService::Roster->insert(_node);
}  

void AipcService::removeFromRoster(void)
{
  _node->remove();
  // delete (MSNodeItem *)_node->data();
  // delete _node->data();
  delete _node;
  _node=0;
}

// M:Handle functions

MSBoolean AipcService::ValidateHandle(I handle_)
{
  ipcWarn(wrnlvl(),"%t AipcService::ValidateHandle\n");
  for(MSNodeItem *hp=Roster,*np=hp;hp!=(np=np->next());)
  {
    AipcNodeItem *npd=(AipcNodeItem *)np->data();
    if(handle_==npd->_handle)
      return ((AIPC_BMASK&serviceType())==npd->_asptr->serviceType()) ? 
	MSTrue : MSFalse;
  }
  return MSFalse;
}

  
AipcService *AipcService::lookup(I handle_)
{
  ipcWarn(0,"%t AipcService::lookup %d\n",handle_);
  for(MSNodeItem *hp=Roster,*np=hp;hp!=(np=np->next());)
  {
    AipcNodeItem *npd=(AipcNodeItem *)np->data();
    if(handle_==npd->_handle) return npd->_asptr;
  }
  return 0;
}

AipcService *AipcService::lookup(I handle_,I serviceType_)
{
  AipcService *asptr=lookup(handle_);
  return(asptr&&(AIPC_BMASK&asptr->serviceType())==serviceType_)?asptr:0;
}

A AipcService::roster(void)
{
  ipcWarn(0,"%t AipcService::roster\n");
  int n;
  MSNodeItem *hp=Roster,*np;
  for(np=hp,n=0;hp!=(np=np->next());) n++;
  A z=gv(It,n);
  for(np=hp,n=0;hp!=(np=np->next());) 
    z->p[n++]=((AipcNodeItem *)(np->data()))->_handle;
  return z;
}


// M:Other methods

void AipcService::ACallback(const C *event_,A data_)
{
  ipcWarn(wrnlvl(),"%t AipcService::ACallback %s\n",event_);
  E e=(E)ma(5);
  A ahandle=gi(handle());
  A aevent=gsym(event_);
  e->n=3; 
  e->f=(I)acbfunc(); 
  e->a[0]=(I)ahandle;
  e->a[1]=(I)aevent;
  e->a[2]=(I)data_;
  dc((A)ez(ME(e)));
  dc(aevent);
  dc(ahandle);
  mf((I *)e);
}

void AipcService::serviceErrorNotify(const C *msg_)
{
  A d=gsym(msg_);
  ACallback("error",d);
  dc(d);
}
