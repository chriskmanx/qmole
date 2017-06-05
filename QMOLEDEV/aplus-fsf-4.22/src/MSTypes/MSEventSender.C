///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSEventSender.H>

MSEventSender::~MSEventSender(void)
{ removeAllReceivers(); }

void MSEventSender::removeAllReceivers(void)
{
  if (_pReceiverList!=0)
   {
     List *pList=_pReceiverList;
     unsigned n=pList->_allocated;
     MSEventReceiver *pReceiver;

     // prevent removeSender from calling removeReceiver and
     // deleting the pReceiver twice - also this prevents a search
     // of the list should removeReceiver get called  
     _pReceiverList=0;

     for (unsigned i=0;i<n;i++)
      {
	pReceiver=pList->_array[i];
	if (pReceiver!=0)
	 {
	   pList->_array[i]=0;
	   pReceiver->removeSender(this);
	 }
      }
     delete pList;
     _pReceiverList=0;
   }
}

MSBoolean MSEventSender::addReceiver(MSEventReceiver *pReceiver_)
{
  if (pReceiver_!=0)
   {
     // special case for first time
     if (_pReceiverList==0)
      {
	_pReceiverList=new (1) List;
	_pReceiverList->_allocated=1;
        _pReceiverList->_array[0]=pReceiver_;
	pReceiver_->addSender(this);
//	return MSTrue;
      }
     else
       {
	 // check for open slot
	 List *pList=_pReceiverList;
	 unsigned i,n=pList->_allocated;
	 unsigned j=n;
	 for (i=0;i<n;i++) 
	   {
	     if (pList->_array[i]==pReceiver_) return MSTrue;
	     else if (j==n&&pList->_array[i]==0) j=i;
	   }
	 if (j!=n)
	   {
	     pList->_array[j]=pReceiver_;
	     pReceiver_->addSender(this);	      
//	   return MSTrue;
	   }
	 else
	   {
	     // need a new slot
	     unsigned newSize=n+1;
	     List *pNewList=new (newSize) List;
	     pNewList->_allocated=newSize;
	     
	     for (i=0;i<n;i++) pNewList->_array[i]=pList->_array[i];
	     
	     pNewList->_array[n]=pReceiver_;
	     delete pList;
	     _pReceiverList=pNewList;
	     pReceiver_->addSender(this);
//	     return MSTrue;
	   }
       }
     
     addReceiverNotify(pReceiver_);
     return MSTrue;
   }

  return MSFalse;
}

// this method must insure that the _receiverList, which is a special 
// type of Node (HeadNode) is preserved. if the receiver to be 
// removed is stored in this node, then we delete the next node
// and move its contents into the _receiverList node
MSBoolean MSEventSender::removeReceiver(MSEventReceiver *pReceiver_)
{
  if (_pReceiverList!=0&&pReceiver_!=0)
   {
     List *pList=_pReceiverList;
     unsigned n=pList->_allocated;
     for (unsigned i=0;i<n;i++)
      {
	if (pList->_array[i]==pReceiver_)
	 {
	   pList->_array[i]=0;
	   pReceiver_->removeSender(this);
	   removeReceiverNotify(pReceiver_);
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}

// virtual classes called respectively from addReceiver() (after the receiver has been added)
// and from removeReceiver() (after the receiver has been removed), to provide notification
// mechanism for subclasses
void MSEventSender::addReceiverNotify(MSEventReceiver *)    {}
void MSEventSender::removeReceiverNotify(MSEventReceiver *) {}
  
unsigned MSEventSender::numReceivers() const
{
  unsigned count=0;

  if (_pReceiverList)
    {
      List *pList=_pReceiverList;
      unsigned n=pList->_allocated;
      for (unsigned i=0;i<n;++i)
	if (pList->_array[i])
	  ++count;
    }

  return count;
}

void MSEventSender::blockEvents(void)
{ if (_pReceiverList!=0) _pReceiverList->_blockCount++; }
void MSEventSender::unblockEvents(MSBoolean sendEvent_)
{ 
  if (_pReceiverList!=0&&_pReceiverList->_blockCount>0) 
   {
     _pReceiverList->_blockCount--; 
     if (sendEvent_==MSTrue&&_pReceiverList->_blockCount==0)
      {
        MSNullEvent aEvent;
	sendEvent(aEvent);
      }
   }
}

void MSEventSender::sendEvent(MSEvent& aEvent_)
{
  if (_pReceiverList!=0)
   {
     if (_pReceiverList->_blockCount==0)
      {
	unsigned n=_pReceiverList->_allocated;
	MSEventSender *lastSender=aEvent_.sender();

	aEvent_.sender(this);
	for (unsigned i=0;i<n;i++)
	 {
	   if (_pReceiverList->_array[i]!=0) _pReceiverList->_array[i]->receiveEvent(aEvent_);
	 }
	aEvent_.sender(lastSender);
      }
   }
}


void *MSEventSender::List::operator new(size_t,unsigned realSize_)
{
  return ::new char[sizeof(unsigned)+sizeof(unsigned)+(sizeof(MSEventReceiver *)*realSize_)];
}
    

void MSEventSender::List::operator delete(void *pData_)
{
  ::delete [](char*)pData_;
}
