///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSEventReceiver.H>
#include <MSTypes/MSEventSender.H>

MSEventReceiver::~MSEventReceiver(void)
{ 
  if (senderList()!=0)
   {
     Node          *np=senderList();
     Node          *tp;
     MSEventSender *sndr;
     _senderList=0; // prevent removeReceiver from calling removeSender and
                    // deleting the node twice - also this prevents a search
                    // of the list should removeSender get called  
     while (np!=0)
      {
        tp=np->next();
        sndr=np->sender();
        delete np;
	sndr->removeReceiver(this);
	removeSenderNotify(sndr);
        np=tp;
      }
   }
}

void MSEventReceiver::receiveEvent(MSEvent&) {}

// this allows subclasses to get notified of the add or remove
void MSEventReceiver::addSenderNotify(MSEventSender *) {}
void MSEventReceiver::removeSenderNotify(MSEventSender *) {}

MSBoolean MSEventReceiver::addSender(MSEventSender *pSender_)
{
  if (pSender_!=0)
   {
     if (senderList()!=0)
      {
	Node *next=0;
	Node *np=senderList();
	for (;np!=0;np=np->next()) 
         {
           if (np->sender()==pSender_) return MSFalse;
           else next=np;
         }
        next->next(new Node(pSender_));
      }
     else _senderList=new Node(pSender_);
     pSender_->addReceiver(this);
     addSenderNotify(pSender_);
     return MSTrue;
   }
  return MSFalse;
}

MSBoolean MSEventReceiver::removeSender(MSEventSender *pSender_)
{
  if (pSender_!=0)
   {
     Node *np=senderList();
     Node *pp=np;
     for (;np!=0;pp=np,np=np->next()) 
      {
	if (np->sender()==pSender_) 
         {
           pp->next(np->next());
           if (np==senderList()) _senderList=pp->next();
	   delete np;
           pSender_->removeReceiver(this);
           removeSenderNotify(pSender_);
	   return MSTrue;
	 }
      }
   }
  return MSFalse;
}






