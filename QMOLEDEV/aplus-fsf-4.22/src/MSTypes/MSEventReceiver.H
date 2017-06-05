#ifndef MSEventReceiverHEADER
#define MSEventReceiverHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEvent.H>

class MSEventSender;

class MSTypesExport MSEventReceiver
{
public:
  MSEventReceiver(void) : _senderList(0) {};
  MSEventReceiver(const MSEventReceiver&) : _senderList(0) {};
  virtual ~MSEventReceiver(void);

  // message for receiving MSEvents from MSEventSenders
  virtual void receiveEvent(MSEvent&);

  // protection against pointer bit wise copy 
  MSEventReceiver& operator=(const MSEventReceiver&) { return *this; }
protected:
  virtual void addSenderNotify(MSEventSender *);
  virtual void removeSenderNotify(MSEventSender *);
  virtual MSBoolean addSender(MSEventSender *);
  virtual MSBoolean removeSender(MSEventSender *);

private:
friend class MSEventSender;

class MSTypesExport Node
{
private:
  Node          *_next;
  MSEventSender *_sender;
public:
  Node(MSEventSender *sender_=0,Node *next_=0):_next(next_),_sender(sender_) {}
  ~Node(void) {}
  
  void next(Node *next_)      { _next=next_; }
  Node *next(void)            { return _next; }
  MSEventSender *sender(void) { return _sender; }
};

  Node *_senderList;
  Node *senderList(void) const { return _senderList; }
};

#endif 







