///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSCallbackBehavior.H>  
#include <MSTypes/MSUnsignedLongVector.H>

class CallbackNode
{
public:
  MSCallback *_pCallback;
  MSSymbol    _name;
  void       *_pIdentifier;

  CallbackNode(const MSSymbol& name_,MSCallback *pCallback_);
  CallbackNode(const MSSymbol& name_,MSCallback *pCallback_,void *pIdentifier_);
  ~CallbackNode(void);
    
  void callback(MSCallback *pCallback_);
};

class CallbackVector
{
public:
  enum State {Normal=0,Processing=1,Dirty=2};

  CallbackVector(void);
  ~CallbackVector(void);    

  CallbackNode *node(unsigned i) {return (CallbackNode *)_callbackVector(i);}  
  unsigned length(void) const {return _callbackVector.length();}

  void markForRemoval(unsigned i);
  void removeMarkedElements(void);
  CallbackVector& append(CallbackNode *node_);

  void state(State state_) {_state=state_;}

  MSUnsignedLongVector _callbackVector;
  State                _state;
};

 
MSCallbackBehavior::MSCallbackBehavior(void) :
_pCallbackVector(0)
{}

MSCallbackBehavior::~MSCallbackBehavior(void)
{ deleteCallbackList();}

// add a callback for the given event, replacing the current
// callback. the identifier will be equal to the callback pointer
// and thus will allow it to be uniquely identified.
void MSCallbackBehavior::callback(const MSSymbol& name_,MSCallback *pCallback_)
{
  CallbackNode *node;
  if ((node=callbackNode(name_))!=0) node->callback(pCallback_);
  else
   {
     if (_pCallbackVector==0) _pCallbackVector=new CallbackVector;
     node=new CallbackNode(name_,pCallback_);
     _pCallbackVector->append(node);
   }
}

// add a callback to the end of the callback list
void MSCallbackBehavior::addCallback(const MSSymbol& name_,MSCallback *pCallback_,void *pIdentifier_)
{
  if (_pCallbackVector==0) _pCallbackVector=new CallbackVector;
  CallbackNode *node=new CallbackNode(name_,pCallback_,pIdentifier_);
  _pCallbackVector->append(node);
}

// remove the callback with the given name and identifier
void MSCallbackBehavior::removeCallback(const MSSymbol& name_,void *pIdentifier_)
{
  if (_pCallbackVector!=0)
   {
     unsigned n=_pCallbackVector->length();
     for (unsigned i=0;i<n;i++)
      {
	CallbackNode *node=_pCallbackVector->node(i);
	if (node!=0&&node->_name==name_&&node->_pIdentifier==pIdentifier_)
	 {
	   delete node;
	   _pCallbackVector->markForRemoval(i);
	   break;
	 }
      }
   }
}

void MSCallbackBehavior::removeCallback(const MSSymbol& name_)
{
  if (_pCallbackVector!=0)
   {
     unsigned n=_pCallbackVector->length();
     for (unsigned i=0;i<n;i++)
      {
	CallbackNode *node=_pCallbackVector->node(i);
	if (node!=0&&node->_name==name_)
	 {
	   delete node;
	   _pCallbackVector->markForRemoval(i);
	 }
      }
   }
}

MSBoolean MSCallbackBehavior::activateCallback(const MSSymbol& name_)
{
  if (_pCallbackVector!=0)
   {
     unsigned n=_pCallbackVector->length();
     MSBoolean rValue=MSFalse;
     CallbackVector::State previousState=_pCallbackVector->_state;
     if (_pCallbackVector->_state==CallbackVector::Normal)
      {
	_pCallbackVector->state(CallbackVector::Processing);
      }
     for (unsigned i=0;i<n;i++)
      {
	CallbackNode *node=_pCallbackVector->node(i);
	if (node!=0&&node->_name==name_)
	 {
	   MSCallback *pCallback=node->_pCallback;
	   if (pCallback!=0) doCallback(pCallback);
	   rValue=MSTrue;
	 }
      }
     CallbackVector::State afterState=_pCallbackVector->_state;
     if (previousState==CallbackVector::Normal) // first entry
      {
	if (afterState==CallbackVector::Dirty) _pCallbackVector->removeMarkedElements();
	_pCallbackVector->state(previousState);
      }
     else if (previousState==CallbackVector::Processing) // reentrant
      {
	if (afterState!=CallbackVector::Dirty) _pCallbackVector->state(previousState);
      }
     return rValue;
   }
  return MSFalse;
}

MSCallback *MSCallbackBehavior::callback(const MSSymbol& name_)
{
  CallbackNode *node=callbackNode(name_);
  return (node!=0)?node->_pCallback:0;
}

CallbackNode *MSCallbackBehavior::callbackNode(const MSSymbol& name_)
{
  if (_pCallbackVector!=0)
   {
     unsigned n=_pCallbackVector->length();
     for (unsigned i=0;i<n;i++)
      {
	CallbackNode *node=_pCallbackVector->node(i);
	if (node!=0&&node->_name==name_&&node->_pIdentifier==(void *)node)
	 {
	   return node;
	 }
      }
   }
  return 0;
}

void MSCallbackBehavior::removeAllCallbacks(void)
{
  if (_pCallbackVector!=0)
   {
     unsigned n=_pCallbackVector->length();
     for (unsigned i=0;i<n;i++)
      {
	CallbackNode *node=_pCallbackVector->node(i);
	if (node!=0)
         {
           delete node;
	   _pCallbackVector->markForRemoval(i);
         }
      }
   }
}

void MSCallbackBehavior::deleteCallbackList(void)
{
  if (_pCallbackVector!=0)
   {
     removeAllCallbacks();
     delete _pCallbackVector;
     _pCallbackVector=0;
   }
}

MSBoolean MSCallbackBehavior::hasCallback(const MSSymbol& name_) const
{
  if (_pCallbackVector!=0)
   {
     unsigned n=_pCallbackVector->length();
     for (unsigned i=0;i<n;i++)
      {
	CallbackNode *node=_pCallbackVector->node(i);
	if (node!=0&&node->_name==name_) return MSTrue;
      }
   }
  return MSFalse;
}

MSBoolean MSCallbackBehavior::hasCallback(const MSSymbol& name_, void *pIdentifier_) const
{
  if (_pCallbackVector!=0)
   {
     unsigned n=_pCallbackVector->length();
     for (unsigned i=0;i<n;i++)
      {
	CallbackNode *node=_pCallbackVector->node(i);
	if (node!=0&&node->_name==name_&&node->_pIdentifier==pIdentifier_) return MSTrue;
      }
   }
  return MSFalse;
}

void MSCallbackBehavior::doCallback(MSCallback *callback_)
{
  callback_->process();
}

// #########################################################
// CallbackVector
// #########################################################
CallbackVector::CallbackVector(void) :
_state(Normal)
{}

CallbackVector::~CallbackVector(void)
{}

void CallbackVector::markForRemoval(unsigned i)
{
  _callbackVector.replaceAt(i,0);
  _state=Dirty;
}

void CallbackVector::removeMarkedElements(void)
{
  _callbackVector.remove(_callbackVector.binaryCompare(0,MSEqualTo));
  _state=Normal;
}

CallbackVector& CallbackVector::append(CallbackNode *node_)
{ _callbackVector.append((unsigned long)node_); return *this; }

// #########################################################
// CallbackNode
// #########################################################
CallbackNode::CallbackNode(const MSSymbol& name_,MSCallback *pCallback_) :
_name(name_),
_pCallback(pCallback_)
{ _pIdentifier=(void *)this; }

CallbackNode::CallbackNode(const MSSymbol& name_,MSCallback *pCallback_,void *pIdentifier_) :
_name(name_),
_pCallback(pCallback_),
_pIdentifier(pIdentifier_)
{}

CallbackNode::~CallbackNode(void)
{
  delete _pCallback;
  _pCallback=0;
}

void CallbackNode::callback(MSCallback *pCallback_)
{
  if (_pCallback!=pCallback_)
   {
     delete _pCallback;
     _pCallback=pCallback_;
   }
}

