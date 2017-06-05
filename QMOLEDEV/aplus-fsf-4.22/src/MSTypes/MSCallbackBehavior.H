#ifndef MSCallbackBehaviorHEADER
#define MSCallbackBehaviorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSSymbol.H>
#include <MSTypes/MSCallback.H>

class CallbackVector;
class CallbackNode;

class MSTypesExport MSCallbackBehavior
{
protected:
  CallbackVector *_pCallbackVector;
  
  CallbackNode *callbackNode(const MSSymbol&);
  
  void deleteCallbackList(void);

  virtual void doCallback(MSCallback *);

public:
  MSCallbackBehavior(void);
  virtual ~MSCallbackBehavior(void);
  
  // Callback methods - old API - should use add/remove API
  MSCallback *callback(const MSSymbol& name_);
  void callback(const MSSymbol& name_,MSCallback *callback_);

  // Callback interface
  void addCallback(const MSSymbol&,MSCallback *,void *);
  void removeCallback(const MSSymbol&,void *);
  void removeCallback(const MSSymbol&);
  void removeAllCallbacks(void);

  // does a callback exist for the given symbol
  MSBoolean hasCallback(const MSSymbol&) const;
  MSBoolean hasCallback(const MSSymbol&,void *) const;

  // activate all callbacks associated with the symbol 
  MSBoolean activateCallback(const MSSymbol&);
};

#endif
