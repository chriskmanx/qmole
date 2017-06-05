#ifndef __AipcService_H
#define __AipcService_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <a/k.h>
#include <a/fncdcls.h>
#include <a/arthur.h>
#include <dap/Warn.h>

#include <MSTypes/MSEnum.H>
#include <MSTypes/MSNodeList.H>

#include <IPCUtilities.H>

#define AIPC_BMASK 0xff

class AipcService 
{
 private:
  static MSNodeItem *Roster;
  static I HandleSource;

  MSNodeItem *_node;
  A _acbfunc;
  I _handle;
  MSBoolean _debug;

  // private methods
  void init(A cbfunc_);
  void addToRoster(void);
  void removeFromRoster(void);
  
 public:
  
  // AipcService (Service Control Block) Constructors
  AipcService(A cbfunc_);
  virtual ~AipcService(void);

  // Accessors and Mutators
  A acbfunc(void) { return _acbfunc;}
  void acbfunc(A _new) { dc(_acbfunc); _acbfunc=(A)ic(_new); }

  // M:Debug functions
  void turnDebugOn(void){_debug=MSTrue;}
  void turnDebugOff(void){_debug=MSFalse;}
  MSBoolean isDebug(void){return _debug;}
  int wrnlvl(void){return _debug?-1:1;}

  // M:Handle functions
  virtual I serviceType(void) {return 0;}
  virtual I handle(void) { return _handle; }
  MSBoolean ValidateHandle(I handle_);
  MSBoolean ValidateHandle(void){return ValidateHandle(handle());}
  static AipcService *lookup(I handle_);
  static AipcService *lookup(I handle_,I serviceType_);
  static A roster(void);

  // M:Other methods
  virtual void ACallback(const C *event_,A data_=aplus_nl);
  virtual void serviceErrorNotify(const C *msg_);
  virtual const C *protocol(void){ return "unknown";}
};

class AipcNodeItem
{
 public:
  AipcService *_asptr;
  I _handle;
  
  AipcNodeItem(AipcService *asptr_,I handle_) : 
    _asptr(asptr_), _handle(handle_) {}
};

#endif


