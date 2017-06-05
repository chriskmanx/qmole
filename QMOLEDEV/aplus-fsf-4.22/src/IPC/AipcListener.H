#ifndef __AipcListener_H
#define __AipcListener_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <a/k.h>
#include <a/fncdcls.h>
#include <dap/Warn.h>

#if defined(_AIX)
#include <sys/select.h>
#endif

#include <MSIPC/MSProtocolListener.H>
#include <AipcService.H>

#include <AipcAttributes.H>
#include <AipcHostPortProtocol.H>

class AipcListener : public MSProtocolListener, public AipcService
{
 public:
  static I ServiceType;
 protected:

  AipcAttributes _attrs;
  MSBoolean _zeroPort;

  // private methods
  void init(void);
  
 public:
  
  // AipcListener (Service Control Block) Constructors
  AipcListener(C *name_,I port_,A cbfunc_);
  AipcListener(C *name_, A cbfunc_);
  AipcListener(AipcHostPortProtocol &ahpp_, A cbfunc_);
  virtual ~AipcListener(void);

  // M:Attribute interface
 public:
  virtual A setableAttrlist(void);
  virtual A getableAttrlist(void);
  virtual MSBoolean setAttr(C *attr_,A aobj);
  virtual A getAttr(C *attr_);

  // M:Callbacks
  virtual void acceptNotify(int,const struct sockaddr *,int,
			    const struct sockaddr *,int);

  // M:Other methods
  C*name(void) {return (C*)MSProtocolListener::name().string();}
  virtual I serviceType(void) {return AipcListener::ServiceType;}
  MSBoolean isOpen(void) {return(-1==fd())?MSFalse:MSTrue;}
  int getListenPort(void);
  void open(void);
  void close(void);
};


#endif
