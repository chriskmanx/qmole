#ifndef __pA_Listener_H
#define __pA_Listener_H

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

#include <AipcListener.H>

#include <pA_Attributes.H>
#include <AipcHostPortProtocol.H>

class pA_Listener : public AipcListener
{
 protected:

  pA_Attributes _pA_attrs;

  // private methods
  void init(void);
  
 public:
  
  // pA_Listener (Service Control Block) Constructors
  pA_Listener(C *name_,I port_,A cbfunc_)
    : AipcListener(name_,port_,cbfunc_){}
  pA_Listener(C *name_, A cbfunc_);
  pA_Listener(AipcHostPortProtocol &ahpp_, A cbfunc_)
    : AipcListener(ahpp_,cbfunc_){}
  
  // M:Attribute interface
 public:
  virtual A setableAttrlist(void);
  virtual A getableAttrlist(void);
  virtual MSBoolean setAttr(C *attr_,A aobj);
  virtual A getAttr(C *attr_);

  // M:Callbacks
  virtual void acceptNotify(int,const struct sockaddr *,int,
			    const struct sockaddr *,int);

  virtual const C *protocol(void){ return "A";}
};


#endif
