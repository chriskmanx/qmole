#ifndef __pSimple_Listener_H
#define __pSimple_Listener_H

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

#include <pA_Listener.H>

#include <AipcHostPortProtocol.H>

class pSimple_Listener : public pA_Listener
{
 protected:
 public:

  // pSimple_Listener (Service Control Block) Constructors
  pSimple_Listener(C *name_,I port_,A cbfunc_)
    : pA_Listener(name_,port_,cbfunc_){}
  pSimple_Listener(C *name_, A cbfunc_)
    : pA_Listener(name_,cbfunc_){}
  pSimple_Listener(AipcHostPortProtocol &ahpp_, A cbfunc_)
    : pA_Listener(ahpp_,cbfunc_){}

  // M:Callbacks
  virtual void acceptNotify(int,const struct sockaddr *,int,
			    const struct sockaddr *,int);
  virtual const C *protocol(void){ return "simple";}
};

#endif
