#ifndef __pRaw_Listener_H
#define __pRaw_Listener_H

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

class pRaw_Listener : public pA_Listener
{
 protected:
 public:

  // pRaw_Listener (Service Control Block) Constructors
  pRaw_Listener(C *name_,I port_,A cbfunc_)
    : pA_Listener(name_,port_,cbfunc_){}
  pRaw_Listener(C *name_, A cbfunc_)
    : pA_Listener(name_,cbfunc_){}
  pRaw_Listener(AipcHostPortProtocol &ahpp_, A cbfunc_)
    : pA_Listener(ahpp_,cbfunc_){}

  // M:Callbacks
  virtual void acceptNotify(int,const struct sockaddr *,int,
			    const struct sockaddr *,int);
  virtual const C *protocol(void){ return "raw";}
};

#endif
