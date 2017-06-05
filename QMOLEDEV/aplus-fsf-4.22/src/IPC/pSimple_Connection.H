#ifndef __pSimple_Connection_H
#define __pSimple_Connection_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <pA_Connection.H>

class pSimple_Connection : public pA_Connection
{
 protected:
  virtual A getAobjFromBuffer(MSBuffer *bb);
  virtual A readBurst(void);
  virtual A readOne(void);
 public:
  
  // Constructors
  pSimple_Connection(C *name_,C *host_,I port_,A cbfunc_)
    :pA_Connection(name_,host_,port_,cbfunc_){}
  pSimple_Connection(C *serviceName_,A cbfunc_)
    :pA_Connection(serviceName_,cbfunc_){}
  pSimple_Connection(AipcHostPortProtocol &ahpp_,A cbfunc_)
    :pA_Connection(ahpp_,cbfunc_){}
  pSimple_Connection(C *name_,int fd_,A cbfunc_,
		  AipcAttributes &attrs_,pA_Attributes &pA_attrs_)
    : pA_Connection(name_,fd_,cbfunc_,attrs_,pA_attrs_) {}

  // Flags

  // M:Attribute interface

  // Other Methods
  virtual int send(const A &msg_);
  virtual const C *protocol(void){ return "simple";}
};

#endif

