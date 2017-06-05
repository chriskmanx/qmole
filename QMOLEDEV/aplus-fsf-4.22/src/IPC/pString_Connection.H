#ifndef __pString_Connection_H
#define __pString_Connection_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <pA_Connection.H>

class pString_Connection : public pA_Connection
{
 protected:
  virtual A getAobjFromBuffer(MSBuffer *bb);
  virtual A readBurst(void);
  virtual A readOne(void);
 public:
  
  // Constructors
  pString_Connection(C *name_,C *host_,I port_,A cbfunc_)
    :pA_Connection(name_,host_,port_,cbfunc_){}
  pString_Connection(C *serviceName_,A cbfunc_)
    :pA_Connection(serviceName_,cbfunc_){}
  pString_Connection(AipcHostPortProtocol &ahpp_,A cbfunc_)
    :pA_Connection(ahpp_,cbfunc_){}
  pString_Connection(C *name_,int fd_,A cbfunc_,
		  AipcAttributes &attrs_,pA_Attributes &pA_attrs_)
    : pA_Connection(name_,fd_,cbfunc_,attrs_,pA_attrs_) {}

  // Flags

  // M:Attribute interface

  // Other Methods
  virtual int send(const A &msg_);
  virtual const C *protocol(void){ return "string";}

  // M:Syncronous Send and Read;
 protected:
  I syncDoWrite(void);
  I syncWriteLoop(struct timeval *pgameover);
  I syncDoRead(A *paobj);
  A syncReadLoop(struct timeval *pgameover);

  // Other Methods
 public:
  virtual A syncSendCover(A msg_, A timeout_);
  virtual A syncReadCover(A timeout_);

};

#endif

