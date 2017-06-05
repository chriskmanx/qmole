#ifndef __pA_Connection_H
#define __pA_Connection_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <AipcConnection.H>

#include <MSIPC/MSFds.H>
#include <pA_Attributes.H>

class pA_Connection : public AipcConnection
{
 protected:
  static MSFds Syncfds;
  pA_Attributes _pA_attrs;
  virtual void doRead(void);
  virtual A getAobjFromBuffer(MSBuffer *bb);
  virtual A readBurst(void);
  virtual A readOne(void);
 public:
  
  // Constructors
    pA_Connection(C *name_,C *host_,I port_,A cbfunc_)
      :AipcConnection(name_,host_,port_,cbfunc_){}
  pA_Connection(C *serviceName_,A cbfunc_)
    :AipcConnection(serviceName_,cbfunc_){}
  pA_Connection(AipcHostPortProtocol &ahpp_,A cbfunc_)
    :AipcConnection(ahpp_,cbfunc_){}
  pA_Connection(C *name_,int fd_,A cbfunc_,
		AipcAttributes &attrs_,pA_Attributes &pA_attrs_)
    : AipcConnection(name_,fd_,cbfunc_,attrs_), _pA_attrs(pA_attrs_)
  {}

  // Flags;
  void turnBurstModeOn(void) {_pA_attrs.burstMode(MSTrue);}
  void turnBurstModeOff(void) {_pA_attrs.burstMode(MSFalse);}
  MSBoolean isBurstMode(void) {return _pA_attrs.burstMode();}

  // M:Attribute interface;
  virtual A setableAttrlist(void);
  virtual A getableAttrlist(void);
  virtual MSBoolean setAttr(C *attr_,A aobj);
  virtual A getAttr(C *attr_);

  // Other Methods;
  virtual int send(const A &);

  // M:Syncronous Send and Read;
 protected:
  I syncDoWrite(void);
  I syncWriteLoop(struct timeval *pgameover);
  I syncDoRead(A *paobj);
  A syncReadLoop(struct timeval *pgameover);
 public:
  virtual A syncSendCover(A msg_, A timeout_);
  virtual A syncReadCover(A timeout_);
  virtual const C *protocol(void){ return "A";}
};

#endif
