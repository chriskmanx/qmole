#ifndef __AipcHostPortProtocol_H
#define __AipcHostPortProtocol_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <a/k.h>
#include <a/fncdcls.h>
#include <MSIPC/MSHostPort.H>

class AipcHostPortProtocol
{
 private:
  MSHostPort _hostport;
  MSString _service;
  MSString _protocol;
  
 public:
  
  // Constructors

  AipcHostPortProtocol(C *host_,I port_,C *protocol_=(C*)0)
    : _hostport(host_,port_) {_protocol=protocol_;_service=0;}

  AipcHostPortProtocol(C *service_);

  // Accessors and Mutators
  const C *protocol(void) {return _protocol.string();}
  void protocol(C *new_) { _protocol=new_; }
  const C *service(void) {return _service.string();}
  void service(C *new_) { _service=new_; }
  const C *host(void) {return _hostport.host().string();}
  int port(void) {return _hostport.port();}
  void setHostPort(C *host_,I port_){_hostport.set(host_,port_);}

  // Other methods

  MSBoolean lookupService(void);

};


#endif
