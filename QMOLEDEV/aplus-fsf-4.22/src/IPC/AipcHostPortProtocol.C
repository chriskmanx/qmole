///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// AipcHostPortProtocol
//
// This class has no superclass.  It contains an MSHostPort, plus
// additional fields for protocol and service name, both strings.
// 
// The primary purpose of this class is to support retrieval of
// host/port/protocol from a named remprog service.
// 

#include <AipcHostPortProtocol.H>

AipcHostPortProtocol::AipcHostPortProtocol(C *service_) : _hostport("",-1)
{
  _service=service_;
  lookupService();
}

extern "C" int GetService(const char *name);
extern "C" int NextService(char **host, long *program, char **protocol);

MSBoolean AipcHostPortProtocol::lookupService(void)
{
  if (_service==MSString( (C *)0 )) return (-1==port())?MSFalse:MSTrue;
  const C *name=service();
  C *host;
  I port;
  C *pcl;
  if (0 >= GetService(name)) {return MSFalse;}
  if (0 > NextService(&host,&port,&pcl)) {return MSFalse;}
  setHostPort(host,port);
  protocol(pcl);
  return MSTrue;
}
