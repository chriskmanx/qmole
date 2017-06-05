#ifndef MSHostPortHEADER
#define MSHostPortHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSString.H>

class MSIPCExport MSHostPort
{
protected:
  MSString _host;
  int      _port;

public:
  MSHostPort(void);
  MSHostPort(struct sockaddr_in *);
  MSHostPort(const char *host_,int port_);
  ~MSHostPort(void) {}

  void set(const char *host_,int port_);

  const MSString& host(void) const {return _host;}
  int port(void)             const {return _port;}

  struct sockaddr_in *sockaddr_in(int&);  
};

#endif
