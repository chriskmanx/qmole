///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSIPC/MSProtocolListener.H>

MSProtocolListener::MSProtocolListener(const char *name_,int port_) :
MSListener(name_,0,MSListener::Yes,2,2,PF_INET,SOCK_STREAM,0)
{
  hostPort().set((const char *) 0,port_);
  _name=name_;
}

MSProtocolListener::MSProtocolListener(const char *name_,const MSString &serviceName_) :
MSListener(name_,0,MSListener::Yes,2,2,PF_INET,SOCK_STREAM,0)
{
  service().establish(serviceName_);
  hostPort().set((const char *) 0,service().port());
  _name=name_;
}

MSProtocolListener::~MSProtocolListener(void)
{
  _retry=MSListener::No;
  close();
}

MSBoolean MSProtocolListener::establish(void)
{
  if (service().isValid()==MSTrue)
   {
    if (service().isReady()==MSFalse)
    {
      service().establish();
      hostPort().set((const char *) 0,service().port());
    }
   }
  _localName=(struct sockaddr *)hostPort().sockaddr_in(_localNamelen);
  return (_localName!=0)?MSTrue:MSFalse;  
}
