///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSIPC/MSIpcConnection.H>

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSIPC/MSProtocolConnection.C>

#if defined (MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSProtocolConnection<XDR>
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSProtocolConnection<XDR>)
#endif

#if defined (MS_STD_TEMPLATE_INSTANTIATION)
template class MSProtocolConnection<XDR>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSProtocolConnection<XDR>;
#endif

#endif  //MSTK_MANUAL_INSTANTIATION

MSIpcConnection::MSIpcConnection(const char *name_, const char *host_, int port_) :
MSProtocolConnection<XDR>(name_, host_, port_)
{
}

MSIpcConnection::MSIpcConnection(const char *name_, const MSString &serviceName_) :
MSProtocolConnection<XDR>(name_, serviceName_)
{
}

MSIpcConnection::MSIpcConnection(const char *name_, int fd_) :
MSProtocolConnection<XDR>(name_, fd_)
{
}

MSIpcConnection::~MSIpcConnection(void) {}

void MSIpcConnection::doRead(void)
{
}

int MSIpcConnection::send(const XDR &)
{
  return 0;
}

int MSIpcConnection::syncSend(const XDR &,double)
{
  return 0;
}

int MSIpcConnection::syncSend(const XDR &,int,int,MSBoolean)
{
  return 0;
}

int MSIpcConnection::doSyncRead(XDR &)
{
  return 0;
}

