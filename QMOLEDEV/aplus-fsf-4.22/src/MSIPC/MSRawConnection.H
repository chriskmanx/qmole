#ifndef MSRawConnectionHEADER
#define MSRawConnectionHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSIPC/MSProtocolConnection.H>

#ifdef MS_VC_TEMPLATE_INSTANTIATION
class MSIPCExport MSProtocolConnection<MSString>;
#endif

class MSIPCExport MSRawConnection : public MSProtocolConnection<MSString>
{
protected:
  virtual void doRead(void);
public:
  MSRawConnection(const char *name_, const char *host_, int port_);
  MSRawConnection(const char *name_, const MSString &serviceName_);
  MSRawConnection(const char *name_, int fd_);
  ~MSRawConnection(void);  
  virtual int send(const MSString &);
  virtual int syncSend(const MSString &msg_,double seconds);
  virtual int syncSend(const MSString &msg_,int seconds,int microseconds,MSBoolean isAbsolute=MSFalse);
  virtual int doSyncRead(MSString &);
};

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSIPCLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSProtocolConnection<MSString>
#endif // !__MSIPCLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
