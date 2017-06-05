#ifndef MSAConnectionHEADER
#define MSAConnectionHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>
#include <MSTypes/MSA.H>
#include <MSIPC/MSProtocolConnection.H>
#include <MSIPC/MSBuffer.H>

#ifdef MS_VC_TEMPLATE_INSTANTIATION
class MSIPCExport MSProtocolConnection<MSA>;
#endif

class MSIPCExport MSAConnection : public MSProtocolConnection<MSA>
{
protected:
  int _bytesToNextMessage;
  virtual void doRead(void);
  virtual void doReadBurst(void);
  MSA getAobjectFromBuffer(MSBuffer *buf);
public:
  MSAConnection(const char *name_, const char *host_, int port_);
  MSAConnection(const char *name_, const MSString &serviceName_);
  MSAConnection(const char *name_, int fd_);
  ~MSAConnection(void);
  virtual int send(const MSA &);
  virtual int syncSend(const MSA &msg_,double seconds);
  virtual int syncSend(const MSA &msg_,int seconds,int microseconds,MSBoolean isAbsolute);
  virtual int doSyncRead(MSA &);

  MSBuffer *exportAObject(const MSA& aObj_);
#ifdef MS_64BIT
  MSBuffer *export64AObject(const MSA& aObj_);
#endif
};

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSIPCLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSProtocolConnection<MSA>
#endif // !__MSIPCLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
