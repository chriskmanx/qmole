#ifndef MSIpcConnectionHEADER
#define MSIpcConnectionHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSIPC/MSProtocolConnection.H>

class MSIPCExport XDR
{
private:
	void	*_data;
public:
	XDR(void){}
	~XDR(void){}
};

#ifdef MS_VC_TEMPLATE_INSTANTIATION
class MSIPCExport MSProtocolConnection<XDR>;
#endif

class MSIPCExport MSIpcConnection : public MSProtocolConnection<XDR>
{
protected:
  virtual void doRead(void);
public:
  MSIpcConnection(const char *name_, const char *host_, int port_);
  MSIpcConnection(const char *name_, const MSString &serviceName_);
  MSIpcConnection(const char *name_, int fd_);
  ~MSIpcConnection(void);  
  virtual int send(const XDR &);
  virtual int syncSend(const XDR &msg_,double seconds);
  virtual int syncSend(const XDR &msg_,int seconds,int microseconds,MSBoolean isAbsolute);
  virtual int doSyncRead(XDR &msg_);
};

// We provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSIPCLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSProtocolConnection<XDR>
#endif // !__MSIPCLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif
