///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// AipcListener
//
// This class is a sub-class of MSProtocolListener, and intended to be
// an abstract class for all libIPC listener classes compatible with MStk.

#include <AipcListener.H>
#include <netinet/in.h> 

// Static members

I AipcListener::ServiceType=2;

AipcListener::AipcListener(C *name_,I port_,A cbfunc_)
  : MSProtocolListener(name_,port_),
    AipcService(cbfunc_)
{
  ipcWarn(0,"%t AipcListener::AipcListener(0)\n");
  init();
}  

AipcListener::AipcListener(C *serviceName_,A cbfunc_)
  : MSProtocolListener(serviceName_,-1),
    AipcService(cbfunc_)
{
  ipcWarn(0,"%t AipcListener::AipcListener(1)\n");
  AipcHostPortProtocol ahpp(serviceName_);
  hostPort().set(ahpp.host(),ahpp.port());
  init();
}

AipcListener::AipcListener(AipcHostPortProtocol &ahpp_,
				 A cbfunc_)
  : MSProtocolListener(ahpp_.service(),ahpp_.port()),
    AipcService(cbfunc_)
{
  ipcWarn(0,"%t AipcListener::AipcListener(2)\n");
  init();
}

AipcListener::~AipcListener(void)
{
  ipcWarn(wrnlvl(),"%t AipcListener::~AipcListener\n");
  close();
}

void AipcListener::init(void)
{
  ipcWarn(0,"%t AipcListener::init\n");
  _zeroPort=(0==port())?MSTrue:MSFalse;
  _attrs.listener(handle());
}

// M:Attribute interface

A AipcListener::setableAttrlist(void){return (A)ic(_attrs.setableAttrs());}

A AipcListener::getableAttrlist(void)
{
  int i,idx=0;
  A sattrs=_attrs.setableAttrs();
  A nsattrs=_attrs.nonsetableAttrs();
  A z=gv(Et,sattrs->n+nsattrs->n);
  for (i=0;i<sattrs->n;++i)z->p[idx++]=sattrs->p[i];
  for (i=0;i<nsattrs->n;++i)z->p[idx++]=nsattrs->p[i];
  R z;
}

MSBoolean AipcListener::setAttr(C *attr_,A aval_)
{
  ipcWarn(wrnlvl(),"%t AipcListener::setAttr\n");
  int idx=_attrs.setAttrIndex(attr_);
  I ival;
  switch(idx) {
    CS(0,BoolCHK(aval_);_attrs.noDelay(ival?MSTrue:MSFalse));
    CS(1,BoolCHK(aval_);_attrs.readPause(ival?MSTrue:MSFalse));
    CS(2,BoolCHK(aval_);_attrs.writePause(ival?MSTrue:MSFalse));
    CS(3,ItCHK(aval_);_attrs.readPriority(ival));
    CS(4,ItCHK(aval_);_attrs.writePriority(ival));
    CS(5,ItCHK(aval_);_attrs.readBufsize(ival));
    CS(6,ItCHK(aval_);_attrs.writeBufsize(ival));
    CS(7,BoolCHK(aval_);_attrs.retry(ival?MSTrue:MSFalse));
    CS(8,_attrs.clientData((A)ic(aval_)));
    CS(9,BoolCHK(aval_);
       if(ival)turnDebugOn();else turnDebugOff();
       _attrs.debug(ival?MSTrue:MSFalse));
  default: R MSFalse;
  }
  R MSTrue;
}

A AipcListener::getAttr(C *attr_)
{
  ipcWarn(wrnlvl(),"%t AipcListener::getAttr\n");
  int idx=_attrs.setAttrIndex(attr_);
  if(-1!=idx)
  {
    switch(idx) {
      CSR(0,R gi(_attrs.noDelay()?1:0));
      CSR(1,R gi(_attrs.readPause()?1:0));
      CSR(2,R gi(_attrs.writePause()?1:0));
      CSR(3,R gi(_attrs.readPriority()););
      CSR(4,R gi(_attrs.writePriority()););
      CSR(5,R gi(_attrs.readBufsize()););
      CSR(6,R gi(_attrs.writeBufsize()););
      CSR(7,R gi(_attrs.retry()?1:0));
      CSR(8,R(A)ic(_attrs.clientData()));
      CSR(9,R gi(_attrs.debug()?1:0));
    default: R aplus_nl;
    }
  } else {
    int idx=_attrs.nonsetAttrIndex(attr_);
    if(-1!=idx)
    {
      switch(idx) {
	CSR(0,R gi(fd())); /* fd */
	CSR(1,R gi(port())); /* port */
      default: R aplus_nl;
      }
    }
    else R aplus_nl; /* subclasses call parent class here */
  } 
}

  
// M:Callbacks

void AipcListener::acceptNotify(int fd_,
				const struct sockaddr *peername,
				int peernamelen,
				const struct sockaddr *sockname,
				int socknamelen)
{
  ipcWarn(wrnlvl(),
	  "%t AipcListener::acceptNotify: no connection specified. fd=%d\n",
	  fd_);
  /* next line is just to stop compiler from whining about unused names. */
  if(0)ipcWarn(0,"",peername,peernamelen,sockname,socknamelen);
}


// M:Other methods;

int AipcListener::getListenPort(void)
{
  ipcWarn(wrnlvl(),"%t AipcListener::getListenPort\n");
  if (-1==fd()) R -1;

#if defined(HAVE_SOCKLEN_T)
  if(0!=(getsockname(fd(),localName(),(socklen_t *)&_localNamelen))) R -1;
#else
  if(0!=(getsockname(fd(),localName(),&_localNamelen))) R -1;
#endif
  struct sockaddr_in *pSockaddrIn=( sockaddr_in *)localName(); 
  R ntohs(pSockaddrIn->sin_port); 
}

void AipcListener::open(void)
{
  ipcWarn(wrnlvl(),"%t AipcListener::open\n");
  MSProtocolListener::open();
  if (_zeroPort)
  {
    _hostPort.set(host(),getListenPort());
    ipcWarn(wrnlvl(),"%t   port fixed at %d\n",port());
  }
}

void AipcListener::close(void)
{
  ipcWarn(wrnlvl(),"%t AipcListener::close\n");
  if (_zeroPort) _hostPort.set(host(),0);
  MSProtocolListener::close();
}

