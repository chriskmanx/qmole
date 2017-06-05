///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <dap/Warn.h>
#include <dap/balloc.h>
#include <a/f.h>
#include <a/x.h>
#include <a/fir.h>
#include <a/fncdcls.h>

#undef ENTRYPOINT
#define ENTRYPOINT static

#include <MSTypes/MSMessageLog.H>

#include <pA_Listener.H>
#include <pA_Connection.H>
#include <pIpc_Listener.H>
#include <pIpc_Connection.H>
#include <pRaw_Listener.H>
#include <pRaw_Connection.H>
#include <pString_Listener.H>
#include <pString_Connection.H>
#include <pSimple_Listener.H>
#include <pSimple_Connection.H>
#include <TimrConnection.H>

#include <IPCUtilities.H>

#define SRVPTR(X,MSG)  AipcService *srvptr=lookupAnyHandle(X); \
                        ipcWarn(srvptr?srvptr->wrnlvl():0,MSG); \
                        if(!srvptr) return -1;

#define H_SRVPTR(MSG)   SRVPTR(handle_,MSG);

I ipcWarnFlag=-1;

static C *getString(A aobj)
{
  C *z;
  if (Ct==aobj->t)z=(C *)aobj->p;
  else if (Et==aobj->t&&1==aobj->n&&QS(*aobj->p))z=XS(*aobj->p)->n;
  else z=(C *)0;
  return z;
}

static S getSymbol(A aobj)
{
  if (Et==aobj->t&&1==aobj->n&&QS(*aobj->p))return XS(*aobj->p);
  return (S)0;
}

static void WarnFunc(C *s)
{
  fputs(s,stderr); fflush(stderr);
}

static int Initialized=0;

static S ProtocolList[32][2];

static void Initialize(void)
{
  int idx=0;
  Initialized=1;

  ipcWarnFlag=-1;
  SetWarnFunc(0);
  MSMessageLog::quietMode(MSTrue);
  ipcWarn(0,"%t Initialize:\n");
  // Protocol Name                         Alt Protocal Name
  ProtocolList[idx++][0]=si("A");       ProtocolList[idx][1]=0;
  ProtocolList[idx++][0]=si("raw");     ProtocolList[idx][1]=si("RAW");
  ProtocolList[idx++][0]=si("tick");    ProtocolList[idx][1]=0;
  ProtocolList[idx++][0]=si("stream");  ProtocolList[idx][1]=0;
  ProtocolList[idx++][0]=si("ipc");     ProtocolList[idx][1]=0;
  ProtocolList[idx++][0]=si("delta");   ProtocolList[idx][1]=0;
  ProtocolList[idx++][0]=si("string");  ProtocolList[idx][1]=0;
  ProtocolList[idx++][0]=si("simple");  ProtocolList[idx][1]=0;
  ProtocolList[idx++][0]=(S)0;
}

static void InitializeIfNeeded(void)
{
  if(!Initialized)Initialize();
}

static int lookupProtocol(S sym)
{
  int i;
  for(i=0;ProtocolList[i][0];++i)
    if((sym==ProtocolList[i][0])||sym==ProtocolList[i][1])
      return i;
  return -1;
}

// static AipcConnection *lookupConnHandle(int handle_)
// {
//   InitializeIfNeeded();
//   return (AipcConnection *)
//     AipcService::lookup(handle_,AipcConnection::ServiceType);
// }

static AipcService *lookupAnyHandle(int handle_)
{
  InitializeIfNeeded();
  return (AipcService *)AipcService::lookup(handle_);
}

ENTRYPOINT
A ipcRoster(void){return AipcService::roster();}  

ENTRYPOINT
I ipcListenNPP(A afunc, A aname, I port, A aprotocol)
{
  C *serviceName=getString(aname);
  S protocolSym=getSymbol(aprotocol);
  int protocolidx;
  AipcListener *nLstn;

  InitializeIfNeeded();
  ipcWarn(0,"%t ipcListenNPP:\n");

  if(!serviceName||!protocolSym||!QF(afunc))return -1;

  ipcWarn(0,"%t ipcListenNPP: name:%s port:%d protocol:%s\n",serviceName,
	  port,protocolSym->n);

  protocolidx=lookupProtocol(protocolSym);
  switch(protocolidx) {
  case 0: nLstn=new pA_Listener(serviceName,port,afunc); break;
  case 1: nLstn=new pRaw_Listener(serviceName,port,afunc); break;
  case 4: nLstn=new pIpc_Listener(serviceName,port,afunc); break;
  case 6: nLstn=new pString_Listener(serviceName,port,afunc); break;
  case 7: nLstn=new pSimple_Listener(serviceName,port,afunc); break;
  default: /* unknown protocol */
    Warn("%t ipcListenNPP: unknown protocol:%s\n",protocolSym->n);
    return -1;
  }
  return nLstn->handle();
}

ENTRYPOINT
I ipcListenN(A afunc, A aname)
{
  C *serviceName=getString(aname);
  int protocolidx;
  AipcListener *nLstn;

  InitializeIfNeeded();
  ipcWarn(0,"%t ipcListenN:\n");

  if(!serviceName||!QF(afunc))return -1;

  AipcHostPortProtocol *ahpp=new AipcHostPortProtocol(serviceName);

//   const C *hostName=ahpp->host();
  I port=ahpp->port();
  S protocolSym=si((char *)ahpp->protocol());

  ipcWarn(0,"%t ipcListenN: name:%s port:%d protocol:%s\n",serviceName,
	  port,protocolSym->n);

  protocolidx=lookupProtocol(protocolSym);
  switch(protocolidx) {
  case 0: nLstn=new pA_Listener(*ahpp,afunc); break;
  case 1: nLstn=new pRaw_Listener(*ahpp,afunc); break;
  case 4: nLstn=new pIpc_Listener(*ahpp,afunc); break;
  case 6: nLstn=new pString_Listener(*ahpp,afunc); break;
  case 7: nLstn=new pSimple_Listener(*ahpp,afunc); break;
  default: /* unknown protocol */
    Warn("%t ipcListenN: unknown protocol:%s\n",protocolSym->n);
    return -1;
  }
  return nLstn->handle();
}

ENTRYPOINT
I ipcListenNP(A afunc, A aname, A aprotocol)
{
  C *serviceName=getString(aname);
  S protocolSym=getSymbol(aprotocol);
  int protocolidx;
  AipcListener *nLstn;

  InitializeIfNeeded();
  ipcWarn(0,"%t ipcListenN:\n");

  if(!serviceName||!QF(afunc))return -1;

  AipcHostPortProtocol *ahpp=new AipcHostPortProtocol(serviceName);

//   const C *hostName=ahpp->host();
  I port=ahpp->port();

  ipcWarn(0,"%t ipcListenN: name:%s port:%d protocol:%s\n",serviceName,
	  port,protocolSym->n);

  protocolidx=lookupProtocol(protocolSym);
  switch(protocolidx) {
  case 0: nLstn=new pA_Listener(*ahpp,afunc); break;
  case 1: nLstn=new pRaw_Listener(*ahpp,afunc); break;
  case 4: nLstn=new pIpc_Listener(*ahpp,afunc); break;
  case 6: nLstn=new pString_Listener(*ahpp,afunc); break;
  case 7: nLstn=new pSimple_Listener(*ahpp,afunc); break;
  default: /* unknown protocol */
    Warn("%t ipcListenN: unknown protocol:%s\n",protocolSym->n);
    return -1;
  }
  return nLstn->handle();
}

ENTRYPOINT
I ipcConnectNHPP(A afunc, A aname, A ahost, I port, A aprotocol)
{
  C *serviceName=getString(aname);
  C *hostName=getString(ahost);
  S protocolSym=getSymbol(aprotocol);
  int protocolidx;
  AipcService *nSrv;

  InitializeIfNeeded();
  ipcWarn(0,"%t ipcConnectNHPP:\n");

  if (!serviceName||!hostName||!protocolSym||!QF(afunc))return -1;

  ipcWarn(0,"%t ipcConnectNHPP: name:%s host:%s port:%d protocol:%s\n",
	serviceName,hostName,port,protocolSym->n);

  protocolidx=lookupProtocol(protocolSym);
  switch(protocolidx) {
  case 0: nSrv=new pA_Connection(serviceName,hostName,port,afunc); break;
  case 1: nSrv=new pRaw_Connection(serviceName,hostName,port,afunc); break;
  case 2: 
    Warn("%t ipcConnectHPP: unsupported protocol:%s\n",protocolSym->n);
    return -1;
    break;
  case 4: nSrv=new pIpc_Connection(serviceName,hostName,port,afunc); break;
  case 5: 
    Warn("%t ipcConnectHPP: unsupported protocol:%s\n",protocolSym->n);
    return -1;
    break;
  case 6: nSrv=new pString_Connection(serviceName,hostName,port,afunc); break;
  case 7: nSrv=new pSimple_Connection(serviceName,hostName,port,afunc); break;
  default: /* unknown protocol */
    Warn("%t ipcConnectHPP: unknown protocol:%s\n",protocolSym->n);
    return -1;
  }
  return nSrv->handle();
}

ENTRYPOINT
I ipcConnectN(A afunc, A aname)
{
  C *serviceName=getString(aname);
  int protocolidx;
  AipcService *nSrv;

  InitializeIfNeeded();
  ipcWarn(0,"%t ipcConnectN:\n");

  if (!serviceName||!QF(afunc))return -1;

  AipcHostPortProtocol *ahpp=new AipcHostPortProtocol(serviceName);

//   const C *hostName=ahpp->host();
//   I port=ahpp->port();
  S protocolSym=si((char *)ahpp->protocol());

  ipcWarn(0,"%t ipcConnectN: name:%s host:%s port:%d protocol:%s\n",
	serviceName,ahpp->host(),ahpp->port(),ahpp->protocol());

  protocolidx=lookupProtocol(protocolSym);
  switch(protocolidx) {
  case 0: nSrv=new pA_Connection(*ahpp,afunc); break;
  case 1: nSrv=new pRaw_Connection(*ahpp,afunc); break;
  case 4: nSrv=new pIpc_Connection(*ahpp,afunc); break;
  case 5: {
    Warn("%t ipcConnectHPP: unsupported protocol:%s\n",protocolSym->n);
    return -1;
  }
  case 6: nSrv=new pString_Connection(*ahpp,afunc); break;
  case 7: nSrv=new pSimple_Connection(*ahpp,afunc); break;
  default: /* unknown protocol */
    Warn("%t ipcConnectN: unknown protocol:%s\n",protocolSym->n);
    return -1;
  }
  return nSrv->handle();
}

ENTRYPOINT
I ipcConnectNP(A afunc, A aname, A aprotocol)
{
  C *serviceName=getString(aname);
  S protocolSym=getSymbol(aprotocol);
  int protocolidx;
  AipcService *nSrv;

  InitializeIfNeeded();
  ipcWarn(0,"%t ipcConnectN:\n");

  if (!serviceName||!protocolSym||!QF(afunc))return -1;

  AipcHostPortProtocol *ahpp=new AipcHostPortProtocol(serviceName);
  ahpp->protocol(protocolSym->n);

//   const C *hostName=ahpp->host();
//   I port=ahpp->port();

  ipcWarn(0,"%t ipcConnectN: name:%s host:%s port:%d protocol:%s\n",
	serviceName,ahpp->host(),ahpp->port(),ahpp->protocol());

  protocolidx=lookupProtocol(protocolSym);
  switch(protocolidx) {
  case 0: nSrv=new pA_Connection(*ahpp,afunc); break;
  case 1: nSrv=new pRaw_Connection(*ahpp,afunc); break;
  case 4: nSrv=new pIpc_Connection(*ahpp,afunc); break;
  case 6: nSrv=new pString_Connection(*ahpp,afunc); break;
  case 7: nSrv=new pSimple_Connection(*ahpp,afunc); break;
  default: /* unknown protocol */
    Warn("%t ipcConnectN: unknown protocol:%s\n",protocolSym->n);
    return -1;
  }
  return nSrv->handle();
}

ENTRYPOINT
int ipcTimer(A afunc, A timeout)
{
  InitializeIfNeeded();
  ipcWarn(0,"%t ipcTimer\n");
  TimrConnection *nTmr=new TimrConnection(timeout, afunc);
  return nTmr->handle();
}


ENTRYPOINT
int ipcOpen(int handle_)
{
  H_SRVPTR("%t ipcOpen\n");  
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    ((AipcConnection *)srvptr)->open();
    break;
  case 2: /* listener */
    ((AipcListener *)srvptr)->open();
    break;
  case 4: /* timer */
    ((TimrConnection *)srvptr)->open();
    break;
  default: return -1;
  }
  return 0;
}

ENTRYPOINT
int ipcOpenSync(int handle_, int timeout_)
{
  H_SRVPTR("%t ipcOpenSync\n");  
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    return ((AipcConnection *)srvptr)->openSync(timeout_);
  default: return -1;
  }
  return 0;
}

ENTRYPOINT
int ipcDestroy(int handle_)
{
  H_SRVPTR("%t ipcDestroy\n");
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    delete (AipcConnection *)srvptr;
    break;
  case 2: /* listener */
    delete (AipcListener *)srvptr;
    break;
  case 4: /* timer */
    delete (TimrConnection *)srvptr;
    break;
  default: return -1;
  }
  return 0;
}

ENTRYPOINT
I ipcSend(int handle_,A msg_)
{
  H_SRVPTR("%t ipcSend\n");
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    return ((AipcConnection *)srvptr)->send(msg_);
  default: return -1;
  }
}

ENTRYPOINT
A ipcSyncSend(int handle_,A msg_,A timeout_)
{
  AipcService *srvptr=lookupAnyHandle(handle_);
  ipcWarn(srvptr?srvptr->wrnlvl():0,"%t ipcSyncSend\n");
  if(!srvptr) R(A)0;
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    return ((AipcConnection *)srvptr)->syncSendCover(msg_,timeout_);
  default: R(A)0;
  }
}

ENTRYPOINT
A ipcSyncRead(int handle_,A timeout_)
{
  AipcService *srvptr=lookupAnyHandle(handle_);
  ipcWarn(srvptr?srvptr->wrnlvl():0,"%t ipcSyncRead\n");
  if(!srvptr)R(A)0;
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    return ((AipcConnection *)srvptr)->syncReadCover(timeout_);
  default: return aplus_nl;
  }
}

ENTRYPOINT
A ipcGetTimeout(A aobj_)
{
  return getAbsoluteTimeout(aobj_);
}

ENTRYPOINT
I ipcSetDebug(I handle_,I val_)
{
  InitializeIfNeeded();

  if (0==handle_) ipcWarnFlag=val_;
  else 
  {    
    AipcService *srvptr=lookupAnyHandle(handle_);
    if(!srvptr)return -1;
    if(val_)srvptr->turnDebugOn();else srvptr->turnDebugOff();
    if (-1==ipcWarnFlag) ipcWarnFlag=0;
  }
  if (ipcWarnFlag>=0)
  {
    SetWarnFunc(WarnFunc); 
    MSMessageLog::quietMode(MSFalse);
  } else {
    SetWarnFunc(0);
    MSMessageLog::quietMode(MSTrue);
  }
  return val_;
}

ENTRYPOINT
I ipcDebug(I s)
{
  InitializeIfNeeded();
  ipcWarn(0,"%t ipcDebug %d\n",s);
  if (s) ipcSetDebug(0,2); else ipcSetDebug(0,-1); 
  return s;
}

ENTRYPOINT
int ipcClose(int handle_)
{
  H_SRVPTR("%t ipcClose\n");
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    ((AipcConnection *)srvptr)->reset();
    break;
  case 2: /* listener */
    ((AipcListener *)srvptr)->close();
    break;
  case 4: /* timer */
    ((TimrConnection *)srvptr)->close();
    break;
  default: return -1;
  }
  return 0;
}

ENTRYPOINT
int ipcSetAttr(I handle_,A aflag_,A aval_){
  H_SRVPTR("%t ipcSetAttr\n");
  C *flag=getString(aflag_);
  MSBoolean setok;
  if(!flag)return -1;
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    setok=((AipcConnection *)srvptr)->setAttr(flag,aval_);
    break;
  case 2: /* listener */
    setok=((AipcListener *)srvptr)->setAttr(flag,aval_);
    break;
  case 4: /* timer */
    setok=((TimrConnection *)srvptr)->setAttr(flag,aval_);
     break;
  default: return -1;
  }
  return setok?0:-1;
}

ENTRYPOINT
A ipcGetAttr(I handle_,A aflag_)
{
  AipcService *srvptr=lookupAnyHandle(handle_);
  ipcWarn(srvptr?srvptr->wrnlvl():0,"%t ipcGetAttr\n");
  C *flag=getString(aflag_);
  A z=(A)0;
  if(!srvptr)return z;
  if(!flag)return z;
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    z=((AipcConnection *)srvptr)->getAttr(flag);
    break;
  case 2: /* listener */
    z=((AipcListener *)srvptr)->getAttr(flag);
    break;
  case 4: /* timer */
    z=((TimrConnection *)srvptr)->getAttr(flag);
    break;
  default:
    break;
  }
  return z;
}


ENTRYPOINT
A ipcAttrlists(I handle_)
{
  AipcService *srvptr=lookupAnyHandle(handle_);
  ipcWarn(srvptr?srvptr->wrnlvl():0,"%t ipcAttrlists\n");
  A z=(A)0;
  if(!srvptr)return z;
  switch(srvptr->serviceType()) {
  case 1: /* connection */
    z=gvi(Et,2,((AipcConnection *)srvptr)->getableAttrlist(),
	  ((AipcConnection *)srvptr)->setableAttrlist());
    break;
  case 2: /* listener */
    z=gvi(Et,2,((AipcListener *)srvptr)->getableAttrlist(),
	  ((AipcListener *)srvptr)->setableAttrlist());
    break;
  case 4: /* timer */
    z=gvi(Et,2,((TimrConnection *)srvptr)->getableAttrlist(),
	  ((TimrConnection *)srvptr)->setableAttrlist());
    break;
  }
  return z;
}

static C*serviceNames[] = {"unknown", "connector", "listener", "libtick",
			     "timer"};

ENTRYPOINT
A ipcWhatis(I handle_)
{
  AipcService *srvptr=lookupAnyHandle(handle_);
  ipcWarn(srvptr?srvptr->wrnlvl():0,"%t ipcWhatis\n");
  if(srvptr==0) return gvi(Et,2,MS(si("")),MS(si("")));
  return gvi(Et,2,MS(si(serviceNames[srvptr->serviceType()])),
	MS(si((char *)srvptr->protocol())));
}

extern "C" void ipcInstall(void);

void ipcInstall(void)
{
  CX saveCx=Cx;
  Cx=cx("i");
  install((PFI)ipcRoster,       "roster",   A_, 0,  0,  0,  0,  0,  0, 0,0,0);
  install((PFI)ipcListenNPP,    "listenNPP",IV, 4, A_, A_, IV, A_,  0, 0,0,0);
  install((PFI)ipcListenN,      "listenN",  IV, 2, A_, A_, 0,   0,  0, 0,0,0);
  install((PFI)ipcListenNP,     "listenNP", IV, 3, A_, A_, A_,  0,  0, 0,0,0);
  install((PFI)ipcConnectNHPP,"connectNHPP",IV, 5, A_, A_, A_, IV, A_, 0,0,0);
  install((PFI)ipcConnectN,     "connectN", IV, 2, A_, A_,  0,  0,  0, 0,0,0);
  install((PFI)ipcConnectNP,    "connectNP",IV, 3, A_, A_, A_,  0,  0, 0,0,0);
  install((PFI)ipcTimer,        "timer",    IV, 2, A_, A_,  0,  0,  0, 0,0,0);
  install((PFI)ipcOpen,         "open",     IV, 1, IV,  0,  0,  0,  0, 0,0,0);
  install((PFI)ipcOpenSync,     "openSync", IV, 2, IV, IV,  0,  0,  0, 0,0,0);
  install((PFI)ipcSend,         "send",     IV, 2, IV, A_,  0,  0,  0, 0,0,0);
  install((PFI)ipcSyncSend,     "syncsend", A_, 3, IV, A_, A_,  0,  0, 0,0,0);
  install((PFI)ipcSyncRead,     "syncread", A_, 2, IV, A_,  0,  0,  0, 0,0,0);
  install((PFI)ipcGetTimeout,   "timeout",  A_, 1, A_,  0,  0,  0,  0, 0,0,0);
  install((PFI)ipcSetDebug,     "setdebug", IV, 2, IV, A_,  0,  0,  0, 0,0,0);
  install((PFI)ipcDebug,        "debug",    IV, 1, IV,  0,  0,  0,  0, 0,0,0);
  install((PFI)ipcDestroy,      "destroy",  IV, 1, IV,  0,  0,  0,  0, 0,0,0);
  install((PFI)ipcClose,        "close",    IV, 1, IV,  0,  0,  0,  0, 0,0,0);
  install((PFI)ipcAttrlists,    "attrs",    A_, 1, IV,  0,  0,  0,  0, 0,0,0);
  install((PFI)ipcSetAttr,      "setattr",  IV, 3, IV, A_, A_,  0,  0, 0,0,0);
  install((PFI)ipcGetAttr,      "getattr",  A_, 2, IV, A_,  0,  0,  0, 0,0,0);
  install((PFI)ipcWhatis,       "whatis",   A_, 1, IV,  0,  0,  0,  0, 0,0,0);

  Cx=saveCx;

  {
    char *atree;
    char *ipcfile;
    
    if ((atree = getenv("ATREE")) == (char *)(0))
    {
      atree = "/usr/local/a+";
    }
    ipcfile = bnstring(atree, "/acore/idap.+", (char *)(0));
    loadafile(ipcfile,0);
    bfree(ipcfile);
  }
}
