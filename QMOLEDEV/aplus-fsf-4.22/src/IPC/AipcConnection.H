#ifndef __AipcConnection_H
#define __AipcConnection_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <a/k.h>
#include <a/fncdcls.h>
#include <dap/Warn.h>

#if defined(_AIX)
#include <sys/select.h>
#endif

#include <MSIPC/MSProtocolConnection.H>
#include <AipcService.H>

#include <AipcAttributes.H>
#include <AipcHostPortProtocol.H>

class AipcConnection : public MSProtocolConnection<A>, public AipcService
{
 public:
  static I ServiceType;

 protected:
  AipcAttributes _attrs;

  // subroutine methods
  void init(void);
  void initAttrs(void);
  int readFileLength(void);
  
 public:
  // AipcConnection (Service Control Block) Constructors;
  AipcConnection(C *name_,C *host_,I port_,A cbfunc_);
  AipcConnection(C *name_, A cbfunc_);
  AipcConnection(AipcHostPortProtocol &ahpp_, A cbfunc_);
  AipcConnection(C *name_,int fd_, A cbfunc_,AipcAttributes &attrs_);
  virtual ~AipcConnection(void);

  // M:Flags;
  void turnNoDelayOn(void);
  void turnNoDelayOff(void);
  MSBoolean isNoDelay(void) {return _attrs.noDelay();}

  void turnInReadOn(void) {set(MSProtocolConnection<A>::Read);}
  void turnInReadOff(void) {unset(MSProtocolConnection<A>::Read);}
  MSBoolean isInRead(void) {return isSet(MSProtocolConnection<A>::Read);}

  void turnInWriteOn(void) {set(MSProtocolConnection<A>::Write);}
  void turnInWriteOff(void) {unset(MSProtocolConnection<A>::Write);}
  MSBoolean isInWrite(void) {return isSet(MSProtocolConnection<A>::Write);}

  void turnReadPauseOn(void);
  void turnReadPauseOff(void);
  MSBoolean isReadPause(void) {
    return isSet(MSProtocolConnection<A>::ReadPause);}

  void turnWritePauseOn(void);
  void turnWritePauseOff(void);
  MSBoolean isWritePause(void) {
    return isSet(MSProtocolConnection<A>::WritePause);}

  void turnRetryOn(void) {_retry=MSConnection::Yes;}
  void turnRetryOff(void) {_retry=MSConnection::No;}
  MSBoolean isRetry(void) {return (MSConnection::Yes==_retry)?MSTrue:MSFalse;}

  void turnInResetOn(void) {set(MSProtocolConnection<A>::Reset);}
  void turnInResetOff(void) {unset(MSProtocolConnection<A>::Reset);}
  MSBoolean isInReset(void) {return isSet(MSProtocolConnection<A>::Reset);}

  // M:Attribute interface;
  virtual A setableAttrlist(void);
  virtual A getableAttrlist(void);
  virtual MSBoolean setAttr(C *attr_,A aobj);
  virtual A getAttr(C *attr_);

  // M:Callbacks;
  virtual void readNotify(const A&);
  virtual void doConnect(void);
  virtual void connectNotify(void);
  virtual void sentNotify(int);
  virtual void resetNotify(MSProtocolConnection<A>::State);
  virtual void resetNotify(const C *errmsg_);
  virtual void resetWithError(const C *errmsg_){reset();resetNotify(errmsg_);}
  virtual void resetWithError(State){resetWithError("unknown State");}

  // M:Other methods;
  // C *name(void) {return (C*)MSProtocolConnection<A>::name().string();}
  virtual void initFromListener(void);
  virtual I serviceType(void) {return AipcConnection::ServiceType;}
  virtual int send(const A &aobj)
  {
    static char fmt[]="this connection does not send\n";
    Warn(fmt);
    return aobj?0:0;
  }
  virtual void reset(void);
  MSBoolean isOpen(void) {return(-1==fd())?MSFalse:MSTrue;}
  A writeQueueStatus(void);
  A readQueueStatus(void);

  // M:Syncronous Send and Read;
  virtual A syncSendCover(A,A){R aplus_nl;}
  virtual A syncReadCover(A){R aplus_nl;}
  virtual A syncTimeout(A){R aplus_nl;}

  int openSync(int timeout);
};


#endif

