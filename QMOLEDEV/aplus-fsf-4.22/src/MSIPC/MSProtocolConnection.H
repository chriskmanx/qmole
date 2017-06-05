#ifndef MSProtocolConnectionHEADER
#define MSProtocolConnectionHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSDefines.H>
#include <MSTypes/MSCallback.H>
#include <MSTypes/MSNodeList.H>
#include <MSTypes/MSString.H>
#include <MSIPC/MSChannel.H>
#include <MSIPC/MSRegularTimer.H>
#include <MSIPC/MSConnection.H>
#include <MSIPC/MSListener.H>
#include <MSIPC/MSBuffer.H>
#include <MSIPC/MSHostPort.H>
#include <MSIPC/MSIPService.H>
#include <MSIPC/MSFds.H>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>


template <class Type>
class MSProtocolConnection: public MSConnection
{
public:
#ifdef MS_CLASS_ENUM_OVERLOADING_BUG
  enum State2{ReadPause=0x1,WritePause=0x2,Debug=0x10,Read=0x20,Write=0x40,Reset=0x80};  
  typedef int State;
#else
  enum State{ReadPause=0x1,WritePause=0x2,Debug=0x10,Read=0x20,Write=0x40,Reset=0x80};  
#endif
  typedef void (*ReadCallback)(const Type &,void *);
  typedef void (*SentCallback)(int,void *);
  typedef void (*ConnectCallback)(void *);
  typedef void (*ResetCallback)(MSProtocolConnection<Type>::State,void *);

protected:
  MSHostPort                 _hostPort;
  MSIPService                _service;
  MSNodeItem                 _writeList;
  MSBuffer                  *_headBuffer;
  MSBuffer                  *_readBuffer;  
  MSRegularTimer            *_timer;
  MSString                   _name;
  MSChannel		    *_readChannel;  
  MSChannel		    *_writeChannel;  
  ReadCallback		     _readCB;
  void 		            *_readCBData;
  SentCallback		     _sentCB;
  void 		            *_sentCBData;
  ConnectCallback	     _connectCB;
  void 		            *_connectCBData;
  ResetCallback		     _resetCB;
  void 		            *_resetCBData;
  unsigned long              _state;
  MSBoolean                  _burstMode;
  char                      *_syncErrorBuffer;
  
  inline MSHostPort& hostPort(void);
  inline MSRegularTimer *timer(void);

  // workaround a lucid compiler bug - i.e. MethodCallbacks
  // cannot directly invoke a virtual method.
  inline void doReadCall(void);
  inline void doWriteCall(void);
  inline void doConnectCall(void);
  inline void doWriteResetCall(void);

  void init(void);
  
  virtual void doRead(void);
  virtual void doReadBurst(void);
  virtual int doWrite(void);
  virtual int doWrite(MSBoolean);
  virtual void doConnect(void);
  virtual int doSyncWrite(void);
  virtual int doSyncRead(Type &);
  int syncReadSelectLoop(Type &,struct timeval *timeout);
  int syncWriteSelectLoop(struct timeval *timeout);
  virtual void syncErrorReport(void);
  int syncError(int rc,const char *symbol_,const char *fmt_, ...);

  virtual void writeReset(void);
  virtual void reset(void);           // reset the connection
  virtual void cleanup(void);         // cleanup before connection close
  virtual MSBoolean establish(void);  // establish connection
  virtual MSBoolean setup(void);      // setup the connection remote name
  virtual int readTheBuffer(MSBuffer *,int);
  virtual void sendTheBuffer(MSBuffer *);
  virtual int writeTheBuffer(MSBuffer *,int);

public:
  MSProtocolConnection(const char *name_,const char *host_,int port_);
  MSProtocolConnection(const char *name_,const MSString &serviceName_);
  MSProtocolConnection(const char *name_,int fd_);
  ~MSProtocolConnection(void);

  inline MSBoolean isSet(State);
  inline MSChannel *readChannel(void);
  inline MSChannel *writeChannel(void);
  inline MSNodeItem *writeList(void);
  inline MSBuffer *headBuffer(void);
  inline MSBuffer *readBuffer(void);
  inline MSIPService& service(void);
  inline const MSString& host(void) const;
  inline int port(void) const;
  inline const MSString& name(void) const;
  inline unsigned long state(void) const;
  inline MSBoolean burstMode(void);
  inline void burstMode(MSBoolean burstMode_);

  inline void set(State);
  inline void unset(State);

  virtual int send(const Type&);
  virtual int syncRead(Type &,double seconds);
  virtual int syncRead(Type &,int seconds,int microseconds,MSBoolean isAbsolute=MSFalse);
  virtual int syncSend(const Type &,double seconds);
  virtual int syncSend(const Type &,int seconds,int microseconds,MSBoolean isAbsolute=MSFalse);
  virtual void readNotify(const Type&);
  virtual void sentNotify(int);
  virtual void connectNotify(void);
  virtual void resetNotify(State);
  virtual void resetWithError(State);

  // Callback API
  void readCallback(ReadCallback,void *);
  inline ReadCallback readCallback(void);
  inline void *readCallbackData(void);
  void sentCallback(SentCallback,void *);
  inline SentCallback sentCallback(void);
  inline void *sentCallbackData(void);
  void connectCallback(ConnectCallback,void *);
  inline ConnectCallback connectCallback(void);
  inline void *connectCallbackData(void);
  void resetCallback(ResetCallback,void *);
  inline ResetCallback resetCallback(void);
  inline void *resetCallbackData(void);
};

template <class Type>
inline MSHostPort& MSProtocolConnection<Type>::
hostPort(void)
{ return _hostPort; }

template <class Type>
inline MSIPService& MSProtocolConnection<Type>::
service(void)
{ return _service; }

template <class Type>
inline const MSString& MSProtocolConnection<Type>::
name(void) const
{ return _name; }

template <class Type>
inline unsigned long MSProtocolConnection<Type>::
state(void) const
{ return _state; }

template <class Type>
inline MSBoolean MSProtocolConnection<Type>::
burstMode(void)
{ return _burstMode; }

template <class Type>
inline void MSProtocolConnection<Type>::
burstMode(MSBoolean burstMode_)
{ _burstMode=burstMode_; }

template <class Type>
inline typename MSProtocolConnection<Type>::ReadCallback MSProtocolConnection<Type>::
readCallback(void)
{ return _readCB; }

template <class Type>
inline void *MSProtocolConnection<Type>::
readCallbackData(void)
{ return _readCBData; }

template <class Type>
inline typename MSProtocolConnection<Type>::SentCallback MSProtocolConnection<Type>::
sentCallback(void)
{ return _sentCB; }

template <class Type>
inline void *MSProtocolConnection<Type>::
sentCallbackData(void)
{ return _sentCBData; }

template <class Type>
inline typename MSProtocolConnection<Type>::ConnectCallback MSProtocolConnection<Type>::
connectCallback(void)
{ return _connectCB; }

template <class Type>
inline void *MSProtocolConnection<Type>::
connectCallbackData(void)
{ return _connectCBData; }

template <class Type>
inline typename MSProtocolConnection<Type>::ResetCallback MSProtocolConnection<Type>::
resetCallback(void) { return _resetCB; }

template <class Type>
inline void *MSProtocolConnection<Type>::
resetCallbackData(void) { return _resetCBData; }

template <class Type>
inline MSRegularTimer *MSProtocolConnection<Type>::
timer(void) { return _timer; }

template <class Type>
inline MSBoolean MSProtocolConnection<Type>::
isSet(State s_) { return (_state&s_)?MSTrue:MSFalse; }

template <class Type>
inline MSChannel *MSProtocolConnection<Type>::
readChannel(void) { return _readChannel; }

template <class Type>
inline MSChannel *MSProtocolConnection<Type>::
writeChannel(void) { return _writeChannel; }

template <class Type>
inline MSNodeItem *MSProtocolConnection<Type>::
writeList(void) { return &_writeList; }

template <class Type>
inline MSBuffer *MSProtocolConnection<Type>::
headBuffer(void) { return _headBuffer; }

template <class Type>
inline MSBuffer *MSProtocolConnection<Type>::
readBuffer(void)
{ return _readBuffer; }

template <class Type>
inline const MSString& MSProtocolConnection<Type>::
host(void) const
{ return _hostPort.host(); }

template <class Type>
inline int MSProtocolConnection<Type>::
port(void) const
{ return _hostPort.port(); }

template <class Type>
inline void MSProtocolConnection<Type>::
set(State s_)
{ _state|=s_; }

template <class Type>
inline void MSProtocolConnection<Type>::
unset(State s_)
{ _state&=~s_; }

template <class Type>
inline void MSProtocolConnection<Type>::
doReadCall(void)
{
  if (burstMode()==MSTrue) doReadBurst();
  else doRead();
}

template <class Type>
inline void MSProtocolConnection<Type>::
doWriteCall(void)
{ doWrite(); } 

template <class Type>
inline void MSProtocolConnection<Type>::
doConnectCall(void)
{ doConnect(); }

template <class Type>
inline void MSProtocolConnection<Type>::
doWriteResetCall(void)
{ writeReset(); }

#endif

