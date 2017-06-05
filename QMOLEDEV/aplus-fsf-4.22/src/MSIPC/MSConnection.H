#ifndef MSConnectionHEADER
#define MSConnectionHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <MSTypes/MSString.H>
#include <MSIPC/MSChannel.H>
#include <MSIPC/MSRegularTimer.H>
#include <MSIPC/MSExpBackoff.H>
#include <MSIPC/MSTv.H>

class MSIPCExport MSConnection
{
public:
  enum Retry{No=0,Yes=1};

protected:
// configuration information 
  MSString            _name;          // connection's name 
  int                 _pri;           // connection accept priority 
  MSConnection::Retry _retry;         // retry connection after failure 
  int                 _domain;        // socket address domain 
  int                 _type;          // socket type 
  int                 _protocol;      // socket protocol 
  MSBoolean	      _tcpNoDelay;    // TCP_NODELAY socket option on connection

// state information 
  MSExpBackoff            _retryTime;          // retry time limit with back-off 
  int                     _fd;                 // file descriptor 
  int                     _remoteNamelen;      // local name length
  struct sockaddr        *_remoteName;         // local name
  MSRegularTimer         *_retryTimer;         // retry timer 
  MSChannel		 *_establishChannel;   // connection establishment channel
  MSBoolean               _established;

// administrative information 
  time_t   _createTod;        // time connection was created
  time_t   _openTod;          // time connection was opened 
  time_t   _connectTod;       // time connection was attempted
  time_t   _establishTod;     // time connection was established
  time_t   _disconnectTod;    // time connection was disconnected
  time_t   _closeTod;         // time connection was closed
  time_t   _openedTime;       // seconds opened in prev instances 
  time_t   _establishedTime;  // seconds established in prev instances 
  unsigned _openCount;        // number of times opened 
  unsigned _connectCount;     // number of times connected
  unsigned _establishCount;   // number of times established
  unsigned _disconnectCount;  // number of times disconnected
  unsigned _closeCount;       // number of times closed 

  int priority(void)              const {return _pri;}           
  int domain(void)                const {return _domain;}        
  int type(void)                  const {return _type;}          
  int protocol(void)              const {return _protocol;}      

  time_t createTod(void)          const {return _createTod;}        
  time_t openTod(void)            const {return _openTod;}          
  time_t connectTod(void)         const {return _connectTod;}        
  time_t establishTod(void)       const {return _establishTod;}        
  time_t disconnectTod(void)      const {return _disconnectTod;}        
  time_t closeTod(void)           const {return _closeTod;}        
  time_t openedTime(void)         const {return _openedTime;}       
  time_t establishedTime(void)    const {return _establishedTime;}     

  MSExpBackoff& retryTime(void)            {return _retryTime;}  
  MSRegularTimer *retryTimer(void)         {return _retryTimer;} 
  int remoteNamelen(void)                  {return _remoteNamelen;}
  struct sockaddr *remoteName(void)        {return _remoteName;}     
  MSChannel *establishChannel(void)	   {return _establishChannel;}

  // callback entry points
  void doReadCall(void);
  void doWriteCall(void);
  void doConnectCall(void);
  void writeResetCall(void);
  void processEstablish(void);
  
  virtual void doRead(void);
  virtual int doWrite(void);
  virtual int doWrite(MSBoolean);
  virtual void doConnect(void);
  virtual void writeReset(void);
  virtual void cleanup(void);
  virtual MSBoolean establish(void);
  virtual MSBoolean setup(void);
  virtual void acknowledge(void);

  virtual void doRetry(void);
  virtual void doOpen(void);
  virtual void doClose(void);

  void createRetryTimer(time_t);
  void removeRetryTimer(void);
  void initRetryTimer(void);
  void createEstablishChannel(void);
  void removeEstablishChannel(void);
  void setBlockingMode(int fd_);

public:
  MSConnection(const char *,int,Retry,int,int,int,int,int);
  virtual ~MSConnection(void);

  void retry(MSConnection::Retry retry_) {_retry=retry_;}

  int fd(void)                    const {return _fd;}
  MSConnection::Retry retry(void) const {return _retry;}
  const MSString& name(void)      const {return _name;}          
  unsigned openCount(void)        const {return _openCount;}        
  unsigned connectCount(void)     const {return _connectCount;}      
  unsigned disconnectCount(void)  const {return _disconnectCount;}      
  unsigned establishCount(void)   const {return _establishCount;}        
  unsigned closeCount(void)       const {return _closeCount;}       
  MSBoolean isEstablished(void)   const {return _established;}
  MSBoolean tcpNoDelay(void)      const {return _tcpNoDelay;}
  void tcpNoDelay(MSBoolean);	 

  void open(void);
  void close(void);
};

#endif 
