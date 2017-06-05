#ifndef MSListenerHEADER
#define MSListenerHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <MSTypes/MSDefines.H>
#include <MSTypes/MSString.H>
#include <MSIPC/MSChannel.H>
#include <MSIPC/MSRegularTimer.H>
#include <MSIPC/MSExpBackoff.H>
#include <MSIPC/MSTv.H>

class MSIPCExport MSListener
{
public:
  enum LRetry{No=0,Yes=1};
  typedef void (*AcceptCallback)(int,const struct sockaddr *,int,const struct sockaddr *,int,void *);

protected:
// configuration information 
  MSString           _name;          // MSListener's name 
  int                _pri;           // connection accept priority 
  MSListener::LRetry _retry;         // retry listening after failure 
  int                _domain;        // socket address domain 
  int                _type;          // socket type 
  int                _protocol;      // socket protocol 
  int                _remoteNamelen; // maximum remote name len on accept 

// state information 
  MSExpBackoff          _retryTime;          // retry time limit with back-off 
  int                   _fd;                 // file descriptor 
  int                   _localNamelen;       // local name length 
  struct sockaddr      *_localName;          // local name 
  struct sockaddr      *_remoteName;         // remote name 
  MSRegularTimer       *_retryTimer;         // retry timer 
  MSChannel	       *_acceptChannel;

// administrative information 
  time_t   _createTod;        // time MSListener was created
  time_t   _openTod;          // time MSListener was opened 
  time_t   _listenTod;        // time MSListener started listening 
  time_t   _acceptTod;        // time of last accept 
  time_t   _deafTod;          // time of MSListener stopped listening 
  time_t   _closeTod;         // time of last close 
  time_t   _openedTime;       // seconds opened in prev instances 
  time_t   _listenedTime;     // seconds listened in prev instances 
  unsigned _openCount;        // number of times opened 
  unsigned _listenCount;      // number of times listening 
  unsigned _acceptCount;      // number of connections accepted 
  unsigned _deafCount;        // number of times stopped listening 
  unsigned _closeCount;       // number of times closed 

  AcceptCallback  _acceptCB;
  void		 *_acceptCBData;

  inline int priority(void) const;
  inline int domain(void) const;
  inline int type(void) const;
  inline int protocol(void) const;
  inline int remoteNamelen(void) const;

  inline int fd(void) const;
  inline int localNamelen(void) const;

  inline MSExpBackoff& retryTime(void);
  inline MSRegularTimer *retryTimer(void);
  inline struct sockaddr *localName(void);
  inline struct sockaddr *remoteName(void);
  inline MSChannel *acceptChannel(void);

  inline time_t createTod(void) const;
  inline time_t openTod(void) const;
  inline time_t listenTod(void) const;
  inline time_t acceptTod(void) const;
  inline time_t deafTod(void) const;
  inline time_t closeTod(void) const;
  inline time_t openedTime(void) const;
  inline time_t listenedTime(void) const;
  inline unsigned openCount(void) const;
  inline unsigned listenCount(void) const;
  inline unsigned acceptCount(void) const;
  inline unsigned deafCount(void) const;
  inline unsigned closeCount(void) const;

  void processAccept(void);
  void setBlockingMode(int fd_);
  void createRetryTimer(time_t);
  void removeRetryTimer(void);
  void initRetryTimer(void);
  void createAcceptChannel(void);
  void removeAcceptChannel(void);
  
  virtual void doRetry(void);
  virtual void doOpen(void);
  virtual void doClose(void);

public:
  MSListener(const char *,int,LRetry,int,int,int,int,int);
  virtual ~MSListener(void);

  inline const MSString& name(void) const;
  inline MSListener::LRetry retry(void) const;
  inline void retry(MSListener::LRetry retry_);

  void acceptCallback(AcceptCallback,void *);
  inline AcceptCallback acceptCallback(void);
  inline void *acceptCallbackData(void);

  void open(void);
  void close(void);
  
  virtual MSBoolean establish(void);
  virtual MSBoolean publish(void);
  virtual void acceptNotify(int,const struct sockaddr *,int,const struct sockaddr *,int);
};

inline void MSListener::retry(MSListener::LRetry retry_) { _retry=retry_;}

inline const MSString& MSListener::name(void)     const {return _name;}
inline int MSListener::priority(void)             const {return _pri;}
inline MSListener::LRetry MSListener::retry(void) const {return _retry;}
inline int MSListener::domain(void)               const {return _domain;}
inline int MSListener::type(void)                 const {return _type;}
inline int MSListener::protocol(void)             const {return _protocol;}
inline int MSListener::remoteNamelen(void)        const {return _remoteNamelen;}
inline int MSListener::fd(void)                   const {return _fd;}
inline int MSListener::localNamelen(void)         const {return _localNamelen;}
inline time_t MSListener::createTod(void)         const {return _createTod;}
inline time_t MSListener::openTod(void)           const {return _openTod;}
inline time_t MSListener::listenTod(void)         const {return _listenTod;}
inline time_t MSListener::acceptTod(void)         const {return _acceptTod;}
inline time_t MSListener::deafTod(void)           const {return _deafTod;}
inline time_t MSListener::closeTod(void)          const {return _closeTod;}
inline time_t MSListener::openedTime(void)        const {return _openedTime;}
inline time_t MSListener::listenedTime(void)      const {return _listenedTime;}
inline unsigned MSListener::openCount(void)       const {return _openCount;}
inline unsigned MSListener::listenCount(void)     const {return _listenCount;}
inline unsigned MSListener::acceptCount(void)     const {return _acceptCount;}
inline unsigned MSListener::deafCount(void)       const {return _deafCount;}
inline unsigned MSListener::closeCount(void)      const {return _closeCount;}

inline MSExpBackoff& MSListener::retryTime(void)        {return _retryTime;}
inline MSRegularTimer *MSListener::retryTimer(void)     {return _retryTimer;}
inline struct sockaddr *MSListener::localName(void)     {return _localName;}
inline struct sockaddr *MSListener::remoteName(void)    {return _remoteName;}
inline MSChannel *MSListener::acceptChannel(void)	{return _acceptChannel;}

inline MSListener::AcceptCallback  MSListener::acceptCallback(void) {return _acceptCB;}
inline void *MSListener::acceptCallbackData(void) {return _acceptCBData;}

#endif 
