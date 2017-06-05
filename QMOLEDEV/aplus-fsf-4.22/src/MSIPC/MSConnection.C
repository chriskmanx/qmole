///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSIPC/MSConnection.H>
#include <MSTypes/MSMethodCallback.H>
#include <MSTypes/MSMessageLog.H>

#include <errno.h>
#include <unistd.h>

#if defined(__NetBSD__)
#include <sys/types.h>
#endif

#include <netdb.h>
#include <netinet/tcp.h>

#if defined(MS_USE_FCNTL_BLOCKING)
#include <fcntl.h>
#else
#include <sys/ioctl.h>
#endif // MS_USE_FCNTL_BLOCKING

MSConnection::MSConnection(const char *name_,int pri_,MSConnection::Retry retry_,
			   int firstRetry_,
			   int lastRetry_,int domain_,int type_,int protocol_) :
_retryTime(firstRetry_,lastRetry_),
_name((name_==0)?"<UNKNOWN>":name_),
_pri(pri_),
_retry(retry_),
_domain(domain_),
_type(type_),
_protocol(protocol_),
_retryTimer(0),
_fd(-1),
_remoteNamelen(0),
_remoteName((struct sockaddr *)(0)),
_establishChannel(0),
_openTod((time_t)(0)),
_connectTod((time_t)(0)),
_establishTod((time_t)(0)),
_disconnectTod((time_t)(0)),
_closeTod((time_t)(0)),
_openedTime((time_t)(0)),
_established(MSFalse),
_establishedTime((time_t)(0)),
_openCount((unsigned)(0)),
_connectCount((unsigned)(0)),
_establishCount((unsigned)(0)),
_disconnectCount((unsigned)(0)),
_closeCount((unsigned)(0))
{
  _createTod=todsec();
}

MSConnection::~MSConnection(void)
{}

void MSConnection::doReadCall(void)
{ doRead();} 
void MSConnection::doWriteCall(void)
{ doWrite();} 
void MSConnection::doConnectCall(void)
{ doConnect();} 
void MSConnection::writeResetCall(void)
{ writeReset();}
void MSConnection::open(void)
{ doOpen(); }
void MSConnection::close(void)
{ doClose();}

void MSConnection::initRetryTimer(void)
{
  _retryTimer=0;
}

void MSConnection::createRetryTimer(time_t rt_)
{
  _retryTimer=new MSRegularTimer(rt_,0,new MSMethodCallback<MSConnection>(this,&MSConnection::open));
}

void MSConnection::removeRetryTimer()
{
  // retry timer can only be removed if it did not expired yet.
  if (_retryTimer) delete _retryTimer;
  _retryTimer=0;
}

void MSConnection::doOpen(void)
{
  if (fd()<0)
   {
     int cfd;
     initRetryTimer();

     if ((cfd=socket(domain(),type(),protocol()))<0)
      {
        MSMessageLog::warningMessage("MSConnection::open(%s) : error: socket()\n",name().string()); 
        close();
        return;
      }
     _openTod=todsec();
     _openCount++;
     
     MSChannel::fdsfresh(fd());
     _fd=cfd;
     setBlockingMode(_fd);

     if (setup()==MSFalse)                     { close(); return; }
     if (remoteName()==(struct sockaddr *)(0)) { close(); return; }
     
     if (connect(fd(),remoteName(),remoteNamelen())<0)
      {
#if defined(MS_WINSOCK)
	int err=WSAGetLastError();
	if(err==WSAEWOULDBLOCK)
#else
	if (errno==EINPROGRESS)
#endif
	 {
	   _connectTod=todsec();
	   _connectCount++;
           createEstablishChannel();
	   return;
	 }
#if defined(MS_WINSOCK)
	if (err != WSAEISCONN)
#else
	if (errno!=EISCONN) 
#endif
	 {
           MSMessageLog::warningMessage("MSConnection::open(%s): error: Connect(%d)\n",name().string(),fd());
	   close();
	   return;
	 }
        // we treat EISCONN as though the connection succeeded 
      }
     _connectTod=todsec();
     _connectCount++;
     if (establish()==MSTrue) acknowledge();
   }
}

void MSConnection::doClose(void)
{
  if (fd()<0)
   {
     removeRetryTimer();
   }
  else
   {
     if (isEstablished()==MSTrue)
      {
        _retryTime.reset();
	_established=MSFalse;
	cleanup();
	_disconnectTod=todsec();
	_disconnectCount++;
	_establishedTime+=(disconnectTod()-establishTod());
      }
     else
      {
        delete _establishChannel;
	_establishChannel=0;
      }
     MSChannel::fdsfresh(fd());
#if defined(MS_WINSOCK)
     if (::closesocket(fd())==SOCKET_ERROR)
#else
     if (::close(fd())<0)
#endif
      {
        MSMessageLog::warningMessage("MSConnection::close(%s) : warning: close(%d)\n",name().string(),fd());
      }
     _fd=-1;
     _closeTod=todsec();
     _closeCount++;
     _openedTime+=(closeTod()-openTod());
   }
  if (_remoteName!=0) delete [] _remoteName;

  _remoteName=(struct sockaddr *)(0);
  _remoteNamelen=0;
  if (retry()==MSConnection::Yes) doRetry();
}

void MSConnection::doRetry()
{
  time_t rt=(time_t)_retryTime.value();
  MSMessageLog::debugMessage("MSConnection::close(%s) : note: retrying in %d seconds\n",name().string(),rt);
  createRetryTimer(rt);
  _retryTime.backoff();
}

void MSConnection::createEstablishChannel(void)
{
  _establishChannel=new MSChannel(name().string(),fd(),
                                  priority(),MSChannel::Write,new MSMethodCallback<MSConnection>
                                  (this,&MSConnection::processEstablish));
  _establishChannel->enable();
}

void MSConnection::removeEstablishChannel(void)
{
  delete _establishChannel;
  _establishChannel=0;
}

void MSConnection::setBlockingMode(int fd_)
{

#if defined(MS_USE_FCNTL_BLOCKING)
  int fdFlags=fcntl(fd_,F_GETFL,0);
  fdFlags|=O_NONBLOCK;
  if (fcntl(fd(),F_SETFL,fdFlags)<0)
#else
#if defined(MS_WINSOCK)
  unsigned long toggle=1;
  if (ioctlsocket(fd(),FIONBIO,&toggle)<0)
#else
  int toggle=1;
  if (ioctl(fd(),FIONBIO,(caddr_t)&toggle)<0)
#endif
#endif 
   {
     MSMessageLog::infoMessage
       ("MSConnection::setBlockingMode(%s):Error seting %d fd to non blocking mode",
        name().string(),fd_);
   }
}

void MSConnection::processEstablish(void)
{
  removeEstablishChannel();
  if (establish()==MSTrue) acknowledge();
}

void MSConnection::acknowledge(void)
{
  if (isEstablished()==MSFalse)
   {
     _establishTod=todsec();
     _establishCount++;
     _established=MSTrue;
   }
}

void MSConnection::cleanup(void)
{
  MSMessageLog::debugMessage("MSConnection: cleanup not defined\n");
}

MSBoolean MSConnection::establish(void)
{
  MSMessageLog::debugMessage("MSConnection: establish not defined\n");
  return MSFalse;
}

static long setNoDelayOnFd(int fd_,int val_)
{
  struct protoent *ProtoEnt;
  MSMessageLog::debugMessage("MSConnection: setNoDelayOnFd(%d,%d)\n",fd_,val_);
  char sockname[16]; int socklen=14;
  if((ProtoEnt=getprotobyname("tcp"))==NULL) 
   {
     MSMessageLog::warningMessage("MSConnection: getprotobyname failed\n");
     return -1;
   }
#if defined(HAVE_SOCKLEN_T)
  if (getpeername(fd_,(struct sockaddr *)sockname, (socklen_t *)&socklen))
#else
  if (getpeername(fd_,(struct sockaddr *)sockname, &socklen))
#endif
  {
    MSMessageLog::warningMessage("MSConnection: getpeername failed: errno=%d\n",errno);
    return -1;
  }
  sockname[socklen]='\0';
  int rc=setsockopt(fd_,ProtoEnt->p_proto,TCP_NODELAY,(char *)(&val_),sizeof(int));
  if(rc>=0) return 0;
  MSMessageLog::warningMessage("MSConnection: Error setting nodelay: errno=%d\n",errno);
  return -2;
}

void MSConnection::tcpNoDelay(MSBoolean tcpNoDelay_)
{
  if(tcpNoDelay_!=_tcpNoDelay)
   {
     if(fd()==-1||setNoDelayOnFd(fd(),tcpNoDelay_==MSTrue?1:0)==0)
      {
        _tcpNoDelay=tcpNoDelay_; // set on if connection is not available yet as well
      }
   }
}

// virtual methods that should be defined by
// a subclass to deal with the specifics of the
// connection.
MSBoolean MSConnection::setup(void)
{ return MSTrue; }

void MSConnection::doRead(void)
{}
int MSConnection::doWrite(void)
{ return MSTrue; }
int MSConnection::doWrite(MSBoolean)
{ return MSTrue; }
void MSConnection::doConnect(void)
{}
void MSConnection::writeReset(void)
{}










