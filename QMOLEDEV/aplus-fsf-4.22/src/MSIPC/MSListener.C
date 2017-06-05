///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSIPC/MSListener.H>
#include <MSTypes/MSMethodCallback.H>
#include <MSTypes/MSMessageLog.H>
#include <errno.h>
#include <unistd.h>

#if defined(MS_USE_FCNTL_BLOCKING)
#include <fcntl.h>
#else
#include <sys/ioctl.h>
#endif // MS_USE_FCNTL_BLOCKING


static const int DefaultRemoteNameLength=128;

MSListener::MSListener(const char *name_,int pri_,MSListener::LRetry retry_,int firstRetry_,
		       int lastRetry_,int domain_,int type_,int protocol_) :
_name((name_==0)?"<UNKNOWN>":name_),
_pri(pri_),
_retryTime(firstRetry_,lastRetry_),
_retry(retry_),
_domain(domain_),
_type(type_),
_protocol(protocol_),
_remoteNamelen(DefaultRemoteNameLength),
_remoteName((struct sockaddr *)(0)),
_retryTimer(0),
_fd(-1),
_localNamelen(0),
_localName((struct sockaddr *)(0)),
_acceptChannel(0),
_openTod((time_t)(0)),
_listenTod((time_t)(0)),
_acceptTod((time_t)(0)),
_deafTod((time_t)(0)),
_closeTod((time_t)(0)),
_openedTime((time_t)(0)),
_listenedTime((time_t)(0)),
_openCount((unsigned)(0)),
_listenCount((unsigned)(0)),
_acceptCount((unsigned)(0)),
_deafCount((unsigned)(0)),
_closeCount((unsigned)(0))
{
  _createTod=todsec();
}

MSListener::~MSListener(void)
{}

void MSListener::open(void)
{ doOpen(); }
void MSListener::close(void)
{ doClose();}

void MSListener::initRetryTimer(void)
{ _retryTimer=0; }

void MSListener::createRetryTimer(time_t rt_)
{
  _retryTimer=new MSRegularTimer(rt_,0,new MSMethodCallback<MSListener>(this,&MSListener::open));
}

void MSListener::removeRetryTimer()
{
  // retry timer can only be removed if it did not expired yet.
  if (_retryTimer) delete _retryTimer;
  _retryTimer=0;
}

void MSListener::doOpen(void)
{
  if (fd()<0)
   {
     int lfd;
     initRetryTimer();
     
     if ((lfd=socket(domain(),type(),protocol()))<0)
      {
        MSMessageLog::warningMessage("MSListener::open(%s): error: socket()\n",name().string());
	close();
        return;
      }
     _openTod=todsec();
     _openCount++;
     MSChannel::fdsfresh(lfd);
     _fd=lfd;

     setBlockingMode(_fd);

     int toggle=1;

     if (setsockopt(fd(),SOL_SOCKET,SO_REUSEADDR,(char *)(&toggle),sizeof(toggle))<0)
      {
        MSMessageLog::warningMessage("MSListener::open(%s): warning: setsockopt(%d,REUSEADDR)\n",name().string(),fd());
      }
     if (establish()==MSFalse)
      {
	close();
	return;
      }
     if (localName()==(struct sockaddr *)(0))
      {
	_localName=(struct sockaddr *)new char[localNamelen()];
        memset((char *)(localName()),0,localNamelen());
      }
     else
      {
	if (bind(fd(),localName(),localNamelen())<0)
	 {
           MSMessageLog::warningMessage("MSListener::open(%s): error: bind(%d)\n",name().string(),fd());
	   close();
	   return;
	 }
      }
     if (listen(fd(),5)<0)
      {
        MSMessageLog::warningMessage("MSListener::open(%s): error: listen(%d)\n",name().string(),fd());
	close();
	return;
      }
#if defined(HAVE_SOCKLEN_T)
     if (getsockname(fd(),localName(),(socklen_t *)&_localNamelen)<0)
#else
     if (getsockname(fd(),localName(),&_localNamelen)<0)
#endif
      {
        MSMessageLog::warningMessage("MSListener::open(%s): error: getsockname(%d)\n",name().string(),fd());
	close();
	return;
      }
     if (publish()==MSFalse)
      {
	close();
	return;
      }

     _listenTod=todsec();
     _listenCount++;
     createAcceptChannel();
     _retryTime.reset();
   }
}

void MSListener::doClose(void)
{
  if (fd()<0)
   {
     removeRetryTimer();
   }
  else
   {
     if (acceptChannel()!=0)
      {
        removeAcceptChannel();
        _deafTod=todsec();
	_listenedTime+=(deafTod()-listenTod());
      }
     delete [] _localName;
     _localName=(struct sockaddr *)(0);
     _localNamelen=0;
     MSChannel::fdsfresh(fd());
#if defined(MS_WINSOCK)
     if (::closesocket(fd())==SOCKET_ERROR)
#else
     if (::close(fd())<0)
#endif
      {
        MSMessageLog::warningMessage("MSListener::close(%s) : warning: close(%d)\n",name().string(),fd());
      }
     _fd=-1;
     _closeTod=todsec();
     _closeCount++;
     _openedTime+=(closeTod()-openTod());
   }
  if (retry()==MSListener::Yes) doRetry();
}

void MSListener::doRetry(void)
{
  time_t rt=(time_t)_retryTime.value();
  MSMessageLog::debugMessage("MSListener::close(%s) : note: retrying in %d seconds\n",
                             name().string(),rt);
  createRetryTimer(rt);
  _retryTime.backoff();
}


void MSListener::acceptNotify(int rfd,const struct sockaddr *rn,int rlen,const struct sockaddr *ln,int llen)
{
  if (acceptCallback()!=0)
        (*acceptCallback())(rfd,rn,rlen,ln,llen,acceptCallbackData());
}

void MSListener::processAccept(void)
{
  int rfd;
  int rlen=remoteNamelen();
  struct sockaddr *rname;
  struct sockaddr *lname;
  int llen=localNamelen();

  rname=(struct sockaddr *)new char[rlen];
#if defined(HAVE_SOCKLEN_T)
  if ((rfd=::accept(fd(),rname,(socklen_t *)&rlen))<0)
#else
  if ((rfd=::accept(fd(),rname,&rlen))<0)
#endif
   {
#if defined(MS_WINSOCK)
	int err=WSAGetLastError();
	if(err==WSAEWOULDBLOCK||err==WSAEINTR){}
	else if(err==WSAEMFILE)
#else
    if ((errno==EWOULDBLOCK)||(errno==EINTR)) {} // nothing to report 
     else if ((errno==EMFILE)||(errno==ENFILE)||(errno==ENXIO)||(errno==EIO))
#endif
      {
        MSMessageLog::warningMessage("MSListener::accept(%s): error: accept(%d)\n",name().string(),fd());
      }
     else
      {
        MSMessageLog::warningMessage("MSListener::accept(%s): error: accept(%d)\n",name().string(),fd());
	close();
      }
     delete [] rname;
     return;
   }
  lname=(struct sockaddr *)new char [llen];
#if defined(HAVE_SOCKLEN_T)
  if (getsockname(rfd,lname,(socklen_t *)&llen)<0)
#else
  if (getsockname(rfd,lname,&llen)<0)
#endif
   {
     MSMessageLog::warningMessage("MSListener::accept(%s): error: getsockname(%d)\n",name().string(),fd());
#if defined(MS_WINSOCK)
	 ::closesocket(fd());
#else
     ::close(fd());
#endif
     delete [] rname;
     delete [] lname;
     return;
   }
  _acceptTod=todsec();
  _acceptCount++;
  
  struct sockaddr *rn=(struct sockaddr *)new char[rlen];
  struct sockaddr *ln=(struct sockaddr *)new char[llen];
  memcpy((char *)rn,(char *)rname,rlen);
  memcpy((char *)ln,(char *)lname,llen);
  delete [] rname;
  delete [] lname;  
  acceptNotify(rfd,rn,rlen,ln,llen);
  delete [] rn;
  delete [] ln;  
}

void MSListener::setBlockingMode(int fd_)
{
#if defined(MS_USE_FCNTL_BLOCKING)
  int fdFlags=fcntl(fd(),F_GETFL,0);
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
   
   MSMessageLog::infoMessage("MSListener::open(%s): Error seting %d fd to non blocking mode",
                             name().string(),fd());
 }
}


void MSListener::createAcceptChannel(void)
{
  _acceptChannel=new MSChannel(name().string(),fd(),priority(),MSChannel::Read,
                               new MSMethodCallback<MSListener>(this,&MSListener::processAccept));
  _acceptChannel->enable();
}

void MSListener::removeAcceptChannel(void)
{
  delete _acceptChannel;
  _acceptChannel=0;
}

void MSListener::acceptCallback(MSListener::AcceptCallback pCallback_,void *pData_)
{
  _acceptCB=pCallback_;
  _acceptCBData=pData_;
}

MSBoolean MSListener::establish(void)
{
  MSMessageLog::debugMessage("MSListener::establish: not defined\n");
  return MSFalse;
}

MSBoolean MSListener::publish(void)
{ return MSTrue; }









