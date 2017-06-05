#ifndef MSProtocolConnectionIMPLEMENTATION
#define MSProtocolConnectionIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <errno.h>
#include <unistd.h>
#include <math.h>

#include <stdlib.h>
#include <stdarg.h>

#include <MSTypes/MSMethodCallback.H>
#include <MSIPC/MSProtocolConnection.H>
#include <MSTypes/MSMessageLog.H>

#if defined(MS_NEED_SYS_NERR_DECLARATION)
extern "C" int sys_nerr;
extern "C" char* sys_errlist[];
#endif

template <class Type>
MSProtocolConnection<Type>::MSProtocolConnection(const char *name_,
                           const char *host_,int port_) :
MSConnection(name_,0,MSConnection::Yes,1,16,AF_INET,SOCK_STREAM,0)
{
  init();
  _hostPort.set(host_,port_);
  _name=name_;
}

template <class Type>
MSProtocolConnection<Type>::MSProtocolConnection(const char *name_,
                           const MSString &serviceName_) :
MSConnection(name_,0,MSConnection::Yes,1,16,AF_INET,SOCK_STREAM,0)
{
  init();
  _service.establish(serviceName_);
  _hostPort.set(_service.host(),_service.port());
  _name=name_;
}

template <class Type>
MSProtocolConnection<Type>::MSProtocolConnection(const char *name_,int fd_) :
MSConnection(name_,0,MSConnection::No,1,16,AF_INET,SOCK_STREAM,0)
{
  init();
  _fd=fd_;
  _name=name_;
  if (establish()==MSTrue) acknowledge();
}

template <class Type>
void MSProtocolConnection<Type>::init(void)
{
  _readChannel=0;
  _writeChannel=0;
  _headBuffer=0;
  _readBuffer=0;
  _state=0;
  _burstMode=MSFalse;
  _timer=0;
  _readCB=0;
  _sentCB=0;
  _connectCB=0;
  _resetCB=0;
  _readCBData=0;
  _sentCBData=0;
  _connectCBData=0;
  _resetCBData=0;
  _syncErrorBuffer=0;
}

template <class Type>
MSProtocolConnection<Type>::~MSProtocolConnection(void) 
{
  _retry=MSConnection::No;
  close();
  cleanup();
}

// subclasses should provide their own send and read methods
// based on the particualars of the data.
template <class Type>
int MSProtocolConnection<Type>::send(const Type&)
{ return 0; }

template <class Type>
void MSProtocolConnection<Type>::doRead(void)
{}

template <class Type>
void MSProtocolConnection<Type>::doReadBurst(void)
{}

template <class Type>
MSBoolean MSProtocolConnection<Type>::setup(void)
{
  if (service().isValid()==MSTrue)
   {
    if (service().isReady()==MSFalse)
    {
      service().establish();
      hostPort().set(service().host(),service().port());
    }
   }
  _remoteName=(struct sockaddr *)hostPort().sockaddr_in(_remoteNamelen);
  return (_remoteName!=0)?MSTrue:MSFalse;  
}

template <class Type>
MSBoolean MSProtocolConnection<Type>::establish(void)
{
  int kaf=1;
  struct sockaddr sockname;
  int socklen=sizeof(struct sockaddr);

  // check to see if connection is really there 
#if defined(HAVE_SOCKLEN_T)
  if (getpeername(fd(),(struct sockaddr *)&sockname,(socklen_t *)&socklen)) 
#else
  if (getpeername(fd(),(struct sockaddr *)&sockname,&socklen)) 
#endif
   {
     MSMessageLog::warningMessage("MSProtocolConnection: getpeername failed: %s\n",
                                  (errno<sys_nerr)?sys_errlist[errno]:"unknown error");
     close();
     return MSFalse;
   }

  _readChannel=new MSChannel(name().string(),fd(),0,MSChannel::Read,
			 new MSMethodCallback<MSProtocolConnection<Type> >(this,
				&MSProtocolConnection<Type>::doReadCall)); 
  _writeChannel=new MSChannel(name().string(),fd(),0,MSChannel::Write,
			 new MSMethodCallback<MSProtocolConnection<Type> >(this,
				&MSProtocolConnection<Type>::doWriteCall)); 
  _headBuffer=new MSBuffer;
  _readBuffer=new MSBuffer;

#if !defined(MS_NO_FCNTL)
  fcntl(fd(),F_SETFD,1);  // turn on close-on-exec
#endif
  int rc=setsockopt(fd(),SOL_SOCKET,SO_KEEPALIVE,(const char *)&kaf,sizeof(int));
  if(rc==-1) 
    return MSFalse;

  _timer=new MSRegularTimer(0,0,
	     new MSMethodCallback<MSProtocolConnection<Type> >
		    (this,&MSProtocolConnection<Type>::doConnectCall));
  return MSTrue;
}

template <class Type>
int MSProtocolConnection<Type>::readTheBuffer(MSBuffer *b_,int n_)
{
  int n; 
  if (isSet(MSProtocolConnection<Type>::Reset)==MSTrue) return 0;
  if ((n=b_->read(fd(),n_))<0) resetWithError(MSProtocolConnection<Type>::Read);
  else if (n>0) { set(MSProtocolConnection<Type>::Read); }
  return n;
}

template <class Type>
void MSProtocolConnection<Type>::readNotify(const Type &message_)
{
  if (readCallback()!=0) (*readCallback())(message_,readCallbackData());
}

template <class Type>
void MSProtocolConnection<Type>::sentNotify(int sent_)
{
  if (sentCallback()!=0) (*sentCallback())(sent_,sentCallbackData());
}

template <class Type>
void MSProtocolConnection<Type>::connectNotify(void)
{
  if (connectCallback()!=0) (*connectCallback())(connectCallbackData());
}

template <class Type>
void MSProtocolConnection<Type>::resetNotify(MSProtocolConnection<Type>::State state_)
{
  if (resetCallback()!=0) (*resetCallback())(state_,resetCallbackData());
}

template <class Type>
void MSProtocolConnection<Type>::sendTheBuffer(MSBuffer *b_)
{
  MSNodeItem *np=new MSNodeItem((void *)b_);
  np->insert(writeList()); // fifo for writing
}

template <class Type>
int MSProtocolConnection<Type>::writeTheBuffer(MSBuffer *b_,int n_)
{
  if (isSet(MSProtocolConnection<Type>::Reset)==MSTrue) return 0;
  int s=0,n=0;
  while ((n_>0)&&((n=b_->write(fd(),n_))>0)) { s+=n; n_-=n; }
  if (n<0) { resetWithError(MSProtocolConnection<Type>::Write); s=n;}
  return s;
}

template <class Type>
void MSProtocolConnection<Type>::doConnect(void)
{
  _timer=0;
  if (isSet(MSProtocolConnection<Type>::ReadPause)==MSFalse) readChannel()->enable();
  if (isSet(MSProtocolConnection<Type>::Reset)==MSTrue) unset(MSProtocolConnection<Type>::Reset);
  connectNotify();
}

template <class Type>
void MSProtocolConnection<Type>::writeReset(void)
{
  _timer=0;
  resetWithError(MSProtocolConnection<Type>::Write);
}

template <class Type>
void MSProtocolConnection<Type>::resetWithError(MSProtocolConnection<Type>::State state_)
{
  reset();
  resetNotify(state_);
}

template <class Type>
void MSProtocolConnection<Type>::cleanup(void)
{
  if (readChannel()!=0)  { delete _readChannel;  _readChannel=0; }
  if (headBuffer()!=0)   { delete _headBuffer;   _headBuffer=0; }
  if (readBuffer()!=0)   { delete _readBuffer;   _readBuffer=0; }
  if (writeChannel()!=0) { delete _writeChannel; _writeChannel=0; }
  if (_timer!=0) { delete _timer; _timer=0; }
  if(0!=_syncErrorBuffer){ delete _syncErrorBuffer; _syncErrorBuffer=0;}
  if (service().isValid()==MSTrue) { service().isReady(MSFalse); }

  MSNodeItem *hp=writeList();
  MSNodeItem *np;
  while((np=hp->next())!=hp)
   {
     delete (MSBuffer *)np->data();
     delete np;
   }
}

template <class Type>
void MSProtocolConnection<Type>::reset(void)
{
  set(MSProtocolConnection<Type>::Reset);
  close();
}

template <class Type>
int MSProtocolConnection<Type>::doWrite(void) { return doWrite(MSTrue); }

template <class Type>
int MSProtocolConnection<Type>::doWrite(MSBoolean sw_)
{
  if (isSet(MSProtocolConnection<Type>::Reset)==MSTrue) return 0;

  MSBoolean cont=MSTrue;
  int bytes=0,msgs=0;
  int c,s,n;
  MSNodeItem *hp=writeList();
  MSNodeItem *np;
  MSBuffer   *pBuffer;

  while (cont==MSTrue&&((np=hp->next())!=hp)&&isSet(MSProtocolConnection<Type>::WritePause)==MSFalse)
   {
     pBuffer=(MSBuffer *)np->data(); 
     c=pBuffer->put()-pBuffer->get(); 
     s=n=0;
     while ((c>0)&&((n=pBuffer->write(fd(),c))>0))
      {
	s+=n;
	c-=n;
	bytes+=n;
      }
     if (pBuffer->get()==pBuffer->put())
      {
        delete pBuffer;
	delete np;
        msgs++;
        unset(MSProtocolConnection<Type>::Write);
      }
     else 
      {
	cont=MSFalse;
        set(MSProtocolConnection<Type>::Write);
      }
     if (n<0)
      {
	_timer=new MSRegularTimer(0,0,
        new MSMethodCallback<MSProtocolConnection<Type> >
				  (this,&MSProtocolConnection<Type>::doWriteResetCall));
        set(MSProtocolConnection<Type>::Reset);
	if (sw_==MSTrue&&msgs!=0) { sentNotify(msgs); }
	return msgs;
      }
   }
  if (hp==hp->next()) writeChannel()->disable();
  if (sw_==MSTrue&&msgs!=0) { sentNotify(msgs); }
  return msgs;
}

template <class Type>
void MSProtocolConnection<Type>::
readCallback(MSProtocolConnection<Type>::ReadCallback pCallback_,void *pData_)
{
  _readCB=pCallback_;
  _readCBData=pData_;
}

template <class Type>
void MSProtocolConnection<Type>::
sentCallback(MSProtocolConnection<Type>::SentCallback pCallback_,void *pData_) 
{
  _sentCB=pCallback_;
  _sentCBData=pData_;
}

template <class Type>
void MSProtocolConnection<Type>::
connectCallback(MSProtocolConnection<Type>::ConnectCallback pCallback_,void *pData_) 
{
  _connectCB=pCallback_;
  _connectCBData=pData_;
}

template <class Type>
void MSProtocolConnection<Type>::
resetCallback(MSProtocolConnection<Type>::ResetCallback pCallback_,void *pData_) 
{
  _resetCB=pCallback_;
  _resetCBData=pData_;
}

template <class Type>
int MSProtocolConnection<Type>::syncError(int rc,const char *symbol_,const char *fmt_,...)
{
  va_list ap;

  if(0==_syncErrorBuffer) _syncErrorBuffer=new char[256];
  strcpy(_syncErrorBuffer,symbol_);
  va_start(ap,fmt_);
  (void)vsprintf(_syncErrorBuffer+20,fmt_,ap);
  va_end(ap);
  syncErrorReport();
  return rc;
}

template <class Type>
void MSProtocolConnection<Type>::syncErrorReport(void)
{
  MSMessageLog::infoMessage(_syncErrorBuffer+20);
}

template <class Type>
int MSProtocolConnection<Type>::doSyncWrite(void)
{
  if (isSet(MSProtocolConnection<Type>::Reset)==MSTrue) 
    return syncError(-1,"closed","Connection Not Open\n");;
  MSBoolean cont=MSTrue;
  int bytes=0;
  int c,s,n;
  MSNodeItem *hp=writeList();
  MSNodeItem *np;
  MSBuffer *pBuffer;
  while (cont==MSTrue&&((np=hp->next())!=hp))
   {
     pBuffer=(MSBuffer *)np->data(); 
     c=pBuffer->put()-pBuffer->get(); 
     s=n=0;
     while ((c>0)&&((n=pBuffer->write(fd(),c))>0))
      {
	s+=n;
	c-=n;
	bytes+=n;
      }
     if (pBuffer->get()==pBuffer->put())
      {
        delete pBuffer;
	delete np;
        unset(MSProtocolConnection<Type>::Write);
      }
     else
      {
	cont=MSFalse;
        set(MSProtocolConnection<Type>::Write);
      }
     if (n<0)
      {
	_timer=new MSRegularTimer(0,0,
                                  new MSMethodCallback<MSProtocolConnection<Type> >
				  (this,&MSProtocolConnection<Type>::doWriteResetCall));
        set(MSProtocolConnection<Type>::Reset);
	return -1;
      }
   }
  if (hp==hp->next())
   {
     if(writeChannel()->enabled()==MSTrue)
      {
        writeChannel()->disable();
      }
     return 1;
   }
  else
   {
     return 0;
   }
}

template <class Type>
int MSProtocolConnection<Type>::doSyncRead(Type &)
{ return 0; }

template <class Type>
int MSProtocolConnection<Type>::syncRead(Type &data_,double seconds_)
{
  return syncRead(data_,(int)(floor(seconds_)),
		  (int)(1000000.0*(seconds_-floor(seconds_))));
}

template <class Type>
int MSProtocolConnection<Type>::syncRead(Type &data_,int seconds_,int microseconds_,MSBoolean isAbsolute_)
{
  if (isSet(MSProtocolConnection<Type>::Reset)==MSTrue) 
    return syncError(-1,"closed","Connection Not Open.\n");
  struct timeval timeout, now, tvp;
  if (isAbsolute_==MSTrue)
  {
    if (microseconds_<0) 
      return syncError(-1,"timeval","Negative Absolute Time\n");
    tvp.tv_sec=seconds_;
    tvp.tv_usec=microseconds_;
  }
  else 
  {
    gettimeofday(&now,NULL);
    timeout.tv_sec=seconds_;
    timeout.tv_usec=microseconds_;
    tvsum(&now,&timeout,&tvp);
  }
  if(readChannel()==0)
    return syncError(-1,"nochan","No Read Channel\n");
  return syncReadSelectLoop(data_,&tvp);
}

template <class Type>
int MSProtocolConnection<Type>::syncSend(const Type &,double)
{ return syncError(-1,"bogus","syncSend Is Not Defined\n."); }

template <class Type>
int MSProtocolConnection<Type>::syncSend(const Type &,int,int,MSBoolean)
{ return syncError(-1,"bogus","syncSend Is Not Defined\n."); }

template <class Type>
int MSProtocolConnection<Type>::syncReadSelectLoop(Type &data_,
						   struct timeval *timeout_)
{
  int rc=0,result=0;
  struct timeval timeLeft, *tvp;

  if (timeout_ != (struct timeval *)0) 
  {
    tvp=&timeLeft;
    tvnorm(timeout_);
    tvdiff(timeout_,tod(),tvp);
    if (tvp->tv_sec<0||tvp->tv_usec<0) tvp->tv_sec=tvp->tv_usec=0;
  }
  else tvp=NULL;

  for(;;)
  {
    if(readChannel()==0) 
      return syncError(-1,"readchan","Lost Read Channel\n");
    rc=MSChannel::select(fd(),MSChannel::Read,tvp);
    if(rc<0)
    {
      if (EINTR==errno)
	return syncError(-1,"interrupt","select() received an interrupt\n");
      else if (EIO==errno)
	return syncError(-1,"fdsisset","unexpected event from select\n");
      else
	return syncError(-1,"select","select() returned %d, errno %d\n",
			 rc, errno);
    }
    else if (rc>0)
    {
      if ((result=doSyncRead(data_)) != 0 ) break;
    }
    if (tvp != (struct timeval *)0) 
    {
      tvdiff(timeout_,tod(),tvp);
      if (tvp->tv_sec<0||tvp->tv_usec<0) tvp->tv_sec=tvp->tv_usec=0;
      if (tvp->tv_sec==0 && tvp->tv_usec==0)
       {
         return syncError(0,"timeout","Sync read loop timed out...\n");
       }
    }
  }
  return result;
}

template <class Type>
int MSProtocolConnection<Type>::syncWriteSelectLoop(struct timeval *timeout_)
{
  long result=0;
  long rc=0;
  struct timeval timeLeft, *tvp;

  if (timeout_!=(struct timeval *)0) 
  {
    tvp=&timeLeft;
    tvnorm(timeout_);
    tvdiff(timeout_,tod(),tvp);
    if (tvp->tv_sec<0||tvp->tv_usec<0) tvp->tv_sec=tvp->tv_usec=0;
  }
  else tvp=NULL;
  
  for (;;) 
  {
    if(writeChannel()==0) 
      return syncError(-1,"writechan","Lost Write Channel\n");
    rc=MSChannel::select(fd(),MSChannel::Write,tvp);
    if(rc<0)
    {
      if (errno==EINTR) 
	return syncError(-1,"interrupt", "select() received an interrupt\n");
      else if (errno==EIO) 
	return syncError(-1,"fdsisset","unexpected event broke select()\n");
      else 
	return syncError(-1,"select",
			 "select() returned %d, errno %d\n",rc,errno);
    }
    else if(rc>0)
    {
      if ((result=doSyncWrite()) < 0) 
	return syncError(-1,"syncwrite","reset during sync write\n");
      else if (result>0) break;
    }
    if (tvp != NULL)
    {
      tvdiff(timeout_,tod(),tvp);
      if (tvp->tv_sec<0||tvp->tv_usec<0) tvp->tv_sec=tvp->tv_usec=0;
      if (tvp->tv_sec==0 && tvp->tv_usec==0)
       {
         if(writeChannel()->enabled()==MSFalse)
          {
            writeChannel()->enable();
          }
         return syncError(0,"timeout", "Sync write loop timed out...\n");
       }
    }
  }
  return result;
}

#endif
  
  




