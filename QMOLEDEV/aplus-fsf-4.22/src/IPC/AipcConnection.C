///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
//
// AipcConnection
//
// This class is a sub-class of MSProtocolConnection, and intended to be
// an abstract class for all libIPC connection classes which use MStk
// compatible protocols.
// 

#include <a/development.h>
#ifdef __sgi /* need for param.h who includes signal.h */
#define _BSD_COMPAT
#endif

#include <errno.h>      

#if defined(__sgi) || defined(_AIX) || defined(SOLARIS_CSET) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#include <sys/types.h>
#endif

#if defined(_AIX) || defined (linux) || defined(__alpha) || defined(__APPLE__)
extern "C" {
#include <sys/ioctl.h>
}
#else
extern "C" {
#include <sys/filio.h>
}
#endif

#if defined(__NetBSD__)
#include <sys/types.h>
#endif

#include <netdb.h>
#include <netinet/tcp.h>

// extern "C" int ioctl(int,unsigned int,long *);

#include <AipcConnection.H>
#include <MSIPC/MSProtocolConnection.C>

// Manually instantiate our template

#if defined(__edgfe)
  #pragma instantiate MSProtocolConnection<A>
#elif defined(_AIX) || defined(_MSC_VER)
  template MSProtocolConnection<A>;
#endif

// Static members

I AipcConnection::ServiceType=1;

AipcConnection::AipcConnection(C *name_,C *host_,I port_,A cbfunc_)
  : MSProtocolConnection<A>(name_,host_,port_),
    AipcService(cbfunc_)
{
  ipcWarn(0,"%t AipcConnection::AipcConnection(0)\n");
  init();
}  

AipcConnection::AipcConnection(C *serviceName_,A cbfunc_)
  : MSProtocolConnection<A>(serviceName_,(C*)0,-1),
    AipcService(cbfunc_)
{
  ipcWarn(0,"%t AipcConnection::AipcConnection(1)\n");
  init();
  AipcHostPortProtocol ahpp(serviceName_);
  hostPort().set(ahpp.host(),ahpp.port());
}

AipcConnection::AipcConnection(AipcHostPortProtocol &ahpp_,
				 A cbfunc_)
  : MSProtocolConnection<A>(ahpp_.service(),ahpp_.host(),ahpp_.port()),
    AipcService(cbfunc_)
{
  ipcWarn(0,"%t AipcConnection::AipcConnection(2)\n");
  init();
}

AipcConnection::AipcConnection(C *name_,int fd_,A cbfunc_,
			       AipcAttributes &attrs_)
  : MSProtocolConnection<A>(name_,fd_),
    AipcService(cbfunc_),
    _attrs(attrs_)
{
  ipcWarn(0,"%t AipcConnection::AipcConnection(3) fd=%d\n",fd_);
  init();
}

AipcConnection::~AipcConnection(void)
{
  ipcWarn(wrnlvl(),"%t AipcConnection::~AipcConnection\n");
  reset();
}

void AipcConnection::init(void)
{
  ipcWarn(0,"%t AipcConnection::init\n");
  initAttrs();
}

void AipcConnection::initAttrs(void)
{
  ipcWarn(0,"%t AipcConnection::initAttrs\n");
  if(_attrs.readPause())turnReadPauseOn();
  if(_attrs.writePause())turnWritePauseOn();
  if(_attrs.retry())turnRetryOn();
  if(_attrs.debug())turnDebugOn();
}

// M:Flags

static I setNoDelayOnFd(I handle_,int wrnlvl_,int fd_,int val_)
{
  ipcWarn(wrnlvl_,"%t setNoDelayOnFd(%d,%d)\n",fd_,val_);
  char sockname[16]; int socklen=14;
  struct protoent *ProtoEnt=(struct protoent *)0;
#if defined(APLUS_THREAD_SAFE_FUNCTIONS)
  struct protoent protoentStruct;
  char charBuf[1024];
#endif // APLUS_THREAD_SAFE_FUNCTIONS

  APLUS_GETPROTOBYNAME("tcp",&protoentStruct,charBuf,1024,ProtoEnt);

#if defined(HAVE_SOCKLEN_T)
  if (getpeername(fd_,(struct sockaddr *)sockname, (socklen_t *)&socklen))
#else
  if (getpeername(fd_,(struct sockaddr *)sockname, &socklen))
#endif
  {
#ifndef HAVE_STRERROR
    Warn("%t Getpeername failed on %d: %s\n",
	 handle_,(errno)?sys_errlist[errno]:"unknown error");
#else
    char *errstr=strerror(errno);
    Warn("%t Getpeername failed on %d: %s\n",
	 handle_,errstr ? errstr : "unknown error");
#endif
    R -1;
  }
  sockname[socklen]='\0';
  int rc=setsockopt(fd_,ProtoEnt->p_proto,TCP_NODELAY,(char *)(&val_),
		    sizeof(int));
  if(0<=rc)R 0;
  {
    static char fmt[]="\343 error setting nodelay %s: errno=%d\n";
    static char on[]="on";
    static char off[]="off";
    Warn(fmt,val_?on:off,errno);
  }
  R -2;
}

void AipcConnection::turnNoDelayOn(void) 
{
  if(MSFalse==isNoDelay()&&
     (-1==fd()||0==setNoDelayOnFd(handle(),wrnlvl(),fd(),1)))
    _attrs.noDelay(MSTrue);
}
void AipcConnection::turnNoDelayOff(void)
{
  if(MSTrue==isNoDelay()&&
     0==(-1==fd()||setNoDelayOnFd(handle(),wrnlvl(),fd(),0)))
    _attrs.noDelay(MSFalse);
}

void AipcConnection::turnReadPauseOn(void) 
{
  if(readChannel())readChannel()->disable();
  set(MSProtocolConnection<A>::ReadPause);
}
void AipcConnection::turnReadPauseOff(void)
{
  if(readChannel())readChannel()->enable();
  unset(MSProtocolConnection<A>::ReadPause);
}

void AipcConnection::turnWritePauseOn(void)
{
  if(writeChannel())writeChannel()->disable();
  set(MSProtocolConnection<A>::WritePause);
}
void AipcConnection::turnWritePauseOff(void)
{
  if(writeChannel())writeChannel()->enable();
  unset(MSProtocolConnection<A>::WritePause);
}


// M:Attribute interface

A AipcConnection::setableAttrlist(void)
{
  ipcWarn(wrnlvl(),"%t AipcConnection::setableAttrlist\n");
  return (A)ic(_attrs.setableAttrs());
}

A AipcConnection::getableAttrlist(void)
{
  ipcWarn(wrnlvl(),"%t AipcConnection::getableAttrlist\n");
  int i,idx=0;
  A sattrs=_attrs.setableAttrs();
  A nsattrs=_attrs.nonsetableAttrs();
  A z=gv(Et,sattrs->n+nsattrs->n);
  for (i=0;i<sattrs->n;++i)z->p[idx++]=sattrs->p[i];
  for (i=0;i<nsattrs->n;++i)z->p[idx++]=nsattrs->p[i];
  R z;
}

MSBoolean AipcConnection::setAttr(C *attr_,A aval_)
{
  ipcWarn(wrnlvl(),"%t AipcConnection::setAttr\n");
  int idx=_attrs.setAttrIndex(attr_);
  I ival;
  switch(idx) {
    CSBOOL(0,aval_,turnNoDelayOn,turnNoDelayOff);
    CSBOOL(1,aval_,turnReadPauseOn,turnReadPauseOff);
    CSBOOL(2,aval_,turnWritePauseOn,turnWritePauseOff);
    CS(3,ItCHK(aval_);_attrs.readPriority(ival));
    CS(4,ItCHK(aval_);_attrs.writePriority(ival));
    CS(5,ItCHK(aval_);_attrs.readBufsize(ival));
    CS(6,ItCHK(aval_);_attrs.writeBufsize(ival));
    CSBOOL(7,aval_,turnRetryOn,turnRetryOff);
    CS(8,_attrs.clientData((A)ic(aval_)));
    CSBOOL(9,aval_,turnDebugOn,turnDebugOff);
  default: R MSFalse;
  }
  R MSTrue;
}

A AipcConnection::getAttr(C *attr_)
{
  ipcWarn(wrnlvl(),"%t AipcConnection::getAttr\n");
  int idx=_attrs.setAttrIndex(attr_);
  if(-1!=idx)
  {
    switch(idx) {
      CSR(0,R gi(isNoDelay()?1:0));
      CSR(1,R gi(isReadPause()?1:0));
      CSR(2,R gi(isWritePause()?1:0));
      CSR(3,R gi(_attrs.readPriority()););
      CSR(4,R gi(_attrs.writePriority()););
      CSR(5,R gi(_attrs.readBufsize()););
      CSR(6,R gi(_attrs.writeBufsize()););
      CSR(7,R gi(isRetry()?1:0));
      CSR(8,R (A)ic(_attrs.clientData()));
      CSR(9,R gi(isDebug()?1:0));
    default: R aplus_nl;
    }
  } else {
    int idx=_attrs.nonsetAttrIndex(attr_);
    if(-1!=idx)
    {
      switch(idx) {
	CSR(0,R gi(fd())); /* fd */
	CSR(1,R gi(port())); /* port */
	CSR(2,R writeQueueStatus());
	CSR(3,R readQueueStatus());
	CSR(4,R gi(_attrs.listener()));
      default: R aplus_nl;
      }
    }
    else R aplus_nl; /* subclasses call parent class here */
  }
} 


// M:Callbacks

void AipcConnection::readNotify(const A &aobj_)
{
  ACallback("read",aobj_);
}

void AipcConnection::doConnect(void)
{
  if(isNoDelay())setNoDelayOnFd(handle(),wrnlvl(),fd(),1);
  MSProtocolConnection<A>::doConnect();
}

void AipcConnection::connectNotify(void)
{
  turnInResetOff();
  A adata=gi(_attrs.listener());
  ACallback("opened",adata);
  dc(adata);
}

void AipcConnection::sentNotify(int nmsgs_)
{
  A adata=gi(nmsgs_);
  ACallback("sent",adata);
  dc(adata);
}

void AipcConnection::resetNotify(MSProtocolConnection<A>::State errstate_)
{
  if (errstate_==MSProtocolConnection<A>::Read) resetNotify("read");
  else if (errstate_==MSProtocolConnection<A>::Write) resetNotify("write");
  else resetNotify("unknownState");
}

void AipcConnection::resetNotify(const C *errmsg_)
{
  A adata=gsym(errmsg_);
  ACallback("closed",adata);
  dc(adata);
}


// M:Other methods

void AipcConnection::initFromListener(void)
{
  ipcWarn(wrnlvl(),"%t AipcConnection::initFromListener\n");
  turnRetryOff();
}

void AipcConnection::reset(void)
{
  ipcWarn(wrnlvl(),"%t AipcConnection::reset\n");
  MSBuffer *db=readBuffer();
  if(db!=0)
  { 
    dc((A)(db->minofbuffer()));
    db->minofbuffer(0);
    db->clear();
  }
  MSProtocolConnection<A>::reset();
  turnInResetOn();
}

A AipcConnection::writeQueueStatus(void)
{
  ipcWarn(wrnlvl(),"%t AipcConnection::writeQueueStatus\n");
  MSNodeItem *np, *hp=writeList();
  int count;

  for(np=hp,count=0;hp!=(np=np->next());++count) ;
  R gvi(It,2,count,isInWrite()?1:0);
}

int AipcConnection::readFileLength(void)
{
  I slen=-1;

  if(-1==ioctl(fd(),FIONREAD,&slen)) {
#ifndef HAVE_STRERROR
    ipcWarn(wrnlvl(),"%t ioctl FIONREAD failed: %s\n", 
	  (errno<sys_nerr)?sys_errlist[errno]:"unknown error");
#else
    char *errstr=strerror(errno);
    ipcWarn(wrnlvl(),"%t ioctl FIONREAD failed: %s\n", 
	  (errstr) ? errstr : "unknown error");
#endif
    resetWithError("ioctl");
    R -1;
  }
  R slen;
}

A AipcConnection::readQueueStatus(void)
{
  ipcWarn(wrnlvl(),"%t AipcConnection::readQueueStatus\n");
  I len=readFileLength();
  R (-1==len)?aplus_nl:gvi(It,2,len,isInRead()?1:0);
}

/// Re-implementation of the async open to a synchronous version
int AipcConnection::openSync(int timeout_)
{
  // Cannot retry with sync connect
  turnRetryOff();
  
  if (fd()<0)
    {
      int cfd;
      initRetryTimer();
	  
      if ((cfd=socket(domain(),type(),MSConnection::protocol()))<0)
	{
	  MSMessageLog::warningMessage("MSConnection::open(%s) : error: socket()\n",name().string()); 
	  close();
	  return -1;
	}
      _openTod=todsec();
      _openCount++;
	  
      MSChannel::fdsfresh(fd());
      _fd=cfd;
      setBlockingMode(_fd);
	  
      if (setup()==MSFalse)                     { close(); return -1; }
      if (remoteName()==(struct sockaddr *)(0)) { close(); return -1; }
	  
      if (connect(fd(),remoteName(),remoteNamelen())<0)
	{
#if defined(MS_WINSOCK)
	  int err=WSAGetLastError();
	  if(err==WSAEWOULDBLOCK)
#else
	    if (errno==EINPROGRESS)
#endif
	      {
		// Now to ensure we really get connected, roughly in time
		// select the socket for write access
		timeval tvp;
		tvp.tv_sec = timeout_;
		tvp.tv_usec = 0;
		int rc = MSChannel::select(fd(), MSChannel::Write, &tvp);
		if (rc < 0) {
		  close();
		  return syncError(-1, "syncConnect", "select() returned %d, errno %d\n", rc, errno);
		};
		if (rc == 0) {
		  // Genuine timeout
		  close();
		  return -1;
		};
			  
	      }
#if defined(MS_WINSOCK)
	    else if (err != WSAEISCONN)
#else
	    else if (errno!=EISCONN) 
#endif
	      {
		MSMessageLog::warningMessage("MSConnection::open(%s): error: Connect(%d)\n",name().string(),fd());
		close();
		return -1;
	      }
	  // we treat EISCONN as though the connection succeeded 
	}
      _connectTod=todsec();
      _connectCount++;
      if (establish()==MSTrue) {
	acknowledge();
      } else {
	// This happens when connection actively fails (e.g. nothing on that port)
	close();
	return -1;
      }
    }
  return 0;
}
