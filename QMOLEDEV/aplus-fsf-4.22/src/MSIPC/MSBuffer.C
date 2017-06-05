///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <errno.h>
#include <sys/socket.h>
#include <MSTypes/MSEnum.H>
#include <MSIPC/MSBuffer.H>
#include <MSTypes/MSMessageLog.H>
#include <MSTypes/MSException.H>


MSBuffer::MSBuffer(int l_)
{
  _buffer=_minofbuffer=_get=_put=_maxofbuffer=0;	
  reserve(l_);
}

void MSBuffer::reserve(int add_)
{
  int def;		
  if ((def=add_-(_maxofbuffer-_put))>0)
   {
     int len=_put-_get;
     int off=_get-_minofbuffer;
     
     if (def>off) // deficiency is more than the offset
      {
	int siz=_maxofbuffer-_minofbuffer;
	int req=siz+def;
	
	siz+=siz/2;
	if (siz<req) siz=req;
        char *tp=_minofbuffer;
	_minofbuffer=new char[siz];
        memcpy(_minofbuffer,tp,(_maxofbuffer-tp));
        if (tp!=0) delete [] tp;
	_get=_minofbuffer+off;
	_put=_get+len;
	_maxofbuffer=_minofbuffer+siz;
      }
     else
      {
        memcpy(_minofbuffer,_get,len);
	_get-=off;
	_put-=off;
      }
   }
  return;
}

int MSBuffer::read(int fd_,int nby_)
{
  register int rby;
  if (nby_<=0) return 0;
  if (_maxofbuffer-_put<nby_) reserve(nby_);
#if defined(MS_WINSOCK)
  if ((rby=::recv(fd_,_put,nby_,0))<0)
  {
	  int err= WSAGetLastError();
	  if(err==WSAEINTR||err==WSAEWOULDBLOCK) return 0;
	  return -1;
  }
#else
  if ((rby=::read(fd_,_put,nby_))<0)
   {
     if (errno==EINTR||errno==EWOULDBLOCK||errno==EAGAIN) return 0;
     return -1;
   }
#endif
  if (rby==0) // end of file 
   {
     errno=EPIPE;
     return -1;
   }
  if (rby>nby_)
   {
     MSMessageLog::criticalMessage("MSBuffer::read: abort: read(%d): too many bytes: %d>%d\n",
                                  fd_,rby,nby_);
     MSTKTHROWEXCEPTION(MSExceptionError("MSBuffer::read too many bytes"));
   }
  _put+=rby;
  return rby;
}

int MSBuffer::write(int fd_,int nby_)
{
  int wby;
  if (nby_<=0) return 0;
  if ((wby=_put-_get)<nby_) nby_=wby;
#if defined(MS_WINSOCK)
  if ((wby=::send(fd_,_get,nby_,0))<0)
  {
	  int err= WSAGetLastError();
	  if(err==WSAEINTR||err==WSAEWOULDBLOCK) return 0;
	  return -1;
  }
#else
  if ((wby=::write(fd_,_get,nby_))<0)
   {
     if (errno==EINTR||errno==EWOULDBLOCK||errno==EAGAIN) return 0;
     return -1;
   }
#endif
  if (wby>nby_)
   {
     MSMessageLog::criticalMessage("MSBuffer::write: abort: write(%d): too many bytes: %d>%d\n",
                                  fd_,wby,nby_);
     MSTKTHROWEXCEPTION(MSExceptionError("MSBuffer::write too many bytes"));
   }
  _get+=wby;
  return wby;
}

int MSBuffer::buffToBuff(MSBuffer *sp,MSBuffer *dp,int n)
{
  if (sp!=0 && dp!=0)
   {
     if (n>sp->put()-sp->get()) n=sp->put()-sp->get();
     dp->stuff(sp->get(),n);
     sp->get(sp->get()+n);
     return n;
   }
  return -1;
}

void MSBuffer::stuff(const char *value_,int size_)
{
  if (size_>0)
   {
     if (_maxofbuffer-_put<size_) reserve(size_);
     memcpy(_put,(char *)value_,size_);
     _put+=size_;
   }
}

void MSBuffer::zero(int size_)
{
  if (size_>0)
   {
     if (_maxofbuffer-_put<size_) reserve(size_);
     memset(_put,0,size_);
     _put+=size_;
   }
}



