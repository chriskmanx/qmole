///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSIPC/MSRawConnection.H>
#include <MSTypes/MSMessageLog.H>
#include <math.h>
static const int MaxReadSize=8192;

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSIPC/MSProtocolConnection.C>

#if defined (MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSProtocolConnection<MSString>
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSProtocolConnection<MSString>)
#endif

#if defined (MS_STD_TEMPLATE_INSTANTIATION)
template class MSProtocolConnection<MSString>;
#endif

#if defined (MS_VC_TEMPLATE_INSTANTIATION)
template MSProtocolConnection<MSString>;
#endif

#endif  //MSTK_MANUAL_INSTANTIATION

MSRawConnection::MSRawConnection(const char *name_, const char *host_, int port_) :
MSProtocolConnection<MSString>(name_, host_, port_)
{
}

MSRawConnection::MSRawConnection(const char *name_, const MSString &serviceName_) :
MSProtocolConnection<MSString>(name_, serviceName_)
{
}

MSRawConnection::MSRawConnection(const char *name_, int fd_) :
MSProtocolConnection<MSString>(name_, fd_) 
{
}

MSRawConnection::~MSRawConnection(void)
{
}

void MSRawConnection::doRead(void)
{
  int n;
  MSBuffer *db=headBuffer();
  if (isSet(MSProtocolConnection<MSString>::Reset)==MSTrue)   return;
  if((n=readTheBuffer(db,MaxReadSize))<0) return;

  if((n=db->put()-db->get())>0)
   {
     unset(MSProtocolConnection<MSString>::Read);
     MSString d(db->get(),n);
     db->get(db->get()+n);
     readNotify(d);
   }
}

int MSRawConnection::send(const MSString& aString_)
{
  if (isSet(MSProtocolConnection<MSString>::Reset)==MSTrue) return MSFalse;
  if (readChannel()==0) return MSFalse; 

  MSBuffer *pBuffer=new MSBuffer;
  pBuffer->stuff(aString_.string(),aString_.length());
  sendTheBuffer(pBuffer);
  if (isSet(MSProtocolConnection<MSString>::WritePause)==MSFalse) writeChannel()->enable();
  // attempt to immediately write the buffer
  return doWrite(MSFalse);
}

int MSRawConnection::syncSend(const MSString &aString_,double seconds_)
{
  return syncSend(aString_,(int)(floor(seconds_)),
		  (int)(1000000.0*(seconds_-floor(seconds_))));
}

int MSRawConnection::syncSend(const MSString &aString_,int seconds,int microseconds,MSBoolean isAbsolute)
{

  if (isSet(MSProtocolConnection<MSString>::Reset)) return 0;
  struct timeval timeout, now, tvp;
  if (isAbsolute==MSTrue)
   {
     if (0 > microseconds) return 0;
     tvp.tv_sec=seconds;
     tvp.tv_usec=microseconds;
   }
  else
   {
     gettimeofday(&now,NULL);
     timeout.tv_sec=seconds;
     timeout.tv_usec=microseconds;
     tvsum(&now,&timeout,&tvp);
   }
  if(writeChannel()==0)
   {
     MSMessageLog::errorMessage("MSRawConnection::syncSend: No Write Channel\n");
     return 0;
   }
  MSBuffer *b=new MSBuffer;
  if (b==NULL) return 0;
  b->stuff(aString_.string(),aString_.length());
  sendTheBuffer(b);
  return syncWriteSelectLoop(&tvp);
}

int MSRawConnection::doSyncRead(MSString &result_)
{
  MSBuffer *db=headBuffer();
  int n;

  if (isSet(MSProtocolConnection<MSString>::Reset)==MSTrue) return 0;
  if ((n=readTheBuffer(db,MaxReadSize))<0) return 0;

  if((n=db->put()-db->get())>0)
   {
     unset(MSProtocolConnection<MSString>::Read);
     result_=MSString(db->get(),n);
     db->get(db->get()+n);
   }
  return 1;
}
