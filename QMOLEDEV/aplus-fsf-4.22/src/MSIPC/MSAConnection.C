///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSIPC/MSAConnection.H>
#include <MSTypes/MSMessageLog.H>
#include <unistd.h>
#include <sys/ioctl.h>
#include <netinet/in.h>


#if !defined (MS_NO_FILIO_HEADER)
#include <sys/filio.h>
#endif

#if defined(MSTK_MANUAL_INSTANTIATION)

#include <MSIPC/MSProtocolConnection.C>

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSProtocolConnection<MSA>
#endif

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define(MSProtocolConnection<MSA>)
#endif

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSProtocolConnection<MSA>;
#endif

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSProtocolConnection<MSA>;
#endif

#endif  //MSTK_MANUAL_INSTANTIATION


MSAConnection::MSAConnection(const char *name_, const char *host_, int port_) :
MSProtocolConnection<MSA>(name_, host_, port_)
{
  _bytesToNextMessage=0;
}

MSAConnection::MSAConnection(const char *name_, const MSString &serviceName_) :
MSProtocolConnection<MSA>(name_, serviceName_)
{
  _bytesToNextMessage=0;
}

MSAConnection::MSAConnection(const char *name_, int fd_) :
MSProtocolConnection<MSA>(name_, fd_)
{
  _bytesToNextMessage=0;
}

MSAConnection::~MSAConnection(void) {}

int MSAConnection::send(const MSA &v_)
{
  if (isSet(MSProtocolConnection<MSA>::Reset)==MSTrue) return MSFalse;
  if (readChannel()==0) return 0; 
  MSBuffer *b=exportAObject( v_);
  if (b==NULL) return MSFalse; 
  sendTheBuffer(b);
  if (isSet(MSProtocolConnection<MSA>::WritePause)==MSFalse) writeChannel()->enable();
  // attempt to immediately write buffer
  return doWrite(MSFalse);
}

void MSAConnection::doRead(void)
{
  MSBuffer *hb=headBuffer() ;
  MSBuffer *db=readBuffer() ;
  int s,n;
  
  if (isSet(MSProtocolConnection<MSA>::Reset)==MSTrue) return;
  if((s=hb->put()-hb->get())<4)
   {
     if((n=readTheBuffer(hb,4-s))<0) return;
     if((s=hb->put()-hb->get())<4) return;
     _bytesToNextMessage=MSA::longAt(hb->get());
     if (_bytesToNextMessage<=0) 
      {
	hb->reset();
        unset(MSProtocolConnection<MSA>::Read);
	return;
      }
   }
  if((n=readTheBuffer(db,_bytesToNextMessage))<0) return;
  if((_bytesToNextMessage-=n)==0)
   {
     MSA d=MSA::importAObject( (char *) db->get(), db->put()-db->get(), (char *)0);
     hb->reset();
     db->clear();
     unset(MSProtocolConnection<MSA>::Read);
     if(d.isNullMSA()==MSTrue){resetWithError(MSProtocolConnection<MSA>::Read);return;}
     readNotify(d);
   }
  return; 
}

MSA MSAConnection::getAobjectFromBuffer(MSBuffer *buf_)
{
  MSBuffer *hb=headBuffer();
  MSBuffer *db=readBuffer();
  int s,n;
  if (isSet(MSProtocolConnection<MSA>::Reset)==MSTrue) return MSA();
  if((s=hb->put()-hb->get())<4)
   {
     if((n=MSBuffer::buffToBuff(buf_,hb,4-s))<0) return MSA();
     if((s=hb->put()-hb->get())<4) return MSA();
     _bytesToNextMessage=MSA::longAt(hb->get());
     if (_bytesToNextMessage<=0) 
      {
	hb->reset();
        unset(MSProtocolConnection<MSA>::Read);
	return MSA();
      }
   }
  if((n=MSBuffer::buffToBuff(buf_,db,_bytesToNextMessage))<0) return MSA();
  if((_bytesToNextMessage-=n)==0)
   {
     MSA d=MSA::importAObject( (char *) db->get(), db->put()-db->get(), (char *)0);
     hb->reset();
     db->clear();
     unset(MSProtocolConnection<MSA>::Read);
     if(d.isNullMSA()==MSTrue)
      {
        resetWithError(MSProtocolConnection<MSA>::Read);return MSA();
      }
     return d;
   }
  return MSA(); 
}

void MSAConnection::doReadBurst(void)
{
  int burstLength,n;
  if (isSet(MSProtocolConnection<MSA>::Reset)==MSTrue) return;
#ifdef MS_WINSOCK
  if (ioctlsocket(fd(),FIONREAD,(unsigned long*)&burstLength) == -1)
#else
  if (ioctl(fd(),FIONREAD,(caddr_t)&burstLength) == -1)
#endif
   {
     resetWithError(MSProtocolConnection<MSA>::Read);
     return;
   }
  if (burstLength<0) {return;}
  if (burstLength==0) 
   {
     MSMessageLog::infoMessage("MSAConnection: No data to be read on read event\n");
     burstLength=4;
   }
  MSBuffer b(burstLength);
  if (n=readTheBuffer(&b,burstLength)<0)
   {
        return;
   }
  if (n==0&&burstLength==0)
   {
     unset(MSProtocolConnection<MSA>::Read);
     return;
   }
  MSA d(getAobjectFromBuffer(&b));
  if (d.isNullMSA()==MSTrue) {return;}
  int count=1,s=0;
  char *cp;
  for(cp=b.get();cp<b.put();cp+=s) 
   {
     s=MSA::longAt(cp);
     cp+=sizeof(long);
     if(b.put()-cp>=s) ++count;
   }

  MSA z(MSA::gv(MSA::ETYPE,count));
  int i;
  for(i=0;i<count;++i) z.aStructPtr()->p[i]=0;
  int index=0;
  z.aStructPtr()->p[index++]=(long)d.aStructPtr();

  while(index<count)
   {
     d=getAobjectFromBuffer(&b);
     if(d.isNullMSA()==MSTrue) break;
     z.aStructPtr()->p[index++]=(long)d.aStructPtr();
   }

  if (index<count) 
   {
     MSMessageLog::warningMessage("MSAConnection: Burst Mode Aborted. Possible Data Loss.\n");
   }
  if (b.get()==b.put()) unset(MSProtocolConnection<MSA>::Read);
  else 
   {
     d=getAobjectFromBuffer(&b);
     if (d.isNullMSA()==MSFalse || b.get()!=b.put())
      {
        MSMessageLog::warningMessage("MSAConnection: Burst Buffer Not Cleared\n");
      }
   }
  readNotify(d);
}

int MSAConnection::syncSend(const MSA &v_,double seconds_)
{
  return  syncSend(v_,(int)(floor(seconds_)),
                   (int)(1000000.0*(seconds_-floor(seconds_))),MSFalse);
}

int MSAConnection::syncSend(const MSA &v_,int seconds_,int microseconds_,MSBoolean isAbsolute_)
{

  if (isSet(MSProtocolConnection<MSA>::Reset)==MSTrue) return 0;
  struct timeval timeout, now, tvp;
  if (isAbsolute_==MSTrue)
   {
     if (0 > microseconds_) return 0;
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
  if(writeChannel()==0)
   {
     MSMessageLog::errorMessage("MSAConnection: No Write Channel\n");
     return 0;
   }
  MSBuffer *b=exportAObject(v_);
  if (b==NULL) return 0;
  sendTheBuffer(b);
  return syncWriteSelectLoop(&tvp);
}

int MSAConnection::doSyncRead(MSA &result_)
{
  MSBuffer *hb=headBuffer();
  MSBuffer *db=readBuffer();
  int s,n;

  if (isSet(MSProtocolConnection<MSA>::Reset)==MSTrue) return 0;
  if ((s=hb->put()-hb->get())<4)
   {
     if ((n=readTheBuffer(hb,4-s))<0) return 0;
     if((s=hb->put()-hb->get())<4) return 0;
     _bytesToNextMessage=MSA::longAt(hb->get());
   }
  if((n=readTheBuffer(db,_bytesToNextMessage))<0) return 0;
  if((_bytesToNextMessage-=n)==0)
   {
     result_=MSA::importAObject((char *) db->get(), db->put()-db->get(), (char *)0);
     hb->reset();
     db->clear();
     unset(MSProtocolConnection<MSA>::Read);
     if((result_.aStructPtr())==(MSAStruct *)0){resetWithError(MSProtocolConnection<MSA>::Read);return 0;}
   }
  
  return 1;
}

MSBuffer * MSAConnection::exportAObject(const MSA& aObj_)
{
  long headsize,datasize;
  int size,temp;
  int rc;
  rc=aObj_.exportAObjectSizePass(&headsize,&datasize);
  if(rc!=0) return NULL;
  size=headsize+datasize;
  MSBuffer *msg=new MSBuffer(4+size); 
  temp=htonl(size);
  msg->stuff((char *)(&temp),4); //bump the put because we are writing ourselves
  aObj_.exportAObjectFillPass(msg->put(),headsize,(char *) 0,1);
  msg->put(msg->put()+size);
  return(msg);
} 

#ifdef MS_64BIT
MSBuffer * MSAConnection::export64AObject(const MSA& aObj_)
{
  long headsize,datasize;
  int size,temp;
  int rc;
  rc=aObj_.exportAObjectSizePass(&headsize,&datasize,8);
  if(rc!=0) return NULL;
  size=headsize+datasize;
  MSBuffer *msg=new MSBuffer(4+size); 
  temp=htonl(size);
  msg->stuff((char *)(&temp),4); //bump the put because we are writing ourselves
  aObj_.exportAObjectFillPass(msg->put(),headsize,(char *) 0,1,8);
  msg->put(msg->put()+size);
  return(msg);
} 

#endif
